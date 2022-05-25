/*
 * @(#)GenerateIndexingData.java created Aug 25, 2006 Casalino
 *
 * Copyright (c) 2006 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with the author.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.diffr.sdpd;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.ProgressFrame;

import java.util.Vector;

/**
 * The GenerateIndexingData is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/12/04 14:30:05 $
 * @since JDK1.1
 */
public class GenerateIndexingData {

  double[] cellData = new double[7];
  int[] cellAndLinesNumber = new int[3];
  public static String[] allUniqueSgHM = SpaceGroups.getAllUniqueSgHM();
  String symmetryString[] = {"all", "triclinic", "monoclinic", "orthorhombic",
      "tetragonal", "trigonal", "hexagonal", "cubic"};
  Phase[] thePhase;
  private Vector listData;
  int actualRunningThreads = 0;

  Sample theSample;
  int linesNumber = 0;
  int extraLines = 0;
  int missingPeaks = 0;
  double amin = 0;
  double amax = 0;
  double anmin = 0;
  double anmax = 0;
  int trialNumber = 0;
  double cellmin = 0;
  double cellmax = 0;
  double peakSeparation = 0.001;

  boolean computing = true;

  public GenerateIndexingData(Sample theSample, int linesNumber, int extraLines, int missingPeaks, double amin,
                              double amax, double anmin, double anmax, int trialNumber, double cellmin,
                              double cellmax) {
    this.theSample = theSample;
    this.linesNumber = linesNumber;
    this.extraLines = extraLines;
    this.missingPeaks = missingPeaks;
    this.amin = amin;
    this.amax = amax;
    this.anmin = anmin;
    this.anmax = anmax;
    this.trialNumber = trialNumber;
    this.cellmin = cellmin;
    this.cellmax = cellmax;

    peakSeparation = MaudPreferences.getDouble("GenerateIndexingData.peakSeparation", 0.001);
  }

  public void generateData(String symmetry, String spacegroup) {
    computing = true;
    int i;

    actualRunningThreads = 0;
    listData = new Vector(100, 100);

    int maxRunning = Constants.maxNumberOfThreads;
    if (Constants.debugThreads)
      System.out.println("Thread Indexing");

    Vector data = new Vector(100, 100);
    if (symmetry.equalsIgnoreCase("all")) {
      for (int k = 1; k < symmetryString.length; k++) {
        int istart = SpaceGroups.getBeginSGUnique(symmetryString[k]);
        int iend = SpaceGroups.getEndSGUnique(symmetryString[k]);
        for (int j = istart; j < iend; j++) {
          String[] sym = new String[2];
          sym[0] = symmetryString[k];
          sym[1] = allUniqueSgHM[j];
          data.add(sym);
        }
      }
    } else {
      if (spacegroup.equalsIgnoreCase("all")) {
        int istart = SpaceGroups.getBeginSGUnique(symmetry);
        int iend = SpaceGroups.getEndSGUnique(symmetry);
        for (int j = istart; j < iend; j++) {
          String[] sym = new String[2];
          sym[0] = symmetry;
          sym[1] = allUniqueSgHM[j];
          data.add(sym);
        }
      } else {
        String[] sym = new String[2];
        sym[0] = symmetry;
        sym[1] = spacegroup;
        data.add(sym);
      }
    }

    if (maxRunning > data.size())
      maxRunning = data.size();

    PersistentThread[] threads = new PersistentThread[maxRunning];
    thePhase = new Phase[maxRunning];
    for (i = 0; i < maxRunning; i++) {
      thePhase[i] = theSample.newPhase();
      threads[i] = new PersistentThread(i) {
        public void executeJob() {
          actualRunningThreads++;
          int index = this.threadNumber;
          Phase aphase = thePhase[index];
          int number = this.data.size();
          ProgressFrame prF = null;
          if (!Constants.textonly && Constants.showProgressFrame)
            try {
              prF = new ProgressFrame(number, 2);
              prF.setTitle("Generating indexing data");
            } catch (NullPointerException npe) {
              System.out.println("Not able to create frame, MacOSX display sleep bug?");
            }

          for (int i = 0; i < number; i++) {
            String[] sym = (String[]) this.data.elementAt(i);
            aphase.setSymmetry(sym[0]);
            aphase.setSpaceGroup(true, sym[1], false);
            if (!Constants.textonly && prF != null) {
              prF.setProgressText("Symmetry: " + sym[0] + ", " + "space group: " + sym[1]);
            }
            Vector dataVector = generateData(aphase, prF);
            addData(dataVector);
          }
          if (!Constants.textonly && prF != null) {
            prF.setVisible(false);
            prF.dispose();
//            prF = null;
          }
          actualRunningThreads--;
        }
      };
    }
    i = 0;
    int istep = data.size() / maxRunning;
    for (int j = 0; j < maxRunning; j++) {
      int is = i;
      if (j < maxRunning - 1)
        i = Math.min(i + istep, data.size());
      else
        i = data.size();
      threads[j].setJobRange(is, i);
      threads[j].data = new Vector(istep, 10);
      for (int k = is; k < i; k++)
        threads[j].data.add(data.elementAt(k));
      threads[j].start();
    }
    do {
      try {
        Thread.sleep(Constants.timeToWaitThreadsEnding);
      } catch (InterruptedException r) {
        r.printStackTrace();
      }
    } while (actualRunningThreads > 0);

    for (int j = 0; j < maxRunning; j++) {
      theSample.removePhase(thePhase[j]);
      threads[j] = null;
    }
//    threads = null;

    computing = false;

  }

  final Object datalock = new Object();

  public void addData(Vector data) {
    synchronized (datalock) {
      listData.add(data);
    }
  }

  public Vector getData() {
    while (computing) {
      try {
        Thread.sleep(500);
      } catch (InterruptedException e) {
        e.printStackTrace();
      }
    }
    return listData;
  }

  final Object lock = new Object();

  public Vector generateData(Phase phase, ProgressFrame prF) {

    int time, ltrialNumber;
    double lcellmin, lcellmax, lanmin, lanmax, lamin, lamax;

    synchronized (lock) {
      time = (int) System.currentTimeMillis();
      ltrialNumber = trialNumber;
      lcellmin = cellmin;
      lcellmax = cellmax;
      lamin = amin;
      lamax = amax;
      lanmin = anmin;
      lanmax = anmax;
      Parameter.doRefresh = false;
    }
    ec.util.MersenneTwisterFast randomizer = new ec.util.MersenneTwisterFast(time);
    Vector dataVector = new Vector(ltrialNumber, 1);

    double[] tricAngles = new double[3];

    if (prF != null)
      prF.setProgressBarValue(ltrialNumber, 1);

    for (int i = 0; i < ltrialNumber; i++) {
      double vc = -1.0;
      while (vc < lcellmin || vc > lcellmax) {
        switch (SpaceGroups.getSymmetryNumber(phase.getSymmetry())) {
          case 0: // triclinic
            for (int j = 0; j < 3; j++) {
              phase.setCellValue(j, lamin + (lamax - lamin) * randomizer.nextDouble());
            }
            for (int j = 0; j < 3; j++) {
              if (j == 1 && (tricAngles[0] + lanmax + lanmin >= 360)) {
                tricAngles[j] = lanmin + (360 - (tricAngles[0] + lanmin) - lanmin) * randomizer.nextDouble();
              } else {
                if (j == 2 && (tricAngles[0] + tricAngles[1] + lanmax >= 360))
                  tricAngles[j] = lanmin + (360 - (tricAngles[0] + tricAngles[1]) - lanmin) * randomizer.nextDouble();
                else
                  tricAngles[j] = lanmin + (lanmax - lanmin) * randomizer.nextDouble();
              }
            }
            for (int j = 0; j < 3; j++) {
              phase.setCellValue(j + 3, tricAngles[j]);
            }
            break;
          case 1: // monoclinic
            for (int j = 0; j < 3; j++)
              phase.setCellValue(j, lamin + (lamax - lamin) * randomizer.nextDouble());
            for (int j = 0; j < 3; j++)
              if (j == 1)
                phase.setCellValue(j + 3, lanmin + (lanmax - lanmin) * randomizer.nextDouble());
              else
                phase.setCellValue(j + 3, 90.0);
            break;
          case 2: // orthorhombic
            for (int j = 0; j < 3; j++)
              phase.setCellValue(j, lamin + (lamax - lamin) * randomizer.nextDouble());
            for (int j = 0; j < 3; j++)
              phase.setCellValue(j + 3, 90.0);
            break;
          case 3: // tetragonal
            double a = lamin + (lamax - lamin) * randomizer.nextDouble();
            phase.setCellValue(0, a);
            phase.setCellValue(1, a);
            phase.setCellValue(2, lamin + (lamax - lamin) * randomizer.nextDouble());
            for (int j = 0; j < 3; j++)
              phase.setCellValue(j + 3, 90.0);
            break;
          case 4: // trigonal
          case 5: // hexagonal
            a = lamin + (lamax - lamin) * randomizer.nextDouble();
            phase.setCellValue(0, a);
            phase.setCellValue(1, a);
            phase.setCellValue(2, lamin + (lamax - lamin) * randomizer.nextDouble());
            for (int j = 0; j < 2; j++)
              phase.setCellValue(j + 3, 90.0);
            phase.setCellValue(5, 120.0);
            break;
          case 6: // cubic
            a = lamin + (lamax - lamin) * randomizer.nextDouble();
            phase.setCellValue(0, a);
            phase.setCellValue(1, a);
            phase.setCellValue(2, a);
            for (int j = 0; j < 3; j++)
              phase.setCellValue(j + 3, 90.0);
            break;
          default: {
          }
        }
        phase.updateAll();
        phase.CellSymmetry();
        phase.updateParametertoDoubleBuffering(false);
        vc = phase.getCellVolume();
      }
      if (!Constants.textonly && prF != null)
        prF.increaseProgressBarValue(1);
      dataVector.add(getLinesAndCell(phase));
    }
    String[] symSg = {phase.getSymmetry(), phase.getSpaceGroup()};
    dataVector.add(symSg);
    synchronized (lock) {
      Parameter.doRefresh = true;
    }
    return dataVector;
  }

  private double[] getLinesAndCell(Phase aphase) {
    double[] peaks = aphase.computeReflectionList(linesNumber + missingPeaks, false, peakSeparation);
    double max = peaks[0];
    double min = peaks[peaks.length - 1];
    ec.util.MersenneTwisterFast randomizer = new ec.util.MersenneTwisterFast();
    for (int i = 0; i < missingPeaks; i++) {
      int random;
      do {
        random = (int) (peaks.length * randomizer.nextDouble() + 0.4999999999);
      } while (peaks[random] == -1.0);
      peaks[random] = -1.0;
    }
    double[] tpeaks = new double[linesNumber];
    int l = 0;
    for (int i = 0; i < peaks.length; i++) {
      if (peaks[i] >= 0.0)
        tpeaks[l++] = peaks[i];
    }
    for (int i = 0; i < extraLines; i++) {
      tpeaks[tpeaks.length - i - 1] = min + (max - min) * randomizer.nextDouble();
    }
    boolean modified = true;
    while (modified) {
      modified = false;
      for (int i = 1; i < tpeaks.length; i++) {
        if (tpeaks[i] > tpeaks[i - 1]) {
          double temp = tpeaks[i - 1];
          tpeaks[i - 1] = tpeaks[i];
          tpeaks[i] = temp;
          modified = true;
        }
      }
    }
    double[] finalPeaks = new double[tpeaks.length + 6];
    System.arraycopy(tpeaks, 0, finalPeaks, 0, tpeaks.length);

    for (int i = 0; i < tpeaks.length; i++)
      finalPeaks[i] = normalizeLine(finalPeaks[i], amin, amax);
    for (int i = 0; i < 6; i++) {
      if (i < 3)
        finalPeaks[tpeaks.length + i] = normalizeValue(aphase.getCellValue(i), amin, amax);
      else
        finalPeaks[tpeaks.length + i] = normalizeValue(aphase.getCellValue(i), anmin, anmax);
    }
    return finalPeaks;
  }

  public static double lambda = 1.540598;

  public static double normalizeLine(double finalPeak, double min, double max) {
    // l = 2d sin theta
//    return Math.asin(lambda / (2.0 * finalPeak)) / Math.PI * 2.0;
//    return 0.5 / finalPeak;
    return finalPeak / max;
  }

  public double normalizeCellValue(double value) {
    return normalizeValue(value, amin, amax);
  }

  public double reconstructCellValue(double norm) {
    return reconstructValue(norm, amin, amax);
  }

  public double normalizeAngleValue(double value) {
    return normalizeValue(value, anmin, anmax);
  }

  public double reconstructAngleValue(double norm) {
    return reconstructValue(norm, anmin, anmax);
  }

  public static double normalizeValue(double value, double min, double max) {
    return (value - min) / (max - min);
//    return min / value;
  }

  public static double reconstructValue(double norm, double min, double max) {
    return min + norm * (max - min);
//    return min / norm;
  }

  public double[] reconstructCell(double[] rcell) {
    for (int i = 0; i < 3; i++)
      rcell[i] = reconstructValue(rcell[i], amin, amax);
    for (int i = 3; i < 6; i++)
      rcell[i] = reconstructValue(rcell[i], anmin, anmax);
    return rcell;
  }
}
