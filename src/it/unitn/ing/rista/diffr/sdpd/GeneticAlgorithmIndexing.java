/*
 * @(#)GeneticAlgorithmIndexing.java created Mar 7, 2005 Pergine Vals.
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.interfaces.Function;
import it.unitn.ing.rista.comp.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.jsginfo.*;

import javax.swing.*;
import java.io.OutputStream;
import java.util.Arrays;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;


/**
 * The GeneticAlgorithmIndexing is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.7 $, $Date: 2006/11/10 09:33:01 $
 * @since JDK1.1
 */

public class GeneticAlgorithmIndexing implements Function {

  double[] peakList = null;
  double[] weightList = null;
  int symmetry = 0;
  double dspacemin = 1.0E30, dspacemax = -1.0E30;
  T_SgInfo SgInfo = null;
  Sghkl sghkl = null;
  int numberOfFreeParameters = 0;
  int numberOfCellNoAngles = 0;
  double[] parameters = null;
  double[] lowerBound = null;
  double[] upperBound = null;
  int[] ic = null;
  GeneticAlgorithmRefinement refinementAlgorithm = null;
  double errorDspace = MaudPreferences.getDouble("indexing.maxDSpacingError", 0.0001);
  int maxToIndex = MaudPreferences.getInteger("indexing.maxPeaksToIndex", 100);
  int maxToExclude = MaudPreferences.getInteger("indexing.maxPeaksToExclude", 1);
//  double penalty = MaudPreferences.getDouble("dspacing.PenaltyError", 1.0);
  Object thephase = null;
  int maxNumberPeaks = 1000;
  double[] list = null;
  double[] dist = null;
  double[] err = null;
  double cellVolume = 1.0;
  double bestFitness = 0.0;
  int[][] hklList = null;
  double[] dList = null;
  boolean isOptimizing = false;
  OutputPanel outputPanel = null;

  public GeneticAlgorithmIndexing(Object aphase, double[][] apeakList, String tmpSymmetry,
                                  double[] pars, double[] lowerParBounds, double[] upperParBounds) {
//    double[] apeakList = aphase.getCustomPeakPositions();
    if (maxToIndex > apeakList[0].length)
      maxToIndex = apeakList[0].length;
    peakList = new double[maxToIndex];
//    weightList = new double[maxToIndex];
    System.arraycopy(apeakList[0], 0, peakList, 0, maxToIndex);
//    System.arraycopy(apeakList[1], 0, weightList, 0, maxToIndex);
//    for (int i = 0; i < maxToIndex; i++)
//      weightList[i] = 1.0 / weightList[i];
    thephase = aphase;
    outputPanel = ((Phase)thephase).getFilePar().getMainFrame().getOutputPanel();
//    peakList = peaklist;
    Arrays.sort(peakList);
//    String tmpSymmetry = aphase.getSymmetry();
    symmetry = SpaceGroups.getSymmetryNumber(tmpSymmetry);
    int sgconv = SpaceGroups.CONV_HM;
    int istart = SpaceGroups.getBeginSG(tmpSymmetry, sgconv);
    String SgName = SpaceGroups.getSpaceGroup(istart, sgconv);
    dspacemin = peakList[0];
    dspacemax = peakList[peakList.length - 1];
    maxNumberPeaks = (peakList.length + 1) * 96;
    list = new double[maxNumberPeaks];
    for (int i = 0, j = peakList.length - 1; i <= j; i++, j--) {
      double tmpValue = 1.0 / (peakList[j] * peakList[j]);
      peakList[j] = 1.0 / (peakList[i] * peakList[i]);
      peakList[i] = tmpValue;
//      tmpValue = weightList[j];
//      weightList[j] = weightList[i];
//      weightList[i] = tmpValue;
    }
    dist = new double[peakList.length];
    err = new double[peakList.length];
    for (int i = 0; i < peakList.length; i++) {
      err[i] = errorDspace * errorDspace; // * peakList[i] * peakList[i];
    }

    char F_Convention;

    F_Convention = 'A';
    if (sgconv == 3)
      F_Convention = 'H';

    SgInfo = new T_SgInfo(new String(SgName), F_Convention);

//    int LGgroup = T_SgInfo.LG_Number(SgInfo.PointGroup);
//    int PGgroup = T_SgInfo.PG_Index(SgInfo.PointGroup);

    sghkl = new Sghkl(SgInfo);

    ic = cellSymmetryControls(symmetry);
    numberOfFreeParameters = 0;
    for (int i = 0; i < 6; i++) {
      if (ic[i] == 1)
        numberOfFreeParameters++;
    }
//    numberOfFreeParameters++;

    parameters = new double[numberOfFreeParameters];
    lowerBound = new double[numberOfFreeParameters];
    upperBound = new double[numberOfFreeParameters];
    int index = 0;
    for (int i = 0, j = 0; i < 6; i++)
      if (ic[i] == 1) {
        parameters[j] = pars[index];
        lowerBound[j] = lowerParBounds[index];
        upperBound[j++] = upperParBounds[index++];
//        parameters[j] = aphase.getCell(i).getValueD();
//        lowerBound[j] = aphase.getCell(i).getValueMinD();
//        upperBound[j++] = aphase.getCell(i).getValueMaxD();
      }
/*    int j = numberOfFreeParameters - 1;
    parameters[j] = 0.0; // Dspacing error
    lowerBound[j] = -0.001;
    upperBound[j] = 0.001;*/

//    System.out.println("Best fitness = " + computeFitness(parameters));
    bestFitness = 1.0E-8;
    bestFitness = computeFitness(parameters) / 10;

    refinementAlgorithm = new GeneticAlgorithmRefinement(null, "Evolutionary indexing");
    refinementAlgorithm.setCrossOverProbability("0.3");
    refinementAlgorithm.setCrossOverType(1);
    refinementAlgorithm.setGenerationsNumber("10");
    refinementAlgorithm.setMutationProbability("0.05");
    refinementAlgorithm.setPopulationSize("10000");
  }

  public OptimizationAlgorithm getOptimizationAlgorithm() {
    return refinementAlgorithm;
  }

  public int getNumberOfData() {
    return peakList.length;
  }

  public double[] getFit() {

    return null;
  }

  public double getWSS() {
    return computeFitness(parameters);
  }

  public void computeFirstFit() {
  }

  public int getNumberOfFreeParameters() {
    return numberOfFreeParameters;
  }

  public double getLowerBound(int index) {
    return (double) lowerBound[index];
  }

  public double getUpperBound(int index) {
    return (double) upperBound[index];
  }

  public double getParameterMinSignificantValue(int i) {
    return (double) lowerBound[i];
  }

  public double getFreeParameter(int index) {
    return (double) parameters[index];
  }

  public void setDerivate(boolean value) {
  }

  public void setOptimizing(boolean value) {
    isOptimizing = value;
  }

  public boolean isOptimizing() {
    return isOptimizing;
  }

//  double[] individualParams = null;

  public void setFreeParameters(double[] parm) {
//    for (int i = 0; i < numberOfFreeParameters; i++)
//      parameters[i] = parm[i];
    parameters = parm;
  }

  public void saveparameters() {
    for (int i = 0, j = 0; i < 6; i++)
      if (ic[i] == 1) {
//        thephase.getCell(i).setValueMin(parameters[j] * 0.95);
//        thephase.getCell(i).setValueMax(parameters[j] * 1.05);
        if (thephase != null)
          ((Phase) thephase).setCellValue(i, parameters[j++]);
      }
  }


  public double computeFitness(double[] parms) {

//    int totalPresent, totalMissing;//, notFound;

    double[] cell = new double[6];

    for (int i = 0, j = 0; i < 6; i++)
      if (ic[i] == 1)
        cell[i] = parms[j++];
    cellSymmetry(cell, symmetry);

    if (hklList == null) {
      hklList = sghkllist(dspacemin * 0.5, dspacemax * 2.0, cell);
      dList = new double[hklList[0].length];
    }
    if (dList == null || dList.length < 1)
      return 1.0E30;
    double[] aList = computeDList(dspacemin - errorDspace * 10.0, dspacemax + errorDspace * 100.0, cell);
    if (aList.length < 1)
      return 10E30;
    int nRi = aList.length;
    double Q20 = peakList[peakList.length - 1];
    int lowStart = 0;
    while (aList[lowStart] < peakList[0] && lowStart < nRi - 1) {
      lowStart++;
    }
    int N20 = nRi - lowStart;
    if (lowStart > 0)
      lowStart--;
    int nRe = getNumberOfData();
    int lowB = lowStart;
    double epsilon = 0.0;
//    int maxDone = 0;
    for (int i = 0; i < nRe; i++) {
      int upB = nRi - 1;
      if (peakList[i] <= aList[lowB])
        upB = lowB;
      else if (peakList[i] >= aList[upB])
        lowB = upB;
      else
        while (upB - lowB > 1) {
          int tempB = (lowB + upB) / 2;
          if (peakList[i] < aList[tempB])
            upB = tempB;
          else
            lowB = tempB;
        }
      double dist1 = Math.abs(peakList[i] - aList[lowB]);
      double dist2 = Math.abs(peakList[i] - aList[upB]);
      if (dist1 <= dist2) {
        dist[i] = dist1;
      } else {
        dist[i] = dist2;
      }
//      if (dist[i] < err[i]) {
//        epsilon += dist[i];
//        Q20 = peakList[i];
//        maxDone++;
//      }
//      if (maxDone >= maxToIndex)
//        break;
    }
    Arrays.sort(dist);
    for (int i = maxToExclude; i < nRe; i++) {
//      if (dist[i] > errorDspace)
        epsilon += dist[i]; // * weightList[i];
    }
    double fitness = (2.0 * epsilon * N20) / Q20;
    if (fitness < bestFitness) {
      bestFitness = fitness;
//      System.out.println(maxDone);
      System.out.println("Q20 = " + Q20);
      System.out.println("N20 = " + N20);
      System.out.println("Epsilon = " + epsilon);
      System.out.println("M20 = " + 1.0 / fitness);
    }
    return fitness;  // Fitness: De Wolff M20 modified
  }

  public double computeOldFitness(double[] parms) {

//    int totalPresent, totalMissing;//, notFound;

    double[] cell = new double[6];

    for (int i = 0, j = 0; i < 6; i++)
      if (ic[i] == 1)
        cell[i] = parms[j++];
    cellSymmetry(cell, symmetry);

    double[] aList = sgdspacelist(dspacemin - errorDspace * 10.0, dspacemax + errorDspace * 100.0, cell);
    if (aList == null || aList.length < 1)
      return 1.0E30;
    int nRi = aList.length;
    double Q20 = peakList[peakList.length - 1];
    int lowStart = 0;
    while (aList[lowStart] < peakList[0]) {
      lowStart++;
    }
    int N20 = nRi - lowStart;
    if (lowStart > 0)
      lowStart--;
    int nRe = getNumberOfData();
    int lowB = lowStart;
    double epsilon = 0.0;
//    int maxDone = 0;
    for (int i = 0; i < nRe; i++) {
      int upB = nRi - 1;
      if (peakList[i] <= aList[lowB])
        upB = lowB;
      else if (peakList[i] >= aList[upB])
        lowB = upB;
      else
        while (upB - lowB > 1) {
          int tempB = (lowB + upB) / 2;
          if (peakList[i] < aList[tempB])
            upB = tempB;
          else
            lowB = tempB;
        }
      double dist1 = Math.abs(peakList[i] - aList[lowB]);
      double dist2 = Math.abs(peakList[i] - aList[upB]);
      if (dist1 <= dist2) {
        dist[i] = dist1;
      } else {
        dist[i] = dist2;
      }
//      if (dist[i] < err[i]) {
        epsilon += dist[i];
//        Q20 = peakList[i];
//        maxDone++;
//      }
//      if (maxDone >= maxToIndex)
//        break;
    }
/*    Arrays.sort(dist);
    for (int i = maxToExclude; i < nRe; i++) {
      epsilon += dist[i];
    }*/
    double fitness = (2.0 * epsilon * N20) / Q20 / 100;
    if (fitness < bestFitness) {
      bestFitness = fitness;
//      System.out.println(maxDone);
      System.out.println("Q20 = " + Q20);
      System.out.println("N20 = " + N20);
      System.out.println("Epsilon = " + epsilon);
      System.out.println("M20 = " + 1.0 / fitness);
    }
    return fitness;  // Fitness: De Wolff M20 modified
  }

  static double[] so = new double[6];

  public double cellVolumeComp(double[] cell) {
    double cella = cell[0];
    double cellb = cell[1];
    double cellc = cell[2];
    double cella2 = cella * cella;
    double cellb2 = cellb * cellb;
    double cellc2 = cellc * cellc;
    double cosalpha = Math.cos(cell[3] * Constants.DEGTOPI);
    double cosbeta = Math.cos(cell[4] * Constants.DEGTOPI);
    double cosgamma = Math.cos(cell[5] * Constants.DEGTOPI);
    double powprod = cosalpha * cosalpha + cosbeta * cosbeta + cosgamma * cosgamma;

    double cellVolume = cella * cellb * cellc * Math.sqrt(1.0 - powprod + 2.0 * cosalpha * cosbeta * cosgamma);
    so[0] = cellb2 * cellc2 * (1.0 - cosalpha * cosalpha);
    so[1] = cella2 * cellc2 * (1.0 - cosbeta * cosbeta);
    so[2] = cellb2 * cella2 * (1.0 - cosgamma * cosgamma);
    so[3] = cella2 * cellb * cellc * (cosbeta * cosgamma - cosalpha);
    so[4] = cellb2 * cella * cellc * (cosalpha * cosgamma - cosbeta);
    so[5] = cellc2 * cellb * cella * (cosbeta * cosalpha - cosgamma);
    return cellVolume;
  }

  public double[] sgdspacelist(double dplanecut, double dplanestart, double[] cell) {

    int h, k, l, iList;
    int Maxh, Maxk, Maxl;
    double dpi, vc;
    int friedelLaw = 1;

    cellVolume = cellVolumeComp(cell);
    double rec_cellVolume2 = 1.0 / (cellVolume * cellVolume);

    Maxh = (int) (cellVolume / (Math.sqrt(so[0]) * dplanecut)) + 1;
    Maxk = (int) (cellVolume / (Math.sqrt(so[1]) * dplanecut)) + 1;
    Maxl = (int) (cellVolume / (Math.sqrt(so[2]) * dplanecut)) + 1;

    int[] Minh = new int[1], Mink = new int[1], Minl = new int[1];
    sghkl.SetListMin_hkl(friedelLaw, Maxh, Maxk, Maxl, Minh, Mink, Minl);
    int totNumber = 0;
    dplanecut = 1.0 / (dplanecut * dplanecut);
    dplanestart = 1.0 / (dplanestart * dplanestart);

    for (h = Minh[0]; h <= Maxh; h++) {
      int h2 = h * h;
      int h_2 = 2 * h;
      for (k = Mink[0]; k <= Maxk; k++) {
        int k2 = k * k;
        int k_2 = 2 * k;
        int hk = h * k * 2;
        for (l = Minl[0]; l <= Maxl; l++) {
          iList = sghkl.IsSysAbsent_hkl(h, k, l, null);
          if (iList == 0) {
            if ((iList = sghkl.IsHidden_hkl(friedelLaw, Minh[0], Mink[0], Minl[0],
                Maxh, Maxk, Maxl,
                h, k, l)) == 0) {
              if (!((h == 0 && k == 0) && l == 0)) {
                dpi = (so[0] * h2 + so[1] * k2 + so[2] * l * l +
                    so[5] * hk + so[3] * k_2 * l + so[4] * h_2 * l) * rec_cellVolume2;
                if (dpi >= dplanestart && dpi <= dplanecut) {
                  list[totNumber++] = dpi;
//									System.out.println("New Reflection " + h +" "+k+" "+l);
                }
              }
            }
          }
          if (totNumber >= maxNumberPeaks)
            break;
        }
        if (totNumber >= maxNumberPeaks)
          break;
      }
      if (totNumber >= maxNumberPeaks)
        break;
    }
    double[] reflectionList = new double[totNumber];
//    for (int i = 0; i < totNumber; i++)
//      reflectionList[i] = list[i];
    System.arraycopy(list, 0, reflectionList, 0, totNumber);
    Arrays.sort(reflectionList);
    return reflectionList;
  }

  public double[] computeDList(double dplanecut, double dplanestart, double[] cell) {
    cellVolume = cellVolumeComp(cell);
    double rec_cellVolume2 = 1.0 / (cellVolume * cellVolume);

    dplanecut = 1.0 / (dplanecut * dplanecut);
    dplanestart = 1.0 / (dplanestart * dplanestart);
    int number = hklList[0].length;
    int totNumber = 0;
    for (int i = 0; i < number; i++) {
      int h = hklList[0][i];
      int k = hklList[1][i];
      int l = hklList[2][i];
      int h2 = h * h;
      int h_2 = 2 * h;
      int k2 = k * k;
      int k_2 = 2 * k;
      int hk = h * k * 2;
      double dpi = (so[0] * h2 + so[1] * k2 + so[2] * l * l +
                  so[5] * hk + so[3] * k_2 * l + so[4] * h_2 * l) * rec_cellVolume2;
      if (dpi >= dplanestart && dpi <= dplanecut) {
        dList[totNumber++] = dpi;
//        System.out.println("New d " + dpi);
      }
    }
    double[] reflectionList = new double[totNumber];
    System.arraycopy(dList, 0, reflectionList, 0, totNumber);
    Arrays.sort(reflectionList);
    return reflectionList;
  }

  public int[][] sghkllist(double dplanecut, double dplanestart, double[] cell) {

    int h, k, l, iList;
    int Maxh, Maxk, Maxl;
    double dpi;
    int friedelLaw = 1;

    cellVolume = cellVolumeComp(cell);
    double rec_cellVolume2 = 1.0 / (cellVolume * cellVolume);

    Maxh = (int) (cellVolume / (Math.sqrt(so[0]) * dplanecut)) + 1;
    Maxk = (int) (cellVolume / (Math.sqrt(so[1]) * dplanecut)) + 1;
    Maxl = (int) (cellVolume / (Math.sqrt(so[2]) * dplanecut)) + 1;

    int[] Minh = new int[1], Mink = new int[1], Minl = new int[1];
    sghkl.SetListMin_hkl(friedelLaw, Maxh, Maxk, Maxl, Minh, Mink, Minl);
    int totNumber = 0;
    dplanecut = 1.0 / (dplanecut * dplanecut);
    dplanestart = 1.0 / (dplanestart * dplanestart);

    int[][] myhklList = new int[3][maxNumberPeaks];

    for (h = Minh[0]; h <= Maxh; h++) {
      int h2 = h * h;
      int h_2 = 2 * h;
      for (k = Mink[0]; k <= Maxk; k++) {
        int k2 = k * k;
        int k_2 = 2 * k;
        int hk = h * k * 2;
        for (l = Minl[0]; l <= Maxl; l++) {
          iList = sghkl.IsSysAbsent_hkl(h, k, l, null);
          if (iList == 0) {
            if ((iList = sghkl.IsHidden_hkl(friedelLaw, Minh[0], Mink[0], Minl[0],
                Maxh, Maxk, Maxl,
                h, k, l)) == 0) {
              if (!((h == 0 && k == 0) && l == 0)) {
                dpi = (so[0] * h2 + so[1] * k2 + so[2] * l * l +
                    so[5] * hk + so[3] * k_2 * l + so[4] * h_2 * l) * rec_cellVolume2;
                if (dpi >= dplanestart && dpi <= dplanecut) {
                  myhklList[0][totNumber] = h;
                  myhklList[1][totNumber] = k;
                  myhklList[2][totNumber++] = l;
//									System.out.println("New Reflection " + h +" "+k+" "+l);
                }
              }
            }
          }
          if (totNumber >= maxNumberPeaks)
            break;
        }
        if (totNumber >= maxNumberPeaks)
          break;
      }
      if (totNumber >= maxNumberPeaks)
        break;
    }
    if (totNumber == maxNumberPeaks)
      return myhklList;
    int[][] reflectionList = new int[3][totNumber];
    System.arraycopy(myhklList[0], 0, reflectionList[0], 0, totNumber);
    System.arraycopy(myhklList[1], 0, reflectionList[1], 0, totNumber);
    System.arraycopy(myhklList[2], 0, reflectionList[2], 0, totNumber);
    return reflectionList;
  }

  public static int[] cellSymmetryControls(int symmetry) {
    int[] ic = new int[6];

      ic[0] = 0;
      ic[1] = 0;
      ic[2] = 0;
      ic[3] = 0;
      ic[4] = 0;
      ic[5] = 0;
      switch (symmetry) {
        case 0:
          ic[0] = 1;
          ic[1] = 1;
          ic[2] = 1;
          ic[3] = 1;
          ic[4] = 1;
          ic[5] = 1;
          break;
        case 1:
          ic[0] = 1;
          ic[1] = 1;
          ic[2] = 1;
          ic[4] = 1;
          break;
        case 2:
          ic[0] = 1;
          ic[1] = 1;
          ic[2] = 1;
          break;
        case 3:
          ic[0] = 1;
          ic[2] = 1;
          ic[1] = 2;
          break;
        case 4:
          ic[0] = 1;
          ic[2] = 1;
          ic[1] = 2;
          break;
        case 5:
          ic[0] = 1;
          ic[2] = 1;
          ic[1] = 2;
          break;
        case 6:
          ic[0] = 1;
          ic[1] = 2;
          ic[2] = 2;
          break;
        default: {}
      }
    return ic;
  }

  public static void cellSymmetry(double[] cell, int symmetry) {
      switch (symmetry) {
        case 0:
          break;
        case 1:
          cell[3] = 90.0;
          cell[5] = 90.0;
          break;
        case 2:
          cell[3] = 90.0;
          cell[4] = 90.0;
          cell[5] = 90.0;
          break;
        case 3:
          cell[1] = cell[0];
          cell[3] = 90.0;
          cell[4] = 90.0;
          cell[5] = 90.0;
          break;
        case 4:
          cell[1] = cell[0];
          cell[3] = 90.0;
          cell[4] = 90.0;
          cell[5] = 120.0;
          break;
        case 5:
          cell[1] = cell[0];
          cell[3] = 90.0;
          cell[4] = 90.0;
          cell[5] = 120.0;
          break;
        case 6:
          cell[2] = cell[1] = cell[0];
          cell[3] = 90.0;
          cell[4] = 90.0;
          cell[5] = 90.0;
          break;
        default: {}
      }
  }



// These are not necessary for the GeneticAlgorithmRefinement class

  public double getData(int index) {
    return 0.0f;
  }

  public double getWeight(int index) {
    return 0.0f;
  }

  public double getFit(int index) {
    return 0.0f;
  }

  public double[] getRefinementIndexes() {
    return null;
  }

  public void setRw(double Rw) {
  }

  public void setR(double R) {
  }

  public void setRexp(double R) {
  }

/*  public void setFreeParameter(int index, double value) {
  }*/

  public void setFreeParameter(int index, double value) {
  }

/*  public void setFreeParameters(double[] parm) {
  }

  public void setErrors(double[] errors) {
  }*/

  public void setErrors(double[] errors) {
  }

  public void computeFit() {
  }

  public boolean checkBound(int j, double parmn) {
    return true;
  }

  public void backupallParameters() {
  }

  public void restoreParametersValues() {
  }

  public void mainfunction(boolean hasoutput, boolean refreshAll) {
  }

  public boolean reduceMemory() {
    return true;
  }

  public boolean singleFunctionComputing() {
    return false;
  }

  public int getNumberofIterations() {
    return 10;
  }

  public int prepareIteration() {
    return 0;
  }

  public OutputStream getResultStream() {
    return null;
  }

  public void endOfComputation() {
  }

  public boolean logOutput() {
    return false;
  }

  public void printInformations(OutputStream resultStream) {
  }

  public void closeLogResultFile() {
  }

  public void fittingFileOutput() {
  }

  public void prepareComputation() {
  }

  public void refreshFit(double[] fit, double[] parm, int[] controls) {
  }

  public IndexingOptionsDialog startDialog(Frame parentFrame) {
    return new IndexingOptionsDialog(parentFrame, "Indexing algorithm options", false, this);
  }

  class IndexingOptionsDialog extends myJDialog {

    GeneticAlgorithmIndexing indexing;
//		Hashtable originalindexes;
    JTextField iterationF = null;

    public IndexingOptionsDialog(Frame parent, String title, boolean modal, GeneticAlgorithmIndexing indexing) {

      super(parent, title, modal);

      this.indexing = indexing;

      Container pane = getContentPane();
      pane.setLayout(new BorderLayout(6, 6));

      JPanel optionPanel = new JPanel(new GridLayout(0, 1, 3, 3));
      pane.add(optionPanel);

      JPanel rowP;

      rowP = new JPanel(new FlowLayout(FlowLayout.CENTER, 6, 6));
      optionPanel.add(rowP);
      rowP.add(new JLabel("Number of iterations: "));
      iterationF = new JTextField(Integer.toString(indexing.getNumberofIterations()));
      rowP.add(iterationF);

      rowP = new JPanel(new FlowLayout(FlowLayout.CENTER, 6, 6));
      optionPanel.add(rowP);

      JButton jb = new JButton("Evolutionary algorithm options");
      rowP.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          evolutionaryOptions();
        }
      });

      JPanel panel1 = new JPanel();
      panel1.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      pane.add("South", panel1);

      panel1.add(jb = new JIconButton("GreenFlag.gif", "Start"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          setVisible(false);
          dispose();
        }
      });
      panel1.add(jb = new JCancelButton());
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          setVisible(false);
          dispose();
        }
      });
      if (!modal)
        setHelpButton(panel1);

      pack();
      setVisible(true);

    }

    public void evolutionaryOptions() {
      ((GeneticAlgorithmRefinement) indexing.getOptimizationAlgorithm()).getOptionsDialog(IndexingOptionsDialog.this.getFrameParent()).
          setVisible(true);
    }

    public void retrieveParameters() {
//      indexing.setResult();
      indexing.getOptimizationAlgorithm().setIterations(Integer.parseInt(iterationF.getText()));
      launchRefine computation = new launchRefine(indexing, outputPanel);
      computation.prepare();
      computation.launch();
    }

  }
}
