/*
 * @(#)BasicIndexingUtilities.java created Feb 24, 2005 Barcellona
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

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.DiffrDataFile;
import it.unitn.ing.rista.awt.*;

import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.util.Vector;
import java.util.StringTokenizer;


/**
 * The BasicIndexingUtilities is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:57 $
 * @since JDK1.1
 */

public class BasicIndexingUtilities {
  public static void fourierSmoothing(DiffrDataFile datafile) {
    int dtanumber = datafile.computeDataNumber();
    int fftlength = MoreMath.getNextPowerof2(dtanumber);
    double[] fft = new double[fftlength];
    int i = 0;
    for (; i < dtanumber; i++)
      fft[i] = datafile.getFit(i + datafile.startingindex);
    for (i = dtanumber; i < fftlength; i++)
      fft[i] = 0.0;

    FFT.realfft(fftlength, fft);

    for (i = 0; i < dtanumber; i++)
      datafile.setPhasesFit(i + datafile.startingindex, fft[i]);

  }

  public static double[] modifyFFT(double[] fft) {
    int fftlength = fft.length;
    double[] newfft = new double[fftlength];
    for (int i = 0; i < fftlength; i++)
      newfft[i] = fft[i];
    double cutFactor = MaudPreferences.getDouble("smoothing.fourierZeros", 0.6);
    if (cutFactor < 0 || cutFactor >= 1) {
      cutFactor = 0.6;
      MaudPreferences.setPref("smoothing.fourierZeros", Double.toString(cutFactor));
    }
    int cut = (int) (fftlength * cutFactor);
    int mode = MaudPreferences.getInteger("smoothing.fourierMode", 1);
    double mfactor = MaudPreferences.getDouble("smoothing.multiplyingFactor", 3.9);
    int zeros = (fftlength - cut) / 2;
    int fftlength2 = fftlength / 2;
    double step = 2.0 / cut * mfactor;
    for (int i = zeros; i < fftlength2; i++) {
//    System.out.println(fftlength + " " + fftlength2 + " " + i);
      switch (mode) {
        case 0:
          newfft[i] = 0.0;
          newfft[fftlength - 1 - i] = 0.0;
          break;
        case 1:
          double factor = 1.0 - step * (i - zeros);
          if (factor < 0)
            factor = 0.0;
//            System.out.println(factor + " " + step + " " + (i - zeros));
          newfft[i] *= factor;
          newfft[fftlength - 1 - i] *= factor;
          break;
        case 2:
          factor = Math.PI / (fftlength2 - zeros) * (i - zeros) * mfactor;
          if (factor > Math.PI)
            factor = Math.PI;
          if (factor < 0.0)
            factor = 0.0;
          factor = (Math.cos(factor) + 1.0) / 2.0;
          if (factor > 1)
            factor = 1.0;
          if (factor < 0.0)
            factor = 0.0;
          newfft[i] *= factor;
          newfft[fftlength - 1 - i] *= factor;
          break;
        default:
        {
        }
      }
    }
    return newfft;
  }

  public static void inverseFourierSmoothing(double[] fft, DiffrDataFile adatafile) {
    int dtanumber = adatafile.computeDataNumber();
    int fftlength = fft.length;
    FFT.realifft(fftlength, fft);
    double scale = 1.0 / fftlength;
    for (int i = 0; i < dtanumber; i++)
      fft[i] *= scale;
// this is temporary
    for (int i = 0; i < dtanumber; i++)
      adatafile.setPhasesFit(i + adatafile.startingindex, fft[i]);
  }

  public static void backgroundSubtraction(DiffrDataFile datafile) {
    datafile.backgroundSubtraction();
  }

  public static void kalpha2Stripping(DiffrDataFile datafile) {
    datafile.kalpha2Stripping();
  }

  public static double[][] peaksLocation(DiffrDataFile datafile) {
    int dtanumber = datafile.computeDataNumber();
    double[] derivative2 = new double[dtanumber];

    return datafile.peaksLocation(derivative2);
  }

  public static void exportPeaksDicvol91(double[][] peaksList, DiffrDataFile datafile) {
    if (peaksList == null)
      return;

    int mode = 2;
    if (datafile.dspacingbase || PlotDataFile.checkScaleModeX() == 1)
      mode = 3;
    boolean[] symmetrySwitch = new boolean[6];
    for (int i = 0; i < 6; i++)
      symmetrySwitch[i] = true;
    double wave = datafile.getDataFileSet().getInstrument().getRadiationType().getMeanRadiationWavelength();
    double error = 0.05;

    int numberOfPeaks = peaksList[0].length;

    String symmetry = "";
    for (int i = 0; i < 6; i++)
      if (symmetrySwitch[i])
        symmetry += " 1";
      else
        symmetry += " 0";
    String filename = Utility.openFileDialog(new Frame(), "Save peak file as (dicvol04)", FileDialog.SAVE,
        "", null, "untitled.dat");
    BufferedWriter output = null;
    if (filename != null) {
      if (filename.toLowerCase().endsWith(".pk")) {
        try {
          output = Misc.getWriter(filename);
          output.write(Fmt.format(wave));
          output.newLine();
          for (int i = 0; i < numberOfPeaks; i++) {
            output.write(Fmt.format(peaksList[0][i]) + "," + Fmt.format(peaksList[1][i]));
            output.newLine();
          }
        } catch (IOException io) {
        }
        try {
          output.flush();
          output.close();
        } catch (IOException io) {
        }
      } else {
        try {
          output = Misc.getWriter(filename);
          output.write("Peaks from: " + datafile.getTitle());
          output.newLine();
          output.write(numberOfPeaks + " " + mode + symmetry);
          output.newLine();
          output.write("30. 30. 30. 0. 4000. 0. 0.");
          output.newLine();
          output.write(Fmt.format(wave) + " 0. 0. 0. 0.");
          output.newLine();
          output.write("1. 0. 1 0 0");
          output.newLine();
          for (int i = 0; i < numberOfPeaks; i++) {
            output.write(Fmt.format(peaksList[0][i]) + " " + Fmt.format(error));
            output.newLine();
          }
        } catch (IOException io) {
        }
        try {
          output.flush();
          output.close();
        } catch (IOException io) {
        }
      }
    }
  }

  public static void exportPeaksDicvol91(double[][] peaksList, double wave, int mode,
                                         String title) {
    if (peaksList == null)
      return;

    double minError = MaudPreferences.getDouble("dspacing.minErrorDicvol", 0.00001);
    double maxError = MaudPreferences.getDouble("dspacing.maxErrorDicvol", 0.0003);
    boolean[] symmetrySwitch = new boolean[6];
    for (int i = 0; i < 5; i++)
      symmetrySwitch[i] = true;
    symmetrySwitch[5] = false;  // triclinic off by default
//    double error = 0.05;

    int numberOfPeaks = peaksList[0].length;

    String symmetry = "";
    for (int i = 0; i < 6; i++)
      if (symmetrySwitch[i])
        symmetry += " 1";
      else
        symmetry += " 0";
    String filename = Utility.openFileDialog(new Frame(), "Save peak file as (dicvol06)", FileDialog.SAVE,
        "", null, "untitled.dat");
    BufferedWriter output = null;
    if (filename != null) {
      if (filename.toLowerCase().endsWith(".pk")) {
        try {
          output = Misc.getWriter(filename);
          output.write(Fmt.format(wave));
          output.newLine();
          for (int i = 0; i < numberOfPeaks; i++) {
            output.write(Fmt.format(peaksList[0][i]) + "," + Fmt.format(peaksList[1][i]));
            output.newLine();
          }
        } catch (IOException io) {
        }
        try {
          output.flush();
          output.close();
        } catch (IOException io) {
        }
      } else {
        try {
          output = Misc.getWriter(filename);
          output.write("Peaks from: " + title);
          output.newLine();
          output.write(numberOfPeaks + " " + mode + symmetry);
          output.newLine();
          output.write("30. 30. 30. 0. 4000. 0. 0.");
          output.newLine();
          output.write(Fmt.format(wave) + " 0. 0. 0. 0.");
          output.newLine();
          output.write("1. 0. 0 1 1 0");
          output.newLine();
          for (int i = 0; i < numberOfPeaks; i++) {
            if (peaksList[1][i] < minError)
              peaksList[1][i] = minError;
            if (peaksList[1][i] > maxError)
              peaksList[1][i] = maxError;
            output.write(Fmt.format(peaksList[0][i]) + " " + Fmt.format(peaksList[1][i]));
            output.newLine();
          }
        } catch (IOException io) {
        }
        try {
          output.flush();
          output.close();
        } catch (IOException io) {
        }
      }
    }
  }

  public static Vector importPeakList(JFrame parentFrame) {
    final String filename = Utility.browseFilename(parentFrame, "Import peak list (txt format)");
    if (filename != null) {
      BufferedReader reader = Misc.getReader(filename);
      if (reader != null) {
        double wave = 1.5405981;
        double position;
        Vector list = new Vector(10, 10);
        try {
          String line = null;
          boolean endoffile = false;
          do {
            line = reader.readLine();
            if (line != null) {
              if (filename.toLowerCase().endsWith(".pk")) {
                StringTokenizer st = new StringTokenizer(line, "= \t\r\n");
                if (st.hasMoreTokens())
                  wave = Double.parseDouble(st.nextToken());
              } else if (line.indexOf("Wavelength") > 0) {
                line = line.substring(line.indexOf('='));
                StringTokenizer st = new StringTokenizer(line, "= \t\r\n");
                if (st.hasMoreTokens())
                  wave = Double.parseDouble(st.nextToken());
              }
              line = reader.readLine();
              while (!endoffile) {
                line = reader.readLine();
                if (line != null) {
                  StringTokenizer st = new StringTokenizer(line, "=, \t\r\n");
                  if (st.hasMoreTokens()) {
                    position = Double.parseDouble(st.nextToken());
                    list.addElement(new Double(position));
                  } else
                    endoffile = true;
                } else
                  endoffile = true;
              }
            } else
              endoffile = true;
          } while (!endoffile);
        } catch (IOException e) {
          System.out.println("Error in loading the data file! Try to remove this data file");
        }
        try {
          reader.close();
        } catch (IOException e) {
        }
        list.addElement(new Double(wave));
        return list;
      }
    }
    return null;
  }

  public static void exportPeaksForMcMaille(double[][] peaksList, double wave, String title) {
    if (peaksList == null)
      return;

    int numberOfPeaks = peaksList[0].length;

    String filename = Utility.openFileDialog(new Frame(), "Save peak file for McMaille", FileDialog.SAVE,
        "", null, "untitled.dat");
    BufferedWriter output = null;
    if (filename != null) {
        try {
          output = Misc.getWriter(filename);
          output.write(title);
          output.newLine();
          output.write("! Wavelength, zeropoint and NGRID (-3 = no triclinic)");
          output.newLine();
          output.write(Fmt.format(wave) + " 0.0 -3");
          output.newLine();
          output.write("! Couples of 2-theta and intensities");
          output.newLine();
          for (int i = 0; i < numberOfPeaks; i++) {
            output.write("   " + Fmt.format(peaksList[0][i]) + " " + Fmt.format(peaksList[1][i]));
            output.newLine();
          }
        } catch (IOException io) {
        }
        try {
          output.flush();
          output.close();
        } catch (IOException io) {
        }
      }
  }
}
