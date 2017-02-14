/*
 * @(#)BrukerImageDatafile.java created Jul 9, 2009 Caen
 *
 * Copyright (c) 2009 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.diffr.data;

import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.diffr.DiffrDataFile;
import it.unitn.ing.rista.util.*;

import java.io.File;
import java.util.Properties;

import ij.plugin.BrukerImageReader;
import ij.gui.FlatCCDReflectionSquareRoi;

/**
 * The BrukerImageDatafile is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jul 9, 2009 3:25:50 PM $
 * @since JDK1.1
 */
public class BrukerImageDatafile extends it.unitn.ing.rista.diffr.MultDiffrDataFile {

  public BrukerImageDatafile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".gfrm";
  }

  public BrukerImageDatafile() {
    identifier = ".gfrm";
  }


  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;

    int[] pixelDimension = new int[2];
    double[] dimension = new double[14];
    String[] properties = new String[4];
//    new Opener().open();
//		OpenDialog od = new OpenDialog("Open...", "");
    String directory = getFolder(); //od.getDirectory();
    String name = getLabel(); //od.getFileName();
//    System.out.println("Opening file: "+ directory + name);
    if (name != null) {
      File file = new File(directory + name);
      if (file.exists()) {
//        imp.show();
        BrukerImageReader fileReader = new BrukerImageReader();
        float[] buffer = fileReader.loadImage(file, pixelDimension, dimension, properties);
        if (buffer != null) {
//          double centerX = MaudPreferences.getDouble("pixelDetector.centerX", 0.5 * pixelDimension[0]);
//          double centerY = MaudPreferences.getDouble("pixelDetector.centerY", 0.5 * pixelDimension[1]);
          double coneInterval = MaudPreferences.getDouble("brukerImage.defaultDiffractionConeInterval", 5.0);
          double theta2Step = MaudPreferences.getDouble("brukerImage.defaultDiffractionStepAngle", 0.02);
//          double azimuthal = MaudPreferences.getDouble("pixelDetector.defaultAzimuthalAngle", 0.0);
//          double phiDetector = MaudPreferences.getDouble("pixelDetector.defaultPhiAngle", 0.0);

          int minX = MaudPreferences.getInteger("brukerImage.xminValue", 0);
          int maxX = MaudPreferences.getInteger("brukerImage.xmaxValue", pixelDimension[0]);
          int minY = MaudPreferences.getInteger("brukerImage.yminValue", 0);
          int maxY = MaudPreferences.getInteger("brukerImage.ymaxValue", pixelDimension[1]);
//    System.out.println("Min max " + minX + " " + maxX + " " + minY + " " + maxY);
          int npointsX = maxX - minX;
          int npointsY = maxY - minY;
          double[] intensity = new double[npointsX * npointsY];
          double[] x = new double[npointsX * npointsY];
          double[] y = new double[npointsX * npointsY];
          int index = 0;
//          boolean cutOuterCircle = properties[0].equalsIgnoreCase("true");
          double radiusToCut = dimension[2];
          radiusToCut *= radiusToCut;
          double halfx = 0.5 * pixelDimension[0];
          double halfy = 0.5 * pixelDimension[1];
          for (int ix = minX; ix < maxX; ix++) {
            for (int iy = minY; iy < maxY; iy++) {
              double dx = ix - halfx + 1;
              double dy = iy - halfy + 1;
              if (dx * dx + dy * dy < radiusToCut)
                intensity[index++] = buffer[ix + iy * pixelDimension[0]];
              else
                intensity[index++] = -999;
            }
          }
          FlatCCDReflectionSquareRoi.getXYFromPixelIndex(minX, maxX, minY, maxY, dimension[0], dimension[1],
              x, y, dimension[11], dimension[12]);
//    System.out.println("Conversion to xy coordinates done!");

          double[] theta2 = new double[npointsX * npointsY];
          double[] eta = new double[npointsX * npointsY];
          Angles.getTheta2EtaFromXYPixelDetector(x, y, theta2, eta, dimension[3],
              dimension[5], 0, dimension[6], 0, dimension[7], 0);
          System.out.println("BrukerImageDatafile conver. to theta, eta " + theta2[0] + " " + theta2[theta2.length - 1] + " " +
              eta[0] + " " + eta[eta.length - 1]);
          double min2theta = 2.0 * Math.PI;
          double max2theta = -2 * Math.PI;
          double mineta = 2 * Math.PI;
          double maxeta = -2 * Math.PI;
          for (int i = 0; i < theta2.length; i++) {
            if (min2theta > theta2[i])
              min2theta = theta2[i];
            if (max2theta < theta2[i])
              max2theta = theta2[i];
            if (mineta > eta[i])
              mineta = eta[i];
            if (maxeta < eta[i])
              maxeta = eta[i];
          }
          double nmineta = 0.0;
          int i = 0;
          while (nmineta < mineta)
            nmineta = i++ * coneInterval * Constants.DEGTOPI;
          while (nmineta >= mineta + coneInterval * Constants.DEGTOPI)
            nmineta = i-- * coneInterval * Constants.DEGTOPI;
          mineta = nmineta;
          double nmintheta = 0.0;
          i = 0;
          while (nmintheta < min2theta)
            nmintheta = i++ * theta2Step * Constants.DEGTOPI;
          while (nmintheta >= min2theta + theta2Step * Constants.DEGTOPI)
            nmintheta = i-- * theta2Step * Constants.DEGTOPI;
          min2theta = nmintheta;

          System.out.println("BrukerImageDatafile, angles: " + dimension[7] + " " + min2theta + " " + max2theta + " " +
              theta2Step * Constants.DEGTOPI + " " + mineta + " " + maxeta + " " + coneInterval * Constants.DEGTOPI);
          double[][][] profile = Angles.spectraFromPixelsByEtaTheta2(theta2, eta, intensity, x, y, dimension[7],
              min2theta, max2theta, theta2Step * Constants.DEGTOPI,
              mineta, maxeta, coneInterval * Constants.DEGTOPI);

//    System.out.println("Conversion to spectra done!");
          double xmin = min2theta * Constants.PITODEG;
          double etaStart = mineta * Constants.PITODEG;
          int dotLocation = name.indexOf(".gfrm");
          String filename = name.substring(0, dotLocation) + ".esg";
          System.out.println("BrukerImageDatafile: " + profile.length + " " + profile[0].length + " " + xmin + " " +
              theta2Step + " " + etaStart + " " + coneInterval);
          FlatCCDReflectionSquareRoi.saveAsText(profile, profile.length, 0, profile[0].length, xmin, theta2Step,
              etaStart, coneInterval, directory, filename, "mm", dimension[7], dimension[3], dimension[4], dimension[6],
              false);
          setLabel(filename);
          for (int spectrumIndex = 0; spectrumIndex < profile.length; spectrumIndex++) {
            String numberString = Integer.toString(spectrumIndex);
            DiffrDataFile datafile = addDiffrDatafile(numberString);
            boolean atmpB = datafile.isAbilitatetoRefresh;
            datafile.isAbilitatetoRefresh = false;

            datafile.setDataType(DIFFRACTION_IMAGE);
            datafile.setAngleValue(0, dimension[3]);
            datafile.setAngleValue(1, dimension[4]);
            datafile.setAngleValue(2, dimension[6]);
            datafile.setAngleValue(3, etaStart + spectrumIndex * coneInterval);

/*            datafile.setField("_riet_meas_datafile_calibrated", "true", "0", "0", "0", false, null, null, null, null,
                false);
*/
            datanumber = 0;
            i = 0;
            while (i < profile[2][spectrumIndex].length)
              if (profile[2][spectrumIndex][i++] >= 0)
                datanumber++;
            datafile.datanumber = datanumber;
            System.out.println(datafile.datanumber);
            if (datanumber < 3)
              datafile.setCompute(false);
            datafile.initData(datanumber);
            datafile.constantstep = false;
            datafile.dspacingbase = false;

            i = 0;
            int indexPoint = 0;
            while (i < profile[2][spectrumIndex].length) {
              double intensityValue = profile[2][spectrumIndex][i];
              if (intensityValue >= 0) {
                datafile.setXData(i, i);
                datafile.setXImage(indexPoint, profile[0][spectrumIndex][i]);
                datafile.setYImage(indexPoint, profile[1][spectrumIndex][i]);
                datafile.setYData(indexPoint, intensityValue);
                double tmpweight = Math.sqrt(datafile.getYData(indexPoint));
                if (tmpweight != 0.0)
                  datafile.setWeight(indexPoint, 1.0 / tmpweight);
                else
                  datafile.setWeight(indexPoint, 1.0);
                indexPoint++;
              }
              i++;
            }
            datafile.isAbilitatetoRefresh = atmpB;
            loadSuccessfull = true;
            datafile.dataLoaded = true;
          }
        }
      }
      isAbilitatetoRefresh = tmpB;
    }
    return loadSuccessfull;
  }



}
