/*
 * @(#)FlatCCDReflectionSquareRoi.java created Jun 5, 2007 Caen
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
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
package ij.gui;

import ij.*;
import ij.io.FileInfo;
import ij.plugin.BrukerImageReader;
import ij.process.ImageProcessor;
import it.unitn.ing.rista.awt.Utility;
import it.unitn.ing.rista.diffr.DataFileSet;
import it.unitn.ing.rista.util.*;

import java.awt.*;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.Properties;

/**
 * The FlatCCDReflectionSquareRoi is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 5, 2007 3:23:10 PM $
 * @since JDK1.1
 */
public class FlatCCDReflectionSquareRoi extends LaueOvalRoi {

  double coneInterval = 5.0;
  double theta2Step = 0.02;
  double omega = 15.0;
  double chi = 0.0;
  double phi = 0.0;
  double det2Theta = 15.0;
  double phiDA = 0.0;
  double omegaDN = 0.0;
  double etaDA = 0.0;
  boolean calibrated = true;
  public static int minimumNumberOfPoints = 10;
  public static boolean batchProcessing = false;
  public static DataFileSet theData = null;
  public static String tiffFileName = "";

  // Creates a new OvalRoi. The ImagePlus argument can be null.
  public FlatCCDReflectionSquareRoi(ImagePlus imp, double radius) {
    super(imp, radius);
  }

  public void setConeInterval(double coneInterval) {
//    tmat = null;
    this.coneInterval = Math.abs(coneInterval);
  }

  public double getConeInterval() {
    return coneInterval;
  }

  public void set2thetaStep(double theta2Step) {
//    tmat = null;
    this.theta2Step = Math.abs(theta2Step);
  }

  public double get2thetaStep() {
    return theta2Step;
  }

  public void setOmega(double omega) {
//    tmat = null;
    this.omega = omega;
  }

  public double getOmega() {
    return omega;
  }

  public void setChi(double chi) {
//    tmat = null;
    this.chi = chi;
  }

  public double getChi() {
    return chi;
  }

  public void setPhi(double phi) {
//    tmat = null;
    this.phi = phi;
  }

  public double getPhi() {
    return phi;
  }

  public void setDet2Theta(double value) {
//    tmat = null;
    this.det2Theta = value;
  }

  public double getDet2Theta() {
    return det2Theta;
  }

  public void setPhiDA(double phi) {
//    tmat = null;
    this.phiDA = phi;
  }

  public double getPhiDA() {
    return phiDA;
  }

  public void setOmegaDN(double omega) {
//    tmat = null;
    this.omegaDN = omega;
  }

  public double getOmegaDN() {
    return omegaDN;
  }

  public void setEtaDA(double eta) {
//    tmat = null;
    this.etaDA = eta;
  }

  public double getEtaDA() {
    return etaDA;
  }

  public void setCalibrated() {
    calibrated = true;
  }

  public void setUncalibrated() {
    calibrated = false;
  }

//  double[][] tmat = null;

  public void updateSelection() {

//    tmat = ConvertImageToSpectra.getTransformationMatrix(getOmegaDN(), getPhiDA(), getDet2Theta(), getOmega());
    super.updateSelection();
  }

//  int count = 0;

  public double[] getXCoordSym(double ycoord) {
    double[] coordX = new double[2];
    ImageProcessor ip = imp.getProcessor();
    if (ip == null) {
      System.out.println("No image processor!");
      return coordX;
    }

//    int maxGray = 256;

/*    int minX = 0;
    int maxX = imp.getWidth();
    int minY = 0;
    int maxY = imp.getHeight();
    if (usableRectangle != null) {
      minX = (int) (usableRectangle.getMinX());
      maxX = (int) (usableRectangle.getMaxX());
      minY = (int) (usableRectangle.getMinY());
      maxY = (int) (usableRectangle.getMaxY());
    }*/

//    count++;
    ycoord *= coordTrasfY;
    double tan2t = Math.tan(getCircle() * Constants.DEGTOPI);
    tan2t *= tan2t;
    double centerX = getX();
    double centerY = getY();
    double zs = 0.0;

    double[] ABC = ConvertImageToSpectra.getABCForYNew(getOmegaDN(), getPhiDA(), getEtaDA(), getDet2Theta(), getOmega(),
        centerX, ycoord, centerY, getRadius(), getCircle());


    double a = ABC[0];
    double b = ABC[1];
    double c = ABC[2];

    if (Math.abs(a) > 1.0E-9) {
      double sqrt = Math.sqrt(b * b - 4.0 * a * c);
      coordX[0] = -(b + sqrt) / (2.0 * a);
      coordX[1] = -(b - sqrt) / (2.0 * a);
    } else {
      if (Math.abs(b) > 1.0E-9) {
        coordX[0] = -c / b;
        coordX[1] = -c / b;
      } else {
        coordX[0] = -9999999.99;
        coordX[1] = -9999999.99;
      }
    }

    coordX[0] /= coordTrasfX;
    coordX[1] /= coordTrasfX;
    return coordX;
  }

  public double[][] getIntervalPixels() {
    ImageProcessor ip = imp.getProcessor();
    if (ip == null) {
      System.out.println("No image processor!");
      return null;
    }

//    int maxGray = 256;

    int minX = 0;
    int maxX = imp.getWidth();
    int minY = 0;
    int maxY = imp.getHeight();
    double halfx = 0.5 * imp.getWidth();
    double halfy = 0.5 * imp.getHeight();
    if (usableRoi != null) {
	    Rectangle rect = usableRoi.getBounds();
	    minX = rect.x;
	    maxX = rect.x + rect.width;
	    minY = rect.y;
	    maxY = rect.y + rect.height;
//	    System.out.println("Using roi: " + minX + " " + maxX + " " + minY + " " + maxY);
    }

    Properties prop = imp.getProperties();
    if (prop != null && prop.containsKey(BrukerImageReader.brukerImage)) {
      MaudPreferences.setPref("brukerImage.xminValue", minX);
      MaudPreferences.setPref("brukerImage.xmaxValue", maxX);
      MaudPreferences.setPref("brukerImage.yminValue", minY);
      MaudPreferences.setPref("brukerImage.ymaxValue", maxY);
    } else {
      MaudPreferences.setPref("squareRoi.xminValue", minX);
      MaudPreferences.setPref("squareRoi.xmaxValue", maxX);
      MaudPreferences.setPref("squareRoi.yminValue", minY);
      MaudPreferences.setPref("squareRoi.ymaxValue", maxY);
    }
//    System.out.println("Min max " + minX + " " + maxX + " " + minY + " " + maxY);
    int npointsX = maxX - minX;
    int npointsY = maxY - minY;
    double[] intensity = new double[npointsX * npointsY];
    double[] x = new double[npointsX * npointsY];
    double[] y = new double[npointsX * npointsY];
    double centerX = getX();
    double centerY = getY();
//    System.out.println("Center " + centerX + " " + centerY);
    int index = 0;
    boolean cutOuterCircle = false;
    if (prop != null)
      cutOuterCircle = prop.containsKey(BrukerImageReader.radiusToCutString);
    double radiusToCut = 1E150;
    if (cutOuterCircle)
      radiusToCut = Double.parseDouble(prop.getProperty(BrukerImageReader.radiusToCutString));
    radiusToCut *= radiusToCut;
    for (int ix = minX; ix < maxX; ix++) {
      for (int iy = minY; iy < maxY; iy++) {
        float intensityValue = ip.getPixelValue(ix, iy);
        double dx = ix - halfx + 1;
        double dy = iy - halfy + 1;
        if ((usableRoi != null && usableRoi.contains(ix, iy)) || (usableRoi == null && dx * dx + dy * dy < radiusToCut))
          intensity[index++] = intensityValue;
        else
          intensity[index++] = -999;
      }
    }
//    System.out.println(index);
    getXYFromPixelIndex(minX, maxX, minY, maxY, coordTrasfX, coordTrasfY, x, y, centerX, centerY);
/*	  System.out.println("Conversion to xy coordinates done: " + minX + " " + maxX + " " + minY + " " + maxY + " " +
			  coordTrasfX + " " + coordTrasfY + " " + x[3] + " " + y[3] + " " + centerX + " " + centerY);*/

    double[] theta2 = new double[npointsX * npointsY];
    double[] eta = new double[npointsX * npointsY];
    double detectorDistance = getRadius();
/*	  System.out.println("Converting to 2theta, eta: " + omega + " " +
			  det2Theta + " " + phiDA + " " +
			  omegaDN + " " + etaDA + " " + detectorDistance);*/
    Angles.getTheta2EtaFromXYPixelDetector(x, y, theta2, eta, omega, det2Theta, phiDA,
        omegaDN, etaDA, detectorDistance, 0.0);
/*    System.out.println("Conversion to theta, eta angles done! " + theta2[0] + " " + theta2[theta2.length - 1] + " " +
        eta[0] + " " + eta[eta.length - 1]);*/


    double min2theta = 1.0E32; //2.0 * Math.PI;
    double max2theta = -1.0E32; //-2 * Math.PI;
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

//    System.out.println("BrukerImageReader, angles: " + detectorDistance + " " + min2theta + " " + max2theta + " " +
//        theta2Step * Constants.DEGTOPI + " " + mineta + " " + maxeta + " " + coneInterval * Constants.DEGTOPI);
/*	  System.out.println("BrukerImageReader, angles: " + theta2.length + " " + eta.length + " " + intensity.length + " " + x.length + " " + y.length + " " + detectorDistance +
			  " " + min2theta + " " + max2theta + " " + theta2Step * Constants.DEGTOPI + " " + mineta + " " + maxeta + " " +
			  coneInterval * Constants.DEGTOPI);*/
    double[][][] profile = Angles.spectraFromPixelsByEtaTheta2(theta2, eta, intensity, x, y, detectorDistance,
        min2theta, max2theta, theta2Step * Constants.DEGTOPI,
        mineta, maxeta, coneInterval * Constants.DEGTOPI);

//    System.out.println("Conversion to spectra done!");
//    System.out.println("BrukerImageDatafile: " + profile.length + " " + profile[0].length + " " + min2theta * Constants.PITODEG + " " +
//        theta2Step + " " + mineta * Constants.PITODEG + " " + coneInterval);
    saveAsText(profile, profile[0].length, 0, profile[0][0].length, min2theta * Constants.PITODEG, theta2Step,
        mineta * Constants.PITODEG, coneInterval);
//      System.out.println(profile[ix] + " " + maxGray);
    return profile[2];

  }

  public static void getXYFromPixelIndex(int minX, int maxX, int minY, int maxY,
                                               double pixelWidth, double pixelHeight,
                                               double[] x, double[] y, double centerX, double centerY) {
    // width, centerX, centerY in pixels
    // the pixel index should be something like: i = nx + ny * width
    int index = 0;
/*    double xmin = 1.0E33;
    double xmax = -1.0E33;
    double ymin = 1.0E33;
    double ymax = -1.0E33;*/
    for (int i = minX; i < maxX; i++) {
      for (int j = minY; j < maxY; j++) {
        x[index] = i * pixelWidth - centerX;
        y[index] = j * pixelHeight - centerY;
/*        if (xmin > x[index])
          xmin = x[index];
        if (ymin > y[index])
          ymin = y[index];
        if (xmax < x[index])
          xmax = x[index];
        if (ymax < y[index])
          ymax = y[index];*/
        index++;
      }
    }
//    System.out.println(xmin + " " + xmax + " " + ymin + " " + ymax);
  }

  static int CCD_OVAL_STEP_ROI = -1002; // to be unique

  public int getType() {
    return CCD_OVAL_STEP_ROI;
  }

  void saveAsText(double[][][] profile, int nprofiles, int startX, int endX, double theta2Start, double theta2Step,
                  double etaStart, double etaStep) {

    DataFileSet data = null;
    String filename = null;
    if (!batchProcessing) {
      data = AreaImage.getData();
      filename = Utility.openFileDialog(new Frame(), "Save as CIF (.esg)...",
                 FileDialog.SAVE, data.getFilePar().getDirectory(), null, "put a name (with extension).esg");
    } else {
      data = theData;
      filename = tiffFileName;
    }
    if (filename == null)
      return;

    String[] folderAndName = Misc.getFolderandName(filename);

    String folder = folderAndName[0];
    filename = folderAndName[1];

    if (filename == null) return;
    if (!filename.endsWith(".esg"))
      filename = filename + ".esg";

    IJ.wait(250);  // give system time to redraw ImageJ window
    IJ.showStatus("Saving plot values...");

    saveAsText(profile, nprofiles, startX, endX, theta2Start, theta2Step, etaStart, etaStep,
        folder, filename, imp.getCalibration().getUnit(), radius, omega, chi, phi, det2Theta, calibrated);
    IJ.wait(500);  // give system time to save the file
    if (filename != null && data != null)
      data.addDataFileforName(folder + filename, false);

  }

  public static void saveAsText(double[][][] profile, int nprofiles, int startX, int endX, double theta2Start, double theta2Step,
                  double etaStart, double etaStep, String folder, String filename, String  distanceUnit,
                  double detectorDistance, double omega, double chi, double phi, double theta2, boolean calibrated) {

//	  System.out.println(filename + " " + nprofiles + " " + startX + " " + endX + " " + theta2Start + " " + theta2Step + " " + etaStart + " " + etaStep);
    String title = "noTitle";
    BufferedWriter output = Misc.getWriter(folder, filename);
//    System.out.println(folder+filename);
//    System.out.println(output);
    try {
      output.write("_pd_block_id " + title + "|#" + 0);
      output.newLine();
      output.newLine();
      for (int ij = 0; ij < nprofiles; ij++) {
        if (ij == 0) {
          output.write("_diffrn_detector 2D");
          output.newLine();
          output.write("_diffrn_detector_type CCD like");
          output.newLine();
          output.write("_pd_meas_step_count_time ?");
          output.newLine();
          output.write("_diffrn_measurement_method diffraction_image");
          output.newLine();
          output.write("_diffrn_measurement_distance_unit " + distanceUnit);
          output.newLine();
          output.write("_pd_instr_dist_spec/detc " + detectorDistance);
          output.newLine();
          output.write("_diffrn_radiation_wavelength ?");
          output.newLine();
          output.write("_diffrn_source_target ?");
          output.newLine();
          output.write("_diffrn_source_power ?");
          output.newLine();
          output.write("_diffrn_source_current ?");
          output.newLine();
          output.write("_pd_meas_angle_omega " + Fmt.format(omega));
          output.newLine();
          output.write("_pd_meas_angle_chi " + Fmt.format(chi));
          output.newLine();
          output.write("_pd_meas_angle_phi " + Fmt.format(phi));
          output.newLine();
	        output.write("_pd_meas_orientation_2theta " + Fmt.format(theta2));
	        output.newLine();
          output.write("_riet_par_spec_displac_x 0");
          output.newLine();
          output.write("_riet_par_spec_displac_y 0");
          output.newLine();
          output.write("_riet_par_spec_displac_z 0");
          output.newLine();
          output.write("_riet_meas_datafile_calibrated false");
          output.newLine();
        }

        int checkPoints = 0;
        for (int i = startX; i < endX; i++) {
          double intensity = profile[2][ij][i - startX];
          if (Double.isNaN(intensity))
            intensity = -1;
          if (intensity >= 0.0) {
            checkPoints++;
          }
        }
        if (checkPoints > minimumNumberOfPoints) {
          output.write("_pd_block_id " + title + "|#" + ij);
          output.newLine();
          output.newLine();
          output.write("_pd_meas_angle_eta " + Fmt.format(etaStart + etaStep * ij));
          output.newLine();
          output.newLine();
          output.write("loop_");
          output.newLine();
          output.write("_pd_meas_position_x _pd_meas_position_y _pd_meas_intensity_total");
          output.newLine();
//        System.out.println(imp.getCalibration().pixelWidth);
//        System.out.println(imp.getCalibration().pixelHeight);
          for (int i = startX; i < endX; i++) {
            double intensity = profile[2][ij][i - startX];
            if (Double.isNaN(intensity))
              intensity = -1;
            if (intensity >= 0) {
              output.write(" " + Fmt.format(profile[0][ij][i - startX]) + " " + Fmt.format(profile[1][ij][i - startX]) + " " + Fmt.format(intensity));
              output.newLine();
            }
          }
          output.newLine();
        }
      }
    } catch (IOException io) {
      io.printStackTrace();
//      LogSystem.printStackTrace(io);
    }

    try {
      output.close();
    } catch (IOException io) {
	    io.printStackTrace();
//      LogSystem.printStackTrace(io);
    }
  }

}
