/*
 * @(#)LaueCircleStepRoi.java created Aug 8, 2003 Berkeley
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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
import ij.process.ImageProcessor;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.Utility;

import java.awt.*;
import java.io.*;
import java.util.Properties;


/**
 * The LaueCircleStepRoi is a class
 *  
 * @version $Revision: 1.8 $, $Date: 2006/02/02 16:11:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class LaueCircleStepRoi extends LaueCircleRoi {

  public int nprofiles = 72;
  double omega = 0.0;
  double chi = 0.0;
  double phi = 0.0;
  boolean calibrated = true;
  private boolean reflectionImage = false;

  // Creates a new OvalRoi. The ImagePlus argument can be null.
  public LaueCircleStepRoi(ImagePlus imp, double radius) {
    super(MaudPreferences.getDouble("image2D.centerX", imp.getWidth() * imp.getCalibration().pixelWidth / 2.0),
            MaudPreferences.getDouble("image2D.centerY", imp.getHeight() * imp.getCalibration().pixelHeight / 2.0),
            MaudPreferences.getDouble("image2D.roiCircle",
            imp.getWidth() * imp.getCalibration().pixelWidth / 2.0),
            imp);
    setRadius(radius);
  }

  public void setOmega(double omega) {
    this.omega = omega;
  }

  public double getOmega() {
    return omega;
  }

  public void setChi(double chi) {
    this.chi = chi;
  }

  public double getChi() {
    return chi;
  }

  public void setPhi(double phi) {
    this.phi = phi;
  }

  public double getPhi() {
    return phi;
  }

  public void setCalibrated() {
    calibrated = true;
  }

  public void setUncalibrated() {
    calibrated = false;
  }

  public double[][] getPixels() {
    ImageProcessor ip = imp.getProcessor();
    if (ip == null) {
      System.out.println("No image processor!");
      return null;
    }
	  int pointsPerPixel = MaudPreferences.getInteger("image2D.pointsPerPixel", 1);
    int minX = 0;
    int maxX = xMax;
    int minY = 0;
    int maxY = yMax;

	  if (usableRoi != null) {
		  Rectangle usableRectangle = usableRoi.getBounds();
		  if (usableRectangle != null) {
			  minX = (int) usableRectangle.getMinX();
			  maxX = (int) usableRectangle.getMaxX();
			  minY = (int) usableRectangle.getMinY();
			  maxY = (int) usableRectangle.getMaxY();
		  }
	  }

	  int npoints = Math.max(maxX - minX, maxY - minY) * pointsPerPixel;//  (int) getPixelCircleX(); //xMax / 2;  / coordTrasf
	  double stepIntegration = imp.getCalibration().pixelWidth / pointsPerPixel;
    double coneStep = (finalAngle - startingAngle) / nprofiles;
    double halfConeStep = startingAngle - coneStep / 2.0;
    double[][] profile = new double[nprofiles][npoints];
    for (int etaIndex = 0; etaIndex < nprofiles; etaIndex++) {
      double startAngle = coneStep * etaIndex + halfConeStep;
      double endAngle = startAngle + coneStep;
//	    System.out.println(startAngle + " " + endAngle + " " + npoints + " " + minX + " " + maxX + " " + minY + " " + maxY + " " + stepIntegration);
      double[][] single_profile = getPixels(startAngle, endAngle, npoints, minX, maxX, minY, maxY, stepIntegration);
      for (int ix = 0; ix < npoints; ix++) {
        profile[etaIndex][ix] = single_profile[0][ix];
      }
    }
    return profile;

  }

	public double[][] getIntervalPixels() {
		ImageProcessor ip = imp.getProcessor();
		if (ip == null) {
			System.out.println("No image processor!");
			return null;
		}
		int pointsPerPixel = MaudPreferences.getInteger("image2D.pointsPerPixel", 1);
		int minX = 0;
		int maxX = xMax;
		int minY = 0;
		int maxY = yMax;

		if (usableRoi != null) {
			Rectangle usableRectangle = usableRoi.getBounds();
			if (usableRectangle != null) {
				minX = (int) usableRectangle.getMinX();
				maxX = (int) usableRectangle.getMaxX();
				minY = (int) usableRectangle.getMinY();
				maxY = (int) usableRectangle.getMaxY();
			}
		}

		Properties prop = imp.getProperties();
		MaudPreferences.setPref("squareRoi.xminValue", minX);
		MaudPreferences.setPref("squareRoi.xmaxValue", maxX);
		MaudPreferences.setPref("squareRoi.yminValue", minY);
		MaudPreferences.setPref("squareRoi.ymaxValue", maxY);

		int npoints = Math.max(maxX - minX, maxY - minY) * pointsPerPixel;//  (int) getPixelCircleX(); //xMax / 2;  / coordTrasf
		double stepIntegration = imp.getCalibration().pixelWidth / pointsPerPixel;
		double coneStep = (finalAngle - startingAngle) / nprofiles;
		double[][] profile = getPixels();
		saveAsText(profile, nprofiles, npoints, coneStep, startingAngle, stepIntegration);
		return profile;
	}

	static int LAUE_CIRCLE_STEP_ROI = -997; // to be unique

  public int getType() {
    return LAUE_CIRCLE_STEP_ROI;
  }

  void saveAsText(double[][] profile, int nprofiles, int npoints, double coneStep, double startAngle,
                  double stepIntegration) {

    DataFileSet data = AreaImage.getData();
    String filename = Utility.openFileDialog(new Frame(), "Save as CIF (.esg)...",
            FileDialog.SAVE, data.getFilePar().getDirectory(), null, "put a name (with extension).esg");
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

	  save(folder, filename, profile, nprofiles, npoints, coneStep, startAngle, stepIntegration);

    IJ.wait(250);  // give system time to save the file
    if (filename != null && data != null)
      data.addDataFileforName(folder + filename, false);

  }

	public void save(String folder, String filename, double[][] profile, int nprofiles, int npoints, double coneStep, double startAngle,
	                double stepIntegration) {

		if (filename == null) return;
		if (!filename.endsWith(".esg"))
			filename = filename + ".esg";

		String title = "noTitle";
		BufferedWriter output = Misc.getWriter(folder, filename);
		try {
			for (int ij = 0; ij < nprofiles; ij++) {
				output.write("_pd_block_id " + title + "|#" + ij);
				output.newLine();
				output.newLine();
				if (ij == 0) {
					output.write("_diffrn_detector Image Plate");
					output.newLine();
					output.write("_diffrn_detector_type Image Plate");
					output.newLine();
					output.write("_pd_meas_step_count_time ?");
					output.newLine();
					output.write("_diffrn_measurement_method ?");
					output.newLine();
					output.write("_diffrn_measurement_distance_unit " + imp.getCalibration().getUnit());
					output.newLine();
					output.write("_pd_instr_dist_spec/detc " + radius);
					output.newLine();
					output.write("_diffrn_radiation_wavelength ?");
					output.newLine();
					output.write("_diffrn_source_target ?");
					output.newLine();
					output.write("_diffrn_source_power ?");
					output.newLine();
					output.write("_diffrn_source_current ?");
					output.newLine();
					output.write("_pd_meas_angle_omega " + Double.toString(omega));
					output.newLine();
					output.write("_pd_meas_angle_chi " + Double.toString(chi));
					output.newLine();
					output.write("_pd_meas_angle_phi " + Double.toString(phi));
					output.newLine();
					output.write("_riet_par_spec_displac_x 0");
					output.newLine();
					output.write("_riet_par_spec_displac_y 0");
					output.newLine();
					output.write("_riet_par_spec_displac_z 0");
					output.newLine();
					if (calibrated && radius > 0)
						output.write("_riet_meas_datafile_calibrated true");
					else
						output.write("_riet_meas_datafile_calibrated false");
					output.newLine();
				}
				double eta = ij * coneStep + startAngle;

				output.write("_pd_meas_angle_eta " + Double.toString(eta));
				double coseta = Math.cos(eta * Constants.DEGTOPI);
				double sineta = Math.sin(eta * Constants.DEGTOPI);
				output.newLine();
				output.newLine();
				output.write("loop_");
				output.newLine();
				output.write(DiffrDataFile.CIFXcoord2T);
				output.newLine();
				output.write(DiffrDataFile.intensityExpCIFstring);
				output.newLine();
//        System.out.println(imp.getCalibration().pixelWidth);
//        System.out.println(imp.getCalibration().pixelHeight);
				int start = 0;
				int end = npoints;
				int step = 1;
				if (reflectionImage) {
					start = npoints - 1;
					end = -1;
					step = -1;
				}
				for (int i = start; i != end; i += step) {
					double x = i * stepIntegration;
//          double y = i * imp.getCalibration().pixelHeight;
					if (calibrated) {
						double x1 = x * coseta;
						double y1 = x * sineta;
						double distX = x1;// - getX();
						double distY = y1;// - getY();
						double distSx = distX * Sx;
						double distSy = distY * Sy;
						distX *= distX;
						distY *= distY;
						double dist = distSx + distSy;
						double upper = distSx + distSy + radius;
						double lower = radius * radius + 2.0 * radius * dist + distX + distY;
						x = Math.acos(upper / Math.sqrt(lower)) * Constants.PITODEG;
						if (reflectionImage)
							x = 180.0 - x;
						// x = Math.atan((x - getX()) / radius) * Constants.PITODEG;
					}
					double intensity = profile[ij][i];
					if (!Double.isNaN(intensity)) {
						output.write(" " + Fmt.format(x) + " " + Fmt.format(intensity));
						output.newLine();
					}
				}
				output.newLine();
			}
		} catch (IOException io) {
			io.printStackTrace();
		}
		try {
			output.close();
		} catch (IOException io) {
			io.printStackTrace();
		}
	}

	public void setInReflection(boolean b) {
    reflectionImage = b;
  }
}
