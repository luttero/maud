package ij.gui;

import ij.*;
import ij.process.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.DataFileSet;
import it.unitn.ing.rista.diffr.DiffrDataFile;
import it.unitn.ing.rista.awt.Utility;

import java.awt.*;
import java.io.BufferedWriter;
import java.io.IOException;

// Oval region of interest

public class LaueOvalRoi extends LaueOvalStepRoi {

  public LaueOvalRoi(ImagePlus imp, double radius) {
    super(imp, radius);
  }

  public double[][] getPixels() {
    ImageProcessor ip = imp.getProcessor();
    if (ip == null) {
      System.out.println("No image processor!");
      return null;
    }

//	  Roi selroi = imp.getRoi();
//    int maxGray = 256;

    int numbAvg = 0;

    int counter = 0;

    int minX = MaudPreferences.getInteger("ovalROI.minX", 0);
    int maxX = MaudPreferences.getInteger("ovalROI.maxX", xMax);
    int minY = MaudPreferences.getInteger("ovalROI.minY", 0);
    int maxY = MaudPreferences.getInteger("ovalROI.maxY", yMax);

    MaudPreferences.setPref("ovalROI.minX", minX);
    MaudPreferences.setPref("ovalROI.maxX", maxX);
    MaudPreferences.setPref("ovalROI.minY", minY);
    MaudPreferences.setPref("ovalROI.maxY", maxY);

//	  System.out.println(minX + " " + maxX + " " + minY + " " + maxY);

	  if (usableRoi != null) {
		  Rectangle usableRectangle = usableRoi.getBounds();
		  if (usableRectangle != null) {
			  minX = (int) usableRectangle.getMinX();
			  maxX = (int) usableRectangle.getMaxX();
			  minY = (int) usableRectangle.getMinY();
			  maxY = (int) usableRectangle.getMaxY();
		  }
	  }

//	  System.out.println("After usable Rectangle: " + minX + " " + maxX + " " + minY + " " + maxY);

    MaudPreferences.setPref("ovalROI.minX", minX);
    MaudPreferences.setPref("ovalROI.maxX", maxX);
    MaudPreferences.setPref("ovalROI.minY", minY);
    MaudPreferences.setPref("ovalROI.maxY", maxY);

	  double correctionExponent = MaudPreferences.getDouble("image2D.exponentCorrectionValue", 0.0);

	  int npoints = (int) ((maxX - minX) * getPointsPerPixels());//  (int) getPixelCircleX(); //xMax / 2;  / coordTrasf
//	  double stepIntegration = imp.getCalibration().pixelWidth / getPointsPerPixels();

//	  int npoints = maxX - minX;
    double[][] profile = new double[1][npoints];
    for (int ix = 0; ix < npoints; ix++) {
      numbAvg = 0;
	    double xreal = (1.0 / getPointsPerPixels() * ix + minX) * coordTrasfX - getX();
	    double twoTheta = xreal / radius * Constants.PITODEG;
//	    System.out.println("Point, 2theta: " + ix + " " + (ix / getPointsPerPixels() + minX) + " " + twoTheta + " " + getXCoord(twoTheta, getY()/coordTrasfY));
      for (int iy = minY; iy < maxY; iy++) {
	      double x1 = getXCoord(twoTheta, iy);
	      double y1 = getYCoord(iy);
          if ((usableRoi != null && usableRoi.contains((int) x1, iy)) ||
		          (usableRoi == null && (x1 >= minX && x1 < maxX))) {
	          double coreta = Math.pow(1.0 + (y1 * y1 / radius / radius), correctionExponent); // 1.0 / MoreMath.cosd(eta);
            profile[0][ix] += ip.getInterpolatedPixel(x1, (double) iy) * coreta;
//      System.out.println("x:  " + iy + " " + x1 + " " + y1 + " " + coreta + " " + profile[0][ix]);
            numbAvg++;
          } //else
// System.out.println("Not x:  " + x1 + " " + y1);
      }
        if (numbAvg > 3) {
          profile[0][ix] /= numbAvg;
//        if (profile[ix] > maxGray) maxGray *= 256;
        } else
          profile[0][ix] = Double.NaN;

    }

    return profile;

  }

  public double[][] getIntervalPixels() {
    double[][] profile = getPixels();
    int minX = MaudPreferences.getInteger("ovalROI.minX", 0);
    int maxX = MaudPreferences.getInteger("ovalROI.maxX", xMax);
    saveAsText(profile, minX, maxX);
//      System.out.println(profile[ix] + " " + maxGray);
    return profile;

  }

  public int getType() {
    return Roi.OVAL;
  }

  void saveAsText(double[][] profile, int startX, int endX) {

    DataFileSet data = AreaImage.getData();
    String filename = Utility.openFileDialog(new Frame(), "Save as CIF (.esg)...",
            FileDialog.SAVE, data.getFilePar().getDirectory(), null, "put a name (with extension).esg");
    if (filename == null)
      return;

    String[] folderAndName = Misc.getFolderandName(filename);

    String folder = folderAndName[0];
    filename = folderAndName[1];

    IJ.wait(250);  // give system time to redraw ImageJ window
    IJ.showStatus("Saving plot values...");
    if (filename == null) return;
    if (Constants.sandboxEnabled && !filename.endsWith(".esg"))
      filename = filename + ".esg";
    save(folder, filename, profile, startX, endX);

    IJ.wait(250);  // give system time to save the file
    if (filename != null && data != null)
      data.addDataFileforName(folder + filename, false);

  }

  public void save(String folder, String filename, double[][] profile, int startX, int endX) {

    String title = "noTitle";
    BufferedWriter output = Misc.getWriter(folder, filename);
    try {
        output.write("_pd_block_id " + title);
        output.newLine();
        output.newLine();
          output.write("_diffrn_detector 2D");
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
        output.write("_pd_meas_angle_eta 0.0");
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
        for (int i = startX; i < endX; i++) {
          double x = 1.0 / getPointsPerPixels() * i * coordTrasfX;
          if (calibrated)
            x = (x - getX()) / radius * Constants.PITODEG;
          double intensity = profile[0][i - startX];
          if (Double.isNaN(intensity))
            intensity = -1;
          if (intensity > 0) {
            output.write(" " + Fmt.format(x) + " " + Fmt.format(intensity));
            output.newLine();
          }
        }
        output.newLine();
    } catch (IOException io) {
    }

    try {
      output.close();
    } catch (IOException io) {
    }
  }

}
