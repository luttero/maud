package ij.plugin.filter;

import ij.*;
import ij.gui.*;
import ij.process.ImageProcessor;
import it.unitn.ing.rista.awt.Utility;
import it.unitn.ing.rista.diffr.DataFileSet;
import it.unitn.ing.rista.diffr.DiffrDataFile;
import it.unitn.ing.rista.util.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.Properties;

public class MultiSpectraFromLinearImage implements PlugInFilter {
	protected ImagePlus imp;
	Rectangle usableRectangle = null;
	//  protected double width, height;

//  protected static double defHeight = MaudPreferences.getDouble("camera.defaultHeight", 100.0);

	public int setup(String args, ImagePlus imp) {
		if (args.equals("set")) {
			doOptions();
			return DONE;
		}
		if (args.equals("about")) {
			showAbout();
			return DONE;
		}
		if (IJ.versionLessThan("1.27e"))
			return DONE;
		if (imp != null) {
			this.imp = imp;
		}
		return DOES_8G + DOES_16 + DOES_32;
	}

	public void doOptions() {
	}

	public void run(ImageProcessor ip) {
		(new Thread() {
			public void run() {
//		OvalSpectraPlot p = new OvalSpectraPlot(imp, radius);
				double[][] pixels = getPixels();
			}
		}).start();
	}

	public double[][] getPixels() {
		ImageProcessor ip = imp.getProcessor();
		if (ip == null) {
			System.out.println("No image processor!");
			return null;
		}
		int pointsPerPixel = MaudPreferences.getInteger("image2D.pointsPerPixel", 1);
		int minX = 0;
		int maxX = imp.getWidth();
		int minY = 0;
		int maxY = imp.getHeight();
		if (usableRectangle != null) {
			minX = (int) usableRectangle.getMinX();
			maxX = (int) usableRectangle.getMaxX();
			minY = (int) usableRectangle.getMinY();
			maxY = (int) usableRectangle.getMaxY();
		}
		int npoints = Math.abs(maxX - minX) * pointsPerPixel;//  (int) getPixelCircleX(); //xMax / 2;  / coordTrasf
		int nprofiles = Math.abs(maxY - minY) * pointsPerPixel;//  (int) getPixelCircleX(); //xMax / 2;  / coordTrasf
		double[][] profile = new double[nprofiles][npoints];
		for (int etaIndex = 0; etaIndex < nprofiles; etaIndex++) {
			for (int ix = 0; ix < npoints; ix++) {
//				System.out.println(ix + " " + etaIndex + " " + ip.getPixelValue(ix, etaIndex));
				profile[etaIndex][ix] = ip.getPixelValue(ix, etaIndex);
			}
		}
		saveAsText(profile);
		return profile;

	}

	void saveAsText(double[][] profile) {

		DataFileSet data = AreaImage.getData();
		String filename = Utility.openFileDialog(new Frame(), "Save as CIF (.esg)...",
				FileDialog.SAVE, data.getFilePar().getDirectory(), null, "put a name (with extension).esg");
		if (filename == null)
			return;

		String[] folderAndName = Misc.getFolderandName(filename);

		String folder = folderAndName[0];
		filename = folderAndName[1];

		if (filename == null) return;
		if (Constants.sandboxEnabled && !filename.endsWith(".esg"))
			filename = filename + ".esg";

		IJ.wait(250);  // give system time to redraw ImageJ window
		IJ.showStatus("Saving plot values...");

		save(folder, filename, profile);

		IJ.wait(250);  // give system time to save the file
		if (filename != null && data != null)
			data.addDataFileforName(folder + filename, false);

	}

	public void save(String folder, String filename, double[][] profile) {

		if (filename == null) return;
		if (Constants.sandboxEnabled && !filename.endsWith(".esg"))
			filename = filename + ".esg";

		String title = "noTitle";
		BufferedWriter output = Misc.getWriter(folder, filename);
		int nprofiles = profile.length;
		try {
			for (int ij = 0; ij < nprofiles; ij++) {
				output.write("_pd_block_id " + title + "|#" + ij);
				output.newLine();
				output.newLine();
				if (ij == 0) {
					output.write("_diffrn_detector ?");
					output.newLine();
					output.write("_diffrn_detector_type ?");
					output.newLine();
					output.write("_pd_meas_step_count_time ?");
					output.newLine();
					output.write("_diffrn_measurement_method ?");
					output.newLine();
					output.write("_diffrn_measurement_distance_unit 'channel number'");
					output.newLine();
					output.write("_diffrn_radiation_wavelength ?");
					output.newLine();
					output.write("_diffrn_source_target ?");
					output.newLine();
					output.write("_diffrn_source_power ?");
					output.newLine();
					output.write("_diffrn_source_current ?");
					output.newLine();
					output.write("_riet_meas_datafile_calibrated false");
					output.newLine();
				}

				output.newLine();
				output.write("loop_");
				output.newLine();
				output.write(DiffrDataFile.CIFXcoordEnergy);
				output.newLine();
				output.write(DiffrDataFile.intensityExpCIFstring);
				output.newLine();
//        System.out.println(imp.getCalibration().pixelWidth);
//        System.out.println(imp.getCalibration().pixelHeight);
				int start = 0;
				int end = profile[ij].length;
				int step = 1;
				for (int i = start; i != end; i += step) {
					double x = i;
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

	void showAbout() {
		IJ.showMessage("About LinearSpectraIntegration", " Transform an image in spectra.");
	}

}

