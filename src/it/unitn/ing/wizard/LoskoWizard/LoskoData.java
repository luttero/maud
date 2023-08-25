package it.unitn.ing.wizard.LoskoWizard;

import ij.*;
import ij.io.*;
import ij.process.ImageProcessor;
import it.unitn.ing.rista.util.MaudPreferences;
import it.unitn.ing.rista.util.Misc;

import java.io.*;
import java.util.StringTokenizer;
import java.util.Vector;

import static it.unitn.ing.rista.util.Constants.ENERGY_LAMBDA;

public class LoskoData {

	public String title = "New LumaCam TOF image analysis";
	public String author = "Adrian";
	public String sampleName = "Sample_x";
	public int panelsNumber = LoskoConfigData.getPropertyValue("numberOfPanels", 5);

	public double omega;
	public double chi;
	public double phi;
	public double step2theta = LoskoConfigData.getPropertyValue("generated2thetaStep", 0.015);
	public double coneInterval = LoskoConfigData.getPropertyValue("etaStepForIntegration", 5.0);
	public double wavelength;

	public boolean useTemplateFile = true;
//	public String sampleDesc = "Sample description";

//	public double something = MaudPreferences.getDouble("LCLS2.something", 0.0);;

//	public Vector<String> correctedImageFiles = new Vector(panelsNumber,1);
//	public Vector<String> uncorrectedImageFiles = new Vector(panelsNumber,1);

	public Vector<LoskoCspad> panels = new Vector(0,1);

	public LoskoData(String title) {
		this.title = title;
	}

	public void addImagePanel(int panelNumber, String corrImageFile, String uncorrImageFile) {
//		correctedImageFiles.add(corrImageFile);
//		uncorrectedImageFiles.add(uncorrImageFile);

		LoskoDetectorPanel panel = LoskoConfigData.detectorPanels.elementAt(panelNumber);
		LoskoDetectorType panelType = LoskoConfigData.getDetectorType(panel.panelType);

		String[] folderAndName = Misc.getFolderandName(uncorrImageFile);
		System.out.println("Opening image: " + folderAndName[1]);

		ImagePlus imp = (new Opener()).openImage(folderAndName[0], folderAndName[1]);
		ImageProcessor ip = imp.getChannelProcessor();
		int width = ip.getWidth();
		int height = ip.getHeight();
		float[][] buffer = new float[height][width];
		for (int ix = 0; ix < width; ix++)
			for (int iy = 0; iy < height; iy++)
				buffer[iy][ix] = ip.getPixelValue(ix, iy);

		LoskoCspad cspad = new LoskoCspad();

		double[][][] mydata = null;

		if (panel.panelType.equalsIgnoreCase("4x4")) {
			if (corrImageFile.length() > 0 && corrImageFile.endsWith(".data"))
				if (LoskoConfigData.version > 0)
					mydata = readData4x2(corrImageFile);
				else
					mydata = readData4x4(corrImageFile);
		} else if (panel.panelType.equalsIgnoreCase("2x2")) {
			if (corrImageFile.length() > 0 && corrImageFile.endsWith(".data")) {
				if (LoskoConfigData.version > 0)
					mydata = readData2x1(corrImageFile);
				else
					mydata = readData2x2(corrImageFile);
			}
		}
		if (mydata != null) {
			for (int i = 0; i < panel.detectorsNumber; i++) {
				LoskoDetectorSensor sensor = panelType.sensors.elementAt(i);
				System.out.println("Panel: " + panel.panelName + " " + panel.panelType + ", detector: " + i);

				double[][] pixelData = mydata[sensor.pedestal_number];
				LoskoSensorImage dark_image = new LoskoSensorImage(pixelData);
				switch (sensor.pedestal_rotation) {
					case LoskoDetectorPanel.ROT_CW:
						dark_image.rotateCW(panelType);
						break;
					case LoskoDetectorPanel.ROT_CCW:
						dark_image.rotateCCW(panelType);
						break;
					case LoskoDetectorPanel.ROT_180:
						dark_image.rotate180(panelType);
						break;
					default: {
					}
				}

				double[][] photon = new double[sensor.detector_height][sensor.detector_width];
				for (int ix = 0; ix < sensor.detector_height; ix++) {
					for (int iy = 0; iy < sensor.detector_width; iy++) {
						photon[ix][iy] = buffer[ix + sensor.origin_y][iy + sensor.origin_x];
					}
				}
				LoskoSensorImage image = new LoskoSensorImage(photon);
				image.name = panel.panelName + "_" + sensor.detector_number;
				image.pixelHeight = panel.pixel_size_mm_y;
				image.pixelWidth = panel.pixel_size_mm_x;
				image.originX = sensor.origin_x;
				image.originY = sensor.origin_y;

				int dotLocation = LoskoConfigData.filenameToSave.lastIndexOf(".");
				String filename = LoskoConfigData.filenameToSave.substring(0, dotLocation) + image.name;
				if (MaudPreferences.getBoolean("LumaCamWizard.saveIntermediateImages", false)) {
					ImagePlus imp1 = image.getImagePlus();
					IJ.saveAsTiff(imp1, filename + "_pre.tiff");

					imp1 = dark_image.getImagePlus();
					IJ.saveAsTiff(imp1, filename + "_dark.tiff");
				}

				if (LoskoConfigData.version > 0)
					image.removeDarkCurrentWithGap(dark_image, sensor.pedestal_shift_x, sensor.pedestal_shift_y);
				else
					image.removeDarkCurrent(dark_image, sensor.pedestal_shift_x, sensor.pedestal_shift_y);

				switch (panel.panelRotation) {
					case LoskoDetectorPanel.ROT_CW:
						image.rotateCW(panelType);
						break;
					case LoskoDetectorPanel.ROT_CCW:
						image.rotateCCW(panelType);
						break;
					case LoskoDetectorPanel.ROT_180:
						image.rotate180(panelType);
						break;
					default: {
						image.originX = -image.originX;
						image.originY = -image.originY;
					}
				}

				image.centerX = panel.center_x + image.pixelWidth * image.originX;
				image.centerY = panel.center_y + image.pixelHeight * image.originY;
				image.distance = panel.panel_distance;
				image.theta2 = panel.panel_2theta;
				image.omegaDN = panel.panel_omegaDN;
				image.phiDA = panel.panel_tilting;

				if (MaudPreferences.getBoolean("LumaCamWizard.saveFinalImages", false)) {
					ImagePlus imp1 = image.getImagePlus();
					IJ.saveAsTiff(imp1, filename + "_final.tiff");
				}

				cspad.detectors.add(image);
			}
			panels.add(cspad);
		} else {
			System.out.println("Not using panel " + panel.panelName);
		}

	}

	public double[][][] readData2x2(String filename) {
		BufferedReader reader = Misc.getReader(filename);

		double[][][] numberData = new double[4][194][185];

		if (reader != null) {
			try {

				String token1 = new String("");
				String token2 = new String("");

				int datanumber_row = 0;
				int datanumber_column = 0;
				int imageNumber = 0;

				String linedata = reader.readLine();

				while (linedata != null) {

					StringTokenizer st = new StringTokenizer(linedata, "' ,\t\r\n");

					try {
						if (st.hasMoreTokens()) {
							token1 = st.nextToken();
							if (st.hasMoreTokens()) {
								token2 = st.nextToken();

//              System.out.println(token1 + " " + token2);
								Double n1 = Double.valueOf(token1);
								Double n2 = Double.valueOf(token2);

								numberData[imageNumber][193 - datanumber_column][datanumber_row] = n1;
								numberData[imageNumber+1][193 - datanumber_column][datanumber_row] = n2;
								datanumber_column++;
								if (datanumber_column == 194) {
									if (imageNumber > 0) {
										datanumber_row++;
									}
									datanumber_column = 0;
									imageNumber += 2;
									if (imageNumber > 2) {
//										datanumber_row = 0;
										imageNumber = 0;
									}
								}
							}
						}
					} catch (Exception ge) {
// not numbers, we don't store them
//              System.out.println("not a number: " + linedata);
					}
					if (datanumber_row == 185)
						linedata = null;
					else
						linedata = reader.readLine();
				}

				// save here

			} catch (Exception e) {
				e.printStackTrace();
				System.out.println("Error in loading the data file! Try to remove this data file");
			}
			try {
				reader.close();
			} catch (IOException e) {
			}
		}
		return numberData;

	}

	public double[][][] readData4x4(String filename) {

		BufferedReader reader = Misc.getReader(filename);

		double[][][] numberData = new double[16][185][194];

		if (reader != null) {
			try {

				String token1 = new String("");
				String token2 = new String("");

				int datanumber_row = 0;
				int datanumber_column = 0;
				int imageNumber = 0;

				String linedata = reader.readLine();

				while (linedata != null) {

					StringTokenizer st = new StringTokenizer(linedata, "' ,\t\r\n");

					try {
						int useImage = imageNumber;
						while (st.hasMoreTokens()) {
							numberData[useImage][datanumber_row][datanumber_column] = Double.valueOf(st.nextToken());
							datanumber_column++;
							if (datanumber_column == 194) {
								datanumber_column = 0;
								useImage = imageNumber + 1;
							}
						}
						datanumber_row++;
						if (datanumber_row == 185) {
							datanumber_row = 0;
							imageNumber += 2;
						}

					} catch (Exception ge) {
// not numbers, we don't store them
//              System.out.println("not a number: " + linedata);
					}
					if (imageNumber == 16 && datanumber_row == 185)
						linedata = null;
					else
						linedata = reader.readLine();
				}

				// save here

			} catch (Exception e) {
				e.printStackTrace();
				System.out.println("Error in loading the data file! Try to remove this data file");
			}
			try {
				reader.close();
			} catch (IOException e) {
			}
		}
		return numberData;

	}

	public double[][][] readData2x1(String filename) {
		double[][][] data2x2 = readData2x2(filename);

		double[][][] numberData = new double[2][388][185];
		for (int i = 0; i < numberData.length; i++) {
			int i1 = i + 2;
			int i2 = i;
			for (int ix = 0; ix < data2x2[0][0].length; ix++) {
				for (int iy = 0; iy < data2x2[0].length; iy++) {
					numberData[i][iy][ix] = data2x2[i1][iy][ix];
					numberData[i][iy + 194][ix] = data2x2[i2][iy][ix];
				}
			}
		}


		return numberData;

	}

	public double[][][] readData4x2(String filename) {

		double[][][] data4x4 = readData4x4(filename);

		double[][][] numberData = new double[8][185][388];

		// 0,1 -> 0
		// 2,3 -> 1
		// 4,5 -> 2

		for (int i = 0; i < numberData.length; i++) {
			int i1 = i * 2;
			int i2 = i1 + 1;
			for (int ix = 0; ix < data4x4[0].length; ix++) {
				for (int iy = 0; iy < data4x4[0][0].length; iy++) {
					numberData[i][ix][iy] = data4x4[i1][ix][iy];
					numberData[i][ix][iy + 194] = data4x4[i2][ix][iy];
				}
			}
		}

		return numberData;

	}

	public void setupDataFrom(String detectorConfigFile, String original_image, String dark_image) {

		double energyInKeV = 10.217;
		omega = 0;
		String property = "detector_config_file";
		String calibrationDirectory = "";
		if (detectorConfigFile == null || detectorConfigFile.isEmpty() || !Misc.checkForFile(detectorConfigFile)) {
			System.out.println("Detector config file not found: " + detectorConfigFile + ", use the one in the preferences!");
			detectorConfigFile = LoskoConfigData.getPropertyValue(property, detectorConfigFile);
		} else
			LoskoConfigData.setPropertyValue(property, detectorConfigFile);
		if (Misc.checkForFile(detectorConfigFile)) {
			LoskoConfigData.readLoskoConfigDataFromFile(detectorConfigFile);
			omega = LoskoConfigData.omega;
			energyInKeV = LoskoConfigData.radiationKeV;
			calibrationDirectory = LoskoConfigData.calibrationDirectory;
		} else {
			System.out.println("No detector config file!");
		}
		int position = original_image.lastIndexOf(".");
		LoskoConfigData.filenameToSave = original_image.substring(0, position) + ".esg";
		LoskoConfigData.setPropertyValue("UnrolledImagesDatafile", LoskoConfigData.filenameToSave);
		System.out.println("Unrolled Images Datafile: " + LoskoConfigData.filenameToSave);
//		LCLS2ConfigData.filenameTemplate = filenameTemplateTF.getText();
//		LCLS2ConfigData.setPropertyValue("LCLSdefaultTemplate", LCLS2ConfigData.filenameTemplate);
		useTemplateFile = true;
		sampleName = "";
		wavelength = ENERGY_LAMBDA / (energyInKeV * 1000);

		String file1 = "", file2 = "";
		for (int i = 0; i < panelsNumber; i++) {
			String prefix = "Cspad";
			if (i == 0) {
				file1 = new String(original_image);
				String property1 = prefix + "." + i + "_uncorrected_image";
				LoskoConfigData.setPropertyValue(property1, file1);
				file2 = new String(dark_image);
				String property2 = prefix + "." + i + "_dark_current_image";
				LoskoConfigData.setPropertyValue(property2, file2);
			} else {
				prefix = prefix + "2x2";
				String property1 = prefix + "." + i + "_uncorrected_image";
				file1 = new String(original_image);
//				System.out.println("file1 before: " + file1);
				file1 = file1.replaceFirst("LumaCam", "Cspad2x2-" + i);
				LoskoConfigData.setPropertyValue(property1, file1);
//				System.out.println("file1 after: " + file1);

				String property2 = prefix + "." + i + "_dark_current_image";
				file2 = new String(dark_image);
//				System.out.println("file2 before: " + file2);
				file2 = file2.replaceFirst("LumaCam_", "LumaCam2x2_");
				file2 = file2.replaceFirst("LumaCam", "LumaCam2x2." + i);
				LoskoConfigData.setPropertyValue(property2, file2);
//				System.out.println("file2 after: " + file2);
			}
			addImagePanel(i, file2, file1);
		}
	}
}
