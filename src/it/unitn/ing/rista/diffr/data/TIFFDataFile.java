/*
 * @(#)TIFFDataFile.java created 9/05/2002 Riva Del Garda
 *
 * Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.cal.*;

import java.lang.*;

import it.unitn.ing.rista.io.StringNumber;
import it.unitn.ing.rista.util.*;
import ij.*;
import ij.io.*;
import ij.gui.*;
import ij.process.*;


/**
 *  The TIFFDataFile is a class
 *
 *
 * @version $Revision: 1.10 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class TIFFDataFile extends it.unitn.ing.rista.diffr.MultDiffrDataFile {

  public TIFFDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".tif";
  }

  public TIFFDataFile() {
    identifier = ".tif";
  }


  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    DataFileSet data = getDataFileSet();
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
//    new Opener().open();
//		OpenDialog od = new OpenDialog("Open...", "");
    String directory = getFolder(); //od.getDirectory();
    String name = getLabel(); //od.getFileName();
//    System.out.println("Opening file: "+ directory + " - " + name);
    if (name != null) {
      ImagePlus imp = (new Opener()).openImage(directory, name);
      if (imp != null) {
	      double[] gonioAngles = StringNumber.checkAngles(name);
	      AngularCalibration angcal = getDataFileSet().getInstrument().getAngularCalibration();

	      if (angcal != null) {
		      if (angcal instanceof AngularInclinedFlatImageCalibration) {
			      angcal.loadAndUnrollImage(imp, this, gonioAngles);
			      loadSuccessfull = true;
		      } else if (angcal instanceof AngularFlatImageTransmissionCalibration) {
			      loadSuccessfull = readTransmissionImage(imp, true, gonioAngles);
		      } else if (angcal instanceof AngularFlatImageReflectionCalibration) {
			      loadSuccessfull = readTransmissionImage(imp, false, gonioAngles);
		      } else if (angcal instanceof Angular2DCurvedDetectorCalibration) {
			      loadSuccessfull = readCurvedReflectionImage(imp, gonioAngles);
		      } else if (angcal instanceof AngularCameraCalibration) {
			      loadSuccessfull = readCameraImage(imp, gonioAngles);
		      }
	      }
      }
    }
    isAbilitatetoRefresh = tmpB;
//    data.refreshAll(false);

    return loadSuccessfull;
  }

	public boolean readReflectionImage(ImagePlus imp, double[] gonioAngles) {
		boolean loadSuccessfull = false;
		AngularCalibration angcal = getDataFileSet().getInstrument().getAngularCalibration();
		String directory = Constants.cachesDirectory; // getFolder(); //od.getDirectory();
		String name = getLabel(); //od.getFileName();

		ij.measure.Calibration cal = imp.getCalibration();
		if (cal.getUnit().compareToIgnoreCase("cm") == 0) {
			cal.setUnit("mm");
			cal.pixelWidth *= 10;
			cal.pixelHeight *= 10;
		} else if (cal.getUnit().compareToIgnoreCase("inches") == 0) {
			cal.setUnit("mm");
			cal.pixelWidth *= 25.4;
			cal.pixelHeight *= 25.4;
		}
		if (cal.pixelWidth == 0.0 || cal.pixelWidth == 1 || cal.getUnit().compareToIgnoreCase("mm") != 0) {
			cal.setUnit("mm");
			cal.pixelWidth = MaudPreferences.getDouble("pixelDetector.pixelWidth", 0.02);
			cal.pixelHeight = MaudPreferences.getDouble("pixelDetector.pixelHeight", 0.02);
		}
		ImageProcessor ip = imp.getChannelProcessor();
		int width = ip.getWidth();
		int height = ip.getHeight();
		int[] buffer = new int[width * height];
		for (int ix = 0; ix < width; ix++)
			for (int iy = 0; iy < height; iy++)
				buffer[ix + iy * width] = ip.getPixel(ix, iy);
//        imp.show();

//	      KCDReader fileReader = new KCDReader();
//	      short[] buffer = fileReader.loadImage(file, pixelDimension, dimension, properties);
//	      if (buffer != null) {

		/*

		double centerX = MaudPreferences.getDouble("pixelDetector.centerX", 0.5 * width);
		double centerY = MaudPreferences.getDouble("pixelDetector.centerY", 0.5 * height);
		double azimuthal = MaudPreferences.getDouble("pixelDetector.defaultPhiDAangle", 0.0);
		double phiDetector = MaudPreferences.getDouble("pixelDetector.defaultOmegaDNangle", 0.0);
		double coneAngle = MaudPreferences.getDouble("pixelDetector.defaultEtaDAangle", 0.0);
		double detector2Theta = MaudPreferences.getDouble("pixelDetector.default2ThetaAngle", 40.0);
		double detectorDistance = MaudPreferences.getDouble("pixelDetector.defaultDetectorDistance", 85);

		 */
		double azimuthal = ((AngularInclinedFlatImageCalibration) angcal).getOriginalPhiDA();
		double phiDetector = ((AngularInclinedFlatImageCalibration) angcal).getOriginalOmegaDN();
		double coneAngle = ((AngularInclinedFlatImageCalibration) angcal).getOriginalEtaDA();
		double detector2Theta = ((AngularInclinedFlatImageCalibration) angcal).getOriginal2Theta();
		double detectorDistance = ((AngularInclinedFlatImageCalibration) angcal).getOriginalDistance();
		double centerX = angcal.getOriginalCenterX();
		double centerY = angcal.getOriginalCenterY();
		double coneInterval = MaudPreferences.getDouble("pixelDetector.defaultDiffractionConeInterval", 5.0);
		double theta2Step = MaudPreferences.getDouble("pixelDetector.defaultDiffractionStepAngle", 0.02);
		double omega = MaudPreferences.getDouble("sample.defaultOmegaAngle", 55.0);
		double chi = MaudPreferences.getDouble("sample.defaultChiAngle", 17.0);
		double phi = MaudPreferences.getDouble("sample.defaultPhiAngle", 0.0);

		if (gonioAngles[0] != StringNumber.dummyAngle)
			detector2Theta = gonioAngles[0];
		if (gonioAngles[1] != StringNumber.dummyAngle)
			omega = gonioAngles[1];
		if (gonioAngles[2] != StringNumber.dummyAngle)
			chi = gonioAngles[2];
		if (gonioAngles[3] != StringNumber.dummyAngle)
			phi = gonioAngles[3];

		int minX = MaudPreferences.getInteger("squareRoi.xminValue", 0);
		int maxX = MaudPreferences.getInteger("squareRoi.xmaxValue", width);
		int minY = MaudPreferences.getInteger("squareRoi.yminValue", 0);
		int maxY = MaudPreferences.getInteger("squareRoi.ymaxValue", height);
//    System.out.println("Using min max " + minX + " " + maxX + " " + minY + " " + maxY);
		int npointsX = maxX - minX;
		int npointsY = maxY - minY;
		double[] intensity = new double[npointsX * npointsY];
		double[] x = new double[npointsX * npointsY];
		double[] y = new double[npointsX * npointsY];
		int index = 0;
		for (int ix = minX; ix < maxX; ix++) {
			for (int iy = minY; iy < maxY; iy++) {
				intensity[index++] = buffer[ix + iy * width];
			}
		}
		FlatCCDReflectionSquareRoi.getXYFromPixelIndex(minX, maxX, minY, maxY, cal.pixelWidth, cal.pixelHeight,
				x, y, centerX, centerY);
/*    System.out.println("Conversion to xy coordinates done: " + minX + " " + maxX + " " + minY + " " + maxY + " " +
		    dimension[0] + " " + dimension[1] + " " + x[3] + " " + y[3] + " " + centerX + " " + centerY);*/

		double[] theta2 = new double[npointsX * npointsY];
		double[] eta = new double[npointsX * npointsY];
//	        Angles.getTheta2EtaFromXYPixelDetector(x, y, theta2, eta, omega, det2Theta, phiDA,
//			        omegaDN, etaDA, detectorDistance, 0.0);
/*	        System.out.println("Converting to 2theta, eta: " + dimension[3] + " " +
			        dimension[5] + " " + azimuthal + " " +
			        phiDetector + " 0 " + dimension[7]);*/
		Angles.getTheta2EtaFromXYPixelDetector(x, y, theta2, eta, omega,
				detector2Theta, azimuthal, phiDetector, coneAngle, detectorDistance, 0);
/*	        System.out.println("Conversion to theta, eta angles done! " + theta2[0] + " " + theta2[theta2.length - 1] + " " +
			        eta[0] + " " + eta[eta.length - 1]);*/
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

//	        System.out.println(theta2.length + " " + eta.length + " " + intensity.length + " " + x.length + " " + y.length + " " + detectorDistance +
//			        " " + min2theta + " " + max2theta + " " + theta2Step * Constants.DEGTOPI + " " + mineta + " " + maxeta + " " +
//			        coneInterval * Constants.DEGTOPI);
		double[][][] profile = Angles.spectraFromPixelsByEtaTheta2(theta2, eta, intensity, x, y, detectorDistance,
				min2theta, max2theta, theta2Step * Constants.DEGTOPI,
				mineta, maxeta, coneInterval * Constants.DEGTOPI);

//    System.out.println("Conversion to spectra done!");
		double xmin = min2theta * Constants.PITODEG;
		double etaStart = mineta * Constants.PITODEG;
		int dotLocation = name.lastIndexOf(".");
		String filename = name.substring(0, dotLocation) + ".esg";
		FlatCCDReflectionSquareRoi.saveAsText(profile, profile[0].length, 0, profile[0][0].length, xmin, theta2Step,
				etaStart, coneInterval, directory, filename, "mm", detectorDistance, omega, chi, phi,
				detector2Theta,true);
		setLabel(filename);
		for (int spectrumIndex = 0; spectrumIndex < profile[0].length; spectrumIndex++) {
			String numberString = Integer.toString(spectrumIndex);
			DiffrDataFile datafile = addDiffrDatafile(numberString);
			boolean atmpB = datafile.isAbilitatetoRefresh;
			datafile.isAbilitatetoRefresh = false;

			datafile.setDataType(DIFFRACTION_IMAGE);
			datafile.setAngleValue(0, omega);
			datafile.setAngleValue(1, chi);
			datafile.setAngleValue(2, phi);
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
//            System.out.println("Check this: " + i + " =? " + datafile.datanumber);
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
					datafile.setXData(indexPoint, indexPoint);
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
		return loadSuccessfull;
	}

	public boolean readCurvedReflectionImage(ImagePlus imp, double[] gonioAngles) { // todo
		boolean loadSuccessfull = false;
		AngularCalibration angcal = getDataFileSet().getInstrument().getAngularCalibration();
		String directory = Constants.cachesDirectory; // getFolder(); //od.getDirectory();
		String name = getLabel(); //od.getFileName();

		ij.measure.Calibration cal = imp.getCalibration();
		if (cal.getUnit().compareToIgnoreCase("cm") == 0) {
			cal.setUnit("mm");
			cal.pixelWidth *= 10;
			cal.pixelHeight *= 10;
		} else if (cal.getUnit().compareToIgnoreCase("inches") == 0) {
			cal.setUnit("mm");
			cal.pixelWidth *= 25.4;
			cal.pixelHeight *= 25.4;
		}
		if (cal.pixelWidth == 0.0 || cal.pixelWidth == 1 || cal.getUnit().compareToIgnoreCase("mm") != 0) {
			cal.setUnit("mm");
			cal.pixelWidth = MaudPreferences.getDouble("pixelDetector.pixelWidth", 0.02);
			cal.pixelHeight = MaudPreferences.getDouble("pixelDetector.pixelHeight", 0.02);
		}
		ImageProcessor ip = imp.getChannelProcessor();
		int width = ip.getWidth();
		int height = ip.getHeight();
		int[] buffer = new int[width * height];
		for (int ix = 0; ix < width; ix++)
			for (int iy = 0; iy < height; iy++)
				buffer[ix + iy * width] = ip.getPixel(ix, iy);
		return loadSuccessfull;
	}

	public boolean readCameraImage(ImagePlus imp, double[] gonioAngles) {
		boolean loadSuccessfull = false;
		AngularCameraCalibration angcal = (AngularCameraCalibration) getDataFileSet().getInstrument().getAngularCalibration();

		ij.measure.Calibration cal = imp.getCalibration();
		if (cal.getUnit().compareToIgnoreCase("cm") == 0) {
			cal.setUnit("mm");
			cal.pixelWidth *= 10;
			cal.pixelHeight *= 10;
		} else if (cal.getUnit().compareToIgnoreCase("inches") == 0) {
			cal.setUnit("mm");
			cal.pixelWidth *= 25.4;
			cal.pixelHeight *= 25.4;
		}

		if (cal.pixelWidth == 0.0 || cal.pixelWidth == 1 || cal.getUnit().compareToIgnoreCase("mm") != 0) {
			cal.setUnit("mm");
			cal.pixelWidth = MaudPreferences.getDouble("pixelDetector.pixelWidth", 0.02);
			cal.pixelHeight = MaudPreferences.getDouble("pixelDetector.pixelHeight", 0.02);
		}

		double xCenter = angcal.getOriginalCenterX();
		double yCenter = angcal.getOriginalCenterY();
		double radius = angcal.getRadius();
		double omega = MaudPreferences.getDouble("camera.defaultOmegaAngle", 15.0);
		double chi = MaudPreferences.getDouble("camera.defaultChiAngle", 0.0);
		double phi = MaudPreferences.getDouble("camera.defaultPhiAngle", 0.0);
		boolean calibrated = MaudPreferences.getBoolean("anglesCalibration.imageToSpectra", false);
//		if (gonioAngles[0] != StringNumber.dummyAngle)
//			detector2Theta = gonioAngles[0];
		if (gonioAngles[1] != StringNumber.dummyAngle)
			omega = gonioAngles[1];
		if (gonioAngles[2] != StringNumber.dummyAngle)
			chi = gonioAngles[2];
		if (gonioAngles[3] != StringNumber.dummyAngle)
			phi = gonioAngles[3];

		LaueOvalRoi roi = new LaueOvalRoi(imp, radius);
		imp.setRoi(roi);
		roi = (LaueOvalRoi) imp.getRoi();
		ImageProcessor ip = imp.getProcessor();
		ip.setCalibrationTable(cal.getCTable());
		roi.setRadius(radius);
		roi.setStartingPoint(xCenter, yCenter);
//        roi.setSelHeight(cameraHeight);
		roi.setOmega(omega);
		roi.setChi(chi);
		roi.setPhi(phi);
		if (calibrated)
			roi.setCalibrated();
		else
			roi.setUncalibrated();
//        roi.updateSelection();

		double[][] profile = roi.getPixels();
		int startX = MaudPreferences.getInteger("ovalROI.minX", 0);
		int endX = MaudPreferences.getInteger("ovalROI.maxX", 10000);
		String directory = Constants.cachesDirectory; // getFolder(); //od.getDirectory();
		String name = getLabel(); //od.getFileName();
		int dotLocation = name.lastIndexOf(".");
		String filename = name.substring(0, dotLocation) + ".esg";
		roi.save(directory, filename, profile, startX, endX);
		setLabel(filename);
		DiffrDataFile datafile = addDiffrDatafile();
		boolean atmpB = datafile.isAbilitatetoRefresh;
		datafile.isAbilitatetoRefresh = false;
		datafile.setString(datafile.saveCustomID, "true");

		datafile.setAngleValue(0, omega);
		datafile.setAngleValue(1, chi);
		datafile.setAngleValue(2, phi);

		datafile.initData(endX - startX);
		datafile.constantstep = false;
		datafile.datanumber = endX - startX;
		datafile.dspacingbase = false;
		datafile.setCalibrated(calibrated);

		for (int i = startX; i < endX; i++) {
			double x = i * cal.pixelWidth;
			if (calibrated) {
				x = (x - xCenter) / radius * Constants.PITODEG;
				datafile.setCalibratedXData(i - startX, x);
			} else
				datafile.setXData(i - startX, x);
			double intensity = profile[0][i - startX];
			datafile.setYData(i - startX, intensity);
			double tmpweight = Math.sqrt(intensity);
			if (tmpweight != 0.0)
				datafile.setWeight(i - startX, 1.0 / tmpweight);
			else
				datafile.setWeight(i - startX, 1.0);
		}

		datafile.isAbilitatetoRefresh = atmpB;
		loadSuccessfull = true;
		datafile.dataLoaded = true;

		return loadSuccessfull;
	}

	public boolean readTransmissionImage(ImagePlus imp, boolean transmission, double[] gonioAngles) {
		AngularFlatImageTransmissionCalibration angcal;
		if (transmission)
			angcal = (AngularFlatImageTransmissionCalibration)
				getDataFileSet().getInstrument().getAngularCalibration();
		else
			angcal = (AngularFlatImageReflectionCalibration)
					getDataFileSet().getInstrument().getAngularCalibration();

		ij.measure.Calibration cal = imp.getCalibration();
		if (cal.pixelWidth == 0.0 || cal.pixelWidth == 1 || cal.getUnit().compareToIgnoreCase("pixel") == 0) {
			cal.setUnit("mm");
			cal.pixelWidth = MaudPreferences.getDouble("image2D.pixelWidth", 0.2);
			cal.pixelHeight = MaudPreferences.getDouble("image2D.pixelHeight", 0.2);
		} else if (cal.getUnit().compareToIgnoreCase("inches") == 0) {
			cal.setUnit("mm");
			cal.pixelWidth *= 25.4;
			cal.pixelHeight *= 25.4;
		}
		if (cal.getUnit().compareToIgnoreCase("cm") == 0) {
			cal.setUnit("mm");
			cal.pixelWidth *= 10;
			cal.pixelHeight *= 10;
		}

		double xCenter = angcal.getOriginalCenterX();// * imp.getCalibration().pixelWidth;
		double yCenter = angcal.getOriginalCenterY();// * imp.getCalibration().pixelHeight;
		double radius = angcal.getRadius();
		double omega = MaudPreferences.getDouble("camera.defaultOmegaAngle", 15.0);
		double chi = MaudPreferences.getDouble("camera.defaultChiAngle", 0.0);
		double phi = MaudPreferences.getDouble("camera.defaultPhiAngle", 0.0);
		boolean calibrated = MaudPreferences.getBoolean("anglesCalibration.imageToSpectra", false);
		double startingAngle = MaudPreferences.getDouble("image2D.StartingAngle", 0.0);
		double finalAngle = MaudPreferences.getDouble("image2D.FinalAngle", 360.0);
		int nDivision = MaudPreferences.getInteger("image2D.nSpectraDivision", 72);
//		if (gonioAngles[0] != StringNumber.dummyAngle)
//			detector2Theta = gonioAngles[0];
		if (gonioAngles[1] != StringNumber.dummyAngle)
			omega = gonioAngles[1];
		if (gonioAngles[2] != StringNumber.dummyAngle)
			chi = gonioAngles[2];
		if (gonioAngles[3] != StringNumber.dummyAngle)
			phi = gonioAngles[3];

		LaueCircleStepRoi roi = new LaueCircleStepRoi(imp, radius);
		imp.setRoi(roi);
		roi = (LaueCircleStepRoi) imp.getRoi();
		ImageProcessor ip = imp.getProcessor();
		ip.setCalibrationTable(cal.getCTable());
		roi.setRadius(radius);
		roi.setStartingPoint(xCenter, yCenter);
//        roi.setSelHeight(cameraHeight);
		roi.setOmega(omega);
		roi.setChi(chi);
		roi.setPhi(phi);
		roi.setInReflection(transmission);
		roi.nprofiles = nDivision;
		roi.startingAngle = startingAngle;
		roi.finalAngle = finalAngle;
		if (calibrated)
			roi.setCalibrated();
		else
			roi.setUncalibrated();
//        roi.updateSelection();
		int width = ip.getWidth();
		int height = ip.getHeight();

		int pointsPerPixel = MaudPreferences.getInteger("image2D.pointsPerPixel", 1);
		int minX = MaudPreferences.getInteger("squareRoi.xminValue", 0);
		int maxX = MaudPreferences.getInteger("squareRoi.xmaxValue", width);
		int minY = MaudPreferences.getInteger("squareRoi.yminValue", 0);
		int maxY = MaudPreferences.getInteger("squareRoi.ymaxValue", height);
		int npoints = Math.max(maxX - minX, maxY - minY) * pointsPerPixel;
		double stepIntegration = imp.getCalibration().pixelWidth / pointsPerPixel;
		double coneStep = (finalAngle - startingAngle) / nDivision;
		double halfConeStep = startingAngle - coneStep / 2.0;
		double[][] profile = new double[nDivision][npoints];
		for (int etaIndex = 0; etaIndex < nDivision; etaIndex++) {
			double startAngle = coneStep * etaIndex + halfConeStep;
			double endAngle = startAngle + coneStep;
//			System.out.println(startAngle + " " + endAngle + " " + npoints + " " + minX + " " + maxX + " " + minY + " " + maxY + " " + stepIntegration);
			double[][] single_profile = roi.getPixels(startAngle, endAngle, npoints, minX, maxX, minY, maxY, stepIntegration);
			for (int ix = 0; ix < npoints; ix++) {
				profile[etaIndex][ix] = single_profile[0][ix];
			}
		}
		String directory = Constants.cachesDirectory; // getFolder(); //od.getDirectory();
		String name = getLabel(); //od.getFileName();
		int dotLocation = name.lastIndexOf(".");
		String filename = name.substring(0, dotLocation) + ".esg";
		roi.save(directory, filename, profile, nDivision, npoints, coneStep, startingAngle, stepIntegration);

		getDataFileSet().addDataFileforName(directory + filename, false);

		return true;
	}


}
