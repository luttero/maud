/*
 * @(#)AngularInclinedFlatImageCalibration.java created Oct 23, 2007 Caen
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
package it.unitn.ing.rista.diffr.cal;

//import com.sun.tools.javac.code.Attribute;
import ij.ImagePlus;
import ij.gui.FlatCCDReflectionSquareRoi;
import ij.process.ImageProcessor;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.io.StringNumber;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import javax.swing.border.BevelBorder;
import java.awt.*;
import java.util.Vector;

/**
 * The AngularInclinedFlatImageCalibration is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Oct 23, 2007 11:39:00 PM $
 * @since JDK1.1
 */
public class AngularInclinedFlatImageCalibration extends AngularCalibration {
  public static String[] diclistc = {"_image_original_dist_spec/detc",
		                                 "_image_original_center_x", "_image_original_center_y",
		                                 "_image_original_detc_2theta", "_image_original_detc_phiDA",
		                                 "_image_original_detc_omegaDN", "_image_original_detc_etaDA",
		                                 "_image_original_rotation_inversion",

                                     "_pd_instr_dist_spec/detc",
                                     "_inst_ang_calibration_center_x", "_inst_ang_calibration_center_y",
                                     "_inst_ang_calibration_detc_2theta", "_inst_ang_calibration_detc_phiDA",
                                     "_inst_ang_calibration_detc_omegaDN", "_inst_ang_calibration_detc_etaDA"};
  public static String[] diclistcrm = {"original detector distance (arb)",
		                                   "image original center x (arb)", "image original center y (arb)",
		                                   "original 2theta (deg)", "original phiDA (deg)",
		                                   "original rotation (deg)", "original etaDA (deg)",
		                                 "transform image with roto inversion",

		                                   "sample detector distance (arb)",
                                       "image center x (arb)", "image center y (arb)",
                                       "image 2theta (deg)", "image phiDA (deg)",
                                       "image rotation (deg)", "image etaDA (deg)"};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshCalibration = true;

  private double detectorDistance = 200.0;
  private double centerX = 0.0;
  private double centerY = 0.0;

  private double detector2Theta = 30.0;
  private double detectorPhiDA = 0.0;
  private double detectorOmegaDN = 0.0;
  private double detectorEtaDA = 0.0;

  private int rotationInversion = 0;

	public static String modelID = "Inclined Reflection Image";

	public AngularInclinedFlatImageCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = modelID;
    IDlabel = modelID;
  }

  public AngularInclinedFlatImageCalibration(XRDcat aobj) {
    this(aobj, "Inclined Reflection Image calibration x");
  }

  public AngularInclinedFlatImageCalibration() {
    identifier = modelID;
    IDlabel = modelID;
  }

  @Override
  public void initConstant() {
    Nstring = 8;
    Nstringloop = 0;
    Nparameter = 7;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

    @Override
  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
  }

  @Override
  public void initParameters() {
    super.initParameters();
	  stringField[0] = MaudPreferences.getPref("pixelDetector.defaultDetectorDistance", "85.0");
    stringField[1] = "0.0";
    stringField[2] = "0.0";
	  stringField[3] = MaudPreferences.getPref("pixelDetector.default2ThetaAngle", "40.0");
	  stringField[4] = MaudPreferences.getPref("pixelDetector.defaultPhiDAangle", "0.0");
	  stringField[5] = MaudPreferences.getPref("pixelDetector.defaultOmegaDNangle", "0.0");
	  stringField[6] = MaudPreferences.getPref("pixelDetector.defaultEtaDAangle", "0.0");
	  stringField[7] = MaudPreferences.getPref("pixelDetector.defaultRotoInversion", "0");
    parameterField[0] = new Parameter(this, getParameterString(0), 85.0,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 10),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 1000));
    for (int i = 1; i < 3; i++)
      parameterField[i] = new Parameter(this, getParameterString(i), 0.0,
              ParameterPreferences.getDouble(getParameterString(i) + ".min", -10000.0),
              ParameterPreferences.getDouble(getParameterString(i) + ".max", 10000.0));
    parameterField[3] = new Parameter(this, getParameterString(3), 60.0,
            ParameterPreferences.getDouble(getParameterString(3) + ".min", 0),
            ParameterPreferences.getDouble(getParameterString(3) + ".max", 180));
    parameterField[4] = new Parameter(this, getParameterString(4), 20.0,
            ParameterPreferences.getDouble(getParameterString(4) + ".min", -180),
            ParameterPreferences.getDouble(getParameterString(4) + ".max", 180));
    parameterField[5] = new Parameter(this, getParameterString(5), 0.0,
            ParameterPreferences.getDouble(getParameterString(5) + ".min", -10),
            ParameterPreferences.getDouble(getParameterString(5) + ".max", 10));
    parameterField[6] = new Parameter(this, getParameterString(6), 0.0,
            ParameterPreferences.getDouble(getParameterString(6) + ".min", -180),
            ParameterPreferences.getDouble(getParameterString(6) + ".max", 180));
  }

  public void setDetectorDistance(double value) {
    parameterField[0].setValue(value);
  }

  public void setDetectorCenterX(double value) {
    parameterField[1].setValue(value);
  }

  public void setDetectorCenterY(double value) {
    parameterField[2].setValue(value);
  }

  public void setDetector2Theta(double value) {
    parameterField[3].setValue(value);
  }

  public void setDetectorPhiDA(double value) {
    parameterField[4].setValue(value);
  }

  public void setDetectorOmegaDN(double value) {
    parameterField[5].setValue(value);
  }

  public void setDetectorEtaDA(double value) {
    parameterField[6].setValue(value);
  }

  public double getDetector2Theta() {
    return detector2Theta;
  }

  public double getDetectorPhiDA() {
    return detectorPhiDA;
  }

  public double getDetectorOmegaDN() {
    return detectorOmegaDN;
  }

  public double getDetectorEtaDA() {
    return detectorEtaDA;
  }

  @Override
  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(firstLoading);
    detectorDistance = getParameterValue(0);
    centerX = getParameterValue(1);
    centerY = getParameterValue(2);
    detector2Theta = getParameterValue(3);
    detectorPhiDA = getParameterValue(4);
    detectorOmegaDN = getParameterValue(5);
    detectorEtaDA = getParameterValue(6);
  }

  public double getRadius() {
    return detectorDistance;
  }

  public void setRadius(String value) {
    parameterField[0].setValue(value);
  }

	public double getOriginalDistance() {
		return Double.parseDouble(stringField[0]);
	}

	public void setOriginalDistance(double value) {
		stringField[0] = Double.toString(value);
	}

	public double getOriginalCenterX() {
    return Double.parseDouble(stringField[1]);
  }

	public void setOriginalCenterX(double value) {
		stringField[1] = Double.toString(value);
	}

	public double getOriginalCenterY() {
    return Double.parseDouble(stringField[2]);
  }

  public void setOriginalCenterY(double value) {
    stringField[2] = Double.toString(value);
  }

	public double getOriginal2Theta() {
		return Double.parseDouble(stringField[3]);
	}

	public void setOriginal2Theta(double value) {
		stringField[3] = Double.toString(value);
	}

	public double getOriginalPhiDA() {
		return Double.parseDouble(stringField[4]);
	}

	public void setOriginalPhiDA(double value) {
		stringField[4] = Double.toString(value);
	}

	public double getOriginalOmegaDN() {
		return Double.parseDouble(stringField[5]);
	}

	public void setOriginalOmegaDN(double value) {
		stringField[5] = Double.toString(value);
	}

	public double getOriginalEtaDA() {
		return Double.parseDouble(stringField[6]);
	}

	public void setOriginalEtaDA(double value) {
		stringField[6] = Double.toString(value);
	}

	public int getOriginalRotoInversionOperation() { return Integer.parseInt(stringField[7]); }

	public void setOriginalRotoInversionOperation(int value) { stringField[7] = Integer.toString(value); }

	@Override
  public boolean freeAllBasicParameters() {
		if (((DataFileSet) getInstrument().getParent()).activedatafilesnumber() > 9)
         for (int i = 1; i < 3; i++)
            parameterField[i].setRefinableCheckBound();
    return true;
  }

  public void refreshForNotificationDown(XRDcat source, int reason) {
    if (!getFilePar().isComputingDerivate() || (source == this ||
            (reason == Constants.ERROR_POSITION_CHANGED))) {
      refreshComputation = true;
      getParent().getParent().refreshForNotificationUp(this, Constants.ANGULAR_CALIBRATION);
    }
  }

/*  public void refreshForNotificationUp(XRDcat source, int reason) {
    refreshComputation = true;
    if (reason == Constants.SAMPLE_Z_POSITION_CHANGED) {
      refreshComputation = true;
      getParent().getParent().refreshForNotificationUp(this, Constants.ANGULAR_CALIBRATION);
    }
  }*/

	public boolean positionAlreadyCorrected() {
		return true;
	}

  @Override
  public void calibrateX(DiffrDataFile datafile) {
    double[] xf;
    double angcal;

    int datanumber = datafile.getTotalNumberOfData();
    updateParametertoDoubleBuffering(false);

    DataFileSet dataset = datafile.getDataFileSet();
    double zs = dataset.getZshift();
    double rx = dataset.getSample().getRadiusDimensionXD();
    double ry = dataset.getSample().getRadiusDimensionYD();
    double[] tiltingAngles = datafile.getTiltingAngle();
    double omega = tiltingAngles[0];
	  double chi = tiltingAngles[1];
	  double detectorProper2Theta = detector2Theta + tiltingAngles[4];

	  if (Math.abs(omega) > 1.0E-9)
      zs /= Math.cos((90.0 - omega) * Constants.DEGTOPI);
    zs /= Math.cos(chi * Constants.DEGTOPI);
    double dx = zs * Math.cos((detectorProper2Theta - 90) * Constants.DEGTOPI);
    double dd = zs * Math.sin((detectorProper2Theta - 90) * Constants.DEGTOPI);

    double[][] tmat = ConvertImageToSpectra.getTransformationMatrixNew(detectorOmegaDN, detectorPhiDA,
        detectorEtaDA, detectorProper2Theta, omega);
    for (int i = 0; i < datanumber; i++) {
//      double value = datafile.getXDataOriginal(i);
      double x = datafile.getXDataImage(i);
      double y = datafile.getYDataImage(i);

      xf = ConvertImageToSpectra.getTransformedVectorNew(tmat, x, y, centerX + dx, centerY, detectorDistance - dd);
      angcal = ConvertImageToSpectra.get2ThetaNew(xf) * Constants.PITODEG;
//	    System.out.println(i + " " + angcal);

      if (rx != 0/* || ry != 0*/) {
        double xc = rx * MoreMath.sind(90.0 - angcal / 2.0);
        double zc = rx * (1.0 - MoreMath.cosd(90.0 - angcal));
        double dx1 = (zs - zc) * Math.cos((detectorProper2Theta - 90) * Constants.DEGTOPI);
        double dd1 = (zs - zc) * Math.sin((detectorProper2Theta - 90) * Constants.DEGTOPI);
        double dx2 = xc * Math.sin((detectorProper2Theta - 90) * Constants.DEGTOPI);
        double dd2 = xc * Math.cos((detectorProper2Theta - 90) * Constants.DEGTOPI);
        xf = ConvertImageToSpectra.getTransformedVectorNew(tmat, x, y, centerX + dx1 + dx2, centerY, detectorDistance - dd1 - dd2);
        angcal = ConvertImageToSpectra.get2ThetaNew(xf) * Constants.PITODEG;
      }

      datafile.setCalibratedXDataOnly(i, angcal);
    }
  }

	public double[][] getTmat(double omega) {
		return ConvertImageToSpectra.getTransformationMatrixNew(detectorOmegaDN, detectorPhiDA,
				detectorEtaDA, detector2Theta, omega);
	}

	public double getBeamInclination(DiffrDataFile datafile, int index, double[][] tmat) {
		updateParametertoDoubleBuffering(false);

		double zs = datafile.getDataFileSet().getZshift();
//    System.out.println("zs = " + zs);
		double[] tiltingAngles = datafile.getTiltingAngle();
		double omega = tiltingAngles[0];
		if (Math.abs(omega) > 1.0E-9)
			zs /= Math.sin(omega * Constants.DEGTOPI);
		double chi = tiltingAngles[1];
		double detectorProper2Theta = detector2Theta + tiltingAngles[4];
		zs /= Math.cos(chi * Constants.DEGTOPI);
		double dx = zs * Math.sin((180.0 - detectorProper2Theta) * Constants.DEGTOPI);
		double dd = zs * Math.cos((180.0 - detectorProper2Theta) * Constants.DEGTOPI);

		double x = datafile.getXDataImage(index);
		double y = datafile.getYDataImage(index);

		double[] xf = ConvertImageToSpectra.getTransformedVectorNew(tmat, x, y, centerX + dx, centerY, detectorDistance - dd);
		double[] xfc = ConvertImageToSpectra.getTransformedVectorNew(tmat, 0, 0, centerX + dx, centerY, detectorDistance - dd);

		double angle = Math.abs(MoreMath.getAngleBetweenPoints(xf, xfc));
		angle += Constants.PI_2;

		return angle;
	}

	@Override
	public boolean loadAndUnrollImage(ImagePlus imp, MultDiffrDataFile mdatafile, double[] gonioAngles) {
		boolean loadSuccessfull = false;
//		AngularCalibration angcal = this;
		String directory = mdatafile.getFolder(); //od.getDirectory();
		String name = mdatafile.getLabel(); //od.getFileName();
		ij.measure.Calibration cal = imp.getCalibration();

		System.out.println("Opening tiff: "+ imp.getBitDepth());
		if (imp.getBitDepth() == 32) {

		}
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
		
		System.out.println("Before rotation: " + ip.getWidth() + " " + ip.getHeight());

		rotationInversion = getOriginalRotoInversionOperation();

		switch (rotationInversion) {
			case 2: // rotation 180 clockwise
        ip = ip.rotateRight();
//				System.out.println("Rotate right image!");
			case 1: // rotation 90 clockwise
//				System.out.println("Rotate right image!");
				ip = ip.rotateRight();
				break;
			case 4: // rotation 180 anticlockwise
//				System.out.println("Rotate left image!");
        ip = ip.rotateLeft();
			case 3: // rotation 90 anticlockwise
//				System.out.println("Rotate left image!");
        ip = ip.rotateLeft();
				break;
			case 7: // flip both
//				System.out.println("Flip vertical image!");
        ip.flipVertical();
			case 5: // flip horizontal
//				System.out.println("Flip horizontal image!");
        ip.flipHorizontal();
				break;
			case 6: // flip vertical
//				System.out.println("Flip vertical image!");
				ip.flipVertical();
				break;
			default: {}
		}
    
    System.out.println("After rotation: " + ip.getWidth() + " " + ip.getHeight());
		int width = ip.getWidth();
		int height = ip.getHeight();
		double[] buffer = new double[width * height];
		for (int ix = 0; ix < width; ix++)
			for (int iy = 0; iy < height; iy++)
				buffer[ix + iy * width] = ip.getPixelValue(ix, iy);
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
		double azimuthal = getOriginalPhiDA();
		double phiDetector = getOriginalOmegaDN();
		double coneAngle = getOriginalEtaDA();
		double detector2Theta = getOriginal2Theta();
		double detectorDistance = getOriginalDistance();
		double centerX = getOriginalCenterX();
		double centerY = getOriginalCenterY();
		double coneInterval = MaudPreferences.getDouble("pixelDetector.defaultDiffractionConeInterval", 5.0);
		double theta2Step = MaudPreferences.getDouble("pixelDetector.defaultDiffractionStepAngle", 0.02);
		double omega = MaudPreferences.getDouble("sample.defaultOmegaAngle", 55.0);
		double chi = MaudPreferences.getDouble("sample.defaultChiAngle", 17.0);
		double phi = MaudPreferences.getDouble("sample.defaultPhiAngle", 0.0);
		if (gonioAngles[0] != StringNumber.dummyAngle)
			detector2Theta += gonioAngles[0];
		if (gonioAngles[1] != StringNumber.dummyAngle)
			omega = gonioAngles[1];
		if (gonioAngles[2] != StringNumber.dummyAngle)
			chi = gonioAngles[2];
		if (gonioAngles[3] != StringNumber.dummyAngle)
			phi = gonioAngles[3];

		int minX = MaudPreferences.getInteger("squareRoi.xminValue", 0);
		int maxX = Math.min(MaudPreferences.getInteger("squareRoi.xmaxValue", width), width);
		int minY = MaudPreferences.getInteger("squareRoi.yminValue", 0);
		int maxY = Math.min(MaudPreferences.getInteger("squareRoi.ymaxValue", height), height);
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

		double xmin = min2theta * Constants.PITODEG;
		double etaStart = mineta * Constants.PITODEG;
		int dotLocation = name.lastIndexOf(".");
		String filename = name.substring(0, dotLocation) + ".esg";
		if (!MaudPreferences.getBoolean("imageUnrolling.saveEsgFileInCachesDir", false)) {
			System.out.println("Conversion to spectra done! Name to save: " + filename);
			FlatCCDReflectionSquareRoi.saveAsText(profile, profile[0].length, 0, profile[0][0].length, xmin, theta2Step,
					etaStart, coneInterval, directory, filename, "mm", detectorDistance, omega, chi, phi, detector2Theta,
					true);
		}
		mdatafile.setLabel(filename);
		for (int spectrumIndex = 0; spectrumIndex < profile[0].length; spectrumIndex++) {
			String numberString = Integer.toString(spectrumIndex);
			DiffrDataFile datafile = mdatafile.addDiffrDatafile(numberString);
			boolean atmpB = datafile.isAbilitatetoRefresh;
			datafile.isAbilitatetoRefresh = false;

			datafile.setDataType(datafile.DIFFRACTION_IMAGE);
			datafile.setAngleValue(0, omega);
			datafile.setAngleValue(1, chi);
			datafile.setAngleValue(2, phi);
			datafile.setAngleValue(3, etaStart + spectrumIndex * coneInterval);
			datafile.setAngleValue(4, detector2Theta);

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

	@Override
  public double notCalibrated(DiffrDataFile datafile, double x) {
    return x;
  }

  @Override
  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new AngularInclinedFlatImageCalibration.JPolAngOptionsD(parent, this);
    return adialog;
  }

  class JPolAngOptionsD extends JOptionsDialog {

		JTextField[] textfield = null;

	  public JPolAngOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(3, 3));

	    JTabbedPane tabPanel = new JTabbedPane();
	    String tempString[] = {"Calibration parameters", "Integration setting"};
	    principalPanel.add(BorderLayout.CENTER, tabPanel);

	    JPanel firstPanel = new JPanel(new GridLayout(0, 4, 3, 3));
	    tabPanel.addTab(tempString[0], null, firstPanel);

		  String[] textStrings = {"Detector distance:", "Center x error:   ", "Center y error:   ",
				                      "Detector 2theta:  ", "Detector tilt:    ", "Detector rotation:",
				                      "Detector eta:     ", "Process image:    "};
		  for (int i = 0; i < parameterField.length; i++)
	      addParField(firstPanel, textStrings[i], parameterField[i]);

	    JPanel secondPanel = new JPanel(new GridLayout(0, 4, 3, 3));
	    tabPanel.addTab(tempString[1], null, secondPanel);

		  textfield = new JTextField[stringField.length];

		  textStrings[1] = "Center x:   ";
		  textStrings[2] = "Center y:   ";
		  for (int i = 0; i < stringField.length; i++)
		    addStringField(secondPanel, textStrings[i], i);

	    setTitle("2D Image angular calibration");
      initParameters();

      pack();
    }

  @Override
    public void initParameters() {

    }

	  public void retrieveParameters() {
		  for (int i = 0; i < stringField.length; i++)
			  stringField[i] = textfield[i].getText();
		  super.retrieveParameters();
	  }

	  public void addStringField(JPanel apanel, String label, int stringFieldNumber) {
		  apanel.add(new JLabel(label));
		  textfield[stringFieldNumber] = new JTextField(Constants.FLOAT_FIELD);
		  textfield[stringFieldNumber].setText(stringField[stringFieldNumber]);
		  apanel.add(textfield[stringFieldNumber]);
	  }

  }

}
