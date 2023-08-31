/*
 * @(#)TOFPanelCalibration.java created 7/06/2023 Los Alamos
 *
 * Copyright (c) 2099 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
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

import it.unitn.ing.rista.diffr.*;

import java.io.*;
import java.lang.*;
import java.util.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 *  The TOFPanelCalibration is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2023/06/7 15:56:04 $
 * @author Luca Lutterotti
 * @since JDK19
 */

// Constants.LAMBDA_SPEED_NEUTRON_CONV_ANG

public class TOFPanelCalibration extends XRDcat {

	static int DIFA_ID = 0;
	static int ZERO_ID = 1;
	static int DISTANCE_ID = 2;
	static int THETA_ID = 3;
	static int ETA_ID = 4;
	static int CENTER_X_ID = 5;
	static int CENTER_Y_ID = 6;
	static int TILT_X_ID = 7;
	static int ROTATION_Z_ID = 8;
	static int SHIFT_ID = -1;

	public static String[] diclistc = {
			"_instrument_bank_ID", "_bank_original_dist_spec/detc",
			"_bank_original_tof_theta", "_bank_original_tof_eta",
			"_bank_original_center_x", "_bank_original_center_y",
			"_bank_original_tilt_x", "_bank_original_rotation_z",

			"_instrument_bank_difa", "_instrument_bank_zero",
			"_pd_instr_dist_spec/detc",
			"_instrument_bank_tof_theta", "_instrument_bank_tof_eta",
			"_instrument_bank_center_x", "_instrument_bank_center_y",
			"_instrument_bank_tilt_x", "_instrument_bank_rot_z"
	};
	public static String[] diclistcrm = {
			"_instrument_bank_ID", "_bank_original_dist_spec/detc",
			"_bank_original_tof_theta", "_bank_original_tof_eta",
			"_bank_original_center_x", "_bank_original_center_y",
			"_bank_original_tilt_x", "_bank_original_rotation_z",

			"_instrument_bank_difa", "_instrument_bank_zero",
			"_pd_instr_dist_spec/detc",
			"_instrument_bank_tof_theta", "_instrument_bank_tof_eta",
			"_instrument_bank_center_x", "_instrument_bank_center_y",
			"_instrument_bank_tilt_x", "_instrument_bank_rot_z"};

	public static String[] classlistc = {};
	public static String[] classlistcs = {};

//	boolean refreshCalibration = true;

	double dist = 120.0;
	double difa = 0;
	double zero = 0;
	double theta = 0;
	double eta = 0;
	double center_x = 0;
	double center_y = 0;
	double tilt_x = 0;
	double rot_z = 0;
	int maxNumberCoefficient = ROTATION_Z_ID + 1;

//	int choosedPanelNumber = 0;

	public static String modelID = "TOF 2D Bank";

	public TOFPanelCalibration(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initBaseObject();
		identifier = modelID;
		IDlabel = modelID;
	}

	public TOFPanelCalibration(XRDcat aobj) {
		this(aobj, "");
	}

	public TOFPanelCalibration() {
		identifier = modelID;
		IDlabel = modelID;
	}

	public void initConstant() {
		Nstring = 8;
		Nstringloop = 0;
		Nparameter = maxNumberCoefficient;
		Nparameterloop = 0;
		Nsubordinate = 0;
		Nsubordinateloop = 0;
	}

	public void initDictionary() {
		for (int i = 0; i < totsubordinateloop; i++)
			diclist[i] = diclistc[i];
		System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
		for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
			classlist[i] = classlistc[i];
		for (int i = 0; i < totsubordinate - totparameterloop; i++)
			classlists[i] = classlistcs[i];
	}

	public void initParameters() {
		super.initParameters();

		double ldistance = MaudPreferences.getDouble("TOFbank2D.defaultDistance", 120.0);
		double ltheta2 = MaudPreferences.getDouble("TOFbank2D.default2ThetaAngle", 90.0);
		double leta = MaudPreferences.getDouble("TOFbank2D.defaultEtaAngle", 0.0);
		double lx = MaudPreferences.getDouble("TOFbank2D.centerX", 57.5);
		double ly = MaudPreferences.getDouble("TOFbank2D.centerY", 57.5);
		double ltilt = MaudPreferences.getDouble("TOFbank2D.defaultTiltAngle", 0.0);
		double lrot = MaudPreferences.getDouble("TOFbank2D.defaultRotationAngle", 0.0);

	   parameterField[DISTANCE_ID] = new Parameter(this, getParameterString(DISTANCE_ID), ldistance,
				ParameterPreferences.getDouble(getParameterString(DISTANCE_ID) + ".min", 0.0),
				ParameterPreferences.getDouble(getParameterString(DISTANCE_ID) + ".max", 10000.0));
		parameterField[THETA_ID] = new Parameter(this, getParameterString(THETA_ID), ltheta2,
				ParameterPreferences.getDouble(getParameterString(THETA_ID) + ".min", -180.0),
				ParameterPreferences.getDouble(getParameterString(THETA_ID) + ".max", 180.0));
		parameterField[ETA_ID] = new Parameter(this, getParameterString(ETA_ID), leta,
				ParameterPreferences.getDouble(getParameterString(ETA_ID) + ".min", -180.0),
				ParameterPreferences.getDouble(getParameterString(ETA_ID) + ".max", 180.0));
		parameterField[CENTER_X_ID] = new Parameter(this, getParameterString(CENTER_X_ID), lx,
				ParameterPreferences.getDouble(getParameterString(CENTER_X_ID) + ".min", 0.0),
				ParameterPreferences.getDouble(getParameterString(CENTER_X_ID) + ".max", 1000.0));
		parameterField[CENTER_Y_ID] = new Parameter(this, getParameterString(CENTER_Y_ID), ly,
				ParameterPreferences.getDouble(getParameterString(CENTER_Y_ID) + ".min", 0.0),
				ParameterPreferences.getDouble(getParameterString(CENTER_Y_ID) + ".max", 1000.0));
		parameterField[TILT_X_ID] = new Parameter(this, getParameterString(TILT_X_ID), ltilt,
				ParameterPreferences.getDouble(getParameterString(TILT_X_ID) + ".min", -2.0),
				ParameterPreferences.getDouble(getParameterString(TILT_X_ID) + ".max", 2.0));
		parameterField[ROTATION_Z_ID] = new Parameter(this, getParameterString(ROTATION_Z_ID), lrot,
				ParameterPreferences.getDouble(getParameterString(ROTATION_Z_ID) + ".min", -5.0),
				ParameterPreferences.getDouble(getParameterString(ROTATION_Z_ID) + ".max", 5.0));

		stringField[0] = "Bank 2D X";
		stringField[1] = parameterField[DISTANCE_ID].getValue();
		for (int i = 2; i < Nstring; i++)
			stringField[i] = parameterField[i + SHIFT_ID].getValue();

		refreshComputation = true;
	}

	public void updateStringtoDoubleBuffering(boolean firstLoading) {
		super.updateStringtoDoubleBuffering(false);
	}

	public void updateParametertoDoubleBuffering(boolean firstLoading) {
		if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
			return;
		isAbilitatetoRefresh = true;
		super.updateParametertoDoubleBuffering(firstLoading);
		isAbilitatetoRefresh = false;

		dist = getParameterValue(DISTANCE_ID);
		difa = getParameterValue(DIFA_ID);
		zero = getParameterValue(ZERO_ID);
		theta = getParameterValue(THETA_ID);
		eta = getParameterValue(ETA_ID);
		center_x = getParameterValue(CENTER_X_ID);
		center_y = getParameterValue(CENTER_Y_ID);
		tilt_x = getParameterValue(TILT_X_ID);
		rot_z = getParameterValue(ROTATION_Z_ID);

		if (dist == 0)
			dist = 1.0;
		dist *= 1000.0;  // m -> mm

		isAbilitatetoRefresh = true;
	}

	public void notifyParameterChanged(Parameter source) {
		FilePar filepar = getFilePar();
		if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
			for (int i = 0; i < Nparameter; i++)
				if (source == getParameter(i)) {
					notifyParameterChanged(source, Constants.ANGULAR_CALIBRATION, -1);
					notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED, -1);
					notifyParameterChanged(source, Constants.TEXTURE_CHANGED, -1);
					return;
				}
			super.notifyParameterChanged(source);
		}
	}

	public boolean freeAllBasicParameters() {
		getParameter(CENTER_X_ID).setRefinableCheckBound();
		getParameter(CENTER_Y_ID).setRefinableCheckBound();
		return true;
	}

	public String getBankID() {
		return stringField[0];
	}
	public void calibrateData(DiffrDataFile datafile) {
	}

	public void calibrateX(DiffrDataFile datafile) {
		int datanumber = datafile.getTotalNumberOfData();
		updateParametertoDoubleBuffering(false);
		double flightPath = ((MultiTOFPanelCalibration) getParent()).getFlightPath();
		double difc_part = 1.0E3 / (Constants.LAMBDA_SPEED_NEUTRON_CONV_ANG * 2.0);

		DataFileSet dataset = datafile.getDataFileSet();
		double zs = dataset.getZshift();
		double rx = dataset.getSample().getRadiusDimensionXD();
		double ry = dataset.getSample().getRadiusDimensionYD();
		double[] tiltingAngles = datafile.getTiltingAngle();
		double omega = tiltingAngles[0];
		double chi = tiltingAngles[1];
		double detectorProper2Theta = theta + tiltingAngles[4];

		if (Math.abs(omega) > 1.0E-18)
			zs /= Math.cos((90.0 - omega) * Constants.DEGTOPI);
		zs /= Math.cos(chi * Constants.DEGTOPI);
		double dx = zs * Math.cos((detectorProper2Theta - 90.0) * Constants.DEGTOPI);
		double dd = zs * Math.sin((detectorProper2Theta - 90.0) * Constants.DEGTOPI);

		for (int i = 0; i < datanumber; i++) {
         double value = datafile.getXDataOriginal(i);
			double x = datafile.getXDataImage(i);
			double y = datafile.getYDataImage(i);
			double[] xf = getThetaEtaAndDist(x, y,
					omega, detectorProper2Theta, eta,
					tilt_x,  rot_z,
					center_x,  center_y,  dist,
					dx,  dd,  rx,  zs);

			double difc = difc_part / ((xf[2] + flightPath) * Math.sin(xf[0] * 0.5));
			double angcal_d = (-difc + Math.sqrt(difc * difc - 4.0 * difa * (zero - value))) / (2.0 * difa);
			if (i == datanumber/2)
				System.out.println(difc + " " + value + " " + angcal_d);

			datafile.setCalibratedXDataOnly(i, angcal_d);
			datafile.setTwothetaImage(i, xf[0] * Constants.PITODEG);
			datafile.setEtaImage(i, xf[1] * Constants.PITODEG);
			datafile.setDistanceImage(i, xf[2]);
		}
	}

	public double calibrateX(DiffrDataFile datafile, double value) {

		// This is only for GSAS instrument broadening functions

		double flightPath = ((MultiTOFPanelCalibration) getParent()).getFlightPath();
		double difc_part = 1.0E3 / (Constants.LAMBDA_SPEED_NEUTRON_CONV_ANG * 2.0);

		DataFileSet dataset = datafile.getDataFileSet();
		double zs = dataset.getZshift();
		double rx = dataset.getSample().getRadiusDimensionXD();
		double ry = dataset.getSample().getRadiusDimensionYD();
		double[] tiltingAngles = datafile.getTiltingAngle();
		double omega = tiltingAngles[0];
		double chi = tiltingAngles[1];
		double detectorProper2Theta = theta + tiltingAngles[4];

		if (Math.abs(omega) > 1.0E-18)
			zs /= Math.cos((90.0 - omega) * Constants.DEGTOPI);
		zs /= Math.cos(chi * Constants.DEGTOPI);
		double dx = zs * Math.cos((detectorProper2Theta - 90.0) * Constants.DEGTOPI);
		double dd = zs * Math.sin((detectorProper2Theta - 90.0) * Constants.DEGTOPI);

		int i = datafile.getOriginalNearestPoint(value);
		double x = datafile.getXDataImage(i);
		double y = datafile.getYDataImage(i);
		double[] xf = getThetaEtaAndDist(x, y,
				omega, detectorProper2Theta, eta,
				tilt_x,  rot_z,
				center_x,  center_y,  dist,
				dx,  dd,  rx,  zs);

		double difc = difc_part / ((xf[2] + flightPath) * Math.sin(xf[0] * 0.5));
		double angcal_d = (-difc + Math.sqrt(difc * difc - 4.0 * difa * (zero - value))) / (2.0 * difa);

		return angcal_d;
	}

	public double notCalibrated(DiffrDataFile datafile, double x) {
		// This is only for GSAS instrument broadening functions

		double flightPath = ((MultiTOFPanelCalibration) getParent()).getFlightPath();
		double difc_part = 1.0E3 / (Constants.LAMBDA_SPEED_NEUTRON_CONV_ANG * 2.0);
		int i = datafile.getOriginalNearestPoint(x);
		double twotheta = datafile.getTwothetaImage(i) * Constants.DEGTOPI;
		double dist = datafile.getDistanceImage(i);
		double difc = difc_part / ((dist + flightPath) * Math.sin(twotheta * 0.5));
		return difc * x + difa * x * x + zero;
	}

/*	public void panelUnrolling(DiffrDataFile datafile, double detector2Theta, double detectorEta,
	                           double detectorTiltX, double detectorRotationZ,
	                           double centerX, double centerY, double distance) {

//		datafile.getIma

		double[] xf;
		double angcal;

		int datanumber = datafile.getTotalNumberOfData();
//		updateParametertoDoubleBuffering(false);

		DataFileSet dataset = datafile.getDataFileSet();
		double zs = dataset.getZshift();
		double rx = dataset.getSample().getRadiusDimensionXD();
		double ry = dataset.getSample().getRadiusDimensionYD();
		double[] tiltingAngles = datafile.getTiltingAngle();
		double omega = tiltingAngles[0];
		double chi = tiltingAngles[1];
		double detectorProper2Theta = detector2Theta + tiltingAngles[4];

		if (Math.abs(omega) > 1.0E-18)
			zs /= Math.cos((90.0 - omega) * Constants.DEGTOPI);
		zs /= Math.cos(chi * Constants.DEGTOPI);
		double dx = zs * Math.cos((detectorProper2Theta - 90.0) * Constants.DEGTOPI);
		double dd = zs * Math.sin((detectorProper2Theta - 90.0) * Constants.DEGTOPI);

		for (int i = 0; i < datanumber; i++) {
//      double value = datafile.getXDataOriginal(i);
			double x = datafile.getXDataImage(i);
			double y = datafile.getYDataImage(i);
			xf = getThetaEtaAndDist(x, y,
					omega, detectorProper2Theta, detectorEta,
					detectorTiltX,  detectorRotationZ,
					centerX,  centerY,  distance,
					dx,  dd,  rx,  zs);

			double difc =
			double angcal =
			datafile.setCalibratedXDataOnly(i, angcal);
		}
	}*/

	public double[] getThetaEtaAndDist(double x, double y, double omega,
	                                   double detectorProper2Theta, double detectorEta,
	                                   double detectorTiltX, double detectorRotationZ,
	                                   double centerX, double centerY, double distance,
	                                   double dx, double dd, double rx, double zs) {

		double[] res = new double[3];

		double[][] tmat = getTmat(omega, detectorProper2Theta, detectorEta, detectorTiltX, detectorRotationZ);
		double[] xf = ConvertImageToSpectra.getTransformedVectorNew(tmat, x, y,
				centerX + dx, centerY, distance - dd);
		double[] etatheta = ConvertImageToSpectra.get2ThetaEtaNew(xf); // * Constants.PITODEG;
		res[0] = etatheta[0];  // theta, rad
		res[1] = etatheta[1];  // eta, rad

		if (rx != 0) { // || ry != 0) {
			double angcal = ConvertImageToSpectra.get2ThetaNew(xf);
			double xc = rx * Math.sin((Constants.PI - angcal) * 0.5);
			double zc = rx * (1.0 - Math.cos(Constants.PI_2 - angcal));
			double dx1 = (zs - zc) * Math.cos((detectorProper2Theta - 90.0) * Constants.DEGTOPI);
			double dd1 = (zs - zc) * Math.sin((detectorProper2Theta - 90.0) * Constants.DEGTOPI);
			double dx2 = xc * Math.sin((detectorProper2Theta - 90.0) * Constants.DEGTOPI);
			double dd2 = xc * Math.cos((detectorProper2Theta - 90.0) * Constants.DEGTOPI);
//			if (Math.abs(omega) > 1.0E-2)
//				zs /= Math.sin(omega * Constants.DEGTOPI);
			dd = dd1 + dd2;
			dx = dx1 + dx2;
			xf = ConvertImageToSpectra.getTransformedVectorNew(tmat, x, y,
					centerX + dx, centerY, distance - dd);
			etatheta = ConvertImageToSpectra.get2ThetaEtaNew(xf);
			res[0] = etatheta[0];
			res[1] = etatheta[1];
		}

		centerX += dx;
		double dx2 = x - centerX;
		dx2 *= dx2;
		double dy2 = y - centerY;
		dy2 *= dy2;
		distance -= dd;
		res[2] = Math.sqrt(distance * distance + dx2 + dy2);
		return res;
	}
	public double[][] getTmat(double omega, double detector2Theta, double detectorEta, double detectorTiltX, double detectorRotationZ) {
		return ConvertImageToSpectra.getTransformationMatrixNew(detectorRotationZ, detectorTiltX,
				detectorEta, detector2Theta, omega);
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
//		double thetaPoint = datafile.getTwothetaImage(index);
		double detectorProper2Theta = theta + tiltingAngles[4];
		zs /= Math.cos(chi * Constants.DEGTOPI);
		double dx = zs * Math.sin((180.0 - detectorProper2Theta) * Constants.DEGTOPI);
		double dd = zs * Math.cos((180.0 - detectorProper2Theta) * Constants.DEGTOPI);

		double x = datafile.getXDataImage(index);
		double y = datafile.getYDataImage(index);
//		double detectorDistance = datafile.getDistanceImage(index);
		double detectorDistance = dist;

		double[] xf = ConvertImageToSpectra.getTransformedVectorNew(tmat, x, y, center_x + dx, center_y, detectorDistance - dd);
		double[] xfc = ConvertImageToSpectra.getTransformedVectorNew(tmat, 0, 0, center_x + dx, center_y, detectorDistance - dd);

		double angle = Math.abs(MoreMath.getAngleBetweenPoints(xf, xfc));
		angle += Constants.PI_2;

		return angle;
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
		JTOFPanelOptionsDialog adialog = new JTOFPanelOptionsDialog(parent, this);
		return adialog;
	}

	class JTOFPanelOptionsDialog extends JOptionsDialog {

		JTextField[] textfield = null;
		public JTOFPanelOptionsDialog(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new BorderLayout(3, 3));

			JTabbedPane tabPanel = new JTabbedPane();
			String tempString[] = {"Calibration parameters", "Data importing setting"};
			principalPanel.add(BorderLayout.CENTER, tabPanel);

			JPanel firstPanel = new JPanel(new GridLayout(0, 4, 3, 3));
			tabPanel.addTab(tempString[0], null, firstPanel);

			String[] textStrings = {"Difa parameter: ", "Zero parameter: ", "Bank distance:  ",
					                  "Bank 2theta:    ", "Bank eta:       ", "Center x error: ",
					                  "Center y error: ", "Bank tilt:      ", "Bank rotation:  "};
			for (int i = 0; i < textStrings.length; i++)
				addParField(firstPanel, textStrings[i], parameterField[i]);

			JPanel secondPanel = new JPanel(new BorderLayout(3, 3));
			tabPanel.addTab(tempString[1], null, secondPanel);

			JPanel secondPanelTop = new JPanel(new GridLayout(0, 4, 3, 3));
			secondPanel.add(secondPanelTop, BorderLayout.CENTER);

			textfield = new JTextField[stringField.length];

			for (int i = 0; i < textStrings.length - 3; i++)
				textStrings[i] = textStrings[i + 2];
			textStrings[3] = "Center x:       ";
			textStrings[4] = "Center y:       ";

			String[] tooltipStrings = {
					"Sample-detector distance used in the image integration, default in Maud preferences with keyword: TOFbank2D.defaultDistance",
					"Position of the detector in 2theta during image integration; pref keyword: TOFbank2D.default2ThetaAngle",
					"Position of the detector in eta (along diffraction circles) during image integration; pref keyword: TOFbank2D.defaultEtaAngle",
					"X coord on the image of the center used in the image integration (where the line perpendicular to the detector plane and passing by the sample center will hit the detector virtual plane, can be outside the image); pref keyword: TOFbank2D.centerX",
					"Y coord on the image of the center used in the image integration (where the line perpendicular to the detector plane and passing by the sample center will hit the detector virtual plane, can be outside the image); pref keyword: TOFbank2D.centerY",
					"Tilt of the detector around its horizontal axis during image integration; pref keyword: TOFbank2D.defaultTiltAngle",
					"Rotation of the detector around its perpendicular axis passing by the center during image integration; pref keyword: TOFbank2D.defaultRotationAngle"};
			for (int i = 0; i < stringField.length - 1; i++)
				addStringField(secondPanelTop, textStrings[i], tooltipStrings[i], i);

			setTitle("TOF 2D Bank calibration");
			initParameters();

			pack();
		}

		@Override
		public void retrieveParameters() {
			for (int i = 0; i < stringField.length - 1; i++)
				stringField[i] = textfield[i].getText();
			super.retrieveParameters();
		}

		public void addStringField(JPanel apanel, String label, String tooltip, int stringFieldNumber) {
			apanel.add(new JLabel(label));
			textfield[stringFieldNumber] = new JTextField(Constants.FLOAT_FIELD);
			textfield[stringFieldNumber].setText(stringField[stringFieldNumber]);
			textfield[stringFieldNumber].setToolTipText(tooltip);
			apanel.add(textfield[stringFieldNumber]);
		}

	}

}
