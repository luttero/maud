/*
 * @(#)Angular2DCurvedDetectorCalibration.java created Dec 19, 2012 Caen
 *
 * Copyright (c) 2012 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.awt.JParameterListPane;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;

/**
 * The Angular2DCurvedDetectorCalibration is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Oct 23, 2007 11:39:00 PM $
 * @since JDK1.1
 */
public class Angular2DCurvedDetectorCalibration extends AngularCalibration {
	public static String[] diclistc = {"_image_original_center_x", "_image_original_center_y",

			"_pd_instr_dist_spec/detc",
			"_inst_ang_calibration_center_x", "_inst_ang_calibration_center_y",
			"_inst_ang_calibration_detc_2theta_shift", "_inst_ang_calibration_ratio_pixels"};
	public static String[] diclistcrm = {"image original center x (arb)", "image original center y (arb)",
			"sample detector distance (arb)",
			"image center x (arb)", "image center y (arb)",
			"image 2theta shift (deg)", "ratio pixelheigth/pixelwidth (correction)"};

	public static String[] classlistc = {};
	public static String[] classlistcs = {};

	boolean refreshCalibration = true;

	public Angular2DCurvedDetectorCalibration(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initXRD();
		identifier = "2D curved image";
		IDlabel = "2D curved image";
	}

	public Angular2DCurvedDetectorCalibration(XRDcat aobj) {
		this(aobj, "2D curved image calibration x");
	}

	public Angular2DCurvedDetectorCalibration() {
		identifier = "2D curved image";
		IDlabel = "2D curved image";
	}

	@Override
	public void initConstant() {
		Nstring = 2;
		Nstringloop = 0;
		Nparameter = 5;
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
		stringField[0] = "0.0";
		stringField[1] = "0.0";
		parameterField[0] = new Parameter(this, getParameterString(0), 200.0,
				ParameterPreferences.getDouble(getParameterString(0) + ".min", 10),
				ParameterPreferences.getDouble(getParameterString(0) + ".max", 1000));
		for (int i = 1; i < 3; i++)
			parameterField[i] = new Parameter(this, getParameterString(i), 0.0,
					ParameterPreferences.getDouble(getParameterString(i) + ".min", -10.0),
					ParameterPreferences.getDouble(getParameterString(i) + ".max", 10.0));
		parameterField[3] = new Parameter(this, getParameterString(3), 0.0,
				ParameterPreferences.getDouble(getParameterString(3) + ".min", -1),
				ParameterPreferences.getDouble(getParameterString(3) + ".max", 1));
		parameterField[4] = new Parameter(this, getParameterString(4), 1.0,
				ParameterPreferences.getDouble(getParameterString(4) + ".min", 0.9),
				ParameterPreferences.getDouble(getParameterString(4) + ".max", 1.1));
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

	public double getRadius() {
		return parameterField[0].getValueD();
	}

	public void setRadius(String value) {
		parameterField[0].setValue(value);
	}

	public double getOriginalCenterX() {
		return Double.parseDouble(stringField[0]);
	}

	public double getOriginalCenterY() {
		return Double.parseDouble(stringField[1]);
	}

	@Override
	public void setOriginalCenterX(String value) {
		stringField[0] = value;
	}

	@Override
	public void setOriginalCenterY(String value) {
		stringField[1] = value;
	}

	public void setOriginalCenterX(double value) {
		stringField[0] = Double.toString(value);
//		parameterField[1].setValue(0.0);
	}

	public void setOriginalCenterY(double value) {
		stringField[1] = Double.toString(value);
//		parameterField[2].setValue(0.0);
	}

	@Override
	public boolean freeAllBasicParameters() {
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

	@Override
	public void calibrateX(DiffrDataFile datafile) {
		double angcal;
		double cosxr, tan2theta;

		int datanumber = datafile.getTotalNumberOfData();
		updateParametertoDoubleBuffering(false);

		double detectorDistance = getParameterValue(0);
		double centerX = getParameterValue(1);
		centerX += getOriginalCenterX();
		double centerY = getParameterValue(2);
		centerY += getOriginalCenterY();
		double radius2 = detectorDistance * detectorDistance;
		double theta2Error = getParameterValue(3);
		double pixelRatio = getParameterValue(4);

		for (int i = 0; i < datanumber; i++) {
			double x = datafile.getXDataImage(i);
			double y = datafile.getYDataImage(i);

			double arg = (x - centerX) / detectorDistance;
			cosxr = detectorDistance * Math.cos(arg);
			double cos2xr = cosxr * cosxr;
//			sinxr = detectorDistance * Math.sin(arg);
			double yr = (y - centerY) * pixelRatio;
			if (Math.abs(cosxr) > 1E-9) {
				tan2theta = Math.sqrt((radius2 + yr * yr) / cos2xr - 1);
				if (cosxr < 0)
					tan2theta = -tan2theta;
				angcal = MoreMath.atand(tan2theta);
				if (arg > Math.PI * 0.5 && angcal < 0)
					angcal = 180.0 + angcal;
			} else {
				if (arg > 0 && arg < Math.PI)
					angcal = 90.0;
				else
					angcal = -90.0;
			}
/*					if (Math.abs(sinxr) > 1E-9) {
						taneta = yr / sinxr;
						eta[index] = MoreMath.atand(taneta);
					} else {
						if (yr < 0)
							eta[index] = -90;
						else
							eta[index] = 90;
					}*/
			angcal += theta2Error;
			datafile.setCalibratedXDataOnly(i, angcal);
		}
	}


	@Override
	public double notCalibrated(DiffrDataFile datafile, double x) {
		return x;
	}

	@Override
	public JOptionsDialog getOptionsDialog(Frame parent) {
		JOptionsDialog adialog = new Angular2DCurvedDetectorCalibration.JAngOptionsD(parent, this);
		return adialog;
	}

	class JAngOptionsD extends JOptionsDialog {

		JParameterListPane coeffPanel;
		JTextField originalCenterXTF;
		JTextField originalCenterYTF;

		public JAngOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new GridLayout(0, 4, 3, 3));

			principalPanel.add(new JLabel("Original center X: "));
			originalCenterXTF = new JTextField(Constants.FLOAT_FIELD);
			originalCenterXTF.setToolTipText("The original x coordinate for integration");
			principalPanel.add(originalCenterXTF);
			principalPanel.add(new JLabel("Original center Y: "));
			originalCenterYTF = new JTextField(Constants.FLOAT_FIELD);
			originalCenterYTF.setToolTipText("The original y coordinate for integration");
			principalPanel.add(originalCenterYTF);

			addParField(principalPanel, "Detector distance :       ", parameterField[0]);
			addParField(principalPanel, "Center x:    ", parameterField[1]);
			addParField(principalPanel, "Center y:    ", parameterField[2]);
			addParField(principalPanel, "Detector 2theta:          ", parameterField[3]);
			addParField(principalPanel, "Ratio width/height pixels:", parameterField[4]);

			setTitle("2D curved image angular calibration");
			initParameters();

			pack();
		}

		@Override
		public void initParameters() {
			super.initParameters();
			originalCenterXTF.setText(Double.toString(getOriginalCenterX()));
			originalCenterYTF.setText(Double.toString(getOriginalCenterY()));
		}

		public void retrieveParameters() {
			super.retrieveParameters();
			setOriginalCenterX(originalCenterXTF.getText());
			setOriginalCenterY(originalCenterYTF.getText());
		}


	}

}
