/*
 * @(#)AngularFlatImageTransmissionCalibration.java created Aug 10, 2003 Berkeley
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

package it.unitn.ing.rista.diffr.cal;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import javax.swing.*;
import java.awt.*;


/**
 * The AngularFlatImageTransmissionCalibration is a class
 *  
 * @version $Revision: 1.6 $, $Date: 2006/11/10 09:33:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class AngularFlatImageTransmissionCalibration extends AngularCalibration {
  public static String[] diclistc = {"_image_original_center_x", "_image_original_center_y",
                                     "_pd_instr_dist_spec/detc",
                                     "_inst_ang_calibration_center_x", "_inst_ang_calibration_center_y",
                                     "_inst_ang_calibration_beam_sx", "_inst_ang_calibration_beam_sy",
                                     "_inst_ang_calibration_ratio_pixels"};
  public static String[] diclistcrm = {"image original center x (arb)", "image original center y (arb)",
		                                 "sample detector distance (arb)",
                                     "image center x (arb)", "image center y (arb)",
                                     "image tilt cos x", "image tilt cos y", "ratio pixelwidth/pixelheigth"};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshCalibration = true;

  double detectorDistance = MaudPreferences.getDouble("image2D.DetectorDistance", 2000.0);

  public AngularFlatImageTransmissionCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Flat Image Transmission";
    IDlabel = "Flat Image Transmission";
  }

  public AngularFlatImageTransmissionCalibration(XRDcat aobj) {
    this(aobj, "Flat Image Transmission calibration x");
  }

  public AngularFlatImageTransmissionCalibration() {
    identifier = "Flat Image Transmission";
    IDlabel = "Flat Image Transmission";
  }

  public void initConstant() {
    Nstring = 2;
    Nstringloop = 0;
    Nparameter = 6;
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
	  stringField[0] = "0";
	  stringField[1] = "0";
    parameterField[0] = new Parameter(this, getParameterString(0), 200.0,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 10),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 1000));
    for (int i = 1; i < 3; i++)
      parameterField[i] = new Parameter(this, getParameterString(i), 0.0,
              ParameterPreferences.getDouble(getParameterString(i) + ".min", -1.0),
              ParameterPreferences.getDouble(getParameterString(i) + ".max", 1.0));
    for (int i = 3; i < 4; i++)
      parameterField[i] = new Parameter(this, getParameterString(i), 0.0,
              ParameterPreferences.getDouble(getParameterString(i) + ".min", -0.1),
              ParameterPreferences.getDouble(getParameterString(i) + ".max", 0.1));
    for (int i = 5; i < 6; i++)
      parameterField[i] = new Parameter(this, getParameterString(i), 1.0,
              ParameterPreferences.getDouble(getParameterString(i) + ".min", -1.1),
              ParameterPreferences.getDouble(getParameterString(i) + ".max", 1.1));

  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(firstLoading);
    detectorDistance = getParameterValue(0);
  }

  public double getRadius() {
    return detectorDistance;
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

	public void setOriginalCenterX(String value) {
    stringField[0] = value;
  }

  public void setOriginalCenterY(String value) {
    stringField[1] = value;
  }

  public boolean freeAllBasicParameters() {
    for (int i = 1; i < 3; i++)
      parameterField[i].setRefinableCheckBound();
    return true;
  }

  public void calibrateX(DiffrDataFile datafile) {
    int datanumber = datafile.getTotalNumberOfData();
    updateParametertoDoubleBuffering(false);
    double eta = datafile.getEtaValue() * Constants.DEGTOPI;
    double coseta = Math.cos(eta);
    double sineta = Math.sin(eta);
    double Xc = getParameterValue(1);
    double Yc = getParameterValue(2);
    double Sx = getParameterValue(3);
    double Sy = getParameterValue(4);
	  double Cy = Math.sqrt(1.0 - Sy * Sy);
//	  Sx *= Math.sqrt(1.0 - Sy * Sy);  //1
    double ratio = getParameterValue(5);
    double angcal = 0.0;
    for (int i = 0; i < datanumber; i++) {

	    // 3s
      double value = datafile.getXDataForCalibration(i);
      double x = value * coseta * ratio;
      double y = value * sineta;
      double distX = x - Xc;
      double distY = y - Yc;
	    double r = distX * distX + distY * distY;
	    if (r > 10E-80) {
		    r = Math.sqrt(r);
		    double tandelta = (-distX * Cy * Sx + distY * Sy) / r;
		    double delta = Math.atan(tandelta);
		    double cosdelta = Math.cos(delta);
		    double cot2theta = detectorDistance / (r * cosdelta) - tandelta;
		    angcal = Math.atan(1.0 / cot2theta) * Constants.PITODEG;
	    } else
	    	angcal = 0;
	    datafile.setCalibratedXDataOnly(i, angcal);
    }
  }

	public void calibrateX_old(DiffrDataFile datafile) {
		int datanumber = datafile.getTotalNumberOfData();
		updateParametertoDoubleBuffering(false);
		double eta = datafile.getEtaValue() * Constants.DEGTOPI;
		double coseta = Math.cos(eta);
		double sineta = Math.sin(eta);
		double Xc = getParameterValue(1);
		double Yc = getParameterValue(2);
		double Sx = getParameterValue(3);
		double Sy = getParameterValue(4);
//	  Sx *= Math.sqrt(1.0 - Sy * Sy);  //1
		double ratio = getParameterValue(5);
		double angcal = 0.0;
		double detectDist2 = detectorDistance * detectorDistance; //2
		for (int i = 0; i < datanumber; i++) {

			// 3s
			double value = datafile.getXDataForCalibration(i);
			double x = value * coseta * ratio;
			double y = value * sineta;
			double distX = x - Xc;
			double distY = y - Yc;
			double distSx = distX * Sx;
			double distSy = distY * Sy;
			distX *= distX;
			distY *= distY;
			double dist = distSx + distSy;
			double upper = distSx + distSy + detectorDistance;
			double lower = detectDist2 + 2.0 * detectorDistance * dist + distX + distY;
			angcal = Math.acos(upper / Math.sqrt(lower)) * Constants.PITODEG;
			//3f
//4s
/*	    double value = datafile.getXDataOriginal(i);
	    double x = value * coseta * ratio;
	    double y = value * sineta;
	    double distX = x - Xc;
	    double distY = y - Yc;
	    double distSx = distX * Sx;
	    double distSy = distY * Sy;
	    distX *= distX;
	    distY *= distY;
	    double dist = Math.sqrt(distX + distY);
	    if (dist > 1E-80) {
	      double tandelta = (distSx + distSy) / dist;
	      double delta = Math.atan(tandelta);
	      double cota = detectorDistance / (dist * Math.cos(delta)) - tandelta;
	      angcal = Math.atan(1.0 / cota) * Constants.PITODEG;
        datafile.setCalibratedXDataOnly(i, angcal);
	    } else
		    datafile.setCalibratedXDataOnly(i, 0);*/ //4f

			datafile.setCalibratedXDataOnly(i, angcal);
		}
	}

	public double notCalibrated(DiffrDataFile datafile, double x) {
    return x;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new AngularFlatImageTransmissionCalibration.JPolAngOptionsD(parent, this);
    return adialog;
  }

  class JPolAngOptionsD extends JOptionsDialog {

//    JParameterListPane coeffPanel;
	  JTextField[] textfield = null;

    public JPolAngOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

	    principalPanel.setLayout(new BorderLayout(3, 3));

	    JPanel warningPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
	    principalPanel.add(BorderLayout.NORTH, warningPanel);
	    warningPanel.add(new JLabel("Obsolete, use Inclined Reflection Image with 2theta=0 (transmission) and 2theta=180 (reflection)"));

	    JTabbedPane tabPanel = new JTabbedPane();
	    String tempString[] = {"Calibration parameters", "Integration setting"};
	    principalPanel.add(BorderLayout.CENTER, tabPanel);

	    JPanel firstPanel = new JPanel(new GridLayout(0, 4, 3, 3));
	    tabPanel.addTab(tempString[0], null, firstPanel);

	    String[] textStrings = {"Detector distance:", "Center x error:   ", "Center y error:   ",
			    "Tilting error x:  ", "Tilting error y:    ", "Ratio width/height pixels:"};
	    for (int i = 0; i < parameterField.length; i++)
		    addParField(firstPanel, textStrings[i], parameterField[i]);

	    JPanel secondPanel = new JPanel(new GridLayout(0, 4, 3, 3));
	    tabPanel.addTab(tempString[1], null, secondPanel);

	    textfield = new JTextField[stringField.length];

	    textStrings[2] = textStrings[0];
	    textStrings[0] = "Center x:         ";
	    textStrings[1] = "Center y:         ";
	    for (int i = 0; i < stringField.length; i++)
		    addStringField(secondPanel, textStrings[i], i);

	    setTitle("Flat Image in transmission/reflection angular calibration");
      initParameters();

      pack();
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
