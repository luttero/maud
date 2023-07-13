/*
 * @(#)AngularCameraCalibration.java created 29/02/2002 Mesiano
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

package it.unitn.ing.rista.diffr.cal;

import it.unitn.ing.rista.diffr.*;

import java.lang.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import java.awt.*;


/**
 *  The AngularCameraCalibration is a class
 *
 *
 * @version $Revision: 1.8 $, $Date: 2006/11/10 09:33:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class AngularCameraCalibration extends AngularCalibration {
  public static String[] diclistc = {"_image_original_center_x", "_image_original_center_y",
                                     "_inst_ang_calibration_start", "_inst_ang_calibration_radius"/*,
                                     "_inst_ang_calibration_delta_x", "_inst_ang_calibration_delta_y"*/};
  public static String[] diclistcrm = {"image original center x (arb)", "image original center y (arb)",
                                      "starting angle 2theta (deg)", "camera radius (mm)"/*,
                                      "x error camera center (mm)", "y error camera center (mm)"*/};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshCalibration = true;

  double startX = 0.0;
  double radius = 200.0;
  double deltaX = 0;
  double deltaY = 0;

  public AngularCameraCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Camera angular";
    IDlabel = "Camera angular";
  }

  public AngularCameraCalibration(XRDcat aobj) {
    this(aobj, "Camera angular calibration x");
  }

  public AngularCameraCalibration() {
    identifier = "Camera angular";
    IDlabel = "Camera angular";
  }

  public void initConstant() {
    Nstring = 2;
    Nstringloop = 0;
    Nparameter = 2; //4;
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
    parameterField[0] = new Parameter(this, getParameterString(0), 0,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", -10),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 100));
    parameterField[1] = new Parameter(this, getParameterString(1), 200.0,
            ParameterPreferences.getDouble(getParameterString(1) + ".min", 10),
            ParameterPreferences.getDouble(getParameterString(1) + ".max", 1000));
/*    parameterField[2] = new Parameter(this, getParameterString(2), 0,
            ParameterPreferences.getDouble(getParameterString(2) + ".min", -1),
            ParameterPreferences.getDouble(getParameterString(2) + ".max", 1));
    parameterField[3] = new Parameter(this, getParameterString(3), 0,
            ParameterPreferences.getDouble(getParameterString(3) + ".min", -1),
            ParameterPreferences.getDouble(getParameterString(3) + ".max", 1));*/

  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(firstLoading);
    startX = getParameterValue(0);
    radius = getParameterValue(1);
//    deltaX = getParameterValue(2);
//    deltaY = getParameterValue(3);
  }

  public boolean freeAllBasicParameters() {
    parameterField[0].setRefinableCheckBound();
    return true;
  }

	public boolean needUncalibrated() {
		return true;
	}

	public void calibrateX(DiffrDataFile datafile) {
    int datanumber = datafile.getTotalNumberOfData();
    updateParametertoDoubleBuffering(false);
    DataFileSet dataset = datafile.getDataFileSet();
    double zs = dataset.getZshift();
    double xs = dataset.getXshift();
    double c1 = 1.0 / radius;
    double angcal;
    for (int i = 0; i < datanumber; i++) {
      double value = datafile.getXDataForCalibration(i);

      angcal = (value + startX) * c1;
      angcal += (xs * Math.cos(angcal) + zs * Math.sin(angcal)) * c1;

      datafile.setCalibratedXDataOnly(i, angcal * Constants.PITODEG);
    }
  }

  public double notCalibrated(DiffrDataFile datafile, double x) {
    return x;
  }

  public double getRadius() {
    return radius;
  }
  
  public void setRadius(String value) {
    parameterField[1].setValue(value);
  }

  public void setStartingValue(String value) {
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
//    parameterField[1].setValue(0.0);
  }

  public void setOriginalCenterY(String value) {
    stringField[1] = value;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JPolAngOptionsD(parent, this);
    return adialog;
  }

  class JPolAngOptionsD extends JOptionsDialog {

    JParameterListPane coeffPanel;

    public JPolAngOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new GridLayout(0, 2, 3, 3));

      addParField(principalPanel, "Starting value  : ", parameterField[0]);
      addParField(principalPanel, "Camera radius   : ", parameterField[1]);
//      addParField(principalPanel, "x position error: ", parameterField[2]);
//      addParField(principalPanel, "y position error: ", parameterField[3]);

      setTitle("Camera angular calibration");
      initParameters();

      pack();
    }

    public void initParameters() {
    }

  }

}
