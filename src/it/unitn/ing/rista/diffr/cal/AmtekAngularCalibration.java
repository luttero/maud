/*
 * @(#)AmtekAngularCalibration.java created Feb 9, 2005 Casalino
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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
import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.awt.JParameterListPane;

import java.awt.*;


/**
 * The AmtekAngularCalibration is a class to calibrate the Amtek solid state detector
 * or similar energy dispersive detectors
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2005/05/06 18:07:25 $
 * @since JDK1.1
 */

public class AmtekAngularCalibration extends AngularCalibration {

  public static final double Etod = 2.0 * Math.PI / 1.01354;

  public static String[] diclistc = {
    "_instrument_counter_bank_ID",
    "_instrument_bank_tof_theta",
    "_inst_ang_calibration_coeff"};
  public static String[] diclistcrm = {
    "_instrument_counter_bank_ID",
    "2theta angle (deg) ",
    "d-spacing calibration coeff"};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshCalibration = true;

  int numberCoeff = 0;
  double[] difc = null;
  double[] theta = null;

  public AmtekAngularCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Amtek polynomial";
    IDlabel = "Amtek polynomial";
  }

  public AmtekAngularCalibration(XRDcat aobj) {
    this(aobj, "Amtek polynomial calibration x");
  }

  public AmtekAngularCalibration() {
    identifier = "Amtek polynomial";
    IDlabel = "Amtek polynomial";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 1;
    Nparameter = 0;
    Nparameterloop = 2;
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
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(firstLoading);
    theta = (double[]) parameterLoopValuesVector.elementAt(0);
    difc = (double[]) parameterLoopValuesVector.elementAt(1);
    numberCoeff = difc.length;
  }

  public String getCoeff(int index) {
    return getCoeffP(index).getValue();
  }

  public double getCoeffD(int index) {
    return getCoeffP(index).getValueD();
  }

  public void addCoeff(int index, String value) {
    addparameterloopField(1, new Parameter(this, getParameterString(1, index), value, "0",
            ParameterPreferences.getPref(getParameterString(1, index) + ".min", "-1"),
            ParameterPreferences.getPref(getParameterString(1, index) + ".max", "1"), false));
  }

  public Parameter getCoeffP(int index) {
    return (Parameter) parameterloopField[1].elementAt(index);
  }

  public void setCoeff(int index, String value) {
    getCoeffP(index).setValue(value);
  }

  public int getBankNumber(DiffrDataFile datafile) {
    return datafile.getAngBankNumber();
  }

  public double getTthetaValue(DiffrDataFile datafile, double twotheta) {
    return theta[getBankNumber(datafile)];
  }

  public void calibrateX(DiffrDataFile datafile) {
    int datanumber = datafile.getTotalNumberOfData();
    updateParametertoDoubleBuffering(false);
    double theta = getTthetaValue(datafile, 0.0) * Constants.DEGTOPI / 2.0;
    double sintheta = Math.sin(theta);

    for (int i = 0; i < datanumber; i++) {
      double angcal = 0.0;
      double value = datafile.getXDataForCalibration(i);
      for (int j = 0; j < numberCoeff; j++) {
        angcal += difc[j] * MoreMath.pow(value, j);
      }
      datafile.setCalibratedXDataOnly(i, Etod / (angcal * sintheta));
    }
  }

  public double calibrateX(DiffrDataFile datafile, double value) {
    double angcal = 0.0;
    for (int j = 0; j < numberCoeff; j++) {
      angcal += difc[j] * MoreMath.pow(value, j);
    }
    return angcal;
  }

   public double notCalibrated(DiffrDataFile datafile, double x) {
    return 0.0;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JPolAngOptionsD(parent, this);
    return adialog;
  }

  class JPolAngOptionsD extends JOptionsDialog {

    JParameterListPane coeffPanel;

    public JPolAngOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      coeffPanel = new JParameterListPane(this, false, true);
      principalPanel.add(BorderLayout.CENTER, coeffPanel);

      setTitle("Amtek polynomial angular calibration");
      initParameters();

      pack();
    }

    public void initParameters() {
      coeffPanel.setList(XRDparent, 0);
    }

    public void retrieveParameters() {
      coeffPanel.retrieveparlist();
    }

    public void dispose() {
      coeffPanel.dispose();
      super.dispose();
    }

  }

}
