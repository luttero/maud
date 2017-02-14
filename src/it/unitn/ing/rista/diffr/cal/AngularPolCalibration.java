/*
 * @(#)AngularPolCalibration.java created 10/07/1998 ILL, Grenoble
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

import java.lang.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import java.awt.*;


/**
 *  The AngularPolCalibration is a class to apply a polynomial function
 *  to calibrate angular data. The original data is converted in 2theta angles
 *  or d-spacing (depending on the datafile loaded) using the polynomial
 *  function here implemented (there is no limit on the polynomial degree).
 *
 *
 * @version $Revision: 1.9 $, $Date: 2006/11/10 09:33:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class AngularPolCalibration extends AngularCalibration {
  public static String[] diclistc = {"_inst_ang_calibration_coeff"};
  public static String[] diclistcrm = {"calibration coeff "};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshCalibration = true;

  int numberCoeff = 0;
  double difc[] = null;

  public AngularPolCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Polynomial Angular";
    IDlabel = "Polynomial Angular";
  }

  public AngularPolCalibration(XRDcat aobj) {
    this(aobj, "Polynomial calibration x");
  }

  public AngularPolCalibration() {
    identifier = "Polynomial Angular";
    IDlabel = "Polynomial Angular";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 1;
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
    difc = (double[]) parameterLoopValuesVector.elementAt(0);
    numberCoeff = difc.length;
  }

  public String getCoeff(int index) {
    return getCoeffP(index).getValue();
  }

  public double getCoeffD(int index) {
    return getCoeffP(index).getValueD();
  }

  public void addCoeff(int index, String value) {
    addparameterloopField(0, new Parameter(this, getParameterString(0, index), value, "0",
            ParameterPreferences.getPref(getParameterString(0, index) + ".min", "-1"),
            ParameterPreferences.getPref(getParameterString(0, index) + ".max", "1"), false));
  }

  public Parameter getCoeffP(int index) {
    return (Parameter) parameterloopField[0].elementAt(index);
  }

  public void setCoeff(int index, String value) {
    getCoeffP(index).setValue(value);
  }

  public void calibrateX(DiffrDataFile datafile) {
    int datanumber = datafile.getTotalNumberOfData();
    updateParametertoDoubleBuffering(false);
    for (int i = 0; i < datanumber; i++) {
      double angcal = 0.0;
      double value = datafile.getXDataForCalibration(i);
      for (int j = 0; j < numberCoeff; j++) {
        angcal += difc[j] * MoreMath.pow(value, j);
      }
      datafile.setCalibratedXDataOnly(i, angcal);
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

      setTitle("Polynomial angular calibration");
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
