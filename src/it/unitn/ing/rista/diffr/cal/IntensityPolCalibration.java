/*
 * @(#)IntensityPolCalibration.java created 10/07/1998 ILL, Grenoble
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
 *  The IntensityPolCalibration is a class to calibrate intensity
 *  using a polynomial function. The calibration is indipendent from the
 *  angular or d-space position.
 *
 *
 * @version $Revision: 1.8 $, $Date: 2006/07/20 13:39:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class IntensityPolCalibration extends IntensityCalibration {
  public static String[] diclistc = {"_inst_intensity_cal_coeff"};
  public static String[] diclistcrm = {"calibration coeff "};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshCalibration = true;

  int numberCoeff = 0;
  double difc[] = null;

  public IntensityPolCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Polynomial Intensity";
    IDlabel = "Polynomial Intensity";
  }

  public IntensityPolCalibration(XRDcat aobj) {
    this(aobj, "Polynomial intensity x");
  }

  public IntensityPolCalibration() {
    identifier = "Polynomial Intensity";
    IDlabel = "Polynomial Intensity";
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
    return Double.valueOf(getCoeff(index)).doubleValue();
  }

  public void addCoeff(int index, String value) {
    addparameterloopField(0, new Parameter(this, getParameterString(0, index), value, "0",
            ParameterPreferences.getPref(getParameterString(0, index) + ".min", "0"),
            ParameterPreferences.getPref(getParameterString(0, index) + ".max", "1"), false));
  }

  public Parameter getCoeffP(int index) {
    return (Parameter) parameterloopField[0].elementAt(index);
  }

  public void setCoeff(int index, String value) {
    getCoeffP(index).setValue(value);
  }

  public double calibrateData(double value) {

//    updateParametertoDoubleBuffering(false);

    if (numberCoeff == 0)
      return value;

    double intcal = 1.0;
    for (int j = 0; j < numberCoeff; j++)
      intcal += difc[j] * MoreMath.pow(value, j+1);
    return intcal;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JPolIntOptionsD(parent, this);
    return adialog;
  }

  class JPolIntOptionsD extends JOptionsDialog {

    JParameterListPane coeffPanel;

    public JPolIntOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      coeffPanel = new JParameterListPane(this, false, true);
      principalPanel.add(BorderLayout.CENTER, coeffPanel);

      setTitle("Intensity polynomial calibration (saturation)");
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
