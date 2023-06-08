/*
 * @(#)IntensityFilmCalibration.java created 25/06/2000 Casalino, Pergine
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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
import it.unitn.ing.rista.util.function.*;

import java.awt.*;
import javax.swing.*;


/**
 *  The IntensityFilmCalibration is a class to calibrate the intensity
 *  obtained by film exposure using the general formula:
 *  Ifilm = Iteor * A * (1 - (2 / (1 + B^(C * x^D))))
 *
 *
 * @version $Revision: 1.7 $, $Date: 2005/05/06 18:07:25 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class IntensityFilmCalibration extends IntensityCalibration {
  public static String[] diclistc = {"_inst_intensity_cal_coeff"};
  public static String[] diclistcrm = {"calibration coeff "};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshCalibration = true;

  int numberCoeff = 0, fixNumber = 5;
  double difc[] = null;

  public IntensityFilmCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Film Calibration";
    IDlabel = "Film Calibration";
  }

  public IntensityFilmCalibration(XRDcat aobj) {
    this(aobj, "Film Calibration x");
  }

  public IntensityFilmCalibration() {
    identifier = "Film Calibration";
    IDlabel = "Film Calibration";
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
  }

  public void checkCoeff() {
    updateParametertoDoubleBuffering(false);
    isAbilitatetoRefresh = false;
    numberCoeff = numberofelementPL(0);
    for (; numberCoeff < fixNumber; numberCoeff++)
      addCoeff(numberCoeff, "0");
    for (; numberCoeff > fixNumber; numberCoeff--)
      parameterloopField[0].removeItemAt(numberCoeff - 1);
    isAbilitatetoRefresh = true;
    updateParametertoDoubleBuffering(false);
  }

  public String getCoeff(int index) {
    return getCoeffP(index).getValue();
  }

  public double getCoeffD(int index) {
    return getCoeffP(index).getValueD();
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

  public double calibrateData(double x) {
    if (numberCoeff != fixNumber)
      checkCoeff();

//    System.out.println(difc[0] + " " + Math.pow(difc[1], difc[2] * Math.pow(x, difc[3])) + " " + Math.pow(x, difc[3]) + " " + x + " " + difc[0] * (1.0 - (2.0 / (1.0 + Math.pow(difc[1], difc[2] * Math.pow(x, difc[3]))))));
		double val = Math.abs(x - difc[4]);
    return difc[0] * (1.0 - (2.0 / (1.0 + Math.pow(difc[1], difc[2] * Math.pow(val, difc[3])))));
  }

  public void plotFunction(Frame theframe, int index) {
    ParameterFunction function = new ParameterFunction(parameterloopField[index]) {
      public double f(double x) {
        return calibrateData(x);
      }
    };
    (new PlotParameterFunction(theframe, function)).setVisible(true);
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JFilmIntOptionsD(parent, this);
    return adialog;
  }

  class JFilmIntOptionsD extends JOptionsDialog {

    JParameterListPane coeffPanel;

    public JFilmIntOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      principalPanel.add(BorderLayout.NORTH,
              new JLabel("Formula: Ifilm=A*(1-(2/(1+B^(C*(Iteor-E)^D))))"));

      coeffPanel = new JParameterListPane(this, false, true, false);
      principalPanel.add(BorderLayout.CENTER, coeffPanel);

      setTitle("Intensity film calibration");
      initParameters();

      pack();
    }

    public void initParameters() {
      checkCoeff();
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
