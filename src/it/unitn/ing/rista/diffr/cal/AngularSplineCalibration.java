/*
 * @(#)AngularSplineCalibration.java created 17/05/2001 Mesiano
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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
 *  The AngularSplineCalibration is a class
 *
 *
 * @version $Revision: 1.7 $, $Date: 2005/05/06 18:07:25 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class AngularSplineCalibration extends AngularCalibration {
  public static String[] diclistc = {"_inst_ang_calibration_coeff"};
  public static String[] diclistcrm = {"spline calibration coeff "};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshCalibration = true;

  int numberCoeff = 0;
  double difc[] = null;

  public AngularSplineCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Spline Angular";
    IDlabel = "Spline Angular";
  }

  public AngularSplineCalibration(XRDcat aobj) {
    this(aobj, "Spline calibration x");
  }

  public AngularSplineCalibration() {
    identifier = "Spline Angular";
    IDlabel = "Spline Angular";
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

    int nSpline = numberCoeff - 3;
    if (nSpline <= 0)
      return;

    double original_range = datafile.getXDataForCalibration(datanumber - 1) -
            datafile.getXDataForCalibration(0);
    double w_range = original_range / (nSpline + 1);
    double w_range2 = w_range * w_range;
    double w_range3 = w_range2 * w_range;
//    double remaining = original_range - w_range * nSpline;

    int range_id = -1;
    double startX = datafile.getXDataForCalibration(0);
    double endX = datafile.getXDataForCalibration(0) - 1.0;

    double a = 0.0;
    double b = 0.0;
    double c = 0.0;
    double d = 0.0;

    for (int i = 0; i < datanumber; i++) {
      double value = datafile.getXDataForCalibration(i);
      if (value > endX) {
        range_id++;
        startX = endX;
        endX = getEndX(range_id, w_range);
//	      System.out.println(range_id + " " + startX + " " + endX);
        if (range_id == 0) {
          startX = datafile.getXDataForCalibration(0);
          d = difc[0];
          c = difc[1];
          b = difc[2] / w_range2;
          a = difc[3] / w_range3;
        } else {
          double deltaX = endX - startX;
          double deltaX2 = deltaX * deltaX;
          double deltaX3 = deltaX2 * deltaX;
          d = a * deltaX3 + b * deltaX2 + c * deltaX + d;
          c = 3 * a * deltaX2 + 2 * b * deltaX + c;
          b = 3 * a * deltaX + b;
	        if (range_id >= numberCoeff - 3)
		        a = 0;
	        else
            a = difc[range_id + 3] / w_range3;
        }
      }
      double delta = value - startX;
      double delta2 = delta * delta;
      double angcal = a * delta * delta2 + b * delta2 + c * delta + d;
      datafile.setCalibratedXDataOnly(i, angcal);
    }
  }

  public double calibrateX(DiffrDataFile datafile, double value) {
/*    double angcal = 0.0;
    for (int j = 0; j < numberCoeff; j++) {
      angcal += difc[j] * MoreMath.pow(value, j);
    } */         // to be implemented
    return value; //angcal;
  }

/*  int getRangeIndex(int index, int nSpline, int w_range, int remaining) {
    int i = 0;
    while (i < nSpline) {
      if (index >= getStartX(i, w_range, remaining) && index <= getEndX(i, w_range, remaining))
        return i;
      i++;
    }
    return -1;
  }

  int getStartX(double i, double w_range) {
    if (i < rest)
      return i * w_range + i;
    else
      return i * w_range + remaining;
  }*/

  double getEndX(int i, double w_range) {
    return w_range * (i + 1);
  }

  public double notCalibrated(DiffrDataFile datafile, double x) {
    return 0.0;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JPolSplineOptionsD(parent, this);
    return adialog;
  }

  class JPolSplineOptionsD extends JOptionsDialog {

    JParameterListPane coeffPanel;

    public JPolSplineOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      coeffPanel = new JParameterListPane(this, false, true);
      principalPanel.add(BorderLayout.CENTER, coeffPanel);

      setTitle("Spline angular calibration");
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
