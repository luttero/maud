/*
 * @(#)IntensityPolAngCalibration.java created 11/01/2001 Mesiano
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
 *  The IntensityPolAngCalibration is a class to calibrate the intensity
 *  with a polynomial function of the point position (angular or d-space)
 *
 *
 * @version $Revision: 1.8 $, $Date: 2006/07/20 13:39:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class IntensityPolAngCalibration extends IntensityCalibration {
  public static String[] diclistc = {"_inst_intensity_cal_coeff", "_inst_intensity_cal_coeff_for_y"};
  public static String[] diclistcrm = {"calibration coeff ", "calibration coeff Y "};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshCalibration = true;

  int numberCoeff = 0;
  double difc[] = null;
	int numberCoeffy = 0;
	double dify[] = null;

  public IntensityPolAngCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Polynomial Angular Intensity";
    IDlabel = "Polynomial Angular Intensity";
  }

  public IntensityPolAngCalibration(XRDcat aobj) {
    this(aobj, "Polynomial Angular intensity x");
  }

  public IntensityPolAngCalibration() {
    identifier = "Polynomial Angular Intensity";
    IDlabel = "Polynomial Angular Intensity";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
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
    difc = (double[]) parameterLoopValuesVector.elementAt(0);
    numberCoeff = difc.length;
	  dify = (double[]) parameterLoopValuesVector.elementAt(1);
	  numberCoeffy = dify.length;
  }

/*  public String getCoeff(int index) {
    return getCoeffP(index).getValue();
  }

  public void addCoeff(int index, String value) {
    addparameterloopField(0, new Parameter(this, getParameterString(0, index), value, "0",
            ParameterPreferences.getPref(getParameterString(0, index) + ".min", "0"),
            ParameterPreferences.getPref(getParameterString(0, index) + ".max", "1"), false));
  }

  public Parameter getCoeffP(int index) {
    return (Parameter) parameterloopField[0].elementAt(index);
  }*/

  public double calibrateData(DiffrDataFile datafile, double x, int index, double coord) {

    updateParametertoDoubleBuffering(false);

    if (numberCoeff == 0 && numberCoeffy == 0)
      return 1.0;

    double intcal = 1.0;
	  if (numberCoeff > 0) {
		  if (getInstrument().getAngularCalibration() instanceof AngularInclinedFlatImageCalibration) {
		    double centerX = getInstrument().getAngularCalibration().getOriginalCenterY();
			  centerX += getInstrument().getAngularCalibration().getParameterValue(1);
			  double y = Math.abs(x - centerX);
			  for (int j = 0; j < numberCoeff; j++)
				  intcal += difc[j] * MoreMath.pow(y, j+1);
		  } else {
			  for (int j = 0; j < numberCoeff; j++)
				  intcal += difc[j] * MoreMath.pow(x, j+1);
		  }
	  }

	  if (numberCoeffy > 0) {
			double centerY = getInstrument().getAngularCalibration().getOriginalCenterY();
		  double pixelRatio = 1;
		  if (getInstrument().getAngularCalibration() instanceof Angular2DCurvedDetectorCalibration) {
			  centerY += getInstrument().getAngularCalibration().getParameterValue(2);
			  pixelRatio = getInstrument().getAngularCalibration().getParameterValue(4);
		  }
	    double y = Math.abs(datafile.getYDataImage(index) - centerY) * pixelRatio;
	    for (int j = 0; j < numberCoeffy; j++)
		    intcal += dify[j] * MoreMath.pow(y, j+1);
	  }
    return intcal;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JPolIntOptionsD(parent, this);
    return adialog;
  }

  class JPolIntOptionsD extends JOptionsDialog {

    JParameterListPane coeffPanel;
	  JParameterListPane coeffPanely;

    public JPolIntOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      coeffPanel = new JParameterListPane(this, false, true);
      principalPanel.add(BorderLayout.NORTH, coeffPanel);
	    coeffPanely = new JParameterListPane(this, false, true);
	    principalPanel.add(BorderLayout.CENTER, coeffPanely);

      setTitle("Intensity polynomial calibration (on angles and on y for images)");
      initParameters();

      pack();
    }

    public void initParameters() {
      coeffPanel.setList(XRDparent, 0);
	    coeffPanely.setList(XRDparent, 1);
    }

    public void retrieveParameters() {
      coeffPanel.retrieveparlist();
	    coeffPanely.retrieveparlist();
    }

    public void dispose() {
      coeffPanel.dispose();
	    coeffPanely.dispose();
      super.dispose();
    }

  }

}
