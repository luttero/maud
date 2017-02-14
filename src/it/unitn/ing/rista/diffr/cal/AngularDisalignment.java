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
import it.unitn.ing.rista.util.ParameterPreferences;
import it.unitn.ing.rista.util.MoreMath;
import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.awt.JParameterListPane;

import java.awt.*;

/**
 *  The AngularDisalignment is a class to apply a shift into twotheta
 * or d-spacing correction.
 *
 *
 * @version $Revision: 1.1 $, $Date: 2006/11/10 17:08:24 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class AngularDisalignment extends AngularCalibration {
  public static String[] diclistc = {"_riet_par_2-theta_offset"};
  public static String[] diclistcrm = {"2-theta or d-spacing offset "};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshCalibration = true;

  int numberCoeff = 0;
  double difc[] = null;

  public AngularDisalignment(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Instrument disalignment";
    IDlabel = "Instrument disalignment";
  }

  public AngularDisalignment(XRDcat aobj) {
    this(aobj, "Instrument disalignment x");
  }

  public AngularDisalignment() {
    identifier = "Instrument disalignment";
    IDlabel = "Instrument disalignment";
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

	public void initializeAsNew() {
		if (initialized)
			return;
		initialized = true;

		addparameterloopField(0, new Parameter(this, getParameterString(0, 0), 0,
				ParameterPreferences.getDouble(getParameterString(0, 0) + ".min", -0.1),
				ParameterPreferences.getDouble(getParameterString(0, 0) + ".max", 0.1)));
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
            ParameterPreferences.getPref(getParameterString(0, index) + ".min", "-0.1"),
            ParameterPreferences.getPref(getParameterString(0, index) + ".max", "0.1"), false));
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
      double value = datafile.getXDataForCalibration(i);
      for (int j = 0; j < numberCoeff; j++) {
        value += difc[j] * MoreMath.pow(value, j);
      }
      datafile.setCalibratedXDataOnly(i, value);
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

	public boolean freeAllBasicParameters() {
		for (int i = 0; i < numberofelementPL(0); i++)
			getCoeffP(i).setRefinableCheckBound();
		return true;
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

      setTitle("Instrument disalignment calibration");
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
