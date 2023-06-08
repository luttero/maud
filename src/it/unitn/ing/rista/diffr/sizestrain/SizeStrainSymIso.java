/*
 * @(#)SizeStrainSymIso.java created 09/10/1998 Mesiano
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

package it.unitn.ing.rista.diffr.sizestrain;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

import java.awt.*;
import javax.swing.*;

/**
 *  The SizeStrainSymIso is a class to manage the isotropic crystallite sizes
 *  and microstrains
 *
 *
 * @version $Revision: 1.10 $, $Date: 2006/12/04 14:30:15 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SizeStrainSymIso extends SizeStrainSymModel {

  protected static String[] diclistc = {"_riet_par_cryst_size", "_riet_par_rs_microstrain"};
  protected static String[] diclistcrm = {"Crystallite size (Dv, angstrom)",
    "r.m.s. microstrain"};

  protected static String[] classlistc = {};

  protected static String[] classlistcs = {};

  public SizeStrainSymIso(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Isotropic";
    IDlabel = "Isotropic";
    description = "select this to have an isotropic model";
  }

  public SizeStrainSymIso(XRDcat aobj) {
    this(aobj, "Size-Strain isotropic model");
  }

  public SizeStrainSymIso() {
    identifier = "Isotropic";
    IDlabel = "Isotropic";
    description = "select this to have an isotropic model";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 2;
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
    parameterField[0] = new Parameter(this, getParameterString(0), ParameterPreferences.getDouble(getParameterString(0) + ".value", 1000),
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 50),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 5000));
    parameterField[0].setPositiveOnly();
    parameterField[0].setMinimumSignificantValue(5);
    parameterField[1] = new Parameter(this, getParameterString(1), ParameterPreferences.getDouble(getParameterString(1) + ".value", 0.0006),
            ParameterPreferences.getDouble(getParameterString(1) + ".min", 0.0),
            ParameterPreferences.getDouble(getParameterString(1) + ".max", 0.005));
    parameterField[1].setPositiveOnly();
    parameterField[1].setMinimumSignificantValue(0.0001);
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
     if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
       return;
     super.updateParametertoDoubleBuffering(false);
     parameterField[0].setPositiveOnly();
      parameterField[0].setMinimumSignificantValue(5);
     parameterField[1].setPositiveOnly();
      parameterField[1].setMinimumSignificantValue(0.0001);
   }

  public void correctCrystalliteAndMicrostrain() {
// due to an error on one of the first Maud versions, this method is called only if a old analysis is loaded
    parameterField[0].setValue(parameterField[0].getValueD() * 2.0);
    parameterField[1].setValue(parameterField[1].getValueD() / 2.0);
  }

  public Parameter getCrystalliteSize() {
    return parameterField[0];
  }

  public Parameter getMicrostrain() {
    return parameterField[1];
  }

  double cryststrain[] = new double[2];

  public double[] getCrystalliteMicrostrain(double d_space, int h, int k, int l, double[] texture_angles) {

    cryststrain[0] = getParameterValue(0);
    cryststrain[1] = getParameterValue(1);

    return cryststrain;
  }

  public double getMeanCrystallite() {
    return getParameterValue(0);
  }

  public double getMeanMicrostrain() {
    return getParameterValue(1);
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JSizeStrainIsoOptionsD(parent, this);
    return adialog;
  }

  public class JSizeStrainIsoOptionsD extends JOptionsDialog {

    JTextField microstrain;
    JTextField crystallite;

    public JSizeStrainIsoOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new GridLayout(1, 2, 1, 1));

      JPanel jp2 = new JPanel();
      jp2.setLayout(new GridLayout(2, 1, 4, 4));
      principalPanel.add(jp2);
      jp2.add(new JLabel("Crystallite size (A):"));
      crystallite = new JTextField(Constants.FLOAT_FIELD);
      crystallite.setText("0");
      jp2.add(crystallite);

      jp2 = new JPanel();
      jp2.setLayout(new GridLayout(2, 1, 4, 4));
      principalPanel.add(jp2);
      jp2.add(new JLabel("R.m.s. microstrain:"));
      microstrain = new JTextField(Constants.FLOAT_FIELD);
      microstrain.setText("0");
      jp2.add(microstrain);

      initParameters();

      setTitle("Isotropic size-strain");
      pack();
    }

    public void initParameters() {
      crystallite.setText(getCrystalliteSize().getValue());
      addComponenttolist(crystallite, getCrystalliteSize());
      microstrain.setText(getMicrostrain().getValue());
      addComponenttolist(microstrain, getMicrostrain());
    }

    public void retrieveParameters() {
      getCrystalliteSize().setValue(crystallite.getText());
      getMicrostrain().setValue(microstrain.getText());
    }

  }
}
