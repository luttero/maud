/*
 * @(#)SizeStrainSymDefault.java created 04/10/1998 Verona-Trento
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

import java.awt.*;

import it.unitn.ing.rista.util.*;

import javax.swing.*;
import javax.swing.border.*;

/**
 *  The SizeStrainSymDefault is a class
 *
 *
 * @version $Revision: 1.6 $, $Date: 2005/05/06 18:07:28 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SizeStrainSymDefault extends SizeStrainSymModel {

  protected static String[] diclistc = {
    "_riet_par_anisocryst_size_11", "_riet_par_anisocryst_size_12", "_riet_par_anisocryst_size_13",
    "_riet_par_anisocryst_size_22", "_riet_par_anisocryst_size_23", "_riet_par_anisocryst_size_33",
    "_riet_par_aniso_microstrain_11", "_riet_par_aniso_microstrain_12", "_riet_par_aniso_microstrain_13",
    "_riet_par_aniso_microstrain_22", "_riet_par_aniso_microstrain_23", "_riet_par_aniso_microstrain_33"
  };
  protected static String[] diclistcrm = {
    "size_11", "size_12", "size_13",
    "size_22", "size_23", "size_33",
    "microstrain_11", "microstrain_12", "microstrain_13",
    "microstrain_22", "microstrain_23", "microstrain_33"
  };

  protected static String[] classlistc = {};

  protected static String[] classlistcs = {};

  public SizeStrainSymDefault(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Anisotropic no rules";
    IDlabel = "Anisotropic no rules";
    description = "select this to have anisotropic model without rules, use the equal to property instead";
  }

  public SizeStrainSymDefault(XRDcat aobj) {
    this(aobj, "Size-Strain symmetry default model");
  }

  public SizeStrainSymDefault() {
    identifier = "Anisotropic no rules";
    IDlabel = "Anisotropic no rules";
    description = "select this to have anisotropic model without rules, use the equal to property instead";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 12;
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
    for (int i = 0; i < 6; i++) {
      parameterField[i] = new Parameter(this, getParameterString(i), 500,
              ParameterPreferences.getDouble(getParameterString(i) + ".min", 5),
              ParameterPreferences.getDouble(getParameterString(i) + ".max", 10000));
      parameterField[i].setMinimumSignificantValue(5);

      parameterField[i+6] = new Parameter(this, getParameterString(i+6), 0.0008,
              ParameterPreferences.getDouble(getParameterString(i+6) + ".min", 0.0),
              ParameterPreferences.getDouble(getParameterString(i+6) + ".max", 0.01));
      parameterField[i+6].setMinimumSignificantValue(0.0001);
    }
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);
    for (int i = 0; i < 6; i++) {
      parameterField[i].setMinimumSignificantValue(5);
      parameterField[i+6].setMinimumSignificantValue(0.0001);
    }
  }

  public Parameter getCrystalliteSize(int index) {
    if (index < 0 || index >= 6)
      return parameterField[0];

    return parameterField[index];
  }

  public Parameter getMicrostrain(int index) {
    if (index < 0 || index >= 6)
      return parameterField[6];

    return parameterField[6 + index];
  }

  double cryststrain[] = new double[2];

  public double[] getCrystalliteMicrostrain(double d_space, int h, int k, int l, double[] texture_angles) {

    int h2 = h * h;
    int k2 = k * k;
    int l2 = l * l;
    int hk = h * k;
    int hl = h * l;
    int kl = l * k;
    hk = 2 * Math.abs(hk);
    hl = 2 * Math.abs(hl);
    kl = 2 * Math.abs(kl);
    double normhklhydro = h2 + k2 + l2;
    double normhkldev = hk + hl + kl;
    if (normhkldev == 0.0)
      normhkldev = 1.0;
    cryststrain[0] = Math.sqrt((h2 * Math.pow(getParameterValue(0), 2.)
            + k2 * Math.pow(getParameterValue(3), 2.)
            + l2 * Math.pow(getParameterValue(5), 2.)) / normhklhydro +
            (hk * Math.pow(getParameterValue(1), 2.)
            + hl * Math.pow(getParameterValue(2), 2.)
            + kl * Math.pow(getParameterValue(4), 2.)) / normhkldev);
    cryststrain[1] = Math.sqrt((h2 * Math.pow(getParameterValue(6), 2.)
            + k2 * Math.pow(getParameterValue(9), 2.)
            + l2 * Math.pow(getParameterValue(11), 2.)) / normhklhydro +
            (hk * Math.pow(getParameterValue(7), 2.)
            + hl * Math.pow(getParameterValue(8), 2.)
            + kl * Math.pow(getParameterValue(10), 2.)) / normhkldev);

    return cryststrain;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JSizeStrainDefOptionsD(parent, this);
    return adialog;
  }

  public class JSizeStrainDefOptionsD extends JOptionsDialog {

    JTextField m[] = new JTextField[6];
    JTextField e[] = new JTextField[6];

    public JSizeStrainDefOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel jp3 = new JPanel();
      jp3.setLayout(new BorderLayout(2, 2));
      jp3.setBorder(new TitledBorder(
              new BevelBorder(BevelBorder.LOWERED), "Crystallite size"));
      principalPanel.add(BorderLayout.WEST, jp3);
      JPanel jp2 = new JPanel();
      jp2.setLayout(new GridLayout(6, 1, 1, 1));
      jp3.add(BorderLayout.WEST, jp2);
      for (int i = 0; i < 6; i++) {
        m[i] = new JTextField(Constants.FLOAT_FIELD);
        m[i].setText("0");
        jp2.add(m[i]);
      }
      jp2 = new JPanel();
      jp2.setLayout(new GridLayout(6, 1, 1, 1));
      jp3.add(BorderLayout.EAST, jp2);
      String labels[] = {"11", "12", "13", "22", "23", "33"};
      for (int i = 0; i < 6; i++)
        jp2.add(new JLabel(labels[i]));

      jp3 = new JPanel();
      jp3.setLayout(new BorderLayout(2, 2));
      jp3.setBorder(new TitledBorder(
              new BevelBorder(BevelBorder.LOWERED), "R.m.s. microstrain"));
      principalPanel.add(BorderLayout.EAST, jp3);
      jp2 = new JPanel();
      jp2.setLayout(new GridLayout(6, 1, 1, 1));
      jp3.add(BorderLayout.WEST, jp2);
      for (int i = 0; i < 6; i++) {
        e[i] = new JTextField(Constants.FLOAT_FIELD);
        e[i].setText("0");
        jp2.add(e[i]);
      }
      jp2 = new JPanel();
      jp2.setLayout(new GridLayout(6, 1, 1, 1));
      jp3.add(BorderLayout.EAST, jp2);
      for (int i = 0; i < 6; i++)
        jp2.add(new JLabel(labels[i]));

      initParameters();

      setTitle("Anisotropic size-strain");
      pack();
    }

    public void initParameters() {
      for (int i = 0; i < 6; i++) {
        m[i].setText(getCrystalliteSize(i).getValue());
        addComponenttolist(m[i], getCrystalliteSize(i));
      }
      for (int i = 0; i < 6; i++) {
        e[i].setText(getMicrostrain(i).getValue());
        addComponenttolist(e[i], getMicrostrain(i));
      }
    }

    public void retrieveParameters() {
      for (int i = 0; i < 6; i++) {
        getCrystalliteSize(i).setValue(m[i].getText());
        getMicrostrain(i).setValue(e[i].getText());
      }
    }

    public Parameter getparameterfrom(Component hit) {

      for (int i = 0; i < 6; i++) {
        if (hit.equals(m[i]))
          return getCrystalliteSize(i);
        else if (hit.equals(e[i]))
          return getMicrostrain(i);
      }
      return super.getparameterfrom(hit);
    }

  }
}
