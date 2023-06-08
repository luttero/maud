/*
 * @(#)PlanarDefectsWarren.java created 11/01/1999 Pergine Vals
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

import java.lang.*;
import java.awt.*;
import java.util.Vector;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.*;

import javax.swing.*;


/**
 * The PlanarDefectsWarren is a class
 * <p/>
 * 
 * @author Luca Lutterotti
 * @version $Revision: 1.7 $, $Date: 2006/01/19 14:45:58 $
 * @since JDK1.1
 */


public class PlanarDefectsWarren extends PlanarDefects {

  public static String[] diclistc = {
    "_riet_deformation_fault_intrinsic",
    "_riet_deformation_fault_extrinsic",
    "_riet_twin_fault_probability"
  };
  public static String[] diclistcrm = {
    "deformation fault probability (intrinsic)",
    "deformation fault probability (extrinsic)",
    "twin fault probability"
  };

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  int cpType = -1;
  double constant1 = 0.0;
  double constant2 = 0.0;
  double faultdiff = 0.0;
  double faultasy = 0.0;

  public PlanarDefectsWarren(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Warren";
    IDlabel = "Warren";
    description = "select this to apply the Warren model for planar defects";
  }

  public PlanarDefectsWarren(XRDcat aobj) {
    this(aobj, "Warren planar defect");
  }

  public PlanarDefectsWarren() {
    identifier = "Warren";
    IDlabel = "Warren";
    description = "select this to apply the Warren model for planar defects";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 3;
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
    parameterField[0] = new Parameter(this, getParameterString(0), 0.0,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.0),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 0.01));
    parameterField[0].setPositiveOnly();
    parameterField[0].setMinimumSignificantValue(0.0001);
    parameterField[1] = new Parameter(this, getParameterString(1), 0.0,
            ParameterPreferences.getDouble(getParameterString(1) + ".min", 0.0),
            ParameterPreferences.getDouble(getParameterString(1) + ".max", 0.01));
    parameterField[1].setPositiveOnly();
    parameterField[1].setMinimumSignificantValue(0.0001);
    parameterField[2] = new Parameter(this, getParameterString(2), 0.0,
            ParameterPreferences.getDouble(getParameterString(2) + ".min", 0.0),
            ParameterPreferences.getDouble(getParameterString(2) + ".max", 0.01));
    parameterField[2].setPositiveOnly();
    parameterField[2].setMinimumSignificantValue(0.0001);
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);
    parameterField[0].setPositiveOnly();
    parameterField[1].setPositiveOnly();
    parameterField[2].setPositiveOnly();
    parameterField[0].setMinimumSignificantValue(0.0001);
    parameterField[1].setMinimumSignificantValue(0.0001);
    parameterField[2].setMinimumSignificantValue(0.0001);
  }

  public Parameter getIntrinsicFault() {
    return parameterField[0];
  }

  public Parameter getExtrinsicFault() {
    return parameterField[1];
  }

  public Parameter getTwinFault() {
    return parameterField[2];
  }

  public void freeAllMicroParameters(int cpType) {
    int i, j;

    switch (cpType) {
      case 0: // hexagonal
        getIntrinsicFault().setRefinableCheckBound();
        getTwinFault().setRefinableCheckBound();
        break;
      case 1: // fcc
        for (i = 0; i < Nparameter; i++)
          parameterField[i].setRefinableCheckBound();
        for (i = 0; i < Nparameterloop; i++) {
          for (j = 0; j < numberofelementPL(i); j++)
            ((Parameter) parameterloopField[i].elementAt(j)).setRefinableCheckBound();
        }
        break;
      case 2: // bcc shift still not implemented
        getIntrinsicFault().setRefinableCheckBound();
        getTwinFault().setRefinableCheckBound();
        break;
      default:
        {
        }
    }
  }

  public void checkhklListForPlanarDefects(Vector reflectionv, int cpType) {
  }

  public void prepareComputation(int cpType) {
    this.cpType = cpType;

    double alphaprime = Math.abs(getIntrinsicFault().getValueD());
    double alphasecond = Math.abs(getExtrinsicFault().getValueD());
    double beta = Math.abs(getTwinFault().getValueD());
    faultdiff = 0.0;
    faultasy = 0.0;

    switch (cpType) {
      case 0: // hexagonal
        constant1 = ((Phase) getParent()).getFullCellValue(2);
        constant1 *= constant1;
        constant2 = constant1 / (3.0 * alphaprime + beta);
        constant1 /= 3.0 * (alphaprime + beta);
        break;
      case 1: // fcc
        faultasy = 4.5 * alphasecond + beta;
        faultdiff = alphaprime - alphasecond;
        constant1 = ((Phase) getParent()).getFullCellValue(0);
        constant2 = 1.5 * (alphaprime + alphasecond) + beta;
        break;
      case 2: // bcc shift still not implemented
        faultasy = -(4.5 * alphasecond + beta);
        faultdiff = alphaprime - alphasecond;
        constant1 = ((Phase) getParent()).getFullCellValue(0);
        constant2 = 1.5 * (alphaprime + alphasecond) + beta;
        break;
      default:
        {
        }
    }
  }

  public double getPlanarDefectDisplacement(Reflection refl) {
    // return delta(d)/d
    if (cpType != 1)
      return 0.0;
    return -faultdiff;
  }

  public double getPlanarDefectAsymmetryConstant1(Reflection reflex) {
    if (cpType != 1)
      return 0.0f;
    double c2 = 2.0 * Constants.PI;
    return c2;
  }

  public double getPlanarDefectAsymmetryConstant2(Reflection refl) {
    if (cpType != 1)
      return 0.0f;
    double asy = faultasy * refl.Lsumdivideabs;
    return asy;
  }

  public double getCrystalliteEquivalent(Reflection refl) {

    int h = refl.getH();
    int k = refl.getK();
    int l = refl.getL();
    double dspace = refl.d_space;
    double cryst = 0.0;

    switch (cpType) {
      case 0: // hexagonal
        if (l == 0)
          return 0.0;
        if (MoreMath.is3Neven(h - k) != 0) {
          if (MoreMath.odd(l))
            cryst = constant2 / (dspace * Math.abs(l));
          else
            cryst = constant1 / (dspace * Math.abs(l));
        }
        break;
      case 1: // fcc
      case 2: // bcc
        double denominator = refl.Labssum * constant2;
        double numerator = refl.hzero * refl.multiplicity * constant1;
        if (denominator != 0.0)
          cryst = numerator / denominator;
        else
          cryst = 0.0;

//				System.out.println(Integer.toXRDcatString(h) + " " + Integer.toXRDcatString(k) + " " + Integer.toXRDcatString(l) + " " +
//													Double.toXRDcatString(cryst) + " " + Double.toXRDcatString(refl.Labssum / (refl.hzero * refl.multiplicity)));
        break;
      default:
        {
        }
    }
    return cryst;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JPlanarDefectsWarrenOptionsD(parent, this);
    return adialog;
  }

  public class JPlanarDefectsWarrenOptionsD extends JOptionsDialog {

    JTextField intrinsic = null;
    JTextField extrinsic = null;
    JTextField twinfault = null;

    public JPlanarDefectsWarrenOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new GridLayout(3, 2, 3, 3));

      principalPanel.add(new JLabel("Intrinsic deformation fault probability: "));
      intrinsic = new JTextField(Constants.FLOAT_FIELD);
      intrinsic.setText("0");
      principalPanel.add(intrinsic);

      principalPanel.add(new JLabel("Extrinsic (or growth) deformation fault probability: "));
      extrinsic = new JTextField(Constants.FLOAT_FIELD);
      extrinsic.setText("0");
      principalPanel.add(extrinsic);

      principalPanel.add(new JLabel("Twin fault probability: "));
      twinfault = new JTextField(Constants.FLOAT_FIELD);
      twinfault.setText("0");
      principalPanel.add(twinfault);

      setTitle("Planar defects");
      initParameters();
      pack();
    }

    public void initParameters() {
      intrinsic.setText(getIntrinsicFault().getValue());
      addComponenttolist(intrinsic, getIntrinsicFault());
      extrinsic.setText(getExtrinsicFault().getValue());
      addComponenttolist(extrinsic, getExtrinsicFault());
      twinfault.setText(getTwinFault().getValue());
      addComponenttolist(twinfault, getTwinFault());
    }

    public void retrieveParameters() {
      getIntrinsicFault().setValue(intrinsic.getText());
      getExtrinsicFault().setValue(extrinsic.getText());
      getTwinFault().setValue(twinfault.getText());
    }
  }

}
