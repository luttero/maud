/*
 * @(#)TriangularInterpolation.java created 13/08/1998 Pergine Vals.
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

package it.unitn.ing.rista.diffr.rta;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;

import java.awt.*;
import javax.swing.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.io.*;

import java.awt.event.*;

/**
 *  The TriangularInterpolation is a class
 *
 *
 * @version $Revision: 1.5 $, $Date: 2005/05/06 18:07:27 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class TriangularInterpolation extends Interpolation {
  public static String[] diclistc = {"_rita_interpol_dist_control",
                                     "_rita_interpol_polar_angle_max",
                                     "_rita_interpol_minimum_angle",
                                     "_rita_interpol_degree"};
  public static String[] diclistcrm = {"_rita_interpol_dist_control",
                                     "_rita_interpol_polar_angle_max",
                                     "_rita_interpol_minimum_angle",
                                     "_rita_interpol_degree"};
  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  WIMVTexture wimv = null;

  public TriangularInterpolation(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Triangular";
    IDlabel = "Triangular";
    description = "select this to apply Triangular interpolation";

    wimv = (WIMVTexture) getParent();
  }

  public TriangularInterpolation(XRDcat aobj) {
    this(aobj, "Triangular interpolation");
  }

  public TriangularInterpolation() {
    identifier = "Triangular";
    IDlabel = "Triangular";
    description = "select this to apply Triangular interpolation";
  }

  public void initConstant() {
    Nstring = 4;
    Nstringloop = 0;
    Nparameter = 0;
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
    stringField[0] = new String("20.0");
    stringField[1] = new String("90.0");
    stringField[2] = new String("20.0");
    stringField[3] = new String("1");
  }

  public double getDistControlD() {
    return Double.valueOf(getDistControl()).doubleValue();
  }

  public String getDistControl() {
    return stringField[0];
  }

  public void setDistControl(String value) {
    stringField[0] = new String(value);
  }

  public void setDistControl(double value) {
    setDistControl(Double.toString(value));
  }

  public double getMaxPolarAngleD() {
    return Double.valueOf(getMaxPolarAngle()).doubleValue();
  }

  public String getMaxPolarAngle() {
    return stringField[1];
  }

  public void setMaxPolarAngle(String value) {
    stringField[1] = new String(value);
  }

  public void setMaxPolarAngle(double value) {
    setMaxPolarAngle(Double.toString(value));
  }

  public double getMinimumAngleD() {
    return Double.valueOf(getMinimumAngle()).doubleValue();
  }

  public String getMinimumAngle() {
    return stringField[2];
  }

  public void setMinimumAngle(String value) {
    stringField[2] = new String(value);
  }

  public void setMinimumAngle(double value) {
    setMinimumAngle(Double.toString(value));
  }

  public int getInterpolationDegree() {
    return Integer.valueOf(stringField[3]).intValue();
  }

  public void setInterpolationDegree(int value) {
    stringField[3] = new String(Integer.toString(value));
  }

  public void setInterpolationDegree(String value) {
    stringField[3] = new String(Integer.toString(Integer.valueOf(value).intValue()));
  }

  public int getMaxDegree() {
    return 2;
  }

  public int getMinDegree() {
    return 0;
  }

  public double getResolution() {
    return wimv.getResolutionD();
  }

  public int getPoleFigureNumber() {
    return wimv.getPoleFigureNumber();
  }

  public int getPointNumber(int pole) {
    return wimv.getPointNumber(pole);
  }

  public double[] getTextureAngles(int pole, int point) {
    return wimv.getTextureAngles(pole, point);
  }

  public double getPoleIntensity(int pole, int point) {
    return wimv.getPoleIntensity(pole, point);
  }

  public double getWeight(int pole, int point) {
    return wimv.getWeight(pole, point);
  }

  public int getH(int pole) {
    return wimv.getH(pole);
  }

  public int getK(int pole) {
    return wimv.getK(pole);
  }

  public int getL(int pole) {
    return wimv.getL(pole);
  }

  public int getIzoveri(int pole) {
    return wimv.getIzoveri(pole);
  }

  public int getH(int pole, int partial) {
    return wimv.getH(pole, partial);
  }

  public int getK(int pole, int partial) {
    return wimv.getK(pole, partial);
  }

  public int getL(int pole, int partial) {
    return wimv.getL(pole, partial);
  }

  public double getWeightSingle(int pole, int partial) {
    return wimv.getWeightSingle(pole, partial);
  }

  public double[][] computeInterpolation() {

    Agtorgl interpolator = new Agtorgl(this);
    return interpolator.getInterpolation();

  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JTInterpolationOptionsD(parent, this);
    return adialog;
  }

  class JTInterpolationOptionsD extends JOptionsDialog {

    JTextField maxPolarTF;
    JTextField minAngleTF;
    JTextField distCtrlTF;
    JSlider degreeJS;

    public JTInterpolationOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));
      JPanel up = new JPanel();
      up.setLayout(new GridLayout(0, 1, 6, 6));
      principalPanel.add(BorderLayout.CENTER, up);
      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      up.add(jPanel8);
      jPanel8.add(new JLabel("Maximum polar angle: "));
      maxPolarTF = new JTextField(Constants.FLOAT_FIELD);
      maxPolarTF.setToolTipText("Specify the maximum polar angle for interpolation");
      jPanel8.add(maxPolarTF);
      JPanel jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      up.add(jPanel9);
      jPanel9.add(new JLabel("Minimum triangle angle: "));
      minAngleTF = new JTextField(Constants.FLOAT_FIELD);
      minAngleTF.setToolTipText("Specify the minimum angle for the triangulation");
      jPanel9.add(minAngleTF);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      up.add(jPanel9);
      jPanel9.add(new JLabel("Distance control value: "));
      distCtrlTF = new JTextField(Constants.FLOAT_FIELD);
      distCtrlTF.setToolTipText("Maximum distance from one measured point");
      jPanel9.add(distCtrlTF);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      up.add(jPanel9);
      JPanel jPanel10 = new JPanel();
      jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 6, 6));
      principalPanel.add(BorderLayout.SOUTH, jPanel10);
      jPanel10.add(new JLabel("Interpolation degree: "));
      degreeJS = new JSlider();
      degreeJS.setToolTipText("Set the degree of the interpolation polynomial function");
      jPanel10.add(degreeJS);

      setTitle("Triangular interpolation options");
      initParameters();
      pack();

    }

    public void initParameters() {
      maxPolarTF.setText(getMaxPolarAngle());
      minAngleTF.setText(getMinimumAngle());
      distCtrlTF.setText(getDistControl());
      degreeJS.setMaximum(getMaxDegree());
      degreeJS.setMinimum(getMinDegree());
      degreeJS.setValue(getInterpolationDegree());
      degreeJS.setPaintTicks(true);
      degreeJS.setMajorTickSpacing(2);
      degreeJS.setMinorTickSpacing(1);

      degreeJS.setPaintLabels(true);
      degreeJS.setSnapToTicks(true);

      degreeJS.setLabelTable(degreeJS.createStandardLabels(1));
    }

    public void retrieveParameters() {
      setMaxPolarAngle(maxPolarTF.getText());
      setMinimumAngle(minAngleTF.getText());
      setDistControl(distCtrlTF.getText());
      setInterpolationDegree(degreeJS.getValue());
    }

  }

}

