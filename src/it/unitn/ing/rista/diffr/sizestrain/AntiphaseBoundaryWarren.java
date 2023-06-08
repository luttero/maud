/*
 * @(#)AntiphaseBoundaryWarren.java created 11/01/1999 Pergine Vals
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

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.*;

import javax.swing.*;


/**
 *  The AntiphaseBoundaryWarren is a class
 *
 *
 * @version $Revision: 1.6 $, $Date: 2005/05/06 18:07:27 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class AntiphaseBoundaryWarren extends AntiphaseBoundary {

  public static String[] diclistc = {
    "_riet_antiphase_boundary_probability"
  };
  public static String[] diclistcrm = {
    "probability"
  };

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  public AntiphaseBoundaryWarren(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Warren";
    IDlabel = "Warren";
    description = "select this to apply the Warren model for antiphase boundary";
  }

  public AntiphaseBoundaryWarren(XRDcat aobj) {
    this(aobj, "antiphase boundary x");
  }

  public AntiphaseBoundaryWarren() {
    identifier = "Warren";
    IDlabel = "Warren";
    description = "select this to apply the Warren model for antiphase boundary";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 1;
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
    parameterField[0] = new Parameter(this, getParameterString(0), 0,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.0000001),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 0.1));
    parameterField[0].setPositiveOnly();
    parameterField[0].setMinimumSignificantValue(0.0001);
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);

    parameterField[0].setPositiveOnly();
    parameterField[0].setMinimumSignificantValue(0.0001);
  }

  public Parameter getAntiphaseBoundary() {
    return parameterField[0];
  }

  public double getIntegralBeta(int cpType, int h, int k, int l) {
    return 0.0;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JAntiphaseBoundaryWarrenOptionsD(parent, this);
    return adialog;
  }

  public class JAntiphaseBoundaryWarrenOptionsD extends JOptionsDialog {

    JTextField antiphase = null;

    public JAntiphaseBoundaryWarrenOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout());

      principalPanel.add(new JLabel("Antiphase Boundary probability: "));
      antiphase = new JTextField(Constants.FLOAT_FIELD);
      antiphase.setText("0");
      principalPanel.add(antiphase);

      setTitle("Antiphase boundary");
      initParameters();
      pack();
    }

    public void initParameters() {
      antiphase.setText(getAntiphaseBoundary().getValue());
      addComponenttolist(antiphase, getAntiphaseBoundary());
    }

    public void retrieveParameters() {
      getAntiphaseBoundary().setValue(antiphase.getText());
    }
  }

}
