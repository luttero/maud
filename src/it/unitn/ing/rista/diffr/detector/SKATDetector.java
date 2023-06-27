/*
 * @(#)SKATDetector.java created 27/04/1999 Firenze.
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.detector;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

import java.awt.*;
import javax.swing.*;

/**
 *  The SKATDetector is a class
 *
 *
 * @version $Revision: 1.5 $, $Date: 2005/05/06 18:07:26 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class SKATDetector extends Detector {

  public static String[] diclistc = {"_diffrn_radiation_detector_theta",
                                     "_diffrn_radiation_detector_efficiency"};
  public static String[] diclistcrm = {"2theta position",
                                     "efficiency value"};


  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  public SKATDetector(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "SKAT detector";
    IDlabel = "SKAT detector";
    description = "SKAT detector";
  }

  public SKATDetector(XRDcat aobj) {
    this(aobj, "SKAT detector");
  }

  public SKATDetector() {
    identifier = "SKAT detector";
    IDlabel = "SKAT detector";
    description = "SKAT detector";
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
    setTheta(90.0);
    setIntensity(1.0);
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
          if (i == 0) {
            notifyParameterChanged(source, Constants.SAMPLE_ORIENTATION_CHANGED, -1);
            return;
          } else {
            notifyParameterChanged(source, Constants.BEAM_INTENSITY_CHANGED, -1);
            return;
          }
        }
      }
      super.notifyParameterChanged(source);
    }

  }

  public Parameter getTheta() {
    return parameterField[0];
  }

  public void setTheta(String value) {
    getTheta().setValue(value);
  }

  public void setTheta(double value) {
    getTheta().setValue(value);
  }

  public Parameter getIntensity() {
    return parameterField[1];
  }

  public void setIntensity(String value) {
    getIntensity().setValue(value);
  }

  public void setIntensity(double value) {
    getIntensity().setValue(value);
  }

  public double getThetaDetector() {
    return (double) getTheta().getValueD();
  }

  public double getIntensityCalibration(DiffrDataFile adatafile, Sample asample, double position,
                                        boolean dspacingbase, boolean energyDispersive) {
    return getIntensity().getValueD();
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JSKATDetectorOptionsD(parent, this);
    return adialog;
  }

  public class JSKATDetectorOptionsD extends JOptionsDialog {

    JTextField intensity;
    JTextField theta;

    public JSKATDetectorOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new GridLayout(2, 1, 1, 1));

      JPanel jp2 = new JPanel();
      jp2.setLayout(new GridLayout(1, 2, 4, 4));
      principalPanel.add(jp2);
      jp2.add(new JLabel("Detector position (degrees, 0-90):"));
      theta = new JTextField(Constants.FLOAT_FIELD);
      theta.setText("90");
      jp2.add(theta);

      jp2 = new JPanel();
      jp2.setLayout(new GridLayout(1, 2, 4, 4));
      principalPanel.add(jp2);
      jp2.add(new JLabel("Detector efficiency:"));
      intensity = new JTextField(Constants.FLOAT_FIELD);
      intensity.setText("1");
      jp2.add(intensity);

      initParameters();

      setTitle("SKAT detector characteristics");
      pack();
    }

    public void initParameters() {
      theta.setText(getTheta().getValue());
      addComponenttolist(theta, getTheta());
      intensity.setText(getIntensity().getValue());
      addComponenttolist(intensity, getIntensity());
    }

    public void retrieveParameters() {
      getTheta().setValue(theta.getText());
      getIntensity().setValue(intensity.getText());
    }

  }

}
