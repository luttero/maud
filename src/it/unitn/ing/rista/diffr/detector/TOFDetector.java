/*
 * @(#)TOFDetector.java created 07/01/1999 Pergine Vals.
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

package it.unitn.ing.rista.diffr.detector;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;

/**
 *  The TOFDetector is a class
 *
 *
 * @version $Revision: 1.7 $, $Date: 2006/11/10 09:33:01 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class TOFDetector extends Detector {

  public static String modelID = "TOF";

  public static String[] diclistc = {"_instrument_counter_bank_ID",
                                     "_diffrn_radiation_detector_theta",
                                     "_diffrn_radiation_detector_eta",
                                     "_diffrn_radiation_detector_efficiency",
                                     "_pd_instr_dist_spec/detc"};
  public static String[] diclistcrm = {"_instrument_counter_bank_ID",
                                     "2theta position (deg)",
                                     "eta angular position (deg)",
                                     "efficiency value",
                                     "sample detector distance (m)"};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  public TOFDetector(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = modelID;
    IDlabel = modelID;
    description = modelID + " detector";
  }

  public TOFDetector(XRDcat aobj) {
    this(aobj, modelID);
  }

  public TOFDetector() {
    identifier = modelID;
    IDlabel = modelID;
    description = modelID + " detector";
  }

  public void initConstant() {
    Nstring = 1;
    Nstringloop = 0;
    Nparameter = 4;
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
    setEta(0.0);
    setIntensity(1.0);
    setDetectorDistance(1.0);
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
          if (i < 2) {
            notifyParameterChanged(source, Constants.SAMPLE_ORIENTATION_CHANGED, -1);
            return;
          }
          if (i == 2) {
            notifyParameterChanged(source, Constants.BEAM_INTENSITY_CHANGED, -1);
            return;
          }
          if (i == 3) {
            notifyParameterChanged(source, Constants.ANGULAR_CALIBRATION, -1);
            return;
          }
        }
      }
      super.notifyParameterChanged(source);
    }

  }

  double theta, eta, distance, efficiency;

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);
    theta = getTheta().getValueD();
    eta = getEta().getValueD();
    distance = getDetectorDistance().getValueD();
    if (distance == 0)
      distance = 1.0;
    distance = distance * 1000.0;
    efficiency = getIntensity().getValueD();
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

  public Parameter getEta() {
    return parameterField[1];
  }

  public void setEta(String value) {
    getEta().setValue(value);
  }

  public void setEta(double value) {
    getEta().setValue(value);
  }

  public Parameter getIntensity() {
    return parameterField[2];
  }

  public void setIntensity(String value) {
    getIntensity().setValue(value);
  }

  public void setIntensity(double value) {
    getIntensity().setValue(value);
  }

  public double getIntensityCalibration(DiffrDataFile adatafile, Sample asample, double position,
                                        boolean dspacingbase, boolean energyDispersive) {
    return efficiency;
  }

  public Parameter getDetectorDistance() {
    return parameterField[3];
  }

  public void setDetectorDistance(String value) {
    getDetectorDistance().setValue(value);
  }

  public void setDetectorDistance(double value) {
    getDetectorDistance().setValue(value);
  }

  public double getThetaDetector() {
    return theta;
  }

  public double getEtaDetector() {
    return eta;
  }

  public double getTthetaValue(DiffrDataFile datafile, double twotheta) {
    return theta;
  }

  public double getEtaValue(DiffrDataFile datafile) {
    return eta;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new TOFDetector.JTOFDetectorOptionsD(parent, this);
    return adialog;
  }

  public class JTOFDetectorOptionsD extends JOptionsDialog {

    JTextField thetaTF;
    JTextField etaTF;
    JTextField intensityTF;
    JTextField distanceTF;

    public JTOFDetectorOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new GridLayout(0, 1, 1, 1));

      JPanel jp2 = new JPanel();
      jp2.setLayout(new GridLayout(1, 2, 4, 4));
      principalPanel.add(jp2);
      jp2.add(new JLabel("TOF theta angle (degrees):"));
      thetaTF = new JTextField(Constants.FLOAT_FIELD);
      thetaTF.setText("90");
      jp2.add(thetaTF);

      jp2 = new JPanel();
      jp2.setLayout(new GridLayout(1, 2, 4, 4));
      principalPanel.add(jp2);
      jp2.add(new JLabel("Eta angle (degrees):"));
      etaTF = new JTextField(Constants.FLOAT_FIELD);
      etaTF.setText("0");
      jp2.add(etaTF);

      jp2 = new JPanel();
      jp2.setLayout(new GridLayout(1, 2, 4, 4));
      principalPanel.add(jp2);
      jp2.add(new JLabel("Detector efficiency:"));
      intensityTF = new JTextField(Constants.FLOAT_FIELD);
      intensityTF.setText("1");
      jp2.add(intensityTF);

      jp2 = new JPanel();
      jp2.setLayout(new GridLayout(1, 2, 4, 4));
      principalPanel.add(jp2);
      jp2.add(new JLabel("Sample-detector distance (m):"));
      distanceTF = new JTextField(Constants.FLOAT_FIELD);
      distanceTF.setText("1");
      jp2.add(distanceTF);

      initParameters();

      setTitle("TOF detector characteristics");
      pack();
    }

    public void initParameters() {
      thetaTF.setText(getTheta().getValue());
      addComponenttolist(thetaTF, getTheta());
      etaTF.setText(getEta().getValue());
      addComponenttolist(etaTF, getEta());
      intensityTF.setText(getIntensity().getValue());
      addComponenttolist(intensityTF, getIntensity());
      distanceTF.setText(getDetectorDistance().getValue());
      addComponenttolist(distanceTF, getDetectorDistance());
    }

    public void retrieveParameters() {
      getTheta().setValue(thetaTF.getText());
      getEta().setValue(etaTF.getText());
      getIntensity().setValue(intensityTF.getText());
      getDetectorDistance().setValue(distanceTF.getText());
    }

  }

}
