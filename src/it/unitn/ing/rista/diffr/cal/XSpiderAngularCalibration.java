/*
 * @(#)XSpiderAngularCalibration.java created Apr 3, 2010 Caen
 *
 * Copyright (c) 2010 Luca Lutterotti All Rights Reserved.
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
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.awt.JParameterListPane;

import java.awt.*;

/**
 * The XSpiderAngularCalibration is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Apr 3, 2010 9:24:14 AM $
 * @since JDK1.1
 */
public class XSpiderAngularCalibration extends AngularInclinedFlatImageCalibration/*AngularCalibration*/ {

/*  public static String[] diclistc = {"_image_original_center_x", "_image_original_center_y",
                                     "_pd_instr_dist_src/spec", "_pd_instr_dist_spec/detc",
                                     "_riet_par_spec_displac_z",
                                     "_inst_ang_calibration_center_x", "_inst_ang_calibration_center_y",
                                     "_inst_ang_calibration_detc_2theta", "_inst_ang_calibration_detc_phiDA",
                                     "_inst_ang_calibration_detc_omegaDN", "_inst_ang_calibration_detc_etaDA"};

  public static String[] diclistcrm = {"image original center x (arb)", "image original center y (arb)",
                                      "source sample distance (arb)", "sample detector (along beam) distance (arb)",
                                     "sample displacement z (arb)",
                                     "image center x (arb)", "image center y (arb)",
                                     "image 2theta (deg)", "image phiDA (deg)",
                                     "image rotation (deg)", "image etaDA (deg)"};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshCalibration = true;

  private double detectorDistance = 40.0;
  private double sampleDistance = 70.0;
  private double sampleDeltaZ = 0.0;
  private double centerX = 0.0;
  private double centerY = 0.0;

  private double detector2Theta = 30.0;
  private double detectorPhiDA = 0.0;
  private double detectorOmegaDN = 0.0;
  private double detectorEtaDA = 0.0;*/

  public XSpiderAngularCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
//    initXRD();
    identifier = "Disabled XSpider Image";
    IDlabel = "XSpider Image";
  }

  public XSpiderAngularCalibration(XRDcat aobj) {
    this(aobj, "XSpider Image calibration");
  }

  public XSpiderAngularCalibration() {
    identifier = "Disabled XSpider Image";
    IDlabel = "XSpider Image";
  }

/*  @Override
  public void initConstant() {
    Nstring = 4;
    Nstringloop = 0;
    Nparameter = 7;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

    @Override
  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
  }

  @Override
  public void initParameters() {
    super.initParameters();
    stringField[0] = "0.0";
    stringField[1] = "0.0";
    stringField[2] = "70";
    stringField[3] = "40";
    parameterField[0] = new Parameter(this, getParameterString(0), 0.0,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 10),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 1000));
    for (int i = 1; i < 3; i++)
      parameterField[i] = new Parameter(this, getParameterString(i), 0.0,
              ParameterPreferences.getDouble(getParameterString(i) + ".min", -10000.0),
              ParameterPreferences.getDouble(getParameterString(i) + ".max", 10000.0));
    parameterField[3] = new Parameter(this, getParameterString(3), 60.0,
            ParameterPreferences.getDouble(getParameterString(3) + ".min", 0),
            ParameterPreferences.getDouble(getParameterString(3) + ".max", 180));
    parameterField[4] = new Parameter(this, getParameterString(4), 20.0,
            ParameterPreferences.getDouble(getParameterString(4) + ".min", -180),
            ParameterPreferences.getDouble(getParameterString(4) + ".max", 180));
    parameterField[5] = new Parameter(this, getParameterString(5), 0.0,
            ParameterPreferences.getDouble(getParameterString(5) + ".min", -10),
            ParameterPreferences.getDouble(getParameterString(5) + ".max", 10));
    parameterField[6] = new Parameter(this, getParameterString(6), 0.0,
            ParameterPreferences.getDouble(getParameterString(6) + ".min", -180),
            ParameterPreferences.getDouble(getParameterString(6) + ".max", 180));
  }

  public void setSampleDeltaZ(double value) {
    parameterField[0].setValue(value);
  }

  public void setDetectorCenterX(double value) {
    parameterField[1].setValue(value);
  }

  public void setDetectorCenterY(double value) {
    parameterField[2].setValue(value);
  }

  public void setDetector2Theta(double value) {
    parameterField[3].setValue(value);
  }

  public void setDetectorPhiDA(double value) {
    parameterField[4].setValue(value);
  }

  public void setDetectorOmegaDN(double value) {
    parameterField[5].setValue(value);
  }

  public void setDetectorEtaDA(double value) {
    parameterField[6].setValue(value);
  }

  public double getDetector2Theta() {
    return detector2Theta;
  }

  public double getDetectorPhiDA() {
    return detectorPhiDA;
  }

  public double getDetectorOmegaDN() {
    return detectorOmegaDN;
  }

  public double getDetectorEtaDA() {
    return detectorEtaDA;
  }

  @Override
  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(firstLoading);
    sampleDeltaZ = getParameterValue(0);
    centerX = getParameterValue(1);
    centerY = getParameterValue(2);
    detector2Theta = getParameterValue(3);
    detectorPhiDA = getParameterValue(4);
    detectorOmegaDN = getParameterValue(5);
    detectorEtaDA = getParameterValue(6);
  }

  public double getRadius() {
    return detectorDistance;
  }

  public void setRadius(String value) {
    parameterField[0].setValue(value);
  }

  public double getOriginalCenterX() {
    return centerX + Double.parseDouble(stringField[0]);
  }

  public double getOriginalCenterY() {
    return centerY + Double.parseDouble(stringField[1]);
  }

  public void setOriginalCenterX(double value) {
    stringField[0] = Double.toString(value);
    parameterField[1].setValue(0.0);
  }

  public void setOriginalCenterY(double value) {
    stringField[1] = Double.toString(value);
    parameterField[2].setValue(0.0);
  }

  @Override
  public boolean freeAllBasicParameters() {
    for (int i = 0; i < 7; i++)
      parameterField[i].setRefinableCheckBound();
    return true;
  }

  public void refreshForNotificationDown(XRDcat source, int reason) {
    if (!getFilePar().isComputingDerivate() || (source == this ||
            (reason == Constants.ERROR_POSITION_CHANGED))) {
      refreshComputation = true;
      getParent().getParent().refreshForNotificationUp(this, Constants.ANGULAR_CALIBRATION);
    }
  }

  @Override
  public void calibrateX(DiffrDataFile datafile) {
    int datanumber = datafile.getTotalNumberOfData();
    updateParametertoDoubleBuffering(false);

    DataFileSet dataset = datafile.getDataFileSet();
    double zs = dataset.getZshift();
//    System.out.println("zs = " + zs);
    double omega = datafile.getOmegaValue();
    zs /= Math.sin(omega * Constants.DEGTOPI);

    double[][] tmat = ConvertImageToSpectra.getTransformationMatrixNew(detectorOmegaDN, detectorPhiDA,
        detectorEtaDA, detector2Theta, omega);
    for (int i = 0; i < datanumber; i++) {
//      double value = datafile.getXDataOriginal(i);
      double x = datafile.getXDataImage(i);
      double y = datafile.getYDataImage(i);

      double[] xf = ConvertImageToSpectra.getTransformedVectorNew(tmat, x, y, centerX, centerY, detectorDistance);
      double angcal = ConvertImageToSpectra.get2ThetaNew(xf) * Constants.PITODEG;

      datafile.setCalibratedXDataOnly(i, angcal);
    }
  }

  @Override
  public double notCalibrated(DiffrDataFile datafile, double x) {
    return x;
  }

  @Override
  public JOptionsDialog getOptionsDialog(Frame parent) {
    return new XSpiderAngularCalibration.JXSAngOptionsD(parent, this);
  }

  class JXSAngOptionsD extends JOptionsDialog {

    JParameterListPane coeffPanel;

    public JXSAngOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new GridLayout(0, 4, 3, 3));

      addParField(principalPanel, "Sample delta z :  ", parameterField[0]);
      addParField(principalPanel, "Center delta x :  ", parameterField[1]);
      addParField(principalPanel, "Center delta y :  ", parameterField[2]);
      addParField(principalPanel, "Detector 2theta:  ", parameterField[3]);
      addParField(principalPanel, "Detector phiDA:   ", parameterField[4]);
      addParField(principalPanel, "Detector omegaDN: ", parameterField[5]);
      addParField(principalPanel, "Detector etaDA:   ", parameterField[6]);

      setTitle("XSpider angular calibration");
      initParameters();

      pack();
    }

  @Override
    public void initParameters() {
    }

  }*/

}
