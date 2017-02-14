/*
 * @(#)BkgPeak.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr;

import java.lang.*;

import it.unitn.ing.rista.util.*;

/**
 * The BkgPeak is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.9 $, $Date: 2006/07/20 13:39:03 $
 * @since JDK1.1
 */

public class BkgPeak extends XRDcat {
  protected static String[] diclistc = {"_riet_par_background_peak_id", "_riet_par_background_peak_exponent",
      "_riet_par_background_peak_height", "_riet_par_background_peak_2th",
      "_riet_par_background_peak_hwhm", "_riet_par_background_peak_gaussian",
      "_riet_par_background_peak_omega_position",
      "_riet_par_background_peak_omega_hwhm", "_riet_par_background_peak_omega_gaussian",
      "_riet_par_background_peak_chi_position",
      "_riet_par_background_peak_chi_hwhm", "_riet_par_background_peak_chi_gaussian",
      "_riet_par_background_peak_phi_position",
      "_riet_par_background_peak_phi_hwhm", "_riet_par_background_peak_phi_gaussian",
      "_riet_par_background_peak_eta_position",
      "_riet_par_background_peak_eta_hwhm", "_riet_par_background_peak_eta_gaussian"};
  protected static String[] diclistcrm = {"_riet_par_background_peak_id", "_riet_par_background_peak_exponent",
      "intensity", "position (arb)", "hwhm (arb)", "Gaussian content (0-1)",
      "position omega (deg)", "hwhm omega (deg)", "Gaussian content omega (0-1)",
      "position chi (deg)", "hwhm chi (deg)", "Gaussian content chi (0-1)",
      "position phi (deg)", "hwhm phi (deg)", "Gaussian content phi (0-1)",
      "position eta (deg)", "hwhm eta (deg)", "Gaussian content eta (0-1)"};

  protected static String[] classlistc = {};

  static int dimensionNumber = 5;

  double intensity = 0;
  double[] position = new double[dimensionNumber], hwhm = new double[dimensionNumber],
      dgx = new double[dimensionNumber], dcx = new double[dimensionNumber];
  double[] cutoff = new double[dimensionNumber];

  double exponent = 0.0;

  public BkgPeak(XRDcat obj, String alabel) {
    super(obj, alabel);
    initXRD();
  }

  public BkgPeak(XRDcat afile) {
    this(afile, "Bkg Peak x");
  }

  public void initConstant() {
    Nstring = 2;
    Nstringloop = 0;
    Nparameter = 3 * dimensionNumber + 1;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
  }

  public void initParameters() {
    super.initParameters();
    stringField[0] = "Background_peak";
    stringField[1] = "0.0";
 		parameterField[0] = new Parameter(this, getParameterString(0), 1.0);
    parameterField[1] = new Parameter(this, getParameterString(1), 0.0);
    parameterField[2] = new Parameter(this, getParameterString(2), 0.1);
    parameterField[3] = new Parameter(this, getParameterString(3), 0.0);
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
          notifyParameterChanged(source, Constants.BKG_PARAMETER_CHANGED);
          return;
        }
      }
    }    
    super.notifyParameterChanged(source);
  }

  public Parameter getIntensity() {
    return parameterField[0];
  }

  public Parameter getPosition(int index) {
    return parameterField[1 + 3 * index];
  }

  public Parameter getHWHM(int index) {
    return parameterField[2 + 3 * index];
  }

  public Parameter getGaussian(int index) {
    return parameterField[3 + 3 * index];
  }

  public void setIntensity(double value) {
    parameterField[0].setValue(value);
  }

  public void setPosition(int index, double value) {
    parameterField[1 + 3 * index].setValue(value);
  }

  public void setHWHM(int index, double value) {
    parameterField[2 + 3 * index].setValue(value);
  }

  public void setGaussian(int index, double value) {
    parameterField[3 + 3 * index].setValue(value);
  }

  public String getExponent() {
    return getString(1);
  }

  public double getExponentValue() {
    return Double.parseDouble(getExponent());
  }

  public void setExponent(String value) {
    setString(1, value);
    exponent = getExponentValue();
  }

  public void setExponentValue(double value) {
    setExponent(Double.toString(value));
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    double cut = ((DataFileSet) getParent()).getPeakCutoffD();
    intensity = getIntensity().getValueD();
    for (int index = 0; index < dimensionNumber; index++) {
      position[index] = getPosition(index).getValueD();
      hwhm[index] = getHWHM(index).getValueD();
      cutoff[index] = Math.abs(hwhm[index] * cut);
      exponent = getExponentValue();

      double eta = getGaussian(index).getValueD();
      if (hwhm[index] > 0) {
        dgx[index] = (1.0 - eta) * Constants.sqrtln2pi / hwhm[index];
        dcx[index] = eta / (Constants.PI * hwhm[index]);
      } else if (hwhm[index] < 0) {
        dgx[index] = -(1.0 - eta) * Constants.sqrtln2pi / hwhm[index];
        dcx[index] = -eta / (Constants.PI * hwhm[index]);
      }
    }
  }

  public void freeAllBackgroundParameters() {
    refreshparametersV();
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getValueD() != 0.0)
        apar.setRefinableCheckBound();
    }
  }

  public void checkBkgPeak() {
    for (int index = 0; index < dimensionNumber; index++) {
      double thwhm = getHWHM(index).getValueD();
      thwhm *= Math.sqrt(Constants.PI / Constants.LN2);
      getHWHM(index).setValue(thwhm);
    }
  }

  public double computeIntensity(double thetaord, double omega, double chi, double phi, double eta) {
    double[] diff = new double[dimensionNumber];
    diff[0] = position[0] - thetaord;
    diff[1] = position[1] - omega;
    diff[2] = position[2] - chi;
    diff[3] = position[3] - phi;
    diff[4] = position[4] - eta;
    double intens = 0.0;
    for (int index = 0; index < dimensionNumber; index++) {
      if (hwhm[index] != 0) {
        double intensi = 0;
        if (Math.abs(diff[index]) < cutoff[index])
          intensi = PseudoVoigt.getY(diff[index], Math.abs(hwhm[index]), dgx[index], dcx[index]);
        if (exponent != 0.0 && index == 0)
          intensi *= Math.pow(Math.abs(diff[index]), exponent);
        if (index == 0)
          intens = intensi;
        else
          intens *= intensi;
      }
    }
    return intensity * intens;
  }
}
