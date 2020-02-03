/*
 * @(#)AngleEnergyMapInstrumentBroadening.java created Aug 29, 2018 Povo
 *
 * Copyright (c) 2018 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.instbroad;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.util.function.*;

import javax.swing.*;
import java.awt.*;

/**
 * The AngleEnergyMapInstrumentBroadening is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Aug 29, 2018 11:30:34 AM $
 * @since JDK1.8
 */

public class AngleEnergyMapInstrumentBroadening extends InstrumentBroadening {

	public static String modelID = "Angle-Energy Map instrument broadening";
	public static String descriptionID = "Broadening of peaks by energy and angle";

	public static final String[] diclistc = {
      "_riet_par_asymmetry_truncation",
      
      "_riet_par_asymmetry_value", "_riet_par_caglioti_value", "_riet_par_gaussian_value",
      "_riet_par_broadening_omega", "_riet_par_broadening_chi",
      "_riet_par_broadening_eta", "_riet_par_broadening_cos_sin_eta",
      "_riet_par_broadening_cos_sin_eta_theta", "_riet_par_broadening_omega_theta",
      "_riet_par_broadening_texture", "_riet_par_asymmetry_exp",
      
			"_maud_energy_broadening_hwhm", "_maud_energy_broadening_gaussian",
			"_maud_energy_step_fraction", "_maud_energy_tail_beta",
			"_maud_energy_tail_fraction_k_alpha", "_maud_energy_fraction_k_beta"
	};

	protected static final String[] diclistcrm = {
      "asymmetry truncation angle",
      
      "asymmetry coeff ", "caglioti coeff ", "gaussian coeff ",
      "omega broadening coeff ", "chi broadening coeff ",
      "eta broadening coeff ", "cos(eta)/sin(eta) broadening coeff ",
      "theta * cos(eta)/sin(eta) broadening coeff ", "sin(theta-omega) broadening coeff ",
      "texture broadening coeff ", "Exponent value of asymmetry function",
      
			"energy broadening coeff ", "energy gaussian coeff ",
			"energy step fraction", "energy tail beta",
			"energy tail fraction Kalpha", "energy tail fraction Kbeta"
	};

	protected static final String[] classlistc = {};

	protected static final String[] classlistcs = {};
	
	public static double minimumHWHMvalue = MaudPreferences.getDouble(
			"instrBroadening.minimumHWHMvalue", 0.0000001);
  
  public static final int asymmetryTruncationID = 0;
  public static final int asymmetryID = 0;
  public static final int cagliotiID = 1;
  public static final int gaussianID = 2;
  public static final int broadeningOmegaID = 3;
  public static final int broadeningChiID = 4;
  public static final int broadeningEtaID = 5;
  public static final int broadeningCosEtaID = 6;
  public static final int broadeningThetaSinEtaID = 7;
  public static final int broadeningSinThetaOmegaID = 8;
  public static final int broadeningTextureID = 9;
  public static final int asymmetryExpID = 10;
  
  public static final int energyParametersIndex = 11;
  
  public AngleEnergyMapInstrumentBroadening(XRDcat obj, String alabel) {
		super(obj, alabel);
		initXRD();
		identifier = modelID;
		IDlabel = modelID;
		description = descriptionID;
	}

	public AngleEnergyMapInstrumentBroadening(XRDcat afile) {
		this(afile, modelID);
	}

	public AngleEnergyMapInstrumentBroadening() {
		identifier = modelID;
		IDlabel = modelID;
		description = descriptionID;
	}

	public void initConstant() {
		Nstring = 1;
		Nstringloop = 0;
		Nparameter = 0;
		Nparameterloop = 17;
		Nsubordinate = 0;
		Nsubordinateloop = 0;
	}

	public void initDictionary() {
		System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
		System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
		System.arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
		System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
	}

	public void initParameters() {
		super.initParameters();
    setTruncationAngle("1.0");
	}

	public void initializeAsNew() {
		if (initialized)
			return;
		initialized = true;
		// HWHM
		int index = 0;
		int number = 0;
    addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0,
        ParameterPreferences.getDouble(getParameterString(index, number) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 1.0)));
    number++;
    addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.0,
        ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -0.1),
        ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 0.1)));
    index++;
    number = 0;
    addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.00252935,
        ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -0.1),
        ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 0.1)));
    number++;
    addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.002716717,
        ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -0.3),
        ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 0.3)));
    number++;
    addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.002312246,
        ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -0.5),
        ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 0.5)));
    index++;
    number = 0;
    addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.0,
        ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
        ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
    number++;
    addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.012,
        ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -0.1),
        ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 0.1)));

    index = 10;
    number = 0;
    addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.0,
        ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -0.9),
        ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));

    index++;
    number = 0;
		addparameterloopField(index, new Parameter(this, getParameterString(index, number), 42.4,
				ParameterPreferences.getDouble(getParameterString(index, number) + ".min", 0),
				ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 100)));
		number++;
		addparameterloopField(index, new Parameter(this, getParameterString(index, number), 5330,
				ParameterPreferences.getDouble(getParameterString(index, number) + ".min", 100),
				ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 10000)));
		number++;
		addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0,
				ParameterPreferences.getDouble(getParameterString(index, number) + ".min", 0),
				ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 10000)));
		// Gaussian
		index++;
		number = 0;
		addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.01,
				ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
				ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
		number++;
		addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.0,
				ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
				ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
		// fS
		index++;
		number = 0;
		addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.0001,
				ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
				ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
		number++;
		addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.0,
				ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
				ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
		// beta
		index++;
		number = 0;
		addparameterloopField(index, new Parameter(this, getParameterString(index, number), 10,
				ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
				ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
		number++;
		addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.0,
				ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
				ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
		// fTKalpha
		index++;
		number = 0;
		addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.002,
				ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
				ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
		number++;
		addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.0,
				ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
				ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
		// fTKbeta
		index++;
		number = 0;
		addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.002,
				ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
				ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
		number++;
		addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.0,
				ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
				ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
	}

	public void notifyParameterChanged(Parameter source) {
		FilePar filepar = getFilePar();
		if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
			if (parameterloopField != null)
				for (int j = 0; j < parameterloopField.length; j++)
					for (int i = 0; i < parameterloopField[j].size(); i++)
						if (source == parameterloopField[j].elementAt(i)) {
							notifyParameterChanged(source, Constants.INSTRUMENT_BROADENING);
							return;
						}
			super.notifyParameterChanged(source);
		}
	}

	public Instrument getInstrument() {
		return (Instrument) getParent();
	}

	public Measurement getMeasurement() {
		return getInstrument().getMeasurement();
	}

	public Geometry getGeometry() {
		return getInstrument().getGeometry();
	}
  
  boolean asymmetryTanDep = false;
  boolean cagliotiTanDep = false;
  boolean broadeningConvoluted = false;
  boolean asymmetryReciprocal = false;
  double asymmetry[] = null;
  int asymmetryN = 0;
  double asymmetryExp[] = null;
  int asymmetryExpN = 0;
  double caglioti[] = null;
  int cagliotiN = 0;
  double gaussian[] = null;
  int gaussianN = 0;
  double broadeningOmega[] = null;
  int broadeningOmegaN = 0;
  double broadeningChi[] = null;
  int broadeningChiN = 0;
  double broadeningEta[] = null;
  int broadeningEtaN = 0;
  double broadeningCosEta[] = null;
  int broadeningCosEtaN = 0;
  double broadeningThetaSinEta[] = null;
  int broadeningThetaSinEtaN = 0;
  double broadeningSinThetaOmega[] = null;
  int broadeningSinThetaOmegaN = 0;
  double truncationAngle = 0.4;
  
  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);
    
    asymmetryTanDep = false;
    cagliotiTanDep = true;
    broadeningConvoluted = false;
    asymmetryReciprocal = true;
//    System.out.println("Asymmetry: " + getAsymmetryTanDependent());
    minimumHWHMvalue = MaudPreferences.getDouble(
        "instrBroadening.minimumHWHMvalue", 0.0000001);
    minimumHWHMvalue *= 4.0 * minimumHWHMvalue;
    truncationAngle = Double.parseDouble(getTruncationAngleString());
  }
  
  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses
    
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);
    
    asymmetry = getParameterLoopVector(asymmetryID);
    asymmetryN = numberOfLoopParameters[asymmetryID];
    asymmetryExp = getParameterLoopVector(asymmetryExpID);
    asymmetryExpN = numberOfLoopParameters[asymmetryExpID];
//    if (MaudPreferences.getBoolean("CagliotiFirstParameter.forcePositive", true))
//      checkCagliotiFirstParameter();
    caglioti = getParameterLoopVector(cagliotiID);
    cagliotiN = numberOfLoopParameters[cagliotiID];
    gaussian = getParameterLoopVector(gaussianID);
    gaussianN = numberOfLoopParameters[gaussianID];
    broadeningOmega = getParameterLoopVector(broadeningOmegaID);
    broadeningOmegaN = numberOfLoopParameters[broadeningOmegaID];
    broadeningChi = getParameterLoopVector(broadeningChiID);
    broadeningChiN = numberOfLoopParameters[broadeningChiID];
    broadeningEta = getParameterLoopVector(broadeningEtaID);
    broadeningEtaN = numberOfLoopParameters[broadeningEtaID];
    broadeningCosEta = getParameterLoopVector(broadeningCosEtaID);
    broadeningCosEtaN = numberOfLoopParameters[broadeningCosEtaID];
    broadeningThetaSinEta = getParameterLoopVector(broadeningThetaSinEtaID);
    broadeningThetaSinEtaN = numberOfLoopParameters[broadeningThetaSinEtaID];
    broadeningSinThetaOmega = getParameterLoopVector(broadeningSinThetaOmegaID);
    broadeningSinThetaOmegaN = numberOfLoopParameters[broadeningSinThetaOmegaID];
  }

  public void setTruncationAngle(String value) {
    setString(asymmetryTruncationID, value);
  }
  
  public String getTruncationAngleString() {
    return getString(asymmetryTruncationID);
  }
  
  public double getTruncationAngle() {
    return truncationAngle;
  }
  
  public java.util.Vector<double[]> getInstrumentBroadeningAt(double x, DiffrDataFile diffrDataFile) {
  
// Attention: x equal to 2theta
    double[] tilting_angles = diffrDataFile.getTiltingAngle();
    double[] hwhm = {0.0};
    double[] eta = {0.0};
    java.util.Vector<double[]> broadV = new java.util.Vector<>(2);
    broadV.add(hwhm);
    broadV.add(eta);
  
    double domega = x - getMeasurement().getOmega(tilting_angles[0], x);
    double tanx = x;
    if (cagliotiTanDep) {
      if (!diffrDataFile.dspacingbase)
        tanx = Math.tan(x * Constants.DEGTOPI / 2.0);
      else {     // the cagliotiTanDep is maintained only for compatibility
        x = Math.asin(Constants.minimumdspace / (2 * x));
        tanx = Math.tan(x * Constants.DEGTOPI / 2.0);
      }
    }

//    broadV.get(1)[0] = 0.0;
    for (int i = 0; i < gaussianN; i++)
      broadV.get(1)[0] += gaussian[i] * MoreMath.pow(x, i);
    if (broadV.get(1)[0] < 0.0)
      broadV.get(1)[0] = 0.0;
    if (broadV.get(1)[0] > 1.0)
      broadV.get(1)[0] = 1.0;

//    broadV.get(0)[0] = 0.0;
    for (int i = 0; i < cagliotiN; i++)
      broadV.get(0)[0] += caglioti[i] * MoreMath.pow(tanx, i);
    if (broadV.get(0)[0] <= 0)
      broadV.get(0)[0] = minimumHWHMvalue;
    else
      broadV.get(0)[0] = Math.sqrt(broadV.get(0)[0]) / 2.0;
  
    if (broadeningOmegaN > 0) {
      for (int i = 0; i < Math.min(3, broadeningOmegaN); i++)
        broadV.get(0)[0] += broadeningOmega[i] * MoreMath.pow(domega, i);
      for (int i = Math.min(3, broadeningOmegaN); i < broadeningOmegaN; i++)
        broadV.get(0)[0] += broadeningOmega[i] * domega * MoreMath.pow(tanx, i - 1);
    }
    if (broadeningChiN > 0) {
      double tano = MoreMath.sind(Math.abs(tilting_angles[1]));
      for (int i = 0; i < Math.min(2, broadeningChiN); i++)
        broadV.get(0)[0] += broadeningChi[i] * MoreMath.pow(tano, i + 1);
      for (int i = Math.min(2, broadeningChiN); i < broadeningChiN; i++)
        broadV.get(0)[0] += broadeningChi[i] * tano * MoreMath.pow(tanx, i - 1);
    }
    if (broadeningEtaN > 0) {
      if (!diffrDataFile.dspacingbase) {
        tanx = Math.tan(x * Constants.DEGTOPI);
        if (Math.abs(tanx) > 1.0E30)
          tanx = 0;
        else
          tanx = 1.0 / tanx;
      }
      double tane = MoreMath.sind(Math.abs(tilting_angles[3]));
      for (int i = 0; i < Math.min(2, broadeningEtaN); i++)
        broadV.get(0)[0] += broadeningEta[i] * MoreMath.pow(tane, i + 1);
      for (int i = Math.min(2, broadeningEtaN); i < broadeningEtaN; i++)
        broadV.get(0)[0] += broadeningEta[i] * tane * MoreMath.pow(tanx, i - 1);
    }
    if (broadeningCosEtaN > 0) {
      double cosx = 1.0;
      if (!diffrDataFile.dspacingbase)
        cosx = Math.abs(Math.cos(x * Constants.DEGTOPI));
      double tane = 2.0 * tilting_angles[3] * Constants.DEGTOPI;
      for (int i = 0; i < Math.min(2, broadeningCosEtaN); i++) {
        double delta = 0;
        if (MoreMath.odd(i))
          delta = Constants.PI / 2.0;
        broadV.get(0)[0] += broadeningCosEta[i] * Math.cos((i + 1) * tane + delta) * cosx;
      }
    
    }
  
    if (broadeningThetaSinEtaN > 0) {
      double cosx = 1.0;
      if (!diffrDataFile.dspacingbase)
        cosx += Math.abs(Math.cos(x * Constants.DEGTOPI));
      double tane = 2.0 * tilting_angles[3] * Constants.DEGTOPI;
      for (int i = 0; i < broadeningThetaSinEtaN; i++) {
        broadV.get(0)[0] += broadeningThetaSinEta[i] * Math.cos((i + 1) * tane) * cosx;
      }
    
    }
  
    if (broadeningSinThetaOmegaN > 2) {
      domega = Math.abs(domega + broadeningSinThetaOmega[0]) * Constants.DEGTOPI;
      double cosx = 1.0;
      if (!diffrDataFile.dspacingbase)
        cosx = 1.0 + broadeningSinThetaOmega[1] * Math.abs(Math.cos(x * Constants.DEGTOPI));
      for (int i = 2; i < broadeningSinThetaOmegaN; i++) {
        broadV.get(0)[0] += broadeningSinThetaOmega[i] * Math.sin(i * domega) * cosx;
      }
    
    }
  
    if (broadV.get(0)[0] < minimumHWHMvalue || Double.isNaN(broadV.get(0)[0]))
      broadV.get(0)[0] = minimumHWHMvalue;
  
    return broadV;
  }
  
  public java.util.Vector<double[]> getInstrumentEnergyBroadeningAt(double x) {
    
    java.util.Vector<double[]> broad = new java.util.Vector<>(parameterloopField.length - energyParametersIndex + 1);
    
    double[] par = getParameterLoopVector(energyParametersIndex);
    double[] value = {0.0};
    for (int i = 0; i < par.length; i++)
      value[0] += par[i] * MoreMath.pow(x, i);
    
    if (value[0] <= 0)
      value[0] = minimumHWHMvalue;
    else
      value[0] = Math.sqrt(value[0]);
    broad.add(value);
    
    par = getParameterLoopVector(energyParametersIndex + 1);
    value = new double[]{0.0};
    value[0] = 0.0;
    for (int i = 0; i < par.length; i++)
      value[0] += par[i] * MoreMath.pow(x, i);
    if (value[0] < 0.0)
      value[0] = 0.0;
    if (value[0] > 1.0)
      value[0] = 1.0;
    broad.add(value);
    
    for (int i = energyParametersIndex + 2; i < parameterloopField.length; i++) {
      par = getParameterLoopVector(i);
      value = new double[par.length];
      for (int j = 0; j < par.length; j++)
        value[j] = par[j];
      broad.add(value);
    }
    
    return broad;
  }
  
  public double getTextureBroadeningAt(double x) {
    int numberCoeff = numberOfLoopParameters[broadeningTextureID];
    if (numberCoeff == 0)
      return -1.0;
    double[] broadCoeff = getParameterLoopVector(broadeningTextureID);
    double hwhm = broadCoeff[0];
    for (int i = 1; i < numberCoeff; i++)
      hwhm += broadCoeff[i] * MoreMath.pow(x, i);
    
    if (hwhm < 0.0 || Double.isNaN(hwhm))
      hwhm = 0.0;
    return hwhm;
  }
  
  /**
   * Return the instrumental asymmetry for the convolution with the profile function.
   * The method here is called by Instrument and should not be modify in general.
   *
   * @param x             the 2-theta or d-spacing (if the spectrum is in d-spacing) of the
   *                      point for which the broadening should be computed.
   * @param diffrDataFile the spectrum
   * @return asymmetry parameter value
   */
  
  public double getInstrumentalAsymmetry(double x, DiffrDataFile diffrDataFile) {
    double[] asymmetry = getParameterLoopVector(asymmetryID);
    int asymmetryN = numberOfLoopParameters[asymmetryID];
    double asy = 0.0;
    if (asymmetryN > 0) {
      double x1 = x;// = 0.0;
      if (diffrDataFile.dspacingbase)
        x1 = 1.0 / x;
      for (int i = 0; i < asymmetryN; i++)
        asy += asymmetry[i] * MoreMath.pow(x1, i);
      if (!diffrDataFile.dspacingbase)
        asy *= Math.tan(x * Constants.DEGTOPI / 2.0);
      if (asy != 0)
        asy = 1.0 / asy;
      if ((asy < 0 || Double.isNaN(asy)) && asy != 0)
        asy = 0;
    }
    return asy;
  }
  
  public double getInstrumentalAsymmetryExp(double x, DiffrDataFile diffrDataFile) {
    double[] asymmetryExp = getParameterLoopVector(asymmetryExpID);
    int asymmetryExpN = numberOfLoopParameters[asymmetryExpID];
    double asy = 1.0;
    if (asymmetryExpN > 0) {
      double x1 = x;// = 0.0;
      if (diffrDataFile.dspacingbase)
        x1 = 1.0 / x;
      for (int i = 0; i < asymmetryExpN; i++)
        asy += asymmetryExp[i] * MoreMath.pow(x1, i);
    }
    return asy;
  }
  
  /**
   * Return the instrumental asymmetry for the convolution with the profile function.
   * The method here is called by Instrument and should not be modify in general.
   *
   * @param diffrDataFile the spectrum
   * @return void
   */
  
  public void computeAsymmetry(DiffrDataFile diffrDataFile, Sample asample, double[] afit, int min, int max) {
    /*
    Instrument ainstrument = getInstrument();
    
    double truncation_angle = getTruncationAngle();
    
    double newFit[] = new double[max - min];
    if (truncation_angle != 0.0) {
      int absdirection = 1;  // increasing step
      if (!diffrDataFile.increasingX())
        absdirection = -absdirection;
      absdirection *= (int) (truncation_angle / Math.abs(truncation_angle));
      truncation_angle = Math.abs(truncation_angle);
      
      for (int j = min; j < max; j++) {
        double x = diffrDataFile.getXData(j);
        double total_asymmetry = getInstrumentalAsymmetry(x, diffrDataFile);
        double asymmetry_exponent = getInstrumentalAsymmetryExp(x, diffrDataFile);
        if (total_asymmetry == 0.0)
          newFit[j - min] = afit[j];
        else {
          int direction = absdirection;
//          if (!diffrDataFile.dspacingbase && x > 90.0)
//            direction = -direction;
          double function = afit[j];
          double normalization = 1.0;
          int ij = j + direction;
          if (diffrDataFile.insiderange(ij)) {
            double difference = Math.abs(diffrDataFile.getXData(ij) - x);
            double expasymmetry = 1.0;
            for (; expasymmetry > 0.001 && difference < truncation_angle && diffrDataFile.insiderange(ij); ij += direction)
            {
              difference = Math.abs(diffrDataFile.getXData(ij) - x);
              double differenceExp = Math.pow(difference, asymmetry_exponent);
              expasymmetry = Math.exp(-differenceExp * total_asymmetry);
              function += afit[ij] * expasymmetry;
              normalization += expasymmetry;
            }
          }
          newFit[j - min] = function / normalization;
        }
        
      }
      System.arraycopy(newFit, 0, afit, min, max - min);
    }
    */
  }
  
	public void computeFluorescenceBroadening(DiffrDataFile diffrDataFile, Sample asample, double afit[], int min, int max) {

	}

	public boolean freeAllBasicParameters() {
		return false;
	}

	public void plotFunction(Frame theframe, int index) {
		ParameterFunction function = new PolynomialFunction(parameterloopField[index], false);
		(new PlotParameterFunction(theframe, function)).setVisible(true);
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
		return new JEDMapOptionsD(parent, this);
	}

	class JEDMapOptionsD extends JOptionsDialog {

		JParameterListPane PLPanel1[];
    JParameterListPane PLPanel2[];

		public JEDMapOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new BorderLayout(6, 6));

			JPanel aberrationPanel = new JPanel(new BorderLayout(3, 3));
      principalPanel.add(BorderLayout.CENTER, aberrationPanel);
			JTabbedPane tabPanel = new JTabbedPane();
      aberrationPanel.add(BorderLayout.CENTER, tabPanel);
      
      JTabbedPane tabPanel1 = new JTabbedPane();
      String tempString1[] = {"Asymmetry", "Caglioti ", "Gaussianity",
          "Omega", "Chi",
          "Eta broadening", "Cos(eta)/sin(eta)",
          "Theta * cos(eta)/sin(eta)", "Sin(theta-omega)",
          "Texture", "Exponent value of asymmetry"};
      PLPanel1 = new JParameterListPane[energyParametersIndex];
      for (int i = 0; i < energyParametersIndex; i++) {
        PLPanel1[i] = new JParameterListPane(this, false, true);
        tabPanel1.addTab(tempString1[i], null, PLPanel1[i]);
      }
      tabPanel.addTab("Angular broadening", null, tabPanel1);
      
      JTabbedPane tabPanel2 = new JTabbedPane();
			String tempString2[] = {"HWHM (FANO)", "Gaussianity", "fS", "Beta", "fT Kalpha", "fT Kbeta"};
      tabPanel.addTab("Energy broadening", null, tabPanel2);

			PLPanel2 = new JParameterListPane[AngleEnergyMapInstrumentBroadening.this.parameterloopField.length - energyParametersIndex];
			for (int i = energyParametersIndex; i < AngleEnergyMapInstrumentBroadening.this.parameterloopField.length; i++) {
				PLPanel2[i - energyParametersIndex] = new JParameterListPane(this, false, true);
				tabPanel2.addTab(tempString2[i - energyParametersIndex], null, PLPanel2[i - energyParametersIndex]);
			}

			setTitle("XRF/EDXRF instrumental function");
			initParameters();
			pack();
		}

		public void initParameters() {
      for (int i = 0; i < energyParametersIndex; i++) {
        PLPanel1[i].setList(AngleEnergyMapInstrumentBroadening.this, i);
      }
			for (int i = energyParametersIndex; i < AngleEnergyMapInstrumentBroadening.this.parameterloopField.length; i++) {
				PLPanel2[i - energyParametersIndex].setList(AngleEnergyMapInstrumentBroadening.this, i);
			}
		}

		public void retrieveParameters() {
			super.retrieveParameters();
      
      for (int i = 0; i < energyParametersIndex; i++) {
        PLPanel1[i].retrieveparlist();
      }
			for (int i = energyParametersIndex; i < AngleEnergyMapInstrumentBroadening.this.parameterloopField.length; i++) {
				PLPanel2[i - energyParametersIndex].retrieveparlist();
			}
		}

		public void dispose() {
      for (int i = 0; i < energyParametersIndex; i++) {
        PLPanel1[i].dispose();
      }
      for (int i = energyParametersIndex; i < AngleEnergyMapInstrumentBroadening.this.parameterloopField.length; i++) {
        PLPanel2[i - energyParametersIndex].dispose();
      }
			super.dispose();
		}
	}

}
