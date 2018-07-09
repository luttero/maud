/*
 * @(#)InstrumentBroadeningPVCaglioti.java created 07/10/2006 Casalino
 *
 * Copyright (c) 2006 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of the author and it is
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

import it.unitn.ing.rista.io.cif.CIFtoken;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.function.*;

import java.io.BufferedWriter;
import java.io.IOException;
import java.lang.*;
import java.awt.*;

import it.unitn.ing.rista.diffr.*;

import javax.swing.*;

/**
 * The InstrumentBroadening is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/12/04 14:30:04 $
 * @since JDK1.1
 */

public class InstrumentBroadeningPVCaglioti extends InstrumentBroadening {
  public static final String[] diclistc = {
      "_riet_caglioti_d_dep", "_riet_asymmetry_tan_dep",
      "_riet_omega/chi_broadening_convoluted",
      "_riet_par_asymmetry_truncation",
		  "_riet_par_asymmetry_reciprocal",

      "_riet_par_asymmetry_value", "_riet_par_caglioti_value", "_riet_par_gaussian_value",
      "_riet_par_broadening_omega", "_riet_par_broadening_chi",
      "_riet_par_broadening_eta", "_riet_par_broadening_cos_sin_eta",
		  "_riet_par_broadening_texture", "_riet_par_asymmetry_exp"};

  protected static final String[] diclistcrm = {
      "_riet_caglioti_d_dep", "_riet_asymmetry_tan_dep",
      "_riet_omega/chi_broadening_convoluted",
      "asymmetry truncation angle",
		  "asymmetry use the reciprocal",

      "asymmetry coeff ", "caglioti coeff ", "gaussian coeff ",
      "omega broadening coeff ", "chi broadening coeff ",
      "eta broadening coeff ", "cos(eta)/sin(eta) broadening coeff ",
		  "texture broadening coeff ", "Exponent value of asymmetry function"};

  protected static final String[] classlistc = {};

  protected static final String[] classlistcs = {};

  public static double minimumHWHMvalue = MaudPreferences.getDouble(
      "instrBroadening.minimumHWHMvalue", 0.0000001);

	public static double minimumAsymmetryValue = MaudPreferences.getDouble(
			"instrBroadening.minimumAsymmetryValue", 1);

  public InstrumentBroadeningPVCaglioti(XRDcat afile, String alabel) {
    super(afile, alabel);
    initXRD();
    identifier = "Caglioti PV";
    IDlabel = "Caglioti PV";
  }

  public InstrumentBroadeningPVCaglioti(XRDcat afile) {
    this(afile, "Caglioti PV");
  }

  public InstrumentBroadeningPVCaglioti() {
    identifier = "Caglioti PV";
    IDlabel = "Caglioti PV";
  }

  public void initConstant() {
    Nstring = 5;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 9;
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

    setAsymmetryTanDependent(false);
    setCagliotiTanDependent(true);
    setBroadeningConvoluted(false);
    setTruncationAngle("1.0");
    setAsymmetryReciprocal(false);
  }

	public void readall(CIFtoken ciffile) {
		super.readall(ciffile);
		if (getVersion() < 2.4)
			checkCaglioti();
	}

	public void checkCaglioti() {
		// for old version
		setAsymmetryTanDependent(true);
	}

	public void initializeAsNew() {
    if (initialized)
      return;
    initialized = true;

		setAsymmetryTanDependent(true);
    addparameterloopField(0, new Parameter(this, getParameterString(0, 0), 72.17134,
        ParameterPreferences.getDouble(getParameterString(0, 0) + ".min", 5.0),
        ParameterPreferences.getDouble(getParameterString(0, 0) + ".max", 1000)));
    addparameterloopField(0, new Parameter(this, getParameterString(0, 1), -.229337,
        ParameterPreferences.getDouble(getParameterString(0, 1) + ".min", -10),
        ParameterPreferences.getDouble(getParameterString(0, 1) + ".max", 10)));
    addparameterloopField(1, new Parameter(this, getParameterString(1, 0), 0.00252935,
        ParameterPreferences.getDouble(getParameterString(1, 0) + ".min", -0.1),
        ParameterPreferences.getDouble(getParameterString(1, 0) + ".max", 0.1)));
    addparameterloopField(1, new Parameter(this, getParameterString(1, 1), 0.002716717,
        ParameterPreferences.getDouble(getParameterString(1, 1) + ".min", -0.3),
        ParameterPreferences.getDouble(getParameterString(1, 1) + ".max", 0.3)));
    addparameterloopField(1, new Parameter(this, getParameterString(1, 2), 0.002312246,
        ParameterPreferences.getDouble(getParameterString(1, 2) + ".min", -0.5),
        ParameterPreferences.getDouble(getParameterString(1, 2) + ".max", 0.5)));
    addparameterloopField(2, new Parameter(this, getParameterString(2, 0), 0.0,
        ParameterPreferences.getDouble(getParameterString(2, 0) + ".min", -1.0),
        ParameterPreferences.getDouble(getParameterString(2, 0) + ".max", 2.0)));
    addparameterloopField(2, new Parameter(this, getParameterString(2, 1), 0.012,
        ParameterPreferences.getDouble(getParameterString(2, 1) + ".min", -0.1),
        ParameterPreferences.getDouble(getParameterString(2, 1) + ".max", 0.1)));
    addparameterloopField(3, new Parameter(this, getParameterString(3, 0), 0.0,
        ParameterPreferences.getDouble(getParameterString(3, 0) + ".min", -1),
        ParameterPreferences.getDouble(getParameterString(3, 0) + ".max", 1)));
    addparameterloopField(8, new Parameter(this, getParameterString(8, 0), 0.0,
				ParameterPreferences.getDouble(getParameterString(8, 0) + ".min", -0.9),
				ParameterPreferences.getDouble(getParameterString(8, 0) + ".max", 2.0)));
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length - 1; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.INSTRUMENT_BROADENING);
            return;
          }
	    for (int i = 0; i < parameterloopField[broadeningTextureID].size(); i++)
		    if (source == parameterloopField[broadeningTextureID].elementAt(i)) {
			    notifyParameterChanged(source, Constants.TEXTURE_CHANGED);
			    return;
		    }

      super.notifyParameterChanged(source);
    }
  }

  public static final int asymmetryTanDepID = 1;

  public void setAsymmetryTanDependent(String avalue) {
    if (!getAsymmetryTanDependent().equalsIgnoreCase(avalue)) {
      setString(asymmetryTanDepID, avalue);
    }
  }

  public void setAsymmetryTanDependent(boolean avalue) {
    if (avalue)
      setAsymmetryTanDependent("true");
    else
      setAsymmetryTanDependent("false");
  }

  public String getAsymmetryTanDependent() {
    return getString(asymmetryTanDepID);
  }

  public boolean isAsymmetryTanDependent() {
    return getAsymmetryTanDependent().equalsIgnoreCase("true");
  }

  public static final int cagliotiTanDepID = 0;

  public void setCagliotiTanDependent(String avalue) {
    if (!getCagliotiTanDependent().equalsIgnoreCase(avalue)) {
      setString(cagliotiTanDepID, avalue);
    }
  }

  public void setCagliotiTanDependent(boolean avalue) {
    if (avalue)
      setCagliotiTanDependent("true");
    else
      setCagliotiTanDependent("false");
  }

  public String getCagliotiTanDependent() {
    return getString(cagliotiTanDepID);
  }

  public boolean isCagliotiTanDependent() {
    return getCagliotiTanDependent().equalsIgnoreCase("true");
  }

  public static final int broadeningConvolutedID = 2;

  public void setBroadeningConvoluted(String avalue) {
    if (!getBroadeningConvoluted().equalsIgnoreCase(avalue)) {
      setString(broadeningConvolutedID, avalue);
    }
  }

  public void setBroadeningConvoluted(boolean avalue) {
    if (avalue)
      setBroadeningConvoluted("true");
    else
      setBroadeningConvoluted("false");
  }

  public String getBroadeningConvoluted() {
    return getString(broadeningConvolutedID);
  }

  public boolean isBroadeningConvoluted() {
    return getBroadeningConvoluted().equalsIgnoreCase("true");
  }

  public static final int asymmetryTruncationID = 3;

  public void setTruncationAngle(String value) {
    setString(asymmetryTruncationID, value);
  }

  public String getTruncationAngleString() {
    return getString(asymmetryTruncationID);
  }

  public double getTruncationAngle() {
    return truncationAngle;
  }

	public static final int asymmetryReciprocalID = 4;

	public void setAsymmetryReciprocal(String avalue) {
		if (!getAsymmetryReciprocal().equalsIgnoreCase(avalue)) {
			setString(asymmetryReciprocalID, avalue);
		}
	}

	public void setAsymmetryReciprocal(boolean avalue) {
		if (avalue)
			setAsymmetryReciprocal("true");
		else
			setAsymmetryReciprocal("false");
	}

	public String getAsymmetryReciprocal() {
		return getString(asymmetryReciprocalID);
	}

	public boolean isAsymmetryReciprocal() {
		return getAsymmetryReciprocal().equalsIgnoreCase("true");
	}

	public int getasymmetrynumber() {
    return getAsymmetryList().size();
  }

  public int getcagliotinumber() {
    return getCagliotiList().size();
  }

  public int getgaussiannumber() {
    return getGaussianList().size();
  }

  public int getbroadeningetanumber() {
    return getBroadeningEtaList().size();
  }

  public int getbroadeningomeganumber() {
    return getBroadeningOmegaList().size();
  }

  public int getbroadeningchinumber() {
    return getBroadeningChiList().size();
  }

  public static final int asymmetryID = 0;

  public ListVector getAsymmetryList() {
    return parameterloopField[asymmetryID];
  }

  public Parameter getAsymmetry(int index) {
    return (Parameter) getAsymmetryList().elementAt(index);
  }

	public static final int asymmetryExpID = 8;

	public ListVector getAsymmetryExpList() {
		return parameterloopField[asymmetryExpID];
	}

	public static final int cagliotiID = 1;

  public ListVector getCagliotiList() {
    return parameterloopField[cagliotiID];
  }

  public Parameter getCaglioti(int index) {
    return (Parameter) getCagliotiList().elementAt(index);
  }

  public static final int gaussianID = 2;

  public ListVector getGaussianList() {
    return parameterloopField[gaussianID];
  }

  public Parameter getGaussian(int index) {
    return (Parameter) getGaussianList().elementAt(index);
  }

  public static final int broadeningOmegaID = 3;

  public ListVector getBroadeningOmegaList() {
    return parameterloopField[broadeningOmegaID];
  }

  public Parameter getBroadeningOmega(int index) {
    return (Parameter) getBroadeningOmegaList().elementAt(index);
  }

  public static final int broadeningChiID = 4;

  public ListVector getBroadeningChiList() {
    return parameterloopField[broadeningChiID];
  }

  public Parameter getBroadeningChi(int index) {
    return (Parameter) getBroadeningChiList().elementAt(index);
  }

  public static final int broadeningEtaID = 5;

  public ListVector getBroadeningEtaList() {
    return parameterloopField[broadeningEtaID];
  }

  public Parameter getBroadeningEta(int index) {
    return (Parameter) getBroadeningEtaList().elementAt(index);
  }

	public static final int broadeningCosEtaID = 6;

	public ListVector getBroadeningCosEtaList() {
		return parameterloopField[broadeningCosEtaID];
	}

	public Parameter getBroadeningCosEta(int index) {
		return (Parameter) getBroadeningCosEtaList().elementAt(index);
	}

	public static final int broadeningTextureID = 7;

  boolean asymmetryTanDep = true;
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
  double truncationAngle = 0.4;

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);

    asymmetryTanDep = isAsymmetryTanDependent();
    cagliotiTanDep = isCagliotiTanDependent();
    broadeningConvoluted = isBroadeningConvoluted();
	  asymmetryReciprocal = isAsymmetryReciprocal();
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
  }

/*  private void checkCagliotiFirstParameter() {
    Parameter firstCaglioti = getCaglioti(0);
    if (firstCaglioti != null) {
      double first = firstCaglioti.getValueD();
      if (first < 0.0)
        firstCaglioti.setValue(-first);
    }
  }*/

  public Instrument getInstrument() {
    return (Instrument) getParent();
  }

  public Measurement getMeasurement() {
    return getInstrument().getMeasurement();
  }

  public Geometry getGeometry() {
    return getInstrument().getGeometry();
  }

	public void exportToCif(BufferedWriter output) throws IOException {
		if (asymmetryN > 0) {
			output.write("loop_");
			output.newLine();
			output.write("_riet_par_asymmetry_value");
			output.newLine();
			for (int i = 0; i < asymmetryN; i++) {
				output.write(" " + Fmt.format(asymmetry[i]));
				output.newLine();
			}
		}

		if (asymmetryExpN > 0) {
			output.write("loop_");
			output.newLine();
			output.write("_riet_par_asymmetry_exp");
			output.newLine();
			for (int i = 0; i < asymmetryExpN; i++) {
				output.write(" " + Fmt.format(asymmetryExp[i]));
				output.newLine();
			}
		}

		if (cagliotiN > 0) {
			output.write("loop_");
			output.newLine();
			output.write("_riet_par_caglioti_value");
			output.newLine();
			for (int i = 0; i < cagliotiN; i++) {
				output.write(" " + Fmt.format(caglioti[i]));
				output.newLine();
			}
		}

		if (gaussianN > 0) {
			output.write("loop_");
			output.newLine();
			output.write("_riet_par_gaussian_value");
			output.newLine();
			for (int i = 0; i < gaussianN; i++) {
				output.write(" " + Fmt.format(gaussian[i]));
				output.newLine();
			}
		}
	}

	public double[][] getInstrumentalBroadeningAt(double x, DiffrDataFile diffrDataFile) {

// Attention: x equal to 2theta

    double[] tilting_angles = diffrDataFile.getTiltingAngle();
    double broad[][] = new double[1][2];
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

    broad[0][0] = 0.0;
    for (int i = 0; i < gaussianN; i++)
      broad[0][0] += gaussian[i] * MoreMath.pow(x, i);
    if (broad[0][0] < 0.0)
      broad[0][0] = 0.0;
    if (broad[0][0] > 1.0)
      broad[0][0] = 1.0;
    broad[0][1] = 0.0;
    for (int i = 0; i < cagliotiN; i++)
      broad[0][1] += caglioti[i] * MoreMath.pow(tanx, i);
    broad[0][1] = Math.sqrt(broad[0][1]) / 2.0;

    if (broadeningOmegaN > 0) {
	    for (int i = 0; i < Math.min(3, broadeningOmegaN); i++)
		    broad[0][1] += broadeningOmega[i] * MoreMath.pow(domega, i);
	    for (int i = Math.min(3, broadeningOmegaN); i < broadeningOmegaN; i++)
		    broad[0][1] += broadeningOmega[i] * domega * MoreMath.pow(tanx, i - 1);
    }
	  if (broadeningChiN > 0) {
      double tano = MoreMath.sind(Math.abs(tilting_angles[1]));
      for (int i = 0; i < Math.min(2, broadeningChiN); i++)
        broad[0][1] += broadeningChi[i] * MoreMath.pow(tano, i + 1);
      for (int i = Math.min(2, broadeningChiN); i < broadeningChiN; i++)
        broad[0][1] += broadeningChi[i] * tano * MoreMath.pow(tanx, i - 1);
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
        broad[0][1] += broadeningEta[i] * MoreMath.pow(tane, i + 1);
      for (int i = Math.min(2, broadeningEtaN); i < broadeningEtaN; i++)
        broad[0][1] += broadeningEta[i] * tane * MoreMath.pow(tanx, i - 1);
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
			  broad[0][1] += broadeningCosEta[i] * Math.cos((i + 1) * tane + delta) * cosx;
		  }

	  }

    if (broad[0][1] < minimumHWHMvalue || Double.isNaN(broad[0][1]))
      broad[0][1] = minimumHWHMvalue;

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

/*  public double getConvolutedBroadening(double x, double[] tilting_angles, boolean dspacingbase) {

    double broad = 0.0;
    double domega = x - getMeasurement().getOmega(tilting_angles[0], x);
    double tanx = MoreMath.sind(Math.abs(tilting_angles[1]));
    if (broadeningConvoluted) {
        for (int i = 0; i < broadeningOmegaN; i++)
          broad += broadeningOmega[i] * MoreMath.pow(domega, i);
        for (int i = 0; i < broadeningChiN; i++)
          broad += broadeningChi[i] * MoreMath.pow(tanx, (i + 1));
    }
    return broad;
  }*/

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
	  double asy = 0.0;
		if (asymmetryN > 0) {
			double x1 = x;// = 0.0;
			boolean inverseBehaviour = diffrDataFile.dspacingbase || !asymmetryTanDep;
			if (diffrDataFile.dspacingbase)
				x1 = 1.0 / x;

			for (int i = 0; i < asymmetryN; i++)
				asy += asymmetry[i] * MoreMath.pow(x1, i);
			if (!inverseBehaviour)
				asy *= Math.tan(x * Constants.DEGTOPI / 2.0);
			if (asymmetryReciprocal) {
				if (asy != 0)
					asy = 1.0 / asy;
				if ((asy < minimumAsymmetryValue || Double.isNaN(asy)) && asy != 0)
					asy = minimumAsymmetryValue;
			} else {
				if ((asy < minimumAsymmetryValue || Double.isNaN(asy)) && asy != 0)
					asy = minimumAsymmetryValue;
			}
		}
    return asy;
  }

	public double getInstrumentalAsymmetryExp(double x, DiffrDataFile diffrDataFile) {
		double asy = 1.0;
		if (asymmetryExpN > 0) {
			double x1 = x;// = 0.0;
			boolean inverseBehaviour = diffrDataFile.dspacingbase || !asymmetryTanDep;
			if (diffrDataFile.dspacingbase)
				x1 = 1.0 / x;
			for (int i = 0; i < asymmetryExpN; i++)
				asy += asymmetryExp[i] * MoreMath.pow(x1, i);
		}
		return asy;
	}

	public void computeAsymmetry(DiffrDataFile diffrDataFile, Sample asample, double[] afit, int min, int max) {

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

  }

  public void computeReflectivityBroadening(DiffrDataFile diffrDataFile, Sample asample, double afit[], int min, int max) {

    DataFileSet adataset = diffrDataFile.getDataFileSet();
    Instrument ainstrument = getInstrument();

    double[][] broadinst = ainstrument.getInstrumentalBroadeningAt((max - min) / 2.0, diffrDataFile);
    double hwhm = broadinst[0][1];
    double eta = broadinst[0][0];
    double newFit[] = new double[max - min];

    double truncation = hwhm * adataset.getPeakCutoffD();

//    System.out.println("truncation " + truncation + " " + hwhm + " " + broadinst[0] + " " + broadinst[1]);
//	  System.out.println("Fit before " + afit[min] + " " + min);
    for (int j = min; j < max; j++) {
//      System.out.println("Fit before " + afit[j]);
      double x = diffrDataFile.getXData(j);
      double difference;
      double function = 0.0;
      double norm = 0.0;
      if (truncation != 0.0) {
        difference = 0.0;
        for (int ij = j; difference < truncation && diffrDataFile.insiderange(ij); ij++) {
          difference = Math.abs(diffrDataFile.getXData(ij) - x);
          double pvFunction = PseudoVoigt.getY(1.0, 0.0, eta, hwhm, difference);
          norm += pvFunction;
          function += afit[ij] * pvFunction;
        }
        difference = 0.0;
        for (int ij = j; difference < truncation && diffrDataFile.insiderange(ij); ij--) {
          difference = Math.abs(diffrDataFile.getXData(ij) - x);
          double pvFunction = PseudoVoigt.getY(1.0, 0.0, eta, hwhm, difference);
          norm += pvFunction;
          function += afit[ij] * pvFunction;
        }
        function /= norm;
      } else
        function = afit[j];
      newFit[j - min] = function;
    }
//	  System.out.println("Fit after " + afit[min] + " " + min);
    System.arraycopy(newFit, 0, afit, min, max - min);
  }

  public boolean freeAllBasicParameters() {
    return false;
  }

  public void plotFunction(Frame theframe, int index) {
    ParameterFunction function;
    if (getCagliotiList() == parameterloopField[index])
      function = new CagliotiFunction(parameterloopField[index], getInstrument().isTOF());
    else
      function = new PolynomialFunction(parameterloopField[index], getInstrument().isTOF());
    (new PlotParameterFunction(theframe, function)).setVisible(true);
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    return new JBankOptionsD(parent, this);
  }

  class JBankOptionsD extends JOptionsDialog {

    JParameterListPane AsymmetryPanel;
	  JParameterListPane AsymmetryExpPanel;
    JParameterListPane HWHMPanel;
    JParameterListPane GaussianPanel;
    JParameterListPane OmegaPanel;
    JParameterListPane ChiPanel;
    JParameterListPane EtaPanel;
	  JParameterListPane CosEtaPanel;
	  JParameterListPane TexturePanel;
    JCheckBox asymmetryTanDepCB;
	  JCheckBox asymmetryReciprocalCB;
    JCheckBox cagliotiTanDepCB;
    JCheckBox broadeningConvolutedCB;
    JTextField truncationTF = null;

    public JBankOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel aberrationPanel = new JPanel(new BorderLayout(3, 3));
      JTabbedPane tabPanel1 = new JTabbedPane();
      String tempString[] = {"Asymmetry",
          "HWHM",
          "Gaussianity",
          "Omega broad.",
          "Chi broad.",
          "Eta broad.",
		      "Cos/Sin(Eta) broad.",
          "Texture broad.", "Asy. Exponent"};
      principalPanel.add(BorderLayout.CENTER, aberrationPanel);
      aberrationPanel.add(BorderLayout.CENTER, tabPanel1);

      JPanel jpasy = new JPanel(new BorderLayout());
      tabPanel1.addTab(tempString[0], null, jpasy);

      AsymmetryPanel = new JParameterListPane(this, false, true);
      jpasy.add(BorderLayout.CENTER, AsymmetryPanel);

      JPanel p6 = new JPanel();
      p6.setLayout(new FlowLayout());
      asymmetryTanDepCB = new JCheckBox("Force Tan(theta) correction    ");
      asymmetryTanDepCB.setToolTipText("Check this box to correct the asymmetry value for tan(theta); not recommended for TOF");
      p6.add(asymmetryTanDepCB);

	    asymmetryReciprocalCB = new JCheckBox("Use reciprocal value    ");
	    asymmetryReciprocalCB.setToolTipText("Check this box to use the reciprocal value for asymmetry: higher value means higher asymmetry");
	    p6.add(asymmetryReciprocalCB);

      jpasy.add(BorderLayout.SOUTH, p6);
      p6.add(new JLabel(" Truncation angle [deg or d]: "));
      truncationTF = new JTextField(Constants.FLOAT_FIELD);
      p6.add(truncationTF);

      JPanel CagliotiPanel = new JPanel(new BorderLayout());
      HWHMPanel = new JParameterListPane(this, false, true);
      CagliotiPanel.add(BorderLayout.CENTER, HWHMPanel);
      tabPanel1.addTab(tempString[1], null, CagliotiPanel);
      p6 = new JPanel(new FlowLayout());
      CagliotiPanel.add(BorderLayout.SOUTH, p6);
      cagliotiTanDepCB = new JCheckBox("Force Caglioti in Tan(theta)");
      cagliotiTanDepCB.setToolTipText("Check this box to force the caglioti formula always in tan(theta); not recommeded for TOF");
      p6.add(cagliotiTanDepCB);

      GaussianPanel = new JParameterListPane(this, false, true);
      tabPanel1.addTab(tempString[2], null, GaussianPanel);

      OmegaPanel = new JParameterListPane(this, false, true);
      tabPanel1.addTab(tempString[3], null, OmegaPanel);

      ChiPanel = new JParameterListPane(this, false, true);
      tabPanel1.addTab(tempString[4], null, ChiPanel);

      EtaPanel = new JParameterListPane(this, false, true);
      tabPanel1.addTab(tempString[5], null, EtaPanel);

	    CosEtaPanel = new JParameterListPane(this, false, true);
	    tabPanel1.addTab(tempString[6], null, CosEtaPanel);

	    TexturePanel = new JParameterListPane(this, false, true);
	    tabPanel1.addTab(tempString[7], null, TexturePanel);

	    AsymmetryExpPanel = new JParameterListPane(this, false, true);
	    tabPanel1.addTab(tempString[asymmetryExpID], null, AsymmetryExpPanel);

	    JPanel convolutionPanel = new JPanel();
      aberrationPanel.add(BorderLayout.SOUTH, convolutionPanel);
      broadeningConvolutedCB = new JCheckBox("Omega/Chi broadening square convolution");
      broadeningConvolutedCB.setToolTipText("Set this option to use the omega/chi broadening as a square convoluted; default is unchecked");
      convolutionPanel.add(broadeningConvolutedCB);

/*      JPanel closebuttonPanel = new JPanel();
      closebuttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
      principalPanel.add(BorderLayout.SOUTH, closebuttonPanel);
      JButton jbok1 = new JCloseButton();
      closebuttonPanel.add(jbok1);
      jbok1.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          retrieveParameters();
          setVisible(false);
          dispose();
        }
      });*/


      setTitle("PseudoVoigt instrumental function");
      initParameters();
      pack();
    }

    public void initParameters() {
      asymmetryTanDepCB.setSelected(isAsymmetryTanDependent());
      cagliotiTanDepCB.setSelected(isCagliotiTanDependent());
      broadeningConvolutedCB.setSelected(isBroadeningConvoluted());
	    asymmetryReciprocalCB.setSelected(isAsymmetryReciprocal());
      AsymmetryPanel.setList(InstrumentBroadeningPVCaglioti.this, 0);
	    AsymmetryExpPanel.setList(InstrumentBroadeningPVCaglioti.this, asymmetryExpID);
      HWHMPanel.setList(InstrumentBroadeningPVCaglioti.this, 1);
      GaussianPanel.setList(InstrumentBroadeningPVCaglioti.this, 2);
      OmegaPanel.setList(InstrumentBroadeningPVCaglioti.this, 3);
      ChiPanel.setList(InstrumentBroadeningPVCaglioti.this, 4);
      EtaPanel.setList(InstrumentBroadeningPVCaglioti.this, 5);
	    CosEtaPanel.setList(InstrumentBroadeningPVCaglioti.this, 6);
	    TexturePanel.setList(InstrumentBroadeningPVCaglioti.this, 7);
      truncationTF.setText(getTruncationAngleString());
    }

    public void retrieveParameters() {
      super.retrieveParameters();

      AsymmetryPanel.retrieveparlist();
	    AsymmetryExpPanel.retrieveparlist();
	    HWHMPanel.retrieveparlist();
      GaussianPanel.retrieveparlist();
      OmegaPanel.retrieveparlist();
      ChiPanel.retrieveparlist();
	    EtaPanel.retrieveparlist();
	    CosEtaPanel.retrieveparlist();
	    TexturePanel.retrieveparlist();

      setTruncationAngle(truncationTF.getText());
      setAsymmetryTanDependent(asymmetryTanDepCB.isSelected());
      setCagliotiTanDependent(cagliotiTanDepCB.isSelected());
      setBroadeningConvoluted(broadeningConvolutedCB.isSelected());
	    setAsymmetryReciprocal(asymmetryReciprocalCB.isSelected());
    }

    public void dispose() {
      AsymmetryPanel.dispose();
	    AsymmetryExpPanel.dispose();
      HWHMPanel.dispose();
      GaussianPanel.dispose();
      OmegaPanel.dispose();
      ChiPanel.dispose();
      EtaPanel.dispose();
	    CosEtaPanel.dispose();
	    TexturePanel.dispose();

      super.dispose();
    }
  }

}

