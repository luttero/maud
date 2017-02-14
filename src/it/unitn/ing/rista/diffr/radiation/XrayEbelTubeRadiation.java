/*
 * @(#)XrayEbelTubeRadiation.java created 06/01/2014 Caen
 *
 * Copyright (c) 2014 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.radiation;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.chemistry.AtomInfo;
import it.unitn.ing.rista.chemistry.XRayDataSqLite;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

/**
 *  The XrayEbelTubeRadiation is a class to calculate the entire
 *  radiation spectrum from an element according to Ebel theory
 *  H. Ebel, X-ray Tube Spectra, X-Ray Spectrom. 28, 255–266 (1999)
 *  and as implemented in Gimpy by G. Pepponi
 *
 *
 * @version $Revision: 1.0 $, $Date: 2014/01/06 11:48:17 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class XrayEbelTubeRadiation extends RadiationType {

	public static String[] diclistc = {
//			"_ebel_radiation_wavelength_id",
			"_ebel_radiation_minimum_keV",
			"_ebel_radiation_step_keV",

			"_ebel_radiation_tube_voltage_kV",
			"_ebel_radiation_incident_angle_degrees",
			"_ebel_radiation_exiting_angle_degrees",
			"_ebel_radiation_characteristic_scale_factor",

			"_diffrn_radiation_wavelength_id",
			"_instrument_filter_material"
	};
	public static String[] diclistcrm = {
//			"_ebel_radiation_wavelength_id",
			"_ebel_radiation_minimum_keV",
			"_ebel_radiation_step_keV",

			"_ebel_radiation_tube_voltage_kV",
			"_ebel_radiation_incident_angle_degrees",
			"_ebel_radiation_exiting_angle_degrees",
			"_ebel_radiation_characteristic_scale_factor",

			"_diffrn_radiation_wavelength_id",
			"_instrument_filter_material"
	};

	public static String[] classlistc = {"it.unitn.ing.rista.diffr.Radiation",
			"superclass:it.unitn.ing.rista.diffr.AbsorptionWindow"
	};
	public static String[] classlistcs = {};

//	private static int wavelength_id = 0;
	private static int minimum_keV_id = 0;
	private static int step_keV_id = 1;
	private static int voltage_kV_id = 0;
	private static int incident_angle_id = 1;
	private static int exiting_angle_id = 2;
	private static int continuous_scale_factor_id = 3;
	private static int radiation_id = 0;
	private static int filter_material_id = 1;

	private int spectrumPointsNumber = 0;

	private static double scale_factor = 1.0E-12;

	public XrayEbelTubeRadiation(XRDcat aobj, String alabel) {
		identifier = "X-ray Ebel tube";
		IDlabel = "X-ray Ebel tube";
		description = "X-ray Ebel tube radiation";
		if (alabel == null || alabel.equals(""))
			setLabel("unknown");
		else
			setLabel(alabel);
		setParent(aobj);
		parametersV = new Vector(0, 1);
		initXRD();
	}

	public XrayEbelTubeRadiation(XRDcat aobj) {
		this(aobj, "X-ray Ebel tube");
	}

	public XrayEbelTubeRadiation() {
		identifier = "X-ray Ebel tube";
		IDlabel = "X-ray Ebel tube";
		description = "X-ray Ebel tube radiation";
	}

	public void initConstant() {
		Nstring = 2;
		Nstringloop = 0;
		Nparameter = 4;
		Nparameterloop = 0;
		Nsubordinate = 0;
		Nsubordinateloop = 2;
	}

	public void initDictionary() {
		System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
		System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
		System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
		System.arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
	}

	public void initXRD() {
		initConstant();
		computeConstant();
		initDictionary();
		initParameters();
		isAbilitatetoRefresh = true;
	}

	public void computeConstant() {
		totstring = Nstring;
		totstringloop = totstring + Nstringloop;
		totparameter = totstringloop + Nparameter;
		totparameterloop = totparameter + Nparameterloop;
		totsubordinate = totparameterloop + Nsubordinate;
		totsubordinateloop = totsubordinate + Nsubordinateloop;

		diclist = new String[totsubordinateloop];
		diclistRealMeaning = new String[totsubordinateloop];
		classlist = new String[Nsubordinateloop];
		classlists = new String[Nsubordinate];
		pivotrequired = new boolean[Nsubordinateloop];
		stringField = new String[Nstring];
		stringloopField = new ListVector[Nstringloop];
		parameterField = new Parameter[Nparameter];
		parameterloopField = new ListVector[Nparameterloop];
		subordinateField = new XRDcat[Nsubordinate];
		subordinateloopField = new ListVector[Nsubordinateloop];

		parameterValues = new double[Nparameter];
		parameterLoopValuesVector = new Vector(Nparameterloop, 1);
		numberOfLoopParameters = new int[Nparameterloop];

	}

/*	public void initParameters() {
		super.initParameters();
	}*/

	public void initParameters() {
		int i;

		for (i = 0; i < Nstring; i++)
			stringField[i] = "";
		for (i = 0; i < Nstringloop; i++)
			stringloopField[i] = new ListVector(0, 1, this);
		for (i = 0; i < Nparameterloop; i++)
			parameterloopField[i] = new ListVector(0, 1, this);
		for (i = 0; i < Nsubordinateloop; i++)
			subordinateloopField[i] = new ListVector(0, 1, this);
		for (i = 0; i < Nsubordinateloop; i++)
			pivotrequired[i] = false;
		for (i = 0; i < Nparameter; i++)
			parameterField[i] = new Parameter(this, getParameterString(i), 0);

		int nsubord = totsubordinate - totparameterloop;
		nsubordSuperclass = new Vector[nsubord];
		for (i = 0; i < nsubord; i++) {
			if (classlists[i].startsWith("superclass")) {
				nsubordSuperclass[i] = Constants.getClassList(filterClass(classlists[i]));
			} else
				nsubordSuperclass[i] = null;
		}

		int nsubordloop = totsubordinateloop - totsubordinate;
		nsubordloopSuperclass = new Vector[nsubordloop];
		for (i = 0; i < nsubordloop; i++) {
			if (classlist[i].startsWith("superclass"))
				nsubordloopSuperclass[i] = Constants.getClassList(filterClass(classlist[i]));
			else
				nsubordloopSuperclass[i] = null;
		}

		refreshComputation = true;

//		stringField[wavelength_id] = MaudPreferences.getPref("ebel_tube.anode_element", "Cu");
		stringField[minimum_keV_id] = MaudPreferences.getPref("ebel_tube.minimum_keV", "1");
		stringField[step_keV_id] = MaudPreferences.getPref("ebel_tube.computation_step_keV", "0.1");

		parameterField[voltage_kV_id] = new Parameter(this, getParameterString(voltage_kV_id), 40,
				ParameterPreferences.getDouble(getParameterString(voltage_kV_id) + ".min", 20),
				ParameterPreferences.getDouble(getParameterString(voltage_kV_id) + ".max", 200));
		parameterField[incident_angle_id] = new Parameter(this, getParameterString(incident_angle_id), 90,
				ParameterPreferences.getDouble(getParameterString(incident_angle_id) + ".min", 1),
				ParameterPreferences.getDouble(getParameterString(incident_angle_id) + ".max", 90));
		parameterField[exiting_angle_id] = new Parameter(this, getParameterString(exiting_angle_id), 6,
				ParameterPreferences.getDouble(getParameterString(exiting_angle_id) + ".min", 0.1),
				ParameterPreferences.getDouble(getParameterString(exiting_angle_id) + ".max", 30));
		parameterField[continuous_scale_factor_id] = new Parameter(this, getParameterString(continuous_scale_factor_id), 1,
				ParameterPreferences.getDouble(getParameterString(continuous_scale_factor_id) + ".min", 0.1),
				ParameterPreferences.getDouble(getParameterString(continuous_scale_factor_id) + ".max", 10.0));
	}

	public void checkRadiation() {
/*		if (getLinesCountForFluorescence() <= 0) {
			addRadiation("Cu");
			// todo: add lines from fluorescence
			refreshComputation = true;
		}
		if (refreshComputation) {
			computeAll();
			refreshComputation = false;
		}*/
	}

	public Radiation getRadiation(int index) {
		if (subordinateloopField[0].size() > index && index >= 0)
			return (Radiation) subordinateloopField[0].elementAt(index);
		else
			return null;
	}

	public double getRadiationWavelength(int index) {
		return getRadiationWavelengthForFluorescence(index);
	}

	public double getRadiationWeigth(int index) {
		return getRadiationWeightForFluorescence(index);
	}

	public int getLinesCount() {
		if (refreshComputation) {
			computeAll();
			refreshComputation = false;
		}
//		checkRadiation();
		if (continuous_spectrum == null || characteristic_spectrum == null)
			return super.getLinesCount();
		return continuous_spectrum[0].length + characteristic_spectrum[0].length;
	}

	public int getSubdivision() {
		return MaudPreferences.getInteger("xrf_detector.energySubdivision", 20);

	}

	public int getLinesCountForPlot() {
		if (refreshComputation) {
			computeAll();
			refreshComputation = false;
		}
//		checkRadiation();
		if (continuous_spectrum == null || characteristic_spectrum == null)
			return super.getLinesCount();
		return characteristic_spectrum[0].length;
	}

/*	public int getLinesCountForFluorescence() {
		if (refreshComputation) {
			computeAll();
			refreshComputation = false;
		}
//		checkRadiation();
		if (continuous_spectrum == null || characteristic_spectrum == null)
			return super.getLinesCount();
		return continuous_spectrum[0].length + characteristic_spectrum[0].length;
	}*/

	public double getRadiationWavelengthForFluorescence(int index) {
//		checkRadiation();
		if (continuous_spectrum == null || characteristic_spectrum == null)
			return super.getRadiationWavelength(index);
		double energy;
		if (index >= characteristic_spectrum[0].length)
			energy = continuous_spectrum[0][index - characteristic_spectrum[0].length];
		else
			energy = characteristic_spectrum[0][index];
		return Constants.ENERGY_LAMBDA / energy * 0.001;
	}

	public double getRadiationEnergyForFluorescence(int index) {
//		checkRadiation();
		if (continuous_spectrum == null || characteristic_spectrum == null)
			return Constants.ENERGY_LAMBDA / super.getRadiationWavelength(index) * 0.001;
		double energy;
		if (index >= characteristic_spectrum[0].length)
			energy = continuous_spectrum[0][index - characteristic_spectrum[0].length];
		else
			energy =  characteristic_spectrum[0][index];
		return energy;
	}

	public int getNumberOfCharacteristicsLines() {
		return characteristic_spectrum[1].length;
	}

	public double getRadiationWeightForFluorescence(int index) {
//		checkRadiation();
		if (continuous_spectrum == null || characteristic_spectrum == null)
			return super.getRadiationWeigth(index);
		if (index >= characteristic_spectrum[1].length)
			return continuous_spectrum[1][index - characteristic_spectrum[1].length] * scale_factor;
		else
			return characteristic_spectrum[1][index] * scale_factor;
	}

	int anodeElementNumber = 0;
	int[] atomNumber = null;
	double[] atomFraction = null;
	double[] atomWeight = null;

	public void computeAll() {
		double minimumAcceptedIntensity = MaudPreferences.getDouble("ebelTube.BremsStrahlung_minimumIntensity", 0.0001);
		// here goes the tube spectrum computation

//		System.out.println("Computing Ebel tube spectrum for anode: " + getRadiation(0).getWavelengthID());

		// first we compute the bremsstrahlung, continuous spectrum

		for (int i = 0; i < Nparameter; i++) {
			parameterValues[i] = parameterField[i].getValueD();
		}

		parameterLoopValuesVector.removeAllElements();
		for (int i = 0; i < Nparameterloop; i++) {
			numberOfLoopParameters[i] = numberofelementPL(i);
			double parameterLoopValues[] = new double[numberOfLoopParameters[i]];
			for (int j = 0; j < numberOfLoopParameters[i]; j++)
				parameterLoopValues[j] = ((Parameter) parameterloopField[i].elementAt(j)).getValueD();
			parameterLoopValuesVector.addElement(parameterLoopValues);
		}

		anodeElementNumber = subordinateloopField[radiation_id].size();
		double totalAnodeWeight = 0.0;
		for (int anodeNumber = 0; anodeNumber < anodeElementNumber; anodeNumber++)
			totalAnodeWeight += getRadiation(anodeNumber).getWeigth().getValueD();
		atomNumber = new int[anodeElementNumber];
		atomFraction = new double[anodeElementNumber];
		atomWeight = new double[anodeElementNumber];
		for (int anodeNumber = 0; anodeNumber < anodeElementNumber; anodeNumber++) {
			atomFraction[anodeNumber] = getRadiation(anodeNumber).getWeigth().getValueD() / totalAnodeWeight;
			atomNumber[anodeNumber] = AtomInfo.retrieveAtomNumber(getRadiation(anodeNumber).getWavelengthID());
			atomWeight[anodeNumber] = AtomInfo.retrieveAtomWeight(getRadiation(anodeNumber).getWavelengthID());
		}

		double[][] tmp_spectrum = computeBremsStrahlung();

		// now we compute the characteristic lines

		characteristicLines();

		// now the absorption

		for (int i = 0; i < getList(filter_material_id).size(); i++) {
			AbsorptionWindow absorptionWindow = (AbsorptionWindow) getList(filter_material_id).elementAt(i);
			absorptionWindow.computeAbsorptionForLineWithEnergy(tmp_spectrum);
			absorptionWindow.computeAbsorptionForLineWithEnergy(characteristic_spectrum);
		}

		double maxIntensity = 0;
		for (int i = 0; i < characteristic_spectrum[0].length; i++) {
			double radFluorescence = characteristic_spectrum[1][i];
			if (maxIntensity < radFluorescence)
				maxIntensity = radFluorescence;
		}
		for (int i = 0; i < tmp_spectrum[0].length; i++) {
			double radFluorescence = tmp_spectrum[1][i];
			if (maxIntensity < radFluorescence)
				maxIntensity = radFluorescence;
		}
		double minimumIntensity = maxIntensity * minimumAcceptedIntensity;
		int index = 0;
		for (int i = 0; i < tmp_spectrum[0].length; i++) {
			double radFluorescence = tmp_spectrum[1][i];
			if (minimumIntensity < radFluorescence)
				index++;
		}
		int totalLines = index;

//		System.out.println("Total lines: " + totalLines + ", minimum " + minimumIntensity + ", " + maxIntensity + ", " + tmp_spectrum[0].length);
		continuous_spectrum = new double[3][totalLines];
		index = 0;
		for (int i = 0; i < tmp_spectrum[0].length; i++) {
			double radFluorescence = tmp_spectrum[1][i];
			if (minimumIntensity < radFluorescence) {
				for (int j = 0; j < 3; j++)
					continuous_spectrum[j][index] = tmp_spectrum[j][i];
				index++;
			}
		}

		scale_factor = 1.0E-12;
	}

	double[][] continuous_spectrum = null;

	public double[][] computeBremsStrahlung() {
		double tubeVoltageInkV = Math.abs(getParameterValue(voltage_kV_id));
		double scale_continuous = getParameterValue(continuous_scale_factor_id);
		double minimumEnergy = Double.parseDouble(getString(minimum_keV_id));
		double energyStep = Double.parseDouble(getString(step_keV_id));
		int n_points = (int) ((tubeVoltageInkV - minimumEnergy) / energyStep);
		double[][] tmp_spectrum = new double[3][n_points];
		for (int i = 0; i < n_points; i++) {
			tmp_spectrum[0][i] = minimumEnergy + energyStep * i;
			tmp_spectrum[1][i] = 0;
			tmp_spectrum[2][i] = 0;
		}

		double inc_ang_deg = getParameterValue(incident_angle_id);
		double ex_ang_deg = getParameterValue(exiting_angle_id);
		double sinRatio = MoreMath.sind(inc_ang_deg) / MoreMath.sind(ex_ang_deg);

		for (int anodeNumber = 0; anodeNumber < anodeElementNumber; anodeNumber++) {
			double J = 0.0135 * atomNumber[anodeNumber];
			double m = 0.1382 - 0.9211 / Math.sqrt(atomNumber[anodeNumber]);
			double lZ = Math.log(atomNumber[anodeNumber]);
			double eta = Math.pow(tubeVoltageInkV, m) * (0.1904 - 0.2236 * lZ + 0.1292 * lZ * lZ - 0.0149 * lZ * lZ * lZ);
			double rho_zeta_m = atomWeight[anodeNumber] / atomNumber[anodeNumber] * (0.787E-5 * Math.sqrt(J) * Math.pow(tubeVoltageInkV, 1.5) +
					0.735E-6 * tubeVoltageInkV * tubeVoltageInkV);
			double xself = 1.0314 - 0.0032 * atomNumber[anodeNumber] + 0.0047 * tubeVoltageInkV;
			double consta = 1.36E9;

			for (int i = 0; i < n_points; i++) {
				double U0 = tubeVoltageInkV / tmp_spectrum[0][i];
				double lU0 = Math.log(U0);
				double rho_zeta_bar = rho_zeta_m * lU0 * (0.49269 - 1.0987 * eta + 0.78557 * eta * eta) /
						(0.70256 - 1.09865 * eta + 1.0046 * eta * eta + lU0);
				double mu_tot = computeMhuTotal(tmp_spectrum[0][i]);
				double expo = mu_tot * rho_zeta_bar;
				expo = 2.0 * expo * sinRatio;
				double f = (1.0 - Math.exp(-expo)) / expo;
				double dN = energyStep * consta * atomNumber[anodeNumber] * f * Math.pow((U0 - 1.0), xself);
				tmp_spectrum[1][i] += dN * atomFraction[anodeNumber] * scale_continuous;
				tmp_spectrum[2][i] += mu_tot * atomFraction[anodeNumber];
			}
		}
		return tmp_spectrum;
	}

	double computeMhuTotal(double energyInKeV) {
		double mu_tot = 0;
		for (int i = 0; i < anodeElementNumber; i++)
			mu_tot += XRayDataSqLite.getTotalAbsorptionForAtomAndEnergy(atomNumber[i], energyInKeV) * atomFraction[i];
		return mu_tot;
	}

	double[] z_KL = {2.0, 8.0};
	double[] b_KL = {0.35, 0.25};
	// double[] const_KL = {5.0E13,6.9E13};  // in the paper
	double[] const_KL = {6.0E13,6.0E13};  // in the conclusions
	double[][] characteristic_spectrum = null;

	public void characteristicLines() {
		double tubeVoltageInkV = Math.abs(getParameterValue(voltage_kV_id));
		double minimumEnergy = Double.parseDouble(getString(minimum_keV_id));

		double energyStep = Double.parseDouble(getString(step_keV_id));
//		double scale_continuous = getParameterValue(continuous_scale_factor_id);
		double inc_ang_deg = getParameterValue(incident_angle_id);
		double ex_ang_deg = getParameterValue(exiting_angle_id);
		double sinRatio = MoreMath.sind(inc_ang_deg) / MoreMath.sind(ex_ang_deg);

//		scale_factor = 0;
		Vector<double[][]> all_spc = new Vector<double[][]>(anodeElementNumber, 1);
		for (int anodeNumber = 0; anodeNumber < anodeElementNumber; anodeNumber++) {
			double J = 0.0135 * atomNumber[anodeNumber];
			double m = 0.1382 - 0.9211 / Math.sqrt(atomNumber[anodeNumber]);
			double lZ = Math.log(atomNumber[anodeNumber]);
			double eta = Math.pow(tubeVoltageInkV, m) * (0.1904 - 0.2236 * lZ + 0.1292 * lZ * lZ - 0.0149 * lZ * lZ * lZ);
			double rho_zeta_m = atomWeight[anodeNumber] / atomNumber[anodeNumber] * (0.787E-5 * Math.sqrt(J) * Math.pow(tubeVoltageInkV, 1.5) +
					0.735E-6 * tubeVoltageInkV * tubeVoltageInkV);
//		double xself = 1.0314 - 0.0032 * atomNumber + 0.0047 * tubeVoltageInkV;
//		double consta = 1.36E9;
			Vector<FluorescenceLine> anodeFluorescenceLines = XRayDataSqLite.getFluorescenceLinesFor(atomNumber[anodeNumber], tubeVoltageInkV, minimumEnergy);
			int n_points = anodeFluorescenceLines.size();
			double[][] characteristic_spc = new double[3][n_points];

			for (int i = 0; i < n_points; i++) {
				FluorescenceLine line = anodeFluorescenceLines.get(i);
				characteristic_spc[0][i] = line.getEnergy();
//			System.out.println("Compute characteristic spectrum for line: " + XRayDataSqLite.shellIDs[line.getCoreShellID()] +
//					", " + line.getEnergy());
				double U0 = tubeVoltageInkV / characteristic_spc[0][i];
				double lU0 = Math.log(U0);
				double rho_zeta_bar = rho_zeta_m * lU0 * (0.49269 - 1.0987 * eta + 0.78557 * eta * eta) /
						(0.70256 - 1.09865 * eta + 1.0046 * eta * eta + lU0);

				double erre = 1.0 - 0.0081517 * atomNumber[anodeNumber] + 3.613E-5 * atomNumber[anodeNumber] * atomNumber[anodeNumber] + 0.009583 *
						atomNumber[anodeNumber] * Math.exp(-U0) + 0.001141 * tubeVoltageInkV;
				int flag_KL = 0; // "K"
				if (line.getCoreShellID() > XRayDataSqLite.K)
					flag_KL = 1;  // "L"
				double inv_S = z_KL[flag_KL] * b_KL[flag_KL] / atomNumber[anodeNumber];
				double den = (U0 * Math.log(U0) + 1.0 - U0);
				double num = Math.sqrt(U0) * Math.log(U0) + 2.0 * (1.0 - Math.sqrt(U0));
				inv_S = inv_S * den * (1.0 + 16.05 * (Math.sqrt(J / characteristic_spc[0][i])) * num / den);

				double mu_tot = computeMhuTotal(characteristic_spc[0][i]);
				double expo = mu_tot * rho_zeta_bar;
				expo = 2.0 * expo * sinRatio;
				double f = (1.0 - Math.exp(-expo)) / expo;

				double N_ch = const_KL[flag_KL] * inv_S * erre * f * line.getTransitionProbability() * line.getFluorescenceYield();

				characteristic_spc[1][i] = N_ch * atomFraction[anodeNumber];
				characteristic_spc[2][i] = mu_tot * atomFraction[anodeNumber];

			}
			all_spc.add(characteristic_spc);
		}
		int n_points = 0;
		for (int i = 0; i < anodeElementNumber; i++)
			n_points += all_spc.elementAt(i)[0].length;
		if (characteristic_spectrum == null || characteristic_spectrum[0].length != n_points)
			characteristic_spectrum = new double[3][n_points];
		int index = 0;
		for (int i = 0; i < anodeElementNumber; i++) {
			double[][] characteristic_spc = all_spc.elementAt(i);
			for (int j = 0; j < characteristic_spc[0].length; j++, index++) {
				for (int k = 0; k < 3; k++)
					characteristic_spectrum[k][index] = characteristic_spc[k][j];
//				System.out.println(i + " " + characteristic_spectrum[0][index]);
			}
		}
	}

	public double getFiltersFluorescenceIntensityTotal() {
		double totalFluorescence = 0;
		for (int i = 0; i < getList(filter_material_id).size(); i++) {
			AbsorptionWindow absorptionWindow = (AbsorptionWindow) getList(filter_material_id).elementAt(i);
			totalFluorescence += absorptionWindow.getFluorescenceIntensity();
		}
		return totalFluorescence;
	}

	public Vector<FluorescenceLine> getFluorescenceLines(double energyInKeV) {
		Vector<FluorescenceLine> allLines = new Vector<FluorescenceLine>();
		for (int i = 0; i < getList(filter_material_id).size(); i++) {
			AbsorptionWindow absorptionWindow = (AbsorptionWindow) getList(filter_material_id).elementAt(i);
			allLines.addAll(absorptionWindow.getFluorescenceLines(energyInKeV));
		}
		return allLines;
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
		JOptionsDialog adialog = new JEbelTubeRadiationOptionsD(parent, this);
		return adialog;
	}

	public class JEbelTubeRadiationOptionsD extends JOptionsDialog {

/*		JComboBox tubechoice;

		public JTubeRadiationOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new FlowLayout());

			principalPanel.add(new JLabel("X-ray tube:"));

			tubechoice = new JComboBox();
			principalPanel.add(tubechoice);
			tubechoice.setEditable(false);
			tubechoice.setMaximumRowCount(5);

			setTitle("X-ray tube radiation");
			initParameters();
			pack();
		}

		public void initParameters() {
		}

		public void retrieveParameters() {
		}
	}*/

		JSubordinateLoopListPane radiationPanel;
		JSubordinateLoopListPane absorptionMaterialPanel;

		JTextField tubeVoltageTF;
		JTextField minimumEnergyTF;
		JTextField stepEnergyTF;
		JTextField incidentAngleTF;
		JTextField exitingAngleTF;
		JTextField ratioIntensitiesTF;

		JButton plotSpectrumB;

	public JEbelTubeRadiationOptionsD(Frame parent, XRDcat obj) {

		super(parent, obj);

		principalPanel.setLayout(new BorderLayout(3, 3));
		JPanel jpc1 = new JPanel(new BorderLayout(3, 3));
		principalPanel.add(BorderLayout.CENTER, jpc1);
		JPanel jp1 = new JPanel(new GridLayout(0, 2, 6, 6));
		jpc1.add(BorderLayout.CENTER, jp1);
		jp1.add(new JLabel("Tube voltage (kV): "));
		tubeVoltageTF = new JTextField(Constants.FLOAT_FIELD);
		tubeVoltageTF.setToolTipText("Specify the voltage applied to the tube in kV");
		jp1.add(tubeVoltageTF);
		jp1.add(new JLabel("Minimum energy (keV): "));
		minimumEnergyTF = new JTextField(Constants.FLOAT_FIELD);
		minimumEnergyTF.setToolTipText("Specify the minimum energy in keV for the spectrum computation");
		jp1.add(minimumEnergyTF);
		jp1.add(new JLabel("Energy step (keV): "));
		stepEnergyTF = new JTextField(Constants.FLOAT_FIELD);
		stepEnergyTF.setToolTipText("Specify step in keV for the spectrum computation (smaller step, longer computation!)");
		jp1.add(stepEnergyTF);
		jp1.add(new JLabel("Tube incident angle (degrees): "));
		incidentAngleTF = new JTextField(Constants.FLOAT_FIELD);
		incidentAngleTF.setToolTipText("Specify the incident angle for the electrons on the anode in degrees");
		jp1.add(incidentAngleTF);
		jp1.add(new JLabel("Tube exiting angle (degrees): "));
		exitingAngleTF = new JTextField(Constants.FLOAT_FIELD);
		exitingAngleTF.setToolTipText("Specify the exiting angle from anode for the x-rays in degrees");
		jp1.add(exitingAngleTF);
		jp1.add(new JLabel("Scale factor for Bremsstrahlung: "));
		ratioIntensitiesTF = new JTextField(Constants.FLOAT_FIELD);
		ratioIntensitiesTF.setToolTipText("Scale factor for the continuous spectrum (1 the default)");
		jp1.add(ratioIntensitiesTF);

		JPanel eastPanel = new JPanel(new BorderLayout(6, 6));
		principalPanel.add(BorderLayout.EAST, eastPanel);

		JPanel jpa = new JPanel(new BorderLayout(3, 3));
		eastPanel.add(jpa, BorderLayout.CENTER);

		radiationPanel = new JSubordinateLoopListPane(this, "X-ray anode line");
		jpa.add(radiationPanel, BorderLayout.CENTER);

		absorptionMaterialPanel = new JSubordinateLoopListPane(this, "Absorption material section");
		jpa.add(absorptionMaterialPanel, BorderLayout.SOUTH);

		JPanel plotButtonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
		jpc1.add(BorderLayout.SOUTH, plotButtonPanel);
		plotSpectrumB = new JButton("Plot tube spectrum");
		plotSpectrumB.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				plotSpectrum();
			}
		});
		plotButtonPanel.add(plotSpectrumB);

		setTitle("XRF Ebel tube data");
		initParameters();
		pack();
	}

	public void initParameters() {
		tubeVoltageTF.setText(getParameterValueAsString(voltage_kV_id));
		addComponenttolist(tubeVoltageTF, getParameter(voltage_kV_id));
		minimumEnergyTF.setText(getString(minimum_keV_id));
		stepEnergyTF.setText(getString(step_keV_id));
		incidentAngleTF.setText(getParameterValueAsString(incident_angle_id));
		addComponenttolist(incidentAngleTF, getParameter(incident_angle_id));
		exitingAngleTF.setText(getParameterValueAsString(exiting_angle_id));
		addComponenttolist(exitingAngleTF, getParameter(exiting_angle_id));
		ratioIntensitiesTF.setText(getParameterValueAsString(continuous_scale_factor_id));
		addComponenttolist(ratioIntensitiesTF, getParameter(continuous_scale_factor_id));

		radiationPanel.setList(XrayEbelTubeRadiation.this, radiation_id);
		absorptionMaterialPanel.setList(XrayEbelTubeRadiation.this, filter_material_id);
	}

	/**
	 * This method is automatically called when the user press the close button on the dialog.
	 */
	public void retrieveParameters() {
		setString(minimum_keV_id, minimumEnergyTF.getText());
		setString(step_keV_id, stepEnergyTF.getText());
		getParameter(voltage_kV_id).setValue(tubeVoltageTF.getText());
		getParameter(incident_angle_id).setValue(incidentAngleTF.getText());
		getParameter(exiting_angle_id).setValue(exitingAngleTF.getText());
		getParameter(continuous_scale_factor_id).setValue(ratioIntensitiesTF.getText());
	}

		public void plotSpectrum() {
			retrieveParameters();
			updateStringtoDoubleBuffering(false);
			refreshComputation = true;
			int lineCounts = getLinesCountForFluorescence();
			double[] x = new double[lineCounts];
			double[] y = new double[lineCounts];
			for (int i = 0; i < lineCounts; i++) {
				x[i] = getRadiationEnergyForFluorescence(i);
				y[i] = getRadiationWeightForFluorescence(i);
			}
			(new PlotSimpleData(this, x, y, true)).setVisible(true);


			lineCounts = getLinesCountForFluorescence() - characteristic_spectrum[0].length;
			x = new double[lineCounts];
			y = new double[lineCounts];
			for (int i = 0; i < lineCounts; i++) {
				x[i] = getRadiationEnergyForFluorescence(i + characteristic_spectrum[0].length);
				y[i] = getRadiationWeightForFluorescence(i + characteristic_spectrum[1].length);
			}
			(new PlotSimpleData(this, x, y, true)).setVisible(true);
		}
}

}


/*
From Giancarlo Gimpy

    Cu_tube=Xray_Tube(Element("Cu"), 90.0,  6.0)
    V_kV=30.0
    dE_keV=0.01
    Cu_tube.bremsstrahlung_ebel(V_kV, dE_keV)
    Cu_tube.characteristic_ebel(V_kV, "K")
    Cu_tube.characteristic_ebel(V_kV, "L1")
    print Cu_tube.cont_spect[1]
#    pylab.plot(Cu_tube.cont_spect[0], Cu_tube.cont_spect[1], linewidth=1.0)
#    pylab.show()
    print Cu_tube.line_spect
    lines = []
    for it in Cu_tube.line_spect:
        for itl in it:
            if itl[1][0]!= 0.0:
                lines.append([itl[0], itl[1][0]])
    li_sp = lines_spectrum(lines, energies=Cu_tube.cont_spect[0]*1000.0, delta_E = 100.0)
    el_be = Element("Be")
    el_mat = Xrf_Material([[el_be, 1.0]], el_be.density)
    be_filter = Filter(el_mat, 300.0)
    be_filt_tr = be_filter.transmission(Cu_tube.cont_spect[0]*1000.0)[1]
    air_tr= Filter(air,  350.0E3).transmission(Cu_tube.cont_spect[0]*1000.0)[1]
    ni_tr = Ni_filter.transmission(Cu_tube.cont_spect[0]*1000.0)[1]
    pylab.plot(Cu_tube.cont_spect[0], be_filt_tr)
    pylab.plot(Cu_tube.cont_spect[0], air_tr)
    pylab.plot(Cu_tube.cont_spect[0], ni_tr)
    pylab.legend([r'300 $\mu$m Be window', r'350 mm air path', r'12.5 $\mu$m Ni  filter'],
                 loc='lower right')
    pylab.title('Cu diffraction tube 30 kV - emission spectrum')
    pylab.xlabel(r"energy [keV]", fontsize = 12)
    pylab.ylabel(r"transmission coefficient", fontsize = 12)
    pylab.title('Transmission of absorbers between anode and sample')
    pylab.show()
    pylab.plot(Cu_tube.cont_spect[0], Cu_tube.cont_spect[1])
    pylab.plot(li_sp[0]/1000.0, li_sp[1])
    pylab.plot(li_sp[0]/1000.0, li_sp[1]+Cu_tube.cont_spect[1])
    pylab.legend(['Bremsstrahlung', 'characteristic lines', 'excitation spectrum'])
    pylab.title('Cu diffraction tube 30 kV - emission spectrum')
    pylab.xlabel(r"energy [keV]", fontsize = 12)
    pylab.ylabel(r"spectral flux density [photons/(sr * s * mA *0.1KeV)]", fontsize = 12)
    pylab.show()


    class Xray_Tube():
    """
    calculates emission from an X-Ray tube accordind to Ebel
    Horst Ebel, X-Ray Spectrom. 28, 255-266 (1999)
    """
    def __init__(self, element, inc_ang_deg, ex_ang_deg):
        self.element=None
        if isinstance(element,  Element) :
            self.element = element
        else:
            try:
                self.element=Element(element)
            except:
                print element, "cannot be identified as an element"

        self.inc_ang_deg = inc_ang_deg
        self.ex_ang_deg = ex_ang_deg
        self.cont_spect = []
        self.line_spect = []

    def bremsstrahlung_ebel(self,  V_kV,  dE_keV, Emin_keV=1.0):
        n_points=(V_kV-Emin_keV)/dE_keV
        E_arr=np.arange(Emin_keV,V_kV,dE_keV,dtype=float)
        new_ens=self.element.ebel.edge_energies_pmd(E_arr*1000.0)
        E_arr=np.concatenate([E_arr, np.array(new_ens)/1000.0])
        E_arr.sort()
        E0=V_kV
        U0=V_kV/E_arr
        J=0.0135*self.element.Z
        m=0.1382-0.9211/np.sqrt(self.element.Z)
        lZ=np.log(self.element.Z)
        eta=np.power(E0,m)*(0.1904-0.2236*lZ+0.1292*lZ*lZ-0.0149*lZ*lZ*lZ)
        rzm=self.element.A/self.element.Z*(0.787E-5*np.sqrt(J)*np.power(E0,1.5)+0.735E-6*E0*E0) #rho_zeta_m
        lU0=np.log(U0)
        rzb=rzm*lU0*(0.49269-1.0987*eta+0.78557*eta*eta)/(0.70256-1.09865*eta+1.0046*eta*eta+lU0) #rho_zeta_bar
        xe=1.0314-0.0032*self.element.Z+0.0047*E0  #exponent xself
        consta=1.36E9
        mu_tot=self.element.ebel.mu(E_arr*1000.0)[1]
        expo=mu_tot*rzb
        expo=2.0*expo*np.sin(deg_to_rad(self.inc_ang_deg))/np.sin(deg_to_rad(self.ex_ang_deg))
        effe=(1.0-np.exp(-expo))/expo   #f
        dN=dE_keV*consta*self.element.Z*effe*np.power((U0-1.0),xe)
        self.cont_spect = [E_arr,dN, mu_tot]
        return [E_arr, dN,  mu_tot]

    def characteristic_ebel(self, V_kV, edge,  Emin_keV=1.0):
        """
        edge in this case is one of "K", "L1", "L2", "L3"
        """
        flag_KL=0
        if edge[0]=="L":
            flag_KL=1
        print self.element.edges
        E_keV=self.element.edges[edge][0]/1000.0
        E0=V_kV
        U0=V_kV/E_keV
        J=0.0135*self.element.Z
        m=0.1382-0.9211/np.sqrt(self.element.Z)
        lZ=np.log(self.element.Z)
        eta=np.power(E0,m)*(0.1904-0.2236*lZ+0.1292*lZ*lZ-0.0149*lZ*lZ*lZ)
        rzm=self.element.A/self.element.Z*(0.787E-5*np.sqrt(J)*np.power(E0,1.5)+0.735E-6*E0*E0) #rho_zeta_m
        lU0=np.log(U0)
        rzb=rzm*lU0*(0.49269-1.0987*eta+0.78557*eta*eta)/(0.70256-1.09865*eta+1.0046*eta*eta+lU0)
        inv_S=1.0
        erre=1.0-0.0081517*self.element.Z+3.613E-5*self.element.Z*self.element.Z+0.009583*self.element.Z*np.exp(-U0)+0.001141*E0
        z_KL=[2.0,8.0]
        b_KL=[0.35,0.25]
        const_KL=[5.0E13,6.9E13]  # in the paper
        const_KL=[6.0E13,6.0E13]  # in the conclusions
        inv_S=z_KL[flag_KL]*b_KL[flag_KL]/self.element.Z
        den=(U0*np.log(U0)+1.0-U0)
        num=np.sqrt(U0)*np.log(U0)+2.0*(1.0-np.sqrt(U0))
        inv_S=inv_S*den*(1.0+16.05*(np.sqrt(J/E_keV))*num/den)
        lines = self.element.transitions(E_min=1000.0,  E_max=V_kV*1000.0,  edge=edge)
        char_lines=[]
        for k, lin in lines.iteritems():   #enumerate(lines):
            mu_tot=self.element.ebel.mu(lin[0])[1]
            expo=2.0*mu_tot*rzb*np.sin(deg_to_rad(self.inc_ang_deg))/np.sin(deg_to_rad(self.ex_ang_deg))
            effe=(1.0-np.exp(-expo))/expo #f
            N_ch=const_KL[flag_KL]*inv_S*erre*effe*lin[1]*self.element.ret_fluo_yield(edge)
            char_line=[lin[0], N_ch]
            char_lines.append(char_line)
        self.line_spect.append(char_lines)
        return char_lines



 */