/*
 * @(#)MEMLTexture.java created 16/07/1998 ILL, Grenoble
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

package it.unitn.ing.rista.diffr.rta;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.util.*;

import static it.unitn.ing.rista.util.MaudPreferences.*;

import it.unitn.ing.rista.comp.*;
import it.unitn.ing.jgraph.ColorMap;

import java.awt.*;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.event.*;
import java.util.*;
import java.io.OutputStream;

/**
 * The MEMLTexture is a class to perform texture computation using the Entropy/WIMV
 * method.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.37 $, $Date: 2006/12/04 14:30:05 $
 * @since JDK1.1
 */


public class MEMLTexture extends DiscreteODFTexture implements MEMFunction {

	public static String[] diclistc = {"_rita_generate_symmetry", "_rita_wimv_sum_coincidence",
			"_rita_wimv_iteration_max", "_rita_wimv_exponent",
			"_rita_wimv_refl_min_int", "_rita_wimv_odf_resolution",
			"_rita_wimv_tube_projection", "_rita_wimv_store_ang_conv",
			"_rita_wimv_odf_coverage_%", "_rita_odf_sharpness",
			"_rita_wimv_phon_use", "_rita_wimv_tube_weight",
			"_rita_wimv_normalize_pole_figures", "_rita_wimv_weigths_exponent",
			"_rita_odf_refinable", "_rita_wimv_refl_min_dspacing"};
	public static String[] diclistcrm = {"_rita_generate_symmetry", "_rita_wimv_sum_coincidence",
			"_rita_wimv_iteration_max", "_rita_wimv_exponent",
			"_rita_wimv_refl_min_int", "_rita_wimv_odf_resolution",
			"_rita_wimv_tube_projection", "_rita_wimv_store_ang_conv",
			"_rita_wimv_odf_coverage_%", "_rita_odf_sharpness",
			"_rita_wimv_phon_use", "_rita_wimv_tube_weight",
			"_rita_wimv_normalize_pole_figures", "_rita_wimv_weigths_exponent",
			"_rita_odf_refinable", "_rita_wimv_refl_min_dspacing"};

	public static String[] classlistcs = {};
	public static String[] classlistc = {};

	int numberOfData = 0;
	int[] baseDataIndex = null;
	//	double parameters[];
	int numberOfParameters = 0;
	boolean refreshFit = true;
	double R = 0.0;
	double Rw = 0.0;
//	int actualfit = 0;
	private double dta_fix[] = null;
	double fit[] = null;
	double wgt[] = null;
	public int[] poleindex = null;
	int[] pointindex = null;
	int[] datasetindex = null;
	int[] datafileindex = null;
	//	int[] symmetryindex = null;
	public double[] poleFactor = null;

//  Sample actualsample = null;

	Vector totCellID = null;
	Vector totCellWGT = null;
	double[] totalWeight = null;
	int[] totalHits = null;
	boolean reduceMemory = true;
	double phiturn = 5.0;
	double dist_factor = 1.0;
	double dist_factor2 = 1.0; //Math.pow(3, 2/3);
	double tubeWeight = 1.0;
	//  boolean testPath = true;
	double weigthsExponent = 0.5;
	double minDspacing = 0.0;
	boolean isOptimizing = false;

	//  boolean useAllhklForCubic = false;
	boolean cubic = false;

	public MEMLTexture(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initXRD();
		identifier = "E-WIMV";
		IDlabel = "E-WIMV";
		description = "select this to apply the arbitrary grid E-WIMV method";
	}

	public MEMLTexture(XRDcat aobj) {
		this(aobj, "E-WIMV");
	}

	public MEMLTexture() {
		identifier = "E-WIMV";
		IDlabel = "E-WIMV";
		description = "select this to apply the arbitrary grid E-WIMV method";
	}

	public void initConstant() {
		Nstring = 16;
		Nstringloop = 0;
		Nparameter = 0;
		Nparameterloop = 0;
		Nsubordinate = 0;
		Nsubordinateloop = 0;
	}

	public void initDictionary() {
		System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
		System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
		System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
		System.arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
	}

	public void initParameters() {
		super.initParameters();

		setSampleSymmetry(NONE);
		setWIMVstatus(true);
		stringField[2] = MaudPreferences.getPref("ewimv.iterationsNumber", "10");
		stringField[3] = MaudPreferences.getPref(prefs[7], prefVal[7]);
		setMinimumIntensity(MaudPreferences.getPref("ewimv.minimumPFIntensity", "0.001"));
		setTubeWeight(MaudPreferences.getPref("ewimv.tubeWeightExponentValue", "0.5"));
		stringField[5] = "15.0";
		setResolution(MaudPreferences.getPref("ewimv.defaultCellValue", "15.0"));
		setResolution(MaudPreferences.getPref(prefs[0], prefVal[0]));
		useTubeProjection(MaudPreferences.getPref(prefs[4], prefVal[4]));
		storeConversion(MaudPreferences.getPref(prefs[6], prefVal[6]));
		normalizePoleFigures(MaudPreferences.getBoolean("ewimv.normalizePoleFigures", true));
		setODFcoverage("0");
//    usePhon(MaudPreferences.getPref("debug.usePhon", "false"));
		setWeightsExponent(MaudPreferences.getPref("ewimv.weigthsExponent", "0.5"));
//    odfnotLoaded = true;
		stringField[14] = "true";
		stringField[15] = MaudPreferences.getPref("ewimv.minimumDspacing", "0.0");
	}

	public void refreshForNotificationDown(XRDcat source, int reason) {
		if (!getFilePar().isComputingDerivate() || (source == this ||
				(reason == Constants.SAMPLE_ORIENTATION_CHANGED || (source == getParent() &&
						(reason == Constants.STRAIN_CHANGED || reason == Constants.CELL_CHANGED))))) {
			refreshComputation = true;
			//     System.out.println("Reason " + reason + " Source " + source.toXRDcatString());
		}
	}

	public void updateStringtoDoubleBuffering(boolean firstLoading) {
		super.updateStringtoDoubleBuffering(false);
		weigthsExponent = Double.parseDouble(stringField[13]);
		minDspacing = Double.parseDouble(getMinimumDspacing());
//    boolean useCubicSymmetry = MaudPreferences.getBoolean("debug.useCubicSymmetry", true);
		cubic = ((Phase) getParent()).isCubic(); // && useCubicSymmetry;
//    System.out.println("Update strings in EWIMV");
	}

	public String getWIMVOption(int i) {
		return stringField[i + 2];
	}

	public void setWIMVOption(int i, String value) {
		if (i == 0) {
			stringField[i + 2] = Integer.toString(Integer.parseInt(value));
		} else
			stringField[i + 2] = value;
	}

	public int getCyclesNumber() {
		return 1; // not needed
	}

	public int getNumberofIterations() {
		return Integer.parseInt(getWIMVOption(0));
	}

	public int prepareIteration() {
		return 0;
	}

	public OutputStream getResultStream() {
		return null;
	}

	public void endOfComputation() {
	}

	public boolean logOutput() {
		return false;
	}

	public void closeLogResultFile() {
	}

	public OptimizationAlgorithm getOptimizationAlgorithm() {
		return null;
	}

	public void fittingFileOutput() {
	}

	public void setNumberofIterations(int value) {
		setWIMVOption(0, Integer.toString(value));
	}

	public double getRexponent() {
		return Double.parseDouble(getWIMVOption(1));
	}

	public void setMinimumIntensity(String value) {
		stringField[4] = value;
	}

	public String getMinimumIntensity() {
		return stringField[4];
	}

	public double getMinimumIntensityD() {
		return Double.parseDouble(getMinimumIntensity());
	}

	public void setMinimumDspacing(String value) {
		stringField[15] = value;
	}

	public String getMinimumDspacing() {
		return stringField[15];
	}

	public double getMinimumDspacingD() {
		return minDspacing;
	}

	public void setResolution(String value) {
		double res = 0.0;
		try {
			res = Double.parseDouble(stringField[5]);
		} catch (Exception e) {
			e.printStackTrace();
		}
		if (value != null && Double.parseDouble(value) != res) {
			stringField[5] = value;
			resetODF();
		}
	}

	public String getResolution() {
		return stringField[5];
	}

	public double getResolutionD() {
//    System.out.println("res " + getResolution());
		return Double.parseDouble(getResolution());
	}

	public String getSampleSymmetry() {
		return stringField[0];
	}

	public void setSampleSymmetry(int i) {
		stringField[0] = symmetrychoice[i];
	}

	public void setSampleSymmetry(String value) {
		stringField[0] = value;
	}

	public int getEWIMVSampleSymmetryMultiplicity() {
		switch (getSampleSymmetryValue()) {
			case 0:
			case 1:
			case 2:
			case 3:
				return getSampleSymmetryValue() + 1;
			case 4:
				return 6;
			case 5:
				return 2;
			case 6:
				return 4;
			case 7:
				return 1;
			default: {
			}
		}
		return 1;
	}

	public boolean getWIMVstatus() {
		return stringField[1].equalsIgnoreCase("true");
	}

	public void setWIMVstatus(boolean status) {
		if (status)
			stringField[1] = "true";
		else
			stringField[1] = "false";
	}

	public void setWIMVstatus(String value) {
		stringField[1] = value;
	}

	public boolean ODFisRefinable() {
		return stringField[14].equalsIgnoreCase("true");
	}

	public void setODFrefinable(boolean status) {
		if (status)
			stringField[14] = "true";
		else
			stringField[14] = "false";
	}

	public void setODFrefinable(String value) {
		stringField[14] = value;
	}

	public boolean useTubeProjection() {
		return stringField[6].equalsIgnoreCase("true");
	}

	public void useTubeProjection(boolean status) {
		if (status)
			stringField[6] = "true";
		else
			stringField[6] = "false";
	}

	public void useTubeProjection(String value) {
		stringField[6] = value;
	}

	public boolean storeConversion() {
		return stringField[7].equalsIgnoreCase("true");
	}

	public void storeConversion(boolean status) {
		if (status)
			stringField[7] = "true";
		else
			stringField[7] = "false";
	}

	public void storeConversion(String value) {
		stringField[7] = value;
	}

	public void setODFcoverage(String value) {
		stringField[8] = value;
	}

	public String getODFcoverage() {
		return stringField[8];
	}

	public void setSharpness(String value) {
		setString(9, value);
	}

	public String getSharpness() {
		return getString(9);
	}

	public void setTubeWeight(String value) {
		stringField[11] = value;
	}

	public String getTubeWeight() {
		return stringField[11];
	}

	public double getTubeWeightD() {
		return Double.parseDouble(getTubeWeight());
	}

	public boolean normalizePoleFigures() {
		return stringField[12].equalsIgnoreCase("true");
	}

	public void normalizePoleFigures(boolean status) {
		if (status)
			normalizePoleFigures("true");
		else
			normalizePoleFigures("false");
	}

	public void normalizePoleFigures(String value) {
		stringField[12] = value;
	}

	public void setWeightsExponent(double value) {
		setWeightsExponent(Double.toString(value));
	}

	public double getWeightsExponent() {
		return weigthsExponent;
	}

	public String getWeightsExponentS() {
		return stringField[13];
	}

	public void setWeightsExponent(String value) {
		stringField[13] = value;
	}

	public int getNumberOfData() {
		return numberOfData;
	}

	public double getData(int index) {
		return dta_fix[index] * poleFactor[poleindex[index]];
	}

	public double getWeight(int index) {
		return wgt[index];
	}

	public double getFit(int index) {
		return fit[index];
	}

	public void setFit(int index, double value) {
		fit[index] = value;
	}

/*	public double[] getData() {
		return dta_fix;
	}*/

	public double[] getWeight() {
		return wgt;
	}

	public double[] getFit() {
		return fit;
	}

	public void refreshFit(double[] fit, double[] parm, int[] controls) {
	}

	public double[] getRefinementIndexes() {

		return null;

	}

	public double getWSS() {
		double WSS = 0.0;
		double diff;

		for (int i = 0; i < getNumberOfData(); i++) {
			if (!Double.isNaN(getData(i))) {
				diff = (getFit(i) - getData(i)) * getWeight(i);
//      System.out.println(i + " " + getFit(i) + " " + getData(i) + " " + getWeight(i));
				WSS += diff * diff;
			}
		}

		if (stillRandomODF)
			WSS *= 1000.0;
		stillRandomODF = false;

		return WSS;
	}

	public double getRw() {
		double rw = 0.0;
		double diff;
		double wgt_s;
		double dtat;
		double den = 0.0;
		int index = 0;
		int npole = getPoleFigureNumber();

		double[] singlePoleRw = new double[npole];
		double[] singleDen = new double[npole];

		for (int i = 0; i < npole; i++) {
//			Reflection reflex = getReflection(i, 0);
			int numberDta = 0; // LucaToDo getPointNumber(i);
			singlePoleRw[i] = 0.0;
			singleDen[i] = 0.0;
			for (int j = 0; j < numberDta; j++) {
				wgt_s = getWeight(index);
				dtat = getData(index);
//        System.out.println(npole + " " + j + " " + getFit(index)+" "+dtat+" "+wgt_s);
				if (!Double.isNaN(dtat))
					diff = (getFit(index) - dtat) * wgt_s;
				else
					diff = 0.0;
				singlePoleRw[i] += diff * diff;
				if (!Double.isNaN(dtat))
					singleDen[i] += dtat * dtat * wgt_s * wgt_s;
				index++;
			}
			rw += singlePoleRw[i];
			den += singleDen[i];
		}

		if (den != 0.0)
			rw /= den;
		rw = Math.sqrt(rw);

		for (int i = 0; i < npole; i++) {
			if (singleDen[i] != 0.0)
				singlePoleRw[i] /= singleDen[i];
			singlePoleRw[i] = Math.sqrt(singlePoleRw[i]);
		}

		setRw(rw);
		setRw(singlePoleRw);

		return rw;
	}

	public double getR() {
		double r = 0.0;
		double dtat;
		double den = 0.0;
		int index = 0;
		int npole = getPoleFigureNumber();

		double[] singlePoleR = new double[npole];
		double[] singleDen = new double[npole];

		for (int i = 0; i < npole; i++) {
//			Reflection reflex = getReflection(i, 0);
			int numberDta = 0; // LucaToDo getPointNumber(i);
			singlePoleR[i] = 0.0;
			singleDen[i] = 0.0;
			for (int j = 0; j < numberDta; j++) {
				dtat = getData(index);
				if (!Double.isNaN(dtat)) {
					singlePoleR[i] += Math.abs(getFit(index) - dtat);
					singleDen[i] += dtat;
				}
				index++;
			}
			r += singlePoleR[i];
			den += singleDen[i];
		}

		if (den != 0.0)
			r /= den;

		for (int i = 0; i < npole; i++) {
			if (singleDen[i] != 0.0)
				singlePoleR[i] /= singleDen[i];
		}

		setR(r);
		setR(singlePoleR);

		return r;
	}

	public void setRw(double Rw) {
	}

	public void setRw(double[] Rw) {
		int endofline5 = 0;
		int izpol = Rw.length;

		StringBuffer tmp = new StringBuffer("  NORMFAKs : ");
		for (int npolex = 0; npolex < izpol; npolex++) {
			endofline5++;
			tmp.append(Misc.getDoubleStringFormatted(poleFactor[npolex], 5, 6));
			if (endofline5 == 5) {
				System.out.println(tmp.toString());
				endofline5 = 0;
				tmp = new StringBuffer("  NORMFAKs : ");
			}
		}
		if (endofline5 != 0)
			System.out.println(tmp.toString());

		endofline5 = 0;

		tmp = new StringBuffer("  RWPFAKs : ");
		for (int npolex = 0; npolex < izpol; npolex++) {
			endofline5++;
			tmp.append(Misc.getDoubleStringFormatted(Rw[npolex] * 100, 5, 6));
			if (endofline5 == 5) {
				System.out.println(tmp.toString());
				endofline5 = 0;
				tmp = new StringBuffer("  RWPFAKs : ");
			}
		}
		if (endofline5 != 0)
			System.out.println(tmp.toString());
	}

	public void setR(double R) {
	}

	public void setR(double[] R) {
		int endofline5 = 0;
		int izpol = R.length;

		StringBuffer tmp = new StringBuffer("   RPFAKs : ");
		for (int npolex = 0; npolex < izpol; npolex++) {
			endofline5++;
			tmp.append(Misc.getDoubleStringFormatted(R[npolex] * 100, 5, 6));
			if (endofline5 == 5) {
				System.out.println(tmp.toString());
				endofline5 = 0;
				tmp = new StringBuffer("   RPFAKs : ");
			}
		}
		if (endofline5 != 0)
			System.out.println(tmp.toString());
	}

	public void setRexp(double R) {
	}

	public double getSS() {
		double SS = 0.0;
		double diff;

		for (int i = 0; i < getNumberOfData(); i++) {
			if (!Double.isNaN(getData(i))) {
				diff = getFit(i) - getData(i);
				SS += diff * diff;
			}
		}

		return SS;
	}

	public int getNumberOfFreeParameters() {
		return numberOfParameters;
	}

	public double[] getFreeParameters(boolean initialCall) {
		double[] parameters = new double[numberOfParameters];

		for (int ng = 0; ng < nge; ng++)
			for (int nb = 0; nb < nbe; nb++)
				for (int na = 0; na < nae; na++) {
//    System.out.println("rr " + na + " " + nb + " " + ng);
					parameters[ODFindex(na, nb, ng)] = odf[na][nb][ng];
				}

		return parameters;
	}

	public void setFreeParameters(double[] parm) {
		for (int ng = 0; ng < nge; ng++)
			for (int nb = 0; nb < nbe; nb++)
				for (int na = 0; na < nae; na++) {
					double odfValue = parm[ODFindex(na, nb, ng)];
					if (odfValue >= 0.0) {
						odf[na][nb][ng] = odfValue;
						odf_covered[na][nb][ng] = true;
					} else {
						odf[na][nb][ng] = -odfValue;
						odf_covered[na][nb][ng] = false;
					}
				}
	}

	public double getFreeParameter(int index) {
		return 0.0;
	}

	public void setFreeParameter(int index, double value) {
	}

	public boolean singleFunctionComputing() {
		return false;
	}

	public void saveparameters() {
	}

	public void resetODF() {

		textureInitialization();
		odf = new double[alphama][betama][alphama];
		odf_covered = new boolean[alphama][betama][alphama];
//    System.out.println("ODF reset");

		for (int ng = 0; ng < alphama; ng++)
			for (int nb = 0; nb < betama; nb++)
				for (int na = 0; na < alphama; na++) {
					odf[na][nb][ng] = 1.0f;
					odf_covered[na][nb][ng] = false;
				}

		//odfnotLoaded = false;
	}

	public void normalizeFit() {
		if (!normalizePoleFigures())
			return;

		fiottu();
		odfNormalization();

		computeFit(getFreeParameters(false));

		if (getNumberOfData() == 0)
			return;
		int actualIndex = -1;
		int lastIndex;
		poleFactor[0] = 0.0f;
		double dataSum = 0.0;
		double fitSum = 0.0;
		for (int i = 0; i < getNumberOfData(); i++) {
			if (poleindex[i] != actualIndex) {
				lastIndex = actualIndex;
				actualIndex = poleindex[i];
				if (lastIndex != -1) {
					if (dataSum > 0)
						poleFactor[lastIndex] = fitSum / dataSum;
					else
						poleFactor[lastIndex] = 1.0;
					poleFactor[actualIndex] = 0.0;
					fitSum = 0.0;
					dataSum = 0.0;
				}
			}
			if (wgt[i] > 0.0 && !Double.isNaN(dta_fix[i])) {
				if (fit[i] > 0.0)
					fitSum += fit[i];
				dataSum += dta_fix[i];
			}
		}
		if (actualIndex != -1) {
			if (dataSum > 0)
				poleFactor[actualIndex] = fitSum / dataSum;
			else
				poleFactor[actualIndex] = 1.0f;
		}
	}

	public void computeFit() {
    ArrayList<double[]> textF = recomputedTextureFactor(getPhase(), getFilePar().getActiveSample(), false);
		int index = 0;
		int npole = getPoleFigureNumber();
		for (int i = 0; i < npole; i++) {
			Reflection reflex = null; // LucaToDo getReflection(i);
			double[] textFactors = textF.get(i);
			int numberDta = textFactors.length;
			for (int j = 0; j < numberDta; j++) {
				int izoveri = reflex.izoveri;
				double texturefactor = 0.0;
				for (int ih = 0; ih < izoveri; ih++) {
// LucaToDo 					if (!Double.isNaN(textF[getPoint(reflex, j)][poleFigureIndex[i] + ih]))
// LucaToDo 						texturefactor += textF[getPoint(reflex, j)][poleFigureIndex[i] + ih] * getOverlappedWeight(i, ih);
				}
				fit[index++] = texturefactor;
			}
		}

	}

	public void computeFit(double[] parmn) {

		int npole = getPoleFigureNumber();

		final int maxThreads = Math.min(Constants.maxNumberOfThreads, npole);
		int checkNumber = getNumberOfData();
		if (baseDataIndex != null && maxThreads > 1 && checkNumber > 5000 &&
				Constants.threadingGranularity >= Constants.FINE_GRANULARITY) {
			if (Constants.debugThreads)
				System.out.println("Thread EWIMV computeFit " + getLabel());
			int i;
			PersistentThread[] threads = new PersistentThread[maxThreads];
			for (i = 0; i < maxThreads; i++) {
				final double[] parmnt = parmn;
				final MEMLTexture lock = this;
				threads[i] = new PersistentThread(i) {
					@Override
					public void executeJob() {
						int i1 = this.getJobNumberStart();
						int i2 = this.getJobNumberEnd();

						for (int i = i1; i < i2; i++) {
							int numberDta = 0; // LucaToDo getPointNumber(i);
							double[] texturefactor = new double[numberDta];
							int index = baseDataIndex[i];
							for (int j = 0; j < numberDta; j++) {
								int[] cellID = getMEMCellID(index);
								double[] cellWGT = getMEMCellWGT(index++);
								int cellNumber = cellID.length;
								if (cellWGT == null)
									for (int k = 0; k < cellNumber; k++)
										texturefactor[j] += Math.abs(parmnt[cellID[k]]);
								else
									for (int k = 0; k < cellNumber; k++)
										texturefactor[j] += Math.abs(parmnt[cellID[k]] * cellWGT[k]);
							}
							synchronized (lock) {
								index = baseDataIndex[i];
								for (int j = 0; j < numberDta; j++) {
									fit[index++] = texturefactor[j];
								}
							}
						}

					}
				};
			}
			i = 0;
			int istep = (int) (0.9999 + npole / maxThreads);
			for (int j = 0; j < maxThreads; j++) {
				int is = i;
				if (j < maxThreads - 1)
					i = Math.min(i + istep, npole);
				else
					i = npole;
				threads[j].setJobRange(is, i);
				threads[j].start();
			}
			boolean running;
			do {
				running = false;
				try {
					Thread.sleep(Constants.timeToWaitThreadsEnding);
				} catch (InterruptedException r) {
				}
				for (int h = 0; h < maxThreads; h++) {
					if (!threads[h].isEnded())
						running = true;
				}
			} while (running);

		} else {
			int index = 0;
			for (int i = 0; i < npole; i++) {
				int numberDta = 0; // LucaToDo getPointNumber(i);
				for (int j = 0; j < numberDta; j++) {
//				  int index = baseDataIndex[i] + j;
					double texturefactor = 0.0;
					int[] cellID = getMEMCellID(index);
					double[] cellWGT = getMEMCellWGT(index);
					int cellNumber = cellID.length;
					if (cellWGT == null)
						for (int k = 0; k < cellNumber; k++)
							texturefactor += Math.abs(parmn[cellID[k]]);
					else
						for (int k = 0; k < cellNumber; k++)
							texturefactor += Math.abs(parmn[cellID[k]] * cellWGT[k]);
					fit[index++] = texturefactor;
				}
			}
		}

	}

	public void odfNormalization() {
		double vbg, va, hvg, hvb, a = 0., b;
		double fn;
		int[] tmp_index = new int[3];

		double fnorm = 0.0f;
		double roundOffCorrection = 0.0;
		double coverageCorrection = 0.0;
		double totalCorrection = 0.0;

//    boolean negODFout = false;
//    if (Constants.testing)
//      negODFout = getBoolean("debug.negativeODFout", false);

		for (int ng = 0; ng < alphama; ng++) {

			hvg = resolutionR;
			if (ng == 0 || ng == alphama1)
				hvg /= 2;

			for (int nb = 0; nb < betama; nb++) {
				if (nb == 0) {
					a = 0.0;
					b = pi25g;
				} else if (nb == betama1) {
					b = nb * resolutionR;
					a -= pi25g;
				} else {
					a = nb * resolutionR - pi25g;
					b = a + resolutionR;
				}
				hvb = Math.cos(a) - Math.cos(b);
				vbg = hvb * hvg;

				for (int na = 0; na < alphama; na++) {
					if (na == 0 || na == alphama1)
						va = pi25g * vbg;
					else
						va = resolutionR * vbg;
					tmp_index[0] = na;
					tmp_index[1] = nb;
					tmp_index[2] = ng;
// l_last1202    			applyCrystalSymmetryAndCheck(tmp_index);
// l_last1202					fn = odf[tmp_index[0]][tmp_index[1]][tmp_index[2]];
					fn = getODF(tmp_index);
					if (odf_covered[tmp_index[0]][tmp_index[1]][tmp_index[2]]) {
						fnorm += fn * va;
						roundOffCorrection += va;
						coverageCorrection += va;
					} else {
//	  	  		fnorm -= fn * va;
//	  	  		roundOffCorrection += va;
					}
					totalCorrection += va;
				}

			}
		}

//    double fnormTheoretical = Constants.PI * 8. * Constants.PI / fnorm;
		fnorm = roundOffCorrection / fnorm;

		for (int ng = 0; ng < alphama; ng++)
			for (int nb = 0; nb < betama; nb++)
				for (int na = 0; na < alphama; na++)
					if (odf_covered[na][nb][ng]) {
						odf[na][nb][ng] *= fnorm;
//            odf_covered[na][nb][ng] = true;
					}

		double ODFcoverage = coverageCorrection / totalCorrection * 100.0;
		setODFcoverage(Fmt.format(ODFcoverage));
		System.out.println("ODF coverage: " + Fmt.format(ODFcoverage) + " %, Normalization factor: " +
				Fmt.format(fnorm));
//	  System.out.println("Normalization factor: " + Fmt.format(fnormTheoretical));
	}

	public boolean checkBound(int j, double parmn) {
//    boolean bound = false;
//    if (parmn < 0.0)
//      bound = true;

		return false; //bound;
	}

	public void setErrors(double[] errors) {
	}

	public void computeFirstFit() {
	}

	public void backupallParameters() {
	}

	public void restoreParametersValues() {
	}

	public void setDerivate(boolean value) {
	}

	public void setOptimizing(boolean value) {
		isOptimizing = value;
	}

	public boolean isOptimizing() {
		return isOptimizing;
	}

	public void mainfunction(boolean hasoutput, boolean refreshAll) {
		computeFit();
	}

	public boolean reduceMemory() {
		return reduceMemory;
	}

	static int maxindex = 10000;
	Vector<double[]> wgtContainer = new Vector<double[]>();

	public int prepareiteration(Sample asample) {

		refreshFit = true;

		Rw = 0.0;
		R = 0.0;

//  	FilePar aparFile = (FilePar) asample.getFilePar();

		textureInitialization();

//		initializeReflexes(asample);

		Phase phase = getPhase();

		numberOfData = 0;
		baseDataIndex = new int[getPoleFigureNumber()];
		for (int i = 0; i < getPoleFigureNumber(); i++) {
			baseDataIndex[i] = numberOfData;
			numberOfData += 0; // LucaToDo getPointNumber(i);
			// asample.getNumberOfTexturePoints(phase, poleFigureIndex[i]) * getEWIMVSampleSymmetryMultiplicity();
		}

		System.out.println("Number of PFs: " + getPoleFigureNumber() + ", and data number: " + getNumberOfData());

		dta_fix = new double[getNumberOfData()];
		wgt = new double[getNumberOfData()];
		fit = new double[getNumberOfData()];
		poleindex = new int[getNumberOfData()];
		pointindex = new int[getNumberOfData()];
		datasetindex = new int[getNumberOfData()];
		datafileindex = new int[getNumberOfData()];

		int index = 0;
		for (int i = 0; i < getPoleFigureNumber(); i++) {
			for (int k = 0; k < getEWIMVSampleSymmetryMultiplicity(); k++)
				for (int d = 0; d < asample.activeDatasetsNumber(); d++) {
					DataFileSet dataset = asample.getActiveDataSet(d);
					for (int dd = 0; dd < dataset.activedatafilesnumber(); dd++) {
						datasetindex[index] = d;
						datafileindex[index++] = dd;
					}
				}
		}
//	  symmetryindex = new int[getNumberOfData()];
		poleFactor = new double[getPoleFigureNumber()];

		index = 0;
		for (int i = 0; i < getPoleFigureNumber(); i++) {
			poleFactor[i] = 1.0f;
			int numberDta = 0; // LucaToDo getPointNumber(i);
			for (int j = 0; j < numberDta; j++) {
				DiffrDataFile datafile = null; // LucaToDo getPointFromAll(j);
				if (datafile.isPeakInsideRange(phase, poleFigureIndex[i])) {  // todo: v3.0 for all radiations?
					dta_fix[index] = 0; // LucaToDo getPoleIntensity(i, j);
					wgt[index] = 0; // LucaToDo getWeight(i, j);
				} else {
					dta_fix[index] = 0;
					wgt[index] = 0;
				}

				poleindex[index] = i;
//	      symmetryindex = new int[getNumberOfData()];
				pointindex[index++] = j;
			}
		}

		for (int i = 0; i < totalWeight.length; i++) {
			totalWeight[i] = 0.0f;
			totalHits[i] = 0;
		}

		maxindex = nae * nbe * nge;
		if (wgtContainer.size() < 1 || wgtContainer.elementAt(0).length != maxindex) {
			if (wgtContainer.size() > 0)
				wgtContainer.removeAllElements();
			wgtContainer.add(new double[maxindex]);
		}

		if (!reduceMemory()) {
//      wgtcellt = new double[maxindex];
//      cellidt = new int[maxindex];
			totCellID = new Vector(getNumberOfData(), 1);
			totCellWGT = new Vector(getNumberOfData(), 1);

			for (int j = 0; j < getNumberOfData(); j++) {
				totCellID.addElement(new int[0]);
				totCellWGT.addElement(new double[0]);
			}

			ProgressFrame prF = null;
			if (!Constants.textonly && Constants.showProgressFrame)
				try {
					prF = new ProgressFrame(getNumberOfData());
				} catch (NullPointerException npe) {
					System.out.println("Not able to create frame, MacOSX display sleep bug?");
				}
			printf("Preparing for odf cells angles conversion...            ", prF);

			final int maxThreads = Math.min(Constants.maxNumberOfThreads, getNumberOfData());
			int checkNumber = getNumberOfData();
			if (maxThreads > 1 && checkNumber > 100 &&
					Constants.threadingGranularity >= Constants.MEDIUM_GRANULARITY) {
				if (Constants.debugThreads)
					System.out.println("Thread EWIMV cell and weights " + getLabel());
				int i;
				PersistentThread[] threads = new PersistentThread[maxThreads];
				for (i = 0; i < maxThreads; i++) {
					if (wgtContainer.size() < (i + 1) || wgtContainer.elementAt(i).length != maxindex) {
						if (wgtContainer.size() > i) {
							if (wgtContainer.elementAt(i).length != maxindex)
								wgtContainer.setElementAt(new double[maxindex], i);
						} else
							wgtContainer.add(new double[maxindex]);
					}
					final ProgressFrame finalPrF = prF;
					threads[i] = new PersistentThread(i) {
						@Override
						public void executeJob() {
							int i1 = this.getJobNumberStart();
							int i2 = this.getJobNumberEnd();

							for (int j = i1; j < i2; j++) {
								if (!Double.isNaN(dta_fix[j])) {
									Vector cellAndWgt = null;
									if (useTubeProjection())
										cellAndWgt = computeCellAndWeight(j, wgtContainer, this.threadNumber);
									else
										cellAndWgt = computeCellAndWeightS(j);
									setCellElement(j, cellAndWgt);
								} else {
									setCellElement(j, null);
								}
								if (finalPrF != null)
									finalPrF.increaseProgressBarValue();
							}
						}
					};
				}
				i = 0;
				int istep = (int) (0.9999 + getNumberOfData() / maxThreads);
				for (int j = 0; j < maxThreads; j++) {
					int is = i;
					if (j < maxThreads - 1)
						i = Math.min(i + istep, getNumberOfData());
					else
						i = getNumberOfData();
					threads[j].setJobRange(is, i);
					threads[j].start();
				}
				boolean running;
				do {
					running = false;
					try {
						Thread.sleep(Constants.timeToWaitThreadsEnding);
					} catch (InterruptedException r) {
					}
					for (int h = 0; h < maxThreads; h++) {
						if (!threads[h].isEnded())
							running = true;
					}
				} while (running);

			} else {
				for (int j = 0; j < getNumberOfData(); j++) {
					if (!Double.isNaN(dta_fix[j])) {
						Vector cellAndWgt;
						if (useTubeProjection())
							cellAndWgt = computeCellAndWeight(j, wgtContainer, 0);
						else
							cellAndWgt = computeCellAndWeightS(j);
						totCellID.setElementAt(cellAndWgt.elementAt(0), j);
						totCellWGT.setElementAt(cellAndWgt.elementAt(1), j);
					}
					if (prF != null)
						prF.increaseProgressBarValue();
				}
			}
//      wgtcellt = null;
//      cellidt = null;
			if (prF != null) {
				prF.setVisible(false);
				prF.dispose();
			}
//			prF = null;
			if (totCellID.size() < getNumberOfData())
				System.out.println("Warning: not all the cellID are filled");
		}

		return numberOfParameters;
	}

	private void setCellElement(int index, Vector cellAndWgt) {
		synchronized (this) {
			if (cellAndWgt != null) {
				totCellID.setElementAt(cellAndWgt.elementAt(0), index);
				totCellWGT.setElementAt(cellAndWgt.elementAt(1), index);
			}
		}
	}

	public int prepareiteration(double[][] experimentalPF) {

		refreshFit = true;

		Rw = 0.0;
		R = 0.0;

//  	FilePar aparFile = (FilePar) getFilePar();

		textureInitialization();

		Phase aphase = getPhase();
		cdsc = aphase.lattice();
		numberPoleFigures = experimentalPF.length;
//    numberOfPFPoint = new int[getPoleFigureNumber()];

		numberOfData = 0;
		for (int i = 0; i < getPoleFigureNumber(); i++) {
			numberOfData += 0; // LucaToDo getPointNumber(i);
		}

//		System.out.println("Number of PFs : " + getPoleFigureNumber());

		dta_fix = new double[getNumberOfData()];
		wgt = new double[getNumberOfData()];
		fit = new double[getNumberOfData()];
		poleindex = new int[getNumberOfData()];
		pointindex = new int[getNumberOfData()];
		datasetindex = null;
		datafileindex = null;
//	  symmetryindex = null;
		poleFactor = new double[getPoleFigureNumber()];

		int index = 0;
		for (int i = 0; i < getPoleFigureNumber(); i++) {
			poleFactor[i] = 1.0f;
			int numberDta = 0; // LucaToDo getPointNumber(i);
			for (int j = 0; j < numberDta; j++) {
				dta_fix[index] = experimentalPF[i][j];
				wgt[index] = 1.0f;
				poleindex[index] = i;
				pointindex[index++] = j;
			}
		}

		for (int i = 0; i < totalWeight.length; i++) {
			totalWeight[i] = 0.0;
			totalHits[i] = 0;
		}
		maxindex = nae * nbe * nge;
		if (wgtContainer.size() < 1 || wgtContainer.elementAt(0).length != maxindex) {
			if (wgtContainer.size() > 0)
				wgtContainer.removeAllElements();
			wgtContainer.add(new double[maxindex]);
		}
		if (!reduceMemory()) {
			totCellID = new Vector(getNumberOfData(), 1);
			totCellWGT = new Vector(getNumberOfData(), 1);

			ProgressFrame prF = null;
			if (!Constants.textonly && Constants.showProgressFrame)
				try {
					prF = new ProgressFrame(getNumberOfData());
				} catch (NullPointerException npe) {
					System.out.println("Not able to create frame, MacOSX display sleep bug?");
				}
			printf("Preparing for odf cells angles conversion...            ", prF);
			for (int j = 0; j < getNumberOfData(); j++) {
				if (!Double.isNaN(dta_fix[j])) {
					Vector cellAndWgt = null;
					if (useTubeProjection())
						cellAndWgt = computeCellAndWeight(j, wgtContainer, 0);
					else
						cellAndWgt = computeCellAndWeightS(j);
					totCellID.addElement(cellAndWgt.elementAt(0));
					totCellWGT.addElement(cellAndWgt.elementAt(1));
				} else {
					totCellID.addElement(new int[0]);
					totCellWGT.addElement(new double[0]);
				}
				if (prF != null)
					prF.increaseProgressBarValue();
			}
			if (prF != null) {
				prF.setVisible(false);
				prF.dispose();
			}
//			prF = null;
			if (totCellID.size() < getNumberOfData())
				System.out.println("Warning: not all the cellID are filled");
		}

		return numberOfParameters;
	}

	public int textureInitialization() {
		nfismax = (int) (Constants.PI2 / integrationStepPFR + 1.000001);
		numberOfParameters = super.textureInitialization();
		reduceMemory = !storeConversion();

		sampleSymmetryValue = computeSampleSymmetryValue();

		dist_factor = resolution * 2.0;
		tubeWeight = getTubeWeightD();
		dist_factor2 = dist_factor * dist_factor;

		phiturn = getDouble(prefs[2], Double.parseDouble(Texture.prefVal[2]));
		if (phiturn <= 0.0)
			phiturn = resolution;

		if (Constants.testing) {
			testCollapse = getBoolean("debug.testCollapse", testCollapse);
		}

		if (totalWeight == null || totalWeight.length != numberOfParameters) {
			totalWeight = new double[numberOfParameters];
			totalHits = new int[numberOfParameters];
		}
		return numberOfParameters;
	}

/*	public int getPointNumber(int pole) {
    int number = numberOfPFPoint[pole];
    if (!fromPF)
      number *= getEWIMVSampleSymmetryMultiplicity();
    return number;
	}

	public Reflection getReflection(int pole, int partial) {
		return getPhase().getReflectionVector().elementAt(poleFigureIndex[pole] + partial);
	}

	public double[] getTextureAngles(int pole, int point) {
		if (fromPF) {
			double[] angles = new double[2];
			angles[0] = textureAngles[0][pole][point];
			angles[1] = textureAngles[1][pole][point];
			return angles;
		}

		Reflection reflex = getReflection(pole, 0);
		int np = getFilePar().getActiveSample().getNumberActiveDatafiles();
//		System.out.println(np);
		return applySampleSymmetry(reflex, point, np);
	}

	public int getPoint(Reflection reflex, int point) {
		int truepointmax = getFilePar().getActiveSample().getNumberActiveDatafiles();
		int sector = point / truepointmax;
		int residual = point - sector * truepointmax;
		return residual;
	}

	public DiffrDataFile getPointFromAll(int point) {
		Sample asample = getFilePar().getActiveSample();
		int truepointmax = asample.getNumberActiveDatafiles();
		int sector = point / truepointmax;
		int residual = point - sector * truepointmax;
		return asample.getActiveDiffrDataFile(residual);
	}

	public int getPoint(int reflexindex, int point) {
		Reflection reflex = getReflection(reflexindex, 0);
		return getPoint(reflex, point);
	}

	public double[] applySampleSymmetry(Reflection reflex, int point, int truepointmax) {
		int sector = point / truepointmax;
		int residual = point - sector * truepointmax;

		double[] angles = getFilePar().getActiveSample().getActiveTextureAngles(reflex, residual);
//		System.out.println(residual + " " +actualsample+" "+angles[0] +" "+ angles[1]);

		if (angles[0] < 0) {
			angles[0] = -angles[0];
			angles[1] += 180.0f;
		}
		while (angles[0] >= 360.0)
			angles[0] -= 360.0f;
		if (angles[0] >= 180.0) {
			angles[0] -= 180.0f;
			angles[1] += 180.0f;       // to test
		}
		if (angles[0] >= 90.0)
			angles[0] = 180.0f - angles[0];
		while (angles[1] < 0.0)
			angles[1] += 360.0f;
		while (angles[1] >= 360.0)
			angles[1] -= 360.0f;

		if (truepointmax > point)
			return angles;

		int fold;
		double lphiturn;

		switch (getSampleSymmetryValue()) {
			case 1:
			case 2:
			case 3:
				fold = getSampleSymmetryValue() + 1;
				lphiturn = 360f / fold;
				angles[1] += sector * lphiturn;
				break;
			case 4:
				fold = 6;
				lphiturn = 360f / fold;
				angles[1] += sector * lphiturn;
				break;
			case 5:
				angles[1] = 360.f - angles[1];
				break;
			case 6: // orthorhombic
				switch (sector) {
					case 0:
						break;
					case 1:
						angles[1] = 180.f - angles[1];
						break;
					case 2:
						angles[1] = 180.f + angles[1];
						break;
					case 3:
						angles[1] = 360.f - angles[1];
						break;
					default: {
					}
				}
				break;
			case 7:
//				fold = 72;
//				phiturn = 360f / fold;
				angles[1] += sector * phiturn;
				break;
			default: {
			}
		}
		while (angles[1] < 0)
			angles[1] += 360.0f;
		while (angles[1] >= 360.0)
			angles[1] -= 360.0f;

		return angles;
	}

	public double getPoleIntensity(int pole, int point) {
		int izoveri = getIzoveri(pole);
		double texturefactor = 0.0;
		Phase phase = getPhase();
		for (int i = 0; i < izoveri; i++) {
			int reflexIndex = poleFigureIndex[pole] + i;
//		 	int mult = reflex.multiplicity;
			texturefactor += getPointFromAll(point).getExperimentalTextureFactor(phase, reflexIndex, 0) *            // todo: v3.0 for all radiations?
					phase.getReflex(reflexIndex).getOverlappedWeight(); // * mult;
		}
		return texturefactor;
	}

	public double getWeight(int pole, int point) {
//		if (!useIntensityWeigth())
//			return 1.0;
		int izoveri = getIzoveri(pole);
		double wgt = 0.0;
		for (int i = 0; i < izoveri; i++) {
			Reflection reflex = getReflection(pole, i);
//      int mult = reflex.multiplicity;
//			System.out.println(reflex.getH() + " " + reflex.getK() + " " + reflex.getL() + ", weight(" + i + "): " + reflex.getWeight());
			wgt += reflex.getWeight();// * mult;
		}
		return Math.pow(wgt, getWeightsExponent()); // (wgt / izoveri);
		// // we decrease the weight of the superposed reflections by an additional factor
//		return 1.0f;
	}

	public int getH(int pole) {
		return getReflection(pole, 0).getH();
	}

	public int getK(int pole) {
		return getReflection(pole, 0).getK();
	}

	public int getL(int pole) {
		return getReflection(pole, 0).getL();
	}

	public int getIzoveri(int pole) {
		if (fromPF)
			return izoveriPF[pole];
		return getReflection(pole, 0).izoveri;
	}

	public int getH(int pole, int partial) {
		return getReflection(pole, partial).getH();
	}

	public int getK(int pole, int partial) {
		return getReflection(pole, partial).getK();
	}

	public int getL(int pole, int partial) {
		return getReflection(pole, partial).getL();
	}

	public double getOverlappedWeight(int pole, int partial) {
		if (fromPF)
			return weightSingle[pole][partial];

		Reflection reflex = getReflection(pole, partial);
		return reflex.getOverlappedWeight();
	}

	public int[][] gethklList(int pole, int i) {

		Reflection reflex = null;
		if (!fromPF)
			reflex = getReflection(pole, i);
		int mult;
		mult = 1;
		int mult2 = mult / 2;

		int[][] hkllist = new int[3][mult];

		for (int imult = 0; imult < mult; imult++) {
			if (imult < mult2 || mult == 1) {
				if (!fromPF) {
					hkllist[0][imult] = reflex.hlist[imult];
					hkllist[1][imult] = reflex.klist[imult];
					hkllist[2][imult] = reflex.llist[imult];
				} else {
					hkllist[0][imult] = hklPF[0][imult][pole][i];
					hkllist[1][imult] = hklPF[1][imult][pole][i];
					hkllist[2][imult] = hklPF[2][imult][pole][i];
				}
			} else {
				if (!fromPF) {
					hkllist[0][imult] = -reflex.hlist[imult - mult2];
					hkllist[1][imult] = -reflex.klist[imult - mult2];
					hkllist[2][imult] = -reflex.llist[imult - mult2];
				} else {
					hkllist[0][imult] = -hklPF[0][imult - mult2][pole][i];
					hkllist[1][imult] = -hklPF[1][imult - mult2][pole][i];
					hkllist[2][imult] = -hklPF[2][imult - mult2][pole][i];
				}
			}
		}
		return hkllist;
	}
*/

	public int[] getMEMCellID(int dataindex) {
		if (!reduceMemory()) {
			return (int[]) totCellID.elementAt(dataindex);
		}
		return (int[]) computeCellAndWeight(dataindex, wgtContainer, 0).elementAt(0);
	}

	public double[] getMEMCellWGT(int dataindex) {
		if (!reduceMemory()) {
			return (double[]) totCellWGT.elementAt(dataindex);
		}
		return (double[]) computeCellAndWeight(dataindex, wgtContainer, 0).elementAt(1);
	}

	public double[] getMEMCountData() {
		return totalWeight;
	}

	public int[] getHitsCountData() {
		return totalHits;
	}

	public double[] getODFCoverage() {
		double[] weightsODF = getMEMCountData();
		double[] coverage = new double[weightsODF.length];
		double min = 1000000000.0f;
		double max = 0.0f;
		for (int i = 0; i < weightsODF.length; i++) {
			coverage[i] = weightsODF[ODFindexAndCheck(i)];
			if (coverage[i] < min)
				min = coverage[i];
			if (coverage[i] > max)
				max = coverage[i];
		}
		System.out.println("ODF coverage by weights, min = " + min + " , max = " + max);
		return coverage;
	}

	public double[] getODFCoverageByHits() {
		int[] weightsODF = getHitsCountData();
		double[] coverage = new double[weightsODF.length];
		double min = 1000000000.0f;
		double max = 0.0f;
		for (int i = 0; i < weightsODF.length; i++) {
			coverage[i] = weightsODF[ODFindexAndCheck(i)];
			if (coverage[i] < min)
				min = coverage[i];
			if (coverage[i] > max)
				max = coverage[i];
		}
		System.out.println("ODF coverage by hits, min = " + min + " , max = " + max);
		return coverage;
	}

	/**
	 * Plot an hystogram of the ODF cell weighted hits.
	 *
	 * @param aFrame the parent frame
	 */

	public void plotODFWeightsHystogram(Frame aFrame) {
		(new PlotSimpleData(aFrame, getODFCoverage())).setVisible(true);
	}

	public void plotODFHitsHystogram(Frame aFrame) {
		(new PlotSimpleData(aFrame, getODFCoverageByHits())).setVisible(true);
	}

	public void plotODFHitsMap() {

		int is15 = (int) (Math.sqrt(nbe) * 800.0 / 600.0);
		int columns = is15;
		int rows = nbe / is15 + 1;
		if (nbe < is15)
			columns = nbe;
		int width = columns * nae + columns - 1;
		int height = rows * nge + rows - 1;
		double[][] mapToPlot = new double[width][height];
		for (int i = 0; i < width; i++)
			for (int n = 0; n < height; n++)
				mapToPlot[i][n] = ColorMap.DUMMY_VALUE;
		width = 0;
		height = 0;
		int row = 0;
		int column = 0;
		double IntensityMin = 0.0f;
		double IntensityMax = 0.0f;
		double[] ODFcoverage = getODFCoverage();
		for (int i = 0; i < nbe; i++) {
			for (int n = 0; n < nae; n++) {
				for (int m = 0; m < nge; m++) {
					mapToPlot[width + n][height + m] = ODFcoverage[ODFindex(n, m, i)];
					if (IntensityMax < mapToPlot[width + n][height + m])
						IntensityMax = (double) mapToPlot[width + n][height + m];
				}
			}
			column++;
			if (column >= is15) {
				row++;
				column = 0;
			}
			width = column * (nae + 1);
			height = row * (nge + 1);
		}
		String title = "ODF hits map for " + getParent().toXRDcatString();
		new ODFMapPlot(new Frame(), mapToPlot, title,
				IntensityMin, IntensityMax, 0.0f, odfMaxAngles[0]);
	}

	boolean testCollapse = true;

	public Vector computeCellAndWeightS(int dataindex) {
//		double dist_a, dist_b, dist_g, real_dist, als, bets, gam;
		int k;
		boolean collapse;
//		int stepa, stepb, stepg, maxa, maxb, maxg;
		int[] findex = new int[3];
		double[] tmp_angles = new double[3];
		boolean cubic7 = getSampleSymmetryValue() == 7;

		Vector result = new Vector(2, 2);

/*	  if (dataindex == lastindex)
      return;
    lastindex = dataindex;*/
		int pole = poleindex[dataindex];
		int point = pointindex[dataindex];

		Phase phase = getPhase();
		Sample sample = phase.getSample();
		boolean checkIn = true;

		if (!fromPF) {
			DiffrDataFile diffrDataFile = sample.getActiveDataSet(datasetindex[dataindex]).getActiveDataFile(datafileindex[dataindex]);
			checkIn = diffrDataFile.isPeakInsideRange(phase, poleFigureIndex[pole]);
		}

		if (checkIn) {
			double[] textAngles = null; // LucaToDo getTextureAngles(pole, point);

//		System.out.println(pole + " " + point+" "+textAngles[0] + " " + textAngles[1]);

			Vector cellIDandWGT = new Vector(0, 2);

			int izoveri = 0; // LucaToDo getIzoveri(pole);

			int totalcell = 0;

			int tk = 0;
			double[] wgtcellt = new double[maxindex];
			int[] cellidt = new int[maxindex];

			double[][] odfpath;

			double totReflexWeigth = 1.0;
			for (int i = 0; i < izoveri; i++) {
				int[][] hklList = null; // LucaToDo gethklList(pole, i);

				int mult = hklList[0].length;
				double oneOverMult = 1.0 / mult;

				double overlappingWgt = 0; // LucaToDo getOverlappedWeight(pole, i);

				double reflexWeigth = 1.0; // / izoveri;
				for (int imult = 0; imult < mult; imult++) {

//        System.out.println("hkl: "+hklList[0][imult] +" "+ hklList[1][imult]+" "+ hklList[2][imult]);
					double[] sctf = Uwimvuo.tfhkl(hklList[0][imult], hklList[1][imult], hklList[2][imult],
							cdsc[7], cdsc[5], cdsc[3], cdsc[6], cdsc[0], cdsc[1]);
					double fhir = Math.acos(sctf[3]);
					int inv = Uwimvuo.equiv(LaueGroupSnumber, sctf);

//			System.out.println(Integer.toXRDcatString(i) + " " + Fmt.format(textAngles[0]) + " " + Fmt.format(textAngles[1]));

//        int inversion = 1;
//        if (inv != 1)
//          inversion++;

					if (cubic)
						odfpath = calculateCellPathAnglesForCubic(textAngles[0], textAngles[1],
								sctf[0], sctf[1],
								fhir, inv, odfMaxAnglesR, mdb, cubic7);
					else
						odfpath = calculateCellPathAngles(textAngles[0], textAngles[1],
								sctf[0], sctf[1],
								fhir, inv, odfMaxAnglesR, mdb, cubic7);

					int cellNumber = odfpath[0].length; //nfismax * inversion;

					for (k = 0; k < cellNumber; k++) {

						tmp_angles[0] = odfpath[0][k];
						tmp_angles[1] = odfpath[1][k];
						tmp_angles[2] = odfpath[2][k];
/*			System.out.println(tmp_angles[0]*Constants.PITODEG + " " + tmp_angles[1]*Constants.PITODEG + " " +
              tmp_angles[2]*Constants.PITODEG);       */

						getIndicesR(tmp_angles, findex);
//			System.out.println(findex[0] + " " + findex[1] + " " + findex[2]);
						int cellid = ODFindex(findex);
//			System.out.println(cellid);

						double wgtcell = odfpath[3][k] * oneOverMult * overlappingWgt * reflexWeigth;

						increaseCoverage(cellid, wgtcell, totReflexWeigth);

						if (tk == 0 || !testCollapse) {
							cellidt[tk] = cellid;
							wgtcellt[tk++] = wgtcell;
//              System.out.println("Not testing " + totalcell);
						} else {
							collapse = false;
							for (int j = tk - 1; j >= 0 && !collapse; j--) {
								if (cellid == cellidt[j]) {
									wgtcellt[j] += wgtcell;
									collapse = true;
								}
							}
							if (!collapse) {
								int actualsize = cellIDandWGT.size();
								for (int j = 0; j < actualsize && !collapse; j += 2) {
									int[] cellidk = (int[]) cellIDandWGT.elementAt(j);
									for (int ij = 0; ij < maxindex && !collapse; ij++) {
										if (cellid == cellidk[ij]) {
											double[] wgtcellk = (double[]) cellIDandWGT.elementAt(j + 1);
											wgtcellk[ij] += wgtcell;
											collapse = true;
										}
									}
								}
							}
							if (!collapse) {
								cellidt[tk] = cellid;
								wgtcellt[tk++] = wgtcell;
							}
						}
						if (tk == maxindex) {
							cellIDandWGT.addElement(cellidt);
							cellIDandWGT.addElement(wgtcellt);
							totalcell += tk;
							tk = 0;
							wgtcellt = new double[maxindex];
							cellidt = new int[maxindex];
						}
					}
				}
			}
			totalcell += tk;

			double[] cellWGT = new double[totalcell];
			int[] cellID = new int[totalcell];

			int sizev = cellIDandWGT.size() / 2;
			int index = 0;
			for (int i = 0; i < sizev; i++) {
				int[] tcellid = (int[]) cellIDandWGT.elementAt(i * 2);
				double[] twgtcell = (double[]) cellIDandWGT.elementAt(i * 2 + 1);
				for (int j = 0; j < maxindex; j++) {
					cellID[index] = tcellid[j];
					cellWGT[index++] = twgtcell[j];
				}
			}
			for (int j = 0; j < tk; j++) {
				cellID[index] = cellidt[j];
				cellWGT[index++] = wgtcellt[j];
			}
			result.add(cellID);
			result.add(cellWGT);
		} else {
			result.add(new int[0]);
			result.add(new double[0]);
		}
		return result;
	}

	private void increaseCoverage(int cellid, double wgtcell, double totReflexWeigth) {
		synchronized (this) {
			totalWeight[cellid] += wgtcell * totReflexWeigth;
			totalHits[cellid]++;
		}
	}

	public Vector computeCellAndWeight(int dataindex, Vector<double[]> wgtContainer, int threadIndex) {
		double dist_a, dist_b, dist_g, real_dist;//, als, bets, gam;
		int k;
//l		boolean collapse;
		int stepa, stepb, stepg, maxa, maxb, maxg;
		int[] tmp_index = new int[3];
		double[] tmp_angles = new double[3];
		boolean cubic7 = getSampleSymmetryValue() == 7;
		Vector result = new Vector(2, 2);

		int numberCellTube = 27;
		double[] wgtcell = new double[numberCellTube];
		int[] cellid = new int[numberCellTube];
		double[] gridAngles = new double[3];
		int[] iaindex = new int[3];

		int pole = poleindex[dataindex];
		int point = pointindex[dataindex];

		Phase phase = getPhase();
		Sample sample = phase.getSample();
		boolean checkIn = true;

		if (!fromPF) {
			DiffrDataFile diffrDataFile = sample.getActiveDataSet(datasetindex[dataindex]).getActiveDataFile(datafileindex[dataindex]);
			checkIn = diffrDataFile.isPeakInsideRange(phase, poleFigureIndex[pole]);
		}

		if (checkIn) {
			double[] textAngles = null; // LucaToDo getTextureAngles(pole, point);

			double[] wgtcellt;
			if (wgtContainer != null)
				wgtcellt = wgtContainer.elementAt(threadIndex);
			else
				wgtcellt = new double[maxindex];

//l			Vector cellIDandWGT = new Vector(0, 2);

			int izoveri = 0; // LucaToDo getIzoveri(pole);

			int totalcell = 0;

//l			int tk = 0;
//l			int[] cellidt = new int[maxindex];

			double[][] odfpath;
			double totReflexWeigth = 1;
			for (int i = 0; i < izoveri; i++) {
				int[][] hklList = null; // LucaToDo gethklList(pole, i);

				int mult = hklList[0].length;
				double oneOverMult = 1.0 / mult;

				double overlappingWgt = 0; // LucaToDo getOverlappedWeight(pole, i);

//			overlappingWeigth[i] = reflex.getWeight();
//			overlappingWeigthTotal += overlappingWeigth[i];

				double reflexWeigth = 1.0; // / izoveri;
// System.out.println("tot " + totReflexWeigth);
				for (int imult = 0; imult < mult; imult++) {

					double[] sctf = Uwimvuo.tfhkl(hklList[0][imult], hklList[1][imult], hklList[2][imult],
							cdsc[7], cdsc[5], cdsc[3], cdsc[6], cdsc[0], cdsc[1]);
					double fhir = Math.acos(sctf[3]);
					int inv = Uwimvuo.equiv(LaueGroupSnumber, sctf);

//        int inversion = 1;
//        if (inv != 1)
//          inversion++;

					if (cubic)
						odfpath = calculateCellPathAnglesForCubic(textAngles[0], textAngles[1],
								sctf[0], sctf[1],
								fhir, inv, odfMaxAnglesR, mdb, cubic7);
					else
						odfpath = calculateCellPathAngles(textAngles[0], textAngles[1],
								sctf[0], sctf[1],
								fhir, inv, odfMaxAnglesR, mdb, cubic7);

					int cellNumber = odfpath[0].length; //nfismax * inversion;
//		    System.out.println(i + " " + Fmt.format(textAngles[0]) + " " + Fmt.format(textAngles[1]) + " " + cellNumber);


					for (k = 0; k < cellNumber; k++) {

						tmp_angles[0] = odfpath[0][k];
						tmp_angles[1] = odfpath[1][k];
						tmp_angles[2] = odfpath[2][k];

						getIndicesR(tmp_angles, iaindex);

						getAnglesR(iaindex, gridAngles);

						if (gridAngles[0] > tmp_angles[0]) {
							stepa = -1;
							maxa = -2;
						} else if (gridAngles[0] == tmp_angles[0]) {
							stepa = 1;
							maxa = 1;
						} else {
							stepa = 1;
							maxa = 2;
						}
						if (gridAngles[1] > tmp_angles[1]) {
							stepb = -1;
							maxb = -2;
						} else if (gridAngles[1] == tmp_angles[1]) {
							stepb = 1;
							maxb = 1;
						} else {
							stepb = 1;
							maxb = 2;
						}
						if (gridAngles[2] > tmp_angles[2]) {
							stepg = -1;
							maxg = -2;
						} else if (gridAngles[2] == tmp_angles[2]) {
							stepg = 1;
							maxg = 1;
						} else {
							stepg = 1;
							maxg = 2;
						}

						double orDista = -stepa * (gridAngles[0] - tmp_angles[0]);
						double orDistb = -stepb * (gridAngles[1] - tmp_angles[1]);
						double orDistg = -stepg * (gridAngles[2] - tmp_angles[2]);

//	  		int maxCount = (1 + Math.abs(stepa)) * (1 + Math.abs(stepb)) * (1 + Math.abs(stepg));
						double wgtot = 0.0;
						int kcell = 0;
						for (int ia = 0; ia != maxa; ia += stepa) {
							dist_a = ia * resolutionR * stepa - orDista;
							dist_a *= dist_a;
							for (int ib = 0; ib != maxb; ib += stepb) {
								dist_b = ib * resolutionR * stepb - orDistb;
								dist_b *= dist_b;
								for (int ig = 0; ig != maxg; ig += stepg) {
									dist_g = ig * resolutionR * stepg - orDistg;
									real_dist = Math.sqrt(dist_a + dist_b + dist_g * dist_g);
									if (real_dist <= dist_factor) {
										if (real_dist > 1.0E-5)
											wgtcell[kcell] = (1.0 / Math.pow(real_dist, tubeWeight));
										else
											wgtcell[kcell] = 1.0E5f;
//								wgtcell[kcell] = (double) ((dist_factor - Math.sqrt(real_dist)) / dist_factor);
//								wgtcell[kcell] *= wgtcell[kcell];
										tmp_index[0] = ia + iaindex[0];
										tmp_index[1] = ib + iaindex[1];
										tmp_index[2] = ig + iaindex[2];
//L_last1202								applyCrystalSymmetryAndCheck(tmp_index);
										wgtot += wgtcell[kcell];
										cellid[kcell++] = ODFindexAndCheck(tmp_index);//L_last1202
									}
								}
							}
						}

						double wgtcells = odfpath[3][k] * oneOverMult * overlappingWgt * reflexWeigth;

						if (kcell == 0) {
							wgtcell[kcell] = wgtcells;
							wgtot = 1.0;
//L_last1202					applyCrystalSymmetryAndCheck(iaindex);
							cellid[kcell++] = ODFindexAndCheck(iaindex);//L_last1202
						}

						double wgtw = wgtcells / wgtot;
						for (int ks = 0; ks < kcell; ks++)
							wgtcell[ks] *= wgtw;
//						increaseCoverage(cellid, wgtcell, kcell, totReflexWeigth);

						for (int ks = 0; ks < kcell; ks++) {
							wgtcellt[cellid[ks]] += wgtcell[ks];
						}
					}

				}

			}

			for (int i = 0; i < maxindex; i++)
				if (wgtcellt[i] > 0)
					totalcell++;
//l			totalcell += tk;
			double[] cellWGT = new double[totalcell];
			int[] cellID = new int[totalcell];

			int index = 0;
			for (int i = 0; i < maxindex; i++)
				if (wgtcellt[i] > 0) {
					cellID[index] = i;
					cellWGT[index++] = wgtcellt[i];
					wgtcellt[i] = 0; // we reset for next
				}
			increaseCoverage(cellID, cellWGT, totalcell, totReflexWeigth); // we divide by izoveri to weight less overlapped
			result.add(cellID);
			result.add(cellWGT);
		} else {
			result.add(new int[0]);
			result.add(new double[0]);
		}
		return result;
	}

	private void increaseCoverage(int[] cellid, double[] wgtcell, int kcell, double totReflexWeigth) {
		synchronized (this) {
			for (int ks = 0; ks < kcell; ks++) {
				totalWeight[cellid[ks]] += wgtcell[ks] * totReflexWeigth;
				totalHits[cellid[ks]]++;
			}
		}
	}

	public int ODFindex(int[] ia) {
		return ODFindex(ia[0], ia[1], ia[2]);
	}

	public int ODFindex(int ia, int ib, int ig) {
		return nae * nbe * ig + nae * ib + ia;
	}

	public int[] ODFindex(int index) {
		int[] iabg = new int[3];
		iabg[2] = index / (nae * nbe);
		int res = index % (nae * nbe);
		iabg[1] = res / nae;
		iabg[0] = res % nae;
		return iabg;
	}

	public int ODFindexAndCheck(int[] index) {
		applyCrystalSymmetryAndCheck(index);
		return ODFindex(index);
	}

	public int ODFindexAndCheck(int index) {
		int[] findex = ODFindex(index);
		applyCrystalSymmetryAndCheck(findex);
		return ODFindex(findex);
	}

	public double getODF(int[] index) {
//    applyCrystalSymmetryLightCheck(index);
		applyCrystalSymmetryAndCheck(index);
		return odf[index[0]][index[1]][index[2]];
	}

	public double getODF(double als, double bets, double gams) {
		// carefull this needs radiant angles !!!!!
		int[] index = new int[3];
		index[0] = (int) ((als + pi25g) / resolutionR + .000001);
		index[1] = (int) ((bets + pi25g) / resolutionR + .000001);
		index[2] = (int) ((gams + pi25g) / resolutionR + .000001);
		return getODF(index);
	}

	public void getIndicesNoCheckR(double[] angles, int[] index) {

		index[0] = (int) ((angles[0] + pi25g) / resolutionR + .000001);
		index[1] = (int) ((angles[1] + pi25g) / resolutionR + .000001);
		index[2] = (int) ((angles[2] + pi25g) / resolutionR + .000001);

	}

	public void getIndicesR(double[] angles, int[] index) {

		index[0] = (int) ((angles[0] + pi25g) / resolutionR + .000001);
		index[1] = (int) ((angles[1] + pi25g) / resolutionR + .000001);
		index[2] = (int) ((angles[2] + pi25g) / resolutionR + .000001);

		applyCrystalSymmetryAndCheck(index);

	}

	public void getAnglesR(int[] alpha, double[] index) {

		index[0] = alpha[0] * resolutionR;
		index[1] = alpha[1] * resolutionR;
		index[2] = alpha[2] * resolutionR;


	}

	public static void applyCrystalSymmetryAndCheck(double[] index, double[] odfMaxAnglesR, int mdb, boolean cubic7) {

//    double lphiturn;
//    int fold;

		if (index[1] >= Constants.PI2)
			index[1] -= Constants.PI2;
		if (index[1] < 0)
			index[1] += Constants.PI2;
		if (index[1] > Constants.PI) {
			index[1] = Constants.PI2 - index[1];
			index[0] += Constants.PI;
			index[2] += Constants.PI;
		}

		if (index[0] >= Constants.PI2)
			index[0] -= Constants.PI2;
		if (index[0] < 0)
			index[0] += Constants.PI2;

		if (index[2] >= Constants.PI2)
			index[2] -= Constants.PI2;
		if (index[2] < 0)
			index[2] += Constants.PI2;

		if (mdb != 0) {
//  For GB = Dn : Symmetry element C2XB
//                PI+ALPHA, PI-BETA, 2PI-GAMMA

			if (index[1] >= odfMaxAnglesR[1]) {
				index[1] = Constants.PI - index[1];
				index[2] = Constants.PI2 - index[2];
				index[0] += Constants.PI;
			}
			if (index[0] >= Constants.PI2)
				index[0] -= Constants.PI2;
		}

		while (index[2] >= odfMaxAnglesR[2])
			index[2] -= odfMaxAnglesR[2];

// ODF space general properties
// beta = 0

		if (index[1] == 0.0) {
			index[0] += index[2];
			index[2] = 0.0;
			if (index[0] >= Constants.PI2)
				index[0] -= Constants.PI2;
			if (index[0] < 0)
				index[0] += Constants.PI2;
		}

// beta = PI

		if (index[1] == Constants.PI) {
			index[0] -= index[2];
			index[2] = 0.0;
			if (index[0] >= Constants.PI2)
				index[0] -= Constants.PI2;
			if (index[0] < 0)
				index[0] += Constants.PI2;
		}

//  GAMMA REGION 0-360 degrees IS COMPLETE

// Sample symmetry
//    System.out.println("other " + multiplicity);
		if (cubic7)
			index[0] = 0.0;

	}


	public void applyCrystalSymmetryAndCheck(int[] index) {

//    int fold, lphiturn;

		if (index[1] >= alphama)
			index[1] -= alphama1;
		if (index[1] < 0)
			index[1] += alphama1;
		if (index[1] >= betama) {
			index[1] = alphama1 - index[1];
			index[0] += betama1;
			index[2] += betama1;
		}


		if (index[0] >= alphama1)
			index[0] -= alphama1;
		if (index[0] < 0)
			index[0] += alphama1;

		if (index[2] >= alphama)
			index[2] -= alphama1;
		if (index[2] < 0)
			index[2] += alphama1;

		if (mdb != 0) {
//  For GB = Dn : Symmetry element C2XB
//                PI+ALPHA, PI-BETA, 2PI-GAMMA

			if (index[1] >= nbe) {
				index[1] = betama1 - index[1];
				index[2] = alphama1 - index[2];
				index[0] += betama1;
			}
			if (index[0] >= alphama1)
				index[0] -= alphama1;
		}

		while (index[2] >= nge)
			index[2] -= nge - 1;

// ODF space general properties
// beta = 0

		if (index[1] == 0) {
			index[0] += index[2];
			index[2] = 0;
			if (index[0] >= alphama1)
				index[0] -= alphama1;
			if (index[0] < 0)
				index[0] += alphama1;
		}

// beta = PI

		if (index[1] == betama1) {
			index[0] -= index[2];
			index[2] = 0;
			if (index[0] >= alphama1)
				index[0] -= alphama1;
			if (index[0] < 0)
				index[0] += alphama1;
		}

//  GAMMA REGION 0-360 degrees IS COMPLETE

// Sample symmetry
		if (getSampleSymmetryValue() == 7)
			index[0] = 0;
	}

	public void applyCrystalSymmetry(int[] index) {

//    int fold, lphiturn;

		if (mdb != 0) {
//  For GB = Dn : Symmetry element C2XB
//                PI+ALPHA, PI-BETA, 2PI-GAMMA

			if (index[1] >= nbe) {
				index[1] = betama1 - index[1];
				index[2] = alphama1 - index[2];
				index[0] += betama1;
			}
			if (index[0] >= alphama1)
				index[0] -= alphama1;
		}

		while (index[2] >= nge)
			index[2] -= nge - 1;

//  GAMMA REGION 0-360 degrees IS COMPLETE

// Sample symmetry
//    System.out.println("still " + multiplicity);
		switch (getSampleSymmetryValue()) {
			case 7:
				index[0] = 0;
				break;
			default: {
			}
		}

	}

	public void applyCrystalSymmetryLightCheck(int[] index) {

//    int fold, lphiturn;

		if (index[1] >= alphama)
			index[1] -= alphama1;
		if (index[1] < 0)
			index[1] += alphama1;
		if (index[1] >= betama) {
			index[1] = alphama1 - index[1];
			index[0] += betama1;
			index[2] += betama1;
		}


		if (index[0] >= alphama1)
			index[0] -= alphama1;
		if (index[0] < 0)
			index[0] += alphama1;

		if (index[2] >= alphama)
			index[2] -= alphama1;
		if (index[2] < 0)
			index[2] += alphama1;

	}

	public double[] calculatePF(ReflectionTexture reflectionTexture) {
    double ang;
    int nfis;
    double ca2, cb2, sa2;

//     Calculation of a complete reduced pole figure
//       INPUT FIO given in the whole G-space OUTPUT POLREF=FS

//    boolean negODFout = false;
//    if (Constants.testing)
//    	negODFout = MaudPreferences.getBoolean("debug.negativeODFout", false);
    
    cb2 = reflectionTexture.sctf[1];
    int nfismax = (int) (Constants.PI2 / integrationStepPFR + 1.000001);
    
    int numberOfPoints = reflectionTexture.getPointsNumber();
    double[] fs = new double[numberOfPoints];
    
    int[] referenceCounter = new int[numberOfPoints];
    int[] references = new int[numberOfPoints];
    int[] pointReference = new int[numberOfPoints];
    
    pointReference[0] = 0;
    references[0] = 0;
    int numberOfReferences = 0;
    for (int n = 0; n < numberOfPoints; n++) {
//      thetaphi[0][n] *= Constants.DEGTOPI;
//      thetaphi[1][n] *= Constants.DEGTOPI;
      boolean isNewReference = true;
      for (int nscan = 0; nscan < numberOfReferences; nscan++)
        if (Math.abs(reflectionTexture.getAngles(references[nscan])[0] -
            reflectionTexture.getAngles(n)[0]) < 0.0002) {
          isNewReference = false;
          referenceCounter[pointReference[references[nscan]]]++;
          pointReference[n] = pointReference[references[nscan]];
          break;
        }
      if (isNewReference) {
        references[numberOfReferences] = n;
        pointReference[n] = numberOfReferences;
        referenceCounter[numberOfReferences]++;
        numberOfReferences++;
      }
    }
    
    int[] finalPointReference = new int[numberOfReferences];
    finalPointReference[0] = referenceCounter[0];
    references[0] = 0;
    for (int n = 1; n < numberOfReferences; n++) {
      references[n] = finalPointReference[n - 1];
      finalPointReference[n] = finalPointReference[n - 1] + referenceCounter[n];
    }
    
    for (int n = 0; n < numberOfPoints; n++) {
      referenceCounter[references[pointReference[n]]] = n;
      references[pointReference[n]]++;
    }
    
    int startingPoint = 0;
    
    double[] angles = new double[3];
    int[] iaindex = new int[3];
    
    int maxRepeat = 2 - reflectionTexture.inv;
    double[][] phiRef = new double[maxRepeat][nfismax];
    int[][] cellIndex1 = new int[maxRepeat][nfismax];
    int[][] cellIndex2 = new int[maxRepeat][nfismax];
    double[] g2rv = new double[maxRepeat];
    double[] cb2v = new double[maxRepeat];
    g2rv[0] = Constants.PI - reflectionTexture.fhir;
    while (g2rv[0] < 0.)
      g2rv[0] += Constants.PI2;
    cb2v[0] = cb2;
    for (int iu = 1; iu < maxRepeat; iu++) {
      cb2v[iu] = -cb2;
      g2rv[iu] = g2rv[0] - Constants.PI;
      while (g2rv[iu] < 0.)
        g2rv[iu] += Constants.PI2;
    }
    
    for (int nref = 0; nref < numberOfReferences; nref++) {
      // evaluate the first new theta value, all the other change only in phi
      int n1 = referenceCounter[startingPoint];
      fs[n1] = 0.;
      
      /* Projection thread loop, Simpson integration */
      double[] thetaphi = reflectionTexture.getAngles(n1);
      double cr = Math.cos(thetaphi[0]);
      double sr = Math.sin(thetaphi[0]);
      for (nfis = 0; nfis < nfismax; nfis++) {
        ang = nfis * integrationStepPFR;
        ca2 = -Math.cos(ang);
        sa2 = Math.sin(ang);
        double ffak1 = 0.0;
        for (int repeat = 0; repeat < maxRepeat; repeat++) {
          Angles.g20g100(angles, ca2, sa2, cb2v[repeat], reflectionTexture.sctf[0], cr, sr);
          phiRef[repeat][nfis] = angles[0]; // - iaindex[0] * resolutionR;
          angles[0] += thetaphi[1];
          angles[2] += g2rv[repeat];
          getIndicesNoCheckR(angles, iaindex);
          cellIndex1[repeat][nfis] = iaindex[1];
          cellIndex2[repeat][nfis] = iaindex[2];
          ffak1 += getODF(iaindex);
        }
        if (0 < nfis && nfis < nfismax - 1) {
          if (MoreMath.odd(nfis + 1))
            ffak1 *= 2;
          else
            ffak1 *= 4;
        }
        fs[n1] += ffak1;
      }
      
      fs[n1] *= pisimg / maxRepeat;
      
      startingPoint++;
      for (int n = startingPoint; n < finalPointReference[nref]; n++) {
        // referenceCounter[n] is the point to evaluate
        n1 = referenceCounter[n];
        fs[n1] = 0.;
        
        // Projection thread loop, Simpson integration
        for (nfis = 0; nfis < nfismax; nfis++) {
          double ffak1 = 0.0;
          for (int repeat = 0; repeat < maxRepeat; repeat++) {
            double anglesPhi = phiRef[repeat][nfis] + thetaphi[1];
            iaindex[0] = (int) ((anglesPhi + pi25g) / resolutionR + .000001);
            iaindex[1] = cellIndex1[repeat][nfis];
            iaindex[2] = cellIndex2[repeat][nfis];

//						double f_odf = getODF(iaindex); // Luca_last1202 f[findex[0]][findex[1]][findex[2]];
            ffak1 += getODF(iaindex);
          }
          if (0 < nfis && nfis < nfismax - 1) {
            if (MoreMath.odd(nfis + 1))
              ffak1 *= 2.;
            else
              ffak1 *= 4.;
          }
          fs[n1] += ffak1;
        }
        fs[n1] *= pisimg / maxRepeat;
      }
      startingPoint = finalPointReference[nref];  // next one
    }
    
    return fs;
	}

	public double[] calculatePFbyTubeProjection(ReflectionTexture reflectionTexture) {
// Local variables
		double ffak, ang, ca2, cb2, sa2,
				dist_a, dist_b, dist_g, real_dist, wgtcell;
		int nfis, stepa, stepb, stepg, maxa, maxb, maxg;
		int[] tmp_index = new int[3];

//     Calculation of a complete reduced pole figure
//       INPUT FIO given in the whole G-space OUTPUT POLREF=FS

//    boolean negODFout = false;
//    if (Constants.testing)
//    	negODFout = MaudPreferences.getBoolean("debug.negativeODFout", false);

		cb2 = reflectionTexture.sctf[1];
		int nfismax = (int) (Constants.PI2 / integrationStepPFR + 1.000001);

		int numberOfPoints = reflectionTexture.getPointsNumber();
		double[] fs = new double[numberOfPoints];

		int[] referenceCounter = new int[numberOfPoints];
		int[] references = new int[numberOfPoints];
		int[] pointReference = new int[numberOfPoints];

		pointReference[0] = 0;
		references[0] = 0;
		int numberOfReferences = 0;
		for (int n = 0; n < numberOfPoints; n++) {
			//     thetaphi[0][n] *= Constants.DEGTOPI;
			//     thetaphi[1][n] *= Constants.DEGTOPI;
			boolean isNewReference = true;
			for (int nscan = 0; nscan < numberOfReferences; nscan++) {
        if (Math.abs(reflectionTexture.getAngles(references[nscan])[0] -
            reflectionTexture.getAngles(n)[0]) < 0.0002) {
          isNewReference = false;
          referenceCounter[pointReference[references[nscan]]]++;
          pointReference[n] = pointReference[references[nscan]];
          break;
        }
      }
			if (isNewReference) {
				references[numberOfReferences] = n;
				pointReference[n] = numberOfReferences;
				referenceCounter[numberOfReferences]++;
				numberOfReferences++;
			}
		}

		int[] finalPointReference = new int[numberOfReferences];
		finalPointReference[0] = referenceCounter[0];
		references[0] = 0;
		for (int n = 1; n < numberOfReferences; n++) {
			references[n] = finalPointReference[n - 1];
			finalPointReference[n] = finalPointReference[n - 1] + referenceCounter[n];
		}

		for (int n = 0; n < numberOfPoints; n++) {
			referenceCounter[references[pointReference[n]]] = n;
			references[pointReference[n]]++;
		}

		int startingPoint = 0;

		double[] angles = new double[3];
		int[] iaindex = new int[3];
		double[] gridAngles = new double[3];

		int maxRepeat = 2 - reflectionTexture.inv;
		int[][] cellIndex1 = new int[maxRepeat][nfismax];
		int[][] cellIndex2 = new int[maxRepeat][nfismax];
		int[][] cellstepb = new int[maxRepeat][nfismax];
		int[][] cellstepg = new int[maxRepeat][nfismax];
		int[][] cellmaxb = new int[maxRepeat][nfismax];
		int[][] cellmaxg = new int[maxRepeat][nfismax];
		double[][] phiRef = new double[maxRepeat][nfismax];
		double[][] cellDistb = new double[maxRepeat][nfismax];
		double[][] cellDistg = new double[maxRepeat][nfismax];
		double[] g2rv = new double[maxRepeat];
		double[] cb2v = new double[maxRepeat];
		g2rv[0] = Constants.PI - reflectionTexture.fhir;
		while (g2rv[0] < 0.)
			g2rv[0] += Constants.PI2;
		cb2v[0] = cb2;
		for (int iu = 1; iu < maxRepeat; iu++) {
			cb2v[iu] = -cb2;
			g2rv[iu] = g2rv[0] - Constants.PI;
			while (g2rv[iu] < 0.)
				g2rv[iu] += Constants.PI2;
		}

		for (int nref = 0; nref < numberOfReferences; nref++) {
			// evaluate the first new theta value, all the other change only in phi
			int n1 = referenceCounter[startingPoint];
			fs[n1] = 0.;

// Projection thread loop, Simpson integration
      double[] thetaphi = reflectionTexture.getAngles(n1);
			double cr = Math.cos(thetaphi[0]);
			double sr = Math.sin(thetaphi[0]);
			for (nfis = 0; nfis < nfismax; nfis++) {
				ang = nfis * integrationStepPFR;
				ca2 = -Math.cos(ang);
				sa2 = Math.sin(ang);
				double ffak1 = 0.0;
				for (int repeat = 0; repeat < maxRepeat; repeat++) {
					Angles.g20g100(angles, ca2, sa2, cb2v[repeat], reflectionTexture.sctf[0], cr, sr);
					phiRef[repeat][nfis] = angles[0];
					angles[0] += thetaphi[1];
					angles[2] += g2rv[repeat];
					getIndicesNoCheckR(angles, iaindex);
					getAnglesR(iaindex, gridAngles);
					cellIndex1[repeat][nfis] = iaindex[1];
					cellIndex2[repeat][nfis] = iaindex[2];
					if (gridAngles[0] > angles[0]) {
						stepa = -1;
						maxa = -2;
					} else if (gridAngles[0] == angles[0]) {
						stepa = 1;
						maxa = 1;
					} else {
						stepa = 1;
						maxa = 2;
					}
					if (gridAngles[1] > angles[1]) {
						stepb = -1;
						maxb = -2;
					} else if (gridAngles[1] == angles[1]) {
						stepb = 1;
						maxb = 1;
					} else {
						stepb = 1;
						maxb = 2;
					}
					if (gridAngles[2] > angles[2]) {
						stepg = -1;
						maxg = -2;
					} else if (gridAngles[2] == angles[2]) {
						stepg = 1;
						maxg = 1;
					} else {
						stepg = 1;
						maxg = 2;
					}

					double orDista = -stepa * (gridAngles[0] - angles[0]);
					double orDistb = -stepb * (gridAngles[1] - angles[1]);
					double orDistg = -stepg * (gridAngles[2] - angles[2]);
					cellstepb[repeat][nfis] = stepb;
					cellmaxb[repeat][nfis] = maxb;
					cellstepg[repeat][nfis] = stepg;
					cellmaxg[repeat][nfis] = maxg;
					cellDistb[repeat][nfis] = orDistb;
					cellDistg[repeat][nfis] = orDistg;

					ffak = 0.0;
					double wgtot = 0.0;
//					int cellNumber = 0;
					for (int ia = 0; ia != maxa; ia += stepa) {
						dist_a = ia * resolutionR * stepa - orDista;
						dist_a *= dist_a;
						for (int ib = 0; ib != maxb; ib += stepb) {
							dist_b = ib * resolutionR * stepb - orDistb;
							dist_b *= dist_b;
							for (int ig = 0; ig != maxg; ig += stepg) {
								dist_g = ig * resolutionR * stepg - orDistg;
								real_dist = dist_a + dist_b + dist_g * dist_g;
								if (real_dist <= dist_factor2) {
									if (real_dist < 1.0E-4)
										real_dist = 1.0E-4;
									wgtcell = 1.0 / Math.sqrt(real_dist);
//									wgtcell = ((dist_factor - Math.sqrt(real_dist)) / dist_factor);
//									wgtcell = *= wgtcell;
									tmp_index[0] = ia + iaindex[0];
									tmp_index[1] = ib + iaindex[1];
									tmp_index[2] = ig + iaindex[2];
									ffak += getODF(tmp_index) * wgtcell;
									wgtot += wgtcell;
								}
							}
						}
					}
					if (wgtot == 0.0)
						ffak1 += getODF(iaindex);
					else
						ffak1 += ffak / wgtot;
				}
				if (0 < nfis && nfis < nfismax - 1) {
					if (MoreMath.odd(nfis + 1))
						ffak1 *= 2;
					else
						ffak1 *= 4;
				}
				fs[n1] += ffak1;
			}

			fs[n1] *= pisimg / maxRepeat;

			startingPoint++;
			for (int n = startingPoint; n < finalPointReference[nref]; n++) {
				// referenceCounter[n] is the point to evaluate
				n1 = referenceCounter[n];
				fs[n1] = 0.;

        // Projection thread loop, Simpson integration
				for (nfis = 0; nfis < nfismax; nfis++) {
					double ffak1 = 0.0;
					for (int repeat = 0; repeat < maxRepeat; repeat++) {
						double anglesPhi = phiRef[repeat][nfis] + thetaphi[1];
						iaindex[0] = (int) ((anglesPhi + pi25g) / resolutionR + .000001);
						double anglesGridPhi = iaindex[0] * resolutionR;

						iaindex[1] = cellIndex1[repeat][nfis];
						iaindex[2] = cellIndex2[repeat][nfis];

						anglesGridPhi = anglesGridPhi - anglesPhi;
						if (anglesGridPhi > 0.0) {
							stepa = -1;
							maxa = -2;
						} else if (anglesGridPhi == 0.0) {
							stepa = 1;
							maxa = 1;
						} else {
							stepa = 1;
							maxa = 2;
						}

						stepb = cellstepb[repeat][nfis];
						maxb = cellmaxb[repeat][nfis];
						stepg = cellstepg[repeat][nfis];
						maxg = cellmaxg[repeat][nfis];
						double orDista = -stepa * anglesGridPhi;
						double orDistb = cellDistb[repeat][nfis];
						double orDistg = cellDistg[repeat][nfis];

						ffak = 0.0;
						double wgtot = 0.0;
						for (int ia = 0; ia != maxa; ia += stepa) {
							dist_a = ia * resolutionR * stepa - orDista;
							dist_a *= dist_a;
							for (int ib = 0; ib != maxb; ib += stepb) {
								dist_b = ib * resolutionR * stepb - orDistb;
								dist_b *= dist_b;
								for (int ig = 0; ig != maxg; ig += stepg) {
									dist_g = ig * resolutionR * stepg - orDistg;
									real_dist = dist_a + dist_b + dist_g * dist_g;
									if (real_dist <= dist_factor2) {
										if (real_dist < 1.0E-4)
											real_dist = 1.0E-4;
										wgtcell = 1.0 / Math.sqrt(real_dist);
//										wgtcell = ((dist_factor - Math.sqrt(real_dist)) / dist_factor);
//										wgtcell *= wgtcell;
										tmp_index[0] = ia + iaindex[0];
										tmp_index[1] = ib + iaindex[1];
										tmp_index[2] = ig + iaindex[2];
										ffak += getODF(tmp_index) * wgtcell;
										wgtot += wgtcell;
									}
								}
							}
						}
						if (wgtot == 0.0)
							ffak1 += getODF(iaindex);
						else
							ffak1 += ffak / wgtot;
					}
					if (0 < nfis && nfis < nfismax - 1) {
						if (MoreMath.odd(nfis + 1))
							ffak1 *= 2.;
						else
							ffak1 *= 4.;
					}
					fs[n1] += ffak1;
				}

				fs[n1] *= pisimg / maxRepeat;

			}
			startingPoint = finalPointReference[nref];  // next one
		}

//		System.out.println(fs);
		return fs;
	}
  
  public double[] calculatePF(double[][] thetaphi,
                              double sthi, double cthi, double fhir, int inv) {
    // Local variables
    double ang;
    int nfis;
    double ca2, cb2, sa2;

//     Calculation of a complete reduced pole figure
//       INPUT FIO given in the whole G-space OUTPUT POLREF=FS

    cb2 = cthi;
    int nfismax = (int) (Constants.PI2 / integrationStepPFR + 1.000001);
    
    int numberOfPoints = thetaphi[0].length;
    double[] fs = new double[numberOfPoints];
    
    int[] referenceCounter = new int[numberOfPoints];
    int[] references = new int[numberOfPoints];
    int[] pointReference = new int[numberOfPoints];
    
    pointReference[0] = 0;
    references[0] = 0;
    int numberOfReferences = 0;
    for (int n = 0; n < numberOfPoints; n++) {
//      thetaphi[0][n] *= Constants.DEGTOPI;
//      thetaphi[1][n] *= Constants.DEGTOPI;
      boolean isNewReference = true;
      for (int nscan = 0; nscan < numberOfReferences; nscan++)
        if (Math.abs(thetaphi[0][references[nscan]] - thetaphi[0][n]) < 0.0002) {
          isNewReference = false;
          referenceCounter[pointReference[references[nscan]]]++;
          pointReference[n] = pointReference[references[nscan]];
          break;
        }
      if (isNewReference) {
        references[numberOfReferences] = n;
        pointReference[n] = numberOfReferences;
        referenceCounter[numberOfReferences]++;
        numberOfReferences++;
      }
    }
    
    int[] finalPointReference = new int[numberOfReferences];
    finalPointReference[0] = referenceCounter[0];
    references[0] = 0;
    for (int n = 1; n < numberOfReferences; n++) {
      references[n] = finalPointReference[n - 1];
      finalPointReference[n] = finalPointReference[n - 1] + referenceCounter[n];
    }
    
    for (int n = 0; n < numberOfPoints; n++) {
      referenceCounter[references[pointReference[n]]] = n;
      references[pointReference[n]]++;
    }
    
    int startingPoint = 0;
    
    double[] angles = new double[3];
    int[] iaindex = new int[3];
    
    int maxRepeat = 2 - inv;
    double[][] phiRef = new double[maxRepeat][nfismax];
    int[][] cellIndex1 = new int[maxRepeat][nfismax];
    int[][] cellIndex2 = new int[maxRepeat][nfismax];
    double[] g2rv = new double[maxRepeat];
    double[] cb2v = new double[maxRepeat];
    g2rv[0] = Constants.PI - fhir;
    while (g2rv[0] < 0.)
      g2rv[0] += Constants.PI2;
    cb2v[0] = cb2;
    for (int iu = 1; iu < maxRepeat; iu++) {
      cb2v[iu] = -cb2;
      g2rv[iu] = g2rv[0] - Constants.PI;
      while (g2rv[iu] < 0.)
        g2rv[iu] += Constants.PI2;
    }
    
    for (int nref = 0; nref < numberOfReferences; nref++) {
      // evaluate the first new theta value, all the other change only in phi
      int n1 = referenceCounter[startingPoint];
      fs[n1] = 0.;
      
      // Projection thread loop, Simpson integration
      double cr = Math.cos(thetaphi[0][n1]);
      double sr = Math.sin(thetaphi[0][n1]);
      for (nfis = 0; nfis < nfismax; nfis++) {
        ang = nfis * integrationStepPFR;
        ca2 = -Math.cos(ang);
        sa2 = Math.sin(ang);
        double ffak1 = 0.0;
        for (int repeat = 0; repeat < maxRepeat; repeat++) {
          Angles.g20g100(angles, ca2, sa2, cb2v[repeat], sthi, cr, sr);
          phiRef[repeat][nfis] = angles[0]; // - iaindex[0] * resolutionR;
          angles[0] += thetaphi[1][n1];
          angles[2] += g2rv[repeat];
          getIndicesNoCheckR(angles, iaindex);
          cellIndex1[repeat][nfis] = iaindex[1];
          cellIndex2[repeat][nfis] = iaindex[2];
          ffak1 += getODF(iaindex);
        }
        if (0 < nfis && nfis < nfismax - 1) {
          if (MoreMath.odd(nfis + 1))
            ffak1 *= 2;
          else
            ffak1 *= 4;
        }
        fs[n1] += ffak1;
      }
      
      fs[n1] *= pisimg / maxRepeat;
      
      startingPoint++;
      for (int n = startingPoint; n < finalPointReference[nref]; n++) {
        // referenceCounter[n] is the point to evaluate
        n1 = referenceCounter[n];
        fs[n1] = 0.;
        
        /* Projection thread loop, Simpson integration */
        for (nfis = 0; nfis < nfismax; nfis++) {
          double ffak1 = 0.0;
          for (int repeat = 0; repeat < maxRepeat; repeat++) {
            double anglesPhi = phiRef[repeat][nfis] + thetaphi[1][n1];
            iaindex[0] = (int) ((anglesPhi + pi25g) / resolutionR + .000001);
            iaindex[1] = cellIndex1[repeat][nfis];
            iaindex[2] = cellIndex2[repeat][nfis];

//						double f_odf = getODF(iaindex); // Luca_last1202 f[findex[0]][findex[1]][findex[2]];
            ffak1 += getODF(iaindex);
          }
          if (0 < nfis && nfis < nfismax - 1) {
            if (MoreMath.odd(nfis + 1))
              ffak1 *= 2.;
            else
              ffak1 *= 4.;
          }
          fs[n1] += ffak1;
        }
        fs[n1] *= pisimg / maxRepeat;
      }
      startingPoint = finalPointReference[nref];  // next one
    }
    
    return fs;
  }
  
  public double[] calculatePFbyTubeProjection(double[][] thetaphi,
                                              double sthi, double cthi, double fhir, int inv) {
    // Local variables
    double ffak, ang, ca2, cb2, sa2,
        dist_a, dist_b, dist_g, real_dist, wgtcell;
    int nfis, stepa, stepb, stepg, maxa, maxb, maxg;
    int[] tmp_index = new int[3];

//     Calculation of a complete reduced pole figure
//       INPUT FIO given in the whole G-space OUTPUT POLREF=FS

//    boolean negODFout = false;
//    if (Constants.testing)
//    	negODFout = MaudPreferences.getBoolean("debug.negativeODFout", false);
    
    cb2 = cthi;
    int nfismax = (int) (Constants.PI2 / integrationStepPFR + 1.000001);
    
    int numberOfPoints = thetaphi[0].length;
    double[] fs = new double[numberOfPoints];
    
    int[] referenceCounter = new int[numberOfPoints];
    int[] references = new int[numberOfPoints];
    int[] pointReference = new int[numberOfPoints];
    
    pointReference[0] = 0;
    references[0] = 0;
    int numberOfReferences = 0;
    for (int n = 0; n < numberOfPoints; n++) {
      //     thetaphi[0][n] *= Constants.DEGTOPI;
      //     thetaphi[1][n] *= Constants.DEGTOPI;
      boolean isNewReference = true;
      for (int nscan = 0; nscan < numberOfReferences; nscan++)
        if (Math.abs(thetaphi[0][references[nscan]] - thetaphi[0][n]) < 0.0002) {
          isNewReference = false;
          referenceCounter[pointReference[references[nscan]]]++;
          pointReference[n] = pointReference[references[nscan]];
          break;
        }
      if (isNewReference) {
        references[numberOfReferences] = n;
        pointReference[n] = numberOfReferences;
        referenceCounter[numberOfReferences]++;
        numberOfReferences++;
      }
    }
    
    int[] finalPointReference = new int[numberOfReferences];
    finalPointReference[0] = referenceCounter[0];
    references[0] = 0;
    for (int n = 1; n < numberOfReferences; n++) {
      references[n] = finalPointReference[n - 1];
      finalPointReference[n] = finalPointReference[n - 1] + referenceCounter[n];
    }
    
    for (int n = 0; n < numberOfPoints; n++) {
      referenceCounter[references[pointReference[n]]] = n;
      references[pointReference[n]]++;
    }
    
    int startingPoint = 0;
    
    double[] angles = new double[3];
    int[] iaindex = new int[3];
    double[] gridAngles = new double[3];
    
    int maxRepeat = 2 - inv;
    int[][] cellIndex1 = new int[maxRepeat][nfismax];
    int[][] cellIndex2 = new int[maxRepeat][nfismax];
    int[][] cellstepb = new int[maxRepeat][nfismax];
    int[][] cellstepg = new int[maxRepeat][nfismax];
    int[][] cellmaxb = new int[maxRepeat][nfismax];
    int[][] cellmaxg = new int[maxRepeat][nfismax];
    double[][] phiRef = new double[maxRepeat][nfismax];
    double[][] cellDistb = new double[maxRepeat][nfismax];
    double[][] cellDistg = new double[maxRepeat][nfismax];
    double[] g2rv = new double[maxRepeat];
    double[] cb2v = new double[maxRepeat];
    g2rv[0] = Constants.PI - fhir;
    while (g2rv[0] < 0.)
      g2rv[0] += Constants.PI2;
    cb2v[0] = cb2;
    for (int iu = 1; iu < maxRepeat; iu++) {
      cb2v[iu] = -cb2;
      g2rv[iu] = g2rv[0] - Constants.PI;
      while (g2rv[iu] < 0.)
        g2rv[iu] += Constants.PI2;
    }
    
    for (int nref = 0; nref < numberOfReferences; nref++) {
      // evaluate the first new theta value, all the other change only in phi
      int n1 = referenceCounter[startingPoint];
      fs[n1] = 0.;
      
      /* Projection thread loop, Simpson integration */
      double cr = Math.cos(thetaphi[0][n1]);
      double sr = Math.sin(thetaphi[0][n1]);
      for (nfis = 0; nfis < nfismax; nfis++) {
        ang = nfis * integrationStepPFR;
        ca2 = -Math.cos(ang);
        sa2 = Math.sin(ang);
        double ffak1 = 0.0;
        for (int repeat = 0; repeat < maxRepeat; repeat++) {
          Angles.g20g100(angles, ca2, sa2, cb2v[repeat], sthi, cr, sr);
          phiRef[repeat][nfis] = angles[0];
          angles[0] += thetaphi[1][n1];
          angles[2] += g2rv[repeat];
          getIndicesNoCheckR(angles, iaindex);
          getAnglesR(iaindex, gridAngles);
          cellIndex1[repeat][nfis] = iaindex[1];
          cellIndex2[repeat][nfis] = iaindex[2];
          if (gridAngles[0] > angles[0]) {
            stepa = -1;
            maxa = -2;
          } else if (gridAngles[0] == angles[0]) {
            stepa = 1;
            maxa = 1;
          } else {
            stepa = 1;
            maxa = 2;
          }
          if (gridAngles[1] > angles[1]) {
            stepb = -1;
            maxb = -2;
          } else if (gridAngles[1] == angles[1]) {
            stepb = 1;
            maxb = 1;
          } else {
            stepb = 1;
            maxb = 2;
          }
          if (gridAngles[2] > angles[2]) {
            stepg = -1;
            maxg = -2;
          } else if (gridAngles[2] == angles[2]) {
            stepg = 1;
            maxg = 1;
          } else {
            stepg = 1;
            maxg = 2;
          }
          
          double orDista = -stepa * (gridAngles[0] - angles[0]);
          double orDistb = -stepb * (gridAngles[1] - angles[1]);
          double orDistg = -stepg * (gridAngles[2] - angles[2]);
          cellstepb[repeat][nfis] = stepb;
          cellmaxb[repeat][nfis] = maxb;
          cellstepg[repeat][nfis] = stepg;
          cellmaxg[repeat][nfis] = maxg;
          cellDistb[repeat][nfis] = orDistb;
          cellDistg[repeat][nfis] = orDistg;
          
          ffak = 0.0;
          double wgtot = 0.0;
//					int cellNumber = 0;
          for (int ia = 0; ia != maxa; ia += stepa) {
            dist_a = ia * resolutionR * stepa - orDista;
            dist_a *= dist_a;
            for (int ib = 0; ib != maxb; ib += stepb) {
              dist_b = ib * resolutionR * stepb - orDistb;
              dist_b *= dist_b;
              for (int ig = 0; ig != maxg; ig += stepg) {
                dist_g = ig * resolutionR * stepg - orDistg;
                real_dist = dist_a + dist_b + dist_g * dist_g;
                if (real_dist <= dist_factor2) {
                  if (real_dist < 1.0E-4)
                    real_dist = 1.0E-4;
                  wgtcell = 1.0 / Math.sqrt(real_dist);
//									wgtcell = ((dist_factor - Math.sqrt(real_dist)) / dist_factor);
//									wgtcell = *= wgtcell;
                  tmp_index[0] = ia + iaindex[0];
                  tmp_index[1] = ib + iaindex[1];
                  tmp_index[2] = ig + iaindex[2];
                  ffak += getODF(tmp_index) * wgtcell;
                  wgtot += wgtcell;
                }
              }
            }
          }
          if (wgtot == 0.0)
            ffak1 += getODF(iaindex);
          else
            ffak1 += ffak / wgtot;
        }
        if (0 < nfis && nfis < nfismax - 1) {
          if (MoreMath.odd(nfis + 1))
            ffak1 *= 2;
          else
            ffak1 *= 4;
        }
        fs[n1] += ffak1;
      }
      
      fs[n1] *= pisimg / maxRepeat;
      
      startingPoint++;
      for (int n = startingPoint; n < finalPointReference[nref]; n++) {
        // referenceCounter[n] is the point to evaluate
        n1 = referenceCounter[n];
        fs[n1] = 0.;
        
        // Projection thread loop, Simpson integration
        for (nfis = 0; nfis < nfismax; nfis++) {
          double ffak1 = 0.0;
          for (int repeat = 0; repeat < maxRepeat; repeat++) {
            double anglesPhi = phiRef[repeat][nfis] + thetaphi[1][n1];
            iaindex[0] = (int) ((anglesPhi + pi25g) / resolutionR + .000001);
            double anglesGridPhi = iaindex[0] * resolutionR;
            
            iaindex[1] = cellIndex1[repeat][nfis];
            iaindex[2] = cellIndex2[repeat][nfis];
            
            anglesGridPhi = anglesGridPhi - anglesPhi;
            if (anglesGridPhi > 0.0) {
              stepa = -1;
              maxa = -2;
            } else if (anglesGridPhi == 0.0) {
              stepa = 1;
              maxa = 1;
            } else {
              stepa = 1;
              maxa = 2;
            }
            
            stepb = cellstepb[repeat][nfis];
            maxb = cellmaxb[repeat][nfis];
            stepg = cellstepg[repeat][nfis];
            maxg = cellmaxg[repeat][nfis];
            double orDista = -stepa * anglesGridPhi;
            double orDistb = cellDistb[repeat][nfis];
            double orDistg = cellDistg[repeat][nfis];
            
            ffak = 0.0;
            double wgtot = 0.0;
            for (int ia = 0; ia != maxa; ia += stepa) {
              dist_a = ia * resolutionR * stepa - orDista;
              dist_a *= dist_a;
              for (int ib = 0; ib != maxb; ib += stepb) {
                dist_b = ib * resolutionR * stepb - orDistb;
                dist_b *= dist_b;
                for (int ig = 0; ig != maxg; ig += stepg) {
                  dist_g = ig * resolutionR * stepg - orDistg;
                  real_dist = dist_a + dist_b + dist_g * dist_g;
                  if (real_dist <= dist_factor2) {
                    if (real_dist < 1.0E-4)
                      real_dist = 1.0E-4;
                    wgtcell = 1.0 / Math.sqrt(real_dist);
//										wgtcell = ((dist_factor - Math.sqrt(real_dist)) / dist_factor);
//										wgtcell *= wgtcell;
                    tmp_index[0] = ia + iaindex[0];
                    tmp_index[1] = ib + iaindex[1];
                    tmp_index[2] = ig + iaindex[2];
                    ffak += getODF(tmp_index) * wgtcell;
                    wgtot += wgtcell;
                  }
                }
              }
            }
            if (wgtot == 0.0)
              ffak1 += getODF(iaindex);
            else
              ffak1 += ffak / wgtot;
          }
          if (0 < nfis && nfis < nfismax - 1) {
            if (MoreMath.odd(nfis + 1))
              ffak1 *= 2.;
            else
              ffak1 *= 4.;
          }
          fs[n1] += ffak1;
        }
        
        fs[n1] *= pisimg / maxRepeat;
        
      }
      startingPoint = finalPointReference[nref];  // next one
    }
    
//		System.out.println(fs);
    return fs;
  }
  
  public static double[][] calculateCellPathAngles(double theta, double phi,
	                                                 double sthi, double cthi,
	                                                 double fhir, int inv, double[] odfMaxAnglesR, int mdb, boolean cubic7) {
		int nfis, ntfs;
		double ca2, cb2, sa2, g2r;
		double ang;

		int inversion = 1;
		if (inv != 1)
			inversion = 2;
		double[][] fs = new double[4][nfismax * inversion];

// Projection thread loop, Simpson integration

		cb2 = cthi;
		g2r = Constants.PI - fhir;
		boolean checkL13 = false;
		boolean nextCheck = false;
		theta *= Constants.DEGTOPI;
		phi *= Constants.DEGTOPI;
		double cr = Math.cos(theta);
		double sr = Math.sin(theta);
		ntfs = 0;

//    System.out.println("Theta = " + theta + " , Phi = " + phi);
//    System.out.println("Inv = " + inv);

		do {
			while (g2r < 0.) {
				g2r += Constants.PI2;
			}
			for (nfis = 0; nfis < nfismax; nfis++) {
				ang = nfis * integrationStepPFR;
//    System.out.println("Ang = " + nfis);
				ca2 = -Math.cos(ang);
				sa2 = Math.sin(ang);
				double[] angles = Uwimvuo.g20g100(ca2, sa2, cb2, sthi, cr, sr);
				angles[0] += phi;
				angles[2] += g2r;
//        if (angles[1]*Constants.PITODEG <= 15.0)
//    System.out.println("alfa = " + angles[0]*Constants.PITODEG + " , beta = " + angles[1]*Constants.PITODEG + " ,
				// gamma = " + angles[2]*Constants.PITODEG);
				applyCrystalSymmetryAndCheck(angles, odfMaxAnglesR, mdb, cubic7);
//        if (angles[0]*Constants.PITODEG < 5.0 && angles[1]*Constants.PITODEG < 30.0)
//    System.out.println("After symmetry: alfa = " + angles[0]*Constants.PITODEG + " , beta = " + angles[1]*
				// Constants.PITODEG + " , gamma = " + angles[2]*Constants.PITODEG);
				fs[0][ntfs] = angles[0];
				fs[1][ntfs] = angles[1];
				fs[2][ntfs] = angles[2];
				if (!(nfis == 0 || nfis == nfismax - 1)) {
					if (MoreMath.powint(-1, nfis + 1) < 0)
						fs[3][ntfs] = 2.;
					else
						fs[3][ntfs] = 4.;
				} else
					fs[3][ntfs] = 1.;
//	  	  fs[3][ntfs++] /= 3.0;
				fs[3][ntfs++] *= pisimg;
			}
			if (inv == 1 || nextCheck) {
				checkL13 = true;
			} else {
				nextCheck = true;
				cb2 = -cb2;
				g2r -= Constants.PI;
			}
		} while (!checkL13);
		if (inv != 1) {
			for (int i = 0; i < ntfs; i++)
				fs[3][i] /= 2.0;
		}
//    System.out.println("Inv = " + inv);
		return fs;
	}

	public static double[][] calculateCellPathAnglesForCubic(double theta, double phi,
	                                                         double sthi, double cthi,
	                                                         double fhir, int inv, double[] odfMaxAnglesR, int mdb, boolean cubic7) {
		int nfis, ntfs;
		double ca2, cb2, sa2, g2r;
		double ang;

		int inversion = 1;
		if (inv != 1)
			inversion = 2;
		int nfismax = (int) (Constants.PI2 / integrationStepPFR + 1.000001);
		double[][] fs = new double[4][nfismax * inversion * 3];

// Projection thread loop, Simpson integration

		cb2 = cthi;
		g2r = Constants.PI - fhir;
		boolean checkL13 = false;
		boolean nextCheck = false;
		theta *= Constants.DEGTOPI;
		phi *= Constants.DEGTOPI;
		double cr = Math.cos(theta);
		double sr = Math.sin(theta);
		ntfs = 0;

//    System.out.println("Theta = " + theta + " , Phi = " + phi);
//    System.out.println("Inv = " + inv);
		double[][] cubicAngles = new double[2][3];
		do {
			while (g2r < 0.) {
				g2r += Constants.PI2;
			}
			for (nfis = 0; nfis < nfismax; nfis++) {
				ang = nfis * integrationStepPFR;
//    System.out.println("Ang = " + nfis);
				ca2 = -Math.cos(ang);
				sa2 = Math.sin(ang);
				double[] angles = Uwimvuo.g20g100(ca2, sa2, cb2, sthi, cr, sr);
				angles[0] += phi;
				angles[2] += g2r;
//        if (angles[1]*Constants.PITODEG <= 15.0)
//    System.out.println("alfa = " + angles[0]*Constants.PITODEG + " , beta = " + angles[1]*Constants.PITODEG +
				// " , gamma = " + angles[2]*Constants.PITODEG);
				applyCrystalSymmetryAndCheck(angles, odfMaxAnglesR, mdb, cubic7);
				getCubicC3RotationAngles(angles, cubicAngles);
//        if (angles[0]*Constants.PITODEG < 5.0 && angles[1]*Constants.PITODEG < 30.0)
//    System.out.println("After symmetry: alfa = " + angles[0]*Constants.PITODEG + " , beta = " + angles[1]
				// *Constants.PITODEG + " , gamma = " + angles[2]*Constants.PITODEG);
				fs[0][ntfs] = angles[0];
				fs[1][ntfs] = angles[1];
				fs[2][ntfs] = angles[2];
				if (!(nfis == 0 || nfis == nfismax - 1)) {
					if (MoreMath.powint(-1, nfis + 1) < 0)
						fs[3][ntfs] = 2.;
					else
						fs[3][ntfs] = 4.;
				} else
					fs[3][ntfs] = 1.;
				fs[3][ntfs] /= 3.0;
				fs[3][ntfs++] *= pisimg;
				for (int i = 0; i < 2; i++) {
					applyCrystalSymmetryAndCheck(cubicAngles[i], odfMaxAnglesR, mdb, cubic7);
//          if (cubicAngles[i][0]*Constants.PITODEG < 5.0 && cubicAngles[i][1]*Constants.PITODEG < 30.0)
//      System.out.println("After symmetry: alfa = " + cubicAngles[i][0]*Constants.PITODEG + " , beta = " +
					// cubicAngles[i][1]*Constants.PITODEG + " , gamma = " + cubicAngles[i][2]*Constants.PITODEG);
					fs[0][ntfs] = cubicAngles[i][0];
					fs[1][ntfs] = cubicAngles[i][1];
					fs[2][ntfs] = cubicAngles[i][2];
					if (!(nfis == 0 || nfis == nfismax - 1)) {
						if (MoreMath.powint(-1, nfis + 1) < 0)
							fs[3][ntfs] = 2.;
						else
							fs[3][ntfs] = 4.;
					} else
						fs[3][ntfs] = 1.;
					fs[3][ntfs] /= 3.0;
					fs[3][ntfs++] *= pisimg;
				}
			}
			if (inv == 1 || nextCheck) {
				checkL13 = true;
			} else {
				nextCheck = true;
				cb2 = -cb2;
				g2r -= Constants.PI;
			}
		} while (!checkL13);
		if (inv != 1) {
			for (int i = 0; i < ntfs; i++)
				fs[3][i] /= 2.0;
		}
//    System.out.println("Inv = " + inv);
		return fs;
	}

	public OutputPanel entropyOutputFrame = null;
	boolean stillRandomODF = false;

	public void computeTextureFromPF(double[][] experimentalPF) {

		FilePar aparFile = getFilePar();

		prepareiteration(experimentalPF);
//    stillRandomODF = true;

		System.out.println("Computing ODF for phase: " + getPhase().toXRDcatString());

		double minValue = subfmin();
		if (minValue == 1.0f)
			stillRandomODF = true;

		MaximumEntropyFit mem = new MaximumEntropyTextureFit(this, entropyOutputFrame);
		mem.setIterations(getNumberofIterations());
		mem.solve(aparFile.computation);
		mem = null;

		totCellWGT = null;
		totCellID = null;

		fiottu();
		odfNormalization();
//    odfnotLoaded = false;
//		checkFullODF();

//		String filename = new String(getFilePar().getDirectory() +
//															getPhase().toXRDcatString() + ".odf");
//  	ODFoutputStandard(filename, odf, LaueGroupSnumber, getResolutionD());

		refreshComputation = false;

//		setSharpness(computeAndGetSharpness());

//    notifyObjectChanged(this);

		// testing
//  	Phase aphase = getPhase();
//		Reflection refl = (Reflection) aphase.reflectionv.elementAt(0);
//  	poleFigureOutput(odf, aphase, refl.h, refl.k, refl.l, resolution);
	}
  
  public double[] computeTextureFactor(ReflectionTexture reflectionTexture) {
  
  
    if (odf != null) {
      if (useTubeProjection())
        return calculatePFbyTubeProjection(reflectionTexture);
      else
        return calculatePF(reflectionTexture);
    
    }
    return null;
  }
  
  public double[] computeTextureFactor(double[][] texture_angles,
	                                     double[] sctf, double fhir, int inv) {

		if (odf != null) {
			if (useTubeProjection())
				return calculatePFbyTubeProjection(texture_angles, sctf[0], sctf[1], fhir, inv);
			else
				return calculatePF(texture_angles, sctf[0], sctf[1], fhir, inv);

		}
		return null;
	}

	public void computeTextureFactor(Phase aphase, Sample asample) {

//		textureInitialization();
		if (odf == null) { //odfnotLoaded) {
			loadOdfFromFile();
			refreshComputation = true;
		}

		if (!refreshComputation)
			return;

		//  System.out.println("Computing texture factors");

		FilePar aparFile = asample.getFilePar();

		if (aparFile.isTextureComputationPermitted() && ODFisRefinable()) {

//      actualsample = asample;

			prepareiteration(asample);

			boolean toConsole = MaudPreferences.getBoolean("ewimv.sendOutputToConsole", true);
			if (!Constants.textonly || toConsole) {
				entropyOutputFrame = null; // getFilePar().getMainFrame().getOutputPanel();

			}
			System.out.println("Computing ODF for phase: " + getPhase().toXRDcatString());

			double minValue = subfmin();
			if (minValue == 1.0f)
				stillRandomODF = true;

			MaximumEntropyFit mem = new MaximumEntropyTextureFit(this, entropyOutputFrame);
			mem.setIterations(getNumberofIterations());
			mem.solve(aparFile.computation);
			mem = null;

			totCellWGT = null;
			totCellID = null;

			fiottu();
			odfNormalization();
//		checkFullODF();

//    notifyObjectChanged(null);
		}
//    fiottu();
//    odfNormalization();
//		checkFullODF();

		refreshComputation = false;

		if (odf == null)
			return;

		recomputedTextureFactor(aphase, asample, true);
	}

	public void dispose() {
		super.dispose();
	}

	public double[] computeTextureFactor(Phase aphase, double[][] alphabeta, Reflection reflex) {

//    int numberOfPoints = alphabeta.length/2;

		initializeAll();

		if (odf == null)
			return super.computeTextureFactor(aphase, alphabeta, reflex);

		double[] cdsc = aphase.lattice();

//    double phoninp = subfmin();

		double[] sctf = Uwimvuo.tfhkl(reflex.getH(), reflex.getK(), reflex.getL(), cdsc[7], cdsc[5], cdsc[3], cdsc[6], cdsc[0], cdsc[1]);
		double fhir = Math.acos(sctf[3]);
		int inv = Uwimvuo.equiv(LaueGroupSnumber, sctf);

		return computeTextureFactor(alphabeta, sctf, fhir, inv);
	}

	public void fiottu() {
	}

	/**
	 * Compute the two additional symmetrical points in the ODF space for the cubic
	 * crystal groups using the formula from paragraph 5.2 of Siegfried Matthies book:
	 * Standard Distribution in Texture Analysis, vol 1.
	 * <p/>
	 *
	 * @param inputAngles  Euler angles of the original point, in radiants, alpha, beta, gamma
	 * @param outputAngles Euler angles (radiants) of the calculated additional points, first index
	 *                     refers to the point, second to the angle
	 */
	public static void getCubicC3RotationAngles(double[] inputAngles, double[][] outputAngles) {
		// in radiants
		// see chapter 5.2 of Siegfried Book

		double sinBeta = Math.sin(inputAngles[1]);
		double cosBeta = Math.cos(inputAngles[1]);
		double sinGamma = Math.sin(inputAngles[2]);
		double cosGamma = Math.cos(inputAngles[2]);

		double cosBeta_a = sinBeta * sinGamma;
		double sinBeta_a = Math.sqrt(1.0 - cosBeta_a * cosBeta_a);
		double cosBeta_b = sinBeta * cosGamma;
		double sinBeta_b = Math.sqrt(1.0 - cosBeta_b * cosBeta_b);

		// beta_a
		if (sinBeta_a > 1.0E-30) {
			outputAngles[0][1] = Angles.getAngleR(cosBeta_a, sinBeta_a);
			double cosGamma_a = cosBeta / sinBeta_a;
			outputAngles[0][2] = Math.acos(cosGamma_a);
			if (cosGamma < 0.0)   // => sinGamma_a < 0
				outputAngles[0][2] = Constants.PI2 - outputAngles[0][2];
			double x = Math.acos(sinGamma * cosGamma_a);
			if (cosGamma < 0.0)   // => sinX < 0
				x = Constants.PI2 - x;
			outputAngles[0][0] = Math.PI + inputAngles[0] - x;
		} else {
			outputAngles[0][2] = 0.0; //inputAngles[2];   // can be any angle
			if (cosBeta_a > 0) {
				outputAngles[0][1] = 0.0;
				double x = Math.acos(Math.cos(inputAngles[0]) * sinBeta);
				if (Math.sin(inputAngles[0]) < 0.0)   // => sinX < 0
					x = Constants.PI2 - x;
				outputAngles[0][0] = Math.PI + x - outputAngles[0][2];
			} else {
				outputAngles[0][1] = Math.PI;
				double x = Math.acos(-Math.cos(inputAngles[0]) * sinBeta);
				if (Math.sin(inputAngles[0]) > 0.0)   // => sinX < 0
					x = Constants.PI2 - x;
				outputAngles[0][0] = Math.PI + x + outputAngles[0][2];
			}
		}


		// beta_b
		if (sinBeta_b > 1.0E-30) {
			outputAngles[1][1] = Angles.getAngleR(cosBeta_b, sinBeta_b);
			double cosGamma_b = sinBeta * sinGamma / sinBeta_b;
			outputAngles[1][2] = Math.acos(cosGamma_b);
			if (cosBeta < 0.0)   // => sinGamma_b < 0
				outputAngles[1][2] = Constants.PI2 - outputAngles[1][2];
			double x = Math.acos(cosBeta * cosGamma / sinBeta_b);
			if (sinGamma < 0.0)   // => sinX < 0
				x = Constants.PI2 - x;
			outputAngles[1][0] = Math.PI + inputAngles[0] + x;
		} else {
			outputAngles[1][2] = 0.0; //inputAngles[2];   // can be any angle
			if (cosBeta_b > 0) {
				outputAngles[1][1] = 0.0;
				double x = Math.acos(cosBeta * sinGamma);
				if (cosGamma > 0.0)   // => sinX < 0
					x = Constants.PI2 - x;
				outputAngles[1][0] = inputAngles[0] + x - outputAngles[1][2];
			} else {
				outputAngles[1][1] = Math.PI;
				double x = Math.acos(-cosBeta * sinGamma);
				if (cosGamma < 0.0)   // => sinX < 0
					x = Constants.PI2 - x;
				outputAngles[1][0] = inputAngles[0] + x + outputAngles[1][2];
			}
		}
	}

	/**
	 * Compute the two additional symmetrical points in the ODF space for the cubic
	 * crystal groups using the formula from paragraph 5.2 of Siegfried Matthies book:
	 * Standard Distribution in Texture Analysis, vol 1.
	 * <p/>
	 *
	 * @param inputAngles  Euler angles of the original point, in radiants, alpha, beta, gamma
	 * @param outputAngles Euler angles (radiants) of the calculated additional points, first index
	 *                     refers to the point, second to the angle
	 */
	public static void getC3RotationAngles(double[] inputAngles, double[][] outputAngles) {
		// in radiants
		// see chapter 5.2 of Siegfried Book

		double sinAlpha = Math.sin(inputAngles[0]);
		double cosAlpha = Math.cos(inputAngles[0]);
		double sinBeta = Math.sin(inputAngles[1]);
		double cosBeta = Math.cos(inputAngles[1]);
		double sinGamma = Math.sin(inputAngles[2]);
		double cosGamma = Math.cos(inputAngles[2]);
		double[] salpha = new double[3], calpha = new double[3], sbeta = new double[3],
				cbeta = new double[3], sgamma = new double[3], cgamma = new double[3];
		sc3(cosAlpha, sinAlpha, cosBeta, sinBeta, cosGamma, sinGamma,
				salpha, calpha, sbeta, cbeta, sgamma, cgamma);

		for (int i = 1; i < 3; i++) {
			if (Math.abs(salpha[i]) > 1.0E-30) {
				outputAngles[i - 1][0] = Math.acos(calpha[i]);
				if (salpha[i] < 0.0)
					outputAngles[i - 1][0] = Constants.PI2 - outputAngles[i - 1][0];
			} else {
				if (calpha[i] > 0.0)
					outputAngles[i - 1][0] = 0.0;
				else
					outputAngles[i - 1][0] = Math.PI;
			}

			if (Math.abs(sbeta[i]) > 1.0E-30) {
				outputAngles[i - 1][1] = Math.acos(cbeta[i]);
				if (sbeta[i] < 0.0)
					outputAngles[i - 1][1] = Constants.PI2 - outputAngles[i - 1][1];
			} else {
				if (cbeta[i] > 0.0)
					outputAngles[i - 1][1] = 0.0;
				else
					outputAngles[i - 1][1] = Math.PI;
			}

			if (Math.abs(sgamma[i]) > 1.0E-30) {
				outputAngles[i - 1][2] = Math.acos(cgamma[i]);
				if (sgamma[i] < 0.0)
					outputAngles[i - 1][2] = Constants.PI2 - outputAngles[i - 1][2];
			} else {
				if (cgamma[i] > 0.0)
					outputAngles[i - 1][2] = 0.0;
				else
					outputAngles[i - 1][2] = Math.PI;
			}
		}


	}

	public static void sc3(double can, double san, double cbn, double sbn, double cgn, double sgn,
	                       double[] salphal, double[] calphal, double[] sbetal, double[] cbetal,
	                       double[] sgammal, double[] cgammal) {

		double s2, gklein, hilfsg;

//    INPUT COS,SIN (ALPHA0,BETA0,GAMMA0) OF G0
//    CALCULATION OF SIN COS (ALPHA,BETA,GAMMA) FOR
//    THE 96 CUBIC-ORTHORHOMBIC EQUIVALENT POSITIONS

		gklein = 1.0e-16;
		salphal[0] = san;
		calphal[0] = can;
		sbetal[0] = sbn;
		cbetal[0] = cbn;
		sgammal[0] = sgn;
		cgammal[0] = cgn;

//    DETERMINATION ALPHA,BETA,GAMMA

		hilfsg = sbn * sgn;
		cbetal[1] = hilfsg;
		s2 = 1. - hilfsg * hilfsg;
		if (s2 < 0.) {
			s2 = 0.;
		}
		hilfsg = Math.sqrt(s2);
		sbetal[1] = hilfsg;
		if (hilfsg < gklein) {
			cgammal[1] = 1.;
			sgammal[1] = 0.;
			calphal[1] = can * sbn;
			salphal[1] = san * sbn;
			if (cbetal[1] >= 0.) {
				calphal[1] = -calphal[1];
				salphal[1] = -salphal[1];
			}
		} else {
			cgammal[1] = cbn / hilfsg;
			sgammal[1] = sbn * cgn / hilfsg;
			calphal[1] = -(can * cbn * sgn + san * cgn) / hilfsg;
			salphal[1] = (-san * cbn * sgn + can * cgn) / hilfsg;
		}

//    DETERMINATION ALPHAB,BETAB,GAMMAB

		hilfsg = sbn * cgn;
		cbetal[2] = hilfsg;
		s2 = 1. - hilfsg * hilfsg;
		if (s2 < 0.) {
			s2 = 0.;
		}
		hilfsg = Math.sqrt(s2);
		sbetal[2] = hilfsg;
		if (hilfsg < gklein) {
			cgammal[2] = 1.;
			sgammal[2] = 0.;
			calphal[2] = can * cbn * sgn + san * cgn;
			salphal[2] = san * cbn * sgn - can * cgn;
			if (cbetal[2] > 0.) {
				calphal[2] = -calphal[2];
				salphal[2] = -salphal[2];
			}
		} else {
			cgammal[2] = sbn * sgn / hilfsg;
			sgammal[2] = cbn / hilfsg;
			calphal[2] = -(can * cbn * cgn - san * sgn) / hilfsg;
			salphal[2] = -(san * cbn * cgn + can * sgn) / hilfsg;
		}

	}

	public double getLowerBound(int index) {
		return -1.0E30f;
	}

	public double getUpperBound(int index) {
		return 1.0E30f;
	}

	public double getParameterMinSignificantValue(int i) {
		return 0;
	}

	private void outputODF(double threshold) {
		for (int na = 0; na < alphama; na++)
			for (int nb = 0; nb < betama; nb++)
				for (int ng = 0; ng < alphama; ng++)
					if (odf[na][nb][ng] > threshold)
						System.out.println(((0.5 + na) * resolution) + " " +
								((0.5 + nb) * resolution) + " " +
								((0.5 + ng) * resolution) + " " +
								odf[na][nb][ng]);
	}

	private void setODFzeros() {
		for (int ng = 0; ng < alphama; ng++)
			for (int nb = 0; nb < betama; nb++)
				for (int na = 0; na < alphama; na++)
					if (!odf_covered[na][nb][ng])
						odf[na][nb][ng] = 0;
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
		return new JMEMTextureOptionsD(parent, this);
	}

	public class JMEMTextureOptionsD extends JOptionsDialog {

		JComboBox symmetryCB;
		JComboBox resolutionCB;
		JCheckBox refinableCB;
		JSlider iterationJS;
		JLabel sharpL = null;
		JTextField thresholdTF;
		JTextField thresholdODFValueTF;
		String[] resolutions = {"15", "10", "7.5", "6", "5", "3.75", "3", "2.5", "2", "1.5", "1.25", "1",
				"0.75", "0.5", "0.25", "0.125", "0.1"};

		public JMEMTextureOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new BorderLayout(3, 3));
			JPanel upperPanel = new JPanel();
			upperPanel.setLayout(new FlowLayout());
			upperPanel.add(new JLabel("Iterations number: "));
			JLabel iterationTF = new JLabel();
			iterationJS = new JSlider();
			iterationJS.setToolTipText("Set the maximum number of iterations (default is 10)");
			SliderListener listener = new SliderListener(iterationTF);
			iterationJS.addChangeListener(listener);
			upperPanel.add(iterationTF);
			upperPanel.add(iterationJS);
			principalPanel.add(BorderLayout.NORTH, upperPanel);

			JPanel lowerPanel = new JPanel();
			lowerPanel.setLayout(new GridLayout(0, 2, 3, 3));
			principalPanel.add(BorderLayout.CENTER, lowerPanel);
			JPanel jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel8);
			jPanel8.add(new JLabel("Generate symmetry: "));
			symmetryCB = new JComboBox();
			for (int i = 0; i < symmetrychoicenumber; i++)
				symmetryCB.addItem(symmetrychoice[i]);
			symmetryCB.setToolTipText("Set up unmeasured sample symmetries");
			jPanel8.add(symmetryCB);

			jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel8);
			jPanel8.add(new JLabel("ODF resolution in degrees: "));
			resolutionCB = new JComboBox();
			for (String resolution1 : resolutions) resolutionCB.addItem(resolution1);
			resolutionCB.setToolTipText("Choose the ODF cells resolution");
			jPanel8.add(resolutionCB);

			jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel8);
			refinableCB = new JCheckBox("ODF refinable");
			refinableCB.setToolTipText("Uncheck this box if the ODF should not be modify");
			jPanel8.add(refinableCB);

			jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel8);
			JButton jb = new JButton("Reset ODF");
			jb.setToolTipText("Press this to reset the ODF to a random one");
			jPanel8.add(jb);
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					resetODF();
				}
			});
			jb = new JButton("Sharpen ODF");
			jb.setToolTipText("Press this to sharp the ODF by setting the exponent, > 1 sharpen, < 1 unsharp");
			jPanel8.add(jb);
			thresholdTF = new JTextField(8);
			thresholdTF.setText("1.0");
			jPanel8.add(thresholdTF);
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					double threshold = Double.parseDouble(thresholdTF.getText());
					sharpODF(threshold);
					odfNormalization();
				}
			});

			JPanel jPanel10 = new JPanel();
			jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel10);
			jPanel10.add(new JLabel("Import standard odf:"));

			jPanel10.add(jb = new JButton("Beartex"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					importODFfromBEARTEX();
				}
			});
			jb.setToolTipText("Press this to load an odf using the Beartex/Maud exchange format (.maa)");

			jPanel10 = new JPanel();
			jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel10);
			jPanel10.add(new JLabel("Export ODF formatted (text) for "));
			jPanel10.add(jb = new JButton("Beartex"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					exportODFtoBEARTEX();
				}
			});
			jb.setToolTipText("Press this to save the odf using the Beartex/Maud exchange format");

			jPanel10 = new JPanel();
			jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel10);
			jPanel10.add(new JLabel("Export PFs (.xpc) for "));
			jPanel10.add(jb = new JButton("Beartex"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					exportPFsinBEARTEXformat();
				}
			});
			jb.setToolTipText("Press this to save the PFs using the Beartex format");

			jPanel10 = new JPanel();
			jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel10);
			jPanel10.add(jb = new JButton("ODF from PF"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					(new PersistentThread() {
						public void executeJob() {
							loadPFandComputeODF(JMEMTextureOptionsD.this);
						}
					}).start();
				}
			});
			jb.setToolTipText("Press this to compute the ODF from traditional Pole Figures");

			jPanel10 = new JPanel();
			jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel10);
			jPanel10.add(jb = new JButton("Compute"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					setCursor(new Cursor(Cursor.WAIT_CURSOR));
					setSharpness(computeAndGetSharpness());
					sharpL.setText("Texture index (F2): " + getSharpness());
					setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				}
			});
			jb.setToolTipText("Press this to compute the Texture index (F2)");
			jPanel10.add(sharpL = new JLabel("Texture index (F2): " + getSharpness()));

			jPanel10 = new JPanel();
			jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel10);
			jPanel10.add(new JLabel("Actual ODF coverage: " + getODFcoverage() + " %"));

			jPanel10 = new JPanel();
			jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 3));
			lowerPanel.add(jPanel10);
			jPanel10.add(jb = new JButton("Set 0 uncovered cells"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					setODFzeros();
				}
			});
			jb.setToolTipText("Force to zero the cells not covered experimentally (warning, do only if you know what it means)");

			jPanel10 = new JPanel();
			jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel10);
			jPanel10.add(new JLabel("Plot: "));
			jPanel10.add(jb = new JButton("ODF weights"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					(new PersistentThread() {
						public void executeJob() {
							plotODFWeightsHystogram(JMEMTextureOptionsD.this);
						}
					}).start();
				}
			});
			jb.setToolTipText("Press this to display a plot with the ODF cell weighted hits");
			jPanel10.add(jb = new JButton("ODF hits"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					(new PersistentThread() {
						public void executeJob() {
							plotODFHitsHystogram(JMEMTextureOptionsD.this);
						}
					}).start();
				}
			});
			jb.setToolTipText("Press this to display a plot with the ODF cell hits");
			jPanel10.add(jb = new JButton("2D Plot ODF hits"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					(new PersistentThread() {
						public void executeJob() {
							plotODFHitsMap();
						}
					}).start();
				}
			});
			jb.setToolTipText("Press this to display a 2D plot with the ODF cell weighted hits");

			jPanel10 = new JPanel();
			jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel10);
			jb = new JButton("Save ODF cells with values > ");
			jb.setToolTipText("Press this to save in output a list of cells with content over a certain value");
			jPanel10.add(jb);
			thresholdODFValueTF = new JTextField(8);
			thresholdODFValueTF.setText("1.0");
			jPanel10.add(thresholdODFValueTF);
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					double threshold = Double.parseDouble(thresholdODFValueTF.getText());
					outputODF(threshold);
				}
			});

			jPanel10 = new JPanel();
			jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel10);
			jPanel10.add(jb = new JButton("EWIMV advanced options"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					(new PersistentThread() {
						public void executeJob() {
							JOptionsDialog adialog = new JMEMAdvancedTextureOptionsD(JMEMTextureOptionsD.this,
									JMEMTextureOptionsD.this.XRDparent);
							adialog.setVisible(true);
						}
					}).start();
				}
			});
			jb.setToolTipText("Press this to display the advanced options panel");


			setTitle("E-WIMV options panel");
			initParameters();
			pack();
			iterationJS.setValue(getNumberofIterations());
		}

		public void initParameters() {
			iterationJS.setMaximum(41);
			iterationJS.setMinimum(1);
			iterationJS.setValue(41);
			iterationJS.setPaintTicks(true);
			iterationJS.setMajorTickSpacing(10);
			iterationJS.setMinorTickSpacing(1);
			iterationJS.setPaintLabels(true);
			iterationJS.setSnapToTicks(true);
			iterationJS.setLabelTable(iterationJS.createStandardLabels(5));
			symmetryCB.setSelectedItem(getSampleSymmetry());
			refinableCB.setSelected(ODFisRefinable());
			for (int i = 0; i < resolutions.length; i++) {
				double res1 = Double.parseDouble(resolutions[i]);
				double res2 = Double.parseDouble(getResolution());
				if (Math.abs(res1 - res2) / res2 < 0.0001)
					resolutionCB.setSelectedIndex(i);
			}
		}

		public void retrieveParameters() {
			setSampleSymmetry(symmetryCB.getSelectedItem().toString());
			setODFrefinable(refinableCB.isSelected());
			setResolution(resolutionCB.getSelectedItem().toString());
			setNumberofIterations(iterationJS.getValue());
		}

		public void importODFfromBEARTEX() {
			String filename = Utility.browseFilename(this, "load ODF file from Beartex (.maa)");
			String resolutionS = ODFinputStandard(filename);
			if (resolutionS != null)
				resolutionCB.setSelectedItem(resolutionS);

		}

		public void exportPFsinBEARTEXformat() {
			final String filename = Utility.browseFilenametoSave(this, "choose a file for PFs in BEARTEX format (.xpc)");
			(new PersistentThread() {
				public void executeJob() {
					PoleFigureOutput pfOutput = new PoleFigureOutput(filename, getPhase());
					pfOutput.computeAndWrite();
				}
			}).start();
		}

		public void exportODFtoBEARTEX() {
			String filename = Utility.browseFilenametoSave(this, "export ODF file for Beartex (use .maa extension)");
			ODFoutputStandard(filename);
		}

	}

	class SliderListener implements ChangeListener {
		JLabel tf;

		public SliderListener(JLabel f) {
			tf = f;
		}

		public void stateChanged(ChangeEvent e) {
			JSlider s1 = (JSlider) e.getSource();
			tf.setText(Integer.toString(s1.getValue()));
		}
	}

	public class JMEMAdvancedTextureOptionsD extends JOptionsDialog {

		JTextField minIntTF;
		JCheckBox statusCB;
		//    JCheckBox refinableCB;
		JCheckBox tubeCB;
		//    JCheckBox phonCB;
		JCheckBox convCB;
		JTextField rexpTF;
		JCheckBox normCB;
		//    JLabel sharpL = null;
		JTextField tubeRTF;
		JTextField weightsTF;
		JTextField thresholdTF;

		public JMEMAdvancedTextureOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new BorderLayout(3, 3));

			JPanel lowerPanel = new JPanel();
			lowerPanel.setLayout(new GridLayout(0, 2, 3, 3));
			principalPanel.add(BorderLayout.CENTER, lowerPanel);
			JPanel jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel8);
			jPanel8.add(new JLabel("Min reflex intensity: "));
			minIntTF = new JTextField(Constants.FLOAT_FIELD);
			minIntTF.setToolTipText("Minimum value of intensity for a reflection to be included (respect to max)");
			jPanel8.add(minIntTF);

			jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel8);
			jPanel8.add(new JLabel("Minimun reflection d-spacing: "));
			thresholdTF = new JTextField(Constants.FLOAT_FIELD);
			thresholdTF.setToolTipText("Use only reflections with d-space bigger than this value");
			jPanel8.add(thresholdTF);

			if (Constants.testing) {
				jPanel8 = new JPanel();
				jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
				lowerPanel.add(jPanel8);
				jPanel8.add(new JLabel("Rexponent value: "));
				rexpTF = new JTextField(Constants.FLOAT_FIELD);
				rexpTF.setToolTipText("Set the starting exponent value");
				jPanel8.add(rexpTF);
			}
			jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel8);
			jPanel8.add(new JLabel("Tube weight: "));
			tubeRTF = new JTextField(Constants.FLOAT_FIELD);
			tubeRTF.setToolTipText("Enter the tube weight function exponent (0.0-1.0, 0.5 best)");
			jPanel8.add(tubeRTF);

			JPanel jPanel9 = new JPanel();
			jPanel9.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel9);
			statusCB = new JCheckBox("Real coincidences unique PF");
			statusCB.setToolTipText("Check the box to let Entropy use real coincidence superposed as a unique pole figure");
			jPanel9.add(statusCB);

			jPanel9 = new JPanel();
			jPanel9.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel9);
			tubeCB = new JCheckBox("Use tube projection");
			tubeCB.setToolTipText("Check the box to let Entropy use a smoothing tube projection");
			jPanel9.add(tubeCB);

			if (Constants.testing) {
				jPanel9 = new JPanel();
				jPanel9.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
				lowerPanel.add(jPanel9);
				convCB = new JCheckBox("Store angular conversion");
				convCB.setToolTipText("Check the box to store the angular conversion; use more memory but it's faster");
				jPanel9.add(convCB);

			}

			jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel8);
			normCB = new JCheckBox("Normalize pole figures");
			normCB.setToolTipText("If set, the pole figures will be normalized during the iterations");
			jPanel8.add(normCB);

			jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel8);
			jPanel8.add(new JLabel("Weights exponent: "));
			weightsTF = new JTextField(Constants.FLOAT_FIELD);
			weightsTF.setToolTipText("If > 0 it uses the reflection intensity weight for ODF computation, 1 for linear weighting");
			jPanel8.add(weightsTF);
			
			setTitle("E-WIMV advanced options panel");
			initParameters();
			pack();
		}

		public void initParameters() {
			statusCB.setSelected(getWIMVstatus());
			tubeCB.setSelected(useTubeProjection());
			if (Constants.testing)
				convCB.setSelected(storeConversion());
			minIntTF.setText(getMinimumIntensity());
			thresholdTF.setText(getMinimumDspacing());
			weightsTF.setText(getWeightsExponentS());
			normCB.setSelected(normalizePoleFigures());
			if (Constants.testing)
				rexpTF.setText(getWIMVOption(1));
			tubeRTF.setText(getTubeWeight());
//      phonCB.setSelected(usePhon());
		}

		public void retrieveParameters() {
			normalizePoleFigures(normCB.isSelected());
			setWIMVstatus(statusCB.isSelected());
			useTubeProjection(tubeCB.isSelected());
			if (Constants.testing)
				storeConversion(convCB.isSelected());
			setMinimumIntensity(minIntTF.getText());
			setMinimumDspacing(thresholdTF.getText());
			if (Constants.testing)
				setWIMVOption(1, rexpTF.getText());
			setTubeWeight(tubeRTF.getText());
//      usePhon(phonCB.isSelected());
			setWeightsExponent(weightsTF.getText());
		}

	}

}
