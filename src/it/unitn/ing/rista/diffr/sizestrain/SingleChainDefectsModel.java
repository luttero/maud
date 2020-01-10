/*
 * @(#)SingleChainDefectsModel.java created June 19, 2014 Caen
 *
 * Copyright (c) 2014 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.diffr.sizestrain;

import it.unitn.ing.jsginfo.Sghkl;
import it.unitn.ing.jsginfo.T_Eq_hkl;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.JOptionsDialog;

import javax.swing.*;
import java.awt.*;
import java.util.Vector;

/**
 * The SingleChainDefectsModel is a class to implement
 * the single chain model for polymers fibers by Luca Lutterotti
 * ideally disordered structures
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 19, 2014 9:36:26 PM $
 * @since JDK1.1
 */
public class SingleChainDefectsModel extends PlanarDefects {

	// Dictionary, here we define the CIF string for each parameter and option in the model, will be used for
	// saving and loading files
	public static String[] diclistc = {"_maud_supercell_repetition_number",
			"_maud_polymers_fiber_axis", "_maud_polymers_faulted_axis",
			"_maud_minimum_d_space",

			"_riet_crystallite_factor",
			"_riet_microstrain_factor"
	};

	// this are the corresponding labels that will appear in the GUI in the parameter list window etc.
	public static String[] diclistcrm = {"number of cell repetition for the supercell",
			"crystal axis along the fiber direction",
			"crystal axis with the faulting",
			"minimum energy for disordering",

			"crystallite size increase parallel to the fiber",
			"microstrain decrease parallel to the fiber"
	};

	// this model does not have subobjects, so the class list for subobjects is empty
	public static String[] classlistcs = {};
	public static String[] classlistc = {};

	// here we define the model string to appear in the GUI and the description string
	// change it accordingly to your method, it should be an unique identifier
	final static String id = "Polymer single chain";
	final static String desc = "select this to compute polymer chain ending disorder by the model of Lutterotti et al";


	static final String[] axis = {"a", "b", "c"};

	int axisIndex = 0, fault_axisIndex = 1, neutral_axisIndex = 3, layersNumber = 10;//, layersNumber2 = 100;
	double reciprocal_factor = .1;
	double[] normalization = null;
	double min_d = 0.0;

	// Constructors, and init methods do not change the code between the two stars lines
	// **********************************************************************************

	public SingleChainDefectsModel(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initXRD();
		identifier = id;
		IDlabel = id;
		description = desc;
	}

	public SingleChainDefectsModel(XRDcat aobj) {
		this(aobj, "Polymer chain disordered model");
	}

	public SingleChainDefectsModel() {
		identifier = id;
		IDlabel = id;
		description = desc;
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

	// **********************************************************************************
	// Changes are allowed and needed below

	public void initConstant() {
		Nstring = 4;    // number of options, treated as strings only, in this case only the first
		Nstringloop = 0;  // no vectors of strings for options
		Nparameter = 2;   // 2 parameters refinables in the model
		Nparameterloop = 0;  // no parameter vectors in this model, to be used when the number of parameters may
		// change in the model and/or is defined by other options
		Nsubordinate = 0;    // no subobjects or subordinate objects
		Nsubordinateloop = 0;  // no vectors of subobjects
	}

	// we init the parameters here

	public void initParameters() {
		super.initParameters();

		stringField[0] = "10";
		stringField[1] = axis[axisIndex];
		stringField[2] = axis[fault_axisIndex];
		stringField[3] = Float.toString((float) min_d);

		parameterField[0] = new Parameter(this, getParameterString(0), 1,
				ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.01),
				ParameterPreferences.getDouble(getParameterString(0) + ".max", 100));
		parameterField[1] = new Parameter(this, getParameterString(1), 1,
				ParameterPreferences.getDouble(getParameterString(1) + ".min", 0.01),
				ParameterPreferences.getDouble(getParameterString(1) + ".max", 100));

		refreshComputation = true; // we specify the computation need to be refreshed (it has never done up to now)
	}

	public double getStructureFactorModifier(int h, int k, int l) {
		int[] hkl = {h, k, l};

//		if (hkl[fault_axisIndex] != 0 &&
//				hkl[axisIndex] == 0 && hkl[neutral_axisIndex] == 0)
//			return 1;
		if (hkl[axisIndex] == 0 || hkl[fault_axisIndex] == 0)
			return 1;

		return reciprocal_factor;
	}

	public boolean acceptReflection(int h, int k, int l) {
		int[] hkl = {h, k, l};
		return acceptReflection(hkl);
	}

	public boolean acceptReflection(int[] hkl) {
		if (hkl[axisIndex] == 0 && hkl[fault_axisIndex] % layersNumber != 0)
			return false;
		return true;
	}

	public void updateStringtoDoubleBuffering(boolean firstLoading) {
		layersNumber = Integer.parseInt(stringField[0]);
		min_d = Double.parseDouble(stringField[3]);
		reciprocal_factor = 1.0 / layersNumber;
		axisIndex = 0;
		fault_axisIndex = 1;
		for (int i = 0; i < axis.length; i++)
			if (stringField[1].equalsIgnoreCase(axis[i]))
				axisIndex = i;
		for (int i = 0; i < axis.length; i++)
			if (stringField[2].equalsIgnoreCase(axis[i]))
				fault_axisIndex = i;
		neutral_axisIndex = 0;
		if (neutral_axisIndex == fault_axisIndex || neutral_axisIndex == axisIndex)
			neutral_axisIndex++;
		if (neutral_axisIndex == fault_axisIndex || neutral_axisIndex == axisIndex)
			neutral_axisIndex++;
		divideFactor[fault_axisIndex] = reciprocal_factor;
		divideFactor[neutral_axisIndex] = 1;
		divideFactor[axisIndex] = 1;
		normalization = new double[layersNumber];

	}

	public int getFactor() {
		return layersNumber;
	}

	public int getSuperCellFactor(int index) {
		if (index == fault_axisIndex)
			return getFactor();
		return 1;
	}

	public double getCrystalliteFactor(int h, int k, int l) {
		int[] hkl = {h, k, l};
		if (hkl[axisIndex] != 0 && hkl[fault_axisIndex] == 0 && hkl[neutral_axisIndex] == 0)
			return getParameterValue(0);
		else
			return 1;
	}

	public double getMicrostrainFactor(int h, int k, int l) {
		int[] hkl = {h, k, l};
		if (hkl[axisIndex] != 0 && hkl[fault_axisIndex] == 0 && hkl[neutral_axisIndex] == 0)
			return getParameterValue(1);
		else
			return 1;
	}

/*	public Vector<double[]> computeReflectionsList(Phase phase, Sghkl sghkl,int totNumber, double dmin,
	                                               boolean permitAbsent, double sumOverlapped) {

		Vector<double[]> reflectionList = null;

		if (reflectionList.size() <= 0) {

			int friedelLaw = 1;
			int h, k, l; //restriction;
			int[] Minh = new int[1], Mink = new int[1], Minl = new int[1];
			double[] soVector = phase.getOriginalSoVector();
			int Maxh = totNumber / 4 + 1;
			int Maxk = Maxh, Maxl = Maxh;
//	  System.out.println(this.getLabel() + " Reflection list: " + dmin);
			if (totNumber == 0) {
				Maxh = (int) (1.0 / (Math.sqrt(soVector[0]) * dmin)) + 1;
				Maxk = (int) (1.0 / (Math.sqrt(soVector[1]) * dmin)) + 1;
				Maxl = (int) (1.0 / (Math.sqrt(soVector[2]) * dmin)) + 1;
			}
			int totNumber4 = totNumber * 10;

			do {
				sghkl.SetListMin_hkl(friedelLaw, Maxh, Maxk, Maxl, Minh, Mink, Minl);
				reflectionList = new Vector<double[]>(totNumber4, 100);
				for (h = Minh[0]; h <= Maxh; h++) {
					for (k = Mink[0]; k <= Maxk; k++) {
						for (l = Minl[0]; l <= Maxl; l++) {
							if (sghkl.IsSysAbsent_hkl(h, k, l, null) == 0) {
								if ((sghkl.IsHidden_hkl(friedelLaw, Minh[0], Mink[0], Minl[0],
										Maxh, Maxk, Maxl, h, k, l)) == 0) {
									if (!((h == 0 && k == 0) && l == 0) && acceptReflection(h, k, l)) {
										double[] dpi = {1.0 / Math.sqrt(soVector[0] * h * h + soVector[1] * k * k + soVector[2] * l * l + 2.
												* soVector[5] * h * k + 2. * soVector[3] * k * l + 2. * soVector[4] * h * l)};
										if (sumOverlapped < 0.0)
											reflectionList.addElement(dpi);
										else {
											boolean found = false;
											for (int i = 0; i < reflectionList.size(); i++) {
												double[] doubles = reflectionList.elementAt(i);
												if (Math.abs(doubles[0] - dpi[0]) <= sumOverlapped) {
													found = true;
													break;
												}
											}
											if (!found)
												reflectionList.addElement(dpi);
										}
									}
								}
							} else if (permitAbsent) {
								if ((sghkl.IsHidden_hkl(friedelLaw, Minh[0], Mink[0], Minl[0],
										Maxh, Maxh, Maxh, h, k, l)) == 0) {
									if (!((h == 0 && k == 0) && l == 0) && acceptReflection(h, k, l)) {
										double[] dpi = {1.0 / Math.sqrt(soVector[0] * h * h + soVector[1] * k * k + soVector[2] * l * l + 2.
												* soVector[5] * h * k + 2. * soVector[3] * k * l + 2. * soVector[4] * h * l)};
										if (sumOverlapped < 0.0)
											reflectionList.addElement(dpi);
										else {
											boolean found = false;
											for (int i = 0; i < reflectionList.size(); i++) {
												double[] doubles = reflectionList.elementAt(i);
												if (Math.abs(doubles[0] - dpi[0]) <= sumOverlapped) {
													found = true;
													break;
												}
											}
											if (!found)
												reflectionList.addElement(dpi);
										}
									}
								}
							}
						}
					}
				}
				Maxh *= 2;
			} while (reflectionList.size() < totNumber);

		}

		return reflectionList;
	} */

	public boolean checkSghkllist(Phase aphase, Sghkl sghkl, double dplanecut, double dplanestart) {
//		boolean model = MaudPreferences.getBoolean("singleChainDefectModel.useFullModel", true);
//		if (model)
//			return checkSghkllist_full(aphase, sghkl, dplanecut, dplanestart);
//		else
			return checkSghkllist_red(aphase, sghkl, dplanecut, dplanestart);
	}

	public boolean checkSghkllist_red(Phase aphase, Sghkl sghkl, double dplanecut, double dplanestart) {
		int im, M2;
		int h, k, l, M;
		int Maxh, Maxk, Maxl;
		int[] Minh = new int[1], Mink = new int[1], Minl = new int[1];
		double dpi;
		int[] hlist = new int[24], klist = new int[24], llist = new int[24];
		int[] jhlist;
		int[] jklist;
		int[] jllist;
		T_Eq_hkl Eq_hkl = new T_Eq_hkl();
		int friedelLaw = 1;

		double[] soVector = aphase.getSoVector();
		double[] o_soVector = aphase.getOriginalSoVector();

//		int[] multiplier = {getSuperCellFactor(0), getSuperCellFactor(1), getSuperCellFactor(2)};

		Maxh = (int) (1.0 / (Math.sqrt(o_soVector[0]) * dplanecut)) + 1;
		Maxk = (int) (1.0 / (Math.sqrt(o_soVector[1]) * dplanecut)) + 1;
		Maxl = (int) (1.0 / (Math.sqrt(o_soVector[2]) * dplanecut)) + 1;

		boolean showWarningMaxIndex = MaudPreferences.getBoolean("millerIndices.showWarning", true);
		int maxIndexPermitted = MaudPreferences.getInteger("millerIndices.maxValue", 100);
		if (Maxh > maxIndexPermitted) {
			Maxh = maxIndexPermitted;
			if (showWarningMaxIndex)
				System.out.println(
						"Warning: the h miller index was exceeding the max permitted value (modifiable in the preferences); " +
								"try to restrict the computing range or change max value");
		}
		if (Maxk > maxIndexPermitted) {
			Maxk = maxIndexPermitted;
			if (showWarningMaxIndex)
				System.out.println(
						"Warning: the k miller index was exceeding the max permitted value (modifiable in the preferences); " +
								"try to restrict the computing range or change max value");
		}
		if (Maxl > maxIndexPermitted) {
			Maxl = maxIndexPermitted;
			if (showWarningMaxIndex)
				System.out.println(
						"Warning: the l miller index was exceeding the max permitted value (modifiable in the preferences); " +
								"try to restrict the computing range or change max value");
		}

		sghkl.SetListMin_hkl(friedelLaw, Maxh, Maxk, Maxl, Minh, Mink, Minl);

		Vector<Reflection> reflectionv = aphase.getReflectionVector();
		reflectionv.removeAllElements();  // todo remove only when necessary (changes in the model)

		for (int i = 0; i < reflectionv.size(); i++) {
			Reflection refl = reflectionv.elementAt(i);
			dpi = 1.0 / Math.sqrt(o_soVector[0] * refl.getBaseH2() + o_soVector[1] * refl.getBaseK2() + o_soVector[2] * refl.getBaseL2()
					+ 2. * o_soVector[5] * refl.getBaseH() * refl.getBaseK() + 2. * o_soVector[3] * refl.getBaseK() * refl.getBaseL()
					+ 2. * o_soVector[4] * refl.getBaseH() * refl.getBaseL());
			if (dpi < dplanecut || dpi > dplanestart) {
//        System.out.println("Removing " + refl.h +" "+refl.k+" "+refl.l + " " + refl + " " +dpi);
				reflectionv.removeElementAt(i);
				i--;
			}
		}
		int numberReflections = reflectionv.size();
		boolean[] alreadyChecked = new boolean[numberReflections];
		for (int i = 0; i < numberReflections; i++)
			alreadyChecked[i] = false;
		int minimumChecked = 0;

		for (h = Minh[0]; h <= Maxh; h++) {
			for (k = Mink[0]; k <= Maxk; k++) {
				for (l = Minl[0]; l <= Maxl; l++) {
					if (sghkl.IsSysAbsent_hkl(h, k, l, null) == 0) {
						if (sghkl.IsHidden_hkl(friedelLaw, Minh[0], Mink[0], Minl[0],
								Maxh, Maxk, Maxl, h, k, l) == 0) {
							if (!((h == 0 && k == 0) && l == 0)) {
								dpi = 1.0 / Math.sqrt(o_soVector[0] * h * h + o_soVector[1] * k * k + o_soVector[2] * l * l + 2.
										* o_soVector[5] * h * k + 2. * o_soVector[3] * k * l + 2. * o_soVector[4] * h * l);
								if (dpi >= dplanecut && dpi <= dplanestart) {
//									System.out.println("Checking " + h +" "+k+" "+l + " " +dpi);
									int[] hkl = {h, k, l};
									int factor = getFactor(); // (h, k, l);
									int startIndex = 1;
									int minus1 = 1;
									int abshkl = Math.abs(hkl[fault_axisIndex]);
									int sign = 1;
									if (hkl[fault_axisIndex] < 0)
										sign = -1;
									if (hkl[axisIndex] == 0 ||
											(hkl[axisIndex] == 0 && hkl[neutral_axisIndex] == 0) ||
											dpi < min_d) {
										startIndex = factor;
										for (int i = 0; i < layersNumber; i++)
											normalization[i] = 1.0;
									} else {
										for (int i = 0; i < layersNumber; i++)
											normalization[i] = 1.0 / layersNumber;
									}
//									System.out.println("Iterating " + h +" "+k+" "+l + " " +startIndex);
									for (int index = startIndex; index <= factor; index++) {
										double intensityFraction = normalization[index - startIndex];
										hkl[fault_axisIndex] = sign * ((abshkl - minus1) * factor + index);
/*										if (sghkl.IsSysAbsent_hkl(hkl[0], hkl[1], hkl[2], null) == 0 &&
												sghkl.IsHidden_hkl(friedelLaw, Minh[0], Mink[0], Minl[0],
														Maxh * index, Maxk * index, Maxl * index, hkl[0], hkl[1], hkl[2]) == 0 &&
												!(hkl[0] == 0 && hkl[1] == 0 && hkl[2] == 0)) {*/

//											System.out.println("Passed " + hkl[0] +" "+hkl[1]+" "+hkl[2]);
											dpi = 1.0 / Math.sqrt(soVector[0] * hkl[0] * hkl[0] + soVector[1] * hkl[1] * hkl[1] + soVector[2] * hkl[2] * hkl[2] + 2.
													* soVector[5] * hkl[0] * hkl[1] + 2. * soVector[3] * hkl[1] * hkl[2] + 2. * soVector[4] * hkl[0] * hkl[2]);

											int found = -1;
											for (int i = minimumChecked; i < numberReflections; i++) {
												if (!alreadyChecked[i]) {
													Reflection refl = reflectionv.elementAt(i);
													if (refl.equalsTo(hkl[0], hkl[1], hkl[2])) {
														found = i;
														break;
													}
												} else {
													if (i == minimumChecked)
														minimumChecked++;
												}
											}
											if (found < 0) {
												boolean reallyNew = true;
												for (int i = numberReflections; i < reflectionv.size(); i++) {
													Reflection refl = reflectionv.elementAt(i);
													if (refl.equalsTo(hkl[0], hkl[1], hkl[2])) {
														reallyNew = false;
//													System.out.println("Are equal " + h1 +" "+k1+" "+l1 + " " + refl.getH() + " " + refl.getK() + " " + refl.getL() + " " + refl.d_space);
														break;
													}
												}

												if (reallyNew) {
													M = sghkl.BuildEq_hkl(friedelLaw, Eq_hkl, hkl[0], hkl[1], hkl[2]);
													M2 = M / 2;
													for (im = 0; im < M2; im++) {
														hlist[im] = Eq_hkl.h[im];
														klist[im] = Eq_hkl.k[im];
														llist[im] = Eq_hkl.l[im];
													}
													jhlist = new int[M2];
													jklist = new int[M2];
													jllist = new int[M2];
													for (int i = 0; i < M2; i++) {
														jhlist[i] = hlist[i];
														jklist[i] = klist[i];
														jllist[i] = llist[i];
													}
													Reflection refl = new Reflection(aphase, jhlist, jklist, jllist, M, dpi, h, k, l);
													refl.setStructureModifier(intensityFraction);
													refl.setDividers(divideFactor[0], divideFactor[1], divideFactor[2]);
													reflectionv.addElement(refl);
//												System.out.println("check, New Reflection " + hkl[0] +" "+hkl[1]+" "+hkl[2] + " " + refl.d_space + " " + h + " " + k + " " + l);
												}
											} else {
												alreadyChecked[found] = true;
												Reflection refl = reflectionv.elementAt(found);
												refl.setDSpace(dpi);
												refl.refreshforUpdate();
//                    System.out.println("check, Old Reflection " + h +" "+k+" "+l + " " + refl);
											}
										}
//									}
								}
							}
						}
					}
				}
			}
		}
		for (int i = numberReflections - 1; i >= 0; i--)
			if (!alreadyChecked[i])
				reflectionv.removeElementAt(i);

		return true;
	}

	public boolean checkSghkllist_full(Phase aphase, Sghkl sghkl, double dplanecut, double dplanestart) {

		int im, M2;
		int h, k, l, M;
		int Maxh, Maxk, Maxl;
		int[] Minh = new int[1], Mink = new int[1], Minl = new int[1];
		double dpi;
		int[] hlist = new int[24], klist = new int[24], llist = new int[24];
		int[] jhlist;
		int[] jklist;
		int[] jllist;
		T_Eq_hkl Eq_hkl = new T_Eq_hkl();
		int friedelLaw = 1;

		double[] soVector = aphase.getSoVector();

		Maxh = (int) (1.0 / (Math.sqrt(soVector[0]) * dplanecut)) + 1;
		Maxk = (int) (1.0 / (Math.sqrt(soVector[1]) * dplanecut)) + 1;
		Maxl = (int) (1.0 / (Math.sqrt(soVector[2]) * dplanecut)) + 1;

		boolean showWarningMaxIndex = MaudPreferences.getBoolean("millerIndices.showWarning", true);
		int maxIndexPermitted = MaudPreferences.getInteger("millerIndices.maxValue", 100);
		if (Maxh > maxIndexPermitted) {
			Maxh = maxIndexPermitted;
			if (showWarningMaxIndex)
				System.out.println(
						"Warning: the h miller index was exceeding the max permitted value (modifiable in the preferences); " +
								"try to restrict the computing range or change max value");
		}
		if (Maxk > maxIndexPermitted) {
			Maxk = maxIndexPermitted;
			if (showWarningMaxIndex)
				System.out.println(
						"Warning: the k miller index was exceeding the max permitted value (modifiable in the preferences); " +
								"try to restrict the computing range or change max value");
		}
		if (Maxl > maxIndexPermitted) {
			Maxl = maxIndexPermitted;
			if (showWarningMaxIndex)
				System.out.println(
						"Warning: the l miller index was exceeding the max permitted value (modifiable in the preferences); " +
								"try to restrict the computing range or change max value");
		}

		sghkl.SetListMin_hkl(friedelLaw, Maxh, Maxk, Maxl, Minh, Mink, Minl);

		Vector<Reflection> reflectionv = aphase.getReflectionVector();
		reflectionv.removeAllElements();  // todo remove only when necessary (changes in the model)

		for (int i = 0; i < reflectionv.size(); i++) {
			Reflection refl = reflectionv.elementAt(i);
			dpi = 1.0 / Math.sqrt(soVector[0] * refl.getH2() + soVector[1] * refl.getK2() + soVector[2] * refl.getL2()
					+ 2. * soVector[5] * refl.getH() * refl.getK() + 2. * soVector[3] * refl.getK() * refl.getL()
					+ 2. * soVector[4] * refl.getH() * refl.getL());
			if (dpi < dplanecut || dpi > dplanestart) {
//        System.out.println("Removing " + refl.h +" "+refl.k+" "+refl.l + " " + refl + " " +dpi);
				reflectionv.removeElementAt(i);
				i--;
			}
		}
		int numberReflections = reflectionv.size();
		boolean[] alreadyChecked = new boolean[numberReflections];
		for (int i = 0; i < numberReflections; i++)
			alreadyChecked[i] = false;
		int minimumChecked = 0;

		for (h = Minh[0]; h <= Maxh; h++) {
			for (k = Mink[0]; k <= Maxk; k++) {
				for (l = Minl[0]; l <= Maxl; l++) {
//					if (sghkl.IsSysAbsent_hkl(h, k, l, null) == 0) {
//						if ((sghkl.IsHidden_hkl(friedelLaw, Minh[0], Mink[0], Minl[0],
//								Maxh, Maxk, Maxl, h, k, l)) == 0) {
					if (!((h == 0 && k == 0) && l == 0) && acceptReflection(h, k, l)) {
						dpi = 1.0 / Math.sqrt(soVector[0] * h * h + soVector[1] * k * k + soVector[2] * l * l + 2.
								* soVector[5] * h * k + 2. * soVector[3] * k * l + 2. * soVector[4] * h * l);
						if (dpi >= dplanecut && dpi <= dplanestart) {
							int found = -1;
							for (int i = minimumChecked; i < numberReflections; i++) {
								if (!alreadyChecked[i]) {
									Reflection refl = reflectionv.elementAt(i);
									if (refl.equalsTo(h, k, l)) {
										found = i;
										break;
									}
								} else {
									if (i == minimumChecked)
										minimumChecked++;
								}
							}
							if (found < 0) {
								M = sghkl.BuildEq_hkl(friedelLaw, Eq_hkl, h, k, l);
								M2 = M / 2;
								for (im = 0; im < M2; im++) {
									hlist[im] = Eq_hkl.h[im];
									klist[im] = Eq_hkl.k[im];
									llist[im] = Eq_hkl.l[im];
								}
								jhlist = new int[M2];
								jklist = new int[M2];
								jllist = new int[M2];
								for (int i = 0; i < M2; i++) {
									jhlist[i] = hlist[i];
									jklist[i] = klist[i];
									jllist[i] = llist[i];
								}
								Reflection refl = new Reflection(aphase, jhlist, jklist, jllist, M, dpi, jhlist[0], jklist[0], jllist[0]);
								refl.setStructureModifier(getStructureFactorModifier(jhlist[0], jklist[0], jllist[0]));
								refl.setDividers(divideFactor[0], divideFactor[1], divideFactor[2]);
								reflectionv.addElement(refl);
//										System.out.println("check, New Reflection " + h +" "+k+" "+l + " " + refl);
							} else {
								alreadyChecked[found] = true;
								Reflection refl = reflectionv.elementAt(found);
								refl.setDSpace(dpi);
								refl.refreshforUpdate();
//                    System.out.println("check, Old Reflection " + h +" "+k+" "+l + " " + refl);
							}
						}
					}
//						}
//					}
				}
			}
		}
		for (int i = numberReflections - 1; i >= 0; i--)
			if (!alreadyChecked[i])
				reflectionv.removeElementAt(i);

		return true;
	}

	// this part implements the option window to manipulate the parameters and options
	// you need first to change the name of the Dialog inner class and then if you just have some different
	// parameters changing only the labels (number and strings is sufficient)
	// if you have additional options (stringField[]) then you may want to manage also them inside

	public JOptionsDialog getOptionsDialog(Frame parent) {
		return new JPCOptionsD(parent, this);
	}

	class JPCOptionsD extends JOptionsDialog {

		JTextField layerTF = null;
		JTextField[] pars = null;
		JComboBox axisCB;
		JComboBox fault_axisCB;
		JTextField dminTF = null;

		public JPCOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new BorderLayout(6, 6));
			JPanel jPanel8 = new JPanel();
			jPanel8.setLayout(new GridLayout(0, 2, 3, 3));
			principalPanel.add(BorderLayout.NORTH, jPanel8);


			jPanel8.add(new JLabel("Number of cell repetitions: "));
			layerTF = new JTextField(Constants.FLOAT_FIELD);
			layerTF.setText("10");
			layerTF.setToolTipText("Set the number of cell repetition along the fiber axis");
			jPanel8.add(layerTF);

			jPanel8.add(new JLabel("Fiber axis: "));
			axisCB = new JComboBox();
			for (String resolution1 : axis) axisCB.addItem(resolution1);
			axisCB.setToolTipText("Choose the crystal axis parallel to the fiber direction");
			jPanel8.add(axisCB);

			jPanel8.add(new JLabel("Fault axis: "));
			fault_axisCB = new JComboBox();
			for (String resolution1 : axis) fault_axisCB.addItem(resolution1);
			fault_axisCB.setToolTipText("Choose the fault axis perpendicular to the fiber direction");
			jPanel8.add(fault_axisCB);

			jPanel8.add(new JLabel("d min: "));
			jPanel8.add(dminTF = new JTextField(Constants.FLOAT_FIELD));
			dminTF.setText("0.0");
			dminTF.setToolTipText("No faulting effects for reflection with d space lower than this value");


			String[] labels = {"Crystallite factor: ",
					"Microstrain factor: "};
			pars = new JTextField[labels.length];

			for (int i = 0; i < labels.length; i++) {
				jPanel8.add(new JLabel(labels[i]));
				pars[i] = new JTextField(Constants.FLOAT_FIELD);
				pars[i].setText("0");
				jPanel8.add(pars[i]);
			}

			setTitle("Single chain disorder model options panel");
			initParameters();
			pack();

		}

		public void initParameters() {
			axisCB.setSelectedIndex(axisIndex);
			fault_axisCB.setSelectedIndex(fault_axisIndex);
			dminTF.setText(stringField[3]);
			for (int i = 0; i < pars.length; i++) {
				pars[i].setText(parameterField[i].getValue());
				addComponenttolist(pars[i], parameterField[i]);
			}
			layerTF.setText(stringField[0]);

		}

		public void retrieveParameters() {
			stringField[0] = layerTF.getText();
			stringField[1] = (axisCB.getSelectedItem().toString());
			stringField[2] = (fault_axisCB.getSelectedItem().toString());
			stringField[3] = dminTF.getText();
			for (int i = 0; i < pars.length; i++) {
				parameterField[i].setValue(pars[i].getText());
			}
			updateStringtoDoubleBuffering(false);
		}

	}

}
