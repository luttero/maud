/*
 * @(#)ModulatedTurbostraticModel.java created June 23, 2014 Hermanville
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
import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;
import java.util.Vector;

/**
 * The ModulatedTurbostraticModel is a class to implement
 * the modulated turbostratic model for clays (e.g. Kaolinite)
 * starting from the Ufer single layer model
 * ideally disordered structures
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 23, 2014 11:24:54 PM $
 * @since JDK1.1
 */
public class ModulatedTurbostraticModel extends PlanarDefects {

	// Dictionary, here we define the CIF string for each parameter and option in the model, will be used for
	// saving and loading files
	public static String[] diclistc = {
			"_riet_stacking_layers_number", "_riet_stacking_layers_axis", "_maud_minimum_d_space",
			"_maud_modulated_axis_first", "_maud_modulated_n_first", "_maud_modulated_constant_first", "_maud_turbostratic_order_first",
			"_maud_modulated_axis_second", "_maud_modulated_n_second", "_maud_modulated_constant_second", "_maud_turbostratic_order_second",

			"_riet_crystallite_factor",
			"_riet_microstrain_factor",
			"_riet_crystallite_factor_mod_first",
			"_riet_microstrain_factor_mod_first",
			"_riet_crystallite_factor_mod_second",
			"_riet_microstrain_factor_mod_second"
	};

	// this are the corresponding labels that will appear in the GUI in the parameter list window etc.
	public static String[] diclistcrm = {"number of layers for the disordered model",
			"crystal axis normal to the stacking sequence", "minimum energy for disordering",
			"indices of the first modulation axis", "period of the first modulation", "constant of the first modulation", "first turbostratic order",
			"indices of the second modulation axis","period of the second modulation", "constant of the second modulation", "second turbostratic order",

			"crystallite size increase parallel to the fiber",
			"microstrain decrease parallel to the fiber",
			"crystallite size increase first modulation",
			"microstrain decrease first modulation",
			"crystallite size increase second modulation",
			"microstrain decrease second modulation"
	};

	// this model does not have subobjects, so the class list for subobjects is empty
	public static String[] classlistcs = {};
	public static String[] classlistc = {};

	// here we define the model string to appear in the GUI and the description string
	// change it accordingly to your method, it should be an unique identifier
	final static String id = "Modulated turbostratic";
	final static String desc = "select this to compute a modulated turbostratic disorder";


	static final String[] axis = {"a", "b", "c"};
	static final String[] m_axis = {"a", "b", "c", "none"};

	int axisIndex = 2, layersNumber = 30;//, layersNumber2 = 100;

	int[] neutral_axis = {0, 1};
	int[] modulated_axis = {0, 1};
	int[] modulated_n = {3, 3};
	int[] modulated_constant = {0, 99999};
	double reciprocal_factor = .1;
	double[] normalization = null;
	double min_d = 2.7;
	int[] turbostraticOrder = {99999, 99999};

	// Constructors, and init methods do not change the code between the two stars lines
	// **********************************************************************************

	public ModulatedTurbostraticModel(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initXRD();
		identifier = id;
		IDlabel = id;
		description = desc;
	}

	public ModulatedTurbostraticModel(XRDcat aobj) {
		this(aobj, "Modulated turbostratic model");
	}

	public ModulatedTurbostraticModel() {
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
		Nstring = 11;    // number of options, treated as strings only, in this case only the first
		Nstringloop = 0;  // no vectors of strings for options
		Nparameter = 6;   // 2 parameters refinables in the model
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
		stringField[2] = Float.toString((float) min_d);
		stringField[3] = axis[modulated_axis[0]];
		stringField[4] = Integer.toString(modulated_n[0]);
		stringField[5] = Integer.toString(modulated_constant[0]);
		stringField[6] = Integer.toString(turbostraticOrder[0]);
		stringField[7] = axis[modulated_axis[1]];
		stringField[8] = Integer.toString(modulated_n[1]);
		stringField[9] = Integer.toString(modulated_constant[1]);
		stringField[10] = Integer.toString(turbostraticOrder[1]);

		parameterField[0] = new Parameter(this, getParameterString(0), 1,
				ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.01),
				ParameterPreferences.getDouble(getParameterString(0) + ".max", 100));
		parameterField[1] = new Parameter(this, getParameterString(1), 1,
				ParameterPreferences.getDouble(getParameterString(1) + ".min", 0.01),
				ParameterPreferences.getDouble(getParameterString(1) + ".max", 100));
		parameterField[2] = new Parameter(this, getParameterString(2), .1,
				ParameterPreferences.getDouble(getParameterString(2) + ".min", 0.01),
				ParameterPreferences.getDouble(getParameterString(2) + ".max", 100));
		parameterField[3] = new Parameter(this, getParameterString(3), 1,
				ParameterPreferences.getDouble(getParameterString(3) + ".min", 0.01),
				ParameterPreferences.getDouble(getParameterString(3) + ".max", 100));
		parameterField[4] = new Parameter(this, getParameterString(4), .1,
				ParameterPreferences.getDouble(getParameterString(4) + ".min", 0.01),
				ParameterPreferences.getDouble(getParameterString(4) + ".max", 100));
		parameterField[5] = new Parameter(this, getParameterString(5), 1,
				ParameterPreferences.getDouble(getParameterString(5) + ".min", 0.01),
				ParameterPreferences.getDouble(getParameterString(5) + ".max", 100));

		refreshComputation = true; // we specify the computation need to be refreshed (it has never done up to now)
	}

	/**
	 * Executed when a Parameter of this object is changed. Overwrite the same method in XRDcat.
	 * In this case add to the message that an atom position has changed flag if it is the case.
	 *
	 * @param source the Parameter that has changed its value
	 */
	public void notifyParameterChanged(Parameter source) {
		FilePar filepar = getFilePar();
		if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
			for (int i = 0; i < parameterField.length; i++) {
				if (parameterField[i] == source) {
					if (i > 1) {
						notifyParameterChanged(source, Constants.STRUCTURE_FACTOR_CHANGED);
						((Phase) getParent()).refreshReflectionv = true;
						return;
					}
				}
			}
			super.notifyParameterChanged(source);
		}
	}

	public void updateStringtoDoubleBuffering(boolean firstLoading) {
		layersNumber = Integer.parseInt(stringField[0]);
		min_d = Double.parseDouble(stringField[2]);
		reciprocal_factor = 1.0 / layersNumber;
		axisIndex = 0;
		modulated_axis[0] = 3;
		modulated_axis[1] = 3;

		int index = 0;
		for (int i = 0; i < 3; i++)
			divideFactor[i] = 1;
		for (int i = 0; i < axis.length; i++) {
			if (stringField[1].equalsIgnoreCase(axis[i])) {
				axisIndex = i;
				divideFactor[axisIndex] = reciprocal_factor;
			} else
				neutral_axis[index++] = i;
		}
		for (int i = 0; i < m_axis.length; i++) {
			if (stringField[3].equalsIgnoreCase(m_axis[i])) {
				modulated_axis[0] = i;
			}
			if (stringField[7].equalsIgnoreCase(m_axis[i])) {
				modulated_axis[1] = i;
			}
		}

		// safe check
		if (axisIndex == modulated_axis[1]) {
			// resetting
			modulated_axis[1] = 3;
			stringField[6] = m_axis[3];
		}
		if (axisIndex == modulated_axis[0]) {
			// resetting
			if (modulated_axis[1] == 3) {
				modulated_axis[0] = 3;
				stringField[3] = m_axis[3];
			} else {
				modulated_axis[0] = modulated_axis[1];
				for (int j = 3; j < 7; j++)
					stringField[j] = stringField[j + 4];
				modulated_axis[1] = 3;
				stringField[7] = m_axis[3];
				stringField[10] = "99999";
			}
		}
		if (modulated_axis[1] != 3 && modulated_axis[0] == modulated_axis[1]) {
			// resetting
			modulated_axis[1] = 3;
			stringField[6] = m_axis[3];
		}

		for (int i = 0; i < 2; i++) {
			modulated_n[i] = Integer.parseInt(stringField[4 + i * 4]);
			modulated_constant[i] = Integer.parseInt(stringField[5 + i * 4]);
			turbostraticOrder[i] = Integer.parseInt(stringField[6 + i * 4]);
		}

		if (modulated_axis[0] == neutral_axis[0]) {
			int temp = neutral_axis[0];
			neutral_axis[0] = neutral_axis[1];
			neutral_axis[1] = temp;
		}

		normalization = new double[layersNumber];

	}

	public double getStructureFactorModifier(int h, int k, int l) {
		int[] hkl = {h, k, l};

//		if (hkl[fault_axisIndex] != 0 &&
//				hkl[axisIndex] == 0 && hkl[neutral_axisIndex] == 0)
//			return 1;
		if (hkl[axisIndex] != 0 && hkl[neutral_axis[0]] == 0 && hkl[neutral_axis[1]] == 0)
			return 1;
/*		for (int i = 0; i < 2; i++)
			if (modulated_axis[i] != 3)
				if (hkl[modulated_axis[i]] != 0 && hkl[neutral_axis[i]] % modulated_n[i] != modulated_constant[i])
					return 1;*/

		return reciprocal_factor;
	}

	public boolean acceptReflection(int h, int k, int l) {
		int[] hkl = {h, k, l};
		return acceptReflection(hkl);
	}

	public boolean acceptReflection(int[] hkl) {
		if (hkl[axisIndex] % layersNumber != 0 && hkl[neutral_axis[0]] == 0 && hkl[neutral_axis[1]] == 0)
			return false;
/*		for (int i = 0; i < 2; i++)
			if (modulated_axis[i] != 3)
				if (hkl[modulated_axis[i]] % layersNumber != 0 && hkl[neutral_axis[i]] % modulated_n[i] != modulated_constant[i])
					return false;*/
		return true;
	}

	public int getFactor() {
		return layersNumber;
	}

	public int getSuperCellFactor(int index) {
		if (index == axisIndex)
			return getFactor();
		return 1;
	}

	public double getCrystalliteFactor(int h, int k, int l) {
		int[] hkl = {h, k, l};
		if (hkl[axisIndex] != 0 && hkl[neutral_axis[0]] == 0 && hkl[neutral_axis[1]] == 0)
			return getParameterValue(0);
		else
			return 1;
	}

	public double getMicrostrainFactor(int h, int k, int l) {
		int[] hkl = {h, k, l};
		if (hkl[axisIndex] != 0 && hkl[neutral_axis[0]] == 0 && hkl[neutral_axis[1]] == 0)
			return getParameterValue(1);
		else
			return 1;
	}

	public boolean checkSghkllist(Phase aphase, Sghkl sghkl, double dplanecut, double dplanestart) {
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

//		for (int i = 0; i < 6; i++)
//			System.out.println(soVector[i] + " " + o_soVector[i]);

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
									int[] hkl = {h, k, l, 0};
									int abshkl = Math.abs(hkl[axisIndex]);
									int factor = getFactor(); // (h, k, l);
									int startIndex = 1;
									int minus1 = 1;
									int sign = 1;
									if (hkl[axisIndex] < 0)
										sign = -1;
									if ((hkl[neutral_axis[0]] == 0 && hkl[neutral_axis[1]] == 0) ||
											(modulated_axis[0] < 3 && hkl[modulated_axis[0]] != 0 &&
													Math.abs(hkl[modulated_axis[0]]) % modulated_n[0] == modulated_constant[0] &&
													Math.abs(hkl[modulated_axis[0]]) != turbostraticOrder[0]) ||
											(modulated_axis[1] < 3 && hkl[modulated_axis[1]] != 0 &&
													Math.abs(hkl[modulated_axis[1]]) % modulated_n[1] == modulated_constant[1] &&
													Math.abs(hkl[modulated_axis[1]]) != turbostraticOrder[1]) ||
											dpi < min_d) {
										startIndex = factor;
										for (int i = 0; i < layersNumber; i++)
											normalization[i] = 1.0;
									} else {
										if (modulated_axis[0] < 3 && Math.abs(hkl[modulated_axis[0]]) == turbostraticOrder[0] ||
												modulated_axis[1] < 3 && Math.abs(hkl[modulated_axis[1]]) == turbostraticOrder[1]) {
											for (int i = 0; i < layersNumber; i++)
												normalization[i] = 1.0 / layersNumber;
										} else {
											double sum = 0.0;
											double hwhm = ((getParameterValue(2) + getParameterValue(3) * Math.abs(hkl[modulated_axis[0]])) +
													(getParameterValue(4) + getParameterValue(5) * Math.abs(hkl[modulated_axis[1]]))) / layersNumber;
											hwhm *= hwhm;
											for (int i = 0; i < layersNumber; i++) {
												normalization[layersNumber - i - 1] = 1.0 - hwhm * i * i;
												if (normalization[layersNumber - i - 1] < 0)
													normalization[layersNumber - i - 1] = 0;
												sum += normalization[i];
											}
											for (int i = 0; i < layersNumber; i++)
												normalization[i] /= sum;
										}
									}
									for (int index = startIndex; index <= factor; index++) {
										double intensityFraction = normalization[index - startIndex];

										hkl[axisIndex] = sign * ((abshkl - minus1) * factor + index);

											dpi = 1.0 / Math.sqrt(soVector[0] * hkl[0] * hkl[0] + soVector[1] * hkl[1] * hkl[1] +
													soVector[2] * hkl[2] * hkl[2] + 2. * soVector[5] * hkl[0] * hkl[1] +
													2. * soVector[3] * hkl[1] * hkl[2] + 2. * soVector[4] * hkl[0] * hkl[2]);

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
												}
											} else {
												alreadyChecked[found] = true;
												Reflection refl = reflectionv.elementAt(found);
												refl.setDSpace(dpi);
												refl.refreshforUpdate();
											}
//										}
									}
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
		JComboBox[] axisCB = null;
		JTextField[] nTF = null;
		JTextField[] constantTF = null;
		JTextField[] orderTF = null;

		public JPCOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new BorderLayout(6, 6));
			JPanel jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout());
			principalPanel.add(BorderLayout.NORTH, jPanel8);


			jPanel8.add(new JLabel("Number of cell repetitions: "));
			layerTF = new JTextField(Constants.FLOAT_FIELD);
			layerTF.setText("10");
			layerTF.setToolTipText("Set the number of layers in the stacking sequence");
			jPanel8.add(layerTF);

			String[] tabLabels = {"Stacking direction", "First modulation", "Second modulation"};
			JTabbedPane jtp = new JTabbedPane();
			principalPanel.add(BorderLayout.CENTER, jtp);

			axisCB = new JComboBox[tabLabels.length];
			nTF = new JTextField[tabLabels.length];
			constantTF = new JTextField[tabLabels.length - 1];
			orderTF = new JTextField[tabLabels.length - 1];
			String[] labels = {"Crystallite factor: ", "Microstrain factor: "};
			String[] modulatedlabels = {"Broadening constant: ", "Broadening slope: "};
			pars = new JTextField[labels.length * 3];

			JPanel jp2 = new JPanel(new FlowLayout());
			JPanel jp1 = new JPanel(new GridLayout(0, 2, 3, 3));
			jp2.add(jp1);
			jtp.addTab(tabLabels[0], null, jp2);
			jp1.add(new JLabel("Axis: "));
			axisCB[0] = new JComboBox();
			for (String resolution1 : axis) axisCB[0].addItem(resolution1);
			axisCB[0].setToolTipText("Choose the crystal axis normal to the stacking sequence");
			jp1.add(axisCB[0]);
			jp1.add(new JLabel("d min: "));
			jp1.add(nTF[0] = new JTextField(Constants.FLOAT_FIELD));
			nTF[0].setText("0.0");
			nTF[0].setToolTipText("No faulting effects for reflection with d space lower than this value");
			for (int i = 0; i < labels.length; i++) {
				jp1.add(new JLabel(labels[i]));
				pars[i] = new JTextField(Constants.FLOAT_FIELD + 4);
				pars[i].setText("1");
				jp1.add(pars[i]);
			}

			jp1 = new JPanel(new GridLayout(0, 2, 3, 3));
			jtp.addTab(tabLabels[1], null, jp1);
			jp1.add(new JLabel("Axis: "));
			axisCB[1] = new JComboBox();
			for (String resolution1 : m_axis) axisCB[1].addItem(resolution1);
			axisCB[1].setToolTipText("Choose the crystal axis for the first modulation (not the stacking axis)");
			jp1.add(axisCB[1]);
			jp1.add(new JLabel("Modulation period: "));
			jp1.add(nTF[1] = new JTextField(4));
			nTF[1].setText("3");
			jp1.add(new JLabel("Modulation constant: "));
			jp1.add(constantTF[0] = new JTextField(4));
			constantTF[0].setText("0");
			jp1.add(new JLabel("Turbostratic order: "));
			jp1.add(orderTF[0] = new JTextField(4));
			orderTF[0].setText("99999");
			orderTF[0].setToolTipText("Set the order for the turbostratic faulted plane, set 99999 to not use it");
			for (int i = 0; i < labels.length; i++) {
				jp1.add(new JLabel(modulatedlabels[i]));
				pars[i+2] = new JTextField(Constants.FLOAT_FIELD + 4);
				pars[i+2].setText("1");
				jp1.add(pars[i+2]);
			}

			jp1 = new JPanel(new GridLayout(0, 2, 3, 3));
			jtp.addTab(tabLabels[2], null, jp1);
			jp1.add(new JLabel("Axis: "));
			axisCB[2] = new JComboBox();
			for (String resolution1 : m_axis) axisCB[2].addItem(resolution1);
			axisCB[2].setToolTipText("Choose the crystal axis for the second modulation (not the stacking axis)");
			jp1.add(axisCB[2]);
			jp1.add(new JLabel("Modulation period: "));
			jp1.add(nTF[2] = new JTextField(4));
			nTF[2].setText("3");
			jp1.add(new JLabel("Modulation constant: "));
			jp1.add(constantTF[1] = new JTextField(4));
			constantTF[1].setText("0");
			jp1.add(new JLabel("Turbostratic order: "));
			jp1.add(orderTF[1] = new JTextField(4));
			orderTF[1].setText("99999");
			orderTF[1].setToolTipText("Set the order for the turbostratic faulted plane, set 99999 to not use it");
			for (int i = 0; i < labels.length; i++) {
				jp1.add(new JLabel(modulatedlabels[i]));
				pars[i+4] = new JTextField(Constants.FLOAT_FIELD + 4);
				pars[i+4].setText("1");
				jp1.add(pars[i+4]);
			}

			setTitle("Modulated turbostratic model options panel");
			initParameters();
			pack();

		}

		public void initParameters() {
			axisCB[0].setSelectedIndex(axisIndex);
			nTF[0].setText(stringField[2]);
			for (int i = 0; i < 2; i++) {
				axisCB[i + 1].setSelectedIndex(modulated_axis[i]);
				nTF[i + 1].setText(stringField[4 + i * 4]);
				constantTF[i].setText(stringField[5 + i * 4]);
				orderTF[i].setText(stringField[6 + i * 4]);
			}
			for (int i = 0; i < pars.length; i++) {
				pars[i].setText(parameterField[i].getValue());
				addComponenttolist(pars[i], parameterField[i]);
			}
			layerTF.setText(stringField[0]);

		}

		public void retrieveParameters() {
			stringField[0] = layerTF.getText();
			stringField[1] = axisCB[0].getSelectedItem().toString();
			stringField[2] = nTF[0].getText();
			for (int i = 0; i < 2; i++) {
				stringField[3 + i * 4] = axisCB[1 + i].getSelectedItem().toString();
				stringField[4 + i * 4] = nTF[i + 1].getText();
				stringField[5 + i * 4] = constantTF[i].getText();
				stringField[6 + i * 4] = orderTF[i].getText();
			}
			for (int i = 0; i < pars.length; i++) {
				parameterField[i].setValue(pars[i].getText());
			}
			updateStringtoDoubleBuffering(false);
		}

	}

}
