/*
 * @(#)PlanarDefectsReciprocalModel.java created October 1, 2013 Caen
 *
 * Copyright (c) 2013 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.interfaces.Peak;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.JOptionsDialog;

import javax.swing.*;
import java.awt.*;

/**
 * The PlanarDefectsReciprocalModel is a class to implement
 * a modulated disordered structure in 2D
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Oct 1, 2013 02:49:36 PM $
 * @since JDK1.1
 */
public class PlanarDefectsReciprocalModel extends PlanarDefects {

	// Dictionary, here we define the CIF string for each parameter and option in the model, will be used for
	// saving and loading files
	public static String[] diclistc = {"_riet_stacking_modulation_order_a",
			"_riet_stacking_modulation_order_b",
			"_riet_stacking_modulation_order_c",
			"_riet_stacking_zero_amplitude_a",
			"_riet_stacking_zero_amplitude_b",
			"_riet_stacking_zero_amplitude_c",
			"_riet_stacking_modulation_amplitude_a",
			"_riet_stacking_modulation_amplitude_b",
			"_riet_stacking_modulation_amplitude_c"
	};

	// this are the corresponding labels that will appear in the GUI in the parameter list window etc.
	public static String[] diclistcrm = {"_riet_stacking_modulation_order_a",
			"_riet_stacking_modulation_order_b",
			"_riet_stacking_modulation_order_c",
			"_riet_stacking_zero_amplitude_a",
			"_riet_stacking_zero_amplitude_b",
			"_riet_stacking_zero_amplitude_c",
			"_riet_stacking_modulation_amplitude_a",
			"_riet_stacking_modulation_amplitude_b",
			"_riet_stacking_modulation_amplitude_c"
	};

	// this model does not have subobjects, so the class list for subobjects is empty
	public static String[] classlistcs = {};
	public static String[] classlistc = {};

	// here we define the model string to appear in the GUI and the description string
	// change it accordingly to your method, it should be an unique identifier
	final static String id = "Disabled Reciprocal lattice";
	final static String desc = "select this to compute planar disorder by the reciprocal lattice model";

	int[] orderModulation = new int[3];

	// Constructors, and init methods do not change the code between the two stars lines
	// **********************************************************************************

	public PlanarDefectsReciprocalModel(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initXRD();
		identifier = id;
		IDlabel = id;
		description = desc;
	}

	public PlanarDefectsReciprocalModel(XRDcat aobj) {
		this(aobj, "Reciprocal lattice model");
	}

	public PlanarDefectsReciprocalModel() {
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
		Nstring = 3;    // number of options, treated as strings only, in this case only the first
		Nstringloop = 0;  // no vectors of strings for options
		Nparameter = 6;   // # parameters refinables in the model
		Nparameterloop = 0;  // no parameter vectors in this model, to be used when the number of parameters may
		// change in the model and/or is defined by other options
		Nsubordinate = 0;    // no subobjects or subordinate objects
		Nsubordinateloop = 0;  // no vectors of subobjects
	}

	// we init the parameters here

	public void initParameters() {
		super.initParameters();

		for (int i = 0; i < 3; i++)
			stringField[i] = "1";

		for (int i = 0; i < 6; i++)
			parameterField[i] = new Parameter(this, getParameterString(i), 0,
					ParameterPreferences.getDouble(getParameterString(i) + ".min", 0.0),
					ParameterPreferences.getDouble(getParameterString(i) + ".max", 1));

		refreshComputation = true; // we specify the computation need to be refreshed (it has never done up to now)
	}

	public void updateStringtoDoubleBuffering(boolean firstLoading) {
		for (int i = 0; i < 3; i++)
			orderModulation[i] = Integer.parseInt(stringField[i]);
	}

	public double getCrystalliteFactor(int h, int k, int l) {
		return 1;
	}

	public double getMicrostrainFactor(int h, int k, int l) {
		return 1;
	}

	public double getPlanarDefectAsymmetry(Reflection refl) {
		double asy = 0;
		int[] h = new int[3];
		h[0] = Math.abs(refl.getH());
		h[1] = Math.abs(refl.getK());
		h[2] = Math.abs(refl.getL());

		for (int i = 0; i < 3; i++)
			if (h[i] != 0 && h[i] % orderModulation[i] > 0) {
				if (asy < Math.abs(parameterValues[3 + i]))
					asy = Math.abs(parameterValues[3 + i]);
			} else if (h[i] == 0 && orderModulation[i] != 1) {
				for (int j = 0; j < 3; j++) {
					if (j != i && orderModulation[j] != 1) {
						if (h[j] % orderModulation[j] > 0 && asy < Math.abs(parameterValues[j]))
							asy = Math.abs(parameterValues[j]);
					}
				}
			}

		if (asy < 1.0E-6)
			asy = 0;
		if (asy > 1.0E6)
			asy = 1.0E6;
		System.out.println(h[0] + " " + h[1] + " " + h[2] + " " + asy);
		return asy;
	}

	// this part implements the option window to manipulate the parameters and options
	// you need first to change the name of the Dialog inner class and then if you just have some different
	// parameters changing only the labels (number and strings is sufficient)
	// if you have additional options (stringField[]) then you may want to manage also them inside

	public JOptionsDialog getOptionsDialog(Frame parent) {
		return new JSLOptionsD(parent, this);
	}

	class JSLOptionsD extends JOptionsDialog {

		JTextField[] orderTF = null;
		JTextField[] pars = null;

		public JSLOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new BorderLayout(6, 6));
			JPanel jPanel8 = new JPanel();
			jPanel8.setLayout(new GridLayout(0, 2, 3, 3));
			principalPanel.add(BorderLayout.NORTH, jPanel8);

			jPanel8.add(new JLabel("Modulation order:"));
			jPanel8.add(new JLabel(""));
			String[] labels = {"a: ", "b: ", "c: "};
			orderTF = new JTextField[labels.length];
			for (int i = 0; i < labels.length; i++) {
				jPanel8.add(new JLabel(labels[i]));
				orderTF[i] = new JTextField(Constants.FLOAT_FIELD);
				orderTF[i].setText("1");
				orderTF[i].setToolTipText("1: stacking axis (ordered), >1000: turbostratic disorder");
				jPanel8.add(orderTF[i]);
			}

			jPanel8.add(new JLabel("Zero amplitudes: "));
			jPanel8.add(new JLabel(""));

			pars = new JTextField[2 * labels.length];

			for (int i = 0; i < labels.length; i++) {
				jPanel8.add(new JLabel(labels[i]));
				pars[i] = new JTextField(Constants.FLOAT_FIELD);
				pars[i].setText("0");
				pars[i].setToolTipText("0: no broadening (ordered), >1: too high broadening");
				jPanel8.add(pars[i]);
			}

			jPanel8.add(new JLabel("Modulation amplitudes: "));
			jPanel8.add(new JLabel(""));

			for (int i = 0; i < labels.length; i++) {
				jPanel8.add(new JLabel(labels[i]));
				pars[i + labels.length] = new JTextField(Constants.FLOAT_FIELD);
				pars[i + labels.length].setText("0");
				pars[i + labels.length].setToolTipText("0: no broadening (ordered), >1: too high broadening");
				jPanel8.add(pars[i + labels.length]);
			}

			setTitle("Reciprocal lattice model options panel");
			initParameters();
			pack();

		}

		public void initParameters() {
			for (int i = 0; i < orderTF.length; i++)
				orderTF[i].setText(stringField[i]);
			for (int i = 0; i < pars.length; i++) {
				pars[i].setText(parameterField[i].getValue());
				addComponenttolist(pars[i], parameterField[i]);
			}
		}

		public void retrieveParameters() {
			for (int i = 0; i < orderTF.length; i++)
				stringField[i] = orderTF[i].getText();
			for (int i = 0; i < pars.length; i++) {
				parameterField[i].setValue(pars[i].getText());
			}
			updateStringtoDoubleBuffering(false);
		}

	}

}

