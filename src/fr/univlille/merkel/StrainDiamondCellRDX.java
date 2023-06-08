/*
 * @(#)StrainDiamondCellRDX.java created 10/3/2006 Lille
 *
 * Copyright (c) 2006 Sebastien Merkel All Rights Reserved.
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

package fr.univlille.merkel;

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;

/**
 *  The StrainDiamondCellRDX is a class to compute the diffraction shift from
 *  the triaxial stress tensor assuming isotropic elastic constants.
 *  See Noyan and Cohen, Residual Stress, Eq. 5.15 pag. 121.
 *
 * @version $Revision: 1.00 $, $Date: 2006/03/10 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class StrainDiamondCellRDX extends Strain {

// Dictionary, here we define the CIF string for each parameter and option in the model, will be used for
// saving and loading files
public static String[] diclistc = {"_smlille_DAC_RDX_nhkl", "_smlille_DAC_RDX_h", "_smlille_DAC_RDX_k", "_smlille_DAC_RDX_l", "_smlille_DAC_RDX_offsetAlpha", "_smlille_DAC_RDX_offsetBeta", "_smlille_DAC_RDX_Qvalues"};

// this are the corresponding labels that will appear in the GUI in the parameter list window etc.
public static String[] diclistcrm = {"Nhkl", "h", "k", "l", "alpha", "beta", "Lattice strains"};

// this model does not have subobjects, so the class list for subobjects is empty
public static String[] classlistcs = {};
public static String[] classlistc = {};

// here we define the model string to appear in the GUI and the description string
// change it accordingly to your method, it should be an unique identifier
final static String id = "Radial Diffraction in the DAC";
final static String desc = "select this to for radial diffraction in the diamond anvil cell";

//

// Constructors, and init methods do not change the code between the two stars lines
// **********************************************************************************
public StrainDiamondCellRDX(XRDcat aobj, String alabel) {
	super(aobj, alabel);
	initBaseObject();
	identifier = id;
	IDlabel = id;
	description = desc;
}

public StrainDiamondCellRDX(XRDcat aobj) {
	this(aobj, id);
}

public StrainDiamondCellRDX() {
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
Nstring = 1;    // number of options, treated as strings only, in this case: number of hkl
Nstringloop = 3;  // 1 vectors of strings for options: h, k, and l
Nparameter = 2;  // offset angles alpha and beta
Nparameterloop = 1;  // 1 parameter vectors in this model (lattice strains Q)
Nsubordinate = 0;    // no subobjects or subordinate objects
Nsubordinateloop = 0;  // no vectors of subobjects
}

// we init the parameters here, in this case we init only the elastic constant (default value 200, default
// minimum value (for genetic algorithm) 1 and maximum 900, change in case only the values
// do it for all parameters you like, in this case we don't do nothing for the residual stress parameters,
// they will start with a value of zero and default min and max
// the options (these are not parameters, just Strings, are started by assigning a string as in the following
// stringField[] are options stored and managed as String
// parameterField[] are parameters (refinable by the program) stored as String but can be accessed during
// the computation as parameterValues[] (there is an update method that convert all parameterFields in
// parameterValues just before each iteration, function computation. So you shoud always manage the
// parameterField in the GUI and use parameterValues n the strain computation (as in this example)

public void initParameters() {
	super.initParameters();
	//System.out.println("Init parameters for DAC Radial Diffraction Stress Model");
	// Offset angles alpha and beta
	parameterField[0] = new Parameter(this, getParameterString(0), 0.,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", -180.),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 190.));
    parameterField[1] = new Parameter(this, getParameterString(1), 0., 
            ParameterPreferences.getDouble(getParameterString(1) + ".min", -0.1),
            ParameterPreferences.getDouble(getParameterString(1) + ".max", 90.));
	// Number of planes
	stringField[0] = "" + getPhase().gethklNumber();
	// h, k, and l
	int hkln = Integer.parseInt(stringField[0]);
	//System.out.println("I found " + hkln + " reflections");
	String[] hklNames = new String[hkln];
	int h, k, l;
	for (int i = 0; i < hkln; i++) {
		h = getPhase().geth(i);
		k = getPhase().getk(i);
		l = getPhase().getl(i);
		stringloopField[0].addItem(new String("" + h));
		stringloopField[1].addItem(new String("" + k));
		stringloopField[2].addItem(new String("" + l));
		hklNames[i] = (String)stringloopField[0].elementAt(i) + (String)stringloopField[1].elementAt(i) + (String)stringloopField[2].elementAt(i);
	}
	// Loop of vector first vector parameters will contain Q values
	// Loop of  strings will contain labels for Q(hkl)
	//System.out.println("Length of parameter loop array: " + parameterloopField.length);
	//System.out.println("Size of first parameter loop: " + parameterloopField[0].size());
	for (int i = 0; i < hkln; i++) {
		System.out.println("Setting-up fields for peak " + hklNames[i] + " with range -0.01 0.01");	parameterloopField[0].addItem(new Parameter(this, getParameterString(0), 0.0,
			ParameterPreferences.getDouble(getParameterString(0) + ".min", -0.01),
			ParameterPreferences.getDouble(getParameterString(0) + ".max", 0.01)));
		// stringloopField[0].addItem(new String("Q(" + hklNames[i] + ")"));
	}
	refreshComputation = true; // we specify the computation need to be refreshed (it has never done up to now)
}

public int indexFromAngles(double psi, double beta) {
	int hkln = Integer.parseInt(stringField[0]);
	int h, k, l;
	double phiTest;
	double betaTest;
	Reflection refl;
	for (int i = 0; i < hkln; i++) {
		h = Integer.parseInt((String)stringloopField[0].elementAt(i));
		k = Integer.parseInt((String)stringloopField[1].elementAt(i));
		l = Integer.parseInt((String)stringloopField[2].elementAt(i));
		refl = getPhase().getReflectionByhkl(h, k, l);
		phiTest = refl.phi[0];
		betaTest = refl.beta[0];
		if ((java.lang.Math.abs(phiTest-psi) < 0.0001) && (java.lang.Math.abs(betaTest-beta) < 0.0001)) return i;
	}
	return 100;
}

public double computeStrain(double psi, double beta, double chi, double phi) {
	// Angles must be in radiants
	// psi and beta are the polar and azimuthal angles for the crystal setting
	// phi and chi for the sample
	//System.out.println("Strain angles");
	//System.out.println("  psi = " + psi * Constants.PITODEG + "  beta = " + beta * Constants.PITODEG);
	int peakindex = indexFromAngles(psi, beta);
	//System.out.println("  peak index = " + peakindex);
	//System.out.println("  chi = " + chi * Constants.PITODEG + "  phi = " + phi * Constants.PITODEG);
	double latticestrain = Double.parseDouble(((Parameter)parameterloopField[0].elementAt(peakindex)).getValue());
	double coschi = java.lang.Math.cos(chi);
	double sin2chi = java.lang.Math.sin(2.*chi);
	double coschi2 = coschi*coschi;
	double sinchi2 = 1.-coschi2;
	double cosphi = java.lang.Math.cos(chi);
	double cosphi2 = cosphi*cosphi;
	double sin2phi = java.lang.Math.sin(2.*phi);

	double alpha = Constants.DEGTOPI*parameterValues[0];
	double betab = Constants.DEGTOPI*parameterValues[1];

	double cosalpha = java.lang.Math.cos(alpha);
	double cosalpha2 = cosalpha*cosalpha;
	double sin2alpha = java.lang.Math.sin(2.*alpha);
	double cosbeta = java.lang.Math.cos(betab);
	double cosbeta2 = cosbeta*cosbeta;
	double sinbeta = java.lang.Math.sin(betab);
	double sinbeta2 = sinbeta*sinbeta;
	double sin2beta = java.lang.Math.sin(2.*betab);

	return latticestrain * (-2. + 3.*(cosphi2+coschi2) - 3.*cosalpha2*sinbeta2*(2.*cosphi2-1.+coschi2)
				- 3.*cosbeta2*(2.*coschi2-1.+cosphi2) + 3.*cosphi2*coschi2*sinbeta2*(2.*cosalpha2-1.)
				+ 1.5*sin2phi*sin2alpha*sinchi2*sinbeta2 + 1.5*sin2chi*sin2beta*java.lang.Math.cos(alpha+phi));
}

/* public double computeStrain(Phase aphase, double strain_angles[], int h, int k, int l) {
	Reflection refl = aphase.getReflectionByhkl(h, k, l);
	System.out.println("Strain h k l");
	return 0.0;
	//return computeStrain(refl.phi[0], refl.beta[0], strain_angles[0] * Constants.DEGTOPI, strain_angles[1] * Constants.DEGTOPI);
}

public double[] computeStrain(Phase aphase, double alpha[], double beta[], Reflection reflex) {
	int h = reflex.h;
	int k = reflex.k;
	int l = reflex.l;
	int numberOfPoints = alpha.length;
	double[] strainValues = new double[numberOfPoints];
	for (int i = 0; i < numberOfPoints; i++) {
		//strainValues[i] = computeStrain(reflex.phi[0], reflex.beta[0],
		//	alpha[i] * Constants.DEGTOPI,
		//	beta[i] * Constants.DEGTOPI);
	}
//	System.out.println("Strain reflection");
	return strainValues;
}*/

// this part implements the option window to manipulate the parameters and options
// you need first to change the name of the Dialog inner class and then if you just have some different
// parameters changing only the labels (number and strings is sufficient)
// if you have additional options (stringField[]) then you may want to manage also them inside

public JOptionsDialog getOptionsDialog(Frame parent) {
	return new JTSStrainOptionsD(parent, this);
}

class JTSStrainOptionsD extends JOptionsDialog {
	JTextField[] pars = null;

	public JTSStrainOptionsD(Frame parent, XRDcat obj) {
		super(parent, obj);
		int hkln = Integer.parseInt(stringField[0]);
		String[] hklNames = new String[hkln];
		int h, k, l;
		for (int i = 0; i < hkln; i++) {
			hklNames[i] = (String)stringloopField[0].elementAt(i) + (String)stringloopField[1].elementAt(i) + (String)stringloopField[2].elementAt(i);
		}
		String[] labels = new String[2+hkln];
		pars = new JTextField[2+hkln];
		principalPanel.setLayout(new BorderLayout(6, 6));
		JPanel jPanel8 = new JPanel();
		jPanel8.setLayout(new GridLayout(0, 2, 3, 3));
		principalPanel.add(BorderLayout.NORTH, jPanel8);
		// Offset angle alpha
		labels[0] = "Alpha";
		jPanel8.add(new JLabel(labels[0]));
		pars[0] = new JTextField(Constants.FLOAT_FIELD);
		pars[0].setText("0");
		jPanel8.add(pars[0]);
		// Offset angle beta
		labels[1] = "Beta";
		jPanel8.add(new JLabel(labels[1]));
		pars[1] = new JTextField(Constants.FLOAT_FIELD);
		pars[1].setText("0");
		jPanel8.add(pars[1]);
		for (int i = 0; i < (hkln); i++) {
			labels[i+2] = "Q(" + hklNames[i] + ")";
			jPanel8.add(new JLabel(labels[i+2]));
			pars[i+2] = new JTextField(Constants.FLOAT_FIELD);
			pars[i+2].setText("0");
			jPanel8.add(pars[i+2]);
		}
		setTitle("DAC Radial Diffraction Option Panel");
		initParameters();
		pack();
	}

	public void initParameters() {
		pars[0].setText(parameterField[0].getValue());
		pars[1].setText(parameterField[1].getValue());
		addComponenttolist(pars[0], (Parameter)parameterField[0]);
		addComponenttolist(pars[1], (Parameter)parameterField[1]);
		int hkln = Integer.parseInt(stringField[0]);
		for (int i = 0; i < hkln; i++) {
			pars[i+2].setText(((Parameter)parameterloopField[0].elementAt(i)).getValue());
			addComponenttolist(pars[i+2], (Parameter)parameterloopField[0].elementAt(i));
		}
	}

	public void retrieveParameters() {
        parameterField[0].setValue(pars[0].getText());
        parameterField[1].setValue(pars[1].getText());
		int hkln = Integer.parseInt(stringField[0]);
		for (int i = 0; i < hkln; i++) {
			((Parameter)parameterloopField[0].elementAt(i)).setValue(pars[i+2].getText());
		}
	}
}

}

