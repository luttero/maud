/*
 * @(#)TriaxialStress.java created 16/09/2001 Pergine Vals.
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.rsa;

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.RealMatrix;

import javax.swing.*;
import java.awt.*;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.Vector;

/**
 *  The TriaxialStress is a class to compute the diffraction shift from
 *  the triaxial stress tensor assuming isotropic elastic constants.
 *  See Noyan and Cohen, Residual Stress, Eq. 5.15 pag. 121.
 *
 * @version $Revision: 1.12 $, $Date: 2006/07/20 13:39:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class TriaxialStress extends Strain {

  // Dictionary, here we define the CIF string for each parameter and option in the model, will be used for
  // saving and loading files
  public static String[] diclistc = {"_rista_deviatoric_part_only",

                                     "_rista_elastic_modulus_E",
                                     "_rista_poisson_ratio_v",
                                     "_rista_macrostress_11",
                                     "_rista_macrostress_22",
                                     "_rista_macrostress_33",
                                     "_rista_macrostress_23",
                                     "_rista_macrostress_13",
                                     "_rista_macrostress_12"
  };

  // this are the corresponding labels that will appear in the GUI in the parameter list window etc.
  public static String[] diclistcrm = {"_rista_deviatoric_part_only",

                                     "elastic modulus (arb)",
                                     "poisson ratio",
                                     "macrostress_11 (arb)",
                                     "macrostress_22 (arb)",
                                     "macrostress_33 (arb)",
                                     "macrostress_23 (arb)",
                                     "macrostress_13 (arb)",
                                     "macrostress_12 (arb)"
  };

  // this model does not have subobjects, so the class list for subobjects is empty
  public static String[] classlistcs = {};
  public static String[] classlistc = {};

  // here we define the model string to appear in the GUI and the description string
  // change it accordingly to your method, it should be an unique identifier
  final static String id = "Triaxial Stress Isotropic E";
  final static String desc = "select this to apply the Triaxial Stress model with isotropic E";

  // Constructors, and init methods do not change the code between the two stars lines
  // **********************************************************************************
  public TriaxialStress(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = id;
    IDlabel = id;
    description = desc;
  }

  public TriaxialStress(XRDcat aobj) {
    this(aobj, id);
  }

  public TriaxialStress() {
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
    Nstring = 1;    // number of options, treated as strings only, in this case only the first
    Nstringloop = 0;  // no vectors of strings for options
    Nparameter = 8;   // 8 parameters refinables in the model
    Nparameterloop = 0;  // no parameter vectors in this model, to be used when the number of parameters may
                         // change in the model and/or is defined by other options
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

    stringField[0] = "false";  // this option (the deviatoric only computation) is not used in the model actually

    parameterField[0] = new Parameter(this, getParameterString(0), 200,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 1),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 900));
    parameterField[1] = new Parameter(this, getParameterString(1), 0.23,
            ParameterPreferences.getDouble(getParameterString(1) + ".min", -0.1),
            ParameterPreferences.getDouble(getParameterString(1) + ".max", 0.5));

    refreshComputation = true; // we specify the computation need to be refreshed (it has never done up to now)
  }

  // this is the main method that perform the actual computation of the strain based on the crystallographic
  // angles of the hkl reflection (psi, beta), and angles of orientation of the sample respect to the diffraction
  // vector (chi and phi)

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
            notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED);
            notifyParameterChanged(source, Constants.STRAIN_CHANGED);
            return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED);
            notifyParameterChanged(source, Constants.STRAIN_CHANGED);
            return;
          }

      super.notifyParameterChanged(source);
    }
  }

  public double computeStrain(double psi, double beta, double chi, double phi) {
    // Angles are in radiants
    // psi and beta are the polar and azimuthal angles for the crystal setting
    // phi and chi for the sample

//	  System.out.println("refresh Strains");

    double sinphi = Math.sin(phi);
    double sinphi2 = sinphi * sinphi;
    double sin2phi = Math.sin(2.0 * phi);
    double cosphi = Math.cos(phi);
    double cosphi2 = cosphi * cosphi;
    double sinchi2 = Math.sin(chi);
    sinchi2 *= sinchi2;
    double sin2chi = Math.sin(2.0 * chi);
    double kappa = (1.0 + parameterValues[1]) / parameterValues[0];

    // we return the strain in the diffraction vector direction
    return kappa * (parameterValues[2] * cosphi2 + parameterValues[7] * sin2phi +
                    parameterValues[3] * sinphi2 - parameterValues[4]) * sinchi2 +
                    kappa * parameterValues[4] - parameterValues[1] / parameterValues[0] *
                    (parameterValues[2] + parameterValues[3] + parameterValues[4]) +
                    kappa * (parameterValues[6] * cosphi + parameterValues[5] * sinphi) * sin2chi;
  }

	public void outputStressTensor2x2(BufferedWriter writer) throws IOException {


  	  double stress[][] = { {parameterValues[2], parameterValues[7]},
		                     {parameterValues[7], parameterValues[3]} };

		double[][] diagonal = MoreMath.diagonalMatrix(stress);

		writer.write("Diagonalized stress tensor: ");
		writer.write(Constants.lineSeparator);
		writer.write(diagonal[1][1] + "   " + diagonal[0][0]);
		writer.write(Constants.lineSeparator);
		writer.flush();

	}

	public void outputStressTensor(BufferedWriter writer) throws IOException {

		double stress[][] = { {parameterValues[2], parameterValues[7], parameterValues[6]},
			{parameterValues[7], parameterValues[3], parameterValues[5]},
			{parameterValues[6], parameterValues[5], parameterValues[4]} };

  	   double[][] diagonal = MoreMath.diagonalMatrix(stress);

		writer.write("Diagonalized stress tensor: ");
		writer.write(Constants.lineSeparator);
		writer.write(diagonal[1][1] + "   " + diagonal[2][2] + "   " + diagonal[0][0]);
		writer.write(Constants.lineSeparator);

		writer.flush();

}

	public void outputStressTensorTest(BufferedWriter writer) throws IOException {

		double stress[][] = { {4, 3, -1}, {-3, -2, 1}, {-3, -3, 2} };

		writer.write("Original stress tensor: ");
		writer.write(Constants.lineSeparator);
		writer.write(stress[0][0] + "   " + stress[0][1] + "   " + stress[0][2]);
		writer.write(Constants.lineSeparator);
		writer.write(stress[1][0] + "   " + stress[1][1] + "   " + stress[1][2]);
		writer.write(Constants.lineSeparator);
		writer.write(stress[2][0] + "   " + stress[2][1] + "   " + stress[2][2]);
		writer.write(Constants.lineSeparator);

		double[][] diagonal = MoreMath.diagonalMatrix(stress);

		writer.write("Diagonalized stress tensor: ");

		writer.write(Constants.lineSeparator);
		writer.write(diagonal[0][0] + "   " + diagonal[0][1] + "   " + diagonal[0][2]);
		writer.write(Constants.lineSeparator);
		writer.write(diagonal[1][0] + "   " + diagonal[1][1] + "   " + diagonal[1][2]);
		writer.write(Constants.lineSeparator);
		writer.write(diagonal[2][0] + "   " + diagonal[2][1] + "   " + diagonal[2][2]);
		writer.write(Constants.lineSeparator);

		writer.flush();

	}

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

      String[] labels = {"Young modulus:   ",
                         "Poisson modulus: ",
                         "Macrostress11:   ",
                         "Macrostress22:   ",
                         "Macrostress33:   ",
                         "Macrostress23:   ",
                         "Macrostress13:   ",
                         "Macrostress12:   "};
      pars = new JTextField[8];

      principalPanel.setLayout(new BorderLayout(6, 6));
      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.NORTH, jPanel8);

      for (int i = 0; i < 8; i++) {
        jPanel8.add(new JLabel(labels[i]));
        pars[i] = new JTextField(Constants.FLOAT_FIELD);
        pars[i].setText("0");
        jPanel8.add(pars[i]);
      }

      setTitle("Triaxial Stress options panel");
      initParameters();
      pack();

    }

    public void initParameters() {
      for (int i = 0; i < 8; i++) {
        pars[i].setText(parameterField[i].getValue());
        addComponenttolist(pars[i], parameterField[i]);
      }
    }

    public void retrieveParameters() {
      for (int i = 0; i < 8; i++) {
        parameterField[i].setValue(pars[i].getText());
      }
    }

  }

}

