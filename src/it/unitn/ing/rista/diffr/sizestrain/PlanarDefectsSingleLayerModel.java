/*
 * @(#)PlanarDefectsSingleLayerModel.java created Apr 9, 2009 Berkeley
 *
 * Copyright (c) 2009 Luca Lutterotti All Rights Reserved.
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
 * The PlanarDefectsSingleLayerModel is a class to implement
 * the single layer method of Ufer et al. to model turbostratic
 * ideally disordered structures
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Apr 9, 2009 11:45:10 AM $
 * @since JDK1.1
 */
public class PlanarDefectsSingleLayerModel extends PlanarDefects {

  // Dictionary, here we define the CIF string for each parameter and option in the model, will be used for
  // saving and loading files
  public static String[] diclistc = {"_riet_stacking_layers_number", "_riet_stacking_layers_axis",

                                     "_riet_crystallite_factor",
                                     "_riet_microstrain_factor",
		                               "_maud_minimum_d_space"
  };

  // this are the corresponding labels that will appear in the GUI in the parameter list window etc.
  public static String[] diclistcrm = {"number of layers for the disordered model",
                                        "crystal axis normal to the stacking sequence",

                                     "crystallite size increase normal to the stacking",
                                     "microstrain decrease normal to the stacking",
		  "minimum dspace for disordering"
  };

  // this model does not have subobjects, so the class list for subobjects is empty
  public static String[] classlistcs = {};
  public static String[] classlistc = {};

  // here we define the model string to appear in the GUI and the description string
  // change it accordingly to your method, it should be an unique identifier
  final static String id = "Ufer single layer";
  final static String desc = "select this to compute turbostratic disorder by the single layer model of Ufer et al";


  static final String[] axis = {"a", "b", "c"};

  int axisIndex = 2, layersNumber = 10;
	double reciprocal_factor = .1;

	int dmin_index = 2;

  // Constructors, and init methods do not change the code between the two stars lines
  // **********************************************************************************

  public PlanarDefectsSingleLayerModel(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = id;
    IDlabel = id;
    description = desc;
  }

  public PlanarDefectsSingleLayerModel(XRDcat aobj) {
    this(aobj, "Turbostratic model");
  }

  public PlanarDefectsSingleLayerModel() {
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
    Nstring = 2;    // number of options, treated as strings only, in this case only the first
    Nstringloop = 0;  // no vectors of strings for options
    Nparameter = 3;   // 2 parameters refinables in the model
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

    parameterField[0] = new Parameter(this, getParameterString(0), 1,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.01),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 100));
    parameterField[1] = new Parameter(this, getParameterString(1), 1,
            ParameterPreferences.getDouble(getParameterString(1) + ".min", 0.01),
            ParameterPreferences.getDouble(getParameterString(1) + ".max", 100));
	  parameterField[2] = new Parameter(this, getParameterString(2), 0,
			  ParameterPreferences.getDouble(getParameterString(2) + ".min", 0),
			  ParameterPreferences.getDouble(getParameterString(2) + ".max", 10));

    refreshComputation = true; // we specify the computation need to be refreshed (it has never done up to now)
  }

  public double getStructureFactorModifier(int h, int k, int l) {
    switch (axisIndex) {
      case 0:
        if (h != 0 && k == 0 && l == 0)
          return 1;
	      break;
      case 1:
        if (h == 0 && k != 0 && l == 0)
	        return 1;
	      break;
      case 2:
        if (h == 0 && k == 0 && l != 0)
	        return 1;
	      break;
    }
    return reciprocal_factor;
  }

  public boolean acceptReflection(int h, int k, int l) {
    switch (axisIndex) {
      case 0:
        if (h % layersNumber != 0 && k == 0 && l == 0)
          return false;
        break;
      case 1:
        if (h == 0 && k % layersNumber != 0 && l == 0)
          return false;
        break;
      case 2:
        if (h == 0 && k == 0 && l % layersNumber != 0)
          return false;
        break;
    }
    return true;
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    layersNumber = Integer.parseInt(stringField[0]);
	  reciprocal_factor = 1.0 / layersNumber;
    axisIndex = 2;
    for (int i = 0; i < axis.length; i++) {
      if (stringField[1].equalsIgnoreCase(axis[i])) {
        axisIndex = i;
        divideFactor[i] = reciprocal_factor;
      } else
        divideFactor[i] = 1.0;
    }
  }

  public int getFactor() {
    return layersNumber;
  }

  public int getSuperCellFactor(int index) {
    if (index == axisIndex)
      return getFactor();
    else
      return 1;
  }

  public double getCrystalliteFactor(int h, int k, int l) {
    switch (axisIndex) {
      case 0:
        if (h != 0 && k == 0 && l == 0)
          return getParameterValue(0);
        else
          return 1;
      case 1:
        if (h == 0 && k != 0 && l == 0)
          return getParameterValue(0);
        else
          return 1;
    }
    if (h == 0 && k == 0 && l != 0)
      return getParameterValue(0);
    else
      return 1;
  }

  public double getMicrostrainFactor(int h, int k, int l) {
    switch (axisIndex) {
      case 0:
        if (h != 0 && k == 0 && l == 0)
          return getParameterValue(1);
        else
          return 1;
      case 1:
        if (h == 0 && k != 0 && l == 0)
          return getParameterValue(1);
        else
          return 1;
    }
    if (h == 0 && k == 0 && l != 0)
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
		double dmin = getParameterValue(dmin_index);

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

		for (int i = 0; i < reflectionv.size()/* don't change */; i++) {
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
					if (sghkl.IsSysAbsent_hkl(h, k, l, null) == 0) {
						if ((sghkl.IsHidden_hkl(friedelLaw, Minh[0], Mink[0], Minl[0],
								Maxh, Maxk, Maxl, h, k, l)) == 0) {
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
    return new JSLOptionsD(parent, this);
  }

  class JSLOptionsD extends JOptionsDialog {

    JTextField layerTF = null;
    JTextField[] pars = null;
    JComboBox axisCB;

    public JSLOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));
      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.NORTH, jPanel8);


      jPanel8.add(new JLabel("Number of layers: "));
      layerTF = new JTextField(Constants.FLOAT_FIELD);
      layerTF.setText("10");
      layerTF.setToolTipText("Set the number of layers in the stacking sequence");
      jPanel8.add(layerTF);

      jPanel8.add(new JLabel("Stacking direction: "));
      axisCB = new JComboBox();
      for (String resolution1 : axis) axisCB.addItem(resolution1);
      axisCB.setToolTipText("Choose the crystal axis normal to the stacking sequence");
      jPanel8.add(axisCB);


      String[] labels = {"Crystallite factor: ",
                         "Microstrain factor: ",
      "d min: "};
      pars = new JTextField[labels.length];

      for (int i = 0; i < labels.length; i++) {
        jPanel8.add(new JLabel(labels[i]));
        pars[i] = new JTextField(Constants.FLOAT_FIELD);
        pars[i].setText("0");
        jPanel8.add(pars[i]);
      }

      setTitle("Single layer model options panel");
      initParameters();
      pack();

    }

    public void initParameters() {
      axisCB.setSelectedIndex(axisIndex);
      for (int i = 0; i < pars.length; i++) {
        pars[i].setText(parameterField[i].getValue());
        addComponenttolist(pars[i], parameterField[i]);
      }
      layerTF.setText(stringField[0]);

    }

    public void retrieveParameters() {
      stringField[0] = layerTF.getText();
      stringField[1] = (axisCB.getSelectedItem().toString());
      for (int i = 0; i < pars.length; i++) {
        parameterField[i].setValue(pars[i].getText());
      }
      updateStringtoDoubleBuffering(false);
    }

  }

}
