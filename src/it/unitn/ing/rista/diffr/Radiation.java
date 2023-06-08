/*
 * @(#)Radiation.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr;

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.chemistry.AtomInfo;

import javax.swing.*;
import java.awt.*;
import java.lang.*;

/**
 * The Radiation is a class
 *
 * @version $Revision: 1.8 $, $Date: 2006/07/20 13:39:04 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Radiation extends XRDcat {

	protected static String[] diclistc = {"_diffrn_radiation_wavelength", "_diffrn_radiation_wavelength_wt"};
  protected static String[] diclistcrm = {"wavelength (angstrom)", "weight (arb)"};

  protected static String[] classlistc = {};

  public static double[][] xraySF = null;
  public static double[] neutronSF = null;
  public static double[][] magneticSF = null;
  public static double[] neutronAbs = null;
  public static double[][] electronSF = null;
  public static double[] electronAbs = null;

  static boolean loadTables = true;

  public Radiation(XRDcat afile, String alabel) {
    super(afile, alabel);
    initBaseObject();
  }

  public Radiation(XRDcat afile) {
    this(afile, "Radiation x");
  }

	public Radiation() {}

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 2;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
  }

  public void initParameters() {
    super.initParameters();
    parameterField[0] = new Parameter(this, getParameterString(0), 1.5405929,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.1),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 10));
    parameterField[1] = new Parameter(this, getParameterString(1), 1.0,
            ParameterPreferences.getDouble(getParameterString(1) + ".min", 0),
            ParameterPreferences.getDouble(getParameterString(1) + ".max", 1.01));
    loadAtomTables();
  }

  public void setRadiation(String alabel, double awavelength, double aweight) {
    setLabel(alabel);
    parameterField[0] = new Parameter(this, getParameterString(0), awavelength,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.1),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 10));
    parameterField[1] = new Parameter(this, getParameterString(1), aweight,
            ParameterPreferences.getDouble(getParameterString(1) + ".min", 0),
            ParameterPreferences.getDouble(getParameterString(1) + ".max", 1.01));
  }

  public String getWavelengthID() {
    return toXRDcatString();
  }

  public void setWavelengthID(String awave) {
    setLabel(awave);
  }

  public Parameter getWavelength() {
    return parameterField[0];
  }

  public double getWavelengthValue() {
    return parameterValues[0];
  }

  public Parameter getWeigth() {
    return parameterField[1];
  }

  public boolean isNeutron() {
    return ((RadiationType) getParent()).isNeutron();
  }

  public boolean isElectron() {
    return ((RadiationType) getParent()).isElectron();
  }

	public boolean isDynamical() {
		return ((RadiationType) getParent()).isDynamical();
	}

/*	public int getRadiationIDNumber() {
    return ((RadiationType) getParent()).getRadiationIDNumber();
  }*/

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField[0] == source) {
        notifyParameterChanged(source, Constants.RADIATION_WAVELENGTH_CHANGED);
        notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED);
        return;
      }
      if (parameterField[1] == source) {
        notifyParameterChanged(source, Constants.BEAM_INTENSITY_CHANGED);
        return;
      }
      super.notifyParameterChanged(source);
    }
  }

  public void refreshForNotificationUp(XRDcat source, int reason) {
    if (!getFilePar().isComputingDerivate() && reason == Constants.RADIATION_WAVELENGTH_CHANGED) {
      refreshComputation = true;
    }
  }

  public static void loadAtomTables() {
    if (!loadTables)
      return;
    xraySF = AtomInfo.retrieveXrayScatFactors();
    neutronSF = AtomInfo.retrieveNeutronIsotopicScatFactor();
    neutronAbs = AtomInfo.retrieveNeutronAbsorptionCrossSection();
    electronSF = AtomInfo.retrieveElectronScatFactor();
    electronAbs = AtomInfo.retrieveElectronAbsorptionCrossSection();
    magneticSF = AtomInfo.retrieveMagneticScatFactors();
    loadTables = false;
  }

	public JOptionsDialog getOptionsDialog(Frame parent) {
		return new JRadiationOptionsD(parent, this);
	}

	public class JRadiationOptionsD extends JOptionsDialog {

		JTextField[] parTF = null;
		String labels[] = {"Wavelength:", "Weigth:"};

		public JRadiationOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new FlowLayout());
			JPanel radiationPanel = new JPanel(new GridLayout(0, 2, 3, 3));
			principalPanel.add(radiationPanel);

			parTF = new JTextField[labels.length];
			for (int i = 0; i < labels.length; i++) {
				radiationPanel.add(new JLabel(labels[i]));
				parTF[i] = new JTextField(Constants.FLOAT_FIELD);
				parTF[i].setText("0");
				radiationPanel.add(parTF[i]);
			}

			setTitle("Radiation component");
			initParameters();
			pack();
		}

		public void initParameters() {
			for (int i = 0; i < labels.length; i++) {
				parTF[i].setText(getParameter(i).getValue());
				addComponenttolist(parTF[i], getParameter(i));
			}
		}

		public void retrieveParameters() {
			for (int i = 0; i < labels.length; i++) {
				getParameter(i).setValue(parTF[i].getText());
			}
		}

	}

}

