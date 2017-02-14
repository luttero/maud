/*
 * @(#)RadiationType.java created 06/01/1999 Riva del Garda
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr;

import java.lang.*;
import java.awt.*;

import it.unitn.ing.rista.diffr.radiation.ElectronDynamicalRadiation;
import it.unitn.ing.rista.diffr.radiation.ElectronRadiation;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import javax.swing.*;

/**
 *  The RadiationType is a class
 *
 *
 * @version $Revision: 1.7 $, $Date: 2006/07/20 13:39:04 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class RadiationType extends XRDcat {

  public static String[] diclistc = {
		"_diffrn_dynamical_scattering_correction",
		"_diffrn_dynamical_scattering_correction_crystallite",
    "_diffrn_radiation_wavelength_id"
  };
  public static String[] diclistcrm = {
		"_diffrn_dynamical_scattering_correction",
		"_diffrn_dynamical_scattering_correction_crystallite",
    "_diffrn_radiation_wavelength_id"
  };

  public static String[] classlistc = {"it.unitn.ing.rista.diffr.Radiation"};
  public static String[] classlistcs = {};

	public double energy = 0.0;

  public RadiationType(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
  }

  public RadiationType(XRDcat aobj) {
    this(aobj, "Radiation x");
  }

  public RadiationType() {
  }

  public void initConstant() {
    Nstring = 2;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 1;
  }

  public void initDictionary() {
    System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
    System.arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
  }

  public void initParameters() {
    super.initParameters();
	  stringField[0] = "true";
	  stringField[1] = "false";
  }

  public void refreshAll(boolean firstLoading) {
    checkRadiation();
    super.refreshAll(firstLoading);
  }

  public void addRadiation(String astring) {
    subordinateloopField[0].addItem(new Radiation(this, astring));
  }

  public void removeAllRadiations() {
    subordinateloopField[0].removeAllItems();
  }

  public boolean isConstantWavelenght() {
    return true;
  }

  public double getMeanRadiationWavelength() {
//    if (isConstantWavelenght()) {
      checkRadiation();
      double weight, totweight = 0.0, sumwave = 0.0;
      int radnumber = getLinesCount();
      for (int i = 0; i < radnumber; i++) {
        Radiation rad = getRadiation(i);
        if (rad != null) {
          weight = rad.getWeigth().getValueD();
          sumwave += rad.getWavelength().getValueD() * weight;
          totweight += weight;
        }
      }
	  if (sumwave == 0)
		  return MaudPreferences.getDouble("radiation.fake_wavelength_TOF_EnergyDispersive", 0.001);
    return sumwave / totweight;
//    }
//    return 0.001;
  }

  public double getRadiationWavelength() {
//    if (isConstantWavelenght()) {
      checkRadiation();
      Radiation rad = getRadiation(0);
      if (rad == null)
	      System.out.println("No wavelength defined! Using fake wavelength for TOF/Energy dispersive");
      else
        return rad.getWavelength().getValueD();
//    }
    return MaudPreferences.getDouble("radiation.fake_wavelength_TOF_EnergyDispersive", 0.001);
  }

  public double getRadiationWavelength(int index) {
//    if (isConstantWavelenght()) {
      checkRadiation();
      Radiation rad = getRadiation(index);
      if (rad == null)
        System.out.println("No wavelength defined! Using fake wavelength for TOF/Energy dispersive");
      else
        return rad.getWavelength().getValueD();
//    }
	  return MaudPreferences.getDouble("radiation.fake_wavelength_TOF_EnergyDispersive", 0.001);
  }

  public double getRadiationWeigth(int index) {
//    if (isConstantWavelenght()) {
      checkRadiation();
      Radiation rad = getRadiation(index);
      if (rad == null)
        System.out.println("No wavelength defined!");
      else
        return rad.getWeigth().getValueD();
//    }
    return 0.0;
  }

	public int getLinesCountForFluorescence() {
		return getLinesCount();
	}

	public double getRadiationWavelengthForFluorescence(int index) {
		return getRadiationWavelength(index);
	}

	public double getShortestWavelengthForFluorescence() {
		double shortestWave = getRadiationWavelengthForFluorescence(0);
		for (int i = 1; i < getLinesCount(); i++) {
			double wave = getRadiationWavelengthForFluorescence(i);
			if (wave < shortestWave)
				shortestWave = wave;
		}
		return shortestWave;
	}

	public double getRadiationWeightForFluorescence(int index) {
		return getRadiationWeigth(index);
	}

	public void checkRadiation() {
    if (getLinesCount() <= 0)
      addRadiation("Added_by_default");
  }

  public int getLinesCount() {
    return numberofelementSubL(0);
  }

  public int getLinesCountForPlot() {
    return getLinesCount();
  }

  public Radiation getRadiation(int index) {
    if (getLinesCount() > index && index >= 0)
      return (Radiation) subordinateloopField[0].elementAt(index);
    else
      return null;
  }

  public boolean isNeutron() {
    return false;
  }

  public boolean isElectron() {
    return false;
  }

	public boolean isDynamical() {
		return false;
	}

	public boolean isSimilarTo(RadiationType other) {
    boolean isTrue = true;
    if (!((isNeutron() && other.isNeutron()) || (!isNeutron() && !other.isNeutron() &&
        !isElectron() && !other.isElectron()) || (isElectron() && !other.isElectron())))
      isTrue = false;
    if (!((isConstantWavelenght() && other.isConstantWavelenght()) ||
            (!isConstantWavelenght() && !other.isConstantWavelenght())))
      isTrue = false;
    if (Math.abs(getMeanRadiationWavelength() - other.getMeanRadiationWavelength()) > 0.0001)
      isTrue = false;
    return isTrue;
  }

/*  public int getRadiationIDNumber() {
    return ((Instrument) getParent()).getRadiationNumber();
  }

  public int getRadiationTubeNumber() {
    return ((Instrument) getParent()).getRadiationTubeNumber();
  }*/

	public void updateParametertoDoubleBuffering(boolean firstLoading) {
		// to be implemented by subclasses

		if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
			return;
		super.updateParametertoDoubleBuffering(firstLoading);

		energy = lambdaToEnergy(getMeanRadiationWavelength());
	}

	public double energyToLambda(double e)
	{
		return 12.398424 / e;
	}

	public double lambdaToEnergy(double lambda)
	{
		return 12.398424 / lambda;
	}

	public double getRadiationEnergy() {
		return energy;
	}

  public JOptionsDialog getOptionsDialog(Frame parent) {
    return new JRadiationTypeOptionsD(parent, this);
  }

	public boolean useDynamicalCorrection() {
		return stringField[0].equalsIgnoreCase("true");
	}

	private void setDynamicalCorrection(boolean selected) {
		if (selected)
			stringField[0] = "true";
		else
			stringField[0] = "false";
	}

	public boolean useCrystallitesForDynamicalCorrection() {
		return stringField[1].equalsIgnoreCase("true");
	}

	private void setCrystalliteForDynamicalCorrection(boolean selected) {
		if (selected)
			stringField[1] = "true";
		else
			stringField[1] = "false";
	}

  public int getSubdivision() {
    return 1;
  }

  public class JRadiationTypeOptionsD extends JOptionsDialog {

    JSubordListPane RadiationPanel;
		JCheckBox dynamical = null;
		JCheckBox crystalliteForDynamical = null;

    public JRadiationTypeOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(3, 3));

      RadiationPanel = new JSubordListPane(this, false);
      principalPanel.add(RadiationPanel, BorderLayout.CENTER);

	    if (RadiationType.this instanceof ElectronRadiation) {
		    JPanel panel = new JPanel(new GridLayout(0, 1, 3, 3));
		    principalPanel.add(panel, BorderLayout.SOUTH);
		    JPanel fpanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
		    if (!(RadiationType.this instanceof ElectronDynamicalRadiation)) {
			    panel.add(fpanel);
		      fpanel.add(dynamical = new JCheckBox("Dynamical correction (Blackmann)"));
		      dynamical.setToolTipText("Select this to use the Blackmann two beam correction for Fhkl");
	      }
		    fpanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
		    panel.add(fpanel);
		    fpanel.add(crystalliteForDynamical = new JCheckBox("Crystallite value for thickness"));
		    crystalliteForDynamical.setToolTipText("Default is the Grain size value in the Microstructure panel of phase");
	    }

      setTitle("Radiation");
      initParameters();
      pack();
    }

    public void initParameters() {
      checkRadiation();
      String labels[] = {"Wavelength:", "Weigth:"};
      RadiationPanel.setList(RadiationType.this, 0, 2, labels);
	    if (crystalliteForDynamical != null)
		    crystalliteForDynamical.setSelected(useCrystallitesForDynamicalCorrection());
	    if (dynamical != null)
		    dynamical.setSelected(useDynamicalCorrection());
    }

    public void retrieveParameters() {
      RadiationPanel.retrieveparlist();
	    if (crystalliteForDynamical != null)
		    RadiationType.this.setCrystalliteForDynamicalCorrection(crystalliteForDynamical.isSelected());
	    if (dynamical != null)
		    RadiationType.this.setDynamicalCorrection(dynamical.isSelected());
    }

    public void dispose() {
      RadiationPanel.dispose();
      super.dispose();
    }

  }

}
