/*
 * @(#)Layer.java created 01/01/1997 Mesiano
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

import java.io.IOException;
import java.io.OutputStream;
import java.util.*;
import java.lang.*;

import it.unitn.ing.rista.chemistry.AtomInfo;
import it.unitn.ing.rista.chemistry.XRayDataSqLite;
import it.unitn.ing.rista.io.cif.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.interfaces.*;

/**
 * The Layer is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.13 $, $Date: 2006/02/02 16:11:56 $
 * @since JDK1.1
 */

public class Layer extends XRDcat {
  protected static String[] diclistc = {
		  "_reflectivity_layer_qc_from_phases",
		  "_riet_par_spec_layer_thickness",
      "_reflectivity_layer_critical_qc",
      "_reflectivity_layer_absorption",
      "_reflectivity_layer_roughness",

      "_pd_phase_atom_%"};
  protected static String[] diclistcrm = {
		  "compute layer qc from phase composition/density",
		  "layer thickness (angstrom)",
      "critical qc (reflectivity only)",
      "layer absorption (reflectivity only)",
      "layer roughness (reflectivity only)",

      "phase scale factor / volume fraction "};

  protected static String[] classlistc = {};
  protected double absorptionThick = 0.0;
  protected double absorptionPath = 0.0;
  int theindex = 0;
  protected double[] kz = null;
	Vector<AtomQuantity> chemicalComposition = null;
	public boolean qc_from_phase = false;
	public static final int qcFromPhaseID = 0;

  public Layer(XRDcat afile, String alabel) {
    super(afile, alabel);
    initXRD();
  }

  public Layer(XRDcat afile) {
    this(afile, "Layer_x");
  }

  public void initConstant() {
    Nstring = 1;
    Nstringloop = 0;
    Nparameter = 4;
    Nparameterloop = 1;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
  }

  public void initParameters() {
    super.initParameters();
	  setQcFromPhase(false);
    parameterField[0] = new Parameter(this, "Thickness (Angstrom)", 1.0E7,
        ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.1),
        ParameterPreferences.getDouble(getParameterString(0) + ".max", 1.0E8));
    parameterField[0].setPositiveOnly();
    parameterField[1] = new Parameter(this, "Critical qc", .04,
        ParameterPreferences.getDouble(getParameterString(1) + ".min", 0.001),
        ParameterPreferences.getDouble(getParameterString(1) + ".max", 0.5));
    parameterField[1].setPositiveOnly();
    parameterField[2] = new Parameter(this, "Absorption", 2E-7,
        ParameterPreferences.getDouble(getParameterString(2) + ".min", 1E-8),
        ParameterPreferences.getDouble(getParameterString(2) + ".max", 1E-5));
    parameterField[2].setPositiveOnly();
    parameterField[3] = new Parameter(this, "Roughness", 2,
        ParameterPreferences.getDouble(getParameterString(3) + ".min", 0.01),
        ParameterPreferences.getDouble(getParameterString(3) + ".max", 100));
    parameterField[3].setPositiveOnly();
  }

  public void setIndex(int index) {
    theindex = index;
  }

  public int getIndex() {
    return theindex;
  }

  public Sample getSample() {
//    System.out.println(getParent());
    return (Sample) getParent();
  }

	public void setQcFromPhase(boolean value) {
		if (value)
			setString(qcFromPhaseID, "true");
		else
			setString(qcFromPhaseID, "false");
	}

	public boolean getQcFromPhase() {
		return getString(qcFromPhaseID).equalsIgnoreCase("true");
	}

	public boolean qcFromPhase() {
		return qc_from_phase;
	}

	public int checkPhaseNumber() {
    int phasenumber = numberofelementPL(0);
    int realphasenumber = getSample().phasesNumber();
    while (phasenumber < realphasenumber) {
      Parameter par = new Parameter(this,
          new String("Volume fraction of " + getPhaseName(phasenumber)), 1,
          ParameterPreferences.getDouble(getParameterString(0, phasenumber) + ".min", 0),
          ParameterPreferences.getDouble(getParameterString(0, phasenumber) + ".max", 1));
      par.setPositiveOnly();
      addparameterloopField(0, par);
      phasenumber = numberofelementPL(0);
	    normalizePhaseQuantity(false);
    }
    while (phasenumber > realphasenumber) {
      parameterloopField[0].removeItemAt(phasenumber - 1);
      phasenumber = numberofelementPL(0);
	    normalizePhaseQuantity(false);
    }
    return phasenumber;
  }

  public int getPhaseNumber() {
    return numberofelementPL(0);
  }

  public String getPhaseName(int index) {
//    System.out.println(index);
    Phase aphase = getSample().getPhase(index);
    return aphase.getLabel();
  }

  public Phase getPhase(int index) {
    return getSample().getPhase(index);
  }

  public void removePhase(int index) {
    parameterloopField[0].removeItemAt(index);
    normalizePhaseQuantity(false);
  }

  public void addPhase() {
//    System.out.println(numberofelementPL(0));
    double phaseQt = 1.0;
    if (numberofelementPL(0) > 0)
      phaseQt /= numberofelementPL(0);
    Parameter par = new Parameter(this, new String("Volume fraction of "
        + getPhaseName(numberofelementPL(0))), 0, 0, 1);
    par.setPositiveOnly();
    addparameterloopField(0, par);
    int phasenumber = numberofelementPL(0);
    setPhaseQuantity(phasenumber - 1, Double.toString(phaseQt));
    update(false);
    normalizePhaseQuantity(false);
  }

  public void setPhaseQuantity(int index, String avalue) {
    int phasenumber = checkPhaseNumber();
    if (index >= 0 && index < phasenumber) {
      ((Parameter) parameterloopField[0].elementAt(index)).setValue(avalue);
    }
  }

  public Parameter getPhaseQuantity(int index) {
    int phasenumber = checkPhaseNumber();
    if (index >= 0 && index < phasenumber)
      return getPhaseQuantityNoCheck(index);
    else
      return null;
  }

	@Override
	public void updateStringtoDoubleBuffering(boolean firstLoading) {
		super.updateStringtoDoubleBuffering(firstLoading);
		qc_from_phase = getQcFromPhase();
	}

	public void updateParametertoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);

    for (int i = 0; i < numberofelementPL(0); i++)
      ((Parameter) parameterloopField[0].elementAt(i)).setPositiveOnly();
    parameterField[0].setPositiveOnly();
    parameterField[1].setPositiveOnly();
    parameterField[2].setPositiveOnly();
    parameterField[3].setPositiveOnly();
	  if (getSample().getQcFromPhase() || getQcFromPhase()) {
		  isAbilitatetoRefresh = false;
		  double qc = computeQcFromPhaseComposition();
		  isAbilitatetoRefresh = true;
		  if (qc != parameterValues[1])
			  parameterField[1].setValue(qc);
	  }
  }

	private double computeQcFromPhaseComposition() {
		double quantity[] = getVolumePhaseQuantity();
		double qc = 0;
		for (int i = 0; i < quantity.length; i++) {
			qc += quantity[i] * getSample().getPhase(i).getQc();
		}
		return qc;
	}

	public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      for (int i = 0; i < Nparameter; i++)
        if (source == parameterField[i]) {
          if (i == 0) {
            notifyParameterChanged(source, Constants.THICKNESS_CHANGED);
            notifyParameterChanged(source, Constants.REFLECTIVITY_PARAMETER_CHANGED);
//ll	          resetTables();
	          return;
          } else {
            notifyParameterChanged(source, Constants.REFLECTIVITY_PARAMETER_CHANGED);
            return;
          }
        }
      for (int i = 0; i < Nparameterloop; i++) {
        for (int j = 0; j < numberofelementPL(i); j++) {
          Parameter apar = (Parameter) parameterloopField[i].elementAt(j);
          if (apar == source) {
            notifyParameterChanged(source, Constants.PHASE_WEIGHT_CHANGED);
	          chemicalComposition = null;
//ll	          resetTables();
            return;
          }
        }
      }
      super.notifyParameterChanged(source);
    }
  }

	public void refreshForNotificationDown(XRDcat source, int reason) {
		refreshComputation = true;
		if (reason == Constants.CELL_CHANGED || reason == Constants.STRUCTURE_FACTOR_CHANGED
		|| reason == Constants.ATOM_POSITION_CHANGED || reason == Constants.FRAGMENT_POSITION_CHANGED) {
			chemicalComposition = null;
//ll			resetTables();
		}
//    System.out.println("Phase down: " + refreshComputation);
	}

	public void readall(CIFtoken ciffile) {
    super.readall(ciffile);
    if (getVersion() < 1.60)
      checkThickness();
//    refreshAll(false);

  }

  public void refreshAll(boolean firstLoading) {
//    System.out.println("Before " + numberofelementPL(0));
    checkPhaseNumber();
//    System.out.println("After " + numberofelementPL(0));
	  chemicalComposition = null;
//ll	  resetTables();
	  super.refreshAll(firstLoading);
  }

  public void checkThickness() {
    setThickness(getThicknessInAngstrom() * 1E7);
  }

  public Parameter getThickness() {
    return parameterField[0];
  }

  public double getThicknessValue() {
    return getThicknessInAngstrom() * 1E-7;
  }

	public double getThicknessInCm() {
		return getThicknessInAngstrom() * 1E-8;
	}

	public double getThicknessInAngstrom() {
    return getParameterValue(0);
  }

  public void setThickness(String avalue) {
    getThickness().setValue(avalue);
  }

  public void setThickness(double avalue) {
    getThickness().setValue(avalue);
  }

  public Parameter getCriticalQc() {
    return parameterField[1];
  }

  public double getCriticalQcValue() {
    return getCriticalQc().getValueD();
  }

  public void setCriticalQc(String avalue) {
    getCriticalQc().setValue(avalue);
  }

  public void setCriticalQc(double avalue) {
    getCriticalQc().setValue(avalue);
  }

  public Parameter getAbsorption() {  // beta should be computed from f''
    return parameterField[2];
  }

  public double getAbsorptionValue() {
    return getAbsorption().getValueD();
  }

  public void setAbsorption(String avalue) {
    getAbsorption().setValue(avalue);
  }

  public Parameter getRoughness() {
    return parameterField[3];
  }

  public double getRoughnessValue() {
    return getRoughness().getValueD();
  }

	public double getRoughnessInCm() {
		return getRoughnessValue() * 1E-8;
	}

	public void setRoughness(String avalue) {
    getRoughness().setValue(avalue);
  }

  public void setRoughness(double avalue) {
    getRoughness().setValue(avalue);
  }

  public void prepareComputation() {
    normalizePhaseQuantity(false);
    // reset hashTables for layer absorption computation
    resetTables();
  }

  public void computeBefore() {
  }

  public void computeAll() {
  }

  public void computeAfter() {
  }

  public void closeComputation() {
  }

  public void preparePartialComputation() {
  }

  public void computePartial() {
  }

  public void closePartialComputation() {
  }

  public void setZeroPhase(int index) {
    ((Parameter) parameterloopField[0].elementAt(index)).setValue(0.0);
  }

  public double getNormalizedPhaseQuantity(int index) {
    double quantityToGet = 0.0;
    int phasenumber = checkPhaseNumber();
    double quantity[] = new double[phasenumber];
    if (getSample().thinFilmBehaviour()) {
      double totalquantity = 0.0;
      for (int i = 0; i < phasenumber; i++) {
        quantity[i] = getPhaseQuantityNoCheckD(i);
        totalquantity += quantity[i];
      }
      return quantity[index] / totalquantity;
    }
    double totalquantityFixed = 0.0;
    for (int i = 0; i < phasenumber; i++) {
      if (((Parameter) parameterloopField[0].elementAt(i)).getStatusIndex() == Parameter.MANUAL_FIXED) {
        quantity[i] = getPhaseQuantityNoCheckD(i);
        totalquantityFixed += quantity[i];
      }
    }
    if (totalquantityFixed >= 1.0) { // need to normalize
      double totalquantity = 0.0;
      for (int i = 0; i < phasenumber; i++) {
        quantity[i] = getPhaseQuantityNoCheckD(i);
        totalquantity += quantity[i];
      }
//      System.out.println("Warning: need to normalized a phase fraction that was fixed because total fixed was >= 1");
      return quantity[index] / totalquantity;
    } else {
      if (((Parameter) parameterloopField[0].elementAt(index)).getStatusIndex() == Parameter.MANUAL_FIXED)
        return quantity[index];
      double totalquantity = 0.0;
      for (int i = 0; i < phasenumber; i++) {
        if (((Parameter) parameterloopField[0].elementAt(i)).getStatusIndex() != Parameter.MANUAL_FIXED) {
          quantity[i] = getPhaseQuantityNoCheckD(i);
          totalquantity += quantity[i];
        }
      }
      if (totalquantity > 0.0)
        quantityToGet = quantity[index] * (1.0 - totalquantityFixed) / totalquantity;
      else
        quantityToGet = quantity[index];
    }
    return quantityToGet;
  }

  public double getPhaseScaleFactor(int index) {
    int phasenumber = checkPhaseNumber();
    double quantity[] = new double[phasenumber];
    for (int i = 0; i < phasenumber; i++) {
      quantity[i] = getPhaseQuantityNoCheckD(i);
    }
    return quantity[index];
  }

	public double[] getVolumePhaseQuantity() {

		int phasenumber = checkPhaseNumber();

		double quantity[] = new double[phasenumber];
		double totalquantity = 0.0;
		for (int i = 0; i < phasenumber; i++) {
			quantity[i] = getPhaseQuantityNoCheckD(i);
			totalquantity += quantity[i];
		}
		for (int i = 0; i < phasenumber; i++)
			quantity[i] /= totalquantity;
		return quantity;
	}

	public double[] getWeightedPhaseQuantity() {

    int phasenumber = checkPhaseNumber();

    double quantity[] = new double[phasenumber];
    double totalquantity = 0.0;
    for (int i = 0; i < phasenumber; i++) {
      quantity[i] = getPhaseQuantityNoCheckD(i) * getPhase(i).getDensity();
      totalquantity += quantity[i];
    }
    for (int i = 0; i < phasenumber; i++)
      quantity[i] /= totalquantity;
    return quantity;
  }

  public double getWeightedErrorPhaseQuantity(int index) {

    int phasenumber = checkPhaseNumber();

    double totalquantity = 0.0;
    for (int i = 0; i < phasenumber; i++)
      totalquantity += getPhaseQuantityNoCheckD(i) * getPhase(i).getDensity();
    return Double.parseDouble(((Parameter) parameterloopField[0].elementAt(index)).getError()) *
        getPhase(index).getDensity() / totalquantity;
  }

  public double getVolumeErrorPhaseQuantity(int index) {
    return Double.parseDouble(((Parameter) parameterloopField[0].elementAt(index)).getError());
  }

  public double getWeightedPhaseQuantity(int index) {
    double quantity[] = getWeightedPhaseQuantity();
    return quantity[index];
  }

  private Parameter getPhaseQuantityNoCheck(int index) {
    return (Parameter) parameterloopField[0].elementAt(index);
  }

  public double getPhaseQuantityNoCheckD(int index) {
    return Math.abs(getParameterLoopValues(0, index));
  }

  public void normalizePhaseQuantity(boolean complete) {
//    if (getFilePar().isComputingDerivate())
//      return;
    if (getSample().thinFilmBehaviour()) {
      double totalquantity = 0.0;

      int phasenumber = checkPhaseNumber();

      for (int i = 0; i < phasenumber; i++)
        totalquantity += getPhaseQuantityNoCheckD(i);
      if (totalquantity != 0.0) {
        for (int i = 0; i < phasenumber; i++) {
          double avalue = getPhaseQuantityNoCheckD(i) / totalquantity;
          if (!getFilePar().isComputingDerivate())
            ((Parameter) parameterloopField[0].elementAt(i)).setValue(avalue);
        }
      } else {
        for (int i = 0; i < phasenumber; i++) {
          double avalue = 1.0 / phasenumber;
          ((Parameter) parameterloopField[0].elementAt(i)).setValue(avalue);
        }
      }
    } else {
      double totalquantity = 0.0;

      int phasenumber = checkPhaseNumber();
      double[] quantity = new double[phasenumber];

      double totalquantityFixed = 0.0;
      for (int i = 0; i < phasenumber; i++) {
        if (((Parameter) parameterloopField[0].elementAt(i)).getStatusIndex() == Parameter.MANUAL_FIXED) {
          quantity[i] = getPhaseQuantityNoCheckD(i);
          totalquantityFixed += quantity[i];
        }
      }
      if (totalquantityFixed > 1.0) {
//        System.out.println("Warning: need to normalized volume phase fractions that were fixed because total fixed is >= 1");
        for (int i = 0; i < phasenumber; i++)
          totalquantity += getPhaseQuantityNoCheckD(i);
        if (totalquantity != 0.0) {
          for (int i = 0; i < phasenumber; i++) {
            double avalue = getPhaseQuantityNoCheckD(i) / totalquantity;
            if (!(getFilePar().isComputingDerivate() && !getSample().thinFilmBehaviour()))
              ((Parameter) parameterloopField[0].elementAt(i)).setValue(avalue);
          }
          if (!getFilePar().isComputingDerivate() && !getSample().thinFilmBehaviour())
            getFilePar().multiplyScaleFactorsBy(totalquantity);
        } else {
          if (!getFilePar().isComputingDerivate())
            for (int i = 0; i < phasenumber; i++) {
              double avalue = 1.0 / phasenumber;
              ((Parameter) parameterloopField[0].elementAt(i)).setValue(avalue);
            }
        }
      } else {
        for (int i = 0; i < phasenumber; i++)
          if (((Parameter) parameterloopField[0].elementAt(i)).getStatusIndex() != Parameter.MANUAL_FIXED)
            totalquantity += getPhaseQuantityNoCheckD(i);
        if (totalquantity != 0.0) {
          for (int i = 0; i < phasenumber; i++) {
            if (((Parameter) parameterloopField[0].elementAt(i)).getStatusIndex() != Parameter.MANUAL_FIXED) {
              double avalue = getPhaseQuantityNoCheckD(i) * (1.0 - totalquantityFixed) / totalquantity;
              if (!(getFilePar().isComputingDerivate() && !getSample().thinFilmBehaviour()))
                ((Parameter) parameterloopField[0].elementAt(i)).setValue(avalue);
            }
          }
          if (!getFilePar().isComputingDerivate())
            getFilePar().multiplyScaleFactorsBy(totalquantityFixed + totalquantity);
        } else {
          int phaseFree = 0;
          for (int i = 0; i < phasenumber; i++)
            if (((Parameter) parameterloopField[0].elementAt(i)).getStatusIndex() != Parameter.MANUAL_FIXED)
              phaseFree++;
          if (phaseFree > 0 && !getFilePar().isComputingDerivate()) {
            for (int i = 0; i < phasenumber; i++) {
              if (((Parameter) parameterloopField[0].elementAt(i)).getStatusIndex() != Parameter.MANUAL_FIXED) {
                double avalue = (1.0 - totalquantityFixed) / phaseFree;
                ((Parameter) parameterloopField[0].elementAt(i)).setValue(avalue);
              }
            }
            if (!getFilePar().isComputingDerivate())
              getFilePar().multiplyScaleFactorsBy(totalquantityFixed + totalquantity);
          }
        }
      }
/*      double totalquantity = 0.0;

      int phasenumber = checkPhaseNumber();

      for (int i = 0; i < phasenumber; i++)
        totalquantity += getPhaseQuantityNoCheckD(i);
      if (totalquantity != 0.0) {
        for (int i = 0; i < phasenumber; i++) {
          double avalue = getPhaseQuantityNoCheckD(i) / totalquantity;
          if (!getFilePar().isComputingDerivate())
            ((Parameter) parameterloopField[0].elementAt(i)).setValue(avalue);
        }
        if (!getFilePar().isComputingDerivate())
          getFilePar().multiplyScaleFactorsBy(totalquantity);
      } else {
        for (int i = 0; i < phasenumber; i++) {
          double avalue = 1.0 / phasenumber;
          ((Parameter) parameterloopField[0].elementAt(i)).setValue(avalue);
        }
      }*/

    }
	  chemicalComposition = null;
  }

  public void normalizePhaseQuantity(double phaseLimitForRemove) {
    normalizePhaseQuantity(true);

    int phasenumber = checkPhaseNumber();

    for (int i = 0; i < phasenumber; i++)
      if (getPhaseQuantityNoCheckD(i) < phaseLimitForRemove)
        ((Parameter) parameterloopField[0].elementAt(i)).setValue(0.0);

    normalizePhaseQuantity(true);
  }

//ll  Hashtable layerAbsorption = new Hashtable();
//ll  Hashtable overlayerAbsorption = new Hashtable();

  public void resetTables() {
//	    System.out.println("Resetting layer absorption tables!");
//ll    layerAbsorption = new Hashtable();
//ll    overlayerAbsorption = new Hashtable();
  }

  public double getLayerAbsorption(RadiationType rad) {
    if (rad == null)
      return 1.0;
    double[] absorption = new double[1];
    absorption[0] = 0.0;
//ll    if (layerAbsorption == null)
//ll      resetTables();
//ll    if (layerAbsorption.containsKey(rad))
//ll      absorption = (double[]) layerAbsorption.get(rad);
//ll    else {
      absorption[0] = getThicknessValue() * getAbsorption(rad);
//	    System.out.println(getThicknessValue() + " ----- " + getAbsorption(rad));
//ll      layerAbsorption.put(rad, absorption);
//ll    }
    return absorption[0];
  }

	public double getOverLayerAbsorption(RadiationType rad) {
		if (rad == null)
			return 0.0;
		double[] totalthick = new double[1];
		totalthick[0] = 0.0;
//ll		if (overlayerAbsorption.containsKey(rad))
//ll			totalthick = (double[]) overlayerAbsorption.get(rad);
//ll		else {
			Sample asample = (Sample) getParent();
			boolean mustStop = false;
			for (int j = 0; j < asample.layersnumber() && !mustStop; j++) {
				Layer alayer = asample.getlayer(j);
				if (alayer != this)
					totalthick[0] += alayer.getLayerAbsorption(rad);
				else
					mustStop = true;
			}
//ll			overlayerAbsorption.put(rad, totalthick);
//ll		}
		return totalthick[0];
	}

	public double getLayerAbsorption(double energyInKeV) {
		return getThicknessInCm() * getAbsorption(energyInKeV);
	}

	public double getOverLayerAbsorption(double energyInKeV) {
		Sample aSample = (Sample) getParent();
		boolean mustStop = false;
		double totalAbsorption = 0;
		for (int j = 0; j < aSample.layersnumber() && !mustStop; j++) {
			Layer aLayer = aSample.getlayer(j);
			if (aLayer == this)
				mustStop = true;
			else
				totalAbsorption += aLayer.getLayerAbsorption(energyInKeV);
		}
		return totalAbsorption;
	}

	public boolean checkForOldPhasesBehaviour() {
    boolean oldBehaviour = false;
    int phasenumber = checkPhaseNumber();

    for (int i = 0; i < phasenumber; i++) {
      Parameter apar = ((Parameter) parameterloopField[0].elementAt(i));
      if ((apar.getValueD() > 0.0 && apar.getValueD() < 1.0) &&
          apar.getStatusIndex() == Parameter.MANUAL_FIXED)
        oldBehaviour = true;
    }
//    System.out.println("Layer " + oldBehaviour);

    return oldBehaviour;
  }

  public double getzPosition() {
    Layer layerBelow = getLayerBelow();
    if (layerBelow != null) {
	    if (layerBelow.getLayerBelow() == null) // last layer, thickness must be zero for reflectivity (forced)
		    return layerBelow.getzPosition();
	    else
        	return layerBelow.getzPosition() + layerBelow.getThicknessInAngstrom();
    }
    return 0.0;
  }

  public Layer getLayerOver() {
    Sample asample = (Sample) getParent();
    Layer player = null;
    for (int j = 0; j < asample.layersnumber(); j++) {
      Layer alayer = asample.getlayer(j);
      if (alayer != this)
        player = alayer;
      else
        return player;
    }
    return player;
  }

  public Layer getLayerBelow() {
    Sample asample = (Sample) getParent();
    Layer player = null;
    for (int j = asample.layersnumber() - 1; j > 0; j--) {
      Layer alayer = asample.getlayer(j);
      if (alayer != this)
        player = alayer;
      else
        return player;
    }
    return player;
  }

  public double getAbsorption(RadiationType rad) {
    double absorption = 0.0;
    int phasenumber = checkPhaseNumber();
//    normalizePhaseQuantity(); // to be sure it's normalized
    double[] quantity = getWeightedPhaseQuantity();
    for (int i = 0; i < phasenumber; i++) {
//			double quantity = getNormalizedPhaseQuantity(i);
      Phase aphase = getSample().getPhase(i);
      absorption += aphase.getAbsorption(rad) * quantity[i];
    }
    return absorption;
  }

	public double getAbsorption(double energyInKeV) {
		double absorption = 0.0;
		int phasenumber = checkPhaseNumber();
//    normalizePhaseQuantity(); // to be sure it's normalized
		double[] quantity = getWeightedPhaseQuantity();
		for (int i = 0; i < phasenumber; i++) {
//			double quantity = getNormalizedPhaseQuantity(i);
			Phase aphase = getSample().getPhase(i);
			absorption += aphase.getAbsorption(energyInKeV) * quantity[i];
		}
		return absorption;
	}

	public double getDensity() {
    double density = 0.0;
    int phasenumber = checkPhaseNumber();
//    normalizePhaseQuantity(); // to be sure it's normalized
//		double[] quantity = getWeightedPhaseQuantity();
    for (int i = 0; i < phasenumber; i++) {
      double quantity = getNormalizedPhaseQuantity(i);
      Phase aphase = getSample().getPhase(i);
      density += aphase.getDensity() * quantity;
    }
    return density;
  }

  public Vector<AtomQuantity> getChemicalComposition() {
	  if (chemicalComposition == null) {
		  chemicalComposition = new Vector<AtomQuantity>();
		  int phasenumber = checkPhaseNumber();
		  double[] fraction = new double[phasenumber];
		  double[] quantity = new double[phasenumber];
		  for (int i = 0; i < phasenumber; i++) {
			  fraction[i] = getNormalizedPhaseQuantity(i);
			  quantity[i] = fraction[i] * getPhase(i).getDensity();
		  }
//			  System.out.println(getPhase(i).toString());
		  double totAtomFraction = 0;
		  double totAtomWtFraction = 0;
		  for (int i = 0; i < phasenumber; i++) {
			  Vector<AtomQuantity> composition = getPhase(i).getChemicalComposition();
			  double[] atomicFractions = new double[composition.size()];
			  double[] weightFractions = new double[composition.size()];
			  double totalFraction = 0;
			  double totalWeight = 0;
			  for (int i1 = 0; i1 < composition.size(); i1++) {
				  weightFractions[i1] = composition.elementAt(i1).quantity_weight;
				  totalWeight += weightFractions[i1];
				  atomicFractions[i1] = composition.elementAt(i1).quantity;
				  totalFraction += atomicFractions[i1];
			  }
			  for (int i1 = 0; i1 < composition.size(); i1++) {
				  weightFractions[i1] /= totalWeight;
				  atomicFractions[i1] /= totalFraction;
			  }
			  for (int j = 0; j < composition.size(); j++) {
				  weightFractions[j] *= quantity[i];
				  atomicFractions[j] *= fraction[i];
				  totAtomWtFraction += weightFractions[j];
				  totAtomFraction += atomicFractions[j];
				  AtomQuantity anAtomQuantity = new AtomQuantity(composition.elementAt(j).label, composition.elementAt(j).mass,
						  atomicFractions[j], weightFractions[j]);
				  int index = anAtomQuantity.getPositionIn(chemicalComposition);
				  if (index >= 0) {
					  anAtomQuantity = chemicalComposition.elementAt(index);
//					  System.out.print(anAtomQuantity.label + " starting " + anAtomQuantity.quantity);
//					  System.out.print(", adding " + atomicFractions[j]);
					  anAtomQuantity.quantity += atomicFractions[j];
					  anAtomQuantity.quantity_weight += weightFractions[j];
//					  System.out.println(", final " + chemicalComposition.elementAt(index).quantity);
				  } else {
					  chemicalComposition.add(anAtomQuantity);
				  }
			  }
		  }
		  double totAtomFromWtFraction = 0;
		  double[] fromWeightFractions = new double[chemicalComposition.size()];
		  for (int i1 = 0; i1 < chemicalComposition.size(); i1++) {
			  AtomQuantity atomQ = chemicalComposition.elementAt(i1);
			  atomQ.quantity_weight /= totAtomWtFraction;
			  atomQ.quantity /= totAtomFraction;
			  fromWeightFractions[i1] = atomQ.quantity_weight / atomQ.mass;
			  totAtomFromWtFraction += fromWeightFractions[i1];
		  }
		  for (int i1 = 0; i1 < chemicalComposition.size(); i1++) {
			  fromWeightFractions[i1] /= totAtomFromWtFraction;
//			  System.out.println(chemicalComposition.elementAt(i1).label + " " + chemicalComposition.elementAt(i1).quantity + " " + chemicalComposition.elementAt(i1).quantity_weight + " " + fromWeightFractions[i1]);
		  }
	  }
//	  System.out.print("Layer " + toString() + " ");
/*	  for (int j = 0; j < chemicalComposition.size(); j++)
		  System.out.print(chemicalComposition.elementAt(j).label + " " + chemicalComposition.elementAt(j).quantity + ", ");
	  System.out.println();*/

	  return chemicalComposition;
  }

	public void printCustomInformations(OutputStream out) throws IOException {
		// to be implemented by subclasses
		Vector<AtomQuantity> composition = getChemicalComposition();
		printLine(out, "       AtomSite   fractions");
		printLine(out, "n  label  atom  fraction(at)  fraction(wt)");
		for (int i1 = 0; i1 < composition.size(); i1++)
			printLine(out, (i1 + 1) + ") " + composition.elementAt(i1).label
					+ " " + composition.elementAt(i1).quantity  + " " + composition.elementAt(i1).quantity_weight);
		newLine(out);
	}

/*	public double[] getComplexPermittivity(Radiation rad) {
		double lambda = rad.getWavelength().getValueD();
		lambda *= 1.0E-8; // in cm
		Vector<AtomQuantity> composition = getChemicalComposition();
		double[] atomicFractions = new double[composition.size()];
		double totalWeight = 0;
		for (int i = 0; i < composition.size(); i++) {
			atomicFractions[i] = composition.elementAt(i).quantity;
			totalWeight += atomicFractions[i];
		}
		for (int i = 0; i < composition.size(); i++)
			atomicFractions[i] /= totalWeight;
		double meanScatteringFactor = 0;
		double meanAtomicMass = 0;
		for (int i = 0; i < composition.size(); i++) {
			meanScatteringFactor += AtomInfo.retrieveAtomNumber(composition.elementAt(i).label) * atomicFractions[i];
			meanAtomicMass += AtomInfo.retrieveAtomWeight(composition.elementAt(i).label) * atomicFractions[i];
		}
		double[] complexPermittivity = new double[2];
		double density = getDensity();
//		System.out.println("Layer density: " + density + " " + meanAtomicMass);
		complexPermittivity[0] = Constants.AVOGADRO * Constants.E_RADIUS_CM * lambda * lambda * density *
				meanScatteringFactor / (Constants.PI * meanAtomicMass);
		double cost = lambda * getAbsorption(rad) * density * 0.5 / Constants.PI;
		complexPermittivity[1] = cost * Math.sqrt(1.0 - complexPermittivity[0] + cost * cost * 0.25);  // -
		return complexPermittivity;
	}*/

	public double[] getComplexPermittivityDiff(double energyInKeV) {
//		double energyInKeV = Constants.ENERGY_LAMBDA / lambda * 0.001;
//		System.out.println(energyInKeV + " " + lambda);
		double lambda = Constants.ENERGY_LAMBDA / energyInKeV * 1.0E-11; // in cm
//		Vector<AtomQuantity> composition = getChemicalComposition();
		double[] atomicFractions = new double[chemicalComposition.size()];
		double totalWeight = 0;
		for (int i = 0; i < chemicalComposition.size(); i++) {
			atomicFractions[i] = chemicalComposition.elementAt(i).quantity;
			totalWeight += atomicFractions[i];
		}
		for (int i = 0; i < chemicalComposition.size(); i++)
			atomicFractions[i] /= totalWeight;
		double meanScatteringF1 = 0;
		double meanScatteringF2 = 0;
		double meanAtomicMass = 0;
		for (int i = 0; i < chemicalComposition.size(); i++) {
			int atomNumber = AtomInfo.retrieveAtomNumber(chemicalComposition.elementAt(i).label);
			double[] f1f2 = XRayDataSqLite.getF1F2FromHenkeForAtomAndEnergy(atomNumber, energyInKeV);
//			System.out.println(i + " " + composition.elementAt(i).label + " " + atomicFractions[i] + " " + f1f2[0]);
			meanScatteringF1 += f1f2[0] * atomicFractions[i];
			meanScatteringF2 += f1f2[1] * atomicFractions[i];
			meanAtomicMass += AtomInfo.retrieveAtomWeight(chemicalComposition.elementAt(i).label) * atomicFractions[i];
		}
		double[] complexPermittivity = new double[2];
		double density = getDensity();
		double l_fact = Constants.AVOGADRO * Constants.E_RADIUS_CM * lambda * lambda * density
									 / (2.0 * Constants.PI * meanAtomicMass);
		double d = l_fact * meanScatteringF1;  // d of Giancarlo
		double b = l_fact * meanScatteringF2;  // b of Giancarlo
		complexPermittivity[0] = 2.0 * d - d * d + b * b;
		complexPermittivity[1] = 2.0 * (1.0 - d) * b;
//		System.out.print(complexPermittivity[0] + " " + complexPermittivity[1] + " ");
		return complexPermittivity;
	}

	public void merge(basicObj obj) {
    if (Misc.areClassCompatibles("it.unitn.ing.rista.diffr.Layer", obj.getClass())) {
      Layer layerToMerge = (Layer) obj;

      double thick1 = getThicknessInAngstrom();
      double thick2 = layerToMerge.getThicknessInAngstrom();
      double totThick = thick1 + thick2;
      if (getThickness().getReducedStatusIndex() == Parameter.MANUAL_BOUND)
        getThickness().setNotRefinable();
      setThickness(totThick);

      Vector parList = getParameterVector(true, false);
      Vector masterList = layerToMerge.getParameterVector(true, false);
      if (parList.size() == masterList.size()) {
        // first is thickness
        for (int j = 1; j < parList.size(); j++) {
          Parameter par1 = (Parameter) parList.elementAt(j);
          Parameter par2 = (Parameter) masterList.elementAt(j);
          if (par1.getReducedStatusIndex() == Parameter.MANUAL_BOUND)
            par1.setNotRefinable();
          par1.setValue((par1.getValueD() * thick1 + par2.getValueD() * thick2) / totThick);
        }
      }


    }
  }

/*	public Complex[][] getReflectivityTransferMatrix(double[] q2, double lambda) {
		int numberOfPoints = q2.length;
		Layer layerBelow = getLayerBelow();

		double lon = 4 * Math.PI / lambda;
		lon *= lon;
		Complex nm[][] = null;
		double qc = getCriticalQcValue();
		qc *= qc;
		double absorption = 0.5 * getAbsorptionValue() * lon;

		if (layerBelow == null) {
//		System.out.println("first layer");
//		System.out.println(toXRDcatString());
			nm = new Complex[5][numberOfPoints];
			for (int j = 0; j < numberOfPoints; j++) {
				nm[0][j] = new Complex(1.0, 0.0);
				nm[1][j] = new Complex(0.0, 0.0);
				nm[2][j] = new Complex(0.0, 0.0);
				nm[3][j] = new Complex(1.0, 0.0);

				// nm[4] contains the Kz
				nm[4][j] = new Complex((q2[j] - qc) / 4, absorption);
				nm[4][j] = nm[4][j].sqrt();
			}
			return nm;
		}

		double zn = getzPosition();

		nm = layerBelow.getReflectivityTransferMatrix(q2, lambda);

		double roughness = MoreMath.pow(layerBelow.getRoughnessValue(), 2) / 2.0;

//		System.out.println(toXRDcatString());

		Complex kz, pkz, mkz, r1, m, p, tm, tmm, rug2, rug1, tp;
		Complex am[] = new Complex[4];

		for (int j = 0; j < numberOfPoints; j++) {
//		System.out.println(Math.sqrt(q2[j]));
			kz = new Complex((q2[j] - qc) / 4, absorption);
     	kz = kz.sqrt();
     	pkz = kz.add(nm[4][j]);
     	mkz = kz.subtract(nm[4][j]);
     	r1 = nm[4][j].divide(kz);
     	m = r1.add(-1.0).multiply(-1.0);
     	p = r1.add(1.0);
      tm = Complex.exp(mkz.multiply(zn).multiply(Complex.I));
	 		tmm = Complex.exp(mkz.multiply(zn).multiply(Complex.I).multiply(-1.0));
      tp = Complex.exp(pkz.multiply(zn).multiply(Complex.I));
      rug1 = pkz.sqr().multiply(-roughness);
      rug2 = mkz.sqr().multiply(-roughness);
      rug1 = Complex.exp(rug1);
      rug2 = Complex.exp(rug2);
      m = m.multiply(rug1);
      p = p.multiply(rug2);
	 		am[0] = p.multiply(tmm).multiply(0.5);
	 		am[1] = m.divide(tp).multiply(0.5);
	 		am[2] = m.multiply(tp).multiply(0.5);
	 		am[3] = p.multiply(tm).multiply(0.5);
      nm[0][j] = am[0].multiply(nm[0][j]).add(am[1].multiply(nm[2][j]));
      nm[1][j] = am[0].multiply(nm[1][j]).add(am[1].multiply(nm[3][j]));
      nm[2][j] = am[2].multiply(nm[0][j]).add(am[3].multiply(nm[2][j]));
      nm[3][j] = am[2].multiply(nm[1][j]).add(am[3].multiply(nm[3][j]));
      nm[4][j] = kz;
//		System.out.println(nm[0][j].toXRDcatString());
//		System.out.println(nm[1][j].toXRDcatString());
//		System.out.println(nm[2][j].toXRDcatString());
//		System.out.println(nm[3][j].toXRDcatString());
//		System.out.println(nm[4][j].toXRDcatString());
		}

		return nm;
	}*/

/* extracted from Rietquan 2.3

         weightt! = 0
         volumetric! = 0
         For nphas = 1 To phasen
                volumetric! = volumetric! + PARM!(nphas)
                weightt! = weightt! + PARM!(nphas) * Rhophase!(nphas)
         Next nphas
     rhotot! = weightt! / volumetric!
     MusRHOtot! = 0
     For nphas = 1 To phasen
        MUsRHOp!(nphas) = 0
        weighttot! = 0
        For Jx = 1 To Atomn(nphas)
            PCK = PhaseTotal(nphas - 1) + PAC + (Jx - 1) * NPADT + NPAD + NON
            weighttot! = weighttot! + NATOM(nphas) * Watom!(nphas, Jx) * PARM!(PCK)
            MUsRHOp!(nphas) = MUsRHOp!(nphas) + NATOM(nphas) * Watom!(nphas, Jx) * PARM!(PCK) * MUsRHO!(nphas, Jx)
        Next Jx
        MUsRHOp!(nphas) = MUsRHOp!(nphas) / weighttot!
        MusRHOtot! = MusRHOtot! + MUsRHOp!(nphas) * PARM!(nphas) * Rhophase!(nphas) / weightt!
     Next nphas
     ReDim Muf!(phasen), volfrac!(phasen), weightfrac!(phasen) ' Mui!(phasen), Rhomatr!(phasen)
     weightnew! = 0
     volnew! = 0
    For nphas = 1 To phasen
        volnew! = volnew! + PARM!(nphas)
        weightnew! = weightnew! + PARM!(nphas) * Rhophase!(nphas)
    Next nphas
    For nphas = 1 To phasen
        volfrac!(nphas) = PARM!(nphas) / volnew!
        weightfrac!(nphas) = PARM!(nphas) * Rhophase!(nphas) / weightnew!
    Next nphas
    ReDim mr#(phasen)
    For kit = 1 To 10
        For nphas = 1 To phasen
            mr#(nphas) = (MUsRHOp!(nphas) * Rhophase!(nphas) - MusRHOtot! * rhotot!) * Grain!(nphas) / 10000#
            If Left$(LCase$(inf$(totinstr + 11)), 4) <> "anal" Then
                Muf!(nphas) = 1#
            Else
                Select Case Left$(LCase$(inf$(totinstr + 12)), 4)
                    Case "cubi"
                        Muf!(nphas) = (1# - Exp(-mr#(nphas))) / mr#(nphas) * (1# - Exp(-mr#(nphas))) / mr#(nphas)
                    Case "sphe"
                        y0! = -0.00229
                        A1! = 2.0754
                        t1! = 0.69525
                        mr0# = -0.50356
                        mr#(nphas) = mr#(nphas) / 2
                        Muf!(nphas) = y0! + A1! * Exp(-(mr#(nphas) - mr0#) / t1!)
                    Case "line"
                        Muf!(nphas) = (1# - Exp(-mr#(nphas))) / mr#(nphas)
                    Case "brin"
                        Muf!(nphas) = (1# - Exp(-mr#(nphas))) / mr#(nphas)
                    Case "vien"
                        denom# = 1# - 2# * (1# - volfrac!(nphas)) * mr#(nphas)
                        Muf!(nphas) = 1 / (MusRHOtot! * rhotot! / denom#)
                    Case Else
                        Muf!(nphas) = 1#
                End Select
            End If
        Next nphas
        MusRHOtot! = 0
        volnew! = 0
        weightnew! = 0
        For nphas = 1 To phasen
            volfrac!(nphas) = PARM!(nphas) / Muf!(nphas)
            volnew! = volnew! + volfrac!(nphas)
            weightfrac!(nphas) = PARM!(nphas) / Muf!(nphas) * Rhophase!(nphas)
            weightnew! = weightnew! + weightfrac!(nphas)
            MusRHOtot! = MusRHOtot! + MUsRHOp!(nphas) * PARM!(nphas) * Rhophase!(nphas) / Muf!(nphas)
        Next nphas
        For nphas = 1 To phasen
            volfrac!(nphas) = volfrac!(nphas) / volnew!
            weightfrac!(nphas) = weightfrac!(nphas) / weightnew!
        Next nphas
        MusRHOtot! = MusRHOtot! / weightnew!
        rhotot! = weightnew! / volnew!
    Next kit

*/

  public void freeAllScaleParameters() {
    Parameter apar;
    double quantity;
    int phasenumber = checkPhaseNumber();
    boolean notthefirst = !getSample().thinFilmBehaviour();
    if (phasenumber == 1)
      notthefirst = false;

    for (int i = 0; i < phasenumber; i++) {
      apar = getPhaseQuantityNoCheck(i);
      quantity = apar.getValueD();
      if (quantity != 0.0)
        if (notthefirst)
          apar.setRefinableCheckBound();
        else
          notthefirst = true;
    }
  }

}

