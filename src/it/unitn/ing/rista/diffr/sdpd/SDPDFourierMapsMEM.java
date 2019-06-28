/*
 * @(#)SDPDFourierMapsMEM.java created 10/11/2001 Casalino
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

package it.unitn.ing.rista.diffr.sdpd;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.interfaces.*;

import java.awt.*;
import java.util.*;
import javax.swing.*;
import java.awt.event.*;

import it.unitn.ing.rista.io.cif.*;
import it.unitn.ing.rista.comp.*;

import java.io.*;

/**
 * The SDPDFourierMapsMEM is a method to solve the crystal
 * structure using Fourier Transform by Maximum Entropy Method
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.19 $, $Date: 2006/07/20 13:39:06 $
 * @since JDK1.1
 */

public class SDPDFourierMapsMEM extends StructureFactorSolveCrystalStructure implements MEMFunction {

  public static String[] diclistc = {"_rita_entropy_cycles_max",
      "_rita_entropy_iteration_max", "_rita_entropy_exponent",
      "_rita_atom_map_division_number_a",
      "_rita_atom_map_division_number_b",
      "_rita_atom_map_division_number_c",
      "_rita_entropy_store_wgt",
      "_rita_atom_map_reduced_cell"
  };
  public static String[] diclistcrm = {"_rita_entropy_cycles_max",
      "_rita_entropy_iteration_max", "_rita_entropy_exponent",
      "_rita_atom_map_division_number_a",
      "_rita_atom_map_division_number_b",
      "_rita_atom_map_division_number_c",
      "_rita_entropy_store_wgt",
      "_rita_atom_map_reduced_cell"
  };

  public static String[] classlistcs = {};
  public static String[] classlistc = {};

  int numberOfData = 0;
  int numberOfParameters = 0;
  double R = 0.0;
  double Rw = 0.0;
  double dta[] = null;
  double fit[] = null;
  double wgt[] = null;
  private double atomMap[] = null;
  boolean mapnotLoaded = true;
  int aSlices = 0;
  int bSlices = 0;
  int cSlices = 0;
  Vector totCellID = null;
  Vector totCellWGT = null;
//	double[] totalWeight = null;
  boolean reduceMemory = true;
  StructureFactorList[] structureFactorList = null;
  StructureFactor[] absentReflSF = null;
  boolean fitNotInitialized = true;
  boolean needFirstFit = true;
//  boolean useAbsentReflections = false;
  double[] reducedCell = null;
  double forceMapToLowValue = 1.0f;
  double weightMAPforce = 0.0f;
  boolean useWeightForceLowMap = false;
  boolean useReducedCell = true;
  double percentageForCut = 0.0;
  boolean isOptimizing = false;
//  int repeatMax = MaudPreferences.getInteger("meemRefinement.cyclesNumber", 3);
//  boolean useAllSites = false;
//  boolean useequivalentReflections = false;

  public SDPDFourierMapsMEM(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Disabled MEM Electron Maps";
    IDlabel = "MEM Electron Maps";
    description = "select this to use Electron Density Maps by Maximum Entropy Method";
  }

  public SDPDFourierMapsMEM(XRDcat aobj) {
    this(aobj, "MEM Electron Maps");
  }

  public SDPDFourierMapsMEM() {
    identifier = "Disabled MEM Electron Maps";
    IDlabel = "MEM Electron Maps";
    description = "select this to use Electron Maps by Maximum Entropy Method";
  }

  public void initConstant() {
    Nstring = 8;
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

    stringField[0] = MaudPreferences.getPref("meemRefinement.cyclesNumber", "3");
    stringField[1] = "10";
    stringField[2] = MaudPreferences.getPref("entropyMEM.startingExponent", "0.001");
    String default_res = MaudPreferences.getPref("atomMap.divisionNumber_even", "10");
    setResolution(default_res, default_res, default_res);
    storeConversion(MaudPreferences.getBoolean("atomMap.storeWeights", true));
    useReducedCell(MaudPreferences.getBoolean("atomMap.use_reduced_cell", true));
    mapnotLoaded = true;
  }

	public boolean solveStructure(StructureFactorList[] structureFactorList) {
		final Phase aphase = (Phase) getParent();

		FilePar aparFile = getFilePar();

//    if (!aparFile.isStructureFactorComputationPermitted())
//      return false;

//    useAllSites = MaudPreferences.getBoolean("meemMap.useAllSites", false);
//    if (reducedCell[0] != 1.0 || reducedCell[1] != 1.0 || reducedCell[2] != 1.0)
//      useAllSites = true;
//    useequivalentReflections = MaudPreferences.getBoolean("meemMap.useMultipleReflections", false);


		if (mapnotLoaded) {
			loadMapFromFile(aphase);
		}

		this.structureFactorList = structureFactorList;

		initAll();
		MaximumEntropyFit mem = new MaximumEntropyFourierMap(this, getFilePar().getMainFrame().getOutputPanel());
		mem.setIterations(getNumberofIterations());
		mem.solve(aparFile.computation);
		fitNotInitialized = false;

		if (totCellWGT != null && totCellWGT.size() > 0)
			totCellWGT.removeAllElements();
		if (totCellWGT != null && totCellWGT.size() > 0)
			totCellID.removeAllElements();
		totCellWGT = null;
		totCellID = null;

//    computeFit();
//    System.out.println("Final chi :" + getWSS());
		atomMapNormalization();
		computeLastFit();

		mapnotLoaded = false;

		return true;
	}

	void initAll() {
    Rw = 0.0;
    R = 0.0;

//    useAbsentReflections = MaudPreferences.getBoolean("reflns_list.useAbsent", false);

    numberOfData = 0;
    for (int i = 0; i < structureFactorList.length; i++) {
      int reflectionNumber = structureFactorList[i].structureFactor.length;
      for (int j = 0; j < reflectionNumber; j++) {
        StructureFactor sf = structureFactorList[i].structureFactor[j];
        sf.Fhkl_exp = sf.Fhkl_calc; // the calc is the mean value for all exp
        if (sf.weight > 0)
          numberOfData++;
      }
    }
    Phase aphase = (Phase) getParent();
/*    if (useAbsentReflections) {
      absentReflSF = new StructureFactor[aphase.absentreflectionv.size()];
      for (int j = 0; j < aphase.absentreflectionv.size(); j++) {
        Reflection refl = aphase.absentreflectionv.elementAt(j);
        StructureFactor sf = new StructureFactor(refl.h, refl.k, refl.l, refl.d_space,
            0.0, 0.0, 1.0,
            refl.hlist, refl.klist, refl.llist);
        absentReflSF[j] = sf;
        numberOfData++;
      }
    }*/
    if (useWeightForceLowMap = weightMAPforce > 0.0)
      numberOfData++;
//    FilePar aparFile = (FilePar) getFilePar();

    atomMapInitialization();
    percentageForCut = MaudPreferences.getDouble("meem.cuttingFactor", 0.01);
    cutLowDensityMap(percentageForCut);
    atomMapNormalization();
//Last    checkSimmetries();
    dta = new double[numberOfData * 2];
    wgt = new double[numberOfData];
    fit = new double[numberOfData * 2];
    needFirstFit = true;
    int index = 0;
    for (int i = 0; i < structureFactorList.length; i++) {
      int reflectionNumber = structureFactorList[i].structureFactor.length;
      for (int j = 0; j < reflectionNumber; j++) {
        StructureFactor sf = structureFactorList[i].structureFactor[j];
        if (sf.weight > 0) {
        dta[index * 2] = (double) sf.Fhkl_exp;
        dta[index * 2 + 1] = 0.0f;
        if (sf.Fhkl_exp > 1.0E-2)
          wgt[index++] = (double) Math.sqrt(sf.Fhkl_exp);
        else
          wgt[index++] = 1.0f;
//        wgt[index++] = 1.0f; //Math.max(sf.Fhkl_esd, Math.sqrt(sf.Fhkl_exp / 10.0));
//        System.out.println(sf.Fhkl_exp + " +- " + sf.Fhkl_esd);
        }
      }
    }
/*    if (useAbsentReflections) {
      for (int j = 0; j < aphase.absentreflectionv.size(); j++) {
        dta[index * 2] = 0.0f;
        dta[index * 2 + 1] = 0.0f;
        wgt[index++] = 1.0f;
      }
    }*/
    if (useWeightForceLowMap) {
      dta[index * 2] = 1.0f;
      dta[index * 2 + 1] = 1.0f;
      wgt[index++] = weightMAPforce;
    }

    if (!reduceMemory()) {
      totCellID = new Vector(numberOfData, 100);
      totCellWGT = new Vector(numberOfData, 100);

      ProgressFrame prF = null;
      if (!Constants.textonly && Constants.showProgressFrame)
        try {
          prF = new ProgressFrame(numberOfData);
        } catch (NullPointerException npe) {
          System.out.println("Not able to create frame, MacOSX display sleep bug?");
        }
      printf("Preparing Fourier Map computation...            ", prF);
      int indexsf = 0;
      for (int i = 0; i < structureFactorList.length; i++) {
        int reflectionNumber = structureFactorList[i].structureFactor.length;
        for (int j = 0; j < reflectionNumber; j++) {
          StructureFactor sf = structureFactorList[i].structureFactor[j];
          if (sf.weight > 0) {
          computeCellAndWeight(indexsf++, sf);
//          totCellID.addElement(cellID);
          totCellWGT.addElement(cellWGT);
          if (prF != null)
            prF.increaseProgressBarValue();
          }
        }
      }
/*      if (useAbsentReflections) {
        for (int j = 0; j < aphase.absentreflectionv.size(); j++) {
          computeCellAndWeight(indexsf++, absentReflSF[j]);
//          totCellID.addElement(cellID);
          totCellWGT.addElement(cellWGT);
          if (prF != null)
            prF.increaseProgressBarValue();
        }
      }*/
      if (useWeightForceLowMap) {
        computeCellAndWeight(indexsf++);
        totCellWGT.addElement(cellWGT);
        if (prF != null)
          prF.increaseProgressBarValue();
      }
      if (prF != null) {
        prF.setVisible(false);
        prF.dispose();
      }
    }
  }

  public int getNumberofIterations() {
    return Integer.valueOf(stringField[1]).intValue();
  }

  public int getCyclesNumber() {
    return Integer.valueOf(stringField[0]).intValue();
  }

  public int prepareIteration() {
    return 0;
  }

  public OutputStream getResultStream() {
    return null;  //To change body of implemented methods use File | Settings | File Templates.
  }

  public void endOfComputation() {
    //To change body of implemented methods use File | Settings | File Templates.
  }

  public boolean logOutput() {
    return false;  //To change body of implemented methods use File | Settings | File Templates.
  }

  public void closeLogResultFile() {
    //To change body of implemented methods use File | Settings | File Templates.
  }

  public OptimizationAlgorithm getOptimizationAlgorithm() {
    return null;  //To change body of implemented methods use File | Settings | File Templates.
  }

  public void fittingFileOutput() {
    //To change body of implemented methods use File | Settings | File Templates.
  }

  public void setNumberofIterations(int value) {
    stringField[1] = Integer.toString(value);
  }

  public void setNumberofCycles(int value) {
    stringField[0] = Integer.toString(value);
  }

  public double getRexponent() {
    return Double.valueOf(stringField[2]).doubleValue();
  }

  public void setResolution(String a_value, String b_value, String c_value) {
    boolean resetMap = false;
    if (a_value != null && !a_value.equals(stringField[3])) {
      stringField[3] = new String(a_value);
      resetMap = true;
    }
    if (b_value != null && !b_value.equals(stringField[4])) {
      stringField[4] = new String(b_value);
      resetMap = true;
    }
    if (c_value != null && !c_value.equals(stringField[5])) {
      stringField[5] = new String(c_value);
      resetMap = true;
    }
    if (resetMap)
      resetMAP();
  }

  public String getResolution_a() {
    return stringField[3];
  }

  public String getResolution_b() {
    return stringField[4];
  }

  public String getResolution_c() {
    return stringField[5];
  }

  public int getResolutionD_a() {
//    System.out.println("res " + getResolution());
    return Integer.valueOf(getResolution_a());
  }

  public int getResolutionD_b() {
//    System.out.println("res " + getResolution());
    return Integer.valueOf(getResolution_b());
  }

  public int getResolutionD_c() {
//    System.out.println("res " + getResolution());
    return Integer.valueOf(getResolution_c());
  }

  public boolean storeConversion() {
    return stringField[6].equalsIgnoreCase("true");
  }

  public void storeConversion(boolean status) {
    if (status)
      stringField[6] = "true";
    else
      stringField[6] = "false";
  }

  public void storeConversion(String value) {
    stringField[6] = value;
  }

  public boolean useReducedCell() {
    return stringField[7].equalsIgnoreCase("true");
  }

  public void useReducedCell(boolean status) {
    if (status)
      stringField[7] = "true";
    else
      stringField[7] = "false";
  }

  public void useReducedCell(String value) {
    stringField[7] = value;
  }

  public int getNumberOfData() {
    return numberOfData;
  }

  public double getData(int index) {
    return dta[index];
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

  public double[] getData() {
    return dta;
  }

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
//    double diff;

    for (int i = 0; i < numberOfData; i++) {
//			diff = (getFit(i) - getData(i)) * getWeight(i);
//			WSS += diff * diff;
      WSS += Math.abs(getFit(i * 2) - getData(i * 2)) / getWeight(i);
      WSS += Math.abs(getFit(i * 2 + 1) - getData(i * 2 + 1)) / getWeight(i);
//      if (i == 2) {
//      System.out.println(i + " " + getData(i * 2) + " " + getFit(i * 2) + " " + getData(i * 2 + 1) + " " + getFit(i * 2 + 1));
//      }
    }

//    System.out.println("WSS : " + WSS);
    return WSS;
  }

  public double getRw() {
    double rw = 0.0;
    double diff;
    double wgt;
    double dtat;
    double den = 0.0;

    for (int i = 0; i < numberOfData; i++) {
      dtat = getData(i * 2);
      wgt = getWeight(i);
      diff = (getFit(i * 2) - dtat) / wgt;
      rw += diff * diff;
      den += dtat * dtat / wgt / wgt;
      dtat = getData(i * 2 + 1);
      diff = (getFit(i * 2 + 1) - dtat) / wgt;
      rw += diff * diff;
      den += dtat * dtat / wgt / wgt;
    }

    if (den != 0.0)
      rw /= den;
    rw = Math.sqrt(rw);
    setRw(rw);

    return rw;
  }

  public double getR() {
    double r = 0.0;
    double dtat;
    double den = 0.0;

    for (int i = 0; i < numberOfData; i++) {
      dtat = getData(i * 2);
      r += Math.abs(getFit(i * 2) - dtat);
      den += Math.abs(dtat);
      dtat = getData(i * 2 + 1);
      r += Math.abs(getFit(i * 2 + 1) - dtat);
      den += Math.abs(dtat);
    }

    if (den != 0.0)
      r /= den;
    setR(r);

    return r;
  }

  public void setRw(double Rw) {
    this.Rw = Rw;
  }

  public void setR(double R) {
    this.R = R;
  }

  public void setRexp(double R) {
  }

  public double getSS() {
    double SS = 0.0;
    double diff;

    for (int i = 0; i < numberOfData; i++) {
      diff = getFit(i * 2) - getData(i * 2);
      SS += diff * diff;
      diff = getFit(i * 2 + 1) - getData(i * 2 + 1);
      SS += diff * diff;
    }

    return SS;
  }

  public int getNumberOfFreeParameters() {
    return numberOfParameters;
  }

  public double[] getFreeParameters(boolean initialCall) {
    double[] parameters = new double[getNumberOfFreeParameters()];

    double factor = 1.0f;
    if (initialCall)
      factor = forceMapToLowValue;
    for (int na = 0; na < getNumberOfFreeParameters(); na++)
      parameters[na] = atomMap[na] * factor;

    return parameters;
  }

  public void setFreeParameters(double[] parm) {
    for (int na = 0; na < getNumberOfFreeParameters(); na++)
      atomMap[na] = parm[na];
  }

/*  public void setFreeParameters(double[] parm) {
    for (int na = 0; na < getNumberOfFreeParameters(); na++)
      atomMap[na] = (double) parm[na];
  }
*/
  public double getFreeParameter(int index) {
    return 0.0f;
  }

  public void setFreeParameter(int index, double value) {
  }

/*  public void setFreeParameter(int index, double value) {
  }
*/
  public boolean singleFunctionComputing() {
    return false;
  }

  public void saveparameters() {
  }

  public void setErrors(double[] errors) {
  }

/*  public void setErrors(double[] errors) {
  }*/

  public void normalizeFit() {
    if (fitNotInitialized) {
      return;
    }
    if (needFirstFit) {
      computeFirstFit();
    }
//    System.out.println("Normalizing");
    int dataNumber = getNumberOfData();
    if (useWeightForceLowMap)
      dataNumber--;
    for (int i = 0; i < dataNumber; i++) {
      int i1 = i * 2;
      int i2 = i1 + 1;
      double Fobs = Math.sqrt(dta[i1] * dta[i1] + dta[i2] * dta[i2]);
      double Fcalc = Math.sqrt(fit[i1] * fit[i1] + fit[i2] * fit[i2]);
      if (Fcalc > 10E-6) {
        double norm = Fobs / Fcalc;
//        System.out.println(i + " " + dta[i1] + " " + dta[i2]);
//        System.out.println(i + " " + fit[i1] + " " + fit[i2]);
        dta[i1] = (double) (fit[i1] * norm);
        dta[i2] = (double) (fit[i2] * norm);
//        System.out.println("After " + dta[i1] + " " + dta[i2]);
      }
    }
  }

  public void computeFirstFit() {
    computeFit();
    /*
    Phase aphase = (Phase) getParent();
//    aphase.normalizeAtomMap(atomMap, nSlices);

    int index = 0;
    for (int i = 0; i < structureFactorList.length; i++) {
      int reflectionNumber = structureFactorList[i].structureFactor.length;
      RadiationType rad = structureFactorList[i].radiation;
      for (int j = 0; j < reflectionNumber; j++) {
        StructureFactor sf = structureFactorList[i].structureFactor[j];
        double[] Fhkl = aphase.getFhklraw(sf.h, sf.k, sf.l, sf.d_spacing, rad);
        sf.Fhkl_calc = Math.sqrt(Fhkl[0] * Fhkl[0] + Fhkl[1] * Fhkl[1]);
//    System.out.println(sf.Fhkl_calc);
        fit[index * 2] = (double) Fhkl[0];
        fit[index * 2 + 1] = (double) Fhkl[1];
        index++;
      }
    }*/
    needFirstFit = false;
  }

  public void computeStructureFactor(int h, int k, int l, int multiplicity, double dspacing, RadiationType radType,
                                       double defaultFactor, double[] structureFactor) {
//    int h = refl.h, k = refl.k, l = refl.l, int multiplicity = refl.multiplicity;
//    double dspacing = refl.d_space;

    if (atomMap == null)
      resetMAP();
    if (fitNotInitialized) {
	    for (int i = 0; i < structureFactor.length; i++)
		    structureFactor[i] = Constants.STARTING_STRUCTURE_FACTOR * Constants.STARTING_STRUCTURE_FACTOR;
    } else {
	    double[] Fhkl = Fhklcomp(h, k, l, atomMap, aSlices, bSlices, cSlices);
	    double Fhkls = (Fhkl[0] * Fhkl[0] + Fhkl[1] * Fhkl[1]) * multiplicity;
	    for (int i = 0; i < structureFactor.length; i++)
		    structureFactor[i] = Fhkls;
    }
  }

  public void computeFit() {
    computeFit(false);
  }

  public void computeFit(boolean lastFit) {
    Phase aphase = (Phase) getParent();
//    aphase.normalizeAtomMap(atomMap, nSlices);

    int index = 0;
    for (int i = 0; i < structureFactorList.length; i++) {
      int reflectionNumber = structureFactorList[i].structureFactor.length;
      for (int j = 0; j < reflectionNumber; j++) {
        StructureFactor sf = structureFactorList[i].structureFactor[j];
        double[] Fhkl = Fhklcomp(sf.h, sf.k, sf.l, atomMap, aSlices, bSlices, cSlices);
        sf.Fhkl_calc = Math.sqrt(Fhkl[0] * Fhkl[0] + Fhkl[1] * Fhkl[1]);
//    System.out.println(sf.Fhkl_calc);
        if (sf.weight > 0) {
        fit[index * 2] = (double) Fhkl[0];
        fit[index * 2 + 1] = (double) Fhkl[1];
//        if (index == 2)
//        System.out.println("fit "+fit[index * 2] +" "+fit[index * 2+1]);
        if (lastFit && getFilePar().isStructureFactorComputationPermitted())
          sf.Fhkl_exp = sf.Fhkl_calc;
        index++;
        } else {
          if (lastFit && getFilePar().isStructureFactorComputationPermitted())
            sf.Fhkl_exp = sf.Fhkl_calc;
        }
      }
    }
/*    if (useAbsentReflections) {
      for (int j = 0; j < absentReflSF.length; j++) {
        double[] Fhkl = Fhklcomp(absentReflSF[j].h, absentReflSF[j].k, absentReflSF[j].l,
            atomMap, aSlices, bSlices, cSlices);
        fit[index * 2] = (double) Fhkl[0];
        fit[index * 2 + 1] = (double) Fhkl[1];
        index++;
      }
    }*/
    if (useWeightForceLowMap) {
    double totalMap = 0.0;
    for (int i = 0; i < atomMap.length; i++)
      totalMap += atomMap[i];
    fit[index * 2] = (totalMap / (aphase.getAtomMapNormalization() *
        reducedCell[0] * reducedCell[1] * reducedCell[2]));
    fit[index * 2 + 1] = fit[index * 2];
    index++;
    }

  }

  public double[] Fhklcomp(int h, int k, int l, double[] map,
                           int aSlices, int bSlices, int cSlices) {
    // compute Fhkl from electron map
    double[] a1 = new double[2];
    a1[0] = 0.0;
    a1[1] = 0.0;
    Phase aphase = (Phase) getParent();
    int siteNumber = 1;
//    if (useAllSites)
      siteNumber = aphase.getPhaseInfo().getSitePositionNumber();
    double factors = aphase.getActivePlanarDefects().getStructureFactorModifier(null);
    double[] divideFactors = aphase.getActivePlanarDefects().getDivisionFactors();
    double norm = Math.sqrt(factors);// / (siteNumber * reducedCell[0] * reducedCell[1] * reducedCell[2]);
    SitePosition[] sitepos = new SitePosition[siteNumber];
    for (int i = 0; i < siteNumber; i++) {
      sitepos[i] = aphase.getPhaseInfo().getSitePosition(i);
    }
    double x[][] = new double[3][siteNumber], xf[] = new double[3];

    double fi_ra = 1.0 / aSlices * reducedCell[0];
    double fi_rb = 1.0 / bSlices * reducedCell[1];
    double fi_rc = 1.0 / cSlices * reducedCell[2];

//    double constantPart = Constants.PI * (fi_ra + fi_rb + fi_rc);
    double multPart = Constants.PI2;
    int i = 0;
    xf[0] = 0;
    double h1 = multPart * h * divideFactors[0];
    double k1 = multPart * k * divideFactors[1];
    double l1 = multPart * l * divideFactors[2];
    for (int ix = 0; ix < aSlices; ix++) {
      xf[1] = 0;
      for (int iy = 0; iy < bSlices; iy++) {
        xf[2] = 0;
        for (int iz = 0; iz < cSlices; iz++) {
          for (int is = 0; is < siteNumber; is++) {
            for (int js = 0; js < 3; js++)
              x[js][is] = sitepos[is].getcoord(js, xf);
            double arg = h1 * x[0][is] + k1 * x[1][is] + l1 * x[2][is];
            double w1 = Math.cos(arg);
            double w2 = Math.sin(arg);
            a1[0] += map[i] * w1;
            a1[1] += map[i] * w2;
          }
          i++;
          xf[2] += fi_rc;
        }
        xf[1] += fi_rb;
      }
      xf[0] += fi_ra;
    }
    a1[0] *= norm;
    a1[1] *= norm;
    return a1;
  }

  public void computeLastFit() {
    computeFit(true);
  }

  public void computeFit(double[] parmn) {
    fitNotInitialized = false;
    int dataNumber = getNumberOfData();
    Phase aphase = (Phase) getParent();
    int nprm = parmn.length;
//    double norm = aphase.getCellVolume() / computeParameterNumber();
    for (int i = 0; i < dataNumber; i++) {
      double[] cellWGT = getMEMCellWGT(i);
      double a1 = 0.0;
      double a2 = 0.0;
      for (int j = 0; j < nprm; j++) {
        a1 += parmn[j] * cellWGT[2 * j];
        a2 += parmn[j] * cellWGT[2 * j + 1];
      }
//      a1 *= norm;
//      a2 *= norm;
      fit[i * 2] = a1;
      fit[i * 2 + 1] = a2;
//      if (i == 2)
//      System.out.println("fit "+fit[i * 2] +" "+fit[i * 2+1]);
    }

  }

  public void atomMapNormalization() {
    double[] a1 = new double[2];
    a1[0] = 0.0;
    a1[1] = 0.0;
    Phase aphase = (Phase) getParent();
    int siteNumber = 1;
//    if (useAllSites)
      siteNumber = aphase.getPhaseInfo().getSitePositionNumber();
    double norm = 1.0;// / (siteNumber * reducedCell[0] * reducedCell[1] * reducedCell[2]);
    int index = 0;
    double totalMap = 0.0;
    for (int ix = 0; ix < aSlices; ix++)
      for (int iy = 0; iy < bSlices; iy++)
        for (int iz = 0; iz < cSlices; iz++)
          totalMap += atomMap[index++];
    norm = aphase.getAtomMapNormalization() / totalMap * (reducedCell[0] * reducedCell[1] * reducedCell[2]);
    index = 0;
    for (int na = 0; na < aSlices; na++)
      for (int nb = 0; nb < bSlices; nb++)
        for (int ng = 0; ng < cSlices; ng++)
          atomMap[index++] *= norm;

    System.out.println("Normalization factor: " + Fmt.format(norm) + ", total: "
        + totalMap);
//    useAllSites = MaudPreferences.getBoolean("meemMap.useAllSites", false);
//    if (reducedCell[0] != 1.0 || reducedCell[1] != 1.0 || reducedCell[2] != 1.0)
//      useAllSites = true;
//    useequivalentReflections = MaudPreferences.getBoolean("meemMap.useMultipleReflections", false);

  }

  public void cutLowDensityMap(double delta) {
    double max = -1.0;
    int index = 0;
    for (int ix = 0; ix < aSlices; ix++)
      for (int iy = 0; iy < bSlices; iy++)
        for (int iz = 0; iz < cSlices; iz++)
          if (max < atomMap[index])
            max = atomMap[index++];
    double densityToCut = max * delta;
    index = 0;
    for (int na = 0; na < aSlices; na++)
      for (int nb = 0; nb < bSlices; nb++)
        for (int ng = 0; ng < cSlices; ng++)
          if (densityToCut > atomMap[index])
            atomMap[index++] = 0.0f;

  }

  public boolean checkBound(int j, double parmn) {
    boolean bound = false;
//		if (parmn < 0.0 && parmn != -1.0)
//			bound = true;

    return bound;
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
    atomMapNormalization();
    computeFit();
  }

  public boolean reduceMemory() {
    return reduceMemory;
  }

  public void atomMapInitialization() {
	  if (reducedCell == null)
		  reducedCell = new double[3];

    reduceMemory = !storeConversion();

    Phase aphase = (Phase) getParent();

    for (int i = 0; i < 3; i++) {
      if (useReducedCell())
        reducedCell[i] = aphase.reducedCellFactor[i];
      else
        reducedCell[i] = 1.0;
    }

    aSlices = getResolutionD_a();
    bSlices = getResolutionD_b();
    cSlices = getResolutionD_c();

    numberOfParameters = computeParameterNumber();
//		totalWeight = new double[numberOfParameters];
//    useAllSites = MaudPreferences.getBoolean("meemMap.useAllSites", false);
//    if (reducedCell[0] != 1.0 || reducedCell[1] != 1.0 || reducedCell[2] != 1.0)
//      useAllSites = true;
//    useequivalentReflections = MaudPreferences.getBoolean("meemMap.useMultipleReflections", false);

  }

  public int computeParameterNumber() {
    aSlices = getResolutionD_a();
    bSlices = getResolutionD_b();
    cSlices = getResolutionD_c();

    return aSlices * bSlices * cSlices;
  }

  public Phase getPhase() {
    return (Phase) getParent();
  }

//	int lastindex = -1;
  double[] cellWGT = null;
  int[] cellID = new int[1];

  public int[] getMEMCellID(int dataindex) {
//		if (!reduceMemory()) {
//			return (int[]) totCellID.elementAt(dataindex);
//		}
//		computeCellAndWeight(dataindex);
//    checkSimmetries();
    return cellID;
  }

  public double[] getMEMCellWGT(int dataindex) {
    return (double[]) totCellWGT.elementAt(dataindex);
  }

  public double[] getMEMCountData() { // obsolete
//		return totalWeight;
    double[] norm = new double[1];
    double Vc = getPhase().getCellVolume();

    int aSlices1 = aSlices + 1;
    int bSlices1 = bSlices + 1;
    int cSlices1 = cSlices + 1;
    double totalMap = 0.0;
    double totalCells = 0.0;
    int index = 0;
    double wx = 0.5;
    for (int ix = 0; ix < aSlices1; ix++) {
      if (ix == aSlices || ix == 0)
        wx = 0.5;
      else
        wx = 1.0;
      double wy = 0.5;
      for (int iy = 0; iy < bSlices1; iy++) {
        if (iy == bSlices || iy == 0)
          wy = 0.5;
        else
          wy = 1.0;
        double wz = 0.5;
        for (int iz = 0; iz < cSlices1; iz++) {
          if (iz == cSlices || iz == 0)
            wz = 0.5;
          else
            wz = 1.0;
          double w = wx * wy * wz;
          if (atomMap[index] >= 0)
            totalMap += w * atomMap[index];
          totalCells += w;
          index++;
        }
      }
    }
    totalMap *= Vc / (totalCells * reducedCell[0] * reducedCell[1] * reducedCell[2]);
    norm[0] = totalMap;
    System.out.println("MEMcellTotal " + norm[0]);
    return norm;
  }

  public void computeCellAndWeight(int dataindex) {
    int totNumber = computeParameterNumber();
    Phase aphase = (Phase) getParent();
    double wgt = (1.0 / (aphase.getAtomMapNormalization() *
        reducedCell[0] * reducedCell[1] * reducedCell[2]));
    cellWGT = new double[totNumber * 2];
    for (int i = 0; i < totNumber; i++) {
      cellWGT[2 * i] = wgt;
      cellWGT[2 * i + 1] = wgt;
    }
  }

  public void computeCellAndWeight(int dataindex, StructureFactor sf) {
    int totNumber = computeParameterNumber();
    cellWGT = new double[totNumber * 2];
    double fi_ra = 0;
    double fi_rb = 0;
    double fi_rc = 0;
    int multeplicity = 1;
//    if (sf.reflectionhList != null && useequivalentReflections)
//      multeplicity = sf.reflectionhList.length;

    Phase aphase = (Phase) getParent();
    int siteNumber = 1;
//    if (useAllSites)
      siteNumber = aphase.getPhaseInfo().getSitePositionNumber();
    double factors = aphase.getActivePlanarDefects().getStructureFactorModifier(null);
    double[] divideFactors = aphase.getActivePlanarDefects().getDivisionFactors();
    double norm = Math.sqrt(factors); // / (siteNumber * multeplicity);
      //  * reducedCell[0] * reducedCell[1] * reducedCell[2]);
    SitePosition[] sitepos = new SitePosition[siteNumber];
    for (int i = 0; i < siteNumber; i++) {
      sitepos[i] = aphase.getPhaseInfo().getSitePosition(i);
    }
    double x[][] = new double[3][siteNumber], xf[] = new double[3];

    fi_ra = 1.0 / aSlices * reducedCell[0];
    fi_rb = 1.0 / bSlices * reducedCell[1];
    fi_rc = 1.0 / cSlices * reducedCell[2];

//    double constantPart = Constants.PI * (fi_ra + fi_rb + fi_rc);
    double multPart = Constants.PI2;
    int hi, ki, li;
    hi = sf.h;
    ki = sf.k;
    li = sf.l;

    int i = 0;
    xf[0] = 0;
    for (int ix = 0; ix < aSlices; ix++) {
      xf[1] = 0;
      for (int iy = 0; iy < bSlices; iy++) {
        xf[2] = 0;
        for (int iz = 0; iz < cSlices; iz++) {
          for (int is = 0; is < siteNumber; is++) {
            for (int js = 0; js < 3; js++)
              x[js][is] = sitepos[is].getcoord(js, xf);
            if (multeplicity > 1) {
              for (int j = 0; j < multeplicity; j++) {
                hi = sf.reflectionhList[j];
                ki = sf.reflectionkList[j];
                li = sf.reflectionlList[j];
                double arg = multPart * (hi * x[0][is] * divideFactors[0] + ki * x[1][is] * divideFactors[1] +
                    li * x[2][is] * divideFactors[2]);
//          cellID[index] = index;
                cellWGT[i] += (Math.cos(arg) * norm);
                cellWGT[i + 1] += (Math.sin(arg) * norm);
              }
            } else {
              double arg = multPart * (hi * x[0][is] / divideFactors[0] + ki * x[1][is] / divideFactors[1] +
                  li * x[2][is] / divideFactors[2]);
//          cellID[index] = index;
              cellWGT[i] += (Math.cos(arg) * norm);
              cellWGT[i + 1] += (Math.sin(arg) * norm);
            }
          }
          i += 2;
          xf[2] += fi_rc;
        }
        xf[1] += fi_rb;
      }
      xf[0] += fi_ra;
    }

  }

/*  public int MAPindex(int[] ia) {
    return MAPindex(ia[0], ia[1], ia[2]);
  }*/

  public int MAPindex(int ia, int ib, int ig) {
    return getCSlices() * getBSlices() * ia + getCSlices() * ib + ig;
  }

  String lastSpaceGroup = "";

/*  public void checkSimmetries() {
    Phase aphase = (Phase) getParent();
//    String spggrp = aphase.getSpaceGroup();
//    if (spggrp.equals(lastSpaceGroup))
//      return;

    double fi_ra = 1.0 / (aSlices + 1);
    double fi_rb = 1.0 / (bSlices + 1);
    double fi_rc = 1.0 / (cSlices + 1);

    int siteNumber = aphase.getSitePositionNumber();
    SitePosition[] sitepos = new SitePosition[siteNumber];
    for (int i = 0; i < siteNumber; i++) {
      sitepos[i] = (SitePosition) aphase.sitePositionv.elementAt(i);
    }
    double x[][] = new double[3][siteNumber], xf[] = new double[3];
    if (cellID.length != computeParameterNumber())
      cellID = new int[computeParameterNumber()];

    int[] index1 = new int[3];
    int indexc = 0;
    for (int na = 0; na < aSlices; na++) {
      xf[0] = fi_ra * na;
      for (int nb = 0; nb < bSlices; nb++) {
        xf[1] = fi_rb * nb;
        for (int ng = 0; ng < cSlices; ng++) {
          xf[2] = fi_rc * ng;
          double minDist = 1000.0;
          int index = 0;
          double dist = 0.0;
          for (int i = 0; i < siteNumber; i++) {
            for (int j = 0; j < 3; j++) {
              x[j][i] = sitepos[i].getcoord(j, xf);
              if (x[j][i] < 0)
                x[j][i] += 1.0;
              else if (x[j][i] >= 1.0)
                x[j][i] -= 1.0;
            }
            dist = x[0][i] * x[0][i] + x[1][i] * x[1][i] + x[2][i] * x[2][i];
            if (dist < minDist) {
              minDist = dist;
              index = i;
            } else if (dist == minDist) {
              if (x[0][i] < x[0][index]) {
                minDist = dist;
                index = i;
              } else if (x[0][i] == x[0][index] && x[1][i] < x[1][index]) {
                minDist = dist;
                index = i;
              }
            }
          }
          index1[0] = (int) (x[0][index] * aSlices + 0.999999);
          index1[1] = (int) (x[1][index] * bSlices + 0.999999);
          index1[2] = (int) (x[2][index] * cSlices + 0.999999);
          int new_index = MAPindex(index1);
//          if (new_index != indexc)
//            System.out.println("Setting : " + na + " " + nb + " " + ng + ", "
//              + index1[0] + " " + index1[1] + " " + index1[2] + ", " + indexc + " -> "
// + new_index);
          cellID[indexc++] = new_index; //MAPindex(na, nb, ng);
        }
      }
    }
  }*/

/*  public double getAtomMap(double als, double bets, double gams) {
    int index1, index2, index3;

    if (als < 0)
      als += 1.0;
    else if (als >= 1.0)
      als -= 1.0;
    index1 = (int) (als * aSlices);
    if (bets < 0)
      bets += 1.0;
    else if (bets >= 1.0)
      bets -= 1.0;
    index2 = (int) (bets * bSlices);
    if (gams < 0)
      gams += 1.0;
    else if (gams >= 1.0)
      gams -= 1.0;
    index3 = (int) (gams * cSlices);

    return Math.abs(atomMap[MAPindex(index1, index2, index3)]);
  }*/

/*  public int[] getIndices(double[] coord) {
    int[] index = new int[3];
    for (int i = 0; i < 3; i++) {
      while (coord[i] < 0)
        coord[i] += 1.0;
      while (coord[i] >= 1.0)
        coord[i] -= 1.0;
      index[i] = (int) (coord[i] * nSlices);
    }
    return index;
  }

  public int[] getIndices(double alpha, double beta, double gamma) {
    double[] coord = new double[3];
    coord[0] = alpha;
    coord[1] = beta;
    coord[2] = gamma;
    return getIndices(coord);
  }                           */

  public void loadMapFromFile(Phase aphase) {
    String filename = getFilePar().getDirectory() + Misc.toStringDeleteBlank(getPhase().getPhaseName()) + ".map";
    atomMap = MAPinputStandard(filename,
        getResolutionD_a(), getResolutionD_b(), getResolutionD_c());

    atomMapNormalization();
    mapnotLoaded = false;

  }

  public void resetMAP() {

    atomMapInitialization();

//    MersenneTwisterFast randomizer = new MersenneTwisterFast();

    int aSlices1 = getASlices();
    int bSlices1 = getBSlices();
    int cSlices1 = getCSlices();
    atomMap = new double[computeParameterNumber()];
    int index = 0;
    for (int na = 0; na < aSlices1; na++)
      for (int nb = 0; nb < bSlices1; nb++)
        for (int ng = 0; ng < cSlices1; ng++)
          atomMap[index++] = 1.0f; // (double) randomizer.nextDouble();
//    atomMapNormalization();
    fitNotInitialized = true;

//    atomMapNormalization();
    mapnotLoaded = false;
  }

  public void loadXplorMap() {
    String xplorFilename = getFilePar().getDirectory() + Misc.toStringDeleteBlank(getPhase().getPhaseName()) + ".xplor";
    if (Misc.checkForFile(xplorFilename))
      loadXplorMap(xplorFilename);
  }

  public void loadXplorMap(String xplorFilename) {
    BufferedReader reader = Misc.getReader(xplorFilename);
    if (reader != null) {
      try {
        StringTokenizer st;
        String linedata;

        reader.readLine(); // blank
        reader.readLine(); // always 1
        reader.readLine(); // title

        linedata = reader.readLine();
        st = new StringTokenizer(linedata, "() ,\t\r\n");

        String a_value = st.nextToken();
        st.nextToken();
        st.nextToken();
        String b_value = st.nextToken();
        st.nextToken();
        st.nextToken();
        String c_value = st.nextToken();

        useReducedCell(false);
        setResolution(a_value, b_value, c_value);

        reader.readLine(); // cell parameters
        reader.readLine(); // ZYX

        int aSlices1 = getASlices();
        int bSlices1 = getBSlices();
        int cSlices1 = getCSlices();
        atomMap = new double[computeParameterNumber()];
        int index = 0;
        for (int na = 0; na < aSlices1; na++) {
          reader.readLine(); // na increasing
          for (int nb = 0; nb < bSlices1; nb++)
            for (int ng = 0; ng < cSlices1; ng++) {
              linedata = reader.readLine(); // map value
              st = new StringTokenizer(linedata, "() ,\t\r\n");
              atomMap[index++] = Float.valueOf(st.nextToken());
            }
        }
        System.out.println("Xplor map loaded!");
      } catch (IOException e) {
        System.out.println("Error in loading the xplor map file!");
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
    }
    fitNotInitialized = false;

//    atomMapNormalization();
    mapnotLoaded = false;
  }

  public void shakeMAP() {


    int time = (int) System.currentTimeMillis();  // safe because we're getting low-order bits
    ec.util.MersenneTwisterFast randomizer = new ec.util.MersenneTwisterFast(time);

    int aSlices1 = getASlices();
    int bSlices1 = getBSlices();
    int cSlices1 = getCSlices();
//    atomMap = new double[computeParameterNumber()];
    int index = 0;
    for (int na = 0; na < aSlices1; na++)
      for (int nb = 0; nb < bSlices1; nb++)
        for (int ng = 0; ng < cSlices1; ng++)
          atomMap[index] = atomMap[index++] * (randomizer.nextDouble() * 0.01) + (randomizer.nextDouble() * 0.001);
//    atomMapNormalization();
    fitNotInitialized = true;

    atomMapNormalization();
    mapnotLoaded = false;
  }

  public int getASlices() {
    return aSlices;
  }

  public int getASlices_old() {
    return aSlices + 1;
  }

  public int getBSlices() {
    return bSlices;
  }

  public int getBSlices_old() {
    return bSlices + 1;
  }

  public int getCSlices() {
    return cSlices;
  }

  public int getCSlices_old() {
    return cSlices + 1;
  }

  public void writeCustomObject(BufferedWriter out) {

    if (atomMap == null)
      return;
    int aSlices1 = getASlices();
    int bSlices1 = getBSlices();
    int cSlices1 = getCSlices();

    try {
      out.newLine();
      out.write("#custom_object_" + "atomMap");
      out.newLine();
      out.write(CIFdictionary.loopDecl);
      out.newLine();
      out.write(CIFdictionary.atomMap_values);
      out.newLine();
      int index = 0;
      for (int na = 0; na < aSlices1; na++) {
        for (int nb = 0; nb < bSlices1; nb++) {
          for (int ng = 0; ng < cSlices1; ng++) {
            float value = (float) atomMap[index++];
            out.write(Float.toString(value) + " ");
          }
          out.newLine();
        }
        out.newLine();
      }
      out.newLine();
      out.write("#end_custom_object_" + "atomMap");
      out.newLine();
      out.newLine();
    } catch (IOException ioe) {
      System.out.println("Error in writing the atom Map for " + toXRDcatString());
    }

  }

  public void readCustomObject(CIFtoken ciffile) {
    // to be override by subclasses
    // the default read and do nothing
//    String thecife;
    int tokentype;
//		XRDcat theobj = null;
    boolean endofInput = false;
    int aindex = 0, bindex = 0, gindex = 0;
    int index = -1;
    resetMAP();
    int aSlices1 = getASlices();
    int bSlices1 = getBSlices();
    int cSlices1 = getCSlices();

    try {
      do {
        tokentype = ciffile.nextToken();
        switch (tokentype) {
          case CIFtoken.TT_CIFE:
            // should be the CIF entry for atomMap values
/*							// CIF item
						thecife = new String(ciffile.thestring);
						newtoken = ciffile.nextToken();
//							System.out.println(ciffile.thestring);
						if (FilePar.isValidToken(newtoken)) {
							theobj = setSubordinateField(thecife, ciffile.thestring);
							endofInput = true;
						}
						else {
							ciffile.pushBack();
//								System.out.println("Pushback: " + ciffile.thestring);
						}*/
            break;
          case CIFtoken.TT_LOOP:
            // start the loop for the values here
            aindex = 0;
            bindex = 0;
            gindex = 0;
            break;
          case CIFtoken.TT_NUMBER:
            // index = MAPindex(aindex, bindex, gindex);
            index++;
            if (index < atomMap.length)
              atomMap[index] = (double) ciffile.thevalue;
            fitNotInitialized = false;

            gindex++;
            if (gindex == cSlices1) {
              bindex++;
              gindex = 0;
            }
            if (bindex == bSlices1) {
              aindex++;
              bindex = 0;
            }
            break;
          case CIFtoken.TT_CUSTOM_END:
            // subordinate loop
            endofInput = true;
            break;
          default: {
          }
        }
      } while (tokentype != CIFtoken.TT_EOF && !endofInput);
    } catch (IOException ioe) {
      System.out.println("IO exception in custom object for " + toXRDcatString());
    }

    /*		if (theobj != null)
			theobj.readall(ciffile);*/
  }

  public void MAPoutputStandard(String filename, double[] map, int ares, int bres, int cres) {

    int aSlices1 = getASlices();
    int bSlices1 = getBSlices();
    int cSlices1 = getCSlices();
    BufferedWriter PFwriter = Misc.getWriter(filename);
    if (PFwriter != null) {
      try {

        PFwriter.write(Integer.toString(ares));
        PFwriter.write(" ");
        PFwriter.write(Integer.toString(bres));
        PFwriter.write(" ");
        PFwriter.write(Integer.toString(cres));
        PFwriter.newLine();
        int index = 0;
        for (int na = 0; na < aSlices1; na++) {
          for (int nb = 0; nb < bSlices1; nb++) {
            for (int ng = 0; ng < cSlices1; ng++) {
              float value = (float) map[index++];
              PFwriter.write(Float.toString(value) + " ");
            }
            PFwriter.newLine();
          }
          PFwriter.newLine();
        }
        PFwriter.flush();
        PFwriter.close();
      } catch (IOException io) {
        try {
          PFwriter.flush();
          PFwriter.close();
        } catch (IOException ieo) {
        }
      }
    }
  }

  public double[] MAPinputStandard(String filename, int ares, int bres, int cres) {

    double[] map = new double[getASlices() * getBSlices() * getCSlices()];
    BufferedReader PFreader = Misc.getReader(filename);

    int aSlices1 = getASlices();
    int bSlices1 = getBSlices();
    int cSlices1 = getCSlices();

    if (PFreader != null) {
      try {

        String line = PFreader.readLine();
        StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");
        String token = null;
        int aresr = Integer.valueOf(token = st.nextToken()).intValue();
        int bresr = Integer.valueOf(token = st.nextToken()).intValue();
        int cresr = Integer.valueOf(token = st.nextToken()).intValue();
        if (st.hasMoreTokens()) {
          token = st.nextToken();
        }

        if (aresr == ares && bresr == bres && cresr == cres) {
          line = PFreader.readLine();
          st = new StringTokenizer(line, " ,\t\r\n");
          int index = 0;
          for (int na = 0; na < aSlices1; na++)
            for (int nb = 0; nb < bSlices1; nb++)
              for (int ng = 0; ng < cSlices1; ng++) {
                if (!st.hasMoreTokens()) {
                  do {
                    line = PFreader.readLine();
                    st = new StringTokenizer(line, " ,\t\r\n");
                  } while (!st.hasMoreTokens());
                }
                map[index++] = Float.valueOf(st.nextToken()).floatValue();
                fitNotInitialized = false;
              }
        } else {

          System.out.println("Resolution not corresponding!");

          int index = 0;
          for (int na = 0; na < aSlices1; na++)
            for (int nb = 0; nb < bSlices1; nb++)
              for (int ng = 0; ng < cSlices1; ng++)
                map[index++] = 1.0f;
          fitNotInitialized = true;
        }
        PFreader.close();
      } catch (IOException io) {

        int index = 0;
        for (int na = 0; na < aSlices1; na++)
          for (int nb = 0; nb < bSlices1; nb++)
            for (int ng = 0; ng < cSlices1; ng++)
              map[index++] = 1.0f;
        fitNotInitialized = true;

      }
    } else {
      int index = 0;
      for (int na = 0; na < aSlices1; na++)
        for (int nb = 0; nb < bSlices1; nb++)
          for (int ng = 0; ng < cSlices1; ng++)
            map[index++] = 1.0f;
      fitNotInitialized = true;
    }

    return map;
  }

  double[][] mapToPlot = null;
  double IntensityMin = 0.0f;
  double IntensityMax = 0.0f;
  double xMax, yMax;

  public void plotElectronMap() {
    if (atomMap == null)
      return;
//    if (sliceStart > nSlices)
    int numberOfSections = cSlices;

    int columns = 1;
    int rows = 1;
    while (columns * rows < numberOfSections) {
      if (columns * (aSlices + 1) > rows * (bSlices + 1))
        rows++;
      else
        columns++;
    }
    boolean check = true;
    while (check && columns * rows > numberOfSections) {
      check = false;
      if ((columns - 1) * rows >= numberOfSections) {
        columns--;
        check = true;
      } else if (columns * (rows - 1) >= numberOfSections) {
        rows--;
        check = true;
      }
    }
    int columns1 = columns;
    int rows1 = rows;

    columns = 1;
    rows = 1;
    while (columns * rows < numberOfSections) {
      if ((columns + 1) * (aSlices + 1) > (rows + 1) * (bSlices + 1))
        rows++;
      else
        columns++;
    }
    check = true;
    while (check && columns * rows > numberOfSections) {
      check = false;
      if ((columns - 1) * rows >= numberOfSections) {
        columns--;
        check = true;
      } else if (columns * (rows - 1) >= numberOfSections) {
        rows--;
        check = true;
      }
    }

    int sol1 = Math.abs(columns1 * (aSlices + 1) - rows1 * (bSlices + 1));
    int sol2 = Math.abs(columns * (aSlices + 1) - rows * (bSlices + 1));
    if (sol1 < sol2) {
      columns = columns1;
      rows = rows1;
    }

    xMax = reducedCell[0] * columns;
    yMax = reducedCell[1] * rows;

    int width = columns * (aSlices + 1) - 1;
    int height = rows * (bSlices + 1) - 1;
    int zoom = 1;
/*    while (width * zoom < 1500) {
      zoom++;
    }*/
    mapToPlot = new double[width * zoom][height * zoom];
    width = 0;
    height = 0;
    int row = 0;
    int column = 0;
    IntensityMin = 0.0f;
    IntensityMax = 0.0f;
    boolean sqrtMap = MaudPreferences.getBoolean("electronMap.sqrt", false);
    for (int i = 0; i < numberOfSections; i++) {
      for (int n = 0; n < aSlices * zoom; n++) {
        for (int m = 0; m < bSlices * zoom; m++) {
          if (sqrtMap) {
          mapToPlot[width + n][height + m] = (double) Math.sqrt(atomMap[MAPindex(n / zoom, m / zoom, i)]);
          if (IntensityMax < Math.sqrt(atomMap[MAPindex(n / zoom, m / zoom, i)]))
            IntensityMax = (double) Math.sqrt(atomMap[MAPindex(n / zoom, m / zoom, i)]);
          } else {
            mapToPlot[width + n][height + m] = atomMap[MAPindex(n / zoom, m / zoom, i)];
            if (IntensityMax < atomMap[MAPindex(n / zoom, m / zoom, i)])
              IntensityMax = atomMap[MAPindex(n / zoom, m / zoom, i)];
          }
        }
      }
      column++;
      if (column >= columns) {
        row++;
        column = 0;
      }
      width = column * (aSlices + 1) * zoom;
      height = row * (bSlices + 1) * zoom;
    }
    final String title = "Electron density map for " + getParent().toXRDcatString();
    (new PersistentThread() {
  @Override
      public void executeJob() {
    new ElectronMap2DPlot(new Frame(), mapToPlot, title,
            IntensityMin, IntensityMax, xMax, yMax);
      }
    }).start();
  }

  public void plot3DElectronMap() {
    if (atomMap == null)
      resetMAP();
    int aSlices1 = getASlices();
    int bSlices1 = getBSlices();
    int cSlices1 = getCSlices();
    String title = "Electron density slices map for " + getParent().toXRDcatString();
//    new ThreeDMapPlot(new Frame(), atomMap, title);
    myJFrame mapFrame = new myJFrame(null);
    mapFrame.getContentPane().setLayout(new BorderLayout(Constants.borderInside,
        Constants.borderInside));
    Slices2DPlotPanel sl = new Slices2DPlotPanel(mapFrame, atomMap,
        title, aSlices1, bSlices1, cSlices1);
    mapFrame.getContentPane().add(BorderLayout.CENTER, sl);
    JMenuBar amenubar = new JMenuBar();
    mapFrame.setJMenuBar(amenubar);
//    mapFrame.createDefaultMenuBar();
    amenubar.add(sl.createEditMenu());
    mapFrame.pack();
    mapFrame.setVisible(true);
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

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(firstLoading);

    atomMapInitialization();
    
    forceMapToLowValue = MaudPreferences.getDouble("meemMap.forceLowMapConstant", 1.0);
    weightMAPforce = MaudPreferences.getDouble("meemMap.forceLowMapWeight", 10.0);
//    System.out.println("Normalization");
    atomMapNormalization();
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
//    Phase aphase = (Phase) getParent();
//    aphase.computeReducedCellFactors();
//    for (int i = 0; i < 3; i++)
//      reducedCell[i] = aphase.reducedCellFactor[i];
    JOptionsDialog adialog = new JFMSDPDOptionsD(parent, this);
    return adialog;
  }

  public class JFMSDPDOptionsD extends JOptionsDialog {

    JTextField[] parsTF = null;
//    JTextField initialSliceTF = null;
    JCheckBox[] additionalCB = null;


    public JFMSDPDOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel tfPanel = new JPanel();
      tfPanel.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.NORTH, tfPanel);

      String[] labels = {
          "Cycles number    : ",
          "Iterations number: ",
          "Entropy exponent : ",
          "Map divisions a  : ",
          "Map divisions b  : ",
          "Map divisions c  : "
      };

      int numberFields = labels.length;
      parsTF = new JTextField[numberFields];

      for (int i = 0; i < numberFields; i++) {
        tfPanel.add(new JLabel(labels[i]));
        parsTF[i] = new JTextField(Constants.FLOAT_FIELD);
//        genTF.setToolTipText("Set the number of generations for the Genetic Algorithm");
        tfPanel.add(parsTF[i]);
      }

      String[] cblabels = {
          "Store map weights (faster)",
          "Use reduced cell (faster) "
      };

      int cbnumberFields = cblabels.length;
      additionalCB = new JCheckBox[cbnumberFields];

      for (int i = 0; i < cbnumberFields; i++) {
        additionalCB[i] = new JCheckBox(cblabels[i]);
//        genTF.setToolTipText("Set the number of generations for the Genetic Algorithm");
        tfPanel.add(additionalCB[i]);
      }



      String[] rlabels = {
          "reduced cell a (factor): ", Double.toString(reducedCell[0]),
          "reduced cell b (factor): ", Double.toString(reducedCell[1]),
          "reduced cell c (factor): ", Double.toString(reducedCell[2])
      };
      for (int i = 0; i < rlabels.length; i++)
        tfPanel.add(new JLabel(rlabels[i]));

      tfPanel = new JPanel();
      tfPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(BorderLayout.CENTER, tfPanel);

      /* tfPanel.add(new JLabel("Initial c slice: "));
      initialSliceTF = new JTextField(Constants.FLOAT_FIELD);
      tfPanel.add(initialSliceTF);*/

      JButton jb = new JButton("Reset map");
      tfPanel.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          resetMAP();
        }
      });
      jb.setToolTipText("Press this to reset the electron density map");

      jb = new JButton("Shake map");
      tfPanel.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          shakeMAP();
        }
      });
      jb.setToolTipText("Press this to shake randomly the electron density map");

      jb = new JButton("Plot map");
      tfPanel.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          plotElectronMap();
        }
      });
      jb.setToolTipText("Press this to plot the electron density map");

      jb = new JButton("Single slices map");
      tfPanel.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          plot3DElectronMap();
        }
      });
      jb.setToolTipText("Press this for an electron density map");

      jb = new JButton("Load map (.xplor)");
      tfPanel.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          loadXplorElectronMap();
        }
      });
      jb.setToolTipText("Press this to load an electron density map in xplor format");

      setTitle("MEM Electron Map options panel");
      initParameters(true);
      for (int i = 0; i < cbnumberFields; i++) {
        additionalCB[i].addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent action) {
            retrieveParameters();
            initParameters(false);
          }
        });
      }

      pack();
    }

    private void loadXplorElectronMap() {
      retrieveParameters();
      String filename = Utility.browseFilename(this, "Load xplor density map");
      loadXplorMap(filename);
      updateStringtoDoubleBuffering(false);
      initParameters(false);
    }

    public void initParameters(boolean updateCB) {
      int i;
      for (i = 0; i < parsTF.length; i++)
        parsTF[i].setText(stringField[i]);
      if (updateCB)
        for (int j = 0; j < additionalCB.length; i++, j++)
          additionalCB[j].setSelected(stringField[i].equalsIgnoreCase("true"));
    }

    public void retrieveParameters() {
      int i;
      for (i = 0; i < parsTF.length; i++) {
        if (i == 3) {
          setResolution(parsTF[i].getText(), parsTF[++i].getText(), parsTF[++i].getText());
        } else
          stringField[i] = parsTF[i].getText();
      }
      for (int j = 0; j < additionalCB.length; i++, j++) {
        if (additionalCB[j].isSelected())
          stringField[i] = "true";
        else
          stringField[i] = "false";
      }
      updateStringtoDoubleBuffering(false);
    }

  }
}

