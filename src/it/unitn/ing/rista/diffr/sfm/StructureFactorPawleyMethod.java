/*
 * @(#)StructureFactorPawleyMethod.java created Feb 21, 2010 Caen
 *
 * Copyright (c) 2010 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.diffr.sfm;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.interfaces.ReflectionListDelegate;
import it.unitn.ing.rista.interfaces.Peak;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.JOptionsDialog;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.Vector;

/**
 * The StructureFactorPawleyMethod is a class to extract structure factors by fitting
 * using the Pawley method
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Feb 21, 2010 11:05:24 AM $
 * @since JDK1.1
 */
public class StructureFactorPawleyMethod extends StructureFactorExtractor implements ReflectionListDelegate {

  final static String id = "Pawley method";
  final static String desc = "select this to extract structure factors using Pawley method";

  protected static String[] diclistc = {"_riet_Pawley_method_enabled",
      "_diffrn_refln_index_h", "_diffrn_refln_index_k", "_diffrn_refln_index_l",
      "_diffrn_refln_counts_peak"};
  protected static String[] diclistcrm = {"Pawley method enabled",
      "_diffrn_refln_index_h", "_diffrn_refln_index_k", "_diffrn_refln_index_l",
      "intensity"};
  protected static String[] classlistc = {};

  private int internalIndex = 0;

  public StructureFactorPawleyMethod(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = id;
    IDlabel = id;
    description = desc;
  }

  public StructureFactorPawleyMethod(XRDcat aobj) {
    this(aobj, "structure factor Pawley method");
  }

  public StructureFactorPawleyMethod() {
    identifier = id;
    IDlabel = id;
    description = desc;

  }

  public void initConstant() {
    Nstring = 1;
    Nstringloop = 3;
    Nparameter = 0;
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
    stringField[0] = "true";
//    System.out.println("Setting enabled to true");
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
            notifyParameterChanged(source, Constants.STRUCTURE_FACTOR_CHANGED);
            return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.STRUCTURE_FACTOR_CHANGED);
            return;
          }

      super.notifyParameterChanged(source);
    }
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);
    // check peak list
  }

  public void extractStructureFactors(Sample asample) {

//    if (!getFilePar().isStructureFactorExtractionPermitted())
//      return;

    int numberdataset = asample.activeDatasetsNumber();
    internalIndex = 0;

	  Phase phase = (Phase) getParent();

//    System.out.println("Extracting");
    for (int di = 0; di < numberdataset; di++) {
      DataFileSet dataset = asample.getActiveDataSet(di);
      Vector<Peak> fullpeaklist = dataset.getPeakList();
      int numberofpeaks = dataset.getNumberofPeaks();
		  double[][] structureFactors = dataset.getStructureFactors(phase);
      for (int np = 0; np < numberofpeaks; np++) {
        if (fullpeaklist.elementAt(np).getReflex().getParent() == phase) {
          Reflection reflex = fullpeaklist.elementAt(np).getReflex();
          double[] value = getIntensityForPeak(reflex.getH(), reflex.getK(), reflex.getL(),
		          structureFactors[1][fullpeaklist.elementAt(np).getOrderPosition()]);
          structureFactors[0][fullpeaklist.elementAt(np).getOrderPosition()] =  value[0];
	        structureFactors[1][fullpeaklist.elementAt(np).getOrderPosition()] = value[0];
	        structureFactors[2][fullpeaklist.elementAt(np).getOrderPosition()] = value[1];
        }
      }
    }
  }

  public void reflectionListHasChanged() {
 //   System.out.println("Pawley called: " + getFilePar().isOptimizing() + " " + getFilePar().isComputingDerivate());
    if (getFilePar().isOptimizing() || !isAbilitatetoRefresh)
      return;
	  isAbilitatetoRefresh = false;
	  Constants.STARTING_STRUCTURE_FACTOR = MaudPreferences.getDouble("structure_factors.default_value",
			  Constants.STARTING_STRUCTURE_FACTOR);
	  Constants.MINIMUM_STRUCTURE_FACTOR = MaudPreferences.getDouble("structure_factors.minimum_value_for_extraction",
			  Constants.MINIMUM_STRUCTURE_FACTOR);
	  Phase aPhase = (Phase) getParent();
	  int hklNumber = aPhase.gethklNumber();
	  int hklLocalNumber = parameterloopField[0].size();
	  if (hklLocalNumber == 0) {
		  Sample sample = aPhase.getSample();
		  DataFileSet dataset = sample.getActiveDataSet(0);
		  if (dataset != null && dataset.getStructureFactors(aPhase) != null)
			  for (int i = 0; i < hklNumber; i++) {
				  Reflection reflex = aPhase.getReflex(i);
				  int h = reflex.getH();
				  int k = reflex.getK();
				  int l = reflex.getL();
				  addReflectionAt(h, k, l, i, Math.abs(dataset.getStructureFactors(aPhase)[1][i]));  // todo not from only the first dataset
			  }
	  } else {
		  for (int i = 0; i < hklNumber; i++) {
			  Reflection reflex = aPhase.getReflex(i);
			  int h = reflex.getH();
			  int k = reflex.getK();
			  int l = reflex.getL();
			  forceReflectionAt(h, k, l, i, reflex.isGoodforStructureFactor());
		  }
	  }
	  hklLocalNumber = parameterloopField[0].size();
	  for (int i = hklLocalNumber - 1; i >= hklNumber; i--) {
		  removesPLField(0, i);
		  stringloopField[0].removeItemAt(i);
		  stringloopField[1].removeItemAt(i);
		  stringloopField[2].removeItemAt(i);
	  }
	  if (MaudPreferences.getBoolean("pawley_refinement.doNotRefineOutsideBound", true)) {
		  for (int i = 0; i < hklNumber && i < hklLocalNumber; i++) {
			  Reflection reflex = aPhase.getReflex(i);
			  if (!reflex.isGoodforStructureFactor())
				  ((Parameter) parameterloopField[0].elementAt(i)).setNotRefinable();
			  checkName(((Parameter) parameterloopField[0].elementAt(i)), reflex.getH(), reflex.getK(), reflex.getL());
		  }
	  }
	  isAbilitatetoRefresh = true;
  }

  private void checkName(Parameter parameter, int h, int k, int l) {
    if (!parameter.getLabel().endsWith("_" + Integer.toString(l)))
      parameter.setLabel(getParameterString(0, h, k, l));
  }

  private void addReflectionAt(int h, int k, int l, int i, double value) {
    Parameter par = new Parameter(this, getParameterString(0, h, k, l), value,
        ParameterPreferences.getDouble(getParameterStringOnlyZero(0, i) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterStringOnlyZero(0, i) + ".max", 1.0E9));
    addparameterloopField(0, par);
    if (stringField[0].equalsIgnoreCase("true"))
      par.setRefinable();
    addstringloopField(0, Integer.toString(h));
    addstringloopField(1, Integer.toString(k));
    addstringloopField(2, Integer.toString(l));
  }

  public String getParameterString(int index, int h, int k, int l) {
//    System.out.println(diclist[index + totparameter]);
    return getParameterString(diclist[index + totparameter], "_" + Integer.toString(h) + "_" + Integer.toString(k) +
        "_" + Integer.toString(l));
  }

  private void forceReflectionAt(int h, int k, int l, int i, boolean refine) {
    int hklNumber = parameterloopField[0].size();
//    System.out.println(h + " " + k + " " + l + " " + refine);
    if (i < hklNumber) {
      int hs = Integer.parseInt((String) stringloopField[0].elementAt(i));
      int ks = Integer.parseInt((String) stringloopField[1].elementAt(i));
      int ls = Integer.parseInt((String) stringloopField[2].elementAt(i));
//      System.out.println(hs + " " + ks + " " + ls + " " + i);
      if (hs == h && ks == k && ls == l)
        return;
      boolean found = false;
      for (int j = i + 1; j < hklNumber && !found; j++) {
        hs = Integer.parseInt((String) stringloopField[0].elementAt(j));
        ks = Integer.parseInt((String) stringloopField[1].elementAt(j));
        ls = Integer.parseInt((String) stringloopField[2].elementAt(j));
        if (hs == h && ks == k && ls == l) {
          found = true;
          parameterloopField[0].swapElements(i, j);
          stringloopField[0].swapElements(i, j);
          stringloopField[1].swapElements(i, j);
          stringloopField[2].swapElements(i, j);
        }
      }
      for (int j = 0; j < i && !found; j++) {
        hs = Integer.parseInt((String) stringloopField[0].elementAt(j));
        ks = Integer.parseInt((String) stringloopField[1].elementAt(j));
        ls = Integer.parseInt((String) stringloopField[2].elementAt(j));
        if (hs == h && ks == k && ls == l) {
          found = true;
          parameterloopField[0].swapElements(i, j);
          stringloopField[0].swapElements(i, j);
          stringloopField[1].swapElements(i, j);
          stringloopField[2].swapElements(i, j);
        }
      }
      if (!found) {
        Parameter par = new Parameter(this, getParameterString(0, h, k, l), Constants.STARTING_STRUCTURE_FACTOR,
            ParameterPreferences.getDouble(getParameterStringOnlyZero(0, i) + ".min", 0.0),
            ParameterPreferences.getDouble(getParameterStringOnlyZero(0, i) + ".max", 1.0E9));
        parameterloopField[0].insertItem(par, i);
        if (stringField[0].equalsIgnoreCase("true") && refine)
          par.setRefinable();
        stringloopField[0].insertItem(Integer.toString(h), i);
        stringloopField[1].insertItem(Integer.toString(k), i);
        stringloopField[2].insertItem(Integer.toString(l), i);
      }
    } else {
      Parameter par = new Parameter(this, getParameterString(0, h, k, l), Constants.STARTING_STRUCTURE_FACTOR,
          ParameterPreferences.getDouble(getParameterStringOnlyZero(0, i) + ".min", 0.0),
          ParameterPreferences.getDouble(getParameterStringOnlyZero(0, i) + ".max", 1.0E9));
      parameterloopField[0].addItem(par);
      if (stringField[0].equalsIgnoreCase("true") && refine)
        par.setRefinable();
      stringloopField[0].addItem(Integer.toString(h));
      stringloopField[1].addItem(Integer.toString(k));
      stringloopField[2].addItem(Integer.toString(l));
    }
  }

  private double[] getIntensityForPeak(int h, int k, int l, double afactor) {
    int hklNumber = getParameterLoopVector(0).length;
    double[] factor = new double[2];
    factor[0] = afactor;
    if (internalIndex >= hklNumber)
      internalIndex = 0;
    int index = internalIndex;
    ListVector hList = stringloopField[0];
    ListVector kList = stringloopField[1];
    ListVector lList = stringloopField[2];
    if (index < hklNumber) {
      if (h == Integer.parseInt((String) hList.elementAt(index)) &&
          k == Integer.parseInt((String) kList.elementAt(index)) &&
          l == Integer.parseInt((String) lList.elementAt(index))) {
        internalIndex++;
        factor[0] = Math.abs(getParameterLoopValues(0, index));
        factor[1] = Math.abs(getParameterLoopError(0, index));
      }
    } else {
      index++;
      if (index >= hklNumber)
        index = 0;
      while (index != internalIndex) {
        if (h == Integer.parseInt((String) hList.elementAt(index)) &&
            k == Integer.parseInt((String) kList.elementAt(index)) &&
            l == Integer.parseInt((String) lList.elementAt(index))) {
          internalIndex++;
          factor[0] = Math.abs(getParameterLoopValues(0, index));
          factor[1] = Math.abs(getParameterLoopError(0, index));
          break;
        }
        index++;
        if (index >= hklNumber)
          index = 0;
      }
    }
    if (internalIndex >= hklNumber)
      internalIndex = 0;

//    System.out.println(h + " " + k + " " + l + " " + factor);
//    System.out.println(getFilePar().isComputingDerivate() + " " + getFilePar().isOptimizing());
    return factor;
  }

  private void resetZeroStructureFactors() {
    Constants.STARTING_STRUCTURE_FACTOR = MaudPreferences.getDouble("structure_factors.default_value",
        Constants.STARTING_STRUCTURE_FACTOR);
    Constants.MINIMUM_STRUCTURE_FACTOR = MaudPreferences.getDouble("structure_factors.minimum_value_for_extraction",
        Constants.MINIMUM_STRUCTURE_FACTOR);
    for (int i = 0; i < parameterloopField[0].size(); i++) {
      Parameter par = ((Parameter) parameterloopField[0].elementAt(i));
      if (par.getValueD() < Constants.MINIMUM_STRUCTURE_FACTOR)
        par.setValue(Constants.MINIMUM_STRUCTURE_FACTOR);
    }
  }

  private void resetStructureFactors() {
    Constants.STARTING_STRUCTURE_FACTOR = MaudPreferences.getDouble("structure_factors.default_value",
        Constants.STARTING_STRUCTURE_FACTOR);
    Constants.MINIMUM_STRUCTURE_FACTOR = MaudPreferences.getDouble("structure_factors.minimum_value_for_extraction",
        Constants.MINIMUM_STRUCTURE_FACTOR);
    for (int i = 0; i < parameterloopField[0].size(); i++) {
      ((Parameter) parameterloopField[0].elementAt(i)).setValue(Constants.STARTING_STRUCTURE_FACTOR);
    }
  }

  private void setPawleyEnabled(boolean selected) {
    if (selected) {
      stringField[0] = "true";
      for (int i = 0; i < parameterloopField[0].size(); i++) {
        ((Parameter) parameterloopField[0].elementAt(i)).setRefinable();
      }
	    reflectionListHasChanged();
	    getParent().refreshAll(false);
    } else {
	    if (isPawleyEnabled())
        for (int i = 0; i < parameterloopField[0].size(); i++) {
          ((Parameter) parameterloopField[0].elementAt(i)).setNotRefinable();
        }
	    stringField[0] = "false";
    }
  }

  private boolean isPawleyEnabled() {
    return stringField[0].equalsIgnoreCase("true");
  }

/*  public void loadStructureFactors(String filename) {
    FilePar aparFile = getFilePar();
    Sample asample = aparFile.getActiveSample();
    int realDataSetNumber = asample.datasetsNumber();
    Phase aphase = (Phase) getParent();
    if (filename == null)
      filename = aparFile.getDirectory() + aphase.toXRDcatString() + ".sf";
    int hasValidDataSet = checkForDataSet(filename, aparFile);
    BufferedReader PFreader = Misc.getReader(filename);
    boolean endoffile = false;
    String datasetEntry = CIFdictionary.dataDecl + "dataset_";
    int datasetLength = datasetEntry.length();
    if (PFreader != null) {
      try {
        String line = PFreader.readLine();
        while (!endoffile) {
          if (hasValidDataSet != 0) {
            while (line != null && !line.startsWith(datasetEntry)) {
              line = PFreader.readLine();
            }
          }
          if (line == null)
            endoffile = true;
          else {
// dataset entry
            int datasetindex = -1;

            if (hasValidDataSet > 0) {
              String datasetName = line.substring(datasetLength);
              DataFileSet adataset = asample.getDataSetByName(datasetName);
              if (adataset != null)
                datasetindex = adataset.getIndex();
            } else {
              datasetindex++;
            }
//                System.out.println("dataset " + adataset.toXRDcatString());
            CIFloop aloop = new CIFloop(PFreader);
            aloop.lookForAndReadLoop();
            int cifEntriesNumber = aloop.getNumberOfCIFentries();

            int maxEntries = 10;
            int[] FhklCIFindex = new int[maxEntries];
            for (int i = 0; i < maxEntries; i++)
              FhklCIFindex[i] = -1;
            String[] loopIDs = new String[maxEntries];
            loopIDs[0] = CIFdictionary.refln_h;
            loopIDs[1] = CIFdictionary.refln_k;
            loopIDs[2] = CIFdictionary.refln_l;
            loopIDs[3] = CIFdictionary.refln_multiplicity;
            loopIDs[4] = CIFdictionary.refln_FsquaredMeas;
            loopIDs[5] = CIFdictionary.refln_FsquaredCalc;
            loopIDs[6] = CIFdictionary.refln_FsquaredEsd;
//          loopIDs[6] = CIFdictionary.refln_dspacing;
            loopIDs[7] = CIFdictionary.refln_wavelength;
            loopIDs[8] = CIFdictionary.refln_FMeas;
            loopIDs[9] = CIFdictionary.refln_FCalc;

//              System.out.println(cifEntriesNumber);
            for (int i = 0; i < cifEntriesNumber; i++) {
              String CIFentry = aloop.getCIFentry(i);
//              System.out.println(CIFentry);
              for (int j = 0; j < maxEntries; j++)
                if (CIFentry.equalsIgnoreCase(loopIDs[j])) {
                  FhklCIFindex[j] = i;
//                  System.out.println(j + " " + i);
                }
            }
            boolean meas = (FhklCIFindex[4] != -1);
            boolean calc = (FhklCIFindex[5] != -1);
            boolean meas_notsquared = (FhklCIFindex[8] != -1);
            boolean calc_notsquared = (FhklCIFindex[9] != -1);
            if (datasetindex < realDataSetNumber &&
                ((FhklCIFindex[0] != -1 && FhklCIFindex[1] != -1 && FhklCIFindex[2] != -1) &&
                    (meas || calc || meas_notsquared || calc_notsquared))) { // sufficient information

              int elementsNumber = aloop.getNumberOfCIFelements();

              if (FhklCIFindex[7] != -1 && elementsNumber > 0) {
                if (hasValidDataSet == 0)
                  for (int ij = 0; ij < realDataSetNumber; ij++)
                    wave[ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[7], 0));
                else
                  wave[datasetindex] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[7], 0));
              } else {
                if (hasValidDataSet == 0)
                  for (int ij = 0; ij < realDataSetNumber; ij++)
                    wave[ij] = asample.getDataSet(ij).getMeanRadiationWavelength();
                else
                  wave[datasetindex] = asample.getDataSet(datasetindex).getMeanRadiationWavelength();
              }

// set structure factors
              for (int i = 0; i < elementsNumber && i < dspacing.length; i++) {
                hklm[0][i] = Integer.valueOf(aloop.getCIFelement(FhklCIFindex[0], i));
                hklm[1][i] = Integer.valueOf(aloop.getCIFelement(FhklCIFindex[1], i));
                hklm[2][i] = Integer.valueOf(aloop.getCIFelement(FhklCIFindex[2], i));
                if (meas) {
                  if (hasValidDataSet == 0)
                    for (int ij = 0; ij < realDataSetNumber; ij++)
                      Fhkl[0][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[4], i));
                  else
                    Fhkl[0][i][datasetindex] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[4], i));
                  if (!calc) {
                    if (hasValidDataSet == 0)
                      for (int ij = 0; ij < realDataSetNumber; ij++)
                        Fhkl[1][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[4], i));
                    else
                      Fhkl[1][i][datasetindex] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[4], i));
                  }
                }
                if (calc) {
                  if (hasValidDataSet == 0)
                    for (int ij = 0; ij < realDataSetNumber; ij++)
                      Fhkl[1][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[5], i));
                  else
                    Fhkl[1][i][datasetindex] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[5], i));
                  if (!meas) {
                    if (hasValidDataSet == 0)
                      for (int ij = 0; ij < realDataSetNumber; ij++)
                        Fhkl[0][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[5], i));
                    else
                      Fhkl[0][i][datasetindex] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[5], i));
                  }
                }
                if (meas_notsquared) {
                  if (hasValidDataSet == 0) {
                    for (int ij = 0; ij < realDataSetNumber; ij++) {
                      Fhkl[0][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[8], i));
                      Fhkl[0][i][ij] *= Fhkl[0][i][ij];
                    }
                  } else {
                    Fhkl[0][i][datasetindex] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[8], i));
                    Fhkl[0][i][datasetindex] *= Fhkl[0][i][datasetindex];
                  }
                  if (!calc_notsquared) {
                    if (hasValidDataSet == 0) {
                      for (int ij = 0; ij < realDataSetNumber; ij++) {
                        Fhkl[1][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[8], i));
                        Fhkl[1][i][ij] *= Fhkl[1][i][ij];
                      }
                    } else {
                      Fhkl[1][i][datasetindex] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[8], i));
                      Fhkl[1][i][datasetindex] *= Fhkl[1][i][datasetindex];
                    }
                  }
                }
                if (calc_notsquared) {
                  if (hasValidDataSet == 0) {
                    for (int ij = 0; ij < realDataSetNumber; ij++) {
                      Fhkl[1][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[9], i));
                      Fhkl[1][i][ij] *= Fhkl[1][i][ij];
                    }
                  } else {
                    Fhkl[1][i][datasetindex] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[9], i));
                    Fhkl[1][i][datasetindex] *= Fhkl[1][i][datasetindex];
                  }
                  if (!meas_notsquared) {
                    if (hasValidDataSet == 0) {
                      for (int ij = 0; ij < realDataSetNumber; ij++) {
                        Fhkl[0][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[9], i));
                        Fhkl[0][i][ij] *= Fhkl[0][i][ij];
                      }
                    } else {
                      Fhkl[0][i][datasetindex] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[9], i));
                      Fhkl[0][i][datasetindex] *= Fhkl[0][i][datasetindex];
                    }
                  }
                }
                System.out.println("Loaded " + hklm[0][i] + " " + hklm[1][i] + " " + hklm[2][i] + " " +
                    Fhkl[0][i][datasetindex] + " " + Fhkl[1][i][datasetindex]);
                if (FhklCIFindex[6] != -1) {
                  if (hasValidDataSet == 0)
                    for (int ij = 0; ij < realDataSetNumber; ij++)
                      Fhkl[2][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[6], i));
                  else
                    Fhkl[2][i][datasetindex] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[6], i));
                }
              }
            }
            if (hasValidDataSet == 0)
              datasetindex = asample.datasetsNumber();
            line = aloop.getLastReadedLine();
            aloop = null;
          }
        } //endoffile
        needRestore = true;
        PFreader.close();
      } catch (IOException io) {
      }
      try {
        PFreader.close();
      } catch (IOException io) {
      }
    }
  }*/

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JSFPOptionsD(parent, this);
    return adialog;
  }

  public class JSFPOptionsD extends JOptionsDialog {

    JCheckBox enableCB;

    public JSFPOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      JButton /*jb = new JButton("Import Fhkl from File");
      principalPanel.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
//          retrieveParameters();
          importStructureFactors();
        }
      });
      jb.setToolTipText("Press this to load structure factors in CIF format");

      jb = new JButton("Export Fhkl to File");
      principalPanel.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
//          retrieveParameters();
          exportStructureFactors();
        }
      });
      jb.setToolTipText("Press this to save structure factors in CIF format");
      */

      jb = new JButton("Reset all Fhkl");
      principalPanel.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
//          retrieveParameters();
          resetStructureFactors();
        }
      });
      jb.setToolTipText(
          "Press this to set the small structure factors to a minimum value of structure_factors.minimum_value_for_extraction");

      jb = new JButton("Reset zero Fhkl");
      principalPanel.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
//          retrieveParameters();
          resetZeroStructureFactors();
        }
      });
      jb.setToolTipText(
          "Press this to set the small structure factors to a minimum value of structure_factors.minimum_value_for_extraction");

      enableCB = new JCheckBox("Refine structure factors");
      enableCB.setToolTipText("Uncheck/check the box to disable/enable the refinement of structure factors");
      principalPanel.add(enableCB);
      initParameters();

      setTitle("Structure Factor options panel");
      setHelpFilename("pawley.txt");
      pack();
    }

    public void initParameters() {
      enableCB.setSelected(isPawleyEnabled());
      System.out.println("Getting enabled as:" + isPawleyEnabled());
    }

    public void retrieveParameters() {
      setPawleyEnabled(enableCB.isSelected());
    }

/*    public void exportStructureFactors() {
      String filename = Utility.browseFilenametoSave(this, "Save structure factors in CIF format");
      if (filename != null) {
        Phase aphase = (Phase) StructureFactorPawleyMethod.this.getParent();
        aphase.gethklNumber(); // forcing to build a reflection list
        saveStructureFactors(filename);
      }
    }*/

/*    public void importStructureFactors() {
      String filename = Utility.browseFilename(this, "import structure factors in CIF format");
      if (filename != null) {
        Phase aphase = (Phase) StructureFactorPawleyMethod.this.getParent();
        aphase.gethklNumber(); // forcing to build a reflection list
        loadStructureFactors(filename);
      }
    }*/

  }

}
