/*
 * @(#)FilePar.java created 01/01/1997 Mesiano
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

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.comp.*;
import it.unitn.ing.rista.diffr.detector.XRFDetector;
import it.unitn.ing.rista.interfaces.Function;
import it.unitn.ing.rista.interfaces.basicObj;
import it.unitn.ing.rista.io.cif.CIFItem;
import it.unitn.ing.rista.io.cif.CIFParser;
import it.unitn.ing.rista.io.cif.CIFtoken;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.instrument.DefaultInstrument;
import it.unitn.ing.rista.diffr.instbroad.InstrumentBroadeningPVCaglioti;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.*;
import java.io.*;
import java.net.URL;
import java.util.Vector;

import base64.Base64;

/**
 * The FilePar is a class to manage the root of the analysis.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.47 $, $Date: 2006/12/04 14:30:04 $
 * @since JDK1.1
 */

public class FilePar extends XRDcat implements lFilePar, Function {

  protected static final String[] diclistc = {
    "_audit_creation_date", "_audit_creation_method", "_audit_update_record",
    "_computing_structure_refinement", "_refine_ls_R_factor_all", "_refine_ls_wR_factor_all",
    "_refine_ls_goodness_of_fit_all", "_publ_contact_author_name", "_publ_section_title",
    "_pd_proc_ls_extract_int", "_pd_proc_ls_texture_comp", "_computing_reduce_memory_occ",
    "_pd_proc_ls_theoretical_weight", "_pd_proc_ls_extract_pos", "_pd_proc_ls_strain_comp",
    "_pd_proc_ls_extract_Fhkl", "_pd_proc_ls_Fhkl_comp", "_pd_proc_ls_weight_scheme",
    "_refine_ls_weighting_scheme", "_refine_ls_WSS_factor", "_maud_store_spectra_with_analysis",
    "_riet_remove_phases_under", "_riet_refine_cell_over", "_riet_refine_sizestrain_over",
    "_riet_refine_crystal_structure_over", "_riet_refine_texture_over", "_riet_refine_strain_over",
    "_pd_proc_ls_interpolation_comp", "_maud_analysis_name", "_maud_analysis_prog_number",
		"_maud_store_structure_factors_with_analysis", "_maud_store_texture_factors_with_analysis",

    "_computing_refinement_algorithm",

    "_pd_spec_id"
  };

  protected static final String[] diclistcrm = {
    "_audit_creation_date", "_audit_creation_method", "_audit_update_record",
    "_computing_structure_refinement", "_refine_ls_R_factor_all", "_refine_ls_wR_factor_all",
    "_refine_ls_goodness_of_fit_all", "_publ_contact_author_name", "_publ_section_title",
    "_pd_proc_ls_extract_int", "_pd_proc_ls_texture_comp", "_computing_reduce_memory_occ",
    "_pd_proc_ls_theoretical_weight", "_pd_proc_ls_extract_pos", "_pd_proc_ls_strain_comp",
    "_pd_proc_ls_extract_Fhkl", "_pd_proc_ls_Fhkl_comp", "_pd_proc_ls_weight_scheme",
    "_refine_ls_weighting_scheme", "_refine_ls_WSS_factor", "_maud_store_spectra_with_analysis",
    "_riet_remove_phases_under", "_riet_refine_cell_over", "_riet_refine_sizestrain_over",
    "_riet_refine_crystal_structure_over", "_riet_refine_texture_over", "_riet_refine_strain_over",
    "_pd_proc_ls_interpolation_comp", "name of the analysis file", "progressive number of refinements",
		  "_maud_store_structure_factors_with_analysis", "_maud_store_texture_factors_with_analysis",

    "_computing_refinement_algorithm",

    "_pd_spec_id"
  };

  protected static final String[] classlistcs = {
    "superclass:it.unitn.ing.rista.comp.OptimizationAlgorithm"
  };

  protected static final String[] classlistc = {
    "superclass:it.unitn.ing.rista.diffr.Sample"};

  // Lists string

  public static final String[] listString = {"Sample"};
  public static final String[] database = {"datasets.mdb", "structures.mdb", "samples.mdb", "instruments.mdb"};

	public static String analysisFile = "analysis.last";
	public static String analysisPath = "analysis.path";

	public static int optimizationAlgorithmID = 0;
	public static final int sampleID = 0;

  public ListVector boundlistv;
//	public Vector parametersV;
  public Vector backupPar = null;

  principalJFrame themainframe = null;

//  public static int iterationnumber = MaudPreferences.getInteger(MaudPreferences.iterations);

  public CIFtoken ciffile;

  public launchBasic computation = null;
  public OutputPanel outputframe;
//	private int threadPriority = 4;

//  String thefilename;
  String thefolder = new String(".");
  boolean isOptimizing = false;

  public static int ComputationOptionNumber = 3;

  public static String[] COMP_STATUS = {"never", "end of iteration", "always"};
  public static String[] weights = {"default", "sqrt", "linear", "log10",
      "sqrt, no bkg", "linear, no bkg", "log10, no bkg",
      "sqrt, no bkg(-int)", "linear, no bkg(-int)", "log10, no bkg(-int)",
      "sqrt*q", "linear*q", "log10*q",
      "sqrt*q^2", "linear*q^2", "log10*q^2",
      "sqrt*q^4", "linear*q^4", "log10*q^4",
      "sqrt*q, no bkg", "linear*q, no bkg", "log10*q, no bkg",
      "sqrt*q^2, no bkg", "linear*q^2, no bkg", "log10*q^2, no bkg",
      "sqrt*q^4, no bkg", "linear*q^4, no bkg", "log10*q^4, no bkg"};
  public static String[] minimizeQuantity = {"WgtSS", "Rwp"}; //, "Rwp-bkg"};
/*  public static String[] differenceQuantity = {"default", "sqrt", "linear", "log10",
      "sqrt*q", "linear*q", "log10*q",
      "sqrt*q^2", "linear*q^2", "log10*q^2",
      "sqrt*q^4", "linear*q^4", "log10*q^4"};*/

  // boolean refreshComputation = true;
//	boolean isAbilitatetoRefresh = false;

  public boolean loadingFile = false;
  boolean reduceMemory = false;
  boolean theoreticalWeightingScheme = false;
  public boolean addStatisticalError = false;

  int numberOfData = 0;
  private double parameters[];
	private double[] parameters_multipliers;
  int numberOfParameters = 0;
  boolean refreshFit = true;
  double R = 0.0;
  double Rw = 0.0;
  int actualfit = 0;
  boolean hasBounds[] = null;
  double lbound[] = null;
  double ubound[] = null;
  double dta[] = null;
  double fit[] = null;
  double wgt[] = null;
  boolean computingDerivate = false;
  boolean computingFunction = false;
  boolean saved = false;
  BoundTracker boundList = null;
  public double phaseLimitForRemove = MaudPreferences.getDouble("wizard._riet_remove_phases_under", 0.001);
  public double phaseLimitForCellParameters = MaudPreferences.getDouble("wizard._riet_refine_cell_over", 0.1);
  public double phaseLimitForMicrostructure = MaudPreferences.getDouble("wizard._riet_refine_sizestrain_over", 0.1);
  public double phaseLimitForCrystalStructure = MaudPreferences.getDouble("wizard._riet_refine_crystal_structure_over", 0.1);
  public double phaseLimitForTexture = MaudPreferences.getDouble("wizard._riet_refine_texture_over", 0.15);
  public double phaseLimitForStrain = MaudPreferences.getDouble("wizard._riet_refine_strain_over", 0.25);
  public static int phaseLimitForRemoveID = 21;
  public static int phaseLimitForCellParametersID = 22;
  public static int phaseLimitForMicrostructureID = 23;
  public static int phaseLimitForCrystalStructureID = 24;
  public static int phaseLimitForTextureID = 25;
  public static int phaseLimitForStrainID = 26;
	public static int analysisNameID = 28;
	public static int refinementNumberID = 29;
	public static int saveStructureFactorsID = 30;
	public static int saveTextureFactorsID = 31;

	public FilePar(String name) {
    super(null, name);

    initXRD();
    identifier = new String(name);
    setFileName(name, true);
  }

  public FilePar(String name, principalJFrame aframe) {
    this(name);

    themainframe = aframe;
  }

	public FilePar() {}

  public URL getCodeBase() {
    return null;
  }

  public FilePar getFilePar() {
    return this;
  }

  public void initConstant() {
    Nstring = 32;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 1;
    Nsubordinateloop = 1;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    for (int i = 0; i < totsubordinateloop; i++)
      diclistRealMeaning[i] = diclistcrm[i];
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
  }

  public void initParameters() {
    super.initParameters();

    boundlistv = new ListVector(0, 1, this);
    parametersV = new Vector(0, 1);

    stringField[0] = new String(Misc.getCurrentDateTime());
    stringField[1] = new String("Maud version " + Double.toString(Constants.maud_version));
    stringField[2] = new String("No record");
    stringField[3] = new String("Maud version " + Double.toString(Constants.maud_version));
    stringField[4] = new String("0");
    stringField[5] = new String("0");
    stringField[6] = new String("0");
    stringField[7] = new String("Pinco Pallino");
    stringField[8] = new String("No title");
    stringField[11] = new String("true");
    stringField[12] = new String("false");

	  stringField[analysisNameID] = "untitled";
	  stringField[refinementNumberID] = "0";

	  setTextureFactorsExtractionStatus(1);
    setTextureComputationStatus(1);
    setPositionExtractionStatus(1);
    setStrainComputationStatus(1);
    setMemoryOccupationControl(true);
    setStructureFactorExtractionStatus(1);
    setStructureFactorComputationStatus(1);
    setBackgroundInterpolationStatus(1);
    setWeightingScheme(weights[0]);
    setMinimizeQuantity(minimizeQuantity[0]);
    stringField[19] = new String("0");
    if (MaudPreferences.getBoolean("analysis_default.storeSpectraWithParameters", true))
      stringField[20] = "true";
    else
      stringField[20] = "false";
    stringField[phaseLimitForRemoveID] = Double.toString(phaseLimitForRemove);
    stringField[phaseLimitForCellParametersID] = Double.toString(phaseLimitForCellParameters);
    stringField[phaseLimitForMicrostructureID] = Double.toString(phaseLimitForMicrostructure);
    stringField[phaseLimitForCrystalStructureID] = Double.toString(phaseLimitForCrystalStructure);
    stringField[phaseLimitForTextureID] = Double.toString(phaseLimitForTexture);
    stringField[phaseLimitForStrainID] = Double.toString(phaseLimitForStrain);
	  if (MaudPreferences.getBoolean("analysis_default.storeStructureFactors", true))
		  stringField[saveStructureFactorsID] = "true";
	  else
		  stringField[saveStructureFactorsID] = "false";
	  if (MaudPreferences.getBoolean("analysis_default.storeTextureFactors", true))
		  stringField[saveTextureFactorsID] = "true";
	  else
		  stringField[saveTextureFactorsID] = "false";
    setOptimizationAlgorithm("Marqardt Least Squares");
  }

  public void setLimitsForWizard(double phaseLimitForRemove,
                                 double phaseLimitForCellParameters,
                                 double phaseLimitForMicrostructure,
                                 double phaseLimitForCrystalStructure,
                                 double phaseLimitForTexture,
                                 double phaseLimitForStrain) {
    stringField[phaseLimitForRemoveID] = Double.toString(phaseLimitForRemove);
    stringField[phaseLimitForCellParametersID] = Double.toString(phaseLimitForCellParameters);
    stringField[phaseLimitForMicrostructureID] = Double.toString(phaseLimitForMicrostructure);
    stringField[phaseLimitForCrystalStructureID] = Double.toString(phaseLimitForCrystalStructure);
    stringField[phaseLimitForTextureID] = Double.toString(phaseLimitForTexture);
    stringField[phaseLimitForStrainID] = Double.toString(phaseLimitForStrain);
  }

  public void exportExperimentalComputedData(BufferedWriter output) {
    for (int i = 0; i < samplesNumber(); i++) {
      Sample tmpSample = getSample(i);
      if (tmpSample != null)
        tmpSample.exportExperimentalComputedData(output);
    }
  }

  public void dispose() {
    super.dispose();
    closeLogResultFile();
    if (boundlistv != null)
      boundlistv.dispose();
    if (parametersV != null)
      parametersV.removeAllElements();
    if (backupPar != null)
      backupPar.removeAllElements();
    parametersV = null;
    boundlistv = null;
    backupPar = null;
  }

  public boolean isSaved() {
    return saved;
  }

  protected void finalize() throws Throwable {
//    if (Constants.testing)
//      System.out.println("DEBUG: " + this.toXRDcatString() + " finalizing");

//    int i;

    loadingFile = true;

    dispose();

/*    if (infLglobv != null)
    	for (i = 0; i < infLglobv.length; i++) {
				infLglobv[i].dispose();
    		infLglobv[i] = null;
    	}
		infLglobv = null;*/

    outputframe = null;
    computation = null;
    themainframe = null;
    parameters = null;
	  parameters_multipliers = null;
    hasBounds = null;
    lbound = null;
    ubound = null;
    dta = null;
    fit = null;
    wgt = null;

    loadingFile = false;

    super.finalize();
  }

/*	public void merge(basicObj obj) {
	}

	public boolean isObjectSupported(XRDcat source, ListVector list) {
		if (source instanceof Phase && list.equals(phaselistv))
			return true;
		else if (source instanceof Instrument && list.equals(getInstrumentsList()))
			return true;
		else if (source instanceof Sample && list.equals(getSamplesList()))
			return true;
		else if (source instanceof DataFileSet && list.equals(getDatasetsList()))
			return true;
		return false;
	}

	public boolean isObjectSupported(ListVector list) {
		return false;
	}*/

  public void setDirectory(String folder) {
    thefolder = new String(folder);
//		if (MaudPreferences != null)
    MaudPreferences.setPref(analysisPath, folder);
  }

  public String getDirectory() {
    return thefolder;
  }

  public String getCreatorID() {
    return stringField[1];
  }

  public double getVersion() {
    int length = getCreatorID().length();
    int i = -1;
    char tmp;
    do {
      i++;
      tmp = getCreatorID().charAt(i);
    } while (!Character.isDigit(tmp) && i < length - 1);
    return Double.valueOf(getCreatorID().substring(i, getCreatorID().length())).doubleValue();
  }

	void incrementRefinementNumber() {
		int number = Integer.valueOf(stringField[refinementNumberID]).intValue();
		number++;
		stringField[refinementNumberID] = String.valueOf(number);
	}

	String getIncrementRefinementNumber() {
		return stringField[refinementNumberID];
	}

	public void resetIncrementRefinementNumber() {
		stringField[refinementNumberID] = "0";
	}

  public String getOperatorField() {
    return stringField[7];
  }

  public String getTitleField() {
    return stringField[8];
  }

  public void setOperatorField(String value) {
    stringField[7] = value;
  }

  public void setTitleField(String value) {
    stringField[8] = value;
  }

  public ListVector getSamplesList() {
    return subordinateloopField[sampleID];
  }

  public boolean shouldNotifyParent(XRDcat source, int reason) {
    return false; // there is no parent
  }

  public Object lock = new Object(); // for synchronization

  public void refreshForNotificationUp(XRDcat source, int reason) {
    refreshComputation = true;
    saved = false;
    notifyDownObjectChanged(source, reason);
//    System.out.println("Source " + source.toXRDcatString() + ", Reason " + reason);
    if (reason != Constants.PARAMETER_CHANGED && !Constants.textonly) {
//      synchronized(lock) {
        themainframe.refreshParList(source, reason);
//      }
    }
  }

  public void refreshForNotificationDown(XRDcat source, int reason) {
    refreshComputation = true;
  }

  public void refreshAll(boolean firstLoading) {
  	if (firstLoading) {
  		int checkNumber = 0;
  		while (!Constants.allXrayTablesLoaded && checkNumber < 10) {
		   try {
			   Thread.currentThread().sleep(1000);
		   } catch (InterruptedException ie) {
		   }
	   }
   }
    if (!isLoadingFile() && isAbilitatetoRefresh) {
      refreshComputation = true;
      setRefreshAllStatus();
      update(firstLoading);
      Object[] childrens = getObjectChildren();
      int numberOfChildrens = childrens.length;
      for (int i = 0; i < numberOfChildrens; i++) {
//				System.out.println(childrens[i].toXRDcatString());
        ((XRDcat) childrens[i]).refreshAll(firstLoading);
      }
//      System.out.println("Sample indices");
      refreshSampleIndices();
    }
  }

  public void checkOwnPolynomial() {
    for (int j = 0; j < samplesNumber(); j++) {
      Sample asample = getSample(j);
      for (int i = 0; i < asample.datasetsNumber(); i++)
        asample.getDataSet(i).checkOwnPolynomial();
    }
  }

  public void addMinMaxToParameters() {

  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(firstLoading);
    if (isLoadingFile() || !isAbilitatetoRefresh)
      return;
    if (getMemoryOccupationControl())
      reduceMemory = true;
    else
      reduceMemory = false;
    weightSchemeSwitch = checkWeightingSchemeSwitch();
    minimizingQuantity = checkMinimizeQuantity();
    if (theoreticalWeightingScheme())
      theoreticalWeightingScheme = true;
    else
      theoreticalWeightingScheme = false;
    logOutput = MaudPreferences.getBoolean("log_output.saveInFile", Constants.stdoutput != Constants.NO_OUTPUT);
    phaseLimitForRemove = Double.parseDouble(stringField[phaseLimitForRemoveID]);
    phaseLimitForCellParameters = Double.parseDouble(stringField[phaseLimitForCellParametersID]);
    phaseLimitForMicrostructure = Double.parseDouble(stringField[phaseLimitForMicrostructureID]);
    phaseLimitForCrystalStructure = Double.parseDouble(stringField[phaseLimitForCrystalStructureID]);
    phaseLimitForTexture = Double.parseDouble(stringField[phaseLimitForTextureID]);
    phaseLimitForStrain = Double.parseDouble(stringField[phaseLimitForStrainID]);
  }

  public boolean isLoadingFile() {
    return loadingFile;
  }

  public void setMemoryOccupationControl(boolean control) {
    if (control)
      stringField[11] = new String("true");
    else
      stringField[11] = new String("false");
    updateStringtoDoubleBuffering(false);
  }

  public void setMemoryOccupationControl(String control) {
    stringField[11] = new String(control);
    updateStringtoDoubleBuffering(false);
  }

  public String getMemoryOccupationControlString() {
    return stringField[11];
  }

  public boolean getMemoryOccupationControl() {
    return getMemoryOccupationControlString().equalsIgnoreCase("true");
  }

  public boolean reduceMemory() {
//		return reduceMemory;  // still dangerous
    return false;
  }

  public boolean singleFunctionComputing() {
    if (MaudPreferences.getBoolean("leastSquares.computeExtractionOnWSSCheck", false))
      return true;
    switch (getTextureFactorsExtractionStatusI()) {
      case 2:
      case 1:
        return false;
      case 0:
      default:
        {
        }
    }
    return true;
  }

  int weightSchemeSwitch = 0;

  public void setWeightingScheme(String scheme) {
    if (stringField[17] == null || !stringField[17].equalsIgnoreCase(scheme)) {
      stringField[17] = scheme;
    }
    weightSchemeSwitch = checkWeightingSchemeSwitch();
  }

  public String getWeightingScheme() {
    return stringField[17];
  }

  public int checkWeightingSchemeSwitch() {
    for (int i = 0; i < weights.length; i++)
      if (getWeightingScheme().equalsIgnoreCase(weights[i]))
        return i;
    return 0;
  }

  public int getWeightingSchemeSwitch() {
    return weightSchemeSwitch;
  }

  int minimizingQuantity = 0;

  public void setMinimizeQuantity(String scheme) {
    if (stringField[18] == null || !stringField[18].equalsIgnoreCase(scheme)) {
      stringField[18] = scheme;
    }
    minimizingQuantity = checkMinimizeQuantity();
  }

  public String getMinimizeQuantity() {
    return stringField[18];
  }

  public int checkMinimizeQuantity() {
    for (int i = 0; i < minimizeQuantity.length; i++)
      if (getMinimizeQuantity().equalsIgnoreCase(minimizeQuantity[i]))
        return i;
    return 0;
  }

  public int getMinimizeQuantitySwitch() {
    return minimizingQuantity;
  }

  public void setTheoreticalWeightingScheme(boolean control) {
    if (control)
      stringField[12] = new String("true");
    else
      stringField[12] = new String("false");
  }

  public void setTheoreticalWeightingScheme(String control) {
    stringField[12] = new String(control);
  }

  public String getTheoreticalWeightingSchemeString() {
    return stringField[12];
  }

  public boolean theoreticalWeightingScheme() {
    if (getTheoreticalWeightingSchemeString().equalsIgnoreCase("true"))
      return true;
    else
      return false;
  }

  public boolean theoreticalWeight() {
    return theoreticalWeightingScheme;
  }

  public int samplesNumber() {
    return getSamplesList().size();
  }

  public Sample getSample(int index) {
    if (index >= 0 && index < samplesNumber())
      return (Sample) getSamplesList().elementAt(index);
    else
      return null;
  }

  public int getSample(String id) {
    for (int i = 0; i < samplesNumber(); i++)
      if (id.equals(getSample(i).toXRDcatString()))
        return i;
    return -1;
  }

  static Vector listParametersForBound = null;

  public static void resetListParameters() {
    if (listParametersForBound != null && listParametersForBound.size() > 0)
      listParametersForBound.removeAllElements();
    listParametersForBound = null;
  }

  public void addParameterToList(Parameter par) {
    if (listParametersForBound == null)
      listParametersForBound = new Vector(0, 10);
    checkBoundListForCellPars(par);
    listParametersForBound.addElement(par);
  }

  public void checkBoundListForCellPars(Parameter par) {
    if (par == null)
      return;
    if (getVersion() > 1.69 || listParametersForBound.size() < 1)
//		if (listParametersForBound.size() < 1)
      return;
    int parIndex = getCellParOrder(par.toString());
    Parameter parTmp = (Parameter) listParametersForBound.lastElement();
    if (parTmp == null)
      return;
    int parLastIndex = getCellParOrder(parTmp.toString());
    for (int i = parLastIndex + 1; i < parIndex; i++)
      listParametersForBound.addElement(null);
  }

  public static int getCellParOrder(String label) {
    if (label.equalsIgnoreCase("_cell_length_a"))
      return 1;
    if (label.equalsIgnoreCase("_cell_length_b"))
      return 2;
    if (label.equalsIgnoreCase("_cell_length_c"))
      return 3;
    if (label.equalsIgnoreCase("_cell_length_alpha"))
      return 4;
    if (label.equalsIgnoreCase("_cell_length_beta"))
      return 5;
    if (label.equalsIgnoreCase("_cell_length_gamma"))
      return 6;
    if (label.equalsIgnoreCase("_riet_par_strain_thermal"))
      return 7;
    return 0;
  }

  public static Vector instruments = null;
  public static Vector datasets = null;
  public static Vector phases = null;

  public void initializeAnalysis() {
    Constants.useNewAbsorption = MaudPreferences.getBoolean("absorption.new_model", true);
    MultDiffrDataFile.resetCache();
    resetListParameters();
  }

  public void readall(Reader in, ProgressPanel pcontrol) {
//		String mem = Misc.freeMemory();
//		if (Constants.testing)
//			System.out.println(mem);

//					System.out.println("Start loading");
    loadingFile = true;
    initializeAnalysis();
    boundList = new BoundTracker();
    instruments = new Vector(0, 1);
    datasets = new Vector(0, 1);
    phases = new Vector(0, 1);
    String thecife;
    int newtoken, tokentype;
    XRDcat theobj = null;

    if (in != null) {

      ciffile = new CIFtoken(in);

//					System.out.println("The try loop");
      try {
        do {
          tokentype = ciffile.nextToken();
/*          if (Constants.testing) {
            System.out.println(ciffile.thestring);
            System.out.println(tokentype);
          }*/
          switch (tokentype) {
            case CIFtoken.TT_DATA:
              // new block of data
              break;
            case CIFtoken.TT_GLOB:
              // global data
              if (pcontrol != null) {
                pcontrol.setProgressText("Reading global data");
                pcontrol.increaseValue();
              }

              theobj = this;
              break;
            case CIFtoken.TT_INST:
              // instrumental data

              if (pcontrol != null) {
                pcontrol.setProgressText("Reading instrument data");
                if (instruments.size() < 1)
                  pcontrol.increaseValue();
              }
              if (pcontrol != null) {
                pcontrol.setProgressText("Reading instrument: " + ciffile.thestring);
              }
              instruments.add(new DefaultInstrument(this, ciffile.thestring));
              theobj = (XRDcat) instruments.lastElement();
              break;
            case CIFtoken.TT_DATASET:
              // data

              if (pcontrol != null) {
                pcontrol.setProgressText("Reading data set");
                if (datasets.size() < 1)
                  pcontrol.increaseValue();
              }
              if (pcontrol != null) {
                pcontrol.setProgressText("Reading dataset: " + ciffile.thestring);
              }
              datasets.add(new DataFileSet(this, ciffile.thestring));
              theobj = (XRDcat) datasets.lastElement();
              break;
            case CIFtoken.TT_SAMPLE:
              // sample

              if (pcontrol != null) {
                pcontrol.setProgressText("Reading sample data");
                if (samplesNumber() < 1)
                  pcontrol.increaseValue();
              }

              if (pcontrol != null) {
                pcontrol.setProgressText("Reading sample: " + ciffile.thestring);
              }
              theobj = addSample(new Sample(this, ciffile.thestring));
//							theobj = (XRDcat) getSamplesList().lastElement();
              break;
            case CIFtoken.TT_BOUND:
              // parameter binding

              if (pcontrol != null) {
                pcontrol.setProgressText("Reading parameter binding");
                if (boundlistv.size() < 1)
                  pcontrol.increaseValue();
              }

              boundlistv.addItem(new Bound(this, ciffile.thestring));
              theobj = (XRDcat) boundlistv.lastElement();
              break;
            case CIFtoken.TT_PHASE:
              // phase data

              if (pcontrol != null) {
                pcontrol.setProgressText("Reading phases data");
                if (phases.size() < 1)
                  pcontrol.increaseValue();
              }

              if (pcontrol != null) {
                pcontrol.setProgressText("Reading phase: " + ciffile.thestring);
              }
              phases.add(new Phase(this, ciffile.thestring));
              theobj = (XRDcat) phases.lastElement();
              break;
            case CIFtoken.TT_SUBO:
              // subordinate loop
/*          if (Constants.testing) {
            System.out.println("SUB:" + ciffile.thestring);
            System.out.println(theobj);
          }*/
              if (getVersion() < 2.042) { // to force loading spectra in the old analysis files
                setStoreSpectraOption(false);
              }
              try {
                theobj.readtheobject(ciffile);
              } catch (Exception e) {
                e.printStackTrace();
              }
              break;
            case CIFtoken.TT_LOOP:
              // data loop
              Vector itemlistv = new Vector(0, 1);
              Vector cifelistv = new Vector(0, 1);
              newtoken = ciffile.nextToken();
//							System.out.println(ciffile.thestring);
              while (newtoken == CIFtoken.TT_CIFE) {
                itemlistv.addElement(ciffile.thestring);
                newtoken = ciffile.nextToken();
//								System.out.println(ciffile.thestring);
              }
              int loopitem = itemlistv.size();
              if (loopitem > 0) {
                while (isValidToken(newtoken)) {
                  cifelistv.addElement(new CIFItem((String) itemlistv.elementAt(0),
                      ciffile));
                  for (int i = 1; i < loopitem; i++) {
                    newtoken = ciffile.nextToken();
//										System.out.println(ciffile.thestring);
                    cifelistv.addElement(new CIFItem((String) itemlistv.elementAt(i),
                        ciffile));
                  }
                  newtoken = ciffile.nextToken();
//									System.out.println(ciffile.thestring);
                }
              }
              ciffile.pushBack();
//							System.out.println("Pushback: " + ciffile.thestring);
              if (theobj != null)
                theobj.setLoop(cifelistv, loopitem);
              cifelistv.removeAllElements();
              itemlistv.removeAllElements();
              break;
            case CIFtoken.TT_CIFE:
              // CIF item
              thecife = new String(ciffile.thestring);
              newtoken = ciffile.nextToken();
//							System.out.println(ciffile.thestring);
              if (isValidToken(newtoken))
                theobj.setField(thecife, new CIFItem(thecife, ciffile));
              else {
                ciffile.pushBack();
//								System.out.println("Pushback: " + ciffile.thestring);
              }
              break;
            default:
              {
              }
          }
        } while (tokentype != CIFtoken.TT_EOF);
      } catch (IOException ioe) {
        System.out.println("IO exception in reading analysis file!");
        ioe.printStackTrace();
      }

      try {
        in.close();
      } catch (IOException ioe) {
        System.out.println("IO exception in closing the file");
        ioe.printStackTrace();
      }
    } else {
	    System.out.println("File analysis not opened!!");
    }
//		theframe.loadingFinished();
//	  System.out.println("Start binding");
    if (getVersion() < 2.0) {
      // first Maud version
      getSample(0).getDatasetsList().removeAllItems();
      for (int i = 0; i < datasets.size(); i++) {
        DataFileSet data = (DataFileSet) datasets.elementAt(i);
        data.setParent(getSample(0));
        getSample(0).addsubordinateloopField(1, data);
        getSample(0).checkEnabledDatasets(data);
      }
      getSample(0).oldEnabledDatasets.removeAllElements();
      for (int i = 0; i < phases.size(); i++) {
        Phase aphase = (Phase) phases.elementAt(i);
        aphase.setParent(getSample(0));
        getSample(0).getPhasesList().addItem(aphase);
      }
//      in = Misc.getReader(getDirectory(), getFileName());
//      getSample(0).readOldParFile(in, getDirectory(), getFileName());
    }
    if (instruments != null) {
        instruments.removeAllElements();
        instruments = null;
    }
    if (phases != null) {
        phases.removeAllElements();
        phases = null;
    }
    if (datasets != null) {
        datasets.removeAllElements();
        datasets = null;
    }
    if (getVersion() < 1.70) {
/*      if (it.unitn.ing.rista.MaudText.textonly)
        System.out.println("Warning: old analysis file, parameter bounds ignored for safety!");
      else {
        (new AttentionD("Warning: old analysis file, parameter bounds ignored for safety!", 0)).setVisible(true);
      }*/
    } else
      setBinding();
    boundList.dispose();
    boundList = null;
    loadingFile = false;
    MultDiffrDataFile.resetCache();
    if (getVersion() < 2.06) {
    if (Constants.testing)
                System.out.println("First refresh");
      refreshAll(true);
    if (Constants.testing)
								System.out.println("End refresh");
    }
    loadingFile = true;
    if (getVersion() < 1.86)
      checkMonitorCounts();
//								System.out.println("Monitor checked");
    if (getVersion() < 1.57)
      checkPhiandChiAngles();
//								System.out.println("PhiandChi checked");
    if (getVersion() < 1.69)
      checkAsymmetryTanDep();
//    System.out.println("Asymmetry checked");
    if (getVersion() < 1.94)
      moveAtomsToStructureModel();
//    System.out.println("Move atoms checked");
    if (getVersion() < 1.95)
      addMinMaxToParameters();
//    System.out.println("Min max checked");
    if (getVersion() < 1.96)
      checkCagliotiTanDep();
//    System.out.println("Caglioti checked");
    if (getVersion() < 1.993)
      checkCrystalliteAndMicrostrain();
//		System.out.println("tanDep chk, Start refresh");
    if (getVersion() < 1.9993) {
      if (!Constants.textonly) {

        if (switchToNewAbsorptionModel(new Frame()))
          Constants.useNewAbsorption = true;
        else
          Constants.useNewAbsorption = false;
      } else {
        Constants.useNewAbsorption = true;
      }
    } else if (getVersion() < 2.06) {
      checkIntensityAndCaglioti();
    }
    if (getVersion() < 2.045)
      checkBkgPeak();
    if (getVersion() < 2.3)
      getSample(0).checkForOldPhasesBehaviour();

	  loadingFile = false;
    if (Constants.testing)
      System.out.println("Last refresh");
    refreshAll(true);
	  if (getVersion() < 2.4 && getVersion() > 1.8) {
		  getSample(0).computeRange();
		  for (int i = 0; i < getSample(0).getDatasetsList().size(); i++) {
			  DataFileSet data = getSample(0).getDataSet(i);
			  if (data.isBackgroundInterpolated())
			    data.changeBackgroundInterpolation();
		  }
	  }
	  if (getVersion() < 2.4) {
		  for (int i = 0; i < getSample(0).getDatasetsList().size(); i++) {
			  InstrumentBroadening instrBr = getSample(0).getDataSet(i).getInstrument().getInstrumentBroadening();
			  if (instrBr instanceof InstrumentBroadeningPVCaglioti)
				  ((InstrumentBroadeningPVCaglioti) instrBr).checkCaglioti();
		  }
	  }

	  refreshAll(true);
	  for (int i = 0; i < getSample(0).getDatasetsList().size(); i++) {
		  DataFileSet data = getSample(0).getDataSet(i);

		  data.forceRangeCut();
	  }
	  if (getVersion() < 2.61) {
		  for (int i = 0; i < getSample(0).getDatasetsList().size(); i++) {
			  DataFileSet dataset = getSample(0).getDataSet(i);
			  if (dataset.getInstrument().getDetector() instanceof XRFDetector) {
				  double theta2 = 75.0; // ((XRFDetector) dataset.getInstrument().getDetector()).getThetaDetector();
				  double eta = dataset.getInstrument().getDetector().getEtaDetector(null);
				  for (int j = 0; j < dataset.datafilesnumber(); j++) {
					  dataset.getDataFile(j).setAngleValue(4, theta2 + dataset.getDataFile(j).getAngleValue(0));
					  if (dataset.getDataFile(j).getAngleValue(3) == 45.0)
						  dataset.getDataFile(j).setAngleValue(3, 0);
					  if (getVersion() > 2.56) {
						  DiffrDataFile datafile = dataset.getDataFile(j);
						  double start = datafile.getXDataOriginal(0);
						  System.out.println("Shifting uncalibrated x by " + start);
						  for (int k = 0; k < datafile.twotheta.length; k++) {
							  datafile.setXData(k, datafile.getXDataOriginal(k) - start);
						  }
					  }
				  }
			  }
		  }
	  }
	  checkForVersion(getVersion());

	  stringField[1] = "Maud version " + Constants.maud_version;

	  if (Constants.testing)
		  System.out.println("End refresh");
	  saved = true;
//		System.gc();
  }

  private void checkBkgPeak() {
    for (int j = 0; j < samplesNumber(); j++) {
      Sample asample = getSample(j);
      for (int i = 0; i < asample.datasetsNumber(); i++) {
        DataFileSet adata = asample.getDataSet(i);
        int peaksNumber = adata.backgpeaksnumber();
        for (int k = 0; k < peaksNumber; k++) {
          BkgPeak bpeak = adata.getBkgPeak(k);
          bpeak.checkBkgPeak();
        }
      }
    }
  }

  boolean result;

  public boolean switchToNewAbsorptionModel(Frame aframe) {
    JButton removeButton = new JIconButton("Check.gif", "New model");
    result = false;

    final AttentionD attdlg = new AttentionD(aframe,
            "Do you want to switch to new absorption model? (advised for new refinements)", true, removeButton, true);
    removeButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        result = true;
        attdlg.setVisible(false);
        attdlg.dispose();
      }
    });
    attdlg.setVisible(true);

    while (attdlg.isVisible()) {
      try {
        Thread.sleep(100);
      } catch (InterruptedException ie) {
      }

    }

    return result;
  }

  public void checkMonitorCounts() {
    for (int j = 0; j < samplesNumber(); j++) {
      Sample asample = getSample(j);
      for (int i = 0; i < asample.datasetsNumber(); i++) {
        DataFileSet adata = asample.getDataSet(i);
        for (int ij = 0; ij < adata.datafilesnumber(); ij++) {
          DiffrDataFile adatafile = adata.getDataFile(ij);
          adatafile.setMonitorCounts(1.0);
        }
      }
    }
  }

  public void checkIntensityAndCaglioti() {
    for (int j = 0; j < samplesNumber(); j++) {
      Sample asample = getSample(j);
      for (int i = 0; i < asample.datasetsNumber(); i++) {
        DataFileSet adata = asample.getDataSet(i);
        Instrument ainstrument = adata.getInstrument();
        ainstrument.checkIntensity();
        if (adata.datafilesnumber() > 0) {
          DiffrDataFile adatafile = adata.getDataFile(0);
          if (!adatafile.dspacingbase) {
            InstrumentBroadening instBroad = ainstrument.getInstrumentBroadening();
            if (instBroad instanceof InstrumentBroadeningPVCaglioti)
              ((InstrumentBroadeningPVCaglioti) instBroad).setCagliotiTanDependent(true);
          }
        }
      }
    }
  }

  public void checkPhiandChiAngles() {
    for (int j = 0; j < samplesNumber(); j++) {
      Sample asample = getSample(j);
      for (int i = 0; i < asample.datasetsNumber(); i++) {
        DataFileSet adata = asample.getDataSet(i);
        if (adata.datafilesnumber() > 1) {
          Class aclass = adata.getInstrument().getGeometry().getClass();
          if (!Misc.areClassCompatibles("it.unitn.ing.rista.diffr.geometry.GeometryDubnaSkat",
              aclass) && !Misc.areClassCompatibles("it.unitn.ing.rista.diffr.geometry.GeometryTOFGeneric",
                  aclass))
            for (int ij = 0; ij < adata.datafilesnumber(); ij++) {
              DiffrDataFile adatafile = adata.getDataFile(ij);
              adatafile.setAngleValue(1, adatafile.getAngleValue(1) - 90.0);
              if (Misc.areClassCompatibles("it.unitn.ing.rista.diffr.geometry.GeometryBraggBrentano",
                  aclass))
                adatafile.setAngleValue(2, adatafile.getAngleValue(0) - 90.0);
            }
        }
      }
    }
  }

  public void checkAsymmetryTanDep() {
    for (int j = 0; j < samplesNumber(); j++) {
      Sample asample = getSample(j);
      for (int i = 0; i < asample.datasetsNumber(); i++) {
        DataFileSet adata = asample.getDataSet(i);
        Instrument ainstrument = adata.getInstrument();
        InstrumentBroadening instBroad = ainstrument.getInstrumentBroadening();
        if (instBroad instanceof InstrumentBroadeningPVCaglioti)
          ((InstrumentBroadeningPVCaglioti) instBroad).setAsymmetryTanDependent(true);
      }
    }
  }

  public void checkCagliotiTanDep() {
    for (int j = 0; j < samplesNumber(); j++) {
      Sample asample = getSample(j);
      for (int i = 0; i < asample.datasetsNumber(); i++) {
        DataFileSet adata = asample.getDataSet(i);
        Instrument ainstrument = adata.getInstrument();
        InstrumentBroadening instBroad = ainstrument.getInstrumentBroadening();
        if (instBroad instanceof InstrumentBroadeningPVCaglioti)
          ((InstrumentBroadeningPVCaglioti) instBroad).setCagliotiTanDependent(true);
      }
    }
  }

  public void checkCrystalliteAndMicrostrain() {
    for (int j = 0; j < samplesNumber(); j++) {
      Sample asample = getSample(j);
      for (int i = 0; i < asample.phasesNumber(); i++) {
        Phase aphase = asample.getPhase(i);
        aphase.checkCrystalliteAndMicrostrain();
      }
    }
  }

  public void moveAtomsToStructureModel() {
    for (int j = 0; j < samplesNumber(); j++) {
      Sample asample = getSample(j);
      for (int i = 0; i < asample.phasesNumber(); i++) {
        Phase aphase = asample.getPhase(i);
        aphase.moveAtomsToStructureModel();
      }
    }
  }

  public static boolean isValidToken(int token) {
    return (token == CIFtoken.TT_NUMBER || token == CIFtoken.TT_WORD
        || token == CIFtoken.TT_STRING || token == CIFtoken.TT_QUEST);
  }

  public String getLabel() {
    return toXRDcatString();
  }

  public String toXRDcatString() {
    return getFileName();
  }

  public String getFileName() {
    return stringField[analysisNameID];
  }

  public void setFileName(String thename, boolean reading) {
	  setFileNameDontCloseLog(thename, reading);
    closeLogResultFile();
  }

  public void setFileNameDontCloseLog(String thename, boolean reading) {
//	  thefilename = thename;
	  stringField[analysisNameID] = Misc.getFilenameNoParAndVersionExtension(thename);
/*	  if (!thename.endsWith(".par")) {
		  String message = "Next time you save a .par extension will be added";
		  if (!Constants.textonly) {
			  new AttentionD(null, "Warning: new behaviour!", message, null, false);
		  }
		  System.out.println(message);
	  }*/
	  MaudPreferences.setPref(analysisFile, getNameToSave(reading));
  }

	public void setFileNamePreserveExtension(String thename, boolean reading) {
//	  thefilename = thename;
		stringField[analysisNameID] = thename;
		MaudPreferences.setPref(analysisFile, thename);
	}

	public void setLabel(String alabel) {
  }

  public String getSavedFileAsBase64String() {
    ByteArrayOutputStream stream = new ByteArrayOutputStream();
    BufferedWriter br = new BufferedWriter(new PrintWriter(stream));
    writeall(br);
    return Base64.encodeBytes(stream.toByteArray());
  }

  public void writeall(BufferedWriter out) {
    if (themainframe != null)
      themainframe.retrieveParameters();
    try {
      if (out != null) {
        writeParameters(out);
        out.flush();
        out.close();
      }
    } catch (IOException ioe) {
    }
  }

  public void writeParameters(BufferedWriter out) {

    refreshBoundList();

    refreshProgramInformations();

    super.writeParameters(out);

    saved = true;
  }

  public void createFirstDateRecord() {
	  stringField[1] = new String("Maud, version " + Constants.getVersion());
	  stringField[0] = new String(Misc.getCurrentDateTime());
  }

  public void refreshProgramInformations() {
    stringField[2] = new String(Misc.getCurrentDateTime());
    stringField[3] = new String("Maud, version " + Constants.getVersion());
  }

  public void writeDataField(BufferedWriter out) {
    try {
      out.write("data_global");
      out.newLine();
    } catch (IOException ioe) {
      ioe.printStackTrace();
    }
  }

  public void writeAllFieldsCOD(BufferedWriter out) {
    int i;

    for (i = 0; i < 9; i++)
      writeField(out, diclist[i], stringField[i]);
  }

  public void writeAllSubordinates(BufferedWriter out, String previousDic, String previousObj) {

    for (int i = 0; i < Nsubordinate; i++) {
      XRDcat obj = subordinateField[i];
      if (obj != null)
        writeSubordinate(out, diclist[totparameterloop + i], obj);
    }
    for (int i = 0; i < Nsubordinateloop; i++) {
      if (subordinateloopField[i].size() > 0)
        for (int j = 0; j < subordinateloopField[i].size(); j++) {
          XRDcat obj = (XRDcat) subordinateloopField[i].elementAt(j);
          if (obj != null)
            writeSubordinate_no_subordinate(out, diclist[totsubordinate + i], obj);
        }
    }

  }

  public void writeBounds(BufferedWriter out) {
    // is at the end
    boundList.dispose();
    boundList = null;
//    refreshBoundList();
//    for (int i = 0; i < boundlistv.size(); i++)
//      ((Bound) boundlistv.elementAt(i)).writeParameters(out);
//    boundlistv = null;

  }

  public BoundTracker getBoundTracker() {
    return boundList;
  }

  public void refreshBoundList() {
    boundList = new BoundTracker();
    refreshparametersV();
    for (int i = 0; i < parametersV.size(); i++) {
      Parameter par = (Parameter) parametersV.elementAt(i);
      if (par.getRefparameter() != null)
        boundList.addReferenceParameter(par.getRefparameter());
    }
  }

  public void loadObject(int index, String filename) {
    switch (index) {
/*      case instrumentID:
        loadinstrument(filename);
        break;
      case dataID:
        loaddata(filename);
        break;
      case phaseID:
        loadphase(filename);
        break;*/
      case sampleID:
        loadsample(filename);
        break;
      default:
        {
        }
    }
    return;
  }

  public void removeObject(int index) {
    switch (index) {
/*      case instrumentID:
        removeinstrument();
        break;
      case dataID:
        removedata();
        break;
      case phaseID:
        removephase();
        break;*/
      case sampleID:
        removesample();
        break;
      default:
        {
        }
    }
    return;
  }

  public void newObject(int index) {
    switch (index) {
/*      case instrumentID:
        newinstrument().initializeAsNew();
        break;
      case dataID:
        newdata().initializeAsNew();
        break;
      case phaseID:
        newphase().initializeAsNew();
        break;*/
      case sampleID:
        newsample().initializeAsNew();
        break;
      default:
        {
        }
    }
  }

  public void addObject(int index, XRDcat obj) {
    switch (index) {
/*      case instrumentID:
        addsubordinateloopField(index, obj);
        break;
      case dataID:
        addsubordinateloopField(index, obj);
        break;
      case phaseID:
        addphase((Phase) obj);
        break;*/
      case sampleID:
        addSample((Sample) obj);
        break;
      default:
        {
        }
    }
  }

  public Sample addSample(Sample asample) {
    getSamplesList().addItem(asample);
    return asample;
  }

  public Sample getSelectedSample() {
    Sample object = (Sample) getSamplesList().selectedElement();
    if (object == null && getSamplesList().size() > 0) {
      object = getSample(0);
    }
    return object;
  }

  public Sample newsample() {
    Sample asample = new Sample(this);
    addSample(asample);
    asample.addLayer();
    return asample;
  }

  public void removesample() {
    getSamplesList().removeSelElement();
    if (getSamplesList().isEmpty()) {
    	Sample asample = newsample();
    	setActiveSample(asample);
    	DataFileSet adataset = asample.newData(3);
    	adataset.getInstrument();
    	refreshAll(true);
    }
  }

  public void loadsample(String filename) {
    loadingFile = true;
    CIFParser sampleParser = (new CIFParser(filename, (Frame) themainframe, this, "Sample"));
    Object[] asample = sampleParser.getMainCat();
    if (asample != null) {
      for (int i = 0; i < asample.length; i++) {
        if (asample[i] != null) {
          addSample((Sample) asample[i]);
          ((Sample) asample[i]).fixAllParameters();
        }
      }
      loadingFile = false;
      refreshAll(true);
    }
  }

  public void savesample() {
  }


  // All parameters methods

  public void enlargeMinMax() {
    refreshparametersV();
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      apar.enlargeMinMax();
    }
  }

  public void shrinkMinMax() {
    refreshparametersV();
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      apar.shrinkMinMax();
    }
  }

  public void centerMinMax() {
    refreshparametersV();
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      apar.centerMinMax();
    }
  }

  public void fixAllParameters() {
    refreshparametersV();
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      apar.setNotRefinable();
    }
  }

  public void fixAllParametersPreserveBound() {
    refreshparametersV();
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      apar.setNotRefinableCheckBound();
    }
  }

  public void freeAllParameters() {
    refreshparametersV();
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      apar.setRefinable();
    }
  }

  public void freeAllParametersPreserveBound() {
    refreshparametersV();
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      apar.setRefinableCheckBound();
    }
  }

  public void freeAllBackgroundParameters() {
    Sample obj;
    for (int i = 0; i < samplesNumber(); i++) {
      obj = getSample(i);
      obj.freeAllBackgroundParameters();
    }
  }

  public void freeAllScaleParameters() {
    Sample obj;
    for (int i = 0; i < samplesNumber(); i++) {
      obj = getSample(i);
      obj.freeAllScaleParameters();
    }
  }

  public boolean freeAllBasicParameters() {
    boolean done = false;
    for (int i = 0; i < samplesNumber(); i++) {
      done = getSample(i).freeAllBasicParameters(phaseLimitForCellParameters) ||done;
    }
    boundBFactors();
    return done;
  }

  public void boundBFactors() {
    refreshparametersV();
    boolean notFirstIntheList = false;
    Parameter firstOne = null;
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.toString().startsWith("B factor") || apar.toString().contains("atom_site_B_iso")) {
        if (notFirstIntheList) {
          apar.setEqualToAutomatic(firstOne, "1", "0");
        } else {
          firstOne = apar;
          apar.setRefinableCheckBound();
          notFirstIntheList = true;
        }
      }
    }
  }

  public void boundMonitorsByBank() {
    for (int i = 0; i < samplesNumber(); i++) {
      getSample(i).boundMonitorsByBank();
    }
  }

  public void boundMonitorsByAngles(boolean[] useAngle) {
    for (int i = 0; i < samplesNumber(); i++) {
      getSample(i).boundMonitorsByAngles(useAngle);
    }
  }

  public void freeAllMicroParameters() {
    for (int i = 0; i < samplesNumber(); i++) {
      getSample(i).freeAllMicroParameters(phaseLimitForMicrostructure);
    }
  }

  public void freeAllCrystalParameters() {
    for (int i = 0; i < samplesNumber(); i++) {
      getSample(i).freeAllCrystalParameters(phaseLimitForCrystalStructure);
    }
  }

  public void freeAllTextureParameters() {
    for (int i = 0; i < samplesNumber(); i++) {
      getSample(i).freeAllTextureParameters(phaseLimitForTexture);
    }
  }

  public void fixAllTextureParametersPreserveBound() {
    for (int i = 0; i < samplesNumber(); i++) {
      getSample(i).fixAllTextureParametersPreserveBound();
    }
  }

  public void freeAllStrainParameters() {
    for (int i = 0; i < samplesNumber(); i++) {
      getSample(i).freeAllStrainParameters(phaseLimitForStrain);
    }
  }

  public void fixAllStrainParametersPreserveBound() {
    for (int i = 0; i < samplesNumber(); i++) {
      getSample(i).fixAllStrainParametersPreserveBound();
    }
  }

  public void fixAllBackgroundParametersPreserveBound() {
    for (int i = 0; i < samplesNumber(); i++) {
      getSample(i).fixAllBackgroundParametersPreserveBound();
    }
  }

  public void freeAllCountMonitors() {
    for (int i = 0; i < samplesNumber(); i++) {
      getSample(i).freeAllCountMonitors();
    }
  }

  public void fixAllCountMonitorsPreserveBound() {
    for (int i = 0; i < samplesNumber(); i++) {
      getSample(i).fixAllCountMonitorsPreserveBound();
    }
  }

	public void freeAllLinearAbsorption() {
		for (int i = 0; i < samplesNumber(); i++) {
			Sample asample = getSample(i);
			for (int j = 0; j < asample.datasetsNumber(); j++) {
				DataFileSet adataset = asample.getDataSet(j);
				if (adataset.isEnabled()) {
					for (int k = 0; k < adataset.activedatanumber; k++) {
						DiffrDataFile data = adataset.getActiveDataFile(k);
						data.getParameter(data.absorptionFactorID).setRefinableCheckBound();
					}
				}
			}
		}
	}

	public void fixAllLinearAbsorptionPreserveBound() {
		for (int i = 0; i < samplesNumber(); i++) {
			Sample asample = getSample(i);
			for (int j = 0; j < asample.datasetsNumber(); j++) {
				DataFileSet adataset = asample.getDataSet(j);
				if (adataset.isEnabled()) {
					for (int k = 0; k < adataset.activedatanumber; k++) {
						DiffrDataFile data = adataset.getActiveDataFile(k);
						data.getParameter(data.absorptionFactorID).setNotRefinableCheckBound();
					}
				}
			}
		}
	}

	public void boundAllBankCoefficients() {
    for (int k = 0; k < samplesNumber(); k++) {
      Sample asample = getSample(k);
      for (int j = 0; j < asample.datasetsNumber(); j++) {
        asample.getDataSet(j).getInstrument().boundAllBankCoefficients();
      }
    }
  }

  public void forceBoundAllBankCoefficients() {
    for (int k = 0; k < samplesNumber(); k++) {
      Sample asample = getSample(k);
      for (int j = 0; j < asample.datasetsNumber(); j++) {
        asample.getDataSet(j).getInstrument().forceBoundAllBankCoefficients();
      }
    }
  }

  public void refineAllTOFSFBankCoefficients() {
    for (int k = 0; k < samplesNumber(); k++) {
      Sample asample = getSample(k);
      for (int j = 0; j < asample.datasetsNumber(); j++) {
        asample.getDataSet(j).getInstrument().refineAllTOFSFBankCoefficients();
      }
    }
  }

  public void refineAllZEROBankCoefficients(boolean forceFree) {
    for (int k = 0; k < samplesNumber(); k++) {
      Sample asample = getSample(k);
      for (int j = 0; j < asample.datasetsNumber(); j++) {
        asample.getDataSet(j).freeAllShiftsParameters(forceFree);
      }
    }
  }

  public void refineAllXYSampleDisplacements() {
    for (int k = 0; k < samplesNumber(); k++) {
      Sample asample = getSample(k);
      for (int j = 0; j < asample.datasetsNumber(); j++) {
        asample.getDataSet(j).refineAllXYSampleDisplacements();
      }
    }
  }

  public void setBinding() {
//		refreshparametersV();
    for (int i = 0; i < boundlistv.size(); i++) {
      Bound parbound = (Bound) boundlistv.elementAt(i);
      int boundIndex = parbound.getBound();
      int boundToIndex = parbound.getBoundTo();
      if (boundIndex < listParametersForBound.size()) {
        Parameter par = (Parameter) listParametersForBound.elementAt(boundIndex);
        if (boundToIndex < listParametersForBound.size()) {
          Parameter parto = (Parameter) listParametersForBound.elementAt(boundToIndex);
          if (par != null && parto != null) {
            par.setEqualTo(parto, parbound.getRatio(), parbound.getConstant());
/*if (Constants.testing) {
	System.out.println("Parameter: " + par.toXRDcatString() + " bound to :" + parto.toXRDcatString());
}*/
          }
        } else {
          System.out.println("Error, not in list!! File a bug report to maud@ing.unitn.it");
          System.out.println(par.toString() + " " + boundToIndex + "; max is " +
              listParametersForBound.size());
        }
      } else {
        System.out.println("Error, not in list!! File a bug report to maud@ing.unitn.it");
        System.out.println(boundIndex + "; max is " + listParametersForBound.size());
      }
    }
    boundlistv = null;
    resetListParameters();
    refreshparametersV();
    for (int i = 0; i < parametersV.size(); i++) {
      Parameter par = (Parameter) parametersV.elementAt(i);
      par.checkTempBound(boundList);
    }
  }

  public boolean isActive(XRDcat acat) {
    for (int i = 1; i < samplesNumber(); i++)
      if (acat == getSample(i))
        return false;
    return true;
  }

  public int getParameterIndex(Object par) {
    int index = -1;
    for (int i = 0; i < parametersV.size(); i++) {
      if (par == parametersV.elementAt(i))
        return i;
    }
    return index;
  }

/*  public int getChildCount() {
    return samplesNumber();
  }

  public basicObj[] getChildren() {

    int i, k;

    int numberofObjects = samplesNumber();

    basicObj childrens[] = new basicObj[numberofObjects];

    k = 0;
    for (i = 0; i < samplesNumber(); i++)
      childrens[k++] = (basicObj) getSamplesList().elementAt(i);

    return childrens;
  }

  public Object[] getObjectChildren() {

    int i, k;

    int numberofObjects = samplesNumber();

    Object childrens[] = new Object[numberofObjects];

    k = 0;
    for (i = 0; i < samplesNumber(); i++)
      childrens[k++] = getSamplesList().elementAt(i);

    return childrens;
  }*/

  /**
   * Check the consistency of this object respect to the problem implemented.
   * It's automaticly called before computation by the validate routine.
   * Subclasses of this object could overwrite it to implement specific checking that must be
   * done before starting computation.
   */
  public String checkIntegrity() {
    StringBuffer tmp = new StringBuffer("");
    if (samplesNumber() < 1) {
      tmp.append("(4) Sample missing");
    }

/*    boolean firstTime = false;
    if (!MaudPreferences.isPresent("forcePhaseVolumeFraction.asScaleFactors"))
      firstTime = true;*/
    if (isOptimizing()) {
      if (getTextureFactorsExtractionStatusI() == 2)
        setTextureFactorsExtractionStatus(1);
//        tmp.append("Attention: structure factor extraction set to always! Refinement stopped!\n");
      if (getTextureComputationStatusI() == 2)
        setTextureComputationStatus(1);
//        tmp.append("Attention: texture weigths extraction set to always! Refinement stopped!\n");
      if (getPositionExtractionStatusI() == 2)
        setPositionExtractionStatus(1);
//        tmp.append("Attention: positions extraction set to always! Refinement stopped!\n");
      if (getBackgroundInterpolationStatusI() == 2)
        setBackgroundInterpolationStatus(1);
      if (getStructureFactorComputationStatusI() == 2)
        setStructureFactorComputationStatus(1);
    }
    String result = tmp.toString();
    if (result.equals(""))
      return null;
    else
      return result;
  }

  public String getTextureFactorsExtractionStatus() {
    return stringField[9];
  }

  public int getTextureFactorsExtractionStatusI() {
    for (int i = 0; i < ComputationOptionNumber; i++)
      if (getTextureFactorsExtractionStatus().equals(COMP_STATUS[i]))
        return i;
    return 0;
  }

  public void setTextureFactorsExtractionStatus(int i) {
    setTextureFactorsExtractionStatus(COMP_STATUS[i]);
  }

  public void setTextureFactorsExtractionStatus(String value) {
    stringField[9] = new String(value);
  }

  public String getTextureComputationStatus() {
    return stringField[10];
  }

  public int getTextureComputationStatusI() {
    for (int i = 0; i < ComputationOptionNumber; i++)
      if (getTextureComputationStatus().equals(COMP_STATUS[i]))
        return i;
    return 0;
  }

  public void setTextureComputationStatus(int i) {
    setTextureComputationStatus(COMP_STATUS[i]);
  }

  public void setTextureComputationStatus(String value) {
    stringField[10] = new String(value);
  }

  public boolean isTextureFactorsExtractionPermitted() {
    switch (getTextureFactorsExtractionStatusI()) {
      case 2:
        return true;
      case 1:
        if (!isComputingDerivate() && !computingFunction)
          return true;
      case 0:
      default:
        {
        }
    }
    return false;
  }

  public boolean isTextureComputationPermitted() {
    switch (getTextureComputationStatusI()) {
      case 2:
        return true;
      case 1:
        if (!isComputingDerivate() && !computingFunction)
          return true;
      case 0:
      default:
        {
        }
    }
    return false;
  }

  public String getPositionExtractionStatus() {
    return stringField[13];
  }

  public int getPositionExtractionStatusI() {
    for (int i = 0; i < ComputationOptionNumber; i++)
      if (getPositionExtractionStatus().equals(COMP_STATUS[i]))
        return i;
    return 0;
  }

  public void setPositionExtractionStatus(int i) {
    setPositionExtractionStatus(COMP_STATUS[i]);
  }

  public void setPositionExtractionStatus(String value) {
    stringField[13] = new String(value);
  }

  public String getStrainComputationStatus() {
    return stringField[14];
  }

  public int getStrainComputationStatusI() {
    for (int i = 0; i < ComputationOptionNumber; i++)
      if (getStrainComputationStatus().equals(COMP_STATUS[i]))
        return i;
    return 0;
  }

  public void setStrainComputationStatus(int i) {
    setStrainComputationStatus(COMP_STATUS[i]);
  }

  public void setStrainComputationStatus(String value) {
    stringField[14] = new String(value);
  }

  public boolean isPositionExtractionPermitted() {
    switch (getPositionExtractionStatusI()) {
      case 2:
        return true;
      case 1:
        if (!isComputingDerivate() && !computingFunction)
          return true;
      case 0:
      default:
        {
        }
    }
    return false;
  }

  public boolean isStrainComputationPermitted() {
    switch (getStrainComputationStatusI()) {
      case 2:
        return true;
      case 1:
        if (!isComputingDerivate() && !computingFunction)
          return true;
      case 0:
      default:
        {
        }
    }
    return false;
  }

  public String getStructureFactorExtractionStatus() {
    return stringField[15];
  }

  public int getStructureFactorExtractionStatusI() {
    for (int i = 0; i < ComputationOptionNumber; i++)
      if (getStructureFactorExtractionStatus().equals(COMP_STATUS[i]))
        return i;
    return 0;
  }

  public void setStructureFactorExtractionStatus(int i) {
    setStructureFactorExtractionStatus(COMP_STATUS[i]);
  }

  public void setStructureFactorExtractionStatus(String value) {
    stringField[15] = new String(value);
  }

  public String getBackgroundInterpolationStatus() {
    return stringField[27];
  }

  public int getBackgroundInterpolationStatusI() {
    for (int i = 0; i < ComputationOptionNumber; i++)
      if (getBackgroundInterpolationStatus().equals(COMP_STATUS[i]))
        return i;
    return 0;
  }

  public void setBackgroundInterpolationStatus(int i) {
    setBackgroundInterpolationStatus(COMP_STATUS[i]);
  }

  public void setBackgroundInterpolationStatus(String value) {
    stringField[27] = new String(value);
  }

  public String getStructureFactorComputationStatus() {
    return stringField[16];
  }

  public int getStructureFactorComputationStatusI() {
    for (int i = 0; i < ComputationOptionNumber; i++)
      if (getStructureFactorComputationStatus().equals(COMP_STATUS[i]))
        return i;
    return 0;
  }

  public void setStructureFactorComputationStatus(int i) {
    setStructureFactorComputationStatus(COMP_STATUS[i]);
  }

  public void setStructureFactorComputationStatus(String value) {
    stringField[16] = new String(value);
  }

  public boolean isStructureFactorExtractionPermitted() {
    switch (getStructureFactorExtractionStatusI()) {
      case 2:
        return true;
      case 1:
        if (!isComputingDerivate() && !computingFunction)
          return true;
      case 0:
      default:
        {
        }
    }
    return false;
  }

  public boolean isStructureFactorComputationPermitted() {
    switch (getStructureFactorComputationStatusI()) {
      case 2:
        return true;
      case 1:
        if (!isComputingDerivate() && !computingFunction)
          return true;
      case 0:
      default:
        {
        }
    }
    return false;
  }

  public boolean isBackgroundInterpolationPermitted() {
    switch (getBackgroundInterpolationStatusI()) {
      case 2:
        return true;
      case 1:
        if (!isComputingDerivate() && !computingFunction)
          return true;
      case 0:
      default:
        {
        }
    }
    return false;
  }

/*  _________________________________________________________________
   |                  All computations start here                    |
   |_________________________________________________________________|
*/

  /**
   * Computation starts here
   */

  public void compute(OutputPanel aframe) {
    setDerivate(false);
    setOptimizing(false);
    boolean canGo = validate();
    if (!canGo)
      return;
    outputframe = aframe;
    if (computation != null)
      stopcomputation();
    else {
      computingFunction = true;
      computation = new launchComp(this, outputframe);
      computation.prepare();
      computation.launch();
    }
  }

  public void startingRefine() {
    if (computation != null)
      stoprefinement();
  }

  public void launchrefine(OutputPanel aframe) {
	  boolean canGo = validate();
    if (!canGo) {
      System.out.println("Validation not passed");
      return;
    }
	  outputframe = null;

	  setDerivate(false);
	  setOptimizing(false);
	  computingFunction = true;
	  prepareComputation();
	  mainfunction(false, true);

	  outputframe = aframe;
	  computingFunction = false;
    if (outputframe != null) {
      computation = new launchRefine(this, outputframe);
      computation.prepare();
      computation.launch();
    } else {
      getOptimizationAlgorithm().setOutputFrame(outputframe);
      getOptimizationAlgorithm().solveGeneral(null, this);
      fittingFileOutput();
      endOfComputation();
    }
  }

  public void refineWizard(OutputPanel aframe, int wizardindex) {
//		setNumberofIterations(3);
    boolean canGo = validate();
    if (!canGo) {
      System.out.println("Validation not passed");
      return;
    }
	  setDerivate(false);
	  setOptimizing(false);
	  computingFunction = true;
	  prepareComputation();
	  mainfunction(false, true);

	  outputframe = aframe;
    computingFunction = false;
    computation = new launchRefineWizard(this, outputframe, wizardindex);
    computation.prepare();
    computation.launch();
  }

  public void prepareWizard(OutputPanel aframe, int wizardindex) {
    checkAllPhasesBelow(phaseLimitForRemove);
    switch (wizardindex) {
      case 0:
        fixAllParametersPreserveBound();
        freeAllScaleParameters();
        freeAllBackgroundParameters();
        break;
      case 1:
        fixAllParametersPreserveBound();
        freeAllScaleParameters();
        freeAllBackgroundParameters();
        freeAllBasicParameters();
        break;
      case 2:
        fixAllParametersPreserveBound();
        freeAllScaleParameters();
        freeAllBackgroundParameters();
        freeAllBasicParameters();
        freeAllMicroParameters();
        break;
      case 3:
        fixAllParametersPreserveBound();
        freeAllScaleParameters();
        freeAllBackgroundParameters();
        freeAllBasicParameters();
        freeAllMicroParameters();
        freeAllCrystalParameters();
        break;
      case 4:        // texture
        fixAllParametersPreserveBound();
        freeAllScaleParameters();
        freeAllBackgroundParameters();
        freeAllBasicParameters();
        freeAllMicroParameters();
        freeAllTextureParameters();
        break;
      case 5:        // texture and crystal structure
        fixAllParametersPreserveBound();
        freeAllScaleParameters();
        freeAllBackgroundParameters();
        freeAllBasicParameters();
        freeAllMicroParameters();
        freeAllCrystalParameters();
        freeAllTextureParameters();
        break;
      case 6:        // strain
        fixAllParametersPreserveBound();
        freeAllScaleParameters();
        freeAllBackgroundParameters();
        freeAllBasicParameters();
        freeAllMicroParameters();
        freeAllStrainParameters();
        break;
      case 7:        // strain and crystal structure
        fixAllParametersPreserveBound();
        freeAllScaleParameters();
        freeAllBackgroundParameters();
        freeAllBasicParameters();
        freeAllMicroParameters();
        freeAllCrystalParameters();
        freeAllStrainParameters();
        break;
      case 8:        // texture and strain
        fixAllParametersPreserveBound();
        freeAllScaleParameters();
        freeAllBackgroundParameters();
        freeAllBasicParameters();
        freeAllMicroParameters();
        freeAllTextureParameters();
        freeAllStrainParameters();
        break;
      case 9:        // texture, strain and crystal structure
        fixAllParametersPreserveBound();
        freeAllScaleParameters();
        freeAllBackgroundParameters();
        freeAllBasicParameters();
        freeAllMicroParameters();
        freeAllCrystalParameters();
        freeAllTextureParameters();
        freeAllStrainParameters();
        break;
      case 99:
        fixAllParametersPreserveBound();
        freeAllScaleParameters();
        freeAllBackgroundParameters();
        freeAllTextureParameters();
        break;
      default:
        break;
    }
  }

  public void resetWizard() {
    setTextureFactorsExtractionStatus(COMP_STATUS[1]);
    setTextureComputationStatus(COMP_STATUS[1]);
    setPositionExtractionStatus(COMP_STATUS[1]);
    setStrainComputationStatus(COMP_STATUS[1]);
//		fixAllParametersPreserveBound();
  }

  public void stoprefinement() {
    stopcomputation();
  }

  public void endOfComputation() {
    computation = null;
  }

  public void stopcomputation() {
    JButton okButton = new JButton("Stop old computation");
    final AttentionD attdlg = new AttentionD(getMainFrame(),
        "Are you sure to stop the running computation?", okButton);
    okButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        attdlg.setVisible(false);
        attdlg.dispose();
        computation.interruptComputation();
        computation = null;
        setDerivate(false);
//    Removed also from preferences
//        Constants.speedUp = MaudPreferences.getBoolean(MaudPreferences.speedupComp);
//        compute(outputframe);
      }
    });
    attdlg.setVisible(true);
  }

  public void fittingFileOutput() {
    try {
      finalOutput(System.out);
    } catch (IOException ioe) {
      ioe.printStackTrace();
    }
    boolean phaseOutput = MaudPreferences.getBoolean("fittingFileOutput.backgroundAndPhases", true);
    for (int i = 0; i < samplesNumber(); i++) {
      Sample asample = getSample(i);
      asample.fittingFileOutput(addStatisticalError, phaseOutput);
    }
  }

  Sample activeSample = null;

  public Sample getActiveSample() {
    if (activeSample == null)
      return getSample(0);
    return activeSample;
  }

  public void setActiveSample(Sample asample) {
    activeSample = asample;
  }

  public int getNumberOfSpectra() {
    Sample asample = getActiveSample();
    int totnumber = 0;
    for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
      DataFileSet adataset = asample.getActiveDataSet(i);
      totnumber += adataset.activedatafilesnumber();
    }

    return totnumber;
  }

  public DiffrDataFile getDatafile(int index) {
    int number = 0;
    for (int s = 0; s < samplesNumber(); s++) {
      Sample asample = getSample(s);
      for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
        DataFileSet adataset = asample.getActiveDataSet(i);
        int datafilenumber = adataset.activedatafilesnumber();
        for (int j = 0; j < datafilenumber; j++) {
          if (number == index)
            return adataset.getActiveDataFile(j);
          number++;
        }
      }
    }
    return null;
  }

  public DiffrDataFile[] getAllDatafiles() {
    int numberSpectra = getNumberOfSpectra();
    DiffrDataFile[] allData = new DiffrDataFile[numberSpectra];
    int index = 0;
    for (int s = 0; s < samplesNumber(); s++) {
      Sample asample = getSample(s);
      for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
        DataFileSet adataset = asample.getActiveDataSet(i);
        int datafilenumber = adataset.activedatafilesnumber();
        for (int j = 0; j < datafilenumber; j++) {
          allData[index++] = adataset.getActiveDataFile(j);
        }
      }
    }
    return allData;
  }

  boolean hasoutput = false;

  public void setOutput(boolean hasOutput) {
    hasoutput = hasOutput;
  }

  public void mainfunction(boolean hasoutput, boolean refreshAll) {

    //   boolean refreshPartially = false;  // speedModification
	  Constants.refreshTreePermitted = false;
    logOutput = MaudPreferences.getBoolean("log_output.saveInFile", Constants.stdoutput != Constants.NO_OUTPUT);
    fullResults = MaudPreferences.getBoolean("log_output.fullResults", false);
    setOutput(hasoutput);
    if (Constants.testtime)
      Constants.tmpTime = System.currentTimeMillis();

    if (refreshAll) {
      refreshAll(false);
//b		else
//b			refreshPartially = true;  // speedModification

//    refreshDataIndices();

      for (int i = 0; i < samplesNumber(); i++) {
        Sample asample = getSample(i);
        asample.prepareComputation();
      }
//      System.out.println("Updating phases");
      updateAllPhases();
    }
    indexesComputed = false;
    // to be implemented in subclasses
    for (int i = 0; i < samplesNumber(); i++) {
      if (hasoutput && outputframe != null)
        outputframe.appendnewline("Computing spectra for sample: " + getSample(i).toXRDcatString());
//			else if (Constants.testing)
//				System.out.println("Computing spectra for sample: " + getSample(i).toXRDcatString());
      Sample asample = getSample(i);
      setActiveSample(asample);
//      if (asample.refreshComputation) {
      asample.computeSpectra(hasoutput);
      asample.computeEnergy();
//      }
      asample.closeComputation();
    }
    setActiveSample(null);

/* future
		if (refreshPartially) {
			preparingPartialComputation();
			computingPartial();
			closingPartialComputation();
		} else {
			preparingComputation();
			computingBefore();
			computingAll();
			computingAfter();
			closingComputation();
		}
		*/

    if (hasoutput && outputframe != null) {
      double[] indexes = getRefinementIndexes();
      outputframe.appendnewline("Weighted Sum of Squares (fitting): " + String.valueOf(indexes[8]));
      outputframe.appendnewline("Energy: " + String.valueOf(indexes[18]));
      outputframe.appendnewline("Total: " + String.valueOf(indexes[8] + indexes[18]));
      outputframe.appendnewline("Rwp(%): " + indexes[0] * 100);
      outputframe.appendnewline("Rwpnb(%): " + indexes[1] * 100);
	    outputframe.appendnewline("Rwpnb1(%): " + indexes[2] * 100);
	    outputframe.appendnewline("Rwpnb2(%): " + indexes[3] * 100);
      outputframe.appendnewline("R(%): " + indexes[4] * 100);
      outputframe.appendnewline("Rnb(%): " + indexes[5] * 100);
	    outputframe.appendnewline("Rnb1(%): " + indexes[6] * 100);
	    outputframe.appendnewline("Rnb2(%): " + indexes[7] * 100);
      if (themainframe instanceof DiffractionMainFrame)
        themainframe.updateDataFilePlot(true);
    }
//		else if (Constants.testing)
//			System.out.println("Weighted Sum of Squares: " + String.valueOf(wss));

    if (Constants.testtime)
      System.out.println("Time for computation was: " + (System.currentTimeMillis() - Constants.tmpTime) +
          " millisecs.");
	  Constants.refreshTreePermitted = true;
  }

  public void prepareComputation() {
    setActiveSample(getSample(0));
    getActiveSample().computeRange();
    numberOfData = computeDataNumber();
    logOutput = MaudPreferences.getBoolean("log_output.saveInFile", Constants.stdoutput != Constants.NO_OUTPUT);
    fullResults = MaudPreferences.getBoolean("log_output.fullResults", false);
  }

  public OptimizationAlgorithm getOptimizationAlgorithm() {
    return (OptimizationAlgorithm) subordinateField[optimizationAlgorithmID];
  }

  public void setOptimizationAlgorithm(String value) {
    if (subordinateField[optimizationAlgorithmID] == null || !getOptimizationAlgorithm().identifier.equals(value))
      setsubordinateField(optimizationAlgorithmID, value);
  }

  public void setOptimizationAlgorithm(int number) {
    setOptimizationAlgorithm(getsubordIdentifier(optimizationAlgorithmID, number));
  }

  public void computeBefore() {
    updateAllPhases();
  }

  public void computeAll() {
    indexesComputed = false;
    // to be implemented in subclasses
    for (int i = 0; i < samplesNumber(); i++) {
      if (hasoutput && outputframe != null)
        outputframe.appendnewline("Computing spectra for sample: " + getSample(i).toXRDcatString());
//			else if (Constants.testing)
//				System.out.println("Computing spectra for sample: " + getSample(i).toXRDcatString());
      Sample asample = getSample(i);
      setActiveSample(asample);
//      if (asample.refreshComputation) {
      asample.computeSpectra(hasoutput);
//      }
    }
  }

  public void computeAfter() {
    // to be implemented in subclasses
  }

  public void closeComputation() {
    setActiveSample(null);
  }

  public void preparePartialComputation() {
    // to be implemented in subclasses
  }

  public void computePartial() {
    // to be implemented in subclasses
  }

  public void closePartialComputation() {
    // to be implemented in subclasses
  }

  public void updateAllPhases() {
    for (int s = 0; s < samplesNumber(); s++) {
      Sample asample = getSample(s);
      for (int i = 0; i < asample.phasesNumber(); i++)
        asample.getPhase(i).updateAll();
    }
  }

  private void checkAllPhasesBelow(double phaseLimitForRemove) {
    for (int s = 0; s < samplesNumber(); s++) {
      Sample asample = getSample(s);
      asample.normalizePhaseQuantity(phaseLimitForRemove);
    }
  }

  int totNumberOfDatafiles = 0;

  public int getNumberOfDatafiles() {
    // be sure at list refreshAll(false) was called before to call this
    return totNumberOfDatafiles;
  }

/*  public void refreshDataIndices() {  //todo check why not only the active
    int totnumber = 0;
	  int radNumber = 0;
    for (int s = 0; s < samplesNumber(); s++) {
      Sample asample = getSample(s);
      for (int i = 0; i < asample.phasesNumber(); i++) {
        asample.getPhase(i).refreshIndices(asample);
      }
    }
  }*/

  public void refreshSampleIndices() {
    for (int i = 0; i < samplesNumber(); i++) {
      Sample asample = getSample(i);
      asample.setIndex(i);
    }
//    refreshDataIndices(); Luca_remove 15/03/2002
  }

  public int computeDataNumber() {
    int dnumber = 0;
    for (int i = 0; i < samplesNumber(); i++) {
      Sample asample = getSample(i);
      dnumber += asample.computeDataNumber();
//      System.out.println("data number = " + dnumber);
      dnumber += asample.phasesNumber();
//      System.out.println("data number + energy = " + dnumber);
    }
    return dnumber;
  }

  public void refreshData() {
    int dnumber = 0;
    for (int i = 0; i < samplesNumber(); i++) {
      Sample asample = getSample(i);
      int apnumber = asample.getNumberOfData();
      double adta[] = asample.getData();
      for (int j = 0; j < apnumber; j++)
        dta[dnumber + j] = adta[j];
      for (int j = apnumber; j < apnumber + asample.phasesNumber(); j++)
        dta[dnumber + j] = 0.0f;
      dnumber += apnumber + asample.phasesNumber();
    }

  }

  public void refreshWeight() {
    int dnumber = 0;
//    boolean forceNoWeight = MaudPreferences.getBoolean("leastSquares.noWeights", false);

    double Ncorrection = 1.0;
    for (int i = 0; i < samplesNumber(); i++) {
      Sample asample = getSample(i);
      switch (getMinimizeQuantitySwitch()) {
        case 0:
          Ncorrection = 1.0;
          break;
        case 1:
          Ncorrection = Math.sqrt(asample.getNumberActiveDatafiles());
          break;
      }
      int apnumber = asample.getNumberOfData();
      double adta[] = asample.getWeight();
      for (int j = 0; j < apnumber; j++) {
//        if (forceNoWeight)
//          wgt[dnumber + j] = 1.0f;
//        else
        wgt[dnumber + j] = (double) (adta[j] / Ncorrection);
      }
      for (int j = apnumber; j < apnumber + asample.phasesNumber(); j++)
        wgt[dnumber + j] = (double) asample.getPhase(j - apnumber).getActiveStructureModel().getEnergyWeight();
      dnumber += apnumber + asample.phasesNumber();
    }

  }

  public double[] getFit() {
    int dnumber = 0;

    for (int i = 0; i < samplesNumber(); i++) {
      Sample asample = getSample(i);
      int apnumber = asample.getNumberOfData();
//      System.out.println(apnumber);
      double adta[] = asample.getFit();
      int len1 = adta.length;
      int len2 = fit.length;
      for (int j = 0; j < apnumber; j++) {
        if ((dnumber + j) < 0 || (dnumber + j) >= len2) {
          System.out.println("apnumber " + apnumber + " , fit len " + len2 + " , index req " + (dnumber + j));
        }
        if (j < 0 || j >= len1) {
          System.out.println("apnumber " + apnumber + " , adata len " + len1 + " , index req " + j);
        }
        fit[dnumber + j] = adta[j];
      }
      for (int j = apnumber; j < apnumber + asample.phasesNumber(); j++)
        fit[dnumber + j] = asample.getEnergy(j - apnumber);
      dnumber += apnumber + asample.phasesNumber();
    }
    return fit;
  }

  public void refreshFit(double[] fit, double[] parm, int[] controls) {
  }

  public int computeParameterNumber() {
    int pnumber = 0;

    refreshparametersV();
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
//      System.out.println(apar.toXRDcatString() + " " + apar.getFree() + " " + apar.mayRefines());
      if (apar.getFree() && apar.mayRefines())
        ++pnumber;
    }

    return pnumber;
  }

	// interface Function

  public void setParameters() {
    // from parameters[] to the objects

    int pnumber = 0;
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines()) {
        apar.setValue(parameters[pnumber] * parameters_multipliers[pnumber]);
	      pnumber++;
      }
    }
    refreshFit = true;
  }

  public void setParameter(int index) {
    // from parameters[] to the objects

    int pnumber = -1;
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines()) {
        pnumber++;
        if (pnumber == index)
          apar.setValue(parameters[pnumber] * parameters_multipliers[pnumber]);
      }
    }
    refreshFit = true;
  }

  public void setErrors(double[] errors) {
    // refresh the error value computed

    int pnumber = 0;
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines()) {
        apar.setError(errors[pnumber] * parameters_multipliers[pnumber]);
	      pnumber++;
      }
    }
  }

  public void backupallParameters() {
    // from the objects to parameters[]

//		refreshparametersV();
    backupPar = (Vector) parametersV.clone();
    parameters = getfreeParameters();
  }

  public void restoreParametersValues() {
//		refreshparametersV();
    if (backupPar != null) {
      int totnumberParameters = totParameterNumber();

      for (int i = 0; i < totnumberParameters; i++) {
        Parameter apar = (Parameter) parametersV.elementAt(i);
        Parameter backpar = (Parameter) backupPar.elementAt(i);
        if (!apar.getValue().equals(backpar.getValue()))
          apar.setValue(backpar.getValue());
      }
    }
    refreshFit = true;
  }

  public double[] getfreeParameters() {
    // from the objects to parameters[]

    int pnumber = 0;
    int totnumberParameters = totParameterNumber();
    double[] freeParameters = new double[getNumberOfFreeParameters()];

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines()) {
        freeParameters[pnumber] = apar.getValueD();
	      parameters_multipliers[pnumber] = 1.0;
	      lbound[pnumber] = apar.getValueMinD();
	      ubound[pnumber] = apar.getValueMaxD();
	      while (Math.abs(freeParameters[pnumber]) > Constants.MAX_VALUE_FOR_DERIVATIVE) {
		      freeParameters[pnumber] /= Constants.MAX_VALUE_FOR_DERIVATIVE;
		      parameters_multipliers[pnumber] *= Constants.MAX_VALUE_FOR_DERIVATIVE;
		      lbound[pnumber] /= Constants.MAX_VALUE_FOR_DERIVATIVE;
		      ubound[pnumber] /= Constants.MAX_VALUE_FOR_DERIVATIVE;
	      }
        parameters[pnumber] = freeParameters[pnumber];
	      pnumber++;
      }
    }
    return freeParameters;
  }

  public int getNumberofIterations() {
    return 0; //iterationnumber;
  }

  public int prepareIterationNewModel() {

    refreshFit = true;

//    double range[];

    prepareComputation();

    Rw = 0.0;
    R = 0.0;

    System.out.println("Total number of data points: " + numberOfData);

    numberOfParameters = computeParameterNumber();

    System.out.println("Total number of refinable parameters: " + numberOfParameters);

//    System.out.println("Memory needed (for Least Squares), > " + (numberOfData * numberOfParameters * 8) + " bytes");

    parameters = new double[numberOfParameters];
	  parameters_multipliers = new double[numberOfParameters];
	  lbound = new double[numberOfParameters];
    ubound = new double[numberOfParameters];
    hasBounds = new boolean[numberOfParameters];

    for (int i = 0; i < numberOfParameters; i++)
      hasBounds[i] = false;

    parameters = getfreeParameters();

//for (int i = 0; i < numberOfParameters; i++)
//  System.out.println(parameters[i]);
    return numberOfParameters;
  }

  public int prepareIteration() {

    refreshFit = true;

//    double range[];

    prepareComputation();

    Rw = 0.0;
    R = 0.0;

    System.out.println("Total number of data points: " + numberOfData);

    dta = new double[numberOfData];
    wgt = new double[numberOfData];
    fit = new double[numberOfData];
//    System.out.println("i fit: " + fit + ", "+ numberOfData);

    numberOfParameters = computeParameterNumber();

    System.out.println("Total number of refinable parameters: " + numberOfParameters);

    System.out.println("Memory needed (for Least Squares), > " + (numberOfData * numberOfParameters * 8) + " bytes");

    parameters = new double[numberOfParameters];
	  parameters_multipliers = new double[numberOfParameters];
    lbound = new double[numberOfParameters];
    ubound = new double[numberOfParameters];
    hasBounds = new boolean[numberOfParameters];

    for (int i = 0; i < numberOfParameters; i++)
      hasBounds[i] = false;

    parameters = getfreeParameters();
    refreshData();
    refreshWeight();

    return numberOfParameters;
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
    if (refreshFit) {
      computeFit();
      getFit();
//    System.out.println("r fit: " + fit);
    }
    return fit[index];
  }

  public void setDerivate(boolean value) {
    computingDerivate = value;
  }

  public void setOptimizing(boolean value) {
    isOptimizing = value;
  }

  public boolean isOptimizing() {
    return isOptimizing;
  }

  public boolean isComputingDerivate() {
    return computingDerivate;
  }

  public void finalOutput(OutputStream out) throws IOException {
    double[] indexes = getRefinementIndexes();
    printLine(out, "Refinement final output indices:");
	  printLine(out, "Global Rwp: " + Fmt.format(indexes[0]));
	  printLine(out, "Global Rp: " + Fmt.format(indexes[4]));
	  printLine(out, "Global Rwpnb (no background): " + Fmt.format(indexes[1]));
	  printLine(out, "Global Rwpnb1 (no bkg rescaled): " + Fmt.format(indexes[2]));
	  printLine(out, "Global Rwpnb2 (no bkg rescaled^2): " + Fmt.format(indexes[3]));
	  printLine(out, "Global Rpnb (no background): " + Fmt.format(indexes[5]));
	  printLine(out, "Global Rpnb1 (no bkg rescaled): " + Fmt.format(indexes[6]));
	  printLine(out, "Global Rpnb2 (no bkg rescaled^2): " + Fmt.format(indexes[7]));
	  printLine(out, "Total energy: " + Fmt.format(indexes[18]));
    newLine(out);
    printLine(out, "Refinement final output indices for single samples:");
    out.flush();
    for (int i = 0; i < samplesNumber(); i++) {
      getSample(i).finalOutput(out);
    }
  }

  boolean indexesComputed = false;
  double[] refinementIndexes = new double[19];

  public double[] getRefinementIndexes() {

    if (!indexesComputed) {

      //     double diff, wgt, dta, diff2, wgt2, dta2, dtanb, dtanb2, diffb, diffb2;
      for (int j = 0; j < 19; j++)
        refinementIndexes[j] = 0.0;

      for (int i = 0; i < samplesNumber(); i++) {
        Sample asample = getSample(i);

        double[] refIndex = asample.getRefinementIndexes();
        for (int j = 8; j < 19; j++)
          refinementIndexes[j] += refIndex[j];
      }
	    refinementIndexes[0] = MoreMath.sqrt(refinementIndexes[8] / refinementIndexes[14]);
	    refinementIndexes[1] = MoreMath.sqrt(refinementIndexes[8] / refinementIndexes[15]);
	    refinementIndexes[2] = MoreMath.sqrt(refinementIndexes[10] / refinementIndexes[15]);
	    refinementIndexes[3] = MoreMath.sqrt(refinementIndexes[11] / refinementIndexes[15]);
	    refinementIndexes[4] = refinementIndexes[9] / refinementIndexes[16];
	    refinementIndexes[5] = refinementIndexes[9] / refinementIndexes[17];
	    refinementIndexes[6] = refinementIndexes[12] / refinementIndexes[17];
	    refinementIndexes[7] = refinementIndexes[13] / refinementIndexes[17];

      indexesComputed = true;
    }
    setRw(refinementIndexes[0]);
    setR(refinementIndexes[4]);
    setWSS(refinementIndexes[8]);
    return refinementIndexes;

  }

  public double getWSS() {
    double WSS = refinementIndexes[8];
/*    double diff;

    for (int i = 0; i < numberOfData; i++) {
      diff = (getFit(i) - getData(i)) * getWeight(i);
      WSS += diff * diff;
    }*/

//    double[] indexes = getRefinementIndexes();

//    double WSS = refinementIndexes[4];
    switch (getMinimizeQuantitySwitch()) {
      case 1:
        WSS = refinementIndexes[0]; // so we get the Rwp
        break;
    }

    return WSS;
  }

  public String getRexp() {
    return stringField[6];
  }

  public String getRw() {
    return stringField[5];
  }

  public void setRw(double value) {
    stringField[5] = Fmt.format(value);
  }

  public void setR(double value) {
    stringField[4] = Fmt.format(value);
  }

  public void setRexp(double value) {
    stringField[6] = Fmt.format(value);
  }

  public void setWSS(double value) {
    stringField[19] = Fmt.format(value);
  }

  public double getSS() {
    double SS = 0.0;
    double diff;

    for (int i = 0; i < numberOfData; i++) {
      diff = getFit(i) - getData(i);
      SS += diff * diff;
    }

    return SS;
  }

  public int getNumberOfFreeParameters() {
    return numberOfParameters;
  }

  public double getFreeParameter(int index) {
    return parameters[index];
  }

  public double getParameterMinSignificantValue(int index) {
    Parameter thepar = null;
    int pnumber = 0;
    if (parametersV == null)
      refreshparametersV();
    int totnumberParameters = totParameterNumber();
    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines()) {
        if (index == pnumber++) {
          thepar = apar;
          break;
        }
      }
    }
    return thepar.getMinimumSignificantValue();
  }

  public double getLowerBound(int index) {
    return lbound[index];
  }

  public double getUpperBound(int index) {
    return ubound[index];
  }

/*  public void setFreeParameter(int index, double value) {
    int pnumber = 0;
    if (parametersV == null)
      refreshparametersV();
    int totnumberParameters = totParameterNumber();
    if (parameters != null)
      parameters[index] = value;

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines()) {
        if (index == pnumber++)
        apar.setValue(value);
      }
    }
    refreshFit = true;
  }*/

  public void setFreeParameter(int index, double value) {
    int pnumber = 0;
    if (parametersV == null)
      refreshparametersV();
    int totnumberParameters = totParameterNumber();
    if (parameters != null)
      parameters[index] = value;

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines()) {
        if (index == pnumber++)
          apar.setValue(value * parameters_multipliers[index]);
      }
    }
    refreshFit = true;
  }

  public void setFreeParameters(double[] parm) {
    int pnumber = 0;
    if (parametersV == null)
      refreshparametersV();
    int totnumberParameters = totParameterNumber();
    if (parameters != null)
      for (int i = 0; i < parm.length; i++)
        parameters[i] = parm[i];

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines()) {
        apar.setValue(parm[pnumber] * parameters_multipliers[pnumber]);
	      pnumber++;
      }
    }
    refreshFit = true;
  }

  public void computeFit() {
    setParameters();
    boolean refreshAll = MaudPreferences.getBoolean("testing.computeWithRefresh", false);
    mainfunction(false, refreshAll);
//    getFit();

    refreshFit = false;
  }

  public void computeFirstFit() {
    setParameters();

//		updateAllPhases();

    mainfunction(false, true);

    refreshFit = false;
  }

  public void setBounds(int i, double lowerbound, double upperbound) {
    hasBounds[i] = true;
    lbound[i] = lowerbound;
    ubound[i] = upperbound;
  }

  public boolean checkBound(int j, double parmn) {
    boolean bound = false;
    /*if (hasBounds[j])
      if (parmn < lbound[j] || parmn > ubound[j])
        bound = true;
             */  // for the moment
    return bound;
  }

  int[] divideValue = null;

  public int checkForParRangeToDivide() {
    int total = 1;
    refreshparametersV();
    int totnumberParameters = totParameterNumber();
    divideValue = new int[getNumberOfFreeParameters()];
    int index = 0;
    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines()) {
        double val = Double.parseDouble(apar.getError());
        if (val < -1) {
          divideValue[index] = (int) -val;
        } else
          divideValue[index] = 1;
        total *= divideValue[index++];
      }
    }
    return total;
  }

  public void setParametersAndBounds(double[] nparameters, double[] lBounds, double[] uBounds) {
    refreshparametersV();
    int totnumberParameters = totParameterNumber();
    int index = 0;
    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines()) {
        apar.setValue(nparameters[index] * parameters_multipliers[index]);
        apar.setValueMin(lBounds[index] * parameters_multipliers[index]);
        apar.setValueMax(uBounds[index] * parameters_multipliers[index]);
	      index++;
      }
    }
  }

  public int[] getRangeDivision() {
    return divideValue;
  }

  public void saveparameters() {
    for (int i = 0; i < samplesNumber(); i++)
      getSample(i).normalizePhaseQuantity();
    refreshparametersV();
    parameters = getfreeParameters();
    if (themainframe == null)
      return;
    saveparametersfromComp();
  }

  public void updatePlot() {
	  if (!Constants.textonly)
      themainframe.updateDataFilePlot(false);
  }

  private OutputStream outputResultStream = null;

  public OutputStream getResultStream() {
    if (outputResultStream == null && logOutput()) {
//      System.out.println("Opening log file: " + getDirectory() + ":" + getLstNameToSave());
      outputResultStream = new BufferedOutputStream(Misc.getOutputStream("", getLstNameToSave()));
    }
    return outputResultStream;
  }

  public void closeLogResultFile() {
    if (outputResultStream != null) {
      try {
//        System.out.println("Closing log file");
        outputResultStream.flush();
        outputResultStream.close();
        outputResultStream = null;
      } catch (IOException io) {
        io.printStackTrace();
      }
    } else {
//      System.out.println("Log file already closed !?");
    }
  }

  public void saveparametersfromComp() {
    if (MaudPreferences.getBoolean("analysis.saveAfterIterations", false)) {
	    themainframe.saveFile(false);
	    saved = true;
    }
  }

 	public String getNameToSave(boolean reading) {
		if (!reading && MaudPreferences.getBoolean("analysis.saveAlwaysIncrementalName", false)) {
			incrementRefinementNumber();
		}
		return stringField[analysisNameID]; // + getFileNameVersionString() + ".par";
	}

	public String getLstNameToSave() {
    return getDirectory() + stringField[analysisNameID] + /*getFileNameVersionString() +*/ ".lst";
	}

/*	public String getFileNameVersionString() {
		if (Integer.valueOf(getIncrementRefinementNumber()).intValue() > 0)
			return "_" + getIncrementRefinementNumber() + "v";
		return "";
	}*/

  public void printPreInformations(OutputStream out) throws IOException {

    printLine(out, "Analysis title: " + stringField[8]);
    newLine(out);

    printLine(out, "Refined parameters:");
    newLine(out);

//        int pnumber = 0;
    int totnumberParameters = totParameterNumber();
    int index = -1;
    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines()) {
        index++;
        printString(out, index + " " + apar.toIDString());
        printString(out, " value:");
        printString(out, apar.getValue());
        if (Double.parseDouble(apar.getError()) < 0) {
          printLine(out, " not refinable, cholesky negative diagonal value!");
        } else {
          printString(out, " error:");
          printLine(out, apar.getError());
        }
      }
    }

    newLine(out);
    finalOutput(out);
    newLine(out);
    for (int s = 0; s < samplesNumber(); s++) {
      Sample asample = getSample(s);
      printLine(out, "Sample:" + asample.toXRDcatString());
      newLine(out);
      printLine(out, "Phases:");
      for (int i = 0; i < asample.phasesNumber(); i++) {
        Phase aphase = asample.getPhase(i);
        printLine(out, aphase.toXRDcatString());
        printLine(out, "Density: " + aphase.getDensity());
//        printLine(out, "Qc: " + aphase.getQc());
      }
      newLine(out);
    }
    printLine(out, "             Object tree full informations");
    newLine(out);
    out.flush();
  }

  public void setNumberofIterations(int i) {
    getOptimizationAlgorithm().setIterations(i);
  }

  boolean logOutput = MaudPreferences.getBoolean("log_output.saveInFile", Constants.stdoutput != Constants.NO_OUTPUT);
  boolean fullResults = MaudPreferences.getBoolean("log_output.fullResults", false);

  public boolean logOutput() {
    return !isComputingDerivate() && logOutput;
  }

  public boolean fullResults() {
    return fullResults;
  }

  public void appendResultsTo(String folder, String filename, boolean simpleOutput) {
    try {
      boolean newFile = false;
      if (!Misc.checkForFile(folder+filename))
        newFile = true;
      BufferedWriter out = Misc.getWriterForAppend(folder, filename);
      if (out != null) {
        if (simpleOutput) {
          if (newFile) {
            writeSimpleResultsFirstLine(out);
            out.write(Constants.lineSeparator);
          }
          writeSimpleResults(out);
        } else {
          if (newFile) {
            writeResultsFirstLine(out);
            out.write(Constants.lineSeparator);
          }
          writeResults(out);
        }
        out.write(Constants.lineSeparator);
        out.flush();
        out.close();
      } else {
        System.out.println("Not able to open the file for append");
      }
    } catch (IOException ioe) {
      System.out.println("Unable to save the object " + toXRDcatString());
    }
  }

  public void writeResults(BufferedWriter out) throws IOException {
//    if (automaticOutput) {
      writeObjectResults(out);
//    }
    writeParameterResults(out);
  }

  public void writeObjectResults(BufferedWriter out) throws IOException {
    if (!Constants.textonly)
      themainframe.retrieveParameters();
    out.write(getTitleField());
    out.write("\t");
    double[] indexes = getRefinementIndexes();
    if (indexes[0] == 1.0)
      out.write(stringField[5]);
    else
      out.write(Fmt.format(indexes[0] * 100));
    out.write("\t");
  }
  public void writeResultsFirstLine(BufferedWriter out) throws IOException {
//    if (automaticOutput) {
      writeObjectResultsFirstLine(out);
//    }
    writeParameterResultsFirstLine(out);
  }


  public void writeObjectResultsFirstLine(BufferedWriter out) throws IOException {
    out.write("Title");
    out.write("\t");
    out.write("Rwp(%)");
    out.write("\t");
  }

  public void writeSimpleResults(BufferedWriter out) throws IOException {

    if (!Constants.textonly)
      themainframe.retrieveParameters();
    DiffrDataFile datafile = getDatafile(0);
//    if (datafile != null)
//      out.write(datafile.getLabel());
//    else
      out.write(getTitleField());
    out.write("\t");
    double[] indexes = getRefinementIndexes();
    if (indexes[0] == 1.0)
      out.write(stringField[5]);
    else
      out.write(Fmt.format(indexes[0] * 100));
    out.write("\t");
    for (int s = 0; s < samplesNumber(); s++) {
      Sample asample = getSample(s);
      for (int i = 0; i < asample.phasesNumber(); i++) {
        Phase aphase = asample.getPhase(i);
        out.write(aphase.getPhaseName() + "\t");
        getSample(0).writeSimpleResults(out, i);
        aphase.writeSimpleResults(out);
      }
    }
  }

  public void writeSimpleResultsFirstLine(BufferedWriter out) throws IOException {
    out.write("Title");
    out.write("\t");
    out.write("Rwp(%)");
    out.write("\t");
    for (int s = 0; s < samplesNumber(); s++) {
      Sample asample = getSample(s);
      for (int i = 0; i < asample.phasesNumber(); i++) {
        Phase aphase = asample.getPhase(i);
        out.write("Phase_Name" + "\t");
        getSample(0).writeSimpleResultsFirstLine(out, i);
        aphase.writeSimpleResultsFirstLine(out);
      }
    }
  }

  public void setStoreSpectraOption(boolean value) {
    if (value)
      stringField[20] = "true";
    else
      stringField[20] = "false";
  }

  public boolean storeSpectraWithAnalysis() {
    return stringField[20].equalsIgnoreCase("true");
  }

  public principalJFrame getMainFrame() {
    return themainframe;
  }

  public int getNumberNonZeroPhases() {
    int tot = 0;
    for (int s = 0; s < samplesNumber(); s++) {
      Sample asample = getSample(s);
      for (int i = 0; i < asample.phasesNumber(); i++) {
        if (asample.getPhaseTotalVolumeFraction(i) > 0.0)
          tot++;
      }
    }
    return tot;
  }

  public int getMinorPhase() {
    double minor = 1000.0;
    int index = -1;
    for (int s = 0; s < samplesNumber(); s++) {
      Sample asample = getSample(s);
      for (int i = 0; i < asample.phasesNumber(); i++) {
        double amount = asample.getPhaseTotalVolumeFraction(i);
        if (amount < minor && amount > 0.0) {
          index = i;
          minor = amount;
        }
      }
    }
    return index;
  }

  public double[] getPhaseQuantity(int index) {
    return getSelectedSample().getPhaseTotalQuantity(index);
  }

  public Phase getPhase(int index) {
    return getSelectedSample().getPhase(index);
  }

  public void setZeroPhase(int index) {
    for (int s = 0; s < samplesNumber(); s++) {
      Sample asample = getSample(s);
      for (int i = 0; i < asample.phasesNumber(); i++) {
        if (i == index) {
          System.out.println("Setting zero volume fraction of phase " + asample.getPhase(i).getPhaseName());
          asample.setZeroPhase(i);
        }
      }
    }
  }

  public void multiplyScaleFactorsBy(double totalquantity) {
    int number = 0;
    for (int s = 0; s < samplesNumber(); s++) {
      Sample asample = getSample(s);
      for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
        DataFileSet adataset = asample.getActiveDataSet(i);
        adataset.getInstrument().multiplyScaleFactorBy(totalquantity);
      }
    }
  }

  public void readAll() {
    Reader in = Misc.getReader(getDirectory(), getFileName());
    readall(in, null);
  }

	public void freeParameter(int mRow) {
		//To change body of created methods use File | Settings | File Templates.
	}

	public boolean compactSavingTextureFactors() {
		return stringField[saveTextureFactorsID].equalsIgnoreCase("false");
	}

	public boolean compactSavingStructureFactors() {
		return stringField[saveStructureFactorsID].equalsIgnoreCase("false");
	}

	public void setCompactSavingTextureFactors(boolean value) {
		if (!value)
			stringField[saveTextureFactorsID] = "true";
		else
			stringField[saveTextureFactorsID] = "false";
	}

	public void setCompactSavingStructureFactors(boolean value) {
		if (!value)
			stringField[saveStructureFactorsID] = "true";
		else
			stringField[saveStructureFactorsID] = "false";
	}

}

