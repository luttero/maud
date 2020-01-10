/*
 * @(#)Phase.java created 1/1/1997 xx
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.jsginfo.*;
import it.unitn.ing.rista.awt.PhaseD;
import it.unitn.ing.rista.awt.Utility;
import it.unitn.ing.rista.io.DicVol91Result;
import it.unitn.ing.rista.io.cif.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.diffr.sizestrain.SizeStrainSymIso;
import it.unitn.ing.rista.chemistry.AtomInfo;

import java.awt.*;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.util.*;

//import com.sun.java.util.collections.*;

/**
 * The Phase is a class to perform all computation regarding phase characteristics and
 * to store the relevant parameters.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.52 $, $Date: 2006/12/04 14:30:04 $
 * @since JDK1.1
 */


public class Phase extends XRDcat {

  protected static String[] diclistc = {
      "_chemical_name_common", "_chemical_formula_sum", "_symmetry_cell_setting",
      "_symmetry_Int_Tables_number", "_symmetry_space_group_name_sch",
      "_symmetry_space_group_name_H-M", "_symmetry_space_group_name_Hall",
      "_cell_formula_units_Z", "_refine_ls_d_res_low", "_refine_ls_d_res_high",
      "_reflns_d_resolution_low", "_reflns_d_resolution_high",
      "_maud_sg_centering_type", "_chemical_name_mineral", "_chemical_name_systematic",

		  "_space_group_symop_operation_xyz",


      "_cell_length_a", "_cell_length_b", "_cell_length_c",
      "_cell_angle_alpha", "_cell_angle_beta", "_cell_angle_gamma",
      "_riet_par_strain_thermal", "_exptl_absorpt_cryst_size",
      "_riet_par_phase_scale_factor",

      "_pd_proc_ls_pref_orient_corr",
      "_riet_size_strain_model", "_riet_sizestrain_sym_model",
      "_riet_antiphase_boundary", "_riet_planar_defect",
      "_riet_magnetic_structure_model", "_riet_par_strain_model",
//                "_riet_par_stress_model",
      "_riet_micro_absorption_model", "_riet_structure_model", "_riet_structure_factor_model",
      "_riet_structure_factor_extractor", /*"_riet_structure_solution_method",*/

      "_riet_tds_model",

      "_atom_site_label", "_diffrn_refln_id"
  };
  protected static String[] diclistcrm = {
      "_chemical_name_common", "_chemical_formula_sum", "_symmetry_cell_setting",
      "_symmetry_Int_Tables_number", "_symmetry_space_group_name_sch",
      "_symmetry_space_group_name_H-M", "_symmetry_space_group_name_Hall",
      "_cell_formula_units_Z", "_refine_ls_d_res_low", "_refine_ls_d_res_high",
      "_reflns_d_resolution_low", "_reflns_d_resolution_high",
      "_maud_sg_centering_type", "_chemical_name_mineral", "_chemical_name_systematic",

		  "_space_group_symop_operation_xyz",


      "a (Angstrom)", "b (Angstrom)", "c (Angstrom)",
      "alpha (deg)", "beta (deg)", "gamma (deg)",
      "thermal strain (not active)", "grain size (microabs/dynamical F, angstrom)",
      "normalization factor",

      "_pd_proc_ls_pref_orient_corr",
      "_riet_size_strain_model", "_riet_sizestrain_sym_model",
      "_riet_antiphase_boundary", "_riet_planar_defect",
      "_riet_magnetic_structure_model", "_riet_par_strain_model",
//                "_riet_par_stress_model",
      "_riet_micro_absorption_model", "_riet_structure_model", "_riet_structure_factor_model",
      "_riet_structure_factor_extractor", /*"_riet_structure_solution_method",*/

      "_riet_tds_model",

      "_atom_site_label", "_diffrn_refln_id"
  };

  protected static String[] classlistc = {"superclass:it.unitn.ing.rista.diffr.AtomSite",
      "superclass:it.unitn.ing.rista.diffr.Reflex"};

  protected static String[] classlistcs = {
      "superclass:it.unitn.ing.rista.diffr.Texture",
      "superclass:it.unitn.ing.rista.diffr.SizeStrainModel",
      "superclass:it.unitn.ing.rista.diffr.SizeStrainSymModel",
      "superclass:it.unitn.ing.rista.diffr.AntiphaseBoundary",
      "superclass:it.unitn.ing.rista.diffr.PlanarDefects",
      "superclass:it.unitn.ing.rista.diffr.MagneticStructure",
      "superclass:it.unitn.ing.rista.diffr.Strain",
//                "superclass:it.unitn.ing.rista.diffr.Stress",
      "superclass:it.unitn.ing.rista.diffr.MicroAbsorption",
      "superclass:it.unitn.ing.rista.diffr.StructureModel",
      "superclass:it.unitn.ing.rista.diffr.StructureFactorModel",
      "superclass:it.unitn.ing.rista.diffr.StructureFactorExtractor",
//      "superclass:it.unitn.ing.rista.diffr.StructureSolutionMethod",
      "superclass:it.unitn.ing.rista.diffr.TDSModel"
  };

  public static int textureID = 0;
  public static int sizeStrainID = 1;
  public static int sizeStrainSymID = 2;
  public static int antiphaseBoundaryID = 3;
  public static int planarDefectsID = 4;
  public static int magneticStructureID = 5;
  public static int strainID = 6;
  //	public static int stressID = 7;
  public static int microAbsorptionID = 7;
  public static int structureFactorModelID = 9;
  public static int structureFactorExtractorID = 10;
//  public static int structureSolutionMethodID = 11;

  public static int structureModelID = 8;
  public static int tdsModelID = 11;

  public static final int scaleFactorID = 8;

  static final int startingCellParID = 0;

  public static final String TRICLINIC = "triclinic";
  public static final String MONOCLINIC = "monoclinic";
  public static final String ORTHOROMBIC = "orthorhombic";
  public static final String TETRAGONAL = "tetragonal";
  public static final String TRIGONAL = "trigonal";
  public static final String HEXAGONAL = "hexagonal";
  public static final String CUBIC = "cubic";

  public static final String[] cs = {TRICLINIC, MONOCLINIC, ORTHOROMBIC, TETRAGONAL,
      TRIGONAL, HEXAGONAL, CUBIC};
  public int ic[] = new int[6];
  public boolean isosizestrain = true;
  public static final String[] sgconvlist = {"Int. number",
      "Schoenflies",
      "Hermann-Mauguin",
      "Hall"};
  public static final int SGconvN = 4;
  public String SGconv = sgconvlist[2];
  protected Vector<Reflection> reflectionv = new Vector<>(100, 100);
//  public Vector<Reflection> absentreflectionv = new Vector<Reflection>(0, 10);
  public boolean refreshReflectionv = true;
  //	protected boolean refreshReflectionvHard = false;
  public boolean refreshAtoms = true;
  public boolean refreshEnergyComputation = true;
  public boolean refreshFragments = false;
  public boolean refreshFhklcomp = true;
  public boolean refreshPositions = true;
  protected boolean refreshCellSymmetry = true;
  protected boolean refreshCellVolume = true;
  protected boolean refreshCrystMicrostrain = true;
  boolean isRefreshingCell = false;
  boolean computeFaultAsymmetry = false;
  protected boolean refreshSpaceGroup = true;

  protected boolean extractIntensities = false;
  protected boolean extractPositions = false;
  protected boolean extractStructureFactors = false;
  protected boolean solveStructure = false;

  private double cellVolume = 1.0;
  private double fullCellVolume = 1.0;
  private double so[] = new double[9];
	private double full_so[] = new double[9];
  //  public int totalNumberOfAtoms = 0;
  public Vector<AtomSite> fullAtomList = null;
//  private T_SgInfo SgInfo = null;
	private PhaseInfo phaseInfo = null;

  private boolean checkSGLater = false;

  public static final int lowDspaceID = 8;
  public static final int highDspaceID = 9;
  public static final int lowReflnDspaceID = 10;
  public static final int highReflnDspaceID = 11;
  public static final int centeringTypeID = 12;
  public static final int mineralNameID = 13;
	public static final int systematicNameID = 14;
  public static final int symopsID = 0;
  double lowDspace = 0.0;
  double highDspace = 0.0;
  double lowReflnDspace = 0.0;
  double highReflnDspace = 0.0;
  public double[] reducedCellFactor = {1.0, 1.0, 1.0};
  private boolean plotFit = false;
	public double grainSizeInAngstrom;
	public double thermalStrain;

  public boolean plotFit() {
    return plotFit;
  }

  public void toggleView() {
    plotFit = !plotFit;
  }

	public int getNumberOfSizeStrainCoefficients() {
		return getActiveSizeStrain().getNumberOfSizeStrainCoefficients();  //To change body of created methods use File | Settings | File Templates.
	}

	public int getReflexIndex(Reflection refl) {
		int numberHKL = gethklNumber();
		for (int i = 0; i < numberHKL; i++)
			if (getReflex(i).equalsTo(refl))
				return i;
		return -1;
	}

	public double[] getSoVector() {
		return full_so;
	}

	public double[] getOriginalSoVector() {
		return so;
	}

	public static enum CellOperation {
    FORWARD, BACKWARD, INVERT_A, INVERT_B, INVERT_C,
    INVERT_ALPHA, INVERT_BETA, INVERT_GAMMA, SWITCH_AB, SWITCH_BC, SWITCH_CA
  }

  public Phase(XRDcat afile, String alabel) {
    super(afile, alabel);
    initXRD();
    identifier = "Phase";
  }

  public Phase(XRDcat afile) {
    this(afile, "Phase_x");
  }

	public Phase() {
		identifier = "Phase";
		IDlabel = "Phase";
		description = "select this to use a Phase";
	}

  public void initConstant() {
    Nstring = 15;
    Nstringloop = 1;
    Nparameter = 9;
    Nparameterloop = 0;
    Nsubordinate = classlistcs.length;
    Nsubordinateloop = 2;
  }

  public void initDictionary() {
    System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
    System.arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
  }

  public void initParameters() {
    super.initParameters();
    setPhaseName("Phase unknown");
    setPhaseID("Phase unknown");
    setSGconv(sgconvlist[2]);
    stringField[2] = "triclinic";
    stringField[3] = "1";
    stringField[4] = "C1^1";
    stringField[5] = "P1";
    stringField[6] = "P1";
    stringField[centeringTypeID] = "P";
    stringField[lowDspaceID] = MaudPreferences.getPref("refinement.lowDspacing", "0");
    stringField[highDspaceID] = MaudPreferences.getPref("refinement.highDspacing", "5000");
    stringField[lowReflnDspaceID] = MaudPreferences.getPref("reflns_list.lowDspacing", "0.7");
    stringField[highReflnDspaceID] = MaudPreferences.getPref("reflns_list.highDspacing", "50");
    stringField[mineralNameID] = "Unknown";
    stringField[systematicNameID] = "Unknown";

	  setZ("1");
//    stringField[8] = new String("none");
    for (int i = 0; i < 3; i++) {
      parameterField[i] = new Parameter(this, getParameterString(i), 5,
          ParameterPreferences.getDouble(getParameterString(i) + ".min", 5),
          ParameterPreferences.getDouble(getParameterString(i) + ".max", 30));
      parameterField[i].setPositiveOnly();
    }
    for (int i = 3; i < 6; i++) {
      parameterField[i] = new Parameter(this, getParameterString(i), 90,
          ParameterPreferences.getDouble(getParameterString(i) + ".min", 90),
          ParameterPreferences.getDouble(getParameterString(i) + ".max", 120));
      parameterField[i].setPositiveOnly();
    }
    parameterField[6] = new Parameter(this, getParameterString(6), 0,
        ParameterPreferences.getDouble(getParameterString(6) + ".min", -0.1),
        ParameterPreferences.getDouble(getParameterString(6) + ".max", 0.1));
    parameterField[7] = new Parameter(this, getParameterString(7), 0,
        ParameterPreferences.getDouble(getParameterString(7) + ".min", 0.001),
        ParameterPreferences.getDouble(getParameterString(7) + ".max", 100));
    parameterField[7].setPositiveOnly();
    parameterField[scaleFactorID] = new Parameter(this, getParameterString(scaleFactorID), 1,
        ParameterPreferences.getDouble(getParameterString(scaleFactorID) + ".min", 0),
        ParameterPreferences.getDouble(getParameterString(scaleFactorID) + ".max", 100));
    parameterField[scaleFactorID].setPositiveOnly();
    setMagneticStructureModel("no magnetic");
    setTextureModel("none tex");
    setSizeStrainModel("Delft");
    setSizeStrainSymModel("Isotropic");
    setAntiphaseBoundary("none abm");
    setPlanarDefects("none pd");
	  setSubordinateModel(microAbsorptionID, "No microabsorption");
    setSymmetry(cs[0]);
    setStrainModel("no strain");
//		setStressModel(0);
    setSubordinateModel(structureFactorModelID, "atomic standard model");
    setSubordinateModel(structureFactorExtractorID, "Le Bail");
//    setSubordinateModel(structureSolutionMethodID, "Genetic Algorithm SDPD");
    setSubordinateModel(structureModelID, "Atomic Structure");
    setSubordinateModel(tdsModelID, "None TDS");

  }

  public String toDataString() {
    return "phase_" + toXRDcatString();
  }

// we rewrite this to not include the dummy cell parameters

  public Vector getParameterVector(boolean mainPars, boolean subAlso) {
    int i, j, k;
    XRDcat obj = null;

    Vector parVector = new Vector(0, 1);
    Vector tmpVector = null;

    // first we force a refresh of the cell
    CellSymmetry();

    if (mainPars) {
      for (i = 0; i < Nparameter; i++) {
        // here the last modification
        if (i > 5 || ic[i] == 1)
          parVector.addElement(parameterField[i]);
      }
      for (i = 0; i < Nparameterloop; i++) {
        for (j = 0; j < numberofelementPL(i); j++)
          parVector.addElement(parameterloopField[i].elementAt(j));
      }
    }

    if (subAlso) {
      for (i = 0; i < Nsubordinate; i++) {
        obj = subordinateField[i];
        if (obj != null) {
          tmpVector = obj.getParameterVector(true, subAlso);
          for (j = 0; j < tmpVector.size(); j++)
            parVector.addElement(tmpVector.elementAt(j));
        }
      }
      for (i = 0; i < Nsubordinateloop; i++) {
        for (j = 0; j < numberofelementSubL(i); j++) {
          obj = (XRDcat) subordinateloopField[i].elementAt(j);
          tmpVector = obj.getParameterVector(true, subAlso);
          for (k = 0; k < tmpVector.size(); k++)
            parVector.addElement(tmpVector.elementAt(k));
        }
      }
    }
    return parVector;
  }

// we rewrite this to not include the dummy cell parameters

  public basicObj[] getChildren(String searchString) {

    int i, j, k;

//	  System.out.println(this + " - " + searchString);
    basicObj childrens[] = new basicObj[getChildCount(searchString)];

    basicObj obj;

    k = 0;
    for (i = 0; i < Nparameter; i++) {
      // here the last modification
      if (i > 5 || ic[i] == 1)
        if (searchString == null || searchString.equalsIgnoreCase("") ||
            parameterField[i].getLabel().contains(searchString))
          childrens[k++] = parameterField[i];
    }
    for (i = 0; i < Nparameterloop; i++)
      for (j = 0; j < numberofelementPL(i); j++)
        if ((obj = (basicObj) parameterloopField[i].elementAt(j)) != null)
          if (searchString == null || searchString.equalsIgnoreCase("") ||
              obj.getLabel().contains(searchString))
            childrens[k++] = obj;
    for (i = 0; i < Nsubordinate; i++)
      if ((subordinateField[i]) != null && subordinateField[i].getChildCount(searchString) > 0) {
        childrens[k++] = subordinateField[i];
//	      System.out.println("Adding for phase: " + subordinateField[i].toString());
      }
	  for (i = 0; i < Nsubordinateloop; i++)
      for (j = 0; j < numberofelementSubL(i); j++)
        if ((obj = (basicObj) subordinateloopField[i].elementAt(j)) != null &&
            ((basicObj) subordinateloopField[i].elementAt(j)).getChildCount(searchString) > 0)
          childrens[k++] = obj;

    return childrens;
  }

  public int getChildCount(String searchString) {
    int i, j;

    // first we force a refresh of the cell
    CellSymmetry();

    int numberofObjects = 0;

    for (i = 0; i < Nparameter; i++) {
      // here the last modification
      if (i > 5 || ic[i] == 1)
        if (searchString == null || searchString.equalsIgnoreCase("") ||
            parameterField[i].getLabel().contains(searchString))
          numberofObjects++;
    }

    for (i = 0; i < Nparameterloop; i++)
      for (j = 0; j < numberofelementPL(i); j++)
        if (parameterloopField[i].elementAt(j) != null)
          if (searchString == null || searchString.equalsIgnoreCase("") ||
              ((basicObj) parameterloopField[i].elementAt(j)).getLabel().contains(searchString))
            numberofObjects++;
    for (i = 0; i < Nsubordinate; i++)
      if (subordinateField[i] != null && subordinateField[i].getChildCount(searchString) > 0)
        numberofObjects++;
    for (i = 0; i < Nsubordinateloop; i++)
      for (j = 0; j < numberofelementSubL(i); j++)
        if (subordinateloopField[i].elementAt(j) != null &&
            ((basicObj) subordinateloopField[i].elementAt(j)).getChildCount(searchString) > 0)
          numberofObjects++;


    return numberofObjects;
  }

  public basicObj getChildAt(int childIndex) {
    int i, j, index = 0;

    // first we force a refresh of the cell
    CellSymmetry();

    for (i = 0; i < Nparameter; i++) {
      // here the last modification
      if (i > 5 || ic[i] == 1) {
        if (childIndex == index)
          return parameterField[i];
        index++;
      }
    }

    for (i = 0; i < Nparameterloop; i++)
      for (j = 0; j < numberofelementPL(i); j++) {
        if (parameterloopField[i].elementAt(j) != null) {
          if (index == childIndex)
            return (basicObj) parameterloopField[i].elementAt(j);
          index++;
        }
      }
    for (i = 0; i < Nsubordinate; i++)
      if (subordinateField[i] != null) {
        if (index == childIndex)
          return (basicObj) subordinateField[i];
        index++;
      }
    for (i = 0; i < Nsubordinateloop; i++)
      for (j = 0; j < numberofelementSubL(i); j++)
        if (subordinateloopField[i].elementAt(j) != null) {
          if (index == childIndex)
            return (basicObj) subordinateloopField[i].elementAt(j);
          index++;
        }

    return null;
  }

  public int getIndex(basicObj node) {
    int i, j, index = 0;

    // first we force a refresh of the cell
    CellSymmetry();

    for (i = 0; i < Nparameter; i++) {
      // here the last modification
      if (i > 5 || ic[i] == 1) {
        if (parameterField[i] == node)
          return index;
        index++;
      }
    }

    for (i = 0; i < Nparameterloop; i++)
      for (j = 0; j < numberofelementPL(i); j++) {
        if (parameterloopField[i].elementAt(j) != null) {
          if (parameterloopField[i].elementAt(j) == node)
            return index;
          index++;
        }
      }
    for (i = 0; i < Nsubordinate; i++)
      if (subordinateField[i] != null) {
        if (subordinateField[i] == node)
          return index;
        index++;
      }
    for (i = 0; i < Nsubordinateloop; i++)
      for (j = 0; j < numberofelementSubL(i); j++)
        if (subordinateloopField[i].elementAt(j) != null) {
          if (subordinateloopField[i].elementAt(j) == node)
            return index;
          index++;
        }

    return -1;
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      for (int i = 0; i < parameterField.length; i++)
        if (source == parameterField[i]) {
          if (i < 6 && !isRefreshingCell) {
            notifyParameterChanged(source, Constants.CELL_CHANGED);
            return;
          }
          if (i == 6) {
            notifyParameterChanged(source, Constants.THERMAL_SHIFT_CHANGED);
            notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED);
            return;
          }
          if (i == 7) {
            notifyParameterChanged(source, Constants.PHASE_WEIGHT_CHANGED);
	          notifyParameterChanged(source, Constants.STRUCTURE_FACTOR_CHANGED);
            return;
          }
          if (i == 8) {
            notifyParameterChanged(source, Constants.BEAM_INTENSITY_CHANGED);
            return;
          }
        }

      super.notifyParameterChanged(source, Constants.PARAMETER_CHANGED);
    }
  }

  public void updateAll() {
    refreshComputation = true;
    refreshReflectionv = true;
    refreshFhklcomp = true;
    refreshPositions = true;
	  refreshSpaceGroup = true;
//		Constants.speedUp = false;
//			refreshReflectionvHard = true;
    refreshCellSymmetry = true;
    refreshCellVolume = true;
    refreshCrystMicrostrain = true;
    refreshAtoms = true;
    refreshEnergyComputation = true;
    refreshFragments = true;
    fullAtomList = null;
//    getActiveTexture().refreshComputation = true;
  }

  public void refreshForNotificationUp(XRDcat source, int reason) {
    refreshComputation = true;
    if (source == null)
      return;
    if (source == this && !getFilePar().isComputingDerivate()) {
      updateAll();
    }
    if (reason == Constants.CELL_CHANGED) {
      refreshCellSymmetry = true;
      refreshCellVolume = true;
      refreshReflectionv = true;
      refreshEnergyComputation = true;
      refreshPositions = true;
	    refreshSpaceGroup = true;
      CellSymmetry();
    }
    if (reason == Constants.STRUCTURE_FACTOR_CHANGED) {
//      refreshReflectionv = true;
      refreshFhklcomp = true;
//      System.out.println("Refresh peak list");
    }
    if (reason == Constants.PEAKS_PARAMETER_CHANGED) {
      refreshReflectionv = true;
      refreshFhklcomp = true;
      refreshPositions = true;
	    refreshReflectionv = true;
	    refreshCrystMicrostrain = true;
//      System.out.println("Refresh peak list up");
    }
    if (reason == Constants.ERROR_POSITION_CHANGED) {
	    refreshPositions = true;
    }
    if (reason == Constants.SAMPLE_BROADENING) {
      refreshCrystMicrostrain = true;
    }
    if (reason == Constants.ATOM_POSITION_CHANGED
      /*(source instanceof StructureModel || source instanceof AtomSite || source instanceof Fragment)*/) {
      refreshAtoms = true;
      refreshEnergyComputation = true;
      fullAtomList = null;
      refreshFhklcomp = true;
      // <-- mauro
    }
    if (reason == Constants.FRAGMENT_POSITION_CHANGED) {
      refreshAtoms = true;
      refreshEnergyComputation = true;
      refreshFragments = true;
      fullAtomList = null;
      refreshFhklcomp = true;
      // mauro -->
    }
//    System.out.println("Phase up: " + refreshComputation);
  }

  public void refreshForNotificationDown(XRDcat source, int reason) {
    refreshComputation = true;
    if (source == null)
      return;
    if (source == this && !getFilePar().isComputingDerivate()) {
      updateAll();
    }
    if (reason == Constants.CELL_CHANGED) {
      refreshCellSymmetry = true;
      refreshCellVolume = true;
      refreshReflectionv = true;
      refreshEnergyComputation = true;
      refreshPositions = true;
	    refreshSpaceGroup = true;
      CellSymmetry();
    }
    if (reason == Constants.STRUCTURE_FACTOR_CHANGED) {
//      refreshReflectionv = true;
      refreshFhklcomp = true;
//      System.out.println("Refresh peak list");
    }
    if (reason == Constants.PEAKS_PARAMETER_CHANGED) {
//      refreshReflectionv = true;
      refreshFhklcomp = true;
      refreshPositions = true;
	    refreshReflectionv = true;
	    refreshCrystMicrostrain = true;
//      System.out.println("Refresh peak list down");
    }
    if (reason == Constants.ERROR_POSITION_CHANGED) {
	    refreshPositions = true;
    }
    if (reason == Constants.SAMPLE_BROADENING) {
      refreshCrystMicrostrain = true;
    }
    if (reason == Constants.ATOM_POSITION_CHANGED
      /*(source instanceof StructureModel || source instanceof AtomSite || source instanceof Fragment)*/) {
      refreshAtoms = true;
      refreshEnergyComputation = true;
      fullAtomList = null;
      refreshFhklcomp = true;
      // <-- mauro
    }
    if (reason == Constants.FRAGMENT_POSITION_CHANGED) {
      refreshAtoms = true;
      refreshEnergyComputation = true;
      refreshFragments = true;
      fullAtomList = null;
      refreshFhklcomp = true;
      // mauro -->
    }
//    System.out.println("Phase down: " + refreshComputation);
  }

  public void moveAtomsToStructureModel() {
    boolean oldStatus = isAbilitatetoRefresh;
    StructureModel structureM = getActiveStructureModel();
    for (int i = 0; i < getAtomList().size(); i++) {
      AtomSite anatom = (AtomSite) getAtomList().get(i);
      anatom.setParent(structureM);
      structureM.addAtom(anatom);
    }

    // now we remove all atoms from the list avoiding notifications
    if (getAtomList().size() > 0) {
      getAtomList().removeAllElements();
      getAtomList().updateList();
    }

    isAbilitatetoRefresh = oldStatus;
  }

  public boolean quantityFromOccupancy() {
//    System.out.println(getActiveStructureModel().quantityFromOccupancy());
    return getActiveStructureModel().quantityFromOccupancy();
    /*  if (getStructureFactorModel() instanceof
              it.unitn.ing.rista.diffr.sfm.StructureFactorStandardModel)
        return true;
      else
        return false; */
  }

	public boolean isDebyeWallerModelDimensionLess() {
		return getActiveStructureModel().isDebyeWallerModelDimensionLess();
	}

	public void refreshAll(boolean firstLoading) {
//    refreshComputation = true;
    FilePar aparfile = getFilePar();
    if (!aparfile.isLoadingFile() && isAbilitatetoRefresh) {
      if (!aparfile.isComputingDerivate()) {
        updateAll();
        getActiveTexture().refreshComputation = true;
        getActiveStrain().refreshComputation = true;
//			getActiveStress().refreshStress = true;
        CellSymmetry();
      }
      super.refreshAll(firstLoading);
    }
  }

  public void writeAllFields(BufferedWriter out) {
    int i;

    stringField[0] = toXRDcatString();
    for (i = 0; i < 3; i++)
      writeField(out, diclist[i], stringField[i]);
    int spgr = 3 + getSGconv();
    writeField(out, diclist[spgr], stringField[spgr]);
    for (i = 7; i < Nstring; i++)
      writeField(out, diclist[i], stringField[i]);

  }

  public void writeAllParameters(BufferedWriter out) {
    int i;

    CellSymmetry();
    for (i = 0; i < 6; i++)
//      if (ic[i] == 1)
      writeParameter(out, diclist[i + totstringloop], parameterField[i]);
    for (i = 6; i < Nparameter; i++)
      writeParameter(out, diclist[i + totstringloop], parameterField[i]);
    try {
      if (Nparameter > 0)
        out.newLine();
    } catch (IOException ioe) {
      System.out.println("Error in writing the loop parameter " + toXRDcatString());
    }

  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);

    extractIntensities = getActiveTexture().needIntensityExtractor();
    extractPositions = getActiveStrain().needPositionExtractor();
    extractStructureFactors = ((StructureFactorModel)
        getActiveSubordinateModel(structureFactorModelID)).needStructureFactorExtractor();
    solveStructure = ((StructureFactorModel)
        getActiveSubordinateModel(structureFactorModelID)).canSolveStructure();

    lowDspace = Double.parseDouble(getString(lowDspaceID));
    highDspace = Double.parseDouble(getString(highDspaceID));
    lowReflnDspace = Double.parseDouble(getString(lowReflnDspaceID));
    highReflnDspace = Double.parseDouble(getString(highReflnDspaceID));

  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(firstLoading);

    for (int i = 0; i < 9; i++)
      if (i != 6)
        parameterField[i].setPositiveOnly();

	  grainSizeInAngstrom = Math.abs(getAbsorptionCrystSize().getValueD());
	  thermalStrain = Math.abs(getThermalStrain().getValueD());
  }

  private int lastSGLoaded = 0;

  public int setField(String cif, String astring, String astringerror, String min, String max, boolean free,
                      String refName, String refBound, String constant, String ratio, String expression,
                      boolean autoTrace, boolean positive) {
    int index = super.setField(cif, astring, astringerror, min, max, free,
        refName, refBound, constant, ratio, expression, autoTrace, positive);
// System.out.println(cif + " " + astring);
    if (index >= 3 && index <= 6) {
      int conv = index - 3;
	    System.out.println("Setting: " + stringField[index] + " " + conv);
      if (allowSetting(lastSGLoaded, conv)) {
	      System.out.println("Allowing: " + stringField[index]);
        setSGconv(sgconvlist[conv]);
        setSpaceGroup(true, stringField[index], false);
        lastSGLoaded = conv;
      }
    }

    if (isImportingFromCIF && (index == mineralNameID || index == systematicNameID || index == 0)) {
      String curatedName = getString(index).replaceAll(" ", "");
      if (!curatedName.isEmpty() && !curatedName.equalsIgnoreCase("unknown") &&
          !curatedName.equalsIgnoreCase("?"))
        setLabel(getString(index));
    }

    return index;
  }

  boolean allowSetting(int SGLoaded, int conv) {
	  if (conv == 3 && SpaceGroups.useCCTBX())
		  return true;
    if (conv == 2)
      return true;
    return false;
  }

  boolean spaceGroupAssigned = true;

  public void readall(CIFtoken ciffile) {
    checkSGLater = true;
    lastSGLoaded = 0;
    super.readall(ciffile);
    checkSGLater = false;
    lastSGLoaded = 0;
    setSpaceGroup(true, getSpaceGroup(), false);
  }

	public Parameter getThermalStrain() {
		return parameterField[6];
	}

	public Parameter getAbsorptionCrystSize() {
    return parameterField[7];
  }

  public void setAbsorptionCrystSize(String value) {
    getAbsorptionCrystSize().setValue(value);
  }

  public double getAbsorptionCrystSizeD() {
    return Math.abs(getAbsorptionCrystSize().getValueD() / 10000); // in microns
  }

	public double getCrystalThickness() {
		return grainSizeInAngstrom; // in microns
	}

	public double getThermalStrainCached() {
		return thermalStrain; // in microns
	}

	public void addAtom() {
    AtomSite newatom = new AtomSite(this);
    addAtom(newatom);
    newatom.addAtomWithSymbol("Ca");
  }

  public void addAtom(AtomSite newatom) {
    addsubordinateloopField(0, newatom);
    refreshAtoms = true;
    refreshEnergyComputation = true;
    fullAtomList = null;
    refreshFhklcomp = true;

  }

  public boolean removeSelectedAtom() {
    refreshAtoms = true;
    refreshEnergyComputation = true;
    fullAtomList = null;
    refreshFhklcomp = true;
    return removeselSubLField(0);
  }

  public void removeAtomAt(int number) {
    getAtomList().removeItemAt(number);
    refreshAtoms = true;
    refreshEnergyComputation = true;
    fullAtomList = null;
    refreshFhklcomp = true;
  }

  public int getAtomNumber() {
    return numberofelementSubL(0);
  }

  public ListVector getAtomList() {
    return subordinateloopField[0];
  }

  public static final int getNumber(String sym) {
    return SpaceGroups.getSymmetryNumber(sym);
  }

  public int getSGconv() {
//    if (SGconv != null)
//      for (int i = 0; i < SGconvN; i++)
//        if (SGconv.equalsIgnoreCase(sgconvlist[i]))
//          return i;
	  if (SpaceGroups.useCCTBX())
			return 3;
	  return 2; // lastSGLoaded;
  }

/*  public String getSGconvS() {
    return sgconvlist[getSGconv()];
  }*/

  public void setSGconv(String sg) {
    if (!SGconv.equals(sg)) {
      SGconv = sg;
    }
  }

  public String getSpaceGroup() {
    return stringField[3 + getSGconv()];
  }

  public String getSpaceGroupHM() {
    return stringField[5];
  }

  public String getSpaceGroupHall() {
    return stringField[6];
  }

  public String getSpaceGroupNumber() {
    return stringField[3];
  }

  public String checkNotStandardGroup(String sg) {
    if (sg == null || (!getSymmetry().equalsIgnoreCase("triclinic") && !getSymmetry().equalsIgnoreCase("monoclinic")))
      return sg;
//    if (Constants.testing)
//      System.out.println("Checking "+sg);
    switch (sg.charAt(0)) {
      case 'A':
      case 'a':
      case 'B':
      case 'b':
      case 'C':
      case 'c':
      case 'I':
      case 'i':
        convertSG(sg.charAt(0));
        StringBuffer tmp = new StringBuffer(sg);
        tmp.setCharAt(0, 'P');
        return tmp.toString();
      default: {
      }
    }
    return sg;
  }

  public void setSpaceGroup(boolean forceChange, String sg, boolean checkMonoclinicAxis) {

    if (checkSGLater)
      return;

    System.out.println("Trying to change to space group: " + sg);

	  spaceGroupAssigned = true;
	  if (SpaceGroups.useCCTBX()) {
		  String oldSg = getSpaceGroup();
		  Spacegroup space_group = SpaceGroups.getCorrectSpaceGroup(sg);
		  if (forceChange || !oldSg.equals(space_group.hall)) {
			  int monoclinicAxis = getMonoclinicAxis();
			  refreshSpaceGroup = true;
//		  System.out.println(space_group.symmetry + " : " + space_group.number + " | " + space_group.hermann_mauguin + " | " + space_group.hall + " | " + space_group.schoenflies);
			  stringField[5] = space_group.hermann_mauguin;
			  stringField[6] = space_group.hall;
			  stringField[4] = space_group.schoenflies;
			  stringField[3] = Integer.toString(space_group.number);
			  setSymmetry(space_group.symmetry);
			  int newMonoclinicAxis = getMonoclinicAxis();
			  if (checkMonoclinicAxis && monoclinicAxis != newMonoclinicAxis) {
				  boolean oldPermission = isAbilitatetoRefresh;
				  isAbilitatetoRefresh = false;
				  switch (monoclinicAxis) {
					  case 1:
						  switch (newMonoclinicAxis) {
							  case 1:
								  break;
							  case 2:
								  refreshCellForChange(CellOperation.FORWARD);
								  break;
							  case 3:
								  refreshCellForChange(CellOperation.BACKWARD);
								  break;
						  }
						  break;
					  case 2:
						  switch (newMonoclinicAxis) {
							  case 1:
								  refreshCellForChange(CellOperation.BACKWARD);
								  break;
							  case 2:
								  break;
							  case 3:
								  refreshCellForChange(CellOperation.FORWARD);
								  break;
						  }
						  break;
					  case 3:
						  switch (newMonoclinicAxis) {
							  case 1:
								  refreshCellForChange(CellOperation.FORWARD);
								  break;
							  case 2:
								  refreshCellForChange(CellOperation.BACKWARD);
								  break;
							  case 3:
								  break;
						  }
						  break;
				  }
				  isAbilitatetoRefresh = oldPermission;
//          refreshAll(false);
			  }
			  refreshReflectionv = true;
			  refreshFhklcomp = true;
			  refreshPositions = true;
			  refreshCellSymmetry = true;
			  refreshCellVolume = true;
			  refreshCrystMicrostrain = true;
			  getActiveTexture().refreshComputation = true;
			  getActiveStrain().refreshComputation = true;
			  notifyUpObjectChanged(this, Constants.STRING_CHANGED);
		  }
	  } else {
		  String newsg = null;
//    if (Constants.testing)
//		  System.out.println("Start to modify:" + sg);
		  sg = Misc.toStringDeleteBlank(sg);
//		sg = checkNotStandardGroup(sg);
		  String oldSg = getSpaceGroup();
		  if (forceChange || !oldSg.equals(sg)) {
			  int monoclinicAxis = getMonoclinicAxis();
			  refreshSpaceGroup = true;
//    if (Constants.testing)
//		  System.out.println("Symmetry:" + getSymmetry());
//    if (Constants.testing)
//		  System.out.println("Looking for space group:" + sg);
			  try {
				  newsg = SpaceGroups.sglookup(sg, getSGconv());
			  } catch (NullPointerException e) {
				  e.printStackTrace();
			  }
			  try {
				  boolean found = false;
				  for (int i = 0; i < cs.length && !found; i++) {
					  for (int i1 = SpaceGroups.getBeginSG(cs[i], 2); i1 <= SpaceGroups.getEndSG(cs[i], 2); i1++) {
//            System.out.println("Checking: " + cs[i] + " , " + i1 + " ->" + getSpaceGroup(i1, getSGconv()));
						  if (newsg != null && newsg.equals(getSpaceGroup(i1, getSGconv()))) {
							  setSymmetry(cs[i]);
							  found = true;
							  break;
						  }
					  }
				  }
				  try {
					  newsg = SpaceGroups.sglookup(sg, getSGconv());
				  } catch (NullPointerException e) {
//    if (Constants.testing)
//		  System.out.println("Non standard space group:" + sg);
					  newsg = checkNotStandardGroup(sg);
				  }
				  if (newsg == null)
					  newsg = checkNotStandardGroup(sg);
//		  System.out.println("Building for space group:" + newsg);
				  stringField[3 + getSGconv()] = newsg;
				  if (getSGconv() != 2) {
					  int index = 0;
					  found = false;
					  for (int i = SpaceGroups.getBeginSG(getSymmetry(), 2); i <= SpaceGroups.getEndSG(getSymmetry(), 2)
							  && !found; i++) {
						  if (sg == getSpaceGroup(i, getSGconv())) {
							  index = i;
							  found = true;
						  }
					  }
					  if (!found) {
						  String tmpSymmetry = getSymmetry();
						  for (int i = 0; i < cs.length; i++) {
							  if (!found && !cs[i].equals(tmpSymmetry)) {
								  stringField[2] = new String(cs[i]);
								  for (int i1 = SpaceGroups.getBeginSG(getSymmetry(), 2); i1 <= SpaceGroups.getEndSG(getSymmetry(), 2)
										  && !found; i1++) {
									  if (sg == getSpaceGroup(i1, getSGconv())) {
										  index = i1;
										  found = true;
									  }
								  }
							  }
						  }
						  if (!found)
							  stringField[2] = tmpSymmetry;
					  }
					  stringField[5] = new String(getSpaceGroup(index, 2));
				  }
				  int newMonoclinicAxis = getMonoclinicAxis();
				  if (checkMonoclinicAxis && monoclinicAxis != newMonoclinicAxis) {
					  boolean oldPermission = isAbilitatetoRefresh;
					  isAbilitatetoRefresh = false;
					  switch (monoclinicAxis) {
						  case 1:
							  switch (newMonoclinicAxis) {
								  case 1:
									  break;
								  case 2:
									  refreshCellForChange(CellOperation.FORWARD);
									  break;
								  case 3:
									  refreshCellForChange(CellOperation.BACKWARD);
									  break;
							  }
							  break;
						  case 2:
							  switch (newMonoclinicAxis) {
								  case 1:
									  refreshCellForChange(CellOperation.BACKWARD);
									  break;
								  case 2:
									  break;
								  case 3:
									  refreshCellForChange(CellOperation.FORWARD);
									  break;
							  }
							  break;
						  case 3:
							  switch (newMonoclinicAxis) {
								  case 1:
									  refreshCellForChange(CellOperation.FORWARD);
									  break;
								  case 2:
									  refreshCellForChange(CellOperation.BACKWARD);
									  break;
								  case 3:
									  break;
							  }
							  break;
					  }
					  isAbilitatetoRefresh = oldPermission;
//          refreshAll(false);
				  }
				  refreshReflectionv = true;
				  refreshFhklcomp = true;
				  refreshPositions = true;
				  refreshCellSymmetry = true;
				  refreshCellVolume = true;
				  refreshCrystMicrostrain = true;
				  getActiveTexture().refreshComputation = true;
				  getActiveStrain().refreshComputation = true;
				  notifyUpObjectChanged(this, Constants.STRING_CHANGED);
			  } catch (NullPointerException e) {
				  e.printStackTrace();
				  stringField[5] = sg;
				  spaceGroupAssigned = false;
			  }
		  }
	  }
  }

  public void convertSG(char firstletter) {
    double traslx = 0.0;
    double trasly = 0.0;
    double traslz = 0.0;
//    if (Constants.testing)
//      System.out.println("Converting "+firstletter);
    switch (firstletter) {
      case 'C':
      case 'c':
        traslx = 0.5;
        trasly = 0.5;
        break;
      case 'B':
      case 'b':
        traslx = 0.5;
        traslz = 0.5;
        break;
      case 'A':
      case 'a':
        trasly = 0.5;
        traslz = 0.5;
        break;
      case 'I':
      case 'i':
        traslx = 0.5;
        trasly = 0.5;
        traslz = 0.5;
        break;
      default: {
        return;
      }
    }
    convertAtomsForSG(traslx, trasly, traslz);
  }

  public void convertAtomsForSG(double traslx, double trasly, double traslz) {
    int numbAtomn = getAtomNumber();
//    if (Constants.testing)
//      System.out.println("Adding "+numbAtomn);
    for (int i = 0; i < numbAtomn; i++) {
      AtomSite oldatom = getAtom(i);
      AtomSite newatom = new AtomSite(this);
      newatom.setSiteLabel(oldatom.getSiteLabel() + "a");
	    for (int j = 0; j < oldatom.subordinateloopField[AtomSite.scattererLoopID].size(); j++) {
		    newatom.addsubordinateloopField(AtomSite.scattererLoopID, ((XRDcat)
				    oldatom.subordinateloopField[AtomSite.scattererLoopID].elementAt(j)).getCopy(this));
	    }
      newatom.getLocalCoordX().setValue(oldatom.getLocalCoordX().getValueD() + traslx);
      newatom.getLocalCoordX().setEqualTo(oldatom.getLocalCoordX(), 1.0, traslx);
      newatom.getLocalCoordY().setValue(oldatom.getLocalCoordY().getValueD() + trasly);
      newatom.getLocalCoordY().setEqualTo(oldatom.getLocalCoordY(), 1.0, trasly);
      newatom.getLocalCoordZ().setValue(oldatom.getLocalCoordZ().getValueD() + traslz);
      newatom.getLocalCoordZ().setEqualTo(oldatom.getLocalCoordZ(), 1.0, traslz);
      newatom.getOccupancy().setValue(oldatom.getOccupancy().getValueD());
      newatom.getOccupancy().setEqualTo(oldatom.getOccupancy(), 1.0, 0.0);
      newatom.getBfactor().setValue(oldatom.getBfactor().getValueD());
      newatom.getBfactor().setEqualTo(oldatom.getBfactor(), 1.0, 0.0);
      newatom.setDummy(oldatom.isDummyAtom());
      for (int j = 0; j < 6; j++) {
        newatom.getAnisoBfactor(j).setValue(oldatom.getAnisoBfactor(j).getValueD());
        newatom.getAnisoBfactor(j).setEqualTo(oldatom.getAnisoBfactor(j), 1.0, 0.0);
      }
      addAtom(newatom);
    }
    getActiveStructureModel().convertAtomsForSG(traslx, trasly, traslz);
  }

  public void setPhaseName(String name) {
    stringField[0] = new String(name);
  }

public static final String getSpaceGroup(int index, int sgconv) {
    if (sgconv == 0)
      return SpaceGroups.getSgNumber(index);
    else if (sgconv == 1)
      return SpaceGroups.getSgSchoenflies(index);
    else if (sgconv == 3)
      return SpaceGroups.getSgHall(index);
    return SpaceGroups.getSgHM(index);
  }

  public String getPhaseName() {
    return stringField[0];
  }

  public void setPhaseID(String name) {
    stringField[1] = name;
  }

  public String getPhaseID() {
    return stringField[1];
  }

  public void setSymmetry(String name) {
    if (!stringField[2].equalsIgnoreCase(name)) {
      stringField[2] = name;
//      setSpaceGroup(true, getSpaceGroup(SpaceGroups.getBeginSG(getSymmetry(), getSGconv()), getSGconv()), false);
    }
  }

  public String getSymmetry() {
    return stringField[2];
  }

  public void setZ(String name) {
    if (!name.equals(stringField[7])) {
      stringField[7] = name;
    }
  }

  public String getZ() {
    return stringField[7];
  }

  public int getZnumber() {
    return Integer.valueOf(getZ());
  }

  // not used
/*	public boolean computeExpTextureFactor() {
		if (stringField[9].equalsIgnoreCase("compute"))
			return true;
		return false;
	}*/

  public String getMagneticStructureModel() {
    return getActiveMagneticStructure().identifier;
  }

  public void setMagneticStructureModel(String value) {
    if (subordinateField[getMagneticStructureID()] == null || !getMagneticStructureModel().equals(value)) {
      setsubordinateField(getMagneticStructureID(), value);
    }
  }

  public void setMagneticStructureModel(int number) {
    setMagneticStructureModel(getsubordIdentifier(getMagneticStructureID(), number));
  }

  public int getMagneticStructureID() {
    return 5;
  }

  public int magneticStructureModelsNumber() {
    return getsubordClassNumber(getMagneticStructureID());
  }

  public MagneticStructure getActiveMagneticStructure() {
    if (subordinateField[getMagneticStructureID()] == null)
      setMagneticStructureModel(0);
    return (MagneticStructure) subordinateField[getMagneticStructureID()];
  }

  public String getTextureModel() {
    return getActiveTexture().identifier;
  }

  public void setTextureModel(String value) {
    if (getActiveTexture().identifier != value) {
      refreshReflectionv = true;
      refreshSpaceGroup = true; // we force rebuilding of the reflection list
      getActiveTexture().refreshComputation = true;
      getActiveStrain().refreshComputation = true;
    }
    setSubordinateModel(getTextureID(), value);
  }

/*  public void setTextureModel(int number) {
    setSubordinateModel(getTextureID(), number);
  }*/

  public int getTextureID() {
    return 0;
  }

  public int textureModelsNumber() {
    return getsubordClassNumber(getTextureID());
  }

  public Texture getActiveTexture() {
    if (subordinateField[getTextureID()] == null)
      setSubordinateModel(getTextureID(), "none tex");
    return (Texture) subordinateField[getTextureID()];
  }

  public boolean extractIntensities() {
    return extractIntensities;
  }

  public boolean extractStructureFactors() {
    return extractStructureFactors;
  }

  public boolean solveStructure() {
    return solveStructure;
  }

  public String getSizeStrainModel() {
    return getActiveSizeStrain().identifier;
  }

  public void setSizeStrainModel(String value) {
    if (subordinateField[getSizeStrainID()] == null || !getSizeStrainModel().equals(value)) {
      setsubordinateField(getSizeStrainID(), value);
    }
  }

  public void setSizeStrainModel(int number) {
    setSizeStrainModel(getsubordIdentifier(getSizeStrainID(), number));
  }

  public int getSizeStrainID() {
    return 1;
  }

  public int sizeStrainModelsNumber() {
    return getsubordClassNumber(getSizeStrainID());
  }

  public SizeStrainModel getActiveSizeStrain() {
    if (subordinateField[getSizeStrainID()] == null)
      setSizeStrainModel(0);
    return (SizeStrainModel) subordinateField[getSizeStrainID()];
  }

  public String getSizeStrainSymModel() {
    return getActiveSizeStrainSym().identifier;
  }

  public void setSizeStrainSymModel(String value) {
    if (subordinateField[getSizeStrainSymID()] == null || !getSizeStrainSymModel().equals(value))
      setsubordinateField(getSizeStrainSymID(), value);
  }

  public void setSizeStrainSymModel(int number) {
    setSizeStrainSymModel(getsubordIdentifier(getSizeStrainSymID(), number));
  }

  public int getSizeStrainSymID() {
    return 2;
  }

  public int sizeStrainSymModelsNumber() {
    return getsubordClassNumber(getSizeStrainSymID());
  }

  public SizeStrainSymModel getActiveSizeStrainSym() {
    if (subordinateField[getSizeStrainSymID()] == null)
      setSizeStrainSymModel(0);
    return (SizeStrainSymModel) subordinateField[getSizeStrainSymID()];
  }

  public double getMeanCrystallite() {
    return getActiveSizeStrainSym().getMeanCrystallite();
  }

  public double getMeanMicrostrain() {
    return getActiveSizeStrainSym().getMeanMicrostrain();
  }

  public String getAntiphaseBoundary() {
    return getActiveAntiphaseBoundary().identifier;
  }

  public void setAntiphaseBoundary(String value) {
    if (subordinateField[getAntiphaseBoundaryID()] == null || !getAntiphaseBoundary().equals(value))
      setsubordinateField(getAntiphaseBoundaryID(), value);
  }

  public void setAntiphaseBoundary(int number) {
    setAntiphaseBoundary(getsubordIdentifier(getAntiphaseBoundaryID(), number));
  }

  public int getAntiphaseBoundaryID() {
    return 3;
  }

  public int AntiphaseBoundaryNumber() {
    return getsubordClassNumber(getAntiphaseBoundaryID());
  }

  public AntiphaseBoundary getActiveAntiphaseBoundary() {
    if (subordinateField[getAntiphaseBoundaryID()] == null)
      setAntiphaseBoundary(0);
    return (AntiphaseBoundary) subordinateField[getAntiphaseBoundaryID()];
  }

  public String getPlanarDefects() {
    return getActivePlanarDefects().identifier;
  }

  public void setPlanarDefects(String value) {
    if (subordinateField[getPlanarDefectsID()] == null || !getPlanarDefects().equals(value)) {
	    setsubordinateField(getPlanarDefectsID(), value);
//	    System.out.println("Resetting " + reflectionv);
	    if (reflectionv != null) {
		    reflectionv.removeAllElements();
		    refreshReflectionv = true;
	    }
    }
  }

  public void setPlanarDefects(int number) {
    setPlanarDefects(getsubordIdentifier(getPlanarDefectsID(), number));
  }

  public int getPlanarDefectsID() {
    return 4;
  }

  public int PlanarDefectsNumber() {
    return getsubordClassNumber(getPlanarDefectsID());
  }

  public PlanarDefects getActivePlanarDefects() {
    if (subordinateField[getPlanarDefectsID()] == null)
      setPlanarDefects(0);
    return (PlanarDefects) subordinateField[getPlanarDefectsID()];
  }

  public String getStrainModel() {
    return getActiveStrain().identifier;
  }

  public void setStrainModel(String value) {
    if (subordinateField[getStrainID()] == null || !getStrainModel().equals(value)) {
      setsubordinateField(getStrainID(), value);
      refreshReflectionv = true;
      refreshSpaceGroup = true; // we force rebuilding of the reflection list
      getActiveTexture().refreshComputation = true;
      getActiveStrain().refreshComputation = true;
    }
  }

  public void setStrainModel(int number) {
    setStrainModel(getsubordIdentifier(getStrainID(), number));
  }

  public int getStrainID() {
    return 6;
  }

  public int strainModelsNumber() {
    return getsubordClassNumber(getStrainID());
  }

  public Strain getActiveStrain() {
    if (subordinateField[getStrainID()] == null)
      setStrainModel(0);
    return (Strain) subordinateField[getStrainID()];
  }

  public boolean extractPositions() {
    return extractPositions;
  }

/*	public String getStressModel() {
		return getActiveStress().identifier;
	}

	public void setStressModel(String value) {
  	if (subordinateField[getStressID()] == null || !getStressModel().equals(value))
			setsubordinateField(getStressID(), value);
	}

	public void setStressModel(int number) {
		setStressModel(getsubordIdentifier(getStressID(), number));
	}

	public int getStressID() {
		return 7;
	}

  public int stressModelsNumber() {
  	return getsubordClassNumber(getStressID());
  }

  public Stress getActiveStress() {
  	if (subordinateField[getStressID()] == null)
  		setStressModel(0);
		return (Stress) subordinateField[getStressID()];
  }*/

  public StructureFactorModel getStructureFactorModel() {
    return ((StructureFactorModel) getActiveSubordinateModel(structureFactorModelID));
  }

  public StructureModel getActiveStructureModel() {
    return ((StructureModel) getActiveSubordinateModel(structureModelID));
  }

  public TDSModel getActiveTDSModel() {
    return ((TDSModel) getActiveSubordinateModel(tdsModelID));
  }

  /* diffrDataFile, expfit, this, intensity, Fhklist, actualPosition, minmaxindex  */
  private Parameter getCell(int i) {
    CellSymmetry();
    if (i >= 0 && i < 6)
      return parameterField[i];
    else
      return null;
  }

  private void setCell(int i, String avalue) {
    if (!avalue.equals(getCell(i).getValue())) {
      getCell(i).setValue(avalue);
    }
  }

  private void setCell(int i, double avalue) {
    if (avalue != getCell(i).getValueD())
      getCell(i).setValue(avalue);
  }

  public void setCellValue(int i, double avalue) {
    setCell(i, avalue);
  }

  public double getCellValue(int index) {
//    if (index < 6 && index >= 0) {
    return parameterValues[index];
//		} else
//      return 1.0; // avoiding errors
  }

  public double getFullCellValue(int index) {
//    if (index < 6 && index >= 0) {
    return parameterValues[index] * getActivePlanarDefects().getSuperCellFactor(index);
//		} else
//      return 1.0; // avoiding errors
  }

  public double getCellValueMinD(int i) {
    return getCell(i).getValueMinD();  //To change body of created methods use File | Settings | File Templates.
  }

  public double getCellValueMaxD(int i) {
    return getCell(i).getValueMaxD();  //To change body of created methods use File | Settings | File Templates.
  }

  public Parameter getStrain() {
    return parameterField[6];
  }

  public void checkAnglesSum() {
    // only for triclinic
    double alpha = getCellValue(3);
    double beta = getCellValue(4);
    double gamma = getCellValue(5);
    double sum = alpha + beta + gamma;
    if (sum > 360.0) {
      if ((getCell(3).mayRefines() && getCell(4).mayRefines() && getCell(5).mayRefines()) ||
          (!getCell(3).mayRefines() && !getCell(4).mayRefines() && !getCell(5).mayRefines())) {
        setCell(3, 180.0 - alpha);
        setCell(4, 180.0 - beta);
        setCell(5, 180.0 - gamma);
      } else {
        if (getCell(3).mayRefines()) {
          setCell(3, 180.0 - alpha);
        }
        if (getCell(4).mayRefines()) {
          setCell(4, 180.0 - beta);
        }
        if (getCell(5).mayRefines()) {
          setCell(5, 180.0 - gamma);
        }
      }
    }
  }

  int cellParametersNumber = 0;

  public int numberOfCellParameters() {
    CellSymmetry();
    return cellParametersNumber;
  }

  public boolean CellSymmetry() {
    isRefreshingCell = true;
    if (refreshCellSymmetry) {
      refreshCellSymmetry = false;
      String sg = getSpaceGroup();

      ic[0] = 0;
      ic[1] = 0;
      ic[2] = 0;
      ic[3] = 0;
      ic[4] = 0;
      ic[5] = 0;
      cellParametersNumber = 0;
      switch (getNumber(getSymmetry())) {
        case 0:
          ic[0] = 1;
          ic[1] = 1;
          ic[2] = 1;
          ic[3] = 1;
          ic[4] = 1;
          ic[5] = 1;
          cellParametersNumber = 6;
//          checkAnglesSum();
          break;
        case 1:
          ic[0] = 1;
          ic[1] = 1;
          ic[2] = 1;
          ic[4] = 1;
          cellParametersNumber = 4;
          if (sg != null) {
            int cchar = sg.indexOf(':');
            if (cchar > -1) {
              char achar = sg.charAt(cchar + 1);
              if (achar == '-')
                achar = sg.charAt(cchar + 2);
              switch (achar) {
                case 'a':
                case 'A':
                  ic[3] = 1;
                  ic[4] = 0;
                  setCell(4, "90");
                  setCell(5, "90");
                  break;
                case 'c':
                case 'C':
                  ic[5] = 1;
                  ic[4] = 0;
                  setCell(3, "90");
                  setCell(4, "90");
                  break;
                default: {
                  setCell(3, "90");
                  setCell(5, "90");
                }
              }
            }
          } else {
            setCell(3, "90");
            setCell(5, "90");
          }
          break;
        case 2:
          ic[0] = 1;
          ic[1] = 1;
          ic[2] = 1;
          cellParametersNumber = 3;
          setCell(3, "90");
          setCell(4, "90");
          setCell(5, "90");
          break;
        case 3:
          ic[0] = 1;
          ic[2] = 1;
          ic[1] = 2;
          cellParametersNumber = 3;
          setCell(1, getCell(0).getValue());
          setCell(3, "90");
          setCell(4, "90");
          setCell(5, "90");
          break;
        case 4:
          ic[0] = 1;
          ic[2] = 1;
          cellParametersNumber = 2;
          int f = sg.indexOf(':');
          int f1 = -1;
          if (f != -1)
            f1 = sg.toUpperCase().lastIndexOf('R');
          if (f1 > f) {
            ic[2] = 0;
            ic[3] = 1;
            ic[1] = 2;
            ic[2] = 2;
            ic[4] = 5;
            ic[5] = 5;
            setCell(1, getCell(0).getValue());
            setCell(2, getCell(0).getValue());
//        setCell(3, "60");
            setCell(4, getCell(3).getValue());
            setCell(5, getCell(3).getValue());
          } else {
            ic[1] = 2;
            setCell(1, getCell(0).getValue());
            setCell(3, "90");
            setCell(4, "90");
            setCell(5, "120");
          }
          break;
        case 5:
          ic[0] = 1;
          ic[2] = 1;
          ic[1] = 2;
          cellParametersNumber = 2;
          setCell(1, getCell(0).getValue());
          setCell(3, "90");
          setCell(4, "90");
          setCell(5, "120");
          break;
        case 6:
          ic[0] = 1;
          ic[1] = 2;
          ic[2] = 2;
          cellParametersNumber = 1;
          setCell(1, getCell(0).getValue());
          setCell(2, getCell(0).getValue());
          setCell(3, "90");
          setCell(4, "90");
          setCell(5, "90");
          break;
        default: {
          isRefreshingCell = false;
          refreshCellSymmetry = false;
          return false;
        }
      }
      isRefreshingCell = false;
      refreshCellSymmetry = false;
      return true;
    }
    isRefreshingCell = false;
    refreshCellSymmetry = false;
    return false;
  }

  double cdsc[] = new double[8];

  public final double[] lattice() {
/* Local variables */
    double arg;
    double cella, cellb, cellc, alpha23, alpha31, alpha12;

/*     INPUT : Lattice parameters necessary for Theta,Phi calculation
 *     of (HKL)-directions */

    cella = getFullCellValue(0);
    cellb = getFullCellValue(1);
    cellc = getFullCellValue(2);
    alpha12 = getFullCellValue(5);
    alpha23 = getFullCellValue(3);
    alpha31 = getFullCellValue(4);

    int igbl = SpaceGroups.getLGNumberSiegfriedConv(getPointGroup());

    cdsc[0] = cellc / cella;
    cdsc[1] = cellc / cellb;
    switch (igbl) {
      case 1:
        arg = alpha12 * Constants.DEGTOPI;
        cdsc[2] = Math.sin(arg);
        cdsc[3] = Math.cos(arg);
        arg = alpha23 * Constants.DEGTOPI;
        cdsc[4] = Math.sin(arg);
        cdsc[5] = Math.cos(arg);
        arg = alpha31 * Constants.DEGTOPI;
        cdsc[6] = Math.sin(arg);
        cdsc[7] = Math.cos(arg);
        break;
      case 2:
        arg = alpha12 * Constants.DEGTOPI;
        cdsc[2] = Math.sin(arg);
        cdsc[3] = Math.cos(arg);
        cdsc[4] = 1.;
        cdsc[5] = 0.;
        cdsc[6] = 1.;
        cdsc[7] = 0.;
        break;
      case 3:
        cdsc[2] = 1.;
        cdsc[3] = 0.;
        cdsc[4] = 1.;
        cdsc[5] = 0.;
        cdsc[6] = 1.;
        cdsc[7] = 0.;
        break;
      case 4:
      case 5:
        cdsc[2] = 1.;
        cdsc[3] = 0.;
        cdsc[4] = 1.;
        cdsc[5] = 0.;
        cdsc[6] = 1.;
        cdsc[7] = 0.;
        cdsc[1] = cdsc[0];
        break;
      case 6:
      case 7:
        cdsc[2] = 1.;
        cdsc[3] = 0.;
        cdsc[4] = 1.;
        cdsc[5] = 0.;
        cdsc[6] = 1.;
        cdsc[7] = 0.;
        cdsc[0] = 1.;
        cdsc[1] = 1.;
        break;
      case 8:
      case 9:
      case 10:
      case 11:
        cdsc[2] = Constants.sinArg2PIover3;
        cdsc[3] = Constants.cosArg2PIover3;
        cdsc[4] = 1.;
        cdsc[5] = 0.;
        cdsc[6] = 1.;
        cdsc[7] = 0.;
        cdsc[1] = cdsc[0];
        break;
      default: {
      }
    }
    return cdsc;
  } /* lattice */

  public boolean isRhombohedral() {
    String sg = getSpaceGroup();

    switch (getNumber(getSymmetry())) {
      case 4:
        if (sg.indexOf('r') > 2 || sg.indexOf('R') > 2)
          return true;
        break;
      default: {
      }
    }
    return false;
  }

  public int getMonoclinicAxis() {
    String sg = getSpaceGroup();

    switch (getNumber(getSymmetry())) {
      case 1:
        if (sg != null) {
          int cchar = sg.indexOf(':');
          if (cchar > -1) {
            char achar = sg.charAt(cchar + 1);
            if (achar == '-')
              achar = sg.charAt(cchar + 2);
            switch (achar) {
              case 'a':
              case 'A':
                return 1;
              case 'c':
              case 'C':
                return 3;
              default: {
                return 2;
              }
            }
          }
        }
        break;
      default: {
      }
    }
    return 2;
  }

  public void sghklcompute(boolean forceRange) {
    if (refreshReflectionv) {
//      System.out.println("compute " + refreshReflectionv);
      refreshReflectionv = false;
      refreshFhklcomp = true;
      refreshCrystMicrostrain = true;
      refreshPositions = true;

      double dspacemin = getSample().getDspacingMin();
      double dspacemax = getSample().getDspacingMax();
      if (dspacemin > dspacemax || forceRange) {
        if (dspacemin > lowReflnDspace)
          dspacemin = lowReflnDspace;
        if (dspacemax < highReflnDspace)
          dspacemax = highReflnDspace;
      } else {
        dspacemin *= 0.95;
        dspacemax *= 1.1;
        if (lowDspace > dspacemin)
          dspacemin = lowDspace;
        if (highDspace < dspacemax)
          dspacemax = highDspace;
      }

      long previousTime = 0;
      if (Constants.testtime)
        previousTime = System.currentTimeMillis();

//      System.out.println(getPhaseName() + ", refresh peak list " + reflectionv.size() + " : " + getNumberOfCustomPeaks());
      if (getNumberOfCustomPeaks() > 0)
        updateReflectionsFromInternalList(reflectionv);
      else {
	      if (SpaceGroups.useCCTBX())
		      checkSghkllist(getSpaceGroup(), dspacemin);
	      else
	        checkSghkllist(getSpaceGroup(), getSGconv(), dspacemin, dspacemax);
      }
      getActivePlanarDefects().checkhklListForPlanarDefects(reflectionv, getClosePackedType());
      if (Constants.testtime)
        System.out.println("Reflection list computation for phase " + getLabel() + ": " +
            (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");
      Collections.sort(reflectionv, new dComparer());
      if (Constants.testtime)
        System.out.println("Reflection list sorting for phase " + getLabel() + ": " +
            (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");

      hklsupordercomp();
//      setDataIndices();
//      System.out.println(this.getPhaseName() + ", refreshed peak list " + reflectionv.size());

      notifySubordinateReflectionListChanged();

    }
  }

  public void notifySubordinateReflectionListChanged() {
//    FilePar filepar = getFilePar();
//    if (filepar != null && !filepar.isLoadingFile() && isAbilitatetoRefresh) {
	  Object[] childrens = getObjectChildren();
    int numberOfChildrens = childrens.length;
    for (int i = 0; i < numberOfChildrens; i++) {
      if (childrens[i] instanceof ReflectionListDelegate)
        ((ReflectionListDelegate) childrens[i]).reflectionListHasChanged();
    }

//    }
  }

	public void checkReflectionsInRange() {
		FilePar aparFile = getFilePar();
		Sample asample = getSample();

		if (!aparFile.isComputingDerivate() && !getFilePar().isOptimizing()) {

			int numberDataSets = asample.activeDatasetsNumber();

			for (int k = 0; k < gethklNumber(); k++) {
				Reflection refl = getReflex(k);
//		  System.out.println("Checking reflection: " + refl.h + " " + refl.k + " "  + refl.l);
				boolean isInRange = false;
				for (int i = 0; i < numberDataSets && !isInRange; i++) {
					DataFileSet dataset = asample.getActiveDataSet(i);
					if (dataset != null) {
						for (int j = 0; j < dataset.activedatafilesnumber() && !isInRange; j++) {
							boolean check = dataset.getActiveDataFile(j).checkinRangeandIntensity(this, k);
							if (check)
								isInRange = true;
						}
					}
				}
				refl.goodforStructureFactor = isInRange;
				refl.goodforStrain = isInRange;
				refl.goodforTexture = isInRange;
			}
		}

	}

  public int getNumberOfCustomPeaks() {
    return numberofelementSubL(1);
  }

  private void updateReflectionsFromInternalList(Vector<Reflection> reflectionList) {
    int nop = getNumberOfCustomPeaks();
    int actualNumber = reflectionList.size();
    if (nop > actualNumber) {
      int M = 2;
      int M2 = M / 2;
      int[] jhlist = new int[M2];
      int[] jklist = new int[M2];
      int[] jllist = new int[M2];
      for (int i = 0; i < M2; i++) {
        jhlist[i] = 0;
        jklist[i] = 0;
        jllist[i] = 0;
      }
      for (int ir = actualNumber; ir < nop; ir++) {
        Reflection refl = new Reflection(this, jhlist, jklist, jllist, M, 0.0);
        reflectionList.addElement(refl);
      }
    } else if (nop < actualNumber) {
      for (int ir = actualNumber - 1; ir >= nop; ir--) {
        reflectionList.removeElementAt(ir);
      }
    }
    for (int ir = 0; ir < nop; ir++) {
      Reflection refl = reflectionList.elementAt(ir);
      Reflex reflx = (Reflex) subordinateloopField[1].elementAt(ir);
      refl.setDSpace(reflx.getDspacing().getValueD());
      refl.structureFactor = reflx.getIntensity().getValueD();
	    refl.crystallite = reflx.getDomain().getValueD();
	    refl.microstrain = reflx.getMicrostrain().getValueD();
//	    System.out.println(ir + " " + sf);
/*      refl.setStructureFactor(sf * sf);
      refl.setCrystallite(reflx.getDomain().getValueD());
      refl.setMicrostrain(reflx.getMicrostrain().getValueD());*/
    }
//	  System.out.println("Refresh peak list n " + nop);
//    refreshCrystMicrostrain = false;
  }

  public void setCustomPeakList(double[] peakPositions) {
    int peaksNumber = peakPositions.length;
//    System.out.println("Added " + peaksNumber + " peaks!");
    removePeakList();
    reflectionv.removeAllElements();
//    absentreflectionv.removeAllElements();
    for (int i = 0; i < peaksNumber; i++) {
      Reflex refl = new Reflex(this, "Peak_" + Integer.toString(i + 1));
      refl.getDspacing().setValue(peakPositions[i]);
      refl.getIntensity().setValue(100);
      refl.getDomain().setValue(getMeanCrystallite());
      refl.getMicrostrain().setValue(getMeanMicrostrain());
      subordinateloopField[1].addItem(refl);
    }
  }

  public ListVector getCustomPeaksList() {
    return getList(1);
  }

  public double[] getCustomPeakPositions() {
    int nop = getNumberOfCustomPeaks();
    double[] peakPositions = new double[nop];
    for (int ir = 0; ir < nop; ir++) {
      Reflex reflx = (Reflex) getCustomPeaksList().elementAt(ir);
      peakPositions[ir] = reflx.getDspacing().getValueD();
    }
    return peakPositions;
  }

  public double[][] getCustomPeakPositionsAndErrors() {
    int nop = getNumberOfCustomPeaks();
    double[][] peakPositions = new double[2][nop];
    for (int ir = 0; ir < nop; ir++) {
      Reflex reflx = (Reflex) getCustomPeaksList().elementAt(ir);
      peakPositions[0][ir] = reflx.getDspacing().getValueD();
      peakPositions[1][ir] = Double.parseDouble(reflx.getDspacing().getError()) * 3.0;
    }
    return peakPositions;
  }

  public void removePeakList() {
    subordinateloopField[1].removeAllItems();
/*    int nop = getNumberOfCustomPeaks();
//    System.out.println("Removing " + nop + " peaks!");
    for (int index = nop - 1; index >= 0; index--)
      removeSubLField(1, index);*/
    refreshAll(false);
  }

  public void customPeaksRefinable() {
    int nop = getNumberOfCustomPeaks();
    for (int ir = 0; ir < nop; ir++) {
      Reflex reflx = (Reflex) subordinateloopField[1].elementAt(ir);
      reflx.freeAllParameters();
    }
  }

  public void customPeaksHeightRefinable() {
    int nop = getNumberOfCustomPeaks();
    for (int ir = 0; ir < nop; ir++) {
      Reflex reflx = (Reflex) subordinateloopField[1].elementAt(ir);
      reflx.getIntensity().setRefinable();
    }
  }

  public void customPeaksNotRefinable() {
    int nop = getNumberOfCustomPeaks();
    for (int ir = 0; ir < nop; ir++) {
      Reflex reflx = (Reflex) subordinateloopField[1].elementAt(ir);
      reflx.fixAllParameters();
    }
  }

  public void customPeaksBoundSizeStrain() {
    SizeStrainSymModel sizestrain = getActiveSizeStrainSym();
    if (sizestrain instanceof SizeStrainSymIso) {
      int nop = getNumberOfCustomPeaks();
      for (int ir = 0; ir < nop; ir++) {
        Reflex reflx = (Reflex) subordinateloopField[1].elementAt(ir);
        reflx.getDomain().setEqualTo(((SizeStrainSymIso) sizestrain).getCrystalliteSize(), 1.0, 0.0);
        reflx.getMicrostrain().setEqualTo(((SizeStrainSymIso) sizestrain).getMicrostrain(), 1.0, 0.0);
      }
    }
  }

  public void checkCrystalliteAndMicrostrain() {
    getActiveSizeStrainSym().correctCrystalliteAndMicrostrain();
  }

  public void computehklSizeStrain() {
    sghklcompute(false);
    if (refreshCrystMicrostrain) {
	    if (getNumberOfCustomPeaks() == 0) {
      getActiveSizeStrainSym().applySymmetryRules();
      int cpType = getClosePackedType();
      if (getActivePlanarDefects() != null)
        getActivePlanarDefects().prepareComputation(cpType);
	    }
      refreshCrystMicrostrain = false;
    }
  }

  public void printCustomInformations(OutputStream out) throws IOException {
    // to be implemented by subclasses
	  getPhaseInfo().printCustomInformations(out);

    printLine(out, "       AtomSite list");
    printLine(out, "n  label  symbol  quantity  occupancy  x  y  z  multiplicity  B  radius  weight  neutron " +
        "scattering  neutron absorption");
    int totalNumber = fullAtomList.size();
    for (int i = 0; i < totalNumber; i++) {
      AtomSite ato = fullAtomList.get(i);
      printLine(out, (i + 1) + ") " + ato.getInformationString());
    }
    newLine(out);

	  printLine(out, "       Atomic fractions");
	  printLine(out, "n  label  atomic fraction");
	  Vector<AtomQuantity> composition = getChemicalComposition();
	  double[] atomicFractions = new double[composition.size()];
	  double totalWeight = 0;
	  for (int i1 = 0; i1 < composition.size(); i1++) {
		  atomicFractions[i1] = composition.elementAt(i1).quantity;
		  totalWeight += atomicFractions[i1];
	  }
	  for (int i1 = 0; i1 < composition.size(); i1++) {
		  atomicFractions[i1] /= totalWeight;
		  printLine(out, (i1 + 1) + ") " + composition.elementAt(i1).label + " " + atomicFractions[i1]);
	  }
	  newLine(out);

	  printLine(out, "       Reflection list");
    printLine(out, "n  h  k  l  multiplicity  d-space  crystallite(Angstrom)  microstrain");
    int hkln = gethklNumber();
    for (int i = 0; i < hkln; i++) {
      Reflection refl = reflectionv.elementAt(i);
      printLine(out, (i + 1) + ") " + refl.getInformationString());
    }
    newLine(out);
  }

/*  public void addhkl(int h, int k, int l, int multiplicity, double dspacing) {
  	reflectionv.addElement(new Reflection(this, h,k,l,multiplicity,dspacing));
  }

  public void addhkl(int[] hlist, int[] klist, int[] llist, int multiplicity, double dspacing) {
  	reflectionv.addElement(new Reflection(this, hlist, klist, llist, multiplicity, dspacing));
  }*/

  public void hklsupordercomp() {
    for (int i = 0; i < reflectionv.size(); i++) {
    }
  }

  public AtomSite getAtom(int index) {
    return (AtomSite) getAtomList().elementAt(index);
  }

  public void refreshFhklcompv() {
	  refreshSpaceGroup = true;
    refreshsgxyz();
    refreshAtoms();
    refreshSubordinateStructureModels();
    refreshEnergyComputation = true;
  }

  private void refreshSubordinateStructureModels() {
    ((StructureFactorModel) getActiveSubordinateModel(structureFactorModelID)).
        refreshStructureFactor();
  }

  public void refreshsgxyz() {
	  if (refreshSpaceGroup)
		  refreshSpaceGroup = refreshSpaceGroupComputation(getSpaceGroup(), getSGconv());
  }

  public void refreshAtoms() {
//	  System.out.println("Refresh atoms: " + refreshAtoms);
    if (refreshAtoms) {
      fullAtomList = new Vector<AtomSite>(0, 1);
      fullAtomList.addAll(getAtomList());
      fullAtomList.addAll(getActiveStructureModel().getFullAtomList());
	    refreshOccupancyAndQuantity();
	    for (int i = 0; i < fullAtomList.size(); i++)
		    fullAtomList.get(i).setRefreshPosition(true);
      refreshAtoms = false;
    }
  }

  public void deleteAtomFromFullList(int i) {
    if (i >= 0 && i < getAtomList().size())
      getAtomList().removeItemAt(i);
    else
      getActiveStructureModel().deleteAtomFromFullList(i - getAtomList().size());
  }

  public void checkAtomPositions() {
    refreshsgxyz();
    for (int i = 0; i < getFullAtomList().size(); i++) {
      ((AtomSite) getFullAtomList().get(i)).collapsePositions();
    }
    refreshOccupancyAndQuantity();
//    getActiveStructureModel().checkAtomPositions();
  }

  public void mergeEquivalentAtoms() {
    refreshsgxyz();
    for (int i = 0; i < getFullAtomList().size(); i++) {
      AtomSite ato1 = ((AtomSite) getFullAtomList().get(i));
      Vector found = new Vector(10, 10);
      for (int j = i + 1; j < getFullAtomList().size(); j++) {
        AtomSite ato2 = getFullAtomList().get(j);
        if (ato1.equalsByDistance(ato2))
          found.add(new Integer(j));
      }
      if (found.size() > 0)
        for (int j = found.size() - 1; j >= 0; j--)
          deleteAtomFromFullList(((Integer) found.get(j)));
    }
    refreshAtoms = true;
    refreshOccupancyAndQuantity();
    refreshAtoms();
  }

  public void refreshOccupancyAndQuantity() {
    for (int i = 0; i < getFullAtomList().size(); i++) {
	    getFullAtomList().get(i).setRefreshPosition(true);
      getFullAtomList().get(i).refreshPositions(false);
      getFullAtomList().get(i).refreshOccupancyAndQuantity();
    }
    refreshAtoms = false;
    refreshFhklcomp = true;
//    getActiveStructureModel().refreshOccupancyAndQuantity();
  }

	public void normalizeOccupancies() {
		refreshOccupancyAndQuantity();
		double[] totalOccupancy = new double[getFullAtomList().size()];
		for (int i = 0; i < getFullAtomList().size(); i++) {
			if (totalOccupancy[i] == 0) {
				AtomSite atom = getFullAtomList().get(i);
				totalOccupancy[i] = atom.getOccupancyValue();
				for (int j = i + 1; j < getFullAtomList().size(); j++) {
					AtomSite atomj = getFullAtomList().get(j);
					if (atom.shareSiteWith(atomj)) {
						totalOccupancy[i] += atomj.getOccupancyValue();
						totalOccupancy[j] = -999;
					}
				}
			}
		}
		double maxOccupancy = 0;
		for (int i = 0; i < getFullAtomList().size(); i++) {
			if (totalOccupancy[i] > maxOccupancy)
				maxOccupancy = totalOccupancy[i];
		}
		System.out.println("Normalizing all atom occupancies by: " + maxOccupancy);
		for (int i = 0; i < getFullAtomList().size(); i++) {
			AtomSite atom = getFullAtomList().get(i);
			atom.getOccupancy().setValue(atom.getOccupancyValue() / maxOccupancy);
			atom.computeQuantityFromOccupancy();
		}

	}

	public double getScaleFactor() {
    return parameterValues[scaleFactorID];
  }

  public Vector<AtomSite> getFullAtomList() {
    if (fullAtomList == null)
      refreshAtoms();
    return fullAtomList;
  }

  // Used by StructureSolution (Genetic et al.)
/*
  public double Fhklcomp(int h, int k, int l, double dspacing, int radType, int tubeNumber) {
    double factors = getActivePlanarDefects().getStructureFactorModifier(h, k, l);
    double[] divideFactors = getActivePlanarDefects().getDivisionFactors();
    double a1 = 0.0;
    double a2 = 0.0;
    double i_dspace = 0.0;
	  if (dspacing != 0.0)
		  i_dspace = 0.25 / (dspacing * dspacing);
    for (int j = 0; j < getFullAtomList().size(); j++) {
      AtomSite ato = getFullAtomList().get(j);
      if (ato.useThisAtom) {
        double[] scatf = ato.scatfactor(dspacing, radType, tubeNumber);
        double DWfactor = Math.exp(-ato.getBfactorValue() * i_dspace);
        //ato.DebyeWaller(h, k, l, dspacing);
        double scatFactor = DWfactor * ato.getQuantityD() / ato.getSiteMultiplicity();
        scatf[0] *= scatFactor;
        scatf[1] *= scatFactor;
        for (int ix = 0; ix < ato.getSiteMultiplicity(); ix++) {
          double[] x = ato.getCoordinates(ix);
          double arg = 2.0 * Constants.PI * (h * x[0] * divideFactors[0] + k * x[1] * divideFactors[1] +
              l * x[2] * divideFactors[2]);
          double w1 = Math.cos(arg);
          double w2 = Math.sin(arg);
          a1 += scatf[0] * w1 - scatf[1] * w2;
          a2 += scatf[0] * w2 + scatf[1] * w1;
        }
      }
    }
	  double structureFactor = (a1 * a1 + a2 * a2) * factors;
    return structureFactor;
  }*/

  public double getAtomMapNormalization() {
    int nAtom = getFullAtomList().size();
    if (nAtom <= 0)
      return 1.0;
    double atomScat = 0.0;
    for (int j = 0; j < nAtom; j++) {
      AtomSite ato = getFullAtomList().get(j);
      ato.refreshPositions(false);
      ato.refreshOccupancyAndQuantity();
      atomScat += ato.xrayscatfactor() * ato.getQuantityD();
    }
//    System.out.println("Zelectron = " + atomScat);
    return atomScat;// + a;
  }

  public double getAtomScatteringCellNormalization() {
    int nAtom = getFullAtomList().size();
    if (nAtom <= 0)
      return 1.0;
    double atomScat = 0.0;
    for (int j = 0; j < nAtom; j++) {
      AtomSite ato = (AtomSite) getFullAtomList().get(j);
      ato.refreshPositions(false);
      ato.refreshOccupancyAndQuantity();
      atomScat += ato.xrayscatfactor() * ato.getQuantityD();
    }
//    System.out.println("Zelectron = " + atomScat);
    return atomScat;// + a;
  }

  public double[] getStructureParamsLBound() { // to be ported to the new model
    int nAtom = getFullAtomList().size();
    int paramsNumber = nAtom * 3;
    double[] lbound = new double[paramsNumber];
    int np = 0;
    for (int j = 0; j < nAtom; j++) {
      lbound[np++] = 0.0;
      lbound[np++] = 0.0;
      lbound[np++] = 0.0;
    }
    return lbound;
  }

  public double[] getStructureParamsUBound() { // to be ported to the new model
    int nAtom = getFullAtomList().size();
    int paramsNumber = nAtom * 3;
    double[] ubound = new double[paramsNumber];
    int np = 0;
    for (int j = 0; j < nAtom; j++) {
      ubound[np++] = reducedCellFactor[0];
      ubound[np++] = reducedCellFactor[1];
      ubound[np++] = reducedCellFactor[2];
    }
    return ubound;
  }

  public double[] getStructureParams() { // to be ported to the new model
    int nAtom = getFullAtomList().size();
    int paramsNumber = nAtom * 3;
    double[] params = new double[paramsNumber];
    int np = 0;
    for (int j = 0; j < nAtom; j++) {
      AtomSite ato = (AtomSite) getFullAtomList().get(j);
      if (ato.useThisAtom) {
        params[np++] = ato.getLocalCoordX().getValueD();
        params[np++] = ato.getLocalCoordY().getValueD();
        params[np++] = ato.getLocalCoordZ().getValueD();
      }
    }
    return params;
  }

  public void setStructureParams(double[] params) { // to be ported to the new model
    int nAtom = getFullAtomList().size();
    int np = 0;
    for (int j = 0; j < nAtom; j++) {
      AtomSite ato = (AtomSite) getFullAtomList().get(j);
      if (ato.useThisAtom) {
        ato.getLocalCoordX().setValue(params[np++]);
        ato.getLocalCoordY().setValue(params[np++]);
        ato.getLocalCoordZ().setValue(params[np++]);
      }
    }
    refreshAtoms();
  }

  public void extractStructureFactors(Sample asample) {
    ((StructureFactorExtractor) getActiveSubordinateModel(structureFactorExtractorID)).
        extractStructureFactors(asample);
  }

  public void computeStructureFactors(Sample asample, boolean structureSolution) {
    ((StructureFactorModel) getActiveSubordinateModel(structureFactorModelID)).
        computeStructureFactors(asample, structureSolution);
    refreshFhklcomp = false;
  }

  double energyLevel = 0.0;

  public void computeEnergy() {
    if (refreshEnergyComputation)
      energyLevel = getActiveStructureModel().computeEnergy() * getActiveStructureModel().getEnergyWeight();
    refreshEnergyComputation = false;
  }

  public double getEnergyLevel() {
    return energyLevel;
  }

  public boolean solveCrystalStructure() {
    if (!solveStructure())  // the Structure Solution method is a fake one
      return false;
//		if (reflectionv == null) {
    refreshReflectionv = true;
    sghklcompute(false); // forcing to build a reflection list
//		}
	  ((StructureFactorModel) getActiveSubordinateModel(structureFactorModelID)).solveStructure();
	  return true;
  }

/*  public StructureSolutionMethod getStructureSolutionMethod() {
    return (StructureSolutionMethod) getActiveSubordinateModel(structureSolutionMethodID);
  }*/

  public int gethklNumber() {
//	  System.out.println("Before " + getPhaseName() + " " + reflectionv.size());
    sghklcompute(false);
//	  System.out.println("Reflections number for " + getPhaseName() + ": " + reflectionv.size());
    return reflectionv.size();
  }

/*  public void setReflectionNumber(int newnumber) {
    int oldnumber = gethklNumber();
    for (int i = oldnumber - 1; i >= newnumber; i--)
      reflectionv.removeElementAt(i);
  }*/

  public Reflection getReflectionByAnyhkl(int h, int k, int l) {
    Reflection refl;

    int hklnumber = gethklNumber();
    for (int i = 0; i < hklnumber; i++) {
      refl = reflectionv.elementAt(i);
      int mult2 = refl.multiplicity / 2;
      if (mult2 == 0)
        mult2 = 1;
      for (int j = 0; j < mult2; j++) {
        if ((refl.hlist[j] == h && refl.klist[j] == k && refl.llist[j] == l) ||
            (refl.hlist[j] == -h && refl.klist[j] == -k && refl.llist[j] == -l))
          return refl;
      }
    }
    return null;
  }

	public int getReflectionIndexByAnyhkl(int h, int k, int l) {
		Reflection refl;

		int hklnumber = gethklNumber();
		for (int i = 0; i < hklnumber; i++) {
			refl = reflectionv.elementAt(i);
			int mult2 = refl.multiplicity / 2;
			if (mult2 == 0)
				mult2 = 1;
			for (int j = 0; j < mult2; j++) {
				if ((refl.hlist[j] == h && refl.klist[j] == k && refl.llist[j] == l) ||
						(refl.hlist[j] == -h && refl.klist[j] == -k && refl.llist[j] == -l))
					return i;
			}
		}
		return -1;
	}

  public Reflection getReflectionByhkl(int h, int k, int l) {
    Reflection refl;

    int hklnumber = gethklNumber();
    for (int i = 0; i < hklnumber; i++) {
      refl = reflectionv.elementAt(i);
      if (refl.getH() == h && refl.getK() == k && refl.getL() == l)
        return refl;
    }
    return null;
  }

  public int geth(int index) {
    sghklcompute(false);
    return (reflectionv.elementAt(index)).getH();
  }

  public int getk(int index) {
    sghklcompute(false);
    return (reflectionv.elementAt(index)).getK();
  }

  public int getl(int index) {
    sghklcompute(false);
    return (reflectionv.elementAt(index)).getL();
  }

  public boolean isTextureActive(int index) {
    sghklcompute(false);
    try {
      return (reflectionv.elementAt(index)).poleFigurePlot;
    } catch (Exception e) {
      return false;
    }
  }

  public void setTextureActive(int index, boolean status) {
    sghklcompute(false);
    try {
      (reflectionv.elementAt(index)).poleFigurePlot = status;
    } catch (Exception e) {
//      return;
    }
  }

  public Vector sortDicVol91Result(DicVol91Result result) {
//    Vector sgnameList = new Vector(0, 1);
    int totalPresent, totalMissing;//, notFound;

    String tmpSymmetry = result.symmetry;
    System.out.println("Import dicvol, sorting space groups for symmetry: " + tmpSymmetry);
    double wavelength = result.getWavelength();
    int sgconv = getSGconv();
    int numberReflectionIndexed = result.dspaceList.size();
//    System.out.println(numberReflectionIndexed);
//    int[][] hklList = new int[3][numberReflectionIndexed];
    double[] dspceList = new double[numberReflectionIndexed];
    double[] errorList = new double[numberReflectionIndexed];
    boolean[] found = new boolean[numberReflectionIndexed];
    double dspacemin = result.dspacemin;
    double dspacemax = result.dspacemax;
    if (wavelength != -1.0) {
      dspacemin = 1.0E50;
      dspacemax = -1.0E10;
    }
    double errorDspace = MaudPreferences.getDouble("dspacing.maxRelativeError", 0.001);
    for (int i = 0; i < numberReflectionIndexed; i++) {
//      int[] hklTmp = (int[]) result.reflexList.elementAt(i);
//      hklList[0][i] = hklTmp[0];
//      hklList[1][i] = hklTmp[1];
//      hklList[2][i] = hklTmp[2];
      double[] dspaceTmp = (double[]) result.dspaceList.elementAt(i);
      dspceList[i] = dspaceTmp[0];
      errorList[i] = dspaceTmp[1];
      if (wavelength > 0.0) {
        errorList[i] = wavelength / (2.0 * MoreMath.sind((dspceList[i] + errorList[i]) / 2.0));
        dspceList[i] = wavelength / (2.0 * MoreMath.sind(dspceList[i] / 2.0));
        errorList[i] = dspceList[i] - errorList[i];
      } else {
	      errorList[i] *= dspceList[i] * 3; // to fix the too small error
      }
      if (dspceList[i] < dspacemin)
        dspacemin = dspceList[i];
      if (dspceList[i] > dspacemax)
        dspacemax = dspceList[i];
      if (errorDspace < errorList[i])
        errorDspace = errorList[i];
    }
    Vector spaceGroupList = new Vector(0, 1);
    do {
      setSymmetry(tmpSymmetry);

      String sgname;
      int istart = SpaceGroups.getBeginSG(tmpSymmetry, sgconv);
      int iend = SpaceGroups.getEndSG(tmpSymmetry, sgconv);

      for (int k = istart; k <= iend; k++) {
        sgname = getSpaceGroup(k, sgconv);
        int[] sgObject = new int[5];
        sgObject[0] = getNumber(tmpSymmetry);
        sgObject[1] = k;

        for (int i = 0; i < 6; i++)
          if (result.cell[i] != null)
            setCell(i, result.cell[i]);
        refreshSpaceGroup = true;
        refreshCellVolume = true;
        refreshPositions = true;
        Vector<Reflection> aList = sghkllist(sgname, sgconv, dspacemin - errorDspace,
            dspacemax + errorDspace, false);
//    System.out.println((dspacemin - errorDspace) + " " + (dspacemax + errorDspace));
//    System.out.println(aList.size());
        totalPresent = 0;
        totalMissing = 0;
        for (int i = 0; i < numberReflectionIndexed; i++)
          found[i] = false;
        for (int i = 0; i < aList.size(); i++) {
          Reflection refl = aList.elementAt(i);
          boolean present = false;
          for (int j = 0; j < numberReflectionIndexed; j++) {
//    System.out.println(refl.d_space + " =? " + dspceList[j] + " +- " + errorList[j]);
            if (refl.similarsTo(dspceList[j], errorList[j])) {
              present = true;
              found[j] = true;
              break;
            }
          }
          if (present)
            totalPresent++;
          else
            totalMissing++;
        }
        int notCorresponding = 0;
        for (int i = 0; i < numberReflectionIndexed; i++)
          if (!found[i])
            notCorresponding++;

        sgObject[2] = notCorresponding;
        sgObject[3] = totalMissing;
        sgObject[4] = totalPresent;
        spaceGroupList.add(sgObject);
      }

      if (tmpSymmetry.equalsIgnoreCase(cs[5]))
        tmpSymmetry = cs[4];
      else
        tmpSymmetry = "end";

    } while (!tmpSymmetry.equals("end"));
    Collections.sort(spaceGroupList, new figureOfMerit());
//    Enumeration enumeration = Collections.enumeration(spaceGroupList);

    for (Enumeration e = spaceGroupList.elements(); e.hasMoreElements();) {
      int[] spsg = (int[]) e.nextElement();
      String sgname = SpaceGroups.getSpaceGroup(spsg[1], sgconv);
      System.out.println("Symmetry: " + cs[spsg[0]] + ", Space group: " + sgname +
          ", Not present: " + spsg[2] + ", extra: " + spsg[3] + ", totalIndexed: " + spsg[4]);
    }
    return spaceGroupList;
  }

  public Reflection getReflex(int index) {
    sghklcompute(false);
    return reflectionv.elementAt(index);
  }

  public int getMultiplicity(int index) {
    sghklcompute(false);
    return (reflectionv.elementAt(index)).multiplicity;
  }

  public double getDspacing(int index) {
    sghklcompute(false);
    return (reflectionv.elementAt(index)).d_space;
  }

  public double getCrystallite(int index) {
    boolean oldRefresh = refreshCrystMicrostrain;
    computehklSizeStrain();
    refreshCrystMicrostrain = oldRefresh;
    return getCrystalliteMicrostrain(getReflex(index))[0];
  }

  public double getMicrostrainD(int index) {
    boolean oldRefresh = refreshCrystMicrostrain;
    computehklSizeStrain();
    refreshCrystMicrostrain = oldRefresh;
    return getCrystalliteMicrostrain(getReflex(index))[1];
  }

/*  public double[] getCrystalliteDistribution(int index) {
    return null;
  }

  public double[] getMicrostrainDistribution(int index) {
    return null;
  }

  public int[] getDistributionCoord() {
    return null;
  }

  public int getDistributionSize() {
    return 0;
  }*/

/*  public void setDataIndices() {
    FilePar aparfile = getFilePar();
    aparfile.refreshSampleIndices();
    aparfile.refreshDataIndices();
  }*/

  boolean isRefreshingIndices = false;

	public void refreshIndices(Sample sample) {
		if (isRefreshingIndices)
			return;
//    getFilePar().getActiveSample().resetActiveDatafiles();
		isRefreshingIndices = true;
		int hkln = gethklNumber();
//		for (int i = hkln - 1; i >= 0; i--) {
//			Reflection refl = reflectionv.elementAt(i);
		for (int j = 0; j < sample.activeDatasetsNumber(); j++)
			sample.getActiveDataSet(j).refreshIndices(this);
//      refl.checkinRangeandIntensity(getFilePar().getActiveSample());
//		}
		isRefreshingIndices = false;
	}

/* old indices
	public void setDataIndices(int numberOfDatafiles, int numberdatasets, int radNumber) {
    if (isRefreshingIndices)
      return;
//    getFilePar().getActiveSample().resetActiveDatafiles();
    isRefreshingIndices = true;
    int hkln = gethklNumber();
    for (int i = hkln - 1; i >= 0; i--) {
      Reflection refl = reflectionv.elementAt(i);
      refl.checkDataFiles(numberOfDatafiles);
      refl.checkDataSets(numberdatasets, radNumber);
//      refl.checkinRangeandIntensity(getFilePar().getActiveSample());
    }
    isRefreshingIndices = false;
  }

  public void resetForRandomTexture(DataFileSet dataFileSet) {
    int hkln = gethklNumber();
    for (int i = hkln - 1; i >= 0; i--) {
      Reflection refl = reflectionv.elementAt(i);
      refl.resetForRandomTexture(dataFileSet);
    }
  }*/

  public void computeTextureFactor(Sample asample) {
//	  System.out.println("hkl number: " + gethklNumber());
	  getActiveTexture().initializeReflexes(asample);
    getActiveTexture().computeTextureFactor(this, asample);
    getActiveTexture().saveTextureFactor(this, asample);
  }

/*  public void expToTextureFactor() {
    int hkln = gethklNumber();
    for (int i = 0; i < hkln; i++) {
      Reflection refl = reflectionv.elementAt(i);
      refl.expToTextureFactor();
    }
  }*/

/*  public double getShapeAbsFactor(int datafile, Reflection reflex) {
    return reflex.getShapeAbsFactor(datafile);
  }

  public double getShapeAbsFactor(int index) {
    return (reflectionv.elementAt(index)).getShapeAbsFactor();
  }*/

/*  public static void updateTextureFactorsAndStrain(
      Vector<Reflection> newreflectionv,
      Vector<Reflection> oldreflectionv) {
    int hkln = newreflectionv.size();
    int hkln1 = oldreflectionv.size();
    int numberofDataFiles = 0;
    int numberDatasets = 0;
	  int radNumber = 0;
    if (hkln1 == 0)
      return;
    for (int i = 0; i < hkln; i++) {
      Reflection refl = newreflectionv.elementAt(i);
      if (i == 0) {
        Reflection oldrefl = oldreflectionv.elementAt(0);
        numberofDataFiles = oldrefl.getNumberDatafiles();
        numberDatasets = oldrefl.numberDataSets;
      }
      boolean found = false;
      for (int j1 = i, j2 = i - 1; (j1 < hkln1 || j2 >= 0) && !found; j1++, j2--) {
        if (j1 < hkln1) {
          Reflection oldrefl = oldreflectionv.elementAt(j1);
          if (refl.equalsTo(oldrefl)) {
            numberofDataFiles = oldrefl.getNumberDatafiles();
            refl.checkDataFiles(numberofDataFiles);
            numberDatasets = oldrefl.numberDataSets;
	          radNumber = oldrefl.radNumber;
            refl.checkDataSets(numberDatasets, radNumber);
            found = true;
            for (int i1 = 0; i1 < numberofDataFiles; i1++) {
              refl.setExpTextureFactor(i1, oldrefl.getExpTextureFactor(i1));
              refl.setTextureFactor(i1, oldrefl.getTextureFactor(i1));
              refl.setExpStrain(i1, oldrefl.getExpStrain(i1));
              refl.setStrain(i1, oldrefl.getStrain(i1));
              refl.setShapeAbsFactor(i1, oldrefl.getShapeAbsFactor(i1));
              refl.setInstBroadFactor(i1, oldrefl.getInstBroadFactor(i1));
              refl.setSampleBroadFactor(i1, oldrefl.getSampleBroadFactor(i1));
              refl.setPosition(i1, oldrefl.getPosition(i1));
              refl.setLorentzPolarization(i1, oldrefl.getLorentzPolarization(i1));
            }
            for (int i1 = 0; i1 < numberDatasets; i1++) {
              refl.setExpStructureFactor(i1, oldrefl.getExpStructureFactor(i1));
              refl.setStructureFactor(i1, oldrefl.getStructureFactor(i1));
              refl.setEsdStructureFactor(i1, oldrefl.getEsdStructureFactor(i1));
              for (int j = 0; j < refl.getParent().getFullAtomList().size(); j++)
                refl.setScatteringFactor(i1, oldrefl.getScatteringFactor(i1, j), j);
            }
            break;
          }
        }
        if (j2 >= 0 && j2 < hkln1 && !found) {
          Reflection oldrefl = oldreflectionv.elementAt(j2);
          if (refl.equalsTo(oldrefl)) {
            numberofDataFiles = oldrefl.getNumberDatafiles();
            refl.checkDataFiles(numberofDataFiles);
            numberDatasets = oldrefl.numberDataSets;
	          radNumber = oldrefl.radNumber;
            refl.checkDataSets(numberDatasets, radNumber);
            found = true;
            for (int i1 = 0; i1 < numberofDataFiles; i1++) {
              refl.setExpTextureFactor(i1, oldrefl.getExpTextureFactor(i1));
              refl.setTextureFactor(i1, oldrefl.getTextureFactor(i1));
              refl.setExpStrain(i1, oldrefl.getExpStrain(i1));
              refl.setStrain(i1, oldrefl.getStrain(i1));
              refl.setShapeAbsFactor(i1, oldrefl.getShapeAbsFactor(i1));
              refl.setInstBroadFactor(i1, oldrefl.getInstBroadFactor(i1));
              refl.setPosition(i1, oldrefl.getPosition(i1));
              refl.setLorentzPolarization(i1, oldrefl.getLorentzPolarization(i1));
            }
            for (int i1 = 0; i1 < numberDatasets; i1++) {
              refl.setExpStructureFactor(i1, oldrefl.getExpStructureFactor(i1));
              refl.setStructureFactor(i1, oldrefl.getStructureFactor(i1));
              refl.setEsdStructureFactor(i1, oldrefl.getEsdStructureFactor(i1));
              for (int j = 0; j < refl.getParent().getFullAtomList().size(); j++)
                refl.setScatteringFactor(i1, oldrefl.getScatteringFactor(i1, j), j);
            }
            break;
          }
        }
      }
      if (!found) {
        refl.checkDataFiles(numberofDataFiles);
        refl.checkDataSets(numberDatasets, radNumber);
      }
    }
  }*/

/*  public static void updateStructureFactors(
      Vector<Reflection> newreflectionv,
      Vector<Reflection> oldreflectionv) {
    int hkln = newreflectionv.size();
    int hkln1 = oldreflectionv.size();
    if (hkln1 == 0)
      return;
    int numberofDataSets = 0;
    for (int i = 0; i < hkln; i++) {
      Reflection refl = newreflectionv.elementAt(i);
      if (i == 0) {
        Reflection oldrefl = oldreflectionv.elementAt(0);
        numberofDataSets = oldrefl.numberDataSets;
      }
      boolean found = false;
      for (int j1 = i, j2 = i - 1; (j1 < hkln1 || j2 >= 0) && !found; j1++, j2--) {
        if (j1 < hkln1) {
          Reflection oldrefl = oldreflectionv.elementAt(j1);
          if (refl.equalsTo(oldrefl)) {
            numberofDataSets = oldrefl.numberDataSets;
            refl.checkDataSets(numberofDataSets);
            found = true;
            for (int i1 = 0; i1 < numberofDataSets; i1++) {
//							System.out.println("refl "+refl+" oldrefl "+oldrefl+" "+oldrefl.getStructureFactor(i1));
              refl.setExpStructureFactor(i1, oldrefl.getExpStructureFactor(i1));
              refl.setStructureFactor(i1, oldrefl.getStructureFactor(i1));
            }
            break;
          }
        }
        if (j2 >= 0 && j2 < hkln1 && !found) {
          Reflection oldrefl = oldreflectionv.elementAt(j2);
          if (refl.equalsTo(oldrefl)) {
            numberofDataSets = oldrefl.numberDataSets;
            refl.checkDataSets(numberofDataSets);
            found = true;
            for (int i1 = 0; i1 < numberofDataSets; i1++) {
              refl.setExpStructureFactor(i1, oldrefl.getExpStructureFactor(i1));
              refl.setStructureFactor(i1, oldrefl.getStructureFactor(i1));
            }
            break;
          }
        }
      }
      if (!found)
        refl.checkDataSets(numberofDataSets);
    }
  }*/

  public void computeStrain(Sample asample) {
    getActiveStrain().computeStrain(this, asample);
// todo: transform this to export only from menu	  getActiveStrain().saveStrainValues(this, asample);
  }

/*  public void setStrain(int datafile, Reflection reflex, double value) {
    reflex.setStrain(datafile, value);
  }

  public double getStrain(int datafile, Reflection reflex) {
    return reflex.getStrain(datafile);
  }

  public double getExpStrain(int datafile, Reflection reflex) {
    return reflex.getExpStrain(datafile);
  }

  public double getStrain(int index) {
//		getActiveStrain().computeStrain();
    return (reflectionv.elementAt(index)).getStrain();
  }

  public void expToStrain() {
    int hkln = gethklNumber();
    for (int i = 0; i < hkln; i++) {
      Reflection refl = reflectionv.elementAt(i);
      refl.expToStrain();
    }
  }*/

  // SgInfo methods start here, pure java

	public PhaseInfo getPhaseInfo() {
		if (phaseInfo == null) {
			phaseInfo = new PhaseInfo();
			phaseInfo.refreshSpaceGroupComputation(getSpaceGroup(), getSGconv());
		}
		return phaseInfo;
	}

  public Vector<Reflection> sghkllist(String SgName, int sgconv, double dplanecut, double dplanestart,
                                      boolean permitAbsent) {

	  Vector<Reflection> reflectionList = new Vector<Reflection>(100, 100);
	  if (SpaceGroups.useCCTBX()) {
// todo
	  } else {
		  int im, M2;
//    char F_Convention;
		  int h, k, l, M; //restriction;
		  int Maxh, Maxk, Maxl;
		  int[] Minh = new int[1], Mink = new int[1], Minl = new int[1];
		  double dpi; //, so12, so23, so13;
//    double 		          parm12, parm22, parm32, w1, so11, so22, so33;
		  int[] hlist = new int[24], klist = new int[24], llist = new int[24];
		  int[] jhlist;
		  int[] jklist;
		  int[] jllist;
		  T_Eq_hkl Eq_hkl = new T_Eq_hkl();
		  int friedelLaw = 1;
		  if (refreshSpaceGroup) {
			  refreshSpaceGroup = refreshSpaceGroupComputation(SgName, sgconv);
//      reflectionList.removeAllElements();
		  }

		  cellVolumeComp();
		  double[] soVector = getSoVector();

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

		  Sghkl sghkl = new Sghkl(getPhaseInfo().SgInfo);

		  sghkl.SetListMin_hkl(friedelLaw, Maxh, Maxk, Maxl, Minh, Mink, Minl);

//    System.out.println("Build reflection list: " + Maxh +" "+Maxk+" "+Maxl + " " +dplanestart + " " +dplanecut);
		  for (h = Minh[0]; h <= Maxh; h++) {
			  for (k = Mink[0]; k <= Maxk; k++) {
				  for (l = Minl[0]; l <= Maxl; l++) {
					  if (sghkl.IsSysAbsent_hkl(h, k, l, null) == 0) {
						  if ((sghkl.IsHidden_hkl(friedelLaw, Minh[0], Mink[0], Minl[0],
								  Maxh, Maxk, Maxl, h, k, l)) == 0) {
							  if (!((h == 0 && k == 0) && l == 0)) {
								  dpi = 1.0 / Math.sqrt(soVector[0] * h * h + soVector[1] * k * k + soVector[2] * l * l + 2.
										  * soVector[5] * h * k + 2. * soVector[3] * k * l + 2. * soVector[4] * h * l);
//                System.out.println("dpi: " + dpi);
								  if (dpi >= dplanecut && dpi <= dplanestart) {
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
									  reflectionList.addElement(new Reflection(this, jhlist, jklist, jllist, M, dpi));
//                  System.out.println("New Reflection " + h +" "+k+" "+l + " " +dpi);
								  }
							  }
						  }
					  } else if (permitAbsent) {
						  if ((sghkl.IsHidden_hkl(friedelLaw, Minh[0], Mink[0], Minl[0],
								  Maxh, Maxk, Maxl, h, k, l)) == 0) {
							  if (!((h == 0 && k == 0) && l == 0)) {
								  dpi = 1.0 / Math.sqrt(soVector[0] * h * h + soVector[1] * k * k + soVector[2] * l * l + 2.
										  * soVector[5] * h * k + 2. * soVector[3] * k * l + 2. * soVector[4] * h * l);
								  if (dpi >= dplanecut && dpi <= dplanestart) {
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
									  reflectionList.addElement(new Reflection(this, jhlist, jklist, jllist, M, dpi));
//									System.out.println("New Reflection " + h +" "+k+" "+l + " " +dpi);
								  }
							  }
						  }
					  }
				  }
			  }
		  }
	  }
	  return reflectionList;
  }

  public double[] computeReflectionList(int totNumber, boolean permitAbsent, double sumOverlapped) {
    Vector reflectionList = getReflectionList(totNumber, 0.0, Math.max(Math.max(getFullCellValue(0), getFullCellValue(1)),
        getFullCellValue(2)), permitAbsent, sumOverlapped);
    if (totNumber == 0)
      totNumber = reflectionList.size();
    double[] results = new double[totNumber];
    for (int i = 0; i < totNumber; i++) {
      results[i] = ((double[]) reflectionList.elementAt(i))[0];
    }
    return results;
  }

  public double[] computeReflectionList(double mind, double maxd, boolean permitAbsent, double sumOverlapped) {
    Vector reflectionList = getReflectionList(0, mind, maxd, permitAbsent, sumOverlapped);
    int totNumber = reflectionList.size();
    double[] results = new double[totNumber];
    for (int i = 0; i < totNumber; i++)
      results[i] = ((double[]) reflectionList.elementAt(i))[0];
    return results;
  }

	public Vector<Reflection> getReflectionVector() {
		return reflectionv;
	}

  public Vector getReflectionList(int totNumber, double dmin, double dmax, boolean permitAbsent, double sumOverlapped) {

    long previousTime = 0;
    if (Constants.testtime)
      previousTime = System.currentTimeMillis();

	  cellVolumeComp();
	  char F_Convention = 'A';
	  getPhaseInfo().SgInfo = new T_SgInfo(this.getSpaceGroup(), F_Convention);
	  Sghkl sghkl = new Sghkl(getPhaseInfo().SgInfo);

	  Vector<double[]> reflectionList = getActivePlanarDefects().computeReflectionsList(this, sghkl, totNumber, dmin,
			  permitAbsent, sumOverlapped);

	  if (reflectionList == null) {

		  int friedelLaw = 1;
		  int h, k, l; //restriction;
		  int[] Minh = new int[1], Mink = new int[1], Minl = new int[1];
		  double[] soVector = getOriginalSoVector();
		  int Maxh = totNumber / 4 + 1;
		  int Maxk = Maxh, Maxl = Maxh;
//	  System.out.println(this.getLabel() + " Reflection list: " + dmin);
		  if (totNumber == 0) {
			  Maxh = (int) (1.0 / (Math.sqrt(soVector[0]) * dmin)) + 1;
			  Maxk = (int) (1.0 / (Math.sqrt(soVector[1]) * dmin)) + 1;
			  Maxl = (int) (1.0 / (Math.sqrt(soVector[2]) * dmin)) + 1;
		  }
		  int totNumber4 = totNumber * 10;

		  do {
			  sghkl.SetListMin_hkl(friedelLaw, Maxh, Maxk, Maxl, Minh, Mink, Minl);
			  reflectionList = new Vector<double[]>(totNumber4, 100);
			  for (h = Minh[0]; h <= Maxh; h++) {
				  for (k = Mink[0]; k <= Maxk; k++) {
					  for (l = Minl[0]; l <= Maxl; l++) {
						  if (sghkl.IsSysAbsent_hkl(h, k, l, null) == 0) {
							  if ((sghkl.IsHidden_hkl(friedelLaw, Minh[0], Mink[0], Minl[0],
									  Maxh, Maxk, Maxl, h, k, l)) == 0) {
								  if (!((h == 0 && k == 0) && l == 0)) {
									  double[] dpi = {1.0 / Math.sqrt(soVector[0] * h * h + soVector[1] * k * k + soVector[2] * l * l + 2.
											  * soVector[5] * h * k + 2. * soVector[3] * k * l + 2. * soVector[4] * h * l)};
									  if (sumOverlapped < 0.0)
										  reflectionList.addElement(dpi);
									  else {
										  boolean found = false;
										  for (int i = 0; i < reflectionList.size(); i++) {
											  double[] doubles = reflectionList.elementAt(i);
											  if (Math.abs(doubles[0] - dpi[0]) <= sumOverlapped) {
												  found = true;
												  break;
											  }
										  }
										  if (!found)
											  reflectionList.addElement(dpi);
									  }
								  }
							  }
						  } else if (permitAbsent) {
							  if ((sghkl.IsHidden_hkl(friedelLaw, Minh[0], Mink[0], Minl[0],
									  Maxh, Maxh, Maxh, h, k, l)) == 0) {
								  if (!((h == 0 && k == 0) && l == 0)) {
									  double[] dpi = {1.0 / Math.sqrt(soVector[0] * h * h + soVector[1] * k * k + soVector[2] * l * l + 2.
											  * soVector[5] * h * k + 2. * soVector[3] * k * l + 2. * soVector[4] * h * l)};
									  if (sumOverlapped < 0.0)
										  reflectionList.addElement(dpi);
									  else {
										  boolean found = false;
										  for (int i = 0; i < reflectionList.size(); i++) {
											  double[] doubles = reflectionList.elementAt(i);
											  if (Math.abs(doubles[0] - dpi[0]) <= sumOverlapped) {
												  found = true;
												  break;
											  }
										  }
										  if (!found)
											  reflectionList.addElement(dpi);
									  }
								  }
							  }
						  }
					  }
				  }
			  }
			  Maxh *= 2;
		  } while (reflectionList.size() < totNumber);

	  }

    if (Constants.testtime)
      System.out.println("Reflection list computation for phase " + getLabel() + ": " +
          (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");

    Collections.sort(reflectionList, new ComparerPured());

    if (Constants.testtime)
      System.out.println("Reflection list sorting: " +
          (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");

    hklsupordercomp();  // todo forgot what todo
    return reflectionList;
  }

  private void checkSghkllist(String SgName, double dplanecut) {

  	// cctbx

    if (refreshSpaceGroup) {
      refreshSpaceGroup = refreshSpaceGroupComputation(SgName, getSGconv());
    }

	  cellVolumeComp(); // to refresh

	  boolean done = getActivePlanarDefects().checkSghkllist(this, dplanecut);

	  if (!done) {

		  double dpi;
		  double[] soVector = getSoVector();

		  for (int i = 0; i < reflectionv.size()/* don't change */; i++) {
			  Reflection refl = reflectionv.elementAt(i);
			  dpi = 1.0 / Math.sqrt(soVector[0] * refl.getH() * refl.getH() + soVector[1] * refl.getK() * refl.getK()
					  + soVector[2] * refl.getL() * refl.getL()
					  + 2. * soVector[5] * refl.getH() * refl.getK() + 2. * soVector[3] * refl.getK() * refl.getL()
					  + 2. * soVector[4] * refl.getH() * refl.getL());
			  if (dpi < dplanecut ) {
				  reflectionv.removeElementAt(i);
				  i--;
			  }
		  }

		  double[] cell = new double[6];
		  for (int i = 0; i < 6; i++)
			  cell[i] = getCellValue(i);
		  Vector<PureReflection> reflList = SpaceGroups.getReflectionListFor(getSpaceGroupHall(), cell, dplanecut);

		  int numberReflections = reflectionv.size();
		  boolean[] alreadyChecked = new boolean[numberReflections];
		  for (int i = 0; i < numberReflections; i++)
			  alreadyChecked[i] = false;
		  int minimumChecked = 0;

		  int h, k, l;
		  for (int j = 0; j < reflList.size(); j++) {
			  PureReflection prefl = reflList.elementAt(j);
			  h = prefl.h[0];
			  k = prefl.k[0];
			  l = prefl.l[0];
//			  System.out.println("Reflection: " + h + " " + k + " " + l + ", n = " + reflectionv.size());
			  dpi = prefl.dspace;
			  if (!((h == 0 && k == 0) && l == 0) && getActivePlanarDefects().acceptReflection(h, k, l, dpi)) {
				  if (dpi >= dplanecut) {
//        System.out.println("Checking " + h +" "+k+" "+l + " " +dpi);
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
						  int[] jhlist = new int[prefl.multiplicity];
						  int[] jklist = new int[prefl.multiplicity];
						  int[] jllist = new int[prefl.multiplicity];
						  for (int i = 0; i < prefl.multiplicity; i++) {
							  jhlist[i] = prefl.h[i];
							  jklist[i] = prefl.k[i];
							  jllist[i] = prefl.l[i];
						  }
						  int M = prefl.mates * prefl.multiplicity;
						  Reflection refl = new Reflection(this, jhlist, jklist, jllist,
								  M, dpi);
						  reflectionv.add(refl);
						  System.out.println("Adding, New Reflection " + refl.toString() + " " + reflectionv.size());
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
		  System.out.println("Reflection number " + numberReflections + " " + reflectionv.size());
		  for (int i = numberReflections - 1; i >= 0; i--)
			  if (!alreadyChecked[i])
				  reflectionv.removeElementAt(i);
		  System.out.println("Reflection number after removing: " + reflectionv.size());
	  }
  }

	private void checkSghkllist(String SgName, int sgconv,
	                            double dplanecut, double dplanestart/*, boolean permitAbsent*/) {

//    SgInfo

		if (refreshSpaceGroup) {
			refreshSpaceGroup = refreshSpaceGroupComputation(SgName, sgconv);
		}

		cellVolumeComp(); // to refresh

		Sghkl sghkl = new Sghkl(getPhaseInfo().SgInfo);

		boolean done = getActivePlanarDefects().checkSghkllist(this, sghkl, dplanecut, dplanestart);

		if (!done) {
			int im, M2;
//    char F_Convention;
			int h, k, l, M; //restriction;
			int Maxh, Maxk, Maxl;
			int[] Minh = new int[1], Mink = new int[1], Minl = new int[1];
			double dpi; //, so12, so23, so13;
//    double 		          parm12, parm22, parm32, w1, so11, so22, so33;
			int[] hlist = new int[24], klist = new int[24], llist = new int[24];
			int[] jhlist;
			int[] jklist;
			int[] jllist;
			T_Eq_hkl Eq_hkl = new T_Eq_hkl();
			int friedelLaw = 1;
//    Vector reflectionList = new Vector(0, 1);


			double[] soVector = getSoVector();
//		  for (int i = 0; i < 6; i++)
//			  System.out.println(soVector[i] + " " + soVector[i]);

//	  System.out.println(this.getLabel() + " Reflection list: " + dplanecut);

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

			for (int i = 0; i < reflectionv.size()/* don't change */; i++) {
				Reflection refl = reflectionv.elementAt(i);
				dpi = 1.0 / Math.sqrt(soVector[0] * refl.getH() * refl.getH() + soVector[1] * refl.getK() * refl.getK()
						+ soVector[2] * refl.getL() * refl.getL()
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

/*    for (int i = 0; i < absentreflectionv.size(); i++) {
      Reflection refl = absentreflectionv.elementAt(i);
      dpi = 1.0 / Math.sqrt(soVector[0] * refl.h * refl.h + soVector[1] * refl.k * refl.k + soVector[2] * refl.l * refl.l
          + 2. * soVector[5] * refl.h * refl.k + 2. * soVector[3] * refl.k * refl.l
          + 2. * soVector[4] * refl.h * refl.l);
      if (dpi < dplanecut || dpi > dplanestart) {
        absentreflectionv.removeElementAt(i);
        i--;
      }
    }
    int numberReflectionsA = absentreflectionv.size();
    boolean[] alreadyCheckedA = new boolean[numberReflectionsA];
    for (int i = 0; i < numberReflectionsA; i++)
      alreadyCheckedA[i] = false;
    int minimumCheckedA = 0;*/

			for (h = Minh[0]; h <= Maxh; h++) {
				for (k = Mink[0]; k <= Maxk; k++) {
					for (l = Minl[0]; l <= Maxl; l++) {
						if (sghkl.IsSysAbsent_hkl(h, k, l, null) == 0) {
							if ((sghkl.IsHidden_hkl(friedelLaw, Minh[0], Mink[0], Minl[0],
									Maxh, Maxk, Maxl, h, k, l)) == 0) {
								dpi = 1.0 / Math.sqrt(soVector[0] * h * h + soVector[1] * k * k + soVector[2] * l * l + 2.
										* soVector[5] * h * k + 2. * soVector[3] * k * l + 2. * soVector[4] * h * l);
								if (!((h == 0 && k == 0) && l == 0) && getActivePlanarDefects().acceptReflection(h, k, l, dpi)) {
									if (dpi >= dplanecut && dpi <= dplanestart) {
//        System.out.println("Checking " + h +" "+k+" "+l + " " +dpi);
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
											Reflection refl = new Reflection(this, jhlist, jklist, jllist, M, dpi);
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
						}/* else if (permitAbsent) {
            if ((sghkl.IsHidden_hkl(friedelLaw, Minh[0], Mink[0], Minl[0],
                Maxh, Maxk, Maxl, h, k, l)) == 0) {
              if (!((h == 0 && k == 0) && l == 0) && getActivePlanarDefects().acceptReflection(h, k, l)) {
                dpi = 1.0 / Math.sqrt(soVector[0] * h * h + soVector[1] * k * k + soVector[2] * l * l + 2.
                    * soVector[5] * h * k + 2. * soVector[3] * k * l + 2. * soVector[4] * h * l);
                if (dpi >= dplanecut && dpi <= dplanestart) {
                  int found = -1;
                  for (int i = minimumCheckedA; i < numberReflectionsA; i++) {
                    if (!alreadyCheckedA[i]) {
                      Reflection refl = absentreflectionv.elementAt(i);
                      if (refl.equalsTo(h, k, l)) {
                        found = i;
                        break;
                      }
                    } else {
                      if (i == minimumCheckedA)
                        minimumCheckedA++;
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
                    Reflection refl = new Reflection(this, jhlist, jklist, jllist, M, dpi);
                    absentreflectionv.addElement(refl);
//										System.out.println("check, New Reflection " + h +" "+k+" "+l + " " + refl);
                  } else {
                    alreadyCheckedA[found] = true;
                    Reflection refl = absentreflectionv.elementAt(found);
                    refl.setDSpace(dpi);
                    refl.refreshforUpdate();
                  }
                }
              }
            }
          }*/
					}
				}
			}
			for (int i = numberReflections - 1; i >= 0; i--)
				if (!alreadyChecked[i])
					reflectionv.removeElementAt(i);
/*    for (int i = numberReflectionsA - 1; i >= 0; i--)
      if (!alreadyCheckedA[i])
        absentreflectionv.removeElementAt(i);*/

//	  System.out.println(getPhaseName() + " " + reflectionv.size());
//    return reflectionList;
		}
	}

  public boolean isCentrosymmetric() {
	  if (refreshSpaceGroup)
		  refreshSpaceGroup = refreshSpaceGroupComputation(getSpaceGroup(), getSGconv());
    return (getPhaseInfo().InversionOffOrigin() == 0);
  }

  public String fixSpaceGroupForTexture() {
    String sg;

    sg = new String(getSpaceGroup());

    switch (getNumber(getSymmetry())) {
      case 0: // triclinic
        break;
      case 1: // monoclinic
        if (sg != null) {
          int cchar = sg.indexOf(':');
          if (cchar > -1) {
            char achar = sg.charAt(++cchar);
            if (achar == '-')
              achar = sg.charAt(++cchar);
            switch (achar) {
              case 'a':
              case 'A': // alpha <> 90
                String sg1 = sg.substring(0, cchar) + "c";
                if (sg.length() > sg1.length())
                  sg1 = sg1 + sg.substring(cchar + 1, sg.length());

//                turnCellBackward();
                System.out.println("Need to change to space group: " + sg1);
                setSpaceGroup(false, sg1, true);

                break;
              case 'c':
              case 'C': // gamma <> 90
                // it's Ok
                break;
              default: // beta <> 90
              {
                sg1 = sg.substring(0, cchar) + "c";
                if (sg.length() > sg1.length()) {
                  sg1 = sg1 + sg.substring(cchar + 1, sg.length());
                }
//                turnCellForward();
                System.out.println("Need to change to space group: " + sg1);

                setSpaceGroup(false, sg1, true);

              }
            }
          }
          if (getCellValue(5) > 90.0) {
//            boolean oldPermission = isAbilitatetoRefresh;
//            isAbilitatetoRefresh = false;
            refreshCellForChange(CellOperation.SWITCH_AB); //switchAandB();
            refreshCellForChange(CellOperation.INVERT_GAMMA); //invertGamma();
            refreshCellForChange(CellOperation.INVERT_B); //invertB();
/*            isAbilitatetoRefresh = oldPermission;
            refreshReflectionv = true;
            refreshFhklcomp = true;
            refreshCellSymmetry = true;
            refreshCellVolume = true;
            refreshCrystMicrostrain = true;
            getActiveTexture().refreshComputation = true;
            getActiveStrain().refreshComputation = true;
            notifyUpObjectChanged(this, Constants.STRING_CHANGED);*/
          }
          if (getCellValue(0) > getCellValue(1)) {
//            boolean oldPermission = isAbilitatetoRefresh;
//            isAbilitatetoRefresh = false;
            refreshCellForChange(CellOperation.SWITCH_AB); //switchAandB();
            refreshCellForChange(CellOperation.INVERT_C); //invertC();
/*            isAbilitatetoRefresh = oldPermission;
            refreshReflectionv = true;
            refreshFhklcomp = true;
            refreshCellSymmetry = true;
            refreshCellVolume = true;
            refreshCrystMicrostrain = true;
            getActiveTexture().refreshComputation = true;
            getActiveStrain().refreshComputation = true;
            notifyUpObjectChanged(this, Constants.STRING_CHANGED);*/
          }

        } else { // beta <> 90
        }
        break;
      case 2: // orthorhombic
        break;
      case 3: // tetragonal
        break;
      case 4:
        int f = sg.indexOf(':');
        int f1 = -1;
        if (f != -1)
          f1 = sg.toUpperCase().lastIndexOf('R');
        if (f1 > f) {
          // trigonal setting
        } else {
          // hexagonal setting
        }
        break;
      case 5: // hexagonal
        break;
      case 6: // cubic
        break;
      default: {
      }
    }
    return getSpaceGroup();
  }

  public String switchSpaceGroup(String sg) {
    String sg1 = null;
    if (sg != null) {
      int cchar = sg.indexOf(':');
      if (cchar > -1) {
        char achar = sg.charAt(++cchar);
        if (achar == '-') {
          sg1 = sg.substring(0, cchar);
          if (sg.length() > sg1.length()) {
            sg = sg1 + sg.substring(cchar + 1, sg.length());
          }
        } else {
          sg1 = sg.substring(0, cchar) + "-";
          if (sg.length() > sg1.length()) {
            sg1 = sg1 + sg.substring(cchar, sg.length());
          }
          int istart = SpaceGroups.getBeginSG(getSymmetry(), getSGconv());
          int iend = SpaceGroups.getEndSG(getSymmetry(), getSGconv());
          for (int i = istart; i <= iend; i++) {
            String ansg = new String(Phase.getSpaceGroup(i, getSGconv()));
            if (ansg.equalsIgnoreCase(sg1))
              sg = sg1;
          }
        }
      }
    }
    System.out.println("Need to switch to space group: " + sg);

    return sg;
  }

  public void changeCellFor(Phase.CellOperation operation) {
    super.changeCellFor(operation);
    updateReflectionsForCellChange(operation);
  }

  public void switchAandB() {
    String label1 = getCell(0).getLabel();
    String label2 = getCell(1).getLabel();
    Parameter temp = getCell(1);
    parameterField[startingCellParID + 1] = getCell(0);
    parameterField[startingCellParID + 1].setLabel(label2);
    parameterField[startingCellParID + 1].editingField = getCell(0).editingField;
    parameterField[startingCellParID] = temp;
    parameterField[startingCellParID].setLabel(label1);
    parameterField[startingCellParID].editingField = temp.editingField;

    label1 = getCell(3).getLabel();
    label2 = getCell(4).getLabel();
    temp = getCell(4);
    parameterField[startingCellParID + 4] = getCell(3);
    parameterField[startingCellParID + 4].setLabel(label2);
    parameterField[startingCellParID + 4].editingField = getCell(3).editingField;
    parameterField[startingCellParID + 3] = temp;
    parameterField[startingCellParID + 3].setLabel(label1);
    parameterField[startingCellParID + 3].editingField = temp.editingField;

  }

  public void switchBandC() {
    String label1 = getCell(2).getLabel();
    String label2 = getCell(1).getLabel();
    Parameter temp = getCell(1);
    parameterField[startingCellParID + 1] = getCell(2);
    parameterField[startingCellParID + 1].setLabel(label2);
    parameterField[startingCellParID + 1].editingField = getCell(2).editingField;
    parameterField[startingCellParID + 2] = temp;
    parameterField[startingCellParID + 2].setLabel(label1);
    parameterField[startingCellParID + 2].editingField = temp.editingField;

    label1 = getCell(3).getLabel();
    label2 = getCell(4).getLabel();
    temp = getCell(4);
    parameterField[startingCellParID + 4] = getCell(5);
    parameterField[startingCellParID + 4].setLabel(label2);
    parameterField[startingCellParID + 4].editingField = getCell(5).editingField;
    parameterField[startingCellParID + 5] = temp;
    parameterField[startingCellParID + 5].setLabel(label1);
    parameterField[startingCellParID + 5].editingField = temp.editingField;

  }

  public void switchCandA() {
    String label1 = getCell(0).getLabel();
    String label2 = getCell(2).getLabel();
    Parameter temp = getCell(2);
    parameterField[startingCellParID + 2] = getCell(0);
    parameterField[startingCellParID + 2].setLabel(label2);
    parameterField[startingCellParID + 2].editingField = getCell(0).editingField;
    parameterField[startingCellParID] = temp;
    parameterField[startingCellParID].setLabel(label1);
    parameterField[startingCellParID].editingField = temp.editingField;

    label1 = getCell(3).getLabel();
    label2 = getCell(5).getLabel();
    temp = getCell(5);
    parameterField[startingCellParID + 5] = getCell(3);
    parameterField[startingCellParID + 5].setLabel(label2);
    parameterField[startingCellParID + 5].editingField = getCell(3).editingField;
    parameterField[startingCellParID + 3] = temp;
    parameterField[startingCellParID + 3].setLabel(label1);
    parameterField[startingCellParID + 3].editingField = temp.editingField;

  }

  public void invertGamma() {
    setCell(5, 180.0 - getCellValue(5));
  }

  public void invertAlpha() {
    setCell(3, 180.0 - getCellValue(3));
  }

  public void invertBeta() {
    setCell(4, 180.0 - getCellValue(4));
  }

  public void turnForward() {
    String label1 = getCell(0).getLabel();
    String label2 = getCell(1).getLabel();
    String label3 = getCell(2).getLabel();
    Parameter temp = getCell(2);
    parameterField[startingCellParID + 2] = getCell(1);
    parameterField[startingCellParID + 2].setLabel(label3);
    parameterField[startingCellParID + 2].editingField = getCell(1).editingField;
    parameterField[startingCellParID + 1] = getCell(0);
    parameterField[startingCellParID + 1].setLabel(label2);
    parameterField[startingCellParID + 1].editingField = getCell(0).editingField;
    parameterField[startingCellParID] = temp;
    parameterField[startingCellParID].setLabel(label1);
    parameterField[startingCellParID].editingField = temp.editingField;

    label1 = getCell(3).getLabel();
    label2 = getCell(4).getLabel();
    label3 = getCell(5).getLabel();
    temp = getCell(5);
    parameterField[startingCellParID + 5] = getCell(4);
    parameterField[startingCellParID + 5].setLabel(label3);
    parameterField[startingCellParID + 5].editingField = getCell(4).editingField;
    parameterField[startingCellParID + 4] = getCell(3);
    parameterField[startingCellParID + 4].setLabel(label2);
    parameterField[startingCellParID + 4].editingField = getCell(3).editingField;
    parameterField[startingCellParID + 3] = temp;
    parameterField[startingCellParID + 3].setLabel(label1);
    parameterField[startingCellParID + 3].editingField = temp.editingField;

//    updateReflectionsForCellChange(CellOperation.FORWARD);
  }

  public void turnBackward() {
    String label1 = getCell(0).getLabel();
    String label2 = getCell(1).getLabel();
    String label3 = getCell(2).getLabel();
    Parameter temp = getCell(0);
    parameterField[startingCellParID] = getCell(1);
    parameterField[startingCellParID].setLabel(label3);
    parameterField[startingCellParID].editingField = getCell(1).editingField;
    parameterField[startingCellParID + 1] = getCell(2);
    parameterField[startingCellParID + 1].setLabel(label2);
    parameterField[startingCellParID + 1].editingField = getCell(2).editingField;
    parameterField[startingCellParID + 2] = temp;
    parameterField[startingCellParID + 2].setLabel(label1);
    parameterField[startingCellParID + 2].editingField = temp.editingField;

    label1 = getCell(3).getLabel();
    label2 = getCell(4).getLabel();
    label3 = getCell(5).getLabel();
    temp = getCell(3);
    parameterField[startingCellParID + 3] = getCell(4);
    parameterField[startingCellParID + 3].setLabel(label3);
    parameterField[startingCellParID + 3].editingField = getCell(4).editingField;
    parameterField[startingCellParID + 4] = getCell(5);
    parameterField[startingCellParID + 4].setLabel(label2);
    parameterField[startingCellParID + 4].editingField = getCell(5).editingField;
    parameterField[startingCellParID + 5] = temp;
    parameterField[startingCellParID + 5].setLabel(label1);
    parameterField[startingCellParID + 5].editingField = temp.editingField;

//    updateReflectionsForCellChange(CellOperation.BACKWARD);
  }

  public void updateReflectionsForCellChange(CellOperation operation) {
    int hkln = gethklNumber();
    for (int i = 0; i < hkln; i++) {
      Reflection refl = reflectionv.elementAt(i);
      refl.updateForCellChange(operation);
    }
  }

  public boolean refreshSpaceGroupComputation(String SgName, int sgconv) {

	  refreshAtoms = true;
	  refreshEnergyComputation = true;
	  refreshFhklcomp = true;
	  refreshPositions = true;
	  fullAtomList = null;
	  if (reflectionv != null)
		  reflectionv.removeAllElements();
	  refreshReflectionv = true;

	  getPhaseInfo().refreshSpaceGroupComputation(SgName, sgconv);

    computeReducedCellFactors();

//    System.out.println("- Refreshing Space Group -");
//    System.out.println("Laue group: " + SpaceGroups.laueGroup[SpaceGroups.getLGNumber(PGgroup)]);
    return false;

  }

// End SgInfo calls

  public void computeReducedCellFactors() {
    reducedCellFactor[0] = 1.0;
    reducedCellFactor[1] = 1.0;
    reducedCellFactor[2] = 1.0;

    int siteNumber = getPhaseInfo().getSitePositionNumber();
//    System.out.println("Sites " + siteNumber);
    int[][] factors = MoreMath.get3factors(siteNumber);
    SitePosition[] sitepos = new SitePosition[siteNumber];
    for (int i = 0; i < siteNumber; i++)
      sitepos[i] = getPhaseInfo().getSitePosition(i);
    double[] xf = new double[3];

    double[] maxRedCell = new double[3];
    int[][] ind = new int[3][6];
    ind[0][0] = 0;
    ind[1][0] = 1;
    ind[2][0] = 2;

    ind[0][1] = 2;
    ind[1][1] = 1;
    ind[2][1] = 0;

    ind[0][2] = 1;
    ind[1][2] = 2;
    ind[2][2] = 0;

    ind[0][3] = 0;
    ind[1][3] = 2;
    ind[2][3] = 1;

    ind[0][4] = 2;
    ind[1][4] = 0;
    ind[2][4] = 1;

    ind[0][5] = 1;
    ind[1][5] = 0;
    ind[2][5] = 2;

    Vector reducedCell = new Vector(5, 5);

    for (int p = 0; p < 6; p++)
      for (int i = 0; i < factors[0].length; i++) {
        boolean goodReducedCell = true;
        for (double x1 = 0.02; x1 < .98; x1 += 0.19) {
          for (double y1 = 0.03; y1 < .98; y1 += 0.19) {
            for (double z1 = 0.04; z1 < .98; z1 += 0.19) {
              xf[0] = x1;
              xf[1] = y1;
              xf[2] = z1;
              for (int j = 0; j < 3; j++) {
                maxRedCell[j] = 1.0 / factors[ind[j][p]][i];
                xf[j] *= maxRedCell[j];
              }
//      System.out.println("maxReduced(" + i + ":" + maxRedCell[0] + " " + maxRedCell[1] + " " + maxRedCell[2]);
//      System.out.println("start :" + xf[0] + " " + xf[1] + " " + xf[2]);
              for (int is = 1; is < siteNumber; is++) { // skipping the first
                boolean checkSingle = false;
                for (int js = 0; js < 3; js++) {
                  double x = sitepos[is].getcoord(js, xf);
//          System.out.println("generated (" + js + ") :" + x);
                  if (x > maxRedCell[js])
                    checkSingle = true;
                }
                if (!checkSingle) {
                  goodReducedCell = false;
                  break;
                }
              }
            }
          }
          if (!goodReducedCell)
            break;
        }
        if (goodReducedCell) {
//          System.out.println("Asymmetric unit found #" + i + ":" + maxRedCell[0] + " " + maxRedCell[1] + " " + maxRedCell[2]);
          reducedCell.add(maxRedCell.clone());
        }
      }

    if (reducedCell.size() > 1) {
      CellSymmetry();
      double[] cell = new double[3];
      for (int i = 0; i < 3; i++)
        cell[i] = getCellValue(i);
      boolean found = false;
      for (int index = 0; index < reducedCell.size(); index++) {
        boolean checkit = true;
        maxRedCell = (double[]) reducedCell.elementAt(index);
        if (cell[0] == cell[1] && maxRedCell[0] != maxRedCell[1])
          checkit = false;
        if (cell[2] == cell[1] && maxRedCell[2] != maxRedCell[1])
          checkit = false;
        if (cell[0] == cell[2] && maxRedCell[0] != maxRedCell[2])
          checkit = false;
        if (checkit) {
//          System.out.println("Asymmetric unit assigned: " + maxRedCell[0] + " " + maxRedCell[1] + " " + maxRedCell[2]);
          for (int j = 0; j < 3; j++)
            reducedCellFactor[j] = maxRedCell[j];
          found = true;
        }
      }
      if (!found) {
        maxRedCell = (double[]) reducedCell.elementAt(0);
//        System.out.println("Asymmetric unit assigned: " + maxRedCell[0] + " " + maxRedCell[1] + " " + maxRedCell[2]);
        for (int j = 0; j < 3; j++)
          reducedCellFactor[j] = maxRedCell[j];
      }
    } else if (reducedCell.size() > 0) {
      maxRedCell = (double[]) reducedCell.elementAt(0);
//      System.out.println("Asymmetric unit assigned: " + maxRedCell[0] + " " + maxRedCell[1] + " " + maxRedCell[2]);
      for (int j = 0; j < 3; j++)
        reducedCellFactor[j] = maxRedCell[j];
    } // else
      // System.out.println("Warning: reduced cell not found!");
  }

  public int getLaueGroup() {
	  if (refreshSpaceGroup || getPhaseInfo().LGgroup == -1)
		  refreshSpaceGroup = refreshSpaceGroupComputation(getSpaceGroup(), getSGconv());
	  return getPhaseInfo().LGgroup;
  }

  public int getPointGroup() {
	  if (refreshSpaceGroup || getPhaseInfo().PGgroup == -1)
		  refreshSpaceGroup = refreshSpaceGroupComputation(getSpaceGroup(), getSGconv());
	  return getPhaseInfo().PGgroup;
  }

  public void cellVolumeComp() {
    if (refreshCellVolume) {
      double cella = getCellValue(0);
      double cellb = getCellValue(1);
      double cellc = getCellValue(2);
      double fullcella = getFullCellValue(0);
      double fullcellb = getFullCellValue(1);
      double fullcellc = getFullCellValue(2);
      double cella2 = cella * cella;
      double cellb2 = cellb * cellb;
      double cellc2 = cellc * cellc;
	    double fullcella2 = fullcella * fullcella;
	    double fullcellb2 = fullcellb * fullcellb;
	    double fullcellc2 = fullcellc * fullcellc;
      double cosalpha = Math.cos(getCellValue(3) * Constants.DEGTOPI);
      double cosbeta = Math.cos(getCellValue(4) * Constants.DEGTOPI);
      double cosgamma = Math.cos(getCellValue(5) * Constants.DEGTOPI);
      double powprod = cosalpha * cosalpha + cosbeta * cosbeta + cosgamma * cosgamma;
      double arg = 1.0 - powprod + 2.0 * cosalpha * cosbeta * cosgamma;
      if (arg <= 0.0) {
        cellVolume = 1.0;
        fullCellVolume = 1.0;
        refreshCellVolume = false;
        return;
      }

      cellVolume = cella * cellb * cellc * Math.sqrt(1.0 - powprod + 2.0 * cosalpha * cosbeta * cosgamma);
	    double oneOverCellVolume = 1.0 / cellVolume;
	    double oneOverCellVolume2 = oneOverCellVolume * oneOverCellVolume;
      fullCellVolume = fullcella * fullcellb * fullcellc * Math.sqrt(1.0 - powprod + 2.0 * cosalpha * cosbeta * cosgamma);
	    double oneOverFullCellVolume = 1.0 / fullCellVolume;
	    double oneOverFullCellVolume2 = oneOverFullCellVolume * oneOverFullCellVolume;
      so[0] = cellb2 * cellc2 * (1.0 - cosalpha * cosalpha) * oneOverCellVolume2;
      so[1] = cella2 * cellc2 * (1.0 - cosbeta * cosbeta) * oneOverCellVolume2;
      so[2] = cellb2 * cella2 * (1.0 - cosgamma * cosgamma) * oneOverCellVolume2;
      so[3] = cella2 * cellb * cellc * (cosbeta * cosgamma - cosalpha) * oneOverCellVolume2;
      so[4] = cellb2 * cella * cellc * (cosalpha * cosgamma - cosbeta) * oneOverCellVolume2;
      so[5] = cellc2 * cellb * cella * (cosbeta * cosalpha - cosgamma) * oneOverCellVolume2;
	    // for aniso temperature factors
	    so[6] = Math.sqrt(so[0] * so[1]);
	    so[7] = Math.sqrt(so[0] * so[2]);
	    so[8] = Math.sqrt(so[1] * so[2]);

	    full_so[0] = fullcellb2 * fullcellc2 * (1.0 - cosalpha * cosalpha) * oneOverFullCellVolume2;
	    full_so[1] = fullcella2 * fullcellc2 * (1.0 - cosbeta * cosbeta) * oneOverFullCellVolume2;
	    full_so[2] = fullcellb2 * fullcella2 * (1.0 - cosgamma * cosgamma) * oneOverFullCellVolume2;
	    full_so[3] = fullcella2 * fullcellb * fullcellc * (cosbeta * cosgamma - cosalpha) * oneOverFullCellVolume2;
	    full_so[4] = fullcellb2 * fullcella * fullcellc * (cosalpha * cosgamma - cosbeta) * oneOverFullCellVolume2;
	    full_so[5] = fullcellc2 * fullcellb * fullcella * (cosbeta * cosalpha - cosgamma) * oneOverFullCellVolume2;
	    // for aniso temperature factors
	    full_so[6] = Math.sqrt(full_so[0] * full_so[1]);
	    full_so[7] = Math.sqrt(full_so[0] * full_so[2]);
	    full_so[8] = Math.sqrt(full_so[1] * full_so[2]);
      refreshCellVolume = false;

    }
  }

  public double getDspacing(int h, int k, int l) {
    return 1.0 / Math.sqrt(full_so[0] * h * h + full_so[1] * k * k + full_so[2] * l * l + 2.0
        * full_so[5] * h * k + 2.0 * full_so[3] * k * l + 2.0 * full_so[4] * h * l);

  }

  public double getCellVolume() {
    cellVolumeComp();
    return cellVolume;
  }

  public double getFullCellVolume() {
    cellVolumeComp();
    return fullCellVolume;
  }

  public double getDensity() {
    double weight = 0.0;
    for (int j = 0; j < getFullAtomList().size(); j++)
      weight += getFullAtomList().get(j).getSiteWeight();

//    weight += getActiveStructureModel().getCellWeigth();
    double density = weight * 1.0E+24 / (Constants.AVOGADRO * getCellVolume());
    if (density == 0.0)
      density = 1.0;
    return density;
  }

	public double[] getComplexPermittivity(Radiation rad) {
		double lambda = rad.getWavelength().getValueD();
		double energyInKeV = Constants.ENERGY_LAMBDA / lambda * 0.001;
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
//		System.out.println("Density of " + toString() + ": " + density);
		complexPermittivity[0] = 1.0 - Constants.AVOGADRO * Constants.E_RADIUS_CM * lambda * lambda * density *
				meanScatteringFactor / (Constants.PI * meanAtomicMass);
		double cost = lambda * getAbsorption(energyInKeV) * density * 0.5 / Constants.PI;
		complexPermittivity[1] = cost * Math.sqrt(complexPermittivity[0] + cost * cost * 0.25);  // -
		return complexPermittivity;
	}

	/**
   * Return the Qc (reflectivity) for this phase.
   */

  public double getQc(Radiation rad) {
    return 4.0 * Constants.PI / rad.getWavelengthValue() * Math.sqrt(2.0 * (1.0 - getComplexPermittivity(rad)[0]));
  }

	public double getQc() {
// approximating sin(theta) with theta at low angle permits to avoid the lambda
// parameter. Based on the book of A. Gibaud et al. on reflectivity.
	  return 0.03751 * Math.sqrt(getAtomScatteringCellNormalization() / getCellVolume());
	}

	public double getAbsorption(RadiationType rad) {
	  if (rad.isElectron() || rad.isNeutron()) {
		  double absorption = 0.0;
		  double weight = 0.0;
		  for (int j = 0; j < getFullAtomList().size(); j++) {
			  absorption += getFullAtomList().get(j).getSiteAbsorption(rad);
			  weight += getFullAtomList().get(j).getSiteWeight();
		  }
		  if (weight == 0.0 || absorption == 0.0)
			  return 100.0;
			return absorption / weight;
	  }
	  double lambda = rad.getMeanRadiationWavelength();
	  double energyInKeV = Constants.ENERGY_LAMBDA / lambda * 0.001;
		return getAbsorption(energyInKeV);
  }

	public double getAbsorption(RadiationType rad, int index) {
		if (rad.isElectron() || rad.isNeutron()) {
			double absorption = 0.0;
			double weight = 0.0;
			for (int j = 0; j < getFullAtomList().size(); j++) {
				absorption += getFullAtomList().get(j).getSiteAbsorption(rad);
				weight += getFullAtomList().get(j).getSiteWeight();
			}
			if (weight == 0.0 || absorption == 0.0)
				return 100.0;
			return absorption / weight;
		}
		double lambda = rad.getRadiationWavelength(index);
		double energyInKeV = Constants.ENERGY_LAMBDA / lambda * 0.001;
		return getAbsorption(energyInKeV);
	}

	public double getAbsorption(double energyInKeV) {
		double absorption = 0.0;
		double totalNumber = 0.0;
		for (int j = 0; j < getFullAtomList().size(); j++) {
			absorption += getFullAtomList().elementAt(j).getSiteAbsorption(energyInKeV);
			totalNumber += getFullAtomList().elementAt(j).getSiteWeight();
		}
//			System.out.println(absorption + " ++++++ " + totalNumber);
		absorption /= totalNumber;
//		System.out.println("Absorption of " + toString() + ": " + absorption);
		return absorption;
	}

	public Vector<AtomQuantity> getChemicalComposition() {
		Vector<AtomQuantity> chemicalComposition = new Vector<AtomQuantity>();
		int numberAtoms = getFullAtomList().size();
		for (int j = 0; j < numberAtoms; j++) {
			Vector<AtomQuantity> atomSiteComposition = getFullAtomList().get(j).getChemicalComposition();
			for (AtomQuantity atomQuantity : atomSiteComposition) {
				int index = atomQuantity.getPositionIn(chemicalComposition);
				if (index >= 0) {
					AtomQuantity anAtomQuantity = chemicalComposition.elementAt(index);
					anAtomQuantity.quantity += atomQuantity.quantity;
					anAtomQuantity.quantity_weight += atomQuantity.quantity_weight;
				} else
					chemicalComposition.add(atomQuantity);
			}
		}
		return chemicalComposition;
	}

	// todo deprecated, to eliminate
	public double[] getCrystalliteMicrostrain(Reflection refl) {
		double[] crystmic = getActiveSizeStrainSym().getCrystalliteMicrostrain(refl.d_space, refl.getH(), refl.getK(), refl.getL(), null);
		double cryst1 = crystmic[0];
		double cryst2 = getPlanarDefectBroadening(refl);
		if (cryst1 != 0.0 && cryst2 != 0.0)
			crystmic[0] = 1.0 / (1.0 / cryst1 + 1.0 / cryst2);
		else if (cryst2 != 0.0)
			crystmic[0] = cryst2;

		crystmic[0] *= getActivePlanarDefects().getCrystalliteFactor(refl.getH(), refl.getK(), refl.getL());
		crystmic[1] *= getActivePlanarDefects().getMicrostrainFactor(refl.getL(), refl.getK(), refl.getL());
		return crystmic;
	}

	public double[] getCrystalliteMicrostrain(Reflection refl, double[] texture_angles) {
		if (getNumberOfCustomPeaks() > 0) {
			double[] crystmic = new double[2];
			crystmic[0] = refl.crystallite;
			crystmic[1] = refl.microstrain;
			return crystmic;
		}
		double[] crystmic = getActiveSizeStrainSym().getCrystalliteMicrostrain(refl.d_space, refl.getH(), refl.getK(), refl.getL(),
				texture_angles);
		double cryst1 = crystmic[0];
		double cryst2 = getPlanarDefectBroadening(refl);
		if (cryst1 != 0.0 && cryst2 != 0.0)
			crystmic[0] = 1.0 / (1.0 / cryst1 + 1.0 / cryst2);
		else if (cryst2 != 0.0)
			crystmic[0] = cryst2;

		crystmic[0] *= getActivePlanarDefects().getCrystalliteFactor(refl.getH(), refl.getK(), refl.getL());
		crystmic[1] *= getActivePlanarDefects().getMicrostrainFactor(refl.getH(), refl.getK(), refl.getL());
		return crystmic;
	}

	public double getPlanarDefectBroadening(Reflection refl) {
    if (!getActivePlanarDefects().identifier.equals("none pd"))
      return getActivePlanarDefects().getCrystalliteEquivalent(refl);
    return 0.0;
  }

  public boolean isCubic() {
    int symnumber = getNumber(getSymmetry());
    if (symnumber == 6)
      return true;
    else
      return false;
  }

  public boolean closePacked() {
    int symnumber = getNumber(getSymmetry());
    if (symnumber == 5 || symnumber == 6)
      return true;
    else
      return false;
  }

  public int getClosePackedType() {
    computeFaultAsymmetry = false;
    if (!closePacked())
      return -1;
    if (getNumber(getSymmetry()) == 5)
      return 0;  // hexagonal

    AtomSite anatom;
    int atomNumbers = getFullAtomList().size();

    for (int i = 0; i < atomNumbers; i++) {
      anatom = getFullAtomList().get(i);
      if (anatom.fccPosition()) {
        computeFaultAsymmetry = true;
        return 1;  // fcc
      }
    }
    return 2;   // bcc
  }

  public double getApparentQuantity(double volFraction, RadiationType rad, Layer alayer) {
    return ((MicroAbsorption) getActiveSubordinateModel(microAbsorptionID)).getApparentQuantity(
        volFraction, rad, alayer, getAbsorptionCrystSizeD());
  }

  public Sample getSample() {
    return (Sample) getParent();
  }

  public double[] getMeanCrystStrain() {
    double[] sizestrain = new double[2];
    refreshAll(false);
    int numberReflexes = gethklNumber();
    sizestrain[0] = 0.0;
    sizestrain[1] = 0.0;
    int total = 0;
    for (int row = 0; row < numberReflexes; row++) {
      int mult = getMultiplicity(row);
	    double[] cryststrain = getCrystalliteMicrostrain(getReflex(row));
      double cryst = Math.abs(cryststrain[0]);
      double strain = Math.abs(cryststrain[1]);
      sizestrain[0] += cryst * mult;
      sizestrain[1] += strain * mult;
      total += mult;
    }
    sizestrain[0] /= total;
    sizestrain[1] /= total;
    return sizestrain;
  }

  public boolean freeAllBasicParameters() {
    boolean done = false;
    CellSymmetry();
    for (int i = 0; i < 6; i++)
      if (ic[i] == 1) {
        getCell(i).setRefinableCheckBound();
        done = true;
      }
    return done;
  }

  public void freeAllMicroParameters() {
    boolean amorphous = false;
    if (getPhaseID().toLowerCase().indexOf("amorph") >= 0)
      amorphous = true;
    getActiveSizeStrainSym().freeAllMicroParameters(amorphous);
    getActivePlanarDefects().freeAllMicroParameters(getClosePackedType());
  }

  public void freeAllCrystalParameters() {
    AtomSite anatom;
    int atomNumbers = getAtomNumber();
    boolean share;

    for (int i = 0; i < atomNumbers; i++) {
      anatom = getAtom(i);
      share = false;
      for (int j = 0; j < i; j++)
        if (anatom.shareSiteWith(getAtom(j))) {
          share = true;
          anatom.boundAllParametersTo(getAtom(j));
        }
      if (!share)
        anatom.freeAllCrystalParameters();
    }
    getActiveStructureModel().freeAllCrystalParameters();
  }

  public void freeAllTextureParameters() {
    getActiveTexture().freeAllParameters();
  }

  public void fixAllTextureParametersPreserveBound() {
    getActiveTexture().fixAllParametersPreserveBound();
  }

  public void freeAllStrainParameters() {
    getActiveStrain().freeAllParameters();
  }

  public void fixAllStrainParametersPreserveBound() {
    getActiveStrain().fixAllParametersPreserveBound();
  }

/*	public void freeAllStressParameters() {
		getActiveStress().freeAllParameters();
	}

	public void fixAllStressParametersPreserveBound() {
		getActiveStress().fixAllParametersPreserveBound();
	}*/

  PhaseD theFrame = null;

  public void edit(Frame aframe) {
    (new PhaseD(getFilePar().getMainFrame(), this)).setVisible(true);
    //(new PhaseD_new(aframe, this)).setVisible(true);
  }

  public void writeCustomObject(BufferedWriter out) {/*

    try {
      out.newLine();
      out.write("#custom_object_" + "reflections_list");
      out.newLine();
      int hkln = reflectionv.size();
      for (int i = 0; i < hkln; i++) {
        Reflection refl = reflectionv.elementAt(i);
        refl.writeCustomObject(out);
      }
      out.newLine();
      out.write("#end_custom_object_" + "reflections_list");
      out.newLine();
      out.newLine();
    } catch (IOException ioe) {
      System.out.println("Error in writing the reflections list for " + toXRDcatString());
    }*/

  }

  public void readCustomObject(CIFtoken ciffile) {/*
    // to be override by subclasses
    // the default read and do nothing

    int newtoken, tokentype;
//		XRDcat theobj = null;
    boolean endofInput = false;
    boolean newLoop = false;
    boolean startLoop = false;

    try {
      do {
        tokentype = ciffile.nextToken();
        switch (tokentype) {
          case CIFtoken.TT_DATA:
          case CIFtoken.TT_DATASET:
            newLoop = false;
            break;
          case CIFtoken.TT_CIFE:
            // CIF item
            break;
          case CIFtoken.TT_LOOP:
            // start the loop for the values here
            newLoop = true;
            startLoop = true;
            break;
          case CIFtoken.TT_NUMBER:
            break;
          case CIFtoken.TT_CUSTOM:
            Reflection refl = new Reflection(this);
            reflectionv.add(refl);
            refl.readCustomObject(ciffile);
            break;
          case CIFtoken.TT_CUSTOM_END:
            endofInput = true;
            break;
          default: {
          }
        }
      } while (tokentype != CIFtoken.TT_EOF && !endofInput);

    } catch (IOException ioe) {
      System.out.println("IO exception in custom object for " + toXRDcatString());
    }*/

/*		if (theobj != null)
			theobj.readall(ciffile);*/
  }

  public String exportForCOD(Frame frame) {
//    String filename = Constants.filesfolder + "phaseSubmittedToCOD.CIF";
    String filename = Utility.browseFilenametoSave(frame, "Save the cif file before to send it");
    int extension = filename.lastIndexOf(".");
    if (extension > 0)
      filename = filename.substring(0, extension) + Long.toString(System.currentTimeMillis()) +
          filename.substring(extension, filename.length());
    else
      filename = filename + Long.toString(System.currentTimeMillis()) + ".cif";
    try {
      BufferedWriter out = Misc.getWriter(filename);
      if (out != null) {
        writeForCOD(out);
        out.flush();
        out.close();
      } else {
        System.out.println("Not able to open the file for append");
      }
    } catch (IOException ioe) {
      System.out.println("Unable to save the object " + toXRDcatString());
    }
    return filename;
  }

  public void writeForCOD(BufferedWriter out) {

    try {
      out.write("data_" + Misc.toStringDeleteBlankTabAndEOF(this.getLabel()));
      out.newLine();
      out.newLine();
    } catch (IOException e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
    writeDataField(out);
    writeGeneralRefinement(out);
    writeAllFieldsCOD(out);
//    writeAllLoopFields(out);
    writeAllParametersCOD(out);
	  writeGeneralCoordinateCOD(out);
//    writeAllLoopParameters(out);
//    writeAllSubordinates(out, "", "");
//    writeCustomObject(out);
//    writeBounds(out);
    writeAllAtoms(out);
  }

  public void writeGeneralRefinement(BufferedWriter out) {
    getFilePar().refreshProgramInformations();
    getFilePar().writeAllFieldsCOD(out);
    try {
      out.newLine();
    } catch (IOException ioe) {
      System.out.println("Error in writing the object " + toXRDcatString());
    }
  }

  public void writeAllFieldsCOD(BufferedWriter out) {
    int i;

    stringField[0] = toXRDcatString();
    for (i = 0; i < 3; i++)
      writeField(out, diclist[i], stringField[i]);
    int spgr = 3 + getSGconv();
    writeField(out, diclist[spgr], stringField[spgr]);
    for (i = 7; i < Nstring; i++)
      writeField(out, diclist[i], stringField[i]);

  }

	public void writeGeneralCoordinateCOD(BufferedWriter out) {

		try {
			out.write("loop_");
			out.newLine();
			out.write("_symmetry_equiv_pos_site_id");
			out.newLine();
			out.write("_symmetry_equiv_pos_as_xyz");
			out.newLine();
			for (int i = 0; i < getPhaseInfo().getSitePositionNumber(); i++) {
				out.write(Integer.toString(i));
				out.write(" '");
				String pos = getPhaseInfo().getSitePosition(i).getx();
				if (pos.startsWith("+"))
					pos = pos.substring(1);
				out.write(pos);
				out.write(", ");
				pos = getPhaseInfo().getSitePosition(i).gety();
				if (pos.startsWith("+"))
					pos = pos.substring(1);
				out.write(pos);
				out.write(", ");
				pos = getPhaseInfo().getSitePosition(i).getz();
				if (pos.startsWith("+"))
					pos = pos.substring(1);
				out.write(pos);
				out.write("'");
				out.newLine();
			}
			out.newLine();
		} catch (IOException ioe) {
			System.out.println("Error in writing the object " + toXRDcatString());
		}

	}

	public void writeAllParametersCOD(BufferedWriter out) {
    int i;

    CellSymmetry();
    for (i = 0; i < 6; i++)
      writeParameterCOD(out, diclist[i + totstringloop], parameterField[i]);
    try {
      if (Nparameter > 0)
        out.newLine();
    } catch (IOException ioe) {
      System.out.println("Error in writing the loop parameter " + toXRDcatString());
    }

  }

  public void writeAllAtoms(BufferedWriter out) {
    refreshAtoms();
    int totalNumber = getFullAtomList().size();
    if (totalNumber < 1)
      return;
    try {
      out.write("loop_");
      out.newLine();
      AtomSite ato = getFullAtomList().get(0);
      out.write("_atom_site_label");
      out.newLine();
      out.write(AtomSite.diclistc[0]);
      out.newLine();
      out.write(AtomSite.diclistc[ato.totstringloop]);
      out.newLine();
      out.write(AtomSite.diclistc[ato.totstringloop + 1]);
      out.newLine();
      out.write(AtomSite.diclistc[ato.totstringloop + 2]);
      out.newLine();
      out.write(AtomSite.diclistc[ato.totstringloop + 3]);
      out.newLine();
      out.write(AtomSite.diclistc[ato.totstringloop + 4]);
      out.newLine();
      out.write(AtomSite.diclistc[3]);
      out.newLine();
	    for (int i = 0; i < totalNumber; i++) {
		    ato = getFullAtomList().get(i);
		    if (ato.useThisAtom) {
				 for (int j = 0; j < ato.subordinateloopField[AtomSite.scattererLoopID].size(); j++) {
					 AtomScatterer atomScatterer = (AtomScatterer) ato.subordinateloopField[AtomSite.scattererLoopID].elementAt(j);
					 out.write(ato.getLabel() + "_" + atomScatterer.getLabel() + " ");
					 out.write(atomScatterer.getAtomSymbol());
					 out.write(" ");
					 writeParameterCOD(out, atomScatterer.parameterField[0]);
					 Coordinates coord = ato.getCoordinates();
					 out.write(" ");
					 out.write(Float.toString((float) coord.x));
					 out.write(" ");
					 out.write(Float.toString((float) coord.y));
					 out.write(" ");
					 out.write(Float.toString((float) coord.z));
					 out.write(" ");
					 writeParameterCOD(out, ato.parameterField[4]);
					 out.write(" " + ato.stringField[3]);
					 out.newLine();
				 }
		    }
	    }
      out.newLine();
    } catch (IOException ioe) {
      System.out.println("Error in writing the object " + toXRDcatString());
    }
  }

  public void writeParameterCOD(BufferedWriter out, String dicterm, Parameter par) {
    try {
      out.write(dicterm);
      out.write(" ");
      out.write(par.getValueForCOD());
      if (par.getFree())
        out.write("(" + par.getErrorForCOD() + ")");
      out.newLine();
    } catch (IOException ioe) {
      System.out.println("Error in writing the Parameter in object " + toXRDcatString());
    }
  }

  public void writeParameterCOD(BufferedWriter out, Parameter par) {
    try {
      out.write(par.getValueForCOD());
      if (par.getFree())
        out.write("(" + par.getErrorForCOD() + ")");
    } catch (IOException ioe) {
      System.out.println("Error in writing the object " + toXRDcatString());
    }
  }

  public void writeSimpleResults(BufferedWriter out) throws IOException {
    double[] sizestrain = getMeanCrystStrain();
    // first we force a refresh of the cell
    CellSymmetry();
    for (int i = 0; i < 6; i++) {
      // here the last modification
      if (ic[i] == 1) {
        out.write(Fmt.format(parameterField[i].getValueD()));
        out.write("\t");
      }
    }
    out.write(Fmt.format(sizestrain[0]));
    out.write("\t");
    out.write(Fmt.format(sizestrain[1]));
    out.write("\t");
  }

  public void writeSimpleResultsFirstLine(BufferedWriter out) throws IOException {

//    double[] sizestrain = getMeanCrystStrain();
    // first we force a refresh of the cell
    CellSymmetry();
    for (int i = 0; i < 6; i++) {
      // here the last modification
      if (ic[i] == 1) {
        out.write("Cell_Par(Angstrom)");
        out.write("\t");
      }
    }
    out.write("Size(Angstrom)");
    out.write("\t");
    out.write("Microstrain");
    out.write("\t");
  }

//	public static native double getWindowPosition();

  class dComparer implements Comparator {
    public int compare(Object obj1, Object obj2) {
      double dspace1 = ((Reflection) obj1).d_space;
      double dspace2 = ((Reflection) obj2).d_space;
      double diff = dspace1 - dspace2;

      if (diff == 0.0)
        return 0;
      else if (diff > 0)
        return -1;
      return 1;
    }
  }

  class ComparerPured implements Comparator {
    public int compare(Object obj1, Object obj2) {
      double dspace1 = ((double[]) obj1)[0];
      double dspace2 = ((double[]) obj2)[0];
      double diff = dspace1 - dspace2;

      if (diff == 0.0)
        return 0;
      else if (diff > 0)
        return -1;
      return 1;
    }
  }

  class figureOfMerit implements Comparator {
    public int compare(Object obj1, Object obj2) {
      int[] spaceGroup1 = (int[]) obj1;
      int[] spaceGroup2 = (int[]) obj2;

      int figureOfMerit1 = spaceGroup1[2] + spaceGroup1[3];
      int figureOfMerit2 = spaceGroup2[2] + spaceGroup2[3];
      int diff = figureOfMerit1 - figureOfMerit2;

      if (diff == 0.0) {
        if (spaceGroup1[2] > spaceGroup2[2])
          return 1;
        else if (spaceGroup1[2] < spaceGroup2[2])
          return -1;
        return 0;
      } else if (diff < 0)
        return -1;
      return 1;
    }
  }

}

