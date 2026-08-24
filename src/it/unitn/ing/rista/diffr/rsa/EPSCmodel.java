/*
 * @(#)EPSCmodel.java created 13/12/2024 Povo
 *
 * Copyright (c) 2024 Luca Lutterotti All Rights Reserved.
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

import java.awt.*;
import javax.swing.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.rta.DiscreteODFTexture;
import it.unitn.ing.rista.util.*;

import java.io.*;
import java.util.StringTokenizer;
import java.util.Vector;

/**
 *  The EPSCmodel is a class to compute the diffraction shift from
 *  the EPSC model of C. N. Tomè and R. A. Lebensohn, LANL.
 *  Based on EPSC4 version
 *  with contributions from B. Clausen, L. Capolungo, S. Merkel
 *
 * @version $Revision: 1.0 $, $Date: 2024/12/13 12:44:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

// enum usage:
// Ellipsoid.valueOf("EVOLVING") -> EVOLVING
// Ellipsoid array[] = Ellipsoid.values();
// for (Ellipsoid ell : array) {
//    System.out.println(ell + " at index "
//                               + ell.ordinal());
// }
//
// NON_EVOLVING at index 0
// etc

enum Ellipsoid {
  NON_EVOLVING,
  EVOLVING,
  INDIVIDUAL;
}

record PlaneDirections(int h, int k, int l, Vector<double[]> angles) {}

public class EPSCmodel extends Strain {

  public static int TITLE_ID = 0;
  public static int SHAPE_CTRL_ID = 1;
  public static int USE_TEXTURE_ID = 2;
  public static int LARGE_STRAIN_ID = 3;
  public static int ITMAX_ID = 4;
  public static int ERROR_SC_ID = 5;
  public static int IT_ACTIVE_SYS_ID = 6;
  public static int USE_PREVIOUS_ID = 7;
  public static int TEXTURE_IT_SKIP_ID = 8;
  public static int CALCULATE_STRAIN_ID = 9;
  public static int INVERSE_PF_ID = 10;
  public static int STIFF_PRESSURE_DEP_ID = 11;
  public static int LAW_HARDENING_ID = 12;
  public static int SPREAD_ID = 13;
  public static int ODF_RESOLUTION_ID = 14;

  public static int ELLIPSOID_RATIO_ID = 0;
  public static int ELLIPSOID_ANGLE_ID = 3;
  public static int STIFFNESS_ID = 6;
  public static int STIFFNESS_P_DER_ID = 27;
  public static int THERMAL_EXPANSION_ID = 48;
  public static int DISLOCATION_MODEL_ID = 54;
  public static int THERMO_MECHANICAL_PROC_ID = 0;
  public static int SLIP_MODE_ID = 1;
  public static int TWINNING_MODE_ID = 2;

  public static String[] diclistc = {
      "_rista_epsc_user_title",                    //              string
      "_rista_epsc_grain_shape_ctrl",              // ishape       int 0-2
      "_rista_residual_stress_use_texture",        //              boolean
      "_rista_epsc_large_strain_model",            // irot         boolean
      "_rista_epsc_iterations_self_cons",          // itmax_mod    int 100
      "_rista_epsc_error_self_cons",               // error_mod    double
      "_rista_epsc_iterations_active_systems",     // itmax_grain  int
      "_rista_epsc_use_previous_iteration",        // i_prev_proc  boolean
      "_rista_epsc_texture_iterations_skip",       // itexskip     boolean
      "_rista_epsc_internal_strain_calculate",     // i_diff_dir   boolean
      "_rista_epsc_strain_inverse_pole_figures",   // i_strpf      boolean
      "_rista_epsc_stiff_pressure_dependence",     // (iSM=0 no, 1 yes) boolean
      "_rista_epsc_constitutive_law_hardening",    // kCL          int 0-2
      "_rista_epsc_spread_deg",                    // spread          int 0-2
      "_rista_epsc_odf_resolution_deg",            // odf resolution

      "_rista_epsc_ellipsoid_ratio_x", // 0
      "_rista_epsc_ellipsoid_ratio_y", // 1
      "_rista_epsc_ellipsoid_ratio_z",
      "_rista_epsc_ellipsoid_euler_alpha",
      "_rista_epsc_ellipsoid_euler_beta",
      "_rista_epsc_ellipsoid_euler_gamma", // 5

      "_rista_stiffness_11", // 1     6
      "_rista_stiffness_12", // 2
      "_rista_stiffness_13", // 3
      "_rista_stiffness_14", // 4
      "_rista_stiffness_15", // 5
      "_rista_stiffness_16", // 6
      "_rista_stiffness_22", // 7
      "_rista_stiffness_23", // 8
      "_rista_stiffness_24", // 9
      "_rista_stiffness_25", // 10
      "_rista_stiffness_26", // 11
      "_rista_stiffness_33", // 12
      "_rista_stiffness_34", // 13
      "_rista_stiffness_35", // 14
      "_rista_stiffness_36", // 15
      "_rista_stiffness_44", // 16
      "_rista_stiffness_45", // 17
      "_rista_stiffness_46", // 18
      "_rista_stiffness_55", // 19
      "_rista_stiffness_56", // 20
      "_rista_stiffness_66", // 21

      "_rista_stiffness_p_der_11", // 1     27
      "_rista_stiffness_p_der_12", // 2
      "_rista_stiffness_p_der_13", // 3
      "_rista_stiffness_p_der_14", // 4
      "_rista_stiffness_p_der_15", // 5
      "_rista_stiffness_p_der_16", // 6
      "_rista_stiffness_p_der_22", // 7
      "_rista_stiffness_p_der_23", // 8
      "_rista_stiffness_p_der_24", // 9
      "_rista_stiffness_p_der_25", // 10
      "_rista_stiffness_p_der_26", // 11
      "_rista_stiffness_p_der_33", // 12
      "_rista_stiffness_p_der_34", // 13
      "_rista_stiffness_p_der_35", // 14
      "_rista_stiffness_p_der_36", // 15
      "_rista_stiffness_p_der_44", // 16
      "_rista_stiffness_p_der_45", // 17
      "_rista_stiffness_p_der_46", // 18
      "_rista_stiffness_p_der_55", // 19
      "_rista_stiffness_p_der_56", // 20
      "_rista_stiffness_p_der_66", // 21

      "_rista_thermal_expansion_11", // 48
      "_rista_thermal_expansion_22",
      "_rista_thermal_expansion_33",
      "_rista_thermal_expansion_23",
      "_rista_thermal_expansion_13",
      "_rista_thermal_expansion_12", // 53

      "_rista_epsc_dislocation_interaction_const", // 54
      "_rista_epsc_dislocation_Q",
      "_rista_epsc_dislocation_strain_rate",

      "_rista_epsc_thermomechanical_processes_id",
      "_rista_epsc_slip_mode_id",
      "_rista_epsc_twinning_mode_id"


  };
  public static String[] diclistcrm = {
      "_rista_epsc_user_title",
      "Grain shape ctrl (0-2)",              // ishape
      "_rista_residual_stress_use_texture",
      "_rista_epsc_large_strain_model",            // irot
      "max iterations self consistent",          // itmax_mod
      "_rista_epsc_error_self_cons",               // error_mod
      "_rista_epsc_iterations_active_systems",     // itmax_grain
      "_rista_epsc_use_previous_iteration",        // i_prev_proc
      "_rista_epsc_texture_iterations_skip",       // itexskip
      "_rista_epsc_internal_strain_calculate",     // i_diff_dir
      "_rista_epsc_strain_inverse_pole_figures",   // i_strpf
      "_rista_epsc_stiff_pressure_dependence",     // (kSM=0 no; kSM=1 yes)
      "_rista_epsc_constitutive_law_hardening",    // kCL
      "Spread in degrees to merge directions",
      "_rista_epsc_odf_resolution_deg",            // odf resolution

      "Ellipsoid ratio x", // 0
      "Ellipsoid ratio y", // 1
      "Ellipsoid ratio z",
      "Ellipsoid orientation alpha",
      "Ellipsoid orientation beta",
      "Ellipsoid orientation gamma", // 5

      "stiffness_11 (arb)", // 1
      "stiffness_12 (arb)", // 2
      "stiffness_13 (arb)", // 3
      "stiffness_14 (arb)", // 4
      "stiffness_15 (arb)", // 5
      "stiffness_16 (arb)", // 6
      "stiffness_22 (arb)", // 7
      "stiffness_23 (arb)", // 8
      "stiffness_24 (arb)", // 9
      "stiffness_25 (arb)", // 10
      "stiffness_26 (arb)", // 11
      "stiffness_33 (arb)", // 12
      "stiffness_34 (arb)", // 13
      "stiffness_35 (arb)", // 14
      "stiffness_36 (arb)", // 15
      "stiffness_44 (arb)", // 16
      "stiffness_45 (arb)", // 17
      "stiffness_46 (arb)", // 18
      "stiffness_55 (arb)", // 19
      "stiffness_56 (arb)", // 20
      "stiffness_66 (arb)", // 20

      "stiffness derivative 11 (arb)", // 1
      "stiffness derivative 12 (arb)", // 2
      "stiffness derivative 13 (arb)", // 3
      "stiffness derivative 14 (arb)", // 4
      "stiffness derivative 15 (arb)", // 5
      "stiffness derivative 16 (arb)", // 6
      "stiffness derivative 22 (arb)", // 7
      "stiffness derivative 23 (arb)", // 8
      "stiffness derivative 24 (arb)", // 9
      "stiffness derivative 25 (arb)", // 10
      "stiffness derivative 26 (arb)", // 11
      "stiffness derivative 33 (arb)", // 12
      "stiffness derivative 34 (arb)", // 13
      "stiffness derivative 35 (arb)", // 14
      "stiffness derivative 36 (arb)", // 15
      "stiffness derivative 44 (arb)", // 16
      "stiffness derivative 45 (arb)", // 17
      "stiffness derivative 46 (arb)", // 18
      "stiffness derivative 55 (arb)", // 19
      "stiffness derivative 56 (arb)", // 20
      "stiffness derivative 66 (arb)", // 21

      "thermal expansion coeff 11",
      "thermal expansion coeff 22",
      "thermal expansion coeff 33",
      "thermal expansion coeff 23",
      "thermal expansion coeff 13",
      "thermal expansion coeff 12",

      "_rista_epsc_dislocation_interaction_const", // 54
      "_rista_epsc_dislocation_Q",
      "_rista_epsc_dislocation_strain_rate",

      "thermomechanical processes id",
      "epsc slip mode id",
      "epsc twinning mode id"
  };

  public static String[] classlistcs = {};
  public static String[] classlistc = {"it.unitn.ing.rista.diffr.rsa.ThermoMechanicalProcess",
                                       "it.unitn.ing.rista.diffr.rsa.EpscSlipMode",
                                       "it.unitn.ing.rista.diffr.rsa.EpscTwinningMode"};

  public static int THERMO_MECHANICAL_ID = 0;

  public static String[] reminders = {
      "        Information about grain shape:", // line 2
      "        Name and Path for Texture File:", // line 6
      "        Name and Path for Material Data File (Single Crystal File):", // line 9
      "        Precision Settings for Convergence Procedures", // line 11
      "        Input/Output Settings for the Run", // line 15
      "        Number of thermomechanical processes to be run:", // line 22
      "        Path and Name for Process Files", // line 24
      "" // line 28
  };

  String programName = "epsc4";
  String insName = "epsc4.in";

  public String userDefinedTitle = "EPSC4 input file, created by Maud";

  // Information about grain shape:
  public int grainShape = 0;
  public double[] ellipsoidRatios = {1.0, 1.0, 1.0};
  public double[] ellipsoidEulerAngles = {0.0, 0.0, 0.0};

  // Name and Path for Texture File:
//  public String textureFile = "texture.red";
  public boolean textureEvolution = false; // 0 = false, 1 = true

  // Name and Path for Material Data File (Single Crystal File)
//  public String materialDatafile = "crystal.sx";

  // Precision Settings for Convergence Procedures
//  public int itmax_mod = 100;
//  public double error_mod = 0.01;
//  public int itmax_grain = 100;

  // Input/Output Settings for the Run
//  public boolean prev_proc = false;    // "i_prev_proc" - Reads state from previous process (1=YES or 0=NO) and related file:
  public String prev_proc_file = "epsc4.out";
//  public int  itexskip = 50; // "itexskip" - Sets Frequency of Texture Downloads
//  public boolean diff_dir = true; // "i_diff_dir"    Read diffracting planes and directions (1=YES or 0=NO) and file:
//  public String diff_dirFile = "epsc4.dif";
//  public boolean strpf = false; // "i_strpf"       Read directions and calculate strain pole figure (1=YES or 0=NO):

  // Number of thermomechanical processes to be run:
  public double spread = MaudPreferences.getDouble("epsc4.spread", 3.0);
//  public double step = 5.0;

  int irandom = 1;
  double[][] e0 = new double[6][6];

  boolean debug_output = MaudPreferences.getBoolean("epsc4.debug", false);
  boolean log_output = false;

  public EPSCmodel(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "EPSC4";
    IDlabel = "EPSC4";
    description = "select this to apply the EPSC4 model of Tomè and Lebensohn";
  }

  public EPSCmodel(XRDcat aobj) {
    this(aobj, "EPSC4");
  }

  public EPSCmodel() {
    identifier = "EPSC4";
    IDlabel = "EPSC4";
    description = "select this to apply the  EPSC4 model of Tomè and Lebensohn";
  }

  public void initConstant() {
    Nstring = 15;
    Nstringloop = 0;
    Nparameter = 57;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 3;
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

  public void initParameters() {
    super.initParameters();

    setString(TITLE_ID, "Put a title here");
    setString(SHAPE_CTRL_ID, "0");
    setString(USE_TEXTURE_ID, "true");
    setString(LARGE_STRAIN_ID, "true");
    setString(ITMAX_ID, "100");
    setString(ERROR_SC_ID, "0.001");
    setString(IT_ACTIVE_SYS_ID, "100");
    setString(USE_PREVIOUS_ID, "false");
    setString(TEXTURE_IT_SKIP_ID, "50");
    setString(CALCULATE_STRAIN_ID, "true");
    setString(INVERSE_PF_ID, "false");
    setString(STIFF_PRESSURE_DEP_ID, "false");
    setString(LAW_HARDENING_ID, "1");
    setString(SPREAD_ID, Double.toString(spread));
    setString(ODF_RESOLUTION_ID, "15");

    double s11 = 168.4;
    double s12 = 121.4;
    for (int i = ELLIPSOID_RATIO_ID; i < ELLIPSOID_ANGLE_ID; i++)
    parameterField[i] = new Parameter(this, getParameterString(i), ellipsoidRatios[i],
        ParameterPreferences.getDouble(getParameterString(i) + ".min", 0),
        ParameterPreferences.getDouble(getParameterString(i) + ".max", 100));
    for (int i = ELLIPSOID_ANGLE_ID; i < STIFFNESS_ID; i++)
      parameterField[i] = new Parameter(this, getParameterString(i), ellipsoidEulerAngles[i - ELLIPSOID_ANGLE_ID],
          ParameterPreferences.getDouble(getParameterString(i) + ".min", 0),
          ParameterPreferences.getDouble(getParameterString(i) + ".max", 360));
    for (int i = STIFFNESS_ID; i < STIFFNESS_P_DER_ID; i++) {
      if (i == STIFFNESS_ID || i == STIFFNESS_ID + 6 || i == STIFFNESS_ID + 11) // 11, 22, 33
        parameterField[i] = new Parameter(this, getParameterString(i), s11,
            ParameterPreferences.getDouble(getParameterString(i) + ".min", 1),
            ParameterPreferences.getDouble(getParameterString(i) + ".max", 1000));
      else if (i == STIFFNESS_ID + 15 || i == STIFFNESS_ID + 18 || i == STIFFNESS_ID + 20)  // 44, 55, 66
        parameterField[i] = new Parameter(this, getParameterString(i), 2.0 * (s11 - s12),
            ParameterPreferences.getDouble(getParameterString(i) + ".min", 1),
            ParameterPreferences.getDouble(getParameterString(i) + ".max", 500));
      else if (i == STIFFNESS_ID + 1 || i == STIFFNESS_ID + 2 || i == STIFFNESS_ID + 7)  // 12, 13, 23
        parameterField[i] = new Parameter(this, getParameterString(i), s12,
            ParameterPreferences.getDouble(getParameterString(i) + ".min", 1),
            ParameterPreferences.getDouble(getParameterString(i) + ".max", 300));
      else
        parameterField[i] = new Parameter(this, getParameterString(i), 0,
            ParameterPreferences.getDouble(getParameterString(i) + ".min", -100),
            ParameterPreferences.getDouble(getParameterString(i) + ".max", 100));
    }
    for (int i = STIFFNESS_P_DER_ID; i < THERMAL_EXPANSION_ID; i++) {
      parameterField[i] = new Parameter(this, getParameterString(i), 0.0,
          ParameterPreferences.getDouble(getParameterString(i) + ".min", -1000),
          ParameterPreferences.getDouble(getParameterString(i) + ".max", 1000));
    }
    for (int i = THERMAL_EXPANSION_ID; i < THERMAL_EXPANSION_ID + 6; i++) {
      parameterField[i] = new Parameter(this, getParameterString(i), 0.0,
          ParameterPreferences.getDouble(getParameterString(i) + ".min", -1),
          ParameterPreferences.getDouble(getParameterString(i) + ".max", 1));
    }

    int index = DISLOCATION_MODEL_ID;
    parameterField[index] = new Parameter(this, getParameterString(index), 0.35,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 1));
    parameterField[index] = new Parameter(this, getParameterString(index), 4.0,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 10));
    parameterField[index] = new Parameter(this, getParameterString(index), 1.0E-5,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 0.001));

    refreshComputation = true;
  }

  public int getNumberEnabled() {
    int total = 0;
    for (int i = 0; i < numberofelementSubL(1); i++)
      if (((EpscDeformationMode) subordinateloopField[1].elementAt(i)).isEnabled())
        total++;
    for (int i = 0; i < numberofelementSubL(2); i++)
      if (((EpscDeformationMode) subordinateloopField[2].elementAt(i)).isEnabled())
        total++;
    return total;
  }

  double[] macrostress = new double[6];
  int imodel = 0;

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    super.updateParametertoDoubleBuffering(false);

    fio_tmp = null;

    int k = 1;
    for (int i = 0; i < 6; i++)
      for (int j = 0; j < 6; j++) {
        if (i <= j)
          e0[i][j] = parameterValues[k++];
        else
          e0[i][j] = e0[j][i];
      }
    int factor = 1;
    for (int i = 0; i < 6; i++) {
      if (i == 3) factor++;
      macrostress[i] = parameterValues[k++] * factor;
    }
    checkForSymmetries(e0);
  }

  public void checkForSymmetries(double[][] e0) {
  }

  public boolean useTexture() {
    return stringField[USE_TEXTURE_ID].equalsIgnoreCase("true");
  }

  public void useTexture(boolean status) {
    if (status)
      stringField[USE_TEXTURE_ID] = "true";
    else
      stringField[USE_TEXTURE_ID] = "false";
  }

  public void useTexture(String value) {
    stringField[USE_TEXTURE_ID] = value;
  }

  public boolean useLargeStrains() {
    return stringField[LARGE_STRAIN_ID].equalsIgnoreCase("true");
  }

  public void useLargeStrains(boolean status) {
    if (status)
      stringField[LARGE_STRAIN_ID] = "true";
    else
      stringField[LARGE_STRAIN_ID] = "false";
  }

  public void useLargeStrains(String value) {
    stringField[LARGE_STRAIN_ID] = value;
  }

  public boolean usePreviousState() {
    return stringField[USE_PREVIOUS_ID].equalsIgnoreCase("true");
  }

  public void usePreviousState(boolean status) {
    if (status)
      stringField[USE_PREVIOUS_ID] = "true";
    else
      stringField[USE_PREVIOUS_ID] = "false";
  }

  public void usePreviousState(String value) {
    stringField[USE_PREVIOUS_ID] = value;
  }

  public boolean computeInternalStrains() {
    return stringField[CALCULATE_STRAIN_ID].equalsIgnoreCase("true");
  }

  public void computeInternalStrains(boolean status) {
    if (status)
      stringField[CALCULATE_STRAIN_ID] = "true";
    else
      stringField[CALCULATE_STRAIN_ID] = "false";
  }

  public void computeInternalStrains(String value) {
    stringField[CALCULATE_STRAIN_ID] = value;
  }

  public boolean computeInverseStrainPF() {
    return stringField[INVERSE_PF_ID].equalsIgnoreCase("true");
  }

  public void computeInverseStrainPF(boolean status) {
    if (status)
      stringField[INVERSE_PF_ID] = "true";
    else
      stringField[INVERSE_PF_ID] = "false";
  }

  public void computeInverseStrainPF(String value) {
    stringField[INVERSE_PF_ID] = value;
  }

  public boolean usePressureDependentStiffness() {
    return stringField[STIFF_PRESSURE_DEP_ID].equalsIgnoreCase("true");
  }

  public void usePressureDependentStiffness(boolean status) {
    if (status)
      stringField[STIFF_PRESSURE_DEP_ID] = "true";
    else
      stringField[STIFF_PRESSURE_DEP_ID] = "false";
  }

  public void usePressureDependentStiffness(String value) {
    stringField[STIFF_PRESSURE_DEP_ID] = value;
  }

  public int getMaxIterations() {
    return Integer.parseInt(getString(ITMAX_ID));
  }

  public double getErrorSC() {
    return Double.parseDouble(getString(ERROR_SC_ID));
  }

  public int getIterationsActiveSystem() {
    return Integer.parseInt(getString(IT_ACTIVE_SYS_ID));
  }

  public int getSkipTextureSteps() {
    return Integer.parseInt(getString(TEXTURE_IT_SKIP_ID));
  }

  public int getGrainShapeIndex() {
    return Integer.parseInt(getString(SHAPE_CTRL_ID));
  }

  public int getHardeningLawIndex() {
    return Integer.parseInt(getString(LAW_HARDENING_ID));
  }


/*  public void computeStrain(Sample asample) {

    Phase aphase = getPhase();
    computeStrain(aphase, asample);

  }*/

  double[][][] fio_tmp = null;
  OutputStream out = null;

  public String getCellType(Phase aphase) {
    switch (SpaceGroups.getSymmetryNumber(aphase.getSymmetry())) {
      case 0: // triclinic
        return "TRICL";
      case 1: // monoclinic
        return "MONOC";
      case 2: // orthorhombic
        return "ORTHO";
      case 3: // tetragonal
        return "TETRA";
      case 4: // trigonal
        return "TRIGO";
      case 5: // hexagonal
        return "HEXAG";
      case 6: // cubic
        return "CUBIC";
      default: {}
    }
    return "TRICL";
  }

  public int getStiffIndex(int i1, int j1) {

    int index = 0;
    for (int i = 0; i < 6; i++) {
      for (int j = 0; j < 6; j++) {
        if (i == i1 && j == j1)
          return index;
        if (i <= j)
          index++;
      }
    }
    return 0;
  }

  public int getPressureDependentValue() {
    if (stringField[STIFF_PRESSURE_DEP_ID].equalsIgnoreCase("true"))
      return 1;
    return 0;
  }

  public double getODFResolution() {
    return Double.parseDouble(getString(ODF_RESOLUTION_ID));
  }

  public void initAll(Sample asample, Phase aphase, String filename) {
    BufferedWriter output = null;
    String phaseFilename = aphase.getPhaseName() + ".sx";
    if (filename != null) {
      try {
        output = Misc.getWriter(filename);
        filename = null;
        output.write(aphase.getPhaseName() + userDefinedTitle);
        output.newLine();
        output.write(reminders[0]);
        output.newLine();
        output.write(grainShape + "                       Grain Shape and Orientation Control (0 non-evolving ellipsoid; 1 evolving ellipsoid; 2 individual ellipsoid)");
        output.newLine();
        for (int i = 0; i < ellipsoidRatios.length; i++)
          output.write(Fmt.format(ellipsoidRatios[i]) + "  ");
        output.write("         Initial Ellipsoid Ratios");
        output.newLine();
        for (int i = 0; i < ellipsoidEulerAngles.length; i++)
          output.write(Fmt.format(ellipsoidEulerAngles[i]) + "  ");
        output.write("         Initial Euler Angle Ellipsoid Axes");
        output.newLine();
        output.write(reminders[1]);
        output.newLine();
        output.write(aphase.getPhaseName() + ".red");
        output.newLine();
        String texv = "0";
        if (textureEvolution)
          texv = "1";
        output.write(texv + "\t\t\tRotations Due to Slip - IE Texture evolution (0 for no, 1 for yes)");
        output.newLine();
        output.write(reminders[2]);
        output.newLine();
        output.write(phaseFilename);
        output.newLine();
        output.write(reminders[3]);
        output.newLine();
        output.write(getString(ITMAX_ID) + "                     itmax_mod");
        output.newLine();
        output.write(getString(ERROR_SC_ID) + "                     error_mod");
        output.newLine();
        output.write(getString(IT_ACTIVE_SYS_ID) + "                     itmax_grain");
        output.newLine();

        int prevproc = 0;
        if (usePreviousState())
          prevproc = 1;
        int i_diff_dir = 0;
        if (computeInternalStrains())
          i_diff_dir = 1;
        int i_strpf = 0;
        if (computeInverseStrainPF())
          i_strpf = 1;

        output.write(reminders[4]);
        output.newLine();
        output.write(prevproc + "                       i_prev_proc - Reads state from previous process (1=YES or 0=NO) and related file:");
        output.newLine();
        output.write(prev_proc_file);
        output.newLine();
        output.write(getString(TEXTURE_IT_SKIP_ID) + "                       itexskip - Sets Frequency of Texture Downloads");
        output.newLine();
        output.write(i_diff_dir + "       i_diff_dir    Read diffracting planes and directions (1=YES or 0=NO) and file:");
        output.newLine();
        filename = aphase.getPhaseName() + ".dif";
        output.write(filename);
        output.newLine();
        output.write(i_strpf + "       i_strpf       Read directions and calculate strain pole figure (1=YES or 0=NO):");
        output.newLine();
        output.write(reminders[5]);
        output.newLine();
        int nproc = numberofelementSubL(THERMO_MECHANICAL_ID);
        output.write(nproc + "                       nproc");
        output.newLine();
        output.write(reminders[6]);
        output.newLine();
        for (int i = 0; i < nproc; i++) {
          output.write(((ThermoMechanicalProcess) subordinateloopField[THERMO_MECHANICAL_ID].elementAt(i)).getFilename());
          output.newLine();
        }

        output.flush();
        output.close();
      } catch (Exception io) {
        io.printStackTrace();
      }
    }

    filename = getFilePar().getDirectory() + aphase.getPhaseName() + ".dif";
    if (filename != null) {
      try {
        output = Misc.getWriter(filename);
        filename = null;
        output.write("*DIFFRACTING PLANES AND DIRECTION FOR " + aphase.getPhaseName());
        output.newLine();
        output.write("*Number of diffraction directions and diffracting angle spread: 0,0 is 3, 90,90 is 2, 90,0 is 1");
        output.newLine();

        int hkln = aphase.gethklNumber();
        Vector<PlaneDirections> allData = new Vector<>(hkln);

        int ndif = 0;
        for (int j = 0; j < hkln; j++) {
          Reflection refl = aphase.getReflectionVector().elementAt(j);
          if (refl.isGoodforTexture()) {
            Vector<double[]> pf_data = new Vector<>(100, 100);
            int numberDatasets = asample.activeDatasetsNumber();
            for (int i = 0; i < numberDatasets; i++) {
              DataFileSet dataset = asample.getActiveDataSet(i);
              int radCount = dataset.getInstrument().getRadiationType().getLinesCount();
              for (int k = 0; k < dataset.activedatafilesnumber(); k++) {
                DiffrDataFile datafile = dataset.getActiveDataFile(k);
                for (int ppp = 0; ppp < datafile.positionsPerPattern; ppp++) {
                  for (int l = 0; l < radCount; l++) {
                    double[] pfd = new double[3];
                    pfd[2] = datafile.getExperimentalTextureFactors(aphase, j)[ppp][l];
                    double position = datafile.getPositions(aphase)[j][ppp][l];
                    if (!Double.isNaN(pfd[2]) && datafile.isInsideRange(position)) {
                      double[] angles = datafile.getTextureAngles(position, ppp);
                      pfd[0] = angles[0];
                      pfd[1] = angles[1];
                      pf_data.add(pfd);
                      ndif++;
                    }
                  }
                }
              }
            }
            PlaneDirections planeData = new PlaneDirections(refl.getH(), refl.getK(), refl.getL(), pf_data);
            allData.add(planeData);
          }
        }

        output.write("    " + ndif + "  " + getString(SPREAD_ID) + "   ndif  Spread");
        output.newLine();
        output.write("*Plane type and direction angle:");
        output.newLine();
        output.write("*n3 or n4       theta           phi");
        output.newLine();

        for (PlaneDirections planedir: allData) {
          for (double[] angle: planedir.angles()) {
            output.write(" " + planedir.h() + " " + " " + planedir.k() + " " + planedir.l() +
                "           " + angle[0] + "      " + angle[1]);
            output.newLine();
          }
        }

        output.flush();
        output.close();
      } catch (Exception io) {
        io.printStackTrace();
      }
    }

    filename = getFilePar().getDirectory() + phaseFilename;
    if (filename != null) {
      try {
        output = Misc.getWriter(filename);
        output.write("*Material: " + aphase.getPhaseName());
        output.newLine();
        output.write(getCellType(aphase) + "            crysym");
        output.newLine();
        for (int i = 0; i < 6; i++)
          output.write(aphase.getCellValue(i) + "   ");
        output.write("unit cell axes and angles");
        output.newLine();

        output.write("Elastic stiffness of single crystal");
        output.newLine();
        int index = 0;
        for (int i = 0; i < 6; i++) {
          for (int j = 0; j < 6; j++) {
            if (i > j) {
              int indexr = getStiffIndex(j, i);
              output.write(parameterField[STIFFNESS_ID + indexr].getValue() + " ");
            } else
              output.write(parameterField[STIFFNESS_ID + index++].getValue() + " ");
            if (j == 5)
              output.newLine();
          }
        }

        output.write("*Large elastic strain & pressure dependent Cij (kSM=0 or 1)");
        output.newLine();
        int kSM = getPressureDependentValue();
        output.write(" " + kSM);
        output.newLine();

        if (kSM == 1) {
          output.write("FIRST PRESSURE DERIVATIVE OF SINGLE CRYSTAL ELASTIC STIFFNESS");
          output.newLine();
          index = 0;
          for (int i = 0; i < 6; i++) {
            for (int j = 0; j < 6; j++) {
              if (i > j) {
                int indexr = getStiffIndex(j, i);
                output.write(parameterField[STIFFNESS_P_DER_ID + indexr].getValue() + " ");
              } else
                output.write(parameterField[STIFFNESS_P_DER_ID + index++].getValue() + " ");
              if (j == 5)
                output.newLine();
            }
          }
        }

        output.write("*Thermal expansion coefficients of single crystal (in crystal axis)");
        output.newLine();
        for (int i = 0; i < 6; i++)
          output.write(" " + parameterField[THERMAL_EXPANSION_ID + i].getValue() + " ");
        output.newLine();

        int totalModes = (numberofelementSubL(1) + numberofelementSubL(2));
        output.write("*Info about slip & twinning modes in this file:");
        output.newLine();
        output.write("  " + totalModes + "          nmodesx    (total # of modes listed in file)");
        output.newLine();
        int modesEnabled = getNumberEnabled();
        output.write("  " + modesEnabled + "          nmodes     (# of modes to be used in the calculation)");
        output.newLine();
        if (modesEnabled > 0) {
          output.write("  ");
          for (int i = 0; i < numberofelementSubL(1); i++) {
            if (((EpscDeformationMode) subordinateloopField[1].elementAt(i)).isEnabled())
              output.write((i + 1) + " ");
          }
          for (int i = 0; i < numberofelementSubL(2); i++) {
            if (((EpscDeformationMode) subordinateloopField[2].elementAt(i)).isEnabled())
              output.write((numberofelementSubL(1) + i + 1) + " ");
          }
          output.write("  mode(i)    (label of the modes to be used)");
          output.newLine();
        }
        for (int i = 0; i < numberofelementSubL(1); i++) {
          EpscSlipMode mode = (EpscSlipMode) subordinateloopField[1].elementAt(i);
          output.write(mode.getString(TITLE_ID));
          output.newLine();
          int slipOrTwin = 1;
          int twinOrSlip = 0;
          output.write(" " + (i + 1) + " " + mode.numberofelementSubL(0) + "  20   " + slipOrTwin +
              "   " + twinOrSlip + "                    modex,nsmx,nrsx,iopsysx,itwx");
          output.newLine();
          if (aphase.hasHexagonalAxes()) {
            for (int j = 0; j < mode.numberofelementSubL(0); j++) {
              output.write("  " + mode.getPlaneAsStringHex(j) + "           " + mode.getDirectionAsStringHex(j));
              output.newLine();
            }
          } else {
            for (int j = 0; j < mode.numberofelementSubL(0); j++) {
              output.write("  " + mode.getPlaneAsString(j) + "           " + mode.getDirectionAsString(j));
              output.newLine();
            }
          }
        }

        for (int i = 0; i < numberofelementSubL(2); i++) {
          EpscTwinningMode mode = (EpscTwinningMode) subordinateloopField[2].elementAt(i);
          output.write(mode.getString(TITLE_ID));
          output.newLine();
          int slipOrTwin = 0;
          int twinOrSlip = 1;
          output.write(" " + (numberofelementSubL(1) + i + 1) + " " + mode.numberofelementSubL(0) + "  20   " + slipOrTwin +
              "   " + twinOrSlip + "                    modex,nsmx,nrsx,iopsysx,itwx");
          output.newLine();
          output.write(" " + mode.getCharacteristicTwinStress() + "                           stwx");
          output.newLine();
          if (aphase.hasHexagonalAxes()) {
            for (int j = 0; j < mode.numberofelementSubL(0); j++) {
              output.write("  " + mode.getPlaneAsStringHex(j) + "           " + mode.getDirectionAsStringHex(j));
              output.newLine();
            }
          } else {
            for (int j = 0; j < mode.numberofelementSubL(0); j++) {
              output.write("  " + mode.getPlaneAsString(j) + "           " + mode.getDirectionAsString(j));
              output.newLine();
            }
          }
        }

        output.write("Constitutive law");
        output.newLine();
        output.write("  " + (Integer.parseInt(getString(LAW_HARDENING_ID)) + 1));
        output.newLine();
        if (modesEnabled > 0) {
          output.write("DISLOCATION MODEL");
          output.newLine();
          for (int i = 0; i < 3; i++)
            output.write("  " + parameterField[DISLOCATION_MODEL_ID + i].getValue());
          output.write("          !INTERACTION CONSTANT, Q IN EQ. (3.14),REF STRAIN RATE (1/s) ");
          output.newLine();
          for (int i = 0; i < numberofelementSubL(1); i++) {
            EpscSlipMode mode = (EpscSlipMode) subordinateloopField[1].elementAt(i);
            if (mode.isEnabled()) {
              output.write(mode.getString(TITLE_ID) + "------------------------------------------------------------------------------------");
              output.newLine();
              output.write(" " + mode.parameterField[0].getValue() + " " + mode.parameterField[1].getValue() + "                !BURG (m), NORM ACTENER g IN EQ. (3.12) (0.00375)");
              output.newLine();
              output.write(" " + mode.parameterField[2].getValue() + " " + mode.parameterField[3].getValue() + "                !K1 IN EQ. (3.8) (1/m), DRAG STRESS-D IN EQ. (3.12) (MPa) ");
              output.newLine();
              output.write(" " + mode.parameterField[4].getValue() + "                               !EDOT_0 IN EQ. (3.12)");
              output.newLine();
              output.write(" " + mode.parameterField[5].getValue() + " " + mode.parameterField[6].getValue() + "                !INITIAL RHO_S (1/m^2), INITIAL RHO_DEB FOR EACH SLIP MODE (1/m^2)");
              output.newLine();
              output.write(" " + mode.parameterField[7].getValue() + " " + mode.parameterField[8].getValue() + " " + mode.parameterField[9].getValue() + "  !A,B,C FOR ATHERMAL TAU = A + B * EXP(-TEMP/C) EQ. (3.17) ");
              output.newLine();
              output.write(" " + mode.parameterField[10].getValue() + " " + mode.parameterField[11].getValue() + "                !TLATENT HARDENING BY THIS SLIP MODE ON TWIN1, TWIN2:  C IN EQ. (3.28)");
              output.newLine();
              output.write(" " + mode.parameterField[12].getValue() + " " + mode.parameterField[13].getValue() + " " + mode.parameterField[14].getValue() + "  !FOR HPFAC COEF FOR THIS SLIP MODE FOR GRAIN BOUNDARY, TWIN1 BOUNDARY, TWIN2 BOUNDARY");
              output.newLine();
              output.write(" " + mode.parameterField[15].getValue() + " " + mode.parameterField[16].getValue() + " " + mode.parameterField[17].getValue() + "  !A_deb_a, A_deb_b, A_deb_c FOR A IN EQ. (3.15) A_deb_a+A_deb_b*LOG(1+TEMP/A_deb_c)");
              output.newLine();
            }
          }
          for (int i = 0; i < numberofelementSubL(2); i++) {
            EpscTwinningMode mode = (EpscTwinningMode) subordinateloopField[2].elementAt(i);
            if (mode.isEnabled()) {
              output.write(mode.getString(TITLE_ID) + "------------------------------------------------------------------------------------");
              output.newLine();
              output.write(" " + mode.parameterField[1].getValue() + " " + mode.parameterField[2].getValue() + " " + mode.parameterField[3].getValue() + "  !A,B,C FOR TAU_CRIT IN EQ. (3.26):  A + B*exp(-TEMP/C)");
              output.newLine();
              output.write(" " + mode.parameterField[4].getValue() + " " + mode.parameterField[5].getValue() + " " + mode.parameterField[6].getValue() + "  !A,B,C FOR TAU_PROP IN EQ. (3.26):  A + B*exp(-TEMP/C)");
              output.newLine();
              output.write(" " + mode.parameterField[7].getValue() + "                               !TWIN BURGERS VECTOR (m) (see Yoo, 1969)");
              output.newLine();
              output.write(" " + mode.parameterField[8].getValue() + " " + mode.parameterField[9].getValue() + "         !IniFraction,TwinCRSS ");
              output.newLine();
            }
          }
        }

        output.flush();
        output.close();
      } catch (Exception io) {
        io.printStackTrace();
      }
    }

    filename = getFilePar().getDirectory() + aphase.getPhaseName() + ".red";
    if (filename != null) {
      try {
        output = Misc.getWriter(filename);
        output.write(" " + aphase.getPhaseName());
        output.newLine();
        output.write(" CALCULATED BY MAUD");
        output.newLine();
        output.write("DISCRETE TEXTURE FROM TEXTURE MODEL " + aphase.getTextureModel());
        output.newLine();

        Texture texModel = aphase.getActiveTexture();

        double res = getODFResolution();
        double half_res = res * 0.5;
        int laueGroup = SpaceGroups.getLGNumberSiegfriedConv(aphase.getPointGroup());
        int[] nae = DiscreteODFTexture.getCellNumber(laueGroup, res);

        output.write("B    " + (nae[0] * nae[1] * nae[2]));
        output.newLine();

        boolean useODF = useTexture();

//        ψRoe = ψ1,Bunge – π/2
//        θRoe = φBunge
//        φRoe = ψ2, Bunge + π/2

        for (int ng = 0; ng < nae[0]; ng++) {
          for (int nb = 0; nb < nae[1]; nb++) {
            for (int na = 0; na < nae[2]; na++) {
              double alpha = na * res + half_res - 90.0;
              double beta = nb * res + half_res;
              double gamma = ng * res + half_res + 90.0;
              output.write("  " + alpha + "  " + beta + "  " + gamma + "  ");
              if (alpha < 0)
                alpha = alpha + 360.0;
              if (gamma >= 360.0)
                gamma = gamma - 360.0;
              double odf = 1.0;
              if (useODF)
                odf = texModel.getODF(alpha * Constants.DEGTOPI, beta * Constants.DEGTOPI, gamma * Constants.DEGTOPI);
              output.write(Double.toString(odf));
              output.newLine();
            }
          }
        }

        output.flush();
        output.close();
      } catch (Exception io) {
        io.printStackTrace();
      }
    }

    for (int i = 0; i < numberofelementSubL(THERMO_MECHANICAL_ID); i++)
      ((ThermoMechanicalProcess) subordinateloopField[THERMO_MECHANICAL_ID].elementAt(i)).writeInputFile();

  }

  public Vector<Double> loadFhkl(Phase aphase, String fhklFilename) {

    String line = null;
    String theGoodLine = null;
    BufferedReader reader = Misc.getReader(fhklFilename);
//    System.out.println("Reading: " + fhklFilename);
    Vector<Double> hklList = new Vector<Double>(100, 100);
    if (reader != null) {
      try {
        line = reader.readLine(); // 0 0 0
        boolean next = false;
        while (line != null) {
          line = reader.readLine();
          if (line != null && line.length() > 0) {
            if (next) {
              theGoodLine = line;
              next = false;
            }
            if (line.startsWith(" Temp      Eref"))
              next = true;
          }
        }
      } catch (Exception e) {
        System.out.println("Error in loading the Fhkl file!");
        e.printStackTrace();
      }
      try {
        reader.close();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
    if (theGoodLine != null) {
      StringTokenizer st = new StringTokenizer(theGoodLine, " ,\t\r\n");
      int index = 0;
      while (st.hasMoreTokens()) {
        String data = st.nextToken();
        if (index++ > 6) {
          hklList.add(Double.parseDouble(data));
        }
      }
    }
    return hklList;
  }

  public void prepareComputation(Phase aphase, Sample asample) {
    log_output = /* getFilePar().isStrainComputationPermitted() && */getFilePar().logOutput();
    if (log_output)
      out = getFilePar().getResultStream();
    update(false);
    if (useTexture())
      irandom = 0;
    else
      irandom = 1;

    programName = MaudPreferences.getPref("epsc4.executable_name", programName);

    insName = MaudPreferences.getPref("epsc4.instr_filename", insName);
    initAll(asample, aphase, getFilePar().getDirectory() + insName);

    // call epsc4
    String epscProgram = Constants.startingAppDirectory + "epsc4" + File.separator + programName;
    try {
      if (programName.equalsIgnoreCase("internal"))
        it.unitn.ing.rista.diffr.rsa.epsc.EPSC4.run(getFilePar().getDirectory(), insName);
      else {
        System.out.println("Executing: " + Misc.checkForWindowsPath(epscProgram)); // + " " + Misc.checkForWindowsPath(insName));
        Executable process = new Executable(Misc.checkForWindowsPath(epscProgram),
            Misc.checkForWindowsPath(getFilePar().getDirectory()), null, false); //new String[] {Misc.checkForWindowsPath(insName)});
        process.start();
        while (!process.getStatus().equals(Executable.TERMINATED))
          Thread.currentThread().sleep(100);
        System.out.println("Execution of EPSC4 terminate with code: " + process.getTerminationResult());
        process.cleanUp();
      }

      String hklFilename = getFilePar().getDirectory() + "epsc9.out";
      if (Constants.windoze) {
        hklFilename = Misc.getUserDir() + "\\" + "epsc9.out";
      }
//      System.out.println("EPSC4 results saved in " + hklFilename);
      Vector<Double> hklList = loadFhkl(aphase, hklFilename);

      if (hklList.size() > 0) {
        int hkln = aphase.gethklNumber();
        int ndif = 0;
        int index = 0;
        for (int j = 0; j < hkln; j++) {
          Reflection refl = aphase.getReflectionVector().elementAt(j);
          if (refl.isGoodforTexture()) {
            int numberDatasets = asample.activeDatasetsNumber();
            for (int i = 0; i < numberDatasets; i++) {
              DataFileSet dataset = asample.getActiveDataSet(i);
              int radCount = dataset.getInstrument().getRadiationType().getLinesCount();
              for (int k = 0; k < dataset.activedatafilesnumber(); k++) {
                DiffrDataFile datafile = dataset.getActiveDataFile(k);
                double[][][][] strains = datafile.getStrainFactors(aphase);
                for (int ppp = 0; ppp < datafile.positionsPerPattern; ppp++) {
                  for (int l = 0; l < radCount; l++) {
                    double pfd = datafile.getExperimentalTextureFactors(aphase, j)[ppp][l];
                    double position = datafile.getPositions(aphase)[j][ppp][l];
                    if (!Double.isNaN(pfd) && datafile.isInsideRange(position)) {
                      strains[1][j][ppp][l] = hklList.elementAt(ndif++).doubleValue();
                    }
                  }
                }
              }
            }
          }
        }
      }

    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
  }

  public double computeStrain(Reflection refl, double[] strain_angles, double previousStrain) { // you don't need to modify this unless
//    actualReflexIndex = getPhase().getReflexIndex(refl);
    return previousStrain;
  }

  public double computeStrain(double psi, double beta, double chi, double phi) {
    // Angles must be in radiants
    // psi and beta are the polar and azimuthal angles for the crystal setting
    // phi and chi for the sample

    double cfhi = Math.cos(beta);
    double sfhi = Math.sin(beta);
    double cthi = Math.cos(psi);
    double sthi = Math.sin(psi);
    double ctyj = Math.cos(chi);
    double styj = Math.sin(chi);
    double cfyj = Math.cos(phi);
    double sfyj = Math.sin(phi);
    if (debug_output) {
      try {
        printLine(out, "fhi " + beta * Constants.PITODEG + ", " +
            "thi " + psi * Constants.PITODEG + ", " +
            "tyj " + chi * Constants.PITODEG + ", " +
            "fyj " + phi * Constants.PITODEG
        );
      } catch (IOException io) {
        io.printStackTrace();
      }
    }
/*    double[] sla33 = subsla33(imodel, irandom, bk0_tmp,
        sthi, cthi, sfhi, cfhi, 1,
        styj, ctyj,
        sfyj, cfyj, pfthreshold_tmp, fio_tmp);*/
    double strain33 = 0.0;

/*    for (int i = 0; i < 6; i++)
      strain33 += sla33[i] * macrostress[i];*/
    return strain33;
  }


  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JTSStrainOptionsD(parent, this);
    return adialog;
  }

  class JTSStrainOptionsD extends JOptionsDialog {

    String[] labels = {
        "Title (not mandatory) : ",
        "Use texture from model",
        "Use large strain model",
        "Iterations max  (100) : ",
        "Error self consistent : ",
        "Iterations active syst: ",
        "Start from previous solution",
        "Skip texture cal every: ",
        "Compute internal strain",
        "Compute strain inverse pole figures",
        "Pressure dependent stiffness",
        "Hardening constitutive law: ",
        "Averaging spread (deg): ",
        "ODF resolution (deg): "
    };
    String[] labelsPar = {
        "Ellipsoid ratio x : ",
        "Ellipsoid ratio y : ",
        "Ellipsoid ratio z : ",
        "Orientation alpha : ",
        "Orientation beta  : ",
        "Orientation gamma : "
    };
    String[] thermalExp = {
        "Thermal expansion 11: ",
        "Thermal expansion 12: ",
        "Thermal expansion 13: ",
        "Thermal expansion 22: ",
        "Thermal expansion 23: ",
        "Thermal expansion 33: "
    };
    String[] grainShapeL = {
        "No ellipsoid evolution",
        "Evolution with average grain shape",
        "Individual grains"
    };
    String[] hardeningL = {
        "Voce type",
        "Dislocations density"
    };

    String[] dislocationL = {
        "Interaction constant : ",
        "Q in Equation (3.14) : ",
        "Ref strain rate (1/s): "
    };

    JTextField titleTF = null;
    JCheckBox textureCB;
    JCheckBox largeStrainCB = null;
    JTextField itmaxTF = null;
    JTextField errorTF = null;
    JTextField iterActiveSysTF = null;
    JTextField spreadTF = null;
    JTextField odfResTF = null;
    JCheckBox usePreviousCB = null;
    JTextField skipTextureTF = null;
    JCheckBox internalStrainsCB = null;
    JCheckBox inversePoleCB = null;

    JComboBox hardeningLawCB = null;
    JComboBox grainShapeCB = null;

    JTextField[] ellipsoidPars = null;
    JTextField[] thermalExpansionPars = null;

    JTextField[] cijTF = null;

    JCheckBox pressureDepStiffnessCB = null;
    JTextField[] cijpTF = null;

    JTextField[] dislocationPars = null;

    JSubordinateLoopListPane slipPanel;
    JSubordinateLoopListPane twinPanel;

    JSubordinateLoopListPane processesPanel;

    public JTSStrainOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel p1 = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(BorderLayout.NORTH, p1);
      p1.add(new JLabel(labels[0]));
      titleTF = new JTextField(48);
      p1.add(titleTF);

      JTabbedPane ptabP = new JTabbedPane();
      principalPanel.add(BorderLayout.CENTER, ptabP);
      String tpString[] = {"General", "Grains/Thermal exp.", "Stiffness", "Pressure dep. stiffness",
          "Hardening"};

      // General TabPanel
      JPanel p3 = new JPanel();
      p3.setLayout(new FlowLayout(FlowLayout.CENTER, 3, 3));
      ptabP.addTab(tpString[0], null, p3);
      JPanel jPanel8 = new JPanel(new GridLayout(0, 2, 3, 3));
      p3.add(jPanel8);
      JPanel jp = new JPanel(new FlowLayout(FlowLayout.LEFT));
      jPanel8.add(jp);
      textureCB = new JCheckBox(labels[1]);
      textureCB.setToolTipText("True = ODF from Maud texture model is used");
      jp.add(textureCB);
      jp = new JPanel(new FlowLayout(FlowLayout.LEFT));
      jPanel8.add(jp);
      jp.add(new JLabel(labels[3]));
      itmaxTF = new JTextField(12);
      itmaxTF.setToolTipText("Max number of iterations in the self consistent model");
      jp.add(itmaxTF);
      jp = new JPanel(new FlowLayout(FlowLayout.LEFT));
      jPanel8.add(jp);
      largeStrainCB = new JCheckBox(labels[2]);
      largeStrainCB.setToolTipText("Large strain model = rotations due to slip are included");
      jp.add(largeStrainCB);
      jp = new JPanel(new FlowLayout(FlowLayout.LEFT));
      jPanel8.add(jp);
      jp.add(new JLabel(labels[4]));
      errorTF = new JTextField(12);
      errorTF.setToolTipText("Max error for self consistent elasto-plastic equation");
      jp.add(errorTF);
      jp = new JPanel(new FlowLayout(FlowLayout.LEFT));
      jPanel8.add(jp);
      usePreviousCB = new JCheckBox(labels[6]);
      usePreviousCB.setToolTipText("Read grains state from previous process");
      jp.add(usePreviousCB);
      jp = new JPanel(new FlowLayout(FlowLayout.LEFT));
      jPanel8.add(jp);
      jp.add(new JLabel(labels[5]));
      iterActiveSysTF = new JTextField(12);
      iterActiveSysTF.setToolTipText("Max iterations to find active systems");
      jp.add(iterActiveSysTF);
      jp = new JPanel(new FlowLayout(FlowLayout.LEFT));
      jPanel8.add(jp);
      internalStrainsCB = new JCheckBox(labels[8]);
      internalStrainsCB.setToolTipText("Compute internal strains for specified diffraction directions");
      jp.add(internalStrainsCB);
      jp = new JPanel(new FlowLayout(FlowLayout.LEFT));
      jPanel8.add(jp);
      jp.add(new JLabel(labels[7]));
      skipTextureTF = new JTextField(12);
      skipTextureTF.setToolTipText("Skip texture output for the amount of steps specified");
      jp.add(skipTextureTF);
      jp = new JPanel(new FlowLayout(FlowLayout.LEFT));
      jPanel8.add(jp);
      inversePoleCB = new JCheckBox(labels[9]);
      inversePoleCB.setToolTipText("Compute inverse strain pole figures");
      jp.add(inversePoleCB);
      jp = new JPanel(new FlowLayout(FlowLayout.LEFT));
      jPanel8.add(jp);
      jp.add(new JLabel(labels[12]));
      spreadTF = new JTextField(12);
      spreadTF.setToolTipText("Set the spread in degrees to average around a direction");
      jp.add(spreadTF);
      jp = new JPanel(new FlowLayout(FlowLayout.LEFT));
      jPanel8.add(jp);
      jp.add(new JLabel(labels[13]));
      odfResTF = new JTextField(12);
      odfResTF.setToolTipText("Set the odf resolution in degrees for the grain population");
      jp.add(odfResTF);

      // Grains tabPanel
      p3 = new JPanel(new BorderLayout(3, 3));
      ptabP.addTab(tpString[1], null, p3);

      jPanel8 = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
      p3.add(BorderLayout.NORTH, jPanel8);
      jPanel8.add(new JLabel("Grain morphology ctrl: "));
      grainShapeCB = new JComboBox();
      for (int i = 0; i < grainShapeL.length; i++)
        grainShapeCB.addItem(grainShapeL[i]);
      grainShapeCB.setToolTipText("Grain morphology control for Eshelby tensor calculation");
      jPanel8.add(grainShapeCB);

      jPanel8 = new JPanel();
      jPanel8.setLayout(new GridLayout(0, 2, 3, 3));
      p3.add(BorderLayout.CENTER, jPanel8);
      ellipsoidPars = new JTextField[6];
      thermalExpansionPars = new JTextField[6];
      for (int i = 0; i < 6; i++) {
        JPanel jpl = new JPanel(new FlowLayout(FlowLayout.LEFT));
        jPanel8.add(jpl);
        jpl.add(new JLabel(labelsPar[i]));
        ellipsoidPars[i] = new JTextField(Constants.FLOAT_FIELD);
        jpl.add(ellipsoidPars[i]);
        jpl = new JPanel(new FlowLayout(FlowLayout.LEFT));
        jPanel8.add(jpl);
        jpl.add(new JLabel(thermalExp[i]));
        thermalExpansionPars[i] = new JTextField(Constants.FLOAT_FIELD);
        jpl.add(thermalExpansionPars[i]);
      }

      p3 = new JPanel(new FlowLayout());
      ptabP.addTab(tpString[2], null, p3);
      jPanel8 = new JPanel();
      jPanel8.setLayout(new GridLayout(0, 6, 1, 1));
      p3.add(jPanel8);
      cijTF = new JTextField[21];
      int ij = 0;
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          if (i <= j) {
            cijTF[ij] = new JTextField(Constants.FLOAT_FIELD);
            cijTF[ij].setText("0");
            jPanel8.add(cijTF[ij++]);
          } else
            jPanel8.add(new JLabel("-"));
        }
      }

      JPanel p2 = new JPanel(new BorderLayout(3, 3));
      ptabP.addTab(tpString[3], null, p2);
      p3 = new JPanel(new FlowLayout());
      p2.add(BorderLayout.NORTH, p3);
      pressureDepStiffnessCB = new JCheckBox(labels[10]);
      p3.add(pressureDepStiffnessCB);
      p3 = new JPanel(new FlowLayout());
      p2.add(BorderLayout.CENTER, p3);
      jPanel8 = new JPanel();
      jPanel8.setLayout(new GridLayout(0, 6, 1, 1));
      p3.add(jPanel8);
      cijpTF = new JTextField[21];
      ij = 0;
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          if (i <= j) {
            cijpTF[ij] = new JTextField(Constants.FLOAT_FIELD);
            cijpTF[ij].setText("0");
            jPanel8.add(cijpTF[ij++]);
          } else
            jPanel8.add(new JLabel("-"));
        }
      }

      p2 = new JPanel(new BorderLayout(3, 3));
      ptabP.addTab(tpString[4], null, p2);
      p3 = new JPanel(new FlowLayout());
      p2.add(BorderLayout.NORTH, p3);
      p3.add(new JLabel(labels[11]));
      hardeningLawCB = new JComboBox();
      for (int i = 0; i < hardeningL.length; i++)
        hardeningLawCB.addItem(hardeningL[i]);
      p3.add(hardeningLawCB);
      p3 = new JPanel(new FlowLayout(FlowLayout.CENTER));
      p2.add(BorderLayout.CENTER, p3);
      JPanel dp = new JPanel(new GridLayout(0, 2));
      p3.add(dp);
      dislocationPars = new JTextField[dislocationL.length];
      for (int i = 0; i < dislocationL.length; i++) {
        dp.add(new JLabel(dislocationL[i]));
        dislocationPars[i] = new JTextField(Constants.FLOAT_FIELD);
        dp.add(dislocationPars[i]);
      }
      slipPanel = new JSubordinateLoopListPane(this, "Slip modes");
      p3.add(slipPanel);
      twinPanel = new JSubordinateLoopListPane(this, "Twinning modes");
      p3.add(twinPanel);

      p3 = new JPanel(new FlowLayout());
      processesPanel = new JSubordinateLoopListPane(this, "Thermomechanical processes");
      p3.add(processesPanel);
      principalPanel.add(BorderLayout.SOUTH, p3);

      setTitle("EPSC4 options panel");
      initParameters();
      pack();

    }

    public void initParameters() {
      for (int i = 0; i < STIFFNESS_ID; i++)
        addComponenttolist(ellipsoidPars[i], parameterField[i]);
      for (int i = 0; i < cijTF.length; i++)
        addComponenttolist(cijTF[i], parameterField[i + STIFFNESS_ID]);
      for (int i = 0; i < cijpTF.length; i++)
        addComponenttolist(cijpTF[i], parameterField[i + STIFFNESS_P_DER_ID]);
      for (int i = 0; i < 6; i++)
        addComponenttolist(thermalExpansionPars[i], parameterField[THERMAL_EXPANSION_ID + i]);

      titleTF.setText(getString(TITLE_ID));

      textureCB.setSelected(useTexture());
      largeStrainCB.setSelected(useLargeStrains());
      usePreviousCB.setSelected(usePreviousState());
      internalStrainsCB.setSelected(computeInternalStrains());
      inversePoleCB.setSelected(computeInverseStrainPF());

      itmaxTF.setText(getString(ITMAX_ID));
      errorTF.setText(getString(ERROR_SC_ID));
      iterActiveSysTF.setText(getString(IT_ACTIVE_SYS_ID));
      skipTextureTF.setText(getString(TEXTURE_IT_SKIP_ID));
      spreadTF.setText(getString(SPREAD_ID));
      odfResTF.setText(getString(ODF_RESOLUTION_ID));

      pressureDepStiffnessCB.setSelected(usePressureDependentStiffness());

      for (int i = 0; i < dislocationPars.length; i++)
        addComponenttolist(dislocationPars[i], parameterField[DISLOCATION_MODEL_ID + i]);

      grainShapeCB.setSelectedIndex(getGrainShapeIndex());
      hardeningLawCB.setSelectedIndex(getHardeningLawIndex());

      processesPanel.setList(EPSCmodel.this, THERMO_MECHANICAL_PROC_ID);
      slipPanel.setList(EPSCmodel.this, SLIP_MODE_ID);
      twinPanel.setList(EPSCmodel.this, TWINNING_MODE_ID);
    }

    public void retrieveParameters() {
      for (int i = 0; i < STIFFNESS_ID; i++)
        parameterField[i].setValue(ellipsoidPars[i].getText());
      for (int i = 0; i < cijTF.length; i++)
        parameterField[i + STIFFNESS_ID].setValue(cijTF[i].getText());
      for (int i = 0; i < cijpTF.length; i++)
        parameterField[i + STIFFNESS_P_DER_ID].setValue(cijpTF[i].getText());
      for (int i = 0; i < 6; i++)
        parameterField[THERMAL_EXPANSION_ID + i].setValue(thermalExpansionPars[i].getText());

      setString(TITLE_ID, titleTF.getText());

      useTexture(textureCB.isSelected());
      useLargeStrains(largeStrainCB.isSelected());
      usePreviousState(usePreviousCB.isSelected());
      computeInternalStrains(internalStrainsCB.isSelected());
      computeInverseStrainPF(inversePoleCB.isSelected());

      setString(ITMAX_ID, itmaxTF.getText());
      setString(ERROR_SC_ID, errorTF.getText());
      setString(IT_ACTIVE_SYS_ID, iterActiveSysTF.getText());
      setString(TEXTURE_IT_SKIP_ID, skipTextureTF.getText());
      setString(SPREAD_ID, spreadTF.getText());
      setString(ODF_RESOLUTION_ID, odfResTF.getText());

      usePressureDependentStiffness(pressureDepStiffnessCB.isSelected());
      for (int i = 0; i < dislocationPars.length; i++)
        parameterField[DISLOCATION_MODEL_ID + i].setValue(dislocationPars[i].getText());

      int index = grainShapeCB.getSelectedIndex();
      setString(SHAPE_CTRL_ID, Integer.toString(index));
      index = hardeningLawCB.getSelectedIndex();
      setString(LAW_HARDENING_ID, Integer.toString(index));
    }

  }

}

