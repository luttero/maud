package it.unitn.ing.rista.diffr.rsa;

/*
 * @(#)EpscSlipMode.java created 31/03/2025 Casalino
 *
 * Copyright (c) 2025 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.ParameterPreferences;

/**
 *  The EpscSlipMode is a class used by EPSC4
 *
 * @version $Revision: 1.0 $, $Date: 2025/03/31 18:19:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class EpscSlipMode extends EpscDeformationMode {

  public static String[] diclistc = {
      "_rista_epsc_slip_mode_title",
      "_rista_epsc_slip_mode_enabled",

      "_rista_epsc_slip_burg_m",
      "_rista_epsc_slip_norm_actener_g",
      "_rista_epsc_slip_K1_1/m",
      "_rista_epsc_slip_drag_stress_D",
      "_rista_epsc_slip_edot_0",
      "_rista_epsc_slip_init_rhos",
      "_rista_epsc_slip_init_rho_deb",
      "_rista_epsc_slip_athermal_tau_A",
      "_rista_epsc_slip_athermal_tau_B",
      "_rista_epsc_slip_athermal_tau_C",
      "_rista_epsc_slip_T_latent_hardening_twin1",
      "_rista_epsc_slip_T_latent_hardening_twin2",
      "_rista_epsc_slip_HPFAC_grain_boundary",
      "_rista_epsc_slip_HPFAC_twin1_boundary",
      "_rista_epsc_slip_HPFAC_twin2_boundary",
      "_rista_epsc_slip_A_deb_a",
      "_rista_epsc_slip_A_deb_b",
      "_rista_epsc_slip_A_deb_c",

      "_rista_plane_direction_system_id"
  };

  // these are the corresponding labels that will appear in the GUI in the parameter list window etc.
  public static String[] diclistcrm = {
      "Slip mode title",
      "Slip mode enabled",

      "Burg m (Eq. 3.12)",
      "Norm actener (Eq. 3.12)",
      "K1 (1/m) (Eq. 3.8)",
      "Drag stress-D (Eq. 3.12)",
      "Edot_0 (Eq. 3.12)",
      "Initial rho_s (1/m^2)",
      "Initial rho_deb (1/m^2)",
      "Athermal tau A (Eq. 3.17)",
      "Athermal tau B (Eq. 3.17)",
      "Athermal tau C (Eq. 3.17)",
      "T latent hardening on twin1",
      "T latent hardening on twin2",
      "HPFAC coeff for grain boundary",
      "HPFAC coeff for twin1 boundary",
      "HPFAC coeff for twin2 boundary",
      "A_deb_a (Eq. 3.15)",
      "A_deb_b (Eq. 3.15)",
      "A_deb_c (Eq. 3.15)",

      "Plane and direction for the slip"
  };

  // this model does not have subobjects, so the class list for subobjects is empty
  public static String[] classlistcs = {};
  public static String[] classlistc = {"it.unitn.ing.rista.diffr.rsa.PlaneDirectionSystem"};

  // here we define the model string to appear in the GUI and the description string
  // change it accordingly to your method, it should be an unique identifier
  final static String id = "Slip mode";
  final static String desc = "Describe a slip system to be used in EPSC4";

  // Constructors, and init methods do not change the code between the two stars lines
  // **********************************************************************************
  public EpscSlipMode(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = id;
    IDlabel = id;
    description = desc;
  }

  public EpscSlipMode(XRDcat aobj) {
    this(aobj, id);
  }

  public EpscSlipMode() {
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

  public void initConstant() {
    Nstring = 2;    // number of options, treated as strings only, in this case only the first
    Nstringloop = 0;  // no vectors of strings for options
    Nparameter = 18;   // 0 parameters refinables in the model
    Nparameterloop = 0;  // no parameter vectors in this model, to be used when the number of parameters may
    // change in the model and/or is defined by other options
    Nsubordinate = 0;    // no subobjects or subordinate objects
    Nsubordinateloop = 1;  // no vectors of subobjects
  }

  public void initParameters() {
    super.initParameters();

    stringField[0] = "<111>{110} SLIP";
    stringField[1] = "true";

    int index = 0;
    parameterField[index] = new Parameter(this, getParameterString(index), 2.546e-10,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 0.0001));
    parameterField[index] = new Parameter(this, getParameterString(index), 0.00375,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 0.01));
    parameterField[index] = new Parameter(this, getParameterString(index), 2.75E8,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 1.0E7),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 1.0E9));
    parameterField[index] = new Parameter(this, getParameterString(index), 3.0E2,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 1.0E2),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 1.0E3));
    parameterField[index] = new Parameter(this, getParameterString(index), 1.0E7,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 1.0E6),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 1.0E8));
    parameterField[index] = new Parameter(this, getParameterString(index), 5.0E9,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 1.0E9),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 1.0E10));
    parameterField[index] = new Parameter(this, getParameterString(index), 1.0E11,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 1.0E+10),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 1.0E+12));
    parameterField[index] = new Parameter(this, getParameterString(index), 87.0,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 10.0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 1000.0));
    parameterField[index] = new Parameter(this, getParameterString(index), 0.0,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 10.0));
    parameterField[index] = new Parameter(this, getParameterString(index), 1.0,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 10.0));
    parameterField[index] = new Parameter(this, getParameterString(index), 1.0,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 10.0));
    parameterField[index] = new Parameter(this, getParameterString(index), 1.0,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 10.0));
    parameterField[index] = new Parameter(this, getParameterString(index), 0.0,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 10.0));
    parameterField[index] = new Parameter(this, getParameterString(index), 0.0,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 10.0));
    parameterField[index] = new Parameter(this, getParameterString(index), 250.0,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 100.0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 1000.0));
    parameterField[index] = new Parameter(this, getParameterString(index), 70.0,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 10.0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 1000.0));
    parameterField[index] = new Parameter(this, getParameterString(index), 0.0,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 10.0));
    parameterField[index] = new Parameter(this, getParameterString(index), 1.0,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 10.0));

    refreshComputation = true; // we specify the computation need to be refreshed (it has never done up to now)
  }

}
