package it.unitn.ing.rista.diffr.rsa;

/*
 * @(#)EpscTwinningMode.java created 31/03/2025 Casalino
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
import it.unitn.ing.rista.util.*;

/**
 *  The EpscTwinningMode is a class used by EPSC4
 *
 * @version $Revision: 1.0 $, $Date: 2025/03/31 22:05:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class EpscTwinningMode extends EpscDeformationMode {

  public static String[] diclistc = {
      "_rista_epsc_twinning_mode_title",
      "_rista_epsc_twinning_mode_enabled",

      "_rista_epsc_characteristic_twin_shear",
      "_rista_epsc_twinning_tau_crit_A",
      "_rista_epsc_twinning_tau_crit_B",
      "_rista_epsc_twinning_tau_crit_C",
      "_rista_epsc_twinning_tau_prop_A",
      "_rista_epsc_twinning_tau_prop_B",
      "_rista_epsc_twinning_tau_prop_C",
      "_rista_epsc_twin_burgers_vector",
      "_rista_epsc_twin_fraction",
      "_rista_epsc_twin_CRSS",

      "_rista_plane_direction_system_id"
  };

  // these are the corresponding labels that will appear in the GUI in the parameter list window etc.
  public static String[] diclistcrm = {
      "twinning mode title",
      "twinning mode enabled",

      "Twin shear",
      "Tau crit coeff A",
      "Tau crit coeff B",
      "Tau crit coeff C",
      "Tau prop coeff A",
      "Tau prop coeff B",
      "Tau prop coeff C",
      "Twin Burgers vector (m)",
      "Twin fraction",
      "Twin CRSS",

      "Plane and direction for twin"
  };

  // this model does not have subobjects, so the class list for subobjects is empty
  public static String[] classlistcs = {};
  public static String[] classlistc = {"it.unitn.ing.rista.diffr.rsa.PlaneDirectionSystem"};

  // here we define the model string to appear in the GUI and the description string
  // change it accordingly to your method, it should be an unique identifier
  final static String id = "Twinning mode";
  final static String desc = "Describe a twinning system to be used in EPSC4";

  // Constructors, and init methods do not change the code between the two stars lines
  // **********************************************************************************
  public EpscTwinningMode(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = id;
    IDlabel = id;
    description = desc;
  }

  public EpscTwinningMode(XRDcat aobj) {
    this(aobj, id);
  }

  public EpscTwinningMode() {
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
    Nparameter = 10;   // 0 parameters refinables in the model
    Nparameterloop = 0;  // no parameter vectors in this model, to be used when the number of parameters may
    // change in the model and/or is defined by other options
    Nsubordinate = 0;    // no subobjects or subordinate objects
    Nsubordinateloop = 1;  // no vectors of subobjects
  }

  public void initParameters() {
    super.initParameters();

    stringField[0] = "<111>{112} TWIN";
    stringField[1] = "true";

    int index = 0;
    parameterField[index] = new Parameter(this, getParameterString(index), 0.25,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0.01),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 2.0));
    for (int i = 0; i < 2; i++) {
      parameterField[index] = new Parameter(this, getParameterString(index), 62.0,
          ParameterPreferences.getDouble(getParameterString(index) + ".min", 10),
          ParameterPreferences.getDouble(getParameterString(index++) + ".max", 100));
      parameterField[index] = new Parameter(this, getParameterString(index), 0.0,
          ParameterPreferences.getDouble(getParameterString(index) + ".min", 0.0),
          ParameterPreferences.getDouble(getParameterString(index++) + ".max", 10.0));
      parameterField[index] = new Parameter(this, getParameterString(index), 10.0,
          ParameterPreferences.getDouble(getParameterString(index) + ".min", 1.0),
          ParameterPreferences.getDouble(getParameterString(index++) + ".max", 100.0));
    }
    parameterField[index] = new Parameter(this, getParameterString(index), 6.326E-11,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 1E-6));
    parameterField[index] = new Parameter(this, getParameterString(index), 0.03,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0.01),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 0.1));
    parameterField[index] = new Parameter(this, getParameterString(index), 1.0,
        ParameterPreferences.getDouble(getParameterString(index) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(index++) + ".max", 10.0));

    refreshComputation = true; // we specify the computation need to be refreshed (it has never done up to now)
  }

}
