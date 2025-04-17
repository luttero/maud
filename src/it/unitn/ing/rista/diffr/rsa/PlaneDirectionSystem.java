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

import javax.swing.table.AbstractTableModel;

/**
 *  The PlaneDirectionSystem is a class used to define a
 *  slip or twin plane/direction couple
 *
 * @version $Revision: 1.0 $, $Date: 2025/03/31 20:20:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class PlaneDirectionSystem extends XRDcat {

  public static String[] diclistc = {
      "_refln_index_h", "_refln_index_k", "_refln_index_l",
      "_diffrn_orient_refln_index_h", "_diffrn_orient_refln_index_k", "_diffrn_orient_refln_index_l"
  };

  // these are the corresponding labels that will appear in the GUI in the parameter list window etc.
  public static String[] diclistcrm = {
      "crystallographic plane index h", "crystallographic plane index k", "crystallographic plane index l",
      "crystallographic direction index h", "crystallographic direction index k","crystallographic direction index l"
  };

  // this model does not have subobjects, so the class list for subobjects is empty
  public static String[] classlistcs = {};
  public static String[] classlistc = {};

  // here we define the model string to appear in the GUI and the description string
  // change it accordingly to your method, it should be an unique identifier
  final static String id = "slip/twin indices";
  final static String desc = "Store the indices of the plane and direction";

  // Constructors, and init methods do not change the code between the two stars lines
  // **********************************************************************************
  public PlaneDirectionSystem(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = id;
    IDlabel = id;
    description = desc;
  }

  public PlaneDirectionSystem(XRDcat aobj) {
    this(aobj, id);
  }

  public PlaneDirectionSystem() {
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
    Nstring = 6;    // number of options, treated as strings only, in this case only the first
    Nstringloop = 0;  // no vectors of strings for options
    Nparameter = 0;   // 0 parameters refinables in the model
    Nparameterloop = 0;  // no parameter vectors in this model, to be used when the number of parameters may
    // change in the model and/or is defined by other options
    Nsubordinate = 0;    // no subobjects or subordinate objects
    Nsubordinateloop = 0;  // no vectors of subobjects
  }

  public void initParameters() {
    super.initParameters();

    stringField[0] = "1";
    stringField[1] = "1";
    stringField[2] = "1";
    stringField[3] = "1";
    stringField[4] = "1";
    stringField[5] = "0";

    refreshComputation = true; // we specify the computation need to be refreshed (it has never done up to now)
  }

  public String getPlaneIndex(int index) {
    return stringField[index];
  }

  public void setPlaneIndex(int index, String value) {
    stringField[index] = value;
  }

  public String getDirectionIndex(int index) {
    return stringField[3 + index];
  }

  public void setDirectionIndex(int index, String value) {
    stringField[3 + index] = value;
  }

}
