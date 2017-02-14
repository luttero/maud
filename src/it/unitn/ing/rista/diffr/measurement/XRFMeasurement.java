/*
 * @(#)XRFMeasurement.java created May 16, 2007 Caen
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.measurement;

import it.unitn.ing.rista.diffr.*;

import java.awt.*;

/**
 * The XRFMeasurement is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: May 16, 2007 11:32:47 AM $
 * @since JDK1.1
 */
public class XRFMeasurement extends Measurement {

  public static String modelID = "XRF Measurement";

  public static String[] diclistc = {};
  public static String[] diclistcrm = {};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshXRFMeasurement = true;

  public XRFMeasurement(XRDcat obj, String alabel) {
    super(obj, alabel);
    identifier = modelID;
    IDlabel = modelID;
    description = modelID;
  }

  public XRFMeasurement(XRDcat afile) {
    this(afile, modelID);
  }

  public XRFMeasurement() {
    identifier = modelID;
    IDlabel = modelID;
    description = modelID;
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    System.arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
    System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
  }

  public void initParameters() {
    super.initParameters();

/*
    stringField[0] = "";
    parameterField[0] = new Parameter(this, getParameterString(0), 0,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", -1),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 1));
*/

  }

  public void edit(Frame aframe) {
    autoDialog = true;
    super.edit(aframe);
  }

}
