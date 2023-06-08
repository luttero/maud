/*
 * @(#)SaturationIntensityCalibration.java created 30/05/2007 Caen
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.diffr.cal;

import it.unitn.ing.rista.diffr.*;

import java.lang.*;

import it.unitn.ing.rista.util.*;

/**
 *  The SaturationIntensityCalibration is a class to calibrate the intensity
 *  cutting off intensities bigger then the parameter value
 *
 *
 * @version $Revision: 1.7 $, $Date: 2007/05/07 18:07:25 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SaturationIntensityCalibration extends IntensityCalibration {
  public static String[] diclistc = {"_inst_intensity_cutoff_value"};
  public static String[] diclistcrm = {"saturation intensity "};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshCalibration = true;

  public SaturationIntensityCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Saturation Intensity Calibration";
    IDlabel = "Saturation Intensity Calibration";
  }

  public SaturationIntensityCalibration(XRDcat aobj) {
    this(aobj, "Saturation Intensity Calibration x");
  }

  public SaturationIntensityCalibration() {
    identifier = "Saturation Intensity Calibration";
    IDlabel = "Saturation Intensity Calibration";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 1;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
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
    parameterField[0] = new Parameter(this, getParameterString(0), 65536,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.1),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 1000000));
    parameterField[0].setPositiveOnly();
  }

  public double calibrateData(double x) {
    return Math.min(x, Math.abs(parameterValues[0]));  // todo remove abs when positive only working
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);

    parameterField[0].setPositiveOnly();
  }

}
