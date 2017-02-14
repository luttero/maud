/*
 * @(#)Oscillator.java created 21/11/2000 Casalino
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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

import java.util.*;
import java.lang.*;

import it.unitn.ing.rista.util.*;

/**
 * The Oscillator is a class
 *
 * @version $Revision: 1.4 $, $Date: 2005/05/06 18:07:25 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Oscillator extends XRDcat {
  protected static String[] diclistc = {"_riet_par_background_oscillator",
                                        "_riet_par_background_peak_height", "_riet_par_background_peak_2th",
                                        "_riet_par_background_peak_hwhm"};
  protected static String[] diclistcrm = {"_riet_par_background_oscillator",
                                        "intensity", "position (arb)",
                                        "hwhm (arb)"};

  protected static String[] classlistc = {};

  double Intensity;
  double position;
  double hwhm;

  public Oscillator(XRDcat obj, String alabel) {
    super(obj, alabel);
    initXRD();
  }

  public Oscillator(XRDcat afile) {
    this(afile, "Oscillator x");
  }

  public void initConstant() {
    Nstring = 1;
    Nstringloop = 0;
    Nparameter = 3;
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
  }

  public void initParameters() {
    super.initParameters();
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
            notifyParameterChanged(source, Constants.BKG_PARAMETER_CHANGED);
            return;
        }
      }
      super.notifyParameterChanged(source);
    }
  }

  public Parameter getIntensity() {
    return parameterField[0];
  }

  public Parameter getPosition() {
    return parameterField[1];
  }

  public Parameter getHWHM() {
    return parameterField[2];
  }

  public void preparecomputing() {
    Intensity = getIntensity().getValueD();
    position = getPosition().getValueD();
    hwhm = getHWHM().getValueD();
  }

  public double getIntensity(double thetaord) {
    double intensity = Intensity * MoreMath.sind((thetaord - position) / hwhm);
    if (Intensity != 1.0)
      return Math.abs(intensity);
    return intensity;
  }
}
