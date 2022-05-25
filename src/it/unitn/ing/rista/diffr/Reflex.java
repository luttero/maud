/*
 * @(#)Reflex.java created Mar 6, 2005 Casalino
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.util.*;


/**
 * The Reflex is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2005/05/06 18:07:25 $
 * @since JDK1.1
 */

public class Reflex extends XRDcat {
  protected static String[] diclistc = {"_diffrn_refln_index_h", "_diffrn_refln_index_k", "_diffrn_refln_index_l",
                                        "_diffrn_refln_d_spacing", "_diffrn_refln_counts_peak",
                                        "_diffrn_refln_domain_size", "_diffrn_refln_microstrain"};
  protected static String[] diclistcrm = {"_diffrn_refln_index_h", "_diffrn_refln_index_k", "_diffrn_refln_index_l",
                                        "position d-spacing (angstrom)", "intensity",
                                        "crystallite size (angstrom)", "r.m.s. microstrain"};

  protected static String[] classlistc = {};

  int h, k, l;
  double d_spacing;
  double intensity;
  double domain;
  double microstrain;

  public Reflex(XRDcat obj, String alabel) {
    super(obj, alabel);
    initXRD();
  }

  public Reflex(XRDcat afile) {
    this(afile, "Reflection x");
  }

	public Reflex() {}

  public void initConstant() {
    Nstring = 3;
    Nstringloop = 0;
    Nparameter = 4;
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
    stringField[0] = "0";
    stringField[1] = "0";
    stringField[2] = "0";
    initializeParameter(0, 10.0, 0.0001, 100.0);
    initializeParameter(1, 0.0, 0.0, 1000000.0);
    initializeParameter(2, 0.0, 1.0, 10000.0);
    initializeParameter(3, 0.0, 0.0, 1.0);
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
          notifyParameterChanged(source, Constants.PEAKS_PARAMETER_CHANGED);
          return;
        }
      }
      super.notifyParameterChanged(source);
    }

  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    intensity = getIntensity().getValueD();
    d_spacing = getDspacing().getValueD();
    domain = getDomain().getValueD();
    microstrain = getMicrostrain().getValueD();
  }

  public Parameter getIntensity() {
    return parameterField[1];
  }

  public Parameter getDspacing() {
    return parameterField[0];
  }

  public void getDspacing(double avalue) {
    getDspacing().setValue(avalue);
  }

  public Parameter getDomain() {
    return parameterField[2];
  }

  public Parameter getMicrostrain() {
    return parameterField[3];
  }

}
