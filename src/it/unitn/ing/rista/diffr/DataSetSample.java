/*
 * @(#)DataSetSample.java created 01/01/1997 Mesiano
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

import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.util.Constants;

import java.util.Vector;

/**
 * The DataSetSample is a class
 *
 * @version $Revision: 1.4 $, $Date: 2005/05/06 18:07:25 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class DataSetSample extends XRDcat {
  protected static String[] diclistc = {
    "_pd_spec_orientation_omega", "_pd_spec_orientation_chi", "_pd_spec_orientation_phi",
    "_riet_par_spec_displac_x", "_riet_par_spec_displac_y", "_riet_par_spec_displac_z"};
  protected static String[] diclistcrm = {
    "sample ref. system omega (deg)", "sample ref. system chi (deg)", "sample ref. system phi (deg)",
    "sample displacement x (mm)", "sample displacement y (mm)", "sample displacement z (mm)"};

  protected static String[] classlistc = {};

  public DataSetSample(XRDcat afile, String alabel) {
    super(afile, alabel);
    initXRD();
  }

  public DataSetSample(XRDcat afile) {
    this(afile, "Data_set_x");
  }

	public DataSetSample() {}

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 6;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    for (int i = 0; i < totsubordinateloop; i++)
      diclistRealMeaning[i] = diclistcrm[i];
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
  }

  public void initParameters() {
    super.initParameters();

    setomega("0");
    setchi("0");
    setphi("0");
    setdispx("0");
    setdispy("0");
    setdispz("0");
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if (filepar != null && !filepar.isLoadingFile() && isAbilitatetoRefresh) {
      for (int i = 0; i < 3; i++)
        if (source == parameterField[i]) {
          notifyParameterChanged(source, Constants.SAMPLE_ORIENTATION_CHANGED);
          return;
        }
      for (int i = 3; i < 6; i++)
        if (source == parameterField[i]) {
          notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED);
          return;
        }
    }
    super.notifyParameterChanged(source);
  }

  public String getomega() {
    return parameterField[0].getValue();
  }

  public void setomega(String astring) {
    parameterField[0].setValue(astring);
  }

  public String getchi() {
    return parameterField[1].getValue();
  }

  public void setchi(String astring) {
    parameterField[1].setValue(astring);
  }

  public String getphi() {
    return parameterField[2].getValue();
  }

  public void setphi(String astring) {
    parameterField[2].setValue(astring);
  }

  public String getdispx() {
    return parameterField[0].getValue();
  }

  public void setdispx(String astring) {
    parameterField[3].setValue(astring);
  }

  public String getdispy() {
    return parameterField[1].getValue();
  }

  public void setdispy(String astring) {
    parameterField[4].setValue(astring);
  }

  public String getdispz() {
    return parameterField[2].getValue();
  }

  public void setdispz(String astring) {
    parameterField[5].setValue(astring);
  }
}
