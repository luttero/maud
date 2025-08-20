/*
 * @(#)ExtinctionModel.java created 21/11/2025 Los Alamos
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

package it.unitn.ing.rista.diffr;

import java.lang.*;

import it.unitn.ing.rista.util.*;

/**
 * The ExtinctionModel is a class for dynamical extinction
 * correction. This is the base model to be overwritten by
 * the proper models (inside the sfm subdirectory)
 *
 * @version $Revision: 1.0 $, $Date: 2025/05/06 10:50:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class ExtinctionModel extends XRDcat {
  protected static String[] diclistc = {};
  protected static String[] diclistcrm = {};

  protected static String[] classlistc = {};

  public ExtinctionModel(XRDcat obj, String alabel) {
    super(obj, alabel);
    initBaseObject();
  }

  public ExtinctionModel(XRDcat afile) {
    this(afile, "Extinction Model x");
  }

	public ExtinctionModel() {}

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 0;
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
            notifyParameterChanged(source, Constants.STRUCTURE_FACTOR, -1);
            return;
        }
      }
      super.notifyParameterChanged(source);
    }
  }

  public void preparecomputing() {
  }

  public double getExtinctionCorrection(int h, int k, int l, double structureFactor) {
    return intensity;
  }
}
