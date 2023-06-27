/*
 * @(#)IntensityCalibration.java created 10/07/1998 ILL, Grenoble
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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
 *  The IntensityCalibration is a class
 *
 *
 * @version $Revision: 1.6 $, $Date: 2006/11/10 09:33:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class IntensityCalibration extends it.unitn.ing.rista.diffr.Calibration {


  public IntensityCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "none cal";
    IDlabel = "none cal";
  }

  public IntensityCalibration(XRDcat aobj) {
    this(aobj, "Calibration intensity x");
  }

  public IntensityCalibration() {
    identifier = "none cal";
    IDlabel = "none cal";
  }

  public int getBankNumber(String bankID) throws Exception {
    String number = Misc.toStringFinalOnlyDigits(bankID);
    if (number.length() > 0)
      return Integer.parseInt(number);
    return 0;
  }

  public void notifyParameterChanged(Parameter source) {
      FilePar filepar = getFilePar();
      if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
        if (parameterField != null)
        for (int i = 0; i < parameterField.length; i++) {
          if (source == parameterField[i]) {
            notifyParameterChanged(source, Constants.INTENSITY_CALIBRATION, -1);
            return;
          }
        }
        if (parameterloopField != null)
        for (int j = 0; j < parameterloopField.length; j++)
          for (int i = 0; i < parameterloopField[j].size(); i++)
            if (source == parameterloopField[j].elementAt(i)) {
              notifyParameterChanged(source, Constants.INTENSITY_CALIBRATION, -1);
              return;
            }
        super.notifyParameterChanged(source);
      }
  }

  public void notifyStringChanged(String source) {
    notifyStringChanged(source, Constants.INTENSITY_CALIBRATION);
  }

  public void notifyObjectChanged(XRDcat source) {
    notifyUpObjectChanged(source, Constants.INTENSITY_CALIBRATION, -1);
  }

}
