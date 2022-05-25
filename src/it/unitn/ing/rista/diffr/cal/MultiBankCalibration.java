/*
 * @(#)MultiBankCalibration.java created 11/06/2002 Mesiano
 *
 * Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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
import java.util.*;

import it.unitn.ing.rista.util.*;


/**
 *  The MultiBankCalibration is a class
 *
 *
 * @version $Revision: 1.6 $, $Date: 2006/11/10 09:33:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class MultiBankCalibration extends GSASbankCalibration {

  public static String modelID = "Multi Bank";

  public MultiBankCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = modelID;
    IDlabel = modelID;
  }

  public MultiBankCalibration(XRDcat aobj) {
    this(aobj, modelID);
  }

  public MultiBankCalibration() {
    identifier = modelID;
    IDlabel = modelID;
  }

  public int getBankNumber(DiffrDataFile datafile) {
    return datafile.getAngBankNumber();
  }

  public double getTthetaValue(DiffrDataFile datafile, double twotheta) {
//	  System.out.println(this + ", asking for bank number: " + getBankNumber(datafile));
    return theta[getBankNumber(datafile)];
  }

  public double getEtaValue(DiffrDataFile datafile) {
    return eta[getBankNumber(datafile)];
  }

  protected boolean bankIsActive(int bank) {
    DataFileSet data = (DataFileSet) getParent().getParent();
    int datafiles = data.activedatafilesnumber();
    boolean isActive = false;
    for (int i = 0; i < datafiles && !isActive; i++) {
      if (data.getActiveDataFile(i).getBankNumber() == bank)
        isActive = true;
    }
    return isActive;
  }

}
