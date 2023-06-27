/*
 * @(#)AngularCalibration.java created 10/07/1998 ILL, Grenoble
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

import ij.ImagePlus;
import it.unitn.ing.rista.diffr.*;

import java.lang.*;

import it.unitn.ing.rista.util.*;


/**
 *  The AngularCalibration is a class
 *
 *
 * @version $Revision: 1.7 $, $Date: 2006/11/10 09:33:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class AngularCalibration extends it.unitn.ing.rista.diffr.Calibration {


  public AngularCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "no ang";
    IDlabel = "no ang";
  }

  public AngularCalibration(XRDcat aobj) {
    this(aobj, "Angular calibration x");
  }

  public AngularCalibration() {
    identifier = "no ang";
    IDlabel = "no ang";
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
          notifyParameterChanged(source, Constants.ANGULAR_CALIBRATION, -1);
          return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.ANGULAR_CALIBRATION, -1);
            return;
          }
      super.notifyParameterChanged(source);
    }
  }

  public void notifyStringChanged(String source) {
    notifyStringChanged(source, Constants.ANGULAR_CALIBRATION);
  }

  public void notifyObjectChanged(XRDcat source) {
    notifyUpObjectChanged(source, Constants.ANGULAR_CALIBRATION, -1);
  }

  public boolean freeAllBasicParameters() {
// here is an example from Instrument
//		for (int i = 0; i < getthetaoffsetnumber(); i++)
//			getThetaDisplacement(i).setRefinableCheckBound();
    return false;
  }

  public double getDetectorDistanceValue(DiffrDataFile datafile) {
    return getRadius();
  }

  public double getRadius() {
    return ((Instrument) getParent()).getGeometry().getRadius(null);
  }

  public void setRadius(String value) {
    ((Instrument) getParent()).getGeometry().setRadius(value);
  }

  public double getOriginalCenterX() {
    return 0.0;
  }

  public double getOriginalCenterY() {
    return 0.0;
  }

  public void setOriginalCenterX(String value) {
  }

	public void setOriginalCenterY(String value) {
  }

	public double getBeamInclination(DiffrDataFile datafile, int index, double[][] tmat) {
		return Constants.PI_2;
	}

	public boolean loadAndUnrollImage(ImagePlus imp, MultDiffrDataFile mdatafile, double[] gonioAngles) { return false; }

	public boolean positionAlreadyCorrected() {
		return false;
	}

	public int getChannelForZero(DiffrDataFile datafile) {
		return 0;
	}

	public double getChannelStep(DiffrDataFile diffrDataFile) {
		return 1.0;
	}

  public double getReal2ThetaValue(int number, double twotheta) {
    return twotheta;
  }

  public void invertEta() {}
  
}
