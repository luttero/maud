/*
 * @(#)Detector.java created 06/01/1999 Riva del Garda
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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
import java.awt.*;

import it.unitn.ing.rista.awt.*;

import javax.swing.*;

import it.unitn.ing.rista.diffr.cal.*;


/**
 *  The Detector is a class
 *
 *
 * @version $Revision: 1.5 $, $Date: 2004/11/18 09:30:49 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class Detector extends XRDcat {

  public Detector(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public Detector(XRDcat aobj) {
    this(aobj, "Detector x");
  }

  public Detector() {
  }

  public AngularCalibration getAngularCalibration() {
    return ((Instrument) getParent()).getAngularCalibration();
  }

  public double getThetaDetector(DiffrDataFile datafile, double twotheta) {
    double ang = getAngularCalibration().getTthetaValue(datafile, twotheta);
    if (ang < 0)
      ang += 360f;
    return ang;
  }

  public double getEtaDetector(DiffrDataFile datafile) {
    return getAngularCalibration().getEtaValue(datafile);
  }

	public double getDistanceFromSample() {
		return 100;
	}

	public double getIntensityCalibration(DiffrDataFile adatafile, Sample asample, double position,
	                                      boolean dspacingbase, boolean energyDispersive) {
    return 1.0;
  }

	public double getAbsorptionCorrection(DiffrDataFile adatafile, int pointIndex, double energyInKeV) {
		return 1.0;
	}

	public void freeAllScaleParameters() {
//		getIntensity().setRefinableCheckBound();
  }

  public double[] getTextureAngles(double[] tilting_angles, double twotheta) {
    double[] newAngles = new double[2];

    newAngles[0] = tilting_angles[1];
    newAngles[1] = tilting_angles[2];
    return newAngles;
  }

	public double getGeometryCorrection(double beamOutCorrection) {
		return beamOutCorrection;
	}

	public int getBankNumber(String bankID) {
    return -1;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JDetectorOptionsD(parent, this);
    return adialog;
  }

	public class JDetectorOptionsD extends JOptionsDialog {

    public JDetectorOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout());
      principalPanel.add(new JLabel("No options for this detector"));

      setTitle("Options panel");
      initParameters();
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }
  }

}
