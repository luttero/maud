/*
 * @(#)InstrumentBroadening.java created 06/10/2006 Casalino
 *
 * Copyright (c) 2006 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of the author and it is
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

import it.unitn.ing.rista.awt.*;

import java.io.BufferedWriter;
import java.io.IOException;
import java.lang.*;
import java.awt.*;
//import javax.swing.*;

import javax.swing.*;

/**
 * The InstrumentBroadening is a class
 *
 * @version $Revision: 1.2 $, $Date: 2006/12/04 14:30:04 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class InstrumentBroadening extends XRDcat {

  public InstrumentBroadening(XRDcat afile, String alabel) {
    super(afile, alabel);
  }

  public InstrumentBroadening(XRDcat afile) {
    this(afile, "Instrument broadening x");
  }

  public InstrumentBroadening() {
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    return new JBroadOptionsD(parent, this);
  }

  public double[] getInstrumentalBroadeningAt(double x, DiffrDataFile diffrDataFile) {
    return new double[2];
  }

/*  public double getConvolutedBroadening(double x, double[] tilting_angles, boolean dspacingbase) {
    return 0.0;
  }*/

  public double getInstrumentalAsymmetry(double x, DiffrDataFile diffrDataFile) {

    return 0.0;
  }

  public double getAsymmetryTruncationAngle() {
    return 0.0;
  }

  public void computeAsymmetry(DiffrDataFile diffrDataFile, Sample asample, double[] afit, int min, int max) {
  }

  public void computeReflectivityBroadening(DiffrDataFile diffrDataFile, Sample asample, double afit[], int min, int max) {
  }

  public void computeFluorescenceBroadening(DiffrDataFile diffrDataFile, Sample asample, double afit[], int min, int max) {
  }

	public double getTextureBroadeningAt(double position) {
		return 0;
	}

	public void exportToCif(BufferedWriter output) throws IOException {

	}

	public class JBroadOptionsD extends JOptionsDialog {

    public JBroadOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout());
      principalPanel.add(new JLabel("No options for this model"));

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

