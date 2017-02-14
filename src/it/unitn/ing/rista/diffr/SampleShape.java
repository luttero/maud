/*
 * @(#)SampleShape.java created 31/12/2000 Casalino
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr;

import java.awt.*;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.Constants;

import javax.swing.*;

/**
 *  The SampleShape is a class
 *
 *
 * @version $Revision: 1.9 $, $Date: 2004/08/12 09:36:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SampleShape extends XRDcat {

  public SampleShape(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public SampleShape(XRDcat aobj) {
    this(aobj, "Sample shape model x");
  }

  public SampleShape() {
  }

  public double getShape(double azimuthal, double polar) { // angles in radiant
    return 1.0;
  }

  public double getNormalizedShape(double azimuthal, double polar) {
    return 1.0;
  }

  public double getMeanShape() {
    return 1.0;
  }

/*	public double getShapeOmegaD() {
		return 0.0;
	}*/

  public double getShapeChiD() {
    return 0.0;
  }

  public double getShapePhiD() {
    return 0.0;
  }

  public void freeAllShapeParameters() {
    int i, j;

    for (i = 0; i < Nparameter; i++)
      parameterField[i].setRefinableCheckBound();
    for (i = 0; i < Nparameterloop; i++) {
      for (j = 0; j < numberofelementPL(i); j++)
        ((Parameter) parameterloopField[i].elementAt(j)).setRefinableCheckBound();
    }
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JShapeOptionsD(parent, this);
    return adialog;
  }

  public void refreshForNotificationUp(XRDcat source, int reason) {
    if (!getFilePar().isComputingDerivate() || source == this)
      refreshComputation = true;
//    System.out.println("SampleShape up: " + refreshComputation);
  }

  public void refreshForNotificationDown(XRDcat source, int reason) {
    if (!getFilePar().isComputingDerivate() || source == this || reason == Constants.SAMPLE_ORIENTATION_CHANGED
     || reason == Constants.PHASE_WEIGHT_CHANGED || reason == Constants.THICKNESS_CHANGED ||
        reason == Constants.CELL_CHANGED || reason == Constants.RADIATION_WAVELENGTH_CHANGED)
      refreshComputation = true;
//    System.out.println("SampleShape down: " + refreshComputation);
  }

  public void computeAbsorptionPath(double[][] incidentAndDiffraction_angles, double absorption, double[] position,
                                    double[] intensity, double toLambda) {
  }

  public double getCorrectionForVelocity(DiffrDataFile adatafile, Instrument instrument,
                                         double position, double toLambda) {
    return 1.0;
  }

  public class JShapeOptionsD extends JOptionsDialog {

    public JShapeOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      principalPanel.add(new JLabel("No options for this model"));

      setTitle("Sample shape options panel");
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }

  }
}
