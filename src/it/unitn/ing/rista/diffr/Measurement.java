/*
 * @(#)Measurement.java created 06/01/1999 Riva del Garda
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

package it.unitn.ing.rista.diffr;

import java.lang.*;
import java.awt.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import javax.swing.*;


/**
 *  The Measurement is a class
 *
 *
 * @version $Revision: 1.6 $, $Date: 2005/09/07 17:14:06 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class Measurement extends XRDcat {

  public Measurement(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public Measurement(XRDcat aobj) {
    this(aobj, "Measurement x");
  }

  public Measurement() {
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JMeasurementOptionsD(parent, this);
    return adialog;
  }

  public class JMeasurementOptionsD extends JOptionsDialog {

    public JMeasurementOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout());
      principalPanel.add(new JLabel("No options for this measurement"));

      setTitle("Options panel");
      initParameters();
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }
  }

  public double getPathK(double position, boolean dspacingbase, double[] tiltingAngles) {
    if (dspacingbase)
      return 0.0; // we don't know how to manage this

    double omega = getOmega(tiltingAngles[0], position);

    if (position - omega < 1.0E-6)
      return 0.0;
    return 1.0 / MoreMath.cosd(tiltingAngles[1]) * (1.0 / MoreMath.sind(position - omega)
            + 1.0 / MoreMath.sind(omega));
  }

  public double getCorrectedPosition(Sample asample, double x, double[] tilting_angles, double radius, DiffrDataFile adatafile) {
    return x;
  }

  public double getOmega(double omega, double twotheta) {
    return omega;
  }

  public boolean isTOF() {
    return false;
  }

}
