/*
 * @(#)SpecimenPrecessionError.java created 08/10/2002 Mesiano
 *
 * Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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

import java.awt.*;

import it.unitn.ing.rista.awt.*;

import javax.swing.*;

/**
 *  The SpecimenPrecessionError is a class
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SpecimenPrecessionError extends XRDcat {

  public double[] xyz = {0.0, 0.0, 0.0};

  public SpecimenPrecessionError(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public SpecimenPrecessionError(XRDcat aobj) {
    this(aobj, "Specimen precession model x");
  }

  public SpecimenPrecessionError() {
  }

  public double[] getXYZForPrecession(double[] angles, double x) {
    return xyz;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JPrecessionErrorOptionsD(parent, this);
    return adialog;
  }

  public class JPrecessionErrorOptionsD extends JOptionsDialog {

    public JPrecessionErrorOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      principalPanel.add(new JLabel("No options for this model"));

      setTitle("Specimen precession error options panel");
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }

  }
}
