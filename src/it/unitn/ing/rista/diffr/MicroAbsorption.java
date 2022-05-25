/*
 * @(#)MicroAbsorption.java created 27/01/2001 Casalino
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import javax.swing.*;


/**
 *  The MicroAbsorption is the superclass for the models to compute
 *  the correction for microabsorption effects. Subclass must implements the
 *  method getApparentQuantity(....) to return the apparent volumetric fraction
 *  given the real one.
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class MicroAbsorption extends XRDcat {

  public MicroAbsorption(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public MicroAbsorption(XRDcat aobj) {
    this(aobj, "Absorption x");
  }

  public MicroAbsorption() {
  }

  public double getApparentQuantity(double volFraction, RadiationType rad,
                                    Layer alayer, double crystSize) {
    return volFraction;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JMicroAbsorptionOptionsD(parent, this);
    return adialog;
  }

  public class JMicroAbsorptionOptionsD extends JOptionsDialog {

    public JMicroAbsorptionOptionsD(Frame parent, XRDcat obj) {

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
