/*
 * @(#)MagneticStructure.java created 17/04/1999 Riva del Garda
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

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import javax.swing.*;


/**
 *  The MagneticStructure is a class to represent a magnetic structure
 *  and to compute the magnetic structure factors
 *
 *
 * @version $Revision: 1.4 $, $Date: 2006/07/20 13:39:04 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class MagneticStructure extends XRDcat {

  public boolean refreshMagneticStructure = true;

  public MagneticStructure(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public MagneticStructure(XRDcat aobj) {
    this(aobj, "Magnetic structure x");
  }

  public MagneticStructure() {
  }

  public double getMagneticStructureFactor(int h, int k, int l, int multiplicity, double dspacing,
                                           int radType, int tubeNumber) {
    return 0.0;
  }

  public double getSatelliteMagneticStructureFactor(int h, int k, int l, int multiplicity, int dspacing,
                                           int radType, int tubeNumber) {

    return 0.0;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JMagneticStructureOptionsD(parent, this);
    return adialog;
  }

  public class JMagneticStructureOptionsD extends JOptionsDialog {

    public JMagneticStructureOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout());


      principalPanel.add(new JLabel("No options for this model"));

      setTitle("Magnetic structure");
      initParameters();
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }
  }

}

