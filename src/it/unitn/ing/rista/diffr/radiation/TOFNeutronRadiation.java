/*
 * @(#)TOFNeutronRadiation.java created 06/01/1999 Riva del Garda
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

package it.unitn.ing.rista.diffr.radiation;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;

import java.awt.*;
import javax.swing.*;

/**
 *  The TOFNeutronRadiation is a class
 *
 *
 * @version $Revision: 1.6 $, $Date: 2006/11/10 09:33:01 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class TOFNeutronRadiation extends NeutronRadiation {

  public static String modelID = "TOF";

  public TOFNeutronRadiation(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = modelID;
    IDlabel = modelID;
    description = modelID + " radiation type";
  }

  public TOFNeutronRadiation(XRDcat aobj) {
    this(aobj, modelID);
  }

  public TOFNeutronRadiation() {
    identifier = modelID;
    IDlabel = modelID;
    description = modelID + " radiation";
  }

  public void initParameters() {
    super.initParameters();
  }

  public boolean isConstantWavelenght() {
    return false;
  }

  public void checkRadiation() {
    if (getLinesCount() <= 0) {
      addRadiation("Fake wavelength for TOF");
      getRadiation(0).setRadiation("Fake wavelength for TOF", 0.001, 1.0);
    }
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JTOFRadiationOptionsD(parent, this);
    return adialog;
  }

  public class JTOFRadiationOptionsD extends JOptionsDialog {

    public JTOFRadiationOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout());

      principalPanel.add(new JLabel("No options for TOF"));

      setTitle("TOF measurement");
      initParameters();
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }
  }

}
