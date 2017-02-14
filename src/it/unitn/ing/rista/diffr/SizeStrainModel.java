/*
 * @(#)SizeStrainModel.java created 04/10/1998 Verona-Trento
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

import java.awt.*;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.interfaces.Peak;

import javax.swing.*;

/**
 *  The SizeStrainModel is a class
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SizeStrainModel extends XRDcat {

  public SizeStrainModel(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public SizeStrainModel(XRDcat aobj) {
    this(aobj, "Size-Strain model x");
  }

  public SizeStrainModel() {
  }

  public double getBetaChauchy(double dspace, double cryst, double mstrain) {
    return 0.0;
  }

  public double getBetaGauss(double dspace, double cryst, double mstrain) {
    return 0.0;
  }

	public int getNumberOfSizeStrainCoefficients() {
		return 2; // crystallite size and r.m.s. microstrain
	}

  public Peak createPeak(double pos, boolean control, boolean energyDispersive, double[] wave, double[] weight,
                         Reflection reflex, int order) {

/*    int peakf = 0;
    for (int i = 0; i < nFunction; i++) {
      if (getPeakFunction().equalsIgnoreCase(peakFunctionClass[i])) {
        peakf = i;
      }
    }
    switch (peakf) {
      case 0:
        return new PseudoVoigtPeak(pos, control, wave, weight, reflex, order);
      default:
    }*/
    return new PseudoVoigtPeak(pos, control, energyDispersive, wave, weight, reflex, order);
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JSizeStrainOptionsD(parent, this);
    return adialog;
  }

  public class JSizeStrainOptionsD extends JOptionsDialog {

    public JSizeStrainOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      principalPanel.add(new JLabel("No options for this model"));

      setTitle("Line Broadening options panel");
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }

  }
}
