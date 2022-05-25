/*
 * @(#)SizeStrainSymModel.java created 04/10/1998 Verona-Trento
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
import it.unitn.ing.rista.util.Constants;

import javax.swing.*;

/**
 *  The SizeStrainSymModel is a class
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SizeStrainSymModel extends XRDcat {

  public SizeStrainSymModel(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public SizeStrainSymModel(XRDcat aobj) {
    this(aobj, "Size-Strain symmetry model x");
  }

  public SizeStrainSymModel() {
  }

/*	public void updateParametertoDoubleBuffering() {
		applySymmetryRules();
		super.updateParametertoDoubleBuffering();
	}*/

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
            notifyParameterChanged(source, Constants.SAMPLE_BROADENING);
            return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.SAMPLE_BROADENING);
            return;
          }

      super.notifyParameterChanged(source);
    }
  }

  public void applySymmetryRules() {
  }

  double cryststrain[] = new double[2];

  public double[] getCrystalliteMicrostrain(double d_space, int h, int k, int l, double[] texture_angles) {

//		cryststrain[0] = 0.0;
//		cryststrain[1] = 0.0;
    return cryststrain;
  }

  public double getMeanCrystallite() {
    return 0.0;
  }

  public double getMeanMicrostrain() {
    return 0.0;
  }

  public void correctCrystalliteAndMicrostrain() {
  }

  public void freeAllMicroParameters(boolean amorphous) {
    int i, j;

    if (!amorphous) {
      for (i = 0; i < Nparameter; i++)
        parameterField[i].setRefinableCheckBound();
      for (i = 0; i < Nparameterloop; i++) {
        for (j = 0; j < numberofelementPL(i); j++)
          ((Parameter) parameterloopField[i].elementAt(j)).setRefinableCheckBound();
      }
    } else {
      if (Nparameter > 0)
        parameterField[0].setRefinableCheckBound();
    }
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JSizeStrainSymOptionsD(parent, this);
    return adialog;
  }

  public class JSizeStrainSymOptionsD extends JOptionsDialog {

    public JSizeStrainSymOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      principalPanel.add(new JLabel("No options for this model"));

      setTitle("Symmetry rules options panel");
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }

  }
}
