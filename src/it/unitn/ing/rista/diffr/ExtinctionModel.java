/*
 * @(#)ExtinctionModel.java created 20/08/2025 Los Alamos
 *
 * Copyright (c) 2025 Luca Lutterotti All Rights Reserved.
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
import java.lang.*;

import it.unitn.ing.rista.awt.JOptionsDialog;

import javax.swing.*;

/**
 * The ExtinctionModel is a class for dynamical extinction
 * correction. This is the base model to be overwritten by
 * the proper models (inside the sfm subdirectory)
 *
 * @version $Revision: 1.0 $, $Date: 2025/08/20 10:50:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class ExtinctionModel extends XRDcat {
  public ExtinctionModel(XRDcat obj, String alabel) {
    super(obj, alabel);
  }

  public ExtinctionModel(XRDcat afile) {
    this(afile, "Extinction Model x");
  }

	public ExtinctionModel() {}

  public void preparecomputing() {
  }

  public boolean canCorrect(DataFileSet adataset) {
    return false;
  }

  public double getExtinctionCorrectionByThetaRadiants(double dspace, double structureFactor, double thetar) {
    return 1.0;
  }

  public double getExtinctionCorrectionByWave(double dspace, double structureFactor, double wavelength) {
    return 1.0;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new ExtinctionModel.JExtinctionOptionsD(parent, this);
    return adialog;
  }

  public class JExtinctionOptionsD extends JOptionsDialog {

    public JExtinctionOptionsD(Frame parent, XRDcat obj) {

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
