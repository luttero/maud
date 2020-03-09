/*
 * @(#)Fluorescence.java created Apr 8, 2007 Casalino
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.awt.JOptionsDialog;

import javax.swing.*;
import java.awt.*;

/**
 * The Fluorescence is the base class to compute fluorescence patterns
 * XRF, TXRF, EDAX etc.
 * Subclasses should implement the method computeFluorescence
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Apr 8, 2007 9:50:01 PM $
 * @since JDK1.1
 */
public class Fluorescence extends XRDcat {

  public Fluorescence(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public Fluorescence(XRDcat aobj) {
    this(aobj, "Fluorescence x");
  }

  public Fluorescence() {
  }

	public void computeFluorescence(Sample asample, DataFileSet adataset) {
	}

	/**
	 * The method here do nothing, subclasses should overwrite it to compute the fluorescence pattern
	 * for the <code>DiffrDataFile</code>. When the pattern is computed it should be add to the
	 * <code>DiffrDataFile</code> using one of the addtoFit methods.
	 *
	 *
	 * @param adatafile
	 */
  public void computeFluorescence(Sample asample, DiffrDataFile adatafile) {
  }

	/**
	 * To be used to overwrite the default method that uses the model set by the analysis to
	 * compute the quantity to be minimize during refinement. Some models may need a specific
	 * way to compute it disregarding what the user will set for the analysis.
	 *
	 * @return true if the class needs to overwrite the statistical model for computing
	 * the quantity to minimize (example: reflectivity minimize always the difference in the Log(I)
	 */

  public boolean needFluorescenceStatistic() {
    return false;
  }

	public double getIntensityCorrection(int atomNumber) {
		return 1;
	}

	public void computeasymmetry(Sample asample, DiffrDataFile datafile) {
		computeasymmetry(asample, datafile, datafile.phasesfit, datafile.startingindex, datafile.finalindex - 1);
		if (!getFilePar().isComputingDerivate()) {
			for (int i = 0; i < datafile.phaseFit.length; i++)
				computeasymmetry(asample, datafile, datafile.phaseFit[i], datafile.startingindex, datafile.finalindex - 1);
		}
		refreshComputation = false;
	}

	public void computeasymmetry(Sample asample, DiffrDataFile datafile, double afit[], int min, int max) {

		Instrument ainstrument = datafile.getDataFileSet().getInstrument();

		ainstrument.getInstrumentBroadening().computeAsymmetry(datafile, asample, afit, min, max);

		for (int j = min; j < max; j++) {
//      System.out.print("Before: " + afit[j]);
			afit[j] *= datafile.computeAngularIntensityCorrection(asample, ainstrument, j);
//      System.out.println(", after: " + afit[j]);
		}
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JFluorescenceOptionsD(parent, this);
    return adialog;
  }

  public class JFluorescenceOptionsD extends JOptionsDialog {

    public JFluorescenceOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout());
      principalPanel.add(new JLabel("No options for this Fluorescence model"));

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
