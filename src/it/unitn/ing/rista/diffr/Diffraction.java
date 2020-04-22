/*
 * @(#)Diffraction.java created 28/08/2018 Povo
 *
 * Copyright (c) 2018 Luca Lutterotti All Rights Reserved.
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
import java.util.Vector;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.interfaces.Peak;

import javax.swing.*;


/**
 *  The Diffraction is a general class to perform diffraction computation
 *
 *
 * @version $Revision: 1.0 $, $Date: 2018/08/28 11:55:51 $
 * @author Luca Lutterotti
 * @since JDK1.8
 */


public class Diffraction extends XRDcat {

	public Diffraction(XRDcat aobj, String alabel) {
		super(aobj, alabel);
	}

	public Diffraction(XRDcat aobj) {
		this(aobj, "Diffraction standard");
	}

	public Diffraction() {
	}
	
	public DataFileSet getDataFileSet() {
	  return (DataFileSet) getParent();
  }

	public void computeDiffraction(Sample asample) {
	}

		/**
		 * The method here do nothing, subclasses should overwrite it to compute the diffraction pattern
		 * for the <code>DiffrDataFile</code>. When the pattern is computed it should be add to the
		 * <code>DiffrDataFile</code> using one of the addtoFit methods.
		 *
		 *
		 * @param adatafile
		 * @see DiffrDataFile#addtoFit
		 */
	public void computeDiffraction(Sample asample, DiffrDataFile adatafile) {
	}

	public int[] computeReflectionIntensity(Sample asample, Vector<Peak> peaklist, boolean computeBroadening,
	                                        double[] expfit, double rangefactor, int computeTexture,
	                                        int computeStrain, int computeFhkl, boolean leBailExtraction,
	                                        Phase phase, DiffrDataFile datafile) {
		return new int[0];
	}
  
  public void computeasymmetry(Sample asample, DiffrDataFile datafile) {
    InstrumentBroadening inst_broad = datafile.getDataFileSet().getInstrument().getInstrumentBroadening();
    inst_broad.computeAsymmetry(datafile, asample, datafile.phasesfit, datafile.startingindex, datafile.finalindex);
    if (!getFilePar().isComputingDerivate()) {
      for (int i = 0; i < datafile.phaseFit.length; i++)
        inst_broad.computeAsymmetry(datafile, asample, datafile.phaseFit[i], datafile.startingindex, datafile.finalindex);
    }
    refreshComputation = false;
  }
  
  public void computeasymmetry(Sample asample, DiffrDataFile datafile, double afit[], int min, int max) {
    getDataFileSet().getInstrument().getInstrumentBroadening().
        computeAsymmetry(datafile, asample, afit, min, max);
  }
  
  public JOptionsDialog getOptionsDialog(Frame parent) {
		JOptionsDialog adialog = new JDiffractionOptionsD(parent, this);
		return adialog;
	}
  
  public Peak createPeak(SizeStrainModel activeSizeStrain, double dspace, boolean dspacingbase, boolean energyDispersive,
                         double[] wavelength, double[] radweight, Reflection refl, int i) {
    return activeSizeStrain.createPeak(dspace, dspacingbase, energyDispersive, wavelength, radweight, refl, i);
  }
  
  public class JDiffractionOptionsD extends JOptionsDialog {

		public JDiffractionOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new FlowLayout());
			principalPanel.add(new JLabel("No options for this diffraction model"));

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
