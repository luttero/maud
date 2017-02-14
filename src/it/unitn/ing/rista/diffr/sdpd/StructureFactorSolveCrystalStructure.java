/*
 * @(#)StructureFactorSolveCrystalStructure.java created 22/08/2001 Cassino
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

package it.unitn.ing.rista.diffr.sdpd;

import it.unitn.ing.rista.diffr.*;

import it.unitn.ing.rista.util.*;

/**
 *  The StructureFactorSolveCrystalStructure is a class that implements a correction of the
 *  structure factors by solving the structure from the extracted values.
 *
 *
 * @version $Revision: 1.9 $, $Date: 2006/07/20 13:39:06 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class StructureFactorSolveCrystalStructure extends StructureFactorModel {

  public StructureFactorSolveCrystalStructure(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Disabled structure solution model";
    IDlabel = "structure solution model";
    description = "select this to solve the structure and use structure factors from solution";
  }

  public StructureFactorSolveCrystalStructure(XRDcat aobj) {
    this(aobj, "structure solution model");
  }

  public StructureFactorSolveCrystalStructure() {
    identifier = "Disabled structure solution model";
    IDlabel = "structure solution model";
    description = "select this to solve the structure and use structure factors from solution";
  }

	public boolean needStructureFactorExtractor() {
		return true;
	}

	public boolean canSolveStructure() {
		return true;
	}

	public boolean solveStructure() {
		if (getFilePar().isStructureFactorComputationPermitted())
			return solveStructure(getStructureFactorList());
		else
			return false;
	}

	public boolean solveStructure(StructureFactorList[] structureFactorList) {
		return true;
	}

	public void computeStructureFactors(Sample asample, DataFileSet adataset) {
		final Phase phase = (Phase) getParent();
		if (!phase.refreshFhklcomp)
			return;
		computeStructureFactors(phase, asample, adataset);
	}

	public void computeStructureFactors(final Phase phase, Sample asample, DataFileSet adataset) {
		double defaultFactor = Constants.STARTING_STRUCTURE_FACTOR * Constants.STARTING_STRUCTURE_FACTOR;
		Instrument ainstrument = adataset.getInstrument();
		RadiationType rad1 = ainstrument.getRadiationType();
		phase.refreshFhklcompv();
//		Vector<AtomSite> atomList = phase.getFullAtomList();
		int hkln = phase.gethklNumber();
		final double[] fhkl = new double[hkln];
//		double volume = phase.getCellVolume();
		for (int i = 0; i < hkln; i++) {
			Reflection refl = phase.getReflex(i);
			int multiplicity = refl.multiplicity;
			double dspacing = refl.d_space;
//				double[][][] scatFactors = adataset.getScatteringFactor(phase);
			double structureFactor = computeStructureFactor(refl.getH(), refl.getK(), refl.getL(), multiplicity, dspacing, rad1,
					defaultFactor);
			fhkl[i] = structureFactor;
		}

		adataset.storeComputedStructureFactors(phase, fhkl);
	}

	public double computeStructureFactor(int h, int k, int l, int multiplicity, double dspacing, RadiationType radType,
	                                     double defaultFactor) {
//    int h = refl.h, k = refl.k, l = refl.l, int multiplicity = refl.multiplicity;
//    double dspacing = refl.d_space;

		return defaultFactor;
	}

}
