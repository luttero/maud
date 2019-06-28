/*
 * @(#)StructureSolutionMethod.java created 19/08/2001 Casalino
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
import it.unitn.ing.rista.util.*;


/**
 *  The StructureSolutionMethod is a generic model for structure solution
 *  dismissed, to be deleted, see StructureFactorSolveCrystalStructure
 *
 *
 * @version $Revision: 1.4 $, $Date: 2006/07/20 13:39:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class StructureSolutionMethod extends StructureFactorModel {

  public StructureSolutionMethod(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public StructureSolutionMethod(XRDcat aobj) {
    this(aobj, "Structure Factor method x");
  }

  public StructureSolutionMethod() {
  }

	public boolean solveStructure() {
		StructureFactorList[] structureFactorList = null;
		return solveStructure(structureFactorList);
	}

	public boolean solveStructure(StructureFactorList[] structureFactorList) {
    return true;
  }

	public boolean needStructureFactorExtractor() {
		return true;
	}

	public boolean canSolveStructure() {
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
		final double[][] fhkl = new double[hkln][rad1.getLinesCount()];
		double[] structureFactor = new double[rad1.getLinesCount()];
//		double volume = phase.getCellVolume();
			for (int i = 0; i < hkln; i++) {
				Reflection refl = phase.getReflex(i);
				int multiplicity = refl.multiplicity;
				double dspacing = refl.d_space;
//				double[][][] scatFactors = adataset.getScatteringFactor(phase);
				computeStructureFactor(refl.getH(), refl.getK(), refl.getL(), multiplicity, dspacing, rad1,
						defaultFactor, structureFactor);
				for (int k = 0; k < rad1.getLinesCount(); k++)
					fhkl[i][k] = structureFactor[k];
			}

		adataset.storeComputedStructureFactors(phase, fhkl);
	}

	public void computeStructureFactor(int h, int k, int l, int multiplicity, double dspacing, RadiationType radType,
	                                     double defaultFactor, double[] structureFactor) {
//    int h = refl.h, k = refl.k, l = refl.l, int multiplicity = refl.multiplicity;
//    double dspacing = refl.d_space;
		for (int i = 0; i < structureFactor.length; i++)
			structureFactor[i] = defaultFactor;
  }

}
