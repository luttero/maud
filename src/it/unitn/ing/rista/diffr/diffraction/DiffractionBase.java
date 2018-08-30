/*
 * @(#)DiffractionBase.java created 29/8/2018 Povo
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

package it.unitn.ing.rista.diffr.diffraction;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Constants;

import java.lang.*;

/**
 *  The DiffractionBase is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2018/08/29 11:20:12 $
 * @author Luca Lutterotti
 * @since JDK1.8
 */

public class DiffractionBase extends Diffraction {

	public DiffractionBase(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initXRD();
		identifier = "Basic diffraction";
		IDlabel = identifier;
		description = identifier;
	}

	public DiffractionBase(XRDcat aobj) {
		this(aobj, "Basic diffraction");
	}

	public DiffractionBase() {
		identifier = "Basic diffraction";
		IDlabel = identifier;
		description = identifier;
	}

	public void computeDiffraction(Sample asample, DiffrDataFile datafile) {
		DataFileSet datafileset = datafile.getDataFileSet();
		if (getFilePar().isComputingDerivate()) {
//      System.out.println("refreshing derivative: " + this.toXRDcatString());
			for (int ij = 0; ij < getFilePar().getActiveSample().phasesNumber(); ij++) {
				double expfit[] = new double[datafile.getTotalNumberOfData()];
				int minmaxindex[] = datafileset.computeReflectionIntensity(asample, datafileset.getPeakList(), true,
						expfit, Constants.ENTIRE_RANGE, Constants.COMPUTED,
						Constants.COMPUTED, Constants.COMPUTED, false,
						getFilePar().getActiveSample().getPhase(ij), datafile);
				for (int j = minmaxindex[0]; j < minmaxindex[1]; j++)
					datafile.addtoPhasesFit(j, expfit[j]);
			}
		} else {
//      System.out.println("refreshing: " + this.toXRDcatString());
			for (int ij = 0; ij < getFilePar().getActiveSample().phasesNumber(); ij++) {
//        System.out.println("Phase: " + getFilePar().getActiveSample().getPhase(ij).toXRDcatString());
				double expfit[] = new double[datafile.getTotalNumberOfData()];
				int minmaxindex[] = datafileset.computeReflectionIntensity(asample, datafileset.getPeakList(), true,
						expfit, Constants.ENTIRE_RANGE, Constants.COMPUTED,
						Constants.COMPUTED, Constants.COMPUTED, false,
						getFilePar().getActiveSample().getPhase(ij), datafile);
//        System.out.println("indices: " + minmaxindex[0] + " " + minmaxindex[1] + " " + expfit[1000]);
				for (int j = minmaxindex[0]; j < minmaxindex[1]; j++)
					datafile.addtoPhasesFit(j, expfit[j], ij);
			}
		}
	}

}
