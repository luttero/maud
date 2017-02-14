/*
 * @(#)TOFBankNoLPCorrection.java created 11/10/2013 J-Parc
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

package it.unitn.ing.rista.diffr.geometry;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;

/**
 *  The TOFBankNoLPCorrection is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2013/10/11 10:55:01 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class TOFBankNoLPCorrection extends GeometryIPNS_LANSCE {

	public static String modelID = "MultiBank TOF d4 corrected";

	public TOFBankNoLPCorrection(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		identifier = modelID;
		IDlabel = modelID;
		description = modelID + " instrument geometry";
	}

	public TOFBankNoLPCorrection(XRDcat aobj) {
		this(aobj, modelID);
	}

	public TOFBankNoLPCorrection() {
		identifier = modelID;
		IDlabel = modelID;
		description = modelID + " instrument geometry";
	}

	public double LorentzPolarization(DiffrDataFile adatafile, Sample asample, double position, boolean dspacingbase, boolean energyDispersive) {
		return 1.0;
	}

}
