/*
 * @(#)CalculatedAndExtractedStandardModel.java created 9/12/2013 Caen
 *
 * Copyright (c) 2013 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.sfm;

import it.unitn.ing.rista.diffr.*;

/**
 *  The CalculatedAndExtractedStandardModel is a class that use the atomic standard model
 *  to calculate stracture factors and also store the extracted structure factors by
 *  ones of the extractor methods.
 *
 *
 * @version $Revision: 1.0 $, $Date: 2013/09/12 17:06:18 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class CalculatedAndExtractedStandardModel extends StructureFactorStandardModel {
	//insert class definition here

	public CalculatedAndExtractedStandardModel(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initXRD();
		identifier = "calculated and extracted";
		IDlabel = "calculated and extracted";
		description = "select this for calculated and extracted structure factors";
	}

	public CalculatedAndExtractedStandardModel(XRDcat aobj) {
		this(aobj, "calculated and extracted");
	}

	public CalculatedAndExtractedStandardModel() {
		identifier = "calculated and extracted";
		IDlabel = "calculated and extracted";
		description = "select this for calculated and extracted structure factors";
	}

	public boolean needStructureFactorExtractor() {
		return true;
	}

	public void storeStructureFactors(Sample asample) {
	}

}
