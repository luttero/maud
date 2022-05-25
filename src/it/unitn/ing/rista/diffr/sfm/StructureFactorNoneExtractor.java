/*
 * @(#)StructureFactorNoneExtractor.java created 10/12/2013 Caen
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

import java.lang.*;
import it.unitn.ing.rista.diffr.*;

/**
 * The StructureFactorNoneExtractor is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: 2013/12/10 14:56:15 $
 * @since JDK1.1
 */

public class StructureFactorNoneExtractor extends StructureFactorExtractor {

	public StructureFactorNoneExtractor(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initXRD();
		identifier = "None";
		IDlabel = "None";
		description = "select this to not use any structure factor extraction";
	}

	public StructureFactorNoneExtractor(XRDcat aobj) {
		this(aobj, "none extractor");
	}

	public StructureFactorNoneExtractor() {
		identifier = "None";
		IDlabel = "None";
		description = "select this to not use any structure factor extraction";
	}


}
