/*
 * @(#)TXRFluorescence.java created Feb 21, 2013 Paris-Beauvais
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
package it.unitn.ing.rista.diffr.fluorescence;

import it.unitn.ing.rista.diffr.*;

/**
 * The TXRFluorescence is a class to calculate the total X-ray
 * Fluorescence using Giancarlo Pepponi model. It uses the intensity
 * coming from the reflectivity model (Matrix) for the calculation.
 * Otherwise the raw incident intensity.
 *
 * @author Luca Lutterotti, Giancarlo Pepponi
 * @version $Revision: 1.00 $, $Date: Feb 21, 2013 8:27:36 PM $
 * @since JDK1.1
 */
public class TXRFluorescence extends Fluorescence {

	public static String modelID = "TXRF";
	public static String descriptionID = "Total X-Ray Fluorescence model, can be coupled with reflectivity matrix model";

	public TXRFluorescence(XRDcat obj, String alabel) {
		super(obj, alabel);
		identifier = modelID;
		IDlabel = modelID;
		description = descriptionID;
	}

	public TXRFluorescence(XRDcat afile) {
		this(afile, modelID);
	}

	public TXRFluorescence() {
		identifier = modelID;
		IDlabel = modelID;
		description = descriptionID;
	}

}
