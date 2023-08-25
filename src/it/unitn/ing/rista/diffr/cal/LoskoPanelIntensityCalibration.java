/*
 * @(#)LoskoPanelIntensityCalibration.java created 21/08/2023 Los Alamos
 *
 * Copyright (c) 2099 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.cal;

import it.unitn.ing.rista.diffr.*;
import java.lang.*;

/**
 *  The LoskoPanelIntensityCalibration is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2023/08/21 17:31:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class LoskoPanelIntensityCalibration extends HippoMultBankIntCalibration {

	public static String modelID = "LumaCam Incident Spectrum";

	public LoskoPanelIntensityCalibration(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		identifier = modelID;
		IDlabel = modelID;
	}

	public LoskoPanelIntensityCalibration(XRDcat aobj) {
		this(aobj, "");
	}

	public LoskoPanelIntensityCalibration() {
		identifier = modelID;
		IDlabel = modelID;
	}

}
