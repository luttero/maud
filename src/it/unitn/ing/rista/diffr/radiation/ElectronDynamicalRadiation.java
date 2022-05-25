/*
 * @(#)ElectronDynamicalRadiation.java created Mars 2, 2012 Caen
 *
 * Copyright (c) 2012 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.diffr.radiation;

import it.unitn.ing.rista.diffr.RadiationType;
import it.unitn.ing.rista.diffr.XRDcat;

/**
 * The ElectronDynamicalRadiation is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Mar 2, 2012 1:19:56 PM $
 * @since JDK1.1
 */
public class ElectronDynamicalRadiation extends ElectronRadiation {

	public ElectronDynamicalRadiation(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		identifier = "Dynamical Electron";
		IDlabel = "Dynamical Electron";
		description = "Dynamical Electron radiation type";
	}

	public ElectronDynamicalRadiation(XRDcat aobj) {
		this(aobj, "Dynamical Electron");
	}

	public ElectronDynamicalRadiation() {
		identifier = "Dynamical Electron";
		IDlabel = "Dynamical Electron";
		description = "Dynamical Electron radiation";
	}

	public void initParameters() {
		super.initParameters();
	}

	public boolean isElectron() {
		return true;
	}

	public boolean isDynamical() {
		return true;
	}

}
