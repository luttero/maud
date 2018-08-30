/*
 * @(#)DiffractionAngleEnergyMap.java created 29/8/2018 Povo
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
import java.lang.*;

/**
 *  The DiffractionAngleEnergyMap is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2018/08/29 11:15:28 $
 * @author Luca Lutterotti
 * @since JDK1.8
 */

public class DiffractionAngleEnergyMap extends Diffraction {

	public DiffractionAngleEnergyMap(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initXRD();
		identifier = "Angle-energy map diffraction";
		IDlabel = identifier;
		description = identifier;
	}

	public DiffractionAngleEnergyMap(XRDcat aobj) {
		this(aobj, "Angle-energy map diffraction");
	}

	public DiffractionAngleEnergyMap() {
		identifier = "Angle-energy map diffraction";
		IDlabel = identifier;
		description = identifier;
	}

}
