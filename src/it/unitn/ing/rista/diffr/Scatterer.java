/*
 * @(#)Scatterer.java created 04/06/16 Casalino
 *
 * Copyright (c) 1996-2016 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of the author and it is 
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

import it.unitn.ing.rista.util.AtomQuantity;

/**
 * The Scatterer is a class ....
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.0 $, $Date: 04/06/16 12:10 $
 * @since JDK1.1
 */

public class Scatterer extends XRDcat {

	public Scatterer(XRDcat obj, String alabel) {
		super(obj, alabel);
		initXRD();
	}

	public Scatterer(XRDcat afile) {
		this(afile, "Scatterer x");
	}

	public Scatterer() {
	}

	public double getSiteWeight() {
		return 0;
	}

	public AtomQuantity getAtomQuantity() {
		return null;
	}

	/**
	 * Gets the total absorption of this scatterer. Proportional to the quantity.
	 *
	 * @param rad the Radiation kind for which the absorption must be computed.
	 * @return the total absorption of this scatterer.
	 */
/*	public double getSiteAbsorption(RadiationType rad) {
		return 0;
	}*/

}