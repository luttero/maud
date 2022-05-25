/*
 * @(#)ElectronRadiation.java created Jun 30, 2008 Caen
 *
 * Copyright (c) 2008 Luca Lutterotti All Rights Reserved.
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
 * The ElectronRadiation is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 30, 2008 1:19:56 PM $
 * @since JDK1.1
 */
public class ElectronRadiation extends RadiationType {

  public ElectronRadiation(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "Electron";
    IDlabel = "Electron";
    description = "Electron radiation type";
  }

  public ElectronRadiation(XRDcat aobj) {
    this(aobj, "Electron");
  }

  public ElectronRadiation() {
    identifier = "Electron";
    IDlabel = "Electron";
    description = "Electron radiation";
  }

  public void initParameters() {
    super.initParameters();
  }

  public boolean isElectron() {
    return true;
  }

	public double energyToLambda(double e)
	{
		return 12.26426 / Math.sqrt(e * (1.0 + 9.784753E-7 * e));
	}

	public double lambdaToEnergy(double lambda)
	{
		return (Math.sqrt(1.0 + 5.8869799437E-4 / lambda / lambda) - 1.0) / 1.9569506E-6;
	}

	public boolean isDynamical() {
		return useDynamicalCorrection();
	}


}
