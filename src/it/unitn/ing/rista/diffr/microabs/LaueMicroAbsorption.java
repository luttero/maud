/*
 * @(#)LaueMicroAbsorption.java created Feb 5, 2009 Caen
 *
 * Copyright (c) 2009 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.diffr.microabs;

import it.unitn.ing.rista.diffr.*;

/**
 * The LaueMicroAbsorption is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Feb 5, 2009 5:44:01 PM $
 * @since JDK1.1
 */
public class LaueMicroAbsorption  extends MicroAbsorption {

  public LaueMicroAbsorption(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "Disabled Laue microabsorption";
    IDlabel = "Laue microabsorption";
    description = "Laue microabsorption correction is performed (only for Laue transmission camera)";
    initBaseObject();
  }

  public LaueMicroAbsorption(XRDcat aobj) {
    this(aobj, "Laue microabsorption");
  }

  public LaueMicroAbsorption() {
    identifier = "Disabled Laue microabsorption";
    IDlabel = "Laue microabsorption";
    description = "Laue microabsorption correction is performed (only for Laue transmission camera)";
  }

  public double getApparentQuantity(double volFraction, RadiationType rad,
                                    Layer alayer, double crystSize) {

    if (crystSize < 50.0)
      return volFraction;

    Phase aphase = (Phase) getParent();

    return volFraction * alayer.getDensity() / aphase.getDensity();
  }

}