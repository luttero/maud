/*
 * @(#)NoStrain.java created 16/11/1999 Pergine Vals.
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.rsa;

import it.unitn.ing.rista.diffr.*;

/**
 *  The NoStrain is a class that implements a default (zero) strain model
 *
 *
 * @version $Revision: 1.5 $, $Date: 2006/07/20 13:39:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class NoStrain extends Strain {
  //insert class definition here

  public NoStrain(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "no strain";
    IDlabel = "no strain";
    description = "select this for unstrained phase";
  }

  public NoStrain(XRDcat aobj) {
    this(aobj, "no strain");
  }

  public NoStrain() {
    identifier = "no strain";
    IDlabel = "no strain";
    description = "select this for unstrained phase";
  }

  public double computeStrain(double psi, double beta, double chi, double phi) {
    // Angles must be in radiants
    // psi and beta are the polar and azimuthal angles for the crystal setting
    // phi and chi for the sample

    return 0.0;
  }
}
