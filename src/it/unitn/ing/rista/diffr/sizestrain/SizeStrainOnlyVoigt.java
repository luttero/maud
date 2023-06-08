/*
 * @(#)SizeStrainOnlyVoigt.java created 16/7/1999 Pergine
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.sizestrain;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

/**
 *  The SizeStrainOnlyVoigt is a class
 *
 *
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:58 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SizeStrainOnlyVoigt extends SizeStrainModel {

  public SizeStrainOnlyVoigt(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Disabled Voigt microstrain";
    IDlabel = "Voigt microstrain";
    description = "select this to apply the Voigt microstrain model";
  }

  public SizeStrainOnlyVoigt(XRDcat aobj) {
    this(aobj, "Voigt microstrain model");
  }

  public SizeStrainOnlyVoigt() {
    identifier = "Disabled Voigt microstrain";
    IDlabel = "Voigt microstrain";
    description = "select this to apply the Voigt microstrain model";
  }

  public double getBetaChauchy(double dspace, double cryst, double mstrain) {
    if (cryst == 0.0)
      return 0.0;
    cryst = Math.abs(cryst);
    mstrain *= Constants.mstraintoetilde;
    mstrain = Math.abs(mstrain);
    return dspace * dspace / cryst + 2.0 * mstrain * dspace;
  }

  public double getBetaGauss(double dspace, double cryst, double mstrain) {

    return 0.0;

  }

}
