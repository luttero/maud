/*
 * @(#)SizeStrainPopa.java created 09/10/1998 Mesiano
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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
 *  The SizeStrainPopa is a class that implement the Popa model for
 *  separing size and microstrains
 *
 *
 * @version $Revision: 1.5 $, $Date: 2006/07/20 13:39:07 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SizeStrainPopa extends SizeStrainModel {

  public SizeStrainPopa(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Popa LB";
    IDlabel = "Popa LB";
    description = "select this to apply the Popa LB model";
  }

  public SizeStrainPopa(XRDcat aobj) {
    this(aobj, "Line Broadening Popa model");
  }

  public SizeStrainPopa() {
    identifier = "Popa LB";
    IDlabel = "Popa LB";
    description = "select this to apply the Popa LB model";
  }

  public double getBetaChauchy(double dspace, double cryst, double mstrain) {
    if (cryst == 0.0)
      return 0.0;
    return 2.0 / 3.0 * dspace * dspace / Math.abs(cryst);
  }

  public double getBetaGauss(double dspace, double cryst, double mstrain) {
    return 2.0 * Math.abs(mstrain) * Constants.mstraintoetilde * dspace;
  }
}
