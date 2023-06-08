/*
 * @(#)SizeStrainDelf.java created 05/10/1998 Verona
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
 *  The SizeStrainDelf is a class to model profile function with the
 *  method described by Delft researchers in the Young Rietveld book.
 *
 *
 * @version $Revision: 1.6 $, $Date: 2006/07/20 13:39:06 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SizeStrainDelf extends SizeStrainModel {

  public SizeStrainDelf(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Delft";
    IDlabel = "Delft";
    description = "select this to apply the Delft model";
  }

  public SizeStrainDelf(XRDcat aobj) {
    this(aobj, "Line Broadening Delft model");
  }

  public SizeStrainDelf() {
    identifier = "Delft";
    IDlabel = "Delft";
    description = "select this to apply the Delft model";
  }

  public double getBetaChauchy(double dspace, double cryst, double mstrain) {
    if (cryst == 0.0)
      return 0.0;
    return dspace * dspace / Math.abs(cryst);
  }

  public double getBetaGauss(double dspace, double cryst, double mstrain) {
    return 2.0 * Math.abs(mstrain) * Constants.mstraintoetilde * dspace;
  }

}
