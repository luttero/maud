/*
 * @(#)SizeStrainNone.java created 09/10/1998 Mesiano
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

/**
 *  The SizeStrainNone is a class
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:08 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SizeStrainNone extends SizeStrainModel {

  public SizeStrainNone(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "None";
    IDlabel = "None";
    description = "select this to apply any LB model";
  }

  public SizeStrainNone(XRDcat aobj) {
    this(aobj, "No Line Broadening model");
  }

  public SizeStrainNone() {
    identifier = "None";
    IDlabel = "None";
    description = "select this to apply any LB model";
  }

}
