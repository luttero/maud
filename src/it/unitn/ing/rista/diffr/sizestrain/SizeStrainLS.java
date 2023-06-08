/*
 * @(#)SizeStrainLS.java created 05/10/1998 Verona
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
 *  The SizeStrainLS is a class
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:08 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SizeStrainLS extends SizeStrainModel {

  public SizeStrainLS(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Disabled Trento";
    IDlabel = "Trento";
    description = "select this to apply the Trento model";
  }

  public SizeStrainLS(XRDcat aobj) {
    this(aobj, "Line Broadening Trento model");
  }

  public SizeStrainLS() {
    identifier = "Disabled Trento";
    IDlabel = "Trento";
    description = "select this to apply the Trento model";
  }

}
