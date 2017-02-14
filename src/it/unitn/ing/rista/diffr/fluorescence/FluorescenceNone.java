/*
 * @(#)FluorescenceNone.java created Apr 8, 2007 Casalino
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.fluorescence;

import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.diffr.Fluorescence;

/**
 * The FluorescenceNone is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Apr 8, 2007 9:52:50 PM $
 * @since JDK1.1
 */
public class FluorescenceNone extends Fluorescence {

  public FluorescenceNone(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "none fluorescence";
    IDlabel = "none fluorescence";
    description = "No fluorescence computation";
  }

  public FluorescenceNone(XRDcat aobj) {
    this(aobj, "none fluorescence");
  }

  public FluorescenceNone() {
    identifier = "none fluorescence";
    IDlabel = "none fluorescence";
    description = "No fluorescence computation";
  }


}
