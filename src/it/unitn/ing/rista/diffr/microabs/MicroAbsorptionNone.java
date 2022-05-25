/*
 * @(#)MicroAbsorptionNone.java created 27/01/2001 Casalino
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

import java.lang.*;
import java.awt.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.*;

import javax.swing.*;


/**
 *  The MicroAbsorptionNone is the default class for microabsorption that
 *  implement a dummy model; no correction is performed.
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:07 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class MicroAbsorptionNone extends MicroAbsorption {

  public MicroAbsorptionNone(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "No microabsorption";
    IDlabel = "No microabsorption";
    description = "No microabsorption correction is performed";
  }

  public MicroAbsorptionNone(XRDcat aobj) {
    this(aobj, "No microabsorption");
  }

  public MicroAbsorptionNone() {
    identifier = "No microabsorption";
    IDlabel = "No microabsorption";
    description = "No microabsorption correction is performed";
  }

}
