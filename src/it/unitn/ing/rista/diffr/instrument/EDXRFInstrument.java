/*
 * @(#)EDXRFInstrument.java created Apr 8, 2007 Casalino
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

package it.unitn.ing.rista.diffr.instrument;

import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.diffr.Instrument;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.Constants;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * The EDXRFInstrument is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Apr 8, 2007 10:55:45 PM $
 * @since JDK1.1
 */
public class EDXRFInstrument extends XRFInstrument {
  public static String modelID = "EDXRF Instrument";

  public EDXRFInstrument(XRDcat obj, String alabel) {
    super(obj, alabel);
    identifier = modelID;
    IDlabel = modelID;
    description = modelID;
  }

  public EDXRFInstrument(XRDcat afile) {
    this(afile, modelID);
  }

  public EDXRFInstrument() {
    identifier = modelID;
    IDlabel = modelID;
    description = modelID;
  }

}