/*
 * @(#)DefaultInstrument.java created Nov 15, 2004 Casalino
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.Instrument;
import it.unitn.ing.rista.diffr.XRDcat;


/**
 * The DefaultInstrument is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.3 $, $Date: 2006/11/10 09:33:01 $
 * @since JDK1.1
 */

public class DefaultInstrument extends Instrument {

  public static String modelID = "Diffraction Instrument";

  public DefaultInstrument(XRDcat obj, String alabel) {
    super(obj, alabel);
    identifier = modelID;
    IDlabel = modelID;
    description = modelID;
  }

  public DefaultInstrument(XRDcat afile) {
    this(afile, modelID);
  }

  public DefaultInstrument() {
    identifier = modelID;
    IDlabel = modelID;
    description = modelID;
  }

}
