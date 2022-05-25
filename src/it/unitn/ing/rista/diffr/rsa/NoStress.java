/*
 * @(#)NoStress.java created 16/11/1999 Pergine Vals.
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
import it.unitn.ing.rista.awt.*;

/**
 *  The NoStress is a class that implements a default (zero) stress model
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:07 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class NoStress extends Stress {
  //insert class definition here

  public NoStress(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "no stress";
    IDlabel = "no stress";
    description = "select this for unstressed phase";
  }

  public NoStress(XRDcat aobj) {
    this(aobj, "no stress");
  }

  public NoStress() {
    identifier = "no stress";
    IDlabel = "no stress";
    description = "select this for unstressed phase";
  }
}
