/*
 * @(#)NoneMagneticStructure.java	1.00 created 17/04/1999 Riva del Garda
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.magnetic;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;


/**
 *  The NoneMagneticStructure is a class to represent a magnetic structure
 *  and to compute the magnetic structure factors
 *
 *
 * @version $Revision: 1.5 $, $Date: 2004/08/12 09:36:07 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class NoneMagneticStructure extends MagneticStructure {

  public NoneMagneticStructure(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "no magnetic";
    IDlabel = "no magnetic";
    description = "select this for diamagnetic phase";
  }

  public NoneMagneticStructure(XRDcat aobj) {
    this(aobj, "Diamagnetic structure x");
  }

  public NoneMagneticStructure() {
    identifier = "no magnetic";
    IDlabel = "no magnetic";
    description = "select this for diamagnetic phase";
  }

}
