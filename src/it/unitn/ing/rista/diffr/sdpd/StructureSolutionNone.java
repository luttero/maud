/*
 * @(#)StructureSolutionNone.java created 22/08/2001 Roma-Firenze
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

package it.unitn.ing.rista.diffr.sdpd;

import it.unitn.ing.rista.diffr.*;

/**
 *  The StructureSolutionNone is a fake method to solve the Crystal
 *  Structure
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:08 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class StructureSolutionNone extends StructureSolutionMethod {

  public StructureSolutionNone(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "None SDPD";
    IDlabel = "None SDPD";
    description = "select this to not solve structure";
  }

  public StructureSolutionNone(XRDcat aobj) {
    this(aobj, "None SDPD");
  }

  public StructureSolutionNone() {
    identifier = "None SDPD";
    IDlabel = "None SDPD";
    description = "select this to not solve structure";
  }

}

