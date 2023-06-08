/*
 * @(#)LayerNoSolution.java created 29/10/2001 Le Mans
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

package it.unitn.ing.rista.diffr.reflectivity;

import it.unitn.ing.rista.diffr.*;

import java.lang.*;

/**
 *  The LayerNoSolution is a generic model for working out heterostructures
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:07 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class LayerNoSolution extends LayerSolutionMethod {

  public LayerNoSolution(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "None Layer workout";
    IDlabel = "None Layer workout";
    description = "select this to not workout layers structure";
  }

  public LayerNoSolution(XRDcat aobj) {
    this(aobj, "None Layer workout");
  }

  public LayerNoSolution() {
    identifier = "None Layer workout";
    IDlabel = "None Layer workout";
    description = "select this to not workout layers structure";
  }

}
