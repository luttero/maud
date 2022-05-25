/*
 * @(#)TDSModel.java created Jun 30, 2003 Berkeley
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr;


/**
 * The TDSModel is a class
 *
 * @version $Revision: 1.2 $, $Date: 2004/08/12 09:36:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class TDSModel extends XRDcat {

  public TDSModel(XRDcat obj, String alabel) {
    super(obj, alabel);
  }

  public TDSModel(XRDcat afile) {
    this(afile, "TDSModel x");
  }

  public TDSModel() {
  }

  public void initParameters() {
    super.initParameters();
  }

  public void computeTDS(DiffrDataFile spectrum, double[] expfit, PseudoVoigtPeak peak, double[] intensity,
                         double Fhkl, double[] position, int[] minmaxindex) {
  }

}
