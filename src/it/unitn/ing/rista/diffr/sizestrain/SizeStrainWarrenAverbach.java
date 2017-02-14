/*
 * @(#)SizeStrainWarrenAverbach.java created Jul 23, 2004 Braila
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

package it.unitn.ing.rista.diffr.sizestrain;

import it.unitn.ing.rista.diffr.SizeStrainModel;
import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.diffr.Reflection;
import it.unitn.ing.rista.diffr.FourierTransformPeak;
import it.unitn.ing.rista.interfaces.Peak;


/**
 * The SizeStrainWarrenAverbach is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2004/08/12 09:38:45 $
 * @since JDK1.1
 */

public class SizeStrainWarrenAverbach extends SizeStrainModel {

  public SizeStrainWarrenAverbach(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Disabled Warren-Averbach";
    IDlabel = "Warren-Averbach";
    description = "select this to apply the Warren-Averbach model";
  }

  public SizeStrainWarrenAverbach(XRDcat aobj) {
    this(aobj, "Line Broadening Warren-Averbach model");
  }

  public SizeStrainWarrenAverbach() {
    identifier = "Disabled Warren-Averbach";
    IDlabel = "Warren-Averbach";
    description = "select this to apply the Warren-Averbach model";
  }

/*  public Peak createPeak(double pos, boolean control, double[] wave, double[] weight,
                         Reflection reflex, int order) {

    return new FourierTransformPeak(pos, control, wave, weight, reflex, order, this);
  }*/

}
