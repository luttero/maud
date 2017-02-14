/*
 * @(#)IntegralEWIMVTexture.java created Dec 29, 2003 Casalino
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

package it.unitn.ing.rista.diffr.rta;

import it.unitn.ing.rista.diffr.XRDcat;


/**
 * The IntegralEWIMVTexture is a class for texture analysis
 * using the E-WIMV method modified for detectors collecting
 * over an area instead of point-wise.
 * 
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2004/01/21 11:17:00 $
 * @since JDK1.1
 */

public class IntegralEWIMVTexture extends MEMLTexture {

  public IntegralEWIMVTexture(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "Disabled Integral E-WIMV";
    IDlabel = "Integral E-WIMV";
    description = "select this to apply the arbitrary grid Integral E-WIMV method";
  }

  public IntegralEWIMVTexture(XRDcat aobj) {
    this(aobj, "Integral E-WIMV");
  }

  public IntegralEWIMVTexture() {
    identifier = "Disabled Integral E-WIMV";
    IDlabel = "Integral E-WIMV";
    description = "select this to apply the arbitrary grid Integral E-WIMV method";
  }

  public double getParameterMinSignificantValue(int i) {
    return 0;
  }
}
