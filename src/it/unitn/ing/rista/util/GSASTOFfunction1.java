/*
 * @(#)GSASTOFfunction1.java created 19/10/2006 Verona-Trento
 *
 * Copyright (c) 2006 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.util;

import com.imsl.math.Sfun;

/**
 * The GSASTOFfunction1 is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/12/04 14:30:15 $
 * @since JDK1.1
 */

public class GSASTOFfunction1 {

  public GSASTOFfunction1() {
  }

	public static final double getIntensity(double intensity, double norm, double u, double v, double y, double z) {
    return intensity * norm * getY(u, v, y, z);
	}


  public static final double getY(double u, double v, double y, double z) {
    double a1 = 0.0;
    double a2 = 0.0;
    if (u == 0.0)
      a1 = Sfun.erfc(y);
    else if (u < 30.0 && u > -30.0)
      a1 = Math.exp(u) * Sfun.erfc(y);
    if (v == 0.0)
      a2 = Sfun.erfc(z);
    else if (v < 30.0 && v > -30.0)
      a2 = Math.exp(v) * Sfun.erfc(z);
    return  a1 + a2;
  }

}
