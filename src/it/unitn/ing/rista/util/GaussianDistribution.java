/*
 * @(#)GaussianDistribution.java created 08/12/1998 Berkeley, CA
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
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

/**
 *  The GaussianDistribution is a class
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:10 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GaussianDistribution {

  private GaussianDistribution() {
  }

  public static final double getY(double hwhm, double dx) {
    double value = dx / hwhm;
    value *= value;
    if (value > 100.0)
      return 0.0;
    return Math.exp(-value);
  }

  public static final double getX(double hwhm, double y) {
    return -(hwhm * Math.sqrt(Math.abs(Math.log(Math.abs(y)))));
  }
}
