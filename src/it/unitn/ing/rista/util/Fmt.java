/*
 * @(#)Fmt.java created 20/01/2001 Casalino
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

package it.unitn.ing.rista.util;

/**
 * The Fmt is a class
 *
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Fmt {

  public static String format(double value) {
//	System.out.println(value);
    if (Double.isNaN(value) || Double.isInfinite(value) || value == 0.0)
      return "0.0";
    String result = "";
    double avalue = Math.abs(value);

    if (avalue < Float.MIN_VALUE) {
      result = Double.toString(value);
    } else if (avalue > Float.MAX_VALUE) {
      result = Double.toString(value);
    } else {
      // we cast the value to a float to reduce the number of digits
      float fvalue = (float) value;
      result = Float.toString(fvalue);
    }
//	System.out.println("Formatting to : " + result);
    return result;
  }

}
