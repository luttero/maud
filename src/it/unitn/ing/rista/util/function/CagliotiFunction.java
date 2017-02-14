/*
 * @(#)CagliotiFunction.java created 21/03/1999 Pergine Vals.
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

package it.unitn.ing.rista.util.function;

import it.unitn.ing.rista.util.*;

import java.util.*;
import java.lang.*;

/**
 * The CagliotiFunction is a class that implements a polynomial function
 * of a parameters list.
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:10 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class CagliotiFunction extends ParameterFunction {

  /**
   General constructor; do nothing.
   */
  public CagliotiFunction() {
    super();
  }

  /**
   Constructor that pass the parameter list to the class.

   @param			parameterlist the list of Parameter.
   */
  public CagliotiFunction(Vector parameterlist) {
    super(parameterlist);
  }

  /**
   Constructor that pass coordinate setting.

   @param			dspacing true if coordinate is in d-spacing instead of 2-Theta.
   */
  public CagliotiFunction(boolean dspacing) {
    super(dspacing);
  }

  /**
   Constructor that pass the parameter list and the coordinate setting.

   @param			parameterlist the list of Parameter.
   @param			dspacing true if coordinate is in d-spacing instead of 2-Theta.
   */
  public CagliotiFunction(Vector parameterlist, boolean dspacing) {
    super(parameterlist, dspacing);
  }

  /**
   Return the Caglioti function for the value x.

   @param	x the point where the function is computed.
   */
  public double f(double x) {

//		if (dspacingbase)
//			x = Math.asin(Constants.minimumdspace / (2 * x));
    double tanx = Math.tan(x * Constants.DEGTOPI / 2.0);

    double f = 0.0;
    for (int i = 0; i < getNumberOfParameters(); i++) {
      f += getParameterValue(i) * MoreMath.pow(tanx, i);
    }
    return Math.sqrt(Math.abs(f)) / 2.0;
  }

  /**
   Return the derivate of the Caglioti for the value x. Not completed.

   @param	x the point where the derivate is computed.
   */
  public double fprime(double x) {

    // wrong, be carefull to be finished

    if (dspacingbase)
      x = Math.asin(Constants.minimumdspace / (2 * x));
    double tanx = Math.tan(x * Constants.DEGTOPI / 2.0);

    double fprime = 0.0;
    for (int i = 1; i < getNumberOfParameters(); i++) {
      fprime += i * getParameterValue(i) * MoreMath.pow(tanx, i - 1);
    }
    return Math.sqrt(Math.abs(fprime)) / 2.0;
  }
}
