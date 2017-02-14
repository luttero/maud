/*
 * @(#)Newton.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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
 * The Newton is a class
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:10 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Newton {

  NewtonFunction thefunction;
  double controlValue = 1.0E-6;
  int maxiterations = 300;
  double min, max;
  boolean minmax = false;

  public Newton(NewtonFunction afunction) {
    thefunction = afunction;
  }

  public void setControlValue(double newvalue) {
    controlValue = newvalue;
  }

  public void setMaxIteration(int newvalue) {
    maxiterations = newvalue;
  }

  public void setMinMaxValue(double minvalue, double maxvalue) {
    min = minvalue;
    max = maxvalue;
    minmax = true;
  }

  public double getSolution(double startingvalue) {
    double x0 = startingvalue;
    double x1 = x0 - thefunction.f(x0) / thefunction.fprime(x0);
    int count = 0;

    while ((Math.abs((x1 - x0) / x0) > controlValue) && (count < maxiterations)) {
      if (minmax) {
        while (x1 > max || x1 < min) {
          x1 = (x1 + x0) / 2.0;
        }
      } else
        x0 = x1;
      x1 = x0 - thefunction.f(x0) / thefunction.fprime(x0);
      count++;
    }

    if (minmax) {
      while (x1 > max || x1 < min) {
        x1 = (x1 + x0) / 2.0;
      }
    }
    return x1;
  }
}
