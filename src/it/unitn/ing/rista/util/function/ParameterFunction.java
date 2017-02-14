/*
 * @(#)ParameterFunction.java created 21/03/1999 Pergine Vals.
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

import java.util.*;

import it.unitn.ing.rista.diffr.*;

/**
 * The ParameterFunction is an class that implements a generic function
 * of a parameters list; to be subclassed.
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:10 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class ParameterFunction {

  /**
   The parameter list of Parameter.
   */
  Vector parameterList = null;

  /**
   The minimum value for coordinate x.
   */
  double minValue = 0.0;

  /**
   The maximum value for coordinate x.
   */
  double maxValue = 155.0;

  /**
   boolean value controlling if the coordinate is 2-Theta or d-spacing, dspacingbase false means 2-Theta.
   */
  boolean dspacingbase = false;

  /**
   General constructor; do nothing.
   */
  public ParameterFunction() {
  }

  /**
   Constructor that pass the parameter list to the class.

   @param			parameterlist the list of Parameter.
   */
  public ParameterFunction(Vector parameterlist) {
    this();

    parameterList = parameterlist;
  }

  /**
   Constructor that pass coordinate setting.

   @param			dspacing true if coordinate is in d-spacing instead of 2-Theta.
   */
  public ParameterFunction(boolean dspacing) {
    this();

    dspacingbase = dspacing;
    if (dspacingbase) {
      minValue = 0.001;
      maxValue = 10.0;
    }
  }

  /**
   Constructor that pass the parameter list and the coordinate setting.

   @param			parameterlist the list of Parameter.
   @param			dspacing true if coordinate is in d-spacing instead of 2-Theta.
   */
  public ParameterFunction(Vector parameterlist, boolean dspacing) {
    this(parameterlist);

    dspacingbase = dspacing;
    if (dspacingbase) {
      minValue = 0.001;
      maxValue = 10.0;
    }
  }

  /**
   Set the coordinate setting.

   @param			dspacing true if coordinate is in d-spacing instead of 2-Theta.
   */
  public void setDSpacing(boolean dspacing) {
    dspacingbase = dspacing;
    if (dspacingbase) {
      minValue = 0.001;
      maxValue = 10.0;
    }
  }

  /**
   Set the coordinate setting to d-spacing.
   */
  public void setDSpacing() {
    setDSpacing(true);
  }

  /**
   Set the coordinate setting to 2-Theta.
   */
  public void setTwoTheta() {
    setDSpacing(false);
  }

  /**
   Set the parameter list to be used.

   @param			parameterlist the list of Parameter.
   */
  public void setList(Vector parameterlist) {
    parameterList = parameterlist;
  }

  /**
   Return the parameter list in use.

   @return		Vector the list of Parameter.
   */
  public Vector getList() {
    return parameterList;
  }

  /**
   Set the minimum value for the coordinate x.

   @param			value the minimum value to which the function is computed.
   */
  public void setMin(double value) {
    minValue = value;
  }

  /**
   Return the minimum value for the coordinate x.

   @return		double the minimum value to which the function is computed.
   */
  public double getMin() {
    return minValue;
  }

  /**
   Set the maximum value for the coordinate x.

   @param			value the maximum value to which the function is computed.
   */
  public void setMax(double value) {
    maxValue = value;
  }

  /**
   Return the maximum value for the coordinate x.

   @return		double the maximum value to which the function is computed.
   */
  public double getMax() {
    return maxValue;
  }

  /**
   Return the number of parameters in use.

   @return		int the number of Parameter.
   */
  public int getNumberOfParameters() {
    if (getList() != null)
      return getList().size();
    else
      return 0;
  }

  /**
   Return the value of the parameter at the position index
   in the list.

   @param		index the position of the Parameter in the list.
   @return	the double value of the parameter; 0 if there is not a parameter.
   */
  public double getParameterValue(int index) {
    if (parameterList != null && (getNumberOfParameters() > index && index >= 0)) {
      Parameter par = (Parameter) getList().elementAt(index);
      return par.getValueD();
    } else
      return 0.0;
  }

  /**
   Return the function for the value x. To be implemented by subclasses.

   @param	x the point where the function wil be computed.
   */
  public double f(double x) {
    return 0.0;
  }

  /**
   Return the derivate for the value x. To be implemented by subclasses.

   @param	x the point where the derivate wil be computed.
   */
  public double fprime(double x) {
    return 0.0;
  }
}
