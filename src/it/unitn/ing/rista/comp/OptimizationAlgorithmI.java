/*
 * @(#)OptimizationAlgorithmI.java created Feb 27, 2003 Berkeley
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

package it.unitn.ing.rista.comp;

import it.unitn.ing.rista.interfaces.Function;

import java.lang.*;

/**
 * The OptimizationAlgorithmI is a class
 *
 * @version $Revision: 1.6 $, $Date: 2005/03/10 13:50:38 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public interface OptimizationAlgorithmI {

  public void solve(launchBasic computation, Function functionToMinimize);

  public double simpleSolve(double[] dta, double[] wgt, double[] fit, double[] parm, boolean releaseMemory,
                          int[] controls);

  public void setIterations(int numberOfIterations);

  public int getIterations();

  public void setOutputFrame(OutputFrameI outputframe);

  public void setFunction(Function f);

}
