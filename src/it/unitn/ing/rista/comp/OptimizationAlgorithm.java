/*
 * @(#)OptimizationAlgorithm.java created Feb 23, 2003 Berkeley
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.util.Fmt;
import it.unitn.ing.rista.util.Misc;

/**
 * The OptimizationAlgorithm is an interface ......
 *
 * @version $Revision: 1.9 $, $Date: 2006/01/19 14:45:54 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class OptimizationAlgorithm extends XRDcat implements OptimizationAlgorithmI {

  Function fittingFunction;
  OutputFrameI outputframe = null;
  int numberOfIterations = 0;
  SimpleFunction simpleFittingFunction;
  public boolean outputEnabled = true;
	public static String iterations = "analysis.iterations";

	public OptimizationAlgorithm(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public OptimizationAlgorithm(XRDcat aobj) {
    this(aobj, "Generic structure x");
  }

  public OptimizationAlgorithm() {
    identifier = "Optimization Algorithm";
    IDlabel = "Optimization Algorithm";
    description = "select this to use an Optimization Algorithm";
  }

  public OptimizationAlgorithm(SimpleFunction fitFunction, int iterations) {
    setSimpleFunction(fitFunction);
    setIterations(iterations);
  }

  public void setParent(XRDcat obj) {
    super.setParent(obj);
//    setFunction((Function) obj);
  }

  public void solve(launchBasic computation, Function functionToMinimize) {}

  public void solveXGRID(launchBasic computation, Function functionToMinimize) {
    solve(computation, functionToMinimize);
  }

  public void solveJPVM(launchBasic computation, Function functionToMinimize) {
    solve(computation, functionToMinimize);
  }

  public void solveGeneral(launchBasic computation, Function functionToMinimize) {
    functionToMinimize.setOptimizing(true);
    Parameter.doInterfaceRefresh = false;
    switch (ParallelComputationController.activeStructure) {
      case ParallelComputationController.NONE:
        solve(computation, functionToMinimize);
      break;
      case ParallelComputationController.XGRID:
        solveXGRID(computation, functionToMinimize);
      break;
      case ParallelComputationController.JPVM:
        solveJPVM(computation, functionToMinimize);
      break;
      default: {
        solve(computation, functionToMinimize);
      }
    }
    Parameter.doInterfaceRefresh = true;    
    functionToMinimize.setOptimizing(false);
  }

  public double simpleSolve(double[] dta, double[] wgt, double[] fit, double[] parm, boolean releaseMemory,
                            int[] controls) {return 0;}

  public void setIterations(int number) {
    numberOfIterations = number;
  }

  public int getIterations() {
    return numberOfIterations;
  }

  public void setOutputFrame(OutputFrameI outframe) {
    outputframe = outframe;
  }

  public void setFunction(Function f) {
    fittingFunction = f;
  }

  public void setSimpleFunction(SimpleFunction f) {
    simpleFittingFunction = f;
  }

  public void printf(String astring) {
    if (!outputEnabled)
      return;
    if (outputframe != null) {
      outputframe.appendnewline(astring);
    } else
      System.out.println(astring);
  }

  public void printf(String astring, int value) {
    printf(astring + Integer.toString(value));
  }

  public void printf(String astring, int value, String astring1, int value1, String astring2) {
    printf(astring + Integer.toString(value) + astring1 + Integer.toString(value1) + astring2);
  }

  public void printf(String astring, double value) {
    printf(astring + Fmt.format(value));
  }

  public void printf(String astring, double value, String asstring, double svalue) {
    printf(astring + Fmt.format(value) + asstring + Fmt.format(svalue));
  }

  public void printf(int value, String astring) {
    printf(Integer.toString(value) + astring);
  }

  public void printf(double value1) {
    printf(Fmt.format(value1));
  }

  public void printf(double value1, double value2) {
    printf(Fmt.format(value1) + " " + Fmt.format(value2));
  }

  public void printf(int value1, double value2) {
    printf(Integer.toString(value1) + " " + Fmt.format(value2));
  }

  public void printf(double value1, double value2, double value3) {
    printf(Fmt.format(value1) + " " + Fmt.format(value2) +
            " " + Fmt.format(value3));
  }

  public void printf(String value, double value1, double value2, double value3) {
    printf(value + Fmt.format(value1) + " " + Fmt.format(value2) +
            " " + Fmt.format(value3));
  }

  public void printf(String astring, int value1, int value2, double value3) {
    printf(astring + Integer.toString(value1) + " " + Integer.toString(value2) +
            " " + Fmt.format(value3));
  }

  public void printout(double[] parmn, int nprm) {
    for (int j = 0; j < nprm; j += 3) {
      if (j + 2 < nprm)
        printf(parmn[j], parmn[j + 1], parmn[j + 2]);
      else if (j + 1 < nprm)
        printf(parmn[j], parmn[j + 1]);
      else
        printf(parmn[j]);
    }
  }

/*  public void printout(double[] parmn, int nprm) {
    for (int j = 0; j < nprm; j += 3) {
      if (j + 2 < nprm)
        printf(parmn[j], parmn[j + 1], parmn[j + 2]);
      else if (j + 1 < nprm)
        printf(parmn[j], parmn[j + 1]);
      else
        printf(parmn[j]);
    }
  }*/

}
