/*
 * @(#)Function.java created 01/01/1997 Mesiano
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

package it.unitn.ing.rista.interfaces;

import it.unitn.ing.rista.comp.OptimizationAlgorithm;

import java.io.OutputStream;

/**
 * The Function is a class
 *
 * @version $Revision: 1.9 $, $Date: 2006/02/02 16:11:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public interface Function extends SimpleFunction {

  public int getNumberOfData();

  public double getData(int index);

  public double getWeight(int index);

  public double getFit(int index);

  public double[] getFit();

  public double getWSS();

  public double[] getRefinementIndexes();

  public void setRw(double Rw);

  public void setR(double R);

  public void setRexp(double R);

  public int getNumberOfFreeParameters();

  public double getFreeParameter(int index);

  public void setFreeParameter(int index, double value);

//  public void setFreeParameter(int index, double value);

  public void setFreeParameters(double[] parm);

//  public void setFreeParameters(double[] parm);

  public void setErrors(double[] errors);

//  public void setErrors(double[] errors);

  public void saveparameters();

  public void computeFit();

  public boolean checkBound(int j, double parmn);

  public void backupallParameters();

  public void restoreParametersValues();

  public void setDerivate(boolean value);

  public void setOptimizing(boolean value);

  public boolean isOptimizing();

  public void mainfunction(boolean hasoutput, boolean refreshAll);

  public boolean reduceMemory();

  public boolean singleFunctionComputing();

  public void computeFirstFit();

  public int getNumberofIterations();

  public int prepareIteration();


/*	public double compFit(int index);
	public double derFit(int index, int par);*/
  public OutputStream getResultStream();

  public void endOfComputation();

  public boolean logOutput();

  public void printInformations(OutputStream resultStream);

  public void closeLogResultFile();

  public OptimizationAlgorithm getOptimizationAlgorithm();

  public void fittingFileOutput();

  public void prepareComputation();

  public double getLowerBound(int index);

  public double getUpperBound(int index);

  double getParameterMinSignificantValue(int i);
}

