/*
 * @(#)MEMFunction.java created 31/07/1999 Pergine Vals.
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

package it.unitn.ing.rista.interfaces;

/**
 * The MEMFunction is an interface for Maximum Entropy Method function.
 *
 * @version $Revision: 1.4 $, $Date: 2005/09/07 17:14:08 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public interface MEMFunction extends Function {

//  public double[] getData();

  public double[] getWeight();

  public void setFit(int index, double value);

  public double getRw();

  public double getR();

  public double getSS();

  public double[] getFreeParameters(boolean initialCall);

  public void setFreeParameters(double[] parm);

  public void normalizeFit();

  public void computeFit(double[] parmn);

  public boolean checkBound(int j, double parmn);

  public int[] getMEMCellID(int dataindex);

  public double[] getMEMCellWGT(int dataindex);

  public double[] getMEMCountData();

  public double getRexponent();

  public int getCyclesNumber();
  
}

