/*
 * @(#)MaximumEntropyTextureFit.java created 24/02/2001 Casalino
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

package it.unitn.ing.rista.comp;

import java.lang.*;

import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.util.*;

/**
 * The MaximumEntropyTextureFit is a general class to fit data using the Maximum Entropy
 * Method.
 *
 * @version $Revision: 1.7 $, $Date: 2006/01/19 14:45:54 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class MaximumEntropyTextureFit extends MaximumEntropyFit {

  public MaximumEntropyTextureFit(MEMFunction f, OutputPanel outframe) {
    super(f, outframe);
  }

	double[] parmn = null;

  public void entropy(double[] parm, double relax, double factord) {

//	  System.out.println("nprm & data: " + nprm + " " + dataNumber);
	  if (parmn == null || parmn.length != nprm)
		  parmn = new double[nprm];
    final double[] countdata = fittingFunction.getMEMCountData();
    double val1;
    for (int n = 0; n < nprm; n++)
      parmn[n] = 1;
		final double[] fit = fittingFunction.getFit();
    final double val = relax / factord;
	  final boolean lazyEntropy = MaudPreferences.getBoolean("ewimvEntropy.lazyMinimization", true);

	  final int maxThreads = Math.min(Constants.maxNumberOfThreads, dataNumber / 10);
	  if (maxThreads > 1 && Constants.threadingGranularity >= Constants.FINE_GRANULARITY) {
		  if (Constants.debugThreads)
			  System.out.println("Thread Entropy Texture Fit");
		  int i;
		  PersistentThread[] threads = new PersistentThread[maxThreads];
		  for (i = 0; i < maxThreads; i++) {
//			  final double[] parmnt = parmn;
			  final MaximumEntropyTextureFit lock = this;
			  threads[i] = new PersistentThread(i) {
				  @Override
				  public void executeJob() {
					  int i1 = this.getJobNumberStart();
					  int i2 = this.getJobNumberEnd();
					  if (lazyEntropy) {
						  double[] tempValues = new double[10000];
						  for (int i = i1; i < i2; i++) {
							  if (wgt[i] > 0.0) {
								  double val1 = fit[i];
								  if (val1 > 0.0)
									  val1 = fittingFunction.getData(i) / val1;
								  else
									  val1 = 0.0;
								  int[] cellID = fittingFunction.getMEMCellID(i);
								  double[] cellWGT = fittingFunction.getMEMCellWGT(i);
								  int cellNumber = cellID.length;
								  if (cellNumber > tempValues.length)
									  tempValues = new double[cellNumber];
								  if (cellWGT == null) {
									  double val3 = Math.pow(val1, val * wgt[i]);
									  synchronized(lock) {
										  for (int j = 0; j < cellNumber; j++)
											  parmn[cellID[j]] *= val3;
									  }
								  } else {
									  double val3 = val * wgt[i];
									  for (int j = 0; j < cellNumber; j++)
										  tempValues[j] = parmn[cellID[j]] * Math.pow(val1, val3 * cellWGT[j]);
									  synchronized(lock) {
										  for (int j = 0; j < cellNumber; j++)
											  parmn[cellID[j]] = tempValues[j];
									  }
								  }
							  }
						  }
					  } else {
						  double[] tempValues = new double[10000];
						  for (int i = i1; i < i2; i++) {
							  if (wgt[i] > 0.0) {
								  double val1 = fit[i];
								  if (val1 > 0.0)
									  val1 = fittingFunction.getData(i) / val1;
								  else
									  val1 = 0.0;
								  int[] cellID = fittingFunction.getMEMCellID(i);
								  double[] cellWGT = fittingFunction.getMEMCellWGT(i);
								  int cellNumber = cellID.length;
								  if (cellNumber > tempValues.length)
									  tempValues = new double[cellNumber];
								  if (cellWGT == null) {
									  double val3 = Math.pow(val1, val * wgt[i]);
									  synchronized(lock) {
										  for (int j = 0; j < cellNumber; j++)
											  parmn[cellID[j]] *= val3;
									  }
								  } else {
									  double val3 = val * wgt[i];
									  for (int j = 0; j < cellNumber; j++)
										  tempValues[j] = Math.pow(val1, val3 * cellWGT[j]);
									  synchronized(lock) {
										  for (int j = 0; j < cellNumber; j++)
											  parmn[cellID[j]] *= tempValues[j];
									  }
								  }
							  }
						  }
					  }

				  }
			  };
		  }
		  i = 0;
		  int istep = (int) (0.9999 + dataNumber / maxThreads);
		  for (int j = 0; j < maxThreads; j++) {
			  int is = i;
			  if (j < maxThreads - 1)
				  i = Math.min(i + istep, dataNumber);
			  else
				  i = dataNumber;
			  threads[j].setJobRange(is, i);
			  threads[j].start();
		  }
		  boolean running;
		  do {
			  running = false;
			  try {
				  Thread.sleep(Constants.timeToWaitThreadsEnding);
			  } catch (InterruptedException r) {
			  }
			  for (int h = 0; h < maxThreads; h++) {
				  if (!threads[h].isEnded())
					  running = true;
			  }
		  } while (running);

	  } else {
		  for (int i = 0; i < dataNumber; i++) {
			  if (wgt[i] > 0.0) {
				  val1 = fit[i];
				  if (val1 > 0.0)
					  val1 = fittingFunction.getData(i) / val1;
				  else
					  val1 = 0.0;
				  int[] cellID = fittingFunction.getMEMCellID(i);
				  double[] cellWGT = fittingFunction.getMEMCellWGT(i);

				  int cellNumber = cellID.length;
				  if (cellWGT == null) {
					  double val3 = Math.pow(val1, val * wgt[i]);
					  for (int j = 0; j < cellNumber; j++)
						  parmn[cellID[j]] *= val3;
				  } else {
					  double val3 = val * wgt[i];
					  for (int j = 0; j < cellNumber; j++)
						  parmn[cellID[j]] *= Math.pow(val1, val3 * cellWGT[j]);
				  }
			  }
		  }

	  }

	  int uncover = nprm;
	  for (int n = 0; n < nprm; n++) {
		  if (countdata[n] > 0) {
			  parm[n] = Math.pow(parmn[n], factord / countdata[n]) * Math.abs(parm[n]);
		  } else {
			  parm[n] = -1;
			  uncover--;
		  }
	  }
    coverage = (double) uncover / nprm;
//	  System.out.println("End!!");
  }

}
