/*
 * @(#)MaximumEntropyFourierMap.java created 11/11/2001 Riva Del Garda
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

import it.unitn.ing.rista.interfaces.MEMFunction;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.XRDcat;

/**
 * The MaximumEntropyFourierMap is a general class to fit data using the Maximum Entropy
 * Method.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.7 $, $Date: 2006/01/19 14:45:54 $
 * @since JDK1.1
 */

public class MaximumEntropyFourierMap extends MaximumEntropyFit {

  public MaximumEntropyFourierMap(MEMFunction f, OutputPanel outframe) {
    super(f, outframe);
  }

  public void solve(launchBasic computation) {

    double lambdaMax = MaudPreferences.getDouble("entropy.lambdaMax", 10.0);
    double lambdaIncrement = MaudPreferences.getDouble("entropy.lambdaIncrement", 0.31622777);
    double lambdaDecrement = MaudPreferences.getDouble("entropy.lambdaDecrement", 10.0);

// 		fittingFunction.mainfunction(false, true);

    nprm = fittingFunction.getNumberOfFreeParameters();
    double[] parm = fittingFunction.getFreeParameters(true);
    double[] parmn = new double[nprm];
	  for (int i = 0; i < nprm; i++)
		  parmn[i] = parm[i];
//		fittingFunction.computeFirstFit();
    fittingFunction.normalizeFit();

    dataNumber = fittingFunction.getNumberOfData();
    double[] oldfit = new double[dataNumber];

//		dta = fittingFunction.getData();
    wgt = fittingFunction.getWeight();
//		fit = fittingFunction.getFit();

/*    double wgtnorm = 0.0;
    for (int i = 0; i < dataNumber; i++) {
      wgtnorm += wgt[i];
    }
    wgtnorm /= dataNumber;*/

    if (outputframe != null) {
      outputframe.reset();
    }
    printf("Computing Fourier Map for phase: " +
        ((XRDcat) fittingFunction).getParent().toXRDcatString());
    int repeatMax = fittingFunction.getCyclesNumber();
    printf("Number of cycles: " + repeatMax);
    for (int repetition = 0; repetition < repeatMax; repetition++) {
      printf("Cycle: " + repetition);
      fittingFunction.computeFit(parm);
	    fittingFunction.getFit();
	    double wss = fittingFunction.getWSS();

      // checking if the weighted sum of squares is not NaN or Infinity

      if (MoreMath.isInvalidNumber(wss))
        return;


      printf("Starting Wss = ", wss);

      int iter = 0;
      if (ipflg != 0)
        iter = niter;
      int check = 0; // if check != 0 the new wss can be sligthly greater than the old one
      int brkflg = 0; // operator interrupt
      int mxiter = numberOfIterations;
      if (mxiter < 0)
        mxiter = 0;
      printf("Number of iterations : ", mxiter);

// start entropy maximization

      int conver = 0;
      double lambda = fittingFunction.getRexponent();
      double lmin = 1.0e-20;
      double phi = 1.0;
      niter = -1;
      double oldwss = wss;
      int incr = 0;
      double factord = 10.0;

//		fittingFunction.saveparameters();

      if (outputframe != null) {
        outputframe.getProgressBar().setProgressBarValue(mxiter);
      }
      boolean flagfgl = (conver == 0) && (brkflg == 0);
      while ((niter + 1 < mxiter) && flagfgl) {
        ++niter;
        if ((niter > 4) && (lambda < lmin))
          lmin = lambda;
        printf("Iterations : ", niter + 1);
        String message = "Computing iteration # " + Integer.toString(niter + 1)
            + " of " + Integer.toString(mxiter);
        if (outputframe != null) {
          outputframe.setProgressText(message);
        } else
          printf(message);

//           increase lambda
        if (lambda < lambdaMax * lambdaIncrement)
          lambda /= lambdaIncrement;
        else
          lambda /= Math.sqrt(lambdaIncrement);
        incr = 0;
//		printf("lambda1 = ",lambda);

        flg = 0;
        do {
          if (Constants.testing)
            printf("Lambda: ", lambda);
          entropy(parmn, lambda, factord);    //    lambda / wgtnorm

          if (conver == 0) {
/*    check the odf convergence */

            n0 = 0;
            for (int i = 0; i < nprm; i++) {
              if (Math.abs(parmn[i] - parm[i]) <= Math.abs(prcsn * parm[i]))
                ++n0;
            }
            if (nprm < 50)
              printout(parmn, nprm);

          }
          if (n0 == nprm)
            conver = 1;
          else {
            boolean bounds = false;
            for (int j = 0; j < nprm; j++)
              bounds = (bounds || fittingFunction.checkBound(j, parmn[j]));
            if (bounds) {
              wss = oldwss * 1.01;
              printf("At least one parameter out of bounds, recomputing...");
            } else {
              for (int ij = 0; ij < dataNumber; ij++)
                oldfit[ij] = fittingFunction.getFit(ij);
              fittingFunction.setFreeParameters(parmn);
              fittingFunction.computeFit(parmn);
//    				fit = fittingFunction.getFit();
	            fittingFunction.getFit();
              wss = fittingFunction.getWSS();
              printf("Partial Wgt'd ssq = ", wss);

            }
            if (MoreMath.isInvalidNumber(wss))
              wss = oldwss * 1.02;
            if (wss < oldwss && wss >= 0.0) {
              oldwss = wss;
              for (int ij = 0; ij < dataNumber; ij++)
                oldfit[ij] = fittingFunction.getFit(ij);
            } else {
              if (check != 0) {
                oldwss = wss;
                wss = oldwss * 1.01;
              }
//  					if (lambda < prcsn)
//    					lambda = prcsn;
              incr++;
              lambda /= lambdaDecrement;
              if (lambda < 0.1 * lmin)
                conver = 1;
	            for (int i = 0; i < nprm; i++) {
		            parmn[i] = parm[i];
	            }
            }
            for (int ij = 0; ij < dataNumber; ij++)
              fittingFunction.setFit(ij, oldfit[ij]);
          }
        } while ((wss > oldwss) && (conver == 0));

        fittingFunction.normalizeFit();
//    	fit = fittingFunction.getFit();
	      fittingFunction.getFit();
        wss = fittingFunction.getWSS();
        printf("Wgt'd ssq = ", wss);
        oldwss = wss;

        flagfgl = (conver == 0) && (brkflg == 0);

        if (conver == 0) {
          parm = fittingFunction.getFreeParameters(false);
          for (int i = 0; i < nprm; i++) {
            parmn[i] = parm[i];
          }
          if (outputframe != null)
            outputframe.increaseProgressBarValue();

        }
      }       //   next iteration


      if (conver == 0) {
        if (niter + 1 == mxiter) {
          ++niter;
          printf(mxiter, ", iterations - pause ");
        } else {
          printf("operator interrupt");
          brkflg = 0;
        }
      } else {
//             convergence reached
        double ri = lambda / lmin;
        printf("           convergence reached");
        printf("# of params fit = ", nprm);
        printf("# of params converged = ", n0);
        printf("lambda/l(min) = ", ri);
        n0 = 0;
      }
      parm = fittingFunction.getFreeParameters(true);
    }
    finaloutput(nprm, dataNumber, niter);
    fittingFunction.setFreeParameters(parm);
    fittingFunction.saveparameters();
  }

	double[] map = null;

	public void entropy(double[] parm, double relax, double factord) {
		if (map == null || map.length != nprm)
			map = new double[nprm];
    double val1, val2;

    for (int n = 0; n < nprm; n++)
      map[n] = 0;

    for (int i = 0; i < dataNumber; i++) {
      val1 = (fittingFunction.getData(i * 2) - fittingFunction.getFit(i * 2)) / wgt[i];
      val2 = (fittingFunction.getData(i * 2 + 1) - fittingFunction.getFit(i * 2 + 1)) / wgt[i];
      double[] cellWGT = fittingFunction.getMEMCellWGT(i);
      for (int j = 0; j < nprm; j++) {
        map[j] += val1 * cellWGT[2 * j] + val2 * cellWGT[2 * j + 1];
      }
    }
    for (int n = 0; n < nprm; n++) {
      parm[n] = parm[n] + relax * map[n] / dataNumber;
      if (parm[n] < 0)
        parm[n] = 0;
    }
    coverage = 1.0;
  }

}
