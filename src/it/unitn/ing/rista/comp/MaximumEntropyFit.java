/*
 * @(#)MaximumEntropyFit.java created 04/08/1999 Mesiano
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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
import java.util.*;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.interfaces.*;

import java.awt.*;

/**
 * The MaximumEntropyFit is a general class to fit data using the Maximum Entropy
 * Method.
 *
 * @version $Revision: 1.9 $, $Date: 2006/01/19 14:45:54 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class MaximumEntropyFit extends Object {

  OutputPanel outputframe = null;

  MEMFunction fittingFunction;
  int numberOfIterations = 10;
  double prcsn = 0.0001;
  int ipflg = 0;
  int brkflg = 0;
  int n0 = 0;
  int flg;
  int niter = 0;
//	double dta[] = null;
  double wgt[] = null;
//	double fit[] = null;
  int dataNumber = 0;
  int nprm = 0;

  public MaximumEntropyFit(MEMFunction f, OutputPanel outframe) {
    setFunction(f);

    setOutputFrame(outframe);
  }

  public MaximumEntropyFit() {
  }

  public void setOutputFrame(OutputPanel outframe) {
    outputframe = outframe;
  }

  public void setFunction(MEMFunction f) {
    fittingFunction = f;
  }

  public void setPrecision(double value) {
    if (value != 0.0)
      prcsn = value;
  }

  public void setIterations(int number) {
    numberOfIterations = number;
  }

  public void solve(launchBasic computation) {

    double lambdaMax = MaudPreferences.getDouble("entropy.lambdaMax", 10.0);
    double lambdaIncrement = MaudPreferences.getDouble("entropy.lambdaIncrement", 0.31622777);
    double lambdaDecrement = MaudPreferences.getDouble("entropy.lambdaDecrement", 10.0);

// 		fittingFunction.mainfunction(false, true);

    nprm = fittingFunction.getNumberOfFreeParameters();
    double[] parm = fittingFunction.getFreeParameters(true);
//		fittingFunction.computeFirstFit();
    fittingFunction.normalizeFit();
    fittingFunction.computeFit(parm);

    dataNumber = fittingFunction.getNumberOfData();

//		dta = fittingFunction.getData();
    wgt = fittingFunction.getWeight();
//		fit = fittingFunction.getFit();

    double wgtnorm = 0.0;
    for (int i = 0; i < dataNumber; i++) {
      wgtnorm += wgt[i];
    }
    wgtnorm /= dataNumber;

	  fittingFunction.getFit();
	  double wss = fittingFunction.getWSS();

    // checking if the weighted sum of squares is not NaN or Infinity

    if (MoreMath.isInvalidNumber(wss))
      return;

    double[] parmn = new double[nprm];
	  for (int i = 0; i < nprm; i++)
		  parmn[i] = parm[i];

    printf("");
    printf("-----------------------");
    printf("Maximum Entropy fit: ");
    printf("Wss = ", wss);

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
    double[] oldfit = new double[dataNumber];

//		fittingFunction.saveparameters();

    boolean flagfgl = (conver == 0) && (brkflg == 0);
    while ((niter + 1 < mxiter) && flagfgl) {
      ++niter;
      if ((niter > 4) && (lambda < lmin))
        lmin = lambda;
      printf("Iterations : ", niter + 1);
      String message = "Computing iteration # " + Integer.toString(niter + 1)
              + " of " + Integer.toString(mxiter);
      if (outputframe != null)
        outputframe.setProgressText(message);
      else
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
    finaloutput(nprm, dataNumber, niter);
    printf("End of Maximum Entropy Fit");
    printf("-----------------------");
    printf("");
    fittingFunction.setFreeParameters(parm);
    fittingFunction.saveparameters();
  }

  public void entropy(double[] parm, double relax, double factord) {

    double[] parmn = new double[nprm];
    double[] countdata = new double[nprm];
    double val1 = 0.0, val2 = 0.0;

    for (int n = 0; n < nprm; n++) {
      parmn[n] = 1;
      countdata[n] = 0;
    }

    double val = relax / factord;
    for (int i = 0; i < dataNumber; i++) {
      if (wgt[i] > 0.0) {
        if (fittingFunction.getFit(i) > 0.0)
          val1 = fittingFunction.getData(i) / fittingFunction.getFit(i);
        else if (fittingFunction.getData(i) <= 0.0)
          val1 = 0.0;
        else
          val1 = fittingFunction.getData(i) * 10.0;
        val2 = wgt[i] * val;
        int[] cellID = fittingFunction.getMEMCellID(i);
        double[] cellWGT = fittingFunction.getMEMCellWGT(i);
        int cellNumber = cellID.length;
        if (cellWGT == null) {
          double val3 = Math.pow(val1, val2);
          for (int j = 0; j < cellNumber; j++) {
            countdata[cellID[j]] += wgt[i];
            parmn[cellID[j]] *= val3;
          }
        } else {
          for (int j = 0; j < cellNumber; j++) {
            countdata[cellID[j]] += cellWGT[j] * wgt[i];
            parmn[cellID[j]] *= Math.pow(val1, val2 * cellWGT[j]);
          }
        }
      }
    }
//  	boolean first = true;
    int uncover = 0;
    for (int n = 0; n < nprm; n++) {
      if (countdata[n] > 0) {
        parm[n] = Math.pow(parmn[n], factord / countdata[n]) * Math.abs(parm[n]);
/*	  		if (first) {
	  			first = false;
	  			System.out.println(parmn[n]);
	  		}*/
      } else {
        parm[n] = -1;
        uncover++;
      }
    }
    coverage = (double) (nprm - uncover) / nprm;
  }

  public void checkNormalization() {
  }

  double coverage = 0.0;

  public void finaloutput(int nprm, int dataNumber, int niter) {
    double wss;

    if (nprm > 0) {
	    fittingFunction.getFit();
      wss = fittingFunction.getWSS();
//		  double sig = Math.sqrt(wss / (dataNumber - nprm));
//      printf("sig= ", sig);
      double Rw = fittingFunction.getRw();
      double R = fittingFunction.getR();
      double ss = fittingFunction.getSS();
      if (Rw == 0.0)
        Rw = 1.0;
      if (R == 0.0)
        R = 1.0;
      double rsw = Rw * 100.0;
      printf("Rw (%) = ", rsw);
      double rb = R * 100.0;
      printf("Rb (%) = ", rb);
//      printf("Coverage (%) = ", coverage * 100.0);
//      double rexp = Rw / sig;
//      fittingFunction.setRexp(rexp);
//      printf("Rexp (%) = ", rexp * 100.0);
      printf("# iterations = ", niter);
//  		fittingFunction.saveparameters();
    }
//  	niter += iter;
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

  public void printf(String astring) {
    if (outputframe != null) {
      outputframe.appendnewline(astring);
    } else
      System.out.println(astring);
    try {
      Thread.sleep(10);
    } catch (InterruptedException e) {
    }
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

  public void printf(String astring, float value) {
    printf(astring + Fmt.format(value));
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

  public void printf(String astring, int value1, int value2, double value3) {
    printf(astring + Integer.toString(value1) + " " + Integer.toString(value2) +
            " " + Fmt.format(value3));
  }

}
