/*
 * @(#)LeastSquareFit.java created 10/05/2006 Casalino
 *
 * Copyright (c) 1997-2006 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.interfaces.SimpleFunction;
import it.unitn.ing.rista.util.Misc;

/**
 * The MarqardLeastSquares is a class
 *
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:04 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class MarqardLeastSquares {

  int niter = 0;
  int ipflg = 0;
  int brkflg = 0;
  double derstep = 0.001;
  boolean doubleder = false;
  double prcsn = 0.00000001;
  int n0 = 0;
  int flg;
  double am[] = null;
  double g[] = null;
  int[] choleskyFlag = null;

/* Memory allocation */

  double s_deriv[] = null;
  double s_b[] = null;
  double s_grad[] = null;
  double s_c[] = null;
  double s_parmn[] = null;
  double[][] s_derivf = null;
  double s_firstfit[] = null;
/*   end memory allocation  */

  boolean initialized = false;
  public boolean outputEnabled = false;
  SimpleFunction simpleFittingFunction;
  int numberOfIterations = 0;
  public double wssLimit = 0.0;

  public MarqardLeastSquares(SimpleFunction fitFunction, int iterations) {
    this(fitFunction, iterations, false);
  }

  public MarqardLeastSquares(SimpleFunction fitFunction, int iterations, boolean outputenabled) {
//    super(fitFunction, iterations);
    simpleFittingFunction = fitFunction;
    numberOfIterations = iterations;
    outputEnabled = outputenabled;
  }

  public void setIterations(int number) {
    numberOfIterations = number;
  }

  public int getIterations() {
    return numberOfIterations;
  }

  public void setDerivateStep(double value) {
    if (value != 0) {
      derstep = value;
    }
  }

  public void setPrecision(double value) {
    if (value != 0.0) {
      prcsn = value;
    }
  }

  public double getWSS(double[] dta, double[] fit, double[] wgt) {
    double wss = 0.0;
    for (int i = 0; i < dta.length; i++) {
      double diff = dta[i] - fit[i];
      wss += diff * diff * wgt[i] * wgt[i];
    }
    return wss;
  }

  public double simpleSolve(double[] dta, double[] wgt, double[] fit, double[] parm, boolean releaseMemory,
                            int[] controls) {
//    updateStringtoDoubleBuffering();
    int dataNumber = dta.length;
    int nprm = parm.length;
    if (nprm > dataNumber) {
      System.out.println("Number of parameters > number of points! Not able to solve, terminated.");
      return 0.0;
    }
    choleskyFlag = new int[nprm];
    int mdi = (nprm + 1) * nprm / 2;
    if (!initialized || (s_parmn.length != nprm || s_firstfit.length != dataNumber))
      initializeMatrices(dataNumber, nprm);
    simpleFittingFunction.refreshFit(fit, parm, controls);
    double wss = getWSS(dta, fit, wgt);

    for (int i = 0; i < nprm; i++)
      s_b[i] = parm[i];

// start least squares fitting

    int conver = 0;
    double lambda = 0.01;
    double lmin = 1.0e+20;
    double phi = 1.0;
    niter = -1;
    double oldwss = wss;
    int check = 0; // dont permit bigger wss

    while ((niter + 1 < getIterations()) && conver == 0) {
      ++niter;
      if ((niter > 4) && (lambda < lmin))
        lmin = lambda;

//           decrease lambda
      lambda *= 0.31622777;
      computeDerivativeMatrix(dataNumber, nprm, parm, s_derivf, fit, controls);

      for (int i = 0; i < mdi; i++)
        am[i] = 0.0;
      for (int i = 0; i < nprm; i++)
        s_grad[i] = 0.0;

      for (int i = 0; i < dataNumber; i++) {
        double fmm = (fit[i] - dta[i]) * wgt[i];
        for (int sp = 0; sp < nprm; sp++) {
          s_grad[sp] += s_derivf[i][sp] * fmm;
          int l = (sp + 1) * sp / 2;
          for (int kcs = 0; kcs <= sp; kcs++)
            am[l + kcs] += s_derivf[i][sp] * s_derivf[i][kcs] * wgt[i];
        }
      }
//           save "a" matrix and current parameter values "b"
      for (int i = 0; i < mdi; i++)
        s_c[i] = am[i];
      for (int i = 0; i < nprm; i++)
        s_deriv[i] = s_b[i];
//          doctor "a" matrix diagonal elements to be:
//             a=a(1+lambda)+phi*lambda
      do {
        flg = 1;
        while (flg != 0 && (conver == 0)) {
          double da = phi * lambda;
          for (int j = 0; j < nprm; j++) {
            g[j] = -s_grad[j];
            int l1 = (j + 1) * (j + 2) / 2 - 1;
            am[l1] = s_c[l1] * (1.0 + lambda) + da;
            for (int i = 0; i < j; i++)
              am[l1 - i - 1] = s_c[l1 - i - 1];
          }
          flg = chodec(nprm); //    choleski decomposition
          if (flg != 0) {
            if (lambda < prcsn)
              lambda = prcsn;
            lambda *= 10.0;
            if (lambda > 100000.0 * lmin)
              conver = 1;
          }
        }
        if (conver == 0) {
          choback(nprm);
//         find new parameters b=d+g
//        (no counts the  of zero elements in g)
          n0 = 0;
          for (int i = 0; i < nprm; i++) {
            s_b[i] = s_deriv[i] + g[i];
            if (Math.abs(g[i]) <= Math.abs(prcsn * s_deriv[i]))
              ++n0;
            s_parmn[i] = (double) s_b[i];
          }
        }
        if (n0 == nprm)
          conver = 1;
        else {
          simpleFittingFunction.refreshFit(fit, s_parmn, controls);
          wss = getWSS(dta, fit, wgt);
/*          if (Double.toXRDcatString(wss).equalsIgnoreCase("NaN")) {
            wss = oldwss * 1.01;
            printf("Wrong parameters, recomputing...");
          } */
          if (outputEnabled) {
            System.out.println("new Wss = " + wss);
            System.out.println("actual iteration " + niter);
            for (int i = 0; i < dataNumber; i++)
              System.out.println(dta[i] + " " + fit[i]);
          }
          if (wss < oldwss && wss >= 0.0) {
            oldwss = wss;
            for (int i = 0; i < nprm; i++)
              parm[i] = s_parmn[i];
          } else {
            if (check != 0) {
              oldwss = wss;
              wss = oldwss * 1.1;
            }
            if (lambda < prcsn)
              lambda = prcsn;
            lambda *= 10.0;
            if (lambda > 100000.0 * lmin)
              conver = 1;
          }
          if (wss < wssLimit)
            conver = 1;
        }
      } while ((wss > oldwss) && (conver == 0));
      oldwss = wss;
      if (conver == 0) {
        for (int i = 0; i < nprm; i++) {
          s_parmn[i] = parm[i];
          s_b[i] = parm[i];
        }
      }
    }       //   next iteration
    if (outputEnabled) {
      System.out.println("Wss = " + wss);
      System.out.println("Iterations " + niter);
      for (int i = 0; i < dataNumber; i++)
        System.out.println(dta[i] + " " + fit[i]);
    }
    if (releaseMemory)
      releaseMemory();
    return wss;
  }

  public void computeDerivativeMatrix(int dataNumber, int nprm, double parm[],
                                      double derivf[][], double fit[], int[] controls) {
    //        compute matrix of derivative

    double dparp;
//    double secondfit[] = new double[dataNumber];

    for (int sp = 0; sp < nprm; sp++) {
      if (parm[sp] == 0)
        dparp = (double) derstep;
      else
        dparp = parm[sp] * (double) derstep;
//      double dparp2 = dparp * 2.0f;
      double oldpar = parm[sp];
      parm[sp] += dparp;
//      for (int i = 0; i < dataNumber; i++)
        simpleFittingFunction.refreshFit(s_firstfit, parm, controls);
/*      if (doubleder) {
        parm[sp] = oldpar - dparp;
        simpleFittingFunction.getFit(secondfit, parm, controls);
      }     */

      for (int i = 0; i < dataNumber; i++) {
//        if (doubleder)
//          derivf[i][sp] = ((firstfit[i] - secondfit[i]) / dparp2);
//        else
        derivf[i][sp] = ((s_firstfit[i] - fit[i]) / dparp);
      }
      parm[sp] = oldpar;
    }
  }

  public void choback(int nfit1)

/*        subroutine obtains cholesky backward solution of matrix a   */ {
    int k1, i, j, l1, mdi1;

    g[0] /= am[0];
    l1 = 0;
    for (i = 1; i < nfit1; i++) {
      for (j = 0; j < i; j++)
        g[i] -= am[++l1] * g[j];
      if (am[++l1] == 0.0)
        am[l1] = 1.0;
      g[i] /= am[l1];
    }
    mdi1 = nfit1 * (nfit1 + 1) / 2 - 1;
    g[nfit1 - 1] /= am[mdi1];
    for (k1 = 1; k1 < nfit1; k1++) {
      i = nfit1 - k1;
      l1 = i * (i + 1) / 2 - 1;
      for (j = 0; j < i; j++)
        g[j] -= g[i] * am[l1 + j + 1];
      g[i - 1] /= am[l1];
    }
  }

  public int chodec(int nfit1) {
/*        subroutine to perform cholesky decomposition        */

    int i, j, k1, l1, k2, k;
    double f;

    flg = 0;
    for (j = 0; j < nfit1; j++) {
      l1 = (j + 2) * (j + 1) / 2 - 1;
      if (j > 0) {
        for (i = j; i < nfit1; i++) {
          k1 = i * (i + 1) / 2 + j;
          f = am[k1];
          for (k = 0; k < j; k++)
            f -= am[k1 - k - 1] * am[l1 - k - 1];
          am[k1] = f;
        }
      }
      if (am[l1] > 0) {
        f = Math.sqrt(am[l1]);
        for (i = j; i < nfit1; i++) {
          k2 = i * (i + 1) / 2 + j;
          am[k2] /= f;
        }
        choleskyFlag[j] = 1;
      } else {
        flg = 1;
        choleskyFlag[j] = -1;
        if (outputEnabled)
          System.out.println("cholesky negative diag j,l,a(l) : " + j + " " + l1 + " " + am[l1]);
      }
    }
    return flg;
  }

  public void initializeMatrices(int dataNumber, int nprm) {

    int mdi = (nprm + 1) * nprm / 2;

/* Memory allocation */

    s_deriv = new double[nprm];
    s_b = new double[nprm];
    s_grad = new double[nprm];
    s_c = new double[mdi];
    s_parmn = new double[nprm];
    s_derivf = new double[dataNumber][nprm];
    am = new double[mdi];
    g = new double[nprm];
    s_firstfit = new double[dataNumber];
    initialized = true;
/*   end memory allocation  */

  }

  public void releaseMemory() {
    s_deriv = null;
    s_b = null;
    s_grad = null;
    s_c = null;
    s_parmn = null;
    am = null;
    g = null;
    s_firstfit = null;
    s_derivf = null;
    initialized = false;
  }

}
