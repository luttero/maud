/*
 * @(#)HybridMonteCarloRefinement.java created Jan 14, 2012 Caen
 *
 * Copyright (c) 2012 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.diffr.FilePar;
import it.unitn.ing.rista.interfaces.Function;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.JOptionsDialog;

import javax.swing.*;
import javax.swing.event.ChangeListener;
import javax.swing.event.ChangeEvent;
import java.util.Vector;
import java.util.Comparator;
import java.awt.*;

/**
 * The HybridMonteCarloRefinement is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jan 14, 2012 3:12:52 PM $
 * @since JDK1.1
 */
public class HybridMonteCarloRefinement extends OptimizationAlgorithm {

  public static String[] diclistc = {"_riet_montecarlo_trials_number",
      "_refine_ls_number_iteration", "_riet_refine_ls_precision",
      "_riet_refine_ls_derivative_step", "_riet_refine_ls_double_derivative"
  };
  public static String[] diclistcrm = {"Number of Montecarlo trials",
      "_refine_ls_number_iteration", "_riet_refine_ls_precision",
      "_riet_refine_ls_derivative_step", "_riet_refine_ls_double_derivative"};
  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  double[] defParams = null;
  double[] bestParams = null;
  double defWSS = 0.0;
  double bestWSS = 0.0;

  ec.util.MersenneTwisterFast randomizer = null;

  // Least squares
  int niter = 0;
  int ipflg = 0;
  int brkflg = 0;
  double derstep = 0.001;
  boolean doubleder = false;
  double prcsn = 0.00000001;
  int iterationsLeastSquares = 5;
  int n0 = 0;
  int flg;
  double am[] = null;
  double g[] = null;
  int[] choleskyFlag = null;
  double dta[] = null;
  double wgt[] = null;
  double fit[] = null;

  public boolean simplifyForm = false;

  public double wssLimit = 0.0;

  public HybridMonteCarloRefinement(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Lamarckian MonteCarlo Refinement";
    IDlabel = "Lamarckian MonteCarlo Refinement";
    description = "select this to use a Lamarckian MonteCarlo approach";
  }

  public HybridMonteCarloRefinement(XRDcat aobj) {
    this(aobj, "Lamarckian MonteCarlo Refinement");
  }

  public HybridMonteCarloRefinement() {
    identifier = "Lamarckian MonteCarlo Refinement";
    IDlabel = "Lamarckian MonteCarlo Refinement";
    description = "select this to use a Lamarckian MonteCarlo approach";
  }

  public void initConstant() {
    Nstring = 5;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    for (int i = 0; i < totsubordinateloop; i++)
      diclistRealMeaning[i] = diclistcrm[i];
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
  }

  public void initParameters() {
    super.initParameters();
    setTrialsNumber("100");
    // Least squares
    stringField[1] = MaudPreferences.getPref(iterations, "3");
    stringField[3] = MaudPreferences.getPref("leastSquares.derivateStep", "0.001");
    stringField[4] = MaudPreferences.getPref("leastSquares.doubleDerivate", "false");
    stringField[2] = MaudPreferences.getPref("leastSquares.precision", "0.00000001");
  }

  public void setTrialsNumber(String value) {
//    System.out.println("Set trials:" + value) ;
    stringField[0] = value;
  }

  public String getTrialsNumber() {
    return stringField[0];
  }

  public int getIntTrialsNumber() {
    return Integer.parseInt(getTrialsNumber());
  }

  int actualThread = 0;
  Vector results = null;
  int divideRange = 1;
  int[] divideValue = null;
  String filesBase64;
  int startIndex = 0;
  boolean started = false;

/*  public void solveXGRID(launchBasic computation, Function funtionToMinimize) {
    if (funtionToMinimize instanceof FilePar) {
      fittingFunction = funtionToMinimize;
      int iterations = getIntNumberOfPopulations();
      setNumberOfPopulations(1); // temporarly
      fittingFunction.prepareIteration();
      nprm = fittingFunction.getNumberOfFreeParameters();

      divideRange = ((FilePar) fittingFunction).checkForParRangeToDivide();
      filesBase64 = ParallelComputationController.maudEssentialBase64;
      final String[] filesBase64ToSubmit = new String[2];
      if (divideRange == 1) {
        filesBase64ToSubmit[0] = filesBase64;
        filesBase64ToSubmit[1] = ((FilePar) fittingFunction).getSavedFileAsBase64String();
      } else {
        iterations = divideRange;
      }

      initAll(computation);
      if (outputframe != null)
        computation.hideIterationPanel();
      if (outputframe != null) {
        outputframe.getProgressBar().setProgressBarValue(iterations);
//      computation.hideIterationPanel();
      }

      defParams = new double[nprm];
      lbound = new double[nprm];
      ubound = new double[nprm];
      divideValue = ((FilePar) fittingFunction).getRangeDivision();
      for (int i = 0; i < nprm; i++) {
        defParams[i] = fittingFunction.getFreeParameter(i);
        lbound[i] = fittingFunction.getLowerBound(i);
        ubound[i] = fittingFunction.getUpperBound(i);
//        printf("Parameter, min, max : ", defParams[i], lbound[i], ubound[i]);
      }

      printf("Number of populations : ", iterations);

      actualThread = 0;
      results = new Vector(0, iterations);
      if (divideRange == 1) {
        for (int i = 0; i < iterations; i++) {
          (new PersistentThread() {
            public void executeJob() {
              actualThread++;
              boolean success = false;
              while (!success) {
                String resultData = XGridClient.submitJobAndWait("Maud_analysis",
                    ParallelComputationController.xgridFilenames, filesBase64ToSubmit, ParallelComputationController.javaCommand,
                    ParallelComputationController.javaArguments);
                if (!resultData.equals(Client.CANCELED) && !resultData.equals(Client.FAILED)) {
                  success = true;
                  StringTokenizer st = new StringTokenizer(resultData, " {}=,;:'\t\r\n");
                  String token = st.nextToken();
                  while (!token.equalsIgnoreCase("XGrid") && st.hasMoreTokens())
                    token = st.nextToken();
                  if (st.hasMoreTokens()) {
                    token = st.nextToken(); // solution:
                    token = st.nextToken();
                    double Rwp = Double.parseDouble(token);
                    int i = 0;
                    double[] newparameters = new double[nprm];
                    while (st.hasMoreTokens()) {
                      token = st.nextToken();
                      newparameters[i++] = Float.parseFloat(token);
                    }
                    Result result = new Result(Rwp, newparameters);
                    results.add(result);

                    if (outputframe != null) {
                      printf("Rwp = ", Rwp);
                      printout(newparameters, newparameters.length);
                      outputframe.increaseProgressBarValue();
                    }
                  }
                } else {
                  try {
                    Thread.sleep(10000);
                  } catch (InterruptedException ie) {
                    ie.printStackTrace(System.err);
                  }
                }
              }

              actualThread--;
            }
          }).start();
          try {
            Thread.sleep(500);
          } catch (InterruptedException ie) {
          }
        }
      } else {
        double[] parameters = new double[nprm];
        double[] lBounds = new double[nprm];
        double[] uBounds = new double[nprm];
        for (int i = 0; i < nprm; i++) {
          parameters[i] = (double) defParams[i];
          lBounds[i] = lbound[i];
          uBounds[i] = ubound[i];
//        printf("Parameter, min, max : ", defParams[i], lbound[i], ubound[i]);
        }
        int boundsIndex = 0;
        int divideIndex = 0;
        startIndex = 0;
        startRecursiveSubmission(parameters, lBounds, uBounds, boundsIndex, divideIndex);
      }
      while (actualThread > 0) {
        try {
          Thread.sleep(500);
        } catch (InterruptedException ie) {
        }
      }

      Collections.sort(results, new bestSolution());
      Result bestResult = (Result) results.firstElement();
      if (outputframe != null) {
        printf("Best Rwp = ", bestResult.Rwp);
        printout(bestResult.parameters, bestResult.parameters.length);
      }
      fittingFunction.setFreeParameters(bestResult.parameters);
      if (divideRange > 1) {
        ((FilePar) fittingFunction).setParametersAndBounds(bestResult.parameters, lbound, ubound);
      }
      fittingFunction.computeFirstFit();
      setNumberOfPopulations(iterations); // back to the correct value
      fittingFunction.setDerivate(false);
    } else
      solve(computation, fittingFunction);
  }

  public void startRecursiveSubmission(final double[] parameters, final double[] lBounds, final double[] uBounds,
                                       final int boundsIndex, final int divideIndex) {
//    for (boundsIndex = 0; boundsIndex < nprm; boundsIndex++)
//      for (divideIndex = 0; divideIndex < divideValue[boundsIndex]; divideIndex++) {
//            System.out.println(startIndex);
    lBounds[boundsIndex] = lbound[boundsIndex] + (ubound[boundsIndex] - lbound[boundsIndex])
        * divideIndex / divideValue[boundsIndex];
    uBounds[boundsIndex] = lbound[boundsIndex] + (ubound[boundsIndex] - lbound[boundsIndex])
        * (divideIndex + 1) / divideValue[boundsIndex];
    parameters[boundsIndex] = (double) ((uBounds[boundsIndex] + lBounds[boundsIndex]) / 2.0);
//    System.out.println(boundsIndex + " " + lbound[boundsIndex] + " " + ubound[boundsIndex] + " " + divideValue[boundsIndex] + " "
//            + divideIndex);
//    System.out.println(boundsIndex + " " + lBounds[boundsIndex] + " " + uBounds[boundsIndex] + " " + parameters[boundsIndex]);
    if (boundsIndex < nprm - 1) {
      int newBoundsIndex = boundsIndex + 1;
      if (newBoundsIndex < nprm) {
        for (int i = 0; i < divideValue[newBoundsIndex]; i++) {
          startRecursiveSubmission(parameters, lBounds, uBounds, newBoundsIndex, i);
        }
      }
      return;
    }
    started = false;
    (new PersistentThread() {
      public void executeJob() {
        actualThread++;
        String indexName = Integer.toString(startIndex++);
        String popName = "Maud_population_" + indexName;
        String resultData = null;
        ((FilePar) fittingFunction).setParametersAndBounds(parameters, lBounds, uBounds);

        String[] filesBase64ToSubmit = new String[2];
        filesBase64ToSubmit[0] = filesBase64;
        filesBase64ToSubmit[1] = ((FilePar) fittingFunction).getSavedFileAsBase64String();
        boolean first = true;
        boolean success = false;
        while (!success) {
          System.out.println("Submitting: " + popName);
          String clientName = "XGridClient_" + indexName;
          String jobId = XGridClient.submitJob(clientName,
              popName,
              ParallelComputationController.xgridFilenames, filesBase64ToSubmit, ParallelComputationController.javaCommand,
              ParallelComputationController.javaArguments);
          // we wait a little before starting retrieve the data
          if (!jobId.equals(Client.FAILED)) {
            if (first) {
              started = true;
              first = false;
            }
            while (!ParallelComputationController.retrieveData) {
              try {
                Thread.sleep(1000);
              } catch (InterruptedException ie) {
                ie.printStackTrace(System.err);
              }
            }
            resultData = XGridClient.getResults(clientName, jobId);
            System.out.println(resultData);
            if (!resultData.equals(Client.CANCELED) && !resultData.equals(Client.FAILED)) {
              success = true;
              StringTokenizer st = new StringTokenizer(resultData, " {}=,;'\t\r\n");
              String token = st.nextToken();
              while (!token.equalsIgnoreCase("XGrid") && st.hasMoreTokens())
                token = st.nextToken();
              if (st.hasMoreTokens()) {
                token = st.nextToken(); // solution:
                token = st.nextToken();
                double Rwp = Double.parseDouble(token);
                int i = 0;
                double[] newparameters = new double[nprm];
                while (st.hasMoreTokens()) {
                  token = st.nextToken();
                  newparameters[i++] = Float.parseFloat(token);
                }
                Result result = new Result(Rwp, newparameters);
                results.add(result);
                if (outputframe != null) {
                  printf("Rwp = ", Rwp);
                  printout(newparameters, nprm);
                  outputframe.increaseProgressBarValue();
                }
              }
            } else {
                try {
                  Thread.sleep(10000);
                } catch (InterruptedException ie) {
                  ie.printStackTrace(System.err);
                }
            }
          } else {
            try {
              Thread.sleep(10000);
            } catch (InterruptedException ie) {
              ie.printStackTrace(System.err);
            }
          }
        }

        actualThread--;
      }
    }).start();
    while (!started) {
      try {
        Thread.sleep(500);
      } catch (InterruptedException ie) {
        ie.printStackTrace(System.err);
      }
    }
//      }
  }*/

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
//    super.setIterations(Integer.parseInt(stringField[6]));
//    MaudPreferences.setPref(iterations, stringField[1]);
    derstep = Double.parseDouble(stringField[3]);
    prcsn = Double.parseDouble(stringField[2]);
    doubleder = isDerivative2();
    iterationsLeastSquares = Integer.parseInt(stringField[1]);
  }

  public void setDerivateStep(double value) {
    if (value != 0) {
      derstep = value;
      stringField[8] = Double.toString(value);
    }
  }

  public void setPrecision(double value) {
    if (value != 0.0) {
      prcsn = value;
      stringField[2] = Double.toString(value);
    }
  }

  public void setDerivative2(boolean value) {
    doubleder = value;
    if (value)
      stringField[4] = "true";
    else
      stringField[4] = "false";
  }

  public boolean isDerivative2() {
    if (stringField[4].equalsIgnoreCase("true"))
      return true;
    return false;
  }

  public void setIterationsLeastSquares(int number) {
    super.setIterations(number);
    stringField[1] = Integer.toString(number);
    iterationsLeastSquares = Integer.parseInt(stringField[1]);
  }

  public int getIterationsLeastSquares() {
    return iterationsLeastSquares;
  }

  private class Result {
    public double Rwp = 1.0E33;
    public double[] parameters = null;

    public Result(double rwp, double[] pars) {
      Rwp = rwp;
      parameters = pars;
    }
  }

  class bestSolution implements Comparator {
    public int compare(Object obj1, Object obj2) {
      double Rwp1 = ((Result) obj1).Rwp;
      double Rwp2 = ((Result) obj2).Rwp;

      if (Rwp2 == Rwp1) {
        return 0;
      } else if (Rwp1 < Rwp2)
        return -1;
      return 1;
    }
  }

  public void solve(launchBasic computation, Function funtionTominimize) {

    setIterations(getIntTrialsNumber());

    fittingFunction = funtionTominimize;

    int iterat = getIterations();
    if (outputframe != null)
      computation.setIterationSliderValue(iterat);

    printf("Number of trials : ", iterat);

//  Init the randomizer for random number generation
    initAll(computation);

//  Generate the starting solution configuration
    generateStartingSolutions();

    for (int i = 0; i < iterat; i++) {
      if (i > getIterations()) {
        printf("Iterations stopped!");
        break;
      }

      if (computation != null && computation.shouldStop()) {
        endOfIterations();
        fittingFunction.setDerivate(false);
        return;
      }
      computeSolution();
    }
    endOfIterations();
    fittingFunction.setDerivate(false);
    am = null;
     g = null;
    choleskyFlag = null;
     dta = null;
     wgt = null;
     fit = null;
  }

  int dataNumber = 0;
/*  double dta[] = null;
  double wgt[] = null;
  double fit[] = null; */
  int nprm = 0;
  double lbound[] = null;
  double ubound[] = null;

  void initAll(launchBasic computation) {

    // getFilePar().prepareComputation();
    fittingFunction.prepareIteration();
    if (outputframe != null) {
      outputframe.getProgressBar().setProgressBarValue(getIntTrialsNumber());
//      computation.hideIterationPanel();
    }

    dataNumber = fittingFunction.getNumberOfData();
    fittingFunction.computeFirstFit();
    fittingFunction.getFit();
    dta = new double[dataNumber];
    wgt = new double[dataNumber];
    fit = new double[dataNumber];
    for (int i = 0; i < dataNumber; i++) {
      dta[i] = fittingFunction.getData(i);
      wgt[i] = fittingFunction.getWeight(i);
      wgt[i] *= wgt[i];
      fit[i] = fittingFunction.getFit(i);
    }


/*    dta = new double[dataNumber];
    wgt = new double[dataNumber];
    fit = new double[dataNumber]; */

    if (computation != null && computation.shouldStop()) {
      return;
    }

/*    for (int i = 0; i < dataNumber; i++) {
      dta[i] = fittingFunction.getData(i);
      wgt[i] = fittingFunction.getWeight(i);
      wgt[i] *= wgt[i];
      fit[i] = fittingFunction.getFit(i);
    } */
    defWSS = fittingFunction.getWSS();
    nprm = fittingFunction.getNumberOfFreeParameters();
    defParams = new double[nprm];
    lbound = new double[nprm];
    ubound = new double[nprm];
    for (int i = 0; i < nprm; i++) {
      defParams[i] = fittingFunction.getFreeParameter(i);
      lbound[i] = fittingFunction.getLowerBound(i);
      ubound[i] = fittingFunction.getUpperBound(i);
//      System.out.println("Parameter, min, max : " + defParams[i] + " " + lbound[i] + " " + ubound[i]);
      printf("Parameter, min, max : ", defParams[i], lbound[i], ubound[i]);
    }

    printf("Wss = ", defWSS);
    if (computation != null && computation.shouldStop()) {
      return;
    }

    if (computation != null && computation.shouldStop()) {
      return;
    }

//    fittingFunction.setDerivate(true);
    bestParams = new double[defParams.length];
    bestWSS = 1.0E50;
    for (int i = 0; i < bestParams.length; i++)
      bestParams[i] = defParams[i];

    int time = (int) System.currentTimeMillis();  // safe because we're getting low-order bits
    randomizer = new ec.util.MersenneTwisterFast(time);

    updateStringtoDoubleBuffering(false);

  }

  double randomGenerator() {
    double random = randomizer.nextDouble();
//    while (random == 1.0)  // escluding 1.0
//      random = randomizer.nextDouble();
    return random;
  }

  /**
   * return value between min and max excluded
   *
   * @param min
   * @param max
   * @return
   */
  double randomGenerator(double min, double max) {
    return min + (max - min) * randomGenerator();
  }


  void generateStartingSolutions() {
  }

  void computeSolution() {
    double[] pars = new double[nprm];
    for (int i = 0; i < nprm; i++) {
      pars[i] = randomGenerator(lbound[i], ubound[i]);
    }
    double wss = getFitness(pars);
  }

  void endOfIterations() {
    if (bestWSS < defWSS) {
      fittingFunction.setFreeParameters(bestParams);
      fittingFunction.saveparameters();
      double wss = fittingFunction.getWSS();
      if (fittingFunction instanceof FilePar)
        ((FilePar) fittingFunction).updatePlot();
      System.out.println("Final chi :" + wss);
    } else {
      fittingFunction.setFreeParameters(defParams);
      fittingFunction.saveparameters();
      double wss = fittingFunction.getWSS();
      if (fittingFunction instanceof FilePar)
        ((FilePar) fittingFunction).updatePlot();
      System.out.println("Final chi :" + wss);
    }
//    structureFactorList = null;
    fittingFunction.computeFirstFit();
    fittingFunction.getRefinementIndexes();
    defParams = null;
    bestParams = null;
  }

  public double getFitness(double[] params) {
    fittingFunction.setFreeParameters(params);
    double wss = solveLeastSquares();
    for (int i = 0; i < params.length; i++)
      params[i] = fittingFunction.getFreeParameter(i);
    if (outputframe != null)
      outputframe.increaseProgressBarValue();

    if (wss < bestWSS) {
      bestWSS = wss;
      printf("Parameters values:");
      printout(params, nprm);
      printf("Actual best wss :", wss);
      for (int i = 0; i < bestParams.length; i++) {
        bestParams[i] = params[i];
      }
      if (fittingFunction instanceof FilePar)
        ((FilePar) fittingFunction).updatePlot();
    }
    return wss;
  }

  // Least squares

  public double solveLeastSquares() {

//    fittingFunction = funtionTominimize;

    int dataNumber = fittingFunction.getNumberOfData();

    fittingFunction.setDerivate(false);

    fittingFunction.computeFirstFit();

      fittingFunction.getFit();
      for (int i = 0; i < dataNumber; i++) {
        fit[i] = fittingFunction.getFit(i);
      }

    int nprm = fittingFunction.getNumberOfFreeParameters();
    double parm[] = new double[nprm];
    double[] minSignificantValue = new double[nprm];

    choleskyFlag = new int[nprm];
    for (int i = 0; i < nprm; i++) {
      parm[i] = fittingFunction.getFreeParameter(i);
      minSignificantValue[i] = fittingFunction.getParameterMinSignificantValue(i);
    }
    double parmn[] = new double[nprm];

    int check = 0; // if check != 0 the new wss can be sligthly greater than the old one

    fittingFunction.setDerivate(true);
    int mdi = (nprm + 1) * nprm / 2;

/* Memory allocation */

    final double deriv[] = new double[nprm];
    final double b[] = new double[nprm];
    final double grad[] = new double[nprm];
    final double c[] = new double[mdi];

    am = new double[mdi];
    g = new double[nprm];

    double[][] derivf = new double[dataNumber][nprm];

/*   end memory allocation  */

    for (int i = 0; i < nprm; i++)
      b[i] = parm[i];

// start least squares fitting

    int conver = 0;
    double lambda = 0.01;
    double lmin = 1.0e+20;
    double phi = 1.0;
    niter = -1;


//	  Assert.assert(derivf);


    double wss = 0.0;
      wss = fittingFunction.getWSS();
    double oldwss = wss;

/*          next iteration      */

    boolean flagfgl = (conver == 0);
    while ((niter + 1 < getIterationsLeastSquares()) && flagfgl) {
      ++niter;
      if ((niter > 4) && (lambda < lmin))
        lmin = lambda;
//           decrease lambda
      lambda *= 0.31622777;

      computeDerivativeMatrix(dataNumber, nprm, parm, derivf, fit, minSignificantValue);

      for (int i = 0; i < mdi; i++)
        am[i] = 0.0;
      for (int i = 0; i < nprm; i++)
        grad[i] = 0.0;

        for (int i = 0; i < dataNumber; i++) {
          double fmm = (fit[i] - dta[i]) * wgt[i];
          for (int sp = 0; sp < nprm; sp++) {
            grad[sp] += derivf[i][sp] * fmm;
            int l = (sp + 1) * sp / 2;
            for (int kcs = 0; kcs <= sp; kcs++)
              am[l + kcs] += derivf[i][sp] * derivf[i][kcs] * wgt[i];
          }
        }
//           save "a" matrix and current parameter values "b"
      for (int i = 0; i < mdi; i++)
        c[i] = am[i];
      for (int i = 0; i < nprm; i++)
        deriv[i] = b[i];
      do {
        flg = 1;
        while (flg != 0 && (conver == 0)) {
          double da = phi * lambda;
          for (int j = 0; j < nprm; j++) {
            g[j] = -grad[j];
            int l1 = (j + 1) * (j + 2) / 2 - 1;
            am[l1] = c[l1] * (1.0 + lambda) + da;
            for (int i = 0; i < j; i++)
              am[l1 - i - 1] = c[l1 - i - 1];
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
            b[i] = deriv[i] + g[i];
            if (Math.abs(g[i]) <= Math.abs(prcsn * deriv[i]))
              ++n0;
            parmn[i] = (double) b[i];
          }
//          printout(parmn, nprm);
        }
        if (n0 == nprm)
          conver = 1;
        else {
          boolean bounds = false;
          for (int j = 0; j < nprm; j++)
            bounds = (bounds || fittingFunction.checkBound(j, parmn[j]));
          if (bounds) {
            wss = oldwss * 1.01;
//        		printf("At least one parameter out of bounds, recomputing...");
          } else {
            if (fittingFunction.singleFunctionComputing())
              fittingFunction.setDerivate(false);
            fittingFunction.setFreeParameters(parmn);
              for (int i = 0; i < dataNumber; i++)
                fit[i] = fittingFunction.getFit(i);
              wss = fittingFunction.getWSS();
            if (fittingFunction.singleFunctionComputing())
              fittingFunction.setDerivate(true);
          }
          if (Double.isNaN(wss) || wss == 0.0) {
            wss = oldwss * 1.01;
//            printf("Wrong parameters, recomputing...");
          }
          if (wss < oldwss && wss > 0.0) {
            oldwss = wss;
          } else {
            if (check != 0) {
              oldwss = wss;
              wss = oldwss * 1.01;
            }
            if (lambda < prcsn)
              lambda = prcsn;
            lambda *= 10.0;
            if (lambda > 100000.0 * lmin) {
              conver = 1;
            }
          }
        }
      } while ((wss > oldwss) && (conver == 0));
      if (wss > oldwss) {
//        printf("No solution found, setting old values...");
        fittingFunction.setFreeParameters(parm);
      }

      fittingFunction.setDerivate(false);

      // todo : this is only a hack to speed up, but not safe, uncomment it for safety
      if (!fittingFunction.singleFunctionComputing()) {
          fittingFunction.computeFirstFit();
          double[] newFit = fittingFunction.getFit();
          for (int i = 0; i < dataNumber; i++)
            fit[i] = fittingFunction.getFit(i);
          wss = fittingFunction.getWSS();
      }
      oldwss = wss;
      fittingFunction.setDerivate(true);
      flagfgl = (conver == 0);
      if (conver == 0) {
        for (int i = 0; i < nprm; i++) {
          parm[i] = fittingFunction.getFreeParameter(i);
          parmn[i] = parm[i];
          b[i] = parm[i];
        }
      }
    }       //   next iteration
    fittingFunction.setDerivate(false);
    if (conver == 0) {
      if (niter + 1 >= getIterationsLeastSquares()) {
        ++niter;
//        printf(niter, ", iterations - pause ");
      } else {
//        printf("operator interrupt");
        brkflg = 0;
      }
    } else {
//             convergen\ufffdce reached
      double ri = lambda / lmin;
      n0 = 0;
    }
    return wss;
  }

  public void computeDerivativeMatrix(int dataNumber, int nprm, double parm[],
                                      double derivf[][], double fit[],
                                      double[] minSignificantValue) {
    //        compute matrix of derivative

    double dparp;
    //

    double firstfit[] = new double[dataNumber];
    double secondfit[] = null;
    if (doubleder)
      secondfit = new double[dataNumber];
    for (int sp = 0; sp < nprm; sp++) {
      if (parm[sp] == 0 && minSignificantValue[sp] == 0)
        dparp = (double) derstep;
      else if (Math.abs(parm[sp]) < Math.abs(minSignificantValue[sp]))
        dparp = (double) (minSignificantValue[sp] * derstep);
      else
        dparp = parm[sp] * (double) derstep;
      double dparp2 = dparp * 2.0f;
      double oldpar = parm[sp];
      double parm1 = parm[sp] + dparp;
      fittingFunction.setFreeParameter(sp, parm1);
        for (int i = 0; i < dataNumber; i++)
          firstfit[i] = fittingFunction.getFit(i);
      if (doubleder) {
        parm1 = oldpar - dparp;
        fittingFunction.setFreeParameter(sp, parm1);
          for (int i = 0; i < dataNumber; i++)
            secondfit[i] = fittingFunction.getFit(i);
      }

        for (int i = 0; i < dataNumber; i++) {
          if (doubleder)
            derivf[i][sp] = ((firstfit[i] - secondfit[i]) / dparp2);
          else
            derivf[i][sp] = ((firstfit[i] - fit[i]) / dparp);
        }
      fittingFunction.setFreeParameter(sp, oldpar);
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
//        if (outputEnabled)
//          printf("cholesky negative diag j,l,a(l) : ", j, l1, am[l1]);
      }
    }
    return flg;
  }


  public JOptionsDialog getOptionsDialog(Frame parent) {
    return new JHMCSDPDOptionsD(parent, this);
  }

  public class JHMCSDPDOptionsD extends JOptionsDialog {

    JTextField[] parsTF = null;
    JComboBox crossTypeCB = null;
    JSlider iterationJS;
    JCheckBox doubleDerivativeCB;

    public JHMCSDPDOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel tfPanel = new JPanel();
      tfPanel.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.CENTER, tfPanel);

      String[] labels = {
          "Number of trials:   ",
          "Iterations number:       ",
          "Parameter precision:     ",
          "Derivative step:         ",
          "Double derivative:       "};

      int numberFields = labels.length;
      parsTF = new JTextField[numberFields];

      for (int i = 0; i < numberFields; i++) {
        if (i == 1) {
          JLabel iterationTF = new JLabel();
          iterationJS = new JSlider();
          iterationJS.setToolTipText("Set the number of iterations during refinement (best values: 3-7)");
          SliderListener listener = new SliderListener(iterationTF);
          iterationJS.addChangeListener(listener);
          tfPanel.add(iterationJS);
          tfPanel.add(iterationTF);
        } else if (i == 4) {
          doubleDerivativeCB = new JCheckBox("Double derivative");
          doubleDerivativeCB.setToolTipText("By selecting numerical double derivative will be used (slower computation)");
          tfPanel.add(doubleDerivativeCB);
        } else {
          tfPanel.add(new JLabel(labels[i]));
          tfPanel.add(parsTF[i] = new JTextField(Constants.FLOAT_FIELD));
        }
      }

      setTitle("Hybrid MonteCarlo refinement options panel");
      initParameters();
      pack();
      iterationJS.setValue(getIterationsLeastSquares());
    }

    public void initParameters() {
      for (int i = 0; i < parsTF.length; i++) {
        if (!(i == 1 || i == 4))
          parsTF[i].setText(stringField[i]);
      }
      int maxIterations = MaudPreferences.getInteger("analysis.maxIterationsSelectable", 21);
      iterationJS.setMaximum(maxIterations);
      iterationJS.setMinimum(1);
      iterationJS.setValue(maxIterations);
      iterationJS.setPaintTicks(false);
      iterationJS.setMajorTickSpacing(5);
      iterationJS.setMinorTickSpacing(1);
      iterationJS.setPaintLabels(false);
      iterationJS.setSnapToTicks(true);
      iterationJS.setLabelTable(iterationJS.createStandardLabels(5));
      doubleDerivativeCB.setSelected(isDerivative2());
    }

    public void retrieveParameters() {
      for (int i = 0; i < parsTF.length; i++) {
        if (!(i == 1 || i == 4))
          stringField[i] = parsTF[i].getText();
      }
      setIterationsLeastSquares(iterationJS.getValue());
      setDerivative2(doubleDerivativeCB.isSelected());
    }

    class SliderListener implements ChangeListener {
      JLabel tf;

      public SliderListener(JLabel f) {
        tf = f;
      }

      public void stateChanged(ChangeEvent e) {
        JSlider s1 = (JSlider) e.getSource();
        tf.setText(Integer.toString(s1.getValue()));
      }
    }
  }
}
