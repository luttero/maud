/*
 * @(#)HybridRefinement.java created Feb 12, 2011 Caen
 *
 * Copyright (c) 2011 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.interfaces.GAProblem;
import it.unitn.ing.rista.interfaces.Function;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.xgridclient.XGridClient;
import it.unitn.ing.xgridclient.Client;
import ec.EvolutionState;

import javax.swing.*;
import javax.swing.event.ChangeListener;
import javax.swing.event.ChangeEvent;
import java.util.*;
import java.awt.*;

/**
 * The HybridRefinement is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Feb 12, 2011 7:43:25 PM $
 * @since JDK1.1
 */
public class HybridRefinement extends OptimizationAlgorithm implements GAProblem {

  public static String[] diclistc = {"_riet_ga_population_size", "_riet_ga_generations_number",
      "_riet_ga_mutation_prob", "_riet_ga_cross_over_probability",
      "_riet_ga_cross_over_type", "_riet_ga_populations_number",
      "_refine_ls_number_iteration", "_riet_refine_ls_precision",
      "_riet_refine_ls_derivative_step", "_riet_refine_ls_double_derivative"
      /*, "_riet_ga_min_max_automatic_%",
     "_riet_ga_cycles_number", "_riet_ga_cycles_decr_factor"*/
  };
  public static String[] diclistcrm = {"_riet_ga_population_size", "_riet_ga_generations_number",
      "_riet_ga_mutation_prob", "_riet_ga_cross_over_probability",
      "_riet_ga_cross_over_type", "_riet_ga_populations_number",
      "_refine_ls_number_iteration", "_riet_refine_ls_precision",
      "_riet_refine_ls_derivative_step", "_riet_refine_ls_double_derivative"
      /*, "_riet_ga_min_max_automatic_%",
     "_riet_ga_cycles_number", "_riet_ga_cycles_decr_factor"*/
  };
  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  EvolutionState state;
  boolean startRandomConfiguration = true;

  static String EVALTHREADS = "evalthreads=";
  static String BREEDTHREADS = "breedthreads=";
  static String GENERATIONS_NUMBER = "generations=";
  static String GENOME_SIZE = "pop.subpop.0.species.genome-size=";
  static String POPULATION_SIZE = "pop.subpop.0.size=";
  static String MUTATION_PROB = "pop.subpop.0.species.mutation-prob=";
  static String DIFF_GENE = "pop.subpop.0.species.diff-gene=";
  public static String MIN_GENE = "pop.subpop.0.species.min-gene.";
  public static String MAX_GENE = "pop.subpop.0.species.max-gene.";
  static String CHUNK_SIZE = "pop.subpop.0.species.chunk-size=";
  static String SEED_NUMBER = "seed.0=";
  static String CROSS_OVER = "pop.subpop.0.species.crossover-type=";
  static String CROSS_OVER_PROBABILITY = "pop.subpop.0.species.crossover-prob=";

  static String[] cross_over_type = {"one", "two", "any"};

  String[] argsToDefine = {
// number of threads
      EVALTHREADS, "1",
      BREEDTHREADS, "1",
// a good random seed for thread 0
      SEED_NUMBER, "4357",
      "state=", "ec.simple.SimpleEvolutionState",
// ec.EvolutionState
      GENERATIONS_NUMBER, "10",
      "pop.subpop.0.species.min-gene=", "0.0",
      "pop.subpop.0.species.max-gene=", "1.0",
      GENOME_SIZE, "100",
      "select.tournament.size=", "2",
      MUTATION_PROB, "0.01",
      CROSS_OVER, "one",
      POPULATION_SIZE, "100",
      DIFF_GENE, "3",
      CHUNK_SIZE, "1",
      CROSS_OVER_PROBABILITY, "0.1"
/*    "pop.subpop.0.species.min-gene_0=", "0.0",
    "pop.subpop.0.species.max-gene_0=", "2000.0",
    "pop.subpop.0.species.min-gene_1=", "0.001",
    "pop.subpop.0.species.max-gene_1=", "0.1",
    "pop.subpop.0.species.min-gene_2=", "0.0",
    "pop.subpop.0.species.max-gene_2=", "50.0"   */
  };

//  StructureFactorList[] structureFactorList = null;
  double[] defParams = null;
  double[] bestParams = null;
  double defWSS = 0.0;
  double bestWSS = 0.0;

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

  public HybridRefinement(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Lamarckian Refinement";
    IDlabel = "Lamarckian Refinement";
    description = "select this to use a Lamarckian evolutionary approach";
  }

  public HybridRefinement(XRDcat aobj) {
    this(aobj, "Lamarckian Refinement");
  }

  public HybridRefinement() {
    identifier = "Lamarckian Refinement";
    IDlabel = "Lamarckian Refinement";
    description = "select this to use a Lamarckian evolutionary approach";
  }

  public void initConstant() {
    Nstring = 10;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
    System.arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
  }

  public void initParameters() {
    super.initParameters();
    setPopulationSize("500");
    setGenerationsNumber("2");
    setMutationProbability("0.01");
    setCrossOverProbability("0.3");
    setCrossOverType(0);
    setNumberOfPopulations(1);
    // Least squares
    stringField[6] = MaudPreferences.getPref(iterations, "3");
    stringField[8] = MaudPreferences.getPref("leastSquares.derivateStep", "0.001");
    stringField[9] = MaudPreferences.getPref("leastSquares.doubleDerivate", "false");
    stringField[7] = MaudPreferences.getPref("leastSquares.precision", "0.00000001");
  }

  public void setNumberOfPopulations(int value) {
    stringField[5] = Integer.toString(value);
  }

  public void setNumberOfPopulations(String value) {
    stringField[5] = value;
  }

  public String getNumberOfPopulations() {
    return stringField[5];
  }

  public int getIntNumberOfPopulations() {
    return Integer.parseInt(getNumberOfPopulations());
  }

  public void setPopulationSize(String value) {
    stringField[0] = value;
  }

  public String getPopulationSize() {
    return stringField[0];
  }

  public int getIntPopulationSize() {
    return Integer.parseInt(getPopulationSize());
  }

  public void setGenerationsNumber(String value) {
    stringField[1] = value;
  }

  public String getGenerationsNumber() {
    return stringField[1];
  }

  public int getIntGenerationsNumber() {
    return Integer.parseInt(getGenerationsNumber());
  }

  public void setMutationProbability(String value) {
    stringField[2] = value;
  }

  public String getMutationProbability() {
    return stringField[2];
  }

  public void setCrossOverProbability(String value) {
    stringField[3] = value;
  }

  public String getCrossOverProbability() {
    return stringField[3];
  }

  public void setCrossOverType(int index) {
    stringField[4] = cross_over_type[index];
  }

  public void setCrossOverType(String value) {
    stringField[4] = value;
  }

  public String getCrossOverType() {
    return stringField[4];
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
//    super.setIterations(Integer.parseInt(stringField[6]));
//    MaudPreferences.setPref(iterations, stringField[6]);
    derstep = Double.parseDouble(stringField[8]);
    prcsn = Double.parseDouble(stringField[7]);
    doubleder = isDerivative2();
    iterationsLeastSquares = Integer.parseInt(stringField[6]);
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
      stringField[7] = Double.toString(value);
    }
  }

  public void setDerivative2(boolean value) {
    doubleder = value;
    if (value)
      stringField[9] = "true";
    else
      stringField[9] = "false";
  }

  public boolean isDerivative2() {
    if (stringField[9].equalsIgnoreCase("true"))
      return true;
    return false;
  }

  public void setIterationsLeastSquares(int number) {
    super.setIterations(number);
    stringField[6] = Integer.toString(number);
    iterationsLeastSquares = Integer.parseInt(stringField[6]);
  }

  public int getIterationsLeastSquares() {
    return iterationsLeastSquares;
  }

  int actualThread = 0;
  Vector results = null;
  int divideRange = 1;
  int[] divideValue = null;
  String filesBase64;
  int startIndex = 0;
  boolean submissionStarted = false;

  public void solveXGRID(launchBasic computation, Function funtionToMinimize) {
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
                    st.nextToken(); // solution:
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
          //noinspection EmptyCatchBlock
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
        //noinspection EmptyCatchBlock
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
    submissionStarted = false;
    (new PersistentThread() {
      public void executeJob() {
        actualThread++;
        String indexName = Integer.toString(startIndex++);
        String popName = "Maud_population_" + indexName;
        String resultData;
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
          System.out.println("Submitted job ID: " + jobId);
          if (!jobId.equals(Client.FAILED)) {
            // we wait a little before starting retrieve the data
            if (first) {
              submissionStarted = true;
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
                st.nextToken(); // solution:
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
    while (!submissionStarted) {
      try {
        Thread.sleep(500);
      } catch (InterruptedException ie) {
        ie.printStackTrace(System.err);
      }
    }
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

    setIterations(getIntNumberOfPopulations());

    fittingFunction = funtionTominimize;

    int iterat = getIterations();
    if (outputframe != null)
      computation.setIterationSliderValue(iterat);

    printf("Number of iterations : ", iterat);

    for (int i = 0; i < iterat; i++) {
      printf("Starting iteration ", i);
      if (i > getIterations()) {
        printf("Iterations stopped!");
        break;
      }

      if (computation != null && computation.shouldStop()) {
        return;
      }
//  Init the randomizer for random number generation
      initAll(computation);

      if (computation != null && computation.shouldStop()) {
        return;
      }
//  Generate the starting solution configuration
      generateStartingSolutions();

      if (computation != null && computation.shouldStop()) {
        return;
      }
      startSolutionLoop();
    }
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
      outputframe.getProgressBar().setProgressBarValue(
          getIntPopulationSize() * getIntGenerationsNumber());
//      computation.hideIterationPanel();
    }

    nprm = fittingFunction.getNumberOfFreeParameters();
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

    initializeMatrices(nprm, dataNumber);


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
	  fittingFunction.computeFit();
	  fittingFunction.getFit();
    defWSS = fittingFunction.getWSS();
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

    fittingFunction.setDerivate(true);
    if (computation != null && computation.shouldStop()) {
      return;
    }

    bestParams = new double[defParams.length];
    bestWSS = 1.0E50;
    System.arraycopy(defParams, 0, bestParams, 0, bestParams.length);

    updateStringtoDoubleBuffering(false);

  }

  void generateStartingSolutions() {
    if (startRandomConfiguration)
      generateRandomConfiguration();
    else
      pickLastSolution();
  }

  void generateRandomConfiguration() {
  }

  void pickLastSolution() {
  }

  void startSolutionLoop() {
    int numberParameters = defParams.length;
    String genomeSize = Integer.toString(numberParameters);
    String[] args = new String[argsToDefine.length + (numberParameters) * 4];
    for (int i = 0; i < argsToDefine.length; i += 2) {
      args[i] = "-p";
      if (argsToDefine[i].startsWith(GENOME_SIZE))
        args[i + 1] = GENOME_SIZE + genomeSize;
      else if (argsToDefine[i].startsWith(POPULATION_SIZE))
        args[i + 1] = POPULATION_SIZE + getPopulationSize();
      else if (argsToDefine[i].startsWith(GENERATIONS_NUMBER))
        args[i + 1] = GENERATIONS_NUMBER + getGenerationsNumber();
      else if (argsToDefine[i].startsWith(MUTATION_PROB))
        args[i + 1] = MUTATION_PROB + getMutationProbability();
      else if (argsToDefine[i].startsWith(DIFF_GENE))
        args[i + 1] = DIFF_GENE + genomeSize;
      else if (argsToDefine[i].startsWith(SEED_NUMBER))
        args[i + 1] = SEED_NUMBER + Long.toString((System.currentTimeMillis() / 1000));
      else if (argsToDefine[i].startsWith(CROSS_OVER))
        args[i + 1] = CROSS_OVER + getCrossOverType();
      else if (argsToDefine[i].startsWith(CROSS_OVER))
        args[i + 1] = CROSS_OVER_PROBABILITY + getCrossOverProbability();
//      else if (argsToDefine[i].startsWith(CHUNK_SIZE))
//        args[i+1] = CHUNK_SIZE + genomeSize;
      else
        args[i + 1] = argsToDefine[i] + argsToDefine[i + 1];
    }

    int actualNumber = argsToDefine.length;

    for (int i = 0; i < numberParameters; i++) {

      args[i * 4 + actualNumber] = "-p";
      args[i * 4 + 1 + actualNumber] = MIN_GENE + Integer.toString(i) + "=" +
          Float.toString((float)lbound[i]);
      args[i * 4 + 2 + actualNumber] = "-p";
      args[i * 4 + 3 + actualNumber] = MAX_GENE + Integer.toString(i) + "=" +
          Float.toString((float)ubound[i]);
    }
    fittingFunction.setDerivate(true);
    GeneralEvolutionProblem.evolve(this, args);
    System.out.println("End of solution loop, evolution finished");
    fittingFunction.setDerivate(false);
//    Phase aphase = (Phase) getParent();
    if (bestWSS < defWSS) {
      fittingFunction.setFreeParameters(bestParams);
      fittingFunction.saveparameters();
	    fittingFunction.computeFit();
	    fittingFunction.getFit();
	    double wss = fittingFunction.getWSS();
      if (fittingFunction instanceof FilePar)
        ((FilePar) fittingFunction).updatePlot();
      System.out.println("Final chi :" + wss);
    } else {
      fittingFunction.setFreeParameters(defParams);
      fittingFunction.saveparameters();
	    fittingFunction.computeFit();
	    fittingFunction.getFit();
      double wss = fittingFunction.getWSS();
      if (fittingFunction instanceof FilePar)
        ((FilePar) fittingFunction).updatePlot();
      System.out.println("Final chi :" + wss);
    }
//    structureFactorList = null;
    fittingFunction.computeFirstFit();
    fittingFunction.getRefinementIndexes();
    GeneralEvolutionProblem.cleanUp();
    defParams = null;
    bestParams = null;
  }

  public double getFitness(double[] params) {
    fittingFunction.setFreeParameters(params);
//    double wss = fittingFunction.getWSS();
    double wss = solveLeastSquares();
//    double wss = fittingFunction.getWSS();
    for (int i = 0; i < params.length; i++)
      params[i] = fittingFunction.getFreeParameter(i);
    if (outputframe != null)
      outputframe.increaseProgressBarValue();

    if (wss < bestWSS) {
      bestWSS = wss;
      printf("Parameters values:");
      printout(params, nprm);
      printf("Actual best wss :", wss);
      System.arraycopy(params, 0, bestParams, 0, bestParams.length);
      if (fittingFunction instanceof FilePar)
        ((FilePar) fittingFunction).updatePlot();
    }
    return wss;
  }

  // Least squares

  double[][] derivf;
  double[] deriv, b, grad, c, parmn, parm, minSignificantValue;

  public void initializeMatrices(int nprm1, int dataNumber1) {
    parm = new double[nprm1];
    minSignificantValue = new double[nprm1];

    choleskyFlag = new int[nprm1];
    parmn = new double[nprm1];

    int mdi = (nprm1 + 1) * nprm1 / 2;

    deriv = new double[nprm1];
    b = new double[nprm1];
    grad = new double[nprm1];
    c = new double[mdi];

    am = new double[mdi];
    g = new double[nprm1];

    derivf = new double[dataNumber1][nprm1];


  }

  public double solveLeastSquares() {

//    fittingFunction = funtionTominimize;

    int dataNumber = fittingFunction.getNumberOfData();

    fittingFunction.setDerivate(false);

    fittingFunction.computeFirstFit();
	  fittingFunction.getRefinementIndexes();

      fittingFunction.getFit();
      for (int i = 0; i < dataNumber; i++) {
        fit[i] = fittingFunction.getFit(i);
      }

    int nprm = fittingFunction.getNumberOfFreeParameters();

    for (int i = 0; i < nprm; i++) {
      parm[i] = fittingFunction.getFreeParameter(i);
      minSignificantValue[i] = fittingFunction.getParameterMinSignificantValue(i);
      choleskyFlag[i] = 0;
      parmn[i] = parm[i];
      b[i] = parm[i];
    }

    int check = 0; // if check != 0 the new wss can be sligthly greater than the old one

    int mdi = (nprm + 1) * nprm / 2;

// start least squares fitting

    int conver = 0;
    double lambda = 0.01;
    double lmin = 1.0e+20;
    double phi = 1.0;
    niter = -1;


//	  Assert.assert(derivf);


    double wss = 0.0;
//	  fittingFunction.computeFit();
	  fittingFunction.getRefinementIndexes();
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
            parmn[i] = b[i];
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
            fittingFunction.setFreeParameters(parmn);
              for (int i = 0; i < dataNumber; i++)
                fit[i] = fittingFunction.getFit(i);
	          wss = fittingFunction.getWSS();
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

//      fittingFunction.getFit();
          for (int i = 0; i < dataNumber; i++)
            fit[i] = fittingFunction.getFit(i);
          wss = fittingFunction.getWSS();
      oldwss = wss;
      flagfgl = (conver == 0);
      if (conver == 0) {
        for (int i = 0; i < nprm; i++) {
          parm[i] = fittingFunction.getFreeParameter(i);
          parmn[i] = parm[i];
          b[i] = parm[i];
        }
      }
    }       //   next iteration
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

	  fittingFunction.setDerivate(true);
    double firstfit[] = new double[dataNumber];
    double secondfit[] = null;
    if (doubleder)
      secondfit = new double[dataNumber];
    for (int sp = 0; sp < nprm; sp++) {
      if (parm[sp] == 0 && minSignificantValue[sp] == 0)
        dparp = derstep;
      else if (Math.abs(parm[sp]) < Math.abs(minSignificantValue[sp]))
        dparp = (minSignificantValue[sp] * derstep);
      else
        dparp = parm[sp] * derstep;
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
//	        System.out.println(sp + " " + i + " " + firstfit[i] + " " + fit[i] + " " + dparp);
        }
      fittingFunction.setFreeParameter(sp, oldpar);
    }
	  fittingFunction.setDerivate(false);
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
    return new JHYBOptionsD(parent, this);
  }

  public class JHYBOptionsD extends JOptionsDialog {

    JTextField[] parsTF = null;
    JComboBox crossTypeCB = null;
    JSlider iterationJS;
    JCheckBox doubleDerivativeCB;

    public JHYBOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel tfPanel = new JPanel();
      tfPanel.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.NORTH, tfPanel);

      String[] labels = {
          "Population size:         ",
          "Number of generations:   ",
          "Mutation probability:    ",
          "Cross over probability:  ",
          "Cross over type:         ",
          "Number of populations:   ",
          "Iterations number:       ",
          "Parameter precision:     ",
          "Derivative step:         ",
          "Double derivative:       "};

      int numberFields = labels.length;
      parsTF = new JTextField[numberFields];

      for (int i = 0; i < numberFields; i++) {
        if (i == 4) {
          tfPanel.add(new JLabel(labels[i]));
          crossTypeCB = new JComboBox();
          tfPanel.add(crossTypeCB);
          for (int j = 0; j < cross_over_type.length; j++)
            crossTypeCB.addItem(cross_over_type[j]);
        } else if (i == 6) {
          JLabel iterationTF = new JLabel();
          iterationJS = new JSlider();
          iterationJS.setToolTipText("Set the number of iterations during refinement (best values: 3-7)");
          SliderListener listener = new SliderListener(iterationTF);
          iterationJS.addChangeListener(listener);
          tfPanel.add(iterationJS);
          tfPanel.add(iterationTF);
        } else if (i == 9) {
          doubleDerivativeCB = new JCheckBox("Double derivative");
          doubleDerivativeCB.setToolTipText("By selecting numerical double derivative will be used (slower computation)");
          tfPanel.add(doubleDerivativeCB);
        } else {
          tfPanel.add(new JLabel(labels[i]));
          parsTF[i] = new JTextField(Constants.FLOAT_FIELD);
//        genTF.setToolTipText("Set the number of generations for the Genetic Algorithm");
          tfPanel.add(parsTF[i]);
        }
      }

      setTitle("Hybrid refinement options panel");
      initParameters();
      pack();
      iterationJS.setValue(getIterationsLeastSquares());
    }

    public void initParameters() {
      updateStringtoDoubleBuffering(false);
      for (int i = 0; i < parsTF.length; i++) {
        if (!(i == 4 || i == 6 || i == 9))
          parsTF[i].setText(stringField[i]);
      }
      crossTypeCB.setSelectedItem(getCrossOverType());
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
        if (!(i == 4 || i == 6 || i == 9))
          stringField[i] = parsTF[i].getText();
      }
      setCrossOverType((String) crossTypeCB.getSelectedItem());
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

