/*
* @(#)MonteCarloAlgorithmRefinement.java created 10/09/2006 Trento, Rosa's home
*
* Copyright (c) 1996-2006 Luca Lutterotti All Rights Reserved.
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
import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.diffr.FilePar;
import it.unitn.ing.rista.awt.JOptionsDialog;

import javax.swing.*;
import java.util.*;
import java.awt.*;

/**
 * The MonteCarloAlgorithmRefinement is a method to refine the spectrum
 * using a MonteCarlo Algorithm
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/12/04 14:30:03 $
 * @since JDK1.1
 */

public class MonteCarloAlgorithmRefinement extends OptimizationAlgorithm {

  public static String[] diclistc = {"_riet_montecarlo_trials_number"};
  public static String[] diclistcrm = {"Number of Montecarlo trials"};
  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  double[] defParams = null;
  double[] bestParams = null;
  double defWSS = 0.0;
  double bestWSS = 0.0;

  ec.util.MersenneTwisterFast randomizer = null;

  public MonteCarloAlgorithmRefinement(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "MonteCarlo Refinement";
    IDlabel = "MonteCarlo Refinement";
    description = "select this to use a MonteCarlo approach";
  }

  public MonteCarloAlgorithmRefinement(XRDcat aobj) {
    this(aobj, "MonteCarlo Refinement");
  }

  public MonteCarloAlgorithmRefinement() {
    identifier = "MonteCarlo Refinement";
    IDlabel = "MonteCarlo Refinement";
    description = "select this to use a MonteCarlo approach";
  }

  public void initConstant() {
    Nstring = 1;
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
    setTrialsNumber("10000");
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

//    fittingFunction.setDerivate(true);
    if (computation != null && computation.shouldStop()) {
      return;
    }

    bestParams = new double[defParams.length];
    bestWSS = 1.0E50;
    for (int i = 0; i < bestParams.length; i++)
      bestParams[i] = defParams[i];

    int time = (int) System.currentTimeMillis();  // safe because we're getting low-order bits
    randomizer = new ec.util.MersenneTwisterFast(time);

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
    defParams = null;
    bestParams = null;
  }

  public double getFitness(double[] params) {
    fittingFunction.setFreeParameters(params);
	  fittingFunction.computeFirstFit();
	  fittingFunction.getFit();
    double wss = fittingFunction.getWSS();
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

  public JOptionsDialog getOptionsDialog(Frame parent) {
    return new JMCSDPDOptionsD(parent, this);
  }

  public class JMCSDPDOptionsD extends JOptionsDialog {

    JTextField[] parsTF = null;
    JComboBox crossTypeCB = null;

    public JMCSDPDOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel tfPanel = new JPanel();
      tfPanel.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.CENTER, tfPanel);

      String[] labels = {
          "Number of trials:   "};

      int numberFields = labels.length;
      parsTF = new JTextField[numberFields];

      for (int i = 0; i < numberFields; i++) {
        tfPanel.add(new JLabel(labels[i]));
        tfPanel.add(parsTF[i] = new JTextField(12));
      }

      setTitle("MonteCarlo refinement options panel");
      initParameters();
      pack();
    }

    public void initParameters() {
      for (int i = 0; i < parsTF.length; i++) {
//        System.out.println(i);
//        System.out.println(parsTF[i]);
//        System.out.println(stringField[i]);
        parsTF[i].setText(stringField[i]);
      }
    }

    public void retrieveParameters() {
      for (int i = 0; i < parsTF.length; i++)
        stringField[i] = parsTF[i].getText();
    }

  }
}
