/*
* @(#)GeneticAlgorithmAltRefinement.java created 17/06/2006 Casalino
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

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.xgridclient.*;

import javax.swing.*;
import java.awt.*;
import java.util.*;

import org.jgap.*;
import org.jgap.impl.*;

/**
 * The GeneticAlgorithmAltRefinement is a method to refine the spectrum
 * using a Genetic Algorithm from jgap library
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/12/04 14:30:03 $
 * @since JDK1.1
 */

public class GeneticAlgorithmAltRefinement extends OptimizationAlgorithm {

  public static String[] diclistc = {"_riet_ga_population_size", "_riet_ga_generations_number",
      "_riet_ga_mutation_prob", "_riet_ga_cross_over_probability",
      "_riet_ga_cross_over_type", "_riet_ga_populations_number"
      /*, "_riet_ga_min_max_automatic_%",
     "_riet_ga_cycles_number", "_riet_ga_cycles_decr_factor"*/
  };
  public static String[] diclistcrm = {"_riet_ga_population_size", "_riet_ga_generations_number",
      "_riet_ga_mutation_prob", "_riet_ga_cross_over_probability",
      "_riet_ga_cross_over_type", "_riet_ga_populations_number"
      /*, "_riet_ga_min_max_automatic_%",
     "_riet_ga_cycles_number", "_riet_ga_cycles_decr_factor"*/
  };
  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean startRandomConfiguration = true;

//  StructureFactorList[] structureFactorList = null;
  double[] defParams = null;
  double[] bestParams = null;
  double defWSS = 0.0;
  double bestWSS = 0.0;

  static String[] cross_over_type = {"one", "two", "any"};

  public GeneticAlgorithmAltRefinement(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Genetic Algorithm Refinement";
    IDlabel = "Genetic Algorithm Refinement";
    description = "select this to use Genetic Algorithm of jgap";
  }

  public GeneticAlgorithmAltRefinement(XRDcat aobj) {
    this(aobj, "Genetic Algorithm Refinement");
  }

  public GeneticAlgorithmAltRefinement() {
    identifier = "Genetic Algorithm Refinement";
    IDlabel = "Genetic Algorithm Refinement";
    description = "select this to use Genetic Algorithm of jgap";
  }

  public void initConstant() {
    Nstring = 6;
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
    setPopulationSize("500");
    setGenerationsNumber("5");
    setMutationProbability("0.01");
    setCrossOverProbability("0.3");
    setCrossOverType(0);
    setNumberOfPopulations(1);
  }

  public void setNumberOfPopulations(int value) {
    stringField[5] = Integer.toString(value);
  }

  public void setNumberOfPopulations(String value) {
    stringField[5] = new String(value);
  }

  public String getNumberOfPopulations() {
    return stringField[5];
  }

  public int getIntNumberOfPopulations() {
    return Integer.parseInt(getNumberOfPopulations());
  }

  public void setPopulationSize(String value) {
    stringField[0] = new String(value);
  }

  public String getPopulationSize() {
    return stringField[0];
  }

  public int getIntPopulationSize() {
    return Integer.parseInt(getPopulationSize());
  }

  public void setGenerationsNumber(String value) {
    stringField[1] = new String(value);
  }

  public String getGenerationsNumber() {
    return stringField[1];
  }

  public int getIntGenerationsNumber() {
    return Integer.parseInt(getGenerationsNumber());
  }

  public void setMutationProbability(String value) {
    stringField[2] = new String(value);
  }

  public String getMutationProbability() {
    return stringField[2];
  }

  public void setCrossOverProbability(String value) {
    stringField[3] = new String(value);
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

  int actualThread = 0;
  Vector results = null;
  int divideRange = 1;
  int[] divideValue = null;
  String filesBase64;
  int startIndex = 0;
  boolean started = false;

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

    fittingFunction.setDerivate(true);
    if (computation != null && computation.shouldStop()) {
      return;
    }

    bestParams = new double[defParams.length];
    bestWSS = 1.0E50;
    for (int i = 0; i < bestParams.length; i++)
      bestParams[i] = defParams[i];

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
/*    String genomeSize = Integer.toString(numberParameters);
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
          Float.toString(lbound[i]);
      args[i * 4 + 2 + actualNumber] = "-p";
      args[i * 4 + 3 + actualNumber] = MAX_GENE + Integer.toString(i) + "=" +
          Float.toString(ubound[i]);
    }*/

    // Start with a DefaultConfiguration, which comes setup with the
    // most common settings.
    // -------------------------------------------------------------
    try {
    Configuration conf = new DefaultConfiguration();
    conf.setPreservFittestIndividual(true);
    conf.setKeepPopulationSizeConstant(true);
    // Set the fitness function we want to use, which is our
    // MinimizingMakeChangeFitnessFunction. We construct it with
    // the target amount of change passed in to this method.
    // ---------------------------------------------------------
    FitnessFunction myFunc =
        new MinimizingMakeChangeFitnessFunction();
//    conf.setFitnessFunction(myFunc);
    conf.setBulkFitnessFunction(new BulkFitnessOffsetRemover(myFunc));
    // Optionally, this example is working with DeltaFitnessEvaluator.
    // See MinimizingMakeChangeFitnessFunction for details!
    // ---------------------------------------------------------------
//    conf.setFitnessEvaluator(new DeltaFitnessEvaluator());

    // Now we need to tell the Configuration object how we want our
    // Chromosomes to be setup. We do that by actually creating a
    // sample Chromosome and then setting it on the Configuration
    // object. As mentioned earlier, we want our Chromosomes to each
    // have four genes, one for each of the coin types. We want the
    // values (alleles) of those genes to be integers, which represent
    // how many coins of that type we have. We therefore use the
    // IntegerGene class to represent each of the genes. That class
    // also lets us specify a lower and upper bound, which we set
    // to sensible values for each coin type.
    // --------------------------------------------------------------
    Gene[] sampleGenes = new Gene[numberParameters];
      for (int i = 0; i < numberParameters; i++)
        sampleGenes[i] = new DoubleGene(conf, lbound[i], ubound[i]);
/*    sampleGenes[0] = new IntegerGene(conf, 0, 3 * 10); // Quarters
    sampleGenes[1] = new IntegerGene(conf, 0, 2 * 10); // Dimes
    sampleGenes[2] = new IntegerGene(conf, 0, 1 * 10); // Nickels
    sampleGenes[3] = new IntegerGene(conf, 0, 4 * 10); // Pennies */
    IChromosome sampleChromosome = new Chromosome(conf, sampleGenes);
    conf.setSampleChromosome(sampleChromosome);
    // Finally, we need to tell the Configuration object how many
    // Chromosomes we want in our population. The more Chromosomes,
    // the larger number of potential solutions (which is good for
    // finding the answer), but the longer it will take to evolve
    // the population (which could be seen as bad).
    // ------------------------------------------------------------
    conf.setPopulationSize(getIntPopulationSize());

    // Create random initial population of Chromosomes.
    // Here we try to read in a previous run via XMLManager.readFile(..)
    // for demonstration purpose only!
    // -----------------------------------------------------------------
    Genotype population;
/*    try {
      Document doc = XMLManager.readFile(new File("JGAPExample26.xml"));
      population = XMLManager.getGenotypeFromDocument(conf, doc);
    }
    catch (UnsupportedRepresentationException uex) {*/
      // JGAP codebase might have changed between two consecutive runs
      population = Genotype.randomInitialGenotype(conf);
/*    }
    catch (FileNotFoundException fex) {
      population = Genotype.randomInitialGenotype(conf);
    }*/
    // Now we initialize the population randomly, anyway!
    // If you want to load previous results from file, remove the next line!
    population = Genotype.randomInitialGenotype(conf);
    // Evolve the population. Since we don't know what the best answer
    // is going to be, we just evolve the max number of times.
    // ---------------------------------------------------------------
    for (int i = 0; i < getIntGenerationsNumber(); i++) {
      population.evolve();
    }
    // Save progress to file. A new run of this example will then be able to
    // resume where it stopped before!
    // ---------------------------------------------------------------------

    // Represent Genotype as tree with elements Chromomes and Genes.
/*    DataTreeBuilder builder = DataTreeBuilder.getInstance();
    IDataCreators doc2 = builder.representGenotypeAsDocument(population);
    // create XML document from generated tree
    XMLDocumentBuilder docbuilder = new XMLDocumentBuilder();
    Document xmlDoc = (Document) docbuilder.buildDocument(doc2);
    XMLManager.writeFile(xmlDoc, new File("JGAPExample26.xml"));*/
    // Display the best solution we found.
    // -----------------------------------
    IChromosome bestSolutionSoFar = population.getFittestChromosome();
    double[] params = getParametersFromGenes(bestSolutionSoFar);
    System.out.println("End of solution loop, evolution finished");
//    Phase aphase = (Phase) getParent();
    fittingFunction.setFreeParameters(params);
    fittingFunction.saveparameters();
    fittingFunction.computeFit();
	    fittingFunction.getFit();
    double wss = fittingFunction.getWSS();
    if (fittingFunction instanceof FilePar)
      ((FilePar) fittingFunction).updatePlot();
    System.out.println("Final chi :" + wss);
//    structureFactorList = null;
    fittingFunction.computeFirstFit();
    fittingFunction.getRefinementIndexes();
    } catch (Exception ice) {
      System.out.println("End of solution loop, error occurred");
      ice.printStackTrace();
    }
    GeneralEvolutionProblem.cleanUp();
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

  public double[] getParametersFromGenes(IChromosome a_potentialSolution) {
  int numberParameters = defParams.length;
  double[] params = new double[numberParameters];

  for (int i = 0; i < numberParameters; i++) {
    Double parValue =
        (Double) a_potentialSolution.getGene(i).getAllele();
    params[i] = parValue.doubleValue();
  }
    return params;
  }


  public JOptionsDialog getOptionsDialog(Frame parent) {
    return (JOptionsDialog) new JGAASDPDOptionsD(parent, this);
  }

  /**
   * Sample fitness function for the MakeChange example.
   *
   */
  public class MinimizingMakeChangeFitnessFunction
      extends FitnessFunction {

    public MinimizingMakeChangeFitnessFunction() {
    }

    /**
     * Determine the fitness of the given Chromosome instance. The higher the
     * return value, the more fit the instance. This method should always
     * return the same fitness value for two equivalent Chromosome instances.
     *
     * @param a_potentialSolution the Chromosome instance to evaluate
     *
     * @return positive double reflecting the fitness rating of the given
     * Chromosome
     */
    public double evaluate(IChromosome a_potentialSolution) {
      // Take care of the fitness evaluator. It could either be weighting higher
      // fitness values higher (e.g.DefaultFitnessEvaluator). Or it could weight
      // lower fitness values higher, because the fitness value is seen as a
      // defect rate (e.g. DeltaFitnessEvaluator)
      boolean defaultComparation = Genotype.getStaticConfiguration().
          getFitnessEvaluator().isFitter(2, 1);

      double[] params = getParametersFromGenes(a_potentialSolution);

      double fitness = getFitness(params);
      if (fitness < bestWSS) {
        bestWSS = fitness;
        printf("Parameters values:");
        printout(params, nprm);
        printf("Actual best wss :", fitness);
        for (int i = 0; i < bestParams.length; i++) {
          bestParams[i] = params[i];
        }
        if (fittingFunction instanceof FilePar)
          ((FilePar) fittingFunction).updatePlot();
      }
      if (defaultComparation) {
        if (fitness != 0.0)
          fitness = 1.0 / fitness;
        else
          fitness = 1.0E300;
      }
      return fitness;
    }

  }

  public class JGAASDPDOptionsD extends JOptionsDialog {

    JTextField[] parsTF = null;
    JComboBox crossTypeCB = null;

    public JGAASDPDOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel tfPanel = new JPanel();
      tfPanel.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.CENTER, tfPanel);

      String[] labels = {
          "Population size:         ",
          "Number of generations:   "}; /*,
          "Mutation probability:    ",
          "Cross over probability:  ",
          "Cross over type:         ",
          "Number of populations:   "};*/

      int numberFields = labels.length;
      parsTF = new JTextField[numberFields];

      for (int i = 0; i < numberFields; i++) {
        tfPanel.add(new JLabel(labels[i]));
        if (i != 4) {
          parsTF[i] = new JTextField(Constants.FLOAT_FIELD);
//        genTF.setToolTipText("Set the number of generations for the Genetic Algorithm");
          tfPanel.add(parsTF[i]);
        } else {
          crossTypeCB = new JComboBox();
          tfPanel.add(crossTypeCB);
          for (int j = 0; j < cross_over_type.length; j++)
            crossTypeCB.addItem(cross_over_type[j]);
        }
      }

      setTitle("Evolution refinement options panel");
      initParameters();
      pack();
    }

    public void initParameters() {
      for (int i = 0; i < parsTF.length/* - 2*/; i++)
        parsTF[i].setText(stringField[i]);
//      crossTypeCB.setSelectedItem(getCrossOverType());
//      parsTF[parsTF.length - 1].setText(stringField[parsTF.length - 1]);
    }

    public void retrieveParameters() {
      for (int i = 0; i < parsTF.length/* - 2*/; i++)
        stringField[i] = parsTF[i].getText();
//      setCrossOverType((String) crossTypeCB.getSelectedItem());
//      stringField[parsTF.length - 1] = parsTF[parsTF.length - 1].getText();
    }

  }
}

