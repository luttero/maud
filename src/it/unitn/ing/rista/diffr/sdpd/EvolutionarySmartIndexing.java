/*
 * @(#)EvolutionarySmartIndexing.java created Sep 6, 2006 Casalino
 *
 * Copyright (c) 2006 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.sdpd;

import it.unitn.ing.rista.interfaces.Function;
import it.unitn.ing.rista.comp.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;

import javax.swing.*;
import java.io.OutputStream;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * The EvolutionarySmartIndexing is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/11/10 16:32:30 $
 * @since JDK1.1
 */
public class EvolutionarySmartIndexing implements Function {

  int symmetry = 0;
  double dspacemin = 1.0E30, dspacemax = -1.0E30;
  int numberOfFreeParameters = 0;
  double[] parameters = null;
  double[] lowerBound = null;
  double[] upperBound = null;
  OptimizationAlgorithm refinementAlgorithm = null;
  double errorDspace = MaudPreferences.getDouble("indexing.maxDSpacingError", 0.0001);
  int maxToIndex = MaudPreferences.getInteger("indexing.maxPeaksToIndex", 100);
  int maxToExclude = MaudPreferences.getInteger("indexing.maxPeaksToExclude", 1);
//  double penalty = MaudPreferences.getDouble("dspacing.PenaltyError", 1.0);
  Phase thephase = null;
  double bestFitness = 0.0;
  DiffrDataFile[] thespectra;
  double[] checkDone = null;
  double rangeFactor = MaudPreferences.getDouble("indexingSmart.cutoffAngle", 0.05);
  boolean useGaussian = MaudPreferences.getBoolean("indexingSmart.useGaussianWeight", false);
  boolean useMonteCarlo = MaudPreferences.getBoolean("indexingSmart.useMonteCarlo", false);
  double expFactor = MaudPreferences.getDouble("indexingSmart.exponentFactor", 0.5);
  boolean isOptimizing = false;
  OutputPanel outputPanel = null;

  public EvolutionarySmartIndexing(Phase aphase, DiffrDataFile[] spectra) {
    thephase = aphase;
    outputPanel = thephase.getFilePar().getMainFrame().getOutputPanel();
    thespectra = spectra;
    initEvolutionarySmartIndexing();
  }

  public void initEvolutionarySmartIndexing() {
    numberOfFreeParameters = 0;
    for (int i = 0; i < 6; i++) {
      if (thephase.ic[i] == 1)
        numberOfFreeParameters++;
    }
//    numberOfFreeParameters++;

    parameters = new double[numberOfFreeParameters];
    lowerBound = new double[numberOfFreeParameters];
    upperBound = new double[numberOfFreeParameters];
    int index = 0;
    for (int i = 0, j = 0; i < 6; i++)
      if (thephase.ic[i] == 1) {
        parameters[j] = thephase.getCellValue(i);
        lowerBound[j] = thephase.getCellValueMinD(i);
        upperBound[j++] = thephase.getCellValueMaxD(i);
      }
/*    int j = numberOfFreeParameters - 1;
    parameters[j] = 0.0; // Dspacing error
    lowerBound[j] = -0.001;
    upperBound[j] = 0.001;*/

//    System.out.println("Best fitness = " + computeFitness(parameters));

    dspacemin = 1.0E30;
    dspacemax = -1.0E30;
    int maxIntRange = 0;
    for (int s = 0; s < thespectra.length; s++) {
      double[] arange = thespectra[s].getRange(thespectra[s].startingindex, thespectra[s].finalindex);
      int intRange = thespectra[s].finalindex - thespectra[s].startingindex;
      if (intRange > maxIntRange)
        maxIntRange = intRange;
      if (dspacemin > arange[0])
        dspacemin = arange[0];
      if (dspacemax < arange[1])
        dspacemax = arange[1];
      thespectra[s].getDataFileSet().computeBackground();
    }
//    System.out.println("Range: " + dspacemin + " " + dspacemax);
    checkDone = new double[maxIntRange];

    bestFitness = 1.0E-8;
    bestFitness = computeFitness(parameters);
    System.out.println("Starting fitness: " + bestFitness + ", wss: " + 1.0 / bestFitness);
    bestFitness /= 10.0;

    if (useMonteCarlo) {
      refinementAlgorithm = new MonteCarloAlgorithmRefinement(null, "MonteCarlo smart indexing");
      ((MonteCarloAlgorithmRefinement) refinementAlgorithm).setTrialsNumber("10000");
    } else {
      refinementAlgorithm = new GeneticAlgorithmRefinement(null, "Evolutionary indexing");
      ((GeneticAlgorithmRefinement) refinementAlgorithm).setCrossOverProbability("0.3");
      ((GeneticAlgorithmRefinement) refinementAlgorithm).setCrossOverType(1);
      ((GeneticAlgorithmRefinement) refinementAlgorithm).setGenerationsNumber("10");
      ((GeneticAlgorithmRefinement) refinementAlgorithm).setMutationProbability("0.05");
      ((GeneticAlgorithmRefinement) refinementAlgorithm).setPopulationSize("1000");
    }
  }

  public OptimizationAlgorithm getOptimizationAlgorithm() {
    return refinementAlgorithm;
  }

  public int getNumberOfData() {
    return 10;
  }

  public double[] getFit() {

    return null;
  }

  public double getWSS() {
    return 1.0 / computeFitness(parameters);
  }

  public void computeFirstFit() {
  }

  public int getNumberOfFreeParameters() {
    return numberOfFreeParameters;
  }

  public double getLowerBound(int index) {
    return (double) lowerBound[index];
  }

  public double getUpperBound(int index) {
    return (double) upperBound[index];
  }

  public double getParameterMinSignificantValue(int i) {
    return (double) lowerBound[i];
  }

  public double getFreeParameter(int index) {
    return (double) parameters[index];
  }

  public void setDerivate(boolean value) {
  }

  public void setOptimizing(boolean value) {
    isOptimizing = value;
  }

  public boolean isOptimizing() {
    return isOptimizing;
  }

//  double[] individualParams = null;

  public void setFreeParameters(double[] parm) {
//    for (int i = 0; i < numberOfFreeParameters; i++)
//      parameters[i] = parm[i];
    parameters = parm;
  }

  public void saveparameters() {
    for (int i = 0, j = 0; i < 6; i++)
      if (thephase.ic[i] == 1) {
//        thephase.getCell(i).setValueMin(parameters[j] * 0.95);
//        thephase.getCell(i).setValueMax(parameters[j] * 1.05);
        if (thephase != null)
          thephase.setCellValue(i, parameters[j++]);
      }
  }


  public double computeFitness(double[] parms) {

//    int totalPresent, totalMissing;//, notFound;

//    System.out.println("a:" + parms[0]);
    double[] aList = getLines(thephase, parms, dspacemin - errorDspace, dspacemax + errorDspace);
    int nRi = aList.length;
    if (nRi < 1)
      return 10E-30;
    double fitness = 0.0;
    for (int s = 0; s < thespectra.length; s++) {
      for (int i = 0; i < checkDone.length; i++) {
        checkDone[i] = thespectra[s].getBkgFit(i + thespectra[s].startingindex);
      }
      for (int i = 0; i < aList.length; i++) {
        double[] positions = thespectra[s].getCoordinatesForDspacing(aList[i]);
//        System.out.println("positions:" + positions[0] + " " + positions[1]);
        double cutoff = rangeFactor;
        double cutoff2 = - 0.25 / (cutoff * cutoff);
        for (int j = 0; j < positions.length; j++) {
          if (thespectra[s].getDataFileSet().getInstrument().getRadiationType().getRadiationWeigth(j) > .1) {
            int startIndex = thespectra[s].getOldNearestPoint(positions[j] - cutoff);
            int stopIndex = thespectra[s].getOldNearestPoint(positions[j] + cutoff);
            if (stopIndex < startIndex) {
              int temp = stopIndex;
              stopIndex = startIndex;
              startIndex = temp;
            }
            if (startIndex < thespectra[s].startingindex)
              startIndex = thespectra[s].startingindex;
            if (stopIndex > thespectra[s].finalindex)
              stopIndex = thespectra[s].finalindex;
            for (int l = startIndex; l < stopIndex; l++) {
              double remaining = thespectra[s].getYData(l) - checkDone[l - thespectra[s].startingindex];
              checkDone[l - thespectra[s].startingindex] += remaining;
//              System.out.println("remaining:" + remaining + ", at position " + thespectra[s].getXData(l) + ", check " + checkDone[l - thespectra[s].startingindex]);
              if (remaining < 0.0)
                remaining = 0.0;
              else if (useGaussian) {
                double dist = positions[j] - thespectra[s].getXData(l);
                dist *= dist;
                fitness += Math.sqrt(remaining) * Math.exp(dist * cutoff2);
//                System.out.println("dist:" + Math.sqrt(dist) + ", weight " + Math.exp(dist * cutoff2));
              } else {
                //double dist = positions[j] - thespectra[s].getXData(l);
                //dist *= dist;
                fitness += Math.sqrt(remaining); // * Math.exp(dist * cutoff2);
//                System.out.println("dist:" + Math.sqrt(dist) + ", weight " + Math.exp(dist * cutoff2));
              }
            }
          }
        }
      }
    }
    fitness = fitness * Math.pow(nRi, -expFactor);
//    fitness = 1.0 / fitness;
    if (1.0 / fitness < bestFitness) {
      bestFitness = 1.0 / fitness;
    }
    return fitness;
  }

  public double[] getLines(Phase phase, double[] pars, double dmin, double dmax) {
    Parameter.doRefresh = false;
    int i = 0;
    for (int j = 0; j < 3; j++)
      if (phase.ic[j] == 1)
        phase.setCellValue(j, pars[i++]);
    phase.updateAll();
    phase.CellSymmetry();
    phase.updateParametertoDoubleBuffering(false);
    double[] lines = phase.computeReflectionList(dmin, dmax, false, 0.0001);
    Parameter.doRefresh = true;
    return lines;
  }


// These are not necessary for the GeneticAlgorithmRefinement class

  public double getData(int index) {
    return 0.0f;
  }

  public double getWeight(int index) {
    return 0.0f;
  }

  public double getFit(int index) {
    return 0.0f;
  }

  public double[] getRefinementIndexes() {
    return null;
  }

  public void setRw(double Rw) {
  }

  public void setR(double R) {
  }

  public void setRexp(double R) {
  }

  public void setFreeParameter(int index, double value) {
  }

/*  public void setFreeParameter(int index, double value) {
  }

  public void setFreeParameters(double[] parm) {
  }

  public void setErrors(double[] errors) {
  }*/

  public void setErrors(double[] errors) {
  }

  public void computeFit() {
  }

  public boolean checkBound(int j, double parmn) {
    return true;
  }

  public void backupallParameters() {
  }

  public void restoreParametersValues() {
  }

  public void mainfunction(boolean hasoutput, boolean refreshAll) {
  }

  public boolean reduceMemory() {
    return true;
  }

  public boolean singleFunctionComputing() {
    return false;
  }

  public int getNumberofIterations() {
    return 10;
  }

  public int prepareIteration() {
    return 0;
  }

  public OutputStream getResultStream() {
    return null;
  }

  public void endOfComputation() {
  }

  public boolean logOutput() {
    return false;
  }

  public void printInformations(OutputStream resultStream) {
  }

  public void closeLogResultFile() {
  }

  public void fittingFileOutput() {
  }

  public void prepareComputation() {
  }

  public void refreshFit(double[] fit, double[] parm, int[] controls) {
  }

  public SmartIndexingOptionsDialog startDialog(Frame parentFrame) {
    return new SmartIndexingOptionsDialog(parentFrame, "Indexing algorithm options", false, this);
  }

  class SmartIndexingOptionsDialog extends myJDialog {

    EvolutionarySmartIndexing indexing;
//		Hashtable originalindexes;
    JTextField iterationF = null;

    public SmartIndexingOptionsDialog(Frame parent, String title, boolean modal, EvolutionarySmartIndexing indexing) {

      super(parent, title, modal);

      this.indexing = indexing;

      Container pane = getContentPane();
      pane.setLayout(new BorderLayout(6, 6));

      JPanel optionPanel = new JPanel(new GridLayout(0, 1, 3, 3));
      pane.add(optionPanel);

      JPanel rowP;

      rowP = new JPanel(new FlowLayout(FlowLayout.CENTER, 6, 6));
      optionPanel.add(rowP);
      rowP.add(new JLabel("Number of iterations: "));
      iterationF = new JTextField(Integer.toString(indexing.getNumberofIterations()));
      rowP.add(iterationF);

      rowP = new JPanel(new FlowLayout(FlowLayout.CENTER, 6, 6));
      optionPanel.add(rowP);

      JButton jb = new JButton("Evolutionary algorithm options");
      rowP.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          evolutionaryOptions();
        }
      });

      JPanel panel1 = new JPanel();
      panel1.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      pane.add("South", panel1);

      panel1.add(jb = new JIconButton("GreenFlag.gif", "Start"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          setVisible(false);
          dispose();
        }
      });
      panel1.add(jb = new JCancelButton());
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          setVisible(false);
          dispose();
        }
      });
      if (!modal)
        setHelpButton(panel1);

      pack();
      setVisible(true);

    }

    public void evolutionaryOptions() {
      (indexing.getOptimizationAlgorithm()).getOptionsDialog(SmartIndexingOptionsDialog.this.getFrameParent()).
          setVisible(true);
    }

    public void retrieveParameters() {
//      indexing.setResult();
      indexing.getOptimizationAlgorithm().setIterations(Integer.parseInt(iterationF.getText()));
      launchRefine computation = new launchRefine(indexing, outputPanel);
      computation.prepare();
      computation.launch();
    }

  }
}
