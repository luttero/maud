/*
 * @(#)ReflectivityGeneticAlgorithm.java created 28/10/2001 Le Mans
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

package it.unitn.ing.rista.diffr.reflectivity;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

import java.awt.*;
import javax.swing.*;
import java.awt.event.*;

import ec.util.*;
import ec.*;
import ec.simple.*;
import ec.vector.*;

import java.io.IOException;

import it.unitn.ing.rista.interfaces.*;

/**
 *  The ReflectivityGeneticAlgorithm is a method to find the initial configuration
 *  of a layered structure using a Genetic Algorithm
 *
 *
 * @version $Revision: 1.8 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class ReflectivityGeneticAlgorithm extends LayerSolutionMethod
        implements GAProblem {

  public static String[] diclistc = {"_riet_ga_population_size", "_riet_ga_generations_number",
                                     "_riet_ga_mutation_prob", "_riet_ga_permutation_prob",
                                     "_riet_ga_thickness_min", "_riet_ga_thickness_max",
                                     "_riet_ga_qc_min", "_riet_ga_qc_max",
                                     "_riet_ga_roughness_min", "_riet_ga_roughness_max",
  };
  public static String[] diclistcrm = {"_riet_ga_population_size", "_riet_ga_generations_number",
                                     "_riet_ga_mutation_prob", "_riet_ga_permutation_prob",
                                     "_riet_ga_thickness_min", "_riet_ga_thickness_max",
                                     "_riet_ga_qc_min", "_riet_ga_qc_max",
                                     "_riet_ga_roughness_min", "_riet_ga_roughness_max",
  };
  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  EvolutionState state;
  boolean startRandomConfiguration = true;

  String[] argsToDefine = {
// number of threads
    "evalthreads=", "1",
    "breedthreads=", "1",
// a good random seed for thread 0
    "seed.0=", "4357",
// ec.EvolutionState
    "generations=", "20",
    "pop.subpop.0.species.min-gene=", "0.0",
    "pop.subpop.0.species.max-gene=", "1.0",
    "pop.subpop.0.species.genome-size=", "100",
    "select.tournament.size=", "2",
    "pop.subpop.0.species.mutation-prob=", "0.01",
    "pop.subpop.0.species.crossover-type=", "one",
    "pop.subpop.0.size=", "500",
    "pop.subpop.0.species.permutation-prob=", "0.01",
    "pop.subpop.0.species.diff-gene=", "3",
    "pop.subpop.0.species.min-gene.0=", "0.0",
    "pop.subpop.0.species.max-gene.0=", "2000.0",
    "pop.subpop.0.species.min-gene.1=", "0.001",
    "pop.subpop.0.species.max-gene.1=", "0.1",
    "pop.subpop.0.species.min-gene.2=", "0.0",
    "pop.subpop.0.species.max-gene.2=", "50.0"
  };

  static int EVALTHREADS = 0;
  static int BREEDTHREADS = 2;
  static int GENERATIONS_NUMBER = 6;
  static int GENOME_SIZE = 12;
  static int POPULATION_SIZE = 20;
  static int MUTATION_PROB = 16;
  static int PERMUTATION_PROB = 22;
  static int THICKNESS_MIN = 26;
  static int THICKNESS_MAX = 28;
  static int QC_MIN = 30;
  static int QC_MAX = 32;
  static int ROUGHNESS_MIN = 34;
  static int ROUGHNESS_MAX = 36;

  DataFileSet[] dataSetList = null;
  double[] defParams = null;
  double[] bestParams = null;
  double defWSS = 0.0;
  double bestWSS = 0.0;

  public ReflectivityGeneticAlgorithm(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Genetic Algorithm Reflectivity";
    IDlabel = "Genetic Algorithm Reflectivity";
    description = "select this to use a Genetic Algorithm";
  }

  public ReflectivityGeneticAlgorithm(XRDcat aobj) {
    this(aobj, "Genetic Algorithm Reflectivity");
  }

  public ReflectivityGeneticAlgorithm() {
    identifier = "Genetic Algorithm Reflectivity";
    IDlabel = "Genetic Algorithm Reflectivity";
    description = "select this to use a Genetic Algorithm";
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
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
  }

  public void initParameters() {
    super.initParameters();
    setPopulationSize("500");
    setGenerationsNumber("20");
    setMutationProbability("0.01");
    setPerMutationProbability("0.01");
    stringField[4] = "0.0";
    stringField[5] = "2000.0";
    stringField[6] = "0.001";
    stringField[7] = "0.2";
    stringField[8] = "0.0";
    stringField[9] = "50.0";
  }

  public void setPopulationSize(String value) {
    stringField[0] = new String(value);
  }

  public String getPopulationSize() {
    return stringField[0];
  }

  public void setGenerationsNumber(String value) {
    stringField[1] = new String(value);
  }

  public String getGenerationsNumber() {
    return stringField[1];
  }

  public void setMutationProbability(String value) {
    stringField[2] = new String(value);
  }

  public String getMutationProbability() {
    return stringField[2];
  }

  public void setPerMutationProbability(String value) {
    stringField[3] = new String(value);
  }

  public String getPerMutationProbability() {
    return stringField[3];
  }

  public boolean canWorkoutHeterostructure() {
    return false;
  }

  public boolean workoutHeterostructure(DataFileSet[] tmpDataSet) {

    dataSetList = tmpDataSet;

//  Init the randomizer for random number generation
    initAll();

//  Generate the starting structure configuration
    generateStartingPositions();

    startSolutionLoop();
    return true;
  }

  void initAll() {

    getFilePar().prepareComputation();
    getFilePar().mainfunction(false, true);

    Sample asample = (Sample) getParent();
    defParams = asample.getLayerParams();
    bestParams = new double[defParams.length];
    defWSS = getFitness(defParams);
    bestWSS = 1.0E50;
    for (int i = 0; i < bestParams.length; i++)
      bestParams[i] = defParams[i];

  }

  void generateStartingPositions() {
    if (startRandomConfiguration)
      generateRandomConfiguration();
    else
      pickLastStructure();
  }

  void generateRandomConfiguration() {
  }

  void pickLastStructure() {
  }

  void startSolutionLoop() {
    String genomeSize = Integer.toString(defParams.length);
    String[] args = new String[argsToDefine.length];
    for (int i = 0; i < argsToDefine.length; i += 2) {
      args[i] = "-p";
      if (i == GENOME_SIZE)
        args[i + 1] = argsToDefine[i] + genomeSize;
      else if (i == POPULATION_SIZE)
        args[i + 1] = argsToDefine[i] + getPopulationSize();
      else if (i == GENERATIONS_NUMBER)
        args[i + 1] = argsToDefine[i] + getGenerationsNumber();
      else if (i == MUTATION_PROB)
        args[i + 1] = argsToDefine[i] + getMutationProbability();
      else if (i == PERMUTATION_PROB)
        args[i + 1] = argsToDefine[i] + getPerMutationProbability();
      else if (i == THICKNESS_MIN)
        args[i + 1] = argsToDefine[i] + stringField[4];
      else if (i == THICKNESS_MAX)
        args[i + 1] = argsToDefine[i] + stringField[5];
      else if (i == QC_MIN)
        args[i + 1] = argsToDefine[i] + stringField[6];
      else if (i == QC_MAX)
        args[i + 1] = argsToDefine[i] + stringField[7];
      else if (i == ROUGHNESS_MIN)
        args[i + 1] = argsToDefine[i] + stringField[8];
      else if (i == ROUGHNESS_MAX)
        args[i + 1] = argsToDefine[i] + stringField[9];
      else
        args[i + 1] = argsToDefine[i] + argsToDefine[i + 1];
    }
    GeneticAlgorithm.evolve(this, args);
    System.out.println("End of solution loop, GA finished");
    dataSetList = null;
    GeneticAlgorithm.cleanUp();

    Sample asample = (Sample) getParent();
    if (bestWSS < defWSS)
      asample.setLayerParams(bestParams);
    else
      asample.setLayerParams(defParams);
    defParams = null;
    bestParams = null;
  }

  public double getFitness(double[] params) {

    Sample asample = (Sample) getParent();

    asample.setLayerParams(params);
    asample.computeSpectra(dataSetList);

    double ws1 = 0.0;
    double ws2 = 0.0;
    double wss = 0.0;
    for (int i = 0; i < dataSetList.length; i++) {
      double data[] = dataSetList[i].getData();
      double fit[] = dataSetList[i].getFit();
      double weight[] = dataSetList[i].getWeight();
/*      ws1 = 0.0;
      ws2 = 0.0;
      for (int k = 0; k < data.length; k++) {
        ws1 += fit[k] * weight[k];
        ws2 += data[k] * weight[k];
      }*/
      for (int k = 0; k < data.length; k++) {
//        fit[k] *= ws2 / ws1;
        wss += Math.abs(fit[k] - data[k]) * weight[k];// * sf.Fhkl_esd;
      }
    }

    if (wss < bestWSS) {
      bestWSS = wss;
      for (int i = 0; i < bestParams.length; i++)
        bestParams[i] = params[i];
    }
    return wss;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JGAReflOptionsD(parent, this);
    return adialog;
  }

  public class JGAReflOptionsD extends JOptionsDialog {

    JTextField[] parsTF = null;

    public JGAReflOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel tfPanel = new JPanel();
      tfPanel.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.CENTER, tfPanel);

      String[] labels = {
        "Population size:         ",
        "Number of generations:   ",
        "Mutation probability:    ",
        "Permutation probability: ",
        "Thickness min (angstrom):",
        "Thickness max (angstrom):",
        "Qc min                  :",
        "Qc max                  :",
        "Roughness min           :",
        "Roughness max           :",
      };

      int numberFields = labels.length;
      parsTF = new JTextField[numberFields];

      for (int i = 0; i < numberFields; i++) {
        tfPanel.add(new JLabel(labels[i]));
        parsTF[i] = new JTextField(Constants.FLOAT_FIELD);
//        genTF.setToolTipText("Set the number of generations for the Genetic Algorithm");
        tfPanel.add(parsTF[i]);
      }

/*			tfPanel = new JPanel();
      tfPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      principalPanel.add(BorderLayout.SOUTH, tfPanel);

      JButton jb = new JButton("Solve structure");
			tfPanel.add(jb);
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					solveCrystalStructure();
          Component parent = getParent();
          while (parent != null && !(parent instanceof myJFrame)) {
            parent = parent.getParent();
          }
          if (parent != null)
            ((myJFrame) parent).updateFieldsChanged();
				}
			});
			jb.setToolTipText("Press this to solve the crystal structure from structure factors");
			*/
      setTitle("GA Layered Structure Solution options panel");
      initParameters();
      pack();
    }

    public void initParameters() {
      for (int i = 0; i < parsTF.length; i++)
        parsTF[i].setText(stringField[i]);
    }

    public void retrieveParameters() {
      for (int i = 0; i < parsTF.length; i++)
        stringField[i] = parsTF[i].getText();
    }

  }
}

