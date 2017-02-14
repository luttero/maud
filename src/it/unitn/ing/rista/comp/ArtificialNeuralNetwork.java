/*
 * @(#)ArtificialNeuralNetwork.java created July 8, 2006 Casalino
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

import org.joone.net.*;
import org.joone.engine.*;
import org.joone.engine.learning.TeachingSynapse;
import org.joone.io.*;
import org.joone.util.DynamicAnnealing;

import java.io.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.ProgressFrame;

/**
 * The ArtificialNeuralNetworkIndexing
 * <p/>
 * References
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/11/10 16:32:30 $
 * @since JDK1.1
 */

public class ArtificialNeuralNetwork implements NeuralNetListener, Serializable {

  NeuralNet annet = null;
  String filename = MaudPreferences.getPref("NeuralNetwork.SaveFilename", Constants.documentsDirectory +
      "ArtificialNeuralNetwork.nnet");
  int inputDimension = 10;
  int neuronNumber = 7;
  int outputDimension = 6;
  int totCycles = MaudPreferences.getInteger("NeuralNetwork.optimizationCycles", 10000);
  int numberOfPatterns = 1;

  public static final int SIGMOID = 0;
  public static final int GAUSS = 1;
  public static final int TANH = 2;
  public static final int LOG = 3;
  public static final int SINE = 4;
  public static final int LINEAR = 5;

  int neuronType = SIGMOID;

  Layer inputLayer = null;
  Layer outputLayer = null;
  TeachingSynapse teachingOutput = null;
  MemoryOutputSynapse outputResult = null;
  MemoryInputSynapse targetSynapse = null;
  MemoryInputSynapse inputSet = null;

  ProgressFrame prF = null;

  double[] results = null;

  boolean training = false;

  static final long serialVersionUID = 1L;

  public ArtificialNeuralNetwork() {
  }

  public ArtificialNeuralNetwork(String afilename, double[][] evaluateSet) {
    try {
      annet = restoreNeuralNet(afilename);
      runNeuralNet(annet, this, evaluateSet);
    } catch (Exception e) {
      try {
        annet = restoreNeuralNet(filename);
        results = runNeuralNet(annet, this, evaluateSet);
      } catch (Exception ie) {
        System.out.println("No trained Neural Network found!");
        ie.printStackTrace();
      }
    }
  }

  public ArtificialNeuralNetwork(String filename, double[][] trainingSet, double[][] expectedResults) {
    try {
      annet = restoreNeuralNet(filename);
    } catch (Exception e) {
      e.printStackTrace();
      System.out.println("File not valid or not found!");
      firstRun(trainingSet, expectedResults);
    }
  }

  public ArtificialNeuralNetwork(double[][] trainingSet, double[][] expectedResults) {
    firstRun(trainingSet, expectedResults);
  }

  public void prepareNeuralNet(double[][] trainingSet, double[][] expectedResults) {
    numberOfPatterns = trainingSet.length;
    inputDimension = trainingSet[0].length;
    outputDimension = expectedResults[0].length;
    neuronNumber = outputDimension * MaudPreferences.getInteger("NeuralNetwork.neuronMultiplication", 1)
        + MaudPreferences.getInteger("NeuralNetwork.neuronAddition", 1);

    annet = createNeuralNet();

    if (!Constants.textonly && Constants.showProgressFrame) {
      try {
        prF = new ProgressFrame(totCycles / 100);
        prF.setTitle("Training neural network (" + numberOfPatterns + " patterns)");
      } catch (NullPointerException npe) {
        System.out.println("Not able to create frame, MacOSX display sleep bug?");
      }

      if (prF != null) {
        prF.setProgressText("Cycles remaining " + totCycles);
      }
    }
  }

  public void firstRun(double[][] trainingSet, double[][] expectedResults) {
    prepareNeuralNet(trainingSet, expectedResults);
    trainNeuralNet(trainingSet, expectedResults, true);
  }

  public void trainNeuralNet(double[][] trainingSet, double[][] expectedResults, boolean save) {
    trainNeuralNet(annet, this, trainingSet, expectedResults);
    if (save)
      saveNeuralNet(annet, filename);
  }

  public double[] getResults() {
    return results;
  }

  public double[] evaluateSet(double[][] evaluateSet) {
    results = runNeuralNet(annet, this, evaluateSet);
    return getResults();
  }

  public NeuralNet createNeuralNet() {
    Layer hiddenLayer;
    // NeuralNet
    NeuralNet nnet = new NeuralNet();

    // Layers
    outputLayer = new SigmoidLayer();
    neuronType = MaudPreferences.getInteger("NeuralNetwork.hiddenLayerType", SIGMOID);
    switch (neuronType) {
      case SIGMOID:
        hiddenLayer = new SigmoidLayer();
        break;
      case GAUSS:
        hiddenLayer = new GaussLayer();
        break;
      case TANH:
        hiddenLayer = new TanhLayer();
        break;
      case SINE:
        hiddenLayer = new SineLayer();
        break;
      case LOG:
        hiddenLayer = new LogarithmicLayer();
        break;
      default: {
        hiddenLayer = new SigmoidLayer();
      }
    }
    neuronType = MaudPreferences.getInteger("NeuralNetwork.inputLayerType", LINEAR);
    switch (neuronType) {
      case SIGMOID:
        inputLayer = new SigmoidLayer();
        break;
      case GAUSS:
        inputLayer = new GaussLayer();
        break;
      case TANH:
        inputLayer = new TanhLayer();
        break;
      case SINE:
        inputLayer = new SineLayer();
        break;
      case LOG:
        inputLayer = new LogarithmicLayer();
        break;
      default: {
        inputLayer = new SigmoidLayer();
      }
    }
    neuronType = MaudPreferences.getInteger("NeuralNetwork.outputLayerType", SIGMOID);
    switch (neuronType) {
      case SIGMOID:
        outputLayer = new SigmoidLayer();
        break;
      case GAUSS:
        outputLayer = new GaussLayer();
        break;
      case TANH:
        outputLayer = new TanhLayer();
        break;
      case SINE:
        outputLayer = new SineLayer();
        break;
      case LOG:
        outputLayer = new LogarithmicLayer();
        break;
      default: {
        outputLayer = new SigmoidLayer();
      }
    }

    inputLayer.setLayerName("Input");
    inputLayer.setRows(inputDimension);
    nnet.addLayer(inputLayer, NeuralNet.INPUT_LAYER);

    hiddenLayer.setLayerName("Hidden");
    hiddenLayer.setRows(neuronNumber);
    nnet.addLayer(hiddenLayer, NeuralNet.HIDDEN_LAYER);
    outputLayer.setLayerName("Output");
    outputLayer.setRows(outputDimension);
    nnet.addLayer(outputLayer, NeuralNet.OUTPUT_LAYER);

    // Synapses
    FullSynapse synapse1 = new FullSynapse();
    synapse1.setName("Synapse 1");
    synapse1.setEnabled(true);
    synapse1.setLoopBack(false);

    // synapse1 connects inputLayer to hiddenLayer
    inputLayer.addOutputSynapse(synapse1);
    hiddenLayer.addInputSynapse(synapse1);

    FullSynapse synapse2 = new FullSynapse();
    synapse2.setName("Synapse 2");
    synapse2.setEnabled(true);
    synapse2.setLoopBack(false);

    // synapse2 connects hiddenLayer to outputLayer
    hiddenLayer.addOutputSynapse(synapse2);
    outputLayer.addInputSynapse(synapse2);

    // I/O Components
    inputSet = new MemoryInputSynapse();
    inputSet.setName("MemoryInputLayer");
    inputSet.setAdvancedColumnSelector(getStringSelector(1, inputDimension));
    inputSet.setFirstRow(1);
    inputSet.setLastRow(0);
//    inputSet.setBuffered(true);
    inputSet.setStepCounter(true);
    inputSet.setMaxBufSize(0);
//    inputSet.setFileName("/Users/luca/Desktop/anncubic_t.txt");
    inputLayer.addInputSynapse(inputSet);

    teachingOutput = new TeachingSynapse();
    teachingOutput.setName("TeachingOutputLayer");
    teachingOutput.setEnabled(true);
    // Teacher's desired synapse
    targetSynapse = new MemoryInputSynapse();
    targetSynapse.setName("TargetInputLayer");
    targetSynapse.setAdvancedColumnSelector(getStringSelector(1, outputDimension));
    targetSynapse.setFirstRow(1);
    targetSynapse.setLastRow(0);
//    targetSynapse.setBuffered(true);
    targetSynapse.setStepCounter(false);
    targetSynapse.setMaxBufSize(0);
//    targetSynapse.setFileName("/Users/luca/Desktop/anncubic.txt");
    // Teacher's result synapses
    teachingOutput.setDesired(targetSynapse);
    nnet.setTeacher(teachingOutput);
    
    outputLayer.addOutputSynapse(teachingOutput);
    return nnet;
  }

  private String getStringSelector(int i, int outputDimension) {
    StringBuffer result = new StringBuffer(Integer.toString(i));
    result.append("-");
    result.append(Integer.toString(outputDimension));
    return result.toString();
  }


  public void trainNeuralNet(NeuralNet nnet, NeuralNetListener listener,
                             double[][] trainingSet, double[][] expectedResults) {
    // Monitor
    training = true;
    System.out.println("Starting Neural Network training");
    Monitor monitor = nnet.getMonitor();
    monitor.setBatchSize(0);
    monitor.setLearningMode(0);
    monitor.setLearningRate(0.6);
    monitor.setMomentum(0.6);
    monitor.setPreLearning(0);
    monitor.setSupervised(true);
    monitor.setTotCicles(totCycles);
    monitor.setTrainingPatterns(numberOfPatterns);
    monitor.setValidation(false);
    monitor.setValidationPatterns(0);
    monitor.setLearning(true);
    DynamicAnnealing dinan = new DynamicAnnealing();
    dinan.setStep(5);
    dinan.setRate(15);
    monitor.addNeuralNetListener(dinan);
    teachingOutput.setEnabled(true);
    targetSynapse.setInputArray(expectedResults);
    inputSet.setInputArray(trainingSet);

    nnet.addNeuralNetListener(listener);
    nnet.start();
    monitor.Go();
    nnet.join();
    while (training) {
      try {
        Thread.sleep(100);

      } catch (InterruptedException e) {
        e.printStackTrace();
      }
    }
    System.out.println("Neural Network training finished!");
  }

  public double[] runNeuralNet(NeuralNet nnet, NeuralNetListener listener, double[][] evaluateSet) {
    // Monitor
    System.out.println("Start Neural Network evaluation");
    Monitor monitor = nnet.getMonitor();
/*    monitor.setBatchSize(0);
    monitor.setLearningMode(0);
    monitor.setLearningRate(0.7);
    monitor.setMomentum(0.3);
    monitor.setPreLearning(0);
    monitor.setSupervisioned(true);*/
    monitor.setTotCicles(1);
    monitor.setTrainingPatterns(1);
//    monitor.setValidation(false);
//    monitor.setValidationPatterns(0);
    monitor.setLearning(false);

//    nnet.addNeuralNetListener(listener);

    inputLayer.removeAllInputs();
    inputSet = new MemoryInputSynapse();
    inputSet.setName("MemoryInputLayer");
    inputSet.setAdvancedColumnSelector(getStringSelector(1, inputDimension));
    inputSet.setFirstRow(1);
    inputSet.setLastRow(0);
//    inputSet.setBuffered(true);
    inputSet.setStepCounter(true);
    inputSet.setMaxBufSize(0);
//    inputSet.setFileName("/Users/luca/Desktop/anncubic_t.txt");
    inputLayer.addInputSynapse(inputSet);
    inputSet.setInputArray(evaluateSet);

//    teachingOutput.setEnabled(false);
    outputLayer.removeAllOutputs();
    outputResult = new MemoryOutputSynapse();
    outputResult.setName("FinalOutputLayer");
    outputResult.setEnabled(true);
//    outputResult.setFileName("/Users/luca/output.txt");
//    outputResult.setBuffered(true);
    outputLayer.addOutputSynapse(outputResult);

    nnet.start();
    monitor.Go();
    nnet.join();

    double[] pattern = outputResult.getLastPattern();
    System.out.println("Neural Network evaluation finished!");
    nnet.stop();
    return pattern;
  }

  public void stop() {
    annet.stop();
  }

  public void saveNeuralNet(NeuralNet nnet, String fileName) {
    try {
      FileOutputStream stream = new FileOutputStream(fileName);
      ObjectOutputStream out = new ObjectOutputStream(stream);
      out.writeObject(nnet);
      out.close();
    }
    catch (Exception excp) {
      excp.printStackTrace();
    }
  }

  public NeuralNet restoreNeuralNet(String filename) throws Exception {
    if (filename == null)
      throw new NullPointerException();
    FileInputStream stream = new FileInputStream(filename);
    ObjectInputStream inp = new ObjectInputStream(stream);
    return (NeuralNet) inp.readObject();
  }

  /**
   * JOONE Callback: called when the neural network
   * stops. Not used.
   *
   * @param e The JOONE event
   */
  public void netStopped(NeuralNetEvent e) {
  }

  /**
   * JOONE Callback: called to update the progress
   * of the neural network. Used to update the
   * status line.
   *
   * @param e The JOONE event
   */
  public void cicleTerminated(NeuralNetEvent e) {
    Monitor mon = (Monitor) e.getSource();
    long c = mon.getCurrentCicle();
    if (c < 2) {
      training = false;
      if (!Constants.textonly && prF != null) {
        prF.setVisible(false);
        prF.dispose();
        prF = null;
      }
    } else {
      long cl = c / 100;
      // print the results every 1000 cycles
      if ((cl * 100) == c) {
        if (!Constants.textonly && prF != null) {
          prF.setProgressText("Cycles remaining " + c + " - Error = " + mon.getGlobalError());
          prF.increaseProgressBarValue();
        }
      }
    }
  }

  /**
   * JOONE Callback: Called when the network
   * is starting up. Not used.
   *
   * @param e The JOONE event.
   */
  public void netStarted(NeuralNetEvent e) {
  }

  public void netStoppedError(NeuralNetEvent e, String string) {
  }

  public void errorChanged(NeuralNetEvent e) {
  }

}
