/*
 * @(#)NeuralNetworkControlPanel.java created Mar 22, 2006 Casalino
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

package it.unitn.ing.rista.neuralnetwork;

import org.joone.engine.*;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;


/**
 * The NeuralNetworkControlPanel is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:04 $
 * @since JDK1.1
 */

public class NeuralNetworkControlPanel extends javax.swing.JPanel implements NeuralNetListener {

  private static final int PREFERRED_WIDTH = 120;
  private static final int PREFERRED_HEIGHT = 500;

  private JTextField numNumberSpectraField;
  private JTextField numNumberCategoriesField;
  private JTextField maxTokenField;
  private JTextField statusField;
  private JTextField inputNeuronField;
  private JTextField hiddenNeuronField;
  private JTextField outputNeuronField;
  private JTextField learnRateField;
  private JTextField momentumField;
  private JTextField stepsField;

  // ProgressBar
  private JProgressBar netProgressBar;

  private JButton trainButton;
  private JButton stopButton;

  /**
   * Creates new form NeuralNetworkControlPanel
   */
  public NeuralNetworkControlPanel() {
    initComponents();

    JPanel netInfoPanel = new JPanel();
    netInfoPanel.setLayout(new BorderLayout());
    netInfoPanel.setBorder(new TitledBorder("Network Info"));

    // Create the labels.
    JLabel numNumberSpectraLabel = new JLabel("Number Spectra");
    JLabel numNumberCategoriesLabel = new JLabel("Number Categories");
    JLabel maxTokenLabel = new JLabel("Max Token");
    JLabel statusLabel = new JLabel("Status");

    // Create the TextFields
    numNumberSpectraField = new JTextField("N/A");
    numNumberSpectraField.setEditable(false);
    numNumberSpectraField.setBorder(BorderFactory.createEmptyBorder(1, 5, 1, 1));

    numNumberCategoriesField = new JTextField("N/A");
    numNumberCategoriesField.setEditable(false);
    numNumberCategoriesField.setBorder(BorderFactory.createEmptyBorder(1, 5, 1, 1));

    maxTokenField = new JTextField("N/A");
    maxTokenField.setEditable(false);
    maxTokenField.setBorder(BorderFactory.createEmptyBorder(1, 5, 1, 1));

    statusField = new JTextField("Not trained yet");
    statusField.setForeground(Color.red);
    statusField.setEditable(false);
    statusField.setBorder(BorderFactory.createEmptyBorder(1, 5, 1, 1));

    // connect the labels with the fields
    numNumberSpectraLabel.setLabelFor(numNumberSpectraField);
    numNumberCategoriesLabel.setLabelFor(numNumberCategoriesField);
    maxTokenLabel.setLabelFor(maxTokenField);
    statusLabel.setLabelFor(statusField);

    // Layout the text labels in a panel.
    JPanel labelPane = new JPanel();
    labelPane.setLayout(new GridLayout(4, 1));
    labelPane.add(numNumberSpectraLabel);
    labelPane.add(numNumberCategoriesLabel);
    labelPane.add(maxTokenLabel);
    labelPane.add(statusLabel);

    // Layout the text fields in a panel.
    JPanel fieldPane = new JPanel();
    fieldPane.setLayout(new GridLayout(4, 1));
    fieldPane.add(numNumberSpectraField);
    fieldPane.add(numNumberCategoriesField);
    fieldPane.add(maxTokenField);
    fieldPane.add(statusField);


    netInfoPanel.add(labelPane, BorderLayout.WEST);
    netInfoPanel.add(fieldPane, BorderLayout.CENTER);

    JPanel netOpPanel = new JPanel();
    netOpPanel.setLayout(new BorderLayout());
    netOpPanel.setBorder(new TitledBorder("Network Operation"));

    // Create the labels.
    JLabel inputNeuronLabel = new JLabel("Input Neurons");
    JLabel hiddenNeuronLabel = new JLabel("Hidden Neurons");
    JLabel outputNeuronLabel = new JLabel("Output Neurons");
    JLabel learnRateLabel = new JLabel("Learning Rate");
    JLabel momentumLabel = new JLabel("Momentum");
    JLabel stepsLabel = new JLabel("Steps");

    // Create the TextFields
    inputNeuronField = new JTextField("1");
    hiddenNeuronField = new JTextField("1");
    outputNeuronField = new JTextField("1");
    learnRateField = new JTextField("0.7");
    momentumField = new JTextField("0.6");
    stepsField = new JTextField("1000");

    // connect the labels with the fields
    inputNeuronLabel.setLabelFor(inputNeuronField);
    hiddenNeuronLabel.setLabelFor(hiddenNeuronField);
    outputNeuronLabel.setLabelFor(outputNeuronField);
    learnRateLabel.setLabelFor(learnRateField);
    momentumLabel.setLabelFor(momentumField);
    stepsLabel.setLabelFor(stepsField);

    // Layout the text labels in a panel.
    JPanel labelPane2 = new JPanel();
    labelPane2.setLayout(new GridLayout(6, 1));
    labelPane2.add(inputNeuronLabel);
    labelPane2.add(hiddenNeuronLabel);
    labelPane2.add(outputNeuronLabel);
    labelPane2.add(learnRateLabel);
    labelPane2.add(momentumLabel);
    labelPane2.add(stepsLabel);

    // Layout the text fields in a panel.
    JPanel fieldPane2 = new JPanel();
    fieldPane2.setLayout(new GridLayout(6, 1));
    fieldPane2.add(inputNeuronField);
    fieldPane2.add(hiddenNeuronField);
    fieldPane2.add(outputNeuronField);
    fieldPane2.add(learnRateField);
    fieldPane2.add(momentumField);
    fieldPane2.add(stepsField);

    JPanel netContrPanel = new JPanel();
    netContrPanel.setLayout(new BorderLayout());

    // create the progress bar
    netProgressBar = new JProgressBar(0, 50);
    netProgressBar.setValue(0);
    netProgressBar.setStringPainted(true);

    // create the buttons for the controls
    trainButton = new JButton("Train");
    trainButton.setEnabled(false);
    trainButton.setMargin(new Insets(0, 1, 0, 0));
    trainButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        // Grab the input from the text field.
        int inputNeuron = 0;
        int hiddenNeuron = 0;
        int outputNeuron = 0;
        double learningRate = 0.0;
        double momentum = 0.0;
        int steps = 0;

        try {
          inputNeuron = Integer.parseInt(inputNeuronField.getText());
        }
        catch (NumberFormatException exp) {
          JOptionPane.showMessageDialog(getParentFrame(),
              "Invalid input for number of input neuron. Integer format required.",
              "Integer Format Required",
              JOptionPane.INFORMATION_MESSAGE);

          return;
        }

        try {
          hiddenNeuron = Integer.parseInt(hiddenNeuronField.getText());
        }
        catch (NumberFormatException exp) {
          JOptionPane.showMessageDialog(getParentFrame(),
              "Invalid input for number of hidden neuron. Integer format required.",
              "Integer Format Required",
              JOptionPane.INFORMATION_MESSAGE);

          return;
        }

        try {
          outputNeuron = Integer.parseInt(outputNeuronField.getText());
        }
        catch (NumberFormatException exp) {
          JOptionPane.showMessageDialog(getParentFrame(),
              "Invalid input for number of output neuron. Integer format required.",
              "Integer Format Required",
              JOptionPane.INFORMATION_MESSAGE);

          return;
        }

        try {
          learningRate = Double.parseDouble(learnRateField.getText());
        }
        catch (NumberFormatException exp) {
          JOptionPane.showMessageDialog(getParentFrame(),
              "Invalid input for learning rate. Double format required.",
              "Double Format Required",
              JOptionPane.INFORMATION_MESSAGE);

          return;
        }

        try {
          momentum = Double.parseDouble(momentumField.getText());
        }
        catch (NumberFormatException exp) {
          JOptionPane.showMessageDialog(getParentFrame(),
              "Invalid input for momentum. Double format required.",
              "Double Format Required",
              JOptionPane.INFORMATION_MESSAGE);

          return;
        }

        try {
          steps = Integer.parseInt(stepsField.getText());
        }
        catch (NumberFormatException exp) {
          JOptionPane.showMessageDialog(getParentFrame(),
              "Invalid input for number of steps. Integer format required.",
              "Integer Format Required",
              JOptionPane.INFORMATION_MESSAGE);

          return;
        }

        ProjectEnvironment env = getProjectEnvironment();
        double[][] inputArray;

        int numOfCategory = env.getNumberOfNumberCategory();
        int total = env.getNumberOfProcessedNumberSpectrum();
        int maxToken = env.getMaxToken();

        //// DEBUG
        //// DEBUG
        //// DEBUG

        if (numOfCategory < 0 || total < 0 || maxToken < 0) {

          // This shouldn't happen since the updateInfo
          // will perform early check on the above condition
          // and disable train network button if any of the
          // above condition is true.
          return;
        }

        if (inputNeuron < maxToken) {
          JOptionPane.showMessageDialog(getParentFrame(),
              "Invalid input for number of input neuron. Integer format required.",
              "Integer Format Required",
              JOptionPane.INFORMATION_MESSAGE);
          return;
        }

        if (hiddenNeuron <= 0) {
          JOptionPane.showMessageDialog(getParentFrame(),
              "Invalid input for number of hidden neuron. Integer format required.",
              "Integer Format Required",
              JOptionPane.INFORMATION_MESSAGE);
          return;
        }

        if (outputNeuron != numOfCategory) {
          JOptionPane.showMessageDialog(getParentFrame(),
              "Invalid input for number of output neuron. Integer format required.",
              "Integer Format Required",
              JOptionPane.INFORMATION_MESSAGE);
          return;
        }

        if (learningRate <= 0) {
          JOptionPane.showMessageDialog(getParentFrame(),
              "Invalid input for learning rate. Double format required.",
              "Double Format Required",
              JOptionPane.INFORMATION_MESSAGE);
          return;
        }

        if (momentum <= 0) {
          JOptionPane.showMessageDialog(getParentFrame(),
              "Invalid input for momentum. Double format required.",
              "Double Format Required",
              JOptionPane.INFORMATION_MESSAGE);
          return;
        }

        if (steps <= 0) {
          JOptionPane.showMessageDialog(getParentFrame(),
              "Invalid input for number of steps. Integer format required.",
              "Integer Format Required",
              JOptionPane.INFORMATION_MESSAGE);
          return;
        }

        inputArray = new double[total][inputNeuron + numOfCategory];

        int tmpIndex = 0;
        for (int i = 0; i < numOfCategory; i++) {
          NumberCategory numberCategory = (NumberCategory) env.getNumberCategory(i);

          double[] tmpID = new double[numOfCategory];
          for (int j = 0; j < tmpID.length; j++) {
            if (j == i) {
              tmpID[j] = 1;
            } else {
              tmpID[j] = 0;
            }
          }
          numberCategory.setID(tmpID);

          int numOfSpectra = numberCategory.getNumberOfSpectra();

          for (int j = 0; j < numOfSpectra; j++) {
            NumberSpectrum numberSpectrum = numberCategory.getSpectrum(j);

            int numOfTokens = numberSpectrum.getNumberOfTokens();

            if (numOfTokens > 0) {
              double[] integerArrayList = numberSpectrum.getTokens();

              int k = 0;
              for (; k < numOfTokens; k++) {
                inputArray[tmpIndex][k] = integerArrayList[k];
              }

              for (; k < inputNeuron; k++) {
                inputArray[tmpIndex][k] = 0;
              }

              double[] ID = numberCategory.getID();
              for (k = 0; k < ID.length; k++) {
                inputArray[tmpIndex][k + inputNeuron] = ID[k];
              }

              tmpIndex++;
            }
          }
        }

        getNeuralNetworkPanel().trainNetwork(inputNeuron, hiddenNeuron, outputNeuron, steps, learningRate, momentum, inputArray);

        /***
         *DEBUG
         *
         System.out.println("inputNeuron="+inputNeuron);
         System.out.println("hiddenNeuron="+hiddenNeuron);
         System.out.println("outputNeuron="+outputNeuron);
         System.out.println("steps="+steps);
         System.out.println("learningRate="+learningRate);
         System.out.println("momentum="+momentum);
         try {
         FileOutputStream out;   // declare a file output object
         PrintStream p;  // declare a print stream object
         // Create a new file output stream
         // connected to "myfile.txt"
         out = new FileOutputStream("debug.txt");

         // Connect print stream to the output stream
         p = new PrintStream( out );

         for(int i=0; i<inputArray.length; i++) {
         for(int j=0; j<inputArray[i].length; j++) {
         if(j==(inputArray[i].length-1)) {
         System.out.print((int)inputArray[i][j]);
         p.print ((int)inputArray[i][j]);
         }
         else {
         System.out.print((int)inputArray[i][j]+";");
         p.print ((int)inputArray[i][j]+";");
         }
         }
         System.out.println("");
         p.print("\n");
         }

         p.close();
         }
         catch(Exception exp) {
         exp.printStackTrace();
         }
         System.out.println("end writing file...");
         **/
      }
    });

    stopButton = new JButton("Stop");
    stopButton.setMargin(new Insets(0, 1, 0, 0));
    stopButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        getNeuralNetworkPanel().stopNetwork();
      }
    });

    JButton defaultButton = new JButton("Set default");
    defaultButton.setMargin(new Insets(0, 1, 0, 0));
    defaultButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        ProjectEnvironment env = getProjectEnvironment();
        int maxToken = env.getMaxToken();
        int hiddenNeurons = 40;
        int numOfCategory = env.getNumberOfNumberCategory();
        double learningRate = 0.7;
        double momentum = 0.6;
        int steps = 1000;

        inputNeuronField.setText("" + maxToken);
        hiddenNeuronField.setText("" + hiddenNeurons);
        outputNeuronField.setText("" + numOfCategory);
        learnRateField.setText("" + learningRate);
        momentumField.setText("" + momentum);
        stepsField.setText("" + steps);
      }
    });

    JPanel buttonPane = new JPanel();
    buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.X_AXIS));
    buttonPane.setBorder(BorderFactory.createEmptyBorder(5, 0, 5, 5));
    buttonPane.add(trainButton);
    buttonPane.add(Box.createRigidArea(new Dimension(5, 0)));
    buttonPane.add(stopButton);
    buttonPane.add(Box.createRigidArea(new Dimension(5, 0)));
    buttonPane.add(defaultButton);
    buttonPane.add(Box.createHorizontalGlue());

    netContrPanel.add(netProgressBar, BorderLayout.CENTER);
    netContrPanel.add(buttonPane, BorderLayout.SOUTH);

    netOpPanel.add(labelPane2, BorderLayout.WEST);
    netOpPanel.add(fieldPane2, BorderLayout.CENTER);
    netOpPanel.add(netContrPanel, BorderLayout.SOUTH);

    add(netInfoPanel, BorderLayout.NORTH);
    add(netOpPanel, BorderLayout.SOUTH);
  }

  ProjectEnvironment getProjectEnvironment() {
    return getNeuralNetworkPanel().getProjectEnvironment();
  }

  public Frame getParentFrame() {
    Container aparent = getParent();
    while (aparent != null && !(aparent instanceof Frame)) {
      aparent = aparent.getParent();
    }
    if (aparent != null)
      return (Frame) aparent;
    else
      return null;
  }

  NeuralNetworkPanel getNeuralNetworkPanel() {
    Container aparent = getParent();
    while (aparent != null && !(aparent instanceof NeuralNetworkPanel)) {
      aparent = aparent.getParent();
    }
    if (aparent != null)
      return (NeuralNetworkPanel) aparent;
    else
      return null;
  }

  /**
   * This method is called from within the constructor to
   * initialize the form.
   * WARNING: Do NOT modify this code. The content of this method is
   * always regenerated by the Form Editor.
   */
  private void initComponents() {//GEN-BEGIN:initComponents

    setLayout(new java.awt.BorderLayout());

  }//GEN-END:initComponents

  public Dimension getPreferredSize() {
    return new Dimension(PREFERRED_WIDTH, PREFERRED_HEIGHT);
  }

  public void updateInfo() {
    ProjectEnvironment env = getProjectEnvironment();

    if (env.getNumberOfNumberCategory() <= 0 || env.getMaxToken() <= 0) {
      numNumberSpectraField.setText("N/A");
      numNumberCategoriesField.setText("N/A");
      maxTokenField.setText("N/A");
      statusField.setForeground(Color.red);
      statusField.setText("Not trained yet");

      trainButton.setEnabled(false);
      stopButton.setEnabled(false);
    } else {
      numNumberSpectraField.setText("" + env.getNumberOfNumberSpectrum());
      numNumberCategoriesField.setText("" + env.getNumberOfNumberCategory());
      maxTokenField.setText("" + env.getMaxToken());

      if (env.getIsNeuralNetworkRunning()) {
        trainButton.setEnabled(false);
        stopButton.setEnabled(true);
        statusField.setForeground(Color.red);
        statusField.setText("Not trained yet");
      } else {
        trainButton.setEnabled(true);
        stopButton.setEnabled(false);

        if (env.getIsNeuralNetworkTrained()) {
          statusField.setForeground(Color.blue);
          statusField.setText("Successful");

          inputNeuronField.setText("" + getNeuralNetworkPanel().getInputRows());
          hiddenNeuronField.setText("" + getNeuralNetworkPanel().getHiddenRows());
          outputNeuronField.setText("" + getNeuralNetworkPanel().getOutputRows());
          learnRateField.setText("" + getNeuralNetworkPanel().getLearningRate());
          momentumField.setText("" + getNeuralNetworkPanel().getMomentum());
        } else {
          statusField.setForeground(Color.red);
          statusField.setText("Not trained yet");
        }
      }
    }
  }

  public void initProgressBar() {
    int steps = 0;

    if (netProgressBar != null) {
      try {
        steps = Integer.parseInt(stepsField.getText());
      }
      catch (NumberFormatException exp) {
        return;
      }

      netProgressBar.setMaximum(steps);
      netProgressBar.setMinimum(0);
      netProgressBar.setValue(0);
    }
  }

  public void increaseProgressBarByOne() {
    netProgressBar.setValue(netProgressBar.getValue() + 1);
  }

  public void clear() {
    netProgressBar.setValue(0);
  }


  public void netStoppedError(NeuralNetEvent e, String error) {
  }

  /**
   * JOONE Callback: Called when the network
   * is starting up. Not used.
   *
   * @param e The JOONE event.
   */
  public void netStarted(NeuralNetEvent e) {
    getProjectEnvironment().setIsNeuralNetworkRunning(true);
    getProjectEnvironment().setIsNeuralNetworkTrained(false);

    updateTabbedPaneInfo();
    initProgressBar();
  }

  /**
   * JOONE Callback: Called when the network
   * is starting up. Not used.
   *
   * @param e The JOONE event.
   */
  public void netStopped(NeuralNetEvent e) {
    getProjectEnvironment().setIsNeuralNetworkRunning(false);
    getProjectEnvironment().setIsNeuralNetworkTrained(true);

    updateTabbedPaneInfo();
  }

  public void updateTabbedPaneInfo() {
    getNeuralNetworkPanel().updateInfo();
  }

  public void errorChanged(NeuralNetEvent e) {
    getNeuralNetworkPanel().addError(((Monitor) e.getSource()).getGlobalError());
  }

  /**
   * JOONE Callback: called to update the progress
   * of the neural network. Used to update the
   * status line.
   *
   * @param e The JOONE event
   */
  public void cicleTerminated(NeuralNetEvent e) {
    increaseProgressBarByOne();
  }

}
