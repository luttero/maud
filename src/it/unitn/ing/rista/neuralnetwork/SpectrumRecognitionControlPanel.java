/*
 * @(#)SpectrumRecognitionControlPanel.java created Mar 23, 2006 Casalino
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

import org.joone.engine.NeuralNetListener;
import org.joone.engine.NeuralNetEvent;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.ArrayList;

import it.unitn.ing.rista.util.PersistentThread;


/**
 * The SpectrumRecognitionControlPanel is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/12/04 14:30:15 $
 * @since JDK1.1
 */

public class SpectrumRecognitionControlPanel extends javax.swing.JPanel implements NeuralNetListener {

  private static final int PREFERRED_WIDTH = 120;
  private static final int PREFERRED_HEIGHT = 500;

  private JTextField spectrumNameField;
  private JTextField spectrumTokenField;
  private JTextField statusField;
  private SpectrumRecognitionTablePanel spectrumRecognitionTablePanel;
  private JButton recognizeButton;
  private JProgressBar imgControlBar;

  /**
   * Creates new form SpectrumRecognitionControlPanel
   */
  public SpectrumRecognitionControlPanel() {
    initComponents();

    JPanel recogInfoPanel = new JPanel();
    recogInfoPanel.setLayout(new BorderLayout());
    recogInfoPanel.setBorder(new TitledBorder("Recognition Info"));

    // Create the labels.
    JLabel spectrumNameLabel = new JLabel("Spectrum");
    JLabel spectrumTokenLabel = new JLabel("Token");
    JLabel statusLabel = new JLabel("Status");

    // Create the TextFields
    spectrumNameField = new JTextField("N/A");
    spectrumNameField.setEditable(false);
    spectrumNameField.setBorder(BorderFactory.createEmptyBorder(1, 5, 1, 1));

    spectrumTokenField = new JTextField("N/A");
    spectrumTokenField.setEditable(false);
    spectrumTokenField.setBorder(BorderFactory.createEmptyBorder(1, 5, 1, 1));

    statusField = new JTextField("Not recognized yet");
    statusField.setForeground(Color.red);
    statusField.setEditable(false);
    statusField.setBorder(BorderFactory.createEmptyBorder(1, 5, 1, 1));

    // connect the labels with the fields
    spectrumNameLabel.setLabelFor(spectrumNameField);
    spectrumTokenLabel.setLabelFor(spectrumTokenField);
    statusLabel.setLabelFor(statusField);

    // Layout the text labels in a panel.
    JPanel labelPane = new JPanel();
    labelPane.setLayout(new GridLayout(3, 1));
    labelPane.add(spectrumNameLabel);
    labelPane.add(spectrumTokenLabel);
    labelPane.add(statusLabel);

    // Layout the text fields in a panel.
    JPanel fieldPane = new JPanel();
    fieldPane.setLayout(new GridLayout(3, 1));
    fieldPane.add(spectrumNameField);
    fieldPane.add(spectrumTokenField);
    fieldPane.add(statusField);

    recogInfoPanel.add(labelPane, BorderLayout.WEST);
    recogInfoPanel.add(fieldPane, BorderLayout.CENTER);

    JPanel recogResultPanel = new JPanel();
    recogResultPanel.setLayout(new BorderLayout());
    recogResultPanel.setBorder(new TitledBorder("Recognition Result"));

    spectrumRecognitionTablePanel = new SpectrumRecognitionTablePanel();

    recogResultPanel.add(spectrumRecognitionTablePanel, BorderLayout.CENTER);

    JPanel recogControlPanel = new JPanel();
    recogControlPanel.setLayout(new BorderLayout());
    recogControlPanel.setBorder(new TitledBorder("Recognition Control"));

    // create the progress bar
    imgControlBar = new JProgressBar(0, 100);
    imgControlBar.setValue(0);
    imgControlBar.setStringPainted(true);

    // create the buttons for the controls
    recognizeButton = new JButton("Recognize");
    recognizeButton.setMargin(new Insets(0, 1, 0, 0));

    recognizeButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        recognizeButton.setEnabled(false);

        Thread t = new PersistentThread() {
        public void executeJob() {
            NumberSpectrum activeNumberSpectrum = getSpectrumRecognitionPanel().getNumberSpectrum();

            if (activeNumberSpectrum == null) {
              recognizeButton.setEnabled(true);
              return;
            }

            ProjectEnvironment env = getProjectEnvironment();
            if (env.getIsNeuralNetworkRunning()) {
              JOptionPane.showMessageDialog(getParentFrame(),
                  "This operation is not allowed due to neural network is running. \\nPlease re-run this operation when neural network is finished running.",
                  "Neural Network Is Running",
                  JOptionPane.INFORMATION_MESSAGE);
              recognizeButton.setEnabled(true);
              return;
            }

            clear();

            try {
              // Preparing input for neural network.
              int inputRows = getNeuralNetworkPanel().getInputRows();
              int numOfTokens = activeNumberSpectrum.getNumberOfTokens();
              double[][] inputArray = new double[1][inputRows];

              if (inputRows < numOfTokens) {
                JOptionPane.showMessageDialog(getParentFrame(),
                    "The row number of neural network input layer is not enough to process this spectrum. \\nTry to re-train the neural network with higher input rows.",
                    "Input Layer Row Not Enough",
                    JOptionPane.INFORMATION_MESSAGE);

                spectrumTokenField.setText("" + numOfTokens);
                recognizeButton.setEnabled(true);
                setStatusField("Fail to recognize spectrum.");
                return;
              }

              if (numOfTokens > 0) {
                double[] data = activeNumberSpectrum.getTokens();

                int k = 0;
                for (; k < numOfTokens; k++) {
                  inputArray[0][k] = data[k];
                }

                for (; k < inputRows; k++) {
                  inputArray[0][k] = 0;
                }
              }

              /*
               *DEBUG
               *
              for(int i=0; i<inputArray.length; i++) {
                  for(int j=0; j<inputArray[i].length; j++) {
                      if(j==(inputArray[i].length-1)) {
                          System.out.print ((int)inputArray[i][j]);
                      }
                      else {
                          System.out.print ((int)inputArray[i][j]+";");
                      }
                  }
                  System.out.print("\n");
              }
              ***/

              double[] result = getNeuralNetworkPanel().runNetwork(inputRows, inputArray);

              int numberOfNumberCategory = env.getNumberOfNumberCategory();
              ArrayList numberCategories = env.getNumberCategories();

              double maxPercentage = 0.0;
              NumberCategory maxNumberCategory = null;

              for (int i = 0; i < numberOfNumberCategory; i++) {
                NumberCategory numberCategory = (NumberCategory) numberCategories.get(i);
                double error = 0;
                double[] ID = numberCategory.getID();

                for (int j = 0; j < result.length; j++) {
                  error += Math.abs(ID[j] - result[j]);
                }

                double percentage = 100.0 * (1.0 - error / ((double) result.length));
                spectrumRecognitionTablePanel.addResult(numberCategory, percentage);

                if (maxNumberCategory == null) {
                  maxNumberCategory = numberCategory;
                  maxPercentage = percentage;
                } else {
                  if (percentage > maxPercentage) {
                    maxNumberCategory = numberCategory;
                    maxPercentage = percentage;
                  }
                }
              }

              if (maxNumberCategory != null) {
                spectrumNameField.setText(maxNumberCategory.getCategoryName());
                spectrumTokenField.setText("" + numOfTokens);
                statusField.setForeground(Color.blue);
                statusField.setText("Recognized");
              }

              imgControlBar.setValue(100);

              setStatusField("Spectrum recognition successful.");
              updateTabbedPaneInfo();
              recognizeButton.setEnabled(true);
            } catch (Exception exp) {
              exp.printStackTrace();
              setStatusField("Fail to recognize spectrum.");
              recognizeButton.setEnabled(true);
            } catch (Error err) {
              err.printStackTrace();
              setStatusField("Fail to recognize spectrum.");
              recognizeButton.setEnabled(true);
            }
          }
        };

        t.start();
      }
    });

    JButton resetButton = new JButton("Reset");
    resetButton.setMargin(new Insets(0, 1, 0, 0));

    resetButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        clear();
      }
    });

    JPanel buttonPane = new JPanel();
    buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.X_AXIS));
    buttonPane.setBorder(BorderFactory.createEmptyBorder(5, 0, 5, 5));
    buttonPane.add(recognizeButton);
    buttonPane.add(Box.createRigidArea(new Dimension(5, 0)));
    buttonPane.add(resetButton);
    buttonPane.add(Box.createHorizontalGlue());

    recogControlPanel.add(imgControlBar, BorderLayout.CENTER);
    recogControlPanel.add(buttonPane, BorderLayout.SOUTH);

    add(recogInfoPanel, BorderLayout.NORTH);
    add(recogResultPanel, BorderLayout.CENTER);
    add(recogControlPanel, BorderLayout.SOUTH);
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

  void setStatusField(String value) {
    getSpectrumRecognitionPanel().getSpectrumScholarPanel().setStatusField(value);
  }

  void updateTabbedPaneInfo() {
    getSpectrumRecognitionPanel().getSpectrumScholarPanel().updateTabbedPaneInfo();
  }

  ProjectEnvironment getProjectEnvironment() {
    return getSpectrumRecognitionPanel().getProjectEnvironment();
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
    return getSpectrumRecognitionPanel().getSpectrumScholarPanel().getNeuralNetworkPanel();
  }

  SpectrumRecognitionPanel getSpectrumRecognitionPanel() {
    Container aparent = getParent();
    while (aparent != null && !(aparent instanceof SpectrumRecognitionPanel)) {
      aparent = aparent.getParent();
    }
    if (aparent != null)
      return (SpectrumRecognitionPanel) aparent;
    else
      return null;
  }

  public void updateInfo() {
    ProjectEnvironment env = getProjectEnvironment();

    if (env.getIsNeuralNetworkTrained()) {
      recognizeButton.setEnabled(true);
    } else {
      recognizeButton.setEnabled(false);
    }
  }

  public void clear() {
    spectrumRecognitionTablePanel.clear();
    imgControlBar.setValue(0);
    spectrumNameField.setText("N/A");
    spectrumTokenField.setText("N/A");
    statusField.setForeground(Color.red);
    statusField.setText("Not recognized yet");
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
  }

  /**
   * JOONE Callback: Called when the network
   * is starting up. Not used.
   *
   * @param e The JOONE event.
   */
  public void netStopped(NeuralNetEvent e) {
  }

  public void errorChanged(NeuralNetEvent e) {
  }

  /**
   * JOONE Callback: called to update the progress
   * of the neural network. Used to update the
   * status line.
   *
   * @param e The JOONE event
   */
  public void cicleTerminated(NeuralNetEvent e) {
  }

}
