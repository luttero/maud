/*
 * @(#)AnglesGeneratorD.java created 6/01/2001 Casalino
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

package it.unitn.ing.rista.awt;

import java.awt.*;
import java.awt.event.*;
import java.lang.*;
import java.io.*;
import javax.swing.*;

/**
 * The AnglesGeneratorD is a dialog class to prepare angles for measurement (ESQUIGo).
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:52 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class AnglesGeneratorD extends JFrame {

  final static int ANGLES_NUMBER = 3;

  JTextField filenameField = null;
  JTextField[] beginField = new JTextField[3];
  JTextField[] endField = new JTextField[3];
  JTextField[] stepField = new JTextField[3];

  public AnglesGeneratorD() {
    this("Multiple datafile CIF Creator");
  }

  public AnglesGeneratorD(String title) {
    super();

    this.getContentPane().add(anglesPanel(), BorderLayout.NORTH);
    this.getContentPane().add(filePanel(), BorderLayout.CENTER);

    JPanel buttonPanel = new JPanel();
    JButton goButton = new JButton("OK", getImage("images/Check.gif"));
    goButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        try {
          createFile(filenameField.getText());
        } catch (Exception exc) {
          AttentionD.showAlertDialog(AnglesGeneratorD.this, "One or more bad values!");
          return;
        }
      }
    });
    buttonPanel.add(goButton);
    JButton closeButton = new JButton("Exit", getImage("images/Exit.gif"));
    closeButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setVisible(false);
        dispose();
      }
    });
    buttonPanel.add(closeButton);

    this.getContentPane().add(buttonPanel, BorderLayout.SOUTH);

    this.pack();
    this.setSize(250, 200);
    Utility.putOnScreenAt(this, 30);
    this.setVisible(true);
  }

  JPanel anglesPanel() {

    JPanel anglesPanel = new JPanel(new GridLayout(ANGLES_NUMBER + 1, 4));
    JLabel[] angleLabel = new JLabel[3];
    String[] angleString = {"Phi",
                            "Chi",
                            "Omega"
    };

    anglesPanel.add(new JLabel("Angle"));
    anglesPanel.add(new JLabel("From"));
    anglesPanel.add(new JLabel("To"));
    anglesPanel.add(new JLabel("Step"));

    for (int i = 0; i < ANGLES_NUMBER; i++) {
      angleLabel[i] = new JLabel(angleString[i]);
      beginField[i] = new JTextField();
      endField[i] = new JTextField();
      stepField[i] = new JTextField();
      anglesPanel.add(angleLabel[i]);
      anglesPanel.add(beginField[i]);
      anglesPanel.add(endField[i]);
      anglesPanel.add(stepField[i]);
    }
    return anglesPanel;
  }

  JPanel filePanel() {
    JPanel filePanel = new JPanel();
    filePanel.add(new JLabel("Filename"));
    filenameField = new JTextField(12);
    filePanel.add(filenameField);
    return filePanel;
  }

  void createFile(String filenameString) {
    int increment = 1;

    double phiInitValue = Float.valueOf(beginField[0].getText()).floatValue();
    double chiInitValue = Float.valueOf(beginField[1].getText()).floatValue();
    double omegaInitValue = Float.valueOf(beginField[2].getText()).floatValue();

    double phiEndValue = Float.valueOf(endField[0].getText()).floatValue();
    double chiEndValue = Float.valueOf(endField[1].getText()).floatValue();
    double omegaEndValue = Float.valueOf(endField[2].getText()).floatValue();

    double phiStepValue = Float.valueOf(stepField[0].getText()).floatValue();
    double chiStepValue = Float.valueOf(stepField[1].getText()).floatValue();
    double omegaStepValue = Float.valueOf(stepField[2].getText()).floatValue();

    if (phiStepValue == 0) phiStepValue = 1;
    if (chiStepValue == 0) chiStepValue = 1;
    if (omegaStepValue == 0) omegaStepValue = 1;

    try {
      BufferedWriter outputBuffer = new BufferedWriter(new FileWriter(filenameString + ".cif"));
      outputBuffer.write("loop_");
      outputBuffer.newLine();
      outputBuffer.write("_riet_meas_datafile_name");
      outputBuffer.newLine();
      outputBuffer.write("_pd_meas_orientation_omega");
      outputBuffer.newLine();
      outputBuffer.write("_pd_meas_orientation_chi");
      outputBuffer.newLine();
      outputBuffer.write("_pd_meas_orientation_phi");
      outputBuffer.newLine();
      for (double i = omegaInitValue; i <= omegaEndValue; i += omegaStepValue) {
        for (double j = chiInitValue; j <= chiEndValue; j += chiStepValue) {
          for (double k = phiInitValue; k <= phiEndValue; k += phiStepValue) {
            outputBuffer.write(" " + filenameString + "(" + increment++ + ") ");
            outputBuffer.write(Float.toString((float)i) + " ");
            outputBuffer.write(Float.toString((float)j) + " ");
            outputBuffer.write(Float.toString((float)k));
            outputBuffer.newLine();
          }
        }
      }
      outputBuffer.newLine();
      outputBuffer.flush();
      outputBuffer.close();
    } catch (IOException ioExcep) {
    }

  }

  ImageIcon getImage(String imagePath) {
    return new ImageIcon(getClass().getResource(imagePath));
  }

  public static void main(String[] args) {
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
    } catch (Exception exc) {
      AttentionD.showAlertDialog(new Frame(), "Error loading the Look and Feel!");
    }
    new AnglesGeneratorD();
  }
}
