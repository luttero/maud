package it.unitn.ing.esqui.client;

import it.unitn.ing.rista.util.Misc;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

/** ESQUI_Client.java
 * <br>
 * Title:			<b>ESQUI Client Analyses</b>
 * </br>
 * Description:		Methods to create the analysis frame for the ESQUI
 client
 * @author:			Leonardo Cont, December 2000
 * @revision:		January 2001
 * @comment:		none
 */

public class ClientAnalysisFrame extends JFrame {

  ClientAnalysis newAnalysis = null;

  JTextField[] zFields = null;
  JTextField[] yFields = null;
  JTextField[] xFields = null;
  JTextField[] phiFields = null;
  JTextField[] chiFields = null;
  JTextField[] omegaFields = null;
  JTextField[] twothetaFields = null;

  JPanel advSettPanel = null;
  JButton openAdvBox = null;

  ClientAnalysisFrame(ClientAnalysis analysis) {
    super(analysis.typeOfAnalysis);

    newAnalysis = analysis;

    Border ebR = BorderFactory.createEtchedBorder(EtchedBorder.RAISED);

//		Create a first upper panel which will contain info
//		about analysis settings
    JPanel infoPanel = new JPanel(new BorderLayout());
    JLabel infoLabel = new JLabel("Current analysis settings");
    infoPanel.add(infoLabel, BorderLayout.NORTH);
//		Create the analysis info area
    JTextArea infoArea = new JTextArea();
    infoArea.setLineWrap(true);
    infoArea.setEditable(false);
    infoArea.setPreferredSize(new Dimension(300, 200));
//		Create the JScrollPane
    JScrollPane infoScrollPane = new JScrollPane(infoArea);
    infoPanel.add(infoScrollPane, BorderLayout.CENTER);
//		SOUTH of infoArea panel is FREE!!!
    infoPanel.setBorder(ebR);
    getContentPane().add(infoPanel, BorderLayout.CENTER);

//		Create the advanced settings panel
    advSettPanel = createAdvancedSettingsPanel();
//		Add the advanced settings panel
    getContentPane().add(advSettPanel, BorderLayout.EAST);

//		Create the lower button panel
    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    JButton setDefault = ClientMisc.createButton("RotCWRight.gif", "set default parameters");
    setDefault.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        System.out.println("setDefault");
      }
    });
    buttonPanel.add(setDefault);

    openAdvBox = ClientMisc.createButton("OpenLock.gif", "open advanced settings box");
    openAdvBox.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (advSettPanel.isVisible()) {
          openAdvBox.setIcon(ClientMisc.getImage("OpenLock.gif"));
          advSettPanel.setVisible(false);
          pack();
        } else {
          setSize(500, 300);
          advSettPanel.setVisible(true);
          pack();
          openAdvBox.setIcon(ClientMisc.getImage("Lock.gif"));
        }
      }
    });
    buttonPanel.add(openAdvBox);

    JButton confirmSettings = ClientMisc.createButton("Check.gif", "confirm settings");
    confirmSettings.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        System.out.println("confirmSettings");
      }
    });
    buttonPanel.add(confirmSettings);
    getContentPane().add(buttonPanel, BorderLayout.SOUTH);

    pack();
    ClientMisc.locateOnScreen(this, 30, 30);
    setVisible(true);
  }

  JPanel createAdvancedSettingsPanel() {
    JPanel tmpPanel = new JPanel(new GridLayout(8, 4));
    String[] advSettingsLabels = {"Parameter", "min", "max", "step"};
    for (int i = 0; i < (ClientAnalysis.maxIndex + 1); i++) {
      JLabel tmpLabel = new JLabel(advSettingsLabels[i]);
      tmpLabel.setHorizontalAlignment(JLabel.CENTER);
      tmpPanel.add(tmpLabel);
    }
//		Create the textfields
    createAdvancedTextFields();
    tmpPanel.add(new JLabel("z"));
    for (int i = 0; i < ClientAnalysis.maxIndex; i++)
      tmpPanel.add(zFields[i]);
    tmpPanel.add(new JLabel("y"));
    for (int i = 0; i < ClientAnalysis.maxIndex; i++)
      tmpPanel.add(yFields[i]);
    tmpPanel.add(new JLabel("x"));
    for (int i = 0; i < ClientAnalysis.maxIndex; i++)
      tmpPanel.add(xFields[i]);
    tmpPanel.add(new JLabel("phi"));
    for (int i = 0; i < ClientAnalysis.maxIndex; i++)
      tmpPanel.add(phiFields[i]);
    tmpPanel.add(new JLabel("chi"));
    for (int i = 0; i < ClientAnalysis.maxIndex; i++)
      tmpPanel.add(chiFields[i]);
    tmpPanel.add(new JLabel("omega"));
    for (int i = 0; i < ClientAnalysis.maxIndex; i++)
      tmpPanel.add(omegaFields[i]);
    tmpPanel.add(new JLabel("2-theta"));
    for (int i = 0; i < ClientAnalysis.maxIndex; i++)
      tmpPanel.add(twothetaFields[i]);
    tmpPanel.setVisible(false);
    return tmpPanel;
  }

  void createAdvancedTextFields() {
    zFields = new JTextField[ClientAnalysis.maxIndex];
    yFields = new JTextField[ClientAnalysis.maxIndex];
    xFields = new JTextField[ClientAnalysis.maxIndex];
    phiFields = new JTextField[ClientAnalysis.maxIndex];
    chiFields = new JTextField[ClientAnalysis.maxIndex];
    omegaFields = new JTextField[ClientAnalysis.maxIndex];
    twothetaFields = new JTextField[ClientAnalysis.maxIndex];
    for (int i = 0; i < ClientAnalysis.maxIndex; i++) {
      zFields[i] = new JTextField();
      zFields[i].setHorizontalAlignment(JTextField.CENTER);
      zFields[i].addFocusListener(new TextFieldListener(this));
      yFields[i] = new JTextField();
      yFields[i].setHorizontalAlignment(JTextField.CENTER);
      yFields[i].addFocusListener(new TextFieldListener(this));
      xFields[i] = new JTextField();
      xFields[i].setHorizontalAlignment(JTextField.CENTER);
      xFields[i].addFocusListener(new TextFieldListener(this));
      phiFields[i] = new JTextField();
      phiFields[i].setHorizontalAlignment(JTextField.CENTER);
      phiFields[i].addFocusListener(new TextFieldListener(this));
      chiFields[i] = new JTextField();
      chiFields[i].setHorizontalAlignment(JTextField.CENTER);
      chiFields[i].addFocusListener(new TextFieldListener(this));
      omegaFields[i] = new JTextField();
      omegaFields[i].setHorizontalAlignment(JTextField.CENTER);
      omegaFields[i].addFocusListener(new TextFieldListener(this));
      twothetaFields[i] = new JTextField();
      twothetaFields[i].setHorizontalAlignment(JTextField.CENTER);
      twothetaFields[i].addFocusListener(new TextFieldListener(this));
    }
  }

  public void setFieldsParameters(JTextField[] fields, float[] values) {
    for (int i = 0; i < fields.length; i++)
      fields[i].setText(String.valueOf(values[i]));
  }
}
