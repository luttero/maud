/*
 * @(#)InstrumentD.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.border.BevelBorder;
import java.awt.*;
import java.awt.event.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

/**
 * The InstrumentD is a class to implement the interface to edit the
 * instrumental setting.
 *
 * @version $Revision: 1.9 $, $Date: 2006/11/10 09:32:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class InstrumentD extends myJFrame {

  public Instrument theinstrument;

  JTextField InstrumentnameTF;
  JTextField intensityTF;
  JComboBox[] optchoice;
//  JParameterListPane ThetaPanel;

  public InstrumentD(Frame parent, Instrument ainstrument) {

    super(parent);

    initializeSizeAndPosition(
            false, "instrumentFrame.frameWidth", "instrumentFrame.frameHeight", 400, 500,
            true, "instrumentFrame.framePositionX", "instrumentFrame.framePositionY", 50, 50);

    parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));

    theinstrument = ainstrument;

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout(6, 6));

   /* JTabbedPane p1 = new JTabbedPane();
    c1.add(p1, BorderLayout.CENTER);
    String p1String[] = {"General",
                         "Errors"};*/

    JPanel jp1 = new JPanel();
    jp1.setLayout(new BorderLayout(6, 6));
    c1.add(jp1, BorderLayout.CENTER);
//    p1.addTab(p1String[0], null, jp1);

    JPanel jPanel12 = new JPanel();
    jPanel12.setLayout(new BorderLayout(6, 6));
    jp1.add("North", jPanel12);

    JPanel jPanel6 = new JPanel();
    jPanel6.setLayout(new GridLayout(2, 1, 3, 3));
    jPanel12.add("West", jPanel6);

    String[] tmpStringS = {"Instrument name:",
                           "Incident intensity:"};
    for (int i = 0; i < 2; i++)
      jPanel6.add(new JLabel(tmpStringS[i]));

    JPanel jPanel8 = new JPanel();
    jPanel8.setLayout(new GridLayout(2, 1, 3, 3));
    jPanel12.add("Center", jPanel8);

    InstrumentnameTF = new JTextField(24);
    jPanel8.add(InstrumentnameTF);
    intensityTF = new JTextField(Constants.FLOAT_FIELD);
    jPanel8.add(intensityTF);
    intensityTF.setText("1");

    jPanel12 = new JPanel();
    jPanel12.setLayout(new BorderLayout(2, 2));
    jp1.add("Center", jPanel12);

    jPanel6 = new JPanel();
    jPanel6.setLayout(new GridLayout(0, 1, 1, 1));
    jPanel12.add("West", jPanel6);

    String[] tmpStringS1 = {"      Intensity calibration:",
                            "      Angular calibration:",
                            "      Geometry:",
                            "      Measurement:",
                            "      Source:",
                            "      Detector:",
                            "      Instrument Broadening:"};

    for (int i = 0; i < theinstrument.Nsubordinate - 1; i++)
      jPanel6.add(new JLabel(tmpStringS1[i]));

    jPanel8 = new JPanel();
    jPanel8.setLayout(new GridLayout(0, 1, 1, 1));
    jPanel12.add("Center", jPanel8);

    optchoice = new JComboBox[theinstrument.Nsubordinate - 1];
    for (int i = 0; i < theinstrument.Nsubordinate - 1; i++) { //absorption removed here
      final int index = i;
      JPanel jPanel2 = new JPanel();
      jPanel2.setLayout(new FlowLayout(FlowLayout.RIGHT, 1, 1));
      jPanel8.add(jPanel2);
      optchoice[i] = new JComboBox();
      optchoice[i].setEditable(false);
      optchoice[i].setMaximumRowCount(4);
      jPanel2.add(optchoice[i]);
      JButton optbutton = new JIconButton("Eyeball.gif", "Options");
      optbutton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          subordinateOptions(index);
        }
      });
      jPanel2.add(optbutton);
    }

/*    JPanel panel1 = new JPanel(new FlowLayout());
    panel1.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "2theta or d-space displacement"));
    //"2theta displ.";
    p1.addTab(p1String[1], null, panel1);

    ThetaPanel = new JParameterListPane(this, false, true);
    panel1.add(ThetaPanel);*/

    JPanel closebuttonPanel = new JPanel();
    closebuttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    c1.add("South", closebuttonPanel);
    JButton jbok1 = new JCloseButton();
    closebuttonPanel.add(jbok1);
    jbok1.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        retrieveParameters();
        setVisible(false);
        dispose();
      }
    });
    getRootPane().setDefaultButton(jbok1);

    setTitle(ainstrument.toXRDcatString());
    initparameters();
    parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

    createDefaultMenuBar();

    pack();
  }

  public Instrument getData() {
    return theinstrument;
  }

  void initparameters() {
    int i, j;

    InstrumentnameTF.setText(theinstrument.getInstrumentID());
    intensityTF.setText(theinstrument.getIntensity().getValue());
    addComponenttolist(intensityTF, theinstrument.getIntensity());

//    ThetaPanel.setList(theinstrument, 0);

    for (i = 0; i < theinstrument.Nsubordinate - 1; i++) { //absorption removed here
      for (j = 0; j < theinstrument.getsubordClassNumber(i); j++)
        optchoice[i].addItem(theinstrument.getsubordIdentifier(i, j));
      optchoice[i].setSelectedItem(theinstrument.subordinateField[i].identifier);
    }

  }

  public void retrieveParameters() {
    super.retrieveParameters();

    theinstrument.setInstrumentID(InstrumentnameTF.getText());
    theinstrument.getIntensity().setValue(intensityTF.getText());

//    ThetaPanel.retrieveparlist();

    for (int i = 0; i < theinstrument.Nsubordinate - 1; i++) { //absorption removed here
      String value = optchoice[i].getSelectedItem().toString();
      if (theinstrument.subordinateField[i] == null ||
              !value.equals(theinstrument.subordinateField[i].identifier))
        theinstrument.setsubordinateField(i, value);
    }
  }

  public void subordinateOptions(int index) {
    String value = optchoice[index].getSelectedItem().toString();
    if (theinstrument.subordinateField[index] == null ||
            !value.equals(theinstrument.subordinateField[index].identifier))
      theinstrument.setsubordinateField(index, value);

    theinstrument.subordinateField[index].getOptionsDialog(this).setVisible(true);
  }

  public void dispose() {
    for (int i = 0; i < theinstrument.Nsubordinate - 1; i++) {  //absorption removed here
      optchoice[i].removeAllItems();
    }
    optchoice = null;
    theinstrument = null;
//    ThetaPanel.dispose();

    super.dispose();
  }

}
