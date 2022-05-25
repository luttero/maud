/*
 * @(#)JOptionDialogComplete.java created May 16, 2007 Caen
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.XRDcat;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * The JOptionDialogComplete is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: May 16, 2007 12:01:15 PM $
 * @since JDK1.1
 */
public class JOptionDialogComplete extends JOptionsDialog {

  public XRDcat theObject;
  JTextField theObjectTF;
  JComboBox[] optchoice;
  boolean[] active = {};

  public JOptionDialogComplete(Frame parent, XRDcat anObject) {

    super(parent);

    initializeSizeAndPosition(
        false, "JOptionDialogFrame.frameWidth", "JOptionDialogFrame.frameHeight", 400, 500,
        true, "JOptionDialogFrame.framePositionX", "JOptionDialogFrame.framePositionY", 50, 50);

    parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));

    theObject = anObject;

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout(6, 6));

    JTabbedPane p1 = new JTabbedPane();
    c1.add(p1, BorderLayout.CENTER);
    String p1String[] = {"Fields & Parameters",
                         "Models"};

    JPanel jp1 = getFieldsAndParametersPanel();
    p1.addTab(p1String[0], null, jp1);
    jp1 = getModelsPanel();
    p1.addTab(p1String[1], null, jp1);


    JPanel closebuttonPanel = new JPanel();
    closebuttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    c1.add(closebuttonPanel, BorderLayout.SOUTH);
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

    setTitle(theObject.toXRDcatString());
    initparameters();
    parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

    createDefaultMenuBar();

    pack();
  }

  JPanel getModelsPanel() {
    JPanel jp1 = new JPanel();
    jp1.setLayout(new BorderLayout(6, 6));

    JPanel jPanel12 = new JPanel();
    jPanel12.setLayout(new BorderLayout(2, 2));
    jp1.add(jPanel12, BorderLayout.CENTER);

    JPanel jPanel6 = new JPanel();
    jPanel6.setLayout(new GridLayout(0, 1, 1, 1));
    jPanel12.add(jPanel6, BorderLayout.WEST);

    String[] tmpStringS1 = {};

    for (int i = 0; i < theObject.Nsubordinate; i++)
      if (active[i])
        jPanel6.add(new JLabel(tmpStringS1[i]));

    JPanel jPanel8 = new JPanel();
    jPanel8.setLayout(new GridLayout(0, 1, 1, 1));
    jPanel12.add(jPanel8, BorderLayout.CENTER);

    optchoice = new JComboBox[theObject.Nsubordinate];
    initializeActives(theObject.Nsubordinate);

    for (int i = 0; i < theObject.Nsubordinate; i++) {
      if (active[i]) {
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
    }

    return jp1;

  }

  JPanel getFieldsAndParametersPanel() {
    JPanel jp1 = new JPanel();
    jp1.setLayout(new BorderLayout(6, 6));

    JPanel jPanel12 = new JPanel();
    jPanel12.setLayout(new BorderLayout(6, 6));
    jp1.add(jPanel12, BorderLayout.NORTH);

    JPanel jPanel6 = new JPanel();
    jPanel6.setLayout(new GridLayout(0, 1, 3, 3));
    jPanel12.add(jPanel6, BorderLayout.WEST);

    String[] tmpStringS = {"Object name:"};
    for (int i = 0; i < tmpStringS.length; i++)
      jPanel6.add(new JLabel(tmpStringS[i]));

    JPanel jPanel8 = new JPanel();
    jPanel8.setLayout(new GridLayout(0, 1, 3, 3));
    jPanel12.add(jPanel8, BorderLayout.CENTER);

    theObjectTF = new JTextField(24);
    jPanel8.add(theObjectTF);

    jPanel12 = new JPanel();
    jPanel12.setLayout(new BorderLayout(2, 2));
    jp1.add(jPanel12, BorderLayout.CENTER);

    jPanel6 = new JPanel();
    jPanel6.setLayout(new GridLayout(0, 1, 1, 1));
    jPanel12.add(jPanel6, BorderLayout.WEST);

    return jp1;

  }


  public XRDcat getData() {
    return theObject;
  }

  void initializeActives(int number) {
    active = new boolean[number];
    for (int i = 0; i < number; i++)
      active[i] = true;
  }

  void initparameters() {
    int i, j;

    theObjectTF.setText(getData().getLabel());
//        intensityTF.setText(getData().getIntensity().getValue());
//        addComponenttolist(intensityTF, theinstrument.getIntensity());

//      ThetaPanel.setList(theinstrument, 0);

    for (i = 0; i < getData().Nsubordinate; i++) { //absorption removed here
      if (active[i]) {
      for (j = 0; j < getData().getsubordClassNumber(i); j++)
        optchoice[i].addItem(getData().getsubordIdentifier(i, j));
      optchoice[i].setSelectedItem(getData().subordinateField[i].identifier);
      }
    }

  }

  public void retrieveParameters() {
    super.retrieveParameters();

    getData().setLabel(theObjectTF.getText());
//        getData().getIntensity().setValue(intensityTF.getText());

//      ThetaPanel.retrieveparlist();

    for (int i = 0; i < getData().Nsubordinate; i++) {
      if (active[i]) {
      String value = optchoice[i].getSelectedItem().toString();
      if (getData().subordinateField[i] == null ||
          !value.equals(getData().subordinateField[i].identifier))
        getData().setsubordinateField(i, value);
      }
    }
  }

  public void subordinateOptions(int index) {
    String value = optchoice[index].getSelectedItem().toString();
    if (getData().subordinateField[index] == null ||
        !value.equals(getData().subordinateField[index].identifier))
      getData().setsubordinateField(index, value);

    getData().subordinateField[index].getOptionsDialog(this).setVisible(true);
  }

  public void dispose() {
    for (int i = 0; i < getData().Nsubordinate; i++) {  //absorption removed here
      if (active[i])
        optchoice[i].removeAllItems();
    }
    optchoice = null;
    theObject = null;
//      ThetaPanel.dispose();

    super.dispose();
  }

}
