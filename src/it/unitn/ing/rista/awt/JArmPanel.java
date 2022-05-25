/*
 * @(#)JArmPanel.java created 01/01/1997 Mesiano
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

import java.awt.*;
import javax.swing.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.FilePar;


/**
 * The JArmPanel is a class
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:02 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class JArmPanel extends JPanel {
  //insert class definition here

  JRadioButton filteredRB;
  JRadioButton monochromatorRB;
  JTextField angleTF;
  JPanel angleP;
  public boolean mon = false;
  String lastAngle;
  Frame theparent = null;

  public JArmPanel(Frame parent) {
    super();
    setFrameParent(parent);

    setLayout(new BorderLayout());
    JPanel jPanel1 = new JPanel();
    jPanel1.setLayout(new GridLayout(0, 1, 6, 6));
    add("West", jPanel1);
    ButtonGroup rbg = new ButtonGroup();
    filteredRB = new JRadioButton("Filtered radiation");
    filteredRB.setSelected(true);
    filteredRB.setToolTipText("Select this if the monochromator is not present in this arm");
    jPanel1.add(filteredRB);
    rbg.add(filteredRB);
    monochromatorRB = new JRadioButton("Monochromator");
    monochromatorRB.setToolTipText("Select this if the monochromator is present in this arm");
    jPanel1.add(monochromatorRB);
    rbg.add(monochromatorRB);
    JPanel jPanel2 = new JPanel();
    jPanel2.setLayout(new BorderLayout(0, 0));
    add("South", jPanel2);
    angleP = new JPanel();
    angleP.setLayout(new FlowLayout());
    jPanel2.add("East", angleP);
    angleP.add(new JLabel("Angle (2x):"));
    angleTF = new JTextField(Constants.FLOAT_FIELD);
    angleTF.setText("0");
    angleTF.setToolTipText("Insert the monochromator angle here (the double angle in degrees)");
    angleP.add(angleTF);

  }

  public void setFrameParent(Frame parent) {
    theparent = parent;
  }

  public Frame getFrameParent() {
    return theparent;
  }

  public FilePar getFileParent() {
    Frame aparent = getFrameParent();
    while (aparent != null && !(aparent instanceof principalJFrame)) {
      aparent = ((ParentFrame) aparent).getFrameParent();
    }
    if (aparent != null)
      return ((ParentFrame) aparent).getFileParent();
    else
      return null;
  }

  public JRadioButton getfilteredRB() {
    return filteredRB;
  }

  public JRadioButton getmonochromatorRB() {
    return monochromatorRB;
  }

  public JTextField getangleTF() {
    return angleTF;
  }

  public JPanel getangleP() {
    return angleP;
  }

  public void initFiltered() {
    mon = true;
    setFiltered();
    filteredRB.setSelected(true);
    angleP.setVisible(false);
  }

  public void setFiltered() {
    if (mon) {
      lastAngle = new String(angleTF.getText());
      angleTF.setText("0");
      angleP.setVisible(false);
      mon = false;
      packAgain();
    }
  }

  public void initMonochromator(String astring) {
    mon = false;
    setMonochromator(astring);
    monochromatorRB.setSelected(true);
  }

  public void setMonochromator(String astring) {
    if (!mon) {
      angleTF.setText(astring);
      angleP.setVisible(true);
      mon = true;
      packAgain();
    }
  }

  public void packAgain() {
    getFrameParent().pack();
  }

  Item lItem = null;

  public void initListener() {
    lItem = new Item();
    filteredRB.addItemListener(lItem);
    monochromatorRB.addItemListener(lItem);
  }

  public void removeListener() {
    filteredRB.removeItemListener(lItem);
    monochromatorRB.removeItemListener(lItem);
    lItem = null;
  }

  public String getAngle() {
    return angleTF.getText();
  }

  class Item implements java.awt.event.ItemListener {
    public void itemStateChanged(java.awt.event.ItemEvent event) {
      Object object = event.getSource();
      if (object == filteredRB && filteredRB.isSelected())
        setFiltered();
      else if (object == monochromatorRB && monochromatorRB.isSelected())
        setMonochromator(lastAngle);
    }
  }

}
