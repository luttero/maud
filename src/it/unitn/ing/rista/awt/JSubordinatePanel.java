/*
 * @(#)JSubordinatePanel.java created 01/01/2001 Casalino
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
import javax.swing.*;

import it.unitn.ing.rista.diffr.*;

/**
 * The JSubordinatePane is a class
 *
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:52 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class JSubordinatePanel extends JPanel {
  //insert class definition here

  JComboBox modelsCB = null;
  XRDcat rootObject = null;
  int identifierNumber = -1;
  Frame theParent = null;

  public JSubordinatePanel(Frame parent, XRDcat rootObject, int identifierNumber, String label, String tooltipString) {
    super();

    theParent = parent;

    this.rootObject = rootObject;
    this.identifierNumber = identifierNumber;

    setLayout(new FlowLayout(FlowLayout.CENTER, 3, 3));
    add(new JLabel(label + ": "));
    modelsCB = new JComboBox();
    modelsCB.setToolTipText(tooltipString);
    for (int i = 0; i < rootObject.subordinateModelsNumber(identifierNumber); i++)
      modelsCB.addItem(rootObject.getsubordIdentifier(identifierNumber, i));
    add(modelsCB);
    JButton jb = new JIconButton("Eyeball.gif", "Options");
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        options();
      }
    });
    add(jb);

    initParameters();
  }

  public void retrieveParameters() {
//		System.out.println(modelsCB.getSelectedItem().toXRDcatString());
    rootObject.setSubordinateModel(identifierNumber, modelsCB.getSelectedItem().toString());
  }

  public void initParameters() {
//		System.out.println("Setting: " + rootObject.getSubordinateModel(identifierNumber));
    modelsCB.setSelectedItem(rootObject.getSubordinateModel(identifierNumber));
    modelsCB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        if (event.getActionCommand().equals("comboBoxChanged"))
          retrieveParameters();
      }
    });
  }

  public void options() {
    rootObject.setSubordinateModel(identifierNumber, modelsCB.getSelectedItem().toString());
    rootObject.getActiveSubordinateModel(identifierNumber).getOptionsDialog(theParent).setVisible(true);
  }
}
