/*
 * @(#)licenseD.java created 1/01/1997 ?
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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
import java.awt.event.*;
import java.awt.FlowLayout;
import java.net.URL;

import it.unitn.ing.rista.util.*;

/**
 * The licenseD is a class that implement a dialog to display the registration box.
 *
 * @version $Revision: 1.2 $, $Date: 1970/01/04 19:14:44 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class licenseD extends myJDialog {

  JTextField licenseTF;
  JTextField passwdTF;

  public licenseD(Frame parent) {
    super(parent, title, true);

//		createDefaultMenuBar();

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout());
    JLabel label1 = new JLabel(label0);
    c1.add("North", label1);

    JPanel p1 = new JPanel();
    p1.setLayout(new GridLayout(3, 1, 5, 5));
    c1.add("Center", p1);
    JPanel p2 = new JPanel();
    p2.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
    p2.add(new JLabel(label2));
    licenseTF = new JTextField(25);
    p2.add(licenseTF);
    p1.add(p2);
    p2 = new JPanel();
    p2.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
    p2.add(new JLabel(label3 + Double.toString(Constants.maud_version)));
    p1.add(p2);
    p2 = new JPanel();
    p2.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
    p2.add(new JLabel(label4));
    passwdTF = new JTextField(25);
    p2.add(passwdTF);
    p1.add(p2);

    p1 = new JPanel();
    p1.setLayout(new FlowLayout(FlowLayout.RIGHT, 5, 5));
    c1.add("South", p1);
    JButton closeButton = new JCloseButton();
    p1.add(closeButton);
    closeButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        register();
        setVisible(false);
        dispose();
      }
    });
    getRootPane().setDefaultButton(closeButton);

    pack();
  }

  public void register() {
    String first = licenseTF.getText();
    String second = passwdTF.getText();
    if (first.equals(""))
      first = new String("0");
    if (second.equals(""))
      second = new String("0");
    long programID = Long.valueOf(first).longValue();
    long passwd = Long.valueOf(second).longValue();
//		it.unitn.ing.rista.util.Diffraction.registerMaud(programID, passwd);
  }

}

