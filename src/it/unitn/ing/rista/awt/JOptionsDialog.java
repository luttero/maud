/*
 * @(#)JOptionsDialog.java created 01/01/1997 Mesiano
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

import it.unitn.ing.rista.diffr.*;

import java.awt.event.*;

/**
 * The JOptionsDialog is a class
 *
 * @version $Revision: 1.5 $, $Date: 2005/03/10 13:50:38 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class JOptionsDialog extends myJFrame {
  public XRDcat XRDparent = null;
  public JPanel principalPanel = null;

  public JOptionsDialog(Frame parent) {
    super(parent);
  }

  public JOptionsDialog(Frame parent, XRDcat obj) {
    this("OK", parent, obj);
  }

  public JOptionsDialog(String label, Frame parent, XRDcat obj) {

    super(parent);
    setOwnPosition = true;
    XRDparent = obj;

    createDefaultMenuBar();

    Container c1 = getContentPane();

    c1.setLayout(new BorderLayout(6, 6));
    principalPanel = new JPanel();
    c1.add(BorderLayout.CENTER, principalPanel);

    JPanel southpanel = new JPanel();
    southpanel.setLayout(new BorderLayout());
    c1.add(BorderLayout.SOUTH, southpanel);

    JPanel panel1 = new JPanel();
    panel1.setLayout(new FlowLayout(FlowLayout.LEFT, 6, 6));
    southpanel.add(BorderLayout.WEST, panel1);

    setHelpButton(panel1);

    panel1 = new JPanel();
    panel1.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
    southpanel.add(BorderLayout.EAST, panel1);

    JButton jb;
    panel1.add(jb = new JCancelButton());
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        setVisible(false);
        dispose();
      }
    });

    panel1.add(jb = new JCloseButton(label));
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        retrieveParameters();
        setVisible(false);
        dispose();
      }
    });
    getRootPane().setDefaultButton(jb);
  }

  public JOptionsDialog(Frame parent, XRDcat obj, String title) {
    this("OK", parent, obj);
    setTitle(title);
  }

  public void retrieveParameters() {
    super.retrieveParameters();
  }
}
