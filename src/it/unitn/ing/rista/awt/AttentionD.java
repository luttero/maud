/*
 * @(#)AttentionD.java created 1/01/1997 ?
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

import it.unitn.ing.rista.util.*;

/**
 * The AttentionD is a dialog class providing a general attention window.
 *
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:52 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class AttentionD extends myJDialog {

  public AttentionD(Frame parent, String title, String message, String iconImage,
                    boolean modal) {
    super(parent, title, modal);

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout());
    if (message.length() > 72) {
      JTextArea tarea = new JTextArea(message);
      c1.add(tarea, BorderLayout.CENTER);
    } else {
    JLabel label1 = new JLabel(message);
    if (iconImage != null)
      label1.setIcon(new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + iconImage)));
    c1.add("Center", label1);
    }

    Utility.centerOnScreen(this);
    pack();
  }

  public AttentionD(Frame parent) {
    this(parent, "Attention", "Event", null, true);
    addOkCancelButton(null);
  }

  public AttentionD(Frame parent, boolean modal) {
    this(parent, "Attention", "Event", modal);
    addOkCancelButton(null);
  }

  // Add a constructor for Interactions (ignoring modal)
  public AttentionD(Frame parent, String title, String message, String iconImage) {
    this(parent, title, message, iconImage, true);
    addOkCancelButton(null);
  }

  public AttentionD(Frame parent, String message) {
    this(parent, "Attention!", message, "Caution.gif", true);
    addOkCancelButton(null);
  }

  public AttentionD(String message) {
    this(new Frame(), "Attention!", message, "Caution.gif", true);
    addOkCancelButton(null);
  }

  public AttentionD(String message, int mode) {
    this(new Frame(), "Attention!", message, "Caution.gif", true);
    addCloseButton(null);
  }

  public AttentionD(String title, String message) {
    this(new Frame(), title, message, "Caution.gif", true);
    addOkCancelButton(null);
  }

  public AttentionD(Frame parent, String title, String message) {
    this(parent, title, message, null, true);
    addOkCancelButton(null);
  }

  public AttentionD(Frame parent, String title, String message, boolean modal) {
    this(parent, title, message, null, modal);
    addOkCancelButton(null);
  }

  public AttentionD(Frame parent, String message, boolean modal) {
    this(parent, "Attention", message, null, modal);
    addOkCancelButton(null);
  }

  public AttentionD(Frame parent, JButton okButton) {
    this(parent, "Attention", "Event", null, true);
    addOkCancelButton(okButton);
  }

  public AttentionD(Frame parent, boolean modal, JButton okButton) {
    this(parent, "Attention", "Event", modal);
    addOkCancelButton(okButton);
  }

  // Add a constructor for Interactions (ignoring modal)
  public AttentionD(Frame parent, String title, String message, String iconImage, JButton okButton) {
    this(parent, title, message, iconImage, true);
    addOkCancelButton(okButton);
  }

  public AttentionD(Frame parent, String title, String message, JButton okButton) {
    this(parent, title, message, null, true);
    addOkCancelButton(okButton);
  }

  public AttentionD(Frame parent, String title, String message, boolean modal, JButton okButton) {
    this(parent, title, message, null, modal);
    addOkCancelButton(okButton);
  }

  public AttentionD(String title, String message, boolean modal, JButton okButton) {
    this(new Frame(), title, message, null, modal);
    addOkCancelButton(okButton);
  }

  public AttentionD(Frame parent, String message, boolean modal, JButton okButton) {
    this(parent, "Attention", message, null, modal);
    addOkCancelButton(okButton);
  }

  public AttentionD(Frame parent, String message, boolean modal, JButton okButton, boolean defaultButton) {
    this(parent, "Attention", message, null, modal);
    addOkCancelButton(okButton, null, defaultButton);
  }

  public AttentionD(Frame parent, String message, JButton okButton) {
    this(parent, "Attention", message, null, true);
    addOkCancelButton(okButton);
  }

  public AttentionD(Frame parent, String title, String message, boolean modal, JButton okButton, JButton cancelButton) {
    this(parent, title, message, null, modal);
    addOkCancelButton(okButton, cancelButton, false);
  }

  public void addOkCancelButton(JButton okButton) {
    addOkCancelButton(okButton, null, false);
  }

  public void addOkCancelButton(JButton okButton, JButton cancelButton) {
    addOkCancelButton(okButton, cancelButton, false);
  }

  public void addOkCancelButton(JButton okButton, JButton cancelButton, boolean defaultButton) {
    JPanel p1 = new JPanel();
    p1.setLayout(new FlowLayout(FlowLayout.RIGHT, 5, 5));
    getContentPane().add("South", p1);
    if (cancelButton == null)
      cancelButton = new JCancelButton();
    p1.add(cancelButton);
    cancelButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        setVisible(false);
        dispose();
      }
    });

    if (okButton != null)
      p1.add(okButton);
    getRootPane().setDefaultButton(okButton);
    pack();
  }

  public void addCloseButton(JButton okButton) {
    JPanel p1 = new JPanel();
    p1.setLayout(new FlowLayout(FlowLayout.RIGHT, 5, 5));
    getContentPane().add("South", p1);
    JButton cancelButton = new JCloseButton();
    p1.add(cancelButton);
    cancelButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        setVisible(false);
        dispose();
      }
    });

    if (okButton != null)
      p1.add(okButton);
    pack();
  }

  public static void showAlertDialog(Frame parent, String message) {
    (new AttentionD(parent, message)).setVisible(true);
  }

}

