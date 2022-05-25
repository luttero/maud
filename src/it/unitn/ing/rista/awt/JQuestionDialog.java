/*
 * @(#)JQuestionDialog.java created 02/10/2002 Mesiano
 *
 * Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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
import java.net.URL;

import it.unitn.ing.rista.util.*;

/**
 * The JQuestionDialog is a dialog class providing a general question window.
 *
 * @version $Revision: 1.2 $, $Date: 1970/01/04 19:14:43 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class JQuestionDialog extends myJDialog {
  static int BOOLEAN = 0;
  static int STRING = 1;
  static int PASSWORD = 2;
  boolean response = false;
  String responseString = "";
  JTextField stringTF = null;
  JPasswordField psswdTF = null;

  public JQuestionDialog(Frame parent, String message, int type) {
    super(parent, message, true);

    Container c1 = this.getContentPane();
    c1.setLayout(new BorderLayout());
    JPanel p1 = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 6));
    c1.add(BorderLayout.CENTER, p1);
    JLabel label1 = new JLabel(message);
    label1.setIcon(new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + "Inform.gif")));
    p1.add(label1);
    if (type == STRING) {
      stringTF = new JTextField(16);
      stringTF.setText("");
      p1.add(stringTF);
    } else if (type == PASSWORD) {
      psswdTF = new JPasswordField(16);
      psswdTF.setText("");
      p1.add(psswdTF);
    }

    JPanel p4 = new JPanel();
    p4.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
    c1.add(BorderLayout.SOUTH, p4);
    JButton exit = new JCancelButton();
    p4.add(exit);
    exit.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        setVisible(false);
        response = false;
      }
    });

    exit = new JCloseButton();
    p4.add(exit);
    if (type == STRING) {
      exit.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          responseString = stringTF.getText();
          setVisible(false);
        }
      });
    } else if (type == PASSWORD) {
      exit.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          responseString = new String(psswdTF.getPassword());
          setVisible(false);
        }
      });
    } else {
      exit.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          setVisible(false);
          response = true;
        }
      });
    }

    pack();
    setResizable(false);
    getRootPane().setDefaultButton(exit);
//    Misc.centerOnFrame(this, parent);
  }

  public static boolean getResponse(Frame parent, String question) {
    boolean finalResponse = false;
    final JQuestionDialog cfd = new JQuestionDialog(parent, question, BOOLEAN);
    cfd.setVisible(true);

//    (new Thread() {
//      public void run() {
    do {
      try {
        Thread.currentThread().sleep(300);
      } catch (InterruptedException ie) {
      }
    } while (cfd.isVisible());
    finalResponse = cfd.response;
    cfd.dispose();
    return finalResponse;
//      }
//    }).start();
  }

  public static String getString(Frame parent, String question) {
    String finalResponse = "";
    final JQuestionDialog cfd = new JQuestionDialog(parent, question, STRING);
    cfd.setVisible(true);

//    (new Thread() {
//      public void run() {
    do {
      try {
        Thread.currentThread().sleep(300);
      } catch (InterruptedException ie) {
      }
    } while (cfd.isVisible());
    finalResponse = cfd.responseString;
    cfd.dispose();
    return finalResponse;
//      }
//    }).start();
  }

  public static String getPassword(Frame parent, String question) {
    String finalResponse = "";
    final JQuestionDialog cfd = new JQuestionDialog(parent, question, PASSWORD);
    cfd.setVisible(true);

//    (new Thread() {
//      public void run() {
    do {
      try {
        Thread.currentThread().sleep(300);
      } catch (InterruptedException ie) {
      }
    } while (cfd.isVisible());
    finalResponse = cfd.responseString;
    cfd.dispose();
    return finalResponse;
//      }
//    }).start();
  }

}

