/*
 * @(#)LabelD.java created 1/01/1997 xxx
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


/**
 * The LabelD is a class that implement a dialog to change the atom site label.
 *
 * @version $Revision: 1.2 $, $Date: 1970/01/04 19:14:43 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class LabelD extends myJDialog {

  JLabel label1;
  JTextField textField1;
  public JButton jbok;

  public LabelD(Frame parent, String title, boolean modal) {
    super(parent, title, false);

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout(6, 6));

    JPanel tfPanel = new JPanel();
    c1.add(BorderLayout.CENTER, tfPanel);
    tfPanel.add(label1 = new JLabel("Label:"));
    tfPanel.add(textField1 = new JTextField(24));

    JPanel jp = new JPanel();
    jp.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
    c1.add(BorderLayout.SOUTH, jp);
    JButton button2 = new JCancelButton();
    jp.add(button2);
    button2.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        dispose();
      }
    });
    jbok = new JCloseButton();
    jp.add(jbok);
    getRootPane().setDefaultButton(jbok);
    pack();
    setVisible(true);

  }

  public void setLabel(String alabel) {
//		label1.setLabel(alabel);
    pack();
  }

  public void setTextField(String alabel) {
    textField1.setText(alabel);
    pack();
  }

  public String getTextField() {
    return new String(textField1.getText());
  }
}
