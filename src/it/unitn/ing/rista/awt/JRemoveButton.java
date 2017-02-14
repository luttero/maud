/*
 * @(#)JRemoveButton.java created 19/02/2000 Tione (TN)
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
 *  The JRemoveButton is a class to implement a remove button with
 *  an icon and a method to display a dialog for action confirmation.
 *  The class has been created to avoid problems due to unpredictable Rudy's mouse
 *  clicks over the remove buttons.
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/11/18 09:30:48 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class JRemoveButton extends JIconButton {

//  boolean result = true;

  public JRemoveButton() {
    super("Delete.gif", "Remove");
  }

  public JRemoveButton(String removeString) {
    super("Delete.gif", removeString);
  }

  public JRemoveButton(String iconString, String removeString) {
    super(iconString, removeString);
  }

  public JRemoveButton(String icon, String text, String actionCommand, String toolTipText) {
    super(icon, text, actionCommand, toolTipText);
  }

/*  public boolean areYouSureToRemove(Frame aframe) {
    JButton removeButton = new JIconButton("Check.gif", "Remove");
    result = false;

    final AttentionD attdlg = new AttentionD(aframe,
            "Are you sure you want to remove this?", true, removeButton, true);
    removeButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        result = true;
        attdlg.setVisible(false);
        attdlg.dispose();
      }
    });
    attdlg.setVisible(true);

    while (attdlg.isVisible()) {
      try {
        Thread.sleep(100);
      } catch (InterruptedException ie) {
      }

    }

    return result;
  }*/
}
