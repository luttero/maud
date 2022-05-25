package it.unitn.ing.esqui.client;

import java.awt.*;
import javax.swing.*;

/** InfoWindow.java
 * <br>
 * Title:			<b>ESQUI Information Frame</b>
 * </br>
 * Description:	Abstract class for creating a general information window
 * @author:			Leonardo Cont, December 2000
 * @revision:		February 2001
 * @comment:		none
 */

abstract class InfoWindow extends JWindow {

  JPanel tmpPanel;

  public InfoWindow(Window owner, String title) {
//		super(owner);
    tmpPanel = new JPanel(new BorderLayout());
    tmpPanel.setBorder(ClientMisc.newBorder(null, 3));
    JTextField tmpLabel = new JTextField(title);
    tmpLabel.setHorizontalAlignment(JTextField.CENTER);
    tmpLabel.setEnabled(false);
    tmpLabel.setDisabledTextColor(Color.black);
    tmpLabel.setFont(new Font("Arial", Font.BOLD, 13));
    tmpLabel.setBackground(Color.yellow);
    tmpPanel.add(tmpLabel, BorderLayout.NORTH);
    getContentPane().add(tmpPanel, BorderLayout.CENTER);
  }

  void showWindow() {
    pack();
    ClientMisc.locateOnScreen(this, 50, 50);
    setVisible(true);
    requestFocus();
  }
}
