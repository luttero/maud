package it.unitn.ing.esqui.client;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/** ParameterFieldListener.java
 * <br>
 * Title:			<b>ESQUI Client FieldListener</b>
 * </br>
 * Description:	Class to remove the focus from the
 default button when entering a textfield and
 do other useful actions
 * @author:			Leonardo Cont, December 2000
 * @revision:		January 2001
 * @comment:		none
 */

public class ParameterFieldListener extends FieldListener {

  public ParameterFieldListener(JFrame frame) {
    super(frame);
  }

  public void focusGained(FocusEvent e) {
    tmpTextField = (JTextField) e.getSource();
    tmpTextField.setBackground(Color.white);
  }

  public void focusLost(FocusEvent e) {
    tmpTextField = (JTextField) e.getSource();
    String textfieldText = tmpTextField.getText();
    if (textfieldText.trim().equals("")) {
      tmpTextField.setBackground(Color.red);
      tmpTextField.setText("******");
    }
  }
}

