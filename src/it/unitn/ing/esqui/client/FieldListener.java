package it.unitn.ing.esqui.client;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/** FieldListener.java
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

public abstract class FieldListener implements FocusListener {

  JFrame tmpFrame = null;
  JWindow tmpWindow = null;
  JRootPane tmpRootPane = null;
  JTextField tmpTextField = null;

  public FieldListener(JFrame frame) {
    tmpFrame = frame;
    tmpRootPane = tmpFrame.getRootPane();
  }

  public FieldListener(JWindow window) {
    tmpWindow = window;
    tmpRootPane = tmpWindow.getRootPane();
  }

  public void focusGained(FocusEvent e) {
    tmpTextField = (JTextField) e.getSource();
    tmpRootPane.setDefaultButton(null);
  }

  public void focusLost(FocusEvent e) {
  }
}

