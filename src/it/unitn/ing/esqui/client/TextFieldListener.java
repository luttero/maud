package it.unitn.ing.esqui.client;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/** TextFieldListener.java
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

public class TextFieldListener extends FieldListener {

  public TextFieldListener(JFrame frame) {
    super(frame);
  }

  public TextFieldListener(JWindow window) {
    super(window);
  }
}
