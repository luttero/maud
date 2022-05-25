package it.unitn.ing.esqui.server;

import it.unitn.ing.rista.awt.MaudProgressBar;

import java.awt.*;
import javax.swing.*;

/** ProgressFrame.java
 * <br>
 * Title:			<b>ESQUI Progress Frame</b>
 * </br>
 * Description:	Class for creating and controlling a progress bar
 * @author:			Leonardo Cont, December 2000
 * @revision:		February 2001
 * @comment:		none
 */

class ProgressFrame extends JFrame {

//  MaudProgressBar bar = null;
  JProgressBar bar = null;

  public ProgressFrame(String title, int maximum) {
    super(title);
//    bar = new MaudProgressBar(0, maximum);
    bar = new JProgressBar(0, maximum);
    bar.setPreferredSize(new Dimension(300, 20));
    getContentPane().add(bar, BorderLayout.CENTER);
    pack();
    ServerMisc.locateOnScreen(this, 50, 50);
    setVisible(true);
  }

  public void setBarValue(int value) {
    bar.setValue(value);
  }
}
