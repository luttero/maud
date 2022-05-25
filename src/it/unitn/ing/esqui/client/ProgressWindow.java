package it.unitn.ing.esqui.client;

import it.unitn.ing.rista.awt.MaudProgressBar;

import java.awt.*;
import javax.swing.*;

/** ProgressWindow.java
 * <br>
 * Title:			<b>ESQUI Progress Window</b>
 * </br>
 * Description:	Class for creating and controlling a progress bar
 * @author:			Leonardo Cont, December 2000
 * @revision:		February 2001
 * @comment:		none
 */

class ProgressWindow extends InfoWindow {

//  MaudProgressBar bar = null;
  JProgressBar abar = null;

  public ProgressWindow(Window owner, String title, int maximum) {
    super(owner, title);
//    bar = new MaudProgressBar(0, maximum);
//    bar.setPreferredSize(new Dimension(300, 20));
    abar = new JProgressBar(0, maximum);
    abar.setPreferredSize(new Dimension(300, 20));
    tmpPanel.add(abar, BorderLayout.CENTER);

    showWindow();
  }

  public void setBarValue(int value) {
    abar.setValue(value);
  }

  public void resetBar() {
    abar.setValue(0);
  }
}
