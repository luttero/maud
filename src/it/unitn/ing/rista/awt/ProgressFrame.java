/*
 * @(#)ProgressFrame.java created 04/08/1999 Mesiano
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.util.*;

import javax.swing.border.*;

/**
 * The ProgressFrame is a basic frame to show a progressBar.
 *
 * @version $Revision: 1.4 $, $Date: 2006/12/04 14:30:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class ProgressFrame extends JFrame {

  ProgressPanel[] pcontrol = null;

  public ProgressFrame(int maxprogressindex) {
//		super();

    getContentPane().setLayout(new FlowLayout(FlowLayout.CENTER, 3, 3));
    pcontrol = new ProgressPanel[1];
    pcontrol[0] = new ProgressPanel(maxprogressindex);
    pcontrol[0].setProgressText("Work in progress....");
    getContentPane().add(pcontrol[0]);

    pack();

    setVisible(true);

  }

  public ProgressFrame(int maxprogressindex, int numberOfProgressBar) {
//		super();


    getContentPane().setLayout(new GridLayout(0, 1));
    pcontrol = new ProgressPanel[numberOfProgressBar];
    for (int i = 0; i < numberOfProgressBar; i++) {
      JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
      pcontrol[i] = new ProgressPanel(maxprogressindex);
      pcontrol[i].setProgressText("Work in progress....");
      panel.add(pcontrol[i]);
      getContentPane().add(panel);
    }

    pack();

    setVisible(true);

  }

  public void increaseProgressBarValue() {
	  synchronized(this) {
      setVisible(true);
      if (pcontrol[0] != null)
        pcontrol[0].increaseValue();
	  }
  }

  public void decreaseProgressBarValue() {
    setVisible(true);
    if (pcontrol[0] != null)
      pcontrol[0].decreaseValue();
  }

  public void setProgressBar(int value) {
    if (pcontrol[0] != null)
      pcontrol[0].setProgressBar(value);
  }

  public void setProgressBarValue(int maximum) {
    if (pcontrol[0] != null)
      pcontrol[0].setProgressBarValue(maximum);
  }

  public ProgressPanel getProgressBar() {
    return pcontrol[0];
  }

  public void setProgressText(String value) {
    if (pcontrol[0] != null) {
      pcontrol[0].setProgressText(value);
      pack();
    }
  }

  public void setProgressTextNoResize(String value) {
    if (pcontrol[0] != null) {
      pcontrol[0].setProgressText(value);
    }
  }

  public void hideprogressBar() {
    if (pcontrol[0] != null)
      pcontrol[0].setVisible(false);
  }

  public void increaseProgressBarValue(int index) {
    if (pcontrol[index] != null)
      pcontrol[index].increaseValue();
  }

  public void setProgressBarValue(int maximum, int index) {
    if (pcontrol[index] != null)
      pcontrol[index].setProgressBarValue(maximum);
  }

  public ProgressPanel getProgressBar(int index) {
    return pcontrol[index];
  }

  public void setProgressText(String value, int index) {
    if (pcontrol[index] != null) {
      pcontrol[index].setProgressText(value);
      pack();
    }
  }

  public void setProgressTextNoResize(String value, int index) {
    if (pcontrol[index] != null) {
      pcontrol[index].setProgressText(value);
    }
  }

  public void hideprogressBar(int index) {
    if (pcontrol[index] != null)
      pcontrol[index].setVisible(false);
  }

}
