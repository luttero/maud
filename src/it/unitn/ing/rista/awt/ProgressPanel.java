/*
 * @(#)ProgressPanel.java created 20/5/1998 xxx
 *
 * Copyright (c) 1998 Luca Lutterotti. All Rights Reserved.
 *
 * This software is the confidential and proprietary information of Luca
 * Lutterotti, ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Sun.
 *
 * LUCA LUTTEROTTI MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY
 * OF THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. SUN SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.awt;

import javax.swing.*;
import javax.swing.text.*;
import javax.swing.border.*;
//import java.accessibility.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;


/**
 * Instantiate a progress bar using swing
 *
 * @version $Revision: 1.4 $, $Date: 2006/12/04 14:30:03 $
 * @author Luca Lutterotti
 */
public class ProgressPanel extends JPanel {

  JProgressBar progressBar;
  JTextArea progressTextArea;
  int maximum = 0;
  int minimum = 0;
  int actualValue = 0;

  public ProgressPanel(int maximum) {

    this.maximum = maximum;
    this.minimum = 0;

    setLayout(new BorderLayout());

    JPanel textWrapper = new JPanel(new BorderLayout());
//		textWrapper.setBorder(new BevelBorder(BevelBorder.LOWERED));
    textWrapper.setAlignmentX(LEFT_ALIGNMENT);
    progressTextArea = new MyTextArea();
//		progressTextArea.getAccessibleContext().setAccessibleName("Loading the default parameter file");
//		progressTextArea.getAccessibleContext().setAccessibleDescription("Here will display the loading data block");
    textWrapper.add(progressTextArea, BorderLayout.CENTER);

    add(textWrapper, BorderLayout.NORTH);

//    progressBar = new MaudProgressBar();
    progressBar = new JProgressBar();
//    progressBar.setOrientation(JProgressBar.HORIZONTAL);
//		progressBar.getAccessibleContext().setAccessibleName("Parameter file loading progress");
    add(progressBar, BorderLayout.CENTER);
    if (maximum >= 0) {
      progressBar.setMinimum(0);
      progressBar.setMaximum(maximum);
      progressBar.setValue(0);
    }
  }

  public Insets getInsets() {
    return new Insets(10, 10, 10, 10);
  }

  public void increaseValue() {
    ++actualValue;
    setProgressBar(actualValue);
  }

  public void decreaseValue() {
    --actualValue;
    setProgressBar(actualValue);
  }

  public void setProgressText(String atext) {
    progressTextArea.setText(atext);
  }

  public void setProgressBar(int value) {
    actualValue = value;
    if (value > maximum) {
      resizeProgressBarMaximum(value);
    }
    if (value >= minimum)
      progressBar.setValue(actualValue);
  }

  public void setProgressBarValue(int maximum) {
    actualValue = 0;
    this.maximum = maximum;
    progressBar.setMinimum(0);
    progressBar.setMaximum(maximum);
    progressBar.setValue(0);
  }

  public void resizeProgressBarMaximum(int maximum) {
    this.maximum = maximum;
    progressBar.setMaximum(maximum);
    progressBar.setValue(actualValue);
  }

  class MyTextArea extends JTextArea {
    public MyTextArea() {
      super(null, 0, 0);
      setEditable(false);
      setText("");
    }

    public float getAlignmentX() {
      return LEFT_ALIGNMENT;
    }

    public float getAlignmentY() {
      return TOP_ALIGNMENT;
    }
  }
}
