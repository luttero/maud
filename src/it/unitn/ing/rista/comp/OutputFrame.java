/*
 * @(#)OutputFrame.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.comp;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.Constants;

import javax.swing.*;
import java.awt.*;

/**
 * The OutputFrame is a class
 *
 * @version $Revision: 1.7 $, $Date: 2006/01/19 14:45:54 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class OutputFrame extends myJFrame implements OutputFrameI {

  JTextArea textarea = null;
  ProgressPanel pcontrol = null;
  JScrollPane scrollarea = null;
  JPanel buttonArea = null;

  public OutputFrame(Frame parent) {
    super(parent);

    frameWLabel = "outputFrame.frameWidth";
    frameHLabel = "outputFrame.frameHeight";
    defaultFrameW = 500;
    defaultFrameH = 400;
    setOwnSize = true;
    framePositionX = "outputFrame.framePositionX";
    framePositionY = "outputFrame.framePositionY";
    defaultFramePositionX = 400;
    defaultFramePositionY = 300;
    setOwnPosition = true;

    createDefaultMenuBar();

    getContentPane().setLayout(new BorderLayout());
    buttonArea = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
    getContentPane().add(BorderLayout.NORTH, buttonArea);
    scrollarea = new JScrollPane(textarea = new JTextArea());
    getContentPane().add(BorderLayout.CENTER, scrollarea);
    textarea.setAutoscrolls(true);

    textarea.setEditable(false);

//    setComponentToPrint(textarea);

  }

  public OutputFrame(Frame parent, int maxprogressindex) {
    super(parent);

    frameWLabel = "outputFrame.frameWidth";
    frameHLabel = "outputFrame.frameHeight";
    defaultFrameW = 550;
    defaultFrameH = 500;
    setOwnSize = true;
    framePositionX = "outputFrame.framePositionX";
    framePositionY = "outputFrame.framePositionY";
    defaultFramePositionX = 400;
    defaultFramePositionY = 300;
    setOwnPosition = true;

    createDefaultMenuBar();

    getContentPane().setLayout(new BorderLayout());
    scrollarea = new JScrollPane(textarea = new JTextArea());
//		scrollarea.setBorder(new LineBorder(Color.black));

    buttonArea = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
    getContentPane().add(BorderLayout.NORTH, buttonArea);
    getContentPane().add(BorderLayout.CENTER, scrollarea);
    textarea.setEditable(false);
    textarea.setAutoscrolls(true);

    pcontrol = new ProgressPanel(maxprogressindex);
    pcontrol.setProgressText("Starting iterations....");
    getContentPane().add(BorderLayout.SOUTH, pcontrol);

  }

  public void addComponent(JComponent aComponent) {
    buttonArea.add(aComponent);
  }

  public void reset() {
    textarea.setText("");
  }

  public void append(String atext) {
    textarea.append(atext);
    setVisible(true);
  }

  public void appendnewline(String atext) {
    textarea.append(atext);
    newline();
  }

  public void newline() {
    textarea.append("\n");
    setVisible(true);
    scrollToEnd();
  }

  public void increaseProgressBarValue() {
    if (pcontrol != null) {
      pcontrol.increaseValue();
    }
  }

  public void resizeProgressBarMaximum(int maximum) {
    if (pcontrol != null) {
      pcontrol.resizeProgressBarMaximum(maximum);
    }
  }

  public ProgressPanel getProgressBar() {
    return pcontrol;
  }

  public void setProgressText(String value) {
    if (pcontrol != null)
      pcontrol.setProgressText(value);
    else
      appendnewline(value);

  }

  public void computationStopped() {
    setProgressText("Computation stopped by user, waiting to finish current task...");
  }

  public void iterationStopped() {
    setProgressText("Iterations stopped by user, waiting to finish current one...");
  }

  public void computationPaused() {
    setProgressText("Computation slowed-down by user, press the wake-up button to speed-up...");
  }

  public void computationResumed() {
    setProgressText("Computation resumed by user....");
  }

  public void iterationResumed() {
    setProgressText("Iterations resumed by user...");
  }

  public void hideprogressBar() {
    if (pcontrol != null)
      pcontrol.setVisible(false);
  }

  public void scrollToEnd() {
    if (!Constants.windoze)
      scrollarea.getVerticalScrollBar().setValue(scrollarea.getVerticalScrollBar().getMaximum());
  }

}
