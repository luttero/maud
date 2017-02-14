/*
 * @(#)JStepValueEdit.java created Dec 4, 2004 Casalino
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.util.Fmt;

import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.*;


/**
 * The JStepValueEdit is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2005/09/16 15:47:23 $
 * @since JDK1.1
 */

public class JStepValueEdit extends JPanel {

  JTextField textField = null;
  JButton upButton = null;
  JButton downButton = null;
  JTextField stepField = null;
  double step_perc = 0.0;
  Frame frameParent = null;
  boolean hasStepField = true; // modified by LL 9/9/05

  public JStepValueEdit(Frame aFrame) {
    this(aFrame, false);
  }

  public JStepValueEdit(Frame aFrame, JTextField errorField) {
    this(aFrame, false);
    if (errorField != null)
      stepField = errorField;
  }

  public JStepValueEdit(Frame aFrame, boolean up_and_down) {
    frameParent = aFrame;
//    hasStepField = up_and_down;
    if (up_and_down) {
      setLayout(new BorderLayout(0, 0));
      textField = new JTextField(8);
      add(BorderLayout.WEST, textField);
      JPanel buttonPanel = new JPanel(new BorderLayout(0, 0));
      add(BorderLayout.CENTER, buttonPanel);
      upButton = new JIconButton("UpSmall.gif");
      upButton.setMinimumSize(new Dimension(20, 12));
      upButton.setMargin(new Insets(1, 1, 0, 0));
      upButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          upButton_action();
        }
      });
      downButton = new JIconButton("DownSmall.gif");
      downButton.setMinimumSize(new Dimension(20, 12));
      downButton.setMargin(new Insets(1, 1, 0, 0));
      downButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          downButton_action();
        }
      });
      buttonPanel.add(BorderLayout.NORTH, upButton);
      buttonPanel.add(BorderLayout.SOUTH, downButton);
      stepField = new JTextField(8);
      add(BorderLayout.EAST, stepField);
    } else {
      setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));

      downButton = new JIconButton("LeftSmall.gif");
      downButton.setMinimumSize(new Dimension(12, 20));
      downButton.setMargin(new Insets(1, 1, 1, 1));
      downButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          downButton_action();
        }
      });
      add(downButton);

      textField = new JTextField(8);
      add(textField);

      upButton = new JIconButton("RightSmall.gif");
      upButton.setMinimumSize(new Dimension(12, 20));
      upButton.setMargin(new Insets(1, 1, 1, 1));
      upButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          upButton_action();
        }
      });
      add(upButton);

      if (hasStepField) { // modified by LL 9/9/05
        stepField = new JTextField(8);
        add(stepField);
      }
    }
  }

  double defaultStep = 0.0001;

  public void setText(String s) {
    textField.setText(s);
    if (step_perc == 0.0) {
      double s_value = Double.parseDouble(s);
      if (s_value == 0.0)
        step_perc = 0.0001;
      else
        step_perc = s_value / 1000.0;
    }
    defaultStep = step_perc;
    if (hasStepField)
      stepField.setText(Fmt.format(step_perc));
  }

  public String getText() {
    return textField.getText();
  }

  public void upButton_action() {
    double ds = Double.parseDouble(getText());
    step_perc = Double.parseDouble(stepField.getText());
    if (step_perc == 0.0)
      step_perc = defaultStep;
    ds += step_perc;
    String newValue = Fmt.format(ds);
    textField.setText(newValue);
    if (frameParent instanceof DiffractionMainFrame) {
      ((DiffractionMainFrame) frameParent).setValueAt(newValue);
    }
  }

  public void downButton_action() {
    double ds = Double.parseDouble(getText());
    step_perc = Double.parseDouble(stepField.getText());
    if (step_perc == 0.0)
      step_perc = defaultStep;
    ds -= step_perc;
    String newValue = Fmt.format(ds);
    textField.setText(newValue);
    if (frameParent instanceof DiffractionMainFrame) {
      ((DiffractionMainFrame) frameParent).setValueAt(newValue);
    }
  }

  public void addActionListener(ActionListener listener) {
    textField.addActionListener(listener);
  }

}

