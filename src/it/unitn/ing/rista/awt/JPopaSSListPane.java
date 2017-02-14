/*
 * @(#)JPopaSSListPane.java created 22/10/1998 Mesiano
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
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
import java.awt.event.*;
import javax.swing.event.*;
import javax.swing.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.FilePar;

import javax.swing.border.*;

/**
 *  The JPopaSSListPane is a class
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class JPopaSSListPane extends JParListPane {

  public JSlider expansionJS;
  public JLabel expansionTF;

  public JPopaSSListPane(Frame parent, boolean showTotal) {
    super(parent);

    JPanel jp1, jp2, jp3;

    setLayout(new BorderLayout(6, 6));
    jp1 = new JPanel();
    jp1.setLayout(new BorderLayout(6, 6));
    add("Center", jp1);
    if (showTotal) {
      jp2 = new JPanel();
      jp2.setLayout(new BorderLayout(6, 6));
      jp1.add("North", jp2);
      jp3 = new JPanel();
      jp3.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));
      jp2.add("East", jp3);
      jp3.add(new JLabel("Total parameter:"));
      totTF = new JTextField(4);
      totTF.setEditable(false);
      totTF.setText("0");
      jp3.add(totTF);
    }
    thelist = new JList();
    thelist.setVisibleRowCount(4);
    thelist.setPrototypeCellValue("123456789012345678901234567890");
    JScrollPane sp1 = new JScrollPane();
//		sp1.setBorder(new LineBorder(Color.black));
    sp1.getViewport().add(thelist);
    jp1.add("North", sp1);
    jp2 = new JPanel();
    jp2.setLayout(new FlowLayout());
    jp1.add("South", jp2);
    jp2.add(new JLabel("Lmax: "));
    expansionTF = new JLabel("00");
    jp2.add(expansionTF);
    expansionJS = new JSlider();
    expansionJS.setToolTipText("Set the expansion degree (Lmax) for the spherical surface harmonics");
    jp2.add(expansionJS);
    jp2 = new JPanel();
    jp2.setLayout(new BorderLayout(6, 6));
    jp1.add("Center", jp2);
    jp3 = new JPanel();
    jp3.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));
    jp2.add("Center", jp3);
    jp3.add(new JLabel("Value:"));
    firstvalueTF = new JTextField(Constants.FLOAT_FIELD);
    firstvalueTF.setText("0");
    jp3.add(firstvalueTF);

  }

  public void setExpansionSlider(int min, int max) {
    expansionJS.setMaximum(max);
    expansionJS.setMinimum(min);
    expansionJS.setPaintTicks(true);
    expansionJS.setMajorTickSpacing(10);
    expansionJS.setMinorTickSpacing(2);

    expansionJS.setValue(max);

    expansionJS.setPaintLabels(true);
    expansionJS.setSnapToTicks(true);

    expansionJS.setLabelTable(expansionJS.createStandardLabels(4));
  }

  public void initListener() {
    super.initListener();
    SliderListener listener = new SliderListener(expansionTF);
    expansionJS.addChangeListener(listener);
  }

  public void setSliderValue(int value) {
    expansionJS.setValue(value);
  }

  public void expansionHasChanged(int value) {
  }

  class SliderListener implements ChangeListener {
    JLabel tf;

    public SliderListener(JLabel f) {
      tf = f;
    }

    public void stateChanged(ChangeEvent e) {
      JSlider s1 = (JSlider) e.getSource();
      int value = s1.getValue();
      tf.setText(Integer.toString(value));
      expansionHasChanged(value);
    }
  }

}
