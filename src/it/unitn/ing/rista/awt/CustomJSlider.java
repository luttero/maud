/*
 * @(#)CustomJSlider.java created Sep 5, 2005 Casalino
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

import javax.swing.*;
import javax.swing.event.ChangeListener;
import javax.swing.event.ChangeEvent;


/**
 * The CustomJSlider is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2005/09/07 17:23:34 $
 * @since JDK1.1
 */

public class CustomJSlider extends JPanel {
  JSlider aslider = null;
  JLabel alabel = null;

  public CustomJSlider(String label, int min, int max) {
    alabel = new JLabel();
    add(alabel);
    aslider = new JSlider();
//      aslider.setToolTipText("Set the number of iterations during refinement (best values: 3-7)");
    SliderListener listener = new SliderListener(alabel);
    aslider.addChangeListener(listener);
    aslider.setMaximum(max);
    aslider.setMinimum(min);
    aslider.setValue(max);
//    aslider.setPaintTicks(true);
//    aslider.setMajorTickSpacing(5);
//    aslider.setMinorTickSpacing(1);
//    aslider.setPaintLabels(true);
//    aslider.setSnapToTicks(true);
//    aslider.setLabelTable(aslider.createStandardLabels(5));
    add(aslider);
  }

  public void setValue(int value) {
    aslider.setValue(value);
  }

  public int getValue() {
    return aslider.getValue();
  }

  class SliderListener implements ChangeListener {
    JLabel tf;

    public SliderListener(JLabel f) {
      tf = f;
    }

    public void stateChanged(ChangeEvent e) {
      JSlider s1 = (JSlider) e.getSource();
      tf.setText(Integer.toString(s1.getValue()));
    }
  }

}

