/*
 * @(#)WindowUtilities.java created 23/12/1998 Mesiano
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

package it.unitn.ing.rista.util;

import java.lang.*;
import java.awt.*;
import java.util.Vector;

import it.unitn.ing.rista.awt.CustomJSlider;

import javax.swing.*;

/**
 * The WindowUtilities is a class providing methods for managing GUI objects
 * and actions for XRDcat objects. To be used in the JOptionsDialog class and
 * subclass
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2005/09/07 17:14:08 $
 * @since JDK1.1
 */


public class WindowUtilities {

  public WindowUtilities() {
  }

  public static final JComponent createComponent(JPanel container, Object stringField, String label) {
    return createComponent(container, stringField, label, null);
  }

  public static final JComponent createComponent(JPanel container, Object stringField, String label,
                                                 Vector listOfChoices) {
    container.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    if (stringField instanceof String) {
      String tmpString = (String) stringField;
      if (tmpString.equalsIgnoreCase("true") || tmpString.equalsIgnoreCase("false"))
        return new JCheckBox(label);
      if (listOfChoices != null) {
        container.add(new JLabel(label + ": "));
        JComboBox comboBox = new JComboBox();
        for (int i = 0; i < listOfChoices.size(); i++)
          comboBox.addItem(listOfChoices.elementAt(i));
        return comboBox;
      }
      container.add(new JLabel(label + ": "));
      return new JTextField(Constants.FLOAT_FIELD);
    }
    // todo: add for objects
    return null;
  }

  public static final JComponent createSlider(JPanel container, Object stringField, String label, int min, int max) {
    container.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    container.add(new JLabel(label + ": "));
    return new CustomJSlider(label, min, max);
  }

  public static final void setJComponentContent(JComponent component, Object stringField) {
    if (component instanceof JTextField)
      ((JTextField) component).setText((String) stringField);
    else if (component instanceof JCheckBox)
      ((JCheckBox) component).setSelected(isTrue((String) stringField));
    else if (component instanceof JComboBox)
      ((JComboBox) component).setSelectedItem(stringField);
    else if (component instanceof CustomJSlider) {
      ((CustomJSlider) component).setValue(Integer.parseInt((String) stringField));
    }
    // todo: add for objects
  }

  public static final String getJComponentContent(JComponent component) {
    if (component instanceof JTextField)
      return ((JTextField) component).getText();
    else if (component instanceof JCheckBox)
      return trueOrFalse(((JCheckBox) component).isSelected());
    else if (component instanceof JComboBox)
      return ((JComboBox) component).getSelectedItem().toString();
      // todo: add for objects
    else if (component instanceof CustomJSlider) {
      return Integer.toString(((CustomJSlider) component).getValue());
    }
    return null;
  }

  public static boolean isTrue(String stringField) {
    if (stringField.equalsIgnoreCase("true"))
      return true;
    return false;
  }

  public static final String trueOrFalse(boolean value) {
    if (value)
      return "true";
    else
      return "false";
  }

}
