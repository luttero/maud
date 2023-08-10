/*
 * @(#)ParameterTreeMutableModel.java created Dec 1, 2004 Casalino
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

package it.unitn.ing.rista.awt.treetable;

import it.unitn.ing.rista.interfaces.basicObj;
import it.unitn.ing.rista.diffr.Parameter;
import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.Constants;

import javax.swing.*;
import java.awt.*;


/**
 * The ParameterTreeMutableModel is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:53 $
 * @since JDK1.1
 */

public class ParameterTreeMutableModel extends AbstractTreeTableModel implements TreeTableModel {

  // Names of the columns.
  static protected String[] cNames = {"Name", "Value", "Error", "Min", "Max", "Status", "Output"};
  static protected String[] status = {"Fixed", "Refined", "Equal to", "*****"};

  // Types of the columns.
  public Class[] cTypes = { TreeTableModel.class, JStepValueEdit.class, String.class, //JTextField.class, modified by LL 9/9/05
                            String.class, String.class, JComboBox.class, JCheckBox.class };

  protected JComboBox refinableCB;
  protected JCheckBox outputCB;
  protected JStepValueEdit stepValueEdit;
  protected JTextField textFieldError = null;
  protected Frame parentFrame;

  public ParameterTreeMutableModel(Frame parent, basicObj parameterfile) {
//    super(new ParameterNode(parameterfile));
		super(parameterfile);

    parentFrame = parent;

    refinableCB = new JComboBox();
    outputCB = new JCheckBox();
    for (int i = 0; i < status.length; i++)
      refinableCB.addItem(status[i]);

//    textFieldError = new JTextField(Constants.FLOAT_FIELD); modified by LL 9/9/05
    stepValueEdit = new JStepValueEdit(parentFrame, textFieldError);
  }

  public String getTreeColumnLabel() {
    return cNames[0];
  }

  public String getValueColumnLabel() {
    return cNames[1];
  }

  public String getErrorColumnLabel() {
    return cNames[2];
  }

  public JComboBox getComboBox() {
    return refinableCB;
  }

  public JCheckBox getCheckBox() {
    return outputCB;
  }

  public JStepValueEdit getStepValueEdit() {
    return stepValueEdit;
  }

  public JTextField getErrorTextField() {
    return textFieldError;
  }

  public boolean isCellEditable(Object node, int column) {
    switch (column) {
      case 0:
        return true;
      case 1:
      case 2:
      case 3:
      case 4:
        basicObj par = getParameter(node);
        return isParameter(par);
      case 5:
        return true;
      case 6:
        return true;
      default: {}
    }

    return false;
  }


  //
  // Some convenience methods.
  //

  public basicObj getParameter(Object node) {
//    basicObj parNode = ((basicObj) node);
    return ((basicObj) node);
  }

  protected Object[] getChildren(Object node) {
    basicObj parNode = ((basicObj) node);
    return parNode.getChildren(null, false);
  }

  //
  // The TreeModel interface
  //

  public int getChildCount(Object node) {
    return getParameter(node).getChildCount(null, false);
  }

  public Object getChild(Object node, int i) {
//    System.out.println(node.toXRDcatString() + ", " + getChildren(node).length + ", " + getChildCount(node) + ", " + i);
    return getChildren(node)[i];
  }

  // The superclass's implementation would work, but this is more efficient.
  public boolean isLeaf(Object node) {
    return getParameter(node).isLeaf();
  }

  public void checkTree(XRDcat source, int reason) {
    if (reason == Constants.PARAMETER_CHANGED) {
      basicObj[] obj = source.getChildren(null, false);
      for (basicObj anObj : obj)
        if (isParameter(anObj))
          nodeChanged(anObj);
    } else
      reload(source);
  }

  //
  //  The TreeTableNode interface.
  //

  public boolean isParameter(Object obj) {
    return (obj instanceof Parameter);
  }

  public int getColumnCount() {
    return cNames.length;
  }

  public String getColumnName(int column) {
    return cNames[column];
  }

  public Class getColumnClass(int column) {
    return cTypes[column];
  }

  public Object getValueAt(Object node, int column) {
    basicObj par = getParameter(node);
    switch (column) {
      case 0:
        return par.toString();
      case 1:
        return isParameter(par) ? ((Parameter) par).getValue() : "-";
      case 2:
        return isParameter(par) ? ((Parameter) par).getError() : "-";
      case 3:
        return isParameter(par) ? ((Parameter) par).getValueMin() : "-";
      case 4:
        return isParameter(par) ? ((Parameter) par).getValueMax() : "-";
      case 5:
        return isParameter(par) ? status[((Parameter) par).getReducedStatusIndex()] : status[3];
      case 6:
        return par.automaticOutput();
      default: {}
    }

    return null;
  }

  public void setValueAt(Object aValue, Object node, int column) {
    basicObj par = getParameter(node);
    switch (column) {
      case 1:
        if (isParameter(par))
          ((Parameter) par).setValue((String) aValue);
        break;
      case 2:
        if (isParameter(par))
          ((Parameter) par).setError((String) aValue);
        break;
      case 3:
        ((Parameter) par).setValueMin((String) aValue);
        break;
      case 4:
        ((Parameter) par).setValueMax((String) aValue);
        break;
      case 5:
        if (isParameter(par)) {
          if (aValue.equals(status[1]))
            ((Parameter) par).setRefinable();
          else if (aValue.equals(status[2]))
            setparameterEqualto((Parameter) par);
          else if (aValue.equals(status[0]))
            ((Parameter) par).setNotRefinable();
        } else {
          if (aValue.equals(status[1]))
            ((XRDcat) par).freeAllParameters();
          else if (aValue.equals(status[2]))
            setXRDcatEqualto((XRDcat) par);
          else if (aValue.equals(status[0]))
            ((XRDcat) par).fixAllParameters();
        }
        break;
      case 6:
        par.setAutomaticOutput((Boolean) aValue);
        break;
      default: {}
    }
  }

  public void setXRDcatEqualto(XRDcat par) {
    if (par != null) {
      SetEqualtoXRDcatD setequaldlg = ((DiffractionMainFrame) parentFrame).XRDcatDlg;
      if (setequaldlg == null) {
        setequaldlg = new SetEqualtoXRDcatD(parentFrame, true, par);
        setequaldlg.setVisible(true);
      } else {
        setequaldlg.setVisible(true);
        setequaldlg.setParameter(par);
      }
      ((DiffractionMainFrame) parentFrame).XRDcatDlg = setequaldlg;
    }
  }

  public void setparameterEqualto(Parameter par) {
    if (par != null) {
      SetEqualtoD setequaldlg = ((DiffractionMainFrame) parentFrame).ParameterDlg;
      if (setequaldlg == null) {
        setequaldlg = new SetEqualtoD(parentFrame, true, par);
        setequaldlg.setVisible(true);
      } else {
        setequaldlg.setVisible(true);
        setequaldlg.setParameter(par);
      }

      ((DiffractionMainFrame) parentFrame).ParameterDlg = setequaldlg;
    }
  }

}

