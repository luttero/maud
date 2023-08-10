/*
 * @(#)ParameterTreeModel.java created 01/01/1998 Mesiano
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.Parameter;
import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.interfaces.basicObj;

import javax.swing.*;
import java.awt.*;

/**
 * ParameterTreeModel is a TreeTableModel representing a hierarchical file
 * system. Nodes in the ParameterTreeModel are FileNodes which, when they
 * are directory nodes, cache their children to avoid repeatedly querying
 * the real file system.
 * 
 * @author Luca Lutterotti
 * @version $Revision: 1.9 $, $Date: 2005/09/16 15:47:23 $
 * @since JDK1.1
 */

public class ParameterTreeModel extends AbstractTreeTableModel
    implements TreeTableModel {

  // Names of the columns.
  static protected String[] cNames = {"Name", "Value", "Min", "Max", "Error", "Status", "Output"};
  static protected String[] status = {"Fixed", "Refined", "Equal to", "*****"};

  // Types of the columns.
  public Class[] cTypes = {TreeTableModel.class, String.class, String.class,
                           String.class, String.class, JComboBox.class, JCheckBox.class};

  protected JComboBox refinableCB;
  protected JCheckBox outputCB;
  protected ParameterTreeTableFrame parentFrame;

  public ParameterTreeModel(ParameterTreeTableFrame parent, basicObj parameterfile) {
//    super(new ParameterNode(parameterfile));
		super(parameterfile);

    parentFrame = parent;

    refinableCB = new JComboBox();
    outputCB = new JCheckBox();
    for (int i = 0; i < status.length; i++)
      refinableCB.addItem(status[i]);
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
    return null;
  }

  public JTextField getErrorTextField() {
    return null;
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
      default:
        {
        }
    }

    return false;
  }


  //
  // Some convenience methods.
  //

  public basicObj getParameter(Object node) {
    basicObj parNode = ((basicObj) node);
    return parNode;
  }

  protected Object[] getChildren(Object node) {
    basicObj parNode = ((basicObj) node);
    return parNode.getChildren(parentFrame.getSearchString(), parentFrame.refinableOnly);
  }

  //
  // The TreeModel interface
  //

  public int getChildCount(Object node) {
    Object[] children = getChildren(node);
    return (children == null) ? 0 : children.length;
  }

  public Object getChild(Object node, int i) {
    return getChildren(node)[i];
  }

  // The superclass's implementation would work, but this is more efficient.
  public boolean isLeaf(Object node) {
    return isParameter(getParameter(node));
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
        return isParameter(par) ? ((Parameter) par).getValueMin() : "-";
      case 3:
        return isParameter(par) ? ((Parameter) par).getValueMax() : "-";
      case 4:
        return isParameter(par) ? ((Parameter) par).getError() : "-";
      case 5:
        return isParameter(par) ? status[((Parameter) par).getReducedStatusIndex()] : status[3];
      case 6:
        return par.automaticOutput();
      default:
        {
        }
    }

    return null;
  }

  public void setValueAt(Object aValue, Object node, int column) {
    basicObj par = getParameter(node);
    switch (column) {
      case 1:
        ((Parameter) par).setValue((String) aValue);
        break;
      case 2:
        ((Parameter) par).setValueMin((String) aValue);
        break;
      case 3:
        ((Parameter) par).setValueMax((String) aValue);
        break;
      case 4:
        ((Parameter) par).setError((String) aValue);
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
            par.freeAllParameters(parentFrame.getSearchString(), parentFrame.refinableOnly);
          else if (aValue.equals(status[2]))
            setXRDcatEqualto((XRDcat) par);
          else if (aValue.equals(status[0]))
            par.fixAllParameters(parentFrame.getSearchString(), parentFrame.refinableOnly);
        }
        break;
      case 6:
        par.setAutomaticOutput(((Boolean) aValue).booleanValue());
        break;
      default:
        {
        }
    }
  }

  public void setXRDcatEqualto(XRDcat par) {
    if (par != null) {
      SetEqualtoXRDcatD setequaldlg = parentFrame.XRDcatDlg;
      if (setequaldlg == null) {
        setequaldlg = new SetEqualtoXRDcatD(parentFrame, true, par);
        setequaldlg.setVisible(true);
      } else {
        setequaldlg.setVisible(true);
        setequaldlg.setParameter(par);
      }
      parentFrame.XRDcatDlg = setequaldlg;
    }
  }

  public void setparameterEqualto(Parameter par) {
    if (par != null) {
      SetEqualtoD setequaldlg = parentFrame.ParameterDlg;
      if (setequaldlg == null) {
        setequaldlg = new SetEqualtoD(parentFrame, true, par);
        setequaldlg.setVisible(true);
      } else {
        setequaldlg.setVisible(true);
        setequaldlg.setParameter(par);
      }

      parentFrame.ParameterDlg = setequaldlg;
    }
  }

}

