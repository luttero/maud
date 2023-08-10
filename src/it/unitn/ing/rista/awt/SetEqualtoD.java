/*
 * @(#)SetEqualtoD.java created 01/12/1996 Mesiano
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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
import javax.swing.tree.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.util.*;

import java.awt.*;
import java.util.*;

/**
 *  The SetEqualtoD is a class that........
 *
 *
 * @version $Revision: 1.5 $, $Date: 2006/07/20 13:39:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class SetEqualtoD extends myJFrame {
  Parameter theparameter = null;

  JTextField ratioTF;
  JTextField constantTF;
  JTree parameterTree;
  int rowtoselect = -1;
  int actualrow = 0;
  Parameter refpar = null;
  DefaultMutableTreeNode startLeaf = null;

  public SetEqualtoD(Frame parent, boolean modal) {
    super(parent);
  }

  public SetEqualtoD(Frame parent, boolean modal, Parameter par) {

    super(parent);

    theparameter = par;

    Container principalPane = getContentPane();
    principalPane.setLayout(new BorderLayout(6, 6));
    JPanel jPanel5 = new JPanel();
    jPanel5.setLayout(new BorderLayout(6, 6));
    principalPane.add("Center", jPanel5);
    JPanel jPanel4 = new JPanel();
    jPanel4.setLayout(new BorderLayout(6, 6));
    jPanel5.add("West", jPanel4);
    jPanel4.add("North", new JLabel("Parameter:"));
    JPanel jPanel3 = new JPanel();
    jPanel3.setLayout(new BorderLayout(6, 6));
    jPanel5.add("Center", jPanel3);
    JPanel jPanel2 = new JPanel();
    jPanel2.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    jPanel3.add("Center", jPanel2);
    jPanel2.add(new JLabel("x"));
    ratioTF = new JTextField(Constants.FLOAT_FIELD);
    ratioTF.setText("1");
    jPanel2.add(ratioTF);
    jPanel2.add(new JLabel("+"));
    constantTF = new JTextField(Constants.FLOAT_FIELD);
    constantTF.setText("0");
    jPanel2.add(constantTF);

    refpar = theparameter.getRefparameter();

    basicObj filepar = theparameter.getParent().getFilePar();
    startLeaf = new DefaultMutableTreeNode(filepar);
    basicObj tmpPar[] = filepar.getChildren(null, false);
    if (tmpPar != null) {
      for (int i = 0; i < tmpPar.length; i++) {
        DefaultMutableTreeNode newnode = new DefaultMutableTreeNode(tmpPar[i]);
        addChildren(startLeaf, newnode, tmpPar[i]);
      }
    }

    parameterTree = new JTree(startLeaf);
//		parameterTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

    JScrollPane scrollpane = new JScrollPane(parameterTree);
//		scrollpane.setBorder(new LineBorder(Color.black));
    scrollpane.setPreferredSize(new Dimension(350, 200));
    jPanel3.add("North", scrollpane);

    JPanel jPanel1 = new JPanel();
    jPanel1.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    principalPane.add("South", jPanel1);
    JButton okB = new JIconButton("Check.gif", "Set bound");
    jPanel1.add(okB);
    JCancelButton cancelB = new JCancelButton("Close");
    jPanel1.add(cancelB);
    setTitle("Parameter binding");
    okB.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent event) {
        okB_Action();
      }
    });
    cancelB.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent event) {
        cancelB_Action();
      }
    });
    pack();
    setParameters();
  }

  public SetEqualtoD(Frame parent, String title, boolean modal, Parameter par) {
    this(parent, modal, par);
    setTitle(title);
  }

  public void addChildren(DefaultMutableTreeNode parent, DefaultMutableTreeNode children, basicObj child) {
    parent.add(children);
    actualrow++;
    if (refpar != null && child == refpar)
      rowtoselect = actualrow;
    basicObj tmpPar[] = child.getChildren(null, false);
    if (tmpPar != null) {
      for (int i = 0; i < tmpPar.length; i++) {
        DefaultMutableTreeNode newnode = new DefaultMutableTreeNode(tmpPar[i]);
        addChildren(children, newnode, tmpPar[i]);
      }
    }
  }

  public void checkTree(DefaultMutableTreeNode actualNode, Object child) {
    Enumeration enum1 = actualNode.children();
//		DefaultMutableTreeNode actualNode = startLeaf;
    actualrow++;
    while (enum1.hasMoreElements()) {
      DefaultMutableTreeNode node = (DefaultMutableTreeNode) enum1.nextElement();
      if (node.getUserObject() == child) {
        rowtoselect = actualrow;
        break;
      }
      checkTree(node, child);
    }
  }

  public void setParameter(Parameter par) {
    theparameter = par;
    refpar = theparameter.getRefparameter();
    if (refpar != null) {
      actualrow = -1;
      rowtoselect = actualrow;
      actualrow++;
      checkTree(startLeaf, refpar);
    }

    if (rowtoselect != -1)
      setSelectedRow();
    ratioTF.setText(theparameter.getRatio());
    constantTF.setText(theparameter.getConstant());
  }

  public void setParameters() {
    expandAllRows();
    if (rowtoselect != -1)
      setSelectedRow();
    ratioTF.setText(theparameter.getRatio());
    constantTF.setText(theparameter.getConstant());
  }

  public void expandAllRows() {
    int rownumbers = 0;
    int i = 0;
    rownumbers = parameterTree.getRowCount();
    while (i <= rownumbers) {
      parameterTree.expandRow(i++);
      rownumbers = parameterTree.getRowCount();
    }
  }

  public void setSelectedRow() {
//		expandAllRows();
    if (!isVisible())
      setVisible(true);
    TreePath treepath = parameterTree.getPathForRow(rowtoselect);
    parameterTree.setSelectionPath(treepath);
    parameterTree.makeVisible(treepath);
    parameterTree.scrollPathToVisible(treepath);
    rowtoselect = -1;
  }

  public void retrieveParameters() {

    Object par = parameterTree.getLastSelectedPathComponent();
    if (par != null && par instanceof DefaultMutableTreeNode) {
      par = ((DefaultMutableTreeNode) par).getUserObject();
      if (par != null && par instanceof Parameter)
        theparameter.setEqualTo((Parameter) par, ratioTF.getText(), constantTF.getText());
    }
  }

  void okB_Action() {
    retrieveParameters();
//		cancelB_Action();
  }

  void cancelB_Action() {
    setVisible(false);
    dispose();
  }
}
