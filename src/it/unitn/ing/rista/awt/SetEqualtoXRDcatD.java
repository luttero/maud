/*
 * @(#)SetEqualtoXRDcatD.java created 14/08/2000 Casalino
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
import javax.swing.border.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.io.*;

import java.awt.*;

/**
 *  The SetEqualtoXRDcatD is a class to display a hierarchical tree with
 *  all the XRDcat objects in an analysis to select the one for the setEqualTo
 *  method in XRDcat to set all parameters of an object equal to the selected
 *  one.
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class SetEqualtoXRDcatD extends myJFrame {
  XRDcat theparameter = null;

  JTextField ratioTF;
  JTextField constantTF;
  JTree parameterTree;
  int rowtoselect = -1;
  int actualrow = 0;
  XRDcat refpar = null;
  DefaultMutableTreeNode startLeaf = null;
  JCheckBox statusCB;

  public SetEqualtoXRDcatD(Frame parent, boolean modal) {
    super(parent);
  }

  public SetEqualtoXRDcatD(Frame parent, boolean modal, XRDcat par) {

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
    jPanel4.add("North", new JLabel("XRDcat:"));
    JPanel jPanel3 = new JPanel();
    jPanel3.setLayout(new BorderLayout(6, 6));
    jPanel5.add("Center", jPanel3);
    JPanel jPanel2 = new JPanel();
    jPanel2.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    jPanel3.add("Center", jPanel2);

    statusCB = new JCheckBox("Extend to subordinate objects");
    statusCB.setSelected(true);
    statusCB.setToolTipText("Check the box to enforce the set equal to bound to all parameters in the objects inside this one");
    jPanel2.add(statusCB);

    basicObj filepar = getFileParent();
    startLeaf = new DefaultMutableTreeNode(filepar);
    Object tmpPar[] = filepar.getObjectChildren();
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
    setTitle("Object binding");
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
    expandAllRows();
  }

  public SetEqualtoXRDcatD(Frame parent, String title, boolean modal, XRDcat par) {
    this(parent, modal, par);
    setTitle(title);
  }

  public void addChildren(DefaultMutableTreeNode parent, DefaultMutableTreeNode children, Object child) {
    parent.add(children);
    actualrow++;
    if (refpar != null && child == refpar)
      rowtoselect = actualrow;
    Object tmpPar[] = ((basicObj) child).getObjectChildren();
    if (tmpPar != null) {
      for (int i = 0; i < tmpPar.length; i++) {
        DefaultMutableTreeNode newnode = new DefaultMutableTreeNode(tmpPar[i]);
        addChildren(children, newnode, tmpPar[i]);
      }
    }
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

  public void setParameter(XRDcat par) {
    theparameter = par;
  }

  public void retrieveParameters() {

    Object par = parameterTree.getLastSelectedPathComponent();
    if (par != null && par instanceof DefaultMutableTreeNode) {
      par = ((DefaultMutableTreeNode) par).getUserObject();
      if (par != null && par instanceof XRDcat)
        theparameter.setEqualTo((XRDcat) par, statusCB.isSelected());
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
