/*
 * @(#)ParameterTreePanel.java created Jan 25, 2004 Casalino
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.FilePar;
import it.unitn.ing.rista.diffr.Parameter;
import it.unitn.ing.rista.interfaces.basicObj;
import it.unitn.ing.rista.util.Constants;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import java.awt.*;


/**
 * The ParameterTreePanel is a class
 * 
 * @author Luca Lutterotti
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:52 $
 * @since JDK1.1
 */

public class ParameterTreePanel extends JPanel {

  JLabel sizeTF;
  JLabel dateTF;
  Object selectedObject = null;
  FilePar rootPar = null;
  JTree parameterTree = null;
  TreeEventReceiver receiver = null;

  public ParameterTreePanel(TreeEventReceiver parent) {

    super();

    receiver = parent;

    setLayout(new BorderLayout(Constants.borderInside, Constants.borderInside));

/*    JPanel dateSizePanel = new JPanel();
    dateSizePanel.setLayout(new FlowLayout(FlowLayout.LEFT, Constants.borderInside, Constants.borderInside));
    add(BorderLayout.SOUTH, dateSizePanel);
    dateSizePanel.add(new JLabel("Size: "));
    sizeTF = new JLabel("0");
    dateSizePanel.add(sizeTF);
    dateSizePanel.add(new JLabel("Date: "));
    dateTF = new JLabel("-");
    dateSizePanel.add(dateTF);*/

  }

  public void setParameterFile(FilePar parFile) {
    rootPar = parFile;

    DefaultMutableTreeNode startLeaf = new DefaultMutableTreeNode(parFile);
    basicObj tmpPar[] = parFile.getChildren(null, false);
    if (tmpPar != null) {
      for (int i = 0; i < tmpPar.length; i++) {
        DefaultMutableTreeNode newnode = new DefaultMutableTreeNode(tmpPar[i]);
        addChildren(startLeaf, newnode, tmpPar[i]);
      }
    }

    parameterTree = new JTree(startLeaf);
//		parameterTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    parameterTree.addTreeSelectionListener(new javax.swing.event.TreeSelectionListener() {
      public void valueChanged(javax.swing.event.TreeSelectionEvent event) {
        updateSelectedParameter();
      }
    });

    JScrollPane scrollpane = new JScrollPane(parameterTree);
//		scrollpane.setBorder(new LineBorder(Color.black));
    scrollpane.setPreferredSize(new Dimension(350, 200));
    add(BorderLayout.CENTER, scrollpane);

  }

  public void addChildren(DefaultMutableTreeNode parent, DefaultMutableTreeNode children, basicObj child) {
    parent.add(children);
    basicObj tmpPar[] = child.getChildren(null, false);
    if (tmpPar != null) {
      for (int i = 0; i < tmpPar.length; i++) {
        DefaultMutableTreeNode newnode = new DefaultMutableTreeNode(tmpPar[i]);
        addChildren(children, newnode, tmpPar[i]);
      }
    }
  }

  public void updateSelectedParameter() {
    Object obj = parameterTree.getLastSelectedPathComponent();
    if (obj != null && obj instanceof DefaultMutableTreeNode) {
      Object par = ((DefaultMutableTreeNode) obj).getUserObject();
      if (par != null && par != selectedObject) {
        selectedObject = par;
//        sizeTF.setText(Long.toXRDcatString(file.length()));
//        dateTF.setText((new Date(file.lastModified())).toXRDcatString());
        if (par instanceof Parameter) {
          receiver.fireSelectionChanged(par, this);
        }
      }
    }
  }
}
