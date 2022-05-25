/*
 * @(#)FileSystemTreePanel.java created Jan 25, 2004 Casalino
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
import javax.swing.tree.DefaultMutableTreeNode;
import java.awt.*;
import java.io.File;
import java.util.Date;


/**
 * The FileSystemTreePanel is a class
 * 
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2004/08/12 09:36:02 $
 * @since JDK1.1
 */

public class FileSystemTreePanel extends JPanel {

  JLabel sizeTF;
  JLabel dateTF;
  File selectedFile = null;
  File rootPath = null;
  JTree filesTree = null;
  TreeEventReceiver receiver = null;

  public FileSystemTreePanel(TreeEventReceiver parent) {
    this(File.separator, parent);
  }

  public FileSystemTreePanel(String path, TreeEventReceiver parent) {

    super();

    receiver = parent;
    if (path == null)
      path = File.separator;

    rootPath = new File(path);

    setLayout(new BorderLayout(6, 6));

    JPanel dateSizePanel = new JPanel();
    dateSizePanel.setLayout(new FlowLayout(FlowLayout.LEFT, 6, 6));
    add(BorderLayout.SOUTH, dateSizePanel);
    dateSizePanel.add(new JLabel("Size: "));
    sizeTF = new JLabel("0");
    dateSizePanel.add(sizeTF);
    dateSizePanel.add(new JLabel("Date: "));
    dateTF = new JLabel("-");
    dateSizePanel.add(dateTF);

    DynamicTreeNode root = new DynamicTreeNode(rootPath);
    addChildren(root, rootPath);

    filesTree = new JTree(root);
//		parameterTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    filesTree.addTreeSelectionListener(new javax.swing.event.TreeSelectionListener() {
      public void valueChanged(javax.swing.event.TreeSelectionEvent event) {
        updateSelectedFile();
      }
    });

    JScrollPane scrollpane = new JScrollPane(filesTree);
//		scrollpane.setBorder(new LineBorder(Color.black));
    scrollpane.setPreferredSize(new Dimension(350, 200));
    add(BorderLayout.CENTER, scrollpane);

  }

  public void addChildren(DynamicTreeNode parent, File child) {
    String[] files = child.list();
    String path = child.getPath();
    if (files != null) {
      for (int i = 0; i < files.length; i++) {
        DynamicTreeNode fileNode = new DynamicTreeNode(new File(path, files[i]));
        parent.add(fileNode);
      }
    }
  }

  public void updateSelectedFile() {
    Object obj = filesTree.getLastSelectedPathComponent();
    if (obj != null && obj instanceof DynamicTreeNode) {
      File file = (File) ((DynamicTreeNode) obj).getUserObject();
      if (file != null && file != selectedFile) {
        selectedFile = file;
        sizeTF.setText(Long.toString(file.length()));
        dateTF.setText((new Date(file.lastModified())).toString());
        if (file.isFile()) {
          receiver.fireSelectionChanged(file, this);
        }
      }
    }
  }

  public class DynamicTreeNode extends DefaultMutableTreeNode {
    // Class stuff.

    /** Have the children of this node been loaded yet? */
    protected boolean hasLoaded;

    /**
     * Constructs a new DynamicTreeNode instance with o as the user
     * object.
     */
    public DynamicTreeNode(Object o) {
      super(o);
    }

    public boolean isLeaf() {
      if (((File) getUserObject()).isFile())
        return true;
      return false;
    }

    public String toString() {
      return ((File) getUserObject()).getName();
    }

    /**
     * If hasLoaded is false, meaning the children have not yet been
     * loaded, loadChildren is messaged and super is messaged for
     * the return value.
     */
    public int getChildCount() {
      if (!hasLoaded) {
        loadChildren();
      }
      return super.getChildCount();
    }

    /**
     * Messaged the first time getChildCount is messaged.  Creates
     * children with random names from names.
     */
    protected void loadChildren() {
      hasLoaded = true;
      File child = (File) getUserObject();
      if (child.isFile())
        return;
      String[] files = child.list();
      String path = child.getPath();
      if (files != null) {
        for (int i = 0; i < files.length; i++) {
          DynamicTreeNode fileNode = new DynamicTreeNode(new File(path, files[i]));
          add(fileNode);
        }
      }
    }
  }

}

