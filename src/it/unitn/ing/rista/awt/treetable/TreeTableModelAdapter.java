/*
 * @(#)TreeTableModelAdapter.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.XRDcat;

import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.table.AbstractTableModel;
import javax.swing.tree.TreePath;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;

/**
 * This is a wrapper class takes a TreeTableModel and implements
 * the table model interface. The implementation is trivial, with
 * all of the event dispatching support provided by the superclass:
 * the AbstractTableModel.
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:53 $
 * @author Philip Milne
 * @author Scott Violet
 * @author Luca Lutterotti
 */


public class TreeTableModelAdapter extends AbstractTableModel {
  JTree tree;
  TreeTableModel treeTableModel;

  public TreeTableModelAdapter(TreeTableModel treeTableModel, JTree tree) {
    this.tree = tree;
    this.treeTableModel = treeTableModel;

    tree.addTreeExpansionListener(new TreeExpansionListener() {
      // Don't use fireTableRowsInserted() here;
      // the selection model would get  updated twice.
      public void treeExpanded(TreeExpansionEvent event) {
        fireTableDataChanged();
      }

      public void treeCollapsed(TreeExpansionEvent event) {
        fireTableDataChanged();
      }
    });
// Install a TreeModelListener that can update the table when
// tree changes. We use delayedFireTableDataChanged as we can
// not be guaranteed the tree will have finished processing
// the event before us.
    treeTableModel.addTreeModelListener(new TreeModelListener() {
      public void treeNodesChanged(TreeModelEvent e) {
        delayedFireTableDataChanged();
      }

      public void treeNodesInserted(TreeModelEvent e) {
        delayedFireTableDataChanged();
      }

      public void treeNodesRemoved(TreeModelEvent e) {
        delayedFireTableDataChanged();
      }

      public void treeStructureChanged(TreeModelEvent e) {
        delayedFireTableDataChanged();
      }
    });
  }

  // Wrappers, implementing TableModel interface.

  public int getColumnCount() {
    return treeTableModel.getColumnCount();
  }

  public String getColumnName(int column) {
    return treeTableModel.getColumnName(column);
  }

  public Class getColumnClass(int column) {
    return treeTableModel.getColumnClass(column);
  }

  public int getRowCount() {
    return tree.getRowCount();
  }

  protected Object nodeForRow(int row) {
    TreePath treePath = tree.getPathForRow(row);
    if (treePath != null)
      return treePath.getLastPathComponent(); //getPathComponent(0);
    return null;
  }

  public Object getValueAt(int row, int column) {
    Object rowObject = nodeForRow(row);
    if (rowObject != null)
      return treeTableModel.getValueAt(rowObject, column);
    else
      return null;
  }

  public boolean isCellEditable(int row, int column) {
    Object rowObject = nodeForRow(row);
    if (rowObject != null)
      return treeTableModel.isCellEditable(rowObject, column);
    else
      return false;
  }

  public void setValueAt(Object value, int row, int column) {
    Object rowObject = nodeForRow(row);
    if (rowObject != null)
      treeTableModel.setValueAt(value, rowObject, column);
  }

  public void setValueAt(Object value, TreePath row, int column) {
    if (row != null)
      treeTableModel.setValueAt(value, row.getLastPathComponent(), column);
  }

  public void checkTree(XRDcat source, int reason) {
    ((ParameterTreeMutableModel) treeTableModel).checkTree(source, reason);
  }

  /**
   * Invokes fireTableDataChanged after all the pending events have been
   * processed. SwingUtilities.invokeLater is used to handle this.
   */
  protected void delayedFireTableDataChanged() {
    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        fireTableDataChanged();
      }
    });
  }
}

