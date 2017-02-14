/*
 * @(#)JTreeTable.java created 01/01/1997 Mesiano
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

import it.unitn.ing.rista.awt.JStepValueEdit;
import it.unitn.ing.rista.diffr.Parameter;

import javax.swing.*;
import javax.swing.text.TableView;
import javax.swing.event.*;
import javax.swing.tree.*;
import javax.swing.table.*;

import java.awt.Dimension;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;

import java.awt.event.MouseEvent;

import java.util.EventObject;

/**
 * This example shows how to create a simple JTreeTable component,
 * by using a JTree as a renderer (and editor) for the cells in a
 * particular column in the JTable.
 * Updated for JTreeTable 1.2
 *
 * @author Philip Milne
 * @author Scott Violet
 * @author Luca Lutterotti
 * @version $Revision: 1.9 $, $Date: 2005/09/16 15:47:23 $
 */

public class JTreeTable extends JTable {
  protected TreeTableCellRenderer tree;
//    TreeTableModel treeTableModel;

  public JTreeTable(TreeTableModel treeTableModel) {
    super();

//	this.treeTableModel = treeTableModel;

// Create the tree. It will be used as a renderer and editor.
    tree = new TreeTableCellRenderer(treeTableModel);

// Install a tableModel representing the visible rows in the tree.
    super.setModel(new TreeTableModelAdapter(treeTableModel, tree));

// Force the JTable and JTree to share their row selection models.
    ListToTreeSelectionModelWrapper selectionWrapper = new
        ListToTreeSelectionModelWrapper();
    tree.setSelectionModel(selectionWrapper);
    setSelectionModel(selectionWrapper.getListSelectionModel());
// Install the tree editor renderer and editor.
    setDefaultRenderer(TreeTableModel.class, tree);
    setDefaultEditor(TreeTableModel.class, new TreeTableCellEditor());

    JComboBox jc = treeTableModel.getComboBox();
    if (jc != null)
      setDefaultEditor(JComboBox.class, new DefaultCellEditor(jc));

    JCheckBox jcb = treeTableModel.getCheckBox();
    if (jcb != null)
      setDefaultEditor(JCheckBox.class, new DefaultCellEditor(jcb));

    JStepValueEdit jsv = treeTableModel.getStepValueEdit();
    if (jsv != null)
      setDefaultEditor(JStepValueEdit.class, new JStepValueEditor(jsv));

    JTextField jtf = treeTableModel.getErrorTextField();
    if (jtf != null)
      setDefaultEditor(JTextField.class, new DefaultCellEditor(jtf));

    TableColumn treeColumn = getColumn(treeTableModel.getTreeColumnLabel());
    treeColumn.setPreferredWidth(350);

    TableColumn valueColumn = getColumn(treeTableModel.getValueColumnLabel());
    valueColumn.setPreferredWidth(250);
    getColumn(treeTableModel.getErrorColumnLabel()).setPreferredWidth(100);

// No grid.
    setShowGrid(false);

// No intercell spacing
    setIntercellSpacing(new Dimension(0, 0));

// And update the height of the trees row to match that of
// the table, do not remove!
    setRowHeight(24);
  }

  /**
   * Overridden to message super and forward the method to the tree.
   * Since the tree is not actually in the component hierarchy it will
   * never receive this unless we forward it in this manner.
   */
  public void updateUI() {
    super.updateUI();
    if (tree != null) {
      tree.updateUI();
    }
// Use the tree's default foreground and background colors in the
// table.
    LookAndFeel.installColorsAndFont(this, "Tree.background",
        "Tree.foreground", "Tree.font");
  }

  /* Workaround for BasicTableUI anomaly. Make sure the UI never tries to
   * paint the editor. The UI currently uses different techniques to
   * paint the renderers and editors and overriding setBounds() below
   * is not the right thing to do for an editor. Returning -1 for the
   * editing row in this case, ensures the editor is never painted.
   */
  public int getEditingRow() {
    return (getColumnClass(editingColumn) == TreeTableModel.class) ? -1 :
        editingRow;
  }

  /**
   * Overridden to pass the new rowHeight to the tree.
   */
  public void setRowHeight(int rowHeight) {
    super.setRowHeight(rowHeight);
    if (tree != null && tree.getRowHeight() != rowHeight)
      tree.setRowHeight(getRowHeight());

  }

  public boolean setValueAt(Object value) {
    setValueAt(value, editingRow, 1);
    return true;
  }

  /**
   * Returns the tree that is being shared between the model.
   */
  public JTree getTree() {
    return tree;
  }

  /**
   * A TreeCellRenderer that displays a JTree.
   */
  public class TreeTableCellRenderer extends JTree implements
      TableCellRenderer {
    /**
     * Last table/tree row asked to renderer.
     */
    protected int visibleRow;

    public TreeTableCellRenderer(TreeModel model) {
      super(model);
    }

    /**
     * updateUI is overridden to set the colors of the Tree's renderer
     * to match that of the table.
     */
    public void updateUI() {
      super.updateUI();
      // Make the tree's cell renderer use the table's cell selection
      // colors.
      TreeCellRenderer tcr = getCellRenderer();
      if (tcr instanceof DefaultTreeCellRenderer) {
        DefaultTreeCellRenderer dtcr = ((DefaultTreeCellRenderer) tcr);
        // For 1.1 uncomment this, 1.2 has a bug that will cause an
        // exception to be thrown if the border selection color is
        // null.
        // dtcr.setBorderSelectionColor(null);
        dtcr.setTextSelectionColor(UIManager.getColor
            ("Table.selectionForeground"));
        dtcr.setBackgroundSelectionColor(UIManager.getColor
            ("Table.selectionBackground"));
      }
    }

    /**
     * Sets the row height of the tree, and forwards the row height to
     * the table.
     */
    public void setRowHeight(int rowHeight) {
      if (rowHeight > 0) {
        super.setRowHeight(rowHeight);
        if (JTreeTable.this != null &&
            JTreeTable.this.getRowHeight() != rowHeight) {
          JTreeTable.this.setRowHeight(TreeTableCellRenderer.this.getRowHeight());
        }
      }
    }

    /**
     * This is overridden to set the height to match that of the JTable.
     */
    public void setBounds(int x, int y, int w, int h) {
      super.setBounds(x, 0, w, JTreeTable.this.getHeight());
    }

    /**
     * Sublcassed to translate the graphics such that the last visible
     * row will be drawn at 0,0.
     */
    public void paint(Graphics g) {
      g.translate(0, -visibleRow * TreeTableCellRenderer.this.getRowHeight());
      super.paint(g);
    }

    /**
     * TreeCellRenderer method. Overridden to update the visible row.
     */
    public Component getTableCellRendererComponent(JTable table,
                                                   Object value,
                                                   boolean isSelected,
                                                   boolean hasFocus,
                                                   int row, int column) {
      if (isSelected)
        TreeTableCellRenderer.this.setBackground(table.getSelectionBackground());
      else
        TreeTableCellRenderer.this.setBackground(table.getBackground());

      visibleRow = row;
      return this;
    }
  }


  /**
   * TreeTableCellEditor implementation. Component returned is the
   * JTree.
   */
  public class TreeTableCellEditor extends AbstractCellEditor implements
      TableCellEditor {
    public Component getTableCellEditorComponent(JTable table,
                                                 Object value,
                                                 boolean isSelected,
                                                 int r, int c) {
      return tree;
    }

    /**
     * Overridden to return false, and if the event is a mouse event
     * it is forwarded to the tree.<p>
     * The behavior for this is debatable, and should really be offered
     * as a property. By returning false, all keyboard actions are
     * implemented in terms of the table. By returning true, the
     * tree would get a chance to do something with the keyboard
     * events. For the most part this is ok. But for certain keys,
     * such as left/right, the tree will expand/collapse where as
     * the table focus should really move to a different column. Page
     * up/down should also be implemented in terms of the table.
     * By returning false this also has the added benefit that clicking
     * outside of the bounds of the tree node, but still in the tree
     * column will select the row, whereas if this returned true
     * that wouldn't be the case.
     * <p>By returning false we are also enforcing the policy that
     * the tree will never be editable (at least by a key sequence).
     */
    public boolean isCellEditable(EventObject e) {
      return true;
/*	    if (e instanceof MouseEvent) {
		for (int counter = getColumnCount() - 1; counter >= 0;
		     counter--) {
		    if (getColumnClass(counter) == TreeTableModel.class) {
			MouseEvent me = (MouseEvent)e;
			MouseEvent newME = new MouseEvent(tree, me.getID(),
				   me.getWhen(), me.getModifiers(),
				   me.getX() - getCellRect(0, counter, true).x,
				   me.getY(), me.getClickCount(),
                                   me.isPopupTrigger());
			tree.dispatchEvent(newME);
// This is a workaround for Apple bug
			newME = new MouseEvent(tree, me.MOUSE_RELEASED,
				   me.getWhen(), me.getModifiers(),
				   me.getX() - getCellRect(0, counter, true).x,
				   me.getY(), me.getClickCount(),
                                    me.isPopupTrigger());
			tree.dispatchEvent(newME);


			break;
		    }
		}
	    }
	    return false;*/
    }
  }


  /**
   * ListToTreeSelectionModelWrapper extends DefaultTreeSelectionModel
   * to listen for changes in the ListSelectionModel it maintains. Once
   * a change in the ListSelectionModel happens, the paths are updated
   * in the DefaultTreeSelectionModel.
   */
  class ListToTreeSelectionModelWrapper extends DefaultTreeSelectionModel {
    /**
     * Set to true when we are updating the ListSelectionModel.
     */
    protected boolean updatingListSelectionModel;

    public ListToTreeSelectionModelWrapper() {
      super();
      getListSelectionModel().addListSelectionListener(createListSelectionListener());
    }

    /**
     * Returns the list selection model. ListToTreeSelectionModelWrapper
     * listens for changes to this model and updates the selected paths
     * accordingly.
     */
    ListSelectionModel getListSelectionModel() {
      return listSelectionModel;
    }

    /**
     * This is overridden to set <code>updatingListSelectionModel</code>
     * and message super. This is the only place DefaultTreeSelectionModel
     * alters the ListSelectionModel.
     */
    public void resetRowSelection() {
      if (!updatingListSelectionModel) {
        updatingListSelectionModel = true;
        try {
          super.resetRowSelection();
        } finally {
          updatingListSelectionModel = false;
        }
      }
      // Notice how we don't message super if
      // updatingListSelectionModel is true. If
      // updatingListSelectionModel is true, it implies the
      // ListSelectionModel has already been updated and the
      // paths are the only thing that needs to be updated.
    }

    /**
     * Creates and returns an instance of ListSelectionHandler.
     */
    protected ListSelectionListener createListSelectionListener() {
      return new ListSelectionHandler();
    }

    /**
     * If <code>updatingListSelectionModel</code> is false, this will
     * reset the selected paths from the selected rows in the list
     * selection model.
     */
    protected void updateSelectedPathsFromSelectedRows() {
      if (!updatingListSelectionModel) {
        updatingListSelectionModel = true;
        try {
          // This is way expensive, ListSelectionModel needs an
          // enumerator for iterating.
          int min = listSelectionModel.getMinSelectionIndex();
          int max = listSelectionModel.getMaxSelectionIndex();

          ListToTreeSelectionModelWrapper.this.clearSelection();
          if (min != -1 && max != -1) {
            for (int counter = min; counter <= max; counter++) {
              if (listSelectionModel.isSelectedIndex(counter)) {
                TreePath selPath = tree.getPathForRow(counter);
                if (selPath != null) {
                  addSelectionPath(selPath);
                }
              }
            }
          }
        } finally {
          updatingListSelectionModel = false;
        }
      }
    }

    /**
     * Class responsible for calling updateSelectedPathsFromSelectedRows
     * when the selection of the list change.
     */
    class ListSelectionHandler implements ListSelectionListener {
      public void valueChanged(ListSelectionEvent e) {
        updateSelectedPathsFromSelectedRows();
      }
    }
  }
}
