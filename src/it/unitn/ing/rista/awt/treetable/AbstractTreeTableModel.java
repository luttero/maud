/*
 * @(#)AbstractTreeTableModel.java created 01/01/1997 Mesiano
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

import it.unitn.ing.rista.interfaces.basicObj;
import it.unitn.ing.rista.diffr.XRDcat;

import javax.swing.tree.*;
import javax.swing.event.*;

/**
 * An abstract implementation of the TreeTableModel interface, handling
 * the list of listeners.
 *
 * @author Philip Milne
 * @author Luca Lutterotti
 * @version $Revision: 1.3 $, $Date: 2005/03/10 13:50:38 $
 */

public abstract class AbstractTreeTableModel implements TreeTableModel {
  protected basicObj root;
  protected EventListenerList listenerList = new EventListenerList();

  public AbstractTreeTableModel(basicObj root) {
    this.root = root;
  }

  /**
   * Sets the root to <code>root</code>. A null <code>root</code> implies
   * the tree is to display nothing, and is legal.
   */
  public void setRoot(basicObj root) {
    Object oldRoot = this.root;
    this.root = root;
    if (root == null && oldRoot != null) {
      fireTreeStructureChanged(this, null);
    } else {
      nodeStructureChanged(root);
    }
  }

  //
  // Default implmentations for methods in the TreeModel interface.
  //

  public Object getRoot() {
    return root;
  }

  public boolean isLeaf(Object node) {
    return getChildCount(node) == 0;
  }

  public void valueForPathChanged(TreePath path, Object newValue) {
  }

  /**
   * Returns the index of child in parent.
   * If either the parent or child is <code>null</code>, returns -1.
   *
   * @param parent a note in the tree, obtained from this data source
   * @param child  the node we are interested in
   * @return the index of the child in the parent, or -1
   *         if either the parent or the child is <code>null</code>
   */
  public int getIndexOfChild(Object parent, Object child) {
    if (parent == null || child == null)
      return -1;
    return ((basicObj) parent).getIndex((basicObj) child);
  }

  /**
   * Returns the child of <I>parent</I> at index <I>index</I> in the parent's
   * child array.  <I>parent</I> must be a node previously obtained from
   * this data source. This should not return null if <i>index</i>
   * is a valid index for <i>parent</i> (that is <i>index</i> >= 0 &&
   * <i>index</i> < getChildCount(<i>parent</i>)).
   *
   * @param parent a node in the tree, obtained from this data source
   * @return the child of <I>parent</I> at index <I>index</I>
   */
  public Object getChild(Object parent, int index) {
    return ((basicObj) parent).getChildAt(index);
  }

  /**
   * Invoke this method if you've modified the TreeNodes upon which this
   * model depends.  The model will notify all of its listeners that the
   * model has changed.
   */
  public void reload() {
    reload(root);
  }

  /**
   * Invoked this to insert newChild at location index in parents children.
   * This will then message nodesWereInserted to create the appropriate
   * event. This is the preferred way to add children as it will create
   * the appropriate event.
   */
  public void insertNodeInto(basicObj newChild,
                             XRDcat parent, int index) {
//      parent.insert(newChild, index);

    int[] newIndexs = new int[1];

    newIndexs[0] = index;
    nodesWereInserted(parent, newIndexs);
  }

  /**
   * Message this to remove node from its parent. This will message
   * nodesWereRemoved to create the appropriate event. This is the
   * preferred way to remove a node as it handles the event creation
   * for you.
   */
  public void removeNodeFromParent(basicObj node) {
    XRDcat parent = node.getParent();

    if (parent == null)
      throw new IllegalArgumentException("node does not have a parent.");

    int[] childIndex = new int[1];
    Object[] removedArray = new Object[1];

    childIndex[0] = parent.getIndex(node);
//      parent.remove(childIndex[0]);
    removedArray[0] = node;
    nodesWereRemoved(parent, childIndex, removedArray);
  }

  /**
   * Invoke this method after you've changed how node is to be
   * represented in the tree.
   */
  public void nodeChanged(basicObj node) {
    if (listenerList != null && node != null) {
      basicObj parent = node.getParent();

      if (parent != null) {
        int anIndex = parent.getIndex(node);
        if (anIndex != -1) {
          int[] cIndexs = new int[1];

          cIndexs[0] = anIndex;
          nodesChanged(parent, cIndexs);
        }
      } else if (node == getRoot()) {
        nodesChanged(node, null);
      }
    }
  }

  /**
   * Invoke this method if you've modified the TreeNodes upon which this
   * model depends.  The model will notify all of its listeners that the
   * model has changed below the node <code>node</code> (PENDING).
   */
  public void reload(basicObj node) {
    if (node != null) {
      fireTreeStructureChanged(this, getPathToRoot(node), null, null);
    }
  }

  /**
   * Invoke this method after you've inserted some TreeNodes into
   * node.  childIndices should be the index of the new elements and
   * must be sorted in ascending order.
   */
  public void nodesWereInserted(basicObj node, int[] childIndices) {
    if (listenerList != null && node != null && childIndices != null
        && childIndices.length > 0) {
      int cCount = childIndices.length;
      Object[] newChildren = new Object[cCount];

      for (int counter = 0; counter < cCount; counter++)
        newChildren[counter] = node.getChildAt(childIndices[counter]);
      fireTreeNodesInserted(this, getPathToRoot(node), childIndices,
          newChildren);
    }
  }

  /**
   * Invoke this method after you've removed some TreeNodes from
   * node.  childIndices should be the index of the removed elements and
   * must be sorted in ascending order. And removedChildren should be
   * the array of the children objects that were removed.
   */
  public void nodesWereRemoved(basicObj node, int[] childIndices,
                               Object[] removedChildren) {
    if (node != null && childIndices != null) {
      fireTreeNodesRemoved(this, getPathToRoot(node), childIndices,
          removedChildren);
    }
  }

  /**
   * Invoke this method after you've changed how the children identified by
   * childIndicies are to be represented in the tree.
   */
  public void nodesChanged(basicObj node, int[] childIndices) {
    if (node != null) {
      if (childIndices != null) {
        int cCount = childIndices.length;

        if (cCount > 0) {
          Object[] cChildren = new Object[cCount];

          for (int counter = 0; counter < cCount; counter++)
            cChildren[counter] = node.getChildAt
                (childIndices[counter]);
          fireTreeNodesChanged(this, getPathToRoot(node),
              childIndices, cChildren);
        }
      } else if (node == getRoot()) {
        fireTreeNodesChanged(this, getPathToRoot(node), null, null);
      }
    }
  }

  /**
   * Builds the parents of node up to and including the root node,
   * where the original node is the last element in the returned array.
   * The length of the returned array gives the node's depth in the
   * tree.
   *
   * @param aNode the basicObj to get the path for
   */
  public basicObj[] getPathToRoot(basicObj aNode) {
    return getPathToRoot(aNode, 0);
  }

  /**
   * Builds the parents of node up to and including the root node,
   * where the original node is the last element in the returned array.
   * The length of the returned array gives the node's depth in the
   * tree.
   *
   * @param aNode the basicObj to get the path for
   * @param depth an int giving the number of steps already taken towards
   *              the root (on recursive calls), used to size the returned array
   * @return an array of basicObjs giving the path from the root to the
   *         specified node
   */
  protected basicObj[] getPathToRoot(basicObj aNode, int depth) {
    basicObj[] retNodes;
// This method recurses, traversing towards the root in order
// size the array. On the way back, it fills in the nodes,
// starting from the root and working back to the original node.

    /* Check for null, in case someone passed in a null node, or
       they passed in an element that isn't rooted at root. */
    if (aNode == null) {
      if (depth == 0)
        return null;
      else
        retNodes = new basicObj[depth];
    } else {
      depth++;
      if (aNode == root)
        retNodes = new basicObj[depth];
      else
        retNodes = getPathToRoot(aNode.getParent(), depth);
      retNodes[retNodes.length - depth] = aNode;
    }
    return retNodes;
  }

  /**
   * Invoke this method if you've totally changed the children of
   * node and its childrens children...  This will post a
   * treeStructureChanged event.
   */
  public void nodeStructureChanged(basicObj node) {
    if (node != null) {
      fireTreeStructureChanged(this, getPathToRoot(node), null, null);
    }
  }

  public void addTreeModelListener(TreeModelListener l) {
    listenerList.add(TreeModelListener.class, l);
  }

  public void removeTreeModelListener(TreeModelListener l) {
    listenerList.remove(TreeModelListener.class, l);
  }

  /*
   * Notify all listeners that have registered interest for
   * notification on this event type.  The event instance
   * is lazily created using the parameters passed into
   * the fire method.
   * @see EventListenerList
   */
  protected void fireTreeNodesChanged(Object source, Object[] path,
                                      int[] childIndices,
                                      Object[] children) {
    // Guaranteed to return a non-null array
    Object[] listeners = listenerList.getListenerList();
    TreeModelEvent e = null;
    // Process the listeners last to first, notifying
    // those that are interested in this event
    for (int i = listeners.length - 2; i >= 0; i -= 2) {
      if (listeners[i] == TreeModelListener.class) {
        // Lazily create the event:
        if (e == null)
          e = new TreeModelEvent(source, path,
              childIndices, children);
        ((TreeModelListener) listeners[i + 1]).treeNodesChanged(e);
      }
    }
  }

  /*
   * Notify all listeners that have registered interest for
   * notification on this event type.  The event instance
   * is lazily created using the parameters passed into
   * the fire method.
   * @see EventListenerList
   */
  protected void fireTreeNodesInserted(Object source, Object[] path,
                                       int[] childIndices,
                                       Object[] children) {
    // Guaranteed to return a non-null array
    Object[] listeners = listenerList.getListenerList();
    TreeModelEvent e = null;
    // Process the listeners last to first, notifying
    // those that are interested in this event
    for (int i = listeners.length - 2; i >= 0; i -= 2) {
      if (listeners[i] == TreeModelListener.class) {
        // Lazily create the event:
        if (e == null)
          e = new TreeModelEvent(source, path,
              childIndices, children);
        ((TreeModelListener) listeners[i + 1]).treeNodesInserted(e);
      }
    }
  }

  /*
   * Notify all listeners that have registered interest for
   * notification on this event type.  The event instance
   * is lazily created using the parameters passed into
   * the fire method.
   * @see EventListenerList
   */
  protected void fireTreeNodesRemoved(Object source, Object[] path,
                                      int[] childIndices,
                                      Object[] children) {
    // Guaranteed to return a non-null array
    Object[] listeners = listenerList.getListenerList();
    TreeModelEvent e = null;
    // Process the listeners last to first, notifying
    // those that are interested in this event
    for (int i = listeners.length - 2; i >= 0; i -= 2) {
      if (listeners[i] == TreeModelListener.class) {
        // Lazily create the event:
        if (e == null)
          e = new TreeModelEvent(source, path,
              childIndices, children);
        ((TreeModelListener) listeners[i + 1]).treeNodesRemoved(e);
      }
    }
  }

  /*
   * Notify all listeners that have registered interest for
   * notification on this event type.  The event instance
   * is lazily created using the parameters passed into
   * the fire method.
   * @see EventListenerList
   */
  protected void fireTreeStructureChanged(Object source, Object[] path,
                                          int[] childIndices,
                                          Object[] children) {
    // Guaranteed to return a non-null array
    Object[] listeners = listenerList.getListenerList();
    TreeModelEvent e = null;
    // Process the listeners last to first, notifying
    // those that are interested in this event
    for (int i = listeners.length - 2; i >= 0; i -= 2) {
      if (listeners[i] == TreeModelListener.class) {
        // Lazily create the event:
        if (e == null)
          e = new TreeModelEvent(source, path,
              childIndices, children);
        ((TreeModelListener) listeners[i + 1]).treeStructureChanged(e);
      }
    }
  }

  /*
   * Notifies all listeners that have registered interest for
   * notification on this event type.  The event instance
   * is lazily created using the parameters passed into
   * the fire method.
   *
   * @param source the node where the tree model has changed
   * @param path the path to the root node
   * @see EventListenerList
   */
  private void fireTreeStructureChanged(Object source, TreePath path) {
    // Guaranteed to return a non-null array
    Object[] listeners = listenerList.getListenerList();
    TreeModelEvent e = null;
    // Process the listeners last to first, notifying
    // those that are interested in this event
    for (int i = listeners.length - 2; i >= 0; i -= 2) {
      if (listeners[i] == TreeModelListener.class) {
        // Lazily create the event:
        if (e == null)
          e = new TreeModelEvent(source, path);
        ((TreeModelListener) listeners[i + 1]).treeStructureChanged(e);
      }
    }
  }

  //
  // Default impelmentations for methods in the TreeTableModel interface.
  //

  public Class getColumnClass(int column) {
    return Object.class;
  }

  /**
   * By default, make the column with the Tree in it the only editable one.
   * Making this column editable causes the JTable to forward mouse
   * and keyboard events in the Tree column to the underlying JTree.
   */
  public boolean isCellEditable(Object node, int column) {
    return getColumnClass(column) == TreeTableModel.class;
  }

  public void setValueAt(Object aValue, Object node, int column) {
  }


  // Left to be implemented in the subclass:

  /*
   *   public Object getChild(Object parent, int index)
   *   public int getChildCount(Object parent)
   *   public int getColumnCount()
   *   public String getColumnName(Object node, int column)
   *   public Object getValueAt(Object node, int column)
   */

}
