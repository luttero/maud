/*
 * @(#)ListVector.java created 01/01/1997 Mesiano
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

package it.unitn.ing.rista.util;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;

import javax.swing.*;

import it.unitn.ing.rista.interfaces.*;

/**
 * The ListVector is a class
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class ListVector extends Vector {
  private JList list = null;
  java.awt.event.MouseAdapter themouselistener = null;
  private XRDcat parent = null;

  public ListVector(int dimension, int increment, XRDcat obj) {
    super(dimension, increment);
    parent = obj;
  }

  public ListVector(int dimension, int increment, JList alist, XRDcat obj) {
    this(dimension, increment, obj);
    setList(alist);
  }

  public void updateList() {
    if (list != null) {
//	    System.out.println("Updating list " + this.toString());
      list.setListData(this);
      list.invalidate();
      list.getParent().validate();
    }
  }

  public XRDcat getParent() {
    return parent;
  }

  public void addItem(Object obj) {
    addElement(obj);
    if (obj instanceof basicObj) {
    XRDcat parent = ((basicObj) obj).getParent();
    if (parent != null) {
      if (obj instanceof Parameter)
        parent.notifyUpObjectChanged(parent, Constants.PARAMETER_ADDED);
      else
        parent.notifyUpObjectChanged(parent, Constants.OBJECT_ADDED);
    }
    }
    updateList();
  }

  public void insertItem(Object obj, int index) {
    insertElementAt(obj, index);
    if (obj instanceof basicObj) {
    XRDcat parent = ((basicObj) obj).getParent();
    if (parent != null) {
      if (obj instanceof Parameter)
        parent.notifyUpObjectChanged(parent, Constants.PARAMETER_ADDED);
      else
        parent.notifyUpObjectChanged(parent, Constants.OBJECT_ADDED);
    }
    }
    updateList();
  }

  public void setItemAt(Object obj, int index) {
    setElementAt(obj, index);
    if (obj instanceof basicObj) {
    XRDcat parent = ((basicObj) obj).getParent();
    if (parent != null) {
      if (obj instanceof Parameter)
        parent.notifyUpObjectChanged(parent, Constants.PARAMETER_CHANGED);
      else
        parent.notifyUpObjectChanged(parent, Constants.OBJECT_CHANGED);
    }
    }
    updateList();
  }

  public void replaceElement(Object obj, Object repl) {
    int index = indexOf(obj);
    if (index >= 0)
      setItemAt(repl, index);
    else
      addItem(repl);
  }

  public void setLabelAt(String alabel, int index) {
    XRDcat xrdobj = ((XRDcat) elementAt(index));
    xrdobj.setLabel(alabel);
    XRDcat parent = xrdobj.getParent();
    if (parent != null)
      parent.notifyUpObjectChanged(parent, Constants.OBJECT_CHANGED);
    updateList();
  }

  public void removeItemAt(int index) {
//    System.out.println("Removing a peaks from " + this);
    if (index >= 0 && index < size()) {
      Object obj = elementAt(index);
//      System.out.println("Removing the peaks " + obj);
      if (Misc.areClassCompatibles("it.unitn.ing.rista.interfaces.basicObj", obj.getClass())) {
        XRDcat parent = ((basicObj) obj).getParent();
        removeElementAt(index);
//        System.out.println("Removed the peaks " + index);
        if (parent != null) {
          if (obj instanceof Parameter)
            parent.notifyUpObjectChanged(parent, Constants.PARAMETER_REMOVED);
          else
            parent.notifyUpObjectChanged(parent, Constants.OBJECT_REMOVED);
        }
        ((basicObj) obj).dispose();
      } else
        removeElementAt(index);
      updateList();
      obj = null;
    }
  }

  public void mergeItemAt(int index1, int index2) {
    if (index1 >= 0 && index1 < size() &&
            index2 >= 0 && index2 < size()) {
      Object obj1 = elementAt(index1);
      Object obj2 = elementAt(index2);
      if (Misc.areClassCompatibles("it.unitn.ing.rista.interfaces.basicObj", obj1.getClass()) &&
              Misc.areClassCompatibles("it.unitn.ing.rista.interfaces.basicObj", obj2.getClass())) {
        ((basicObj) obj1).merge((basicObj) obj2);
        XRDcat parent = ((basicObj) obj1).getParent();
        if (parent != null)
          parent.notifyUpObjectChanged(parent, Constants.OBJECT_CHANGED);
      }
    }
  }

  public void swapElements(int firstindex, int secondindex) {
    if ((firstindex >= 0 && firstindex < size())
            && (secondindex >= 0 && secondindex < size())) {
      Object firstobj = elementAt(firstindex);
      Object secondobj = elementAt(secondindex);
      setElementAt(firstobj, secondindex);
      setElementAt(secondobj, firstindex);
      if (firstobj instanceof basicObj) {
        XRDcat parent = ((basicObj) firstobj).getParent();
        if (parent != null)
          parent.notifyUpObjectChanged(parent, Constants.OBJECT_CHANGED);
      }
      if (secondobj instanceof basicObj) {
        XRDcat parent = ((basicObj) secondobj).getParent();
        if (parent != null)
          parent.notifyUpObjectChanged(parent, Constants.OBJECT_CHANGED);
      }
      updateList();
    }
  }

  public Object selectedElement() {
    if (list == null)
      return null;
    int index = list.getSelectedIndex();
    if (index != -1) {
      return elementAt(index);
    } else
      return null;
  }

  public Vector selectedElements() {
    if (list == null)
      return null;
    int index[] = getSelectedIndices();
    if (index != null) {
      Vector elements = new Vector(0, 1);
      int max = index.length;
      for (int i = 0; i < max; i++) {
        elements.addElement(elementAt(index[i]));
      }
      return elements;
    } else
      return null;
  }

  public boolean removeSelElement() {
    if (list == null)
      return false;
    int index[] = getSelectedIndices();
    if (index != null) {
      int max = index.length;
      for (int i = max - 1; i >= 0; i--) {
        removeItemAt(index[i]);
      }
      list.setSelectedIndex(-1);
      return true;
    }
    return false;
  }

  public boolean mergeSelElement() {
    if (list == null)
      return false;
    int index[] = getSelectedIndices();
    if (index != null) {
      int max = index.length;
      for (int i = 1; i < max; i++) {
        mergeItemAt(index[0], index[i]);
      }
      for (int i = max - 1; i >= 1; i--) {
        removeItemAt(index[i]);
      }
      list.setSelectedIndex(-1);
      return true;
    }
    return false;
  }

  public int getSelectedIndex() {
    if (list == null)
      return -1;
    return list.getSelectedIndex();
  }

  public int[] getSelectedIndices() {
    if (list == null || list.isSelectionEmpty())
      return null;
    return list.getSelectedIndices();
  }

  public void replaceSelElement(Object obj) {
    if (list == null)
      return;
    int index = list.getSelectedIndex();
    setItemAt(obj, index);
    list.setSelectedIndex(index);
  }

  public void removeAllItems() {
    for (int index = elementCount - 1; index >= 0; index--)
      removeItemAt(index);
    super.removeAllElements();
    updateList();
//		removeList();
  }

  public int setList(JList alist) {

//	  System.out.println(toString() + " setting: " + alist + ", old is " + list);
    if (list != alist) {
      removeList();
      list = alist;

      Component c = alist.getParent();
      while (c != null && !(c instanceof myJFrame))
        c = c.getParent();
      if (c != null)
        ((myJFrame) c).addListData(list, this);

      if (list != null) {
        list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        list.addMouseListener(themouselistener = new java.awt.event.MouseAdapter() {
          public void mouseClicked(java.awt.event.MouseEvent event) {
            if (event.getClickCount() == 2) {
              int index = list.locationToIndex(event.getPoint());
              if (index >= 0 && index < size()) {
                Object obj = elementAt(index);
                if (obj != null)
                  changeLabel(obj);
              }
            }
          }
        });
      }
//      updateList();
    } else {
//      list.invalidate();
//      list.getParent().validate();
    }
	  updateList();
	  return elementCount;
  }

  public void changeLabel(Object obj) {
/*    if (Constants.newMaud && (obj instanceof Phase || obj instanceof Sample || obj instanceof DataFileSet)) {
      ((XRDcat) obj).edit(null);
    } else */if (obj instanceof basicObj) {
      Container parent = list.getParent();
      while (parent != null && !(parent instanceof Frame))
        parent = parent.getParent();
      if (parent != null) {
        final basicObj xrdobj = ((basicObj) obj);

        final LabelD labD = new LabelD((Frame) parent, "Change object label", true);
        labD.setTextField(xrdobj.getLabel());
        labD.jbok.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            xrdobj.setLabel(labD.getTextField());
            labD.dispose();
          }
        });
      }

    } else {
      System.out.println(obj.toString() + " is not an instance of XRDcat");
    }
  }

  public void select(int index) {
//	  if (elementCount > 0)
    list.setSelectedIndex(index);
  }

  public void removeList() {
    if (list != null) {
      list.setListData(new Vector());
      if (themouselistener != null)
        list.removeMouseListener(themouselistener);
      themouselistener = null;
      list = null;
    }
  }

  public void dispose() {
    removeList();
    removeAllItems();
  }

}
