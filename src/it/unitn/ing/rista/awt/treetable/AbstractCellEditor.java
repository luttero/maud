/*
 * @(#)AbstractCellEditor.java created 01/01/1997 Mesiano
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

import javax.swing.*;
import javax.swing.event.*;
import java.util.EventObject;

/**
 * A base class for CellEditors, providing default implementations for all
 * methods in the CellEditor interface and support for managing a series
 * of listeners.
 *
 * @version $Revision: 1.3 $, $Date: 2004/12/27 16:05:17 $
 * @author Philip Milne
 * @author Luca Lutterotti
 */

public class AbstractCellEditor implements CellEditor {

  protected EventListenerList listenerList = new EventListenerList();

  public Object getCellEditorValue() {
    return null;
  }

  public boolean isCellEditable(EventObject e) {
    return true;
  }

  public boolean shouldSelectCell(EventObject anEvent) {
    return false;
  }

  public boolean stopCellEditing() {
    return true;
  }

  public void cancelCellEditing() {
  }

  public void addCellEditorListener(CellEditorListener l) {
    listenerList.add(CellEditorListener.class, l);
  }

  public void removeCellEditorListener(CellEditorListener l) {
    listenerList.remove(CellEditorListener.class, l);
  }

  /**
   * Notify all listeners that have registered interest for
   * notification on this event type.
   * @see EventListenerList
   */
  protected void fireEditingStopped() {
// Guaranteed to return a non-null array
    Object[] listeners = listenerList.getListenerList();
// Process the listeners last to first, notifying
// those that are interested in this event
    for (int i = listeners.length - 2; i >= 0; i -= 2) {
      if (listeners[i] == CellEditorListener.class) {
        ((CellEditorListener) listeners[i + 1]).editingStopped(new ChangeEvent(this));
      }
    }
  }

  /**
   * Notify all listeners that have registered interest for
   * notification on this event type.
   * @see EventListenerList
   */
  protected void fireEditingCanceled() {
// Guaranteed to return a non-null array
    Object[] listeners = listenerList.getListenerList();
// Process the listeners last to first, notifying
// those that are interested in this event
    for (int i = listeners.length - 2; i >= 0; i -= 2) {
      if (listeners[i] == CellEditorListener.class) {
        ((CellEditorListener) listeners[i + 1]).editingCanceled(new ChangeEvent(this));
      }
    }
  }
}
