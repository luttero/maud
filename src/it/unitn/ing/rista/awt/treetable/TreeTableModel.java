/*
 * @(#)TreeTableModel.java created 01/01/1997 Mesiano
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

import javax.swing.tree.TreeModel;
import java.awt.Component;
import javax.swing.*;

/**
 * TreeTableModel is the model used by a JTreeTable. It extends TreeModel
 * to add methods for getting inforamtion about the set of columns each
 * node in the TreeTableModel may have. Each column, like a column in
 * a TableModel, has a name and a type associated with it. Each node in
 * the TreeTableModel can return a value for each of the columns and
 * set that value if isCellEditable() returns true.
 *
 * @version $Revision: 1.6 $, $Date: 2005/09/16 15:47:23 $
 * @author Philip Milne
 * @author Scott Violet
 * @author Luca Lutterotti
 */
public interface TreeTableModel extends TreeModel {
  /**
   * Returns the number ofs availible column.
   */
  public int getColumnCount();

  /**
   * Returns the name for column number <code>column</code>.
   */
  public String getColumnName(int column);

  /**
   * Returns the type for column number <code>column</code>.
   */
  public Class getColumnClass(int column);

  /**
   * Returns the value to be displayed for node <code>node</code>,
   * at column number <code>column</code>.
   */
  public Object getValueAt(Object node, int column);

  /**
   * Indicates whether the the value for node <code>node</code>,
   * at column number <code>column</code> is editable.
   */
  public boolean isCellEditable(Object node, int column);

  public JComboBox getComboBox();

  public JCheckBox getCheckBox();

  public JStepValueEdit getStepValueEdit();

  public JTextField getErrorTextField();

  public String getTreeColumnLabel();
  
  public String getValueColumnLabel();

  public String getErrorColumnLabel();

  /**
   * Sets the value for node <code>node</code>,
   * at column number <code>column</code>.
   */
  public void setValueAt(Object aValue, Object node, int column);
}
