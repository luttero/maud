/*
 * @(#)prefTableModel.java created 03/3/2001 Casalino
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.models;

import it.unitn.ing.rista.util.NaturalStringSorter;
import it.unitn.ing.rista.interfaces.PreferencesInterface;

import javax.swing.table.AbstractTableModel;
import java.util.*;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

/**
 *  The prefTableModel is a model interface for the JTable class that display
 *  the a general list of fields and values from an hashtable.
 *
 *
 * @version $Revision: 1.6 $, $Date: 2004/08/12 09:36:09 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class prefTableModel extends AbstractTableModel {

	/**
	 * Column labels for the table
	 */
	private String columns[] = {"field", "value"};

	/**
	 * Number of columns in the table, stored for speed enhancement.
	 */
	private int numColumns = columns.length;

	/**
	 * Number of rows in the table, stored for speed enhancement.
	 */
	private int numRows = 0;

	/**
	 * The Hashtable object where the data are to be retrieved.
	 */
	private Hashtable theTable = null;

	/**
	 * To store the keys of the Hashtable.
	 */
	private Object[] tableKeys = null;

  /**
   * The object storing the preferences
   */
  PreferencesInterface thePrefs = null;

	/**
	 * Sets up the Hashtable object where to retrieve the fields and
	 * values to generate the table.
	 *
	 * @param atable				the Hashtable
	 * @see                 Hashtable
	 */

	public prefTableModel(Preferences atable, PreferencesInterface aPrefs) {
		try {
			String[] keys = atable.keys();
			theTable = new Hashtable(keys.length);
			for (int i = 0; i < keys.length; i++)
				theTable.put(keys[i], aPrefs.getValue(keys[i]));
			thePrefs = aPrefs;
			refreshTable();
		} catch (BackingStoreException e) {
			e.printStackTrace();
		}
	}

	public prefTableModel(Hashtable atable, PreferencesInterface aPrefs) {
    theTable = atable;
    thePrefs = aPrefs;
    refreshTable();
  }

	public void refreshTable() {
		TreeMap sortedMap = new TreeMap(new NaturalStringSorter());
		sortedMap.putAll(theTable);
		numRows = sortedMap.size();
		tableKeys = new Object[numRows];
		int i = 0;
		for (Iterator e = sortedMap.keySet().iterator(); e.hasNext();) {
			tableKeys[i++] = e.next();
		}
	}

	/**
	 * @return the column number, equal to the number fields in the Hashtable
	 */

	public int getColumnCount() {
		return numColumns;
	}

	/**
	 * @return the row number
	 */

	public int getRowCount() {
		return numRows;
	}

	/**
	 * Gives the content to be displayed at position row x column.
	 * It is retrieved directly from the Hashtable.
	 * In any row the elements are:
	 *
	 *  field 	value
	 *
	 * @param  row		the row position
	 * @param  column	the column position
	 * @return the element at position row x column
	 *
	 * @see Hashtable
	 */

	public Object getValueAt(int row, int column) {
		switch (column) {
			case 0:
				return tableKeys[row];
			case 1:
				return thePrefs.getValue((String) tableKeys[row]);
			default:
				{
					return null;
				}
		}
	}

	public void setValueAt(Object aValue, int row, int column) {
		switch (column) {
			case 1:
				thePrefs.setValue((String) tableKeys[row], aValue);
				break;
			default:
				{
				}
		}
    fireTableCellUpdated(row, column);
	}

	/**
	 * Gets the label of the <code>columnIndex</code> column
	 *
	 * @param columnIndex the column number
	 * @return the appropriate label for the column
	 */

	public String getColumnName(int columnIndex) {
		return columns[columnIndex];
	}

	// Types of the columns.
//	public Class[] cTypes = {String.class, String.class};

	/*public Class getColumnClass(int column) {
		return cTypes[column];
	}*/

	public Class getColumnClass(int c) {
		return getValueAt(0, c).getClass();
	}

	public boolean isCellEditable(int rowIndex, int columnIndex) {
		switch (columnIndex) {
			case 0:
				return false;
			case 1:
				return true;
			default:
				{
				}
		}

		return false;
	}

}

