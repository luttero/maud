/*
 * @(#)ElementalTable.java created 23/06/16 nowhere
 *
 * Copyright (c) 1996-2016 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of the author and it is 
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

import it.unitn.ing.rista.diffr.*;

import javax.swing.table.AbstractTableModel;

/**
 * The ElementalTableModel is a class ....
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.0 $, $Date: 23/06/16 10:19 $
 * @since JDK1.1
 */

public class ElementalTableModel extends AbstractTableModel {

	XRDcat parent = null;

	/**
	 * Column labels for the table
	 */
	private String columns[] = {"Atom", "Quantity"};

	/**
	 * Number of columns in the table, stored for speed enhancement.
	 */
	private int numColumns = columns.length;

	/**
	 * The object storing the preferences
	 */
	int subordinateListIndex = -1;

	/**
	 * Sets up the subordinate object loop where to retrieve the fields and
	 * values to generate the table.
	 *
	 * @param listIndex				index of the subordinateLoop
	 * @see                 XRDcat
	 */

	public ElementalTableModel(XRDcat theParent, int listIndex) {
		parent = theParent;
		subordinateListIndex = listIndex;
	}

	public void add() {
		int size = getRowCount();
		CompositionElement element = new CompositionElement(parent);
		element.setString(0, "Be");
		parent.addsubordinateloopField(subordinateListIndex, element);
		fireTableRowsInserted(size, size);
	}

	public void setAsAir() {
		for (int i = getRowCount() - 1; i >= 0; i--)
			remove(i);
		CompositionElement elementN = new CompositionElement(parent);
		elementN.setString(0, "N");
		elementN.getParameter(0).setValue(0.755267);
		parent.addsubordinateloopField(subordinateListIndex, elementN);
		CompositionElement elementO = new CompositionElement(parent);
		elementO.setString(0, "O");
		elementO.getParameter(0).setValue(0.231779);
		parent.addsubordinateloopField(subordinateListIndex, elementO);
		CompositionElement elementAr = new CompositionElement(parent);
		elementAr.setString(0, "Ar");
		elementAr.getParameter(0).setValue(0.0128269);
		parent.addsubordinateloopField(subordinateListIndex, elementAr);
		CompositionElement elementC = new CompositionElement(parent);
		elementC.setString(0, "C");
		elementC.getParameter(0).setValue(0.000124);
		parent.addsubordinateloopField(subordinateListIndex, elementC);
		CompositionElement elementKr = new CompositionElement(parent);
		elementKr.setString(0, "Kr");
		elementKr.getParameter(0).setValue(3.2E-6);
		parent.addsubordinateloopField(subordinateListIndex, elementKr);
		fireTableRowsInserted(0, 0);
	}

	public void remove(int index) {
		if (index >= 0 && index < getRowCount()) {
			parent.subordinateloopField[subordinateListIndex].removeItemAt(index);
			fireTableRowsDeleted(index, index);
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
		return parent.subordinateloopField[subordinateListIndex].size();
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
	 */

	public Object getValueAt(int row, int column) {
		switch (column) {
			case 0:
				return ((CompositionElement) parent.subordinateloopField[subordinateListIndex].elementAt(row)).getString(0);
			case 1:
				return ((CompositionElement) parent.subordinateloopField[subordinateListIndex].elementAt(row)).getParameter(0).getValue();
			default:
			{
				return null;
			}
		}
	}

	public void setValueAt(Object aValue, int row, int column) {
		switch (column) {
			case 0:
				((CompositionElement) parent.subordinateloopField[subordinateListIndex].elementAt(row)).setString(0, (String) aValue);
				break;
			case 1:
				String sValue = (String) aValue;
				double dValue = Double.valueOf(sValue);
				((CompositionElement) parent.subordinateloopField[subordinateListIndex].elementAt(row)).getParameter(0).setValue(dValue);
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
		switch (c) {
			case 0:
				return String.class;
			case 1:
				return String.class;
			default:
			{
			}
		}
		return null;
	}

	public boolean isCellEditable(int rowIndex, int columnIndex) {
		switch (columnIndex) {
			case 0:
				return true;
			case 1:
				return true;
			default:
			{
			}
		}

		return false;
	}

}

