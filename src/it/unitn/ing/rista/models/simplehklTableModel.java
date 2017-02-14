/*
 * @(#)simplehklTableModel.java created 18/09/1999 Pergine Valsugana
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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

import javax.swing.table.AbstractTableModel;

import it.unitn.ing.rista.diffr.Phase;
import it.unitn.ing.rista.util.MaudPreferences;

import java.lang.*;
import javax.swing.*;

/**
 *  The simplehklTableModel is a model interface for the JTable class that display
 *  the actual list of hkl reflexes for a given phase.
 *  The first three columns report the Miller indices (h k l) then the nexts
 *  the multiplicity of the plane and the d-spacing in Angstrom and subsequently
 *  if available the structure factor Fhkl and the intensity Rhkl.
 *  It retrieves the column and row data from the parent phase.
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:09 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class simplehklTableModel extends hklTableModel {

  private static String[] tmpcolumns = {"h", "k", "l", "Active"};

  // Types of the columns.
  public Class[] cTypes = {Integer.class, Integer.class, Integer.class, Boolean.class};

  /**
   * Sets up the parent phase where to retrieve the hkl reflexes and
   * retrieve from it the number of peaks to store as number of rows.
   *
   * @param aphase				the parent phase
   * @see                 Phase
   */

  public simplehklTableModel(Phase aphase) {
    thephase = aphase;

	  int maxNumbersOfReflections = MaudPreferences.getInteger("texturePlot.maxNumberOfReflections", 100);
    numRows = 0;
    if (thephase != null)
      numRows = thephase.gethklNumber();
	  if (numRows > maxNumbersOfReflections)
		  numRows = maxNumbersOfReflections;
    columns = tmpcolumns;
    numColumns = columns.length;
  }

  public boolean isCellEditable(int row, int column) {
    if (column == 3)
      return true;

    return false;
  }

  public Class getColumnClass(int column) {
    return cTypes[column];
  }

  /**
   * @return the column number, equal to the number of hkl reflexes
   */

  public int getColumnCount() {
    return numColumns;
  }

  /**
   * @return the row number
   */

  public int getRowCount() {
    return numRows + 7;
  }

  /**
   * Gives the content to be displayed at position row x column.
   * It is retrieved directly from the parent phase.
   * In any row the elements are:
   *
   *	h  k  l  Active
   *
   * @param  row		the row position
   * @param  column	the column position
   * @return the element at position row x column
   *
   * @see Phase
   */

  int[][] unitHKL = {{1, 0 ,0}, {0, 1, 0}, {0, 0, 1}, {1, 1, 0}, {1, 0, 1}, {0, 1, 1}, {1, 1, 1}};

  public Object getValueAt(int row, int column) {
    if (thephase == null)
      return null;
    switch (column) {
      case 0:
        if (row < numRows)
          return new Integer(thephase.geth(row));
        else
          return unitHKL[row - numRows][0];
      case 1:
        if (row < numRows)
          return new Integer(thephase.getk(row));
        else
          return unitHKL[row - numRows][1];
      case 2:
        if (row < numRows)
          return new Integer(thephase.getl(row));
        else
          return unitHKL[row - numRows][2];
      case 3:
        return new Boolean(thephase.isTextureActive(row));
      default:
        {
          return null;
        }
    }
  }

  public void setValueAt(Object aValue, int row, int column) {
    if (thephase == null)
      return;
    switch (column) {
      case 3:
        boolean status = ((Boolean) aValue).booleanValue();
        thephase.setTextureActive(row, status);
        break;
      default:
        {
        }
    }
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
}

