/*
 * @(#)generalPosTableModel.java created 21/06/1998 Le Mans
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
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

import java.lang.*;

/**
 *  The generalPosTableModel is a model interface for the JTable class that display
 *  the actual list of general site positions for a given phase.
 *  The columns report the progressive number and the three atomic coordinates.
 *  It retrieves the column and row data from the Phase class.
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:09 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class generalPosTableModel extends AbstractTableModel {

  /**
   * Column labels for the table
   */
  private String columns[] = {"#", "x", "y", "z"};

  /**
   * Number of columns in the table, stored for speed enhancement.
   */
  private int numColumns = columns.length;

  /**
   * Number of rows in the table, stored for speed enhancement.
   */
  private int numRows = 0;

  /**
   * The Phase object where the xyz data are to be retrieved.
   */
  private Phase thephase = null;

  /**
   * Sets up the phase object where to retrieve the xyz positions and
   * retrieve from it the number of positions to store as number of rows.
   *
   * @param aphase				the Phase object from which the atom positions are retrieved
   */

  public generalPosTableModel(Phase aphase) {
    thephase = aphase;
    numRows = thephase.getPhaseInfo().getSitePositionNumber();
  }

  /**
   * @return the column number, equal to the number of site positions
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
   * It is retrieved directly from the parent phase.
   * In any row the elements are:
   *
   *  row 	x  y  z
   *
   * @param  row		the row position
   * @param  column	the column position
   * @return the element at position row x column
   */

  public Object getValueAt(int row, int column) {
    switch (column) {
      case 0:
        return new Integer(row + 1);
      case 1:
        return thephase.getPhaseInfo().getSitePosition(row).getx();
      case 2:
        return thephase.getPhaseInfo().getSitePosition(row).gety();
      case 3:
        return thephase.getPhaseInfo().getSitePosition(row).getz();
      default:
        {
          return null;
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

