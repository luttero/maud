/*
 * @(#)hklTableModel.java created 21/06/1998 Le Mans
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

import it.unitn.ing.rista.diffr.AtomSite;

import java.lang.*;

/**
 *  The xyzTableModel is a model interface for the JTable class that display
 *  the actual list of xyz positions of an atom of a given phase.
 *  The columns report the progressive number and the three atomic coordinates.
 *  It retrieves the column and row data from the AtomSite class.
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:09 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class xyzTableModel extends AbstractTableModel {

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
   * The AtomSite object where the xyz data are to be retrieved.
   */
  private AtomSite theatom = null;

  /**
   * Sets up the atom object where to retrieve the xyz positions and
   * retrieve from it the number of positions to store as number of rows.
   *
   * @param anatom				the atom
   * @see                 AtomSite
   */

  public xyzTableModel(AtomSite anatom) {
    theatom = anatom;
    anatom.refreshPositions(false);
    numRows = anatom.getSiteMultiplicity();
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
   * It is retrieved directly from the atom.
   * In any row the elements are:
   *
   *  row 	x  y  z
   *
   * @param  row		the row position
   * @param  column	the column position
   * @return the element at position row x column
   *
   * @see AtomSite
   */

  public Object getValueAt(int row, int column) {
    switch (column) {
      case 0:
        return new Integer(row + 1);
      case 1:
        return new Double(theatom.getx(row));
      case 2:
        return new Double(theatom.gety(row));
      case 3:
        return new Double(theatom.getz(row));
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

