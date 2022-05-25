/*
 * @(#)hklTableModel.java created 08/02/1998 Riva del Garda
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
 *  The hklTableModel is a model interface for the JTable class that display
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

public class hklTableModel extends AbstractTableModel {

  /**
   * Column labels for the table
   */
  protected String columns[] = null;
  private static String tmpcolumns[] = {"number", "h", "k", "l", "multiplicity", "D-spacing",
                                        "Crystallite", "Microstrain"};

  /**
   * Number of columns in the table, stored for speed enhancement.
   */
  protected int numColumns = 0;

  /**
   * Number of rows in the table, stored for speed enhancement.
   */
  protected int numRows = 0;

  /**
   * The parent Phase object where the the hkl data are to be retrieved.
   */
  protected Phase thephase = null;

  /**
   * Sets up the parent phase where to retrieve the hkl reflexes and
   * retrieve from it the number of peaks to store as number of rows.
   *
   * @param aphase				the parent phase
   * @see                 Phase
   */

  public hklTableModel(Phase aphase) {
    thephase = aphase;

    numRows = 0;
    if (thephase != null)
      numRows = thephase.gethklNumber();
    columns = tmpcolumns;
    numColumns = columns.length;
  }

  /**
   * Null constructor for subclasses.
   *
   */

  public hklTableModel() {
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
    return numRows;
  }

  /**
   * Gives the content to be displayed at position row x column.
   * It is retrieved directly from the parent phase.
   * In any row the elements are:
   *
   * row 	h  k  l  multiplicity  D-spacing  Fhkl  Rhkl
   *
   * the last two columns are not jet implemented.
   *
   * @param  row		the row position
   * @param  column	the column position
   * @return the element at position row x column
   *
   * @see Phase
   */

  public Object getValueAt(int row, int column) {
    if (thephase == null)
      return null;
    switch (column) {
      case 0:
        return new Integer(row + 1);
      case 1:
        return new Integer(thephase.geth(row));
      case 2:
        return new Integer(thephase.getk(row));
      case 3:
        return new Integer(thephase.getl(row));
      case 4:
        return new Integer(thephase.getMultiplicity(row));
      case 5:
        return new Double(thephase.getDspacing(row));
      case 6:
        return new Double(thephase.getCrystallite(row));
      case 7:
        return new Double(thephase.getMicrostrainD(row));
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

