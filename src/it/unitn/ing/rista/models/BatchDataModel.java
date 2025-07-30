/*
 * @(#)BatchDataModel.java created 3/7/2025 Caen
 *
 * Copyright (c) 2025 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.awt.BatchFileDialog;
import javax.swing.table.AbstractTableModel;
import java.lang.*;

/**
 *  The BatchDataModel is a model interface for the JTable class that display
 *  the actual list of hkl reflexes for a given phase.
 *  The first three columns report the Miller indices (h k l) then the nexts
 *  the multiplicity of the plane and the d-spacing in Angstrom and subsequently
 *  if available the structure factor Fhkl and the intensity Rhkl.
 *  It retrieves the column and row data from the parent phase.
 *
 *
 * @version $Revision: 1.0 $, $Date: 2025/07/03 13:17:09 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class BatchDataModel extends AbstractTableModel {

  BatchFileDialog dataParent;

  private static String[] columns = {"Analysis to load", "Datafile to load", "Analysis to save", "Analysis title"};

  // Types of the columns.
  public Class[] cTypes = {String.class, String.class, String.class, String.class};
  int numColumns;

  /**
   * Sets up the parent BatchFileDialog where to retrieve the data and
   * setup it when changed
   *
   * @param parent				the parent dialog with the data
   * @see                 BatchFileDialog
   */

  public BatchDataModel(BatchFileDialog parent) {
    dataParent = parent;
    numColumns = columns.length;
  }

  public boolean isCellEditable(int row, int column) {
    boolean editable = false;
    switch (column) {
      case 0:
      case 1:
      case 2:
      case 3:
        editable = true;
        break;
      default: {
        editable = false;
      }
    }
    return editable;
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
    return Math.max(Math.max(dataParent.analysisFiles.size(), dataParent.dataFiles.size()),
        Math.max(dataParent.saveFiles.size(), dataParent.titles.size()));
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
   */

  public Object getValueAt(int row, int column) {
    if (dataParent != null) {
      switch (column) {
        case 0:
          if (row < dataParent.analysisFiles.size())
            return dataParent.analysisFiles.elementAt(row);
          break;
        case 1:
          if (row < dataParent.dataFiles.size())
            return dataParent.dataFiles.elementAt(row);
          break;
        case 2:
          if (row < dataParent.saveFiles.size())
            return dataParent.saveFiles.elementAt(row);
          break;
        case 3:
          if (row < dataParent.titles.size())
            return dataParent.titles.elementAt(row);
          break;
        default: {}
      }
    }
    return null;
  }

  public void setValueAt(Object aValue, int row, int column) {
    if (dataParent == null)
      return;
    switch (column) {
      case 0:
        dataParent.analysisFiles.set(row, (String) aValue);
        break;
      case 1:
        dataParent.dataFiles.set(row, (String) aValue);
        break;
      case 2:
        dataParent.saveFiles.set(row, (String) aValue);
        break;
      case 3:
        dataParent.titles.set(row, (String) aValue);
        break;
      default: {}
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

