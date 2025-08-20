package it.unitn.ing.rista.diffr.rsa;

/*
 * @(#)EpscDeformationMode.java created 31/03/2025 Casalino
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

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.Misc;

import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import java.awt.*;

/**
 *  The EpscDeformationMode is a class used by EPSC4
 *
 * @version $Revision: 1.0 $, $Date: 2025/03/31 15:54:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class EpscDeformationMode extends XRDcat {


  // Constructors, and init methods do not change the code between the two stars lines
  // **********************************************************************************
  public EpscDeformationMode(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public EpscDeformationMode(XRDcat aobj) {
    this(aobj, "Deformation mode");
  }

  public EpscDeformationMode() {
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
        for (int i = 0; i < parameterField.length; i++) {
          if (parameterField[i] == source) {
            notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED, -1);
            notifyParameterChanged(source, Constants.STRAIN_CHANGED, -1);
            return;
          }
        }
      super.notifyParameterChanged(source);
    }
  }

  public boolean isEnabled() {
    return stringField[1].toLowerCase().equalsIgnoreCase("true");
  }

  public void setEnabled(boolean value) {
    if (value)
      setEnabled("true");
    else
      setEnabled("false");
  }

  public void setEnabled(String value) {
    stringField[1] = value.toLowerCase();
  }

  public String getModeTitle() {
    return stringField[0];
  }

  public void setModeTitle(String atitle) {
    stringField[0] = atitle;
  }

  public String getCharacteristicTwinStress() {
    return parameterField[0].getValue();
  }

  public void setCharacteristicTwinStress(String value) {
    getParameter(0).setValue(value);
  }

  public int getPlaneDirectionNumber() {
    return numberofelementSubL(0);
  }

  public PlaneDirectionSystem getPlaneDirection(int index) {
    return ((PlaneDirectionSystem) subordinateloopField[0].elementAt(index));
  }

  public String getPlaneH(int index) {
    return getPlaneDirection(index).getString(0);
  }

  public String getPlaneK(int index) {
    return getPlaneDirection(index).getString(1);
  }

  public String getPlaneL(int index) {
    return getPlaneDirection(index).getString(2);
  }

  public String getDirectionH(int index) {
    return getPlaneDirection(index).getString(3);
  }

  public String getDirectionK(int index) {
    return getPlaneDirection(index).getString(4);
  }

  public String getDirectionL(int index) {
    return getPlaneDirection(index).getString(5);
  }

  public String getPlaneAsString(int index) {
    return getPlaneH(index) + " " + getPlaneK(index) + " " + getPlaneL(index);
  }

  public String getDirectionAsString(int index) {
    return getDirectionH(index) + " " + getDirectionK(index) + " " + getDirectionL(index);
  }

  public String getPlaneAsStringHex(int index) {
    int h = Integer.parseInt(getPlaneH(index));
    int k = Integer.parseInt(getPlaneK(index));
    int i = -h - k;
    return getPlaneH(index) + " " + getPlaneK(index) + " " + Integer.toString(i) + " " + getPlaneL(index);
  }

  public String getDirectionAsStringHex(int index) {
    int h = Integer.parseInt(getDirectionH(index));
    int k = Integer.parseInt(getDirectionK(index));
    int i = -h - k;
    return getDirectionH(index) + " " + getDirectionK(index) + " " + Integer.toString(i) + " " + getDirectionL(index);
  }

  public void resetPlanesDirections() {
    boolean isAbilitate = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    subordinateloopField[0].removeAllItems();
    isAbilitatetoRefresh = isAbilitate;
  }

  public void removePlaneDirection(int index) {
    try {
      boolean isAbilitate = isAbilitatetoRefresh;
      isAbilitatetoRefresh = false;
      subordinateloopField[0].removeItemAt(index);
      isAbilitatetoRefresh = isAbilitate;
    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
  }

  public void addPlaneDirection() {
    addsubordinateloopField(0, new PlaneDirectionSystem(this, "plane-dir X"));
  }

  public boolean isTwin() {
    return Misc.areClassCompatibles("it.unitn.ing.rista.diffr.rsa.EpscTwinningMode", this.getClass());
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    return new EpscDeformationMode.JDeformationModeOptionsD(parent, this);
  }

  class JDeformationModeOptionsD extends JOptionsDialog {

    JTextField titleTF;
    JTextField[] parsTF;
    HKLTableModel hklmodel;
    boolean istwin = false; // otherwise is a slip
    JCheckBox enabled;

    public JDeformationModeOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      istwin = isTwin();

      principalPanel.setLayout(new BorderLayout(3, 3));

      JPanel tablePanel = new JPanel();
      tablePanel.setLayout(new BorderLayout(6, 6));

      hklmodel = new HKLTableModel();
      JTable hklTable = new JTable(hklmodel);
      JScrollPane tablescrollPane = new JScrollPane(hklTable);
      hklTable.setPreferredScrollableViewportSize(new Dimension(700, 400));

      tablePanel.add(BorderLayout.CENTER, tablescrollPane);

      JPanel bottomTablePanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 3, 3));

      tablePanel.add(BorderLayout.NORTH, bottomTablePanel);

      JButton jb1;
      bottomTablePanel.add(jb1 = new JButton("Add a plane and direction"));
      jb1.addActionListener(event -> ((HKLTableModel) hklTable.getModel()).add());
      jb1.setToolTipText("Add a new plane and direction");

      JButton jb2;
      bottomTablePanel.add(jb2 = new JButton("Remove plane/dir"));
      jb2.addActionListener(event -> hklmodel.remove(hklTable.getSelectedRow()));
      jb2.setToolTipText("Remove the selected plane and direction");

      JButton jb3;
      bottomTablePanel.add(jb3 = new JButton("Remove all"));
      jb3.addActionListener(event -> ((HKLTableModel) hklTable.getModel()).removeAll());
      jb3.setToolTipText("Remove all planes and directions");

      principalPanel.add(BorderLayout.CENTER, tablePanel);

      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new GridLayout(0, 4, 3, 3));
      principalPanel.add(BorderLayout.NORTH, jPanel8);
      jPanel8.add(new JLabel("Title: "));
      titleTF = new JTextField(18);
      jPanel8.add(titleTF);
      enabled = new JCheckBox("enabled");
      enabled.setToolTipText("Set if this mode will be used or not in the calculation");
      jPanel8.add(enabled);
      jPanel8.add(new JLabel(" "));
      if (Nparameter > 0) {
        parsTF = new JTextField[Nparameter];
        int baseIndex = Nstring + Nstringloop;
        for (int i = 0; i < Nparameter; i++) {
          jPanel8.add(new JLabel(diclistRealMeaning[baseIndex + i] + ": "));
          parsTF[i] = new JTextField(Constants.FLOAT_FIELD);
          jPanel8.add(parsTF[i]);
        }
      }
      if (istwin)
        setTitle("Twinning modes");
      else
        setTitle("Slip modes");
      initParameters();
      pack();

    }

    public void initParameters() {
      titleTF.setText(getModeTitle());
      enabled.setSelected(isEnabled());
      if (Nparameter > 0) {
        for (int i = 0; i < Nparameter; i++) {
          parsTF[i].setText(parameterField[i].getValue());
          addComponenttolist(parsTF[i], parameterField[i]);
        }
      }
    }

    public void retrieveParameters() {
      setModeTitle(titleTF.getText());
      setEnabled(enabled.isSelected());
      if (Nparameter > 0)
        for (int i = 0; i < Nparameter; i++)
          parameterField[i].setValue(parsTF[i].getText());
    }

  }

  class HKLTableModel extends AbstractTableModel {

    String[] columnNames;
    Object[][] data;

    public HKLTableModel() {
      columnNames = new String[]{"h (plane)", "k (plane)", "l (plane)", "h (dir)", "k (dir)", "l (dir)"};
      updateData();
    }

    public void updateData() {
      int size = getPlaneDirectionNumber();
      data = new Object[size][columnNames.length];
      for (int nd = 0; nd < size; nd++) {
        for (int i = 0; i < columnNames.length; i++)
          data[nd][i] = ((XRDcat) subordinateloopField[0].elementAt(nd)).getString(i);
      }
    }

    public int getColumnCount() {
      return columnNames.length;
    }

    public int getRowCount() {
      return data.length;
    }

    public String getColumnName(int col) {
      return columnNames[col];
    }

    public Object getValueAt(int row, int col) {
      return data[row][col];
    }

    public Class getColumnClass(int c) {
      return getValueAt(0, c).getClass();
    }

    public boolean isCellEditable(int row, int col) {
      return true;
    }

    public void setValueAt(Object value, int row, int col) {
      data[row][col] = value;
      ((XRDcat) subordinateloopField[0].elementAt(row)).setString(col, (String) value);
      fireTableCellUpdated(row, col);
    }

    public void add() {
      int size = getRowCount();
//				System.out.println("Rows number: " + size);
      addPlaneDirection();
      updateData();
      fireTableRowsInserted(size, size);
    }

    public void remove(int index) {
      int size = getRowCount();
      if (index >= 0 && index < size) {
        removePlaneDirection(index);
        updateData();
        fireTableRowsDeleted(index, index);
      }
    }

    public void removeAll() {
      resetPlanesDirections();
      updateData();
      fireTableRowsDeleted(0, 0);
    }

  }

}
