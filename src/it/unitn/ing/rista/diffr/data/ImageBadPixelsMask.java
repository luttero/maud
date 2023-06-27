/*
 * @(#)ImageBadPixelsMask.java created 5/11/2021 DII, Povo
 *
 * Copyright (c) 2021 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.data;

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
//import it.unitn.ing.rista.util.Coord2D;

import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import java.awt.*;
//import java.util.Vector;


/**
 *  The ImageBadPixelsMask is a class to operate on images to remove
 *  bad pixels or area prior to integration
 *
 *
 * @version $Revision: 1.0 $, $Date: 2021/11/5 16:37:00 $
 * @author Luca Lutterotti
 * @since JDK1.8
 */


public class ImageBadPixelsMask extends it.unitn.ing.rista.diffr.DataMask {

//	Vector<Coord2D> badPixels;

	public static String[] diclistc = {"_image2D_bad_pixels_std_dev", "_image2D_bad_pixels_surround_layers", "_image2D_bad_pixels_x", "_image2D_bad_pixels_y"};
	public static String[] diclistcrm = {"Remove bad pixel with std deviation higher than this value (0 to not use this)", "Consider surrounding pixels up to the specified layer", "Remove bad pixel at coordinate x", "Remove bad pixel at coordinate y"};

	public static String[] classlistc = {};
	public static String[] classlistcs = {};

	public ImageBadPixelsMask(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initBaseObject();
		identifier = "Bad pixels mask";
		IDlabel = "Bad pixels mask";
	}

	public ImageBadPixelsMask(XRDcat aobj) {
		this(aobj, "Bad pixels mask");
	}

	public ImageBadPixelsMask() {
		identifier = "Bad pixels mask";
		IDlabel = "Bad pixels mask";
	}

	public void initConstant() {
		Nstring = 2;
		Nstringloop = 2;
		Nparameter = 0;
		Nparameterloop = 0;
		Nsubordinate = 0;
		Nsubordinateloop = 0;
	}

	public void initDictionary() {
		for (int i = 0; i < totsubordinateloop; i++)
			diclist[i] = diclistc[i];
		System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
		for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
			classlist[i] = classlistc[i];
		for (int i = 0; i < totsubordinate - totparameterloop; i++)
			classlists[i] = classlistcs[i];
	}

	@Override
	public void initParameters() {
		super.initParameters();

		stringField[0] = MaudPreferences.getPref("badPixels.stdDeviation", "0");
		stringField[1] = MaudPreferences.getPref("badPixels.layersNumber", "2");;
	}

	public double getStandardDeviation() {
		double stdDev = Double.parseDouble(getStandardDeviationS());
		return stdDev;
	}

	public String getStandardDeviationS() {
		return stringField[0];
	}

	public void setStandardDev(String value) {
		stringField[0] = value;
	}

	public int getNumberOfLayers() {
		return Integer.parseInt(getNumberOfLayersS());
	}

	public String getNumberOfLayersS() {
		return stringField[1];
	}

	public void setNumberOfLayers(String value) {
		stringField[1] = value;
	}

	public void filterData(int[][] buffer) {
//		refreshCachedBadPixels();
		double stdDev = getStandardDeviation();
		int layersNumber = getNumberOfLayers();
		int dataNumber = 2 * layersNumber + 1;
		dataNumber *= dataNumber;
		int[] data = new int[dataNumber];
		int dataMin, dataMax, dataCenter, disabled;
		double sum, avg;
		if (stdDev > 0) {
			for (int i = 0; i < buffer.length; i++) {
				int minI = i - 1;
				if (minI < 0) minI = 0;
				int maxI = minI + 2 * layersNumber;
				while (maxI > buffer.length) {
					maxI--;
					minI--;
				}
				for (int j = 0; j < buffer[0].length; j++) {
					int minJ = j - layersNumber;
					if (minJ < 0) minJ = 0;
					int maxJ = minJ + 2 * layersNumber;
					while (maxJ > buffer[0].length) {
						maxJ--;
						minJ--;
					}
					int index = 0;
					sum = 0;
					dataMin = 0;
					dataMax = 0;
					disabled = 0;
					dataCenter = dataNumber / 2;
					for (int i1 = minI; i1 < maxI; i1++) {
						for (int j1 = minJ; j1 < maxJ; j1++) {
							data[index] = buffer[i1][j1];
							if (i1 == i & j1 == j)
								dataCenter = index;
							else if (data[index] >= 0) {
								if (data[index] > data[dataMax])
									dataMax = index;
								if (data[index] < data[dataMin])
									dataMin = index;
							} else {
								disabled++;
								data[index] = 0;
							}
							sum += data[index++];
						}
					}
					int active = dataNumber - disabled - 3;
					if (active > 1) {
						avg = (sum - data[dataCenter] - data[dataMax] - data[dataMin]) / active;
						if (Math.abs(data[dataCenter] - avg) / avg > stdDev) {
							buffer[i][j] = -1;
							System.out.println("Disabling pixel: " + i + " " + j);
						}
					}
				}
			}
		}
		for (int i = 0; i < badPixelsNumber(); i++) {
//			buffer[badPixels.elementAt(i).x][badPixels.elementAt(i).y] = -1;
			buffer[Integer.parseInt((String) stringloopField[0].elementAt(i))][Integer.parseInt((String) stringloopField[1].elementAt(i))] = -1;
		}
	}

/*	public void updateStringtoDoubleBuffering(boolean firstLoading) {
		super.updateStringtoDoubleBuffering(false);

		refreshCachedBadPixels();
	}

	public void refreshCachedBadPixels() {
		badPixels.clear();
		for (int i = 0; i < badPixelsNumber(); i++) {
			int x = Integer.parseInt((String) stringloopField[0].elementAt(i));
			int y = Integer.parseInt((String) stringloopField[1].elementAt(i));
			badPixels.add(new Coord2D(x, y));
		}
	}*/

	public void resetMask() {
		boolean isAbilitate = isAbilitatetoRefresh;
		isAbilitatetoRefresh = false;
		for (int i = 0; i < Nstringloop; i++)
			if (stringloopField[i] != null)
				stringloopField[i].removeAllItems();
		isAbilitatetoRefresh = isAbilitate;
	}

	public int badPixelsNumber() {
		return numberofelementSL(0);
	}

	public void removeBadPixel(int index) {
		try {
			boolean isAbilitate = isAbilitatetoRefresh;
			isAbilitatetoRefresh = false;
			for (it.unitn.ing.rista.util.ListVector listVector : stringloopField) listVector.removeItemAt(index);
			isAbilitatetoRefresh = isAbilitate;
			notifyUpObjectChanged(this, Constants.ANGULAR_CALIBRATION, -1);
		} catch (Exception e) {
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}

	public void addBadPixel(int x, int y) {
		stringloopField[0].addItem(Integer.toString(x));
		stringloopField[1].addItem(Integer.toString(y));
	}

	public void addBadPixel(String x, String y) {
		stringloopField[0].addItem(x);
		stringloopField[1].addItem(y);
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
		return new ImageBadPixelsMask.JBadPixelsMaskOptionsD(parent, this);
	}

	public class JBadPixelsMaskOptionsD extends JOptionsDialog {

		String[] columnNames;
		Object[][] data;
		JTextField stdDevTF;
		JTextField layersNumberTF;

		public JBadPixelsMaskOptionsD(Frame parent, XRDcat obj) {

			super("OK", parent, obj);

			principalPanel.setLayout(new BorderLayout(3, 3));

			JPanel tablePanel = new JPanel();
			tablePanel.setLayout(new BorderLayout(6, 6));

			PixelTableModel model = new PixelTableModel();
			JTable pixelsCoordTable = new JTable(model);
			JScrollPane tablescrollPane = new JScrollPane(pixelsCoordTable);
			pixelsCoordTable.setPreferredScrollableViewportSize(new Dimension(350, 400));

			tablePanel.add(BorderLayout.CENTER, tablescrollPane);

			JPanel bottomTablePanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 3, 3));

			tablePanel.add(BorderLayout.NORTH, bottomTablePanel);

			JButton jb1;
			bottomTablePanel.add(jb1 = new JButton("Add pixel"));
			jb1.addActionListener(event -> ((PixelTableModel) pixelsCoordTable.getModel()).add());
			jb1.setToolTipText("Add a new bad pixel to be removed from the intensity integration to generate patterns. You need to reload the images.");

			JButton jb2;
			bottomTablePanel.add(jb2 = new JButton("Remove pixel"));
			jb2.addActionListener(event -> model.remove(pixelsCoordTable.getSelectedRow()));
			jb2.setToolTipText("Remove a bad pixel. You need to reload the images to apply.");

			JButton jb3;
			bottomTablePanel.add(jb3 = new JButton("Remove all"));
			jb3.addActionListener(event -> ((PixelTableModel) pixelsCoordTable.getModel()).removeAll());
			jb3.setToolTipText("Remove all bad pixels. You need to reload the images to apply.");

			principalPanel.add(BorderLayout.CENTER, tablePanel);

			JPanel optionsPanel = new JPanel(new GridLayout(0, 1));
			principalPanel.add(BorderLayout.NORTH, optionsPanel);

			JPanel deviationPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 3, 3));
			deviationPanel.add(new JLabel("Standard deviation:"));
			stdDevTF = new JTextField(Constants.FLOAT_FIELD);
			stdDevTF.setText(getStandardDeviationS());
			stdDevTF.setToolTipText("Remove pixel with higher deviation respect to surrounding pixels than the standard deviation specified (0 for disabling)");
			deviationPanel.add(stdDevTF);
			optionsPanel.add(deviationPanel);

			JPanel layersPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 3, 3));
			layersPanel.add(new JLabel("Number of layers:"));
			layersNumberTF = new JTextField(Constants.FLOAT_FIELD);
			layersNumberTF.setText(getNumberOfLayersS());
			layersNumberTF.setToolTipText("Set the number of surronding pixels layers to consider (min 1)");
			layersPanel.add(layersNumberTF);
			optionsPanel.add(layersPanel);

			setTitle("Bad pixels coordinates");
//      setHelpFilename("DistanceAngleBondRestraints.txt");

			initParameters();
			pack();
		}

		public void initParameters() {
			super.initParameters();
		}

		public void retrieveParameters() {
			setStandardDev(stdDevTF.getText());
			setNumberOfLayers(layersNumberTF.getText());
//			resetMask();
//			for (Object[] datum : data) addBadPixel((String) datum[0], (String) datum[1]);
//			refreshCachedBadPixels();
		}

		class PixelTableModel extends AbstractTableModel {

			public PixelTableModel() {
				columnNames = new String[]{"x (pixels)", "y (pixels)"};
				updateData();
			}

			public void updateData() {
				data = new Object[badPixelsNumber()][2];
				for (int nd = 0; nd < badPixelsNumber(); nd++) {
					data[nd][0] = stringloopField[0].elementAt(nd);
					data[nd][1] = stringloopField[1].elementAt(nd);
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
				stringloopField[col].setItemAt(value, row);
				fireTableCellUpdated(row, col);
			}

			public void add() {
				int size = getRowCount();
//				System.out.println("Rows number: " + size);
				addBadPixel(0, 0);
				updateData();
				fireTableRowsInserted(size, size);
			}

			public void remove(int index) {
				if (index >= 0 && index < getRowCount()) {
					removeBadPixel(index);
					updateData();
					fireTableRowsDeleted(index, index);
				}
			}

			public void removeAll() {
				int size = getRowCount();
				resetMask();
				updateData();
				fireTableRowsDeleted(0, size - 1);
			}

		}
	}

}
