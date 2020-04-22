/*
 * @(#)AbsorptionWindow.java created 06/01/2014 Caen
 *
 * Copyright (c) 2014 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr;

import it.unitn.ing.rista.awt.ElementalTableModel;
import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.chemistry.AtomInfo;
import it.unitn.ing.rista.chemistry.XRayDataSqLite;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

/**
 *  The AbsorptionWindow is a class to calculate the absorption
 *  by a window
 *
 *
 * @version $Revision: 1.0 $, $Date: 2014/01/06 11:48:17 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class AbsorptionWindow extends XRDcat {

	public static String[] diclistc = {
			"_radiation_incident_angle_degrees",
			"_instrument_window_thickness",
			"_instrument_window_density",
			"_instrument_fluorescence_intensity",
			"_instrument_window_material_composition"
	};
	public static String[] diclistcrm = {
			"_radiation_incident_angle_degrees",
			"_instrument_window_thickness",
			"_instrument_window_density",
			"_instrument_fluorescence_intensity",
			"_instrument_window_material_composition"
	};

	public static String[] classlistc = {
			"superclass:it.unitn.ing.rista.diffr.CompositionElement",
	};
	public static String[] classlistcs = {};

	private static int incident_angle_id = 0;
	private static int window_thickness_id = 1;
	private static int window_density_id = 2;
	private static int fluorescence_intensity_id = 3;
	private static int window_composition_id = 0;

	double totalAtomFraction = 1.0;
	double fluorescenceIntensity = 0.0;

	public AbsorptionWindow(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initXRD();
		identifier = "Absorption window";
		IDlabel = "Absorption window";
		description = "Absorption window description and composition";
	}

	public AbsorptionWindow(XRDcat aobj) {
		this(aobj, "Absorption window");
	}

	public AbsorptionWindow() {
		identifier = "Absorption window";
		IDlabel = "Absorption window";
		description = "Absorption window description and composition";
	}

	public void initConstant() {
		Nstring = 0;
		Nstringloop = 0;
		Nparameter = 4;
		Nparameterloop = 0;
		Nsubordinate = 0;
		Nsubordinateloop = 1;
	}

	public void initDictionary() {
		System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
		System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
		System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
	}

	public void initParameters() {
		super.initParameters();

		parameterField[incident_angle_id] = new Parameter(this, getParameterString(incident_angle_id), 90,
				ParameterPreferences.getDouble(getParameterString(incident_angle_id) + ".min", 1),
				ParameterPreferences.getDouble(getParameterString(incident_angle_id) + ".max", 90));
		parameterField[window_thickness_id] = new Parameter(this, getParameterString(window_thickness_id), 0.03,
				ParameterPreferences.getDouble(getParameterString(window_thickness_id) + ".min", 0.001),
				ParameterPreferences.getDouble(getParameterString(window_thickness_id) + ".max", 0.1));
		parameterField[window_density_id] = new Parameter(this, getParameterString(window_density_id), 2.329,
				ParameterPreferences.getDouble(getParameterString(window_density_id) + ".min", 1.0),
				ParameterPreferences.getDouble(getParameterString(window_density_id) + ".max", 5));
		parameterField[fluorescence_intensity_id] = new Parameter(this, getParameterString(fluorescence_intensity_id), 0,
				ParameterPreferences.getDouble(getParameterString(fluorescence_intensity_id) + ".min", 0),
				ParameterPreferences.getDouble(getParameterString(fluorescence_intensity_id) + ".max", 10));

		refreshComputation = true;
	}

	public void updateStringtoDoubleBuffering(boolean firstLoading) {
		super.updateStringtoDoubleBuffering(firstLoading);

		double totalFraction = 0;
		for (int i = 0; i < subordinateloopField[window_composition_id].size(); i++)
			totalFraction += ((CompositionElement) subordinateloopField[window_composition_id].elementAt(i)).getParameterValue(0);
		totalAtomFraction = totalFraction;
	}

	public void updateParametertoDoubleBuffering(boolean firstLoading) {
		// to be implemented by subclasses

		if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
			return;
		super.updateParametertoDoubleBuffering(false);

		fluorescenceIntensity = getParameterValue(fluorescence_intensity_id);
	}

	public double getWindowThickness() {
		return getParameterValue(window_thickness_id);
	}

	public double getWindowDensity() {
		return getParameterValue(window_density_id);
	}

	public void setWindowDensity(double value) {
		getParameter(window_density_id).setValue(value);
	}

	public void computeAbsorptionForLineWithEnergy(double[][] energyInKeVIntensity) {
	  if (totalAtomFraction == 0)
      updateStringtoDoubleBuffering(true);
		for (int j = 0; j < energyInKeVIntensity[0].length; j++) {
			double absorption = 0;
			for (int i = 0; i < subordinateloopField[window_composition_id].size(); i++) {
				String atomLabel = ((CompositionElement) subordinateloopField[window_composition_id].elementAt(i)).getString(0);
				double atomFraction = ((CompositionElement) subordinateloopField[window_composition_id].elementAt(i)).getParameterValue(0) /
						totalAtomFraction;
				int atomNumber = AtomInfo.retrieveAtomNumber(atomLabel);
				absorption += atomFraction * XRayDataSqLite.getTotalAbsorptionForAtomAndEnergy(atomNumber, energyInKeVIntensity[0][j]);
//        System.out.println(absorption + " " + atomNumber + " " + atomFraction + " " + energyInKeVIntensity[0][j] + " " + XRayDataSqLite.getTotalAbsorptionForAtomAndEnergy(atomNumber, energyInKeVIntensity[0][j])); // linear absorption window
			}
			absorption *= Math.abs(getWindowDensity() * getWindowThickness()); // linear absorption window

			double integral = 0;
			if (absorption < 1E30)
				integral = Math.exp(-absorption);
			energyInKeVIntensity[1][j] *= integral;
		}
	}

	public double computeAbsorptionForLineWithEnergy(double energyInKeV) {
		double absorption = 0;
		for (int i = 0; i < subordinateloopField[window_composition_id].size(); i++) {
			String atomLabel = ((CompositionElement) subordinateloopField[window_composition_id].elementAt(i)).getString(0);
			double atomFraction = ((CompositionElement) subordinateloopField[window_composition_id].elementAt(i)).getParameterValue(0) /
					totalAtomFraction;
			int atomNumber = AtomInfo.retrieveAtomNumber(atomLabel);
			absorption += atomFraction * XRayDataSqLite.getTotalAbsorptionForAtomAndEnergy(atomNumber, energyInKeV);
		}
		absorption *= Math.abs(getWindowDensity() * getWindowThickness()); // linear absorption window

		double integral = 0;
		if (absorption < 1E30)
			integral = Math.exp(-absorption);
		return integral;
	}

	public double computeMACForLineWithEnergy(double energyInKeV) {
		double absorption = 0;
		for (int i = 0; i < subordinateloopField[window_composition_id].size(); i++) {
			String atomLabel = ((CompositionElement) subordinateloopField[window_composition_id].elementAt(i)).getString(0);
			double atomFraction = ((CompositionElement) subordinateloopField[window_composition_id].elementAt(i)).getParameterValue(0) /
					totalAtomFraction;
			int atomNumber = AtomInfo.retrieveAtomNumber(atomLabel);
			absorption += atomFraction * XRayDataSqLite.getTotalAbsorptionForAtomAndEnergy(atomNumber, energyInKeV);
		}
		if (MoreMath.isInvalidNumber(absorption))
			return 0.0;
		return absorption;
	}

	public double computeAbsorptionForLineWithEnergy(double energyInKeVIntensity, double incidentAngleInRad) {
		double absorption = 0;
		for (int i = 0; i < subordinateloopField[window_composition_id].size(); i++) {
			String atomLabel = ((CompositionElement) subordinateloopField[window_composition_id].elementAt(i)).getString(0);
			double atomFraction = ((CompositionElement) subordinateloopField[window_composition_id].elementAt(i)).getParameterValue(0) /
					totalAtomFraction;
			int atomNumber = AtomInfo.retrieveAtomNumber(atomLabel);
			absorption += atomFraction * XRayDataSqLite.getTotalAbsorptionForAtomAndEnergy(atomNumber, energyInKeVIntensity);
		}
		absorption *= Math.abs(getWindowDensity() * getWindowThickness() / Math.sin(Math.abs(incidentAngleInRad))); // linear absorption window

		double integral = 0;
		if (absorption < 1E30)
			integral = Math.exp(-absorption);
		return integral;
	}

	public double getFluorescenceIntensity() {
		return fluorescenceIntensity;
	}

	public Vector<FluorescenceLine> getFluorescenceLines(double energyInKeV) {
		double intensity = getFluorescenceIntensity();

		Vector<FluorescenceLine> allLines = new Vector<FluorescenceLine>();
		for (int i = 0; i < subordinateloopField[window_composition_id].size(); i++) {
			String atomLabel = ((CompositionElement) subordinateloopField[window_composition_id].elementAt(i)).getString(0);
			double atomFraction = ((CompositionElement) subordinateloopField[window_composition_id].elementAt(i)).getParameterValue(0) / totalAtomFraction;
			int atomNumber = AtomInfo.retrieveAtomNumber(atomLabel);
			Vector<FluorescenceLine> linesForAtom = XRayDataSqLite.getFluorescenceLinesFor(
					atomNumber, energyInKeV);
			for (int j = 0; j < linesForAtom.size(); j++) {
				FluorescenceLine line = linesForAtom.elementAt(j);
//				double lineEnergy = line.getEnergy();
				//			System.out.println(line.getIntensity() + " " + intensity + " " + computeDetectorEfficiency(lineEnergy));
				line.multiplyIntensityBy(atomFraction * intensity * getWindowDensity());
/*				if (!getFilePar().isOptimizing() && Constants.testing) {
					System.out.println("Abs window: adding fluorescence line for " + atomLabel + ", Energy = " + lineEnergy + " KeV, Intensity = " + line.getIntensity());
				}*/
			}
			allLines.addAll(linesForAtom);
		}

		return allLines;
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
		JOptionsDialog adialog = new JAbsorptionWindowOptionsD(parent, this);
		return adialog;
	}

	public class JAbsorptionWindowOptionsD extends JOptionsDialog {

		JTextField incidentAngleTF;
		JTextField windowThicknessTF;
		JTextField windowDensityTF;
		JTextField fluorescenceIntensityTF;
		JButton addWindowB = new JButton("Add element");
		JButton airWindowB = new JButton("Set as air");
		JButton removeWindowB = new JButton("Remove element");
		ElementalTableModel windowCompositionModel = null;
		JTable windowCompositionTable = null;

		public JAbsorptionWindowOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new BorderLayout(3, 3));
			JPanel jpc1 = new JPanel(new FlowLayout());
			principalPanel.add(BorderLayout.CENTER, jpc1);
			JPanel jp1 = new JPanel(new GridLayout(0, 2, 6, 6));
			jpc1.add(jp1);
			jp1.add(new JLabel("Radiation incident angle (degrees): "));
			incidentAngleTF = new JTextField(Constants.FLOAT_FIELD);
			incidentAngleTF.setToolTipText("Specify the incident angle on the material for the radiation in degrees");
			jp1.add(incidentAngleTF);
			jp1.add(new JLabel("Material thickness (cm): "));
			windowThicknessTF = new JTextField(Constants.FLOAT_FIELD);
			windowThicknessTF.setToolTipText("Specify the material thickness in cm");
			jp1.add(windowThicknessTF);
			jp1.add(new JLabel("Material density (g/cm3): "));
			windowDensityTF = new JTextField(Constants.FLOAT_FIELD);
			windowDensityTF.setToolTipText("Specify the material density in g/cm3");
			jp1.add(windowDensityTF);
			jp1.add(new JLabel("Fluorescence probability: "));
			fluorescenceIntensityTF = new JTextField(Constants.FLOAT_FIELD);
			fluorescenceIntensityTF.setToolTipText("Insert the probability for fluorescence from this material");
			jp1.add(fluorescenceIntensityTF);

			JPanel eastPanel = new JPanel(new BorderLayout(6, 6));
			principalPanel.add(BorderLayout.EAST, eastPanel);

			JTabbedPane jpT = new JTabbedPane();
			eastPanel.add(BorderLayout.CENTER, jpT);

			jp1 = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
			jpT.addTab("Material composition", jp1);
			windowCompositionModel = new ElementalTableModel(AbsorptionWindow.this, window_composition_id);
			windowCompositionTable = new JTable(windowCompositionModel);
			JScrollPane windowCompositionScrollpane = new JScrollPane(windowCompositionTable);
			JPanel jPanel = new JPanel(new BorderLayout(3, 3));
			jp1.add(jPanel);
			jPanel.add(BorderLayout.CENTER, windowCompositionScrollpane);
			JPanel jp2 = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
			jPanel.add(BorderLayout.EAST, jp2);
			JPanel jp3 = new JPanel(new GridLayout(0, 1, 3, 3));
			jp2.add(jp3);
			jp3.add(addWindowB);
			jp3.add(removeWindowB);
			jp3.add(airWindowB);
			addWindowB.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent actionEvent) {
					windowCompositionModel.add();
				}
			});
			removeWindowB.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent actionEvent) {
					int index = windowCompositionTable.getSelectedRow();
					windowCompositionModel.remove(index);
				}
			});
			airWindowB.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent actionEvent) {
					windowCompositionModel.setAsAir();
					AbsorptionWindow.this.setWindowDensity(0.00120479);
					windowDensityTF.setText("0.00120479");
				}
			});


/*			JPanel plotButtonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
			eastPanel.add(BorderLayout.SOUTH, plotButtonPanel);
			plotSpectrumB = new JButton("Plot tube spectrum");
			plotSpectrumB.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent actionEvent) {
					plotSpectrum();
				}
			});
			plotButtonPanel.add(plotSpectrumB); */

			setTitle("Absorption material setting");
			initParameters();
			pack();
		}

		public void initParameters() {
			incidentAngleTF.setText(getParameterValueAsString(incident_angle_id));
			addComponenttolist(incidentAngleTF, getParameter(incident_angle_id));
			windowThicknessTF.setText(getParameterValueAsString(window_thickness_id));
			addComponenttolist(windowThicknessTF, getParameter(window_thickness_id));
			windowDensityTF.setText(getParameterValueAsString(window_density_id));
			addComponenttolist(windowDensityTF, getParameter(window_density_id));
			fluorescenceIntensityTF.setText(getParameterValueAsString(fluorescence_intensity_id));
			addComponenttolist(fluorescenceIntensityTF, getParameter(fluorescence_intensity_id));
		}

		/**
		 * This method is automatically called when the user press the close button on the dialog.
		 */
		public void retrieveParameters() {
			getParameter(incident_angle_id).setValue(incidentAngleTF.getText());
			getParameter(window_thickness_id).setValue(windowThicknessTF.getText());
			getParameter(window_density_id).setValue(windowDensityTF.getText());
			getParameter(fluorescence_intensity_id).setValue(fluorescenceIntensityTF.getText());
		}

	}

/*	public class elementTableModel extends AbstractTableModel {

		private String columns[] = {"Atom", "Quantity"};
		private int numColumns = columns.length;
		int subordinateListIndex = -1;

		public elementTableModel(int listIndex) {
			subordinateListIndex = listIndex;
		}

		public void add() {
			int size = getRowCount();
			CompositionElement element = new CompositionElement(AbsorptionWindow.this);
			element.setString(0, "Be");
			addsubordinateloopField(subordinateListIndex, element);
			fireTableRowsInserted(size, size);
		}

		public void remove(int index) {
			if (index >= 0 && index < getRowCount()) {
				subordinateloopField[subordinateListIndex].removeItemAt(index);
				fireTableRowsDeleted(index, index);
			}
		}

		public int getColumnCount() {
			return numColumns;
		}

		public int getRowCount() {
			return subordinateloopField[subordinateListIndex].size();
		}

		public Object getValueAt(int row, int column) {
			switch (column) {
				case 0:
					return ((CompositionElement) subordinateloopField[subordinateListIndex].elementAt(row)).getString(0);
				case 1:
					return ((CompositionElement) subordinateloopField[subordinateListIndex].elementAt(row)).getParameter(0).getValue();
				default:
				{
					return null;
				}
			}
		}

		public void setValueAt(Object aValue, int row, int column) {
			switch (column) {
				case 0:
					((CompositionElement) subordinateloopField[subordinateListIndex].elementAt(row)).setString(0, (String) aValue);
					break;
				case 1:
					String sValue = (String) aValue;
					double dValue = Double.valueOf(sValue);
					((CompositionElement) subordinateloopField[subordinateListIndex].elementAt(row)).getParameter(0).setValue(dValue);
					break;
				default:
				{
				}
			}
			fireTableCellUpdated(row, column);
		}

		public String getColumnName(int columnIndex) {
			return columns[columnIndex];
		}

		// Types of the columns.
//	public Class[] cTypes = {String.class, String.class};

	//public Class getColumnClass(int column) {
	//	return cTypes[column];
	//}

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

	}*/


}
