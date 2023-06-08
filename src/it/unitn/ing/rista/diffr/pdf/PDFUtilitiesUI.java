/*
 * @(#)PDFUtilitiesUI.java created June 1, 2023 Los Alamos
 *
 * Copyright (c) 1996-2023 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.pdf;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.*;

import javax.swing.*;
import java.awt.*;
import java.io.PrintStream;
import java.util.Vector;

/**
 * The PDFUtilitiesUI is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.3 $, $Date: 2006/12/04 14:30:05 $
 * @since JDK1.1
 */

public class PDFUtilitiesUI extends JFrame {

	Phase thePhase;
	boolean saveInFile = false;
	private Vector listData = null;

	JComboBox radiationCB;
	JTextField[] allTFs;

	String[] properties = {"PDFgenerateGr.radiation",
			"PDFgenerateGr.minQ",
			"PDFgenerateGr.maxQ",
			"PDFgenerateGr.stepQ"};
	String[] defValue = {GeneratePatternAndPDF.radiationType[0],
			"0.5",
			"40.0",
			"0.01"};

	public PDFUtilitiesUI(Phase aphase, boolean toFile) {
		thePhase = aphase;
		saveInFile = toFile;

		Container principalPanel = getContentPane();
		principalPanel.setLayout(new BorderLayout(6, 6));

		JPanel panelUp = new JPanel(new GridLayout(0, 2));
		principalPanel.add(panelUp, BorderLayout.CENTER);

		JPanel leftGeneralPane = new JPanel(new GridLayout(0, 1, 3, 3));
		panelUp.add(BorderLayout.WEST, leftGeneralPane);
		JPanel rightGeneralPane = new JPanel(new GridLayout(0, 1, 3, 3));
		panelUp.add(BorderLayout.CENTER, rightGeneralPane);

		leftGeneralPane.add(new JLabel("Radiation:"));
		JPanel tmpPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
		rightGeneralPane.add(tmpPanel);
		radiationCB = new JComboBox();
		for (int i = 0; i < GeneratePatternAndPDF.radiationType.length; i++)
			radiationCB.addItem(GeneratePatternAndPDF.radiationType[i]);
		radiationCB.setEditable(false);
		radiationCB.setMaximumRowCount(GeneratePatternAndPDF.radiationType.length);
		tmpPanel.add(radiationCB);

		String[] textField = {"Minimum Q", "Maximum Q", "Q step"};
		allTFs = new JTextField[textField.length];
		for (int i = 0; i < textField.length; i++) {
			leftGeneralPane.add(new JLabel(textField[i] + ": "));
			rightGeneralPane.add(allTFs[i] = new JTextField(12));
		}

		JPanel panelDown = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 6));
		principalPanel.add(panelDown, BorderLayout.SOUTH);

		JButton jbok2 = new JCancelButton();
		panelDown.add(jbok2);
		jbok2.addActionListener(e -> {
			setVisible(false);
			dispose();
		});
		JButton jbok1 = new JCloseButton();
		panelDown.add(jbok1);
		jbok1.addActionListener(e -> {
			(new PersistentThread() {
				public void executeJob() {
					retrieveParameters();
				}
			}).start();
			setVisible(false);
			dispose();
		});
		getRootPane().setDefaultButton(jbok1);

		initParameters();
		setTitle("Generate G(r) from phase crystal structure");
		pack();
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
	}

	public void generateAndSaveData(String radiation, double minQ, double maxQ, double stepQ) {
		GeneratePatternAndPDF gen = new GeneratePatternAndPDF(thePhase, minQ, maxQ, stepQ);
		gen.generateData(radiation);
		listData = gen.getGr();

		if (saveInFile) {
			String filename = Utility.browseFilenametoSave(this, "Save G(r) to file");
			if (filename != null) {
				PrintStream printStream = new PrintStream(Misc.getOutputStream(filename));
				saveList(printStream, listData);
				printStream.flush();
				printStream.close();
			}
		}
	}

	public Vector getData() {
		while (listData == null) {
			try {
				Thread.sleep(500);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		return listData;
	}

	private void saveList(PrintStream printStream, Vector data) {
		int number = data.size();
		for (int i = 0; i < number; i++) {
			Vector subdata = (Vector) data.elementAt(i);
			int j = 0;
			for (; j < subdata.size() - 1; j++) {
				double[] row = (double[]) subdata.elementAt(j);
				for (int k = 0; k < row.length; k++)
					printStream.print(Fmt.format(row[k]) + ";");
				if (j < subdata.size() - 2)
					printStream.print(Constants.lineSeparator);
			}
			String[] row = (String[]) subdata.elementAt(j);
			printStream.print(" # " + row[0] + " " + row[1]);
			printStream.print(Constants.lineSeparator);
		}
	}

	public void initParameters() {
		radiationCB.setSelectedItem(LastInputValues.getPref(properties[0], defValue[0]));
		for (int i = 0; i < allTFs.length; i++) {
			allTFs[i].setText(defValue[i + 1] = LastInputValues.getPref(properties[i + 1], defValue[i + 1]));
		}
	}

	public void retrieveParameters() {
		defValue[0] = GeneratePatternAndPDF.radiationType[radiationCB.getSelectedIndex()];
		for (int i = 0; i < allTFs.length; i++) {
			defValue[i + 1] = allTFs[i].getText();
			LastInputValues.setPref(properties[i + 1], defValue[i + 1]);
		}
		String radiation = defValue[0];
		double minQ = Double.parseDouble(defValue[1]);
		double maxQ = Double.parseDouble(defValue[2]);
		double stepQ = Double.parseDouble(defValue[3]);
		generateAndSaveData(radiation, minQ, maxQ, stepQ);
	}

}
