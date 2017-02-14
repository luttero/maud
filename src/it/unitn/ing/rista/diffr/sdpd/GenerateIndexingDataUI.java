/*
 * @(#)GenerateIndexingDataUI.java created July 5, 2006 Casalino
 *
 * Copyright (c) 1996-2006 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.sdpd;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.PrintStream;
import java.util.Vector;

/**
 * The GenerateIndexingDataUI is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.3 $, $Date: 2006/12/04 14:30:05 $
 * @since JDK1.1
 */

public class GenerateIndexingDataUI extends JFrame {

  JComboBox symmetrychoice;
  JComboBox spacegrouplist;
  String symmetry = null;
  String spaceGroup = null;
  JTextField[] allTFs = null;
  double[] cellData = new double[7];
  int[] cellAndLinesNumber = new int[3];
  String[] properties = {"IndexingTraining.symmetry",
      "IndexingTraining.spacegroup",
      "IndexingTraining.minVolume",
      "IndexingTraining.maxVolume",
      "IndexingTraining.min_a_b_c",
      "IndexingTraining.max_a_b_c",
      "IndexingTraining.minAngles",
      "IndexingTraining.maxangles",
      "IndexingTraining.maxError",
      "IndexingTraining.trainingCellNumber",
      "IndexingTraining.linesNumber",
      "IndexingTraining.extraLinesNumber",
      "IndexingTraining.missingLinesNumber"};
  String[] defValue = {"triclinic",
      "all",
      "25.0",
      "10000.0",
      "3",
      "30",
      "90",
      "135",
      "0.01",
      "10000",
      "30",
      "1",
      "1"};
  public static String[] allUniqueSgHM = SpaceGroups.getAllUniqueSgHM();
  String symmetryString[] = {"all", "triclinic", "monoclinic", "orthorhombic",
      "tetragonal", "trigonal", "hexagonal", "cubic"};
  Phase[] thePhase;
  Sample theSample;
  boolean saveInFile = false;
  private Vector listData = null;
  int actualRunningThreads = 0;
  int linesNumber = 0;
  int extraLines = 0;
  int missingPeaks = 0;
  double amin = 0;
  double amax = 0;
  double anmin = 0;
  double anmax = 0;
  int trialNumber = 0;
  double cellmin = 0;
  double cellmax = 0;
  boolean computing = true;

  public GenerateIndexingDataUI(Sample asample, boolean toFile) {
    theSample = asample;
    saveInFile = toFile;

    Container principalPanel = getContentPane();
    principalPanel.setLayout(new BorderLayout(6, 6));

    JPanel panelUp = new JPanel(new GridLayout(0, 2));
    principalPanel.add(panelUp, BorderLayout.CENTER);

    JPanel leftGeneralPane = new JPanel(new GridLayout(0, 1, 3, 3));
    panelUp.add(BorderLayout.WEST, leftGeneralPane);
    JPanel rightGeneralPane = new JPanel(new GridLayout(0, 1, 3, 3));
    panelUp.add(BorderLayout.CENTER, rightGeneralPane);

    leftGeneralPane.add(new JLabel("Symmetry:"));
    JPanel tmpPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
    rightGeneralPane.add(tmpPanel);
    symmetrychoice = new JComboBox();
    for (int i = 0; i < symmetryString.length; i++)
      symmetrychoice.addItem(symmetryString[i]);
    symmetrychoice.setEditable(false);
    symmetrychoice.setMaximumRowCount(symmetryString.length);
    tmpPanel.add(symmetrychoice);

    leftGeneralPane.add(new JLabel("Space group:"));
    rightGeneralPane.add(tmpPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3)));
    spacegrouplist = new JComboBox();
    spacegrouplist.setMaximumRowCount(symmetryString.length);
    tmpPanel.add(spacegrouplist);

    String[] textField = {"Minimum volume", "Maximum volume", "Minimum a,b,c",
        "Maximum a,b,c", "Minimum alpha,beta,gamma", "Maximum alpha,beta,gamma",
        "Maximum error (0=no error)", "Total number of training cells",
        "Number of lines", "Number of extra lines", "Number of missing lines"};
    allTFs = new JTextField[textField.length];
    for (int i = 0; i < textField.length; i++) {
      leftGeneralPane.add(new JLabel(textField[i] + ": "));
      rightGeneralPane.add(allTFs[i] = new JTextField(12));
    }

    JPanel panelDown = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 6));
    principalPanel.add(panelDown, BorderLayout.SOUTH);

    JButton jbok2 = new JCancelButton();
    panelDown.add(jbok2);
    jbok2.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setVisible(false);
        dispose();
      }

    });
    JButton jbok1 = new JCloseButton();
    panelDown.add(jbok1);
    jbok1.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        (new PersistentThread() {
        public void executeJob() {
            retrieveParameters();
            generateAndSaveData();
          }
        }).start();
        setVisible(false);
        dispose();
      }

    });
    getRootPane().setDefaultButton(jbok1);

    initParameters();
    initListener();
    setTitle("Generate indexing data");
    pack();
    setDefaultCloseOperation(DISPOSE_ON_CLOSE);
  }

  public void generateAndSaveData() {
    GenerateIndexingData gen = new GenerateIndexingData(theSample, linesNumber, extraLines, missingPeaks,
        amin, amax, anmin, anmax, trialNumber, cellmin, cellmax);
    gen.generateData(symmetry, spaceGroup);
    listData = gen.getData();

    if (saveInFile) {
      String filename = Utility.browseFilenametoSave(this, "Save Neural Network training data for indexing");
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
    symmetry = LastInputValues.getPref(properties[0], defValue[0]);
    spaceGroup = LastInputValues.getPref(properties[1], defValue[1]);
    symmetrychoice.setSelectedItem(symmetry);
    initspacegroup(symmetry, spaceGroup);
    for (int i = 0; i < allTFs.length; i++) {
      allTFs[i].setText(defValue[i + 2] = LastInputValues.getPref(properties[i + 2], defValue[i + 2]));
    }
  }

  public void initListener() {
/*
        Here go the listeners that need to be activated later
*/
    symmetrychoice.addItemListener(new ItemListener() {
      public void itemStateChanged(ItemEvent event) {
        symmetrychoice_Action();
      }
    });
    spacegrouplist.addItemListener(new ItemListener() {
      public void itemStateChanged(ItemEvent event) {
        spacegrouplist_ListSelect();
      }
    });
  }

  public void initspacegroup(String symmetry, String spacegroup) {
    int symnumber = loadspacegroup(symmetry, spacegroup);
    spacegrouplist.setSelectedIndex(symnumber);
  }

  public void changeSpaceGroup(String spacegroup) {
    spacegrouplist.setSelectedItem(spacegroup);
  }

  public int loadspacegroup(String symmetry, String spacegroup) {
    if (spacegrouplist.getItemCount() > 0)
      spacegrouplist.removeAllItems();
    spacegrouplist.addItem("all");
    if (symmetry.equalsIgnoreCase("all"))
      return 0;
    int i, selectedsg;
    String ansg;

    int istart = SpaceGroups.getBeginSGUnique(symmetry);
    int iend = SpaceGroups.getEndSGUnique(symmetry);
    selectedsg = istart;

    for (i = istart; i < iend; i++) {
      ansg = allUniqueSgHM[i];
      spacegrouplist.addItem(ansg);
      if (ansg.equalsIgnoreCase(spacegroup))
        selectedsg = i + 1;
    }
    return selectedsg - istart;
  }

  void symmetrychoice_Action() {
    String newsym = symmetrychoice.getSelectedItem().toString();
    if (!newsym.equalsIgnoreCase(symmetry)) {
      symmetry = newsym;
      LastInputValues.setPref(properties[0], symmetry);
      if (!symmetry.equalsIgnoreCase("all")) {
        spaceGroup = "all";
        LastInputValues.setPref(properties[1], spaceGroup);
        initspacegroup(newsym, spaceGroup);
      }
    }
  }

  public void dispose(myJFrame child) {
  }

  void spacegrouplist_ListSelect() {
    if (spacegrouplist.getSelectedItem() != null &&
        !spacegrouplist.getSelectedItem().toString().equals(spaceGroup)) {
      spaceGroup = spacegrouplist.getSelectedItem().toString();
      LastInputValues.setPref(properties[1], spaceGroup);
    }
  }

  public void retrieveParameters() {
    symmetrychoice_Action();
    spacegrouplist_ListSelect();
    defValue[0] = symmetry;
    defValue[1] = spaceGroup;
    for (int i = 0; i < allTFs.length; i++) {
      defValue[i + 2] = allTFs[i].getText();
      LastInputValues.setPref(properties[i + 2], defValue[i + 2]);
    }
    amin = Double.parseDouble(defValue[4]);
    amax = Double.parseDouble(defValue[5]);
    anmin = Double.parseDouble(defValue[6]);
    anmax = Double.parseDouble(defValue[7]);
    trialNumber = Integer.parseInt(defValue[9]);
    cellmin = Double.parseDouble(defValue[2]);
    cellmax = Double.parseDouble(defValue[3]);
    linesNumber = Integer.parseInt(defValue[10]);
    extraLines = Integer.parseInt(defValue[11]);
    missingPeaks = Integer.parseInt(defValue[12]);
  }

  public double normalizeCellValue(double value) {
    return GenerateIndexingData.normalizeValue(value, amin, amax);
  }

  public double reconstructCellValue(double norm) {
    return GenerateIndexingData.reconstructValue(norm, amin, amax);
  }

  public double normalizeAngleValue(double value) {
    return GenerateIndexingData.normalizeValue(value, anmin, anmax);
  }

  public double reconstructAngleValue(double norm) {
    return GenerateIndexingData.reconstructValue(norm, anmin, anmax);
  }

  public double[] reconstructCell(double[] rcell) {
    for (int i = 0; i < 3; i++)
      rcell[i] = GenerateIndexingData.reconstructValue(rcell[i], amin, amax);
    for (int i = 3; i < 6; i++)
      rcell[i] = GenerateIndexingData.reconstructValue(rcell[i], anmin, anmax);
    return rcell;
  }

}
