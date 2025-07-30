/*
 * @(#)BatchFileDialog.java created 3/7/2025 Caen
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

package it.unitn.ing.rista.awt;

import it.unitn.ing.rista.models.BatchDataModel;
import it.unitn.ing.rista.util.FileDrop;
import it.unitn.ing.rista.util.Misc;

import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.util.Vector;

/**
 *  The BatchFileDialog is a model interface for the JTable class that display
 *  the actual list of hkl reflexes for a given phase.
 *  The first three columns report the Miller indices (h k l) then the nexts
 *  the multiplicity of the plane and the d-spacing in Angstrom and subsequently
 *  if available the structure factor Fhkl and the intensity Rhkl.
 *  It retrieves the column and row data from the parent phase.
 *
 *
 * @version $Revision: 1.0 $, $Date: 2025/07/03 10:02:39 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class BatchFileDialog extends JFrame {


  JTextField filenameField = null;
  String workingDir = ".";

  JTable datatable;
  JCheckBox saveOutCB;
  JCheckBox savePlotCB;
  String[] outText = {"datafile", "_out", ".cif"};
  boolean[] outEdit = {true, true, true};
  JTextField[] outTF = new JTextField[outText.length];
  String[] plotText = {"analysis", "_plot", ".png"};
  boolean[] plotEdit = {true, true, false};
  JTextField[] plotTF = new JTextField[plotText.length];
  String[] analysisText = {"prefix", "001", ".par"};
  boolean[] analysisEdit = {true, true, true};
  JTextField[] analysisTF = new JTextField[analysisText.length];
  String[] datafileText = {"prefix", "001", ".xye"};
  boolean[] datafileEdit = {true, true, true};
  JTextField[] datafileTF = new JTextField[datafileText.length];
  String[] saveText = {"prefix", "001", ".par"};
  boolean[] saveEdit = {true, true, true};
  JTextField[] saveTF = new JTextField[saveText.length];
  String[] titleText = {"Analysis of ", "datafile", " by Maud"};
  boolean[] titleEdit = {true, true, true};
  JTextField[] titleTF = new JTextField[titleText.length];

  JTabbedPane tabPanel;
  JTextField iterationsTF;
  JComboBox wizardCB;
  JTextField resultsTF;
  JTextField simpleResultsTF;

  public Vector<String> analysisFiles = new Vector<>(0, 10);
  public Vector<String> dataFiles = new Vector<>(0, 10);
  public Vector<String> saveFiles = new Vector<>(0, 10);
  public Vector<String> titles = new Vector<>(0, 10);
  public int iterations = 5;
  public int wizard = 3;
  public String results = "results.txt";
  public String simpleResults = "simple_results.txt";
  public Vector<String> outputDataV = new Vector<>(0, 10);
  public Vector<String> plotFileV = new Vector<>(0, 10);

  public String outputData = "_out.txt";
  public String plotFile = "_plot.png";


  public BatchFileDialog(String title) {
    super(title);

    JPanel principalPanel = new JPanel(new BorderLayout(6, 6));
    getContentPane().add(principalPanel, BorderLayout.CENTER);

    principalPanel.add(directoryPanel(), BorderLayout.NORTH);
    principalPanel.add(contentPanel(), BorderLayout.CENTER);
    principalPanel.add(optionsPanel(), BorderLayout.SOUTH);

    JPanel buttonPanel = new JPanel();

    JButton updateButton = new JButton("Apply rules");
    updateButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        try {
          processData();
        } catch (Exception exc) {
          AttentionD.showAlertDialog(BatchFileDialog.this, "Something went wrong");
          exc.printStackTrace();
        }
      }
    });
    buttonPanel.add(updateButton);

    JButton goButton = new JButton("Save");
    goButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        try {
          saveFile();
          setVisible(false);
          dispose();
        } catch (Exception exc) {
          AttentionD.showAlertDialog(BatchFileDialog.this, "Something went wrong");
          exc.printStackTrace();
        }
      }
    });
    buttonPanel.add(goButton);

    JButton closeButton = new JButton("Cancel");
    closeButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setVisible(false);
        dispose();
      }
    });
    buttonPanel.add(closeButton);

    getContentPane().add(buttonPanel, BorderLayout.SOUTH);

    pack();
//    setSize(250, 200);
    Utility.putOnScreenAt(this, 30);
  }

  JPanel contentPanel() {
    JPanel dataPanel = new JPanel(new BorderLayout(3,3));

    TableModel dataModel = new BatchDataModel(this);
    datatable = new JTable(dataModel);
    datatable.setPreferredScrollableViewportSize(new Dimension(800, 200));
    JScrollPane scrollpane = new JScrollPane(datatable);
//		scrollpane.setBorder(new LineBorder(Color.black));
    dataPanel.add(scrollpane, BorderLayout.CENTER);

    return dataPanel;
  }

  String[] rb1String = {"Use the list (or just one)", "Use from previous analysis", "Generate: "};
  JRadioButton[] analysisInRB = new JRadioButton[rb1String.length];

  String[] rb2String = {"Use the list", "No datafiles (included in analysis)", "Generate: "};
  JRadioButton[] datafileRB = new JRadioButton[rb2String.length];

  String[] rb3String = {"Same as analysis load", "Don't save the analysis", "Generate: "};
  JRadioButton[] analysisSaveRB = new JRadioButton[rb3String.length];
  String[] rb4String = {"No title", "Generate: "};
  JRadioButton[] titlesRB = new JRadioButton[rb4String.length];

  JPanel optionsPanel() {
    JPanel optionsP = new JPanel(new BorderLayout(3,3));
    tabPanel = new JTabbedPane();
    optionsP.add(tabPanel, BorderLayout.CENTER);
    String tpString[] = {"Analysis load", "Datafiles", "Analysis save",
        "Titles", "Others"};

    JPanel jp1 = new JPanel(new BorderLayout(3, 3));
    tabPanel.addTab(tpString[0], null, jp1);
    JPanel l1 = new JPanel(new FlowLayout(FlowLayout.LEFT));
    JLabel label1 = new JLabel("Drop analyses here to add to list");
    l1.add(label1);
    jp1.add(l1, BorderLayout.NORTH);
    l1 = new JPanel(new GridLayout(0, 1));
    jp1.add(l1, BorderLayout.CENTER);
    ButtonGroup bg1 = new ButtonGroup();
    for (int i = 0; i < rb1String.length; i++) {
      JPanel lp = new JPanel(new FlowLayout(FlowLayout.LEFT));
      l1.add(lp);
      lp.add(analysisInRB[i] = new JRadioButton(rb1String[i]));
      if (rb1String[i].endsWith(": "))
        createPrePostFields(lp, analysisTF, analysisText, analysisEdit);
      bg1.add(analysisInRB[i]);
    }
    new FileDrop(jp1, files -> {
      // handle file drop
      for (File file: files)
        analysisFiles.add(file.getName());
      ((AbstractTableModel) datatable.getModel()).fireTableDataChanged();
    }); // end FileDrop.Listener
    analysisInRB[0].setSelected(true);

    JPanel jp2 = new JPanel(new BorderLayout(3, 3));
    tabPanel.addTab(tpString[1], null, jp2);
    JPanel l2 = new JPanel(new FlowLayout(FlowLayout.LEFT));
    l2.add(new JLabel("Drop datafiles here to add to list"));
    jp2.add(l2, BorderLayout.NORTH);
    l2 = new JPanel(new GridLayout(0, 1));
    jp2.add(l2, BorderLayout.CENTER);
    ButtonGroup bg2 = new ButtonGroup();
    for (int i = 0; i < rb2String.length; i++) {
      JPanel lp = new JPanel(new FlowLayout(FlowLayout.LEFT));
      l2.add(lp);
      lp.add(datafileRB[i] = new JRadioButton(rb2String[i]));
      if (rb2String[i].endsWith(": ")) {
        createPrePostFields(lp, datafileTF, datafileText, datafileEdit);
      }
      bg2.add(datafileRB[i]);
    }
    new FileDrop(jp2, files -> {
      // handle file drop
      for (File file: files)
        dataFiles.add(file.getName());
      ((AbstractTableModel) datatable.getModel()).fireTableDataChanged();
    }); // end FileDrop.Listener
    datafileRB[0].setSelected(true);

    JPanel jp3 = new JPanel(new BorderLayout(3, 3));
    tabPanel.addTab(tpString[2], null, jp3);
    JPanel l3 = new JPanel(new GridLayout(0, 1));
    jp3.add(l3, BorderLayout.CENTER);
    ButtonGroup bg3 = new ButtonGroup();
    for (int i = 0; i < rb3String.length; i++) {
      JPanel lp = new JPanel(new FlowLayout(FlowLayout.LEFT));
      l3.add(lp);
      lp.add(analysisSaveRB[i] = new JRadioButton(rb3String[i]));
      if (rb3String[i].endsWith(": ")) {
        createPrePostFields(lp, saveTF, saveText, saveEdit);
      }
      bg3.add(analysisSaveRB[i]);
    }
    analysisSaveRB[0].setSelected(true);

    JPanel jp4 = new JPanel(new BorderLayout(3, 3));
    tabPanel.addTab(tpString[3], null, jp4);
    JPanel l4 = new JPanel(new GridLayout(0, 1));
    jp4.add(l4, BorderLayout.CENTER);
    ButtonGroup bg4 = new ButtonGroup();
    for (int i = 0; i < rb4String.length; i++) {
      JPanel lp = new JPanel(new FlowLayout(FlowLayout.LEFT));
      l4.add(lp);
      lp.add(titlesRB[i] = new JRadioButton(rb4String[i]));
      if (rb4String[i].endsWith(": ")) {
        createPrePostFields(lp, titleTF, titleText, titleEdit);
      }
      bg4.add(titlesRB[i]);
    }
    titlesRB[1].setSelected(true);

    JPanel jp5 = new JPanel(new GridLayout(0, 1));
    tabPanel.addTab(tpString[4], null, jp5);
    JPanel p1 = new JPanel(new FlowLayout(FlowLayout.LEFT));
    jp5.add(p1);
    p1.add(new JLabel("Iterations: "));
    p1.add(iterationsTF = new JTextField(6));
    iterationsTF.setText(Integer.toString(iterations));
//    p1 = new JPanel(new FlowLayout(FlowLayout.LEFT));
//    jp5.add(p1);
    p1.add(new JLabel("  Wizard number: "));
    wizardCB = new JComboBox();
    wizardCB.addItem("No wizard refinement");
    for (int i = 0; i < refinementWizardD.wizardAnalysisTitle.length; i++)
      wizardCB.addItem(refinementWizardD.wizardAnalysisTitle[i]);
    wizardCB.setSelectedIndex(wizard);
    p1.add(wizardCB);
    p1 = new JPanel(new FlowLayout(FlowLayout.LEFT));
    jp5.add(p1);
    p1.add(new JLabel("Results filename: "));
    resultsTF = new JTextField(18);
    resultsTF.setText(results);
    p1.add(resultsTF);
//    p1 = new JPanel(new FlowLayout(FlowLayout.LEFT));
//    jp5.add(p1);
    p1.add(new JLabel("   Simple results filename: "));
    simpleResultsTF = new JTextField(18);
    simpleResultsTF.setText(simpleResults);
    p1.add(simpleResultsTF);

    jp5.add(p1 = new JPanel(new FlowLayout(FlowLayout.LEFT)));
    p1.add(saveOutCB = new JCheckBox("Data output"));
    createPrePostFields(p1, outTF, outText, outEdit);

    jp5.add(p1 = new JPanel(new FlowLayout(FlowLayout.LEFT)));
    p1.add(savePlotCB = new JCheckBox("Plot output"));
    createPrePostFields(p1, plotTF, plotText, plotEdit);

    return optionsP;
  }

  public void createPrePostFields(JPanel p1, JTextField[] tf, String[] text, boolean[] canEdit) {
    for (int i = 0; i < tf.length; i++) {
      tf[i] = new JTextField(20);
      tf[i].setText(text[i]);
      tf[i].setEditable(canEdit[i]);
      p1.add(tf[i]);
      if (i != tf.length - 1)
        p1.add(new JLabel(" + "));
    }
  }

  JPanel directoryPanel() {
    JPanel filePanel = new JPanel();
    filePanel.add(new JLabel("Working directory: "));
    filenameField = new JTextField(48);
    filePanel.add(filenameField);
    JButton dirButton = new JButton("Choose...");
    dirButton.addActionListener(e -> {
      String filename = Utility.browseFolder(BatchFileDialog.this, "Select the working directory", null);
      if (!filename.endsWith("/"))
        filename = filename + "/";
      filenameField.setText(filename);
    });
    filePanel.add(dirButton);

    return filePanel;
  }

  public void processData() {
    int maxRows = datatable.getModel().getRowCount();
    int selAnalysisLoad = getRadioButtonSelection(analysisInRB);
    int selDatafiles = getRadioButtonSelection(datafileRB);
    int selAnalysisSave = getRadioButtonSelection(analysisSaveRB);
    int selTitles = getRadioButtonSelection(titlesRB);

    for (int i = 0; i < maxRows; i++) {
      if (selAnalysisLoad == 0) {
        // use list
        if (analysisFiles.size() == 0) {
          System.out.println("Warning! The batch file preparation requires at least the input of one analysis file for this selection");
          return;
        }
        if (analysisFiles.size() == i)
          analysisFiles.addElement(analysisFiles.elementAt(i - 1));
      } else if (selAnalysisLoad == 1) {
        // from previous
        if (analysisFiles.size() == 0) {
          System.out.println("Warning! The batch file preparation requires at least the input of one analysis file for this selection");
          return;
        }
        if (i > 0) {
          if (i < analysisFiles.size())
            analysisFiles.setElementAt(saveFiles.elementAt(i - 1), i);
          else
            analysisFiles.addElement(saveFiles.elementAt(i - 1));
        }
      } else if (selAnalysisLoad == 2) {
        // generate
        if (i == 0)
          analysisFiles.removeAllElements();
        analysisFiles.addElement(generateString(analysisTF[0].getText(), analysisTF[1].getText(), analysisTF[2].getText(), i));
      }

      if (selDatafiles == 0) {
        if (dataFiles.size() == 0) {
          System.out.println("Warning! The batch file preparation requires at least the input of one datafile for this selection");
          return;
        }
        if (i == dataFiles.size())
          dataFiles.addElement(dataFiles.elementAt(i - 1));
      } else if (selDatafiles == 1) {
        if (i == 0)
          dataFiles.removeAllElements();
      } else if (selDatafiles == 2) {
        if (i == 0)
          dataFiles.removeAllElements();
        dataFiles.addElement(generateString(datafileTF[0].getText(), datafileTF[1].getText(), datafileTF[2].getText(), i));
      }

      if (selAnalysisSave == 0) {
        if (i == 0)
          saveFiles.removeAllElements();
        saveFiles.addElement(analysisFiles.elementAt(i));
      } else if (selAnalysisSave == 1) {
        if (i == 0)
          saveFiles.removeAllElements();
      } else if (selAnalysisSave == 2) {
        if (i == 0)
          saveFiles.removeAllElements();
        saveFiles.addElement(generateString(saveTF[0].getText(), saveTF[1].getText(), saveTF[2].getText(), i));
      }

      if (selTitles == 0) {
        if (i == 0)
          titles.removeAllElements();
      } else if (selTitles == 1) {
        if (i == 0)
          titles.removeAllElements();
        titles.addElement(generateString(titleTF[0].getText(), titleTF[1].getText(), titleTF[2].getText(), i));
      }
    }
    getOtherFields();

    ((AbstractTableModel) datatable.getModel()).fireTableDataChanged();
  }

  public void getOtherFields() {

    workingDir = filenameField.getText();

    iterations = Integer.parseInt(iterationsTF.getText());
    wizard = wizardCB.getSelectedIndex() - 1;
    results = resultsTF.getText();
    simpleResults = simpleResultsTF.getText();
    outputDataV.removeAllElements();
    if (saveOutCB.isSelected())
      outputDataV = generateList(outTF[0].getText(), outTF[1].getText(), outTF[2].getText());
    plotFileV.removeAllElements();
    if (savePlotCB.isSelected())
      plotFileV = generateList(plotTF[0].getText(), plotTF[1].getText(), plotTF[2].getText());

  }

  public String generateString(String prefix, String increment, String postfix, int index) {
    StringBuffer buffer = new StringBuffer();
    buffer.append(getStringFrom(prefix, index));
    buffer.append(getStringFrom(increment, index));
    buffer.append(getStringFrom(postfix, index));
    return buffer.toString();
  }

  public Vector<String> generateList(String prefix, String increment, String postfix) {
    int maxRows = datatable.getModel().getRowCount();
    Vector<String> result = new Vector<>(maxRows);
    for (int i = 0; i < maxRows; i++) {
      StringBuffer buffer = new StringBuffer();
      buffer.append(getStringFrom(prefix, i));
      buffer.append(getStringFrom(increment, i));
      buffer.append(getStringFrom(postfix, i));
      result.add(buffer.toString());
    }
    return result;
  }

  public String getStringFrom(String prefix, int i) {
    String result = prefix;
    if (prefix.equalsIgnoreCase("datafile")) {
      result = Misc.getFilenameNoExtension(dataFiles.elementAt(i));
    } else if (prefix.equalsIgnoreCase("analysis")) {
      result = Misc.getFilenameNoExtension(analysisFiles.elementAt(i));
    } else if (isInteger(prefix)) {
      int index = Integer.parseInt(prefix) + i;
      int numberDigits = prefix.length();
      result = Misc.getIntStringFormattedFullZeros(index, numberDigits);
    }
    return result;
  }

  public static boolean isInteger(String str) {
    try {
      Integer.parseInt(str);
      return true;
    } catch (NumberFormatException e) {
      return false;
    }
  }

  public int getRadioButtonSelection(JRadioButton[] radio) {
    for (int i = 0; i < radio.length; i++) {
      if (radio[i].isSelected())
        return i;
    }
    return 0;
  }

  public void saveFile() {
    String filename = Utility.browseFilenametoSave(BatchFileDialog.this, "Save the batch instruction file");
    if (filename != null && analysisFiles.size() > 0) {
      getOtherFields();

      int increment = 1;

      try {
        BufferedWriter outputBuffer = new BufferedWriter(new FileWriter(filename));

        outputBuffer.write("_maud_working_directory");
        outputBuffer.newLine();
        outputBuffer.write(" " + Misc.addQuotesToStringWithBlank(workingDir));
        outputBuffer.newLine();
        outputBuffer.newLine();

        outputBuffer.write("loop_");
        outputBuffer.newLine();
        outputBuffer.write("_riet_analysis_file");
        outputBuffer.newLine();
        if (dataFiles.size() > 0) {
          outputBuffer.write("_riet_meas_datafile_name");
          outputBuffer.newLine();
        }
        if (saveFiles.size() > 0) {
          outputBuffer.write("_riet_analysis_fileToSave");
          outputBuffer.newLine();
        }
        outputBuffer.write("_riet_analysis_iteration_number");
        outputBuffer.newLine();
        if (wizard >= 0) {
          outputBuffer.write("_riet_analysis_wizard_index");
          outputBuffer.newLine();
        }
        if (titles.size() > 0) {
          outputBuffer.write("_publ_section_title");
          outputBuffer.newLine();
        }
        if (simpleResults.length() > 0) {
          outputBuffer.write("_riet_append_simple_result_to");
          outputBuffer.newLine();
        }
        if (results.length() > 0) {
          outputBuffer.write("_riet_append_result_to");
          outputBuffer.newLine();
        }
        if (outputDataV.size() > 0) {
          outputBuffer.write("_maud_output_diff_data_filename");
          outputBuffer.newLine();
        }
        if (plotFileV.size() > 0) {
          outputBuffer.write("_maud_output_plot_filename");
          outputBuffer.newLine();
        }
        for (int i = 0; i < analysisFiles.size(); i++) {
          outputBuffer.write(" " + Misc.addQuotesToStringWithBlank(analysisFiles.elementAt(i)) + " ");
          if (dataFiles.size() > i)
            outputBuffer.write(Misc.addQuotesToStringWithBlank(dataFiles.elementAt(i)) + " ");
          if (saveFiles.size() > 0)
            outputBuffer.write(Misc.addQuotesToStringWithBlank(saveFiles.elementAt(i)) + " ");
          outputBuffer.write(iterations + " ");
          if (wizard >= 0)
            outputBuffer.write(wizard + " ");
          if (titles.size() > 0)
            outputBuffer.write(Misc.addQuotesToStringWithBlank(titles.elementAt(i)) + " ");
          if (simpleResults.length() > 0)
            outputBuffer.write(Misc.addQuotesToStringWithBlank(simpleResults) + " ");
          if (results.length() > 0)
            outputBuffer.write(Misc.addQuotesToStringWithBlank(results) + " ");
          if (outputDataV.size() > 0)
            outputBuffer.write(Misc.addQuotesToStringWithBlank(outputDataV.elementAt(i)) + " ");
          if (plotFileV.size() > 0)
            outputBuffer.write(Misc.addQuotesToStringWithBlank(plotFileV.elementAt(i)) + " ");
          outputBuffer.newLine();
        }
        outputBuffer.flush();
        outputBuffer.close();
      } catch (IOException ioExcep) {
      }

    }

  }

  public static void main(String[] args) {
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
    } catch (Exception exc) {
      AttentionD.showAlertDialog(new Frame(), "Error loading the Look and Feel!");
    }
    new AnglesGeneratorD();
  }
}
