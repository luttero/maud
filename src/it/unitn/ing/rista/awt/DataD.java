/*
 * @(#)DataD.java created 13/1/1998 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti, All Rights Reserved.
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

// import com.hypernex.analysis.rietveld.PhotoplateConversionToASCII;
import ij.AreaImage;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.data.FdtTransformToMBin;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.StringTokenizer;
import java.util.Vector;

/**
 * Dialog for data file parsing and loading.
 *
 * @version $Revision: 1.18 $, $Date: 2006/12/04 14:30:02 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class DataD extends myJFrame {

  public static AreaImage areaImage = null;

  DataFileSet thedata;
  int fileselected = -1;

  JTextField dataidTF;
  JComboBox InstrumentC;
  JComboBox IntensityExtractorCB;
  JComboBox PositionExtractorCB;
	JComboBox DiffractionCB;
  JComboBox ReflectivityCB;
  JComboBox FluorescenceCB;
  JTextField minTF;
  JTextField maxTF;
  JTextField peakcutoffTF;
	JTextField datasetWeightTF;
  JTextField groupCountTF;
  JSubordSListPane excludedregionP;
  JList datafileL;
  JTextField[] anglesTF;
  JTextField monitorTF;
  JTextField bankIDTF;
	JTextField dateTF;
	JTextField timeTF;
	JTextField weightTF;
  JParameterListPane polynomialP;
  JParameterListPane backgroundChiP;
  JParameterListPane backgroundEtaP;
  JSubordListPane gaussianP;

  JCheckBox plotCB;
	JCheckBox computeCB;
	JCheckBox useCountTimeCB;
  JCheckBox asBkgCB;
  JCheckBox enabledCB;
  JCheckBox replaceCB;
	JCheckBox randomCB;
	JCheckBox noStrainCB;

  JComboBox unitCB;
  JTextField countTimeTF;

  JCheckBox lorentzRestrictedCB;
  JCheckBox backgroundInterpolatedCB;
  JTextField backgroundInterpolationPointsTF;
	JTextField backgroundInterpolationIterationsTF;
  JLabel InstLabel;

  /**
   * Creates a DataD dialog istantiating the visualizaton part.
   * This is the constructor to be used.
   *
   * @param parent  the parent Frame.
   * @param adata   a DataFileSet to visualize and edit.
   */

  public DataD(Frame parent, DataFileSet adata) {


    super(parent);

    initializeSizeAndPosition(
            false, "dataFrame.frameWidth", "dataFrame.frameHeight", 400, 500,
            true, "dataFrame.framePositionX", "dataFrame.framePositionY", 50, 50);

    if (areaImage != null) {
      areaImage.setDataSet(null);
      areaImage.setVisible(false);
    }

    if (parent != null)
      parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));

    // add the default menuBar
    createDefaultMenuBar();

    JPanel p1, p2, p3, p4, p5, p6;
    JScrollPane sp1;

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout(3, 3));

    // create Buttons on the lower part of the panel
    p1 = new JPanel();
    p1.setLayout(new FlowLayout());
    c1.add(p1, BorderLayout.CENTER);

    p2 = new JPanel();
    p2.setLayout(new BorderLayout(3, 3));
    c1.add(p2, BorderLayout.SOUTH);

    JPanel southpanel = new JPanel();
    southpanel.setLayout(new BorderLayout());
    p2.add(BorderLayout.SOUTH, southpanel);

    JPanel panel1 = new JPanel();
    panel1.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
    southpanel.add(BorderLayout.WEST, panel1);

    setHelpButton(panel1);
    setHelpFilename("DataFormat.txt");

    JPanel closebuttonPanel = new JPanel();
    closebuttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    southpanel.add(closebuttonPanel, BorderLayout.EAST);

    JButton jbok1 = new JCloseButton();
    closebuttonPanel.add(jbok1);
    jbok1.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        retrieveParameters();
        setVisible(false);
        dispose();
      }
    });
    getRootPane().setDefaultButton(jbok1);

    JTabbedPane tp1 = new JTabbedPane();
    p1.add(tp1);
    String tp1String[] = {"General", "Datafiles", "Excluded regions",
                          "Background function"};

    // General TabPanel
    p3 = new JPanel();
    p3.setLayout(new FlowLayout());
    tp1.addTab(tp1String[0], null, p3);

    p2 = new JPanel();
    p2.setLayout(new BorderLayout());
    p3.add(p2);

    p3 = new JPanel();
    p3.setLayout(new GridLayout(0, 2, 3, 3));
    p2.add(p3, BorderLayout.NORTH);

    JPanel jp6 = new JPanel(new FlowLayout());
    p3.add(jp6);
    jp6.add(new JLabel("Data block id:"));
    dataidTF = new JTextField(Constants.FLOAT_FIELD);
    dataidTF.setText("data block 1");
    jp6.add(dataidTF);

    enabledCB = new JCheckBox("Computation enabled");
    p3.add(enabledCB);

    randomCB = new JCheckBox("Force random texture");
    randomCB.setToolTipText("If marked, spectra will be treated as for no texture, independently from other settings");
    p3.add(randomCB);
	  noStrainCB = new JCheckBox("Force no strain");
	  noStrainCB.setToolTipText("If marked, spectra will be treated as having no strain, independently from other settings");
	  p3.add(noStrainCB);

	  replaceCB = new JCheckBox("Replace datafile on add");
	  replaceCB.setToolTipText("When adding a new datafile, the old one/ones is/are removed");
	  p3.add(replaceCB);

	  jp6 = new JPanel(new FlowLayout());
    p3.add(jp6);
    jp6.add(new JLabel("Peak cutoff:"));
    peakcutoffTF = new JTextField(Constants.FLOAT_FIELD);
    peakcutoffTF.setToolTipText("Cutoff for peak calculation in HWHM units");
    jp6.add(peakcutoffTF);

	  jp6 = new JPanel(new FlowLayout());
	  p3.add(jp6);
	  jp6.add(new JLabel("Dataset weight:"));
	  datasetWeightTF = new JTextField(Constants.FLOAT_FIELD);
	  datasetWeightTF.setToolTipText("Set the relative weight respect to other datasets");
	  jp6.add(datasetWeightTF);

	  p3 = new JPanel();
    p3.setLayout(new GridLayout(0, 2, 3, 3));
    p2.add(p3, BorderLayout.CENTER);

    jp6 = new JPanel(new FlowLayout());
    jp6.setBorder(new TitledBorder(
            new BevelBorder(BevelBorder.LOWERED), "Instrument"));
    p3.add(jp6);

    JPanel leftpanel = new JPanel(new GridLayout(0, 1, 3, 3));
    jp6.add(leftpanel);
    JPanel comboP = new JPanel(new FlowLayout(FlowLayout.CENTER, 0, 0));
    leftpanel.add(comboP);
    comboP.add(new JLabel("Type:"));
    InstrumentC = new JComboBox();
    InstrumentC.setEditable(false);
    InstrumentC.setMaximumRowCount(4);
    comboP.add(InstrumentC);

    comboP = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
    leftpanel.add(comboP);
    comboP.add(new JLabel("Name: "));
    comboP.add(InstLabel = new JLabel(""));

    JPanel instButtonsP = new JPanel(new GridLayout(0, 1, 1, 1));
    jp6.add(instButtonsP);
    JButton jbi = new JIconButton("Eyeball.gif", "Edit");
    jbi.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        InstrumentOptions();
      }
    });
    instButtonsP.add(jbi);
    jbi = new JIconButton("DataExtract.gif", "Import...");
    jbi.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        ImportInstrument();
      }
    });
    instButtonsP.add(jbi);
    jbi = new JIconButton("DataStore.gif", "Store...");
    jbi.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        StoreInstrument();
      }
    });
    instButtonsP.add(jbi);

    JPanel bP2 = new JPanel();
    p3.add(bP2);
    bP2.setBorder(new TitledBorder(
            new BevelBorder(BevelBorder.LOWERED), "Computation range"));
    bP2.setLayout(new FlowLayout());
    p6 = new JPanel();
    p6.setLayout(new GridLayout(0, 2, 3, 3));
    bP2.add(p6);
    p6.add(new JLabel("Min in data units:"));
    minTF = new JTextField(Constants.FLOAT_FIELD);
    minTF.setText("0");
    p6.add(minTF);
    p6.add(new JLabel("Max in data units:"));
    maxTF = new JTextField(Constants.FLOAT_FIELD);
    maxTF.setText("0");
    p6.add(maxTF);
    p6.add(new JLabel("Data in groups of:"));
    groupCountTF = new JTextField(Constants.FLOAT_FIELD);
    groupCountTF.setToolTipText("Merge datapoints in datafiles in groups of the specified number, " +
        "to reduce datapoints");
    p6.add(groupCountTF);
/*    JButton applyB = new JIconButton("Cut.gif", "Apply now");
    applyB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        changeRange();
      }
    });
    applyB.setToolTipText("Force the new range setting now; Warning, if option lower memory occupation is on " +
        "(in options), the data out of the range will be removed");
    bP2.add(applyB);*/

    p5 = new JPanel();
    p5.setLayout(new FlowLayout(FlowLayout.RIGHT, 1, 1));
    p2.add(BorderLayout.SOUTH, p5);
    p6 = new JPanel();
    p6.setLayout(new GridLayout(0, 2, 1, 1));
    p5.add(p6);

    p4 = new JPanel();
    p4.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    p6.add(p4);
    p4.add(new JLabel("Intensity extractor:"));
    IntensityExtractorCB = new JComboBox();
    IntensityExtractorCB.setToolTipText("Select the intensity extractor method");
    p4.add(IntensityExtractorCB);
    JButton jb = new JIconButton("Eyeball.gif", "Options");
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        IntensityExtractorOptions();
      }
    });
    p4.add(jb);

    p4 = new JPanel();
    p4.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    p6.add(p4);
    p4.add(new JLabel("Position extractor:"));
    PositionExtractorCB = new JComboBox();
    PositionExtractorCB.setToolTipText("Select the position extractor method");
    p4.add(PositionExtractorCB);
    jb = new JIconButton("Eyeball.gif", "Options");
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        PositionExtractorOptions();
      }
    });
    p4.add(jb);

	  p4 = new JPanel();
	  p4.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
	  p6.add(p4);
	  p4.add(new JLabel("Diffraction model:"));
	  DiffractionCB = new JComboBox();
	  DiffractionCB.setToolTipText("Select the diffraction computation");
	  p4.add(DiffractionCB);
	  jb = new JIconButton("Eyeball.gif", "Options");
	  jb.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent event) {
			  DiffractionOptions();
		  }
	  });
	  p4.add(jb);

	  p4 = new JPanel();
    p4.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    p6.add(p4);
    p4.add(new JLabel("Reflectivity model:"));
    ReflectivityCB = new JComboBox();
    ReflectivityCB.setToolTipText("Select the reflectivity method");
    p4.add(ReflectivityCB);
    jb = new JIconButton("Eyeball.gif", "Options");
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        ReflectivityOptions();
      }
    });
    p4.add(jb);

    p4 = new JPanel();
    p4.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    p6.add(p4);
    p4.add(new JLabel("Fluorescence model:"));
    FluorescenceCB = new JComboBox();
    FluorescenceCB.setToolTipText("Select the fluorescence method");
    p4.add(FluorescenceCB);
    jb = new JIconButton("Eyeball.gif", "Options");
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        FluorescenceOptions();
      }
    });
    p4.add(jb);

    p4 = new JPanel();
    p4.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    p6.add(p4);
    lorentzRestrictedCB = new JCheckBox("Lorentz restricted");
    lorentzRestrictedCB.setToolTipText(
        "Check this box to compute Lorentz polarization on the centroid of the peaks, and not point by point");
    p4.add(lorentzRestrictedCB);

    // datafiles list tabPanel

    p2 = new JPanel();
    p2.setLayout(new FlowLayout(FlowLayout.CENTER, 0, 0));
    tp1.addTab(tp1String[1], null, p2);

    JPanel datafileP = new JPanel();
    datafileP.setLayout(new BorderLayout(0, 0));
    p2.add(datafileP);

    p5 = new JPanel();
    p5.setLayout(new BorderLayout(0, 0));
    datafileP.add(p5, BorderLayout.NORTH);


    p3 = new JPanel();
    p3.setLayout(new BorderLayout());
    p3.setBorder(new TitledBorder(
            new EtchedBorder(EtchedBorder.LOWERED), "Spectra list"));
    p5.add(BorderLayout.CENTER, p3);

    datafileL = new JList();
    new FileDrop(datafileL, new FileDrop.Listener() {
      public void filesDropped( java.io.File[] files ) {
                  // handle file drop
        datafilesDropped(files);
      }   // end filesDropped
    }); // end FileDrop.Listener
    datafileL.setVisibleRowCount(7);
    datafileL.setPrototypeCellValue("123456789012345678901234567890");
    datafileL.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    sp1 = new JScrollPane();
//		sp1.setBorder(new LineBorder(Color.black));
    sp1.getViewport().add(datafileL);
    p3.add(sp1, BorderLayout.CENTER);

    // Button panel for datafile list
    jp6 = new JPanel();
    jp6.setLayout(new FlowLayout());
    p3.add(jp6, BorderLayout.WEST);
    p4 = new JPanel();
    p4.setLayout(new GridLayout(0, 1, 3, 3));
    jp6.add(p4);

    JIconButton addFileB = new JIconButton("Open.gif", "Browse..");
    p4.add(addFileB);
    addFileB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        loadDataFile();
      }
    });
    addFileB.setToolTipText("Load datafile/s (different formats supported)");

    addFileB = new JIconButton("Paint.gif", "From images..");
    p4.add(addFileB);
    addFileB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        showAreaDetector();
      }
    });
    addFileB.setToolTipText("Show the Image manager for importing film or area detector data");

    final JRemoveButton removeFileB = new JRemoveButton();
    p4.add(removeFileB);
    removeFileB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        if (!Constants.confirmation || Utility.areYouSureToRemove("Remove the selected datafiles?"))
          removeDataFile();
      }
    });
    removeFileB.setToolTipText("Remove the selected spectra from the list");

    JIconButton viewFileB = new JIconButton("LineGraph.gif", "View");
    p4.add(viewFileB);
    viewFileB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        viewSelectedDataFile();
      }
    });
    viewFileB.setToolTipText("Open a plot window for the selected spectra");

    JButton moreB = new JIconButton("Eyeball.gif", "Additional par");
    p4.add(moreB);
    moreB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        moreDiffractionDatafileOptions();
      }
    });
    moreB.setToolTipText("Edit the additional parameters of the selected spectrum");

    JButton rbd = new JIconButton("Bulb.gif", "Bound Spectra");
    p4.add(rbd);
    rbd.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        boundSelectedSpectra();
      }
    });
    rbd.setToolTipText("Bound all parameters of selected spectra to the first selected spectrum");


    JPanel jp4 = new JPanel();
    jp4.setLayout(new BorderLayout(0, 0));
    jp4.setBorder(new TitledBorder(
            new EtchedBorder(EtchedBorder.LOWERED), "Command shortcuts"));
    datafileP.add(jp4, BorderLayout.SOUTH);

    p4 = new JPanel();
    p4.setLayout(new GridLayout(0, 4, 1, 1));
    jp4.add(p4, BorderLayout.NORTH);

    JIconButton importB = new JIconButton("DocumentIn.gif", "Importing script..");
    p4.add(importB);
    importB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        importDataFileList();
      }
    });
    importB.setToolTipText("Load an instruction file in CIF format containing a list of datafiles");

    rbd = new JIconButton("Plus.gif", "Add bkg par");
    p4.add(rbd);
    rbd.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        addAdditionalBackground();
      }
    });
    rbd.setToolTipText("Add one parameter to the additional background of the selected spectra");

    rbd = new JRemoveButton("Remove bkg par");
    p4.add(rbd);
    rbd.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        removeAdditionalBackground();
      }
    });
    rbd.setToolTipText("Remove all parameters of the additional background of the selected spectra");

    rbd = new JIconButton("Plus.gif", "Add shift par");
    p4.add(rbd);
    rbd.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        addAdditionalShift();
      }
    });
    rbd.setToolTipText("Add one shift parameter to all selected spectra");

    rbd = new JRemoveButton("Remove shift par");
    p4.add(rbd);
    rbd.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        removeAdditionalShift();
      }
    });
    rbd.setToolTipText("Remove all additional shift parameters of the selected spectra");

/*    final JRemoveButton rbd1 = new JRemoveButton("Remove all");
    p4.add(rbd1);
    rbd1.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        if (!Constants.confirmation || Utility.areYouSureToRemove("Remove all the datafiles?"))
          removeAllFiles();
      }
    });
    rbd1.setToolTipText("Remove all spectra from the list");

    JRemoveButton rbdx = new JRemoveButton("Disable all");
    p4.add(rbdx);
    rbdx.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        disableAllFiles();
      }
    });
    rbdx.setToolTipText("Disable all spectra of the list");

    JCloseButton rbdx1 = new JCloseButton("Enable all");
    p4.add(rbdx1);
    rbdx1.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        enableAllFiles();
      }
    });
    rbdx1.setToolTipText("Enable all spectra of the list");*/

    final JRemoveButton rbd2 = new JRemoveButton("Remove disabled");
    p4.add(rbd2);
    rbd2.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        if (!Constants.confirmation || Utility.areYouSureToRemove("Remove all the disabled datafiles?"))
          removeAllDisabledFiles();
      }
    });
    rbd2.setToolTipText("Remove all disabled spectra from the list");

/*    rbd = new JIconButton("Plus.gif", "Add bkg par to All");
    p4.add(rbd);
    rbd.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        addAdditionalBackgroundToAll();
      }
    });
    rbd.setToolTipText("Add one parameter to the additional background of all spectra");*/

    viewFileB = new JIconButton("LineGraph.gif", "View summed");
        p4.add(viewFileB);
        viewFileB.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent event) {
            viewSelectedDataFileSummed();
          }
        });
        viewFileB.setToolTipText("Open a plot window for the selected spectra as summed");


    JButton sumB = new JButton("Sum spectra");
    p4.add(sumB);
    sumB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        SumDatafileOutput();
      }
    });
    sumB.setToolTipText("Save in a file a mean spectrum of all the selected spectra");

    sumB = new JButton("Sum spectra by rules");
    p4.add(sumB);
    sumB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        SumDatafileOutputRules();
      }
    });
    sumB.setToolTipText("Save in a file a mean spectrum of enabled datafiles based on rules");

    sumB = new JButton("Modify Angles");
    p4.add(sumB);
    sumB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        changeAngles();
      }
    });
    sumB.setToolTipText("Modify the tilting angles for all selected datafiles");

    sumB = new JButton("Plot Int. hystogram");
    p4.add(sumB);
    sumB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        plotIntensityHystogram();
      }
    });
    sumB.setToolTipText("Plot the hystogram of total intensity of each enabled spectrum");

    sumB = new JButton("Sort by angles");
    p4.add(sumB);
    sumB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        sortByAngles();
      }
    });
    sumB.setToolTipText("Sort the spectra by angles");

    sumB = new JButton("Sort by name");
    p4.add(sumB);
    sumB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        sortByName();
      }
    });
    sumB.setToolTipText("Sort the spectra by name");

    sumB = new JButton("Sort by bank number");
    p4.add(sumB);
    sumB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        sortByBankNumber();
      }
    });
    sumB.setToolTipText("Sort the spectra by the bank number if any");

    sumB = new JButton("Disable/group by rules");
    p4.add(sumB);
    sumB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        removeByAngles();
      }
    });
    sumB.setToolTipText("Disable or group together spectra by rules");

	  sumB = new JButton("Set angles by time/index");
	  p4.add(sumB);
	  sumB.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent event) {
			  setAnglesByMeasurementTime();
		  }
	  });
	  sumB.setToolTipText("Set the measurement angles based on measurement time or spectrum index");

	  // Fields and options for the selected datafile

    JPanel jp1 = new JPanel();
    jp1.setLayout(new FlowLayout());
    jp1.setBorder(new TitledBorder(
            new EtchedBorder(EtchedBorder.LOWERED), "Selected spectrum options"));
    p5.add(BorderLayout.EAST, jp1);

    p4 = new JPanel();
    p4.setLayout(new BorderLayout(0, 0));
    jp1.add(p4);
    p3 = new JPanel();
    p3.setLayout(new GridLayout(0, 1, 1, 1));
	  String[] angleLabels = {"Omega:", "Chi:", "Phi:", "Eta:", "2Theta:", "Energy:"};
	  anglesTF = new JTextField[angleLabels.length];
	  for (int j = 0; j < angleLabels.length; j++) {
		  p6 = new JPanel();
		  p6.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 1));
		  p3.add(p6);
		  p6.add(new JLabel(angleLabels[j]));
		  anglesTF[j] = new JTextField(Constants.FLOAT_FIELD);
		  anglesTF[j].setText("0");
		  p6.add(anglesTF[j]);
	  }
    p4.add(BorderLayout.WEST, p3);

    p3 = new JPanel();
    p3.setLayout(new GridLayout(0, 1, 3, 1));
    p4.add(BorderLayout.EAST, p3);

    p6 = new JPanel();
    p6.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
    computeCB = new JCheckBox("Enabled");
    p6.add(computeCB);
    computeCB.setToolTipText("Check this box to enable the selected spectrum in the dataset");
    computeCB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        enableAllSelectedFiles(computeCB.isSelected());
      }
    });
    p3.add(p6);

    p6 = new JPanel();
    p6.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
    plotCB = new JCheckBox("Fitting file output");
    p6.add(plotCB);
    plotCB.setToolTipText("If checked a file with the fitting data will be saved");
    plotCB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        fittingOutputSelectedFiles(plotCB.isSelected());
      }
    });
    p3.add(p6);

	  p6 = new JPanel();
	  p6.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
	  asBkgCB = new JCheckBox("As background");
	  p6.add(asBkgCB);
	  asBkgCB.setToolTipText("Check this box to use the selected spectrum as exp background for the others");
	  asBkgCB.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent event) {
			  asBkgAllSelectedFiles(asBkgCB.isSelected());
		  }
	  });
	  p3.add(p6);

	  p6 = new JPanel();
	  p6.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
	  useCountTimeCB = new JCheckBox("Correct for count time");
	  p6.add(useCountTimeCB);
	  useCountTimeCB.setToolTipText("Check this box to correct pattern scale factor for counting time");
	  useCountTimeCB.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent event) {
			  useCountTimeAllSelectedFiles(useCountTimeCB.isSelected());
		  }
	  });
	  p3.add(p6);

	  p3 = new JPanel();
    p3.setLayout(new GridLayout(0, 2, 3, 1));
    p4.add(BorderLayout.SOUTH, p3);

    unitCB = new JComboBox();
    for (int i = 0; i < DiffrDataFile.meas_intensity_unit.length; i++)
      unitCB.addItem(DiffrDataFile.meas_intensity_unit[i]);
    p3.add(unitCB);
    unitCB.setToolTipText("Select measurement mode");
    countTimeTF = new JTextField(12);
    p3.add(countTimeTF);
    countTimeTF.setToolTipText("Set counting time per step");

    p3.add(new JLabel("Monitor Counts: "));
    monitorTF = new JTextField(Constants.FLOAT_FIELD);
    monitorTF.setText("0.0");
    p3.add(monitorTF);

    p3.add(new JLabel("Bank ID: "));
    bankIDTF = new JTextField(Constants.FLOAT_FIELD);
    bankIDTF.setText("-");
    p3.add(bankIDTF);

	  p3.add(new JLabel("Measurement date: "));
	  dateTF = new JTextField(Constants.FLOAT_FIELD);
	  dateTF.setText("1985-01-01");
	  p3.add(dateTF);

	  p3.add(new JLabel("Measurement time: "));
	  timeTF = new JTextField(Constants.FLOAT_FIELD);
	  timeTF.setText("00:00:00");
	  p3.add(timeTF);

	  p3.add(new JLabel("Datafile weight:  "));
	  weightTF = new JTextField(Constants.FLOAT_FIELD);
	  weightTF.setText("1.0");
	  p3.add(weightTF);

	  // Excluded region tabPanel

    excludedregionP = new JSubordSListPane(this, false);
    tp1.addTab(tp1String[2], null, excludedregionP);

    JPanel bkgPanel = new JPanel();
    bkgPanel.setLayout(new BorderLayout(3, 3));
    JTabbedPane tabPanel1 = new JTabbedPane();
    String tempString[] = {"Polynomial", "Interpolated", "Background peaks", "Chi dependent", "Eta dependent"};

    polynomialP = new JParameterListPane(this, false, true);
    tabPanel1.addTab(tempString[0], null, polynomialP);

	  JPanel bkgpTop = new JPanel();
	  bkgpTop.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
	  JPanel bkgp = new JPanel();
    bkgp.setLayout(new BorderLayout(3, 3));
	  bkgpTop.add(bkgp);

    JPanel jpbkg = new JPanel();
    jpbkg.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
    bkgp.add(BorderLayout.NORTH, jpbkg);
    backgroundInterpolatedCB = new JCheckBox("Interpolate background");
    backgroundInterpolatedCB.setToolTipText("Check this box to compute background by interpolation");
    jpbkg.add(backgroundInterpolatedCB);

    jpbkg = new JPanel();
    jpbkg.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
    bkgp.add(BorderLayout.CENTER, jpbkg);
    jpbkg.add(new JLabel("Interval for interpolation (in points):"));
    backgroundInterpolationPointsTF = new JTextField(5);
    jpbkg.add(backgroundInterpolationPointsTF);
    backgroundInterpolationPointsTF.setToolTipText("Set the interval between points in points unit for the interpolation");

	  jpbkg.add(new JLabel("Number of iterations:"));
	  backgroundInterpolationIterationsTF = new JTextField(5);
	  jpbkg.add(backgroundInterpolationIterationsTF);
	  backgroundInterpolationIterationsTF.setToolTipText("Use more iterations for highly overlapped peaks, less for more regular points");

	  jpbkg = new JPanel();
	  bkgp.add(BorderLayout.SOUTH, jpbkg);
/*	  sumB = new JButton("Set points manually");
	  jpbkg.add(sumB);
    sumB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
	      thedata.isAbilitatetoRefresh = false;
        thedata.setBackgroundInterpolated(backgroundInterpolatedCB.isSelected());
        thedata.setInterpolatedPoints(backgroundInterpolationPointsTF.getText());
	      thedata.setBackgroundInterpolationIterations(backgroundInterpolationIterationsTF.getText());
	      removeManualInterpolated();
	      thedata.isAbilitatetoRefresh = true;
        manualInterpolatedPointsShowPlot();
      }
    });
    sumB.setToolTipText("Press this to show a window plot where to modify/set the intepolated point positions (use this only for same range patterns)");
*/
    sumB = new JButton("Remove all points");
        jpbkg.add(sumB);
        sumB.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent event) {
            thedata.setBackgroundInterpolated(backgroundInterpolatedCB.isSelected());
            thedata.setInterpolatedPoints(backgroundInterpolationPointsTF.getText());
	          thedata.setBackgroundInterpolationIterations(backgroundInterpolationIterationsTF.getText());
	          removeManualInterpolated();
          }
    });
    sumB.setToolTipText("Press this to remove all the manually selected background points");

    tabPanel1.addTab(tempString[1], null, bkgpTop);

    gaussianP = new JSubordListPane(this, false);
    tabPanel1.addTab(tempString[2], null, gaussianP);
    tabPanel1.setSelectedIndex(0);

    backgroundChiP = new JParameterListPane(this, false, true);
    tabPanel1.addTab(tempString[3], null, backgroundChiP);

    backgroundEtaP = new JParameterListPane(this, false, true);
    tabPanel1.addTab(tempString[4], null, backgroundEtaP);

    bkgPanel.add(BorderLayout.CENTER, tabPanel1);

    tp1.addTab(tp1String[3], null, bkgPanel);

    setTitle(adata.toXRDcatString());
    thedata = adata;

    // start listener on the datafile list
    datafileL.addListSelectionListener(new ListSelectionListener() {
      public void valueChanged(ListSelectionEvent event) {
        datafileL_ListSelect();
      }
    });

    initparameters();

    pack();

    setResizable(false);

    if (parent != null)
      parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

  }

  public JMenuBar createDefaultMenuBar() {
    JMenuBar amenubar = super.createDefaultMenuBar();
    amenubar.add(createToolsMenu());
    return amenubar;
  }

  public JMenu createToolsMenu() {

    JMenuItem menuitem;

    JMenu toolsMenu = new JMenu("Tools");
    toolsMenu.setMnemonic('t');
    toolsMenu.add(menuitem = new JMenuItem("Transform datafiles (.fdt)"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        transformDataFile();
      }
    });

    toolsMenu.add(menuitem = new JMenuItem("Merge reflectivity spectra (selected)"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        mergeReflectivityDataFile();
      }
    });

    toolsMenu.add(menuitem = new JMenuItem("Read Cyclone information file (*.ana)"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        readCycloneAnaFile();
      }
    });

/*    toolsMenu.add(menuitem = new JMenuItem("Convert SIS image to ASCII"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        (new Thread() {
          public void run() {
            (new PhotoplateConversionToASCII()).performAnalysis();
          }
        }).start();
      }
    });*/

    toolsMenu.add(menuitem = new JMenuItem("Convert 3 column txt image to ASCII"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        (new Thread() {
          public void run() {
            ConvertTextImageFromColumnsToArea.run(DataD.this);
          }
        }).start();
      }
    });


    return toolsMenu;
  }

  /**
   * Initialize the textfields, comboBoxes etc with the data of the
   * DataFileSet. It's called automaticly by the constructor.
   */

  protected void initparameters() {

    minTF.setText(thedata.getMinRange());
    maxTF.setText(thedata.getMaxRange());
    lorentzRestrictedCB.setSelected(thedata.isLorentzRestricted());
    if (!Constants.testing) {
      lorentzRestrictedCB.setSelected(true);
      lorentzRestrictedCB.setEnabled(false);
    }
    backgroundInterpolatedCB.setSelected(thedata.isBackgroundInterpolated());
    backgroundInterpolationPointsTF.setText(thedata.getInterpolatedPoints());
	  backgroundInterpolationIterationsTF.setText(thedata.getInterpolationIterations());
	  dataidTF.setText(thedata.getDataFileSetID());
    peakcutoffTF.setText(thedata.getPeakCutoff());
	  datasetWeightTF.setText(thedata.getDatasetWeightString());
    groupCountTF.setText(thedata.getGroupCount());

    thedata.getDataFileList().setList(datafileL);
    if (thedata.datafilesnumber() > 0)
      thedata.getDataFileList().select(0);
    polynomialP.setList(thedata, 0);

    String labels[] = {"Heigth:", "Position:", "HWHM:", "Eta:",
                       "Position (omega):", "HWHM (omega):", "Eta (omega):",
                       "Position (chi):", "HWHM (chi):", "Eta (chi):",
                       "Position (phi):", "HWHM (phi):", "Eta (phi):",
                       "Position (eta):", "HWHM (eta):", "Eta (eta):"
                      };
    gaussianP.setList(thedata, 1, labels.length, labels);

    backgroundChiP.setList(thedata, 1);
    backgroundEtaP.setList(thedata, 2);

    String exlabels[] = {"Min in data units :", "Max in data units :"};
    excludedregionP.setList(thedata, 0, exlabels.length, exlabels);

//    FilePar thefile = thedata.getFilePar();
//    for (int i = 0; i < thefile.instrumentsnumber(); i++)
//      InstrumentC.addItem(thefile.getinstrument(i).toXRDcatString());
//    InstrumentC.setSelectedItem(thedata.getInstrument());

    for (int i = 0; i < thedata.getsubordClassNumber(thedata.getInstrumentID()); i++) {
      InstrumentC.addItem(thedata.getsubordIdentifier(thedata.getInstrumentID(), i));
    }
    InstrumentC.setSelectedItem(thedata.getInstrument().identifier);
    InstLabel.setText(thedata.getInstrument().toXRDcatString());
    for (int i = 0; i < thedata.getsubordClassNumber(thedata.getIntensityExtractorID()); i++) {
      IntensityExtractorCB.addItem(thedata.getsubordIdentifier(thedata.getIntensityExtractorID(), i));
    }
    IntensityExtractorCB.setSelectedItem(thedata.getIntensityExtractorMethod());
    for (int i = 0; i < thedata.getsubordClassNumber(thedata.getPositionExtractorID()); i++) {
      PositionExtractorCB.addItem(thedata.getsubordIdentifier(thedata.getPositionExtractorID(), i));
    }
    PositionExtractorCB.setSelectedItem(thedata.getPositionExtractorMethod());
    for (int i = 0; i < thedata.getsubordClassNumber(thedata.getDiffractionID()); i++) {
	    DiffractionCB.addItem(thedata.getsubordIdentifier(thedata.getDiffractionID(), i));
    }
	  DiffractionCB.setSelectedItem(thedata.getDiffractionMethod());
	  for (int i = 0; i < thedata.getsubordClassNumber(thedata.getReflectivityID()); i++) {
		  ReflectivityCB.addItem(thedata.getsubordIdentifier(thedata.getReflectivityID(), i));
	  }
	  ReflectivityCB.setSelectedItem(thedata.getReflectivityMethod());
    for (int i = 0; i < thedata.getsubordClassNumber(thedata.getFluorescenceID()); i++) {
      FluorescenceCB.addItem(thedata.getsubordIdentifier(thedata.getFluorescenceID(), i));
    }
    FluorescenceCB.setSelectedItem(thedata.getFluorescenceMethod());
    enabledCB.setSelected(thedata.isEnabled());
    enabledCB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        thedata.setEnabled(enabledCB.isSelected());
      }
    });
    replaceCB.setSelected(thedata.replaceDatafile());
    replaceCB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        thedata.setReplaceDatafile(replaceCB.isSelected());
      }
    });
    randomCB.setSelected(thedata.hasRandomTexture());
    randomCB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        thedata.setRandomTexture(randomCB.isSelected());
      }
    });
	  noStrainCB.setSelected(thedata.hasNoStrain());
	  noStrainCB.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent e) {
			  thedata.setNoStrain(noStrainCB.isSelected());
		  }
	  });
  }

  /**
   * Retrieve the actual content of the fields to update the DataFileSet
   * obiect. In response to the press of the Ok button on close of the
   * frame.
   */

  public void retrieveParameters() {
    super.retrieveParameters();
    DiffrDataFile datafile = thedata.getSelectedDataFile();
    if (datafile != null) {
	    for (int j = 0; j < DiffrDataFile.maxAngleNumber; j++)
        datafile.setString(j + 1, anglesTF[j].getText());
			datafile.setCompute(computeCB.isSelected());
      datafile.setAsBackground(asBkgCB.isSelected());
	    datafile.useCountTimeToScale(useCountTimeCB.isSelected());
      datafile.setGeneratePlotfile(plotCB.isSelected());
      datafile.setIntensityUnit(unitCB.getSelectedItem().toString());
      datafile.setCountTime(countTimeTF.getText());
      datafile.setMonitorCounts(monitorTF.getText());
      datafile.setBankID(bankIDTF.getText());
	    datafile.setMeasurementDate(dateTF.getText());
	    datafile.setMeasurementTime(timeTF.getText());
	    datafile.setDatafileWeight(weightTF.getText());
      removeComponentfromlist(monitorTF);
    }
    polynomialP.retrieveparlist();
    gaussianP.retrieveparlist();
    backgroundChiP.retrieveparlist();
    backgroundEtaP.retrieveparlist();
    excludedregionP.retrieveparlist();
    thedata.setInstrument(InstrumentC.getSelectedItem().toString());
    thedata.setIntensityExtractor(IntensityExtractorCB.getSelectedItem().toString());
    thedata.setPositionExtractor(PositionExtractorCB.getSelectedItem().toString());
	  thedata.setDiffraction(DiffractionCB.getSelectedItem().toString());
    thedata.setReflectivity(ReflectivityCB.getSelectedItem().toString());
    thedata.setFluorescence(FluorescenceCB.getSelectedItem().toString());

    thedata.setMinRange(minTF.getText());
    thedata.setMaxRange(maxTF.getText());
    thedata.setLorentzRestricted(lorentzRestrictedCB.isSelected());
    thedata.setBackgroundInterpolated(backgroundInterpolatedCB.isSelected());
    thedata.setInterpolatedPoints(backgroundInterpolationPointsTF.getText());
	  thedata.setBackgroundInterpolationIterations(backgroundInterpolationIterationsTF.getText());

    thedata.setDataFileSetID(dataidTF.getText());
    thedata.setPeakCutoff(peakcutoffTF.getText());
	  thedata.setDatasetWeight(datasetWeightTF.getText());
    thedata.setGroupCount(Integer.parseInt(groupCountTF.getText()));
    thedata.refreshAll(false);
	  thedata.forceRangeCut();
  }

  /**
   * @return  the DataFileSet object under visual editing.
   */

  public DataFileSet getData() {
    return thedata;
  }

  void changeRange() {
    thedata.setGroupCount(Integer.parseInt(groupCountTF.getText()));
    thedata.refreshAll(false);
    thedata.setMinRange(minTF.getText());
    thedata.setMaxRange(maxTF.getText());
    thedata.forceRangeCut();
  }

  /**
   * Refresh the visible fields in response to a change in the datafile
   * list selection.
   */

  public void datafileL_ListSelect() {
    if (thedata == null)
      return;
    DiffrDataFile datafile;

    if (fileselected >= 0 && fileselected < thedata.getDataFileList().size()) {
      datafile = thedata.getDataFile(fileselected);
	    for (int j = 0; j < DiffrDataFile.maxAngleNumber; j++)
		    datafile.setString(j + 1, anglesTF[j].getText());
			datafile.setCompute(computeCB.isSelected());
      datafile.setAsBackground(asBkgCB.isSelected());
	    datafile.useCountTimeToScale(useCountTimeCB.isSelected());
      datafile.setGeneratePlotfile(plotCB.isSelected());
      datafile.setIntensityUnit(unitCB.getSelectedItem().toString());
      datafile.setCountTime(countTimeTF.getText());
      datafile.setMonitorCounts(monitorTF.getText());
      removeComponentfromlist(monitorTF);
      datafile.setBankID(bankIDTF.getText());
    }
    fileselected = datafileL.getSelectedIndex();
    datafile = thedata.getSelectedDataFile();
    if (datafile != null) {
	    for (int j = 0; j < DiffrDataFile.maxAngleNumber; j++)
		    anglesTF[j].setText(Misc.getFormattedValue(Double.parseDouble(datafile.getString(j + 1))));
      computeCB.setSelected(datafile.getComputePermission());
      asBkgCB.setSelected(datafile.getAsBackgroundPermission());
	    useCountTimeCB.setSelected(datafile.useCountTimeToScale());
      plotCB.setSelected(datafile.getPlotfilePermission());
      unitCB.setSelectedItem(datafile.getIntensityUnit());
      countTimeTF.setText(datafile.getCountTime());
      addComponenttolist(monitorTF, datafile.getMonitorCounts());
      monitorTF.setText(datafile.getMonitorCounts().getValue());
      bankIDTF.setText(datafile.getBankID());
	    dateTF.setText(datafile.getMeasurementDate());
	    timeTF.setText(datafile.getMeasurementTime());
	    weightTF.setText(datafile.getDatafileWeightString());
    }
  }

  /**
   * Open the a window to edit the additional background associated with
   * the selected datafile.
   */

  void moreDiffractionDatafileOptions() {
    DiffrDataFile datafile = thedata.getSelectedDataFile();
    if (datafile != null)
      datafile.getOptionsDialog(this).setVisible(true);
  }

  /**
   * Browse a single datafile to load in the DataFileSet.
   */

  public void loadDataFile() {
    String[] filename = Utility.browseFilenames(this, "Load data file");
    if (filename != null) {
      setCursor(new Cursor(Cursor.WAIT_CURSOR));
      Constants.refreshTreePermitted = false;
      for (int i = 0; i < filename.length; i++)
        thedata.addDataFileforName(filename[i], true);
      Constants.refreshTreePermitted = true;
      thedata.notifyUpObjectChanged(thedata, 0);
      setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    }
  }

  public void transformDataFile() {
    String filename = Utility.browseFilename(this, "Load fdt data file to transform");
    if (filename != null) {
      FdtTransformToMBin.readallSpectra(filename);
    }
  }

  public void mergeReflectivityDataFile() {
    thedata.mergeReflectivityDatafile(this);
  }

  /**
   * Import a CIF file containing the datafiles name and options to be
   * loaded.
   */

  public void importDataFileList() {
    String filename = Utility.browseFilename(this, "Import CIF list of data files");

    setCursor(new Cursor(Cursor.WAIT_CURSOR));

    thedata.addDatafilesFromScript(filename);

	setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
  }

  private void datafilesDropped(File[] files) {
    if (files != null && files.length > 0) {
      for (int i = 0; i < files.length; i++) {
        try {
          thedata.addDataFileforName(files[i].getCanonicalPath(), true);
        } catch (IOException e) {
	        e.printStackTrace();
          // LogSystem.printStackTrace(e);
        }
      }
    }
  }

  /**
   * Remove the selected datafile from the list.
   */

  public void removeDataFile() {
    fileselected = -1;
    thedata.removeSelectedDataFile();
  }

  /**
   * Remove all the disable datafiles from the list.
   */

  public void removeAllDisabledFiles() {
    thedata.removeAllDisabledFiles();
  }

  /**
   * Remove all the datafiles from the list.
   */

  public void removeAllFiles() {
    thedata.removeAllFiles();
  }

  /**
   * Disable all the datafiles of the list.
   */

  public void disableAllFiles() {
    fileselected = -1;
    thedata.setStatusAllFiles(false);
  }

  /**
   * Enable all the datafiles of the list.
   */

  public void enableAllFiles() {
    fileselected = -1;
    thedata.setStatusAllFiles(true);
  }

  /**
   * Enable/disable all the datafiles of the list.
   * @param enable true or false to enable or disable
   */

  public void enableAllSelectedFiles(boolean enable) {
//		fileselected = -1;
    thedata.setStatusAllSelectedFiles(enable);
  }

  public void fittingOutputSelectedFiles(boolean enable) {
//		fileselected = -1;
    thedata.setFittingOutputAllSelectedFiles(enable);
  }

  /**
   * Use as background all the selected datafiles of the list.
   * @param enable true or false to enable or disable
   */

  public void asBkgAllSelectedFiles(boolean enable) {
//		fileselected = -1;
    thedata.setAsBkgAllSelectedFiles(enable);
  }

	/**
	 * Use the counting time to scale the instensity for
	 * all the selected datafiles of the list.
	 * @param enable true or false to enable or disable
	 */

	public void useCountTimeAllSelectedFiles(boolean enable) {
//		fileselected = -1;
		thedata.useCountTimeAllSelectedFiles(enable);
	}

	/**
   * Add one background parameter to all the datafiles.
   */

  public void addAdditionalBackgroundToAll() {
    thedata.addAdditionalBackgroundToAll();
  }

  /**
   * Add one background parameter to the selected datafile.
   */

  public void addAdditionalBackground() {
    thedata.addAdditionalBackgroundToSelected();
  }

  /**
   * Remove all background parameters from the selected datafile.
   */

  public void removeAdditionalBackground() {
    thedata.removeAdditionalBackgroundToSelected();
  }

  /**
   * Add one shift parameter to the selected datafile.
   */

  public void addAdditionalShift() {
    thedata.addAdditionalShiftToSelected();
  }

  /**
   * Remove all shift parameters from the selected datafile.
   */

  public void removeAdditionalShift() {
    thedata.removeAdditionalShiftToSelected();
  }

  /**
   * Bound all parameters of selected spectra to the first selected.
   */

  public void boundSelectedSpectra() {
    thedata.boundSelectedSpectra();
  }

  /**
   * View a plot of the selected datafile.
   */

  public void viewSelectedDataFile() {
//		(new PlotDataFile(this, thedata.getSelectedDataFile())).setVisible(true);
    (new PersistentThread() {
        public void executeJob() {
        DiffrDataFile[] datafiles = thedata.getSelectedDataFiles();
        if (datafiles.length > 1)
          (new MultiPlotFitting(DataD.this, datafiles, thedata.getLabel())).setVisible(true);
        else
          (new PlotFitting(DataD.this, datafiles)).setVisible(true);
      }
    }).start();
  }

  public void viewSelectedDataFileSummed() {
//		(new PlotDataFile(this, thedata.getSelectedDataFile())).setVisible(true);
    (new PersistentThread() {
        public void executeJob() {
        DiffrDataFile[] datafiles = thedata.getSelectedDataFiles();
        (new PlotFitting(DataD.this, datafiles)).setVisible(true);
      }
    }).start();
  }

  public void manualInterpolatedPointsShowPlot() {
//		(new PlotDataFile(this, thedata.getSelectedDataFile())).setVisible(true);
    (new PersistentThread() {
        public void executeJob() {
          thedata.checkManualInterpolation();
          DiffrDataFile[] plotdatafiles = thedata.getActiveDataFiles();
          PlotFitting plot = new PlotFitting(DataD.this, plotdatafiles);
	        plot.editInterpolatedBackgroundPoints();
          plot.setVisible(true);
      }
    }).start();
  }

  public void removeManualInterpolated() {
//		(new PlotDataFile(this, thedata.getSelectedDataFile())).setVisible(true);
    (new PersistentThread() {
        public void executeJob() {
          thedata.removeManualInterpolation(true);
      }
    }).start();
  }

  /**
   * Sums all the selected datafiles to generate a new datafile.
   */

  public void SumDatafileOutput() {
    thedata.SumDatafileOutput(this);
  }

  /**
   * Sums the datafiles with some rules to generate a new datafile.
   */

  public void SumDatafileOutputRules() {
    (new summationRulesFrame(this)).setVisible(true);
  }

  /**
   * Sums the datafiles with some rules to generate a new datafile.
   * @param aframe the frame
   * @param sameAngles true if sum spectra with same omega
   * @param angles the omega value
   */

  public void SumDatafileOutput(Frame aframe, boolean[] sameAngles,
                                double[] angles) {
    thedata.SumDatafileOutput(aframe, sameAngles, angles);
  }

	/**
	 * Sums the datafiles with some rules to generate a new datafile.
	 * @param aframe the frame
	 * @param selectedAngle index of the angle over which to sum
	 * @param angle_range the angle range value for summation
	 */

	public void sumDatafileOutput(Frame aframe, int selectedAngle,
	                              double angle_range) {
		thedata.sumDatafileOutput(aframe, selectedAngle, angle_range);
	}

	/**
   * Show the image manager AreaDetector using ImageJ package.
   */

  public void showAreaDetector() {
    if (areaImage == null)
      areaImage = new AreaImage();
    else
      areaImage.setVisible(true);
    areaImage.setDataSet(thedata);
  }

  /**
   * Read Packard Bioscience Cyclone scanner (now Perkin Elmer) ana file
   * containing the comments on the measurement.
   */

  public void readCycloneAnaFile() {
    String filename = Utility.browseFilename(this,
            "Choose the ana Optiquant file for the measurement");
    filename = Misc.filterFileName(filename);
    InputStream is = Misc.getBufferedInputStream("", filename);
    if (is != null) {
    DataInputStream reader = new DataInputStream(is);
    StringBuffer calchar = new StringBuffer();
      try {
//        byte Commentaires1[] = new byte[80];
//        reader.read(Commentaires1);  // comments
        char onechar = Misc.readCharOneByte(reader);
        while (reader.available() > 0) {
          calchar.append(onechar); // calibration filename
          onechar = Misc.readCharLittleEndian(reader);
        }

      } catch (IOException e) {
        System.out.println("Error in loading the Cyclone *.ana file!");
	      e.printStackTrace();
        // LogSystem.printStackTrace(e);
      }
      try {
        reader.close();
      } catch (IOException e) {
	      e.printStackTrace();
        // LogSystem.printStackTrace(e);
      }
//      String calName = calchar.toString();
//      System.out.println(calName);
      // todo must be finish, what we do with it?
    }
  }

  /**
   * Modify dialog for the datafiles tilting angles.
   */

  public void changeAngles() {
    (new changeAnglesFrame(this)).setVisible(true);
  }

	/**
	 * Modify dialog for the datafiles tilting angles.
	 */

	public void setAnglesByMeasurementTime() {
		(new SetAnglesByTimeFrame(this)).setVisible(true);
	}

  /**
   * Plot an hystogram of the total intensity of each spectrum.
   */

  public void plotIntensityHystogram() {
    (new PlotSimpleData(this, thedata.get2ThetaForActiveSpectra(), thedata.getTotalIntensityForActiveSpectra())).setVisible(true);
  }

  /**
   * Sort spectra listing by bank number.
   */

  public void sortByBankNumber() {
    fileselected = -1;
    thedata.sortByBankNumber();
  }

  /**
   * Sort spectra listing by name.
  */
  public void sortByName() {
    fileselected = -1;
    thedata.sortByName();
  }

  /**
   * Sort spectra listing by angles.
  */
  public void sortByAngles() {
    fileselected = -1;
    (new SortByAnglesFrame(this)).setVisible(true);
  }

  /**
    * Remove spectra by angle range.
   */
   public void removeByAngles() {
     fileselected = -1;
     (new RemoveByAnglesFrame(this)).setVisible(true);
   }

  /**
   * Instrument setup methods.
   */

  public void InstrumentOptions() {
    String selectedInst = InstrumentC.getSelectedItem().toString();
    if (!thedata.getInstrument().identifier.equals(selectedInst))
      thedata.setInstrument(selectedInst);
    InstLabel.setText(thedata.getInstrument().toXRDcatString());
    thedata.getInstrument().edit(this);
  }

  public void ImportInstrument() {
    String selectedInst = InstrumentC.getSelectedItem().toString();
    if (!thedata.getInstrument().identifier.equals(selectedInst))
      thedata.setInstrument(selectedInst);

    // add instrument from CIF database.
    String filename = Utility.openFileDialog(this, "Open CIF file or database", FileDialog.LOAD,
        MaudPreferences.getPref(principalJFrame.databasePath, Constants.documentsDirectory),
        null, Constants.documentsDirectory);
    if (filename != null) {
      final String[] folderAndName = Misc.getFolderandName(filename);
      MaudPreferences.setPref(principalJFrame.databasePath, folderAndName[0]);
      thedata.loadInstrument(folderAndName[0] + folderAndName[1], this);
      InstrumentC.setSelectedItem(thedata.getInstrument().identifier);
      InstLabel.setText(thedata.getInstrument().toXRDcatString());
    }
  }

  public void StoreInstrument() {
    String selectedInst = InstrumentC.getSelectedItem().toString();
    if (!thedata.getInstrument().identifier.equals(selectedInst))
      thedata.setInstrument(selectedInst);

    InstLabel.setText(thedata.getInstrument().toXRDcatString());
    // add the selected object to a CIF database.
    String filename = Utility.openFileDialog(this, "Select the Instrument database", FileDialog.LOAD,
        MaudPreferences.getPref(principalJFrame.databasePath, Constants.documentsDirectory),
        null, Constants.documentsDirectory + FilePar.database[3]);
    if (filename != null) {
      String[] folderAndName = Misc.getFolderandName(filename);
      MaudPreferences.setPref(principalJFrame.databasePath, folderAndName[0]);
      thedata.getInstrument().storeOnDB(folderAndName[0] + folderAndName[1]);
    }

  }

  /**
   * Open the option dialog for the active intensity extractor.
   */

  public void IntensityExtractorOptions() {
    String selectedIntExt = IntensityExtractorCB.getSelectedItem().toString();
    if (!thedata.getIntensityExtractorMethod().equals(selectedIntExt))
      thedata.setIntensityExtractor(selectedIntExt);
    thedata.getIntensityExtractor().getOptionsDialog(this).setVisible(true);
  }

  /**
   * Open the option dialog for the active position extractor.
   */

  public void PositionExtractorOptions() {
    String selectedIntExt = PositionExtractorCB.getSelectedItem().toString();
    if (!thedata.getPositionExtractorMethod().equals(selectedIntExt))
      thedata.setPositionExtractor(selectedIntExt);
    thedata.getPositionExtractor().getOptionsDialog(this).setVisible(true);
  }

	public void DiffractionOptions() {
		String selectedDiffraction = DiffractionCB.getSelectedItem().toString();
		if (!thedata.getDiffractionMethod().equals(selectedDiffraction))
			thedata.setDiffraction(selectedDiffraction);
		thedata.getDiffraction().getOptionsDialog(this).setVisible(true);
	}

	public void ReflectivityOptions() {
    String selectedReflectivity = ReflectivityCB.getSelectedItem().toString();
    if (!thedata.getReflectivityMethod().equals(selectedReflectivity))
      thedata.setReflectivity(selectedReflectivity);
    thedata.getReflectivity().getOptionsDialog(this).setVisible(true);
  }

  public void FluorescenceOptions() {
    String selectedFluorescence = FluorescenceCB.getSelectedItem().toString();
    if (!thedata.getFluorescenceMethod().equals(selectedFluorescence))
      thedata.setFluorescence(selectedFluorescence);
    thedata.getFluorescence().getOptionsDialog(this).setVisible(true);
  }

  public void dispose() {
    if (areaImage != null) {
      areaImage.setDataSet(null);
      areaImage.setVisible(false);
    }
    thedata.getFilePar().refreshAll(false);
    if (getFrameParent() instanceof DiffractionMainFrame) {
      ((DiffractionMainFrame) getFrameParent()).updateDataFilePlot(false);
    }
    thedata = null;
    excludedregionP.dispose();
    polynomialP.dispose();
    backgroundEtaP.dispose();
    gaussianP.dispose();
    PositionExtractorCB.removeAllItems();
    IntensityExtractorCB.removeAllItems();
    InstrumentC.removeAllItems();
	  DiffractionCB.removeAllItems();
    ReflectivityCB.removeAllItems();
    FluorescenceCB.removeAllItems();
    super.dispose();
  }

  class summationRulesFrame extends myJFrame {

    public summationRulesFrame(DataD aframe) {

      super(aframe);

      summationRulesFrame.this.setOwnPosition = true;

      Container c1 = summationRulesFrame.this.getContentPane();

      c1.setLayout(new BorderLayout(3, 3));

	    JPanel panel3 = new JPanel();
	    panel3.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
	    panel3.add(new JLabel("Sum range +- "));
	    final JTextField angleTF = new JTextField("0.0");
	    angleTF.setToolTipText("Specify the angle range for summation here");
	    panel3.add(angleTF);
	    c1.add(BorderLayout.NORTH, panel3);

      JPanel principalPanel = new JPanel();
      c1.add(BorderLayout.CENTER, principalPanel);

      principalPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));

      panel3 = new JPanel();
      panel3.setLayout(new GridLayout(0, 1, 3, 3));
      principalPanel.add(panel3);

      panel3.add(new JLabel("Groups spectra by:"));

	   final String[] sameLabels = {"omega angle", "chi angle", "phi angle", "eta angle",
			    "2theta angle", "energy"};
      final JRadioButton[] sameAnglesCB = new JRadioButton[DiffrDataFile.maxAngleNumber];
	    final ButtonGroup rbg = new ButtonGroup();
	    for (int j = 0; j < DiffrDataFile.maxAngleNumber; j++) {
		    sameAnglesCB[j] = new JRadioButton(sameLabels[j]);
		    rbg.add(sameAnglesCB[j]);
		    if (j == 0)
		      sameAnglesCB[j].setSelected(true);
		    panel3.add(sameAnglesCB[j]);
	    }

      panel3 = new JPanel();
      panel3.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      c1.add(BorderLayout.SOUTH, panel3);

      JButton startD = new JCloseButton();
      startD.setToolTipText("Do the summation based on the specified angle ranges and save the file");

      final DataD adata = aframe;
      startD.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
        	int selectedIndex = -1;
	        for (int j = 0; j < DiffrDataFile.maxAngleNumber; j++) {
		        if (sameAnglesCB[j].isSelected())
			        selectedIndex = j;
	        }
	        double angle = Double.parseDouble(angleTF.getText());
          summationRulesFrame.this.setCursor(new Cursor(Cursor.WAIT_CURSOR));
          adata.sumDatafileOutput(summationRulesFrame.this, selectedIndex, angle);
          summationRulesFrame.this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
          summationRulesFrame.this.setVisible(false);
          summationRulesFrame.this.dispose();
        }
      });
      panel3.add(startD);

      JButton stopD = new JCancelButton();
      stopD.setToolTipText("Exit with no action");
      stopD.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          summationRulesFrame.this.setVisible(false);
          summationRulesFrame.this.dispose();
        }
      });
      panel3.add(stopD);

      this.setHelpButton(panel3);

      summationRulesFrame.this.setTitle("Sum spectra");

      summationRulesFrame.this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);

      summationRulesFrame.this.pack();
      summationRulesFrame.this.setResizable(false);

    }
  }

  class changeAnglesFrame extends myJFrame {

    JComboBox[] operationCB = null;
    JTextField[] angleOffsetTF = null;

    public changeAnglesFrame(DataD aframe) {

      super(aframe);

      changeAnglesFrame.this.setOwnPosition = true;

      Container c1 = changeAnglesFrame.this.getContentPane();

      c1.setLayout(new BorderLayout(3, 3));
      JPanel principalPanel = new JPanel();
      c1.add(BorderLayout.CENTER, principalPanel);

      principalPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));

      JPanel panel3 = new JPanel();
      panel3.setLayout(new GridLayout(0, 1, 3, 3));
      principalPanel.add(panel3);

      panel3.add(new JLabel("Modify angles of selected datafiles as:"));

      String[] newLabels = {" New omega = ",
                            "   New chi = ",
                            "   New phi = ",
                            "   New eta = ",
		                        "New 2theta = ",
		                        "New energy = ",
		                        "New coordX = "
      };
      String[] oldLabels = {" omega",
                            " chi",
                            " phi",
                            " eta",
                            " 2theta",
		                        " energy",
		                        " coordX"
      };

      final int totalNumber = newLabels.length;

      JPanel jp1;
      angleOffsetTF = new JTextField[totalNumber];
      operationCB = new JComboBox[totalNumber];
      for (int i = 0; i < totalNumber; i++) {
        jp1 = new JPanel(new FlowLayout(FlowLayout.LEFT, 2, 2));
        panel3.add(jp1);
        jp1.add(new JLabel(newLabels[i]));
        jp1.add(angleOffsetTF[i] = new JTextField(Constants.FLOAT_FIELD));
        angleOffsetTF[i].setText("0");
        jp1.add(operationCB[i] = new JComboBox());
        operationCB[i].addItem("+");
        operationCB[i].addItem("-");
        jp1.add(new JLabel(oldLabels[i]));
      }

      panel3 = new JPanel();
      panel3.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      c1.add(BorderLayout.SOUTH, panel3);

      JButton stopD = new JCancelButton();
      stopD.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          changeAnglesFrame.this.setVisible(false);
          changeAnglesFrame.this.dispose();
        }
      });
      panel3.add(stopD);

      JButton startD = new JCloseButton();

      final DataFileSet adata = aframe.thedata;
      startD.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          int[] mult = new int[totalNumber];
          double[] offset = new double[totalNumber];
          for (int i = 0; i < totalNumber; i++) {
            mult[i] = 1;
            if (operationCB[i].getSelectedItem().toString().equals("-"))
              mult[i] = -1;
            offset[i] = Double.valueOf(angleOffsetTF[i].getText()).doubleValue();
          }
          adata.setNewAngles(mult, offset);
 //         int fileselected = datafileL.getSelectedIndex();
          DiffrDataFile datafile = adata.getSelectedDataFile();
          if (datafile != null) {
	          for (int j = 0; j < DiffrDataFile.maxAngleNumber; j++)
		          anglesTF[j].setText(Misc.getFormattedValue(Double.parseDouble(datafile.getString(j + 1))));
          }
          changeAnglesFrame.this.setVisible(false);
          changeAnglesFrame.this.dispose();
        }
      });
      panel3.add(startD);

//			this.setHelpButton(panel3);

      changeAnglesFrame.this.setTitle("Change angles");

      changeAnglesFrame.this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);

      changeAnglesFrame.this.pack();
      changeAnglesFrame.this.setResizable(false);

    }
  }

	class SetAnglesByTimeFrame extends myJFrame {

		JComboBox angleCB = null;
		JComboBox datafileCB = null;
		JTextField angleOffsetTF = null;
		JTextField angleStepTF = null;
		JComboBox angle2CB = null;
		JTextField angleOffset2TF = null;
		JTextField angleStep2TF = null;

		public SetAnglesByTimeFrame(DataD aframe) {

			super(aframe);

			SetAnglesByTimeFrame.this.setOwnPosition = true;

			Container c1 = SetAnglesByTimeFrame.this.getContentPane();

			c1.setLayout(new BorderLayout(3, 3));
			JPanel panel2 = new JPanel(new BorderLayout(3, 3));
			c1.add(BorderLayout.CENTER, panel2);
			JPanel panel3 = new JPanel();
			panel2.add(BorderLayout.NORTH, panel3);
			panel3.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));

			panel3.add(new JLabel("Set angles of selected datafiles using a time formula and the measurement time in millisecs:"));

			panel3 = new JPanel();
			panel3.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			c1.add(BorderLayout.CENTER, panel3);

			angleCB = new JComboBox();
			String[] newLabels = {"Omega",
					"Chi",
					"Phi",
					"Eta",
					"2Theta",
					"Energy"
			};
			for (int i = 0; i < newLabels.length; i++)
				angleCB.addItem(newLabels[i]);
			panel3.add(angleCB);

			panel3.add(new JLabel(" = "));

			angleOffsetTF = new JTextField(12);
			panel3.add(angleOffsetTF);
			angleOffsetTF.setText("0");

			panel3.add(new JLabel("(degs) + "));

			angleStepTF = new JTextField(12);
			panel3.add(angleStepTF);
			angleStepTF.setText("0.001");

			panel3.add(new JLabel("(degs/ms) * (measurement_time - measurement_time of "));

			final DataFileSet adata = aframe.thedata;
			datafileCB = new JComboBox();
			DiffrDataFile[] selDatafiles = adata.getSelectedDataFiles();
			for (int i = 0; i < selDatafiles.length; i++)
				datafileCB.addItem(selDatafiles[i].toString());
			panel3.add(datafileCB);

			panel3.add(new JLabel(")"));

			panel3 = new JPanel();
			panel3.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
			panel2.add(BorderLayout.SOUTH, panel3);

			JButton stopD = new JCancelButton();
			stopD.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					SetAnglesByTimeFrame.this.setVisible(false);
					SetAnglesByTimeFrame.this.dispose();
				}
			});
			panel3.add(stopD);

			JButton startD = new JCloseButton();

			startD.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					double offset = Double.valueOf(angleOffsetTF.getText()).doubleValue();
					double step = Double.valueOf(angleStepTF.getText()).doubleValue();
					int angleIndex = angleCB.getSelectedIndex();
					int datafileIndex = datafileCB.getSelectedIndex();
					DiffrDataFile[] selDatafiles = adata.getSelectedDataFiles();
					long refTime = selDatafiles[datafileIndex].getMeasurementTimeInMilliSec();
					for (int i = 0; i < selDatafiles.length; i++) {
						long actualTime = selDatafiles[i].getMeasurementTimeInMilliSec();
						double newAngle = offset + step * (actualTime - refTime);
						selDatafiles[i].setAngleValue(angleIndex, newAngle);
					}
					DiffrDataFile datafile = adata.getSelectedDataFile();
					if (datafile != null) {
						for (int j = 0; j < DiffrDataFile.maxAngleNumber; j++)
							anglesTF[j].setText(Misc.getFormattedValue(datafile.getAngleValue(j)));
					}
					SetAnglesByTimeFrame.this.setVisible(false);
					SetAnglesByTimeFrame.this.dispose();
				}
			});
			panel3.add(startD);

//			this.setHelpButton(panel3);

			panel2 = new JPanel(new BorderLayout(3, 3));
			c1.add(BorderLayout.SOUTH, panel2);
			panel3 = new JPanel();
			panel2.add(BorderLayout.NORTH, panel3);
			panel3.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));

			panel3.add(new JLabel("Set angles of selected datafiles using a time formula and the measurement time in millisecs:"));

			panel3 = new JPanel();
			panel3.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			c1.add(BorderLayout.CENTER, panel3);

			angle2CB = new JComboBox();
			for (int i = 0; i < newLabels.length; i++)
				angle2CB.addItem(newLabels[i]);
			panel3.add(angle2CB);

			panel3.add(new JLabel(" = "));

			angleOffset2TF = new JTextField(12);
			panel3.add(angleOffset2TF);
			angleOffset2TF.setText("0");

			panel3.add(new JLabel("(degs) + "));

			angleStep2TF = new JTextField(12);
			panel3.add(angleStep2TF);
			angleStep2TF.setText("0.001");

			panel3.add(new JLabel("(degs) * spectrum index"));

			final DataFileSet adata1 = aframe.thedata;

			panel3 = new JPanel();
			panel3.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
			panel2.add(BorderLayout.SOUTH, panel3);

			stopD = new JCancelButton();
			stopD.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					SetAnglesByTimeFrame.this.setVisible(false);
					SetAnglesByTimeFrame.this.dispose();
				}
			});
			panel3.add(stopD);

			startD = new JCloseButton();

			startD.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					double offset = Double.valueOf(angleOffset2TF.getText()).doubleValue();
					double step = Double.valueOf(angleStep2TF.getText()).doubleValue();
					int angleIndex = angle2CB.getSelectedIndex();
					DiffrDataFile[] selDatafiles = adata.getSelectedDataFiles();
					for (int i = 0; i < selDatafiles.length; i++) {
						double newAngle = offset + step * i;
						selDatafiles[i].setAngleValue(angleIndex, newAngle);
					}
					DiffrDataFile datafile = adata.getSelectedDataFile();
					if (datafile != null) {
						for (int j = 0; j < DiffrDataFile.maxAngleNumber; j++)
							anglesTF[j].setText(Misc.getFormattedValue(datafile.getAngleValue(j)));
					}
					SetAnglesByTimeFrame.this.setVisible(false);
					SetAnglesByTimeFrame.this.dispose();
				}
			});
			panel3.add(startD);


			SetAnglesByTimeFrame.this.setTitle("Set angles by measurement time or spectrum index");

			SetAnglesByTimeFrame.this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);

			SetAnglesByTimeFrame.this.pack();
			SetAnglesByTimeFrame.this.setResizable(false);

		}
	}

	class SortByAnglesFrame extends myJFrame {

    JComboBox[] orderCB = null;

    public SortByAnglesFrame(DataD aframe) {

      super(aframe);

      SortByAnglesFrame.this.setOwnPosition = true;

      Container c1 = SortByAnglesFrame.this.getContentPane();

      c1.setLayout(new BorderLayout(3, 3));
      JPanel principalPanel = new JPanel();
      c1.add(BorderLayout.CENTER, principalPanel);

      principalPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));

      JPanel panel3 = new JPanel();
      panel3.setLayout(new GridLayout(0, 1, 3, 3));
      principalPanel.add(panel3);

      panel3.add(new JLabel("Priority order :"));

      String[] priority = {"first :",
                           "second:",
                           "third :",
                           "forth :",
                           "fifth :",
                           "Sixth :"};
      String[] angleNames = {"omega",
                             "chi",
                             "phi",
		                         "eta",
		                         "2theta",
                             "energy"};

      JPanel jp1;
      orderCB = new JComboBox[priority.length];
      for (int i = 0; i < priority.length; i++) {
        jp1 = new JPanel(new FlowLayout(FlowLayout.LEFT, 2, 2));
        panel3.add(jp1);
        jp1.add(new JLabel(priority[i]));
        jp1.add(orderCB[i] = new JComboBox());
        for (int j = 0; j < angleNames.length; j++)
          orderCB[i].addItem(angleNames[j]);
        orderCB[i].setSelectedIndex(i);
        orderCB[i].addItemListener(new ItemListener() {
          public void itemStateChanged(ItemEvent e) {
            JComboBox actualCB = (JComboBox) e.getSource();
            int actualIndex = actualCB.getSelectedIndex();
            for (int i = 0; i < 4; i++) {
              if (orderCB[i] != null && orderCB[i] != actualCB) {
                int selIndex = orderCB[i].getSelectedIndex();
                if (selIndex == actualIndex) {
                  int newIndex;
                  for (int j = 0; j < 4; j++) {
                    boolean thisIsGood = true;
                    for (int k = 0; k < 4; k++)
                      if (k != i && orderCB[k] != null)
                        if (j == orderCB[k].getSelectedIndex())
                          thisIsGood = false;
                    if (thisIsGood) {
                      newIndex = j;
                      orderCB[i].setSelectedIndex(newIndex);
                      break;
                    }
                  }
                }
              }
            }
          }
        });
      }

      panel3 = new JPanel();
      panel3.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      c1.add(BorderLayout.SOUTH, panel3);

      JButton stopD = new JCancelButton();
      stopD.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          SortByAnglesFrame.this.setVisible(false);
          SortByAnglesFrame.this.dispose();
        }
      });
      panel3.add(stopD);

      JButton startD = new JCloseButton();

      final DataFileSet adata = aframe.thedata;
      startD.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          int[] order = new int[6];
          for (int i = 0; i < 6; i++) {
            order[i] = orderCB[i].getSelectedIndex();
          }
          adata.sortByAngles(order);

          SortByAnglesFrame.this.setVisible(false);
          SortByAnglesFrame.this.dispose();
        }
      });
      panel3.add(startD);

//			this.setHelpButton(panel3);

      SortByAnglesFrame.this.setTitle("Sort by angles: priorities");

      SortByAnglesFrame.this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);

      SortByAnglesFrame.this.pack();
      SortByAnglesFrame.this.setResizable(false);

    }
  }

  class RemoveByAnglesFrame extends myJFrame {

    JComboBox orderCB = null;
	  JComboBox orderCB1 = null;
	  JTextField startingTF;
    JTextField finalTF;
	  JTextField startingTF1;
	  JTextField finalTF1;
	  JTextField everyAngleTF;
	  JTextField everyTF;

    public RemoveByAnglesFrame(DataD aframe) {

      super(aframe);

	    final DataFileSet adata = aframe.thedata;
      RemoveByAnglesFrame.this.setOwnPosition = true;

      Container c1 = RemoveByAnglesFrame.this.getContentPane();

      c1.setLayout(new BorderLayout(3, 3));
      JPanel principalPanel = new JPanel();
      c1.add(BorderLayout.CENTER, principalPanel);

      principalPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));

      JPanel panel3 = new JPanel();
      panel3.setLayout(new GridLayout(0, 1, 3, 3));
      principalPanel.add(panel3);

      JPanel jp1 = new JPanel(new FlowLayout(FlowLayout.LEFT, 2, 2));
      panel3.add(jp1);
      jp1.add(new JLabel("Datafiles with angle "));
      String[] angleNames = {"omega",
                             "chi",
                             "phi",
                             "eta",
                             "2theta",
                             "energy"};
      orderCB = new JComboBox();
      for (int j = 0; j < angleNames.length; j++)
        orderCB.addItem(angleNames[j]);
      orderCB.setSelectedIndex(angleNames.length - 1);
      jp1.add(orderCB);
      jp1.add(new JLabel(" every "));
      jp1.add(startingTF = new JTextField("0.0"));
      jp1.add(new JLabel(" to "));
      jp1.add(finalTF = new JTextField("90.0"));
	    jp1.add(new JLabel(" every "));
	    jp1.add(everyAngleTF = new JTextField("5.0"));
	    jp1.add(new JLabel(" degrees "));
	    JButton startD = new JButton("Disable");
	    startD.addActionListener(new ActionListener() {
		    public void actionPerformed(ActionEvent event) {
			    int order = orderCB1.getSelectedIndex();
			    double startingAngle = Double.parseDouble(startingTF1.getText());
			    double finalAngle = Double.parseDouble(finalTF1.getText());
			    double everyAngle = Double.parseDouble(everyAngleTF.getText());
			    adata.disableByAngles(order, startingAngle, finalAngle, everyAngle);

			    RemoveByAnglesFrame.this.setVisible(false);
			    RemoveByAnglesFrame.this.dispose();
		    }
	    });
	    jp1.add(startD);
	    startD = new JButton("Group");
	    startD.addActionListener(new ActionListener() {
		    public void actionPerformed(ActionEvent event) {
			    int order = orderCB1.getSelectedIndex();
			    double startingAngle = Double.parseDouble(startingTF1.getText());
			    double finalAngle = Double.parseDouble(finalTF1.getText());
			    double everyAngle = Double.parseDouble(everyAngleTF.getText());
			    adata.groupByAngles(order, startingAngle, finalAngle, everyAngle);

			    RemoveByAnglesFrame.this.setVisible(false);
			    RemoveByAnglesFrame.this.dispose();
		    }
	    });
	    jp1.add(startD);

	    jp1 = new JPanel(new FlowLayout(FlowLayout.LEFT, 2, 2));
	    panel3.add(jp1);
	    jp1.add(new JLabel("Datafiles every "));
	    jp1.add(everyTF = new JTextField("2"));
	    startD = new JButton("Disable");
	    startD.addActionListener(new ActionListener() {
		    public void actionPerformed(ActionEvent event) {
			    int every = Integer.parseInt(everyTF.getText());
			    adata.removeEvery(every);

			    RemoveByAnglesFrame.this.setVisible(false);
			    RemoveByAnglesFrame.this.dispose();
		    }
	    });
	    jp1.add(startD);
	    startD = new JButton("Enable");
	    startD.addActionListener(new ActionListener() {
		    public void actionPerformed(ActionEvent event) {
			    int every = Integer.parseInt(everyTF.getText());
			    adata.enableEvery(every);

			    RemoveByAnglesFrame.this.setVisible(false);
			    RemoveByAnglesFrame.this.dispose();
		    }
	    });
	    jp1.add(startD);
	    startD = new JButton("Group");
	    startD.addActionListener(new ActionListener() {
		    public void actionPerformed(ActionEvent event) {
			    int every = Integer.parseInt(everyTF.getText());
			    adata.groupEvery(every);

			    RemoveByAnglesFrame.this.setVisible(false);
			    RemoveByAnglesFrame.this.dispose();
		    }
	    });
	    jp1.add(startD);

	    panel3 = new JPanel();
      panel3.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      c1.add(BorderLayout.SOUTH, panel3);

      JButton stopD = new JCancelButton();
      stopD.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          RemoveByAnglesFrame.this.setVisible(false);
          RemoveByAnglesFrame.this.dispose();
        }
      });
      panel3.add(stopD);

//			this.setHelpButton(panel3);

      RemoveByAnglesFrame.this.setTitle("Disable by rules: priorities");

      RemoveByAnglesFrame.this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);

      RemoveByAnglesFrame.this.pack();
      RemoveByAnglesFrame.this.setResizable(false);

    }
  }


}

