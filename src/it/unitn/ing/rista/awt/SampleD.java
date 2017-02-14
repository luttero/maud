/*
 * @(#)SampleD.java created 06/7/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

import java.awt.*;
import java.awt.event.*;
import javax.swing.border.*;
import javax.swing.*;
import javax.swing.event.*;
import java.io.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import it.unitn.ing.jgraph.*;

/**
 *  The SampleD, a Dialog for editing the sample features.
 *
 *
 * @version $Revision: 1.13 $, $Date: 2006/12/04 14:30:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class SampleD extends myJFrame {

  Sample thesample;
  Phase thephase = null;
  Layer selectedlayer = null;
  int phaseselected = -1;

  JTextField specaxialTF;
  JTextField specequatTF;
  JTextField specthickTF;
  JTextField specradiusXTF;
  JTextField specradiusYTF;
  JLabel phasewtL;
  JTextField ThicknessTF;
  JTextField CriticalQcTF;
  JTextField AbsorptionTF;
  JTextField RoughnessTF;
	JCheckBox qcFromPhaseCB;

  public SampleD(Frame parent) {
    super(parent);

    framePositionX = "sampleFrame.framePositionX";
    framePositionY = "sampleFrame.framePositionY";
    defaultFramePositionX = 10;
    defaultFramePositionY = 20;
    setOwnPosition = true;

  }

  public SampleD(Frame parent, Sample asample) {
    this(parent);

    thesample = asample;
    createDefaultMenuBar();

    if (parent != null)
      parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout(6, 6));

    int subordinate = 2; // thesample.Nsubordinate;
    JSubordinatePanel[] subordinatePanels = new JSubordinatePanel[subordinate];
    String[] labels = {"Shape model", "Layer trainer"};
    String[] toolTips = {"Choose the model for the sample shape",
                         "Choose the model for working out the layered structure at a glance"};

    jPanel16 = new JPanel();
    jPanel16.setLayout(new BorderLayout());
    c1.add(BorderLayout.NORTH, jPanel16);
    jPanel13 = new JPanel();
    jPanel13.setLayout(new BorderLayout());
    jPanel16.add(BorderLayout.WEST, jPanel13);
    jPanel14 = new JPanel();
    jPanel14.setLayout(new FlowLayout());
    jPanel13.add(jPanel14, BorderLayout.NORTH);
    jPanel14.add(new JLabel("Sample id:"));

    sampleidTF = new JTextField(12);
    sampleidTF.setText("Sample description");
    jPanel14.add(sampleidTF);

    jPanel14 = new JPanel();
    jPanel14.setLayout(new GridLayout(0, 1, 6, 6));
    jPanel13.add(jPanel14, BorderLayout.CENTER);

    int subIndex = 0;

    jPanel13 = new JPanel();
    jPanel13.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    jPanel16.add(BorderLayout.EAST, jPanel13);
    JPanel phaseModelPanel = new JPanel();
    phaseModelPanel.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    jPanel13.add(phaseModelPanel);
    phaseModelPanel.add(new JLabel("Phase refinement model: "));
    phaseModelC = new JComboBox();
    phaseModelC.setEditable(false);
    phaseModelC.setMaximumRowCount(3);
    phaseModelPanel.add(phaseModelC);
    for (int i = 0; i < Sample.phaseRefinementModel.length; i++)
      phaseModelC.addItem(Sample.phaseRefinementModel[i]);
    phaseModelC.setSelectedIndex(thesample.getPhaseRefinementBehaviour());
    phaseModelC.setToolTipText("Select how the automatic refinement handle phase fraction refinement");

    jPanel13 = new JPanel();
    jPanel13.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
    c1.add(BorderLayout.SOUTH, jPanel13);
    JPanel closebuttonPanel = new JPanel();
    closebuttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    jPanel13.add(closebuttonPanel);
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
//		cancelB = new JCancelButton();
//		jPanel13.add(cancelB);
    jPanel12 = new JTabbedPane();
    String tpString[] = {"Data sets", "Sample position", "Sample dimensions", "Layers"};
    c1.add(BorderLayout.CENTER, jPanel12);

    borderPanel1 = new JPanel();
    borderPanel1.setLayout(new BorderLayout());
    jPanel12.addTab(tpString[3], null, borderPanel1);

/*    jPanel6 = new JPanel();
    jPanel6.setLayout(new BorderLayout());
    jPanel12.addTab(tpString[0], null, jPanel6);
    datasetlist = new JList();
    datasetlist.setVisibleRowCount(4);
    datasetlist.setPrototypeCellValue("12345678901234567890");
    JScrollPane sp = new JScrollPane();
		sp.setBorder(new LineBorder(Color.black));
    sp.getViewport().add(datasetlist);
    jPanel13 = new JPanel();
    jPanel13.setLayout(new BorderLayout(6, 6));
    jPanel13.add("North", new JLabel("Available sets"));
    jPanel13.add("Center", sp);
    jPanel6.add("West", jPanel13);
    jPanel5 = new JPanel();
    jPanel5.setLayout(new BorderLayout());
    jPanel7 = new JPanel();
    jPanel7.setLayout(new GridLayout(4, 1, 6, 6));
    addallB = new JIconButton("FingerRight.gif", "Add all");
    jPanel7.add(addallB);
    addsetB = new JIconButton("FingerRight.gif", "Add selected");
    jPanel7.add(addsetB);
    removeB = new JIconButton("Delete.gif", "Remove");
    jPanel7.add(removeB);
    removeallB = new JIconButton("Delete.gif", "Remove All");
    jPanel7.add(removeallB);
    jPanel5.add("South", jPanel7);
    jPanel6.add("Center", jPanel5);
    activesetlist = new JList();
    activesetlist.setVisibleRowCount(4);
    activesetlist.setPrototypeCellValue("12345678901234567890");
    sp = new JScrollPane();
//		sp.setBorder(new LineBorder(Color.black));
    sp.getViewport().add(activesetlist);
    jPanel13 = new JPanel();
    jPanel13.setLayout(new BorderLayout(6, 6));
    jPanel13.add("North", new JLabel("Active sets"));
    jPanel13.add("Center", sp);
    jPanel6.add("East", jPanel13);*/

    jPanel6 = new JPanel();
    jPanel6.setLayout(new BorderLayout(6, 6));
    jPanel5 = new JPanel();
    jPanel5.setLayout(new FlowLayout());
    jPanel12.addTab(tpString[1], null, jPanel6);
    jPanel6.add(BorderLayout.NORTH, jPanel5);
    SampleOrientationBPanel = new JPanel();
    SampleOrientationBPanel.setBorder(new TitledBorder(
            new BevelBorder(BevelBorder.LOWERED), "Sample orientation"));
    SampleOrientationBPanel.setLayout(new BorderLayout(6, 6));
    jPanel5.add(SampleOrientationBPanel);
    jPanel3 = new JPanel();
    jPanel3.setLayout(new GridLayout(3, 1, 6, 6));
    SampleOrientationBPanel.add(BorderLayout.WEST, jPanel3);
    jPanel3.add(new JLabel("Omega: "));
    jPanel3.add(new JLabel("Chi: "));
    jPanel3.add(new JLabel("Phi: "));
    jPanel4 = new JPanel();
    jPanel4.setLayout(new GridLayout(3, 1, 6, 6));
    SampleOrientationBPanel.add(BorderLayout.EAST, jPanel4);
    specoromegaTF = new JTextField(Constants.FLOAT_FIELD);
    specoromegaTF.setText("0");
    jPanel4.add(specoromegaTF);
    specorchiTF = new JTextField(Constants.FLOAT_FIELD);
    specorchiTF.setText("0");
    jPanel4.add(specorchiTF);
    specorphiTF = new JTextField(Constants.FLOAT_FIELD);
    specorphiTF.setText("0");
    jPanel4.add(specorphiTF);
    sampledisplacementBPanel = new JPanel();
    sampledisplacementBPanel.setBorder(new TitledBorder(
            new BevelBorder(BevelBorder.LOWERED), "Sample displacement"));
    sampledisplacementBPanel.setLayout(new BorderLayout(6, 6));
    jPanel5.add(sampledisplacementBPanel);
    jPanel1 = new JPanel();
    jPanel1.setLayout(new GridLayout(0, 1, 6, 6));
    sampledisplacementBPanel.add(BorderLayout.WEST, jPanel1);
    jPanel1.add(new JLabel("x: "));
    jPanel1.add(new JLabel("y: "));
    jPanel1.add(new JLabel("z: "));
    jPanel2 = new JPanel();
    jPanel2.setLayout(new GridLayout(0, 1, 6, 6));
    sampledisplacementBPanel.add(BorderLayout.EAST, jPanel2);
    specdispxTF = new JTextField(Constants.FLOAT_FIELD);
    specdispxTF.setText("0");
    jPanel2.add(specdispxTF);
    specdispyTF = new JTextField(Constants.FLOAT_FIELD);
    specdispyTF.setText("0");
    jPanel2.add(specdispyTF);
    specdispzTF = new JTextField(Constants.FLOAT_FIELD);
    specdispzTF.setText("0");
    jPanel2.add(specdispzTF);

    jPanel2 = new JPanel();
    jPanel2.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    sampledisplacementBPanel.add(BorderLayout.SOUTH, jPanel2);
    JSubordinatePanel precessionP = new JSubordinatePanel(this, thesample, 2,
            "Precession error", "Choose a sample precession error model");
    jPanel2.add(precessionP);
    jPanel6.add(BorderLayout.CENTER, jPanel2);

    jPanel5 = new JPanel();
    jPanel5.setLayout(new FlowLayout(FlowLayout.CENTER, 3, 3));
    jPanel12.addTab(tpString[2], null, jPanel5);
    jPanel2 = new JPanel();
    jPanel2.setLayout(new BorderLayout());
    jPanel5.add(jPanel2);
    jPanel3 = new JPanel();
    jPanel3.setLayout(new GridLayout(0, 1, 6, 6));
    jPanel2.add(BorderLayout.CENTER, jPanel3);
    jPanel3.add(new JLabel("Axial      (mm): "));
    jPanel3.add(new JLabel("Equatorial (mm): "));
    jPanel3.add(new JLabel("Thickness  (mm): "));
    jPanel3.add(new JLabel("Radius X   (mm): "));
    jPanel3.add(new JLabel("Radius Y   (mm): "));
    jPanel4 = new JPanel();
    jPanel4.setLayout(new GridLayout(0, 1, 6, 6));
    jPanel2.add(BorderLayout.EAST, jPanel4);
    specaxialTF = new JTextField(Constants.FLOAT_FIELD);
    specaxialTF.setText("0");
    jPanel4.add(specaxialTF);
    specequatTF = new JTextField(Constants.FLOAT_FIELD);
    specequatTF.setText("0");
    jPanel4.add(specequatTF);
    specthickTF = new JTextField(Constants.FLOAT_FIELD);
    specthickTF.setText("0");
    jPanel4.add(specthickTF);
    specradiusXTF = new JTextField(Constants.FLOAT_FIELD);
    specradiusXTF.setText("0");
    jPanel4.add(specradiusXTF);
    specradiusYTF = new JTextField(Constants.FLOAT_FIELD);
    specradiusYTF.setText("0");
    jPanel4.add(specradiusYTF);

    jPanel4 = new JPanel();
    jPanel4.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    jPanel2.add(BorderLayout.SOUTH, jPanel4);
    subordinatePanels[subIndex] = new JSubordinatePanel(this, thesample, subIndex,
            labels[subIndex], toolTips[subIndex]);
    jPanel4.add(subordinatePanels[subIndex]);
    subIndex++;



    JPanel jps = new JPanel();
    jps.setLayout(new FlowLayout(FlowLayout.CENTER, 3, 3));
    jPanel11 = new JPanel();
    jPanel11.setLayout(new BorderLayout(3, 3));

    LayersL = new JList();
    LayersL.setPrototypeCellValue("12345678901234567890");
    LayersL.setVisibleRowCount(8);
    JScrollPane sp = new JScrollPane();
//		sp.setBorder(new LineBorder(Color.black));
    sp.getViewport().add(LayersL);

    jPanel11.add(BorderLayout.CENTER, sp);
    jps.add(jPanel11);
    borderPanel1.add(BorderLayout.CENTER, jps);

	  JPanel densityAndQcOption = new JPanel();
	  densityAndQcOption.setLayout(new GridLayout(0, 1, 3, 3));
	  jPanel11.add(BorderLayout.SOUTH, densityAndQcOption);
    JPanel jpDensity = new JPanel();
    jpDensity.setLayout(new FlowLayout(FlowLayout.CENTER, 3, 3));
	  densityAndQcOption.add(jpDensity);
    JButton densityB = new JIconButton("LineGraph.gif", "Density plot");
    densityB.setToolTipText("Press this to see a layer density profile obtained by reflectivity");
    densityB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        densityB_Clicked();
      }
    });
    jpDensity.add(densityB);
	  jpDensity = new JPanel();
	  jpDensity.setLayout(new FlowLayout(FlowLayout.CENTER, 3, 3));
	  qcFromPhaseCB = new JCheckBox("compute Qc");
	  densityAndQcOption.add(qcFromPhaseCB);
	  qcFromPhaseCB.setToolTipText("If enabled, the Qc for each layer is computed from phase content");

    jPanel11 = new JPanel();
    jPanel11.setLayout(new GridLayout(0, 1, 3, 3));
    jps.add(jPanel11);
    addlayerB = new JIconButton("Plus.gif", "Add a layer");
    addlayerB.setToolTipText("Press this to add a layer after the last one");
    jPanel11.add(addlayerB);
    JButton insertlayerB = new JIconButton("Insert.gif", "Insert a layer");
    insertlayerB.setToolTipText("Press this to insert a layer before the selected one; no selection = at end");
    insertlayerB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        insertlayerB_Clicked();
      }
    });
    jPanel11.add(insertlayerB);
    insertlayerB = new JIconButton("MultiLayer.gif", "Insert a sequence");
    insertlayerB.setToolTipText("Press this to insert a multiLayer before the selected one; no selection = at end");
    insertlayerB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        insertMultilayerB_Clicked();
      }
    });
    jPanel11.add(insertlayerB);
    final JRemoveButton removelayerB = new JRemoveButton();
    removelayerB.setToolTipText("Press this to remove the selected layer");
    removelayerB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (!Constants.confirmation || Utility.areYouSureToRemove("Remove the selected layer?"))
          removelayerB_Clicked();
      }
    });
    jPanel11.add(removelayerB);

    insertlayerB = new JButton("Merge layers");
    insertlayerB.setToolTipText("Press this to merge the selected layers");
    insertlayerB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        mergelayerB_Clicked();
      }
    });
    jPanel11.add(insertlayerB);

    jPanel9 = new JPanel();
    jPanel9.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    borderPanel1.add(BorderLayout.EAST, jPanel9);
    jPanel8 = new JPanel();
    jPanel8.setLayout(new BorderLayout(6, 6));
    jPanel9.add(jPanel8);
    jPanel9 = new JPanel();
    jPanel9.setLayout(new GridLayout(7, 1, 6, 6));
    jPanel8.add("West", jPanel9);
    jPanel9.add(new JLabel("Thickness (Angstroms):"));
    jPanel9.add(new JLabel("Critical qc (reflectivity):"));
    jPanel9.add(new JLabel("Absorption (reflectivity):"));
    jPanel9.add(new JLabel("Roughness (reflectivity):"));
    jPanel9.add(new JLabel("Phase:"));
    jPanel9.add(new JLabel("Volume fraction:"));
    jPanel9.add(new JLabel("Weight fraction:"));
    jPanel10 = new JPanel();
    jPanel10.setLayout(new GridLayout(7, 1, 6, 6));
    jPanel8.add("Center", jPanel10);
    ThicknessTF = new JTextField(16);
    ThicknessTF.setText("0");
    jPanel10.add(ThicknessTF);
    CriticalQcTF = new JTextField(16);
    CriticalQcTF.setText("0");
    jPanel10.add(CriticalQcTF);
    AbsorptionTF = new JTextField(16);
    AbsorptionTF.setText("0");
    jPanel10.add(AbsorptionTF);
    RoughnessTF = new JTextField(16);
    RoughnessTF.setText("0");
    jPanel10.add(RoughnessTF);
    phaseC = new JComboBox();
    phaseC.setEditable(false);
    phaseC.setMaximumRowCount(4);
    jPanel10.add(phaseC);
    phaseintTF = new JTextField(16);
    phaseintTF.setText("0");
    jPanel10.add(phaseintTF);
    phasewtL = new JLabel();
    phasewtL.setText("0");
    jPanel10.add(phasewtL);

    jPanel4 = new JPanel();
    jPanel4.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    borderPanel1.add(BorderLayout.SOUTH, jPanel4);
    subordinatePanels[subIndex] = new JSubordinatePanel(this, thesample, subIndex,
            labels[subIndex], toolTips[subIndex]);
    jPanel4.add(subordinatePanels[subIndex]);
    subIndex++;

    setTitle("Sample input");

    setTitle(asample.toXRDcatString());
    initparameters();
    pack();
    initListener();
    if (parent != null)
      parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
  }

  public JMenuBar createDefaultMenuBar() {
    JMenuBar amenubar = super.createDefaultMenuBar();
    amenubar.add(createToolsMenu());
    return amenubar;
  }

  public JMenu createToolsMenu() {

    JMenuItem menuitem = null;

    JMenu toolsMenu = new JMenu("Tools");
    toolsMenu.setMnemonic('t');

    toolsMenu.add(menuitem = new JMenuItem("Refine all thicknesses"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        int number = thesample.layersnumber() - 1;
        for (int i = 1; i < number; i++) {
          Layer alayer = thesample.getlayer(i);
          alayer.getThickness().setRefinable();
//            alayer.setCriticalQc(initialQc);
//            alayer.getCriticalQc().setRefinable();
          }
      }
    });

    toolsMenu.add(menuitem = new JMenuItem("Refine all Qc"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        int number = thesample.layersnumber() - 1;
        for (int i = 1; i < number; i++) {
          Layer alayer = thesample.getlayer(i);
          alayer.getCriticalQc().setRefinable();
        }
      }
    });

    toolsMenu.add(menuitem = new JMenuItem("Refine all roughness"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        int number = thesample.layersnumber();
        for (int i = 1; i < number; i++) {
          Layer alayer = thesample.getlayer(i);
          alayer.getRoughness().setRefinable();
        }
      }
    });

    toolsMenu.add(menuitem = new JMenuItem("Workout the heterostructure by GA"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        heterostructureByGA();
      }
    });

    toolsMenu.add(menuitem = new JMenuItem("-"));

    toolsMenu.add(menuitem = new JMenuItem("Sample orientations -> measurement angles"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        transferAngles();
      }
    });

    return toolsMenu;
  }

  private void transferAngles() {
    thesample.setomega(specoromegaTF.getText());
    thesample.setchi(specorchiTF.getText());
    thesample.setphi(specorphiTF.getText());
    thesample.transferAnglesToMeasurement();
    specoromegaTF.setText("0");
    specorchiTF.setText("0");
    specorphiTF.setText("0");
    thesample.setomega("0");
    thesample.setchi("0");
    thesample.setphi("0");
  }

  public void initparameters() {
    int i;

    sampleidTF.setText(thesample.getSampleID());

    specoromegaTF.setText(thesample.getomega().getValue());
    specorchiTF.setText(thesample.getchi().getValue());
    specorphiTF.setText(thesample.getphi().getValue());

    specdispxTF.setText(thesample.getdispx().getValue());
    specdispyTF.setText(thesample.getdispy().getValue());
    specdispzTF.setText(thesample.getdispz().getValue());

    specaxialTF.setText(thesample.getAxialDimension().getValue());
    specequatTF.setText(thesample.getEquatorialDimension().getValue());
    specthickTF.setText(thesample.getThicknessDimension().getValue());
    specradiusXTF.setText(thesample.getRadiusDimensionX().getValue());
    specradiusYTF.setText(thesample.getRadiusDimensionY().getValue());

    addComponenttolist(specoromegaTF, thesample.getomega());
    addComponenttolist(specorchiTF, thesample.getchi());
    addComponenttolist(specorphiTF, thesample.getphi());
    addComponenttolist(specdispxTF, thesample.getdispx());
    addComponenttolist(specdispyTF, thesample.getdispy());
    addComponenttolist(specdispzTF, thesample.getdispz());
    addComponenttolist(specaxialTF, thesample.getAxialDimension());
    addComponenttolist(specequatTF, thesample.getEquatorialDimension());
    addComponenttolist(specthickTF, thesample.getThicknessDimension());
    addComponenttolist(specradiusXTF, thesample.getRadiusDimensionX());
    addComponenttolist(specradiusYTF, thesample.getRadiusDimensionY());

    thesample.normalizePhaseQuantity();
    if (thesample.layersnumber() > 0) {
      ListVector layerlist = thesample.getlayerlist();
      layerlist.setList(LayersL);
      layerlist.select(0);
    }

//		((FilePar) thesample.getFilePar()).datalistv.setList(datasetlist);
/*    int realsetnumber = thesample.getRealDataSetNumber();
    String[] data = new String[realsetnumber];
    for (i = 0; i < realsetnumber; i++)
      data[i] = new String(thesample.getRealDataSet(i).toXRDcatString());
    datasetlist.setListData(data);
    thesample.getDataSetlist().setList(activesetlist);*/

    for (i = 0; i < thesample.phasesNumber(); i++)
      phaseC.addItem(thesample.getPhase(i).toXRDcatString());
    selectphase(0);
	  qcFromPhaseCB.setSelected(thesample.getQcFromPhase());
    updatelayerfield();
  }

  public void initListener() {

    SymAction lSymAction = new SymAction();
//    removeB.addActionListener(lSymAction);
//    removeallB.addActionListener(lSymAction);
//    addallB.addActionListener(lSymAction);
//    addsetB.addActionListener(lSymAction);
    addlayerB.addActionListener(lSymAction);
    ListItem listItem = new ListItem();
    LayersL.addListSelectionListener(listItem);
    Item lItem = new Item();
    phaseC.addItemListener(lItem);
  }

  void heterostructureByGA() {
    (new PersistentThread() {
        public void executeJob() {
        AttentionD attnD = new AttentionD(
                SampleD.this, "Heterostructure by GA",
                "Please wait, finding an heterostructure approximation...",
                "Binocular.gif", false);
        attnD.setVisible(true);
        updatelayerfield();
        boolean result = thesample.workoutHeterostructure();
        if (!result) {
          System.out.println("Not able to workout the heterostructure. Maybe there is no reflectivity model or pattern?");
        } else {
          selectedlayer = null;
          updatelayerfield();
        }
        attnD.setVisible(false);
        attnD.dispose();
        attnD = null;
      }
    }).start();
  }


  public Parameter getparameterfrom(Component hit) {
    Layer alayer = (Layer) thesample.getlayerlist().selectedElement();

    if (hit.equals(ThicknessTF))
      return alayer.getThickness();
    else if (hit.equals(phaseintTF)) {
      int phasesel = phaseC.getSelectedIndex();
      return alayer.getPhaseQuantity(phasesel);
    }
    return super.getparameterfrom(hit);
  }

  public void selectphase(int index) {
    if (index < phaseC.getItemCount())
      phaseC.setSelectedIndex(index);
  }

  public void updateintensityfield() {
    if (selectedlayer != null) {
      if (phaseselected >= 0)
        selectedlayer.setPhaseQuantity(phaseselected, phaseintTF.getText());
    }
    selectedlayer = (Layer) thesample.getlayerlist().selectedElement();
//    selectedlayer.normalizePhaseQuantity();
    if (selectedlayer != null) {
      phaseselected = phaseC.getSelectedIndex();
      Parameter quantitypar = selectedlayer.getPhaseQuantity(phaseselected);
      double wt = selectedlayer.getWeightedPhaseQuantity(phaseselected);
      if (quantitypar != null) {
        phaseintTF.setText(quantitypar.getValue());
        phasewtL.setText(Fmt.format(wt));
        addComponenttolist(phaseintTF, quantitypar);
      } else {
        phaseintTF.setText("0");
        removeComponentfromlist(phaseintTF);
      }
    }
//		else
//			selectphase(-1);
  }

  public void updatelayerfield() {
    if (thesample == null)
      return;
    if (selectedlayer != null) {
      selectedlayer.setThickness(ThicknessTF.getText());
      selectedlayer.setCriticalQc(CriticalQcTF.getText());
      selectedlayer.setAbsorption(AbsorptionTF.getText());
      selectedlayer.setRoughness(RoughnessTF.getText());
      if (phaseselected >= 0)
        selectedlayer.setPhaseQuantity(phaseselected, phaseintTF.getText());
    }
    selectedlayer = (Layer) thesample.getlayerlist().selectedElement();
    if (selectedlayer != null) {
      selectedlayer.normalizePhaseQuantity(false);
      ThicknessTF.setText(selectedlayer.getThickness().getValue());
      addComponenttolist(ThicknessTF, selectedlayer.getThickness());
      CriticalQcTF.setText(selectedlayer.getCriticalQc().getValue());
      addComponenttolist(CriticalQcTF, selectedlayer.getCriticalQc());
      AbsorptionTF.setText(selectedlayer.getAbsorption().getValue());
      addComponenttolist(AbsorptionTF, selectedlayer.getAbsorption());
      RoughnessTF.setText(selectedlayer.getRoughness().getValue());
      addComponenttolist(RoughnessTF, selectedlayer.getRoughness());
      phaseselected = phaseC.getSelectedIndex();
      if (phaseselected >= 0) {
        phaseintTF.setText(selectedlayer.getPhaseQuantity(phaseselected).getValue());
        phasewtL.setText(Fmt.format(selectedlayer.getWeightedPhaseQuantity(phaseselected)));
        addComponenttolist(phaseintTF, selectedlayer.getPhaseQuantity(phaseselected));
      } else {
        phaseintTF.setText("0");
        phasewtL.setText("0");
        removeComponentfromlist(phaseintTF);
      }
    } else {
      ThicknessTF.setText("0");
      removeComponentfromlist(ThicknessTF);
      CriticalQcTF.setText("0");
      removeComponentfromlist(CriticalQcTF);
      AbsorptionTF.setText("0");
      removeComponentfromlist(AbsorptionTF);
      RoughnessTF.setText("0");
      removeComponentfromlist(RoughnessTF);
      removeComponentfromlist(phaseintTF);
      selectphase(-1);
    }
  }

  public void layerselectionchanged() {
    updatelayerfield();
  }

  public void phaseselectionchanged() {
    updateintensityfield();
  }

  public void retrieveParameters() {
    if (thesample == null)
      return;
    thesample.setSampleID(sampleidTF.getText());

//		thesample.setshape(sampleshapeC.getSelectedItem().toXRDcatString());

    thesample.setomega(specoromegaTF.getText());
    thesample.setchi(specorchiTF.getText());
    thesample.setphi(specorphiTF.getText());

    thesample.getAxialDimension().setValue(specaxialTF.getText());
    thesample.getEquatorialDimension().setValue(specequatTF.getText());
    thesample.getThicknessDimension().setValue(specthickTF.getText());
    thesample.getRadiusDimensionX().setValue(specradiusXTF.getText());
    thesample.getRadiusDimensionY().setValue(specradiusYTF.getText());

    thesample.setdispx(specdispxTF.getText());
    thesample.setdispy(specdispyTF.getText());
    thesample.setdispz(specdispzTF.getText());

    updatelayerfield();
    thesample.normalizePhaseQuantity();
    thesample.setPhaseRefinementBehaviour(phaseModelC.getSelectedIndex());
	  thesample.setQcFromPhase(qcFromPhaseCB.isSelected());
  }

  JPanel jPanel16;
  JPanel jPanel14;
  JPanel jPanel15;
  JTextField sampleidTF;
  JPanel jPanel13;
//	JButton cancelB;
  JTabbedPane jPanel12;
  JPanel borderPanel1;
  JPanel jPanel11;
  JButton addlayerB;
  JPanel jPanel8;
  JPanel jPanel9;
  JPanel jPanel10;
  JComboBox phaseC;
  JComboBox phaseModelC;
  JTextField phaseintTF;
  JList LayersL;
  JPanel borderPanel2;
  JPanel jPanel6;
//  JList datasetlist;
  JPanel jPanel7;
//  JButton addallB;
//  JButton addsetB;
//  JButton removeB;
//  JButton removeallB;
//  JList activesetlist;
  JPanel jPanel5;
  JPanel SampleOrientationBPanel;
  JPanel jPanel3;
  JPanel jPanel4;
  JTextField specoromegaTF;
  JTextField specorchiTF;
  JTextField specorphiTF;
  JPanel sampledisplacementBPanel;
  JPanel jPanel1;
  JPanel jPanel2;
  JTextField specdispxTF;
  JTextField specdispyTF;
  JTextField specdispzTF;

  public Sample getData() {
    return thesample;
  }

  void LayersL_ListSelect() {
    if (thesample == null)
      return;
    layerselectionchanged();
  }

  void phaseC_Action() {
    if (thesample == null)
      return;
    phaseselectionchanged();
  }

/*  void addallB_Clicked() {
    int realsetnumber = thesample.getRealDataSetNumber();
    for (int i = 0; i < realsetnumber; i++)
      thesample.getDataSetlist().addItem(thesample.getRealDataSet(i));
    thesample.refreshDataIndices();
  }

  void addsetB_Clicked() {
    int index = datasetlist.getSelectedIndex();
    if (index >= 0)
      thesample.getDataSetlist().addItem(thesample.getRealDataSet(index));
    thesample.refreshDataIndices();
  }

  void removeB_Clicked() {
    thesample.getDataSetlist().removeSelElement();
    thesample.refreshDataIndices();
  }

  void removeallB_Clicked() {
    thesample.getDataSetlist().removeAllItems();
//		thesample.getDataSetlist().select(-1);
    thesample.refreshDataIndices();
  }*/

  void addlayerB_Clicked() {
    if (thesample == null)
      return;
    thesample.addLayer();
    layerselectionchanged();
  }

  void insertlayerB_Clicked() {
    if (thesample == null)
      return;
    selectedlayer = null;

    int index = thesample.getlayerlist().getSelectedIndex();
    thesample.addLayer(index);

    layerselectionchanged();
  }

  void insertMultilayerB_Clicked() {
    if (thesample == null)
      return;
    selectedlayer = null;

    int index = thesample.getlayerlist().getSelectedIndex();
    new MultiLayerD(this, index);

    layerselectionchanged();
  }

  void removelayerB_Clicked() {
    selectedlayer = null;

    thesample.getlayerlist().removeSelElement();
    layerselectionchanged();
  }

  void mergelayerB_Clicked() {
    selectedlayer = null;

    thesample.getlayerlist().mergeSelElement();
    layerselectionchanged();
  }

  void densityB_Clicked() {
    if (thesample == null || thesample.getlayerlist().size() <= 1)
      return;

    new DensityProfileD(this);
  }

  void sampleshapeC_Action() {
  }

  public void dispose() {
    thesample = null;
    thephase = null;
    selectedlayer = null;
    super.dispose();
  }

  class SymAction implements ActionListener {
    public void actionPerformed(ActionEvent event) {
      Object object = event.getSource();
      if (object == addlayerB)
        addlayerB_Clicked();
    }
  }

  class Item implements ItemListener {
    public void itemStateChanged(ItemEvent event) {
      Object object = event.getSource();
      if (object == phaseC)
        phaseC_Action();
    }
  }

  class ListItem implements ListSelectionListener {
    public void valueChanged(ListSelectionEvent event) {
      Object object = event.getSource();
      if (object == LayersL)
        LayersL_ListSelect();
    }
  }

  class MultiLayerD extends myJFrame {

    JTextField nsequenceTF = null;
    JTextField nrepetitionTF = null;
    JTextField qcTF = null;
    JCheckBox freeQcCB = null;
    int theindex = -1;

    public MultiLayerD(Frame aframe, int index) {
      super(aframe, "Multilayer definition");

      MultiLayerD.this.setOwnPosition = true;

      theindex = index;

      JPanel principalP = new JPanel();
      principalP.setLayout(new BorderLayout(3, 3));
      MultiLayerD.this.getContentPane().add(principalP);
      JPanel jp1 = new JPanel();
      jp1.setLayout(new GridLayout(0, 1, 3, 3));
      principalP.add(BorderLayout.CENTER, jp1);
      JPanel jp2 = new JPanel();
      jp2.setLayout(new FlowLayout());
      jp2.add(new JLabel("Number of layers in the sequence: "));
      nsequenceTF = new JTextField(6);
      nsequenceTF.setText("1");
      jp2.add(nsequenceTF);
      jp1.add(jp2);
      jp2 = new JPanel();
      jp2.setLayout(new FlowLayout());
      jp2.add(new JLabel("Number of repetitions of the sequence: "));
      nrepetitionTF = new JTextField(6);
      nrepetitionTF.setText("1");
      jp2.add(nrepetitionTF);
      jp1.add(jp2);
      freeQcCB = new JCheckBox("Refinable separated Qc");
      freeQcCB.setToolTipText("Set refinable Qc in all layers in sequence repetition");
      jp1.add(freeQcCB);
      jp2 = new JPanel();
      jp2.setLayout(new FlowLayout());
      jp2.add(new JLabel("Initial Qc: "));
      qcTF = new JTextField("0.03");
      jp2.add(qcTF);
      jp1.add(jp2);

      jp1 = new JPanel();
      jp1.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
      principalP.add(BorderLayout.SOUTH, jp1);

      JButton jbok1 = new JCloseButton();
      jp1.add(jbok1);
      jbok1.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          retrieveParameters();
          MultiLayerD.this.setVisible(false);
          MultiLayerD.this.dispose();
        }
      });
      MultiLayerD.this.getRootPane().setDefaultButton(jbok1);

      jbok1 = new JCancelButton();
      jp1.add(jbok1);
      jbok1.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          MultiLayerD.this.setVisible(false);
          MultiLayerD.this.dispose();
        }
      });

      MultiLayerD.this.pack();
      MultiLayerD.this.setVisible(true);
    }

    public void retrieveParameters() {
      int nseq = Integer.valueOf(nsequenceTF.getText()).intValue();
      int nrep = Integer.valueOf(nrepetitionTF.getText()).intValue();
      boolean freeQc = freeQcCB.isSelected();
      String initialQc = qcTF.getText();

      Layer[] layers = new Layer[nseq];

      if (theindex == -1)
        theindex = thesample.layersnumber();

      for (int i = 0; i < nrep; i++) {
        for (int j = 0; j < nseq; j++) {
          Layer alayer = thesample.addLayer(theindex++);
          alayer.setCriticalQc(initialQc);
          if (i == 0)
            layers[j] = alayer;
          else
            alayer.setEqualTo(layers[j], true);
          if (freeQc)
            alayer.getCriticalQc().setRefinable();
        }
      }

    }
  }

  class DensityProfileD extends myJFrame {

    double[] data = null;

    public DensityProfileD(Frame aframe) {

      super(aframe, "Density profile");

      DensityProfileD.this.frameWLabel = "plotDensityProf.frameWidth";
      DensityProfileD.this.frameHLabel = "plotDensityProf.frameHeight";
      DensityProfileD.this.defaultFrameW = 400;
      DensityProfileD.this.defaultFrameH = 400;
      DensityProfileD.this.setOwnSize = true;
      DensityProfileD.this.framePositionX = "plotDensityProf.framePositionX";
      DensityProfileD.this.framePositionY = "plotDensityProf.framePositionY";
      DensityProfileD.this.defaultFramePositionX = 10;
      DensityProfileD.this.defaultFramePositionY = 20;
      DensityProfileD.this.setOwnPosition = true;

      int l, i, j;

      DensityProfileD.this.getContentPane().setBackground(Color.white);

      DensityProfileD.this.createDefaultMenuBar(true);

      Container c1 = DensityProfileD.this.getContentPane();
      c1.setLayout(new BorderLayout(6, 6));
/*
**      Create the Graph instance and modify the default behaviour
*/
      G2Dint graph = new G2Dint();
      graph.drawzero = false;
      graph.drawgrid = false;
      graph.borderTop = 10;

/*
**      Load a file containing Marker definitions
*/
//        System.out.println("Loading markers....");

      Markers marker = null;
      try {
        marker = new Markers(Constants.documentsDirectory + "marker.txt");
      } catch (IOException ioe) {
        ioe.printStackTrace();
      }

      if (marker != null)
        graph.setMarkers(marker);

/*
**      Retrieve the data Set.
*/

      c1.add(BorderLayout.CENTER, graph);
      DensityProfileD.this.setTitle("Density profile");
//          System.out.println("Loading data....");

      int layernumber = thesample.layersnumber();

      double[][] layerPar = new double[layernumber + 1][3];

      double total_thickness = 0.0;
      for (i = 0; i < layernumber; i++) {
        Layer alayer = thesample.getlayer(i);
        if (i == 0)
          layerPar[i][0] = 0.0;
        if (i < layernumber - 1)
          layerPar[i + 1][0] = layerPar[i][0] + alayer.getThicknessInAngstrom();
        if (i == layernumber - 1) {
          double substrate_thickness = alayer.getThicknessInAngstrom();
          if (substrate_thickness == 0.0)
            substrate_thickness = 50;  // substrate
          layerPar[i + 1][0] = layerPar[i][0] + substrate_thickness;
          layerPar[i + 1][1] = Math.abs(alayer.getCriticalQcValue());
          layerPar[i + 1][2] = 0.0;
          total_thickness = layerPar[i + 1][0];
        }
        layerPar[i][1] = Math.abs(alayer.getCriticalQcValue());
        layerPar[i][2] = Math.abs(alayer.getRoughnessValue()) * Constants.sqrt2;

      }


      int subdivision = 500;
      double step = total_thickness / (subdivision - 1);
      int np = subdivision;
      data = new double[2 * np];
      for (j = 0; j < subdivision; j++) {
        double thickness = step * j;
        data[j * 2] = thickness;
        for (l = 0; l < layernumber; l++) {
          double delta_left = thickness - layerPar[l][0];
          double delta_right = -thickness + layerPar[l+1][0];
          double roughness_left = layerPar[l][2];
          double roughness_right = layerPar[l+1][2];
          double erf1 = 1.0;
          if (roughness_left > 0.0)
            erf1 += JSci.maths.SpecialMath.error(delta_left / roughness_left);
          else if (delta_left > 0.0)
            erf1 += 1.0;
          else
            erf1 -= 1.0;
          double erf2 = 1.0;
          if (roughness_right > 0.0)
            erf2 += JSci.maths.SpecialMath.error(delta_right / roughness_right);
          else if (delta_right > 0.0)
            erf2 += 1.0;
          else
            erf2 -= 1.0;
          data[j * 2 + 1] += layerPar[l][1] * 0.25 * erf1 * erf2;
//          System.out.println(j + " " + data[j * 2] + " " + data[j * 2 + 1]);
        }
      }


//      for (double x = -5.0; x <= 5.; x+=0.1)
//        System.out.println(x + " " + JSci.maths.SpecialMath.error(x));

//          System.out.println("Data loaded");

      DataSet data1 = graph.loadDataSet(data, np);
/*        	data1.linestyle = 0;
        	data1.marker    = markerNumber;
        	data1.markerscale = markerScale;
        	data1.markercolor = markerColor;*/
//        	data1.legend(200,100,datafile[0].getAxisXLegend());
//        	data1.legendColor(Color.black);

/*     data[] = new double[2*np*20];
      for(l=i=j=0; l < layernumber; l++, i+=2, j+=80) {
        data[j] = thickness;
        thickness += layerPar[l][0];
        data[j+1] = layerPar[l][1];
        data[j+2] = thickness;
        data[j+3] = layerPar[l][1];
      }*/

/*
**      Attach data sets to the Xaxis
*/
//          System.out.println("Attaching X-axis....");

      Axis xaxis = graph.createXAxis();
      xaxis.attachDataSet(data1);

      xaxis.setTitleText("Depth [Angstrom]");
      xaxis.setTitleFont(new Font("TimesRoman", Font.BOLD, PlotDataFile.XaxisTitleFontScale));
      xaxis.setLabelFont(new Font("Helvetica", Font.PLAIN, PlotDataFile.XaxisLabelFontScale));
      xaxis.setTitleColor(PlotDataFile.XaxisTitleColor);
/*
**      Attach the first data set to the Left Axis
*/
//          System.out.println("Attaching Y-axis....");

      Axis yaxis = graph.createYAxis();
      yaxis.attachDataSet(data1);
      yaxis.setTitleText("Critical Qc");
      yaxis.setTitleFont(new Font("TimesRoman", Font.BOLD, PlotDataFile.YaxisTitleFontScale));
      yaxis.setLabelFont(new Font("Helvetica", Font.PLAIN, PlotDataFile.YaxisLabelFontScale));
      yaxis.setTitleColor(Color.blue);

      DensityProfileD.this.setComponentToPrint(graph);
      DensityProfileD.this.setVisible(true);
    }

    public void saveFile() {
      // nothing by default
      if (data == null)
        return;
      BufferedWriter output = Misc.getWriter(Utility.browseFilenametoSave(this, "Save plot data as..."));
      try {
        int totalLength = data.length/2;
        output.newLine();
        output.write("_layers_profile " + Integer.toString(totalLength));
        output.newLine();
        output.newLine();
        output.write("loop_");
        output.newLine();
        output.write("_x_coordinate");
        output.newLine();
        output.write("_y_ordinate");
        output.newLine();
        for (int i = 0; i < totalLength; i++) {
          output.write(" " + Fmt.format(data[2 * i]) + " " + Fmt.format(data[2 * i + 1]));
          output.newLine();
        }
      } catch (IOException io) {
      }
      try {
        output.flush();
        output.close();
      } catch (IOException io) {
      }
    }

  }

}
