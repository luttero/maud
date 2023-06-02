/*
 * @(#)PhaseD.java created 1/1/1997 xx
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.pdf.PDFUtilitiesUI;
import it.unitn.ing.rista.diffr.rsa.TensorHomogenization;
import it.unitn.ing.rista.diffr.sdpd.*;
import it.unitn.ing.rista.io.*;
import it.unitn.ing.rista.models.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.table.TableModel;
import java.awt.*;
import java.awt.event.*;
import java.util.StringTokenizer;
import java.util.Vector;

import org.javadev.AnimatingCardLayout;
//import org.javadev.effects.SlideAnimation;
import org.javadev.effects.DashboardAnimation;

/**
 * The PhaseD implements a Frame for visual editing of the characteristics of
 * an object of class Phase.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.26 $, $Date: 2006/12/04 14:30:03 $
 * @since JDK1.1
 */


public class PhaseD extends myJFrame {

  Phase thephase;
  int atomnumber;
  boolean manualSelection = false;
  JComboBox structureFactorModelsCB;
  JComboBox structureFactorExtractorCB;
//  JComboBox structureSolutionCB;
  JComboBox tdsModelCB;
  JTextField atomquantity;
  JTextField atomquantity_incell;
  JTextField xcoord;
  JTextField ycoord;
  JTextField zcoord;
  JTextField Bfactor;
  JButton atomtypechoice;
  JComboBox symmetrychoice;
//  JComboBox conventionchoice;
  JComboBox spacegrouplist;
  JTextField phaseid;
  it.unitn.ing.rista.awt.iJPanel structPanel;
  JPanel cellPanel = null;
  JPanel generalPanel = null;
  JComboBox[] modelCB = null;
  JTextField grainsize;
  JComboBox[] optchoice;
  int[] optionsIndices = {0, 6, 5};
  JTextField minDspaceTF = null;
  JTextField maxDspaceTF = null;
  JTextField minReflnDspaceTF = null;
  JTextField maxReflnDspaceTF = null;
  AnimatingCardLayout panelLayout = null;
  JPanel generalContainer = null;
  JPanel microContainer = null;

  Vector[] sglistv;

	Vector<CrystalSystem> crystalSystems = null;

  public PhaseD(Frame parent) {
    super(parent);
  }

  public PhaseD(Frame parent, Phase aphase) {
    super(parent);

    thephase = aphase;

    framePositionX = "phaseFrame.framePositionX";
    framePositionY = "phaseFrame.framePositionY";
    defaultFramePositionX = 10;
    defaultFramePositionY = 20;
    setOwnPosition = true;

    if (parent != null)
      parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));

    createDefaultMenuBar();

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout());

    JButton jb;

    JPanel closebuttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    c1.add(BorderLayout.SOUTH, closebuttonPanel);
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

    JTabbedPane centerPanel = new JTabbedPane();
    c1.add(BorderLayout.CENTER, centerPanel);

    generalContainer = new JPanel();
    JPanel generalBorderPanel = new JPanel(new FlowLayout());
    generalPanel = new JPanel(new BorderLayout(0, 0));
    centerPanel.addTab("General", null, generalContainer);

    panelLayout = new AnimatingCardLayout();
    panelLayout.setAnimation(new DashboardAnimation());
    generalContainer.setLayout(panelLayout);

    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
    generalPanel.add(buttonPanel, BorderLayout.SOUTH);
    JButton optionsB = new JButton("Options");
    buttonPanel.add(optionsB);
    optionsB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        panelLayout.show(generalContainer, "options");
      }
    });

    generalContainer.add(generalBorderPanel, "general");
    generalBorderPanel.add(generalPanel);
    JPanel tmpPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 0, 0));
    generalPanel.add(BorderLayout.NORTH, tmpPanel);
    JPanel phaseGeneralCharacteristicsPanel = new JPanel(new BorderLayout());
    tmpPanel.add(phaseGeneralCharacteristicsPanel);

    JPanel leftGeneralPane = new JPanel(new GridLayout(0, 1, 0, 0));
    phaseGeneralCharacteristicsPanel.add(BorderLayout.WEST, leftGeneralPane);
    JPanel rightGeneralPane = new JPanel(new GridLayout(0, 1, 0, 0));
    phaseGeneralCharacteristicsPanel.add(BorderLayout.CENTER, rightGeneralPane);

    leftGeneralPane.add(new JLabel("Chemical formula (sum):"));
    tmpPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
    phaseid = new JTextField(16);
    phaseid.setText("XY");
    tmpPanel.add(phaseid);
    rightGeneralPane.add(tmpPanel);

    leftGeneralPane.add(new JLabel("Symmetry:"));
    rightGeneralPane.add(tmpPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3)));
    String symmetryString[] = null;

	  if (SpaceGroups.useCCTBX()) {

//		  System.out.println("Test cctbx, is printing 1? " + SpaceGroups.testCCTBXForMaud());
//		  System.out.flush();

		  crystalSystems = SpaceGroups.getAllSymmetriesAndSpaceGroups();

		  symmetryString = new String[crystalSystems.size()];
		  for (int i = 0; i < crystalSystems.size(); i++)
			  symmetryString[i] = crystalSystems.elementAt(i).symmetry;
	  } else {
		  symmetryString = new String[]{"triclinic", "monoclinic", "orthorhombic",
				  "tetragonal", "trigonal", "hexagonal", "cubic"};
	  }
    symmetrychoice = new JComboBox();
    for (int i = 0; i < symmetryString.length; i++)
      symmetrychoice.addItem(symmetryString[i]);
    symmetrychoice.setEditable(false);
    symmetrychoice.setMaximumRowCount(7);
    tmpPanel.add(symmetrychoice);

/*    leftGeneralPane.add(new JLabel("Convention:"));
    rightGeneralPane.add(tmpPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3)));
    conventionchoice = new JComboBox();
    conventionchoice.addItem("Int. number");
    conventionchoice.addItem("Schoenflies");
    conventionchoice.addItem("Hermann-Mauguin");
    conventionchoice.addItem("Hall");
    conventionchoice.setEditable(false);
    conventionchoice.setMaximumRowCount(4);
    tmpPanel.add(conventionchoice);*/

    leftGeneralPane.add(new JLabel("Space group:"));
    rightGeneralPane.add(tmpPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3)));
    spacegrouplist = new JComboBox();
    spacegrouplist.setMaximumRowCount(7);
    tmpPanel.add(spacegrouplist);
    tmpPanel.add(jb = new JIconButton("NewSheet.gif", "List positions"));
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        show_generalpositions();
      }
    });

    leftGeneralPane.add(new JLabel("Reflections:"));
    rightGeneralPane.add(tmpPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3)));
    tmpPanel.add(jb = new JIconButton("NewSheet.gif", "Show list"));
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        show_hkl();
      }
    });

    refreshCellPanel();

    generalContainer.add(createHKLlistPanel(), "options");

/*
     Here go the the AtomSite or Structure panel
*/
    JPanel structureFactorPanel = new JPanel(new FlowLayout());
    centerPanel.addTab("Structure", null, structureFactorPanel);

    structPanel = ((StructureModel) thephase.getActiveSubordinateModel(Phase.structureModelID)).getEditPanel(this);
    structureFactorPanel.add(structPanel);

// here goes the microstructure panel
    microContainer = new JPanel(new FlowLayout());
    JPanel csfp = new JPanel(new BorderLayout());
    centerPanel.addTab("Microstructure", null, microContainer);
    microContainer.add(csfp);

    JPanel linebroadmethod = new JPanel();
    csfp.add(BorderLayout.NORTH, linebroadmethod);
    linebroadmethod.setLayout(new GridLayout(0, 1, 3, 3));
    linebroadmethod.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Line Broadening"));

    String[] labelstring = {"Line Broadening model:",
                            "Size-Strain model: ",
                            "Antiphase boundary model: ",
                            "Planar defects model: "
    };

    String[] tooltipString = {
      "Select the Line Broadening model to be used by this phase",
      "Select the method to symmetrize anisotropic crystallites and microstrains",
      "Select the antiphase boundary broadening model of super-reflections (only for fcc intermetallics)",
      "Select the planar defect broadening model"
    };

    String[] tooltipButton = {"No options for this",
                              "Edit the crystallite and microstrain values",
                              "Edit the antiphase probability",
                              "Edit deformation/growth and twin faults"
    };

    modelCB = new JComboBox[4];

    for (int j = 1; j < 5; j++) {

      final int index = j - 1;
      JPanel jp1 = new JPanel();
      jp1.setLayout(new FlowLayout(FlowLayout.RIGHT, 2, 2));
      linebroadmethod.add(jp1);
      jp1.add(new JLabel(labelstring[index]));
      modelCB[index] = new JComboBox();
      modelCB[index].setToolTipText(tooltipString[index]);
      for (int i = 0; i < thephase.getsubordClassNumber(j); i++)
        modelCB[index].addItem(thephase.getsubordIdentifier(j, i));
      jp1.add(modelCB[index]);
      JButton jb1 = new JIconButton("Eyeball.gif", "Options");
      jb1.setToolTipText(tooltipButton[index]);
      jb1.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          broadeningOptions(index + 1);
        }
      });
      jp1.add(jb1);
    }

    JPanel jp1 = new JPanel();
    jp1.setLayout(new BorderLayout());
    csfp.add(BorderLayout.SOUTH, jp1);
    JPanel microabsPanel = new JPanel();
    microabsPanel.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Microabsorption/Dynamical scattering correction"));
    microabsPanel.setLayout(new BorderLayout());
    jp1.add(BorderLayout.CENTER, microabsPanel);
    JPanel firstP = new JPanel(new FlowLayout());
    microabsPanel.add(firstP, BorderLayout.NORTH);
    firstP.add(new JLabel("Grain size (Angstrom):"));
    grainsize = new JTextField(Constants.FLOAT_FIELD);
    grainsize.setText("0");
    firstP.add(grainsize);
    JSubordinatePanel subordinatePanels = new JSubordinatePanel(this, thephase, Phase.microAbsorptionID,
        "Microabsorption ",
        "Choose a microabsorption model");
    microabsPanel.add(subordinatePanels, BorderLayout.CENTER);

    grainsize.setText(new String(thephase.getAbsorptionCrystSize().getValue()));
    addComponenttolist(grainsize, thephase.getAbsorptionCrystSize());

    for (int j = 1; j < 5; j++)
      modelCB[j - 1].setSelectedItem((thephase.subordinateField[j]).identifier);

// the Structure Factor models panel
    csfp = new JPanel(new FlowLayout(FlowLayout.CENTER, 0, 0));
    centerPanel.addTab("Advanced models", null, csfp);

    JPanel optionsPanel = new JPanel(new BorderLayout(3, 3));
    csfp.add(optionsPanel);

    JPanel SFPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 2, 2));
    optionsPanel.add(BorderLayout.NORTH, SFPanel);
    SFPanel.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Texture/Stress/Magnetic"));


    JPanel jPanel12 = new JPanel(new BorderLayout(2, 2));
    SFPanel.add(jPanel12);

    JPanel jPanel6 = new JPanel();
    jPanel6.setLayout(new GridLayout(0, 1, 1, 1));
    jPanel12.add(BorderLayout.WEST, jPanel6);

    String[] tmpStringS1 = {"Texture:",
                            "Strain:",
                            "Magnetic:"};

    for (int i = 0; i < optionsIndices.length; i++)
      jPanel6.add(new JLabel(tmpStringS1[i]));

    JPanel jPanel8 = new JPanel();
    jPanel8.setLayout(new GridLayout(0, 1, 1, 1));
    jPanel12.add(BorderLayout.CENTER, jPanel8);

    optchoice = new JComboBox[optionsIndices.length];
    for (int i = 0; i < optionsIndices.length; i++) {
      final int index = i;
      JPanel jPanel2 = new JPanel();
      jPanel2.setLayout(new FlowLayout(FlowLayout.RIGHT, 1, 1));
      jPanel8.add(jPanel2);
      optchoice[i] = new JComboBox();
      optchoice[i].setEditable(false);
//      optchoice[i].setMaximumRowCount(4);
      jPanel2.add(optchoice[i]);
      JButton optbutton = new JIconButton("Eyeball.gif", "Options");
      optbutton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          subordinateOptions(index);
        }
      });
      jPanel2.add(optbutton);
    }

    SFPanel = new JPanel(new GridLayout(0, 1, 0, 0));
    SFPanel.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Structure factors"));
    optionsPanel.add(BorderLayout.CENTER, SFPanel);

    SFPanel.add(createEditParField(new FlowLayout(FlowLayout.LEFT, 1, 1),
        "Optional scale factor: ",
        thephase.parameterField[Phase.scaleFactorID]));

    JPanel rowP = new JPanel();
    rowP.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
    SFPanel.add(rowP);
    rowP.add(new JLabel("Structure Factor model: "));
    structureFactorModelsCB = new JComboBox();
    structureFactorModelsCB.setToolTipText("Select the Structure Factor model to be used by this phase");
    rowP.add(structureFactorModelsCB);
    JButton sfjb = new JButton("Model options");
    sfjb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        structureFactorOptions();
      }
    });
    rowP.add(sfjb);

/*		rowP = new JPanel();
		rowP.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
		SFPanel.add(rowP);
		rowP.add(new JLabel(" "));   */

    rowP = new JPanel();
    rowP.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
    SFPanel.add(rowP);
    rowP.add(new JLabel("Structure Factor extractor: "));
    structureFactorExtractorCB = new JComboBox();
    structureFactorExtractorCB.setToolTipText(
        "Select the Structure Factor extractor method to be used by this phase");
    rowP.add(structureFactorExtractorCB);
    sfjb = new JButton("Method options");
    sfjb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        structureFactorExtractionOptions();
      }
    });
    rowP.add(sfjb);

/*		rowP = new JPanel();
		rowP.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
		SFPanel.add(rowP);
		rowP.add(new JLabel(" "));   */

/*    rowP = new JPanel();
    rowP.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
    SFPanel.add(rowP);
    rowP.add(new JLabel("Structure solution model: "));
    structureSolutionCB = new JComboBox();
    structureSolutionCB.setToolTipText(
        "Select the Structure Solution method to be used by this phase, require a structure factor model like " +
        "the arbitrary or the structure solution");
    rowP.add(structureSolutionCB);
    sfjb = new JButton("Method options");
    sfjb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        structureSolutionOptions();
      }
    });
    rowP.add(sfjb);*/

    JPanel southPanel = new JPanel(new BorderLayout(3, 3));
    optionsPanel.add(BorderLayout.SOUTH, southPanel);

    SFPanel = new JPanel(new GridLayout(0, 1, 0, 0));
    SFPanel.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Scattering Models"));
    southPanel.add(BorderLayout.NORTH, SFPanel);

    rowP = new JPanel();
    rowP.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
    SFPanel.add(rowP);
    rowP.add(new JLabel("TDS model: "));
    tdsModelCB = new JComboBox();
    tdsModelCB.setToolTipText("Select the TDS model to be used by this phase");
    rowP.add(tdsModelCB);
    sfjb = new JButton("Model options");
    sfjb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        TDSOptions();
      }
    });
    rowP.add(sfjb);

    SFPanel = new JPanel(new GridLayout(0, 1, 0, 0));
    SFPanel.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Custom peaks list"));
    southPanel.add(BorderLayout.CENTER, SFPanel);

    rowP = new JPanel();
    rowP.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
    SFPanel.add(rowP);
/*    rowP.add(new JLabel("TDS model: "));
    tdsModelCB = new JComboBox();
    tdsModelCB.setToolTipText("Select the TDS model to be used by this phase");
    rowP.add(tdsModelCB); */
    sfjb = new JButton("Peak list editing");
    sfjb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        peakListEditing();
      }
    });
    rowP.add(sfjb);

    rowP.add(sfjb = new JIconButton("Delete.gif", "Remove peak list"));
    sfjb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        thephase.removePeakList();
      }
    });

    setTitle(aphase.toXRDcatString());
    initParameters();
    initListener();
    if (parent != null)
      parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    pack();
//    setResizable(true);
  }

  JPanel createHKLlistPanel() {
    JButton jb = null;
    JPanel hklPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
    JPanel intermediatePanel = new JPanel(new BorderLayout());
    JPanel panel1 = new JPanel();
    hklPanel.add(intermediatePanel);
    intermediatePanel.add(BorderLayout.CENTER, panel1);

//    thephase.CellSymmetry();

    panel1.setLayout(new GridLayout(0, 2, 0, 0));
    panel1.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Reflections list options"));

    JPanel panel2 = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
    panel1.add(panel2);
    panel2.add(new JLabel("Refinement d-space min"));
    panel2 = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
    panel1.add(panel2);
    panel2.add(minDspaceTF = new JTextField(Constants.FLOAT_FIELD));
    minDspaceTF.setText(thephase.getString(Phase.lowDspaceID));
    minDspaceTF.setToolTipText("To further restrict the number of reflections in the refinement set this value");
    panel2 = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
    panel1.add(panel2);
    panel2.add(new JLabel("Refinement d-space max"));
    panel2 = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
    panel1.add(panel2);
    panel2.add(maxDspaceTF = new JTextField(Constants.FLOAT_FIELD));
    maxDspaceTF.setText(thephase.getString(Phase.highDspaceID));
    maxDspaceTF.setToolTipText("To further restrict the number of reflections in the refinement set this value");

    panel2 = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
    panel1.add(panel2);
    panel2.add(new JLabel("Refln. list d-space min"));
    panel2 = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
    panel1.add(panel2);
    panel2.add(minReflnDspaceTF = new JTextField(Constants.FLOAT_FIELD));
    minReflnDspaceTF.setText(thephase.getString(Phase.lowReflnDspaceID));
    minReflnDspaceTF.setToolTipText("This value is used for the reflection list when no dataset/datafile is " +
        "available to define the range");
    panel2 = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
    panel1.add(panel2);
    panel2.add(new JLabel("Refln. list d-space max"));
    panel2 = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
    panel1.add(panel2);
    panel2.add(maxReflnDspaceTF = new JTextField(Constants.FLOAT_FIELD));
    maxReflnDspaceTF.setText(thephase.getString(Phase.highReflnDspaceID));
    maxReflnDspaceTF.setToolTipText("This value is used for the reflection list when no dataset/datafile is " +
        "available to define the range");


    panel2 = new JPanel(new FlowLayout(FlowLayout.CENTER));
    intermediatePanel.add(BorderLayout.SOUTH, panel2);
    JButton doneB = new JButton("Done");
    panel2.add(doneB);
    doneB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        panelLayout.show(generalContainer, "general");
      }
    });

    return hklPanel;
  }

  Vector cellComponentList = null;

  JPanel createCellPanel() {
    Component cellComp;
    cellComponentList = new Vector(0, 1);
    JPanel panel1 = new JPanel();

    thephase.CellSymmetry();

    panel1.setLayout(new GridLayout(0, 1, 3, 3));
    panel1.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Cell parameters"));

    String labels[] = {"a [Angstrom]:", "b [Angstrom]:", "c [Angstrom]:",
                       "alpha [degrees]:", "beta [degrees]: ", "gamma [degrees]:"};

    for (int i = 0; i < 6; i++)
      if (thephase.ic[i] == 1) {
        panel1.add(cellComp = createEditParField(new GridLayout(0, 2, 6, 6),
            labels[i],
            thephase.parameterField[i]));
        cellComponentList.add(cellComp);
      }

    return panel1;
  }

  void refreshCellPanel() {
    if (generalPanel == null) // for safety
      return;
    boolean changingPanel = false;
    if (cellPanel != null) {
      if (cellComponentList != null && cellComponentList.size() > 0) {
        for (int i = 0; i < cellComponentList.size(); i++) {
          Component comp = (Component) cellComponentList.get(i);
          removeComponentfromlist(comp);
        }
      }
      generalPanel.remove(cellPanel);
      changingPanel = true;
    }
    generalPanel.add(BorderLayout.CENTER, cellPanel = createCellPanel());
    if (changingPanel)
      pack();
  }

  public void broadeningOptions(int index) {
    String value = modelCB[index - 1].getSelectedItem().toString();
	  if (thephase.subordinateField[index] == null ||
			  !value.equals((thephase.subordinateField[index]).identifier))
		  if (index == 4 && value.contains("Warren") && thephase.getClosePackedType() == -1) {
			  thephase.setPlanarDefects("none pd");
			  modelCB[index - 1].setSelectedItem("none pd");
		  } else if (index == 4)
			  thephase.setPlanarDefects(value);
		  else
			  thephase.setsubordinateField(index, value);

	  thephase.subordinateField[index].getOptionsDialog(this).setVisible(true);
  }

  public JMenuBar createDefaultMenuBar() {
    JMenuBar amenubar = super.createDefaultMenuBar();
    amenubar.add(createToolsMenu());
    JMenu fileMenu = amenubar.getMenu(0);
    JMenu importStructure = new JMenu("Import structure...");
    fileMenu.insert(importStructure, 1);
    JMenuItem xyzMenu = new JMenuItem("XYZ molecule");
    importStructure.add(xyzMenu);
    xyzMenu.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        structPanel.importStructure(MolecularImporter.XYZformat);
      }
    });

    JMenuItem homoMenu = new JMenuItem("Tensor homogenization");
    fileMenu.insert(homoMenu, 2);
    homoMenu.addActionListener(new ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		    TensorHomogenization work = new TensorHomogenization(thephase);
		    TensorHomogenization.TensorHomogenizationFrame frame = work.getFrame();
		    frame.initmyFrame();
		    frame.setVisible(true);
	    }
    });


    return amenubar;
  }

  public JMenu createToolsMenu() {

    JMenuItem menuitem = null;

    JMenu optionsMenu = new JMenu("SDPD tools");
    optionsMenu.add(menuitem = new JMenuItem("Fix space group for texture (standard setting)"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
//        retrieveParameters();
        thephase.refreshAll(false);
        thephase.sghklcompute(false);
        String spaceGroup = getData().fixSpaceGroupForTexture();
        System.out.println("Changing space group to " + spaceGroup);
        manualSelection = true;
        changeSpaceGroup(spaceGroup);
        manualSelection = false;
//        retrieveParameters();
        structPanel.setatomsite();
        thephase.refreshAll(false);
        refreshCellPanel();
      }
    });
    optionsMenu.setMnemonic('t');
    optionsMenu.add(menuitem = new JMenuItem("Import Dicvol91 results"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        importDicvol91();
      }
    });
    optionsMenu.add(menuitem = new JMenuItem("Prepare Shelx instruction file"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        shelxInstruction();
      }
    });
    optionsMenu.add(menuitem = new JMenuItem("Solve Crystal Structure"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        (new PersistentThread() {
        public void executeJob() {
            AttentionD attnD = new AttentionD(PhaseD.this, "Solve Crystal Structure",
                "Please wait, solving the crystal structure...",
                "Binocular.gif", false);
            attnD.setVisible(true);
            getData().solveCrystalStructure();
            structPanel.setatomsite();
            attnD.setVisible(false);
            attnD.dispose();
            attnD = null;
          }
        }).start();
      }
    });
    optionsMenu.add(menuitem = new JMenuItem("Check atom positions"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        checkAtomPositions();
      }
    });
    optionsMenu.add(menuitem = new JMenuItem("Merge equivalent atoms"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        mergeEquivalentAtoms();
      }
    });
    optionsMenu.add(menuitem = new JMenuItem("Check atom occupancy"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        getData().refreshOccupancyAndQuantity();
        structPanel.setatomsite();
      }
    });
	  optionsMenu.add(menuitem = new JMenuItem("Normalize occupancy (XRF only)"));
	  menuitem.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent e) {
			  getData().normalizeOccupancies();
			  structPanel.setatomsite();
		  }
	  });
    optionsMenu.add(menuitem = new JMenuItem("-"));
    optionsMenu.add(menuitem = new JMenuItem("Smart index"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        EvolutionarySmartIndexing indexing =
            new EvolutionarySmartIndexing(getData(), getData().getFilePar().getAllDatafiles());
        indexing.startDialog(PhaseD.this);
      }
    });
    
    optionsMenu.add(menuitem = new JMenuItem("Generate indexing data for NN"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        (new GenerateIndexingDataUI((Sample) getData().getParent(), true)).setVisible(true);
      }
    });

    if (Constants.testing) {
	    optionsMenu.add(menuitem = new JMenuItem("Generate pattern for G(r) PDF calculation"));
	    menuitem.addActionListener(new ActionListener() {
		    public void actionPerformed(ActionEvent e) {
			    (new PDFUtilitiesUI(getData(), true)).setVisible(true);
		    }
	    });
    }

	  return optionsMenu;
  }

  public Phase getData() {
    return thephase;
  }

  public void checkAtomPositions() {
    retrieveParameters();
    getData().checkAtomPositions();
    structPanel.setatomsite();
  }

  public void mergeEquivalentAtoms() {
    retrieveParameters();
    getData().mergeEquivalentAtoms();
    structPanel.setatomsite();
  }

	public String getSingleSpaceGroupString(String allTogheter) {
		StringTokenizer st = new StringTokenizer(allTogheter, "|");
		String number = st.nextToken(); // int number
		String herman_maguin = st.nextToken();
		String hall = st.nextToken(); // Hall
		String schoenflies = st.nextToken(); // Schoenflies

		// todo: choose based on the convention

		return herman_maguin.substring(1, herman_maguin.length() - 1);
	}

  public void retrieveParameters() {
    structPanel.retrieveParameters();
    if (thephase != null) {
      thephase.setPhaseID(phaseid.getText());
      thephase.setSymmetry(symmetrychoice.getSelectedItem().toString());
//      thephase.setSGconv(conventionchoice.getSelectedItem().toString());
	    if (SpaceGroups.useCCTBX())
        thephase.setSpaceGroup(false, getSingleSpaceGroupString(spacegrouplist.getSelectedItem().toString()), false);
	    else
		    thephase.setSpaceGroup(false, spacegrouplist.getSelectedItem().toString(), false);
    }
    thephase.setAbsorptionCrystSize(grainsize.getText());

    for (int i = 1; i < 5; i++) {
      String value = modelCB[i - 1].getSelectedItem().toString();
      if (thephase.subordinateField[i] == null ||
          !value.equals(thephase.subordinateField[i].identifier)) {
        if (i == 4 && value.contains("Warren") && thephase.getClosePackedType() == -1) {
	        thephase.setPlanarDefects("none pd");
        } else if (i == 4)
	        thephase.setPlanarDefects(value);
		    else
          thephase.setsubordinateField(i, value);
      }
    }
    for (int i = 0; i < optionsIndices.length; i++) { //absorption removed here
      String value = optchoice[i].getSelectedItem().toString();
      if (thephase.subordinateField[optionsIndices[i]] == null ||
          !value.equals(thephase.subordinateField[optionsIndices[i]].identifier))
        thephase.setsubordinateField(optionsIndices[i], value);
    }

    thephase.setString(Phase.lowDspaceID, minDspaceTF.getText());
    thephase.setString(Phase.highDspaceID, maxDspaceTF.getText());
    thephase.setString(Phase.lowReflnDspaceID, minReflnDspaceTF.getText());
    thephase.setString(Phase.highReflnDspaceID, maxReflnDspaceTF.getText());

    super.retrieveParameters();
  }

  public void subordinateOptions(int index) {
    String value = optchoice[index].getSelectedItem().toString();
    if (thephase.subordinateField[optionsIndices[index]] == null ||
        !value.equals(thephase.subordinateField[optionsIndices[index]].identifier))
      thephase.setsubordinateField(optionsIndices[index], value);

    thephase.subordinateField[optionsIndices[index]].getOptionsDialog(this).setVisible(true);
  }

  public void initParameters() {
    phaseid.setText(thephase.getPhaseID());
    symmetrychoice.setSelectedItem(thephase.getSymmetry());
//    conventionchoice.setSelectedIndex(thephase.getSGconv());
    initspacegroup(thephase.getSymmetry(), thephase.getSpaceGroup(), thephase.getSGconv());
//		structPanel.initatomlist();
    for (int i = 0; i < thephase.getsubordClassNumber(Phase.structureFactorExtractorID); i++)
      structureFactorExtractorCB.addItem(thephase.getsubordIdentifier(Phase.structureFactorExtractorID, i));
    for (int i = 0; i < thephase.getsubordClassNumber(Phase.structureFactorModelID); i++)
      structureFactorModelsCB.addItem(thephase.getsubordIdentifier(Phase.structureFactorModelID, i));
//    for (int i = 0; i < thephase.getsubordClassNumber(Phase.structureSolutionMethodID); i++)
//      structureSolutionCB.addItem(thephase.getsubordIdentifier(Phase.structureSolutionMethodID, i));
    for (int i = 0; i < thephase.getsubordClassNumber(Phase.tdsModelID); i++)
      tdsModelCB.addItem(thephase.getsubordIdentifier(Phase.tdsModelID, i));
    structureFactorExtractorCB.setSelectedItem(thephase.getActiveSubordinateModel(
		    Phase.structureFactorExtractorID).identifier);
    structureFactorModelsCB.setSelectedItem(thephase.getActiveSubordinateModel(
		    Phase.structureFactorModelID).identifier);
    tdsModelCB.setSelectedItem(thephase.getActiveSubordinateModel(Phase.tdsModelID).identifier);
//    structureSolutionCB.setSelectedItem(thephase.getActiveSubordinateModel(
//        Phase.structureSolutionMethodID).identifier);
    for (int i = 0; i < optionsIndices.length; i++) { //absorption removed here
      for (int j = 0; j < thephase.getsubordClassNumber(optionsIndices[i]); j++)
        optchoice[i].addItem(thephase.getsubordIdentifier(optionsIndices[i], j));
      optchoice[i].setSelectedItem(thephase.subordinateField[optionsIndices[i]].identifier);
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
/*		atomtypechoice.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent event) {
				chooseTheAtom();
			}
		}); */
    spacegrouplist.addItemListener(new ItemListener() {
      public void itemStateChanged(ItemEvent event) {
        spacegrouplist_ListSelect();
      }
    });
/*    conventionchoice.addItemListener(new ItemListener() {
      public void itemStateChanged(ItemEvent event) {
        conventionchoice_Action();
      }
    });*/

    structureFactorExtractorCB.addItemListener(new ItemListener() {
	    public void itemStateChanged(ItemEvent event) {
		    thephase.setSubordinateModel(Phase.structureFactorExtractorID,
				    structureFactorExtractorCB.getSelectedItem().toString());
		    thephase.notifySubordinateReflectionListChanged();
	    }
    });
    structureFactorModelsCB.addItemListener(new ItemListener() {
	    public void itemStateChanged(ItemEvent event) {
		    thephase.refreshOccupancyAndQuantity();
		    structPanel.setatomsite();
		    thephase.setSubordinateModel(Phase.structureFactorModelID,
				    structureFactorModelsCB.getSelectedItem().toString());
		    thephase.gethklNumber();
		    thephase.notifySubordinateReflectionListChanged();
	    }
    });
    tdsModelCB.addItemListener(new ItemListener() {
	    public void itemStateChanged(ItemEvent event) {
		    thephase.setSubordinateModel(Phase.tdsModelID,
				    tdsModelCB.getSelectedItem().toString());
	    }
    });
/*    structureSolutionCB.addItemListener(new ItemListener() {
      public void itemStateChanged(ItemEvent event) {
        thephase.setSubordinateModel(Phase.structureSolutionMethodID,
            structureSolutionCB.getSelectedItem().toString());
      }
    });*/

  }

  public void initspacegroup(String symmetry, String spacegroup, int sgconv) {
//		SpaceGroupTF.setText(spacegroup);
    int symnumber = loadspacegroup(symmetry, spacegroup, sgconv);
//		manualSelection = true;
    spacegrouplist.setSelectedIndex(symnumber);
//					spacegrouplist.makeVisible(symnumber);
  }

  public void changeSpaceGroup(String spacegroup) {
    spacegrouplist.setSelectedItem(spacegroup);
  }

  public int loadspacegroup(String symmetry, String spacegroup, int sgconv) {


	  if (SpaceGroups.useCCTBX()) {

		  CrystalSystem selCrystalSystem = null;
		  int i = 0;
		  while (selCrystalSystem == null && i < crystalSystems.size()) {
			  CrystalSystem actualCS = crystalSystems.elementAt(i++);
//		  System.out.println("Checking: " + actualCS.symmetry + " == " + symmetry);
			  if (actualCS.symmetry.equalsIgnoreCase(symmetry))
				  selCrystalSystem = actualCS;
		  }

		  if (spacegrouplist.getItemCount() > 0)
			  spacegrouplist.removeAllItems();

		  int selectedsg;
		  String ansg;

		  selectedsg = -1;

		  for (i = 0; i < selCrystalSystem.space_groups.size(); i++) {
			  ansg = selCrystalSystem.space_groups.elementAt(i).hermann_mauguin;
			  spacegrouplist.addItem(selCrystalSystem.space_groups.elementAt(i).number + " | " + ansg + " | " +
					  selCrystalSystem.space_groups.elementAt(i).hall + " | " + selCrystalSystem.space_groups.elementAt(i).schoenflies);
			  if (Misc.toStringDeleteBlank(ansg).equalsIgnoreCase(Misc.toStringDeleteBlank(spacegroup)))
				  selectedsg = i;
		  }
		  if (selectedsg == -1) {
//	    System.out.println("Not identified from list: " + spacegroup);
			  for (i = 0; i < selCrystalSystem.space_groups.size(); i++) {
				  ansg = Misc.toStringDeleteBlank(selCrystalSystem.space_groups.elementAt(i).hermann_mauguin);
				  if (ansg.startsWith(spacegroup)) {
					  int nextIndex = spacegroup.length();
					  if (ansg.charAt(nextIndex) == ':') {
						  selectedsg = i;
						  i = selCrystalSystem.space_groups.size() + 1;
					  }
				  }
			  }
		  }
		  if (selectedsg == -1)
			  selectedsg = 0;
		  return selectedsg;
	  } else {
		  int i, selectedsg;
		  String ansg;

		  int istart = SpaceGroups.getBeginSG(symmetry, sgconv);
		  int iend = SpaceGroups.getEndSG(symmetry, sgconv);
		  selectedsg = -1;

		  if (spacegrouplist.getItemCount() > 0)
			  spacegrouplist.removeAllItems();
		  for (i = istart; i <= iend; i++) {
			  ansg = new String(Phase.getSpaceGroup(i, sgconv));
			  spacegrouplist.addItem(ansg);
			  if (ansg.equalsIgnoreCase(spacegroup))
				  selectedsg = i;
		  }
		  if (selectedsg == -1) {
			  for (i = istart; i <= iend; i++) {
				  ansg = spacegrouplist.getModel().getElementAt(i - istart).toString();
				  if (ansg.startsWith(spacegroup)) {
					  int nextIndex = spacegroup.length();
					  if (ansg.charAt(nextIndex) == ':') {
						  selectedsg = i;
						  i = iend + 1;
					  }
				  }
			  }
		  }
		  if (selectedsg == -1)
			  selectedsg = istart;
		  return selectedsg - istart;
	  }
  }

  void symmetrychoice_Action() {
    String newsym = symmetrychoice.getSelectedItem().toString();
    if (!newsym.equalsIgnoreCase(thephase.getSymmetry())) {
      thephase.setSymmetry(newsym);
      initspacegroup(newsym, thephase.getSpaceGroup(), thephase.getSGconv());
      thephase.refreshAll(false);
      refreshCellPanel();
    }
  }

  void spacegrouplist_ListSelect() {
    if (spacegrouplist.getSelectedItem() != null && !manualSelection) {
      thephase.refreshAll(false);
      thephase.sghklcompute(false);
	    if (SpaceGroups.useCCTBX())
        thephase.setSpaceGroup(false, getSingleSpaceGroupString(spacegrouplist.getSelectedItem().toString()), true);
	    else
	      thephase.setSpaceGroup(false, spacegrouplist.getSelectedItem().toString(), true);
      structPanel.setatomsite();
      thephase.refreshAll(false);
      refreshCellPanel();
    }
    manualSelection = false;
  }

/*  void conventionchoice_Action() {
    String newconv = conventionchoice.getSelectedItem().toString();
    if (!newconv.equalsIgnoreCase(thephase.getSGconvS())) {
      int oldsel = spacegrouplist.getSelectedIndex();
      thephase.setSGconv(newconv);
      loadspacegroup(symmetrychoice.getSelectedItem().toString(), thephase.getSpaceGroup(), thephase.getSGconv());
      spacegrouplist.setSelectedIndex(oldsel);
    }
//		else
//			loadspacegroup(newsym);
  }*/

  public void show_hkl() {
    final myJFrame jf = new myJFrame(this, "(hkl) list");
    jf.createDefaultMenuBar();
    thephase.refreshAll(false);
    TableModel hklModel = new hklTableModel(thephase);
    final JTable hkltable = new JTable(hklModel);
    JScrollPane scrollpane = new JScrollPane(hkltable);
//		scrollpane.setBorder(new LineBorder(Color.black));
    Container c1 = jf.getContentPane();
    c1.setLayout(new BorderLayout());
    c1.add(scrollpane, BorderLayout.CENTER);

    JPanel jpp = new JPanel();
    jpp.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));

    JCloseButton jb = new JCloseButton();
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        jf.setVisible(false);
        jf.dispose();
      }
    });
    jpp.add(jb);
    c1.add(jpp, BorderLayout.SOUTH);
    jf.getRootPane().setDefaultButton(jb);
    jf.setSize(640, 400);
    jf.setVisible(true);
  }

  public void show_generalpositions() {
    final myJFrame jf = new myJFrame(this, "general site positions");
    jf.createDefaultMenuBar();
    TableModel generalPosModel = new generalPosTableModel(thephase);
    JTable generalPostable = new JTable(generalPosModel);
    JScrollPane scrollpane = new JScrollPane(generalPostable);
//		scrollpane.setBorder(new LineBorder(Color.black));
    Container c1 = jf.getContentPane();
    c1.setLayout(new BorderLayout());
    c1.add(scrollpane, BorderLayout.CENTER);
    JPanel jp = new JPanel();
    jp.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
    JCloseButton jb = new JCloseButton();
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        jf.setVisible(false);
        jf.dispose();
/*			  java.awt.EventQueue.invokeLater(new Runnable() {
		      @Override
		      public void run() {
			      PhaseD.this.toFront();
			      PhaseD.this.repaint();
		      }
	      });*/
      }
    });
    jp.add(jb);
    c1.add(jp, BorderLayout.SOUTH);
    jf.getRootPane().setDefaultButton(jb);
    jf.setSize(600, 350);
    jf.setVisible(true);
  }

  public void dispose() {
    if (structPanel != null)
      structPanel.dispose();
    thephase.refreshAll(false);
    thephase = null;
    sglistv = null;
    for (int i = 0; i < optionsIndices.length; i++) {
      optchoice[i].removeAllItems();
    }
    optchoice = null;
    super.dispose();
  }

  public void structureFactorOptions() {
    thephase.getActiveSubordinateModel(Phase.structureFactorModelID).
        getOptionsDialog(this).setVisible(true);
  }

  public void structureFactorExtractionOptions() {
    thephase.getActiveSubordinateModel(Phase.structureFactorExtractorID).
        getOptionsDialog(this).setVisible(true);
  }

/*  public void structureSolutionOptions() {
    thephase.getActiveSubordinateModel(Phase.structureSolutionMethodID).
        getOptionsDialog(this).setVisible(true);
  }*/

  public void TDSOptions() {
    thephase.getActiveSubordinateModel(Phase.tdsModelID).
        getOptionsDialog(this).setVisible(true);
  }

  public void peakListEditing() {
    PeakListFrame frame = new PeakListFrame(this);
    frame.setVisible(true);
  }

  public void importDicvol91() {
    final String filename = Utility.browseFilename(this, "Import Dicvol91 results");
    if (filename != null) {
      (new PersistentThread() {
        public void executeJob() {
          importDicvol91(filename);
        }
      }).start();
    }
  }

  public void importDicvol91(String filename) {
    DicVol91resultFile resultParser = (new DicVol91resultFile(filename, this));
    DicVol91Result result = resultParser.getSelectedResult();
    if (thephase != null) {
      Vector spaceGroupList = thephase.sortDicVol91Result(result);

      new SelectBestSpaceGroupDialog(this, "Select the space group", true, spaceGroupList);
      int resultSG = getResult();
      int[] spaceGroup = (int[]) spaceGroupList.elementAt(resultSG);
      String sgname = SpaceGroups.getSpaceGroup(spaceGroup[1], 2);
      thephase.setSymmetry(SpaceGroups.getSymmetry(spaceGroup[0]));
      thephase.setSpaceGroup(true, sgname, false);
      symmetrychoice.setSelectedItem(thephase.getSymmetry());
      initspacegroup(thephase.getSymmetry(), thephase.getSpaceGroup(), thephase.getSGconv());
      refreshCellPanel();
    }
  }

  public void shelxInstruction() {
    retrieveParameters();
    SDPDutilities.prepareAndSaveShelXInstruction(thephase, this);
  }

  private int selectedSpageGroup = -1;

  public void setResult(int selection) {
    selectedSpageGroup = selection;
  }

  public int getResult() {
    return selectedSpageGroup;
  }

  public static String GENETIC = "Genetic";
  public static String NEURAL_NET = "Neural Net";
  String[] indexingMethods = {GENETIC, NEURAL_NET, SMART_INDEXING};
  public static String SMART_INDEXING = "Smart indexing";

  class PeakListFrame extends JOptionsDialog {

    JSubordListPane peakListP = null;
    JComboBox indexingComboBox = null;

    public PeakListFrame(Frame aframe) {
      super(aframe, thephase, "Custom peak list editing");

      principalPanel.setLayout(new BorderLayout(9, 9));

      peakListP = new JSubordListPane(PeakListFrame.this, false);
      principalPanel.add(BorderLayout.NORTH, peakListP);

      JPanel buttonP = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
      principalPanel.add(BorderLayout.CENTER, buttonP);
      JButton jb = new JButton("Refine Intensity");
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          thephase.customPeaksHeightRefinable();
        }
      });
      buttonP.add(jb);
      jb = new JButton("Refine all");
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          thephase.customPeaksRefinable();
        }
      });
      buttonP.add(jb);
      jb = new JButton("Fix all");
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          thephase.customPeaksNotRefinable();
        }
      });
      buttonP.add(jb);
      jb = new JButton("Bound Size-Strain");
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          thephase.customPeaksBoundSizeStrain();
        }
      });
      buttonP.add(jb);

      buttonP = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 6));
      principalPanel.add(BorderLayout.SOUTH, buttonP);
      indexingComboBox = new JComboBox();
      for (int i = 0; i < indexingMethods.length; i++)
        indexingComboBox.addItem(indexingMethods[i]);
      buttonP.add(indexingComboBox);
      jb = new JButton("Indexing");
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          index(thephase, PhaseD.this, (String) indexingComboBox.getSelectedItem());
        }
      });
      buttonP.add(jb);

      jb = new JButton("Import txt list");
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          Vector list = BasicIndexingUtilities.importPeakList(PhaseD.this);
          if (list != null) {
            int numberOfPeaks = list.size() - 1;
            double wave = ((Double) list.elementAt(numberOfPeaks)).doubleValue();
            if (numberOfPeaks > 0) {
              double[] pos = new double[numberOfPeaks];
              for (int i = 0; i < numberOfPeaks; i++)
                pos[i] = wave / (MoreMath.sind(((Double) list.elementAt(i)).doubleValue() / 2.0) * 2.0);
              thephase.setCustomPeakList(pos);
            }
          }
        }
      });
      buttonP.add(jb);

      jb = new JButton("Export list for Dicvol");
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          ListVector peaksList = thephase.getCustomPeaksList();
          int nop = peaksList.size();
          double[][] peakPositions = new double[2][nop];
          for (int ir = 0; ir < nop; ir++) {
            Reflex reflx = (Reflex) peaksList.elementAt(ir);
            peakPositions[0][ir] = reflx.getDspacing().getValueD();
            peakPositions[1][ir] = Double.parseDouble(reflx.getDspacing().getError());
          }
          BasicIndexingUtilities.exportPeaksDicvol91(peakPositions, 0.0, 3, "Peaks from Maud fitting");
        }
      });
      buttonP.add(jb);

      jb = new JButton("Export list for McMaille");
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          ListVector peaksList = thephase.getCustomPeaksList();
          int nop = peaksList.size();
          double[][] peakPositions = new double[2][nop];
          for (int ir = 0; ir < nop; ir++) {
            Reflex reflx = (Reflex) peaksList.elementAt(ir);
            double dspace = reflx.getDspacing().getValueD();
            double lOver2d = Constants.defaultWavelengthIndexing / (2.0 * dspace);
//            double derror = Double.parseDouble(reflx.getDspacing().getError()) * 3.0;
            peakPositions[0][ir] = 2.0 * MoreMath.asind(lOver2d);
            peakPositions[1][ir] = reflx.getIntensity().getValueD();
            // derror * (1.0 - lOver2d * lOver2d) / Constants.defaultWavelengthIndexing;
          }
          BasicIndexingUtilities.exportPeaksForMcMaille(peakPositions, Constants.defaultWavelengthIndexing,
              thephase.getLabel());
        }
      });
      buttonP.add(jb);

      initparameters();

      pack();

    }

    public void initparameters() {
      String labels[] = {"d-spacing:", "Fhkl2:", "Size:", "Microstrain:"};
      peakListP.setList(thephase, 1, 4, labels);
    }

    public void retrieveParameters() {
      super.retrieveParameters();
      peakListP.retrieveparlist();
    }

    public void dispose() {
      peakListP.dispose();
      super.dispose();
    }

    public void index(Phase aphase, JFrame parentFrame, String method) {
//    for (int symmetry = 0; symmetry < 1 /*SpaceGroups.SYMMETRY.length*/; symmetry++) {
//      System.out.println("Indexing in "+SpaceGroups.SYMMETRY[symmetry] + " symmetry.....");
      String symmetry = aphase.getSymmetry();
      int[] ic = GeneticAlgorithmIndexing.cellSymmetryControls(SpaceGroups.getSymmetryNumber(symmetry));
      int numberOfFreeParameters = 0;
      for (int i = 0; i < 6; i++) {
        if (ic[i] == 1)
          numberOfFreeParameters++;
      }
      double[] parameters = new double[numberOfFreeParameters];
      double[] lowerBound = new double[numberOfFreeParameters];
      double[] upperBound = new double[numberOfFreeParameters];
      int index = 0;
      for (int i = 0, j = 0; i < 6; i++)
        if (ic[i] == 1) {
          parameters[j] = aphase.getCellValue(i);
          lowerBound[j] = aphase.getCellValueMinD(i);
          upperBound[j++] = aphase.getCellValueMaxD(i);
        }

      double[][] peakList = aphase.getCustomPeakPositionsAndErrors();

      if (method.equalsIgnoreCase(SMART_INDEXING)) {
        EvolutionarySmartIndexing indexing = new EvolutionarySmartIndexing(aphase,
            getData().getFilePar().getAllDatafiles());
        indexing.startDialog(this);
      } else if (method.equalsIgnoreCase(GENETIC)) {
        GeneticAlgorithmIndexing indexing = new GeneticAlgorithmIndexing(aphase, peakList, symmetry,
              parameters, lowerBound, upperBound);
        indexing.startDialog(this);
      } else {
        ArtificialNeuralNetworkIndexing anni = new ArtificialNeuralNetworkIndexing(aphase, peakList);
        anni.solve();
      }
    }

  }

  class SelectBestSpaceGroupDialog extends myJDialog {

    JList thelist;
//		Hashtable originalindexes;

    public SelectBestSpaceGroupDialog(Frame parent, String title, boolean modal,
                                      Vector avector) {

      super(parent, title, modal);

      Container pane = getContentPane();
      pane.setLayout(new BorderLayout(6, 6));
      Vector alist = new Vector(avector.size(), 1);
      for (int i = 0; i < avector.size(); i++) {
        int[] spsg = (int[]) avector.elementAt(i);
        StringBuffer label = (new StringBuffer("Symmetry: ")).append(SpaceGroups.SYMMETRY[spsg[0]])
            .append(", Space group: ")
            .append(SpaceGroups.getSpaceGroup(spsg[1], 2))
            .append(", Not present: ")
            .append(Integer.toString(spsg[2]))
            .append(", extra: ")
            .append(Integer.toString(spsg[3]))
            .append(", totalIndexed: ")
            .append(Integer.toString(spsg[4]));
        alist.addElement(label);
      }
      thelist = new JList(alist);

      thelist.setVisibleRowCount(10);
      thelist.setPrototypeCellValue("123456789012345678901234567890");
      JScrollPane sp1 = new JScrollPane();
//			sp1.setBorder(new LineBorder(Color.black));
      sp1.getViewport().add(thelist);
      pane.add("Center", sp1);

      JPanel panel1 = new JPanel();
      panel1.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      pane.add("South", panel1);

      JButton jb;

      panel1.add(jb = new JIconButton("Check.gif", "Choose"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          setVisible(false);
          SelectBestSpaceGroupDialog.this.dispose();
        }
      });
      panel1.add(jb = new JCancelButton());
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          setVisible(false);
          SelectBestSpaceGroupDialog.this.dispose();
        }
      });
      if (!modal)
        setHelpButton(panel1);

      pack();
      setVisible(true);

    }

    public void retrieveParameters() {
      setResult(thelist.getSelectedIndex());
    }

  }
}
