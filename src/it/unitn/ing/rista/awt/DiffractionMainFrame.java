/*
 * @(#)DiffractionMainFrame.java created Oct 12, 2004 Casalino
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

import com.radiographema.MaudText;
import it.unitn.ing.rista.awt.treetable.*;
import it.unitn.ing.rista.comp.OutputPanel;
import it.unitn.ing.rista.comp.ParallelComputationController;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.io.COD.CODdatabaseConnector;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.wizard.HIPPOWizard.HIPPOWizard;
import it.unitn.ing.xgridclient.Client;
import it.unitn.ing.xgridclient.XGridClient;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.StringTokenizer;


/**
 * The DiffractionMainFrame is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.13 $, $Date: 2006/12/04 14:30:03 $
 * @since JDK1.1
 */

public class DiffractionMainFrame extends principalJFrame implements TreeEventReceiver {

  OutputPanel outputPanel;
  SpectrumPlotPanel datafilePlotPanel = null;
  MultiPlotFitting2DPanel datafile2DPlotPanel = null;
  DifferencePlot2DPanel residuals2DPlotPanel = null;
  JPanel parListPanel = null;
  JTreeTable treeTable = null;
  JList phaseList = null;
  JList datasetsList = null;
  JList samplesList = null;
  JTabbedPane westTB = null;
  JRemoveButton removeB = null;
  JSplitPane eastPanel = null, westPanel = null, principalPanel = null;
  final static String dividerPstring = "diffractionMainFrame.dividerPRINCIPAL";
  final static String dividerEstring = "diffractionMainFrame.dividerEAST";
  final static String dividerWstring = "diffractionMainFrame.dividerWEST";
  final static String windowWidth = "diffractionMainFrame.windowWidth";
  final static String windowHeight = "diffractionMainFrame.windowHeight";
  boolean refreshTreeTable = true;
  boolean parListPaneSouth = true;

  // menu commands Strings

  static final String MENU_SPECIAL = "Special";
  static final String MENU_HELP = "Help";
  static final String MENU_NEW_ANALYSIS = "General analysis";
  static final String MENU_HIPPO_WIZARD = "Hippo wizard";
  static final String ANALYSIS_DEFAULT = " analysis";
  static final String ANALYSIS_ALTERNATE = " general analysis";
  static final String ANALYSIS_CAPILLARY = " capillary analysis";
  static final String ANALYSIS_BRAGG = " Bragg analysis";
  static final String ANALYSIS_GOEBBELS = " reflectivity analysis";

  static String[] mainMenuLabels = {
      "File:15",
        "New:2",
          MENU_NEW_ANALYSIS,
          MENU_HIPPO_WIZARD,
        "Open analysis...",
        "Load datafile...",
        "Restore",
        "-",
        "Save analysis",
        "Save analysis as...",
        "-",
        "Append simple results to...",
        "Append results to...",
        "-",
        "Print plot",
        "Print window",
        "-",
        "Quit",
      "Edit:8",
        "Add new object",
        "Load object from CIF...",
        "Save object to database",
        "Delete object",
        "-",
        "Edit object",
        "-",
        "Duplicate object",
      "Analysis:8",
        "Options",
        "Wizard",
        "Parameters list",
        "Compute spectra",
        "Refine",
        "Results",
        "-",
        "Preferences",
      "Graphic:10",
        "Plot selected dataset",
        "MapPlot of selected dataset",
        "Difference 2D Plot of selected dataset",
        "Polar plot of selected dataset",
		    "Section plot of selected dataset",
        "Texture plot",
        "-",
        "Plot options",
        "-",
        "Waiting for computation...",
      MENU_SPECIAL + ":4",
        "Submit structure to COD",
        "Load RSS feed",
        "-",
        "Refine in batch...",
      MENU_HELP + ":7",
        "Readme", // Help
        "Introduction",
        "Tutorial",
        "Content",
        "License Agreement",
        "-",
        "About the program..."
  };

  static final char[] menuMnemonic = {'f', 'e', 'r', 'g', 's', 'h'};

  static final int nullKeyEvent = -999;

  static final int[] menuNewKeyEvent = {
      nullKeyEvent,
      nullKeyEvent,
      KeyEvent.VK_N,
      nullKeyEvent,
      KeyEvent.VK_O,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,
      KeyEvent.VK_S,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,
      KeyEvent.VK_Q,

      nullKeyEvent,
      KeyEvent.VK_A,
      KeyEvent.VK_D,
      KeyEvent.VK_K,
      KeyEvent.VK_Z,
      nullKeyEvent,
      KeyEvent.VK_E,
      nullKeyEvent,
      nullKeyEvent,

      nullKeyEvent,
      nullKeyEvent,
      KeyEvent.VK_W,
      KeyEvent.VK_L,
      KeyEvent.VK_M,
      KeyEvent.VK_R,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,

      nullKeyEvent,
      KeyEvent.VK_P,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,
		  nullKeyEvent,
      KeyEvent.VK_T,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,

      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,

      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent,
      nullKeyEvent
  };

  static boolean[] menuNewItemEnabled = {
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,

      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,

      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,

      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
		  true,
      true,

      true,
      true,
      true,
      true,
      true,

      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true
  };

  static String[][] mainMenuCommand = {{"New analysis", // File
      "Open analysis...",
      "Load datafile...",
      "Restore",
      "-",
      "Save analysis",
      "Save analysis as...",
      "-",
      "Append simple results to...",
      "Append results to...",
      "-",
      "Print plot",
      "Print window",
      "-",
      "Quit"},
      {"Add new object", // Edit
          "Load object from CIF...",
          "Save object to database",
          "Delete object",
          "-",
          "Edit object",
          "-",
          //"Cut object",
          //"Copy object",
          //"Paste object",
          "Duplicate object",
          null,
          null,
          null,
          null,
          null,
          null,
          null},
      {"Options", // Refinement
          "Wizard",
          "Parameters list",
          "Compute spectra",
          "Refine",
          "Results",
          "-",
          "Preferences",
          null,
          null,
          null,
          null,
          null,
          null,
          null},
      {"Plot selected dataset", // Graphic
          "MapPlot of selected dataset",
          "Difference 2D Plot of selected dataset",
          "Polar plot of selected dataset",
		      "Section plot of selected dataset",
          "Texture plot",
          "-",
          "Plot options",
          "-",
          "Waiting for computation...",
          null,
          null,
          null,
          null,
          null,
          null},
      {"Submit structure to COD",
          "Load RSS feed",
          Constants.testing ? "ESQUI client" : null, // Special
          Constants.testing ? "Test COD JDBC connection" : null,
          Constants.testing ? "Test COD HTTP connection" : null,
          //Constants.testing ? "Start refine on Xgrid" : null,
		      "Refine in batch...",
          null,
          null,
          null,
          null,
          null,
          null,
          null,
          null,
          null},
      {"Readme", // Help
          "Introduction",
          "Tutorial",
          "Content",
          "License Agreement",
          "-",
          "About the program...",
          null,
          null,
          null,
          null,
          null,
          null,
          null,
          null}
  };

  static String[] helpFilenames = {"readme.txt",
      "intro.txt",
      "tutorial.txt",
      "content.txt",
      "license_maud.txt"};

  static String[] buttonMenuCommand = {"New", "Load", "Save", "Delete"};

  static boolean simple = false;

  public DiffractionMainFrame(boolean simple) {
    this();
    this.simple = simple;
  }

  public DiffractionMainFrame() {
    super(true);
  }

  int menuIndex = 0;
  int menuBarIndex = 0;

  void initNewMenuBar(DiffractionMenuAction actionMenuListener) {
    JMenuBar mb = new JMenuBar();
    setJMenuBar(mb);

    int pos;
    int maxMenuIndex = mainMenuLabels.length;
    while (menuIndex < maxMenuIndex) {
      // Insert the interface menu
      String label = mainMenuLabels[menuIndex];
//      System.out.println("JMenuBar, " + menuIndex + ": " + label);
      if ((pos = label.indexOf(":")) >= 0 && label.substring(0, pos).equals(MENU_HELP)) {
        /*JMenu amenu = */addOptionsMenu(mb, actionMenuListener);
//        amenu.setMnemonic(menuMnemonic[menuBarIndex++]);
      }
      JMenu amenu = (JMenu) addMenuItem(mb, actionMenuListener);
      if (label.substring(0, pos).equals(MENU_SPECIAL)) {         // Append network items to Special menu
        addNetworkComputingMenuItems(amenu, actionMenuListener);
      }
    }
  }

  Object addMenuItem(Object menu, DiffractionMenuAction actionMenuListener) {
    JMenu amenu;
    int pos;
    String label = mainMenuLabels[menuIndex++];
/*    if (menu instanceof JMenu)
      System.out.println(((JMenu) menu).getText() + ", " + (menuIndex - 1) + ": " + label);
    if (menu instanceof JMenuBar)
      System.out.println("JMenuBar, " + (menuIndex - 1) + ": " + label);*/
    if ((pos = label.indexOf(":")) >= 0) {
      amenu = new JMenu(label.substring(0, pos));
      amenu.setEnabled(menuNewItemEnabled[menuIndex - 1]);
      int menuitems = Integer.parseInt(label.substring(pos + 1, label.length()));
      if (menu instanceof JMenuBar) {
        ((JMenuBar) menu).add(amenu);
        amenu.setMnemonic(menuMnemonic[menuBarIndex++]);
//        System.out.println(menuitems + ": " + amenu.getText());
        for (int i = 0; i < menuitems; i++) {
//          System.out.println(i + ": " + menuIndex);
          addMenuItem(amenu, actionMenuListener);
        }
        return amenu;
      } else {
        ((JMenu) menu).add(amenu);
//        System.out.println(menuitems + ": " + amenu.getText());
        for (int i = 0; i < menuitems; i++) {
//          System.out.println(i + ": " + menuIndex);
          addMenuItem(amenu, actionMenuListener);
        }
        return amenu;
      }
    } else {
      if (label.equalsIgnoreCase("-"))
        return ((JMenu) menu).add(new JSeparator());
      else {
        JMenuItem amenuitem = ((JMenu) menu).add(new JMenuItem(label));
        if (menuNewKeyEvent[menuIndex - 1] != nullKeyEvent)
          amenuitem.setAccelerator(KeyStroke.getKeyStroke(menuNewKeyEvent[menuIndex - 1],
                                   Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        amenuitem.addActionListener(actionMenuListener);
        amenuitem.setEnabled(menuNewItemEnabled[menuIndex - 1]);
        return amenuitem;
      }
    }
  }

  JToolBar initNewToolBar(DiffractionMenuAction actionMenuListener) {
    String[] iconTB = {"New.gif", "OpenDoc.gif", "Redo.gif", "Save.gif", "SaveAll.gif", null, null,
        "Eyeball.gif", null,
        "Box.gif", "DataExtract.gif", "DataStore.gif", null, "Delete.gif",
        null, null,
        "Bulb.gif", "Calculator.gif", Constants.refineIcon, null, null,
        "LineGraph.gif", null, null,
        "Help.gif"};
    String[] tbToolTipText = {"New Analysis: load the default analysis file",
        "Open an existing analysis",
        "Restore analysis from last computation",
        "Save current analysis",
        "Save current analysis as...",
        "-", "-",
        "Edit the selected object of the visible list",
        "-",
        "Create and add a new object to the visible list",
        "Load an object from a (CIF) database or file...",
        "Save the selected object of the visible list into a database...", "-",
        "Remove the selected object from the visible list",
        "-", "-",
        "Open refine wizard panel",
        "Compute spectra",
        "Launch parameters refinement (I feel lucky!)",
        "-", "-",
        "Plot selected dataset",
        "-", "-",
        "General help for the MAUD program"
    };
    int[] firstIndexTBAction = {0, 0, 0, 0, 0, 0, 0,
        1, 0,
        1, 1, 1, 0, 1, 0, 0,
        2, 2, 2, 0, 0,
        3, 0, 0,
        5
    };
    int[] secondIndexTBAction = {0, 1, 3, 5, 6, 0, 0,
        5, 0,
        0, 1, 2, 0, 3, 0, 0,
        1, 3, 4, 0, 0,
        0, 0, 0,
        1
    };

    JToolBar toolBar = new JToolBar();
    toolBar.setFloatable(false);
    toolBar.add(new JToolBar.Separator(new Dimension(10, 10)));
    for (int i = 0; i < iconTB.length; i++) {
      if (iconTB[i] == null) {
        toolBar.add(new JToolBar.Separator(new Dimension(10, 10)));
      } else {
        JButton tb = new JIconButton(iconTB[i],
            "",
            mainMenuCommand[firstIndexTBAction[i]][secondIndexTBAction[i]],
            tbToolTipText[i]
        );
        toolBar.add(tb);
        tb.addActionListener(actionMenuListener);
//        tb.setEnabled(menuItemEnabled[firstIndexTBAction[i]][secondIndexTBAction[i]]);
      }
    }
    return toolBar;
  }

  void initMenuBar(DiffractionMenuAction actionMenuListener) {
    JMenuBar mb = new JMenuBar();
    setJMenuBar(mb);

    String[] menuString = {"File", "Edit", "Analysis", "Graphic", "Special", "Help"};
    boolean[] menuEnabled = {true, true, true, true, true, true};
    int[][] menuKeyEvent = {{KeyEvent.VK_N, KeyEvent.VK_O, nullKeyEvent, nullKeyEvent, nullKeyEvent,
        KeyEvent.VK_S, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
        nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, KeyEvent.VK_Q},
        {KeyEvent.VK_A, KeyEvent.VK_D, KeyEvent.VK_K, KeyEvent.VK_Z, nullKeyEvent,
            KeyEvent.VK_E, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
            nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent},
        {nullKeyEvent, KeyEvent.VK_W, KeyEvent.VK_L, KeyEvent.VK_M, KeyEvent.VK_R,
            nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
            nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent},
        {KeyEvent.VK_P, nullKeyEvent, nullKeyEvent, nullKeyEvent, KeyEvent.VK_T,
            nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
            nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent},
        {nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
            nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
            nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent},
        {nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
            nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
            nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent}};

    boolean[][] menuItemEnabled = {{true, true, true, true, true,
        true, true, true, true, true,
        true, true, true, true, true},
        {true, true, true, true, true,
            true, true, true, true, true,
            true, true, true, true, true},
        {true, true, true, true, true,
            true, true, true, true, true,
            true, true, true, true, true},
        {true, true, true, true, true,
            true, true, true, true, true,
            true, true, true, true, true},
        {true, true, Constants.esquienabled, true, true,
            true, true, true, true, true,
            true, true, true, true, true},
        {true, true, true, true, true,
            true, true, true, true, true,
            true, true, true, true, true}};

    JMenuItem amenuItem;
    JMenu amenu;
    int maxMenuIndex = menuString.length;
    int maxMenuItemIndex = mainMenuCommand[0].length;

    for (int menuIndex = 0; menuIndex < maxMenuIndex; menuIndex++) {

      amenu = mb.add(new JMenu(menuString[menuIndex]));
      amenu.setMnemonic(menuMnemonic[menuIndex]);
      for (int i = 0; i < maxMenuItemIndex; i++) {
        if (mainMenuCommand[menuIndex][i] == null)
          break;
        if (mainMenuCommand[menuIndex][i].equals("-")) {
          if (menuItemEnabled[menuIndex][i]) {
            amenu.add(new JSeparator());
          }
        } else {
          if (menuItemEnabled[menuIndex][i]) {
            amenuItem = amenu.add(new JMenuItem(mainMenuCommand[menuIndex][i]));
            if (menuKeyEvent[menuIndex][i] != nullKeyEvent)
              amenuItem.setAccelerator(KeyStroke.getKeyStroke(menuKeyEvent[menuIndex][i],
                  Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
            amenuItem.addActionListener(actionMenuListener);
            amenu.setEnabled(menuItemEnabled[menuIndex][i]);
          }
        }
      }
      amenu.setEnabled(menuEnabled[menuIndex]);
      if (menuIndex == 4) {         // Append network items to Special menu
        addNetworkComputingMenuItems(amenu, actionMenuListener);
        // Insert the interface menu
        addOptionsMenu(mb, actionMenuListener);
      }

    }

  }

  JToolBar initToolBar(DiffractionMenuAction actionMenuListener) {
    String[] iconTB = {"New.gif", "OpenDoc.gif", "Redo.gif", "Save.gif", "SaveAll.gif", null, null,
        "Eyeball.gif", null,
        "Box.gif", "DataExtract.gif", "DataStore.gif", null, "Delete.gif",
        null, null,
        "Bulb.gif", "Calculator.gif", Constants.refineIcon, null, null,
        "LineGraph.gif", null, null,
        "Help.gif"};
    String[] tbToolTipText = {"New Analysis: load the default analysis file",
        "Open an existing analysis",
        "Restore analysis from last computation",
        "Save current analysis",
        "Save current analysis as...",
        "-", "-",
        "Edit the selected object of the visible list",
        "-",
        "Create and add a new object to the visible list",
        "Load an object from a (CIF) database or file...",
        "Save the selected object of the visible list into a database...", "-",
        "Remove the selected object from the visible list",
        "-", "-",
        "Open refine wizard panel",
        "Compute spectra",
        "Launch parameters refinement (I feel lucky!)",
        "-", "-",
        "Plot selected dataset",
        "-", "-",
        "General help for the program"
    };
    int[] firstIndexTBAction = {0, 0, 0, 0, 0, 0, 0,
        1, 0,
        1, 1, 1, 0, 1, 0, 0,
        2, 2, 2, 0, 0,
        3, 0, 0,
        5
    };
    int[] secondIndexTBAction = {0, 1, 3, 5, 6, 0, 0,
        5, 0,
        0, 1, 2, 0, 3, 0, 0,
        1, 3, 4, 0, 0,
        0, 0, 0,
        1
    };

    JToolBar toolBar = new JToolBar();
    toolBar.setFloatable(false);
    toolBar.add(new JToolBar.Separator(new Dimension(10, 10)));
    for (int i = 0; i < iconTB.length; i++) {
      if (iconTB[i] == null) {
        toolBar.add(new JToolBar.Separator(new Dimension(10, 10)));
      } else {
        JButton tb = new JIconButton(iconTB[i],
            "",
            mainMenuCommand[firstIndexTBAction[i]][secondIndexTBAction[i]],
            tbToolTipText[i]
        );
        toolBar.add(tb);
        tb.addActionListener(actionMenuListener);
//        tb.setEnabled(menuItemEnabled[firstIndexTBAction[i]][secondIndexTBAction[i]]);
      }
    }
    return toolBar;
  }

  public void initMainFrame(boolean ownHandler, String filename) {
    initDone = true;
    ProgressPanel pcontrol = new ProgressPanel(18);

    StartingAboutD thebox = null;
    if (!simple) {
      thebox = new StartingAboutD(this, pcontrol);
      thebox.setVisible(true);
    }

    pcontrol.setProgressText("Initializing the main frame");
    pcontrol.increaseValue();

    initializeSizeAndPosition(
        true, "DiffractionMainFrame.frameWidth", "DiffractionMainFrame.frameHeight", 800, 600,
        true, "DiffractionMainFrame.framePositionX", "DiffractionMainFrame.framePositionY", 0, 20);

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout(Constants.borderInside, Constants.borderInside));

    boolean oldMenu = false;
    if (oldMenu) {
//  ---------------------- Menubar ----------------------
    pcontrol.setProgressText("Setting menubar");
    pcontrol.increaseValue();

      DiffractionMenuAction actionMenuListener = new DiffractionMenuAction();

      initMenuBar(actionMenuListener);

//  ---------------------- Toolbar ----------------------
    pcontrol.setProgressText("Building toolbar");
    pcontrol.increaseValue();

    JToolBar toolBar = initToolBar(actionMenuListener);
    c1.add(toolBar, BorderLayout.NORTH);
    } else {
//  ---------------------- Menubar ----------------------
      pcontrol.setProgressText("Setting menubar");
      pcontrol.increaseValue();

        DiffractionMenuAction actionMenuListener = new DiffractionMenuAction();

        initNewMenuBar(actionMenuListener);

//  ---------------------- Toolbar ----------------------
      pcontrol.setProgressText("Building toolbar");
      pcontrol.increaseValue();

      JToolBar toolBar = initNewToolBar(actionMenuListener);
      c1.add(toolBar, BorderLayout.NORTH);
    }

    titlePrefix = "Maud";
    setTitle(titlePrefix);

    pcontrol.setProgressText("Initializing Frame");
    pcontrol.increaseValue();

    westTB = new JTabbedPane();
    westTB.setMinimumSize(new Dimension(10, 10));

    String westTBString[] = {"Datasets", "Phases", "Sample"};
    // General TabPanel
    JPanel[] pTab = new JPanel[westTBString.length];
    for (int i = 0; i < westTBString.length; i++) {
      pTab[i] = new JPanel(new BorderLayout());
      westTB.addTab(westTBString[i], null, pTab[i]);
    }

    datasetsList = new JList();
    datasetsList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    datasetsList.addListSelectionListener(new ListSelectionListener() {
      public void valueChanged(ListSelectionEvent event) {
        updateDataFilePlot(false);
      }
    });
    JScrollPane sp = new JScrollPane();
//		sp.setBorder(new LineBorder(Color.black));
    sp.getViewport().add(datasetsList);
    pTab[0].add(BorderLayout.CENTER, sp);

    phaseList = new JList();
    phaseList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
//			phaseList.setVisibleRowCount(6);
    sp = new JScrollPane();
//		sp.setBorder(new LineBorder(Color.black));
    sp.getViewport().add(phaseList);
    pTab[1].add(BorderLayout.CENTER, sp);
    new FileDrop(phaseList, new FileDrop.Listener() {
      public void filesDropped(java.io.File[] files) {
        // handle file drop
        phaseDatabaseDropped(files);
      }   // end filesDropped
    }); // end FileDrop.Listener

    samplesList = new JList();
    samplesList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    sp = new JScrollPane();
//		sp.setBorder(new LineBorder(Color.black));
    sp.getViewport().add(samplesList);
    pTab[2].add(BorderLayout.CENTER, sp);

    outputPanel = new OutputPanel();
    outputPanel.setMinimumSize(new Dimension(10, 10));

    JTabbedPane plotPanel = new JTabbedPane();
    datafilePlotPanel = new SpectrumPlotPanel(true);
    setComponentToPrint(datafilePlotPanel.getComponentToPrint());
    plotPanel.setMinimumSize(new Dimension(10, 10));
    plotPanel.addTab("Plot", new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + "LineGraph.gif")),
        datafilePlotPanel);
    new FileDrop(datafilePlotPanel, new FileDrop.Listener() {
      public void filesDropped(java.io.File[] files) {
        // handle file drop
        datafilesDropped(files);
      }   // end filesDropped
    }); // end FileDrop.Listener

    if (MaudPreferences.getBoolean("interface.show2DPlotTabPanel", true)) {
      datafile2DPlotPanel = new MultiPlotFitting2DPanel(this);
      plotPanel.setMinimumSize(new Dimension(10, 10));
      plotPanel.addTab("Plot 2D", new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + "BarGraph.gif")),
          datafile2DPlotPanel);
      new FileDrop(datafile2DPlotPanel, new FileDrop.Listener() {
        public void filesDropped(java.io.File[] files) {
          // handle file drop
          datafilesDropped(files);
        }   // end filesDropped
      }); // end FileDrop.Listener
      plotPanel.addChangeListener(new ChangeListener() {
        public void stateChanged(ChangeEvent cEvent) {
          updateDataFilePlot(false);
        }
      });
    }

    if (MaudPreferences.getBoolean("interface.show2DResidualPlotTabPanel", true)) {
      residuals2DPlotPanel = new DifferencePlot2DPanel(this);
      plotPanel.setMinimumSize(new Dimension(10, 10));
      plotPanel.addTab("Residuals 2D", new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + "PieGraph.gif")),
          residuals2DPlotPanel);
      new FileDrop(residuals2DPlotPanel, new FileDrop.Listener() {
        public void filesDropped(java.io.File[] files) {
          // handle file drop
          datafilesDropped(files);
        }   // end filesDropped
      }); // end FileDrop.Listener
      plotPanel.addChangeListener(new ChangeListener() {
        public void stateChanged(ChangeEvent cEvent) {
          updateDataFilePlot(false);
        }
      });
    }

    parListPanel = new JPanel(new BorderLayout(Constants.borderInside, Constants.borderInside));
    parListPanel.setMinimumSize(new Dimension(10, 10));

    new FileDrop(parListPanel, new FileDrop.Listener() {
      public void filesDropped(java.io.File[] files) {
        // handle file drop
        parameterFileDropped(files);
      }   // end filesDropped
    }); // end FileDrop.Listener

    parListPaneSouth = MaudPreferences.getBoolean("parameterListPanel.fullBottom", true);

    if (parListPaneSouth) {
      int dividerE = WindowPreferences.getInteger(dividerEstring, 200);
      int dividerW = WindowPreferences.getInteger(dividerWstring, 460);
      int dividerP = WindowPreferences.getInteger(dividerPstring, 300);
      JPanel eastNPanel = new JPanel(new BorderLayout(Constants.borderInside, Constants.borderInside));
      eastNPanel.add(BorderLayout.CENTER, westTB);
      JPanel eastSPanel = new JPanel(new BorderLayout(Constants.borderInside, Constants.borderInside));
      eastSPanel.add(BorderLayout.CENTER, outputPanel);
      eastPanel = new JSplitPane(JSplitPane.VERTICAL_SPLIT, eastNPanel, eastSPanel);
      eastPanel.setContinuousLayout(true);
      eastPanel.setDividerLocation(dividerE);
      westPanel = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, eastPanel, plotPanel);
      westPanel.setContinuousLayout(true);
      westPanel.setDividerLocation(dividerP);
      principalPanel = new JSplitPane(JSplitPane.VERTICAL_SPLIT, westPanel, parListPanel);
      principalPanel.setContinuousLayout(true);
      principalPanel.setDividerLocation(dividerW);
      eastSPanel.setBorder(new EmptyBorder(0, 6, 0, 6));
    } else {
      JPanel eastNPanel = new JPanel(new BorderLayout(Constants.borderInside, Constants.borderInside));
      eastNPanel.add(BorderLayout.CENTER, westTB);
      JPanel eastSPanel = new JPanel(new BorderLayout(Constants.borderInside + 6, Constants.borderInside + 6));
      eastSPanel.add(BorderLayout.CENTER, outputPanel);
      eastPanel = new JSplitPane(JSplitPane.VERTICAL_SPLIT, eastNPanel, eastSPanel);
      eastPanel.setContinuousLayout(true);
      int dividerE = WindowPreferences.getInteger(dividerEstring, 200);
      eastPanel.setDividerLocation(dividerE);
      westPanel = new JSplitPane(JSplitPane.VERTICAL_SPLIT, plotPanel, parListPanel);
      westPanel.setContinuousLayout(true);
      int dividerW = WindowPreferences.getInteger(dividerWstring, 460);
      westPanel.setDividerLocation(dividerW);
      principalPanel = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, eastPanel, westPanel);
      principalPanel.setContinuousLayout(true);
      int dividerP = WindowPreferences.getInteger(dividerPstring, 300);
      principalPanel.setDividerLocation(dividerP);
      eastSPanel.setBorder(new EmptyBorder(0, 6, 0, 6));
    }
    principalPanel.setBorder(new EmptyBorder(0, 0, 0, 0));
    westPanel.setBorder(new EmptyBorder(0, 0, 0, 0));
    eastPanel.setBorder(new EmptyBorder(0, 0, 0, 0));

    c1.add(BorderLayout.CENTER, principalPanel);

    pcontrol.setProgressText("Opening default.par");
    pcontrol.increaseValue();

    newFile_Action(pcontrol, filename);

    pcontrol.setProgressText("Finished loading file");
    pcontrol.increaseValue();

    pcontrol.setProgressText("packing the window");
    pcontrol.increaseValue();

    treeTable = new JTreeTable(new ParameterTreeMutableModel(this, parameterfile));
    JScrollPane parScrollpane = new JScrollPane(treeTable);
    parListPanel.add(BorderLayout.CENTER, parScrollpane);
    expandAllRows();

    if (!simple)
      thebox.setVisible(false);

    validate();

    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    addWindowListener(new DiffractionMainFrame.WindowHandler());

    setResizable(true);
    setVisible(true);

    thebox = null;
    pcontrol = null;

    System.setProperty("apple.awt.brushMetalLook", "false");
    System.setProperty("apple.awt.brushMetalRounded", "false");
  }

  public OutputPanel getOutputPanel() {
    return outputPanel;  //To change body of created methods use File | Settings | File Templates.
  }

  void datafilesDropped(final File[] files) {
    (new PersistentThread() {
        public void executeJob() {
        try {
          sleep(500);
        } catch (InterruptedException ie) {
        }
        if (files != null && files.length > 0) {
          DataFileSet thedata = parameterfile.getActiveSample().getSelectedDataSet();
//          if (Misc.areYouSureToRemove("Do you want to remove the old datafiles from the dataset?"))
//            thedata.removeAllFiles();
          for (int i = 0; i < files.length; i++) {
            try {
              thedata.addDataFileforName(files[i].getCanonicalPath(), true);
            } catch (IOException e) {
              e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }
          }
	        thedata.refreshAll(false);
	        thedata.forceRangeCut();
          updateDataFilePlot(false);
        }
      }
    }).start();
  }

  void openDatafile_Action() {
    DataFileSet adata = parameterfile.getActiveSample().getSelectedDataSet();
    if (adata != null) {
      String[] filename = Utility.browseFilenames(this, "Load data file");
      if (filename != null) {
//        if (Misc.areYouSureToRemove("Do you want to remove the old datafiles from the dataset?"))
//          adata.removeAllFiles();
        setCursor(new Cursor(Cursor.WAIT_CURSOR));
        for (int i = 0; i < filename.length; i++)
          adata.addDataFileforName(filename[i], true);
        updateDataFilePlot(false);
        setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
      }
    }
  }

  void phaseDatabaseDropped(File[] files) {
    if (files != null && files.length > 0) {
      addPhaseFromDB(files);
    }
  }

  public void aboutHelp_Action() {
/*    if (Constants.ITS)
      (new AboutITSMaud(this, false)).setVisible(true);
    else*/
      (new AboutMAUD(this, "About MAUD Diffraction Screamer", false)).setVisible(true);
  }

  public void updateDataFilePlot(boolean keepMaxima) {
    if (parameterfile != null && parameterfile.getActiveSample() != null &&
        parameterfile.getActiveSample().getSelectedDataSet() != null) {
      DataFileSet adata = parameterfile.getActiveSample().getSelectedDataSet();
	    adata.updateDataForPlot();
      DiffrDataFile[] datafiles = adata.getActiveDataFiles();
      if (datafilePlotPanel.isVisible()) {
	      datafilePlotPanel.setNewData(adata, keepMaxima);
      }

      boolean isVisible = (datafile2DPlotPanel != null && datafile2DPlotPanel.isVisible());
      boolean isResVisible = (residuals2DPlotPanel != null && residuals2DPlotPanel.isVisible());
//      if (isVisible)
//        datafile2DPlotPanel.setVisible(false);
      if (isVisible)
        datafile2DPlotPanel.setNewData(datafiles, null, null);
      if (isResVisible)
        residuals2DPlotPanel.setNewData(datafiles, null, null);
//      if (isVisible)
//        datafile2DPlotPanel.setVisible(true);
      validate();
      if (isVisible)
        datafile2DPlotPanel.repaintPlot();
      if (isResVisible)
        residuals2DPlotPanel.repaintPlot();
    }
  }

  public void initParameters() {
    try {
    super.initParameters();
    if (parListFrame != null) {
      parListFrame.setVisible(false);
      parListFrame.dispose();
      parListFrame = null;
    }

    rebuildParameterTreeList(null, -1);
    refreshTheTree();
    updateDataFilePlot(false);
    outputPanel.reset();
    outputPanel.removeAllButtons();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

	public void refreshTheTree() {
		if (parameterfile != null) {
			if (parameterfile.getActiveSample() == null)
				parameterfile.newsample();
			parameterfile.getSamplesList().setList(samplesList);
			if (parameterfile.getActiveSample() != null) {
				if (parameterfile.getActiveSample().getPhasesList() != null)
					parameterfile.getActiveSample().getPhasesList().setList(phaseList);
				if (parameterfile.getActiveSample().getDatasetsList() != null)
					parameterfile.getActiveSample().getDatasetsList().setList(datasetsList);
			}
		}

	}

	public void dispose(myJFrame child) {
    if (child == parListFrame)
      parListFrame = null;
  }

  public void refreshParList(XRDcat source, int reason) {
//	  System.out.println("Refreshing tree list: " + (refreshTreeTable && !parameterfile.isComputingDerivate()));
    if (refreshTreeTable && !parameterfile.isComputingDerivate())
      rebuildParameterTreeList(source, reason);
  }

  public void expandAllRows() {
    int rownumbers = 0;
    int i = 0;
    rownumbers = treeTable.getTree().getRowCount();
    while (i <= rownumbers) {
      treeTable.getTree().expandRow(i++);
      rownumbers = treeTable.getTree().getRowCount();
    }
  }

  public void rebuildParameterTreeList(XRDcat source, int reason) {
//	  System.out.println(Constants.refreshTreePermitted + " " + source);
    if (Constants.refreshTreePermitted && parListPanel != null) {
      if (source == null) {
        parListPanel.removeAll();
        treeTable = new JTreeTable(new ParameterTreeMutableModel(this, parameterfile));
        JScrollPane parScrollpane = new JScrollPane(treeTable);
        parListPanel.add(BorderLayout.CENTER, parScrollpane);
        expandAllRows();
        validate();
      } else {
        if (treeTable != null) {
          TreeTableModelAdapter parameterListModelAdapter = (TreeTableModelAdapter) treeTable.getModel();
          parameterListModelAdapter.checkTree(source, reason);
        }
      }
    }
  }

  public SetEqualtoXRDcatD XRDcatDlg = null;
  public SetEqualtoD ParameterDlg = null;

  public void dispose() {
    if (XRDcatDlg != null) {
      XRDcatDlg.setVisible(false);
      XRDcatDlg.dispose();
      XRDcatDlg = null;
    }
    if (ParameterDlg != null) {
      ParameterDlg.setVisible(false);
      ParameterDlg.dispose();
      ParameterDlg = null;
    }
    super.dispose();
  }

  public void retrieveParameters() {
  }

  void refine_Action() {
    parameterfile.startingRefine();
    outputPanel.setProgressBar(10);
    outputPanel.removeAllButtons();
    parameterfile.launchrefine(outputPanel);
  }

  public void refineWizard(int wizardindex) {
    outputPanel.setProgressBar(10);
    outputPanel.removeAllButtons();
    parameterfile.refineWizard(outputPanel, wizardindex);
  }

  public void compute_Action() {
    outputPanel.removeAllButtons();
    parameterfile.compute(outputPanel);
  }

  void multiPlot_Action() {
    DataFileSet adata = parameterfile.getActiveSample().getSelectedDataSet();
    if (adata != null) {
      adata.multiPlot(this);
    }
  }

  void multiPlot2D_Action() {
    DataFileSet adata = parameterfile.getActiveSample().getSelectedDataSet();
    if (adata != null) {
      adata.multiPlot2D(this);
    }
  }

  void polarPlot2D_Action() {
    DataFileSet adata = parameterfile.getActiveSample().getSelectedDataSet();
    if (adata != null) {
      adata.polarPlot2D(this);
    }
  }

	void sectionPlot2D_Action() {
		DataFileSet adata = parameterfile.getActiveSample().getSelectedDataSet();
		if (adata != null) {
			adata.sectionPlot2D(this);
		}
	}

	void differencePlot2D_Action() {
    DataFileSet adata = parameterfile.getActiveSample().getSelectedDataSet();
    if (adata != null) {
      adata.differencePlot2D(this);
    }
  }

  TexturePlot texturePlotWindow = null;

  void texturePlot_Action() {
    if (texturePlotWindow != null) {
      texturePlotWindow.setVisible(false);
      texturePlotWindow.dispose();
    }
    texturePlotWindow = new TexturePlot(this);
    texturePlotWindow.setVisible(true);
  }

  public int getVisibleTabPanelIndex() {
    return westTB.getSelectedIndex();
  }

  void quitFile_Action() {
    if (parListPaneSouth) {
      WindowPreferences.setPref(dividerWstring, principalPanel.getDividerLocation());
      WindowPreferences.setPref(dividerEstring, eastPanel.getDividerLocation());
      WindowPreferences.setPref(dividerPstring, westPanel.getDividerLocation());
    } else {
      WindowPreferences.setPref(dividerPstring, principalPanel.getDividerLocation());
      WindowPreferences.setPref(dividerEstring, eastPanel.getDividerLocation());
      WindowPreferences.setPref(dividerWstring, westPanel.getDividerLocation());
    }
    super.quitFile_Action();
  }

  public void fireSelectionChanged(Object selected, Object sender) {
    if (sender instanceof FileSystemTreePanel) {
      File file = (File) selected;
    } else if (sender instanceof ParameterTreePanel) {
      Parameter par = (Parameter) selected;
    }
  }

  private ListVector getFocusedList() {
    int index = getVisibleTabPanelIndex();
    switch (index) {
      case 0: // datasets
        return parameterfile.getActiveSample().getDatasetsList();
      case 1: // phases
        return parameterfile.getActiveSample().getPhasesList();
      case 2: // samples
        return parameterfile.getSamplesList();
      default: {
        return null;
      }
    }
  }

  void newObject() {
    int index = getVisibleTabPanelIndex();
    switch (index) {
      case 0: // datasets
        DataFileSet adata = parameterfile.getActiveSample().newData(3);
	      adata.getInstrument();
        break;
      case 1: // phases
        parameterfile.getActiveSample().newPhase();
        break;
      case 2: // samples
/*        parameterfile.newObject(0);
	      parameterfile.getActiveSample().newData(3);
	      adata.getInstrument();*/
        break;
      default: {
      }
    }
  }

  void duplicateObject() {
    XRDcat aobject = null;
    int index = getVisibleTabPanelIndex();
    switch (index) {
      case 0: // datasets
        aobject = (XRDcat) parameterfile.getActiveSample().getDatasetsList().selectedElement();
        if (aobject == null) {
          WarningNothingSelected();
          return;
        }
        DataFileSet newData = (DataFileSet) aobject.getCopy(aobject.getParent());
        newData.setLabel(aobject.getLabel() + " copy");
        parameterfile.getActiveSample().addDataFileSet(newData);
        newData.refreshAll(true);
        break;
      case 1: // phases
        aobject = (XRDcat) parameterfile.getActiveSample().getPhasesList().selectedElement();
        if (aobject == null) {
          WarningNothingSelected();
          return;
        }
        Phase newPhase = (Phase) aobject.getCopy(aobject.getParent());
        newPhase.setLabel(aobject.getLabel() + " copy");
        parameterfile.getActiveSample().addPhase(newPhase);
        newPhase.refreshAll(true);
        break;
      case 2: // samples
        break;
      default: {
      }
    }

  }

  void addObjectFromDB() {
    int index = getVisibleTabPanelIndex();
    if (index == 2)
      return;
    String filename = Utility.openFileDialog(this, "Open CIF file or database", FileDialog.LOAD,
        MaudPreferences.getPref(databasePath, Constants.documentsDirectory),
        null, Constants.documentsDirectory + FilePar.database[1]);
    final String[] folderAndName = Misc.getFolderandName(filename);
    MaudPreferences.setPref(databasePath, folderAndName[0]);
    switch (index) {
      case 0: // datasets
        parameterfile.getActiveSample().loadDataSet(folderAndName[0] + folderAndName[1]);
        break;
      case 1: // phases
        parameterfile.getActiveSample().loadPhase(folderAndName[0] + folderAndName[1]);
        break;
      case 2: // samples
        break;
      default: {
      }
    }
  }

  void addPhaseFromDB(final File[] files) {
    // add object from CIF database.
    (new PersistentThread() {
        public void executeJob() {
        try {
          for (int i = 0; i < files.length; i++) {
            sleep(500);
            try {
              String filename = files[i].getCanonicalPath();
              if (filename != null) {
                final String[] folderAndName = Misc.getFolderandName(filename);
                parameterfile.getActiveSample().loadPhase(folderAndName[0] + folderAndName[1]);
              }
            } catch (IOException e) {
              e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }
          }
        } catch (InterruptedException ie) {
        }
      }
    }).start();
  }

  void storeObjectOnDB() {
    ListVector list = getFocusedList();
    if (list == null)
      return;

    int index = getVisibleTabPanelIndex();

    XRDcat aobject = (XRDcat) list.selectedElement();

    if (aobject == null) {
      WarningNothingSelected();
      return;
    }

    String filename = Utility.openFileDialog(this, "Select the CIF database", FileDialog.LOAD,
        MaudPreferences.getPref(databasePath, Constants.documentsDirectory),
        null, Constants.documentsDirectory + FilePar.database[index]);
    if (filename != null) {
      String[] folderAndName = Misc.getFolderandName(filename);
      MaudPreferences.setPref(databasePath, folderAndName[0]);
      aobject.storeOnDB(folderAndName[0] + folderAndName[1]);
    }

  }

  void editObject() {
    ListVector list = getFocusedList();
    if (list == null)
      return;

    XRDcat aobject = (XRDcat) list.selectedElement();

    if (aobject == null) {
      WarningNothingSelected();
      return;
    }
    aobject.edit(this);
  }

  void removeObject() {
    if (!Constants.confirmation || Utility.areYouSureToRemove("Remove the selected object?")) {
      int index = getVisibleTabPanelIndex();
      switch (index) {
        case 0: // datasets
          parameterfile.getActiveSample().removeData();
          break;
        case 1: // phases
          parameterfile.getActiveSample().removePhase();
          break;
        case 2: // samples
          parameterfile.removeObject(0);
          break;
        default: {
        }
      }
    }
    initParameters();
  }

  boolean result = false;

  myJFrame parListFrame = null;

  public void parameterList_Action() {

    initParameters();
    //    String mem = Misc.freeMemory();
//    if (Constants.testing)
//      System.out.println(mem);

    if (parListFrame != null) {
      parListFrame.setVisible(false);
      parListFrame.dispose();
      parListFrame = null;
    }
    parListFrame = new ParameterTreeTableFrame(this, parameterfile);

  }

  public void setValueAt(String newValue) {
    refreshTreeTable = false;
    if (treeTable.setValueAt(newValue)) {
      parameterfile.prepareComputation();
      parameterfile.mainfunction(false, false);
      updateDataFilePlot(true);
    }
    refreshTreeTable = true;
  }

  void refineWithXgrid() {
    // set XGrid if it is not
    setDistributeComputing(ParallelComputationController.XGRID);

    (new PersistentThread() {
        public void executeJob() {
        String[] filesBase64 = new String[2];
        filesBase64[0] = ParallelComputationController.maudEssentialBase64;
        filesBase64[1] = parameterfile.getSavedFileAsBase64String();
        String result = (new XGridClient()).submitJobAndWait("Maud_analysis",
            ParallelComputationController.xgridFilenames, filesBase64, ParallelComputationController.javaCommand,
            ParallelComputationController.javaArguments);
        if (!result.equals(Client.CANCELED)) {
        int totFreeParameters = parameterfile.computeParameterNumber();
        double[] parameters = new double[totFreeParameters];
        StringTokenizer st = new StringTokenizer(result, " {}=,;'\t\r\n");
        String token = st.nextToken();
        while (!token.equalsIgnoreCase("XGrid") && st.hasMoreTokens())
          token = st.nextToken();
        if (st.hasMoreTokens()) {
          token = st.nextToken(); // solution:
          token = st.nextToken();
        double Rwp = Double.parseDouble(token);
//      System.out.println(Rwp);
        int i = 0;
        while (st.hasMoreTokens()) {
          token = st.nextToken();
          parameters[i++] = Double.parseDouble(token);
//          System.out.println(token);
        }
        parameterfile.setFreeParameters(parameters);
        compute_Action();
        }
        } else {
          System.out.println("Job canceled by controller!");
        }
      }
    }).start();
  }

	void startBatchMode() {
		(new PersistentThread() {
			public void executeJob() {
				String filename = Utility.browseFilename(DiffractionMainFrame.this, "Load the batch instruction file");
				if (filename != null) {
					Constants.textonly = true;
//				Constants.stdoutput = Constants.CONSOLE_WINDOW;
					System.out.println("Starting Maud program, wait........");
					String[] args = new String[2];
					args[0] = "-f";
					args[1] = filename;
					(new MaudText()).execute(MaudText.BATCH, args);
					Constants.textonly = false;
				}
			}
		}).start();
	}

/*  void refineWithXgridUsingConsoleCommand() {
//        XGridClient.connect("trial", "/usr/bin/cal", new String[]{"2005"});
    String[] filenames = {"Maud_essential.jar", "analysis.par"};
    String folder = Misc.getUserDir() + "/xgrid/";
//        Misc.checkForFolderOrCreateIt(folder);
    parameterfile.saveparameters(folder, filenames[1]);
    int totFreeParameters = parameterfile.computeParameterNumber();
    String result = (new XGridClient()).submitBatchJob("Maud_analysis", filenames, folder);
    double[] parameters = new double[totFreeParameters];
    StringTokenizer st = new StringTokenizer(result, " {}=,;'\t\r\n");
    String token = st.nextToken();
    double Rwp = Double.parseDouble(token);
//    System.out.println(Rwp);
    int i = 0;
    while (st.hasMoreTokens()) {
      token = st.nextToken();
      parameters[i++] = Double.parseDouble(token);
    }
    parameterfile.setFreeParameters(parameters);
    compute_Action();
  }*/

/*  public void newParameterFile(StringReader areader, ProgressPanel pcontrol) {
    if (parameterfile != null) {
      parameterfile.dispose(); // If we don't call this no finalization will occur!!
      parameterfile = null;
    }

    // check file existing
    String[] folderAndName = new String[2];
        folderAndName[0] = Constants.filesfolder;
        folderAndName[1] = "noname.par";

    setCursor(new Cursor(Cursor.WAIT_CURSOR));
    try {
//				if (!maudServerIsActive)
      parameterfile = new FilePar(folderAndName[1], this);
//    		else
//    			parameterfile = createFileParOnServer(folderAndName[1]);

      parameterfile.setDirectory(folderAndName[0]);
      parameterfile.readall(areader, pcontrol);
      initParameters();
    } catch (Exception e) {
      e.printStackTrace();
    }
    setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
  }*/

  public void hippoWizard() {
    if (parameterfile != null) {
      parameterfile.dispose(); // If we don't call this no finalization will occur!!
      parameterfile = null;
    }

    // check file existing
    String[] folderAndName = new String[2];
        folderAndName[0] = Constants.documentsDirectory;
        folderAndName[1] = "noname.par";

    parameterfile = new FilePar(folderAndName[1], this);
    parameterfile.setDirectory(folderAndName[0]);

    HIPPOWizard.startWizard(parameterfile);

    initParameters();
  }

  class DiffractionMenuAction implements ActionListener {
    public void actionPerformed(ActionEvent event) {

      String command = event.getActionCommand();

      // File menu actions
      int index = 0;
      if (command.equals(mainMenuCommand[index][0])) {                 // New
        newWizard_Action();
        return;
      } else if (command.equals(MENU_NEW_ANALYSIS)) {
        newWizard_Action();
        return;
      } else if (command.equals(MENU_HIPPO_WIZARD)) {
        hippoWizard();
        return;
      } else if (command.equals(mainMenuCommand[index][1])) {         // Open...
        openFile_Action();
        return;
      } else if (command.equals(mainMenuCommand[index][2])) {         // Load datafile....
        openDatafile_Action();
        return;
      } else if (command.equals(mainMenuCommand[index][3])) {         // Restore
        restoreFile_Action();
        return;
      } else if (command.equals(mainMenuCommand[index][5])) {          // Save
        saveFile_Action();
        return;
      } else if (command.equals(mainMenuCommand[index][6])) {          // Save as...
        saveasFile_Action();
        return;
      } else if (command.equals(mainMenuCommand[index][8])) {          // Append simple results to...
        appendResults_Action(true);
        return;
      } else if (command.equals(mainMenuCommand[index][9])) {          // Append results to...
        appendResults_Action(false);
        return;
      } else if (command.equals(mainMenuCommand[index][11])) {          // Print plot...
//        DiffractionMainFrame.this.setComponentToPrint(datafilePlotPanel);
//        DiffractionMainFrame.this.letsTryToPrint();
        if (datafilePlotPanel.isVisible())
          datafilePlotPanel.letsTryToPrint();
        else {
          boolean isVisible = (datafile2DPlotPanel != null && datafile2DPlotPanel.isVisible());
          boolean isResVisible = (residuals2DPlotPanel != null && residuals2DPlotPanel.isVisible());
          if (isVisible)
            datafile2DPlotPanel.letsTryToPrint();
          else if (isResVisible)
            residuals2DPlotPanel.letsTryToPrint();
        }

        return;
      } else if (command.equals(mainMenuCommand[index][12])) {          // Printing all...
        DiffractionMainFrame.this.setComponentToPrint(DiffractionMainFrame.this.getContentPane());
        DiffractionMainFrame.this.letsTryToPrint();
        return;
      } else if (command.equals(mainMenuCommand[index][14])) {          // Quit
        myFrame_WindowClosing();
        return;
      }

// Edit menu actions
      index++;
      if (command.equals(mainMenuCommand[index][0])) {                 // Add new phase
        newObject();
        return;
      } else if (command.equals(mainMenuCommand[index][1])) {         // Load phase from CIF
        addObjectFromDB();
        return;
      } else if (command.equals(mainMenuCommand[index][2])) {         // Save phase to database
        storeObjectOnDB();
        return;
      } else if (command.equals(mainMenuCommand[index][3])) {          // Delete phase
        removeObject();
        return;
      } else if (command.equals(mainMenuCommand[index][5])) {         // Edit phase
        editObject();
        return;
      } else if (command.equals(mainMenuCommand[index][7])) {        // Duplicate phase
        duplicateObject();
        return;
      }

// Refinement menu actions
      index++;
      if (command.equals(mainMenuCommand[index][0])) {                 // Options
        refinementOptions_Action();
        return;
      } else if (command.equals(mainMenuCommand[index][1])) {         // Wizard
        refinementWizard_Action();
        return;
      } else if (command.equals(mainMenuCommand[index][2])) {         // Parameters list
        parameterList_Action();
        return;
      } else if (command.equals(mainMenuCommand[index][3])) {          // Compute spectra
        compute_Action();
        return;
      } else if (command.equals(mainMenuCommand[index][4])) {         // Refine
        refine_Action();
        return;
      } else if (command.equals(mainMenuCommand[index][5])) {         // Results
        results_Action();
        return;
      } else if (command.startsWith("Preference")) {                     // Preferences...
        Utility.showPrefs(DiffractionMainFrame.this);
        return;
      }

      // Graphic menu actions
      index++;
      if (command.equals(mainMenuCommand[index][0])) {                 // Plot selected dataset
        multiPlot_Action();
        return;
      } else if (command.equals(mainMenuCommand[index][1])) {         // MapPlot selected dataset
        multiPlot2D_Action();
        return;
      } else if (command.equals(mainMenuCommand[index][2])) {         // Difference MapPlot selected dataset
        differencePlot2D_Action();
        return;
      } else if (command.equals(mainMenuCommand[index][3])) {         // Polar Plot selected dataset
        polarPlot2D_Action();
        return;
      } else if (command.equals(mainMenuCommand[index][4])) {         // Section Plot selected dataset
	      sectionPlot2D_Action();
	      return;
      } else if (command.equals(mainMenuCommand[index][5])) {         // Texture Strain plot
        texturePlot_Action();
        return;
      } else if (command.equals(mainMenuCommand[index][7])) {          //  plot options...
        datafilePlotPanel.showOptionsDialog();
        return;
      } else if (command.equals(mainMenuCommand[index][9])) {          // fun
        MidiSynth.createSynthAndShow();
        return;
      }

      // Special menu actions
      index++;
      if (command.equals(mainMenuCommand[index][0])) {                 // Submit to COD
        CODsubmission();
        return;
      } else if (command.equals(mainMenuCommand[index][1])) {      // Load Maud RSS feed
        /*String url = MaudPreferences.getPref("MaudRSSfeed.url",
            "http://www.ing.unitn.it/~maud/feed/MaudRSSfeed.xml");
        (new RSSFeedLoader(url)).loadTheFeed();*/
        return;
      } else if (command.equals(mainMenuCommand[index][2])) {         // ESQUI client
        esquiClient_Action();
        return;
      } else if (command.equals(mainMenuCommand[index][3])) {      // Test COD JDBC connection
        (new CODdatabaseConnector()).testConnection("localhost/cod", "root", "cod");
        return;
      } else if (command.equals(mainMenuCommand[index][4])) {      // Test COD HTTP connection
        (new CODdatabaseConnector()).testConnection("localhost/cod", "root", "cod");
        return;
      } else if (command.equals(mainMenuCommand[index][5])) {     // batch mode
	      startBatchMode();
      } else if (command.equals(JPVMNetworkComputingCommand[0])) {     // Distribute computing configuration
        ParallelComputationController.configure();
        return;
      } else if (command.equals(JPVMNetworkComputingCommand[1])) {     // Distribute computing agent
        ParallelComputationController.runAsAgent();
        return;
      } else if (command.equals(JPVMNetworkComputingCommand[2])) {     // show pvm console
        ParallelComputationController.viewConsole();
        return;
      } else if (command.equals(XGRIDNetworkComputingCommand[0])) {     // Distribute computing agent
        ParallelComputationController.runAsAgent();
        return;
      }

      // Help menu commands, show the help Window loading the appropriate help file
      index++;
      for (int i = 0; i < helpFilenames.length; i++) {
        if (command.equals(mainMenuCommand[index][i])) {
          showHelp(helpFilenames[i]);
          return;
        }
      }
      if (command.equals(mainMenuCommand[index][helpFilenames.length + 1])) {   // About Maud...
        aboutHelp_Action();
        return;
      }

      // Button menu actions
/*
      if (command.equals(buttonMenuCommand[0])) { 							// Add object from database
        newObject();
        return;
      } else if (command.equals(buttonMenuCommand[1])) { 				// New object
        addObjectFromDB();
        return;
      } else if (command.equals(buttonMenuCommand[2])) { 				// Edit object
        editObject(2);
        return;
      } else if (command.equals(buttonMenuCommand[2])) { 				// Remove object
        storeObjectOnDB();
        return;
      } else if (command.equals(buttonMenuCommand[3])) { 				// Store object on database
        removeObject();
        return;
      } else if (command.equals(buttonMenuCommand[5])) { 				// Duplicate object
        duplicateObject();
        return;
      }*/

    }

  }

}
