/*
 * @(#)mainFrame.java created 01/01/1997 Mesiano
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

//import it.unitn.ing.rista.comp.OutputFrame;
import it.unitn.ing.rista.diffr.FilePar;
import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.IOException;

/**
 * The mainFrame is a the old principal frame of Maud.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.18 $, $Date: 2006/12/04 14:30:03 $
 * @since JDK1.1
 */

public class mainFrame extends principalJFrame {

//	Applet theapplet = null;
//  OutputFrame outputframe;
  myJFrame parListFrame = null;

  JTextField title;
  JTextField operatorname;
  JList[] objectslist;
  JTabbedPane tabPanel;
  JRemoveButton[] removeB;

  // menu commands Strings

  static String[][] mainMenuCommand = {{"New", // File
                                        "Open...",
                                        "Restore",
                                        "-",
                                        "Save",
                                        "Save as...",
                                        "-",
                                        "Append simple results to...",
                                        "Append results to...",
                                        "-",
                                        "Quit",
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
                                        "Texture plot",
                                        "-",
                                        "Waiting for computation...",
                                        null,
                                        null,
                                        null,
                                        null,
                                        null,
                                        null,
                                        null,
                                        null},
                                       {"Submit structure to COD",
                                        "ESQUI client", // Special
                                        null,
                                        null,
                                        null,
                                        null,
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
                                        "About Maud...",
                                        null,
                                        null,
                                        null,
                                        null,
                                        null,
                                        null,
                                        null,
                                        null}
  };

  static int nullKeyEvent = -999;

  static String[] helpFilenames = {"readme.txt",
                                   "intro.txt",
                                   "tutorial.txt",
                                   "content.txt",
                                   "license_maud.txt"};

  static String[] buttonMenuCommand = {"Add from database", "Add new", "Edit", "Remove", "Save on database",
                                       "Duplicate"};

  public mainFrame() {
    super();
  }

  public void initMainFrame(boolean ownHandler, String filename) {
    initDone = true;
    initializeSizeAndPosition(false, "mainFrame.frameWidth", "mainFrame.frameHeight", 400, 500,
        true, "mainFrame.framePositionX", "mainFrame.framePositionY", 0, 20);

    ProgressPanel pcontrol = new ProgressPanel(18);

    StartingAboutD thebox = new StartingAboutD(this, pcontrol);
    thebox.setVisible(true);

    pcontrol.setProgressText("Initializing the main frame");
    pcontrol.increaseValue();

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout(6, 6));

    JMenuBar mb = new JMenuBar();
    setJMenuBar(mb);

    MenuAction actionMenuListener = new MenuAction();

//  ---------------------- Menubar ----------------------
    pcontrol.setProgressText("Setting menubar");
    pcontrol.increaseValue();

    // menu Strings

    String[] menuString = {"File", "Refinement", "Graphic", "Special", "Help"};
    char[] menuMnemonic = {'f', 'r', 'g', 's', 'h'};
    boolean[] menuEnabled = {true, true, true, true, true};
    int[][] menuKeyEvent = {{KeyEvent.VK_N, KeyEvent.VK_O, nullKeyEvent, nullKeyEvent, KeyEvent.VK_S,
                             nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
                             KeyEvent.VK_Q, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent},
                            {nullKeyEvent, KeyEvent.VK_W, KeyEvent.VK_L, KeyEvent.VK_M, KeyEvent.VK_R,
                             nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
                             nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent},
                            {KeyEvent.VK_P, nullKeyEvent, nullKeyEvent, KeyEvent.VK_T, KeyEvent.VK_E,
                             nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
                             nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent},
                            {nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
                             nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
                             nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent},
                            {nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
                             nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
                             nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent}};

    boolean[][] menuItemEnabled = {{true, true, true, true, true,
                                    true, true, true, true, ownHandler,
                                    ownHandler, true, true, true, true},
                                   {true, true, true, true, true,
                                    true, ownHandler, ownHandler, true, true,
                                    true, true, true, true, true},
                                   {true, true, true, true, true,
                                    true, true, true, true, true,
                                    true, true, true, true, true},
                                   {true, Constants.esquienabled, true, true, true,
                                    true, true, true, true, true,
                                    true, true, true, true, true},
                                   {true, true, true, true, true,
                                    ownHandler, ownHandler, true, true, true,
                                    true, true, true, true, true}};

    JMenuItem amenuItem;
    JMenu amenu;
    int maxMenuIndex = menuString.length;
    int maxMenuItemIndex = mainMenuCommand[0].length;

    for (int menuIndex = 0; menuIndex < maxMenuIndex; menuIndex++) {

      if (menuIndex == 1) { 				// Insert the edit menu
        mb.add(createEditMenu());
      } else if (menuIndex == 4) {	// Insert the interface menu
        addOptionsMenu(mb, actionMenuListener);
      }

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
      if (menuIndex == 3) 				// Append network items to Special menu
        addNetworkComputingMenuItems(amenu, actionMenuListener);
    }

//  ---------------------- Toolbar ----------------------
    pcontrol.setProgressText("Building toolbar");
    pcontrol.increaseValue();

    String[] iconTB = {"New.gif", "OpenDoc.gif", "Redo.gif", "Save.gif", "SaveAll.gif",
                       "Bulb.gif", "Calculator.gif", "slot_machine_20.gif",
                       "LineGraph.gif",
                       "Help.gif"};
    String[] tbToolTipText = {"New Analysis: load the default analysis file",
                              "Open an existing analysis",
                              "Restore analysis from last computation",
                              "Save current analysis",
                              "Save current analysis as...",
                              "Open refine wizard panel",
                              "Compute spectra",
                              "Launch parameters refinement (I feel lucky!)",
                              "Plot selected dataset",
                              "General help for the MAUD program"
    };
    int[] firstIndexTBAction = {0, 0, 0, 0, 0,
                                1, 1, 1,
                                2,
                                4
    };
    int[] secondIndexTBAction = {0, 1, 2, 4, 5,
                                 1, 3, 4,
                                 0,
                                 1
    };

    JToolBar toolBar = new JToolBar();
    c1.add(toolBar, BorderLayout.NORTH);

    for (int i = 0; i < iconTB.length; i++) {
      JButton tb = new JIconButton(iconTB[i],
          "",
          mainMenuCommand[firstIndexTBAction[i]][secondIndexTBAction[i]],
          tbToolTipText[i]);
      toolBar.add(tb);
      tb.addActionListener(actionMenuListener);
      tb.setEnabled(menuItemEnabled[firstIndexTBAction[i]][secondIndexTBAction[i]]);
    }

//  ---------------------- TabPanel ----------------------
    pcontrol.setProgressText("Initializing tab panel");
    pcontrol.increaseValue();

// Principal panel

    JPanel jPanel5 = new JPanel();
    jPanel5.setLayout(new BorderLayout());
    c1.add(jPanel5, BorderLayout.SOUTH);
    new FileDrop(jPanel5, new FileDrop.Listener() {
      public void filesDropped(java.io.File[] files) {
        // handle file drop
        parameterFileDropped(files);
      }   // end filesDropped
    }); // end FileDrop.Listener


// Set analysis title and operator texfields

    JPanel jPanel7 = new JPanel();
    jPanel7.setLayout(new GridLayout(0, 1, 6, 6));
    jPanel5.add(BorderLayout.WEST, jPanel7);
    jPanel7.add(new JLabel("Analysis title:"));
    jPanel7.add(new JLabel("Operator:"));

    jPanel7 = new JPanel();
    jPanel7.setLayout(new GridLayout(0, 1, 6, 6));
    jPanel5.add(BorderLayout.CENTER, jPanel7);
    title = new JTextField(24);
    title.setText("Untitled");
    jPanel7.add(title);
    operatorname = new JTextField(24);
    operatorname.setText("Unknown");
    jPanel7.add(operatorname);

    JPanel tabAndButtonContainer = new JPanel(new BorderLayout(6, 6));
    c1.add(tabAndButtonContainer, BorderLayout.CENTER);

// Buttons panel

    String[] buttonIcon = {"DataExtract.gif", "Box.gif", "Eyeball.gif", "Delete.gif", "DataStore.gif", "Copy.gif"};
    String[] buttonToolTipText = {"Load an object from a (CIF) database or file (for the visible list)...",
                                  "Create a new object in the visible list",
                                  "Open the edit panel for the selected object of the visible list",
                                  "Remove the selected object from the visible list",
                                  "Add the selected object of the visible list to a database...",
                                  "Duplicate the selected object"};

// TabPanel

    tabPanel = new JTabbedPane();
//		tabPanel.setPreferredSize(new Dimension(300, 100));
    String tempString[] = new String[FilePar.listString.length];
    for (int i = 0; i < FilePar.listString.length; i++)
      tempString[i] = FilePar.listString[i] + "s";
    tabAndButtonContainer.add(tabPanel, BorderLayout.CENTER);

    int numberLists = FilePar.listString.length;
    objectslist = new JList[numberLists];

    removeB = new JRemoveButton[numberLists];

    for (int i = 0; i < numberLists; i++) {
      pcontrol.setProgressText("Adding " + tempString[i] + " panel");
      pcontrol.increaseValue();

      JPanel panel1 = new JPanel();
      panel1.setLayout(new BorderLayout());
      tabPanel.addTab(tempString[i], null, panel1);
      objectslist[i] = new JList();
      objectslist[i].setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
//			objectslist[i].setVisibleRowCount(6);

      JScrollPane sp = new JScrollPane();
//		sp.setBorder(new LineBorder(Color.black));
      sp.getViewport().add(objectslist[i]);
      panel1.add(BorderLayout.CENTER, sp);
      new FileDrop(objectslist[i], new FileDrop.Listener() {
        public void filesDropped(java.io.File[] files) {
          // handle file drop
          databaseDropped(files);
        }   // end filesDropped
      }); // end FileDrop.Listener

      JPanel buttonPanel = new JPanel();
      buttonPanel.setLayout(new GridLayout(0, 1, 2, 2));

      for (int ij = 0; ij < buttonMenuCommand.length; ij++) {
        if (ij == 3) {
          removeB[i] = new JRemoveButton(buttonIcon[ij], buttonMenuCommand[ij], buttonMenuCommand[ij],
              buttonToolTipText[ij]);
          buttonPanel.add(removeB[i]);
          removeB[i].addActionListener(actionMenuListener);
        } else {
          JIconButton button = new JIconButton(buttonIcon[ij], buttonMenuCommand[ij], buttonMenuCommand[ij],
              buttonToolTipText[ij]);
          buttonPanel.add(button);
          button.addActionListener(actionMenuListener);
        }
      }

      panel1.add(BorderLayout.EAST, buttonPanel);
    }

// ------------------ finishing frame construction ------------------------------

    titlePrefix = "Maud";
    setTitle(titlePrefix);

    pcontrol.setProgressText("packing the window");
    pcontrol.increaseValue();

    pack();

    pcontrol.setProgressText("Opening default.par");
    pcontrol.increaseValue();

    newFile_Action(pcontrol, filename);

    pcontrol.setProgressText("Finished loading file");
    pcontrol.increaseValue();

    thebox.setVisible(false);

    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowHandler());

    setResizable(false);
    setVisible(true);

    thebox = null;
    pcontrol = null;

  }

  public void setTitleField(String atitle) {
    title.setText(atitle);
  }

  public String getTitleField() {
    return new String(title.getText());
  }

  public void setOperatorField(String aoperator) {
    operatorname.setText(aoperator);
  }

  public String getOperatorField() {
    return new String(operatorname.getText());
  }

/*  public JList getPhaselist() {
  	return objectslist[2];
  }

  public JList getInstrumentlist() {
  	return objectslist[0];
  }

  public JList getDatalist() {
  	return objectslist[1];
  }

  public JList getSamplelist() {
  	return objectslist[3];
  }*/

  public void aboutHelp_Action() {
    (new AboutMAUD(this, "About MAUD", false)).setVisible(true);
  }

  public void initParameters() {

    setOperatorField(parameterfile.getOperatorField());
    setTitleField(parameterfile.getTitleField());

    for (int i = 0; i < FilePar.listString.length; i++)
      parameterfile.getList(i).setList(objectslist[i]);

    if (parListFrame != null) {
      parListFrame.setVisible(false);
      parListFrame.dispose();
      parListFrame = null;
    }
/*    if (outputframe != null) {
      outputframe.setVisible(false);
      outputframe.dispose();
      outputframe = null;
    }*/
/*		if (MEMLTexture.entropyOutputFrame != null) {
			MEMLTexture.entropyOutputFrame.setVisible(false);
			MEMLTexture.entropyOutputFrame.dispose();
			MEMLTexture.entropyOutputFrame = null;
		}*/

    if (texturePlotWindow != null) {
      texturePlotWindow.setVisible(false);
      texturePlotWindow.dispose();
      texturePlotWindow = null;
    }
    validate();
  }

  public void retrieveParameters() {
    parameterfile.setOperatorField(getOperatorField());
    parameterfile.setTitleField(getTitleField());
  }

  void refine_Action() {
    parameterfile.startingRefine();
/*    if (outputframe != null) {
      outputframe.setVisible(false);
      outputframe.dispose();
      outputframe = null;
    }
    outputframe = new OutputFrame(this, 10);
    outputframe.initializeSizeAndPosition(true, "refineOutFrame.frameWidth", "refineOutFrame.frameHeight", 400, 500,
        true, "refineOutFrame.framePositionX", "refineOutFrame.framePositionY", 200, 100);*/
    parameterfile.launchrefine(null);
  }

  public void refineWizard(int wizardindex) {
/*    if (outputframe != null) {
      outputframe.setVisible(false);
      outputframe.dispose();
      outputframe = null;
    }
    outputframe = new OutputFrame(this, -1);
    outputframe.initializeSizeAndPosition(true, "refineOutFrame.frameWidth", "refineOutFrame.frameHeight", 400, 500,
        true, "refineOutFrame.framePositionX", "refineOutFrame.framePositionY", 200, 100);*/

    parameterfile.refineWizard(null, wizardindex);
  }

  public void parameterList_Action() {

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

  public void compute_Action() {
/*    if (outputframe != null) {
      outputframe.setVisible(false);
      outputframe.dispose();
      outputframe = null;
    }
    outputframe = new OutputFrame(this);
    outputframe.initializeSizeAndPosition(true, "refineOutFrame.frameWidth", "refineOutFrame.frameHeight", 400, 500,
        true, "refineOutFrame.framePositionX", "refineOutFrame.framePositionY", 200, 100);*/

    parameterfile.compute(null);
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
    return tabPanel.getSelectedIndex();
  }

  private void databaseDropped(File[] files) {
    int index = getVisibleTabPanelIndex();
    if (files != null && files.length > 0) {
      addObjectFromDB(files, index);
    }
  }

  void addObjectFromDB() {
    // add object from CIF database.
    int index = getVisibleTabPanelIndex();
    String filename = Utility.openFileDialog(this, "Open CIF file or database", FileDialog.LOAD,
        (String) MaudPreferences.getPref(databasePath, Constants.documentsDirectory),
        null, Constants.documentsDirectory + FilePar.database[index]);
    final String[] folderAndName = Misc.getFolderandName(filename);
    MaudPreferences.setPref(databasePath, folderAndName[0]);
    parameterfile.loadObject(index, folderAndName[0] + folderAndName[1]);
  }

  void addObjectFromDB(final File[] files, final int index) {
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
                parameterfile.loadObject(index, folderAndName[0] + folderAndName[1]);
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
    // add the selected object to a CIF database.
    int index = getVisibleTabPanelIndex();
    XRDcat aobject = (XRDcat) parameterfile.getList(index).selectedElement();
    ;
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
    int index = getVisibleTabPanelIndex();
    XRDcat aobject = (XRDcat) parameterfile.getList(index).selectedElement();

    if (aobject == null) {
      WarningNothingSelected();
      return;
    }
    aobject.edit(this);
  }

  void duplicateObject() {
    int index = getVisibleTabPanelIndex();
    XRDcat aobject = (XRDcat) parameterfile.getList(index).selectedElement();

    if (aobject == null) {
      WarningNothingSelected();
      return;
    }

    XRDcat newobject = aobject.getCopy(aobject.getParent());
    newobject.setLabel(aobject.getLabel() + " copy");
//    newobject.setParent(parameterfile);
    parameterfile.addObject(index, newobject);
    newobject.refreshAll(true);
//    parameterfile.notifyParentChanged();
  }

  void removeObject() {
    int index = getVisibleTabPanelIndex();
    if (!Constants.confirmation || Utility.areYouSureToRemove("Remove the selected object?"))
      parameterfile.removeObject(index);
  }

  void newObject() {
    int index = getVisibleTabPanelIndex();
    parameterfile.newObject(index);
  }

/*	public void setField(String label) {
		int index = getVisibleTabPanelIndex();
		parameterfile.getList(index).setLabelAt(label, objectslist[index].getSelectedIndex());
//		parameterfile.getPhasesList().setLabelAt(label, phaselist.getSelectedIndex());
	}

	public String getField() {
		int index = getVisibleTabPanelIndex();
		return phaselist.getSelectedValue().toXRDcatString();
	}

	public String getLabel() {
		return "Phase name:";
	}*/

/*	public void WarningAlreadyOpened() {
		(new AttentionD(this, "There is already one in editing! Close it before to edit another.")).setVisible(true);
	}*/

  public void dispose(myJFrame child) {
//    if (child == outputframe)
//      outputframe = null;
    if (child == parListFrame)
      parListFrame = null;
  }

  class MenuAction implements ActionListener {
    public void actionPerformed(ActionEvent event) {

      String command = event.getActionCommand();

      // File menu actions

      if (command.equals(mainMenuCommand[0][0])) { 								// New
        newWizard_Action();
        return;
      } else if (command.equals(mainMenuCommand[0][1])) { 				// Open...
        openFile_Action();
        return;
      } else if (command.equals(mainMenuCommand[0][2])) { 				// Restore
        restoreFile_Action();
        return;
      } else if (command.equals(mainMenuCommand[0][4])) {					// Save
        saveFile_Action();
        return;
      } else if (command.equals(mainMenuCommand[0][5])) {					// Save as...
        saveasFile_Action();
        return;
      } else if (command.equals(mainMenuCommand[0][7])) {					// Append simple results to...
        appendResults_Action(true);
        return;
      } else if (command.equals(mainMenuCommand[0][8])) {					// Append results to...
        appendResults_Action(false);
        return;
      } else if (command.equals(mainMenuCommand[0][10])) {					// Quit
        myFrame_WindowClosing();
        return;
      }

// Refinement menu actions

      if (command.equals(mainMenuCommand[1][0])) { 								// Options
        refinementOptions_Action();
        return;
      } else if (command.equals(mainMenuCommand[1][1])) { 				// Wizard
        refinementWizard_Action();
        return;
      } else if (command.equals(mainMenuCommand[1][2])) { 				// Parameters list
        parameterList_Action();
        return;
      } else if (command.equals(mainMenuCommand[1][3])) {					// Compute spectra
        compute_Action();
        return;
      } else if (command.equals(mainMenuCommand[1][4])) { 				// Refine
        refine_Action();
        return;
      } else if (command.equals(mainMenuCommand[1][5])) { 				// Results
        results_Action();
        return;
      } else if (command.startsWith("Preference")) { 										// Preferences...
        Utility.showPrefs(mainFrame.this);
        return;
      }


      // Graphic menu actions

      if (command.equals(mainMenuCommand[2][0])) { 								// Plot selected dataset
        multiPlot_Action();
        return;
      } else if (command.equals(mainMenuCommand[2][1])) { 				// MapPlot selected dataset
        multiPlot2D_Action();
        return;
      } else if (command.equals(mainMenuCommand[2][2])) { 				// Difference MapPlot selected dataset
        differencePlot2D_Action();
        return;
      } else if (command.equals(mainMenuCommand[2][4])) { 				// Texture Strain plot
        texturePlot_Action();
        return;
      } else if (command.equals(mainMenuCommand[2][6])) {					// fun
        MidiSynth.createSynthAndShow();
        return;
      }

      // Special menu actions

      if (command.equals(mainMenuCommand[3][0])) { 								// Submit to COD
        CODsubmission();
        return;
      } else if (command.equals(mainMenuCommand[3][1])) { 			  // ESQUI client
        esquiClient_Action();
        return;
      }/* else if (command.equals(networkComputingCommand[0])) { 		// Distribute computing
        enableNetworkComputing(cbJPVM.isSelected());
        return;
      } else if (command.equals(networkComputingCommand[1])) { 		// Local server
        enableLocalServerComputing(cbServer.isSelected());
        return;
      } else if (command.equals(networkComputingCommand[2])) { 		// show pvm console
        viewConsole();
        return;
      }*/

      // Help menu commands, show the help Window loading the appropriate help file

      for (int i = 0; i < helpFilenames.length; i++) {
        if (command.equals(mainMenuCommand[4][i])) {
          showHelp(helpFilenames[i]);
          return;
        }
      }
      if (command.equals(mainMenuCommand[4][helpFilenames.length + 1])) { 	// About Maud...
        aboutHelp_Action();
        return;
      }

      // Button menu actions

      if (command.equals(buttonMenuCommand[0])) { 							// Add object from database
        addObjectFromDB();
        return;
      } else if (command.equals(buttonMenuCommand[1])) { 				// New object
        newObject();
        return;
      } else if (command.equals(buttonMenuCommand[2])) { 				// Edit object
        editObject();
        return;
      } else if (command.equals(buttonMenuCommand[3])) { 				// Remove object
        removeObject();
        return;
      } else if (command.equals(buttonMenuCommand[4])) { 				// Store object on database
        storeObjectOnDB();
        return;
      } else if (command.equals(buttonMenuCommand[5])) { 				// Duplicate object
        duplicateObject();
        return;
      }

    }

  }

}


