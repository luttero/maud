/*
 * @(#)ReflectivityMainFrame.java created Jan 25, 2004 Casalino
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.comp.OutputPanel;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.File;


/**
 * The ReflectivityMainFrame is a class
 * 
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:53 $
 * @since JDK1.1
 */

public class ReflectivityMainFrame extends principalJFrame implements TreeEventReceiver {

//	Applet theapplet = null;
  FileSystemTreePanel fileSystemTree = null;
  ParameterTreePanel parameterTree = null;
  OutputPanel outputPanel;
  JPanel parListPanel = null;

  // menu commands Strings

  static String[][] mainMenuCommand = {{"New", // File
                                        "Open...",
                                        "Restore",
                                        "-",
                                        "Save",
                                        "Save as...",
                                        "-",
                                        "Quit",
                                        null,
                                        null,
                                        null,
                                        null,
                                        null,
                                        null,
                                        null},
                                       {"Compute", // Refinement
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
                                        null,
                                        null,
                                        null,
                                        null},
                                       {"Submit structure to COD", // Special
                                        "ESQUI client",
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
                                        "About FilmScreamer...",
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

  static String[] helpFilenames = {"readmeFilmScreamer.txt",
                                   "introFilmScreamer.txt",
                                   "tutorialFilmScreamer.txt",
                                   "contentFilmScreamer.txt",
                                   "license_maud.txt"};

  static String[] buttonMenuCommand = {"Load from CIF (DB)", "Add new", "Edit", "Remove", "Save on DB"};

  public ReflectivityMainFrame() {
    super();
  }

  public void initMainFrame(boolean ownHandler, String filename) {
    initDone = true;
    initializeSizeAndPosition(
            false, "ReflectivityMainFrame.frameWidth", "ReflectivityMainFrame.frameHeight", 800, 600,
            true, "ReflectivityMainFrame.framePositionX", "ReflectivityMainFrame.framePositionY", 0, 20);

    ProgressPanel pcontrol = new ProgressPanel(18);

    StartingAboutD thebox = new StartingAboutD(this, pcontrol);
    thebox.setVisible(true);

    pcontrol.setProgressText("Initializing the main frame");
    pcontrol.increaseValue();

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout(6, 6));

    JMenuBar mb = new JMenuBar();
    setJMenuBar(mb);

    ReflectivityMenuAction actionMenuListener = new ReflectivityMenuAction();

//  ---------------------- Menubar ----------------------
    pcontrol.setProgressText("Setting menubar");
    pcontrol.increaseValue();

    // menu Strings

    String[] menuString = {"File", "Refinement", "Special", "Help"};
    char[] menuMnemonic = {'f', 'r', 's', 'h'};
    boolean[] menuEnabled = {true, true, true, true};
    int[][] menuKeyEvent = {{KeyEvent.VK_N, KeyEvent.VK_O, nullKeyEvent, nullKeyEvent, KeyEvent.VK_S,
                             nullKeyEvent, nullKeyEvent, KeyEvent.VK_Q, nullKeyEvent, nullKeyEvent,
                             nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent},
                            {nullKeyEvent, KeyEvent.VK_W, KeyEvent.VK_L, KeyEvent.VK_M, KeyEvent.VK_R,
                             nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
                             nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent},
                            {nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
                             nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
                             nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent},
                            {nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
                             nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent,
                             nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent, nullKeyEvent}};

    boolean[][] menuItemEnabled = {{true, true, true, true, true,
                                    true, true, ownHandler, true, true,
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
    }

//  ---------------------- Frame ----------------------
    pcontrol.setProgressText("Initializing Frame");
    pcontrol.increaseValue();

    JPanel topPanel = new JPanel(new BorderLayout(6, 6));
    c1.add(BorderLayout.NORTH, topPanel);
    JPanel bottomPanel = new JPanel(new BorderLayout(6, 6));
    c1.add(BorderLayout.SOUTH, bottomPanel);

    fileSystemTree = new FileSystemTreePanel(this);
    topPanel.add(BorderLayout.WEST, fileSystemTree);
    parameterTree = new ParameterTreePanel(this);
    topPanel.add(BorderLayout.EAST, parameterTree);

    outputPanel = new OutputPanel();
    bottomPanel.add(BorderLayout.EAST, outputPanel);

// Principal panel

// ------------------ finishing frame construction ------------------------------

    titlePrefix = "FilmScreamer";
    setTitle("FilmScreamer");

    pcontrol.setProgressText("packing the window");
    pcontrol.increaseValue();

//    pack();
    setSize(1024, 768);

    pcontrol.setProgressText("Opening default.par");
    pcontrol.increaseValue();

    newFile_Action(pcontrol, filename);

    pcontrol.setProgressText("Finished loading file");
    pcontrol.increaseValue();

    thebox.setVisible(false);

    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    addWindowListener(new ReflectivityMainFrame.WindowHandler());

    setResizable(true);
    setVisible(true);

    thebox = null;
    pcontrol = null;

  }

  public void aboutHelp_Action() {
    (new AboutMAUD(this, "About FilmScreamer", false)).setVisible(true);
  }

  public void initParameters() {
    super.initParameters();
    parameterTree.setParameterFile(getFileParent());

/*    for (int i = 0; i < FilePar.listString.length; i++)
      parameterfile.getList(i).setList(objectslist[i]);

    if (parListFrame != null) {
      parListFrame.setVisible(false);
      parListFrame.dispose();
      parListFrame = null;
    }
    if (outputframe != null) {
      outputframe.setVisible(false);
      outputframe.dispose();
      outputframe = null;
    }*/

  }

  public void retrieveParameters() {
  }

  void refine_Action() {
    parameterfile.startingRefine();
    parameterfile.launchrefine(outputPanel);
  }

  public void refineWizard(int wizardindex) {
    parameterfile.refineWizard(outputPanel, wizardindex);
  }

  public void compute_Action() {
    parameterfile.compute(outputPanel);
  }

  public int getVisibleTabPanelIndex() {
    return 0; // only for Phases
  }

  void addObjectFromDB() {
    // add object from CIF database.
    int index = getVisibleTabPanelIndex();
    String filename = Utility.openFileDialog(this, "Open CIF file or database", FileDialog.LOAD,
            MaudPreferences.getPref(databasePath, Constants.documentsDirectory),
            null, Constants.documentsDirectory + FilePar.database[index]);
    if (filename != null) {
      String[] folderAndName = Misc.getFolderandName(filename);
      MaudPreferences.setPref(databasePath, folderAndName[0]);
      parameterfile.loadObject(index, folderAndName[0] + folderAndName[1]);
    }
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
    ;
    if (aobject == null) {
      WarningNothingSelected();
      return;
    }
    aobject.edit(this);
  }

  void removeObject() {
    int index = getVisibleTabPanelIndex();
//    if (!Constants.confirmation || removeB[index].areYouSureToRemove(this))
      parameterfile.removeObject(index);
  }

  void newObject() {
    int index = getVisibleTabPanelIndex();
    parameterfile.newObject(index);
  }

  public void fireSelectionChanged(Object selected, Object sender) {
    if (sender instanceof FileSystemTreePanel) {
      File file = (File) selected;
    } else if (sender instanceof ParameterTreePanel) {
      Parameter par = (Parameter) selected;
    }
  }

  class ReflectivityMenuAction implements ActionListener {
    public void actionPerformed(ActionEvent event) {

      String command = event.getActionCommand();

      // File menu actions

      if (command.equals(mainMenuCommand[0][0])) { 								// New
        newFile_Action(null, null);
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
/*      } else if (command.equals(mainMenuCommand[0][7])) {					// Append simple results to...
        appendResults_Action(true);
        return;
      } else if (command.equals(mainMenuCommand[0][8])) {					// Append results to...
        appendResults_Action(false);
        return;*/
      } else if (command.equals(mainMenuCommand[0][7])) {					// Quit
        myFrame_WindowClosing();
        return;
      }

// Refinement menu actions

/*      if (command.equals(mainMenuCommand[1][0])) { 								// Options
        refinementOptions_Action();
        return;
      } else if (command.equals(mainMenuCommand[1][1])) { 				// Wizard
        refinementWizard_Action();
        return;
      } else if (command.equals(mainMenuCommand[1][2])) { 				// Parameters list
        parameterList_Action();
        return;
      } else*/ if (command.equals(mainMenuCommand[1][0])) {					// Compute spectra
        compute_Action();
        return;
      } else if (command.equals(mainMenuCommand[1][1])) { 				// Refine
        refine_Action();
        return;
      } else if (command.equals(mainMenuCommand[1][2])) { 				// Results
        results_Action();
        return;
      } else if (command.startsWith("Preference")) { 										// Preferences...
        Utility.showPrefs(ReflectivityMainFrame.this);
        return;
      }


      // Graphic menu actions
/*
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
      }*/

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
      }

    }

  }

}
