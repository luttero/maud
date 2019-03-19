/*
 * @(#)principalJFrame.java created 01/01/1997 Mesiano
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.util.Console;
import it.unitn.ing.rista.comp.*;
import it.unitn.ing.esqui.client.Client;
import it.unitn.ing.esqui.wizard.BaseWizard;
import com.deadmoo.xgridagent.XGridAgent;
import it.unitn.ing.xgridclient.XGridClient;

import javax.swing.*;
import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.net.URL;
import java.util.Vector;

import HTTPClient.*;

//import java.lang.*;

/**
 * The principalJFrame is a class
 *
 * @version $Revision: 1.14 $, $Date: 2006/11/10 09:32:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class principalJFrame extends myJFrame {

  public FilePar parameterfile;
  public static String titlePrefix = "Maud";
  public boolean initDone = false;
  JCheckBoxMenuItem cb;
	public static String datafilePath = "datafile.path";
	public static String databasePath = "database.path";
	public static String removeConfirm = "remove.confirm";
	public static String showTooltip = "tooltip.show";
	public static String plotScale = "plot.scale";
	public static String showProgressFrame = "shows.floatingProgressWindow";
	public static String swingLF = "swing.defaultL&F";
	public JTextField titleField = null;

  public principalJFrame() {
    super(null);
  }

  public principalJFrame(boolean createStatusBar) {
    super(null, createStatusBar);
  }

  public FilePar getFileParent() {
    return parameterfile;
  }

  public void refreshParList(XRDcat source, int reason) {

  }

  public void initMainFrame(boolean ownHandler, String filename) {}

  public void initMainFrame(boolean ownHandler, String filename, String title) {
    initMainFrame(ownHandler, filename);
    setTitle(title);
  }

  public void setTitle(String title) {
    if (!title.startsWith(titlePrefix))
      title = titlePrefix + " - " + title;
    super.setTitle(title);
  }

  public URL getCodeBase() {
    if (com.radiographema.MaudApplet.fromApplet || Constants.webStart)
      return Constants.ourCodebase;
    else
      return null;
  }

  public Container getRootParent() {
    if (com.radiographema.MaudApplet.fromApplet)
      return com.radiographema.MaudApplet.theApplet;
    else
      return this;
  }

  public void initParameters() {
    setTitle();
  }

  public void setTitle() {
    if (parameterfile != null)
      setTitle(parameterfile.getFileName());
  }

  public void retrieveParameters() {}

  public void restoreFile_Action() {
    if (!com.radiographema.MaudApplet.fromApplet || Constants.macos) {
      String folderAndName[] = new String[2];
      folderAndName[0] = Constants.documentsDirectory;
      folderAndName[1] = Constants.backupFile;
      openParameterFile(folderAndName, null);
    }
  }

  public void openFile_Action() {
    String filename = Utility.openFileDialog(this, "Open parameter file", FileDialog.LOAD,
            MaudPreferences.getPref(FilePar.analysisPath, "default.par"),
            null, "default.par");
    if (filename != null) {
      String[] folderAndName = Misc.getFolderandName(filename);
      openParameterFile(folderAndName, null);
    }
  }

  public void openParameterFile(String[] folderAndName, ProgressPanel pcontrol) {
    try {
    if (parameterfile != null) {
      parameterfile.dispose(); // If we don't call this no finalization will occur!!
      parameterfile = null;
    }

      BufferedReader in = null;
    // check file existing
    try {
      in = Misc.getReader(folderAndName[0], folderAndName[1]);
    } catch (Exception e) {
      e.printStackTrace();
    }

      if (in == null) {
        (new AttentionD("Parameter file not found, the default one will be loaded instead")).setVisible(true);
        folderAndName[0] = Constants.documentsDirectory;
        folderAndName[1] = "default.par";
      }
    setCursor(new Cursor(Cursor.WAIT_CURSOR));
    try {
//				if (!maudServerIsActive)
      parameterfile = new FilePar(folderAndName[1], this);
//    		else
//    			parameterfile = createFileParOnServer(folderAndName[1]);

      parameterfile.setDirectory(folderAndName[0]);
      loadParameters(folderAndName, pcontrol);
      initParameters();
      if (folderAndName[1].equalsIgnoreCase("default.par"))
	      parameterfile.createFirstDateRecord();
    } catch (Exception e) {
      e.printStackTrace();
    }
    setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  public void loadParameters(String[] folderAndName, ProgressPanel pcontrol) {
    java.io.Reader in = null;
    try {
      in = Misc.getReader(folderAndName[0], folderAndName[1]);
      parameterfile.readall(in, pcontrol);
	    parameterfile.setFileName(folderAndName[1], false);
	    parameterfile.setDirectory(folderAndName[0]);
	    if (titleField != null) {
		    titleField.setText(parameterfile.getTitleField());
	    }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  protected void parameterFileDropped(File[] files) {
    if (files != null && files.length > 0) {
      try {
        String filename = files[0].getCanonicalPath();
        NewFile(null, filename);
        initParameters();
//        invalidate();
      } catch (IOException e) {
        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
      }
    }
  }

  public void newFile_Action(ProgressPanel pcontrol, String filename) {
    NewFile(pcontrol, filename);
    initParameters();
  }

  void NewFile(ProgressPanel pcontrol, String filename) {
    try {
    String folderAndName[] = null;
    if (filename == null) {
      filename = "default.par";
      boolean loadLastAnalysisFile = MaudPreferences.getBoolean("analysis.loadLastAnalysisAtStart", false);
      if (pcontrol != null && loadLastAnalysisFile)
        filename = MaudPreferences.getPref(FilePar.analysisFile, filename);
      String dir = Constants.documentsDirectory;
      if (pcontrol != null && loadLastAnalysisFile)
        dir = MaudPreferences.getPref(FilePar.analysisPath, Constants.userHomeDirectory);
      folderAndName = new String[2];
      folderAndName[0] = dir;
      folderAndName[1] = filename;
    } else {
      folderAndName = Misc.getFolderandName(filename);
    }
    openParameterFile(folderAndName, pcontrol);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  void saveFile_Action() {
    saveFile(true);
  }

  void saveasFile_Action() {
    String filename = Utility.openFileDialog(this, "Save analysis file as", FileDialog.SAVE,
        parameterfile.getDirectory(),
        null, parameterfile.getFileName());
    if (filename != null) {
      String[] folderAndName = Misc.getFolderandName(filename);
	    parameterfile.resetIncrementRefinementNumber();
	    parameterfile.setFileName(folderAndName[1], false);
      parameterfile.setDirectory(folderAndName[0]);
      saveFile(false);
      setTitle();
    }
  }

  public void saveFile(boolean increment) {
    if (parameterfile == null)
      return;
    BufferedWriter out = Misc.getWriter(parameterfile.getDirectory(), parameterfile.getNameToSave(false));
    parameterfile.writeall(out);
	  MaudPreferences.setPref(FilePar.analysisPath, parameterfile.getDirectory());
	  MaudPreferences.setPref(FilePar.analysisFile, parameterfile.getNameToSave(increment));
  }

  void results_Action() {
    TextViewer resultWindow = new TextViewer(this);
    resultWindow.DisplayText(parameterfile.getLstNameToSave());
    resultWindow.setVisible(true);
  }

  void appendResults_Action(boolean simple) {
    String filename = Utility.browseFilenameForAppend("Append results to", MaudPreferences.getPref("results.appendFolder",
        "") + MaudPreferences.getPref("results.appendFilename", "results.dat"));

    /*  Misc.openFileDialog(this, "Append results to", FileDialog.SAVE,
          MaudPreferences.getPref("results.appendFolder",
              Constants.filesfolder),
          null, MaudPreferences.getPref("results.appendFilename",
              "results.dat"));*/
    if (filename != null) {
      String[] folderAndName = Misc.getFolderandName(filename);
      MaudPreferences.setPref("results.appendFolder", folderAndName[0]);
      MaudPreferences.setPref("results.appendFilename", folderAndName[1]);
      parameterfile.appendResultsTo(folderAndName[0], folderAndName[1], simple);
    }
  }

  public void esquiClient_Action() {
    new Client("ESQUI Client");
  }

	void newWizard_Action() {
		newFile_Action(null, null);
	}

  void newWizard_Action_disable() {
    String superClassName = "it.unitn.ing.esqui.wizard.BaseWizard";
    Vector actionClassesList = Constants.getClassList(superClassName);

    int numberOfWizards = actionClassesList.size() / 3;
    int choice = -1;

    BaseWizard[] aWizard = new BaseWizard[numberOfWizards];
    for (int i = 0; i < numberOfWizards; i++) {
      try {
        aWizard[i] = BaseWizard.factory(this, "", (String) actionClassesList.elementAt(i * 3));
      } catch (Exception pnf) {
      }
    }

    if (numberOfWizards > 1) {

      final JDialog wizardDialog = new JDialog(this, true);
      wizardDialog.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
      wizardDialog.setTitle("Choose the wizard action");
      JPanel listPanel = new JPanel();
      listPanel.setLayout(new DownFlowLayout());
      wizardDialog.getContentPane().add(listPanel);
      for (int i = 0; i < numberOfWizards; i++) {
        JPanel thePanel = new JPanel();
        thePanel.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
//        JPanel upPanel = new JPanel();
//        upPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 0, 0));
        JButton abutton = aWizard[i].getButton();
        thePanel.add(abutton);
//        thePanel.add(BorderLayout.CENTER, upPanel);
//        JPanel downPanel = new JPanel();
//        downPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 0, 0));
        thePanel.add(aWizard[i].getLabel());
//        thePanel.add(BorderLayout.SOUTH, downPanel);
        final BaseWizard wizard = aWizard[i];
        abutton.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent event) {
            wizardDialog.setVisible(false);
            wizardDialog.dispose();
            wizard.startAction();
          }
        });
        listPanel.add(thePanel);
      }

      wizardDialog.pack();
      wizardDialog.setVisible(true);
      while (wizardDialog.isVisible()) {
        try {
          Thread.sleep(100);
        } catch (InterruptedException ie) {
        }
      }

    } else
      choice = 0;

    if (aWizard != null && choice >= 0)
      aWizard[choice].startWizardAction();
  }

  public void refinementOptions_Action() {
    (new refinementOptionsD(this)).setVisible(true);
  }

  public void refinementWizard_Action() {
    (new refinementWizardD(this)).setVisible(true);
  }

  protected void CODsubmission() {
    // submit the selected object to the COD database.
      XRDcat aobject = (XRDcat) parameterfile.getActiveSample().getPhasesList().selectedElement();
      if (aobject == null || !(aobject instanceof Phase)) {
        WarningNothingSelected();
        return;
      }
    String filename = ((Phase) aobject).exportForCOD(this);

    TextViewer tv = new TextViewer(this, true, true);
    tv.setVisible(true);
    tv.DisplayText(filename);

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

  void differencePlot2D_Action() {
    DataFileSet adata = parameterfile.getActiveSample().getSelectedDataSet();
    if (adata != null) {
      adata.differencePlot2D(this);
    }
  }

  public void aboutHelp_Action() {}

  public boolean initDone() {
    return initDone;
  }

  public void myFrame_WindowClosing() {
    if (MaudPreferences.getBoolean("confirmationDialog.onExit", true)) {
    String title = "Warning, analysis not saved, are you sure to exit " + titlePrefix +
        "? (in 10 secs " + titlePrefix + " will quit anyway)";
    if (parameterfile == null)
      quitFile_Action();
    if (parameterfile.isSaved())
      title = "Are you sure to exit " + titlePrefix + "? (in 10 secs " + titlePrefix + " will quit anyway)";
    final ConfirmOnExitD cfd = new ConfirmOnExitD(title);
    cfd.setVisible(true);

    (new Thread() {
      public void run() {
        try {
          sleep(10000);
        } catch (InterruptedException ie) {
        }
        if (cfd.isVisible())
          quitFile_Action();  // force the quit while the dialog has not been handled
      }
    }).start();
    } else
      quitFile_Action();
  }

  void quitFile_Action() {
    if (parameterfile != null)
      parameterfile.closeLogResultFile();
    setVisible(false);         // hide the Frame
    Constants.close();
	  dispose();      // tell windowing system to free resources
    if (!com.radiographema.MaudApplet.fromApplet)
      System.exit(0);
  }

  public void dispose() {

    ParallelComputationController.disposeJPVM();

//		if (voyagerActive)
//			Voyager.shutdown();

    super.dispose();

  }

  public void refineWizard(int wizardindex) {
  }

  public void WarningNothingSelected() {
    (new AttentionD(this, "No item from the list selected!")).setVisible(true);
  }

  public JMenu addOptionsMenu(JMenuBar menuBar, ActionListener actionMenuListener) {
    // Options Menu
    JMenu options = menuBar.add(new JMenu("Interface"));
    options.setMnemonic('p');

    ButtonGroup group = new ButtonGroup();

    UIManager.LookAndFeelInfo[] LFs = UIManager.getInstalledLookAndFeels();
    int lfsnumber = LFs.length;

    JRadioButtonMenuItem lfsMenuItem[] = new JRadioButtonMenuItem[lfsnumber];

    for (int i = 0; i < lfsnumber; i++) {
      lfsMenuItem[i] = (JRadioButtonMenuItem)
          options.add(new JRadioButtonMenuItem(LFs[i].getName()));
      group.add(lfsMenuItem[i]);
//      if (Constants.testing)
//        System.out.println("Found look and feel: " + LFs[i].getName());
      lfsMenuItem[i].setSelected(UIManager.getLookAndFeel().getName().equals(LFs[i].getName()));
      lfsMenuItem[i].setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_1 + i,
          Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
      lfsMenuItem[i].addItemListener(new ItemListener() {
        public void itemStateChanged(ItemEvent e) {
          Component root = principalJFrame.this;
          root.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
          JRadioButtonMenuItem rb = (JRadioButtonMenuItem) e.getSource();
          String newLookAndFeel = null;
          try {
            if (rb.isSelected()) {
              UIManager.LookAndFeelInfo[] tmpLFs = UIManager.getInstalledLookAndFeels();
              int tmplfsnumber = tmpLFs.length;
              for (int j = 0; j < tmplfsnumber; j++) {
                if (rb.getText().equals(tmpLFs[j].getName())) {
                  UIManager.setLookAndFeel(tmpLFs[j].getClassName());
                  SwingUtilities.updateComponentTreeUI(root);
                  newLookAndFeel = tmpLFs[j].getClassName();
//			updateLookAndFeel();
                }
              }
            }
          } catch (Exception exc) {
            // Error - unsupported L&F
            rb.setEnabled(false);
            System.err.println("Unsupported LookAndFeel: " + rb.getText());
            newLookAndFeel = null;
          }
          if (newLookAndFeel != null)
            MaudPreferences.setPref(swingLF, newLookAndFeel);
          root.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }

      });
    }

    options.add(new JSeparator());

    cb = (JCheckBoxMenuItem) options.add(new JCheckBoxMenuItem("Show ToolTips"));
    boolean showt = MaudPreferences.getBoolean(showTooltip, true);
    cb.setSelected(showt);
    ToolTipManager.sharedInstance().setEnabled(showt);

    cb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (cb.isSelected()) {
          ToolTipManager.sharedInstance().setEnabled(true);
          MaudPreferences.setPref(showTooltip, true);
        } else {
          ToolTipManager.sharedInstance().setEnabled(false);
          MaudPreferences.setPref(showTooltip, false);
        }
      }
    });

    final JCheckBoxMenuItem cb1 = (JCheckBoxMenuItem) options.add(new JCheckBoxMenuItem("Confirm on remove"));
    Constants.confirmation = MaudPreferences.getBoolean(removeConfirm, Constants.confirmation);
    cb1.setSelected(Constants.confirmation);
    cb1.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (cb1.isSelected()) {
          Constants.confirmation = true;
          MaudPreferences.setPref(removeConfirm, Constants.confirmation);
        } else {
          Constants.confirmation = false;
          MaudPreferences.setPref(removeConfirm, Constants.confirmation);
        }
      }
    });

    final JCheckBoxMenuItem cb3 = (JCheckBoxMenuItem) options.add(new JCheckBoxMenuItem("Shows progress windows"));
    Constants.showProgressFrame = MaudPreferences.getBoolean(showProgressFrame, Constants.showProgressFrame);
    cb3.setSelected(Constants.showProgressFrame);
    cb3.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (cb3.isSelected()) {
          Constants.showProgressFrame = true;
          MaudPreferences.setPref(showProgressFrame, Constants.showProgressFrame);
        } else {
          Constants.showProgressFrame = false;
          MaudPreferences.setPref(showProgressFrame, Constants.showProgressFrame);
        }
      }
    });

    final JCheckBoxMenuItem cb2 = (JCheckBoxMenuItem) options.add(new JCheckBoxMenuItem("Console visible"));
    cb2.setSelected(Constants.consoleShouldBeVisible);
    Console.setSelectionMenuItem(cb2);
    cb2.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        Constants.setConsoleVisible(cb2.isSelected());
      }
    });
    return options;

  }

  String[] JPVMNetworkComputingCommand = {"Configure", "Run pvm agent", "Show pvm console"};
  String[] XGRIDNetworkComputingCommand = {"Run as agent"};
  static JCheckBoxMenuItem[] distributeComputingMethodsJCB = null;

  public static void setDistributeComputing(int index) {
    distributeComputingMethodsJCB[index].setSelected(true);
  }

  void addNetworkComputingMenuItems(JMenu amenu, final ActionListener listener) {
    if (Constants.testing) {
      amenu.add(new JSeparator());

      ButtonGroup group = new ButtonGroup();

      final JMenu distrCompMenu = new JMenu("Distribute computing");
      amenu.add(distrCompMenu);
      final JMenu distrNetwMenu = new JMenu("Distribute network");
      amenu.add(distrNetwMenu);

      int numberItems = ParallelComputationController.methodName.length;
      distributeComputingMethodsJCB = new JCheckBoxMenuItem[numberItems];
      for (int i = 0; i < numberItems; i++) {
        distributeComputingMethodsJCB[i] = new JCheckBoxMenuItem(ParallelComputationController.methodName[i]);
        distrCompMenu.add(distributeComputingMethodsJCB[i]);
        group.add(distributeComputingMethodsJCB[i]);
        if (i == 0)
          distributeComputingMethodsJCB[i].setSelected(true);
        final int index = i;
        distributeComputingMethodsJCB[i].addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            if (distributeComputingMethodsJCB[index].isSelected()) {
              setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
              ParallelComputationController.setActiveStructure(index);
              setCursor(Cursor.getDefaultCursor());
            }
            switch (index) {
              case ParallelComputationController.NONE:
                distrNetwMenu.setEnabled(false);
                break;
              case ParallelComputationController.XGRID:
                distrNetwMenu.setEnabled(true);
                distrNetwMenu.removeAll();
                final JMenu controllerMenu = new JMenu("Controller");
                distrNetwMenu.add(controllerMenu);
                ButtonGroup controllerGroup = new ButtonGroup();
                final String[] controllers = XGridClient.getControllersList();
                for (int i = 0; i < controllers.length; i++) {
                  JCheckBoxMenuItem controllerJCB = new JCheckBoxMenuItem(controllers[i]);
                  controllerMenu.add(controllerJCB);
                  controllerGroup.add(controllerJCB);
                  if (controllers[i].equalsIgnoreCase(XGridClient.host))
                    controllerJCB.setSelected(true);
                  final int cindex = i;
                  controllerJCB.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                      XGridAgent.setControllerHost(controllers[cindex]);
                      XGridClient.setControllerHost(controllers[cindex]);
                    }
                  });
                }

                JCheckBoxMenuItem retrieveDataJCB = new JCheckBoxMenuItem("Retrieve results immediately");
                distrNetwMenu.add(retrieveDataJCB);
                retrieveDataJCB.setSelected(ParallelComputationController.retrieveData);
                retrieveDataJCB.addActionListener(new ActionListener() {
                  public void actionPerformed(ActionEvent e) {
                    ParallelComputationController.retrieveData = !ParallelComputationController.retrieveData;
                  }
                });

                for (int i = 0; i < XGRIDNetworkComputingCommand.length; i++) {
                  JMenuItem networks1 = new JMenuItem(XGRIDNetworkComputingCommand[i]);
                  distrNetwMenu.add(networks1);
                  networks1.addActionListener(listener);
                }
                break;
              case ParallelComputationController.JPVM:
                distrNetwMenu.setEnabled(true);
                distrNetwMenu.removeAll();
                for (int i = 0; i < JPVMNetworkComputingCommand.length; i++) {
                  JMenuItem networks1 = new JMenuItem(JPVMNetworkComputingCommand[i]);
                  distrNetwMenu.add(networks1);
                  networks1.addActionListener(listener);
                }
                break;
              default: {}
            }
          }
        });

      }
    }
  }

  public void updateDataFilePlot(boolean keepMaxima) {
  }

  public OutputPanel getOutputPanel() {
    return null;  //To change body of created methods use File | Settings | File Templates.
  }

  class WindowHandler extends WindowAdapter {
    public void windowClosing(WindowEvent evt) {
      myFrame_WindowClosing();
    }
  }

  class ConfirmOnExitD extends myJFrame {

    public ConfirmOnExitD(String message) {
      super(principalJFrame.this, "Exiting warning");

      Container c1 = this.getContentPane();
      c1.setLayout(new BorderLayout());
      JLabel label1 = new JLabel(message);
      label1.setIcon(new ImageIcon(Misc.getResourceURL(Constants.imagesJar, Constants.iconfolder + "Caution.gif")));
      c1.add(BorderLayout.CENTER, label1);

      JPanel p4 = new JPanel();
      p4.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      c1.add(BorderLayout.SOUTH, p4);
      JButton exit = new JCancelButton();
      p4.add(exit);
      exit.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          ConfirmOnExitD.this.setVisible(false);
          ConfirmOnExitD.this.dispose();
        }
      });
      exit.setToolTipText("Press this to not exit from the program");

      exit = new JCloseButton();
      p4.add(exit);
      exit.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          ConfirmOnExitD.this.setVisible(false);
          ConfirmOnExitD.this.dispose();
          quitFile_Action();
        }
      });
      exit.setToolTipText("Press this to exit from the program");
      getRootPane().setDefaultButton(exit);

      this.pack();
      this.setResizable(false);
      this.getRootPane().setDefaultButton(exit);
      Utility.centerOnFrame(this, principalJFrame.this);
    }
  }

}
