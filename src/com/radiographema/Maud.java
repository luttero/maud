/*
 * @(#)Maud.java created 15/10/2000 Casalino
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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

package com.radiographema;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

//import com.apple.eawt.*;

//import org.jdesktop.jdic.filetypes.*;
//import org.jdesktop.jdic.filetypes.Action;

import javax.swing.*;
import java.io.IOException;

/**
 * The Maud is a basic class to
 * startup the Maud program as an application.
 * It performs constant and interface (swing) initialization and launch
 * the principal frame mainFrame.
 *
 * @version $Revision: 1.10 $, $Date: 2006/07/20 13:39:02 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Maud {

  /**
   * Initialize program constants.
   */

  public static principalJFrame appMainFrame = null;

  static boolean programInitialized = false;
  static boolean swingInitialized = false;
  static boolean helpOwnHandler = true;

  public static void programInitialization() {
    if (programInitialized)
      return;
    programInitialized = true;
    Constants.initConstants();
  }

  /**
   * Initialize interface.
   * Force SwingSet to come up in the System Platform L&F.
   * If you want the Cross platform L&F instead, use instead
   * <cose>UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName())</code>
   */

  public static void preSwingInitialization() {
    if (swingInitialized)
      return;
    try {
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
    } catch (Exception exc1) {
    }

  }

  public static void postSwingInitialization() {
    if (swingInitialized)
      return;
    swingInitialized = true;
    try {
      UIManager.setLookAndFeel(MaudPreferences.getPref(principalJFrame.swingLF, UIManager.getSystemLookAndFeelClassName()));
    } catch (Exception exc) {
      try {
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
      } catch (Exception exc1) {
        System.out.println("Error loading Swing Look&Feel (chek java installation): " + exc1);
      }

    }
    JPopupMenu.setDefaultLightWeightPopupEnabled(false);

  }

  /**
   * Call the <code>swingInitialization()</code> and start the mainFrame.
   * By default Maud will run interactively starting the interface; if
   * the -t argument is used Maud will run in text only and will process
   * a default "textonly.ins" file where there should be the instructions
   * on what and how to run the analysis. The textonly.ins file is in CIF
   * format and must contain the analysis/es to run and the iterations number.
   *
   @see #preSwingInitialization
   @see it.unitn.ing.rista.awt.mainFrame
   */

  public static void main(String args[]) {

	  MaudPreferences.loadPreferences();
    Constants.textonly = false;
    boolean reflectivity = false;
    boolean maudette = false;  // now we go
    if (args != null && args.length > 0) {
      for (int i = 0; i < args.length; i++) {
        if (args[i].equalsIgnoreCase("-t") || args[i].equalsIgnoreCase("-textonly")
                || args[i].equalsIgnoreCase("t") || args[i].equalsIgnoreCase("textonly"))
          Constants.textonly = true;
        else if (args[i].equalsIgnoreCase("-film"))
          reflectivity = true;
        else if (args[i].equalsIgnoreCase("-simple"))
          maudette = true;
      }
    }
    if (Constants.textonly) {
      String insFileName = new String("textonly.ins");
      for (int i = 0; i < args.length; i++)
        if (args[i].equalsIgnoreCase("-f") || args[i].equalsIgnoreCase("-file")
                || args[i].equalsIgnoreCase("f") || args[i].equalsIgnoreCase("file"))
          if (i + 1 < args.length)
            insFileName = args[i + 1];

      System.out.println("Starting batch mode");
	    initInteractive();
//      programInitialization();
      batchProcess batch = new batchProcess(insFileName);
      batch.process();

	    try {
		    System.out.print("Press Enter key to exit: ");
		    System.in.read();
	    } catch (IOException e) {
		    e.printStackTrace();
	    }

      System.exit(0);

    } else if (reflectivity) {
      initInteractive();
      goReflectivityInteractive();
    } else if (maudette) {
      initInteractive();
      goDiffractionInteractive();
    } else {
      initInteractive();
      goInteractive();
    }
  }

  public static void initInteractive() {
    preSwingInitialization();
    programInitialization();
    postSwingInitialization();

  }

  public static void goReflectivityInteractive() {

    appMainFrame = new ReflectivityMainFrame();
    appMainFrame.initMainFrame(true, null, "Reflectivity Screamer");
    appMainFrame.setVisible(true);

  }

  public static void goDiffractionInteractive() {

    appMainFrame = new DiffractionMainFrame(true);

    switch (Constants.osType) {
      case Constants.OsWindoof:
        initWindows();
        break;
      case Constants.OsLinux:
        initLinux();
        break;
      case Constants.OsMac:
        initMac();
        break;
      case Constants.OsUnix:
      default: {}
    }

    if (!appMainFrame.initDone())
      appMainFrame.initMainFrame(true, null, "Diffraction Screamer");

    appMainFrame.setVisible(true);

  }

  public static void goInteractive() {
//	    System.out.println("Creating mainframe");

    appMainFrame = new DiffractionMainFrame(false);

    switch (Constants.osType) {
      case Constants.OsWindoof:
        initWindows();
        break;
      case Constants.OsLinux:
        initLinux();
        break;
      case Constants.OsMac:
        initMac();
        break;
      case Constants.OsUnix:
      default: {}
    }

    if (!appMainFrame.initDone())
      appMainFrame.initMainFrame(helpOwnHandler, null);

    appMainFrame.setVisible(true);
  }

  public static void initMac() {
    helpOwnHandler = false;
  }

  public static void initWindows() {
    helpOwnHandler = true;


/*    AssociationService assocService = new AssociationService();
    Association assoc = new Association();

    assoc.addFileExtension(".tst");
    assoc.setMimeType("testmime/submime");
    assoc.setDescription("tst file");
    assoc.setIconFileName("%SystemRoot%\\system32\\shell32.dll,-152");

    Action oneAction;
    oneAction = new Action("open", "C:\\WINNT\\system32\\notepad.exe %1", "open the file");
    assoc.addAction(oneAction);
    oneAction = new Action("edit", "C:\\WINNT\\system32\\notepad.exe %1", "edit the file");
    assoc.addAction(oneAction);
    oneAction = new Action("print", "C:\\WINNT\\system32\\notepad.exe /p %1", "print the file");
    assoc.addAction(oneAction);

    try {
        // Register an association in system level.
        assocService.registerSystemAssociation(assoc);
    } catch (AssociationAlreadyRegisteredException e) {
        System.out.println(e.getMessage());
    } catch (RegisterFailedException e) {
        System.out.println(e.getMessage());
    }



    Application app = Application.getApplication();
    if (!app.isAboutMenuItemPresent())
      app.addAboutMenuItem();
    if (!app.isPreferencesMenuItemPresent())
      app.addPreferencesMenuItem();
    app.setEnabledAboutMenu(true);
    app.setEnabledPreferencesMenu(true);
    // Install the application listener
    app.addApplicationListener(new ApplicationListener() {

      public void handleAbout(ApplicationEvent applicationEvent) {
        appMainFrame.aboutHelp_Action();
        applicationEvent.setHandled(true);
      }

      public void handleOpenApplication(ApplicationEvent applicationEvent) {
        //To change body of implemented methods use Options | File Templates.
      }

      public void handleOpenFile(ApplicationEvent applicationEvent) {
        if (!appMainFrame.initDone())
          appMainFrame.initMainFrame(false, applicationEvent.getFilename());
        else
          appMainFrame.openParameterFile(Misc.getFolderandName(applicationEvent.getFilename()), null);
        applicationEvent.setHandled(true);
      }

      public void handlePreferences(ApplicationEvent applicationEvent) {
        Utility.showPrefs(appMainFrame);
        applicationEvent.setHandled(true);
      }

      public void handlePrintFile(ApplicationEvent applicationEvent) {
        //To change body of implemented methods use Options | File Templates.
      }

      public void handleQuit(ApplicationEvent applicationEvent) {
        appMainFrame.myFrame_WindowClosing();
      }

      public void handleReOpenApplication(ApplicationEvent applicationEvent) {
        //To change body of implemented methods use Options | File Templates.
      }
    });*/

  }

  public static void initLinux() {
    helpOwnHandler = true;


/*    AssociationService assocService = new AssociationService();
    Association assoc = new Association();

    assoc.setName("javaws");
    assoc.addFileExtension(".tst");
    assoc.addFileExtension(".TST");
    assoc.setMimeType("testmime/submime");
    assoc.setDescription("tst file");
    assoc.setIconFileName("/usr/share/pixmaps/gnome-javaws.png");

    Action oneAction;
    // Be careful!!! Here neither "%1" not "%f" is required, and only "open" verb
    // is used for this API.
    oneAction = new Action("open", "/scratch/java/jre/j2re1.5.0/bin/javaws");
    assoc.addAction(oneAction);

    try {
        // Register an association in system level.
        // This requires root permission, would generate three files:
        // javaws.keys and javaws.mime under /usr/share/mime-info or /usr/share/gnome/mime-info and
        // javaws.applications under /usr/share/application-registry or /usr/share/gnome/application-registry.
        //assocService.registerSystemAssociation(assoc);

        // Register an association in user level.
        // This would generate three files: javaws.keys and javaws.mime under ~/.gnome/mime-info,
        // and javaws.applications under ~/.gnome/application-info.
        assocService.registerUserAssociation(assoc);

    } catch (AssociationAlreadyRegisteredException e) {
        System.out.println(e.getMessage());
    } catch (RegisterFailedException e) {
        System.out.println(e.getMessage());
    }


    Application app = Application.getApplication();
    if (!app.isAboutMenuItemPresent())
      app.addAboutMenuItem();
    if (!app.isPreferencesMenuItemPresent())
      app.addPreferencesMenuItem();
    app.setEnabledAboutMenu(true);
    app.setEnabledPreferencesMenu(true);
    // Install the application listener
    app.addApplicationListener(new ApplicationListener() {

      public void handleAbout(ApplicationEvent applicationEvent) {
        appMainFrame.aboutHelp_Action();
        applicationEvent.setHandled(true);
      }

      public void handleOpenApplication(ApplicationEvent applicationEvent) {
        //To change body of implemented methods use Options | File Templates.
      }

      public void handleOpenFile(ApplicationEvent applicationEvent) {
        if (!appMainFrame.initDone())
          appMainFrame.initMainFrame(false, applicationEvent.getFilename());
        else
          appMainFrame.openParameterFile(Misc.getFolderandName(applicationEvent.getFilename()), null);
        applicationEvent.setHandled(true);
      }

      public void handlePreferences(ApplicationEvent applicationEvent) {
        Utility.showPrefs(appMainFrame);
        applicationEvent.setHandled(true);
      }

      public void handlePrintFile(ApplicationEvent applicationEvent) {
        //To change body of implemented methods use Options | File Templates.
      }

      public void handleQuit(ApplicationEvent applicationEvent) {
        appMainFrame.myFrame_WindowClosing();
      }

      public void handleReOpenApplication(ApplicationEvent applicationEvent) {
        //To change body of implemented methods use Options | File Templates.
      }
    });*/

  }

}
