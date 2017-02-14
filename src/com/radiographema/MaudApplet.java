/*
 * @(#)MaudApplet.java created 01/01/1997 Mesiano
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

package com.radiographema;

import java.awt.*;
import java.lang.*;
import java.applet.*;
import java.io.*;
import javax.swing.*;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.io.*;

/**
 * The MaudApplet is a basic extension of the swing JApplet class to
 * startup the Maud program both as an application and as an applet.
 * It performs constant and interface (swing) initialization and launch
 * the principal frame mainFrame.
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:52 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class MaudApplet extends Applet {

  /**
   * Call the <code>swingInitialization()</code> and create the starting applet
   * containing the <b>Go Maud</b> button. Pressing the button will call the
   * <code>GoMaud_Clicked()</code> method.
   @see #swingInitialization
   @see #GoMaud_Clicked
   */

  static principalJFrame appMainFrame = null;
  public static boolean fromApplet = false;
  public static Applet theApplet = null;

  public void init() {

    fromApplet = true;
    theApplet = this;

    Constants.ourCodebase = getCodeBase();

    Panel thepane = new Panel();
    thepane.setBackground(SystemColor.control);
    thepane.setLayout(new FlowLayout());
    thepane.add(new Label("Click here to start the program interface"));
    Button GoMaud = new Button("Start MAUD");
    thepane.add(GoMaud);
    add(thepane, BorderLayout.CENTER);

    GoMaud.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent event) {
        GoMaud_Clicked();
      }
    });
  }

  /**
   * Initialize program constants.
   */

  public static void programInitialization() {
//	  System.out.println("Intializing constants");
    Constants.initConstants();

  }

  /**
   * Initialize interface.
   * Force SwingSet to come up in the System Platform L&F.
   * If you want the Cross platform L&F instead, use instead
   * <cose>UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName())</code>
   */

  public static void swingInitialization() {
    try {

      UIManager.setLookAndFeel(MaudPreferences.getPref(principalJFrame.swingLF, UIManager.getSystemLookAndFeelClassName()));

    } catch (Exception exc) {
      System.out.println("Error loading L&F: " + exc);
      try {
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
      } catch (Exception exc1) {
        System.out.println("Error loading L&F: " + exc1);
      }
    }
    JPopupMenu.setDefaultLightWeightPopupEnabled(false);

  }

  /**
   * Start the mainFrame.
   @see it.unitn.ing.rista.awt.mainFrame
   */

  public void GoMaud_Clicked() {
    programInitialization();
//	    System.out.println("Swing initialization");
    swingInitialization();
//	    System.out.println("Creating mainframe");
    appMainFrame = new mainFrame();
    appMainFrame.initMainFrame(true, null);
    appMainFrame.setVisible(true);
  }

}
