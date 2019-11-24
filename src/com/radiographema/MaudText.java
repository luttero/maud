/*
 * @(#)MaudText.java created 25/10/1999 Berkeley
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

package com.radiographema;

import java.lang.*;

import it.unitn.ing.rista.util.*;
//import com.deadmoo.xgridagent.*;

/**
 * The MaudText is a text only launcher for Maud.
 *
 * @version $Revision: 1.4 $, $Date: 2006/07/20 13:39:02 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class MaudText {

  public static final int BATCH = 0;
  public static final int JPVM = 1;
  public static final int XGRID = 2;
//  public static final int XGRIDAGENT = 3;

  /**
   * Initialize program constants.
   */

  public void programInitialization() {
    System.out.println("Initializing constants");
	  System.out.flush();
    Constants.initConstants();
  }

  public void execute(int computingMode, String[] args) {
	  System.out.println("Starting in text mode....");
	  System.out.flush();
	  programInitialization();
    String insFileName = new String("textonly.ins");
    switch (computingMode) {
      case BATCH:
      for (int i = 0; i < args.length; i++)
        if (args[i].equalsIgnoreCase("-f") || args[i].equalsIgnoreCase("-file")
                || args[i].equalsIgnoreCase("f") || args[i].equalsIgnoreCase("file"))
          if (i + 1 < args.length)
            insFileName = args[++i];
      System.out.println("Starting batch mode");
      programInitialization();
      batchProcess batch = new batchProcess(insFileName);
      batch.process();
        break;
      case JPVM:
        System.out.println("Starting jpvm mode");
        jpvmProcess jbatch = new jpvmProcess(args);
        jbatch.process();
        break;
      case XGRID:
        System.out.println("Starting xgrid mode");
        XgridProcess xbatch = new XgridProcess(args);
        xbatch.process();
        break;
/*      case XGRIDAGENT:
        System.out.println("Starting xgridagent");
          XGridAgent.startAgentWithDefault();
          while (true) {
            try {
              Thread.sleep(1000000);
            } catch (InterruptedException e) {
            }
          }*/
      default: {
      }
    }
  }

  static public void main(String args[]) {
	  try {
		  System.out.println("Loading preferences........");
		  System.out.flush();
//    System.setProperty("apple.laf.useScreenMenuBar","true");
//    System.setProperty("apple.awt.use-file-dialog-packages","true");
//    System.setProperty("apple.awt.showGrowBox","true");

	  MaudPreferences.loadPreferences();
    Constants.textonly = true;
    Constants.stdoutput = Constants.CONSOLE_WINDOW;
    int mode = BATCH;
    for (int i = 0; i < args.length; i++) {
      if (args[i].equalsIgnoreCase("-jpvm")) {
        mode = JPVM; // jpvm parallel computing
      }
      if (args[i].equalsIgnoreCase("-xgrid")) {
        mode = XGRID; // xgrid parallel computing
        Constants.stdoutput = Constants.NO_OUTPUT; // no console output
      }
      if (args[i].equalsIgnoreCase("-silent")) {
        Constants.stdoutput = Constants.NO_OUTPUT; // no console output
      }
/*      if (args[i].equalsIgnoreCase("-xgridagent")) {
        mode = XGRIDAGENT; // xgrid agent mode
      }*/
    }
		  System.out.println("Starting Maud program, wait........");
		  System.out.flush();
    (new MaudText()).execute(mode, args);
	  } catch(Throwable e) {
		  System.err.println(e);
		  // returns null as the cause is nonexistent or unknown.
		  System.err.println("Cause = " + e.getCause());
	  }
    System.exit(0);
  }

}
