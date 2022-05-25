/*
 * @(#)ParallelComputationController.java created 13/01/2003 Normandie Village, Berkeley
 *
 * Copyright (c) 2003 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.comp;

import it.unitn.ing.rista.jpvm.*;
import it.unitn.ing.rista.util.*;
import com.deadmoo.xgridagent.XGridAgent;
import it.unitn.ing.xgridclient.XGridClientMessage;
import it.unitn.ing.xgridclient.XGridClient;
import it.unitn.ing.xgridagent.XGridJavaLauncher;

import java.io.File;

import base64.Base64;

/**
 * The ParallelComputationController is a class to control and manage the parallel computation
 * using jpvm for now.
 *
 * @version $Revision: 1.6 $, $Date: 2006/12/04 14:30:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class ParallelComputationController {

  public static final int NONE = 0;
  public static final int XGRID = 1;
  public static final int JPVM = 2;
  public static final String[] methodName = {"None", "XGrid", "Jpvm"};
  public static String xgridFolder = null;
  public static String[] xgridFilenames = {"xgridlauncher.jar", "analysis.par"};
  public static String maudEssentialBase64 = null;
  public static String[] javaArguments = null;
  public static String javaCommand = "/usr/bin/java";

  public static int activeStructure = NONE;
  public static boolean retrieveData = true;

  public ParallelComputationController() {
  }

  public static void setActiveStructure(int index) {
    if (index == activeStructure)
      return;
    switch (activeStructure) {
      case XGRID:
        XGridAgent.stopAgent();
        break;
      case JPVM:
        disposeJPVM();
        stopJPVMDaemon();
        break;
      case NONE:
      default: {}
    }
    activeStructure = index;
    switch (activeStructure) {
      case XGRID:
        initXGrid();
        break;
      case JPVM:
        initJPVM();
        break;
      case NONE:
      default: {}
    }
  }

  static void initXGrid() {
    XGridClient.checkController();
    XGridAgent.hostname = XGridClient.hostname;
    XGridAgent.host = XGridClient.host;
    String httpAddress = MaudPreferences.getPref("HTTPserverForXGrid.address", "http://" + XGridClient.hostname[0] +
        "/xgrid/");
    XGridAgent.controllerHostList = XGridClient.controllerHostList;
    if (xgridFolder == null) {
      xgridFolder = Misc.getUserDir() + "/xgrid/";
      maudEssentialBase64 = XGridClientMessage.loadfile(xgridFolder + xgridFilenames[0]);
      String filenamemd5 = XGridJavaLauncher.getMD5sumForFile(new File(
          xgridFolder + XGridJavaLauncher.fileJar)) + ";" + httpAddress;
      filenamemd5 = Base64.encodeBytes(filenamemd5.getBytes());
      javaArguments = new String[]{"-mx512M", "-jar", xgridFilenames[0], "-xgrid", "-analysis",
          xgridFilenames[1], "-getbyurl", filenamemd5};
    }
  }

  static void initJPVM() {

  }

  public static void runAsAgent() {
    switch (activeStructure) {
      case XGRID:
        XGridAgent.startAgentWithDefault();
        break;
      case JPVM:
        startJPVMDaemon();
        break;
      case NONE:
      default: {
        System.out.println("Warning: cannot run agent as no valid distribute computing method has been selected!");
      }
    }
  }

  public static void configure() {
    switch (activeStructure) {
      case XGRID:
//        XGridAgent.checkController();
        break;
      case JPVM:
        viewConsole();
        break;
      case NONE:
      default: {
      }
    }
  }

// JPVM stuff

  static jpvmTaskId myTaskId = null;
  static jpvmEnvironment jpvm = null;
  static jpvmTaskId tids[];
  public static String jpvmAnalysisFile = "jpvmAnalysis.par";
  public static String jpvmAnalysisFolder = "jpvmTmp/";
  public static String jpvmDefaultDataServer = "http://127.0.0.1/";
  static jpvmControlFrame consoleWindow = null;

  public static void disposeJPVM() {
    if (jpvm != null)
      try {
        for (int i = 0; i < jpvmDaemon.numTasks; i++) {
          jpvmBuffer buf = new jpvmBuffer();
          buf.pack(-1);
          jpvm.pvm_send(buf, tids[i], jpvmDaemon.sendPVTag);
        }
        jpvm.pvm_exit();
        jpvm = null;
      } catch (jpvmException jpe) {
        System.out.println("jpvm Exception - " + jpe.toString());
      }
  }

  public static void startJPVMDaemon() {
    (new PersistentThread() {
      public void executeJob() {
        jpvmDaemon.startDaemon();
      }
    }).start();
  }

  public static void stopJPVMDaemon() {
    jpvmDaemon.stopDaemon();
  }

  void enableJPVMNetworkComputing(boolean enable) {
    if (!enable) {
      disposeJPVM();
    }// else
     // pvmConsoleMenu.setEnabled(Constants.useJpvm);
  }

  public static void viewConsole() {
    if (consoleWindow == null)
      consoleWindow = new jpvmControlFrame();
    consoleWindow.setVisible(true);
  }

/*		if (Constants.useJpvm) {

			try {
				int ntasks = jpvmDaemon.numTasks;
				if (numberofpeaks < ntasks)
					ntasks = numberofpeaks;
				if (jpvm == null) {
					jpvm = new jpvmEnvironment();
					myTaskId = jpvm.pvm_mytid();
					tids = new jpvmTaskId[jpvmDaemon.numTasks];
					if (jpvmDaemon.useLocalTask) {
						if(jpvmDaemon.numTasks>1)
			  			jpvm.pvm_spawn("it.unitn.ing.rista.util.PseudoVoigt",jpvmDaemon.numTasks-1,tids);
						tids[jpvmDaemon.numTasks-1] = tids[0];
						tids[0] = myTaskId;
					} else {
			  		jpvm.pvm_spawn("it.unitn.ing.rista.util.PseudoVoigt",jpvmDaemon.numTasks,tids);
					}
				}
				for (int i = 0; i < ntasks; i++) {
					// sending a new work
					........
			    jpvmBuffer buf = new jpvmBuffer();
			    buf.pack(i);
			    buf.pack(intensity);
					........
			    buf.pack(rootdim);
			    buf.pack(xcoor,rootdim,1);
			    jpvm.pvm_send(buf,tids[i],jpvmDaemon.sendPVTag);
				}
				int tasksSent = ntasks;
				int receivedTasks = 0;
				while (tasksSent < numberofpeaks) {
					......
					intensity = peaklist[tasksSent].getScaleFactor();
					............
					// receiving a work done
			    jpvmMessage msg = jpvm.pvm_recv(jpvmDaemon.recvPVTag);
			    receivedTasks++;
			    // send a new work on the available task
			    jpvmBuffer buf = new jpvmBuffer();
			    buf.pack(tasksSent);
			    buf.pack(intensity);
					...........
			    buf.pack(rootdim);
			    buf.pack(xcoor, rootdim, 1);
			    jpvm.pvm_send(buf,msg.sourceTid,jpvmDaemon.sendPVTag);
			    tasksSent++;
			    // process the received data
			    int peaknumber = msg.buffer.upkint();
			    rootdim = msg.buffer.upkint();
			    xcoor = new double[rootdim];
			    msg.buffer.unpack(xcoor,rootdim,1);
					for (int j = minindex[peaknumber]; j < maxindex[peaknumber]; j++)
						expfit[j] += xcoor[j - minindex[peaknumber]];
				}
				while (receivedTasks < numberofpeaks) {
					// receiving a work done
			    jpvmMessage msg = jpvm.pvm_recv(jpvmDaemon.recvPVTag);
			    receivedTasks++;
			    int peaknumber = msg.buffer.upkint();
			    int rootdim = msg.buffer.upkint();
			    double[] xcoor = new double[rootdim];
			    msg.buffer.unpack(xcoor,rootdim,1);
					for (int j = minindex[peaknumber]; j < maxindex[peaknumber]; j++) {
						expfit[j] += xcoor[j - minindex[peaknumber]];
					}
				}
			} catch (jpvmException jpe) {
				System.out.println("jpvm Exception - "+jpe.toXRDcatString());
			}
		} */


// Voyager

  boolean maudServerIsActive = false;
//	boolean voyagerActive = false;
  String serverName = "localhost";
  String serverPort = "8000";

  void enableLocalServerComputing(boolean enable) {
    maudServerIsActive = enable;
    if (!enable) {
//			if (voyagerActive)
//				Voyager.shutdown();
//			voyagerActive = false;
    }
  }

/*	public lFilePar createFileParOnServer(String filename) {
		lFilePar tmpFilePar = null;
    try
    {
		  System.out.println("Starting up voyager.....");
      Voyager.startup(); // startup as client
		  System.out.println("Voyager started");
      voyagerActive = true;
      // create a remote FilePar

      Object[] args = new Object[2];
      args[0] = filename;
      args[1] = this;
      final String classname = "it.unitn.ing.rista.diffr.FilePar";
      tmpFilePar = (lFilePar) Factory.create( classname, args, "//" + serverName + ":"
      													+ serverPort);
    }
    catch( Exception exception )
    {
    	exception.printStackTrace();
//     	System.err.println( exception );
    }
		if (tmpFilePar == null) {
		  tmpFilePar = new FilePar(filename, this);
		  System.out.println("Creating the analysis object on the client....");
		}
		return tmpFilePar;

	}*/


}

