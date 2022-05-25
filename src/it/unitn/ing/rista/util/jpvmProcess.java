/*
 * @(#)jpvmProcess.java created 19/12/2002 Berkeley, 405 Office
 *
 * Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.util;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.jpvm.*;
import it.unitn.ing.rista.comp.ParallelComputationController;

import java.io.*;
import java.util.*;

/**
 *  The jpvmProcess is a class to launch and control the jpvm
 *  distributed computing. No interface will be generated or shown (textonly mode).
 *
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class jpvmProcess implements jpvmObject {

  String filename = ParallelComputationController.jpvmAnalysisFolder + ParallelComputationController.jpvmAnalysisFile;
  String[] folderandname = null;
  String httpDataServer = ParallelComputationController.jpvmDefaultDataServer;

  public jpvmProcess(String[] args) {
    for (int i = 0; i < args.length; i++) {
      if (args[i].toLowerCase().startsWith("-location")) {
        StringTokenizer st = new StringTokenizer(args[i], "=\'\r\n");
        String token = st.nextToken();
        if (st.hasMoreTokens())
          httpDataServer = st.nextToken();
      }
    }
  }

  public void process() {
  }

  public int setField(String cif, String astring) {
    String[] newfolderandname = Misc.getFolderandName(astring);//folderandname[0] +
    FilePar analysis = new FilePar(newfolderandname[1]);
    analysis.setDirectory(newfolderandname[0]);
    Reader in = Misc.getReader(analysis.getDirectory(), analysis.getFileName());
    analysis.readall(in, null);
	  analysis.setFileNamePreserveExtension(newfolderandname[1], false);
	  analysis.setDirectory(newfolderandname[0]);
    processAnalysis(analysis, -1);

    return 0;
  }

  public void execute(String[] args) {
/*		try {
			jpvmEnvironment jpvm = null;
			if (args == null)
				jpvm = new jpvmEnvironment();
			else
				jpvm = new jpvmEnvironment(args);

			jpvmTaskId myTaskId = jpvm.pvm_mytid();

		// Get my parent's task id...
			jpvmTaskId parent = jpvm.pvm_parent();

			boolean isRunning = true;

			while (isRunning) {
//		System.out.println("waiting for messages...");
				jpvmMessage msg = jpvm.pvm_recv(Constants.sendPVTag);
//		System.out.println("received, computing");

				int peaknumber = msg.buffer.upkint();
				if (peaknumber == -1) {
					isRunning = false;
				} else {
					double intensity = msg.buffer.upkdouble();
					double position = msg.buffer.upkdouble();
					double eta = msg.buffer.upkdouble();
					double hwhm = msg.buffer.upkdouble();
					int rootdim = msg.buffer.upkint();
					double[] xcoor = new double[rootdim];
					msg.buffer.unpack(xcoor,rootdim,1);

					xcoor = getY(intensity,position,eta,hwhm,xcoor);

					jpvmBuffer buf = new jpvmBuffer();
					buf.pack(peaknumber);
					buf.pack(rootdim);
					buf.pack(xcoor, rootdim, 1);
//		System.out.println("sending results...");
					jpvm.pvm_send(buf, parent, Constants.recvPVTag);
				}

			}
			jpvm.pvm_exit();

		} catch (jpvmException ie) {
			ie.printStackTrace();
		}*/
  }

  public void processAnalysis(FilePar analysis, int wizardindex) {
    if (analysis != null) {
      long time = System.currentTimeMillis();
      double tottime = 0.0f;
      if (wizardindex == 999) {
//        System.out.println("Starting function computation for analysis file: " + analysis.toXRDcatString());
        analysis.launchrefine(null);
        System.out.println(analysis.getWSS());
        double[] parameters = analysis.getfreeParameters();
        for (int i = 0; i < parameters.length; i++) {
          System.out.println(parameters[i]);
        }
//        System.out.println("Time for computation was: " + (System.currentTimeMillis() - time) + " millisecs.");
      } if (wizardindex == -1) {
        System.out.println("Starting function computation for analysis file: " + analysis.toXRDcatString());
        analysis.compute(null);
        System.out.println("Time for computation was: " + (System.currentTimeMillis() - time) + " millisecs.");
      } else if (wizardindex < 0) {
        System.out.println("Starting refinement for analysis file: " + analysis.toXRDcatString());
  //      analysis.startingRefine();
        analysis.launchrefine(null);
        BufferedWriter out = Misc.getWriter(analysis.getDirectory(), analysis.getFileName());
        analysis.writeall(out);
        System.out.println("Time for computation was: " + (System.currentTimeMillis() - time) + " millisecs.");
      } else {
        System.out.println("Starting wizard for analysis file: " + analysis.toXRDcatString());
        analysis.refineWizard(null, wizardindex);
        BufferedWriter out = Misc.getWriter(analysis.getDirectory(), analysis.getFileName());
        analysis.writeall(out);
        System.out.println("Time for computation was: " + (System.currentTimeMillis() - time) + " millisecs.");
      }
    }
  }

}
