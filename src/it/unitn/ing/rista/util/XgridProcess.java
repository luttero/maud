/*
 * @(#)XgridProcess.java created Nov 19, 2005 Casalino
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

package it.unitn.ing.rista.util;

import it.unitn.ing.rista.diffr.FilePar;

import java.util.StringTokenizer;
import java.io.Reader;
import java.io.BufferedWriter;


/**
 * The XgridProcess is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/02/02 16:11:56 $
 * @since JDK1.1
 */

public class XgridProcess {

  String[] folderandname = new String[2];
  String httpDataServer;

  public XgridProcess(String[] args) {
    for (int i = 0; i < args.length; i++) {
      if (args[i].toLowerCase().startsWith("-location")) {
        StringTokenizer st = new StringTokenizer(args[i], "=\'\r\n");
        st.nextToken();
        if (st.hasMoreTokens())
          httpDataServer = st.nextToken();
      }
      if (args[i].toLowerCase().startsWith("-analysis")) {
//        StringTokenizer st = new StringTokenizer(args[i], "=\'\r\n");
//        String token = st.nextToken();
        if (args.length > i+1) {
          folderandname[1] = args[i+1];
          folderandname[0] = "";
        }
      }
    }
  }

  public void process() {
    if (folderandname == null)
      return;
    FilePar analysis = new FilePar(folderandname[1]);
    analysis.setDirectory(folderandname[0]);
    Reader in = Misc.getReader(analysis.getDirectory(), analysis.getFileName());
    analysis.readall(in, null);
	  analysis.setFileNamePreserveExtension(folderandname[1], false);
	  analysis.setDirectory(folderandname[0]);
    processAnalysis(analysis, 999);
  }

  public void processAnalysis(FilePar analysis, int wizardindex) {
    if (analysis != null) {
      long time = System.currentTimeMillis();
//      float tottime = 0.0f;
      if (wizardindex == 999) {
//        System.out.println("Starting function computation for analysis file: " + analysis.toXRDcatString());
        analysis.launchrefine(null);
        System.out.println("XGrid solution:");
        System.out.println(analysis.getRw());
        double[] parameters = analysis.getfreeParameters();
        for (int i = 0; i < parameters.length; i++) {
          System.out.println(parameters[i]);
        }
//        System.out.println("Time for computation was: " + (System.currentTimeMillis() - time) + " millisecs.");
      } else if (wizardindex == -1) {
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
