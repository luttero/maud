/*
 * @(#)FEMRodriguezTexture.java created Jul 15, 2003 Berkeley
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.rta;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import java.io.*;


/**
 * The FEMRodriguezTexture is a class to compute the texture
 * using the Finete Elements over Rodriguez Space rapresentation
 * following the method of N. R. Barton et al., Texture and Microstructure,
 * 35(2), 113-114, 2002.
 *  
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:57 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class FEMRodriguezTexture extends Texture {

  protected static String[] diclistc = {};
  protected static String[] diclistcrm = {};

  protected static String[] classlistc = {};

  public FEMRodriguezTexture(XRDcat obj, String alabel) {
    super(obj, alabel);
    initBaseObject();
    identifier = "Disabled FEM Rodriguez Space";
    IDlabel = "FEM Rodriguez Space";
    description = "select this to apply the FEM over Rodriguez Space";
  }

  public FEMRodriguezTexture(XRDcat afile) {
    this(afile, "FEM Rodriguez Texture");
    identifier = "Disabled FEM Rodriguez Space";
    IDlabel = "FEM Rodriguez Space";
    description = "select this to apply the FEM over Rodriguez Space";
  }

  public FEMRodriguezTexture() {
    identifier = "Disabled FEM Rodriguez Space";
    IDlabel = "FEM Rodriguez Space";
    description = "select this to apply the FEM over Rodriguez Space";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
  }

  public void initParameters() {
    super.initParameters();
  }

  public void computeTextureFactor(Phase aphase, Sample asample) {
//    FilePar aparFile = getFilePar();
    if (!refreshComputation)
      return;
/*    int hkln = aphase.gethklNumber();
    for (int j = 0; j < hkln; j++) {
      Reflection refl = (Reflection) aphase.reflectionv.elementAt(j);
      refl.randomToTextureFactor();
    }*/

    String command = null;
    command = getShellCommandForPlatform();
    Runtime runtime = Runtime.getRuntime();
    Misc.deleteFile(Constants.cachesDirectory + "FEMRodriguez.ended");
    try {
      Process proc = runtime.exec(command);
    } catch (IOException ioe) {
      ioe.printStackTrace();
    }
    try {
      while (!Misc.checkForFile(Constants.cachesDirectory + "FEMRodriguez.ended"))
        Thread.sleep(1000);
    } catch (InterruptedException it) {
      it.printStackTrace();
    }

    refreshComputation = false;
  }

  public String getShellCommandForPlatform() {
    String command = Constants.cachesDirectory + "/FEMRodriguez.";
    switch (Constants.osType) {
      case Constants.OsUnix:
      case Constants.OsLinux:
        command += "sh";
        break;
      case Constants.OsWindoof:
        command += "bat";
        break;
    }
    return command;  //To change body of created methods use Options | File Templates.
  }

/*  public void saveTextureFactor(Phase aphase, Sample asample) {
    super.saveTextureFactor(aphase, asample);
  }*/

  public void inputTextureFactor(Phase aphase, Sample asample) {
/*

    String filename = new String(getFilePar().getDirectory() +
            aphase.toXRDcatString() + ".apf");
    BufferedWriter PFreader = Misc.getReader(filename);
    if (PFreader != null) {
      try {

        PFwriter.write("Texture factors extracted for phase: " + aphase.toXRDcatString());
        PFwriter.newLine();

        int hkln = aphase.gethklNumber();
        PFwriter.write(hkln + "                  IZPOL");
        PFwriter.newLine();
        for (int j = 0; j < hkln; j++) {
          Reflection refl = (Reflection) aphase.reflectionv.elementAt(j);
          PFwriter.write(refl.h + "   " + refl.k + "   " + refl.l + "            H K L");
          PFwriter.newLine();

          int numberDatafiles = asample.getNumberActiveDatafiles();
          PFwriter.write(numberDatafiles + " <-  MEPSUM");
          PFwriter.newLine();
          double wgt = (double) refl.getWeight();

          for (int index = 0; index < numberDatafiles; index++) {
            double[] angles = refl.getActiveTextureAngles(index, asample);
            double pf = (double) refl.getExpTextureFactor(index);
            double chi = angles[0];
            double phi = angles[1];
            int npoint = index + 1;
            PFwriter.write(chi + " " + phi + " " + pf + " " + npoint + " " + wgt);
            PFwriter.newLine();
          }
        }
        PFwriter.flush();
        PFwriter.close();
      } catch (IOException io) {
        try {
          PFwriter.flush();
          PFwriter.close();
        } catch (IOException ieo) {
        }
      }
    }*/
  }

 }
