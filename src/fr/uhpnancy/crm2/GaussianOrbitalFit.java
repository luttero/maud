/*
 * @(#)GaussianOrbitalFit.java created MAy 19, 2011 Nancy
 *
 * Copyright (c) 2011 Luca Lutterotti All Rights Reserved.
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
package fr.uhpnancy.crm2;

import java.io.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.instrument.DefaultInstrument;
import it.unitn.ing.rista.comp.LeastSquareFit;

/**
 * The GaussianOrbitalFit is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: May 19, 2011 1:32:21 PM $
 * @since JDK1.1
 */
public class GaussianOrbitalFit {

  public static void main(String args[]) {
    if (args == null || args.length == 0)
      usage();
    (new GaussianOrbitalFit()).performAnalysis(args);
//    (new GaussianOrbitalFit()).performAnalysis();
  }

  public static void usage() {
    System.out.println("");
  }

  public void performAnalysis(String args[]) {

    long time = System.currentTimeMillis();

    // initialize all Maud constants and variables, loading preferences and checking models and plugins
    Constants.textonly = true; // no graphical interface running
    // Constants.stdoutput = Constants.NO_OUTPUT; // no console output, default is with output
    Constants.initConstants(); // load all preferences and models

    // here we prepare the analysis file
    // we can load it from a file or define completely manually

 //   String[] folderAndName = {"mypath", "mySpectrum.cif"};

    String analysisFilename = args[0];
    String filenameToSave = analysisFilename;
    String[] analysisfolderandname = Misc.getFolderandName(analysisFilename);
    FilePar analysis = new FilePar(analysisfolderandname[1]);
//    		else
//    			parameterfile = createFileParOnServer(folderAndName[1]);

    analysis.setDirectory(analysisfolderandname[0]);

    Sample asample = new Sample(analysis);
    analysis.addSample(asample);
    asample.addLayer();
    DataFileSet adatafileset = new DataFileSet(asample);
    asample.addDataFileSet(adatafileset);
    adatafileset.setBackgroundInterpolated(false);

    // we define the instrument
    Instrument instrument = new DefaultInstrument(adatafileset);
    adatafileset.setInstrument(instrument);

    // now we add the data

    String[] datafile = Misc.getFolderandName(args[2]);
    int patternNumber = -1;
    if (args.length > 3)
      patternNumber = Integer.parseInt(args[3]);
    if (patternNumber >= 0)
      datafile[1] += "(" + Integer.toString(patternNumber) + ")";
    adatafileset.addDataFileforName(datafile[0] + datafile[1], true);

    int l = 2;
    if (args.length > 4)
      l = Integer.parseInt(args[4]);
    int n_max = 0;
    if (args.length > 5)
      n_max = Integer.parseInt(args[5]);
    int maxPeaks = 6;
    if (args.length > 6)
      maxPeaks = Integer.parseInt(args[6]);
    int iterations = 10;
    if (args.length > 7)
      iterations = Integer.parseInt(args[7]);
    analysis.setNumberofIterations(iterations);
    String weightingScheme = "default";
    if (args.length > 11)
      weightingScheme = args[11];
    analysis.setWeightingScheme(weightingScheme);  // sqrt, linear, log10
//    ((LeastSquareFit) analysis.getOptimizationAlgorithm()).setPrecision(0.00000001);

    double hwhm = 0.2;
    if (args.length > 8)
      hwhm = Double.parseDouble(args[8]);
    double step1 = 0.1;
    if (args.length > 9)
      step1 = Double.parseDouble(args[9]);
    double step2 = 0.1;
    if (args.length > 10)
      step2 = Double.parseDouble(args[10]);
    for (int i = 0; i < maxPeaks; i++) {
      for (int n = 0; n <= n_max; n++) {
        BkgPeak peak = new BkgPeak(adatafileset);
        adatafileset.addBkgPeak(peak);
        peak.setExponentValue(l + n * 2);
        peak.setIntensity(5.0/(i + 1) / maxPeaks / (n+1));
        peak.setPosition(0, 0.0);
        peak.setHWHM(0, hwhm);
        peak.getIntensity().setRefinable();
      }
      hwhm += step1 + step2 * i;
    }

    // we force a refresh of the analysis to check out everything
    analysis.refreshAll(false);

    // we save the analysis before to start
    if (filenameToSave != null) {
      String[] folderandnameToSave = Misc.getFolderandName(filenameToSave + ".start");
      BufferedWriter out = Misc.getWriter(folderandnameToSave[0], folderandnameToSave[1]);
      analysis.writeall(out);
    }

    analysis.launchrefine(null);  // fitting first step: only intensities and HWHM

    for (int i = 0; i < maxPeaks * (n_max + 1); i++) {
      BkgPeak peak = adatafileset.getBkgPeak(i);
      peak.getHWHM(0).setRefinable();
    }

    // we force a refresh of the analysis to check out everything
    analysis.refreshAll(false);

    analysis.launchrefine(null);  // fitting first step: only intensities and HWHM


    if (filenameToSave != null) {
      String[] folderandnameToSave = Misc.getFolderandName(filenameToSave + ".par");
      BufferedWriter out = Misc.getWriter(folderandnameToSave[0], folderandnameToSave[1]);
      analysis.writeall(out);
    }
      // here the result summary is saved in a file testResults.txt, but changing where the output goes
      // it can be keep in memory or sent back to the requester
    String[] resfolderAndName = Misc.getFolderandName(args[1]);
    boolean newFile = false;
    if (!Misc.checkForFile(resfolderAndName[0] + resfolderAndName[1]))
      newFile = true;
      try {
        BufferedWriter out = Misc.getWriterForAppend(resfolderAndName[0], resfolderAndName[1]);
        if (newFile) {
          out.write("# Title");
          out.newLine();
          out.write("# Rw GofF numberOfGaussians");
          out.newLine();
          out.write("# Gaussian_number l k Ci Ci(esd) Alphai Alphai(esd)");
          out.newLine();
        }
        out.write(analysis.getDatafile(0).title);
        out.newLine();
        double sigma = Double.parseDouble(analysis.getRw()) / Double.parseDouble(analysis.getRexp());
        out.write(analysis.getRw() + " " + Double.toString(sigma) + " " + maxPeaks);
        out.newLine();
        int k = 0;
        for (int i = 0; i < maxPeaks; i++) {
          for (int n = 0; n <= n_max; n++) {
          BkgPeak peak = adatafileset.getBkgPeak(k);
          double hwhm1 = peak.getHWHM(0).getValueD();
          double intens = peak.getIntensity().getValueD();
          double alphai = Constants.LN2 / hwhm1 / hwhm1;
          double ci = Constants.sqrtln2pi / hwhm1 * intens;
          double dhwhm = Double.parseDouble(peak.getHWHM(0).getError());
          double dI = Double.parseDouble(peak.getIntensity().getError());
          double dalphai = Math.abs(-2.0 * Constants.LN2 / Math.pow(hwhm1, 3) * dhwhm);
          double dci = Math.abs(Constants.sqrtln2pi * intens / hwhm1 / hwhm1 * dhwhm) + Math.abs(Constants.sqrtln2pi / hwhm1 * dI);
//          out.write(i + " " + Double.toString(hwhm1) + " " + Double.toString(dhwhm)
//              + " " + Double.toString(intens) + " " + Double.toString(dI));
//          out.newLine();
          out.write(k + " " + l + " " + (n * 2) + " " + Double.toString(ci) + " " + Double.toString(dci)
              + " " + Double.toString(alphai) + " " + Double.toString(dalphai));
          out.newLine();
            k++;
          }
        }
        out.close();
      } catch (Exception e) {
        e.printStackTrace();
      }


    // Computing finally how much time was needed
    System.out.println("Time for computation was: " + (System.currentTimeMillis() - time) + " millisecs.");
  }

  // this routine was extracted from the FilePar object so in case it can be customize
}
