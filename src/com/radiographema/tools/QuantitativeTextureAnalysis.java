/*
 * @(#)QuantitativeTextureAnalysis.java created Aug 1, 2007 Mesiano
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
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
package com.radiographema.tools;

import it.unitn.ing.rista.awt.ProgressPanel;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.instbroad.InstrumentBroadeningPVCaglioti;
import it.unitn.ing.rista.diffr.instrument.DefaultInstrument;
import it.unitn.ing.rista.diffr.rta.MEMLTexture;
import it.unitn.ing.rista.diffr.rta.ExpHarmonicTexture;
import it.unitn.ing.rista.mdyn.Atom;
import it.unitn.ing.rista.util.*;

import java.io.*;
import java.util.ArrayList;
import java.util.StringTokenizer;

/**
 * The QuantitativeTextureAnalysis is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Aug 1, 2007 11:15:45 AM $
 * @since JDK1.1
 */
public class QuantitativeTextureAnalysis {

  public static int minimumNumberOfPoints = 10;
  public double deltaXCenter = 0.0;
  public double deltaYCenter = 0.0;
  public double omegaDN = 0.0;
  public double phiDA = 0.0;
  public double etaDA = 0.0;
  public double detector2Theta = 0.0;
  public double detectorDistance = 0.0;
  public static final int siliconID = 0;
  public static final int niID = 1;
  double phaseLimitForRemove = 0.01,
      phaseLimitForCellParameters = 0.05,
      phaseLimitForMicrostructure = 0.3,
      phaseLimitForCrystalStructure = 0.2,
      phaseLimitForTexture = 0.2,
      phaseLimitForStrain = 0.5;
  boolean performAutomaticAnalysis = false;
  String beamIntensity = "0.2";

  public void performAnalysis1() {
    (new Thread() {
      public void run() {
        performAnalysis();
      }
    }).start();
  }

  public void performAnalysis() {

    // to check how much time is needed for the full analysis process
    long time = System.currentTimeMillis();

    // initialize all Maud constants and variables, loading preferences and checking models and plugins
    Constants.textonly = true; // no graphical interface running
    // Constants.stdoutput = Constants.NO_OUTPUT; // no console output, default is with output
    Constants.initConstants(); // load all preferences and models

    int wizardindex = 13; // the automatic texture and phase analysis

    // here we prepare the analysis file
    // we can load it from a file or define completely manually
    String analysisFilename = "path/newAnalysis.par";  //
    Misc.println("Load analysis as:" + analysisFilename);
    String[] newfolderandname = Misc.getFolderandName(analysisFilename);
    FilePar analysis = loadParameters(newfolderandname, null);

    // here we specify the name of the file where to save the analysis, for example
    // same name as the one from which we loaded the analysis. Set the string equal to null to
    // avoid saving it

    String filenameToSave = analysis.getDirectory() + analysis.getFileName();  // null;

/*    analysis.initializeAnalysis();
    analysis.setLimitsForWizard(phaseLimitForRemove,
        phaseLimitForCellParameters,
        phaseLimitForMicrostructure,
        phaseLimitForCrystalStructure,
        phaseLimitForTexture,
        phaseLimitForStrain);
    Sample asample = new Sample(analysis);
    analysis.addSample(asample);
    asample.addLayer();
    DataFileSet adatafileset = new DataFileSet(asample);
    asample.addDataFileSet(adatafileset);
    adatafileset.setBackgroundInterpolated(false);

    // we define the instrument
    Instrument instrument = new DefaultInstrument(adatafileset);
    adatafileset.setInstrument(instrument);
    instrument.setLabel("Nova 250 micron");
    instrument.setIntensity(beamIntensity);
    instrument.setMeasurement("2Theta");
    instrument.setDetector("Curved Position Sensitive");
    instrument.add2ThetaDisplacementParameter();
    instrument.setAngularCalibration("Inclined Reflection Image");
    AngularInclinedFlatImageCalibration angcal =
        (AngularInclinedFlatImageCalibration) instrument.getAngularCalibration();
    angcal.setDetectorDistance(detectorDistance);
    angcal.setOriginalCenterX(0.0);
    angcal.setOriginalCenterY(0.0);
    angcal.setDetector2Theta(detector2Theta);
    angcal.setDetectorPhiDA(phiDA);
    angcal.setDetectorEtaDA(etaDA);
    angcal.setDetectorOmegaDN(omegaDN);

    // instrument broadening
    InstrumentBroadening instBroad = instrument.getInstrumentBroadening();
    if (instBroad instanceof InstrumentBroadeningPVCaglioti) {
      InstrumentBroadeningPVCaglioti inb = (InstrumentBroadeningPVCaglioti) instBroad;
      inb.setCagliotiTanDependent(true);
      inb.setAsymmetryTanDependent(false);
      inb.setBroadeningConvoluted(false);
      inb.setTruncationAngle("0.8");
      inb.getAsymmetryList().removeAllItems();
      inb.getAsymmetryList().addItem(new Parameter(inb, "Asymmetry coeff 0", 135.185345));
      inb.getAsymmetryList().addItem(new Parameter(inb, "Asymmetry coeff 1", -0.2044876));
      inb.getAsymmetryList().addItem(new Parameter(inb, "Asymmetry coeff 2", -2.3630368E-5));
      inb.getCagliotiList().removeAllItems();
      inb.getCagliotiList().addItem(new Parameter(inb, "Caglioti coeff W", 0.0103287235));
      inb.getCagliotiList().addItem(new Parameter(inb, "Caglioti coeff V", -0.035830247));
      inb.getCagliotiList().addItem(new Parameter(inb, "Caglioti coeff U", 0.08134942));
      inb.getGaussianList().removeAllItems();
      inb.getGaussianList().addItem(new Parameter(inb, "gaussian coeff 0", 1.1103061));
      inb.getGaussianList().addItem(new Parameter(inb, "gaussian coeff 1", -0.029315038));
      inb.getGaussianList().addItem(new Parameter(inb, "gaussian coeff 2", 3.079934E-4));
    }

    // now we add the data
    adatafileset.addDataFileforName(folderAndName[0] + folderAndName[1], true);
    adatafileset.addBackgroudCoeff();
    adatafileset.addBackgroudCoeff();
    adatafileset.addBackgroudCoeff();
    adatafileset.addAdditionalBackgroundToAll();
    adatafileset.addAdditionalBackgroundToAll();
    adatafileset.addAdditionalBackgroundToAll();
    // phases can be added manually as in the following in which we specify the full setting (this
    // can be the preferred way if the phases are stored inside as a proprietary database
    addPhase(asample, siliconID);
    addPhase(asample, niID);

    // or can be loaded by CIF files, you get either from COD, ICSD or constructed manually
    //this one contains different NiSi phases
    asample.loadPhase("resources/CIF_NiSi/nisi.cif", false);
    // these only ones
    asample.loadPhase("resources/CIF_NiSi/NiSi2.cif", false);
    asample.loadPhase("resources/CIF_NiSi/Ni2Si.cif", false);
    for (int p = 0; p < asample.phasesNumber(); p++)
      asample.getlayer(0).setPhaseQuantity(p, "0.1");

    // now we specify we want to use the EWIMV texture model (the string to use is what appear in the
    // combobox in the Maud Graphic Interface when we select the texture model)

    for (int p = 0; p < asample.phasesNumber(); p++) {
      Phase actualPhase = asample.getPhase(p);
      // int the final version we use the E-WIMV, the harmonic can be used for testing or in the first run
      actualPhase.setTextureModel("Exponential Harmonic");
      ((ExpHarmonicTexture) actualPhase.getActiveTexture()).setSampleSymmetry(Texture.FIBER);//.setSampleSymmetry(Texture.FIBER);
      ((ExpHarmonicTexture) actualPhase.getActiveTexture()).setExpansionDegree(4);
//        actualPhase.setTextureModel("Harmonic");
//        ((HarmonicTexture) actualPhase.getActiveTexture()).setSampleSymmetry("fiber"); // not necessary is the default for harmonic
//        if (actualPhase.isCubic())
//          ((HarmonicTexture) actualPhase.getActiveTexture()).setExpansionDegree(6);  // so we have 2 coefficients at least
//        else
//          ((HarmonicTexture) actualPhase.getActiveTexture()).setExpansionDegree(4);

    }*/

    // we force a refresh of the analysis to check out everything
    analysis.refreshAll(false);

    // we save the analysis before to start
    if (filenameToSave != null) {
      String[] folderandnameToSave = Misc.getFolderandName(filenameToSave + ".start");
      BufferedWriter out = Misc.getWriter(folderandnameToSave[0], folderandnameToSave[1]);
      analysis.writeall(out);
    }

    // here is the main loop for the analysis, the loop check for phases that are eventually
    // not present and force them to zero content. The loop stop when all the zero phases have been
    // eliminated from the analysis
    if (performAutomaticAnalysis) {
      double lastGofF;
      double actualGofF = 999.0;
      do {
        if (filenameToSave != null) {
          String[] folderandnameToSave = Misc.getFolderandName(filenameToSave);
          BufferedWriter out = Misc.getWriter(folderandnameToSave[0], folderandnameToSave[1]);
          analysis.writeall(out);
        }
        lastGofF = actualGofF;
        if (actualGofF != 999.0 && analysis.getNumberNonZeroPhases() > 1) {
          analysis.setZeroPhase(analysis.getMinorPhase());
//          wizardindex = 13;
        }
        if (analysis.getNumberNonZeroPhases() > 0) {
          analysis.refineWizard(null, wizardindex);
          wizardindex = 4;
          actualGofF = Math.sqrt(Double.parseDouble(analysis.getRw()) / Double.parseDouble(analysis.getRexp()));
        } else
          actualGofF = lastGofF + 10.0;
      } while (lastGofF >= actualGofF && analysis.getNumberNonZeroPhases() > 1);

      analysis.getActiveSample().getActiveDataSet(0).setBackgroundInterpolated(true);
      analysis.getActiveSample().getActiveDataSet(0).setInterpolatedPoints(10);
      analysis.refineWizard(null, wizardindex);  // texture analysis

      if (filenameToSave != null) {
        String[] folderandnameToSave = Misc.getFolderandName(filenameToSave);
        BufferedWriter out = Misc.getWriter(folderandnameToSave[0], folderandnameToSave[1]);
        analysis.writeall(out);
      }
      // here the result summary is saved in a file testResults.txt, but changing where the output goes
      // it can be keep in memory or sent back to the requester
      String[] resfolderAndName = Misc.getFolderandName("path/results.txt");
      boolean newFile = false;
      if (!Misc.checkForFile(resfolderAndName[0] + resfolderAndName[1]))
        newFile = true;
      try {
        BufferedWriter out = Misc.getWriterForAppend(resfolderAndName[0], resfolderAndName[1]);
        appendResultsTo(out, newFile, analysis, true);
        out.close();
      } catch (Exception e) {
        e.printStackTrace();
      }
    }
    Sample asample = analysis.getActiveSample();
    for (int p = 0; p < asample.phasesNumber(); p++) {
      Phase actualPhase = asample.getPhase(p);
//      ((ExpHarmonicTexture) actualPhase.getActiveTexture()).
    }

    // Computing finally how much time was needed
    Misc.println("Time for computation was: " + (System.currentTimeMillis() - time) + " millisecs.");

    DataFileSet adatafileset = asample.getActiveDataSet(0);
    adatafileset.setReplaceDatafile(true); // only the datafile loaded will be in the dataset after
    if (adatafileset.isDiffraction()) {// load a diffraction datafile
      DiffrDataFile[] datafileList = adatafileset.addDataFileforName("path/name_of_the_datafile.extension", false);
      for (DiffrDataFile datafile : datafileList) {
        datafile.backgroundSubtraction();
      }

    } else {  /// XRF ???
      DiffrDataFile[] datafileList = adatafileset.addDataFileforName("path/name_of_the_datafile.mca", false);
      for (DiffrDataFile datafile : datafileList) {
        datafile.backgroundSubtraction();
      }
    }
    StringBuffer fileNameForOutput = new StringBuffer(analysisFilename);
    fileNameForOutput.append(".txt");
    analysis.saveDataForAI(fileNameForOutput.toString(), null);

  }

  private void addPhase(Sample asample, int phaseID) {

    switch (phaseID) {
      case (siliconID):
        // we add as an example a new Silicon phase

        Phase silicon = new Phase(asample);
        asample.addPhase(silicon);
        silicon.setPhaseName("Silicon");
        silicon.setPhaseID("Silicon");
        silicon.setLabel("Silicon");
        // setting symmetry, space group and cell parameter
        silicon.setSymmetry(Phase.CUBIC);
        silicon.setSpaceGroup(true, "Fd-3m:1", false);  // :1 = first setting
        silicon.setCellValue(0, 5.430938);  // 0 = a, 1 = b, 2 = c, 3 = alpha, 4 = beta, 5 = gamma
        // adding atoms, this must be done for every atom site
        AtomSite atom = new AtomSite(silicon);
        StructureModel structure = silicon.getActiveStructureModel();
        structure.addAtom(atom);
        // setting lable and symbol
        atom.setSiteLabel("Si1");
        atom.addAtomWithSymbol("Si");
        // setting coordinates x, y, z
        atom.getLocalCoordX().setValue(0.0);
        atom.getLocalCoordY().setValue(0.0);
        atom.getLocalCoordZ().setValue(0.0);
        // setting occupancy and Debye-Waller factor
        atom.getOccupancy().setValue(1.0);
        atom.getBfactor().setValue(0.366);
        break;
      case niID:
        // we add as an example a new Nickel phase

        Phase nickel = new Phase(asample);
        asample.addPhase(nickel);
        nickel.setPhaseName("Nickel");
        nickel.setPhaseID("Nickel");
        nickel.setLabel("Nickel");
        // setting symmetry, space group and cell parameter
        nickel.setSymmetry(Phase.CUBIC);
        nickel.setSpaceGroup(true, "Fm-3m", false);  // :1 = first setting
        nickel.setCellValue(0, 3.529608);  // 0 = a, 1 = b, 2 = c, 3 = alpha, 4 = beta, 5 = gamma
        // adding atoms, this must be done for every atom site
        atom = new AtomSite(nickel);
        structure = nickel.getActiveStructureModel();
        structure.addAtom(atom);
        // setting lable and symbol
        atom.setSiteLabel("Ni1");
        atom.addAtomWithSymbol("Ni");
        // setting coordinates x, y, z
        atom.getLocalCoordX().setValue(0.0);
        atom.getLocalCoordY().setValue(0.0);
        atom.getLocalCoordZ().setValue(0.0);
        // setting occupancy and Debye-Waller factor
        atom.getOccupancy().setValue(1.0);
        // we can just leave the default value (0.0)
//        atom.getBfactor().setValue(0.2);
        break;
      default: {
      }
    }
  }

  public FilePar loadParameters(String[] folderAndName, ProgressPanel pcontrol) {
    java.io.Reader in = null;
    FilePar analysis = new FilePar(folderAndName[1]);
    analysis.setDirectory(folderAndName[0]);
    try {
      in = Misc.getReader(folderAndName[0], folderAndName[1]);
      analysis.readall(in, pcontrol);
//      analysis.setFileName(folderAndName[1], false);
//      analysis.setDirectory(folderAndName[0]);
    } catch (Exception e) {
      e.printStackTrace();
    }
    return analysis;
  }

  private void checkCenter() {
    BufferedReader in = Misc.getReader("", "centerImage.inf");
    try {
      String line = in.readLine();
      StringTokenizer st = new StringTokenizer(line, "' ,\t\r\n");
      deltaXCenter = Double.parseDouble(st.nextToken());
      deltaYCenter = Double.parseDouble(st.nextToken());
      double value = Double.parseDouble(st.nextToken());
      if (value != 0.0)
        detectorDistance = value;
      value = Double.parseDouble(st.nextToken());
      if (value != 0.0)
        detector2Theta = value;
      value = Double.parseDouble(st.nextToken());
      if (value != 0.0)
        phiDA = value;
      value = Double.parseDouble(st.nextToken());
      if (value != 0.0)
        omegaDN = value;
      etaDA = 0.0;
      phaseLimitForRemove = Double.parseDouble(st.nextToken());
      phaseLimitForCellParameters = Double.parseDouble(st.nextToken());
      phaseLimitForMicrostructure = Double.parseDouble(st.nextToken());
      phaseLimitForCrystalStructure = Double.parseDouble(st.nextToken());
      phaseLimitForTexture = Double.parseDouble(st.nextToken());
      phaseLimitForStrain = Double.parseDouble(st.nextToken());
      int automatic = Integer.parseInt(st.nextToken());
      if (automatic == 1)
        performAutomaticAnalysis = true;
      else
        performAutomaticAnalysis = false;
      beamIntensity = st.nextToken();
      in.close();
    } catch (Exception e) {
      e.printStackTrace();
      try {
        in.close();
      } catch (IOException e1) {
        e1.printStackTrace();
      }
    }
  }

  // this routine was extracted from the FilePar object so in case it can be customize
  public void appendResultsTo(BufferedWriter out, boolean newFile, FilePar analysis, boolean simpleOutput) {
    try {
      if (out != null) {
        if (simpleOutput) {
          if (newFile) {
            analysis.writeSimpleResultsFirstLine(out);
            out.write(Constants.lineSeparator);
          }
          analysis.writeSimpleResults(out);
        } else {
          if (newFile) {
            analysis.writeResultsFirstLine(out);
            out.write(Constants.lineSeparator);
          }
          analysis.writeResults(out);
        }
        out.write(Constants.lineSeparator);
        out.flush();
      } else {
        Misc.println("Not able to open the file for append");
      }
    } catch (IOException ioe) {
      ioe.printStackTrace();
    }
  }
}