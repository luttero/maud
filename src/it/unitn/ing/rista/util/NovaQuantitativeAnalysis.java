/*
 * @(#)NovaQuantitativeAnalysis.java created Dec 27, 2007 Caen
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
package it.unitn.ing.rista.util;

import java.awt.*;
import java.io.*;

import it.unitn.ing.rista.awt.ODFMapPlot;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.rta.MEMLTexture;
import it.unitn.ing.rista.diffr.instbroad.InstrumentBroadeningPVCaglioti;
import it.unitn.ing.rista.diffr.instrument.DefaultInstrument;

/**
 * The NovaQuantitativeAnalysis is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Dec 27, 2007 3:37:33 PM $
 * @since JDK1.1
 */
public class NovaQuantitativeAnalysis {

  protected double[] photoplateIntensity = null;
  protected double[] xcoord = null;
  protected double[] ycoord = null;
  public int detectorResolution = 1024;
  public double detectorDistance = 146;
  public double omega = 0.0;
  public double sigma = 0.0;
  public double theta2 = 0.0;
  public double omegaDN = 0.0;
  public double phiDA = 0.0;
  public double etaDA = 0.0;
  public int numberOfSpectra = 16;
  public static int minimumNumberOfPoints = 10;
  public static final int siliconID = 0;
  public static final int niID = 1;
  double phaseLimitForRemove = 0.01,
      phaseLimitForCellParameters = 0.05,
      phaseLimitForMicrostructure = 0.3,
      phaseLimitForCrystalStructure = 0.2,
      phaseLimitForTexture = 0.2,
      phaseLimitForStrain = 0.5;
  boolean performAutomaticAnalysis = true;
  double cuttingRange = 2.0;
  String photoplateName = null;

  public NovaQuantitativeAnalysis(int detRes, double distance, double omega, double omegaDN, double sigma, double theta2,
                                  double[] photoplateIntensity, double[] xcoord, double[] ycoord) {
    detectorResolution = detRes;
    detectorDistance = distance;
    this.omega = omega;
    this.photoplateIntensity = photoplateIntensity;
    this.xcoord = xcoord;
    this.ycoord = ycoord;
    this.omegaDN = omegaDN;
    this.sigma = sigma;
    this.theta2 = theta2;
    etaDA = 0.0;
  }

  public void performAnalysis() {
    (new Thread() {
      public void run() {
        runPerformAnalysis();
      }
    }).start();
  }

  public void runPerformAnalysis() {

    long time = System.currentTimeMillis();
    int totalsize = detectorResolution * detectorResolution;
    double[][] map = new double[detectorResolution][detectorResolution];
    int ij1 = 0;
/*    for (int i = 0; i < detectorResolution; i++) {
      for (int j = 0; j < detectorResolution; j++)
        map[i][j] = photoplateIntensity[ij1++];
    }
    new ODFMapPlot(new Frame(), map, "photoplate", 0.0f, 0.0f, 1, detectorResolution);
    ij1 = 0;
    for (int i = 0; i < detectorResolution; i++) {
      for (int j = 0; j < detectorResolution; j++)
        map[i][j] = xcoord[ij1++];
    }
    new ODFMapPlot(new Frame(), map, "xcoord", 0.0f, 0.0f, 1, detectorResolution);*/
    ij1 = 0;
    for (int i = 0; i < detectorResolution; i++) {
      for (int j = 0; j < detectorResolution; j++)
        map[i][j] = ycoord[ij1++];
    }
    new ODFMapPlot(new Frame(), map, "ycoord", 0.0f, 0.0f, 1, detectorResolution);

    double[] theta2Coord = new double[totalsize];
    double[] etaCoord = new double[totalsize];
    double etaStep = 5.0;
    double theta2Step = 0.05;

    Angles.getTheta2EtaFromXYPixelDetector(xcoord, ycoord, theta2Coord, etaCoord, sigma, theta2, phiDA,
        etaDA, omegaDN, detectorDistance, 0.0);
//    System.out.println("Conversion to theta, eta angles done!");

    double min2theta = 2.0 * Math.PI;
    double max2theta = -2 * Math.PI;
    double mineta = 2 * Math.PI;
    double maxeta = -2 * Math.PI;
    for (int i = 0; i < theta2Coord.length; i++) {
      if (min2theta > theta2Coord[i])
        min2theta = theta2Coord[i];
      if (max2theta < theta2Coord[i])
        max2theta = theta2Coord[i];
      if (mineta > etaCoord[i])
        mineta = etaCoord[i];
      if (maxeta < etaCoord[i])
        maxeta = etaCoord[i];
    }
    System.out.println("min,max 2theta " + min2theta * Constants.PITODEG + " " + max2theta * Constants.PITODEG);
    System.out.println("min,max eta " + mineta * Constants.PITODEG + " " + maxeta * Constants.PITODEG);
    double nmineta = 0.0;
    int i = 0;
    while (nmineta < mineta)
      nmineta = i++ * etaStep * Constants.DEGTOPI;
    while (nmineta >= mineta + etaStep * Constants.DEGTOPI)
      nmineta = i-- * etaStep * Constants.DEGTOPI;
    mineta = nmineta;
    double nmintheta = 0.0;
    i = 0;
    while (nmintheta < min2theta)
      nmintheta = i++ * theta2Step * Constants.DEGTOPI;
    while (nmintheta >= min2theta + theta2Step * Constants.DEGTOPI)
      nmintheta = i-- * theta2Step * Constants.DEGTOPI;
    min2theta = nmintheta;
    System.out.println("min 2theta eta round " + nmintheta * Constants.PITODEG + " " + nmineta * Constants.PITODEG);
/*    double[] theta2Centers = Angles.centers(min2theta, max2theta, theta2Step * Constants.DEGTOPI);
    double[] etaCenters = Angles.centers(mineta, maxeta, coneInterval * Constants.DEGTOPI);
    System.out.println("eta theta centers done! " + min2theta * Constants.PITODEG + " " + max2theta * Constants.PITODEG
        + " " + mineta * Constants.PITODEG + " " + maxeta * Constants.PITODEG);*/

    double[][][] spectra = Angles.spectraFromPixelsByEtaTheta2(theta2Coord, etaCoord, photoplateIntensity,
        xcoord, ycoord, detectorDistance,
        min2theta, max2theta, theta2Step * Constants.DEGTOPI,
        mineta, maxeta, etaStep * Constants.DEGTOPI);

    System.out.println("Conversion to spectra done!");
//    saveAsText(profile, profile[0].length, 0, profile[0][0].length, min2theta * Constants.PITODEG, theta2Step,
//        mineta * Constants.PITODEG, etaStep);

//    double[][] map = new double[resolution][resolution];
//    int ij = 0;
/*    int ij1 = 0;
    for (i = 0; i < resolution; i++) {
      for (int j = 0; j < resolution; j++) {
        map[j][i] = -(i - halfResolution + 0.5) * pixelSize;
      }
      for (int j = 0; j < resolution; j++) {
        map[j][i + resolution] = ycoord[ij1++];
      }
    }
    new ODFMapPlot(new Frame(), map, title, 0.0f, 0.0f, 1, resolution);
    ij = 0;*/
/*    for (i = 0; i < resolution; i++) {
      for (int j = 0; j < resolution; j++) {
        map[j][i] = photoplateIntensity[ij++];
      }
    }
    new ODFMapPlot(new Frame(), map, title, 0.0f, 0.0f, 1, resolution);*/

/*
    if (max2Theta > 85) max2Theta = 85;
    if (min2Theta < 20) min2Theta = 20;


    etaSpectraConverter = new EtaBspectra(photoplateIntensity, xcoord, ycoord,
        0.0, sigma, 0.0, detectorDistance, rdet, theta2 - sigma,
        180.0 + phiDA, 0.0, 0.0, omegaDN,
        resolution, EtaBspectra.DONOTCHECK);

    double etaStep = 5.0;
    double theta2Step = 0.05;
    double[][][] spectra = etaSpectraConverter.convertToSpectra(etaStep, theta2Step, min2Theta + theta2Step,
        max2Theta - theta2Step); */

    // we save to check
    String[] folderAndName = saveDataAsCIF(spectra, mineta * Constants.PITODEG, etaStep);

    // initialize all Maud constants and variables, loading preferences and checking models and plugins
    Constants.textonly = true; // no graphical interface running
    // Constants.stdoutput = Constants.NO_OUTPUT; // no console output, default is with output
    Constants.initConstants(); // load all preferences and models

    int wizardindex = 13; // the automatic texture and phase analysis

    // here we prepare the analysis file
    // we can load it from a file or define completely manually
    String analysisFilename = folderAndName[0] + folderAndName[1].substring(0, folderAndName[1].lastIndexOf(".")) + ".par";
    System.out.println("Save analysis as:" + analysisFilename);
    String[] newfolderandname = Misc.getFolderandName(analysisFilename);
    FilePar analysis = new FilePar(newfolderandname[1]);
    analysis.setDirectory(newfolderandname[0]);

    // here we specify the name of the file where to save the analysis, for example
    // same name as the one from which we loaded the analysis. Set the string equal to null to
    // avoid saving it
    String filenameToSave = analysis.getDirectory() + analysis.getFileName();  // null;

    analysis.initializeAnalysis();
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
    instrument.setIntensity("200.0");
    instrument.setMeasurement("2Theta");
    instrument.setDetector("Curved Position Sensitive");
    instrument.add2ThetaDisplacementParameter();

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
      actualPhase.setTextureModel("E-WIMV");
      ((MEMLTexture) actualPhase.getActiveTexture()).setSampleSymmetry(Texture.FIBER);
//        actualPhase.setTextureModel("Harmonic");
//        ((HarmonicTexture) actualPhase.getActiveTexture()).setSampleSymmetry("fiber"); // not necessary is the default for harmonic
/*        if (actualPhase.isCubic())
          ((HarmonicTexture) actualPhase.getActiveTexture()).setExpansionDegree(6);  // so we have 2 coefficients at least
        else
          ((HarmonicTexture) actualPhase.getActiveTexture()).setExpansionDegree(4);*/

    }

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
        if (actualGofF != 999.0) {
          analysis.setZeroPhase(analysis.getMinorPhase());
          wizardindex = 13;
        }
        if (analysis.getNumberNonZeroPhases() > 0) {
          analysis.refineWizard(null, wizardindex);
          actualGofF = Math.sqrt(Double.parseDouble(analysis.getRw()) / Double.parseDouble(analysis.getRexp()));
        } else
          actualGofF = lastGofF + 10.0;
      } while (lastGofF >= actualGofF && analysis.getNumberNonZeroPhases() > 0);

      if (filenameToSave != null) {
        String[] folderandnameToSave = Misc.getFolderandName(filenameToSave);
        BufferedWriter out = Misc.getWriter(folderandnameToSave[0], folderandnameToSave[1]);
        analysis.writeall(out);
      }
      // here the result summary is saved in a file testResults.txt, but changing where the output goes
      // it can be keep in memory or sent back to the requester
      String[] resfolderAndName = Misc.getFolderandName("../SISBuffer/results.txt");
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

    // Computing finally how much time was needed
    System.out.println("Time for computation was: " + (System.currentTimeMillis() - time) + " millisecs.");
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
        System.out.println("Not able to open the file for append");
      }
    } catch (IOException ioe) {
      ioe.printStackTrace();
    }
  }

  public String[] saveDataAsCIF(double[][][] profile, double etaStart, double etaStep) {

//    String filename = Utility.openFileDialog(new Frame(), "Save as CIF (.esg)...",
//            FileDialog.SAVE, null, null, "put a name (with extension).esg");
    String filename = "../SISBuffer/CurrentAnalysis.esg";
    if (filename == null)
      return null;

    String[] folderAndName = Misc.getFolderandName(filename);

    String folder = folderAndName[0];
    filename = folderAndName[1];

    if (filename == null) return null;
    if (!filename.endsWith(".esg"))
      filename = filename + ".esg";

    int nprofiles = profile[0].length;
    int dataNumber = profile[0][0].length;

    String title = "DataFromPhotoplate_" + photoplateName;
    BufferedWriter output = Misc.getWriter(folder, filename);
    try {
      output.write("_pd_block_id " + title);
      output.newLine();
      output.newLine();
      output.write("_diffrn_detector Area Detector Hypernex");
      output.newLine();
      output.write("_diffrn_detector_type Flat Position Sensitive");
      output.newLine();
      output.write("_pd_meas_step_count_time ?");
      output.newLine();
      output.write("_diffrn_measurement_method diffraction_image");
      output.newLine();
      output.write("_diffrn_measurement_distance_unit ?");
      output.newLine();
      output.write("_pd_instr_dist_spec/detc " + detectorDistance);
      output.newLine();
      output.write("_diffrn_radiation_wavelength ?");
      output.newLine();
      output.write("_diffrn_source_target ?");
      output.newLine();
      output.write("_diffrn_source_power ?");
      output.newLine();
      output.write("_diffrn_source_current ?");
      output.newLine();
      output.write("_pd_meas_angle_omega " + omega);
      output.newLine();
      output.write("_pd_meas_angle_chi 0.0");
      output.newLine();
      output.write("_pd_meas_angle_phi 0.0");
      output.newLine();
      output.write("_riet_par_spec_displac_x 0");
      output.newLine();
      output.write("_riet_par_spec_displac_y 0");
      output.newLine();
      output.write("_riet_par_spec_displac_z 0");
      output.newLine();
      output.write("_riet_meas_datafile_calibrated true");
      output.newLine();
      output.newLine();
      for (int ij = 0; ij < nprofiles; ij++) {
        int checkPoints = 0;
        for (int i = 0; i < dataNumber; i++) {
          if (profile[1][ij][i] > 0.0) {
            checkPoints++;
          }
        }
        if (checkPoints > minimumNumberOfPoints) {
          output.write("_pd_block_id " + title + "|#" + ij);
          output.newLine();
          output.newLine();
          double eta = etaStart + etaStep * ij;
          output.write("_pd_meas_angle_eta " + Double.toString(eta));
          output.newLine();
          output.newLine();
          output.write("loop_");
          output.newLine();
          output.write("_pd_meas_position_x _pd_meas_position_y _pd_meas_intensity_total");
          output.newLine();
          for (int i = 0; i < dataNumber; i++) {
            double intensity = profile[1][ij][i];
            if (intensity > 0.0) {
              output.write(" " + Fmt.format(profile[0][ij][i]) + " " + Fmt.format(profile[1][ij][i]) + " " +
                  Fmt.format(intensity));
              output.newLine();
            }
          }
          output.newLine();
        }
      }
    } catch (IOException io) {
      io.printStackTrace();
    }

    try {
      output.close();
    } catch (IOException io) {
      io.printStackTrace();
    }
    return folderAndName;
  }
}
