/*
 * @(#)DiffractionAngleEnergyMap.java created 29/8/2018 Povo
 *
 * Copyright (c) 2018 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.diffr.diffraction;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.detector.XRFDetector;
import it.unitn.ing.rista.diffr.geometry.GeometryXRFInstrument;
import it.unitn.ing.rista.diffr.radiation.XrayEbelTubeRadiation;
import it.unitn.ing.rista.diffr.sfm.StructureFactorStandardModel;
import it.unitn.ing.rista.diffr.sizestrain.SizeStrainHarmonicTexture;
import it.unitn.ing.rista.interfaces.Peak;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.PersistentThread;

import java.io.*;
import java.lang.*;
import java.util.Vector;

import static java.lang.System.arraycopy;
import static java.lang.System.out;

/**
 * The DiffractionAngleEnergyMap is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.0 $, $Date: 2018/08/29 11:15:28 $
 * @since JDK1.8
 */

public class DiffractionAngleEnergyMap extends Diffraction {
  
  public DiffractionAngleEnergyMap(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Angle-energy map diffraction";
    IDlabel = identifier;
    description = identifier;
  }
  
  public DiffractionAngleEnergyMap(XRDcat aobj) {
    this(aobj, "Angle-energy map diffraction");
  }
  
  public DiffractionAngleEnergyMap() {
    identifier = "Angle-energy map diffraction";
    IDlabel = identifier;
    description = identifier;
  }
  
/*  public void computeDiffraction(Sample asample, DiffrDataFile datafile) {
    DataFileSet datafileset = datafile.getDataFileSet();
//      System.out.println("refreshing derivative: " + this.toXRDcatString());
    for (int ij = 0; ij < asample.phasesNumber(); ij++) {
      double expfit[] = new double[datafile.getTotalNumberOfData()];
      int minmaxindex[] = computeReflectionIntensity(asample, datafileset.getPeakList(), true,
          expfit, Constants.ENTIRE_RANGE, Constants.COMPUTED,
          Constants.COMPUTED, Constants.COMPUTED, false,
          asample.getPhase(ij), datafile);
      if (getFilePar().isComputingDerivate())
        for (int j = minmaxindex[0]; j < minmaxindex[1]; j++)
          datafile.addtoPhasesFit(j, expfit[j]);
      else
        datafile.addtoPhasesFit(expfit, minmaxindex, ij);
    }
  }
  
  public int[] computeReflectionIntensity(Sample asample, Vector<Peak> peaklist, boolean computeBroadening,
                                          double[] expfit, double rangefactor, int computeTexture,
                                          int computeStrain, int computeFhkl, boolean leBailExtraction,
                                          Phase phase, DiffrDataFile datafile) {
    
    Instrument ainstrument = datafile.getDataFileSet().getInstrument();
    FilePar filepar = getFilePar();
    OutputStream out = null;
    boolean logOutput = false;
    PrintStream printStream = null;
    ByteArrayOutputStream baos = null;
    if (filepar.logOutput() && filepar.fullResults() && !leBailExtraction) {
      out = getFilePar().getResultStream();
      logOutput = true;
      
      try {
        baos = new ByteArrayOutputStream();
        printStream = new PrintStream(baos);
        printStream.println("             Diffraction spectrum : " + datafile.toXRDcatString());
        printStream.println("Peaks list : ");
        printStream.print(" #,"
            + "rad#,"
            + "phase,"
            + "h,"
            + "k,"
            + "l,"
            + "d-space,"
            + "energy,"
            + "Fhkl_calc,"
            + "Fhkl_exp,"
            + "position,"
            + "intensity,"
            + "hwhm,"
            + "gaussian,"
            + "incident I,"
            + "LP,"
            + "texture,"
            + "Abs*Vol/Vc,"
            + "rad.wt,"
            + "phase scale,"
            + "detector abs,"
            + "strain,"
            + "planar def"
        );
        printStream.print(Constants.lineSeparator);
        printStream.flush();
//						System.out.println("String length " + toPrint.length());
      } catch (Exception io) {
        io.printStackTrace();
      }
    }

//    Instrument ainstrument = getDataFileSet().getInstrument();
    double cutoff = datafile.getDataFileSet().getPeakCutoffD() * rangefactor;
    if (!datafile.increasingX()) {
      cutoff = -cutoff;
    }
    int[] tmpminmax = new int[2];
    int[] minmaxindex = new int[2];
    minmaxindex[0] = datafile.finalindex - 1;
    minmaxindex[1] = datafile.startingindex;
    arraycopy(minmaxindex, 0, tmpminmax, 0, 2);

//    System.out.println(datafile.toXRDcatString() + " " + peaklist.size());  // todo
    for (int i = 0; i < peaklist.size(); i++) {
      if (phase == null || peaklist.elementAt(i).getPhase() == phase) {
        peaklist.elementAt(i).computePeak(datafile, expfit, asample, ainstrument, printStream, logOutput, cutoff,
            computeTexture, computeStrain, computeFhkl, leBailExtraction, tmpminmax,
            computeBroadening, !datafile.increasingX());
        if (i == 0)
          arraycopy(tmpminmax, 0, minmaxindex, 0, 2);
        else if (!leBailExtraction) {
          if (minmaxindex[0] > tmpminmax[0])
            minmaxindex[0] = tmpminmax[0];
          if (minmaxindex[1] < tmpminmax[1])
            minmaxindex[1] = tmpminmax[1];
        }
      }
    }
    
    if (logOutput && baos != null) {
      try {
        synchronized (out) {
          printLine(out, baos.toString());
          newLine(out);
          out.flush();
        }
      } catch (Exception io) {
        io.printStackTrace();
      }
    }
    
    return minmaxindex;
  }
  */
  public Peak createPeak(SizeStrainModel activeSizeStrain, double dspace, boolean dspacingbase, boolean energyDispersive,
                         double[] wavelength, double[] radweight, Reflection refl, int i) {
    return new PseudoVoigt2DPeak(dspace, dspacingbase, energyDispersive, wavelength, radweight, refl, i);
  }
  
  public void computeDiffraction(Sample asample) {
    
    final DataFileSet theDataset = getDataFileSet();
    int datafilenumber = theDataset.activedatafilesnumber();
    if (datafilenumber < 1)
      return;
    final Sample theSample = asample;
    
    final Instrument ainstrument = theDataset.getInstrument();
    final int maxThreads = Math.min(Constants.maxNumberOfThreads, datafilenumber);
    if (maxThreads > 1 && Constants.threadingGranularity >= Constants.MEDIUM_GRANULARITY) {
      if (Constants.debugThreads)
        out.println("Thread datafileset " + getLabel());
      int i;
      PersistentThread[] threads = new PersistentThread[maxThreads];
      for (i = 0; i < maxThreads; i++) {
        threads[i] = new PersistentThread(i) {
          @Override
          public void executeJob() {
            this.setPriority(7);
            int i1 = this.getJobNumberStart();
            int i2 = this.getJobNumberEnd();
  
            RadiationType radType = ainstrument.getRadiationType();
            XRFDetector detector = (XRFDetector) ainstrument.getDetector();
            int nrad = radType.getLinesCount();
            int radiationSubdivision = 1;
            int cLines = nrad;
            if (radType instanceof XrayEbelTubeRadiation)
              cLines = ((XrayEbelTubeRadiation) radType).getNumberOfCharacteristicsLines();
            int characteristicLines = cLines;
            int totalLines = nrad;
            double[] energy = new double[totalLines];
            double[] radiationWeight = new double[totalLines];
            double[] absCorrection = new double[totalLines];
  
            java.util.Vector<java.util.Vector<double[]>> energyBroadeningVector = new java.util.Vector<>(totalLines);
  
            for (int ir = 0; ir < totalLines; ir++) {
              energy[ir] = radType.getRadiationEnergy(ir);
              radiationWeight[ir] = radType.getRadiationWeigth(ir);
              double lambda = Constants.ENERGY_LAMBDA / energy[ir];
              lambda *= lambda * lambda;
              double energyInKeV = energy[ir] * 0.001;
              double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(energyInKeV);
              double detectorEfficiency = detector.computeDetectorEfficiency(energyInKeV);
              double shapeAbsorption = asample.getAbsorptionForXray(energyInKeV);
              absCorrection[ir] = detectorAbsorption * detectorEfficiency / shapeAbsorption * lambda;
//      System.out.println(energyInKeV + " " + detectorAbsorption + " " + detectorEfficiency + " " + shapeAbsorption);
              java.util.Vector<double[]> instBroadFactor_en = ainstrument.getInstrumentEnergyBroadeningAt(energyInKeV);
              energyBroadeningVector.add(instBroadFactor_en);
            }
  
            Vector<Peak> peaklist = theDataset.getPeakList();
            final double[][] betaf = new double[peaklist.size()][2];
            for (int i = 0; i < peaklist.size(); i++) {
              PseudoVoigt2DPeak peak = (PseudoVoigt2DPeak) peaklist.elementAt(i);
              Reflection refl = peak.getReflex();
              double[] sizestrain = peak.getPhase().getCrystalliteMicrostrain(refl, null);
              betaf[i][0] = peak.getPhase().getActiveSizeStrain().getBetaChauchy(refl.d_space, sizestrain[0], sizestrain[1]); // / 2.0;
              betaf[i][1] = peak.getPhase().getActiveSizeStrain().getBetaGauss(refl.d_space, sizestrain[0], sizestrain[1]); // / 2.0
            }
  
            double cut = theDataset.getPeakCutoffD(); //  * Constants.ENTIRE_RANGE;
  
            if (!theDataset.getActiveDataFile(0).increasingX()) {  // should be all the same
              cut = -cut;
            }
            double cutoff = cut;
  
            for (int j = i1; j < i2; j++) {
              DiffrDataFile datafile = theDataset.getActiveDataFile(j);
              computeReflectionIntensity(theSample, datafile, energy, radiationWeight, betaf,
                  absCorrection, totalLines, characteristicLines, energyBroadeningVector,
                  radiationSubdivision, cutoff);
              computeasymmetry(theSample, datafile);
            }
          }
        };
      }
      i = 0;
      int istep = (int) (0.9999 + datafilenumber / maxThreads);
      for (int j = 0; j < maxThreads; j++) {
        int is = i;
        if (j < maxThreads - 1)
          i = Math.min(i + istep, datafilenumber);
        else
          i = datafilenumber;
        threads[j].setJobRange(is, i);
        threads[j].start();
      }
      boolean running;
      do {
        running = false;
        try {
          Thread.sleep(Constants.timeToWaitThreadsEnding);
        } catch (InterruptedException r) {
        }
        for (int h = 0; h < maxThreads; h++) {
          if (!threads[h].isEnded())
            running = true;
        }
      } while (running);
      
    } else {
      RadiationType radType = ainstrument.getRadiationType();
      XRFDetector detector = (XRFDetector) ainstrument.getDetector();
      int nrad = radType.getLinesCount();
      int radiationSubdivision = 1;
      int cLines = nrad;
      if (radType instanceof XrayEbelTubeRadiation)
        cLines = ((XrayEbelTubeRadiation) radType).getNumberOfCharacteristicsLines();
      int characteristicLines = cLines;
      int totalLines = nrad;
      double[] energy = new double[totalLines];
      double[] radiationWeight = new double[totalLines];
      double[] absCorrection = new double[totalLines];
  
      java.util.Vector<java.util.Vector<double[]>> energyBroadeningVector = new java.util.Vector<>(totalLines);
  
      for (int ir = 0; ir < totalLines; ir++) {
        energy[ir] = radType.getRadiationEnergy(ir);
        radiationWeight[ir] = radType.getRadiationWeigth(ir);
        double E2corr = Constants.ENERGY_LAMBDA / energy[ir];
        E2corr *= E2corr;  // for deltaI/deltaE the correction is for 1/E^2 and not lambda^3, see International tables H, Stephens, p. 254
        double energyInKeV = energy[ir] * 0.001;
        double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(energyInKeV);
        double detectorEfficiency = detector.computeDetectorEfficiency(energyInKeV);
        double shapeAbsorption = asample.getAbsorptionForXray(energyInKeV);
        absCorrection[ir] = detectorAbsorption * detectorEfficiency / shapeAbsorption * E2corr;
//      System.out.println(energyInKeV + " " + detectorAbsorption + " " + detectorEfficiency + " " + shapeAbsorption);
        java.util.Vector<double[]> instBroadFactor_en = ainstrument.getInstrumentEnergyBroadeningAt(energyInKeV);
        energyBroadeningVector.add(instBroadFactor_en);
      }
      
      Vector<Peak> peaklist = theDataset.getPeakList();
      final double[][] betaf = new double[peaklist.size()][2];
      for (int i = 0; i < peaklist.size(); i++) {
        PseudoVoigt2DPeak peak = (PseudoVoigt2DPeak) peaklist.elementAt(i);
        Reflection refl = peak.getReflex();
        double[] sizestrain = peak.getPhase().getCrystalliteMicrostrain(refl, null);
        betaf[i][0] = peak.getPhase().getActiveSizeStrain().getBetaChauchy(refl.d_space, sizestrain[0], sizestrain[1]); // / 2.0;
        betaf[i][1] = peak.getPhase().getActiveSizeStrain().getBetaGauss(refl.d_space, sizestrain[0], sizestrain[1]); // / 2.0
      }
  
      double cut = theDataset.getPeakCutoffD(); //  * Constants.ENTIRE_RANGE;
  
      if (!theDataset.getActiveDataFile(0).increasingX()) {  // should be all the same
        cut = -cut;
      }
      double cutoff = cut;
  
      for (int k = 0; k < datafilenumber; k++) {
        DiffrDataFile datafile = theDataset.getActiveDataFile(k);
        computeReflectionIntensity(theSample, datafile, energy, radiationWeight, betaf,
            absCorrection, totalLines, characteristicLines, energyBroadeningVector,
            radiationSubdivision, cutoff);
        computeasymmetry(theSample, datafile);
      }
    }
    
  }
  
  public void computeReflectionIntensity(final Sample asample, final DiffrDataFile diffrDataFile, final double[] energy,
                                         final double[] radiationWeight, final double[][] betaf, final double[] absCorrection,
                                         final int totalLines, final int characteristicLines,
                                         final java.util.Vector<java.util.Vector<double[]>> energyBroadeningVector,
                                         final int radiationSubdivision, final double cutoff) {
    
    
    boolean leBailExtraction = false;
    double strainFactor = 0;
    double textureFactor = 1.0;
    
    double[] eta = new double[totalLines];
    double[] hwhm_i = new double[totalLines];
    double[][] intensity = new double[1][totalLines];
    int[] minindex = new int[totalLines];
    int[] maxindex = new int[totalLines];
    
    DataFileSet adafileset = getDataFileSet();
    Instrument ainstrument = adafileset.getInstrument();
//    RadiationType radType = ainstrument.getRadiationType();
    FilePar filepar = getFilePar();
    OutputStream out = null;
    boolean logOutput = false;
    PrintStream printStream = null;
    ByteArrayOutputStream baos = null;
    if (filepar.logOutput() && filepar.fullResults() && !leBailExtraction) {
      out = getFilePar().getResultStream();
      logOutput = true;
      
      try {
        baos = new ByteArrayOutputStream();
        printStream = new PrintStream(baos);
        printStream.println("             Diffraction spectrum : " + diffrDataFile.toXRDcatString());
        printStream.println("Peaks list : ");
        printStream.print(" #,"
            + "rad#,"
            + "phase,"
            + "h,"
            + "k,"
            + "l,"
            + "d-space,"
            + "energy,"
            + "Fhkl_calc,"
            + "position,"
            + "intensity,"
            + "hwhm,"
            + "gaussian,"
            + "incident I,"
            + "LP,"
            + "texture,"
            + "rad.wt,"
            + "phase scale,"
            + "absorption,"
            + "strain,"
            + "planar def"
        );
        printStream.print(Constants.lineSeparator);
        printStream.flush();
//						System.out.println("String length " + toPrint.length());
      } catch (Exception io) {
        io.printStackTrace();
      }
    }
  
    double intensitySingle = ainstrument.getIntensityForDiffraction();
    double twotheta = diffrDataFile.get2ThetaValue();
    double theta = twotheta / 2.0;
    double sintheta = diffrDataFile.sintheta;
    double expfit[] = new double[diffrDataFile.getTotalNumberOfData()];

//    double[] sizestrain = new double[2];
    int[] tmpminmax = new int[2];
    int[] minmaxindex = new int[2];
//    double[] betaf = new double[2];

    Geometry geometry = ainstrument.getGeometry();
    Detector detector = ainstrument.getDetector();
//    double[] incidentDiffracted = diffrDataFile.getIncidentAndDiffractionAngles(twotheta);
//    double one_sinPhii = 1.0 / Math.sin(incidentDiffracted[0]);
//    double one_sinPhid = 1.0 / Math.sin(incidentDiffracted[2]);
      double sampleLinearArea = detector.getGeometryCorrection(
          geometry.getBeamOutCorrection(diffrDataFile, asample));
    double areaCorrection = detector.getAreaCorrection(sampleLinearArea);
    double lorentzPolarization = ainstrument.getGeometry().LorentzPolarization(diffrDataFile, asample, twotheta,
        false, false);
    double texture_angles[] = diffrDataFile.getTextureAngles(diffrDataFile.get2ThetaValue());
    double alpha = (texture_angles[0] * Constants.DEGTOPI);
    double beta = (texture_angles[1] * Constants.DEGTOPI);
    
//    System.out.println(twotheta + " " + lorentzPolarization);
  
    Vector<Peak> peaklist = adafileset.getPeakList();
    
    for (int ij = 0; ij < asample.phasesNumber(); ij++) {
      for (int jj = 0; jj < expfit.length; jj++)
        expfit[jj] = 0;
      
      Phase aphase = asample.getPhase(ij);
      String phase_name = aphase.toXRDcatString();
      
      minmaxindex[0] = diffrDataFile.finalindex - 1;
      minmaxindex[1] = diffrDataFile.startingindex;
      arraycopy(minmaxindex, 0, tmpminmax, 0, 2);
  
//      boolean symModelRequireTextureAngles = aphase.getActiveSizeStrainSym().identifier == SizeStrainHarmonicTexture.modelIDstring;
      double[] betaff = new double[2];
      for (int i = 0; i < peaklist.size(); i++) {
        PseudoVoigt2DPeak peak = (PseudoVoigt2DPeak) peaklist.elementAt(i);
        if (peak.getPhase() == aphase) {
          Reflection refl = peak.getReflex();
          int reflexIndex = peak.getOrderPosition();
          double[] Fhkl = adafileset.getCalculatedStructureFactors(aphase, reflexIndex);
          double[] broadFactorForRange = PseudoVoigtPeak.getHwhmEtaFromIntegralBeta(betaf[i],
              ainstrument.getInstrumentBroadeningAt(twotheta, diffrDataFile));
          double dtheta = Math.abs(broadFactorForRange[0] * 0.5 * cutoff);
          double thetaLow = theta - dtheta;
          if (thetaLow < 0.01)
            thetaLow = 0.01;
          double thetaHigh = theta + dtheta;
          if (thetaHigh > 90.0)
            thetaHigh = 90.0;
          double sinthetaLow = sintheta / Math.sin(thetaLow * Constants.DEGTOPI);
          double sinthetaHigh = sintheta / Math.sin(thetaHigh * Constants.DEGTOPI);
          double peakEnergy = Constants.ENERGY_LAMBDA / (2.0 * refl.d_space * sintheta);
          double energy_min = peakEnergy * sinthetaHigh;
          double energy_max = peakEnergy * sinthetaLow;

/*          if (!symModelRequireTextureAngles) {
            sizestrain = aphase.getCrystalliteMicrostrain(refl, null);
            betaf[0] = aphase.getActiveSizeStrain().getBetaChauchy(refl.d_space, sizestrain[0], sizestrain[1]); // / 2.0;
            betaf[1] = aphase.getActiveSizeStrain().getBetaGauss(refl.d_space, sizestrain[0], sizestrain[1]); // / 2.0;
          }*/
  
          textureFactor = aphase.getActiveTexture().computeTextureFactor(refl, alpha, beta);
          
          int principalRad = 0;
          for (int ir = 0; ir < totalLines; ir++) {
//            System.out.println(diffrDataFile.toXRDcatString() + ": " + " " + refl.getH() + " " + refl.getK() + " " + refl.getL() + " " + peakEnergy + " - " + energy_min + " < " + energy[ir] + " < " + energy_max);
            if (energy[ir] > energy_min && energy[ir] < energy_max && radiationWeight[ir] > 0.0) {
              double position = diffrDataFile.getPositionFromDspace(refl.d_space, ir);
              position = diffrDataFile.computeFinalPosition(asample, position, energy[ir], strainFactor);
              if (position != 0) {
                intensity[0][ir] = intensitySingle * textureFactor * Fhkl[ir] * areaCorrection *
                    radiationWeight[ir] * aphase.getScaleFactor() * lorentzPolarization * absCorrection[ir];
  
                double sintheta1 = Math.sin(position * Constants.DEGTOPI * 0.5);
                sintheta1 *= sintheta1;
                double costheta = Math.cos(position * Constants.DEGTOPI * 0.5);
                double corr = 4.0 * sintheta1 / costheta * Constants.PITODEG * energy[ir] / Constants.ENERGY_LAMBDA;
                betaff[0] = betaf[i][0] * corr;
                betaff[1] = betaf[i][1] * corr;
//            double[] deff = diffrDataFile.getCrystallitesMicrostrains(aphase, reflexIndex, 0);
/*              if (symModelRequireTextureAngles) {
                sizestrain = aphase.getCrystalliteMicrostrain(refl, diffrDataFile.getTextureAngles(position));
                betaf[0] = aphase.getActiveSizeStrain().getBetaChauchy(refl.d_space, sizestrain[0], sizestrain[1]); // / 2.0;
                betaf[1] = aphase.getActiveSizeStrain().getBetaGauss(refl.d_space, sizestrain[0], sizestrain[1]); // / 2.0;
              }*/
  
                java.util.Vector<double[]> instBroadFactor = ainstrument.getInstrumentBroadeningAt(position, diffrDataFile);
                double[] broadFactorTotal = PseudoVoigtPeak.getHwhmEtaFromIntegralBeta(betaff, instBroadFactor);
                hwhm_i[ir] = 1.0 / broadFactorTotal[0];
                eta[ir] = broadFactorTotal[1];
                
                double rangeEnergy = Math.abs(energyBroadeningVector.elementAt(ir).get(0)[0] * cutoff);
                minindex[ir] = diffrDataFile.getOldNearestPoint(energy_min/*energy[ir] - rangeEnergy*/);
                maxindex[ir] = diffrDataFile.getOldNearestPoint(energy_max /*energy[ir] + rangeEnergy*/) + 1;
                
                if (!leBailExtraction || (leBailExtraction && ir == principalRad)) {
                  if (minmaxindex[0] > minindex[ir])
                    minmaxindex[0] = minindex[ir];
                  if (minmaxindex[1] < maxindex[ir])
                    minmaxindex[1] = maxindex[ir];
                }
                
                if (logOutput && printStream != null) {
                  try {
                    printStream.print(" ");
                    printStream.print(reflexIndex);
                    printStream.print(" ");
                    printStream.print(ir);
                    printStream.print(" " + phase_name);
                    printStream.print(" ");
                    printStream.print(refl.getH());
                    printStream.print(" ");
                    printStream.print(refl.getK());
                    printStream.print(" ");
                    printStream.print(refl.getL());
                    printStream.print(" ");
                    printStream.print((float) refl.d_space);
                    printStream.print(" ");
                    printStream.print((float) energy[ir]);
                    printStream.print(" ");
                    printStream.print((float) Fhkl[ir]); // diffrDataFile.getDataFileSet().getStructureFactors(aphase)[1][reflexIndex][i]);
                    printStream.print(" ");
                    printStream.print((float) position);
                    printStream.print(" ");
                    printStream.print((float) intensity[0][ir]);
                    printStream.print(" ");
                    printStream.print((float) broadFactorTotal[0]);
                    printStream.print(" ");
                    printStream.print((float) eta[ir]);
                    printStream.print(" ");
                    printStream.print((float) intensitySingle);
                    printStream.print(" ");
                    printStream.print((float) lorentzPolarization);
                    printStream.print(" ");
                    printStream.print((float) textureFactor);
                    printStream.print(" ");
                    printStream.print((float) radiationWeight[ir]);
                    printStream.print(" ");
                    printStream.print((float) aphase.getScaleFactor());
                    printStream.print(" ");
                    printStream.print((float) absCorrection[ir]);
                    printStream.print(" ");
                    printStream.print((float) strainFactor);
                    printStream.print(" ");
                    printStream.print((float) refl.getPlanarDefectDisplacement(0));
                    
                    printStream.print(Constants.lineSeparator);
                    printStream.flush();
                  } catch (Exception e) {
                    e.printStackTrace();
                  }
                }
              } else {
                intensity[0][ir] = 0.0f;
                hwhm_i[ir] = 1.0f;
                eta[ir] = 0.0f;
                minindex[ir] = 0;
                maxindex[ir] = 0;
                energyBroadeningVector.add(null);
              }
              
            } else {
              intensity[0][ir] = 0.0f;
              hwhm_i[ir] = 1.0f;
              eta[ir] = 0.0f;
              minindex[ir] = 0;
              maxindex[ir] = 0;
              energyBroadeningVector.add(null);
            }
          }
          
/*          if (radiationSubdivision > 1)
            peak.computeFunctions(diffrDataFile.getXData(), expfit, minindex, maxindex,
                intensity, eta, hwhm_i, null, null, null, energy,
                diffrDataFile.dspacingbase, diffrDataFile.energyDispersive, diffrDataFile.increasingX(), 0,
                0, diffrDataFile.sintheta, energyBroadeningVector, refl.d_space, radiationSubdivision, characteristicLines);
          else*/
            peak.computeFunctionsQuick(diffrDataFile.getXData(), expfit, minindex, maxindex,
                intensity, eta, hwhm_i, energy, diffrDataFile.sintheta, energyBroadeningVector, refl.d_space);
          
          
        }
        if (!leBailExtraction) {
          if (minmaxindex[0] > tmpminmax[0])
            minmaxindex[0] = tmpminmax[0];
          if (minmaxindex[1] < tmpminmax[1])
            minmaxindex[1] = tmpminmax[1];
        }
      }
      
      if (filepar.isComputingDerivate())
        for (int j = minmaxindex[0]; j < minmaxindex[1]; j++)
          diffrDataFile.addtoPhasesFit(j, expfit[j]);
      else
        diffrDataFile.addtoPhasesFit(expfit, minmaxindex, ij);
    }
    if (logOutput && baos != null) {
      try {
        synchronized (out) {
          printLine(out, baos.toString());
          newLine(out);
          out.flush();
        }
      } catch (Exception io) {
        io.printStackTrace();
      }
    }
    
  }
  
}
