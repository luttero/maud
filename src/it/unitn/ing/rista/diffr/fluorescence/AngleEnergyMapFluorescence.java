/*
 * @(#)AngleEnergyMapFluorescence.java created Jan 13, 2020 Povo
 *
 * Copyright (c) 2020 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.fluorescence;

import it.unitn.ing.rista.chemistry.AtomInfo;
import it.unitn.ing.rista.chemistry.XRayDataSqLite;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.detector.XRFDetector;
import it.unitn.ing.rista.diffr.radiation.XrayEbelTubeRadiation;
import it.unitn.ing.rista.util.*;

import java.util.*;

import static java.lang.System.out;
import static org.apache.commons.math3.special.Erf.erfc;

/**
 * The AngleEnergyMapFluorescence is a class optimized to compute fluorescence
 * lines in the angle-energy map fitting
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jan 13, 2020 4:31:01 PM $
 * @since JDK1.1
 */

public class AngleEnergyMapFluorescence extends Fluorescence {
  
  public static String modelID = "Angle-Energy XRF";
  private static String descriptionID = "Perform quantitative fitting of XRF data in angle-energy maps";
  
  public AngleEnergyMapFluorescence(XRDcat obj, String alabel) {
    super(obj, alabel);
    identifier = modelID;
    IDlabel = modelID;
    description = descriptionID;
  }
  
  public AngleEnergyMapFluorescence(XRDcat afile) {
    this(afile, modelID);
  }
  
  public AngleEnergyMapFluorescence() {
    identifier = modelID;
    IDlabel = modelID;
    description = descriptionID;
  }
  
  public void computeFluorescence(Sample asample) {
  
    long previousTime = System.currentTimeMillis();
  
    final DataFileSet theDataset = getDataFileSet();
    int datafilenumber = theDataset.activedatafilesnumber();

    Instrument ainstrument = theDataset.getInstrument();
    XRFDetector detector = (XRFDetector) ainstrument.getDetector();
    Hashtable<Integer, Vector<AtomQuantity>> atomQuantities = getAllAtomsQuantities(asample);
    RadiationType radType = ainstrument.getRadiationType();
    int rad_lines = radType.getLinesCountForFluorescence();
    double[] energyInKeV = new double[rad_lines];
    double[] energy_intensity = new double[rad_lines];
  
    for (int ej = 0; ej < rad_lines; ej++) {
      energyInKeV[ej] = radType.getRadiationEnergyForFluorescenceKeV(ej);
      energy_intensity[ej] = radType.getRadiationWeightForFluorescence(ej);
    }
//		int sub20 = radType.getSubdivision(); //MaudPreferences.getInteger("xrf_detector.energySubdivision", 20);
  
    int initialContent = rad_lines;
  
    Vector<FluorescenceLine> impurityLines = new Vector<>(initialContent, initialContent);
    if (((XRFDetector) ainstrument.getDetector()).getFiltersFluorescenceIntensityTotal() > 0) {
      for (int ej = 0; ej < rad_lines; ej++) {
        Vector<FluorescenceLine> filtersFluorescenceLines = ((XRFDetector) ainstrument.getDetector()).getFluorescenceLines(energyInKeV[ej]);
        for (FluorescenceLine line : filtersFluorescenceLines) {
          double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(line.getEnergy());
          double detectorEfficiency = detector.computeDetectorEfficiency(line.getEnergy());
          line.setIntensity(line.getIntensity() * detectorAbsorption * detectorEfficiency);
          boolean addLine = true;
          for (int i = 0; i < impurityLines.size() && addLine; i++) {
            FluorescenceLine lineExisting = impurityLines.get(i);
//						lineExisting.setIntensity(lineExisting.getIntensity());
            if (lineExisting.getEnergy() == line.getEnergy()) {
              addLine = false;
              lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity() * energy_intensity[ej]);
            }
          }
          if (addLine && line.getIntensity() > 0) {
            line.setIntensity(line.getIntensity() * energy_intensity[ej]);
            impurityLines.add(line);
          }
        }
      }
    }
    if (ainstrument.getRadiationType() instanceof XrayEbelTubeRadiation) {
      XrayEbelTubeRadiation source = (XrayEbelTubeRadiation) ainstrument.getRadiationType();
      if (source.getFiltersFluorescenceIntensityTotal() > 0) {
        for (int ej = 0; ej < rad_lines; ej++) {
          Vector<FluorescenceLine> filtersFluorescenceLines = source.getFluorescenceLines(energyInKeV[ej]);
          for (FluorescenceLine line : filtersFluorescenceLines) {
            double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(line.getEnergy());
            double detectorEfficiency = detector.computeDetectorEfficiency(line.getEnergy());
            line.setIntensity(line.getIntensity() * detectorAbsorption * detectorEfficiency);
            boolean addLine = true;
            for (FluorescenceLine lineExisting : impurityLines) {
              //							lineExisting.setIntensity(lineExisting.getIntensity());
              if (lineExisting.getEnergy() == line.getEnergy()) {
                addLine = false;
                lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity() * energy_intensity[ej]);
              }
            }
            if (addLine) {
              line.setIntensity(line.getIntensity() * energy_intensity[ej]);
              if (line.getIntensity() > 0) {
                line.areaCorrection = true;
                impurityLines.add(line);
              }
            }
          }
        }
      }
    }
    for (FluorescenceLine aLine : impurityLines) {
//      aLine.setMhuDet(detector.computeMACForLineWithEnergy(aLine.getEnergy()));
      java.util.Vector<double[]> broad = ainstrument.getInstrumentEnergyBroadeningAt(aLine.getEnergy());
      aLine.setShape(broad);
    }
  
    if (Constants.testtime)
      System.out.println("Fluorescence lines - get all lines: " +
          (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");
  
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
  
            Hashtable<Integer, Vector<FluorescenceLine>> fluoLines = getAllAtomsFluorescenceLines(asample,
                energyInKeV, energy_intensity);
            for (Integer key : fluoLines.keySet()) {
              Vector<FluorescenceLine> linesForAtom = fluoLines.get(key);
              for (FluorescenceLine aLine : linesForAtom) {
                double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(aLine.getEnergy());
                double detectorEfficiency = detector.computeDetectorEfficiency(aLine.getEnergy());
                aLine.multiplyIntensityBy(detectorAbsorption * detectorEfficiency);
//                aLine.setMhuDet(detector.computeMACForLineWithEnergy(aLine.getEnergy()));
                java.util.Vector<double[]> broad = ainstrument.getInstrumentEnergyBroadeningAt(aLine.getEnergy());
                aLine.setShape(broad);
              }
            }
  
            for (int j = i1; j < i2; j++) {
              DiffrDataFile datafile = theDataset.getActiveDataFile(j);
              computeFluorescence(asample, datafile, fluoLines, atomQuantities, impurityLines/*,
              energyInKeV, energy_intensity*/);
              computeasymmetry(asample, datafile);
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
      Hashtable<Integer, Vector<FluorescenceLine>> fluoLines = getAllAtomsFluorescenceLines(asample,
          energyInKeV, energy_intensity);
      for (Integer key : fluoLines.keySet()) {
        Vector<FluorescenceLine> linesForAtom = fluoLines.get(key);
        for (FluorescenceLine aLine : linesForAtom) {
          double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(aLine.getEnergy());
          double detectorEfficiency = detector.computeDetectorEfficiency(aLine.getEnergy());
          aLine.multiplyIntensityBy(detectorAbsorption * detectorEfficiency);
//          aLine.setMhuDet(detector.computeMACForLineWithEnergy(aLine.getEnergy()));
          java.util.Vector<double[]> broad = ainstrument.getInstrumentEnergyBroadeningAt(aLine.getEnergy());
          aLine.setShape(broad);
        }
      }
  
      for (int k = 0; k < datafilenumber; k++) {
        DiffrDataFile datafile = theDataset.getActiveDataFile(k);
        computeFluorescence(asample, datafile, fluoLines, atomQuantities, impurityLines/*,
        energyInKeV, energy_intensity*/);
        computeasymmetry(asample, datafile);
      }
    }
    if (Constants.testtime)
      System.out.println("Fluorescence lines - spectra calculation: " +
          (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");
  
  }
  
  public Hashtable<Integer, Vector<FluorescenceLine>> getAllAtomsFluorescenceLines(Sample asample,
                                                                                   double[] energyInKeV,
                                                                                   double[] energy_intensity) {
    DataFileSet adataset = (DataFileSet) getParent();
    double maxEnergyInKeV = adataset.getLargestCoordinate() * 0.0011;
    double minEnergyInKeV = adataset.getSmallestCoordinate() * 0.0009;
//    System.out.println("Max energy " + maxEnergyInKeV);
    Hashtable<Integer, Vector<FluorescenceLine>> linesMap = new Hashtable<Integer, Vector<FluorescenceLine>>();
    for (int i = 0; i < asample.phasesNumber(); i++) {
      Vector<AtomQuantity> composition = asample.getPhase(i).getChemicalComposition();
      for (AtomQuantity atomQuantity : composition) {
        double atomsQuantities = atomQuantity.quantity_weight;
//				System.out.println(atomQuantity.label + " " + atomsQuantities + " " + atomQuantity.quantity);
        if (atomsQuantities > 0) {
          Integer atomNumber = AtomInfo.retrieveAtomNumber(atomQuantity.label);
          if (!linesMap.containsKey(atomNumber)) {
            Vector<FluorescenceLine> linesForAtom = XRayDataSqLite.getFluorescenceLinesFor(atomNumber,
                minEnergyInKeV, maxEnergyInKeV, energyInKeV, energy_intensity);
            linesMap.put(atomNumber, linesForAtom);
          }
        }
      }
    }
    return linesMap;
  }
  
  public Hashtable<Integer, Vector<FluorescenceLine>> getAllAtomsFluorescenceLines(Sample asample, DiffrDataFile datafile) {
    double maxEnergyInKeV = datafile.getLargestCoordinate() * 0.0011;
    double minEnergyInKeV = datafile.getSmallestCoordinate() * 0.0009;
//    System.out.println("Max energy " + maxEnergyInKeV);
    Hashtable<Integer, Vector<FluorescenceLine>> linesMap = new Hashtable<Integer, Vector<FluorescenceLine>>();
    for (int i = 0; i < asample.phasesNumber(); i++) {
      Vector<AtomQuantity> composition = asample.getPhase(i).getChemicalComposition();
      for (AtomQuantity atomQuantity : composition) {
        double atomsQuantities = atomQuantity.quantity_weight;
//				System.out.println(atomQuantity.label + " " + atomsQuantities + " " + atomQuantity.quantity);
        if (atomsQuantities > 0) {
          Integer atomNumber = AtomInfo.retrieveAtomNumber(atomQuantity.label);
          if (!linesMap.containsKey(atomNumber)) {
            Vector<FluorescenceLine> linesForAtom = XRayDataSqLite.getFluorescenceLinesNoSensitivityFor(atomNumber, maxEnergyInKeV, minEnergyInKeV);
            linesMap.put(atomNumber, linesForAtom);
          }
        }
      }
    }
    return linesMap;
  }
  
  public Hashtable<Integer, Vector<AtomQuantity>> getAllAtomsQuantities(Sample asample) {
    Hashtable<Integer, Vector<AtomQuantity>> linesMap = new Hashtable<Integer, Vector<AtomQuantity>>();
    for (int j1 = 0; j1 < asample.numberOfLayers; j1++) {
      Layer layer = asample.getlayer(j1);
      Vector<AtomQuantity> chemicalComposition = layer.getChemicalComposition();
      linesMap.put(j1, chemicalComposition);
    }
    return linesMap;
  }
  
  /**
   * The method compute the fluorescence pattern using the
   * fluorescence model by De Boer.
   * D. K. G. de Boer, Phys. Review B, 44[2], 498, 1991.
   * It uses the ElamDB. When the pattern is computed and it is added to the
   * <code>DiffrDataFile</code> using the addtoFit method.
   *
   * @param adatafile
   * @see DiffrDataFile#addtoFit
   */
  
  public void computeFluorescence(Sample asample, DiffrDataFile adatafile, Hashtable<Integer,
      Vector<FluorescenceLine>> fluoLines, Hashtable<Integer, Vector<AtomQuantity>> atomQuantities,
                                  Vector<FluorescenceLine> impurityLines/*, double[] energyInKeV,
                                      double[] energy_intensity*/) {
    
    XRayDataSqLite.checkMinimumEnergy();

//		boolean checkSensitivity = MaudPreferences.getBoolean("xrf.sensitivityNoEnergy", false);
    Instrument ainstrument = adatafile.getDataFileSet().getInstrument();
    XRFDetector detector = (XRFDetector) ainstrument.getDetector();
    Geometry geometry = ainstrument.getGeometry();
    double incidentIntensity = ainstrument.getIntensityForFluorescence();
    double sampleLinearArea = detector.getGeometryCorrection(
        geometry.getBeamOutCorrection(adatafile, asample));
//		incidentIntensity *= sampleLinearArea;

//		double polarization = ainstrument.getGeometry().getPolarizationAmount();
//		double polarizationAngle = ainstrument.getGeometry().getPolarizationAngle();
//		double cos2polarization = MoreMath.cosd(polarizationAngle);
//		cos2polarization *= cos2polarization;
//		double s_factor = 0.5 - 0.5 * polarization * (1.0 - cos2polarization);
//		double p_factor = 0.5 - 0.5 * polarization * cos2polarization;
    
    double[] xEnergy = adatafile.getXrangeInEnergy();
    int channelZero = adatafile.getChannelForZero();
    double channelStep = adatafile.getChannelStep();
    int numberOfPoints = xEnergy.length;
//    double maxEnergyInKeV = xEnergy[numberOfPoints - 1] * 0.001 * 1.1;
    //double minEnergyInKeV = xEnergy[0] * 0.001 / 1.1;
//    System.out.println(xEnergy[0] + " " + xEnergy[numberOfPoints - 1]);
    
    int layersNumber = asample.numberOfLayers;
  
    double areaCorrection = detector.getAreaCorrection(sampleLinearArea);

    double[] fluorescence = new double[numberOfPoints];
  
    for (FluorescenceLine line: impurityLines) {
      double corr = 1.0;
      if (line.areaCorrection)
        corr = areaCorrection;
      for (int i = 0; i < numberOfPoints; i++/*, hi++*/) {
        fluorescence[i] += line.getIntensity(xEnergy[i]) * corr;
      }
    }
  
    double twothetadetector = adatafile.get2ThetaValue();
    double[] incidentDiffracted = adatafile.getIncidentAndDiffractionAngles(twothetadetector);
//		System.out.println(adatafile.getLabel() + ", incident beam angle: " + incidentDiffracted[0] * Constants.PITODEG + ", exiting beam angle: " + incidentDiffracted[2] * Constants.PITODEG + " " + adatafile.getTiltingAngle()[4]);
//	  incidentDiffracted[0] *= Constants.DEGTOPI;

//		double cosPhi2 = Math.cos(incidentDiffracted[0]);
    double sinPhii = 1.0 / Math.sin(incidentDiffracted[0]);
    double sinPhid = 1.0 / Math.sin(incidentDiffracted[2]);
    areaCorrection *= sinPhii + sinPhid;
    
    RadiationType radType = ainstrument.getRadiationType();
    int rad_lines = radType.getLinesCountForFluorescence();
    double[] energyInKeV = new double[rad_lines];
    double[] energy_intensity = new double[rad_lines];
    
    double[][] layerAbsorption = new double[layersNumber][rad_lines];
    double[][] overLayerAbsorption = new double[layersNumber][rad_lines];
    double[] layerDensity = new double[layersNumber];
    double[] layerThickness = new double[layersNumber];
    for (int j1 = 0; j1 < layersNumber; j1++) {
      Layer layer = asample.getlayer(j1);
      layerDensity[j1] = layer.getDensity();
      layerThickness[j1] = layer.getThicknessInCm();
    }
    for (int ej = 0; ej < rad_lines; ej++) {
      energyInKeV[ej] = radType.getRadiationEnergyForFluorescenceKeV(ej);
      energy_intensity[ej] = radType.getRadiationWeightForFluorescence(ej);
      layerAbsorption[0][ej] = -asample.getlayer(0).getAbsorptionForXray(energyInKeV[ej]) * layerDensity[0] * sinPhii;
      overLayerAbsorption[0][ej] = 0;
      for (int j1 = 1; j1 < layersNumber; j1++) {
        layerAbsorption[j1][ej] = -asample.getlayer(j1).getAbsorptionForXray(energyInKeV[ej]) * layerDensity[j1] * sinPhii;
        overLayerAbsorption[j1][ej] = overLayerAbsorption[j1 - 1][ej] + layerAbsorption[j1 - 1][ej] * layerThickness[j1 - 1];
//				System.out.println(overLayerAbsorption[j1][ej]);
      }
    }
//		int sub20 = radType.getSubdivision(); //MaudPreferences.getInteger("xrf_detector.energySubdivision", 20);
    
    Vector<FluorescenceLine> linesForAtom;
    int initialContent = rad_lines;
    Vector<FluorescenceLine> fluorescenceLines = new Vector<>(initialContent, initialContent);
    for (int j1 = 0; j1 < layersNumber; j1++) {
//      Layer layer = asample.getlayer(j1);
      Vector<AtomQuantity> chemicalComposition = atomQuantities.get(j1); //layer.getChemicalComposition();
      for (AtomQuantity atomQuantity : chemicalComposition) {
        double atomsQuantities = atomQuantity.quantity_weight;
//				System.out.println(atomQuantity.label + " " + atomsQuantities + " " + atomQuantity.quantity);
        if (atomsQuantities > 0) {
          int atomNumber = AtomInfo.retrieveAtomNumber(atomQuantity.label);
          linesForAtom = fluoLines.get(atomNumber); // XRayDataSqLite.getFluorescenceLinesNoSensitivityFor(atomNumber, maxEnergyInKeV);
          
          for (int ij = 0; ij < linesForAtom.size(); ij++) {
            FluorescenceLine line = linesForAtom.elementAt(ij);
            if (line.getFluorescenceYield() * line.getTransitionProbability() > 0) {
              double lineEnergyKeV = line.getEnergy(); // in KeV
//						System.out.println("Line: " + line.transitionID + " " + lineEnergyKeV + " " + line.getIntensity());
              double lineInnerShellEnergyKeV = line.getCoreShellEnergy(); // in KeV
              double overLayerAbsorptionForLine = 0;
              for (int j2 = 0; j2 < j1; j2++) {
                double actualLayerAbs = -asample.getlayer(j2).getAbsorptionForXray(lineEnergyKeV) * layerDensity[j2] * sinPhid;
                overLayerAbsorptionForLine += actualLayerAbs * layerThickness[j2];
              }
              double actualLayerAbsorption = -asample.getlayer(j1).getAbsorptionForXray(lineEnergyKeV) * layerDensity[j1] * sinPhid;
//						System.out.println(actualLayerAbsorption + " " + asample.getlayer(j1).getAbsorption(lineEnergyKeV) + " " + layerDensity[j1] + " " + sinPhid);
              double totalIntensity = 0;
              for (int ej = 0; ej < rad_lines; ej++) {
                if (energyInKeV[ej] > lineInnerShellEnergyKeV && lineEnergyKeV <= energyInKeV[ej]) {
                  double over_abs = overLayerAbsorptionForLine + overLayerAbsorption[j1][ej];
                  if (!Double.isNaN(over_abs)) {
                    if (over_abs > -Double.MAX_EXPONENT / 2 && over_abs < Double.MAX_EXPONENT / 2)
                      over_abs = Math.exp(over_abs);
                    else if (over_abs > 0)
                      over_abs = 1.0;
                    else
                      over_abs = 0;
                  } else
                    over_abs = 0;
      
                  double ab = actualLayerAbsorption + layerAbsorption[j1][ej];
                  double abs = ab * layerThickness[j1];
                  if (!Double.isNaN(abs) && abs != 0) {
                    if (abs > -Double.MAX_EXPONENT / 2 && abs < Double.MAX_EXPONENT / 2)
                      abs = -(1.0 - Math.exp(abs)) / ab;
                    else if (ab != 0)
                      abs = -1.0 / ab;
                    else
                      abs = 1.0;
                  } else
                    abs = 0;
      
       //           double lineSensitivity = XRayDataSqLite.getSensitivity(atomNumber, line.getCoreShellID(),
       //               line.xrl_line_number, energyInKeV[ej], line.getFluorescenceYield() * line.getTransitionProbability()); // / (line.getFluorescenceYield() * line.getTransitionProbability());
//								if (atomNumber > 80 && line.transitionID.startsWith("M"))
//									System.out.println(atomNumber - 1 + " " + lineEnergyKeV + " " + line.transitionID + " " + lineSensitivity + " " + energyInKeV[ej]
//										+ " " + line.getCoreShellID() + " " + XRayDataSqLite.getTauShell(atomNumber - 1, line.getCoreShellID(), energyInKeV[ej]));
//                  System.out.println(line.getFluorescenceYield() + " " + line.getTransitionProbability() + " " + actualLayerAbsorption + " " + layerAbsorption[j1][ej] + " " + lineSensitivity + " " + over_abs + " " + abs + " " + energy_intensity[ej]);
                  totalIntensity += over_abs * abs * line.getIntensityMultiple(ej);
                }
              }
              totalIntensity *= layerDensity[j1];
//              double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(lineEnergyKeV);
//              double detectorEfficiency = detector.computeDetectorEfficiency(lineEnergyKeV);
//						if (lineEnergyKeV * 1000 > xEnergy[0] && lineEnergyKeV * 1000 < xEnergy[numberOfPoints - 1])
//						System.out.println("Line: " + lineEnergyKeV + " " + line.getIntensity() + " " + atomsQuantities + " " + totalIntensity + " " + detectorAbsorption + " " +
//								detectorEfficiency + " " + areaCorrection + " " + getIntensityCorrection(atomNumber));
              double factor = atomsQuantities * totalIntensity * areaCorrection; // * detectorEfficiency * detectorAbsorption; // * getIntensityCorrection(atomNumber);
//						  System.out.println("Line: " + lineEnergyKeV + " " + line.getIntensity() + " " + factor + " " + totalIntensity + " " + totalIntensity1 + " " + (line.getIntensity() * totalIntensity));
//             System.out.println("Line: " + line.transitionID + " " + lineEnergyKeV + " " + line.getIntensity() + " " + atomsQuantities + " " + totalIntensity + " " + detectorAbsorption + " " +
//                  detectorEfficiency + " " + areaCorrection);
//						System.out.println(line.transitionID + " " + line.getIntensity() + " " + lineEnergyKeV);
              boolean addLine = true;
              if (j1 > 0) {
                for (int i = 0; i < fluorescenceLines.size() && addLine; i++) {
                  FluorescenceLine lineExisting = fluorescenceLines.get(i);
                  if (lineExisting == line) {
                    addLine = false;
//                    System.out.println(lineExisting.transitionID + " " + line.transitionID + " " + (lineExisting.getIntensity() + line.getIntensity()) + " " + lineExisting.getEnergy());
                    lineExisting.setIntensity(lineExisting.getIntensity() + factor);
                  }
                }
              }
              if (addLine) {
                line.setIntensity(factor);
                fluorescenceLines.add(line);
              }
            }
          }
        }
      }
    }
/*
    if (((XRFDetector) ainstrument.getDetector()).getFiltersFluorescenceIntensityTotal() > 0) {
      for (int ej = 0; ej < rad_lines; ej++) {
        Vector<FluorescenceLine> filtersFluorescenceLines = ((XRFDetector) ainstrument.getDetector()).getFluorescenceLines(energyInKeV[ej]);
        for (FluorescenceLine line : filtersFluorescenceLines) {
          double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(line.getEnergy());
          double detectorEfficiency = detector.computeDetectorEfficiency(line.getEnergy());
          line.setIntensity(line.getIntensity() * detectorAbsorption * detectorEfficiency);
          boolean addLine = true;
          for (int i = 0; i < fluorescenceLines.size() && addLine; i++) {
            FluorescenceLine lineExisting = fluorescenceLines.get(i);
//						lineExisting.setIntensity(lineExisting.getIntensity());
            if (lineExisting.getEnergy() == line.getEnergy()) {
              addLine = false;
              lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity() * energy_intensity[ej]);
            }
          }
          if (addLine && line.getIntensity() > 0) {
            line.setIntensity(line.getIntensity() * energy_intensity[ej]);
            java.util.Vector<double[]> broad = ainstrument.getInstrumentEnergyBroadeningAt(line.getEnergy());
            line.setShape(broad);
            fluorescenceLines.add(line);
          }
        }
      }
    }
    if (ainstrument.getRadiationType() instanceof XrayEbelTubeRadiation) {
      XrayEbelTubeRadiation source = (XrayEbelTubeRadiation) ainstrument.getRadiationType();
      if (source.getFiltersFluorescenceIntensityTotal() > 0) {
        for (int ej = 0; ej < rad_lines; ej++) {
          Vector<FluorescenceLine> filtersFluorescenceLines = source.getFluorescenceLines(energyInKeV[ej]);
          for (FluorescenceLine line : filtersFluorescenceLines) {
            double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(line.getEnergy());
            double detectorEfficiency = detector.computeDetectorEfficiency(line.getEnergy());
            line.setIntensity(line.getIntensity() * detectorAbsorption * detectorEfficiency * areaCorrection);
            boolean addLine = true;
            for (FluorescenceLine lineExisting : fluorescenceLines) {
              //							lineExisting.setIntensity(lineExisting.getIntensity());
              if (lineExisting.getEnergy() == line.getEnergy()) {
                addLine = false;
                lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity() * energy_intensity[ej]);
              }
            }
            if (addLine) {
              line.setIntensity(line.getIntensity() * energy_intensity[ej]);
              if (line.getIntensity() > 0) {
                java.util.Vector<double[]> broad = ainstrument.getInstrumentEnergyBroadeningAt(line.getEnergy());
                line.setShape(broad);
                fluorescenceLines.add(line);
              }
            }
          }
        }
      }
    }
*/
//		System.out.println("Compute fluo peaks");
    for (FluorescenceLine line : fluorescenceLines) {
//      java.util.Vector<double[]> broad = ainstrument.getInstrumentEnergyBroadeningAt(line.getEnergy());
//      line.setShape(broad);
//      System.out.println("Line: " + line.transitionID + " " + line.getEnergy() + " " + line.getIntensity());
      for (int i = 0; i < numberOfPoints; i++) {
        fluorescence[i] += line.getIntensity(xEnergy[i]);
      }
    }
    
    // escape pekas
    
    double[] escFluorescence = new double[numberOfPoints];

//		if (channelZero >= 0)
//			System.out.println("Channel zero: " + channelZero + " " + adatafile.getXData(channelZero));
    for (int i = 0; i < numberOfPoints; i++)
      escFluorescence[i] = fluorescence[i];
    
    double escapePeak = Math.abs(detector.getEscapePeaksIntensity());
    if (escapePeak > 0) {
      Vector<double[]> escapeIntensitiesAndEnergies = detector.getEscapeIntensity(xEnergy);
      int numberLines = escapeIntensitiesAndEnergies.size() - 1;
      double[] deltaEnergies = escapeIntensitiesAndEnergies.get(numberLines);
//			System.out.println("N lines: " + numberLines);
      for (int l = 0; l < numberLines; l++) {
        int deltaChannel = (int) (deltaEnergies[l] / channelStep * 1000);
//				System.out.println("Line: " + l+ ", delta E = " + deltaEnergies[l] + ", channels = " + deltaChannel);
        double[] relativeIntensities = escapeIntensitiesAndEnergies.get(l);
        for (int i = 0; i < numberOfPoints - deltaChannel; i++) {
          escFluorescence[i] += escapePeak * relativeIntensities[i + deltaChannel] *
              fluorescence[i + deltaChannel];
        }
      }
    }
    
    // sum peaks
    
    for (int i = 0; i < numberOfPoints; i++)
      fluorescence[i] = escFluorescence[i];
    
    double sumPeak = Math.abs(detector.getSumPeaksIntensity());
    if (sumPeak > 0) {
      for (int i = 0; i < numberOfPoints; i++) {
        for (int j = i; j < numberOfPoints; j++) {
          int channel = i + j - channelZero + adatafile.startingindex;
          if (channel >= 0 && channel < numberOfPoints)
            fluorescence[channel] += sumPeak * (escFluorescence[i] * escFluorescence[j]);
        }
      }
    }
    
    for (int i = 0; i < numberOfPoints; i++) {
      fluorescence[i] *= incidentIntensity;
      adatafile.addtoFit(i, fluorescence[i]);
//        System.out.println("Point: " + xEnergy[i] + ", intensity: " + fluorescence[i]);
    }
  }
  
  public void computeFluorescence_alt(Sample asample) {
    
    long previousTime = System.currentTimeMillis();
    
    final DataFileSet theDataset = getDataFileSet();
    int datafilenumber = theDataset.activedatafilesnumber();
    Instrument ainstrument = theDataset.getInstrument();
    XRFDetector detector = (XRFDetector) ainstrument.getDetector();
  
    RadiationType radType = ainstrument.getRadiationType();
    int rad_lines = radType.getLinesCountForFluorescence();
    double[] energyInKeV = new double[rad_lines];
    double[] energy_intensity = new double[rad_lines];

    for (int ej = 0; ej < rad_lines; ej++) {
      energyInKeV[ej] = radType.getRadiationEnergyForFluorescenceKeV(ej);
      energy_intensity[ej] = radType.getRadiationWeightForFluorescence(ej);
/*      layerAbsorption[0][ej] = -asample.getlayer(0).getAbsorptionForXray(energyInKeV[ej]) * layerDensity[0] * sinPhii;
      overLayerAbsorption[0][ej] = 0;
      for (int j1 = 1; j1 < layersNumber; j1++) {
        layerAbsorption[j1][ej] = -asample.getlayer(j1).getAbsorptionForXray(energyInKeV[ej]) * layerDensity[j1] * sinPhii;
        overLayerAbsorption[j1][ej] = overLayerAbsorption[j1 - 1][ej] + layerAbsorption[j1 - 1][ej] * layerThickness[j1 - 1];
//				System.out.println(overLayerAbsorption[j1][ej]);
      }*/
    }
//		int sub20 = radType.getSubdivision(); //MaudPreferences.getInteger("xrf_detector.energySubdivision", 20);
  
    int initialContent = rad_lines;
    
    Vector<FluorescenceLine> impurityLines = new Vector<>(initialContent, initialContent);
    if (((XRFDetector) ainstrument.getDetector()).getFiltersFluorescenceIntensityTotal() > 0) {
      for (int ej = 0; ej < rad_lines; ej++) {
        Vector<FluorescenceLine> filtersFluorescenceLines = ((XRFDetector) ainstrument.getDetector()).getFluorescenceLines(energyInKeV[ej]);
        for (FluorescenceLine line : filtersFluorescenceLines) {
          double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(line.getEnergy());
          double detectorEfficiency = detector.computeDetectorEfficiency(line.getEnergy());
          line.setIntensity(line.getIntensity() * detectorAbsorption * detectorEfficiency);
          boolean addLine = true;
          for (int i = 0; i < impurityLines.size() && addLine; i++) {
            FluorescenceLine lineExisting = impurityLines.get(i);
//						lineExisting.setIntensity(lineExisting.getIntensity());
            if (lineExisting.getEnergy() == line.getEnergy()) {
              addLine = false;
              lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity() * energy_intensity[ej]);
            }
          }
          if (addLine && line.getIntensity() > 0) {
            line.setIntensity(line.getIntensity() * energy_intensity[ej]);
            impurityLines.add(line);
          }
        }
      }
    }
    if (ainstrument.getRadiationType() instanceof XrayEbelTubeRadiation) {
      XrayEbelTubeRadiation source = (XrayEbelTubeRadiation) ainstrument.getRadiationType();
      if (source.getFiltersFluorescenceIntensityTotal() > 0) {
        for (int ej = 0; ej < rad_lines; ej++) {
          Vector<FluorescenceLine> filtersFluorescenceLines = source.getFluorescenceLines(energyInKeV[ej]);
          for (FluorescenceLine line : filtersFluorescenceLines) {
            double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(line.getEnergy());
            double detectorEfficiency = detector.computeDetectorEfficiency(line.getEnergy());
            double areaCorrection = 1.0; // detector.getAreaCorrection(sampleLinearArea);
            line.setIntensity(line.getIntensity() * detectorAbsorption * detectorEfficiency * areaCorrection);
            boolean addLine = true;
            for (FluorescenceLine lineExisting : impurityLines) {
              //							lineExisting.setIntensity(lineExisting.getIntensity());
              if (lineExisting.getEnergy() == line.getEnergy()) {
                addLine = false;
                lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity() * energy_intensity[ej]);
              }
            }
            if (addLine) {
              line.setIntensity(line.getIntensity() * energy_intensity[ej]);
              if (line.getIntensity() > 0) {
                impurityLines.add(line);
              }
            }
          }
        }
      }
    }
  
  
  
    Hashtable<Integer, Vector<AtomQuantity>> atomQuantities = getAllAtomsQuantities(asample);
    if (Constants.testtime)
      System.out.println("Fluorescence lines - atoms retrieval: " +
          (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");
    
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
            int i1 = this.getJobNumberStart();
            int i2 = this.getJobNumberEnd();
  
            for (int j = i1; j < i2; j++) {
              DiffrDataFile datafile = theDataset.getActiveDataFile(j);
              computeFluorescence_alt(asample, datafile, atomQuantities, impurityLines);
              computeasymmetry(asample, datafile);
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
      for (int k = 0; k < datafilenumber; k++) {
        DiffrDataFile datafile = theDataset.getActiveDataFile(k);
        computeFluorescence_alt(asample, datafile, atomQuantities, impurityLines);
        computeasymmetry(asample, datafile);
      }
    }
    if (Constants.testtime)
      System.out.println("Fluorescence lines - spectra calculation: " +
          (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");
    
  }
  
  /**
   * The method compute the fluorescence pattern using the
   * fluorescence model by De Boer.
   * D. K. G. de Boer, Phys. Review B, 44[2], 498, 1991.
   * It uses the ElamDB. When the pattern is computed and it is added to the
   * <code>DiffrDataFile</code> using the addtoFit method.
   *
   * @param adatafile
   * @see DiffrDataFile#addtoFit
   */
  
  public void computeFluorescence_alt(Sample asample, DiffrDataFile adatafile, Hashtable<Integer,
      Vector<AtomQuantity>> atomQuantities, Vector<FluorescenceLine> impurityLines) {
    
    XRayDataSqLite.checkMinimumEnergy();

//		boolean checkSensitivity = MaudPreferences.getBoolean("xrf.sensitivityNoEnergy", false);
    Instrument ainstrument = adatafile.getDataFileSet().getInstrument();
    XRFDetector detector = (XRFDetector) ainstrument.getDetector();
    Geometry geometry = ainstrument.getGeometry();
    double incidentIntensity = ainstrument.getIntensityForFluorescence();
    double sampleLinearArea = detector.getGeometryCorrection(
        geometry.getBeamOutCorrection(adatafile, asample));
    double areaCorrection = detector.getAreaCorrection(sampleLinearArea);

//		incidentIntensity *= sampleLinearArea;

//		double polarization = ainstrument.getGeometry().getPolarizationAmount();
//		double polarizationAngle = ainstrument.getGeometry().getPolarizationAngle();
//		double cos2polarization = MoreMath.cosd(polarizationAngle);
//		cos2polarization *= cos2polarization;
//		double s_factor = 0.5 - 0.5 * polarization * (1.0 - cos2polarization);
//		double p_factor = 0.5 - 0.5 * polarization * cos2polarization;
  
    Hashtable<Integer, Vector<FluorescenceLine>> fluoLines = getAllAtomsFluorescenceLines(asample, adatafile);
  
    double[] xEnergy = adatafile.getXrangeInEnergy();
    int channelZero = adatafile.getChannelForZero();
    double channelStep = adatafile.getChannelStep();
    int numberOfPoints = xEnergy.length;
//    double maxEnergyInKeV = xEnergy[numberOfPoints - 1] * 0.001 * 1.1;
    //double minEnergyInKeV = xEnergy[0] * 0.001 / 1.1;
//    System.out.println(xEnergy[0] + " " + xEnergy[numberOfPoints - 1]);
    
    int layersNumber = asample.numberOfLayers;
    
    double[] fluorescence = new double[numberOfPoints];
    
    for (FluorescenceLine line: impurityLines) {
//      line.setMhuDet(detector.computeMACForLineWithEnergy(line.getEnergy()));
      java.util.Vector<double[]> broad = ainstrument.getInstrumentEnergyBroadeningAt(line.getEnergy());
      line.setShape(broad);
      for (int i = 0; i < numberOfPoints; i++/*, hi++*/) {
        fluorescence[i] += line.getIntensity(xEnergy[i]) * areaCorrection;
      }
    }
    
    double twothetadetector = adatafile.get2ThetaValue();
    double[] incidentDiffracted = adatafile.getIncidentAndDiffractionAngles(twothetadetector);
//		System.out.println(adatafile.getLabel() + ", incident beam angle: " + incidentDiffracted[0] * Constants.PITODEG + ", exiting beam angle: " + incidentDiffracted[2] * Constants.PITODEG + " " + adatafile.getTiltingAngle()[4]);
//	  incidentDiffracted[0] *= Constants.DEGTOPI;

//		double cosPhi2 = Math.cos(incidentDiffracted[0]);
    double sinPhii = 1.0 / Math.sin(incidentDiffracted[0]);
    double sinPhid = 1.0 / Math.sin(incidentDiffracted[2]);
    
    RadiationType radType = ainstrument.getRadiationType();
    int rad_lines = radType.getLinesCountForFluorescence();
    double[] energyInKeV = new double[rad_lines];
    double[] energy_intensity = new double[rad_lines];
    
    double[][] layerAbsorption = new double[layersNumber][rad_lines];
    double[][] overLayerAbsorption = new double[layersNumber][rad_lines];
    double[] layerDensity = new double[layersNumber];
    double[] layerThickness = new double[layersNumber];
    for (int j1 = 0; j1 < layersNumber; j1++) {
      Layer layer = asample.getlayer(j1);
      layerDensity[j1] = layer.getDensity();
      layerThickness[j1] = layer.getThicknessInCm();
    }
    for (int ej = 0; ej < rad_lines; ej++) {
      energyInKeV[ej] = radType.getRadiationEnergyForFluorescenceKeV(ej);
      energy_intensity[ej] = radType.getRadiationWeightForFluorescence(ej);
      layerAbsorption[0][ej] = -asample.getlayer(0).getAbsorptionForXray(energyInKeV[ej]) * layerDensity[0] * sinPhii;
      overLayerAbsorption[0][ej] = 0;
      for (int j1 = 1; j1 < layersNumber; j1++) {
        layerAbsorption[j1][ej] = -asample.getlayer(j1).getAbsorptionForXray(energyInKeV[ej]) * layerDensity[j1] * sinPhii;
        overLayerAbsorption[j1][ej] = overLayerAbsorption[j1 - 1][ej] + layerAbsorption[j1 - 1][ej] * layerThickness[j1 - 1];
//				System.out.println(overLayerAbsorption[j1][ej]);
      }
    }
//		int sub20 = radType.getSubdivision(); //MaudPreferences.getInteger("xrf_detector.energySubdivision", 20);
    
    Vector<FluorescenceLine> linesForAtom;
    int initialContent = 100;
    double source_intensity = ((XRFDetector) ainstrument.getDetector()).getSourceSpectrumIntensity();
    if (source_intensity > 0 && initialContent < rad_lines)
      initialContent = rad_lines;
    Vector<FluorescenceLine> fluorescenceLines = new Vector<>(initialContent, 100);
    for (int j1 = 0; j1 < layersNumber; j1++) {
//      Layer layer = asample.getlayer(j1);
      Vector<AtomQuantity> chemicalComposition = atomQuantities.get(j1); //layer.getChemicalComposition();
      for (AtomQuantity atomQuantity : chemicalComposition) {
        double atomsQuantities = atomQuantity.quantity_weight;
//				System.out.println(atomQuantity.label + " " + atomsQuantities + " " + atomQuantity.quantity);
        if (atomsQuantities > 0) {
          int atomNumber = AtomInfo.retrieveAtomNumber(atomQuantity.label);
          linesForAtom = fluoLines.get(atomNumber); // XRayDataSqLite.getFluorescenceLinesNoSensitivityFor(atomNumber, maxEnergyInKeV);
          
          for (int ij = 0; ij < linesForAtom.size(); ij++) {
            FluorescenceLine line = linesForAtom.elementAt(ij);
            if (line.getFluorescenceYield() * line.getTransitionProbability() > 0) {
              double lineEnergyKeV = line.getEnergy(); // in KeV
//						System.out.println("Line: " + line.transitionID + " " + lineEnergyKeV + " " + line.getIntensity());
              double lineInnerShellEnergyKeV = line.getCoreShellEnergy(); // in KeV
              double overLayerAbsorptionForLine = 0;
              for (int j2 = 0; j2 < j1; j2++) {
                double actualLayerAbs = -asample.getlayer(j2).getAbsorptionForXray(lineEnergyKeV) * layerDensity[j2] * sinPhid;
                overLayerAbsorptionForLine += actualLayerAbs * layerThickness[j2];
              }
              double actualLayerAbsorption = -asample.getlayer(j1).getAbsorptionForXray(lineEnergyKeV) * layerDensity[j1] * sinPhid;
//						System.out.println(actualLayerAbsorption + " " + asample.getlayer(j1).getAbsorption(lineEnergyKeV) + " " + layerDensity[j1] + " " + sinPhid);
              double totalIntensity = 0;
              for (int ej = 0; ej < rad_lines; ej++) {
                if (energyInKeV[ej] > lineInnerShellEnergyKeV && lineEnergyKeV <= energyInKeV[ej]) {
                  double over_abs = overLayerAbsorptionForLine + overLayerAbsorption[j1][ej];
                  if (!Double.isNaN(over_abs)) {
                    if (over_abs > -Double.MAX_EXPONENT / 2 && over_abs < Double.MAX_EXPONENT / 2)
                      over_abs = Math.exp(over_abs);
                    else if (over_abs > 0)
                      over_abs = 1.0;
                    else
                      over_abs = 0;
                  } else
                    over_abs = 0;
                  
                  double ab = actualLayerAbsorption + layerAbsorption[j1][ej];
                  double abs = ab * layerThickness[j1];
                  if (!Double.isNaN(abs) && abs != 0) {
                    if (abs > -Double.MAX_EXPONENT / 2 && abs < Double.MAX_EXPONENT / 2)
                      abs = -(1.0 - Math.exp(abs)) / ab;
                    else if (ab != 0)
                      abs = -1.0 / ab;
                    else
                      abs = 1.0;
                  } else
                    abs = 0;
                  
                  double lineSensitivity = XRayDataSqLite.getSensitivity(atomNumber, line.getCoreShellID(),
                      line.xrl_line_number, energyInKeV[ej], line.getFluorescenceYield() * line.getTransitionProbability()); // / (line.getFluorescenceYield() * line.getTransitionProbability());
//								if (atomNumber > 80 && line.transitionID.startsWith("M"))
//									System.out.println(atomNumber - 1 + " " + lineEnergyKeV + " " + line.transitionID + " " + lineSensitivity + " " + energyInKeV[ej]
//										+ " " + line.getCoreShellID() + " " + XRayDataSqLite.getTauShell(atomNumber - 1, line.getCoreShellID(), energyInKeV[ej]));
//                  System.out.println(line.getFluorescenceYield() + " " + line.getTransitionProbability() + " " + actualLayerAbsorption + " " + layerAbsorption[j1][ej] + " " + lineSensitivity + " " + over_abs + " " + abs + " " + energy_intensity[ej]);
                  totalIntensity += lineSensitivity * over_abs * abs * energy_intensity[ej];
                }
              }
              totalIntensity *= layerDensity[j1];
              double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(lineEnergyKeV);
              double detectorEfficiency = detector.computeDetectorEfficiency(lineEnergyKeV);
//						if (lineEnergyKeV * 1000 > xEnergy[0] && lineEnergyKeV * 1000 < xEnergy[numberOfPoints - 1])
//						System.out.println("Line: " + lineEnergyKeV + " " + line.getIntensity() + " " + atomsQuantities + " " + totalIntensity + " " + detectorAbsorption + " " +
//								detectorEfficiency + " " + areaCorrection + " " + getIntensityCorrection(atomNumber));
              double factor = atomsQuantities * detectorAbsorption *
                  detectorEfficiency * areaCorrection * getIntensityCorrection(atomNumber);
//						  System.out.println("Line: " + lineEnergyKeV + " " + line.getIntensity() + " " + factor + " " + totalIntensity + " " + totalIntensity1 + " " + (line.getIntensity() * totalIntensity));
              line.setIntensity(factor * totalIntensity);
//             System.out.println("Line: " + line.transitionID + " " + lineEnergyKeV + " " + line.getIntensity() + " " + atomsQuantities + " " + totalIntensity + " " + detectorAbsorption + " " +
//                  detectorEfficiency + " " + areaCorrection);
//						System.out.println(line.transitionID + " " + line.getIntensity() + " " + lineEnergyKeV);
              boolean addLine = true;
              if (j1 > 0) {
                for (int i = 0; i < fluorescenceLines.size() && addLine; i++) {
                  FluorescenceLine lineExisting = fluorescenceLines.get(i);
                  if (lineExisting.getEnergy() == line.getEnergy()) {
                    addLine = false;
                    lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity());
                  }
                }
              }
              if (addLine) {
                fluorescenceLines.add(line);
              }
            }
          }
        }
      }
    }
/*
    if (((XRFDetector) ainstrument.getDetector()).getFiltersFluorescenceIntensityTotal() > 0) {
      for (int ej = 0; ej < rad_lines; ej++) {
        Vector<FluorescenceLine> filtersFluorescenceLines = ((XRFDetector) ainstrument.getDetector()).getFluorescenceLines(energyInKeV[ej]);
        for (FluorescenceLine line : filtersFluorescenceLines) {
          double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(line.getEnergy());
          double detectorEfficiency = detector.computeDetectorEfficiency(line.getEnergy());
          line.setIntensity(line.getIntensity() * detectorAbsorption * detectorEfficiency);
          boolean addLine = true;
          for (int i = 0; i < fluorescenceLines.size() && addLine; i++) {
            FluorescenceLine lineExisting = fluorescenceLines.get(i);
//						lineExisting.setIntensity(lineExisting.getIntensity());
            if (lineExisting.getEnergy() == line.getEnergy()) {
              addLine = false;
              lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity() * energy_intensity[ej]);
            }
          }
          if (addLine && line.getIntensity() > 0) {
            line.setIntensity(line.getIntensity() * energy_intensity[ej]);
            fluorescenceLines.add(line);
          }
        }
      }
    }
    if (ainstrument.getRadiationType() instanceof XrayEbelTubeRadiation) {
      XrayEbelTubeRadiation source = (XrayEbelTubeRadiation) ainstrument.getRadiationType();
      if (source.getFiltersFluorescenceIntensityTotal() > 0) {
        for (int ej = 0; ej < rad_lines; ej++) {
          Vector<FluorescenceLine> filtersFluorescenceLines = source.getFluorescenceLines(energyInKeV[ej]);
          for (FluorescenceLine line : filtersFluorescenceLines) {
            double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(line.getEnergy());
            double detectorEfficiency = detector.computeDetectorEfficiency(line.getEnergy());
            line.setIntensity(line.getIntensity() * detectorAbsorption * detectorEfficiency * areaCorrection);
            boolean addLine = true;
            for (FluorescenceLine lineExisting : fluorescenceLines) {
              //							lineExisting.setIntensity(lineExisting.getIntensity());
              if (lineExisting.getEnergy() == line.getEnergy()) {
                addLine = false;
                lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity() * energy_intensity[ej]);
              }
            }
            if (addLine) {
              line.setIntensity(line.getIntensity() * energy_intensity[ej]);
              if (line.getIntensity() > 0) {
                fluorescenceLines.add(line);
              }
            }
          }
        }
      }
    }
*/
//		System.out.println("Compute fluo peaks");
    for (FluorescenceLine line : fluorescenceLines) {
//      java.util.Vector<double[]> broad = ainstrument.getInstrumentEnergyBroadeningAt(line.getEnergy());
//      line.setShape(broad);
//      line.setMhuDet(detector.computeMACForLineWithEnergy(line.getEnergy()));
      java.util.Vector<double[]> broad = ainstrument.getInstrumentEnergyBroadeningAt(line.getEnergy());
      line.setShape(broad);
      for (int i = 0; i < numberOfPoints; i++/*, hi++*/) {
        fluorescence[i] += line.getIntensity(xEnergy[i]);
      }
    }
    
    // escape pekas
    
    double[] escFluorescence = new double[numberOfPoints];

//		if (channelZero >= 0)
//			System.out.println("Channel zero: " + channelZero + " " + adatafile.getXData(channelZero));
    for (int i = 0; i < numberOfPoints; i++)
      escFluorescence[i] = fluorescence[i];
    
    double escapePeak = Math.abs(detector.getEscapePeaksIntensity());
    if (escapePeak > 0) {
      Vector<double[]> escapeIntensitiesAndEnergies = detector.getEscapeIntensity(xEnergy);
      int numberLines = escapeIntensitiesAndEnergies.size() - 1;
      double[] deltaEnergies = escapeIntensitiesAndEnergies.get(numberLines);
//			System.out.println("N lines: " + numberLines);
      for (int l = 0; l < numberLines; l++) {
        int deltaChannel = (int) (deltaEnergies[l] / channelStep * 1000);
//				System.out.println("Line: " + l+ ", delta E = " + deltaEnergies[l] + ", channels = " + deltaChannel);
        double[] relativeIntensities = escapeIntensitiesAndEnergies.get(l);
        for (int i = 0; i < numberOfPoints - deltaChannel; i++) {
          escFluorescence[i] += escapePeak * relativeIntensities[i + deltaChannel] *
              fluorescence[i + deltaChannel];
        }
      }
    }
    
    // sum peaks
    
    for (int i = 0; i < numberOfPoints; i++)
      fluorescence[i] = escFluorescence[i];
    
    double sumPeak = Math.abs(detector.getSumPeaksIntensity());
    if (sumPeak > 0) {
      for (int i = 0; i < numberOfPoints; i++) {
        for (int j = i; j < numberOfPoints; j++) {
          int channel = i + j - channelZero + adatafile.startingindex;
          if (channel >= 0 && channel < numberOfPoints)
            fluorescence[channel] += sumPeak * (escFluorescence[i] * escFluorescence[j]);
        }
      }
    }
    
    for (int i = 0; i < numberOfPoints; i++) {
      fluorescence[i] *= incidentIntensity;
      adatafile.addtoFit(i, fluorescence[i]);
//        System.out.println("Point: " + xEnergy[i] + ", intensity: " + fluorescence[i]);
    }
  }
  
}


