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
import it.unitn.ing.rista.diffr.geometry.GeometryXRFInstrument;
import it.unitn.ing.rista.diffr.radiation.XrayEbelTubeRadiation;
import it.unitn.ing.rista.util.*;

import java.util.Vector;

import static java.lang.System.out;

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
  
  public void computeFluorescence(Sample asample, DataFileSet adataset) {
    
    XRayDataSqLite.checkMinimumEnergy();
  
    final Instrument ainstrument = adataset.getInstrument();
    final XRFDetector detector = (XRFDetector) ainstrument.getDetector();
//    Geometry geometry = ainstrument.getGeometry();
  
    int datafilenumber = adataset.activedatafilesnumber();

    RadiationType radType = ainstrument.getRadiationType();
    int rad_lines = radType.getLinesCount();
    double[] energyInKeV = new double[rad_lines];
    double[] energy_intensity = new double[rad_lines];

    final Vector<FluorescenceLine> fluorescenceLines = computeFluorescenceLines(asample, adataset);
  
    /*
    int maxNumberOfPoints = 0;
    for (int k = 0; k < datafilenumber; k++) {
      DiffrDataFile datafile = adataset.getActiveDataFile(k);
      int dataNumber = datafile.computeDataNumber();
      if (dataNumber > maxNumberOfPoints)
        maxNumberOfPoints = dataNumber;
    }
    double[] xMeanEnergy = new double[maxNumberOfPoints];
    int[] totalCount = new int[maxNumberOfPoints];
    for (int k = 0; k < datafilenumber; k++) {
      DiffrDataFile datafile = adataset.getActiveDataFile(k);
      double[] xData = datafile.getXrangeInEnergy();
      int dataCount = xData.length;
      for (int i = 0; i < maxNumberOfPoints; i++) {
        if (i < dataCount) {
          xMeanEnergy[i] += xData[i];
          totalCount[i]++;
        }
      }
    }
    for (int i = 0; i < maxNumberOfPoints; i++) {
      xMeanEnergy[i] /= totalCount[i];
    }
    final Vector<double[]> escapeIntensitiesAndEnergies = computeEscapePeaks(detector, xMeanEnergy);
  */
    double incidentIntensity = ainstrument.getIntensityForFluorescence();
    final Vector<FluorescenceLine> allFiltersFluorescenceLines = new Vector<>(0,10);
    if (detector.getFiltersFluorescenceIntensityTotal() > 0) {
      for (int ej = 0; ej < rad_lines; ej++) {
        Vector<FluorescenceLine> filtersFluorescenceLines = ((XRFDetector) ainstrument.getDetector()).getFluorescenceLines(energyInKeV[ej]);
        for (FluorescenceLine line : filtersFluorescenceLines)
          line.setIntensity(line.getIntensity() * energy_intensity[ej] * incidentIntensity);
        allFiltersFluorescenceLines.addAll(filtersFluorescenceLines);
      }
    }
    if (radType instanceof XrayEbelTubeRadiation) {
      XrayEbelTubeRadiation source = (XrayEbelTubeRadiation) radType;
      if (source.getFiltersFluorescenceIntensityTotal() > 0) {
         for (int ej = 0; ej < rad_lines; ej++) {
          Vector<FluorescenceLine> filtersFluorescenceLines = source.getFluorescenceLines(energyInKeV[ej]);
           for (FluorescenceLine line : filtersFluorescenceLines)
             line.setIntensity(line.getIntensity() * energy_intensity[ej] * incidentIntensity);
           allFiltersFluorescenceLines.addAll(filtersFluorescenceLines);
        }
      }
    }
  
    for (FluorescenceLine line : fluorescenceLines) {
      java.util.Vector<double[]> broad = ainstrument.getInstrumentEnergyBroadeningAt(line.getEnergy());
      line.setShape(broad);
      line.setEnergy(line.getEnergy() * 1000.0); // in eV
    }
  
    for (FluorescenceLine line : allFiltersFluorescenceLines) {
      java.util.Vector<double[]> broad = ainstrument.getInstrumentEnergyBroadeningAt(line.getEnergy());
      line.setShape(broad);
      line.setEnergy(line.getEnergy() * 1000.0); // in eV
    }
  
    final Sample theSample = asample;
    final DataFileSet theDataset = adataset;
    
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
              DiffrDataFile adatafile = theDataset.getActiveDataFile(j);
  
              double[] xEnergy = adatafile.getXrangeInEnergy();
              int channelZero = adatafile.getChannelForZero();
              double channelStep = adatafile.getChannelStep();
              int numberOfPoints = xEnergy.length;
              double[] fluorescence = new double[numberOfPoints];

//		System.out.println("Compute fluo peaks");
              for (FluorescenceLine line : fluorescenceLines) {
                for (int i = 0; i < numberOfPoints; i++) {
                  fluorescence[i] += line.getIntensity(xEnergy[i], j);
                }
              }
  
              for (FluorescenceLine line : allFiltersFluorescenceLines) {
                for (int i = 0; i < numberOfPoints; i++) {
                  fluorescence[i] += line.getIntensity(xEnergy[i]);
                }
              }
  
              // escape pekas
  
              double[] escFluorescence = new double[numberOfPoints];

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
                  for (int n = i; n < numberOfPoints; n++) {
                    int channel = i + n - channelZero + adatafile.startingindex;
                    if (channel >= 0 && channel < numberOfPoints)
                      fluorescence[channel] += sumPeak * (escFluorescence[i] * escFluorescence[n]);
                  }
                }
              }
  
              for (int i = 0; i < numberOfPoints; i++)
                adatafile.addtoFit(i, fluorescence[i]);
  
              computeasymmetry(theSample, adatafile);
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
      
    } else
      for (int k = 0; k < datafilenumber; k++) {
        DiffrDataFile adatafile = theDataset.getActiveDataFile(k);
  
        double[] xEnergy = adatafile.getXrangeInEnergy();
        int channelZero = adatafile.getChannelForZero();
        double channelStep = adatafile.getChannelStep();
        int numberOfPoints = xEnergy.length;
        double[] fluorescence = new double[numberOfPoints];

//		System.out.println("Compute fluo peaks");
        for (FluorescenceLine line : fluorescenceLines) {
          for (int i = 0; i < numberOfPoints; i++) {
            fluorescence[i] += line.getIntensity(xEnergy[i], k);
          }
        }
  
        for (FluorescenceLine line : allFiltersFluorescenceLines) {
          for (int i = 0; i < numberOfPoints; i++) {
            fluorescence[i] += line.getIntensity(xEnergy[i]);
          }
        }
  
        // escape pekas
  
        double[] escFluorescence = new double[numberOfPoints];
  
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
            for (int n = i; n < numberOfPoints; n++) {
              int channel = i + n - channelZero + adatafile.startingindex;
              if (channel >= 0 && channel < numberOfPoints)
                fluorescence[channel] += sumPeak * (escFluorescence[i] * escFluorescence[n]);
            }
          }
        }
  
        for (int i = 0; i < numberOfPoints; i++)
          adatafile.addtoFit(i, fluorescence[i]);
  
        computeasymmetry(theSample, adatafile);
      }
    
  }
  
  /**
   * The method compute the fluorescence lines using the
   * fluorescence formula of Sherman.
   *
   * @param asample
   * @see Sample
   */
  
  public Vector<FluorescenceLine> computeFluorescenceLines(Sample asample, DataFileSet adataset) {
  
    Instrument ainstrument = adataset.getInstrument();
    XRFDetector detector = (XRFDetector) ainstrument.getDetector();
    double incidentIntensity = ainstrument.getIntensityForFluorescence();
  
    int datafilenumber = adataset.activedatafilesnumber();
  
    double maxEnergyInKeV = 0.0;
    for (int k = 0; k < datafilenumber; k++) {
      double actualEnergyInKeV = adataset.getActiveDataFile(k).getMaxEnergy();
      if (actualEnergyInKeV > maxEnergyInKeV)
        maxEnergyInKeV = actualEnergyInKeV;
    }
  
    RadiationType radType = ainstrument.getRadiationType();
    int rad_lines = radType.getLinesCount();
    double[] energyInKeV = new double[rad_lines];
    double[] energy_intensity = new double[rad_lines];
  
    for (int ej = 0; ej < rad_lines; ej++) {
      energyInKeV[ej] = Constants.ENERGY_LAMBDA / radType.getRadiationWavelengthForFluorescence(ej) * 0.001;
      energy_intensity[ej] = radType.getRadiationWeightForFluorescence(ej) * incidentIntensity;
    }

    int layersNumber = asample.numberOfLayers;
    double[] layerDensity = new double[layersNumber];
    double[] layerThickness = new double[layersNumber];
    for (int j1 = 0; j1 < layersNumber; j1++) {
      Layer layer = asample.getlayer(j1);
      layerDensity[j1] = layer.getDensity();
      layerThickness[j1] = layer.getThicknessInCm();
    }
  
    Geometry geometry = ainstrument.getGeometry();
    double[] areaCorrection = new double[datafilenumber];
  
    double[] one_sinPhii = new double[datafilenumber];
    double[] one_sinPhid = new double[datafilenumber];
    for (int k = 0; k < datafilenumber; k++) {
      DiffrDataFile datafile = adataset.getActiveDataFile(k);
      double[] incidentDiffracted = datafile.getIncidentAndDiffractionAngles(datafile.get2ThetaValue());
      one_sinPhii[k] = 1.0 / Math.sin(incidentDiffracted[0]);
      one_sinPhid[k] = 1.0 / Math.sin(incidentDiffracted[2]);
      double sampleLinearArea = detector.getGeometryCorrection(
          ((GeometryXRFInstrument) geometry).getBeamOutCorrection(datafile, asample));
      areaCorrection[k] = detector.getAreaCorrection(sampleLinearArea);
    }
  
    double[][][] layerAbsorption_ns = new double[layersNumber][rad_lines][datafilenumber];
    double[][][] overLayerAbsorption_ns = new double[layersNumber][rad_lines][datafilenumber];
    for (int ej = 0; ej < rad_lines; ej++) {
      energyInKeV[ej] = Constants.ENERGY_LAMBDA / radType.getRadiationWavelengthForFluorescence(ej) * 0.001;
      energy_intensity[ej] = radType.getRadiationWeightForFluorescence(ej);
      double layerAbsorption = -asample.getlayer(0).getAbsorptionForXray(energyInKeV[ej]) * layerDensity[0];
      for (int k = 0; k < datafilenumber; k++) {
        layerAbsorption_ns[0][ej][k] = layerAbsorption * one_sinPhii[k];
        overLayerAbsorption_ns[0][ej][k] = 0;
      }
      for (int j1 = 1; j1 < layersNumber; j1++) {
        layerAbsorption = -asample.getlayer(j1).getAbsorptionForXray(energyInKeV[ej]) * layerDensity[j1];
        for (int k = 0; k < datafilenumber; k++) {
          layerAbsorption_ns[j1][ej][k] = layerAbsorption * one_sinPhii[k];
          overLayerAbsorption_ns[j1][ej][k] = overLayerAbsorption_ns[j1 - 1][ej][k] + layerAbsorption_ns[j1 - 1][ej][k] * layerThickness[j1 - 1];
        }
      }
    }
    double[] overLayerAbsorptionForLine_ns = new double[datafilenumber];
    double[] actualLayerAbsorption_ns = new double[datafilenumber];
    double[] totalIntensity = new double[datafilenumber];
  
  
    Vector<FluorescenceLine> linesForAtom;
    int initialContent = 100;
    double source_intensity = ((XRFDetector) ainstrument.getDetector()).getSourceSpectrumIntensity();
    if (source_intensity > 0 && initialContent < rad_lines)
      initialContent = rad_lines;
    Vector<FluorescenceLine> fluorescenceLines = new Vector<>(initialContent, 100);
    for (int j1 = 0; j1 < layersNumber; j1++) {
      Layer layer = asample.getlayer(j1);
      Vector<AtomQuantity> chemicalComposition = layer.getChemicalComposition();
      for (AtomQuantity atomQuantity : chemicalComposition) {
        double atomsQuantities = atomQuantity.quantity_weight;
//				System.out.println(atomQuantity.label + " " + atomsQuantities + " " + atomQuantity.quantity);
        if (atomsQuantities > 0) {
          int atomNumber = AtomInfo.retrieveAtomNumber(atomQuantity.label);

          linesForAtom = XRayDataSqLite.getFluorescenceLinesNoSensitivityFor(atomNumber, maxEnergyInKeV);
          
          for (int ij = 0; ij < linesForAtom.size(); ij++) {
            FluorescenceLine line = linesForAtom.elementAt(ij);
            double lineEnergyKeV = line.getEnergy(); // in KeV
            double lineInnerShellEnergyKeV = line.getCoreShellEnergy(); // in KeV
            for (int k = 0; k < datafilenumber; k++)
              overLayerAbsorptionForLine_ns[k] = 0;
            for (int j2 = 0; j2 < j1; j2++) {
              double actualLayerAbs = -asample.getlayer(j2).getAbsorptionForXray(lineEnergyKeV) * layerDensity[j2] * layerThickness[j2];
              for (int k = 0; k < datafilenumber; k++)
                overLayerAbsorptionForLine_ns[k] += actualLayerAbs * one_sinPhid[k];
            }
            double actualLayerAbs = -asample.getlayer(j1).getAbsorptionForXray(lineEnergyKeV) * layerDensity[j1];
            for (int k = 0; k < datafilenumber; k++) {
              actualLayerAbsorption_ns[k] = actualLayerAbs * one_sinPhid[k];
              totalIntensity[k] = 0;
            }
            for (int ej = 0; ej < rad_lines; ej++) {
              if (energyInKeV[ej] > lineInnerShellEnergyKeV && lineEnergyKeV <= energyInKeV[ej]) {
                for (int k = 0; k < datafilenumber; k++) {
                  double lineSensitivity = XRayDataSqLite.getSensitivity(atomNumber, line.getCoreShellID(),
                      line.xrl_line_number, energyInKeV[ej], line.getFluorescenceYield() * line.getTransitionProbability());
  
                  double over_abs = overLayerAbsorptionForLine_ns[k] + overLayerAbsorption_ns[j1][ej][k];
                  if (!Double.isNaN(over_abs)) {
                    if (over_abs > -Double.MAX_EXPONENT / 2 && over_abs < Double.MAX_EXPONENT / 2)
                      over_abs = Math.exp(over_abs);
                    else if (over_abs > 0)
                      over_abs = Double.MAX_VALUE / 2;
                    else
                      over_abs = 0;
                  } else
                    over_abs = 0;
  
                  double ab = (actualLayerAbsorption_ns[k] + layerAbsorption_ns[j1][ej][k]);
                  double abs = ab * layerThickness[j1];
                  if (!Double.isNaN(abs) && abs != 0) {
                    if (abs > -Double.MAX_EXPONENT / 2 && abs < Double.MAX_EXPONENT / 2)
                      abs = -(1.0 - Math.exp(abs)) / ab;
                    else
                      abs = -1.0 / ab;
                  } else
                    abs = 0;

                  totalIntensity[k] += lineSensitivity * energy_intensity[ej] * over_abs * abs;
                }
              }
            }
            double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(lineEnergyKeV);
            line.mhuDet = detector.computeMACForLineWithEnergy(lineEnergyKeV);
            double detectorEfficiency = detector.computeDetectorEfficiency(lineEnergyKeV);
            double correction = atomsQuantities * detectorAbsorption * detectorEfficiency * getIntensityCorrection(atomNumber);
            for (int k = 0; k < datafilenumber; k++) {
              totalIntensity[k] *= layerDensity[j1] * areaCorrection[k] * correction;
            }
            line.multiplyIntensityBy(totalIntensity);
//						System.out.println(line.transitionID + " " + line.getIntensity() + " " + lineEnergyKeV);
            boolean addLine = true;
            for (int i = 0; i < fluorescenceLines.size() && addLine; i++) {
              FluorescenceLine lineExisting = fluorescenceLines.get(i);
              if (lineExisting.getEnergy() == line.getEnergy()) {
                addLine = false;
                double[] existingIntensity = lineExisting.getMultipleIntensity();
                double[] intensity = line.getMultipleIntensity();
                for (int k = 0; k < datafilenumber; k++) {
                  existingIntensity[k] = existingIntensity[k] + intensity[k];
                }
                lineExisting.setMultipleIntensity(existingIntensity);
              }
            }
            if (addLine) {
              fluorescenceLines.add(line);
            }
            
          }
        }
        
      }
    }

    return fluorescenceLines;
  }
  
  public Vector<double[]> computeEscapePeaks(XRFDetector detector, double[] xEnergy) {
    Vector<double[]> escapeIntensitiesAndEnergies = null;
    double escapePeak = Math.abs(detector.getEscapePeaksIntensity());
    if (escapePeak > 0) {
      escapeIntensitiesAndEnergies = detector.getEscapeIntensity(xEnergy);
    }
    return escapeIntensitiesAndEnergies;
  }
  
  public void computeFluorescence_old(Sample asample, DiffrDataFile adatafile) {
    
    XRayDataSqLite.checkMinimumEnergy();

//		boolean checkSensitivity = MaudPreferences.getBoolean("xrf.sensitivityNoEnergy", false);
    Instrument ainstrument = adatafile.getDataFileSet().getInstrument();
    XRFDetector detector = (XRFDetector) ainstrument.getDetector();
    Geometry geometry = ainstrument.getGeometry();
    double incidentIntensity = ainstrument.getIntensityForFluorescence();
    double sampleLinearArea = detector.getGeometryCorrection(
        ((GeometryXRFInstrument) geometry).getBeamOutCorrection(adatafile, asample));
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
    double maxEnergyInKeV = xEnergy[numberOfPoints - 1] * 0.001 * 1.1;
    //double minEnergyInKeV = xEnergy[0] * 0.001 / 1.1;
//    System.out.println(xEnergy[0] + " " + xEnergy[numberOfPoints - 1]);
    
    int layersNumber = asample.numberOfLayers;
    
    double[] fluorescence = new double[numberOfPoints];
    
    double[] incidentDiffracted = adatafile.getIncidentAndDiffractionAngles(adatafile.get2ThetaValue());
//		System.out.println(adatafile.getLabel() + ", incident beam angle: " + incidentDiffracted[0] * Constants.PITODEG + ", exiting beam angle: " + incidentDiffracted[2] * Constants.PITODEG + " " + adatafile.getTiltingAngle()[4]);
//	  incidentDiffracted[0] *= Constants.DEGTOPI;

//		double cosPhi2 = Math.cos(incidentDiffracted[0]);
    double sinPhii = Math.sin(incidentDiffracted[0]);
    double sinPhid = Math.sin(incidentDiffracted[2]);
    
    RadiationType radType = ainstrument.getRadiationType();
    int rad_lines = radType.getLinesCount();
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
      energyInKeV[ej] = Constants.ENERGY_LAMBDA / radType.getRadiationWavelengthForFluorescence(ej) * 0.001;
      energy_intensity[ej] = radType.getRadiationWeightForFluorescence(ej);
      layerAbsorption[0][ej] = -asample.getlayer(0).getAbsorptionForXray(energyInKeV[ej]) * layerDensity[0] / sinPhii;
      overLayerAbsorption[0][ej] = 0;
      for (int j1 = 1; j1 < layersNumber; j1++) {
        layerAbsorption[j1][ej] = -asample.getlayer(j1).getAbsorptionForXray(energyInKeV[ej]) * layerDensity[j1] / sinPhii;
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
      Layer layer = asample.getlayer(j1);
      Vector<AtomQuantity> chemicalComposition = layer.getChemicalComposition();
      for (AtomQuantity atomQuantity : chemicalComposition) {
        double atomsQuantities = atomQuantity.quantity_weight;
//				System.out.println(atomQuantity.label + " " + atomsQuantities + " " + atomQuantity.quantity);
        if (atomsQuantities > 0) {
          int atomNumber = AtomInfo.retrieveAtomNumber(atomQuantity.label);

//					if (checkSensitivity)
//						linesForAtom = XRayDataSqLite.getFluorescenceLinesFor(     // remove NoSensitivity
//							atomNumber, maxEnergyInKeV);
//					else
          linesForAtom = XRayDataSqLite.getFluorescenceLinesNoSensitivityFor(atomNumber, maxEnergyInKeV);
          
          for (int ij = 0; ij < linesForAtom.size(); ij++) {
            FluorescenceLine line = linesForAtom.elementAt(ij);
            double lineEnergyKeV = line.getEnergy(); // in KeV
            double lineInnerShellEnergyKeV = line.getCoreShellEnergy(); // in KeV
            double overLayerAbsorptionForLine = 0;
            for (int j2 = 0; j2 < j1; j2++) {
              double actualLayerAbs = -asample.getlayer(j2).getAbsorptionForXray(lineEnergyKeV) * layerDensity[j2] / sinPhid;
              overLayerAbsorptionForLine += actualLayerAbs * layerThickness[j2];
            }
            double actualLayerAbsorption = -asample.getlayer(j1).getAbsorptionForXray(lineEnergyKeV) * layerDensity[j1] / sinPhid;
//						System.out.println(actualLayerAbsorption + " " + asample.getlayer(j1).getAbsorption(lineEnergyKeV) + " " + layerDensity[j1] + " " + sinPhid);
            double totalIntensity = 0;
            for (int ej = 0; ej < rad_lines; ej++) {
              if (energyInKeV[ej] > lineInnerShellEnergyKeV && lineEnergyKeV <= energyInKeV[ej]) {
                double over_abs = overLayerAbsorptionForLine + overLayerAbsorption[j1][ej];
                if (!Double.isNaN(over_abs)) {
                  if (over_abs > -Double.MAX_EXPONENT / 2 && over_abs < Double.MAX_EXPONENT / 2)
                    over_abs = Math.exp(over_abs);
                  else if (over_abs > 0)
                    over_abs = Double.MAX_VALUE / 2;
                  else
                    over_abs = 0;
                } else
                  over_abs = 0;
                
                double ab = (actualLayerAbsorption + layerAbsorption[j1][ej]);
                double abs = ab * layerThickness[j1];
                if (!Double.isNaN(abs) && abs != 0) {
                  if (abs > -Double.MAX_EXPONENT / 2 && abs < Double.MAX_EXPONENT / 2)
                    abs = -(1.0 - Math.exp(abs)) / ab;
                  else
                    abs = -1.0 / ab;
                } else
                  abs = 0;
                
                double lineSensitivity = XRayDataSqLite.getSensitivity(atomNumber, line.getCoreShellID(),
                    line.xrl_line_number, energyInKeV[ej], line.getFluorescenceYield() * line.getTransitionProbability());
//								if (atomNumber > 80 && line.transitionID.startsWith("M"))
//									System.out.println(atomNumber - 1 + " " + lineEnergyKeV + " " + line.transitionID + " " + lineSensitivity + " " + energyInKeV[ej]
//										+ " " + line.getCoreShellID() + " " + XRayDataSqLite.getTauShell(atomNumber - 1, line.getCoreShellID(), energyInKeV[ej]));
//								System.out.println(totalIntensity + " " + actualLayerAbsorption + " " + layerAbsorption[j1][ej] + " " + lineSensitivity + " " + over_abs + " " + abs + " " + energy_intensity[ej]);
                totalIntensity += lineSensitivity * over_abs * abs * energy_intensity[ej];
              }
            }
            totalIntensity *= layerDensity[j1];
            double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(lineEnergyKeV);
            line.mhuDet = detector.computeMACForLineWithEnergy(lineEnergyKeV);
            double detectorEfficiency = detector.computeDetectorEfficiency(lineEnergyKeV);
            double areaCorrection = detector.getAreaCorrection(sampleLinearArea);
//						if (lineEnergyKeV * 1000 > xEnergy[0] && lineEnergyKeV * 1000 < xEnergy[numberOfPoints - 1])
//						System.out.println("Line: " + lineEnergyKeV + " " + line.getIntensity() + " " + atomsQuantities + " " + totalIntensity + " " + detectorAbsorption + " " +
//								detectorEfficiency + " " + areaCorrection + " " + getIntensityCorrection(atomNumber));
            line.multiplyIntensityBy(atomsQuantities * totalIntensity * detectorAbsorption *
                detectorEfficiency * areaCorrection * getIntensityCorrection(atomNumber));
//						System.out.println(line.transitionID + " " + line.getIntensity() + " " + lineEnergyKeV);
            boolean addLine = true;
            for (int i = 0; i < fluorescenceLines.size() && addLine; i++) {
              FluorescenceLine lineExisting = fluorescenceLines.get(i);
              if (lineExisting.getEnergy() == line.getEnergy()) {
                addLine = false;
                lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity());
              }
            }
            if (addLine) {
              fluorescenceLines.add(line);
            }
            
          }
        }
        
      }
    }
    
    if (((XRFDetector) ainstrument.getDetector()).getFiltersFluorescenceIntensityTotal() > 0) {
      
      for (int ej = 0; ej < rad_lines; ej++) {
        Vector<FluorescenceLine> filtersFluorescenceLines = ((XRFDetector) ainstrument.getDetector()).getFluorescenceLines(energyInKeV[ej]);
        for (FluorescenceLine line : filtersFluorescenceLines) {
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
              if (line.getIntensity() > 0)
                fluorescenceLines.add(line);
            }
          }
        }
      }
    }
    
    if (source_intensity > 0) {
      double lastEnergyInKeV = 0;
      double lastEnergyIntensity = 0;
      int sub20 = 1; //radType.getSubdivision(); //MaudPreferences.getInteger("xrf_detector.energySubdivision", 20);
      for (int ej = 0; ej < rad_lines; ej++) {
        if (radType instanceof XrayEbelTubeRadiation && ej >= ((XrayEbelTubeRadiation) radType).getNumberOfCharacteristicsLines()) {
          double stepEnergy = (energyInKeV[ej] - lastEnergyInKeV) / sub20;
          double stepIntEnergy = (energy_intensity[ej] - lastEnergyIntensity) / sub20;
          double subEnergy = lastEnergyInKeV;
          double subEnergyInt = lastEnergyIntensity;
          for (int sub_i = 0; sub_i < sub20; sub_i++) {
            subEnergy += stepEnergy;
            subEnergyInt += stepIntEnergy;
            FluorescenceLine transfertLine = new FluorescenceLine(subEnergy, -1, 0, "transfert line");
            double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(subEnergy);
            double detectorEfficiency = detector.computeDetectorEfficiency(subEnergy);
            double areaCorrection = detector.getAreaCorrection(sampleLinearArea);
            transfertLine.setIntensity(transfertLine.getIntensity() * source_intensity * subEnergyInt *
                detectorAbsorption * detectorEfficiency * areaCorrection / sub20);
            if (transfertLine.getIntensity() > 0)
              fluorescenceLines.add(transfertLine);
          }
          lastEnergyInKeV = energyInKeV[ej];
          lastEnergyIntensity = energy_intensity[ej];
        }
      }
    }

//		System.out.println("Compute fluo peaks");
    for (FluorescenceLine line : fluorescenceLines) {
      java.util.Vector<double[]> broad = ainstrument.getInstrumentEnergyBroadeningAt(line.getEnergy());
      line.setShape(broad);
      line.setEnergy(line.getEnergy() * 1000.0); // in eV
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
