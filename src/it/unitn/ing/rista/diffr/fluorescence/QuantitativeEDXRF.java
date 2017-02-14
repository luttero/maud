/*
 * @(#)QualitativeEDXRF.java created Apr 8, 2007 Casalino
 *
 * Copyright (c) 2006 Luca Lutterotti All Rights Reserved.
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

/**
 * The QuantitativeEDXRF is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Apr 8, 2007 10:59:53 PM $
 * @since JDK1.1
 */
public class QuantitativeEDXRF extends QuantitativeXRF {
  
  public static String modelID = "Quantitative EDXRF";
  public static String descriptionID = "Perform Quantitative fitting of EDXRF data";

  public QuantitativeEDXRF(XRDcat obj, String alabel) {
    super(obj, alabel);
    identifier = modelID;
    IDlabel = modelID;
    description = descriptionID;
  }

  public QuantitativeEDXRF(XRDcat afile) {
    this(afile, modelID);
  }

  public QuantitativeEDXRF() {
    identifier = modelID;
    IDlabel = modelID;
    description = descriptionID;
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

	public void computeFluorescence(DiffrDataFile adatafile) {
		super.computeFluorescence(adatafile);
/*
		Sample asample = adatafile.getDataFileSet().getSample();
		Instrument ainstrument = adatafile.getDataFileSet().getInstrument();
		Detector detector = ainstrument.getDetector();
		Geometry geometry = ainstrument.getGeometry();
		double incidentIntensity = ainstrument.getIntensityValue();
//		double sampleLinearArea = 1;

		double[] xEnergy = adatafile.getXrangeInEnergy();
		int numberOfPoints = xEnergy.length;
		double maxEnergyInKeV = xEnergy[numberOfPoints - 1] * 0.001 * 1.1;

		int layersNumber = asample.numberOfLayers + 1;

		double[] fluorescence = new double[numberOfPoints];

		double twothetadetector = ((Instrument) getParent()).getDetector().getThetaDetector(adatafile, 0);
		double[] incidentDiffracted = adatafile.getIncidentAndDiffractionAngles(twothetadetector);
		double sinPhii = Math.sin(incidentDiffracted[0]);
//		incidentDiffracted[1] = ainstrument.getDetector().getThetaDetector(adatafile, 0) * Constants.DEGTOPI;
		double sinPhid = Math.sin(incidentDiffracted[1]);
//		System.out.println("Sinphid " + sinPhid + ", phi " + incidentDiffracted[1]);
		RadiationType radType = ainstrument.getRadiationType();
		int rad_lines = radType.getLinesCountForFluorescence();
		int sub20 = radType.getSubdivision(); //MaudPreferences.getInteger("xrf_detector.energySubdivision", 20);

		int initialContent = 100;
		double source_intensity = ((XRFDetector) ainstrument.getDetector()).getSourceSpectrumIntensity();
		if (source_intensity > 0 && initialContent < rad_lines) {
			initialContent = rad_lines;
		}
		Vector<FluorescenceLine> fluorescenceLines = new Vector<FluorescenceLine>(initialContent, 10);
		for (int ej = 0; ej < rad_lines; ej++) {
			double lambda = radType.getRadiationWavelengthForFluorescence(ej);
			double energy_intensity = radType.getRadiationWeightForFluorescence(ej);
			double constant1 = 2.0 * Constants.PI / lambda * 1E8;
			double energy = Constants.ENERGY_LAMBDA / lambda;    // in eV
			double energyInKeV = energy * 0.001;
			for (int j1 = 0; j1 < layersNumber - 1; j1++) {
				int j = j1 + 1;
				Layer layer = asample.getlayer(j1);
				double thickness = layer.getThicknessInCm();
				double layerAbsorption = layer.getAbsorption(energyInKeV) * layer.getDensity();

				Vector<AtomQuantity> chemicalComposition = layer.getChemicalComposition();

				double density = layer.getDensity();
				if (source_intensity > 0) {

					double subEnergy = energyInKeV;
					double subEnergyInt = energy_intensity;
					FluorescenceLine transfertLine = new FluorescenceLine(subEnergy, -1, 0);
					double absorptionLineLambda = layer.getAbsorption(subEnergy) * density;
					double totalIntensity = Math.exp(-layer.getOverLayerAbsorption(subEnergy) * density / sinPhid)
							* subEnergyInt;
					double totalRe = 1.0;  // 0.0;
					double mhuOverSinD = absorptionLineLambda / sinPhid;
					double bm_mhu = mhuOverSinD;
					double expm = -bm_mhu * thickness; //_or_zero;
					if (expm > -Double.MAX_EXPONENT / 2 && expm < Double.MAX_EXPONENT / 2)
						expm = Math.exp(expm);
					else {
						if (expm > 0)
							expm = Double.MAX_VALUE / 2;
						else
							expm = 0;
					}
					double detectorAbsorption = ((XRFDetector) detector).computeAbsorptionForLineWithEnergy(subEnergy);
					double detectorEfficiency = ((XRFDetector) detector).computeDetectorEfficiency(subEnergy);
//							double areaCorrection = ((XRFDetector) detector).getAreaCorrection(sampleLinearArea);
					transfertLine.setIntensity(transfertLine.getIntensity() * totalRe * totalIntensity * source_intensity *
							detectorAbsorption * detectorEfficiency);
					boolean addLine = true;
					for (int i = 0; i < fluorescenceLines.size() && addLine; i++) {
						FluorescenceLine lineExisting = fluorescenceLines.get(i);
						if (lineExisting.getEnergy() == transfertLine.getEnergy()) {
							addLine = false;
							lineExisting.setIntensity(lineExisting.getIntensity() + transfertLine.getIntensity());
						}
					}
					if (addLine)
						fluorescenceLines.add(transfertLine);
				}

				for (int k = 0; k < chemicalComposition.size(); k++) {
					int atomNumber = AtomInfo.retrieveAtomNumber(chemicalComposition.elementAt(k).label);
					Vector<FluorescenceLine> linesForAtom = XRayDataSqLite.getFluorescenceLinesFor(
							atomNumber, energyInKeV);
					double atomsQuantities = chemicalComposition.elementAt(k).quantity_weight;
					if (atomsQuantities > 0) {
						for (int ij = 0; ij < linesForAtom.size(); ij++) {
							FluorescenceLine line = linesForAtom.elementAt(ij);
							double lineEnergyKeV = line.getEnergy(); // in KeV
							if (lineEnergyKeV <= energyInKeV) {
								double absorptionLineLambda = layer.getAbsorption(lineEnergyKeV) * density;

								double totalIntensity = Math.exp(-layer.getOverLayerAbsorption(lineEnergyKeV) * density / sinPhid)
										* energy_intensity;
								double totalRe = 1.0; // 0.0;
								double mhuOverSinD = absorptionLineLambda / sinPhid;
								double detectorAbsorption = ((XRFDetector) detector).computeAbsorptionForLineWithEnergy(lineEnergyKeV);
								double detectorEfficiency = ((XRFDetector) detector).computeDetectorEfficiency(lineEnergyKeV);
								line.multiplyIntensityBy(atomsQuantities * totalRe * totalIntensity * detectorAbsorption *
										detectorEfficiency * getIntensityCorrection(atomNumber));
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

					Vector<FluorescenceLine> filtersFluorescenceLines = ((XRFDetector) ainstrument.getDetector()).getFluorescenceLines(energyInKeV);
					for (int si = 0; si < filtersFluorescenceLines.size(); si++) {
						FluorescenceLine line = filtersFluorescenceLines.get(si);
						boolean addLine = true;
						for (int i = 0; i < fluorescenceLines.size(); i++) {
							FluorescenceLine lineExisting = fluorescenceLines.get(i);
							lineExisting.setIntensity(lineExisting.getIntensity());
							if (lineExisting.getEnergy() == line.getEnergy()) {
								addLine = false;
								lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity() * energy_intensity);
							}
						}
						if (addLine) {
							line.setIntensity(line.getIntensity() * energy_intensity);
							fluorescenceLines.add(line);
						}
					}
				}
				if (ainstrument.getRadiationType() instanceof XrayEbelTubeRadiation) {
					XrayEbelTubeRadiation source = (XrayEbelTubeRadiation) ainstrument.getRadiationType();
					if (source.getFiltersFluorescenceIntensityTotal() > 0) {

						Vector<FluorescenceLine> filtersFluorescenceLines = source.getFluorescenceLines(energyInKeV);
						for (int si = 0; si < filtersFluorescenceLines.size(); si++) {
							FluorescenceLine line = filtersFluorescenceLines.get(si);
							boolean addLine = true;
							for (int i = 0; i < fluorescenceLines.size(); i++) {
								FluorescenceLine lineExisting = fluorescenceLines.get(i);
								lineExisting.setIntensity(lineExisting.getIntensity());
								if (lineExisting.getEnergy() == line.getEnergy()) {
									addLine = false;
									lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity() * energy_intensity);
								}
							}
							if (addLine) {
								line.setIntensity(line.getIntensity() * energy_intensity);
								fluorescenceLines.add(line);
							}
						}
					}
				}
			}
		}

		Vector<FluorescenceLine> sumLines = null;
		if (((XRFDetector) ainstrument.getDetector()).getSumPeaksIntensity() > 0)
			sumLines = ((XRFDetector) ainstrument.getDetector()).getPileUpPeaks(maxEnergyInKeV, fluorescenceLines);
		Vector<FluorescenceLine> escapeLines = null;
		if (((XRFDetector) ainstrument.getDetector()).getEscapePeaksIntensity() > 0)
			escapeLines = ((XRFDetector) ainstrument.getDetector()).getEscapePeaks(maxEnergyInKeV, fluorescenceLines);

		if (sumLines != null) {
			for (int si = 0; si < sumLines.size(); si++) {
				FluorescenceLine line = sumLines.get(si);
				boolean addLine = true;
				for (int i = 0; i < fluorescenceLines.size() && addLine; i++) {
					FluorescenceLine lineExisting = fluorescenceLines.get(i);
					if (lineExisting.getEnergy() == line.getEnergy()) {
						addLine = false;
						lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity());
					}
				}
				if (addLine) {
					line.setIntensity(line.getIntensity());
					fluorescenceLines.add(line);
				}
			}
		}

		if (escapeLines != null) {
			for (int si = 0; si < escapeLines.size(); si++) {
				FluorescenceLine line = escapeLines.get(si);
				boolean addLine = true;
				for (int i = 0; i < fluorescenceLines.size() && addLine; i++) {
					FluorescenceLine lineExisting = fluorescenceLines.get(i);
					if (lineExisting.getEnergy() == line.getEnergy()) {
						addLine = false;
						lineExisting.setIntensity(lineExisting.getIntensity() + line.getIntensity());
					}
				}
				if (addLine) {
					line.setIntensity(line.getIntensity());
					fluorescenceLines.add(line);
				}
			}
		}

		for (int i = 0; i < numberOfPoints; i++)
			fluorescence[i] = 0.0;
		for (int k = 0; k < fluorescenceLines.size(); k++) {
			FluorescenceLine line = fluorescenceLines.get(k);
			double[] broad = ainstrument.getInstrumentalBroadeningAt(line.getEnergy(), adatafile);
			line.setShape(broad[1], broad[0]);
			line.setEnergy(line.getEnergy() * 1000.0); // in eV
//      System.out.println(line.getEnergy() + " " + line.getIntensity());
			for (int i = 0; i < numberOfPoints; i++)
				fluorescence[i] += line.getIntensity(xEnergy[i]);
		}

		for (int i = 0; i < numberOfPoints; i++) {
			fluorescence[i] *= incidentIntensity;
			adatafile.addtoFit(i, fluorescence[i]);
//        System.out.println("Point: " + xEnergy[i] + ", intensity: " + fluorescence[i]);
		}*/
	}

}
