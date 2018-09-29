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
import it.unitn.ing.rista.interfaces.Peak;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.PersistentThread;

import java.io.*;
import java.lang.*;
import java.util.Vector;

import static java.lang.System.arraycopy;
import static java.lang.System.out;

/**
 *  The DiffractionAngleEnergyMap is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2018/08/29 11:15:28 $
 * @author Luca Lutterotti
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

/*	public void computeDiffraction(Sample asample, DataFileSet adataset) {

		int datafilenumber = adataset.activedatafilesnumber();
		if (datafilenumber == 0)
			return;

		final Sample theSample = asample;
		final DataFileSet theDataset = adataset;

		DiffrDataFile datafile = theDataset.getActiveDataFile(0);
		if (!datafile.energyDispersive)
			return;

		RadiationType radType = theDataset.getInstrument().getRadiationType();
		int rad_lines = radType.getLinesCount();
		double[] energyInKeV = new double[rad_lines];
		double[] energy_intensity = new double[rad_lines];

		int datanumber = datafile.getNumberOfData();

		int layersNumber = asample.layersnumber();

		double[][] layerAbsorption = new double[layersNumber][datafilenumber];
		double[][] overLayerAbsorption = new double[layersNumber][datafilenumber];
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
		}

		double[][] x = new double[DiffrDataFile.maxAngleNumber][datafilenumber];
		double[] fit = new double[datafilenumber];
		double[] sinPhii = new double[datafilenumber];
		double[] sinPhid = new double[datafilenumber];
		for (int k = 0; k < datafilenumber; k++) {
			for (int i = 0; i < DiffrDataFile.maxAngleNumber; i++)
				x[i][k] = theDataset.getActiveDataFile(k).getAngleValue(i);
			double[] incidentDiffracted = theDataset.getActiveDataFile(k).getIncidentAndDiffractionAngles(
					x[DiffrDataFile.DATAFILE_THETA2][k]);
//	  incidentDiffracted[0] *= Constants.DEGTOPI;

//		double cosPhi2 = Math.cos(incidentDiffracted[0]);
			sinPhii[k] = Math.sin(incidentDiffracted[0]);
			sinPhid[k] = Math.sin(incidentDiffracted[2]);
		}
		for (int i = 0; i < datanumber; i++) {

			for (int k = 0; k < datafilenumber; k++)
				fit[k] = 0;
			double energy_coord = datafile.getXData(i);
			if (energy_coord >= energyInKeV[0] && energy_coord <= energyInKeV[rad_lines]) {
				int ej = 0;
				while (energy_coord < energyInKeV[ej] && ej < rad_lines)
					ej++;

				double energyIntensity = energy_intensity[ej - 1] + (energy_intensity[ej] - energy_intensity[ej - 1]) *
						(energy_coord - energyInKeV[ej - 1]) / (energyInKeV[ej] - energyInKeV[ej - 1]);

				for (int k = 0; k < datafilenumber; k++) {

					layerAbsorption[0][ej] = -asample.getlayer(0).getAbsorption(energyInKeV[ej]) * layerDensity[0] / sinPhii[k];
					overLayerAbsorption[0][ej] = 0;
					for (int j1 = 1; j1 < layersNumber; j1++) {
						layerAbsorption[j1][ej] = -asample.getlayer(j1).getAbsorption(energyInKeV[ej]) * layerDensity[j1] / sinPhii[k];
						overLayerAbsorption[j1][ej] = overLayerAbsorption[j1 - 1][ej] + layerAbsorption[j1 - 1][ej] * layerThickness[j1 - 1];
//				System.out.println(overLayerAbsorption[j1][ej]);
					}
					double overLayerAbsorptionForLine = 0;
					for (int j2 = 0; j2 < j1; j2++) {
						double actualLayerAbs = -asample.getlayer(j2).getAbsorption(lineEnergyKeV) * layerDensity[j2] / sinPhid;
						overLayerAbsorptionForLine += actualLayerAbs * layerThickness[j2];
					}
					double actualLayerAbsorption = -asample.getlayer(j1).getAbsorption(lineEnergyKeV) * layerDensity[j1] / sinPhid;
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

					double total_abs = over_abs * abs;
				}
			}




			if (getFilePar().isComputingDerivate()) {
//      System.out.println("refreshing derivative: " + this.toXRDcatString());
				for (int ij = 0; ij < getFilePar().getActiveSample().phasesNumber(); ij++) {
					double expfit[] = new double[datafile.getTotalNumberOfData()];
					int minmaxindex[] = computeReflectionIntensity(asample, datafileset, true,
							expfit, Constants.ENTIRE_RANGE, Constants.COMPUTED,
							Constants.COMPUTED, Constants.COMPUTED, false,
							getFilePar().getActiveSample().getPhase(ij), datafile);
					for (int j = minmaxindex[0]; j < minmaxindex[1]; j++)
						datafile.addtoPhasesFit(j, expfit[j]);
				}
			} else {
//      System.out.println("refreshing: " + this.toXRDcatString());
				for (int ij = 0; ij < getFilePar().getActiveSample().phasesNumber(); ij++) {
//        System.out.println("Phase: " + getFilePar().getActiveSample().getPhase(ij).toXRDcatString());
					double expfit[] = new double[datafile.getTotalNumberOfData()];
					int minmaxindex[] = computeReflectionIntensity(asample, datafileset, true,
							expfit, Constants.ENTIRE_RANGE, Constants.COMPUTED,
							Constants.COMPUTED, Constants.COMPUTED, false,
							getFilePar().getActiveSample().getPhase(ij), datafile);
//        System.out.println("indices: " + minmaxindex[0] + " " + minmaxindex[1] + " " + expfit[1000]);
					for (int j = minmaxindex[0]; j < minmaxindex[1]; j++)
						datafile.addtoPhasesFit(j, expfit[j], ij);
				}
			}
			for (int k = 0; k < datafilenumber; k++) {
				DiffrDataFile datafile = theDataset.getActiveDataFile(k);
				computeDiffraction(theSample, datafile);
				computeasymmetry(theSample, datafile);
			}
		}
	}

	public void computeDiffraction(Sample asample, DiffrDataFile datafile) {
		DataFileSet datafileset = datafile.getDataFileSet();
		if (getFilePar().isComputingDerivate()) {
//      System.out.println("refreshing derivative: " + this.toXRDcatString());
			for (int ij = 0; ij < getFilePar().getActiveSample().phasesNumber(); ij++) {
				double expfit[] = new double[datafile.getTotalNumberOfData()];
				int minmaxindex[] = computeReflectionIntensity(asample, datafileset, true,
						expfit, Constants.ENTIRE_RANGE, Constants.COMPUTED,
						Constants.COMPUTED, Constants.COMPUTED, false,
						getFilePar().getActiveSample().getPhase(ij), datafile);
				for (int j = minmaxindex[0]; j < minmaxindex[1]; j++)
					datafile.addtoPhasesFit(j, expfit[j]);
			}
		} else {
//      System.out.println("refreshing: " + this.toXRDcatString());
			for (int ij = 0; ij < getFilePar().getActiveSample().phasesNumber(); ij++) {
//        System.out.println("Phase: " + getFilePar().getActiveSample().getPhase(ij).toXRDcatString());
				double expfit[] = new double[datafile.getTotalNumberOfData()];
				int minmaxindex[] = computeReflectionIntensity(asample, datafileset, true,
						expfit, Constants.ENTIRE_RANGE, Constants.COMPUTED,
						Constants.COMPUTED, Constants.COMPUTED, false,
						getFilePar().getActiveSample().getPhase(ij), datafile);
//        System.out.println("indices: " + minmaxindex[0] + " " + minmaxindex[1] + " " + expfit[1000]);
				for (int j = minmaxindex[0]; j < minmaxindex[1]; j++)
					datafile.addtoPhasesFit(j, expfit[j], ij);
			}
		}
	}

	public int[] computeReflectionIntensity(double[] x, double[] expfit, Sample asample, DataFileSet datafileset, boolean computeBroadening,
	                                        double rangefactor, int computeTexture,
	                                        int computeStrain, int computeFhkl, boolean leBailExtraction,
	                                        Phase aphase, DiffrDataFile datafile) {

		Vector<Peak> peaklist = datafileset.getPeakList();
		Instrument ainstrument = datafileset.getInstrument();
		FilePar filepar = getFilePar();
		OutputStream out = null;
		boolean logOutput = false;
		PrintStream printStream = null;
		ByteArrayOutputStream baos = null;
//    Instrument ainstrument = getDataFileSet().getInstrument();
		double cutoff = datafileset.getPeakCutoffD() * rangefactor;
		if (!datafile.increasingX()) {
			cutoff = -cutoff;
		}
		int[] tmpminmax = new int[2];
		int[] minmaxindex = new int[2];
		minmaxindex[0] = datafile.finalindex - 1;
		minmaxindex[1] = datafile.startingindex;
		arraycopy(minmaxindex, 0, tmpminmax, 0, 2);

		for (int i = 0; i < peaklist.size(); i++) {
			if (aphase == null || peaklist.elementAt(i).getPhase() == aphase) {

				double[] hwhm_i, eta, const1, const2, wave, thwhm, teta;
				int[] minindex, maxindex;
				Reflection refl = peaklist.elementAt(i).getReflex();

				String phase_name = aphase.toXRDcatString();
				while (phase_name.length() < 20)
					phase_name += " ";
				phase_name = phase_name.substring(0, 20);
//    int dataindex = diffrDataFile.getIndex();
//    int datasetIndex = diffrDataFile.getDataFileSet().getIndex();

//    if (computeBroadening)
//        addInstrumentalBroadening(ainstrument.getInstrumentalBroadeningAt(getMeanPosition(), diffrDataFile));
//      addInstrumentalBroadening(refl.getInstBroadFactor(dataindex));

				int nrad = ainstrument.getRadiationType().getLinesCount();
				int totalLines = diffrDataFile.positionsPerPattern * nrad;
				double[] finalposition = new double[totalLines];
				double[][] intensity = new double[3][totalLines];
				hwhm_i = new double[totalLines];
				eta = new double[totalLines];
				double[][] actualPosition = new double[3][totalLines];
				const1 = new double[totalLines];
				const2 = new double[totalLines];
				wave = new double[nrad];
				minindex = new int[totalLines];
				maxindex = new int[totalLines];

				double[][][] positions = diffrDataFile.getPositions(aphase);
				double[][] absDetectorCorrection = new double[nrad][diffrDataFile.positionsPerPattern];
				int ipv = 0;
				for (int i = 0; i < nrad; i++) {
					double energy = Constants.ENERGY_LAMBDA / getRadiationWavelength(i) * 0.001;
					for (int j = 0; j < diffrDataFile.positionsPerPattern; j++) {
						finalposition[ipv++] = positions[i][getOrderPosition()][j];
//						int pointIndex = diffrDataFile.getOldNearestPoint(positions[i][getOrderPosition()][j]);
//						absDetectorCorrection[i][j] = ainstrument.getDetector().getAbsorptionCorrection(diffrDataFile, pointIndex, energy);
					}
				}

				double[][][] hwhm_eta = diffrDataFile.getBroadFactors(aphase);
				double[] deff = diffrDataFile.getCrystallitesMicrostrains(aphase)[0][getOrderPosition()];

//	  for (int j = 0; j < diffrDataFile.positionsPerPattern; j++) {
				thwhm = hwhm_eta[0][getOrderPosition()];
				teta = hwhm_eta[1][getOrderPosition()];
//	  }
				double intensitySingle = getScaleFactor();
				double[] textureFactor;
				double Fhkl;

				switch (computeTexture) {
					case Constants.COMPUTED:
						textureFactor = diffrDataFile.getTextureFactors(aphase)[1][getOrderPosition()];
						break;
					case Constants.EXPERIMENTAL:
						textureFactor = diffrDataFile.getTextureFactors(aphase)[0][getOrderPosition()];
						break;
					case Constants.UNITARY:
					default:
						textureFactor = new double[diffrDataFile.positionsPerPattern];
						for (int i = 0; i < diffrDataFile.positionsPerPattern; i++)
							textureFactor[i] = 1.0;
				}
				for (int i = 0; i < diffrDataFile.positionsPerPattern; i++)
					if (Double.isNaN(textureFactor[i]))
						textureFactor[i] = 1.0;
//		if (getOrderPosition() == 2)
//	    System.out.println(diffrDataFile.getLabel() + ", texture factor: " + textureFactor[0]);
				switch (computeFhkl) {
					case Constants.COMPUTED:
						Fhkl = diffrDataFile.getDataFileSet().getStructureFactors(aphase)[1][getOrderPosition()];
						break;
					case Constants.EXPERIMENTAL:
						Fhkl = diffrDataFile.getDataFileSet().getStructureFactors(aphase)[0][getOrderPosition()];
						break;
					case Constants.UNITARY:
						Fhkl = 99.0;
						break;
					default:
						Fhkl = 88.0;
				}

				double[] shapeAbs = diffrDataFile.getShapeAbsFactors(aphase, getOrderPosition());
				double asyConst1 = aphase.getActivePlanarDefects().getPlanarDefectAsymmetryConstant1(getReflex());
				double asyConst2 = aphase.getActivePlanarDefects().getPlanarDefectAsymmetryConstant2(getReflex());
				double planar_asymmetry = aphase.getActivePlanarDefects().getPlanarDefectAsymmetry(getReflex());

//	  System.out.println(" Total " + diffrDataFile.startingindex + " " + diffrDataFile.finalindex);
//	  System.out.println(" Range " + diffrDataFile.getXData(diffrDataFile.startingindex) + " " +
//			  diffrDataFile.getXData(diffrDataFile.finalindex));

//    Fhklist = new double[totalLines];
				double[] radiationWeight = new double[nrad];
				int principalRad = 0;
				radiationWeight[0] = getRadiationWeight(0);
				double weight = radiationWeight[0];
				for (int i1 = 1; i1 < nrad; i1++) {
					radiationWeight[i1] = getRadiationWeight(i1);
					if (leBailExtraction && weight < radiationWeight[i1]) {
						weight = radiationWeight[i1];
						principalRad = i1;
					}
				}
				double[] lorentzPolarization = diffrDataFile.getLorentzPolarization(aphase, getOrderPosition());
				ipv = 0;
				for (int i = 0; i < nrad; i++) {
					for (int j = 0; j < diffrDataFile.positionsPerPattern; j++) {
						if (radiationWeight[i] > 0.0) {
							double tmpIntensity = (intensitySingle * textureFactor[j] * shapeAbs[j] * Fhkl *
									radiationWeight[i] * aphase.getScaleFactor() * lorentzPolarization[j] * absDetectorCorrection[i][j]);
							if (const2[ipv] != 0.0) {
								for (int index = 0; index < 3; index++)
									intensity[index][ipv] = tmpIntensity * getReflex().pd_deltaIndex[index];
							} else {
								intensity[0][ipv] = tmpIntensity;
							}
							hwhm_i[ipv] = 1.0 / thwhm[j];
							eta[ipv] = teta[j];

							minindex[ipv] = diffrDataFile.getOldNearestPoint(finalposition[ipv] - thwhm[j] * cutoff);
							maxindex[ipv] = diffrDataFile.getOldNearestPoint(finalposition[ipv] + thwhm[j] * cutoff) + 1;

							if (!leBailExtraction || (leBailExtraction && i == principalRad)) {
								if (minmaxindex[0] > minindex[ipv])
									minmaxindex[0] = minindex[ipv];
								if (minmaxindex[1] < maxindex[ipv])
									minmaxindex[1] = maxindex[ipv];
							}

							const1[ipv] = asyConst1;
							const2[ipv] = asyConst2;
							wave[i] = getRadiationWavelength(i);

							if (const2[ipv] != 0.0) {
								for (int index = 0; index < 3; index++)
									actualPosition[index][ipv] = getPositionChangeForPlanarDefectDisplacement(finalposition[ipv], index);
							} else {
								actualPosition[0][ipv] = finalposition[ipv];
							}

						} else {
							intensity[0][ipv] = 0.0f;
//						Fhkl = 1.0f;
							hwhm_i[ipv] = 1.0f;
							eta[ipv] = 0.0f;
							actualPosition[0][ipv] = 0.0f;
							minindex[ipv] = 0;
							maxindex[ipv] = 4;
							const1[ipv] = 0.0f;
							const2[ipv] = 0.0f;
						}
						ipv++;
					}
				}

//    diffrDataFile.computeLorentzPolarization(ainstrument, asample, actualPosition, intensity);
// planar defects
//		aphase.getActiveTDSModel().computeTDS(diffrDataFile, expfit, this, intensity[0], Fhkl, actualPosition[0], minmaxindex);


				peaklist.elementAt(i).computeFunctions(x, expfit, int[] minindex, int[] maxindex,
				double[][] intensity, double[] eta, double[] hwhm_i, double[][] position,
				double[] const1, double[] const2, double[] wave, boolean dspacingBase,
				boolean energyDispersive, boolean increasingX, double planar_asymmetry,
				double[] deff);
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

	}*/

	public void computeasymmetry(Sample asample, DiffrDataFile datafile) {
		computeasymmetry(asample, datafile, datafile.phasesfit, datafile.startingindex, datafile.finalindex - 1);
		if (!getFilePar().isComputingDerivate()) {
			for (int i = 0; i < datafile.phaseFit.size(); i++)
				computeasymmetry(asample, datafile, (double[]) datafile.phaseFit.elementAt(i), datafile.startingindex, datafile.finalindex - 1);
		}
		refreshComputation = false;
	}

	public void computeasymmetry(Sample asample, DiffrDataFile datafile, double afit[], int min, int max) {

		Instrument ainstrument = datafile.getDataFileSet().getInstrument();

		ainstrument.getInstrumentBroadening().computeAsymmetry(datafile, asample, afit, min, max);

		for (int j = min; j < max; j++) {
//      System.out.print("Before: " + afit[j]);
			afit[j] *= datafile.computeAngularIntensityCorrection(asample, ainstrument, j);
//      System.out.println(", after: " + afit[j]);
		}
	}

}
