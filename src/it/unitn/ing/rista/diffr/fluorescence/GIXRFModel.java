/*
 * @(#)TXRFluorescence.java created May 28, 2013 Hermanville-sur-Mer
 *
 * Copyright (c) 2013 Luca Lutterotti All Rights Reserved.
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
 * The GIXRFModel is a class to calculate the Grazing Incidence
 * fluorescence model by De Boer.
 * D. K. G. de Boer, Phys. Review B, 44[2], 498, 1991.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Feb 21, 2013 8:27:36 PM $
 * @since JDK1.1
 */
public class GIXRFModel extends Fluorescence {

	public static String[] diclistc = {
			"_maud_gixrf_smoothing_points",
			"_maud_gixrf_smoothing_divergence",
			"_fluorescence_atom_intensity_correction"
	};
	public static String[] diclistcrm = {
			"_maud_gixrf_smoothing_points",
			"_maud_gixrf_smoothing_divergence",
			"_fluorescence_atom_intensity_correction"
	};

	public static String[] classlistc = {};
	public static String[] classlistcs = {};

	public static String modelID = "GIXRF";
	public static String descriptionID = "Grazing Incidence X-Ray Fluorescence model, for bulk or multilayers";

	public GIXRFModel(XRDcat obj, String alabel) {
		super(obj, alabel);
		initXRD();
		identifier = modelID;
		IDlabel = modelID;
		description = descriptionID;
	}

	public GIXRFModel(XRDcat afile) {
		this(afile, modelID);
	}

	public GIXRFModel() {
		identifier = modelID;
		IDlabel = modelID;
		description = descriptionID;
	}

	public void initConstant() {
		Nstring = 1;
		Nstringloop = 0;
		Nparameter = 1;
		Nparameterloop = 1;
		Nsubordinate = 0;
		Nsubordinateloop = 0;
	}

	public void initDictionary() {
		for (int i = 0; i < totsubordinateloop; i++)
			diclist[i] = diclistc[i];
		System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
		for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
			classlist[i] = classlistc[i];
		for (int i = 0; i < totsubordinate - totparameterloop; i++)
			classlists[i] = classlistcs[i];
	}

	int maxAtomsNumber = 130;

	public void initParameters() {
		super.initParameters();
		setString(0, "11");
		initializeParameter(0, 0.001, 0.0001, 0.1);
		for (int i = 0; i < maxAtomsNumber; i++)
			addparameterloopField(0, new Parameter(this, getParameterString(0, i), 1,
				ParameterPreferences.getDouble(getParameterString(0, i) + ".min", 0.8),
				ParameterPreferences.getDouble(getParameterString(0, i) + ".max", 1.2)));
	}

	public void updateStringtoDoubleBuffering(boolean firstLoading) {
		super.updateStringtoDoubleBuffering(false);

		phiIntegrationNumber = Integer.parseInt(getString(0));
	}

	public void updateParametertoDoubleBuffering(boolean firstLoading) {
		// to be implemented by subclasses

		if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
			return;
		super.updateParametertoDoubleBuffering(false);

		phiDelta = getParameterValue(0);
		if (phiIntegrationNumber > 1)
			phiStep = phiDelta / (phiIntegrationNumber - 1);
		else
			phiStep = 0;
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

	int phiIntegrationNumber = 1;
	double phiDelta = 0.0;
	double phiStep = 0; // phiDelta / (phiIntegrationNumber - 1);

	public void computeFluorescence(Sample asample) {
    
    final DataFileSet theDataset = getDataFileSet();
    
    int datafilenumber = theDataset.activedatafilesnumber();

		final Sample theSample = asample;

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
							computeFluorescence(theSample, datafile);
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

		} else
			for (int k = 0; k < datafilenumber; k++) {
				DiffrDataFile datafile = theDataset.getActiveDataFile(k);
				computeFluorescence(theSample, datafile);
				computeasymmetry(theSample, datafile);
			}

	}

	public void computeFluorescence(Sample asample, DiffrDataFile adatafile) {

		XRayDataSqLite.checkMinimumEnergy();

		Instrument ainstrument = adatafile.getDataFileSet().getInstrument();
		Detector detector = ainstrument.getDetector();
		Geometry geometry = ainstrument.getGeometry();
		double incidentIntensity = ainstrument.getIntensityForFluorescence();
		double sampleLinearArea = detector.getGeometryCorrection(
				geometry.getBeamOutCorrection(adatafile, asample)) / adatafile.sintheta;
//		incidentIntensity *= sampleLinearArea;

		double polarization = ainstrument.getGeometry().getPolarizationAmount();
		double polarizationAngle = ainstrument.getGeometry().getPolarizationAngle();
		double cos2polarization = MoreMath.cosd(polarizationAngle);
		cos2polarization *= cos2polarization;
		double s_factor = 0.5 - 0.5 * polarization * (1.0 - cos2polarization);
		double p_factor = 0.5 - 0.5 * polarization * cos2polarization;

		double[] xEnergy = adatafile.getXrangeInEnergy();
		int numberOfPoints = xEnergy.length;
		double maxEnergyInKeV = xEnergy[numberOfPoints - 1] * 0.001 * 1.1;
//    System.out.println(xEnergy[0] + " " + xEnergy[numberOfPoints - 1]);

		int layersNumber = asample.numberOfLayers + 1;

		double[][] Nz = new double[layersNumber][2];
		double[][] Nz_eps = new double[layersNumber][2];
		double[][] Rs = new double[layersNumber][2];
		double[][] Ts = new double[layersNumber][2];
		double[][] Rp = new double[layersNumber][2];
		double[][] Tp = new double[layersNumber][2];
		double[][] As = new double[layersNumber][2];
		double[][] Ap = new double[layersNumber][2];

		double[][] Xs = new double[layersNumber][2];
		double[][] Xp = new double[layersNumber][2];
		double[][] A2Xs = new double[layersNumber][2];
		double[][] A2Xp = new double[layersNumber][2];
		double[][] A2XRs = new double[layersNumber][2];
		double[][] A2XRp = new double[layersNumber][2];

		double[] fluorescence = new double[numberOfPoints];

		double[][] Er = new double[layersNumber][2];
		double[][] Et = new double[layersNumber][2];
		double[][] Hr = new double[layersNumber][2];
		double[][] Ht = new double[layersNumber][2];

		double[][] complexPermittivity = new double[layersNumber][2];

		double[] incidentDiffracted = adatafile.getIncidentAndDiffractionAngles(0);
//		System.out.println(incidentDiffracted[1]);
//	  incidentDiffracted[0] *= Constants.DEGTOPI;

		double[] cosPhi2 = new double[phiIntegrationNumber];
		for (int i = 0; i < phiIntegrationNumber; i++) {
			cosPhi2[i] = Math.cos(incidentDiffracted[0] - phiDelta + phiStep * i);
			cosPhi2[i] *= cosPhi2[i];
		}
		double sinPhi = Math.sqrt(1.0 - cosPhi2[phiIntegrationNumber / 2]);
		incidentDiffracted[1] = ainstrument.getDetector().getThetaDetector(adatafile, 0) * Constants.DEGTOPI;
		double sinPhid = Math.sin(incidentDiffracted[1]);

		RadiationType radType = ainstrument.getRadiationType();
		int rad_lines = radType.getLinesCountForFluorescence();
		int sub20 = radType.getSubdivision(); //MaudPreferences.getInteger("xrf_detector.energySubdivision", 20);

		int initialContent = 100;
		double source_intensity = ((XRFDetector) ainstrument.getDetector()).getSourceSpectrumIntensity();
		if (source_intensity > 0 && initialContent < rad_lines) {
			initialContent = rad_lines;
		}
		Vector<FluorescenceLine> fluorescenceLines = new Vector<FluorescenceLine>(initialContent, 10);
		double[] cPerm;
		double lastEnergyInKeV = 0;
		double lastEnergyIntensity = 0;
//		System.out.println(rad_lines + " " + phiIntegrationNumber);
		for (int ej = 0; ej < rad_lines; ej++) {
			double lambda = radType.getRadiationWavelengthForFluorescence(ej);
			double energy_intensity = radType.getRadiationWeightForFluorescence(ej);
			double constant1 = 2.0 * Constants.PI / lambda * 1E8;
			double energy = Constants.ENERGY_LAMBDA / lambda;    // in eV
			double energyInKeV = energy * 0.001;
//			System.out.println(ej + " " + lambda + " " + energy_intensity + ", KeV " + energyInKeV);
//	  System.out.print((incidentDiffracted[0] * Constants.PITODEG) + " ");
			energy_intensity /= phiIntegrationNumber;
			for (int phi_i = 0; phi_i < phiIntegrationNumber; phi_i++) {
				for (int j = 0; j < layersNumber; j++) {
					Layer layer = asample.getlayer(j - 1);
					if (layer == null) {
						complexPermittivity[j][0] = 0;
						complexPermittivity[j][1] = 0;
					} else {
						cPerm = layer.getComplexPermittivityDiff(energyInKeV);
						complexPermittivity[j][0] = cPerm[0];
						complexPermittivity[j][1] = cPerm[1];
					}
/*			double cost = sinPhi2 - complexPermittivity[j][0];
			Nz[j][0] = Math.sqrt(0.5 * (cost + Math.sqrt(cost * cost + MoreMath.pow(complexPermittivity[j][1], 2)))); //sqrtRoot[0];
			Nz[j][1] = complexPermittivity[j][1] / Nz[j][0];*/

					double[] eps = new double[2];
					eps[0] = 1.0 - complexPermittivity[j][0];
					eps[1] = -complexPermittivity[j][1];
					double[] r1 = MoreMath.complexSqrt(eps[0] - cosPhi2[phi_i], eps[1]);
					Nz[j][0] = r1[0];
					Nz[j][1] = r1[1];
					r1 = MoreMath.complexDivide(Nz[j], eps);
					Nz_eps[j][0] = r1[0];
					Nz_eps[j][1] = r1[1];

					double thickness = 0;
					if (layer != null && j != layersNumber - 1)
						thickness = layer.getThicknessInCm();
//		  System.out.print(thickness + " ");
					thickness *= constant1;
					r1 = MoreMath.complexExp(thickness * Nz[j][1], -thickness * Nz[j][0]);
					As[j][0] = r1[0];
					As[j][1] = r1[1];
					r1 = MoreMath.complexExp(thickness * Nz_eps[j][1], -thickness * Nz_eps[j][0]);
					Ap[j][0] = r1[0];
					Ap[j][1] = r1[1];
//        System.out.print(eps[0] + " " + eps[1] + " " + Nz[j][0] + " " + Nz[j][1] + " ");
/*		  System.out.print(j + " ");
		  System.out.println("Nz = " + Nz[j][0] + " i * " + Nz[j][1] + " ");*/
				}
//		System.out.println();

				for (int j = 0; j < layersNumber - 1; j++) {
					Layer layer = asample.getlayer(j);
					double roughness = 0;
					if (layer != null)
						roughness = layer.getRoughnessInCm();
					double roughnessExponent = constant1 * roughness;
					roughnessExponent *= roughnessExponent;
					double[] diffN = MoreMath.complexAdd(Nz[j][0], Nz[j][1], -Nz[j + 1][0], -Nz[j + 1][1]);
					double[] sumN = MoreMath.complexAdd(Nz[j], Nz[j + 1]);
					double[] multN = MoreMath.complexMultiply(Nz[j], Nz[j + 1]);
					double r1 = -2.0 * roughnessExponent;
					multN[0] *= r1;
					multN[1] *= r1;
					double[] s1 = MoreMath.complexExp(multN);
					double[] firstR = MoreMath.complexDivide(diffN, sumN);
					firstR = MoreMath.complexMultiply(firstR, s1);
					Rs[j][0] = firstR[0];
					Rs[j][1] = firstR[1];
					double r2 = roughnessExponent * 0.5;
					firstR = MoreMath.complexMultiply(diffN, diffN);
					firstR[0] *= r2;
					firstR[1] *= r2;
					s1 = MoreMath.complexExp(firstR);
					firstR = MoreMath.complexDivide(Nz[j], sumN);
					firstR = MoreMath.complexMultiply(firstR, s1);
					Ts[j][0] = 2.0 * firstR[0];
					Ts[j][1] = 2.0 * firstR[1];

					diffN = MoreMath.complexAdd(Nz_eps[j][0], Nz_eps[j][1], -Nz_eps[j + 1][0], -Nz_eps[j + 1][1]);
					sumN = MoreMath.complexAdd(Nz_eps[j], Nz_eps[j + 1]);
					multN = MoreMath.complexMultiply(Nz_eps[j], Nz_eps[j + 1]);
					multN[0] *= r1;
					multN[1] *= r1;
					s1 = MoreMath.complexExp(multN);
					firstR = MoreMath.complexDivide(diffN, sumN);
					firstR = MoreMath.complexMultiply(firstR, s1);
					Rp[j][0] = firstR[0];
					Rp[j][1] = firstR[1];
					firstR = MoreMath.complexMultiply(diffN, diffN);
					firstR[0] *= r2;
					firstR[1] *= r2;
					s1 = MoreMath.complexExp(firstR);
					firstR = MoreMath.complexDivide(Nz_eps[j], sumN);
					firstR = MoreMath.complexMultiply(firstR, s1);
					Tp[j][0] = 2.0 * firstR[0];
					Tp[j][1] = 2.0 * firstR[1];

//		  System.out.print(R[j][0] + " " + R[j][1] + " " + T[j][0] + " " + T[j][1] + " | ");

/*		  System.out.print(j + " ");
		  System.out.print("T = " + T[j][0] + " i * " + T[j][1] + " ");
		  System.out.println(", R = " + R[j][0] + " i * " + R[j][1] + " ");*/
				}

				for (int j = asample.numberOfLayers - 1; j >= 0; j--) {
					double[] A2s = MoreMath.complexMultiply(As[j + 1], As[j + 1]);
					double[] r1 = MoreMath.complexMultiply(A2s, Xs[j + 1]);
					A2Xs[j + 1][0] = r1[0];
					A2Xs[j + 1][1] = r1[1];
					r1 = MoreMath.complexMultiply(A2Xs[j + 1], Rs[j]);
					A2XRs[j + 1][0] = r1[0];
					A2XRs[j + 1][1] = r1[1];
					r1 = MoreMath.complexAdd(Rs[j], A2Xs[j + 1]);
					r1 = MoreMath.complexDivide(r1[0], r1[1], 1.0 + A2XRs[j + 1][0], A2XRs[j + 1][1]);
					Xs[j][0] = r1[0];
					Xs[j][1] = r1[1];

					double[] A2p = MoreMath.complexMultiply(Ap[j + 1], Ap[j + 1]);
					r1 = MoreMath.complexMultiply(A2p, Xp[j + 1]);
					A2Xp[j + 1][0] = r1[0];
					A2Xp[j + 1][1] = r1[1];
					r1 = MoreMath.complexMultiply(A2Xp[j + 1], Rp[j]);
					A2XRp[j + 1][0] = r1[0];
					A2XRp[j + 1][1] = r1[1];
					r1 = MoreMath.complexAdd(Rp[j], A2Xp[j + 1]);
					r1 = MoreMath.complexDivide(r1[0], r1[1], 1.0 + A2XRp[j + 1][0], A2XRp[j + 1][1]);
					Xp[j][0] = r1[0];
					Xp[j][1] = r1[1];
//		  System.out.print(X[j][0] + " " + X[j][1] + " " + A2X[j+1][0] + " " + A2X[j+1][1] + " | ");

/*		  System.out.print(j + " ");
		  System.out.print(X[j][0] + " i * " + X[j][1] + " ");
		  System.out.println(A2X[j+1][0] + " i * " + A2X[j+1][1] + " ");*/
				}
				double[] A2s = MoreMath.complexMultiply(As[0], As[0]);
				double[] r1 = MoreMath.complexMultiply(A2s, Xs[0]);
				A2Xs[0][0] = r1[0];
				A2Xs[0][1] = r1[1];
				double[] A2p = MoreMath.complexMultiply(Ap[0], Ap[0]);
				r1 = MoreMath.complexMultiply(A2p, Xp[0]);
				A2Xp[0][0] = r1[0];
				A2Xp[0][1] = r1[1];
				Et[0][0] = 1.0;
				Ht[0][0] = 1.0;
				for (int j = 0; j < layersNumber - 1; j++) {
					r1 = MoreMath.complexMultiply(As[j], Et[j]);
					r1 = MoreMath.complexMultiply(r1, Ts[j]);
					if (j != layersNumber - 2)
						r1 = MoreMath.complexDivide(r1[0], r1[1], 1.0 + A2XRs[j + 1][0], A2XRs[j + 1][1]);
					Et[j + 1][0] = r1[0];
					Et[j + 1][1] = r1[1];
					r1 = MoreMath.complexMultiply(A2Xs[j], Et[j]);
					Er[j][0] = r1[0];
					Er[j][1] = r1[1];

					r1 = MoreMath.complexMultiply(Ap[j], Ht[j]);
					r1 = MoreMath.complexMultiply(r1, Tp[j]);
					if (j != layersNumber - 2)
						r1 = MoreMath.complexDivide(r1[0], r1[1], 1.0 + A2XRp[j + 1][0], A2XRp[j + 1][1]);
					Ht[j + 1][0] = r1[0];
					Ht[j + 1][1] = r1[1];
					r1 = MoreMath.complexMultiply(A2Xp[j], Ht[j]);
					Hr[j][0] = r1[0];
					Hr[j][1] = r1[1];
//        System.out.print((Et[j][0] * Et[j][0]) + " " + (Et[j][1] * Et[j][1]) + " " +
//		        (Er[j][0] * Er[j][0]) + " " + (Er[j][1] * Er[j][1]) + " ");
/*		  System.out.print(j + " ");
			System.out.print(Er[j][0] + " i*" + Er[j][1] + " ");
		  System.out.println(Et[j][0] + " i*" + Et[j][1] + " ");*/
				}
				r1 = MoreMath.complexMultiply(A2Xs[layersNumber - 1], Et[layersNumber - 1]);
				Er[layersNumber - 1][0] = r1[0];
				Er[layersNumber - 1][1] = r1[1];
				r1 = MoreMath.complexMultiply(A2Xp[layersNumber - 1], Ht[layersNumber - 1]);
				Hr[layersNumber - 1][0] = r1[0];
				Hr[layersNumber - 1][1] = r1[1];

				double[] rE = MoreMath.complexConjugate(Er[0]);
				rE = MoreMath.complexMultiply(Er[0], rE);
				double Eint = s_factor * MoreMath.complexAbs(rE);
				rE = MoreMath.complexConjugate(Hr[0]);
				rE = MoreMath.complexMultiply(Hr[0], rE);
				Eint += p_factor * MoreMath.complexAbs(rE);
//		System.out.print(Eint);

				for (int j1 = 0; j1 < layersNumber - 1; j1++) {
					int j = j1 + 1;
					for (int i = 0; i < numberOfPoints; i++)
						fluorescence[i] = 0.0;
					Layer layer = asample.getlayer(j1);
					double thickness = layer.getThicknessInCm();
/*		  double thickness_or_zero = thickness;
		  if (j1 == layersNumber - 2)
			  thickness_or_zero = 0;*/
					double layerAbsorption = layer.getAbsorptionForXray(energyInKeV) * layer.getDensity();

					double[] AE = new double[4];
					double[] bE = new double[4];
//  s polarization
					double cost1 = constant1 * complexPermittivity[j][1] / layerAbsorption;

					double s_cost1 = s_factor * cost1;
					AE[0] = s_cost1 * (Et[j][0] * Et[j][0] + Et[j][1] * Et[j][1]);
					AE[1] = s_cost1 * (Er[j][0] * Er[j][0] + Er[j][1] * Er[j][1]);
					AE[2] = 2.0 * s_cost1 * (Et[j][0] * Er[j][0] + Et[j][1] * Er[j][1]);    // no 2.0 in Giancarlo
					AE[3] = 2.0 * s_cost1 * (-Et[j][1] * Er[j][0] + Et[j][0] * Er[j][1]);
//  p polarization
					cost1 = -2.0 * constant1 * Nz[j][1] * Nz_eps[j][0] / layerAbsorption;
					double p_cost1 = p_factor * cost1;
					AE[0] += p_cost1 * (Ht[j][0] * Ht[j][0] + Ht[j][1] * Ht[j][1]);
					AE[1] += p_cost1 * (Hr[j][0] * Hr[j][0] + Hr[j][1] * Hr[j][1]);
					cost1 = -4.0 * constant1 * Nz[j][0] * Nz_eps[j][1] / layerAbsorption;
					p_cost1 = p_factor * cost1;
					AE[2] += p_cost1 * (Ht[j][0] * Hr[j][0] + Ht[j][1] * Hr[j][1]);
					AE[3] += p_cost1 * (-Ht[j][1] * Hr[j][0] + Ht[j][0] * Hr[j][1]);
//  sum of s and p
					bE[0] = -2.0 * constant1 * Nz[j][1];
					bE[1] = -bE[0];
					bE[2] = 0;
					bE[3] = -2.0 * constant1 * Nz[j][0];

/*		  System.out.print("Layer " + j + ": A and b coeffs ");
		  for (int i = 0; i < 4; i++)
			  System.out.print(AE[i] + " ");
		  for (int i = 0; i < 4; i++)
			  System.out.print(bE[i] + " ");
		  System.out.println();*/

					Vector<AtomQuantity> chemicalComposition = layer.getChemicalComposition();

					double density = layer.getDensity();
//			System.out.print("Layer: " + j);
					if (source_intensity > 0) {
						if (radType instanceof XrayEbelTubeRadiation && ej >= ((XrayEbelTubeRadiation) radType).getNumberOfCharacteristicsLines()) {
							double stepEnergy = (energyInKeV - lastEnergyInKeV) / sub20;
							double stepIntEnergy = (energy_intensity - lastEnergyIntensity) / sub20;
							double subEnergy = lastEnergyInKeV;
							double subEnergyInt = lastEnergyIntensity;
							for (int sub_i = 0; sub_i < sub20; sub_i++) {
								subEnergy += stepEnergy;
								subEnergyInt += stepIntEnergy;
								FluorescenceLine transfertLine = new FluorescenceLine(subEnergy, -1, 0, "source");
								double absorptionLineLambda = layer.getAbsorptionForXray(subEnergy) * density;
								double totalIntensity = Math.exp(-layer.getOverLayerAbsorptionForXray(subEnergy) * density / sinPhid)
										* subEnergyInt;
								double totalRe = 0.0;
								double mhuOverSinD = absorptionLineLambda / sinPhid;
//						System.out.println("Layer: " + j1 + ", atom: " + k + ", line: " + ij + ", energy " + lineEnergyKeV + ", intensity " + totalIntensity + ", mhuOverSinD " + mhuOverSinD);
								for (int m = 0; m < 2; m++) {
									double bm_mhu = bE[m] + mhuOverSinD;
//					  System.out.println(bm_mhu);
									double expm = -bm_mhu * thickness; //_or_zero;
									if (expm > -Double.MAX_EXPONENT / 2 && expm < Double.MAX_EXPONENT / 2)
										expm = Math.exp(expm);
									else {
										if (expm > 0)
											expm = Double.MAX_VALUE / 2;
										else
											expm = 0;
									}
									totalRe += AE[m] * ((1.0 - expm) / bm_mhu);
//							System.out.println("m bm be Ae: " + m + " " + bm_mhu + " " + bE[m] + " " + AE[m] + " " + expm + " " + totalRe);
								}
								double re1 = -thickness * (bE[2] + mhuOverSinD);
								double im1 = -thickness * bE[3];
								double[] res = MoreMath.complexExp(re1, im1);
								res[0] = 1.0 - res[0];
								res[1] = -res[1];
								res = MoreMath.complexDivide(res[0], res[1], bE[2] + mhuOverSinD, bE[3]);
								totalRe += MoreMath.complexMultiply(AE[2], AE[3], res[0], res[1])[0];
								totalRe *= density;
//  				  System.out.println(AE[2] + " " + AE[3] + " " + totalRe);
//						System.out.println("Layer: " + j + " " + chemicalComposition.elementAt(k).label + " " + lineEnergyKeV + " " + line.getIntensity() + " " + atomsQuantities + " " + totalRe + " " + totalIntensity + " " + detectorAbsorption + " " + detectorEfficiency + " " + getIntensityCorrection(atomNumber));
								double detectorAbsorption = ((XRFDetector) detector).computeAbsorptionForLineWithEnergy(subEnergy);
								double detectorEfficiency = ((XRFDetector) detector).computeDetectorEfficiency(subEnergy);
								double areaCorrection = ((XRFDetector) detector).getAreaCorrection(sampleLinearArea);
								transfertLine.setIntensity(transfertLine.getIntensity() * totalRe * totalIntensity * source_intensity *
										detectorAbsorption * detectorEfficiency * areaCorrection);
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
						} else {
							double subEnergy = energyInKeV;
							double subEnergyInt = energy_intensity;
							FluorescenceLine transfertLine = new FluorescenceLine(subEnergy, -1, 0, "source");
							double absorptionLineLambda = layer.getAbsorptionForXray(subEnergy) * density;
							double totalIntensity = Math.exp(-layer.getOverLayerAbsorptionForXray(subEnergy) * density / sinPhid)
									* subEnergyInt;
							double totalRe = 0.0;
							double mhuOverSinD = absorptionLineLambda / sinPhid;
//						System.out.println("Layer: " + j1 + ", atom: " + k + ", line: " + ij + ", energy " + lineEnergyKeV + ", intensity " + totalIntensity + ", mhuOverSinD " + mhuOverSinD);
							for (int m = 0; m < 2; m++) {
								double bm_mhu = bE[m] + mhuOverSinD;
//					  System.out.println(bm_mhu);
								double expm = -bm_mhu * thickness; //_or_zero;
								if (expm > -Double.MAX_EXPONENT / 2 && expm < Double.MAX_EXPONENT / 2)
									expm = Math.exp(expm);
								else {
									if (expm > 0)
										expm = Double.MAX_VALUE / 2;
									else
										expm = 0;
								}
								totalRe += AE[m] * ((1.0 - expm) / bm_mhu);
//							System.out.println("m bm be Ae: " + m + " " + bm_mhu + " " + bE[m] + " " + AE[m] + " " + expm + " " + totalRe);
							}
							double re1 = -thickness * (bE[2] + mhuOverSinD);
							double im1 = -thickness * bE[3];
							double[] res = MoreMath.complexExp(re1, im1);
							res[0] = 1.0 - res[0];
							res[1] = -res[1];
							res = MoreMath.complexDivide(res[0], res[1], bE[2] + mhuOverSinD, bE[3]);
							totalRe += MoreMath.complexMultiply(AE[2], AE[3], res[0], res[1])[0];
							totalRe *= density;
//  				  System.out.println(AE[2] + " " + AE[3] + " " + totalRe);
//						System.out.println("Layer: " + j + " " + chemicalComposition.elementAt(k).label + " " + lineEnergyKeV + " " + line.getIntensity() + " " + atomsQuantities + " " + totalRe + " " + totalIntensity + " " + detectorAbsorption + " " + detectorEfficiency + " " + getIntensityCorrection(atomNumber));
							double detectorAbsorption = ((XRFDetector) detector).computeAbsorptionForLineWithEnergy(subEnergy);
							double detectorEfficiency = ((XRFDetector) detector).computeDetectorEfficiency(subEnergy);
							double areaCorrection = ((XRFDetector) detector).getAreaCorrection(sampleLinearArea);
							transfertLine.setIntensity(transfertLine.getIntensity() * totalRe * totalIntensity * source_intensity *
									detectorAbsorption * detectorEfficiency * areaCorrection);
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
					}

					for (int k = 0; k < chemicalComposition.size(); k++) {
						int atomNumber = AtomInfo.retrieveAtomNumber(chemicalComposition.elementAt(k).label);
						Vector<FluorescenceLine> linesForAtom = XRayDataSqLite.getFluorescenceLinesFor(
								atomNumber, energyInKeV);
//			  System.out.println(atomNumber + " " + chemicalComposition.elementAt(k).label);
//			  Vector <double[]> photoForAtom = AtomInfo.getPhotoAbsorptionFor(atomsLabels[k]);
						double atomsQuantities = chemicalComposition.elementAt(k).quantity_weight;
						if (atomsQuantities > 0) {
							for (int ij = 0; ij < linesForAtom.size(); ij++) {
								FluorescenceLine line = linesForAtom.elementAt(ij);
								double lineEnergyKeV = line.getEnergy(); // in KeV
								if (lineEnergyKeV <= energyInKeV) {
									System.out.println(chemicalComposition.elementAt(k).label + " " + lineEnergyKeV + " " + line.getIntensity());
//				  double lineEnergy = lineEnergyKeV * 1000; // in eV
//				  double lineLambda = Constants.ENERGY_LAMBDA / lineEnergy;
									double absorptionLineLambda = layer.getAbsorptionForXray(lineEnergyKeV) * density;

									double totalIntensity = Math.exp(-layer.getOverLayerAbsorptionForXray(lineEnergyKeV) * density / sinPhid)
											* energy_intensity;
									double totalRe = 0.0;
									double mhuOverSinD = absorptionLineLambda / sinPhid;
//						System.out.println("Layer: " + j1 + ", atom: " + k + ", line: " + ij + ", energy " + lineEnergyKeV + ", intensity " + totalIntensity + ", mhuOverSinD " + mhuOverSinD);
									for (int m = 0; m < 2; m++) {
										double bm_mhu = bE[m] + mhuOverSinD;
//					  System.out.println(bm_mhu);
										double expm = -bm_mhu * thickness; //_or_zero;
										if (expm > -Double.MAX_EXPONENT / 2 && expm < Double.MAX_EXPONENT / 2)
											expm = Math.exp(expm);
										else {
											if (expm > 0)
												expm = Double.MAX_VALUE / 2;
											else
												expm = 0;
										}
										totalRe += AE[m] * ((1.0 - expm) / bm_mhu);
//							System.out.println("m bm be Ae: " + m + " " + bm_mhu + " " + bE[m] + " " + AE[m] + " " + expm + " " + totalRe);
									}
									double re1 = -thickness * (bE[2] + mhuOverSinD);
									double im1 = -thickness * bE[3];
									double[] res = MoreMath.complexExp(re1, im1);
									res[0] = 1.0 - res[0];
									res[1] = -res[1];
									res = MoreMath.complexDivide(res[0], res[1], bE[2] + mhuOverSinD, bE[3]);
									totalRe += MoreMath.complexMultiply(AE[2], AE[3], res[0], res[1])[0];
									totalRe *= density;
//  				  System.out.println(AE[2] + " " + AE[3] + " " + totalRe);
									double detectorAbsorption = ((XRFDetector) detector).computeAbsorptionForLineWithEnergy(lineEnergyKeV);
									double detectorEfficiency = ((XRFDetector) detector).computeDetectorEfficiency(lineEnergyKeV);
//						System.out.println("Layer: " + j + " " + chemicalComposition.elementAt(k).label + " " + lineEnergyKeV + " " + line.getIntensity() + " " + atomsQuantities + " " + totalRe + " " + totalIntensity + " " + detectorAbsorption + " " + detectorEfficiency);
									double areaCorrection = ((XRFDetector) detector).getAreaCorrection(sampleLinearArea);
//									if (lineEnergyKeV * 1000 > xEnergy[0] && lineEnergyKeV * 1000 < xEnergy[numberOfPoints - 1])
//									System.out.println("# " + lineEnergyKeV + " " + line.getIntensity() + " " + atomsQuantities + " " + (totalIntensity * totalRe) + " " + detectorAbsorption + " " +
//											detectorEfficiency + " " + areaCorrection + " " + getIntensityCorrection(atomNumber));
									line.multiplyIntensityBy(atomsQuantities * totalRe * totalIntensity * detectorAbsorption *
											detectorEfficiency * areaCorrection * getIntensityCorrection(atomNumber));
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

//										if (!getFilePar().isOptimizing() && Constants.testing) {
//											System.out.println("Adding fluorescence line for " + chemicalComposition.elementAt(k).label + ", Energy = " + line.getEnergy() + " KeV, Intensity = " + line.getIntensity());
//										}
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
			lastEnergyInKeV = energyInKeV;
			lastEnergyIntensity = energy_intensity;
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

		for (int k = 0; k < fluorescenceLines.size(); k++) {
			FluorescenceLine line = fluorescenceLines.get(k);
      java.util.Vector<double[]> broad = ainstrument.getInstrumentEnergyBroadeningAt(line.getEnergy());
//			line.setEnergy(line.getEnergy() * 1000.0); // in eV
			line.setShape(broad);
//      System.out.println(line.getEnergy() + " " + line.getIntensity());
			for (int i = 0; i < numberOfPoints; i++)
				fluorescence[i] += line.getIntensity(xEnergy[i]);
		}

		for (int i = 0; i < numberOfPoints; i++) {
			fluorescence[i] *= incidentIntensity;
			adatafile.addtoFit(i, fluorescence[i]);
//        System.out.println("Point: " + xEnergy[i] + ", intensity: " + fluorescence[i]);
		}
	}

	public double getIntensityCorrection(int atomNumber) {
		return getParameterLoopValues(0, atomNumber - 1);
	}

}
