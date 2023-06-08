/*
 * @(#)ReflectivityRecursiveParrat.java created 24/06/2013 Hermanville sur Mer
 *
 * Copyright (c) 2013 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.reflectivity;

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;

import static java.lang.System.out;

/**
 * The ReflectivityRecursiveParrat is a class to compute reflectivity using the
 * Recursive method of Parrat method.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.0 $, $Date: 2013/06/24 22:30:50 $
 * @since JDK1.1
 */

public class ReflectivityRecursiveParrat extends Reflectivity {

	static String idString = "Parrat recursive method";
	static String idDescription = "Use the Parrat recursive method for reflectivity computation";

	protected static String[] diclistc = {
			"_maud_reflectivity_scale_factor"
	};
	protected static String[] diclistcrm = {
			"_maud_reflectivity_scale_factor"
	};

	protected static String[] classlistc = {};

	protected static String[] classlistcs = {};

	public ReflectivityRecursiveParrat(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initBaseObject();
		identifier = idString;
		IDlabel = idString;
		description = idDescription;
	}

	public ReflectivityRecursiveParrat(XRDcat aobj) {
		this(aobj, idString);
	}

	public ReflectivityRecursiveParrat() {
		identifier = idString;
		IDlabel = idString;
		description = idDescription;
	}

	public void initConstant() {
		Nstring = 0;
		Nstringloop = 0;
		Nparameter = 1;
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
		for (int i = 0; i < totsubordinate - totparameterloop; i++)
			classlists[i] = classlistcs[i];
	}

	public void initParameters() {
		super.initParameters();
		parameterField[0] = new Parameter(this, getParameterString(0), 1.0,
				ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.1),
				ParameterPreferences.getDouble(getParameterString(0) + ".max", 1E9));
	}

	public void computeReflectivity(Sample asample, DataFileSet adataset) {

		int datafilenumber = adataset.activedatafilesnumber();

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
							computeReflectivity(theSample, theDataset.getActiveDataFile(j));
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
			for (int k = 0; k < datafilenumber; k++)
				computeReflectivity(theSample, theDataset.getActiveDataFile(k));

	}

	public void computeReflectivity(Sample asample, DiffrDataFile adatafile) {
//		Radiation rad = adatafile.getDataFileSet().getRadiation();
//		double lambda = rad.getWavelength().getValueD();
//		double constant1 = 2.0 * Constants.PI / lambda * 1E8;
//		double energy = Constants.ENERGY_LAMBDA / lambda;    // in eV
//		double energyInKeV = energy * 0.001;

		Instrument ainstrument = adatafile.getDataFileSet().getInstrument();

		double polarization = ainstrument.getGeometry().getPolarizationAmount();
		double polarizationAngle = ainstrument.getGeometry().getPolarizationAngle();
		double cos2polarization = MoreMath.cosd(polarizationAngle);
		cos2polarization *= cos2polarization;
		double s_factor = 0.5 - 0.5 * polarization * (1.0 - cos2polarization);
		double p_factor = 0.5 - 0.5 * polarization * cos2polarization;

		double incidentIntensity = ainstrument.getIntensityValue() * parameterValues[0];// * 1.0E8;

		double reflectivity;
//		double[] acr;
		double[] cPerm;
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
		double[][] Er = new double[layersNumber][2];
		double[][] Et = new double[layersNumber][2];
		double[][] Hr = new double[layersNumber][2];
		double[][] Ht = new double[layersNumber][2];

		double[][] eps = new double[layersNumber][2];

		RadiationType radType = ainstrument.getRadiationType();
		int rad_lines = radType.getLinesCount();
		for (int ej = 0; ej < rad_lines; ej++) {
			double lambda = radType.getRadiationWavelength(ej);
			double energy_intensity = radType.getRadiationWeigth(ej);
			double constant1 = 2.0 * Constants.PI / lambda * 1E8;
			double energy = Constants.ENERGY_LAMBDA / lambda;    // in eV
			double energyInKeV = energy * 0.001;

		for (int j = 0; j < layersNumber; j++) {
			Layer layer = asample.getlayer(j - 1);
			double[] complexPermittivity = new double[2];
			if (layer == null) {
				complexPermittivity[0] = 0;
				complexPermittivity[1] = 0;
			} else {
				cPerm = layer.getComplexPermittivityDiff(energyInKeV);
				complexPermittivity[0] = cPerm[0];
				complexPermittivity[1] = cPerm[1];
			}
			eps[j][0] = 1.0 - complexPermittivity[0];
			eps[j][1] = -complexPermittivity[1];
		}
		for (int k = adatafile.startingindex; k < adatafile.finalindex; k++) {
			double theta = adatafile.getXData(k) / 2;
			theta *= Constants.DEGTOPI;
			double cosPhi2 = Math.cos(theta);
			cosPhi2 *= cosPhi2;

			for (int j = 0; j < layersNumber; j++) {
				Layer layer = asample.getlayer(j - 1);
				double[] r1 = MoreMath.complexSqrt(eps[j][0] - cosPhi2, eps[j][1]);
				Nz[j][0] = r1[0];
				Nz[j][1] = r1[1];
				r1 = MoreMath.complexDivide(Nz[j], eps[j]);
				Nz_eps[j][0] = r1[0];
				Nz_eps[j][1] = r1[1];

				double thickness = 0;
				if (layer != null && j != layersNumber - 1)
					thickness = layer.getThicknessInCm();
				thickness *= constant1;
				r1 = MoreMath.complexExp(thickness * Nz[j][1], -thickness * Nz[j][0]);
				As[j][0] = r1[0];
				As[j][1] = r1[1];
				r1 = MoreMath.complexExp(thickness * Nz_eps[j][1], -thickness * Nz_eps[j][0]);
				Ap[j][0] = r1[0];
				Ap[j][1] = r1[1];
			}

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

			reflectivity = incidentIntensity * Eint * energy_intensity;
			adatafile.addtoPhasesFit(k, reflectivity);
		}
		}
		adatafile.computeReflectivityBroadening(asample);

	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
		JOptionsDialog adialog = new JParratOptionsD(parent, this);
		return adialog;
	}

	public class JParratOptionsD extends JOptionsDialog {

		JTextField scaleFactorTF;

		public JParratOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new BorderLayout(3, 3));

			JPanel jp1 = new JPanel(new GridLayout(0, 2));
			principalPanel.add(jp1, BorderLayout.NORTH);

			JPanel jp2 = new JPanel(new FlowLayout());
			jp2.add(new JLabel("Scale factor: "));
			scaleFactorTF = new JTextField(Constants.FLOAT_FIELD);
			scaleFactorTF.setToolTipText("Use the scale factor to balance with the diffraction intensity");
			jp2.add(scaleFactorTF);
			jp1.add(jp2);

			setTitle("Parrat reflectivity options");
			initParameters();
			pack();
		}

		public void initParameters() {
			scaleFactorTF.setText(parameterField[0].getValue());
			addComponenttolist(scaleFactorTF, parameterField[0]);
		}

		public void retrieveParameters() {
			parameterField[0].setValue(scaleFactorTF.getText());
		}

		public void dispose() {
			super.dispose();
		}

	}

}

