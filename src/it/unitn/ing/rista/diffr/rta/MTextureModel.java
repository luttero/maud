/*
 * @(#)MTextureModel.java created 7/03/2019 White Rock, Los Alamos, NM
 *
 * Copyright (c) 2019 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.rta;

import com.jtex.arrays.Array1D;
import com.jtex.geom.*;
import com.jtex.plot.Plotter;
import com.jtex.qta.*;
import com.jtex.qta.kernel.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;
import static it.unitn.ing.rista.util.MaudPreferences.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Vector;
import javax.swing.*;

/**
 * The MTextureModel is a class to perform texture computation using the JTex java interface
 * by Florian Bachmann to the MTex texture package of Ralf Hielscher.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.37 $, $Date: 2006/12/04 14:30:05 $
 * @since JDK1.1
 */


public class MTextureModel extends DiscreteODFTexture {

	public static String[] diclistc = {"_rita_generate_symmetry", "_rita_wimv_odf_resolution", "_rita_wimv_refl_min_int",
			"_rita_wimv_refl_min_dspacing", "_rita_odf_refinable", "_rita_odf_sharpness", "_mtex_kernel_type"};
	public static String[] diclistcrm = {"_rita_generate_symmetry", "_rita_wimv_odf_resolution", "_rita_wimv_refl_min_int",
			"_rita_wimv_refl_min_dspacing", "_rita_odf_refinable", "_rita_odf_sharpness", "_mtex_kernel_type"};

	public static String[] classlistcs = {};
	public static String[] classlistc = {};

	double minDspacing = 0.0;

	double phiturn = 2.5;

	com.jtex.qta.ODF odf = null;
	com.jtex.qta.PoleFigure pf = null;

	public static final int KERNEL_VONMISES = 0, KERNEL_DELAVALLEEPOUSSIN = 1;
	public static final String[] kernel_type = {"VonMises", "DeLaValleePoussin"};
	public static final int kernelID = 6;
	int actualKernel = 0;

	boolean showPlotsAtEnd = false;

	public static String modelID = "MTex model";

	public MTextureModel(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initXRD();
		identifier = modelID;
		IDlabel = modelID;
		description = "select this to apply the Mtex model";
	}

	public MTextureModel(XRDcat aobj) {
		this(aobj, modelID);
	}

	public MTextureModel() {
		identifier = modelID;
		IDlabel = modelID;
		description = "select this to apply the MTex model";
	}

	public void initConstant() {
		Nstring = 7;
		Nstringloop = 0;
		Nparameter = 0;
		Nparameterloop = 0;
		Nsubordinate = 0;
		Nsubordinateloop = 0;
	}

	public void initDictionary() {
		System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
		System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
		System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
		System.arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
	}

	public void initParameters() {
		super.initParameters();

		setSampleSymmetry(NONE);
		setResolution(MaudPreferences.getPref("odf.defaultResolution", "7.5"));
		setMinimumIntensity(MaudPreferences.getPref("ewimv.minimumPFIntensity", "0.001"));
		setMinimumDspacing(MaudPreferences.getPref("ewimv.minimumDspacing", "0.0"));
		setODFrefinable(true);
		setKernelType(KERNEL_VONMISES);
		determineKernelType();
	}

	public void refreshForNotificationDown(XRDcat source, int reason) {
		if (!getFilePar().isComputingDerivate() || (source == this ||
				(reason == Constants.SAMPLE_ORIENTATION_CHANGED || (source == getParent() &&
						(reason == Constants.STRAIN_CHANGED || reason == Constants.CELL_CHANGED))))) {
			refreshComputation = true;
			//     System.out.println("Reason " + reason + " Source " + source.toXRDcatString());
		}
	}

	public void updateStringtoDoubleBuffering(boolean firstLoading) {
		super.updateStringtoDoubleBuffering(false);
		minDspacing = Double.parseDouble(getMinimumDspacing());
	}

	public void setResolution(String value) {
		double res = 5.0;
		try {
			res = Double.parseDouble(stringField[1]);
		} catch (Exception e) {
//			e.printStackTrace();
		}
		if (value != null && Double.parseDouble(value) != res) {
			stringField[1] = value;
//			resetODF();
		}
	}

	public String getResolution() {
		return stringField[1];
	}

	public double getResolutionD() {
//    System.out.println("res " + getResolution());
		return Double.parseDouble(getResolution());
	}

	public void setKernelType(int type) {
		setKernelType(kernel_type[type]);
	}

	public void setKernelType(String type) {
		stringField[kernelID] = type;
	}

	public String getKernelType() {
		return stringField[kernelID];
	}

	public int getKernelTypeAsInt() {
		determineKernelType();
		return actualKernel;
	}

	public void determineKernelType() {
		actualKernel = 0;
		for (int i = 0; i < kernel_type.length; i++)
			if (kernel_type[i].equalsIgnoreCase(getKernelType())) {
				actualKernel = i;
				break;
			}

	}

	public void setMinimumIntensity(String value) {
		stringField[2] = value;
	}

	public String getMinimumIntensity() {
		return stringField[2];
	}

	public double getMinimumIntensityD() {
		return Double.parseDouble(getMinimumIntensity());
	}

	public void setMinimumDspacing(String value) {
		stringField[3] = value;
	}

	public String getMinimumDspacing() {
		return stringField[3];
	}

	public double getMinimumDspacingD() {
		return minDspacing;
	}

	public String getSampleSymmetry() {
		return stringField[0];
	}

	public void setSampleSymmetry(int i) {
		stringField[0] = symmetrychoice[i];
	}

	public void setSampleSymmetry(String value) {
		stringField[0] = value;
	}

	public int getSampleSymmetryMultiplicity() {
		switch (getSampleSymmetryValue()) {
			case 0:
			case 1:
			case 2:
			case 3:
				return getSampleSymmetryValue() + 1;
			case 4:
				return 6;
			case 5:
				return 2;
			case 6:
				return 4;
			case 7:
				return (int) (360f / phiturn + 0.001f);
			default: {
			}
		}
		return 1;
	}

//	NONE, TWO_FOLD, THREE_FOLD, FOUR_FOLD,
//	SIX_FOLD, MIRROR, ORTHOROMBIC, FIBER

	public Symmetry getMTexSampleSymmetry() {
		Symmetry ss;
		switch (getSampleSymmetryValue()) {
			case 0:
				ss = new Symmetry(PointGroup.C1.getSchoenflies());
				break;
			case 1:
				ss = new Symmetry(PointGroup.C2.getSchoenflies());
				break;
			case 2:
				ss = new Symmetry(PointGroup.C3.getSchoenflies());
				break;
			case 3:
				ss = new Symmetry(PointGroup.O.getSchoenflies());
				break;
			case 4:
				ss = new Symmetry(PointGroup.C6h.getSchoenflies());
				break;
			case 5:
				ss = new Symmetry(PointGroup.D2.getSchoenflies());
				break;
			case 6:
				ss = new Symmetry(PointGroup.D4.getSchoenflies());
				break;
			case 7:
				ss = new Symmetry(PointGroup.Fib.getSchoenflies());
				break;
			default: {
				ss = new Symmetry(PointGroup.C1.getSchoenflies());
				break;
			}
		}
		return ss;
	}

	public boolean ODFisRefinable() {
		return stringField[4].equalsIgnoreCase("true");
	}

	public void setODFrefinable(boolean status) {
		if (status)
			stringField[4] = "true";
		else
			stringField[4] = "false";
	}

	public void setODFrefinable(String value) {
		stringField[4] = value;
	}

	public void setSharpness(String value) {
		setString(5, value);
	}

	public String getSharpness() {
		return getString(5);
	}

	public int prepareiteration(double[][] experimentalPF) {

		textureInitialization();

		Phase aphase = getPhase();
		cdsc = aphase.lattice();
		numberPoleFigures = experimentalPF.length;
//    numberOfPFPoint = new int[getPoleFigureNumber()];

/*
		poleFactor = new double[getPoleFigureNumber()];


		int index = 0;
		for (int i = 0; i < getPoleFigureNumber(); i++) {
			poleFactor[i] = 1.0f;
			int numberDta = getPointNumber(i);
			for (int j = 0; j < numberDta; j++) {
				dta_fix[index] = experimentalPF[i][j];
				wgt[index] = 1.0f;
				poleindex[index] = i;
				pointindex[index++] = j;
			}
		}

		for (int i = 0; i < totalWeight.length; i++) {
			totalWeight[i] = 0.0;
			totalHits[i] = 0;
		}*/
		return 0;
	}

	public void prepareiteration(Sample asample) {

		textureInitialization();

		Phase aphase = getPhase();

		int hkln = aphase.gethklNumber();
		int numberPoleFigures = 0;
		for (int j = 0; j < hkln; j++) {
			Reflection refl = aphase.getReflectionVector().elementAt(j);

			if (refl.isGoodforTexture())
				numberPoleFigures++;
		}

		int LGIndex = SpaceGroups.getLGNumber(getPhase().getPointGroup());

		String symmetry = it.unitn.ing.rista.util.SpaceGroups.laueGroupOnly[LGIndex];
		Symmetry cs = new Symmetry(symmetry, aphase.getFullCellValue(0), aphase.getFullCellValue(1), aphase.getFullCellValue(2),
				aphase.getFullCellValue(3), aphase.getFullCellValue(4), aphase.getFullCellValue(5));

//		System.out.println(cs.euler("ZXZ").toDegrees());

		pf = new com.jtex.qta.PoleFigure();
		pf.setCS(cs);
		pf.setSS(getMTexSampleSymmetry());

/* test
		double[] th = new double[5];
		double[] ph = new double[5];
		double[] dd = new double[5];
		for (int i = 0; i < 5; i++) {
			th[i] = 10.0 * i * Constants.DEGTOPI;
			ph[i] = 10.0 * i * Constants.DEGTOPI;
			dd[i] = 0.5 + 0.25 * i;
		}

		PoleFigure ps1 = new PoleFigure(new Miller(1, 1, 1, cs, ""),
				new Vec3(th, ph), new Array1D(dd));
//				ps.setCS(cs);
		pf.add(ps1);

 */
		int pf_number = 0;
		for (int j = 0; j < hkln; j++) {
			Reflection refl = aphase.getReflectionVector().elementAt(j);

			if (refl.isGoodforTexture()) {
				Vector<double[]> pf_data = new Vector<>(100, 100);
				int numberDatasets = asample.activeDatasetsNumber();
				int numberDataPoints = 0;
				for (int i = 0; i < numberDatasets; i++) {
					DataFileSet dataset = asample.getActiveDataSet(i);
					int radCount = dataset.getInstrument().getRadiationType().getLinesCount();
					for (int k = 0; k < dataset.activedatafilesnumber(); k++) {
						DiffrDataFile datafile = dataset.getActiveDataFile(k);
							for (int l = 0; l < radCount; l++) {
								double[] pfd = new double[3];
								pfd[2] = datafile.getExperimentalTextureFactor(aphase, j, l);
								double position = datafile.getPosition(aphase, j, l);
								if (!Double.isNaN(pfd[2]) && datafile.isInsideRange(position)) {
									numberDataPoints++;
									double[] angles = datafile.getTextureAngles(position);
									pfd[0] = angles[0];
									pfd[1] = angles[1];
//									System.out.println(pfd[0] + " " + pfd[1] + " " + (pfd[2] * 100));
									pf_data.add(pfd);
								}
							}
					}
				}

				int symNumberDataPoints = numberDataPoints * getSampleSymmetryMultiplicity();
				double[] theta = new double[symNumberDataPoints],
						   rho = new double[symNumberDataPoints],
						   data = new double[symNumberDataPoints];
				int index = 0;
				for (int k = 0; k < getSampleSymmetryMultiplicity(); k++) {
				for (int i = 0; i < pf_data.size(); i++) {
					double[] pfd = pf_data.elementAt(i);
						double angles[] = new double[2];
						angles[0] = pfd[0];
						angles[1] = pfd[1];
						double[] newangles = applySampleSymmetry(angles, index, numberDataPoints);
						theta[index] = newangles[0] * Constants.DEGTOPI;
						rho[index] = newangles[1] * Constants.DEGTOPI;
						data[index] = pfd[2];
						index++;
					}
				}
/*
				double[][] expTFAndAngles = refl.getExpPoleFigureGrid();
				int numberDataPoints = expTFAndAngles[0].length;
				int symNumberDataPoints = numberDataPoints * getSampleSymmetryMultiplicity();
				double[] theta = new double[symNumberDataPoints],
						rho = new double[symNumberDataPoints],
						data = new double[symNumberDataPoints];
				int index = 0;
				for (int k = 0; k < getSampleSymmetryMultiplicity(); k++) {
					for (int i = 0; i < numberDataPoints; i++) {
						double angles[] = new double[2];
						angles[0] = expTFAndAngles[0][i];
						angles[1] = expTFAndAngles[1][i];
						double[] newangles = applySampleSymmetry(angles, index, numberDataPoints);
						theta[index] = newangles[0] * Constants.DEGTOPI;
						rho[index] = newangles[1] * Constants.DEGTOPI;
						data[index] = expTFAndAngles[2][i];
						index++;
					}
				}*/

//            System.out.println("PF number & size: " + pf_number + " " + data.length);
				com.jtex.qta.PoleFigure ps = new com.jtex.qta.PoleFigure(new Miller(refl.getH(), refl.getK(), refl.getL(), cs, ""),
						new Vec3(theta, rho), new Array1D(data));
//				ps.setCS(cs);
				pf.add(ps);
				pf_number++;
			}
		}
		if (showPlotsAtEnd)
			Plotter.show(Plotter.plot(pf));
//		pf.setC(2, new Array1D(0.52, 1.23));
	}

	public int textureInitialization() {
		sampleSymmetryValue = computeSampleSymmetryValue();

		phiturn = getDouble(prefs[2], Double.parseDouble(Texture.prefVal[2]));
		if (phiturn <= 0.0)
			phiturn = resolution / 2.0;

		return 0;
	}

	public Reflection getReflection(int pole, int partial) {
		return getPhase().getReflectionVector().elementAt(poleFigureIndex[pole] + partial);
	}

	public double[] getTextureAngles(int pole, int point) {
		if (fromPF) {
			double[] angles = new double[2];
			angles[0] = textureAngles[0][pole][point];
			angles[1] = textureAngles[1][pole][point];
			return angles;
		}

		Reflection reflex = getReflection(pole, 0);
		int np = getFilePar().getActiveSample().getNumberActiveDatafiles();
//		System.out.println(np);
		return applySampleSymmetry(reflex, point, np);
	}

	public int getPoint(Reflection reflex, int point) {
		int truepointmax = getFilePar().getActiveSample().getNumberActiveDatafiles();
		int sector = point / truepointmax;
		int residual = point - sector * truepointmax;
		return residual;
	}

	public DiffrDataFile getPointFromAll(int point) {
		Sample asample = getFilePar().getActiveSample();
		int truepointmax = asample.getNumberActiveDatafiles();
		int sector = point / truepointmax;
		int residual = point - sector * truepointmax;
		return asample.getActiveDiffrDataFile(residual);
	}

	public int getPoint(int reflexindex, int point) {
		Reflection reflex = getReflection(reflexindex, 0);
		return getPoint(reflex, point);
	}

	public double[] applySampleSymmetry(double[] angles, int point, int truepointmax) {
		int sector = point / truepointmax;
//		int residual = point - sector * truepointmax;

		if (angles[0] < 0) {
			angles[0] = -angles[0];
			angles[1] += 180.0f;
		}
		while (angles[0] >= 360.0)
			angles[0] -= 360.0f;
		if (angles[0] >= 180.0) {
			angles[0] -= 180.0f;
			angles[1] += 180.0f;       // to test
		}
		if (angles[0] >= 90.0)
			angles[0] = 180.0f - angles[0];
		while (angles[1] < 0.0)
			angles[1] += 360.0f;
		while (angles[1] >= 360.0)
			angles[1] -= 360.0f;

		if (truepointmax > point)
			return angles;

		int fold;
		double lphiturn;

		switch (getSampleSymmetryValue()) {
			case 1:
			case 2:
			case 3:
				fold = getSampleSymmetryValue() + 1;
				lphiturn = 360f / fold;
				angles[1] += sector * lphiturn;
				break;
			case 4:
				fold = 6;
				lphiturn = 360f / fold;
				angles[1] += sector * lphiturn;
				break;
			case 5:
				angles[1] = 360.f - angles[1];
				break;
			case 6: // orthorhombic
				switch (sector) {
					case 0:
						break;
					case 1:
						angles[1] = 180.f - angles[1];
						break;
					case 2:
						angles[1] = 180.f + angles[1];
						break;
					case 3:
						angles[1] = 360.f - angles[1];
						break;
					default: {
					}
				}
				break;
			case 7:
//				fold = 72;
//				phiturn = 360f / fold;
				angles[1] += sector * phiturn;
				break;
			default: {
			}
		}
		while (angles[1] < 0)
			angles[1] += 360.0f;
		while (angles[1] >= 360.0)
			angles[1] -= 360.0f;

		return angles;
	}

	public double getPoleIntensity(int pole, int point) {
		int izoveri = getIzoveri(pole);
		double texturefactor = 0.0;
		Phase phase = getPhase();
		for (int i = 0; i < izoveri; i++) {
			int reflexIndex = poleFigureIndex[pole] + i;
//		 	int mult = reflex.multiplicity;
			texturefactor += getPointFromAll(point).getExperimentalTextureFactor(phase, reflexIndex,0) *            // todo: v3.0 for all radiations?
					phase.getReflex(reflexIndex).getOverlappedWeight(); // * mult;
		}
		return texturefactor;
	}

/*	public double getWeight(int pole, int point) {
//		if (!useIntensityWeigth())
//			return 1.0;
		int izoveri = getIzoveri(pole);
		double wgt = 0.0;
		for (int i = 0; i < izoveri; i++) {
			Reflection reflex = getReflection(pole, i);
//      int mult = reflex.multiplicity;
//			System.out.println(reflex.getH() + " " + reflex.getK() + " " + reflex.getL() + ", weight(" + i + "): " + reflex.getWeight());
			wgt += reflex.getWeight();// * mult;
		}
		return Math.pow(wgt, getWeightsExponent()); // (wgt / izoveri);
		// // we decrease the weight of the superposed reflections by an additional factor
//		return 1.0f;
	}*/

	public int getH(int pole) {
		return getReflection(pole, 0).getH();
	}

	public int getK(int pole) {
		return getReflection(pole, 0).getK();
	}

	public int getL(int pole) {
		return getReflection(pole, 0).getL();
	}

	public int getIzoveri(int pole) {
		if (fromPF)
			return izoveriPF[pole];
		return getReflection(pole, 0).izoveri;
	}

	public int getH(int pole, int partial) {
		return getReflection(pole, partial).getH();
	}

	public int getK(int pole, int partial) {
		return getReflection(pole, partial).getK();
	}

	public int getL(int pole, int partial) {
		return getReflection(pole, partial).getL();
	}

	public double getOverlappedWeight(int pole, int partial) {
		if (fromPF)
			return weightSingle[pole][partial];

		Reflection reflex = getReflection(pole, partial);
		return reflex.getOverlappedWeight();
	}

	public int[][] gethklList(int pole, int i) {

		Reflection reflex = null;
		if (!fromPF)
			reflex = getReflection(pole, i);
		int mult;
/*    if (useAllhklForCubic && !fromPF)
      mult = reflex.multiplicity;
    else if (useAllhklForCubic)
      mult = allhklmult[pole][i];
    else  */
		mult = 1;
		int mult2 = mult / 2;

		int[][] hkllist = new int[3][mult];

		for (int imult = 0; imult < mult; imult++) {
			if (imult < mult2 || mult == 1) {
				if (!fromPF) {
					hkllist[0][imult] = reflex.hlist[imult];
					hkllist[1][imult] = reflex.klist[imult];
					hkllist[2][imult] = reflex.llist[imult];
				} else {
					hkllist[0][imult] = hklPF[0][imult][pole][i];
					hkllist[1][imult] = hklPF[1][imult][pole][i];
					hkllist[2][imult] = hklPF[2][imult][pole][i];
				}
			} else {
				if (!fromPF) {
					hkllist[0][imult] = -reflex.hlist[imult - mult2];
					hkllist[1][imult] = -reflex.klist[imult - mult2];
					hkllist[2][imult] = -reflex.llist[imult - mult2];
				} else {
					hkllist[0][imult] = -hklPF[0][imult - mult2][pole][i];
					hkllist[1][imult] = -hklPF[1][imult - mult2][pole][i];
					hkllist[2][imult] = -hklPF[2][imult - mult2][pole][i];
				}
			}
		}
		return hkllist;
	}

	public void computeTextureFactor(Phase aphase, Sample asample) {

//		textureInitialization();
		if (!refreshComputation)
			return;

		//  System.out.println("Computing texture factors");

		FilePar aparFile = asample.getFilePar();

		if (aparFile.isTextureComputationPermitted() && ODFisRefinable()) {

//      actualsample = asample;

			prepareiteration(asample);

			System.out.println("Computing ODF using MTex for phase: " + getPhase().toXRDcatString());


//			Plotter.show(Plotter.plot(pf));
//			ODF rec = ODF.estimate(pf, 5);

			Kernel kernel;
			switch (getKernelTypeAsInt()) {
				case KERNEL_DELAVALLEEPOUSSIN:
					kernel =  new DeLaValleePoussin(Math.toRadians(getResolutionD()));
					break;
				default: {
					kernel =  new VonMisesFisher(Math.toRadians(getResolutionD()));
				}
			}
			if (odf == null)
				odf = new ODF();
			com.jtex.qta.ODFOptions odfOptions = new com.jtex.qta.ODFOptions(pf, Math.toRadians(getResolutionD()), kernel);
			odfOptions.setGhostCorrection(false);

			odf = odf.estimate(pf, odfOptions);

//			System.out.println("ODF components number: " + odf.componentsNumber());

//			Plotter.show(Plotter.plotsigma(odf));
//			Plotter.show(Plotter.plot(pf));
			if (showPlotsAtEnd)
				Plotter.show(Plotter.plotpdf(odf, pf.getH()));

		}

		refreshComputation = false;

		recomputedTextureFactor(aphase, asample, true);
	}

	public ArrayList<double[]> recomputedTextureFactor(Phase aphase, Sample asample, boolean setValues) {

		if (odf == null)
			return null;

		int LGIndex = SpaceGroups.getLGNumber(getPhase().getPointGroup());

		String symmetry = it.unitn.ing.rista.util.SpaceGroups.laueGroupOnly[LGIndex];
		Symmetry cs = new Symmetry(symmetry, aphase.getFullCellValue(0), aphase.getFullCellValue(1), aphase.getFullCellValue(2),
				aphase.getFullCellValue(3), aphase.getFullCellValue(4), aphase.getFullCellValue(5));
		Symmetry ss = getMTexSampleSymmetry();
		int hkln = aphase.gethklNumber();
		Vector<Array1D> allData = new Vector<>(hkln, 1);
		for (int j = 0; j < hkln; j++) {
			Reflection refl = aphase.getReflectionVector().elementAt(j);

			Vector<double[]> pf_data = new Vector<>(100, 100);
			int numberDatasets = asample.activeDatasetsNumber();
			int numberDataPoints = 0;
			for (int i = 0; i < numberDatasets; i++) {
				DataFileSet dataset = asample.getActiveDataSet(i);
				int radCount = dataset.getInstrument().getRadiationType().getLinesCount();
				for (int k = 0; k < dataset.activedatafilesnumber(); k++) {
					DiffrDataFile datafile = dataset.getActiveDataFile(k);
						for (int l = 0; l < radCount; l++) {
							double[] pfd = new double[2];
//						pfd[2] = datafile.getExperimentalTextureFactors(aphase, j)[ppp][l];
							double position = datafile.getPosition(aphase, j, l);
//							if (!Double.isNaN(pfd[2])) {
							numberDataPoints++;
							double[] angles = datafile.getTextureAngles(position);
							pfd[0] = angles[0] * Constants.DEGTOPI;
							pfd[1] = angles[1] * Constants.DEGTOPI;
							pf_data.add(pfd);
//							}
						}
				}
			}
			double[] theta = new double[numberDataPoints], rho = new double[numberDataPoints], data = new double[numberDataPoints];
			for (int i = 0; i < numberDataPoints; i++) {
				double[] pfd = pf_data.elementAt(i);
				theta[i] = pfd[0];
				rho[i] = pfd[1];
			}
			com.jtex.qta.PoleFigure pf = odf.calcPoleFigure(new Miller(refl.getH(), refl.getK(), refl.getL(), cs, ""), new Vec3(theta, rho));
			allData.add(pf.getData());
		}

		for (int j = 0; j < hkln; j++) {
			Array1D reflData = allData.elementAt(j);
			int index = 0;
			for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
				DataFileSet adataset = asample.getActiveDataSet(i);
				int datafilenumber = adataset.activedatafilesnumber();
				int radCount = adataset.getInstrument().getRadiationType().getLinesCount();
				for (int i1 = 0; i1 < datafilenumber; i1++) {
					DiffrDataFile adatafile = adataset.getActiveDataFile(i1);
					int totalNumber = radCount;
					double[] textF = new double[totalNumber];
					for (int k = 0; k < totalNumber; k++)
						textF[k] = reflData.get(index + k);
					adatafile.setTextureFactors(aphase, j, textF);
					index += totalNumber;
				}
			}
		}
		return null;
	}

	public double[] computeTextureFactor(Phase aphase, double[][] alphabeta, Reflection reflex) {

//    int numberOfPoints = alphabeta.length/2;

		initializeAll();

		if (odf != null)
			return super.computeTextureFactor(aphase, alphabeta, reflex);

		return null;
	}
	public JOptionsDialog getOptionsDialog(Frame parent) {
		return new JMTextureOptionsD(parent, this);
	}

	public class JMTextureOptionsD extends JOptionsDialog {

		JComboBox symmetryCB;
		JTextField resolutionTF;
		JComboBox kernelTypeCB;
		JCheckBox refinableCB;
		JCheckBox showPlotCB;
		JTextField minIntTF;
		JTextField thresholdTF;
		JButton jb;

		public JMTextureOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new BorderLayout(3, 3));

			JPanel lowerPanel = new JPanel();
			lowerPanel.setLayout(new GridLayout(0, 2, 3, 3));
			principalPanel.add(BorderLayout.CENTER, lowerPanel);
			JPanel jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			jPanel8.add(new JLabel("ODF resolution in degrees: "));
			resolutionTF = new JTextField(Constants.FLOAT_FIELD);
			resolutionTF.setToolTipText("Choose the ODF cells resolution");
			jPanel8.add(resolutionTF);
			lowerPanel.add(jPanel8);

			jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel8);
			jPanel8.add(new JLabel("Kernel type: "));
			kernelTypeCB = new JComboBox();
			for (String type : kernel_type) kernelTypeCB.addItem(type);
			kernelTypeCB.setToolTipText("Choose the kernel type");
			jPanel8.add(kernelTypeCB);

			jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel8);
			refinableCB = new JCheckBox("ODF refinable");
			refinableCB.setToolTipText("Uncheck this box if the ODF should not be modify");
			jPanel8.add(refinableCB);

			jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel8);
			showPlotCB = new JCheckBox("Show plots");
			showPlotCB.setToolTipText("Chck this box to get an automatic PFs plot at the end of each iteration");
			jPanel8.add(showPlotCB);

			jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel8);
			jPanel8.add(new JLabel("Min reflex intensity: "));
			minIntTF = new JTextField(Constants.FLOAT_FIELD);
			minIntTF.setToolTipText("Minimum value of intensity for a reflection to be included (respect to max)");
			jPanel8.add(minIntTF);

			jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel8);
			jPanel8.add(new JLabel("Minimun reflection d-spacing: "));
			thresholdTF = new JTextField(Constants.FLOAT_FIELD);
			thresholdTF.setToolTipText("Use only reflections with d-space bigger than this value");
			jPanel8.add(thresholdTF);

			jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			jPanel8.add(jb = new JButton("Plot PFs"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					(new PersistentThread() {
						public void executeJob() {
							Plotter.show(Plotter.plotpdf(odf, pf.getH()));
						}
					}).start();
				}
			});
			jb.setToolTipText("Press this to display a plot of the reconstructed PFs");

			jPanel8.add(jb = new JButton("Plot exp PFs"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					(new PersistentThread() {
						public void executeJob() {
							Plotter.show(Plotter.plot(pf));
						}
					}).start();
				}
			});
			jb.setToolTipText("Press this to display a plot of the extracted pole figures");

			jPanel8.add(jb = new JButton("Plot PFs diff"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					(new PersistentThread() {
						public void executeJob() {
							Plotter.show(Plotter.plotDiff(odf, pf));
						}
					}).start();
				}
			});
			jb.setToolTipText("Press this to display a plot of the difference between experimental and recalculated PFs");

			lowerPanel.add(jPanel8);

			jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			jPanel8.add(jb = new JButton("Plot ODF (sigma)"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					(new PersistentThread() {
						public void executeJob() {
							Plotter.show(Plotter.plotsigma(odf));
						}
					}).start();
				}
			});
			jb.setToolTipText("Press this to display a plot of the ODF in sigma sections");

			jPanel8.add(jb = new JButton("Plot ODF (phi2full)"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					(new PersistentThread() {
						public void executeJob() {
							Plotter.show(Plotter.plotphi2full(odf));
						}
					}).start();
				}
			});
			jb.setToolTipText("Press this to display a plot of the ODF in phi2 sections");
			lowerPanel.add(jPanel8);

			jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			jPanel8.add(jb = new JButton("Plot ODF (phi2)"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					(new PersistentThread() {
						public void executeJob() {
							Plotter.show(Plotter.plotphi2(odf));
						}
					}).start();
				}
			});
			jb.setToolTipText("Press this to display a plot of the ODF in phi2 sections");

			jPanel8.add(jb = new JButton("Plot ODF (phi1)"));
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					retrieveParameters();
					(new PersistentThread() {
						public void executeJob() {
							Plotter.show(Plotter.plotphi1(odf));
						}
					}).start();
				}
			});
			jb.setToolTipText("Press this to display a plot of the ODF in phi1 sections");
			lowerPanel.add(jPanel8);

         jPanel8 = new JPanel();
			jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
			lowerPanel.add(jPanel8);
			jPanel8.add(new JLabel("Generate symmetry: "));
			symmetryCB = new JComboBox();
			for (int i = 0; i < symmetrychoicenumber; i++)
				symmetryCB.addItem(symmetrychoice[i]);
			symmetryCB.setToolTipText("Set up unmeasured sample symmetries");
			jPanel8.add(symmetryCB);
			lowerPanel.add(jPanel8);

			setTitle("MTex options panel");
			initParameters();
			pack();
		}

		public void initParameters() {
			symmetryCB.setSelectedItem(getSampleSymmetry());
			refinableCB.setSelected(ODFisRefinable());
			minIntTF.setText(getMinimumIntensity());
			thresholdTF.setText(getMinimumDspacing());
			resolutionTF.setText(getResolution());
			kernelTypeCB.setSelectedIndex(getKernelTypeAsInt());
			showPlotCB.setSelected(showPlotsAtEnd);
		}

		public void retrieveParameters() {
			setSampleSymmetry(symmetryCB.getSelectedItem().toString());
			setResolution(resolutionTF.getText());
			setKernelType(kernelTypeCB.getSelectedIndex());
			setODFrefinable(refinableCB.isSelected());
			showPlotsAtEnd = showPlotCB.isSelected();
			setMinimumIntensity(minIntTF.getText());
			setMinimumDspacing(thresholdTF.getText());
		}

	}

}
