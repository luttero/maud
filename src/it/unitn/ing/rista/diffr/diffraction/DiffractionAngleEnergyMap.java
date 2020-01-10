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

	public void computeDiffraction(Sample asample, DataFileSet adataset) {

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
							DiffrDataFile datafile = theDataset.getActiveDataFile(j);
							computeDiffraction(theSample, datafile);
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
				computeDiffraction(theSample, datafile);
				computeasymmetry(theSample, datafile);
			}
	}

	public void computeDiffraction(Sample asample, DiffrDataFile datafile) {
		DataFileSet datafileset = datafile.getDataFileSet();
		if (getFilePar().isComputingDerivate()) {
//      System.out.println("refreshing derivative: " + this.toXRDcatString());
			for (int ij = 0; ij < getFilePar().getActiveSample().phasesNumber(); ij++) {
				double expfit[] = new double[datafile.getTotalNumberOfData()];
				int minmaxindex[] = computeReflectionIntensity(asample, datafileset.getPeakList(), true,
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
				int minmaxindex[] = computeReflectionIntensity(asample, datafileset.getPeakList(), true,
						expfit, Constants.ENTIRE_RANGE, Constants.COMPUTED,
						Constants.COMPUTED, Constants.COMPUTED, false,
						getFilePar().getActiveSample().getPhase(ij), datafile);
//        System.out.println("indices: " + minmaxindex[0] + " " + minmaxindex[1] + " " + expfit[1000]);
				for (int j = minmaxindex[0]; j < minmaxindex[1]; j++)
					datafile.addtoPhasesFit(j, expfit[j], ij);
			}
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
  
  public Peak createPeak(SizeStrainModel activeSizeStrain, double dspace, boolean dspacingbase, boolean energyDispersive,
                         double[] wavelength, double[] radweight, Reflection refl, int i) {
    return new PseudoVoigt2DPeak(dspace, dspacingbase, energyDispersive, wavelength, radweight, refl, i);
  }
  
	/*
	public void computeDiffraction(Sample asample, DataFileSet adataset) {

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

			double energy_coord = datafile.getXData(i);
			if (energy_coord >= energyInKeV[0] && energy_coord <= energyInKeV[rad_lines - 1]) {
				int ej = 1;
				while (energy_coord < energyInKeV[ej] && ej < rad_lines)
					ej++;

				double energyIntensity = energy_intensity[ej - 1] + (energy_intensity[ej] - energy_intensity[ej - 1]) *
						(energy_coord - energyInKeV[ej - 1]) / (energyInKeV[ej] - energyInKeV[ej - 1]);

//				if (getFilePar().isComputingDerivate()) {
//      System.out.println("refreshing derivative: " + this.toXRDcatString());
					for (int ij = 0; ij < getFilePar().getActiveSample().phasesNumber(); ij++) {
						for (int k = 0; k < datafilenumber; k++)
							fit[k] = 0;
						int minmaxindex[] = computeReflectionIntensity(x[DiffrDataFile.DATAFILE_THETA2], fit, asample, theDataset, true,
								Constants.ENTIRE_RANGE, Constants.COMPUTED, Constants.COMPUTED, Constants.COMPUTED, false,
								getFilePar().getActiveSample().getPhase(ij), energyIntensity, energy_coord, i);
						for (int k = minmaxindex[0]; k < minmaxindex[1]; k++)
							theDataset.getActiveDataFile(k).addtoPhasesFit(i, fit[k], ij);
					}
			}


		}
	}



	public int[] computeReflectionIntensity(double[] x, double[] expfit, Sample asample, DataFileSet datafileset, boolean computeBroadening,
	                                        double rangefactor, int computeTexture, int computeStrain, int computeFhkl, boolean leBailExtraction,
	                                        Phase aphase, double energyIntensity, double energy_coord, int datafileIndex) {

		Vector<Peak> peaklist = datafileset.getPeakList();
		Instrument ainstrument = datafileset.getInstrument();
		FilePar filepar = getFilePar();
		OutputStream out = null;
		boolean logOutput = false;
		PrintStream printStream = null;
		ByteArrayOutputStream baos = null;
//    Instrument ainstrument = getDataFileSet().getInstrument();
		double cutoff = datafileset.getPeakCutoffD() * rangefactor;
//		if (!datafile.increasingX()) {
//			cutoff = -cutoff;
//		}
		int[] tmpminmax = new int[2];
		int[] minmaxindex = new int[2];
		minmaxindex[0] = 0;
		minmaxindex[1] = x.length;
		arraycopy(minmaxindex, 0, tmpminmax, 0, 2);
		DiffrDataFile diffrDataFile = datafileset.getDataFile(datafileIndex);

		for (int i = 0; i < peaklist.size(); i++) {
			if (aphase == null || peaklist.elementAt(i).getPhase() == aphase) {

				double[] hwhm_i, eta, const1, const2, wave;
				double[][] thwhm, teta;
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

				int nrad = 1;  // only one energy
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

				int orderPosition = peaklist.elementAt(i).getOrderPosition();

				double[][][] positions = diffrDataFile.getPositions(aphase);
				double[][] absDetectorCorrection = new double[nrad][diffrDataFile.positionsPerPattern];
				int ipv = 0;
				double energy = energy_coord;
				for (int j = 0; j < diffrDataFile.positionsPerPattern; j++) {
					finalposition[ipv++] = positions[i][orderPosition][j];
					int pointIndex = diffrDataFile.getOldNearestPoint(positions[i][orderPosition][j]);
					absDetectorCorrection[i][j] = ainstrument.getDetector().getAbsorptionCorrection(diffrDataFile, pointIndex, energy);
				}

				double[][][][] hwhm_eta = diffrDataFile.getBroadFactors(aphase);
				double[][] deff = diffrDataFile.getCrystallitesMicrostrains(aphase)[0][orderPosition];

//	  for (int j = 0; j < diffrDataFile.positionsPerPattern; j++) {
				thwhm = hwhm_eta[0][orderPosition];
				teta = hwhm_eta[1][orderPosition];
//	  }
				double intensitySingle = ((basicPeak) peaklist.elementAt(i)).getScaleFactor();
				double[][] textureFactor;
				double Fhkl;

				switch (computeTexture) {
					case Constants.COMPUTED:
						textureFactor = diffrDataFile.getTextureFactors(aphase)[1][orderPosition];
						break;
					case Constants.EXPERIMENTAL:
						textureFactor = diffrDataFile.getTextureFactors(aphase)[0][orderPosition];
						break;
					case Constants.UNITARY:
					default:
						textureFactor = new double[diffrDataFile.positionsPerPattern][nrad];
						for (int i = 0; i < diffrDataFile.positionsPerPattern; i++)
							textureFactor[i][0] = 1.0;
				}
				for (int i = 0; i < diffrDataFile.positionsPerPattern; i++)
					if (Double.isNaN(textureFactor[i][0]))
						textureFactor[i][0] = 1.0;
//		if (getOrderPosition() == 2)
//	    System.out.println(diffrDataFile.getLabel() + ", texture factor: " + textureFactor[0]);
				switch (computeFhkl) {
					case Constants.COMPUTED:
						Fhkl = diffrDataFile.getDataFileSet().getStructureFactors(aphase)[1][orderPosition];
						break;
					case Constants.EXPERIMENTAL:
						Fhkl = diffrDataFile.getDataFileSet().getStructureFactors(aphase)[0][orderPosition];
						break;
					case Constants.UNITARY:
						Fhkl = 99.0;
						break;
					default:
						Fhkl = 88.0;
				}

				double[][] shapeAbs = diffrDataFile.getShapeAbsFactors(aphase, orderPosition);
				double asyConst1 = aphase.getActivePlanarDefects().getPlanarDefectAsymmetryConstant1(refl);
				double asyConst2 = aphase.getActivePlanarDefects().getPlanarDefectAsymmetryConstant2(refl);
				double planar_asymmetry = aphase.getActivePlanarDefects().getPlanarDefectAsymmetry(refl);

//	  System.out.println(" Total " + diffrDataFile.startingindex + " " + diffrDataFile.finalindex);
//	  System.out.println(" Range " + diffrDataFile.getXData(diffrDataFile.startingindex) + " " +
//			  diffrDataFile.getXData(diffrDataFile.finalindex));

//    Fhklist = new double[totalLines];
				double[] radiationWeight = new double[nrad];
				int principalRad = 0;
				radiationWeight[0] = energyIntensity;
				double weight = radiationWeight[0];
				double[][] lorentzPolarization = diffrDataFile.getLorentzPolarization(aphase, orderPosition);
				ipv = 0;
//				for (int i = 0; i < nrad; i++) {
					for (int j = 0; j < diffrDataFile.positionsPerPattern; j++) {
						if (radiationWeight[0] > 0.0) {
							double tmpIntensity = (intensitySingle * textureFactor[j][0] * shapeAbs[j][0] * Fhkl *
									radiationWeight[0] * aphase.getScaleFactor() * lorentzPolarization[j][0] * absDetectorCorrection[i][j]);
							if (const2[ipv] != 0.0) {
								for (int index = 0; index < 3; index++)
									intensity[index][ipv] = tmpIntensity * refl.pd_deltaIndex[index];
							} else {
								intensity[0][ipv] = tmpIntensity;
							}
							hwhm_i[ipv] = 1.0 / thwhm[j][0];
							eta[ipv] = teta[j][0];

							minindex[ipv] = diffrDataFile.getOldNearestPoint(finalposition[ipv] - thwhm[j][0] * cutoff);
							maxindex[ipv] = diffrDataFile.getOldNearestPoint(finalposition[ipv] + thwhm[j][0] * cutoff) + 1;

							if (!leBailExtraction || (leBailExtraction && i == principalRad)) {
								if (minmaxindex[0] > minindex[ipv])
									minmaxindex[0] = minindex[ipv];
								if (minmaxindex[1] < maxindex[ipv])
									minmaxindex[1] = maxindex[ipv];
							}

							const1[ipv] = asyConst1;
							const2[ipv] = asyConst2;
							wave[i] = energy ;

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
//				}

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

	}

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
	}*/

}
