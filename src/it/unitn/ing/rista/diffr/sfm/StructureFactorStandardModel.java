/*
 * @(#)StructureFactorStandardModel.java created 28/06/2001 Casalino
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.sfm;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import java.util.Vector;

/**
 *  The StructureFactorStandardModel is a class that use the atomic standard model.
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:08 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class StructureFactorStandardModel extends StructureFactorModel {
  //insert class definition here
	public StructureFactorStandardModel(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "atomic standard model";
    IDlabel = "atomic standard model";
    description = "select this for standard atomic structure factors";
  }

  public StructureFactorStandardModel(XRDcat aobj) {
    this(aobj, "atomic standard model");
  }

  public StructureFactorStandardModel() {
    identifier = "atomic standard model";
    IDlabel = "atomic standard model";
    description = "select this for standard atomic structure factors";
  }

	public void computeStructureFactors(Sample asample, DataFileSet adataset) {
		final Phase phase = (Phase) getParent();
		if (!phase.refreshFhklcomp)
			return;
		computeStructureFactors(phase, asample, adataset);
	}

  public static void computeStructureFactors(final Phase phase, Sample asample, DataFileSet adataset) {
	  double structureFactor, intA = 1;
	  final double cuttingThreshold = 0.0001;
	  final double thermalS = phase.getThermalStrainCached();
	  phase.refreshFhklcompv();
	  int hkln = phase.gethklNumber();
	  final double[] fhkl = new double[hkln];
	  final boolean useAnisotropicCrystallites =
			  adataset.getInstrument().getRadiationType().useCrystallitesForDynamicalCorrection();
	  if (phase.getNumberOfCustomPeaks() > 0) {
		  for (int i = 0; i < hkln; i++)
		    fhkl[i] = phase.getReflex(i).structureFactor;
	  } else {
	  Instrument ainstrument = adataset.getInstrument();
	  final Radiation rad1 = ainstrument.getRadiationType().getRadiation(0);
//	  final int radType = rad1.getRadiationIDNumber();
	  Vector<AtomSite> atomList = phase.getFullAtomList();
		double volume = phase.getCellVolume();
	  double t = 1, t_corr = 1, constV = 1;
	  if (rad1.isDynamical()) { // dynamical scattering
			t = phase.getCrystalThickness();
		  if (t <= 0)
			  t = 1;
		  else
			  t_corr = t;
			double E = adataset.getInstrument().getRadiationType().getRadiationEnergy();
//			double m_me = 1.0 + Constants.ENERGY_CONSTANT * E;
			double K02 = Constants.E_SCAT_FACTOR_PI * E;
			int atomNumber = atomList.size();
			double[][] scatteringFactors = new double[atomNumber][2];
			for (int j = 0; j < atomNumber; j++) {
				double[] scat = atomList.get(j).scatfactor(0.0, rad1);
				scatteringFactors[j][0] = scat[0];
				scatteringFactors[j][1] = scat[1];
//				System.out.println(atomList.get(j).getLabel() + " " + scat[0] + " " + scat[1]);
			}
		  double U0corr = Constants.E_SCAT_FACTOR_PI / volume;
			double U0 = Math.sqrt(Fhklcomp0(phase, scatteringFactors)) * U0corr;
		  double k = Math.sqrt(K02 + U0);
			constV = Constants.E_SCAT_FACTOR_PI * t_corr / k * Math.PI;
/*		  if (Constants.testing && !phase.getFilePar().isOptimizing())
		    System.out.println("U0: " + constV + " " + k + " " + K02 + " " + U0 + " " + t + " " + t_corr);*/
		}
		final double meanCrystallite_corr = phase.getMeanCrystallite() * thermalS + t;

	  final int maxThreads = Math.min(Constants.maxNumberOfThreads, hkln / 10);
	  if ((hkln * atomList.size()) > 1000 && maxThreads > 1 && Constants.threadingGranularity >= Constants.MEDIUM_GRANULARITY) {
		  if (Constants.debugThreads)
			  System.out.println("Thread structure factors " + phase.getLabel());
		  int i;
		  PersistentThread[] threads = new PersistentThread[maxThreads];
		  for (i = 0; i < maxThreads; i++) {
			  final DataFileSet dataset = adataset;
			  final double thickness = t;
			  final double thickness_corr = t_corr;
			  final double volumeCorrection = constV;
			  final double volumeCell = volume;
			  threads[i] = new PersistentThread(i) {
				  @Override
				  public void executeJob() {
					  int i1 = this.getJobNumberStart();
					  int i2 = this.getJobNumberEnd();

					  double[] fhkl_t = new double[i2 - i1];

					  double[][][][] scatFactors = dataset.getScatteringFactor(phase);
					  for (int j = i1; j < i2; j++) {
						  Reflection refl = phase.getReflex(j);
						  // todo, should be modified to compute scatfactors for each rad line
						  double structureFactor = Fhklcomp(phase, refl, scatFactors[j + 1][0]);
						  if (rad1.isDynamical()) {
							  double crystCorr = 1.0;
							  if (useAnisotropicCrystallites)
								  crystCorr = (phase.getCrystallite(j) * thermalS + thickness) / thickness_corr;
							  double Fhkl = Math.sqrt(structureFactor) / volumeCell;
							  double A = Fhkl * volumeCorrection * crystCorr;
//							  if (A > cuttingThreshold)
							  double _intA = IntegratedBesselJ0.averageIntegralBesselUpTo(A);
								structureFactor *= _intA / meanCrystallite_corr;
//							  else
//								  structureFactor = 1.207107;
//								structureFactor *= volumeCorrection / thickness_corr; // we eliminate the thickness
						  }
						  fhkl_t[j - i1] = structureFactor;
					  }

					  synchronized(phase) {
						  for (int j = i1; j < i2; j++)
							  fhkl[j] = fhkl_t[j - i1];
					  }
				  }
			  };
		  }
		  i = 0;
		  int istep = (int) (0.9999 + hkln / maxThreads);
		  for (int j = 0; j < maxThreads; j++) {
			  int is = i;
			  if (j < maxThreads - 1)
				  i = Math.min(i + istep, hkln);
			  else
				  i = hkln;
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
//		  boolean testingBfactors = false;
//		  if (Constants.testing)
//			  testingBfactors = MaudPreferences.getBoolean("testing.anisotropicBfactors", false);
		  double[][][][] scatFactors = adataset.getScatteringFactor(phase);
		  for (int i = 0; i < hkln; i++) {
			  Reflection refl = phase.getReflex(i);
			  // todo, should be modified to compute scatfactors for each rad line
//			  if (!testingBfactors)
			    structureFactor = Fhklcomp(phase, refl, scatFactors[i + 1][0]);
//			  else
//			    structureFactor = Fhklcomp(phase, refl.hlist, refl.klist, refl.llist, multiplicity, dspacing,
//					    scatFactors[i + 1][0]);
			  if (rad1.isDynamical()) {
				  double crystCorrection = 1.0;
				  if (useAnisotropicCrystallites)
					  crystCorrection = (phase.getCrystallite(i) * thermalS + t) / t_corr;
				  double Fhkl = Math.sqrt(structureFactor) / volume;
				  double A = Fhkl * constV * crystCorrection;
//				  if (A > cuttingThreshold)
				  intA = IntegratedBesselJ0.averageIntegralBesselUpTo(A);
					structureFactor *= intA / meanCrystallite_corr;
//				  else
//				    structureFactor *= 1.207107;
/*				  if (useAnisotropicCrystallites)
				    structureFactor *= constV / t_corr; // we eliminate the thickness;
				  else
					  structureFactor *= constV / t_corr;*/
/*				  if (Constants.testing && !phase.getFilePar().isOptimizing())
				    System.out.println(refl.getH() + " " + refl.getK() + " " + refl.getL() + " " + Fhkl + " " + A + ", IntA/A " + intA +
				        " " + newCorrection + " " + structureFactor + " " + constV + " " + meanCrystallite + " " + t);*/
			  }
			  fhkl[i] = structureFactor;
		  }
	  }
  }
		adataset.storeComputedStructureFactors(phase, fhkl);
	}

	public static double Fhklcomp0(Phase phase, double[][] scatf) {
		double scatf1, scatf2;
		double a1 = 0.0;
		double a2 = 0.0;
		Vector<AtomSite> atomList = phase.getFullAtomList();
		int atomNumber = atomList.size();
		for (int j = 0; j < atomNumber; j++) {
			AtomSite ato = atomList.get(j);
			ato.trowException = true;
			if (ato.useThisAtom) {
				double scatFactor = ato.getOccupancyValue();
				scatf1 = scatf[j][0] * scatFactor;
				scatf2 = scatf[j][1] * scatFactor;
				for (int ix = 0; ix < ato.getQuickAtomCoordinatesNumber(); ix++) {
					a1 += scatf1;
					a2 += scatf2;
				}
			}
			ato.trowException = false;
		}
		double structurefactor = (a1 * a1 + a2 * a2);
		if (phase.getFullAtomList().size() == 0)
			structurefactor = 0;
		return structurefactor;
	}


	public static double Fhklcomp(Phase phase, Reflection refl, double[][] scatf) {
		double scatf1, scatf2;
			double factors = refl.getStructureModifier();
			double[] divideFactors = refl.getDivisionFactors();
			double h1 = Constants.PI2 * refl.getH() * divideFactors[0];
			double k1 = Constants.PI2 * refl.getK() * divideFactors[1];
			double l1 = Constants.PI2 * refl.getL() * divideFactors[2];
			double a1 = 0.0;
			double a2 = 0.0;
			double i_4dspace2 = 0.25 / (refl.d_space * refl.d_space);
			Vector<AtomSite> atomList = phase.getFullAtomList();
			int atomNumber = atomList.size();
			for (int j = 0; j < atomNumber; j++) {
				AtomSite ato = atomList.get(j);
				double[][] x = ato.getQuickAtomCoordinates();
				ato.trowException = true;
				if (ato.useThisAtom) {
					double scatFactor = ato.DebyeWaller(refl.getH(), refl.getK(), refl.getL(), i_4dspace2) * ato.getOccupancyValue();
					//ato.DebyeWaller(h, k, l, dspacing) * ato.getOccupancyValue();
					scatf1 = scatf[j][0] * scatFactor;
					scatf2 = scatf[j][1] * scatFactor;
					for (int ix = 0; ix < ato.getQuickAtomCoordinatesNumber(); ix++) {
						double arg = h1 * x[ix][0] + k1 * x[ix][1] + l1 * x[ix][2];
						double w1 = Math.cos(arg);
						double w2 = Math.sin(arg);
						a1 += scatf1 * w1 - scatf2 * w2;
						a2 += scatf1 * w2 + scatf2 * w1;
					}
				}
				ato.trowException = false;
			}
			double structurefactor = (a1 * a1 + a2 * a2) * refl.multiplicity * factors;
			if (phase.getFullAtomList().size() == 0)
				structurefactor = (double) refl.multiplicity;
		return structurefactor;
	}

/*	public static double Fhklcomp(Phase phase, Vector<Reflection> reflections, double[][] scatf) {
		double scatf1, scatf2;
		Vector<AtomSite> atomList = phase.getFullAtomList();
		int atomNumber = atomList.size();
		double structurefactor = 0.0;
		double[] divideFactors = phase.getActivePlanarDefects().getDivisionFactors();
		for (int hkli = 0; hkli < h.length; hkli++) {
			double i_4dspace2 = 0.25 / (refl.d_space * refl.d_space);
			double factors = phase.getActivePlanarDefects().getStructureFactorModifier(h[hkli], k[hkli], l[hkli]);
			int h1 = h[hkli];
			int k1 = k[hkli];
			int l1 = l[hkli];
			for (int mult = 0; mult < 2; mult++) {
				double a1 = 0.0, a2 = 0.0;
				if (mult == 1) {
					h1 = -h1;
					k1 = -k1;
					l1 = -l1;
				}
				for (int j = 0; j < atomNumber; j++) {
					AtomSite ato = atomList.get(j);
					double[][] x = ato.getQuickAtomCoordinates();
					ato.trowException = true;
					if (ato.useThisAtom) {
						double scatFactor = ato.DebyeWaller(h1, k1, l1, i_4dspace2) * ato.getOccupancyValue();
						//ato.DebyeWaller(h, k, l, dspacing) * ato.getOccupancyValue();
						scatf1 = scatf[j][0] * scatFactor;
						scatf2 = scatf[j][1] * scatFactor;
						double h1x = Constants.PI2 * h1 * divideFactors[0];
						double k1x = Constants.PI2 * k1 * divideFactors[1];
						double l1x = Constants.PI2 * l1 * divideFactors[2];
						for (int ix = 0; ix < ato.getQuickAtomCoordinatesNumber(); ix++) {
							double arg = h1x * x[ix][0] + k1x * x[ix][1] + l1x * x[ix][2];
							double w1 = Math.cos(arg);
							double w2 = Math.sin(arg);
							a1 += scatf1 * w1 - scatf2 * w2;
							a2 += scatf1 * w2 + scatf2 * w1;
						}
					}
					ato.trowException = false;
				}
				structurefactor += (a1 * a1 + a2 * a2) * factors;
			}
		}
		if (phase.getFullAtomList().size() == 0)
			structurefactor = (double) multiplicity;
		return structurefactor;
	}*/

/*	public static double dynamicalScatteringFactor(double Fhkl2, double constV) {
		double A = Math.sqrt(Fhkl2) * constV;
		double intA = integralBesselUpTo(A);
		System.out.println(Fhkl2 + " " + constV + " " + A + " " + intA);
		return intA * Fhkl2;
	}

	public static double dynamicalScatteringFactorCorrection(double Fhkl, double constV) {
		double A = Fhkl * constV;
		double intA = integralBesselUpTo(A);
		System.out.println(Fhkl + " " + constV + " " + A + " " + intA);
		return intA;
	}*/

}
