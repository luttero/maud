/*
 * @(#)FourierTransformPeak.java created Jul 23, 2004 Braila
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr;

import it.unitn.ing.fortran.Formatter;
import it.unitn.ing.fortran.InvalidFormatException;
import it.unitn.ing.rista.awt.PlotSimpleData;
import it.unitn.ing.rista.diffr.sizestrain.*;
import it.unitn.ing.rista.util.*;

import java.awt.*;
import java.io.IOException;
import java.io.PrintStream;


/**
 * The FourierTransformPeak is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.5 $, $date: Jul 23, 2004 7:24:17 PM $
 * @since JDK1.1
 */

public class FourierTransformPeak extends PseudoVoigtPeak {

//  double instrumentalHWHM = 0.0, instrumentalGaussian = 0.0;
  double[] sigmaG = null, sigmaC = null, kappa1 = null, kappa2 = null;
  SizeStrainDistributions sizeStrainModel = null;
  int sizeDistN, strainDistN;
  double deltaL;
  public int Ldivision = 64; /*MaudPreferences.getInteger("lineBroadening_FourierTransform.numberOfDivisions", 512);*/

  public FourierTransformPeak(double pos, boolean dspacingbase, boolean energyDispersive, double[] wave, double[] weight,
                              Reflection reflex, int order, SizeStrainDistributions sizestrainModel) {
    super(pos, dspacingbase, energyDispersive, wave, weight, reflex, order);
    sizeStrainModel = sizestrainModel;
  }

/*  protected void addInstrumentalBroadening(double broadInst[]) {
    instrumentalHWHM = Math.abs(broadInst[1]);
    instrumentalGaussian = broadInst[0];

    super.addInstrumentalBroadening(broadInst);  // todo verify this and the previous
    prepareInstrument();
  }*/

  public void prepareInstrument(DiffrDataFile diffrDataFile, double[] finalPositions, double[] wave, double[] hwhm_eta) {

	  int nrad = wave.length;
	  // instrumental part
    double sigma = 0.0;
    sigmaG = new double[nrad];
    sigmaC = new double[nrad];
    kappa1 = new double[nrad];
    kappa2 = new double[nrad];
//	  System.out.println(nrad + " " + finalPositions.length + " " + thwhm.length + " " + wave.length);
    for (int i = 0; i < nrad; i++) {
      if (getdspacingBase())
        sigma = 1.0 / finalPositions[i] - 1.0 / (finalPositions[i] + hwhm_eta[0]);
      else
        sigma = 2.0 / wave[i] * (MoreMath.sind(0.5 * (finalPositions[i] + hwhm_eta[0])) -
            MoreMath.sind(0.5 * finalPositions[i]));
//        sigma = instrumentalHWHM * Math.cos(position[i] / 2 * Constants.DEGTOPI) / wavelength[i];
      sigmaC[i] = -Constants.PI2 * sigma;
      sigmaG[i] = -Constants.PI_QUADRO * sigma * sigma / Constants.LN2;
//      if (instrumentalGaussian < 0.9999999999999)
      kappa1[i] = Constants.SQRT_OF_PILN2 * (1.0 - hwhm_eta[1]) * sigma;
//      else
      kappa2[i] = hwhm_eta[1] * Constants.PI * sigma;
    }

    sizeDistN = sizeStrainModel.crystalliteDistributionNumber();
    strainDistN = sizeStrainModel.microstrainDistributionNumber();
    // end of instrumental part

  }

  public void computePVTransform(double[] pvt, double deltaL, int waveIndex) {
    double L = 0.0;
    for (int i = 0; i < pvt.length; i++) {
      pvt[i] = kappa1[waveIndex] * Math.exp(sigmaG[waveIndex] * L * L) +
          kappa2[waveIndex] * Math.exp(sigmaC[waveIndex] * L);
      L += deltaL;
    }
  }

  public void computePeak(DiffrDataFile diffrDataFile, double[] expfit,
                          Sample asample, Instrument ainstrument,
                          PrintStream out, boolean logOutput, double cutoff,
                          int computeTexture, int computeStrain,
                          int computeFhkl, boolean leBailExtraction, int[] minmaxindex,
                          boolean computeBroadening, boolean reverseX) {

    double[] intensity, Fhklist, actualPosition; //, hwhm, eta, const1, const2;
    int[] minindex, maxindex;

	  Phase aphase = getPhase();
    Reflection refl = getReflex();

	  String phase_name = aphase.toXRDcatString();
	  while (phase_name.length() < 20)
		  phase_name += " ";
	  phase_name = phase_name.substring(20);
//    int dataindex = diffrDataFile.getIndex();
//    int datasetIndex = diffrDataFile.getDataFileSet().getIndex();

//    if (computeBroadening) // todo verify with new beta
//      addInstrumentalBroadening(refl.getInstBroadFactor(dataindex));

    int numberOfFourierPoints = sizeStrainModel.getNumberOfFourierPoints();
    double Lmaxfactor = sizeStrainModel.getLmaxFactor();
    double Lmax = sizeStrainModel.getDeff() * Lmaxfactor;
    if (Lmax <= 5000.0) {
      Lmax = 5000;
    }

	  int nrad = diffrDataFile.getDataFileSet().getInstrument().getRadiationType().getLinesCount();
	  int totalLines = diffrDataFile.positionsPerPattern * nrad;

	  double[] finalposition = new double[totalLines];
	  intensity = new double[totalLines];
	  actualPosition = new double[totalLines];
	  double[] wave = new double[totalLines];
	  double[] const1 = new double[totalLines];
	  double[] const2 = new double[totalLines];
	  minindex = new int[totalLines];
	  maxindex = new int[totalLines];
	  Fhklist = new double[totalLines];
	  double[] radiationWeight = new double[totalLines];

	  double[][][] positions = diffrDataFile.getPositions(aphase);
	  double[][] absDetectorCorrection = new double[diffrDataFile.positionsPerPattern][nrad];
	  int ipv = 0;
	  for (int i = 0; i < nrad; i++) {
		  double energy = Constants.ENERGY_LAMBDA / getRadiationWavelength(i) * 0.001;
		  for (int j = 0; j < diffrDataFile.positionsPerPattern; j++) {
			  finalposition[ipv++] = positions[getOrderPosition()][j][i];
			  int pointIndex = diffrDataFile.getOldNearestPoint(positions[getOrderPosition()][j][i]);
			  absDetectorCorrection[j][i] = ainstrument.getDetector().getAbsorptionCorrection(diffrDataFile, pointIndex, energy);
		  }
	  }
	  double Fhkl;
//	  System.out.println(computeFhkl + " " + refl.getStructureFactor(datasetIndex));
	  switch (computeFhkl) {
		  case Constants.COMPUTED:
			  Fhkl = diffrDataFile.getDataFileSet().getStructureFactors(aphase)[1][getOrderPosition()];
			  break;
		  case Constants.EXPERIMENTAL:
			  Fhkl = diffrDataFile.getDataFileSet().getStructureFactors(aphase)[0][getOrderPosition()];
			  break;
		  case Constants.UNITARY:
			  Fhkl = 1.0;
			  break;
		  default:
			  Fhkl = 1.0;
	  }

	  double intensitySingle = getScaleFactor();
	  double[][] textureFactor;
	  switch (computeTexture) {
		  case Constants.COMPUTED:
			  textureFactor = diffrDataFile.getTextureFactors(aphase)[1][getOrderPosition()];
			  break;
		  case Constants.EXPERIMENTAL:
			  textureFactor = diffrDataFile.getTextureFactors(aphase)[0][getOrderPosition()];
			  break;
		  case Constants.UNITARY:
		  default:
			  textureFactor = new double[diffrDataFile.positionsPerPattern][nrad];
			  for (int i = 0; i < diffrDataFile.positionsPerPattern; i++)
				  for (int j = 0; j < nrad; j++)
				  	   textureFactor[i][j] = 1.0;
	  }
	  for (int i = 0; i < diffrDataFile.positionsPerPattern; i++)
		  for (int j = 0; j < nrad; j++)
			  if (Double.isNaN(textureFactor[i][j]))
				  textureFactor[i][j] = 1.0;

	  double[][] shapeAbs = diffrDataFile.getShapeAbsFactors(aphase, getOrderPosition());
	  double asyConst1 = aphase.getActivePlanarDefects().getPlanarDefectAsymmetryConstant1(this.getReflex());
	  double asyConst2 = aphase.getActivePlanarDefects().getPlanarDefectAsymmetryConstant2(this.getReflex());
// not active, see reciprocal model			double planar_asymmetry = aphase.getActivePlanarDefects().getPlanarDefectAsymmetry(getReflex());

    int startingRad = 0;
    radiationWeight[0] = getRadiationWeight(0);
    double weight = radiationWeight[0];
    for (int i1 = 1; i1 < totalLines; i1++) {
      if (weight < (radiationWeight[i1] = getRadiationWeight(i1))) {
        if (leBailExtraction) {
          weight = radiationWeight[i1];
//							startingRad = i1; this is causing problems
        }
      }
//					if (leBailExtraction)       this is causing problems
//						numberRadiation = startingRad + 1;    this is causing problems
    }

    double step = 0.005; // diffrDataFile.getStep() / 4.0;
    if (getdspacingBase())
      step = finalposition[0] / (Lmax / finalposition[0] - 1.0);
    else
      step = wavelength[0] / (Math.cos(finalposition[0] / 2 * Constants.DEGTOPI) * Lmax);

    Ldivision = 2;
    while (Ldivision < numberOfFourierPoints)
      Ldivision *= 2;
    deltaL = Lmax / Ldivision;
    double range = deltaL;
    if (!getdspacingBase()) {
      range = Math.asin(wavelength[0] / (4.0 * deltaL * MoreMath.cosd(0.5 * finalposition[0]))) * Constants.PITODEG;
      while (Double.isNaN(range) && Ldivision > 2) {
        Ldivision /= 2;
        deltaL = Lmax / Ldivision;
        range = Math.asin(wavelength[0] / (4.0 * deltaL * MoreMath.cosd(0.5 * finalposition[0]))) * Constants.PITODEG;
      }
    }
//    System.out.println("final position " + finalposition[0] + ", range " + range);
//    double rangeMax = step * Ldivision;
//    System.out.println(step + " deltaL " + deltaL + ", Ldivision " + Ldivision);

    step = 2.0 * (MoreMath.sind(0.5 * finalposition[0] + 0.25 * range) - MoreMath.sind(0.5 * finalposition[0] -
        0.25 * range))
        / (wavelength[0] * Ldivision);
//    System.out.println(step + " deltaL " + deltaL + ", Ldivision " + Ldivision);

	  double[][] lorentzPolarization = diffrDataFile.getLorentzPolarization(aphase, getOrderPosition());
	  ipv = 0;
	  for (int i = 0; i < nrad; i++) {
		  for (int j = 0; j < diffrDataFile.positionsPerPattern; j++) {
			  Fhklist[j] = Fhkl;
      if (radiationWeight[j] > 0.0) {
        intensity[ipv] = (intensitySingle * textureFactor[j][i] * shapeAbs[j][i] * Fhkl *
		        radiationWeight[i] * aphase.getScaleFactor() * lorentzPolarization[j][i] * absDetectorCorrection[j][i]);

//        System.out.println(ipv + " 2theta, min " + (finalposition[j] - range) + ", max " + (finalposition[j] + range));
        minindex[ipv] = diffrDataFile.getOldNearestPoint(finalposition[j] - range);
        maxindex[ipv] = diffrDataFile.getOldNearestPoint(finalposition[j] + range) + 1;
	      if (minindex[ipv] > maxindex[ipv]) {
		      int temp = minindex[ipv];
		      minindex[ipv] = maxindex[ipv];
		      maxindex[ipv] = temp;
	      }
//    System.out.println(indexPeak + " min " + minindex[indexPeak] + ", max " + maxindex[indexPeak]);

	      if (!(leBailExtraction && j > startingRad)) {
		      if (minmaxindex[0] > minindex[ipv])
          minmaxindex[0] = minindex[ipv];
        if (minmaxindex[1] < maxindex[ipv])
          minmaxindex[1] = maxindex[ipv];
	      }

        actualPosition[ipv] = finalposition[ipv];
        const1[ipv] = asyConst1;
        const2[ipv] = asyConst2;
        wave[i] = getRadiationWavelength(i);

//          if (aphase.computeFaultAsymmetry)
//            for (int ij = minindex[indexPeak]; ij < maxindex[indexPeak]; ij++) {
//              expfit[ij] += intensity[indexPeak] * peaklist[i].getAsymmetricIntensity(getXData(ij) -
//                      actualPosition[indexPeak]);
//            }
        if (logOutput && out != null) {
          try {
		          out.print(" ");
/*		          Constants.ifmt.write(getOrderPosition(), out);
		          out.print(" ");
		          Constants.ifmt.write(i, out);
		          out.print(" " + phase_name);
		          Constants.ifmt.write(refl.getH(), out);
		          out.print(" ");
		          Constants.ifmt.write(refl.getK(), out);
		          out.print(" ");
		          Constants.ifmt.write(refl.getL(), out);
		          out.print(" ");
		          Constants.ffmt.write((float) refl.d_space, out);
		          out.print(" ");
		          Constants.efmt.write((float) diffrDataFile.getDataFileSet().getStructureFactors(aphase)[1][getOrderPosition()], out);
		          out.print(" ");
		          Constants.efmt.write((float) diffrDataFile.getDataFileSet().getStructureFactors(aphase)[0][getOrderPosition()], out);
		          out.print(" ");
		          Constants.ffmt.write((float) actualPosition[j], out);
		          out.print(" ");
		          Constants.efmt.write((float) diffrDataFile.getStrains(aphase, getOrderPosition())[j], out);
		          out.print(" ");
		          Constants.efmt.write((float) refl.getPlanarDefectDisplacement(), out);
		          out.print(" ");
		          Constants.efmt.write((float) intensity[ipv], out);
		          out.print(" ");
		          Constants.efmt.write(0, out);
		          out.print(" ");
		          Constants.efmt.write(0, out);
		          out.print(" ");
		          Constants.efmt.write((float) Fhkl, out);
		          out.print(" ");
		          Constants.efmt.write((float) intensitySingle, out);
		          out.print(" ");
		          Constants.efmt.write((float) lorentzPolarization[j], out);
		          out.print(" ");
		          Constants.efmt.write((float) textureFactor[j], out);
		          out.print(" ");
		          Constants.efmt.write((float) shapeAbs[j], out);
		          out.print(" ");
		          Constants.ffmt.write((float) radiationWeight[i], out);
		          out.print(" ");
		          Constants.efmt.write((float) aphase.getScaleFactor(), out);*/

	          out.print(getOrderPosition());
	          out.print(" ");
	          out.print(i);
	          out.print(" " + phase_name);
	          out.print(refl.getH());
	          out.print(" ");
	          out.print(refl.getK());
	          out.print(" ");
	          out.print(refl.getL());
	          out.print(" ");
	          out.print((float) refl.d_space);
	          out.print(" ");
	          out.print((float) diffrDataFile.getDataFileSet().getStructureFactors(aphase)[1][getOrderPosition()]);
	          out.print(" ");
	          out.print((float) diffrDataFile.getDataFileSet().getStructureFactors(aphase)[0][getOrderPosition()]);
	          out.print(" ");
	          out.print((float) actualPosition[j]);
	          out.print(" ");
	          out.print((float) diffrDataFile.getStrains(aphase, getOrderPosition())[j][i]);
	          out.print(" ");
	          out.print((float) refl.getPlanarDefectDisplacement(1));
	          out.print(" ");
	          out.print((float) intensity[ipv]);
	          out.print(" ");
	          out.print((float) 0);
	          out.print(" ");
	          out.print((float) 0);
	          out.print(" ");
	          out.print((float) Fhkl);
	          out.print(" ");
	          out.print((float) intensitySingle);
	          out.print(" ");
	          out.print((float) lorentzPolarization[j][i]);
	          out.print(" ");
	          out.print((float) textureFactor[j][i]);
	          out.print(" ");
	          out.print((float) shapeAbs[j][i]);
	          out.print(" ");
	          out.print((float) radiationWeight[i]);
	          out.print(" ");
	          out.print((float) aphase.getScaleFactor());

		          out.print(Constants.lineSeparator);
		          out.flush();
	          } catch (Exception io) {
		          io.printStackTrace();
	          }
        }

      } else {
        intensity[ipv] = 0.0f;
        Fhklist[ipv] = 1.0f;
//        hwhm[indexPeak] = 1.0f;
//        eta[indexPeak] = 0.0f;
        actualPosition[ipv] = 0.0f;
        minindex[ipv] = 0;
        maxindex[ipv] = 4;
        const1[ipv] = 0.0;
        const2[ipv] = 0.0;
      }
			  ipv++;
    }
	  }

	  double[][][][] hwhm_eta = diffrDataFile.getBroadFactors(getPhase());

//	  for (int j = 0; j < diffrDataFile.positionsPerPattern; j++) {
	  double[][] thwhm = hwhm_eta[0][getOrderPosition()];
	  double[][] teta = hwhm_eta[1][getOrderPosition()];
//	  }

	  double[] hwet = new double[2];


	  hwet[0] = thwhm[0][0];  // todo, the radiation should not only be the first
	  hwet[1] = teta[0][0];
	  prepareInstrument(diffrDataFile, finalposition, wave, hwet);
//    diffrDataFile.computeLorentzPolarization(ainstrument, asample, actualPosition, intensity);
    aphase.getActiveTDSModel().computeTDS(diffrDataFile, expfit, this, intensity, Fhklist[0], actualPosition, minmaxindex);
    int[] indexPoint = new int[totalLines];
    for (int j = startingRad; j < totalLines; j++) {
      indexPoint[j] = diffrDataFile.getOldNearestPoint(actualPosition[j]);
    }
    computeFT(diffrDataFile, expfit, minindex, maxindex, intensity, actualPosition, const1, const2,
        indexPoint, wave, step);
  }

  public void computeFT(DiffrDataFile diffrDataFile, double[] f, int[] minindex, int[] maxindex,
                        double[] intensity, double[] position, double[] const1, double[] const2,
                        int[] indexPoint, double[] wave, double step) {

    double[] Atot = new double[Ldivision];
    double[] Apart1 = new double[Ldivision];
    double[] Apart2 = new double[Ldivision];

    double[] x = diffrDataFile.getXData();
//    boolean dspacingBase = diffrDataFile.dspacingbase;
//    boolean increasingX = diffrDataFile.increasingX();
//    boolean constantStep = diffrDataFile.constantstep;

    int numberOfPV = minindex.length;
    computePVTransform(Atot, deltaL, 0);
//    (new PlotSimpleData(new Frame(), Atot)).setVisible(true);
    if (sizeStrainModel.getDeff() != 0.0) {
      double minA = 0;
      double maxA = 0;
      for (int i = 0; i < sizeDistN; i++) {
        CrystalliteDistribution sizeDist = sizeStrainModel.getCrystalliteDistribution(i);
        sizeDist.computeSizeCoefficients(Apart2, sizeStrainModel.getDeff(), deltaL);
        for (int j = 0; j < Ldivision; j++) {
          Apart1[j] += Apart2[j];
          if (minA > Apart1[j])
            minA = Apart1[j];
          if (maxA < Apart1[j])
            maxA = Apart1[j];
        }
      }
//      (new PlotSimpleData(new Frame(), Apart1)).setVisible(true);
      if (maxA != 0 || minA != 0)
        for (int j = 0; j < Ldivision; j++) {
          Atot[j] *= Apart1[j];
          Apart1[j] = 0.0;
        }
    }

    if (sizeStrainModel.getMicrostrain() != 0.0) {
      double minA = 0;
      double maxA = 0;
      for (int i = 0; i < strainDistN; i++) {
        MicrostrainDistribution strainDist = sizeStrainModel.getMicrostrainDistribution(i);
        strainDist.computeStrainCoefficients(Apart2, sizeStrainModel.getMicrostrain(),
            sizeStrainModel.getDeff(), deltaL, getReflex().d_space);
        for (int j = 0; j < Ldivision; j++) {
          Apart1[j] += Apart2[j];
          if (minA > Apart1[j])
            minA = Apart1[j];
          if (maxA < Apart1[j])
            maxA = Apart1[j];
        }
      }
//      (new PlotSimpleData(new Frame(), Apart1)).setVisible(true);
      if (maxA != 0 || minA != 0)
        for (int j = 0; j < Ldivision; j++) {
          Atot[j] *= Apart1[j];
          Apart1[j] = 0.0;
        }

//      (new PlotSimpleData(new Frame(), Atot)).setVisible(true);
    }

    double[] Afinal = new double[Ldivision * 2];
	  double[] Bfinal = new double[Ldivision * 2];   // todo here goes the planar defect prob., const1, const2
    for (int i = 0; i < Ldivision; i++)
      Afinal[i] = Atot[i];

    FFT.realifft(Ldivision * 2, Afinal);

    double Lmax = deltaL * Ldivision;
    double deltaK = wave[0] / (4.0 * Lmax);
    double norm = 0.0;
    double c1 = deltaK * 2.0;
//    double c2 = sin(position[0]);
    double theta = position[0];
    double sintheta = MoreMath.sind(0.5 * theta);
    double stepT = step;
    double theta1 = theta-stepT;
    for (int i = 0; i < Ldivision; i++) {
      double k1 = -c1 + sintheta;
      if (k1 <= 1 && k1 >= 0) {
        theta1 = 2.0 * Math.asin(k1) * Constants.PITODEG;
        stepT = Math.abs(theta - theta1);
      }
      norm += Afinal[i] * stepT;
//      System.out.println(theta + " " + theta1);
      theta = theta1;
      sintheta = MoreMath.sind(0.5 * theta);
    }
//    norm = 1.0;

    for (int ipv = 0; ipv < numberOfPV; ipv++) {
      double singlenorm = intensity[ipv] / norm;
      for (int i = 0; i < Ldivision; i++)
        Atot[i] = Afinal[i] * singlenorm;
//      (new PlotSimpleData(new Frame(), Atot)).setVisible(true);
//        System.out.println(ipv + " " + minindex[ipv] + " " + maxindex[ipv]);
      for (int i = minindex[ipv] + 1; i < maxindex[ipv]; i++) {
        double delta = Math.abs(MoreMath.sind(0.5 * position[ipv]) - MoreMath.sind(0.5 * x[i]));
///        double Lx = 1.0 / delta;
        int n = (int) (delta / deltaK); //(Lx / deltaL);
        int n1 = n + 1;
//        System.out.println(n + " " + n1 + " "+ delta + " " + step + " " + Ldivision);
        if (n1 < Ldivision) {
          double y1 = Atot[n];
          double x1 = n * deltaK;
          double a = (y1 - Atot[n1]) / deltaK;
          double b = y1 - x1 * a;
          f[i] += a * delta + b;
        }
      }
    }
//      (new PlotSimpleData(new Frame(), f)).setVisible(true);


  }

}
