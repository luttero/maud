/*
 * @(#)PseudoVoigt2DPeak.java created 30/08/2019 Casalino
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

package it.unitn.ing.rista.diffr;

import it.unitn.ing.rista.diffr.detector.XRFDetector;
import it.unitn.ing.rista.diffr.radiation.XrayEbelTubeRadiation;
import it.unitn.ing.rista.util.*;
import java.io.PrintStream;

import static org.apache.commons.math3.special.Erf.erfc;

/**
 * The PseudoVoigt2DPeak is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.0 $, $Date: 2019/08/30 20:12:42 $
 * @since JDK1.11
 */

public class PseudoVoigt2DPeak extends PseudoVoigtPeak {
  
  public PseudoVoigt2DPeak(double pos, boolean dspacingbase, boolean energyDispersive, double[] wave, double[] weight,
                         Reflection reflex, int order) {
    super(pos, dspacingbase, energyDispersive, wave, weight, reflex, order);
  }
  
  public void computePeak(DiffrDataFile diffrDataFile, double[] expfit,
                          Sample asample, Instrument ainstrument,
                          PrintStream out, boolean logOutput, double cutoff,
                          int computeTexture, int computeStrain,
                          int computeFhkl, boolean leBailExtraction, int[] minmaxindex,
                          boolean computeBroadening, boolean reverseX) {
    
    double[] hwhm_i, eta, const1, const2, energy;
    int[] minindex, maxindex;
    Phase aphase = getPhase();
    Reflection refl = getReflex();
    int reflexIndex = getOrderPosition();
  
    String phase_name = aphase.toXRDcatString();
//    while (phase_name.length() < 20)
//      phase_name += " ";
//    phase_name = phase_name.substring(0, 20);
//    int dataindex = diffrDataFile.getIndex();
//    int datasetIndex = diffrDataFile.getDataFileSet().getIndex();

//    if (computeBroadening)
//        addInstrumentalBroadening(ainstrument.getInstrumentalBroadeningAt(getMeanPosition(), diffrDataFile));
//      addInstrumentalBroadening(refl.getInstBroadFactor(dataindex));
  
    RadiationType radType = ainstrument.getRadiationType();
    XRFDetector detector = (XRFDetector) ainstrument.getDetector();
    int nrad = radType.getLinesCount();
    int radiationSubdivision = radType.getSubdivision();
    int characteristicLines = nrad;
    if (radType instanceof XrayEbelTubeRadiation)
      characteristicLines = ((XrayEbelTubeRadiation) radType).getNumberOfCharacteristicsLines();
      
    int totalLines = nrad;
    double[] finalposition = new double[totalLines];
    double[][] intensity = new double[3][totalLines];
    hwhm_i = new double[totalLines];
    eta = new double[totalLines];
    double[][] actualPosition = new double[3][totalLines];
    const1 = new double[totalLines];
    const2 = new double[totalLines];
    energy = new double[totalLines];
    minindex = new int[totalLines];
    maxindex = new int[totalLines];
    
    double[] absDetectorCorrection = new double[nrad];
    for (int i = 0; i < nrad; i++) {
      double position = diffrDataFile.getPosition(aphase, reflexIndex, i);
      energy[i] = Constants.ENERGY_LAMBDA / getRadiationWavelength(i);
      finalposition[i] = position;
//      if (position != 0)
//      System.out.println(i + " " + position + " " + energy + " " + reflexIndex);
//      int pointIndex = diffrDataFile.getOldNearestPoint(energy[i]);
      double energyInKeV = energy[i] * 0.001;
      double detectorAbsorption = detector.computeAbsorptionForLineWithEnergy(energyInKeV);
      double detectorEfficiency = detector.computeDetectorEfficiency(energyInKeV);
      absDetectorCorrection[i] = detectorAbsorption * detectorEfficiency;
          // * detector.getAbsorptionCorrection(diffrDataFile, pointIndex, energyInKeV);
  
      const1[i] = 0.0;
      const2[i] = 0.0;
    }
    
    double[] tmpFhkl;
    switch (computeFhkl) {
      case Constants.COMPUTED:
        tmpFhkl = diffrDataFile.getDataFileSet().getCalculatedStructureFactors(aphase, reflexIndex);
        break;
      case Constants.EXPERIMENTAL:
        tmpFhkl = diffrDataFile.getDataFileSet().getExperimentalStructureFactors(aphase, reflexIndex);
        break;
      case Constants.UNITARY:
      default:
        tmpFhkl = new double[nrad];
        for (int i = 0; i < nrad; i++)
          tmpFhkl[i] = 99.0;
    }
    double[] Fhkl = new double[tmpFhkl.length];
    for (int i = 0; i < tmpFhkl.length; i++) {
      Fhkl[i] = tmpFhkl[i];
    }

//    Fhklist = new double[totalLines];
    double[] radiationWeight = new double[nrad];
    int principalRad = 0;
    
    double asyConst1 = aphase.getActivePlanarDefects().getPlanarDefectAsymmetryConstant1(refl);
    double asyConst2 = aphase.getActivePlanarDefects().getPlanarDefectAsymmetryConstant2(refl);
    double planar_asymmetry = aphase.getActivePlanarDefects().getPlanarDefectAsymmetry(refl);
    
    double[] deff = diffrDataFile.getCrystallitesMicrostrains(aphase, reflexIndex, 0);

//	  for (int j = 0; j < diffrDataFile.positionsPerPattern; j++) {
    double[] thwhm = new double[nrad];
    double[] teta = new double[nrad];
    for (int i = 0; i < nrad; i++) {
      thwhm[i] = diffrDataFile.getBroadFactorHWHM(aphase, reflexIndex, i);
      teta[i] = diffrDataFile.getBroadFactorEta(aphase, reflexIndex, i);
    }
//	  }
    double intensitySingle = getScaleFactor();
    double[] textureFactor = new double[nrad];
    
    switch (computeTexture) {
      case Constants.COMPUTED:
        for (int i = 0; i < nrad; i++)
          textureFactor[i] = diffrDataFile.getTextureFactor(aphase, reflexIndex, i);
        break;
      case Constants.EXPERIMENTAL:
        for (int i = 0; i < nrad; i++)
          textureFactor[i] = diffrDataFile.getExperimentalTextureFactor(aphase, reflexIndex, i);
        break;
      case Constants.UNITARY:
      default:
        textureFactor = new double[nrad];
        for (int j = 0; j < nrad; j++)
          textureFactor[j] = 1.0;
    }
    for (int j = 0; j < nrad; j++)
      if (Double.isNaN(textureFactor[j]))
        textureFactor[j] = 1.0;
    
    double[] strainFactor = new double[nrad];
    
    switch (computeStrain) {
      case Constants.COMPUTED:
        for (int i = 0; i < nrad; i++)
          strainFactor[i] = diffrDataFile.getStrainFactor(aphase, reflexIndex, i);
        break;
      case Constants.EXPERIMENTAL:
        for (int i = 0; i < nrad; i++)
          strainFactor[i] = diffrDataFile.getExperimentalStrainFactor(aphase, reflexIndex, i);
        break;
      case Constants.UNITARY:
      default:
        strainFactor = new double[nrad];
        for (int j = 0; j < nrad; j++)
          strainFactor[j] = 0.0;
    }
    for (int j = 0; j < nrad; j++)
      if (Double.isNaN(strainFactor[j]))
        strainFactor[j] = 0.0;
    
    //		if (getOrderPosition() == 2)
//	    System.out.println(diffrDataFile.getLabel() + ", texture factor: " + textureFactor[0]);
    double[] shapeAbs = new double[nrad];
    for (int i = 0; i < nrad; i++)
      shapeAbs[i] = diffrDataFile.getShapeAbsFactor(aphase, reflexIndex, i);

//	  System.out.println(" Total " + diffrDataFile.startingindex + " " + diffrDataFile.finalindex);
//	  System.out.println(" Range " + diffrDataFile.getXData(diffrDataFile.startingindex) + " " +
//			  diffrDataFile.getXData(diffrDataFile.finalindex));
    
    double[] lorentzPolarization = new double[nrad];
    java.util.Vector<java.util.Vector<double[]>> energyBroadeningVector = new java.util.Vector<>(nrad);
//    int totalIndex = 0;
    for (int i = 0; i < nrad; i++) {
      radiationWeight[i] = getRadiationWeight(i);
//      if (radiationWeight[i] > 0.0 && finalposition[i] != 0) {
        lorentzPolarization[i] = diffrDataFile.getLorentzPolarizationFactor(aphase, reflexIndex, i);

      java.util.Vector<double[]> energyBroadening = diffrDataFile.getEnergyBroadFactor(aphase, reflexIndex, i);
      energyBroadeningVector.add(energyBroadening);
//        totalIndex++;
//      }
    }
  
    for (int i = 0; i < nrad; i++) {
      if (radiationWeight[i] > 0.0 && finalposition[i] != 0) {
        const1[i] = asyConst1;
        const2[i] = asyConst2;
        hwhm_i[i] = 1.0 / thwhm[i];
        eta[i] = teta[i];
        double tmpIntensity = intensitySingle * textureFactor[i] * shapeAbs[i] * Fhkl[i] *
            radiationWeight[i] * aphase.getScaleFactor() * lorentzPolarization[i] * absDetectorCorrection[i];
        if (const2[i] != 0.0) {
          for (int index = 0; index < 3; index++)
            intensity[index][i] = tmpIntensity * getReflex().pd_deltaIndex[index];
        } else {
          intensity[0][i] = tmpIntensity;
        }
  
        java.util.Vector<double[]> energyBroadening = energyBroadeningVector.get(i);
        double rangeEnergy = Math.abs(energyBroadening.get(0)[0] * cutoff);
        minindex[i] = diffrDataFile.getOldNearestPoint(energy[i] - rangeEnergy);
        maxindex[i] = diffrDataFile.getOldNearestPoint(energy[i] + rangeEnergy) + 1;
//        System.out.println(i + " " + minindex[i] + " " + maxindex[i]);
        
        if (!leBailExtraction || (leBailExtraction && i == principalRad)) {
          if (minmaxindex[0] > minindex[i])
            minmaxindex[0] = minindex[i];
          if (minmaxindex[1] < maxindex[i])
            minmaxindex[1] = maxindex[i];
        }
        
        if (const2[i] != 0.0) {
          for (int index = 0; index < 3; index++)
            actualPosition[index][i] = getPositionChangeForPlanarDefectDisplacement(finalposition[i], index);
        } else {
          actualPosition[0][i] = finalposition[i];
        }
        
        if (logOutput && out != null) {
          try {
            out.print(" ");
            out.print(getOrderPosition());
            out.print(" ");
            out.print(i);
            out.print(" " + phase_name);
            out.print(" ");
            out.print(refl.getH());
            out.print(" ");
            out.print(refl.getK());
            out.print(" ");
            out.print(refl.getL());
            out.print(" ");
            out.print((float) refl.d_space);
            out.print(" ");
            out.print((float) energy[i]);
            out.print(" ");
            out.print((float) Fhkl[i]); // diffrDataFile.getDataFileSet().getStructureFactors(aphase)[1][reflexIndex][i]);
            out.print(" ");
            out.print((float) diffrDataFile.getDataFileSet().getStructureFactors(aphase)[0][reflexIndex][i]);
            out.print(" ");
            out.print((float) actualPosition[0][i]);
            out.print(" ");
            out.print((float) intensity[0][i]);
            out.print(" ");
            out.print((float) thwhm[i]);
            out.print(" ");
            out.print((float) eta[i]);
            out.print(" ");
            out.print((float) intensitySingle);
            out.print(" ");
            out.print((float) lorentzPolarization[i]);
            out.print(" ");
            out.print((float) textureFactor[i]);
            out.print(" ");
            out.print((float) shapeAbs[i]);
            out.print(" ");
            out.print((float) radiationWeight[i]);
            out.print(" ");
            out.print((float) aphase.getScaleFactor());
            out.print(" ");
            out.print((float) absDetectorCorrection[i]);
            out.print(" ");
            out.print((float) strainFactor[i]);
            out.print(" ");
            out.print((float) refl.getPlanarDefectDisplacement(0));
            
            out.print(Constants.lineSeparator);
            out.flush();
          } catch (Exception e) {
            e.printStackTrace();
          }
        }
        
      } else {
        intensity[0][i] = 0.0f;
        Fhkl[i] = 0.0f;
        hwhm_i[i] = 1.0f;
        eta[i] = 0.0f;
        actualPosition[0][i] = 0.0f;
        minindex[i] = 0;
        maxindex[i] = 0;
        const1[i] = 0.0f;
        const2[i] = 0.0f;
      }
    }

//    diffrDataFile.computeLorentzPolarization(ainstrument, asample, actualPosition, intensity);
// planar defects
    /*		aphase.getActiveTDSModel().computeTDS(diffrDataFile, expfit, this, intensity[0], Fhkl, actualPosition[0], minmaxindex);*/

    if (radiationSubdivision > 1)
      computeFunctions(diffrDataFile.getXData(), expfit, minindex, maxindex,
        intensity, eta, hwhm_i, actualPosition, const1, const2, energy,
        diffrDataFile.dspacingbase, diffrDataFile.energyDispersive, diffrDataFile.increasingX(), planar_asymmetry,
        deff[0], diffrDataFile.sintheta, energyBroadeningVector, refl.d_space, radiationSubdivision, characteristicLines);
    else
      computeFunctionsQuick(diffrDataFile.getXData(), expfit, minindex, maxindex,
          intensity, eta, hwhm_i, energy, diffrDataFile.sintheta, energyBroadeningVector, refl.d_space);
  }
  
  public void computeFunctions(double[] x, double[] f, int[] minindex, int[] maxindex,
                               double[][] intensity, double[] eta, double[] hwhm_i, double[][] position,
                               double[] const1, double[] const2, double[] energy, boolean dspacingBase,
                               boolean energyDispersive, boolean increasingX, double planar_asymmetry,
                               double deff, double sintheta, java.util.Vector<java.util.Vector<double[]>> energyBroadeningVector,
                               double d_space, int radiationSubdivision, int characteristicLines) {
  
    double theta2 = MoreMath.asind(sintheta) * 2.0;
  
    double constEnergy = Constants.ENERGY_LAMBDA / (2.0 * d_space);

    int numberOfPV = minindex.length;
    sintheta *= 2.0;
    double differenceDspace;
    double diffE = 1.0 / radiationSubdivision;
//    int indexV = 0;
    int bremLines = numberOfPV - characteristicLines;
    int totalLines = (bremLines - 1) * radiationSubdivision + 1 + characteristicLines;
    double[] energy_div = new double[totalLines];
    double[] intensity_div = new double[totalLines];
//    double[] intensity_div_alt = new double[totalLines];
    double[] dgx_e_div = new double[totalLines];
    double[] dcx_e_div = new double[totalLines];
    double[] hwhm_i_e_div = new double[totalLines];
    int[] imin = new int[totalLines];
    int[] imax = new int[totalLines];
    int ii = 0;
    double intensity_a;
    for (int ipv = 0; ipv < numberOfPV; ipv++) {
      java.util.Vector<double[]> broad = energyBroadeningVector.get(ipv);
      if (ipv > characteristicLines) {
  
        if (broad != null) {
          int lastIndex = ii - 1;
          int nextIndex = lastIndex + radiationSubdivision;
  
          imin[nextIndex] = minindex[ipv];
          imax[nextIndex] = maxindex[ipv];
          double deltaEnergy = (energy[ipv] - energy[ipv - 1]) * diffE;
  
          energy_div[nextIndex] = energy[ipv];
          hwhm_i_e_div[nextIndex] = 1.0 / broad.get(0)[0];
          double eta_e = broad.get(1)[0];
          dgx_e_div[nextIndex] = (1.0 - eta_e) * Constants.sqrtln2pi * hwhm_i_e_div[nextIndex];
          dcx_e_div[nextIndex] = eta_e * hwhm_i_e_div[nextIndex] / Math.PI;
  
          double dgx = (1.0 - eta[ipv]) * Constants.sqrtln2pi * hwhm_i[ipv];
          double dcx = eta[ipv] * hwhm_i[ipv] / Math.PI;
          double theta2_i = constEnergy / energy[ipv];
          theta2_i = MoreMath.asind(theta2_i) * 2.0;
          double dx = theta2_i - theta2;
          dx *= hwhm_i[ipv];
          dx *= dx;
          if (dx > 30.0)
            intensity_a = dcx / (1.0 + dx);
          else
            intensity_a = dcx / (1.0 + dx) + dgx * Math.exp(-Constants.LN2 * dx);
          
          intensity_div[nextIndex] = intensity_a * intensity[0][ipv] * diffE;
//          intensity_div_alt[nextIndex] = intensity_div[nextIndex];
  
          double delta_hwhm_i_e = (hwhm_i_e_div[nextIndex] - hwhm_i_e_div[lastIndex]) * diffE;
          double delta_dgx_e = (dgx_e_div[nextIndex] - dgx_e_div[lastIndex]) * diffE;
          double delta_dcx_e = (dcx_e_div[nextIndex] - dcx_e_div[lastIndex]) * diffE;
          double delta_intensity_e = (intensity[0][ipv] - intensity[0][ipv - 1]) * diffE;
          double delta_eta = (eta[ipv] - eta[ipv - 1]) * diffE;
          double delta_hwhm_i = (hwhm_i[ipv] - hwhm_i[ipv - 1]) * diffE;
//          double delta_intensity_e_alt = (intensity_div_alt[nextIndex] - intensity_div_alt[lastIndex]) * diffE;
//          if (energy_div[nextIndex] > 11000 && energy_div[nextIndex] < 14000)
//            System.out.println(energy_div[nextIndex] + " " + ii + " " + intensity[0][ipv] + " " + intensity[0][ipv - 1]);
  
          for (int y = 0; y < radiationSubdivision - 1; y++) {
            imin[ii] = minindex[ipv];
            imax[ii] = maxindex[ipv];
            energy_div[ii] = energy[ipv - 1] + deltaEnergy * y;
            hwhm_i_e_div[ii] = hwhm_i_e_div[lastIndex] + delta_hwhm_i_e * y;
            dgx_e_div[ii] = dgx_e_div[lastIndex] + delta_dgx_e * y;
            dcx_e_div[ii] = dcx_e_div[lastIndex] + delta_dcx_e * y;
  
            double eta1 = eta[ipv - 1] + delta_eta * y;
            double hwhm_i1 = hwhm_i[ipv - 1] + delta_hwhm_i * y;
            dgx = (1.0 - eta1) * Constants.sqrtln2pi * hwhm_i1;
            dcx = eta1 * hwhm_i1 / Math.PI;
            theta2_i = constEnergy / energy_div[ii];
            theta2_i = MoreMath.asind(theta2_i) * 2.0;
            dx = theta2_i - theta2;
            dx *= hwhm_i1;
            dx *= dx;
            if (dx > 30.0)
              intensity_a = dcx / (1.0 + dx);
            else
              intensity_a = dcx / (1.0 + dx) + dgx * Math.exp(-Constants.LN2 * dx);
  
            double inten = intensity[0][ipv - 1] + delta_intensity_e * y;
            intensity_div[ii] = inten * intensity_a * diffE;
//            double intensity_div_alt1 = intensity_div_alt[lastIndex] + delta_intensity_e_alt * y;
//          if (energy_div[ii] > 11000 && energy_div[ii] < 14000)
//            System.out.println(energy_div[ii] + " " + intensity_div_alt1 + " " + intensity_div[ii] + " " + intensity_a + " " + inten);
            ii++;
          }
          ii++;
        } else {
          for (int y = 0; y < radiationSubdivision; y++) {
            imin[ii] = 0;
            imax[ii] = 0;
            ii++;
          }
        }
      } else {
        
        imin[ii] = minindex[ipv];
        imax[ii] = maxindex[ipv];
        double hwhm = 1.0;
        double eta_e = 0;
        if (broad != null) {
          hwhm = broad.get(0)[0];
          eta_e = broad.get(1)[0];
        }
        hwhm_i_e_div[ii] = 1.0 / hwhm;
      
        energy_div[ii] = energy[ipv];
        dgx_e_div[ii] = (1.0 - eta_e) * Constants.sqrtln2pi * hwhm_i_e_div[ii];
        dcx_e_div[ii] = eta_e * hwhm_i_e_div[ii] / Math.PI;
  
        double dgx = (1.0 - eta[ipv]) * Constants.sqrtln2pi * hwhm_i[ipv];
        double dcx = eta[ipv] * hwhm_i[ipv] / Math.PI;
        double theta2_i = constEnergy / energy[ipv];
        theta2_i = MoreMath.asind(theta2_i) * 2.0;
        double dx = theta2_i - theta2;
        dx *= hwhm_i[ipv];
        dx *= dx;
        if (dx > 30.0)
          intensity_a = dcx / (1.0 + dx);
        else
          intensity_a = dcx / (1.0 + dx) + dgx * Math.exp(-Constants.LN2 * dx);
        intensity_div[ii] = intensity_a * intensity[0][ipv];
//        intensity_div_alt[ii] = intensity_div[ii];
        
        if (ipv == characteristicLines) {
          intensity_div[ii] *= diffE;
//          intensity_div_alt[ii] *= diffE;
        }
        ii++;
      }
      
    }
    
    for (int ipv = 0; ipv < totalLines; ipv++) {
      int imin1 = imin[ipv];
      int imax1 = imax[ipv];
      if (imax1 - imin1 > 1) {
//				System.out.println(ipv + " " + planar_asymmetry + " " + const1[ipv] + " " + const2[ipv]);
/*        if (const2[ipv] != 0.0f) {
          for (int index = 0; index < 3; index++) {
            if (position[index][ipv] != 0.0) {
              double dgx = intensity[index][ipv] * (1.0 - eta[ipv]) * Constants.sqrtln2pi * hwhm_i[ipv];
              double dcx = intensity[index][ipv] * eta[ipv] * hwhm_i[ipv] / Math.PI;
              for (int i = imin1; i < imax1; i++) {
                double dx = x[i] - position[index][ipv];
                double dx_pi = dx;
                differenceDspace = (x[i] - position[index][ipv]) / Constants.ENERGY_LAMBDA * sintheta;
                double asy = 0.0;
                if (Math.abs(dx) > 1.0E-6) {
                  double cxasy = const1[ipv] * deff * differenceDspace;
                  double rasy = 1.0 + 1.0 / (cxasy * cxasy);
                  asy = intensity[index][ipv] * const2[ipv] / (rasy * dx_pi);
//							System.out.println("Peak: " + ipv + " " + x[i] + " " + position[ipv] + " " + cxasy + " " + const1[ipv] + " " + const2[ipv] + " " + i + " " + asy);
                }
                dx *= hwhm_i[ipv];
                dx *= dx;
                if (dx > 30.0)
                  f[i] += (float) (dcx / (1.0 + dx));
                else
                  f[i] += (float) (dcx / (1.0 + dx) + dgx * Math.exp(-Constants.LN2 * dx));
                f[i] += asy;
              }
            }
          }
        } else {
          if (position[0][ipv] != 0.0 && intensity[0][ipv] != 0.0) {*/
  
/*            java.util.Vector<double[]> broad = energyBroadeningVector.get(ipv);
  
            double hwhm_e = broad.get(0)[0] * 0.01;
            double eta_e = broad.get(1)[0];*/
  
/*            if (broad.size() > 2) {
              fS = broad.get(2)[0];
              double beta = broad.get(3)[0];
              fS *= mhuDet;
    
              if (beta > 0)
                one_over_beta = 1.0 / beta;
              else
                one_over_beta = 0.0;
    
              if (coreShellID != -1) {
                if (transitionID.toUpperCase().startsWith("KL")) { // Kalpha
                  fT = broad.get(4)[0];
                  for (int i = 1; i < broad.get(4).length; i++)
                    fT += broad.get(4)[i] * MoreMath.pow(mhuDet, i - 1);
                } else { //if (transitionID.toUpperCase().startsWith("KM"))
                  fT = broad.get(5)[0];
                  for (int i = 1; i < broad.get(5).length; i++)
                    fT += broad.get(5)[i] * MoreMath.pow(mhuDet, i - 1);
                }
              }
            } else {
             double fT = 0;
             double fS = 0;
             double one_over_beta = 1.0;
            }
  
            double one_over_hwhm = 1.0 / hwhm_e;
            double symPeakIntensity = 1.0 - fT - fS;
            double dgx_e = symPeakIntensity * (1.0 - eta_e) * Constants.sqrtln2pi * one_over_hwhm;
            double dcx_e = symPeakIntensity * eta_e * one_over_hwhm / Math.PI;
//            double one_over_sigma = Constants.sqrt2ln2 * one_over_hwhm;
//            double one_over_beta2 = one_over_beta * one_over_beta;
  */
        double[] tmpFit = new double[imax1 - imin1];
        double intensity1 = intensity_div[ipv];
/*            double dgx = (1.0 - eta[ipv]) * Constants.sqrtln2pi * hwhm_i[ipv];
            double dcx = eta[ipv] * hwhm_i[ipv] / Math.PI;
  
            double theta2_i = Constants.ENERGY_LAMBDA / (energy[ipv] * 2.0 * d_space);
            theta2_i = MoreMath.asind(theta2_i) * 2.0;
            double dx = theta2_i - theta2;
            dx *= hwhm_i[ipv];
            dx *= dx;
            if (dx > 30.0)
              intensity_a = dcx / (1.0 + dx);
            else
              intensity_a = dcx / (1.0 + dx) + dgx * Math.exp(-Constants.LN2 * dx);*/
  
        if (intensity1 > 1.0E-9) {
//              intensity_a *= intensity[0][ipv];
    
          for (int i = imin1; i < imax1; i++) {
            double dx_e = x[i] - energy_div[ipv]; //position[0][ipv];
            dx_e *= hwhm_i_e_div[ipv];
            dx_e *= dx_e;
//                System.out.println("Refl: " + x[i] + " " + energy_div[ipv] + " " + hwhm_i_e_div[ipv]);
      
            if (dx_e > 30.0)
              tmpFit[i - imin1] += intensity1 * dcx_e_div[ipv] / (1.0 + dx_e);
            else
              tmpFit[i - imin1] += intensity1 * (dcx_e_div[ipv] / (1.0 + dx_e) + dgx_e_div[ipv] *
                  Math.exp(-Constants.LN2 * dx_e));
  
/*            if (fT > 0)
              intensity += getIntensity() * fT * one_over_beta * one_over_sigma * 0.5 / Math.exp(-0.5 * one_over_beta2) *
                  Math.exp(dx1 * one_over_beta * one_over_sigma) *
                  erfc(Constants.one_sqrt2 * (dx1 * one_over_sigma + one_over_beta));
            if (fS > 0)
              intensity += getIntensity() * fS * erfc(Constants.one_sqrt2 * dx1 * one_over_sigma) /
                  (2.0 * getEnergy());*/
  
/*              double dx1 = position[0][ipv] - x[i];
              dx1 *= hwhm_i[ipv];
              dx1 *= dx1;
              if (dx1 > 30.0)
                tmpFit[i - imin] += (float) (dcx / (1.0 + dx1));
              else
                tmpFit[i - imin] += (float) (dcx / (1.0 + dx1) + dgx * Math.exp(-Constants.LN2 * dx1));
 */
          }
        }
        if (planar_asymmetry != 0.0) {
          double rec_planar_asymmetry = 1.0 / planar_asymmetry;
          double newFit[] = new double[imax1 - imin1];
          int absdirection = -1;  // increasing step
          if (!increasingX)
            absdirection = -absdirection;
          for (int j = imin1; j < imax1; j++) {
            int direction = absdirection;
            double function = tmpFit[j - imin1];
            double normalization = 1.0;
            int ij = j + direction;
            double difference;
            double expasymmetry = 1.0;
            for (; expasymmetry > 0.0001 && ij < imax1 && ij >= imin1; ij += direction) {
              difference = Math.abs(x[ij] - x[j]);
              expasymmetry = Math.exp(-difference * rec_planar_asymmetry);
              function += tmpFit[ij - imin1] * expasymmetry;
              normalization += expasymmetry;
            }
            newFit[j - imin1] = function / normalization;
          }
          System.arraycopy(newFit, 0, tmpFit, 0, imax1 - imin1);
        }
        for (int i = imin1; i < imax1; i++)
          f[i] += tmpFit[i - imin1];
      }// else
//        System.out.println(intensity_div[ipv] + " " + energy_div[ipv] + " " + imin1 + " " + imax1);
//        }
//      }
    }
  }
  
  public void computeFunctionsQuick(double[] x, double[] f, int[] minindex, int[] maxindex, double[][] intensity,
                                    double[] eta, double[] hwhm_i, double[] energy, double sintheta,
                                    java.util.Vector<java.util.Vector<double[]>> energyBroadeningVector,
                                    double d_space) {
    
    double theta2 = MoreMath.asind(sintheta) * 2.0;
    double constEnergy = Constants.ENERGY_LAMBDA / (2.0 * d_space);
    
    int numberOfPV = minindex.length;
    int totalLines = numberOfPV;
    double intensity_a;
    for (int ipv = 0; ipv < totalLines; ipv++) {
      double hwhm = 1.0;
      double eta_e = 0;
      java.util.Vector<double[]> broad = energyBroadeningVector.get(ipv);
      if (broad != null) {
        hwhm = broad.get(0)[0];
        eta_e = broad.get(1)[0];
      }
      double hwhm_i_e = 1.0 / hwhm;
  
      double dgx_e = (1.0 - eta_e) * Constants.sqrtln2pi * hwhm_i_e;
      double dcx_e = eta_e * hwhm_i_e / Math.PI;
  
      double dgx = (1.0 - eta[ipv]) * Constants.sqrtln2pi * hwhm_i[ipv];
      double dcx = eta[ipv] * hwhm_i[ipv] / Math.PI;
      double theta2_i = constEnergy / energy[ipv];
      theta2_i = MoreMath.asind(theta2_i) * 2.0;
      double dx = theta2_i - theta2;
      dx *= hwhm_i[ipv];
      dx *= dx;
      if (dx > 30.0)
        intensity_a = dcx / (1.0 + dx);
      else
        intensity_a = dcx / (1.0 + dx) + dgx * Math.exp(-Constants.LN2 * dx);
      intensity_a *= intensity[0][ipv];
      
      int imin1 = minindex[ipv];
      int imax1 = maxindex[ipv];
      if (imax1 - imin1 > 0) {
        double[] tmpFit = new double[imax1 - imin1];
        if (intensity_a > 1.0E-9) {
          for (int i = imin1; i < imax1; i++) {
            double dx_e = x[i] - energy[ipv]; //position[0][ipv];
            dx_e *= hwhm_i_e;
            dx_e *= dx_e;
            if (dx_e > 30.0)
              tmpFit[i - imin1] += intensity_a * dcx_e / (1.0 + dx_e);
            else
              tmpFit[i - imin1] += intensity_a * (dcx_e / (1.0 + dx_e) + dgx_e *
                  Math.exp(-Constants.LN2 * dx_e));
          }
        }
        for (int i = imin1; i < imax1; i++)
          f[i] += tmpFit[i - imin1];
      }
    }
  }
  
}
