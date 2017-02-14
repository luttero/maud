/*
 * @(#)basicPeak.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.MoreMath;

import java.io.PrintStream;

/**
 * The basicPeak is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.5 $, $Date: 2006/11/10 09:33:00 $
 * @since JDK1.1
 */

public class basicPeak implements Peak {

  public boolean dspacingBase = false;
  public boolean energyDispersive = false;
  double IntegratedIntensity = 0.0;
  double[] wavelength;
  double[] waveWeight;
  int numberOfRadiations = 0;
  double[] position = null;
  double meanPosition = 0.0;
  double meanWavelength = 0.0;
  Phase thephase;
  Reflection reflex = null;
  public int orderPosition = 0;
  boolean toRemove = false;

  public basicPeak(double pos, boolean dspacingBase, boolean energyDispersive, double[] wave, double[] weight,
                   Reflection reflex, int order) {
    this.dspacingBase = dspacingBase;
    this.energyDispersive = energyDispersive;

    numberOfRadiations = wave.length;
    wavelength = new double[numberOfRadiations];
    waveWeight = new double[numberOfRadiations];
    position = new double[numberOfRadiations];
    meanPosition = 0.0;
    meanWavelength = 0.0;
    double totWeight = 0.0;
    for (int i = 0; i < numberOfRadiations; i++) {
      wavelength[i] = wave[i];
      waveWeight[i] = weight[i];
      if (dspacingBase || energyDispersive)
        position[i] = pos;
      else
        position[i] = DiffrDataFile.computeposition(pos, wavelength[i]);
      if (position[i] < 180.0 && position[i] > -180.0) {
        meanPosition += position[i] * weight[i];
        totWeight += weight[i];
	      meanWavelength += wavelength[i] * weight[i];
      }
    }
    if (totWeight != 0.0) {
      meanPosition /= totWeight;
      meanWavelength /= totWeight;
    } else {
      meanPosition = 180.0;
      meanWavelength = 1.54;
    }

    setReflex(reflex);

    orderPosition = order;
//	  System.out.println("Creating peak n: " + order + " for phase " + reflex.getParent().getPhaseName() + " " + meanPosition);
  }

  public basicPeak() {
  }

  public double getIntensityAt(double x) {
    return 0.0;
  }

  public double getRadiationWeight(int index) {
    return waveWeight[index];
  }

  public double getRadiationWavelength(int index) {
    return wavelength[index];
  }

  public double getMeanWavelength() {
    return meanWavelength;
  }

  public void setIntensity(double intensity) {
    IntegratedIntensity = intensity;
  }

  public double getScaleFactor() {
    return IntegratedIntensity;
  }

	public int getOrderPosition() {
		return orderPosition;
	}

  public boolean getdspacingBase() {
    return dspacingBase;
  }

  public boolean getEnergyDispersive() {
    return energyDispersive; // todo cambiare come per dspacingBase
  }

	public double getPositionChangeForPlanarDefectDisplacement(double pos, int index) {
		double planarDefectDisplacement = getReflex().getPlanarDefectDisplacement(index); // in d-space, delta(d)/d
		if (planarDefectDisplacement != 0.0) {
			if (!getdspacingBase()) {
				planarDefectDisplacement *= 2.0 * Constants.PITODEG;
				pos -= MoreMath.tand(pos * 0.5) * planarDefectDisplacement;
			} else if (getEnergyDispersive()) {
				planarDefectDisplacement += 1.0;
				pos /= planarDefectDisplacement;
			} else {
				pos = pos + planarDefectDisplacement * Constants.PITODEG * MoreMath.tand(pos * 0.5);
			}
		}
		return pos;
	}


	public void setReflex(Reflection refl) {
    reflex = refl;
    thephase = getReflex().getParent();
  }

  public Reflection getReflex() {
    return reflex;
  }

  public Phase getPhase() {
    return thephase;
  }

  public double[] getPosition() {
    // in deg if an angle
    return position;
  }

  public double getMeanPosition() {
    // in deg if an angle
    return meanPosition;
  }

  public void setPosition(double pos, int radnumber) {
    // in deg if an angle
    position[radnumber] = pos;
  }

  public boolean intensityExtractionAllowed() {
    return getPhase().extractIntensities();
  }

  public boolean structureFactorExtractionAllowed() {
    return getPhase().extractStructureFactors();
  }

  public void computePeak(DiffrDataFile diffrDataFile, double[] expfit,
                              Sample asample, Instrument ainstrument,
                              PrintStream out, boolean logOutput, double cutoff,
                              int computeTexture, int computeStrain,
                              int computeFhkl, boolean leBailExtraction, int[] minmaxindex,
                              boolean computeBroadening, boolean reverseX) {

  }

}
