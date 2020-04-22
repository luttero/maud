/*
 * @(#)FluorescenceLine.java created Mar 9, 2009 Caen
 *
 * Copyright (c) 2009 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.util;

import com.github.tschoonj.xraylib.Xraylib;

import static org.apache.commons.math3.special.Erf.erfc;
//import com.imsl.math.Sfun;

/**
 * The FluorescenceLine is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Mar 9, 2009 11:20:32 AM $
 * @since JDK1.1
 */
public class FluorescenceLine {

	double dgx = 1.0;
	double dcx = 1.0;
	double eta;
	double hwhm;
	double one_over_hwhm;
	double one_over_sigma;
	double one_over_beta;
	double one_over_beta2;
	double exp_one_over_beta2;
	double erf_arg;
	double one_over_2energy;
	double fT;
	double fS;

	double energy;
	double energy_eV;
	double intensity = 1.0;
	double[] multipleIntensity = null;
	private double transitionProbability;
	int coreShellID = -1;
	double fluorescenceYield = 0;
	double coreShellEnergy = 0;
	public String transitionID = "";
	public int xrl_line_number = -9999;
//	double mhuDet = 0;
	
	public boolean areaCorrection = false;

	public FluorescenceLine(double energyPosition, int inner_shell_ID, double innerShellEnergy, String id) {
    energy = energyPosition;
    energy_eV = energy * 1000;
    intensity = 1.0;
		coreShellID = inner_shell_ID;
		coreShellEnergy = innerShellEnergy;
		transitionID = id;
		if (inner_shell_ID != -1) { // not for transfert lines
			String xlr_lineID = transitionID + "_LINE";
			try {
				xrl_line_number = (int) Xraylib.class.getDeclaredField(xlr_lineID).get(null);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}

	}

	public FluorescenceLine(FluorescenceLine lineToCopy) {
		energy = lineToCopy.energy;
    energy_eV = lineToCopy.energy_eV;
		intensity = lineToCopy.intensity;
//		mhuDet = lineToCopy.mhuDet;
		coreShellID = lineToCopy.coreShellID;
		transitionID = lineToCopy.transitionID;
		xrl_line_number = lineToCopy.xrl_line_number;
		transitionProbability = lineToCopy.transitionProbability;
		fluorescenceYield = lineToCopy.fluorescenceYield;
		coreShellEnergy = lineToCopy.coreShellEnergy;
		eta = lineToCopy.eta;
		hwhm = lineToCopy.hwhm;
		one_over_hwhm = lineToCopy.one_over_hwhm;
    one_over_sigma = lineToCopy.one_over_sigma;
    one_over_beta = lineToCopy.one_over_beta;
    one_over_beta2 = lineToCopy.one_over_beta2;
    exp_one_over_beta2 = lineToCopy.exp_one_over_beta2;
    erf_arg = lineToCopy.erf_arg;
    one_over_2energy = lineToCopy.one_over_2energy;
    fT = lineToCopy.fT;
    fS = lineToCopy.fS;
		dgx = lineToCopy.dgx;
		dcx = lineToCopy.dcx;
		if (lineToCopy.multipleIntensity != null) {
      multipleIntensity = new double[lineToCopy.multipleIntensity.length];
      for (int i = 0; i < lineToCopy.multipleIntensity.length; i++)
        multipleIntensity[i] = lineToCopy.multipleIntensity[i];
    }
    areaCorrection = lineToCopy.areaCorrection;
  }

  public void setIntensity(double intensity) {
    this.intensity = intensity;
  }

  public double getIntensity() {
    return intensity;
  }

  public void setEnergy(double energy) {
    this.energy = energy;
    energy_eV = energy * 1000;
  }

  public double getEnergy() {
    return energy;
  }
  
  public double getEnergyIneV() {
	  return energy_eV;
  }
  
//  public void setMhuDet(double value) {
//	  mhuDet = value;
//  }
  
//  public double getMhuDet() {
//	  return mhuDet;
//  }

	public double getCoreShellEnergy() { return coreShellEnergy; }

  public void setShape(java.util.Vector<double[]> broad) {
    hwhm = broad.get(0)[0];
//    System.out.println("Fluo: " + hwhm + " " + getEnergy());
    eta = broad.get(1)[0];
  
    fT = 0;
    fS = 0;
    one_over_beta = 1.0;
    if (broad.size() > 2) {
      fS = broad.get(2)[0];
      double beta = broad.get(3)[0];
    
      if (beta > 0)
        one_over_beta = 1.0 / beta;
      else
        one_over_beta = 0.0;
    
      if (coreShellID != -1) {
        if (transitionID.toUpperCase().startsWith("KL")) // Kalpha
          fT = broad.get(4)[0];
        else if (transitionID.toUpperCase().startsWith("KM"))
          fT = broad.get(5)[0];
        else
          fT = broad.get(4)[0];
      }
    }

	  one_over_hwhm = 1.0 / hwhm;
//      hwhm_cut = Math.abs(hwhm) * 5.0;
	  double symPeakIntensity = (1.0 - fT - fS);
	  dgx = symPeakIntensity * (1.0 - eta) * Constants.sqrtln2pi * one_over_hwhm;
	  dcx = symPeakIntensity * eta * one_over_hwhm / Math.PI;
	  one_over_sigma = Constants.sqrt2ln2 * one_over_hwhm;
	  one_over_beta2 = one_over_beta * one_over_beta;
	  exp_one_over_beta2 = one_over_beta * one_over_sigma * 0.5 / Math.exp(-0.5 * one_over_beta2);
  
    erf_arg = Constants.one_sqrt2 * one_over_sigma;
    one_over_2energy = 1.0 / (2.0 * getEnergyIneV());
  }

  public double getIntensity(double x) {
    double intensity;
    double dx1 = x - getEnergyIneV();
//    if (dx1 > hwhm_cut)
//      return 0.0;
    double dx = dx1;
    dx *= one_over_hwhm;
    dx *= dx;
    if (dx > 30.0)
      intensity = dcx / (1.0 + dx);
    else
      intensity = dcx / (1.0 + dx) + dgx * Math.exp(-Constants.LN2 * dx);
  
    if (fT > 0)
      intensity += exp_one_over_beta2 * fT * Math.exp(dx1 * one_over_beta * one_over_sigma) *
          erfc(Constants.one_sqrt2 * (dx1 * one_over_sigma + one_over_beta));
    if (fS > 0)
      intensity += erfc(dx1 * erf_arg) * one_over_2energy * fS;
    return getIntensity() * intensity;
	}
  
  public void multiplyIntensityBy(double value) {
    setIntensity(getIntensity() * value);
    if (multipleIntensity != null)
      for (int i = 0; i < multipleIntensity.length; i++)
        multipleIntensity[i] *= value;
  }
  
  public void multiplyIntensityBy(double[] value) {
	  if (multipleIntensity == null || multipleIntensity.length != value.length) {
      multipleIntensity = new double[value.length];
      for (int i = 0; i < multipleIntensity.length; i++)
        multipleIntensity[i] = getIntensity();
    }
	  for (int i = 0; i < value.length; i++)
      multipleIntensity[i] *= value[i];
  }
  
  public double[] getMultipleIntensity() {
	  return multipleIntensity;
  }
  
  public void setMultipleIntensity(double[] someIntensity) {
    multipleIntensity = new double[someIntensity.length];
    for (int i = 0; i < someIntensity.length; i++)
      multipleIntensity[i] = someIntensity[i];
  }
  
  public double getIntensityMultiple(int index) {
	  return multipleIntensity[index];
  }
  
  public void setTransitionProbability(double transitionProbability) {
		this.transitionProbability = transitionProbability;
	}

	public double getTransitionProbability() {
		return transitionProbability;
	}

	public int getCoreShellID() {
		return coreShellID;
	}

	public void setFluorescenceYield(double value) {
		fluorescenceYield = value;
	}

	public double getFluorescenceYield() {
		return fluorescenceYield;
	}

	public String toString() {
		return transitionID + " " + getEnergy() + " " + getTransitionProbability() * getFluorescenceYield();
	}

	public void printToConsole() {
		System.out.println("Peak " + energy + " " + dcx + " " + dgx + " " + hwhm + " " + eta);
	}
}
