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
	double fT;
	double fS;

	double energy;
	double intensity = 1.0;
	private double transitionProbability;
	int coreShellID = -1;
	double fluorescenceYield = 0;
	double coreShellEnergy = 0;
	public String transitionID = "";
	public int xrl_line_number = -9999;
	public double mhuDet = 0;

	public FluorescenceLine(double energyPosition, int inner_shell_ID, double innerShellEnergy, String id) {
    energy = energyPosition;
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
		intensity = lineToCopy.intensity;
		mhuDet = lineToCopy.mhuDet;
		coreShellID = lineToCopy.coreShellID;
		transitionID = lineToCopy.transitionID;
		xrl_line_number = lineToCopy.xrl_line_number;
		transitionProbability = lineToCopy.transitionProbability;
		fluorescenceYield = lineToCopy.fluorescenceYield;
		coreShellEnergy = lineToCopy.coreShellEnergy;
		eta = lineToCopy.eta;
		hwhm = lineToCopy.hwhm;
		one_over_hwhm = lineToCopy.one_over_hwhm;
		dgx = lineToCopy.dgx;
		dcx = lineToCopy.dcx;
	}

  public void setIntensity(double intensity) {
    this.intensity = intensity;
  }

  public double getIntensity() {
    return intensity;
  }

  public void setEnergy(double energy) {
    this.energy = energy;
  }

  public double getEnergy() {
    return energy;
  }

	public double getCoreShellEnergy() { return coreShellEnergy; }

  public void setShape(double[][] broad) {
	  double energy = getEnergy();
	  hwhm = broad[0][0];
	  eta = broad[1][0];
	  for (int i = 1; i < broad[0].length; i++) {
	  	  hwhm += broad[0][i] * MoreMath.pow(energy, i - 1);
		  eta += broad[1][i] * MoreMath.pow(energy, i - 1);
	  }
	  hwhm = Math.sqrt(Math.abs(hwhm));

	  if (broad.length > 2) {
		  fS = broad[2][0];
		  double beta = broad[3][0];
		  for (int i = 1; i < broad[0].length; i++) {
			  fS += broad[2][i] * MoreMath.pow(energy, i - 1);
			  beta += broad[3][i] * MoreMath.pow(energy, i - 1);
		  }
		  fS *= mhuDet;

		  if (beta > 0)
			  one_over_beta = 1.0 / beta;
		  else
			  one_over_beta = 0.0;

		  if (coreShellID != -1) {
			  if (transitionID.toUpperCase().startsWith("KL")) { // Kalpha
				  fT = broad[4][0];
				  for (int i = 1; i < broad[0].length; i++)
					  fT += broad[4][i] * MoreMath.pow(mhuDet, i - 1);
			  } else { //  "KM" -> Kbeta
				  fT = broad[5][0];
				  for (int i = 1; i < broad[0].length; i++)
					  fT += broad[5][i] * MoreMath.pow(mhuDet, i - 1);
			  }
		  }
	  } else {
	  	 fT = 0;
	  	 fS = 0;
		 one_over_beta = 1.0;
	  }

	  one_over_hwhm = 1.0 / hwhm;
	  double symPeakIntensity = 1.0 - fT - fS;
	  dgx = symPeakIntensity * (1.0 - eta) * Constants.sqrtln2pi * one_over_hwhm;
	  dcx = symPeakIntensity * eta * one_over_hwhm / Math.PI;
	  one_over_sigma = Constants.sqrt2ln2 * one_over_hwhm;
	  one_over_beta2 = one_over_beta * one_over_beta;
  }

  public double getIntensity(double x) {
		double intensity = 0;
    double dx1 = x - getEnergy();
    double dx = dx1;
	 dx *= one_over_hwhm;
	 dx *= dx;
//	 dx1 *= 0.001;
	 if (dx > 30.0)
		 intensity = getIntensity() * dcx / (1.0 + dx);
	 else
		 intensity = getIntensity() * (dcx / (1.0 + dx) + dgx * Math.exp(-Constants.LN2 * dx));

	 if (fT > 0)
	   intensity += getIntensity() * fT * one_over_beta * one_over_sigma * 0.5 / Math.exp(-0.5 * one_over_beta2) *
			 Math.exp(dx1 * one_over_beta * one_over_sigma) *
			 erfc(Constants.one_sqrt2 * (dx1 * one_over_sigma + one_over_beta));
	 if (fS > 0)
	   intensity += getIntensity() * fS * erfc(Constants.one_sqrt2 * dx1 * one_over_sigma) /
			   (2.0 * getEnergy());
	 return intensity;
	}

  public void multiplyIntensityBy(double atomsQuantity) {
    setIntensity(getIntensity() * atomsQuantity);
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
