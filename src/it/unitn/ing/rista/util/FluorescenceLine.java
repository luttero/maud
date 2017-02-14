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
import it.unitn.ing.rista.chemistry.XRayDataSqLite;

/**
 * The FluorescenceLine is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Mar 9, 2009 11:20:32 AM $
 * @since JDK1.1
 */
public class FluorescenceLine {

	double dgx_dx = 1.0;
	double dcx_dx = 1.0;
	double eta_dx;
	double hwhm_dx;
	double one_over_hwhm_dx;
	double dgx_sx = 1.0;
	double dcx_sx = 1.0;
	double eta_sx;
	double hwhm_sx;
	double one_over_hwhm_sx;

	double energy;
	double intensity = 1.0;
	private double transitionProbability;
	int coreShellID = -1;
	double fluorescenceYield = 0;
	double coreShellEnergy = 0;
	public String transitionID = "";
	public int xrl_line_number = -9999;

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
		coreShellID = lineToCopy.coreShellID;
		transitionID = lineToCopy.transitionID;
		xrl_line_number = lineToCopy.xrl_line_number;
		transitionProbability = lineToCopy.transitionProbability;
		fluorescenceYield = lineToCopy.fluorescenceYield;
		coreShellEnergy = lineToCopy.coreShellEnergy;
		eta_dx = lineToCopy.eta_dx;
		hwhm_dx = lineToCopy.hwhm_dx;
		one_over_hwhm_dx = 1.0 / hwhm_dx;
		dgx_dx = (1.0 - eta_dx) * Constants.sqrtln2pi * one_over_hwhm_dx;
		dcx_dx = eta_dx * one_over_hwhm_dx / Math.PI;
		eta_sx = lineToCopy.eta_sx;
		hwhm_sx = lineToCopy.hwhm_sx;
		one_over_hwhm_sx = 1.0 / hwhm_sx;
		dgx_sx = (1.0 - eta_sx) * Constants.sqrtln2pi * one_over_hwhm_sx;
		dcx_sx = eta_sx * one_over_hwhm_sx / Math.PI;
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

  public void setShape(double hwhm_dx, double eta_dx, double hwhm_sx, double eta_sx) {
    this.hwhm_dx = hwhm_dx;
    this.eta_dx = eta_dx;
	  one_over_hwhm_dx = 1.0 / hwhm_dx;
	  dgx_dx = (1.0 - eta_dx) * Constants.sqrtln2pi * one_over_hwhm_dx;
	  dcx_dx = eta_dx * one_over_hwhm_dx / Math.PI;
	  this.hwhm_sx = hwhm_sx;
	  this.eta_sx = eta_sx;
	  one_over_hwhm_sx = 1.0 / hwhm_sx;
	  dgx_sx = (1.0 - eta_sx) * Constants.sqrtln2pi * one_over_hwhm_sx;
	  dcx_sx = eta_sx * one_over_hwhm_sx / Math.PI;
  }

  public double getIntensity(double x) {
    double dx = x - getEnergy();
	  if (dx <= 0) {
		  dx *= one_over_hwhm_sx;
		  dx *= dx;
		  if (dx > 30.0)
			  return getIntensity() * dcx_sx / (1.0 + dx);
		  else
			  return getIntensity() * (dcx_sx / (1.0 + dx) + dgx_sx * Math.exp(-Constants.LN2 * dx));
	  } else {
		  dx *= one_over_hwhm_dx;
		  dx *= dx;
		  if (dx > 30.0)
			  return getIntensity() * dcx_dx / (1.0 + dx);
		  else
			  return getIntensity() * (dcx_dx / (1.0 + dx) + dgx_dx * Math.exp(-Constants.LN2 * dx));
	  }
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
		System.out.println("Peak " + energy + " " + dcx_dx + " " + dcx_sx + " " + dgx_dx + " " + dgx_sx + " " + hwhm_dx + " " + hwhm_sx + " " + eta_dx + " " + eta_sx);
	}
}
