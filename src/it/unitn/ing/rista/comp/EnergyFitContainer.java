/*
 * @(#)EnergyFitContainer.java created Mar 13, 2005 Riva Del Garda
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
package it.unitn.ing.rista.comp;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Constants;

import java.util.Vector;

/**
 * The EnergyFitContainer is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.3 $, $Date: 2006/01/19 14:45:53 $
 * @since JDK1.1
 */
public class EnergyFitContainer extends SpectrumFitContainer {

  Sample thesample;

  public EnergyFitContainer() {
  }

  public EnergyFitContainer(Sample asample) {
    thesample = asample;
    dataNumber = thesample.phasesNumber();
    dta = new double[dataNumber];
    double sumDataWeighted = 1.0;
/*    switch (thesample.getFilePar().getMinimizeQuantitySwitch()) {
      case 0:
        sumDataWeighted = 1.0;
        break;
      case 1:
        sumDataWeighted = 0.0;
        break;
    }*/
    wgt = new double[dataNumber];
    wgt2 = new double[dataNumber];
    for (int j = 0; j < dataNumber; j++) {
      wgt[j] = getWeight(j);
      wgt2[j] = wgt[j] * wgt[j];
    }
    checkFit();
  }

  private double getWeight(int j) {
    return 1.0; // thesample.getPhase(j).getActiveStructureModel().getEnergyWeight();
  }

  private double getFit(int j) {
    return thesample.getEnergy(j);
  }

  public void checkFit() {
    fit = new double[dataNumber];
    for (int j = 0; j < dataNumber; j++)
      fit[j] = getFit(j);
  }

  public double getWSS() {
    double WSS = 0.0;
    double diff;

    for (int i = 0; i < dataNumber; i++) {
      diff = (fit[i] - dta[i]) * wgt[i];
      WSS += diff * diff;
    }
    if (Constants.testing && !thesample.getFilePar().isOptimizing())
      System.out.println("WSS energy = " + WSS);
    return WSS;
  }

  public void checkDerivateFit() {
    derfit = new double[dataNumber];
    for (int j = 0; j < dataNumber; j++)
      derfit[j] = getFit(j);
  }

  public void checkDerivate2Fit() {
    der2fit = new double[dataNumber];
    for (int j = 0; j < dataNumber; j++)
      der2fit[j] = getFit(j);
  }

  public double[] computeDerivate(double dpar) {
    double sum = 0.0;
    double[] deriv = new double[0];
    if (thesample.energyModified) {
      deriv = new double[dataNumber];
      for (int i = 0; i < dataNumber; i++) {
        deriv[i] = (derfit[i] - fit[i]) / dpar;
        sum += Math.abs(deriv[i]);
      }
      if (sum == 0.0)
        deriv = new double[0];
      thesample.energyModified = false;
    }
    derfit = null;
    der2fit = null;
    return deriv;
  }

  public double[] computeDerivate2(double dpar) {
    double sum = 0.0;
    double[] deriv = new double[0];
    if (thesample.energyModified) {
      deriv = new double[dataNumber];
      for (int i = 0; i < dataNumber; i++) {
        deriv[i] = (derfit[i] - der2fit[i]) / dpar;
        sum += Math.abs(deriv[i]);
      }
      if (sum == 0.0)
        deriv = new double[0];
      thesample.energyModified = false;
    }
    derfit = null;
    der2fit = null;
    return deriv;
  }

  public void createDerivate(int numberParameters) {
    derivate = new Vector(numberParameters, 1);
    for (int i = 0; i < numberParameters; i++)
      derivate.addElement(new double[0]);
  }

  public void setDerivate(double[] deriv, int parameterNumber) {
    derivate.setElementAt(deriv, parameterNumber);
  }

  public double[] getDerivate(int parameterNumber) {
    return (double[]) derivate.elementAt(parameterNumber);
  }

  public void dispose() {
    fit = null;
    derfit = null;
    der2fit = null;
    dta = null;
    wgt = null;
    thesample = null;
    if (derivate != null && derivate.size() > 0)
      derivate.removeAllElements();
    derivate = null;
  }

}
