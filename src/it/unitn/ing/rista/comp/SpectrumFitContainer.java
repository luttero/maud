/*
 * @(#)SpectrumFitContainer.java created Apr 23, 2003 Berkeley
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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

import java.util.Vector;


/**
 * The SpectrumFitContainer is a class
 *
 * @version $Revision: 1.3 $, $Date: 2005/03/14 13:38:08 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class SpectrumFitContainer {
  public double[] fit = null;
  public double[] derfit = null;
  public double[] der2fit = null;
  public int dataNumber = 0;
  public double[] dta = null;
  public double[] wgt = null;
  public double[] wgt2 = null;
  private DiffrDataFile datafile = null;
  Vector derivate = null;

  public SpectrumFitContainer() {
  }

  public SpectrumFitContainer(DiffrDataFile adatafile, Sample asample) {
    datafile = adatafile;
    dataNumber = adatafile.getNumberOfData();
    dta = adatafile.getDataForStatistic();
    double sumDataWeighted = 1.0;
    switch (adatafile.getFilePar().getMinimizeQuantitySwitch()) {
      case 0:
        sumDataWeighted = 1.0;
        break;
      case 1:
        sumDataWeighted = Math.sqrt(adatafile.getDataWeightSum()) *
                Math.sqrt(asample.getNumberActiveDatafiles());
        break;
    }
    wgt = adatafile.getWeight();
    wgt2 = new double[dataNumber];
    for (int j = 0; j < dataNumber; j++) {
      wgt[j] = wgt[j] / sumDataWeighted;
      wgt2[j] = wgt[j] * wgt[j];
    }
    checkFit();
  }

  public void checkFit() {
    fit = datafile.getFitForStatistic();
    dta = datafile.getDataForStatistic();
    wgt = datafile.getWeight();
  }

  public double getWSS() {
    double WSS = 0.0;
    double diff;

    for (int i = 0; i < dataNumber; i++) {
      diff = (fit[i] - dta[i]) * wgt[i];
      WSS += diff * diff;
    }
    return WSS;
  }

  public void checkDerivateFit() {
    derfit = datafile.getFitForStatistic();
  }

  public void checkDerivate2Fit() {
    der2fit = datafile.getFitForStatistic();
  }

  public double[] computeDerivate(double dpar) {
    double sum = 0.0;
    double[] deriv = new double[0];
    if (datafile.spectrumModified) {
      deriv = new double[dataNumber];
      for (int i = 0; i < dataNumber; i++) {
        deriv[i] = (derfit[i] - fit[i]) / dpar;
//	      System.out.println(i+" "+derfit[i]+" "+fit[i]+" "+dpar+" "+deriv[i]);
        sum += Math.abs(deriv[i]);
      }
      if (sum == 0.0)
        deriv = new double[0];
      datafile.spectrumModified = false;
    }
    derfit = null;
    der2fit = null;
    return deriv;
  }

  public double[] computeDerivate2(double dpar) {
    double sum = 0.0;
    double[] deriv = new double[0];
    if (datafile.spectrumModified) {
      deriv = new double[dataNumber];
      for (int i = 0; i < dataNumber; i++) {
        deriv[i] = (derfit[i] - der2fit[i]) / dpar;
        sum += Math.abs(deriv[i]);
      }
      if (sum == 0.0)
        deriv = new double[0];
      datafile.spectrumModified = false;
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
    datafile = null;
    if (derivate != null && derivate.size() > 0)
      derivate.removeAllElements();
    derivate = null;
  }

}
