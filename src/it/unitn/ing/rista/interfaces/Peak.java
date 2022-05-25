/*
 * @(#)Peak.java created 01/01/1997 Mesiano
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

package it.unitn.ing.rista.interfaces;

import it.unitn.ing.rista.diffr.*;

import java.io.PrintStream;

/**
 * The Peak is a class
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:08 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public interface Peak {
  //insert class definition here

  public double getIntensityAt(double x);

  public void setIntensity(double intensity);

  public double[] getPosition();

  public double getMeanPosition();

  public Reflection getReflex();

//  public void setCrystMstrainDistribution(double f[], double s[], int x[], int size);

  public void computePeak(DiffrDataFile diffrDataFile, double[] expfit,
                              Sample asample, Instrument ainstrument,
                              PrintStream out, boolean logOutput, double cutoff,
                              int computeTexture, int computeStrain,
                              int computeFhkl, boolean leBailExtraction, int[] minmaxindex,
                              boolean computeBroadening, boolean reverseX);

	public void computeFunctions(double[] x, double[] f, int[] minindex, int[] maxindex,
	                             double[][] intensity, double[] eta, double[] hwhm_i, double[][] position,
	                             double[] const1, double[] const2, double[] wave, boolean dspacingBase,
	                             boolean energyDispersive, boolean increasingX, double planar_asymmetry,
	                             double[] deff);

  public Phase getPhase();

  public boolean intensityExtractionAllowed();

  public boolean structureFactorExtractionAllowed();

  public boolean getdspacingBase();

  public double getMeanWavelength();

	public int getOrderPosition();

}
