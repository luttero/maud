/*
 * @(#)IntensityExtLeBail.java created 04/3/1999 Caldonazzo lake
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.diffr.intext;

import java.lang.*;
import java.util.Vector;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.sfm.StructureFactorLeBailExtractor;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.interfaces.Peak;

/**
 *  The IntensityExtLeBailWt is a class
 *
 *
 * @version $Revision: 1.8 $, $Date: 2006/11/10 09:33:01 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class IntensityExtLeBailWt extends IntensityExtLeBail {

  boolean useBKG = false;

  public IntensityExtLeBailWt(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "LeBail wt";
    IDlabel = "LeBail wt";
    description = "select this to apply Le Bail method for intensity extraction";
  }

  public IntensityExtLeBailWt(XRDcat aobj) {
    this(aobj, "Weighted le Bail method");
  }

  public IntensityExtLeBailWt() {
    identifier = "Disabled LeBail wt";
    IDlabel = "LeBail wt";
    description = "select this to apply a weighted Le Bail method for intensity extraction";
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);

    useBKG = getUseBkgB();
  }

  public void extractIntensities(Sample asample) {

//		System.out.println("Extracting factors by Le Bail: " + getDataFile().toXRDcatString());

    Vector<Peak> tpeaklist;
    DiffrDataFile datafile = getDataFile();
    DataFileSet datafileset = datafile.getDataFileSet();
	  Diffraction diffraction = datafileset.getDiffraction();
    Vector<Peak> fullpeaklist = datafileset.getPeakList();
    int numberofpeaks = datafileset.getNumberofPeaks();

    int datanumber = datafile.getTotalNumberOfData();
    double[] expfit = new double[datanumber];
    double[] fit = new double[datanumber];

    boolean useHKL = getUseHklB();

    double delta = getDeltaMax();
    if (delta == 0.0)
      delta = 1.0E-9;
    int ndelta;

    int startingindex = datafile.startingindex;
    int finalindex = datafile.finalindex;

    double minBkg = datafile.getBkgFit(startingindex);
    double bkg;
    for (int j = startingindex; j < finalindex; j++) {
//      fit[j] = datafile.getFit(j);
      bkg = datafile.getBkgFit(j);
      if (bkg < minBkg)
        minBkg = bkg;
    }
    if (minBkg < 0.0)
      minBkg = - minBkg;
    else
      minBkg = 0.0;

    int minmaxindex[] = new int[2];
    minmaxindex[0] = startingindex;
    minmaxindex[1] = finalindex;

    double rangefactor = getRangeFactorD();

    boolean convergence = false;

    double diff[] = new double[numberofpeaks];
    int superOrder[] = new int[numberofpeaks];

    superOrder[0] = -1;
    for (int i = 0, n = 0; i < numberofpeaks; i++, n++) {
      superOrder[n] = -1;
      if (useHKL)
        for (int j = 0; j < i; j++)
          if (checkOrder(fullpeaklist.elementAt(i), fullpeaklist.elementAt(j)))
            superOrder[n] = j;
    }

    int maxiter = getIterationMax();
    int iteration = 0;

    while (!convergence && iteration++ < maxiter) {
      for (int j = startingindex; j < finalindex; j++)
        fit[j] = 0.0f;
	    diffraction.computeReflectionIntensity(asample, fullpeaklist, true, fit, Constants.ENTIRE_RANGE,
              Constants.EXPERIMENTAL, Constants.COMPUTED, Constants.COMPUTED, false, null, datafile);
	    diffraction.computeasymmetry(asample, datafile, fit, startingindex, finalindex);
      datafile.postComputation(asample, fit);
      for (int j = startingindex; j < finalindex; j++)
        datafile.setPhasesFit(j, fit[j]);
      convergence = true;
      int i = 0, n = 0;
      int numberpeaktouse;
      while (i < numberofpeaks) {
        if (delta >= 0 && superOrder[n] < 0) {
          double pos = fullpeaklist.elementAt(i).getMeanPosition(); // to be corrected
          ndelta = 1;
          while (i + ndelta < numberofpeaks &&
                  (superOrder[n + ndelta] < 0 &&
                  Math.abs((pos - fullpeaklist.elementAt(i + ndelta).getMeanPosition()) // to be corrected
                  / pos) < delta))
            ndelta++;
        } else
          ndelta = 1;
        numberpeaktouse = ndelta;
        tpeaklist = StructureFactorLeBailExtractor.vectorcopy(fullpeaklist, i, numberpeaktouse);
        if (datafile.checkPeakInsideRange(tpeaklist.elementAt(0).getPhase(), tpeaklist.elementAt(0).getOrderPosition(),
		        rangefactor)
		        && tpeaklist.elementAt(0).intensityExtractionAllowed()) {
          double lebailfactor = 0.0;
          if (superOrder[n] < 0) {
            for (int j = minmaxindex[0]; j < minmaxindex[1]; j++)
              expfit[j] = 0.0f;
            minmaxindex = diffraction.computeReflectionIntensity(asample, tpeaklist, false,
                    expfit, rangefactor, Constants.UNITARY,
                    Constants.COMPUTED, Constants.COMPUTED, true, null, datafile);
	          diffraction.computeasymmetry(asample, datafile, expfit, minmaxindex[0], minmaxindex[1]); // todo: interpolation and experimental background
            double expfitnorm = 0.0;
						if (useBKG)
            for (int k = minmaxindex[0]; k < minmaxindex[1]; k++) {
              double wgt = datafile.getWeight(k);
              lebailfactor += expfit[k] * Math.abs((datafile.getYData(k) + minBkg) * wgt / (datafile.getFit(k) + minBkg));
              expfitnorm += expfit[k] * wgt;
            }
						else
							for (int k = minmaxindex[0]; k < minmaxindex[1]; k++) {
								bkg = datafile.getBkgFit(k);
								double wgt = datafile.getWeight(k);
								lebailfactor += expfit[k] * Math.abs(Math.abs(datafile.getYData(k) - bkg) * wgt
															/ (datafile.getFit(k) - bkg));
								expfitnorm += expfit[k] * wgt;
							}
            if (expfitnorm <= 0.0)
              lebailfactor = 0.0001;
            else
              lebailfactor /= expfitnorm;
            if (lebailfactor < 0.0)
              lebailfactor = 0.0001;
            for (int ij = 0; ij < ndelta; ij++) {
              double oldfactor = datafile.getExpTextureFactor(tpeaklist.elementAt(ij).getPhase(),
		              tpeaklist.elementAt(ij))[0][0];
              if (Double.isNaN(oldfactor))
                oldfactor = 1.0;
              double newlebailfactor = lebailfactor * oldfactor;
//						System.out.println(ij);
//						System.out.println(oldfactor);
//						System.out.println(lebailfactor);
//						System.out.println(newlebailfactor);
              if (oldfactor == 0.0)
                diff[n + ij] = Math.abs(newlebailfactor);
              else
                diff[n + ij] = Math.abs((newlebailfactor - oldfactor) / oldfactor);
              if (diff[n + ij] > getErrorMax())
                convergence = false;
              datafile.setExpTextureFactor(tpeaklist.elementAt(ij).getPhase(), tpeaklist.elementAt(ij), 0, newlebailfactor);
            }
          } else {
            lebailfactor = datafile.getExpTextureFactor(fullpeaklist.elementAt(superOrder[n]).getPhase(),
		            fullpeaklist.elementAt(superOrder[n]))[0][0];
            if (Double.isNaN(lebailfactor))
              lebailfactor = 1.0;
            numberpeaktouse = 1;
            ndelta = 1;
            for (int k = 0; k < numberpeaktouse; k++)
              datafile.setExpTextureFactor(tpeaklist.elementAt(k).getPhase(), tpeaklist.elementAt(k), 0, lebailfactor);
          }
        }
        i += numberpeaktouse;
        n += ndelta;
      }
    } // convergence or maxiteration reached

//		datafile.computeReflectionIntensity();
//		datafile.computeasymmetry();
//    notifyObjectChanged(this);
  }

}
