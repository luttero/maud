/*
 * @(#)StructureFactorLeBailExtractor.java created 28/06/2001 Casalino
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.sfm;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.interfaces.Peak;

import java.awt.*;
import java.util.Vector;
import javax.swing.*;
import javax.swing.event.*;

/**
 * The StructureFactorLeBailExtractor is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.15 $, $Date: 2006/12/04 14:30:15 $
 * @since JDK1.1
 */

public class StructureFactorLeBailExtractor extends StructureFactorExtractor {
  public static String[] diclistc = {"_riet_lebail_iteration_max", "_riet_lebail_error_max",
      "_riet_lebail_range_factor", "_riet_lebail_use_bkg",
      "_riet_lebail_summation_delta", "_riet_lebail_use_previous_factors"};
  public static String[] diclistcrm = {"_riet_lebail_iteration_max", "_riet_lebail_error_max",
      "_riet_lebail_range_factor", "_riet_lebail_use_bkg",
      "_riet_lebail_summation_delta", "_riet_lebail_use_previous_factors"};
  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean useBKG = false;

  public StructureFactorLeBailExtractor(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Le Bail";
    IDlabel = "Le Bail";
    description = "select this to apply Le Bail method for structure factor extraction";
  }

  public StructureFactorLeBailExtractor(XRDcat aobj) {
    this(aobj, "Le Bail method");
  }

  public StructureFactorLeBailExtractor() {
    identifier = "Le Bail";
    IDlabel = "Le Bail";
    description = "select this to apply Le Bail method for structure factor extraction";
  }

  public void initConstant() {
    Nstring = 6;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
	  System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
	  System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
	  System.arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
  }

  public void initParameters() {
    super.initParameters();
    setIterationMax(5);
    setErrorMax(0.005);
    setRangeFactor(0.05);
    setUseBkg(true);
    setDeltaMax(0.0001);
    setUseLastFactors(true);
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);

    useBKG = getUseBkgB();
  }

  public String getIterationMaximum() {
    return stringField[0];
  }

  public int getIterationMax() {
    return Integer.valueOf(getIterationMaximum());
  }

  public void setIterationMax(String value) {
    stringField[0] = value;
  }

  public void setIterationMax(int value) {
    setIterationMax(Integer.toString(value));
  }

  public String getErrorMaximum() {
    return stringField[1];
  }

  public double getErrorMax() {
    return Double.valueOf(getErrorMaximum());
  }

  public void setErrorMax(String value) {
    stringField[1] = value;
  }

  public void setErrorMax(double value) {
    setErrorMax(Double.toString(value));
  }

  public String getDeltaMaximum() {
    return stringField[4];
  }

  public double getDeltaMax() {
    return Double.valueOf(getDeltaMaximum());
  }

  public void setDeltaMax(String value) {
    stringField[4] = value;
  }

  public void setDeltaMax(double value) {
    setDeltaMax(Double.toString(value));
  }

  public String getRangeFactor() {
    return stringField[2];
  }

  public double getRangeFactorD() {
    return Double.valueOf(getRangeFactor());
  }

  public void setRangeFactor(String value) {
    stringField[2] = value;
  }

  public void setRangeFactor(double value) {
    setRangeFactor(Double.toString(value));
  }

  public String getUseBkg() {
    return stringField[3];
  }

  public boolean getUseBkgB() {
    return getUseBkg().equalsIgnoreCase("true");
  }

  public void setUseBkg(boolean value) {
    if (value)
      setUseBkg("true");
    else
      setUseBkg("false");
  }

  public void setUseBkg(String value) {
    stringField[3] = value;
  }

  public String getUseLastFactors() {
    return stringField[5];
  }

  public boolean getUseLastFactorsB() {
    return getUseLastFactors().equalsIgnoreCase("true");
  }

  public void setUseLastFactors(boolean value) {
    if (value)
      setUseLastFactors("true");
    else
      setUseLastFactors("false");
  }

  public void setUseLastFactors(String value) {
    stringField[5] = value;
  }

  /*
Le Bail algorithm for PF extraction

The Le Bail method is a iterative scheme to extract the peak intensities from a powder spectrum.
The principal formula as used in our procedure is the following (without considering the background
for simplicity) for the n-th iteration:

PF (h ,y  ) = PF (h ,y  ) * SUM (Yobs(y ,d ) S(d  ,y  ;d  -d )/SUM(S(d  ,y ;d  -d )PF   (h  ,y )I  (y )))
n  k  j      n-1 k  j      i         j  i     h   j   h   i   r     h   j  h   i   n-1  r   j  h   j
k       k             r      r                   r

where all the symbols used are defined above (the D-spectrum model) except that in the PF symbol
appears the subscript n or n-1. In the first iteration, we start with all PF's equal to 1.
The PF's of peaks of the same family are imposed equals for all the lines.

  */

  public void extractStructureFactors(Sample asample) {
    if (!getFilePar().isStructureFactorExtractionPermitted())
      return;

    double diff, diffb;
    int numberdataset = asample.activeDatasetsNumber();
    Constants.STARTING_STRUCTURE_FACTOR = MaudPreferences.getDouble("structure_factors.default_value",
        Constants.STARTING_STRUCTURE_FACTOR);
    Constants.MINIMUM_STRUCTURE_FACTOR = MaudPreferences.getDouble("structure_factors.minimum_value_for_extraction",
        Constants.MINIMUM_STRUCTURE_FACTOR);
    double delta = getDeltaMax();
//    if (delta == 0.0)
//      delta = 1.0E-9;
    int maxiter = getIterationMax();
    boolean useLastFactors = getUseLastFactorsB();
    double rangefactor = getRangeFactorD();

    ProgressFrame prF = null;
    if (!Constants.textonly && Constants.showProgressFrame) {
      try {
        prF = new ProgressFrame(numberdataset);
        printf("Extracting structure factors from datafiles using " + toXRDcatString(), prF);
      } catch (NullPointerException npe) {
        System.out.println("Not able to create frame, MacOSX display sleep bug?");
      }
    }

	  Phase phase = (Phase) getParent();
	  int numberReflections = phase.gethklNumber();
//	  System.out.println(phase.getPhaseName() + ", number of peaks: " + phase.gethklNumber());

	  for (int di = 0; di < numberdataset; di++) {
      DataFileSet dataset = asample.getActiveDataSet(di);
		  Diffraction diffraction = dataset.getDiffraction();
      int datafilenumber = dataset.activedatafilesnumber();
      Vector<Peak> fullpeaklist = dataset.getPeakList();
      int numberofpeaks = dataset.getNumberofPeaks();
//		  System.out.println("Number of peaks: " + numberofpeaks + " " + fullpeaklist.size());
      double[] lastfactors = new double[numberofpeaks];
      double[] newfactors = new double[numberofpeaks];
      double[] weightfactors = new double[numberofpeaks];
		  int[] reflectionListIndices = new int[numberReflections];
//      double[] esdfactors = new double[numberofpeaks];

		  double[][] datasetSFactors = dataset.getStructureFactors(phase);

      if (!useLastFactors) {
        for (int np = 0; np < numberofpeaks; np++) {
	        Phase actualPhase = fullpeaklist.elementAt(np).getPhase();
          if (actualPhase == phase) {
            lastfactors[np] = Constants.STARTING_STRUCTURE_FACTOR * Constants.STARTING_STRUCTURE_FACTOR;
            newfactors[np] = lastfactors[np];
	          reflectionListIndices[fullpeaklist.elementAt(np).getOrderPosition()] = np;
          } else {
            lastfactors[np] = dataset.getStructureFactors(actualPhase)[1][fullpeaklist.elementAt(np).getOrderPosition()];
            newfactors[np] = lastfactors[np];
          }
          weightfactors[np] = 0.0;
        }
      } else {
//	      for (int np = 0; np < numberofpeaks; np++)
//		      System.out.println(np + " " + fullpeaklist.elementAt(np).getPhase().getPhaseName() + " " +
//				      fullpeaklist.elementAt(np).getOrderPosition());
        for (int np = 0; np < numberofpeaks; np++) {
	        Phase actualPhase = fullpeaklist.elementAt(np).getPhase();
          if (actualPhase == phase) {
            lastfactors[np] = datasetSFactors[1][fullpeaklist.elementAt(np).getOrderPosition()];
            if (lastfactors[np] <= 1.0E-9)
              lastfactors[np] = Constants.MINIMUM_STRUCTURE_FACTOR * Constants.MINIMUM_STRUCTURE_FACTOR; // just we don't want to start from 0.0 otherwise will remain 0.0
            newfactors[np] = lastfactors[np];
	          reflectionListIndices[fullpeaklist.elementAt(np).getOrderPosition()] = np;
          } else {
            lastfactors[np] = dataset.getStructureFactors(actualPhase)[1][fullpeaklist.elementAt(np).getOrderPosition()];
            newfactors[np] = lastfactors[np];
          }
          weightfactors[np] = 0.0;
        }
      }
		  int maxNumberHKL = 0;
		  for (int np = 0; np < numberofpeaks; np++)
			  if (fullpeaklist.elementAt(np).getPhase() == phase) {
			    datasetSFactors[0][fullpeaklist.elementAt(np).getOrderPosition()] = lastfactors[np];
				  if (fullpeaklist.elementAt(np).getOrderPosition() > maxNumberHKL)
					  maxNumberHKL = fullpeaklist.elementAt(np).getOrderPosition();
			  }
		  numberReflections = maxNumberHKL;
      int[] numberpeaktouse = new int[numberofpeaks];
      double[] newlebailfactor = new double[numberofpeaks];
      double[] newexpfitnorm = new double[numberofpeaks];
      for (int in = 0; in < numberofpeaks; in++)
        numberpeaktouse[in] = 1;

      if (delta > 0) {
	      for (int in = 0; in < numberReflections;) {
          double pos = fullpeaklist.elementAt(reflectionListIndices[in]).getMeanPosition(); // to be corrected
		      int next = in + numberpeaktouse[reflectionListIndices[in]];
//		      System.out.println(in + " " + pos);
          while (next < numberReflections &&
              Math.abs((pos - fullpeaklist.elementAt(reflectionListIndices[next]).getMeanPosition()) / pos) < delta) {
//	          System.out.println(fullpeaklist.elementAt(reflectionListIndices[next]).getMeanPosition() + " " +
//			          Math.abs((pos - fullpeaklist.elementAt(reflectionListIndices[next]).getMeanPosition()) / pos) + " < " + delta);
            numberpeaktouse[reflectionListIndices[in]]++;
	          next++;
          }
		      in = next;
	      }
      }

      boolean convergence = false;
      int iteration = 0;
      while (!convergence && iteration++ < maxiter) {
        for (int in = 0; in < numberofpeaks; in++) {
          newlebailfactor[in] = 0;
          newexpfitnorm[in] = 0;
        }
        for (int ks = 0; ks < datafilenumber; ks++) {
        	DataFileSet datafileset = asample.getActiveDataSet(di);
          DiffrDataFile datafile = datafileset.getActiveDataFile(ks);
          int datanumber = datafile.getTotalNumberOfData();
          double[] expfit = new double[datanumber];
          double[] fit = new double[datanumber];
          int startingindex = datafile.startingindex;
          int finalindex = datafile.finalindex;
          double minBkg = 0.0;
          int minmaxindex[] = new int[2];
          minmaxindex[0] = startingindex;
          minmaxindex[1] = finalindex;
          boolean computeBroadening = true;
          int datafileIndex = datafile.getIndex();

          for (int j = startingindex; j < finalindex; j++)
            if (datafile.getBkgFit(j) < 0.0 && minBkg > datafile.getBkgFit(j))
              minBkg = datafile.getBkgFit(j);
          minBkg = -minBkg;
// System.out.println("minBkg " + minBkg);
          for (int j = startingindex; j < finalindex; j++)
            fit[j] = 0.0f;
	        diffraction.computeReflectionIntensity(asample, fullpeaklist, computeBroadening, fit,
              Constants.ENTIRE_RANGE, Constants.COMPUTED, Constants.COMPUTED,
              Constants.EXPERIMENTAL, false, null, datafile);
          computeBroadening = false;
	        diffraction.computeasymmetry(asample, datafile, fit, startingindex, finalindex);
          datafile.postComputation(asample, fit);
          for (int j = startingindex; j < finalindex; j++)
            datafile.setPhasesFit(j, fit[j]);
//	        datafile.resetBkg();
//	        datafile.computeBackground(startingindex, finalindex);

          convergence = true;
          int i = 0;
          while (i < numberReflections) {
	          Vector<Peak> tpeaklist = new Vector<Peak>(numberpeaktouse[reflectionListIndices[i]]);
	          for (int j = 0; j < numberpeaktouse[reflectionListIndices[i]]; j++)
              tpeaklist.add(fullpeaklist.elementAt(reflectionListIndices[i + j]));
            if (datafile.checkPeakInsideRange(phase, tpeaklist.elementAt(0).getOrderPosition(), rangefactor)) {
              for (int j = startingindex; j < finalindex; j++)
                expfit[j] = 0.0f;
              minmaxindex = diffraction.computeReflectionIntensity(asample, tpeaklist, computeBroadening,
                  expfit, rangefactor,
                  Constants.COMPUTED, Constants.COMPUTED,
                  Constants.UNITARY, true, null, datafile);
	            diffraction.computeasymmetry(asample, datafile, expfit, minmaxindex[0], minmaxindex[1]); // todo: interpolation and experimental background
              double expfitnorm = 0.0;
              double lebailfactor = 0.0;
              if (useBKG)
                for (int k = minmaxindex[0]; k < minmaxindex[1]; k++) {
	       //         System.out.println("k " + k + " " + datafile.getXData(k) + " " + datafile.getYData(k) + " " + datafile.getFit(k));
                  lebailfactor += Math.abs(expfit[k] * (datafile.getYData(k) + minBkg) / (datafile.getFit(k) + minBkg));
                  expfitnorm += Math.abs(expfit[k]);
                }
              else
                for (int k = minmaxindex[0]; k < minmaxindex[1]; k++) {
                  double bkg = datafile.getBkgFit(k);
                  diffb = datafile.getFit(k) - bkg;
                  if (diffb > 1.0E-9) {
                    lebailfactor += Math.abs(expfit[k] * (datafile.getYData(k) - bkg) / diffb);
                    expfitnorm += Math.abs(expfit[k]);
                  }
                }
              if (expfitnorm <= 0.0)
                lebailfactor = 0.0;
              else
                lebailfactor /= expfitnorm;
	            for (int ij = 0; ij < numberpeaktouse[reflectionListIndices[i]]; ij++) {
		            for (int ik = 0; ik < datafile.positionsPerPattern; ik++) {
//			            System.out.println("Le Bail: " + phase.getPhaseName() + " " + i + " " +
//					            numberpeaktouse[reflectionListIndices[i]] + " " + ij + " " + reflectionListIndices[i + ij]);
			            double textureFactor = datafile.getTextureFactors(phase, i + ij)[ik];
			            if (Double.isNaN(textureFactor))
				            textureFactor = datafile.getExperimentalTextureFactors(phase, i + ij)[ik];
			            if (Double.isNaN(textureFactor))
				            textureFactor = 1.0;
			            double sqrtTextureFactor = textureFactor; // * datafile.getShapeAbsFactors(phase,
					           // fullpeaklist.elementAt(i + ij).getOrderPosition())[ik];
			            if (sqrtTextureFactor > 0.0) {
				            sqrtTextureFactor = MoreMath.sqrt_or_zero(sqrtTextureFactor);
				            newlebailfactor[reflectionListIndices[i + ij]] += lebailfactor * sqrtTextureFactor;
				            newexpfitnorm[reflectionListIndices[i + ij]] += sqrtTextureFactor;
			            }
		            }
              }
            }
            i += numberpeaktouse[reflectionListIndices[i]];
          } // numberofpeaks
        } // datafilenumber
        int i = 0;
        while (i < numberReflections) {
        for (int ij = 0; ij < numberpeaktouse[reflectionListIndices[i]]; ij++) {
            double oldfactor = datasetSFactors[0][i + ij];
            double newlebailf = oldfactor;
            if (newexpfitnorm[reflectionListIndices[i + ij]] > 0)
              newlebailf = newlebailfactor[reflectionListIndices[i + ij]] / newexpfitnorm[reflectionListIndices[i + ij]] * oldfactor;
            if (oldfactor == 0.0)
              diff = 0.0;
            else
              diff = Math.abs((newlebailf - oldfactor) / oldfactor);
            if (diff > getErrorMax())
              convergence = false;
//	          System.out.println("Phase " + phase.getPhaseName() + ", new factor(" + (i + ij) + "): " + newlebailf + ", old factor: " + oldfactor);
	          datasetSFactors[0][i + ij] = newlebailf;
	          datasetSFactors[2][i + ij] = diff * Math.abs(oldfactor);
        }
          i += numberpeaktouse[reflectionListIndices[i]];
        }
      } // convergence or maxiteration reached

      if (prF != null)
        prF.increaseProgressBarValue();
    }  // dataset loop
    if (prF != null) {
      prF.setVisible(false);
      prF.dispose();
    }
  }

	static public Vector<Peak> vectorcopy(Vector<Peak> fullpeaklist, int start, int total) {
		Vector<Peak> tpeaklist = new Vector<Peak>(total, 10);
		tpeaklist.addAll(fullpeaklist.subList(start, start + total));
		return tpeaklist;
	}

/*  public void extractStructureFactorsOld(Sample asample) {

    double diff, diffb;

    if (!getFilePar().isStructureFactorExtractionPermitted())
      return;

	  Phase phase = (Phase) getParent();

    Constants.STARTING_STRUCTURE_FACTOR = MaudPreferences.getDouble("structure_factors.default_value",
        Constants.STARTING_STRUCTURE_FACTOR);
    Constants.MINIMUM_STRUCTURE_FACTOR = MaudPreferences.getDouble("structure_factors.minimum_value_for_extraction",
        Constants.MINIMUM_STRUCTURE_FACTOR);
//		System.out.println("Extracting factors by Le Bail: " + getDataFile().toXRDcatString());

    Peak tpeaklist[];

    int numberdataset = asample.activeDatasetsNumber();

    int totdatafiles = 0;
    for (int i = 0; i < numberdataset; i++)
      totdatafiles += asample.getActiveDataSet(i).activedatafilesnumber();

    double delta = getDeltaMax();
    if (delta == 0.0)
      delta = 1.0E-9;
    int ndelta;
    int maxiter = getIterationMax();

    ProgressFrame prF = null;
    if (!Constants.textonly && Constants.showProgressFrame)
      try {
        prF = new ProgressFrame(totdatafiles);
      } catch (NullPointerException npe) {
        System.out.println("Not able to create frame, MacOSX display sleep bug?");
      }
    if (prF != null)
      printf("Extracting structure factors from datafiles using " + toXRDcatString(), prF);

    boolean useLastFactors = getUseLastFactorsB();

    for (int di = 0; di < numberdataset; di++) {
      int datasetIndex = asample.getActiveDataSet(di).getIndex();
      int datafilenumber = asample.getActiveDataSet(di).activedatafilesnumber();
      Peak fullpeaklist[] = asample.getActiveDataSet(di).getPeakList();
      int numberofpeaks = asample.getActiveDataSet(di).getNumberofPeaks();

      double[] lastfactors = new double[numberofpeaks];
      double[] newfactors = new double[numberofpeaks];
      double[] weightfactors = new double[numberofpeaks];
      double[] esdfactors = new double[numberofpeaks];

      if (!useLastFactors) {
        for (int np = 0; np < numberofpeaks; np++) {
          if (fullpeaklist[np].getReflex().getParent() == getParent()) {
            lastfactors[np] = Constants.STARTING_STRUCTURE_FACTOR * Constants.STARTING_STRUCTURE_FACTOR;
            newfactors[np] = lastfactors[np];
//            System.out.println("ext 1:" + newfactors[np]);
          } else {
             lastfactors[np] = fullpeaklist[np].getReflex().getStructureFactor(datasetIndex);
            newfactors[np] = lastfactors[np];
//            System.out.println("ext 2:" + newfactors[np]);
          }
          weightfactors[np] = 0.0;
        }
      } else
        for (int np = 0; np < numberofpeaks; np++) {
           if (fullpeaklist[np].getReflex().getParent() == getParent()) {
            lastfactors[np] = fullpeaklist[np].getReflex().getStructureFactor(datasetIndex);
            if (lastfactors[np] <= 1.0E-9)
              lastfactors[np] = Constants.MINIMUM_STRUCTURE_FACTOR * Constants.MINIMUM_STRUCTURE_FACTOR; // just we don't want to start from 0.0 otherwise will remain 0.0
            newfactors[np] = lastfactors[np];
//           System.out.println("ext 3:" + newfactors[np]);
          } else {
            lastfactors[np] = fullpeaklist[np].getReflex().getStructureFactor(datasetIndex);
            newfactors[np] = lastfactors[np];
//            System.out.println("ext 4:" + newfactors[np]);
          }
          weightfactors[np] = 0.0;
        }

//      boolean computed = false;
      for (int ks = 0; ks < datafilenumber; ks++) {
        DiffrDataFile datafile = asample.getActiveDataSet(di).getActiveDataFile(ks);
        int datanumber = datafile.getTotalNumberOfData();
        double[] expfit = new double[datanumber];
        double[] fit = new double[datanumber];
//        if (adatafile[k].computeExpTextureFactor(asample))
//          computed = true;

        for (int np = 0; np < numberofpeaks; np++) {
          if (fullpeaklist[np].getReflex().getParent() == getParent())
             fullpeaklist[np].getReflex().setExpStructureFactor(datasetIndex, lastfactors[np]);
//          else
//            System.out.println(lastfactors[np] + " " + fullpeaklist[np].getReflex().getExpStructureFactor(datasetIndex));

        }

        int startingindex = datafile.startingindex;
        int finalindex = datafile.finalindex;

        double minBkg = 0.0;
        for (int j = startingindex; j < finalindex; j++)
          if (datafile.getBkgFit(j) < 0.0 && minBkg > datafile.getBkgFit(j))
            minBkg = datafile.getBkgFit(j);
        minBkg = -minBkg;

        int minmaxindex[] = new int[2];
        minmaxindex[0] = startingindex;
        minmaxindex[1] = finalindex;

        double rangefactor = getRangeFactorD();

        boolean convergence = false;

        int iteration = 0;

        boolean computeBroadening = true;
        while (!convergence && iteration++ < maxiter) {
          for (int j = startingindex; j < finalindex; j++)
            fit[j] = 0.0f;
          datafile.computeReflectionIntensity(asample, fullpeaklist, computeBroadening, fit,
              Constants.ENTIRE_RANGE, Constants.COMPUTED, Constants.COMPUTED,
              Constants.EXPERIMENTAL, false, null);
          computeBroadening = false;
          datafile.computeasymmetry(asample, fit);
          datafile.postComputation(asample, fit);
          for (int j = startingindex; j < finalindex; j++)
            datafile.setPhasesFit(j, fit[j]);

          convergence = true;
          int i = 0, n = 0;
          int numberpeaktouse;
          double[] allLeBailFactors = new double[numberofpeaks];
          while (i < numberofpeaks) {
            if (delta >= 0) {
              double pos = fullpeaklist[i].getMeanPosition(); // to be corrected
              ndelta = 1;
              while (i + ndelta < numberofpeaks &&
                  Math.abs((pos - fullpeaklist[i + ndelta].getMeanPosition()) // to be corrected
                      / pos) < delta) {
                if (fullpeaklist[i + ndelta].getReflex().getParent() == getParent())
                  break;
                ndelta++;
              }
            } else
              ndelta = 1;
            numberpeaktouse = ndelta;
            tpeaklist = new Peak[numberpeaktouse];
            System.arraycopy(fullpeaklist, i, tpeaklist, 0, numberpeaktouse);
            if (datafile.checkPeakInsideRange(phase, tpeaklist[0].getOrderPosition()) &&
                tpeaklist[0].getReflex().getParent() == getParent()) {
              double lebailfactor = 0.0;
              for (int j = startingindex; j < finalindex; j++)
                expfit[j] = 0.0f;
              minmaxindex = datafile.computeReflectionIntensity(asample, tpeaklist, computeBroadening,
                  expfit, rangefactor,
                  Constants.COMPUTED, Constants.COMPUTED,
                  Constants.UNITARY, true, null);
              datafile.computeasymmetryandAddbkg(asample, expfit, minmaxindex[0], minmaxindex[1]);
              double expfitnorm = 0.0;
              if (useBKG)
                for (int k = minmaxindex[0]; k < minmaxindex[1]; k++) {
                  lebailfactor += Math.abs(expfit[k] * (datafile.getYData(k) + minBkg) / (datafile.getFit(k) + minBkg));
                  expfitnorm += Math.abs(expfit[k]);
                }
              else
                for (int k = minmaxindex[0]; k < minmaxindex[1]; k++) {
                  double bkg = datafile.getBkgFit(k);
                  diffb = datafile.getFit(k) - bkg;
                  if (diffb > 1.0E-6) {
                    lebailfactor += Math.abs(expfit[k] * (datafile.getYData(k) - bkg)
                        / diffb);
                    expfitnorm += Math.abs(expfit[k]);
                  }
                }
              if (expfitnorm <= 0.0)
                lebailfactor = 0.0001;
              else
                lebailfactor /= expfitnorm;
              if (lebailfactor < 0.0)
                lebailfactor = 0.0001;
              for (int ij = 0; ij < ndelta; ij++) {
                if (fullpeaklist[i+ij].getReflex().getParent() == getParent()) {
                  double oldfactor = fullpeaklist[i+ij].getReflex().
                      getExpStructureFactor(datasetIndex);

                  double newlebailfactor = lebailfactor * oldfactor;
//						System.out.println(i + " " + oldfactor + " " + lebailfactor + " " + newlebailfactor);
                  if (oldfactor == 0.0)
                    diff = Math.abs(newlebailfactor);
                  else
                    diff = Math.abs((newlebailfactor - oldfactor) / oldfactor);
                  if (diff > getErrorMax())
                    convergence = false;
                  allLeBailFactors[i+ij] = newlebailfactor;
//                  fullpeaklist[i+ij].getReflex().setExpStructureFactor(
//                      datasetIndex, newlebailfactor);
                  fullpeaklist[i+ij].getReflex().setEsdStructureFactor(
                      datasetIndex, diff * Math.abs(oldfactor));
                }
              }
            }
            i += numberpeaktouse;
            n += ndelta;
          }
          for (i = 0; i < numberofpeaks; i++)
            if (fullpeaklist[i].getReflex().getParent() == getParent())
              fullpeaklist[i].getReflex().setExpStructureFactor(
                      datasetIndex, allLeBailFactors[i]);

        } // convergence or maxiteration reached

        int datafileIndex = datafile.getIndex();
        for (int np = 0; np < numberofpeaks; np++) {
          if (fullpeaklist[np].getReflex().getParent() == getParent()) {
            double newlebailf = fullpeaklist[np].getReflex().getExpStructureFactor(datasetIndex);
            double esdlebailf = fullpeaklist[np].getReflex().getEsdStructureFactor(datasetIndex);
            double textureFactor = fullpeaklist[np].getReflex().getTextureFactor(datafileIndex);
            if (Double.isNaN(textureFactor))
              textureFactor = fullpeaklist[np].getReflex().getExpTextureFactor(datafileIndex);
            if (Double.isNaN(textureFactor))
              textureFactor = 0.0;
            double sqrtTextureFactor = textureFactor * fullpeaklist[np].getReflex().getShapeAbsFactor(
                datafileIndex);
            if (sqrtTextureFactor > 0.0) {
              sqrtTextureFactor = MoreMath.sqrt_or_zero(sqrtTextureFactor);
              newfactors[np] = (newfactors[np] * weightfactors[np] + newlebailf * sqrtTextureFactor) /
                  (weightfactors[np] + sqrtTextureFactor);
              esdfactors[np] = (esdfactors[np] * weightfactors[np] + esdlebailf * sqrtTextureFactor) /
                  (weightfactors[np] + sqrtTextureFactor);
              weightfactors[np] += sqrtTextureFactor;
            }
          }
        }
        if (prF != null)
          prF.increaseProgressBarValue();
      }
      for (int np = 0; np < numberofpeaks; np++) {
        if (fullpeaklist[np].getReflex().getParent() == getParent()) {
          fullpeaklist[np].getReflex().setExpStructureFactor(datasetIndex, newfactors[np]);
          fullpeaklist[np].getReflex().setEsdStructureFactor(datasetIndex, esdfactors[np]);
//          System.out.println("fxt :" + newfactors[np]);
        } else {
//          System.out.println("f :" + newfactors[np]);
        }
      }
    }
    if (prF != null) {
      prF.setVisible(false);
      prF.dispose();
    }
  }*/

  public JOptionsDialog getOptionsDialog(Frame parent) {
    return new JLBSFExtOptionsD(parent, this);
  }

  class JLBSFExtOptionsD extends JOptionsDialog {

    JSlider iterationJS;
    JTextField maxerrorTF;
    JTextField rangefactorTF;
    JTextField deltafactorTF;
    JCheckBox usebkgCB, useLastFactorsCB, showBoxCB;

    public JLBSFExtOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new GridLayout(0, 1, 6, 6));
      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      principalPanel.add(jPanel8);
      jPanel8.add(new JLabel("Number of iterations: "));
      JLabel iterationTF = new JLabel();
      iterationJS = new JSlider();
      iterationJS.setToolTipText("Maximum number of Le Bail iterations during a cycle");
      SliderListener listener = new SliderListener(iterationTF);
      iterationJS.addChangeListener(listener);
      jPanel8.add(iterationTF);
      jPanel8.add(iterationJS);
      JPanel jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      principalPanel.add(jPanel9);
      jPanel9.add(new JLabel("Convergence error: "));
      maxerrorTF = new JTextField(Constants.FLOAT_FIELD);
      maxerrorTF.setToolTipText("Iterations stop when all intensities change less than this value");
      jPanel9.add(maxerrorTF);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      principalPanel.add(jPanel9);
      jPanel9.add(new JLabel("Range factor: "));
      rangefactorTF = new JTextField(Constants.FLOAT_FIELD);
      rangefactorTF.setToolTipText("Factor for peak range computation (1 = the entire peak range)");
      jPanel9.add(rangefactorTF);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      principalPanel.add(jPanel9);
      jPanel9.add(new JLabel("Summation range: "));
      deltafactorTF = new JTextField(Constants.FLOAT_FIELD);
      deltafactorTF.setToolTipText("All peaks closer than this range (relative to position) will be used togheter");
      jPanel9.add(deltafactorTF);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      principalPanel.add(jPanel9);
      usebkgCB = new JCheckBox("Use background");
      usebkgCB.setToolTipText("Uncheck the box to subtract the background during the iterations");
      jPanel9.add(usebkgCB);
      useLastFactorsCB = new JCheckBox("Use previous factors");
      useLastFactorsCB.setToolTipText("Check this box to start from the last extracted factors");
      jPanel9.add(useLastFactorsCB);

      setTitle("Le Bail options");
      initParameters();
      pack();
      iterationJS.setValue(getIterationMax());
    }

    public void initParameters() {
      iterationJS.setMaximum(100);
      iterationJS.setMinimum(0);
      iterationJS.setValue(100);
      iterationJS.setPaintTicks(true);
      iterationJS.setMajorTickSpacing(10);
      iterationJS.setMinorTickSpacing(5);

      iterationJS.setPaintLabels(true);
      iterationJS.setSnapToTicks(false);

      iterationJS.setLabelTable(iterationJS.createStandardLabels(20));

      maxerrorTF.setText(getErrorMaximum());
      rangefactorTF.setText(getRangeFactor());
      deltafactorTF.setText(getDeltaMaximum());
      usebkgCB.setSelected(getUseBkgB());
      useLastFactorsCB.setSelected(getUseLastFactorsB());
    }

    public void retrieveParameters() {
      setIterationMax(iterationJS.getValue());
      setErrorMax(maxerrorTF.getText());
      setDeltaMax(deltafactorTF.getText());
      setRangeFactor(rangefactorTF.getText());
      setUseBkg(usebkgCB.isSelected());
      setUseLastFactors(useLastFactorsCB.isSelected());
    }
  }

  class SliderListener implements ChangeListener {
    JLabel tf;

    public SliderListener(JLabel f) {
      tf = f;
    }

    public void stateChanged(ChangeEvent e) {
      JSlider s1 = (JSlider) e.getSource();
      tf.setText(Integer.toString(s1.getValue()));
    }
  }

}
