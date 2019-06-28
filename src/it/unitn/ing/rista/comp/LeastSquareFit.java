/*
 * @(#)LeastSquareFit.java created 01/01/1997 Mesiano
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

package it.unitn.ing.rista.comp;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.JOptionsDialog;

import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.util.Vector;
import java.io.*;

import Jama.Matrix;

/**
 * The LeastSquareFit is a class
 *
 * @version $Revision: 1.25 $, $Date: 2006/07/20 13:39:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class LeastSquareFit extends OptimizationAlgorithm {

  public static String[] diclistc = {"_refine_ls_number_iteration",
                                     "_riet_refine_ls_precision",
                                     "_riet_refine_ls_derivative_step",
		                                 "_riet_refine_ls_lambda",
                                     "_riet_refine_ls_double_derivative"

  };
  public static String[] diclistcrm = {"_refine_ls_number_iteration",
                                       "_riet_refine_ls_precision",
                                       "_riet_refine_ls_derivative_step",
		                                   "_riet_refine_ls_lambda",
                                       "_riet_refine_ls_double_derivative"

  };
  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  int niter = 0;
  int ipflg = 0;
  int brkflg = 0;
  double derstep = 0.001;
  boolean doubleder = false;
  double prcsn = 0.00000001;
  int n0 = 0;
  int flg;
  double am[] = null;
  double g[] = null;
  int[] choleskyFlag = null;
	double lambdaStart = 0.01;

  public boolean simplifyForm = false;

  public double wssLimit = 0.0;

  public LeastSquareFit(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Marqardt Least Squares";
    IDlabel = "Marqardt Least Squares";
    description = "select this to use the Marqardt Least Squares";
  }

  public LeastSquareFit(XRDcat aobj) {
    this(aobj, "Marqardt Least Squares");
  }

  public LeastSquareFit() {
    identifier = "Marqardt Least Squares";
    IDlabel = "Marqardt Least Squares";
    description = "select this to use the Marqardt Least Squares";
  }

  public void initConstant() {
    Nstring = 5;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    for (int i = 0; i < totsubordinateloop; i++)
      diclistRealMeaning[i] = diclistcrm[i];
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
  }

  public void initParameters() {
    super.initParameters();
    stringField[0] = MaudPreferences.getPref(iterations, "3");
    stringField[2] = MaudPreferences.getPref("leastSquares.derivateStep", "0.001");
    stringField[4] = MaudPreferences.getPref("leastSquares.doubleDerivate", "false");
    stringField[1] = MaudPreferences.getPref("leastSquares.precision", "0.00000001");
	  stringField[3] = MaudPreferences.getPref("leastSquares.lambda", "0.01");
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.setIterations(Integer.parseInt(stringField[0]));
//    MaudPreferences.setPref(LeastSquareFit.iterations, stringField[0]);
    derstep = Double.parseDouble(stringField[2]);
    prcsn = Double.parseDouble(stringField[1]);
    doubleder = isDerivative2();
	  lambdaStart = Double.parseDouble(stringField[3]);
  }

  public void setDerivateStep(double value) {
    if (value != 0) {
      derstep = value;
      stringField[2] = Double.toString(value);
    }
  }

  public void setPrecision(double value) {
    if (value != 0.0) {
      prcsn = value;
      stringField[1] = Double.toString(value);
    }
  }

  public void setDerivative2(boolean value) {
    doubleder = value;
    if (value)
      stringField[4] = "true";
    else
      stringField[4] = "false";
  }

  public boolean isDerivative2() {
    if (stringField[4].equalsIgnoreCase("true"))
      return true;
    return false;
  }

  public void setIterations(int number) {
    super.setIterations(number);
    stringField[0] = Integer.toString(number);
  }

  public boolean newModel = true;

  public void solve(launchBasic computation, Function funtionTominimize) {

    fittingFunction = funtionTominimize;

    newModel = MaudPreferences.getBoolean("leastSquares.newModelReduceMemory", true);
    if (!(fittingFunction instanceof FilePar))
      newModel = false;
    updateStringtoDoubleBuffering(false);

/*		GregorianCalendar greg = new GregorianCalendar();
		GregorianCalendar releasingreg = new GregorianCalendar(	Constants.releasingYear,
																														Constants.releasingMonth - 1,
																														Constants.releasingDate);
		GregorianCalendar expiringreg  = new GregorianCalendar(	Constants.expiringYear,
																														Constants.expiringMonth - 1,
																														Constants.expiringDate);

    if (greg.before(releasingreg) || greg.after(expiringreg)) {
      if (outputframe != null)
    	  (new AttentionD(outputframe, "This beta version has expired, download a newer one at "
        + "http://www.ing.unitn.it/~luttero/maud", (JButton) null)).setVisible(true);
    	else
    	  System.out.println("This beta version has expired, download a newer one at "
        + "http://www.ing.unitn.it/~luttero/maud");

    }*/

    int parNumber = 0;

    fittingFunction.setDerivate(false);
    if (newModel)
      parNumber = ((FilePar) fittingFunction).prepareIterationNewModel();
    else
      parNumber = fittingFunction.prepareIteration();
    int iterations = getIterations();
    if (outputframe != null) {
      outputframe.getProgressBar().setProgressBarValue(
              (parNumber + 1) * iterations + 1);
      computation.setIterationSliderValue(iterations);
    }
    if (computation != null && computation.shouldStop()) {
      return;
    }
    int dataNumber = fittingFunction.getNumberOfData();
    fittingFunction.computeFirstFit();

    double dta[] = null;
    double wgt[] = null;
    double fit[] = null;
    if (!newModel) {
      fittingFunction.getFit();
      dta = new double[dataNumber];
      wgt = new double[dataNumber];
      fit = new double[dataNumber];
    }

    if (computation != null && computation.shouldStop()) {
      return;
    }

    if (!newModel) {
      for (int i = 0; i < dataNumber; i++) {
        dta[i] = fittingFunction.getData(i);
        wgt[i] = fittingFunction.getWeight(i);
        wgt[i] *= wgt[i];
        fit[i] = fittingFunction.getFit(i);
      }
    }
    int nprm = fittingFunction.getNumberOfFreeParameters();
    double parm[] = new double[nprm];
    double[] minSignificantValue = new double[nprm];

    choleskyFlag = new int[nprm];
    for (int i = 0; i < nprm; i++) {
      parm[i] = fittingFunction.getFreeParameter(i);
      minSignificantValue[i] = fittingFunction.getParameterMinSignificantValue(i);
    }
    double parmn[] = new double[nprm];

    if (computation != null && computation.shouldStop()) {
      return;
    }

//    int iter = 0;
//    if (ipflg != 0)
//      iter = niter;
    int check = 0; // if check != 0 the new wss can be sligthly greater than the old one
//    int brkflg = 0; // operator interrupt
//  	int mxiter = getIteration();

    fittingFunction.setDerivate(true);
    if (computation != null && computation.shouldStop()) {
      return;
    }

    int mdi = (nprm + 1) * nprm / 2;

/* Memory allocation */

    final double deriv[] = new double[nprm];
    final double b[] = new double[nprm];
    final double grad[] = new double[nprm];
    final double c[] = new double[mdi];

/*   end memory allocation  */

    for (int i = 0; i < nprm; i++)
      b[i] = parm[i];

    if (nprm <= 0) {
      printf("Only function computation, no free parameters");
      return;
    }
    if (dataNumber < nprm) {
      printf("# of data pts (", dataNumber, ") is less than # of param. (", nprm, ") to be fit");
      return;
    }

// start least squares fitting

    int conver = 0;
    double lambda = lambdaStart;
    double lmin = 1.0e+20;
    double phi = 1.0;
    niter = -1;

    fittingFunction.saveparameters();
    if (fittingFunction instanceof FilePar)
      ((FilePar) fittingFunction).updatePlot();

    if (computation != null && computation.shouldStop()) {
      return;
    }

    double[][] derivf = null;
    Vector fitVector = null;

    if (!newModel) {
      derivf = new double[dataNumber][nprm];
    } else {

      int numberSpectra = ((FilePar) fittingFunction).getNumberOfSpectra();
      fitVector = new Vector(numberSpectra + 1, 1);
      for (int i = 0; i < numberSpectra; i++) {
        SpectrumFitContainer spectrum = new SpectrumFitContainer(((FilePar) fittingFunction).getDatafile(i),
                ((FilePar) fittingFunction).getActiveSample());

        spectrum.createDerivate(nprm);
        fitVector.addElement(spectrum);
      }
      EnergyFitContainer energy = new EnergyFitContainer(((FilePar) fittingFunction).getActiveSample());
      energy.createDerivate(nprm);
      fitVector.addElement(energy);
    }
//	  Assert.assert(derivf);


    double wss = 0.0;
    if (newModel) {
      for (int i = 0; i < fitVector.size(); i++) {
        SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVector.elementAt(i);
        double wssp = spectrum.getWSS();
        if (Constants.testing)
          System.out.println("Wss Least Squares element " + i + " = " + wssp);
        wss += wssp;
      }
    } else {
      wss = fittingFunction.getWSS();
    }
    double oldwss = wss;
    printf("Wss = ", wss);
    printf("Number of iterations : ", getIterations());


    am = new double[mdi];
    g = new double[nprm];
/*          next iteration      */
    if (outputframe != null)
      outputframe.increaseProgressBarValue();

    if (computation != null && computation.shouldStop()) {
      return;
    }

    boolean flagfgl = (conver == 0);
    if (computation != null)
      flagfgl = flagfgl && (!computation.shouldStopIteration());
    while ((niter + 1 < getIterations()) && flagfgl) {
      if (niter > -1 && fittingFunction instanceof FilePar)
        ((FilePar) fittingFunction).closeLogResultFile();
      ++niter;
      if ((niter > 4) && (lambda < lmin))
        lmin = lambda;
      String message = "Computing iteration # " + Integer.toString(niter + 1)
              + " of " + Integer.toString(getIterations());
      if (outputframe != null)
        outputframe.setProgressText(message);
      else
        System.out.println(message);

      if (computation != null && computation.shouldStop()) {
        return;
      }

//           decrease lambda
      lambda *= 0.31622777;
//		printf("lambda1 = ",lambda);

/*			fittingFunction.backupallParameters();
			for (int i = 0; i < nprm; i++)
				parm[i] = fittingFunction.getFreeParameter(i);*/

			computeDerivativeMatrix(dataNumber, nprm, parm, fitVector, derivf, fit, computation, minSignificantValue);

      if (computation != null && computation.shouldStop()) {
        return;
      }

      for (int i = 0; i < mdi; i++)
        am[i] = 0.0;
      for (int i = 0; i < nprm; i++)
        grad[i] = 0.0;

      if (newModel) {
/*
        for (int i1 = 0; i1 < fitVector.size(); i1++) {
          SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVector.elementAt(i1);
          for (int i2 = 0; i2 < spectrum.dataNumber; i2++) {
            double fmm = (spectrum.fit[i2] - spectrum.dta[i2]) * spectrum.wgt2[i2];
            for (int sp = 0; sp < nprm; sp++) {
              double[] derivr = spectrum.getDerivate(sp);
              if (derivr.length > 0) {
                grad[sp] += derivr[i2] * fmm;
                int l = (sp + 1) * sp / 2;
                for (int kcs = 0; kcs <= sp; kcs++) {
                  double[] derivs = spectrum.getDerivate(kcs);
                  if (derivs.length > 0)
                    am[l + kcs] += derivr[i2] * derivs[i2] * spectrum.wgt2[i2];
                }
              }
            }
          }
        }*/
	      final int maxThreads = Constants.maxNumberOfThreads; // Math.min(Constants.maxNumberOfThreads, fitVector.size());
	      if (maxThreads > 1 && Constants.threadingGranularity >= Constants.MEDIUM_GRANULARITY) {
		      if (Constants.debugThreads)
			      System.out.println("Thread datafileset " + getLabel());
		      int i;
		      final double[][] gradf = new double[maxThreads][nprm];
		      final double[][] amf = new double[maxThreads][mdi];
		      PersistentThread[] threads = new PersistentThread[maxThreads];

		      if (maxThreads >= fitVector.size()) {
			      for (i = 0; i < maxThreads; i++) {
				      final int nprmf = nprm;
				      final Vector fitVectorf = fitVector;
				      threads[i] = new PersistentThread(i) {
					      @Override
					      public void executeJob() {
						      int istart = this.getJobNumberStart();
						      int iend = this.getJobNumberEnd();

						      for (int j = istart; j < iend; j++) {
							      SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVectorf.elementAt(j);
							      for (int i2 = 0; i2 < spectrum.dataNumber; i2++) {
								      double fmm = (spectrum.fit[i2] - spectrum.dta[i2]) * spectrum.wgt2[i2];
								      for (int sp = 0; sp < nprmf; sp++) {
									      double[] derivr = spectrum.getDerivate(sp);
									      if (derivr.length > 0) {
										      gradf[this.threadNumber][sp] += derivr[i2] * fmm;
										      int l = (sp + 1) * sp / 2;
										      for (int kcs = 0; kcs <= sp; kcs++) {
											      double[] derivs = spectrum.getDerivate(kcs);
											      if (derivs.length > 0)
												      amf[this.threadNumber][l + kcs] += derivr[i2] * derivs[i2] * spectrum.wgt2[i2];
										      }
									      }
								      }
							      }
						      }
					      }
				      };
			      }
			      i = 0;
			      int istep = (int) (0.9999 + fitVector.size() / maxThreads);
			      for (int j = 0; j < maxThreads; j++) {
				      int is = i;
				      if (j < maxThreads - 1)
					      i = Math.min(i + istep, fitVector.size());
				      else
					      i = fitVector.size();
				      threads[j].setJobRange(is, i);
				      threads[j].start();
			      }
		      } else {
			      int maxDataNumber = 0;
			      for (int i1 = 0; i1 < fitVector.size(); i1++) {
				      SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVector.elementAt(i1);
				      int dataNumber1 = spectrum.dataNumber;
				      if (maxDataNumber < dataNumber1)
					      maxDataNumber = dataNumber1;
			      }
			      for (i = 0; i < maxThreads; i++) {
				      final int nprmf = nprm;
				      final Vector fitVectorf = fitVector;
				      threads[i] = new PersistentThread(i) {
					      @Override
					      public void executeJob() {
						      int istart = this.getJobNumberStart();
						      int iend = this.getJobNumberEnd();

						      for (int j = 0; j < fitVectorf.size(); j++) {
							      SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVectorf.elementAt(j);
							      if (istart < spectrum.dataNumber) {
								      int iendn = Math.min(iend, spectrum.dataNumber);
								      for (int i2 = istart; i2 < iendn; i2++) {
									      double fmm = (spectrum.fit[i2] - spectrum.dta[i2]) * spectrum.wgt2[i2];
									      for (int sp = 0; sp < nprmf; sp++) {
										      double[] derivr = spectrum.getDerivate(sp);
										      if (derivr.length > 0) {
											      gradf[this.threadNumber][sp] += derivr[i2] * fmm;
											      int l = (sp + 1) * sp / 2;
											      for (int kcs = 0; kcs <= sp; kcs++) {
												      double[] derivs = spectrum.getDerivate(kcs);
												      if (derivs.length > 0)
													      amf[this.threadNumber][l + kcs] += derivr[i2] * derivs[i2] * spectrum.wgt2[i2];
											      }
										      }
									      }
								      }
							      }
						      }
					      }
				      };
			      }
			      i = 0;
			      int istep = (int) (0.9999 + maxDataNumber / maxThreads);
			      for (int j = 0; j < maxThreads; j++) {
				      int is = i;
				      if (j < maxThreads - 1)
					      i = Math.min(i + istep, maxDataNumber);
				      else
					      i = maxDataNumber;
				      threads[j].setJobRange(is, i);
				      threads[j].start();
			      }
		      }

		      boolean running;
		      do {
			      running = false;
			      try {
				      Thread.sleep(Constants.timeToWaitThreadsEnding);
			      } catch (InterruptedException r) {
			      }
			      for (int h = 0; h < maxThreads; h++) {
				      if (!threads[h].isEnded())
					      running = true;
			      }
		      } while (running);
		      for (i = 0; i < maxThreads; i++) {
			      for (int j = 0; j < nprm; j++)
				      grad[j] += gradf[i][j];
			      for (int j = 0; j < mdi; j++)
				      am[j] += amf[i][j];
		      }
	      } else
		      for (int i1 = 0; i1 < fitVector.size(); i1++) {
			      SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVector.elementAt(i1);
			      for (int i2 = 0; i2 < spectrum.dataNumber; i2++) {
				      double fmm = (spectrum.fit[i2] - spectrum.dta[i2]) * spectrum.wgt2[i2];
				      for (int sp = 0; sp < nprm; sp++) {
					      double[] derivr = spectrum.getDerivate(sp);
					      if (derivr.length > 0) {
						      grad[sp] += derivr[i2] * fmm;
						      int l = (sp + 1) * sp / 2;
						      for (int kcs = 0; kcs <= sp; kcs++) {
							      double[] derivs = spectrum.getDerivate(kcs);
							      if (derivs.length > 0)
								      am[l + kcs] += derivr[i2] * derivs[i2] * spectrum.wgt2[i2];
						      }
					      }
				      }
			      }
		      }
      } else {
/*        for (int i = 0; i < dataNumber; i++) {
          double fmm = (fit[i] - dta[i]) * wgt[i];
          for (int sp = 0; sp < nprm; sp++) {
            grad[sp] += derivf[i][sp] * fmm;
            int l = (sp + 1) * sp / 2;
            for (int kcs = 0; kcs <= sp; kcs++)
              am[l + kcs] += derivf[i][sp] * derivf[i][kcs] * wgt[i];
          }
        }*/

	      final int maxThreads = Math.min(Constants.maxNumberOfThreads, dataNumber / 10);
	      if (maxThreads > 1 && Constants.threadingGranularity >= Constants.MEDIUM_GRANULARITY) {
		      if (Constants.debugThreads)
			      System.out.println("Thread datafileset " + getLabel());
		      int i;
		      final double[][] gradf = new double[maxThreads][nprm];
		      final double[][] amf = new double[maxThreads][mdi];
		      PersistentThread[] threads = new PersistentThread[maxThreads];
		      for (i = 0; i < maxThreads; i++) {
			      final int nprmf = nprm;
			      final double[] fitf = fit;
			      final double[] dtaf = dta;
			      final double[] wgtf = wgt;
			      final double[][] derivff = derivf;
			      threads[i] = new PersistentThread(i) {
				      @Override
				      public void executeJob() {
					      int i1 = this.getJobNumberStart();
					      int i2 = this.getJobNumberEnd();

					      for (int j = i1; j < i2; j++) {
						      double fmm = (fitf[j] - dtaf[j]) * wgtf[j];
						      for (int sp = 0; sp < nprmf; sp++) {
							      gradf[this.threadNumber][sp] += derivff[j][sp] * fmm;
							      int l = (sp + 1) * sp / 2;
							      for (int kcs = 0; kcs <= sp; kcs++)
								      amf[this.threadNumber][l + kcs] += derivff[j][sp] * derivff[j][kcs] * wgtf[j];
						      }
					      }
				      }
			      };
		      }
		      i = 0;
		      int istep = (int) (0.9999 + dataNumber / maxThreads);
		      for (int j = 0; j < maxThreads; j++) {
			      int is = i;
			      if (j < maxThreads - 1)
				      i = Math.min(i + istep, dataNumber);
			      else
				      i = dataNumber;
//			      if (Constants.debugThreads)
//				      System.out.println("Thread indices " + getLabel() + " from: " + is + " to: " + i);
			      threads[j].setJobRange(is, i);
			      threads[j].start();
		      }
		      boolean running;
		      do {
			      running = false;
			      try {
				      Thread.sleep(Constants.timeToWaitThreadsEnding);
			      } catch (InterruptedException r) {
			      }
			      for (int h = 0; h < maxThreads; h++) {
				      if (!threads[h].isEnded())
					      running = true;
			      }
		      } while (running);
		      for (int j = 0; j < nprm; j++)
			      grad[j] = gradf[0][j];
		      for (int j = 0; j < mdi; j++)
			      am[j] = amf[0][j];
		      for (i = 1; i < maxThreads; i++) {
		        for (int j = 0; j < nprm; j++)
			        grad[j] += gradf[i][j];
			      for (int j = 0; j < mdi; j++)
				      am[j] += amf[i][j];
		      }
	      } else
		      for (int i = 0; i < dataNumber; i++) {
			      double fmm = (fit[i] - dta[i]) * wgt[i];
			      for (int sp = 0; sp < nprm; sp++) {
				      grad[sp] += derivf[i][sp] * fmm;
				      int l = (sp + 1) * sp / 2;
				      for (int kcs = 0; kcs <= sp; kcs++)
					      am[l + kcs] += derivf[i][sp] * derivf[i][kcs] * wgt[i];
			      }
		      }
      }
//           save "a" matrix and current parameter values "b"
      for (int i = 0; i < mdi; i++)
        c[i] = am[i];
      if (Constants.testing) {
        boolean printMatrices = MaudPreferences.getBoolean("leastSquares.printMatrices", false);
        if (printMatrices) {
          for (int i = 0; i < mdi; i++)
            System.out.println("am[" + i + "]" + am[i]);
          for (int i = 0; i < nprm; i++)
            System.out.println("grad[" + i + "]" + grad[i]);
        }
      }
      for (int i = 0; i < nprm; i++)
        deriv[i] = b[i];
//          doctor "a" matrix diagonal elements to be:
//             a=a(1+lambda)+phi*lambda
      do {
        if (computation != null && computation.shouldStop())
          return;
        flg = 1;
        while (flg != 0 && (conver == 0)) {
          double da = phi * lambda;
          for (int j = 0; j < nprm; j++) {
            g[j] = -grad[j];
            int l1 = (j + 1) * (j + 2) / 2 - 1;
            am[l1] = c[l1] * (1.0 + lambda) + da;
            for (int i = 0; i < j; i++)
              am[l1 - i - 1] = c[l1 - i - 1];
          }
          flg = chodec(nprm); //    choleski decomposition
          if (flg != 0) {
            if (lambda < prcsn)
              lambda = prcsn;
            lambda *= 10.0;
            if (lambda > 100000.0 * lmin)
              conver = 1;
          }
        }
        if (conver == 0) {
          choback(nprm);
//         find new parameters b=d+g
//        (no counts the  of zero elements in g)
          if (computation != null && computation.shouldStop()) {
            return;
          }
          n0 = 0;
          for (int i = 0; i < nprm; i++) {
            b[i] = (deriv[i] + g[i]);
            if (Math.abs(g[i]) <= Math.abs(prcsn * deriv[i]))
              ++n0;
            parmn[i] = b[i];
          }
          printout(parmn, nprm);
        }
        if (n0 == nprm)
          conver = 1;
        else {
          if (computation != null && computation.shouldStop()) {
            return;
          }
          boolean bounds = false;
          for (int j = 0; j < nprm; j++)
            bounds = (bounds || fittingFunction.checkBound(j, parmn[j]));
          if (bounds) {
            wss = oldwss * 1.01;
//        		printf("At least one parameter out of bounds, recomputing...");
          } else {
            if (fittingFunction.singleFunctionComputing())
              fittingFunction.setDerivate(false);
            fittingFunction.setFreeParameters(parmn);
            if (!newModel) {
              for (int i = 0; i < dataNumber; i++)
                fit[i] = fittingFunction.getFit(i);
              wss = fittingFunction.getWSS();
//              if (fittingFunction instanceof FilePar)  // added
//                ((FilePar) fittingFunction).updatePlot();
            } else {
              fittingFunction.computeFit();
              wss = 0.0;
              for (int i = 0; i < fitVector.size(); i++) {
                SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVector.elementAt(i);
                spectrum.checkFit();
                wss += spectrum.getWSS();
              }
//              if (fittingFunction instanceof FilePar)  // added
//                ((FilePar) fittingFunction).updatePlot();
            }

            printf("Wgt'd ssq = ", wss);
            if (fittingFunction.singleFunctionComputing())
              fittingFunction.setDerivate(true);                      // to check
            if (computation != null && computation.shouldStop()) {
              return;
            }
          }
          if (Double.isNaN(wss) || wss == 0.0) {
            wss = oldwss * 1.01;
            printf("Wrong parameters, recomputing...");
          }
	        if (wss < oldwss && wss > 0.0) {
            oldwss = wss;
          } else {
            if (check != 0) {
              oldwss = wss;
              wss = oldwss * 1.01;
            }
            if (lambda < prcsn)
              lambda = prcsn;
            lambda *= 10.0;
            if (lambda > 100000.0 * lmin) {
              conver = 1;
            }
          }
          if (computation != null && computation.shouldStop()) {
            return;
          }
        }
      } while ((wss > oldwss) && (conver == 0));
      if (wss > oldwss) {
        printf("No solution found, setting old values...");
        fittingFunction.setFreeParameters(parm);
      }

      if (computation != null && computation.shouldStop()) {
        return;
      }
      fittingFunction.setDerivate(false);
      if (!fittingFunction.singleFunctionComputing()) {
        if (newModel) {
          fittingFunction.computeFirstFit();
          if (fittingFunction instanceof FilePar)  // added
            ((FilePar) fittingFunction).updatePlot();
          for (int i1 = 0; i1 < fitVector.size(); i1++) {
            SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVector.elementAt(i1);
            spectrum.checkFit();
          }
          wss = 0.0;
          for (int i = 0; i < fitVector.size(); i++) {
            SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVector.elementAt(i);
            wss += spectrum.getWSS();
          }
        } else {
          fittingFunction.computeFirstFit();
          double[] newFit = fittingFunction.getFit();
          if (fittingFunction instanceof FilePar)  // added
            ((FilePar) fittingFunction).updatePlot();
          for (int i = 0; i < dataNumber; i++)
            fit[i] = fittingFunction.getFit(i);
          wss = fittingFunction.getWSS();
        }
        printf("Wgt'd ssq = ", wss);
      }
      oldwss = wss;
      fittingFunction.setDerivate(true);
      flagfgl = (conver == 0);
      if (computation != null)
        flagfgl = flagfgl && (!computation.shouldStopIteration());
      if (conver == 0) {
        fittingFunction.saveparameters();
        if (fittingFunction instanceof FilePar)
          ((FilePar) fittingFunction).updatePlot();
        for (int i = 0; i < nprm; i++) {
          parm[i] = fittingFunction.getFreeParameter(i);
          parmn[i] = parm[i];
          b[i] = parm[i];
        }
        if (outputframe != null)
          outputframe.increaseProgressBarValue();
        if (computation != null && computation.shouldStop()) {
          return;
        }
      }
    }       //   next iteration
    fittingFunction.setDerivate(false);
    if (conver == 0) {
      if (niter + 1 >= getIterations()) {
        ++niter;
        printf(niter, ", iterations - pause ");
      } else {
        printf("operator interrupt");
        brkflg = 0;
      }
    } else {
//             convergen\ufffdce reached
      double ri = lambda / lmin;
      printf("           convergence reached");
      printf("# of params fit = ", nprm);
      printf("# of params converged = ", n0);
      printf("lambda/l(min) = ", ri);
      n0 = 0;
    }
    finaloutput(nprm, dataNumber, niter, mdi, c, flg);
    if (newModel) {
      for (int i1 = 0; i1 < fitVector.size(); i1++) {
        SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVector.elementAt(i1);
        spectrum.dispose();
      }
      fitVector.removeAllElements();
      fitVector = null;
    }
  }

  public void computeDerivativeMatrix(int dataNumber, int nprm, double parm[],
                                      Vector fitVector, double derivf[][], double fit[],
                                      launchBasic computation, double[] minSignificantValue) {
    //        compute matrix of derivative

    if (computation != null && computation.shouldStop()) {
      return;
    }

    double dparp;
    double firstfit[] = null;
    double secondfit[] = null;
    if (!newModel) {
      firstfit = new double[dataNumber];
      secondfit = new double[dataNumber];
    }
    //

//    System.out.println(derstep);

    for (int sp = 0; sp < nprm; sp++) {

      if (parm[sp] == 0 && minSignificantValue[sp] == 0)
        dparp = derstep;
      else if (Math.abs(parm[sp]) < Math.abs(minSignificantValue[sp]))
        dparp = (minSignificantValue[sp] * derstep);
      else
        dparp = parm[sp] * derstep;
//      System.out.println(parm[sp] + " " + minSignificantValue[sp] + " " + dparp);
      double dparp2 = dparp * 2.0f;
      double oldpar = parm[sp];
      double parm1 = parm[sp] + dparp;
//      System.out.println("Setting (" + sp + ") = " + parm1);
      fittingFunction.setFreeParameter(sp, parm1);
      fittingFunction.computeFit();
      if (newModel) {
        for (int i1 = 0; i1 < fitVector.size(); i1++) {
          SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVector.elementAt(i1);
          spectrum.checkDerivateFit();
        }
      } else {
        for (int i = 0; i < dataNumber; i++)
          firstfit[i] = fittingFunction.getFit(i);
      }
      if (doubleder) {
        parm1 = oldpar - dparp;
        fittingFunction.setFreeParameter(sp, parm1);
        fittingFunction.computeFit();
        if (newModel) {
          for (int i1 = 0; i1 < fitVector.size(); i1++) {
            SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVector.elementAt(i1);
            spectrum.checkDerivate2Fit();
          }
        } else {
          for (int i = 0; i < dataNumber; i++)
            secondfit[i] = fittingFunction.getFit(i);
        }
      }

      if (computation != null && computation.shouldStop()) {
        return;
      }

      if (outputframe != null)
        outputframe.increaseProgressBarValue();
      if (newModel) {
//        fittingFunction.computeFit();
        for (int i1 = 0; i1 < fitVector.size(); i1++) {
          SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVector.elementAt(i1);
          if (doubleder)
            spectrum.setDerivate(spectrum.computeDerivate2(dparp2), sp);
          else
            spectrum.setDerivate(spectrum.computeDerivate(dparp), sp);
        }
      } else {
        for (int i = 0; i < dataNumber; i++) {
//          System.out.println(i + " " + firstfit[i] + " " + fit[i]);
          if (doubleder)
            derivf[i][sp] = ((firstfit[i] - secondfit[i]) / dparp2);
          else
            derivf[i][sp] = ((firstfit[i] - fit[i]) / dparp);
        }
      }
//      System.out.println("Setting back (" + sp + ") = " + oldpar);
      fittingFunction.setFreeParameter(sp, oldpar);
    }
	  return;
  }

  public void finaloutput(int nprm, int dataNumber, int niter, int mdi, double c[], int flg) {
//    double wss;

    if (nprm > 0) {
      double[] rValues = fittingFunction.getRefinementIndexes();
      double sig = Math.sqrt(rValues[8] / (dataNumber - nprm));
      printf("sig= ", sig);
      double Rw = rValues[0];
      double R = rValues[4];
//      double ss = rValues[9];
      if (Rw == 0.0)
        Rw = 1.0;
      if (R == 0.0)
        R = 1.0;
      double rsw = Rw * 100.0;
      printf("Rwp (%) = ", rsw);
      printf("Rwpnb (%, no bkg) = ", rValues[1] * 100);
	    printf("Rwpnb1 (%, no bkg rescaled) = ", rValues[2] * 100);
	    printf("Rwpnb2 (%, no bkg rescaled^2) = ", rValues[3] * 100);
      double rb = R * 100.0;
      printf("Rb (%) = ", rb);
      double rexp = Rw / sig;
      fittingFunction.setRexp(rexp);
      printf("Rexp (%) = ", rexp * 100.0);
      printf("# iterations = ", niter);
      System.out.println("Generating correlation matrix");
      if (niter > 0) {
        for (int j = 0; j < mdi; j++)
          am[j] = c[j];

        if (fittingFunction instanceof FilePar && fittingFunction.logOutput()) {
          OutputStream out = fittingFunction.getResultStream();
          if (out != null) {
            int maxcolumn = 30;
            int maxdigits = 15;
            try {
              printLine(out, "Jacobi matrix :");
              printString(out, "#", 4);
              StringBuffer dump = new StringBuffer("    ");
              for (int sp = 0; sp < nprm; sp++) {
                printString(out, " " + sp, maxdigits);
                if (MoreMath.isMultipleOf(sp + 1, maxcolumn)) {
                  newLine(out);
                  dump.append("  ");
                  printString(out, dump.toString());
                }
              }
              newLine(out);
              for (int sp = 0; sp < nprm; sp++) {
                printString(out, sp + " ", 4);
                int l = (sp + 1) * sp / 2;
                dump = new StringBuffer("    ");
                for (int kcs = 0; kcs <= sp; kcs++) {
                  printString(out, " " + ((double) am[l + kcs]), maxdigits);
                  if (MoreMath.isMultipleOf(kcs + 1, maxcolumn)) {
                    newLine(out);
                    dump.append("  ");
                    printString(out, dump.toString());
                  }
                }
                newLine(out);
              }
              out.flush();
            } catch (Exception io) {
              io.printStackTrace();
            }
          }
        }


        flg = chodec(nprm);
        double[][] cov1 = new double[nprm][nprm];
        double[][] cov2;
        for (int i = 0; i < nprm; i++) {
          int l = (i + 1) * i / 2;
          for (int j = 0; j < i; j++) {
            cov1[i][j] = cov1[j][i] = am[l + j];
          }
        }
        if (fittingFunction instanceof FilePar && fittingFunction.logOutput()) {
          OutputStream out = fittingFunction.getResultStream();
          int maxcolumn = 30;
          int maxdigits = 15;
          try {
            try {
              Matrix icov = new Matrix(cov1);
              Matrix cov = icov.inverse();
              cov2 = cov.getArray();
              for (int i = 0; i < nprm; i++)
                for (int j = 0; j < nprm; j++)
                  cov1[i][j] = cov2[i][j] / Math.sqrt(Math.abs(cov2[i][i] * cov2[j][j]));
            } catch (Exception eio) {
              printLine(out, "Warning: error computing correlation matrix, no correlation matrix in output!");
            }
            printLine(out, "Correlation matrix:");
            printString(out, "#", 4);
            StringBuffer dump = new StringBuffer("    ");
            for (int sp = 0; sp < nprm; sp++) {
              printString(out, " " + sp, maxdigits);
              if (MoreMath.isMultipleOf(sp + 1, maxcolumn)) {
                newLine(out);
                dump.append("  ");
                printString(out, dump.toString());
              }
            }
            newLine(out);
            for (int sp = 0; sp < nprm; sp++) {
              printString(out, sp + " ", 4);
              //         int l = (sp + 1) * sp / 2;
              dump = new StringBuffer("    ");
              for (int kcs = 0; kcs <= sp; kcs++) {
                printString(out, " " + ((double) cov1[sp][kcs]), maxdigits);
                if (MoreMath.isMultipleOf(kcs + 1, maxcolumn)) {
                  newLine(out);
                  dump.append("  ");
                  printString(out, dump.toString());
                }
              }
              newLine(out);
            }
            out.flush();
          } catch (Exception io) {
            io.printStackTrace();
          }
        }


        for (int j = 0; j < mdi; j++)
          am[j] = c[j];

        flg = chodec(nprm);

        cov2 = new double[nprm][nprm];
        for (int i = 0; i < nprm; i++) {
          int l = (i + 1) * i / 2;
          for (int j = 0; j < i; j++) {
            cov2[i][j] = am[l + j];
          }
        }
        if (fittingFunction instanceof FilePar && fittingFunction.logOutput()) {
          OutputStream out = fittingFunction.getResultStream();
          int maxcolumn = 30;
          int maxdigits = 15;
          if (out != null) {
          try {
          /*  Matrix icov = new Matrix(cov2);
            Matrix cov = icov.inverse();
            icov = cov.transpose().times(cov);
            cov2 = icov.getArray();
            for (int i = 0; i < nprm; i++)
              for (int j = 0; j < nprm; j++)
                cov1[i][j] = cov2[i][j] / Math.sqrt(Math.abs(cov2[i][i] * cov2[j][j]));*/
            printLine(out, "Correlation matrix from Choleski decomposition :");
            printString(out, "#", 4);
            StringBuffer dump = new StringBuffer("    ");
            for (int sp = 0; sp < nprm; sp++) {
              printString(out, " " + sp, maxdigits);
              if (MoreMath.isMultipleOf(sp + 1, maxcolumn)) {
                newLine(out);
                dump.append("  ");
                printString(out, dump.toString());
              }
            }
            newLine(out);
            for (int sp = 0; sp < nprm; sp++) {
              printString(out, sp + " ", 4);
              //         int l = (sp + 1) * sp / 2;
              dump = new StringBuffer("    ");
              for (int kcs = 0; kcs <= sp; kcs++) {
                printString(out, " " + ((double) cov1[sp][kcs]), maxdigits);
                if (MoreMath.isMultipleOf(kcs + 1, maxcolumn)) {
                  newLine(out);
                  dump.append("  ");
                  printString(out, dump.toString());
                }
              }
              newLine(out);
            }
            out.flush();
          } catch (Exception io) {
            io.printStackTrace();
          }
          }
        }

        double dparm[] = new double[nprm];
        for (int j = 0; j < mdi; j++)
          am[j] = c[j];
        flg = chodec(nprm);
        for (int z = 0; z < nprm; z++) {
          for (int j = 0; j < nprm; j++)
            g[j] = 0.0;
          g[z] = 1.0;
          choback(nprm);
          if (choleskyFlag[z] > 0 && !Double.isNaN(g[z]))
            dparm[z] = Math.sqrt(Math.abs(g[z])) * sig;
          else
            dparm[z] = choleskyFlag[z];
//          printf(z, dparm[z]);
        }
        fittingFunction.setErrors(dparm);
      }
      fittingFunction.saveparameters();
      if (fittingFunction instanceof FilePar)
        ((FilePar) fittingFunction).updatePlot();
    }
//  	niter += iter;
    if (flg == 1) {
      printf(" ");
      printf("cholesky negative diagonal :");
      printf("unable to solve with supplied initial parameters");
    }
    if (fittingFunction instanceof FilePar) {
      for (int index = 0; index < ((FilePar) fittingFunction).getSelectedSample().phasesNumber(); index++) {
        Phase aphase = ((FilePar) fittingFunction).getPhase(index);
        double[] qt = ((FilePar) fittingFunction).getPhaseQuantity(index);
        if (qt[0] > 0.0) {
          printf(aphase.getLabel() + " , weight %: ", qt[2], " +- ", qt[3]);
        }
      }
    }
  }

  public void choback(int nfit1)

/*        subroutine obtains cholesky backward solution of matrix a   */ {
    int k1, i, j, l1, mdi1;

    g[0] /= am[0];
    l1 = 0;
    for (i = 1; i < nfit1; i++) {
      for (j = 0; j < i; j++)
        g[i] -= am[++l1] * g[j];
      if (am[++l1] == 0.0)
        am[l1] = 1.0;
      g[i] /= am[l1];
    }
    mdi1 = nfit1 * (nfit1 + 1) / 2 - 1;
    g[nfit1 - 1] /= am[mdi1];
    for (k1 = 1; k1 < nfit1; k1++) {
      i = nfit1 - k1;
      l1 = i * (i + 1) / 2 - 1;
      for (j = 0; j < i; j++)
        g[j] -= g[i] * am[l1 + j + 1];
      g[i - 1] /= am[l1];
    }
  }

  public int chodec(int nfit1) {
/*        subroutine to perform cholesky decomposition        */

    int i, j, k1, l1, k2, k;
    double f;

    flg = 0;
    for (j = 0; j < nfit1; j++) {
      l1 = (j + 2) * (j + 1) / 2 - 1;
      if (j > 0) {
        for (i = j; i < nfit1; i++) {
          k1 = i * (i + 1) / 2 + j;
          f = am[k1];
          for (k = 0; k < j; k++)
            f -= am[k1 - k - 1] * am[l1 - k - 1];
          am[k1] = f;
        }
      }
      if (am[l1] > 0) {
        f = Math.sqrt(am[l1]);
        for (i = j; i < nfit1; i++) {
          k2 = i * (i + 1) / 2 + j;
          am[k2] /= f;
        }
        choleskyFlag[j] = 1;
      } else {
        flg = 1;
        choleskyFlag[j] = -1;
        if (outputEnabled)
          printf("cholesky negative diag j,l,a(l) : ", j, l1, am[l1]);
      }
    }
    return flg;
  }

/* Memory allocation */

  double s_deriv[] = null;
  double s_b[] = null;
  double s_grad[] = null;
  double s_c[] = null;
  double s_parmn[] = null;
  double[][] s_derivf = null;
  double s_firstfit[] = null;
/*   end memory allocation  */

  boolean initialized = false;

  public void initializeMatrices(int dataNumber, int nprm) {

    int mdi = (nprm + 1) * nprm / 2;

/* Memory allocation */

    s_deriv = new double[nprm];
    s_b = new double[nprm];
    s_grad = new double[nprm];
    s_c = new double[mdi];
    s_parmn = new double[nprm];
    s_derivf = new double[dataNumber][nprm];
    am = new double[mdi];
    g = new double[nprm];
    s_firstfit = new double[dataNumber];
    initialized = true;
/*   end memory allocation  */

  }

  public void releaseMemory() {
    s_deriv = null;
    s_b = null;
    s_grad = null;
    s_c = null;
    s_parmn = null;
    am = null;
    g = null;
    s_firstfit = null;
    s_derivf = null;
    initialized = false;
  }

  public LeastSquareFit(SimpleFunction fitFunction, int iterations) {
    super(fitFunction, iterations);
    newModel = false;
  }

  public double simpleSolve(double[] dta, double[] wgt, double[] fit, double[] parm, boolean releaseMemory,
                            int[] controls) {
//    updateStringtoDoubleBuffering();
    int dataNumber = dta.length;
    int nprm = parm.length;
    choleskyFlag = new int[nprm];
    int mdi = (nprm + 1) * nprm / 2;
    if (!initialized || (s_parmn.length != nprm || s_firstfit.length != dataNumber))
      initializeMatrices(dataNumber, nprm);
    simpleFittingFunction.refreshFit(fit, parm, controls);
    double wss = getWSS(dta, fit, wgt);

    for (int i = 0; i < nprm; i++)
      s_b[i] = parm[i];

// start least squares fitting

    int conver = 0;
    double lambda = 0.01;
    double lmin = 1.0e+20;
    double phi = 1.0;
    niter = -1;
    double oldwss = wss;
    int check = 0; // dont permit bigger wss

    while ((niter + 1 < getIterations()) && conver == 0) {
      ++niter;
      if ((niter > 4) && (lambda < lmin))
        lmin = lambda;

//           decrease lambda
      lambda *= 0.31622777;
      computeDerivativeMatrix(dataNumber, nprm, parm, s_derivf, fit, controls);

      for (int i = 0; i < mdi; i++)
        am[i] = 0.0;
      for (int i = 0; i < nprm; i++)
        s_grad[i] = 0.0;

      for (int i = 0; i < dataNumber; i++) {
        double fmm = (fit[i] - dta[i]) * wgt[i];
        for (int sp = 0; sp < nprm; sp++) {
          s_grad[sp] += s_derivf[i][sp] * fmm;
          int l = (sp + 1) * sp / 2;
          for (int kcs = 0; kcs <= sp; kcs++)
            am[l + kcs] += s_derivf[i][sp] * s_derivf[i][kcs] * wgt[i];
        }
      }
//           save "a" matrix and current parameter values "b"
      for (int i = 0; i < mdi; i++)
        s_c[i] = am[i];
      for (int i = 0; i < nprm; i++)
        s_deriv[i] = s_b[i];
//          doctor "a" matrix diagonal elements to be:
//             a=a(1+lambda)+phi*lambda
      do {
        flg = 1;
        while (flg != 0 && (conver == 0)) {
          double da = phi * lambda;
          for (int j = 0; j < nprm; j++) {
            g[j] = -s_grad[j];
            int l1 = (j + 1) * (j + 2) / 2 - 1;
            am[l1] = s_c[l1] * (1.0 + lambda) + da;
            for (int i = 0; i < j; i++)
              am[l1 - i - 1] = s_c[l1 - i - 1];
          }
          flg = chodec(nprm); //    choleski decomposition
          if (flg != 0) {
            if (lambda < prcsn)
              lambda = prcsn;
            lambda *= 10.0;
            if (lambda > 100000.0 * lmin)
              conver = 1;
          }
        }
        if (conver == 0) {
          choback(nprm);
//         find new parameters b=d+g
//        (no counts the  of zero elements in g)
          n0 = 0;
          for (int i = 0; i < nprm; i++) {
            s_b[i] = s_deriv[i] + g[i];
            if (Math.abs(g[i]) <= Math.abs(prcsn * s_deriv[i]))
              ++n0;
            s_parmn[i] = (double) s_b[i];
          }
        }
        if (n0 == nprm)
          conver = 1;
        else {
          simpleFittingFunction.refreshFit(fit, s_parmn, controls);
          wss = getWSS(dta, fit, wgt);
/*          if (Double.toXRDcatString(wss).equalsIgnoreCase("NaN")) {
            wss = oldwss * 1.01;
            printf("Wrong parameters, recomputing...");
          } */
/*          if (outputEnabled) {
            printf("new Wss = ", wss);
            printf("actual iteration ", niter);
            for (int i = 0; i < dataNumber; i++)
              printf(dta[i], fit[i]);
          }*/
          if (wss < oldwss && wss >= 0.0) {
            oldwss = wss;
            for (int i = 0; i < nprm; i++)
              parm[i] = s_parmn[i];
          } else {
            if (check != 0) {
              oldwss = wss;
              wss = oldwss * 1.1;
            }
            if (lambda < prcsn)
              lambda = prcsn;
            lambda *= 10.0;
            if (lambda > 100000.0 * lmin)
              conver = 1;
          }
          if (wss < wssLimit)
            conver = 1;
        }
      } while ((wss > oldwss) && (conver == 0));
      oldwss = wss;
      if (conver == 0) {
        for (int i = 0; i < nprm; i++) {
          s_parmn[i] = parm[i];
          s_b[i] = parm[i];
        }
      }
    }       //   next iteration
    if (outputEnabled) {
      printf("Wss = ", wss);
      printf("Iterations ", niter);
      for (int i = 0; i < dataNumber; i++)
        printf(dta[i], fit[i]);
    }
    if (releaseMemory)
      releaseMemory();
    return wss;
  }

  public void computeDerivativeMatrix(int dataNumber, int nprm, double parm[],
                                      double derivf[][], double fit[], int[] controls) {
    //        compute matrix of derivative

    double dparp;
//    double secondfit[] = new double[dataNumber];

    for (int sp = 0; sp < nprm; sp++) {
      if (parm[sp] == 0)
        dparp = (double) derstep;
      else
        dparp = parm[sp] * (double) derstep;
//      double dparp2 = dparp * 2.0f;
      double oldpar = parm[sp];
      parm[sp] += dparp;
//      for (int i = 0; i < dataNumber; i++)
      simpleFittingFunction.refreshFit(s_firstfit, parm, controls);
/*      if (doubleder) {
        parm[sp] = oldpar - dparp;
        simpleFittingFunction.getFit(secondfit, parm, controls);
      }     */

      for (int i = 0; i < dataNumber; i++) {
//        if (doubleder)
//          derivf[i][sp] = ((firstfit[i] - secondfit[i]) / dparp2);
//        else
        derivf[i][sp] = ((s_firstfit[i] - fit[i]) / dparp);
      }
      parm[sp] = oldpar;
    }
  }

  public double getWSS(double[] dta, double[] fit, double[] wgt) {
    double wss = 0.0;
    for (int i = 0; i < dta.length; i++) {
      double diff = dta[i] - fit[i];
      wss += diff * diff * wgt[i] * wgt[i];
    }
    return wss;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    updateStringtoDoubleBuffering(false);
    JOptionsDialog adialog = new JLSSDPDOptionsD(parent, this);
    return adialog;
  }

  public class JLSSDPDOptionsD extends JOptionsDialog {

    JSlider iterationJS;
    JTextField[] parsTF = null;
    JCheckBox doubleDerivativeCB;

    public JLSSDPDOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel tfPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
      principalPanel.add(BorderLayout.NORTH, tfPanel);
      tfPanel.add(new JLabel("Iterations number: "));
      JLabel iterationTF = new JLabel();
      iterationJS = new JSlider();
      iterationJS.setToolTipText("Set the number of iterations during refinement (best values: 3-7)");
      SliderListener listener = new SliderListener(iterationTF);
      iterationJS.addChangeListener(listener);
      tfPanel.add(iterationTF);
      tfPanel.add(iterationJS);


      tfPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
      principalPanel.add(BorderLayout.CENTER, tfPanel);
      JPanel sPanel = new JPanel(new GridLayout(0, 2, 3, 3));
      tfPanel.add(sPanel);

      String[] labels = {
        "Parameter precision: ",
        "Derivative step    : ",
		    "Initial lambda     : "
      };

      int numberFields = labels.length;
      parsTF = new JTextField[numberFields];

      for (int i = 0; i < numberFields; i++) {
        sPanel.add(new JLabel(labels[i]));
        parsTF[i] = new JTextField(Constants.FLOAT_FIELD);
//        genTF.setToolTipText("Set the number of generations for the Genetic Algorithm");
        sPanel.add(parsTF[i]);
      }

      tfPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
      principalPanel.add(BorderLayout.SOUTH, tfPanel);
      doubleDerivativeCB = new JCheckBox("Use numerical double derivative");
      doubleDerivativeCB.setToolTipText("By selecting numerical double derivative will be used (slower computation)");
      tfPanel.add(doubleDerivativeCB);

      setTitle("Least squares options");
      initParameters();
      pack();
      iterationJS.setValue(getIterations());
    }

    public void initParameters() {
      int maxIterations = MaudPreferences.getInteger("analysis.maxIterationsSelectable", 21);
      iterationJS.setMaximum(maxIterations);
      iterationJS.setMinimum(1);
      iterationJS.setValue(maxIterations);
      iterationJS.setPaintTicks(true);
      iterationJS.setMajorTickSpacing(5);
      iterationJS.setMinorTickSpacing(1);
      iterationJS.setPaintLabels(true);
      iterationJS.setSnapToTicks(true);
      iterationJS.setLabelTable(iterationJS.createStandardLabels(5));

      for (int i = 0; i < parsTF.length; i++)
        parsTF[i].setText(stringField[i + 1]);

      doubleDerivativeCB.setSelected(isDerivative2());

    }

    public void retrieveParameters() {
      setIterations(iterationJS.getValue());
      for (int i = 0; i < parsTF.length; i++)
        stringField[i + 1] = parsTF[i].getText();
      setDerivative2(doubleDerivativeCB.isSelected());
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
}
