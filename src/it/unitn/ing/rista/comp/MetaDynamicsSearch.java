/*
 * @(#)MetaDynamicsSearch.java created Aug 29, 2005 Firenze, IUcr 2005
 *
 * Copyright (c) 1996-2005 Luca Lutterotti All Rights Reserved.
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
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.interfaces.Function;
import it.unitn.ing.rista.interfaces.SimpleFunction;
import it.unitn.ing.rista.awt.JOptionsDialog;

import java.util.*;
import java.io.OutputStream;
import java.awt.*;

import Jama.Matrix;

import javax.swing.*;

import ec.util.MersenneTwisterFast;


/**
 * The MetaDynamicsSearch is a class to provide an optimization
 * algorithm based on the Metadynamics method of Parrinello group.
 * <p/>
 * Description
 * The idea for such algorithm arrived after seeing the lecture of Laio at
 * the IUcr congress in Firenze.
 * The Metadynamics algorithm is based on the idea of filling a newly found
 * minimum in the solution space with a Gaussian function of a certain width
 * and intensity to be add as a cost function to prevent the system to
 * remain in such minimum. So other solutions can be investigated.
 * This implementation shares only the general idea, but was written without
 * looking at the original algorithm. So many details can be completely
 * differents.
 *
 * The implementation in Maud is based on the Marqardt least squares algorithm
 * also.
 * The user should first fix the minimum and maximum limit value for each
 * refinable parameter (used not to bound the solution but for generating
 * a new starting point in the least squares by shaking the system).
 * Then check the options in the metadynamics algorithm.
 * Starting a refinement, the program start from the actual parameters and
 * perform the number of cycles imposed.
 * For each cycle the algorithm performs a least squares refinement (Marqardt)
 * until the convergence or the number of iterations is reached (see the
 * options for both parameters value). Then the solution is stored and
 * ranked with the others (only a certain maximum number is stored, the
 * worser are rejected when there are more than the maximum).
 * At the end of each cycle after storing the solution (unless is equivalent
 * to a previous one) a Gaussian additional cost function is added to the
 * Weighted Sum of Squares centered on that solution.
 * A new random starting point (based on the parameter limits) is generated
 * to perform the next cycle.
 * At end the best solution is kept.
 *
 * Algorithm options:
 *
 * "Number of cycles" : number of cycles to be performed
 * "Number of iterations" : maximum number of LS iterations in each cycle
 * "Convergenge precision" : convergence is reached if each parameter change
 *                           less than the precision (relative value)
 * "Step for derivative computation" : relative step for numerical derivative
 *                                     computation in the LS
 * "Number of stored solutions" : max number of stored solutions
 * "Filling Gaussian normalized HWHM" : the Gaussian width of the filling cost
 *                                      function is computed by this value *
 *                                      parameter error. If the error is zero
 *                                      then equal to the value * 1000 *
 *                                      derivative step.
 * "Filling minima starting intensity" : initial intensity of a filling
 *                                       gaussian.
 * "Filling minima intensity increase step" : increment of intensity for the
 *                                            Gaussian cost function when
 *                                            an equivalent solution is
 *                                            found.
 * "Use double derivative" : use double numerical derivative in the LS
 * "Fill minima" : fill the minima with a Gaussian cost function. If false
 *                 the algorithm just scan the solution space by generating
 *                 each cycle a random starting point. No additional cost
 *                 function. Not in the original Metadynamics algorithm.
 *
 *
 * References
 *
 * A. Laio and M. PArrinello, Proc. Natl. Acad. Sci. USA 20, 12562 (2002).
 * M. Iannuzzi, A. Laio and M. Parrinello, Phys. Rev. Lett. 90, 238302 (2003).
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.3 $, $Date: 2006/02/02 16:11:56 $
 * @since JDK1.1
 */

public class MetaDynamicsSearch extends OptimizationAlgorithm {

  public static String[] diclistc = {
      "_riet_refine_md_number_cycles",
      "_refine_ls_number_iteration",
      "_riet_refine_ls_precision",
      "_riet_refine_ls_derivative_step",
      "_riet_refine_md_solutions",
      "_riet_refine_md_gaussian_norm_hwhm",
      "_riet_refine_md_intensity_start",
      "_riet_refine_md_intensity_step",
      "_riet_refine_ls_double_derivative",
      "_riet_refine_md_fill_minima"
  };
  public static String[] diclistcrm = {
      "Number of cycles",
      "Number of iterations",
      "Convergenge precision",
      "Step for derivative computation",
      "Number of stored solutions",
      "Filling Gaussian normalized HWHM",
      "Filling minima starting intensity",
      "Filling minima intensity increase step",
      "Use double derivative",
      "Fill minima"
  };
  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  int niter = 0;
  int ipflg = 0;
  int brkflg = 0;
  double derstep = 0.0001;
  boolean doubleder = false;
  double prcsn = 0.00000001;
  int n0 = 0;
  int flg;
  double am[] = null;
  double g[] = null;
  int[] choleskyFlag = null;
  MersenneTwisterFast randomizer = null;

  public boolean simplifyForm = false;

  public double wssLimit = 0.0;

  Vector multipleSolution = null;

  double firstWSS = 0.0;

  public MetaDynamicsSearch(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "MetaDynamics optimization";
    IDlabel = "MetaDynamics optimization";
    description = "select this to use the MetaDynamics-Marqardt Least Squares optimization";
  }

  public MetaDynamicsSearch(XRDcat aobj) {
    this(aobj, "MetaDynamics optimization");
  }

  public MetaDynamicsSearch() {
    identifier = "MetaDynamics optimization";
    IDlabel = "MetaDynamics optimization";
    description = "select this to use the MetaDynamics-Marqardt Least Squares optimization";
  }

  public void initConstant() {
    Nstring = 10;
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

  static final int cyclesIndex = 0, iterationsIndex = 1, precisionIndex = 2, derivateStepIndex = 3,
      solutionsNumberIndex = 4, HWHMnormIndex = 5, intensityIndex = 6, intensityStepIndex = 7, doubleDerIndex = 8,
      fillMinimaIndex = 9;

  public void initParameters() {
    super.initParameters();
    stringField[cyclesIndex] = MaudPreferences.getPref("metaDynamics.cycles", "50");
    stringField[iterationsIndex] = MaudPreferences.getPref("metaDynamics.iterations", "5");
    stringField[precisionIndex] = MaudPreferences.getPref("metaDynamics.precision", "0.00000001");
    stringField[derivateStepIndex] = MaudPreferences.getPref("leastSquares.derivativeStep", "0.0001");
    stringField[solutionsNumberIndex] = MaudPreferences.getPref("metaDynamics.numberOfSolutions", "20");
    stringField[HWHMnormIndex] = MaudPreferences.getPref("metaDynamics.GaussianHWHMnormalized", "1.0");
    stringField[intensityIndex] = MaudPreferences.getPref("metaDynamics.GaussianIntensityStart", "1.0");
    stringField[intensityStepIndex] = MaudPreferences.getPref("metaDynamics.IntensityIncreaseStep", "0.1");
    stringField[doubleDerIndex] = MaudPreferences.getPref("leastSquares.doubleDerivate", "false");
    stringField[fillMinimaIndex] = MaudPreferences.getPref("metaDynamics.fillMinima", "true");
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.setIterations(Integer.parseInt(stringField[iterationsIndex]));
//    MaudPreferences.setPref("metaDynamics.iterations", stringField[iterationsIndex]);
    derstep = Double.parseDouble(stringField[derivateStepIndex]);
    prcsn = Double.parseDouble(stringField[precisionIndex]);
    doubleder = isDerivative2();
  }

  public void setDerivateStep(double value) {
    if (value != 0) {
      derstep = value;
      stringField[derivateStepIndex] = Double.toString(value);
    }
  }

  public void setPrecision(double value) {
    if (value != 0.0) {
      prcsn = value;
      stringField[precisionIndex] = Double.toString(value);
    }
  }

  public void setDerivative2(boolean value) {
    doubleder = value;
    if (value)
      stringField[doubleDerIndex] = "true";
    else
      stringField[doubleDerIndex] = "false";
  }

  public boolean isDerivative2() {
    if (stringField[doubleDerIndex].equalsIgnoreCase("true"))
      return true;
    return false;
  }

  public boolean fillMinima() {
    if (stringField[fillMinimaIndex].equalsIgnoreCase("true"))
      return true;
    return false;
  }

  public void setIterations(int number) {
    super.setIterations(number);
    stringField[iterationsIndex] = Integer.toString(number);
  }

  public void setCycles(String cycles) {
    stringField[cyclesIndex] = cycles;
  }

  public String getCycles() {
    return stringField[cyclesIndex];
  }

  public void setSolutionNumber(String cycles) {
    stringField[solutionsNumberIndex] = cycles;
  }

  public String getMaxSolutionNumber() {
    return stringField[solutionsNumberIndex];
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

    Vector fitVector = null;
    Vector filler = new Vector(10, 10);
    multipleSolution = new Vector(Integer.parseInt(getMaxSolutionNumber()) + 1);
    double dta[] = null;
    double wgt[] = null;
    double fit[] = null;
    int parNumber = 0;

    if (newModel)
      parNumber = ((FilePar) fittingFunction).prepareIterationNewModel();
    else
      parNumber = fittingFunction.prepareIteration();
    int iterations = getIterations();
    if (computation != null && computation.shouldStop()) {
      return;
    }
    int dataNumber = fittingFunction.getNumberOfData() + 1;
    int nprm = fittingFunction.getNumberOfFreeParameters();
    double parm[] = new double[nprm];
    int mdi = mdi = (nprm + 1) * nprm / 2;

/* Memory allocation */

    double deriv[] = null;
    double b[] = null;
    double grad[] = null;
    double c[] = null;

    int maxCycles = Integer.parseInt(getCycles());
    for (int cycles = 0; cycles < maxCycles; cycles++) {

      fittingFunction.setDerivate(false);
    fittingFunction.computeFirstFit();

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
      for (int i = 0; i < dataNumber - 1; i++) {
        dta[i] = fittingFunction.getData(i);
        wgt[i] = fittingFunction.getWeight(i);
        wgt[i] *= wgt[i];
        fit[i] = fittingFunction.getFit(i);
      }
      dta[dataNumber - 1] = 0.0f;
      wgt[dataNumber - 1] = 1.0f;
      fit[dataNumber - 1] = 0.0f;
    }
    parm = new double[nprm];
    choleskyFlag = new int[nprm];
    for (int i = 0; i < nprm; i++)
      parm[i] = fittingFunction.getFreeParameter(i);
    double parmn[] = new double[nprm];

    if (computation != null && computation.shouldStop()) {
      return;
    }

/* Memory allocation */

    deriv = new double[nprm];
    b = new double[nprm];
    grad = new double[nprm];
    c = new double[mdi];

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

    fittingFunction.saveparameters();

    if (computation != null && computation.shouldStop()) {
      return;
    }

    double[][] derivf = null;

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
      MetaDynamicsFitContainer metadynamics = new MetaDynamicsFitContainer(filler);
      metadynamics.createDerivate(nprm);
      fitVector.addElement(metadynamics);
    }
//	  Assert.assert(derivf);


    am = new double[mdi];
    g = new double[nprm];
/*          next iteration      */
    firstWSS = 0.0;
      int conver = 0;
      double lambda = 0.01;
      double lmin = 1.0e+20;
      double phi = 1.0;
      niter = -1;
      int check = 0; // if check != 0 the new wss can be sligthly greater than the old one
      if (outputframe != null) {
        outputframe.getProgressBar().setProgressBarValue(
            (parNumber + 1) * iterations + 1);
        computation.setIterationSliderValue(iterations);
      }
      fittingFunction.setDerivate(true);
      double wss = 0.0, or_wss = 0.0;
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
        or_wss = wss;
        wss += getMetaDynamicWSS(parm, filler);
      }
      double oldwss = wss;
      printf("Wss = ", wss);
      printf("Cycle: " + cycles + ", Number of iterations: ", getIterations());
      randomizer = new MersenneTwisterFast((long) (wss * nprm));
      if (outputframe != null)
        outputframe.increaseProgressBarValue();

      if (computation != null && computation.shouldStop()) {
        return;
      }

      boolean flagfgl = (conver == 0) && (!computation.shouldStopIteration());
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

        computeDerivativeMatrix(dataNumber, nprm, parm, fitVector, derivf, fit, computation, filler);

        if (computation != null && computation.shouldStop()) {
          return;
        }

        for (int i = 0; i < mdi; i++)
          am[i] = 0.0;
        for (int i = 0; i < nprm; i++)
          grad[i] = 0.0;

        if (newModel) {

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
          if (computation != null && computation.shouldStop()) {
            return;
          }
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
              b[i] = deriv[i] + g[i];
              if (Math.abs(g[i]) <= Math.abs(prcsn * deriv[i]))
                ++n0;
              parmn[i] = (double) b[i];
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
                for (int i = 0; i < dataNumber - 1; i++)
                  fit[i] = fittingFunction.getFit(i);
                wss = fittingFunction.getWSS();
                fit[dataNumber - 1] = (double) Math.sqrt(getMetaDynamicWSS(parm, filler));
                wss += fit[dataNumber - 1] * fit[dataNumber - 1];
              } else {
                wss = 0.0;
                fittingFunction.computeFit();
	              fittingFunction.getFit();
                for (int i = 0; i < fitVector.size(); i++) {
                  SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVector.elementAt(i);
                  spectrum.checkFit();
                  wss += spectrum.getWSS();
                }
              }

              printf("Wgt'd ssq = ", wss);
              if (fittingFunction.singleFunctionComputing())
                fittingFunction.setDerivate(true);
              if (computation != null && computation.shouldStop()) {
                return;
              }
            }
            if (Double.isNaN(wss)) {
              wss = oldwss * 1.01;
              printf("Wrong parameters, recomputing...");
            }
            if (wss < oldwss && wss >= 0.0) {
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

        if (computation != null && computation.shouldStop()) {
          return;
        }
        fittingFunction.setDerivate(false);
        if (!fittingFunction.singleFunctionComputing()) {
          if (newModel) {
            fittingFunction.computeFirstFit();
            for (int i1 = 0; i1 < fitVector.size(); i1++) {
              SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVector.elementAt(i1);
              spectrum.checkFit();
            }
            wss = 0.0;
            or_wss = 0.0;
            for (int i = 0; i < fitVector.size(); i++) {
              SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVector.elementAt(i);
              double twss = spectrum.getWSS();
              wss += twss;
              if (i < fitVector.size() - 1)
                or_wss += twss;
            }
          } else {
             for (int i = 0; i < dataNumber - 1; i++)
              fit[i] = fittingFunction.getFit(i);
            wss = fittingFunction.getWSS();
            or_wss = wss;
            fit[dataNumber - 1] = (double) Math.sqrt(getMetaDynamicWSS(parm, filler));
            wss += fit[dataNumber - 1] * fit[dataNumber - 1];
          }

          printf("Wgt'd ssq = ", wss);
        }
        oldwss = wss;
        fittingFunction.setDerivate(true);
        flagfgl = (conver == 0) && (!computation.shouldStopIteration());
        if (conver == 0) {
          fittingFunction.saveparameters();
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
      double[] dparm = computeErrorsNoCholesky(nprm, dataNumber, niter, mdi, c, flg);
      if (firstWSS == 0.0)
        firstWSS = wss;
      checkSolution(parm, dparm, or_wss, filler);
      if (cycles < maxCycles) {
        parm = shakeSolution();

        fittingFunction.setFreeParameters(parm);
        fittingFunction.setDerivate(false);
        if (!fittingFunction.singleFunctionComputing()) {
          if (newModel) {
            fittingFunction.computeFirstFit();
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
             for (int i = 0; i < dataNumber - 1; i++)
              fit[i] = fittingFunction.getFit(i);
	          wss = fittingFunction.getWSS();
            fit[dataNumber - 1] = (double) Math.sqrt(getMetaDynamicWSS(parm, filler));
            wss += fit[dataNumber - 1] * fit[dataNumber - 1];
          }

          printf("Shaked Wgt'd ssq = ", wss);
        }
        oldwss = wss;
      }
    }
    finaloutput(nprm, dataNumber, niter, mdi, c, flg);
    fittingFunction.setFreeParameters(((Solution) multipleSolution.elementAt(0)).parm);
    double wss = 0.0;
    fittingFunction.setDerivate(false);
    if (!fittingFunction.singleFunctionComputing()) {
      if (newModel) {
        fittingFunction.computeFirstFit();
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
        for (int i = 0; i < dataNumber - 1; i++)
          fit[i] = fittingFunction.getFit(i);
        wss = fittingFunction.getWSS();
        fit[dataNumber - 1] = (double) Math.sqrt(getMetaDynamicWSS(parm, filler));
        wss += fit[dataNumber - 1] * fit[dataNumber - 1];
      }
      printf("Final Wgt'd ssq = ", wss);
    }
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
                                      launchBasic computation, Vector filler) {
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
      if (parm[sp] == 0)
        dparp = (double) derstep;
      else
        dparp = parm[sp] * (double) derstep;
      double dparp2 = dparp * 2.0f;
      double oldpar = parm[sp];
      double parm1 = parm[sp] + dparp;
      fittingFunction.setFreeParameter(sp, parm1);
      if (newModel) {
        fittingFunction.computeFit();
        for (int i1 = 0; i1 < fitVector.size(); i1++) {
          SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVector.elementAt(i1);
          spectrum.checkDerivateFit();
        }
      } else {
        for (int i = 0; i < dataNumber - 1; i++)
          firstfit[i] = fittingFunction.getFit(i);
        parm[sp] = parm1;
        fit[dataNumber - 1] = (double) Math.sqrt(getMetaDynamicWSS(parm, filler));
        parm[sp] = oldpar;
      }
      if (doubleder) {
        parm1 = oldpar - dparp;
        fittingFunction.setFreeParameter(sp, parm1);
        if (newModel) {
          fittingFunction.computeFit();
          for (int i1 = 0; i1 < fitVector.size(); i1++) {
            SpectrumFitContainer spectrum = (SpectrumFitContainer) fitVector.elementAt(i1);
            spectrum.checkDerivate2Fit();
          }
        } else {
          for (int i = 0; i < dataNumber - 1; i++)
            secondfit[i] = fittingFunction.getFit(i);
          parm[sp] = parm1;
          secondfit[dataNumber - 1] = (double) Math.sqrt(getMetaDynamicWSS(parm, filler));
          parm[sp] = oldpar;
        }
      }

      if (computation != null && computation.shouldStop()) {
        return;
      }

      if (outputframe != null)
        outputframe.increaseProgressBarValue();
      if (newModel) {
        fittingFunction.computeFit();
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
      fittingFunction.setFreeParameter(sp, oldpar);
    }
  }

  public double[] computeErrorsNoCholesky(int nprm, int dataNumber, int niter, int mdi, double c[], int flg) {
//    double wss;

    double[] dparm = new double[nprm];
    if (nprm > 0) {
      double[] rValues = fittingFunction.getRefinementIndexes();
      double sig = Math.sqrt(rValues[8] / (dataNumber - 1 - nprm));
      if (niter > 0) {
        for (int j = 0; j < mdi; j++)
          am[j] = c[j];
        flg = chodec(nprm);

        for (int z = 0; z < nprm; z++) {
          for (int j = 0; j < nprm; j++)
            g[j] = 0.0;
          g[z] = 1.0;
          choback(nprm);
          if (choleskyFlag[z] > 0 && !Double.isNaN(g[z]))
            dparm[z] = (double) (Math.sqrt(Math.abs(g[z])) * sig);
          else
            dparm[z] = 0.0f;
        }
      }
    }
    return dparm;
  }

  public void finaloutput(int nprm, int dataNumber, int niter, int mdi, double c[], int flg) {
//    double wss;

    if (nprm > 0) {
      double[] rValues = fittingFunction.getRefinementIndexes();
      double sig = Math.sqrt(rValues[8] / (dataNumber - 1 - nprm));
      printf("sig= ", sig);
      double Rw = rValues[0];
      double R = rValues[4];
//      double ss = rValues[9];
      if (Rw == 0.0)
        Rw = 1.0;
      if (R == 0.0)
        R = 1.0;
      double rsw = Rw * 100.0;
      printf("Rw (%) = ", rsw);
      printf("Rwnb (%) = ", rValues[1]);
	    printf("Rwnb1 (%) = ", rValues[2]);
	    printf("Rwnb2 (%) = ", rValues[3]);
      double rb = R * 100.0;
      printf("Rb (%) = ", rb);
      double rexp = Rw / sig;
      fittingFunction.setRexp(sig);
      printf("Rexp (%) = ", rexp * 100.0);
      printf("# iterations = ", niter);
      System.out.println("Generating correlation matrix");
      if (niter > 0) {
        for (int j = 0; j < mdi; j++)
          am[j] = c[j];

        if (fittingFunction instanceof FilePar && ((FilePar) fittingFunction).logOutput()) {
          OutputStream out = ((FilePar) fittingFunction).getResultStream();
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
        double[][] cov2 = new double[nprm][nprm];
        for (int i = 0; i < nprm; i++) {
          int l = (i + 1) * i / 2;
          for (int j = 0; j < i; j++) {
            cov1[i][j] = cov1[j][i] = am[l + j];
          }
        }
        if (fittingFunction instanceof FilePar && ((FilePar) fittingFunction).logOutput()) {
          OutputStream out = ((FilePar) fittingFunction).getResultStream();
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
        if (fittingFunction instanceof FilePar && ((FilePar) fittingFunction).logOutput()) {
          OutputStream out = ((FilePar) fittingFunction).getResultStream();
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

        double[] dparm = new double[nprm];
        for (int j = 0; j < mdi; j++)
          am[j] = c[j];
        flg = chodec(nprm);
        for (int z = 0; z < nprm; z++) {
          for (int j = 0; j < nprm; j++)
            g[j] = 0.0;
          g[z] = 1.0;
          choback(nprm);
          if (choleskyFlag[z] > 0 && !Double.isNaN(g[z]))
            dparm[z] = (double) (Math.sqrt(Math.abs(g[z])) * sig);
          else
            dparm[z] = choleskyFlag[z];
          printf(z, dparm[z]);
        }
        fittingFunction.setErrors(dparm);
      }
      fittingFunction.saveparameters();
    }
//  	niter += iter;
    if (flg == 1) {
      printf(" ");
      printf("cholesky negative diagonal :");
      printf("unable to solve with supplied initial parameters");
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

  public MetaDynamicsSearch(SimpleFunction fitFunction, int iterations) {
    super(fitFunction, iterations);
    newModel = false;
  }

  public double simpleSolve(double[] dta, double[] wgt, double[] fit, double[] parm, boolean releaseMemory,
                            int[] controls) {
//    updateStringtoDoubleBuffering();
    int dataNumber = dta.length;
    int nprm = parm.length;
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
      for (int i = 0; i < dataNumber; i++)
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
    JOptionsDialog adialog = new JMDSDPDOptionsD(parent, this);
    return adialog;
  }

  private double getMetaDynamicWSS(double[] parm, Vector filler) {
    double extraWSS = 0.0;
    if (!fillMinima())
      return extraWSS;
    for (int i = 0; i < filler.size(); i++)
      extraWSS += ((FillerMat) filler.elementAt(i)).getFillerAmount(parm);
    return extraWSS;
  }

  private void checkSolution(double[] parm, double[] dparm, double wss, Vector filler) {
    boolean isNewSolution = true;
    for (int i = 0; i < filler.size(); i++)
      if (((FillerMat) filler.elementAt(i)).isSimilarTo(parm, wss)) {
        isNewSolution = false;
        break;
      }
    if (isNewSolution) {
      filler.add(new FillerMat(parm, dparm, wss));
      multipleSolution.add(new Solution(parm, wss));
      Collections.sort(multipleSolution, new bestSolution());
      System.out.println("New solution, wss:" + wss);
//      for (int i = 0; i < parm.length; i++)
//        System.out.println(i + ": " + parm[i] + "(" + dparm[i] + ")");
      int max = Integer.parseInt(getMaxSolutionNumber());
      if (multipleSolution.size() > max) {
        multipleSolution.removeElementAt(max - 1);
      }

    }
  }

  private double[] shakeSolution() {
    int nprm = fittingFunction.getNumberOfFreeParameters();
    double[] parm = new double[nprm];
    for (int i = 0; i < nprm; i++) {
      double lbound = fittingFunction.getLowerBound(i);
      double ubound = fittingFunction.getUpperBound(i);
      parm[i] = (ubound - lbound) * (double) randomizer.nextDouble() + lbound;
    }
//    System.out.println("Shake solution: ");
//    for (int i = 0; i < parm.length; i++)
//      System.out.println(i + ": " + parm[i] + "(" + parm[i] + ")");
    return parm;
  }

  class FillerMat {
    double[] parm = null;
    double[] parmWidth = null;
    double intensity = 0.01;
    double step = 0.01;
    double WSS = 0.0;

    public FillerMat(double[] prms, double[] dparm, double wss) {
      int nprm = prms.length;
      parm = new double[nprm];
      parmWidth = new double[nprm];
      double HWHMnorm = Double.parseDouble(stringField[HWHMnormIndex]);
      for (int i = 0; i < nprm; i++) {
        parm[i] = prms[i];
        if (dparm[i] == 0) {
          if (parm[i] == 0)
            parmWidth[i] = (double) (derstep * 1000.0f * HWHMnorm);
          else
            parmWidth[i] = prms[i] * (double) (derstep * 1000.0f * HWHMnorm);
        } else
          parmWidth[i] = (double) (dparm[i] * HWHMnorm);
      }
      WSS = wss;
      intensity = Double.parseDouble(stringField[intensityIndex]);
      step = Double.parseDouble(stringField[intensityStepIndex]);
    }

    public void increaseIntensity() {
      intensity += step;
    }

    public double getFillerAmount(double[] parms) {
      double amount = 0.0;
      double arg = 0.0;
      double divider = 0.0;
      int nprm = parms.length;
      for (int i = 0; i < nprm; i++) {
        double diff = parms[i] - parm[i];
        arg += diff * diff;
        divider += parmWidth[i] * parmWidth[i];
      }
      if (arg > 10.0)
        amount = 0.0;
      else
        amount = intensity * Constants.sqrtln2pi * Math.exp(-Constants.LN2 * arg / divider) /
            Math.sqrt(divider);
      return WSS * amount;
    }

    public boolean isSimilarTo(double[] prms, double wss) {
      boolean isSimilar = false;
//      return isSimilar;
      int nprm = prms.length;
      double arg = 0.0;
      double divider = 0.0;
      for (int i = 0; i < nprm; i++) {
        double diff = prms[i] - parm[i];
        arg += diff * diff;
        divider += parmWidth[i] * parmWidth[i];
      }
      if (arg * 3 < divider && wss > WSS) {
        isSimilar = true;
        increaseIntensity();
      }
      return isSimilar;
    }
  }

  class Solution {
    public double[] parm = null;
    public double WSS = 0.0;

    public Solution(double[] prms, double wss) {
      int nprm = prms.length;
      parm = new double[nprm];
      for (int i = 0; i < nprm; i++)
        parm[i] = prms[i];
      WSS = wss;
    }
  }

  class bestSolution implements Comparator {
    public int compare(Object obj1, Object obj2) {
      double WSS1 = ((Solution) obj1).WSS;
      double WSS2 = ((Solution) obj2).WSS;

      if (WSS2 == WSS1) {
        return 0;
      } else if (WSS1 < WSS2)
        return -1;
      return 1;
    }
  }

  public class JMDSDPDOptionsD extends JOptionsDialog {

//    JSlider iterationJS;
    JComponent[] parsTF = null;

    public JMDSDPDOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel gridPanel = new JPanel(new GridLayout(0, 1));
      principalPanel.add(BorderLayout.CENTER, gridPanel);

      JPanel tfPanel;
      parsTF = new JComponent[Nstring];
      for (int i = 0; i < Nstring; i++) {
        tfPanel = new JPanel();
        if (i == cyclesIndex || i == iterationsIndex)
          parsTF[i] = WindowUtilities.createSlider(tfPanel, stringField[i], diclistcrm[i], 1, 100);
        else
          parsTF[i] = WindowUtilities.createComponent(tfPanel, stringField[i], diclistcrm[i]);
        tfPanel.add(parsTF[i]);
        gridPanel.add(tfPanel);
      }

      setHelpFilename("metadynamics.txt");
      setTitle("Meta Dynamics options");
      initParameters();
      pack();
    }

    public void initParameters() {
      for (int i = 0; i < parsTF.length; i++) {
        WindowUtilities.setJComponentContent(parsTF[i], stringField[i]);
      }
    }

    public void retrieveParameters() {
      for (int i = 0; i < parsTF.length; i++)
        stringField[i] = WindowUtilities.getJComponentContent(parsTF[i]);
    }
  }

  public class MetaDynamicsFitContainer extends SpectrumFitContainer {

    double[] tparm = null;
    Vector filler = null;

    public MetaDynamicsFitContainer(Vector afiller) {
      dataNumber = 1;
      dta = new double[dataNumber];
      wgt = new double[dataNumber];
      wgt2 = new double[dataNumber];
      tparm = new double[fittingFunction.getNumberOfFreeParameters()];
      for (int j = 0; j < dataNumber; j++) {
        wgt[j] = 1.0f;
        wgt2[j] = wgt[j] * wgt[j];
        dta[j] = 0.0f;
      }
      filler = afiller;
      checkFit();
    }

    public void checkFit() {
      fit = new double[dataNumber];
      for (int i = 0; i < tparm.length; i++)
        tparm[i] = fittingFunction.getFreeParameter(i);
      fit[0] = (double) Math.sqrt(getMetaDynamicWSS(tparm, filler));
    }

    public double getWSS() {
      double WSS = 0.0;
      double diff;
      checkFit();

      for (int i = 0; i < dataNumber; i++) {
        diff = (fit[i] - dta[i]) * wgt[i];
        WSS += diff * diff;
      }
//    if (Constants.testing)
//      System.out.println("WSS energy = " + WSS);
      return WSS;
    }

    public void checkDerivateFit() {
      derfit = new double[dataNumber];
      checkFit();
      derfit[0] = fit[0];
    }

    public void checkDerivate2Fit() {
      der2fit = new double[dataNumber];
      checkFit();
      der2fit[0] = fit[0];
    }

    public double[] computeDerivate(double dpar) {
      double sum = 0.0;
      double[] deriv = new double[0];
        deriv = new double[dataNumber];
        for (int i = 0; i < dataNumber; i++) {
          deriv[i] = (derfit[i] - fit[i]) / dpar;
          sum += Math.abs(deriv[i]);
        }
        if (sum == 0.0)
          deriv = new double[0];
      derfit = null;
      der2fit = null;
      return deriv;
    }

    public double[] computeDerivate2(double dpar) {
      double sum = 0.0;
      double[] deriv = new double[0];
        deriv = new double[dataNumber];
        for (int i = 0; i < dataNumber; i++) {
          deriv[i] = (derfit[i] - der2fit[i]) / dpar;
          sum += Math.abs(deriv[i]);
        }
        if (sum == 0.0)
          deriv = new double[0];
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
      if (derivate != null && derivate.size() > 0)
        derivate.removeAllElements();
      derivate = null;
    }

  }
}
