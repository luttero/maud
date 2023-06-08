/*
 * @(#)SimplexMethodRefinement.java created Feb 26, 2006 Casalino
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

import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.diffr.FilePar;
import it.unitn.ing.rista.interfaces.Function;
import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.util.Constants;

import javax.swing.*;
import java.awt.*;


/**
 * The SimplexMethodRefinement is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:04 $
 * @since JDK1.1
 */

public class SimplexMethodRefinement extends OptimizationAlgorithm {

  protected int nParam;
  protected double startWSS;
  protected double[] startParams;			// starting solution parameters

  protected double[] parentParams;		// parent solution parameters
  protected double[] newParams;				// new trial solution parameters
  protected double[] bestParams;    	// best solution parameters

  protected double[] lParamBound;
  protected double[] uParamBound;

  protected double parentFitness;
  protected double bestWSS;

  public static String[] diclistc = {"_riet_simplex_trial_number", "_riet_simplex_convergence_tolerance",
                                    "_riet_simplex_extension_coeff", "_riet_simplex_contraction_coeff",
                                    "_riet_simplex_reflection_coeff", "_riet_simplex_randomize_start"};
  public static String[] diclistcrm = {"_riet_simplex_trial_number", "_riet_simplex_convergence_tolerance",
                                    "_riet_simplex_extension_coeff", "_riet_simplex_contraction_coeff",
                                    "_riet_simplex_reflection_coeff", "_riet_simplex_randomize_start"};
  public static String[] classlistc = {};
  public static String[] classlistcs = {};


  public SimplexMethodRefinement(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Simplex (Nelder-Mead) Refinement";
    IDlabel = "Simplex (Nelder-Mead) Refinement";
    description = "select this to use Simplex (Nelder-Mead) method";
  }

  public SimplexMethodRefinement(XRDcat aobj) {
    this(aobj, "Simplex (Nelder-Mead) Refinement");
  }

  public SimplexMethodRefinement() {
    identifier = "Simplex (Nelder-Mead) Refinement";
    IDlabel = "Simplex (Nelder-Mead) Refinement";
    description = "select this to use Simplex (Nelder-Mead) method";
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
    setnTrial("300");
    setConvergenceTolerance("1.0E-13");
    setExtensionCoeff("1.6667");
    setContractionCoeff("0.6667");
    setReflectionCoeff("1.0");
    setRandomizeStart(false);
  }

  public void solve(launchBasic computation, Function funtionTominimize) {

    fittingFunction = funtionTominimize;

    if (computation != null && computation.shouldStop()) {
      return;
    }

    initAll(computation);

    if (computation != null && computation.shouldStop()) {
      return;
    }

    int nTrial = Integer.parseInt(getnTrial());
    NelderMeadSimplex simplex = new NelderMeadSimplex(this);
    simplex.rCoeff = Double.parseDouble(getReflectionCoeff());
    simplex.eCoeff = Double.parseDouble(getExtensionCoeff());
    simplex.cCoeff = Double.parseDouble(getContractionCoeff());
    simplex.randomizeStart = randomizeStart();
    double fTol = Double.parseDouble(getConvergenceTolerance());
    double[] step = new double[startParams.length];
    for (int i = 0; i < startParams.length; i++) {
      step[i] = (uParamBound[i] - lParamBound[i]) / 2.0;
      startParams[i] = lParamBound[i] + step[i] / 2.0;
      printf("Starting parameter, step : " + startParams[i] + ", " + step[i]);
    }
    simplex.nelderMead(funtionTominimize, startParams, step, fTol, nTrial);
    fittingFunction.setFreeParameters(bestParams);
	  fittingFunction.computeFit();
	  fittingFunction.getFit();
    double wss = fittingFunction.getWSS();
    if (fittingFunction instanceof FilePar)
      ((FilePar) fittingFunction).updatePlot();
  }

  void initAll(launchBasic computation) {

    int nTrial = Integer.parseInt(getnTrial());

    fittingFunction.prepareIteration();
    if (outputframe != null) {
      outputframe.getProgressBar().setProgressBarValue(nTrial);
      computation.hideIterationPanel();
    }

    fittingFunction.computeFirstFit();
    fittingFunction.getFit();

    if (computation != null && computation.shouldStop()) {
      return;
    }

    startWSS = fittingFunction.getWSS();
    nParam = fittingFunction.getNumberOfFreeParameters();
    startParams = new double[nParam];
    lParamBound = new double[nParam];
    uParamBound = new double[nParam];
    for (int i = 0; i < nParam; i++) {
      startParams[i] = fittingFunction.getFreeParameter(i);
      lParamBound[i] = ((FilePar) fittingFunction).getLowerBound(i);
      uParamBound[i] = ((FilePar) fittingFunction).getUpperBound(i);
      printf("Parameter, min, max : ", startParams[i], lParamBound[i], uParamBound[i]);
    }

    printf("Initial Fitness = ", startWSS);
    if (computation != null && computation.shouldStop()) {
      return;
    }

//    fittingFunction.setDerivate(true);
    if (computation != null && computation.shouldStop()) {
      return;
    }

    bestParams = new double[startParams.length];
    bestWSS = 1.0E50;
    for (int i = 0; i < bestParams.length; i++)
      bestParams[i] = startParams[i];

    parentFitness = 1.0E50;

  }

  public void updateSolution(double[] bestParm, double ynewlo) {
    fittingFunction.setFreeParameters(bestParm);
	  fittingFunction.computeFit();
	  fittingFunction.getFit();
    double wss = fittingFunction.getWSS();
    if (outputframe != null)
      outputframe.increaseProgressBarValue();

    if (wss < bestWSS) {
      bestWSS = wss;
      printf("Parameters values:");
      printout(bestParm, bestParm.length);
      printf("Actual best wss :", wss);
      for (int i = 0; i < bestParams.length; i++) {
        bestParams[i] = bestParm[i];
      }
      if (fittingFunction instanceof FilePar)
        ((FilePar) fittingFunction).updatePlot();
    }
  }

  public String getnTrial() {
    return stringField[0];
  }

  public void setnTrial(String nTrial) {
    stringField[0] = nTrial;
  }

  public String getConvergenceTolerance() {
    return stringField[1];
  }

  public void setConvergenceTolerance(String value) {
    stringField[1] = value;
  }

  public String getExtensionCoeff() {
    return stringField[2];
  }

  public void setExtensionCoeff(String value) {
    stringField[2] = value;
  }

  public String getContractionCoeff() {
    return stringField[3];
  }

  public void setContractionCoeff(String value) {
    stringField[3] = value;
  }

  public String getReflectionCoeff() {
    return stringField[4];
  }

  public void setReflectionCoeff(String value) {
    stringField[4] = value;
  }

  public boolean randomizeStart() {
    return (stringField[1].equalsIgnoreCase("true"));
  }

  public void setRandomizeStart(boolean value) {
    if (value)
      stringField[5] = "true";
    else
      stringField[5] = "false";
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    return (JOptionsDialog) new JSMSDPDOptionsD(parent, this);
  }

  public class JSMSDPDOptionsD extends JOptionsDialog {

    JTextField[] parsTF = null;
    JCheckBox randomizeCB = null;

    public JSMSDPDOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel tfPanel = new JPanel();
      tfPanel.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.CENTER, tfPanel);

      String[] labels = { "Max number of iterations: ",
                          "Converge tolerance      : ",
                          "Extension coefficient   : ",
                          "Contraction coefficient : ",
                          "Reflection coefficient  : "
      };

      int numberFields = labels.length;
      parsTF = new JTextField[numberFields];

      for (int i = 0; i < numberFields; i++) {
        tfPanel.add(new JLabel(labels[i]));
        parsTF[i] = new JTextField(Constants.FLOAT_FIELD);
//        genTF.setToolTipText("Set the number of generations for the Genetic Algorithm");
        tfPanel.add(parsTF[i]);
      }
      randomizeCB = new JCheckBox("Randomize starting vertices");
      tfPanel.add(randomizeCB);

      setTitle("Simplex method (Nelder-Mead) options panel");
      initParameters();
      pack();
    }

    public void initParameters() {
      for (int i = 0; i < parsTF.length; i++)
        parsTF[i].setText(stringField[i]);
      randomizeCB.setSelected(randomizeStart());
    }

    public void retrieveParameters() {
      for (int i = 0; i < parsTF.length; i++)
        stringField[i] = parsTF[i].getText();
      setRandomizeStart(randomizeCB.isSelected());
    }

  }
}
