/*
 * @(#)SimulatedAnnealingRefinement.java created 16/04/2003 Trento
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

import it.unitn.ing.rista.diffr.FilePar;
import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.interfaces.Function;
import it.unitn.ing.rista.util.Misc;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.ItemListener;
import java.awt.event.ItemEvent;

/**
 *  The SimulatedAnnealingRefinement is a method to refine the spectrum
 *  using Simulated Annealing
 *
 *
 * @version $Revision: 1.22 $, $Date: 2006/07/20 13:39:03 $
 * @author Mauro Bortolotti
 * @since JDK1.1
 */

public class SimulatedAnnealingRefinement extends OptimizationAlgorithm {


  protected launchBasic m_computation = null;
  ec.util.MersenneTwisterFast randomizer = null;


  protected int nParam;
  protected double startWSS;
  protected double[] startParams;			// starting solution parameters

  protected double[] parentParams;		// parent solution parameters
  protected double[] newParams;				// new trial solution parameters
  protected double[] bestParams;    	// best solution parameters

  protected double[] lParamBound;
  protected double[] uParamBound;

  protected double parentFitness;
  protected double bestFitness;

  protected double DEFAULT_BOLTZMANN = 1.0;
  protected double K = DEFAULT_BOLTZMANN;						// Boltzmann - type constant

  protected double[] coolingSchedule;
  protected double[] mutationSchedule;

  public static String[] scheduleType = {
    "Constant",
    "Linear",
    "Geometric",
    "Exponential"
  };


  public static String[] diclistc = {"_riet_sa_temperature_step", "_riet_sa_trial_number",
                                     "_riet_sa_temperature_const", "_riet_sa_temperature_max", "_riet_sa_temperature_min",
                                     "_riet_sa_mutation_const", "_riet_sa_mutation_max", "_riet_sa_mutation_min",
                                     "_riet_sa_temperature_schedule", "_riet_sa_mutation_schedule",
                                     "_riet_sa_use_constraints", "_riet_sa_init_random",
                                     "_riet_sa_smart_scheduling", "_riet_sa_smart_acceptance", "_riet_sa_smart_size"
  };
  public static String[] diclistcrm = {"_riet_sa_temperature_step", "_riet_sa_trial_number",
                                     "_riet_sa_temperature_const", "_riet_sa_temperature_max", "_riet_sa_temperature_min",
                                     "_riet_sa_mutation_const", "_riet_sa_mutation_max", "_riet_sa_mutation_min",
                                     "_riet_sa_temperature_schedule", "_riet_sa_mutation_schedule",
                                     "_riet_sa_use_constraints", "_riet_sa_init_random",
                                     "_riet_sa_smart_scheduling", "_riet_sa_smart_acceptance", "_riet_sa_smart_size"
  };
  public static String[] classlistc = {};
  public static String[] classlistcs = {};


  public SimulatedAnnealingRefinement(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Simulated Annealing Refinement";
    IDlabel = "Simulated Annealing Refinement";
    description = "select this to use Simulated Annealing ";
  }

  public SimulatedAnnealingRefinement(XRDcat aobj) {
    this(aobj, "Simulated Annealing Refinement");
  }

  public SimulatedAnnealingRefinement() {
    identifier = "Simulated Annealing Refinement";
    IDlabel = "Simulated Annealing Refinement";
    description = "select this to use Simulated Annealing ";
  }

  public void initConstant() {
    Nstring = 15;
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
    setnStep("10");
    setnTrial("100");
    setTConst("0.9");
    setTStart("100000");
    setTEnd("1");
    setMutConst("0.9");
    setMutStart("1");
    setMutEnd("0.001");
    setSmartAcceptance(".8");
    setSmartSize("100");
  }

  void initRandomizer() {

//    iseed = 2 * ((int) secnds_(0.0)) + 1;

/*  run the random number generator 100 times
    for avoiding affects of starting value */

    int time = (int) System.currentTimeMillis();  // safe because we're getting low-order bits
    randomizer = new ec.util.MersenneTwisterFast(time);

    return;
  }

  /**
   * return value between 0 and 1 escluded
   *
   * @return
   */

  double randomGenerator() {
    double random = randomizer.nextDouble();
    while (random == 1.0)  // escluding 1.0
      random = randomizer.nextDouble();
    return random;
  }

  /**
   * return value between min and max excluded
   *
   * @param min
   * @param max
   * @return
   */
  double randomGenerator(double min, double max) {
    return min + (max - min) * randomGenerator();
  }

  protected double[] generateSchedule(String type, double vConst, double vStart, double vEnd, int nStep) {

    double[] Schedule = new double[nStep];

    if (type == "Constant") {
      for (int i = 0; i < nStep; i++)
        Schedule[i] = vConst;
    } else if (type == "Linear") {
      for (int i = 0; i < nStep; i++)
        Schedule[i] = (vStart - (vStart - vEnd) * ((double) i / nStep));
    } else if (type == "Geometric") {
      for (int i = 0; i < nStep; i++)
        Schedule[i] = (double) (vStart * Math.pow(vConst, i));
    } else if (type == "Exponential") {
      for (int i = 0; i < nStep; i++)
        Schedule[i] = (double) (vStart * Math.pow(vEnd / vStart, (double) i / nStep));
    }
    return Schedule;
  }

  protected double smartBoltzmann() {

    printf("Smart scheduling calibration - Please wait...");

    int nSmart = Integer.parseInt(getSmartSize());
    double Accept = Double.parseDouble(getSmartAcceptance());
    double smartParams[] = new double[nParam];
    double fitnessValues[] = new double[nSmart];
    double delta = Double.parseDouble(getMutStart());

    for (int ns = 0; ns < nSmart; ns++) {
      if (getUseConstraintsB()) {
        for (int np = 0; np < nParam; np++) {
          double newValue = delta * randomGenerator(lParamBound[np], uParamBound[np]);
          if (newValue < lParamBound[np])
            smartParams[np] = lParamBound[np];
          else if (newValue > uParamBound[np])
            smartParams[np] = lParamBound[np];
          else
            smartParams[np] = newValue;
        }
      } else
        for (int np = 0; np < nParam; np++)
          smartParams[np] = randomGenerator(-delta / 2, delta / 2);
      fittingFunction.setFreeParameters(smartParams);
      fittingFunction.computeFit();
	    fittingFunction.getFit();
      fitnessValues[ns] = fittingFunction.getWSS();

    }

    double FitnessSUM = 0;
    for (int ns = 0; ns < nSmart; ns++) {
      FitnessSUM += fitnessValues[ns];
    }
    //printf("FitnessSUM: " + FitnessSUM);
    double FitnessMEAN = FitnessSUM / nSmart;
    //printf("FitnessMEAN: " + FitnessMEAN);
    double T = coolingSchedule[0];

    double KB = -(FitnessMEAN / (T * Math.log(Accept)));
    printf("K Boltzmann: " + KB);

    return KB;
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
//  Generate the starting solution configuration
    generateStartingSolutions();

    if (computation != null && computation.shouldStop()) {
      return;
    }
    startSolutionLoop();
  }

  void initAll(launchBasic computation) {

    m_computation = computation;

    // getFilePar().prepareComputation();

    int nStep = Integer.parseInt(getnStep());
    int nTrial = Integer.parseInt(getnTrial());

    fittingFunction.prepareIteration();
    if (outputframe != null) {
      outputframe.getProgressBar().setProgressBarValue(nStep * nTrial);
      computation.hideIterationPanel();
    }

    fittingFunction.computeFirstFit();
    fittingFunction.getFit();
    initRandomizer();

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
    bestFitness = 1.0E50;
    for (int i = 0; i < bestParams.length; i++)
      bestParams[i] = startParams[i];

    parentFitness = 1.0E50;

  }

  void generateStartingSolutions() {
    if (getInitRandomB())
      generateRandomConfiguration();
    else
      pickLastSolution();
  }

  /**
   * randomize starting parameters using optional user-defined constraints
   */

  void generateRandomConfiguration() {
    parentParams = new double[nParam];
    newParams = new double[nParam];
    if (getUseConstraintsB()) {
      for (int i = 0; i < nParam; i++)
        parentParams[i] = lParamBound[i] + (uParamBound[i] - lParamBound[i]) * randomGenerator();
    } else {
      for (int i = 0; i < nParam; i++)
        parentParams[i] = randomGenerator();
    }
  }

  void pickLastSolution() {
    parentParams = new double[nParam];
    newParams = new double[nParam];
    for (int i = 0; i < nParam; i++)
      parentParams[i] = startParams[i];
  }

  void startSolutionLoop() {

    int nStep = Integer.parseInt(getnStep());
    int nTrial = Integer.parseInt(getnTrial());

    printf("Temperature steps : ", nStep);
    printf("Trial size : ", nTrial);

    coolingSchedule = generateSchedule(
            getTempSchedule(),
            Double.parseDouble(getTConst()),
            Double.parseDouble(getTStart()),
            Double.parseDouble(getTEnd()),
            nStep);

    mutationSchedule = generateSchedule(
            getMutSchedule(),
            Double.parseDouble(getMutConst()),
            Double.parseDouble(getMutStart()),
            Double.parseDouble(getMutEnd()),
            nStep);

    for (int i = 0; i < coolingSchedule.length; i++) {
      printf("coolingSchedule[" + i + "] = " + coolingSchedule[i]);
    }

    for (int i = 0; i < mutationSchedule.length; i++) {
      printf("mutationSchedule[" + i + "] = " + mutationSchedule[i]);
    }


    // Smart Scheduling

    if (getSmartSchedulingB()) {
      System.out.println("K = smartBoltzmann();");
      K = smartBoltzmann();
    } else
      K = DEFAULT_BOLTZMANN;


    for (int ns = 0; ns < nStep; ns++) {
      double T = coolingSchedule[ns];							// the current temperature
      for (int nt = 0; nt < nTrial; nt++) {

        if (m_computation != null && m_computation.shouldStop()) {
          return;
        }

        double delta = mutationSchedule[ns];  		// mutation range
        moveParentSolution(delta);								// Monte Carlo step
        double newFitness = getFitness(newParams);

        if (newFitness < parentFitness) { 				// store as parent solution
          printf("Improved parent solution");
          parentFitness = newFitness;
          for (int np = 0; np < nParam; np++)
            parentParams[np] = newParams[np];
          if (parentFitness < bestFitness) {			// store as best solution
            printf("Found better solution");
            printf("Parameters values:");
            printout(newParams, nParam);
            printf("Current best fitness :", newFitness);
            bestFitness = parentFitness;
            for (int np = 0; np < nParam; np++)
              bestParams[np] = parentParams[np];
          }
        } else { 		// Statistically accept new solution even if newFitness >= parentFitness
          double p = Math.exp(-(newFitness - parentFitness) / (K * T));
          double r = randomGenerator();
          if (p > r) {
            printf("Statistically accepted worse solution");
            parentFitness = newFitness;
            for (int np = 0; np < nParam; np++)
              parentParams[np] = newParams[np];
          }
        }
      }
    }/**/
  }

  private void moveParentSolution(double delta) {
    if (getUseConstraintsB()) {
      for (int np = 0; np < nParam; np++) {
        double newValue = parentParams[np] + delta * randomGenerator((lParamBound[np] - parentParams[np]), (uParamBound[np] - parentParams[np]));
        if (newValue < lParamBound[np])
          newParams[np] = lParamBound[np];
        else if (newValue > uParamBound[np])
          newParams[np] = lParamBound[np];
        else
          newParams[np] = newValue;
      }
    } else
      for (int np = 0; np < nParam; np++)
        newParams[np] = parentParams[np] + randomGenerator(-delta / 2, delta / 2);
  }

  public double getFitness(double[] params) {
    fittingFunction.setFreeParameters(params);
	  fittingFunction.computeFit();
	  fittingFunction.getFit();
    double Fitness = fittingFunction.getWSS();

    if (outputframe != null)
      outputframe.increaseProgressBarValue();

    return Fitness;
  }

  public String getnStep() {
    return stringField[0];
  }

  public void setnStep(String nStep) {
    stringField[0] = nStep;
  }

  public String getnTrial() {
    return stringField[1];
  }

  public void setnTrial(String nTrial) {
    stringField[1] = nTrial;
  }

  public String getTConst() {
    return stringField[2];
  }

  public void setTConst(String TConst) {
    stringField[2] = TConst;
  }

  public String getTStart() {
    return stringField[3];
  }

  public void setTStart(String TStart) {
    stringField[3] = TStart;
  }

  public String getTEnd() {
    return stringField[4];
  }

  public void setTEnd(String TEnd) {
    stringField[4] = TEnd;
  }

  public String getMutConst() {
    return stringField[5];
  }

  public void setMutConst(String mutConst) {
    stringField[5] = mutConst;
  }

  public String getMutStart() {
    return stringField[6];
  }

  public void setMutStart(String mutStart) {
    stringField[6] = mutStart;
  }

  public String getMutEnd() {
    return stringField[7];
  }

  public void setMutEnd(String mutEnd) {
    stringField[7] = mutEnd;
  }

  public String getTempSchedule() {
    return stringField[8];
  }

  public void setTempSchedule(String TempSchedule) {
    stringField[8] = TempSchedule;
  }

  public String getMutSchedule() {
    return stringField[9];
  }

  public void setMutSchedule(String MutSchedule) {
    stringField[9] = MutSchedule;
  }

  public String getUseConstraints() {
    return stringField[10];
  }

  public boolean getUseConstraintsB() {
    if (stringField[10] == "true")
      return true;
    return false;
  }

  public void setUseConstraints(String UseConstraints) {
    stringField[10] = UseConstraints;
  }

  public String getInitRandom() {
    return stringField[11];
  }

  public boolean getInitRandomB() {
    if (stringField[11] == "true")
      return true;
    return false;
  }

  public void setInitRandom(String InitRandom) {
    stringField[11] = InitRandom;
  }

  public boolean getSmartSchedulingB() {
    if (stringField[12] == "true")
      return true;
    return false;
  }

  public void setSmartScheduling(String SmartScheduling) {
    stringField[12] = SmartScheduling;
  }

  public void setSmartAcceptance(String SmartAcceptance) {
    stringField[13] = SmartAcceptance;
  }

  public String getSmartAcceptance() {
    return stringField[13];
  }

  public void setSmartSize(String SmartSize) {
    stringField[14] = SmartSize;
  }

  public String getSmartSize() {
    return stringField[14];
  }


  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new SimulatedAnnealingRefinement.AnnealingOptionsD(parent, this);
    return adialog;
  }

  public class AnnealingOptionsD extends JOptionsDialog implements ActionListener, ItemListener {

    // Variables declaration

    private javax.swing.JPanel stepPanel;
    private javax.swing.JPanel schedulePanel;
    private javax.swing.JPanel leftPanel;

    private javax.swing.JTextField textStep;
    private javax.swing.JTextField textTrial;
    private javax.swing.JComboBox comboMutSchedule;
    private javax.swing.JComboBox comboTempSchedule;

    private javax.swing.JCheckBox checkRandomize;
    private javax.swing.JCheckBox checkUseConstraints;

    private javax.swing.JPanel smartPanel;
    private javax.swing.JCheckBox checkSmartScheduling;
    private javax.swing.JTextField textSmartAcceptance;
    private javax.swing.JTextField textSmartSize;

    private javax.swing.JPanel rightPanel;
    private javax.swing.JPanel checkPanel;

    private javax.swing.JPanel boundPanel;
    private javax.swing.JTextField textMutConst;
    private javax.swing.JTextField textMutStart;
    private javax.swing.JTextField textMutEnd;
    private javax.swing.JTextField textTempConst;
    private javax.swing.JTextField textTempStart;
    private javax.swing.JTextField textTempEnd;


    public AnnealingOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);
      initComponents();
      initParameters();

    }

    private void initComponents() {
      leftPanel = new JPanel(new GridLayout(3, 1, 5, 5));
//leftPanel = new JPanel(new java.awt.BorderLayout());
      stepPanel = new JPanel(new GridLayout(2, 2, 5, 5));
      textStep = new JTextField();
      textTrial = new JTextField();
      schedulePanel = new JPanel(new GridLayout(2, 2, 5, 5));
      comboTempSchedule = new JComboBox(scheduleType);
      comboTempSchedule.setActionCommand("COMBO_TEMPERATURE");
      comboTempSchedule.addActionListener(this);
      comboMutSchedule = new JComboBox(scheduleType);
      comboMutSchedule.setActionCommand("COMBO_MUTATION");
      comboMutSchedule.addActionListener(this);
      checkPanel = new JPanel(new GridLayout(2, 1, 5, 5));
      checkRandomize = new JCheckBox("Randomize starting parameters");
      checkUseConstraints = new JCheckBox("Use parameters constraints");
      smartPanel = new JPanel(new GridLayout(3, 2, 5, 5));
      checkSmartScheduling = new JCheckBox();
      checkSmartScheduling.addItemListener(this);
      textSmartAcceptance = new JTextField();
      textSmartSize = new JTextField();
      rightPanel = new JPanel();
      boundPanel = new JPanel();
      textTempConst = new JTextField();
      textTempStart = new JTextField();
      textTempEnd = new JTextField();
      textMutConst = new JTextField();
      textMutStart = new JTextField();
      textMutEnd = new JTextField();

      setResizable(false);

      //principalPanel.setLayout(new java.awt.GridLayout(1, 2));

      leftPanel.setBorder(new javax.swing.border.EtchedBorder());

      stepPanel.setBorder(new javax.swing.border.EmptyBorder(new java.awt.Insets(5, 5, 5, 5)));
      stepPanel.add(new JLabel("Temperature steps"));
      stepPanel.add(textStep);
      stepPanel.add(new JLabel("Trial number"));
      stepPanel.add(textTrial);

      leftPanel.add(stepPanel);

      schedulePanel.setBorder(new javax.swing.border.EmptyBorder(new java.awt.Insets(5, 5, 5, 5)));
      schedulePanel.add(new JLabel("Temperature schedule"));
      schedulePanel.add(comboTempSchedule);
      schedulePanel.add(new JLabel("Mutation schedule"));
      schedulePanel.add(comboMutSchedule);

      leftPanel.add(schedulePanel);

      checkPanel.setBorder(new javax.swing.border.EmptyBorder(new java.awt.Insets(5, 5, 5, 5)));
      checkPanel.add(checkUseConstraints);
      checkPanel.add(checkRandomize);


      leftPanel.add(checkPanel);
//leftPanel.add(smartPanel);

      principalPanel.add(leftPanel);

      rightPanel.setLayout(new java.awt.BorderLayout());
      rightPanel.setBorder(new javax.swing.border.EtchedBorder());

      boundPanel.setLayout(new java.awt.GridLayout(9, 2, 5, 5));
      boundPanel.setBorder(new javax.swing.border.EmptyBorder(new java.awt.Insets(5, 5, 5, 5)));
      boundPanel.add(new JLabel("Const Value"));
      boundPanel.add(textTempConst);
      boundPanel.add(new JLabel("Max temperature"));
      boundPanel.add(textTempStart);
      boundPanel.add(new JLabel("Min temperature"));
      boundPanel.add(textTempEnd);
      boundPanel.add(new JLabel("Const Value"));
      boundPanel.add(textMutConst);
      boundPanel.add(new JLabel("Max mutation"));
      boundPanel.add(textMutStart);
      boundPanel.add(new JLabel("Min mutation"));
      boundPanel.add(textMutEnd);
      boundPanel.add(new JLabel("Smart Scheduling"));
      boundPanel.add(checkSmartScheduling);
      boundPanel.add(new JLabel("Acceptance (0-1)"));
      boundPanel.add(textSmartAcceptance);
      boundPanel.add(new JLabel("Trial size"));
      boundPanel.add(textSmartSize);

      rightPanel.add(boundPanel, java.awt.BorderLayout.CENTER);

      principalPanel.add(rightPanel);

      setTitle("Simulated Annealing refinement options panel");

      updateComboSchedule();
      textSmartAcceptance.setEnabled(checkSmartScheduling.isSelected());
      textSmartSize.setEnabled(checkSmartScheduling.isSelected());

      pack();

    }

    public void actionPerformed(ActionEvent e) {
      updateComboSchedule();
    }

    public void itemStateChanged(ItemEvent e) {
      Object source = e.getItemSelectable();
      if (source == checkSmartScheduling) {
        textSmartAcceptance.setEnabled((e.getStateChange() == ItemEvent.SELECTED));
        textSmartSize.setEnabled((e.getStateChange() == ItemEvent.SELECTED));
      }
    }

    public void updateComboSchedule() {

      String tempSchedule = (String) comboTempSchedule.getSelectedItem();
      String mutSchedule = (String) comboMutSchedule.getSelectedItem();

      if (tempSchedule == "Constant") {
        textTempConst.setEnabled(true);
        textTempStart.setEnabled(false);
        textTempEnd.setEnabled(false);
      } else if (tempSchedule == "Geometric") {
        textTempConst.setEnabled(true);
        textTempStart.setEnabled(true);
        textTempEnd.setEnabled(false);
      } else {
        textTempConst.setEnabled(false);
        textTempStart.setEnabled(true);
        textTempEnd.setEnabled(true);
      }

      if (mutSchedule == "Constant") {
        textMutConst.setEnabled(true);
        textMutStart.setEnabled(false);
        textMutEnd.setEnabled(false);
      } else if (mutSchedule == "Geometric") {
        textMutConst.setEnabled(true);
        textMutStart.setEnabled(true);
        textMutEnd.setEnabled(false);
      } else {
        textMutConst.setEnabled(false);
        textMutStart.setEnabled(true);
        textMutEnd.setEnabled(true);
      }
    }

    public void initParameters() {
      textStep.setText(getnStep());
      textTrial.setText(getnTrial());
      textTempConst.setText(getTConst());
      textTempStart.setText(getTStart());
      textTempEnd.setText(getTEnd());
      textMutConst.setText(getMutConst());
      textMutStart.setText(getMutStart());
      textMutEnd.setText(getMutEnd());
      comboTempSchedule.setSelectedItem(getTempSchedule());
      comboMutSchedule.setSelectedItem(getMutSchedule());
      checkUseConstraints.setSelected(getUseConstraintsB());
      checkRandomize.setSelected(getInitRandomB());
      checkSmartScheduling.setSelected(getSmartSchedulingB());
      textSmartAcceptance.setText(getSmartAcceptance());
      textSmartSize.setText(getSmartSize());
    }

    public void retrieveParameters() {
      setnStep(textStep.getText());
      setnTrial(textTrial.getText());
      setTConst(textTempConst.getText());
      setTStart(textTempStart.getText());
      setTEnd(textTempEnd.getText());
      setMutConst(textMutConst.getText());
      setMutStart(textMutStart.getText());
      setMutEnd(textMutEnd.getText());
      setTempSchedule((String) comboTempSchedule.getSelectedItem());
      setMutSchedule((String) comboMutSchedule.getSelectedItem());
      setUseConstraints(String.valueOf(checkUseConstraints.isSelected()));
      setInitRandom(String.valueOf(checkRandomize.isSelected()));
      setSmartScheduling(String.valueOf(checkSmartScheduling.isSelected()));
      setSmartAcceptance(textSmartAcceptance.getText());
      setSmartSize(textSmartSize.getText());
    }
  }
}

