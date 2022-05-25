/*
 * @(#)launchRefine.java created 01/01/1997 Mesiano
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

import it.unitn.ing.rista.awt.JIconButton;
import it.unitn.ing.rista.interfaces.Function;
import it.unitn.ing.rista.util.Misc;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * The launchRefine is a class
 *
 * @version $Revision: 1.13 $, $Date: 2006/01/19 14:45:54 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class launchRefine extends launchBasic {

  public launchRefine(Function afileparameter, OutputPanel aframe) {
    super(afileparameter, aframe);

    if (aframe != null)
      aframe.addComponent(createStopIterationButton());

    if (aframe != null)
      aframe.addComponent(createStopButton());

    if (aframe != null)
      aframe.addComponent(createChangeIterationSlider());

  }

  public OptimizationAlgorithm sol;

  public void prepare() {

//      String mem = Misc.freeMemory();
//      if (Constants.testing)
//        System.out.println(mem);

    if (parameterfile == null)
      return;
//    String mem = Misc.freeMemory();
//    if (Constants.testing)
//      System.out.println(mem);
    sol = parameterfile.getOptimizationAlgorithm();//new LeastSquareFit((Function) parameterfile, outputframe);
		sol.setOutputFrame(outputframe);
    shouldStopIteration = false;
  }

  String endOfIterationString = "End of refinement, have a good day!";

  public void stuffToRun() {

//      System.out.println("Arriving here");
    try {
      Thread.sleep(200);
    } catch (InterruptedException e) {
    }


    reset();

    print("Start rita/rista refinement");

    if (parameterfile != null) {
      try {
	      sol.solveGeneral(this, parameterfile);
      } catch (Exception e) {

        print("Error in the refinement, check the java console window for more details.");
        print("Have a nice day (if you get it working)!");
        e.printStackTrace();
        parameterfile.setOptimizing(false);

      }
    }

  }

  public void endOfRun() {
    sol = null;

    if (parameterfile != null)
      parameterfile.fittingFileOutput();

    if (outputframe != null)
      outputframe.setProgressText(endOfIterationString);
    else
      System.out.println(endOfIterationString);

//		outputframe.hideprogressBar();
    if (parameterfile != null) {
      parameterfile.endOfComputation();
    }

    if (stopIterationB != null)
      stopIterationB.setEnabled(false);
    if (stopB != null)
      stopB.setEnabled(false);
    if (iterationPanel != null)
      iterationPanel.setVisible(false);


    super.endOfRun();
  }

  boolean shouldStopIteration = false;

  public void stopComputation() {
    super.stopComputation();
    if (shouldStop())
      endOfIterationString = "Computation safely stopped by user, have a good day!";
    else
      endOfIterationString = "End of refinement, have a good day!";
  }

  public void stopIteration() {
    if (!shouldStopIteration) {
      setResumeIterationButton();
      shouldStopIteration = true;
      if (outputframe != null)
        outputframe.iterationStopped();
    } else {
      setStopIterationButton();
      shouldStopIteration = false;
      if (outputframe != null)
        outputframe.iterationResumed();
    }

    if (shouldStopIteration())
      endOfIterationString = "Iterations safely stopped by user, have a good day!";
    else
      endOfIterationString = "End of refinement, have a good day!";
  }

  public boolean shouldStopIteration() {
    return shouldStopIteration;
  }

  JIconButton stopIterationB = null;
  static String stopIterationBicon = "RedFlag.gif";
  static String resumeIterationBicon = "GreenFlag.gif";

  JButton createStopIterationButton() {
    stopIterationB = new JIconButton(stopIterationBicon);
    stopIterationB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        stopIteration();
      }
    });
    setStopIterationButton();
    return stopIterationB;
  }

  public void setResumeIterationButton() {
    stopIterationB.setIcon(resumeIterationBicon);
    stopIterationB.setToolTipText("Pressing this button will resume the iterations");
  }

  public void setStopIterationButton() {
    stopIterationB.setIcon(stopIterationBicon);
    stopIterationB.setToolTipText("Pressing this button will stop the iteration at the end of the current one");
  }

  JPanel iterationPanel = null;

  JSlider iterationJS = null;
  JPanel createChangeIterationSlider() {
    iterationPanel = new JPanel();
//    iterationPanel.add(new JLabel("Iterations: "));
    iterationJS = new JSlider();
    iterationJS.setToolTipText("Change the number of iterations during refinement (best values: 3-7)");
    iterationJS.addChangeListener(new ChangeListener() {
      public void stateChanged(ChangeEvent e) {
        setIterations(((JSlider) e.getSource()).getValue());
      }
    });
    iterationPanel.add(iterationJS);
    initIterationJS(iterationJS);
    return iterationPanel;
  }

  public void initIterationJS(JSlider iterationJS) {
    iterationJS.setMaximum(21);
    iterationJS.setMinimum(1);
    iterationJS.setValue(21);
    iterationJS.setPaintTicks(true);
    iterationJS.setMajorTickSpacing(5);
    iterationJS.setMinorTickSpacing(1);
    iterationJS.setPaintLabels(true);
    iterationJS.setSnapToTicks(true);
    iterationJS.setLabelTable(iterationJS.createStandardLabels(5));
  }

  public void setIterationSliderValue(int value) {
    iterationJS.setValue(value);
  }

  public void setIterations(int iteration) {
    if (sol != null && sol.getIterations() != iteration) {
      sol.setIterations(iteration);
      if (outputframe != null)
        outputframe.resizeProgressBarMaximum((parameterfile.getNumberOfFreeParameters() + 1) * iteration);
    }
  }

  public void hideIterationPanel() {
    iterationPanel.setVisible(false);
  }

  public int getIterations() {
    if (sol != null)
      return sol.getIterations();
    else
      return 0;
  }

  public void interruptComputation() {
    super.interruptComputation();
    sol.setOutputFrame(null);
    sol = null;
  }

}

