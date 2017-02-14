/*
 * @(#)launchBasic.java created 01/01/1997 Mesiano
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
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.interfaces.Function;

import javax.swing.*;
import java.awt.event.*;
import java.awt.*;

/**
 * The launchBasic is a class
 *
 * @version $Revision: 1.13 $, $Date: 2006/12/04 14:30:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class launchBasic extends PersistentThread {

  Function parameterfile = null;
  OutputPanel outputframe = null;
  boolean shouldStop = false;
  boolean shouldPause = true;

  public launchBasic(Function afileparameter, OutputPanel aframe) {
    super();
    parameterfile = afileparameter;
    parameterfile.getResultStream();  // to open the log file
    outputframe = aframe;
/*    Constants.useAltivec = MaudPreferences.getBoolean("debug.useG4Altivec", Constants.useAltivec);
    if (Constants.nativeComputation || Constants.useAltivec) {
      try {
        if (!it.unitn.maud.JNIAltivec.isAltiVecAvailable())
          Constants.useAltivec = false;
      } catch (Exception e) {
        Constants.useAltivec = false;
        Constants.nativeComputation = false;
      }
    }*/

/*		if (!Constants.mustLoad && !it.unitn.ing.rista.MaudText.textonly) {
			if (!Misc.loadFilebyName(aframe, MoreMath.getRandomString()))
				parameterfile = null;
			else {
				Constants.mustLoad = true;
			}
		}*/
    if (outputframe != null)
      outputframe.addComponent(createPauseButton());
  }

  public void prepare() {
  }

  public void executeJob() {
    try {
      stuffToRun();
      endOfRun();
    } catch (Exception e) {
      e.printStackTrace();
      if (parameterfile != null)
        parameterfile.endOfComputation();
    }
  }

  public void endOfRun() {
    if (pauseB != null)
      pauseB.setEnabled(false);
//    System.out.println("parameterfiel "+parameterfile);
    if (parameterfile != null && parameterfile.logOutput()) {
      parameterfile.printInformations(parameterfile.getResultStream());
//      System.out.println("Computation: closing log");
      parameterfile.closeLogResultFile();
    }
    if (outputframe != null && MaudPreferences.getBoolean("computation.beepAtEnd", false))
      Toolkit.getDefaultToolkit().beep();
  }

  public void launch() {
    if (Constants.textonly)
      stuffToRun();
    else {

      shouldStop = false;
      start();
    }
  }

  public void stuffToRun() {

    reset();

    if (parameterfile != null)
      parameterfile.endOfComputation();
  }

  public void print(String text) {
    if (outputframe != null)
      outputframe.appendnewline(text);
    else
      System.out.println(text);
  }

  public void reset() {
    if (outputframe != null)
      outputframe.reset();
  }

  public void canWait() {
/*		if (getPriority() > 4 && !it.unitn.ing.rista.MaudText.textonly)
			try {
				sleep(100);
			} catch (InterruptedException ie) {}
			*/
  }

  JIconButton stopB = null;
  static String stopBicon = "TrafficRed.gif";
  static String resumeBicon = "TrafficGreen.gif";

  JButton createStopButton() {
    stopB = new JIconButton(stopBicon);
    stopB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        stopComputation();
      }
    });
    setStopButton();
    return stopB;
  }

  public void setResumeButton() {
    stopB.setIcon(resumeBicon);
    stopB.setToolTipText("Pressing this button will resume the computation");
  }

  public void setStopButton() {
    stopB.setIcon(stopBicon);
    stopB.setToolTipText("Pressing this button will stop safely the computation");
  }

  public void stopComputation() {
    if (!shouldStop) {
      setResumeButton();
      shouldStop = true;
      if (outputframe != null)
        outputframe.computationStopped();
    } else {
      setStopButton();
      shouldStop = false;
      if (outputframe != null)
        outputframe.computationResumed();
    }
  }

  public void interruptComputation() {
    parameterfile = null;
    outputframe = null;
  }

  public boolean shouldStop() {
    return shouldStop;
  }

  public boolean shouldStopIteration() {
    return false;
  }

  JIconButton pauseB = null;
  static String pauseBicon = "VCRPause.gif";
  static String resumePauseBicon = "VCRForward.gif";

  JButton createPauseButton() {
    pauseB = new JIconButton(stopBicon);
    pauseB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setPause();
      }
    });
    setPauseButton();
    return pauseB;
  }

  public void setPauseButton() {
    pauseB.setIcon(pauseBicon);
    pauseB.setToolTipText("Pressing this button will slow-down the computation");
  }

  public void setRestartFromPauseButton() {
    pauseB.setIcon(resumePauseBicon);
    pauseB.setToolTipText("Pressing this button will wake-up the computation");
  }

  public void setPause() {
    if (shouldPause && parameterfile != null) {
      setPriority(pausePriority);
      shouldPause = false;
      setRestartFromPauseButton();
      if (outputframe != null)
        outputframe.computationPaused();
    } else if (parameterfile != null) {
      setPriority(normalPriority);
      shouldPause = true;
      setPauseButton();
      if (outputframe != null)
        outputframe.computationResumed();
    }
  }

  public void hideIterationPanel() {
  }

  public void setIterationSliderValue(int value) {
  }

}

