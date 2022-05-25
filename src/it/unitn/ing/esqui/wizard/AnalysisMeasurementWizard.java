/*
 * @(#)AnalysisMeasurementWizard.java created 16/03/2002 Casalino
 *
 * Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.esqui.wizard;

import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

import java.util.*;
import java.lang.reflect.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;

/**
 * The AnalysisMeasurementWizard is a basic class to
 * provide wizard starting actions.
 *
 * @version $Revision: 1.4 $, $Date: 2004/11/18 09:30:48 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class AnalysisMeasurementWizard extends BaseWizard {

  public AnalysisMeasurementWizard(principalJFrame aframe, String alabel) {
    this();
    theMainFrame = aframe;
  }

  public AnalysisMeasurementWizard() {
    identifier = "Experiment wizard";
    IDlabel = "Experiment wizard ";
    description = "Start a new Experiment wizard (analysis & measurement)";
    iconName = "NewExperiment.gif";
  }

  public void startWizardAction() {
//    super.startWizardAction();
//    theMainFrame.esquiClient_Action();


  }

}
