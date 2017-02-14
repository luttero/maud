/*
 * @(#)AntiphaseBoundary.java created 11/01/1999 Pergine Vals
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

package it.unitn.ing.rista.diffr;

import java.lang.*;
import java.awt.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import javax.swing.*;


/**
 *  The AntiphaseBoundary is a class
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:04 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class AntiphaseBoundary extends XRDcat {

  public AntiphaseBoundary(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public AntiphaseBoundary(XRDcat aobj) {
    this(aobj, "Antiphase boundary x");
  }

  public AntiphaseBoundary() {
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
          notifyParameterChanged(source, Constants.SAMPLE_BROADENING);
          return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.SAMPLE_BROADENING);
            return;
          }

      super.notifyParameterChanged(source);
    }
  }

  public double getIntegralBeta(int cpType, int h, int k, int l) {
    return 0.0;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JAntiphaseBoundaryOptionsD(parent, this);
    return adialog;
  }

  public class JAntiphaseBoundaryOptionsD extends JOptionsDialog {

    public JAntiphaseBoundaryOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout());


      principalPanel.add(new JLabel("No options for this model"));

      setTitle("Antiphase boundary");
      initParameters();
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }
  }

}
