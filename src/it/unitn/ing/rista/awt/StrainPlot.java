/*
 * @(#)StrainPlot.java created 17/11/1999 Mesiano
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.awt;

import java.awt.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.models.*;
import it.unitn.ing.rista.util.*;

import java.util.*;
import javax.swing.*;
import java.awt.event.*;
import javax.swing.event.*;
import javax.swing.border.*;
import javax.swing.table.*;

import it.unitn.ing.rista.render3d.*;

/**
 * Display a StrainPlot window to manage pole figure and coverage plotting.
 *
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class StrainPlot extends TexturePlot {

  public StrainPlot(Frame parentFrame) {
    super(parentFrame);

    setTitle("Strain plotting");

    setHelpFilename("texturePlotting.txt");

    logScaleCB.setEnabled(false);

  }

    @Override
  public void plot_action() {
    if (thephase == null || thesample == null)
      return;
    boolean twoDmap = plotTypeRB[0].isSelected();
    boolean coverage = plotWhatRB[0].isSelected();
    boolean reconstructed = plotWhatRB[1].isSelected();
    lastResolution = (int) Math.abs(Integer.valueOf(pointsTF.getText()).intValue());
    MaudPreferences.setPref(gridResString, Integer.toString(lastResolution));
    zoom = Math.abs(Double.parseDouble(zoomTF.getText()));
    MaudPreferences.setPref(zoomString, Double.toString(zoom));
    String maxAngleS = maxAngleTF.getText();
    double maxAngle = Double.valueOf(maxAngleS).doubleValue();
    maxAngle = Constants.sqrt2 * Math.sin(maxAngle * Constants.DEGTOPI / 2.0);
    MaudPreferences.setPref(maxAngleString, maxAngleS);

    int colrsNumber = expansionJS.getValue();
    MaudPreferences.setPref(numberofColors, colrsNumber);

    int hklnumbersel = hkltable.getSelectedRow();
    if (hklnumbersel < 0)
      hklnumbersel = 0;

    Reflection[] poleList = null;
    if (reconstructed && twoDmap) {
      int numPoles = thephase.gethklNumber();
      Vector list = new Vector(0, 1);
      for (int i = 0; i < numPoles; i++) {
        Reflection refl = thephase.getReflex(i);
        if (refl.poleFigurePlot)
          list.addElement(refl);
      }
      int selPoles = list.size();
      if (selPoles == 0) {
        poleList = new Reflection[1];
        poleList[0] = thephase.getReflex(hklnumbersel);
      } else {
        poleList = new Reflection[selPoles];
        for (int i = 0; i < selPoles; i++)
          poleList[i] = (Reflection) list.elementAt(i);
        list.removeAllElements();
      }
    } else {
      poleList = new Reflection[1];
      poleList[0] = thephase.getReflex(hklnumbersel);
    }

    if (coverage && !reconstructed)
      (new PlotPFCoverage(this, thesample, thephase, hklnumbersel)).setVisible(true);
    else if ((coverage && reconstructed) && twoDmap)
      (new PlotPoleFigure(this, thesample, poleList, 4, lastResolution, zoom, 
              filterWidth, grayShadedCB.isSelected(), maxAngle, false, colrsNumber)).setVisible(true);
    else if (reconstructed && !twoDmap)
      show3DPole(this, poleList[0], -1, lastResolution, maxAngle, false, colrsNumber);
    else if (reconstructed)
      (new PlotPoleFigure(this, thesample, poleList, 5, lastResolution, zoom, 
              filterWidth, grayShadedCB.isSelected(), maxAngle, false, colrsNumber)).setVisible(true);
    return;
  }

}
