/*
 * @(#)DicVol91Result.java created 6/7/2002 Barcelona
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

package it.unitn.ing.rista.io;

import java.io.*;
import java.util.*;
import java.awt.*;
import java.net.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.io.*;
import it.unitn.ing.rista.interfaces.*;

import javax.swing.*;
import java.awt.event.*;
import javax.swing.border.*;

/**
 *  The DicVol91Result is a class to store a Dicvol91 result
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:09 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class DicVol91Result {
  public String symmetry = "";
  public String[] cell = new String[6];
//  public Vector reflexList = new Vector(0, 1);
  public Vector dspaceList = new Vector(0, 1);
  public double dspacemin = 1.0E50;
  public double dspacemax = -1.0;
  public double wavelength = -1.0;

  public DicVol91Result(String value) {
    symmetry = value;
  }

  public void setLabel(String value) {
    symmetry = value;
  }

  public void setCellA(String value) {
    cell[0] = value;
  }

  public void setCellB(String value) {
    cell[1] = value;
  }

  public void setCellC(String value) {
    cell[2] = value;
  }

  public void setCellAlpha(String value) {
    cell[3] = value;
  }

  public void setCellBeta(String value) {
    cell[4] = value;
  }

  public void setCellGamma(String value) {
    cell[5] = value;
  }

  public void setWavelength(double wave) {
    wavelength = wave;
  }

  public double getWavelength() {
    return wavelength;
  }

  public void addReflex(double position, double error) {
    double[] dspaceObs = new double[2];
    dspaceObs[0] = position;
    dspaceObs[1] = error;
    if (dspaceObs[0] < dspacemin)
      dspacemin = dspaceObs[0];
    if (dspaceObs[0] > dspacemax)
      dspacemax = dspaceObs[0];
    dspaceList.addElement(dspaceObs);
  }

}
