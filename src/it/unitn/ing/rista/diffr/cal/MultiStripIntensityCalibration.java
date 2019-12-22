/*
 * @(#)MultiStripIntensityCalibration.java created 17/10/2019 Povo
 *
 * Copyright (c) 2019 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.cal;

import it.unitn.ing.rista.diffr.*;
import java.io.*;
import java.lang.*;
import java.util.*;

import it.unitn.ing.rista.io.cif.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 *  The MultiStripIntensityCalibration is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2019/10/17 9:42:44 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class MultiStripIntensityCalibration extends IntensityCalibration {
  
  public static String modelID = "Multistrip channels scale factor";
  public static String descriptionID = "One scale factor for each multistrip detector channel";
  
  public static String[] diclistc = {
      "_inst_inc_spectrum_scale_factor"
  };
  public static String[] diclistcrm = {
      "incident spectrum scale factor"
  };
  
  public static String[] classlistc = {};
  public static String[] classlistcs = {};
  
  boolean refreshCalibration = true;
  
  public MultiStripIntensityCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = modelID;
    IDlabel = modelID;
    description = descriptionID;
  }
  
  public MultiStripIntensityCalibration(XRDcat aobj) {
    this(aobj, "");
  }
  
  public MultiStripIntensityCalibration() {
    identifier = modelID;
    IDlabel = modelID;
    description = descriptionID;
  }
  
  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 1;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }
  
  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
  }
  
  public void initParameters() {
    super.initParameters();
  }
  
  public void checkBanksNumber(int bank) {
    int banks = banknumbers();
    if (bank < banks)
      return;
    for (int i = banks; i <= bank; i++) {
      Parameter par = new Parameter(this, getParameterString(0, i), 1.0,
          ParameterPreferences.getDouble(getParameterString(0, i) + ".min", 0.01),
          ParameterPreferences.getDouble(getParameterString(0, i) + ".max", 100));
      addparameterloopField(0, par);
    }
    updateParametertoDoubleBuffering(false);
  }
  
  public int banknumbers() {
    return numberofelementPL(0);
  }
  
  public double calibrateData(DiffrDataFile datafile, double x, int index) {
//    updateStringtoDoubleBuffering(false);
    int bankNumber = datafile.getBankNumber();
    checkBanksNumber(bankNumber);
    return getParameterLoopValues(0, bankNumber);
  }
  
  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JMultiStripIntensityOptionsD(parent, this);
    return adialog;
  }
  
  class JMultiStripIntensityOptionsD extends JOptionsDialog {
  
    JParameterListPane coeffPanel;

    public JMultiStripIntensityOptionsD(Frame parent, XRDcat obj) {
      
      super(parent, obj);
      
      principalPanel.setLayout(new BorderLayout(6, 6));
  
      coeffPanel = new JParameterListPane(this, false, true);
      principalPanel.add(BorderLayout.CENTER, coeffPanel);
  
      setTitle("Multistrip intensity channel calibration");
      initParameters();
  
      pack();
    }
  
    public void initParameters() {
      super.initParameters();
      coeffPanel.setList(XRDparent, 0);
    }
  
    public void dispose() {
      coeffPanel.dispose();
      super.dispose();
    }
  
  }
  
}
