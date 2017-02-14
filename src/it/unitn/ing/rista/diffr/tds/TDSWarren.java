/*
 * @(#)TDSWarren.java created Jun 30, 2003 Berkeley
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

package it.unitn.ing.rista.diffr.tds;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.JOptionsDialog;

import javax.swing.*;
import java.awt.*;


/**
 * The TDSWarren is a class
 *
 * @version $Revision: 1.4 $, $Date: 2005/05/06 18:07:28 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class TDSWarren extends TDSModel {
  protected static String[] diclistc = {"_riet_par_tds_scale_factor", "_riet_par_tds_width_factor",
                                        "_riet_par_tds_B_factor"};
  protected static String[] diclistcrm = {"scale factor", "width factor",
                                        "B factor"};

  protected static String[] classlistc = {};

  double relaxation_factor = 0.01;
  boolean useFhkl = false;
  double width = 0.0;

  public TDSWarren(XRDcat obj, String alabel) {
    super(obj, alabel);
    initXRD();
    identifier = "Warren TDS";
    IDlabel = "Warren TDS";
  }

  public TDSWarren(XRDcat afile) {
    this(afile, "Warren TDS");
  }

  public TDSWarren() {
    identifier = "Warren TDS";
    IDlabel = "Warren TDS";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 3;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
  }

  public void initParameters() {
    super.initParameters();
    parameterField[0] = new Parameter(this, getParameterString(0), 1.0,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.0),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 100));
    parameterField[1] = new Parameter(this, getParameterString(1), 0.2,
            ParameterPreferences.getDouble(getParameterString(1) + ".min", 0.01),
            ParameterPreferences.getDouble(getParameterString(1) + ".max", 1));
    parameterField[2] = new Parameter(this, getParameterString(2), 0.0,
            ParameterPreferences.getDouble(getParameterString(2) + ".min", 0.0),
            ParameterPreferences.getDouble(getParameterString(2) + ".max", 10));
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
          notifyParameterChanged(source, Constants.TDS_CHANGED);
          return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.TDS_CHANGED);
            return;
          }

      super.notifyParameterChanged(source);
    }
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);
    relaxation_factor = MaudPreferences.getDouble("tdsLogFunction.relaxationFactor", 0.01);
    useFhkl = MaudPreferences.getBoolean("tdsFunction.useFhkl", false);
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    super.updateParametertoDoubleBuffering(false);
    width = Math.abs(getParameterValue(1));
  }

  public void computeTDS(DiffrDataFile diffrDataFile, double[] expfit, PseudoVoigtPeak peak, double[] intensity_v,
                         double Fhkl, double[] position, int[] minmaxindex) {


/*
    for (int j = startingRad; j < totalLines; j++) {

      double radWave = getRadiationWavelength(j);
      int indexPoint = diffrDataFile.getOldNearestPoint(actualPosition[j]);
      aphase.computeTDS(diffrDataFile, diffrDataFile.getXData(), expfit, refl, intensity[j], Fhklist[j],
          actualPosition[j], radWave, indexPoint, minmaxindex);
    }
    */

    double conversion = Constants.DEGTOPI / 2.0;
    int maxIndex = expfit.length - 1;
    double center, intensity;

    for (int j = 0; j < position.length; j++) {
      double radWave = peak.getRadiationWavelength(j);
      int indexPoint = diffrDataFile.getOldNearestPoint(position[j]);
      double[] xdata = diffrDataFile.getXData();


    if (diffrDataFile.dspacingbase) {
      center = 0.5 / position[j];
    } else {
      center = Math.sin(position[j] * conversion) / radWave;
    }
    if (useFhkl)
      intensity = intensity_v[j];
    else
      intensity = intensity_v[j] / Fhkl;

    double scale_factor = getParameterValue(0) * Math.exp(-getParameterValue(2) * center * center) * intensity;
    boolean checkProfile = true;
    int index = indexPoint;
    while (checkProfile) {
      if (index < 0 || index > maxIndex)
        checkProfile = false;
      else {
        double dl = xdata[index];
        if (diffrDataFile.dspacingbase)
          dl = 0.5 / dl;
        else
          dl = Math.sin(dl * conversion) / radWave;
        double diff = Math.abs(dl - center);
        double logPart = width / diff;
        if (logPart < 1.0)
          checkProfile = false;
        else {
          logPart = Math.log(Math.pow(logPart, Math.pow(diff, relaxation_factor)));
          if (logPart > 0.0)
            expfit[index] += scale_factor * logPart;
        }

        index--;
      }
    }
    if (index < 0)
      index = 0;
    if (index < minmaxindex[0])
      minmaxindex[0] = index;
    index = indexPoint + 1;
    checkProfile = true;
    while (checkProfile) {
      if (index > maxIndex || index < 0)
        checkProfile = false;
      else {
        double dl = xdata[index];
        if (diffrDataFile.dspacingbase)
          dl = 0.5 / dl;
        else
          dl = Math.sin(dl * conversion) / radWave;
        double diff = Math.abs(dl - center);
        double logPart = width / diff;
        if (logPart < 1.0)
          checkProfile = false;
        else {
          logPart = Math.log(Math.pow(logPart, Math.pow(diff, relaxation_factor)));
          if (logPart > 0.0)
            expfit[index] += scale_factor * logPart;
        }

        index++;
      }
    }
    if (index > maxIndex)
      index = maxIndex;
    if (index > minmaxindex[1])
      minmaxindex[1] = index;
    }
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JTDSWarrenOptionsD(parent, this);
    return adialog;
  }

  public class JTDSWarrenOptionsD extends JOptionsDialog {

    JTextField[] parTF;
    String[] labels = {"Scale factor  :",
                       "Peak width    :",
                       "Thermal factor:"};

    public JTDSWarrenOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new GridLayout(0, 2, 1, 1));


      parTF = new JTextField[labels.length];
      for (int i = 0; i < labels.length; i++) {
        principalPanel.add(new JLabel(labels[i]));
        parTF[i] = new JTextField(Constants.FLOAT_FIELD);
        parTF[i].setText("0");
        principalPanel.add(parTF[i]);
      }

      initParameters();

      setTitle("TDS Warren model");
      pack();
    }

    public void initParameters() {
      for (int i = 0; i < labels.length; i++) {
        parTF[i].setText(parameterField[i].getValue());
        addComponenttolist(parTF[i], parameterField[i]);
      }
    }

    public void retrieveParameters() {
      for (int i = 0; i < labels.length; i++) {
        parameterField[i].setValue(parTF[i].getText());
      }
    }

  }
}
