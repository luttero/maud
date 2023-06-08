/*
 * @(#)SizeStrainDistributions.java created 05/10/1998 Verona
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

package it.unitn.ing.rista.diffr.sizestrain;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.interfaces.Peak;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.MaudPreferences;
import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.awt.JSubordinateLoopListPane;
import it.unitn.ing.rista.awt.JIconButton;
import it.unitn.ing.rista.awt.PlotSimpleData;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 *  The SizeStrainDistributions is a class
 *
 *
 * @version $Revision: 1.8 $, $Date: 2006/01/19 14:45:58 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SizeStrainDistributions extends SizeStrainModel {

  protected static String[] diclistc = {"_riet_fourier_transform_LmaxToDv_ratio",
                                        "_riet_fourier_transform_fft_points",
    "_riet_par_dist_crystallite", "_riet_par_dist_microstrain"};
  protected static String[] diclistcrm = {"Lmax to Dv ratio",
                                        "Number of FFT points (power of 2)",
    "_riet_par_dist_crystallite", "_riet_par_dist_microstrain"};

  protected static String[] classlistc = {"it.unitn.ing.rista.diffr.sizestrain.CrystalliteDistribution",
                                          "it.unitn.ing.rista.diffr.sizestrain.MicrostrainDistribution"};

  protected static String[] classlistcs = {};

  int numberOfFourierPoints = 4096;
  double LmaxFactor = 3.0;

  public SizeStrainDistributions(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Distributions";
    IDlabel = "Distributions";
    description = "select this to apply the Distributions model";
  }

  public SizeStrainDistributions(XRDcat aobj) {
    this(aobj, "Line Broadening Distributions model");
  }

  public SizeStrainDistributions() {
    identifier = "Distributions";
    IDlabel = "Distributions";
    description = "select this to apply the Distributions model";
  }

  public void initConstant() {
    Nstring = 2;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = classlistcs.length;
    Nsubordinateloop = classlistc.length;
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
    stringField[0] = MaudPreferences.getPref("lineBroadening_FourierTransform.LmaxToDvRatio", "3.0");
    stringField[1] = MaudPreferences.getPref("lineBroadening_FourierTransform.numberOfDivisions", "4096");
  }

  public void initializeAsNew() {
    if (initialized)
      return;
    initialized = true;
    addsubordinateloopField(0);
    addsubordinateloopField(1);
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);
    LmaxFactor = Double.parseDouble(stringField[0]);
    numberOfFourierPoints = Integer.parseInt(stringField[1]);
  }

  public int crystalliteDistributionNumber() {
    return numberofelementSubL(0);
  }

  public CrystalliteDistribution getCrystalliteDistribution(int index) {
    return (CrystalliteDistribution) subordinateloopField[0].elementAt(index);
  }

  public int microstrainDistributionNumber() {
    return numberofelementSubL(1);
  }

  public MicrostrainDistribution getMicrostrainDistribution(int index) {
    return (MicrostrainDistribution) subordinateloopField[1].elementAt(index);
  }

  private void plotCrystalliteDistribution(XRDcat selectedObject) {
    // selected
    double[] distr = ((CrystalliteDistribution) selectedObject).getCrystalliteDistribution(getDeff());
    (new PlotSimpleData(new Frame(), distr)).setVisible(true);
  }

  private void plotMicrostrainDistribution(XRDcat selectedObject) {
    // selected
    (new PlotSimpleData(new Frame(), ((MicrostrainDistribution) selectedObject).getMicrostrainDistribution(
        getMicrostrain(), getDeff()
    ))).setVisible(true);
  }

	private void plotCrystalliteDistribution() {
    // all togheter
    double[] distr = null;
    double totWeight = 0.0;
    for (int i = 0; i < crystalliteDistributionNumber(); i++) {
      CrystalliteDistribution cryst = getCrystalliteDistribution(i);
      if (i == 0) {
        distr = cryst.getCrystalliteDistribution(getDeff());
        for (int j = 0; j < distr.length; j++)
          distr[j] *= cryst.weight;
      } else {
        double[] tdistr = cryst.getCrystalliteDistribution(getDeff());
        for (int j = 0; j < distr.length; j++)
          distr[j] += cryst.weight * tdistr[j];
      }
      totWeight += cryst.weight;
    }
    for (int j = 0; j < distr.length; j++)
      distr[j] /= totWeight;
    (new PlotSimpleData(new Frame(), distr)).setVisible(true);
  }

  private void plotMicrostrainDistribution() {
    // all togheter
    double[] distr = null;
    double totWeight = 0.0;
    for (int i = 0; i < microstrainDistributionNumber(); i++) {
      MicrostrainDistribution mstrain = getMicrostrainDistribution(i);
      if (i == 0) {
        distr = mstrain.getMicrostrainDistribution(getMicrostrain(), getDeff());
        for (int j = 0; j < distr.length; j++)
          distr[j] *= mstrain.weight;
      } else {
        double[] tdistr = mstrain.getMicrostrainDistribution(getMicrostrain(), getDeff());
        for (int j = 0; j < distr.length; j++)
          distr[j] += mstrain.weight * tdistr[j];
      }
      totWeight += mstrain.weight;

    }
    for (int j = 0; j < distr.length; j++)
      distr[j] /= totWeight;
    (new PlotSimpleData(new Frame(), distr)).setVisible(true);
  }

  public double getDeff() {
    return ((Phase) getParent()).getMeanCrystallite();
  }

  public double getMicrostrain() {
    return ((Phase) getParent()).getMeanMicrostrain();
  }

  public double getBetaChauchy(double dspace, double cryst, double mstrain) {
    if (cryst == 0.0)
      return 0.0;
    return dspace * dspace / Math.abs(cryst);
  }

  public double getBetaGauss(double dspace, double cryst, double mstrain) {
    return 2.0 * Math.abs(mstrain) * Constants.mstraintoetilde * dspace;
  }

  public double getLmaxFactor() {
    return LmaxFactor;
  }

  public int getNumberOfFourierPoints() {
    return numberOfFourierPoints;
  }

  public Peak createPeak(double pos, boolean control, boolean energyDispersive, double[] wave, double[] weight,
                         Reflection reflex, int order) {

    return new FourierTransformPeak(pos, control, energyDispersive, wave, weight, reflex, order, this);
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new SizeStrainDistributions.JSizeStrainDistributionsOptionsD(parent, this);
    return adialog;
  }

  public class JSizeStrainDistributionsOptionsD extends JOptionsDialog {

    JSubordinateLoopListPane crystallitePanel;
    JSubordinateLoopListPane microstrainPanel;
    JTextField[] stringFieldTF;

    public JSizeStrainDistributionsOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);


      principalPanel.setLayout(new BorderLayout(6, 6));
      JPanel distrP = new JPanel(new GridLayout(1, 2, 1, 1));
      principalPanel.add(BorderLayout.NORTH, distrP);
      JPanel optionsP = new JPanel(new GridLayout(0, 1, 1, 1));
      principalPanel.add(BorderLayout.CENTER, optionsP);
      JPanel upP;
      stringFieldTF = new JTextField[Nstring];
      for (int i = 0; i < Nstring; i++) {
        upP = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
        optionsP.add(upP);
        upP.add(new JLabel(diclistRealMeaning[i] + ": "));
        upP.add(stringFieldTF[i] = new JTextField(Constants.FLOAT_FIELD));
      }

      crystallitePanel = new JSubordinateLoopListPane(this, "Size distributions");
      distrP.add(crystallitePanel);

      microstrainPanel = new JSubordinateLoopListPane(this, "Microstrain distributions");
      distrP.add(microstrainPanel);

      initParameters();

      setTitle("Size-strain distributions");
      pack();
    }

    public void initParameters() {
      for (int i = 0; i < Nstring; i++)
        stringFieldTF[i].setText(stringField[i]);
      crystallitePanel.setList(SizeStrainDistributions.this, 0);
      JButton crystDistrPlotB = new JIconButton("LineGraph.gif", "Plot selected");
      crystallitePanel.addButton(crystDistrPlotB);
      crystDistrPlotB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          ((SizeStrainDistributions) crystallitePanel.itsparent).plotCrystalliteDistribution(
              crystallitePanel.getSelectedObject());
        }
      });
      JButton crystDistrAllPlotB = new JIconButton("LineGraph.gif", "Plot all");
      crystallitePanel.addButton(crystDistrAllPlotB);
      crystDistrAllPlotB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          getFilePar().refreshAll(false);
          ((SizeStrainDistributions) crystallitePanel.itsparent).plotCrystalliteDistribution();
        }
      });

      microstrainPanel.setList(SizeStrainDistributions.this, 1);
      JButton strainDistrPlotB = new JIconButton("LineGraph.gif", "Plot selected");
      microstrainPanel.addButton(strainDistrPlotB);
      strainDistrPlotB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          getFilePar().refreshAll(false);
          ((SizeStrainDistributions) microstrainPanel.itsparent).plotMicrostrainDistribution(
              microstrainPanel.getSelectedObject());
        }
      });
      JButton strainDistrAllPlotB = new JIconButton("LineGraph.gif", "Plot all");
      microstrainPanel.addButton(strainDistrAllPlotB);
      strainDistrAllPlotB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          getFilePar().refreshAll(false);
          ((SizeStrainDistributions) microstrainPanel.itsparent).plotMicrostrainDistribution();
        }
      });
    }

    public void retrieveParameters() {
      for (int i = 0; i < Nstring; i++)
        stringField[i] = stringFieldTF[i].getText();
    }

  }

}
