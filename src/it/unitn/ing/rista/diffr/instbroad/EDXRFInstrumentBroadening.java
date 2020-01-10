/*
 * @(#)EDXRFInstrumentBroadening.java created Apr 11, 2007 Casalino
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.instbroad;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.util.function.*;

import javax.swing.*;
import java.awt.*;

/**
 * The EDXRFInstrumentBroadening is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Apr 11, 2007 8:38:05 AM $
 * @since JDK1.1
 */
public class EDXRFInstrumentBroadening extends InstrumentBroadening {

  public static String modelID = "XRF/EDXRF broadening";
  public static String descriptionID = "Broadening of peaks by energy";

  public static final String[] diclistc = {
		  "_riet_par_broadening_hwhm", "_riet_par_broadening_gaussian",
		  "_riet_par_step_fraction", "_riet_par_tail_beta",
		  "_riet_par_tail_fraction_k_alpha", "_riet_par_tail_fraction_k_beta"
  };

  protected static final String[] diclistcrm = {
		  "broadening coeff ", "gaussian coeff ",
		  "step fraction", "tail beta",
		  "tail fraction Kalpha", "tail fraction Kbeta"
  };

  protected static final String[] classlistc = {};

  protected static final String[] classlistcs = {};

  public static double minimumHWHMvalue = MaudPreferences.getDouble(
      "instrBroadening_EDXRF.minimumHWHMvalue", 0.001);

	public EDXRFInstrumentBroadening(XRDcat obj, String alabel) {
    super(obj, alabel);
    initXRD();
    identifier = modelID;
    IDlabel = modelID;
    description = descriptionID;
  }

  public EDXRFInstrumentBroadening(XRDcat afile) {
    this(afile, modelID);
  }

  public EDXRFInstrumentBroadening() {
    identifier = modelID;
    IDlabel = modelID;
    description = descriptionID;
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 6;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    System.arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
    System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
  }

  public void initParameters() {
    super.initParameters();
  }

  public void initializeAsNew() {
    if (initialized)
      return;
    initialized = true;
    // HWHM
    int index = 0;
    int number = 0;
    addparameterloopField(index, new Parameter(this, getParameterString(index, number), 42.4,
        ParameterPreferences.getDouble(getParameterString(index, number) + ".min", 0),
        ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 100)));
    number++;
    addparameterloopField(index, new Parameter(this, getParameterString(0, number), 5330,
        ParameterPreferences.getDouble(getParameterString(index, number) + ".min", 100),
        ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 10000)));
	  number++;
	  addparameterloopField(index, new Parameter(this, getParameterString(0, number), 0,
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".min", 0),
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 10000)));
    // Gaussian
    index++;
    number = 0;
    addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.01,
        ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
        ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
    number++;
	  addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.0,
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
	  // fS
	  index++;
	  number = 0;
	  addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.0001,
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
	  number++;
	  addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.0,
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
	  // beta
	  index++;
	  number = 0;
	  addparameterloopField(index, new Parameter(this, getParameterString(index, number), 10,
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
	  number++;
	  addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.0,
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
	  // fTKalpha
	  index++;
	  number = 0;
	  addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.002,
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
	  number++;
	  addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.0,
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
	  // fTKbeta
	  index++;
	  number = 0;
	  addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.002,
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
	  number++;
	  addparameterloopField(index, new Parameter(this, getParameterString(index, number), 0.0,
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".min", -1.0),
			  ParameterPreferences.getDouble(getParameterString(index, number) + ".max", 2.0)));
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.INSTRUMENT_BROADENING);
            return;
          }

      super.notifyParameterChanged(source);
    }
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);

    minimumHWHMvalue = MaudPreferences.getDouble(
        "instrBroadening.minimumHWHMvalue", 0.0000001);
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);

  }

  public Instrument getInstrument() {
    return (Instrument) getParent();
  }

  public Measurement getMeasurement() {
    return getInstrument().getMeasurement();
  }

  public Geometry getGeometry() {
    return getInstrument().getGeometry();
  }

  public java.util.Vector<double[]> getInstrumentEnergyBroadeningAt(double x, DiffrDataFile diffrDataFile) {

    java.util.Vector<double[]> broad = new java.util.Vector<>(parameterloopField.length);

	  double[] par = getParameterLoopVector(0);
	  double[] value = {0.0};
	  for (int i = 0; i < par.length; i++)
      value[0] += par[i] * MoreMath.pow(x, i);
	  
	  if (value[0] <= 0)
      value[0] = minimumHWHMvalue;
	  else
      value[0] = Math.sqrt(value[0]);
    broad.add(value);

	  par = getParameterLoopVector(1);
    value = new double[]{0.0};
    value[0] = 0.0;
	  for (int i = 0; i < par.length; i++)
      value[0] += par[i] * MoreMath.pow(x, i);
	  if (value[0] < 0.0)
      value[0] = 0.0;
	  if (value[0] > 1.0)
      value[0] = 1.0;
    broad.add(value);

	  for (int i = 2; i < parameterloopField.length; i++) {
		 par = getParameterLoopVector(i);
		 value = new double[par.length];
		 for (int j = 0; j < par.length; j++)
       value[j] = par[j];
      broad.add(value);
	 }

    return broad;
  }

  public void computeAsymmetry(DiffrDataFile diffrDataFile, Sample asample, double[] afit, int min, int max) {

  }

  public void computeFluorescenceBroadening(DiffrDataFile diffrDataFile, Sample asample, double afit[], int min, int max) {

  }

  public boolean freeAllBasicParameters() {
    return false;
  }

  public void plotFunction(Frame theframe, int index) {
    ParameterFunction function = new PolynomialFunction(parameterloopField[index], false);
    (new PlotParameterFunction(theframe, function)).setVisible(true);
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    return new JEDXRFOptionsD(parent, this);
  }

  class JEDXRFOptionsD extends JOptionsDialog {

    JParameterListPane PLPanel[];

    public JEDXRFOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel aberrationPanel = new JPanel(new BorderLayout(3, 3));
      JTabbedPane tabPanel1 = new JTabbedPane();
      String tempString[] = {"HWHM (FANO)", "Gaussianity", "fS", "Beta", "fT Kalpha", "fT Kbeta"};
      principalPanel.add(BorderLayout.CENTER, aberrationPanel);
      aberrationPanel.add(BorderLayout.CENTER, tabPanel1);

	    PLPanel = new JParameterListPane[EDXRFInstrumentBroadening.this.parameterloopField.length];
	    for (int i = 0; i < EDXRFInstrumentBroadening.this.parameterloopField.length; i++) {
		    PLPanel[i] = new JParameterListPane(this, false, true);
		    tabPanel1.addTab(tempString[i], null, PLPanel[i]);
	    }

      setTitle("XRF/EDXRF instrumental function");
      initParameters();
      pack();
    }

    public void initParameters() {
	    for (int i = 0; i < EDXRFInstrumentBroadening.this.parameterloopField.length; i++) {
		    PLPanel[i].setList(EDXRFInstrumentBroadening.this, i);
	    }
    }

    public void retrieveParameters() {
      super.retrieveParameters();

	    for (int i = 0; i < EDXRFInstrumentBroadening.this.parameterloopField.length; i++) {
		    PLPanel[i].retrieveparlist();
	    }
    }

    public void dispose() {
	    for (int i = 0; i < EDXRFInstrumentBroadening.this.parameterloopField.length; i++) {
		    PLPanel[i].dispose();
	    }
      super.dispose();
    }
  }

}
