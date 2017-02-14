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
		  "_riet_par_asymmetry_truncation",

		  "_riet_par_asymmetry_value_inv",
		  "_riet_par_broadening_hwhm", "_riet_par_broadening_gaussian"
  };

  protected static final String[] diclistcrm = {
		  "asymmetry truncation angle",

		  "asymmetry coeff ",
		  "broadening coeff ", "gaussian coeff "
  };

  protected static final String[] classlistc = {};

  protected static final String[] classlistcs = {};

  public static double minimumHWHMvalue = MaudPreferences.getDouble(
      "instrBroadening.minimumHWHMvalue", 0.0000001);

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
    Nstring = 1;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 3;
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
	  setTruncationAngle("-1000.0");
  }

  public void initializeAsNew() {
    if (initialized)
      return;
    initialized = true;
	  addparameterloopField(0, new Parameter(this, getParameterString(0, 0), 0,
			  ParameterPreferences.getDouble(getParameterString(0, 0) + ".min", 0),
			  ParameterPreferences.getDouble(getParameterString(0, 0) + ".max", 1)));
    addparameterloopField(1, new Parameter(this, getParameterString(1, 0), 42.4,
        ParameterPreferences.getDouble(getParameterString(1, 0) + ".min", 0),
        ParameterPreferences.getDouble(getParameterString(1, 0) + ".max", 100)));
    addparameterloopField(1, new Parameter(this, getParameterString(1, 1), 5330,
        ParameterPreferences.getDouble(getParameterString(1, 1) + ".min", 100),
        ParameterPreferences.getDouble(getParameterString(1, 1) + ".max", 10000)));
    addparameterloopField(2, new Parameter(this, getParameterString(2, 0), 0.1,
        ParameterPreferences.getDouble(getParameterString(2, 0) + ".min", -1.0),
        ParameterPreferences.getDouble(getParameterString(2, 0) + ".max", 2.0)));
	  addparameterloopField(2, new Parameter(this, getParameterString(2, 0), 0.0,
			  ParameterPreferences.getDouble(getParameterString(2, 1) + ".min", -1.0),
			  ParameterPreferences.getDouble(getParameterString(2, 1) + ".max", 2.0)));
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

	public static final int asymmetryTruncationID = 0;

	public void setTruncationAngle(String value) {
		setString(asymmetryTruncationID, value);
	}

	public String getTruncationAngleString() {
		return getString(asymmetryTruncationID);
	}

	public double getTruncationAngle() {
		return truncationAngle;
	}

	public static final int asymmetryID = 0;

   public static final int cagliotiID = 1;

   public static final int gaussianID = 2;

	double asymmetry[] = null;
	int asymmetryN = 0;
	double caglioti[] = null;
	int cagliotiN = 0;
	double gaussian[] = null;
	int gaussianN = 0;
	double truncationAngle = -1000;

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);

    minimumHWHMvalue = MaudPreferences.getDouble(
        "instrBroadening.minimumHWHMvalue", 0.0000001);
	  truncationAngle = Double.parseDouble(getTruncationAngleString());
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);

//    if (MaudPreferences.getBoolean("CagliotiFirstParameter.forcePositive", true))
//      checkCagliotiFirstParameter();
	  asymmetry = getParameterLoopVector(asymmetryID);
	  asymmetryN = numberOfLoopParameters[asymmetryID];
//    if (MaudPreferences.getBoolean("CagliotiFirstParameter.forcePositive", true))
//      checkCagliotiFirstParameter();
	  caglioti = getParameterLoopVector(cagliotiID);
	  cagliotiN = numberOfLoopParameters[cagliotiID];
	  gaussian = getParameterLoopVector(gaussianID);
	  gaussianN = numberOfLoopParameters[gaussianID];
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

  public double[] getInstrumentalBroadeningAt(double x, DiffrDataFile diffrDataFile) {

// Attention: x equal to 2theta

    x *= 0.001;
    double broad[] = new double[4];

    broad[0] = 0.0;
    for (int i = 0; i < gaussianN; i++)
      broad[0] += gaussian[i] * MoreMath.pow(x, i);
/*    if (broad[0] < 0.0)
      broad[0] = 0.0;
    if (broad[0] > 1.0)
      broad[0] = 1.0;*/
    broad[1] = 0.0;
    for (int i = 0; i < cagliotiN; i++)
      broad[1] += caglioti[i] * MoreMath.pow(x, i);
    if (broad[1] < minimumHWHMvalue * 10000)
      broad[1] = minimumHWHMvalue * 10000;
	  // sx
	  broad[2] = broad[0];     // symmetric
	  broad[3] = broad[1];     // symmetric

    return broad;
  }

  /**
   * Return the instrumental asymmetry for the convolution with the profile function.
   * The method here is called by Instrument and should not be modify in general.
   *
   * @param x             the 2-theta or d-spacing (if the spectrum is in d-spacing) of the
   *                      point for which the broadening should be computed.
   * @param diffrDataFile the spectrum
   * @return asymmetry parameter value
   */

  public double getInstrumentalAsymmetry(double x, DiffrDataFile diffrDataFile) {

	  double asy = 0.0;
	  if (asymmetryN > 0) {
		  double x1 = x;// = 0.0;
		  if (diffrDataFile.dspacingbase)
			  x1 = 1.0 / x;

		  for (int i = 0; i < asymmetryN; i++)
			  asy += asymmetry[i] * MoreMath.pow(x1, i);
	  }
	  return asy;
  }

  public void computeAsymmetry(DiffrDataFile diffrDataFile, Sample asample, double[] afit, int min, int max) {

	  Instrument ainstrument = getInstrument();

	  double truncation_angle = getTruncationAngle();

	  double newFit[] = new double[max - min];
	  if (truncation_angle != 0.0) {
		  int absdirection = 1;  // increasing step
		  if (!diffrDataFile.increasingX())
			  absdirection = -absdirection;
		  absdirection *= (int) (truncation_angle / Math.abs(truncation_angle));
		  truncation_angle = Math.abs(truncation_angle);

		  for (int j = min; j < max; j++) {
			  double x = diffrDataFile.getXData(j);
			  double total_asymmetry = ainstrument.getInstrumentalAsymmetry(x, diffrDataFile) * 0.0001;
			  if (total_asymmetry == 0.0)
				  newFit[j - min] = afit[j];
			  else {
				  int direction = absdirection;
				  if (!diffrDataFile.dspacingbase && x > 90.0)
					  direction = -direction;
				  double function = afit[j];
				  double normalization = 1.0;
				  int ij = j + direction;
				  if (diffrDataFile.insiderange(ij)) {
					  double difference = Math.abs(diffrDataFile.getXData(ij) - x);
					  double expasymmetry = 1.0;
					  for (; expasymmetry > 0.001 && difference < truncation_angle && diffrDataFile.insiderange(ij); ij += direction)
					  {
						  difference = Math.abs(diffrDataFile.getXData(ij) - x);
						  expasymmetry = Math.exp(-difference * total_asymmetry);
						  function += afit[ij] * expasymmetry;
						  normalization += expasymmetry;
					  }
				  }
				  newFit[j - min] = function / normalization;
			  }

		  }
		  System.arraycopy(newFit, 0, afit, min, max - min);
	  }

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

	  JParameterListPane AsymmetryPanel;
    JParameterListPane HWHMPanel;
    JParameterListPane GaussianPanel;
	  JTextField truncationTF = null;

    public JEDXRFOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel aberrationPanel = new JPanel(new BorderLayout(3, 3));
      JTabbedPane tabPanel1 = new JTabbedPane();
      String tempString[] = {"Asymmetry", "HWHM", "Gaussianity"};
      principalPanel.add(BorderLayout.CENTER, aberrationPanel);
      aberrationPanel.add(BorderLayout.CENTER, tabPanel1);

	    JPanel jpasy = new JPanel(new BorderLayout());
	    tabPanel1.addTab(tempString[0], null, jpasy);

	    AsymmetryPanel = new JParameterListPane(this, false, true);
	    jpasy.add(BorderLayout.CENTER, AsymmetryPanel);

	    JPanel p6 = new JPanel();
	    p6.setLayout(new FlowLayout());
	    jpasy.add(BorderLayout.SOUTH, p6);
	    p6.add(new JLabel(" Truncation angle [deg or d]: "));
	    truncationTF = new JTextField(Constants.FLOAT_FIELD);
	    p6.add(truncationTF);

      HWHMPanel = new JParameterListPane(this, false, true);
      tabPanel1.addTab(tempString[1], null, HWHMPanel);

      GaussianPanel = new JParameterListPane(this, false, true);
      tabPanel1.addTab(tempString[2], null, GaussianPanel);

/*      JPanel closebuttonPanel = new JPanel();
      closebuttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
      principalPanel.add(BorderLayout.SOUTH, closebuttonPanel);
      JButton jbok1 = new JCloseButton();
      closebuttonPanel.add(jbok1);
      jbok1.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          retrieveParameters();
          setVisible(false);
          dispose();
        }
      });*/


      setTitle("XRF/EDXRF instrumental function");
      initParameters();
      pack();
    }

    public void initParameters() {
	    AsymmetryPanel.setList(EDXRFInstrumentBroadening.this, 0);
      HWHMPanel.setList(EDXRFInstrumentBroadening.this, 1);
      GaussianPanel.setList(EDXRFInstrumentBroadening.this, 2);
	    truncationTF.setText(getTruncationAngleString());
    }

    public void retrieveParameters() {
      super.retrieveParameters();

	    AsymmetryPanel.retrieveparlist();
      HWHMPanel.retrieveparlist();
      GaussianPanel.retrieveparlist();
	    setTruncationAngle(truncationTF.getText());
    }

    public void dispose() {
	    AsymmetryPanel.dispose();
      HWHMPanel.dispose();
      GaussianPanel.dispose();

      super.dispose();
    }
  }

}
