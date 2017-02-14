/*
 * @(#)IntensityExtEntropy.java created 04/3/1999 Pergine Vals.
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

package it.unitn.ing.rista.diffr.intext;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.sfm.StructureFactorLeBailExtractor;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.interfaces.Peak;

import java.awt.*;
import java.util.Vector;
import javax.swing.*;
import javax.swing.event.*;

/**
 *  The IntensityExtEntropy is a class
 *
 *
 * @version $Revision: 1.10 $, $Date: 2006/11/10 09:33:01 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class IntensityExtEntropy extends IntensityExtractor {
  public static String[] diclistc = {"_riet_entropyL_iteration_max", "_riet_entropyL_error_max", "_riet_entropyL_range_factor",
                                     "_riet_entropyL_relax_factor", "_riet_entropyL_use_hkl", "_riet_entropyL_summation_delta",
                                     "_riet_entropyL_use_weight", "_riet_entropyL_step_factor"};
  public static String[] diclistcrm = {"_riet_entropyL_iteration_max", "_riet_entropyL_error_max", "_riet_entropyL_range_factor",
                                     "_riet_entropyL_relax_factor", "_riet_entropyL_use_hkl", "_riet_entropyL_summation_delta",
                                     "_riet_entropyL_use_weight", "_riet_entropyL_step_factor"};
  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  public IntensityExtEntropy(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "EntropyL";
    IDlabel = "EntropyL";
    description = "select this to apply an entropy method for intensity extraction";
  }

  public IntensityExtEntropy(XRDcat aobj) {
    this(aobj, "Entropy Ext method");
  }

  public IntensityExtEntropy() {
    identifier = "EntropyL";
    IDlabel = "EntropyL";
    description = "select this to apply an entropy method for intensity extraction";
  }

  public void initConstant() {
    Nstring = 8;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
    System.arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
  }

  public void initParameters() {
    super.initParameters();
    setIterationMax(3);
    setErrorMax(0.005);
    setRangeFactor(0.05);
    setRelaxFactor(1.0);
    setUseHkl(false);
    setDeltaMax(0.0001);
    setUseWgt(false);
    setStepFactor(1.2);
  }

  public String getIterationMaximum() {
    return stringField[0];
  }

  public int getIterationMax() {
    return Integer.valueOf(getIterationMaximum());
  }

  public void setIterationMax(String value) {
    stringField[0] = value;
  }

  public void setIterationMax(int value) {
    setIterationMax(Integer.toString(value));
  }

  public String getErrorMaximum() {
    return stringField[1];
  }

  public double getErrorMax() {
    return Double.valueOf(getErrorMaximum());
  }

  public void setErrorMax(String value) {
    stringField[1] = value;
  }

  public void setErrorMax(double value) {
    setErrorMax(Double.toString(value));
  }

  public String getDeltaMaximum() {
    return stringField[5];
  }

  public double getDeltaMax() {
    return Double.valueOf(getDeltaMaximum());
  }

  public void setDeltaMax(String value) {
    stringField[5] = value;
  }

  public void setDeltaMax(double value) {
    setDeltaMax(Double.toString(value));
  }

  public String getRangeFactor() {
    return stringField[2];
  }

  public double getRangeFactorD() {
    return Double.valueOf(getRangeFactor());
  }

  public void setRangeFactor(String value) {
    stringField[2] = value;
  }

  public void setRangeFactor(double value) {
    setRangeFactor(Double.toString(value));
  }

  public String getRelaxFactor() {
    return stringField[3];
  }

  public double getRelaxFactorD() {
    return Double.valueOf(getRelaxFactor());
  }

  public void setRelaxFactor(double value) {
    setRelaxFactor(Double.toString(value));
  }

  public void setRelaxFactor(String value) {
    stringField[3] = value;
  }

  public String getUseHkl() {
    return stringField[4];
  }

  public boolean getUseHklB() {
    if (((DataFileSet) getParent()).getInstrument().getMeasurementS().startsWith("2Theta")) {
      setUseHkl(false);
    }
    return getUseHkl().equalsIgnoreCase("true");
  }

  public void setUseHkl(boolean value) {
    if (value)
      setUseHkl("true");
    else
      setUseHkl("false");
  }

  public void setUseHkl(String value) {
    stringField[4] = value;
  }

  public String getUseWgt() {
    return stringField[6];
  }

  public boolean getUseWgtB() {
    return getUseWgt().equalsIgnoreCase("true");
  }

  public void setUseWgt(boolean value) {
    if (value)
      setUseWgt("true");
    else
      setUseWgt("false");
  }

  public void setUseWgt(String value) {
    stringField[6] = value;
  }

  public String getStepFactor() {
    return stringField[7];
  }

  public double getStepFactorD() {
    return Double.valueOf(getStepFactor());
  }

  public void setStepFactor(double value) {
    setStepFactor(Double.toString(value));
  }

  public void setStepFactor(String value) {
    stringField[7] = value;
  }

  /*
EntropyL algorithm for PF extraction

The EntropyL method is a iterative scheme to extract the peak intensities from a powder spectrum.
The principal formula as used in our procedure is the following (without considering the background
for simplicity) for the n-th iteration:

PF (h ,y  ) = PF (h ,y  ) * SUM (Yobs(y ,d ) S(d  ,y  ;d  -d )/SUM(S(d  ,y ;d  -d )PF   (h  ,y )I  (y )))
n  k  j      n-1 k  j      i         j  i     h   j   h   i   r     h   j  h   i   n-1  r   j  h   j
k       k             r      r                   r

where all the symbols used are defined above (the D-spectrum model) except that in the PF symbol
appears the subscript n or n-1. In the first iteration, we start with all PF's equal to 1.
The PF's of peaks of the same family are imposed equals for all the lines.

  */

  public void extractIntensities(Sample asample) {

//		System.out.println("Extracting factors by EntropyL: " + getDataFile().toXRDcatString());

    Vector<Peak> tpeaklist;
    DiffrDataFile datafile = getDataFile();
    DataFileSet datafileset = datafile.getDataFileSet();
    Vector<Peak> fullpeaklist = datafileset.getPeakList();
    int numberofpeaks = datafileset.getNumberofPeaks();

    int datanumber = datafile.getTotalNumberOfData();
    double[] expfit = new double[datanumber];
    double[] fit = new double[datanumber];

    boolean useHKL = getUseHklB();

    double delta = getDeltaMax();
    if (delta == 0.0)
      delta = 1.0E-9;
    int ndelta;

    double decrease;

    int startingindex = datafile.startingindex;
    int finalindex = datafile.finalindex;

    double minBkg = datafile.getBkgFit(startingindex);
    double bkg;
    for (int j = startingindex; j < finalindex; j++) {
//      fit[j] = datafile.getFit(j);
      bkg = datafile.getBkgFit(j);
      if (bkg < minBkg)
        minBkg = bkg;
    }
    if (minBkg < 0.0)
      minBkg = - minBkg;
    else
      minBkg = 0.0;

    int minmaxindex[] = new int[2];
    minmaxindex[0] = startingindex;
    minmaxindex[1] = finalindex;

    double rangefactor = getRangeFactorD();

    boolean convergence = false;

    double diff[] = new double[numberofpeaks];
    int superOrder[] = new int[numberofpeaks];

    superOrder[0] = -1;
    for (int i = 0, n = 0; i < numberofpeaks; i++, n++) {
      superOrder[n] = -1;
      if (useHKL)
        for (int j = 0; j < i; j++)
          if (IntensityExtLeBail.checkOrder(fullpeaklist.elementAt(i), fullpeaklist.elementAt(j)))
            superOrder[n] = j;
    }

    int maxiter = getIterationMax();
    double stepchange = getStepFactorD();
    double relax = getRelaxFactorD() / stepchange;
    boolean useWgt = getUseWgtB();
    int iteration = 0;

    while (!convergence && iteration++ < maxiter) {
      if (iteration < 6)
        relax *= stepchange;
      else
        relax /= stepchange;
      for (int j = startingindex; j < finalindex; j++)
        fit[j] = 0.0f;
      datafile.computeReflectionIntensity(asample, fullpeaklist, true, fit, Constants.ENTIRE_RANGE,
              Constants.EXPERIMENTAL, Constants.COMPUTED, Constants.COMPUTED, false, null);
      datafile.computeasymmetry(asample, fit);
      datafile.postComputation(asample, fit);
      for (int j = startingindex; j < finalindex; j++)
        datafile.setPhasesFit(j, fit[j]);
      convergence = true;
      int i = 0, n = 0;
      int numberpeaktouse;
      while (i < numberofpeaks) {
        if (delta >= 0 && superOrder[n] < 0) {
          double pos = fullpeaklist.elementAt(i).getMeanPosition(); // to be corrected
          ndelta = 1;
          while (((i + ndelta) < numberofpeaks &&
                  (n + ndelta) < numberofpeaks) &&
                  (superOrder[n + ndelta] < 0 &&
                  Math.abs((pos - fullpeaklist.elementAt(i + ndelta).getMeanPosition())  // to be corrected
                  / pos) < delta))
            ndelta++;
        } else
          ndelta = 1;
        numberpeaktouse = ndelta;
        tpeaklist = StructureFactorLeBailExtractor.vectorcopy(fullpeaklist, i, numberpeaktouse);
        if (datafile.checkPeakInsideRange(tpeaklist.elementAt(0).getPhase(), tpeaklist.elementAt(0).getOrderPosition(),
		        rangefactor)
		        && tpeaklist.elementAt(0).intensityExtractionAllowed()) {
          double lebailfactor = 1.0;
          if (superOrder[n] < 0) {
            for (int j = minmaxindex[0]; j < minmaxindex[1]; j++)
              expfit[j] = 0.0f;
            minmaxindex = datafile.computeReflectionIntensity(asample, tpeaklist, false,
                    expfit, rangefactor, Constants.UNITARY,
                    Constants.COMPUTED, Constants.COMPUTED, true, null);
            double maxexpfit = 0.0;
            for (int k = minmaxindex[0]; k < minmaxindex[1]; k++)
              if (Math.abs(expfit[k]) > Math.abs(maxexpfit))
                maxexpfit = expfit[k];
/*						double totalint = 0.0;
						for (int ij = 0; ij < numberpeaktouse; ij++) {
							totalint += tpeaklist[ij].getFinalIntensity();
						}
						if (totalint > 1.0E-4)
							totalint = 1.0 / Math.sqrt(totalint);
						else
							totalint = 1.0;*/
//						System.out.println(maxexpfit);
            datafile.computeasymmetryandbkg(asample, expfit, minmaxindex[0], minmaxindex[1]);
            double expfitnorm = 0.0;
            decrease = (minmaxindex[1] - minmaxindex[0]) * 10.0;
            if (maxexpfit != 0.0)
              if (useWgt) {
                for (int k = minmaxindex[0]; k < minmaxindex[1]; k++) {
                  double expfitwgt = Math.sqrt(expfit[k] / maxexpfit);
                  lebailfactor *= Math.pow(Math.abs((datafile.getYData(k) + minBkg) / (datafile.getFit(k) + minBkg)),
                          expfitwgt * relax / decrease);
                  expfitnorm += expfitwgt;
                }
              } else {
                for (int k = minmaxindex[0]; k < minmaxindex[1]; k++) {
                  double expfitwgt = expfit[k] / maxexpfit;
                  lebailfactor *= Math.pow(Math.abs((datafile.getYData(k) + minBkg) / (datafile.getFit(k) + minBkg)),
                          expfitwgt * relax / decrease);
                  expfitnorm += expfitwgt;
                }
              }
            if (expfitnorm <= 0.0)
              lebailfactor = 0.0001;
            else
              lebailfactor = Math.pow(lebailfactor, decrease / (expfitnorm));
            if (lebailfactor <= 0.0)
              lebailfactor = 0.0001;
            for (int ij = 0; ij < ndelta; ij++) {
              double oldfactor = datafile.getExpTextureFactor(tpeaklist.elementAt(ij).getPhase(),
		              tpeaklist.elementAt(ij))[0];
              if (Double.isNaN(oldfactor))
                oldfactor = 1.0;
              double newlebailfactor = lebailfactor * oldfactor;
//						System.out.println(ij);
//						System.out.println(oldfactor);
//						System.out.println(lebailfactor);
//						System.out.println(newlebailfactor);
              if (oldfactor == 0.0)
                diff[n + ij] = Math.abs(newlebailfactor);
              else
                diff[n + ij] = Math.abs((newlebailfactor - oldfactor) / oldfactor);
              if (diff[n + ij] > getErrorMax())
                convergence = false;
              datafile.setExpTextureFactor(tpeaklist.elementAt(ij).getPhase(), tpeaklist.elementAt(ij), 0, newlebailfactor);
            }
          } else {
            lebailfactor = datafile.getExpTextureFactor(fullpeaklist.elementAt(superOrder[n]).getPhase(),
		            fullpeaklist.elementAt(superOrder[n]))[0];
            if (Double.isNaN(lebailfactor))
              lebailfactor = 1.0;
            numberpeaktouse = 1;
            ndelta = 1;
            for (int k = 0; k < numberpeaktouse; k++)
              datafile.setExpTextureFactor(tpeaklist.elementAt(k).getPhase(), tpeaklist.elementAt(k), 0, lebailfactor);
          }
        }
        i += numberpeaktouse;
        n += ndelta;
      }
    } // convergence or maxiteration reached

//		datafile.computeReflectionIntensity();
//		datafile.computeasymmetry();
//    notifyObjectChanged(this);
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    return new JELIntExtOptionsD(parent, this);
  }

  class JELIntExtOptionsD extends JOptionsDialog {

    JSlider iterationJS;
    JTextField maxerrorTF;
    JTextField rangefactorTF;
    JTextField deltafactorTF;
    JTextField relaxfactorTF;
    JCheckBox usehklCB;
    JCheckBox usewgtCB;
    JTextField stepfactorTF;

    public JELIntExtOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout());
      JPanel subpanel = new JPanel();
      subpanel.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      principalPanel.add(subpanel, BorderLayout.NORTH);
      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      subpanel.add(jPanel8);
      jPanel8.add(new JLabel("Number of iterations: "));
      JLabel iterationTF = new JLabel();
      iterationJS = new JSlider();
      iterationJS.setToolTipText("Maximum number of EntropyL iterations during a cycle");
      SliderListener listener = new SliderListener(iterationTF);
      iterationJS.addChangeListener(listener);
      jPanel8.add(iterationTF);
      jPanel8.add(iterationJS);
      subpanel = new JPanel();
      subpanel.setLayout(new GridLayout(5, 1, 6, 6));
      principalPanel.add(subpanel, BorderLayout.CENTER);
      JPanel jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      subpanel.add(jPanel9);
      jPanel9.add(new JLabel("Convergence error: "));
      maxerrorTF = new JTextField(Constants.FLOAT_FIELD);
      maxerrorTF.setToolTipText("Iterations stop when all intensities change less than this value");
      jPanel9.add(maxerrorTF);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      subpanel.add(jPanel9);
      jPanel9.add(new JLabel("Range factor: "));
      rangefactorTF = new JTextField(Constants.FLOAT_FIELD);
      rangefactorTF.setToolTipText("Factor for peak range computation (1 = the entire peak range)");
      jPanel9.add(rangefactorTF);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      subpanel.add(jPanel9);
      jPanel9.add(new JLabel("Summation range: "));
      deltafactorTF = new JTextField(Constants.FLOAT_FIELD);
      deltafactorTF.setToolTipText("All peaks closer than this range (relative to position) will be used togheter");
      jPanel9.add(deltafactorTF);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      subpanel.add(jPanel9);
      jPanel9.add(new JLabel("Relaxation factor: "));
      relaxfactorTF = new JTextField(Constants.FLOAT_FIELD);
      relaxfactorTF.setToolTipText("Set the exponent or relax factor for the EntropyL computation");
      jPanel9.add(relaxfactorTF);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      subpanel.add(jPanel9);
      jPanel9.add(new JLabel("Step factor: "));
      stepfactorTF = new JTextField(Constants.FLOAT_FIELD);
      stepfactorTF.setToolTipText("Set the change for the relax factor at any step");
      jPanel9.add(stepfactorTF);
      subpanel = new JPanel();
      subpanel.setLayout(new GridLayout(2, 1, 6, 6));
      principalPanel.add(subpanel, BorderLayout.SOUTH);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      subpanel.add(jPanel9);
      usehklCB = new JCheckBox("Use hkl information");
      usehklCB.setToolTipText("Check the box to impose hkl peaks of different order to have the same texture");
      jPanel9.add(usehklCB);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      subpanel.add(jPanel9);
      usewgtCB = new JCheckBox("Weighted method");
      usewgtCB.setToolTipText("Check the box to use a weighted EntropyL method");
      jPanel9.add(usewgtCB);

      setTitle("EntropyL options");
      initParameters();
      pack();
      iterationJS.setValue(getIterationMax());
    }

    public void initParameters() {
      iterationJS.setMaximum(100);
      iterationJS.setMinimum(0);
      iterationJS.setValue(100);
      iterationJS.setPaintTicks(true);
      iterationJS.setMajorTickSpacing(10);
      iterationJS.setMinorTickSpacing(5);

      iterationJS.setPaintLabels(true);
      iterationJS.setSnapToTicks(false);

      iterationJS.setLabelTable(iterationJS.createStandardLabels(20));

      maxerrorTF.setText(getErrorMaximum());
      rangefactorTF.setText(getRangeFactor());
      deltafactorTF.setText(getDeltaMaximum());
      relaxfactorTF.setText(getRelaxFactor());
      usehklCB.setSelected(getUseHklB());
      usewgtCB.setSelected(getUseWgtB());
      stepfactorTF.setText(getStepFactor());
    }

    public void retrieveParameters() {
      setIterationMax(iterationJS.getValue());
      setErrorMax(maxerrorTF.getText());
      setDeltaMax(deltafactorTF.getText());
      setRangeFactor(rangefactorTF.getText());
      setRelaxFactor(relaxfactorTF.getText());
      setUseHkl(usehklCB.isSelected());
      setUseWgt(usewgtCB.isSelected());
      setStepFactor(stepfactorTF.getText());
    }
  }

  class SliderListener implements ChangeListener {
    JLabel tf;

    public SliderListener(JLabel f) {
      tf = f;
    }

    public void stateChanged(ChangeEvent e) {
      JSlider s1 = (JSlider) e.getSource();
      tf.setText(Integer.toString(s1.getValue()));
    }
  }

}
