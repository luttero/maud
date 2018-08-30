/*
 * @(#)IntensityExtLeBail.java created 02/08/1998 Braila
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
 *  The IntensityExtLeBail is a class
 *
 *
 * @version $Revision: 1.11 $, $Date: 2006/12/04 14:30:04 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class IntensityExtLeBail extends IntensityExtractor {
  public static String[] diclistc = {"_riet_lebail_iteration_max", "_riet_lebail_error_max", "_riet_lebail_range_factor",
                                     "_riet_lebail_use_bkg", "_riet_lebail_use_hkl", "_riet_lebail_summation_delta"};
  public static String[] diclistcrm = {"_riet_lebail_iteration_max", "_riet_lebail_error_max", "_riet_lebail_range_factor",
                                     "_riet_lebail_use_bkg", "_riet_lebail_use_hkl", "_riet_lebail_summation_delta"};
  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean useBKG = false;

  public IntensityExtLeBail(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Le Bail";
    IDlabel = "Le Bail";
    description = "select this to apply Le Bail method for intensity extraction";
  }

  public IntensityExtLeBail(XRDcat aobj) {
    this(aobj, "Le Bail method");
  }

  public IntensityExtLeBail() {
    identifier = "Le Bail";
    IDlabel = "Le Bail";
    description = "select this to apply Le Bail method for intensity extraction";
  }

  public void initConstant() {
    Nstring = 6;
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
    setIterationMax(5);
    setErrorMax(0.005);
    setRangeFactor(0.05);
    setUseBkg(true);
    setUseHkl(false);
    setDeltaMax(0.0001);
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(firstLoading);

    useBKG = getUseBkgB();
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

  public String getUseBkg() {
    return stringField[3];
  }

  public boolean getUseBkgB() {
    return getUseBkg().equalsIgnoreCase("true");
  }

  public void setUseBkg(boolean value) {
    if (value)
      setUseBkg("true");
    else
      setUseBkg("false");
  }

  public void setUseBkg(String value) {
    stringField[3] = value;
  }

  public String getUseHkl() {
    return stringField[4];
  }

  public boolean getUseHklB() {
    Instrument instru = ((DataFileSet) getParent()).getInstrument();
    if (instru != null && instru.getMeasurementS().startsWith("2Theta")) {
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

  /*
Le Bail algorithm for PF extraction

The Le Bail method is a iterative scheme to extract the peak intensities from a powder spectrum.
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

//		System.out.println("Extracting factors by Le Bail: " + getDataFile().toXRDcatString());

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

    if (numberofpeaks > 0)
      superOrder[0] = -1;
    for (int i = 0, n = 0; i < numberofpeaks; i++, n++) {
      superOrder[n] = -1;
      if (useHKL)
        for (int j = 0; j < i; j++)
          if (checkOrder(fullpeaklist.elementAt(i), fullpeaklist.elementAt(j)))
            superOrder[n] = j;
    }

    int maxiter = getIterationMax();
    int iteration = 0;

    while (!convergence && iteration++ < maxiter) {
      for (int j = startingindex; j < finalindex; j++)
        fit[j] = 0.0f;
	    datafileset.computeReflectionIntensity(asample, fullpeaklist, true, fit, Constants.ENTIRE_RANGE,
              Constants.EXPERIMENTAL, Constants.COMPUTED, Constants.COMPUTED, false, null, datafile);
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
          while (i + ndelta < numberofpeaks &&
                  (superOrder[n + ndelta] < 0 &&
                  Math.abs((pos - fullpeaklist.elementAt(i + ndelta).getMeanPosition()) // to be corrected
                  / pos) < delta))
            ndelta++;
        } else
          ndelta = 1;
        numberpeaktouse = ndelta;
        tpeaklist = StructureFactorLeBailExtractor.vectorcopy(fullpeaklist, i, numberpeaktouse);
        if (datafile.checkPeakInsideRange(tpeaklist.elementAt(0).getPhase(), tpeaklist.elementAt(0).getOrderPosition(),
		        rangefactor)
		        && tpeaklist.elementAt(0).intensityExtractionAllowed()) {
          double lebailfactor = 0.0;
          if (superOrder[n] < 0) {
            for (int j = minmaxindex[0]; j < minmaxindex[1]; j++)
              expfit[j] = 0.0f;
            minmaxindex = datafileset.computeReflectionIntensity(asample, tpeaklist, false,
                    expfit, rangefactor, Constants.UNITARY,
                    Constants.COMPUTED, Constants.COMPUTED, true, null, datafile);
            datafile.computeasymmetryandbkg(asample, expfit, minmaxindex[0], minmaxindex[1]);
            double expfitnorm = 0.0;
						if (useBKG)
            for (int k = minmaxindex[0]; k < minmaxindex[1]; k++) {
              lebailfactor += Math.abs(expfit[k] * (datafile.getYData(k) + minBkg) / (datafile.getFit(k) + minBkg));
              expfitnorm += Math.abs(expfit[k]);
            }
						else
							for (int k = minmaxindex[0]; k < minmaxindex[1]; k++) {
								bkg =  datafile.getBkgFit(k);
								double difffb = datafile.getFit(k) - bkg;
								if (difffb > 1.0E-6) {
									lebailfactor += Math.abs(expfit[k] * (datafile.getYData(k) - bkg)
															/ difffb);
									expfitnorm += Math.abs(expfit[k]);
								}
							}
            if (expfitnorm <= 0.0)
              lebailfactor = 0.0001;
            else
              lebailfactor /= expfitnorm;
            if (lebailfactor < 0.0001)
              lebailfactor = 0.0001;
            for (int ij = 0; ij < ndelta; ij++) {
              double oldfactor = datafile.getExpTextureFactor(tpeaklist.elementAt(ij).getPhase(),
		              tpeaklist.elementAt(ij))[0]; // todo we need the other points per pattern
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

  public static boolean checkOrder(Peak peakn, Peak peak) {

    boolean superorder = false;
    int h = 0, k = 0, l = 0;

    if (peakn.getPhase() == peak.getPhase()) {
      Reflection refln = peakn.getReflex();
      Reflection refl = peak.getReflex();
      int h2 = 0;
      if (refl.getH() == 0 && refln.getH() == 0)
        h2 = 1;
      else if (refl.getH() != 0)
        h = refln.getH() / refl.getH();
      else
        h = -1320;
      int k2 = 0;
      if (refl.getK() == 0 && refln.getK() == 0)
        k2 = 1;
      else if (refl.getK() != 0)
        k = refln.getK() / refl.getK();
      else
        k = -1310;
      int l2 = 0;
      if (refl.getL() == 0 && refln.getL() == 0)
        l2 = 1;
      else if (refl.getL() != 0)
        l = refln.getL() / refl.getL();
      else
        l = -1300;
      if (h2 != 1 && (h * refl.getH() != refln.getH()))
        h = -1320;
      if (k2 != 1 && (k * refl.getK() != refln.getK()))
        h = -1320;
      if (l2 != 1 && (l * refl.getL() != refln.getL()))
        h = -1320;
      if (h2 == 1 || (h == k || k2 == 1))
        if (k2 == 1 || (k == l || l2 == 1))
          if (l2 == 1 || (h == l || h2 == 1))
            superorder = true;
    }

    return superorder;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    return new JLBIntExtOptionsD(parent, this);
  }

  class JLBIntExtOptionsD extends JOptionsDialog {

    JSlider iterationJS;
    JTextField maxerrorTF;
    JTextField rangefactorTF;
    JTextField deltafactorTF;
    JCheckBox usebkgCB;
    JCheckBox usehklCB;

    public JLBIntExtOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new GridLayout(6, 1, 6, 6));
      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      principalPanel.add(jPanel8);
      jPanel8.add(new JLabel("Number of iterations: "));
      JLabel iterationTF = new JLabel();
      iterationJS = new JSlider();
      iterationJS.setToolTipText("Maximum number of Le Bail iterations during a cycle");
      SliderListener listener = new SliderListener(iterationTF);
      iterationJS.addChangeListener(listener);
      jPanel8.add(iterationTF);
      jPanel8.add(iterationJS);
      JPanel jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      principalPanel.add(jPanel9);
      jPanel9.add(new JLabel("Convergence error: "));
      maxerrorTF = new JTextField(Constants.FLOAT_FIELD);
      maxerrorTF.setToolTipText("Iterations stop when all intensities change less than this value");
      jPanel9.add(maxerrorTF);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      principalPanel.add(jPanel9);
      jPanel9.add(new JLabel("Range factor: "));
      rangefactorTF = new JTextField(Constants.FLOAT_FIELD);
      rangefactorTF.setToolTipText("Factor for peak range computation (1 = the entire peak range)");
      jPanel9.add(rangefactorTF);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      principalPanel.add(jPanel9);
      jPanel9.add(new JLabel("Summation range: "));
      deltafactorTF = new JTextField(Constants.FLOAT_FIELD);
      deltafactorTF.setToolTipText("All peaks closer than this range (relative to position) will be used togheter");
      jPanel9.add(deltafactorTF);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      principalPanel.add(jPanel9);
      usebkgCB = new JCheckBox("Use background");
      usebkgCB.setToolTipText("Uncheck the box to subtract the background during the iterations");
      jPanel9.add(usebkgCB);
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      principalPanel.add(jPanel9);
      usehklCB = new JCheckBox("Use hkl information");
      usehklCB.setToolTipText("Check the box to impose hkl peaks of different order to have the same texture");
      jPanel9.add(usehklCB);

      setTitle("Le Bail options");
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
      usebkgCB.setSelected(getUseBkgB());
      usehklCB.setSelected(getUseHklB());
    }

    public void retrieveParameters() {
      setIterationMax(iterationJS.getValue());
      setErrorMax(maxerrorTF.getText());
      setDeltaMax(deltafactorTF.getText());
      setRangeFactor(rangefactorTF.getText());
      setUseBkg(usebkgCB.isSelected());
      setUseHkl(usehklCB.isSelected());
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
