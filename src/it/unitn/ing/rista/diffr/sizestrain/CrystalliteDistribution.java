/*
 * @(#)CrystalliteDistribution.java created Jul 24, 2004 Braila
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.sizestrain;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.ParameterPreferences;
import it.unitn.ing.rista.util.MoreMath;

import javax.swing.*;
import java.awt.*;

import com.imsl.math.Sfun;
import cern.jet.stat.Probability;
import JSci.maths.SpecialMath;


/**
 * The CrystalliteDistribution is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:58 $
 * @since JDK1.1
 */

public class CrystalliteDistribution extends XRDcat {

  protected static String[] diclistc = {"_riet_par_distribution_type", "_riet_par_distribution_weight",
                                        "_riet_par_distribution_size_delta", "_riet_par_distribution_size_variance"};
  protected static String[] diclistcrm = {"_riet_par_distribution_type", "weight (fraction)",
                                        "deviation from mean size (angstrom)", "variance"};

  protected static String[] classlistc = {};

  public static String[] distributionType = {"Lognormal", "Gamma"}; //, "Lognormal_alt"};
  public int distributionTypeN = 0;
  public double weight = 0.0, delta = 0.0, variance = 0.0;
  public double nuFcoeff0n, nuFcoeff0d, nuFcoeff1n, nuFcoeff1d, nuFcoeff3n, nuFcoeff3d, vara, var2, var3, overC;

  public CrystalliteDistribution(XRDcat obj, String alabel) {
    super(obj, alabel);
    initBaseObject();
  }

  public CrystalliteDistribution(XRDcat afile) {
    this(afile, "Crystallite Distribution x");
  }

	public CrystalliteDistribution() {}

  public void initConstant() {
    Nstring = 1;
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
    stringField[0] = new String(distributionType[0]);
    parameterField[0] = new Parameter(this, getParameterString(0), 1.0,
        ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(0) + ".max", 1.0));
    parameterField[0].setPositiveOnly();
    parameterField[1] = new Parameter(this, getParameterString(1), 0.0,
        ParameterPreferences.getDouble(getParameterString(1) + ".min", -3000.0),
        ParameterPreferences.getDouble(getParameterString(1) + ".max", +3000.0));
    parameterField[2] = new Parameter(this, getParameterString(2), 0.5,
        ParameterPreferences.getDouble(getParameterString(2) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(2) + ".max", 1.0));
    parameterField[2].setPositiveOnly();
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

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);
    updateEverything();
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);
    parameterField[0].setPositiveOnly();
    parameterField[2].setPositiveOnly();

    updateEverything();
  }

  public void updateEverything() {
    distributionTypeN = getCrystalliteDistributionTypeNumber();
    weight = Math.abs(getCrystalliteDistributionWeight().getValueD());
    delta = getCrystalliteDistributionDelta().getValueD();
    variance = Math.abs(getCrystalliteDistributionVariance().getValueD());
    if (variance < 1E-8)
      variance = 1E-8;
    double var1 = 1.0 + variance;
    switch (distributionTypeN) {
      case 0:
        var2 = var1 * var1 * 2.0;
        var3 = var2 * var1;
        var3 = 1.0 / var3;
        var2 = -3.0 / var2;
        nuFcoeff0n = Math.pow(var1, -2.5);
        nuFcoeff0d = 1.0 / Math.sqrt(2.0 * Math.log(var1));
        nuFcoeff1n = Math.pow(var1, -1.5);
        nuFcoeff3n = Math.pow(var1, 0.5);
        break;
      case 1:
        var2 = 0.5 / (1.0 + 2.0 * variance);
        vara = 2.0 / 3.0 / (1.0 + 3.0 * variance);
        var3 = var2 / var1;
        var2 = -3.0 * var2;
        overC = 1.0 / variance;
        nuFcoeff0n = overC + 3.0;
        nuFcoeff0d = 1.0 / incompleteGamma(nuFcoeff0n, 0.0);
        nuFcoeff1n = nuFcoeff0n - 1;
        nuFcoeff1d = 1.0 / incompleteGamma(nuFcoeff1n, 0.0);
        nuFcoeff3n = nuFcoeff0n - 3;
        nuFcoeff3d = 1.0 / incompleteGamma(nuFcoeff3n, 0.0);
        break;
      case 2:
        var2 = 1.0 / (variance * Constants.SQRTPI2);
        var3 = -1.0 / (2.0 * variance * variance);
        break;
      default:
        {
        }
    }
  }

  public int getCrystalliteDistributionTypeNumber() {
    for (int i = 0; i < distributionType.length; i++)
      if (getCrystalliteDistributionType().equalsIgnoreCase(distributionType[i]))
        return i;
    stringField[0] = new String(distributionType[0]);
    return 0;
  }

  public String getCrystalliteDistributionType() {
    return stringField[0];
  }

  public void setCrystalliteDistributionType(int type) {
    if (type < distributionType.length && !getCrystalliteDistributionType().equals(distributionType[type]))
      stringField[0] = new String(distributionType[type]);
  }

  public void setCrystalliteDistributionType(String type) {
    if (type != null && !getCrystalliteDistributionType().equals(type))
      stringField[0] = new String(type);
  }

  public Parameter getCrystalliteDistributionWeight() {
    return parameterField[0];
  }

  public Parameter getCrystalliteDistributionDelta() {
    return parameterField[1];
  }

  public Parameter getCrystalliteDistributionVariance() {
    return parameterField[2];
  }

  public double[] getCrystalliteDistribution(int Ldivision, double diameter, double deltaL) {
    double[] size = new double[Ldivision];

//    System.out.println("diameter " + diameter);
// For plot only, no weight
    diameter += delta;
    switch (distributionTypeN) {
      case 0:
        double L = 0.0;
        size[0] = 0.0;
//    System.out.println(size[0]);
        diameter *= 2.666666666666667 * var3;
        double den1 = 1.0 / (Math.sqrt(2.0 * Constants.PI * Math.log(1.0 + variance)));
        double den2 = -1.0 / (2.0 * Math.log(1.0 + variance));
        double Rsqrt = Math.sqrt(1.0 + variance) / diameter * 2.0;
        for (int i = 1; i < Ldivision; i++) {
          L += deltaL;
          double log2 = Math.log(L * Rsqrt);
          size[i] = 1.0 / L * den1 * Math.exp(den2 * log2 * log2);
//      System.out.println(size[i]);
        }
        break;
      case 4:
        diameter *= 2.0 * vara;  // Dv
        L = 0.0;
        size[0] = weight;
        for (int i = 1; i < size.length; i++) {
          L += deltaL;
          double Lover2R = L / diameter;
          double Lover2R3 = Lover2R * Lover2R * Lover2R;
          double Lover2RoverC = Lover2R * overC;
          double x0L = incompleteGamma(nuFcoeff0n, Lover2RoverC) * nuFcoeff0d;
          double x1L = incompleteGamma(nuFcoeff1n, Lover2RoverC) * nuFcoeff1d;
          double x3L = incompleteGamma(nuFcoeff3n, Lover2RoverC) * nuFcoeff3d;
          size[i] = weight * (x0L + x1L * Lover2R * var2 + x3L * Lover2R3 * var3);
//          size[i] = incompleteGamma(nuFcoeff0n, Lover2RoverC);
        }
        break;
      case 1:
        L = 0.0;
        size[0] = 0.0;
        diameter *= vara;  // Dv / 2
        double gammac = Math.pow(overC, overC) / (diameter * SpecialMath.gamma(overC));
        for (int i = 1; i < Ldivision; i++) {
          L += deltaL;
          double LoverR = L / diameter;
          size[i] = gammac * Math.pow(LoverR, overC - 1.0) * Math.exp(- LoverR * overC);
        }
        break;
      case 2:
        L = 0.0;
        size[0] = 0.0;
//    System.out.println(size[0]);
        double variance2 = variance * variance;
        diameter = Math.log(diameter) - variance2 / 2.0;
        for (int i = 1; i < Ldivision; i++) {
          L += deltaL;
          double logDgamma = Math.log(L) - diameter;
          size[i] = var2 / L * Math.exp(var3 * logDgamma * logDgamma);
//      System.out.println(size[i]);
        }
        break;
      default:
        {
        }
    }
    return size;
  }

  public static double incompleteGamma(double a, double x) {
    return cern.jet.stat.Gamma.incompleteGammaComplement(a, x);
  }

  public static double gamma(double x) {
    return cern.jet.stat.Gamma.gamma(x);
  }

  public double[] getCrystalliteDistribution(double size) {
    return getCrystalliteDistribution(5000, size, 1);
  }

  public void computeSizeCoefficients(double[] sizeCoeff, double diameter, double deltaL) {
    diameter += delta;
//    deltaL *= 2.0;
    switch (distributionTypeN) {
      case 0:
        diameter *= 2.666666666666667 * var3;  // Dv
        double L = 0.0;
        sizeCoeff[0] = weight;
        for (int i = 1; i < sizeCoeff.length; i++) {
          L += deltaL;
          double Lover2R = L / diameter;
          double Lover2R3 = Lover2R * Lover2R * Lover2R;
          double v0L = Math.log(Lover2R * nuFcoeff0n) * nuFcoeff0d;
          double v1L = Math.log(Lover2R * nuFcoeff1n) * nuFcoeff0d;
          double v3L = Math.log(Lover2R * nuFcoeff3n) * nuFcoeff0d;
          sizeCoeff[i] = 0.5 * weight * (Sfun.erfc(v0L) +
              Sfun.erfc(v1L) * Lover2R * var2 +
              Sfun.erfc(v3L) * Lover2R3 * var3);
        }
        break;
      case 1:
        diameter *= 2.0 * vara;  // Dv
        L = 0.0;
        sizeCoeff[0] = weight;
        for (int i = 1; i < sizeCoeff.length; i++) {
          L += deltaL;
          double Lover2R = L / diameter;
          double Lover2R3 = Lover2R * Lover2R * Lover2R;
          double Lover2RoverC = Lover2R * overC;
          double x0L = incompleteGamma(nuFcoeff0n, Lover2RoverC) * nuFcoeff0d;
          double x1L = incompleteGamma(nuFcoeff1n, Lover2RoverC) * nuFcoeff1d;
          double x3L = incompleteGamma(nuFcoeff3n, Lover2RoverC) * nuFcoeff3d;
          sizeCoeff[i] = weight * (x0L + x1L * Lover2R * var2 + x3L * Lover2R3 * var3);
        }
        break;
      case 2:
        L = 0.0;
        double[] moment = new double[4];
        double variance2 = variance * variance;
        diameter = Math.log(diameter) - variance2 / 2.0;
        for (int i = 0; i < 4; i++)
          moment[i] = getDistributionMoment(i, diameter, variance);
        for (int i = 0; i < sizeCoeff.length; i++) {
          sizeCoeff[i] = 0.0;
          for (int n = 0; n < 4; n++) {
            sizeCoeff[i] += Hc[n] * moment[3 - n] / moment[3] *
                /*Sfun.erfc*/
              Probability.errorFunctionComplemented((Math.log(Kc * L) - diameter - variance2 * (3 - n)) /
                  (variance * Constants.sqrt2)) * MoreMath.pow(L, n);
          }
          sizeCoeff[i] *= weight;
          L += deltaL;
//          System.out.println(sizeCoeff[i]);
        }
        break;
      default:
        {
        }
    }
  }

  static double[] Hc = {1.0, -1.5, 0.0, 0.5};
  static double Kc = 1.0;

  public static double getDistributionMoment(int n, double mean, double variance) {
    return Math.exp(mean * n + variance * variance * n * n / 2.0);
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new CrystalliteDistribution.JCrystalliteDistributionDistributionOptionsD(parent, this);
    return adialog;
  }

  public class JCrystalliteDistributionDistributionOptionsD extends JOptionsDialog {

    JTextField sizeWeightTF;
    JTextField sizeDeltaTF;
    JTextField sizeVarianceTF;
    JComboBox distributionTypeCB;

    public JCrystalliteDistributionDistributionOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new GridLayout(1, 4, 1, 1));

      JPanel jp2 = new JPanel();
      jp2.setLayout(new GridLayout(0, 2, 4, 4));
      principalPanel.add(jp2);
      jp2.add(new JLabel("Distribution type:"));
      distributionTypeCB = new JComboBox();
      distributionTypeCB.setToolTipText("Select the distribution type");
      JPanel jp1 = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
      jp2.add(jp1);
      jp1.add(distributionTypeCB);

//      jp2 = new JPanel();
//      jp2.setLayout(new GridLayout(2, 1, 4, 4));
//      principalPanel.add(jp2);
      jp2.add(new JLabel("Weight:"));
      sizeWeightTF = new JTextField(Constants.FLOAT_FIELD);
      sizeWeightTF.setText("0");
      jp1 = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
      jp2.add(jp1);
      jp1.add(sizeWeightTF);

//      jp2 = new JPanel();
//      jp2.setLayout(new GridLayout(2, 1, 4, 4));
//      principalPanel.add(jp2);
      jp2.add(new JLabel("Crystallite delta:"));
      sizeDeltaTF = new JTextField(Constants.FLOAT_FIELD);
      sizeDeltaTF.setText("0");
      jp1 = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
      jp2.add(jp1);
      jp1.add(sizeDeltaTF);

//      jp2 = new JPanel();
//      jp2.setLayout(new GridLayout(2, 1, 4, 4));
//      principalPanel.add(jp2);
      jp2.add(new JLabel("Variance:"));
      sizeVarianceTF = new JTextField(Constants.FLOAT_FIELD);
      sizeVarianceTF.setText("0");
      jp1 = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
      jp2.add(jp1);
      jp1.add(sizeVarianceTF);

      initParameters();

      setTitle("Crystallite distribution coefficients");
      pack();
    }

    public void initParameters() {
      sizeWeightTF.setText(getCrystalliteDistributionWeight().getValue());
      addComponenttolist(sizeWeightTF, getCrystalliteDistributionWeight());
      sizeDeltaTF.setText(getCrystalliteDistributionDelta().getValue());
      addComponenttolist(sizeDeltaTF, getCrystalliteDistributionDelta());
      sizeVarianceTF.setText(getCrystalliteDistributionVariance().getValue());
      addComponenttolist(sizeVarianceTF, getCrystalliteDistributionVariance());
      for (int i = 0; i < distributionType.length; i++) {
        distributionTypeCB.addItem(distributionType[i]);
      }
      distributionTypeCB.setSelectedItem(getCrystalliteDistributionType());
    }

    public void retrieveParameters() {
      getCrystalliteDistributionWeight().setValue(sizeWeightTF.getText());
      getCrystalliteDistributionDelta().setValue(sizeDeltaTF.getText());
      getCrystalliteDistributionVariance().setValue(sizeVarianceTF.getText());
      setCrystalliteDistributionType(distributionTypeCB.getSelectedItem().toString());
    }

  }
}
