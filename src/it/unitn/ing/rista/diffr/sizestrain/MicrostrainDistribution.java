/*
 * @(#)MicrostrainDistribution.java created Jul 24, 2004 Braila
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
import it.unitn.ing.rista.util.ParameterPreferences;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.awt.JOptionsDialog;

import javax.swing.*;
import java.awt.*;


/**
 * The MicrostrainDistribution is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:58 $
 * @since JDK1.1
 */

public class MicrostrainDistribution extends XRDcat {

  protected static String[] diclistc = {"_riet_par_distribution_type", "_riet_par_distribution_weight",
                                        "_riet_par_distribution_strain_delta", "_riet_par_distribution_strain_decay"};
  protected static String[] diclistcrm = {"_riet_par_distribution_type", "weight (fraction)",
                                        "deviation from mean value", "decay exponent"};

  protected static String[] classlistc = {};

  public static String[] distributionType = {"General"};
  public int distributionTypeN = 0;
  public double weight = 0.0, delta = 0.0, slope = 0.0;

  public MicrostrainDistribution(XRDcat obj, String alabel) {
    super(obj, alabel);
    initXRD();
  }

  public MicrostrainDistribution(XRDcat afile) {
    this(afile, "Microstrain Distribution x");
  }

	public MicrostrainDistribution() {}

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
        ParameterPreferences.getDouble(getParameterString(1) + ".min", -0.01),
        ParameterPreferences.getDouble(getParameterString(1) + ".max", +0.01));
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
    distributionTypeN = getMicrostrainDistributionTypeNumber();
    weight = Math.abs(getMicrostrainDistributionWeight().getValueD());
    delta = getMicrostrainDistributionDelta().getValueD();
    slope = getMicrostrainDistributionSlope().getValueD();
    switch (distributionTypeN) {
      case 0:
        break;
      default:
        {
        }
    }
  }

  public int getMicrostrainDistributionTypeNumber() {
    for (int i = 0; i < distributionType.length; i++)
      if (getMicrostrainDistributionType().equalsIgnoreCase(distributionType[i]))
        return i;
    stringField[0] = new String(distributionType[0]);
    return 0;
  }

  public String getMicrostrainDistributionType() {
    return stringField[0];
  }

  public void setMicrostrainDistributionType(int type) {
    if (type < distributionType.length && !getMicrostrainDistributionType().equals(distributionType[type]))
      stringField[0] = new String(distributionType[type]);
  }

  public void setMicrostrainDistributionType(String type) {
    if (type != null && !getMicrostrainDistributionType().equals(type))
      stringField[0] = new String(type);
  }

  public Parameter getMicrostrainDistributionWeight() {
    return parameterField[0];
  }

  public Parameter getMicrostrainDistributionDelta() {
    return parameterField[1];
  }

  public Parameter getMicrostrainDistributionSlope() {
    return parameterField[2];
  }

  public double[] getMicrostrainDistribution(int Ldivision, double microstrain, double diameter, double deltaL) {

    // only for plotting, no weight
    double[] strain = new double[Ldivision];
//    System.out.println("diameter " + diameter);

//    System.out.println("microstrain " + microstrain);


    double L = 0.0;
    strain[0] = 0.0;
    microstrain += delta;
    for (int i = 1; i < Ldivision; i++) {
      L += deltaL;
      strain[i] = microstrain / Math.pow(L / diameter * 2.0, slope);
//          System.out.println(strain[i]);
    }
    return strain;
  }

  public double[] getMicrostrainDistribution(double microstrain, double size) {
    return getMicrostrainDistribution(5000, microstrain, size, 1);
  }

  public void computeStrainCoefficients(double[] strainCoeff, double microstrain, double diameter, double deltaL,
                                      double dspacing) {
    microstrain += delta;
//    microstrain *= microstrain;
    dspacing *= dspacing;
//    deltaL *= 2.0;
    double c1 = - 2.0 * Constants.PI_QUADRO / dspacing;
    switch (distributionTypeN) {
      case 0:
        double L = 0;
        strainCoeff[0] = weight;
        for (int i = 1; i < strainCoeff.length; i++) {
          L += deltaL;
          double distr = microstrain / Math.pow(L / diameter * 2.0, slope);
          strainCoeff[i] = weight * Math.exp(c1 * distr * distr * L * L);
        }
        break;
      default:
        {
        }
    }
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new MicrostrainDistribution.JMicrostrainDistributionDistributionOptionsD(parent, this);
    return adialog;
  }

  public class JMicrostrainDistributionDistributionOptionsD extends JOptionsDialog {

    JTextField strainWeightTF;
    JTextField strainDeltaTF;
    JTextField strainSlopeTF;
    JComboBox distributionTypeCB;

    public JMicrostrainDistributionDistributionOptionsD(Frame parent, XRDcat obj) {

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
      strainWeightTF = new JTextField(Constants.FLOAT_FIELD);
      strainWeightTF.setText("0");
      jp1 = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
      jp2.add(jp1);
      jp1.add(strainWeightTF);

//      jp2 = new JPanel();
//      jp2.setLayout(new GridLayout(2, 1, 4, 4));
//      principalPanel.add(jp2);
      jp2.add(new JLabel("Microstrain delta:"));
      strainDeltaTF = new JTextField(Constants.FLOAT_FIELD);
      strainDeltaTF.setText("0");
      jp1 = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
      jp2.add(jp1);
      jp1.add(strainDeltaTF);

//      jp2 = new JPanel();
//      jp2.setLayout(new GridLayout(2, 1, 4, 4));
//      principalPanel.add(jp2);
      jp2.add(new JLabel("Slope coefficient:"));
      strainSlopeTF = new JTextField(Constants.FLOAT_FIELD);
      strainSlopeTF.setText("0");
      jp1 = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
      jp2.add(jp1);
      jp1.add(strainSlopeTF);

      initParameters();

      setTitle("Size-strain distribution coefficients");
      pack();
    }

    public void initParameters() {
      strainWeightTF.setText(getMicrostrainDistributionWeight().getValue());
      addComponenttolist(strainWeightTF, getMicrostrainDistributionWeight());
      strainDeltaTF.setText(getMicrostrainDistributionDelta().getValue());
      addComponenttolist(strainDeltaTF, getMicrostrainDistributionDelta());
      strainSlopeTF.setText(getMicrostrainDistributionSlope().getValue());
      addComponenttolist(strainSlopeTF, getMicrostrainDistributionSlope());
      for (int i = 0; i < distributionType.length; i++) {
        distributionTypeCB.addItem(distributionType[i]);
      }
      distributionTypeCB.setSelectedItem(getMicrostrainDistributionType());
    }

    public void retrieveParameters() {
      getMicrostrainDistributionWeight().setValue(strainWeightTF.getText());
      getMicrostrainDistributionDelta().setValue(strainDeltaTF.getText());
      getMicrostrainDistributionSlope().setValue(strainSlopeTF.getText());
      setMicrostrainDistributionType(distributionTypeCB.getSelectedItem().toString());
    }

  }
}
