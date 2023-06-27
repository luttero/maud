/*
 * @(#)VoigtReussStress.java created 27/08/2002 Seoul, Korea
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

package it.unitn.ing.rista.diffr.rsa;

import java.awt.*;
import javax.swing.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

/**
 *  The VoigtReussStress is a class to compute the diffraction shift from
 *  the stress tensor using a weighted mean between Reuss and Voigt
 *  approximation.
 *  See N. C. Popa, J. Appl. Cryst. (2000), 33, 103-107.
 *
 * @version $Revision: 1.10 $, $Date: 2005/05/06 18:07:27 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class VoigtReussStress extends Strain {

  public static String[] diclistc = {
    "_rista_voigt_reuss_weight",
    "_rista_compliance_11_GPa", // 1
    "_rista_compliance_12_GPa", // 2
    "_rista_compliance_13_GPa", // 3
    "_rista_compliance_14_GPa", // 4
    "_rista_compliance_15_GPa", // 5
    "_rista_compliance_16_GPa", // 6
//                                      "_rista_compliance_21_GPa",
    "_rista_compliance_22_GPa", // 7
    "_rista_compliance_23_GPa", // 8
    "_rista_compliance_24_GPa", // 9
    "_rista_compliance_25_GPa", // 10
    "_rista_compliance_26_GPa", // 11
//                                      "_rista_compliance_31_GPa",
//                                      "_rista_compliance_32_GPa",
    "_rista_compliance_33_GPa", // 12
    "_rista_compliance_34_GPa", // 13
    "_rista_compliance_35_GPa", // 14
    "_rista_compliance_36_GPa", // 15
//                                      "_rista_compliance_41_GPa",
//                                      "_rista_compliance_42_GPa",
//                                      "_rista_compliance_43_GPa",
    "_rista_compliance_44_GPa", // 16
    "_rista_compliance_45_GPa", // 17
    "_rista_compliance_46_GPa", // 18
//                                      "_rista_compliance_51_GPa",
//                                      "_rista_compliance_52_GPa",
//                                      "_rista_compliance_53_GPa",
//                                      "_rista_compliance_54_GPa",
    "_rista_compliance_55_GPa", // 19
    "_rista_compliance_56_GPa", // 20
//                                      "_rista_compliance_61_GPa",
//                                      "_rista_compliance_62_GPa",
//                                      "_rista_compliance_63_GPa",
//                                      "_rista_compliance_64_GPa",
//                                      "_rista_compliance_65_GPa",
    "_rista_compliance_66_GPa", // 21
    "_rista_macrostress_11_MPa",
    "_rista_macrostress_22_MPa",
    "_rista_macrostress_33_MPa",
    "_rista_macrostress_23_MPa",
    "_rista_macrostress_13_MPa",
    "_rista_macrostress_12_MPa"
  };
  public static String[] diclistcrm = {
    "_rista_voigt_reuss_weight",
    "_rista_compliance_11_GPa", // 1
    "_rista_compliance_12_GPa", // 2
    "_rista_compliance_13_GPa", // 3
    "_rista_compliance_14_GPa", // 4
    "_rista_compliance_15_GPa", // 5
    "_rista_compliance_16_GPa", // 6
//                                      "_rista_compliance_21_GPa",
    "_rista_compliance_22_GPa", // 7
    "_rista_compliance_23_GPa", // 8
    "_rista_compliance_24_GPa", // 9
    "_rista_compliance_25_GPa", // 10
    "_rista_compliance_26_GPa", // 11
//                                      "_rista_compliance_31_GPa",
//                                      "_rista_compliance_32_GPa",
    "_rista_compliance_33_GPa", // 12
    "_rista_compliance_34_GPa", // 13
    "_rista_compliance_35_GPa", // 14
    "_rista_compliance_36_GPa", // 15
//                                      "_rista_compliance_41_GPa",
//                                      "_rista_compliance_42_GPa",
//                                      "_rista_compliance_43_GPa",
    "_rista_compliance_44_GPa", // 16
    "_rista_compliance_45_GPa", // 17
    "_rista_compliance_46_GPa", // 18
//                                      "_rista_compliance_51_GPa",
//                                      "_rista_compliance_52_GPa",
//                                      "_rista_compliance_53_GPa",
//                                      "_rista_compliance_54_GPa",
    "_rista_compliance_55_GPa", // 19
    "_rista_compliance_56_GPa", // 20
//                                      "_rista_compliance_61_GPa",
//                                      "_rista_compliance_62_GPa",
//                                      "_rista_compliance_63_GPa",
//                                      "_rista_compliance_64_GPa",
//                                      "_rista_compliance_65_GPa",
    "_rista_compliance_66_GPa", // 21
    "_rista_macrostress_11_MPa",
    "_rista_macrostress_22_MPa",
    "_rista_macrostress_33_MPa",
    "_rista_macrostress_23_MPa",
    "_rista_macrostress_13_MPa",
    "_rista_macrostress_12_MPa"
  };

  public static String[] classlistcs = {};
  public static String[] classlistc = {};

  Sample actualsample = null;
//	int actuallayer = 0;

  public VoigtReussStress(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Voigt-Reuss Stress";
    IDlabel = "Voigt-Reuss Stress";
    description = "select this to apply the Voigt-Reuss mean stress model";
  }

  public VoigtReussStress(XRDcat aobj) {
    this(aobj, "Voigt-Reuss Stress");
  }

  public VoigtReussStress() {
    identifier = "Voigt-Reuss Stress";
    IDlabel = "Voigt-Reuss Stress";
    description = "select this to apply the Voigt-Reuss mean stress model";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 28;
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
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
  }

  public void initParameters() {
    super.initParameters();

    double s11 = 0.01;
    double s12 = 0.003;
    parameterField[0] = new Parameter(this, getParameterString(0),
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 0),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 1));

    parameterField[1] = new Parameter(this, getParameterString(1), s11);
    parameterField[7] = new Parameter(this, getParameterString(7), s11);
    parameterField[12] = new Parameter(this, getParameterString(12), s11);
    parameterField[2] = new Parameter(this, getParameterString(2), s12);
    parameterField[3] = new Parameter(this, getParameterString(3), s12);
    parameterField[8] = new Parameter(this, getParameterString(8), s12);
    parameterField[16] = new Parameter(this, getParameterString(16), 2.0 * (s11 - s12));
    parameterField[19] = new Parameter(this, getParameterString(19), 2.0 * (s11 - s12));
    parameterField[21] = new Parameter(this, getParameterString(21), 2.0 * (s11 - s12));

    refreshComputation = true;
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
            notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED, -1);
            notifyParameterChanged(source, Constants.STRAIN_CHANGED, -1);
            return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED, -1);
            notifyParameterChanged(source, Constants.STRAIN_CHANGED, -1);
            return;
          }

      super.notifyParameterChanged(source);
    }
  }

  double[] macrostrain = new double[6];
  public static final int[] rhoMatrix = {1, 1, 1, 2, 2, 2};

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    super.updateParametertoDoubleBuffering(false);

    double[][] complianceMatrix = new double[6][6];
    int k = 0;
    for (int i = 0; i < 6; i++)
      for (int j = 0; j < 6; j++) {
        if (i >= j)
          complianceMatrix[i][j] = parameterValues[k++];
        else
          complianceMatrix[i][j] = complianceMatrix[j][i];
      }
    checkForSymmetries(complianceMatrix);
  }

  public static double[][] getEulerMatrix(double phi, double beta, double psi, double gamma,
                                          double omega) {

    double[][] amat = new double[3][3];
    double sinPhi = Math.sin(phi);
    double cosPhi = Math.cos(phi);
    double sinBeta = Math.sin(beta);
    double cosBeta = Math.cos(beta);
    double sinPsi = Math.sin(psi);
    double cosPsi = Math.cos(psi);
    double sinGamma = Math.sin(gamma);
    double cosGamma = Math.cos(gamma);
    double sinOmega = Math.sin(omega);
    double cosOmega = Math.cos(omega);

    double[][] eulerMat = new double[3][3];
    if (omega == 0.0) {
      eulerMat[0][0] = sinBeta * sinGamma
              + (cosPhi * cosBeta * cosPsi + sinPhi * cosBeta * sinPsi) * cosGamma;
      eulerMat[0][1] = -sinBeta * cosGamma
              + (cosPhi * cosBeta * cosPsi + sinPhi * cosBeta * sinPsi) * sinGamma;
      eulerMat[0][2] = -cosPhi * cosBeta * sinPsi + sinPhi * cosBeta * cosPsi;
      eulerMat[1][0] = -cosBeta * sinGamma
              + (cosPhi * sinBeta * cosPsi + sinPhi * sinBeta * sinPsi) * cosGamma;
      eulerMat[1][1] = cosBeta * cosGamma
              + (cosPhi * sinBeta * cosPsi + sinPhi * sinBeta * sinPsi) * sinGamma;
      eulerMat[1][2] = -cosPhi * sinBeta * sinPsi + sinPhi * sinBeta * cosPsi;
      eulerMat[2][0] = (-sinPhi * cosPsi + cosPhi * sinPsi) * cosGamma;
      eulerMat[2][1] = (-sinPhi * cosPsi + cosPhi * sinPsi) * sinGamma;
      eulerMat[2][2] = sinPhi * sinPsi + cosPhi * cosPsi;
    } else {
      double const1 = -sinPhi * cosPsi + cosPhi * sinPsi;
      double const2 = cosPhi * sinBeta * cosPsi + sinPhi * sinBeta * sinPsi;
      double const3 = cosPhi * cosBeta * cosPsi + sinPhi * cosBeta * sinPsi;
      eulerMat[0][0] = (sinBeta * sinGamma + const3 * cosGamma) * cosOmega
              - (-sinBeta * cosGamma + const3 * sinGamma) * sinOmega;
      eulerMat[0][1] = (sinBeta * sinGamma + const3 * cosGamma) * sinOmega
              + (-sinBeta * cosGamma + const3 * sinGamma) * cosOmega;
      eulerMat[0][2] = -cosPhi * cosBeta * sinPsi + sinPhi * cosBeta * cosPsi;
      eulerMat[1][0] = (-cosBeta * sinGamma + const2 * cosGamma) * cosOmega
              - (cosBeta * cosGamma + const2 * sinGamma) * sinOmega;
      eulerMat[1][1] = (-cosBeta * sinGamma + const2 * cosGamma) * sinOmega
              + (cosBeta * cosGamma + const2 * sinGamma) * cosOmega;
      eulerMat[1][2] = -cosPhi * sinBeta * sinPsi + sinPhi * sinBeta * cosPsi;
      eulerMat[2][0] = const1 * cosGamma * cosOmega - const1 * sinGamma * sinOmega;
      eulerMat[2][1] = const1 * cosGamma * sinOmega + const1 * sinGamma * cosOmega;
      eulerMat[2][2] = sinPhi * sinPsi + cosPhi * cosPsi;
    }
    return eulerMat;
  }

  public static double[][] getQEulerMatrix(double[][] eulerMat) {
    double[][] qMat = new double[6][6];
    int k = 0;
    for (int i = 0; i < 3; i++) {
      for (int j = 0; j < 3; j++) {
        qMat[i][j] = eulerMat[i][j] * eulerMat[i][j];
        if (i == j) { // Okk
          switch (i) {
            case 0:
              qMat[i + 3][j + 3] = eulerMat[1][1] * eulerMat[2][2] + eulerMat[1][2] * eulerMat[2][1];
              break;
            case 1:
              qMat[i + 3][j + 3] = eulerMat[0][0] * eulerMat[2][2] + eulerMat[0][2] * eulerMat[2][0];
              break;
            default:
              {
                qMat[i + 3][j + 3] = eulerMat[0][0] * eulerMat[1][1] + eulerMat[0][1] * eulerMat[1][0];
              }
          }
        } else { // Oij
          switch (i) {
            case 0:
              switch (j) {
                case 1:
                  k = 2;
                  break;
                default:
                  { // 2
                    k = 1;
                  }
              }
              break;
            case 1:
              switch (j) {
                case 0:
                  k = 2;
                  break;
                default:
                  { // 2
                    k = 0;
                  }
              }
              break;
            default:
              {
                switch (j) {
                  case 0:
                    k = 1;
                    break;
                  default:
                    { // 1
                      k = 0;
                    }
                }
              }
          }
          qMat[i][j] = eulerMat[k][k] * eulerMat[j][i] + eulerMat[k][i] * eulerMat[j][k];
        }
      }
    }
    // Mlk and Nkl
    for (int l = 0; l < 3; l++) {
      qMat[l][3] = 2.0 * eulerMat[l][1] * eulerMat[l][2];
      qMat[l][4] = 2.0 * eulerMat[l][0] * eulerMat[l][2];
      qMat[l][5] = 2.0 * eulerMat[l][0] * eulerMat[l][1];
      qMat[3][l] = eulerMat[1][l] * eulerMat[2][l];
      qMat[4][l] = eulerMat[0][l] * eulerMat[2][l];
      qMat[5][l] = eulerMat[0][l] * eulerMat[1][l];
    }
    return qMat;
  }

  public static double[][] getPEulerMatrix(double[][] eulerMat) {
    double[][] pMat = new double[6][6];
    int k = 0;
    for (int i = 0; i < 3; i++) {
      for (int j = 0; j < 3; j++) {
        pMat[j][i] = eulerMat[i][j] * eulerMat[i][j];
        if (i == j) { // Okk
          switch (i) {
            case 0:
              pMat[i + 3][j + 3] = eulerMat[1][1] * eulerMat[2][2] + eulerMat[1][2] * eulerMat[2][1];
              break;
            case 1:
              pMat[i + 3][j + 3] = eulerMat[0][0] * eulerMat[2][2] + eulerMat[0][2] * eulerMat[2][0];
              break;
            default:
              {
                pMat[i + 3][j + 3] = eulerMat[0][0] * eulerMat[1][1] + eulerMat[0][1] * eulerMat[1][0];
              }
          }
        } else { // Oij
          switch (i) {
            case 0:
              switch (j) {
                case 1:
                  k = 2;
                  break;
                default:
                  { // 2
                    k = 1;
                  }
              }
              break;
            case 1:
              switch (j) {
                case 0:
                  k = 2;
                  break;
                default:
                  { // 2
                    k = 0;
                  }
              }
              break;
            default:
              {
                switch (j) {
                  case 0:
                    k = 1;
                    break;
                  default:
                    { // 1
                      k = 0;
                    }
                }
              }
          }
          pMat[i][j] = eulerMat[k][k] * eulerMat[j][i] + eulerMat[k][i] * eulerMat[j][k];
        }
      }
    }
    // Mlk and Nkl
    for (int l = 0; l < 3; l++) {
      pMat[3][l] = eulerMat[l][1] * eulerMat[l][2];
      pMat[4][l] = eulerMat[l][0] * eulerMat[l][2];
      pMat[5][l] = eulerMat[l][0] * eulerMat[l][1];
      pMat[l][3] = 2.0 * eulerMat[1][l] * eulerMat[2][l];
      pMat[l][4] = 2.0 * eulerMat[0][l] * eulerMat[2][l];
      pMat[l][5] = 2.0 * eulerMat[0][l] * eulerMat[1][l];
    }
    return pMat;
  }

  public double computeStrain(double phi, double beta, double psi, double gamma) {
    // Angles must be in radiants
    // phi and beta are the polar and azimuthal angles for the crystal setting
    // psi and gamma for the sample

    return getVoigtWeight() * computeVoigtStrain(phi, beta, psi, gamma) +
            (1.0 - getVoigtWeight()) * computeReussStrain(phi, beta, psi, gamma);
  }

  public double computeVoigtStrain(double phi, double beta, double psi, double gamma) {
    // Angles must be in radiants
    // phi and beta are the polar and azimuthal angles for the crystal setting
    // psi and gamma for the sample

        double[] b = getDirectionCosine(psi, gamma);

    return b[0] * b[0] * macrostrain[0] +
            b[1] * b[1] * macrostrain[1] +
            b[2] * b[2] * macrostrain[2] +
            2.0 * b[1] * b[2] * macrostrain[3] +
            2.0 * b[0] * b[2] * macrostrain[4] +
            2.0 * b[0] * b[1] * macrostrain[5];
  }

  public static double[] getDirectionCosine(double psi, double gamma) {
    double[] b = new double[3];
    double sinpsi = Math.sin(psi);
    b[0] = Math.cos(gamma) * sinpsi;
    b[1] = Math.sin(gamma) * sinpsi;
    b[2] = Math.cos(psi);
    return b;
  }

  public double computeReussStrain(double phi, double beta, double psi, double gamma) {
    // Angles must be in radiants
    // phi and beta are the polar and azimuthal angles for the crystal setting
    // psi and gamma for the sample

    double sinphi = Math.sin(phi);
    double sinphi2 = sinphi * sinphi;
    double sin2phi = Math.sin(2.0 * phi);
    double cosphi = Math.cos(phi);
    double cosphi2 = cosphi * cosphi;
    double sinchi2 = Math.sin(beta);
    sinchi2 *= sinchi2;
    double sin2chi = Math.sin(2.0 * beta);
    double kappa = (1.0 + parameterValues[1]) / parameterValues[0];

    double strain33 = kappa * (parameterValues[2] * cosphi2 +
            parameterValues[7] * sin2phi +
            parameterValues[3] * sinphi2 -
            parameterValues[4]) * sinchi2 +
            kappa * parameterValues[4] -
            parameterValues[1] / parameterValues[0] *
            (parameterValues[2] + parameterValues[3] + parameterValues[4]) +
            kappa * (parameterValues[6] * cosphi +
            parameterValues[5] * sinphi) * sin2chi;
    return strain33;
  }

  public double computeStrain(Phase aphase, double strain_angles[],
                              int h, int k, int l) {
//    Reflection refl = aphase.getReflectionByhkl(h, k, l);
    return computeStrain(0.0, 0.0, //refl.phi[0], refl.beta[0],
            strain_angles[0] * Constants.DEGTOPI,
            strain_angles[1] * Constants.DEGTOPI);

  }

  public double[] computeStrain(Phase aphase, double alpha[], double beta[],
                                Reflection reflex) {

    int numberOfPoints = alpha.length;

    double[] strainValues = new double[numberOfPoints];

    double[] strain_angles = new double[2];
    for (int i = 0; i < numberOfPoints; i++) {
      strain_angles[0] = alpha[i];
      strain_angles[1] = beta[i];
      strainValues[i] = computeStrain(reflex.phi[0], reflex.beta[0],
              strain_angles[0] * Constants.DEGTOPI,
              strain_angles[1] * Constants.DEGTOPI);
    }

    return strainValues;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JTSStrainOptionsD(parent, this);
    return adialog;
  }

    private void checkForSymmetries(double[][] complianceMatrix) {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    private double getVoigtWeight() {
        throw new UnsupportedOperationException("Not yet implemented");
    }

  class JTSStrainOptionsD extends JOptionsDialog {

    JTextField[] pars = null;

    public JTSStrainOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      String[] labels = {"Young modulus (GPa): ",
                         "    Poisson modulus: ",
                         "Macrostress11 (MPa): ",
                         "Macrostress22 (MPa): ",
                         "Macrostress33 (MPa): ",
                         "Macrostress23 (MPa): ",
                         "Macrostress13 (MPa): ",
                         "Macrostress12 (MPa): "};
      pars = new JTextField[8];

      principalPanel.setLayout(new BorderLayout(6, 6));
      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.NORTH, jPanel8);

      for (int i = 0; i < 8; i++) {
        jPanel8.add(new JLabel(labels[i]));
        pars[i] = new JTextField(Constants.FLOAT_FIELD);
        pars[i].setText("0");
        jPanel8.add(pars[i]);
      }

      setTitle("Triaxial Stress options panel");
      initParameters();
      pack();

    }

    public void initParameters() {
      for (int i = 0; i < 8; i++) {
        pars[i].setText(parameterField[i].getValue());
        addComponenttolist(pars[i], parameterField[i]);
      }
    }

    public void retrieveParameters() {
      for (int i = 0; i < 8; i++) {
        parameterField[i].setValue(pars[i].getText());
      }
    }

  }

}

