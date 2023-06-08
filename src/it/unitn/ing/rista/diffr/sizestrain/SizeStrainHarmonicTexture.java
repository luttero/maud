/*
 * @(#)SizeStrainHarmonicTexture.java created Mar 5, 2007 Casalino
 *
 * Copyright (c) 2006 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.rta.PoleFigureOutput;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import javax.swing.border.BevelBorder;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedWriter;
import java.io.IOException;

/**
 * The SizeStrainHarmonicTexture is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Mar 5, 2007 10:40:09 AM $
 * @since JDK1.1
 */
public class SizeStrainHarmonicTexture extends SizeStrainSymModel {


  protected static String[] diclistc = {"_rita_size_sample_symmetry", "_rita_size_harmonic_expansion_degree",
                                        "_rita_microstrain_sample_symmetry", "_rita_microstrain_harmonic_expansion_degree",

                                        "_riet_par_cryst_size", "_riet_par_rs_microstrain",

                                        "_rita_size_harmonic_parameter", "_rita_microstrain_harmonic_parameter"};
  protected static String[] diclistcrm = {"sample symmetry for size", "size harmonic expansion degree (Lmax)",
                                          "sample symmetry for microstrain", "microstrain harmonic expansion degree (Lmax)",

                                          "Crystallite size (Dv, angstrom)", "r.m.s. microstrain",

                                          "size harmonic coeff ", "microstrain harmonic coeff "};

  protected static String[] classlistc = {};

  protected static String[] classlistcs = {};

  static String modelIDstring = "Disabled Size-strain harmonic ODF";

  public static String[] symmetrychoice = {"-1",
                                           "2/m",
                                           "2/mmm",
                                           "4/m",
                                           "4/mmm",
                                           "-3",
                                           "-3m",
                                           "6/m",
                                           "6/mmm",
                                           "m3",
                                           "m3m",
                                           "fiber"};

  Sample actualsample = null;
//	int actuallayer = 0;

  int expansionDegreeSize = 4;
  int sampleSymmetrySize = 0;
  int expansionDegreeMicrostrain = 4;
  int sampleSymmetryMicrostrain = 0;
  int LGIndex = 0;
//	int PGIndex = 0;

  double[] coefficientSize = null;
  double[] coefficientMicrostrain = null;

  public SizeStrainHarmonicTexture() {
    identifier = modelIDstring;
    IDlabel = modelIDstring;
    description = "select this to have an isotropic model";
  }

  public SizeStrainHarmonicTexture(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = modelIDstring;
    IDlabel = modelIDstring;
    description = "select this to have an anisotropic model for different sample direction based on harmonic expansion";
  }

  public SizeStrainHarmonicTexture(XRDcat aobj) {
    this(aobj, modelIDstring);
  }

  public void initConstant() {
    Nstring = 4;
    Nstringloop = 0;
    Nparameter = 2;
    Nparameterloop = 2;
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
    parameterField[0] = new Parameter(this, getParameterString(0), 1000,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 50),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 5000));
    parameterField[0].setPositiveOnly();
    parameterField[0].setMinimumSignificantValue(5);
    parameterField[1] = new Parameter(this, getParameterString(1), 0.0008,
            ParameterPreferences.getDouble(getParameterString(1) + ".min", 0.0),
            ParameterPreferences.getDouble(getParameterString(1) + ".max", 0.005));
    parameterField[1].setPositiveOnly();
    parameterField[1].setMinimumSignificantValue(0.0001);
    setSampleSymmetrySize(11);
    setSampleSymmetryMicrostrain(11);
    checkCrystalSampleGroups();
    setHarmonicExpansionSize(4);
    setHarmonicExpansionMicrostrain(4);

    refreshComputation = true;
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);
    parameterField[0].setPositiveOnly();
    parameterField[0].setMinimumSignificantValue(5);
    parameterField[1].setPositiveOnly();
    parameterField[1].setMinimumSignificantValue(0.0001);
  }

  public Parameter getCrystalliteSize() {
    return parameterField[0];
  }

  public Parameter getMicrostrain() {
    return parameterField[1];
  }

  double cryststrain[] = new double[2];

  public double[] getCrystalliteMicrostrain(double d_space, int h, int k, int l, double[] texture_angles) {

    cryststrain[0] = getParameterValue(0);
    cryststrain[1] = getParameterValue(1);

    return cryststrain;
  }

  public double getMeanCrystallite() {
    return getParameterValue(0);
  }

  public double getMeanMicrostrain() {
    return getParameterValue(1);
  }

  public String getSampleSymmetrySize() {
    return stringField[0];
  }

  public int getSampleSymmetrySizeValue() {

    String samplesym = getSampleSymmetrySize();

    for (int i = 0; i < symmetrychoice.length; i++) {
      if (samplesym.equals(symmetrychoice[i]))
        return i;
    }
    return 0;
  }

  public void setSampleSymmetrySize(int i) {
    stringField[0] = symmetrychoice[i];
  }

  public void setSampleSymmetrySize(String value) {
    stringField[0] = value;
  }

  public String getHarmonicExpansionSize() {
    return stringField[1];
  }

  public int getHarmonicExpansionSizeValue() {
    return Integer.valueOf(getHarmonicExpansionSize()).intValue();
  }

  public void setHarmonicExpansionSize(int i) {
    setHarmonicExpansionSize(Integer.toString(i));
  }

  public void setHarmonicExpansionSize(String value) {
    stringField[1] = value;
  }

  public String getSampleSymmetryMicrostrain() {
    return stringField[2];
  }

  public int getSampleSymmetryMicrostrainValue() {

    String samplesym = getSampleSymmetryMicrostrain();

    for (int i = 0; i < symmetrychoice.length; i++) {
      if (samplesym.equals(symmetrychoice[i]))
        return i;
    }
    return 0;
  }

  public void setSampleSymmetryMicrostrain(int i) {
    stringField[2] = symmetrychoice[i];
  }

  public void setSampleSymmetryMicrostrain(String value) {
    stringField[2] = value;
  }

  public String getHarmonicExpansionMicrostrain() {
    return stringField[3];
  }

  public int getHarmonicExpansionMicrostrainValue() {
    return Integer.valueOf(getHarmonicExpansionMicrostrain()).intValue();
  }

  public void setHarmonicExpansionMicrostrain(int i) {
    setHarmonicExpansionMicrostrain(Integer.toString(i));
  }

  public void setHarmonicExpansionMicrostrain(String value) {
    stringField[3] = value;
  }

  public ListVector getHarmonicSizeParameterList() {
    return parameterloopField[0];
  }

  public int numberHarmonicSizeParameters() {
    return getHarmonicSizeParameterList().size();
  }

  public Parameter getHarmonicSizeParameter(int index) {
    return (Parameter) getHarmonicSizeParameterList().elementAt(index);
  }

  public double getHarmonicSizeParameterValue(int index) {
    Parameter harmonic = (Parameter) getHarmonicSizeParameterList().elementAt(index);
    if (harmonic != null)
      return harmonic.getValueD();
    else
      return 0.0;
  }

  public void setExpansionDegreeSize(int value) {
    setHarmonicExpansionSize(value);
    if (expansionDegreeSize != value) {
      expansionDegreeSize = value;
      applySymmetryRulesSize();
      refreshComputation = true;
    }
  }

  public void checkHarmonicParametersSize() {
    int numberHarmonics = getNumberHarmonicsSize();
    int actualNumber = numberHarmonicSizeParameters();

    isAbilitatetoRefresh = false;
    if (actualNumber < numberHarmonics) {
      for (int i = actualNumber; i < numberHarmonics; i++) {
        addparameterloopField(0, new Parameter(this, getParameterString(0, i), 0,
            ParameterPreferences.getDouble(getParameterString(0, i) + ".min", -1),
            ParameterPreferences.getDouble(getParameterString(0, i) + ".max", 1)));
      }
      refreshComputation = true;
    }
    if (actualNumber > numberHarmonics) {
      for (int i = actualNumber - 1; i >= numberHarmonics; i--)
        getHarmonicSizeParameterList().removeItemAt(i);
      refreshComputation = true;
    }
    isAbilitatetoRefresh = true;
  }

  public String getParameterString(int index, int number) {
    String parLabel = diclist[index + totparameter];
    int k = 0;
//    System.out.println("Start " + number);
    int expansionDegree = expansionDegreeMicrostrain;
    if (index == 0)
      expansionDegree = expansionDegreeSize;
    int sampleSymmetry = sampleSymmetryMicrostrain;
    if (index == 0)
      sampleSymmetry = sampleSymmetrySize;
    for (int l = 2; l <= expansionDegree; l += 2) {
      int ml2 = SphericalHarmonics.getN(LGIndex, l);
      int nl2 = SphericalHarmonics.getN(sampleSymmetry, l);
//      System.out.println(l + " " + ml2 + " " + nl2 + " " + LGIndex + " " + sampleSymmetry);
      for (int m = 1; m <= ml2; m++) {
        for (int n = 1; n <= nl2; n++) {
          if (number == k++) {
            parLabel = parLabel.concat("_");
            parLabel = parLabel.concat(Integer.toString(l));
            parLabel = parLabel.concat("_");
            parLabel = parLabel.concat(Integer.toString(m));
            parLabel = parLabel.concat("_");
            parLabel = parLabel.concat(Integer.toString(n));
//            System.out.println(number + " " + parLabel);
            break;
          }
        }
        if (k > number)
          break;
      }
      if (k > number)
        break;
    }

    return parLabel;
  }

  public int getNumberHarmonicsSize() {

//	  LGIndex = SpaceGroups.getLGNumber(getPhase());
//	  sampleSymmetry = getSampleSymmetryValue();
    int index = 0;
    for (int l = 2; l <= expansionDegreeSize; l += 2) {
      index += SphericalHarmonics.getN(LGIndex, l) * SphericalHarmonics.getN(sampleSymmetrySize, l);
    }

    return index;
  }

  public void applySymmetryRulesSize() {
    checkCrystalSampleGroups();
    expansionDegreeSize = getHarmonicExpansionSizeValue();
    checkHarmonicParametersSize();
    refreshCoefficientsSize();
  }

  public void refreshCoefficientsSize() {
    int numberCoefficients = numberHarmonicSizeParameters();
    coefficientSize = new double[numberCoefficients];
    for (int i = 0; i < numberCoefficients; i++) {
      coefficientSize[i] = getHarmonicSizeParameterValue(i);
    }
  }



  public ListVector getHarmonicMicrostrainParameterList() {
    return parameterloopField[1];
  }

  public int numberHarmonicMicrostrainParameters() {
    return getHarmonicMicrostrainParameterList().size();
  }

  public Parameter getHarmonicMicrostrainParameter(int index) {
    return (Parameter) getHarmonicMicrostrainParameterList().elementAt(index);
  }

  public double getHarmonicMicrostrainParameterValue(int index) {
    Parameter harmonic = (Parameter) getHarmonicMicrostrainParameterList().elementAt(index);
    if (harmonic != null)
      return harmonic.getValueD();
    else
      return 0.0;
  }

  public void setExpansionDegreeMicrostrain(int value) {
    setHarmonicExpansionMicrostrain(value);
    if (expansionDegreeMicrostrain != value) {
      expansionDegreeMicrostrain = value;
      applySymmetryRulesMicrostrain();
      refreshComputation = true;
    }
  }

  public void checkHarmonicParametersMicrostrain() {
    int numberHarmonics = getNumberHarmonicsMicrostrain();
    int actualNumber = numberHarmonicMicrostrainParameters();

    isAbilitatetoRefresh = false;
    if (actualNumber < numberHarmonics) {
      for (int i = actualNumber; i < numberHarmonics; i++) {
        addparameterloopField(1, new Parameter(this, getParameterString(1, i), 0,
            ParameterPreferences.getDouble(getParameterString(1, i) + ".min", -1),
            ParameterPreferences.getDouble(getParameterString(1, i) + ".max", 1)));
      }
      refreshComputation = true;
    }
    if (actualNumber > numberHarmonics) {
      for (int i = actualNumber - 1; i >= numberHarmonics; i--)
        getHarmonicMicrostrainParameterList().removeItemAt(i);
      refreshComputation = true;
    }
    isAbilitatetoRefresh = true;
  }

  public int getNumberHarmonicsMicrostrain() {

//	  LGIndex = SpaceGroups.getLGNumber(getPhase());
//	  sampleSymmetry = getSampleSymmetryValue();
    int index = 0;
    for (int l = 2; l <= expansionDegreeSize; l += 2) {
      index += SphericalHarmonics.getN(LGIndex, l) * SphericalHarmonics.getN(sampleSymmetrySize, l);
    }

    return index;
  }

  public void applySymmetryRulesMicrostrain() {
    checkCrystalSampleGroups();
    expansionDegreeSize = getHarmonicExpansionMicrostrainValue();
    checkHarmonicParametersMicrostrain();
    refreshCoefficientsMicrostrain();
  }

  public void refreshCoefficientsMicrostrain() {
    int numberCoefficients = numberHarmonicMicrostrainParameters();
    coefficientMicrostrain = new double[numberCoefficients];
    for (int i = 0; i < numberCoefficients; i++) {
      coefficientMicrostrain[i] = getHarmonicMicrostrainParameterValue(i);
    }
  }



  public void initializeAll() {
    applySymmetryRulesSize();
    applySymmetryRulesMicrostrain();
  }

  private void checkCrystalSampleGroups() {
    LGIndex = SpaceGroups.getLGNumber(getPhase().getPointGroup());
    sampleSymmetrySize = getSampleSymmetrySizeValue();
    sampleSymmetryMicrostrain = getSampleSymmetryMicrostrainValue();
  }

  public int getLGnumber() {
    return SpaceGroups.getLGNumber(getPhase().getPointGroup());
  }

  public int getPGnumber() {
    return getPhase().getPointGroup();
  }

  public int getLaueGroupNumber() {
    return SpaceGroups.getLGNumberSiegfriedConv(getPhase().getPointGroup());
  }

  public Phase getPhase() {
    return (Phase) getParent();
  }


  public double computeMicrostrain(double phi, double beta, double psi, double gamma) {
    // Angles must be in radiants
    // phi and beta are the polar and azimuthal angles for the crystal setting
    // psi and gamma for the sample
    // see Popa, J. Appl. Cryst. 25, 611, 1992.

//    System.out.println(phi + " "+beta+" "+psi+" "+gamma);
    double poleIntensity = 1.0;
    int k = 0;

    for (int l = 2; l <= expansionDegreeMicrostrain; l += 2) {
      double tmpIntensity = 0.0;
      int ml2 = SphericalHarmonics.getN(LGIndex, l);
      for (int m = 1; m <= ml2; m++) {
        double tmpPole = 0.0;
        int nl2 = SphericalHarmonics.getN(sampleSymmetryMicrostrain, l);
        for (int n = 1; n <= nl2; n++) {
          tmpPole += coefficientMicrostrain[k] * SphericalHarmonics.getSphericalHarmonic(sampleSymmetryMicrostrain
              , l, n, gamma, psi);
          k++;
        }
        tmpIntensity += tmpPole * SphericalHarmonics.getSphericalHarmonic(LGIndex, l, m, beta, phi);
      }
      tmpIntensity *= (4.0 * Constants.PI) / (2 * l + 1);
      poleIntensity += tmpIntensity;
    }
    return poleIntensity;
  }

  public double[] computeMicrostrain(Phase aphase, double[][] alphabeta, Reflection reflex) {

    int numberOfPoints = alphabeta[0].length;

    double[] textureValues = new double[numberOfPoints];

    for (int i = 0; i < numberOfPoints; i++) {
      textureValues[i] = computeMicrostrain(reflex.phi[0], reflex.beta[0],
          alphabeta[0][i],
          alphabeta[1][i]);
    }

    return textureValues;
  }

  public double getMicrostrainODF(double alpha, double beta, double gamma) {
    double odf = 1.0;
    int k = 0;

    alpha = Constants.PI - alpha;
    gamma = Constants.PI - gamma;

    for (int l = 2; l <= expansionDegreeMicrostrain; l += 2) {
      int ml2 = SphericalHarmonics.getN(LGIndex, l);
      for (int m = 1; m <= ml2; m++) {
        int nl2 = SphericalHarmonics.getN(sampleSymmetryMicrostrain, l);
        for (int n = 1; n <= nl2; n++) {
          odf += coefficientMicrostrain[k] * SphericalHarmonics.getDSphericalHarmonic(LGIndex, sampleSymmetryMicrostrain,
              l, m, n, gamma, beta, alpha);
          k++;
        }
      }
    }
    return odf;
  }

  public double[][] getExpPoleFigureGridMicrostrain(Reflection refl, int numberofPoints, double maxAngle) {
    return getPoleFigureGridMicrostrain(refl, numberofPoints, maxAngle);
  }

  public double[][] getPoleFigureGridMicrostrain(Reflection refl, int numberofPoints, double maxAngle) {

    double[][] PFreconstructed = new double[numberofPoints][numberofPoints];

    double texture_angles[] = new double[2];

    double x, y, r;
    double dxy = 2.0 * maxAngle / numberofPoints;

//		Phase aphase = (Phase) refl.getParent();
    applySymmetryRulesMicrostrain();
//		aphase.sghklcompute(false);

    for (int i = 0; i < numberofPoints; i++)
      for (int j = 0; j < numberofPoints; j++) {
        y = (j + 0.5) * dxy - maxAngle;
        x = (i + 0.5) * dxy - maxAngle;
        r = Math.sqrt(x * x + y * y);
        if (r == 0.0) {
          texture_angles[0] = 0.0f;
          texture_angles[1] = 0.0f;
          PFreconstructed[i][j] = computeMicrostrain(refl.phi[0], refl.beta[0],
              texture_angles[0],
              texture_angles[1]);
        } else if (r < maxAngle) {
          double phaseAng = Math.atan2(y, x);
          if (phaseAng < 0.0)
            phaseAng += Constants.PI2;
          texture_angles[0] = 2.0f * (double) Math.asin(r / Constants.sqrt2);
          if (texture_angles[0] < 0.0) {
            texture_angles[0] = -texture_angles[0];
            phaseAng += Constants.PI;
            while (phaseAng >= Constants.PI2)
              phaseAng -= Constants.PI2;
          }
          texture_angles[1] = (double) phaseAng;
//					System.out.println(Double.toXRDcatString(texture_angles[0]) + " " + Double.toXRDcatString(texture_angles[1]));

          PFreconstructed[i][j] = computeMicrostrain(refl.phi[0], refl.beta[0],
              texture_angles[0],
              texture_angles[1]);
        } else
          PFreconstructed[i][j] = Double.NaN;
      }
    return PFreconstructed;
  }

  public double[][] getInversePoleFigureGridMicrostrain(double[] texture_angles,
                                             double maxPhi, int phiPointNumber,
                                             double maxBeta, int betaPointNumber) {

    double[][] PFreconstructed = new double[phiPointNumber][betaPointNumber];

    double phi, beta;
    double dphi = maxPhi / (phiPointNumber - 1);
    double dbeta = maxBeta / (betaPointNumber - 1);

//		Phase aphase = (Phase) refl.getParent();
    applySymmetryRulesMicrostrain();
//		aphase.sghklcompute(false);

    for (int i = 0; i < phiPointNumber; i++)
      for (int j = 0; j < betaPointNumber; j++) {
        beta = j * dbeta;
        phi = i * dphi;
        PFreconstructed[i][j] = computeMicrostrain(phi, beta,
		        texture_angles[0] * Constants.DEGTOPI,
		        texture_angles[1] * Constants.DEGTOPI);
      }
    return PFreconstructed;
  }

  public double[] getInversePoleFigureGridMicrostrain(double[] texture_angles,
                                           double[][] phibeta) {
    int pointNumber = phibeta[0].length;
    double[] PFreconstructed = new double[pointNumber];

//		Phase aphase = (Phase) refl.getParent();
    applySymmetryRulesMicrostrain();
//		aphase.sghklcompute(false);

    for (int i = 0; i < pointNumber; i++)
      PFreconstructed[i] = computeMicrostrain(phibeta[0][i], phibeta[1][i],
          texture_angles[0] * Constants.DEGTOPI,
          texture_angles[1] * Constants.DEGTOPI);

    return PFreconstructed;
  }


  public double computeSize(double phi, double beta, double psi, double gamma) {
    // Angles must be in radiants
    // phi and beta are the polar and azimuthal angles for the crystal setting
    // psi and gamma for the sample
    // see Popa, J. Appl. Cryst. 25, 611, 1992.

//    System.out.println(phi + " "+beta+" "+psi+" "+gamma);
    double poleIntensity = 1.0;
    int k = 0;

    for (int l = 2; l <= expansionDegreeSize; l += 2) {
      double tmpIntensity = 0.0;
      int ml2 = SphericalHarmonics.getN(LGIndex, l);
      for (int m = 1; m <= ml2; m++) {
        double tmpPole = 0.0;
        int nl2 = SphericalHarmonics.getN(sampleSymmetrySize, l);
        for (int n = 1; n <= nl2; n++) {
          tmpPole += coefficientSize[k] * SphericalHarmonics.getSphericalHarmonic(sampleSymmetrySize
              , l, n, gamma, psi);
          k++;
        }
        tmpIntensity += tmpPole * SphericalHarmonics.getSphericalHarmonic(LGIndex, l, m, beta, phi);
      }
      tmpIntensity *= (4.0 * Constants.PI) / (2 * l + 1);
      poleIntensity += tmpIntensity;
    }
    return poleIntensity;
  }

  public double[] computeSize(Phase aphase, double[][] alphabeta, Reflection reflex) {

    int numberOfPoints = alphabeta[0].length;

    double[] textureValues = new double[numberOfPoints];

    for (int i = 0; i < numberOfPoints; i++) {
      textureValues[i] = computeSize(reflex.phi[0], reflex.beta[0],
          alphabeta[0][i],
          alphabeta[1][i]);
    }

    return textureValues;
  }

  public double getSizeODF(double alpha, double beta, double gamma) {
    double odf = 1.0;
    int k = 0;

    alpha = Constants.PI - alpha;
    gamma = Constants.PI - gamma;

    for (int l = 2; l <= expansionDegreeSize; l += 2) {
      int ml2 = SphericalHarmonics.getN(LGIndex, l);
      for (int m = 1; m <= ml2; m++) {
        int nl2 = SphericalHarmonics.getN(sampleSymmetrySize, l);
        for (int n = 1; n <= nl2; n++) {
          odf += coefficientSize[k] * SphericalHarmonics.getDSphericalHarmonic(LGIndex, sampleSymmetrySize,
              l, m, n, gamma, beta, alpha);
          k++;
        }
      }
    }
    return odf;
  }

  public double[][] getExpPoleFigureGridSize(Reflection refl, int numberofPoints, double maxAngle) {
    return getPoleFigureGridSize(refl, numberofPoints, maxAngle);
  }

  public double[][] getPoleFigureGridSize(Reflection refl, int numberofPoints, double maxAngle) {

    double[][] PFreconstructed = new double[numberofPoints][numberofPoints];

    double texture_angles[] = new double[2];

    double x, y, r;
    double dxy = 2.0 * maxAngle / numberofPoints;

//		Phase aphase = (Phase) refl.getParent();
    applySymmetryRulesSize();
//		aphase.sghklcompute(false);

    for (int i = 0; i < numberofPoints; i++)
      for (int j = 0; j < numberofPoints; j++) {
        y = (j + 0.5) * dxy - maxAngle;
        x = (i + 0.5) * dxy - maxAngle;
        r = Math.sqrt(x * x + y * y);
        if (r == 0.0) {
          texture_angles[0] = 0.0f;
          texture_angles[1] = 0.0f;
          PFreconstructed[i][j] = computeSize(refl.phi[0], refl.beta[0],
              texture_angles[0],
              texture_angles[1]);
        } else if (r < maxAngle) {
          double phaseAng = Math.atan2(y, x);
          if (phaseAng < 0.0)
            phaseAng += Constants.PI2;
          texture_angles[0] = 2.0f * (double) Math.asin(r / Constants.sqrt2);
          if (texture_angles[0] < 0.0) {
            texture_angles[0] = -texture_angles[0];
            phaseAng += Constants.PI;
            while (phaseAng >= Constants.PI2)
              phaseAng -= Constants.PI2;
          }
          texture_angles[1] = (double) phaseAng;
//					System.out.println(Double.toXRDcatString(texture_angles[0]) + " " + Double.toXRDcatString(texture_angles[1]));

          PFreconstructed[i][j] = computeSize(refl.phi[0], refl.beta[0],
              texture_angles[0],
              texture_angles[1]);
        } else
          PFreconstructed[i][j] = Double.NaN;
      }
    return PFreconstructed;
  }

  public double[][] getInversePoleFigureGridSize(double[] texture_angles,
                                             double maxPhi, int phiPointNumber,
                                             double maxBeta, int betaPointNumber) {

    double[][] PFreconstructed = new double[phiPointNumber][betaPointNumber];

    double phi, beta;
    double dphi = maxPhi / (phiPointNumber - 1);
    double dbeta = maxBeta / (betaPointNumber - 1);

//		Phase aphase = (Phase) refl.getParent();
    applySymmetryRulesSize();
//		aphase.sghklcompute(false);

    for (int i = 0; i < phiPointNumber; i++)
      for (int j = 0; j < betaPointNumber; j++) {
        beta = j * dbeta;
        phi = i * dphi;
        PFreconstructed[i][j] = computeSize(phi, beta,
		        texture_angles[0] * Constants.DEGTOPI,
		        texture_angles[1] * Constants.DEGTOPI);
      }
    return PFreconstructed;
  }

  public double[] getInversePoleFigureGridSize(double[] texture_angles,
                                           double[][] phibeta) {
    int pointNumber = phibeta[0].length;
    double[] PFreconstructed = new double[pointNumber];

//		Phase aphase = (Phase) refl.getParent();
    applySymmetryRulesSize();
//		aphase.sghklcompute(false);

    for (int i = 0; i < pointNumber; i++)
      PFreconstructed[i] = computeSize(phibeta[0][i], phibeta[1][i],
          texture_angles[0] * Constants.DEGTOPI,
          texture_angles[1] * Constants.DEGTOPI);

    return PFreconstructed;
  }


  public JOptionsDialog getOptionsDialog(Frame parent) {
    return new JHSizeStrainOptionsD(parent, this);
  }

  public class JSizeStrainIsoOptionsD extends JOptionsDialog {

    JTextField microstrain;
    JTextField crystallite;

    public JSizeStrainIsoOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new GridLayout(1, 2, 1, 1));

      JPanel jp2 = new JPanel();
      jp2.setLayout(new GridLayout(2, 1, 4, 4));
      principalPanel.add(jp2);
      jp2.add(new JLabel("Crystallite size (A):"));
      crystallite = new JTextField(Constants.FLOAT_FIELD);
      crystallite.setText("0");
      jp2.add(crystallite);

      jp2 = new JPanel();
      jp2.setLayout(new GridLayout(2, 1, 4, 4));
      principalPanel.add(jp2);
      jp2.add(new JLabel("R.m.s. microstrain:"));
      microstrain = new JTextField(Constants.FLOAT_FIELD);
      microstrain.setText("0");
      jp2.add(microstrain);

      initParameters();

      setTitle("Isotropic size-strain");
      pack();
    }

    public void initParameters() {
      crystallite.setText(getCrystalliteSize().getValue());
      addComponenttolist(crystallite, getCrystalliteSize());
      microstrain.setText(getMicrostrain().getValue());
      addComponenttolist(microstrain, getMicrostrain());
    }

    public void retrieveParameters() {
      getCrystalliteSize().setValue(crystallite.getText());
      getMicrostrain().setValue(microstrain.getText());
    }

  }

  class JHSizeStrainOptionsD extends JOptionsDialog {

    JComboBox symmetryCB;
    HarmonicPane harmonicCoefficientP;
    JLabel sharpL = null;

    public JHSizeStrainOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));
      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 6, 6));
      principalPanel.add(BorderLayout.NORTH, jPanel8);
      jPanel8.add(new JLabel("Sample symmetry: "));
      symmetryCB = new JComboBox();
      for (int i = 0; i < symmetrychoice.length; i++)
        symmetryCB.addItem(symmetrychoice[i]);
      symmetryCB.setToolTipText("Set up expected sample symmetry");
      jPanel8.add(symmetryCB);

      harmonicCoefficientP = new HarmonicPane(parent, false);
      JPanel jp3 = new JPanel();
      jp3.setLayout(new BorderLayout());
      jp3.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Harmonic coefficients"));
      principalPanel.add(BorderLayout.CENTER, jp3);
      jp3.add("Center", harmonicCoefficientP);

      JPanel jp1 = new JPanel();
      jp1.setLayout(new GridLayout(0, 1));
      jp1.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Options"));
      principalPanel.add(BorderLayout.SOUTH, jp1);

      setTitle("Harmonic texture options panel");
      initParameters();
      pack();

      symmetryCB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          setSampleSymmetrySize(symmetryCB.getSelectedItem().toString());
          applySymmetryRulesSize();
        }
      });

      harmonicCoefficientP.initListener();
      harmonicCoefficientP.setSliderValue(expansionDegreeSize);
    }

    public void initParameters() {
      applySymmetryRules();
      symmetryCB.setSelectedItem(getSampleSymmetrySize());
      harmonicCoefficientP.setExpansionSlider(0, 22);
      harmonicCoefficientP.setList(XRDparent, 0);
    }

    public void retrieveParameters() {
      setSampleSymmetrySize(symmetryCB.getSelectedItem().toString());
      harmonicCoefficientP.retrieveparlist();
    }

    public void exportPFsinBEARTEXformat() {
      final String filename = Utility.browseFilenametoSave(this, "choose a file for PFs in BEARTEX format (.xpc)");
      (new PersistentThread() {
        public void executeJob() {
          PoleFigureOutput pfOutput = new PoleFigureOutput(filename, getPhase());
          pfOutput.computeAndWrite();
        }
      }).start();
    }

    public void exportCoeffinBEARTEXformat() {
      final String filename = Utility.browseFilenametoSave(this,
          "choose a file for Coefficients in BEARTEX format (.hha)");

      Phase phase = getPhase();

      refreshCoefficientsSize();

      BufferedWriter PFwriter = Misc.getWriter(filename);

      String commentLine = "Harmonic coeeficients from RiTA computing of phase: " + phase.toXRDcatString();

      try {
        PFwriter.write(commentLine);
        PFwriter.write(Constants.lineSeparator);

        PFwriter.write(filename);
        PFwriter.write(Constants.lineSeparator);

        PFwriter.write("    " + Integer.toString(SpaceGroups.getLGNumberSiegfriedConv(phase.getPointGroup())) + "    " +
            Integer.toString(getSampleSymmetrySizeValue() + 1));
        PFwriter.write(Constants.lineSeparator);

//			new String("    1.0000    1.0000    1.0000   90.0000   90.0000   90.0000");
        PFwriter.write(Misc.getFirstHHline(phase));
        PFwriter.write(Constants.lineSeparator);

        String firstline = "    10.000    40.411     1.000   " + Integer.toString(expansionDegreeSize);
        PFwriter.write(firstline);
        PFwriter.write(Constants.lineSeparator);

        int k = 0;
        for (int l = 2; l <= expansionDegreeSize; l += 2) {
          int ml2 = SphericalHarmonics.getN(LGIndex, l - 1);
          for (int m = 1; m <= ml2; m++) {
            int nl2 = SphericalHarmonics.getN(sampleSymmetrySize, l - 1);
            for (int n = 1; n <= nl2; n++) {
              PFwriter.write("  " + Integer.toString(l - 1) + "  ");
              PFwriter.write(Integer.toString(m) + "  ");
              PFwriter.write(Integer.toString(n) + "  ");
              PFwriter.write("0.0");
              PFwriter.write(Constants.lineSeparator);
            }
          }
          ml2 = SphericalHarmonics.getN(LGIndex, l);
          for (int m = 1; m <= ml2; m++) {
            int nl2 = SphericalHarmonics.getN(sampleSymmetrySize, l);
            for (int n = 1; n <= nl2; n++) {
              PFwriter.write("  " + Integer.toString(l) + "  ");
              PFwriter.write(Integer.toString(m) + "  ");
              PFwriter.write(Integer.toString(n) + "  ");
              double coeff = coefficientSize[k++]; // * (2 * l + 1) / Math.sqrt(8.0 * Constants.PI);
              PFwriter.write(Fmt.format(coeff));
              PFwriter.write(Constants.lineSeparator);
            }
          }
        }
      } catch (IOException io) {
	      io.printStackTrace();
//        LogSystem.printStackTrace(io);
      }

      try {
        PFwriter.flush();
        PFwriter.close();
      } catch (IOException io) {
	      io.printStackTrace();
       // LogSystem.printStackTrace(io);
      }
    }

  }

  public class HarmonicPane extends JPopaSSListPane {
    public HarmonicPane(Frame parent, boolean showTotal) {
      super(parent, showTotal);
    }

    public void expansionHasChanged(int value) {
      if (!MoreMath.odd(value)) {
        retrieveparlist(selected);
        selected = -1;
        setparameterlist(selected);
        setExpansionDegreeSize(value);
      }
    }

    public void setExpansionSlider(int min, int max) {
      expansionJS.setMaximum(max);
      expansionJS.setMinimum(min);
      expansionJS.setPaintTicks(true);
      expansionJS.setMajorTickSpacing(4);
      expansionJS.setMinorTickSpacing(2);

      expansionJS.setPaintLabels(true);
      expansionJS.setSnapToTicks(true);

      expansionJS.setValue(max);

      expansionJS.setLabelTable(expansionJS.createStandardLabels(4));
    }

  }

}
