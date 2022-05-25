/*
 * @(#)HarmonicTexture.java created 16/07/1998 ILL, Grenoble
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.rta;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.io.cif.CIFtoken;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.comp.*;
import it.unitn.ing.rista.interfaces.Function;

import javax.swing.*;
import javax.swing.border.BevelBorder;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.util.Vector;
import java.util.StringTokenizer;

/**
 * The HarmonicTexture is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.21 $, $Date: 2006/12/04 14:30:05 $
 * @since JDK1.1
 */


public class HarmonicTexture extends Texture implements Function {

  public static String[] diclistc = {"_rita_sample_symmetry", "_rita_harmonic_expansion_degree",
                                     "_rita_odf_sharpness",

                                     "_rita_harmonic_parameter"
  };
  public static String[] diclistcrm = {"_rita_sample_symmetry", "_rita_harmonic_expansion_degree",
                                     "_rita_odf_sharpness", "_rita_harmonic_GSAS_mode",

                                     "harmonic coeff "
  };

  public static String[] classlistcs = {};
  public static String[] classlistc = {};
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

  int expansionDegree = 4;
  int sampleSymmetry = 0;

//	double acell[];
//	double astar[];

  int LGIndex = 0;
//	int PGIndex = 0;

  double[] coefficient = null;


  // for experimental pole figures loading case
  boolean fromPF = false;
//  int[][][][] hklPF = null;
  double[][][] phicosphi = null;

  int[] izoveriPF = null;
//  int[][] allhklmult = null;
  double[][] weightSingle = null;
  double[][][] textureAngles = null;
//  double[][] experimentalPoleFigures = null;
  int[] numberOfPFPoint = null;
  int maxizoveri = 1;
  public launchBasic computation = null;
  public int[] poleindex = null;
  int[] pointindex = null;
  public double[] poleFactor = null;
  boolean refreshFit = true;
  double parameters[];
  Vector backupPar;
  boolean computingDerivate = false;
  boolean isOptimizing = false;

  public HarmonicTexture(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Harmonic";
    IDlabel = "Harmonic";
    description = "select this to apply Harmonic model";
  }

  public HarmonicTexture(XRDcat aobj) {
    this(aobj, "Harmonic method");
  }

  public HarmonicTexture() {
    identifier = "Harmonic";
    IDlabel = "Harmonic";
    description = "select this to apply Harmonic model";
  }

  public void initConstant() {
    Nstring = 3;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 1;
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

    setSampleSymmetry(11);
    checkCrystalSampleGroups();
    setHarmonicExpansion(4);

	  applySymmetryRules();
  }

	public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
          notifyParameterChanged(source, Constants.TEXTURE_CHANGED);
          return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.TEXTURE_CHANGED);
            return;
          }

      super.notifyParameterChanged(source);
    }
  }

	public void refreshForNotificationDown(XRDcat source, int reason) {
		if (!getFilePar().isComputingDerivate() || (source == this ||
				(reason == Constants.SAMPLE_ORIENTATION_CHANGED ||
						reason == Constants.CELL_CHANGED ||
						reason == Constants.TEXTURE_CHANGED)))
			refreshComputation = true;
	}

	public void initializeReflexes(Sample asample) {
  }

  public String getSampleSymmetry() {
    return stringField[0];
  }

  public int getSampleSymmetryValue() {

    String samplesym = getSampleSymmetry();

    for (int i = 0; i < symmetrychoice.length; i++) {
      if (samplesym.equals(symmetrychoice[i]))
        return i;
    }
    return 0;
  }

  public void setSampleSymmetry(int i) {
    stringField[0] = symmetrychoice[i];
  }

  public void setSampleSymmetry(String value) {
    stringField[0] = value;
  }

  public String getHarmonicExpansion() {
    return stringField[1];
  }

  public int getHarmonicExpansionValue() {
    return Integer.valueOf(getHarmonicExpansion());
  }

  public void setHarmonicExpansion(int i) {
    setHarmonicExpansion(Integer.toString(i));
  }

  public void setHarmonicExpansion(String value) {
    stringField[1] = new String(value);
  }

  public void setSharpness(String value) {
    setString(2, value);
  }

  public String getSharpness() {
    return getString(2);
  }

  public int getLaueGroupNumber() {
    return SpaceGroups.getLGNumberSiegfriedConv(getPhase().getPointGroup());
  }

  public Phase getPhase() {
    return (Phase) getParent();
  }

  public ListVector getHarmonicParameterList() {
    return parameterloopField[0];
  }

  public int numberHarmonicParameters() {
    return getHarmonicParameterList().size();
  }

  public Parameter getHarmonicParameter(int index) {
    return (Parameter) getHarmonicParameterList().elementAt(index);
  }

  public double getHarmonicParameterValue(int index) {
    Parameter harmonic = (Parameter) getHarmonicParameterList().elementAt(index);
    if (harmonic != null)
      return harmonic.getValueD();
    else
      return 0.0;
  }

  public void setExpansionDegree(int value) {
    setHarmonicExpansion(value);
    if (expansionDegree != value) {
      expansionDegree = value;
      applySymmetryRules();
      refreshComputation = true;
    }
  }

  public void checkHarmonicParameters() {
    int numberHarmonics = getNumberHarmonics();
    int actualNumber = numberHarmonicParameters();

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
        getHarmonicParameterList().removeItemAt(i);
      refreshComputation = true;
    }
    isAbilitatetoRefresh = true;
  }

  public String getParameterString(int index, int number) {
    String parLabel = new String(diclist[index + totparameter]);
    int k = 0;
//    System.out.println("Start " + number);
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

  public int getNumberHarmonics() {

//	  LGIndex = SpaceGroups.getLGNumber(getPhase());
//	  sampleSymmetry = getSampleSymmetryValue();
    int index = 0;
    for (int l = 2; l <= expansionDegree; l += 2) {
      index += SphericalHarmonics.getN(LGIndex, l) * SphericalHarmonics.getN(sampleSymmetry, l);
    }

    return index;
  }

  public int getLGnumber() {
    return SpaceGroups.getLGNumber(getPhase().getPointGroup());
  }

  public int getPGnumber() {
    return ((Phase) getPhase()).getPointGroup();
  }

  public void applySymmetryRules() {
    checkCrystalSampleGroups();
    expansionDegree = getHarmonicExpansionValue();
    checkHarmonicParameters();
    refreshCoefficients();
  }

  private void checkCrystalSampleGroups() {
    LGIndex = SpaceGroups.getLGNumber(getPhase().getPointGroup());
    sampleSymmetry = getSampleSymmetryValue();
  }

  public void initializeAll() {
    applySymmetryRules();
  }

  public void refreshCoefficients() {
    int numberCoefficients = numberHarmonicParameters();
    coefficient = new double[numberCoefficients];
    for (int i = 0; i < numberCoefficients; i++) {
      coefficient[i] = getHarmonicParameterValue(i);
    }
  }

  public void computeTextureFactor(Phase aphase, Sample asample) {

//		double[] cdsc = aphase.lattice();

    if (refreshComputation) {
      applySymmetryRules();
//  	System.out.println("computing texture");
      aphase.sghklcompute(false);
//  	System.out.println("computing texture");
//			setSharpness(computeAndGetSharpness());
      refreshComputation = false;
	    int hkln = aphase.gethklNumber();
			double[] textF = new double[hkln];
	    for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
		    DataFileSet adataset = asample.getActiveDataSet(i);
		    int datafilenumber = adataset.activedatafilesnumber();
		    for (int i1 = 0; i1 < datafilenumber; i1++) {
			    DiffrDataFile adatafile = adataset.getActiveDataFile(i1);
			    double[][][] positions = adatafile.getPositions(aphase);
//			    for (int ppp = 0; ppp < adatafile.positionsPerPattern; ppp++) {
				    for (int j = 0; j < hkln; j++) {
					    Reflection refl = aphase.getReflex(j);
					    double texture_angles[] = adatafile.getTextureAngles(positions[j][0][0]);
					    textF[j] = computeTextureFactor(refl.phi[0], refl.beta[0],
							    texture_angles[0] * Constants.DEGTOPI,
							    texture_angles[1] * Constants.DEGTOPI);
//						refl.setExpTextureFactor(adatafile.getIndex(), textF);
				    }
				    adatafile.setTextureFactors(aphase, textF);
//			    }
		    }
	    }
    }
  }

  public double computeTextureFactor(double phi, double beta, double psi, double gamma) {
    // Angles must be in radiants
    // phi and beta are the polar and azimuthal angles for the crystal setting
    // psi and gamma for the sample
    // see Popa, J. Appl. Cryst. 25, 611, 1992.

//    System.out.println(phi + " "+beta+" "+psi+" "+gamma);
    double poleIntensity = 1.0;
    int k = 0;

    for (int l = 2; l <= expansionDegree; l += 2) {
      double tmpIntensity = 0.0;
      int ml2 = SphericalHarmonics.getN(LGIndex, l);
      for (int m = 1; m <= ml2; m++) {
        double tmpPole = 0.0;
        int nl2 = SphericalHarmonics.getN(sampleSymmetry, l);
        for (int n = 1; n <= nl2; n++) {
          tmpPole += coefficient[k] * SphericalHarmonics.getSphericalHarmonic(sampleSymmetry, l, n, gamma, psi);
          k++;
        }
        tmpIntensity += tmpPole * SphericalHarmonics.getSphericalHarmonic(LGIndex, l, m, beta, phi);
      }
      tmpIntensity *= (4.0 * Constants.PI) / (2 * l + 1);
      poleIntensity += tmpIntensity;
    }
    return poleIntensity;
  }

  public double[] computeTextureFactor(Phase aphase, double[][] alphabeta, Reflection reflex) {

    int numberOfPoints = alphabeta[0].length;

    double[] textureValues = new double[numberOfPoints];

    for (int i = 0; i < numberOfPoints; i++) {
      textureValues[i] = computeTextureFactor(reflex.phi[0], reflex.beta[0],
          alphabeta[0][i],
          alphabeta[1][i]);
    }

    return textureValues;
  }

  public double getODF(double alpha, double beta, double gamma) {
    double odf = 1.0;
    int k = 0;

    alpha = Constants.PI - alpha;
    gamma = Constants.PI - gamma;

    for (int l = 2; l <= expansionDegree; l += 2) {
      int ml2 = SphericalHarmonics.getN(LGIndex, l);
      for (int m = 1; m <= ml2; m++) {
        int nl2 = SphericalHarmonics.getN(sampleSymmetry, l);
        for (int n = 1; n <= nl2; n++) {
          odf += coefficient[k] * SphericalHarmonics.getDSphericalHarmonic(LGIndex, sampleSymmetry,
              l, m, n, gamma, beta, alpha);
          k++;
        }
      }
    }
    return odf;
  }

  public double[][] getExpPoleFigureGrid(Reflection refl, int numberofPoints, double maxAngle) {
    return getPoleFigureGrid(refl, numberofPoints, maxAngle);
  }

  public double[][] getPoleFigureGrid(Reflection refl, int numberofPoints, double maxAngle) {

    double[][] PFreconstructed = new double[numberofPoints][numberofPoints];

    double texture_angles[] = new double[2];

    double x, y, r;
    double dxy = 2.0 * maxAngle / numberofPoints;

//		Phase aphase = (Phase) refl.getParent();
    applySymmetryRules();
//		aphase.sghklcompute(false);

    for (int i = 0; i < numberofPoints; i++)
      for (int j = 0; j < numberofPoints; j++) {
        x = (j + 0.5) * dxy - maxAngle;
        y = (i + 0.5) * dxy - maxAngle;
        r = Math.sqrt(x * x + y * y);
        if (r == 0.0) {
          texture_angles[0] = 0.0f;
          texture_angles[1] = 0.0f;
          PFreconstructed[i][j] = computeTextureFactor(refl.phi[0], refl.beta[0],
              texture_angles[0],
              texture_angles[1]);
        } else if (r < maxAngle) {
          double phaseAng = Math.atan2(x, y);
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

          PFreconstructed[i][j] = computeTextureFactor(refl.phi[0], refl.beta[0],
              texture_angles[0],
              texture_angles[1]);
        } else
          PFreconstructed[i][j] = Double.NaN;
      }
    return PFreconstructed;
  }

  public double[][] getInversePoleFigureGrid(double[] texture_angles,
                                             double maxPhi, int phiPointNumber,
                                             double maxBeta, int betaPointNumber) {

    double[][] PFreconstructed = new double[phiPointNumber][betaPointNumber];

    double phi, beta;
    double dphi = maxPhi / (phiPointNumber - 1);
    double dbeta = maxBeta / (betaPointNumber - 1);

//		Phase aphase = (Phase) refl.getParent();
    applySymmetryRules();
//		aphase.sghklcompute(false);

    for (int i = 0; i < phiPointNumber; i++)
      for (int j = 0; j < betaPointNumber; j++) {
        beta = j * dbeta;
        phi = i * dphi;
        PFreconstructed[i][j] = computeTextureFactor(phi, beta,
            texture_angles[0] * Constants.DEGTOPI,
            texture_angles[1] * Constants.DEGTOPI);
      }
    return PFreconstructed;
  }

  public double[] getInversePoleFigureGrid(double[] texture_angles,
                                           double[][] phibeta) {
    int pointNumber = phibeta[0].length;
    double[] PFreconstructed = new double[pointNumber];

//		Phase aphase = (Phase) refl.getParent();
    applySymmetryRules();
//		aphase.sghklcompute(false);

    for (int i = 0; i < pointNumber; i++)
      PFreconstructed[i] = computeTextureFactor(phibeta[0][i], phibeta[1][i],
          texture_angles[0] * Constants.DEGTOPI,
          texture_angles[1] * Constants.DEGTOPI);

    return PFreconstructed;
  }

  public void loadPFandComputeODF(Frame aframe) {
    try {
      String filename = Utility.openFileDialog(aframe, "Open PF file (Beartex or CIF format)", FileDialog.LOAD,
          MaudPreferences.getPref(FilePar.analysisPath, Constants.documentsDirectory),
          null, "");
      if (filename != null) {
        fromPF = true;
        textureAngles = null;
        Vector expPF = poleFigureInput(filename); //, numberOfPFPoint, textureAngles);
        numberPoleFigures = (expPF.size() - 1) / 4;
//			System.out.println("Number of PFs, init : " + numberPoleFigures);
//			System.out.println("Number of PFs, init : " + getPoleFigureNumber());

        int[] izoveriv = (int[]) expPF.elementAt(expPF.size() - 1);
        maxizoveri = izoveriv[0];
//        System.out.println("maxizoveri : " + maxizoveri);

        Phase thephase = getPhase();
        int hklnumber = thephase.gethklNumber();
        int maxEq = 1;
        for (int i = 0; i < hklnumber; i++) {
          Reflection reflex = thephase.getReflex(i);
          int mult = reflex.multiplicity / 2;
          if (mult > maxEq)
            maxEq = mult;
        }
//        System.out.println("Eq " + maxEq);
        phicosphi = new double[2][numberPoleFigures][maxizoveri];
//        allhklmult = new int[numberPoleFigures][maxizoveri];
        weightSingle = new double[numberPoleFigures][maxizoveri];
        izoveriPF = new int[numberPoleFigures];

//        double res = getResolutionD();
//        int alphamax = getAlphamax(res);
//        int betamax = (getBetamax(res) - 1) / 2 + 1;
        int totmax = 0; // (alphamax-1) * betamax;
        for (int i = 0; i < numberPoleFigures; i++) {
          int totpf = 0;
          totpf = numberOfPFPoint[i];
          if (totpf > totmax)
            totmax = totpf;
        }

        double[][] experimentalPF = new double[numberPoleFigures][totmax];

//             int numberIzoveri = 0;
        for (int i = 0; i < numberPoleFigures; i++) {
          int[][] hkl = (int[][]) expPF.elementAt(i * 4);
          double[] weight = (double[]) expPF.elementAt(i * 4 + 1);
          izoveriPF[i] = weight.length;
          for (int j = 0; j < izoveriPF[i]; j++) {
            weightSingle[i][j] = weight[j];
//            System.out.println(numberIzoveri + " " + weight[j] + " " + hkl[0][j] +" "+ hkl[1][j]+" "+ hkl[2][j]);
//            numberIzoveri++;
          }
          for (int k = 0; k < izoveriPF[i]; k++) {
            Reflection reflex = thephase.getReflectionByAnyhkl(hkl[0][k], hkl[1][k], hkl[2][k]);
            if (reflex != null) {
//              allhklmult[i][k] = reflex.multiplicity;
              double[] phicosphit = Angles.getPhicosPhi(getPhase(), hkl[0][k], hkl[1][k], hkl[2][k]);
              phicosphi[0][i][k] = phicosphit[0];
              phicosphi[1][i][k] = phicosphit[1];
            } else {
//              allhklmult[i][k] = 1;
              double[] phicosphit = Angles.getPhicosPhi(getPhase(), hkl[0][k], hkl[1][k], hkl[2][k]);
              phicosphi[0][i][k] = phicosphit[0];
              phicosphi[1][i][k] = phicosphit[1];
            }
          }
          double[] pfInt = (double[]) expPF.elementAt(i * 4 + 3);
//          int j = 0;
//          int k = 0;

          for (int k1 = 0; k1 < pfInt.length; k1++)
            experimentalPF[i][k1] = pfInt[k1];

        }
        computeTextureFromPF(experimentalPF);
        fromPF = false;
      }
    } catch (Throwable to) {
      to.printStackTrace();
      System.out.println("Error catched, check it out or send it to maud@ing.unitn.it");
      System.out.println("Your command was not completed successfully!");
    }
  }

  public Vector poleFigureInput(String filename) {

    Sample asample = getFilePar().getSample(0);
    double[] sampleAngles = asample.getSampleAngles();
//    double[] expInt = null;
    int[][] hkli = null;
    double[] weight = null;
    Vector expPF = null;
    String token = null;
    int maxizoveri_local = 1;

    if (filename.toLowerCase().endsWith(".xrdml")) {
      XRDMLPoleFigureReader xreader = new XRDMLPoleFigureReader();
      xreader.readFile(Misc.getDataBufferedInputStream(filename));
      Vector polesAndAngles = xreader.getPoleFigure();

      int numberPoleFiguresPF = polesAndAngles.size() / 2;
      numberOfPFPoint = new int[numberPoleFiguresPF];
      int totmax = 0;
      for (int i = 0; i < numberPoleFiguresPF; i++) {
        double[][] pf = (double[][]) polesAndAngles.elementAt(i * 2 + 1);
        numberOfPFPoint[i] = pf[0].length;
        if (numberOfPFPoint[i] > totmax)
          totmax = numberOfPFPoint[i];
      }
      textureAngles = new double[2][numberPoleFiguresPF][totmax];
      expPF = new Vector(0, 1);
      for (int i = 0; i < numberPoleFiguresPF; i++) {
        double[] thetaAndRes = new double[3];
        thetaAndRes[0] = 0.0;
        thetaAndRes[1] = 90.0;
        thetaAndRes[2] = 5.0;
        weight = new double[1];
        weight[0] = 1.0;
        hkli = (int[][]) polesAndAngles.elementAt(i * 2);
        double[] pfInt = new double[numberOfPFPoint[i]];
        double[][] pf = (double[][]) polesAndAngles.elementAt(i * 2 + 1);
        for (int j = 0; j < numberOfPFPoint[i]; j++) {
          textureAngles[0][i][j] = pf[0][j] + sampleAngles[1];
          textureAngles[1][i][j] = pf[1][j] + sampleAngles[2];
          pfInt[j] = pf[2][j];
        }
        expPF.addElement(hkli);
        expPF.addElement(weight);
        expPF.addElement(thetaAndRes);
        expPF.addElement(pfInt);
      }
      int[] izoveriv = new int[1];
      izoveriv[0] = maxizoveri_local;
      expPF.addElement(izoveriv);
    } else {
      BufferedReader PFreader = Misc.getReader(filename);
      if (PFreader != null) {
        try {

          int izoveriCheck = 0;
          int izoveri = 1;
          hkli = new int[3][1];
          weight = new double[1];
          weight[0] = 1.0;

          String line = PFreader.readLine();
          StringTokenizer st = null;
          if (filename.toLowerCase().endsWith(".apf")) {
            // texture weights Maud export format

            line = PFreader.readLine();
            st = new StringTokenizer(line, "' ,\t\r\n");
            int totmax = 0;
            int numberPoleFiguresPF = Integer.valueOf(st.nextToken()).intValue();
            numberOfPFPoint = new int[numberPoleFiguresPF];
            Vector allPFs = new Vector(numberPoleFiguresPF * 2, 1);
            for (int i = 0; i < numberPoleFiguresPF; i++) {
              int[] hkl = new int[3];
              line = PFreader.readLine();
              st = new StringTokenizer(line, "' ,\t\r\n");
              hkl[0] = Integer.valueOf(st.nextToken()).intValue();
              hkl[1] = Integer.valueOf(st.nextToken()).intValue();
              hkl[2] = Integer.valueOf(st.nextToken()).intValue();
              line = PFreader.readLine();
              st = new StringTokenizer(line, "' ,\t\r\n");
              numberOfPFPoint[i] = Integer.valueOf(st.nextToken()).intValue();
              if (numberOfPFPoint[i] > totmax)
                totmax = numberOfPFPoint[i];
              Vector poleFig = new Vector(numberOfPFPoint[i], 1);
              for (int j = 0; j < numberOfPFPoint[i]; j++) {
                line = PFreader.readLine();
                st = new StringTokenizer(line, "' ,\t\r\n");
                double[] pf = new double[4];
                pf[0] = Double.valueOf(st.nextToken()).doubleValue();
                pf[1] = Double.valueOf(st.nextToken()).doubleValue();
                pf[2] = Double.valueOf(st.nextToken()).doubleValue();
                if (st.hasMoreTokens()) st.nextToken();
                if (st.hasMoreTokens()) st.nextToken();
                if (st.hasMoreTokens())
                  pf[3] = Double.valueOf(st.nextToken()).doubleValue();
                poleFig.add(pf);
              }
              allPFs.add(hkl);
              allPFs.add(poleFig);
            }
            textureAngles = new double[2][numberPoleFiguresPF][totmax];
            expPF = new Vector(0, 1);
            for (int i = 0; i < numberPoleFiguresPF; i++) {
              double[] thetaAndRes = new double[3];
              thetaAndRes[0] = 0.0;
              thetaAndRes[1] = 90.0;
              thetaAndRes[2] = 0.0;
              hkli = new int[3][1];
              weight = new double[1];
              weight[0] = 1.0;
              int[] hkl = (int[]) allPFs.elementAt(i * 2);
              for (int i1 = 0; i1 < 3; i1++)
                hkli[i1][0] = hkl[i1];
              double[] pfInt = new double[numberOfPFPoint[i]];
              Vector poleFig = (Vector) allPFs.elementAt(i * 2 + 1);
              for (int j = 0; j < numberOfPFPoint[i]; j++) {
                double[] pf = (double[]) poleFig.elementAt(j);
                textureAngles[0][i][j] = (pf[0] + sampleAngles[1]) * Constants.DEGTOPI;
                textureAngles[1][i][j] = (pf[1] + sampleAngles[2]) * Constants.DEGTOPI;
                pfInt[j] = pf[2];
              }
              expPF.addElement(hkli);
              expPF.addElement(weight);
              expPF.addElement(thetaAndRes);
              expPF.addElement(pfInt);
            }
            maxizoveri_local = 1;
          } else if (line.toLowerCase().startsWith("data_") || line.startsWith("_")) {
            // cif format
            Vector allPFs = new Vector(0, 1);
            while (line != null) {
              int[] hkl = new int[3];
              while (line != null && !line.toLowerCase().startsWith("loop_")) {
//          	System.out.println(linedata);
                st = new StringTokenizer(line, "' ,\t\r\n");

                while (st.hasMoreTokens()) {
                  token = st.nextToken();
                  if (token.equalsIgnoreCase("_diffrn_refln_index_h")) {
                    token = st.nextToken();
                    hkl[0] = Integer.valueOf(token).intValue();
                  } else if (token.equalsIgnoreCase("_diffrn_refln_index_k")) {
                    token = st.nextToken();
                    hkl[1] = Integer.valueOf(token).intValue();
                  } else if (token.equalsIgnoreCase("_diffrn_refln_index_l")) {
                    token = st.nextToken();
                    hkl[2] = Integer.valueOf(token).intValue();
                  }
                }
                line = PFreader.readLine();
              }

//						System.out.println("Reading pole figure: " + hkl[0] + " " + hkl[1] + " " + hkl[2]);

              int index = -1;
              int chiIndex = -1, phiIndex = -1, intensityIndex = -1, sigmaIndex = -1;

              line = PFreader.readLine();
              while (line != null && line.toLowerCase().startsWith("_")) {
//          	System.out.println(line);
                st = new StringTokenizer(line, "' ,\t\r\n");
                while (st.hasMoreTokens()) {
                  index++;
                  token = st.nextToken();
                  if (token.equalsIgnoreCase("_diffrn_refln_angle_chi") ||
                      token.equalsIgnoreCase("_pd_meas_angle_chi")) {
                    chiIndex = index;
                  } else if (token.equalsIgnoreCase("_diffrn_refln_angle_phi") ||
                      token.equalsIgnoreCase("_pd_meas_angle_phi")) {
                    phiIndex = index;
                  } else if (token.equalsIgnoreCase("_diffrn_refln_intensity_net") ||
                      token.equalsIgnoreCase("_pd_meas_intensity_total")) {
                    intensityIndex = index;
                  } else if (token.equalsIgnoreCase("_diffrn_refln_intensity_sigma")) {
                    sigmaIndex = index;
                  }
                }
                line = PFreader.readLine();
              }

//						System.out.println("Reading data for pole figure: " + chiIndex + " " + phiIndex + " " + intensityIndex);

              Vector poleFig = new Vector(10, 10);
              while (line != null && !line.toLowerCase().startsWith("_")
                  && !line.toLowerCase().startsWith("data_")) {
//          	System.out.println(line);
                st = new StringTokenizer(line, "' ,\t\r\n");

                index = 0;
                boolean found = false;
                double[] pf = new double[4];
                pf[3] = 1.0;
                while (st.hasMoreTokens()) {
                  token = st.nextToken();
                  if (index == chiIndex) {
                    pf[0] = Double.valueOf(token).doubleValue();
                  } else if (index == phiIndex) {
                    pf[1] = Double.valueOf(token).doubleValue();
                  } else if (index == intensityIndex) {
                    pf[2] = Double.valueOf(token).doubleValue();
                    found = true;
                    if (sigmaIndex == -1) {
                      if (pf[3] > 0.0)
                        pf[3] = 1.0 / pf[2];
                      else
                        pf[3] = 1.0;
                    }
                  } else if (index == sigmaIndex) {
                    pf[3] = Double.valueOf(token).doubleValue();
/*									if (pf[3] > 0.0)
										pf[3] = 1.0 / pf[3];
									else
										pf[3] = 0.0;*/
                  }
                  index++;
                }
                if (found) {
                  poleFig.add(pf);
//						System.out.println("Adding to pole figure: " + pf[0] + " " + pf[1] + " " + pf[2]);
                }
                line = PFreader.readLine();
              }
              allPFs.add(hkl);
              allPFs.add(poleFig);
            }

            int totmax = 0;
            int numberPoleFiguresPF = allPFs.size() / 2;
            numberOfPFPoint = new int[numberPoleFiguresPF];
            for (int i = 0; i < numberPoleFiguresPF; i++) {
//						int[] hkl = (int[]) allPFs.elementAt(i * 2);
              Vector poleFig = (Vector) allPFs.elementAt(i * 2 + 1);
              numberOfPFPoint[i] = poleFig.size();
              if (numberOfPFPoint[i] > totmax)
                totmax = numberOfPFPoint[i];
            }
            textureAngles = new double[2][numberPoleFiguresPF][totmax];
            expPF = new Vector(0, 1);
            for (int i = 0; i < numberPoleFiguresPF; i++) {
              double[] thetaAndRes = new double[3];
              thetaAndRes[0] = 0.0;
              thetaAndRes[1] = 90.0;
              thetaAndRes[2] = 0.0;
              hkli = new int[3][1];
              weight = new double[1];
              weight[0] = 1.0;
              int[] hkl = (int[]) allPFs.elementAt(i * 2);
              for (int i1 = 0; i1 < 3; i1++)
                hkli[i1][0] = hkl[i1];
              double[] pfInt = new double[numberOfPFPoint[i]];
              Vector poleFig = (Vector) allPFs.elementAt(i * 2 + 1);
              for (int j = 0; j < numberOfPFPoint[i]; j++) {
                double[] pf = (double[]) poleFig.elementAt(j);
                textureAngles[0][i][j] = (pf[0] + sampleAngles[1]) * Constants.DEGTOPI;
                textureAngles[1][i][j] = (pf[1] + sampleAngles[2]) * Constants.DEGTOPI;
                pfInt[j] = pf[2];
                weight[0] = pf[3];
              }
              expPF.addElement(hkli);
              expPF.addElement(weight);
              expPF.addElement(thetaAndRes);
              expPF.addElement(pfInt);
            }
            maxizoveri_local = 1;
          } else if (filename.toLowerCase().endsWith(".gpf")) {
            Vector allPFs = new Vector(3, 3);
            line = PFreader.readLine();
            while (line != null) {

              Vector poleFig = new Vector(10, 10);

              System.out.println("Reading: " + line);
//            st = new StringTokenizer(line, " ,\t\r\n");
              hkli = new int[3][1];
              int index = 1;
              for (int i = 0; i < 3; i++) {
//              token = st.nextToken();
//              hkli[i][0] = Integer.valueOf(token).intValue();
                hkli[i][0] = Integer.valueOf(line.substring(index, index + 1)).intValue();
                index++;
//              System.out.println(hkli[i][0]);
              }
              double[] thetaAndRes = new double[3];
/*            for (int i = 0; i < 3; i++) {
              token = st.nextToken();
              thetaAndRes[i] = Double.valueOf(token).doubleValue();
            }*/
              thetaAndRes[0] = 0.0;
              thetaAndRes[1] = 90.0;
              thetaAndRes[2] = 5.0;

              allPFs.addElement(hkli);
//            double weightTot = 0.0;
              izoveri = 1;
              weight = new double[izoveri];
              for (int i = 0; i < izoveri; i++)
                weight[i] = 1.0;
              allPFs.addElement(weight);
//            expPF.addElement(thetaAndRes);

              double res = thetaAndRes[2];

              int alphamax = Uwimvuo.getAlphamax(res);
              int betamax = (Uwimvuo.getBetamax(res) - 1) / 2 + 1;
              int totData = 0;
              int a = 0;
              int b = 0;
              do {
                line = PFreader.readLine();
                for (int i = 1; i < 72; i += 4) {
                  if (totData < (alphamax - 1) * betamax) {
                    String subData = line.substring(i, i + 4);
                    totData++;
//                System.out.println(i + " " + subData);
                    double pfvalue = Double.valueOf(Misc.toStringDeleteBlank(subData)).doubleValue() / 100.0;
                    double theta = b * res; //(totData / (alphamax - 1)) * res;
                    double phi = a * res;
                    if (theta >= thetaAndRes[0] && theta <= thetaAndRes[1]) {
                      double[] pf = new double[3];
                      pf[0] = theta;
                      pf[1] = phi;
                      pf[2] = pfvalue;
//                    System.out.println(theta + " " + phi + " " + pfvalue);
                      poleFig.add(pf);
                    }
                    a++;
                    if (a >= alphamax - 1) {
                      a = 0;
                      b++;
                    }
                  }
                }
              } while (totData < (alphamax - 1) * betamax);
              allPFs.add(poleFig);
              line = PFreader.readLine();
              line = PFreader.readLine();

// next pole figure
              line = PFreader.readLine();
            }

            int totmax = 0;
            int numberPoleFiguresPF = allPFs.size() / 3;
            numberOfPFPoint = new int[numberPoleFiguresPF];
            for (int i = 0; i < numberPoleFiguresPF; i++) {
//						int[] hkl = (int[]) allPFs.elementAt(i * 2);
              Vector poleFig = (Vector) allPFs.elementAt(i * 3 + 2);
              numberOfPFPoint[i] = poleFig.size();
              if (numberOfPFPoint[i] > totmax)
                totmax = numberOfPFPoint[i];
            }
            textureAngles = new double[2][numberPoleFiguresPF][totmax];
            expPF = new Vector(0, 1);
            for (int i = 0; i < numberPoleFiguresPF; i++) {
              double[] thetaAndRes = new double[3];
              thetaAndRes[0] = 0.0;
              thetaAndRes[1] = 90.0;
              thetaAndRes[2] = 0.0;
              int[][] hkl = (int[][]) allPFs.elementAt(i * 3);
              double[] weig = (double[]) allPFs.elementAt(i * 3 + 1);
              double[] pfInt = new double[numberOfPFPoint[i]];
              Vector poleFig = (Vector) allPFs.elementAt(i * 3 + 2);
              for (int j = 0; j < numberOfPFPoint[i]; j++) {
                double[] pf = (double[]) poleFig.elementAt(j);
                textureAngles[0][i][j] = (pf[0] + sampleAngles[1]) * Constants.DEGTOPI;
                textureAngles[1][i][j] = (pf[1] + sampleAngles[2]) * Constants.DEGTOPI;
                pfInt[j] = pf[2];
              }
              expPF.addElement(hkl);
              expPF.addElement(weig);
              expPF.addElement(thetaAndRes);
              expPF.addElement(pfInt);
            }
          } else if (filename.toLowerCase().endsWith(".ppf")) {
 //           Vector allPFs = new Vector(3, 3);

            for (int i = 0; i < 4; i++)
              line = PFreader.readLine();
            st = new StringTokenizer(line, " ,\t\r\n");
            int poleFiguresNumber = Integer.valueOf(st.nextToken());
            line = PFreader.readLine();

            int[][] indices = new int[3][poleFiguresNumber];
            double[][] grid = new double[6][poleFiguresNumber];
            double[] cutoff = new double[poleFiguresNumber];
            for (int i = 0; i < poleFiguresNumber; i++) {
              line = PFreader.readLine();
              st = new StringTokenizer(line, " ,\t\r\n");
              st.nextToken();
              grid[0][i] = Double.valueOf(st.nextToken());
              grid[1][i] = Double.valueOf(st.nextToken());
              grid[2][i] = Double.valueOf(st.nextToken());
              grid[3][i] = Double.valueOf(st.nextToken());
              grid[4][i] = Double.valueOf(st.nextToken());
              grid[5][i] = Double.valueOf(st.nextToken());
              st.nextToken();
              indices[0][i] = Integer.valueOf(st.nextToken());
              indices[1][i] = Integer.valueOf(st.nextToken());
              indices[2][i] = Integer.valueOf(st.nextToken());
              st.nextToken();
	            if (st.hasMoreTokens())
              st.nextToken();
              cutoff[i] = grid[1][i];
              if (st.hasMoreTokens())
                cutoff[i] = Double.valueOf(st.nextToken());
            }

            expPF = new Vector(0, 1);

            int totmax = 0;
            numberOfPFPoint = new int[poleFiguresNumber];
            int[] numberOfPFPointReal = new int[poleFiguresNumber];
            for (int i = 0; i < poleFiguresNumber; i++) {
              numberOfPFPoint[i] = (int) ((cutoff[i] - grid[0][i]) / grid[2][i] + 1.01);
              numberOfPFPointReal[i] = (int) ((grid[1][i] - grid[0][i]) / grid[2][i] + 1.01);
              numberOfPFPoint[i] *= (int) (360 / grid[2][i] + 0.00001);
              numberOfPFPointReal[i] *= (int) (360 / grid[2][i] + 0.00001);
              if (numberOfPFPoint[i] > totmax)
                totmax = numberOfPFPoint[i];
            }
            textureAngles = new double[2][poleFiguresNumber][totmax];

            for (int i = 0; i < poleFiguresNumber; i++) {
              double[] thetaAndRes = new double[3];
              thetaAndRes[0] = grid[0][i];
              thetaAndRes[1] = grid[1][i];
              thetaAndRes[2] = cutoff[i];
              int[][] hkl = new int[3][1];
              for (int j = 0; j < 3; j++)
                hkl[j][0] = indices[j][i];

              double[] weig = new double[1];
              weig[0] = 1.0;

              double[] pfInt = new double[totmax];

              System.out.println("Pole figure: " + indices[0][i] + " " + indices[1][i] + " " + indices[2][i]
                                  + ", number of points to read: " + numberOfPFPointReal[i]);

              int j = 0;
              line = PFreader.readLine();
              double alpha = grid[3][i];
              double beta = grid[0][i];
              while (j < numberOfPFPointReal[i] && line != null) {
                st = new StringTokenizer(line, " ,\t\r\n");
                while (st.hasMoreTokens()) {
                  double pfvalue = Double.valueOf(st.nextToken());
//                    System.out.println(beta + " " + alpha + " " + pf[2]);
                  if (j < numberOfPFPoint[i]) {
                    pfInt[j] = pfvalue;
                    textureAngles[0][i][j] = beta;
                    textureAngles[1][i][j] = alpha;
                  }
                  alpha += grid[5][i];
                  if (alpha > grid[4][i]) {
 //                   System.out.println(beta + " " + alpha + " " + pfInt[2][j]);
                    alpha = grid[3][i];
                    beta += grid[2][i];
                  }
                  j++;
                }
                line = PFreader.readLine();
              }

              if (cutoff[i] > 0) {
              expPF.addElement(hkl);
              expPF.addElement(weig);
              expPF.addElement(thetaAndRes);
              expPF.addElement(pfInt);
              }
            }

          } else {
            Vector allPFs = new Vector(3, 3);
            for (int i = 0; i < 4; i++) {
              if (line != null) {
                line = PFreader.readLine();
                if (line != null && !line.equals("")) {
                  st = new StringTokenizer(line, " ,\t\r\n");
                  while (st.hasMoreTokens()) {
                    switch (izoveriCheck) {
                      case 0:
                        izoveri = Integer.valueOf(token = st.nextToken()).intValue();
                        if (maxizoveri_local < izoveri)
                          maxizoveri_local = izoveri;
                        hkli = new int[3][izoveri];
                        weight = new double[izoveri];
                        izoveriCheck++;
                        break;
                      default:
                        {
                          hkli[0][izoveriCheck - 1] = Integer.valueOf(token = st.nextToken()).intValue();
                          hkli[1][izoveriCheck - 1] = Integer.valueOf(token = st.nextToken()).intValue();
                          hkli[2][izoveriCheck - 1] = Integer.valueOf(token = st.nextToken()).intValue();
                          weight[izoveriCheck - 1] = Double.valueOf(token = st.nextToken()).doubleValue();
                          izoveriCheck++;
                        }
                    }
                  }
                }
              }
            }

            while (line != null) {

              Vector poleFig = new Vector(10, 10);

              PFreader.readLine();
              line = PFreader.readLine();
              System.out.println("Reading: " + line);
              st = new StringTokenizer(line, " ,\t\r\n");
              for (int i = 0; i < 3; i++) {
                token = st.nextToken();
                hkli[i][0] = Integer.valueOf(token).intValue();
//              System.out.println(hkli[i][0]);
              }
              double[] thetaAndRes = new double[3];
              for (int i = 0; i < 3; i++) {
                token = st.nextToken();
                thetaAndRes[i] = Double.valueOf(token).doubleValue();
              }

              allPFs.addElement(hkli);
              double weightTot = 0.0;
              for (int i = 0; i < izoveri; i++)
                weightTot += weight[i];
              if (weightTot == 0) {
                for (int i = 0; i < izoveri; i++)
                  weight[i] = 1.0 / izoveri;
              } else
                for (int i = 0; i < izoveri; i++)
                  weight[i] /= weightTot;
              allPFs.addElement(weight);
//            expPF.addElement(thetaAndRes);

              double res = thetaAndRes[2];

              int alphamax = Uwimvuo.getAlphamax(res);
              int betamax = (Uwimvuo.getBetamax(res) - 1) / 2 + 1;
              int totData = 0;
              int a = 0;
              int b = 0;
              do {
                line = PFreader.readLine();
                for (int i = 1; i < 72; i += 4) {
                  if (totData < (alphamax - 1) * betamax) {
                    String subData = line.substring(i, i + 4);
                    totData++;
//                System.out.println(i + " " + subData);
                    double pfvalue = Double.valueOf(Misc.toStringDeleteBlank(subData)).doubleValue() / 100.0;
                    double theta = b * res; //(totData / (alphamax - 1)) * res;
                    double phi = a * res;
                    if (theta >= thetaAndRes[0] && theta <= thetaAndRes[1]) {
                      double[] pf = new double[3];
                      pf[0] = theta;
                      pf[1] = phi;
                      pf[2] = pfvalue;
//                    System.out.println(theta + " " + phi + " " + pfvalue);
                      poleFig.add(pf);
                    }
                    a++;
                    if (a >= alphamax - 1) {
                      a = 0;
                      b++;
                    }
                  }
                }
              } while (totData < (alphamax - 1) * betamax);
              allPFs.add(poleFig);
              line = PFreader.readLine();

// next pole figure
              line = PFreader.readLine();

              izoveriCheck = 0;
              izoveri = 1;
              hkli = new int[3][1];
              weight = new double[1];
              weight[0] = 1.0;
              for (int i = 0; i < 4; i++) {
                if (line != null) {
                  line = PFreader.readLine();
                  if (line != null && !line.equals("")) {
                    st = new StringTokenizer(line, " ,\t\r\n");
                    while (st.hasMoreTokens()) {
                      switch (izoveriCheck) {
                        case 0:
                          izoveri = Integer.valueOf(token = st.nextToken()).intValue();
                          if (maxizoveri_local < izoveri)
                            maxizoveri_local = izoveri;
                          hkli = new int[3][izoveri];
                          weight = new double[izoveri];
                          izoveriCheck++;
                          break;
                        default:
                          {
                            hkli[0][izoveriCheck - 1] = Integer.valueOf(token = st.nextToken()).intValue();
                            hkli[1][izoveriCheck - 1] = Integer.valueOf(token = st.nextToken()).intValue();
                            hkli[2][izoveriCheck - 1] = Integer.valueOf(token = st.nextToken()).intValue();
                            weight[izoveriCheck - 1] = Double.valueOf(token = st.nextToken()).doubleValue();
                            izoveriCheck++;
                          }
                      }
                    }
                  }
                }
              }

            }

            int totmax = 0;
            int numberPoleFiguresPF = allPFs.size() / 3;
            numberOfPFPoint = new int[numberPoleFiguresPF];
            for (int i = 0; i < numberPoleFiguresPF; i++) {
//						int[] hkl = (int[]) allPFs.elementAt(i * 2);
              Vector poleFig = (Vector) allPFs.elementAt(i * 3 + 2);
              numberOfPFPoint[i] = poleFig.size();
              if (numberOfPFPoint[i] > totmax)
                totmax = numberOfPFPoint[i];
            }
            textureAngles = new double[2][numberPoleFiguresPF][totmax];
            expPF = new Vector(0, 1);
            for (int i = 0; i < numberPoleFiguresPF; i++) {
              double[] thetaAndRes = new double[3];
              thetaAndRes[0] = 0.0;
              thetaAndRes[1] = 90.0;
              thetaAndRes[2] = 0.0;
              int[][] hkl = (int[][]) allPFs.elementAt(i * 3);
              double[] weig = (double[]) allPFs.elementAt(i * 3 + 1);
              double[] pfInt = new double[numberOfPFPoint[i]];
              Vector poleFig = (Vector) allPFs.elementAt(i * 3 + 2);
              for (int j = 0; j < numberOfPFPoint[i]; j++) {
                double[] pf = (double[]) poleFig.elementAt(j);
                textureAngles[0][i][j] = (pf[0] + sampleAngles[1]) * Constants.DEGTOPI;
                textureAngles[1][i][j] = (pf[1] + sampleAngles[2]) * Constants.DEGTOPI;
                pfInt[j] = pf[2];
              }
              expPF.addElement(hkl);
              expPF.addElement(weig);
              expPF.addElement(thetaAndRes);
              expPF.addElement(pfInt);
            }
          }


          int[] izoveriv = new int[1];
          izoveriv[0] = maxizoveri_local;
          expPF.addElement(izoveriv);

          PFreader.close();
        } catch (Throwable io) {
          io.printStackTrace();
          try {
            PFreader.close();
          } catch (Throwable to) {
          }
          System.out.println("Error catched, check it out or send it to maud@ing.unitn.it");
          System.out.println("Your command was not completed successfully!");
        }
      }
    }

    return expPF;
  }

//  OutputFrame outputframe = null;

  public void computeTextureFromPF(double[][] experimentalPF) {

//    FilePar aparFile = getFilePar();

    prepareiteration(experimentalPF);

    startingRefine();

    if (!Constants.textonly) {
/*      if (outputframe != null) {
        outputframe.setVisible(false);
        outputframe.dispose();
        outputframe = null;
      }
      outputframe = new OutputFrame(null, getNumberofIterations());
      outputframe.initializeSizeAndPosition(true, "refineOutFrame.frameWidth", "refineOutFrame.frameHeight", 400, 500,
          true, "refineOutFrame.framePositionX", "refineOutFrame.framePositionY", 90, 5);
      outputframe.appendnewline("Computing ODF for phase: " + getPhase().toXRDcatString());*/
    } else {
      System.out.println("Computing ODF for phase: " + getPhase().toXRDcatString());
    }

    launchrefine(null);

    refreshComputation = false;

  }

  private void startingRefine() {
    if (computation != null)
      stoprefinement();
  }

  public void launchrefine(OutputPanel aPanel) {
    boolean canGo = validate();
    if (!canGo) {
      System.out.println("Validation not passed");
      return;
    }
    computation = new launchRefine(this, aPanel);
    computation.prepare();
    computation.launch();
  }

  public void stoprefinement() {
    stopcomputation();
  }

  public void endOfComputation() {
    computation = null;
  }

  public void stopcomputation() {
    JButton okButton = new JButton("Stop old computation");
    final AttentionD attdlg = new AttentionD(getFilePar().getMainFrame(),
        "Are you sure to stop the running computation?", okButton);
    okButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        attdlg.setVisible(false);
        attdlg.dispose();
        computation.interruptComputation();
        computation = null;
        setDerivate(false);
      }
    });
    attdlg.setVisible(true);
  }

  double[] dta, wgt, fit;
  int numberOfData = 0, numberOfParameters = 0;
  double Rw = 0.0, R = 0.0, Rexp = 0.0;

  public int prepareiteration(double[][] experimentalPF) {

    refreshFit = true;
    indexesComputed = false;
    numberOfParameters = computeParameterNumber();

    Rw = 0.0;
    R = 0.0;

    // textureInitialization();  // todo: uniform all initialize methods for texture
    initializeAll();

    numberPoleFigures = experimentalPF.length;

    numberOfData = 0;
    for (int i = 0; i < getPoleFigureNumber(); i++) {
      numberOfData += getPointNumber(i);
    }

//		System.out.println("Number of PFs : " + getPoleFigureNumber());

    dta = new double[numberOfData];
    wgt = new double[numberOfData];
    fit = new double[numberOfData];
    poleindex = new int[numberOfData];
    pointindex = new int[numberOfData];
    poleFactor = new double[getPoleFigureNumber()];

    int index = 0;
    for (int i = 0; i < getPoleFigureNumber(); i++) {
      poleFactor[i] = 1.0f;
      int numberDta = getPointNumber(i);
      for (int j = 0; j < numberDta; j++) {
        dta[index] = (double) experimentalPF[i][j];
        wgt[index] = 1.0f;
        poleindex[index] = i;
        pointindex[index++] = j;
      }
    }

    return numberOfParameters;
  }

  public int getPointNumber(int pole) {
    if (fromPF)
      return numberOfPFPoint[pole];
    else
      return 0;
  }

  public int getNumberOfData() {
    return numberOfData;
  }

  public double getData(int index) {
    return dta[index];
  }

  public double getWeight(int index) {
    return wgt[index];
  }

  public double getFit(int index) {
    if (refreshFit) {
      computeFit();
      getFit();
    }
    return fit[index];
  }

  public double[] getFit() {
    return fit;
  }

  public double getWSS() {
    double WSS = 0.0;
    double diff;
//System.out.println("data number " + numberOfData);
    for (int i = 0; i < numberOfData; i++) {
//      System.out.println(i+" " + getFit(i) + " " + getData(i) + " " + getWeight(i));
      diff = (getFit(i) - getData(i)) * getWeight(i);
      WSS += diff * diff;
    }
//    WSS = Math.sqrt(WSS); // so we get the Rwp
    return WSS;
  }

  boolean indexesComputed = false;
  double[] refinementIndexes = new double[19];

  public double[] getRefinementIndexes() {

    if (!indexesComputed) {
      double diff, wgt, dta, diff2, wgt2, dta2, dtanb, dtanb2, diffb, diffb2;
      double rw = 0.0, r = 0.0, rwb = 0.0, rb = 0.0, den = 0.0, rden = 0.0, denb = 0.0, rdenb = 0.0;

      for (int i = 0; i < getNumberOfData(); i++) {
        wgt = getWeight(i);
        wgt2 = wgt * wgt;
        dta = this.dta[i];
        dta2 = dta * dta;
        dtanb = Math.abs(dta - fit[i]);
        dtanb2 = dtanb * dtanb;
        diff = Math.abs(dta - fit[i]);
        if (dta != 0)
          diffb = diff * dtanb / dta;
        else
          diffb = diff * dtanb;
        diff2 = diff * diff;
        diffb2 = diffb * diffb;

        rw += diff2 * wgt2;
        r += diff;
        rwb += diffb2 * wgt2;
        rb += diffb;
        den += dta2 * wgt2;
        rden += dta;
        denb += dtanb2 * wgt2;
        rdenb += dtanb;
      }

      refinementIndexes[0] = MoreMath.sqrt(rw / den);
      refinementIndexes[1] = MoreMath.sqrt(rwb / denb);
      refinementIndexes[2] = r / rden;
      refinementIndexes[3] = rb / rdenb;
      refinementIndexes[4] = rw;
      refinementIndexes[5] = rwb;
      refinementIndexes[6] = r;
      refinementIndexes[7] = rb;
      refinementIndexes[8] = den;
      refinementIndexes[9] = denb;
      refinementIndexes[10] = rden;
      refinementIndexes[11] = rdenb;

      indexesComputed = true;
    }

    return refinementIndexes;

  }

  public void setRw(double Rw) {
    this.Rw = Rw;
  }

  public void setR(double R) {
    this.R = R;
  }

  public void setRexp(double R) {
    this.Rexp = R;
  }

  public int computeParameterNumber() {
    int pnumber = 0;

    refreshparametersV();
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines())
        ++pnumber;
    }

    return pnumber;
  }

  public int getNumberOfFreeParameters() {
    return numberOfParameters;  //To change body of implemented methods use File | Settings | File Templates.
  }

  public double getFreeParameter(int index) {
    return parameters[index];
  }

  public void setFreeParameter(int index, double value) {
    parameters[index] = value;
    setParameters();
    refreshFit = true;
  }

/*  public void setFreeParameter(int index, double value) {
    parameters[index] = (double) value;
    setParameters();
    refreshFit = true;
  }*/

  public void setFreeParameters(double[] parm) {
    for (int i = 0; i < getNumberOfFreeParameters(); i++) {
      parameters[i] = parm[i];
      setParameters();
      refreshFit = true;
    }
  }

/*  public void setFreeParameters(double[] parm) {
    for (int i = 0; i < getNumberOfFreeParameters(); i++) {
      parameters[i] = (double) parm[i];
      setParameters();
      refreshFit = true;
    }
  }*/

  public int totParameterNumber() {
    return parametersV.size();
  }

  public void setParameters() {
    // from parameters[] to the objects

    int pnumber = 0;
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines())
        apar.setValue(parameters[pnumber++]);
    }
    refreshFit = true;
  }

  public void setParameter(int index) {
    // from parameters[] to the objects

    int pnumber = -1;
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines()) {
        pnumber++;
        if (pnumber == index)
          apar.setValue(parameters[pnumber]);
      }
    }
    refreshFit = true;
  }

  public void setErrors(double[] errors) {
    // refresh the error value computed

    int pnumber = 0;
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines())
        apar.setError(errors[pnumber++]);
    }
  }

/*  public void setErrors(double[] errors) {
    // refresh the error value computed

    int pnumber = 0;
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines())
        apar.setError(errors[pnumber++]);
    }
  }*/

  public void saveparameters() {
    refreshparametersV();
    getfreeParameters();
  }

  public void computeFit() {
    setParameters();
    mainfunction(false, false);
//    getFit();

    refreshFit = false;
  }

  public boolean checkBound(int j, double parmn) {
    return false;  //To change body of implemented methods use File | Settings | File Templates.
  }

  public void backupallParameters() {
    // from the objects to parameters[]

//		refreshparametersV();
    backupPar = (Vector) parametersV.clone();
    getfreeParameters();
  }

  public void restoreParametersValues() {
//		refreshparametersV();
    if (backupPar != null) {
      int totnumberParameters = totParameterNumber();

      for (int i = 0; i < totnumberParameters; i++) {
        Parameter apar = (Parameter) parametersV.elementAt(i);
        Parameter backpar = (Parameter) backupPar.elementAt(i);
        if (!apar.getValue().equals(backpar.getValue()))
          apar.setValue(backpar.getValue());
      }
    }
    refreshFit = true;
  }

  public void getfreeParameters() {
    // from the objects to parameters[]

    int pnumber = 0;
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      if (apar.getFree() && apar.mayRefines()) {
        parameters[pnumber++] = (double) apar.getValueD();
//        lbound[pnumber] = (double) apar.getValueMinD();
//        ubound[pnumber++] = (double) apar.getValueMaxD();
      }
    }
  }

  public void setDerivate(boolean value) {
    computingDerivate = value;
  }

  public boolean isComputingDerivate() {
    return computingDerivate;
  }

  public void setOptimizing(boolean value) {
    isOptimizing = value;
  }

  public boolean isOptimizing() {
    return isOptimizing;
  }

  public void mainfunction(boolean hasoutput, boolean refreshAll) {

    //   boolean refreshPartially = false;  // speedModification
//    setOutput(hasoutput);
    if (Constants.testtime)
      Constants.tmpTime = System.currentTimeMillis();

    if (refreshAll)
      refreshAll(false);
    refreshCoefficients();
//b		else
//b			refreshPartially = true;  // speedModification

//    refreshDataIndices();

    for (int i = 0; i < numberOfData; i++) {
      int pole = poleindex[i];
      int point = pointindex[i];
//      System.out.println(pole +" "+point);
      fit[i] = (double) computeTextureFactor(phicosphi[1][pole][0], phicosphi[0][pole][0],
          textureAngles[0][pole][point], textureAngles[1][pole][point]);
    }

    indexesComputed = false;
    // to be implemented in subclasses

//    System.out.println("Computed fit");

    OutputPanel outputP = getFilePar().getMainFrame().getOutputPanel();

    if (hasoutput && outputP != null) {
      double[] indexes = getRefinementIndexes();
      outputP.appendnewline("Harmonic texture computation: " + String.valueOf(indexes[4]));
      outputP.appendnewline("Weighted Sum of Squares: " + String.valueOf(indexes[4]));
      outputP.appendnewline("Rw(%): " + indexes[0] * 100);
      outputP.appendnewline("Rwnb(%): " + indexes[1] * 100);
      outputP.appendnewline("R(%): " + indexes[2] * 100);
      outputP.appendnewline("Rnb(%): " + indexes[3] * 100);
    }
//		else if (Constants.testing)
//			System.out.println("Weighted Sum of Squares: " + String.valueOf(wss));

    if (Constants.testtime)
      System.out.println("Time for computation was: " + (System.currentTimeMillis() - Constants.tmpTime) +
          " millisecs.");
  }

  public boolean reduceMemory() {
    return true;  //To change body of implemented methods use File | Settings | File Templates.
  }

  public boolean singleFunctionComputing() {
    return false;  //To change body of implemented methods use File | Settings | File Templates.
  }

  public void computeFirstFit() {
    setParameters();

//		updateAllPhases();

    mainfunction(false, true);

    refreshFit = false;
  }

  public int getNumberofIterations() {
    return 5;  //To change body of implemented methods use File | Settings | File Templates.
  }

  public int prepareIteration() {

    refreshFit = true;

//    double range[];

    prepareComputation();

    Rw = 0.0;
    R = 0.0;

    System.out.println("Total number of data points: " + numberOfData);

//    dta = new double[numberOfData];
//    wgt = new double[numberOfData];
//    fit = new double[numberOfData];

    numberOfParameters = computeParameterNumber();

    System.out.println("Total number of refinable parameters: " + numberOfParameters);

    System.out.println("Memory needed (for Least Squares), > " + (numberOfData * numberOfParameters * 8) + " bytes");

    parameters = new double[numberOfParameters];
//    lbound = new double[numberOfParameters];
//    ubound = new double[numberOfParameters];
//    hasBounds = new boolean[numberOfParameters];

//    for (int i = 0; i < numberOfParameters; i++)
//      hasBounds[i] = false;

    getfreeParameters();

    return numberOfParameters;
  }

  public OutputStream getResultStream() {
    return null;  //To change body of implemented methods use File | Settings | File Templates.
  }

  public boolean logOutput() {
    return false;  //To change body of implemented methods use File | Settings | File Templates.
  }

  public void closeLogResultFile() {
    //To change body of implemented methods use File | Settings | File Templates.
  }

  public OptimizationAlgorithm getOptimizationAlgorithm() {
    LeastSquareFit lsf = new LeastSquareFit(this, "Harmonic texture refinement");
    lsf.setIterations(3);
    return lsf;
  }

  public void fittingFileOutput() {
    //To change body of implemented methods use File | Settings | File Templates.
  }

  public void refreshFit(double[] fit, double[] parm, int[] controls) {
    //To change body of implemented methods use File | Settings | File Templates.
  }

  public void setGSASBungeCoefficients(int lmax, int ncoeff, int sampleSymmetry, double omega, double chi, double phi,
                                       int[][] hklGSAS, double[] coeff) {
    Sample asample = getPhase().getSample();
    asample.setomega(Double.toString(omega));
    asample.setchi(Double.toString(chi));
    asample.setphi(Double.toString(phi));
    setExpansionDegree(lmax);
    switch (sampleSymmetry) {
      case 0:
        setSampleSymmetry(11);
        break;
      case 1:
        setSampleSymmetry(0);
        break;
      case 2:
        setSampleSymmetry(1);
        break;
      case 3:
        setSampleSymmetry(2);
        break;
      default:
        {
        }
    }
    applySymmetryRules();
    int[][] hkl = getCoeffHKL();
//    for (int i = 0; i < ncoeff; i++) {
//      coeff[i] *= MoreMath.pow(-1.0, hklGSAS[1][i] + hklGSAS[2][i]);  // first conversion from Bunge to Matthies
//    }
    int SLGnumber = 0;
    switch (sampleSymmetry) {
      case 0: // cylindrical       Cinfinity
        SLGnumber = 11;
        break;
      case 2: // monoclinic        C2
        SLGnumber = 1;
        break;
      case 3: // orthorhombic      D2
        SLGnumber = 2;
        break;
      case 1: // triclinic         C1
      default:
        {
          SLGnumber = 0;
        }
    }
    int cLGnumber = getLGnumber();
//    System.out.println(cLGnumber + " " + SLGnumber);
    for (int j = 0; j < numberHarmonicParameters(); j++) {
      double hcoeff = 0.0;
      int l = hkl[0][j];
//      System.out.println(l + " " + hkl[1][j] + " " + hkl[2][j]);
      for (int m = -l; m <= l; m++) {
        for (int n = -l; n <= l; n++) {
          for (int k = 0; k < ncoeff; k++) {
            if (hklGSAS[0][k] == l && hklGSAS[1][k] == m && hklGSAS[2][k] == n) {
              double Almn = getAlmn(cLGnumber, l, hkl[1][j], m);
              double AlmnStar = getAstarlmn(SLGnumber, l, hkl[2][j], n);
//              System.out.println(l + " " + m + " " + n + " " + Almn + " " + AlmnStar + " " + coeff[k]);
              hcoeff += Almn * coeff[k] * AlmnStar;
            }
          }
        }
      }
      if (Math.abs(hcoeff) < 1.0E-12)
        hcoeff = 0.0;
      getHarmonicParameter(j).setValue(hcoeff);
    }
  }

  private double getAstarlmn(int slGnumber, int l, int m, int n) {
    int n_rot, mhu2, mprime;
    switch (slGnumber) {
      case 1: //	2/m    C2
        n_rot = 2;
        mhu2 = (int) (m / 2);
        mprime = (int) MoreMath.pow(-1.0, m + 1) * n_rot * mhu2;
        if (Math.abs(mprime) == Math.abs(n)) {
          if (n >= 0)
            return 1.0;
          else
            return MoreMath.pow(-1.0, m + 1);
        }
        break;
      case 2: //	2/mmm  D2
        n_rot = 2;
        mhu2 = (int) (m - (1 + MoreMath.pow(-1.0, l)) / 2);
        mprime = (int) MoreMath.pow(-1.0, l) * n_rot * mhu2;
        if (Math.abs(mprime) == Math.abs(n)) {
          if (n >= 0)
            return 1.0;
          else
            return MoreMath.pow(-1.0, m + 1);
        }
        break;
      case 3: //	4/m    C4
        n_rot = 4;
        mhu2 = (int) (m / 2);
        mprime = (int) MoreMath.pow(-1.0, m + 1) * n_rot * mhu2;
        if (Math.abs(mprime) == Math.abs(n)) {
          if (n >= 0)
            return 1.0;
          else
            return MoreMath.pow(-1.0, m + 1);
        }
        break;
      case 9: //	m3     T
      case 10://	m3m    O
        return 1.0;
      case 4: //	4/mmm  D4
        n_rot = 4;
        mhu2 = (int) (m - (1 + MoreMath.pow(-1.0, l)) / 2);
        mprime = (int) MoreMath.pow(-1.0, l) * n_rot * mhu2;
        if (Math.abs(mprime) == Math.abs(n)) {
          if (n >= 0)
            return 1.0;
          else
            return MoreMath.pow(-1.0, m + 1);
        }
        break;
      case 5: //	-3     C3
        n_rot = 3;
        mhu2 = (int) (m / 2);
        mprime = (int) MoreMath.pow(-1.0, m + 1) * n_rot * mhu2;
        if (Math.abs(mprime) == Math.abs(n)) {
          if (n >= 0)
            return 1.0;
          else
            return MoreMath.pow(-1.0, m + 1);
        }
        break;
      case 6: //  -3m    D3
        n_rot = 3;
        mhu2 = (int) (m - (1 + MoreMath.pow(-1.0, l)) / 2);
        mprime = (int) MoreMath.pow(-1.0, l) * n_rot * mhu2;
        if (Math.abs(mprime) == Math.abs(n)) {
          if (n >= 0)
            return 1.0;
          else
            return MoreMath.pow(-1.0, m + 1);
        }
        break;
      case 7: //	6/m    C6
        n_rot = 6;
        mhu2 = (int) (m / 2);
        mprime = (int) MoreMath.pow(-1.0, m + 1) * n_rot * mhu2;
        if (Math.abs(mprime) == Math.abs(n)) {
          if (n >= 0)
            return 1.0;
          else
            return MoreMath.pow(-1.0, m + 1);
        }
        break;
      case 8: //	6/mmm  D6
        n_rot = 6;
        mhu2 = (int) (m - (1 + MoreMath.pow(-1.0, l)) / 2);
        mprime = (int) MoreMath.pow(-1.0, l) * n_rot * mhu2;
        if (Math.abs(mprime) == Math.abs(n)) {
          if (n >= 0)
            return 1.0;
          else
            return MoreMath.pow(-1.0, m + 1);
        }
        break;
      case 11://  fiber
        if (Math.abs(m) <= 1)
          mprime = 0;
        else
          return 0;
        if (Math.abs(mprime) == Math.abs(n)) {
          if (n >= 0)
            return 1.0;
          else
            return MoreMath.pow(-1.0, m + 1);
        }
        break;
      case 0: //  -1    C1
      default:
        {
          n_rot = 1;
          mhu2 = (int) (m / 2);
          mprime = (int) MoreMath.pow(-1.0, m + 1) * n_rot * mhu2;
          if (Math.abs(mprime) == Math.abs(n)) {
            if (n >= 0)
              return 1.0;
            else
              return MoreMath.pow(-1.0, m + 1);
          }
        }
    }
    return 0;
  }

  private double getAlmn(int lGnumber, int l, int m, int n) {
    int n_rot, mhu2, mprime;
    switch (lGnumber) {
      case 1: //	2/m    C2
        n_rot = 2;
        mhu2 = (int) (m / 2);
        mprime = (int) MoreMath.pow(-1.0, m + 1) * n_rot * mhu2;
        if (Math.abs(mprime) == Math.abs(n))
          return 1.0;
/*        if (Math.abs(mprime) == Math.abs(n)) {
          if (mprime == n)
            return 1.0;
          else
            return -1.0;
        }*/
        break;
      case 2: //	2/mmm  D2
        n_rot = 2;
        mhu2 = (int) (m - (1 + MoreMath.pow(-1.0, l)) / 2);
        mprime = (int) MoreMath.pow(-1.0, l) * n_rot * mhu2;
        if (Math.abs(mprime) == Math.abs(n))
          return 1.0;
/*        if (Math.abs(mprime) == Math.abs(n)) {
          if (mprime == n)
            return 1.0;
          else
            return -1.0;
        }*/
        break;
      case 3: //	4/m    C4
        n_rot = 4;
        mhu2 = (int) (m / 2);
        mprime = (int) MoreMath.pow(-1.0, m + 1) * n_rot * mhu2;
        if (Math.abs(mprime) == Math.abs(n))
          return 1.0;
/*        if (Math.abs(mprime) == Math.abs(n)) {
          if (mprime == n)
            return 1.0;
          else
            return -1.0;
        }*/
        break;
      case 9: //	m3     T
      case 10://	m3m    O
        return 1.0;
      case 4: //	4/mmm  D4
        n_rot = 4;
        mhu2 = (int) (m - (1 + MoreMath.pow(-1.0, l)) / 2);
        mprime = (int) MoreMath.pow(-1.0, l) * n_rot * mhu2;
//        System.out.println("C1 " + mprime + " " + n + " " + m + " " + n_rot + " " + mhu2);
        if (Math.abs(mprime) == Math.abs(n))
          return 1.0;
/*        if (Math.abs(mprime) == Math.abs(n)) {
          if (mprime == n)
            return 1.0;
          else
            return -1.0;
        }*/
        break;
      case 5: //	-3     C3
        n_rot = 3;
        mhu2 = (int) (m / 2);
        mprime = (int) MoreMath.pow(-1.0, m + 1) * n_rot * mhu2;
        if (Math.abs(mprime) == Math.abs(n)) {
          if (n >= 0)
            return 1.0;
          else
            return MoreMath.pow(-1.0, m + 1);
        }
        break;
      case 6: //  -3m    D3
        n_rot = 3;
        mhu2 = (int) (m - (1 + MoreMath.pow(-1.0, l)) / 2);
        mprime = (int) MoreMath.pow(-1.0, l) * n_rot * mhu2;
        if (Math.abs(mprime) == Math.abs(n))
          return 1.0;
/*        if (Math.abs(mprime) == Math.abs(n)) {
          if (mprime == n)
            return 1.0;
          else
            return -1.0;
        }*/
        break;
      case 7: //	6/m    C6
        n_rot = 6;
        mhu2 = (int) (m / 2);
        mprime = (int) MoreMath.pow(-1.0, m + 1) * n_rot * mhu2;
        if (Math.abs(mprime) == Math.abs(n))
          return 1.0;
/*        if (Math.abs(mprime) == Math.abs(n)) {
          if (mprime == n)
            return 1.0;
          else
            return -1.0;
        }*/
        break;
      case 8: //	6/mmm  D6
        n_rot = 6;
        mhu2 = (int) (m - (1 + MoreMath.pow(-1.0, l)) / 2);
        mprime = (int) MoreMath.pow(-1.0, l) * n_rot * mhu2;
        if (Math.abs(mprime) == Math.abs(n))
          return 1.0;
/*        if (Math.abs(mprime) == Math.abs(n)) {
          if (mprime == n)
            return 1.0;
          else
            return -1.0;
        }*/
        break;
      case 11://  fiber
        if (Math.abs(m) <= 1)
          mprime = 0;
        else
          return 0;
        if (Math.abs(mprime) == Math.abs(n))
          return 1.0;
/*        if (Math.abs(mprime) == Math.abs(n)) {
          if (mprime == n)
            return 1.0;
          else
            return -1.0;
        }*/
        break;
      case 0: //  -1    C1
      default:
        {
          n_rot = 1;
          mhu2 = (int) (m / 2);
          mprime = (int) MoreMath.pow(-1.0, m + 1) * n_rot * mhu2;
          if (Math.abs(mprime) == Math.abs(n))
            return 1.0;
        }
    }
    return 0;
  }

  private int[][] getCoeffHKL() {
    int[][] hkl = new int[3][numberHarmonicParameters()];
    int k = 0;
    for (int l = 2; l <= expansionDegree; l += 2) {
      int ml2 = SphericalHarmonics.getN(LGIndex, l);
      for (int m = 1; m <= ml2; m++) {
        int nl2 = SphericalHarmonics.getN(sampleSymmetry, l);
        for (int n = 1; n <= nl2; n++) {
          hkl[0][k] = l;
          hkl[1][k] = m;
          hkl[2][k++] = n;
        }
      }
    }
    return hkl;
  }

  public double getLowerBound(int index) {
    return -1.0E30f;
  }

  public double getUpperBound(int index) {
    return 1.0E30f;
  }

  public double getParameterMinSignificantValue(int i) {
    return 0;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JHTextureOptionsD(parent, this);
    return adialog;
  }

  class JHTextureOptionsD extends JOptionsDialog {

    JComboBox symmetryCB;
    HarmonicPane harmonicCoefficientP;
    JLabel sharpL = null;

    public JHTextureOptionsD(Frame parent, XRDcat obj) {

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

      jp3 = new JPanel();
      jp3.setLayout(new FlowLayout());
      jp1.add(jp3);
      JButton jb = new JButton("Compute");
      jp3.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          setCursor(new Cursor(Cursor.WAIT_CURSOR));
          setSharpness(computeAndGetSharpness());
          sharpL.setText("ODF sharpness: " + getSharpness());
          setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        }
      });
      jb.setToolTipText("Press this to compute the texture sharpness");
      jp3.add(sharpL = new JLabel("ODF sharpness: " + getSharpness()));

      jp3 = new JPanel();
      jp3.setLayout(new FlowLayout());
      jp1.add(jp3);
      jp3.add(new JLabel("Export PFs for "));
      jp3.add(jb = new JButton("Beartex"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          exportPFsinBEARTEXformat();
        }
      });
      jb.setToolTipText("Press this to save the PFs using the Beartex format");

      jp3 = new JPanel();
      jp3.setLayout(new FlowLayout());
      jp1.add(jp3);
      jp3.add(new JLabel("Export Coeffs for "));
      jp3.add(jb = new JButton("Beartex"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          exportCoeffinBEARTEXformat();
        }
      });
      jb.setToolTipText("Press this to save the coefficients in the Beartex format");

      jp3 = new JPanel();
      jp3.setLayout(new FlowLayout());
      jp1.add(jp3);
      jp3.add(new JLabel("Import Coefficient from "));
      jp3.add(jb = new JButton("GSAS"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          importCoefficientFromGSASExp();
        }
      });
      jb.setToolTipText("Press this to import harmonic coefficients from GSAS exp file");

      jp3 = new JPanel(new FlowLayout());
      jp1.add(jp3);
      jp3.add(jb = new JButton("Compute ODF from PF"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          (new PersistentThread() {
            public void executeJob() {
              loadPFandComputeODF(JHTextureOptionsD.this);
            }
          }).start();
        }
      });
      jb.setToolTipText("Press this to compute the ODF from traditional Pole Figures");
      setTitle("Harmonic texture options panel");
      initParameters();
      pack();

      symmetryCB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          setSampleSymmetry(symmetryCB.getSelectedItem().toString());
          applySymmetryRules();
        }
      });

      harmonicCoefficientP.initListener();
      harmonicCoefficientP.setSliderValue(expansionDegree);
    }

    public void initParameters() {
      applySymmetryRules();
      symmetryCB.setSelectedItem(getSampleSymmetry());
      harmonicCoefficientP.setExpansionSlider(0, 22);
      harmonicCoefficientP.setList(XRDparent, 0);
    }

    public void retrieveParameters() {
      setSampleSymmetry(symmetryCB.getSelectedItem().toString());
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

      refreshCoefficients();

      BufferedWriter PFwriter = Misc.getWriter(filename);

      String commentLine = new String("Harmonic coeeficients from RiTA computing of phase: " + phase.toXRDcatString());

      try {
        PFwriter.write(commentLine);
        PFwriter.write(Constants.lineSeparator);

        PFwriter.write(filename);
        PFwriter.write(Constants.lineSeparator);

        PFwriter.write("    " + Integer.toString(SpaceGroups.getLGNumberSiegfriedConv(phase.getPointGroup())) + "    " +
            Integer.toString(getSampleSymmetryValue() + 1));
        PFwriter.write(Constants.lineSeparator);

//			new String("    1.0000    1.0000    1.0000   90.0000   90.0000   90.0000");
        PFwriter.write(Misc.getFirstHHline(phase));
        PFwriter.write(Constants.lineSeparator);

        String firstline = new String("    10.000    40.411     1.000   " + Integer.toString(expansionDegree));
        PFwriter.write(firstline);
        PFwriter.write(Constants.lineSeparator);

        int k = 0;
        for (int l = 2; l <= expansionDegree; l += 2) {
          int ml2 = SphericalHarmonics.getN(LGIndex, l - 1);
          for (int m = 1; m <= ml2; m++) {
            int nl2 = SphericalHarmonics.getN(sampleSymmetry, l - 1);
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
            int nl2 = SphericalHarmonics.getN(sampleSymmetry, l);
            for (int n = 1; n <= nl2; n++) {
              PFwriter.write("  " + Integer.toString(l) + "  ");
              PFwriter.write(Integer.toString(m) + "  ");
              PFwriter.write(Integer.toString(n) + "  ");
              double coeff = coefficient[k++]; // * (2 * l + 1) / Math.sqrt(8.0 * Constants.PI);
              PFwriter.write(Fmt.format(coeff));
              PFwriter.write(Constants.lineSeparator);
            }
          }
        }
      } catch (IOException io) {
      }

      try {
        PFwriter.flush();
        PFwriter.close();
      } catch (IOException io) {
      }
    }

    public void importCoefficientFromGSASExp() {
      int Lmax = 0, Ncoeff = 0, sampleSymmetry = 0;
      double omega = 0.0, chi = 0.0, phi = 0.0;
      int[][] hkl = null;
      double[] coeff = null;
      String filename = Utility.openFileDialog(JHTextureOptionsD.this, "Load GSAS Exp file", FileDialog.LOAD,
          (String) MaudPreferences.getPref(FilePar.analysisPath, Constants.documentsDirectory),
          null, "");
      if (filename != null) {
        Vector phaseList = new Vector(0, 1);
        BufferedReader PFreader = Misc.getReader(filename);

        if (PFreader != null) {
          try {
//            int lastIndex = 0;
            String line = PFreader.readLine();
            while (line != null) {
              if (line.startsWith("CRS")) {
//                System.out.println(line.substring(8, 12));
                if (line.substring(8, 12).equalsIgnoreCase("PNAM")) {
                  String name = line.substring(14);
                  while (name.length() > 0 && name.endsWith(" ")) {
                    name = name.substring(0, name.length() - 1);
                  }
                  phaseList.addElement(name);
//                  lastIndex++;
                }
              }
              line = PFreader.readLine();
            }
            PFreader.close();
          } catch (IOException io) {
            io.printStackTrace();
            try {
              PFreader.close();
            } catch (IOException ion) {
            }
          }
        }

        int selectIndex = (new ListSelection()).getSelection(phaseList, "Select the phase to import", false);
        if (selectIndex >= 0) {
          PFreader = Misc.getReader(filename);
          String phaseIndentifier = "CRS" + Integer.toString(selectIndex + 1) + "  OD";
          if (PFreader != null) {
            try {
              String line = PFreader.readLine();
              while (line != null && !(line.startsWith(phaseIndentifier))) {
                line = PFreader.readLine();
              }
              if (line != null) {
                line = line.substring(phaseIndentifier.length());
                StringTokenizer st = new StringTokenizer(line, " ,\t\n\r");
                String token = null;
                int index = 0;
                while (st.hasMoreTokens()) {
                  token = st.nextToken();
                  switch (index++) {
                    case 0:
                      Lmax = Integer.parseInt(token);
                      break;
                    case 1:
                      Ncoeff = Integer.parseInt(token);
                      break;
                    case 2:
                      sampleSymmetry = Integer.parseInt(token);
                      break;
                    case 5:
                      omega = Double.parseDouble(token);
                      break;
                    case 6:
                      chi = Double.parseDouble(token);
                      break;
                    case 7:
                      phi = Double.parseDouble(token);
                      break;
                    default:
                      {
                      }
                  }
                }
                hkl = new int[3][Ncoeff];
                coeff = new double[Ncoeff];
                line = PFreader.readLine();
                int indexCoeff = 0;
                int indexHKL = 0;
                while (line != null && line.startsWith(phaseIndentifier)) {
                  if (line.substring(11, 12).toUpperCase().equals("A")) {
                    line = line.substring(12);
//        			String[] data = readFortranLine(linedata,inputString,number,repeat);
                    int[] digits = new int[3];
                    digits[0] = 4;
                    digits[1] = 3;
                    digits[2] = 3;
                    int repeat = 6;
                    int number = 3;
                    if (Ncoeff < indexHKL + repeat)
                      repeat = Ncoeff - indexHKL;
                    String[] data = Misc.readFormattedLine(line, digits, number, repeat);
                    for (int j = 0; j < repeat * number; j += number) {
                      hkl[0][indexHKL] = Integer.parseInt(Misc.toStringDeleteBlankAndTab(data[j]));
                      // inverting m and n from GSAS to get standard notation
                      hkl[2][indexHKL] = Integer.parseInt(Misc.toStringDeleteBlankAndTab(data[j + 1]));
                      hkl[1][indexHKL++] = Integer.parseInt(Misc.toStringDeleteBlankAndTab(data[j + 2]));
                    }
/*                    st = new StringTokenizer(line, " ,\t\n\r");
                    while (st.hasMoreTokens()) {
                      hkl[0][indexHKL] = Integer.parseInt(st.nextToken());
                      hkl[2][indexHKL] = Integer.parseInt(st.nextToken());   // inverting m and n from GSAS to get
                      hkl[1][indexHKL++] = Integer.parseInt(st.nextToken()); // standard notation
                    }*/
                  } else if (line.substring(11, 12).toUpperCase().equals("B")) {
                    line = line.substring(12);
                    while (line.length() > 0 && line.endsWith(" ")) {
                      line = line.substring(0, line.length() - 1);
                    }
                    while (line.length() >= 10) {
                      coeff[indexCoeff++] = Double.parseDouble(line.substring(0, 10));
                      line = line.substring(10);
                    }
                  }
                  line = PFreader.readLine();
                }
              }
              PFreader.close();
            } catch (IOException io) {
              io.printStackTrace();
              try {
                PFreader.close();
              } catch (IOException ion) {
              }
            }
          }
        }
      }
      System.out.println("Harmonic ODF imported from GSAS exp file:");
      System.out.println("Lmax :" + Lmax);
      System.out.println("Ncoefficient :" + Ncoeff);
      System.out.println("Sample sysmmetry :" + sampleSymmetry);
      System.out.println("Omega :" + omega);
      System.out.println("Chi :" + chi);
      System.out.println("Phi :" + phi);
      for (int i = 0; i < Ncoeff; i++)
        System.out.println("C(" + hkl[0][i] + "," + hkl[1][i] + "," + hkl[2][i] + ")" + "=" + coeff[i]);

      setGSASBungeCoefficients(Lmax, Ncoeff, sampleSymmetry, omega, chi, phi, hkl, coeff);

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
        setExpansionDegree(value);
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
