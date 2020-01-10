/*
 * @(#)WIMVTexture.java created 16/07/1998 ILL, Grenoble
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

package it.unitn.ing.rista.diffr.rta;

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.awt.Utility;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

/**
 * The WIMVTexture is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.25 $, $Date: 2006/12/04 14:30:05 $
 * @since JDK1.1
 */

/*  extracted from WIMV original program by S. Matthies

		the WIMV parameters
C
C                      ITZ        (*)         [20]
C                      ABPRO      (*)         [1.]
C                      VITPRO     (*)         [.3]
C                      PEPS       (*)        [.05]
C                      IZICK      (*)          [1]
C                      IRP        (*)          [0]
C                      R          (*)         [2.]
C                      RFAK       (*)        [1.1]
C                      IANFSU     (*)          [1]
C                      RFANF      (*)         [2.]
C
C  Meaning of the parameters :
C  -------------------------
C
C        ITZ               - maximum number of iteration steps.
C        ABPRO  (per cent) - Stop if RP for each PF-value will be lower.
C        VITPRO (per cent) - Stop if the velocity of iteration will be lower.
C        R                 -  Starting value of the iteration exponent.
C        PEPS   (per cent) - "RP0"-level.
C        IZICK  = 1        - "Zickzack regime" for the iteration exponent R.
C        IANFSU = 0        - No optimization of the Starting ODF approximation
C                            estimated using the exponent RFANF.
C        IRP = 0/1         - Optimization criteria for the starting ODF
C                            approximation is formulated using RP or RP1.
C        RFAK              - Factor changing the R-value if the Zickzack
C                            regime is working.
C
*/


public class WIMVTexture extends DiscreteODFTexture {
  
  public static String[] diclistc = {"_rita_generate_symmetry", "_rita_wimv_sum_coincidence",
      "_rita_wimv_iteration_max", "_rita_wimv_Rp_minimum", // ITZ, ABPRO
      "_rita_wimv_speed_minimum", "_rita_wimv_exponent", // VITPRO, R
      "_rita_wimv_RPO_level", "_rita_wimv_zigzag_factor", // PEPS, RFAK
      "_rita_wimv_zigzag", "_rita_wimv_ODF_optimization", // IZICK, IANFSU/IRP
      "_rita_wimv_refl_min_int", "_rita_odf_sharpness",
      "_rita_wimv_odf_resolution", "_rita_odf_refinable",
      
      "_rita_interpolation_method"
  };
  public static String[] diclistcrm = {"_rita_generate_symmetry", "_rita_wimv_sum_coincidence",
      "_rita_wimv_iteration_max", "_rita_wimv_Rp_minimum", // ITZ, ABPRO
      "_rita_wimv_speed_minimum", "_rita_wimv_exponent", // VITPRO, R
      "_rita_wimv_RPO_level", "_rita_wimv_zigzag_factor", // PEPS, RFAK
      "_rita_wimv_zigzag", "_rita_wimv_ODF_optimization", // IZICK, IANFSU/IRP
      "_rita_wimv_refl_min_int", "_rita_odf_sharpness",
      "_rita_wimv_odf_resolution", "_rita_odf_refinable",
      
      "_rita_interpolation_method"
  };
  public static String[] classlistcs = {"superclass:it.unitn.ing.rista.diffr.rta.Interpolation"};
  public static String[] classlistc = {};
  public static String[] ODFoptimization = {"not enabled", "Rp", "Rp1"};
  public static int ODFoptimizationnumber = 3;
  public static String[] ZigZag = {"not enabled", "enabled"};
  public static int ZigZagnumber = 2;
  public static String[] WIMVtooltip = {"maximum number of iteration steps",
      "Stop if RP for each PF-value will be lower",
      "Stop if the velocity of iteration will be lower",
      "Starting value of the iteration exponent",
      "RP0 level",
      "Factor changing the R-value if the Zickzack regime is working",
      "Zickzack regime for the iteration exponent R",
      "Optimization of the Starting ODF approximation (off or using as exponent Rp or Rp1)"
  };
//  Sample actualsample = null;
  
  double[][] interpolatedPoleFigures = null;
  //  double odf[][][] = null;
//  int numberPoleFigures = 0;
//  double resolution = MaudPreferences.getDouble(Texture.prefs[0], Texture.prefVal[0]);
  int numberPoleFiguresPF = 0;
  boolean fromPF = false;
  int[][][] hklPF = null;
  int[] izoveriPF = null;
  double[][] weightSingle = null;
  public static double phonstepeps = .005;
  public static double pi2deg = Constants.PI / 180.0;
  
  public WIMVTexture(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "WIMV";
    IDlabel = "WIMV";
    description = "select this to apply WIMV model";
  }
  
  public WIMVTexture(XRDcat aobj) {
    this(aobj, "WIMV");
  }
  
  public WIMVTexture() {
    identifier = "WIMV";
    IDlabel = "WIMV";
    description = "select this to apply WIMV model";
  }
  
  public void initConstant() {
    Nstring = 14;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 1;
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
    
    setSampleSymmetry(0);
    setWIMVstatus(true);
    stringField[2] = new String(MaudPreferences.getPref("texture.maxWIMVIterations", "20"));
    stringField[3] = new String("1.0");
    stringField[4] = new String("0.01");
    stringField[5] = new String("2.0");
    stringField[6] = new String("0.05");
    stringField[7] = new String("1.1");
    stringField[10] = new String(MaudPreferences.getPref(Texture.prefs[5], Texture.prefVal[5]));
    setZigZag(1);
    setODFoptimization(1);
    setResolution(MaudPreferences.getPref(Texture.prefs[0], Texture.prefVal[0]));
    setInterpolationMethod("Triangular");
    stringField[13] = "true";
  }
  
  public void refreshForNotificationDown(XRDcat source, int reason) {
    if (!getFilePar().isComputingDerivate() || (source == this ||
        (reason == Constants.SAMPLE_ORIENTATION_CHANGED || (source == getParent() &&
            (reason == Constants.STRAIN_CHANGED || reason == Constants.CELL_CHANGED)))))
      refreshComputation = true;
  }
  
  public String getWIMVOption(int i) {
    return stringField[i + 2];
  }
  
  public void setWIMVOption(int i, String value) {
    if (i == 0) {
      stringField[i + 2] = new String(Integer.toString(Integer.valueOf(value).intValue()));
    } else
      stringField[i + 2] = new String(value);
  }
  
  public int getIteration() {
    return Integer.valueOf(getWIMVOption(0)).intValue();
  }
  
  public double getRpMinimum() {
    return Double.valueOf(getWIMVOption(1)).doubleValue();
  }
  
  public double getSpeedMinimum() {
    return Double.valueOf(getWIMVOption(2)).doubleValue();
  }
  
  public double getRexponent() {
    return Double.valueOf(getWIMVOption(3)).doubleValue();
  }
  
  public double getRPOlevel() {
    return Double.valueOf(getWIMVOption(4)).doubleValue();
  }
  
  public double getRfactor() {
    return Double.valueOf(getWIMVOption(5)).doubleValue();
  }
  
  public void setMinimumIntensity(String value) {
    stringField[10] = new String(value);
  }
  
  public String getMinimumIntensity() {
    return stringField[10];
  }
  
  public double getMinimumIntensityD() {
    return Double.valueOf(getMinimumIntensity()).doubleValue();
  }
  
  public void setSharpness(String value) {
    setString(11, value);
  }
  
  public String getSharpness() {
    return getString(11);
  }
  
  public String getSampleSymmetry() {
    return stringField[0];
  }
  
  public void setResolution(String value) {
    double res = 0.0;
    try {
      res = Double.parseDouble(stringField[12]);
    } catch (Exception e) {
    }
    if (value != null && Double.parseDouble(value) != res) {
      stringField[12] = new String(value);
      resolution = getResolutionD();
      resetODF();
    }
  }
  
  public String getResolution() {
    return stringField[12];
  }
  
  public double getResolutionD() {
    return Double.valueOf(getResolution()).doubleValue();
  }
  
  public void setSampleSymmetry(int i) {
    stringField[0] = new String(symmetrychoice[i]);
  }
  
  public void setSampleSymmetry(String value) {
    stringField[0] = new String(value);
  }
  
  public boolean getWIMVstatus() {
    return stringField[1].equalsIgnoreCase("true");
  }
  
  public void setWIMVstatus(boolean status) {
    if (status)
      stringField[1] = new String("true");
    else
      stringField[1] = new String("false");
  }
  
  public void setWIMVstatus(String value) {
    stringField[1] = new String(value);
  }
  
  public boolean ODFisRefinable() {
    return stringField[13].equalsIgnoreCase("true");
  }
  
  public void setODFrefinable(boolean status) {
    if (status)
      stringField[13] = new String("true");
    else
      stringField[13] = new String("false");
  }
  
  public void setODFrefinable(String value) {
    stringField[13] = new String(value);
  }
  
  public String getZigZag() {
    return stringField[8];
  }
  
  public int getZigZagValue() {
    for (int i = 0; i < ZigZagnumber; i++)
      if (getZigZag().equalsIgnoreCase(ZigZag[i]))
        return i;
    return 1;
  }
  
  public void setZigZag(int i) {
    stringField[8] = new String(ZigZag[i]);
  }
  
  public void setZigZag(String value) {
    stringField[8] = new String(value);
  }
  
  public String getODFoptimization() {
    return stringField[9];
  }
  
  public int getODFoptimizationValue() {
    int result = 0;
    for (int i = 0; i < ODFoptimizationnumber; i++)
      if (getODFoptimization().equalsIgnoreCase(ODFoptimization[i]))
        result = i;
    if (result > 1)
      result = 1;
    return result;
  }
  
  public int getODFoptimizationCriteria() {
    int result = 0;
    for (int i = 0; i < ODFoptimizationnumber; i++)
      if (getODFoptimization().equalsIgnoreCase(ODFoptimization[i]))
        result = i - 1;
    
    if (result < 0)
      result = 0;
    return result;
  }
  
  public void setODFoptimization(int i) {
    stringField[9] = new String(ODFoptimization[i]);
  }
  
  public void setODFoptimization(String value) {
    stringField[9] = new String(value);
  }
  
  public void setInterpolationMethod(String value) {
    if (subordinateField[0] == null || !value.equals(subordinateField[0].identifier))
      setsubordinateField(getInterpolationID(), value);
  }
  
  public String getInterpolationMethod() {
    return getActiveInterpolation().identifier;
  }
  
  public void setInterpolationMethod(int number) {
    setInterpolationMethod(getsubordIdentifier(getInterpolationID(), number));
  }
  
  public int getInterpolationID() {
    return 0;
  }
  
  public Interpolation getActiveInterpolation() {
    if (subordinateField[getInterpolationID()] == null)
      setInterpolationMethod(0);
    return (Interpolation) subordinateField[getInterpolationID()];
  }

/*  public int getLaueGroupNumber() {
    return SpaceGroups.getLGNumberSiegfriedConv(getPhase());
  }*/
  
  public int getLaueGroupNumberS() {
    return 1;
  }
  
  public int getPoleFigureNumberAll() {
    return getPhase().gethklNumber();
  }
  
  public int getPoleFigureNumber() {
    if (fromPF)
      return numberPoleFiguresPF;
    else
      return numberPoleFigures;
  }
  
  public int getPointNumber(int pole) {
    return getFilePar().getActiveSample().getNumberActiveDatafiles()
        * getSampleSymmetryMultiplicity();
  }
  
  public Reflection getReflectionAll(int pole) {
    return getPhase().getReflectionVector().elementAt(pole);
  }
  
  public Reflection getReflection(int pole, int partial) {
    return getPhase().getReflectionVector().elementAt(poleFigureIndex[pole] + partial);
  }
  
  public double[] getTextureAngles(int pole, int point) {
    Reflection reflex = getReflection(pole, 0);
    
    return applySampleSymmetry(reflex, point, getFilePar().getActiveSample().getNumberActiveDatafiles());
  }
  
  public int getPoint(Reflection reflex, int point) {
    int truepointmax = getFilePar().getActiveSample().getNumberActiveDatafiles();
    int sector = point / truepointmax;
    int residual = point - sector * truepointmax;
    return residual;
  }
  
  public DiffrDataFile getPointFromAll(int point) {
    Sample asample = getFilePar().getActiveSample();
    int truepointmax = asample.getNumberActiveDatafiles();
    int sector = point / truepointmax;
    int residual = point - sector * truepointmax;
    return asample.getActiveDiffrDataFile(residual);
  }
  
  public double[] applySampleSymmetry(Reflection reflex, int point, int truepointmax) {
    int sector = point / truepointmax;
    int residual = point - sector * truepointmax;
    
    double[] angles = getFilePar().getActiveSample().getActiveTextureAngles(reflex, residual);
    
    if (angles[0] < 0) {
      angles[0] = -angles[0];
      angles[1] += 180.0f;
    }
    while (angles[0] >= 360.0) {
      angles[0] -= 360.0f;
    }
    if (angles[0] >= 180.0) {
      angles[0] -= 180.0f;
//			angles[1] += 180.0f; // to test
    }
    if (angles[0] >= 90.0) {
      angles[0] = 180.0f - angles[0];
      angles[1] += 180.0f;
    }
    while (angles[1] < 0.0)
      angles[1] += 360.0f;
    while (angles[1] >= 360.0)
      angles[1] -= 360.0f;
    
    
    if (truepointmax > point)
      return angles;
    
    int fold;
    double phiturn;
    
    int multiplicity = getSampleSymmetryValue();
    switch (multiplicity) {
      case 1:
      case 2:
      case 3:
        fold = multiplicity + 1;
        phiturn = 360f / fold;
        angles[1] += sector * phiturn;
        break;
      case 4:
        fold = 6;
        phiturn = 360f / fold;
        angles[1] += sector * phiturn;
        break;
      case 5:
        angles[1] = 360.f - angles[1];
        break;
      case 6:
        switch (sector) {
          case 1:
            angles[1] = 180.f - angles[1];
            break;
          case 2:
            angles[1] += 180.f;
            break;
          case 3:
            angles[1] = 360.f - angles[1];
            break;
          default: {
          }
        }
        break;
      case 7:
        fold = 72;
        phiturn = 360f / fold;
        angles[1] += sector * phiturn;
        break;
      default: {
      }
    }
    while (angles[1] < 0)
      angles[1] += 360.0f;
    while (angles[1] >= 360.0)
      angles[1] -= 360.0f;
    
    return angles;
  }
  
  public double getPoleIntensity(int pole, int point) {
    int izoveri = getIzoveri(pole);
    double texturefactor = 0.0;
    double wgt, wgtsum = 0.0;
    Phase phase = getPhase();
    for (int i = 0; i < izoveri; i++) {
      Reflection reflex = getReflection(pole, i);
      wgt = reflex.getWeight();
      wgtsum += wgt;
      int reflexIndex = poleFigureIndex[pole] + i;
//		 	int mult = reflex.multiplicity;
      texturefactor += getPointFromAll(point).getExperimentalTextureFactor(phase, reflexIndex, 0) *         // todo: v3.0  for all radiations?
          wgt; // * mult;
    }
    return texturefactor / wgtsum;
  }
  
  public double getWeight(int pole, int point) {
/*		Reflection reflex = getReflection(pole, 0);
		return reflex.getWeight(getPointFromAll(reflex, point));*/
    return 1.0;
  }
  
  public int getH(int pole) {
    return getH(pole, 0);
  }
  
  public int getK(int pole) {
    return getK(pole, 0);
  }
  
  public int getL(int pole) {
    return getL(pole, 0);
  }
  
  public int getIzoveri(int pole) {
    if (fromPF) {
//  System.out.println(pole + " Izoveri " + izoveriPF[pole]);
      return izoveriPF[pole];
    } else {
//  System.out.println(pole + ", izoveri " + getReflection(pole, 0).izoveri);
      return getReflection(pole, 0).izoveri;
    }
  }
  
  public int getH(int pole, int partial) {
    if (fromPF)
      return hklPF[0][pole][partial];
    else
      return getReflection(pole, partial).getH();
  }
  
  public int getK(int pole, int partial) {
    if (fromPF)
      return hklPF[1][pole][partial];
    else
      return getReflection(pole, partial).getK();
  }
  
  public int getL(int pole, int partial) {
    if (fromPF)
      return hklPF[2][pole][partial];
    else
      return getReflection(pole, partial).getL();
  }
  
  public double getWeightSingle(int pole, int partial) {
    if (fromPF) {
//    System.out.println("pole "+pole+"  partial "+partial+" "+Double.toXRDcatString(weightSingle[pole][partial]));
      return weightSingle[pole][partial];
    } else {
/*      int izoveri = getIzoveri(pole);
      double wgtsum = 0.0;
      for (int i = 0; i < izoveri; i++) {
        Reflection reflex = getReflection(pole, i);
        wgtsum += reflex.getWeight();
      }      */
      Reflection reflex = getReflection(pole, partial);
      return reflex.getOverlappedWeight(); // / wgtsum;
    }
  }
  
  public Phase getPhase() {
    return (Phase) getParent();
  }
  
  public void computeTextureFactor(Phase aphase, Sample asample) {
    
    if (odf == null) { //odfnotLoaded) {
      loadOdfFromFile();
      refreshComputation = true;
    }
    
    if (!refreshComputation)
      return;
    
    FilePar aparFile = getFilePar();
    
    fromPF = false;
    
    if (aparFile.isTextureComputationPermitted() && ODFisRefinable()) {
//      actualsample = asample;
      interpolatedPoleFigures = getActiveInterpolation().computeInterpolation();
      odf = (new Uwimvuo(this, getPoleFigureNumber(), numberOfPoleFigureIzoveri)).computeODF(interpolatedPoleFigures);
      
    }
    refreshComputation = false;
    
    if (odf == null)
      return;
    
    recomputedTextureFactor(aphase, asample, true);

/*    double[] cdsc = aphase.lattice();

    double phoninp = Uwimvuo.subfmin(odf, getAlphamax(getResolutionD()));

    int hkln = aphase.gethklNumber();
    for (int j = 0; j < hkln; j++) {
      Reflection refl = (Reflection) aphase.reflectionv.elementAt(j);
      double energy = (double) aphase.getDspacing(j);

      double[] sctf = Uwimvuo.tfhkl(refl.h, refl.k, refl.l, cdsc[7], cdsc[5], cdsc[3], cdsc[6], cdsc[0], cdsc[1]);
      double fhir = Math.acos(sctf[3]);
      int inv = Uwimvuo.equiv(getLaueGroupNumber(), sctf);

      for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
        DataFileSet adataset = asample.getActiveDataSet(i);
        int datafilenumber = adataset.activedatafilesnumber();
        double[][] texture_angles = new double[4][datafilenumber];
        adataset.computeActiveTextureAngles(asample, energy, texture_angles);
        double[] textF = computeTextureFactor(odf, cdsc, texture_angles, sctf, fhir, inv,
                phoninp, getResolutionD());
        for (int i1 = 0; i1 < datafilenumber; i1++) {
          DiffrDataFile adatafile = adataset.getActiveDataFile(i1);
          refl.setTextureFactor(adatafile.getIndex(), textF[i1]);
//					refl.setExpTextureFactor(adatafile.getIndex(), textF);
        }
      }
    }*/
  }
  
  public double[] computeTextureFactor(double[][] texture_angles,
                                       double[] sctf, double fhir, int inv) {
    
    if (odf != null) {
      inv = Uwimvuo.equiv(getLaueGroupNumber(), sctf);
      double phoninp = Uwimvuo.subfmin(odf, Uwimvuo.getAlphamax(getResolutionD()));
      double[] cdsc = getPhase().lattice();
      double res = getResolutionD();
      return computeTextureFactor(odf, cdsc, texture_angles,
          sctf, fhir, inv, phoninp, res);
      
    }
    return null;
  }
  
  public double[] computeTextureFactor(ReflectionTexture reflectionTexture) {
    
    if (odf != null) {
      double phoninp = Uwimvuo.subfmin(odf, Uwimvuo.getAlphamax(getResolutionD()));
      double res = getResolutionD();
      return computeTextureFactor(odf, reflectionTexture, phoninp, res);
      
    }
    return null;
  }
  
  public double[] computeTextureFactor(Phase aphase, double[][] alphabeta,
                                       Reflection reflex) {

//    int numberOfPoints = alphabeta.length / 2;
    
    initializeAll();
    
    if (odf == null)
      return super.computeTextureFactor(aphase, alphabeta, reflex);
    
    double[] cdsc = aphase.lattice();
    
    double phoninp = Uwimvuo.subfmin(odf, Uwimvuo.getAlphamax(getResolutionD()));
    
    double[] sctf = Uwimvuo.tfhkl(reflex.getH(), reflex.getK(), reflex.getL(), cdsc[7], cdsc[5], cdsc[3], cdsc[6], cdsc[0], cdsc[1]);
    double fhir = Math.acos(sctf[3]);
    int inv = Uwimvuo.equiv(getLaueGroupNumber(), sctf);
    
    double[] textureValues = computeTextureFactor(odf, cdsc, alphabeta, sctf, fhir,
        inv, phoninp, getResolutionD());
    return textureValues;
  }
  
  public double getODF(double als, double bets, double gams) {
    while (bets >= Constants.PI2)
      bets -= Constants.PI2;
    while (bets < 0)
      bets += Constants.PI2;
    if (bets >= Constants.PI) {
      bets = Constants.PI2 - bets;
      als += Constants.PI;
      gams += Constants.PI;
    }
    while (als >= Constants.PI2)
      als -= Constants.PI2;
    while (als < 0)
      als += Constants.PI2;
    while (gams >= Constants.PI2) {
      gams -= Constants.PI2;
    }
    while (gams < 0) {
      gams += Constants.PI2;
    }
    int nal = (int) ((als + pi25g) / resolution + .000001);
    int nb = (int) ((bets + pi25g) / resolution + .000001);
    int nga = (int) ((gams + pi25g) / resolution + .000001);
//        System.out.println(nal + " " + nb + " " + nga);
    return odf[nal][nb][nga];
  }
  
  public double[][] getPoleFigureGrid(Reflection refl, int numberofPoints, double maxAngle) {
    
    int h = refl.getH();
    int k = refl.getK();
    int l = refl.getL();
    
    Phase aphase = (Phase) getParent();
    
    initializeAll();
    
    double[][] PFreconstructed = new double[numberofPoints][numberofPoints];
    
    if (odf == null)
      return null;
    
    double[] cdsc = aphase.lattice();
    
    double phoninp = Uwimvuo.subfmin(odf, Uwimvuo.getAlphamax(getResolutionD()));
    
    //   int hkln = aphase.gethklNumber();
    
    double[] sctf = Uwimvuo.tfhkl(h, k, l, cdsc[7], cdsc[5], cdsc[3], cdsc[6], cdsc[0], cdsc[1]);
    double fhir = Math.acos(sctf[3]);
    int inv = Uwimvuo.equiv(getLaueGroupNumber(), sctf);
    
    double texture_angles[] = new double[2];
    
    double x, y, r;
    double dxy = 2.0 * maxAngle / numberofPoints;
    
    for (int i = 0; i < numberofPoints; i++)
      for (int j = 0; j < numberofPoints; j++) {
        x = (j + 0.5) * dxy - maxAngle;
        y = (i + 0.5) * dxy - maxAngle;
        r = Math.sqrt(x * x + y * y);
        if (r == 0.0) {
          texture_angles[0] = 0.0f;
          texture_angles[1] = 0.0f;
          PFreconstructed[i][j] = computeTextureFactor(odf, cdsc, texture_angles, sctf, fhir,
              inv, phoninp, getResolutionD());
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
          
          PFreconstructed[i][j] = computeTextureFactor(odf, cdsc, texture_angles, sctf, fhir,
              inv, phoninp, getResolutionD());
        } else
          PFreconstructed[i][j] = Double.NaN;
      }
    return PFreconstructed;
  }
  
  public double[][] getInversePoleFigureGrid(double[] texture_angles,
                                             double maxPhi, int phiPointNumber,
                                             double maxBeta, int betaPointNumber) {
    
    Phase aphase = (Phase) getParent();
    
    initializeAll();
    
    double[] cdsc = aphase.lattice();
    
    double phoninp = Uwimvuo.subfmin(odf, Uwimvuo.getAlphamax(getResolutionD()));
    
    double[][] PFreconstructed = new double[phiPointNumber][betaPointNumber];
    
    double[][] textureAngles = new double[2][1];
    for (int i = 0; i < 2; i++)
      textureAngles[i][0] = (double) (texture_angles[i] * Constants.DEGTOPI);
    double phi, beta;
    double dphi = maxPhi / (phiPointNumber - 1);
    double dbeta = maxBeta / (betaPointNumber - 1);
    double[] sctf = new double[4];
    
    for (int i = 0; i < phiPointNumber; i++)
      for (int j = 0; j < betaPointNumber; j++) {
        beta = j * dbeta;
        phi = i * dphi;
        sctf[0] = Math.sin(phi);
        sctf[1] = Math.cos(phi);
        sctf[2] = Math.sin(beta);
        sctf[3] = Math.cos(beta);
        double fhir = beta;
        int inv = Uwimvuo.equiv(getLaueGroupNumber(), sctf);
        double[] value = computeTextureFactor(odf, cdsc, textureAngles, sctf, fhir, inv, phoninp, getResolutionD());
        PFreconstructed[i][j] = value[0];
/*        PFreconstructed[i][j] = computeTextureFactor(phi, beta,
                  texture_angles[0] * Constants.DEGTOPI,
                  texture_angles[1] * Constants.DEGTOPI); */
      }
    return PFreconstructed;
  }
  
  public double[] getInversePoleFigureGrid(double[] texture_angles, double[][] phibeta) {
    Phase aphase = (Phase) getParent();
    
    initializeAll();
    
    double[] cdsc = aphase.lattice();
    
    double phoninp = Uwimvuo.subfmin(odf, Uwimvuo.getAlphamax(getResolutionD()));
    
    int pointNumber = phibeta[0].length;
    double[] PFreconstructed = new double[pointNumber];
    
    double[][] textureAngles = new double[2][1];
    for (int i = 0; i < 2; i++)
      textureAngles[i][0] = (double) (texture_angles[i] * Constants.DEGTOPI);
    double[] sctf = new double[4];
    
    for (int i = 0; i < pointNumber; i++) {
      sctf[0] = Math.sin(phibeta[0][i]);
      sctf[1] = Math.cos(phibeta[0][i]);
      sctf[2] = Math.sin(phibeta[1][i]);
      sctf[3] = Math.cos(phibeta[1][i]);
      double fhir = phibeta[1][i];
      int inv = Uwimvuo.equiv(getLaueGroupNumber(), sctf);
      double[] value = computeTextureFactor(odf, cdsc, textureAngles, sctf, fhir, inv, phoninp, getResolutionD());
      PFreconstructed[i] = value[0];
    }
    
    return PFreconstructed;
  }
  
  public void loadPFandComputeODF(Frame aframe) {
    String filename = Utility.openFileDialog(aframe, "Open PF file (Beartex format)", FileDialog.LOAD,
        (String) MaudPreferences.getPref(FilePar.analysisPath, Constants.documentsDirectory),
        null, "");
    if (filename != null) {
      fromPF = true;
      Vector expPF = poleFigureInput(filename, getResolutionD());
      numberPoleFiguresPF = (expPF.size() - 1) / 4;
      int[] izoveriv = (int[]) expPF.elementAt(expPF.size() - 1);
      maxizoveri = izoveriv[0];
      hklPF = new int[3][numberPoleFiguresPF][maxizoveri];
      weightSingle = new double[numberPoleFiguresPF][maxizoveri];
      izoveriPF = new int[numberPoleFiguresPF];
      
      double res = getResolutionD();
      int alphamax = Uwimvuo.getAlphamax(res);
      int betamax = (Uwimvuo.getBetamax(res) - 1) / 2 + 1;
      int totmax = alphamax * betamax;
      int totpopls = (alphamax - 1) * betamax; // Popla
/*      for (int i = 0; i < numberPoleFiguresPF; i++) {
        double[] pfInt = (double[]) expPF.elementAt(i * 4 + 3);
        if (pfInt.length > totmax)
          totmax = pfInt.length;
      }*/
      
      double[][] experimentalPF = new double[numberPoleFiguresPF][totmax];
      
      int numberIzoveri = 0;
      for (int i = 0; i < numberPoleFiguresPF; i++) {
        int[][] hkl = (int[][]) expPF.elementAt(i * 4);
        double[] weight = (double[]) expPF.elementAt(i * 4 + 1);
        izoveriPF[i] = weight.length;
        for (int j = 0; j < izoveriPF[i]; j++) {
          weightSingle[i][j] = weight[j];
          numberIzoveri++;
        }
        for (int j = 0; j < 3; j++) {
          for (int k = 0; k < izoveriPF[i]; k++)
            hklPF[j][i][k] = hkl[j][k];
        }
        double[] pfInt = (double[]) expPF.elementAt(i * 4 + 3);
        int index = 0;
        int index1 = 0;
/*        if (totpopls == pfInt.length) {
          for (int j = 0; j < betamax; j++) {
            for (int k = 0; k < alphamax - 1; k++)
              experimentalPF[i][index++] = pfInt[index1++];
            experimentalPF[i][index++] = pfInt[index1 - alphamax + 1];
            if (index1 >= pfInt.length)
              break;
          }
          for (int j = pfInt.length; j < totmax; j++)
            experimentalPF[i][j] = -666.666;
        } else {*/
        for (int j = 0; j < pfInt.length; j++)
          experimentalPF[i][j] = pfInt[j];
        for (int j = pfInt.length; j < totmax; j++)
          experimentalPF[i][j] = -666.666;
        // }
      }
      computeTextureFromPF(numberPoleFiguresPF, numberIzoveri, experimentalPF);
      fromPF = false;
    }
  }
  
  public void computeTextureFromPF(int numberPF, int numberIzoveri, double[][] experimentalPF) {
    
    odf = (new Uwimvuo(this, numberPF, numberIzoveri)).computeODF(experimentalPF);

//    String filename = new String(((FilePar) getPhase().getFilePar()).getDirectory() +
//            getPhase().toXRDcatString() + ".odf");
    // odfnotLoaded = false;
//  	ODFoutputStandard(filename, odf, SpaceGroups.getLGNumberSiegfriedConv(getPhase()), getResolutionD());
  }
  
  int maxizoveri = 1;
  
  public Vector poleFigureInput(String filename, double internalResolution) {
    
    return poleFigureInput(filename);

/*    BufferedReader PFreader = Misc.getReader(filename);
//    double[] expInt = null;
    int[][] hkli = null;
    double[] weight = null;
    Vector expPF = null;
    String token = null;
    int maxizoveri_local = 1;

    if (PFreader != null) {
      try {

        String line = PFreader.readLine();
        int izoveriCheck = 0;
        int izoveri = 1;
        hkli = new int[3][1];
        weight = new double[1];
        weight[0] = 1.0;
        for (int i = 0; i < 4; i++) {
          if (line != null) {
            line = PFreader.readLine();
            if (line != null && !line.equals("")) {
              StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");
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

        expPF = new Vector(0, 1);

        while (line != null) {

          PFreader.readLine();
          line = PFreader.readLine();
          System.out.println("Reading: " + line);
          StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");
          for (int i = 0; i < 3; i++) {
            token = st.nextToken();
            hkli[i][0] = Integer.valueOf(token).intValue();
          }
          double[] thetaAndRes = new double[3];
          for (int i = 0; i < 3; i++) {
            token = st.nextToken();
            thetaAndRes[i] = Double.valueOf(token).doubleValue();
          }

          if (internalResolution != 0.0 && thetaAndRes[2] != internalResolution) {
            System.out.println("Resolution not corresponding: " + token + " , Exiting...");
            PFreader.close();
            return null;
          }
          expPF.addElement(hkli);
          double weightTot = 0.0;
          for (int i = 0; i < izoveri; i++)
            weightTot += weight[i];
          if (weightTot == 0) {
            for (int i = 0; i < izoveri; i++)
              weight[i] = 1.0 / izoveri;
          } else
            for (int i = 0; i < izoveri; i++)
              weight[i] /= weightTot;
          expPF.addElement(weight);
          expPF.addElement(thetaAndRes);

          double res = thetaAndRes[2];

          int alphamax = getAlphamax(res);
          int betamax = (getBetamax(res) - 1) / 2 + 1;

          double[] pfInt = new double[alphamax * betamax];

          int totData = 0;
          do {
            line = PFreader.readLine();
            for (int i = 1; i < 72; i += 4) {
              if (totData < alphamax * betamax) {
                String subData = line.substring(i, i + 4);
//                System.out.println(i + " " + subData);
                double pfvalue = Double.valueOf(Misc.toStringDeleteBlank(subData)).doubleValue() / 100.0;
                double theta = (totData / alphamax) * res;
                if (theta < thetaAndRes[0] || theta > thetaAndRes[1])
                  pfvalue = -1.0;
                pfInt[totData++] = pfvalue;
                if (((totData + 1) / alphamax) * alphamax == totData + 1) {
//                  System.out.println(totData + " " + alphamax + " " + Double.toXRDcatString(pfInt[totData - alphamax + 1]));
                  pfInt[totData] = pfInt[totData - alphamax + 1];
                  totData++;
                }
              }
            }
          } while (totData < alphamax * betamax);
          expPF.addElement(pfInt);
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

        int[] izoveriv = new int[1];
        izoveriv[0] = maxizoveri_local;
        expPF.addElement(izoveriv);

        PFreader.close();
      } catch (IOException io) {
      }
    }

    return expPF;*/
  }
  
  public static void poleFigureOutput(double odfl[][][], Phase thephase, int h, int k, int l, double res) {
    
    double[] cdsc = thephase.lattice();
    double[] sctf = Uwimvuo.tfhkl(h, k, l, cdsc[7], cdsc[5], cdsc[3], cdsc[6], cdsc[0], cdsc[1]);
    double fhir = Math.acos(sctf[3]);
    int inv = Uwimvuo.equiv(SpaceGroups.getLGNumberSiegfriedConv(thephase.getPointGroup()), sctf);
    
    double phoninp = Uwimvuo.subfmin(odfl, Uwimvuo.getAlphamax(res));
    
    BufferedWriter PFwriter = Misc.getWriter(thephase.getFilePar().getDirectory() +
        thephase.toXRDcatString() + ".xpt");
    
    String title = new String(thephase.toXRDcatString() + ": recalculated pole figure, ");
    
    StringBuffer tmp = new StringBuffer(title);
    tmp = tmp.append(" ").append(Integer.toString(h)).
        append(",").append(Integer.toString(k)).
        append(",").append(Integer.toString(l));
    int bufflength = tmp.length();
    for (int i = 0; i < 79 - bufflength; i++)
      tmp = tmp.append(" ");
    
    String commentLine = new String(tmp.toString().substring(0, 79) + "#");
    
    try {
      PFwriter.write(commentLine);
      for (int i = 0; i < 5; i++)
        PFwriter.write(Constants.lineSeparator);
      
      PFwriter.write(Misc.getFirstPFline(thephase));
      PFwriter.write(Constants.lineSeparator);
      
      String firstline = new String(" " + Misc.getIntStringFormatted(h, 3) +
          Misc.getIntStringFormatted(k, 3) +
          Misc.getIntStringFormatted(l, 3) +
          "   .0 90.0" + Misc.getDoubleStringFormatted(res, 3, 1) +
          "   .0360.0" + Misc.getDoubleStringFormatted(res, 3, 1) +
          " 1 1");
      PFwriter.write(firstline);
      PFwriter.write(Constants.lineSeparator);
    } catch (IOException io) {
    }
    
    int until18 = 0;
    double[] texture_angles = new double[2];
    int alphamax = Uwimvuo.getAlphamax(res) - 1;
    int betamax = (Uwimvuo.getBetamax(res) - 1) / 2 + 1;
    
    for (int nty = 0; nty < betamax; ++nty) {
      for (int nfy = 0; nfy < alphamax; ++nfy) {
        texture_angles[0] = (double) (nty * res * Constants.DEGTOPI);
        texture_angles[1] = (double) (nfy * res * Constants.DEGTOPI);
        double textF = computeTextureFactor(odfl, cdsc, texture_angles, sctf, fhir, inv,
            phoninp, res);
        int imh = (int) (textF * 100.000001);
        
        try {
          if (until18 == 0)
            PFwriter.write(" ");
          PFwriter.write(Misc.getIntStringFormatted(imh, 4));
          if (++until18 >= 18) {
            until18 = 0;
            PFwriter.write(Constants.lineSeparator);
          }
        } catch (IOException io) {
        }
      }
    }
    if (until18 != 0)
      try {
        PFwriter.write(Constants.lineSeparator);
      } catch (IOException io) {
      }
    try {
      PFwriter.write(Constants.lineSeparator);
    } catch (IOException io) {
    }
    
    try {
      PFwriter.flush();
      PFwriter.close();
    } catch (IOException io) {
    }
  }
  
  public void fiottu() {
    int igbl = SpaceGroups.getLGNumberSiegfriedConv(getPhase().getPointGroup());
    Uwimvuo.fiottu(odf, igbl, alphama);
  }
  
  public static void ODFoutputStandardOld(String filename, double[][][] odfl, int igbl, double res) {
    
    int alphama = Uwimvuo.getAlphamax(res);
    
    int betama = (alphama - 1) / 2 + 1;
    int betamad2 = (betama - 1) / 2 + 1;
    int alphamad3 = (alphama - 1) / 3 + 1;
    int alphamad6 = (alphama - 1) / 6 + 1;
    
    int nyzgl[][] = {{betama, betama, betamad2, betama, betamad2, betamad2, betamad2, betama, betamad2, betama, betamad2},
        {alphama, betama, betama, betamad2, betamad2, betama, betamad2, alphamad3, alphamad3, alphamad6, alphamad6}};
    
    int nae = nyzgl[1][0];
    int nbe = nyzgl[0][igbl - 1];
    int nge = nyzgl[1][igbl - 1];
    
    BufferedWriter PFwriter = Misc.getWriter(filename);
    if (PFwriter != null) {
      try {
        
        
        PFwriter.write(Integer.toString(igbl) + " " + Fmt.format(res));
        PFwriter.write(Constants.lineSeparator);
// System.out.println(nae + " " + nbe + " " + nge);
        for (int ng = 0; ng < nge; ng++) {
          for (int nb = 0; nb < nbe; nb++) {
            for (int na = 0; na < nae; na++) {
// System.out.println(na + " " + nb + " " + ng);
              float value = (float) odfl[na][nb][ng];
              PFwriter.write(Float.toString(value) + " ");
//    					PFwriter.flush();
            }
            PFwriter.write(Constants.lineSeparator);
          }
          PFwriter.write(Constants.lineSeparator);
        }
        PFwriter.flush();
        PFwriter.close();
      } catch (IOException io) {
        try {
          PFwriter.flush();
          PFwriter.close();
        } catch (IOException ieo) {
        }
      }
    }
  }

/*  public void ODFoutputStandard(String filename) {

    fiottu();
    Phase aphase = getPhase();
    int[] ne = getCellNumber(LaueGroupSnumber, 5.0);

    BufferedWriter PFwriter = Misc.getWriter(filename);
    if (PFwriter != null) {
      try {

        PFwriter.write(getFilePar().getTitleField() + " - " + aphase.toXRDcatString());
        PFwriter.write("\r");
        PFwriter.write(Integer.toXRDcatString(LaueGroupSnumber) + " " + "5.0");
        PFwriter.write("\r");
        PFwriter.write( aphase.getCella().getValue()+ " " +
                        aphase.getCellb().getValue()+ " " +
                        aphase.getCellc().getValue()+ " " +
                        aphase.getCellalpha().getValue()+ " " +
                        aphase.getCellbeta().getValue()+ " " +
                        aphase.getCellgamma().getValue()+ " "
                       );
        PFwriter.write("\r");
        for (int ng = 0; ng < ne[2]; ng++) {
          double gamma = ng * 5.0;
          if (ng == 0)
            gamma += 1.25;
          if (ng == ne[2] - 1)
            gamma -= 1.25;
          for (int nb = 0; nb < ne[1]; nb++) {
            double beta = nb * 5.0;
            if (nb == 0)
              beta += 1.25;
            if (nb == ne[1] - 1)
              beta -= 1.25;
            for (int na = 0; na < ne[0]; na++) {
              double alpha = na * 5.0;
              if (na == 0)
                alpha += 1.25;
              if (na == ne[0] - 1)
                alpha -= 1.25;
              double value = (double) getODF(alpha * Constants.DEGTOPI, beta * Constants.DEGTOPI, gamma * Constants.DEGTOPI);
              PFwriter.write(Float.toXRDcatString(value) + " ");
//							PFwriter.write(Fmt.format(odf[na][nb][ng]));
//    					PFwriter.flush();
            }
            PFwriter.write("\r");
          }
          PFwriter.write("\r");
        }
        PFwriter.flush();
        PFwriter.close();
      } catch (IOException io) {
        try {
          PFwriter.flush();
          PFwriter.close();
        } catch (IOException ieo) {
        }
      }
    }
  }*/

/*  public static double[][][] ODFinputStandard(String filename, int igbl, double res) {

    double[][][] odfl;

    int alphama = getAlphamax(res);

    int betama = (alphama - 1) / 2 + 1;
    int betamad2 = (betama - 1) / 2 + 1;
    int alphamad3 = (alphama - 1) / 3 + 1;
    int alphamad6 = (alphama - 1) / 6 + 1;

    odfl = new double[alphama][betama][alphama];

    int nyzgl[][] = {{betama, betama, betamad2, betama, betamad2, betamad2, betamad2, betama, betamad2, betama, betamad2},
                     {alphama, betama, betama, betamad2, betamad2, betama, betamad2, alphamad3, alphamad3, alphamad6, alphamad6}};

    int nae = nyzgl[1][0];
    int nbe = nyzgl[0][igbl - 1];
    int nge = nyzgl[1][igbl - 1];

    BufferedReader PFreader = Misc.getReader(filename);

    if (PFreader != null) {
      try {

        String line = PFreader.readLine();
        StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");
        String token = st.nextToken();
        int igbr = Integer.valueOf(token).intValue();
        token = st.nextToken();
        double resr = Double.valueOf(token).doubleValue();
        if (st.hasMoreTokens()) {
          token = st.nextToken();
        }

        if (igbr == igbl && resr == res) {
          line = PFreader.readLine();
          st = new StringTokenizer(line, " ,\t\r\n");
          for (int ng = 0; ng < nge; ng++)
            for (int nb = 0; nb < nbe; nb++)
              for (int na = 0; na < nae; na++) {
                if (!st.hasMoreTokens()) {
                  do {
                    line = PFreader.readLine();
                    st = new StringTokenizer(line, " ,\t\r\n");
                  } while (!st.hasMoreTokens());
                }
                odfl[na][nb][ng] = Double.valueOf(st.nextToken()).doubleValue();
              }

        } else {

          System.out.println("Crystal symmetry or resolution not corresponding!");

          for (int ng = 0; ng < nge; ng++)
            for (int nb = 0; nb < nbe; nb++)
              for (int na = 0; na < nae; na++)
                odfl[na][nb][ng] = 1.0;
        }
        PFreader.close();
        Uwimvuo.fiottu(odfl, igbl, alphama);
      } catch (IOException io) {

        for (int ng = 0; ng < alphama; ng++)
          for (int nb = 0; nb < betama; nb++)
            for (int na = 0; na < alphama; na++)
              odfl[na][nb][ng] = 1.0;
      }
    } else {
      for (int ng = 0; ng < alphama; ng++)
        for (int nb = 0; nb < betama; nb++)
          for (int na = 0; na < alphama; na++)
            odfl[na][nb][ng] = 1.0;
    }

    return odfl;
  }*/
  
  public static void ODFoutputBeartex(String filename, double[][][] odfl, int igbl, double res) {
    
    int alphama = Uwimvuo.getAlphamax(res);
    
    int betama = (alphama - 1) / 2 + 1;
    int betamad2 = (betama - 1) / 2 + 1;
    int alphamad3 = (alphama - 1) / 3 + 1;
    int alphamad6 = (alphama - 1) / 6 + 1;
    
    int nyzgl[][] = {{betama, betama, betamad2, betama, betamad2, betamad2, betamad2, betama, betamad2, betama, betamad2},
        {alphama, betama, betama, betamad2, betamad2, betama, betamad2, alphamad3, alphamad3, alphamad6, alphamad6}};
    
    int nae = nyzgl[1][0];
    int nbe = nyzgl[0][igbl - 1];
    int nge = nyzgl[1][igbl - 1];
    
    BufferedWriter PFwriter = Misc.getWriter(filename);
    if (PFwriter != null) {
      try {
        
        
        PFwriter.write(Integer.toString(igbl) + " " + Fmt.format(res));
        PFwriter.write(Constants.lineSeparator);
        
        for (int ng = 0; ng < nge; ng++) {
          for (int nb = 0; nb < nbe; nb++) {
            for (int na = 0; na < nae; na++) {
              PFwriter.write(Fmt.format(odfl[na][nb][ng]) + " ");
            }
            PFwriter.write(Constants.lineSeparator);
            PFwriter.write(Constants.lineSeparator);
          }
          PFwriter.write(Constants.lineSeparator);
          PFwriter.write(Constants.lineSeparator);
          PFwriter.write(Constants.lineSeparator);
        }
        PFwriter.flush();
        PFwriter.close();
      } catch (IOException io) {
      }
    }

/*
C
      SUBROUTINE STDFILE(IGB)
C                                   simplified variant : IMEM=1  fixed
C
C    INPUT : ODF given in the full G-space -->  ARRAY FIO(197173) ;
C    5 degree ALPHA,BETA,GAMMA structure.
C    CONSTRUCTION of the section ARRAY FS(73,37) according to the
C    WIMV Standard Listing Format. Gamma sections.
C    GBW(GB)/GA=C1 elementary G-space region.
C    CALL of the section PRINTROUTINE "PRIU" : Print of a Gamma-section.
C
C    OUTPUT : Standard file (F88.DAT unit 88,  smoothed ODF for IMEM=1)
C                           (F99.DAT unit 99, sharpened ODF for IMEM=2)
      DIMENSION NYZG(11,2),MD(11)
      CHARACTER ITITLE*40
      CHARACTER*5 NAMET,NAMEFI,NAMESE
      COMMON /CMIMA/ FMIN,FMAX
      COMMON /CPRIU/NSECA,NSECE,SECWIST,
     *             NTETAA,NTETAE,NFIA,NFIE,NAMESE,NAMET,NAMEFI
c     COMMON /CFIO/FIO(197173)
      COMMON /CF/FIO(197173)
      COMMON /CFH/ FS(73,37)
      COMMON /CIFIW/IFIW(73)
      COMMON /CTITLE/ITITLE
      DATA NYZG/37,37,19,37,19,19,19,37,19,37,19,
     *          73,37,37,19,19,37,19,25,25,13,13/
      DATA MD/0,0,1,0,1,1,1,0,1,0,1/
C
c     Special case of the elementary region for GBW=Dn and GA=D3 :
C     (ALPHA : 30-90 degrees).
C
        IGA=1
        IMEM=1

        NAEC=73
        NBE=NYZG(IGB,1)
        NGE=NYZG(IGB,2)
        NAEE=NAEC
        NAA=1
C
        SECWIST=5.
        NSECA=1
        NSECE=NGE
        NTETAA=1
        NTETAE=NBE
        NFIA=NAA
        NFIE=NAEE
C
        FMIN = 8888.
        FMAX = 0.
C
       NYE=73*37
       iupl=0
       if(imem.eq.1) iulst=88
       if(imem.eq.2) iulst=99
       if(imem.eq.1)write(88,8009)ITITLE
       if(imem.eq.2)write(99,8010)ITITLE
c 8009  format(1x,'ODF (SMOOTHED) ',A40)
 8009  format(1x,'ODF (called) ',A40)
 8010  format(1x,'ODF (SHARPENED) ',A40)
C
C GAMMA-SECTION OUTPUT
C
      NAMET = 'BETA '
      NAMEFI = 'ALPHA'
      NAMESE = 'GAMMA'
      ityp=3
      DO  1 IGAM = 1,NGE
      IA = NYE*(IGAM-1)
      GAM = IFIW(IGAM)
      IF (imem.eq.1) WRITE (88,190) GAM
      IF (imem.eq.2) WRITE (99,200) GAM
  190 FORMAT(   //5X,'GAMMA = ',F5.1,
     1 2X,'called ODF'/)
c    1 2X,'SMOOTHED ODF'/)
  200 FORMAT(   //5X,'GAMMA = ',F5.1,
     1 2X,'SHARPENED ODF '/)
      DO 2 IBET = 1,NBE
      IB = 73*(IBET-1)+IA
C Standard region
      DO 2 IALFA = NAA,NAEE
      NG = IB+IALFA

      FS(IALFA,IBET) =FIO(NG)

    2 continue
             CALL PRIU(ityp,IGAM,iulst,iupl)
    1 CONTINUE
C
      RETURN
      END
C
C    *--------------------------------*
C
      SUBROUTINE PRIU(ityp,NSEC,iulst,iupl)
C
C    OUTPUT OF THE NSEC-TH ITYP section (ARRAY FS)
C  - ON UNIT iulst - LISTING
C  - ON UNIT iupl - PLOTFILE
C    Listing : Azimuth  :   NAA,NAEE  as desired
C    Plot    :              1,NAEP
C
C    NAMET  - NAME OF THETA ANGLE   : 'BETA','FI'
C    NAMEFI - NAME OF FI ANGLE      > 'ALPHA','GAMMA','FI1','FI2','PHI'
C    NAMESE - NAME OF SECTION ANGLE > 'GAMMA','ALPHA','FI2','FI1','SIGMA',
C                                     'OMEGA'
C
      DIMENSION IMH(1368)
      CHARACTER ITITLE*40
      CHARACTER*5 NAMET,NAMEFI,NAMESE
      CHARACTER*4 MIN,MAX
      CHARACTER ISTR(73)*4, ISTR2(73)*2
      COMMON /CFH/ FS(73,37)
      COMMON /CIFIW/IFIW(73)
      COMMON /CMIMA/ FMIN,FMAX
      COMMON /CPRIU/NSECA,NSECE,SECWIST,
     *             NTETAA,NTETAE,NFIA,NFIE,NAMESE,NAMET,NAMEFI
      COMMON /TIT/ITITLE
      DATA MIN /' MIN'/
      DATA MAX /' MAX'/
      DATA ISTR /73*'----'/,ISTR2/73*'--'/
      IUNIT=IULST
      FSMAX = 0.
      FSMIN = 8888.0
      istep=1
      NAA=NFIA
      NAEE=NFIE
      NAEP=NAEE
C     NBA=NTETAA
      NBE=NTETAE
      NGE=NSECE
      DO 15 NA = NAA,NAEE
      DO 15 NB = 1,NBE
      FMM = FS(NA,NB)
      IF (FMM.LE.FSMAX) GOTO 14
      FSMAX = FMM
      SAMAX = IFIW(NA)
      SBMAX = IFIW(NB)
   14 IF (FMM.GE.FSMIN) GOTO 15
      FSMIN = FMM
      SAMIN = IFIW(NA)
      SBMIN = IFIW(NB)
   15 CONTINUE
C
      WRITE (IUNIT,44) MIN,FSMIN,NAMET,SBMIN,NAMEFI,SAMIN
      WRITE (IUNIT,44) MAX,FSMAX,NAMET,SBMAX,NAMEFI,SAMAX
      IFE = NAA
      NAEEE=NAEE
   16 IFA = IFE
      IFE = IFA+18
      IF (IFE.GT.NAEEE) IFE = NAEEE
      WRITE(IUNIT,42) NAMEFI,(IFIW(IFI),IFI = IFA,IFE)
      WRITE(IUNIT,43) NAMET,(ISTR(IS),ISTR2(IS),IS=IFA,IFE)
      DO 77 NB=1,NBE
      WRITE (IUNIT,35) IFIW(NB),(FS(NA,NB),NA = IFA,IFE)
   77 CONTINUE
   36 IF (IFE.LT.NAEEE) GOTO 16
      IF (FSMAX.LT.FMAX) GOTO 17
      FMAX = FSMAX
      AMAX = SAMAX
      BMAX = SBMAX
      SIMAX = IFIW(NSEC)
      IF(SECWIST.EQ.2.5)SIMAX=SIMAX/2.
   17 IF (FSMIN.GT.FMIN) GOTO 1
      FMIN = FSMIN
      AMIN = SAMIN
      BMIN = SBMIN
      SIMIN = IFIW(NSEC)
      IF(SECWIST.EQ.2.5)SIMIN=SIMIN/2.
    1 CONTINUE
      if(nsec.ne.nge) goto 200
      write (iunit,45) min,fmin,namefi,amin,namet,bmin,namese,simin
      write (iunit,45) max,fmax,namefi,amax,namet,bmax,namese,simax
C
C     CONSTRUCTION OF THE PLOTFILE (BERKELEY-FORMAT)
C
 200  IF(IUPL.EQ.0)GOTO 100
      NAE=NAEP
      DELT = 5.
      DELF = 5.
      TMAX = 90.
      FIMAX = 5.*(NAE-1)
      hormfak=0.
C     HORMFAK = 999./FMAXM
      IF (HORMFAK.EQ.0.)HORMFAK=1.
      normfak=HORMFAK
      nnormfak=1
      nnormfak=0
c     nnormfak=10
      IW =1
      ANGLE = 5.*(NSEC-1)
      IF(SECWIST.EQ.2.5)ANGLE=ANGLE/2.
      NAEH=NAEP
      IF (NAEP.EQ.73)NAEH=72
      NYZ = NAEH*19
      WRITE(IUPL,40)ITITLE,NAMESE
      WRITE(IUPL,50)DELT,TMAX,DELF,FIMAX,IW,IW,IW,IW,IW,nnormfak,ANGLE
      DO 60 NB=1,19
      NBFAK = (NB-1)*NAEH
      DO 60 NA=1,NAEH
      NY = NBFAK+NA
C                                                       !!!!!!!!!!!!!
C                                                       !!!!!!!!!!!!!
C                                                       !!!!!!!!!!!!!
C                                                       !!!!!!!!!!!!!
      IMH(NY)=NINT(FS(NA,NB)*HORMFAK*10)
c      IMH(NY)=NINT(FS(NA,NB)*HORMFAK*100)
  60  CONTINUE
      IF (NAEH.EQ.72)WRITE(IUPL,70)(IMH(NY),NY=1,NYZ)
      IF (NAEH.EQ.37)WRITE(IUPL,71)(IMH(NY),NY=1,NYZ)
      IF (NAEH.EQ.25)WRITE(IUPL,72)(IMH(NY),NY=1,NYZ)
      IF (NAEH.EQ.19)WRITE(IUPL,73)(IMH(NY),NY=1,NYZ)
      IF (NAEH.EQ.13)WRITE(IUPL,74)(IMH(NY),NY=1,NYZ)
      IF (NAEH.EQ.10)WRITE(IUPL,75)(IMH(NY),NY=1,NYZ)
      IF(NAEH.NE.72.AND.
     *   NAEH.NE.37.AND.
     *   NAEH.NE.25.AND.
     *   NAEH.NE.19.AND.
     *   NAEH.NE.13.AND.
     *   NAEH.NE.10)WRITE(IUPL,76)
      WRITE(IUPL,*)
      IF (NBE.EQ.19)GOTO 100
C
C     LOWER HALFSPHERE
C
      WRITE(IUPL,40)ITITLE,NAMESE
      WRITE(IUPL,90)DELT,TMAX,DELF,FIMAX,IW,IW,IW,IW,IW,nnormfak,ANGLE
      DO 160 NB=19,37
      NBH = 38-NB
      NBFAK = (NBH-1)*NAEH
      DO 160 NA=1,NAEH
      NY = NBFAK+NA
      IMH(NY)=NINT(FS(NA,NB)*HORMFAK*100)
  160 CONTINUE
      IF (NAEH.EQ.72)WRITE(IUPL,70)(IMH(NY),NY=1,NYZ)
      IF (NAEH.EQ.37)WRITE(IUPL,71)(IMH(NY),NY=1,NYZ)
      IF (NAEH.EQ.25)WRITE(IUPL,72)(IMH(NY),NY=1,NYZ)
      IF (NAEH.EQ.19)WRITE(IUPL,73)(IMH(NY),NY=1,NYZ)
      IF (NAEH.EQ.13)WRITE(IUPL,74)(IMH(NY),NY=1,NYZ)
      IF (NAEH.EQ.10)WRITE(IUPL,75)(IMH(NY),NY=1,NYZ)
      IF(NAEH.NE.72.AND.
     *   NAEH.NE.37.AND.
     *   NAEH.NE.25.AND.
     *   NAEH.NE.19.AND.
     *   NAEH.NE.13.AND.
     *   NAEH.NE.10)WRITE(IUPL,76)
      WRITE(IUPL,*)
  100 CONTINUE
      RETURN
C
C     CONSTRUCTION OF THE PLOTFILE (METZ-FORMAT)
c 200 CONTINUE
      GOTO 80
      NYZA=NAEP
      NYZB=NBE
      NYZG=NGE
      IU=IUPL
      if (ityp.eq.1) icas=2
      if (ityp.eq.2) icas=1
      if (ityp.eq.3) icas=2
      if (ityp.eq.4) icas=1
      jtypdt=1
      jpair=3
      lmax=88
      lfmax=4
      dfi1=5.
      dfi=5.
      dfi2=5.
      if(icas.eq.1)dfi1=5.*istep
      if(icas.eq.2)dfi2=5.*istep
      fi1x=5.*(nyza-1)
      fix=5.*(nyzb-1)
      fi2x=5.*(nyzg-1)
       if(nsec.gt.1) goto 112
       if (iu.eq.11)write(iu,8009)
       if (iu.eq.12)write(iu,8010)
       write(iu,8005)ititle
       write(iu,8006)lmax,lfmax
       write(iu,8004)jtypdt,jpair,icas
       write(iu,8003)dfi1,dfi,dfi2,fi1x,fix,fi2x
       do 111 ip=1,lfmax
  111  write(iu,8002)
  112 IFE = 1
  116 IFA = IFE
      IFE = IFA+18
      IF (IFE.GT.NAE) IFE = NAE
      DO 177 NB=1,NBE
      WRITE (iu,8017) (FS(NA,NB),NA = IFA,IFE)
 177  CONTINUE
      IF (IFE.LT.NAE) GOTO 116
      if(nsec.eq.nge) write(iu,8001)fmax,fmin
 80     RETURN
 8001 format(5x,f8.3,4x,f8.3)
 8002 format(' ')
 8003 format(1x,'DFI1=',f4.0,'DFI =',f4.0,'DFI2=',f4.0,
     1       'FI1X=',f4.0,'FIX =',f4.0,'FI2X=',f4.0)
 8004 format(1x,'JTYPDT=',i1,1x,'JPAIR=',i1,1x,'ICAS=',i1)
 8005 format(a40)
 8006 format(1x,'LMAX=',i2,'LFMAX=',i3)
 8009 format(1x,'ODF (SMOOTHED) ')
 8010 format(1x,'ODF (SHARPENED) ')
 8017 format(10f10.5)
   35 FORMAT(1X,I3,'|',19F6.2)
   40 FORMAT(A40,1X,A5,'sections')
   42 FORMAT(//1X,A5,'>',I4,18I6)
   43 FORMAT(1X,A5,19(A4,A2))
   44 FORMAT(8X,A4,'IMUM = ',F9.3,5X,
     * A5,' =',F4.0,5X,A5,' = ',F4.0)
   45 FORMAT(/2X,'ODF ',A4,' = ',F9.3,5X,A5,' = ',F4.0,3X,
     * A5,' = ',F4.0,3X,A5,' = ',F5.1)
   50 FORMAT(1X,'O  ',1X,4F5.0,5I2,I5,10X,F5.1)
   70 FORMAT(1X,18I4)
   71 FORMAT(1X,18I4,/,1X,19I4)
   72 FORMAT(1X,18I4,/,1X, 7I4)
   73 FORMAT(1X,19I4)
   74 FORMAT(1X,13I4)
   75 FORMAT(1X,10I4)
   76 FORMAT('  "PHI"-region out of BERKELEY PLOT FORMAT variants',/,
     *       ', see SUBROUTINE PRIU labels 60,160.')
   90 FORMAT(1X,'U  ',1X,4F5.0,5I2,I5,10X,F5.1)
      END
C
C
C  *****************************************
*/
  }
  
  public static double[] computeTextureFactor(double[][][] odfl, ReflectionTexture reflectionTexture,
                                              double phoninp, double res) {
    
    int numberOfPoint = reflectionTexture.getPointsNumber();
    double[] pfValue = new double[numberOfPoint];
    for (int i = 0; i < numberOfPoint; i++)
      pfValue[i] = 1.0;
    
    
    if (odfl != null) {
      for (int i = 0; i < numberOfPoint; i++) {
        double[] texture_angles = reflectionTexture.getAngles(i);
        pfValue[i] = calculatePF(odfl, phoninp, texture_angles[0], texture_angles[1],
            reflectionTexture.sctf[0], reflectionTexture.sctf[1], reflectionTexture.fhir,
            reflectionTexture.inv, res);
      }
    }
    return pfValue;
  }
  
  public static double[] computeTextureFactor(double[][][] odfl, double[] cdsc, double texture_angles[][],
                                              double[] sctf, double fhir, int inv, double phoninp, double res) {
    
    int numberOfPoint = texture_angles[0].length;
    double[] pfValue = new double[numberOfPoint];
    for (int i = 0; i < numberOfPoint; i++)
      pfValue[i] = 1.0;
    
    if (odfl != null) {
      for (int i = 0; i < numberOfPoint; i++)
        pfValue[i] = calculatePF(odfl, phoninp, texture_angles[0][i], texture_angles[1][i],
            sctf[0], sctf[1], fhir, inv, res);
    }
    return pfValue;
  }
  
  public static double computeTextureFactor(double[][][] odfl, double[] cdsc, double texture_angles[],
                                            double[] sctf, double fhir, int inv, double phoninp, double res) {
    
    double pfValue = 1.0;
    
    if (odfl != null) {
      pfValue = calculatePF(odfl, phoninp, texture_angles[0], texture_angles[1],
          sctf[0], sctf[1], fhir, inv, res);
    }
    return pfValue;
  }
  
  public static final double calculatePF(double[][][] f, double phoninp, double theta, double phi,
                                         double sthi, double cthi,
                                         double fhir, int inv, double resolution) {
    /* Local variables */
    double ffak;//, pfak, gams, bets;
    int nfis;//, nfiy, ntfs;
//    int nals1, i;
//    double s;
//    int nbgam;
//    double pinpf;
    int /*ntety, ivorz,*/ nb;
//    double sn;
//    int ny;
    double ca2, cb2, sa2, g2r;//, gam;
    int nga, nal;
//    double als;
//    int iswitch;
    
    boolean negODFout = false;
    if (Constants.testing)
      negODFout = MaudPreferences.getBoolean("debug.negativeODFout", false);

//    int alphamax = (int) (360.0 / resolution + 1.00001);
//    int betamax = alphamax / 2 + 1;
//    int old2701max = alphamax * betamax;
    
    double pi5g = resolution * Constants.DEGTOPI;
    double pi25g = pi5g / 2.;
//    double pi75g = pi25g * 3.;
    
    /*     Calculation of a complete reduced pole figure */
    /*     Normalization */
    /*     INPUT FIO given in the whole G-space OUTPUT POLREF=FS */
    
    double phonstep = phoninp + phonstepeps;
    
    double fs = 0.;
    
    /* Projection thread loop, Simpson integration */
    
    cb2 = cthi;
    g2r = Constants.PI - fhir;
    boolean checkL13 = false;
    boolean nextCheck = false;
    double cr = Math.cos(theta);
    double sr = Math.sin(theta);
    
    int nfismax = (int) (Constants.PI2 / Constants.integrationStepPFR + 1.000001);
    do {
      while (g2r < 0.) {
        g2r += Constants.PI2;
      }
      for (nfis = 0; nfis < nfismax; nfis++) {
        double ang = nfis * Constants.integrationStepPFR;
        ca2 = -Math.cos(ang);
        sa2 = Math.sin(ang);
//				for (ntety = 1; ntety <= 19; ++ntety) {
        double[] angles = Uwimvuo.g20g100(ca2, sa2, cb2, sthi, cr, sr);
        angles[0] += phi;
        angles[2] += g2r;
        while (angles[1] >= Constants.PI2)
          angles[1] -= Constants.PI2;
        while (angles[1] < 0)
          angles[1] += Constants.PI2;
        if (angles[1] >= Constants.PI) {
          angles[1] = Constants.PI2 - angles[1];
          angles[0] += Constants.PI;
          angles[2] += Constants.PI;
        }
        while (angles[0] >= Constants.PI2)
          angles[0] -= Constants.PI2;
        while (angles[0] < 0)
          angles[0] += Constants.PI2;
        while (angles[2] >= Constants.PI2) {
          angles[2] -= Constants.PI2;
        }
        while (angles[2] < 0) {
          angles[2] += Constants.PI2;
        }
        nal = (int) ((angles[0] + pi25g) / pi5g + .000001);
        nb = (int) ((angles[1] + pi25g) / pi5g + .000001);
        nga = (int) ((angles[2] + pi25g) / pi5g + .000001);
        ffak = f[nal][nb][nga];
        if (ffak < 0.0) {
          ffak -= ffak;
          if (negODFout)
            System.out.println("Negative odf: " + nal + " " + nb + " " + nga);
        }
        if (ffak > phonstep) {
          ffak = (ffak - phoninp) * Constants.pisimg;
//				ffak *= pisimg;
          if (!(nfis == 0 || nfis == nfismax - 1)) {
            if (MoreMath.powint(-1, nfis + 1) < 0)
              ffak *= 2.;
            else
              ffak *= 4.;
          }
          fs += ffak;
        }
      }
      fs += phoninp;
      if (inv == 1 || nextCheck) {
        checkL13 = true;
      } else {
        nextCheck = true;
        cb2 = -cb2;
        g2r -= Constants.PI;
      }
    } while (!checkL13); /*goto L13; */
    
    if (inv != 1) {
      fs /= 2.;
    }
    /*                                          Normalization to PINPF */
//		System.out.println(fs);
    return fs;
  } /* calpolo_ */
  
  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JWTextureOptionsD(parent, this);
    return adialog;
  }
  
  class JWTextureOptionsD extends JOptionsDialog {
    
    JComboBox symmetryCB;
    JComboBox wimvenabledCB;
    JComboBox interpolationCB;
    JTextField minIntTF;
    JTextField resTF;
    JCheckBox statusCB;
    JCheckBox refinableCB;
    JLabel sharpL = null;
    
    public JWTextureOptionsD(Frame parent, XRDcat obj) {
      
      super(parent, obj);
      
      principalPanel.setLayout(new GridLayout(0, 2, 3, 3));
      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(jPanel8);
      jPanel8.add(new JLabel("Generate symmetry: "));
      symmetryCB = new JComboBox();
      for (int i = 0; i < symmetrychoicenumber; i++)
        symmetryCB.addItem(symmetrychoice[i]);
      symmetryCB.setToolTipText("Set up unmeasured sample symmetries");
      jPanel8.add(symmetryCB);
      jPanel8 = new JPanel();
      jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(jPanel8);
      jPanel8.add(new JLabel("Minimum reflection intensity: "));
      minIntTF = new JTextField(Constants.FLOAT_FIELD);
      minIntTF.setToolTipText("Minimum value of intensity for a reflection to be included (respect to max)");
      jPanel8.add(minIntTF);
      jPanel8 = new JPanel();
      jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(jPanel8);
      jPanel8.add(new JLabel("ODF resolution (deg): "));
      resTF = new JTextField(Constants.FLOAT_FIELD);
      resTF.setToolTipText("Set the cell dimension in degrees for the ODF (standard=5.0)");
      jPanel8.add(resTF);
      
      JPanel jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(jPanel9);
      statusCB = new JCheckBox("Real coincidences unique PF");
      statusCB.setToolTipText("Check the box to let WIMV use real coincidence superposed as a unique pole figure");
      jPanel9.add(statusCB);
      
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(jPanel9);
      refinableCB = new JCheckBox("ODF refinable");
      refinableCB.setToolTipText("Uncheck this box if the ODF should not be modify");
      jPanel9.add(refinableCB);
      
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(jPanel9);
      JButton jb1 = new JButton("Reset ODF");
      jb1.setToolTipText("Press this to reset the ODF to a random one");
      jPanel9.add(jb1);
      jb1.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          resetODF();
        }
      });
      
      jPanel9 = new JPanel();
      jPanel9.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(jPanel9);
      jPanel9.add(new JLabel("Interpolation method: "));
      interpolationCB = new JComboBox();
      for (int i = 0; i < getsubordClassNumber(getInterpolationID()); i++) {
        interpolationCB.addItem(getsubordIdentifier(getInterpolationID(), i));
      }
      interpolationCB.setToolTipText("Interpolation method for PF before WIMV");
      jPanel9.add(interpolationCB);
      JButton jb = new JButton("Interpolation options");
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          interpolationOptions();
        }
      });
      jPanel9.add(jb);
      JPanel jPanel10 = new JPanel();
      jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(jPanel10);
      jPanel10.add(jb = new JButton("Advanced options"));
      jb.setToolTipText("General options for WIMV algorithm");
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          advancedOptions();
        }
      });
      
      jPanel10 = new JPanel();
      jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(jPanel10);
      jPanel10.add(new JLabel("ODF sharpness: " + getSharpness()));
      
      jPanel10 = new JPanel();
      jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(jPanel10);
      jPanel10.add(new JLabel("Import standard odf:"));
      jPanel10.add(jb = new JButton("Beartex"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          String resS = importODFfromBEARTEX();
          if (resS != null)
            resTF.setText(resS);
        }
      });
      jb.setToolTipText("Press this to load an odf using the Beartex/Maud exchange format (.mod)");
      
      jPanel10 = new JPanel();
      jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(jPanel10);
      jPanel10.add(new JLabel("Export ODF formatted (text) for "));
      jPanel10.add(jb = new JButton("Beartex"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          exportODFtoBEARTEX();
        }
      });
      jb.setToolTipText("Press this to save the odf using an exchage Beartex/Maud format");
      
      jPanel10 = new JPanel();
      jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(jPanel10);
      jPanel10.add(new JLabel("Export PFs (.xpc) for "));
      jPanel10.add(jb = new JButton("Beartex"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          exportPFsinBEARTEXformat();
        }
      });
      jb.setToolTipText("Press this to save the PFs using the Beartex format");
      
      jPanel10 = new JPanel();
      jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(jPanel10);
      jPanel10.add(jb = new JButton("Compute"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          setCursor(new Cursor(Cursor.WAIT_CURSOR));
          setSharpness(computeAndGetSharpness());
          sharpL.setText("Texture index (F2): " + getSharpness());
          setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        }
      });
      jb.setToolTipText("Press this to compute the Texture index (F2)");
      jPanel10.add(sharpL = new JLabel("Texture index (F2): " + getSharpness()));
      
      jPanel10 = new JPanel();
      jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(jPanel10);
      jPanel10.add(jb = new JButton("ODF from PF"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          (new PersistentThread() {
            public void executeJob() {
              loadPFandComputeODF(JWTextureOptionsD.this);
            }
          }).start();
        }
      });
      jb.setToolTipText("Press this to compute the ODF from traditional Pole Figures");
      
      
      setTitle("Texture options panel");
      initParameters();
      pack();
    }
    
    public void initParameters() {
      symmetryCB.setSelectedItem(getSampleSymmetry());
      statusCB.setSelected(getWIMVstatus());
      refinableCB.setSelected(ODFisRefinable());
      interpolationCB.setSelectedItem(getInterpolationMethod());
      minIntTF.setText(getMinimumIntensity());
      resTF.setText(getResolution());
    }
    
    public void retrieveParameters() {
      setSampleSymmetry(symmetryCB.getSelectedItem().toString());
      setWIMVstatus(statusCB.isSelected());
      setODFrefinable(refinableCB.isSelected());
      setInterpolationMethod(interpolationCB.getSelectedItem().toString());
      setMinimumIntensity(minIntTF.getText());
      setResolution(resTF.getText());
    }
    
    public void advancedOptions() {
      (new advancedOptionsFrame(this)).setVisible(true);
    }
    
    public void interpolationOptions() {
      String selectedInterpolation = interpolationCB.getSelectedItem().toString();
      if (!getInterpolationMethod().equals(selectedInterpolation))
        setInterpolationMethod(selectedInterpolation);
      getActiveInterpolation().getOptionsDialog(this).setVisible(true);
    }
    
    public String importODFfromBEARTEX() {
      String filename = Utility.browseFilename(this, "load ODF file from BEARTEX (.maa)");
      String resS = ODFinputStandard(filename);
//      odfnotLoaded = false;
      return resS;
    }
    
    public void exportODFtoBEARTEX() {
      String filename = Utility.browseFilenametoSave(this, "export ODF file for BEARTEX (.maa)");
      ODFoutputStandard(filename);
//      ODFoutputBeartex(filename, odf, SpaceGroups.getLGNumberSiegfriedConv(getPhase()), getResolutionD());
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
    
  }
  
  class advancedOptionsFrame extends JOptionsDialog {
    JTextField optionTF[] = null;
    JComboBox optionCB[] = null;
    
    public advancedOptionsFrame(Frame parent) {
      
      super(parent, null);
      
      int rownumber = 8;
      
      principalPanel.setLayout(new GridLayout(rownumber, 1, 6, 6));
      
      JPanel panel1;
      
      optionTF = new JTextField[6];
      optionCB = new JComboBox[2];
      
      for (int i = 0; i < rownumber; i++) {
        panel1 = new JPanel();
        panel1.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
        principalPanel.add(panel1);
        panel1.add(new JLabel(diclist[i + 2].substring(11)));
        if (i < 6) {
          optionTF[i] = new JTextField(Constants.FLOAT_FIELD);
          optionTF[i].setToolTipText(WIMVtooltip[i]);
          panel1.add(optionTF[i]);
        } else if (i == 6) {
          optionCB[i - 6] = new JComboBox();
          for (int j = 0; j < ZigZagnumber; j++) {
            optionCB[i - 6].addItem(ZigZag[j]);
          }
          optionCB[i - 6].setToolTipText(WIMVtooltip[i]);
          panel1.add(optionCB[i - 6]);
        } else if (i == 7) {
          optionCB[i - 6] = new JComboBox();
          for (int j = 0; j < ODFoptimizationnumber; j++) {
            optionCB[i - 6].addItem(ODFoptimization[j]);
          }
          optionCB[i - 6].setToolTipText(WIMVtooltip[i]);
          panel1.add(optionCB[i - 6]);
        }
      }
      
      setTitle("WIMV advanced options");
      initParameters();
      pack();
      
    }
    
    public void initParameters() {
      for (int i = 0; i < 6; i++)
        optionTF[i].setText(getWIMVOption(i));
      optionCB[0].setSelectedItem(getZigZag());
      optionCB[1].setSelectedItem(getODFoptimization());
    }
    
    public void retrieveParameters() {
      for (int i = 0; i < 6; i++)
        setWIMVOption(i, optionTF[i].getText());
      setZigZag(optionCB[0].getSelectedItem().toString());
      setODFoptimization(optionCB[1].getSelectedItem().toString());
    }
    
  }
  
}

