/*
 * @(#)StandardFunctionTexture.java created Apr 29, 2004 Casalino
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.border.BevelBorder;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;


/**
 * The StandardFunctionTexture is a class
 * 
 * @author Luca Lutterotti
 * @version $Revision: 1.8 $, $Date: 2006/12/04 14:30:05 $
 * @since JDK1.1
 */

public class StandardFunctionTexture extends Texture {

  public static String[] diclistc = {"_rita_sample_symmetry", "_rita_odf_sharpness",

                                     "_rita_odf_background",

                                     "_texture_fiber_component_id",
                                     "_texture_spherical_component_id"
  };
  public static String[] diclistcrm = {"_rita_sample_symmetry", "_rita_odf_sharpness",

                                     "_rita_odf_background",

                                     "_texture_fiber_component_id",
                                     "_texture_spherical_component_id"
  };

  public static String[] classlistcs = {};
  public static String[] classlistc = {"it.unitn.ing.rista.diffr.rta.FiberTextureComponent",
                                       "it.unitn.ing.rista.diffr.rta.SphericalTextureComponent"};
  public static String[] symmetrychoice = {"triclinic",
                                           "monoclinic",
                                           "orthorhombic",
                                           "planar"};

  Sample actualsample = null;
  int sampleSymmetry = 0;
  boolean GSASmode = false;
  double background = 0.0;
  double totalIntensity = 1.0;
	int gammaNumber = 1;
	double[] sfVector;
	double[] cfVector;

  public int IZGA, IZGB, IGA, IGB;

  public static final int[][] MGE =
      {{1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1},
       {0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1}, {0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1},
       {0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1}, {0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1},
       {0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1},
       {0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1}, {0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1},
       {0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1},
       {0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0},
       {0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0}, {0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0},
       {0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0},
       {0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0},
       {0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0}, {0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0},
       {0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0},
       {1, 2, 4, 4, 8, 12, 24, 3, 6, 6, 12}};

  int IJE = 0;
  public int[] MGB = new int[96];

  public int IRED = 0; // 0 = unred , 1 = upper, 2 = lower

  public StandardFunctionTexture(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Standard Functions";
    IDlabel = "Standard Functions";
    description = "select this to apply Standard Functions model";
  }

  public StandardFunctionTexture(XRDcat aobj) {
    this(aobj, "Standard Functions method");
  }

  public StandardFunctionTexture() {
    identifier = "Standard Functions";
    IDlabel = "Standard Functions";
    description = "select this to apply Standard Functions model";
  }

  public void initConstant() {
    Nstring = 2;
    Nstringloop = 0;
    Nparameter = 1;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 2;
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
    parameterField[0] = new Parameter(this, getParameterString(0), 0.0,
        ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.0),
        ParameterPreferences.getDouble(getParameterString(0) + ".max", 1.0));

    refreshComputation = true;
  }

	public void rotateODFBy(double alpha, double beta, double gamma, int multAlpha, int multBeta, int multGamma) {
		int numberOfFiberComponent = fiberTextureComponentsNumber();
		for (int i = 0; i < numberOfFiberComponent; i++) {
			FiberTextureComponent fiberComp = getFiberTextureComponent(i);
			fiberComp.rotateODFBy(alpha, beta, gamma, multAlpha, multBeta, multGamma);
		}
		int numberOfSphericalComponent = sphericalTextureComponentsNumber();
		for (int i = 0; i < numberOfSphericalComponent; i++) {
			SphericalTextureComponent sphericalComp = getSphericalTextureComponent(i);
			sphericalComp.rotateODFBy(360 - alpha, beta, gamma, -multAlpha, multBeta, multGamma);
		}
	}

	public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
          notifyParameterChanged(source, Constants.TEXTURE_CHANGED, -1);
          return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.TEXTURE_CHANGED, -1);
            return;
          }

      super.notifyParameterChanged(source);
    }
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
    stringField[0] = new String(symmetrychoice[i]);
  }

  public void setSampleSymmetry(String value) {
    stringField[0] = new String(value);
  }

  public void setSharpness(String value) {
    setString(1, value);
  }

  public String getSharpness() {
    return getString(1);
  }

  public void update(boolean firstLoading) {
    background = Math.abs(parameterField[0].getValueD());
  }

  public int fiberTextureComponentsNumber() {
    return numberofelementSubL(0);
  }

  public FiberTextureComponent getFiberTextureComponent(int index) {
    return (FiberTextureComponent) subordinateloopField[0].elementAt(index);
  }

  public int sphericalTextureComponentsNumber() {
    return numberofelementSubL(1);
  }

  public SphericalTextureComponent getSphericalTextureComponent(int index) {
    return (SphericalTextureComponent) subordinateloopField[1].elementAt(index);
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);
  }

  public int getLaueGroupNumber() {
    return SpaceGroups.getLGNumberSiegfriedConv(getPhase().getPointGroup());
  }

  public Phase getPhase() {
    return (Phase) getParent();
  }

  public int getLGnumber() {
    return getPhase().getLaueGroup();
  }

  public int getPGnumber() {
    return getPhase().getPointGroup();
  }

  public void refreshForNotificationDown(XRDcat source, int reason) {
    if (!getFilePar().isComputingDerivate() || (source == this ||
            (reason == Constants.SAMPLE_ORIENTATION_CHANGED || reason == Constants.TEXTURE_CHANGED
		            || (source == getParent() &&
            (reason == Constants.STRAIN_CHANGED || reason == Constants.CELL_CHANGED))))) {
      refreshComputation = true;
/*	    Object[] childrens = getObjectChildren();
	    int numberOfChildrens = childrens.length;
	    for (int i = 0; i < numberOfChildrens; i++) {
//				System.out.println(this.toXRDcatString() + ": " + childrens[i].toXRDcatString());
		    ((XRDcat) childrens[i]).refreshAll(false);
	    }*/
    }
  }

  public void refreshForNotificationUp(XRDcat source, int reason, int paramNumber) {
    super.refreshForNotificationUp(source, reason, paramNumber);
//    System.out.println("Refreshing texture: " + reason + " " + Constants.TEXTURE_CHANGED);
    if (reason == Constants.TEXTURE_CHANGED)
      refreshComputation = true;
  }

  public void applySymmetryRules() {
//    LGIndex = SpaceGroups.getLGNumber(getPhase().getPointGroup());
    sampleSymmetry = getSampleSymmetryValue();
    IGA = sampleSymmetry + 1;
	  if (sampleSymmetry > 2)
		  IGA = 1;
	  gammaNumber = 1;
	  if (sampleSymmetry == 3)
		  gammaNumber = MaudPreferences.getInteger("standardFunctionsTexture.numberOfIntegrationPointPlanarSymmetry", 72);
	  sfVector = new double[gammaNumber];
	  cfVector = new double[gammaNumber];
	  if (sampleSymmetry == 3) {
		  double stepPsi = Constants.PI2 / gammaNumber;
		  for (int i = 0; i < gammaNumber; i++) {
			  double psi = stepPsi * i;
			  sfVector[i] = Math.sin(psi);
			  cfVector[i] = Math.cos(psi);
		  }
	  }
	  IGB = getLaueGroupNumber();
     refreshCoefficients();
  }

  public void initializeAll() {
    applySymmetryRules();
  }

  public void refreshCoefficients() {

//    double FAK5 = Constants.PI / 36.;

//   Pole Figure Regions
//      int ifie=73;
//      int itm=19;

//    LATTICE();

    IZGA = MGE[24][IGA - 1]; // MGE(IGA,25)
    IZGB = MGE[24][IGB - 1];
    int IZE = 12;
    if (IGB < 8)
      IZE = 24;
    IJE = IZE * IZGA;
    for (int IJ = 0; IJ < IJE; IJ++)
      MGB[IJ] = MGE[IJ - IZE * (IJ / IZE)][IGB - 1];

/*    for (int ITF = 1; ITF <= 73; ITF++) {
      double TF = FAK5 * (ITF - 1);
      IFIW[ITF] = 5 * (ITF - 1);
      SM[ITF] = Math.sin(TF);
      CM[ITF] = Math.cos(TF);
    }*/
    int IELAG = 48;
    if (IGB < 8)
      IELAG = 96;

    double intensitySum = 0.0;
    int numberOfFiberComponent = fiberTextureComponentsNumber();
    for (int i = 0; i < numberOfFiberComponent; i++) {
      FiberTextureComponent fiberComp = getFiberTextureComponent(i);
      intensitySum += fiberComp.intensity;
    }
    int numberOfSphericalComponent = sphericalTextureComponentsNumber();
    for (int i = 0; i < numberOfSphericalComponent; i++) {
      SphericalTextureComponent sphericalComp = getSphericalTextureComponent(i);
      intensitySum += sphericalComp.intensity;
    }
/*
 *    PREPARATION OF INPUT DATA FOR FIBRE COMPONENTS
 */
    for (int i = 0; i < numberOfFiberComponent; i++) {
      FiberTextureComponent fiberComp = getFiberTextureComponent(i);
//      fiberComp.intensity *= (1.0 - background) / intensitySum;
      fiberComp.prepareComputation(IZE);
    }

/*
 *    PREPARATION OF INPUT DATA FOR SPHERICAL COMPONENTS
 */

    for (int i = 0; i < numberOfSphericalComponent; i++) {
      SphericalTextureComponent sphericalComp = getSphericalTextureComponent(i);
//      sphericalComp.intensity *= (1.0 - background) / intensitySum;
      sphericalComp.prepareComputation(IELAG);
    }

    totalIntensity = intensitySum + background;
    if (totalIntensity == 0.0)
      totalIntensity = 1.0;

  }

  public void computeTextureFactor(Phase aphase, Sample asample) {

//		double[] cdsc = aphase.lattice();

//    System.out.println("Should compute texture: " + refreshComputation);
    if (refreshComputation) {
      applySymmetryRules();
      aphase.sghklcompute(false);
	    int fnumber = fiberTextureComponentsNumber();
	    int snumber = sphericalTextureComponentsNumber();
//  	System.out.println("computing texture");
//			setSharpness(computeAndGetSharpness());
      refreshComputation = false;
	    int hkln = aphase.gethklNumber();
	    double[] textF = new double[hkln];
	    for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
		    DataFileSet adataset = asample.getActiveDataSet(i);
//					Instrument ainstrument = adataset.getInstrument();
//					Radiation rad = ainstrument.getRadiationType().getRadiation(0);
		    double betaBroad = adataset.getInstrument().getInstrumentBroadening().getTextureBroadeningAt(0);
		    if (betaBroad >= 0.0) {
//		            System.out.println(snumber);
			    for (int ig = 0; ig < fnumber; ig++) {
				    FiberTextureComponent fiberComp = getFiberTextureComponent(ig);
//		              System.out.println(ig + " " + fiberComp.betag + betaBroad);
				    fiberComp.PARFP(fiberComp.betag + betaBroad);
			    }
			    for (int ig = 0; ig < snumber; ig++) {
				    SphericalTextureComponent sphericalComp = getSphericalTextureComponent(ig);
//		              System.out.println(ig + " " + sphericalComp.betag + betaBroad);
				    sphericalComp.PARGLP(sphericalComp.betag + betaBroad);
			    }
		    }

		    int datafilenumber = adataset.activedatafilesnumber();
		    for (int i1 = 0; i1 < datafilenumber; i1++) {
			    DiffrDataFile adatafile = adataset.getActiveDataFile(i1);
			    double[][][] positions = adatafile.getPositions(aphase);
//			    for (int ppp = 0; ppp < adatafile.positionsPerPattern; ppp++) {
				    for (int j = 0; j < hkln; j++) {
					    Reflection refl = aphase.getReflex(j);
					    double texture_angles[] = adatafile.getTextureAngles(positions[j][0][0]);
					    double newBetaBroad = adataset.getInstrument().getInstrumentBroadening().getTextureBroadeningAt(positions[j][0][0]);
//	            System.out.println(snumber + " " + betaBroad);
					    if (betaBroad != newBetaBroad) {
//		            System.out.println(snumber);
						    for (int ig = 0; ig < fnumber; ig++) {
							    FiberTextureComponent fiberComp = getFiberTextureComponent(ig);
//		              System.out.println(ig + " " + fiberComp.betag + betaBroad);
							    fiberComp.PARFP(fiberComp.betag + betaBroad);
						    }
						    for (int ig = 0; ig < snumber; ig++) {
							    SphericalTextureComponent sphericalComp = getSphericalTextureComponent(ig);
//		              System.out.println(ig + " " + sphericalComp.betag + betaBroad);
							    sphericalComp.PARGLP(sphericalComp.betag + betaBroad);
						    }
					    }
					    textF[j] = computeTextureFactor(refl.phi[0], refl.beta[0],
							    texture_angles[0] * Constants.DEGTOPI,
							    texture_angles[1] * Constants.DEGTOPI);
//		              System.out.println(positions[j][0][0] + " " + refl.getH() + " " + refl.getK() + " " + refl.getL() + " " +
//				              refl.phi[0] * Constants.PITODEG + " " + refl.beta[0] * Constants.PITODEG + " " + texture_angles[0] + " " + texture_angles[1] + " " + textF[j]);
//						refl.setExpTextureFactor(adatafile.getIndex(), textF);
				    }
			    adatafile.setTextureFactors(aphase, textF);
//			    }
		    }
	    }
    }
  }

  double[] HX = new double[96], HY = new double[96], HZ = new double[96];

  public double computeTextureFactor(double phi, double beta, double psi, double gamma) {

	  // for planar sample symmetry we homogenize around psi

    double poleIntensity = 0;
    double ZH = Math.cos(phi);
    double STH = Math.sin(phi);
    double XH = STH * Math.cos(beta);
    double YH = STH * Math.sin(beta);

/*
 *    CALCULATION OF (G(J,K))**-1*HI FOR ALL
 *    EQUIVALENT POSITIONS (IJ)
 */
	  if (gammaNumber == 1) {
		  sfVector[0] = Math.sin(gamma);
		  cfVector[0] = Math.cos(gamma);
	  }
	  for (int i = 0; i < gammaNumber; i++)
	    poleIntensity += computeTextureFactor(ZH, STH, XH, YH, psi, sfVector[i], cfVector[i]);

    return poleIntensity / (totalIntensity * gammaNumber) + background;
  }

	public double computeTextureFactor(double ZH, double STH, double XH, double YH, double psi, double SF, double CF) {

		// for planar sample symmetry we homogenize around psi

		double poleIntensity = 0;

/*
 *    CALCULATION OF (G(J,K))**-1*HI FOR ALL
 *    EQUIVALENT POSITIONS (IJ)
 */
//		double SF = Math.sin(gamma);
//		double CF = Math.cos(gamma);
		double ST = Math.sin(psi);
		double CT = Math.cos(psi);
		double STCF = ST * CF;
		double STSF = ST * SF;
		int numberOfSphericalComponent = sphericalTextureComponentsNumber();
		for (int i = 0; i < numberOfSphericalComponent; i++) {
			SphericalTextureComponent sphericalComp = getSphericalTextureComponent(i);
			for (int IJ = 0; IJ < IJE; IJ++) {
				if (MGB[IJ] != 0) {
					double CA = -sphericalComp.HMCG[IJ];
					double CB = sphericalComp.HMCB[IJ];
					double CG = -sphericalComp.HMCA[IJ];
					double SA = sphericalComp.HMSG[IJ];
					double SB = sphericalComp.HMSB[IJ];
					double SG = sphericalComp.HMSA[IJ];
					HX[IJ] = (CA * CB * CG - SA * SG) * XH + (SA * CB * CG + CA * SG) * YH - SB * CG * ZH;
					HY[IJ] = -(CA * CB * SG + SA * CG) * XH - (SA * CB * SG - CA * CG) * YH + SB * SG * ZH;
					HZ[IJ] = CA * SB * XH + SA * SB * YH + CB * ZH;
				}
			}
			double PSUMG = 0.;
			double PSUML = 0.;
			double Z1 = 0.;
			for (int IJ = 0; IJ < IJE; IJ++) {
				if (MGB[IJ] != 0) {
					if (IRED == 2) {
						Z1 = -HX[IJ] * STCF - HY[IJ] * STSF + HZ[IJ] * CT;
					} else {
						Z1 = HX[IJ] * STCF + HY[IJ] * STSF + HZ[IJ] * CT;
					}
//          System.out.println(IJ + " " + Z1 + " " + HX[IJ] + " " + HY[IJ] + " " +HZ[IJ]);
					PSUMG += sphericalComp.PSHG(Z1);
				}
			}
			for (int IJ = 0; IJ < IJE; IJ++) {
				if (MGB[IJ] != 0) {
					Z1 = sphericalComp.HL2 * (HX[IJ] * STCF + HY[IJ] * STSF + HZ[IJ] * CT);
					PSUML += sphericalComp.PSHL(Z1);
				}
			}
			poleIntensity += PSUML * sphericalComp.HNL + PSUMG * sphericalComp.HNG;

		}

		int numberOfFiberComponent = fiberTextureComponentsNumber();
		double[] ZZ1 = new double[IZGB];
		double[] WW1 = new double[IZGB];
		for (int i = 0; i < numberOfFiberComponent; i++) {
			FiberTextureComponent fiberComp = getFiberTextureComponent(i);

			for (int JB = 0; JB < IZGB; JB++) {
				ZZ1[JB] = fiberComp.HXHF[JB] * XH + fiberComp.HYHF[JB] * YH + fiberComp.HZHF[JB] * ZH;
				WW1[JB] = 0.;
				double ZZ = 1. - ZZ1[JB] * ZZ1[JB];
				if (ZZ > 1.E-20)
					WW1[JB] = Math.sqrt(ZZ);
			}

			double[] ZZ2 = new double[IZGA];
			double[] WW2 = new double[IZGA];
			for (int KA = 0; KA < IZGA; KA++) {
				ZZ2[KA] = fiberComp.HXYF[KA] * STCF + fiberComp.HYYF[KA] * STSF + fiberComp.HZYF[KA] * CT;
				WW2[KA] = 0.;
				double ZZ = 1. - ZZ2[KA] * ZZ2[KA];
				if (ZZ > 1.E-20)
					WW2[KA] = Math.sqrt(ZZ);
			}
			double PSUMG = 0.;
			double PSUML = 0.;

//      IF (IGLF(IKF).EQ.2) GOTO 185
			for (int JB = 0; JB < IZGB; JB++) {
				for (int KA = 0; KA < IZGA; KA++) {
					double ZZ = ZZ1[JB] * ZZ2[KA];
					double WW = WW1[JB] * WW2[KA];
					PSUMG += FiberTextureComponent.PSGF(ZZ, WW, fiberComp.FS);
				}
			}
			//     GO TO 191
			for (int JB = 0; JB < IZGB; JB++) {
				for (int KA = 0; KA < IZGA; KA++) {
					double ZZ = ZZ1[JB] * ZZ2[KA];
					double WW = WW1[JB] * WW2[KA];
					PSUML += FiberTextureComponent.PSLF(ZZ, WW, fiberComp.EPT2, fiberComp.ZT);
				}
			}
//			System.out.println(IZGB + " " + IZGA + " " + PSUMG + " " + fiberComp.HNFUKG + " " + PSUML + " " + fiberComp.HNFUKL);
			poleIntensity += PSUMG * fiberComp.HNFUKG + PSUML * fiberComp.HNFUKL;

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

	  double odf = background;

	  double sg = Math.sin(gamma);
	  double cg = Math.cos(gamma);
	  double sb = Math.sin(beta);
	  double cb = Math.cos(beta);
	  double sa = Math.sin(alpha);
	  double ca = Math.cos(alpha);

	  int numberOfSphericalComponent = sphericalTextureComponentsNumber();
	  for (int i = 0; i < numberOfSphericalComponent; i++) {
		  SphericalTextureComponent sphericalComp = getSphericalTextureComponent(i);
		  double[] sum = sphericalComp.ovf(IJE, MGB, IRED, ca,  sa, cb, sb, cg, sg);
		  odf += sum[0];
	  }

/*	  int numberOfSphericalComponent = sphericalTextureComponentsNumber();
	  for (int i = 0; i < numberOfSphericalComponent; i++) {
		  SphericalTextureComponent sphericalComp = getSphericalTextureComponent(i);
		  for (int IJ = 0; IJ < IJE; IJ++) {
			  if (MGB[IJ] != 0) {
				  double CA = -sphericalComp.HMCG[IJ];
				  double CB = sphericalComp.HMCB[IJ];
				  double CG = -sphericalComp.HMCA[IJ];
				  double SA = sphericalComp.HMSG[IJ];
				  double SB = sphericalComp.HMSB[IJ];
				  double SG = sphericalComp.HMSA[IJ];
				  HX[IJ] = (CA * CB * CG - SA * SG) * XH + (SA * CB * CG + CA * SG) * YH - SB * CG * ZH;
				  HY[IJ] = -(CA * CB * SG + SA * CG) * XH - (SA * CB * SG - CA * CG) * YH + SB * SG * ZH;
				  HZ[IJ] = CA * SB * XH + SA * SB * YH + CB * ZH;
			  }
		  }
		  double PSUMG = 0.;
		  double PSUML = 0.;
		  double Z1 = 0.;
		  for (int IJ = 0; IJ < IJE; IJ++) {
			  if (MGB[IJ] != 0) {
				  if (IRED == 2) {
					  Z1 = -HX[IJ] * STCF - HY[IJ] * STSF + HZ[IJ] * CT;
				  } else {
					  Z1 = HX[IJ] * STCF + HY[IJ] * STSF + HZ[IJ] * CT;
				  }
//          System.out.println(IJ + " " + Z1 + " " + HX[IJ] + " " + HY[IJ] + " " +HZ[IJ]);
				  PSUMG += sphericalComp.PSHG(Z1);
			  }
		  }
		  for (int IJ = 0; IJ < IJE; IJ++) {
			  if (MGB[IJ] != 0) {
				  Z1 = sphericalComp.HL2 * (HX[IJ] * STCF + HY[IJ] * STSF + HZ[IJ] * CT);
				  PSUML += sphericalComp.PSHL(Z1);
			  }
		  }
		  odf += PSUML * sphericalComp.HNL + PSUMG * sphericalComp.HNG;

	  }*/


	  int numberOfFiberComponent = fiberTextureComponentsNumber();
	  for (int i = 0; i < numberOfFiberComponent; i++) {
		  FiberTextureComponent fiberComp = getFiberTextureComponent(i);
			double[] sum = fiberComp.ovfib(IZGA, IZGB, IRED, ca,  sa, cb, sb, cg, sg);
		  odf += sum[0];
	  }

	  return odf / totalIntensity;
  }

  public double[][] getExpPoleFigureGrid(Reflection refl, int numberofPoints, double maxAngle) {
    return getPoleFigureGrid(refl, numberofPoints, maxAngle);
  }

  public void addTextureBroadening() {
  	Sample asample = getFilePar().getActiveSample();
	  int fnumber = fiberTextureComponentsNumber();
	  int snumber = sphericalTextureComponentsNumber();

	  double betaBroad = 0;
	  for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
		  DataFileSet adataset = asample.getActiveDataSet(i);
		  betaBroad += adataset.getInstrument().getInstrumentBroadening().getTextureBroadeningAt(0);
	  }
	  betaBroad /= asample.activeDatasetsNumber();
	  if (betaBroad >= 0.0) {
		  for (int ig = 0; ig < fnumber; ig++) {
			  FiberTextureComponent fiberComp = getFiberTextureComponent(ig);
			  fiberComp.PARFP(fiberComp.betag + betaBroad);
		  }
		  for (int ig = 0; ig < snumber; ig++) {
			  SphericalTextureComponent sphericalComp = getSphericalTextureComponent(ig);
			  sphericalComp.PARGLP(sphericalComp.betag + betaBroad);
		  }
	  }
  }

  public double[][] getPoleFigureGrid(Reflection refl, int numberofPoints, double maxAngle) {

    double[][] PFreconstructed = new double[numberofPoints][numberofPoints];

    double texture_angles[] = new double[2];

    double x, y, r;
    double dxy = 2.0 * maxAngle / numberofPoints;

//		Phase aphase = (Phase) refl.getParent();
    applySymmetryRules();
//		aphase.sghklcompute(false);
	  addTextureBroadening();

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

  public double[] getPoleFigureGrid(Reflection refl, double[] x, double[] y) {

    double r;
    double texture_angles[] = new double[2];


//		Phase aphase = (Phase) refl.getParent();
    applySymmetryRules();
//		aphase.sghklcompute(false);
	  addTextureBroadening();

    for (int i = 0; i < x.length; i++) {
        r = Math.sqrt(x[i] * x[i] + y[i] * y[i]);
        if (r == 0.0) {
          texture_angles[0] = 0.0f;
          texture_angles[1] = 0.0f;
          y[i] = computeTextureFactor(refl.phi[0], refl.beta[0],
              texture_angles[0],
              texture_angles[1]);
        } else if (r < Math.PI / 2.0) {
          double phaseAng = Math.atan2(x[i], y[i]);
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

          y[i] = computeTextureFactor(refl.phi[0], refl.beta[0],
              texture_angles[0],
              texture_angles[1]);
        } else
          y[i] = 0;
      }
    return y;
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
	  addTextureBroadening();

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
	  addTextureBroadening();

    for (int i = 0; i < pointNumber; i++)
      PFreconstructed[i] = computeTextureFactor(phibeta[0][i], phibeta[1][i],
          texture_angles[0] * Constants.DEGTOPI,
          texture_angles[1] * Constants.DEGTOPI);

    return PFreconstructed;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new StandardFunctionTexture.JSFTextureOptionsD(parent, this);
    return adialog;
  }

  public class JSFTextureOptionsD extends JOptionsDialog {

    JComboBox symmetryCB;
    JLabel sharpL = null;
    JSubordListPane sphericalP;
    JSubordListPane fiberP;
    JButton mybutton;
    JTextField backGroundODFTF;

    public JSFTextureOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(3, 3));

      JPanel upperPanel = new JPanel();
      principalPanel.add(BorderLayout.NORTH, upperPanel);
      upperPanel.add(new JLabel("ODF background: "));
      backGroundODFTF = new JTextField(Constants.FLOAT_FIELD);
      upperPanel.add(backGroundODFTF);

      upperPanel = new JPanel();
      principalPanel.add(BorderLayout.CENTER, upperPanel);
      fiberP = new JSubordListPane(this, false);
      fiberP.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Fiber components"));
      upperPanel.add(fiberP);
      sphericalP = new JSubordListPane(this, false);
      sphericalP.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Spherical components"));
      upperPanel.add(sphericalP);

      JPanel lowerPanel = new JPanel();
      lowerPanel.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.SOUTH, lowerPanel);
      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      lowerPanel.add(jPanel8);
      jPanel8.add(new JLabel("Sample symmetry: "));
      symmetryCB = new JComboBox();
      for (int i = 0; i < symmetrychoice.length; i++)
        symmetryCB.addItem(symmetrychoice[i]);
      symmetryCB.setToolTipText("Set up unmeasured sample symmetries");
      jPanel8.add(symmetryCB);

      JPanel /* jPanel10 = new JPanel();
      jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      lowerPanel.add(jPanel10);
      jPanel10.add(new JLabel("Export ODF formatted (text) for "));
      jPanel10.add(jb = new JButton("Beartex"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          exportODFtoBEARTEX();
        }
      });
      jb.setToolTipText("Press this to save the odf using the Beartex/Maud exchange format");*/

          jPanel10 = new JPanel();
      jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      lowerPanel.add(jPanel10);
      jPanel10.add(new JLabel("Export PFs (.xpc) for "));
      JButton jb = new JButton("Beartex");
      jPanel10.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          exportPFsinBEARTEXformat();
        }
      });
      jb.setToolTipText("Press this to save the PFs using the Beartex format");

      jPanel10 = new JPanel();
      jPanel10.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      lowerPanel.add(jPanel10);
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

      setTitle("Texture options panel");
      initParameters();
      pack();
    }

    public void initParameters() {
      symmetryCB.setSelectedItem(getSampleSymmetry());
      String[] labels = new String[6];
      labels[0] = "Intensity";
      labels[1] = "Alpha";
      labels[2] = "Beta";
      labels[3] = "Gamma";
      labels[4] = "FWHM";
      labels[5] = "Gaussian";
      sphericalP.setList(StandardFunctionTexture.this, 1, 6, labels);
      labels = new String[7];
      labels[0] = "Intensity";
      labels[1] = "ThetaY";
      labels[2] = "PhiY";
      labels[3] = "ThetaH";
      labels[4] = "PhiH";
      labels[5] = "FWHM";
      labels[6] = "Gaussian";
      fiberP.setList(StandardFunctionTexture.this, 0, 7, labels);
      addComponenttolist(backGroundODFTF, parameterField[0]);
    }

    public void retrieveParameters() {
      setSampleSymmetry(symmetryCB.getSelectedItem().toString());
      sphericalP.retrieveparlist();
      fiberP.retrieveparlist();
      parameterField[0].setValue(backGroundODFTF.getText());
      removeComponentfromlist(backGroundODFTF);
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

/*    public void exportODFtoBEARTEX() {
      String filename = Misc.browseFilenametoSave(this, "export ODF file for Beartex (use .maa extension)");
      ODFoutputStandard(filename);
    }*/

    public void dispose() {
      sphericalP.dispose();
      fiberP.dispose();
      super.dispose();
    }

  }

}


/*

C    PROGRAM POLSSF    (PSSFB.FOR)
c      hardwired: ibst=1 (halfwidth in degrees)
c                 igl=1 (Gauss)
C    MATTHIES/VINEL-ROSSENDORF PC VERSION; BERKELEY VARIANT 4/92
C
C    VERSION FOR REDUCED AND UNREDUCED POLE FIGURES UPPER AND LOWER
C    SPHERE ETC. WENK+HELMING 9.9.1992
C    COMPUTES REDUCED POLE FIGURES P-TILDE (OR 'SCHLANGE'-SNAKE)
C    PSHI(Y) IN A 5-DEGREE GRID FOR THE VECTOR Y(TETA,FI)
C    ON THE UPPER HALFSPERE (TETA: 0...90,FI: 0..5*(IFIE-1) DEGREES)
C    FOR SPHERICAL PEAK- AND FIBRE- COMPONENTS
C    BY STANDARD FUNCTIONS (GAUSS OR LORENTZ SHAPE) AND POSSIBLE
C    - SAMPLE SYMMETRY  :
C                           ROTATION GROUP GA   CODE NUMBER IGA
C          ORTHORHOMBIC      222    D2                3
C          MONOCLINIC        2      C2                2
C          TRICLINIC         1      C1                1
C    - CRYSTAL SYMMETRY :
C                           ROTATION GROUP GB   CODE NUMBER IGB
C                            432    O                 7
C                            23     T                 6
C                            422    D4                5
C                            4      C4                4
C                            222    D2                3
C                            2      C2                2
C                            1      C1                1
C                            622    D6               11
C                            6      C6               10
C                            322    D3                9
C                            3      C3                8
C    ITITLE - TEXT ARRAY FOR FREE USE
C    IZPOL  - NUMBER OF POLE FIGURES TO BE CALCULATED (LE.20)
C    N      - NUMBER OF SPHERICAL COMPONENTS TO BE MIXED (LE.50)
C    NF     - NUMBER OF FIBRE     COMPONENTS TO BE MIXED (LE.5)
C    FON    - BACKGROUND  (0.LE.FON.LT.1)
C    EVERY POLE FIGURE IS CHARACTERIZED BY
C    THE VECTOR HI (TETAH,FIH) - SPHERICAL ANGLES (IN DEGREES)
C    WITH REGARD TO THE CRYSTAL COORDINATE SYSTEM KB AND BY ITS
C    NAME (MILLER INDEXES MILH,MILK,MILL) TO BE USED FOR PRINT ONLY
C    EVERY SPHERICAL COMPONENT IS DESCRIBED BY
C    ITS ORIENTATION (POSITION) G0 (ALPHA0,BETA0,GAMMA0) IN DEGREES
C    AND THE PARAMETERS:
C    IBST,BST (B,S OR T),HIN (TOTAL INTENSITY,0.LT.HIN.LE.1) AND IGL
C    EVERY FIBRE COMPONENT IS DESCRIBED BY
C    ITS AXIS (VECTOR Y REG.KA AND H REG.KB IN SPHER. KOORD.) IN DEGREES
C    AND THE PARAMETERS:
C    IBST,BST (B,S OR T),HIN (TOTAL INTENSITY,0.LT.HIN.LE.1) AND IGL
C          IBST = 0 HALFWIDTH GIVEN BY S (0.LE.S.LT.INFINITE)
C          IBST = 1 HALFWIDTH GIVEN BY B IN DEGREES (0.LT.B.LE.360)
C          IBST =-1 HALFWIDTH GIVEN BY T (0.LE.T.LT.1)
C          IGL  = 1 GAUSS-SHAPED CURVE (B OR S INPUT IS POSSIBLE)
C          IGL  = 2 LORENTZ-SHAPED CURVE (B OR T INPUT IS POSSIBLE)
C
C ********************
C    FOR PLOT THE FULL POLE FIGURE REGION IFIE = 73 IS TO BE USED.
C    NONREDUNDANT REGIONS : GA=D2 IFIE=19, GA=C2 IFIE=37,
C    FOR PURE CYCLIC FIBRE TEXTURES ( NS= 0, NF.GE.1, ALL TEYF = 0.)
C    USE IGA = 1 IFIE = 1.
C    FOR PLOT OF AN INCOMPLETE PF-ARRAY : TETAMAX=5*(ITM-1);ITM.LE.19
C    THE CALCULATION RUNS ALWAYS UP TO THETA =90 DEGREES
C
C          FILE ORGANISATION :
C          UNIT 5  INPUT DATA (IF THEY ARE PREPARED IN A DATA FILE)
C               6  LIST OTPUT (FOR PRINT)
C               9  OUTPUT POLE FIGURE FOR PLOT (BERKELEY FORMAT)
C
C    FOR THE PRESCRIPTION HOW THE CRYSTAL COORDINATE
C    SYSTEM KB IS DEFINED SEE TABLE  5.1, 14.1
C    AND FIG. 5.1-5.6,5.9-5.12
C
C    ATTENTION. IN ORDER TO GET NORMALIZED POLE FIGURES
C    FON + SUM OF THE N HIN = 1 MUST BE VALID
C
C    ATTENTION. FOR THE TYPE 3 OF CRYSTAL SYMMETRY
C    GBS INSTEAD OF GB HAS TO BE USED (CF. TABLE 14.1)
C
      CHARACTER FNAME*75,IGNAME(11)*4,f1*75, ititle*75
      DIMENSION TETAH(20),FIH(20),MILH(20),MILK(20),MILL(20)
      DIMENSION XIPF(19,73)
      DIMENSION IRED(20), ICO(20), FRAC(20), WEIGHT(5,5000)
      DIMENSION ALPHA0(10),BETA0(10),GAMMA0(10)
      DIMENSION TEYF(5),FIYF(5),TEHF(5),FIHF(5)
      DIMENSION XYF0(5),YYF0(5),ZYF0(5),XHF0(5),YHF0(5),ZHF0(5)
      DIMENSION HX(96),HY(96),HZ(96)
      DIMENSION SPMAS(19,73),IFIW(73)
      DIMENSION SM(73),CM(73)
      DIMENSION SALFA(96),SBETA(96),SGAMMA(96),
     1          CALFA(96),CBETA(96),CGAMMA(96)
      DIMENSION MGE(11,25)
      DIMENSION MGB(96)
      DIMENSION Z1(24),W1(24),Z2(4),W2(4)
      COMMON /ABGSC/SALFA,SBETA,SGAMMA,CALFA,CBETA,CGAMMA
      COMMON /HMSC/HMSA(96,10),HMSB(96,10),HMSG(96,10),
     1             HMCA(96,10),HMCB(96,10),HMCG(96,10)
      COMMON /HMGL/HL1(10),HL2(10),HL3(10),HN(10)
      COMMON /HGL/BST(10),HIN(10),IGALO(10),IBST(10)
      COMMON /PAR/B(10),S(10),T(10)
      COMMON /CYHF/BSTF(5),HINF(5),IGLF(5),IBSTF(5)
      COMMON /PARF/FB(5),FS(5),FT(5)
      COMMON /HMPF/HNF(5),EPT2(5),ZT(5)
      COMMON /HMXYZH/HXHF(24,5),HYHF(24,5),HZHF(24,5)
      COMMON /HMXYZY/HXYF(4,5),HYYF(4,5),HZYF(4,5)
      COMMON /XYZ/FX(24),FY(24),FZ(24)
      COMMON /GE/IZGA,IZGB
      COMMON /CSPMAS/ SPMAS
      COMMON /CMGB/MGB
      COMMON /CIFIW/IFIW,IFIE,IOUT,IUNIT,ITM
      COMMON /CMIL/TETAH,FIH,XH,YH,ZH,MILH,MILK,MILL,ICO,FRAC
      COMMON /TIT/ITITLE
      COMMON /CSCL/S12L,C12L,S23L,C23L,S31L,C31L
      COMMON /CABC/CDA,CDB
      COMMON /CPI/PI,FAK
      COMMON /LAT/ AA,BB,CC,ALPHA23,ALPHA31,ALPHA12,IGA,IGB
      common/inp/ fon,standhw,wemin,ieuler,ns,nf
      DATA MGE/
     1 1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,0,0,1,1,0,0,0,
     2 0,0,1,1,1,1,1,1,0,0,0,0,1,0,1,0,0,1,1,0,0,0,0,0,0,
     3 1,1,1,1,1,0,0,0,0,0,0,1,0,0,1,1,0,0,0,1,1,0,1,0,0,
     4 0,1,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,1,0,0,0,1,0,
     5 0,1,0,1,1,1,0,1,0,1,0,0,0,0,0,1,1,0,0,0,1,0,0,0,0,
     6 0,1,1,0,1,0,1,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1,1,
     7 0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,0,1,0,0,0,
     8 0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
     9 0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
     * 0,1,0,0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,1,1,0,
     1 0,0,0,0,0,0,0,0,1,1,0,0,0,0,1,2,4,4,8,12,24,3,6,6,12/
      DATA IGNAME/'(C1)','(C2)','(D2)','(C4)','(D4)','( T)','( O)',
     2'(C3)','(D3)','(C6)','(D6)'/
C
      OPEN (111,file='c:\beartex\status.tex')
      istatus=0
      write(111,*) istatus
      close (111)
c
      call setup(fname,weight,wgauss,imusic)
c  reposition cpt file
      read(8,*)
      read(8,*)
      read(8,*)
      read(8,*)
      read(8,*)
      read(8,*)
      read(8,*)
c
cc    WRITE (0,2001)
 2001 FORMAT(
     1 ' INPUT DATA (INPUT FILES)'/
     2 ' *****************************************'/
     3 ' ITITLE                               (A75*)'/
     4 ' SYMMETRY: IGB,IGA                    (2I*)'/
     2 ' LATTICE PARAMETERS                   (6F*)'/
     5 ' IZPOL,N,NF                           (3I*)'/
     5 ' FON,HW,IEULER                        (2F,I*)'/
     5 ' IWEI,WEMIN                           (I,F*)'/
     6 ' IZPOL DATA SETS FOR POLE FIGURES:    '/
     7 '   H,K,L,IRED(0=RED;1=UPPER,2=LOWER UNRED),ICO,XFRAC (5I,F*)'/
     8 ' SPHERICAL COMPONENT DATA - N SETS :'/
     9 '    2 OPTIONS  INTERACTIVE OR FROM FILE'/
     1 '      INTERACTIVE (UP TO 10 COMPONENTS)  '/
     1 '        ALP,BET,GAM,IBST,BST,HIN,IGL   (3F,I,2F,I*)'/
     2 '      FROM FILE (UNLIMITED)                     '/
     3 '        AL,BE,GA,HIN,WEI(2-5),N,NN,BST (3F8.2,5F5.3,I3,I2,F5.1)'
     4 /' FIBRE COMPONENT DATA - NF TIMES TWO SETS :   '/
     5 '    TEYF,FIYF,TEHF,FIHF            (4F*)      '/
     6 '    IBST,BST,HIN,IGL               (I*,2F*,I*)'/)
c
      PI=ACOS(-1.)
      FAK=PI/180.
      FAK5=PI/36.
      INP=5
      IOUT=6
      IUNIT=9
      IDAT=8
      icheck=0
C
C   Pole Figure Regions
      ifie=73
      itm=19
      WRITE(IOUT,*) IGB,IGA,IFIE,ITM
c
      CALL LATTICE
c
      IZGA=MGE(IGA,25)
      IZGB=MGE(IGB,25)
      IF (IGB.LT.8) IZE=24
      IF (IGB.GT.7) IZE=12
      IJE=IZE*IZGA
      DO 10 IJ=1,IJE
      JGE=IJ-IZE*((IJ-1)/IZE)
      IGE=MGE(IGB,JGE)
   10 MGB(IJ)=IGE
      DO 40 ITF=1,73
      TF=FAK5*(ITF-1)
      IFIW(ITF)=5*(ITF-1)
      SM(ITF)=SIN(TF)
   40 CM(ITF)=COS(TF)
      IF (IGB.LT.8) IELAG=96
      IF (IGB.GT.7) IELAG=48
C
C    READ AND WRITE OF THE INPUT DATA FOR POLE FIGURES
C    AND COMPONENTS
C
c  weight code
      iweight=1
      WRITE (IOUT,*) IZPOL,NS,NF,FON,IWEIGHT,WEMIN,STANDHW,IEULER
C    READ MILLER INDICES. CALCULATE PHI, THETA
      read(5,*) izpol
      write(6,'(a,i5)') ' Number of pole figures: ',izpol
      DO 5 IP=1,IZPOL
cc    WRITE (*,64) IP
   64 FORMAT('PF ',I2,' Enter h,k,l IRED(0=unred.,1=up,2=low),ico,
     *,fraction(5I,F*)')
      READ (5,*)MILH(IP),MILK(IP),MILL(IP),IRED(IP),ICO(IP),FRAC(IP)
      IF(FRAC(IP).EQ.0.) FRAC(IP)=1.
      WRITE(6,'(4i6,f6.2)')MILH(IP),MILK(IP),MILL(IP),IRED(IP),FRAC(IP)
      WRITE(*,*)MILH(IP),MILK(IP),MILL(IP),IRED(IP),FRAC(IP)
      CALL TFHKL(MILH(IP),MILK(IP),MILL(IP),CF,SF,CT,ST)
      TETAH(IP)=ATAN2(ST,CT)/FAK
      FIH(IP)=ATAN2(SF,CF)/FAK
    5 CONTINUE
c   end of input on file stpf.cfg
      close (5)
C
C    PREPARATION OF INPUT DATA FOR FIBRE COMPONENTS
C
   27 IF (NF.EQ.0) GOTO 23
      DO 26 IKF=1,NF
cc    WRITE (*,67) IKF
   67 FORMAT (2x,'Fibre Component ',i2,'  TEYF,FIYF,TEHF,FIHF    : '/)
      READ (inp,*) TEYF(IKF),FIYF(IKF),TEHF(IKF),FIHF(IKF)
C      WRITE (*,'(A/)')
C     * ' IBSTF (1,0,-1), Half Width, Intensity, IGLF (G=1,L=2): '
     1 ,IBSTF(IKF),BSTF(IKF),HINF(IKF),IGLF(IKF)
      CALL VECTOR (TEYF(IKF),FIYF(IKF),XYF0(IKF),YYF0(IKF),ZYF0(IKF))
      CALL VECTOR (TEHF(IKF),FIHF(IKF),XHF0(IKF),YHF0(IKF),ZHF0(IKF))
      CALL PARFP (IKF)
      IF (IGB.LT.8) CALL VXYZ24 (XHF0(IKF),YHF0(IKF),ZHF0(IKF))
      IF (IGB.GT.7) CALL VXYZ12 (XHF0(IKF),YHF0(IKF),ZHF0(IKF))
      IEL = 0
      DO 310 IJ=1,IZE
      IF (MGE(IGB,IJ).NE.1) GOTO 310
      IEL = IEL+1
      HXHF(IEL,IKF) = FX(IJ)
      HYHF(IEL,IKF) = FY(IJ)
      HZHF(IEL,IKF) = FZ(IJ)
  310 CONTINUE
      CALL VXYZ24 (XYF0(IKF),YYF0(IKF),ZYF0(IKF))
      IEL = 0
      DO 320 IJ=1,24
      IF (MGE(IGA,IJ).NE.1) GOTO 320
      IEL = IEL+1
      HXYF(IEL,IKF) = FX(IJ)
      HYYF(IEL,IKF) = FY(IJ)
      HZYF(IEL,IKF) = FZ(IJ)
  320 CONTINUE
   26 CONTINUE
c  it is not quite clear what happens above
   23 WRITE (IOUT,61)
   61 FORMAT (4X,'INPUT DATA AS THEY ARE READ'//)
      WRITE (IOUT,77) ITITLE
   77 FORMAT (A40,/)
      WRITE (IOUT,380)IGA,IGNAME(IGA),IZGA,IGB,IGNAME(IGB),IZGB
  380 FORMAT(/
     1 5X,'SAMPLE  SYMMETRY  IGA = ',I2,2X,A4,
     2 6X,'NUMBER OF GROUP ELEMENTS IZGA = ',I2,/,
     3 5X,'CRYSTAL SYMMETRY  IGB = ',I2,2X,A4,
     4 6X,'NUMBER OF GROUP ELEMENTS IZGB = ',I2,/)
      IF (IGB.LT.8) WRITE (IOUT,410) (MGB(IJ),IJ=1,IJE)
  410 FORMAT( 5X,'ELEMENT-CODE MGB(K,J) : ',
     1 24(I2)/,3(29X,24(I2)/)/)
      IF (IGB.GT.7) WRITE (IOUT,420) (MGB(IJ),IJ=1,IJE)
  420 FORMAT( 5X,'ELEMENT-CODE MGB(K,J) : ',
     1 12(I2)/,3(29X,12(I2)/)/)
      WRITE (IOUT,370) IZPOL,NS,NF,FON,IFIE,ITM
  370 FORMAT(3X,'NUMBER OF POLE FIGURES  ',I3/
     1 3X,'NUMBER OF SPHERICAL COMPONENTS  NS = ',I2,
     2 3X,'NUMBER OF FIBRE COMPONENTS NF = ',I2/3X,'FON = ',F7.5,
     3 '  IFIE = ',I2,'   ITM = ',I2,/,
     4 3X,75(1H-)/60X,'8888.0 MEANS DUMMY'/)
   22 CONTINUE
C
C    POLE FIGURE-LOOP (IP)
C
      icount=1
      DO 230 IP=1,IZPOL
      WRITE(6,*) 'IRED=',IRED(IP),'  ++++++++'
      TH=FAK*TETAH(IP)
      FH=FAK*FIH(IP)
      ZH=COS(TH)
      STH=SIN(TH)
      XH=STH*COS(FH)
      YH=STH*SIN(FH)
      DO 50 IT=1,19
      DO 50 IFI=1,IFIE
   50 SPMAS(IT,IFI)=FON
C
C    SPHER. COMPONENT-LOOP (IK)
C
      IF (NS.EQ.0) GOTO 131
C
      IKF = 0
cc      DO 130 IK=1,NS
c  iiiik is just a running index, components not dimensioned
      DO 130 IIIIK=1,NS
cc    write(*,*) 'icheck= ',icheck
      IF(ICHECK.EQ.1) IK=IIIIK
      IF(ICHECK.EQ.1) GO TO 2999
      ik=1
C
C    PREPARATION OF INPUT DATA FOR SPHERICAL COMPONENTS
C
  199 READ (8,197,END=198) ALPHA0[IK],BETA0[IK],GAMMA0(IK),
     1 xxx,BST(IK)
      write(*,197) ALPHA0(IK),BETA0(IK),GAMMA0(IK),
     1 WEIGHT(1,IIIIK),BST(IK)
      write(*,'(a,5f6.1)') 'abc: ',ALPHA0(IK),BETA0(IK),GAMMA0(IK),
     1 WEIGHT(1,IIIIK),BST(IK)
  197 format (3f8.2,f6.3,18x,f5.1)
cc    convert to Roe/Matthies angles if necessary
      call transform(alpha0,beta0,gamma0,ieuler)
      WRITE(IOUT,200) ALPHA0(IK),BETA0(IK),GAMMA0(IK),
     1 (WEIGHT(I,IIIIK),I=1,5),BST(IK)
  200 FORMAT(3F8.2,5F6.2,F5.1)
      HIN(IK)=WEIGHT(1,IIIIK)
      IF(HIN(IK).EQ.0.) HIN(IK)=(1.-FON)/NS
      IF(BST(IK).EQ.0.) BST(IK)=STANDHW
      IF(wgauss.ne.0.) BST(IK)=wgauss
      IBST(IK)=1
      IGALO(IK)=1
      IF(WEIGHT(IWEIGHT,IIIIK).LT.WEMIN) GO TO 199
      CALL PARGLP (IK)
      CAN=COS(ALPHA0(IK)*FAK)
      SAN=SIN(ALPHA0(IK)*FAK)
      CBN=COS(BETA0(IK)*FAK)
      SBN=SIN(BETA0(IK)*FAK)
      CGN=COS(GAMMA0(IK)*FAK)
      SGN=SIN(GAMMA0(IK)*FAK)
      IF (IGB.LT.8) CALL SC96 (CAN,SAN,CBN,SBN,CGN,SGN)
      IF (IGB.GT.7) CALL SC48 (CAN,SAN,CBN,SBN,CGN,SGN)
      DO 30 I=1,IELAG
      HMSA(I,IK)=SALFA(I)
      HMSB(I,IK)=SBETA(I)
      HMSG(I,IK)=SGAMMA(I)
      HMCA(I,IK)=CALFA(I)
      HMCB(I,IK)=CBETA(I)
   30 HMCG(I,IK)=CGAMMA(I)
      WRITE (0,62) IP,IIIIK,IKF
 2999 CONTINUE
      IGL=IGALO(IK)
      CALL COMPP(IK,IELAG)
C
C    CALCULATION OF (G(J,K))**-1*HI FOR ALL
C    EQUIVALENT POSITIONS (IJ)
C
      DO 60 IJ=1,IJE
      IF (MGB(IJ).EQ.0) GO TO 60
      CA=-CGAMMA(IJ)
      CB=CBETA(IJ)
      CG=-CALFA(IJ)
      SA=SGAMMA(IJ)
      SB=SBETA(IJ)
      SG=SALFA(IJ)
      HX(IJ)=(CA*CB*CG-SA*SG)*XH+(SA*CB*CG+CA*SG)*YH-SB*CG*ZH
      HY(IJ)=-(CA*CB*SG+SA*CG)*XH-(SA*CB*SG-CA*CG)*YH+SB*SG*ZH
      HZ(IJ)=CA*SB*XH+SA*SB*YH+CB*ZH
   60 CONTINUE
C
C    TETA(IT)- AND FI(IFI)-LOOPS (VECTOR Y)
C
      DO 120 IT=1,19
      CT=CM(IT)
      ST=SM(IT)
      DO 110 IFI=1,IFIE
      CF=CM(IFI)
      SF=SM(IFI)
      STCF=ST*CF
      STSF=ST*SF
      PSUM=0.
C
C    SWITCH -GAUSS OR LORENTZ VARIANT
C    SUM OF THE POLE FIGURE CONTRIBUTIONS OF ALL
C    COMPONENTS AND THEIR EQUIVALENT POSITIONS IN SPMAS
C
      IF (IGL-1) 70,70,90
   70 DO 80 IJ=1,IJE
      IF (MGB(IJ).EQ.0) GO TO 80
      IF(IRED(IP).EQ.2) THEN
c   run from S-pole
        Z=-HX(IJ)*STCF-HY(IJ)*STSF+HZ(IJ)*CT
       ELSE
        Z=HX(IJ)*STCF+HY(IJ)*STSF+HZ(IJ)*CT
      ENDIF
      IREDH=IRED(IP)
      PSUM=PSUM+PSHG(Z,IK,IREDH)
   80 CONTINUE
      GO TO 110
   90 DO 100 IJ=1,IJE
      IF (MGB(IJ).EQ.0) GO TO 100
      Z=HL2(IK)*(HX(IJ)*STCF+HY(IJ)*STSF+HZ(IJ)*CT)
      PSUM=PSUM+PSHL(Z,IK)
  100 CONTINUE
  110 SPMAS(IT,IFI)=SPMAS(IT,IFI)+PSUM*HN(IK)
  120 CONTINUE
  130 CONTINUE
  198 NS=IIIIK-1
C
C    FIBRE COMPONENT-LOOP (IKF)
C
  131 IF (NF.EQ.0) GOTO 141
      DO 140 IKF=1,NF
      WRITE (0,62) IP,NS,IKF
      DO 150 JB=1,IZGB
      Z1(JB)=HXHF(JB,IKF)*XH + HYHF(JB,IKF)*YH + HZHF(JB,IKF)*ZH
      W1(JB) = 0.
      ZZ = 1.-Z1(JB)*Z1(JB)
      IF (ZZ.GT.1.E-20) W1(JB) = SQRT(ZZ)
  150 CONTINUE
C
C    TETA(IT)- AND FI(IFI)-LOOPS (VECTOR Y)
C
      DO 160 IT=1,19
      CT=CM(IT)
      ST=SM(IT)
      DO 160 IFI=1,IFIE
      CF=CM(IFI)
      SF=SM(IFI)
      STCF=ST*CF
      STSF=ST*SF
      DO 170 KA=1,IZGA
      Z2(KA) = HXYF(KA,IKF)*STCF + HYYF(KA,IKF)*STSF + HZYF(KA,IKF)*CT
      W2(KA) = 0.
      ZZ = 1.-Z2(KA)*Z2(KA)
      IF (ZZ.GT.1.E-20) W2(KA) = SQRT(ZZ)
  170 CONTINUE
      PSUM=0.
C
C    SWITCH - GAUSS OR LORENTZ VARIANT
C    SUM OF THE POLE FIGURE CONTRIBUTIONS OF ALL
C    COMPONENTS AND THEIR EQUIVALENT POSITIONS IN SPMAS
C
      IF (IGLF(IKF).EQ.2) GOTO 185
      DO 180 JB = 1,IZGB
      DO 180 KA = 1,IZGA
      ZZ = Z1(JB)*Z2(KA)
      WW = W1(JB)*W2(KA)
      PSUM = PSUM + PSGF(ZZ,WW,FS(IKF))
  180 CONTINUE
      GO TO 191
  185 DO 190 JB = 1,IZGB
      DO 190 KA = 1,IZGA
      ZZ = Z1(JB)*Z2(KA)
      WW = W1(JB)*W2(KA)
      PSUM = PSUM + PSLF(ZZ,WW,EPT2(IKF),ZT(IKF))
  190 CONTINUE
  191 CONTINUE
  160 SPMAS(IT,IFI) = SPMAS(IT,IFI) + PSUM*HNF(IKF)
  140 CONTINUE
C
C    END OF THE  FI-, TETA-, AND COMPONENT-LOOPS
C    DETERMINATION AND PRINT OF THE MAXIMUM AND MINIMUM VALUE
C    PRINT OF THE POLE FIGURE
C
  141 CALL PDRUCK(IP,XIPF,icount)
C
      write(*,*) ' Pole figure printout'
C    END OF THE POLE FIGURE-LOOP
C
      IF(ICHECK.EQ.0) THEN
        REWIND(8)
        READ(8,*)
        READ(8,*)
        READ(8,*)
        READ(8,*)
        READ(8,*)
        READ(8,*)
        READ(8,*)
      ENDIF
  230 CONTINUE
C
   62 FORMAT ('+Calculating pole figure',I3,3X,'spherical component',I3,
     * '  fibre component',I3)
   68 FORMAT (5X,'Error in opening file ',I2,15X,'Try again !')
   69 FORMAT (A20)
  240 FORMAT(A40)
  340 FORMAT(5X,'POSITION G0 : ALPHA0 = ',F7.3,
     1 3X,'BETA0 = ',F7.3,3X,'GAMMA0 = ',F7.3/
     2 20X,'  FI1 = ',F7.3,2X,'    FI = ',F7.3,
     3 4X,'  FI2 = ',F7.3/)
  400 FORMAT(3X,'SPHER.COMP.NR. = ',I2,1X,A7,
     1 '  B =',F5.2,'  S =',F7.2,'  T =',F9.4,
     2 ' HIN =',F6.4,/3X,75(1H-))
  401 FORMAT(3X,'FIBRE COMP.NR. = ',I2,1X,A7,
     1 '  B =',F5.2,'  S =',F7.2,'  T =',F9.4,
     2 ' HIN =',F6.4,/3X,75(1H-))
  402 FORMAT (5X,'FIBRE VECTOR IN KA :',
     * 'THETA = ',F7.3,10X,'X = ',F7.3/
     * 25X,'FI    = ',F7.3,10X,'Y = ',F7.3/
     * 50X,'Z = ',F7.3/)
  403 FORMAT (5X,'FIBRE VECTOR IN KB :',
     * 'THETA = ',F7.3,10X,'X = ',F7.3/
     * 25X,'FI    = ',F7.3,10X,'Y = ',F7.3/
     * 50X,'Z = ',F7.3/)

      close (9)
C
      OPEN (11,file='c:\beartex\status.tex')
1000  istatus=1
      write(11,*) istatus
      close (11)
      write(6,*) 'Successful end of STPF compilation!'
      if(imusic.eq.1) call shave1
      if(imusic.eq.2) call shave2
      STOP
      END
C
C
C   *********************************************
c
      subroutine setup(fname,weight,wgauss,imusic)
      character ititle*75,fname*75,fnamex*75,title*75,xtitle*75
      character xeuler*1
      dimension abc(6),weight(5,5000)
      common/inp/ fon,stand,weighmi,ieuler,ns,nf
      COMMON /TIT/ITITLE
      COMMON /LAT/ AA,BB,CC,ALPHA23,ALPHA31,ALPHA12,IGA,IGB
      write(*,*) '+++++++++++++++++++++++++++++++++++++++++++++'
      write(*,*)
      write(*,*) ' Program STPF'
      write(*,*) '   Based on Matthies/Vinel Program POLSSF'
      write(*,*) '   arranged for WIMDOWS and BEARTEX, 9.6.1994'
      write(*,*) '   Computes reduced and unreduced pole figures'
      write(*,*)
      write(*,*) '   Needs as input:'
      write(*,*) '      STPF.CFG with control data'
      write(*,*) '      fn.CPT for components'
      write(*,*) '   Output pole figures in new Berkeley format:'
      write(*,*) '      "fn.XPF" '
      write(*,*) '      "STPF.LST" for error messages etc.'
      write(*,*)
      open(5,file='c:\beartex\stpf.cfg')
      READ(5,*) imusic, xfactor
      read(5,'(1x,a)') ititle
      write(*,'(1x,a)') ititle
      read(5,'(a)') fname
      write(*,'(1x,a)') fname
c
      open(6,file='STPF.LST')
      iii=index(fname,'.')
      if(iii.gt.0) then
          fnamex=fname(1:iii-1)
         else
          iii=index(fname,' ')
        endif
      fnamex=fname(1:iii-1)//'.XPF'
      open(9,file=fnamex)
      write(*,'(1x,a,a)') ' Pole figures: ',fnamex
c
      read(5,*) igb,iga
      read(5,*) (abc(i),i=1,6)
      read(5,*) wgauss
c  open cpt file
      open(8,file=fname,err=10)
      read(8,'(a)') xtitle
      read(8,*)
      read(8,*)
      read(8,*)
      read(8,*) fon, stand, weighmi,ieuler
c  ieuler conforms with visual basic interface and
c  other programs: 0 open, 1 Roe, 2 Bunge, 3 Canova, 4 Kocks
      read(8,*) ns,nf
      read(8,'(a)') xeuler
      if (xeuler.eq.B) ieuler=2
      if (xeuler.eq.R) ieuler=1
      if (xeuler.eq.C) ieuler=3
      if (xeuler.eq.K) ieuler=4
cc
      write(6,*)' Program STPF: Pole figures from Standard Gaussians'
      write(6,*)
      write(6,*) ' Data on CFG file:'
      write(6,'(3x,2a)') ' Title on CPT file: ',ititle
      write(6,'(1x,a)')  ' filename: ',fname
      write(6,'(a,2i5)') ' igb,iga ', igb,iga
      write(6,'(6f10.3)')  (abc(i),i=1,6)
      write(6,'(a,f10.2)') ' Gauss width ', wgauss
      write(6,*)
      write(6,*) ' Information on CPT file:'
      write(6,'(3x,2a)') ' Title on CPT file:', xtitle
      write(6,'(a,3f8.2,i4)') ' fon,stand,weighmi,ieuler:'
     &   , fon, stand, weighmi,ieuler
      write(6,'(a,i5)') ' Number of components: ',ns
      write(6,'(2a)') ' Euler Code (this is used): ',xeuler
      write(6,*)

c
c     scan over data and renormalize weights:
      sumweigh=0.
      do 200 i=1,ns
      READ (8,205,END=210) xxx,xxx,xxx,weight(1,i)
  200 sumweigh=sumweigh+weight(1,i)
  205 format (3f8.2,f6.3)
      go to 220
  210 ns=i-1
  220 do 230 i=1,ns
  230 weight(1,i)=weight(1,i)*(1.-fon)/sumweigh
      rewind (8)
C
        aa=abc(1)
        bb=abc(2)
        cc=abc(3)
        alpha23=abc(4)
        alpha31=abc(5)
        alpha12=abc(6)
        return
   10   write(6,'(a)') ' Cannot open component file. STOP!'
        stop
        return
        end
c*******************************************************************
      subroutine transform(alpha0,beta0,gamma0,ieuler)
c  ieuler conforms conforms with Visual Basic with interface and
c  other programs: 0 open, 1 Roe, 2 Bunge, 3 Canova, Kocks 4
C
C  transform to Roe Matthies convention
c
      IF(IEULER.EQ.2) THEN  !Bunge
         ALPHA0= ALPHA0-90.
         GAMMA0= GAMMA0+90.
      ENDIF
      IF(IEULER.EQ.3) THEN   !Canova
         ALPHA0= 90.-ALPHA0
         BETA0  = -BETA0
         GAMMA0= 270-GAMMA0
      ENDIF
      IF(IEULER.EQ.4) THEN   ! Kocks
         GAMMA0= 180-GAMMA0
      ENDIF
      RETURN
      END
c*******************************************************************
C
      SUBROUTINE PDRUCK(IP,XIPF,icount)
C
C     PRINT OF A POLE FIGURE
C
      CHARACTER ITITLE*75,code*1
      DIMENSION SPMAS(19,73),IFIW(73),IPF(19,73),XIPF(19,73)
      DIMENSION TETAH(20),FIH(20),MILH(20),MILK(20),MILL(20)
      DIMENSION ICO(20), FRAC(20)
      COMMON /CSPMAS/ SPMAS
      COMMON /CIFIW/IFIW,IFIE,IOUT,IUNIT,ITM
      COMMON /CMIL/TETAH,FIH,XH,YH,ZH,MILH,MILK,MILL,ICO,FRAC
      COMMON /TIT/ITITLE
      COMMON /LAT/ AA,BB,CC,ALPHA23,ALPHA31,ALPHA12,IGA,IGB
C
      code='#'
cc      WRITE (IOUT,360) MILH(IP),MILK(IP),MILL(IP),
cc     1                 TETAH(IP),FIH(IP),XH,YH,ZH
C
C    DETERMINATION OF THE MINIMUM AND MAXIMUM
C
      PMAX=0.
      PMIN=8888.0
      DO 150 IT=1,19
      DO 150 IFI=1,IFIE
      PMM=SPMAS(IT,IFI)
      IF (PMM.LE.PMAX) GO TO 140
      PMAX=PMM
      TMAX=IFIW(IT)
      FIMAX=IFIW(IFI)
  140 IF (PMM.GE.PMIN) GO TO 150
      PMIN=PMM
      TMIN=IFIW(IT)
      FIMIN=IFIW(IFI)
  150 CONTINUE
      WRITE(6,433) MILH(IP),MILK(IP),MILL(IP)
      WRITE (*,440) PMIN,PMAX,TMIN,TMAX,FIMIN,FIMAX
      WRITE (6,440) PMIN,PMAX,TMIN,TMAX,FIMIN,FIMAX
C
C    PRINT OF THE POLE FIGURE
C
cc      IF(IFIE.NE.1)GOTO 161
cc      WRITE(IOUT,431)
cc      WRITE(IOUT,432)(SPMAS(IT,1),IT=1,19)
cc      IF(IUNIT.NE.9)RETURN
cc      WRITE(9,10)ITITLE
cc      WRITE(9,433)MILH(IP),MILK(IP),MILL(IP)
cc      WRITE(9,432)(SPMAS(IT,1),IT=1,19)
cc      RETURN
  161 IFE=1
  160 IFA=IFE
C     IFE=IFA+18
      IFE=IFA+ 9
      IF (IFE.GT.IFIE) IFE = IFIE
c     WRITE (IOUT,430) (IFIW(IFI),IFI=IFA,IFE)
cc    WRITE (IOUT,350) (IFIW(IT),(SPMAS(IT,LFI),LFI=IFA,IFE),
c c  1                  IT=1,19)
cc    WRITE(IOUT,*) 'FINISHED WITH MATTHIES OUTPUT',IP,ICO(IP)
      IF (IFE.LT.IFIE) GO TO 160
C
C  OUTPUT POLE FIGURES  FILE  FOR BERKELEY PLOT
C  IFIE=73 IS ASSUMED
C
cc    IF(IUNIT.NE.9.OR.IFIE.NE.73)GOTO 664
      write(*,*) 'Berkeley output'
      IF (icount.eq.ico(ip)) THEN
       IFE = IFIE-1
       TETAM = 5*(ITM-1)
       AMAX = TETAM
       FIM = 5*IFE
       BMAX = FIM
       DELA = 5.
       DELB = 5.
       amin=0.
       bmin=0.
       IW = 1
       WRITE(9,'(A75,4x,a1)')ITITLE,code

       write(*,*) ititle
       write(9,*)
       write(9,*)
       write(9,*)
       write(9,*)
       write(9,'(6f10.4,2i5)')AA,BB,CC,ALPHA23,ALPHA31,ALPHA12,IGB,IGA
       WRITE(9,'(1x,3i3,6f5.1,2i2)')MILH(IP),MILK(IP),MILL(IP),
     & AMIN,AMAX,DELA,BMIN,BMAX,DELB,IW,IW
       DO 25 J=1,19
       DO 25 I=1,IFE
       IPF(J,I)=(SPMAS(J,I)*FRAC(IP)+XIPF(J,I))*100+0.5
       XIPF(J,I)=0
   25  IF(IPF(J,I).GT.9999)IPF(J,I)=9999
       icount=1
       WRITE(9,30)((IPF(J,I),I=1,IFE),J=1,19)
       WRITE(9,*)
      ELSE
       DO 26 J=1,19
       DO 26 I=1,IFE
       XIPF(J,I)=SPMAS(J,I)*FRAC(IP)+XIPF(J,I)
   26  IF(XIPF(J,I).GT.9999)XIPF(J,I)=9999
       icount=icount+1
      ENDIF
  664 CONTINUE
      RETURN
   10 FORMAT(3X,A75)
   20 FORMAT(1X,3I1,1X,4F5.0,2I2)
   30 FORMAT(1X,18I4)
C 350 FORMAT(2X,I2,'|',19F6.2)
  350 FORMAT(2X,I2,'|',10F6.2)
  360 FORMAT(//15X,'POLE FIGUR  (',I2,',',I2,',',I2,')'/
     1 15X,22(1H-)/7X,' VECTOR HI IN KB'/,
     2 8X,'IN SPHER. ANGLES  : TETAH = ',F10.5,5X,'FIH = ',
     3 F10.5/,8X,'IN KART. COORD.   : XH = ',F10.5,
     4 '   YH = ',F10.5,'   ZH = ',F10.5/)
C 430 FORMAT(//2X,'FI>',1X,19(I5,1X)/1X,'THETA',113(1H-))
  430 FORMAT(//2X,'FI>',1X,10(I5,1X)/1X,'THETA',113(1H-))
  431 FORMAT(//2X,'FI = 0, THETA = 0(5)90 DEGREES')
  432 FORMAT(1X,10F6.2)
  433 FORMAT(15X,'POLE FIGURE (',I2,',',I2,',',I2,')')
  440 FORMAT(8X,'MINIMUM : PMIN  = ',F7.3,5X,
     1'MAXIMUM : PMAX  = ',F7.3/2(18X,'THETA = ',F4.0)
     2 /2(18X,'FI    = ',F4.0))
C
      END
C
C    *---------------------------------*
C
      FUNCTION PSHG (Z,IK,IRED)
C
C    CALCULATES (P(S,Z)+P(S,-Z))/2  - GAUSS STANDARD
C    REDUCING  EXP IS COMPENSATED IN PARGLP AND Bessel
C
      COMMON /PAR/B(10),S(10),T(10)
      ARG1M=S(IK)*(1.-Z)
      ARG2M=S(IK)*(1.+Z)
      IF (ARG1M-20.) 10,20,20
   10 ARG=ARG2M*0.5
      PS1H=EXP(-ARG1M)*Bessel(ARG,0)
      GO TO 30
   20 PS1H=0.
   30 IF(IRED.EQ.1) THEN
        PSHG=PS1H*2.
        RETURN
      ENDIF
      IF (ARG2M-20.) 40,50,50
   40 ARG=ARG1M*0.5
      PS2H=EXP(-ARG2M)*Bessel(ARG,0)
      GO TO 60
   50 PS2H=0.
   60 IF(IRED.EQ.2) THEN
        PSHG=PS2H*2.
        RETURN
      ENDIF
      PSHG=(PS1H+PS2H)
      RETURN
      END
C
C    *---------------------------------*
C
      FUNCTION PSHL (Z,IK)
C
C    CALCULATES (P(T,Z)+P(T,-Z))/2  - LORENTZ STANDARD
C
      COMMON /HMGL/HL1(10),HL2(10),HL3(10),HN(10)
      PS1H=HL3(IK)-Z
      PS1H=HL1(IK)/(PS1H*SQRT(PS1H))
      PS2H=HL3(IK)+Z
      PS2H=HL1(IK)/(PS2H*SQRT(PS2H))
      PSHL=PS1H+PS2H
      RETURN
      END
C
C    *------------------------------*
C
      FUNCTION PSGF (ZZ,WW,SF)
C
C    CALCULATION OF  P(SF,Z1,Z2)+P(SF,-Z1,Z2)  - GAUSS STANDARD FIBRE
C    REDUCING EXP IS COMPENSATED IN PARFP AND Bessel
C
      ARG1 = SF*( ZZ+WW-1.)
      ARG2 = SF*(-ZZ+WW-1.)
      P1 = 0.
      P2 = 0.
      IF (ARG1.GT.-20.) P1 = EXP(ARG1)
      IF (ARG2.GT.-20.) P2 = EXP(ARG2)
      PSGF = Bessel(SF*WW,0) * (P1+P2)
      RETURN
      END
C
C    *------------------------------*

C
      FUNCTION PSLF(ZZ,WW,EPT2,ZT)
C
C    CALCULATION OF  P(TF,Z1,Z2)+P(TF,-Z1,Z2)  - LORENTZ STANDARD FIBRE
C
      C1 = EPT2 - ZT*ZZ
      C2 = EPT2 + ZT*ZZ
      D = ZT*WW
      C1D = C1 + D
      C2D = C2 + D
      P1 = CEIS(2*D/C1D)/(C1-D)/SQRT(C1D)
      P2 = CEIS(2*D/C2D)/(C2-D)/SQRT(C2D)
      PSLF = P1+P2
      RETURN
      END
C
C
C    *------------------------------*
C
      SUBROUTINE COMPP (IK,IELAG)
C
C    SUBSIDIARY ROUTINE, ORGANISATION OF PARAMETERS
C
      DIMENSION SALFA(96),SBETA(96),SGAMMA(96),
     1          CALFA(96),CBETA(96),CGAMMA(96)
      COMMON /ABGSC/SALFA,SBETA,SGAMMA,CALFA,CBETA,CGAMMA
      COMMON /HMSC/HMSA(96,10),HMSB(96,10),HMSG(96,10),
     1             HMCA(96,10),HMCB(96,10),HMCG(96,10)
C
      DO 10 I=1,IELAG
      SALFA(I)=HMSA(I,IK)
      SBETA(I)=HMSB(I,IK)
      SGAMMA(I)=HMSG(I,IK)
      CALFA(I)=HMCA(I,IK)
      CBETA(I)=HMCB(I,IK)
   10 CGAMMA(I)=HMCG(I,IK)
      RETURN
      END
C
C    *---------------------------------*
C
      SUBROUTINE PARGLP (IK)
C
C    CALCULATION OF WORKING PARAMETERS FOR GAUSS OR
C    LORENTZ VARIANTS OF STANDARD POLE FIGURES
C
      COMMON /HGL/BST(10),HIN(10),IGALO(10),IBST(10)
      COMMON /HMGL/HL1(10),HL2(10),HL3(10),HN(10)
      COMMON /PAR/B(10),S(10),T(10)
      COMMON /GE/IZGA,IZGB
      COMMON /CPI/PI,PIF
      ZGE=IZGA*IZGB
      IBS=IBST(IK)
      IGL=IGALO(IK)
      IF (IGL-1) 10,10,70
   10 T(IK)=8888.
      IF (IBS) 50,20,50
   20 S(IK)=BST(IK)
      B(IK)=8888.
      SGRENZ=ALOG(2.)/2.
      IF (S(IK)-SGRENZ) 60,30,40
   30 B(IK)=360.
      GO TO 60
   40 ARGH=SQRT(SGRENZ/S(IK))
      B(IK)=720.*ASIN(ARGH)/PI
      GO TO 60
   50 B(IK)=BST(IK)
      BR4=B(IK)*PI/720.
      S(IK)=ALOG(2.)/(2.*SIN(BR4)**2)
   60 HN(IK)=0.5*HIN(IK)/(Bessel(S(IK),0)-Bessel(S(IK),1))/ZGE
      RETURN
   70 S(IK)=8888.
      IF (IBS) 80,110,110
   80 B(IK)=8888.
      T(IK)=BST(IK)
      TAUG=0.5*(5.+SQRT(17.))
      TGRENZ=SQRT(TAUG)-SQRT(TAUG-1.)
      IF (T(IK)-TGRENZ) 120,90,100
   90 B(IK)=360.
      GO TO 120
  100 TAU=((1+T(IK)*T(IK))**2)/(4*T(IK)*T(IK))
      TAU2=TAU*TAU
      TAU3=TAU*TAU2
      TAU4=TAU*TAU3
      ARGH=5.*TAU4-8.*TAU3+2*TAU2+1
      C=((2*TAU2-TAU+1)-SQRT(ARGH))/(1.+TAU)
      SC=SQRT(C)
      B(IK)=720*ACOS(SC)/PI
      GO TO 120
  110 B(IK)=BST(IK)
      BR4=B(IK)*PI/720.
      C=COS(BR4)**2
      AC=19.*C**2-34.*C+19.
      HG=(-82.*C**3+240.*C**2-246.*C+80.)/AC**1.5
      HG=ACOS(HG)/3.
      TAU=(2.*SQRT(AC)*COS(HG)+(5.-4.*C))/3.
      AC=TAU-1.
      T(IK)=SQRT(TAU)-SQRT(AC)
  120 TH4=T(IK)**4
      TH2M2=2.*T(IK)**2
      AC=1.-TH4
      HN(IK)=0.5*HIN(IK)/ZGE
      HL1(IK)=AC
      HL2(IK)=TH2M2
      HL3(IK)=1.+TH4
      RETURN
      END
C
C    *--------------------------------*
C
      SUBROUTINE PARFP (IKF)
C
C     CALCULATES GAUSSIAN AND LORENTZIAN WORKING PARAMETERS
C     FOR FIBRE COMPONENTS
C
      COMMON /CPI/PI,PIF
      COMMON /PARF/FB(5),FS(5),FT(5)
      COMMON /HMPF/HNF(5),EPT2(5),ZT(5)
      COMMON /CYHF/BSTF(5),HINF(5),IGLF(5),IBSTF(5)
      COMMON /GE/IZGA,IZGB
      NAB = IZGA*IZGB
      PIF = 180./PI
      IBST = IBSTF(IKF)
      BST = BSTF(IKF)
      IGL = IGLF(IKF)
      HIN = HINF(IKF)
      IF (IGL-1) 10,10,70
   10 IF (IBST) 50,20,50
   20 S=BST
      T=8888.
      B=8888.
      SGRENZ=ALOG(2.)/2.
      IF (S-SGRENZ) 60,30,40
   30 B=360.
      GO TO 60
   40 ARGH=SQRT(SGRENZ/S)
      B=4.*ASIN(ARGH)*PIF
      GO TO  60
   50 T=8888.
      B=BST
      BR4=B/(4.*PIF)
      S=ALOG(2.)/(2.*SIN(BR4)**2)
   60 HNFUK = HIN/(NAB+0.)
      IF (S.LT.1.E-10) GOTO 65
      HNFUK = HNFUK*S
      IF (S.LT.20.) HNFUK = HNFUK/(1.-EXP(-2.*S))
      GOTO 130
   65 hnfuk = 0.5*hnfuk
      goto 130
   70 Z23 = 4.**(1./3.)
      RN = Z23-1.
      IF (IBST) 80,110,110
   80 B=8888.
      T=BST
      S=8888.
      TGRENZ = (Z23+1.-Z23*Z23)/RN
      IF (T-TGRENZ) 120,90,100
   90 B=360.
      GO TO 120
  100 R = 0.5*(1.+T*T)/T
      B = 2.*PIF*ACOS(RN*R+Z23)
      GO TO 120
  110 B=BST
      S=8888.
      BR2=B/(2.*PIF)
      R = (Z23-COS(BR2))/RN
      T = R-SQRT(R*R-1.)
  120 T2=T*T
      HNFUK=HIN*(1.-T2)/(NAB+0.)/PI
      EPT2(IKF) = 1.+T2
      ZT(IKF)   = 2.*T
  130 HNF(IKF)=HNFUK
      FB(IKF) = B
      FS(IKF) = S
      FT(IKF) = T
      RETURN
      END
C
C
C    *------------------------------*
C
      FUNCTION Bessel (XX,IND)
C
C    CALCULATES BESSELFUNCTIONS I0(XX)*EXP(-XX) FOR IND=0
C    RESPECTIVELY I1(XX)*EXP(-XX) FOR IND=1
C    XX GE.NULL
C
      DOUBLE PRECISION A,B,C,D
      DIMENSION A(14),B(10),C(14),D(10)
      DATA A/
     1 0.926522283D-27,0.665920310D-25,0.187543306D-21,
     2 0.678818504D-19,0.293709299D-16,0.936223818D-14,0.240378975D-11,
     3 0.470922041D-09,0.678173901D-7,0.678167711D-5,0.434027830D-3,
     4 0.156249998D-1,0.25D+00,0.1D+1/,
     5 B/
     6 0.228201527D-7,0.460958396D-7,0.542737131D-7,0.204865735D-6,
     7 0.911964648D-6,0.448357120D-5,0.292186032D-4,0.280504770D-3,
     8 0.498677852D-2,0.398942281D+0/,
     9 C/
     X 0.312719049D-28,0.378500541D-26,0.744839607D-23,0.314048171D-20,
     X 0.146127307D-17,0.520686381D-15,0.150206750D-12,0.336384070D-10,
     X 0.565142090D-8,0.678168183D-6,0.542534739D-4,0.260416666D-2,
     X 0.625D-1,0.5D+00/,
     X D/
     X  -0.250141085D-7,-0.513119520D-7,-0.637466188D-7,-0.244333379D-6,
     X  -0.111375439D-5,-0.576272322D-5,-0.409063053D-4,-0.467509130D-3,
     X -0.149603355D-1,+0.398942281D+00/
      IF (IND.EQ.0) GO TO 50
      X=XX
      IF (ABS(X)-10.) 10,30,30
   10 FI2=C(1)
      V=X**2
      DO 20 L=1,13
   20 FI2=FI2*V+C(L+1)
      Bessel=FI2*X*EXP(-X)
      RETURN
   30 FI3=D(1)
      X=ABS(X)
      Y=10./X
      DO 40 L=1,9
   40 FI3=FI3*Y+D(L+1)
      Bessel=FI3/SQRT(X)
      IF (XX.LT.0.) Bessel=-Bessel
      RETURN
   50 X=ABS(XX)
      IF (X-10.) 60,80,80
   60 FI=A(1)
      U=X**2
      DO 70 K=1,13
   70 FI=FI*U+A(K+1)
      Bessel=FI*EXP(-X)
      RETURN
   80 FI1=B(1)
      Y=10./X
      DO 90 K=1,9
   90 FI1=FI1*Y+B(K+1)
      Bessel=FI1/SQRT(X)
      RETURN
      END
C
C    *--------------------------------*
C
      SUBROUTINE SC48 (CAN,SAN,CBN,SBN,CGN,SGN)
C
C    INPUT COS,SIN (ALPHA0,BETA0,GAMMA0) OF G0
C    CALCULATION OF SIN COS (ALPHA,BETA,GAMMA) FOR
C    THE 48 HEXAGONAL-ORTHORHOMBIC EQUIVALENT POSITIONS
C
      DIMENSION SALFA(96),SBETA(96),SGAMMA(96),
     1          CALFA(96),CBETA(96),CGAMMA(96)
      COMMON /ABGSC/ SALFA,SBETA,SGAMMA,CALFA,CBETA,CGAMMA
      DO 10 I=1,48
   10 SBETA(I)=SBN
      DO 20 I=1,6
      SALFA(I)=SAN
      CALFA(I)=CAN
   20 CBETA(I)=CBN
      SGAMMA(1)=SGN
      CGAMMA(1)=CGN
      S3=SQRT(3.)
      SGAMMA(2)=(S3*CGN+SGN)/2.
      SGAMMA(3)=(S3*CGN-SGN)/2.
      CGAMMA(2)=(CGN-S3*SGN)/2.
      CGAMMA(3)=(-CGN-S3*SGN)/2.
      DO 30 I=4,6
      SGAMMA(I)=-SGAMMA(I-3)
   30 CGAMMA(I)=-CGAMMA(I-3)
      DO 40 J=1,7
      DO 40 I=1,6
      K=I+J*6
      ISA=(-1)**((J+1)/2)
      ISG=(-1)**(J/4)
      ICA=ISA*ISG
      ICBG=(-1)**(5*J/4)
      SALFA(K)=ISA*SALFA(I)
      CALFA(K)=ICA*CALFA(I)
      CBETA(K)=ICBG*CBETA(I)
      SGAMMA(K)=ISG*SGAMMA(I)
      CGAMMA(K)=ICBG*CGAMMA(I)
   40 CONTINUE
      RETURN
      END
C
C    *--------------------------------*
C
      SUBROUTINE SC96 (CAN,SAN,CBN,SBN,CGN,SGN)
C
C    INPUT COS,SIN (ALPHA0,BETA0,GAMMA0) OF G0
C    CALCULATION OF SIN COS (ALPHA,BETA,GAMMA) FOR
C    THE 96 CUBIC-ORTHORHOMBIC EQUIVALENT POSITIONS
C
      DIMENSION SALFA(96),SBETA(96),SGAMMA(96),
     1          CALFA(96),CBETA(96),CGAMMA(96)
      COMMON /ABGSC/SALFA,SBETA,SGAMMA,CALFA,CBETA,CGAMMA
C
      GKLEIN=1.E-16
      SALFA(1)=SAN
      CALFA(1)=CAN
      SBETA(1)=SBN
      CBETA(1)=CBN
      SGAMMA(1)=SGN
      CGAMMA(1)=CGN
C
C    DETERMINATION ALPHAA,BETAA,GAMMAA
C
      HILFSG=SBN*SGN
      CBETA(2)=HILFSG
      S2=1.0-HILFSG*HILFSG
      IF (S2.GT.0.) GO TO 10
      S2=0.
   10 HILFSG=SQRT(S2)
      SBETA(2)=HILFSG
      IF (HILFSG.LT.GKLEIN) GO TO 20
      CGAMMA(2)=CBN/HILFSG
      SGAMMA(2)=SBN*CGN/HILFSG
      CALFA(2)=-(CAN*CBN*SGN+SAN*CGN)/HILFSG
      SALFA(2)=(-SAN*CBN*SGN+CAN*CGN)/HILFSG
      GO TO 30
   20 CGAMMA(2)=1.
      SGAMMA(2)=0.
      CALFA(2)=CAN*SBN
      SALFA(2)=SAN*SBN
      IF (CBETA(2).LT.0.) GO TO 30
      CALFA(2)=-CALFA(2)
      SALFA(2)=-SALFA(2)
C
C    DETERMINATION ALPHAB,BETAB,GAMMAB
C
   30 HILFSG=SBN*CGN
      CBETA(3)=HILFSG
      S2=1.-HILFSG*HILFSG
      IF (S2.GT.0.) GO TO 40
      S2=0.
   40 HILFSG=SQRT(S2)
      SBETA(3)=HILFSG
      IF (HILFSG.LT.GKLEIN) GO TO 50
      CGAMMA(3)=SBN*SGN/HILFSG
      SGAMMA(3)=CBN/HILFSG
      CALFA(3)=-(CAN*CBN*CGN-SAN*SGN)/HILFSG
      SALFA(3)=-(SAN*CBN*CGN+CAN*SGN)/HILFSG
      GO TO 60
   50 CGAMMA(3)=1.
      SGAMMA(3)=0.
      CALFA(3)=CAN*CBN*SGN+SAN*CGN
      SALFA(3)=SAN*CBN*SGN-CAN*CGN
      IF (CBETA(3).GT.0.) GO TO 60
      CALFA(3)=-CALFA(3)
      SALFA(3)=-SALFA(3)
C
C    CYCLES OF SIGNS FOR THE 96 CASES
C
   60 DO 100 I=1,3
      DO 90 K=1,8
      J=(K-1)*3
      IK11=(-1)**(K+1)
      IK02=(-1)**(K/2)
      DO 80 L=1,4
      M=(L-1)*24
      SBETA(M+J+I)=SBETA(I)
      IL11=(-1)**(L+1)
      IL02=(-1)**(L/2)
      SALFA(M+J+I)=IL11*IK11*SALFA(I)
      CALFA(M+J+I)=IL02*IK11*CALFA(I)
      IL32=(-1)**((L+3)/2)
      CBETA(M+J+I)=IL32*IK11*CBETA(I)
      IF (IK02.GT.0) GO TO 70
      IK54=(-1)**((K+5)/4)
      IK34=(-1)**((K+3)/4)
      SGAMMA(M+J+I)=IK54*IL32*CGAMMA(I)
      CGAMMA(M+J+I)=IK34*IL32*SGAMMA(I)
      GO TO 80
   70 IK74=(-1)**((K+7)/4)
      IK14=(-1)**((K+1)/4)
      SGAMMA(M+J+I)=IK14*IL32*SGAMMA(I)
      CGAMMA(M+J+I)=IK74*IL32*CGAMMA(I)
   80 CONTINUE
   90 CONTINUE
  100 CONTINUE
      RETURN
      END
C
C    *--------------------------------*
C
      SUBROUTINE VXYZ24 (GXN, GYN, GZN)
C
C    CALCULATION OF 24 CUBIC SYMM. EQUIVALENT VECTORS
C
      COMMON /XYZ/ X(24),Y(24),Z(24)
      X(1)=GXN
      Y(1)=GYN
      Z(1)=GZN
      X(2)=-GZN
      Y(2)=-GXN
      Z(2)=GYN
      X(3)=-GYN
      Y(3)=GZN
      Z(3)=-GXN
C
      DO 10 I=4,6
      II=I-3
      X(I)=Y(II)
      Y(I)=X(II)
   10 Z(I)=-Z(II)
C
      DO 20 J=7,12
      J10=0.1*J
      JJ=J-3-6*J10
      X(J)=X(JJ)
      Y(J)=-Y(JJ)
   20 Z(J)=Z(J-6)
C
      DO 30 K=13,24
      KK=K-12
      X(K)=-X(KK)
      Y(K)=-Y(KK)
   30 Z(K)=Z(KK)
C
      RETURN
      END
C
C    *--------------------------------*
C
      SUBROUTINE VXYZ12 (GXN, GYN, GZN)
C
C    CALCULATION OF 12 HEXAGONAL SYMM. EQUIVALENT VECTORS
C
      COMMON /XYZ/ X(24),Y(24),Z(24)
      S3=SQRT(3.)
      X(1)=GXN
      Y(1)=GYN
C
      DO 10 I=1,6
   10 Z(I)=GZN
C
      X(2)=0.5*(GXN+S3*GYN)
      Y(2)=0.5*(GYN-S3*GXN)
      X(3)=0.5*(-GXN+S3*GYN)
      Y(3)=0.5*(-GYN-S3*GXN)
C
      DO 20 J=4,6
      JJ=J-3
      X(J)=-X(JJ)
   20 Y(J)=-Y(JJ)
C
      DO 30 K=7,12
      KK=K-6
      X(K)=-X(KK)
      Y(K)=Y(KK)
   30 Z(K)=-GZN
      RETURN
      END
C
C    *--------------------------------*
C
      SUBROUTINE VECTOR (THETA,FI,X,Y,Z)
C
C    SPHERICAL COORD. ---> CARTHESIAN COORD.
C
      COMMON /CPI/PI,PIF
      PIF = 180./PI
      PIT = THETA/PIF
      FIT = FI/PIF
      Z = COS(PIT)
      ST = SIN(PIT)
      Y = SIN(FIT)*ST
      X = COS(FIT)*ST
      RETURN
      END
C
C    *--------------------------------*
C
      FUNCTION CEIS(X)
C
C    CALCULATION OF THE COMPLETE ELLIPTIC INTEGRAL
C    OF THE SECOND KIND E(M)
C
C    INPUT (0.LE.X.LE.1.)
C
      DIMENSION A(4),B(4)
      COMMON /I/ IOUT
      DATA A/
     1 0.44325141463, 0.06260601220,
     2 0.04757383546, 0.01736506451/,
     3 B/
     4 0.24998368310, 0.09200180037,
     5 0.04069697526, 0.00526449639/
      IF ((0.0.GT.X). OR .(X.GT.1.)) GOTO 66
      X1=1.-X
      IF (X1.LT.1.E-09) GOTO 77
      SA=0.
      DO 10 I=1,4
      SA=SA+A(5-I)
      SA=SA*X1
   10 CONTINUE
      SB=0.
      DO 20 K=1,4
      SB=SB+B(5-K)
      SB=SB*X1
   20 CONTINUE
      FAK=ALOG(1./X1)
      CEIS=1.+SA+SB*FAK
      RETURN
   77 CEIS=1.
      RETURN
   66 WRITE (IOUT,100) X
  100 FORMAT (/' INPUT X= ',D15.8,' DOES NOT ',
     1 'SATISFY THE INPUT CONDITION (O.LE.X.LE.1.)!'/)
      CEIS=8888.
      RETURN
      END
C    *--------------------------------*
C
      SUBROUTINE LATTICE
C
C     INPUT : Lattice parameters necessary for Theta,Phi calculation
C     of (HKL)-directions
C
      COMMON /CSCL/S12L,C12L,S23L,C23L,S31L,C31L
      COMMON /CABC/CDA,CDB
      COMMON /CPI/PI,FAK
      COMMON /LAT/ AA,BB,CC,ALPHA23,ALPHA31,ALPHA12,IGA,IGB
C
      ARG=ALPHA12*FAK
      S12L=SIN(ARG)
      C12L=COS(ARG)
      ARG=ALPHA23*FAK
      S23L=SIN(ARG)
      C23L=COS(ARG)
      ARG=ALPHA31*FAK
      S31L=SIN(ARG)
      C31L=COS(ARG)
      CDA=CC/AA
      CDB=CC/BB
      WRITE(6,40)S12L,C12L,S23L,C23L,S31L,C31L,CDA,CDB
  40  FORMAT(/,'  Lattice parameters :'/,
     W  '  SIN(Alpha12) = ',F7.4,'  COS(Alpha12) = ',F7.4,/
     W  '  SIN(Alpha23) = ',F7.4,'  COS(Alpha23) = ',F7.4,/
     W  '  SIN(Alpha31) = ',F7.4,'  COS(Alpha31) = ',F7.4,/
     W  '  c/a = ',E12.5,'  c/b = ',E12.5,/)
C
      RETURN
      END
C
C    *--------------------------------*
C
      SUBROUTINE TFHKL(IH,K,L,CF,SF,CT,ST)
C
C     Theta,Phi calculation for given (H,K,L) and Lattice parameters
C
      COMMON /CSCL/S12L,C12L,S23L,C23L,S31L,C31L
      COMMON /CABC/CDA,CDB
      Q = 1.+2.*C12L*C31L*C23L-C12L*C12L-C23L*C23L-C31L*C31L
      Q = SQRT(Q)
      X = CDA*IH-C31L*L
      Y = (C31L*C23L-C12L)*X
      Y = Y + S31L*S31L*(CDB*K-C23L*L)
      Y = Y/Q
      Z = S31L*L
      R = X*X+Y*Y+Z*Z
      R = SQRT(R)
      X = X/R
      Z = Z/R
      ST=0.
      CT=1.
      SF=0.
      CF=1.
      IF(ABS(Z).GE.1.)GOTO 333
      CT=Z
      TR=ACOS(Z)
      ST=SIN(TR)
      ARG=X/ST
      IF(ABS(ARG).GE.1.)GOTO 33
      FR=ACOS(ARG)
      SF=SIN(FR)
      IF(Y.LT.0.)SF=-SF
      CF=ARG
      RETURN
  33  CF=ARG/ABS(ARG)
      RETURN
  333 IF(Z.LT.0.)CT=-1.
      RETURN
      END
C
c*****************************************************************
        subroutine shave1
c
c  This routine will play "Fifth".
c
        implicit none
        INCLUDE 'FLIB.FD'
c
c  Local variables
        integer*4 c1,d1,e1,f1,g1,a1,b1,c2,d2,e2,f2,g2,a2,b2
        integer*4 sixteen, eighth, quarter, half, whole
        integer*4 eightdot, quartdot
        integer*4 maxnotes
c
        parameter (c2 = 1046)
        parameter (e2 = 1318)
c
        parameter (eighth  = 125*20)
        parameter (half    = 500*20)
        parameter (maxnotes = 4)
c
        integer*4 note, frequency, duration
        integer*4 marseil(maxnotes*2)
        integer*2 i
c
        data marseil /
     &   e2, eighth,
     &   e2, eighth,
     &   e2, eighth,
     &   c2, half/
c
c  Loop through and play notes
c
        note = 1
        do 1000 i=1,maxnotes
        frequency = marseil(note)
        note = note + 1
        duration = marseil(note)
        note = note + 1
        call beepqq(duration,frequency)
1000    continue
c
        return
        end
c*****************************************************************
        subroutine shave2
c
c  This routine will play "Marseilleaise".
c
        implicit none
        INCLUDE 'FLIB.FD'
c
c
c  Local variables
c
        integer*4 c1,d1,e1,f1,g1,a1,b1,c2,d2,e2,f2,g2,a2,b2
        integer*4 sixteen, eighth, quarter, half, whole
        integer*4 eightdot, quartdot
        integer*4 maxnotes
c
        parameter (c1 = 523)
        parameter (d1 = 587)
        parameter (e1 = 659)
        parameter (f1 = 698)
        parameter (g1 = 784)
        parameter (a1 = 880)
        parameter (b1 = 988)
        parameter (c2 = 1046)
        parameter (d2 = 1174)
        parameter (e2 = 1318)
        parameter (f2 = 1396)
        parameter (g2 = 1568)
        parameter (a2 = 1760)
        parameter (b2 = 1976)
c
        parameter (sixteen = 62*20)
        parameter (eighth  = 125*20)
        parameter (eightdot= 188*20)
        parameter (quarter = 250*20)
        parameter (quartdot= 375*20)
        parameter (half    = 500*20)
        parameter (whole   = 1000*20)
        parameter (maxnotes = 10)
c
        integer*4 note, frequency, duration
        integer*4 marseil(maxnotes*2)
        integer*2 i
c
        data marseil /
     &   g1, eighth,
     &   g1, eightdot,
     &   g1, sixteen,
     &   c2, quarter,
     &   c2, quarter,
     &   d2, quarter,
     &   d2, quarter,
     &   g2, quartdot,
     &   e2, eighth,
     &   c2, eightdot/
c
c  Loop through and play notes
c
        note = 1
        do 1000 i=1,maxnotes
        frequency = marseil(note)
        note = note + 1
        duration = marseil(note)
        note = note + 1
        call beepqq(duration,frequency)
1000    continue
c
        return
        end

*/

/*


C
C    *--------------------------------*
C
C
C    PROGRAM  ODFSF      FILENAME ODFSF.FOR
C    MATTHIES/VINEL ROSSENDORF
C    PC VERSION 12/92 Dresden (BERKELEY VARIANT)
C
C    In Subroutine PRI by "hormfak" the plotfile normalization
C    can be determined
C
C    COMPUTES THE ODF F(G) AND/OR THE REDUCED ODF FS(G) (F-TILDE
C    OR 'SCHLANGE' -SNAKE) IN A 5-DEGREE GRID
C    (G = {ALPHA,BETA,GAMMA} USED FOR ALL CALCULATIONS)
C    BY STANDARD FUNCTIONS (GAUSS OR LORENTZ SHAPE)  FOR
C    SPHERICAL AND FIBRE COMPONENTS AND POSSIBLE
C
C    - SAMPLE SYMMETRY  :
C                         ROTATION GROUP GA   CODE NUMBER IGA
C        ORTHORHOMBIC             D2               3
C        MONOCLINIC               C2               2
C        TRICLINIC                C1               1
C    - CRYSTAL SYMMETRY :
C                         ROTATION GROUP GB   CODE NUMBER IGB
C                                 O                7
C                                 T                6
C                                 D4               5
C                                 C4               4
C                                 D2               3
C                                 C2               2
C                                 C1               1
C                                 D6              11
C                                 C6              10
C                                 D3               9
C                                 C3               8
C    ITYP   - FOR OUTPUT SECTION: GAMMA (3),FI2 (1),FI1 (2),ALPHA (4)
C    ITITLE - TEXT ARRAY FOR FREE USE (A40)
C    N      - NUMBER OF TEXTUR COMPONENTS TO BE MIXED (LE.50)
C    NF     - NUMBER OF FIBRE  COMPONENTS TO BE MIXED (LE.5)
C    FON    - BACKGROUND (0.LE.FON.LT.1)
C    EVERY SPHERICAL COMPONENT IS DESCRIBED BY ITS
C    ORIENTATION (POSITION) GO (ALPHA0,BETA0,GAMMA0) IN DEGREES
C    AND BY THE PARAMETERS :
C    IBST,BST (B,S,T), HIN (TOTAL INTENSITY,0.LT.HIN.LE.1), IGL
C    EVERY FIBRE COMPONENT IS DESCRIBED BY
C    ITS AXIS (VECTOR Y REG.KA, H REG.KB IN SPHER. COORD.) IN DEGREES
C    AND BY THE PARAMETERS :
C    IBST,BST (B,S,T), HIN (TOTAL INTENSITY,0.LT.HIN.LE.1), IGL
C        IBST = 0 HALFWIDTH GIVEN BY S (0.LE.S.LT.INFINITE)
C        IBST = 1 HALFWIDTH GIVEN BY B IN DEGREES (0.LT.B.LE.360)
C        IBST =-1 HALFWIDTH GIVEN BY T (0.LE.T.LT.1)
C        IGL  = 1 GAUSS-SHAPED CURVE (B OR S INPUT IS POSSIBLE)
C        IGL  = 2 LORENTZ-SHAPED CURVE (B OR T INPUT IS POSSIBLE)
C    CALCULATION VARIANTS  :
C        IRED = 0 F(G) ODF - UNREDUCED ODF
C        IRED = 1 FS(G) ODFS - REDUCED ODF
C
C ********************
C
C    BERKELEY PLOTFILE STRUCTURE : see Subroutine PRI
C
C        ***********
C
C        INPUT DATA:
C        ITITLE                   FORMAT(A40)
C        ITYP                     FORMAT(I*)
C        IGA,IGB                  FORMAT(2I*)
C        IRED,N,NF                FORMAT(3I*)
C        FON                      FORMAT(F*)
C        SPHERICAL COMPONENT DATA N TIMES TWO SETS :
C        ALPHA0,BETA0,GAMMA0      FORMAT(3F*)
C        IBST,BST,HIN,IGL         FORMAT(I*,2F*,I*)
C        FIBRE COMPONENT DATA     NF TIMES TWO SETS :
C        TEYF,FIYF,TEHF,FIHF      FORMAT(4F*)
C        IBST,BST,HIN,IGL         FORMAT(I*,2F*,I*)
C
C        ************
C
C        FILE ORGANISATION
C        UNIT 5   INPUT DATA
C             6   LIST OUTPUT (INPUT PROTOCOL + GAMMA-SECTIONS)
C             9   LIST OUTPUT  : ODF OR ODFS - ITYP-SECTIONS
C            10   PLOT-FILE  : ODF OR ODFS - ITYP-SECTIONS (BERKELEY FORMAT)
C
C        ************
C
C    FOR THE PRESCRIPTION HOW THE CRYSTAL COORDINATE
C    SYSTEM KB IS DEFINED SEE TABLES  5.1, 14.1
C    AND FIG. 5.1-5.6, 5.9-5.12
C
C    ATTENTION. The array F(197173) is necessary for the variable output
C    of ityp-sections, if all variants of symmetry will be permitted.
C    This DIMENSION needs a LAHEY FORTRAN COMPILER. The DIMENSION may be
C    reduced e.g. 51311 in order to work in the common DOS regions for RAM.
C    But then the limitation 51311 excludes the symmetry variants GB/GA:
C    C2/C1, C1/C1,  C1/C2,  C3/C1.
C
C    ATTENTION. IN ORDER TO GET NORMALIZED ODF'S
C    FON + SUM OF THE N HIN = 1 MUST BE VALID
C
C    ATTENTION. FOR TYPE 3 OF CRYSTAL SYMMETRY
C    F(G) HAS TO BE CALCULATED WITH GB
C    THE REDUCED ODF FS(G) HAS TO BE CALCULATED
C    WITH THE ROTATION GROUP GBS (TABLE 14.1)
C
C    IT IS TO RECOMMEND TO ARRANGE GAUSSIAN COMPONENTS
C    WITH SAME HALFWIDTHS ONE AFTER ANOTHER
C
      CHARACTER SHAPE(2)*7,VARIANT(3)*15,FNAME*20,IGNAME(11)*4
      CHARACTER STYP (4)*5
      CHARACTER ITITLE*40
      DIMENSION SALFA(96),SBETA(96),SGAMMA(96),
     1          CALFA(96),CBETA(96),CGAMMA(96)
      DIMENSION SR(73),CR(73)
      DIMENSION FUK(73,37),FUM(73,37),F(197173)
      DIMENSION FGM(183),IFIW(73)
      DIMENSION MGE(11,25),NYZGB(11,2)
      COMMON /HMSC/HMSA(96,50),HMSB(96,50),HMSG(96,50),
     1             HMCA(96,50),HMCB(96,50),HMCG(96,50)
      COMMON /ABGSC/SALFA,SBETA,SGAMMA,CALFA,CBETA,CGAMMA
      COMMON /GEIST/FGM
      COMMON/HTAB/HMFGM(183,50)
      COMMON /SICO/SG,CG,SB,CB,SA,CA
      COMMON /CPI/PI
      COMMON /PAR/B,BST,SUM,SUM1,S,T,TAU,TAUS,TAUSS,
     1            TAUSSS,HNFUK,HNFUM
      COMMON /HMPAR/HB(50),HS(50),HT(50),HMNFUK(50),
     1              HMNFUM(50),HTAU(4,50),IHIGL(50)
      COMMON /HVEC/XHF(24),YHF(24),ZHF(24),
     *             XYF(4),YYF(4),ZYF(4)
      COMMON /HMXYZY/HXYF(4,5),HYYF(4,5),HZYF(4,5)
      COMMON /HMXYZH/HXHF(24,5),HYHF(24,5),HZHF(24,5)
      COMMON /PARFIB/BF,SF,TF,SUMF,SUMF1,EPT2,ZT,HNFUKF,HNFUMF
      COMMON /HMPARF/HSF(5),HTF(5),HFUKF(5),
     1               HTFIB(2,5),IGLF(5)
      COMMON /XYZ/ X(24),Y(24),Z(24)
      COMMON /CF/FUK,FUM,F
      COMMON /CDIM/ NYZA,NYZB,NYZG,NYE
      COMMON /CIFIW/IFIW,IOUT,IUNIT,IUNIT1
      COMMON /BSA/BANF,SANF
      COMMON /CMGB/MGB(96)
      COMMON /TIT/ITITLE
      COMMON /CMIMA/ FMIN,FMAX,AMIN,AMAX,BMIN,BMAX,SIMIN,SIMAX
C     DATA MGE/
C    1 1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,0,0,1,1,
C    2 0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,0,1,0,0,1,1,
C    3 0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,1,0,0,1,1,
C    4 0,0,0,1,1,0,1,0,0,0,1,0,0,0,0,0,0,1,0,1,0,1,
C    5 0,0,0,0,0,0,1,0,0,0,1,0,0,1,0,1,1,1,0,1,0,1,
C    6 0,0,0,0,0,1,1,0,0,0,1,0,0,0,0,0,1,1,0,1,0,1,
C    7 0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,
C    8 0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,
C    9 0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,
C    X 0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,
C    X 0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,1,1,1,0,0,0,0,
C    X 0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,
C    X 1,2,4,4,8,12,24,3,6,6,12/
      DATA MGE/11*1,5*0,2*1,2*0,2*1,5*0,6*1,4*0,1,0,1,0,0,1,1,
     1 6*0,5*1,6*0,1,0,0,1,1,3*0,1,1,0,1,3*0,1,6*0,1,0,1,0,1,
     2 6*0,1,3*0,1,0,0,1,0,3*1,0,1,0,1,5*0,1,1,3*0,1,5*0,1,1,0,1,0,1,
     3 0,6*1,9*0,1,1,9*0,1,1,8*0,1,0,1,10*0,1,10*0,1,7*0,1,1,0,1,
     4 10*0,1,10*0,1,6*0,1,0,3*1,9*0,1,1,9*0,1,1,4*0,
     5 1,2,4,4,8,12,24,3,6,6,12/
      DATA IGNAME/
     1 '(C1)','(C2)','(D2)','(C4)','(D4)','( T)','( O)',
     2 '(C3)','(D3)','(C6)','(D6)'/
      DATA NYZGB/37,37,19,37,19,19,19,37,19,37,19,
     1           73,37,37,19,19,37,19,25,25,13,13/
      DATA SHAPE/' GAUSS ','LORENTZ'/
      DATA VARIANT/
     * '      FS(G)    ','F(G) AND FS(G) ','      F(G)     '/
      DATA STYP/'FI2  ','FI1  ','GAMMA','ALPHA'/
C
      WRITE (0,66)
      GOTO 52
   51 IUNIT=5
      WRITE (0,68) IUNIT
   52 WRITE (0,'(A/)')
     * ' Enter File on Unit  5 - INPUT DATA  (default CON) : '
      READ (0,69) FNAME
      IF (FNAME.EQ.' ') FNAME='CON'
      OPEN ( 5,FILE=FNAME,ERR=51)
      INP=5
      IF (FNAME.NE.'CON') FNAME='NUL'
      OPEN ( 4,FILE=FNAME)
      GOTO 54
   53 IUNIT=6
      WRITE (0,68) IUNIT
   54 WRITE (0,'(A)')
     * ' Enter File on Unit  6 - LIST OUTPUT (INPUT DATA) +'
      WRITE (0,'(A/)')
     * ' ODF OR ODFS GAMMA-SECTIONS        (default NUL) :'
      READ (0,69) FNAME
      IF (FNAME.EQ.' ') OPEN ( 6,FILE='NUL',ERR=53)
      IF (FNAME.NE.' ') OPEN ( 6,FILE=FNAME,ERR=53)
      IOUT=6
      IUNIT=9
      IUNIT1=10
      GOTO 56
   55 WRITE (0,68) IUNIT
   56 WRITE (0,'(A)')
     * ' Enter File on Unit  9 '
      WRITE (0,'(A/)')
     * ' LIST OUTPUT ODF OR ODFS (ITYP-SECTIONS) (default MEMFILE) : '
      READ (0,69) FNAME
      IF (FNAME.EQ.' ') OPEN(9,FILE='MEMFILE.DAT')
      IF (FNAME.NE.' ') OPEN(9,FILE=FNAME,ERR=55)
      GOTO 58
   57 WRITE (0,68) IUNIT1
   58 WRITE (0,'(A)')
     * ' Enter File on Unit 10  PLOTFILE (BERKELEY FORMAT)'
      WRITE (0,'(A/)')
     * ' ODF OR ODFS ITYP SECTIONS              (default F10.DAT)  : '
      READ (0,69) FNAME
      IF (FNAME.EQ.' ')OPEN(10,FILE='F10.DAT')
      IF (FNAME.NE.' ')OPEN(10,FILE=FNAME,ERR=57)
      DEFI=5.
C
C    INPUT : USER SPECIFICATION,
C            SAMPLE AND CRYSTAL SYMMETRY
C            DETERMINATION OF THE SYMMETRY ELEMENTS TO BE USED
C
      WRITE (4,'(A/)') ' Title Line ( ITITLE   A40 )         : '
      READ (INP,120) ITITLE
      WRITE (4,'(A/)')
     1 ' ITYP  (Section-code = Fi2 (1),Fi1 (2),Gamma (3),Alpha (4)):'
      READ (INP,*) ITYP
      WRITE(IOUT,66)
      WRITE(IOUT,61) STYP(ITYP)
      WRITE (4,65)
      WRITE (4,'(A/)') ' Code Sample, Crystal Symmetry          : '
      READ (INP,*) IGA,IGB
      IZGA=MGE(IGA,25)
      IZGB=MGE(IGB,25)
      IF (IGB.LT.8) IZE=24
      IF (IGB.GT.7) IZE=12
      IJE=IZE*IZGA
      DO 10 IJ=1,IJE
      JGE=IJ-IZE*((IJ-1)/IZE)
   10 MGB(IJ)=MGE(IGB,JGE)
      WRITE(IOUT,160)IGA,IGNAME(IGA),IZGA,IGB,IGNAME(IGB),IZGB
      IF (IGB.LT.8) WRITE (IOUT,170) (MGB(IJ),IJ=1,IJE)
      IF (IGB.GT.7) WRITE (IOUT,180) (MGB(IJ),IJ=1,IJE)
      NYZA=73
      IF (IGA.EQ.2) NYZA=37
      IF (IGA.EQ.3) NYZA=19
      NYZB=NYZGB(IGB,1)
      NYZG=NYZGB(IGB,2)
      NYZ=NYZA
      IF (NYZB.GT.NYZ) NYZ=NYZB
      IF (NYZG.GT.NYZ) NYZ=NYZG
      NAB=IZGA*IZGB
      PI=180./ACOS(-1.)
      RF=DEFI/PI
      NYE=NYZA*NYZB
      DO 20 IFI=1,73
      FIR=RF*(IFI-1)
      IFIW(IFI)=DEFI*(IFI-1)
      SR(IFI)=SIN(FIR)
   20 CR(IFI)=COS(FIR)
      BANF=0.
      SANF=0.
      IF(IGB.LT.8) IELAG=96
      IF(IGB.GT.7) IELAG=48
C
C    INPUT : CALCULATION VARIANT,
C    PARAMETERS CHARACTERIZING THE COMPONENTS TO BE MIXED
C    CALCULATION OF THEIR EQUIVALENT POSITIONS AND
C    (IF NECESSERY) OF THE STANDARD FUNCTION-TABLE FG(S,OMEGA)
C    OMEGA = 1,2,...,180 DEGREES
C
      WRITE (4,'(A/)')
     *' IRED (F=0,FS=1) | N Numb.of Spher.Comp.| NF Numb.of Fibres :'
      READ(INP,*) IRED,N,NF
      IPRINT = 1
      IF(IRED.EQ.0)IPRINT=3
      WRITE (4,'(A/)') ' FON  (Background)                     : '
      READ (INP,*) FON
      WRITE (IOUT,150) VARIANT(IPRINT),N,NF,FON
      WRITE(0,61) STYP(ITYP)
      WRITE(0,160)IGA,IGNAME(IGA),IZGA,IGB,IGNAME(IGB),IZGB
      IF (IGB.LT.8) WRITE (0,170) (MGB(IJ),IJ=1,IJE)
      IF (IGB.GT.7) WRITE (0,180) (MGB(IJ),IJ=1,IJE)
      WRITE (0,150) VARIANT(IPRINT),N,NF,FON
C
C    PREPARATION OF INPUT DATA FOR SHERICAL COMPONENTS
C
      IF (N.EQ.0) GOTO 330
      DO 110 IK=1,N
      WRITE (4,63) IK
      READ (INP,*) ALPHA0,BETA0,GAMMA0
      WRITE (4,'(A/)')
     *' IBST (1,0,-1), BST (Width), HIN (Intensity), IGL (G=1,L=2) :'
      READ (INP,*) IBST,BST,HIN,IGL
      CALL PARGLF (IBST,IGL,HIN,NAB,IK)
      FI1=90.+ALPHA0
      IF (FI1.GE.360.) FI1=FI1-360.
      FI=BETA0
      FI2=270.+GAMMA0
      IF (FI2.GE.360.) FI2=FI2-360.
      WRITE (IOUT,140) IK,SHAPE(IGL),B,S,T,HIN,ALPHA0,BETA0,GAMMA0,
     1                 FI1,FI,FI2
      WRITE (0,140) IK,SHAPE(IGL),B,S,T,HIN,ALPHA0,BETA0,GAMMA0,
     1                 FI1,FI,FI2
      IF (IRED.EQ.0.OR.IGL.EQ.2) GO TO 40
      WRITE(0,67)IK
      CALL FGTAB (B,S,IBST,IOUT,IK)
   40 AR=ALPHA0/PI
      BR=BETA0/PI
      GR=GAMMA0/PI
      CAN=COS(AR)
      SAN=SIN(AR)
      CBN=COS(BR)
      SBN=SIN(BR)
      CGN=COS(GR)
      SGN=SIN(GR)
      IF (IGB.LT.8) CALL SC96 (CAN,SAN,CBN,SBN,CGN,SGN)
      IF (IGB.GT.7) CALL SC48 (CAN,SAN,CBN,SBN,CGN,SGN)
      DO 220 I=1,IELAG
      HMSA(I,IK)=SALFA(I)
      HMSB(I,IK)=SBETA(I)
      HMSG(I,IK)=SGAMMA(I)
      HMCA(I,IK)=CALFA(I)
      HMCB(I,IK)=CBETA(I)
  220 HMCG(I,IK)=CGAMMA(I)
  110 CONTINUE
cc    CALL TIMER(ITICK1)
C
C    PREPARATION OF INPUT DATA FOR FIBRE COMPONENTS
C
330      IF (NF.EQ.0) GOTO 340
      DO 300 IKF=1,NF
      WRITE (4,64)IKF
      READ (INP,*) TEYF, FIYF, TEHF, FIHF
      WRITE (4,'(A/)')
     * ' IBSTF (1,0,-1), Half Width, Intensity, IGLF (G=1,L=2): '
      READ (INP,*) IBST, BST, HIN, IGL
      CALL PFIBF (IBST,BST,HIN,IGL,NAB,IKF)
      CALL VECTOR (TEYF,FIYF,XYF0,YYF0,ZYF0)
      CALL VECTOR (TEHF,FIHF,XHF0,YHF0,ZHF0)
      WRITE (IOUT,145) IKF,SHAPE(IGL),BF,SF,TF,HIN
      WRITE (0,145) IKF,SHAPE(IGL),BF,SF,TF,HIN
      WRITE (IOUT,146) TEYF,XYF0,FIYF,YYF0,ZYF0
      WRITE (IOUT,147) TEHF,XHF0,FIHF,YHF0,ZHF0
      WRITE (0,146) TEYF,XYF0,FIYF,YYF0,ZYF0
      WRITE (0,147) TEHF,XHF0,FIHF,YHF0,ZHF0
      IF (IGB.LT.8) CALL VXYZ24 (XHF0,YHF0,ZHF0)
      IF (IGB.GT.7) CALL VXYZ12 (XHF0,YHF0,ZHF0)
      IEL = 0
      DO 310 IJ=1,IZE
      IF (MGE(IGB,IJ).NE.1) GOTO 310
      IEL = IEL+1
      HXHF(IEL,IKF) = X(IJ)
      HYHF(IEL,IKF) = Y(IJ)
      HZHF(IEL,IKF) = Z(IJ)
  310 CONTINUE
      CALL VXYZ24 (XYF0,YYF0,ZYF0)
      IEL = 0
      DO 320 IJ=1,24
      IF (MGE(IGA,IJ).NE.1) GOTO 320
      IEL = IEL+1
      HXYF(IEL,IKF) = X(IJ)
      HYYF(IEL,IKF) = Y(IJ)
      HZYF(IEL,IKF) = Z(IJ)
  320 CONTINUE
  300 CONTINUE
cc    IF (N.EQ.0)CALL TIMER(ITICK1)
  340 CONTINUE
C
      FMAX = 0.
      FMIN = 8888.
C
C    GAMMA - LOOP
C
      DO 100 IGAMMA=1,NYZG
      SG=SR(IGAMMA)
      CG=CR(IGAMMA)
      DO 230 IB=1,NYZB
      DO 230 IA=1,NYZA
      FUK(IA,IB)=FON
  230 FUM(IA,IB)=FON
C
C    SPHERICAL COMPONENT - LOOP
C
      IF (N.EQ.0) GOTO 350
      IKF=0
      DO 30 IK=1,N
      WRITE (0,62) IGAMMA,NYZG,IK,N,IKF,NF
      CALL COMPF (IK,IRED,IELAG,IGL)
C
C    BETA - LOOP
C
      DO 90 IBETA=1,NYZB
      SB=SR(IBETA)
      CB=CR(IBETA)
C
C    ALPHA - LOOP
C
      DO 80 IALFA=1,NYZA
      SA=SR(IALFA)
      CA=CR(IALFA)
      SUM=0.
      SUM1=0.
C
C    SWICH - GAUSS OR LORENTZ VARIANT
C    SUM OF THE F(G) (FS(G)) CONTRIBUTIONS OF ALL
C    COMPONENTS AND THEIR EQUIVALENT POSITIONS IN FUK (FUM)
C
      IF (IGL-1) 50,50,60
   50 CALL OVFG (IRED,IJE)
      GO TO 70
   60 CALL OVFL (IRED,IJE)
   70 SUM1=SUM1*0.5
C
C  FUK - ODF; FUM - ODFS
C
      FUK(IALFA,IBETA)=FUK(IALFA,IBETA)+HNFUK*SUM1
      IF (IRED.EQ.0) GO TO 80
      FUM(IALFA,IBETA)=FUM(IALFA,IBETA)+HNFUM*
     1                 (SUM1*TAUSSS+SUM*TAUSS)
   80 CONTINUE
   90 CONTINUE
   30 CONTINUE
C
C    FIBRE - COMPONENT - LOOP
C
  350 IF (NF.EQ.0) GOTO 360
      IKH=N
      DO 430 IKF=1,NF
      WRITE (0,62) IGAMMA,NYZG,IKH,N,IKF,NF
      CALL COMFIB (IKF,IZGA,IZGB,IGL)
C
C    BETA - LOOP
C
      DO 490 IBETA=1,NYZB
      SB=SR(IBETA)
      CB=CR(IBETA)
C
C    ALPHA - LOOP
C
      DO 480 IALFA=1,NYZA
      SA=SR(IALFA)
      CA=CR(IALFA)
      SUMF=0.
      SUMF1=0.
C
C    SWICH - GAUSS OR LORENTZ VARIANT
C    SUM OF THE F(G) (FS(G)) CONTRIBUTIONS OF ALL
C    COMPONENTS AND THEIR EQUIVALENT POSITIONS IN FUK (FUM)
C
      IF (IGL-1) 450,450,460
  450 CALL OVFIBG (IRED,IZGA,IZGB)
      GO TO 470
  460 CALL OVFIBL (IRED,IZGA,IZGB)
  470 FUK(IALFA,IBETA)=FUK(IALFA,IBETA)+HNFUKF*SUMF1
      IF (IRED.EQ.0) GO TO 480
      FUM(IALFA,IBETA)=FUM(IALFA,IBETA)+HNFUMF*(SUMF+SUMF1)
  480 CONTINUE
  490 CONTINUE
  430 CONTINUE
  360 CONTINUE
C
C    END OF THE ALPHA-, BETA- AND COMPONENT-LOOPS
C    OUTPUT OF ODF OR ODFS GAMMA-SECTION ON FILE 6
C
      CALL FOUT (IGAMMA,IRED)
C
C    END OF THE GAMMA-LOOP
C
  100 CONTINUE
cc      CALL TIMER(ITICK2)
        ITICK = ITICK2-ITICK1
        IS = ITICK/100
        IM = IS/60
        IS = IS-IM*60
        WRITE (0,600)IM,IS
C
C    OUTPUT OF ODF OR ODFS (ITYP-SECTIONS) ON FILE 9 (LIST) OR 10 (PLOT)
C
      CALL TYPOUT(ITYP,IGA,IGB,IRED)
      IF(IUNIT.EQ.9) CLOSE (9)
      IF (IUNIT1.EQ.10) CLOSE (10)
      STOP
C
  600 FORMAT (4X,'time = ',i5,' min.',i2,' sek.'//)
   61 FORMAT (/4X,'INPUT DATA AS THEY ARE READ',//,4X,
     1        'OUTPUT SECTIONS ',A5/)
   62 FORMAT ('+Run : ODF section',I3,' (',I2,')',
     * '  *spher. comp. Nr.',I3,' (',I2,')*',
     * '  fibre comp. Nr.',I3,' (',I2,')')

   63 FORMAT (/2X,
     * '(Spherical Component NR. ',I2,')  ALPHA0,BETA0,GAMMA0  : '/)
   64 FORMAT (/2X,
     * '(Fibre  Component NR. ',I2,')   TEYF, FIYF, TEHF, FIHF : '/)
   65 FORMAT (/
     * '  SAMPLE SYMMETRY  :                D2  C2  C1'/
     * '  CRYSTAL SYMMETRY : O   T  D4  C4  D2  C2  C1  D6  C6  D3  C3'/
     * '  CODE NUMBER      : 7   6   5   4   3   2   1  11  10   9   8')
   66 FORMAT(//3X,'PROGRAM ODFSF  MATTHIES/VINEL ROSSENDORF',/
     * 3X,'PC-VERSION BERKELEY 5/92',/
     * 3X,50(1H_)//3X,
     * 'COMPUTES ODF (F(G)) OR ODFS (FS(G)) AS A MIXTURE OF',/3X,
     * 'SPHERICAL OR FIBRE COMPONENTS BY',/3X,
     * 'STANDARD FUNCTIONS FOR ALL SYMMETRIES'/)
   67 FORMAT('+RUN : FGTAB spher. comp. Nr. ',I2)
   68 FORMAT (5X,'Error in opening file ',I2,15X,'Try again !')
   69 FORMAT (A20)
  120 FORMAT(A40)
  140 FORMAT(3X,'SPHER.COMP.NR. =',I2,1X,A7,
     1 ' B = ',F5.2,' S = ',F7.2,' T = ',F9.4,
     2 ' HIN = ',F6.4/3X,75(1H-)/
     3 5X,'POSITION G0: ALPHA0 = ',
     4 F8.4,3X,'BETA0 = ',F8.4,3X,'GAMMA0 = ',F8.4/
     5 21X,'FI1 = ',F8.4,6X,'FI = ',F8.4,6X,'FI2 = ',F8.4/)
  145 FORMAT(3X,'FIBRE COMP.NR. =',I2,1X,A7,
     1 ' B = ',F5.2,' S = ',F7.2,' T = ',F9.4,
     2 ' HIN = ',F6.4/3X,75(1H-))
  146 FORMAT (5X,'FIBRE VECTOR IN KA :',
     * 'THETA = ',F7.3,10X,'X = ',F7.3/
     * 25X,'FI    = ',F7.3,10X,'Y = ',F7.3/
     * 50X,'Z = ',F7.3/)
  147 FORMAT (5X,'FIBRE VECTOR IN KB :',
     * 'THETA = ',F7.3,10X,'X = ',F7.3/
     * 25X,'FI    = ',F7.3,10X,'Y = ',F7.3/
     * 50X,'Z = ',F7.3/)
  150 FORMAT(3X,A15,'CALCULATED  NUMBER OF SHERICAL COMPONENTS N=',I2/
     1          18X,'CALCULATED  NUMBER OF FIBRE    COMPONENTS N=',I2/
     2'   FON = ',F7.5,/ 3X,75(1H-)/
     3 60X,'8888.0 MEANS DUMMY'/)
  160 FORMAT(5X,'SAMPLE  SYMMETRY   IGA = ',I2,2X,A4,
     2 4X,'NUMBER OF GROUP ELEMENTS  IZGA = ',I2,/,
     3 5X,'CRYSTAL SYMMETRY   IGB = ',I2,2X,A4,
     4 4X,'NUMBER OF GROUP ELEMENTS  IZGB = ',I2,/)
  170 FORMAT(5X,'ELEMENT-CODE MGB(K,J) : ',
     1 24(I2)/,3(29X,24(I2)/)/)
  180 FORMAT(5X,'ELEMENT-CODE MGB(K,J) : ',
     1 12(I2)/,3(29X,12(I2)/)/)
C
      END
C
C    *--------------------------------*
C
      SUBROUTINE PARGLF (IBST,IGL,HIN,NAB,IK)
C
C     CALCULATES GAUSSIAN AND LORENTZIAN WORKING PARAMETERS
C
      COMMON /PAR/B,BST,SUM,SUM1,S,T,TAU,TAUS,TAUSS,
     1            TAUSSS,HNFUK,HNFUM
      COMMON /HMPAR/HB(50),HS(50),HT(50),HMNFUK(50),
     1              HMNFUM(50),HTAU(4,50),IHIGL(50)
      COMMON /CPI/PI
C
      IF (IGL-1) 10,10,70
   10 IF (IBST) 50,20,50
   20 S=BST
      T=8888.
      B=8888.
      SGRENZ=ALOG(2.)/2.
      IF (S-SGRENZ) 60,30,40
   30 B=360.
      GO TO 60
   40 ARGH=SQRT(SGRENZ/S)
      B=4.*ASIN(ARGH)*PI
      GO TO 60
   50 T=8888.
      B=BST
      BR4=B/(4.*PI)
      S=ALOG(2.)/(2.*SIN(BR4)**2)
   60 B0=BES(S,0)
      AMP=1./(B0-BES(S,1))
      HNFUK=HIN*2.*AMP/(NAB+0.)
      HNFUM=HIN/(NAB+0.)
      TAUSS=1.
      TAUSSS=AMP
      HB(IK)=B
      HS(IK)=S
      GOTO 130
   70 IF (IBST) 80,110,110
   80 B=8888.
      T=BST
      S=8888.
      TAU=(1.+T*T)/(2.*T)
      TAU=TAU*TAU
      TAUG=0.5*(5.+SQRT(17.))
      TGRENZ=SQRT(TAUG)-SQRT(TAUG-1.)
      IF (T-TGRENZ) 120,90,100
   90 B=360.
      GO TO 120
  100 TAU2=TAU*TAU
      TAU3=TAU2*TAU
      TAU4=TAU3*TAU
      ARGH=5.*TAU4-8.*TAU3+2.*TAU2+1.
      C=((2.*TAU2-TAU+1.)-SQRT(ARGH))/(1.+TAU)
      SC=SQRT(C)
      B=4.*ACOS(SC)*PI
      GO TO 120
  110 B=BST
      S=8888.
      BR4=B/(4.*PI)
      C=COS(BR4)**2
      AC=19.*C**2-34.*C+19.
      HG=(-82.*C**3+240.*C**2-246.*C+80.)/AC**1.5
      HG=ACOS(HG)/3.
      TAU=(2.*SQRT(AC)*COS(HG)+(5.-4.*C))/3.
      AC=TAU-1.
      T=SQRT(TAU)-SQRT(AC)
  120 T2=T*T
      TAUS=((1.-T2)/(2.*T))**2
      TAUSS=(1.+T2)/(8.*T2)
      TAUSSS=(1.-T2)/(4.*T2)
      HNFUK=HIN*2.*TAUSSS/(NAB+0.)
      HNFUM=HIN/(NAB+0.)
      HT(IK)=T
      HTAU(3,IK)=TAUS
      HTAU(4,IK)=TAU
 130  HMNFUK(IK)=HNFUK
      HMNFUM(IK)=HNFUM
      HTAU(1,IK)=TAUSSS
      HTAU(2,IK)=TAUSS
      IHIGL(IK)=IGL
      RETURN
      END
C
C    *--------------------------------*
C
      SUBROUTINE PFIBF (IBST,BST,HIN,IGL,NAB,IKF)
C
C     CALCULATES GAUSSIAN AND LORENTZIAN WORKING PARAMETERS
C
      COMMON /CPI/PI
      COMMON /PARFIB/B,S,T,SUM,SUM1,EPT2,ZT,HNFUK,HNFUM
      COMMON /HMPARF/HSF(5),HTF(5),HFUKF(5),
     1               HTFIB(2,5),IGLF(5)
      IF (IGL-1) 10,10,70
   10 IF (IBST) 50,20,50
   20 S=BST
      T=8888.
      B=8888.
      SGRENZ=ALOG(2.)/2.
      IF (S-SGRENZ) 60,30,40
   30 B=360.
      GO TO 60
   40 ARGH=SQRT(SGRENZ/S)
      B=4.*ASIN(ARGH)*PI
      GO TO 60
   50 T=8888.
      B=BST
      BR4=B/(4.*PI)
      S=ALOG(2.)/(2.*SIN(BR4)**2)
   60 HNFUK = HIN/(NAB+0.)
      IF (S.LT.1.E-10) GOTO 65
      S2 = 2.*S
      HNFUK = HNFUK*S2
      IF (S2.LT.170.) HNFUK = HNFUK/(1.-EXP(-S2))
   65 HSF(IKF)=S
      GOTO 130
   70 Z23 = 4.**(1./3.)
      RN = Z23-1.
      IF (IBST) 80,110,110
   80 B=8888.
      T=BST
      S=8888.
      TGRENZ = (Z23+1.-Z23*Z23)/RN
      IF (T-TGRENZ) 120,90,100
   90 B=360.
      GO TO 120
  100 R = 0.5*(1.+T*T)/T
      B = 2.*PI*ACOS(RN*R+Z23)
      GO TO 120
  110 B=BST
      S=8888.
      BR2=B/(2.*PI)
      R = (Z23-COS(BR2))/RN
      T = R-SQRT(R*R-1.)
  120 T2=T*T
      HNFUK=HIN*(1.-T2)/(NAB+0.)
      HTF(IKF)=T
      HTFIB(1,IKF)=1.+T2
      HTFIB(2,IKF)=2.*T
  130 HFUKF(IKF)=HNFUK
      IGLF(IKF)=IGL
      RETURN
      END
C
C
C    *--------------------------------*
C
      SUBROUTINE OVFG (IRED,IJE)
C
C    CALCULATES GAUSSIAN F(G) OR FS(G) (DEPENDING ON IRED)
C    TAKING INTO ACCOUNT ALL NA*NB EQUIVALENT PEAK POSITIONS
C    AND USING THE FG(S,OMEGA) - TABLE
C    INPUT G BY SICO, INPUT G0(JK) BY ABGSC
C    COS(OMEGA)=(SPUR(G0(-1)*G)-1)/2
C    SPUR=2*COS(OMEGA)+1
C    LINEAR FG-INTERPOLATION FOR B.GE.20 (DEGREES)
C    OR OMEGA. LT.180-1.5*B
C    FOR B.LT.20 AND OMEGA.GT.180-1.5*B
C    FG-INTERPOLATION IN PRCINT, USING 4 NODES
C    UGR -  PROTECTION AGAINST UNDERFLOW
C    SUM1 - ODF
C    SUM - ODFS
C
      DIMENSION FGM(183)
      DIMENSION ARGM(4),VAL(4)
      DIMENSION SALFA(96),SBETA(96),SGAMMA(96),
     1          CALFA(96),CBETA(96),CGAMMA(96)
      COMMON /ABGSC/SALFA,SBETA,SGAMMA,CALFA,CBETA,CGAMMA
      COMMON /GEIST/FGM
      COMMON /SICO/SG,CG,SB,CB,SA,CA
      COMMON /PAR/B,BST,SUM,SUM1,S,HFELD(7)
      COMMON /CMGB/MGB(96)
      COMMON /CPI/PI
C
      UGR=-170.
      DO 170 IK=1,IJE
      IF (MGB(IK).EQ.0) GO TO 170
      SAN=SALFA(IK)
      CAN=CALFA(IK)
      SBN=SBETA(IK)
      CBN=CBETA(IK)
      SGN=SGAMMA(IK)
      CGN=CGAMMA(IK)
      SCA=CA*CAN+SA*SAN
      SCB=CG*CGN+SG*SGN
      SPUR=SCA*SCB*(1.+CB*CBN)-(SA*CAN-CA*SAN)*(SG*CGN
     1    -CG*SGN)*(CB+CBN)+(SCA+SCB)*SB*SBN+CB*CBN
      IF (SPUR+1.) 10,10,20
   10 SPUR=-1.
      SPURT=0.5*(SPUR-1.)
      ARG=S*(SPURT-1.)
      IF (ARG.LE.UGR) GOTO 25
      SUM1=SUM1+EXP(ARG)
   25 IF (IRED.EQ.0) GO TO 170
      SUM=SUM+FGM(181)
      GO TO 170
   20 IF (SPUR-3.) 40,30,30
   30 SPUR=3.
      SPURT=0.5*(SPUR-1.)
      ARG=S*(SPURT-1.)
      IF (ARG.LE.UGR) GOTO 35
      SUM1=SUM1+EXP(ARG)
   35 IF (IRED.EQ.0) GO TO 170
      SUM=SUM+FGM(1)
      GO TO 170
   40 SPURT=0.5*(SPUR-1.)
      ARG=S*(SPURT-1.)
      IF (ARG.LE.UGR) GOTO 45
      SUM1=SUM1+EXP(ARG)
   45 IF (IRED.EQ.0) GO TO 170
      OMEGA=PI*ACOS(SPURT)
C
C     WORKS ALSO FOR B - DUMMY (8888)
C
      IF (B-20.) 60,60,50
   50 IOME1=OMEGA+1
      IOME2=IOME1+1
      FUG=FGM(IOME1)+(FGM(IOME2)-FGM(IOME1))*(OMEGA-IOME1+1)
      GO TO 160
   60 IF (180.-OMEGA-1.5*B) 70,50,50
   70 IF (OMEGA-179.5) 90,90,80
   80 ARGM(1)=180.
      ARGM(2)=179.
      VAL(1)=FGM(181)
      VAL(2)=FGM(180)
      GO TO 110
   90 IF (OMEGA-179.) 120,100,100
  100 ARGM(1)=179.
      ARGM(2)=180.
      VAL(1)=FGM(180)
      VAL(2)=FGM(181)
  110 ARGM(3)=178.
      ARGM(4)=177.
      VAL(3)=FGM(179)
      VAL(4)=FGM(178)
      GO TO 150
  120 IOM=OMEGA
      IOM1=IOM+1
      IOM2=IOM+2
      IOM3=IOM+3
      IOM4=IOM-1
      IF (OMEGA-IOM-0.5) 130,140,140
  130 ARGM(1)=IOM
      ARGM(2)=IOM1
      ARGM(3)=IOM4
      ARGM(4)=IOM2
      VAL(1)=FGM(IOM1)
      VAL(2)=FGM(IOM2)
      VAL(3)=FGM(IOM)
      VAL(4)=FGM(IOM3)
      GO TO 150
  140 ARGM(1)=IOM1
      ARGM(2)=IOM
      ARGM(3)=IOM2
      ARGM(4)=IOM4
      VAL(1)=FGM(IOM2)
      VAL(2)=FGM(IOM1)
      VAL(3)=FGM(IOM3)
      VAL(4)=FGM(IOM)
  150 CALL PRCINT(OMEGA,ARGM,VAL,FUG)
  160 SUM=SUM+FUG
  170 CONTINUE
      UGR=10.8E-36
      IF (SUM1.LE.UGR) SUM1=0.
      RETURN
      END
C
C    *--------------------------------*
C
      SUBROUTINE OVFL (IRED,IJE)
C
C    CALCULATES LORENTZIAN F(G) OR FS(G) (DEPENDING ON IRED)
C    TAKING INTO ACCOUNT ALL NA*NB EQUIVALENT PEAK POSITIONS
C    INPUT G BY SICO, INPUT G0(JK) BY ABGSC
C    COS(OMEGA)=(SPUR(G0(-1)*G)-1)/2
C    SPUR=2*COS(OMEGA)+1
C
      DIMENSION SALFA(96),SBETA(96),SGAMMA(96),
     1          CALFA(96),CBETA(96),CGAMMA(96)
      COMMON /ABGSC/SALFA,SBETA,SGAMMA,CALFA,CBETA,CGAMMA
      COMMON /SICO/SG,CG,SB,CB,SA,CA
      COMMON /PAR/B,BST,SUM,SUM1,S,T,TAU,TAUS,HFELD(4)
      COMMON /CMGB/MGB(96)
      COMMON /CPI/PI
C
      DO 50 IK=1,IJE
      IF (MGB(IK).EQ.0) GO TO 50
      SAN=SALFA(IK)
      CAN=CALFA(IK)
      SBN=SBETA(IK)
      CBN=CBETA(IK)
      SGN=SGAMMA(IK)
      CGN=CGAMMA(IK)
      SCA=CA*CAN+SA*SAN
      SCB=CG*CGN+SG*SGN
      SPUR=SCA*SCB*(1.+CB*CBN)-(SA*CAN-CA*SAN)*(SG*CGN
     1    -CG*SGN)*(CB+CBN)+(SCA+SCB)*SB*SBN+CB*CBN
      IF (SPUR+1.) 10,10,20
   10 SPUR=-1.
   20 IF (SPUR-3.) 40,40,30
   30 SPUR=3.
   40 COMH2=(1.+SPUR)/4.
      SUM1=SUM1+(TAU+COMH2)/(TAU-COMH2)**2
      IF (IRED.EQ.0) GO TO 50
      SUM=SUM+(TAUS-COMH2)/(TAUS+COMH2)**2
   50 CONTINUE
      RETURN
      END
C
C
C    *--------------------------------*
C
      SUBROUTINE OVFIBG (IRED,IZGA,IZGB)
C
C    FIBRE COMPONENT  -  GAUSSIAN
C
      DIMENSION HX(4),HY(4),HZ(4)
      COMMON /SICO/SG,CG,SB,CB,SA,CA
      COMMON /HVEC/XHF(24),YHF(24),ZHF(24),
     *             XYF(4),YYF(4),ZYF(4)
      COMMON /PARFIB/B,S,T,SUMF,SUMF1,EPT2,ZT,HNFUK,HNFUM
C
      UGR = -170.
      DO 10 IJA = 1,IZGA
      XX = XYF(IJA)
      YY = YYF(IJA)
      ZZ = ZYF(IJA)
      HX(IJA)= (CA*CB*CG-SA*SG)*XX + (SA*CB*CG+CA*SG)*YY - SB*CG*ZZ
      HY(IJA)=-(CA*CB*SG+SA*CG)*XX - (SA*CB*SG-CA*CG)*YY + SB*SG*ZZ
      HZ(IJA)=            CA*SB*XX +            SA*SB*YY +    CB*ZZ
      DO 20 IJB = 1,IZGB
      Z = XHF(IJB)*HX(IJA)+YHF(IJB)*HY(IJA)+ZHF(IJB)*HZ(IJA)
      ARG1 = (Z-1.)*S
      IF (ARG1.GT.UGR) SUMF1=SUMF1+EXP(ARG1)
      IF (IRED.EQ.0) GOTO 20
      ARG = -(Z+1.)*S
      IF (ARG.GT.UGR) SUMF=SUMF+EXP(ARG)
   20 CONTINUE
   10 CONTINUE
      RETURN
      END
C
C
C    *--------------------------------*
C
      SUBROUTINE OVFIBL (IRED,IZGA,IZGB)
C
C    FIBRE COMPONENT  -  LORENTZIAN
C
      DIMENSION HX(4),HY(4),HZ(4)
      COMMON /SICO/SG,CG,SB,CB,SA,CA
      COMMON /HVEC/XHF(24),YHF(24),ZHF(24),
     *             XYF(4),YYF(4),ZYF(4)
      COMMON /PARFIB/B,S,T,SUMF,SUMF1,EPT2,ZT,HNFUK,HNFUM
C
      DO 10 IJA = 1,IZGA
      XX = XYF(IJA)
      YY = YYF(IJA)
      ZZ = ZYF(IJA)
      HX(IJA)= (CA*CB*CG-SA*SG)*XX + (SA*CB*CG+CA*SG)*YY - SB*CG*ZZ
      HY(IJA)=-(CA*CB*SG+SA*CG)*XX - (SA*CB*SG-CA*CG)*YY + SB*SG*ZZ
      HZ(IJA)=            CA*SB*XX +            SA*SB*YY +    CB*ZZ
      DO 20 IJB = 1,IZGB
      Z = XHF(IJB)*HX(IJA)+YHF(IJB)*HY(IJA)+ZHF(IJB)*HZ(IJA)
      SUMF1 = SUMF1+1./(EPT2-ZT*Z)**1.5
      IF (IRED.EQ.0) GOTO 20
      SUMF  = SUMF +1./(EPT2+ZT*Z)**1.5
   20 CONTINUE
   10 CONTINUE
      RETURN
      END
C
C    *--------------------------------*
C
      SUBROUTINE FOUT (IGAMMA,IRED)
C
C    DEPENDING ON IRED (0 OR 1)
C    PRINT OF F(G), FS(G) FOR A GAMMA SECTION   (FILE IOUT=6)
C
      CHARACTER*5 NAMET,NAMEFI,NAMESE
      CHARACTER ITITLE*40
      DIMENSION IFIW(73)
      DIMENSION FUK(73,37),FUM(73,37),F(197173)
      COMMON /CPRI/ NGE,NAE,NBE,NAMET,NAMEFI,NAMESE
      COMMON /CF/FUK,FUM,F
      COMMON /CMIMA/ FMIN,FMAX,AMIN,AMAX,BMIN,BMAX,SIMIN,SIMAX
      COMMON /CDIM/ NYZA,NYZB,NYZG,NYE
      COMMON /CIFIW/IFIW,IOUT,IUNIT,IUNIT1
      COMMON /TIT/ ITITLE
C
      NGE = NYZG
      NAE = NYZA
      NBE = NYZB
      NAMET = 'BETA '
      NAMEFI = 'ALPHA'
      NAMESE = 'GAMMA'
      IU = IUNIT
      GAM = IFIW(IGAMMA)
      IF (IRED.EQ.0) WRITE (IOUT,190) GAM
      IF (IRED.EQ.1) WRITE (IOUT,200) GAM
C     PRINT OF THE GAMMA-SECTION  (FILE IOUT)
      IF (IRED.EQ.0) CALL PRI(IGAMMA,3)
      IF (IRED.EQ.1) CALL PRI(IGAMMA,4)
C
C    LOADING OF THE F ARRAY NECESSARY FOR LATER OUTPUT
C    OF ITYP-SECTIONS
C
      IF (IRED.EQ.0.AND.IU.NE.0)GOTO 1
      GOTO 2
   1  IADG=NYE*(IGAMMA-1)
      DO 3 IBET=1,NYZB
      IADGB=IADG+NYZA*(IBET-1)
      DO 4 IALF=1,NYZA
   4  F(IADGB+IALF)=FUK(IALF,IBET)
   3  CONTINUE
      RETURN
   2  IF (IRED.EQ.1.AND.IU.NE.0)GOTO 5
      RETURN
   5  IADG=NYE*(IGAMMA-1)
      DO 6 IBET=1,NYZB
      IADGB=IADG+NYZA*(IBET-1)
      DO 7 IALF=1,NYZA
   7  F(IADGB+IALF)=FUM(IALF,IBET)
   6  CONTINUE
C
      RETURN
C
  190 FORMAT(   //5X,'GAMMA = ',F5.1,
     1 2X,'ODF  F(G)'/)
  200 FORMAT(   //5X,'GAMMA = ',F5.1,
     1 2X,'REDUCED ODF  FS(G)'/)
      END
C
C    *--------------------------------*
C
      SUBROUTINE TYPOUT(ITYP,IGA,IGB,IRED)
C
C    Input : ARRAY F(197173)-ALPHA,BETA,GAMMA structure
C    containing f(g) or f~(g) in the IGA,IGB elementary region
C
C    CALL OF THE OUTPUT ROUTINE  TPRINT
C    LIST and PLOTFILE by ITYP-SECTIONS
C    ITYP ( FI2 Sections - 1,  FI1 Sections - 2,
C           GAMMA Sections - 3,  ALPHA Sections - 4)
C
      CHARACTER ITITLE*40
      DIMENSION IFIW(73)
      COMMON /CF/FS(73,37),HF(73,37),F(197173)
      COMMON /CDIM/ NYZA,NYZB,NYZG,NYE
      COMMON /CIFIW/IFIW,IOUT,IUNIT,IUNIT1
      COMMON /TIT/ ITITLE
C
      IU = IUNIT
C
      IF (IU.EQ.0) GOTO 55
C
C   PRINT OF ITITLE (unit 9 IF IT EXCISTS) for Listing
C   AS A HEADLINE BEFORE PRINT OF THE ITYP-SECTIONS
C   IN TPRINT
C
      IF (IRED.EQ.0)
     1 WRITE (IU,3005) ITITLE
      IF (IRED.EQ.1)
     1 WRITE (IU,3006) ITITLE
      CAll SIGAB (IGAB,IGA,IGB)
   55 CALL TPRINT(ITYP,IGA,IGAB,IRED)
  101 FORMAT (19F8.2)
 3005 FORMAT ('   ODF  ',A40)
 3006 FORMAT ('   REDUCED ODF  ',A40)
      RETURN
      END
C
C
C    *--------------------------------*
C
      SUBROUTINE  SIGAB(igab,iga,igb)
C
C     igab is necessary for the construction of Phi1 or Phi2 sections
C     from Gamma sections. See SUBROUTINE TPRINT
C
      if (iga.eq.3) goto 1
      igab=1
      RETURN
    1 if (igb.eq.3) igab=2
      if (igb.eq.9) igab=2
      if (igb.eq.5) igab=2
      if (igb.eq.11) igab=2
      if (igb.eq.7) igab=2
      if (igb.eq.6) igab=2
      if (igb.eq.1) igab=3
      if (igb.eq.2) igab=3
      if (igb.eq.4) igab=3
      if (igb.eq.8) igab=3
      if (igb.eq.10) igab=3
      RETURN
      END
C
C
C    *--------------------------------*
C
      SUBROUTINE  TPRINT(ITYP,IGA,IGAB,IRED)
C
C    INPUT F(197173) ALPHA,BETA,GAMMA STRUCTURE. CONTAINS f(g) OR
C    f~(g) IN THE ELEMENTARY IGA IGB REGION. SECTION LOOP.
C    CONSTRUCTION OF THE SECTION ARRAY FS(73,37) FOR THE CHOSEN
C    VARIANT OF SECTIONS (ITYP). CALL OF THE SECTION PRINTROUTINE
C    "PRI" :  ON UNIT 9 (LIST) AND 10 (PLOTFILE)
C
C    ITYP (FI2 Sections - 1,  FI1 Sections - 2,
C          GAMMA Sections - 3,  ALPHA Sections - 4)
C
      DIMENSION IFIW(73)
      CHARACTER ITITLE*40
      CHARACTER*5 NAMET,NAMEFI,NAMESE
      COMMON /CMIMA/ FMIN,FMAX,AMIN,AMAX,BMIN,BMAX,SIMIN,SIMAX
      COMMON /CPRI/ NGE,NAE,NBE,NAMET,NAMEFI,NAMESE
      COMMON /CF/ FS(73,37),HF(73,37),F(197173)
      COMMON /CDIM/ NYZA,NYZB,NYZG,NYE
      COMMON /CIFIW/IFIW,IOUT,IUNIT,IUNIT1
      COMMON /TIT/ ITITLE
C
      FMIN = 8888.
      FMAX = 0.
      NYZAH = 37
      IF (IGA.EQ.1) NYZAH = 73
C
      ID = 2
      IF (IRED.EQ.0) ID = 1
      IU = IUNIT
      IF (IU.EQ.0) RETURN
      IF (ITYP-2)101,19,3
    3 IF (ITYP-4) 5,6,6
C
C GAMMA-SECTION OUTPUT
C
    5 NGE = NYZG
      NAE = NYZA
      NBE = NYZB
      NAMET = 'BETA '
      NAMEFI = 'ALPHA'
      NAMESE = 'GAMMA'
      DO  1 IGAM = 1,NYZG
      IA = NYE*(IGAM-1)
      GAM = IFIW(IGAM)
      IF (IRED.EQ.0) WRITE (IU,190) GAM
      IF (IRED.EQ.1) WRITE (IU,200) GAM
  190 FORMAT(   //5X,'GAMMA = ',F5.1,
     1 2X,'ODF  F(G)'/)
  200 FORMAT(   //5X,'GAMMA = ',F5.1,
     1 2X,'REDUCED ODF  FS(G)'/)
      DO 2 IBET = 1,NYZB
      IB = NYZA*(IBET-1)+IA
      DO 2 IALFA = 1,NYZA
      NG = IB+IALFA
    2 FS(IALFA,IBET) =F(NG)
             CALL PRI(IGAM,ID)
    1 CONTINUE
C
      RETURN
C
C ALFA-SECTION OUTPUT
C
C       If 60 or 120 degrees sector plots impossible,
C       but 90,180,360 degrees only, use the C* variant
C
C
   6  NYZGE=NYZG
C*    if(nyzg.lt.19)nyzge=19
C*    if(nyzg.gt.19.and.nyzg.lt.73)nyzge=37
      NGE = NYZA
      NAE = NYZGE
      NBE = NYZB
      NAMET = 'BETA '
      NAMEFI = 'GAMMA'
      NAMESE = 'ALPHA'
      DO  7 IALFA = 1,NYZA
      ALFA = IFIW(IALFA)
      IF (ID.EQ.1) WRITE (IU,112) ALFA
      IF (ID.EQ.2) WRITE (IU,113) ALFA
  112 FORMAT(   //5X,'ALPHA = ',F5.1,
     1 2X,'ODF  F(G)'/)
  113 FORMAT(   //5X,'ALPHA = ',F5.1,
     1 2X,'REDUCED ODF  FS(G)'/)
      DO 8 IBET = 1,NYZB
      IB = NYZA*(IBET-1)
      DO  9 IGAM = 1,NYZGE
      NG = NYE*(IGAM-1)+IB+IALFA
    9 FS(IGAM,IBET) = F(NG)
    8 CONTINUE
      CALL PRI(IALFA,ID)
C
    7 CONTINUE
      RETURN
C
C FI2-SECTION OUTPUT
C
  101 NGE = NYZG
      NAE = NYZA
      NBE = NYZB
      NAMET = 'FI   '
      NAMEFI = 'FI1  '
      NAMESE = 'FI2  '
      DO 28 IFI2=1,NYZG
      FI2=IFIW(IFI2)
      IF (IGAB.EQ.1) NGA = IFI2+18
      IF (IGAB.EQ.2) NGA=20-IFI2
      IF (IGAB.EQ.3) NGA=IFI2-18
   65 IF(NGA.GT.NYZG) NGA = NGA-NYZG+1
      IF (NGA.GT.NYZG) GOTO 65
C
   56 IF (NGA.LT.1) NGA=NGA+NYZG-1
      IF (NGA.LT.1) GOTO 56
      IHGA = NYE*(NGA-1)
      IF (ID.EQ.1) WRITE (IU,110) FI2
      IF (ID.EQ.2) WRITE (IU,111) FI2
  110 FORMAT(   //5X,' FI2 = ',F5.1,
     1 2X,'ODF  F(G)'/)
  111 FORMAT(   //5X,' FI2 = ',F5.1,
     1 2X,'REDUCED ODF  FS(G)'/)
      DO 29 IFI=1,NYZB
      IBET  = IFI
      IF (IGAB.EQ.3) IBET = 38-IFI
      IA=NYZA*(IBET-1)
      DO 30 IFI1=1,NYZA
      IF (IGAB.EQ.1) IALFA = IFI1-18
      IF (IGAB.EQ.2) IALFA = 56-IFI1
      IF (IGAB.EQ.3) IALFA = 56-IFI1
   66 IF (IALFA.LT.1) IALFA = IALFA+NYZAH-1
      IF (IALFA.LT.1) GOTO 66
C
   77 IF (IALFA.GT.NYZA) IALFA=IALFA-NYZAH+1
      IF (IALFA.GT.NYZA) GOTO 77
      NG = IHGA+IA+IALFA
   30 FS(IFI1,IFI) = F(NG)
   29 CONTINUE
C
          CALL PRI(IFI2,ID)
C
   28 CONTINUE
  108 FORMAT(2X,I2,'*',19F6.2)
      RETURN
C
C FI1-SECTION OUTPUT
C
C     See comment for ALFA-sections
C
   19 NYZGE=NYZG
C*    if(nyzg.lt.19)nyzge=19
C*    if(nyzg.gt.19.and.nyzg.lt.73)nyzge=37
      NGE = NYZA
      NAE = NYZGE
      NBE = NYZB
      NAMET = 'FI   '
      NAMEFI = 'FI2  '
      NAMESE = 'FI1  '
      DO 31 IFI1=1,NYZA
      FI1=IFIW(IFI1)
      IF (IGAB.EQ.1) IALFA = IFI1-18
      IF (IGAB.EQ.2) IALFA = 56-IFI1
      IF (IGAB.EQ.3) IALFA = 56-IFI1
   67 IF (IALFA.LT.1) IALFA = IALFA+NYZAH-1
      IF (IALFA.LT.1) GOTO 67
C
   76 IF (IALFA.GT.NYZA) IALFA=IALFA-NYZAH+1
      IF (IALFA.GT.NYZA) GOTO 76
      IF (ID.EQ.1) WRITE (IU,114) FI1
      IF (ID.EQ.2) WRITE (IU,115) FI1
  114 FORMAT(   //5X,' FI1 = ',F5.1,
     1 2X,'ODF  F(G)'/)
  115 FORMAT(   //5X,' FI1 = ',F5.1,
     1 2X,'REDUCED ODF  FS(G)'/)
      DO 32 IFI=1,NYZB
      IBET = IFI
      IF (IGAB.EQ.3) IBET = 38-IFI
      IA=NYZA*(IBET-1)
      DO 33 IFI2=1,NYZGE
      IF (IGAB.EQ.1) NGA = IFI2+18
      IF (IGAB.EQ.2) NGA=20-IFI2
      IF (IGAB.EQ.3) NGA=IFI2-18
   68 IF(NGA.GT.NYZG) NGA = NGA-NYZG+1
      IF (NGA.GT.NYZG) GOTO 68
C
   86 IF (NGA.LT.1) NGA=NGA+NYZG-1
      IF (NGA.LT.1) GOTO 86
      NG=NYE*(NGA-1)+IA+IALFA
   33 FS(IFI2,IFI) = F(NG)
   32 CONTINUE
  152 FORMAT(1X,'FI2>',13I6/1X,' FI ',114(1H*))
C
          CALL PRI(IFI1,ID)
C
   31 CONTINUE
      RETURN
      END
C
C
C
C    *--------------------------------*
C
      SUBROUTINE PRI(NSEC,ID)
C
C    OUTPUT OF THE NSEC-TH ODF (ID=1,3) OR ODFS (ID=2,4) SECTION
C
C  - ON UNIT 6 (LISTING) CALLED FOR GAMMA-SECTIONS IN FOUT (ID=3 OR 4)
C
C    ID=1,2,3 uses FS(73,37) ; ID=4 uses HF(73,37) as INPUT
C
C    ITYP-SECTIONS (ID=1 OR 2) CALLED IN TYPOUT/TPRINT (ARRAY FS)
C  - ON UNIT 9  -LISTING-
C  - ON UNIT 10 -PLOT,BERKELEY FORMAT-
C
C
C    NAMET  - NAME OF THETA ANGLE   : 'BETA','FI'
C    NAMEFI - NAME OF FI ANGLE      > 'ALPHA','GAMMA','FI1','FI2'
C    NAMESE - NAME OF SECTION ANGLE > 'GAMMA','ALPHA','FI2','FI1'
C
C    FOR ID=3,4 : FS(=FUK=ODF),HF(=FUM=ODFS) ARE GIVEN BY COMMON/CF/
C
      DIMENSION IFIW(73)
      DIMENSION IMH(1368)
      CHARACTER ITITLE*40
      CHARACTER*5 NAMET,NAMEFI,NAMESE
      CHARACTER*4 MIN,MAX
      CHARACTER ISTR(73)*4, ISTR2(73)*2
      CHARACTER FSYMB(2)*6
      COMMON /CF/ FS(73,37),HF(73,37),F(197173)
      COMMON /CDIM/ NYZA,NYZB,NYZG,NYE
      COMMON /CIFIW/IFIW,IOUT,IUNIT,IUNIT1
      COMMON /CMIMA/ FMIN,FMAX,AMIN,AMAX,BMIN,BMAX,SIMIN,SIMAX
      COMMON /CPRI/ NGE,NAE,NBE,NAMET,NAMEFI,NAMESE
      COMMON /TIT/ITITLE
      COMMON /CMERK/FMAXM
      DATA FSYMB/'  f(g)',' f~(g)'/
      DATA MIN /' MIN'/
      DATA MAX /' MAX'/
      DATA ISTR /73*'----'/,ISTR2/73*'--'/
      IF (ID.LE.2) IU = IUNIT
      IF (ID.GT.2) IU = IOUT
      FSMAX = 0.
      FSMIN = 8888.0
      DO 15 NA = 1,NAE
      DO 15 NB = 1,NBE
      IF (ID.LT.4) FMM = FS(NA,NB)
      IF (ID.GT.3) FMM = HF(NA,NB)
      IF (FMM.LE.FSMAX) GOTO 14
      FSMAX = FMM
      SAMAX = IFIW(NA)
      SBMAX = IFIW(NB)
   14 IF (FMM.GE.FSMIN) GOTO 15
      FSMIN = FMM
      SAMIN = IFIW(NA)
      SBMIN = IFIW(NB)
   15 CONTINUE
      WRITE (IU,44) MIN,FSMIN,NAMET,SBMIN,NAMEFI,SAMIN
      WRITE (IU,44) MAX,FSMAX,NAMET,SBMAX,NAMEFI,SAMAX
      IFE = 1
   16 IFA = IFE
      IFE = IFA+18
      IF (IFE.GT.NAE) IFE = NAE
      WRITE(IU,42) NAMEFI,(IFIW(IFI),IFI = IFA,IFE)
      WRITE(IU,43) NAMET,(ISTR(IS),ISTR2(IS),IS=IFA,IFE)
      DO 77 NB=1,NBE
      IF (ID.LT.4)
     1 WRITE (IU,35) IFIW(NB),(FS(NA,NB),NA = IFA,IFE)
      IF (ID.GT.3)
     1 WRITE (IU,35) IFIW(NB),(HF(NA,NB),NA = IFA,IFE)
  77  CONTINUE
   36 IF (IFE.LT.NAE) GOTO 16
      IF (FSMAX.LT.FMAX) GOTO 17
      FMAX = FSMAX
      AMAX = SAMAX
      BMAX = SBMAX
      SIMAX = IFIW(NSEC)
   17 IF (FSMIN.GT.FMIN) GOTO 1
      FMIN = FSMIN
      AMIN =SAMIN
      BMIN = SBMIN
      SIMIN = IFIW(NSEC)
    1 CONTINUE
C
C     CONSTRUCTION OF THE PLOTFILE (BERKELEY-FORMAT)
C
      IF(ID.GT.2)GOTO 100
      DELT = 5.
      DELF = 5.
      TMAX = 90.
      FIMAX = 5.*(NAE-1)
      hormfak=0.
C     HORMFAK = 999./FMAXM
      IF (HORMFAK.EQ.0.)HORMFAK=1.
      normfak=HORMFAK
      nnormfak=0.
      IW =1
      ANGLE = 5.*(NSEC-1)
      IFSYMB = 1
      IF (ID.EQ.2)IFSYMB=2
      NAEH=NAE
      IF (NAE.EQ.73)NAEH=72
      NYZ = NAEH*19
      WRITE(10,40)ITITLE,NAMESE,FSYMB(IFSYMB)
      WRITE(10,50)DELT,TMAX,DELF,FIMAX,IW,IW,IW,IW,IW,nnormfak,ANGLE
      DO 60 NB=1,19
      NBFAK = (NB-1)*NAEH
      DO 60 NA=1,NAEH
      NY = NBFAK+NA
c     IF (ID.EQ.1)IMH(NY)=NINT(FS(NA,NB)*HORMFAK)
      IF (ID.EQ.1)IMH(NY)=NINT(FS(NA,NB)*HORMFAK*100)
      IF (ID.EQ.2)IMH(NY)=NINT(HF(NA,NB)*HORMFAK*100)
c     IF (ID.EQ.2)IMH(NY)=NINT(HF(NA,NB)*HORMFAK)
  60  CONTINUE
      IF (NAE.EQ.73)WRITE(10,70)(IMH(NY),NY=1,NYZ)
      IF (NAE.EQ.37)WRITE(10,71)(IMH(NY),NY=1,NYZ)
      IF (NAE.EQ.25)WRITE(10,72)(IMH(NY),NY=1,NYZ)
      IF (NAE.EQ.19)WRITE(10,73)(IMH(NY),NY=1,NYZ)
      IF (NAE.EQ.13)WRITE(10,74)(IMH(NY),NY=1,NYZ)
      WRITE(10,*)
      IF (NBE.EQ.19)GOTO 100
C
C     LOWER HALFSPHERE
C
      WRITE(10,40)ITITLE,NAMESE,FSYMB(IFSYMB)
      WRITE(10,90)DELT,TMAX,DELF,FIMAX,IW,IW,IW,IW,IW,nnormfak,ANGLE
      DO 160 NB=19,37
      NBH = 38-NB
      NBFAK = (NBH-1)*NAEH
      DO 160 NA=1,NAEH
      NY = NBFAK+NA
      IF (ID.EQ.1)IMH(NY)=NINT(FS(NA,NB)*HORMFAK)
      IF (ID.EQ.2)IMH(NY)=NINT(HF(NA,NB)*HORMFAK)
  160 CONTINUE
      IF (NAE.EQ.73)WRITE(10,70)(IMH(NY),NY=1,NYZ)
      IF (NAE.EQ.37)WRITE(10,71)(IMH(NY),NY=1,NYZ)
      IF (NAE.EQ.25)WRITE(10,72)(IMH(NY),NY=1,NYZ)
      IF (NAE.EQ.19)WRITE(10,73)(IMH(NY),NY=1,NYZ)
      IF (NAE.EQ.13)WRITE(10,74)(IMH(NY),NY=1,NYZ)
      WRITE(10,*)
  100 CONTINUE
      IF (NSEC.NE.NGE) RETURN
      FMAXM = FMAX
      WRITE (IU,45) MIN,FMIN,NAMEFI,AMIN,NAMET,BMIN,NAMESE,SIMIN
      WRITE (IU,45) MAX,FMAX,NAMEFI,AMAX,NAMET,BMAX,NAMESE,SIMAX
      RETURN
C
   35 FORMAT(1X,I3,'|',19F6.2)
   40 FORMAT(A40,1X,A5,'sections',A6)
   42 FORMAT(//1X,A5,'>',I4,18I6)
   43 FORMAT(1X,A5,19(A4,A2))
   44 FORMAT(8X,A4,'IMUM = ',F9.3,5X,
     * A5,' =',F4.0,5X,A5,' = ',F4.0)
   45 FORMAT(/2X,'F OR FS',A4,' = ',F9.3,5X,A5,' = ',F4.0,3X,
     * A5,' = ',F4.0,3X,A5,' = ',F5.1)
   50 FORMAT(1X,'O  ',1X,4F5.0,5I2,I5,10X,F5.1)
   70 FORMAT(1X,18I4)
   71 FORMAT(1X,18I4,/,1X,19I4)
   72 FORMAT(1X,18I4,/,1X, 7I4)
   73 FORMAT(1X,19I4)
   74 FORMAT(1X,13I4)
   90 FORMAT(1X,'U  ',1X,4F5.0,5I2,I5,10X,F5.1)
 3063 FORMAT(/  F5.0, '  DEGREE  -  PHI',I1,'-SECTION',
     1       T76,  '( FMAX ABS =',F7.2,'    S-FMAX =',F7.2,' )',
     2          T120,'PHI',I1,'=',F4.0)
 3064 FORMAT ('-------',19F6.2,' -------'/)
 3065 FORMAT (F5.0,2X, 19F6.2, T121,F8.0)
      END
C
C    *--------------------------------*
C
      SUBROUTINE COMPF (IK,IRED,IELAG,IGL)
C
C    SUBSIDIARY ROUTINE, ORGANIZATION OF PARAMETERS
C
      DIMENSION FGM(183)
      DIMENSION SALFA(96),SBETA(96),SGAMMA(96),
     1          CALFA(96),CBETA(96),CGAMMA(96)
      COMMON /GEIST/FGM
      COMMON/HTAB/HMFGM(183,50)
      COMMON /PAR/B,BST,SUM,SUM1,S,T,TAU,TAUS,TAUSS,
     1            TAUSSS,HNFUK,HNFUM
      COMMON /HMPAR/HB(50),HS(50),HT(50),HMNFUK(50),
     1              HMNFUM(50),HTAU(4,50),IHIGL(50)
      COMMON /HMSC/HMSA(96,50),HMSB(96,50),HMSG(96,50),
     1             HMCA(96,50),HMCB(96,50),HMCG(96,50)
      COMMON /ABGSC/SALFA,SBETA,SGAMMA,CALFA,CBETA,CGAMMA
      IGL=IHIGL(IK)
      IF (IRED.EQ.0.OR.IGL.EQ.2) GOTO 30
      DO 10 IOM=1,183
   10 FGM(IOM)=HMFGM(IOM,IK)
   30 DO 20 I=1,IELAG
      SALFA(I)=HMSA(I,IK)
      SBETA(I)=HMSB(I,IK)
      SGAMMA(I)=HMSG(I,IK)
      CALFA(I)=HMCA(I,IK)
      CBETA(I)=HMCB(I,IK)
   20 CGAMMA(I)=HMCG(I,IK)
      IF (IGL-1) 60,60,70
   60 B=HB(IK)
      S=HS(IK)
      GOTO 50
   70 T=HT(IK)
      TAUS=HTAU(3,IK)
      TAU=HTAU(4,IK)
   50 HNFUK=HMNFUK(IK)
      HNFUM=HMNFUM(IK)
      TAUSSS=HTAU(1,IK)
      TAUSS=HTAU(2,IK)
      RETURN
      END
C
C
C    *--------------------------------*
C
      SUBROUTINE COMFIB (IKF,IZGA,IZGB,IGL)
C
C    SUBSIDIARY ROUTINE, ORGANIZATION OF PARAMETERS
C
      COMMON /HVEC/XHF(24),YHF(24),ZHF(24),
     *             XYF(4),YYF(4),ZYF(4)
      COMMON /PARFIB/B,S,T,SUM,SUM1,EPT2,ZT,HNFUKF,HNFUMF
      COMMON /HMPARF/HSF(5),HTF(5),HFUKF(5),
     *             HTFIB(2,5),IGLF(5)
      COMMON /HMXYZY/HXYF(4,5),HYYF(4,5),HZYF(4,5)
      COMMON /HMXYZH/HXHF(24,5),HYHF(24,5),HZHF(24,5)
      IGL=IGLF(IKF)
      DO 20 I=1,IZGB
      XHF(I)=HXHF(I,IKF)
      YHF(I)=HYHF(I,IKF)
      ZHF(I)=HZHF(I,IKF)
   20 CONTINUE
      DO 30 I=1,IZGA
      XYF(I)=HXYF(I,IKF)
      YYF(I)=HYYF(I,IKF)
      ZYF(I)=HZYF(I,IKF)
   30 CONTINUE
      IF (IGL-1) 60,60,70
   60 S=HSF(IKF)
      GOTO 50
   70 T=HTF(IKF)
      EPT2 = HTFIB(1,IKF)
      ZT   = HTFIB(2,IKF)
   50 HNFUKF=HFUKF(IKF)
      HNFUMF=HNFUKF*0.5
      RETURN
      END
C
C    *--------------------------------*
C
      SUBROUTINE FGTAB (B,S,IB,IOUT,IK)
C
C    COMPUTES THE FG(S,OMEGA)-TABLE
C    FOR OMEGA = 1,2,...,180 DEGREES
C    FGM(182)=S, FGM(183)=B
C
      DIMENSION FGM(183)
      COMMON /GEIST/FGM
      COMMON /BSA/ BA,SA
      COMMON/HTAB/HMFGM(183,50)
      DELTA=1.E-2
      BN= B
      SN= S
      FGM(182)=S
      FGM(183)=B
      IF (IB) 10,10,30
   10 DB=ABS(SA-SN)
      IF (DB.GT.DELTA) GO TO 20
      SA=S
      BA=B
      WRITE (IOUT,100)
      GOTO 130
   20 IF (S-LOG(2.)/2.) 50,50,30
   30 DB=ABS(BA-BN)
      IF (DB.GT.DELTA) GO TO 40
      BA=B
      SA=S
      WRITE (IOUT,100)
      GO TO 130
   40 IF (B.LT.25.) GO TO 70
      IF (B.LT.60.) GO TO 60
   50 IPUNKT=100
      SCHR=0.01
      GO TO 80
   60 IPUNKT=200
      SCHR=0.005
      GO TO 80
   70 IPUNKT=400
      SCHR=0.0025
   80 DO 90 IOM=1,181
      OMEGA=IOM-1.
   90 FGM(IOM)=FG(S,OMEGA,IPUNKT,SCHR)
      BA=B
      SA=S
      WRITE (IOUT,110)
  130 DO 120 IOM=1,183
  120 HMFGM(IOM,IK)=FGM(IOM)
      RETURN
C
  100 FORMAT(80X,'FGTAB - CALCULATION WAS SKIPPED'/)
  110 FORMAT(91X,'FGTAB WAS CALCULATED'/)
      END
C
C    *--------------------------------*
C
      FUNCTION FG (S,OMEGA,IPUNKT,SCHR)
C
C    CALCULATES THE GAUSSIAN GHOST FUNCTION
C    USING IPUNKT NODES FOR THE INTEGRATION
C
      DIMENSION AM(401)
      IE=IPUNKT+1
      GRENZ=1.E-10
      TBER=0.
      B0=BES(S,0)
      AMP=1./(B0-BES(S,1))
      FGPI=0.5*B0*AMP
      IF (OMEGA) 10,10,20
   10 FG=FGPI-S
      RETURN
   20 IF (OMEGA-180.) 40,30,40
   30 FG=FGPI
      RETURN
   40 TG=0.
      PIH=ACOS(0.)
      OMERA=OMEGA*PIH/90.
      COM=COS(OMERA)
      COM2=COM**2
      SOM2=1.-COM2
      COM2H=(1.+COM)/2.
      DO 60 ITSU=1,IE
      T2=(1.-(ITSU-1)*SCHR)**2
      ARG=S*SQRT(COM2+T2*SOM2)
      AM(ITSU)=EXP(-S+ARG)*BES(ARG,0)
      IF (AM(ITSU)/AM(1)-GRENZ) 50,50,60
   50 TG=SQRT(T2)
      GO TO 70
   60 CONTINUE
      GO TO 90
   70 TBER=(1.-TG)*SCHR
      DO 80 IT=1,IE
      T2=(1.-(IT-1)*TBER)**2
      ARG=S*SQRT(COM2+T2*SOM2)
   80 AM(IT)=EXP(ARG-S)*BES(ARG,0)
   90 RIN1=SIMP(AM,TG,IPUNKT)
      IF (OMEGA-90.) 120,100,120
  100 RIN2C=0.
  110 FG=FGPI-AMP*S*COM2H*(RIN1-RIN2C)
      RETURN
  120 TG=0.
      DO 140 ITSU=1,IE
      T2=(1.-(ITSU-1)*SCHR)**2
      ARG=S*SQRT(COM2+T2*SOM2)
      AM(ITSU)=EXP(-S+ARG)*BES(ARG,1)/ARG
      IF (AM(ITSU)/AM(1)-GRENZ) 130,130,140
  130 TG=SQRT(T2)
      GO TO 150
  140 CONTINUE
      GO TO 170
  150 TBER=(1.-TG)*SCHR
      DO 160 IT=1,IE
      T2=(1.-(IT-1)*TBER)**2
      ARG=S*SQRT(COM2+T2*SOM2)
  160 AM(IT)=EXP(-S+ARG)*BES(ARG,1)/ARG
  170 RIN2C=COM*SIMP(AM,TG,IPUNKT)*S
      GO TO 110
      END
C
C    *--------------------------------*
C
      FUNCTION SIMP (AM,TG,IPUNKT)
C
C    SIMPSON INTEGRATION PROCEDURE
C
      DIMENSION AM(401)
      J=IPUNKT+1
      FAK=IPUNKT*3
      IE=IPUNKT/2
      S=AM(1)-AM(J)
      DO 10 I=1,IE
   10 S=S+4.*AM(2*I)+2.*AM(2*I+1)
      SIMP=S*(1.-TG)/FAK
      RETURN
      END
C
C    *------------------------------*
C
      FUNCTION BES (XX,IND)
C
C    CALCULATES BESSELFUNCTIONS I0(XX)*EXP(-XX) FOR IND=0
C    RESPECTIVELY I1(XX)*EXP(-XX) FOR IND=1
C    XX GE.NULL
C
      DOUBLE PRECISION A,B,C,D
      DIMENSION A(14),B(10),C(14),D(10)
      DATA A/
     1 0.926522283D-27,0.665920310D-25,0.187543306D-21,
     2 0.678818504D-19,0.293709299D-16,0.936223818D-14,0.240378975D-11,
     3 0.470922041D-09,0.678173901D-7,0.678167711D-5,0.434027830D-3,
     4 0.156249998D-1,0.25D+00,0.1D+1/,
     5 B/
     6 0.228201527D-7,0.460958396D-7,0.542737131D-7,0.204865735D-6,
     7 0.911964648D-6,0.448357120D-5,0.292186032D-4,0.280504770D-3,
     8 0.498677852D-2,0.398942281D+0/,
     9 C/
     X 0.312719049D-28,0.378500541D-26,0.744839607D-23,0.314048171D-20,
     X 0.146127307D-17,0.520686381D-15,0.150206750D-12,0.336384070D-10,
     X 0.565142090D-8,0.678168183D-6,0.542534739D-4,0.260416666D-2,
     X 0.625D-1,0.5D+00/,
     X D/
     X  -0.250141085D-7,-0.513119520D-7,-0.637466188D-7,-0.244333379D-6,
     X  -0.111375439D-5,-0.576272322D-5,-0.409063053D-4,-0.467509130D-3,
     X -0.149603355D-1,+0.398942281D+00/
      IF (IND.EQ.0) GO TO 50
      X=XX
      IF (ABS(X)-10.) 10,30,30
   10 FI2=C(1)
      V=X**2
      DO 20 L=1,13
   20 FI2=FI2*V+C(L+1)
      BES=FI2*X*EXP(-X)
      RETURN
   30 FI3=D(1)
      X=ABS(X)
      Y=10./X
      DO 40 L=1,9
   40 FI3=FI3*Y+D(L+1)
      BES=FI3/SQRT(X)
      IF (XX.LT.0.) BES=-BES
      RETURN
   50 X=ABS(XX)
      IF (X-10.) 60,80,80
   60 FI=A(1)
      U=X**2
      DO 70 K=1,13
   70 FI=FI*U+A(K+1)
      BES=FI*EXP(-X)
      RETURN
   80 FI1=B(1)
      Y=10./X
      DO 90 K=1,9
   90 FI1=FI1*Y+B(K+1)
      BES=FI1/SQRT(X)
      RETURN
      END
C
C    *--------------------------------*
C
      SUBROUTINE SC48 (CAN,SAN,CBN,SBN,CGN,SGN)
C
C    INPUT COS,SIN (ALPHA0,BETA0,GAMMA0) OF G0
C    CALCULATION OF SIN COS (ALPHA,BETA,GAMMA) FOR
C    THE 48 HEXAGONAL-ORTHORHOMBIC EQUIVALENT POSITIONS
C
      DIMENSION SALFA(96),SBETA(96),SGAMMA(96),
     1          CALFA(96),CBETA(96),CGAMMA(96)
      COMMON /ABGSC/ SALFA,SBETA,SGAMMA,CALFA,CBETA,CGAMMA
      DO 10 I=1,48
   10 SBETA(I)=SBN
      DO 20 I=1,6
      SALFA(I)=SAN
      CALFA(I)=CAN
   20 CBETA(I)=CBN
      SGAMMA(1)=SGN
      CGAMMA(1)=CGN
      S3=SQRT(3.)
      SGAMMA(2)=(S3*CGN+SGN)/2.
      SGAMMA(3)=(S3*CGN-SGN)/2.
      CGAMMA(2)=(CGN-S3*SGN)/2.
      CGAMMA(3)=(-CGN-S3*SGN)/2.
      DO 30 I=4,6
      SGAMMA(I)=-SGAMMA(I-3)
   30 CGAMMA(I)=-CGAMMA(I-3)
      DO 40 J=1,7
      DO 40 I=1,6
      K=I+J*6
      ISA=(-1)**((J+1)/2)
      ISG=(-1)**(J/4)
      ICA=ISA*ISG
      ICBG=(-1)**(5*J/4)
      SALFA(K)=ISA*SALFA(I)
      CALFA(K)=ICA*CALFA(I)
      CBETA(K)=ICBG*CBETA(I)
      SGAMMA(K)=ISG*SGAMMA(I)
      CGAMMA(K)=ICBG*CGAMMA(I)
   40 CONTINUE
      RETURN
      END
C
C    *--------------------------------*
C
      SUBROUTINE SC96 (CAN,SAN,CBN,SBN,CGN,SGN)
C
C    INPUT COS,SIN (ALPHA0,BETA0,GAMMA0) OF G0
C    CALCULATION OF SIN COS (ALPHA,BETA,GAMMA) FOR
C    THE 96 CUBIC-ORTHORHOMBIC EQUIVALENT POSITIONS
C
      DIMENSION SALFA(96),SBETA(96),SGAMMA(96),
     1          CALFA(96),CBETA(96),CGAMMA(96)
      COMMON /ABGSC/SALFA,SBETA,SGAMMA,CALFA,CBETA,CGAMMA
C
      GKLEIN=1.E-16
      SALFA(1)=SAN
      CALFA(1)=CAN
      SBETA(1)=SBN
      CBETA(1)=CBN
      SGAMMA(1)=SGN
      CGAMMA(1)=CGN
C
C    DETERMINATION ALPHAA,BETAA,GAMMAA
C
      HILFSG=SBN*SGN
      CBETA(2)=HILFSG
      S2=1.0-HILFSG*HILFSG
      IF (S2.GT.0.) GO TO 10
      S2=0.
   10 HILFSG=SQRT(S2)
      SBETA(2)=HILFSG
      IF (HILFSG.LT.GKLEIN) GO TO 20
      CGAMMA(2)=CBN/HILFSG
      SGAMMA(2)=SBN*CGN/HILFSG
      CALFA(2)=-(CAN*CBN*SGN+SAN*CGN)/HILFSG
      SALFA(2)=(-SAN*CBN*SGN+CAN*CGN)/HILFSG
      GO TO 30
   20 CGAMMA(2)=1.
      SGAMMA(2)=0.
      CALFA(2)=CAN*SBN
      SALFA(2)=SAN*SBN
      IF (CBETA(2).LT.0.) GO TO 30
      CALFA(2)=-CALFA(2)
      SALFA(2)=-SALFA(2)
C
C    DETERMINATION ALPHAB,BETAB,GAMMAB
C
   30 HILFSG=SBN*CGN
      CBETA(3)=HILFSG
      S2=1.-HILFSG*HILFSG
      IF (S2.GT.0.) GO TO 40
      S2=0.
   40 HILFSG=SQRT(S2)
      SBETA(3)=HILFSG
      IF (HILFSG.LT.GKLEIN) GO TO 50
      CGAMMA(3)=SBN*SGN/HILFSG
      SGAMMA(3)=CBN/HILFSG
      CALFA(3)=-(CAN*CBN*CGN-SAN*SGN)/HILFSG
      SALFA(3)=-(SAN*CBN*CGN+CAN*SGN)/HILFSG
      GO TO 60
   50 CGAMMA(3)=1.
      SGAMMA(3)=0.
      CALFA(3)=CAN*CBN*SGN+SAN*CGN
      SALFA(3)=SAN*CBN*SGN-CAN*CGN
      IF (CBETA(3).GT.0.) GO TO 60
      CALFA(3)=-CALFA(3)
      SALFA(3)=-SALFA(3)
C
C    CYCLES OF SIGNS FOR THE 96 CASES
C
   60 DO 100 I=1,3
      DO 90 K=1,8
      J=(K-1)*3
      IK11=(-1)**(K+1)
      IK02=(-1)**(K/2)
      DO 80 L=1,4
      M=(L-1)*24
      SBETA(M+J+I)=SBETA(I)
      IL11=(-1)**(L+1)
      IL02=(-1)**(L/2)
      SALFA(M+J+I)=IL11*IK11*SALFA(I)
      CALFA(M+J+I)=IL02*IK11*CALFA(I)
      IL32=(-1)**((L+3)/2)
      CBETA(M+J+I)=IL32*IK11*CBETA(I)
      IF (IK02.GT.0) GO TO 70
      IK54=(-1)**((K+5)/4)
      IK34=(-1)**((K+3)/4)
      SGAMMA(M+J+I)=IK54*IL32*CGAMMA(I)
      CGAMMA(M+J+I)=IK34*IL32*SGAMMA(I)
      GO TO 80
   70 IK74=(-1)**((K+7)/4)
      IK14=(-1)**((K+1)/4)
      SGAMMA(M+J+I)=IK14*IL32*SGAMMA(I)
      CGAMMA(M+J+I)=IK74*IL32*CGAMMA(I)
   80 CONTINUE
   90 CONTINUE
  100 CONTINUE
      RETURN
      END
C
C    *--------------------------------*
C
      SUBROUTINE PRCINT(X,ARG,VAL,Y)
C
C    INTERPOLATION PROCEDURE
C    USING 3 OR 4 INTERPOLATION NODES
C    IN DEPENDENCE WETHER THE ABSOLUTE ERROR IS
C    LOWER 0.05 OR NOT
C
      DIMENSION ARG(4),VAL(4)
      EPS=0.05
      DELT2=0.
      DO 60 J=2,4
      IEND=J-1
      DO 20 I=1,IEND
      H=ARG(I)-ARG(J)
   20 VAL(J)=(VAL(I)*(X-ARG(J))-VAL(J)*(X-ARG(I)))/H
      DELT2=ABS(VAL(J)-VAL(IEND))
      IF (J-2) 60,60,30
   30 IF (DELT2-EPS) 80,80,60
   60 CONTINUE
      J=4
   80 Y=VAL(J)
   90 RETURN
      END
C
C
C    *--------------------------------*
C
      SUBROUTINE VXYZ24 (GXN, GYN, GZN)
C
      COMMON /XYZ/ X(24),Y(24),Z(24)
      X(1)=GXN
      Y(1)=GYN
      Z(1)=GZN
      X(2)=-GZN
      Y(2)=-GXN
      Z(2)=GYN
      X(3)=-GYN
      Y(3)=GZN
      Z(3)=-GXN
C
      DO 10 I=4,6
      II=I-3
      X(I)=Y(II)
      Y(I)=X(II)
   10 Z(I)=-Z(II)
C
      DO 20 J=7,12
      J10=0.1*J
      JJ=J-3-6*J10
      X(J)=X(JJ)
      Y(J)=-Y(JJ)
   20 Z(J)=Z(J-6)
C
      DO 30 K=13,24
      KK=K-12
      X(K)=-X(KK)
      Y(K)=-Y(KK)
   30 Z(K)=Z(KK)
C
      RETURN
      END
C
C
C    *--------------------------------*
C
      SUBROUTINE VXYZ12 (GXN, GYN, GZN)
C
      COMMON /XYZ/ X(24),Y(24),Z(24)
      S3=SQRT(3.)
      X(1)=GXN
      Y(1)=GYN
C
      DO 10 I=1,6
   10 Z(I)=GZN
C
      X(2)=0.5*(GXN+S3*GYN)
      Y(2)=0.5*(GYN-S3*GXN)
      X(3)=0.5*(-GXN+S3*GYN)
      Y(3)=0.5*(-GYN-S3*GXN)
C
      DO 20 J=4,6
      JJ=J-3
      X(J)=-X(JJ)
   20 Y(J)=-Y(JJ)
C
      DO 30 K=7,12
      KK=K-6
      X(K)=-X(KK)
      Y(K)=Y(KK)
   30 Z(K)=-GZN
      RETURN
      END
C
C
C    *--------------------------------*
C
      SUBROUTINE VECTOR (THETA,FI,X,Y,Z)
C
      COMMON /CPI/PI
      PIT = THETA/PI
      FIT = FI/PI
      Z = COS(PIT)
      ST = SIN(PIT)
      Y = SIN(FIT)*ST
      X = COS(FIT)*ST
      RETURN
      END

      SUBROUTINE GETTIM(I1,I2,I3,I4)
      I1=11
      I2=22
      I3=33
      I4=44
      RETURN
      END

*/
