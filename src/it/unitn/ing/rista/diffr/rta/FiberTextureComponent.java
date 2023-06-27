/*
 * @(#)FiberTextureComponent.java created May 1, 2004 Braila
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

import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.diffr.Parameter;
import it.unitn.ing.rista.diffr.FilePar;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.ParameterPreferences;


/**
 * The FiberTextureComponent is a class
 * 
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:57 $
 * @since JDK1.1
 */

public class FiberTextureComponent extends XRDcat {

  protected static String[] diclistc = {"_texture_fiber_component_id",
                                        "_texture_fiber_component_intensity",
                                        "_texture_fiber_component_thetaY",
                                        "_texture_fiber_component_phiY",
                                        "_texture_fiber_component_thetaH",
                                        "_texture_fiber_component_phiH",
                                        "_texture_fiber_component_fwhm", // only in degrees
                                        "_texture_fiber_component_gauss_content"
  };
  protected static String[] diclistcrm = {"_texture_fiber_component_id",
                                        "weight",
                                        "component position thetaY (deg)",
                                        "component position phiY (deg)",
                                        "component position thetaH (deg)",
                                        "component position phiH (deg)",
                                        "fwhm (deg)", // only in degrees
                                        "gaussian content"
  };

  protected static String[] classlistc = {};

  public double intensity;
  public double[] position = new double[4];
  public double betag;
  public double gauss;
  public double HNFUKG, HNFUKL, HNFUKL_ODF, EPT2, ZT, FB, FS;
  public double[] HXHF = new double[24], HYHF = new double[24], HZHF = new double[24];
  public double[] HXYF = new double[24], HYYF = new double[24], HZYF = new double[24];

  public static final double SGRENZ = Math.log(2.) / 2.;

  public FiberTextureComponent(XRDcat obj, String alabel) {
    super(obj, alabel);
    initBaseObject();
  }

  public FiberTextureComponent(XRDcat afile) {
    this(afile, "FiberTextureComponent x");
  }

	public FiberTextureComponent() {}

  public void initConstant() {
    Nstring = 1;
    Nstringloop = 0;
    Nparameter = 7;
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
    parameterField[0] = new Parameter(this, getParameterString(0), 0.5,
              ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.0),
              ParameterPreferences.getDouble(getParameterString(0) + ".max", 1.0));
    parameterField[0].setPositiveOnly();
    parameterField[1] = new Parameter(this, getParameterString(1), 10.0,
              ParameterPreferences.getDouble(getParameterString(1) + ".min", 0.0),
              ParameterPreferences.getDouble(getParameterString(1) + ".max", 90.0));
    parameterField[2] = new Parameter(this, getParameterString(2), 20.0,
              ParameterPreferences.getDouble(getParameterString(2) + ".min", 0.0),
              ParameterPreferences.getDouble(getParameterString(2) + ".max", 360.0));
    parameterField[3] = new Parameter(this, getParameterString(3), 10.0,
              ParameterPreferences.getDouble(getParameterString(3) + ".min", 0.0),
              ParameterPreferences.getDouble(getParameterString(3) + ".max", 90.0));
    parameterField[4] = new Parameter(this, getParameterString(4), 20.0,
              ParameterPreferences.getDouble(getParameterString(4) + ".min", 0.0),
              ParameterPreferences.getDouble(getParameterString(4) + ".max", 360.0));
    parameterField[5] = new Parameter(this, getParameterString(5), 30,
              ParameterPreferences.getDouble(getParameterString(5) + ".min", 0.0),
              ParameterPreferences.getDouble(getParameterString(5) + ".max", 360.0));
    parameterField[5].setPositiveOnly();
    parameterField[6] = new Parameter(this, getParameterString(6), 0.5,
              ParameterPreferences.getDouble(getParameterString(6) + ".min", 0.0),
              ParameterPreferences.getDouble(getParameterString(6) + ".max", 1.0));
    parameterField[6].setPositiveOnly();
  }

	public void rotateODFBy(double alpha, double beta, double gamma, int multAlpha, int multBeta, int multGamma) {
		double alpha_a = parameterField[1].getValueD();
		beta = multBeta * parameterField[1].getValueD() + beta;
		while (beta >= 360)
			beta -= 360;
		while (beta < -90) {
			beta += 180;
			alpha_a += 180;
		}
		while (beta >= 180) {
			beta -= 180;
			alpha_a += 180;
		}
		while (beta > 90) {
			beta = 180 - beta;
			alpha_a += 180;
		}
		if (beta < -90) {
			beta = 90 + beta;
		}
		parameterField[1].setValue(beta);
		alpha = multAlpha * alpha_a + alpha;
		while (alpha < 0)
			alpha += 360;
		while (alpha >= 360)
			alpha -= 360;
		parameterField[2].setValue(alpha);
	}

	public Parameter getIntensity() {
    return parameterField[0];
  }

  public Parameter getPosition(int i) {
    return parameterField[1 + i];
  }

  public Parameter getHWHM() {
    return parameterField[5];
  }

  public Parameter getGauss() {
    return parameterField[6];
  }

  public void update(boolean firstLoading) {
    intensity = getIntensity().getValueD();
    for (int i = 0; i < 4; i++) {
      position[i] = getPosition(i).getValueD() * Constants.DEGTOPI;
    }
    betag = getHWHM().getValueD();
    gauss = getGauss().getValueD();
    if (gauss < 0)
      gauss = 0.0;
    if (gauss > 1.0)
      gauss = 1.0;
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
	        notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED, -1);
	        notifyParameterChanged(source, Constants.STRAIN_CHANGED, -1);
          notifyParameterChanged(source, Constants.TEXTURE_CHANGED, -1);
          return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
	          notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED, -1);
	          notifyParameterChanged(source, Constants.STRAIN_CHANGED, -1);
            notifyParameterChanged(source, Constants.TEXTURE_CHANGED, -1);
            return;
          }

      super.notifyParameterChanged(source);
    }
  }

	public void refreshForNotificationDown(XRDcat source, int reason) {
		if (!getFilePar().isComputingDerivate() || (source == this ||
				(reason == Constants.SAMPLE_ORIENTATION_CHANGED || reason == Constants.TEXTURE_CHANGED
						|| (source == getParent() &&
						(reason == Constants.STRAIN_CHANGED || reason == Constants.CELL_CHANGED))))) {
			refreshComputation = true;
			//     System.out.println("Reason " + reason + " Source " + source.toXRDcatString());
		}
	}

	public void updateParametertoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);

    parameterField[0].setPositiveOnly();
    parameterField[5].setPositiveOnly();
    parameterField[6].setPositiveOnly();
  }

  public double computeIntensity(double[] thetaord, double alphama, double betama) {
    return 0.0;
  }

  public void prepareComputation(int IZE) {
	  StandardFunctionTexture sft = (StandardFunctionTexture) getParent();
    double[] xyzY = VECTOR(position[0], position[1]);
    double[] xyzH = VECTOR(position[2], position[3]);
    PARFP(betag);
    double[][] FXYZ;
    if (sft.IGB < 8)
      FXYZ = VXYZ24(xyzH[0], xyzH[1], xyzH[2]);
    else
      FXYZ = VXYZ12(xyzH[0], xyzH[1], xyzH[2]);
    int IEL = -1;
    for (int IJ = 0; IJ < IZE; IJ++) {
      if (StandardFunctionTexture.MGE[IJ][sft.IGB - 1] == 1) {
        IEL++;
        HXHF[IEL] = FXYZ[0][IJ];
        HYHF[IEL] = FXYZ[1][IJ];
        HZHF[IEL] = FXYZ[2][IJ];
      }
    }
    FXYZ = VXYZ24(xyzY[0], xyzY[1], xyzY[2]);
    IEL = -1;
    for (int IJ = 0; IJ < 24; IJ++) {
      if (StandardFunctionTexture.MGE[IJ][sft.IGA - 1] == 1) {
        IEL++;
        HXYF[IEL] = FXYZ[0][IJ];
        HYYF[IEL] = FXYZ[1][IJ];
        HZYF[IEL] = FXYZ[2][IJ];
      }
    }
  }

/*
 *   SPHERICAL COORD. ---> CARTHESIAN COORD.
 */
  public double[] VECTOR(double THETA, double FI) {
//      COMMON /CPI/PI,PIF
    double[] XYZ = new double[3];
    double PIT = THETA;
    double FIT = FI;
    XYZ[2] = Math.cos(PIT);
    double ST = Math.sin(PIT);
    XYZ[1] = Math.sin(FIT) * ST;
    XYZ[0] = Math.cos(FIT) * ST;
    return XYZ;
  }

  /*
   *     CALCULATES GAUSSIAN AND LORENTZIAN WORKING PARAMETERS
   *     FOR FIBRE COMPONENTS
   */
  public void PARFP(double betaGauss) {
    double S, T;
//    double HNFUKG, HNFUKL;

	  StandardFunctionTexture sft = (StandardFunctionTexture) getParent();
	  double betaGaussR = betaGauss * Constants.DEGTOPI;

    int NAB = sft.IZGA * sft.IZGB;
    // Gauss part
    double BR4 = Math.sin(betaGaussR * 0.25);
    S = SGRENZ / (BR4 * BR4);
    HNFUKG = intensity / NAB * gauss;
    if (S < 1.E-10)
      HNFUKG = 0.5 * HNFUKG;
    else {
      HNFUKG = HNFUKG * S;
      if (S < 20.)
        HNFUKG = HNFUKG / (1. - Math.exp(-2. * S));
      else
        HNFUKG = 0.5 * HNFUKG;
    }
    // Lorentz part
    double Z23 = Math.pow(4., 1. / 3.);
    double RN = Z23 - 1.;
    double BR2 = betaGaussR * 0.5;
    double R = (Z23 - Math.cos(BR2)) / RN;
    T = R - Math.sqrt(R * R - 1.);

    double T2 = T * T; // 120
	  HNFUKL_ODF = intensity / NAB * (1. - T2) * (1. - gauss);
    HNFUKL = intensity / NAB * (1. - T2) / Constants.PI * (1. - gauss);

    EPT2 = 1. + T2;
    ZT = 2. * T;
//    HNF=(HNFUKG * gauss + HNFUKL * (1.0 - gauss));
    FB = betaGauss;
    FS = S;
//    FT = T;

//    System.out.println("Fiber: " + HNFUKG + " " + HNFUKL + " " + intensity + " " + NAB + " " + EPT2 + " "
//        + ZT + " " + FB + " " + FS);
  }

  /*
   *    CALCULATION OF  P(SF,Z1,Z2)+P(SF,-Z1,Z2)  - GAUSS STANDARD FIBRE
   *    REDUCING EXP IS COMPENSATED IN PARFP AND Bessel
   */
  public static double PSGF(double ZZ, double WW, double SF) {
    double ARG1 = SF * (ZZ + WW - 1.);
    double ARG2 = SF * (-ZZ + WW - 1.);
    double P1 = 0., P2 = 0.;
    if (ARG1 > -80.)
      P1 = Math.exp(ARG1);
    if (ARG2 > -80.)
      P2 = Math.exp(ARG2);
    return SphericalTextureComponent.Bessel(SF * WW, 0) * (P1 + P2);
  }


  /*
   *    CALCULATION OF  P(TF,Z1,Z2)+P(TF,-Z1,Z2)  - LORENTZ STANDARD FIBRE
   */
  public static double PSLF(double ZZ, double WW, double EPT2, double ZT) {
    double C1 = EPT2 - ZT * ZZ;
    double C2 = EPT2 + ZT * ZZ;
    double D = ZT * WW;
    double C1D = C1 + D;
    double C2D = C2 + D;
    double P1 = CEIS(2 * D / C1D) / (C1 - D) / Math.sqrt(C1D);
    double P2 = CEIS(2 * D / C2D) / (C2 - D) / Math.sqrt(C2D);
    return P1 + P2;
  }

  static final double[] Ace = {0.44325141463, 0.06260601220,
                               0.04757383546, 0.01736506451},
  Bce = {0.24998368310, 0.09200180037,
         0.04069697526, 0.00526449639};

  public static double CEIS(double X) {
/*
 *    CALCULATION OF THE COMPLETE ELLIPTIC INTEGRAL
 *    OF THE SECOND KIND E(M)
 *
 *    INPUT (0.LE.X.LE.1.)
 */
    double CEIS;
    if (X >= 0.0 && X <= 1.0) {
      double X1 = 1. - X;
      if (X1 >= 1.E-9) {
        double SA = 0.;
        for (int I = 0; I < 4; I++) {
          SA = SA + Ace[3 - I];
          SA = SA * X1;
        }
        double SB = 0.;
        for (int K = 0; K < 4; K++) {
          SB = SB + Bce[3 - K];
          SB = SB * X1;
        }
        double FAK = Math.log(1. / X1);
        CEIS = 1. + SA + SB * FAK;
      } else
        CEIS = 1.;
    } else
      CEIS = 8888.;
    return CEIS;
  }

/*
 *    CALCULATION OF 24 CUBIC SYMM. EQUIVALENT VECTORS
 */

  public double[][] VXYZ24(double GXN, double GYN, double GZN) {
//      COMMON /XYZ/ X(24),Y(24),Z(24)
    double[][] XYZ = new double[3][24];
    XYZ[0][0] = GXN;
    XYZ[1][0] = GYN;
    XYZ[2][0] = GZN;
    XYZ[0][1] = -GZN;
    XYZ[1][1] = -GXN;
    XYZ[2][1] = GYN;
    XYZ[0][2] = -GYN;
    XYZ[1][2] = GZN;
    XYZ[2][2] = -GXN;

    for (int I = 3; I < 6; I++) {
      int II = I - 3;
      XYZ[0][I] = XYZ[1][II];
      XYZ[1][I] = XYZ[0][II];
      XYZ[2][I] = -XYZ[2][II];
    }
    for (int J = 6; J < 12; J++) {
      int J10 = (int) (0.1 * (J + 1));
      int JJ = J - 3 - 6 * J10;
      XYZ[0][J] = XYZ[0][JJ];
      XYZ[1][J] = -XYZ[1][JJ];
      XYZ[2][J] = XYZ[2][J - 6];
    }
    for (int K = 12; K < 24; K++) {
      int KK = K - 12;
      XYZ[0][K] = -XYZ[0][KK];
      XYZ[1][K] = -XYZ[1][KK];
      XYZ[2][K] = XYZ[2][KK];
    }
    return XYZ;
  }


/*
 *    CALCULATION OF 12 HEXAGONAL SYMM. EQUIVALENT VECTORS
 */
  public double[][] VXYZ12(double GXN, double GYN, double GZN) {
//      COMMON /XYZ/ X(24),Y(24),Z(24)
    double[][] XYZ = new double[3][24];
    double S3 = Math.sqrt(3.);
    XYZ[0][0] = GXN;
    XYZ[1][0] = GYN;

    for (int I = 0; I < 6; I++)
      XYZ[2][I] = GZN;

    XYZ[0][1] = 0.5 * (GXN + S3 * GYN);
    XYZ[1][1] = 0.5 * (GYN - S3 * GXN);
    XYZ[0][2] = 0.5 * (-GXN + S3 * GYN);
    XYZ[1][2] = 0.5 * (-GYN - S3 * GXN);

    for (int J = 3; J < 6; J++) {
      int JJ = J - 3;
      XYZ[0][J] = -XYZ[0][JJ];
      XYZ[1][J] = -XYZ[1][JJ];
    }
    for (int K = 6; K < 12; K++) {
      int KK = K - 6;
      XYZ[0][K] = -XYZ[0][KK];
      XYZ[1][K] = XYZ[1][KK];
      XYZ[2][K] = -GZN;
    }
    return XYZ;
  }


//    FIBRE COMPONENT  -  GAUSSIAN-LORENTZIAN

	public double[] ovfib(int izga, int izgb, int ired,
	                      double ca, double sa, double cb, double sb, double cg, double sg) {
		double z;
		double[] hx = new double[izga], hy = new double[izga], hz = new double[izga];
		int ija, ijb;
		double arg, ugr, arg1;
		double[] sum = new double[2];
		sum[0] = 0;
		sum[1] = 0;

		ugr = -170.f;
		for (ija = 0; ija < izga; ija++) {
			hx[ija] = (ca * cb * cg - sa * sg) * HXYF[ija] + (sa * cb * cg + ca * sg) * HYYF[ija] - sb * cg * HZYF[ija];
			hy[ija] = -(ca * cb * sg + sa * cg) * HXYF[ija] - (sa * cb * sg - ca * cg) * HYYF[ija] + sb * sg * HZYF[ija];
			hz[ija] = ca * sb * HXYF[ija] + sa * sb * HYYF[ija] + cb * HZYF[ija];
			for (ijb = 0; ijb < izgb; ijb++) {
				z = HXHF[ijb] * hx[ija] + HYHF[ijb] * hy[ija] + HZHF[ijb] * hz[ija];
				sum[0] += HNFUKL_ODF / Math.pow(EPT2 - ZT * z, 1.5);
				arg1 = (z - 1.) * FS;
				if (arg1 > ugr)
					sum[0] += HNFUKG * Math.exp(arg1);
				if (ired != 0) {
					sum[1] += HNFUKL_ODF / Math.pow(EPT2 + ZT * z, 1.5);
					arg = -(z + 1.) * FS;
					if (arg > ugr)
						sum[1] += HNFUKG * Math.exp(arg);
				}
			}
		}
		return sum;
	} // ovfibg_

}
