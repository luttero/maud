/*
 * @(#)SphericalTextureComponent.java created May 1, 2004 Braila
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

import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.ParameterPreferences;
import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.diffr.Parameter;
import it.unitn.ing.rista.diffr.FilePar;


/**
 * The SphericalTextureComponent is a class
 * 
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:57 $
 * @since JDK1.1
 */

public class SphericalTextureComponent extends XRDcat {

/*
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
*/

  protected static String[] diclistc = {"_texture_spherical_component_id",
                                        "_texture_spherical_component_intensity",
                                        "_texture_spherical_component_alpha",
                                        "_texture_spherical_component_beta",
                                        "_texture_spherical_component_gamma",
                                        "_texture_spherical_component_fwhm", // only in degrees
                                        "_texture_spherical_component_gauss_content"
  };
  protected static String[] diclistcrm = {"_texture_spherical_component_id",
                                        "weight",
                                        "component position alpha (deg)",
                                        "component position beta (deg)",
                                        "component position gamma (deg)",
                                        "fwhm (deg)", // only in degrees
                                        "gaussian content"
  };

  protected static String[] classlistc = {};

  public double intensity;
  public double[] position = new double[3];
  public double betag;
  public double gauss;
  public static final double SGRENZ = Math.log(2.) / 2.;
  public double HNG, HNL, HL1, HL2, HL3, S, B, TAU, TAUS;
  public double[] HMSA = new double[96],
                  HMSB = new double[96],
                  HMSG = new double[96],
                  HMCA = new double[96],
                  HMCB = new double[96],
                  HMCG = new double[96];

  public SphericalTextureComponent(XRDcat obj, String alabel) {
    super(obj, alabel);
    initXRD();
  }

  public SphericalTextureComponent(XRDcat afile) {
    this(afile, "SphericalTextureComponent x");
  }

	public SphericalTextureComponent() {}

  public void initConstant() {
    Nstring = 1;
    Nstringloop = 0;
    Nparameter = 6;
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
              ParameterPreferences.getDouble(getParameterString(1) + ".max", 360.0));
    parameterField[2] = new Parameter(this, getParameterString(2), 5.0,
              ParameterPreferences.getDouble(getParameterString(2) + ".min", 0.0),
              ParameterPreferences.getDouble(getParameterString(2) + ".max", 180.0));
    parameterField[3] = new Parameter(this, getParameterString(3), 10.0,
              ParameterPreferences.getDouble(getParameterString(3) + ".min", 0.0),
              ParameterPreferences.getDouble(getParameterString(3) + ".max", 360.0));
    parameterField[4] = new Parameter(this, getParameterString(4), 30.0,
              ParameterPreferences.getDouble(getParameterString(4) + ".min", 0.0),
              ParameterPreferences.getDouble(getParameterString(4) + ".max", 360.0));
    parameterField[4].setPositiveOnly();
    parameterField[5] = new Parameter(this, getParameterString(5), 0.5,
              ParameterPreferences.getDouble(getParameterString(5) + ".min", 0.0),
              ParameterPreferences.getDouble(getParameterString(5) + ".max", 1.0));
    parameterField[5].setPositiveOnly();
  }

	public void rotateODFBy(double alpha, double beta, double gamma, int multAlpha, int multBeta, int multGamma) {
  	  alpha = multAlpha * parameterField[1].getValueD() + alpha;
  	  while (alpha < 0)
  	  	alpha += 360;
		while (alpha >= 360)
			alpha -= 360;
  	  parameterField[1].setValue(alpha);
		beta = multBeta * parameterField[2].getValueD() + beta;
		while (beta < 0)
			beta += 180;
		while (beta >= 180)
			beta -= 180;
		parameterField[2].setValue(beta);
		gamma = multGamma * parameterField[3].getValueD() + gamma;
		while (gamma < 0)
			gamma += 360;
		while (gamma >= 360)
			gamma -= 360;
		parameterField[3].setValue(gamma);
	}

	public Parameter getIntensity() {
    return parameterField[0];
  }

  public Parameter getPosition(int i) {
    return parameterField[1 + i];
  }

  public Parameter getHWHM() {
    return parameterField[4];
  }

  public Parameter getGauss() {
    return parameterField[5];
  }

  public void update(boolean firstLoading) {
    intensity = getIntensity().getValueD();
    for (int i = 0; i < 3; i++) {
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
	        notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED);
	        notifyParameterChanged(source, Constants.STRAIN_CHANGED);
          notifyParameterChanged(source, Constants.TEXTURE_CHANGED);
          return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
	          notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED);
	          notifyParameterChanged(source, Constants.STRAIN_CHANGED);
            notifyParameterChanged(source, Constants.TEXTURE_CHANGED);
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
    parameterField[4].setPositiveOnly();
    parameterField[5].setPositiveOnly();
  }

  public double computeIntensity(double[] thetaord, double alphama, double betama) {
    return 0.0;
  }

  public void prepareComputation(int IELAG) {

    double[] SALFA = new double[IELAG], CALFA = new double[IELAG], SBETA = new double[IELAG],
             CBETA = new double[IELAG], SGAMMA = new double[IELAG], CGAMMA = new double[IELAG];

	  StandardFunctionTexture sft = (StandardFunctionTexture) getParent();
    PARGLP(betag);
    double CAN = Math.cos(position[0]);
    double SAN = Math.sin(position[0]);
    double CBN = Math.cos(position[1]);
    double SBN = Math.sin(position[1]);
    double CGN = Math.cos(position[2]);
    double SGN = Math.sin(position[2]);
    if (sft.IGB < 8)
      Uwimvuo.sc96(CAN, SAN, CBN, SBN, CGN, SGN, SALFA, CALFA, SBETA, CBETA, SGAMMA, CGAMMA);
    else
      Uwimvuo.sc48(CAN, SAN, CBN, SBN, CGN, SGN, SALFA, CALFA, SBETA, CBETA, SGAMMA, CGAMMA);
    for (int i = 0; i < IELAG; i++) {
      HMSA[i] = SALFA[i];
      HMSB[i] = SBETA[i];
      HMSG[i] = SGAMMA[i];
      HMCA[i] = CALFA[i];
      HMCB[i] = CBETA[i];
      HMCG[i] = CGAMMA[i];
    }
  }

  public void PARGLP(double beta) {

	  StandardFunctionTexture sft = (StandardFunctionTexture) getParent();
    double ZGE = sft.IZGA * sft.IZGB;
	  B = beta;
    double BR4 = Math.sin(beta * Constants.DEGTOPI / 4.0);
    S = SGRENZ / (BR4 * BR4);
    HNG = 0.5 * intensity / (Bessel(S, 0) - Bessel(S, 1)) / ZGE * gauss;

    BR4 = Math.cos(beta * Constants.DEGTOPI / 4.);
    double C = BR4 * BR4;
    double C2 = C * C;
    double C3 = C * C2;
    double AC = 19. * C2 - 34. * C + 19.;
    double HG = (-82. * C3 + 240. * C2 - 246. * C + 80.) / Math.pow(AC, 1.5);
    HG = Math.acos(HG) / 3.;
    TAU = (2. * Math.sqrt(AC) * Math.cos(HG) + (5. - 4. * C)) / 3.;
    AC = TAU - 1.;
    double T = Math.sqrt(TAU) - Math.sqrt(AC);
	  double t2 = T * T;
/* Computing 2nd power */
	  double r__1 = (1. - t2) / (T * 2.);
	  TAUS = r__1 * r__1;

    double TH2 = T * T;
    double TH4 = TH2 * TH2;
    HNL = 0.5 * intensity / ZGE * (1.0 - gauss);
    HL1 = 1. - TH4;
    HL2 = 2. * TH2;
    HL3 = 1. + TH4;

	  fgtab(B, S, 1);
  }

  /*
  *    CALCULATES (P(S,Z)+P(S,-Z))/2  - GAUSS STANDARD
  *    REDUCING  EXP IS COMPENSATED IN PARGLP AND Bessel
  */
  public double PSHG(double Z) {
//  COMMON /PAR/B(10),S(10),T(10)
	  StandardFunctionTexture sft = (StandardFunctionTexture) getParent();
    double PS1H = 0.0, PS2H = 0.0;
    double ARG1M = S * (1.0 - Z);
    double ARG2M = S * (1.0 + Z);
    if (ARG1M  < 80.)
      PS1H = Math.exp(-ARG1M) * Bessel(ARG2M * 0.5, 0);
    if (sft.IRED == 1)
      return PS1H * 2.0;
    if (ARG2M < 80.0)
      PS2H = Math.exp(-ARG2M) * Bessel(ARG1M * 0.5, 0);
    if (sft.IRED == 2)
      return PS2H * 2.0;
//    System.out.println("PSHG: " + PS1H + " " + PS2H + " " + Bessel(ARG1M * 0.5, 0) + " " + Bessel(ARG2M * 0.5, 0));
    return PS1H + PS2H;
  }


  /*
   *    CALCULATES (P(T,Z)+P(T,-Z))/2  - LORENTZ STANDARD
   */
  public double PSHL(double Z) {
//       COMMON /HMGL/HL1(10),HL2(10),HL3(10),HN(10)
    double PS1H = HL3 - Z;
    PS1H = HL1 / (PS1H * Math.sqrt(PS1H));
    double PS2H = HL3 + Z;
    PS2H = HL1 / (PS2H * Math.sqrt(PS2H));
    return PS1H + PS2H;
  }

/*    CALCULATES GAUSSIAN F(G) OR FS(G) (DEPENDING ON IRED) */
/*    TAKING INTO ACCOUNT ALL NA*NB EQUIVALENT PEAK POSITIONS */
/*    AND USING THE FG(S,OMEGA) - TABLE */
/*    INPUT G BY SICO, INPUT G0(JK) BY ABGSC */
/*    COS(OMEGA)=(SPUR(G0(-1)*G)-1)/2 */
/*    SPUR=2*COS(OMEGA)+1 */
/*    LINEAR FG-INTERPOLATION FOR B.GE.20 (DEGREES) */
/*    OR OMEGA. LT.180-1.5*B */
/*    FOR B.LT.20 AND OMEGA.GT.180-1.5*B */
/*    FG-INTERPOLATION IN PRCINT, USING 4 NODES */
/*    UGR -  PROTECTION AGAINST UNDERFLOW */
/*    SUM1 - ODF */
/*    SUM - ODFS */
/*    CALCULATES LORENTZIAN F(G) OR FS(G) (DEPENDING ON IRED) */
/*    TAKING INTO ACCOUNT ALL NA*NB EQUIVALENT PEAK POSITIONS */
/*    INPUT G BY SICO, INPUT G0(JK) BY ABGSC */
/*    COS(OMEGA)=(SPUR(G0(-1)*G)-1)/2 */
/*    SPUR=2*COS(OMEGA)+1 */

	public double[] ovf(int ije, int[] mgb, int ired,
	                    double ca, double sa, double cb, double sb, double cg, double sg)
	{
    // Local variables
		double can, cbn, sca, cgn, scb, san, sbn, sgn, spur, comh2;
		double[] val = new double[4], argm = new double[4];
		double arg, ugr = -170.0, spurt, omega, fug;
		double[] sum_l = new double[2];
		sum_l[0] = 0;
		sum_l[1] = 0;
		double[] sum_g = new double[2];
		sum_g[0] = 0;
		sum_g[1] = 0;

		for (int i = 0; i < ije; i++) {
		if (mgb[i] != 0) {
			san = HMSA[i];
			can = HMCA[i];
			sbn = HMSB[i];
			cbn = HMCB[i];
			sgn = HMSG[i];
			cgn = HMCG[i];
			sca = ca * can + sa * san;
			scb = cg * cgn + sg * sgn;
			spur = sca * scb * (cb * cbn + 1.0) - (sa * can - ca * san) * (sg * cgn - cg * sgn) *
						 (cb + cbn) + (sca + scb) * sb * sbn + cb * cbn;
			double spurl = spur;
			if (spurl < -1)
				spurl = -1.0;
			if (spurl > 3)
				spurl = 3.0;
			comh2 = (spurl + 1.0) * 0.25;
// Computing 2nd power
			double r__1 = TAU - comh2;
			r__1 *= r__1;
			sum_l[0] += (TAU + comh2) / r__1;
			if (ired != 0) {
// Computing 2nd power
				r__1 = TAUS + comh2;
				r__1 *= r__1;
				sum_l[1] += (TAUS - comh2) / r__1;
			}

// GAUSSIAN part
//    CALCULATES GAUSSIAN F(G) OR FS(G) (DEPENDING ON IRED)
//    TAKING INTO ACCOUNT ALL NA*NB EQUIVALENT PEAK POSITIONS
//    AND USING THE FG(S,OMEGA) - TABLE
//    INPUT G BY SICO, INPUT G0(JK) BY ABGSC
//    COS(OMEGA)=(SPUR(G0(-1)*G)-1)/2
//    SPUR=2*COS(OMEGA)+1
//    LINEAR FG-INTERPOLATION FOR B.GE.20 (DEGREES)
//    OR OMEGA. LT.180-1.5*B
//    FOR B.LT.20 AND OMEGA.GT.180-1.5*B
//    FG-INTERPOLATION IN PRCINT, USING 4 NODES
//    UGR -  PROTECTION AGAINST UNDERFLOW
//    SUM1 - ODF
//    SUM - ODFS

			int iom, iome1, iome2, iom1, iom2, iom3, iom4;
			if (spur <= -1) {
				spur = -1;
				spurt = -1;
				arg = -2 * S;
				if (arg > ugr)
					sum_g[0] += Math.exp(arg);
				if (ired != 0)
					sum_g[1] += fgm[180];
			} else {
				if (spur - 3. >= 0.) {
					spur = 3.;
					spurt = (spur - 1.) * .5;
					arg = S * (spurt - 1.);
					if (arg > ugr)
						sum_g[0] += Math.exp(arg);
					if (ired != 0)
						sum_g[1] += fgm[0];
				} else {
					spurt = (spur - 1.) * .5;
					arg = S * (spurt - 1.);
					if (arg > ugr)
						sum_g[0] += Math.exp(arg);
					if (ired != 0) {
						omega = Constants.PITODEG * Math.acos(spurt);

//     WORKS ALSO FOR B - DUMMY (8888)

						if (B - 20. <= 0.) {
							if (180. - omega - B * 1.5 >= 0.) {
								iome1 = (int) omega;
								iome2 = iome1 + 1;
								fug = fgm[iome1] + (fgm[iome2] - fgm[iome1])
										* (omega - iome1);
							} else {
								if (omega - 179.5 <= 0.) {

									if (omega - 179. >= 0.) {
										argm[0] = 179.;
										argm[1] = 180.;
										val[0] = fgm[179];
										val[1] = fgm[180];
										argm[2] = 178.;
										argm[3] = 177.;
										val[2] = fgm[178];
										val[3] = fgm[177];
									} else {
										iom = (int) omega;
										iom1 = iom;
										iom2 = iom1 + 1;
										iom3 = iom2 + 1;
										iom4 = iom - 1;
										if (omega - iom - .5 >= 0.f) {
											argm[0] = iom1;
											argm[1] = iom;
											argm[2] = iom2;
											argm[3] = iom4;
											val[0] = fgm[iom2];
											val[1] = fgm[iom1];
											val[2] = fgm[iom3];
											val[3] = fgm[iom];
										} else {
											argm[0] = iom;
											argm[1] = iom1;
											argm[2] = iom4;
											argm[3] = iom2;
											val[0] = fgm[iom1];
											val[1] = fgm[iom2];
											val[2] = fgm[iom];
											val[3] = fgm[iom3];
										}
									}

								} else {
									argm[0] = 180.f;
									argm[1] = 179.f;
									val[0] = fgm[180];
									val[1] = fgm[179];
									argm[2] = 178.f;
									argm[3] = 177.f;
									val[2] = fgm[178];
									val[3] = fgm[177];
								}

								fug = prcint(omega, argm, val);

							}
						} else {

							iome1 = (int) omega;
							iome2 = iome1 + 1;
							fug = fgm[iome1] + (fgm[iome2] - fgm[iome1])
									* (omega - iome1);

						}
						sum_g[1] += fug;

					}
				}
			}
// end of GAUSSIAN part
		}
		}
		if (sum_g[0] <= 1.08e-35)
			sum_g[0] = 0.0;

		for (int i = 0; i < 2; i++)
			sum_l[i] = sum_l[i] * (1.0 - gauss) + sum_g[i] * gauss;
		return sum_l;
	} // ovf

	double[] fgm = new double[183];

	private void fgtab(double b, double s, int ib) {
		double db;
		double bn, sn, schr, omega, delta;
		int iom, ipunkt;

//    COMPUTES THE FG(S,OMEGA)-TABLE
//    FOR OMEGA = 1,2,...,180 DEGREES
//    FGM(182)=S, FGM(183)=B

		double sa = 0, ba = 0;
		delta = .01;
		bn = b;
		sn = s;
		fgm[181] = s;
		fgm[182] = b;
		if (ib <= 0) {
			db = Math.abs(sa - sn);
			if (db <= delta) {
				sa = s;
				ba = b;
				return;
			}
			if (s - Constants.LN2_2 <= 0.0) {
				ipunkt = 100;
				schr = .01;
				for (iom = 0; iom < 181; iom++) {
					omega = iom;
					fgm[iom] = fg(s, omega, ipunkt, schr);
				}
				ba = b;
				sa = s;
				return;
			}
		}
		db = Math.abs(ba - bn);
		if (db <= delta) {
			ba = b;
			sa = s;
			return;
		}
		if (b < 25.0) {
			ipunkt = 400;
			schr = .0025;
			for (iom = 0; iom < 181; iom++) {
				omega = iom;
				fgm[iom] = fg(s, omega, ipunkt, schr);
			}
			ba = b;
			sa = s;
			return;
		}
		if (b < 60.0) {
			ipunkt = 200;
			schr = .005;
		} else {
			ipunkt = 400;
			schr = .0025;
		}
		for (iom = 0; iom < 181; iom++) {
			omega = iom;
			fgm[iom] = fg(s, omega, ipunkt, schr);
		}
		ba = b;
		sa = s;
		return;

	} // fgtab

/*    *--------------------------------* */

	public static double fg(double s, double omega, int ipunkt, double schr) {
		double[] am = new double[401];
		double b0, t2, tg;
		double arg, amp, com, com2, rin1, som2, fgpi, tber;
		double com2h, rin2c, grenz;

//    CALCULATES THE GAUSSIAN GHOST FUNCTION
//    USING IPUNKT NODES FOR THE INTEGRATION

		grenz = 1e-10;
		b0 = Bessel(s, 0);
		amp = 1.f / (b0 - Bessel(s, 1));
		fgpi = b0 * .5 * amp;
		if (omega <= 0.f)
			return fgpi - s;
		if (omega - 180.0 == 0.0)
			return fgpi;
		tg = 0.0;
		com = Math.cos(omega * Constants.DEGTOPI);
// Computing 2nd power
		com2 = com * com;
		som2 = 1.0 - com2;
		com2h = (com + 1) / 2;
		boolean repeat = true;
		for (int it = 0; it <= ipunkt; it++) {
// Computing 2nd power
			t2 = 1.0 - it * schr;
			t2 *= t2;
			arg = s * Math.sqrt(com2 + t2 * som2);
			am[it] = Math.exp(-s + arg) * Bessel(arg, 0);
			if (am[it] / am[0] - grenz <= 0.0) {
				tg = Math.sqrt(t2);
				repeat = false;
				break;
			}
		}
		if (repeat) {
			tber = (1.0 - tg) * schr;
			for (int it = 0; it <= ipunkt; it++) {
// Computing 2nd power
				t2 = 1.0 - it * tber;
				t2 *= t2;
				arg = s * Math.sqrt(com2 + t2 * som2);
				am[it] = Math.exp(arg - s) * Bessel(arg, 0);
			}
		}
		rin1 = simp(am, tg, ipunkt);
		if (omega - 90.0 == 0.0)
			return fgpi - amp * s * com2h * rin1;
		tg = 0.0;
		repeat = false;
		for (int it = 0; it <= ipunkt; it++) {
// Computing 2nd power
			t2 = 1.0 - it * schr;
			t2 *= t2;
			arg = s * Math.sqrt(com2 + t2 * som2);
			am[it] = Math.exp(-s + arg) * Bessel(arg, 1) / arg;
			if (am[it] / am[0] - grenz <= 0.0) {
				tg = Math.sqrt(t2);
				repeat = true;
				break;
			}
		}
		if (repeat) {
			tber = (1.0 - tg) * schr;
			for (int it = 0; it <= ipunkt; it++) {
// Computing 2nd power
				t2 = 1.0 - it * tber;
				t2 *= t2;
				arg = s * Math.sqrt(com2 + t2 * som2);
				am[it] = Math.exp(-s + arg) * Bessel(arg, 1) / arg;
			}
		}
		rin2c = com * simp(am, tg, ipunkt) * s;
		return fgpi - amp * s * com2h * (rin1 - rin2c);
} // fg



	public static double simp(double[] am, double tg, int ipunkt) {
//    SIMPSON INTEGRATION PROCEDURE
		double s = am[0] - am[ipunkt];
		for (int i = 0; i < ipunkt / 2; i++)
			s += am[i * 2] * 4.0 + am[2 * i  + 1] * 2.0;
		return s * (1.0 - tg) / (ipunkt * 3);
	}


	public static final double prcint(double x, double[] arg, double[] val) {
	  // interpolation procedure using 3 or 4 interpolation nodes
		// depending wether the absolute error is < 0.05 or not

		for (int j = 1; j < 4; j++) {
			int iend = j - 1;
			for (int i = 0; i < j; i++)
				val[j] = (val[i] * (x - arg[j]) - val[j] * (x - arg[i])) / (arg[i] - arg[j]);
			if (j > 1 && (Math.abs(val[j]-val[iend]) - 0.05) <= 0.0)
				return val[j];
		}
		return val[3];
	}

	static final
  double[] Abe = {0.926522283E-27, 0.665920310E-25, 0.187543306E-21, 0.678818504E-19,
                  0.293709299E-16, 0.936223818E-14, 0.240378975E-11, 0.470922041E-9,
                  0.678173901E-7, 0.678167711E-5, 0.434027830E-3, 0.156249998E-1,
                  0.25, 1.0},
  Bbe = {0.228201527E-7, 0.460958396E-7, 0.542737131E-7, 0.204865735E-6,
         0.911964648E-6, 0.448357120E-5, 0.292186032E-4, 0.280504770E-3,
         0.498677852E-2, 0.398942281},
  Cbe = {0.312719049E-28, 0.378500541E-26, 0.744839607E-23, 0.314048171E-20,
         0.146127307E-17, 0.520686381E-15, 0.150206750E-12, 0.336384070E-10,
         0.565142090E-8, 0.678168183E-6, 0.542534739E-4, 0.260416666E-2,
         0.625E-1, 0.5},
  Dbe = {-0.250141085E-7, -0.513119520E-7, -0.637466188E-7, -0.244333379E-6,
         -0.111375439E-5, -0.576272322E-5, -0.409063053E-4, -0.467509130E-3,
         -0.149603355E-1, +0.398942281};

  public static double Bessel(double XX, int IND) {
    double X, BES;
    if (IND != 0) {
      X = XX;
      if (Math.abs(X) < 10.) { // 10,30,30
        double FI2 = Cbe[0]; // 10
        double V = X * X;
        for (int L = 1; L < 14; L++)
          FI2 = FI2 * V + Cbe[L];
        BES = FI2 * X * Math.exp(-X);
      } else { // 30
        double FI3 = Dbe[0];
        X = Math.abs(X);
        double Y = 10. / X;
        for (int L = 1; L < 10; L++)
          FI3 = FI3 * Y + Dbe[L];
        BES = FI3 / Math.sqrt(X);
        if (XX < 0.)
          BES = -BES;
      }
    } else {
      X = Math.abs(XX);
      if (Math.abs(X) < 10.) { // 60,80,80
        double FI = Abe[0]; // 60
        double U = X * X;
        for (int K = 1; K < 14; K++)
          FI = FI * U + Abe[K];
        BES = FI * Math.exp(-X);
      } else { // 80
        double FI1 = Bbe[0];
        double Y = 10. / X;
        for (int K = 1; K < 10; K++)
          FI1 = FI1 * Y + Bbe[K];
        BES = FI1 / Math.sqrt(X);
      }
    }
    return BES;
  }

}
