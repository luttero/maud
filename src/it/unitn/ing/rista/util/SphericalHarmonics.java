/*
 * @(#)SphericalHarmonics.java 14/10/1998 Pergine
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

package it.unitn.ing.rista.util;

import java.lang.*;

/**
 *  The SphericalHarmonics is a class providing static methods for
 *  spherical harmonics computation. Angle in radiants.
 *
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SphericalHarmonics {

  static int actualMaxIndex = -1;
  static double xcoeff[][][] = null;

  static int actualAlegrendeMaxIndex = -1;
  static double alegrende[][] = null;

  static boolean initialized = false;

  public static int maxExpansion = 23;

  static int NOb[] = {1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0};
  static int NTb[] = {1, 0, 0, 1, 1, 0};
  static int NO[] = null;
  static int NT[] = null;

  public static double[][][] deltaV = null;

  static double A_l_mue_ms_22[][] = {
    {0.0, 0.0, 0.0, 0.0, 0.0, 0.0}, // l = n for zero coeff
    {1.0, 0.0, 0.0, 0.0, 0.0, 0.0}, // 1     T, l = 3
    {0.7637626158, 0.6454972244, 0.0, 0.0, 0.0, 0.0}, // 2     O, l = 4
    {-0.3535533906, 0.9354143467, 0.0, 0.0, 0.0, 0.0}, // 3     O, l = 6
    {-0.8291561976, 0.5590169944, 0.0, 0.0, 0.0, 0.0}, // 4     T
    {0.7359800722, 0.6770032004, 0.0, 0.0, 0.0, 0.0}, // 5     T, l = 7
    {0.7180703308, 0.3818813079, 0.5818433352, 0.0, 0.0, 0.0}, // 6     O, l = 8
    {0.0, -0.8416254115, 0.5400617249, 0.0, 0.0, 0.0}, // 7     O, l = 9
    {-0.4330127019, 0.9013878189, 0.0, 0.0, 0.0, 0.0}, // 8     T
    {-0.4114253679, 0.58630197, 0.697838926, 0.0, 0.0, 0.0}, // 9     O, l = 10
    {-0.8020156898, -0.1572882174, 0.5762215286, 0.0, 0.0, 0.0}, // 10    T
    {0.6653633093, 0.4592793268, 0.5885186205, 0.0, 0.0, 0.0}, // 11    T, l = 11
    {0.6552821454, 0.4832927031, 0.05809147013, 0.5776321097, 0.0, 0.0}, // 12    O, l = 12
    {0.2330863966, -0.4213798472, 0.8764187105, 0.0, 0.0, 0.0}, // 13    O
    {0.2104063529, -0.8267972847, 0.5216660011, 0.0, 0.0, 0.0}, // 14    T
    {0.0, -0.786441087, -0.2282177323, 0.5739573881, 0.0, 0.0}, // 15    O, l = 13
    {-0.4973890161, 0.4934466368, 0.7135226579, 0.0, 0.0, 0.0}, // 16    T
    {-0.440096462, 0.4576818286, 0.4911323014, 0.5963484807, 0.0, 0.0}, // 17    O, l = 14
    {-0.7775432899, -0.2485308394, -0.02017178826, 0.5772797876, 0.0, 0.0}, // 18    T
    {0.0, 0.2997394702, -0.8100925873, 0.5038911093, 0.0, 0.0}, // 19    O, l = 15
    {0.5622940367, 0.5830498339, 0.09698273677, 0.5783361101, 0.0, 0.0}, // 20    T
    {0.2949287513, -0.4266445212, 0.8549803999, 0.0, 0.0, 0.0}, // 21    T
    {0.6108821878, 0.5294942682, 0.114282296, 0.006682146942, 0.5773678883, 0.0}, // 22    O, l = 16
    {0.3017891585, -0.4489648246, 0.4245203783, 0.7260415542, 0.0, 0.0}, // 23    O
    {0.2780489128, -0.7199943938, -0.2803806007, 0.570686949, 0.0, 0.0}, // 24    T
    {0.0, -0.7367536831, -0.3503160775, -0.03827327723, 0.577068291, 0.0}, // 25    O, l = 17
    {-0.5245440729, 0.3236299246, 0.5042947065, 0.6048173579, 0.0, 0.0}, // 26    T
    {-0.4239772421, 0.3016245048, 0.6132361012, 0.131763019, 0.5795221715, 0.0}, // 27    O, l = 18
    {-0.173001669, 0.2837089301, -0.4385676109, 0.835007854, 0.0, 0.0}, // 28    O
    {-0.7531096711, -0.3116800558, -0.04846181296, -0.00213815328, 0.5773458643, 0.0}, // 29    T
    {-0.09533756328, 0.3505528222, -0.7944658769, 0.4866697426, 0.0, 0.0}, // 30    T
    {0.0, 0.3890593624, -0.651020833, -0.3222439229, 0.5665364434, 0.0}, // 31    O, l = 19
    {0.4848328387, 0.6308941469, 0.182438847, 0.01417303353, 0.5774295507, 0.0}, // 32    T
    {0.3843556601, -0.4182462578, 0.3677142755, 0.7362927448, 0.0, 0.0}, // 33    T
    {0.578204599, 0.5521518409, 0.1646198733, 0.01931412426, 0.0006658063932, 0.5773513704}, // 34    O, l = 20
    {0.3412879669, -0.4439643655, 0.2283289352, 0.5076818154, 0.6136311902, 0.0}, // 35    O
    {0.319023703, -0.6254392987, -0.4137675837, -0.05754265571, 0.5766582957, 0.0}, // 36    T
    {0.0, -0.6860146261, -0.4343210709, -0.08611669755, -0.005015236341, 0.5773282446}, // 37    O, l = 21
    {0.0, -0.1494468878, 0.3852817544, -0.7799890157, 0.4699369456, 0.0}, // 38    O
    {-0.4913403909, 0.1282812803, 0.6145306939, 0.163329119, 0.581209221, 0.0}, // 39    T
    {-0.2218186573, 0.283201793, -0.4532337961, 0.8155809915, 0.0, 0.0}, // 40    T
    {-0.4038067747, 0.1815735091, 0.6431379871, 0.23711025, 0.02329711306, 0.5775644145}, // 41    O, l = 22
    {-0.2411390166, 0.3588175931, -0.3968432597, 0.317782288, 0.7447360299, 0.0}, // 42    O
    {-0.7306749131, -0.3554341334, -0.07996224606, -0.007330127079, -0.0002028668964, 0.5773499939}, // 43    T
    {-0.1450125713, 0.437222134, -0.5870284274, -0.3574894532, 0.5616111431, 0.0} /*, up to l = 22 for now
      {,,,,,},
      {,,,,,}*/
  };

  static int row_T[][] = {
    {0, 0, 0, 0, 0, 0, 0, 0}, // l = 0
    {0, 0, 0, 0, 0, 0, 0, 0}, // l = 1
    {0, 0, 0, 0, 0, 0, 0, 0}, // l = 2
    {1, 0, 0, 0, 0, 0, 0, 0}, // l = 3
    {2, 0, 0, 0, 0, 0, 0, 0}, // l = 4
    {0, 0, 0, 0, 0, 0, 0, 0}, // l = 5
    {3, 4, 0, 0, 0, 0, 0, 0}, // l = 6
    {5, 0, 0, 0, 0, 0, 0, 0}, // l = 7
    {6, 0, 0, 0, 0, 0, 0, 0}, // l = 8
    {7, 8, 0, 0, 0, 0, 0, 0}, // l = 9
    {9, 10, 0, 0, 0, 0, 0, 0}, // l = 10
    {11, 0, 0, 0, 0, 0, 0, 0}, // l = 11
    {12, 13, 14, 0, 0, 0, 0, 0}, // l = 12
    {15, 16, 0, 0, 0, 0, 0, 0}, // l = 13
    {17, 18, 0, 0, 0, 0, 0, 0}, // l = 14
    {19, 20, 21, 0, 0, 0, 0, 0}, // l = 15
    {22, 23, 24, 0, 0, 0, 0, 0}, // l = 16
    {25, 26, 0, 0, 0, 0, 0, 0}, // l = 17
    {27, 28, 29, 30, 0, 0, 0, 0}, // l = 18
    {31, 32, 33, 0, 0, 0, 0, 0}, // l = 19
    {34, 35, 36, 0, 0, 0, 0, 0}, // l = 20
    {37, 38, 39, 40, 0, 0, 0, 0}, // l = 21
    {41, 42, 43, 0, 0, 0, 0, 0}                   // l = 22
  };

  static int row_O[][] = {
    {0, 0, 0, 0}, // l = 0
    {0, 0, 0, 0}, // l = 1
    {0, 0, 0, 0}, // l = 2
    {0, 0, 0, 0}, // l = 3
    {2, 0, 0, 0}, // l = 4
    {0, 0, 0, 0}, // l = 5
    {3, 0, 0, 0}, // l = 6
    {0, 0, 0, 0}, // l = 7
    {6, 0, 0, 0}, // l = 8
    {7, 0, 0, 0}, // l = 9
    {9, 0, 0, 0}, // l = 10
    {0, 0, 0, 0}, // l = 11
    {12, 13, 0, 0}, // l = 12
    {15, 0, 0, 0}, // l = 13
    {17, 0, 0, 0}, // l = 14
    {19, 0, 0, 0}, // l = 15
    {22, 23, 0, 0}, // l = 16
    {25, 0, 0, 0}, // l = 17
    {27, 28, 0, 0}, // l = 18
    {31, 0, 0, 0}, // l = 19
    {34, 35, 0, 0}, // l = 20
    {37, 38, 0, 0}, // l = 21
    {41, 42, 0, 0}                   // l = 22
  };

  protected SphericalHarmonics() {
  }

  public static void initialize() {
    if (!initialized) {
      NT = new int[maxExpansion];
      NO = new int[maxExpansion];
      for (int i = 0; i < 6; i++)
        NT[i] = NTb[i];
      for (int i = 0; i < 12; i++)
        NO[i] = NOb[i];
      for (int i = 6; i < maxExpansion; i++)
        NT[i] = NT[i - 6] + i / 6;
      for (int i = 12; i < maxExpansion; i++)
        NO[i] = NO[i - 12] + i / 12;

      MoreMath.initFactorial(maxExpansion * 2 + 1);
      checkAlegrendeCoefficients(maxExpansion - 1);
      checkCoefficients(maxExpansion - 1);

      deltaV = new double[maxExpansion][maxExpansion][maxExpansion];
      for (int l = 0; l < maxExpansion; l++)
        for (int m = 0; m <= l; m++)
          for (int n = 0; n <= l; n++)
            deltaV[l][m][n] = delta(l, m, n);
      initialized = true;
    }
  }

  public static final double getCosineTerm(int l, int m, double beta, double phi) {
    return Math.cos(m * beta) * getNormalizedLegrende(l, m, phi);
  }

  public static final double getSineTerm(int l, int m, double beta, double phi) {
    return Math.sin(m * beta) * getNormalizedLegrende(l, m, phi);
  }

  public static final double getNormalizedLegrende(int l, int m, double phi) {
//	  return Alfpack.legrende(l, -m, phi);
    double cosphi = Math.cos(phi);
    if (1.0 - Math.abs(cosphi) > 1.0E-9) {
      double aleg = Alegrende(l, m);
      double sleg = Slegrende(l, m, cosphi);
      aleg = aleg * sleg / Math.pow(1.0 - cosphi * cosphi, 0.5 * m);
//			System.out.println(Integer.toXRDcatString(l) + " " + Integer.toXRDcatString(m) + " " + Double.toXRDcatString(aleg) + " " + Double.toXRDcatString(Alfpack.legrende(l, -m, phi)));
      return aleg;
    } else if (m == 0)
      return Math.sqrt((l + 0.5) / Constants.PI2);
    else
      return 0.0;
  }

  public static final double Alegrende(int l, int m) {
//		return AlegrendeCoeff(l, m);
    return alegrende[m][l / 2];
  }

  public static final double AlegrendeCoeff(int l, int m) {

    double alegr = (l + 0.5) * MoreMath.factorial(l + m, l - m) / Constants.PI2;
    alegr = Math.sqrt(alegr);
    alegr /= (MoreMath.pow(2, l) * MoreMath.factorial(l));

    if (MoreMath.odd(m))
      return -alegr;
    else
      return alegr;
  }

  public static final double Slegrende(int l, int m, double cosphi) {

//		checkCoefficients(l);
    double slegr = 0.0;
    int lm = l - m;
    int l2 = l / 2 - 1;
    double x2 = cosphi * cosphi - 1.0;
    int indexMax = lm / 2 + 1;
    for (int i = 0; i < indexMax; i++) {
      slegr += xcoeff[l2][lm][i] * MoreMath.pow(cosphi, lm - 2 * i) *
              MoreMath.pow(x2, m + i);
    }
    return slegr;
  }

  public static final void checkCoefficients(int l) {

    if (actualMaxIndex >= l)
      return;

    actualMaxIndex = l;
    int secondIndex = l / 2 + 1;

    double[][] kmi = new double[l + 1][secondIndex];
    kmi[0][0] = 1.0;
    kmi[0][1] = 0.0;
    for (int m = 1; m <= l; m++) {
      int maxi = m / 2 + 1;
      kmi[m][0] = kmi[m - 1][0] * 2;
      for (int i = 1; i < maxi; i++)
        kmi[m][i] = kmi[m - 1][i - 1] * (m - 2 * i + 1) + kmi[m - 1][i] * 2;
      if (m < l)
        kmi[m][maxi] = 0.0;
    }

//		System.out.println(l);
    xcoeff = new double[secondIndex - 1][l + 1][secondIndex];

    for (int lp = 2; lp <= l; lp += 2) {
      int l2 = lp / 2 - 1;
      for (int m = 0; m <= lp; m++) {
        int jmax = m / 2 + 1;
        for (int i = 0; i < jmax; i++) {
          xcoeff[l2][m][i] = kmi[m][i] * MoreMath.factorial(lp, lp - m + i);
//					System.out.println(Integer.toXRDcatString(lp) + " " + Integer.toXRDcatString(m) + " " + Integer.toXRDcatString(i)
//							 + " " + Long.toXRDcatString(xcoeff[l2][m][i]));
        }
      }
    }
  }

  public static final void checkAlegrendeCoefficients(int l) {
    if (actualAlegrendeMaxIndex >= l)
      return;

    actualAlegrendeMaxIndex = l;
    int secondIndex = l / 2 + 1;
    alegrende = new double[l + 1][secondIndex];

    for (int i = 0; i <= l / 2; i++)
      for (int m = 0; m <= i * 2; m++)
        alegrende[m][i] = AlegrendeCoeff(i * 2, m);
  }

  public static final int getN(int LGnumber, int l) {
    initialize();
//		checkAlegrendeCoefficients(l);
//		checkCoefficients(l);
    switch (LGnumber) {
      case 0: // C1
        return 2 * l + 1;
      case 1: // C2
        return l + 1;
      case 2: // D2
        if (MoreMath.odd(l))
          return l / 2;
        else
          return l / 2 + 1;
      case 5: // C3
        return (l / 3) * 2 + 1;
      case 6: // D3
        if (MoreMath.odd(l))
          return l / 3;
        else
          return l / 3 + 1;
      case 3: // C4
        return (l / 4) * 2 + 1;
      case 4: // D4
        if (MoreMath.odd(l))
          return l / 4;
        else
          return l / 4 + 1;
      case 7: // C6
        return (l / 6) * 2 + 1;
      case 8: // D6
        if (MoreMath.odd(l))
          return l / 6;
        else
          return l / 6 + 1;
      case 9: // T
        return NT[l];
      case 10: // O
        return NO[l];
      case 11:
        return 1;
      default:
        {
        }
    }
    return 0;
  }

  public static final double getSphericalHarmonic(int LGnumber, int l, int mu,
                                                  double beta, double phi) {
    // Angles must be in radiants

    double Almun = 0.0;

    initialize();

    double Ylmu = 0.0;
    for (int m = -l; m <= l; m++) {
      Almun = getAlmum(LGnumber, l, mu, m);
      if (Almun != 0.0)
        Ylmu += Almun * getTesseralFunction(l, m, beta, phi);
    }
    return Ylmu;
  }

  public static final double getDSphericalHarmonic(int LGnumberC, int LGnumberS,
                                                   int l, int mu, int vu,
                                                   double alpha, double beta, double gamma) {
    // Angles must be in radiants

    double AlmunS = 0.0;
    double AlmunC = 0.0;

    initialize();

    double Dlmu = 0.0;
    double tmplmu;

    for (int n = -l; n <= l; n++) {
      AlmunS = getAlmum(LGnumberS, l, vu, n);
      if (AlmunS != 0.0) {
        tmplmu = 0.0;
        for (int m = -l; m <= l; m++) {
          AlmunC = getAlmum(LGnumberC, l, mu, m);
          if (AlmunC != 0.0)
            tmplmu += AlmunC * getDTesseralFunction(l, m, n, alpha, beta, gamma);
        }
        Dlmu += tmplmu * AlmunS;
      }
    }
    return Dlmu;
  }

  /**
   * Implements the rules for Ylm computation. Siegfried book, page 48, equation (11.15).
   **/

  public static final double getTesseralFunction(int l, int m, double phi, double theta) {
    if (m == 0)
      return getNormalizedLegrende(l, m, theta);
    if (m < 0)
      return Constants.sqrt2 * getSineTerm(l, -m, phi, theta);
    else
      return Constants.sqrt2 * getCosineTerm(l, m, phi, theta);
  }

  /**
   * Implements the rules for Dlmn computation. Siegfried book, page 107, equation (18.4).
   **/

  public static final double getDTesseralFunction(int l, int m, int n,
                                                  double alpha, double beta, double gamma) {

    double i1 = (MoreMath.odd(m + n)) ? -1.0 : 1.0;
    double i2 = (MoreMath.odd(l)) ? -1.0 : 1.0;
    int mabs = m > 0 ? m : -m ;
	  int nabs = n > 0 ? n : -n ;

    double arg1 = mabs * alpha + nabs * gamma;
    double arg2 = mabs * alpha - nabs * gamma;
    double[] cx = getC(m, n, arg1, arg2);
    if (cx[1] != 0.0) {
      double[] df = getTwoDFunction(l, mabs, nabs, beta);
      return i1 * cx[0] * df[0] + i2 * cx[1] * df[1];
    }
    return i1 * cx[0] * getDFunction(l, mabs, nabs, beta);
  }

  public static final double[] getC(int m, int n, double arg1, double arg2) {
    double[] cx = new double[2];
    if (m > 0) {
      if (n > 0) {
        cx[0] = Math.cos(arg1);
        cx[1] = Math.cos(arg2);
      } else if (n < 0) {
        cx[0] = -Math.sin(arg1);
        cx[1] = Math.sin(arg2);
      } else {
        cx[0] = Constants.sqrt2 * Math.cos(arg1);
        cx[1] = 0.0;
      }
    } else if (m < 0) {
      if (n > 0) {
        cx[0] = Math.sin(arg1);
        cx[1] = Math.sin(arg2);
      } else if (n < 0) {
        cx[0] = Math.cos(arg1);
        cx[1] = -Math.cos(arg2);
      } else {
        cx[0] = Constants.sqrt2 * Math.sin(arg1);
        cx[1] = 0.0;
      }
    } else {
      if (n > 0) {
        cx[0] = Constants.sqrt2 * Math.cos(arg1);
        cx[1] = 0.0;
      } else if (n < 0) {
        cx[0] = -Constants.sqrt2 * Math.sin(arg1);
        cx[1] = 0.0;
      } else {
        cx[0] = Math.cos(arg1);
        cx[1] = 0.0;
      }
    }
    return cx;
  }

  /**
   * Implements the rules for dlmn computation. Siegfried book, page 48, equation (11.19).
   **/

  public static final double getDFunction(int l, int m, int n, double beta) {
    double arg = Constants.PI * (m - n) / 2.0;
	  double df = deltaV[l][0][m] * deltaV[l][0][n] * Math.cos(arg);
    for (int i = 1; i <= l; i++)
      df += 2.0 * deltaV[l][i][m] * deltaV[l][i][n] * Math.cos(beta * i - arg);
    return df;
  }

  public static final double[] getTwoDFunction(int l, int m, int n, double beta) {
    double[] df = new double[2];
    double dftmp;
    double dbeta = Constants.PI - beta;

	  double arg = Constants.PI_2 * (m - n);
	  dftmp = deltaV[l][0][m] * deltaV[l][0][n];
	  df[0] = dftmp * Math.cos(arg);
	  df[1] = df[0];
    for (int i = 1; i <= l; i++) {
      dftmp = 2.0 * deltaV[l][i][m] * deltaV[l][i][n];
      df[0] += dftmp * Math.cos(beta * i - arg);
      df[1] += dftmp * Math.cos(dbeta * i - arg);
    }
    return df;
  }

  /**
   * Implements the rules for delta function computation. Siegfried book, page 49, equation (11.20).
   **/

  public static final double delta(int l, int m, int n) {
    double delta = 0.0;

    double i3, i1 = 1.0;
    if (MoreMath.odd(m + n))
      i1 = -1.0;
    double i2 = 1.0 / MoreMath.pow(2, l);

    double arg = Math.sqrt(MoreMath.factorial(l + m) * MoreMath.factorial(l - m) /
            (MoreMath.factorial(l + n) * MoreMath.factorial(l - n)));
    for (int i = 0; i <= l; i++) {
      i3 = 1.0;
      if (MoreMath.odd(i))
        i3 = -1.0;
      delta += i3 * MoreMath.binomial(l + n, i) * MoreMath.binomial(l - n, i + m - n);
    }
    return delta * arg * i1 * i2;
  }

  /**
   * Implements the rules for Almum computation. Siegfried book, page 52, equation (11.35).
   **/

  public static final double getAlmum(int LGnumber, int l, int mu, int m) {

    int m1;

    if (l == 0 && m == 0 && mu == 1) {
      return 1.0;
    }

    int mp = 0;

    switch (LGnumber) {
      case 0: // C1
        mp = getMpForC(1, mu);
        break;
      case 1: // C2
        mp = getMpForC(2, mu);
        break;
      case 5: // C3
        mp = getMpForC(3, mu);
        break;
      case 3: // C4
        mp = getMpForC(4, mu);
        break;
      case 7: // C6
        mp = getMpForC(6, mu);
        break;
      case 2: // D2
        mp = getMpForD(l, 2, mu);
        break;
      case 6: // D3
        mp = getMpForD3(l, mu);
        break;
      case 4: // D4
        mp = getMpForD(l, 4, mu);
        break;
      case 8: // D6
        mp = getMpForD(l, 6, mu);
        break;
      case 9: // T
        m1 = (Math.abs(m) - 2) / 4;
        if (m1 * 4 + 2 != Math.abs(m))
          return 0.0;
        if ((m < 0 && MoreMath.odd(l)) || m >= 0)
          return A_l_mue_ms_22[row_T[l][mu - 1]][m1];
        else
          return 0.0;
      case 10: // O
        m1 = Math.abs(m) / 4;
        if (m1 * 4 != Math.abs(m))
          return 0.0;
        if ((m < 0 && MoreMath.odd(l)) || m >= 0)
          return A_l_mue_ms_22[row_O[l][mu - 1]][m1];
        else
          return 0.0;
      case 11:
        if (mu == 1 && m == 0)
          return 1.0;
        else
          return 0.0;
      default:
        {
        }
    }

    if (mp == m)
      return 1.0;
    else
      return 0.0;
  }

  /**
   * Implements the rules for Almum computation. Siegfried book, page 52, equation (11.35).
   **/

  public static final int getMpForC(int n, int mu) {
    int m = (mu / 2) * n;
    if (MoreMath.odd(mu))
      return m;
    else
      return -m;
  }

  /**
   * Implements the rules for Almum computation. Siegfried book, page 52, equation (11.35).
   * Actually l should be always even.
   **/

  public static final int getMpForD(int l, int n, int mu) {
    int k = 1;
    if (MoreMath.odd(l))
      k = -k;
    return k * (mu - (1 + k) / 2) * n;
  }

  /**
   * Implements the rules for Almum computation. Siegfried book, page 52, equation (11.35).
   * Actually l should be always even. There is an error for D3 reported by Siegfried
   * hand writing on the paper of Textures and Microstructures Vol 8-9, pp 115-129, 1988 in Rudy's
   * office. D3 groups have cosine and sine alternatively (see also Popa, 92).
   **/

  public static final int getMpForD3(int l, int mu) {
    int k = 1;
    if (MoreMath.odd(l + mu + 1))
      k = -k;
    int k1 = 1;
    if (MoreMath.odd(l))
      k1 = -k1;
    return k * (mu - (1 + k1) / 2) * 3;
  }

  // not used until now

/*	public double[] getCubicSymmetrysedCoeff(int l, int n) {
		initialize();
		int basel = A_l[l - 1];
		double[] result = new double[l/4 + 1];
		for (int i = 0; i < (l/4 + 1); i++) {
			result[i] = A_l_mue_ms22[basel + n][i];
		}
		return result;
	}*/
}
