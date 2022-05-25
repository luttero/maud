/*
 * @(#)StrainSphericalHarmonics.java 12/07/2001 Mesiano
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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
import java.util.*;

import it.unitn.ing.rista.diffr.rsa.*;
import it.unitn.ing.rista.diffr.*;

/**
 *  The StrainSphericalHarmonics is a class providing static methods for
 *  spherical harmonics computation in the case of WSODF. Angle in radiants.
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:10 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class StrainSphericalHarmonics extends SphericalHarmonics {
  public static int numberStrainParameters = 6;

  public static final int getN(int LGnumber, int l, int strainIndex) {
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
        return ((int) (l / 3)) * 2 + 1;
      case 6: // D3
        if (MoreMath.odd(l))
          return l / 3;
        else
          return l / 3 + 1;
      case 3: // C4
        return ((int) (l / 4)) * 2 + 1;
      case 4: // D4
        if (MoreMath.odd(l))
          return l / 4;
        else
          return l / 4 + 1;
      case 7: // C6
        return ((int) (l / 6)) * 2 + 1;
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

  public static final double[][] getParameterMatrix(Vector[] coeffVect) {
    int maxInt = 0;
    int tensorLength = coeffVect.length;
    for (int i = 0; i < tensorLength; i++) {
      if (coeffVect[i].size() > maxInt)
        maxInt = coeffVect[i].size();
    }
    double[][] parMatrix = new double[tensorLength][maxInt];
    for (int i = 0; i < tensorLength; i++) {
      for (int j = 0; j < coeffVect[i].size(); j++) {
        parMatrix[i][j] = ((Parameter) coeffVect[i].elementAt(j)).getValueD();
      }
    }


    return parMatrix;
  }

  public static final double getLegrende() {
    
    return 0.0;
  }

  public static final double[] getStrainSphericalHarmonic(int LGnumber, int l, int mu,
                                                          double beta, double phi) {
    // Angles must be in radiants

    double Almun = 0.0;

    initialize();

    double[] Ylmnu = new double[numberStrainParameters];
    for (int i = 0; i < numberStrainParameters; i++) {
      Ylmnu[i] = 0.0;
      for (int m = -l; m <= l; m++) {
        Almun = getAlmum(LGnumber, l, mu, m);
        if (Almun != 0.0)
          Ylmnu[i] += Almun * getTesseralFunction(l, m, beta, phi);
      }
    }
    return Ylmnu;
  }

  public static final double getDSphericalHarmonic(int LGnumberC, int LGnumberS,
                                                   int l, int mu, int vu,
                                                   double alpha, double beta, double gamma,
                                                   int strainIndex) {
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

}
