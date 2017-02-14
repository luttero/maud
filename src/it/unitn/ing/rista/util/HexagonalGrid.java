/*
 * @(#)HexagonalGrid.java created 17/05/2001 Mesiano
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

/**
 *  The HexagonalGrid is a class fro generating the hexagonal grid coordinate.
 *
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class HexagonalGrid {

  public HexagonalGrid() {
  }

  /**
   * Return the chi, phi angle coordinates for an hexagonal grid
   * coverage up to a given chi angle.
   * Based on Matthies and Wenk paper "On a Hexagonal Grid of Measuring
   * Points for Pole Figure", ???
   * @param maxChi maximum accepted chi angle
   * @param Nhexagon number of "hexagon" divisions for the sphere radius
   * up to 90 degrees in chi.
   */

  public static double[][] getHexagonalGrid(double maxChi, int Nhexagon) {

// compute the number of Hexagon up to 90 degrees for the temporary storage
    int maxHexagon = 1 + 3 * Nhexagon * (Nhexagon + 1);
    double[][] temporaryChiPhi = new double[2][maxHexagon]; // to store temporarily Chi, Phi

    if (maxChi > 90)
      maxChi = 90.0;

    int u = 0, v = 0;
    int index = 0;
    for (int n = 0; n <= Nhexagon; n++) {
      int Mhexagon = 6 * n;
      if (n == 0) {
        double[] chiphit = getChiPhi(0.0, 0.0, (double) Nhexagon);
// we accept only the hexagon points inside the maxChi angle
        if (chiphit[0] <= maxChi) {
          temporaryChiPhi[0][index] = chiphit[0];
          temporaryChiPhi[1][index] = chiphit[1];
          index++;
        }
      }
      for (int m = 1; m <= Mhexagon; m++) {
        if (m <= n) {
// m = 1,2,..., n
          u = n - (m - 1);
          v = m - 1;
        } else if (m <= 2 * n) {
// m = n+1, n+2, ..., 2n
          u = n - (m - 1);
          v = m;
        } else if (m <= 3 * n) {
// m = 2n+1, 2n+2, ..., 3n
          u = -n;
          v = 3 * n - (m - 1);
        } else if (m <= 4 * n) {
// m = 3n+1, 3n+2, ..., 4n
          u = -4 * n + (m - 1);
          v = 3 * n - (m - 1);
        } else if (m <= 5 * n) {
// m = 4n+1, 4n+2, ..., 5n
          u = -4 * n + (m - 1);
          v = -n;
        } else if (m <= 6 * n) {
// m = 5n+1, 5n+2, ..., 6n
          u = n;
          v = -6 * n + (m - 1);
        } else {
// this shouldn't happen
          System.out.println("Error on Hexagon, m > 6n");
        }
        double[] chiphit = getChiPhi((double) u, (double) v, (double) Nhexagon);

// we accept only the hexagon points inside the maxChi angle
//        System.out.println("m: " + m + " , n: " + n);
        if (chiphit[0] <= maxChi) {
          temporaryChiPhi[0][index] = chiphit[0];
          temporaryChiPhi[1][index] = chiphit[1];
//          System.out.println("Accepted: " + chiphit[0] + ", " + chiphit[1]);
          index++;
        } else {
//          System.out.println("Rejected: " + chiphit[0] + ", " + chiphit[1]);
        }
      }
    }

// initialize the vector to be returned
    double[][] ChiPhi = new double[2][index];

    for (int i = 0; i < index; i++) {
      ChiPhi[0][i] = temporaryChiPhi[0][i];
      ChiPhi[1][i] = temporaryChiPhi[1][i];

// for testing
// System.out.println(ChiPhi[0][i] + ", " + ChiPhi[1][i]);
    }

    return ChiPhi;
  }

  /**
   * Return the chi, phi angle coordinates for the points u, v as
   * a vector of two double numbers in degrees.
   * See Matthies and Wenk paper "On a Hexagonal Grid of Measuring
   * Points for Pole Figure", ???
   * @param u first hexagonal coordinate
   * @param v second hexagonal coordinate
   * @param Ndiv number of "hexagon" divisions for the sphere radius
   */

  public static double[] getChiPhi(double u, double v, double Ndiv) {

// initialize the vector to be returned
    double[] ChiPhi = new double[2];

    if (u == 0 && v == 0) {
// special case
      ChiPhi[0] = 0.0;
      ChiPhi[1] = 0.0;
      return ChiPhi;
    }

// Compute Xp, Yp, Rp
    double uv2 = u + v / 2;
    double sqrtuv = Math.sqrt(u * u + u * v + v * v);
    double Xp = Constants.sqrt2 * uv2 / Ndiv;
    double Yp = Constants.sqrt3 * v / (Constants.sqrt2 * Ndiv);
    double Rp = Constants.sqrt2 * sqrtuv / Ndiv;

    if (Rp / 2.0 > 1.0) {
      ChiPhi[0] = 100.0;
      ChiPhi[1] = 0.0;
    } else {
// Compute chi angle and transform it from radiants to degrees
      ChiPhi[0] = 2.0 * Math.asin(Rp / 2.0) * Constants.PITODEG;
//    System.out.println(ChiPhi[0] + ", " + Rp / 2.0 + ", " + u + ", " + v);

// Compute phi angle and transform it from radiants to degrees
      ChiPhi[1] = Math.acos(uv2 / sqrtuv) * Constants.PITODEG;
      if (v < 0)
        ChiPhi[1] = 360.0 - ChiPhi[1];
    }

    return ChiPhi;
  }

}
