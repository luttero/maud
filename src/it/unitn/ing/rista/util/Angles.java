/*
 * @(#)Angles.java created 14/10/1998 Civezzano
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

package it.unitn.ing.rista.util;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.rta.*;

import java.util.*;

/**
 * The Angles is a class providing static methods for
 * angles translation and computation. Angles in radiants.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:59 $
 * @since JDK1.1
 */


public class Angles {

  static final double sqrt3 = Math.sqrt(3.0);

  private Angles() {
  }

  public static final double[] getLattice(Phase aphase) {
    double acell[] = new double[6];

    for (int i = 0; i < 6; i++)
      acell[i] = aphase.getFullCellValue(i);

    return acell;
  }

  public static final double[] getSuperLattice(Phase aphase) {
    double acell[] = new double[6];

    for (int i = 0; i < 6; i++)
      acell[i] = aphase.getCellValue(i);

    return acell;
  }

/*  public static final double[] getReciprocalLattice(Phase aphase) {
    double astar[] = new double[6];
    double acell[] = getLattice(aphase);

    double cosa = MoreMath.cosd(acell[3]);
    double cosb = MoreMath.cosd(acell[4]);
    double cosg = MoreMath.cosd(acell[5]);
    double sina = MoreMath.sind(acell[3]);
    double sinb = MoreMath.sind(acell[4]);
    double sing = MoreMath.sind(acell[5]);

    double denominator = 1.0 + 2.0 * cosa * cosb * cosg - cosa * cosa
        - cosb * cosb - cosg * cosg;
    astar[0] = sina / (acell[0] * Math.sqrt(denominator));
    astar[1] = sinb / (acell[1] * Math.sqrt(denominator));
    astar[2] = sing / (acell[2] * Math.sqrt(denominator));
    astar[3] = MoreMath.sqrt((cosb * cosg - cosa) / (acell[1] * acell[2] * denominator));
    astar[4] = MoreMath.sqrt((cosg * cosa - cosb) / (acell[0] * acell[2] * denominator));
    astar[5] = MoreMath.sqrt((cosa * cosb - cosg) / (acell[0] * acell[1] * denominator));

    return astar;
  }*/

  static double[] sctf = new double[2];

  public static final double[] tfhkl(double h, double k, double l, double c31, double c23, double c12,
                                     double s31, double cda, double cdb) {
/* Local variables */
    double q, r, x, y, z, fr, tr, arg;

/*     Theta,Phi calculation for given (H,K,L) and Lattice parameters
 *     Output : Theta,Phi */

    q = c12 * 2. * c31 * c23 + 1. - c12 * c12 - c23 * c23 - c31 * c31;
    q = Math.sqrt(q);
    x = cda * h - c31 * l;
    y = (c31 * c23 - c12) * x;
    y += s31 * s31 * (cdb * k - c23 * l);
    y /= q;
    z = s31 * l;
    r = x * x + y * y + z * z;
    r = Math.sqrt(r);
    x /= r;
    z /= r;
    sctf[0] = 0.;
    sctf[1] = 0.;
    if (Math.abs(z) >= 1.) {
      if (z < 0.)
        sctf[0] = Constants.PI;
      return sctf;
    }
    sctf[0] = Math.acos(z);
    ;
    arg = x / Math.sin(sctf[0]);
    if (Math.abs(arg) >= 1.) {
      sctf[1] = Math.acos(arg / Math.abs(arg));
      return sctf;
    }
    fr = Math.acos(arg);
    if (y < 0.)
      fr = Constants.PI2 - fr;
    sctf[1] = fr;
    return sctf;
  } /* tfhkl_ */

  public static final double[] getPhicosPhi(Phase aphase, double h, double k, double l) {

    double[] phicosPhi = new double[2];
    double[] cdsc = aphase.lattice();
    tfhkl(h, k, l, cdsc[7], cdsc[5], cdsc[3], cdsc[6], cdsc[0], cdsc[1]);

    phicosPhi[1] = sctf[0];
    phicosPhi[0] = sctf[1];
    return phicosPhi;

  }

  public static final double[] getPhicosPhi(int PGnumberLconv, int h, int k, int l,
                                            double acell[], double astar[],
                                            double dspace) {


    double phicosPhi[] = new double[2];
    double tanphi;

    System.out.println(PGnumberLconv);
    switch (PGnumberLconv) {
      case 0:
      case 1:
      case 4:
      case 5:
      case 6:
      case 7:
      case 9:
      case 12:
      case 13:
      case 14:
      case 15:
        phicosPhi[1] = dspace * (h * astar[0] * astar[4]
            + k * astar[1] * astar[3]
            + l * astar[2]);
        if (h != 0) {
          tanphi = (k * acell[0] / acell[1] - h * MoreMath.cosd(acell[5]))
              / (h * MoreMath.sind(acell[5]));
          phicosPhi[0] = MoreMath.atand(tanphi);
        } else
          phicosPhi[0] = 90.0;
        break;
      case 2:
        phicosPhi[1] = dspace * (l * astar[2] * astar[3]
            + h * astar[0] * astar[5]
            + k * astar[1]);
        if (l != 0) {
          tanphi = (h * acell[2] / acell[0] - l * MoreMath.cosd(acell[4]))
              / (l * MoreMath.sind(acell[4]));
          phicosPhi[0] = MoreMath.atand(tanphi);
        } else
          phicosPhi[0] = 90.0;
        break;
      case 3:
        phicosPhi[1] = dspace * (k * astar[1] * astar[5]
            + l * astar[2] * astar[4]
            + h * astar[0]);
        if (k != 0) {
          tanphi = (l * acell[1] / acell[2] - k * MoreMath.cosd(acell[3]))
              / (k * MoreMath.sind(acell[3]));
          phicosPhi[0] = MoreMath.atand(tanphi);
        } else
          phicosPhi[0] = 90.0;
        break;
      case 8:
      case 10:
        phicosPhi[1] = (dspace / acell[0]) * (h + k + l) /
            Math.sqrt(3 * (1 + 2 * MoreMath.cosd(acell[3])));
        if (h != k) {
          tanphi = (h + k - 2 * l) / (sqrt3 * (h - k));
          phicosPhi[0] = MoreMath.atand(tanphi);
        } else
          phicosPhi[0] = 90.0 * MoreMath.sign(h + k - 2 * l);
        break;
      case 11:
        phicosPhi[1] = dspace * (h * astar[0] * astar[4]
            + k * astar[1] * astar[3]
            + l * astar[2]);
        if (2 * h != -k) {
          tanphi = sqrt3 * k / (2 * h + k);
          phicosPhi[0] = MoreMath.atand(tanphi);
        } else
          phicosPhi[0] = 90.0 * MoreMath.sign(k);
        break;
      default: {
        phicosPhi[0] = 0.0;
        phicosPhi[1] = 0.0;
      }
    }
    if (phicosPhi[1] > 1.0) {
      System.out.println("Warning cos(phi) = " + Double.toString(phicosPhi[1]));
      phicosPhi[1] = 1.0;
    }
    if (phicosPhi[1] < -1.0) {
      System.out.println("Warning cos(phi) = " + Double.toString(phicosPhi[1]));
      phicosPhi[1] = -1.0;
    }
    phicosPhi[1] = Math.acos(phicosPhi[1]);
    phicosPhi[0] *= Constants.DEGTOPI;
    return phicosPhi;
  }

  public static double[] PSD_comp_ang(double omega, double tiltet, double tiltcsi1) {

    double beta0, cosalpha, sinbeta0;
    double[] theta = new double[2];

    cosalpha = MoreMath.cosd(omega) * MoreMath.cosd(tiltet);
    if (cosalpha > 1.0)
      cosalpha = 1.0;
    else if (cosalpha < -1.0)
      cosalpha = -1.0;
    if (cosalpha < 0.999999999) {
      sinbeta0 = MoreMath.cosd(omega) * MoreMath.sind(tiltet) /
          Math.sqrt(1.0 - cosalpha * cosalpha);
      if (sinbeta0 > 1.0)
        sinbeta0 = 1.0;
      else if (sinbeta0 < -1.0)
        sinbeta0 = -1.0;
      beta0 = Math.asin(sinbeta0);
    } else {
      beta0 = 0.0;
    }
    if (omega > 0.0)
      beta0 = -beta0;
    theta[1] = beta0 * Constants.PITODEG + tiltcsi1;
    theta[0] = Math.acos(cosalpha) * Constants.PITODEG;
    while (theta[0] < 0.0)
      theta[0] += 90.0;
    while (theta[0] > 90.0)
      theta[0] -= 90.0;
    while (theta[1] < 0.0)
      theta[1] += 360.0;
    while (theta[1] > 360.0)
      theta[1] -= 360.0;

    return theta;
  }

  public static double[] polfwink(double chsi, double phigreat, double tetdet, double phidet) {
/*      COMMON/CPI/PI,PIF,P2I

        PI=ACOS(-1.) == pi
        P2I=2.*PI    == 2 * pi
        PIF=PI/180.  == pfd8  */

    double thetar, phir, alphr;
    double betr, gamr, alpha, gamma;
    double anglest[] = new double[3];
    double theta[] = new double[2];

    anglest = star(chsi);

    alpha = 135. + anglest[0];
    if (alpha >= 360.)
      alpha = alpha - 360.;
    gamma = phigreat + 90. + anglest[2];
    if (gamma >= 360.)
      gamma = gamma - 360.;
    if (gamma >= 360.)
      gamma = gamma - 360.;
    alphr = alpha * Constants.DEGTOPI;
    betr = anglest[1] * Constants.DEGTOPI;
    gamr = gamma * Constants.DEGTOPI;

    thetar = tetdet * Constants.DEGTOPI;
    phir = phidet * Constants.DEGTOPI;

    double[] tetres = gmalvector(alphr, betr, gamr, thetar, phir);
    theta[0] = tetres[0];
    theta[1] = tetres[1];
    while (theta[0] > 90.0) {
      theta[0] -= 90.0;
      theta[1] -= 180.0f;
      while (theta[1] < 0.0)
        theta[1] += 360.0;
    }
    return theta;
  }

/*
C
C *************************************************************
C
        SUBROUTINE STAR(PIF,alphst,betst,gamst,chsi)
*/

  public static double[] star(double chsi) {

    double[] anglest = new double[3];
    if (chsi >= 89.9999999 && chsi <= 90.0000001) {
      anglest[1] = 0.;
      anglest[0] = 180.;
      anglest[2] = 0.;
    } else {
      anglest[1] = Math.acos(Math.sin(chsi * Constants.DEGTOPI)) / Constants.DEGTOPI;
      if (chsi < 90.) {
        anglest[0] = 0.;
        anglest[2] = 180.;
      } else {
        anglest[0] = 180.;
        anglest[2] = 0.;
      }
    }
    return anglest;
  }

/*
C
C *************************************************************
C
      SUBROUTINE GMALVECTOR(alphar,betar,gammar,thetar,phir,
     *                      tetres,phires)
C
C      h[phih,thetah] = g y[phiy,thetay]
C
C                            -1          *
C      r[phi,theta] = {[r],0}  (001) -> g ={PI+alf-phiy,bet,0}(0,thetay,0}
C
C                   *                   *
C      thetah = beta   phih = Pi-gam-gam
C
        COMMON/CPI/PI,PIF,P2I
*/

  public static double[] gmalvector(double alphar, double betar, double gammar,
                                    double thetar, double phir) {

    double A2, CA2, SA2, B2, CB2, SB2, B1, CB1, SB1;
    double tetres[] = new double[2];

    A2 = Constants.PI + alphar - phir;
    CA2 = Math.cos(A2);
    SA2 = Math.sin(A2);
    B2 = betar;
    CB2 = Math.cos(B2);
    SB2 = Math.sin(B2);
    B1 = thetar;
    CB1 = Math.cos(B1);
    SB1 = Math.sin(B1);

    double[] als = Uwimvuo.g20g100(CA2, SA2, CB2, SB2, CB1, SB1);

    tetres[0] = als[1] / Constants.DEGTOPI;
    tetres[1] = Constants.PI - gammar - als[2];
    if (tetres[0] == 0. || tetres[0] == 180.)
      tetres[1] = 0.;
    while (tetres[1] < 0.)
      tetres[1] = tetres[1] + 2 * Constants.PI;
    tetres[1] = tetres[1] / Constants.DEGTOPI;

    return tetres;
  }

/*
C
C *************************************************************
C
C
      SUBROUTINE G20G100(PI,CA2,SA2,CB2,SB2,CB1,SB1,AL,BET,GAM,P2I)
C
C Calculates the product of two rotations g=g2*g1, g1=(   0,BET1,0)
C                                                  g2=(ALF2,BET2,0)
C OUTPUT in Radians
C
*/

  public static final void g20g100(double[] angles,
                                   double ca2, double sa2,
                                   double cb2, double sb2,
                                   double cb1, double sb1) {
/* Local variables */
    double ca, cb, cg, sb, fak;

/* Calculates the product of two rotations g=g2*g1, g1=(   0,BET1,0)
                                                    g2=(ALF2,BET2,0)
   OUTPUT in Radians
*/
    cb = cb1 * cb2 - sb1 * sb2 * ca2;
    if (cb < 0.999999999999 && cb > -0.99999999999) {
      angles[1] = Math.acos(cb);
      sb = Math.sin(angles[1]);
      ca = (cb1 * ca2 * sb2 + sb1 * cb2) / sb;
      if (ca >= 0.999999999999)
        angles[0] = 0.;
      else {
        if (ca <= -0.9999999999999)
          angles[0] = Constants.PI;
        else
          angles[0] = Math.acos(ca);
        if (sa2 * sb2 < 0.)
          angles[0] = Constants.PI2 - angles[0];
      }
      cg = (sb1 * ca2 * cb2 + cb1 * sb2) / sb;
      if (cg >= 0.999999999999) {
        angles[2] = 0.;
        return;
      } else {
        if (cg <= -0.999999999999)
          angles[2] = Constants.PI;
        else
          angles[2] = Math.acos(cg);
        if (sb1 * sa2 < 0.)
          angles[2] = Constants.PI2 - angles[2];
        return;
      }
    }
    if (cb >= 0.999999999999) {
      angles[1] = 0.;
      fak = 1.;
    } else {
      angles[1] = Constants.PI;
      fak = -1.;
    }
    angles[0] = 0.;
    cg = fak * (cb1 * ca2 * cb2 - sb1 * sb2);
    if (cg >= 0.9999999999999)
      angles[2] = 0.;
    else {
      if (cg <= -0.999999999999)
        angles[2] = Constants.PI;
      else
        angles[2] = Math.acos(cg);
      if (sa2 * cb2 < 0.)
        angles[2] = Constants.PI2 - angles[2];
    }
  }

  /**
   * Compute the product of two rotations
   */

  public static double[] rotationProduct(double omega2, double chi2, double phi2,
                                         double omega1, double chi1, double phi1) {

    double product[] = new double[3];

    product[0] = omega1 + omega2;

    product[1] = chi1 + chi2;
    product[2] = phi1 + phi2;

/*		double CA2, SA2, CB2, SB2, CB1, SB1;

		CA2 = MoreMath.cosd(omega1 + omega2);
		SA2 = MoreMath.sind(omega1 + omega2);
		CB2 = MoreMath.cosd(chi2);
		SB2 = MoreMath.sind(chi2);
		CB1 = MoreMath.cosd(chi1);
		SB1 = MoreMath.sind(chi1);

    product = Uwimvuo.g20g100(CA2,SA2,CB2,SB2,CB1,SB1);
    product[2] += phi1;*/
    return product;
  }

  public static final Vector getHexagonalGridCoordinates(double resolution,
                                                         double maxpolarangle) {
    return getArbitraryGridCoordinates(resolution, 6, maxpolarangle);
  }

  public static final Vector getArbitraryGridCoordinates(double resolution, int division,
                                                         double maxpolarangle) {

    maxpolarangle = maxpolarangle * maxpolarangle + 0.01;

    double costheta[] = new double[division];
    double sintheta[] = new double[division];
    double dtheta = 360 / division;
    for (int i = 0; i < division; i++) {
      double theta = dtheta * i;
      costheta[i] = MoreMath.cosd(theta);
      sintheta[i] = MoreMath.sind(theta);
    }

    Vector pointarray = new Vector(0, 1);

    double coord[] = new double[2];
    double tmpcoord[] = null;
    coord[0] = 0.0;
    coord[1] = 0.0;

    pointarray.addElement(coord);
    int firstadded = 0;
    int lastadded = 1;
    boolean alladded = false;

    while (!alladded) {
      for (int i = firstadded; i < lastadded; i++) {
        tmpcoord = (double[]) pointarray.elementAt(i);
        for (int j = 0; j < division; j++) {
          coord = new double[2];
          coord[0] = tmpcoord[0] + resolution * costheta[j];
          coord[1] = tmpcoord[1] + resolution * sintheta[j];
          double radius2 = coord[0] * coord[0] + coord[1] * coord[1];
          if (radius2 <= maxpolarangle && radius2 >
              tmpcoord[0] * tmpcoord[0] + tmpcoord[1] * tmpcoord[1]) {
            boolean present = false;
            int actualsize = pointarray.size();
            for (int k = 0; k < actualsize; k++) {
              double[] tmpcoord1 = (double[]) pointarray.elementAt(k);
              if (samePosition(tmpcoord1, coord, 2, 0.001))
                present = true;
            }
            if (!present)
              pointarray.addElement(coord);
          }
        }
      }
      firstadded = lastadded;
      lastadded = pointarray.size();
      if (firstadded == lastadded)
        alladded = true;
    }

    return pointarray;
  }

  public static final boolean samePosition(double[] coord1, double[] coord2,
                                           int dimension, double tolerance) {
    boolean coincide = true;

    for (int i = 0; i < dimension; i++)
      if (Math.abs(coord1[i] - coord2[i]) > tolerance)
        coincide = false;

    return coincide;
  }

  public static double getAngleR(double cosAngle, double sinAngle) {
    if (Math.abs(cosAngle) > 1.0E-18) {
      double angle = Math.atan(sinAngle / cosAngle);
      if (sinAngle < 0.0)
        angle += Math.PI;
      return angle;
    } else {
      if (sinAngle >= 0)
        return Math.PI / 2.0;
      else
        return -Math.PI / 2.0;
    }
  }

  public static double getAngle(double cosAngle, double sinAngle) {
    return getAngleR(cosAngle, sinAngle) * Constants.PITODEG;
  }

  public static final double[][] getEulerMatrix(double alpha, double beta, double gamma) {

    // Matthies or normal convention

    double[][] euler = new double[3][3];

    double cosa = Math.cos(alpha);
    double sina = Math.sin(alpha);
    double cosb = Math.cos(beta);
    double sinb = Math.sin(beta);
    double cosg = Math.cos(gamma);
    double sing = Math.sin(gamma);

    euler[0][0] = cosa * cosb * cosg - sina * sing;
    euler[0][1] = sina * cosb * cosg + cosa * sing;
    euler[0][2] = -sinb * cosg;
    euler[1][0] = -cosa * cosb * sing - sina * cosg;
    euler[1][1] = -sina * cosb * sing + cosa * cosg;
    euler[1][2] = sinb * sing;
    euler[2][0] = cosa * sinb;
    euler[2][1] = sina * sinb;
    euler[2][2] = cosb;

    return euler;
  }

  public static final double[][] getLMatrixPopa(double[][] euler) {
    double[][] Lmatrix = new double[3][3];
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++)
        Lmatrix[i][j] = euler[i][j] * euler[i][j];
    return Lmatrix;
  }

  public static final double[][] getLtMatrixPopa(double[][] euler) {
    double[][] Lmatrix = new double[3][3];
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++)
        Lmatrix[j][i] = euler[i][j] * euler[i][j];
    return Lmatrix;
  }

  public static final double[][] getNMatrixPopa(double[][] euler) {
    double[][] Nmatrix = new double[3][3];
    int k = 0, l = 0;

    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++) {
        switch (i) {
          case 0:
            k = 1;
            l = 2;
            break;
          case 1:
            k = 0;
            l = 2;
            break;
          case 2:
            k = 0;
            l = 1;
            break;
          default: {
          }
        }
        Nmatrix[i][j] = euler[k][j] * euler[l][j];
      }
    return Nmatrix;
  }

  public static final double[][] get2NtMatrixPopa(double[][] euler) {
    double[][] Nmatrix = new double[3][3];
    int k = 0, l = 0;

    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++) {
        switch (i) {
          case 0:
            k = 1;
            l = 2;
            break;
          case 1:
            k = 0;
            l = 2;
            break;
          case 2:
            k = 0;
            l = 1;
            break;
          default: {
          }
        }
        Nmatrix[j][i] = 2.0 * euler[k][j] * euler[l][j];
      }
    return Nmatrix;
  }

  public static final double[][] get2MMatrixPopa(double[][] euler) {
    double[][] Mmatrix = new double[3][3];
    int k = 0, l = 0;

    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++) {
        switch (i) {
          case 0:
            k = 1;
            l = 2;
            break;
          case 1:
            k = 0;
            l = 2;
            break;
          case 2:
            k = 0;
            l = 1;
            break;
          default: {
          }
        }
        Mmatrix[j][i] = 2.0 * euler[j][k] * euler[j][l];
      }
    return Mmatrix;
  }

  public static final double[][] getMtMatrixPopa(double[][] euler) {
    double[][] Mmatrix = new double[3][3];
    int k = 0, l = 0;

    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++) {
        switch (i) {
          case 0:
            k = 1;
            l = 2;
            break;
          case 1:
            k = 0;
            l = 2;
            break;
          case 2:
            k = 0;
            l = 1;
            break;
          default: {
          }
        }
        Mmatrix[j][i] = euler[j][k] * euler[j][l];
      }
    return Mmatrix;
  }

  public static final double[][] getOMatrixPopa(double[][] euler) {
    double[][] Omatrix = new double[3][3];
    Omatrix[0][0] = euler[1][1] * euler[2][2] + euler[1][2] * euler[2][1];
    Omatrix[1][1] = euler[2][2] * euler[0][0] + euler[0][2] * euler[2][0];
    Omatrix[2][2] = euler[1][1] * euler[0][0] + euler[1][0] * euler[0][1];
    Omatrix[0][1] = euler[2][2] * euler[1][0] + euler[2][0] * euler[1][2];
    Omatrix[0][2] = euler[1][1] * euler[2][0] + euler[1][0] * euler[2][1];
    Omatrix[1][0] = euler[2][2] * euler[0][1] + euler[2][1] * euler[0][2];
    Omatrix[1][2] = euler[0][0] * euler[2][1] + euler[0][1] * euler[2][0];
    Omatrix[2][0] = euler[1][1] * euler[0][2] + euler[1][2] * euler[0][1];
    Omatrix[2][1] = euler[0][0] * euler[1][2] + euler[0][2] * euler[1][0];
    return Omatrix;
  }

  public static final double[][] getOtMatrixPopa(double[][] euler) {
    double[][] Omatrix = new double[3][3];
    Omatrix[0][0] = euler[1][1] * euler[2][2] + euler[1][2] * euler[2][1];
    Omatrix[1][1] = euler[2][2] * euler[0][0] + euler[0][2] * euler[2][0];
    Omatrix[2][2] = euler[1][1] * euler[0][0] + euler[1][0] * euler[0][1];
    Omatrix[1][0] = euler[2][2] * euler[1][0] + euler[2][0] * euler[1][2];
    Omatrix[2][0] = euler[1][1] * euler[2][0] + euler[1][0] * euler[2][1];
    Omatrix[0][1] = euler[2][2] * euler[0][1] + euler[2][1] * euler[0][2];
    Omatrix[2][1] = euler[0][0] * euler[2][1] + euler[0][1] * euler[2][0];
    Omatrix[0][2] = euler[1][1] * euler[0][2] + euler[1][2] * euler[0][1];
    Omatrix[1][2] = euler[0][0] * euler[1][2] + euler[0][2] * euler[1][0];
    return Omatrix;
  }

  public static final double[][] getPMatrixPopa(double alpha, double beta, double gamma) {
    return getPMatrixPopa(getEulerMatrix(alpha, beta, gamma));
  }

  public static final double[][] getPMatrixPopa(double[][] euler) {
    return getPMatrixPopa(getLtMatrixPopa(euler), get2NtMatrixPopa(euler),
        getMtMatrixPopa(euler), getOtMatrixPopa(euler));
  }

  public static final double[][] getPMatrixPopa(double[][] Lt, double[][] N2t,
                                                double[][] Mt, double[][] Ot) {
    double[][] Pmatrix = new double[6][6];

    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++)
        Pmatrix[i][j] = Lt[i][j];

    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++)
        Pmatrix[i + 2][j] = N2t[i][j];

    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++)
        Pmatrix[i][j + 2] = Mt[i][j];

    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++)
        Pmatrix[i + 2][j + 2] = Ot[i][j];

    return Pmatrix;
  }

  public static final double[] trasformInSampleRefFrame(double[] orVector,
                                                        double alpha, double beta, double gamma) {
    double[][] PMatrix = getPMatrixPopa(getEulerMatrix(alpha, beta, gamma));
    double[] trasfVector = new double[6];
    for (int i = 0; i < 6; i++) {
      trasfVector[i] = 0.0;
      for (int j = 0; j < 6; j++)
        trasfVector[i] += PMatrix[i][j] * orVector[j];
    }
    return trasfVector;
  }

  public static final void getTheta2EtaFromPixelDetector(int width, double pixelWidth, double pixelHeight,
                                                         double[] x, double[] y,
                                                         double centerX, double centerY,
                                                         double[] theta2, double[] eta,
                                                         double sigma, double sigmaDA, double phiDA,
                                                         double omegaDN, double Dd) {
    getXYFromPixelIndex(width, pixelWidth, pixelHeight, x, y, centerX, centerY);
    getTheta2EtaFromXYPixelDetector(x, y, theta2, eta, sigma, sigmaDA, phiDA, omegaDN, Dd);
  }

  public static void getXYFromPixelIndex(int width, double pixelWidth, double pixelHeight,
                                               double[] x, double[] y, double centerX, double centerY) {
    // width, centerX, centerY in pixels
    // the pixel index should be something like: i = nx + ny * width
    for (int i = 0; i < x.length; i++) {
      int ny = i / width;
      int nx = i - ny * width;
      x[i] = pixelWidth * (nx - centerX);
      y[i] = pixelHeight * (ny - centerY);
    }
  }



  public static void getTheta2EtaFromXYPixelDetector(double[] x, double[] y, double[] theta2,
                                                           double[] eta, double omega, double detector2Theta,
                                                           double phiDA, double omegaDN, double etaDA, double detectorDistance,
                                                           double zs) {

    double[][] tmat = ConvertImageToSpectra.getTransformationMatrixNew(omegaDN, phiDA, etaDA, detector2Theta, omega);
    if (Math.abs(omega) > 1.0E-2)
      zs /= Math.sin(omega * Constants.DEGTOPI);
    for (int i = 0; i < x.length; i++) {
      double[] xf = ConvertImageToSpectra.getTransformedVectorNew(tmat, x[i], y[i], 0.0, 0.0, detectorDistance);
      double[] etatheta = ConvertImageToSpectra.get2ThetaEtaNew(xf);
      theta2[i] = etatheta[0];
      eta[i] = etatheta[1];
    }
  }



  public static void getTheta2EtaFromXYPixelDetector(double[] x, double[] y, double[] theta2,
                                                           double[] eta, double sigma, double sigmaDA,
                                                           double phiDA, double omegaDN, double Dd) {
    // everything in radians

/*    System.out.println("Sigma = " + sigma * Constants.PITODEG);
    System.out.println("SigmaDA = " + sigmaDA * Constants.PITODEG);
    System.out.println("PhiDA = " + phiDA * Constants.PITODEG);
    System.out.println("omegaDN = " + omegaDN * Constants.PITODEG);
    System.out.println("Distance = " + Dd);*/
    double csigma = Math.cos(sigma);
    double ssigma = Math.sin(sigma);
    double csigmaDA = Math.cos(sigmaDA);
    double ssigmaDA = Math.sin(sigmaDA);
    double cphiDA = Math.cos(phiDA);
    double sphiDA = Math.sin(phiDA);
    double comegaDN = Math.cos(omegaDN);
    double somegaDN = Math.sin(omegaDN);
/*    double ca = -cphiDA;
    double sa = -sphiDA;
    double cb = -ssigmaDA;
    double sb = csigmaDA;
    double cg = -somegaDN;
    double sg = comegaDN;
//    rsource=[csigma; 0; ssigma]
//    cRvect=[x y 0]
    double g11 = ca * cb * cg - sa * sg;
    double g12 = sa * cb * cg - ca * sg;
    double g13 = -sb * cg;
    double g21 = -ca * cb * sg - sa * cg;
    double g22 = -sa * cb * sg + ca * cg;
    double g23 = sb * sg;*/
    double g11 = -somegaDN * ssigmaDA * cphiDA + comegaDN * sphiDA;
    double g12 = -comegaDN * ssigmaDA * cphiDA - somegaDN * sphiDA;
//    double g13 = -csigmaDA * cphiDA;
    double g21 = -somegaDN * ssigmaDA * sphiDA - comegaDN * cphiDA;
    double g22 = -comegaDN * ssigmaDA * sphiDA + somegaDN * cphiDA;
//    double g23 = -csigmaDA * sphiDA;
    double g31 = somegaDN * csigmaDA;
    double g32 = comegaDN * csigmaDA;
//    double g33 = -ssigmaDA;
//    DAm=Dd*[csigmaDA*cphiDA; csigmaDA*sphiDA; ssigmaDA]
    double DAm11 = Dd * csigmaDA * cphiDA;
    double DAm21 = Dd * csigmaDA * sphiDA;
    double DAm31 = Dd * ssigmaDA;

//  everything should me optimized at maximum below

    for (int i = 0; i < x.length; i++) {
//    KBdetV=cRvect*g
//    KBdetVc=[KBdetV(1,1); KBdetV(1,2); KBdetV(1,3)]
      double KBdetV11 = x[i] * g11 + y[i] * g12;
      double KBdetV21 = x[i] * g21 + y[i] * g22;
      double KBdetV31 = x[i] * g31 + y[i] * g32;
//    cRscat=DAm+KBdetVc
      KBdetV11 += DAm11;
      KBdetV21 += DAm21;
      KBdetV31 += DAm31;
//    rscat=cRscat/sqrt(cRscat(1,1)*cRscat(1,1)+cRscat(2,1)*cRscat(2,1)+cRscat(3,1)*cRscat(3,1))
      double normcRscat = 1.0 / Math.sqrt(KBdetV11 * KBdetV11 + KBdetV21 * KBdetV21 + KBdetV31 * KBdetV31);
      KBdetV11 *= normcRscat;
      KBdetV21 *= normcRscat;
      KBdetV31 *= normcRscat;
//    rsum=rsource+rscat
      double rsum11 = KBdetV11 + csigma;
      double rsum31 = KBdetV31 + ssigma;
//    rbelow=[-csigma) 0 -ssigma]
//    ctheta2=rbelow*rscat

      double ctheta2 = -csigma * KBdetV11 - ssigma * KBdetV31;
      theta2[i] = Math.acos(ctheta2);

//      theta2[i] = Math.acos(-KBdetV11);
//      if (KBdetV31 < 0.0)
//        theta2[i] = -theta2[i];   todo
//      theta2[i] += sigma;
//    Zb=[-ssigma 0 csigma]
//    Nvect=rsum/sqrt(rsum(1,1)*rsum(1,1)+rsum(2,1)*rsum(2,1)+rsum(3,1)*rsum(3,1))
//    ceta=Zb*Nvect
      double normNvect = Math.sqrt(rsum11 * rsum11 + KBdetV21 * KBdetV21 + rsum31 * rsum31);
      double ceta = -rsum11 * ssigma + rsum31 * csigma;
      if (Math.abs(ceta / normNvect) < 1.0E-9) {
        if (KBdetV21 < 0)
          eta[i] = -Math.PI / 2;
        else
          eta[i] = Math.PI / 2;
      } else
        eta[i] = Math.atan(KBdetV21 / ceta);
    }
//    System.out.println("xy min max:" + xmin + " " + xmax + " " + ymin + " " + ymax);
  }

  public static final double[][] spectraFromPixels(double[] theta2, double[] eta, int[] intensity, double etaStep,
                                                   double theta2Step) {

    double min2theta = 2.0 * Math.PI;
    double max2theta = 0.0;
    double mineta = 2 * Math.PI;
    double maxeta = 0.0;
    for (int i = 0; i < theta2.length; i++) {
      if (min2theta > theta2[i])
        min2theta = theta2[i];
      if (max2theta < theta2[i])
        max2theta = theta2[i];
      if (mineta > eta[i])
        mineta = eta[i];
      if (maxeta < eta[i])
        maxeta = eta[i];
    }
    double[] theta2Centers = centers(min2theta, max2theta, theta2Step);
    double[] etaCenters = centers(mineta, maxeta, etaStep);

    return spectraFromPixelsByEtaTheta2(theta2, eta, theta2Centers, etaCenters, intensity);
  }

  public static final double[] centers(double mineta, double maxeta, double etaStep) {
    int count = 0;
    int finalCount = 0;
    double startingEta = 0.0;
    boolean minFound = false;
    for (double eta1 = -2.0 * Math.PI; eta1 < 2.0 * Math.PI; eta1 += etaStep, ++count)
      if (!minFound && eta1 >= mineta) {
        startingEta = eta1;
        minFound = true;
        count = 0;
      } else if (eta1 <= maxeta) {
        finalCount = count;
      }
    double[] etaCenters = new double[finalCount];
    for (int i = 0; i < finalCount; i++)
      etaCenters[i] = startingEta + i * etaStep;
    return etaCenters;
  }

  public static final double[][] spectraFromPixels(double[] theta2, double[] eta, double[] intensity, double etaStep,
                                                   double theta2Step) {

    double min2theta = 2.0 * Math.PI;
    double max2theta = 0.0;
    double mineta = 2 * Math.PI;
    double maxeta = 0.0;
    for (int i = 0; i < theta2.length; i++) {
      if (min2theta > theta2[i])
        min2theta = theta2[i];
      if (max2theta < theta2[i])
        max2theta = theta2[i];
      if (mineta > eta[i])
        mineta = eta[i];
      if (maxeta < eta[i])
        maxeta = eta[i];
    }
    double[] theta2Centers = centers(min2theta, max2theta, theta2Step);
    double[] etaCenters = centers(mineta, maxeta, etaStep);

    return spectraFromPixelsByEtaTheta2(theta2, eta, theta2Centers, etaCenters, intensity);
  }

/*  public static final double[][] spectraFromPixels(double[] theta2, double[] eta, double[] intensity, double etaStep,
                                                   double theta2Step) {

    double min2theta = 2.0 * Math.PI;
    double max2theta = 0.0;
    double mineta = 2 * Math.PI;
    double maxeta = 0.0;
    for (int i = 0; i < theta2.length; i++) {
      if (min2theta > theta2[i])
        min2theta = theta2[i];
      if (max2theta < theta2[i])
        max2theta = theta2[i];
      if (mineta > eta[i])
        mineta = eta[i];
      if (maxeta < eta[i])
        maxeta = eta[i];
    }
    double[] theta2Centers = centers(min2theta, max2theta, theta2Step);
    double[] etaCenters = centers(mineta, maxeta, etaStep);

    return spectraFromPixelsByEtaTheta2(theta2, eta, theta2Centers, etaCenters, intensity);
  }*/

  public static final double[][] spectraFromPixelsByEtaTheta2(double[] theta2, double[] eta, double[] theta2Centers,
                                                              double[] etaCenters, double[] intensity) {
    Vector all2Theta = new Vector(theta2Centers.length, 10);
    for (int i = 0; i < theta2Centers.length; i++) {
      Vector allEta = new Vector(etaCenters.length, 10);
      for (int j = 0; j < etaCenters.length; j++)
        allEta.addElement(new Vector(10, 10));
      all2Theta.addElement(allEta);
    }
    for (int i = 0; i < theta2.length; i++) {
      boolean located = false;
      int iTheta = 0, iEta = 0;
      while (!located && iTheta < theta2Centers.length) {
        double thetaMin, thetaMax;
        if (iTheta > 0)
          thetaMin = (theta2Centers[iTheta] + theta2Centers[iTheta - 1]) / 2;
        else
          thetaMin = theta2Centers[iTheta] + (theta2Centers[iTheta] - theta2Centers[iTheta + 1]) / 2;
        if (iTheta < theta2Centers.length - 1)
          thetaMax = (theta2Centers[iTheta] + theta2Centers[iTheta + 1]) / 2;
        else
          thetaMax = theta2Centers[iTheta] + (theta2Centers[iTheta] - theta2Centers[iTheta - 1]) / 2;
        while (!located && iEta < etaCenters.length) {
          double etaMin, etaMax;
          if (iEta > 0)
            etaMin = (etaCenters[iEta] + etaCenters[iEta - 1]) / 2;
          else
            etaMin = etaCenters[iEta] + (etaCenters[iEta] - etaCenters[iEta + 1]) / 2;
          if (iEta < etaCenters.length - 1)
            etaMax = (etaCenters[iEta] + etaCenters[iEta + 1]) / 2;
          else
            etaMax = etaCenters[iEta] + (etaCenters[iEta] - etaCenters[iEta - 1]) / 2;
          if ((thetaMin <= theta2[i] && theta2[i] < thetaMax) && (etaMin <= eta[i] && eta[i] < etaMax)) {
            located = true;
            Vector container = (Vector) ((Vector) all2Theta.elementAt(iTheta)).elementAt(iEta);
            int[] index = new int[1];
            index[0] = i;
            container.addElement(index);
          }
          iEta++;
        }
        iTheta++;
      }
    }

    double[][] spectra = new double[etaCenters.length][theta2Centers.length];
    for (int iTheta = 0; iTheta < theta2Centers.length; iTheta++) {
      Vector thetaContainer = (Vector) all2Theta.elementAt(iTheta);
      for (int iEta = 0; iEta < etaCenters.length; iEta++) {
        Vector container = (Vector) thetaContainer.elementAt(iEta);
        int numberPixels = container.size();
        for (int i = 0; i < numberPixels; i++) {
          int[] pixelIndex = (int[]) container.elementAt(i);
          spectra[iEta][iTheta] += intensity[pixelIndex[0]];
        }
        if (numberPixels > 0) {
          spectra[iEta][iTheta] /= numberPixels;
          container.removeAllElements();
        } else
          spectra[iEta][iTheta] = -1;
      }
      thetaContainer.removeAllElements();
    }
    all2Theta.removeAllElements();
    all2Theta = null;
    System.gc();
    return spectra;
  }

/*  public static final double[][] spectraFromPixelsByEtaTheta2(double[] theta2, double[] eta, double[] theta2Centers,
                                                              double[] etaCenters, double[] intensity) {
    Vector all2Theta = new Vector(theta2Centers.length, 10);
    for (int i = 0; i < theta2Centers.length; i++) {
      Vector allEta = new Vector(etaCenters.length, 10);
      for (int j = 0; j < etaCenters.length; j++)
        allEta.addElement(new Vector(10, 10));
      all2Theta.addElement(allEta);
    }
    for (int i = 0; i < theta2.length; i++) {
      boolean located = false;
      int iTheta = 0, iEta = 0;
      while (!located && iTheta < theta2Centers.length) {
        double thetaMin, thetaMax;
        if (iTheta > 0)
          thetaMin = (theta2Centers[iTheta] + theta2Centers[iTheta - 1]) / 2;
        else
          thetaMin = theta2Centers[iTheta] + (theta2Centers[iTheta] - theta2Centers[iTheta + 1]) / 2;
        if (iTheta < theta2Centers.length - 1)
          thetaMax = (theta2Centers[iTheta] + theta2Centers[iTheta + 1]) / 2;
        else
          thetaMax = theta2Centers[iTheta] + (theta2Centers[iTheta] - theta2Centers[iTheta - 1]) / 2;
        while (!located && iEta < etaCenters.length) {
          double etaMin, etaMax;
          if (iEta > 0)
            etaMin = (etaCenters[iEta] + etaCenters[iEta - 1]) / 2;
          else
            etaMin = etaCenters[iEta] + (etaCenters[iEta] - etaCenters[iEta + 1]) / 2;
          if (iEta < etaCenters.length - 1)
            etaMax = (etaCenters[iEta] + etaCenters[iEta + 1]) / 2;
          else
            etaMax = etaCenters[iEta] + (etaCenters[iEta] - etaCenters[iEta - 1]) / 2;
          if ((thetaMin <= theta2[i] && theta2[i] < thetaMax) && (etaMin <= eta[i] && eta[i] < etaMax)) {
            located = true;
            Vector container = (Vector) ((Vector) all2Theta.elementAt(iTheta)).elementAt(iEta);
            int[] index = new int[1];
            index[0] = i;
            container.addElement(index);
          }
          iEta++;
        }
        iTheta++;
      }
    }

    double[][] spectra = new double[etaCenters.length][theta2Centers.length];
    for (int iTheta = 0; iTheta < theta2Centers.length; iTheta++) {
      Vector thetaContainer = (Vector) all2Theta.elementAt(iTheta);
      for (int iEta = 0; iEta < etaCenters.length; iEta++) {
        Vector container = (Vector) thetaContainer.elementAt(iEta);
        int numberPixels = container.size();
        for (int i = 0; i < numberPixels; i++) {
          int[] pixelIndex = (int[]) container.elementAt(i);
          spectra[iEta][iTheta] += intensity[pixelIndex[0]];
        }
        if (numberPixels > 0) {
          spectra[iEta][iTheta] /= numberPixels;
          container.removeAllElements();
        } else
          spectra[iEta][iTheta] = -1;
      }
      thetaContainer.removeAllElements();
    }
    all2Theta.removeAllElements();
    all2Theta = null;
    System.gc();
    return spectra;
  }*/

  public static final double[][] spectraFromPixelsByEtaTheta2(double[] theta2, double[] eta, double[] theta2Centers,
                                                              double[] etaCenters, int[] intensity) {
    Vector all2Theta = new Vector(theta2Centers.length, 10);
    double thetaMin, thetaMax;
    double etaMin, etaMax;
    for (int i = 0; i < theta2Centers.length; i++) {
      if (i > 0)
        thetaMin = (theta2Centers[i] + theta2Centers[i - 1]) / 2;
      else
        thetaMin = theta2Centers[i] + (theta2Centers[i] - theta2Centers[i + 1]) / 2;
      if (i < theta2Centers.length - 1)
        thetaMax = (theta2Centers[i] + theta2Centers[i + 1]) / 2;
      else
        thetaMax = theta2Centers[i] + (theta2Centers[i] - theta2Centers[i - 1]) / 2;
      Vector allEta = new Vector(etaCenters.length, 10);
      for (int j = 0; j < etaCenters.length; j++) {
        if (j > 0)
          etaMin = (etaCenters[j] + etaCenters[j - 1]) / 2;
        else
          etaMin = etaCenters[j] + (etaCenters[j] - etaCenters[j + 1]) / 2;
        if (j < etaCenters.length - 1)
          etaMax = (etaCenters[j] + etaCenters[j + 1]) / 2;
        else
          etaMax = etaCenters[j] + (etaCenters[j] - etaCenters[j - 1]) / 2;
        Vector container = new Vector(10, 10);
        boolean located = false;
        for (int index = 0; index < theta2.length; index++) {
          if ((thetaMin <= theta2[i] && theta2[i] < thetaMax) && (etaMin <= eta[i] && eta[i] < etaMax)) {
            located = true;
            int[] indexFound = new int[1];
            indexFound[0] = index;
            container.addElement(indexFound);
          }
        }
        allEta.addElement(container);
      }
      all2Theta.addElement(allEta);
    }
/*    for (int i = 0; i < theta2.length; i++) {
      boolean located = false;
      int iTheta = 0, iEta = 0;
//      System.out.println("Grouping for 2theta = " + i + " " + theta2[i] * Constants.PITODEG);
      while (!located && iTheta < theta2Centers.length) {
        if (iTheta > 0)
          thetaMin = (theta2Centers[iTheta] + theta2Centers[iTheta - 1]) / 2;
        else
          thetaMin = theta2Centers[iTheta] + (theta2Centers[iTheta] - theta2Centers[iTheta + 1]) / 2;
        if (iTheta < theta2Centers.length - 1)
          thetaMax = (theta2Centers[iTheta] + theta2Centers[iTheta + 1]) / 2;
        else
          thetaMax = theta2Centers[iTheta] + (theta2Centers[iTheta] - theta2Centers[iTheta - 1]) / 2;
        while (!located && iEta < etaCenters.length) {
          if (iEta > 0)
            etaMin = (etaCenters[iEta] + etaCenters[iEta - 1]) / 2;
          else
            etaMin = etaCenters[iEta] + (etaCenters[iEta] - etaCenters[iEta + 1]) / 2;
          if (iEta < etaCenters.length - 1)
            etaMax = (etaCenters[iEta] + etaCenters[iEta + 1]) / 2;
          else
            etaMax = etaCenters[iEta] + (etaCenters[iEta] - etaCenters[iEta - 1]) / 2;
          if ((thetaMin <= theta2[i] && theta2[i] < thetaMax) && (etaMin <= eta[i] && eta[i] < etaMax)) {
            located = true;
            Vector container = (Vector) ((Vector) all2Theta.elementAt(iTheta)).elementAt(iEta);
            int[] index = new int[1];
            index[0] = i;
            container.addElement(index);
          }
          iEta++;
        }
        iTheta++;
      }
    }*/

    double[][] spectra = new double[etaCenters.length][theta2Centers.length];
    for (int iTheta = 0; iTheta < theta2Centers.length; iTheta++) {
      Vector thetaContainer = (Vector) all2Theta.elementAt(iTheta);
      for (int iEta = 0; iEta < etaCenters.length; iEta++) {
        Vector container = (Vector) thetaContainer.elementAt(iEta);
        int numberPixels = container.size();
        for (int i = 0; i < numberPixels; i++) {
          int[] pixelIndex = (int[]) container.elementAt(i);
          spectra[iEta][iTheta] += intensity[pixelIndex[0]];
        }
        if (numberPixels > 0) {
          spectra[iEta][iTheta] /= numberPixels;
//          container.removeAllElements();
        } else
          spectra[iEta][iTheta] = -1;
      }
//      thetaContainer.removeAllElements();
    }
//    all2Theta.removeAllElements();
//    all2Theta = null;
//    System.gc();
    return spectra;
  }

  public static final double[][][] spectraFromPixelsByEtaTheta2(double[] theta2, double[] eta, double[] intensity,
                                                              double[] x, double[] y, double detectorDistance,
                                                              double theta2min, double theta2max, double theta2step,
                                                              double etamin, double etamax, double etastep) {

// System.out.println(theta2min + " " + theta2max + " " + theta2step + " " + etamin + " " + etamax + " " + etastep + " ");
	  double correctionExponent = MaudPreferences.getDouble("image2D.exponentCorrectionValue", 0.0);
    int spectrumPointsNumber = (int) ((theta2max - theta2min) / theta2step) + 1;
    int spectraNumber = (int) ((etamax - etamin) / etastep) + 1;
    double[][][] spectra = new double[3][spectraNumber][spectrumPointsNumber];
    int[][] counts = new int[spectraNumber][spectrumPointsNumber];
	  double denominator = 1.0 / (detectorDistance * detectorDistance);
	  for (int i = 0; i < theta2.length; i++) {
      int itheta = (int) ((theta2[i] - theta2min) / theta2step + 0.5);
      int ieta = (int) ((eta[i] - etamin) / etastep + 0.5);
      if ((itheta >= 0 && itheta < spectrumPointsNumber) && (ieta >= 0 && ieta < spectraNumber)) {
        if (intensity[i] >= 0.0) {
        double radius = x[i] * x[i] + y[i] * y[i];
        radius = radius * denominator;
        radius += 1.0;
          spectra[0][ieta][itheta] += x[i];
          spectra[1][ieta][itheta] += y[i];
        spectra[2][ieta][itheta] += intensity[i] * Math.pow(radius, correctionExponent);
        counts[ieta][itheta]++;
        }
      }
    }
    for (int itheta = 0; itheta < spectrumPointsNumber; itheta++) {
      for (int ieta = 0; ieta < spectraNumber; ieta++) {
        if (counts[ieta][itheta] > 2) {
          spectra[0][ieta][itheta] /= counts[ieta][itheta];
          spectra[1][ieta][itheta] /= counts[ieta][itheta];
          spectra[2][ieta][itheta] /= counts[ieta][itheta];
        } else
          spectra[2][ieta][itheta] = -1;
      }
    }
    return spectra;
  }

  public static void eulerianFromKappaRadians(double alpha, double omegaKappa, double kappa, double phiKappa,
                                              double[] omegaChiPhi) {
    // in radians
    if (Math.abs(kappa) < 1.0E-9) {
      omegaChiPhi[0] = omegaKappa;
      omegaChiPhi[1] = kappa;
      omegaChiPhi[2] = phiKappa;
    } else {

      double sinAlpha = Math.sin(alpha);
      double cosAlpha = Math.cos(alpha);
      kappa /= 2.0;
      double sinKappaHalf = Math.sin(kappa);
      double cosKappaHalf = Math.cos(kappa);
      double cosChiHalf = Math.sqrt(cosAlpha * cosAlpha + sinAlpha * sinAlpha * cosKappaHalf * cosKappaHalf);
      omegaChiPhi[1] = Math.acos(cosChiHalf) * 2.0;
      if (sinAlpha * sinKappaHalf < 0.0)
        omegaChiPhi[1] = Math.PI * 2.0 - omegaChiPhi[1];
      if (Math.abs(cosChiHalf) > 1.0E-9) {
        double delta = cosKappaHalf / cosChiHalf;
        delta = Math.acos(delta);
        if (cosAlpha * sinKappaHalf / cosChiHalf < 0.0)
          delta = Math.PI * 2.0 - delta;
        omegaChiPhi[0] = omegaKappa + delta;
        omegaChiPhi[2] = phiKappa + delta;
      } else {
        omegaChiPhi[0] = omegaKappa;
        omegaChiPhi[2] = phiKappa;
      }
    }
    omegaChiPhi[1] -= Math.PI / 2.0;
  }

  public static void eulerianFromKappaDegrees(double alpha, double omegaKappa, double kappa, double phiKappa,
                                              double[] omegaChiPhi) {
    // in degrees
    eulerianFromKappaRadians(alpha * Constants.DEGTOPI, omegaKappa * Constants.DEGTOPI, kappa * Constants.DEGTOPI,
        phiKappa * Constants.DEGTOPI, omegaChiPhi);
    for (int i = 0; i < 3; i++)
      omegaChiPhi[i] *= Constants.PITODEG;
  }

  public static double[] eulerianFromKappa(double alpha, double omegaKappa, double kappa, double phiKappa) {
    // in degrees
    double[] omegaChiPhi = new double[3];
    eulerianFromKappaDegrees(alpha, omegaKappa, kappa, phiKappa, omegaChiPhi);
    return omegaChiPhi;
  }


/*  public static void getTheta2EtaFromCurvedImageDetector(double[] x, double[] y, double[] theta2,
                                                           double[] eta, double detectorDistance) {
    double sinhalfx;
    int index = 0;
    double radius2 = detectorDistance * detectorDistance;
    for (int i = 0; i < x.length; i++) {
      boolean reverse = false;
      if (Math.abs(x[i]) <= 90.0)
        sinhalfx = MoreMath.sind(x[i]);
      else {
        sinhalfx = MoreMath.sind((180.0 - x[i]));
        reverse = true;
      }
      sinhalfx *= sinhalfx;
      double sinx = MoreMath.sind(x[i]);
      for (int j = 0; j < y.length; j++) {
        double cos2theta = Math.sqrt((1.0 - sinhalfx) / (Math.sqrt(1.0 + y[j] * y[j] / radius2)));
//        System.out.println("x " + x[i] + " y " + y[j] + ", " + sinhalfx + ", " + cos2theta);
        double taneta = y[j] / detectorDistance / sinx;
        theta2[index] = MoreMath.acosd(cos2theta);
        if (reverse)
          theta2[index] = 180.0 - theta2[index];
        eta[index++] = MoreMath.atand(taneta);
//        System.out.println(theta2[index - 1] + ", " + eta[index - 1]);
     }
    }
  }*/

  public static void getTheta2EtaFromCurvedImageDetector(double[] x, double[] y, double[] theta2,
                                                           double[] eta, double detectorDistance,
                                                           double centerX, double centerY) {

	  double cosxr, sinxr, tan2theta, taneta;
	  int index = 0;
	  double radius2 = detectorDistance * detectorDistance;
	  for (int i = 0; i < x.length; i++) {
		  double arg = (x[i] - centerX) / detectorDistance;
		  cosxr = detectorDistance * Math.cos(arg);
		  double cos2xr = cosxr * cosxr;
		  sinxr = detectorDistance * Math.sin(arg);
		  for (int j = 0; j < y.length; j++) {
			  double yr = y[j] - centerY;
			  if (Math.abs(cosxr) > 1E-9) {
			    tan2theta = Math.sqrt((radius2 + yr * yr) / cos2xr - 1);
				  if (cosxr < 0)
					  tan2theta = -tan2theta;
				  theta2[index] = MoreMath.atand(tan2theta);
				  if (arg > Math.PI * 0.5 && theta2[index] < 0)
					  theta2[index] = 180.0 + theta2[index];
			  } else {
				  if (arg > 0 && arg < Math.PI)
					  theta2[index] = 90.0;
				  else
					  theta2[index] = -90.0;
			  }
			  if (Math.abs(sinxr) > 1E-9) {
			    taneta = yr / sinxr;
			    eta[index] = MoreMath.atand(taneta);
			  } else {
				  if (yr < 0)
					  eta[index] = -90;
				  else
					  eta[index] = 90;
			  }
			  index++;
		  }
	  }

/*    double cosx;
    int index = 0;
    double radius2 = detectorDistance * detectorDistance;
    for (int i = 0; i < x.length; i++) {
      cosx = MoreMath.cosd(x[i]);
      double sinx = MoreMath.sind(x[i]);
      for (int j = 0; j < y.length; j++) {
        double cos2theta = cosx * Math.pow(1.0 + y[j] * y[j] / radius2, -0.5);
//        System.out.println("x " + x[i] + " y " + y[j] + ", " + sinhalfx + ", " + cos2theta);
        double taneta = y[j] / detectorDistance / sinx;
        theta2[index] = MoreMath.acosd(cos2theta);
        eta[index++] = MoreMath.atand(taneta);
//        System.out.println(theta2[index - 1] + ", " + eta[index - 1]);
     }
    }*/
  }

  public static double getY(double eta, double radius, double x) {
    double taneta = MoreMath.tand(eta);
    double sinx = MoreMath.sind(x);
    double y = taneta * radius * sinx;
    return -y;
  }

  public static double[][][] spectraFromPixelsByEtaTheta2CurvedDetector(double[] theta2, double[] eta,
                                                                              double[] intensity,
                                                              double[] x, double[] y, double detectorDistance,
																													    double centerX, double centerY,
                                                              double theta2min, double theta2max, double theta2step,
                                                              double etamin, double etamax, double etastep) {

    int spectrumPointsNumber = (int) ((theta2max - theta2min) / theta2step) + 1;
    int spectraNumber = (int) ((etamax - etamin) / etastep) + 1;
    double[][][] spectra = new double[5][spectraNumber][spectrumPointsNumber];
    int[][] counts = new int[spectraNumber][spectrumPointsNumber];
	  int index = 0;
	  double radius = 1.0;
	  double denominator = 1.0 / (detectorDistance * detectorDistance);
//	  boolean applyCorrection = MaudPreferences.getBoolean("image2D.applyDistanceCorrectionToIntensities", true);
	  double correctionExponent = MaudPreferences.getDouble("image2D.exponentCorrectionValue", 0.0);
    for (int i = 0; i < x.length; i++) {
	    for (int j = 0; j < y.length; j++) {
		    int itheta = (int) ((theta2[index] - theta2min) / theta2step + 0.5);
		    int ieta = (int) ((eta[index] - etamin) / etastep + 0.5);
		    if ((itheta >= 0 && itheta < spectrumPointsNumber) && (ieta >= 0 && ieta < spectraNumber)) {
			    if (intensity[index] >= 0.0) {
//				    if (applyCorrection) {
					    double y1 = y[j] - centerY;
				      radius = y1 * y1 * denominator + 1.0;
//				    }
				    spectra[0][ieta][itheta] += theta2[index];
				    spectra[1][ieta][itheta] += eta[index];
				    spectra[2][ieta][itheta] += intensity[index] * Math.pow(radius, correctionExponent);
				    spectra[3][ieta][itheta] += x[i];
				    spectra[4][ieta][itheta] += y[j];
				    counts[ieta][itheta]++;
			    }
		    }
		    index++;
	    }
    }
    for (int itheta = 0; itheta < spectrumPointsNumber; itheta++) {
      for (int ieta = 0; ieta < spectraNumber; ieta++) {
        if (counts[ieta][itheta] > 0) {
          spectra[0][ieta][itheta] /= counts[ieta][itheta];
          spectra[1][ieta][itheta] /= counts[ieta][itheta];
          spectra[2][ieta][itheta] /= counts[ieta][itheta];
	        spectra[3][ieta][itheta] /= counts[ieta][itheta];
	        spectra[4][ieta][itheta] /= counts[ieta][itheta];
        } else
          spectra[2][ieta][itheta] = -1;
      }
    }
    return spectra;
  }


}