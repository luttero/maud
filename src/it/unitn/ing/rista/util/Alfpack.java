/*
 * @(#)Alfpack.java created 2/11/1999 Berkeley, Rudy's home
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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
 *  The Alfpack is a class providing static methods for
 *  Legrende polynomial computation. Angle in radiants.
 *
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class Alfpack {

  public static final double legrende(int l, int m, double theta) {
    double legr = lfpt(l, m, theta, alfk(l, m)) / Constants.SQRTPI2;
    if (MoreMath.odd(m))
      legr = -legr;
    return legr;
  }

  /* lfpt.f -- translated by f2c (version 19971204).
     You must link the resulting object file with the libraries:
  	-lf2c -lm   (in that order)
  */

/* SUBROUTINE LFPT (N,M,THETA,CP,PB) */

/* DIMENSION OF */
/* ARGUMENTS */
/*                        CP((N/2)+1) */

/* PURPOSE                ROUTINE LFPT USES COEFFICIENTS COMPUTED BY */
/*                        ROUTINE ALFK TO COMPUTE THE SINGLE PRECISION */
/*                        NORMALIZED ASSOCIATED LEGENDRE FUNCTION PBAR(N, */
/*                        M,THETA) AT COLATITUDE THETA. */

/* USAGE                  CALL LFPT(N,M,THETA,CP,PB) */

/* ARGUMENTS */

/* ON INPUT               N */
/*                          NONNEGATIVE INTEGER SPECIFYING THE DEGREE OF */
/*                          PBAR(N,M,THETA) */
/*                        M */
/*                          IS THE ORDER OF PBAR(N,M,THETA). M CAN BE */
/*                          ANY INTEGER HOWEVER PBAR(N,M,THETA) = 0 */
/*                          IF ABS(M) IS GREATER THAN N AND */
/*                          PBAR(N,M,THETA) = (-1)**M*PBAR(N,-M,THETA) */
/*                          FOR NEGATIVE M. */

/*                        THETA */
/*                          SINGLE PRECISION COLATITUDE IN RADIANS */

/*                        CP */
/*                          SINGLE PRECISION ARRAY OF LENGTH (N/2)+1 */
/*                          CONTAINING COEFFICIENTS COMPUTED BY ROUTINE */
/*                          ALFK */

/* ON OUTPUT              PB */
/*                          SINGLE PRECISION VARIABLE CONTAINING */
/*                          PBAR(N,M,THETA) */

/* SPECIAL CONDITIONS     CALLS TO ROUTINE LFPT MUST BE PRECEDED BY AN */
/*                        APPROPRIATE CALL TO ROUTINE ALFK. */

/* PRECISION              SINGLE */

/* ALGORITHM              THE TRIGONOMETRIC SERIES FORMULA USED BY */
/*                        ROUTINE LFPT TO CALCULATE PBAR(N,M,TH) AT */
/*                        COLATITUDE TH DEPENDS ON M AND N AS FOLLOWS: */

/*                           1) FOR N EVEN AND M EVEN, THE FORMULA IS */
/*                              .5*CP(1) PLUS THE SUM FROM K=1 TO K=N/2 */
/*                              OF CP(K)*COS(2*K*TH) */
/*                           2) FOR N EVEN AND M ODD. THE FORMULA IS */
/*                              THE SUM FROM K=1 TO K=N/2 OF */
/*                              CP(K)*SIN(2*K*TH) */
/*                           3) FOR N ODD AND M EVEN, THE FORMULA IS */
/*                              THE SUM FROM K=1 TO K=(N+1)/2 OF */
/*                              CP(K)*COS((2*K-1)*TH) */
/*                           4) FOR N ODD AND M ODD, THE FORMULA IS */
/*                              THE SUM FROM K=1 TO K=(N+1)/2 OF */
/*                              CP(K)*SIN((2*K-1)*TH) */

/* ACCURACY               COMPARISON BETWEEN ROUTINES LFPT AND DOUBLE */
/*                        PRECISION DLFPT ON THE CRAY1 INDICATES GREATER */
/*                        ACCURACY FOR GREATER VALUES ON INPUT PARAMETER */
/*                        N.  AGREEMENT TO 13 PLACES WAS OBTAINED FOR */
/*                        N=10 AND TO 12 PLACES FOR N=100. */

/* TIMING                 TIME PER CALL TO ROUTINE LFPT IS DEPENDENT ON */
/*                        THE INPUT PARAMETER N. */

  public static final double lfpt(int n, int m, double theta, double[] cp) {
    /* System generated locals */
    int i__1;

    /* Local variables */
    int mmod, nmod, k, ma;
    double ct, st;
    int kp1, np1;
    double cdt;
    int kdo;
    double cth, sdt, sum;


    double pb = 0.0;
    ma = Math.abs(m);
    if (ma > n)
      return pb;
    if (n <= 0 && ma <= 0) {
      pb = Math.sqrt(.5);
    } else {
      np1 = n + 1;
      nmod = n % 2;
      mmod = ma % 2;
      if (nmod <= 0) {
        if (mmod <= 0) {
          kdo = n / 2 + 1;
          cdt = Math.cos(theta + theta);
          sdt = Math.sin(theta + theta);
          ct = 1.0;
          st = 0.0;
          sum = cp[1] * .5;
          i__1 = kdo;
          for (kp1 = 2; kp1 <= i__1; ++kp1) {
            cth = cdt * ct - sdt * st;
            st = sdt * ct + cdt * st;
            ct = cth;
            sum += cp[kp1] * ct;
          }
          pb = sum;
        } else {
          kdo = n / 2;
          cdt = Math.cos(theta + theta);
          sdt = Math.sin(theta + theta);
          ct = 1.0;
          st = 0.0;
          sum = 0.0;
          i__1 = kdo;
          for (k = 1; k <= i__1; ++k) {
            cth = cdt * ct - sdt * st;
            st = sdt * ct + cdt * st;
            ct = cth;
            sum += cp[k] * st;
          }
          pb = sum;
        }
      } else {
        kdo = (n + 1) / 2;
        if (mmod <= 0) {
          cdt = Math.cos(theta + theta);
          sdt = Math.sin(theta + theta);
          ct = Math.cos(theta);
          st = -Math.sin(theta);
          sum = 0.0;
          i__1 = kdo;
          for (k = 1; k <= i__1; ++k) {
            cth = cdt * ct - sdt * st;
            st = sdt * ct + cdt * st;
            ct = cth;
            sum += cp[k] * ct;
          }
          pb = sum;
        } else {
          cdt = Math.cos(theta + theta);
          sdt = Math.sin(theta + theta);
          ct = Math.cos(theta);
          st = -Math.sin(theta);
          sum = 0.0;
          i__1 = kdo;
          for (k = 1; k <= i__1; ++k) {
            cth = cdt * ct - sdt * st;
            st = sdt * ct + cdt * st;
            ct = cth;
            sum += cp[k] * st;
          }
          pb = sum;
        }
      }
    }
    return pb;
  } /* lfpt */

//  static double c_b12 = 2.0;

/* alfk.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

/* SUBROUTINE ALFK (N,M,CP) */

/* DIMENSION OF           REAL CP(N/2 + MOD(N,2)) */
/* ARGUMENTS */

/* PURPOSE                ROUTINE ALFK COMPUTES SINGLE PRECISION FOURIER */
/*                        COEFFICIENTS IN THE TRIGONOMETRIC SERIES */
/*                        REPRESENTATION OF THE NORMALIZED ASSOCIATED */
/*                        LEGENDRE FUNCTION PBAR(N,M,THETA) FOR USE BY */
/*                        ROUTINES LFP AND LFPT IN CALCULATING SINGLE */
/*                        PRECISION PBAR(N,M,THETA). */

/*                        FIRST DEFINE THE NORMALIZED ASSOCIATED */
/*                        LEGENDRE FUNCTIONS */

/*                        PBAR(M,N,THETA) = SQRT((2*N+1)*FACTORIAL(N-M) */
/*                        /(2*FACTORIAL(N+M)))*SIN(THETA)**M/(2**N* */
/*                        FACTORIAL(N)) TIMES THE (N+M)TH DERIVATIVE OF */
/*                        (X**2-1)**N WITH RESPECT TO X=COS(THETA) */

/*                        WHERE THETA IS COLATITUDE. */

/*                        THEN SUBROUTINE ALFK COMPUTES THE COEFFICIENTS */
/*                        CP(K) IN THE FOLLOWING TRIGONOMETRIC */
/*                        EXPANSION OF PBAR(M,N,THETA). */

/*                        1) FOR N EVEN AND M EVEN, PBAR(M,N,THETA) = */
/*                           .5*CP(1) PLUS THE SUM FROM K=1 TO K=N/2 */
/*                           OF CP(K)*COS(2*K*TH) */

/*                        2) FOR N EVEN AND M ODD, PBAR(M,N,THETA) = */
/*                           THE SUM FROM K=1 TO K=N/2 OF */
/*                           CP(K)*SIN(2*K*TH) */

/*                        3) FOR N ODD AND M EVEN, PBAR(M,N,THETA) = */
/*                           THE SUM FROM K=1 TO K=(N+1)/2 OF */
/*                           CP(K)*COS((2*K-1)*TH) */

/*                        4) FOR N ODD AND M ODD,  PBAR(M,N,THETA) = */
/*                           THE SUM FROM K=1 TO K=(N+1)/2 OF */
/*                           CP(K)*SIN((2*K-1)*TH) */


/* USAGE                  CALL ALFK(N,M,CP) */

/* ARGUMENTS */

/* ON INPUT               N */
/*                          NONNEGATIVE INTEGER SPECIFYING THE DEGREE OF */
/*                          PBAR(N,M,THETA) */

/*                        M */
/*                          IS THE ORDER OF PBAR(N,M,THETA). M CAN BE */
/*                          ANY INTEGER HOWEVER CP IS COMPUTED SUCH THAT */
/*                          PBAR(N,M,THETA) = 0 IF ABS(M) IS GREATER */
/*                          THAN N AND PBAR(N,M,THETA) = (-1)**M* */
/*                          PBAR(N,-M,THETA) FOR NEGATIVE M. */

/* ON OUTPUT              CP */
/*                          SINGLE PRECISION ARRAY OF LENGTH (N/2)+1 */
/*                          WHICH CONTAINS THE FOURIER COEFFICIENTS IN */
/*                          THE TRIGONOMETRIC SERIES REPRESENTATION OF */
/*                          PBAR(N,M,THETA) */


/* SPECIAL CONDITIONS     NONE */

/* PRECISION              SINGLE */

/* ALGORITHM              THE HIGHEST ORDER COEFFICIENT IS DETERMINED IN */
/*                        CLOSED FORM AND THE REMAINIG COEFFICIENTS ARE */
/*                        DETERMINED AS THE SOLUTION OF A BACKWARD */
/*                        RECURRENCE RELATION. */

/* ACCURACY               COMPARISON BETWEEN ROUTINES ALFK AND DOUBLE */
/*                        PRECISION DALFK ON THE CRAY1 INDICATES */
/*                        GREATER ACCURACY FOR SMALLER VALUES */
/*                        OF INPUT PARAMETER N.  AGREEMENT TO 14 */
/*                        PLACES WAS OBTAINED FOR N=10 AND TO 13 */
/*                        PLACES FOR N=100. */

  public static final double[] alfk(int n, int m) {
    /* System generated locals */
    int i__1;

    /* Local variables */
    double fden, fnmh, fnum, fnnp1;
    int nmms2, i__, l;
    double a1, b1, fnmsq, c1, t1, t2;
    int ma;
    double fk, cp2, pm1;

    double[] cp = new double[n / 2 + 2];

    cp[1] = 0.0;
    ma = Math.abs(m);
    if (ma > n) {
      return cp;
    }
    if ((i__1 = n - 1) < 0) {
      cp[1] = Math.sqrt(2.0);
      return cp;
    } else if (i__1 == 0) {
      if (ma != 0) {
        cp[1] = Math.sqrt(.75);
        if (m == -1) {
          cp[1] = -cp[1];
        }
        return cp;
      }
      cp[1] = Math.sqrt(1.5);
      return cp;
    }
    if ((n + ma) % 2 != 0) {
      nmms2 = (n - ma - 1) / 2;
      fnum = (double) (n + ma + 2);
      fnmh = (double) (n - ma + 2);
      pm1 = -1.0;
    } else {
      nmms2 = (n - ma) / 2;
      fnum = (double) (n + ma + 1);
      fnmh = (double) (n - ma + 1);
      pm1 = 1.0;
    }
    t1 = 1.0;
    t2 = 1.0;
    if (nmms2 >= 1) {
      fden = 2.0;
      i__1 = nmms2;
      for (i__ = 1; i__ <= i__1; ++i__) {
        t1 = fnum * t1 / fden;
        fnum += 2.0;
        fden += 2.0;
      }
    }
    if (ma != 0) {
      i__1 = ma;
      for (i__ = 1; i__ <= i__1; ++i__) {
        t2 = fnmh * t2 / (fnmh + pm1);
        fnmh += 2.0;
      }
    }
    if (ma / 2 % 2 != 0) {
      t1 = -t1;
    }
    i__1 = n - 1;
    cp2 = t1 * Math.sqrt((n + .5) * t2) / MoreMath.pow(2.0, i__1);
    fnnp1 = (double) (n * (n + 1));
    fnmsq = fnnp1 - ma * 2.0 * ma;
    l = (n + 1) / 2;
    if (n % 2 == 0 && ma % 2 == 0) {
      ++l;
    }
    cp[l] = cp2;
    if (m < 0 && ma % 2 != 0) {
      cp[l] = -cp[l];
    }
    if (l <= 1) {
      return cp;
    }
    fk = (double) (n);
    a1 = (fk - 2.0) * (fk - 1.0) - fnnp1;
    b1 = (fk * fk - fnmsq) * 2.0;
    cp[l - 1] = b1 * cp[l] / a1;
    while (l > 1) {
      --l;
      fk += -2.0;
      a1 = (fk - 2.0) * (fk - 1.0) - fnnp1;
      b1 = (fk * fk - fnmsq) * -2.0;
      c1 = (fk + 1.0) * (fk + 2.0) - fnnp1;
      cp[l - 1] = -(b1 * cp[l] + c1 * cp[l + 1]) / a1;
    }
    return cp;
  } /* alfk */

  public static final void talfpk() {
    /* Local variables */

/* Reference: "Tables of Normalized Associated Legendre Polynomials", */
/*            by S.L. Belousov, Pergamon Press 1962 */


    double theta = 50.0 * Constants.DEGTOPI;
    double pb = lfpt(42, 15, theta, alfk(42, 15));

    System.out.println(" Belousov's tables show PBAR(42,15,50.)= -0.766020");
    System.out.println(" Routine LFPT in ALFPACK returned value =" + Double.toString(pb));
  } /* talfpk */

}
