/* Program AGTORGAC.FOR
                                        S.M. and L.L., Berkeley, July 97

*************************************************************************
Determination (by linear interpolation) of the PF-values for a complete,
regular Y-grid using PF-values measured at an arbitrary (not necessaryly
regular or complete) grid. In nonmeasured regions or in case of senseless
 interpolation the asked PF-values are set to be equal -666.66 .
*************************************************************************

 IZPOL pole figures will be considered
 Pole figure : number -> NPOL, name -> HKL
 A pole figure is given at MEPSUM(NPOL) points YAGj at an arbitray grid
("AG") by the azimuth FiAG(j),the polar angle ThetaAG(j) and value PFAG(j).


 Present program variant :                        MEPSUM <= max5001    !!!

 RESOLUTION determines the step of the regular Y-grid ("RG")
 (DeltaFI = DeltaTheta = RESOLUTION in degrees)

                                90/RESOLUTION must be an int    !!!
 Present program variant :           RESOLUTION >= 1. is assumed    !!!

 Typical case : RESOLUTION = 5. degrees

 Two "measured" points YAGj are considered as identical if their angular
 distance is lower than 1/6 of the RESOLUTION .

For each pole figure all points of the RG (cell centres, complete PF) are
 considered. I.e. for a YRG-point the angular distances to all YAGj are
 determined. Then the YAGj are ordered by their distances.
 If the minimum distance will be greather than DISTCTR (in degrees) the
 PF-value at the corresponding YRG cannot be estimated ("unmeasured") ->
 PFRG(YRG) = -666.

     DISTCTR should not be greater than about 2 times RESOLUTION   !!!

IF at least two YAG-points lie inside the DISTCTR-circle around the asked
 YRG-point they are projected onto the equal area plane.
 IF the asked YRG-point lies inside a threeangle of YAG-points (or on a
 line of two) the PFRG(YRG)-value is determined by linear interpolation.
 In case of ambiguity the nearest neighbours of YAG to YRG are taken.

 By FITURN (in degrees) the original PFAG input data can be rotated
 -> FInewAG = FIoriginalAG + FITURN

*************************************************************************

 Structure of the INPUT data file ->  PFAG.DAT                  unit 2

     READ/WRITE IZPOL,IGB,IGBS
     DO 2 NPOL=1,IZPOL
       READ/WRITE HKL (A12)
 L      READ/WRITE MEPSUM      (,IHEX)
 L      READ/WRITE             (TETMAX,NSMMAX,DTET,NFIZF)
       DO 1 j=1,MEPSUM
         READ/WRITE FiAG(j),ThetaAG(j),PFAG(j)
 1     CONTINUE
 2   CONTINUE
*************************************************************************

 Structure of the INPUT control file ->  PFAG.CTR                unit 3

 RESOLUTION, DISTCTR,FITURN
*************************************************************************

 OUTPUT -> direct access file  PFRG.ACS unit 111  cf. SUBROUTINE OUTACS
 OUTPUT -> Berkeley plotfile   PFRG.xlp unit  44  cf. SUBROUTINE PRIUCIRC
 OUTPUT -> readable listing file  PFRG.LST                  unit IOUT=6 */

/*
 * @(#)Agtorgl.java created 16/11/1998 Berkeley
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

import java.lang.*;

import it.unitn.ing.rista.util.*;

import java.io.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;

/**
 * Original is Agtorgl.f -- translated by f2c (version 19941113).
 * You must link the resulting object file with the libraries:
 * -lf2c -lm   (in that order).
 * Translated in Java by L.L.
 *
 *
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Agtorgl {

  int cpol_izpol, cpol_npol;
  double[] crg_xrg = null, crg_yrg = null, crg_firgradm, crg_ctrgm, crg_strgm;
  double cpif_pif, cpif_pifh, cpif_fiturn;
  int cpriu_nseca, cpriu_nsece;
  double cpriu_secwist;
  int cpriu_ntetaa, cpriu_ntetae, cpriu_nfia, cpriu_nfie;
  int[] milh, milk, mill;
  double[] cag_xag, cag_yag, cpfag_pfag, cpfag_wgt, cfitetag_fiagrad, cfitetag_ctetag, cfitetag_stetag;
  double[] cpf55_pf55, cpfrg_pfrg = null;
  double resolution = 5.0;
  int lastdim = 0;

  int ntetangle = 0, nfiangle = 0, npfrgdim;

  double gap = 0.0;
  double detmin = .005;

  double hitcontroldist;

/* Table of constant values */

  String title = null;

  TriangularInterpolation interpolation = null;

  public Agtorgl(TriangularInterpolation interpolation) {
    this.interpolation = interpolation;

    resolution = interpolation.getResolution();
    nfiangle = (int) (360. / resolution + 1.00001);
    ntetangle = (int) (90. / resolution + 1.00001);
    npfrgdim = nfiangle * ntetangle;
    lastdim = npfrgdim;

    cag_xag = new double[lastdim];
    cag_yag = new double[lastdim];
    cpfag_pfag = new double[lastdim];
    cpfag_wgt = new double[lastdim];
    cfitetag_fiagrad = new double[lastdim];
    cfitetag_ctetag = new double[lastdim];
    cfitetag_stetag = new double[lastdim];
    cpf55_pf55 = new double[npfrgdim]	/* was [73][19] */;
    crg_firgradm = new double[nfiangle];
    crg_ctrgm = new double[ntetangle];
    crg_strgm = new double[ntetangle];

    hitcontroldist = resolution / 6;
  }

  public double[][] getInterpolation() {
/* Local variables */
    int nadd;
    double firg;
    int ntet;
    double cdistctr;
    int ntet1;
    double tetrg;
    int nw;
    int mepsum, nh1;
    double projection;
    int nfi;
    double firgrad;
    double distctr;

/* lu        CHARACTER*40 ITITLE,FNAME */
/*                                                for PRIUCIRC */
/*                             5x5 degree OUTPUT routine */
/*     CHARACTER ITITLE*40 */
/*     CHARACTER*5 NAMET,NAMEFI,NAMESE */
/* lu ,IOUT */
/* lu      COMMON /TIT/ITITLE */

/*    s_copy(cpriu_1.namese, "Phi ", 5L, 4L);
    s_copy(cpriu_1.namet, "THETA", 5L, 5L);
    s_copy(cpriu_1.namefi, "PHI  ", 5L, 5L);*/
    cpriu_nseca = 1;
    cpriu_nsece = 1;
    cpriu_secwist = resolution;
    cpriu_ntetaa = 1;
    cpriu_ntetae = ntetangle;
    cpriu_nfia = 1;
    cpriu_nfie = nfiangle;
/*                                          Opening of the OUTPUT FILEs */
/* lu        IULST=6 */
/* lu        IUPL=44 */
/* lu        OPEN(44,FILE='pfrg.xlp') */
/* lu        IOUT=IULST */
/* lu        OPEN (22,FILE='pfinp.acs',access='direct',recl=5548) */
/*                                  dummy hkl */

/* lu        OPEN(6,FILE='pfrg.lst') */
/* lu        OPEN(3,FILE='pfag.ctr') */
/* lux        READ(3,*)RESOLUTION,DISTCTR,FITURN */
    distctr = interpolation.getDistControlD();
    if (distctr < 0)
      distctr = 90.0;

    cpif_fiturn = 0.0;
/* lu        CLOSE(3) */
/* lu        WRITE(IOUT,4)RESOLUTION,DISTCTR,FITURN */
/* lu 4    FORMAT(' RESOLUTION = ',F7.3,' DISTCTR = ',F7.3, */
/* lu     *       ' FITURN = ',F7.3) */

/*                                    Construction of the regular grid */

    cpif_pif = Constants.PI / 180.;
    cpif_pifh = cpif_pif / 2.;

    crg_xrg = new double[npfrgdim];
    crg_yrg = new double[npfrgdim];
    cpfrg_pfrg = new double[npfrgdim];

    for (ntet = 1; ntet <= ntetangle; ++ntet) {
      ntet1 = ntet - 1;
      nh1 = nfiangle * ntet1;
      tetrg = resolution * ntet1;
      projection = Math.sin(tetrg * cpif_pifh);
      crg_ctrgm[ntet - 1] = Math.cos(tetrg * cpif_pif);
      crg_strgm[ntet - 1] = Math.sin(tetrg * cpif_pif);
      for (nfi = 1; nfi <= nfiangle; ++nfi) {
        nadd = nh1 + nfi;
        firg = resolution * (nfi - 1);
        firgrad = firg * cpif_pif;
        if (ntet == 1) {
          crg_firgradm[nfi - 1] = firgrad;
        }
        crg_xrg[nadd - 1] = projection * Math.cos(firgrad);
        crg_yrg[nadd - 1] = projection * Math.sin(firgrad);
      }
    }
    cdistctr = Math.cos(distctr * cpif_pif);
/* lu        OPEN(2,FILE='hints.hex') */
/* lu      ITITLE = 'title' */
/* lu        WRITE(IOUT,40)ITITLE */
/* lu 40   FORMAT(2X,A40) */
/* lu        READ(2,*)IZPOL,IGB,IGBS */
    cpol_izpol = interpolation.getPoleFigureNumber();
/* lu        WRITE(IOUT,41)IZPOL,IGB,IGBS */
/* lu 41   FORMAT(' IZPOL = ',I5,' IGB = ',I5,' IGBS = ',I5) */

/*                                                     Pole figure loop */

    Phase thephase = (Phase) ((XRDcat) interpolation.getParent()).getParent();
//    FilePar aparFile = (FilePar) thephase.getFilePar();
    BufferedWriter PFwriter = Misc.getWriter(((FilePar) thephase.getFilePar()).getDirectory() +
            thephase.toXRDcatString() + ".xpe");

    title = new String(thephase.toXRDcatString() + ": interpolated experimental pole figure, ");

    double[][] poleIntensity = new double[cpol_izpol][npfrgdim];
    milh = new int[cpol_izpol];
    milk = new int[cpol_izpol];
    mill = new int[cpol_izpol];

    ProgressFrame prF = null;
    if (!Constants.textonly && Constants.showProgressFrame)
      try {
          prF = new ProgressFrame(cpol_izpol);
      } catch (NullPointerException npe) {
        System.out.println("Not able to create frame, MacOSX display sleep bug?");
      }
    printf("Pole figure interpolation for phase: " + thephase.toXRDcatString() + "       ", prF);
    for (cpol_npol = 1; cpol_npol <= cpol_izpol; ++cpol_npol) {
      milh[cpol_npol - 1] = interpolation.getH(cpol_npol - 1);
      milk[cpol_npol - 1] = interpolation.getK(cpol_npol - 1);
      mill[cpol_npol - 1] = interpolation.getL(cpol_npol - 1);
      mepsum = pfagread_(distctr, cpol_npol, interpolation);
      agtorg_(mepsum, cdistctr);
      pfrgto55_();
      priucirc_(PFwriter, thephase, cpol_npol);

      for (ntet = 1; ntet <= ntetangle; ++ntet)
        for (nfi = 1; nfi <= nfiangle; ++nfi) {
          poleIntensity[cpol_npol - 1][(ntet - 1) * nfiangle + nfi - 1] =
                  cpf55_pf55[(ntet - 1) * nfiangle + nfi - 1];

        }
      if (prF != null)
        prF.increaseProgressBarValue();
      printf("Interpoled pole n: " + Integer.toString(cpol_npol) + ", " +
              Integer.toString(milh[cpol_npol - 1]) + "," +
              Integer.toString(milk[cpol_npol - 1]) + "," +
              Integer.toString(mill[cpol_npol - 1]), prF);
    }

    if (prF != null) {
      prF.setVisible(false);
      prF.dispose();
    }
//		prF = null;

    try {
      PFwriter.write(Constants.lineSeparator);
      PFwriter.flush();
      PFwriter.close();
    } catch (IOException io) {
    }
    return poleIntensity;
  } /* agtorgl */

  public void printf(String message, ProgressFrame prF) {
    if (prF != null)
      prF.setProgressText(message);
    else
      System.out.println(message);
  }


  double maxPolar = 0.0;
  double minPolar = 90.0;

  int pfagread_(double distctr, int pole_number, TriangularInterpolation interpolation) {

/* Local variables */
    double fiag;
    int isum;
    double fiagnewr = 0;
    int j = 0;
    double tetag;
    int jh;
    double projection, fiagnew;

/* lux        READ(2,40)HKL */
/* lu 40   FORMAT(A12) */
/* lux        READ(2,*)MEPSUM */
    int mepsum = interpolation.getPointNumber(pole_number - 1);
/* lu        READ(2,*) */
/* lu        WRITE(IOUT,41)HKL,MEPSUM */
/* lu 41   FORMAT(' HKL = ',A12,' MEPSUM = ',I6) */

    int newdim = mepsum * 12 / 10;
    if (newdim > lastdim) {
      lastdim = newdim;
      cag_xag = new double[lastdim];
      cag_yag = new double[lastdim];
      cpfag_pfag = new double[lastdim];
      cpfag_wgt = new double[lastdim];
      cfitetag_fiagrad = new double[lastdim];
      cfitetag_ctetag = new double[lastdim];
      cfitetag_stetag = new double[lastdim];
    }

    isum = 0;

    maxPolar = 0.0;
    minPolar = 90.0;
    for (jh = 1; jh <= mepsum; ++jh) {
      j = jh + isum;
/* lux        READ(2,*) FIAG,TETAG,PFAG(j),wgt(j) */
      double angles[] = interpolation.getTextureAngles(pole_number - 1, jh - 1);
      fiag = angles[1];
      tetag = angles[0];
      cpfag_pfag[j - 1] = (double) interpolation.getPoleIntensity(pole_number - 1, jh - 1);
      cpfag_wgt[j - 1] = (double) interpolation.getWeight(pole_number - 1, jh - 1);

/*    	System.out.println("fi, teta");
    	System.out.println(fiag);
    	System.out.println(tetag);
    	System.out.println(cpfag_pfag[j - 1]);*/

      fiagnew = fiag + cpif_fiturn;
      while (fiagnew >= 360.)
        fiagnew -= 360.;
      while (fiagnew < 0.)
        fiagnew += 360.;

      projection = Math.sin(tetag * cpif_pifh);
/*                               gap 0.3 degree !!!*/
      if (tetag > 90.) {
        ++isum;
        ++j;
        cpfag_pfag[j - 1] = cpfag_pfag[j - 2];
        projection = Math.sin((180. - tetag) * cpif_pifh);
        fiagnew += 180.;
        while (fiagnew >= 360.) {
          fiagnew -= 360.;
        }
      }
      fiagnewr = fiagnew * cpif_pif;
      cag_xag[j - 1] = (double) (projection * Math.cos(fiagnewr));
      cag_yag[j - 1] = (double) (projection * Math.sin(fiagnewr));
      cfitetag_ctetag[j - 1] = (double) Math.cos(tetag * cpif_pif);
      cfitetag_stetag[j - 1] = (double) Math.sin(tetag * cpif_pif);
      cfitetag_fiagrad[j - 1] = (double) fiagnewr;

      if (tetag > maxPolar)
        maxPolar = tetag;
      if (tetag < minPolar)
        minPolar = tetag;
    }
    if (minPolar <= resolution / 2)
      minPolar = -1;
    else
      minPolar -= distctr / 10.0;
    maxPolar += distctr / 10.0;

    mepsum += isum;
/* lu        WRITE(IOUT,42)HKL,MEPSUM */
/* lu 42   FORMAT(' HKL = ',A12,' MEPSUME = ',I6) */
    return mepsum;
  } /* pfagread_ */


  void agtorg_(int mepsum, double cdistctr) {

/* Local variables */
    int nadd, ifi20;
    double det00, chit;
    int[] iord = new int[lastdim];
    double ctrg;
    int ihit;
    double hmax, hitr;
    int ntet, isum;
    double strg;
    int iout, neighboursum, j, n;
    double x, y;
    int ischl;
    double fimin = 0, fimax = 0;
    int[] iordj = new int[lastdim];
    double cdistance, pfint;
    int j1 = 0, j2 = 0, n1, neighbour, n2;
    double r1, r2;
    int j3;
    double x0 = 0.0, y0 = 0.0, x2 = 0.0, y2 = 0.0, x1 = 0.0, y1 = 0.0,
            z1 = 0.0, z2 = 0.0, x3 = 0.0, y3 = 0.0, z3 = 0.0, fi2rel,
            fi3rel, shhit2, ab, flimit1, flimit2;
    int neighboursume = 0, nn;
    double hx, hy;
    int[] iordjj = new int[lastdim];
    double fi1, fi2, fi3;
    int nh1;
    double r1h, rh2, r2h, r3h;
    double[] r2m = new double[lastdim];
    double hx1, hy1;
    int nfi;
    double det, hit;
    int[] nnm = new int[lastdim];
    int nnn;
    double firgrad, sum, det0, fi2p;
    boolean breakL2;


    hit = hitcontroldist;
    hitr = hit * cpif_pif;
    chit = Math.cos(hitr);
/* Computing 2nd power */
    double d__1 = Math.sin(hit * cpif_pifh);
    shhit2 = d__1 * d__1;
    for (ntet = 1; ntet <= ntetangle; ++ntet) {
/* lu        WRITE(0,400)NPOL,IZPOL,NTET,NTETangle */
/* lu 400    FORMAT(1H+,' NPOL->',I4,'(',I4,') NTET-> ',I3,'(',I3,')') */
      ctrg = crg_ctrgm[ntet - 1];
      strg = crg_strgm[ntet - 1];
      nh1 = nfiangle * (ntet - 1);
      for (nfi = 1; nfi <= nfiangle; ++nfi) {
        if (ntet == ntetangle) {
          flimit1 = 0.0;
          flimit2 = 360.0;
        } else {
          flimit1 = 180.0;
          flimit2 = 180.0;
        }

        breakL2 = false;
        firgrad = crg_firgradm[nfi - 1];
        nadd = nh1 + nfi;
/*                          Loop over all YAG */

/*     Looking for neighbours of YRG in the radius DISTCTR */

        neighboursum = 0;
        cpfrg_pfrg[nadd - 1] = -1.0;
        for (j = 1; j <= mepsum; ++j) {
          if (cpfag_wgt[j - 1] > 0.) {
            cdistance = ctrg * cfitetag_ctetag[j - 1] + strg *
                    cfitetag_stetag[j - 1] * Math.cos(firgrad - cfitetag_fiagrad[j - 1]);
            if (cdistance >= chit) {
/*                                        HITcase */
              cpfrg_pfrg[nadd - 1] = cpfag_pfag[j - 1];

              neighboursum = -1;
            } else if (cdistance >= cdistctr) {
/*                           YAG-point within DISTCTR */
              ++neighboursum;
              iord[neighboursum - 1] = j;
            }
          }
          if (neighboursum == -1) {
            neighboursum = 1;
            breakL2 = true;
            j = mepsum + 1;
          }
        }
        if (neighboursum >= 2) {
/*     GOTO 2 ->    not sufficient neighbours for interpolation
                    or YRG outside the measured region

Ordering of the neighbours within DISTCTR in the X/Y equal are a plane
with increasing radial distances */
          x0 = crg_xrg[nadd - 1];
          y0 = crg_yrg[nadd - 1];
          for (neighbour = 1; neighbour <= neighboursum; ++neighbour) {
            j = iord[neighbour - 1];
            x = cag_xag[j - 1] - x0;
            y = cag_yag[j - 1] - y0;
            r2m[neighbour - 1] = x * x + y * y;
            nnm[neighbour - 1] = neighbour;
          }
          for (neighbour = 1; neighbour < neighboursum; ++neighbour) {
            for (n = neighbour + 1; n <= neighboursum; ++n) {
              n1 = nnm[neighbour - 1];
              n2 = nnm[n - 1];
              if (r2m[n1 - 1] > r2m[n2 - 1]) {
                nnm[neighbour - 1] = n2;
                nnm[n - 1] = n1;
              }
            }
          }
          for (nn = 1; nn <= neighboursum; ++nn) {
            neighbour = nnm[nn - 1];
            iordj[nn - 1] = iord[neighbour - 1];
          }
/*                     Exclusion of identical YAG-points
    IORDJ(NN) contains a negative number for the unnecessary YAG-points
    PFAG averaging for the first one of identical points */

          neighboursume = neighboursum;
          for (neighbour = 1; neighbour < neighboursum; ++neighbour) {
            j = iordj[neighbour - 1];
            if (j >= 0) {
              hx = cag_xag[j - 1];
              hy = cag_yag[j - 1];
              ihit = 1;
              sum = cpfag_pfag[j - 1];
              for (n = neighbour + 1; n <= neighboursum; ++n) {
                if (iordj[n - 1] >= 0) {
                  j1 = iordj[n - 1];
                  hx1 = cag_xag[j1 - 1];
                  hy1 = cag_yag[j1 - 1];
/* Computing 2nd power */
                  d__1 = hx - hx1;
/* Computing 2nd power */
                  double d__2 = hy - hy1;
                  rh2 = d__1 * d__1 + d__2 * d__2;
                  if (rh2 <= shhit2) {
/*                            HIT case */
                    --neighboursume;
                    iordj[n - 1] = -iordj[n - 1];
                    ++ihit;
                    sum += cpfag_pfag[j1 - 1];
                  }
                }
              }
              if (ihit != 1) {
                cpfag_pfag[j - 1] = (double) sum / ihit;
              }
            }
          }
          if (neighboursume == 2) {
/*                case NeighboursumE = 2
                  Does the corresponding line go through (0,0) ? */
            ischl = 0;
            for (nn = 1; nn <= neighboursum; ++nn) {
              j = iordj[nn - 1];
              if (j >= 0) {
                ++ischl;
                j2 = j;
                x2 = cag_xag[j - 1] - x0;
                y2 = cag_yag[j - 1] - x0;
                if (ischl != 2) {
                  j1 = j2;
                  x1 = x2;
                  y1 = y2;
                }
              }
            }
            hmax = Math.abs(x1);
            ab = Math.abs(x2);
            if (ab > hmax) {
              hmax = ab;
            }
            ab = Math.abs(y1);
            if (ab > hmax) {
              hmax = ab;
            }
            ab = Math.abs(y2);
            if (ab > hmax) {
              hmax = ab;
            }
            det = Math.abs(x1 * y2 - x2 * y1) / (hmax * hmax);

/*                            case ->   the line hits (0,0)
Have we the case *----o--* (linear interpolation) or *--*---o (GOTO 2) ?
The last case is excluded in order always to guarantee positive PF values*/

            if (det <= detmin && (x1 * x2 <= 0. && y1 * y2 <= 0.)) {
/*                               linear interpolation */
              r1 = Math.sqrt(x1 * x1 + y1 * y1);
              r2 = Math.sqrt(x2 * x2 + y2 * y2);
              cpfrg_pfrg[nadd - 1] = (r2 * cpfag_pfag[j1 - 1] + r1 *
                      cpfag_pfag[j2 - 1]) / (r1 + r2);
            }
          } else if (neighboursume >= 3) {
/*                         case  NeighboursumE >= 3 */
            isum = 0;
            for (nn = 1; nn <= neighboursum; ++nn) {
              j = iordj[nn - 1];
              if (j >= 0) {
                ++isum;
                iordjj[isum - 1] = j;
              }
            }
/*lu   IF(NeighboursumE.NE.ISUM)WRITE(IOUT,'(A)')'  NeighboursumE !!' */
            if (neighboursume != isum) {
              System.out.println("neighboursume != isum");
              return;
            }
/*                                                        j1 */
            j1 = iordjj[0];
            x1 = cag_xag[j1 - 1] - x0;
            y1 = cag_yag[j1 - 1] - y0;
            z1 = cpfag_pfag[j1 - 1];
            r1h = Math.sqrt(x1 * x1 + y1 * y1);
            fi1 = Math.acos(x1 / r1h) / cpif_pif;
            if (y1 < 0.) {
              fi1 = 360. - fi1;
            }
            for (nn = 2; nn < neighboursume; ++nn) {
/*                                                   j2 */
              j2 = iordjj[nn - 1];
              x2 = cag_xag[j2 - 1] - x0;
              y2 = cag_yag[j2 - 1] - y0;
              z2 = cpfag_pfag[j2 - 1];
              r2h = Math.sqrt(x2 * x2 + y2 * y2);
              fi2 = Math.acos(x2 / r2h) / cpif_pif;
              if (y2 < 0.) {
                fi2 = 360. - fi2;
              }
              fi2rel = fi2 - fi1;
              if (fi2rel < 0.) {
                fi2rel += 360.;
              }
/*                                           gap 0.6 degree !!!*/
              if (fi2rel == 180.) {
/*               case of a line with   ->   j1 - j2 interpolation*/
                cpfrg_pfrg[nadd - 1] = (r2h * z1 + r1h * z2) / (r1h + r2h);
                breakL2 = true;
                nn = neighboursume;
              } else {
                ifi20 = 0;
/*                                  gap 0.6 degree !!!*/
//								if (fi2rel <= 0. && fi2rel >= 360.) {
//		    					ifi20 = 1;
//								}
                if (ifi20 != 1) {
                  fi2p = fi2rel + 180.;
                  if (fi2p >= 360.) {
                    fi2p += -360.;
                  }
                  fimin = flimit1;
                  fimax = fi2p;
                  if (fimin >= fimax) {
                    fimin = fimax;
                    fimax = flimit2;
                  }
                }
                for (nnn = nn + 1; nnn <= neighboursume; ++nnn) {
/*          	                                         j3 */
                  j3 = iordjj[nnn - 1];
                  x3 = cag_xag[j3 - 1] - x0;
                  y3 = cag_yag[j3 - 1] - y0;
                  r3h = Math.sqrt(x3 * x3 + y3 * y3);
                  fi3 = Math.acos(x3 / r3h) / cpif_pif;
                  if (y3 < 0.) {
                    fi3 = 360. - fi3;
                  }
                  fi3rel = fi3 - fi1;
                  if (fi3rel < 0.) {
                    fi3rel += 360.;
                  }
                  if (fi3rel < 0.) {
                    fi3rel += 360.;
                  }
/*          	                                 gap 0.6 degree !!!*/
                  if (fi3rel >= 180. - gap && fi3rel <= 180 + gap) {
/*          	    case of a line with   ->   j1 - j3 interpolation*/
                    z3 = cpfag_pfag[j3 - 1];
                    cpfrg_pfrg[nadd - 1] = (r3h * z1 + r1h * z3) / (r1h + r3h);
                    if (ntet == ntetangle) {
                      System.out.println("second");
                      System.out.println(cpfrg_pfrg[nadd - 1]);
                    }
                    breakL2 = true;
                    nnn = neighboursume + 1;
                  } else if (ifi20 != 1) {
/*          	                                  gap 0.6 degree !!!*/
                    if (fi3rel <= fimax + gap && fi3rel >= fimin - gap) {
                      z3 = cpfag_pfag[j3 - 1];
                      det0 = x1 * y2 + x2 * y3 + x3 * y1 - y1 * x2 - y2 * x3 - y3 * x1;
/* lu  IF(DE	T0.EQ.0.)WRITE(IOUT,'(A)')'  case threeangle DET0=0 !!' */
                      if (det0 == 0.) {
                        System.out.println("det0 == 0.");
                        return;
                      }
                      det00 = x1 * y2 * z3 + y1 * z2 * x3 + z1 * x2 * y3 - x1 *
                              z2 * y3 - y1 * x2 * z3 - z1 * y2 * x3;
                      pfint = det00 / det0;
/*lu      IF	(PFint.LT.0.)WRITE(IOUT,'(A)')'  case threeangle PFint <0 !'*/
/*        IF	(PFint.LT.0.)STOP */
                      if (pfint < 0.) {
                        pfint = 0.;
                      }
                      cpfrg_pfrg[nadd - 1] = pfint;
                      breakL2 = true;
                      nnn = neighboursume + 1;
                    }
                  }
                  if (breakL2)
                    nnn = neighboursume + 1;
                }
              }
              if (breakL2)
                nn = neighboursume;
            }
          }
/*                    end of the case neighboursumE >= 3 */
        } /* neighboursum >= 2 */
      } /* nfi */
    } /* ntet */
    return;
  } /* agtorg_ */

  double startalpha = 0.0;
  double finalalpha = 90.0;
  double startbeta = 0.0;
  double finalbeta = 360.0;

  void pfrgto55_() {

/* Local variables */
    int nadd, ntet, ischl, nh1, nfi;

    startalpha = 90.;
    finalalpha = 0.;
    for (ntet = 1; ntet <= ntetangle; ++ntet) {
      double tetangl = (ntet - 1) * resolution;
      nh1 = nfiangle * (ntet - 1);
      ischl = 0;
      if (tetangl < minPolar || tetangl > maxPolar) {
        ischl = 1;
      }
      for (nfi = 1; nfi <= nfiangle; ++nfi) {
        nadd = nh1 + nfi;
        if (cpfrg_pfrg[nadd - 1] < 0.) {
          ischl = 1;
        }
        cpf55_pf55[(ntet - 1) * nfiangle + nfi - 1] = cpfrg_pfrg[nadd - 1];
      }
      if (ischl != 0) {
        for (nfi = 1; nfi <= nfiangle; ++nfi) {
          cpf55_pf55[(ntet - 1) * nfiangle + nfi - 1] = -1.0;
        }
      } else {
        startalpha = Math.min(startalpha, tetangl);
        finalalpha = Math.max(finalalpha, tetangl);
      }
    }

  } /* pfrgto55_ */


  void priucirc_(BufferedWriter PFwriter, Phase thephase, int npole) {

/* Local variables */
    int naeh;
    int na, nb, naa, nbe, nae, nbh, ny, nbfak;
    int[] imh = new int[npfrgdim];

/*    OUTPUT OF POLE FIGURE (ARRAY FS) */

    StringBuffer tmp = new StringBuffer(title);
    tmp = tmp.append(" ").append(Integer.toString(milh[npole - 1])).
            append(",").append(Integer.toString(milk[npole - 1])).
            append(",").append(Integer.toString(mill[npole - 1]));
    int bufflength = tmp.length();
    for (int i = 0; i < 79 - bufflength; i++)
      tmp = tmp.append(" ");

    String commentLine = new String(tmp.toString().substring(0, 79) + "#");

    try {
      PFwriter.write(commentLine);
      PFwriter.write(Constants.lineSeparator);
      int lineend = 0;
      int izoveri = interpolation.getIzoveri(npole - 1);
//      System.out.println(izoveri);
      if (izoveri > 1) {
        StringBuffer tmp1 = new StringBuffer();
        tmp1.append(Integer.toString(izoveri) + " ");
        for (int j = 0; j < izoveri; j++) {
          int h = interpolation.getH(npole - 1, j);
          int k = interpolation.getK(npole - 1, j);
          int l = interpolation.getL(npole - 1, j);
          double wgt = interpolation.getWeightSingle(npole - 1, j);
          String temp = new String(" " + Integer.toString(h) + " " + Integer.toString(k) + " " + Integer.toString(l) + " " + Misc.getDoubleStringFormatted(wgt, 1, 3) + " ");
          if (tmp1.length() + temp.length() <= 79)
            tmp1.append(temp);
          else {
            if (lineend < 4) {
              PFwriter.write(tmp1.toString());
              PFwriter.write(Constants.lineSeparator);
              lineend++;
            }
            tmp1 = new StringBuffer();
            tmp1.append(temp);
          }
        }
        if (lineend < 4) {
          PFwriter.write(tmp1.toString());
          PFwriter.write(Constants.lineSeparator);
          lineend++;
        }
        while (lineend < 4) {
          PFwriter.write(Constants.lineSeparator);
          lineend++;
        }
      } else
        for (int i = 0; i < 4; i++)
          PFwriter.write(Constants.lineSeparator);

      String firstline = Misc.getFirstPFline(thephase);
      PFwriter.write(firstline);
      PFwriter.write(Constants.lineSeparator);

      firstline = new String(" " + Misc.getIntStringFormatted(milh[npole - 1], 3) +
              Misc.getIntStringFormatted(milk[npole - 1], 3) +
              Misc.getIntStringFormatted(mill[npole - 1], 3) +
              Misc.getDoubleStringFormatted(startalpha, 3, 1) +
              Misc.getDoubleStringFormatted(finalalpha, 3, 1) +
              Misc.getDoubleStringFormatted(resolution, 3, 1) +
              Misc.getDoubleStringFormatted(startbeta, 3, 1) +
              Misc.getDoubleStringFormatted(finalbeta, 3, 1) +
              Misc.getDoubleStringFormatted(resolution, 3, 1) +
              " 1 1");
      PFwriter.write(firstline);
      PFwriter.write(Constants.lineSeparator);
    } catch (IOException io) {
    }

/*     CONSTRUCTION OF THE PLOTFILE (BERKELEY-FORMAT) */

    if (cpriu_nfie == nfiangle)
      naeh = nfiangle - 1;
    else
      naeh = cpriu_nfie;
    nbe = cpriu_ntetae;

    int until18 = 0;
    for (nb = 1; nb <= ntetangle; ++nb) {
      nbfak = (nb - 1) * naeh;
      for (na = 1; na <= naeh; ++na) {
        ny = nbfak + na;
        imh[ny - 1] = (int) (cpf55_pf55[(nb - 1) * nfiangle + na - 1] * 100.00001);
        if (imh[ny - 1] < 0) {
          imh[ny - 1] = 0;
        }

        try {
          if (until18 == 0)
            PFwriter.write(" ");
          PFwriter.write(Misc.getIntStringFormatted(imh[ny - 1], 4));
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
  } /* priucirc_ */

// this for using this class for interpolation of plotting experimental pole figures

  public Agtorgl(double resolution) {
    this.interpolation = interpolation;

/*  	resolution = interpolation.getResolution();
    nfiangle = (int) (360. / resolution + 1.00001);
    ntetangle = (int) (90. / resolution + 1.00001);
    npfrgdim = nfiangle * ntetangle;
    lastdim = npfrgdim;

  	cag_xag = new double[lastdim];
  	cag_yag = new double[lastdim];
  	cpfag_pfag = new double[lastdim];
  	cpfag_wgt = new double[lastdim];
  	cfitetag_fiagrad = new double[lastdim];
  	cfitetag_ctetag = new double[lastdim];
  	cfitetag_stetag = new double[lastdim];
  	cpf55_pf55 = new double[npfrgdim];
		crg_firgradm = new double[nfiangle];
		crg_ctrgm = new double[ntetangle];
		crg_strgm = new double[ntetangle];

  	hitcontroldist = resolution / 6;*/
  }


}
