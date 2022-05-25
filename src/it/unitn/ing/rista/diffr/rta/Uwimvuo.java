/*       Program UWIMVUO

     S. Matthies, UC Berkeley, August 1993
     translated by Luca Lutterotti using F2C, 1998
     and subsequently translated manually to java, 10/11/98, Luca Lutterotti

     Original comment for the program by S. Matthies:

                             Incomplete Pole figures with overlapping
                                 CHKLP
   The given variant of the program considers IZPOL (<= 100) pole figures.

    Terminology : A "Pole figure" contains up to 6 (IZOVERI) overlapping
                  "reflexes". IZOVERI=1 represents a common nonoverlapped
                  pole figure.
                  If there are more than 6 overlapping reflexes in a
                  pole figure, please change the DIMENSIONs "600"
                  of the corresponding arrays.
                  Each reflex has its own conventional pole figure with its
                  equivalent projection directions ("Beine",i.e. "legs").
                  The overlapping reflexes of a pole figure are weighted.
                  The sum over all weights of the overlapping reflexes of
                  a pole figure must be equal to 1.
                  The number of a pole figure is NPOLEX. For each NPOLEX
                  a "name of the pole figure" H K L exists.
                  Each reflex has its number NNPOLEX. For each NNPOLEX
                  a "name of the reflex" H K L exists.
                  All Beine considered for a given set of experimental
                  pole figures are described by a number NFMAREF.

  ******************************************

The present program version realizes the first step of the WIMV strategy
--> "Try to find the sharpest ODF with minimum RP-values".
The second WIMV demand --> "realize this with a maximum phon"
(isotropic background) can in principle also be organized in the program,
but for incomplete experimental pole figures (not normalized ; "phon" has
only a meaning for normalized quantities) it will complicate the program
too much.
If the complete, normalized recalculated pole figures (POLREF's in the
file on unit 6) indicate, that there is a remarkable phon
(minimum of all POLREF's : "MP" >= .25) do the following in order to
be shure that there are not "remaining ghosts" or to get a sharper ODF :

 1) Normal run of the program.
 2) Take the renormalized experimental pole figures (files on unit 6 or 13)
    and put the corresponding POLREF-values into the unmeasured regions.
 3) Normalize these "complete experimental pole figures" to 2*PI.
 4) Determine the minimum of all these pole figures --> maximum phon "MP".
 5) Substract MP from the "complete experimental pole figures".
 6) Run the program with these complete, phonreduced pole figures
    --> ODF^, POLREF^'s.
 7) Determine the "true normalized" ODF and POLREF's according to
              ODF = MP + (1-MP)*ODF^
           POLREF = MP + (1-MP)*POLREF^.
 8) Characteristic parameters like F2, Entropy, RP a.s.o. have a true
    meaning only if they will be calculated for the last quantities.
 9) If the so determined RP-values are much worser than the corresponding
    values from step 1) you can repeat the same, but using instead of MP
    in point 5) e.g. (MP-0.1) a.s.o.

  ******************************************


    The crystal symmetry of the ODF is described by the point group GB.
    GB is the rotational part of the crystal class Gb. GB determines the
    elementary G-space region considered in the program. The crystal
    symmetry of the reduced pole figures is described by the point group
    Gb~=Gb*Ci. For crystal classes of typ I or II (pure rotational or
    inversion symmetric groups) Gb~ = GB*Ci.
    For Gb of type III (mirror groups) Gb~=GB~*Ci. GB~ (='GBS') is the
    rotational part of Gb~, is twice greater than GB, and describes the
    higher symmetry of the reproduced ODF from reduced pole figures in
    this case.
    Therefore for type I and II  GB = GBS and for typ III GB "<" GBS is
    valid.

    Code numbers IGB(IGBS) for CRYSTAL SYMMETRY :

                   ROTATION GROUP GB (GBS)   CODE NUMBER IGB (IGBS)
                          D6  622                   11
                          C6    6                   10
                          D3  322                    9
                          C3    3                    8
                          O   432                    7
                          T    23                    6
                          D4  422                    5
                          C4    4                    4
                          D2  222                    3
                          C2    2                    2
                          C1    1                    1

 ******************************************************

        Lattice data will be asked if necessary

 ******************************************************


    FOR THE PRESCRIPTION HOW THE CRYSTAL COORDINATE
    SYSTEM KB IS DEFINED SEE TABLES  5.1 AND FIG. 5.1-5.6, 5.9-5.12 in
    S.Matthies,G.W.Vinel and K.Helming,
    Standard Distributions in Texture Analysis (Vol.1),
    Akademieverlag Berlin (1987).
    For Gb,Gb~,GB,GB~=GBS of the corresponding space group see Table 14.1 .


 INPUT :
 *****

  On unit 11 - CONTROL FILE of the experimental pole figures
  ----------
               Structure :
                           TITLE                (A40)
                           IGB,IGBS             (2I*)
                           IZPOL                ( I*)
                      IZPOL sets << of :

    <<   H, K, L, IZOVERI      (4I*)  "name of the PF",number of overlaps

     <<      if IZOVERI .GT.1 : IZOVERI {{ sets of :

     <<  {{  H, K, L, WGHT       (3I*,F*)  "name of the reflex", weight


  On unit 12 - INPUT DATA FILE of the experimental pole figures
  -----------
               Structure :
                           Direct access

     A Pole figure is given by 1387 numbers

       POLEX(NPHI,NTHETA)=POLEX(NY)
             1,73  1,19       1,1387            NY = NPHI+73*(NTHETA-1)

                             Azimuth :         Phi = 5*(NPHI-1)   degrees
                         Polar angle :       Theta = 5*(NTHETA-1) degrees

     As a rule the experimental pole figures are measured incompletly.
     The given variant of the program supposes a "girdle structure" of
     the measured pole figures. I.e. the informative region of each
     pole figure begins at a Thetamin (NTMIN >= 1) and ends at a
     Thetamax (NTMAX > NTMIN, NTMAX <= 19). For this region all "Phi"
     are measured. The unmeasured regions (!!! necessary condition !!!)
     are completly to be filled up with dummy negative numbers.
     If there are more than one measured girdles the corresponding
     pole figure is to be introduced into the program mg times describing
     always only one girdle.

     Schema of Reading/Writing :

                      DIMENSION POLEX(1387)
                      CHARACTER FNAME*40
                e.g.  FNAME= 'POLEXO.ACS'
                      OPEN(12,file=FNAME,access='direct',recl=5548)
                      DO 1 NPOLEX=1,IZPOL

                      READ/WRITE(12,rec=NPOLEX)POLEX

                  1   CONTINUE

  On unit 22 - There is a default variant of UWIMVU - CONTROL DATA
  ----------   that corresponds to the asked "IDAT" = 0 .

               For IDAT.NE.0 on unit 22 the UWIMVU CONTROL DATA FILE
               will be read containing the following CONTROL NUMBERS
               (in []-parentheses the default values are given)

                      ITZ        (*)         [20]
                      ABPRO      (*)         [1.]
                      VITPRO     (*)         [.3]
                      PEPS       (*)        [.05]
                      IZICK      (*)          [1]
                      IRP        (*)          [0]
                      R          (*)         [2.]
                      RFAK       (*)        [1.1]
                      IANFSU     (*)          [1]
                      RFANF      (*)         [2.]

  Meaning of the parameters :
  -------------------------

        ITZ               - maximum number of iteration steps.
        ABPRO  (per cent) - Stop if RP for each PF-value will be lower.
        VITPRO (per cent) - Stop if the velocity of iteration will be lower.

        R                 -  Starting value of the iteration exponent.
        PEPS   (per cent) - "RP0"-level.
        IZICK  = 1        - "Zickzack regime" for the iteration exponent R.
        IANFSU = 0        - No optimization of the Starting ODF approximation

                            estimated using the exponent RFANF.
        IRP = 0/1         - Optimization criteria for the starting ODF
                            approximation is formulated using RP or RP1.
        RFAK              - Factor changing the R-value if the Zickzack
                            regime is working.


  Adresses
  ********
    Full G-space :
    ------------
                   NG (1,197173) or
                   NA (1,73) ; NB (1,37) ; NGAM (1,73)

                   NG = 2701*(NGAM-1) + 73*(NB-1) + NA
                      = NGBG + NA
                   NGBG = 73*(NG0BG-1) ; NG0BG = 37*(NGAM-1) + NB

                   NGAM-1 = (NG-1)/2701 ; NGH = NG - 2701*(NGAM-1)
                   NB-1 = (NGH-1)/73 ; NA = NGH - 73*(NB-1)

    GB/C1 working region :
    --------------------
                      NA (1,73) ; NB (1,NYZB) ; NGAM (1,NYZG)
      one-dimensional working adresses for Beta,Gamma - N2WBG (1,NYZWBG)
                    NYZWBG=NYZB*NYZG
                    N2WBG = NYZB*(NGAM-1) + NB
      Transformation of N2WBG into NGBG :    NGBG = N2WTOBG(N2WBG)  (2701)
                    NGAM-1 = (N2WBG-1)/NYZB
                    NB = N2WBG - NYZB*(NGAM-1) --> NG0BG --> NGBG

    Cell volumes in the G-space  :
    ---------------------------
         In the working region (BETA,GAMMA) :  VW0BG(N2WBG)   (2701)
         In the whole G-space  (BETA,GAMMA):   VG0BG(NG0BG)   (2701)
                                     ALPHA :         VA(NA)     (73)

                        VG(NG) = VW0BG(N2WBG)*VA(NA)
                               = VG0BG(NG0BG)*VA(NA)

     Cell volumes in the PF-space :
     ----------------------------
         In the incomplete as well as in the complete region

               VP(NY)   (1387) ; NY = 73*(NTHETA-1) + NPHI



 OUTPUT :
 ******

  On unit 17 - WIMV STANDARD ODF INPUT/OUTPUT FILE : F88.DAT
  ----------   (5-degree Gamma sections, region for IGB as given, IGA=1)

  On unit 13 - BERKELEY PLOTFORMAT : Renormalized POLEX
  ----------


  On unit 14 - BERKELEY PLOTFORMAT : Normalized POLREF's - complete
  ----------

  End of S. Matthies comments. */


/*
 * @(#)Uwimvuo.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

import java.lang.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.*;

import java.io.*;
import java.util.StringTokenizer;

/**
 * The Uwimvuo is a class
 *
 * @version $Revision: 1.11 $, $Date: 2006/07/20 13:39:06 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Uwimvuo {

  WIMVTexture wimvTexture;

  int maxpole = 0;
//	int max_sum = 6;

  int alphamax = 73;
  int alphamax1 = alphamax - 1;
  int betamax = alphamax / 2 + 1;
  int betamax1 = betamax - 1;
  int gammamax = alphamax;
  int alphamax2 = (alphamax - 1) / 2 + 1;
  int alphamax3 = (alphamax - 1) / 3 + 1;
  int alphamax4 = (alphamax - 1) / 4 + 1;
  int alphamax5 = (alphamax - 1) / 5 + 1;
  int alphamax6 = (alphamax - 1) / 6 + 1;

  int totalmax = alphamax * betamax * gammamax;
  int old1387max = (betamax + 1) / 2 * alphamax;
  int old2701max = alphamax * betamax;

/* Common Block Declarations */

  int igb, igbs;
  double awert;
  double[] f = new double[totalmax];
//	double[] fio = new double[totalmax]; // was [73][37][73];
  double[] f0 = new double[totalmax];
  double f2, f2h, f2dif, f2difh, ent;
  int izpol;
  int itz;
  double r, rfanf, rfak;
  double[] sr = new double[alphamax], cr = new double[alphamax];
  double[] hmst = new double[24], hmct = new double[24], hmsf = new double[24],
  hmcf = new double[24];
  int nyzb, nyzgn, nyzwbg;
  int[] n2wtobg = new int[old2701max];
  int[] ifiw = new int[alphamax];
  double[] rfiw = new double[alphamax];
  int[] mgb = new int[24];
  double peps;
  double abpro, vitpro;
  int izick, ianfsu, irp;

  double[] cpolex_1 = new double[old1387max];
  double[][] cpolex_23;
  double[][] cpolex_20;
  double[][] cpolex_19;

  int[][] cpolex_21;

  int[] milh, milk, mill;
  double[] vg0bg = new double[old2701max];
  double[] vw0bg = new double[old2701max];
  double[] va = new double[alphamax];
  double[] vp = new double[old1387max];
  int[] m37 = new int[alphamax], m73 = new int[old2701max], m2701 = new int[alphamax];
  int[] mntmin, mntmax;

  int[] minv;
  double[] asthi, acthi, afhir;
  int[] mizmhi;
  double[] weight;

  int new181 = 181;
  double[] sr2 = new double[new181], cr2 = new double[new181];

  double pinfg, pi5g, pi25g, pifg, pisimg, pi75g, pi2g;

  int[] moveri;
  int[] namepfh, namepfk, namepfl;
  double s12l, c12l, s23l, c23l, s31l, c31l;
  double cda, cdb;
  double st, ct, sf, cf;
  double[] salfa = new double[96], sbeta = new double[96], sgamma = new double[96],
  calfa = new double[96], cbeta = new double[96], cgamma = new double[96];
  double[] gjex = new double[old2701max];
  double[] polref = new double[old1387max];

  double[] polex = new double[old1387max];

  double[] uvp, fakk;
  double[] polf = new double[old1387max];
  double[] pol1 = new double[old1387max];
  double phoninp, phonstep;
  double[] sm, sm1;
  double[] fs = new double[old1387max];
  int[] milhp, milkp, millp;

  double resolution = 5.0;
  double integrationStepPF = 2.0;

  int irfon_it, iabi_it, iaba_it;
  double rp_it, hrp_it;

  public Uwimvuo(WIMVTexture wimvText, int numberofpole, int maxpoleizoveri) {

    wimvTexture = wimvText;

    resolution = wimvText.getResolutionD();

    alphamax = (int) (360.00001 / resolution) + 1;   // 73
    alphamax1 = alphamax - 1;                        // 72
    betamax = (alphamax - 1) / 2 + 1;                // 37
    betamax1 = betamax - 1;                          // 36
    gammamax = alphamax;
    alphamax2 = (alphamax - 1) / 2 + 1;              // 37
    alphamax3 = (alphamax - 1) / 3 + 1;              // 25
    alphamax4 = (alphamax - 1) / 4 + 1;              // 19
    alphamax5 = (alphamax - 1) / 5 + 1;              //
    alphamax6 = (alphamax - 1) / 6 + 1;              // 13

    totalmax = alphamax * betamax * gammamax;
    old1387max = (((betamax - 1) / 2) + 1) * alphamax;
    old2701max = alphamax * betamax;

    f = new double[totalmax];
    f0 = new double[totalmax];
    sr = new double[alphamax];
    cr = new double[alphamax];
    n2wtobg = new int[old2701max];
    ifiw = new int[alphamax];
    rfiw = new double[alphamax];

    cpolex_1 = new double[old1387max];

    vg0bg = new double[old2701max];
    vw0bg = new double[old2701max];
    va = new double[alphamax];
    vp = new double[old1387max];
    m37 = new int[alphamax];
    m73 = new int[old2701max];
    m2701 = new int[alphamax];

    integrationStepPF = resolution / 5.0 * 2.0;
    new181 = (int) (360.00001 / integrationStepPF) + 1;
    sr2 = new double[new181];
    cr2 = new double[new181];

    salfa = new double[96];
    sbeta = new double[96];
    sgamma = new double[96];
    calfa = new double[96];
    cbeta = new double[96];
    cgamma = new double[96];

    gjex = new double[old2701max];
    polref = new double[old1387max];

    polex = new double[old1387max];

    polf = new double[old1387max];
    pol1 = new double[old1387max];
    fs = new double[old1387max];

    maxpole = numberofpole + 1;

    cpolex_23 = new double[maxpole][old1387max];
    cpolex_20 = new double[maxpole][old1387max];
    cpolex_19 = new double[maxpole][old1387max];

    cpolex_21 = new int[maxpole * 24][old2701max];

    milh = new int[maxpoleizoveri];
    milk = new int[maxpoleizoveri];
    mill = new int[maxpoleizoveri];
    namepfh = new int[maxpole];
    namepfk = new int[maxpole];
    namepfl = new int[maxpole];
    mntmin = new int[maxpole];
    mntmax = new int[maxpole];
    sm = new double[maxpole];
    sm1 = new double[maxpole];
    milhp = new int[maxpole];
    milkp = new int[maxpole];
    millp = new int[maxpole];
    uvp = new double[maxpole];
    fakk = new double[maxpole];
    moveri = new int[maxpole];

    int maxpole_sum = maxpoleizoveri; //sumover * maxpole;

    minv = new int[maxpole_sum];
    asthi = new double[maxpole_sum];
    acthi = new double[maxpole_sum];
    afhir = new double[maxpole_sum];
    mizmhi = new int[maxpole_sum];
    weight = new double[maxpole_sum];

    nyzgb = new int[22];

    nyzgb[0] = betamax;
    nyzgb[1] = betamax;
    nyzgb[2] = alphamax4;
    nyzgb[3] = betamax;
    nyzgb[4] = alphamax4;
    nyzgb[5] = alphamax4;
    nyzgb[6] = alphamax4;
    nyzgb[7] = betamax;
    nyzgb[8] = alphamax4;
    nyzgb[9] = betamax;
    nyzgb[10] = alphamax4;
    nyzgb[11] = alphamax;
    nyzgb[12] = betamax;
    nyzgb[13] = betamax;
    nyzgb[14] = alphamax4;
    nyzgb[15] = alphamax4;
    nyzgb[16] = betamax;
    nyzgb[17] = alphamax4;
    nyzgb[18] = alphamax3;
    nyzgb[19] = alphamax3;
    nyzgb[20] = alphamax6;
    nyzgb[21] = alphamax6;

  }

  public double[][][] computeODF(double exp_pole[][]) {

/* Local variables */
//    int[] maref = new int[old2701max];
    int ng, it;
    int npolex;
    double rp1;
//    int ita;
    int itp = 0, izr;

    double[][][] odf = new double[alphamax][betamax][alphamax];

    boolean loop_exit;

    boolean complete = datinp_(exp_pole);
    if (!complete) {
      for (int i = 0; i < alphamax; i++)
        for (int j = 0; j < betamax; j++)
          for (int k = 0; k < alphamax; k++)
            odf[i][j][k] = 1.0f;
      return odf;
    }

    rp_it = awert;
    rp1 = awert;
    f2dif = awert;
    irfon_it = 0;
    izr = 0;
    do {
      hrp_it = awert;
      ++izr;
      if (izr <= 5) {
        if (irfon_it > 0) {
          r /= MoreMath.pow(rfak, itp + 1);
          for (ng = 0; ng < totalmax; ++ng)
            f[ng] = f0[ng];
        } else {
//          System.out.println("f " + f[0]);
          System.out.println("                 ANFKOR");
          anfkor_();
//          System.out.println("f " + f[0]);
          System.out.println("                  FANFU");
          fanfuo_();
//   		 		fanfuo_(maref);

//          System.out.println("f " + f[0]);
          for (npolex = 1; npolex <= izpol; ++npolex) {
            for (int ny = 0; ny < old1387max; ++ny)
              cpolex_20[npolex - 1][ny] = cpolex_23[npolex - 1][ny];
//    	  		restore23(npolex, tmp_pol);
//    				backup20(npolex, tmp_pol);
          }
          pfexouto_();
//          System.out.println("f " + f[0]);
          System.out.println("                 FIOTTU");
          fiottu_(f, igb, alphamax);
//          System.out.println("f " + f[0]);
          System.out.println("                 FANFSU");
          System.out.println(" ");
          double rptmp[] = fanfsuo_(rp_it, rp1);
//          System.out.println("f " + f[0]);
           rp_it = rptmp[0];
          rp1 = rptmp[1];
          f2h = f2;
        }
      }  /* checked until here */
      loop_exit = true;
      for (it = 1; it <= itz; ++it) {
        boolean breakL15 = false;
        if (izr <= 5) {
          itp = it - 1;
          if (itp == 0)
            irfon_it = 0;
//          System.out.println("f " + f[0]);
           System.out.println(" TRUE ITERATION STEP # = " + Integer.toString(itp) +
                  "  R = " + Misc.getDoubleStringFormatted(r, 5, 6));
          if (it - itz >= 0)
            iaba_it = 1;
          else
            iaba_it = 0;
          fiottu_(f, igb, alphamax);
          iteruo_(itp);
//					iteruo_(itp, maref);

          if (irfon_it != 0) {
            breakL15 = true;
            it = itz + 1;
            loop_exit = false;
          } else {
            if (iabi_it == 1) {
//              ita = it - 1;
              pfouto_();
              fiottu_(f, igb, alphamax);
              odf_output(odf);
              return odf;
            }
            if (iabi_it == 0)
              breakL15 = true;
          }
        }
        if (!breakL15) {
          rp_it = hrp_it;
          for (ng = 0; ng < totalmax; ++ng) {
            f[ng] = f0[ng];
          }
          for (npolex = 1; npolex <= izpol; ++npolex) {
            for (int ny = 0; ny < old1387max; ++ny)
              cpolex_19[npolex - 1][ny] = cpolex_20[npolex - 1][ny];
//	    			restore20(npolex, tmp_pol);
//	      		backup19(npolex, tmp_pol);
          }
        }
      }
    } while (!loop_exit);
    pfouto_();
    fiottu_(f, igb, alphamax);
    odf_output(odf);
    return odf;
  }

  static int mge[]	/* was [11][25] */ = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                          0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1,
                                          0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1,
                                          0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1,
                                          0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 2, 4, 4, 8, 12, 24, 3, 6, 6,
                                          12};

  int nyzgb[] = null; //new int[22];
/*{betamax,betamax,alphamax4,betamax,alphamax4,alphamax4,alphamax4,betamax,alphamax4,betamax,alphamax4,alphamax,betamax,betamax,alphamax4,alphamax4,betamax,alphamax4,alphamax3,alphamax3,alphamax6,alphamax6};*/

  int igwpart[] = {1, 2, 4, 4, 8, 4, 8, 3, 6, 6, 12};

  int nyzgb_ref(int a_1, int a_2) {
    return nyzgb[(a_2) * 11 + a_1 - 12];
  }

  public static int mge_ref(int a_1, int a_2) {
    return mge[(a_2) * 11 + a_1 - 12];
  }

  boolean datinp_(double expole[][]) {

/* Local variables */
    double cfhi, cthi, fhir, sfhi;
    int nphi;
    double wght, sthi;
    int isum, k, l;
    double s;
//    int[] maref = new int[old2701max];
    double chelp = 0.0;
    int nbein;
    int /*izgbs,*/ ntmin;
    int ntmax, nover;
    double pi2deg;
    int ih, ij;
    double rf;
    int ny, /*mardim,*/ ntheta, npolex;
    int ny1, ny2;
    int ifi, npolex1;
    double fir;
    int ize, nth;
    double pol;
    int nfmaref, inv;
    int ihitctr, ifi1;
    double wghtctl;
    int nnpolex, izoveri;
    int poleremoved;



/*  Entering the UWIMVUO CONTROL DATA.
  Opening of the pole figure control file and pole figure data file.
  Input, determination of the corresponding NTmin and NTmax.
  Construction of the necessary (may be IGB or IGBS-depending) working data
  and arrays. */

    awert = 6.66661e33;

/*  INPUT of the UWIMVUO CONTROL DATA */

    itz = wimvTexture.getIteration();
    abpro = wimvTexture.getRpMinimum();
    vitpro = wimvTexture.getSpeedMinimum();
    peps = wimvTexture.getRPOlevel();
    izick = wimvTexture.getZigZagValue();
    irp = wimvTexture.getODFoptimizationCriteria();
    r = wimvTexture.getRexponent();
    rfak = wimvTexture.getRfactor();
    ianfsu = wimvTexture.getODFoptimizationValue();
    rfanf = wimvTexture.getRexponent();

    ++itz;

/*                            INPUT of the POLE FIGURE CONTROL FILE */

    igb = wimvTexture.getLaueGroupNumber();
    igbs = wimvTexture.getLaueGroupNumber();
    izpol = wimvTexture.getPoleFigureNumber();

    System.out.println(wimvTexture.getPhase().toXRDcatString() + " ,  Laue group = " + Integer.toString(igb));
    System.out.println("   IZPOL = " + Integer.toString(izpol));

/*  CRYSTAL SYMMETRY
    DETERMINATION OF THE SYMMETRY ELEMENTS TO BE USED */

//    izgbs = mge_ref(igbs, 25);
    if (igbs < 8)
      ize = 24;
    else
      ize = 12;
    for (ij = 1; ij <= ize; ++ij)
      mgb[ij - 1] = mge_ref(igbs, ij);

// here the output of mgb is missing

/*  MGB : active GBS symmetry elements for the determination of the
          equivalent pole figure projection directions. Dimension (96)
          because it is used in connection with the SUBROUTINE SC96.

         NYZ... determine the (GB dpendend) elementary G-space region
         used in the program for ODF calculation */

/*       NYZA=1 */
    nyzb = nyzgb_ref(igb, 1);
    nyzgn = nyzgb_ref(igb, 2);
    nyzwbg = nyzb * nyzgn;
//    mardim = nyzwbg << 1;
/*                                                    MARDIM */
    pi2deg = Constants.PI / 90. / 5.0 * resolution;
    pi2g = Constants.PI / 2.0;
    pinfg = Constants.PI * 8. * Constants.PI / igwpart[igb - 1];
    rf = resolution * Constants.DEGTOPI;
    pifg = Constants.DEGTOPI;
    pi5g = resolution * Constants.DEGTOPI;
    pi25g = pi5g / 2.;
//    pisimg = .0018518518518518519;
    pisimg = integrationStepPF / 360.0 / 3.0;
    pi75g = pi25g * 3.;

    for (ifi = 1; ifi <= alphamax; ++ifi) {
      ifi1 = ifi - 1;
      fir = rf * ifi1;
      rfiw[ifi - 1] = fir;
      ifiw[ifi - 1] = (int) (ifi1 * (resolution + 0.000001));
      sr[ifi - 1] = Math.sin(fir);
      cr[ifi - 1] = Math.cos(fir);
      m2701[ifi - 1] = ifi1 * old2701max;
      m37[ifi - 1] = ifi1 * betamax;
    }
    for (ifi = 0; ifi < old2701max; ++ifi) {
      m73[ifi] = ifi * alphamax;
    }
    for (ifi = 0; ifi < new181; ++ifi) {
      sr2[ifi] = Math.sin(pi2deg * ifi);
      cr2[ifi] = Math.cos(pi2deg * ifi);
    }

/*                  Transfer array :   N2WTOBG(N2WBG)  (2701) */
    transwtobg_();

    lattice_();

    System.out.println("  VG,VP calculation");
/*                                                       VG,VP */
    svw0bgvp_();

    nfmaref = 1;
    nnpolex = 0;

/*                            ****************  POLE FIGURE LOOP */

    poleremoved = 0;
    npolex = 0;
    for (npolex1 = 1; npolex1 <= izpol; ++npolex1) {
      ++npolex;

      ih = wimvTexture.getH(npolex1 - 1);
      k = wimvTexture.getK(npolex1 - 1);
      l = wimvTexture.getL(npolex1 - 1);
      izoveri = wimvTexture.getIzoveri(npolex1 - 1);

      namepfh[npolex - 1] = ih;
      namepfk[npolex - 1] = k;
      namepfl[npolex - 1] = l;
      moveri[npolex - 1] = izoveri;
      for (ny = 0; ny < old1387max; ny++) {
        cpolex_1[ny] = expole[npolex - 1][ny];//[(npolex - 1) * 1387 + ny];
//        System.out.println("pole " + npolex1 + ", " + ny + ", :" + cpolex_1[ny]);
      }
      s = 0.;
      for (ny = 0; ny < alphamax; ++ny)
        s += cpolex_1[ny];
      s /= alphamax;
      for (ny = 0; ny < alphamax; ++ny)
        cpolex_1[ny] = s;

/*  Thetamin,Thetamax determination, check of the POLEX */

      ntmin = alphamax4;
      ntmax = 1;
      for (ntheta = 1; ntheta <= alphamax4; ++ntheta) {
        nth = m73[ntheta - 1];
        for (nphi = 1; nphi <= alphamax; ++nphi) {
          ny = nphi + nth;
          pol = cpolex_1[ny - 1];
          if (nphi != 1) {
/*            if (chelp < 0. && pol >= 0.) {
              System.out.println("Error chelp < 0, pol > 0");
            }
            if (chelp >= 0. && pol < 0.) {
              System.out.println("Error chelp > 0, pol < 0");
            }*/
          }
          if (pol < 0.) {
            cpolex_1[ny - 1] = -666.666;
          }
          pol = cpolex_1[ny - 1];
          if (pol >= 0. && ntmin > ntheta) {
            ntmin = ntheta;
          }
          if (pol >= 0. && ntmax < ntheta) {
            ntmax = ntheta;
          }
          chelp = pol;
        } /* nphi*/
      } /* ntheta*/
      if (ntmax - ntmin < 0) {
        --npolex;
        poleremoved += 1;
      } else {
        mntmin[npolex - 1] = ntmin;
        mntmax[npolex - 1] = ntmax;

/* Theta=90 :  PHI and PHI+180  are equivalent */

        if (ntmax >= alphamax4) {
          s = (cpolex_1[old1387max - 2 * betamax1 - 1] + cpolex_1[old1387max - betamax1 - 1]
                  + cpolex_1[old1387max - 1]) / 3.;
          cpolex_1[old1387max - 2 * betamax1 - 1] = s;
          cpolex_1[old1387max - betamax1 - 1] = s;
          cpolex_1[old1387max - 1] = s;
          for (nphi = 2; nphi <= betamax1; ++nphi) {
            ny1 = nphi + old1387max - 2 * betamax1 - 1;
            ny2 = nphi + old1387max - betamax1 - 1;
            s = (cpolex_1[ny1 - 1] + cpolex_1[ny2 - 1]) / 2.;
            cpolex_1[ny1 - 1] = s;
            cpolex_1[ny2 - 1] = s;
          }
        }
        nth = m73[ntmin + 1];
        for (ny = 0; ny < old1387max; ++ny)
          cpolex_23[npolex - 1][ny] = cpolex_1[ny];
/*                         Reflex loop           */

        ntmin = mntmin[npolex - 1];
        ntmax = mntmax[npolex - 1];
        wghtctl = 0.;
        for (nover = 1; nover <= izoveri; ++nover) {
          ++nnpolex;
          ih = wimvTexture.getH(npolex1 - 1, nover - 1);
          k = wimvTexture.getK(npolex1 - 1, nover - 1);
          l = wimvTexture.getL(npolex1 - 1, nover - 1);
          wght = wimvTexture.getWeightSingle(npolex1 - 1, nover - 1);
//	  				System.out.println(Integer.toXRDcatString(ih) + " "+Integer.toXRDcatString(k) +" "+ Integer.toXRDcatString(l));
//						System.out.println(nover - 1);
//						System.out.println(wght);
/* lux 12  READ(11,*)IH,K,L,WGHT */
          milh[nnpolex - 1] = ih;
          milk[nnpolex - 1] = k;
          mill[nnpolex - 1] = l;
          weight[nnpolex - 1] = wght;
          wghtctl += wght;
          if (nover == izoveri) {
            if (wghtctl <= 1.00001 && wghtctl >= .99999) {
              wghtctl = 1.;
            }
          }
          if (nover == izoveri && wghtctl != 1.) {
            System.out.println("nover == izoveri && wghtctl != 1.");
          }

/* Determination of Theta(hi) and Phi(hi) for the actual PF (HKL) */

          tfhkl_(ih, k, l);

/* Determination of equivalent HKL directions. INV=1 detects that the
   HKL-direction is of type h*. Otherwise INV=0 */

          nbein = nfmaref;
          int[] result = equiv_(ize, nbein);

          inv = result[0];
//	  			inv = 1;               // Dangerous, may be is true
          isum = result[1];

          cthi = hmct[0];
          sthi = hmst[0];
          cfhi = hmcf[0];
          sfhi = hmsf[0];
          fhir = Math.acos(cfhi);
          if (sfhi < 0.) {
            fhir = Constants.PI2 - fhir;
          }
          acthi[nnpolex - 1] = cthi;
          asthi[nnpolex - 1] = sthi;
          afhir[nnpolex - 1] = fhir;
          mizmhi[nnpolex - 1] = isum;
          minv[nnpolex - 1] = inv;

/*                                         MAREF CALCULATION */

//	  			nfmaref = marefuo_(nnpolex, nfmaref, maref, ntmin, ntmax);
          nfmaref = marefuo_(nnpolex, nfmaref, ntmin, ntmax);
        }
      } /* ex L1 */
    } /* npolex1 */
    izpol = izpol - poleremoved;

/*                                  GJEX and HIT  CALCULATION */

//  	ihitctr = siguo_(maref);
    ihitctr = siguo_();
    if (ihitctr == 0) {
      System.out.println("Warning: odf not covered sufficiently by PFs, increase the number of PFs");
      return false;
    }
    return true;
  } /* datinp_ */

  void transwtobg_() {

/* Local variables */
    int ngbg, ng0bg, ngam1, n2wbg, nb;


/*  INPUT N2WBG -----> OUTPUT NGBG = 73*(NG0BG-1)
                                         NG0BG = 37*(NGAM-1)+NB

    NGAM-1 = (N2WBG-1)/NYZB ; NB = N2WBG - NYZB*(NGAM-1) */

    for (n2wbg = 1; n2wbg <= nyzwbg; ++n2wbg) {
      ngam1 = (n2wbg - 1) / nyzb;
      nb = n2wbg - nyzb * ngam1;
      ng0bg = m37[ngam1] + nb;
      ngbg = m73[ng0bg - 1];
      n2wtobg[n2wbg - 1] = ngbg;
    }
  } /* transwtobg_ */


  void svw0bgvp_() {

/* Local variables */
    int ngam, iphi;
    double htet;
    int itet, ng0bg, n2wbg;
    double a, b;
    int ihelp, nb, ny;
    double hfi, hvb, hvg;


/*  Structure :
          VW0BG(N2WBG)=VW0BG(NBET,NGAM)    (1,NYZB;1,NYZG)
          VG0BG(NG0BG)=VG0BG(NBET,NGAM)    (1,37  ;1,73)
          VA(NA)
          VP(NY)=VP(NPHI,NTHETA) */

/*                                              VW0BG */

    for (ngam = 1; ngam <= nyzgn; ++ngam) {
      ihelp = nyzb * (ngam - 1);
      if (ngam - 1 <= 0 || ngam - nyzgn >= 0)
        hvg = pi25g;
      else
        hvg = pi5g;

      for (nb = 1; nb <= nyzb; ++nb) {
        n2wbg = nb + ihelp;
        if (nb - 1 <= 0) {
          a = 0.;
          b = pi25g;
        } else {
          if (nb - nyzb >= 0) {
            b = rfiw[nyzb - 1];
            a = b - pi25g;
          } else {
            b = rfiw[nb - 1];
            a = b - pi25g;
            b += pi25g;
          }
        }

        hvb = Math.cos(a) - Math.cos(b);
        vw0bg[n2wbg - 1] = hvb * hvg;
      }
    }
/*                                              VG0BG,VA */
    for (ngam = 1; ngam <= alphamax; ++ngam) {
      ihelp = m37[ngam - 1];
      if (ngam - 1 <= 0 || ngam - alphamax >= 0)
        hvg = pi25g;
      else
        hvg = pi5g;

      va[ngam - 1] = hvg;
      for (nb = 1; nb <= betamax; ++nb) {
        ng0bg = nb + ihelp;
        if (nb - 1 <= 0) {
          a = 0.;
          b = pi25g;
        } else {
          if (nb - betamax >= 0) {
            b = rfiw[betamax1];
            a = b - pi25g;
          } else {
            b = rfiw[nb - 1];
            a = b - pi25g;
            b += pi25g;
          }
        }

        hvb = Math.cos(a) - Math.cos(b);
        vg0bg[ng0bg - 1] = hvb * hvg;
      }
    }
/*                                                VP */
    for (itet = 1; itet <= alphamax4; ++itet) {
      ihelp = m73[itet - 1];
      if (itet - 1 <= 0) {
        a = 0.;
        b = pi25g;
      } else {
        if (itet - alphamax4 >= 0) {
          a = (90.0 - pi25g) * Constants.DEGTOPI;
          b = Constants.PI / 2.;
        } else {
          b = rfiw[itet - 1];
          a = b - pi25g;
          b += pi25g;
        }
      }

      htet = Math.cos(a) - Math.cos(b);
      for (iphi = 1; iphi <= alphamax; ++iphi) {
        ny = iphi + ihelp;
        if (iphi - 1 <= 0 || iphi - alphamax >= 0)
          hfi = pi25g;
        else
          hfi = pi5g;

        vp[ny - 1] = htet * hfi;
      }
    }
  } /* svw0bgvp_ */


  void lattice_() {
/* Local variables */
    double arg;
    double cella, cellb, cellc, alpha23, alpha31, alpha12;

/*     INPUT : Lattice parameters necessary for Theta,Phi calculation
 *     of (HKL)-directions */

    Phase aphase = (Phase) wimvTexture.getParent();

    cella = aphase.getFullCellValue(0);
    cellb = aphase.getFullCellValue(1);
    cellc = aphase.getFullCellValue(2);
    alpha12 = aphase.getFullCellValue(5);
    alpha23 = aphase.getFullCellValue(3);
    alpha31 = aphase.getFullCellValue(4);

    cda = cellc / cella;
    cdb = cellc / cellb;
    switch (igb) {
      case 1:
        arg = alpha12 * Constants.DEGTOPI;
        s12l = Math.sin(arg);
        c12l = Math.cos(arg);
        arg = alpha23 * Constants.DEGTOPI;
        s23l = Math.sin(arg);
        c23l = Math.cos(arg);
        arg = alpha31 * Constants.DEGTOPI;
        s31l = Math.sin(arg);
        c31l = Math.cos(arg);
        break;
      case 2:
        arg = alpha12 * Constants.DEGTOPI;
        s12l = Math.sin(arg);
        c12l = Math.cos(arg);
        s23l = 1.;
        c23l = 0.;
        s31l = 1.;
        c31l = 0.;
        break;
      case 3:
        s12l = 1.;
        c12l = 0.;
        s23l = 1.;
        c23l = 0.;
        s31l = 1.;
        c31l = 0.;
        break;
      case 4:
      case 5:
        s12l = 1.;
        c12l = 0.;
        s23l = 1.;
        c23l = 0.;
        s31l = 1.;
        c31l = 0.;
        cdb = cda;
        break;
      case 6:
      case 7:
        s12l = 1.;
        c12l = 0.;
        s23l = 1.;
        c23l = 0.;
        s31l = 1.;
        c31l = 0.;
        cda = 1.;
        cdb = 1.;
        break;
      case 8:
      case 9:
      case 10:
      case 11:
        s12l = Constants.sinArg2PIover3;
        c12l = Constants.cosArg2PIover3;
        s23l = 1.;
        c23l = 0.;
        s31l = 1.;
        c31l = 0.;
        cdb = cda;
        break;
      default:
        {
        }
    }
  } /* lattice_ */

  void tfhkl_(int ih, int k, int l) {
/* Local variables */
    double q, r, x, y, z, fr, tr, arg;

/*     Theta,Phi calculation for given (H,K,L) and Lattice parameters
 *     Output : sin,cos Theta,Phi */

    q = c12l * 2. * c31l * c23l + 1. - c12l * c12l - c23l * c23l - c31l * c31l;
    q = Math.sqrt(q);
    x = cda * ih - c31l * l;
    y = (c31l * c23l - c12l) * x;
    y += s31l * s31l * (cdb * k - c23l * l);
    y /= q;
    z = s31l * l;
    r = x * x + y * y + z * z;
    r = Math.sqrt(r);
    x /= r;
    z /= r;
    st = 0.;
    ct = 1.;
    sf = 0.;
    cf = 1.;
    if (Math.abs(z) >= 1.) {
      if (z < 0.)
        ct = -1.;
      return;
    }
    ct = z;
    tr = Math.acos(z);
    st = Math.sin(tr);
    arg = x / st;
    if (Math.abs(arg) >= 1.) {
      cf = arg / Math.abs(arg);
      return;
    }
    fr = Math.acos(arg);
    sf = Math.sin(fr);
    if (y < 0.)
      sf = -sf;
    cf = arg;
  } /* tfhkl_ */

  public static final double[] tfhkl(int h, int k, int l, double c31, double c23, double c12,
                                     double s31, double cda, double cdb) {
/* Local variables */
    double q, r, x, y, z, fr, tr, arg;

    double sctf[] = new double[4];

/*     Theta,Phi calculation for given (H,K,L) and Lattice parameters
 *     Output : sin,cos Theta,Phi */

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
    sctf[1] = 1.;
    sctf[2] = 0.;
    sctf[3] = 1.;
    if (Math.abs(z) >= 1.) {
      if (z < 0.)
        sctf[1] = -1.;
      return sctf;
    }
    sctf[1] = z;
    tr = Math.acos(z);
    sctf[0] = Math.sin(tr);
    arg = x / sctf[0];
    if (Math.abs(arg) >= 1.) {
      sctf[3] = arg / Math.abs(arg);
      return sctf;
    }
    fr = Math.acos(arg);
    sctf[2] = Math.sin(fr);
    if (y < 0.)
      sctf[2] = -sctf[2];
    sctf[3] = arg;
    return sctf;
  } /* tfhkl_ */

  int[] equiv_(int ize, int nbein) {

/* Local variables */
    double phih, teth/*, test*/;
    int i, ii, inv, isum;
    int[] im = new int[24];
    int ii1, im1;
    double hcf, can, cbn, cgn;
    int ima, iii;
    double hct, hsf, san, sbn;
    double sgn, hst, hcf1, hct1, hsf1;

/*   Determination of equivalent HKL directions. Trick using SC96 or
 *   SC48 and g(-1)*(001) relation for g=(h,epsilon).INV=1 means that
 *   the HKL-direction is of type h*. Otherwise INV=0. Ordering of the
 *   equivalent directions, print.For INV=1 only the half of the
 *   equivalents (in the upper halfsphere) are given. */

    can = 1.;
    san = 0.;
    cbn = ct;
    sbn = st;
    cgn = -cf;
    sgn = sf;
    if (igb < 8) {
      sc96_(can, san, cbn, sbn, cgn, sgn);
    } else {
      sc48_(can, san, cbn, sbn, cgn, sgn);
    }
    i = 0;
    for (ii = 1; ii <= ize; ++ii) {
      im[ii - 1] = 0;
      if (mgb[ii - 1] != 0) {
        ++i;
        im[i - 1] = 1;
        hmsf[i - 1] = 0.;
        hmcf[i - 1] = 1.;
        hmst[i - 1] = sbeta[ii - 1];
        hmct[i - 1] = cbeta[ii - 1];
        if (Math.abs(hmct[i - 1]) < .999) {
          hmsf[i - 1] = sgamma[ii - 1];
          hmcf[i - 1] = -cgamma[ii - 1];
        }
      }
    }

/*  Detection of inverse directions */

    inv = 0;
    if (i > 1) {
      ima = i;
      im1 = i - 1;
      for (ii = 1; ii <= im1; ++ii) {
        if (im[ii - 1] != 0) {
          hct = hmct[ii - 1];
          hcf = hmcf[ii - 1];
          hsf = hmsf[ii - 1];
          ii1 = ii + 1;
          for (iii = ii1; iii <= ima; ++iii) {
            if (im[iii - 1] != 0) {
              hct1 = hmct[iii - 1];
              if (Math.abs(hct + hct1) <= .001) {
                if (Math.abs(hct) >= .999) {
                  im[iii - 1] = 0;
                  inv = 1;
                } else {
                  hsf1 = hmsf[iii - 1];
                  if (Math.abs(hsf + hsf1) <= .001) {
                    hcf1 = hmcf[iii - 1];
                    if (Math.abs(hcf + hcf1) <= .001) {
                      im[iii - 1] = 0;
                      inv = 1;
                    }
                  }
                }
              }
            }
          }
        }
      }

      /*  Detection of equal directions */

      ima = i;
      im1 = i - 1;
      for (ii = 1; ii <= im1; ++ii) {
        if (im[ii - 1] != 0) {
          hct = hmct[ii - 1];
          hcf = hmcf[ii - 1];
          hsf = hmsf[ii - 1];
          ii1 = ii + 1;
          for (iii = ii1; iii <= ima; ++iii) {
            if (im[iii - 1] != 0) {
              hct1 = hmct[iii - 1];
              if (Math.abs(hct - hct1) <= .001) {
                hsf1 = hmsf[iii - 1];
                if (Math.abs(hsf - hsf1) <= .001) {
                  hcf1 = hmcf[iii - 1];
                  if (Math.abs(hcf - hcf1) <= .001) {
                    im[iii - 1] = 0;
                  }
                }
              }
            }
          }
        }
      }

    }
/*  Order of not inverse and not identical (HKL) into the upper */
/*  halfsphere for INV=1 */

    isum = 0;
    for (i = 1; i <= ize; ++i) {
      if (im[i - 1] != 0) {
        ++isum;
        hct = hmct[i - 1];
        hst = hmst[i - 1];
        hcf = hmcf[i - 1];
        hsf = hmsf[i - 1];
        hmct[isum - 1] = hct;
        hmst[isum - 1] = hst;
        hmcf[isum - 1] = hcf;
        hmsf[isum - 1] = hsf;
/*                   all hmi in the upper halfsphere    */
        if (hct <= .001) {
          if (hct > -.001) {
/* HKL on equator */
            if (hsf < 0. || hcf <= -.999) {
              hmcf[isum - 1] = -hcf;
              hmsf[isum - 1] = -hsf;
            }
          } else {

/* HKL in the lower halfsphere */

            hmct[isum - 1] = -hct;
            hmcf[isum - 1] = -hcf;
            hmsf[isum - 1] = -hsf;
          }
        }
      }
    }

/*  Print of the equivalent (HKL)-directions */

    System.out.println("Equivalent HKL-directions :");
    if (inv == 1) {
      System.out.println("INV = 1, i.e. HKL is of type h*; only the half number of the equivalent HKL-directions is given");
    }

    for (i = 1; i <= isum; ++i) {
      if (hmct[i - 1] < -1.) {
        hmct[i - 1] = -1.;
      }
      if (hmct[i - 1] > 1.) {
        hmct[i - 1] = 1.;
      }
      if (hmcf[i - 1] < -1.) {
        hmcf[i - 1] = -1.;
      }
      if (hmcf[i - 1] > 1.) {
        hmcf[i - 1] = 1.;
      }
      teth = Math.acos(hmct[i - 1]) / Constants.DEGTOPI;
      phih = Math.acos(hmcf[i - 1]) / Constants.DEGTOPI;
      if (hmsf[i - 1] < 0.) {
        phih = 360. - phih;
      }
      ++nbein;
      System.out.println("Theta = " + Misc.getDoubleStringFormatted(teth, 4, 6) +
              " Phi = " + Misc.getDoubleStringFormatted(phih, 4, 6) +
              " NBEIN = " + Integer.toString(nbein));
    }

    int[] result = new int[2];
    result[0] = inv;
//    result[0] = 1;          // Dangerous, may be is true
    result[1] = isum;

    return result;
  } /* equiv_ */

  public static final int equiv(int igbl, double[] sctf) {

/* Local variables */
//    double phih, teth, test;
    int i, ii, inv/*, isum*/;
    int[] im = new int[24];
    int ii1, im1;
    double hcf, can, cbn, cgn;
    int ima, iii;
    double hct, hsf, san, sbn;
    double sgn, /*hst,*/ hcf1, hct1, hsf1;
    double[] salphal, calphal, sbetal, cbetal, sgammal, cgammal;
    double[] hmstl = new double[24], hmctl = new double[24], hmsfl = new double[24],
            hmcfl = new double[24];

    salphal = new double[96];
    calphal = new double[96];
    sbetal = new double[96];
    cbetal = new double[96];
    sgammal = new double[96];
    cgammal = new double[96];

    int izel = 12;
    if (igbl < 8)
      izel = 24;

/*   Determination of equivalent HKL directions. Trick using SC96 or
 *   SC48 and g(-1)*(001) relation for g=(h,epsilon).INV=1 means that
 *   the HKL-direction is of type h*. Otherwise INV=0. Ordering of the
 *   equivalent directions, print.For INV=1 only the half of the
 *   equivalents (in the upper halfsphere) are given. */

    can = 1.;
    san = 0.;
    cbn = sctf[1];
    sbn = sctf[0];
    cgn = -sctf[3];
    sgn = sctf[2];
    if (igbl < 8) {
      sc96(can, san, cbn, sbn, cgn, sgn,
              salphal, calphal, sbetal, cbetal, sgammal, cgammal);
    } else {
      sc48(can, san, cbn, sbn, cgn, sgn,
              salphal, calphal, sbetal, cbetal, sgammal, cgammal);
    }
    i = 0;
    for (ii = 1; ii <= izel; ++ii) {
      im[ii - 1] = 0;
      if (mge_ref(igbl, ii) != 0) {
        ++i;
        im[i - 1] = 1;
        hmsfl[i - 1] = 0.;
        hmcfl[i - 1] = 1.;
        hmstl[i - 1] = sbetal[ii - 1];
        hmctl[i - 1] = cbetal[ii - 1];
        if (Math.abs(hmctl[i - 1]) < .99999) {
          hmsfl[i - 1] = sgammal[ii - 1];
          hmcfl[i - 1] = -cgammal[ii - 1];
        }
      }
    }

/*  Detection of inverse directions */

    inv = 0;
    if (i > 1) {
      ima = i;
      im1 = i - 1;
      for (ii = 1; ii <= im1; ++ii) {
        if (im[ii - 1] != 0) {
          hct = hmctl[ii - 1];
          hcf = hmcfl[ii - 1];
          hsf = hmsfl[ii - 1];
          ii1 = ii + 1;
          for (iii = ii1; iii <= ima; ++iii) {
            if (im[iii - 1] != 0) {
              hct1 = hmctl[iii - 1];
              if (Math.abs(hct + hct1) <= .00001) {
                if (Math.abs(hct) >= .99999) {
                  im[iii - 1] = 0;
                  inv = 1;
                } else {
                  hsf1 = hmsfl[iii - 1];
                  if (Math.abs(hsf + hsf1) <= .00001) {
                    hcf1 = hmcfl[iii - 1];
                    if (Math.abs(hcf + hcf1) <= .00001) {
                      im[iii - 1] = 0;
                      inv = 1;
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
//	  inv = 1;  // dangerous, may be is true
    return inv;
  } /* equiv_ */

  void sc48_(double can, double san, double cbn, double sbn, double cgn, double sgn) {

/* Local variables */
    int icbg, i, j, k;
    double s3;
    int ica, isa, isg;
    int i__1;

/*    INPUT COS,SIN (ALPHA0,BETA0,GAMMA0) OF G0 */
/*    CALCULATION OF SIN COS (ALPHA,BETA,GAMMA) FOR */
/*    THE 48 HEXAGONAL-ORTHORHOMBIC EQUIVALENT POSITIONS */

    for (i = 1; i <= 48; ++i) {
      sbeta[i - 1] = sbn;
    }
    for (i = 1; i <= 6; ++i) {
      salfa[i - 1] = san;
      calfa[i - 1] = can;
      cbeta[i - 1] = cbn;
    }
    sgamma[0] = sgn;
    cgamma[0] = cgn;
    s3 = Math.sqrt(3.);
    sgamma[1] = (s3 * cgn + sgn) / 2.;
    sgamma[2] = (s3 * cgn - sgn) / 2.;
    cgamma[1] = (cgn - s3 * sgn) / 2.;
    cgamma[2] = (-cgn - s3 * sgn) / 2.;
    for (i = 4; i <= 6; ++i) {
      sgamma[i - 1] = -sgamma[i - 4];
      cgamma[i - 1] = -cgamma[i - 4];
    }
    for (j = 1; j <= 7; ++j) {
      for (i = 1; i <= 6; ++i) {
        k = i + j * 6;
        i__1 = (j + 1) / 2;
        isa = MoreMath.powint(-1, i__1);
        i__1 = j / 4;
        isg = MoreMath.powint(-1, i__1);
        ica = isa * isg;
        i__1 = j * 5 / 4;
        icbg = MoreMath.powint(-1, i__1);
        salfa[k - 1] = isa * salfa[i - 1];
        calfa[k - 1] = ica * calfa[i - 1];
        cbeta[k - 1] = icbg * cbeta[i - 1];
        sgamma[k - 1] = isg * sgamma[i - 1];
        cgamma[k - 1] = icbg * cgamma[i - 1];
      }
    }
  } /* sc48_ */

  void sc96_(double can, double san, double cbn, double sbn, double cgn, double sgn) {
    /* System generated locals */
    int i__1;

/* Local variables */
    int i, j, k, l, m;
    double s2, gklein, hilfsg;
    int ik11, ik02, il11, il02, il32, ik34, ik54, ik14, ik74;

/*    INPUT COS,SIN (ALPHA0,BETA0,GAMMA0) OF G0 */
/*    CALCULATION OF SIN COS (ALPHA,BETA,GAMMA) FOR */
/*    THE 96 CUBIC-ORTHORHOMBIC EQUIVALENT POSITIONS */

    gklein = 1.0e-30;
    salfa[0] = san;
    calfa[0] = can;
    sbeta[0] = sbn;
    cbeta[0] = cbn;
    sgamma[0] = sgn;
    cgamma[0] = cgn;

/*    DETERMINATION ALPHA,BETA,GAMMA */

    hilfsg = sbn * sgn;
    cbeta[1] = hilfsg;
    s2 = 1. - hilfsg * hilfsg;
    if (s2 < 0.) {
      s2 = 0.;
    }
    hilfsg = Math.sqrt(s2);
    sbeta[1] = hilfsg;
    if (hilfsg < gklein) {
      cgamma[1] = 1.;
      sgamma[1] = 0.;
      calfa[1] = can * sbn;
      salfa[1] = san * sbn;
      if (cbeta[1] >= 0.) {
        calfa[1] = -calfa[1];
        salfa[1] = -salfa[1];
      }
    } else {
      cgamma[1] = cbn / hilfsg;
      sgamma[1] = sbn * cgn / hilfsg;
      calfa[1] = -(can * cbn * sgn + san * cgn) / hilfsg;
      salfa[1] = (-san * cbn * sgn + can * cgn) / hilfsg;
    }

/*    DETERMINATION ALPHAB,BETAB,GAMMAB */

    hilfsg = sbn * cgn;
    cbeta[2] = hilfsg;
    s2 = 1. - hilfsg * hilfsg;
    if (s2 < 0.) {
      s2 = 0.;
    }
    hilfsg = Math.sqrt(s2);
    sbeta[2] = hilfsg;
    if (hilfsg < gklein) {
      cgamma[2] = 1.;
      sgamma[2] = 0.;
      calfa[2] = can * cbn * sgn + san * cgn;
      salfa[2] = san * cbn * sgn - can * cgn;
      if (cbeta[2] > 0.) {
        calfa[2] = -calfa[2];
        salfa[2] = -salfa[2];
      }
    } else {
      cgamma[2] = sbn * sgn / hilfsg;
      sgamma[2] = cbn / hilfsg;
      calfa[2] = -(can * cbn * cgn - san * sgn) / hilfsg;
      salfa[2] = -(san * cbn * cgn + can * sgn) / hilfsg;
    }

/*    CYCLES OF SIGNS FOR THE 96 CASES */

    for (i = 1; i <= 3; ++i) {
      for (k = 1; k <= 8; ++k) {
        j = (k - 1) * 3;
        i__1 = k + 1;
        ik11 = MoreMath.powint(-1, i__1);
        i__1 = k / 2;
        ik02 = MoreMath.powint(-1, i__1);
        for (l = 1; l <= 4; ++l) {
          m = (l - 1) * 24;
          sbeta[m + j + i - 1] = sbeta[i - 1];
          i__1 = l + 1;
          il11 = MoreMath.powint(-1, i__1);
          i__1 = l / 2;
          il02 = MoreMath.powint(-1, i__1);
          salfa[m + j + i - 1] = il11 * ik11 * salfa[i - 1];
          calfa[m + j + i - 1] = il02 * ik11 * calfa[i - 1];
          i__1 = (l + 3) / 2;
          il32 = MoreMath.powint(-1, i__1);
          cbeta[m + j + i - 1] = il32 * ik11 * cbeta[i - 1];
          if (ik02 > 0) {
            i__1 = (k + 7) / 4;
            ik74 = MoreMath.powint(-1, i__1);
            i__1 = (k + 1) / 4;
            ik14 = MoreMath.powint(-1, i__1);
            sgamma[m + j + i - 1] = ik14 * il32 * sgamma[i - 1];
            cgamma[m + j + i - 1] = ik74 * il32 * cgamma[i - 1];
          } else {
            i__1 = (k + 5) / 4;
            ik54 = MoreMath.powint(-1, i__1);
            i__1 = (k + 3) / 4;
            ik34 = MoreMath.powint(-1, i__1);
            sgamma[m + j + i - 1] = ik54 * il32 * cgamma[i - 1];
            cgamma[m + j + i - 1] = ik34 * il32 * sgamma[i - 1];
          }
        }
      }
    }
  } /* sc96_ */

  public static final void sc48(double can, double san, double cbn, double sbn, double cgn, double sgn,
                                double[] salphal, double[] calphal, double[] sbetal, double[] cbetal,
                                double[] sgammal, double[] cgammal) {

/* Local variables */
    int icbg, i, j, k;
    double s3;
    int ica, isa, isg;
    int i__1;

/*    INPUT COS,SIN (ALPHA0,BETA0,GAMMA0) OF G0 */
/*    CALCULATION OF SIN COS (ALPHA,BETA,GAMMA) FOR */
/*    THE 48 HEXAGONAL-ORTHORHOMBIC EQUIVALENT POSITIONS */

    for (i = 0; i < 48; ++i) {
      sbetal[i] = sbn;
    }
    for (i = 0; i < 6; ++i) {
      salphal[i] = san;
      calphal[i] = can;
      cbetal[i] = cbn;
    }
    sgammal[0] = sgn;
    cgammal[0] = cgn;
    s3 = Math.sqrt(3.);
    sgammal[1] = (s3 * cgn + sgn) / 2.;
    sgammal[2] = (s3 * cgn - sgn) / 2.;
    cgammal[1] = (cgn - s3 * sgn) / 2.;
    cgammal[2] = (-cgn - s3 * sgn) / 2.;
    for (i = 3; i < 6; ++i) {
      sgammal[i] = -sgammal[i - 3];
      cgammal[i] = -cgammal[i - 3];
    }
    for (j = 1; j < 8; ++j) {
      for (i = 0; i < 6; ++i) {
        k = i + j * 6;
        i__1 = (j + 1) / 2;
        isa = MoreMath.powint(-1, i__1);
        i__1 = j / 4;
        isg = MoreMath.powint(-1, i__1);
        ica = isa * isg;
        i__1 = j * 5 / 4;
        icbg = MoreMath.powint(-1, i__1);
        salphal[k] = isa * salphal[i];
        calphal[k] = ica * calphal[i];
        cbetal[k] = icbg * cbetal[i];
        sgammal[k] = isg * sgammal[i];
        cgammal[k] = icbg * cgammal[i];
      }
    }
  } /* sc48 */

  public static final void sc96(double can, double san, double cbn, double sbn, double cgn, double sgn,
                                double[] salphal, double[] calphal, double[] sbetal, double[] cbetal,
                                double[] sgammal, double[] cgammal) {
    /* System generated locals */
    int i__1;

/* Local variables */
    int i, j, k, l, m;
    double s2, gklein, hilfsg;
    int ik11, ik02, il11, il02, il32, ik34, ik54, ik14, ik74;

/*    INPUT COS,SIN (ALPHA0,BETA0,GAMMA0) OF G0 */
/*    CALCULATION OF SIN COS (ALPHA,BETA,GAMMA) FOR */
/*    THE 96 CUBIC-ORTHORHOMBIC EQUIVALENT POSITIONS */

    gklein = 1.0e-30;
    salphal[0] = san;
    calphal[0] = can;
    sbetal[0] = sbn;
    cbetal[0] = cbn;
    sgammal[0] = sgn;
    cgammal[0] = cgn;

/*    DETERMINATION ALPHA,BETA,GAMMA */

    hilfsg = sbn * sgn;
    cbetal[1] = hilfsg;
    s2 = 1. - hilfsg * hilfsg;
    if (s2 < 0.) {
      s2 = 0.;
    }
    hilfsg = Math.sqrt(s2);
    sbetal[1] = hilfsg;
    if (hilfsg < gklein) {
      cgammal[1] = 1.;
      sgammal[1] = 0.;
      calphal[1] = can * sbn;
      salphal[1] = san * sbn;
      if (cbetal[1] >= 0.) {
        calphal[1] = -calphal[1];
        salphal[1] = -salphal[1];
      }
    } else {
      cgammal[1] = cbn / hilfsg;
      sgammal[1] = sbn * cgn / hilfsg;
      calphal[1] = -(can * cbn * sgn + san * cgn) / hilfsg;
      salphal[1] = (-san * cbn * sgn + can * cgn) / hilfsg;
    }

/*    DETERMINATION ALPHAB,BETAB,GAMMAB */

    hilfsg = sbn * cgn;
    cbetal[2] = hilfsg;
    s2 = 1. - hilfsg * hilfsg;
    if (s2 < 0.) {
      s2 = 0.;
    }
    hilfsg = Math.sqrt(s2);
    sbetal[2] = hilfsg;
    if (hilfsg < gklein) {
      cgammal[2] = 1.;
      sgammal[2] = 0.;
      calphal[2] = can * cbn * sgn + san * cgn;
      salphal[2] = san * cbn * sgn - can * cgn;
      if (cbetal[2] > 0.) {
        calphal[2] = -calphal[2];
        salphal[2] = -salphal[2];
      }
    } else {
      cgammal[2] = sbn * sgn / hilfsg;
      sgammal[2] = cbn / hilfsg;
      calphal[2] = -(can * cbn * cgn - san * sgn) / hilfsg;
      salphal[2] = -(san * cbn * cgn + can * sgn) / hilfsg;
    }

/*    CYCLES OF SIGNS FOR THE 96 CASES */

    for (i = 1; i <= 3; ++i) {
      for (k = 1; k <= 8; ++k) {
        j = (k - 1) * 3;
        i__1 = k + 1;
        ik11 = MoreMath.powint(-1, i__1);
        i__1 = k / 2;
        ik02 = MoreMath.powint(-1, i__1);
        for (l = 1; l <= 4; ++l) {
          m = (l - 1) * 24;
          sbetal[m + j + i - 1] = sbetal[i - 1];
          i__1 = l + 1;
          il11 = MoreMath.powint(-1, i__1);
          i__1 = l / 2;
          il02 = MoreMath.powint(-1, i__1);
          salphal[m + j + i - 1] = il11 * ik11 * salphal[i - 1];
          calphal[m + j + i - 1] = il02 * ik11 * calphal[i - 1];
          i__1 = (l + 3) / 2;
          il32 = MoreMath.powint(-1, i__1);
          cbetal[m + j + i - 1] = il32 * ik11 * cbetal[i - 1];
          if (ik02 > 0) {
            i__1 = (k + 7) / 4;
            ik74 = MoreMath.powint(-1, i__1);
            i__1 = (k + 1) / 4;
            ik14 = MoreMath.powint(-1, i__1);
            sgammal[m + j + i - 1] = ik14 * il32 * sgammal[i - 1];
            cgammal[m + j + i - 1] = ik74 * il32 * cgammal[i - 1];
          } else {
            i__1 = (k + 5) / 4;
            ik54 = MoreMath.powint(-1, i__1);
            i__1 = (k + 3) / 4;
            ik34 = MoreMath.powint(-1, i__1);
            sgammal[m + j + i - 1] = ik54 * il32 * cgammal[i - 1];
            cgammal[m + j + i - 1] = ik34 * il32 * sgammal[i - 1];
          }
        }
      }
    }
  } /* sc96 */

//	int marefuo_(int nnpolex, int nfmaref, int maref[], int ntmin, int ntmax) {
  int marefuo_(int nnpolex, int nfmaref, int ntmin, int ntmax) {

/* Local variables */
    int ntet, n2wbg, lbein;
    int nb;
    double xi, yi, zi;
    int ny0;
    int nga;


/*       Identical with MAREFU, only NPOLEX --> NNPOLEX */

/*       Variant with cell "centres" of the boarder cells at the boarder */

/*       NPRH=NYZWBG/20 */
/*                            equivalent hi-directions */

/* Function Body */
    for (lbein = 1; lbein <= mizmhi[nnpolex - 1]; ++lbein) {
      xi = hmst[lbein - 1] * hmcf[lbein - 1];
      yi = hmst[lbein - 1] * hmsf[lbein - 1];
      zi = hmct[lbein - 1];
/*                                     g-loop in the working area */
      for (n2wbg = 1; n2wbg <= nyzwbg; ++n2wbg) {
        nga = (n2wbg - 1) / nyzb + 1;
        nb = n2wbg - nyzb * (nga - 1);
        int ntetv[] = nyadu0_(xi, yi, zi, nb, nga);
        ntet = ntetv[0];
        ny0 = ntetv[1];
        if (ntet < ntmin || ntet > ntmax) {
          ny0 = -ny0;
        }
        cpolex_21[nfmaref - 1][n2wbg - 1] = ny0;
//	    	maref[n2wbg] = ny0;
      }
//    	backup21(nfmaref, maref, nyzwbg);
/*                       STEP NFMAREF */
      ++nfmaref;
    }
    return nfmaref;
  } /* marefuo_ */


  int[] nyadu0_(double xi, double yi, double zi, int nb, int ngam) {
/* Local variables */
    double x, y, z, cb, cg, fi;
    int nf;
    double sb, sg, st;
    int nt1;
    double tet;
    int[] ntet = new int[2];

/*   Determination of the NY adress in the upper halfsphere for */
/*   g(-1)*hmi given by XI,YI,ZI. ALPHA=0. */

    sb = sr[nb - 1];
    cb = cr[nb - 1];
    sg = sr[ngam - 1];
    cg = cr[ngam - 1];
    x = cb * cg * xi - cb * sg * yi + sb * zi;
    y = sg * xi + cg * yi;
    z = -sb * cg * xi + sb * sg * yi + cb * zi;
    if (z >= .999999) {
      tet = 0.;
      fi = 0.;
    } else {
      if (z <= -.999999) {
        tet = Constants.PI;
        fi = 0.;
      } else {
        tet = Math.acos(z);
        st = Math.sin(tet);
        x /= st;
        y /= st;
        if (x - 1. >= 0.)
          fi = 0.;
        else {
          if (x <= -1.)
            fi = Constants.PI;
          else
            fi = Math.acos(x);
          if (y < 0.)
            fi = Constants.PI2 - fi;
        }
      }
    }
/*                                               reduced pole figures */

    if (tet > pi2g) {
      tet = Constants.PI - tet;
      fi = Constants.PI + fi;
      if (fi >= Constants.PI2)
        fi -= Constants.PI2;
    }
/*                                 TET FI IN UPPER HALFSPHERE IN RADIAN */
    nt1 = (int) ((tet - pi25g) / pi5g + 1.000001);
    ntet[0] = nt1 + 1;
    nf = (int) ((fi - pi25g) / pi5g + 2.000001);
    ntet[1] = nt1 * alphamax + nf;
    return ntet;
  } /* nyadu0_ */


//	int siguo_(int maref[]) {
  int siguo_() {

/* Local variables */
    double beta;
    int ngam;
    int[] hmis = new int[old2701max];
    double wght;
    int n2wbg;
    double gamma;
    int hmish;
    int izmhi, nover, nb, npolex;
    int ny0;
    int mhi;
    int[] hit = new int[old2701max];
    int nfmaref, nnpolex, izoveri;
    int ihitctr = 0;

/*                                      OVERLAPPING of reflexes              */

/*  Determines for a g-cell of the working area the number of hits into      */
/*  the measured PF regions.                                                 */
/*  Calculation of the array GJEX(g)  (= 1/IZPOL for complete PF's)          */
/*  Because of the girdle structure of the measured PF regions only          */
/*  the Alpha=0 sections are considered. The hit distributions in the        */
/*  other alpha sections are identical.                                      */

/*  HIT - Hitnumber for all pole figures - pure geometric information.       */
/*  It is equal to the number of all Beine in case of complete pole figure   */
/*  measurements.                                                            */
/* HMIS - Hitnumber for all Beine of a reflex in an overlapped pole figure.  */

/* For incomplete pole figures 1/GJEX(g) =< IZPOL. It is a g-Hit depending   */
/* global weight of the sharpness of the correcting factor for the given g,  */
/* and shall approximately provide the same sharpness for all g, independend */
/* of its hitnumber.                                                         */

/* Function Body */

    for (n2wbg = 1; n2wbg <= nyzwbg; ++n2wbg) {
      gjex[n2wbg - 1] = 0.;
      hit[n2wbg - 1] = 0;
    }
    nfmaref = 0;
    nnpolex = 0;
/*                                                pole figure loop */

    for (npolex = 1; npolex <= izpol; ++npolex) {
      izoveri = moveri[npolex - 1];
/*                                         overlapping reflex loop */

      for (nover = 1; nover <= izoveri; ++nover) {
        for (n2wbg = 1; n2wbg <= nyzwbg; ++n2wbg) {
          hmis[n2wbg - 1] = 0;
        }
        ++nnpolex;
        wght = weight[nnpolex - 1];
        izmhi = mizmhi[nnpolex - 1];
/*                                                 Bein loop */

        for (mhi = 1; mhi <= izmhi; ++mhi) {
          ++nfmaref;
//      		restore21(nfmaref, maref, nyzwbg);
          for (n2wbg = 1; n2wbg <= nyzwbg; ++n2wbg) {
            ny0 = cpolex_21[nfmaref - 1][n2wbg - 1];//maref[n2wbg];
/*                               Hit criterion */
            if (ny0 > 0) {
              hmis[n2wbg - 1] += 1;
            }
          }
        }
        for (n2wbg = 1; n2wbg <= nyzwbg; ++n2wbg) {
          hmish = hmis[n2wbg - 1];
          gjex[n2wbg - 1] += hmish * wght / izmhi;
          hit[n2wbg - 1] += hmish;
        }

      }
    }

/*                                                 HIT,GJEX  Print */

    for (n2wbg = 1; n2wbg <= nyzwbg; ++n2wbg) {
      gjex[n2wbg - 1] = 1. / gjex[n2wbg - 1];
    }

/*                                                  HIT CONTROL */

    ihitctr = 1;
    for (n2wbg = 1; n2wbg <= nyzwbg; ++n2wbg) {
      ngam = (n2wbg - 1) / nyzb + 1;
      gamma = (double) ifiw[ngam - 1];
      nb = n2wbg - nyzb * (ngam - 1);
      beta = (double) ifiw[nb - 1];
//      System.out.println("Hit #: " + hit[n2wbg - 1]
//                           + ", beta: " + beta + ", gamma: " + gamma);
      if (hit[n2wbg - 1] < 3.) {
        ihitctr = 0;
//				return ihitctr;
      }
    }
    return ihitctr;
  } /* siguo_ */


  void anfkor_() {

/* Local variables */
    double suvp, s, helpp, helpv;
    int ny, npolex;
    double fak;

/*  INPUT POLEX ; OUTPUT POLREF = "corrected" POLEX in accordance */
/*  with an estimation of the normalization in the measured regions and */
/*  with symbolic negative values in the unmeasured regions. */

    for (npolex = 1; npolex <= izpol; ++npolex) {
/*                                                    READ POLEX */
      for (ny = 0; ny < old1387max; ++ny)
        polex[ny] = cpolex_23[npolex - 1][ny];
//      restore23(npolex, polex);
      suvp = 0.;
      s = 0.;
      for (ny = 0; ny < old1387max; ++ny) {
        helpp = polex[ny];
        if (helpp > 0.) {
          helpv = vp[ny];
          suvp += helpv;
          s += helpp * helpv;
        }
      }
      uvp[npolex - 1] = suvp;
      fak = s / suvp;
      fakk[npolex - 1] = fak;
      for (ny = 1; ny <= old1387max; ++ny) {
        helpp = polex[ny - 1];
/*                      Einschreiben negativer Werte in POLREF */
        polref[ny - 1] = helpp;
        if (helpp > 0.) {
          polref[ny - 1] = helpp / fak;
        }
      }
      for (ny = 0; ny < old1387max; ++ny)
        cpolex_20[npolex - 1][ny] = polref[ny];
//    	backup20(npolex, polref);
    }
  } /* anfkor_ */


//	void fanfuo_(int maref[]) {
  void fanfuo_() {

/* Local variables */
    int ngbg;
    double /*hjex, */wght;
    int n2wbg;
    double p, helpp;
    int izmhi, nover;
    double fh;
    int na, ng;
    double ex;
    int ny, nahelp, nf0, npolex, nt1, ny0, mhi, nfmaref, nyt, nnpolex, izoveri;


/*                                        Overlapping of reflexes */

/*  Calculation of the "Product function" sharpened by (   )**RFANF */
/*  NO normalization */
/*  INPUT POLREF from ANFKOR, OUTPUT F */

/* Function Body */
    for (n2wbg = 0; n2wbg < nyzwbg; ++n2wbg) {
      ngbg = n2wtobg[n2wbg];
      for (na = 0; na < alphamax; ++na) {
        f[na + ngbg] = 1.f;
      }
    }
/*                                            Pole figure loop */
    nfmaref = 0;
    nnpolex = 0;
    for (npolex = 1; npolex <= izpol; ++npolex) {
      izoveri = moveri[npolex - 1];
      for (ny = 0; ny < old1387max; ++ny)
        polref[ny] = cpolex_20[npolex - 1][ny];
//    	restore20(npolex, polref);
/*                                            overlapping reflex loop
*/
      for (nover = 1; nover <= izoveri; ++nover) {
        ++nnpolex;
        wght = weight[nnpolex - 1];
        izmhi = mizmhi[nnpolex - 1];
        ex = rfanf * wght / izmhi;
        for (ny = 0; ny < old1387max; ++ny) {
          p = polref[ny];
          polf[ny] = p;
/*  POLF contains all true zeros and negative symbolical numbers */
          if (p > 0.)
            polf[ny] = Math.pow(p, ex);
        }
        for (mhi = 1; mhi <= izmhi; ++mhi) {
/*                                  Bein, READ MAREF */
          ++nfmaref;
//        	restore21(nfmaref, maref, nyzwbg);
/*                                    g working region */
          for (n2wbg = 1; n2wbg <= nyzwbg; ++n2wbg) {
            ngbg = n2wtobg[n2wbg - 1];
            ny0 = cpolex_21[nfmaref - 1][n2wbg - 1]; //maref[n2wbg];
            if (ny0 >= 0) {
              nt1 = (ny0 - 1) / alphamax;
              nyt = m73[nt1];
              nf0 = ny0 - nyt;
              for (na = 1; na <= alphamax; ++na) {
                ng = na + ngbg;
                fh = f[ng - 1];
                if (fh != 0.) {
                  nahelp = na - 1 + nf0;
                  if (nahelp > alphamax) {
                    nahelp += -alphamax1;
                  }
                  ny = nyt + nahelp;
/*                                transfer of the  PF zeros into F */
                  helpp = polf[ny - 1];
//                if (ng-1 == 0)
//                  System.out.println("ny = " + ny + ", npolex = " + npolex);
                  if (helpp >= 0.)
                    f[ng - 1] = (double) (fh * helpp);
                }
              }
            }
          }
        }
      }
    }
    for (n2wbg = 0; n2wbg < nyzwbg; ++n2wbg) {
      for (na = 0; na < alphamax; ++na) {
        ng = n2wtobg[n2wbg] + na;
        if (f[ng] != 0.) {
          f[ng] = (double) Math.pow(f[ng], gjex[n2wbg]);
        }
      }
    }
/*                                                  FRASYM */
    frasycdt_();
  } /* fanfuo_ */

/* new fanfuo_
  void fanfuo_() {

//                                        Overlapping of reflexes
//  Calculation of the "Product function" sharpened by (   )**RFANF
//  NO normalization
//  INPUT POLREF from ANFKOR, OUTPUT F

    // Function Body
    for (int n2wbg = 0; n2wbg < nyzwbg; ++n2wbg)
      for (int na = 0; na < alphamax; ++na)
        f[na + n2wtobg[n2wbg]] = 1.;
//                                            Pole figure loop
    int nfmaref = -1;
    int nnpolex = -1;
    for (int npolex = 0; npolex < izpol; ++npolex) {
      for (int ny = 0; ny < old1387max; ++ny)
        polref[ny] = cpolex_20[npolex][ny];
//                                            overlapping reflex loop
      for (int nover = 0; nover < moveri[npolex]; ++nover) {
        ++nnpolex;
        double ex = rfanf * weight[nnpolex] / mizmhi[nnpolex];
        for (ny = 0; ny < old1387max; ++ny) {
          if (polref[ny] > 0.)
            polf[ny] = Math.pow(polref[ny], ex);
          else
            polf[ny] = polref[ny];
        }
        for (int mhi = 0; mhi < mizmhi[nnpolex]; ++mhi) {
          ++nfmaref;
          for (int n2wbg = 0; n2wbg < nyzwbg; ++n2wbg) {
            int ny0 = cpolex_21[nfmaref][n2wbg];
            if (ny0 >= 0) {
              int nt1 = (ny0 - 1) / alphamax;
              int nf0 = ny0 - m73[nt1];
              for (int na = 0; na < alphamax; ++na) {
                int ng = na + n2wtobg[n2wbg];
                if (f[ng] != 0.) {
                  int nahelp = na + nf0;
                  if (nahelp >= alphamax)
                    nahelp += -alphamax1;
                  ny = m73[nt1] + nahelp;
                  if (polf[ny] >= 0.)
                    f[ng] = f[ng] * polf[ny];
                }
              }
            }
          }
        }
      }
    }
    for (int n2wbg = 0; n2wbg < nyzwbg; ++n2wbg) {
      for (na = 0; na < alphamax; ++na) {
        ng = n2wtobg[n2wbg] + na;
        if (f[ng] != 0.)
          f[ng] = Math.pow(f[ng], gjex[n2wbg]);
      }
    }
//                                                  FRASYM
    frasycdt_();
  } // fanfuo_

*/
  public static int nczg[] = {1, 2, 2, 4, 4, 2, 4, 3, 3, 6, 6};
/*  int nyzg[] = { betamax,betamax,alphamax4,betamax,alphamax4,alphamax4,alphamax4,betamax,
	    alphamax4,betamax,alphamax4,alphamax,betamax,betamax,alphamax4,alphamax4,betamax,alphamax4,alphamax3,alphamax3,alphamax6,alphamax6 };*/
  public static int md[] = {0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1};

  int nyzg_ref(int a_1, int a_2) {
    return nyzgb[a_2 * 11 + a_1 - 12];
  }

/*	double fio_ref(int a_1, int a_2, int a_3) {
		return fio[(a_3 * 37 + a_2) * 73 + a_1 - 2775];
	}*/

  public static void fiottu_(double[] fl, int igbl, int alphama) {

/* Local variables */
    int lgam, /*mczh,*/ nczh, ngamstep,
            na, nb, ng, /*mastep,*/ ngstep, mdb, nbe, nae, nge,
            nav, nbv, /*mcz,*/ ncz, ngv;

/* 	COMPLETES THE ARRAY FIO TO THE FULL G-SPACE DIMENSION */
/*       USING THE LEFTSIDED AND RIGHTSIDED SYMMETRY GROUPS GB,GA */
/*       AND THE INPUT VALUES FIO IN THE REGION  : */
/*                  NA=NAA,NAEE (cf. 2)) ; NB=1,NBE ; NG=1,NGE */

/*       Always 5 degree steps */

/*       All 11 rotation groups are possible for GA or GB, but in */
/*       accordance with the WIMV-convention for elementary regions */
/*       in the G-space there is a asymmetry concerning GA and GB. */

/*    1) The working arrays consider C and D groups only, i.e. cubic */
/*       groups are considered in 3 times larger regions. If GA and GB */
/*       are cubic groups so in a 9 times larger region correspondingly.
*/

/*    2) Alpha region : 0 - 2PI/m for Cm and 0 - PI/m for Dm. I.e. only GA
 */
/*       acts on Alpha. Attention !! there is one exclusion : For GA=D3 */
/*       and GB=Dn (n.GT.1) PI/6 .LE. Alpha .LE. PI/2 is to be used ; */
/*       (NAA=7, NAEE=19). */

/*    3) Gamma region : 0 - 2PI/n (for Cn and Dn), i.e. only GB acts on */
/*       Gamma. */

/*    4) Beta region : 0 - PI (GB = Cn) and 0 - PI/2 (GB = Dn), i.e. again
 */
/*       only GB acts. */


/*      SYMMETRY GROUP  : O   T  D4  C4  D2  C2  C1  D6  C6  D3  C3 */
/*                       432 23  422  4  222  2   1  622  6  322  3 */

/*      CODE NUMBER     : 7   6   5   4   3   2   1  11  10   9   8 */
/*       IGA or IGB */

/*        COMMON/CFIO/FIO(73,37,73) */

    int betama = (alphama - 1) / 2 + 1;
    int betamad2 = (betama - 1) / 2 + 1;
    int alphamad3 = (alphama - 1) / 3 + 1;
    int alphamad6 = (alphama - 1) / 6 + 1;

    int nyzgl[][] = {{betama, betama, betamad2, betama, betamad2, betamad2, betamad2, betama, betamad2, betama, betamad2},
                     {alphama, betama, betama, betamad2, betamad2, betama, betamad2, alphamad3, alphamad3, alphamad6, alphamad6}};

    mdb = md[igbl - 1];
    nae = nyzgl[1][0];
    nbe = nyzgl[0][igbl - 1];
    nge = nyzgl[1][igbl - 1];
//    mcz = nczg[0];
    ncz = nczg[igbl - 1];
//    mczh = mcz - 1;
    nczh = ncz - 1;
//    mastep = nae - 1;
    ngstep = nge - 1;

    int m2775 = alphama * (betama + 1) + 1;

/*  COMPLETITION OF THE GAMMA REGION USING SYMMETRY GROUP GB */

/*  Symmetry element CnZB : ALPHA,BETA,GAMMA+2PI/n */

    if (igbl != 1) {
      for (lgam = 1; lgam <= nczh; ++lgam) {
        ngamstep = ngstep * lgam;
        for (ng = 1; ng <= nge; ++ng) {
          ngv = ngamstep + ng;
          for (na = 1; na <= nae; ++na) {
            for (nb = 1; nb <= nbe; ++nb) {
              fl[(ngv * betama + nb) * alphama + na - m2775] =
                      fl[(ng * betama + nb) * alphama + na - m2775];
            }
          }
        }
      }

/*  GAMMA REGION 0-360 degrees IS COMPLETE */

      if (mdb != 0) {
/*  For GB = Dn : Symmetry element C2XB */
/*                PI+ALPHA, PI-BETA, 2PI-GAMMA */

        for (nb = 1; nb <= betamad2; ++nb) {
          nbv = betama + 1 - nb;
          for (ng = 1; ng <= alphama; ++ng) {
            ngv = alphama + 1 - ng;
            for (na = 1; na <= nae; ++na) {
              nav = na + betama - 1;
              if (nav > alphama) {
                nav += -(alphama - 1);
              }
              fl[(ngv * betama + nbv) * alphama + nav - m2775] = fl[(ng * betama + nb) * alphama + na - m2775];
            }
            if (nae >= betama) {
              fl[(ngv * betama + nbv) * alphama + 1 - m2775] = fl[(ngv * betama + nbv) * alphama + alphama - m2775];
            }
          }
        }
      }
    }

/*  G-SPACE IS COMPLETE */

  } /* fiottu_ */

  public static int getAlphamax(double res) {
    return (int) (360.0 / res + 1.00001);
  }

  public static int getBetamax(double res) {
    return (int) (180.0 / res + 1.00001);
  }

  public static double[][][] ODFinputBeartex(String filename) {
    return ODFinputBeartexOld(filename, -1, 5.0);
  }

  public static double[][][] ODFinputBeartexOld(String filename, int igbl, double res) {
    double[][][] odfl;

    int alphama = getAlphamax(res);

    int betama = (alphama - 1) / 2 + 1;
    int betamad2 = (betama - 1) / 2 + 1;
    int alphamad3 = (alphama - 1) / 3 + 1;
 //   int alphamad4 = (alphama - 1) / 4 + 1;
    int alphamad6 = (alphama - 1) / 6 + 1;

    odfl = new double[alphama][betama][alphama];

    int nyzgl[][] = {{betama, betama, betamad2, betama, betamad2, betamad2, betamad2, betama, betamad2, betama, betamad2},
                     {alphama, betama, betama, betamad2, betamad2, betama, betamad2, alphamad3, alphamad3, alphamad6, alphamad6}};

    BufferedReader PFreader = Misc.getReader(filename);

    if (PFreader != null) {
      try {

        String line = null;
        String token = null;
        String stringnumber = null;

        for (int i = 0; i < 2; i++)
          line = PFreader.readLine();
        StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");

        token = st.nextToken();
        int igbr = igbl;
        if (token != null)
          igbr = Integer.valueOf(token).intValue();
        if (igbl == -1)
          igbl = igbr;
        double resr = 5.0;
        int nae = nyzgl[1][0];
        int nbe = nyzgl[0][igbl - 1];
        int nge = nyzgl[1][igbl - 1];


//        while (line.indexOf("GAMMA") < 0) {
//          line = PFreader.readLine();
//        }

        if (igbr == igbl && resr == res) {
          line = PFreader.readLine();
          line = PFreader.readLine();
          st = new StringTokenizer(line, " ,\t\r\n");
          for (int ng = 0; ng < nge; ng++) {
            for (int nb = 0; nb < nbe; nb++) {
              for (int na = 0; na < nae; na++) {
                if (!st.hasMoreTokens()){
                  line = PFreader.readLine();
                  st = new StringTokenizer(line, " ,\t\r\n");
                }
                odfl[na][nb][ng] = Float.parseFloat(st.nextToken());
              } /* alpha angles */
              line = PFreader.readLine();
              st = new StringTokenizer(line, " ,\t\r\n");
            } /* beta sections */
            line = PFreader.readLine();
            if (line != null)
              st = new StringTokenizer(line, " ,\t\r\n");
          } /* gamma sections */

        } else {

          System.out.println("Crystal symmetry or resolution not corresponding!");

          for (int ng = 0; ng < nge; ng++)
            for (int nb = 0; nb < nbe; nb++)
              for (int na = 0; na < nae; na++)
                odfl[na][nb][ng] = 1.0f;
        }
        PFreader.close();
        Uwimvuo.fiottu(odfl, igbl, alphama);
      } catch (IOException io) {

        for (int ng = 0; ng < alphama; ng++)
          for (int nb = 0; nb < betama; nb++)
            for (int na = 0; na < alphama; na++)
              odfl[na][nb][ng] = 1.0f;
      }
    } else {
      for (int ng = 0; ng < alphama; ng++)
        for (int nb = 0; nb < betama; nb++)
          for (int na = 0; na < alphama; na++)
            odfl[na][nb][ng] = 1.0f;
    }

    return odfl;
  }

  public static double[][][] ODFinputBeartex(String filename, int igbl, double res) {
    double[][][] odfl;

    int alphama = getAlphamax(res);

    int betama = (alphama - 1) / 2 + 1;
    int betamad2 = (betama - 1) / 2 + 1;
    int alphamad3 = (alphama - 1) / 3 + 1;
 //   int alphamad4 = (alphama - 1) / 4 + 1;
    int alphamad6 = (alphama - 1) / 6 + 1;

    odfl = new double[alphama][betama][alphama];

    int nyzgl[][] = {{betama, betama, betamad2, betama, betamad2, betamad2, betamad2, betama, betamad2, betama, betamad2},
                     {alphama, betama, betama, betamad2, betamad2, betama, betamad2, alphamad3, alphamad3, alphamad6, alphamad6}};

    BufferedReader PFreader = Misc.getReader(filename);

    if (PFreader != null) {
      try {

        String line = null;
        String token = null;
        String stringnumber = null;

        for (int i = 0; i < 3; i++)
          line = PFreader.readLine();
        StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");

        for (int i = 0; i < 7; i++)
          if (st.hasMoreTokens())
            token = st.nextToken();
        int igbr = igbl;
        if (token != null)
          igbr = Integer.valueOf(token).intValue();
        if (igbl == -1)
          igbl = igbr;
        double resr = 5.0;
        int nae = nyzgl[1][0];
        int nbe = nyzgl[0][igbl - 1];
        int nge = nyzgl[1][igbl - 1];


//        while (line.indexOf("GAMMA") < 0) {
//          line = PFreader.readLine();
//        }

        if (igbr == igbl && resr == res) {

          int na = 0;
          int ng = -1;
          int natmp = na;
          while (line != null) {
//            System.out.println(ng);
            while (line != null && !line.startsWith(" BETA")) {
              line = PFreader.readLine();
//              System.out.println("Gamma: " + line);
              if (line != null && line.indexOf("GAMMA") > 0) {
                na = 0;
                ng++;
              }
            }
            if (line == null)
              break;
            line = PFreader.readLine();
//              System.out.println("tmp: " + line);
            int nb = 0;
            while (line != null && line.indexOf("|") >= 0) {
              natmp = na;
//              System.out.println("natmp " + natmp + " " + nb);
              int startindex = 5;
              int endindex = 5;
//                  System.out.println(alphamad4);
              while (startindex + 6 <= line.length()) {
                endindex = startindex + 6;
                stringnumber = line.substring(startindex, endindex);
                startindex = endindex;
                if (nb < nbe && natmp < nae && ng < nge) {
                  odfl[natmp][nb][ng] = Float.valueOf(stringnumber).floatValue();
//                  System.out.println(natmp + " " + nb + " " + ng + " " + odfl[natmp][nb][ng]);
                } else {
//                  System.out.println(natmp + " " + nb + " " + ng);
                }
                natmp++;
              } /* alpha angles */
              line = PFreader.readLine();
//              System.out.println(line);
//              System.out.flush();
              nb++;
            } /* beta sections */
            na = natmp - 1;
          } /* gamma sections */

        } else {

          System.out.println("Crystal symmetry or resolution not corresponding!");

          for (int ng = 0; ng < nge; ng++)
            for (int nb = 0; nb < nbe; nb++)
              for (int na = 0; na < nae; na++)
                odfl[na][nb][ng] = 1.0f;
        }
        PFreader.close();
        Uwimvuo.fiottu(odfl, igbl, alphama);
      } catch (IOException io) {

        for (int ng = 0; ng < alphama; ng++)
          for (int nb = 0; nb < betama; nb++)
            for (int na = 0; na < alphama; na++)
              odfl[na][nb][ng] = 1.0f;
      }
    } else {
      for (int ng = 0; ng < alphama; ng++)
        for (int nb = 0; nb < betama; nb++)
          for (int na = 0; na < alphama; na++)
            odfl[na][nb][ng] = 1.0f;
    }

    return odfl;
  }

  public static void fiottu(double[][][] fl, int igbl, int alphama) {

/* Local variables */
    int lgam, nczh, ngamstep,
            na, nb, ng, ngstep, mdb, nbe, nae, nge,
            nav, nbv, /*mcz, */ncz, ngv;

/* 	COMPLETES THE ARRAY FIO TO THE FULL G-SPACE DIMENSION */
/*       USING THE LEFTSIDED AND RIGHTSIDED SYMMETRY GROUPS GB,GA */
/*       AND THE INPUT VALUES FIO IN THE REGION  : */
/*                  NA=NAA,NAEE (cf. 2)) ; NB=1,NBE ; NG=1,NGE */

    int betama = (alphama - 1) / 2 + 1;
    int betamad2 = (betama - 1) / 2 + 1;
    int alphamad3 = (alphama - 1) / 3 + 1;
    int alphamad6 = (alphama - 1) / 6 + 1;

    int nyzgl[][] = {{betama, betama, betamad2, betama, betamad2, betamad2, betamad2, betama, betamad2, betama, betamad2},
                     {alphama, betama, betama, betamad2, betamad2, betama, betamad2, alphamad3, alphamad3, alphamad6, alphamad6}};
    mdb = md[igbl - 1];
    nae = nyzgl[1][0];
    nbe = nyzgl[0][igbl - 1];
    nge = nyzgl[1][igbl - 1];
    ncz = nczg[igbl - 1];
    nczh = ncz - 1;
    ngstep = nge - 1;

/*  COMPLETITION OF THE GAMMA REGION USING SYMMETRY GROUP GB */

/*  Symmetry element CnZB : ALPHA,BETA,GAMMA+2PI/n */

    if (igbl != 1) {
      for (lgam = 1; lgam <= nczh; ++lgam) {
        ngamstep = ngstep * lgam;
        for (ng = 1; ng <= nge; ++ng) {
          ngv = ngamstep + ng;
          for (na = 1; na <= nae; ++na) {
            for (nb = 1; nb <= nbe; ++nb) {
              fl[na - 1][nb - 1][ngv - 1] = fl[na - 1][nb - 1][ng - 1];
            }
          }
        }
      }

/*  GAMMA REGION 0-360 degrees IS COMPLETE */

      if (mdb != 0) {
/*  For GB = Dn : Symmetry element C2XB */
/*                PI+ALPHA, PI-BETA, 2PI-GAMMA */

        for (nb = 1; nb <= betamad2; ++nb) {
          nbv = betama + 1 - nb;
          for (ng = 1; ng <= alphama; ++ng) {
            ngv = alphama + 1 - ng;
            for (na = 1; na <= nae; ++na) {
              nav = na + betama - 1;
              if (nav > alphama) {
                nav += -(alphama - 1);
              }
              fl[nav - 1][nbv - 1][ngv - 1] = fl[na - 1][nb - 1][ng - 1];
            }
            if (nae >= betama) {
              fl[0][nbv - 1][ngv - 1] = fl[alphama - 1][nbv - 1][ngv - 1];
            }
          }
        }
      }
    }

/*  G-SPACE IS COMPLETE */

  } /* fiottu */

  public static void fiottum(double[][][] fl, int igbl, int alphama) {

/* Local variables */
    int lgam, /*mczh,*/ nczh, ngamstep,
            na, nb, ng,/* mastep,*/ ngstep, mdb, nbe, nae, nge,
            nav, nbv,/* mcz,*/ ncz, ngv;

/* 	COMPLETES THE ARRAY FIO TO THE FULL G-SPACE DIMENSION */
/*       USING THE LEFTSIDED AND RIGHTSIDED SYMMETRY GROUPS GB,GA */
/*       AND THE INPUT VALUES FIO IN THE REGION  : */
/*                  NA=NAA,NAEE (cf. 2)) ; NB=1,NBE ; NG=1,NGE */

    int betama = (alphama - 1) / 2 + 1;
    int betamad2 = (betama - 1) / 2 + 1;
    int alphamad3 = (alphama - 1) / 3 + 1;
    int alphamad6 = (alphama - 1) / 6 + 1;

    int nyzgl[][] = {{betama, betama, betamad2, betama, betamad2, betamad2, betamad2, betama, betamad2, betama, betamad2},
                     {alphama, betama, betama, betamad2, betamad2, betama, betamad2, alphamad3, alphamad3, alphamad6, alphamad6}};
    mdb = md[igbl - 1];
    nae = nyzgl[1][0];
    nbe = nyzgl[0][igbl - 1];
    nge = nyzgl[1][igbl - 1];
//    mcz = nczg[0];
    ncz = nczg[igbl - 1];
//    mczh = mcz - 1;
    nczh = ncz - 1;
//    mastep = nae - 1;
    ngstep = nge - 1;


//    int m2775 = alphama * (betama + 1) + 1;

/*  COMPLETITION OF THE GAMMA REGION USING SYMMETRY GROUP GB */

/*  Symmetry element CnZB : ALPHA,BETA,GAMMA+2PI/n */

    if (igbl != 1) {
      for (lgam = 1; lgam <= nczh; ++lgam) {
        ngamstep = ngstep * lgam;
        for (ng = 1; ng <= nge; ++ng) {
          ngv = ngamstep + ng;
          if (ngv <= nge)
            for (na = 1; na <= nae; ++na) {
              for (nb = 1; nb <= nbe; ++nb) {
                if (fl[na - 1][nb - 1][ng - 1] >= 0)
                  fl[na - 1][nb - 1][ngv - 1] = fl[na - 1][nb - 1][ng - 1];
                else if (fl[na - 1][nb - 1][ngv - 1] >= 0)
                  fl[na - 1][nb - 1][ng - 1] = fl[na - 1][nb - 1][ngv - 1];
              }
            }
        }
      }

/*  GAMMA REGION 0-360 degrees IS COMPLETE */

      if (mdb != 0) {
/*  For GB = Dn : Symmetry element C2XB */
/*                PI+ALPHA, PI-BETA, 2PI-GAMMA */

        for (nb = 1; nb <= betamad2; ++nb) {
          nbv = betama + 1 - nb;
          if (nbv <= nbe && nb <= nbe) {
            for (ng = 1; ng <= alphama; ++ng) {
              ngv = alphama + 1 - ng;
              if (ngv <= nge && ng <= nge) {
                for (na = 1; na <= nae; ++na) {
                  nav = na + betama - 1;
                  if (nav > alphama) {
                    nav += -(alphama - 1);
                  }
                  if (nav <= nae) {
                    if (fl[na - 1][nb - 1][ng - 1] >= 0)
                      fl[nav - 1][nbv - 1][ngv - 1] = fl[na - 1][nb - 1][ng - 1];
                    else if (fl[nav - 1][nbv - 1][ngv - 1] >= 0)
                      fl[na - 1][nb - 1][ng - 1] = fl[nav - 1][nbv - 1][ngv - 1];
                  }
                }
                if (nae >= betama) {
                  if (fl[alphama - 1][nbv - 1][ngv - 1] >= 0)
                    fl[0][nbv - 1][ngv - 1] = fl[alphama - 1][nbv - 1][ngv - 1];
                  else if (fl[0][nbv - 1][ngv - 1] >= 0)
                    fl[alphama - 1][nbv - 1][ngv - 1] = fl[0][nbv - 1][ngv - 1];
                }
              }
            }
          }
        }
      }
    }

    for (na = 0; na < nae; ++na)
      for (ng = 1; ng < nge; ++ng) {
        int nan = na + ng;
        while (nan >= nae)
          nan -= nae - 1;
        fl[na][0][ng] = fl[nan][0][0];
        if (nbe == betama) {
          nan = na - ng;
          while (nan < 0)
            nan += nae - 1;
          fl[na][nbe - 1][ng] = fl[nan][nbe - 1][0];
        }
      }

  } /* fiottum */

  int nuz_vit;
  double sf_vit, hsm1_vit, hsm_vit;

  double[] fanfsuo_(double rp_it, double rp1) {

/* Local variables */
    double phonstepeps, cthi, fhir;
    int izpolrp1;
    double sthi, wght;
    int i;
    double s, hfakk;
    int nover;
    double s1;
//    double fh;
    int ng;
    double rfanfh = 0.0;
    int ny;
    int npolex, nnpolex, izoveri;
    double hex, rph, rph1;
    int inv;
    double /*vit,*/ corr;
    int izpolrp;
    double rptmp[] = new double[2];

/*                                             Overlapping reflexes */

/* INPUT : F - estimated (not normalized) starting ODF for the INPUT-RFANF
*/
/*  For IANFSU=0 : F normalization only. OUTPUT --> F. */
/*  For IANFSU.NE.0 : F normalization. The corrected EXPOLFs (in POLREF)
*/
/*  are putted into POL1 (commonly an array for the (doped) POLREFs of the
 */
/*  last iteration). */
/*  Calculation of the POLREF. IN PREFKO correction of the POLREF relative
 */
/*  to the intensity of the original POLEX in the measured region. */
/*  RP determination (POLREF<-->POLEX,POLREF<-->POL1). */
/*  Then begin of a cycle. */
/*  RFANF will be enlarged or lowered by .5 up to RP reaches a Minimum. */
/*  This determines the optimum RFANF and starting ODF. */
/*  OUTPUT always F POLREF and POL1. */
/*  All calculations in the whole G-space. */


/*  INPUT F, NORMALIZATION, F2 CALCULATION */

    i = 0;
    boolean loop_exit = false;
    double check = 0;

    do {
      double[] fnorm2 = fnormf2_(0, 1, f2, ent);
      f2 = fnorm2[0];
      ent = fnorm2[1];
      phonstepeps = .005;
      phoninp = subfmin_(f, totalmax);
      phonstep = phoninp + phonstepeps;

      for (npolex = 1; npolex <= izpol; ++npolex) {
        for (ny = 0; ny < old1387max; ++ny)
          cpolex_19[npolex - 1][ny] = cpolex_20[npolex - 1][ny];
//      	restore20(npolex, polref);
/*                                     Putting  (doped) POLREF into POL1 */
//      	backup19(npolex, polref);
      }
      if (ianfsu == 0) {
        rptmp[0] = rp_it;
        rptmp[1] = rp1;

        return rptmp;
      }

      rph = rp_it;
      rph1 = rp1;
/*                                                for RPVIT */
      nuz_vit = 0;
      sf_vit = 0.;
      s = 0.;
      s1 = 0.;
      izpolrp = izpol;
      izpolrp1 = izpol;
      nnpolex = 0;
/*                                                pole figure loop */
      for (npolex = 1; npolex <= izpol; ++npolex) {
        for (ny = 1; ny <= old1387max; ++ny) {
          polref[ny - 1] = 0.;
        }
        izoveri = moveri[npolex - 1];

        if (npolex == 1)
          System.out.println(" RFANF = " + Fmt.format(rfanf) + "   previous RP,RP1 : " +
                  Fmt.format(rp_it) + "," + Fmt.format(rp1) +
                  "    NPOLEX = " + Integer.toString(npolex));
/*                                      overlapping reflex loop */
        for (nover = 1; nover <= izoveri; ++nover) {
          wght = weight[nnpolex];
          sthi = asthi[nnpolex];
          cthi = acthi[nnpolex];
          fhir = afhir[nnpolex];
          inv = minv[nnpolex];
          ++nnpolex;

          calpolo_(sthi, cthi, fhir, inv);
          for (ny = 0; ny < old1387max; ++ny) {
            polref[ny] += wght * fs[ny];
          }
        }
/*                                                  READ  POLEX */
        for (ny = 0; ny < old1387max; ++ny)
          polex[ny] = cpolex_23[npolex - 1][ny];
//    		restore23(npolex, polex);
        hfakk = prefko_();
        fakk[npolex - 1] = hfakk;
/*                                       WRITE  POLREF  (doped) */
/* luy      WRITE(20,rec=NPOLEX)POLREF */
        for (ny = 0; ny < old1387max; ++ny) {
          cpolex_20[npolex - 1][ny] = polref[ny];
          pol1[ny] = cpolex_19[npolex - 1][ny];
        }
/*                                       READ  POL1    (doped) */
/* lux      READ(19,rec=NPOLEX)POL1 */
//    		restore19(npolex, pol1);

        rpvit_(hfakk);
        sm[npolex - 1] = hsm_vit;
        sm1[npolex - 1] = hsm1_vit;
        if (hsm_vit != awert)
          s += hsm_vit;
        else
          --izpolrp;
        if (hsm1_vit != awert)
          s1 += hsm1_vit;
        else
          --izpolrp1;
      }
/*                                     end pole figure loop*/

/*                                     from PREFKO */
/*                          from RPVIT */
//      vit = sf_vit * 100. / nuz_vit;
      rp_it = s / izpolrp;
      rp1 = s1 / izpolrp1;

/*                                  BEGIN  RFANF-OPTIMIZATION */

      if (i != 0) {
        if (irp == 0)
          check = getDifference(rp_it, rph);
        else
          check = getDifference(rp1, rph1);
      } else
        check = -1.;
      if ((i < 0 && check > 0) || (i > 1 && check > 0)) {
        for (ng = 0; ng < totalmax; ++ng)
          f[ng] = f0[ng];
        check = 0.;
      }
      if (check == 0)
        loop_exit = true;
      else {
        if (i > 0 && check > 0) {
          rp_it = rph;
          rp1 = rph1;
          i = -1;
          corr = -1.0;
        } else if (i >= 0) {
          ++i;
          corr = .5;
        } else {
          --i;
          corr = -.5;
        }
        if (check < 0) {
          rfanfh = rfanf;
          for (ng = 0; ng < totalmax; ++ng)
            f0[ng] = f[ng];
        }
        hex = (rfanf + corr) / rfanf;
        rfanf += corr;
        for (ng = 0; ng < totalmax; ++ng) {
          if (f[ng] != 0.)
            f[ng] = (double) Math.pow(f[ng], hex);
        }
      }
    } while (!loop_exit);

    fnormf2_(0, 1, f2, ent);
    rfanf = rfanfh;
    rptmp[0] = rp_it;
    rptmp[1] = rp1;

    return rptmp;
  } /* fanfsuo_ */

  double getDifference(double first, double second) {
    return first - second;
  }

  void calpolo_(double sthi, double cthi, double fhir, int inv) {
/* Local variables */
    double ffak, pfak, gams, bets;
    int nfis, nfiy, ntfs;
    int nals1, i;
    double s;
    int nbgam;
 //   double pinpf;
    int ntety, ivorz, nb;
    double sn;
    int ny;
    double ca2, cb2, sa2, g2r, gam;
    int nga, nal;
    double als;
    int iswitch;

/*     Calculation of a complete reduced pole figure */
/*     Normalization */
/*     INPUT FIO given in the whole G-space OUTPUT POLREF=FS */

    for (i = 0; i < old1387max; ++i) {
      fs[i] = 0.;
    }

/* Projection thread loop, Simpson integration */

    cb2 = cthi;
    g2r = Constants.PI - fhir;
    boolean checkL13 = false;
    boolean nextCheck = false;
    do {
      while (g2r < 0.) {
        g2r += Constants.PI2;
      }
      for (nfis = 1; nfis <= new181; ++nfis) {
        if (nfis == 1 || nfis == new181)
          iswitch = 2;
        else
          iswitch = 1;
        ivorz = MoreMath.powint(-1, nfis);
        ca2 = -cr2[nfis - 1];
        sa2 = sr2[nfis - 1];
        for (ntety = 1; ntety <= alphamax4; ++ntety) {
          ntfs = m73[ntety - 1];
          double[] angles = g20g100(ca2, sa2, cb2, sthi, cr[ntety - 1],
                  sr[ntety - 1]);
          als = angles[0];
          bets = angles[1];
          gams = angles[2];
          nb = (int) ((bets + pi75g) / pi5g + .000001);
          gam = gams + g2r;
          if (gam > Constants.PI2) {
            gam -= Constants.PI2;
          }
          nga = (int) ((gam + pi75g) / pi5g + .000001);
          nbgam = m73[nb - 1] + m2701[nga - 1];
/*                     here MGAPOC0 can be storaged or read. */
/*                       Thread : */
/*                       h k l  NG0=MGAPOC0(HI,NTETY,NFIS) */
/*                                19  * 181=3439 */
/*     NALS1=(ALS-PI25)/PI5+1 */
          nals1 = (int) ((als + pi25g) / pi5g + .0000001);
          for (nfiy = 1; nfiy <= alphamax; ++nfiy) {
            ny = ntfs + nfiy;
/*  	                               here then NG=NG0+NFIY */
/*  	                    but be carefull, see the next "IF" ! */
            nal = nals1 + nfiy;
            if (nal > alphamax) {
              nal += -alphamax1;
            }
/*    	 NG=NAL+NBGAM */
/*     FFAK=FIO(NG) */
            ffak = f[nal + nbgam - 1];
            if (ffak > phonstep) {
              ffak = (ffak - phoninp) * pisimg;
              switch (iswitch) {
                case 1:
                  if (ivorz < 0)
                    ffak *= 2.;
                  else
                    ffak *= 4.;
                case 2:
                  fs[ny - 1] += ffak;
              }
            }
          }
        }
      }
      for (ny = 0; ny < old1387max; ++ny) {
        fs[ny] += phoninp;
      }
      if (inv == 1 || nextCheck) {
        checkL13 = true;
      } else {
        nextCheck = true;
        cb2 = -cb2;
        g2r -= Constants.PI;
      }
    } while (!checkL13); /*goto L13; */

    if (inv != 1) {
      for (ny = 0; ny < old1387max; ++ny) {
        fs[ny] /= 2.;
      }
    }
/*                                          Normalization to PINPF */
    sn = 0.;
    for (ny = 1; ny <= old1387max; ++ny) {
      sn += fs[ny - 1] * vp[ny - 1];
    }
    pfak = Constants.PI2 / sn;
    for (ny = 1; ny <= old1387max; ++ny) {
      fs[ny - 1] *= pfak;
    }

/*  THETA = 90 degree : PHI and (PPHI+FI) are equivalent */

    s = (fs[old1387max - 2 * betamax1 - 1] + fs[old1387max - betamax1 - 1] + fs[old1387max - 1]) / 3.;
    fs[old1387max - 2 * betamax1 - 1] = s;
    fs[old1387max - betamax1 - 1] = s;
    fs[old1387max - 1] = s;
    for (ny = old1387max - 2 * betamax1; ny <= old1387max - betamax1; ++ny) {
      fs[ny - 1] = (fs[ny - 1] + fs[ny + betamax1 - 1]) * .5;
    }
    for (ny = old1387max - betamax1 + 1; ny <= old1387max; ++ny) {
      fs[ny - 1] = fs[ny - betamax];
    }

/* THETA=0 degree        all PHI are equivalent */

    s = 0.;
    for (ny = 1; ny <= alphamax; ++ny) {
      s += fs[ny - 1];
    }
    s /= alphamax;
    for (ny = 1; ny <= alphamax; ++ny) {
      fs[ny - 1] = s;
    }

/* PHI=0=360 degree */

    for (ny = alphamax + 1; ny <= old1387max - 2 * alphamax + 1; ny += alphamax) {
      fs[ny - 1] = (fs[ny - 1] + fs[ny + alphamax1 - 1]) * .5;
      fs[ny + alphamax1 - 1] = fs[ny - 1];
    }
  } /* calpolo_ */


  public static final double[] g20g100_(double ca2, double sa2, double cb2, double sb2, double cb1,
                                        double sb1) {
/* Local variables */
    double ca, cb, cg, sb, fak;

    double angles[] = new double[3];

/* Calculates the product of two rotations g=g2*g1, g1=(   0,BET1,0)
                                                    g2=(ALF2,BET2,0)
   OUTPUT in Radians
*/
    cb = cb1 * cb2 - sb1 * sb2 * ca2;
    if (cb < 1. && cb > -1.) {
      angles[1] = Math.acos(cb);
      sb = Math.sin(angles[1]);
      ca = (cb1 * ca2 * sb2 + sb1 * cb2) / sb;
      if (ca >= 1.) {
        angles[0] = 0.;
      } else {
        if (ca <= -1.) {
          angles[0] = Constants.PI;
        } else {
          angles[0] = Math.acos(ca);
        }
        if (sa2 * sb2 < 0.) {
          angles[0] = Constants.PI2 - angles[0];
        }
      }
      cg = (sb1 * ca2 * cb2 + cb1 * sb2) / sb;
      if (cg >= 1.) {
        angles[2] = 0.;
        return angles;
      } else {
        if (cg <= -1.) {
          angles[2] = Constants.PI;
        } else {
          angles[2] = Math.acos(cg);
        }
        if (sb1 * sa2 < 0.) {
          angles[2] = Constants.PI2 - angles[2];
        }
        return angles;
      }
    }
    if (cb >= 1.) {
      angles[1] = 0.;
      fak = 1.;
    } else {
      angles[1] = Constants.PI;
      fak = -1.;
    }
    angles[2] = 0.;
    ca = fak * (cb1 * ca2 * cb2 - sb1 * sb2);
    if (ca >= 1.) {
      angles[0] = 0.;
    } else {
      if (ca <= -1.) {
        angles[0] = Constants.PI;
      } else {
        angles[0] = Math.acos(ca);
      }
      if (fak * sa2 * cb2 < 0.) {
        angles[0] = Constants.PI2 - angles[0];
      }
    }
    return angles;
  } /* g20g100_ */

  public static final double[] g20g100(double ca2, double sa2,
                                       double cb2, double sb2,
                                       double cb1, double sb1) {
/* Local variables */
    double ca, cb, cg, sb, fak;

    double angles[] = new double[3];

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
        return angles;
      } else {
        if (cg <= -0.999999999999)
          angles[2] = Constants.PI;
        else
          angles[2] = Math.acos(cg);
        if (sb1 * sa2 < 0.)
          angles[2] = Constants.PI2 - angles[2];
        return angles;
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
    return angles;
  } /* g20g100 */

  double prefko_() {
    double s, s1;
    int ny;
    double vpp;

/*  Correction of POLREF using an estimation of the nomalizing factors. */
/*  INPUT,OUTPUT POLREF. In the unmeasured regions there are symbolical */
/*  negative values from POLEX. */


    s = 0.;
    s1 = 0.;
    for (ny = 0; ny < old1387max; ++ny) {
      if (polex[ny] > 0.) {
        vpp = vp[ny];
        s += polex[ny] * vpp;
        s1 += polref[ny] * vpp;
      }
    }
    double hfakk = s / s1;
    for (ny = 0; ny < old1387max; ++ny) {
      if (polex[ny] >= 0.) {
        polref[ny] *= hfakk;
      } else {
/*                Introduction of negative values into POLREF */
/*                                    Creation of "doped" POLREF's. */
        polref[ny] = polex[ny];
      }
    }
    return hfakk;
  } /* prefko_ */


  void rpvit_(double hfakk) {

/* Local variables */
    double absbeitr = 1.0, delta;
    int n1;
    double sh;
    int ny;
    double sh1; //, plr;
    int nyk;


/* Determines the contribution of a pole figure to the RP,RP1,VIT0 values.    */
/* INPUT : POLREF,POL1 (doped), negative symbolical values in the unmeasured  */
/* regions. POLEX - ORIGINAL                                                 */


    sh = 0.;
    sh1 = 0.;
    n1 = 0;
    nyk = 0;
    for (ny = 0; ny < old1387max; ++ny) {
      if (polex[ny] > 0.) {
        if (pol1[ny] > 0.) {
          ++nuz_vit;
/*      ABW=(PLR-BEI1)/BEI1 */
          sf_vit += Math.abs((polref[ny] - pol1[ny]) / pol1[ny]);
        }

/*  antidoping of polex-values for comparability with PEPS and 1. */

        delta = polex[ny] / hfakk;
        if (delta >= peps) {
          ++nyk;
/*      BEITR = (PLR-BEIEX)/BEIEX */
          absbeitr = Math.abs((polref[ny] - polex[ny]) / polex[ny]);
          sh += absbeitr;
        }
        if (delta > 0.999) {
          ++n1;
          sh1 += absbeitr;
        }
      }
    }
    hsm_vit = awert;
    if (nyk > 0) {
      hsm_vit = sh * 100. / nyk;
    }
    hsm1_vit = awert;
    if (n1 > 0) {
      hsm1_vit = sh1 * 100. / n1;
    }
    return;
  } /* rpvit_ */


  double[] fnormf2_(int iw, int if2, double f2, double ent) {

/* Local variables */
    int ngbg;
    double pinf, fvng;
    int ng0bg, n2wbg;
    double s, fnorm;
    int na, ng;
    double se, fng, vbg;
    int iswitch;
    double fnorm2[] = new double[2];

/*  INPUT  : F    IW=1    : F given in the working G-space region only */
/*                IW.NE.1 : F given in the whole G-space */
/*  OUTPUT : F    in the INPUT-region */
/*                for IF2.NE.0  F2 and Entropy calculation also. */
    iswitch = 1;
    if (if2 == 1) {
      iswitch = 2;
    }
    if (iw != 1) {
      pinf = Constants.PI * 8. * Constants.PI;
      s = 0.;
      for (ng0bg = 1; ng0bg <= old2701max; ++ng0bg) {
        vbg = vg0bg[ng0bg - 1];
        ngbg = m73[ng0bg - 1];
        for (na = 1; na <= alphamax; ++na) {
          s += f[ngbg + na - 1] * vbg * va[na - 1];
        }
      }
      fnorm = pinf / s;
      s = 0.;
      se = 0.;
      for (ng0bg = 1; ng0bg <= old2701max; ++ng0bg) {
        vbg = vg0bg[ng0bg - 1];
        ngbg = m73[ng0bg - 1];
        for (na = 1; na <= alphamax; ++na) {
          ng = ngbg + na;
          f[ng - 1] *= fnorm;
          if (iswitch == 2) {
            fng = f[ng - 1];
            if (fng > 0.) {
              fvng = fng * vbg * va[na - 1];
              s += fvng * fng;
              se -= fvng * Math.log(fng);
            }
          }
        }
      }
      if (if2 != 1) {
        fnorm2[0] = f2;
        fnorm2[1] = ent;

        return fnorm2;
      }
      f2 = s / pinf;
      ent = se / pinf;
      fnorm2[0] = f2;
      fnorm2[1] = ent;

      return fnorm2;
    }
/*                                             working region */

    pinf = pinfg;
    s = 0.;
    for (n2wbg = 1; n2wbg <= nyzwbg; ++n2wbg) {
      ngbg = n2wtobg[n2wbg - 1];
      vbg = vw0bg[n2wbg - 1];
      for (na = 1; na <= alphamax; ++na) {
        s += f[ngbg + na - 1] * vbg * va[na - 1];
      }
    }
    fnorm = pinf / s;
    s = 0.;
    se = 0.;
    for (n2wbg = 1; n2wbg <= nyzwbg; ++n2wbg) {
      ngbg = n2wtobg[n2wbg - 1];
      vbg = vw0bg[n2wbg - 1];
      for (na = 1; na <= alphamax; ++na) {
        ng = ngbg + na;
        f[ng - 1] *= fnorm;
        if (iswitch == 2) {
          fng = f[ng - 1];
          if (fng > 0.) {
            fvng = fng * vbg * va[na - 1];
            s += fvng * fng;
            se -= fvng * Math.log(fng);
          }
        }
      }
    }
    if (if2 != 1) {
      fnorm2[0] = f2;
      fnorm2[1] = ent;
      return fnorm2;
    }
    f2 = s / pinf;
    ent = se / pinf;
    fnorm2[0] = f2;
    fnorm2[1] = ent;
    return fnorm2;
  } /* fnormf2_ */

  public static final double subfmin_(double[] fl, int maxfindex) { // Translation completed: Luca

    double fmin;

/*   INPUT F given in the whole G-space, determination of its minimum */

    fmin = 1000.0;
    for (int ng = 0; ng < maxfindex; ++ng) {
//      double ff = Math.abs(fl[ng]);
      if (fl[ng] < fmin && fl[ng] >= 0.0)
        fmin = fl[ng];
    }
    return fmin;
  } /* subfmin_ */

  public static final double subfmin(double[][][] fl, int maxaindex) { // Translation completed: Luca

    double fmin;

/*   INPUT F given in the whole G-space, determination of its minimum */

    int maxbindex = maxaindex / 2 + 1;
    fmin = 1000.;
    for (int ng = 0; ng < maxaindex; ++ng)
      for (int nb = 0; nb < maxbindex; ++nb)
        for (int na = 0; na < maxaindex; ++na)
          if (fl[na][nb][ng] < fmin && fl[na][nb][ng] >= 0.0)
            fmin = fl[na][nb][ng];
    return fmin;
  } /* subfmin */

//	void iteruo_(int itp, int[] maref) {
  void iteruo_(int itp) {

/* Local variables */
    double phonstepeps;
    int ngbg;
    double fehl, cthi, fhir, hjex;
    int izpolrp1;
    double sthi, wght;
    int n2wbg;
    double p, s, hfakk;
    int izmhi, nover;
    double s1;
    double fh;
    int na;
    double fk;
    int ng;
    double ex;
    int ny, nahelp;
    int npolex, nf0;
    double rp1;
    int nt1, ny0;
    int ngb;
    double amp;
    int mhi;
    int inv;
    double plr;
    int nfmaref;
    double vit, plx;
    int nyt;
    int nnpolex, izoveri;
    int izpolrp;

/*  INPUT F, Therefrom calculation of POLREF's. */
/*  Detrmination of the current interpolation control parameters. */
/*  If there will be no inner iteration STOP (for ABPRO-,VITPRO- or */
/* RP- criteria) --> calculation of the next ODF approximation (normalized
).*/
/*  OUTPUT : F, POL1 (last POLREF's), POLREF (new ones) - all doped and */
/*  with negative symbolical values in the unmeasured regions. */
/*  WENN KEIN INNERER ABRUCH (ABPRO-, VITPRO-, RP- KRITERIUM) VORLIEGT */
/*  BERECHNUNG EINER NEUEN NORMIERTEN OVF */
/*  AUSGANG - F, POL1 (GEMERKT),POLREF (NEU) - JEWEILS K-KORRIGIERT */
/*  MIT NEGATIVEN SYMBOLWERTEN IN UNGEMESSENEN BEREICHEN */

/*  Calculation of the pole figures (POLF), correction ("doping") */
/*  by estimated normalization factors. */
/*  Determination of characteristic iteration parameters. */

/* Function Body */
    phonstepeps = .005;
    phoninp = subfmin_(f, totalmax);
    phonstep = phoninp + phonstepeps;

    System.out.println("       PHONINP = " + Fmt.format(phoninp) +
            "  PHONSTEPEPS = " + Fmt.format(phonstepeps));
/*                                                for RPVIT */
    nuz_vit = 0;
    sf_vit = 0.;
    s = 0.;
    s1 = 0.;
    izpolrp = izpol;
    izpolrp1 = izpol;
    nnpolex = 0;
/*                                           pole figure loop */
    for (npolex = 1; npolex <= izpol; ++npolex) {

      izoveri = moveri[npolex - 1];

      if (npolex == 1)
        System.out.println("     ITERU -------->     POLE FIGUREs Nr. " + Integer.toString(npolex) +
                "  to  " + Integer.toString(izpol));

      for (ny = 1; ny <= old1387max; ++ny) {
        polref[ny - 1] = 0.;
      }
/*                                        loop of overlapping reflexes
 */
      for (nover = 1; nover <= izoveri; ++nover) {
        ++nnpolex;
        wght = weight[nnpolex - 1];
        sthi = asthi[nnpolex - 1];
        cthi = acthi[nnpolex - 1];
        fhir = afhir[nnpolex - 1];
        inv = minv[nnpolex - 1];

        calpolo_(sthi, cthi, fhir, inv);
        for (ny = 1; ny <= old1387max; ++ny) {
          polref[ny - 1] += wght * fs[ny - 1];
        }
      }
/*                                     READ POLEX */
      for (ny = 0; ny < old1387max; ++ny)
        polex[ny] = cpolex_23[npolex - 1][ny];
//    	restore23(npolex, polex);

      hfakk = prefko_();
      fakk[npolex - 1] = hfakk;
/*                                     WRITE POLREF (doped) */
      for (ny = 0; ny < old1387max; ++ny)
        cpolex_20[npolex - 1][ny] = polref[ny];
//    	backup20(npolex, polref);
/*                                     READ  POL1 */
      for (ny = 0; ny < old1387max; ++ny)
        pol1[ny] = cpolex_19[npolex - 1][ny];
//    	restore19(npolex, pol1);

      rpvit_(hfakk);
      sm[npolex - 1] = hsm_vit;
      sm1[npolex - 1] = hsm1_vit;
      if (hsm_vit != awert)
        s += hsm_vit;
      else
        --izpolrp;
      if (hsm1_vit != awert)
        s1 += hsm1_vit;
      else
        --izpolrp1;
    }

    System.out.println("  F2 = " + Fmt.format(f2) +
            "  F2DIF = " + Fmt.format(f2dif) +
            " ENTROPIE = " + Fmt.format(ent));

    int endofline5 = 0;
    StringBuffer tmp = new StringBuffer(" NORMFAKs : ");
    for (npolex = 0; npolex < izpol; npolex++) {
      endofline5++;
      tmp.append(Misc.getDoubleStringFormatted(fakk[npolex], 5, 6));
      if (endofline5 == 5) {
        System.out.println(tmp.toString());
        endofline5 = 0;
        tmp = new StringBuffer(" NORMFAKs : ");
      }
    }
    if (endofline5 != 0)
      System.out.println(tmp.toString());

/*                                                   from COMMON */
/*                                                    from RPVIT */
    vit = sf_vit * 100. / nuz_vit;
    rp_it = s / izpolrp;
    rp1 = s1 / izpolrp1;

    System.out.println("Velocity of convergence " + Fmt.format(vit) +
            " per cent, RP = " + Misc.getDoubleStringFormatted(rp_it, 5, 6) +
            ", RP1 = " + Misc.getDoubleStringFormatted(rp1, 5, 6));

    endofline5 = 0;
    tmp = new StringBuffer("   RPFAKs : ");
    for (npolex = 0; npolex < izpol; npolex++) {
      endofline5++;
      tmp.append(Misc.getDoubleStringFormatted(sm1[npolex], 5, 6));
      if (endofline5 == 5) {
        System.out.println(tmp.toString());
        endofline5 = 0;
        tmp = new StringBuffer("   RPFAKs : ");
      }
    }
    if (endofline5 != 0)
      System.out.println(tmp.toString());

/*     New recalculated pole figures are determined */

    if (iaba_it - 1 == 0) {
      iabi_it = 1;
      return;
    }
/*                         EXIT for the STOP regime */

/*     Comparison of POLREF with POLEX, ABPRO-criterion */

    boolean exit_routine = true;
    for (npolex = 1; npolex <= izpol; ++npolex) {
/*                                   READ POLEX,POLREF */
//    	restore23(npolex, polex);
//    	restore20(npolex, polref);
      fk = fakk[npolex - 1];
      for (ny = 0; ny < old1387max; ++ny) {
        plx = cpolex_23[npolex - 1][ny];
        plr = cpolex_20[npolex - 1][ny];
        if (plx > 0. && (plx / fk) > peps) {
/*     BEITR=(PLR-PLX)/PLX */
          fehl = Math.abs((plr - plx) / plx) * 100.;
          if (abpro - fehl < 0.) {
            ny = old1387max;
            npolex = izpol + 1;
            exit_routine = false;
          }
        }
      }
    }

/* ABBRUCH INNEN */

    if (exit_routine) {
      iabi_it = 1;
      return;
    }

/*  Comparison of POLREF,POL1,POLEX */
/*     VITPRO-criterion */

    if (itp == 0) {
      vit = awert;
    }
    if (vit - vitpro >= 0.) {
      if (rp_it - hrp_it >= 0.) {
        if (izick == 0 || itp - 2 > 0) {
          iabi_it = -1;
        } else {
          irfon_it = 1;
        }
      } else {
/*                                       Putting POLREF into POL1 */
        for (npolex = 1; npolex <= izpol; ++npolex) {
          for (ny = 0; ny < old1387max; ++ny)
            cpolex_19[npolex - 1][ny] = cpolex_20[npolex - 1][ny];
//    		  restore20(npolex, polref);
//    		  backup19(npolex, polref);
        }

/*                                       Storage of the old F */

        for (ng = 1; ng <= totalmax; ++ng) {
          f0[ng - 1] = f[ng - 1];
          f[ng - 1] = 1.f;
        }

/*                Calculation of the new F (not normalized) */

        if (izick > 0) {
          if (itp - 1 <= 0) {
            r *= rfak;
          } else {
            if (f2dif * f2difh <= 0.) {
              r /= rfak;
            } else {
              amp = Math.abs(f2difh) - Math.abs(f2dif);
              if (amp <= 0.) {
                r /= rfak;
              }
            }
          }
        }
        nfmaref = 0;
        nnpolex = 0;
/*                              pole figur loop */
        for (npolex = 1; npolex <= izpol; ++npolex) {

          izoveri = moveri[npolex - 1];
          for (ny = 0; ny < old1387max; ++ny)
            polref[ny] = cpolex_20[npolex - 1][ny];

          for (ny = 0; ny < old1387max; ++ny)
            polex[ny] = cpolex_23[npolex - 1][ny];

          for (nover = 1; nover <= izoveri; ++nover) {
            ++nnpolex;
            wght = weight[nnpolex - 1];
            izmhi = mizmhi[nnpolex - 1];
            ex = r * wght / izmhi;
            for (ny = 1; ny <= old1387max; ++ny) {
              p = 1.;
              plx = polex[ny - 1];
              if (plx >= 0.) {
                plr = polref[ny - 1];
                p = 0.;
                if (plx != 0. && plr != 0.) {
                  p = Math.pow(plx / plr, ex);
                }
              }
              polf[ny - 1] = p;
            }

            for (mhi = 1; mhi <= izmhi; ++mhi) {
/*                                  Bein */
              ++nfmaref;

              for (n2wbg = 1; n2wbg <= nyzwbg; ++n2wbg) {
                ngbg = n2wtobg[n2wbg - 1];
                ny0 = cpolex_21[nfmaref - 1][n2wbg - 1];
                if (ny0 >= 0) {
                  nt1 = (ny0 - 1) / alphamax;
                  nyt = m73[nt1];
                  nf0 = ny0 - nyt;
                  for (na = 1; na <= alphamax; ++na) {
                    ng = na + ngbg;
                    if (f0[ng - 1] == 0.f) {
                      f[ng - 1] = 0.f;
                    }
                    fh = f[ng - 1];
                    if (fh != 0.) {
                      nahelp = na - 1 + nf0;
                      if (nahelp > alphamax) {
                        nahelp += -(alphamax - 1);
                      }
                      ny = nyt + nahelp;
                      if (polex[ny - 1] >= 0.) {
                        f[ng - 1] = (double) (fh * polf[ny - 1]);
                      }
                    }
                  }
                }
              }
            }
          }
        }
/*                                        end pole figure loop */
        for (n2wbg = 1; n2wbg <= nyzwbg; ++n2wbg) {
          ngb = n2wtobg[n2wbg - 1];
          hjex = gjex[n2wbg - 1];
          for (na = 1; na <= alphamax; ++na) {
            ng = ngb + na;
            fh = f[ng - 1];
            if (fh != 0.) {
              f[ng - 1] = (double) Math.pow(fh, hjex) * f0[ng - 1];
            }
          }
        }
//    		System.gc();
/*                                                  FRASYM */
        frasycdt_();

/*                         F normalization in the working region */

        double[] fnorm2 = fnormf2_(1, 1, f2, ent);
        f2 = fnorm2[0];
        ent = fnorm2[1];
        f2difh = f2dif;
        f2dif = f2 - f2h;
        f2h = f2;
        iabi_it = 0;
        hrp_it = rp_it;
/* Normal EXIT to the next iteration step */

      }
    } else {
      iabi_it = 1;
    }

  } /* iteruo_ */


  /* Initialized data */

  double getf_ref(int a_1, int a_2, int a_3) {
    return f[((a_3 - 1) * betamax + a_2 - 1) * alphamax + a_1 - 1];
  }

  void setf_ref(int a_1, int a_2, int a_3, double value) {
    f[((a_3 - 1) * betamax + a_2 - 1) * alphamax + a_1 - 1] = (double) value;
  }

  void frasycdt_() { // translation completed (output missing): Luca

/* Local variables */
    int ihit, ngep1, i, idiag, n;
    double s;
    int ischl, n1, ivorz, na, nb, ng;
    double sc;
    int ix, nx, na1, ng1, ng2, mx1 = 0, nx1, mx2 = 0, mx3 = 0, mx4 = 0, mx5 = 0, mx6 = 0,
            mx7 = 0, nbe, nge, nxl, nge1;
    int[] mma = new int[1000], mmg = new int[1000];
    boolean loop_exit, skip;

    int[] mgew = {alphamax, alphamax2, alphamax2, alphamax4, alphamax4, alphamax2,
                  alphamax4, alphamax3, alphamax3, alphamax6, alphamax6};
    int[] mbew = {alphamax2, alphamax2, alphamax4, alphamax2, alphamax4, alphamax4,
                  alphamax4, alphamax2, alphamax4, alphamax2, alphamax4};
    int[] mnw = {1, 2, 2, 4, 4, 2, 4, 3, 3, 6, 6};

/* Realizes the exact (GBW/C1)-symmetry for ODF's in the */
/* (ALPHA,BETA,GAMMA 5 degrees ;  WIMV-convention) elementary region */
/* for (GBW,IGA=C1). GBW=GB for GB=Cn and Dn groups is valid. */

/* For the cubic groups GB=O (432) or GB=T (23)  GBW=D4 (422) and */
/* GBW=D2 (222) is working correspondingly. I.e. also the threefold */
/* enlarged elmentary regions are considered. */
/* A cubic/triclinic symmetry will exactly be realized if the INPUT */
/* ODF was constructed considering the cubic C3-axes before FRASYCDT */
/* has been called. */

/* (Dn,Cn/C1)-symmetries do not change BETA, i.e. VG is well defined; */
/* no change of normalization. */

/*       INPUT - OUTPUT F-array  - in the working G-space region */

    nge = mgew[igb - 1];
    nbe = mbew[igb - 1];
    ivorz = MoreMath.powint(-1, igb);

/*  Upper and lower faces :  ALFA=0=360 degrees */

    for (ng = 1; ng <= nge; ++ng) {
      for (nb = 1; nb <= nbe; ++nb) {
        s = (getf_ref(1, nb, ng) + getf_ref(alphamax, nb, ng)) * .5;
        setf_ref(1, nb, ng, s);
        setf_ref(alphamax, nb, ng, s);
      }
    }

/*  Front and rear faces  :  GAMMA=0=360/n degrees */

    for (nb = 1; nb <= nbe; ++nb) {
      for (na = 1; na <= alphamax; ++na) {
        s = (getf_ref(na, nb, 1) + getf_ref(na, nb, nge)) * .5;
        setf_ref(na, nb, 1, s);
        setf_ref(na, nb, nge, s);
      }
    }

/*  Left face (BETA=0) :  BETA=0, X=ALPHA+GAMMA */

/*    X=0,5,...(360/n-5) degrees : (n+1) diagonals and for X=0 the corner
*/
/*    (ALPHA=360,GAMMA=360/n). */
/*    The diagonals with Xd=X,X+360/n,X+2*360/n,..,X+360 degrees. */

/*  For Cn groups right face (BETA=180 degrees) also : X=ALPHA-GAMMA */

/*    X=0,5,...(360/n-5) degrees  (n+1) diagonals and for X=0 the corner
*/
/*    (ALPHA=360,GAMMA=0). */
/*    The diagonals with Xd=-360/n+X,X,X+360/n,X+2*360/n,..,X+360*(n-1)/n
*/
/*    degrees. */

    nge1 = nge - 1;
    n = mnw[igb - 1];
    n1 = n + 1;
    ischl = 0;

/*       Loop for the sets of diagonals */

    do {
      for (nx = 1; nx <= nge1; ++nx) {
        nx1 = nx - 1;
        ihit = 0;
        s = 0.;
        if (ischl != 0) {
          if (nx == 1) {
            s = getf_ref(alphamax, betamax, 1);
          }
        } else {
          if (nx == 1) {
            s = getf_ref(alphamax, 1, nge);
          }
        }
        for (idiag = 1; idiag <= n1; ++idiag) {
          nxl = nx1 + nge1 * (idiag - 1);
          if (ischl != 0) {
            nxl -= nge1;
          }
          switch (idiag) {
            case 1:
              mx1 = nxl;
              break;
            case 2:
              mx2 = nxl;
              break;
            case 3:
              mx3 = nxl;
              break;
            case 4:
              mx4 = nxl;
              break;
            case 5:
              mx5 = nxl;
              break;
            case 6:
              mx6 = nxl;
              break;
            case 7:
              mx7 = nxl;
              break;
          }
        }
        for (ng = 1; ng <= nge; ++ng) {
          ng2 = ng - 2;
          for (na = 1; na <= alphamax; ++na) {
            ix = na + ng2;
            if (ischl != 0) {
              ix = na - ng;
            }
            boolean stat494 = false;
            switch (n1) {
              case 7:
                if (ix == mx7) {
                  stat494 = true;
                  break;
                }
              case 6:
                if (ix == mx6) {
                  stat494 = true;
                  break;
                }
              case 5:
                if (ix == mx5) {
                  stat494 = true;
                  break;
                }
              case 4:
                if (ix == mx4) {
                  stat494 = true;
                  break;
                }
              case 3:
                if (ix == mx3) {
                  stat494 = true;
                  break;
                }
              case 2:
                if (ix == mx2) {
                  stat494 = true;
                  break;
                }
              case 1:
                if (ix == mx1) {
                  stat494 = true;
                  break;
                }
              default:
                {
                }
            }
            if (stat494) {
              if (ischl != 0)
                s += getf_ref(na, betamax, ng);
              else
                s += getf_ref(na, 1, ng);

              ++ihit;
              mma[ihit - 1] = na;
              mmg[ihit - 1] = ng;
            }
          }
        }

        sc = s / ihit;
        for (i = 1; i <= ihit; ++i) {
          na = mma[i - 1];
          ng = mmg[i - 1];
          if (ischl == 0) {
            setf_ref(na, 1, ng, sc);
          }
          if (ischl != 0) {
            setf_ref(na, betamax, ng, sc);
          }
        }
      }

      loop_exit = true;
      skip = false;

      if ((ivorz > 0 && igb != 6) || igb == 1) {
/*	  For Dn groups right face (BETA=90 degrees) */
/*	      Symmetry element C2XB :  (180 degrees+ALPHA,360 degrees-GAMMA) */
        ++ischl;
        if (ischl == 2) {
          skip = true;
        }
        if (ischl == 1) {
          loop_exit = false;
        }

      }
    } while (!loop_exit);

    if (!skip) {
      ngep1 = nge + 1;
      for (ng = 1; ng <= nge; ++ng) {
        ng1 = ngep1 - ng;
        for (na = 1; na <= alphamax; ++na) {
          na1 = na + betamax1;
          if (na1 > alphamax) {
            na1 += -alphamax1;
          }
          s = (getf_ref(na, alphamax4, ng) + getf_ref(na1, alphamax4, ng1)) * .5;
          setf_ref(na, alphamax4, ng, s);
          setf_ref(na1, alphamax4, ng1, s);
        }
      }
    }

/*   ***** Symmetrization of the boarder cells is finished ***** */

  } /* frasycdt_ */

  void pfouto_() { // translation completed (output missing): Luca

/* Local variables */
    double phonstepeps, cthi, fhir, sthi, wght;
    int nover, ny, npolex;
//    double fak;
    int inv;
    int nnpolex, izoveri;


/*       INPUT : F,POLEX,FAKK(NPOLEX) */

/*       Renormalization of the POLEX's, OUTPUT onto IOUT,IPLOTX */
/*       zeros in the unmeasured regions */
/*       Calculation of the normalized complete POLREF's */
/*       OUTPUT onto IOUT,IPLOTR */

    phonstepeps = .005;
    phoninp = subfmin_(f, totalmax);
    phonstep = phoninp + phonstepeps;
/*                                                  POLE FIGURE LOOP */
    Phase thephase = wimvTexture.getPhase();
    BufferedWriter PFwriter = Misc.getWriter(thephase.getFilePar().getDirectory() +
            thephase.toXRDcatString() + ".xpx");

    String title1 = new String(thephase.toXRDcatString() + ": experimental pole figure, ");
    String title2 = new String(thephase.toXRDcatString() + ": recalculated pole figure, ");

    nnpolex = 0;
    for (npolex = 1; npolex <= izpol; ++npolex) {
      izoveri = moveri[npolex - 1];
      for (ny = 0; ny < old1387max; ++ny) {
        polref[ny] = 0.;
      }
/*                                        loop of overlapping reflexes
 */
      StringBuffer tmp = new StringBuffer(title1);
      tmp = tmp.append(" ").append(Integer.toString(milhp[npolex - 1])).
              append(",").append(Integer.toString(milkp[npolex - 1])).
              append(",").append(Integer.toString(millp[npolex - 1]));
      int bufflength = tmp.length();
      for (int i = 0; i < 79 - bufflength; i++)
        tmp = tmp.append(" ");

      String commentLine = new String(tmp.toString().substring(0, 79) + "#");

      int skip73 = 0;
      int tetangl = 0;
      double startalpha = 90.0;
      double finalalpha = 0.0;
      for (ny = 0; ny < old1387max; ++ny) {
        if (++skip73 != alphamax) {
          polf[ny] = cpolex_23[npolex - 1][ny];
          if (polf[ny] <= 0.0) {
            polf[ny] = 0.0;
          } else {
            startalpha = Math.min(startalpha, tetangl * resolution);
            finalalpha = Math.max(finalalpha, tetangl * resolution);
          }
        } else {
          skip73 = 0;
          tetangl++;
        }
      }

      double startbeta = 0.0;
      double finalbeta = 360.0;

      try {
        PFwriter.write(commentLine);
        for (int i = 0; i < 5; i++)
          PFwriter.write(Constants.lineSeparator);

//			new String("    1.0000    1.0000    1.0000   90.0000   90.0000   90.0000    7    1");
        PFwriter.write(Misc.getFirstPFline(thephase));
        PFwriter.write(Constants.lineSeparator);

        String firstline = new String(" " + Misc.getIntStringFormatted(milhp[npolex - 1], 3) +
                Misc.getIntStringFormatted(milkp[npolex - 1], 3) +
                Misc.getIntStringFormatted(millp[npolex - 1], 3) +
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

      int until18 = 0;
      skip73 = 0;

      for (ny = 0; ny < old1387max; ++ny) {
        if (++skip73 != alphamax) {
          int imh = (int) (polf[ny] * 100.000001);
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
        } else
          skip73 = 0;
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

      tmp = new StringBuffer(title2);
      tmp = tmp.append(" ").append(Integer.toString(milhp[npolex - 1])).
              append(",").append(Integer.toString(milkp[npolex - 1])).
              append(",").append(Integer.toString(millp[npolex - 1]));
      bufflength = tmp.length();
      for (int i = 0; i < 79 - bufflength; i++)
        tmp = tmp.append(" ");

      commentLine = new String(tmp.toString().substring(0, 79) + "#");

      try {
        PFwriter.write(commentLine);
        for (int i = 0; i < 5; i++)
          PFwriter.write(Constants.lineSeparator);

//			new String("    1.0000    1.0000    1.0000   90.0000   90.0000   90.0000    7    1");
        PFwriter.write(Misc.getFirstPFline(thephase));
        PFwriter.write("\n\r");

        String firstline = new String(" " + Misc.getIntStringFormatted(milhp[npolex - 1], 3) +
                Misc.getIntStringFormatted(milkp[npolex - 1], 3) +
                Misc.getIntStringFormatted(millp[npolex - 1], 3) +
                "   .0 90.0" + Misc.getDoubleStringFormatted(resolution, 3, 1) +
                "   .0360.0" + Misc.getDoubleStringFormatted(resolution, 3, 1) +
                " 1 1");
        PFwriter.write(firstline);
        PFwriter.write(Constants.lineSeparator);
      } catch (IOException io) {
      }

      for (nover = 1; nover <= izoveri; ++nover) {
        ++nnpolex;
        if (nover == 1) {
          milhp[npolex - 1] = milh[nnpolex - 1];
          milkp[npolex - 1] = milk[nnpolex - 1];
          millp[npolex - 1] = mill[nnpolex - 1];
        }

        wght = weight[nnpolex - 1];
        sthi = asthi[nnpolex - 1];
        cthi = acthi[nnpolex - 1];
        fhir = afhir[nnpolex - 1];
        inv = minv[nnpolex - 1];

        calpolo_(sthi, cthi, fhir, inv);

        for (ny = 0; ny < old1387max; ++ny) {
          polref[ny] += wght * fs[ny];
        }
      }

      until18 = 0;
      skip73 = 0;

      for (ny = 0; ny < old1387max; ++ny) {
        if (++skip73 != alphamax) {

          int imh = (int) (polref[ny] * 100.000001);

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
        } else
          skip73 = 0;
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
    }

    try {
      PFwriter.write(Constants.lineSeparator);
      PFwriter.flush();
      PFwriter.close();
    } catch (IOException io) {
    }
  } /* pfouto_ */

  void pfexouto_() {	// translation completed (output missing): Luca

/* Local variables */
//    int izoveri;

/*       INPUT : F,POLEX,FAKK(NPOLEX) */

/*       Renormalization of the POLEX's, OUTPUT onto IOUT,IPLOTX */
/*       zeros in the unmeasured regions */
/*       Calculation of the normalized complete POLREF's */
/*       OUTPUT onto IOUT,IPLOTR */

    int nnpolex = -1;
    for (int npolex = 0; npolex < izpol; ++npolex) {
//    	restore23(npolex, polexl);
      for (int ny = 0; ny < old1387max; ++ny) {
        polf[ny] = cpolex_23[npolex][ny];
      }
//      izoveri = moveri[npolex];
/*                                        loop of overlapping reflexes
 */
      for (int nover = 0; nover < moveri[npolex]; ++nover) {
        ++nnpolex;
        if (nover == 0) {
          milhp[npolex] = milh[nnpolex];
          milkp[npolex] = milk[nnpolex];
          millp[npolex] = mill[nnpolex];
        }
      }
    }
  } /* pfexouto_ */

  void odf_output(double[][][] odf) {

    for (int na = 0; na < alphamax; ++na)
      for (int nb = 0; nb < betamax; ++nb)
        for (int ng = 0; ng < alphamax; ++ng)
          odf[na][nb][ng] = f[(ng * betamax + nb) * alphamax + na];
  }

} // end of class Uwimvuo
