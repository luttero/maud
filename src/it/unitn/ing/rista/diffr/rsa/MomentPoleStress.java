/*
 * @(#)MomentPoleStress.java created 04/10/2001 Cassino
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

package it.unitn.ing.rista.diffr.rsa;

import java.awt.*;
import javax.swing.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.rta.*;

import javax.swing.border.*;
import java.io.*;

/**
 *  The MomentPoleStress is a class to compute the diffraction shift from
 *  the triaxial stress tensor using the moment pole method of Siegfried Matthies.
 *  See Siegfried Matthies comments in Sla33.java.
 *
 * @version $Revision: 1.15 $, $Date: 2006/07/20 13:39:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

// #########################################################################
//  Program SLA33PRO.FOR                            S.M. Trento   Sept. 2001

//  Frame program to demonstrate the work of the

//      SUBROUTINE SUBSLA33(IMODEL,IRANDOM,BK0,SThi,CThi,SFhi,CFhi,Tyj,Fyj,
//     *                    Tyj,Fyj,STyj,CTyj,SFyj,CFyj,PFthreshold,SLA33)

//  that can directly be applied in Fit procedures.

//  The program uses the elements of the program SLLA2001.FOR in a shortened,
//  restructurized and only SLA33-oriented form.

//  Using SUBSLA33 in an other calling program identical INPUTS must
//  reproduce the same results of the given Standard Examples !!!

//  """"""
//  OUTPUT :  The array SLA33(I) = SLA33kl  (the double quantities, with four
//  """"""                                  indexes and unreduced values !!!
//                                          are given, but using Voigt's
//                                          shortened index notation I=1-6)

//            in SUBSLA33 and written to the file MODELSLA.OUT (unit 60).
//                                                oooooooooooo

// -------------------------------------------------------
//            Scheme of Voigt's shortened index notation :

//                     I  <->   k l

//                     1        1 1
//                     2        2 2
//                     3        3 3
//                     4        2 3
//                     5        1 3
//                     6        1 2

// --------------------------------------------------------------------------
// The experimental peak shift related strain value EpsilonL33(hi,yj) for the
// (hkl)-reflection -> hi and the sample direction yj is then connected
// with the macroscopic stress components SigmaA(I)=SigmaAkl, given in the
// sample coordinate system KA, by

//      EpsilonL33(hi,yj) =    SUM(I=1-3) SLA33(I;'hi,yj')*SigmaA(I) +

//                           2*SUM(I=4-6) SLA33(I;'hi,yj')*SigmaA(I)
// --------------------------------------------------------------------------
// Attention !

//     If the bulk modulus reduced |SLA33(I;'hi,yj')|, that commonly are in
//     the maximum order of X.xxxx, are lower than 0.0001, we will put them
//     in SUBSLA33 equal to zero.                  ######
//     For such small SLA33(I;'hi,yj') a fit of the SigmaA(I) multiplied to
//     those SLA33-values will be without meaning.
//     But a computer will explicitly exclude these SigmaA(I) connections
//     in such cases only for SLA33(kl) = 0. !!!

//  """""""""""""""
//  Explicit INPUTs of the SUBROUTINE SUBSLA33 :
//  """""""""""""""

//   IMODEL  =  0 ->   VOIGT
//   ******     1 ->   REUSS
//              2 ->   HILL
//              3 ->   PathGeo
//              4 ->   BPGeo      (BulkpathGeo)

//   IRANDOM   for IRANDOM=1 random texture is supposed - no ODF INPUT
//   *******

//   BK0                     single crystalline bulk modulus K0
//   ***

//   SThi,CThi,SFhi,CFhi    sin and cos of the polar angle (THETAhi) and
//   *******************                       azimuth     (PHIhi  )

//                                      of the normal direction hi of the
//                                      (hkl) reflection plane given in the
//                                      crystal coordinate system KB

//   Tyj,Fyj, and
//   STyj,CTyj,SFyj,CFyj    sin and cos of the polar angle (Tyj=THETAyj) and
//   *******************                       azimuth     (Fyj=PHIyj  )

//                                      of the sample direction yj (given in
//                                      the sample coordinate system KA) of the
//                                      scattering vector for the actual ('j')
//                                      sample orientation relative to the
//                                      incoming beam and detector position

//              --------------------------------------------------------------
//              FOR THE PRESCRIPTION HOW THE CRYSTAL COORDINATE SYSTEM KB
//              IS DEFINED SEE TABLES  5.1, 14.1 AND FIG. 5.1-5.6, 5.9-5.12 in
//              S.Matthies,G.W.Vinel and K.Helming,
//              Standard Distributions in Texture Analysis (Vol.1),
//              Akademieverlag Berlin (1987)

//              FOR Gb,GB,GBS see Table 14.1
//              --------------------------------------------------------------

//   PFthreshold > 0 !!
//   ***********
//                   For small pole figure values Phi(yj) (e.g. <= 0.01 )
//                   a reflection peak will hardly be seen in a spectrum
//                   or possesses a bad statistics. Therfore the term
//                   'peak shift' has no meaning in this case. Consequently
//                   for Phi(yj) <= PFthreshold the corresponding SLA33(hi,yj)
//                   will not be calculated, and in the OUTPUTs symbolic
//                   values '0.88888 E8' or a corresponding comment will be
//                   given. Moreover, PFthreshold > 0 avoids in a natural way
//                   the 0/0 - situation for the determination of the
//                   PF-reduced pole figure moments in the case of Phi(yj)=0.

//                   Phi(yj) means in this connection the recalculated
//                   value for the reduced PF P~  that follows as the '0-th'
//                   pole figure moment from the INPUT-ODF (that must be >= 0).

// --------------------------------------------------------------------------

//  """""""""""""""
//  Implicit INPUTs of the SUBROUTINE SUBSLA33:
//  """""""""""""""

//  For the given program the implicit data controling SUBSLA33 are given in

//  the files                        SLA33.CTR (unit 5) and
//                                   ooooooooo

//                                   E0INPUT.DAT (unit 11), and for the case
//                                   ooooooooooo

// of no random texture by the file INPUTODF.STD (unit 9).
//                                  oooooooooooo

// There are two Library data files CWWARRAY.DAT (unit 17), and
//                                  oooooooooooo

//                                  IADARRAY.DAT (unit 18).
//                                  oooooooooooo

// The Library files are necessary for the WWa calculation in case of
// IMODEL.NE.1,3 .AND. IRANDOM.NE.1


// ----------------------------------------------------------------------------
//  SLA33.CTR  contains (cf. the corresponding files for the standard examples)
//  ooooooooo

//  IMODEL
//  ******

//  IRANDOM
//  *******

//  PFthreshold
//  ***********

//  IGB - coded crystal symmetry
//  ****************************
//  IGA - coded crystal symmetry
//  ****************************
//                               used by the INPUT of an ODF in case
//                               of no random texture (IRANDOM.NE.1)
//                               when the ODF is given in the effective
//                               IGA/IGB-related elementary G-space region
//                               by the WIMV ODF Standard Format (5\ufffd-cells) :
//                               File INPUTODF.STD (unit 9)
//                                    oooooooooooo
//                     ------------------------------------------------------
//    Symmetry codes :
//                       ROTATION GROUP           CODE NUMBER
//                          D6  622                   11
//                          C6    6                   10
//                          D3  322                    9
//                          C3    3                    8
//                          O   432                    7
//                          T   232                    6
//                          D4  422                    5
//                          C4    4                    4
//                          D2  222                    3
//                          C2    2                    2
//                          C1    1                    1

//   In this program version for sample symmetry only the rotation groups
//   GA : C1,C2 and D2 can be considered.
//   For cyclic textures the ODF may formally be given in such a GA-depending
//   elementary region. For other sample symmetries the SUBROUTINE ODFINP
//   from the program FALU94 with FIOTTU can be adapted.

//   If the crystal class Gb belongs to type III for normal scattering
//   (Friedels law) GBS instead of the rotation group GB is to be considered.
//   I.e. also the ODF-input needs only the "GBS-reduced" elementary region.
//   If the ODF was reproduced from reduced pole figures it automatically
//   possesses the GBS symmetry due to the "uncorrectable special ghosts" in
//   this case. If the "true" ODF (crystal symmetry GB) is available it has
//   to be "GBS-symmetrized" because the moments for normal scattering feel
//   only this part of the orientation distribution.
//                     ------------------------------------------------------

//  LATTICE DATA :    a  b  c  alpha  beta  gamma [degrees]
//  ************
//                    (a b c can be given by any dimensions or by the relative
//                     variant a/c b/c 1)

//  After a comment line the file  SLA33.CTR contains a block where for
//          ************           ooooooooo

//  testing activities the SLA for more than one reflection hi (hkl) and
//  several reflection-specific sample directions yj can be ordered

//  IZPOL - number of reflections (hkl) to be considered
//  *****

//          For each reflection there must be given

//  h k l        - Miller indexes of the reflection [int numbers]
//  *****

//  NYZ          - number of sample directions yj to be considered
//  ***

//               Then NYZ lines with the corresponding

//  THETAyj PHIyj  follow
//  *************
// --------------------------------------------------------------------------

//  E0INPUT.DAT      contains the monocrystalline ('0') elastic data.
//  ooooooooooo      They can be given for

//  ISTIFF = 1
//  ******           in form of the (21) stiffness components

//                   C0(I,J) I=1-6 , J = I-6           or for
//                   *******
//  ISTIFF .NE.1
//  ******           in form of the (21) compliance components

//                   S0(I,J) I=1-6 , J = I-6
//                   *******

//  Attention ! The C0 or S0 data (capital letters) use only Voigt's reduced
//              index notation, but not Voigt's reduction scheme, with the
//              c0(I,J) or s0(I,J), commonly given in literature.

//              It holds:
//                             C0(I,J) = c0(I,J),   but !!!

//                             S0(I,J) = s0(I,J)    I and J = 1,2,3
//                             S0(I,J) = s0(I,J)/2  only I or J = 1,2,3
//                             S0(I,J) = s0(I,J)/4  I and J = 4,5,6

//  For the right start of the program the symmetric array E0(6,6) in
//  COMMON/CE0/E0(6,6) must contain the full 6x6 array S0.
//  In the given version of the program the 6x6 completition, and the may be
//  necessary conversion from C0 to S0 (case ISTIFF=1) is automatically done
//  by the SUBROUTINE COMPLIAN, that reads the file E0INPUT.DAT.
//                                                  ooooooooooo

// ****************************************************************************

//  STRUCTURE OF THE PROGRAM

//  All INPUT, calculating and OUTPUT activities are documented in the
//  listing file 'SUBSLA33.LST' (unit 16)
//  After a successfull testing of the work of SUBSLA33 in a rewritten
//  calling structure of a FIT procedure the corresponding WRITE(16,
//  commands can simply be commented out.

//  Before the SUBROUTINE SUBSLA33 - called here inside a hi (NPOL=1,IZPOL)
//                                   and a yj (NY=1,NYZ) DO loops

//  can work, and in order to minimize the time of SLA33 calculation a
//  lot of subsidiary data must be ready for use.
//  Their number and kind depend on IMODEL and IRANDOM.

//  A first block of calculations
//  prepares data not depending on
//  IMODEL, IRANDOM, the ODF, hi or yj     :  SUBROUTINE PREPARE1

//  Then for IMODEL.NE.0 PFmoment
//  related data are prepared              :  SUBROUTINE PREPARE2

//  The VOIGT, HILL and BPGeo models
//  need for a nonrandom texture the
//  calculation of bulk properties
//  using the ODF.
//  The corresponding subsidiary data
//  are prepared by                        :  SUBROUTINE PREPARE3

//  monocrystalline elastic parameters
//  **********************************
//  The INPUT of the monocrystalline
//  elastic parameters E0 and their
//  preparation for use is organized by    :  SUBROUTINE E0PREP(IMODEL,BK0)
//  E0(6,6) is a working array with
//  changing content.The arrays C0(6,6)
//  and S0(6,6) are fixed.
//  Determination of single crystalline
//  bulk modulus K0 = BK0. All general
//  calculations use K0-reduced
//  'S0'=S0*BK0 or 'C0'=C0/BK0 without
//  dimensions. Reconstruction of the
//  right dimensions of SLA in SUBSLA33

//  INPUT of CRYSTAL and SAMPLE SYMMETRY   :  SUBROUTINE IGAIGBPREP(...)
//  ************************************
//  if necessary, for INPUT of the ODF,
//  given in an IGA,IGB-specific
//  elementary G-space region

//  ODF INPUT & Preparation (IRANDOM.NE.1) :  SUBROUTINE ODFPREP(PHON,IGA,IGB)
//  **************************************
//  Array FIO(73,37,73) COMMON/CFIO/
//  and the phonreduced weighted variant
//  F(73,37,73) COMMON/CF/

//  Determination of the bulk SVoigt=1/Ca
//  or SGeo for (VOIGT,HILL) or BPGeo      :  SUBROUTINE BULKPREP(IMODEL,PHON)
//  **************************************
//  COMMON /CSVOIGT/SVOIGT(6,6)
//  COMMON /CEGEOM/EGEOM(6,6),EA(6,6),

//  INPUT & Preparation Lattice Parameters :  SUBROUTINE LATTICEPREP(PIF)
//  **************************************


//  Now a                                     hi and an yj loop
//                                            #################
//  are started for IZPOL reflections hi
//  and NYZ measuring points yj for the
//  actual (hkl)reflection. IZPOL and NYZ
//  and the hi and yj parameters are taken
//  from the file SLA.CTR
//                ooooooo

//  Some specific quantities depend on hi
//  only. They are calculated by the       :  SUBROUTINE HI_DATPREP(...)
//  *************************************

//  Now all is ready to                    :  CALL SUBSLA33(...)
//  inside the inner yj-loop.

// *************************************************************************
// l      CHARACTER FNAME*20,IMODELNAME(0:4)*20,COMMENTLINE*72


public class MomentPoleStress extends Strain {

  public static String[] diclistc = {
    "_rista_residual_stress_model",
    "_rista_residual_stress_use_texture",
    "_rista_voigt_reuss_weight",
    "_rista_stiffness_11", // 1
    "_rista_stiffness_12", // 2
    "_rista_stiffness_13", // 3
    "_rista_stiffness_14", // 4
    "_rista_stiffness_15", // 5
    "_rista_stiffness_16", // 6
//                                      "_rista_stiffness_21",
    "_rista_stiffness_22", // 7
    "_rista_stiffness_23", // 8
    "_rista_stiffness_24", // 9
    "_rista_stiffness_25", // 10
    "_rista_stiffness_26", // 11
//                                      "_rista_stiffness_31",
//                                      "_rista_stiffness_32",
    "_rista_stiffness_33", // 12
    "_rista_stiffness_34", // 13
    "_rista_stiffness_35", // 14
    "_rista_stiffness_36", // 15
//                                      "_rista_stiffness_41",
//                                      "_rista_stiffness_42",
//                                      "_rista_stiffness_43",
    "_rista_stiffness_44", // 16
    "_rista_stiffness_45", // 17
    "_rista_stiffness_46", // 18
//                                      "_rista_stiffness_51",
//                                      "_rista_stiffness_52",
//                                      "_rista_stiffness_53",
//                                      "_rista_stiffness_54",
    "_rista_stiffness_55", // 19
    "_rista_stiffness_56", // 20
//                                      "_rista_stiffness_61",
//                                      "_rista_stiffness_62",
//                                      "_rista_stiffness_63",
//                                      "_rista_stiffness_64",
//                                      "_rista_stiffness_65",
    "_rista_stiffness_66", // 21
    "_rista_macrostress_11",
    "_rista_macrostress_22",
    "_rista_macrostress_33",
    "_rista_macrostress_23",
    "_rista_macrostress_13",
    "_rista_macrostress_12"
  };
  public static String[] diclistcrm = {
    "_rista_residual_stress_model",
    "_rista_residual_stress_use_texture",
    "voigt-reuss weight",
    "stiffness_11 (arb)", // 1
    "stiffness_12 (arb)", // 2
    "stiffness_13 (arb)", // 3
    "stiffness_14 (arb)", // 4
    "stiffness_15 (arb)", // 5
    "stiffness_16 (arb)", // 6
//                                      "_rista_stiffness_21",
    "stiffness_22 (arb)", // 7
    "stiffness_23 (arb)", // 8
    "stiffness_24 (arb)", // 9
    "stiffness_25 (arb)", // 10
    "stiffness_26 (arb)", // 11
//                                      "_rista_stiffness_31",
//                                      "_rista_stiffness_32",
    "stiffness_33 (arb)", // 12
    "stiffness_34 (arb)", // 13
    "stiffness_35 (arb)", // 14
    "stiffness_36 (arb)", // 15
//                                      "_rista_stiffness_41",
//                                      "_rista_stiffness_42",
//                                      "_rista_stiffness_43",
    "stiffness_44 (arb)", // 16
    "stiffness_45 (arb)", // 17
    "stiffness_46 (arb)", // 18
//                                      "_rista_stiffness_51",
//                                      "_rista_stiffness_52",
//                                      "_rista_stiffness_53",
//                                      "_rista_stiffness_54",
    "stiffness_55 (arb)", // 19
    "stiffness_56 (arb)", // 20
//                                      "_rista_stiffness_61",
//                                      "_rista_stiffness_62",
//                                      "_rista_stiffness_63",
//                                      "_rista_stiffness_64",
//                                      "_rista_stiffness_65",
    "stiffness_66 (arb)", // 21
    "macrostress_11 (arb)",
    "macrostress_22 (arb)",
    "macrostress_33 (arb)",
    "macrostress_23 (arb)",
    "macrostress_13 (arb)",
    "macrostress_12 (arb)"
  };

  public static String[] classlistcs = {};
  public static String[] classlistc = {};

  Sample actualsample = null;

  public static String[] stressModels = {"Voigt", "Reuss", "Hill", "PathGEO", "BulkPathGEO"};
//	int actuallayer = 0;

  int irandom = 1;
  double pfthreshold_tmp = 0.05;
  double[] hj = new double[9], hjm = new double[9];
//  int[][] mij = new int[3][3];
  double pi, pif, p2i, pi5, pi25, pisim;
  double[][] facun = new double[6][6];
  int[][] mij = new int[6][2];
  int[][] mik = new int[3][3];
  int[] ifiw = new int[73];
  double[] cr2 = new double[181], sr2 = new double[181], cr4 = new double[181], sr4 = new double[181],
  cr6 = new double[181], sr6 = new double[181], cr8 = new double[181], sr8 = new double[181];
  double[][] trigs = new double[9][73];
  double[][] e0 = new double[6][6];
  double[][] s0 = new double[6][6];
  double[][] c0 = new double[6][6];
  double[][] egeom = new double[6][6], ea = new double[6][6];
  double[][] svoigt = new double[6][6];
  double s12l, c12l, s23l, c23l, s31l, c31l;
  double cda, cdb;
  double[] spmas = new double[9], spmasm = new double[9];
  double[][][] sshi0 = null, sshi0m = null;
  double[][][] hs = new double[36][36][9];
//  double fio[73][37][73];
  double[][][][] sgeofull = new double[3][3][3][3];
//	double f[73][37][73];
  double[][] wwarir = new double[36][36];
  double[][] wwarim = new double[36][36];
//  int iadd[] = new int[9885];
//  int c00 = 0;
  public static int c6 = 6;
  int c73 = 73;
  int c37 = 37;
  double phon = 0.;

  boolean debug_output = MaudPreferences.getBoolean("momentPoleStress.debug", false);
  boolean log_output = false;
  boolean siegfried_strict = false;

  public static final int[] mi = {1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 2, 3, 4,
                                  5, 6, 3, 4, 5, 6, 4, 5, 6, 5, 6, 6};
  public static final int[] mj = {1, 2, 3, 4, 5, 6, 2, 3, 4, 5, 6, 3, 4, 5, 6, 4, 5, 6, 5, 6, 6, 1, 1, 1,
                                  1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5};
  public static final int[] mivoigt = {1, 2, 3, 2, 3, 1, 3, 1, 2};
  public static final int[] mjvoigt = {1, 2, 3, 3, 1, 2, 2, 3, 1};

	private int actualReflexIndex = 0;

  public MomentPoleStress(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Moment Pole Stress";
    IDlabel = "Moment Pole Stress";
    description = "select this to apply the Moment Pole Stress method of Siegfried Matthies";
  }

  public MomentPoleStress(XRDcat aobj) {
    this(aobj, "Moment Pole Stress");
  }

  public MomentPoleStress() {
    identifier = "Moment Pole Stress";
    IDlabel = "Moment Pole Stress";
    description = "select this to apply the Moment Pole Stress method of Siegfried Matthies";
  }

  public void initConstant() {
    Nstring = 2;
    Nstringloop = 0;
    Nparameter = 28;
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
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
  }

  public void initParameters() {
    super.initParameters();

    double s11 = 168.4;
    double s12 = 121.4;
    parameterField[0] = new Parameter(this, getParameterString(0), 0.5,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 0),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 1));
    for (int i = 1; i < 22; i++) {
	    if (i == 1 || i == 7 || i == 12) // 11, 22, 33
	    parameterField[i] = new Parameter(this, getParameterString(i), s11,
			    ParameterPreferences.getDouble(getParameterString(i) + ".min", 1),
			    ParameterPreferences.getDouble(getParameterString(i) + ".max", 1000));
	    else if (i == 16 || i == 19 || i == 21)  // 44, 55, 66
		    parameterField[i] = new Parameter(this, getParameterString(i), 2.0 * (s11 - s12),
				    ParameterPreferences.getDouble(getParameterString(i) + ".min", 1),
				    ParameterPreferences.getDouble(getParameterString(i) + ".max", 500));
	    else if (i == 2 || i == 3 || i == 8)  // 12, 13, 23
		    parameterField[i] = new Parameter(this, getParameterString(i), s12,
				    ParameterPreferences.getDouble(getParameterString(i) + ".min", 1),
				    ParameterPreferences.getDouble(getParameterString(i) + ".max", 300));
	    else
		    parameterField[i] = new Parameter(this, getParameterString(i), 0,
				    ParameterPreferences.getDouble(getParameterString(i) + ".min", -100),
				    ParameterPreferences.getDouble(getParameterString(i) + ".max", 100));
    }
    for (int i = 22; i < 28; i++) {
      parameterField[i] = new Parameter(this, getParameterString(i), 0.0,
              ParameterPreferences.getDouble(getParameterString(i) + ".min", -1),
              ParameterPreferences.getDouble(getParameterString(i) + ".max", 1));
    }

    refreshComputation = true;
  }

  double[] macrostress = new double[6];
  int imodel = 0;

	public void updateParametertoDoubleBuffering(boolean firstLoading) {
    super.updateParametertoDoubleBuffering(false);

    imodel = getStressModelValue();
    fio_tmp = null;

    int k = 1;
    for (int i = 0; i < 6; i++)
      for (int j = 0; j < 6; j++) {
        if (i <= j)
          e0[i][j] = parameterValues[k++];
        else
          e0[i][j] = e0[j][i];
      }
    int factor = 1;
    for (int i = 0; i < 6; i++) {
      if (i == 3) factor++;
      macrostress[i] = parameterValues[k++] * factor;
    }
    checkForSymmetries(e0);
  }

  public void checkForSymmetries(double[][] e0) {
  }

  public String getStressModelID() {
    return stringField[0];
  }

  public int getStressModelValue() {

    String modelID = getStressModelID();

    for (int i = 0; i < stressModels.length; i++) {
      if (modelID.equals(stressModels[i]))
        return i;
    }
    return 0;
  }

  public void setStressModel(int i) {
    setStressModel(stressModels[i]);
  }

  public void setStressModel(String value) {
    stringField[0] = value;
  }

  public boolean useTexture() {
    return stringField[1].equalsIgnoreCase("true");
  }

  public void useTexture(boolean status) {
    if (status)
      stringField[1] = "true";
    else
      stringField[1] = "false";
  }

  public void useTexture(String value) {
    stringField[1] = value;
  }

/*  public void computeStrain(Sample asample) {

    Phase aphase = getPhase();
    computeStrain(aphase, asample);

  }*/

  double[][][] fio_tmp = null;
  double bk0_tmp = 0.0;
  OutputStream out = null;

	public void prepareComputation(Phase aphase, Sample asample) {
    log_output = /* getFilePar().isStrainComputationPermitted() && */getFilePar().logOutput();
    if (log_output)
      out = getFilePar().getResultStream();
    update(false);
    int igb = SpaceGroups.getLGNumberSiegfriedConv(aphase.getPointGroup());
    int iga = 1;
    phon = 0.0;

    if (useTexture())
      irandom = 0;
    else
      irandom = 1;

//    double fmin = 10.0;
    Texture atexture = aphase.getActiveTexture();
    atexture.initializeAll();

		double resolution = 5.0; //atexture.getResolutionD(); // for the moment to be changed to variable in the future

		int alphaMaxIndex = (int) (360.0 / resolution + 1.00001);
		int betaMaxIndex = (int) (180.0 / resolution + 1.00001);
		int gammaMaxIndex = (int) (360.0 / resolution + 1.00001);

		fio_tmp = new double[alphaMaxIndex][betaMaxIndex][gammaMaxIndex];
		double odf_min = 1E30;
		double odf_max = -1E30;
    for (int ia = 0; ia < alphaMaxIndex; ia++)
      for (int ib = 0; ib < betaMaxIndex; ib++)
        for (int ig = 0; ig < gammaMaxIndex; ig++) {
          double alpha = resolution * (.25 + ia);
          if (alpha > 360.0)
            alpha -= 360.0;
          double beta = resolution * (.25 + ib);
          if (beta > 180.0)
            beta -= 180.0;
          double gamma = resolution * (.25 + ig);
          if (gamma > 360.0)
            gamma -= 360.0;
          fio_tmp[ia][ib][ig] = atexture.getODF(alpha * Constants.DEGTOPI,
                  beta * Constants.DEGTOPI, gamma * Constants.DEGTOPI);
          if (fio_tmp[ia][ib][ig] < odf_min)
	          odf_min = fio_tmp[ia][ib][ig];
	        if (fio_tmp[ia][ib][ig] > odf_max)
		        odf_max = fio_tmp[ia][ib][ig];
//          System.out.println("fio "+fio_tmp[ia][ib][ig]);
        }
//    if (fmin == 1.0)
//      irandom = 1;
//		System.out.println("ODF min/max: " + odf_min + " - " + odf_max);
    if (log_output) {
      try {
        printString(out, "Use texture in moment pole stress computation : " );
        if (irandom == 1)
          printLine(out, "no" );
        else
          printLine(out, "yes");
      } catch (IOException io) {
        io.printStackTrace();
      }
    }
//    System.out.println("irandom "+irandom);

    prepare1();
    if (imodel != 0)
      subhkwkw();
    if (irandom != 1 && MoreMath.pow_ii(imodel) > 0)
      prepare3();
    if (pfthreshold_tmp < .001)
      pfthreshold_tmp = .001;
    bk0_tmp = e0prep(imodel);
    igaigbprep(iga, igb, irandom);
    double[][][] fio_corrected = null;
    if (irandom != 1)
      fio_corrected = odfprep(iga, igb, fio_tmp);
    if (MoreMath.pow_ii(imodel) > 0)
      bulkprep(imodel, irandom, fio_corrected);

	  int hkln = aphase.gethklNumber();
	  sshi0 = new double[hkln][6][6];
	  sshi0m = new double[hkln][6][6];
//	  double pif = 180. / Math.acos(-1.);
//    latticeprep(pif);
//      aphase.sghklcompute(false);
	  double[] cdsc = aphase.lattice();
	  for (int j = 0; j < hkln; j++) {
		  Reflection refl = aphase.getReflectionVector().elementAt(j);

		  double[] sctf = Uwimvuo.tfhkl(refl.getH(), refl.getK(), refl.getL(),
				  cdsc[7], cdsc[5], cdsc[3], cdsc[6], cdsc[0], cdsc[1]);
//        double fhir = Math.acos(sctf[3]);
		  subshi0(j, sctf[0], sctf[1], sctf[2], sctf[3]);
//        hi_datprep(refl.h, refl.k, refl.l, sctf[0], sctf[1], sctf[2], sctf[3], pif);
	  }
  }

	public double computeStrain(Reflection refl, double[] strain_angles) { // you don't need to modify this unless
		actualReflexIndex = getPhase().getReflexIndex(refl);
		return super.computeStrain(refl, strain_angles);
	}

	public double computeStrain(double psi, double beta, double chi, double phi) {
    // Angles must be in radiants
    // psi and beta are the polar and azimuthal angles for the crystal setting
    // phi and chi for the sample

	  double cfhi = Math.cos(beta);
    double sfhi = Math.sin(beta);
    double cthi = Math.cos(psi);
    double sthi = Math.sin(psi);
    double ctyj = Math.cos(chi);
    double styj = Math.sin(chi);
    double cfyj = Math.cos(phi);
    double sfyj = Math.sin(phi);
    if (debug_output) {
      try {
        printLine(out, "fhi " + beta * Constants.PITODEG + ", " +
                "thi " + psi * Constants.PITODEG + ", " +
                "tyj " + chi * Constants.PITODEG + ", " +
                "fyj " + phi * Constants.PITODEG
        );
      } catch (IOException io) {
        io.printStackTrace();
      }
    }
    double[] sla33 = subsla33(imodel, irandom, bk0_tmp,
            sthi, cthi, sfhi, cfhi, 1 /* not really used */,
/*chi * Constants.PITODEG, phi * Constants.PITODEG,*/ styj, ctyj,
            sfyj, cfyj, pfthreshold_tmp, fio_tmp);
    double strain33 = 0.0;

    for (int i = 0; i < 6; i++)
      strain33 += sla33[i] * macrostress[i];
/*    if (log_output) {
      try {
        printString(out, "sla33 ");
        for (int i = 0; i < 6; i++)
          printString(out, "  " + sla33[i]);
        newLine(out);
        printLine(out, "Strain33 " + strain33);
      } catch (IOException io) {
        io.printStackTrace();
      }
    }*/
    return strain33;
  }

/*  public double computeStrain(Phase aphase, double strain_angles[],
                              int h, int k, int l) {
    Reflection refl = aphase.getReflectionByhkl(h, k, l);
    if (fio_tmp == null)
      prepareComputation(aphase, aphase.getSample());

    return computeStrain(refl, strain_angles);

  }

  public double[] computeStrain(Phase aphase, double alpha[], double beta[],
                                Reflection reflex) {

    if (fio_tmp == null)
      prepareComputation(aphase, aphase.getSample());

    return computeStrain(aphase, alpha, beta, reflex);
  }*/

  public double[] subsla33(int imodel, int irandom, double bk0,
                           double sthi, double cthi, double sfhi, double cfhi, int ny,
/*double tyj, double fyj,*/ double styj, double ctyj,
                           double sfyj, double cfyj, double pfthreshold, double[][][] fio) {
//                     Calculation and output of the original Moments jq
//                     (not divided by P~) for the actual hi and yj.

//     IMODEL-independent at least the P~ will be determined

    moments01(imodel, irandom, sthi, cthi, sfhi, cfhi, ctyj, styj, cfyj, sfyj, fio);

//                     Calculation and output of the SLA33(I) (I=1-6) for the
//                     actual hi and yj, and the specified model
    return pfmomprisla33(imodel, pfthreshold, ny, /*tyj, fyj,*/ styj, ctyj, sfyj, cfyj, bk0);
  } // subsla33

  public double[] pfmomprisla33(int imodel, double pfthreshold, int ny,
/*double ty, double fy, */double sty, double cty, double sfy,
                          double cfy, double bk0) {

// int subbpgeo_(double *, double *, double *);
    double[] slvoigt33 = new double[6];
    int i, j, k, l;
    double[] sslreuss33 = new double[6];
    int iq, is, js;
    double[][] sl = new double[3][3];
    double[][] gy = new double[3][3];
    int kw;
    double ps;
    double[] slpathgeo33 = new double[6];
    int nyh, kws;
    double summ, sump, zvla1, zvla2, zvla3, zvla4, zvla5, zvla6;
    double[] slreusspath33 = new double[6];
    double[] slsgeo = new double[36];
// int geosls_(double *);
    double[][] sslgeop = new double[6][6];
// int voigtsl_(double *, double *);


//#define sl(a_1,a_2) sl[(a_2)*3 + a_1 - 4]
//#define gy(a_1,a_2) gy[(a_2)*3 + a_1 - 4]
//#define sslgeop(a_1,a_2) sslgeop[(a_2)*6 + a_1 - 7]


//     INPUT : original PF moments,SSHI0,SSHI0M

//     Determination and print of the moments,reduced moments and
//     the REUSS mean SLS,SLAS compliances, GEOPATH and BULKPATHGEO

//     If the bulk modulus reduced |SLA33(I;'hi,yj')|, that commonly are in
//     the maximum order of X.xxxx, are lower than 0.0001, we will put them
//     equal to zero.                              ######
//     For such small SLA33(I;'hi,yj') a fit of the SigmaA(I) multiplied to
//     those SLA33-values will be without meaning.
//     But a computer will explicitly exclude these SigmaA(I) connections
//     in such cases only for SLA33(kl) = 0. !!!


//                                             gap P~hi(y).GE.PFthreshold

//                                          33-REUSSpath T-reduced   in KL
//                                          33-REUSSpath T-unreduced in KL
//                                          33-VOIGT     T-unreduced in KL
//                                          SLSGEO(36)=pathgeo T-reduced
//                                                    =SSLGEOP(6,6)  in KL
//                                          33-PathGeo   T-unreduced in KL

    if (debug_output) {
      try {
        for (int i7 = 0; i7 < 9; i7++)
          printLine(out, "P+ / P- " + spmas[i7] + " / " + spmasm[i7]);
      } catch (IOException io) {
        io.printStackTrace();
      }
    }
    ps = (spmas[0] + spmasm[0]) / 2.;
// l      WRITE(16,4)NY,TY,FY,PS

// Of interest and only really determined :  IMODEL=0 : iq=0
//                                                  1 : iq=0-4
//                                                  2 : iq=0-4
//                                                  3 : iq=0-8
//                                                  4 : iq=0-8

    for (iq = 0; iq <= 8; ++iq) {
      hj[iq] = 888.888;
      hjm[iq] = 888.888;
//                                  0.001 gap for print of reduced moments,
//                                  but not used for calculations where
//                                  PFthreshold is asked.
      if (ps > .001f) {
        hj[iq] = spmas[iq] / ps;
        hjm[iq] = spmasm[iq] / ps;
      }
    }
    if (debug_output) {
      try {
        for (int i7 = 0; i7 < 9; i7++)
          printLine(out, "J / JM " + hj[i7] + " / " + hjm[i7]);
      } catch (IOException io) {
        io.printStackTrace();
      }
    }
//                                                    PFthreshold -> 3 RETURN
    if (ps > pfthreshold) {
//      System.out.println("ps "+ps+" "+pfthreshold);
//    } else {
// -------------------------- 2 normal ENTRANCE -----------------------------

      gy[0][0] = cfy * cty;
      gy[0][1] = sfy * cty;
      gy[0][2] = -sty;
      gy[1][0] = -sfy;
      gy[1][1] = cfy;
      gy[1][2] = 0.;
      gy[2][0] = cfy * sty;
      gy[2][1] = sfy * sty;
      gy[2][2] = cty;

      switch (imodel) {
        case 0:
          voigtsl(gy, slvoigt33);
/*          for (int i4 = 0; i4 < 6; i4++)
            System.out.println(" slvoigt :" + slvoigt33[i4]);*/
          sl[0][0] = slvoigt33[0];
          sl[1][1] = slvoigt33[1];
          sl[2][2] = slvoigt33[2];
          sl[1][2] = slvoigt33[3];
          sl[2][1] = slvoigt33[3];
          sl[0][2] = slvoigt33[4];
          sl[2][0] = slvoigt33[4];
          sl[0][1] = slvoigt33[5];
          sl[1][0] = slvoigt33[5];
          break;
        case 1:
//                                                           1         2
//                                          33-REUSSpath for REUSS and HILL
          for (kw = 0; kw < 36; ++kw) {
//                    we need only   KW = 12   13   14   15   23   27
//                    corresponds to IJ  3 3  3 4  3 5  3 6  3 1  3 2
//                                or                           3    8
//                                                           1 3  2 3
            i = mi[kw] - 1;
            if (i == 2) {
              j = mj[kw] - 1;
              sump = 0.;
              summ = 0.;
              for (kws = 0; kws < 36; ++kws) {
                is = mi[kws] - 1;
                js = mj[kws] - 1;
                for (iq = 0; iq <= 8; ++iq) {
                  sump += hs[kw][kws][iq] * hj[iq] * getsshi0()[is][js];
                  summ += hs[kw][kws][iq] * hjm[iq] * getsshi0m()[is][js];
//            System.out.println(" sum :"+kw+" "+kws+" "+iq+" "+hs[kw][kws][iq]+" "+hj[iq]+" "+sshi0[is][js]+" "+hjm[iq]+" "+sshi0m[is][js]);
                }
              }
//                                          33-REUSSpath T-reduced   in KL
              sslreuss33[j] = (sump + summ) / 2.;
//                                          33-REUSSpath T-unreduced in KL
              slreusspath33[j] = sslreuss33[j] * facun[2][j];
            }
          }
          sl[0][0] = slreusspath33[0];
          sl[1][1] = slreusspath33[1];
          sl[2][2] = slreusspath33[2];
          sl[1][2] = slreusspath33[3];
          sl[2][1] = slreusspath33[3];
          sl[0][2] = slreusspath33[4];
          sl[2][0] = slreusspath33[4];
          sl[0][1] = slreusspath33[5];
          sl[1][0] = slreusspath33[5];
          break;
        case 2:
          for (kw = 0; kw < 36; ++kw) {
//                    we need only   KW = 12   13   14   15   23   27
//                    corresponds to IJ  3 3  3 4  3 5  3 6  3 1  3 2
//                                or                           3    8
//                                                           1 3  2 3
            i = mi[kw] - 1;
            if (i == 2) {
              j = mj[kw] - 1;
              sump = 0.;
              summ = 0.;
              for (kws = 0; kws < 36; ++kws) {
                is = mi[kws] - 1;
                js = mj[kws] - 1;
                for (iq = 0; iq <= 8; ++iq) {
                  sump += hs[kw][kws][iq] * hj[iq] * getsshi0()[is][js];
                  summ += hs[kw][kws][iq] * hjm[iq] * getsshi0m()[is][js];
                }
              }
//                                          33-REUSSpath T-reduced   in KL
              sslreuss33[j] = (sump + summ) / 2.;
//                                          33-REUSSpath T-unreduced in KL
              slreusspath33[j] = sslreuss33[j] * facun[2][j];
            }
          }
          voigtsl(gy, slvoigt33);
          sl[0][0] = (slvoigt33[0] + slreusspath33[0]) / 2.;
          sl[1][1] = (slvoigt33[1] + slreusspath33[1]) / 2.;
          sl[2][2] = (slvoigt33[2] + slreusspath33[2]) / 2.;
          sl[1][2] = (slvoigt33[3] + slreusspath33[3]) / 2.;
          sl[2][1] = (slvoigt33[3] + slreusspath33[3]) / 2.;
          sl[0][2] = (slvoigt33[4] + slreusspath33[4]) / 2.;
          sl[2][0] = (slvoigt33[4] + slreusspath33[4]) / 2.;
          sl[0][1] = (slvoigt33[5] + slreusspath33[5]) / 2.;
          sl[1][0] = (slvoigt33[5] + slreusspath33[5]) / 2.;
          break;
        case 3:
//                                                     7  Entrance PathGeo
//                                                                 BPGeo
          geosls(slsgeo);
//                                                     SLS -> SL33
          for (kw = 0; kw < 36; ++kw) {
            i = mi[kw] - 1;
            j = mj[kw] - 1;
            sslgeop[i][j] = slsgeo[kw];
            if (i == 2) {
              slpathgeo33[j] = slsgeo[kw] * facun[2][j];
            }
          }
          sl[0][0] = slpathgeo33[0];
          sl[1][1] = slpathgeo33[1];
          sl[2][2] = slpathgeo33[2];
          sl[1][2] = slpathgeo33[3];
          sl[2][1] = slpathgeo33[3];
          sl[0][2] = slpathgeo33[4];
          sl[2][0] = slpathgeo33[4];
          sl[0][1] = slpathgeo33[5];
          sl[1][0] = slpathgeo33[5];
          break;
//                                                   100  Exit PathGeo
        case 4:
//                                                                 BPGeo
          geosls(slsgeo);
//                                                     SLS -> SL33
          for (kw = 0; kw < 36; ++kw) {
            i = mi[kw] - 1;
            j = mj[kw] - 1;
            sslgeop[i][j] = slsgeo[kw];
            if (i == 2) {
              slpathgeo33[j] = slsgeo[kw] * facun[2][j];
            }
          }
          subbpgeo(gy, sslgeop, sl);
          break;
        default:
          {
          }
      }
//                                             transformation to SLA33(J')
//                                             SL=SL33(J)=SL33(k,l)
      zvla1 = 0.;
      zvla2 = 0.;
      zvla3 = 0.;
      zvla4 = 0.;
      zvla5 = 0.;
      zvla6 = 0.;
      for (k = 0; k < 3; ++k) {
        for (l = 0; l < 3; ++l) {
          zvla1 += sl[k][l] * gy[k][0] * gy[l][0];
          zvla2 += sl[k][l] * gy[k][1] * gy[l][1];
          zvla3 += sl[k][l] * gy[k][2] * gy[l][2];
          zvla4 += sl[k][l] * gy[k][1] * gy[l][2];
          zvla5 += sl[k][l] * gy[k][0] * gy[l][2];
          zvla6 += sl[k][l] * gy[k][0] * gy[l][1];
        }
      }
//                                                      zero condition
      if (siegfried_strict) {
        if (Math.abs(zvla1) < 1e-4)
          zvla1 = 0.;
        if (Math.abs(zvla2) < 1e-4)
          zvla2 = 0.;
        if (Math.abs(zvla3) < 1e-4)
          zvla3 = 0.;
        if (Math.abs(zvla4) < 1e-4)
          zvla4 = 0.;
        if (Math.abs(zvla5) < 1e-4)
          zvla5 = 0.;
        if (Math.abs(zvla6) < 1e-4)
          zvla6 = 0.;
      }

      if (log_output && debug_output) {
        try {
          printLine(out, " SLA*K0 :" + zvla1 + " " + zvla2 + " " + zvla3 + " " + zvla4 + " " + zvla5 + " " + zvla6);
        } catch (IOException io) {
          io.printStackTrace();
        }
      }
//                                Reconstruction of the double dimensions
      zvla1 /= bk0;
      zvla2 /= bk0;
      zvla3 /= bk0;
      zvla4 /= bk0;
      zvla5 /= bk0;
      zvla6 /= bk0;
    } else {
// l      WRITE(16,'(A,F7.3,A,F7.3)')'   DUMMY case  !!!  PS = ',PS,
// l     *                      ' <= PFthreshold = ',PFthreshold
      zvla1 = 0; //8.8888e7f;
      zvla2 = 0; //8.8888e7f;
      zvla3 = 0; //8.8888e7f;
      zvla4 = 0; //8.8888e7f;
      zvla5 = 0; //8.8888e7f;
      zvla6 = 0; //8.8888e7f;
      nyh = -ny;

// -------------------------- 3 IMODEL independend EXIT due to PFthreshold --
    }
    double[] sla33 = new double[6];
    sla33[0] = zvla1;
    sla33[1] = zvla2;
    sla33[2] = zvla3;
    sla33[3] = zvla4;
    sla33[4] = zvla5;
    sla33[5] = zvla6;
    return sla33;
  } // pfmomprisla33

  void moments01(int imodel, int irandom, double sthi, double cthi, double sfhi, double cfhi,
                 double ctyj, double styj, double cfyj, double sfyj, double[][][] fio) {
    // System generated locals
    double r1;

    // Local variables
//    double finterpol_(double *, double *, double *);
    double al;
    int iz;
    double cb2, cg2, sb2, sg2, gam, bet;
    double ffak, beta;
    int nfis;
    double hmom0 = 0.0, gamma = 0.0, alpha = 0.0, hmomc = 0.0, hmoms = 0.0, hmom2c = 0.0, hmom3c = 0.0, hmom4c = 0.0,
            hmom0m = 0.0, hmom0p = 0.0, hmom2s = 0.0, hmom3s = 0.0, hmom4s = 0.0, hmomcm = 0.0, hmomcp = 0.0, hmomsm = 0.0,
            hmomsp = 0.0, hmom2cm = 0.0, hmom3cm = 0.0, hmom4cm = 0.0, hmom2cp = 0.0, hmom3cp = 0.0, hmom4cp = 0.0,
            hmom2sm = 0.0, hmom3sm = 0.0, hmom4sm = 0.0, hmom2sp = 0.0, hmom3sp = 0.0, hmom4sp = 0.0;


//     With FINTERPOL-interpolation, gap=0.

//     OUTPUT : Original Moments, i.e. not devided by P~, in /CSPMAS/.
//     P means +hi , M means -hi moments

    if (irandom == 1) {
      spmas[0] = 1.;
      spmas[1] = 0.;
      spmas[2] = 0.;
      spmas[3] = 0.;
      spmas[4] = 0.;
      spmas[5] = 0.;
      spmas[6] = 0.;
      spmas[7] = 0.;
      spmas[8] = 0.;
      spmasm[0] = 1.;
      spmasm[1] = 0.;
      spmasm[2] = 0.;
      spmasm[3] = 0.;
      spmasm[4] = 0.;
      spmasm[5] = 0.;
      spmasm[6] = 0.;
      spmasm[7] = 0.;
      spmasm[8] = 0.;
      return;
    }

    iz = 0;

// Projection thread loop, Simpson integration

    cb2 = cthi;
    sb2 = sthi;
    cg2 = -cfhi;
    sg2 = sfhi;
    do {
      hmom0 = 0.;
      hmomc = 0.;
      hmoms = 0.;
      hmom2c = 0.;
      hmom2s = 0.;
      hmom3c = 0.;
      hmom3s = 0.;
      hmom4c = 0.;
      hmom4s = 0.;
      for (nfis = 0; nfis <= 180; ++nfis) {
        double[] ODF_angle = g2g10(pi, -cr2[nfis], sr2[nfis], cb2, sb2, cg2, sg2, cfyj, sfyj, ctyj, styj, p2i);

//                                             simplest interpolation
//      NA=(AL-PI25)/PI5+2
//      NB=(BET-PI25)/PI5+2
//      NGA=(GAM-PI25)/PI5+2
//      FFAK=FIO(NA,NB,NGA)

        alpha = ODF_angle[0] * pif;
        beta = ODF_angle[1] * pif;
        gamma = ODF_angle[2] * pif;
//                   Interpolation in the pathpoint using FIO - full G-space

        ffak = finterpol(alpha, beta, gamma, fio);

//     IF(FFAK.LE.0.005)GOTO 3
        if (ffak > 0.005 || !siegfried_strict) {
          ffak *= pisim;
          if (nfis != 0 && nfis != 180) {
            if (MoreMath.pow_ii(nfis + 1) <= 0)
              ffak *= 2.;
            else
              ffak *= 4.;
          }
          hmom0 += ffak;
          if (imodel != 0) {
            hmomc += ffak * cr2[nfis];
            hmoms += ffak * sr2[nfis];
            hmom2c += ffak * cr4[nfis];
            hmom2s += ffak * sr4[nfis];
            if (imodel >= 3) {
              hmom3c += ffak * cr6[nfis];
              hmom3s += ffak * sr6[nfis];
              hmom4c += ffak * cr8[nfis];
              hmom4s += ffak * sr8[nfis];
            }
          }
//        System.out.println(nfis + " ffak " +ffak+" "+hmom0);
        }
      }
      if (iz != 1) {
        hmom0p = hmom0;
        hmomcp = hmomc;
        hmomsp = hmoms;
        hmom2cp = hmom2c;
        hmom2sp = hmom2s;
        hmom3cp = hmom3c;
        hmom3sp = hmom3s;
        hmom4cp = hmom4c;
        hmom4sp = hmom4s;
      }
      hmom0m = hmom0;
      hmomcm = hmomc;
      hmomsm = hmoms;
      hmom2cm = hmom2c;
      hmom2sm = hmom2s;
      hmom3cm = hmom3c;
      hmom3sm = hmom3s;
      hmom4cm = hmom4c;
      hmom4sm = hmom4s;
      spmas[0] = hmom0p;
      spmas[1] = hmomcp;
      spmas[2] = hmomsp;
      spmas[3] = hmom2cp;
      spmas[4] = hmom2sp;
      spmas[5] = hmom3cp;
      spmas[6] = hmom3sp;
      spmas[7] = hmom4cp;
      spmas[8] = hmom4sp;
      spmasm[0] = hmom0m;
      spmasm[1] = hmomcm;
      spmasm[2] = hmomsm;
      spmasm[3] = hmom2cm;
      spmasm[4] = hmom2sm;
      spmasm[5] = hmom3cm;
      spmasm[6] = hmom3sm;
      spmasm[7] = hmom4cm;
      spmasm[8] = hmom4sm;

      cb2 = -cb2;
      cg2 = -cg2;
      sg2 = -sg2;
    } while (1 > iz++);
  } // moments01_

  void prepare1() {
    // Local variables
    int i, j;
    double r2, w2i;
    int ifi;
    double fir2, fir4, fir6, fir8;

    pi = Math.acos(-1.);
    p2i = pi * 2.;
    pi5 = pi / 36.;
    pi25 = pi5 / 2.;
    pisim = .0018518518518518519;
    pif = 180. / pi;
//                                connected with T reduced -> unreduced
    w2i = 1. / Math.sqrt(2.);
    for (i = 0; i < 73; ++i)
      ifiw[i] = i * 5;
    for (i = 0; i < 3; ++i)
      for (j = 0; j < 3; ++j)
        facun[i][j] = 1.;
    for (i = 3; i < 6; ++i)
      for (j = 3; j < 6; ++j)
        facun[i][j] = .5;
    for (i = 3; i < 6; ++i)
      for (j = 0; j < 3; ++j) {
        facun[i][j] = w2i;
        facun[j][i] = w2i;
      }
//                                        connected with Voigt's notation
    mij[0][0] = 1;
    mij[0][1] = 1;
    mij[1][0] = 2;
    mij[1][1] = 2;
    mij[2][0] = 3;
    mij[2][1] = 3;
    mij[3][0] = 2;
    mij[3][1] = 3;
    mij[4][0] = 1;
    mij[4][1] = 3;
    mij[5][0] = 1;
    mij[5][1] = 2;
    mik[0][0] = 1;
    mik[0][1] = 6;
    mik[0][2] = 5;
    mik[1][0] = 6;
    mik[1][1] = 2;
    mik[1][2] = 4;
    mik[2][0] = 5;
    mik[2][1] = 4;
    mik[2][2] = 3;
//                                           2\ufffd steps along path
    r2 = pi / 90.;
    for (ifi = 0; ifi < 181; ++ifi) {
      fir2 = r2 * ifi;
      sr2[ifi] = Math.sin(fir2);
      cr2[ifi] = Math.cos(fir2);
      fir4 = fir2 * 2.;
      sr4[ifi] = Math.sin(fir4);
      cr4[ifi] = Math.cos(fir4);
      fir6 = fir2 * 3.;
      sr6[ifi] = Math.sin(fir6);
      cr6[ifi] = Math.cos(fir6);
      fir8 = fir2 * 4.;
      sr8[ifi] = Math.sin(fir8);
      cr8[ifi] = Math.cos(fir8);
    }
    return;
  }

  void prepare3() {
    // Local variables
    int i;
    double arg, dpi5;

    dpi5 = 5.0 * Constants.DEGTOPI;
//                                    TRIGS for bulk WWa-calculation
//                                    not for random, REUSS or PathGeo
    for (i = 0; i < 73; ++i) {
      arg = dpi5 * i;
      trigs[0][i] = 1.;
      trigs[1][i] = Math.cos(arg);
      trigs[2][i] = Math.cos(arg * 2.);
      trigs[3][i] = Math.cos(arg * 3.);
      trigs[4][i] = Math.cos(arg * 4.);
      trigs[5][i] = Math.sin(arg);
      trigs[6][i] = Math.sin(arg * 2.);
      trigs[7][i] = Math.sin(arg * 3.);
      trigs[8][i] = Math.sin(arg * 4.);
    }
    return;
  }

  void subhkwkw() {
// Initialized data

    int msin[][] = {{0, 2, 0, 0, 0, 1}, {2, 0, 0, 0, 0, 1},
                    {0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 1, 0}, {0, 0, 0, 1, 0, 0}, {1, 1, 0, 0, 0, 8}};
    int mcos[][] = {{2, 0, 0, 0, 0, 1}, {0, 2, 0, 0, 0, 1},
                    {0, 0, 0, 0, 0, 0}, {0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 1, 0}, {1, 1, 0, 0, 0, 8}};
    double gew[][] = {{1., 1., 0., 0., 0., 8.}, {1., 1.,
                                                 0., 0., 0., 8.}, {0., 0., 1., 0., 0., 0.}, {0., 0., 0., 1., 1., 0.},
                      {0., 0., 0., -1., 1., 0.}, {8., 8., 0., 0., 0., 8.}};

// System generated locals
    double r1;

// Local variables
    double c[] = new double[15], d[][] = new double[15][9];
    int i, j, k;
    double h2, h4, h8, w2, h34, h38;
    int ip, iq, is, js, kw;
    double w2m;
    int kws;
    double sum;
    int icos;
    int isin;
    double geww;
    int irang, icosh, isinh;

// Berechnet f?r die Residual GEO Problematik das MASSIV HS(KW,KWS,iq)
// (36,36,9), welches die Beziehung herstellt zu den SLS(KW,hi,y), den
// 9 Momenten Jiq (1,cos,sin,...sin4) und den SSKWS(hi) (fr?here uhi0)
// Siehe Arbeitsseiten November 94

//                                      'S' alles T-Martix reduziert !!!!
//                                                                   !!!!
// KW = 1,36  Kodierung I,J  (I,J=1,6 - reduzierte Laufindexe)

// Wenn man entreduziert mit 'FAK', wie im GEO-Programm, gibt es wieder
// Laufindexe I,J (1-6, oder 1-9), die dann nach Voigt in i1,i2;j1,j2
// entschluesselt werden k\ufffdnnen.
// Der Code liegt fest wegen Table 2, wo die geometrischen Indexe nicht
// einfach manipuliert werden koennen.
// Wegen der Vertauschungssymmetrie bei den entreduzierten SL und S(hi)
// ist es unwesentlich, ob ich in der Reihenfolge beim Ausdruck dieser
// S dann unter I=4 31 oder 13 verstehe. Nur beim Uebergang SL->SLA
// muss ich mich auf eine Variante festlegen. Spielt hier keine Rolle.

//        CHARACTER FNAME*40
//                                       '8' means dummy
    w2 = Math.sqrt(2.);
    w2m = -w2;
    gew[5][0] = w2;
    gew[5][1] = w2m;
    gew[0][5] = w2m;
    gew[1][5] = w2;
//        FNAME='HSKWKWS.LST'
//        OPEN(161,FILE=FNAME)
//                                                    d-Massiv auff?llen
    h2 = .5;
    h4 = .25;
    h34 = h4 * 3.0;
    h8 = .125;
    h38 = h8 * 3.0;
    for (ip = 0; ip < 15; ++ip)
      for (iq = 0; iq < 9; ++iq)
        d[ip][iq] = 0.;
    d[0][0] = 1.;
    d[1][1] = 1.;
    d[2][2] = 1.;
    d[3][0] = h2;
    d[3][3] = h2;
    d[4][4] = h2;
    d[5][0] = h2;
    d[5][3] = -h2;
    d[6][1] = h34;
    d[6][5] = h4;
    d[7][2] = h4;
    d[7][6] = h4;
    d[8][1] = h4;
    d[8][5] = -h4;
    d[9][2] = h34;
    d[9][6] = -h4;
    d[10][0] = h38;
    d[10][3] = h2;
    d[10][7] = h8;
    d[11][4] = h4;
    d[11][8] = h8;
    d[12][0] = h8;
    d[12][7] = -h8;
    d[13][4] = h4;
    d[13][8] = -h8;
    d[14][0] = h38;
    d[14][3] = -h2;
    d[14][7] = h8;

/*    for (ip = 0; ip < 15; ++ip)
      for (iq = 0; iq < 9; ++iq)
       System.out.println("d "+ip+" "+iq+" "+d[ip][iq]);*/

//        WRITE(161,40)d(13,0)
//                                                       KW,KWS-Schleifen
    for (kw = 0; kw < 36; ++kw) {
//        WRITE(*,41)KW
      i = mi[kw] - 1;
      j = mj[kw] - 1;
      for (kws = 0; kws < 36; ++kws) {
        is = mi[kws] - 1;
        js = mj[kws] - 1;

        for (ip = 0; ip < 15; ++ip)
          c[ip] = 0.;
//                                                       C-Massiv berechnen
        if (!(gew[is][i] == 0. || gew[js][j] == 0.)) {
          if (i != 2 || is != 2 || j != 2 || js != 2) {
            if ((i == 5 && is == 5) || (j == 5 && js == 5)) {
              if (i == 5 && is == 5) {
                if (j == 5 && js == 5) {
                  c[10] = 1.;
                  c[12] = -2.;
                  c[14] = 1.;
                } else {

//          			J,JS.NE.6 together, I=IS=6

                  for (k = 1; k <= 2; ++k) {
                    geww = gew[js][j];
                    icos = mcos[js][j];
                    isin = msin[js][j];
                    if (k == 2) {
                      isinh = isin + 2;
                      irang = icos + isinh;
                      r1 = -geww;
                      iord(irang, icos, r1, c);
                    } else {
                      icosh = icos + 2;
                      irang = icosh + isin;
                      iord(irang, icosh, geww, c);
                    }
                  }
                }
              } else {

//          		I,IS.NE.6 together, J=JS=6

                for (k = 1; k <= 2; ++k) {
                  geww = gew[is][i];
                  icos = mcos[is][i];
                  isin = msin[is][i];
                  if (k == 2) {
                    isinh = isin + 2;
                    irang = icos + isinh;
                    r1 = -geww;
                    iord(irang, icos, r1, c);
                  } else {
                    icosh = icos + 2;
                    irang = icosh + isin;
                    iord(irang, icosh, geww, c);
                  }
                }
              }
            } else {
              geww = gew[is][i] * gew[js][j];
              icos = mcos[is][i] + mcos[js][j];
              isin = msin[is][i] + msin[js][j];
              irang = icos + isin;
              iord(irang, icos, geww, c);
            }
          } else
            c[0] = 1.;
        }
//                                                    HS-Massiv berechnen
        for (iq = 0; iq < 9; ++iq) {
          sum = 0.;
          for (ip = 0; ip < 15; ++ip) {
            sum += c[ip] * d[ip][iq];
          }
          hs[kw][kws][iq] = sum;
        }
      }
    }

    return;
  }

  double e0prep(int imodel) {

//                                   READ of file E0INPUT.DAT with ISTIFF
//                                   putting the file data into E0(6,6)
// luca, doesn't do nothing at the moment
    complian();
//            *************
//                                   OUTPUT independend on ISTIFF=0 or 1
//                                   always S0((6,6) T-unreduced
//                                   COMMON /CS0/S0(6,6)

    c0 = inve6x6tunred(s0);
    if (log_output) {
      try {
        printLine(out, "   Unreduced S0");
        for (int ihk1 = 0; ihk1 < 6; ihk1++) {
          for (int jhk1 = 0; jhk1 < 6; jhk1++)
            printString(out, "   " + s0[ihk1][jhk1]);
          newLine(out);
        }
        printLine(out, "   Unreduced C0");
        for (int ihk1 = 0; ihk1 < 6; ihk1++) {
          for (int jhk1 = 0; jhk1 < 6; jhk1++)
            printString(out, "   " + c0[ihk1][jhk1]);
          newLine(out);
        }
      } catch (IOException io) {
        io.printStackTrace();
      }
    }
//     ********************

//                                   INP S0 T-unreduced single compliances
//                                   OUT C0 T-unreduced single stiffnesses
//                                   COMMON /CC0/C0(6,6)

// l        WRITE(16,*)'   Unreduced S0'
//    for (i = 1; i <= 6; ++i) {
// l          WRITE(16,'(6E13.5)')(S0(I,J),J=1,6)
//    }
// l        WRITE(16,*)'   Unreduced C0'
//    for (i = 1; i <= 6; ++i) {
// l          WRITE(16,'(6E13.5)')(C0(I,J),J=1,6)
//    }
//                                   K0 determination
    double bk0 = 0.;
    for (int i = 0; i < 3; ++i)
      for (int j = 0; j < 3; ++j)
        bk0 += c0[i][j];
    bk0 /= 9.;
    if (log_output) {
      try {
        printLine(out, "         Single crystalline bulk modulus K0 = C0iijj/9 = " + bk0);
      } catch (IOException io) {
        io.printStackTrace();
      }
    }
// l      WRITE(16,'(/,A,E13.5,/)')
// l     *'         Single crystalline bulk modulus K0 = C0iijj/9 = ',BK0
//                                              S0 and C0 K0-reduction
//                                                        ############
    for (int i = 0; i < 6; ++i)
      for (int j = 0; j < 6; ++j) {
        s0[i][j] = s0[i][j] * bk0;
        c0[i][j] = c0[i][j] / bk0;
      }
//                                              putting of S0 into E0
    for (int i = 0; i < 6; ++i)
      for (int j = 0; j < 6; ++j)
        e0[i][j] = s0[i][j];
/*    for (int ihk1 = 0; ihk1 < 6; ihk1++)
      for (int jhk1 = 0; jhk1 < 6; jhk1++)
    System.out.println("en0 " + e0[ihk1][jhk1]);*/
    return bk0;
  }

  void complian() {
    int istiff = 1;

/*                                       August 94 ISTIFF introduced
 *    S.Matthies, november 1993, Dresden
 *
 *    Input of elastic constants > Input File on unit 11
 *    ISTIFF=1 indicates    Stiffness data, otherwise Compliances
 *
 *    For ISTIFF=1 the subroutine inverts the INPUT unreduced stiffness
 *                 data into T unreduced compliance data.
 *
 *    OUTPUT always compliances in 'E0INV(6,6)' COMMON/CS0/
 *
 *    Input of the true values (! not renormalized by Voigt or Wooster schemes)
 *    E0(i1i2,j1j2) (i,j=1,2,3) of the elastic tensor components (stiffness or
 *    compliance) using the Voigt Index Code :
 *
 *    I   1  2  3  4  5  6  enlarged code  7  8  9
 *
 *    i1  1  2  3  2  3  1                 3  1  2
 *
 *    i2  1  2  3  3  1  2                 2  3  1
 *
 * 	If E corresponds to the stiffness C so EINV means compliance S
 * 	and reverse
 *
 *    double INPUT : Only the 21 numbers (upper triangle)
 *
 *                 E0(I,J)  I=1,6 ; J=I,6
 *
 * 	Below an example for an E0-DATA FILE is given. True, not modified
 *       values !!!  23 strokes (Textstroke FORMAT A70)  :
 *
 * 	TEXT stroke, e.g. Copper,stiffness
 *       1               ISTIFF
 * 	168.4           E0(1,1)
 * 	121.4           E0(1,2)
 * 	121.4           E0(1,3)
 * 	0.              E0(1,4)
 *  0.              E0(1,5)
 * 	0.              E0(1,6)
 * 	168.4           E0(2,2)
 * 	121.4           E0(2,3)
 * 	0.              E0(2,4)
 * 	0.              E0(2,5)
 * 	0.              E0(2,6)
 * 	168.4           E0(3,3)
 * 	0.              E0(3,4)
 * 	0.              E0(3,5)
 * 	0.              E0(3,6)
 * 	75.5            E0(4,4)
 * 	0.              E0(4,5)
 * 	0.              E0(4,6)
 *  75.5            E0(5,5)
 *  0.              E0(5,6)
 *  75.5            E0(6,6)
 */
    for (int i = 0; i < 6; ++i)
      for (int j = 0; j < 6; ++j)
        s0[i][j] = e0[i][j];

// l        WRITE(16,*)'    E0-matrix :'
// l        WRITE(16,43) ((E0(I,J),J=1,6),I=1,6)
    if (log_output) {
      try {
        printLine(out, "    Matrix E0-matrix :");
        for (int ihk1 = 0; ihk1 < 6; ihk1++) {
          for (int jhk1 = 0; jhk1 < 6; jhk1++)
            printString(out, "    " + e0[ihk1][jhk1]);
          newLine(out);
        }
      } catch (IOException io) {
        io.printStackTrace();
      }
    }

    if (istiff == 1) {
      s0 = inve6x6tunred(e0);
      if (log_output) {
        try {
          printLine(out, "    Unreduced invers matrix E0INV :");
          for (int ihk1 = 0; ihk1 < 6; ihk1++) {
            for (int jhk1 = 0; jhk1 < 6; jhk1++)
              printString(out, "    " + s0[ihk1][jhk1]);
            newLine(out);
          }
        } catch (IOException io) {
          io.printStackTrace();
        }
      }

// l        WRITE(16,*)'   Unreduced invers matrix E0INV'
      for (int i = 0; i < 6; ++i) {
// l        WRITE(16,43)(E0INV(I,J),J=1,6)
      }
    }

// l  43    FORMAT(6E13.5)
// l  48    FORMAT(A70)
// l  49    FORMAT(A70,/,'  ISTIFF = ',I3)
    return;
  } // complian

  public static final double[][] inve6x6tunred(double[][] a) {
    // Local variables
    double[][] ainv = new double[6][6];
    double e;
    int i, j, k;
    double[][] p = new double[6][6];
    double w2;
    double[][] as = new double[6][6];
    double fak, sum;
    double[] eigw = new double[6];
    int ierr;
    double[][] work = new double[6][6];
    double[][] ainvs = new double[6][6];
    int igreek, jgreek;
    double[] eigwin = new double[6];

//     INPUT A(6,6) symmetric not T reduced
//     A T unreduced matrix must be inverted into the reduced form
//     and then be unreduced again

    // Function Body
    w2 = Math.sqrt(2.);
    for (igreek = 0; igreek < 6; ++igreek) {
      for (jgreek = igreek; jgreek < 6; ++jgreek) {
        fak = 1.;
        if (igreek >= 3) {
          fak *= w2;
        }
        if (jgreek >= 3) {
          fak *= w2;
        }
        e = a[igreek][jgreek] * fak;
        as[igreek][jgreek] = e;
        as[jgreek][igreek] = e;
      }
    }
// 	WRITE(16,*)'    A-matrix :'
// 	WRITE(16,43) ((A(I,J),J=1,6),I=1,6)
// 	WRITE(16,*)'    AS-matrix :'
// 	WRITE(16,43) ((AS(I,J),J=1,6),I=1,6)
/*    for (igreek = 0; igreek < 6; ++igreek)
      for (jgreek = 0; jgreek < 6; ++jgreek)
        System.out.println("    AS-matrix : " + as[igreek][jgreek]);*/

// 	Diagonalization of AS

// 	CALL DIAG6(AS,EIGW,P)
    ierr = eisrs1(c6, c6, as, eigw, p, work);
//    System.out.println("Error n " + ierr);

//       WRITE(16,46) IERR
//  46	FORMAT('  IERR = ',I3)
// 	WRITE(16,*)'  EIGENVALUES of AS and P-matrix  (P-1)*AS*P =',
//     1  ' diagonal'
// 	WRITE(16,43) (EIGW(I),I=1,6)
/*    System.out.println("  EIGENVALUES of AS and P-matrix  (P-1)*AS*P =");
    for (igreek = 0; igreek < 6; ++igreek)
      System.out.println("    EIGW : " + eigw[igreek]);*/
// 	WRITE(16,*)'  MATRIX P'
//     DO 3 I=1,6
// 	WRITE(16,43)(P(I,J),J=1,6)
//  3	CONTINUE
/*    System.out.println("  MATRIX P");
    for (igreek = 0; igreek < 6; ++igreek)
      for (jgreek = 0; jgreek < 6; ++jgreek)
        System.out.println("    P : " + p[igreek][jgreek]);*/

// 	Construction of the invers Tensor-matrix

    for (i = 0; i < 6; ++i) {
      if (eigw[i] == 0)
        System.out.println("Problem with eigenValues of Stiffness matrix " + i + " ");
// l        IF(EIGW(I).EQ.0.)WRITE(16,*)
// l     *       ' Eigenvalue=0 ; SUBROUTINE INVE6x6Tunred -> STOP'
      eigwin[i] = 1. / eigw[i];

//                                              automatic STOP for 1/0
    }

    for (i = 0; i < 6; ++i) {
      for (j = 0; j < 6; ++j) {
        sum = 0.;
        for (k = 0; k < 6; ++k) {
          sum += p[i][k] * p[j][k] * eigwin[k];
        }
        ainvs[i][j] = sum;
        ainvs[j][i] = sum;
      }
    }
// 	WRITE(16,*)'   invers AS-matrix :'
// 	WRITE(16,43)((AINVS(I,J),J=1,6),I=1,6)

// 	Transfer to the unreduced matrix AINV(6,6)

//       WRITE(16,*)'  Unreduced invers matrix AINV '
    for (i = 0; i < 6; ++i) {
      for (j = 0; j < 6; ++j) {
        fak = 1.;
        if (i >= 3) {
          fak = w2;
        }
        if (j >= 3) {
          fak *= w2;
        }
        ainv[i][j] = ainvs[i][j] / fak;
      }
// 	WRITE(16,43)(AINV(I,J),J=1,6)
    }
    return ainv;
// l  43    FORMAT(6E13.5)
  } // inve6x6tunred

  void igaigbprep(int iga, int igb, int irandom) {
// Initialized data

    int[][] mge = {{1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                    0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0}, {0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1,
                                                                0, 0, 0, 0, 0, 0}, {1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0}, {0, 1,
                                                                                                                                                                 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0}, {0, 1, 0, 1, 1, 1, 0, 1, 0, 1,
                                                                                                                                                                                                                                        0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0}, {0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0,
                                                                                                                                                                                                                                                                                       0, 0, 0, 0, 0, 1, 1}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0}, {0,
                                                                                                                                                                                                                                                                                                                                                                                           0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0}, {0, 1, 1, 0, 1, 0, 0, 0, 0,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                     0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       0, 0, 0, 0, 0, 1, 1, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 2, 4, 4, 8, 12, 24, 3, 6, 6,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 12}};

// System generated locals
    int i1;

// Local variables
    int ij, ize, izgb;
    int[] mgb = new int[24];

//       Only the READ activity is of interest, all other data for
//       Print only. No OUTPUT of them.
// l      CHARACTER IGNAME(11)*4
// l      DATA IGNAME/
// l     1 '(C1)','(C2)','(D2)','(C4)','(D4)','( T)','( O)',
// l     2 '(C3)','(D3)','(C6)','(D6)'/
// l      READ (5,*) IGB

//                   DETERMINATION OF THE SYMMETRY ELEMENTS TO BE USED

    izgb = mge[igb - 1][24];
// l      WRITE(16,160)IGB,IGNAME(IGB),IZGB
    if (irandom != 1) {
      if (igb < 8) {
        ize = 24;
      } else {
        ize = 12;
      }
      for (ij = 0; ij < ize; ++ij) {
        mgb[ij] = mge[igb - 1][ij];
      }
// l      IF (IGB.LT.8) WRITE (16,170) (MGB(IJ),IJ=1,IZE)
// l      IF (IGB.GT.7) WRITE (16,180) (MGB(IJ),IJ=1,IZE)


// INPUT : dummy for IRANDOM=1                   SAMPLE SYMMETRY
// *****                                         ---------------

    }
// l 			READ (5,*) IGA
// l      WRITE(16,260)IGA,IGNAME(IGA)
    return;
// l  160 FORMAT(
// l     3 5X,'CRYSTAL SYMMETRY   IGB = ',I2,2X,A4,
// l     4 4X,'NUMBER OF GROUP ELEMENTS  IZGB = ',I2,/)
// l  170 FORMAT(5X,'ELEMENT-CODE MGB(K,J) : ',
// l     1 24(I2)/,3(29X,24(I2)/)/)
// l  180 FORMAT(5X,'ELEMENT-CODE MGB(K,J) : ',
// l     1 12(I2)/,3(29X,12(I2)/)/)
// l  260 FORMAT(
// l     1 5X,'SAMPLE SYMMETRY   IGA = ',I2,2X,A4,/)
  } // igaigbprep

  double[][][] odfprep(int iga, int igb, double[][][] fio) {
//    OPENING of the ODF-file, ODF-INPUT into ARRAY FIO COMMON/CFIO/

// l                CALL ODFINP(IGA,IGB)
//                    ***************

//    ENLARGEMENT OF FIO GIVEN IN THE IGA,IGB ELEMENTARY REGION
//    TO THE COMPLETE G-SPACE

//    Uwimvuo.fiottu_(fio, igb, alphama);

// l                CALL FIOTT(IGA,IGB)
//                    **************

//    CONSTRUCTION OF THE PHONREDUCED
//    VG/(8PI*PI)-WEIGHTED TT ODF FTT(73,37,73) FOR COMMON/CF/F

//    IRED=0 hard wired
    double[][][] fio_corrected = fttcon(1, fio);
//                    **************
    return fio_corrected;
  } // odfprep




//   *********************************************

  void bulkprep(int imodel, int irandom, double[][][] fio) {
    int i, j, ical, igeo;

//   Called only for IMODEL=0,2,4
//   Calculation of the bulk EA or EGEOM for IMODEL=0,2 or IMODEL=4
//   correspondingly                         Voigt,Hill    BPGeo

//   INPUT E0 containing S0 from SUBROUTINE E0PREP

//   OUTPUT by COMMON/CEGEOM/EGEOM(6,6),EA(6,6) and COMMON /CSVOIGT/SVOIGT(6,6)
    ical = 1;
//  ICAL=1 -> WWARIM calculation by f(g), phonreduced and VG/8PI**2 weighted,
//  sitting in /CF/F from FTTCON (IRANDOM.NE.1) or
//  for IRANDOM=1 simply by WWARIM=WWARIR
    if (imodel == 4) {
//                                                           GEO
      igeo = 1;
      subsgeo(igeo, irandom, ical, fio);
//                  ************

// l      WRITE(16,*)'    S-GEObulk T-unreduced in KA :'
// l      WRITE(16,'(6E13.5)') ((EGEOM(I,J),J=1,6),I=1,6)
      if (log_output) {
        try {
          printLine(out, "    S-GEObulk T-unreduced in KA :");
          for (i = 0; i < 6; ++i) {
            for (j = 0; j < 6; ++j) {
              printString(out, "    " + egeom[i][j]);
            }
            newLine(out);
          }
        } catch (IOException io) {
          io.printStackTrace();
        }
      }
      return;
    }
    igeo = 0;
//     ICAL=1
//                                          C0 into E0
    for (i = 0; i < 6; ++i) {
      for (j = 0; j < 6; ++j) {
        e0[i][j] = c0[i][j];
//    System.out.println("e0 " + e0[i][j]);
      }
    }
// l      WRITE(16,*)'    C0-matrix T unreduced :'
// l      WRITE(16,'(6E13.5)') ((E0(I,J),J=1,6),I=1,6)
    if (log_output) {
      try {
        printLine(out, "    C0-matrix T unreduced :");
        for (i = 0; i < 6; ++i) {
          for (j = 0; j < 6; ++j) {
            printString(out, "   " + e0[i][j]);
          }
          newLine(out);
        }
      } catch (IOException io) {
        io.printStackTrace();
      }
    }
    subsgeo(igeo, irandom, ical, fio);
//             ************
    svoigt = inve6x6tunred(ea);
//             ************************
// l      WRITE(16,*)'    S-VOIGT-bulk T-unreduced in KA :'
// l      WRITE(16,'(6E13.5)') ((SVOIGT(I,J),J=1,6),I=1,6)
    if (log_output) {
      try {
        printLine(out, "    S-VOIGT-bulk T-unreduced in KA :");
        for (i = 0; i < 6; ++i) {
          for (j = 0; j < 6; ++j) {
            printString(out, "   " + svoigt[i][j]);
          }
          newLine(out);
        }
      } catch (IOException io) {
        io.printStackTrace();
      }
    }
    return;
  } // bulkprep

  void latticeprep(double pif) {
// Local variables
    double al, bl, cl, harg, betal, gammal, alphal;


// INPUT :                                              LATTICE DATA
// *****                                                ------------

// l      READ (5,*) al,bl,cl,alphal,betal,gammal
// l      WRITE(16,'(A,/,6E13.6,/)')' lattice: a,b,c,alpha,beta,gamma ',
// l     * al,bl,cl,alphal,betal,gammal
    Phase aphase = (Phase) getParent();
    al = aphase.getFullCellValue(0);
    bl = aphase.getFullCellValue(1);
    cl = aphase.getFullCellValue(2);
    alphal = aphase.getFullCellValue(3);
    betal = aphase.getFullCellValue(4);
    gammal = aphase.getFullCellValue(5);
    cda = cl / al;
    cdb = cl / bl;
    harg = gammal / pif;
    s12l = Math.sin(harg);
    c12l = Math.cos(harg);
    harg = alphal / pif;
    s23l = Math.sin(harg);
    c23l = Math.cos(harg);
    harg = betal / pif;
    s31l = Math.sin(harg);
    c31l = Math.cos(harg);
//      WRITE(16,40)S12L,C12L,S23L,C23L,S31L,C31L,CDA,CDB
//  40  FORMAT(/,'  Lattice parameters :'/,
//     W  '  SIN(Alpha12) = ',F7.4,'  COS(Alpha12) = ',F7.4,/
//     W  '  SIN(Alpha23) = ',F7.4,'  COS(Alpha23) = ',F7.4,/
//     W  '  SIN(Alpha31) = ',F7.4,'  COS(Alpha31) = ',F7.4,/
//     W  '  c/a = ',E12.5,'  c/b = ',E12.5,/)
    return;
  } // latticeprep

  void voigtsl(double[][] gy, double[] slvoigt33) {
    int i, j, k, l, m, n, ih, jh, ii, ji, ij, jj;

//                                INPUT  SVOIGT    = SA bulk T-unreduced full
//                                OUTPUT SLVOIGT33 = SL bulk T-unreduced 33
// Parameter adjustments
//    --slvoigt33;
//    gy -= 4;

// Function Body
    for (j = 0; j < 6; ++j) {
      slvoigt33[j] = 0.;
    }
    for (k = 0; k < 3; ++k) {
      for (l = 0; l < 3; ++l) {
        ih = mik[k][l] - 1;
        for (m = 0; m < 3; ++m) {
          for (n = 0; n < 3; ++n) {
            jh = mik[m][n] - 1;
//                                                 DO I=1,6 would be full
            for (i = 2; i <= 2; ++i) {
              ii = mij[i][0] - 1;
              ji = mij[i][1] - 1;
              for (j = 0; j < 6; ++j) {
                ij = mij[j][0] - 1;
                jj = mij[j][1] - 1;
                slvoigt33[j] += svoigt[ih][jh] * gy[ii][k] * gy[ji][l] * gy[ij][m] * gy[jj][n];
              }
            }
          }
        }
      }
    }
    return;
  } // voigtsl

  double winkeldeg(double pif, double cosx, double sinx) {
    double wink = 0.0;
    if (Math.abs(cosx) < 1.) {
//        WINK=ACOS(COSX)/PIF
      wink = Math.acos(cosx) * pif;
      if (sinx < 0.) {
        wink = 360. - wink;
      }
// l        RETURN
    } else {
      if (cosx >= 1.) {
        wink = 0.;
      } else {
        wink = 180.;
      }
    }
    return wink;
  } // winkeldeg

  void subshi0(int reflIndex, double sthi0, double cthi0, double sfhi0, double cfhi0) {
// Local variables
    int istvoigt;
    double[][] a = new double[3][3];
    int i, j, k, l, m, n;
    double w2, am[][] = new double[3][3], sh;
    int is, it;
    double fak;
    int mue, nue;
    double shm, sumu, sumum;
    int ijvoigt, klvoigt, mnvoigt;

//                                            S.M., Dresden Nov. 94

//  The SUBROUTINE calculates the (twice-symmetric) hi0-transferred
//  single crystal compliance components

//  Shi0(I,J)=Sabcd(hi0)=Aai()*Abj()*Ack()*Adl({hi0,0})*S0ijkl I-> a,b
//                                       Voigt code            J-> c,d
//                                                             I,J=1,6
//  -hi -> Shi0M(I,J)
//                     reduced forms SShi0(I,J),SShi0M(I,J)

//  Input : sin, cos Thetahi,Phihi
//          S0 - Compliances

//       DIMENSION SHI0(6,6),SHI0M(6,6)               array not used
    w2 = Math.sqrt(2.);
// 				                 A({hi0})-matrix
    a[0][0] = cfhi0 * cthi0;
    a[0][1] = sfhi0 * cthi0;
    a[0][2] = -sthi0;
    a[1][0] = -sfhi0;
    a[1][1] = cfhi0;
    a[1][2] = 0.;
    a[2][0] = cfhi0 * sthi0;
    a[2][1] = sfhi0 * sthi0;
    a[2][2] = cthi0;

    am[0][0] = a[0][0];
    am[0][1] = a[0][1];
    am[0][2] = a[0][2];
    am[1][0] = -a[1][0];
    am[1][1] = -a[1][1];
    am[1][2] = 0.;
    am[2][0] = -a[2][0];
    am[2][1] = -a[2][1];
    am[2][2] = -a[2][2];
    for (istvoigt = 0; istvoigt < 6; ++istvoigt) {
      is = mivoigt[istvoigt] - 1;
      it = mjvoigt[istvoigt] - 1;
      for (mnvoigt = istvoigt; mnvoigt < 6; ++mnvoigt) {
        m = mivoigt[mnvoigt] - 1;
        n = mjvoigt[mnvoigt] - 1;
        sumu = 0.;
        sumum = 0.;
        for (ijvoigt = 0; ijvoigt < 9; ++ijvoigt) {
          mue = ijvoigt;
          i = mivoigt[ijvoigt] - 1;
          j = mjvoigt[ijvoigt] - 1;
          if (ijvoigt >= 6) {
            mue += -3;
          }
          for (klvoigt = 0; klvoigt < 9; ++klvoigt) {
            nue = klvoigt;
            k = mivoigt[klvoigt] - 1;
            l = mjvoigt[klvoigt] - 1;
            if (klvoigt >= 6) {
              nue += -3;
            }
            sumu += a[is][i] * a[it][j] * a[m][k] * a[n][l] * s0[mue][nue];
            sumum += am[is][i] * am[it][j] * am[m][k] * am[n][l] * s0[mue][nue];
          }
        }
//        SHI0(ISTVOIGT,MNVOIGT)=SUMU
//        SHI0M(ISTVOIGT,MNVOIGT)=SUMUM
// l        WRITE(16,4)IS,IT,M,N,SUMU
//                                                   Reduction
/*        if (debug_output) {
          try {
            printLine(out, " SHI0 (" + is + " " + it + " " + m + " " + n + ")= " + sumu);
          } catch (IOException io) {
            io.printStackTrace();
          }
        }*/
        fak = 1.;
        if (istvoigt >= 3) {
          fak = w2;
        }
        if (mnvoigt >= 3) {
          fak *= w2;
        }
        sh = sumu * fak;
        shm = sumum * fak;
//                                                   Symmetrization
        sshi0[reflIndex][istvoigt][mnvoigt] = sh;
//        WRITE(16,41)IS,IT,M,N,SH
        sshi0[reflIndex][mnvoigt][istvoigt] = sh;
        sshi0m[reflIndex][istvoigt][mnvoigt] = shm;
//        WRITE(16,42)IS,IT,M,N,SHM
        sshi0m[reflIndex][mnvoigt][istvoigt] = shm;
      }
    }
// l   4    FORMAT(' SHI0  (',2I2,1X,2I2,') = ',E13.5)
// l   41   FORMAT(' SSHI0 (',2I2,1X,2I2,') = ',E13.5)
// l   42   FORMAT(' SSHI0M(',2I2,1X,2I2,') = ',E13.5)
    return;
  } // subshi0

  void geosls(double[] slsgeo) {

// Local variables
    int i, j, k;
    double[][] p0 = new double[6][6];
    int iq, is, js, kw;
    double sum;
    int kws, ierr;
    double summ, sump;
    double[] eigw0 = new double[6], eigex = new double[6];
    double[][] hlogm = new double[6][6], hlogp = new double[6][6], slslog = new double[6][6],
            work = new double[6][6];
    double summm;
    double[] eiglog = new double[6];

//     INPUT  -> HS-matrix,P~-reduced moments HJ(hi),HJM(-hi),
//               SSHI0,SSHI0M-arrays
//     OUTPUT -> SLSGEO(36) "PATHGEO"
//      COMMON /CGEO/SLSGEO(36)

// Function Body
// l      IOUT=16

//       Diagonalization of SSHI0

    ierr = eisrs1(c6, c6, getsshi0(), eigw0, p0, work);

// l        IF(IERR.NE.0)WRITE(IOUT,46) IERR
//        WRITE(IOUT,*)' EIGENVALUES SSHI0P,P0-matrix :(P0-1)*E0S*P0 =',
//     1  ' diagonal'
//        WRITE(IOUT,43) (EIGW0(I),I=1,6)
//        WRITE(IOUT,*)'  MATRIX P0'
    for (i = 0; i < 6; ++i) {
//       WRITE(IOUT,43)(P0(I,J),J=1,6)
// l        IF(EIGW0(I).LE.0.)WRITE(16,*)
// l     *           ' Eigenvalue <= 0.; SUBROUTINE GEOSLS -> STOP'
      eiglog[i] = Math.log(eigw0[i]);
//                                    automatic STOP for ALOG(<=0.)
    }

//       Construction of the HLOGP-matrix

    for (i = 0; i < 6; ++i) {
      for (j = i; j < 6; ++j) {
        sum = 0.;
        for (k = 0; k < 6; ++k) {
          sum += p0[i][k] * p0[j][k] * eiglog[k];
        }
        hlogp[i][j] = sum;
        hlogp[j][i] = sum;
      }
    }
//       WRITE(IOUT,*)'   HLOGP-matrix :'
//       WRITE(16,43)((HLOGP(I,J),J=1,6),I=1,6)

//       Diagonalization of SSHI0M

    ierr = eisrs1(c6, c6, getsshi0m(), eigw0, p0, work);

// l        IF(IERR.NE.0)WRITE(IOUT,46) IERR
//        WRITE(IOUT,*)' EIGENVALUES SSHI0M,P0-matrix :(P0-1)*E0S*P0 =',
//     1  ' diagonal'
//        WRITE(IOUT,43) (EIGW0(I),I=1,6)
//        WRITE(IOUT,*)'  MATRIX P0'
    for (i = 0; i < 6; ++i) {
//       WRITE(IOUT,43)(P0(I,J),J=1,6)
// l        IF(EIGW0(I).LE.0.)WRITE(16,*)
// l     *           ' Eigenvalue <= 0.; SUBROUTINE GEOSLS -> STOP'
      eiglog[i] = Math.log(eigw0[i]);
//                                    automatic STOP for ALOG(<=0.)
    }

//       Construction of the HLOGM-matrix

    for (i = 0; i < 6; ++i) {
      for (j = i; j < 6; ++j) {
        sum = 0.;
        for (k = 0; k < 6; ++k)
          sum += p0[i][k] * p0[j][k] * eiglog[k];
        hlogm[i][j] = sum;
        hlogm[j][i] = sum;
      }
    }
//       WRITE(IOUT,*)'   HLOGM-matrix :'
//       WRITE(16,43)((HLOGM(I,J),J=1,6),I=1,6)

//                   arithmetic averaging (+- path) of HLOGP and HLOGM

//      DO 60 KW=1,36
    for (kw = 0; kw < 21; ++kw) {
      i = mi[kw] - 1;
      j = mj[kw] - 1;
      sump = 0.;
      summ = 0.;
      for (kws = 0; kws < 36; ++kws) {
        is = mi[kws] - 1;
        js = mj[kws] - 1;
        for (iq = 0; iq <= 8; ++iq) {
          sump += hs[kw][kws][iq] * hj[iq] * hlogp[is][js];
          summ += hs[kw][kws][iq] * hjm[iq] * hlogm[is][js];
        }
      }
      summm = (sump + summ) / 2.;
      slslog[i][j] = summm;
      slslog[j][i] = summm;
    }
//                                                     <Ln(SLS)>
//                                                    e
//       Diagonalization of SLSLOG

    ierr = eisrs1(c6, c6, slslog, eigw0, p0, work);

// l        IF(IERR.NE.0)WRITE(IOUT,46) IERR
//        WRITE(IOUT,*)' EIGENVALUES SLSLOG,P0-matrix :(P0-1)*E0S*P0 =',
//     1  ' diagonal'
//        WRITE(IOUT,43) (EIGW0(I),I=1,6)
//        WRITE(IOUT,*)'  MATRIX P0'
    for (i = 0; i < 6; ++i) {
//       WRITE(IOUT,43)(P0(I,J),J=1,6)
      eigex[i] = Math.exp(eigw0[i]);
    }

//       Construction of the SLSGEO-matrix

    for (kw = 0; kw < 36; ++kw) {
      i = mi[kw] - 1;
      j = mj[kw] - 1;
      sum = 0.;
      for (k = 0; k < 6; ++k)
        sum += p0[i][k] * p0[j][k] * eigex[k];
      slsgeo[kw] = sum;
    }
//        WRITE(IOUT,*)' SLSGEO-matrix T-reduced in KL (36)not(6,6)! :'
//        WRITE(16,43)(SLSGEO(KW),KW=1,36)

    return;
// l 43     FORMAT(2X,6E13.5)
// l 46     FORMAT('  Diagonalization is wrong')
  } // geosls

	private double[][] getsshi0() {
		return sshi0[actualReflexIndex];
	}

	private double[][] getsshi0m() {
		return sshi0m[actualReflexIndex];
	}

	public static final int eisrs1(int nm, int n, double[][] ar, double[] wr, double[][] zr, double[][] work) {

//     ALL EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A double
//     SYMMETRIC MATRIX

    double[] new_work = new double[n];
    tred2(nm, n, ar, wr, new_work, zr);
/*    for (int i = 0; i < 6; ++i)
      System.out.println("tred2 wr " + wr[i]);
    for (int i = 0; i < 6; ++i)
      for (int j = 0; j < 6; ++j)
        System.out.println("tred2 zr " + " " + zr[i][j]);*/
    int ierr = tql2(nm, n, wr, new_work, zr);
/*    for (int i = 0; i < 6; ++i)
      System.out.println("tql2 wr " + wr[i]);
    for (int i = 0; i < 6; ++i)
      for (int j = 0; j < 6; ++j)
        System.out.println("tql2 zr " + " " + zr[i][j]);*/
    return ierr;
  } // eisrs1

  public static final void tred2(int nm, int n, double[][] a, double[] d, double[] e, double[][] z) {
    double f, g, h;
    int i, j, k, l;
    double hh;
    int ii, jp1;
    double scale;

// Function Body
    for (i = 0; i < n; ++i) {
      for (j = 0; j < n; ++j) {
        z[i][j] = a[i][j];
//        System.out.println("z "+z[i][j]);
      }
    }
    if (n != 1) {
      for (ii = 1; ii < n; ++ii) {
        i = n - ii;
        l = i - 1;
        h = 0.;
        scale = 0.;
        if (l > 0) {
          for (k = 0; k <= l; ++k)
            scale += Math.abs(z[i][k]);
          if (Math.abs(scale) > 1.0E-10) {
//          System.out.println(i + " " + l + " " + scale +" "+h);
            for (k = 0; k <= l; ++k) {
              z[i][k] = z[i][k] / scale;
              h += z[i][k] * z[i][k];
//          System.out.println(k + " " + z[i][k] + " " + h);
            }
            f = z[i][l];
            g = -MoreMath.r_sign(Math.sqrt(h), f);
//          System.out.println(i + " " + l + " " + h +" "+f+" "+g);
            e[i] = scale * g;
            h -= f * g;
            z[i][l] = f - g;
            f = 0.;
            for (j = 0; j <= l; ++j) {
              z[j][i] = z[i][j] / (scale * h);
              g = 0.;
              for (k = 0; k <= j; ++k)
                g += z[j][k] * z[i][k];
              jp1 = j + 1;
              if (l >= jp1) {
                for (k = jp1; k <= l; ++k)
                  g += z[k][j] * z[i][k];
              } //L220:
              e[j] = g / h;
              f += e[j] * z[i][j];
            }
            hh = f / (h + h);
            for (j = 0; j <= l; ++j) {
              f = z[i][j];
              g = e[j] - hh * f;
              e[j] = g;
              for (k = 0; k <= j; ++k)
                z[j][k] = z[j][k] - f * e[k] - g * z[i][k];
            }
            for (k = 0; k <= l; ++k)
              z[i][k] = scale * z[i][k];
          } else
            e[i] = z[i][l];
        } else
          e[i] = z[i][l];
        d[i] = h;
      }
    }
    d[0] = 0.;
    e[0] = 0.;
    for (i = 0; i < n; ++i) {
      l = i - 1;
      if (d[i] != 0.) {
        for (j = 0; j <= l; ++j) {
          g = 0.;
          for (k = 0; k <= l; ++k)
            g += z[i][k] * z[k][j];
          for (k = 0; k <= l; ++k)
            z[k][j] = z[k][j] - g * z[k][i];
        }
      }
      d[i] = z[i][i];
      z[i][i] = 1.;
      if (l >= 0) {
        for (j = 0; j <= l; ++j) {
          z[i][j] = 0.;
          z[j][i] = 0.;
        }
      }
    }
    return;
  } // tred2

  public static final int tql2(int nm, int n, double[] d, double[] e, double[][] z) {
    int ierr;
    double b, c, f, g, h;
    int i, j, k, l, m;
    double p, r, s;
    int ii, mml;
    double machep = 1.1920928955078125e-7;
    ierr = 0;
    if (n == 1)
      return ierr;
    for (i = 1; i < n; ++i)
      e[i - 1] = e[i];
    f = 0.;
    b = 0.;
    e[n - 1] = 0.;
    for (l = 0; l < n; ++l) {
      j = 0;
      h = machep * (Math.abs(d[l]) + Math.abs(e[l]));
      if (b < h)
        b = h;
      boolean shouldStop = false;
      for (m = l; m < n && !shouldStop; ++m) {
        if (Math.abs(e[m]) <= b) {
          shouldStop = true;
          break;
        }
      }
      if (!shouldStop)
        m--;
      if (m != l) {
        do { // 130:
          if (j == 30)
            return l + 1;
          ++j;
          p = (d[l + 1] - d[l]) / (e[l] * 2.);
          r = Math.sqrt(p * p + 1.);
          h = d[l] - e[l] / (p + MoreMath.r_sign(r, p));
          for (i = l; i < n; ++i)
            d[i] -= h;
          f += h;
          p = d[m];
          c = 1.;
          s = 0.;
          mml = m - l;
          for (ii = 0; ii < mml; ++ii) {
            i = m - ii - 1;
            g = c * e[i];
            h = c * p;
            if (Math.abs(p) < Math.abs(e[i])) {
              c = p / e[i];
              r = Math.sqrt(c * c + 1.);
              e[i + 1] = s * e[i] * r;
              s = 1. / r;
              c *= s;
            } else {
              c = e[i] / p;
              r = Math.sqrt(c * c + 1.);
              e[i + 1] = s * p * r;
              s = c / r;
              c = 1. / r;
            }
            p = c * d[i] - s * g;
            d[i + 1] = h + s * (c * g + s * d[i]);
            for (k = 0; k < n; ++k) {
              h = z[k][i + 1];
              z[k][i + 1] = s * z[k][i] + c * h;
              z[k][i] = c * z[k][i] - s * h;
            }
          }
          e[l] = s * p;
          d[l] = c * p;
        } while (Math.abs(e[l]) > b);
      }
      d[l] += f;
    }
/*        System.out.println("z temp");
    for (int iu = 0; iu < n; iu++)
      for (int ui = 0; ui < n; ui++)
        System.out.println(z[iu][ui]);
        System.out.println("e temp");
    for (int iu = 0; iu < n; iu++)
        System.out.println(e[iu]);
        System.out.println("d temp");
    for (int iu = 0; iu < n; iu++)
        System.out.println(d[iu]);*/
    for (ii = 1; ii < n; ++ii) {
      i = ii - 1;
      k = i;
      p = d[i];
      for (j = ii; j < n; ++j) {
        if (d[j] < p) {
          k = j;
          p = d[j];
        }
      }
//      System.out.println(k+" "+i);
      if (k != i) {
        d[k] = d[i];
        d[i] = p;
        for (j = 0; j < n; ++j) {
//      System.out.println(j+" - "+z[j][i]+" "+z[j][k]);
          p = z[j][i];
          z[j][i] = z[j][k];
          z[j][k] = p;
        }
      }
    }
    return ierr;
  } // tql2

  void subbpgeo(double[][] gy, double[][] sslgeop, double[][] sl) {

    double w2;
    double[][] ssslp = new double[6][6], sslgeo = new double[6][6];

//                                                               S.M  Sept 01
//  ----------------------------
//  rewritten   SUBROUTINE SITER
//                                                               S.M. May  98
//   The Iteration procedure was changed for the exact formula

//               Calculation of the BULKPATHGEO
//               STOP for negative eigenvalues

//     INPUT     SGEOFULL(3,3,3,3) BULKGEO T-unreduced in KA
//               as COMMON/CGEOFULL in SUBROUTINE GEOBULKL
//               then by GEOBULKL transformed into KL, T-reduced
//               -> SSLGEO(6,6) (y)
//     INPUT     GEOPATH   : SSLGEOP(6,6) T-reduced in KL  (hi,y)

//               Thetay,Phiy given by GY(3,3)

//     OUTPUT    BULKPGEO SL(3,3)
//                                   SSSLP = full BULKPATHGEO T-reduced in KL

// Function Body
    w2 = Math.sqrt(2.);
//                                   Bulk-GEO KA -> KL, T-reduction
    geobulkl(gy, sslgeo);
//       *************
//        WRITE(16,*)
//     *  '  S (GEOPATH) T-reduced in KL :'
//        WRITE(16,43) ((SSLGEOP(I,J),J=1,6),I=1,6)

//        WRITE(16,'(///,A)')' SUBMORAV'
    submorav(sslgeo, sslgeop, ssslp);
//       *************
//                             Transfer of SSSLP to unreduced values.
//                             At this only SSLP(33 mn) is of interest
//                             i.e. I=3 J=1,2,3,4,5,6

//                                             Construction of SL(3,3)
    sl[0][0] = ssslp[2][0];
    sl[1][1] = ssslp[2][1];
    sl[2][2] = ssslp[2][2];
    sl[1][2] = ssslp[2][3] / w2;
    sl[2][1] = sl[1][2];
    sl[0][2] = ssslp[2][4] / w2;
    sl[2][0] = sl[0][2];
    sl[0][1] = ssslp[2][5] / w2;
    sl[1][0] = sl[0][1];
    return;
// 43     FORMAT(6E13.5)
  } // subbpgeo

  public static final double[] g2g10(
          double pi, double ca2, double sa2, double cb2, double sb2, double cg2,
          double sg2, double ca1, double sa1, double cb1, double sb1, double p2i) {
// Local variables
    double a1, a2, g1, g2, ca, cb, cg, sa, sb, sg, ga2, al1, al2, fak;
    double[] ODF_angles = new double[3];


//                                                      0.9999-variant

// Calculates the product of two rotations g=g2*g1, g1=(AL1,BET1,0)

//     double*8 PI,CA2,SA2,CB2,SB2,CG2,SG2,CA1,SA1,CB1,SB1,AL,BET,GAM
//     double*8 CB,SB,A1,A2,SA,CA,G1,G2
//     double*8 SG,CG,FAK,P2I,C2B
    if (cb2 >= 1.) {
      ODF_angles[1] = Math.acos(cb1);
      ODF_angles[0] = Math.acos(ca1);
      if (sa1 < 0.) {
        ODF_angles[0] = p2i - ODF_angles[0];
      }
      al2 = Math.acos(ca2);
      if (sa2 < 0.) {
        al2 = p2i - al2;
      }
      ga2 = Math.acos(cg2);
      if (sg2 < 0.) {
        ga2 = p2i - ga2;
      }
      ODF_angles[2] = al2 + ga2;
      if (ODF_angles[2] > p2i) {
        ODF_angles[2] -= p2i;
      }
      return ODF_angles;
    }
    if (cb1 >= 1.) {
      ODF_angles[1] = Math.acos(cb2);
      ODF_angles[2] = Math.acos(cg2);
      if (sg2 < 0.) {
        ODF_angles[2] = p2i - ODF_angles[2];
      }
      al1 = Math.acos(ca1);
      if (sa1 < 0.) {
        al1 = p2i - al1;
      }
      al2 = Math.acos(ca2);
      if (sa2 < 0.) {
        al2 = p2i - al2;
      }
      ODF_angles[0] = al1 + al2;
      if (ODF_angles[0] > p2i) {
        ODF_angles[0] -= p2i;
      }
      return ODF_angles;
    }
    cb = cb1 * cb2 - sb1 * sb2 * ca2;
//     IF(CB**2-1.D0)1,2,2
//                                                          0.9999 !!!!
// Computing 2nd power
    if (cb * cb - .9999f >= 0.) {
      if (cb >= 0.) {
        ODF_angles[1] = 0.;
//     	FAK=1.D0
        fak = 1.;
      } else {
        ODF_angles[1] = pi;
//     FAK=-1.D0
        fak = -1.;
      }
      ODF_angles[2] = 0.;
      a1 = cb1 * (ca2 * cb2 * cg2 - sa2 * sg2) - sb1 * sb2 * cg2;
      a2 = ca2 * sg2 + sa2 * cb2 * cg2;
      ca = fak * (ca1 * a1 - sa1 * a2);
      sa = fak * (sa1 * a1 + ca1 * a2);
//     IF(CA-1.D0)15,14,14
      if (ca - 1. >= 0.) {
        ODF_angles[0] = 0.;
        return ODF_angles;
      }
      if (ca <= -1.) {
        ODF_angles[0] = pi;
      }
//     IF(CA.GT.-1.D0) AL=DACOS(CA)
      if (ca > -1.) {
        ODF_angles[0] = Math.acos(ca);
      }
      if (sa < 0.) {
        ODF_angles[0] = p2i - ODF_angles[0];
      }
      return ODF_angles;
    } else {
      ODF_angles[1] = Math.acos(cb);
//     SB=DSIN(BET)
      sb = Math.sin(ODF_angles[1]);
      a1 = cb1 * ca2 * sb2 + sb1 * cb2;
      a2 = sa2 * sb2;
      sa = (sa1 * a1 + ca1 * a2) / sb;
      ca = (ca1 * a1 - sa1 * a2) / sb;
//     IF(CA-1.D0)5,4,4
      if (ca - 1. >= 0.) {
        ODF_angles[0] = 0.;
      } else {
        if (ca <= -1.) {
          ODF_angles[0] = pi;
        }
//     IF(CA.GT.-1.D0) AL=DACOS(CA)
        if (ca > -1.) {
          ODF_angles[0] = Math.acos(ca);
        }
        if (sa < 0.) {
          ODF_angles[0] = p2i - ODF_angles[0];
        }
      }
      g1 = sb1 * ca2 * cb2 + cb1 * sb2;
      g2 = sb1 * sa2;
      sg = (g1 * sg2 + g2 * cg2) / sb;
      cg = (g1 * cg2 - g2 * sg2) / sb;
//     IF(CG-1.D0)9,8,8
      if (cg - 1. >= 0.) {
        ODF_angles[2] = 0.;
        return ODF_angles;
      }
      if (cg <= -1.) {
        ODF_angles[2] = pi;
      } else {
        ODF_angles[2] = Math.acos(cg);
      }
      if (sg < 0.) {
        ODF_angles[2] = p2i - ODF_angles[2];
      }
    }
    return ODF_angles;
  } // g2g10

  void iord(int irang, int icos, double geww, double[] c) {
    switch (irang) {
      case 1:
        if (icos == 1) c[1] += geww;
        if (icos == 0) c[2] += geww;
        break;
      case 2:
        if (icos == 2) c[3] += geww;
        if (icos == 1) c[4] += geww;
        if (icos == 0) c[5] += geww;
        break;
      case 3:
        if (icos == 3) c[6] += geww;
        if (icos == 2) c[7] += geww;
        if (icos == 1) c[8] += geww;
        if (icos == 0) c[9] += geww;
        break;
      case 4:
        if (icos == 4) c[10] += geww;
        if (icos == 3) c[11] += geww;
        if (icos == 2) c[12] += geww;
        if (icos == 1) c[13] += geww;
        if (icos == 0) c[14] += geww;
        break;
      default:
        {
          System.out.println("    STUSS, iord");
        }
    }
    return;
  }

  void subsgeo(int igeo, int irandom, int ical, double[][][] fio) {

// Local variables
    double e;
    int i, j, k;
    double[][] p0 = new double[6][6];
    double w2;
    int is, js, kw;
    double[][] ps = new double[6][6], e0s = new double[6][6];
    double fak;
    double[][] esa = new double[6][6];
    int ita, itb, itg;
    double sum, hphon;
    int kws, iad1, iad2, iad3, iad4;
    double[] help = new double[73];
    double[][][] fusc = new double[9][9][37];
    int ierr;
    double[][] work = new double[6][6];
    double[] eigw0 = new double[6];
    int nbeta, ilauf;
    double[] eigwl = new double[6];
    double[] fsimp = new double[73];
    double[][][] trigo = new double[9][9][9];
    double[] cwwww = new double[551124];
    double[][] hloge0 = new double[6][6];
    int iwwww;
    double cmerk5;
    double[] fsimp1 = new double[37];
    double[][] hlogea = new double[6][6];
    int ngamma, nalpha;
    double[] eiglog = new double[6];
    int igreek, jgreek;
    double[][] egeoms = new double[6][6];

//                                                   Variant Sept 2001
// For a given f(g) and

//                   ICAL= 1 WWa calculation by TRIGOS and Lib-data
//               for ICAL><1 WWa is taken from COMMON/CWARIM/WWARIM(36,36)

// IGEO= 1 Calculation of bulk Egeom
// IGEO><1 Calculation of bulk Ea   for the INPUT E0 T-unreduced from
//                                  COMMON /CE0/E0(6,6)

// OUTPUT                           COMMON /CEGEOM/EGEOM(6,6),EA(6,6)
//                                                           (May 98)
// Only Egeom or Ea is calculated, not both together !

// --------------------------------------------------------------------
//    Origin ->  PROGRAM GEO4ELA.FOR                        S.M. Nov.94

//     Here shortened (E arithmetic mean  IGEO.NE.1 or
//                     E geometric  mean  IGEO = 1 calculated only,
//                     no HILLs and SUPERHILL
//     variant with rewritten structure.
//     Repeated INPUTs of several E0 can use the the same WWARIM, only once
//              *****
//     calculated for a given ODF.

// ----------------------------------------------------------------------
//    The subroutine determines the geometric mean basing on the arithmetic
//    mean algorithm for the elastic tensor E (i.e. twice symmetric tensor
//    of rank 4)

//    Used version of the ODF (IRANDOM.NE.1) :  WIMV Standard FORMAT
//                             ////////////
//    (i.e. 5 degree grid and (ALPHA,BETA,GAMMA)-version of EULER angles)
//    GAMMA SECTIONS

//    For IRANDOM=1 random ODF f(g)=1 is assumed - ODF-INPUT not necessary
//        /////////                                ///////////////////////

//    For IRED=1 a reduced ODF of type f~(g) not necessary >0 can be used
//    but IRED=0 is here hard wired.

//    OUTPUT : EGEOM(6,6) or EA(6,6)
//    ******
//             (6X6 symmetric T-unreduced matrix).
//    cf.                           COMMON /CE0/E0(6,6)
//                                  COMMON /CEGEOM/EGEOM(6,6),EA(6,6)

//    Using these data the transformation to a (9x9) schema is trivial.
//    (I,J=7,8,9 correspond to 4,5,6). The (6*6) data can also be used
//    to create Voigt or WOOSTER modified elastic matrix elements.


//        COMMON/CEGEOM/EGEOM(6,6),E0(6,6),EA(6,6)
// *******************************************
    hphon = phon;
    w2 = Math.sqrt(2.);
    if (ical == 1) {
      ++ical;
//                                     Starting values of WWARIM(KW,KWS)
      for (kw = 0; kw < 21; ++kw) {
        for (kws = 0; kws < 36; ++kws) {
          wwarim[kw][kws] = 0.;
          wwarir[kw][kws] = 0.;
        }
      }
      if (irandom == 1)
        hphon = 1.0;
      if (hphon != 0.)
        warir();
      if (hphon < 1.) {
// **********************************************************************
//                 WWARIM-Berechnung ?ber TRIGOintegrale
// TRIGS sind bereits bestimmt
// TRIGOintegrale sind ODF-weighted bulk-moments
// **********************************************************************

        for (nbeta = 0; nbeta < 37; ++nbeta) {// BETA loop !!
          for (ita = 0; ita < 9; ++ita) {// ITA loop
            for (ngamma = 0; ngamma < 73; ++ngamma) {// GAMMA loop
              for (nalpha = 0; nalpha < 73; ++nalpha) {// ALPHA loop
                fsimp[nalpha] = fio[nalpha][nbeta][ngamma] * trigs[ita][nalpha];
              }// END ALPHA loop
              help[ngamma] = dhysto(fsimp, c73);
            }// END GAMMA loop

            for (itg = 0; itg < 9; ++itg) {// ITG loop
              for (ngamma = 0; ngamma < 73; ++ngamma) {// GAMMA loop
                fsimp[ngamma] = help[ngamma] * trigs[itg][ngamma];
              }// END GAMMA loop
              fusc[ita][itg][nbeta] = dhysto(fsimp, c73);
            }// END ITG loop   +
          }// END ITA loop  **
        }// END BETA loop !!
// ---------------------------------------------------------------------
//                                       START of the TRIGO  calculation
// ---------------------------------------------------------------------
        for (itb = 0; itb < 9; ++itb) { //ITB loop
          for (ita = 0; ita < 9; ++ita) {// ITA loop
            for (itg = 0; itg < 9; ++itg) {// ITG loop
              for (nbeta = 0; nbeta < 37; ++nbeta) {// BETA loop
//                                                 sin(Beta)  weight
                fsimp1[nbeta] = trigs[itb][nbeta] * fusc[ita][itg][nbeta];
              }// END BETA loop
              trigo[ita][itb][itg] = dhysto(fsimp1, c37);
            }// END ITG loop
          }// END ITA loop
        }// END ITB loop
//                                  TRIGO complete

//          Determination of the  WWARIM(KW,KWS) using the CWW and TRIGOs
//          *************************************************************

        for (iwwww = 0; iwwww < 551124; ++iwwww)
          cwwww[iwwww] = 0.;
        for (ilauf = 0; ilauf < 9885; ++ilauf)
          cwwww[Sla33Constants.iadd[ilauf] - 1] = Sla33Constants.cwww[ilauf];
        for (kw = 0; kw < 21; ++kw) {
          iad1 = kw * 26244;
          for (kws = 0; kws < 36; ++kws) {
            iad2 = iad1 + kws * 729;
            sum = 0.;
            for (ita = 0; ita < 9; ++ita) {
              iad3 = iad2 + ita * 81;
              for (itb = 0; itb < 9; ++itb) {
                iad4 = iad3 + itb * 9;
                for (itg = 0; itg < 9; ++itg) {
                  cmerk5 = cwwww[iad4 + itg];
                  if (cmerk5 != 0.)
                    sum += cmerk5 * trigo[ita][itb][itg];
                }
              }
            }
            wwarim[kw][kws] = sum;
          }
//       IF(KW.EQ.7)WRITE(16,'(A,I4,/)')
//     *   ' WWARIMs (f-phon)-part for KW = : ',KW
//       IF(KW.EQ.7)WRITE(16,43)(WWARIM(KW,KWS),KWS=1,36)
        }
      } else
        hphon = 1.0;
// ------------------- WWARIM - as WWARIM(ODF-PHON) + WWARIR*PHON -----

      for (kw = 0; kw < 21; ++kw)
        for (kws = 0; kws < 36; ++kws)
          wwarim[kw][kws] += wwarir[kw][kws] * hphon;
    }
// ----------------------------------------END OF WWARIM calculation---

//    Greek indexes indicate that we are working with reduced matrixes now
//    Transfer to the reduced (6*6) E0S-matrix : Eo' = E0S = (T**-1)*E0*T

//        WRITE(16,*)' Below the print of some matrices is suppressed,'
//        WRITE(16,*)' but can simply be reactivated in the program'
//        WRITE(16,*)' containing the corresponding print commands'
//                                                   E0 -> E0S
    for (igreek = 0; igreek < 6; ++igreek) {
      for (jgreek = igreek; jgreek < 6; ++jgreek) {
        fak = 1.;
        if (igreek > 2)
          fak *= w2;
        if (jgreek > 2)
          fak *= w2;
        e = e0[igreek][jgreek] * fak;
        e0s[igreek][jgreek] = e;
        e0s[jgreek][igreek] = e;
        e0[jgreek][igreek] = e0[igreek][jgreek];
      }
    }
    if (igeo == 1) {

//                                        Diagonalization of E0S
      ierr = eisrs1(c6, c6, e0s, eigw0, p0, work);
      for (i = 1; i <= 6; ++i)
        eiglog[i - 1] = Math.log(eigw0[i - 1]);

//                   Construction of the HLOGE0S-matrix (T reduced)

      for (i = 0; i < 6; ++i) {
        for (j = i; j < 6; ++j) {
          sum = 0.;
          for (k = 0; k < 6; ++k)
            sum += p0[i][k] * p0[j][k] * eiglog[k];
          hloge0[i][j] = sum;
          hloge0[j][i] = sum;
        }
      }

//                   Construction of the HLOGEAS-matrix (T reduced)

      for (kw = 0; kw < 21; ++kw) {
        i = mi[kw] - 1;
        j = mj[kw] - 1;
        sum = 0.;
        for (kws = 0; kws < 36; ++kws) {
          is = mi[kws] - 1;
          js = mj[kws] - 1;
          sum += wwarim[kw][kws] * hloge0[is][js];
        }
        hlogea[i][j] = sum;
        hlogea[j][i] = sum;
      }

//                                       Diagonalization of HLOGEAS
// 	CALL DIAG3(HLOGEA,EIGWL,PS)
      ierr = eisrs1(c6, c6, hlogea, eigwl, ps, work);

//         Final step : geometric mean of the T reduced E-matrix

      for (igreek = 0; igreek < 6; ++igreek) {
        for (jgreek = igreek; jgreek < 6; ++jgreek) {
          sum = 0.;
          for (k = 0; k < 6; ++k)
            sum += ps[igreek][k] * ps[jgreek][k] * Math.exp(eigwl[k]);
          egeoms[igreek][jgreek] = sum;
          egeoms[jgreek][igreek] = sum;
        }
      }

//                          Transfer to the unreduced Tensor-matrix

      for (i = 0; i < 6; ++i) {
        for (j = 0; j < 6; ++j) {
          fak = 1.;
          if (i > 2)
            fak = w2;
          if (j > 2)
            fak *= w2;
          egeom[i][j] = egeoms[i][j] / fak;
        }
      }

//                                         Egeobulk(6,6) -> E(3,3,3,3)

      sgeoinp(egeom);
      return;
    } // ---------------------------------------- END GEOMETRIC MEAN -------

//                          Simple arithmetic mean of the E0S-matrix

    for (kw = 0; kw < 21; ++kw) {
//     DO 66 KW=1,36
      i = mi[kw] - 1;
      j = mj[kw] - 1;
      sum = 0.;
      for (kws = 0; kws < 36; ++kws) {
        is = mi[kws] - 1;
        js = mj[kws] - 1;
        sum += wwarim[kw][kws] * e0s[is][js];
      }
      esa[i][j] = sum;
      esa[j][i] = sum;
    }
/*    for (int ihk1 = 0; ihk1 < 6; ihk1++)
      for (int jhk1 = 0; jhk1 < 6; jhk1++)
    System.out.println("esa " + esa[ihk1][jhk1]);*/

// 	Transfer to the unreduced Tensor-matrix EA

// l        WRITE(16,*)' Arithmetic mean : T unreduced Tensor-matrix '
    for (i = 0; i < 6; ++i) {
      for (j = 0; j < 6; ++j) {
        fak = 1.;
        if (i > 2)
          fak = w2;
        if (j > 2)
          fak *= w2;
        ea[i][j] = esa[i][j] / fak;
      }
// l	WRITE(16,43)(EA(I,J),J=1,6)
    }
    if (log_output) {
      try {
        printLine(out, "Arithmetic mean : T unreduced Tensor-matrix ");
        for (i = 0; i < 6; ++i) {
          for (j = 0; j < 6; ++j)
            printString(out, "    " + ea[i][j]);
          newLine(out);
        }
      } catch (IOException io) {
        io.printStackTrace();
      }
    }

// ---------------------------------------- END ARITHMETIC MEAN -------
    return;

  } // subsgeo

  double dhysto(double[] fh, int n) {
//  N (<= 73) number of cells - VG/8PI**2 weighted
    double sum = 0.;
    for (int nfis = 0; nfis < n; nfis++)
      sum += fh[nfis];
    return sum;
  } // dhysto

  double[][][] fttcon(int ired, double[][][] fio) {

// Local variables
    double a, b, fa;
    int ia, ib, ig;
    double pi, pi5, hva, pi25, hvg, sum, hvag;
    double fmin;
    double picon, hnormtt;
    double[] hvbtt = new double[37];

//       VARIANT WITH VG/(8PI*PI)-weighting and Phonreduction

// 	FTT(ALPHA,BETA,GAMMA)=
//       (FIO(ALPHA,BETA,GAMMA)-PHON)*VG(ALPHA,BETA,GAMMA)/(8PI*PI)
// 	COMPLETE G-SPACE CONSIDERED, 5 degree STEPS.
//     FOR IRED.NE.1 :
//     BECAUSE FIO WAS NORMALIZED EARLIER FOR SPEED FIO VALUES LOWER 0.0005
//     CAN BE NEGLECTED DUE TO THEIR SMALL CONTRIBUTION TO THE MEAN VALUE.

//        COMMON/CVBTT/HVBTT(37)

    double[][][] fio_corrected = new double[73][37][73];
    pi = Math.acos(-1.);
    picon = pi * 8. * pi;
    pi5 = pi / 36.;
    pi25 = pi / 72.;
    fmin = 6.66e6;
    for (ia = 0; ia < 73; ++ia) {
      for (ib = 0; ib < 37; ++ib) {
        for (ig = 0; ig < 73; ++ig) {
          if (ired != 1 && fio[ia][ib][ig] < 5.0E-4) {
            fio[ia][ib][ig] = 0.;
          }
          if (fio[ia][ib][ig] < fmin) {
            fmin = fio[ia][ib][ig];
          }
        }
      }
    }
// l  	WRITE(16,41)FMIN
    phon = 0.;

    if (fmin > 0. && fmin < 1.0) {
      phon = fmin;
    }

    if (!siegfried_strict)
      phon = 0.0;

//    if (debug_output)
//      System.out.println(" Phon : " + phon);

// l  	WRITE(16,42)PHON
//        DO 33 IA=1,73
//        DO 34 IB=1,37
//        DO 35 IG=1,73
//                                                       here FIO=FIO-Phon
//        FIO(IA,IB,IG)=FIO(IA,IB,IG)-PHON
//  35    CONTINUE
//  34    CONTINUE
//  33    CONTINUE

    for (ib = 0; ib < 37; ++ib) {
      if (ib <= 0) {
        a = 0.;
        b = pi25;
      } else {
        a = pi25 + pi5 * (ib - 1.);
        b = a + pi5;
        if (ib == 36) {
          b = a + pi25;
        }
      }
      hvbtt[ib] = Math.cos(a) - Math.cos(b);
    }
    sum = 0.;
    for (ia = 0; ia < 73; ++ia) {
      if (ia <= 0 || ia >= 72) {
        hva = pi25 / picon;
      } else {
        hva = pi5 / picon;
      }
      for (ig = 0; ig < 73; ++ig) {
        if (ig <= 0 || ig >= 72) {
          hvg = pi25;
        } else {
          hvg = pi5;
        }
        hvag = hva * hvg;
        for (ib = 0; ib < 37; ++ib) {
//                                                           here FIO-Phon
//        FH=FIO(IA,IB,IG)
//        FA=FH*HVAG*HVBTT(IB)
          fa = (fio[ia][ib][ig] - phon) * hvag * hvbtt[ib];
          sum += fa;
//                                            FTT phonreduced and weighted
//                                            OUTPUT value !!!
//          System.out.println("fio "+fio[ia][ib][ig]+" "+phon+" "+hvag+" "+hvbtt[ib]);
          fio_corrected[ia][ib][ig] = fa;
        }
      }
    }
    sum += phon;

    hnormtt = 1. / sum;
    if (debug_output) {
      try {
        printLine(out, "HNORMTT = " + hnormtt);
      } catch (IOException io) {
        io.printStackTrace();
      }
    }
// l	WRITE(16,40)HNORMTT
//                            FIO was not touched up to the gap "IF" activity
//                                         ok, we need it later for MomentPFs
    return fio_corrected;
// l  40	FORMAT(1X,'HNORMTT = ',D20.10)
// l  41	FORMAT(1X,'  FMIN = ',F8.4)
// l  42	FORMAT(1X,'  PHON = ',F8.4)
  } // fttcon

  double finterpol(double alpha, double beta, double gamma, double[][][] fio) {

    // Local variables
    double x, y, z, a0, b0, g0, f1, f2, f3, f4, f5, f6, f7;
    int n1, n2, n3, n4, n5, n6, n7, n8;
    double f8, f12, f34, f56, f78;
    int ia0, ia1, ia2, ia3, ia4, ia5, ia6, ia7, ia8, ib0, ib1, ib3,
            ib2, ib5, ib6, ib4, ib7, ib8, ig0, ig1, ig5, ig2, ig3, ig4, ig6,
            ig7, ig8;
    double f1234, f5678;
    int nye;
    double r1;


//       Linear interpolation within the 5-degree WIMV-cell structure
//       of the whole G-space using its "rectangular" character and the
//       WIMV-address prescription, determining in a unique way for any
//       g={alf,bet,gam} a cell (with its centre at {alf0,bet0,gam0}) to
//       which this orientation belongs.

//       Depending on (alf - al0), (bet-bet0) and (gam-gam0), with g0 not
//       a G-space boundary cell, 7 neighbour cells can be defined in an
//       unique way so, that their cell centres together with the g0-cell
//       form a cube, with its corners at these cell centres, for that all 8
//       ODF-values are known. This also works when g lies on the surface of
//       a cell.

//  If g hits a cell centre the 'nearest neighbour' cell is ambiguous.
//  But it can always formally be defined by a prescription. So in this case
//  also an 8-cell cube can be considered. However, each reasonable scheme
//  of interpolation will of course return the exact f(g0)-value.

//  The scheme becomes transparent, starting with a one-dimensional space,
//  then going over to the two-dimensional case, a.s.o.

//  With g within or at the surface of this cube, a three-dimensional
//  linear interpolation is then performed. It preserves the positivity
//  of the resulting f(g) for the 8 positive basic values !

//  This scheme also works for the boundary cells of the G-space, because
//  in this case the 'neighbour' cells will be taken only from the inner part
//  of the 'finite, rectangular G-space.

//  If g exactly hits the surface of the G-space, with the 'centres' of the
//  smaller WIMV boundary cells on the surface itself, the situation
//  becomes even simpler. It effectively reduces to the consideration of a
//  "quadrat" [g on a common surface point] -> (4 twofold corners), of a
//  "line" [g on an edge] -> (2 fourfold corners), or for a G-space
//  'corner hit' of a "point" -> (1 eightfold corner), without any
//  interpolation.

    nye = 73 * 37;
    ia0 = (int) ((alpha - 2.5) / 5. + 2.00001);
    a0 = (double) ifiw[ia0 - 1];
    if ((r1 = alpha - a0) < 0.) {
      ia2 = ia0;
      ia1 = ia2 - 1;
    } else if (r1 == 0) {
      ia1 = ia0;
      ia2 = ia1;
    } else {
      ia1 = ia0;
      ia2 = ia1 + 1;
    }
    ia3 = ia1;
    ia5 = ia1;
    ia7 = ia1;
    ia4 = ia2;
    ia6 = ia2;
    ia8 = ia2;
    ib0 = (int) ((beta - 2.5) / 5. + 2.00001);
    b0 = (double) ifiw[ib0 - 1];
    if ((r1 = beta - b0) < 0.) {
      ib3 = ib0;
      ib1 = ib3 - 1;
    } else if (r1 == 0) {
      ib1 = ib0;
      ib3 = ib1;
    } else {
      ib1 = ib0;
      ib3 = ib1 + 1;
    }
    ib2 = ib1;
    ib5 = ib1;
    ib6 = ib1;
    ib4 = ib3;
    ib7 = ib3;
    ib8 = ib3;
    ig0 = (int) ((gamma - 2.5) / 5. + 2.00001);
    g0 = (double) ifiw[ig0 - 1];
    if ((r1 = gamma - g0) < 0.) {
      ig5 = ig0;
      ig1 = ig5 - 1;
    } else if (r1 == 0) {
      ig1 = ig0;
      ig5 = ig1;
    } else {
      ig1 = ig0;
      ig5 = ig1 + 1;
    }
    ig2 = ig1;
    ig3 = ig1;
    ig4 = ig1;
    ig6 = ig5;
    ig7 = ig5;
    ig8 = ig5;
    x = (alpha - ifiw[ia1 - 1]) / 5.;
    y = (beta - ifiw[ib1 - 1]) / 5.;
    z = (gamma - ifiw[ig1 - 1]) / 5.;
//    n1 = nye * (ig1 - 1) + (ib1 - 1) * 73 + ia1;
//    n2 = nye * (ig2 - 1) + (ib2 - 1) * 73 + ia2;
//    n3 = nye * (ig3 - 1) + (ib3 - 1) * 73 + ia3;
//    n4 = nye * (ig4 - 1) + (ib4 - 1) * 73 + ia4;
//    n5 = nye * (ig5 - 1) + (ib5 - 1) * 73 + ia5;
//    n6 = nye * (ig6 - 1) + (ib6 - 1) * 73 + ia6;
//    n7 = nye * (ig7 - 1) + (ib7 - 1) * 73 + ia7;
//    n8 = nye * (ig8 - 1) + (ib8 - 1) * 73 + ia8;
    f1 = fio[ia1 - 1][ib1 - 1][ig1 - 1];
    f2 = fio[ia2 - 1][ib2 - 1][ig2 - 1];
    f3 = fio[ia3 - 1][ib3 - 1][ig3 - 1];
    f4 = fio[ia4 - 1][ib4 - 1][ig4 - 1];
    f5 = fio[ia5 - 1][ib5 - 1][ig5 - 1];
    f6 = fio[ia6 - 1][ib6 - 1][ig6 - 1];
    f7 = fio[ia7 - 1][ib7 - 1][ig7 - 1];
    f8 = fio[ia8 - 1][ib8 - 1][ig8 - 1];
    f12 = f1 + (f2 - f1) * x;
    f34 = f3 + (f4 - f3) * x;
    f56 = f5 + (f6 - f5) * x;
    f78 = f7 + (f8 - f7) * x;
    f1234 = f12 + (f34 - f12) * y;
    f5678 = f56 + (f78 - f56) * y;
//    System.out.println("finterpol "+ f1234+" " + f5678+" " + f1234+" "+ z);
    return f1234 + (f5678 - f1234) * z;
//    return fio[ia0-1][ib0-1][ig0-1];
  } // finterpol

  int warir() {
// Local variables
    int i, j;
    double w2;
    int is, js, kw;
    double res;
    int kws;

// 	Analytical formulae for WWARIM  random case (ODF=1)
    w2 = Math.sqrt(2.);
    for (kw = 0; kw < 36; ++kw) {
      i = mi[kw] - 1;
      j = mj[kw] - 1;
      for (kws = 0; kws < 36; ++kws) {
        is = mi[kws] - 1;
        js = mj[kws] - 1;
        res = del(i, is, j, js, w2);
        wwarir[kw][kws] = res;
      }
    }
    return 0;
  } // warir_

//   *********************************************

  double del(int i, int is, int j, int js, double w2) {

    int[][] mi1 = {{1, 1, 1, 1, 1, 1}, {2, 2, 2, 2, 2, 2},
                   {3, 3, 3, 3, 3, 3}, {2, 2, 2, 2, 2, 2}, {1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1}};
    int[][] mi2 = {{1, 2, 3, 2, 1, 1}, {1, 2, 3, 2, 1, 1},
                   {1, 2, 3, 2, 1, 1}, {1, 2, 3, 2, 3, 1}, {1, 2, 3, 3, 1, 2}, {1, 2, 3, 2, 3, 1}};
    int[][] mi3 = {{1, 1, 1, 1, 1, 1}, {2, 2, 2, 2, 2, 2},
                   {3, 3, 3, 3, 3, 3}, {3, 3, 3, 3, 3, 3}, {3, 3, 3, 3, 3, 3}, {2, 2, 2, 2, 2, 2}};
    int[][] mi4 = {{1, 2, 3, 3, 3, 2}, {1, 2, 3, 3, 3, 2},
                   {1, 2, 3, 3, 3, 2}, {1, 2, 3, 3, 1, 2}, {1, 2, 3, 2, 3, 1}, {1, 2, 3, 3, 1, 2}};
    int[][] mi5 = {{0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0},
                   {0, 0, 0, 0, 0, 0}, {0, 0, 0, 2, 2, 2}, {0, 0, 0, 1, 1, 1}, {0, 0, 0, 1, 1, 1}};
    int[][] mi6 = {{0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0},
                   {0, 0, 0, 0, 0, 0}, {0, 0, 0, 3, 1, 2}, {0, 0, 0, 2, 3, 1}, {0, 0, 0, 3, 1, 2}};
    int[][] mi7 = {{0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0},
                   {0, 0, 0, 0, 0, 0}, {0, 0, 0, 3, 3, 3}, {0, 0, 0, 3, 3, 3}, {0, 0, 0, 2, 2, 2}};
    int[][] mi8 = {{0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0},
                   {0, 0, 0, 0, 0, 0}, {0, 0, 0, 2, 3, 1}, {0, 0, 0, 3, 1, 2}, {0, 0, 0, 2, 3, 1}};

    int ii, ij, ik, il, it, iu, iv;
    double fak;
    int iis;
    double res1;
    double delh;

// 	Arithmetic mean of W'(I,IS,g)*W'(J,JS,g) for random
// 	orientation distribution ODF=1
    fak = 1.;
    if (i <= 2 && is > 2) {
      fak = w2;
    }
    if (is <= 2 && i > 2) {
      fak = w2;
    }
    if (j <= 2 && js > 2) {
      fak *= w2;
    }
    if (js <= 2 && j > 2) {
      fak *= w2;
    }
    ii = mi1[is][i];
    ij = mi3[is][i];
    ik = mi1[js][j];
    il = mi3[js][j];
    iis = mi2[is][i];
    it = mi4[is][i];
    iu = mi2[js][j];
    iv = mi4[js][j];
    res1 = delf(ii, ij, ik, il, iis, it, iu, iv);
    delh = res1;
    if (mi5[is][i] != 0) {
      ii = mi5[is][i];
      ij = mi7[is][i];
      ik = mi1[js][j];
      il = mi3[js][j];
      iis = mi6[is][i];
      it = mi8[is][i];
      iu = mi2[js][j];
      iv = mi4[js][j];
      res1 = delf(ii, ij, ik, il, iis, it, iu, iv);
      delh += res1;
      if (mi5[js][j] != 0) {
        ii = mi5[is][i];
        ij = mi7[is][i];
        ik = mi5[js][j];
        il = mi7[js][j];
        iis = mi6[is][i];
        it = mi8[is][i];
        iu = mi6[js][j];
        iv = mi8[js][j];
        res1 = delf(ii, ij, ik, il, iis, it, iu, iv);
        delh += res1;
      }
    } else if (mi5[js][j] != 0) {
      ii = mi1[is][i];
      ij = mi3[is][i];
      ik = mi5[js][j];
      il = mi7[js][j];
      iis = mi2[is][i];
      it = mi4[is][i];
      iu = mi6[js][j];
      iv = mi8[js][j];
      res1 = delf(ii, ij, ik, il, iis, it, iu, iv);
      delh += res1;
    }
    return delh * fak;
  } // del

  double delf(int ii, int ij, int ik, int il, int is, int it, int iu, int iv) {

    return (delta(ii, ij) * delta(ik, il) * delta(is, it) * delta(iu, iv) +
            delta(ii, ik) * delta(ij, il) * delta(is, iu) * delta(it, iv) +
            delta(ii, il) * delta(ij, ik) * delta(is, iv) * delta(it, iu)) / 6. -
            (delta(ii, ij) * delta(ik, il) + delta(ii, ik) * delta(ij, il) +
            delta(ii, il) * delta(ij, ik)) * (delta(is, it) * delta(iu, iv) +
            delta(is, iu) * delta(it, iv) + delta(is, iv) * delta(it, iu)) / 30.;
  } // delf

  double delta(int i, int j) {
// 	Kronecker Symbol
    double ret_val = 1.;
    if (i != j) {
      ret_val = 0.;
    }
    return ret_val;
  } // delta

  void geobulkl(double[][] gy, double[][] sslgeo) {
// Local variables
    int i, j, i1, i2, j1, j2, l1, l2, l3, l4;
    double w2;
    int kw;
    double fak, sum;

//  INPUT  -> SGEOFULL(3,3,3,3)  T-unreduced in KA
//            Thetay,Phiy by the matrix GY(3,3)

//            Transfer into KL and T-reduction

//  OUTPUT -> SSLGEO(6,6)
//        COMMON /CSSLGEO/SSLGEO(6,6)
//        COMMON /CGY/GY(3,3)

// Function Body
    w2 = Math.sqrt(2.);
    for (kw = 0; kw < 21; ++kw) {
      i = mi[kw] - 1;
      j = mj[kw] - 1;
      i1 = mivoigt[i] - 1;
      i2 = mjvoigt[i] - 1;
      j1 = mivoigt[j] - 1;
      j2 = mjvoigt[j] - 1;
      sum = 0.;
      for (l1 = 0; l1 < 3; ++l1) {
        for (l2 = 0; l2 < 3; ++l2) {
          for (l3 = 0; l3 < 3; ++l3) {
            for (l4 = 0; l4 < 3; ++l4) {
              sum += gy[i1][l1] * gy[i2][l2] * gy[j1][l3]
                      * gy[j2][l4] * sgeofull[l1][l2][l3][l4];
            }
          }
        }
      }
      fak = 1.;
      if (i > 2) {
        fak = w2;
      }
      if (j > 2) {
        fak *= w2;
      }
      sum *= fak;
      sslgeo[i][j] = sum;
      sslgeo[j][i] = sum;
    }
//        WRITE(16,'(A)')' SSLGEO (BULKGEO T-reduced in KL) :'
//        WRITE(16,40)((SSLGEO(I,J),J=1,6),I=1,6)
    return;
// 40     FORMAT(2X,6E13.5)
  } // geobulkl

  void submorav(double[][] a, double[][] b, double[][] s) {
    int i, j, k, l;
    double[][] wub = new double[6][6];
    double sum;
    double[][] binv = new double[6][6];
    double[][] great = new double[6][6], wubinv = new double[6][6], wugreat = new double[6][6];

//     Calculation of S from S = A*(S**-1)*B
//     All matrices symmetrical with positive eigenvalues (otherwise STOP)
//                                                         in WUM
//     Working DIMENSION (6,6)

//       S = WU(B)*WU[INV{WU(B)}*A*INV{WU(B)}]*WU(B)

// Function Body
    wum(b, wub);
//                                                WU(B)**-1'
    sinv(wub, wubinv);
//                                                B**-1'
    sinv(b, binv);
//                                                GREAT
    for (i = 0; i < 6; ++i) {
      for (j = i; j < 6; ++j) {
        sum = 0.;
        for (k = 0; k < 6; ++k) {
          for (l = 0; l < 6; ++l) {
//                                                matrix multiplication
            sum += wubinv[i][k] * a[k][l] * wubinv[l][j];
          }
        }
        great[i][j] = sum;
        great[j][i] = sum;
      }
    }
//                                                WUGREAT
    wum(great, wugreat);
//                                                S
    for (i = 0; i < 6; ++i) {
      for (j = i; j < 6; ++j) {
//                                                matrix multiplication
        sum = 0.;
        for (k = 0; k < 6; ++k) {
          for (l = 0; l < 6; ++l) {
            sum += wub[i][k] * wugreat[k][l] * wub[l][j];
          }
        }
        s[i][j] = sum;
        s[j][i] = sum;
      }
    }
    return;
  } // submorav

  void sgeoinp(double[][] e0) {

    int i, j, i1, i2, j1, j2, ih, jh;

//                                      May 98 kein Lesen eines GEObulk files
//    S.Matthies, november 1994, Dresden

//    Input of elastic compliances (Geometric mean)
//    "BULKGEO T-unreduced in KA"  - here 'E0'

//    Input of the true values (! not renormalized by Voigt or Wooster schemes)
//    E0(i1i2,j1j2) (i,j=1,2,3) of the elastic tensor components
//    (compliances) using the Voigt Index Code :

//    I   1  2  3  4  5  6  enlarged code  7  8  9

//    i1  1  2  3  2  3  1                 3  1  2

//    i2  1  2  3  3  1  2                 2  3  1


//    OUTPUT : COMPLIANCES in SGEOFULL(3,3,3,3) COMMON/CGEOFULL/
//             Working array - T-unreduced, given in KA

    for (i = 0; i < 9; ++i) {
      ih = i;
      if (ih > 5)
        ih -= 3;
      i1 = mivoigt[i] - 1;
      i2 = mjvoigt[i] - 1;
      for (j = 0; j < 9; ++j) {
        jh = j;
        if (jh > 5)
          jh -= 3;
        j1 = mivoigt[j] - 1;
        j2 = mjvoigt[j] - 1;
        sgeofull[i1][i2][j1][j2] = e0[ih][jh];
      }
    }
    return;
  } // sgeoinp

  void wum(double[][] a, double[][] wua) {
// Local variables
    int i, j, k;
    double[][] p = new double[6][6];
    double sum;
    double[] eigw = new double[6];
    double[][] work = new double[6][6];
    double[] eigwin = new double[6];

//        SQRT of A(6,6) -  symmetric matrix
//        STOP IF Eigenvalues .LE.0
//        WRITE(16,*)'    Start-matrix M :'
//        WRITE(16,43) ((A(I,J),J=1,6),I=1,6)

//                                              Diagonalization of A

// Function Body
    int ierr = eisrs1(c6, c6, a, eigw, p, work);

//        WRITE(16,46) IERR
//        WRITE(16,*)'  EIGENVALUES of M and P-matrix  (P-1)*A*P =',
//     1  ' diagonal'
//        WRITE(16,43) (EIGW(I),I=1,6)
//    for (i = 0; i < 6; ++i) {
//      if (eigw[i] >= 0.) {
//        goto L1;
//      }
// l        WRITE(16,'(A)')'  SUBROUTINE WUM : eigenvalue.LT.0 ; STOP'
// l        WRITE(16,43) (EIGW(I),J=1,6)
//L1: ;
//    }
//        WRITE(16,*)'  MATRIX P'
//      DO 3 I=1,6
//        WRITE(16,43)(P(I,J),J=1,6)
//  3   CONTINUE

//                          Construction of the SQRT Tensor-matrix

    for (i = 0; i < 6; ++i) {
      eigwin[i] = Math.sqrt(eigw[i]);
//                                 automatic STOP for negative eigenvalues
    }

    for (i = 0; i < 6; ++i) {
      for (j = i; j < 6; ++j) {
        sum = 0.;
        for (k = 0; k < 6; ++k) {
          sum += p[i][k] * p[j][k] * eigwin[k];
        }
        wua[i][j] = sum;
        wua[j][i] = sum;
      }
    }
//        WRITE(16,*)'   SQRT of the M-matrix :'
//        WRITE(16,43)((WUA(I,J),J=1,6),I=1,6)


    return;
// l  43    FORMAT(6E13.5)
// l  46    FORMAT('  IERR = ',I3)
  } // wum

  void sinv(double[][] a, double[][] ainv) {
    int i, j, k;
    double[][] p = new double[6][6];
    double sum;
    double[] eigw = new double[6];
    int ierr;
    double[][] work = new double[6][6];
    double[] eigwin = new double[6];

//                    Simple invers of a (6,6) symmetric matrix
//                    STOP for eigenvalues = 0.
//       WRITE(16,*)'    A-matrix :'
//       WRITE(16,43) ((A(I,J),J=1,6),I=1,6)

//       Diagonalization of A

// Function Body
    ierr = eisrs1(c6, c6, a, eigw, p, work);

//       WRITE(16,46) IERR
//  46	FORMAT('  IERR = ',I3)
//       WRITE(16,*)'  EIGENVALUES of A and P-matrix  (P-1)*AS*P =',
//     1  ' diagonal'
// 	WRITE(16,43) (EIGW(I),I=1,6)
// 	WRITE(16,*)'  MATRIX P'
//     DO 3 I=1,6
// 	WRITE(16,43)(P(I,J),J=1,6)
//  3	CONTINUE

// 	Construction of the invers Tensor-matrix

    for (i = 0; i < 6; ++i) {
// l        IF(EIGW(I).EQ.0.)WRITE(16,*)
// l     *                 ' Eigenvalue=0; SUBROUTINE SINV-> STOP'
      eigwin[i] = 1. / eigw[i];
//                                      automatic STOP for 1/0
// L60:
    }

    for (i = 0; i < 6; ++i) {
      for (j = i; j < 6; ++j) {
        sum = 0.;
        for (k = 0; k < 6; ++k) {
          sum += p[i][k] * p[j][k] * eigwin[k];
        }
        ainv[i][j] = sum;
        ainv[j][i] = sum;
      }
    }
//       WRITE(16,*)'   invers A-matrix :'
//       WRITE(16,43)((AINV(I,J),J=1,6),I=1,6)

    return;
// l  43    FORMAT(6E13.5)
  } // sinv

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JTSStrainOptionsD(parent, this);
    return adialog;
  }

  class JTSStrainOptionsD extends JOptionsDialog {

    JComboBox ssmodelCB = null;
    JCheckBox textureCB;
    JTextField[] pars = null;
    JTextField[] cijTF = null;
    String[] labels = {
      "Weight (Voigt-Reuss): ",
      " Macrostress11 : ",
      " Macrostress22 : ",
      " Macrostress33 : ",
      " Macrostress23 : ",
      " Macrostress13 : ",
      " Macrostress12 : "};

    public JTSStrainOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      pars = new JTextField[labels.length];

      principalPanel.setLayout(new BorderLayout(6, 6));
      JPanel jPaneln = new JPanel();
      jPaneln.setLayout(new BorderLayout(6, 6));
      principalPanel.add(BorderLayout.NORTH, jPaneln);
      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new GridLayout(0, 2, 3, 3));
      jPaneln.add(BorderLayout.WEST, jPanel8);

      for (int i = 1; i < labels.length; i++) {
        JPanel jpl = new JPanel();
        jpl.setLayout(new FlowLayout(FlowLayout.LEFT));
        jPanel8.add(jpl);
        jpl.add(new JLabel(labels[i]));
        pars[i] = new JTextField(Constants.FLOAT_FIELD);
        pars[i].setText("0");
        jpl.add(pars[i]);
      }

      JPanel jPanel6 = new JPanel();
      jPanel6.setLayout(new GridLayout(0, 1, 3, 3));
      jPaneln.add(BorderLayout.CENTER, jPanel6);
      JPanel jPanel7 = new JPanel();
      jPanel7.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      jPanel6.add(jPanel7);
      jPanel7.add(new JLabel("Stress/strain model: "));
      ssmodelCB = new JComboBox();
      for (int i = 0; i < stressModels.length; i++)
        ssmodelCB.addItem(stressModels[i]);
      ssmodelCB.setToolTipText("Select the micromechanical model for strain computation from macrostresses");
      jPanel7.add(ssmodelCB);

      jPanel7 = new JPanel();
      jPanel7.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      jPanel6.add(jPanel7);
      jPanel7.add(new JLabel(labels[0]));
      pars[0] = new JTextField(Constants.FLOAT_FIELD);
      pars[0].setText("0");
      jPanel7.add(pars[0]);

      jPanel7 = new JPanel();
      jPanel7.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      jPanel6.add(jPanel7);
      textureCB = new JCheckBox("Use texture ODF");
      textureCB.setToolTipText("Check the box to use the ODF for strain computation from stress and stiffness tensors");
      jPanel7.add(textureCB);

      jPanel8 = new JPanel();
      jPanel8.setBorder(new TitledBorder(
              new BevelBorder(BevelBorder.LOWERED), "Stiffness matrix"));
      jPanel8.setLayout(new GridLayout(0, 6, 1, 1));
      principalPanel.add(BorderLayout.CENTER, jPanel8);

      cijTF = new JTextField[21];
      int ij = 0;
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          if (i <= j) {
            cijTF[ij] = new JTextField(Constants.FLOAT_FIELD);
            cijTF[ij].setText("0");
            jPanel8.add(cijTF[ij++]);
          } else
            jPanel8.add(new JLabel("-"));
        }
      }

      setTitle("Moment pole figures options panel");
      initParameters();
      pack();

    }

    public void initParameters() {
//      pars[0].setText(parameterField[0].getValue());
      addComponenttolist(pars[0], parameterField[0]);
      for (int i = 1; i < labels.length; i++) {
//        pars[i].setText(parameterField[i + 21].getValue());
        addComponenttolist(pars[i], parameterField[i + 21]);
      }
      for (int i = 0; i < 21; i++) {
//        System.out.println(i + " " + cijTF[i] + " " + parameterField[i+1]);
//        cijTF[i].setText(parameterField[i + 1].getValue());
        addComponenttolist(cijTF[i], parameterField[i + 1]);
      }
      ssmodelCB.setSelectedItem(getStressModelID());
      textureCB.setSelected(useTexture());
    }

    public void retrieveParameters() {
      parameterField[0].setValue(pars[0].getText());
      for (int i = 1; i < labels.length; i++) {
        parameterField[i + 21].setValue(pars[i].getText());
      }
      for (int i = 0; i < 21; i++) {
        parameterField[i + 1].setValue(cijTF[i].getText());
      }
      setStressModel(ssmodelCB.getSelectedItem().toString());
      useTexture(textureCB.isSelected());
    }

  }

}

