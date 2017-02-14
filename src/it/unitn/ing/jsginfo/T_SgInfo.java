/*
 * @(#)T_SgInfo.java	0.10 created 10/10/1999 Pergine Vals.
 *
 * Translated in 1999 by Luca Lutterotti from
 * Space Group Info (c) 1994-97 Ralf W. Grosse-Kunstleve
 *
 * Permission to use and distribute this software and its documentation
 * for noncommercial use and without fee is hereby granted, provided that
 * the above copyright notice appears in all copies and that both that
 * copyright notice and this permission notice appear in the supporting
 * documentation. It is not allowed to sell this software in any way.
 * This software is not in the public domain.

 * IF YOU COPY AND/OR USE THIS SOFTWARE, YOU AGREE THAT THE SOFTWARE IS
 * FURNISHED ON AN "AS IS" BASIS AND THAT THE AUTHOR IN NO WAY
 * WARRANTS THE SOFTWARE OR ANY OF ITS RESULTS AND IS IN NO WAY LIABLE
 * FOR ANY USE YOU MAKE OF THE SOFTWARE.
 *
 * This software is the research result of the authors and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with the authors.
 *
 * THE AUTHORS MAKE NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHORS SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.jsginfo;

//import it.unitn.ing.rista.util.Misc;

import java.lang.*;
import java.util.*;

/**
 * The T_SgInfo is the base class for space group manipolation.
 *
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:51 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class T_SgInfo {

  public boolean debug = false;

  public int GenOption;
  public int Centric;
  public int InversionOffOrigin;
  public T_LatticeInfo LatticeInfo = null;
  public int StatusLatticeTr;
  public int[] OriginShift = new int[3];
  public int nList;
  public int MaxList;
  public T_RTMx[] ListSeitzMx = null;
  public T_RotMxInfo[] ListRotMxInfo = null;
  public int OrderL;
  public int OrderP;
  public int XtalSystem;
  public int UniqueRefAxis;
  public int UniqueDirCode;
  public int HexMetric;
  public int Chiral;
  public int ExtraInfo;
  public int PointGroup;
  public int nGenerator;
  public int[] Generator_iList = new int[4];
  public String hallSymbol = "";
  public T_TabSgName sTabSgName = null;
  public int[] CCMx_LP;
//    public int                  n_ssVM;
//    public T_ssVM[]             ssVM = new T_ssVM[3];

/* T_Sginfo.GenOption:  0 = full group generation
                        1 = trusted:
                            set Centric/InversionOffOrigin/LatticeInfo only
                       -1 = no group generation

   T_Sginfo.Centric:   0 = acentric
                       1 = inversion in list
                      -1 = inversion removed from list

   T_Sginfo.StatusLatticeTr:   0 = removed from list
                               1 = all translation vectors in list
                              -1 = some translation vectors could be
                                   missing in list
 */

// #include <stdio.h>
// #include <stdlib.h>
// #include <string.h>

// #define SGCLIB_C__
// #include "sginfo.h"

  static final String Err_Ill_SMx_in_List = "Error: Illegal SeitzMx in list";

  public static final int STBF = 12; /* Seitz           Matrix Translation Base Factor */

  public static final int CRBF = 12; /* Change of Basis Matrix Rotation    Base Factor */
  public static final int CTBF = 72; /* Change of Basis Matrix Translation Base Factor */

/* CAUTION: (CTBF / STBF) has to be an INTEGER */

  public static final int EI_Unknown = 0;
  public static final int EI_Enantiomorphic = 1;
  public static final int EI_Obverse = 2;
  public static final int EI_Reverse = 3;

  public static final String EI_Name[] =
          {
            "Unknown",
            "Enantiomorphic",
            "Obverse",
            "Reverse"
          };


  public static final int XS_Unknown = 0;
  public static final int XS_Triclinic = 1;
  public static final int XS_Monoclinic = 2;
  public static final int XS_Orthorhombic = 3;
  public static final int XS_Tetragonal = 4;
  public static final int XS_Trigonal = 5;
  public static final int XS_Hexagonal = 6;
  public static final int XS_Cubic = 7;

  public static final String XS_Name[] =
          {
            "Unknown",
            "Triclinic",
            "Monoclinic",
            "Orthorhombic",
            "Tetragonal",
            "Trigonal",
            "Hexagonal",
            "Cubic"
          };


  public static final int PG_Index(int PG_Code) {
    return PG_Code / (33 * 12);
  }

  public static final int PG_Number(int PG_Code) {
    return (PG_Code / 12) % 33;
  }

  public static final int LG_Number(int PG_Code) {
    return PG_Code % (33 * 12);
  }

  public static final int T(int i) {
    return i * STBF / 12;
  }

  public static final int[] V(int i, int j, int k) {
    int[] tmp = new int[3];
    tmp[0] = T(i);
    tmp[1] = T(j);
    tmp[2] = T(k);
    return tmp;
  }

  public static final int[] V(int i, int j, int k,
                              int i1, int j1, int k1) {
    int[] tmp = new int[6];
    tmp[0] = T(i);
    tmp[1] = T(j);
    tmp[2] = T(k);
    tmp[3] = T(i1);
    tmp[4] = T(j1);
    tmp[5] = T(k1);
    return tmp;
  }

  public static final int[] V(int i, int j, int k,
                              int i1, int j1, int k1,
                              int i2, int j2, int k2) {
    int[] tmp = new int[9];
    tmp[0] = T(i);
    tmp[1] = T(j);
    tmp[2] = T(k);
    tmp[3] = T(i1);
    tmp[4] = T(j1);
    tmp[5] = T(k1);
    tmp[6] = T(i2);
    tmp[7] = T(j2);
    tmp[8] = T(k2);
    return tmp;
  }

  public static final int[] V(int i, int j, int k,
                              int i1, int j1, int k1,
                              int i2, int j2, int k2,
                              int i3, int j3, int k3) {
    int[] tmp = new int[12];
    tmp[0] = T(i);
    tmp[1] = T(j);
    tmp[2] = T(k);
    tmp[3] = T(i1);
    tmp[4] = T(j1);
    tmp[5] = T(k1);
    tmp[6] = T(i2);
    tmp[7] = T(j2);
    tmp[8] = T(k2);
    tmp[9] = T(i3);
    tmp[10] = T(j3);
    tmp[11] = T(k3);
    return tmp;
  }

  public static int LTr_P[] = V(0, 0, 0);
  public static final T_LatticeInfo LI_P = new T_LatticeInfo('P', 1, LTr_P);
  public static int LTr_A[] = V(0, 0, 0, 0, 6, 6);
  public static final T_LatticeInfo LI_A = new T_LatticeInfo('A', 2, LTr_A);
  public static int LTr_B[] = V(0, 0, 0, 6, 0, 6);
  public static final T_LatticeInfo LI_B = new T_LatticeInfo('B', 2, LTr_B);
  public static int LTr_C[] = V(0, 0, 0, 6, 6, 0);
  public static final T_LatticeInfo LI_C = new T_LatticeInfo('C', 2, LTr_C);
  public static int LTr_I[] = V(0, 0, 0, 6, 6, 6);
  public static final T_LatticeInfo LI_I = new T_LatticeInfo('I', 2, LTr_I);
  public static int LTr_R[] = V(0, 0, 0, 8, 4, 4, 4, 8, 8);
  public static final T_LatticeInfo LI_R = new T_LatticeInfo('R', 3, LTr_R);
  public static int LTr_S[] = V(0, 0, 0, 4, 4, 8, 8, 8, 4);
  public static final T_LatticeInfo LI_S = new T_LatticeInfo('S', 3, LTr_S);
  public static int LTr_T[] = V(0, 0, 0, 4, 8, 4, 8, 4, 8);
  public static final T_LatticeInfo LI_T = new T_LatticeInfo('T', 3, LTr_T);
  public static int LTr_F[] = V(0, 0, 0, 0, 6, 6, 6, 0, 6, 6, 6, 0);
  public static final T_LatticeInfo LI_F = new T_LatticeInfo('F', 4, LTr_F);

/*
                        lattice code
                        R    S    T
         unique axis
                  3z   obv   -   rev
                  3y   rev  obv   -
                  3x    -   rev  obv
 */


  public static final int CCMx_PP[] = {1, 0, 0, /* Change of Basis Matrices     */
                                       0, 1, 0, /* (coordinate transformations) */
                                       0, 0, 1
  };
  public static final int CCMx_AP[] = {-1, 0, 0,
                                       0, -1, 1,
                                       0, 1, 1
  };
  public static final int CCMx_BP[] = {-1, 0, 1,
                                       0, -1, 0,
                                       1, 0, 1
  };
  public static final int CCMx_CP[] = {1, 1, 0,
                                       1, -1, 0,
                                       0, 0, -1
  };
  public static final int CCMx_IP[] = {0, 1, 1,
                                       1, 0, 1,
                                       1, 1, 0
  };
  public static final int CCMx_RP_z[] = {1, 0, 1,
                                         -1, 1, 1,
                                         0, -1, 1
  };
  public static final int CCMx_SP_y[] = {1, 1, -1,
                                         -1, 1, 0,
                                         0, 1, 1
  };
  public static final int CCMx_TP_x[] = {1, 0, -1,
                                         1, 1, 0,
                                         1, -1, 1
  };
  public static final int CCMx_TP_z[] = {-1, 0, 1,
                                         1, -1, 1,
                                         0, 1, 1
  };
  public static final int CCMx_RP_y[] = {-1, 1, 1,
                                         1, 1, 0,
                                         0, 1, -1
  };
  public static final int CCMx_SP_x[] = {1, 0, 1,
                                         1, -1, 0,
                                         1, 1, -1
  };
  public static final int CCMx_FI_z[] = {1, 1, 0,
                                         -1, 1, 0,
                                         0, 0, 1
  };
  public static final int CCMx_FI_y[] = {1, 0, -1,
                                         0, 1, 0,
                                         1, 0, 1
  };
  public static final int CCMx_FI_x[] = {1, 0, 0,
                                         0, 1, 1,
                                         0, -1, 1
  };
  public static final int CCMx_FP[] = {-1, 1, 1,
                                       1, -1, 1,
                                       1, 1, -1
  };

  public static final int TXRMxShTrMx[][] =
          {
            V(0, 0, 0, /* [ 0]  1 '.' */
                    0, 0, 0,
                    0, 0, 0),
            V(-6, 0, 0, /* [ 1] -1 '.' */
                    0, -6, 0,
                    0, 0, -6),
            V(-6, 0, 0, /* [ 2]  2 '=' */
                    0, -6, 0,
                    0, 0, 0),
            V(0, 0, 0, /* [ 3] -2 '=' */
                    0, 0, 0,
                    0, 0, -6),
            V(-6, 0, 0, /* [ 4]  2x '=' */
                    -12, 0, 0,
                    0, 0, -6),
            V(-6, 0, 0, /* [ 5] -2x '=' */
                    0, 0, 0,
                    0, 0, 0),
            V(0, -12, 0, /* [ 6]  2y '=' */
                    0, -6, 0,
                    0, 0, -6),
            V(0, 0, 0, /* [ 7] -2y '=' */
                    0, -6, 0,
                    0, 0, 0),
            V(-6, 0, 0, /* [ 8]  2 '"' */
                    6, 0, 0,
                    0, 0, -6),
            V(0, -6, 0, /* [ 9] -2 '"' */
                    0, -6, 0,
                    0, 0, 0),
            V(0, -6, 0, /* [10]  2 ''' */
                    0, -6, 0,
                    0, 0, -6),
            V(-6, 0, 0, /* [11] -2 ''' */
                    6, 0, 0,
                    0, 0, 0),
            V(0, 0, 0, /* [12]  2 '|' */
                    0, -6, 0,
                    0, 0, -6),
            V(0, -12, 0, /* [13] -2 '|' */
                    0, -6, 0,
                    0, 0, 0),
            V(-6, 0, 0, /* [14]  2 '\' */
                    0, 0, 0,
                    0, 0, -6),
            V(-6, 0, 0, /* [15] -2 '\' */
                    -12, 0, 0,
                    0, 0, 0),
            V(-8, 4, 0, /* [16]  3 '=' */
                    -4, -4, 0,
                    0, 0, 0),
            V(0, -12, 0, /* [17] -3 '=' */
                    12, -12, 0,
                    0, 0, -6),
            V(-4, -4, 0, /* [18]  3^-1 '=' */
                    4, -8, 0,
                    0, 0, 0),
            V(-12, 12, 0, /* [19] -3^-1 '=' */
                    -12, 0, 0,
                    0, 0, -6),
            V(-8, 0, -4, /* [20]  3 '*' */
                    4, 0, 8,
                    4, 0, -4),
            V(-6, -6, 6, /* [21] -3 '*' */
                    6, -6, -6,
                    -6, 6, -6),
            V(0, 4, 8, /* [22]  3^-1 '*' */
                    0, -8, -4,
                    0, 4, -4),
            V(-6, 6, -6, /* [23] -3^-1 '*' */
                    -6, -6, 6,
                    6, -6, -6),
            V(-6, 6, 0, /* [24]  4 '=' */
                    -6, -6, 0,
                    0, 0, 0),
            V(-6, -6, 0, /* [25] -4 '=' */
                    6, -6, 0,
                    0, 0, -6),
            V(-6, -6, 0, /* [26]  4^-1 '=' */
                    6, -6, 0,
                    0, 0, 0),
            V(-6, 6, 0, /* [27] -4^-1 '=' */
                    -6, -6, 0,
                    0, 0, -6),
            V(-12, 12, 0, /* [28]  6 '=' */
                    -12, 0, 0,
                    0, 0, 0),
            V(-4, -4, 0, /* [29] -6 '=' */
                    4, -8, 0,
                    0, 0, -6),
            V(0, -12, 0, /* [30]  6^-1 '=' */
                    12, -12, 0,
                    0, 0, 0),
            V(-8, 4, 0, /* [31] -6^-1 '=' */
                    -4, -4, 0,
                    0, 0, -6)
          };

  public static final int[] TXRMxShTrMx(int index) {
    int[] tmp = new int[9];
    for (int i = 0; i < 9; i++)
      tmp[i] = TXRMxShTrMx[index][i];
    return tmp;
  }

  public static int[] get3IntVector(int j1, int j2, int j3) {
    int[] tmp = new int[3];
    tmp[0] = j1;
    tmp[1] = j2;
    tmp[2] = j3;
    return tmp;
  }

  public static int[] get9IntVector(int i1, int i2, int i3,
                                    int i4, int i5, int i6,
                                    int i7, int i8, int i9) {
    int[] tmp = new int[9];
    tmp[0] = i1;
    tmp[1] = i2;
    tmp[2] = i3;
    tmp[3] = i4;
    tmp[4] = i5;
    tmp[5] = i6;
    tmp[6] = i7;
    tmp[7] = i8;
    tmp[8] = i9;
    return tmp;
  }

  public static final T_TabXtalRotMx TabXtalRotMx[] =
          {
            /*  #   EigenVector    DirCode CarHex */

            new T_TabXtalRotMx(/* [ 0] */  1, 0, 0, 0, '.', 3, /* CAUTION:                   */
                    1, 0, 0, /*   Reorganizing this table  */
                    0, 1, 0, /*   affects RMx_????? below. */
                    0, 0, 1, TXRMxShTrMx(0)
            ),
            new T_TabXtalRotMx(/* [ 1] */  2, 0, 0, 1, '=', 3,
                    -1, 0, 0,
                    0, -1, 0,
                    0, 0, 1, TXRMxShTrMx(2)
            ),
            new T_TabXtalRotMx(/* [ 2] */  2, 1, 0, 0, '=', 2, /* hexagonal only */
                    1, -1, 0,
                    0, -1, 0,
                    0, 0, -1, TXRMxShTrMx(4)
            ),
            new T_TabXtalRotMx(/* [ 3] */  2, 0, 1, 0, '=', 2, /* hexagonal only */
                    -1, 0, 0,
                    -1, 1, 0,
                    0, 0, -1, TXRMxShTrMx(6)
            ),
            new T_TabXtalRotMx(/* [ 4] */  2, 1, 1, 0, '"', 3,
                    0, 1, 0,
                    1, 0, 0,
                    0, 0, -1, TXRMxShTrMx(8)
            ),
            new T_TabXtalRotMx(/* [ 5] */  2, 1, -1, 0, '\'', 3,
                    0, -1, 0,
                    -1, 0, 0,
                    0, 0, -1, TXRMxShTrMx(10)
            ),
            new T_TabXtalRotMx(/* [ 6] */  2, 2, 1, 0, '|', 2, /* hexagonal only */
                    1, 0, 0,
                    1, -1, 0,
                    0, 0, -1, TXRMxShTrMx(12)
            ),
            new T_TabXtalRotMx(/* [ 7] */  2, 1, 2, 0, '\\', 2, /* hexagonal only */
                    -1, 1, 0,
                    0, 1, 0,
                    0, 0, -1, TXRMxShTrMx(14)
            ),
            new T_TabXtalRotMx(/* [ 8] */  3, 0, 0, 1, '=', 2, /* hexagonal only */
                    0, -1, 0,
                    1, -1, 0,
                    0, 0, 1, TXRMxShTrMx(16)
            ),
            new T_TabXtalRotMx(/* [ 9] */  3, 1, 1, 1, '*', 1, /* cartesian only */
                    0, 0, 1,
                    1, 0, 0,
                    0, 1, 0, TXRMxShTrMx(20)
            ),
            new T_TabXtalRotMx(/* [10] */  4, 0, 0, 1, '=', 1, /* cartesian only */
                    0, -1, 0,
                    1, 0, 0,
                    0, 0, 1, TXRMxShTrMx(24)
            ),
            new T_TabXtalRotMx(/* [11] */  6, 0, 0, 1, '=', 2, /* hexagonal only */
                    1, -1, 0,
                    1, 0, 0,
                    0, 0, 1, TXRMxShTrMx(28)
            ),
            new T_TabXtalRotMx(0, 0, 0, 0, 0, 0,
                    0, 0, 0,
                    0, 0, 0,
                    0, 0, 0, null
            )
          };

  public static final int[] RMx_1_000 = TabXtalRotMx[0].RMx;
  public static final int[] RMx_2_001 = TabXtalRotMx[1].RMx;
  public static final int[] RMx_2_110 = TabXtalRotMx[4].RMx;
  public static final int[] RMx_3_001 = TabXtalRotMx[8].RMx;
  public static final int[] RMx_3_111 = TabXtalRotMx[9].RMx;

  public static final int RMx_3i111[] =
          {
            0, 1, 0,
            0, 0, 1,
            1, 0, 0
          };
  public static final int[] RMx_4_001 = TabXtalRotMx[10].RMx;
  public static final int RMx_4i001[] =
          {
            0, 1, 0,
            -1, 0, 0,
            0, 0, 1
          };

  public static final int HallTranslations[] =
          {
            'n', T(6), T(6), T(6),
            'a', T(6), T(0), T(0),
            'b', T(0), T(6), T(0),
            'c', T(0), T(0), T(6),
            'd', T(3), T(3), T(3),
            'u', T(3), T(0), T(0),
            'v', T(0), T(3), T(0),
            'w', T(0), T(0), T(3),
            0
          };

  public final int Sg_nLoopInv() {
    return Centric == -1 ? 2 : 1;
  }


  public static final void InitRotMx(int[] RotMx, int diagonal) {
    int private_i_;
    for (private_i_ = 0; private_i_ < 9; private_i_++)
      RotMx[private_i_] = (private_i_ % 4 != 0) ? 0 : diagonal;
  }

  public static final void InitSeitzMx(T_RTMx SeitzMx_, int diagonal) {
    int a[] = new int[12];

    int private_i_;
    for (private_i_ = 0; private_i_ < 12; private_i_++) {
      a[private_i_] = (private_i_ % 4 != 0) ? 0 : diagonal;
    }
    for (private_i_ = 0; private_i_ < 9; private_i_++) {
      SeitzMx_.s.R[private_i_] = a[private_i_];
    }
    for (private_i_ = 0; private_i_ < 3; private_i_++) {
      SeitzMx_.s.T[private_i_] = a[private_i_ + 9];
    }
  }


  public static final int SpecialSMx_Identity = 0x01;
  public static final int SpecialSMx_Inversion = 0x02;
  public static final int SpecialSMx_Transl0 = 0x04;

// #define MaxHarkerInfo  34

  static String SgError = null;

  public static void SetSgError(String msg) {
    if (SgError == null)
      System.err.println(SgError = msg);
  }

  public static int iModPositive(int ix, int iy) {
    if (iy > 0) {
      ix %= iy;
      if (ix < 0) ix += iy;
    }

    return ix;
  }


  public static void SwapRTMx(T_RTMx Mx_a, T_RTMx Mx_b) {
    int i;
    T_RTMx BufMx = new T_RTMx();

    BufMx.copy(Mx_a);
    Mx_a.copy(Mx_b);
    Mx_b.copy(BufMx);
  }


  public static void CopyRotMxInfo(T_RotMxInfo target, T_RotMxInfo source) {
    for (int i = 0; i < 3; i++)
      target.EigenVector[i] = source.EigenVector[i];
    target.Order = source.Order;
    target.Inverse = source.Inverse;
    target.setRefAxis(source.getRefAxis());
    target.setDirCode(source.getDirCode());
  }


  public static void SwapRotMxInfo(T_RotMxInfo RMx_a, T_RotMxInfo RMx_b) {
    T_RotMxInfo Buffer = new T_RotMxInfo();

    CopyRotMxInfo(Buffer, RMx_a);
    CopyRotMxInfo(RMx_a, RMx_b);
    CopyRotMxInfo(RMx_b, Buffer);
    Buffer = null;
  }


  public static int traceRotMx(int[] RotMx) {
    return RotMx[0] + RotMx[4] + RotMx[8];
  }


  public static int deterRotMx(int[] RotMx) {
    int det;

    det = RotMx[0] * (RotMx[4] * RotMx[8] - RotMx[5] * RotMx[7]);
    det -= RotMx[1] * (RotMx[3] * RotMx[8] - RotMx[5] * RotMx[6]);
    det += RotMx[2] * (RotMx[3] * RotMx[7] - RotMx[4] * RotMx[6]);

    return det;
  }


  public static void RotMx_t_Vector(int[] R_t_V, int[] RotMx, int[] Vector, int FacTr) {
    int i = 0;
    int ir = 0;
    int iv = 0;

    if (FacTr > 0) {
      R_t_V[i] = RotMx[ir++] * Vector[iv++];
      R_t_V[i] += RotMx[ir++] * Vector[iv++];
      R_t_V[i] += RotMx[ir++] * Vector[iv];
      R_t_V[i] %= FacTr;
      if (R_t_V[i] < 0) R_t_V[i] += FacTr;
      i++;
      iv = 0;
      R_t_V[i] = RotMx[ir++] * Vector[iv++];
      R_t_V[i] += RotMx[ir++] * Vector[iv++];
      R_t_V[i] += RotMx[ir++] * Vector[iv];
      R_t_V[i] %= FacTr;
      if (R_t_V[i] < 0) R_t_V[i] += FacTr;
      i++;
      iv = 0;
      R_t_V[i] = RotMx[ir++] * Vector[iv++];
      R_t_V[i] += RotMx[ir++] * Vector[iv++];
      R_t_V[i] += RotMx[ir] * Vector[iv];
      R_t_V[i] %= FacTr;
      if (R_t_V[i] < 0) R_t_V[i] += FacTr;
    } else {
      R_t_V[i] = RotMx[ir++] * Vector[iv++];
      R_t_V[i] += RotMx[ir++] * Vector[iv++];
      R_t_V[i++] += RotMx[ir++] * Vector[iv];
      iv = 0;
      R_t_V[i] = RotMx[ir++] * Vector[iv++];
      R_t_V[i] += RotMx[ir++] * Vector[iv++];
      R_t_V[i++] += RotMx[ir++] * Vector[iv];
      iv = 0;
      R_t_V[i] = RotMx[ir++] * Vector[iv++];
      R_t_V[i] += RotMx[ir++] * Vector[iv++];
      R_t_V[i++] += RotMx[ir] * Vector[iv];
    }
  }


  public static void RotMxMultiply(int[] rmxab, int[] rmxa, int[] rmxb) {
/*    int index = 0;

    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++)
        for (int k = 0; k < 3; k++)
          rmxab[index++] = rmxa[k + i] * rmxb[j + 3 * k];*/

    int irmxab = 0;
    int b = 0;
    int ia = 0;

    rmxab[irmxab] = rmxa[ia++] * rmxb[b];
    b += 3; /* r11 */
    rmxab[irmxab] += rmxa[ia++] * rmxb[b];
    b += 3;
    rmxab[irmxab] += rmxa[ia] * rmxb[b];
    b -= 5;
    irmxab++;

    ia = 0;
    rmxab[irmxab] = rmxa[ia++] * rmxb[b];
    b += 3; /* r12 */
    rmxab[irmxab] += rmxa[ia++] * rmxb[b];
    b += 3;
    rmxab[irmxab] += rmxa[ia] * rmxb[b];
    b -= 5;
    irmxab++;

    ia = 0;
    rmxab[irmxab] = rmxa[ia++] * rmxb[b];
    b += 3; /* r13 */
    rmxab[irmxab] += rmxa[ia++] * rmxb[b];
    b += 3;
    rmxab[irmxab] += rmxa[ia++] * rmxb[b];
    b = 0;
    irmxab++;

    int irmxa = ia;
    rmxab[irmxab] = rmxa[ia++] * rmxb[b];
    b += 3; /* r21 */
    rmxab[irmxab] += rmxa[ia++] * rmxb[b];
    b += 3;
    rmxab[irmxab] += rmxa[ia] * rmxb[b];
    b -= 5;
    irmxab++;

    ia = irmxa;
    rmxab[irmxab] = rmxa[ia++] * rmxb[b];
    b += 3; /* r22 */
    rmxab[irmxab] += rmxa[ia++] * rmxb[b];
    b += 3;
    rmxab[irmxab] += rmxa[ia] * rmxb[b];
    b -= 5;
    irmxab++;

    ia = irmxa;
    rmxab[irmxab] = rmxa[ia++] * rmxb[b];
    b += 3; /* r23 */
    rmxab[irmxab] += rmxa[ia++] * rmxb[b];
    b += 3;
    rmxab[irmxab] += rmxa[ia++] * rmxb[b];
    b = 0;
    irmxab++;

    irmxa = ia;
    rmxab[irmxab] = rmxa[ia++] * rmxb[b];
    b += 3; /* r31 */
    rmxab[irmxab] += rmxa[ia++] * rmxb[b];
    b += 3;
    rmxab[irmxab] += rmxa[ia] * rmxb[b];
    b -= 5;
    irmxab++;

    ia = irmxa;
    rmxab[irmxab] = rmxa[ia++] * rmxb[b];
    b += 3; /* r32 */
    rmxab[irmxab] += rmxa[ia++] * rmxb[b];
    b += 3;
    rmxab[irmxab] += rmxa[ia] * rmxb[b];
    b -= 5;
    irmxab++;

    ia = irmxa;
    rmxab[irmxab] = rmxa[ia++] * rmxb[b];
    b += 3; /* r33 */
    rmxab[irmxab] += rmxa[ia++] * rmxb[b];
    b += 3;
    rmxab[irmxab] += rmxa[ia] * rmxb[b];

  }

  public static void RotMxMultiply(T_RTMx rmxab, T_RTMx rmxa, T_RTMx rmxb) {
/*    int index = 0;

    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++)
        for (int k = 0; k < 3; k++)
          rmxab.a(index++, rmxa.a(k + i) * rmxb.a(j + 3 * k));*/

    int irmxab = 0;
    int b = 0;
    int ia = 0;

    rmxab.a(irmxab, rmxa.a(ia++) * rmxb.a(b));
    b += 3; /* r11 */
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia++) * rmxb.a(b));
    b += 3;
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia) * rmxb.a(b));
    b -= 5;
    irmxab++;

    ia = 0;
    rmxab.a(irmxab, rmxa.a(ia++) * rmxb.a(b));
    b += 3; /* r12 */
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia++) * rmxb.a(b));
    b += 3;
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia) * rmxb.a(b));
    b -= 5;
    irmxab++;

    ia = 0;
    rmxab.a(irmxab, rmxa.a(ia++) * rmxb.a(b));
    b += 3; /* r13 */
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia++) * rmxb.a(b));
    b += 3;
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia++) * rmxb.a(b));
    b = 0;
    irmxab++;

    int irmxa = ia;
    rmxab.a(irmxab, rmxa.a(ia++) * rmxb.a(b));
    b += 3; /* r21 */
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia++) * rmxb.a(b));
    b += 3;
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia) * rmxb.a(b));
    b -= 5;
    irmxab++;

    ia = irmxa;
    rmxab.a(irmxab, rmxa.a(ia++) * rmxb.a(b));
    b += 3; /* r22 */
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia++) * rmxb.a(b));
    b += 3;
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia) * rmxb.a(b));
    b -= 5;
    irmxab++;

    ia = irmxa;
    rmxab.a(irmxab, rmxa.a(ia++) * rmxb.a(b));
    b += 3; /* r23 */
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia++) * rmxb.a(b));
    b += 3;
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia++) * rmxb.a(b));
    b = 0;
    irmxab++;

    irmxa = ia;
    rmxab.a(irmxab, rmxa.a(ia++) * rmxb.a(b));
    b += 3; /* r31 */
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia++) * rmxb.a(b));
    b += 3;
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia) * rmxb.a(b));
    b -= 5;
    irmxab++;

    ia = irmxa;
    rmxab.a(irmxab, rmxa.a(ia++) * rmxb.a(b));
    b += 3; /* r32 */
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia++) * rmxb.a(b));
    b += 3;
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia) * rmxb.a(b));
    b -= 5;
    irmxab++;

    ia = irmxa;
    rmxab.a(irmxab, rmxa.a(ia++) * rmxb.a(b));
    b += 3; /* r33 */
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia++) * rmxb.a(b));
    b += 3;
    rmxab.a(irmxab, rmxab.a(irmxab) + rmxa.a(ia) * rmxb.a(b));

  }


  public static void RotateRotMx(int[] RotMx, int[] RMx, int[] InvRMx) {
    int[] BufMx = new int[9];


    RotMxMultiply(BufMx, RotMx, InvRMx);
    RotMxMultiply(RotMx, RMx, BufMx);
  }


  public static void SeitzMxMultiply(T_RTMx smxab, T_RTMx smxa, T_RTMx smxb) {

    RotMxMultiply(smxab, smxa, smxb);

    int ixab = 9;
    int ixa = 0;
    int ixb = 9;
    int ixaa = 9;
    smxab.a(ixab, smxa.a(ixa++) * smxb.a(ixb++)); /* t1 */
    smxab.a(ixab, smxab.a(ixab) + smxa.a(ixa++) * smxb.a(ixb++));
    smxab.a(ixab, smxab.a(ixab) + smxa.a(ixa++) * smxb.a(ixb));
    smxab.a(ixab, smxab.a(ixab) + smxa.a(ixaa++));
    smxab.a(ixab, smxab.a(ixab) % STBF);
    if (smxab.a(ixab) < 0) smxab.a(ixab, smxab.a(ixab) + STBF);
    ixab++;

    ixb = 9;
    smxab.a(ixab, smxa.a(ixa++) * smxb.a(ixb++)); /* t2 */
    smxab.a(ixab, smxab.a(ixab) + smxa.a(ixa++) * smxb.a(ixb++));
    smxab.a(ixab, smxab.a(ixab) + smxa.a(ixa++) * smxb.a(ixb));
    smxab.a(ixab, smxab.a(ixab) + smxa.a(ixaa++));
    smxab.a(ixab, smxab.a(ixab) % STBF);
    if (smxab.a(ixab) < 0) smxab.a(ixab, smxab.a(ixab) + STBF);
    ixab++;

    ixb = 9;
    smxab.a(ixab, smxa.a(ixa++) * smxb.a(ixb++)); /* t3 */
    smxab.a(ixab, smxab.a(ixab) + smxa.a(ixa++) * smxb.a(ixb++));
    smxab.a(ixab, smxab.a(ixab) + smxa.a(ixa) * smxb.a(ixb));
    smxab.a(ixab, smxab.a(ixab) + smxa.a(ixaa));
    smxab.a(ixab, smxab.a(ixab) % STBF);
    if (smxab.a(ixab) < 0) smxab.a(ixab, smxab.a(ixab) + STBF);

  }


  public static void RTMxMultiply(T_RTMx rtmxab, T_RTMx rtmxa, T_RTMx rtmxb,
                                  int FacAug, int FacTr) {
    RotMxMultiply(rtmxab, rtmxa, rtmxb);

    int ixab = 9;
    int ixa = 0;
    int ixb = 9;
    int ixaa = 9;

    if (FacTr > 0) {
      rtmxab.a(ixab, rtmxa.a(ixa++) * rtmxb.a(ixb++)); /* t1 */
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixa++) * rtmxb.a(ixb++));
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixa++) * rtmxb.a(ixb));
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixaa++) * FacAug);
      rtmxab.a(ixab, rtmxab.a(ixab) % FacTr);
      if (rtmxab.a(ixab) < 0) rtmxab.a(ixab, rtmxab.a(ixab) + FacTr);
      ixab++;

      ixb = 9;
      rtmxab.a(ixab, rtmxa.a(ixa++) * rtmxb.a(ixb++)); /* t2 */
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixa++) * rtmxb.a(ixb++));
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixa++) * rtmxb.a(ixb));
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixaa++) * FacAug);
      rtmxab.a(ixab, rtmxab.a(ixab) % FacTr);
      if (rtmxab.a(ixab) < 0) rtmxab.a(ixab, rtmxab.a(ixab) + FacTr);
      ixab++;

      ixb = 9;
      rtmxab.a(ixab, rtmxa.a(ixa++) * rtmxb.a(ixb++)); /* t3 */
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixa++) * rtmxb.a(ixb++));
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixa) * rtmxb.a(ixb));
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixaa) * FacAug);
      rtmxab.a(ixab, rtmxab.a(ixab) % FacTr);
      if (rtmxab.a(ixab) < 0) rtmxab.a(ixab, rtmxab.a(ixab) + FacTr);

    } else {
      rtmxab.a(ixab, rtmxa.a(ixa++) * rtmxb.a(ixb++)); /* t1 */
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixa++) * rtmxb.a(ixb++));
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixa++) * rtmxb.a(ixb));
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixaa++) * FacAug);
      ixab++;

      ixb = 9;
      rtmxab.a(ixab, rtmxa.a(ixa++) * rtmxb.a(ixb++)); /* t2 */
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixa++) * rtmxb.a(ixb++));
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixa++) * rtmxb.a(ixb));
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixaa++) * FacAug);
      ixab++;

      ixb = 9;
      rtmxab.a(ixab, rtmxa.a(ixa++) * rtmxb.a(ixb++)); /* t3 */
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixa++) * rtmxb.a(ixb++));
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixa) * rtmxb.a(ixb));
      rtmxab.a(ixab, rtmxab.a(ixab) + rtmxa.a(ixaa) * FacAug);

    }
  }


  public static void InverseRotMx(int[] RotMx, int[] InvRotMx) {
    InvRotMx[0] = RotMx[4] * RotMx[8] - RotMx[5] * RotMx[7];
    InvRotMx[1] = -RotMx[1] * RotMx[8] + RotMx[2] * RotMx[7];
    InvRotMx[2] = RotMx[1] * RotMx[5] - RotMx[2] * RotMx[4];
    InvRotMx[3] = -RotMx[3] * RotMx[8] + RotMx[5] * RotMx[6];
    InvRotMx[4] = RotMx[0] * RotMx[8] - RotMx[2] * RotMx[6];
    InvRotMx[5] = -RotMx[0] * RotMx[5] + RotMx[2] * RotMx[3];
    InvRotMx[6] = RotMx[3] * RotMx[7] - RotMx[4] * RotMx[6];
    InvRotMx[7] = -RotMx[0] * RotMx[7] + RotMx[1] * RotMx[6];
    InvRotMx[8] = RotMx[0] * RotMx[4] - RotMx[1] * RotMx[3];
  }


  public static void InverseRTMx(T_RTMx RTMx, T_RTMx InvRTMx) {

    InverseRotMx(RTMx.s.R, InvRTMx.s.R);

    InvRTMx.s.T[0] = -InvRTMx.s.R[0] * RTMx.s.T[0] - InvRTMx.s.R[1] * RTMx.s.T[1] - InvRTMx.s.R[2] * RTMx.s.T[2];
    InvRTMx.s.T[1] = -InvRTMx.s.R[3] * RTMx.s.T[0] - InvRTMx.s.R[4] * RTMx.s.T[1] - InvRTMx.s.R[5] * RTMx.s.T[2];
    InvRTMx.s.T[2] = -InvRTMx.s.R[6] * RTMx.s.T[0] - InvRTMx.s.R[7] * RTMx.s.T[1] - InvRTMx.s.R[8] * RTMx.s.T[2];
  }


  public static int IsSMxTransl0(T_LatticeInfo LatticeInfo, int[] SeitzMxT) {
    int iTrV, nTrV, t;
    int[] TrV;


    nTrV = LatticeInfo.nTrVector;
    TrV = LatticeInfo.TrVector;
    int kTrV = 0;

    for (iTrV = 0; iTrV < nTrV; iTrV++) {
      t = (SeitzMxT[0] + TrV[kTrV]) % STBF;
      if (t == 0) {
        t = (SeitzMxT[1] + TrV[kTrV + 1]) % STBF;
        if (t == 0) {
          t = (SeitzMxT[2] + TrV[kTrV + 2]) % STBF;
          if (t == 0)
            return 1;
        }
      }

      kTrV += 3;
    }

    return 0;
  }


  public int IsSpecialSeitzMx(T_RTMx SMx, int ExpandLT) {
    int i, special, smx11;
    T_LatticeInfo ExpLT;


    /* check rotation part for identity or inversion operation */

    smx11 = SMx.s.R[0];
    if (smx11 != 1
            && smx11 != -1)
      return 0;

    for (i = 1; i < 9; i++) {
      if (i % 4 != 0) {
        if (SMx.s.R[i] != 0) return 0;
      } else {
        if (SMx.s.R[i] != smx11) return 0;
      }
    }

    if (smx11 == 1)
      special = SpecialSMx_Identity;
    else
      special = SpecialSMx_Inversion;

    /* rotation part is identity or inversion
       check translation part now
     */

    if (IsSMxTransl0(LatticeInfo, SMx.s.T) == 1)
      return (special | SpecialSMx_Transl0);

    if (ExpandLT != 0 && (smx11 == 1 || Centric != 0)) {
      /* try to expand lattice type */

      ExpLT = null;

      switch (LatticeInfo.getCode()) {
        case 'P':
          if (IsSMxTransl0(LI_A, SMx.s.T) == 1) {
            ExpLT = LI_A;
            break;
          }
          if (IsSMxTransl0(LI_B, SMx.s.T) == 1) {
            ExpLT = LI_B;
            break;
          }
          if (IsSMxTransl0(LI_C, SMx.s.T) == 1) {
            ExpLT = LI_C;
            break;
          }
          if (IsSMxTransl0(LI_I, SMx.s.T) == 1) {
            ExpLT = LI_I;
            break;
          }
          if (IsSMxTransl0(LI_R, SMx.s.T) == 1) {
            ExpLT = LI_R;
            break;
          }
          if (IsSMxTransl0(LI_S, SMx.s.T) == 1) {
            ExpLT = LI_S;
            break;
          }
          if (IsSMxTransl0(LI_T, SMx.s.T) == 1) {
            ExpLT = LI_T;
            break;
          }
        case 'A':
        case 'B':
        case 'C':
          if (IsSMxTransl0(LI_F, SMx.s.T) == 1)
            ExpLT = LI_F;
      }

      if (ExpLT != null) {
        LatticeInfo = new T_LatticeInfo(ExpLT);
        StatusLatticeTr = -1;
        return (special | SpecialSMx_Transl0);
      }
    }

    return special;
  }


  public static int CompareSeitzMx(T_LatticeInfo LatticeInfo,
                                   T_RTMx SeitzMxA, T_RTMx SeitzMxB) {
    int i, iTrV, nTrV, t;
    int[] TrV;


    /* compare rotation part */

    for (i = 0; i < 9; i++)
      if (SeitzMxA.s.R[i] != SeitzMxB.s.R[i]) return 1;

    /* rotation part is same
       check translation
     */

    nTrV = LatticeInfo.nTrVector;
    TrV = LatticeInfo.TrVector;
    int kTrV = 0;

    for (iTrV = 0; iTrV < nTrV; iTrV++, kTrV += 3) {
      t = (SeitzMxA.s.T[0] + TrV[kTrV]) % STBF;
      if (t == SeitzMxB.s.T[0]) {
        t = (SeitzMxA.s.T[1] + TrV[kTrV + 1]) % STBF;
        if (t == SeitzMxB.s.T[1]) {
          t = (SeitzMxA.s.T[2] + TrV[kTrV + 2]) % STBF;
          if (t == SeitzMxB.s.T[2])
            return 0;
        }
      }
    }

    return -1;
  }


  public static int GetRotMxOrder(int[] RotMx) {
    int deter = deterRotMx(RotMx);

    if (deter == -1 || deter == 1) {
      switch (traceRotMx(RotMx)) {
        case -3:
          return -1;
        case -2:
          return -6;
        case -1:
          if (deter == -1)
            return -4;
          else
            return 2;
        case 0:
          if (deter == -1)
            return -3;
          else
            return 3;
        case 1:
          if (deter == -1)
            return -2;
          else
            return 4;
        case 2:
          return 6;
        case 3:
          return 1;
      }
    }

    return 0;
  }


  public int nNextBasis_of_DirCode(char DirCode,
                                   int[] RMx, int[] InvRMx) {
    switch (DirCode) {
      case '.':
        return 1;
      case '=':
      case '"':
      case '\'':
      case '|':
      case '\\':
        copy(RMx, RMx_3_111);
        copy(InvRMx, RMx_3i111);
        return 3;
      case '*':
        copy(RMx, RMx_4_001);
        copy(InvRMx, RMx_4i001);
        return 4;
      default:
        break;
    }

    SetSgError("Internal Error: Corrupt DirCode");
    return -1;
  }

  public void copy(int[] dest, int[] source) {
    for (int i = 0; i < 9; i++)
      dest[i] = source[i];
  }

  public int GetRotMxInfo(int[] RotMx, T_RotMxInfo RotMxInfo, int[] ShTrMx) {
    int i;
    int nNextBasis, iNextBasis;
    int nLoopInv, iLoopInv;
    int Order, AbsOrder;
    int[] RMxCopy = new int[9], MatchMx = new int[9], InvMatchMx = new int[9], REV = new int[3];
    int[] mmx;
    int[] NBRMx = new int[9], InvNBRMx = new int[9];

    int itxrmx;


    Order = GetRotMxOrder(RotMx);

    if (RotMxInfo != null)
      RotMxInfo.Order = Order;

    if (Order != 0) {
      AbsOrder = Math.abs(Order);

      if (Order > 0)
        for (i = 0; i < 9; i++) RMxCopy[i] = RotMx[i];
      else
        for (i = 0; i < 9; i++) RMxCopy[i] = -RotMx[i];

      for (itxrmx = 0; TabXtalRotMx[itxrmx].Order != 0; itxrmx++)
        if (TabXtalRotMx[itxrmx].Order == AbsOrder) break;

      while (TabXtalRotMx[itxrmx].Order == AbsOrder) {
        nNextBasis = nNextBasis_of_DirCode(TabXtalRotMx[itxrmx].getDirCode(), NBRMx, InvNBRMx);

        if (nNextBasis < 0) {
//        	System.out.println("NetBasis");
          return 0;
        }

        if (AbsOrder > 2)
          nLoopInv = 2;
        else
          nLoopInv = 1;

        for (i = 0; i < 9; i++) MatchMx[i] = TabXtalRotMx[itxrmx].RMx[i];

        for (iNextBasis = 0; iNextBasis < nNextBasis; iNextBasis++) {
          if (iNextBasis != 0)
            RotateRotMx(MatchMx, NBRMx, InvNBRMx);

          mmx = MatchMx;

          for (iLoopInv = 0; iLoopInv < nLoopInv; iLoopInv++) {
            if (iLoopInv != 0) {
              InverseRotMx(MatchMx, InvMatchMx);
              mmx = InvMatchMx;
            }

            for (i = 0; i < 9; i++)
              if (mmx[i] != RMxCopy[i]) break;

            if (i == 9) /* matrix has been found */ {
              if (RotMxInfo != null) {
                RotMxInfo.Inverse = iLoopInv;

                if (nNextBasis == 3) {
                  switch (iNextBasis) {
                    case 0:
                      RotMxInfo.setRefAxis('z');
                      break;
                    case 1:
                      RotMxInfo.setRefAxis('x');
                      break;
                    case 2:
                      RotMxInfo.setRefAxis('y');
                      break;
                  }
                } else
                  RotMxInfo.setRefAxis('o');

                RotMxInfo.setDirCode(TabXtalRotMx[itxrmx].getDirCode());

                for (i = 0; i < 3; i++)
                  RotMxInfo.EigenVector[i] = TabXtalRotMx[itxrmx].EigenVector[i];

                for (i = iNextBasis; i-- != 0;) {
                  RotMx_t_Vector(REV, NBRMx, RotMxInfo.EigenVector, 0);

                  if (i-- == 0) {
                    for (i = 0; i < 3; i++)
                      RotMxInfo.EigenVector[i] = REV[i];

                    break;
                  }

                  RotMx_t_Vector(RotMxInfo.EigenVector, NBRMx, REV, 0);
                }
              }

              if (ShTrMx != null) {
                i = 0;
                if (Order < 0) i++;
                if (iLoopInv != 0) i += 2;
                for (int kj = 0; kj < 9; kj++)
                  ShTrMx[kj] = TabXtalRotMx[itxrmx].ShTrMx[i + kj];

                for (i = iNextBasis; i != 0; i--)
                  RotateRotMx(ShTrMx, NBRMx, InvNBRMx);
              }

              return Order;
            }
          }
        }

        itxrmx++;
      }
    }

    return 0;
  }


  public T_RotMxInfo ListOrBufRotMxInfo(int iList, T_RotMxInfo BufRotMxInfo) {
    T_RotMxInfo RMxI;
    int iRMxI = 0;


    if (ListRotMxInfo != null)
      RMxI = ListRotMxInfo[iList];
    else {
      RMxI = BufRotMxInfo;

      if (GetRotMxInfo(ListSeitzMx[iList].s.R, RMxI, null) == 0) {
        SetSgError(Err_Ill_SMx_in_List);
        return null;
      }
    }

    return RMxI;
  }


  public int CoreAdd2ListSeitzMx(T_RTMx NewSMx) {
    int i, iList;
    T_RotMxInfo RotMxInfo = new T_RotMxInfo();
    T_LatticeInfo LI;

    String Err_NonXtalOp =
            "Error: Generators produce non-crystallographic operation";


    if (GenOption != 0)
      LI = LatticeInfo;
    else
      LI = LI_P;

    int ilsmx = 0;
    for (iList = 0; iList < nList; iList++, ilsmx++)
      if (CompareSeitzMx(LI, NewSMx, ListSeitzMx[ilsmx]) == 0)
        return 0; /* matrix is not unique */

    if (GetRotMxInfo(NewSMx.s.R, RotMxInfo, null) == 0) {
      SetSgError(Err_NonXtalOp);
      return -1;
    }

    if (nList >= MaxList) {
      if (nList >= 192)
        SetSgError(Err_NonXtalOp + " :nList");
      else
        SetSgError("Internal Error: Allocated space for ListSeitzMx too small");

      return -1;
    }

/*      System.out.println("Add");
      System.out.println(ilsmx);
      System.out.println(NewSMx.s.T[0]);
      System.out.println(NewSMx.s.T[1]);
      System.out.println(NewSMx.s.T[2]);*/
    ListSeitzMx[ilsmx].copy(NewSMx);

    if (ListRotMxInfo != null)
      CopyRotMxInfo(ListRotMxInfo[nList], RotMxInfo);

    nList++;

    return 1;
  }


  public int Add2ListSeitzMx(T_RTMx NewSMx) {
    int i, special, Enter;
    int iMult, jMult;
    T_RTMx TrialSMx = new T_RTMx();
    ;

    String Err_Ill_lattice_translation =
            "Error: Illegal lattice translation";

    if (nList == 0) {
      /* make sure identity matrix is first in list */

      InitSeitzMx(TrialSMx, 1);

      if (CoreAdd2ListSeitzMx(TrialSMx) < 0)
        return -1;
    }

    for (i = 0; i < 9; i++)
      TrialSMx.s.R[i] = NewSMx.s.R[i];

    for (i = 0; i < 3; i++) {
      TrialSMx.s.T[i] = NewSMx.s.T[i] % STBF;
      if (TrialSMx.s.T[i] < 0)
        TrialSMx.s.T[i] += STBF;
    }
/*      System.out.println("Trial");
      System.out.println(TrialSMx.s.T[0]);
      System.out.println(TrialSMx.s.T[1]);
      System.out.println(TrialSMx.s.T[2]);*/


    iMult = nList;

    jMult = 1; /* skip first = identity matrix */

    for (; ;) {
      Enter = 1;

      special = IsSpecialSeitzMx(TrialSMx, 1);

/*System.out.println(special);
System.out.println(GenOption);
System.out.println(Centric);
System.out.println(InversionOffOrigin);
System.out.println(SpecialSMx_Inversion);
System.out.println(SpecialSMx_Transl0);
System.out.println(SpecialSMx_Identity);*/
      if ((special & SpecialSMx_Identity) != 0) {
        if (!((special & SpecialSMx_Transl0) != 0)) {
          SetSgError(Err_Ill_lattice_translation);
          return -1;
        }

        if (GenOption != 0)
          Enter = 0;
      } else if ((special & SpecialSMx_Inversion) != 0) {
        if (!((special & SpecialSMx_Transl0) != 0)) {
          if (Centric != 0) {
            SetSgError(Err_Ill_lattice_translation);
            return -1;
          }

          InversionOffOrigin = 1;
        } else {
          if (InversionOffOrigin != 0)
            Centric = 1;

          InversionOffOrigin = 0;

          if (GenOption != 0) {
            if (Centric == 0)
              Centric = -1;

            Enter = 0;
          } else
            Centric = 1;
        }
      }
//System.out.println("Enter");
//System.out.println(Enter);
      if (Enter != 0 && CoreAdd2ListSeitzMx(TrialSMx) < 0)
        return -1;

      if (GenOption < 0)
        break;

      if (jMult > iMult) {
        iMult++;

        jMult = 1; /* skip first = identity matrix */
      }

      if (iMult == nList)
        break;

      SeitzMxMultiply(TrialSMx, ListSeitzMx[jMult], ListSeitzMx[iMult]);

      jMult++;
    }

    return 0;
  }

/* pointer sintax checked from here */
  public int AddInversion2ListSeitzMx() {
    T_RTMx SeitzMx = new T_RTMx();

    InitSeitzMx(SeitzMx, -1);
    return Add2ListSeitzMx(SeitzMx);
  }


  public int AddLatticeTr2ListSeitzMx(T_LatticeInfo LatticeInfo) {
    int iTrV, nTrV, i;
    int[] TrV;
    T_RTMx SeitzMx = new T_RTMx();


    InitRotMx(SeitzMx.s.R, 1);

    nTrV = LatticeInfo.nTrVector;
    TrV = LatticeInfo.TrVector; /* skip first vector which is always 000 */

    i = 3;
    for (iTrV = 1; iTrV < nTrV; iTrV++) {
      SeitzMx.s.T[0] = TrV[i++];
      SeitzMx.s.T[1] = TrV[i++];
      SeitzMx.s.T[2] = TrV[i++];

/*      System.out.println("T");
      System.out.println(SeitzMx.s.T[0]);
      System.out.println(SeitzMx.s.T[1]);
      System.out.println(SeitzMx.s.T[2]);*/

      if (Add2ListSeitzMx(SeitzMx) < 0)
        return -1;
    }

    if (GenOption != 0)
      StatusLatticeTr = 0;
    else
      StatusLatticeTr = 1;

    return 0;
  }


  public int RemoveLatticeTr() {
    int iList, jList, i;

    if (LatticeInfo.getCode() == 'P')
      return 0;

    if (StatusLatticeTr == -1) {
      if (AddLatticeTr2ListSeitzMx(LatticeInfo) < 0)
        return -1;
    }

    iList = 0;

    while (iList < nList) {
      jList = iList + 1;

      while (jList < nList) {
        if (CompareSeitzMx(LatticeInfo, ListSeitzMx[iList], ListSeitzMx[jList]) == 0) {
          /* copy last element to this place */

          nList--;
          ListSeitzMx[jList].copy(ListSeitzMx[nList]);

          if (jList < ListRotMxInfo.length)
            CopyRotMxInfo(ListRotMxInfo[jList], ListRotMxInfo[nList]);
        } else
          jList++;
      }

      iList++;
    }

    StatusLatticeTr = 0;

    return 0;
  }


  public int RemoveInversion() {
    int i, iList, special, deter, lCentric, lInversionOffOrigin;
    T_RTMx ProperSMx = new T_RTMx();


    lCentric = 0;
    lInversionOffOrigin = 0;

    int ilsmx = 0;
    for (iList = 0; iList < nList; iList++, ilsmx++) {
      special = IsSpecialSeitzMx(ListSeitzMx[ilsmx], 0);

      if ((special & SpecialSMx_Inversion) != 0) {
        if ((special & SpecialSMx_Transl0) != 0)
          lCentric = 1;
        else
          lInversionOffOrigin = 1;

        break;
      }
    }

    if (lInversionOffOrigin != 0 && lCentric != 0) {
      SetSgError("Internal Error: Corrupt SgInfo");
      return -1;
    }

    if (lCentric == 0) {
      if (lInversionOffOrigin != 0) {
        Centric = 0;
        InversionOffOrigin = 1;
      } else {
        if (Centric != 0) Centric = -1;
        InversionOffOrigin = 0;
      }
    } else {
      InversionOffOrigin = 0;

      ilsmx = 0;
      int ilrmxi = 0;
      iList = 0;

      while (iList < nList) {
        deter = deterRotMx(ListSeitzMx[ilsmx].s.R);

        if (deter == -1 && Centric == -1) {
          for (i = 0; i < 9; i++)
            ProperSMx.s.R[i] = -ListSeitzMx[ilsmx].s.R[i];

          for (i = 0; i < 3; i++) {
            ProperSMx.s.T[i] = -ListSeitzMx[ilsmx].s.T[i] % STBF;
            if (ProperSMx.s.T[i] < 0)
              ProperSMx.s.T[i] += STBF;
          }

          int ismx = 0;

          for (i = 0; i < nList; i++, ismx++)
            if (CompareSeitzMx(LI_P, ProperSMx, ListSeitzMx[ismx]) == 0) break;

          if (i == nList) {
            ListSeitzMx[ilsmx].copy(ProperSMx);

            deter = deterRotMx(ListSeitzMx[ilsmx].s.R);

            if (deter != 1
                    || (ListRotMxInfo[ilrmxi] != null && GetRotMxInfo(ListSeitzMx[ilsmx].s.R, ListRotMxInfo[ilrmxi], null) == 0)) {
              SetSgError(Err_Ill_SMx_in_List);
              return -1;
            }
          }
        }

        if (deter == -1) {
/* copy last element to this place */

          nList--;
          if (nList != iList) {
            ListSeitzMx[ilsmx].copy(ListSeitzMx[nList]);

            if (ilrmxi < ListRotMxInfo.length) {
              CopyRotMxInfo(ListRotMxInfo[ilrmxi], ListRotMxInfo[nList]);
            }
          }
        } else if (deter == 1) {
          ilsmx++;
          if (ilrmxi < ListRotMxInfo.length - 1) ilrmxi++;
          iList++;
        } else {
          SetSgError(Err_Ill_SMx_in_List);
          return -1;
        }
      }

      Centric = -1;
    }

    return 0;
  }


  public int ApplyOriginShift() {
    int HaveOrSh, iList, i, ilsmx;
    T_RTMx SMx = new T_RTMx();
    int[] OrSh = new int[3], lo = new int[3];


    HaveOrSh = 0;

    for (i = 0; i < 3; i++) {
      OrSh[i] = OriginShift[i] * (STBF / 12);
      if (OrSh[i] != 0) HaveOrSh = 1;
    }

    if (HaveOrSh == 0)
      return 0;

    ilsmx = 0;

    for (iList = 0; iList < nList; iList++, ilsmx++) {
      RotMx_t_Vector(lo, ListSeitzMx[ilsmx].s.R, OrSh, STBF);

      for (i = 0; i < 3; i++)
        ListSeitzMx[ilsmx].s.T[i] = iModPositive(ListSeitzMx[ilsmx].s.T[i] - lo[i] + OrSh[i], STBF);
    }

    if (Centric == -1) {
      InitSeitzMx(SMx, -1);

      RotMx_t_Vector(lo, SMx.s.R, OrSh, STBF);

      for (i = 0; i < 3; i++)
        SMx.s.T[i] = iModPositive(SMx.s.T[i] - lo[i] + OrSh[i], STBF);

      if (CoreAdd2ListSeitzMx(SMx) < 0)
        return -1;

      Centric = 0;
      InversionOffOrigin = 1;
    }

    return 1;
  }


  public void TidyTranslation() {
    int iList;
    int iTrV, nTrV;
//  	int[]  		 TrV;
    int ilsmx;
    int t1, t2, t3, mint1, mint2, mint3, t1i, t2i, t3i;
    int n0t, n0mint;


    nTrV = 3 * LatticeInfo.nTrVector;

    ilsmx = 0;

    for (iList = 0; iList < nList; iList++, ilsmx++) {
      mint1 = ListSeitzMx[ilsmx].s.T[0];
      mint2 = ListSeitzMx[ilsmx].s.T[1];
      mint3 = ListSeitzMx[ilsmx].s.T[2];

//    	TrV = LatticeInfo.TrVector;

      for (iTrV = 0; iTrV < nTrV; iTrV += 3) {
        t1 = (ListSeitzMx[ilsmx].s.T[0] + LatticeInfo.TrVector[iTrV]) % STBF;
        t2 = (ListSeitzMx[ilsmx].s.T[1] + LatticeInfo.TrVector[iTrV + 1]) % STBF;
        t3 = (ListSeitzMx[ilsmx].s.T[2] + LatticeInfo.TrVector[iTrV + 2]) % STBF;


        //to check carefully

        if (t1 == 0)
          t1i = 1;
        else
          t1i = 0;
        if (t2 == 0)
          t2i = 1;
        else
          t2i = 0;
        if (t3 == 0)
          t3i = 1;
        else
          t3i = 0;
        n0t = t1i + t2i + t3i;

        if (mint1 == 0)
          t1i = 1;
        else
          t1i = 0;
        if (mint2 == 0)
          t2i = 1;
        else
          t2i = 0;
        if (mint3 == 0)
          t3i = 1;
        else
          t3i = 0;
        n0mint = t1i + t2i + t3i;

        if (n0t > n0mint
                || (n0t == n0mint
                && (t1 + t2 + t3 < mint1 + mint2 + mint3
                || (t1 + t2 + t3 == mint1 + mint2 + mint3
                && (t1 < mint1 || (t1 == mint1 && t2 < mint2)))))) {
          mint1 = t1;
          mint2 = t2;
          mint3 = t3;
        }
      }

      ListSeitzMx[ilsmx].s.T[0] = mint1;
      ListSeitzMx[ilsmx].s.T[1] = mint2;
      ListSeitzMx[ilsmx].s.T[2] = mint3;
    }
  }


  public static int CmpEigenVectors(int[] EVa, int[] EVb) {
    int val_a, val_b, n0_a, n0_b, i;


    n0_a = n0_b = 0;

    for (i = 0; i < 3; i++) {
      if (EVa[i] == 0) n0_a++;
      if (EVb[i] == 0) n0_b++;
    }
    if (n0_a > n0_b) return -1;
    if (n0_a < n0_b) return 1;

    val_a = val_b = 0;

    for (i = 0; i < 3; i++) {
      if (val_a < Math.abs(EVa[i]))
        val_a = Math.abs(EVa[i]);
      if (val_b < Math.abs(EVb[i]))
        val_b = Math.abs(EVb[i]);
    }
    if (val_a < val_b) return -1;
    if (val_a > val_b) return 1;

    val_a = 100 * Math.abs(EVa[2]);
    val_a += 10 * Math.abs(EVa[0]);
    val_a += Math.abs(EVa[1]);
    val_b = 100 * Math.abs(EVb[2]);
    val_b += 10 * Math.abs(EVb[0]);
    val_b += Math.abs(EVb[1]);

    if (n0_a < 2) {
      if (val_a < val_b) return -1;
      if (val_a > val_b) return 1;
    } else {
      if (val_a > val_b) return -1;
      if (val_a < val_b) return 1;
    }

    for (i = 0; i < 3; i++) {
      if (EVa[i] > EVb[i]) return -1;
      if (EVa[i] < EVb[i]) return 1;
    }

    return 0;
  }


  class listComparer implements Comparator {
    public int compare(Object obj_a, Object obj_b) {
      int iList_a = ((Integer) obj_a).intValue();
      int iList_b = ((Integer) obj_b).intValue();

      int val_a, val_b, i;
      T_RotMxInfo RotMxInfo_a = new T_RotMxInfo(), RotMxInfo_b = new T_RotMxInfo();


      if (SgError != null) return 0;

      if (ListRotMxInfo == null) {
        if (GetRotMxInfo(ListSeitzMx[iList_a].s.R, RotMxInfo_a, null) == 0
                || GetRotMxInfo(ListSeitzMx[iList_b].s.R, RotMxInfo_b, null) == 0) {
          SetSgError(Err_Ill_SMx_in_List);
          return 0;
        }
      } else {
        RotMxInfo_a = ListRotMxInfo[iList_a];
        RotMxInfo_b = ListRotMxInfo[iList_b];
      }

      val_a = Math.abs(RotMxInfo_a.Order);
      val_b = Math.abs(RotMxInfo_b.Order);

      if (val_a == 1 && val_b != 1) return -1;
      if (val_a != 1 && val_b == 1) return 1;
      if (RotMxInfo_a.Order == 1 && RotMxInfo_b.Order != 1) return -1;
      if (RotMxInfo_a.Order != 1 && RotMxInfo_b.Order == 1) return 1;

      if (val_a != 1) {
        if (val_a > val_b) return -1;
        if (val_a < val_b) return 1;
        if (RotMxInfo_a.Order > RotMxInfo_b.Order) return -1;
        if (RotMxInfo_a.Order < RotMxInfo_b.Order) return 1;
      }

      i = CmpEigenVectors(RotMxInfo_a.EigenVector, RotMxInfo_b.EigenVector);
      if (i != 0)
        return i;

      if (RotMxInfo_a.Inverse < RotMxInfo_b.Inverse) return -1;
      if (RotMxInfo_a.Inverse > RotMxInfo_b.Inverse) return 1;

      for (i = 0; i < 3; i++) {
        if (ListSeitzMx[iList_a].s.T[i] < ListSeitzMx[iList_b].s.T[i]) return -1;
        if (ListSeitzMx[iList_a].s.T[i] > ListSeitzMx[iList_b].s.T[i]) return 1;
      }

      return 0;
    }
  }


  public int PostSortSgInfoList(int[] List_iList) {
    int iL_iL, jL_iL;
    T_RTMx BufMx1 = new T_RTMx(), BufMx2 = new T_RTMx(), smxab;
    T_RTMx lsmx, smxb;
    T_RotMxInfo RotMxInfo = new T_RotMxInfo();
    T_RotMxInfo rmxi;
    int nO_1, iO, save, i, i_;


    iL_iL = 0;

    while (iL_iL < nList) {
      lsmx = ListSeitzMx[List_iList[iL_iL]];

      rmxi = ListOrBufRotMxInfo(List_iList[iL_iL], RotMxInfo);
      if (rmxi == null)
        return -1;

      iL_iL++;

      iO = rmxi.Order;
      if (iO < 0 && iO % 2 != 0) iO *= 2;
      nO_1 = Math.abs(iO) - 1;

      smxab = BufMx2;
      smxb = lsmx;

      for (iO = 1; iO < nO_1; iO++) {
        SeitzMxMultiply(smxab, lsmx, smxb);

        for (jL_iL = iL_iL; jL_iL < nList; jL_iL++) {
          smxb = ListSeitzMx[List_iList[jL_iL]];
          if (CompareSeitzMx(LatticeInfo, smxab, smxb) == 0)
            break;
        }

        if (jL_iL < nList) {
          save = List_iList[jL_iL];

          for (i = i_ = jL_iL; i > iL_iL; i = i_)
            List_iList[i] = List_iList[--i_];

          List_iList[iL_iL++] = save;
        }

        smxb = smxab;
        if (iO % 2 != 0)
          smxab = BufMx1;
        else
          smxab = BufMx2;
      }
    }

    return 0;
  }


  public void SortSgInfoList(int[] List_iList) {
    int i, j, refi;
    T_RTMx[] lsmx;
    T_RotMxInfo[] lrmxi;
    Vector vectorList = new Vector(nList, 1);


    if (SgError != null) return;

    lsmx = ListSeitzMx;
    lrmxi = ListRotMxInfo;

    for (i = 0; i < nList; i++) vectorList.addElement(new Integer(i));

    Collections.sort(vectorList, new listComparer());

    for (i = 0; i < nList; i++) List_iList[i] = ((Integer) vectorList.elementAt(i)).intValue();

    vectorList.removeAllElements();
    vectorList = null;

    if (SgError != null) return;

    if (PostSortSgInfoList(List_iList) != 0)
      return;

    for (i = 0; i < nList; i++) {
      j = List_iList[i];

      if (j != i) {
        for (refi = i + 1; refi < nList; refi++)
          if (List_iList[refi] == i) break;

        if (refi >= nList) {
          SetSgError("Internal Error: SortSgInfoList(): Corrupt List_iList");
          return;
        }

        SwapRTMx(lsmx[i], lsmx[j]);
        if (lrmxi != null)
          SwapRotMxInfo(lrmxi[i], lrmxi[j]);

        List_iList[refi] = j;
      }
    }
  }


  public int FindSeitzMx(int Order, int HonorSign, int RefAxis, int DirCode) {
    int iList;
    boolean MatchingOrder;
    T_RTMx lsmx;
    T_RotMxInfo lrmxi, RotMxInfo = new T_RotMxInfo();


    int ilsmx = 0;
    int ilrmxi = 0;
    lrmxi = ListRotMxInfo[ilrmxi++];

    if (lrmxi == null) {
      lrmxi = RotMxInfo;
//  		System.out.println("Is a problem");
    }

    for (iList = 0; iList < nList; iList++, ilsmx++) {
      if (lrmxi == RotMxInfo) {
        if (GetRotMxInfo(ListSeitzMx[ilsmx].s.R, lrmxi, null) == 0) {
          SetSgError(Err_Ill_SMx_in_List);
          return -1;
        }
      }

      if (HonorSign == 0)
        MatchingOrder = (Math.abs(Order) == Math.abs(lrmxi.Order));
      else
        MatchingOrder = (Order == lrmxi.Order);

      if (MatchingOrder
              && lrmxi.Inverse == 0
              && (RefAxis == 0 || RefAxis == lrmxi.getIntRefAxis())
              && (DirCode == 0 || DirCode == lrmxi.getIntDirCode())) {
        if (DirCode != '*') return iList;

        if (lrmxi.EigenVector[0] == 1
                && lrmxi.EigenVector[1] == 1
                && lrmxi.EigenVector[2] == 1)
          return iList;
      }

      if (lrmxi != RotMxInfo) lrmxi = ListRotMxInfo[ilrmxi++];
    }

    return -1;
  }


  public int FindXtalSystem() {
    int iList, i;
    int HonorSign = 0, CheckEnantiomorph;
    T_RTMx lsmx;
    T_RotMxInfo RotMxInfo = new T_RotMxInfo();
    T_RotMxInfo lrmxi;

    int N1 = 0, N2 = 1, N3 = 2, N4 = 3, N6 = 4, EndOfAxis = 5;
    int[] N_count = new int[EndOfAxis];


    XtalSystem = XS_Unknown;
    UniqueRefAxis = 0;
    UniqueDirCode = 0;
    ExtraInfo = EI_Unknown;

    CheckEnantiomorph = 0;

    for (i = 0; i < EndOfAxis; i++) N_count[i] = 0;

    int ilsmx = 0;
    int ilrmxi = 0;
    lsmx = ListSeitzMx[ilsmx];
    lrmxi = ListRotMxInfo[ilrmxi];

    if (lrmxi == null) lrmxi = RotMxInfo;

    for (iList = 0; iList < nList; iList++, ilsmx++) {
      if (lrmxi == RotMxInfo) {
        if (GetRotMxInfo(lsmx.s.R, RotMxInfo, null) == 0) {
          SetSgError(Err_Ill_SMx_in_List);
          return XS_Unknown;
        }
      }

      switch (Math.abs(lrmxi.Order)) {
        case 1:
          i = N1;
          break;
        case 2:
          i = N2;
          break;
        case 3:
          i = N3;
          break;
        case 4:
          i = N4;
          break;
        case 6:
          i = N6;
          break;
        default:
          SetSgError("Internal Error: FindXtalSystem(): Corrupt ListRotMxInfo");
          return XS_Unknown;
      }

      if (lrmxi.Inverse == 0) /* skip N^-1 */
        N_count[i]++;

      if (lrmxi != RotMxInfo) {
        ilrmxi++;
        lrmxi = ListRotMxInfo[ilrmxi];
      }
    }

    i = EndOfAxis;

    if (InversionOffOrigin == 1) {
      for (i = 0; i < EndOfAxis; i++) {
        if (N_count[i] % 2 != 0) break;
        N_count[i] /= 2;
      }
    }

    if (i == EndOfAxis) {
      if (N_count[N3] == 4)
        XtalSystem = XS_Cubic;
      else if (N_count[N3] > 1)
        XtalSystem = XS_Unknown;
      else if (N_count[N6] == 1)
        XtalSystem = XS_Hexagonal;
      else if (N_count[N6] > 0)
        XtalSystem = XS_Unknown;
      else if (N_count[N3] == 1)
        XtalSystem = XS_Trigonal;
      else if (N_count[N4] == 1)
        XtalSystem = XS_Tetragonal;
      else if (N_count[N4] > 0)
        XtalSystem = XS_Unknown;
      else if (N_count[N2] > 2)
        XtalSystem = XS_Orthorhombic;
      else if (N_count[N2] > 0)
        XtalSystem = XS_Monoclinic;
      else if (N_count[N1] > 0) XtalSystem = XS_Triclinic;

      HonorSign = 1;
      iList = FindSeitzMx(-1, HonorSign, 'o', '.');
      if (iList < 0) HonorSign = 0;

      switch (XtalSystem) {
        case XS_Monoclinic:
          iList = FindSeitzMx(2, HonorSign, 0, '=');
          if (iList < 0) XtalSystem = XS_Unknown;
          break;
        case XS_Tetragonal:
          CheckEnantiomorph = 1;
          iList = FindSeitzMx(4, HonorSign, 0, '=');
          if (iList < 0) XtalSystem = XS_Unknown;
          break;
        case XS_Trigonal:
          CheckEnantiomorph = 1;
          iList = FindSeitzMx(3, HonorSign, 0, '=');
          if (iList < 0)
            iList = FindSeitzMx(3, HonorSign, 0, '*');
          if (iList < 0) XtalSystem = XS_Unknown;
          break;
        case XS_Hexagonal:
          CheckEnantiomorph = 1;
          iList = FindSeitzMx(6, HonorSign, 0, '=');
          if (iList < 0) XtalSystem = XS_Unknown;
          break;
        case XS_Cubic:
          iList = FindSeitzMx(4, HonorSign, 0, '=');
          if (iList >= 0) CheckEnantiomorph = 1;
          break;
        default:
          iList = -1;
          break;
      }
    }

    if (XtalSystem == XS_Unknown) {
      SetSgError("Error: Can't determine crystal system");
    } else if (iList >= 0) {
      lrmxi = ListOrBufRotMxInfo(iList, RotMxInfo);
      if (lrmxi == null) {
        XtalSystem = XS_Unknown;
        return XS_Unknown;
      }

      if (XtalSystem != XS_Cubic) {
        UniqueRefAxis = lrmxi.getIntRefAxis();
        UniqueDirCode = lrmxi.getIntDirCode();

        if (XtalSystem == XS_Trigonal && lrmxi.getDirCode() == '=') {
          switch (lrmxi.getRefAxis()) {
            case 'z':
              switch (LatticeInfo.getCode()) {
                case 'R':
                  ExtraInfo = EI_Obverse;
                  break;
                case 'T':
                  ExtraInfo = EI_Reverse;
                  break;
              }
              break;
            case 'y':
              switch (LatticeInfo.getCode()) {
                case 'S':
                  ExtraInfo = EI_Obverse;
                  break;
                case 'R':
                  ExtraInfo = EI_Reverse;
                  break;
              }
              break;
            case 'x':
              switch (LatticeInfo.getCode()) {
                case 'T':
                  ExtraInfo = EI_Obverse;
                  break;
                case 'S':
                  ExtraInfo = EI_Reverse;
                  break;
              }
              break;
          }
        }
      }

      if (HonorSign == 0 /* no inversion matrix */
              && LatticeInfo.getCode() == 'P'
              && CheckEnantiomorph == 1) {
        lsmx = ListSeitzMx[iList];

        if (GetRotMxOrder(lsmx.s.R) > 1) {
          i = lsmx.s.T[0] * lrmxi.EigenVector[0];
          i += lsmx.s.T[1] * lrmxi.EigenVector[1];
          i += lsmx.s.T[2] * lrmxi.EigenVector[2];

          if (i % (STBF / 2) != 0) ExtraInfo = EI_Enantiomorphic;
        }
      }
    }

    return XtalSystem;
  }


  public int BuildGenerator_iList() {
    int iList, iList_1, nG;
    int HonorSign, Flag3asterisk, FlagPG;


    boolean SgInfo_CI = (Centric != 0 || InversionOffOrigin != 0);

    PointGroup = SgGroups.PG_Unknown;

    nG = nGenerator = 0;

    HonorSign = 1;
    iList_1 = FindSeitzMx(-1, HonorSign, 'o', '.');
    if (iList_1 < 0) HonorSign = 0;

    if (XtalSystem == XS_Unknown)
      FindXtalSystem();

    switch (XtalSystem) {
      case XS_Triclinic:
        if (iList_1 < 0)
          iList_1 = FindSeitzMx(1, HonorSign, 'o', '.');
        if (iList_1 >= 0) Generator_iList[nG++] = iList_1;

        if (SgInfo_CI)
          PointGroup = SgGroups.PG_1b;
        else
          PointGroup = SgGroups.PG_1;

        nGenerator = nG;
        return 0;

      case XS_Monoclinic:
        iList = FindSeitzMx(2, HonorSign, 0, '=');
        if (iList < 0) break;
        Generator_iList[nG++] = iList;

        if (SgInfo_CI)
          PointGroup = SgGroups.PG_2_m;
        else if (deterRotMx(ListSeitzMx[iList].s.R) == -1)
          PointGroup = SgGroups.PG_m;
        else
          PointGroup = SgGroups.PG_2;

        if (iList_1 >= 0) Generator_iList[nG++] = iList_1;

        nGenerator = nG;
        return 0;

      case XS_Orthorhombic:
        iList = FindSeitzMx(2, HonorSign, 'z', '=');
        if (iList >= 0) Generator_iList[nG++] = iList;

        iList = FindSeitzMx(2, HonorSign, 'x', '=');
        if (iList >= 0) Generator_iList[nG++] = iList;

        if (nG < 2) {
          iList = FindSeitzMx(2, HonorSign, 'y', '=');
          if (iList >= 0) Generator_iList[nG++] = iList;
        }

        if (nG != 2) break;

        if (SgInfo_CI)
          PointGroup = SgGroups.PG_mmm;
        else if (deterRotMx(ListSeitzMx[Generator_iList[0]].s.R) == -1
                || deterRotMx(ListSeitzMx[Generator_iList[1]].s.R) == -1)
          PointGroup = SgGroups.PG_mm2;
        else
          PointGroup = SgGroups.PG_222;

        if (iList_1 >= 0) Generator_iList[nG++] = iList_1;

        nGenerator = nG;
        return 0;

      case XS_Tetragonal:
        iList = FindSeitzMx(4, HonorSign, 0, '=');
        if (iList < 0) break;
        Generator_iList[nG++] = iList;

        if (UniqueRefAxis != 'x') {
          iList = FindSeitzMx(2, HonorSign, 'x', '=');
          if (iList >= 0) Generator_iList[nG++] = iList;
        }
        if (nG < 2 && UniqueRefAxis != 'z') {
          iList = FindSeitzMx(2, HonorSign, 'z', '=');
          if (iList >= 0) Generator_iList[nG++] = iList;
        }
        if (nG < 2 && UniqueRefAxis != 'y') {
          iList = FindSeitzMx(2, HonorSign, 'y', '=');
          if (iList >= 0) Generator_iList[nG++] = iList;
        }

        if (nG < 2) {
          if (SgInfo_CI)
            PointGroup = SgGroups.PG_4_m;
          else if (deterRotMx(ListSeitzMx[Generator_iList[0]].s.R) == -1)
            PointGroup = SgGroups.PG_4b;
          else
            PointGroup = SgGroups.PG_4;
        } else {
          if (SgInfo_CI)
            PointGroup = SgGroups.PG_4_mmm;
          else if (deterRotMx(ListSeitzMx[Generator_iList[0]].s.R) == -1) {
            if (deterRotMx(ListSeitzMx[Generator_iList[1]].s.R) == -1)
              PointGroup = SgGroups.PG_4bm2;
            else
              PointGroup = SgGroups.PG_4b2m;
          } else {
            if (deterRotMx(ListSeitzMx[Generator_iList[1]].s.R) == -1)
              PointGroup = SgGroups.PG_4mm;
            else
              PointGroup = SgGroups.PG_422;
          }
        }

        if (iList_1 >= 0) Generator_iList[nG++] = iList_1;

        nGenerator = nG;
        return 0;

      case XS_Trigonal:
      case XS_Hexagonal:
        Flag3asterisk = 0;

        if (XtalSystem == XS_Trigonal) {
          iList = FindSeitzMx(3, HonorSign, 0, '=');
          if (iList < 0) {
            iList = FindSeitzMx(3, HonorSign, 0, '*');
            Flag3asterisk = 1;
          }
        } else
          iList = FindSeitzMx(6, HonorSign, 0, '=');

        if (iList < 0) break;
        Generator_iList[nG++] = iList;

        iList = FindSeitzMx(2, HonorSign, 0, '\'');
        if (iList >= 0) Generator_iList[nG++] = iList;

        FlagPG = -1;

        if (nG < 2) {
          iList = FindSeitzMx(2, HonorSign, 0, '"');
          if (iList >= 0) Generator_iList[nG++] = iList;
          FlagPG = 1;
        }

        if (XtalSystem == XS_Trigonal) {
          if (nG < 2) {
            if (SgInfo_CI)
              PointGroup = SgGroups.PG_3b;
            else
              PointGroup = SgGroups.PG_3;
          } else {
            if (Flag3asterisk == 1) {
              if (SgInfo_CI)
                PointGroup = SgGroups.PG_3bm;
              else {
                FlagPG = deterRotMx(ListSeitzMx[Generator_iList[1]].s.R);
                if (FlagPG == -1)
                  PointGroup = SgGroups.PG_3m;
                else
                  PointGroup = SgGroups.PG_32;
              }
            } else if (FlagPG == -1) {
              if (SgInfo_CI)
                PointGroup = SgGroups.PG_3b1m;
              else {
                FlagPG = deterRotMx(ListSeitzMx[Generator_iList[1]].s.R);
                if (FlagPG == -1)
                  PointGroup = SgGroups.PG_31m;
                else
                  PointGroup = SgGroups.PG_312;
              }
            } else {
              if (SgInfo_CI)
                PointGroup = SgGroups.PG_3bm1;
              else {
                FlagPG = deterRotMx(ListSeitzMx[Generator_iList[1]].s.R);
                if (FlagPG == -1)
                  PointGroup = SgGroups.PG_3m1;
                else
                  PointGroup = SgGroups.PG_321;
              }
            }
          }
        } else {
          if (nG < 2) {
            if (SgInfo_CI)
              PointGroup = SgGroups.PG_6_m;
            else if (deterRotMx(ListSeitzMx[Generator_iList[0]].s.R) == -1)
              PointGroup = SgGroups.PG_6b;
            else
              PointGroup = SgGroups.PG_6;
          } else {
            if (SgInfo_CI)
              PointGroup = SgGroups.PG_6_mmm;
            else if (deterRotMx(ListSeitzMx[Generator_iList[0]].s.R) == -1) {
              if (deterRotMx(ListSeitzMx[Generator_iList[1]].s.R) == FlagPG)
                PointGroup = SgGroups.PG_6b2m;
              else
                PointGroup = SgGroups.PG_6bm2;
            } else if (deterRotMx(ListSeitzMx[Generator_iList[1]].s.R) == -1)
              PointGroup = SgGroups.PG_6mm;
            else
              PointGroup = SgGroups.PG_622;
          }
        }

        if (iList_1 >= 0) Generator_iList[nG++] = iList_1;

        nGenerator = nG;
        return 0;

      case XS_Cubic:
        FlagPG = 0;

        iList = FindSeitzMx(4, HonorSign, 'z', '=');
        if (iList < 0) {
          iList = FindSeitzMx(2, HonorSign, 'z', '=');
          FlagPG = 1;
        }
        if (iList < 0) break;
        Generator_iList[nG++] = iList;

        iList = FindSeitzMx(2, HonorSign, 'x', '=');
        if (iList < 0) break;
        Generator_iList[nG++] = iList;

        iList = FindSeitzMx(3, HonorSign, 'o', '*');
        if (iList < 0) break;
        Generator_iList[nG++] = iList;

        if (FlagPG != 0) {
          if (SgInfo_CI)
            PointGroup = SgGroups.PG_m3b;
          else
            PointGroup = SgGroups.PG_23;
        } else {
          if (SgInfo_CI)
            PointGroup = SgGroups.PG_m3bm;
          else if (deterRotMx(ListSeitzMx[Generator_iList[0]].s.R) == -1)
            PointGroup = SgGroups.PG_4b3m;
          else
            PointGroup = SgGroups.PG_432;
        }

        if (iList_1 >= 0) Generator_iList[nG++] = iList_1;

        nGenerator = nG;
        return 0;

      default:
        break;
    }

    return -1;
  }


  class cGRT {
    T_RotMxInfo RMxI_Buf = new T_RotMxInfo();
    T_RotMxInfo RMxI = null;
    int[] Transl = new int[3];

    public cGRT() {
    }
  }

/*  public char[] getHallSymbolAsChar() {
    char[] tmpchar = new char[hallSymbol.length()];
    hallSymbol.getChars(0, hallSymbol.length(), tmpchar, 0);
    return tmpchar;
  }*/

  public String getHallSymbolAsString() {
    return hallSymbol;
  }

  public int BuildHSym() {
    int NeedDash, HaveOrSh, nGRT, iList, iG, ip, os, i;
    int AbsOrder, ScrewPrimitive, Screw;
    int PreviousRotation;
    char RefAxis, DirCode, PreviousRefAxis, PreviousDirCode;
    int nTrV, iTrV;
    int[] OrSh = new int[3], RO = new int[3], Transl = new int[3];
    int[] TrV, ht;
    T_RTMx SMx_1 = new T_RTMx();
    T_RTMx SMx;
    T_RotMxInfo rmxi;

    int ihsym = 0;

    cGRT[] GRT = new cGRT[Generator_iList.length];
    for (i = 0; i < Generator_iList.length; i++)
      GRT[i] = new cGRT();

    char[] Digits = {'0', '1', '2', '3', '4', '5', '6'};


    if (nGenerator == 0) {
      SetSgError("Internal Error: BuildHSym(): Empty generator list");
      return -1;
    }

    HaveOrSh = 0;

    for (i = 0; i < 3; i++) {
      OrSh[i] = OriginShift[i] * (STBF / 12);
      if (OrSh[i] != 0) HaveOrSh = 1;
    }

    NeedDash = 0;
    nGRT = 0;

    for (iG = 0; iG < nGenerator; iG++) {
      iList = Generator_iList[iG];

      GRT[nGRT].RMxI = ListOrBufRotMxInfo(iList, GRT[nGRT].RMxI_Buf);
      if (GRT[nGRT].RMxI == null)
        return -1;

      SMx = ListSeitzMx[iList];

      RotMx_t_Vector(RO, SMx.s.R, OrSh, STBF);

      for (i = 0; i < 3; i++)
        GRT[nGRT].Transl[i] = iModPositive(SMx.s.T[i] + RO[i] - OrSh[i], STBF);

      if (GRT[nGRT].RMxI.Order == -1) {
        for (i = 0; i < 3; i++)
          if (GRT[nGRT].Transl[i] != 0) break;

        if (i == 3)
          NeedDash = 1;
        else
          nGRT++;
      } else
        nGRT++;
    }

    if (Centric != 0) {
      if (HaveOrSh == 0)
        NeedDash = 1;
      else {
        for (iG = 0; iG < nGRT; iG++)
          if (GRT[iG].RMxI.Order == 1) break;

        InitSeitzMx(SMx_1, -1);

        if (GetRotMxInfo(SMx_1.s.R, GRT[iG].RMxI_Buf, null) != -1) {
          SetSgError("Internal Error: BuildHSym(): Corrupt GetRotMxInfo()");
          return -1;
        }

        GRT[iG].RMxI = GRT[iG].RMxI_Buf;

        for (i = 0; i < 3; i++)
          GRT[iG].Transl[i] = iModPositive(-2 * OrSh[i], STBF);

        if (iG == nGRT)
          nGRT++;
      }
    }

    hallSymbol = ""; //getHallSymbolAsChar();

    PreviousRotation = 0;
    PreviousRefAxis = 0;
    PreviousDirCode = 0;

    ihsym = 0;
    if (NeedDash != 0)
      hallSymbol += '-';
    else
      hallSymbol += ' ';

    hallSymbol += LatticeInfo.getCode();

    nTrV = LatticeInfo.nTrVector;

    for (iG = 0; iG < nGRT; iG++) {
      rmxi = GRT[iG].RMxI;

      AbsOrder = Math.abs(rmxi.Order);
      RefAxis = rmxi.getRefAxis();
      DirCode = rmxi.getDirCode();
      if (RefAxis == 'o') RefAxis = 0;
      if (DirCode == '=' || DirCode == '.') DirCode = 0;

      if (iG == 0) {
        if (RefAxis == 'z') RefAxis = 0;
      } else {
        if (AbsOrder == 2) {
          if (PreviousRotation == 2 || PreviousRotation == 4) {
            if (RefAxis == 'x') RefAxis = 0;
          } else if (PreviousRotation == 3 || PreviousRotation == 6) {
            if (PreviousDirCode == '*'
                    || RefAxis == PreviousRefAxis)
              RefAxis = 0;
            if (DirCode == '\'') DirCode = 0;
          }
        } else if (AbsOrder == 3) {
          if (DirCode == '*') DirCode = 0;
        }
      }

      PreviousRotation = AbsOrder;
      PreviousRefAxis = rmxi.getRefAxis();
      PreviousDirCode = rmxi.getDirCode();

      hallSymbol += ' ';
      if (rmxi.Order < 0) hallSymbol += '-';
      hallSymbol += Digits[AbsOrder];
      if (RefAxis != 0) hallSymbol += RefAxis;
      if (DirCode != 0) hallSymbol += DirCode;

      TrV = LatticeInfo.TrVector;
      int indexTrV = 0;

      for (iTrV = 0; iTrV < nTrV; iTrV++, indexTrV += 3) {
        for (i = 0; i < 3; i++)
          if ((GRT[iG].Transl[i] + TrV[indexTrV + i]) % STBF != 0)
            break;

        if (i == 3)
          break;
      }

      if (iTrV < nTrV)
        continue; /* next iG */

      int ihsym_mark = ihsym;

      indexTrV = 0;

      for (iTrV = 0; iTrV < nTrV; iTrV++, indexTrV += 3, ihsym = ihsym_mark) {
        for (i = 0; i < 3; i++)
          Transl[i] = iModPositive(GRT[iG].Transl[i] + TrV[indexTrV + i], STBF);

        Screw = 0;

        for (ip = 0; ip < 3; ip++)
          if (rmxi.EigenVector[ip] != 0) break;

        if (ip < 3 && rmxi.EigenVector[ip] == 1) {
          for (i = ip + 1; i < 3; i++)
            if (rmxi.EigenVector[i] != 0) break;

          if (i == 3) {
            ScrewPrimitive = STBF / AbsOrder;
            Screw = Transl[ip] / ScrewPrimitive;
            i = Screw * ScrewPrimitive;
            if (i % 3 != 0) {
              hallSymbol += Digits[Screw];
              Transl[ip] -= i;
            }
          }
        }

        ht = HallTranslations;
        int iht = 0;

        while (ht[iht] != 0) {
          for (i = 0; i < 3; i++)
            if (Transl[i] < ht[iht + i + 1]) break;

          if (i == 3) {
            for (i = 0; i < 3; i++)
              Transl[i] -= ht[iht + i + 1];

            hallSymbol += (char) ht[iht]; //Character.forDigit(ht[iht], Character.MAX_RADIX);
          }

          iht += 4;
        }

        for (i = 0; i < 3; i++)
          if (Transl[i] != 0)
            break;

        if (i == 3)
          break;
      }

      if (iTrV == nTrV) {
        return 0;
      }
    }

    if (nGRT == 0) {
      hallSymbol += ' ';
      hallSymbol += '1';
    }

    if (HaveOrSh != 0) {
      hallSymbol += ' ';
      hallSymbol += '(';

      for (i = 0; i < 3; i++) {
        if (i != 0) hallSymbol += ' ';

        os = iModPositive(OriginShift[i], 12);
        if (os > 6) {
          hallSymbol += '-';
          os = 12 - os;
        }

        hallSymbol += Digits[os];
      }

      hallSymbol += ')';
    }

    return 1;
  }


  static int ShiftTable[] = {0, 1, -1, 2, -2, 3};

  public int BuildHallSymbol(int FixedOriginShift) {
    int ix, iy, iz;
    int status;


    if (SgError != null) return -1;

    if (nGenerator == 0) {
      if (BuildGenerator_iList() != 0) {
        SetSgError("Error: Can't build generator list");
        return -1;
      }
    }

    if (FixedOriginShift != 0) {
      status = BuildHSym();
      if (status == 1)
        return 0;
    } else {
      for (ix = 0; ix < 6; ix++) {
        OriginShift[0] = ShiftTable[ix];

        for (iy = 0; iy < 6; iy++) {
          OriginShift[1] = ShiftTable[iy];

          for (iz = 0; iz < 6; iz++) {
            OriginShift[2] = ShiftTable[iz];

            status = BuildHSym();
            if (status < 0)
              return -1;

            if (status == 1)
              return 0;
          }
        }
      }
    }

    SetSgError("Error: Can't build Hall Symbol");
    return -1;
  }


  public int SetChiralFlag() {
    int iList;
    T_RotMxInfo lrmxi;
    T_RotMxInfo RotMxInfo = new T_RotMxInfo();


    if (Centric != 0 || InversionOffOrigin != 0) {
      Chiral = 0;
      return 0;
    }

    for (iList = 1; iList < nList; iList++) /* skip first          */ {                                               /*   = identity matrix */
      lrmxi = ListOrBufRotMxInfo(iList, RotMxInfo);
      if (lrmxi == null)
        return -1;

      if (lrmxi.Order < 0) {
        Chiral = 0;
        return 0;
      }
    }

    Chiral = 1;

    return 0;
  }


  /**
   * Creates a JtSgInfo instance.
   */

  public T_SgInfo() {
    super();
    int i;


    GenOption = 0;
    Centric = 0;
    InversionOffOrigin = 0;
    LatticeInfo = LI_P;
    StatusLatticeTr = 0;
    for (i = 0; i < 3; i++)
      OriginShift[i] = 0;
    nList = 0;

    OrderL = 0;
    OrderP = 0;
    XtalSystem = XS_Unknown;
    UniqueRefAxis = 0;
    UniqueDirCode = 0;
    HexMetric = -1;
    Chiral = -1;
    ExtraInfo = EI_Unknown;
    PointGroup = SgGroups.PG_Unknown;
    nGenerator = 0;
    hallSymbol = "";
    sTabSgName = null;
    CCMx_LP = null;
//  	n_ssVM = -1;
  }

  /**
   * Creates a JtSgInfo instance.
   */

  public T_SgInfo(String SgName, char F_Convention, int maxlist, T_TabSgName tsgn) {
    this();

    MaxList = maxlist;
    ListSeitzMx = new T_RTMx[MaxList];
    for (int i = 0; i < MaxList; i++)
      ListSeitzMx[i] = new T_RTMx();

    ListRotMxInfo = new T_RotMxInfo[MaxList];
    for (int i = 0; i < MaxList; i++)
      ListRotMxInfo[i] = new T_RotMxInfo();

    int isgname = 0;
    while (SgName.charAt(isgname) == ' ' || SgName.charAt(isgname) == '\t') isgname++;

    if (F_Convention == 'H' && Character.isDigit(SgName.charAt(isgname)))
      F_Convention = 'A';

    if (F_Convention == 'A' || F_Convention == 'I') {
      tsgn = Sgio.FindTabSgNameEntry(SgName, F_Convention);
      if (tsgn != null) {
        SgName = tsgn.HallSymbol;
//      	System.out.println(SgName);
      } else
        System.err.println("No Space Group found for the name");

    }
    sTabSgName = tsgn;
    if (tsgn != null) {
      GenOption = 1;
      ParseHallSymbol(SgName);
    }
  }

  public T_SgInfo(String SgName, char F_Convention) {
    this(SgName, F_Convention, 192, null);

    CompleteSgInfo();
  }

  public int CompleteSgInfo() {
    int[] List_iList = new int[192];
    T_TabSgName tsgn;

    if (StatusLatticeTr == -1) {
      if (AddLatticeTr2ListSeitzMx(LatticeInfo) < 0)
        return -1;
    }

    if (ApplyOriginShift() < 0)
      return -1;

    if (nList > List_iList.length) {
      SetSgError("Internal Error: CompleteSgInfo()");
      return -1;
    }

    if (nList > 1) {
      SortSgInfoList(List_iList);
      if (SgError != null) return -1;
    }

    if (RemoveLatticeTr() != 0)
      return -1;

    if (RemoveInversion() != 0)
      return -1;

    TidyTranslation();

    if (nList > 1) {
      SortSgInfoList(List_iList);
      if (SgError != null) return -1;
    }
    OrderP = nList;
    if (Centric == -1) OrderP *= 2;

    OrderL = OrderP * LatticeInfo.nTrVector;

    if (BuildHallSymbol(0) != 0)
      return -1;

    int itbgn = 0;
    int reach = -1;
    for (itbgn = 0; itbgn < SgGroups.TabSgName.length; itbgn++) {
      if (SgGroups.TabSgName[itbgn].HallSymbol.equals(hallSymbol)
              && (sTabSgName == null
              || SgGroups.TabSgName[itbgn].HallSymbol.equals(sTabSgName.HallSymbol)))
        break;
      if (sTabSgName != null && SgGroups.TabSgName[itbgn].HallSymbol.equals(sTabSgName.HallSymbol))
        reach = itbgn;

    }

    if (sTabSgName != null && itbgn < SgGroups.TabSgName.length) {
//    	if (SgError != null) return -1;
      if (debug)
        if (reach >= 0)
          System.err.println("Problem: HallSymbol mismatch " +
                  hallSymbol + " != " +
                  sTabSgName.HallSymbol + " != " + SgGroups.TabSgName[reach].HallSymbol);
        else
          System.err.println("Internal Error: Input/Output HallSymbol mismatch for " +
                  hallSymbol);

//    	SetSgError("Internal Error: Input/Output HallSymbol mismatch " +
//    												SgGroups.TabSgName[itbgn].HallSymbol);
//    	return -1;
    }

    if (itbgn < SgGroups.TabSgName.length)
      sTabSgName = SgGroups.TabSgName[itbgn];

    CCMx_LP = null;

    switch (LatticeInfo.getCode()) {
      case 'P':
        CCMx_LP = CCMx_PP;
        break;
      case 'A':
        CCMx_LP = CCMx_AP;
        break;
      case 'B':
        CCMx_LP = CCMx_BP;
        break;
      case 'C':
        CCMx_LP = CCMx_CP;
        break;
      case 'I':
        CCMx_LP = CCMx_IP;
        break;
      case 'R':
        switch (UniqueRefAxis) {
          case 0:
          case 'z':
            CCMx_LP = CCMx_RP_z;
            break;
          case 'y':
            CCMx_LP = CCMx_RP_y;
            break;
          default:
            break;
        }
        break;
      case 'S':
        switch (UniqueRefAxis) {
          case 0:
          case 'y':
            CCMx_LP = CCMx_SP_y;
            break;
          case 'x':
            CCMx_LP = CCMx_SP_x;
            break;
          default:
            break;
        }
        break;
      case 'T':
        switch (UniqueRefAxis) {
          case 0:
          case 'x':
            CCMx_LP = CCMx_TP_x;
            break;
          case 'z':
            CCMx_LP = CCMx_TP_z;
            break;
          default:
            break;
        }
        break;
      case 'F':
        CCMx_LP = CCMx_FP;
        break;
      default:
        break;
    }

    if ((XtalSystem == XS_Trigonal && UniqueDirCode != '*')
            || XtalSystem == XS_Hexagonal)
      HexMetric = 1;
    else
      HexMetric = 0;

    if (SetChiralFlag() != 0)
      return -1;

    if (CCMx_LP == null) {
      SetSgError("Internal Error: Illegal lattice code");
      return -1;
    }

    return 0;
  }


  public int CB_SMx(T_RTMx CSiC,
                    T_RTMx CBMx, T_RTMx SMx, T_RTMx InvCBMx) {
    int i;
    T_RTMx BufMx = new T_RTMx();


    RTMxMultiply(BufMx, SMx, InvCBMx, CTBF / STBF, CTBF);
    RTMxMultiply(CSiC, CBMx, BufMx, CRBF, CRBF * CTBF);

    for (i = 0; i < 9; i++) {
      if (CSiC.s.R[i] % (CRBF * CRBF) != 0) {
        SetSgError("Internal Error: Corrupt CBMx/SMx/InvCBMx");
        return -1;
      }

      CSiC.s.R[i] /= (CRBF * CRBF);
    }

    for (i = 0; i < 3; i++) {
      if (CSiC.s.T[i] % (CRBF * (CTBF / STBF)) != 0) {
        SetSgError("Internal Error: Out of STBF range");
        return -1;
      }

      CSiC.s.T[i] /= (CRBF * (CTBF / STBF));
    }

    return 0;
  }

/*
int TransformSgInfo(const T_SgInfo *SgInfo,
                    const T_RTMx *CBMx, const T_RTMx *InvCBMx,
                    T_SgInfo *BC_SgInfo)
{
  int           iList, f, i;
  int           nTrV, iTrV, nLoopInv, iLoopInv;
  const int     *TrV;
  T_RTMx        SMx, BC_SMx;
  const T_RTMx  *lsmx;


  nLoopInv = Sg_nLoopInv(SgInfo);

  nTrV = LatticeInfo.nTrVector;
   TrV = LatticeInfo.TrVector;

  for (iTrV = 0; iTrV < nTrV; iTrV++, TrV += 3)
  {
    for (iLoopInv = 0; iLoopInv < nLoopInv; iLoopInv++)
    {
      if (iLoopInv == 0) f =  1;
      else               f = -1;

      lsmx = ListSeitzMx;

      for (iList = 0; iList < nList; iList++, lsmx++)
      {
        for (i = 0; i < 9; i++)
          SMx.s.R[i] = f * lsmx.s.R[i];

        for (i = 0; i < 3; i++)
          SMx.s.T[i] = f * lsmx.s.T[i] + TrV[i];

        if (CB_SMx(&BC_SMx, CBMx, &SMx, InvCBMx) != 0)
          return -1;

        if (Add2ListSeitzMx(BC_SgInfo, &BC_SMx) < 0)
          return -1;
      }
    }
  }

  return 0;
}*/

// #undef SGCLIB_C__


  public int ParseHallSymbol(String hsym) {
    int c, i, pos_hsym;
    int[] ht;
    int lCentric;
    T_LatticeInfo lLatticeInfo = null;
    int FieldType, PreviousFT;
    int iOriginShift, SignOriginShift;
    int digit, rotation, refaxis, dircode;
    int translation = -1;
    int PreviousRotation, PreviousRefAxis;
    int nHG, ClearHG;
    T_HallGenerator HG = new T_HallGenerator();

    final int FT_Delimiter = 0;
    final int FT_Improper = 1;
    final int FT_Digit = 2;
    final int FT_Rotation = 3;
    final int FT_RefAxis = 4;
    final int FT_DirCode = 5;
    final int FT_Translation = 6;
    final int FT_OriginShift = 7;

    String Err_Ill_ori_shi_val =
            "Error: Illegal origin shift value";

    String Err_Too_ori_shi_val =
            "Error: Too much origin shift values";


    lCentric = 0;
    lLatticeInfo = null;

    HG.Rotation = HG.RefAxis = HG.DirCode = HG.Screw = 0;

    nHG = 0;
    ClearHG = 1;
    FieldType = FT_Delimiter;
    PreviousRotation = 0;
    PreviousRefAxis = 0;
    iOriginShift = 0;
    SignOriginShift = 0;

    pos_hsym = 0;
    int ihsym = 0;

    do {
//    System.out.println(Integer.toXRDcatString(ihsym) + " " + hsym);
      if (hsym.length() <= ihsym ||
              hsym.charAt(ihsym) == '_' || hsym.charAt(ihsym) == '.' ||
              hsym.charAt(ihsym) == '\t')
        c = ' ';
      else
        c = hsym.charAt(ihsym);

      pos_hsym++;

      if (lLatticeInfo == null) {
        if (lCentric == 0 && c == '-') {
          if (AddInversion2ListSeitzMx() < 0)
            return pos_hsym;
          lCentric = 1;
        } else if (c != ' ') {
          c = Character.toUpperCase((char) c);

          switch (c) {
            case 'P':
              lLatticeInfo = LI_P;
              break;
            case 'A':
              lLatticeInfo = LI_A;
              break;
            case 'B':
              lLatticeInfo = LI_B;
              break;
            case 'C':
              lLatticeInfo = LI_C;
              break;
            case 'I':
              lLatticeInfo = LI_I;
              break;
            case 'R':
              lLatticeInfo = LI_R;
              break;
            case 'S':
              lLatticeInfo = LI_S;
              break;
            case 'T':
              lLatticeInfo = LI_T;
              break;
            case 'F':
              lLatticeInfo = LI_F;
              break;
            default:
              SetSgError("Error: Illegal lattice code");
              return pos_hsym;
          }

          if (AddLatticeTr2ListSeitzMx(lLatticeInfo) < 0)
            return pos_hsym;
        }
      } else if (FieldType != FT_OriginShift) {
        c = Character.toLowerCase((char) c);
        if (c == 'q')
          c = '\'';
        else if (c == '+') c = '"';

        PreviousFT = FieldType;
        digit = rotation = refaxis = dircode = 0;
        translation = -1;

        ht = HallTranslations;
        int iht = 0;
        while (iht < ht.length) {
          if (c == ht[iht]) {
            translation = iht;
            FieldType = FT_Translation;
            break;
          }
          iht += 4;
        }

        if (translation == -1) {
          switch (c) {
            case ' ':
              FieldType = FT_Delimiter;
              break;

            case '-':
              FieldType = FT_Improper;
              break;

            case '1':
              digit = 1;
              FieldType = FT_Digit;
              break;
            case '2':
              digit = 2;
              FieldType = FT_Digit;
              break;
            case '3':
              digit = 3;
              FieldType = FT_Digit;
              break;
            case '4':
              digit = 4;
              FieldType = FT_Digit;
              break;
            case '5':
              digit = 5;
              FieldType = FT_Digit;
              break;
            case '6':
              digit = 6;
              FieldType = FT_Digit;
              break;

            case 'x':
            case 'y':
            case 'z':
              refaxis = c;
              FieldType = FT_RefAxis;
              break;

            case '"':
            case '\'':
            case '*':
              dircode = c;
              FieldType = FT_DirCode;
              break;

            case '(':
              FieldType = FT_OriginShift;
              break;

            default:
              SetSgError("Error: Illegal character in Hall symbol");
              return pos_hsym;
          }

          if (FieldType == FT_Digit) {
            if (ClearHG == 0
                    && HG.Rotation > digit
                    && HG.Screw == 0
                    && HG.DirCode == 0) {
              HG.Screw = digit;
              FieldType = FT_Translation;
            } else if (digit == 5) {
              SetSgError("Error: Illegal 5-fold rotation");
              return pos_hsym;
            } else {
              rotation = digit;
              FieldType = FT_Rotation;
            }
          }
        }

        if (ClearHG == 0
                && (FieldType == FT_Delimiter
                || FieldType == FT_OriginShift
                || FieldType < PreviousFT
                || (FieldType == PreviousFT && FieldType != FT_Translation))
                && !(FieldType == FT_RefAxis && HG.RefAxis == 0
                && PreviousFT == FT_DirCode)) {
          if (HG.RefAxis == 0) {
            if (nHG == 0)
              HG.RefAxis = 'z';
            else {
              if (HG.Rotation == 2) {
                if (PreviousRotation == 2 || PreviousRotation == 4)
                  HG.RefAxis = 'x';
                else if (PreviousRotation == 3 || PreviousRotation == 6) {
                  HG.RefAxis = PreviousRefAxis;
                  if (HG.DirCode == 0) HG.DirCode = '\'';
                }
              } else if (HG.Rotation == 3) {
                if (HG.DirCode == 0) HG.DirCode = '*';
              }
            }
          }

          PreviousRefAxis = HG.RefAxis;
          PreviousRotation = HG.Rotation;

          if (Sgio.LookupRotMx(HG) == 0) {
            SetSgError("Error: Illegal generator or need explicit axis symbol");
            return pos_hsym - 1;
          }

          if (HG.Screw != 0) {
            switch (HG.RefAxis) {
              case 'x':
                i = 0;
                break;
              case 'y':
                i = 1;
                break;
              case 'z':
                i = 2;
                break;
              default:
                i = -1;
                break;
            }

            if (HG.DirCode != 0 || i < 0) {
              SetSgError("Error: Screw for non-principal direction");
              return pos_hsym - 1;
            }

            HG.SeitzMx.s.T[i] += STBF * HG.Screw / HG.Rotation;
          }

          for (i = 0; i < 3; i++)
            HG.SeitzMx.s.T[i] %= STBF;

          if (Add2ListSeitzMx(HG.SeitzMx) < 0)
            return pos_hsym - 1;

          if (StatusLatticeTr == -1) {
            if (AddLatticeTr2ListSeitzMx(LatticeInfo) < 0)
              return pos_hsym - 1;
          }

          nHG++;
          ClearHG = 1;
        }

        if (FieldType != FT_Delimiter && FieldType != FT_OriginShift) {
          if (ClearHG != 0) {
            HG.Improper = 0;
            HG.Rotation = 1;
            HG.RefAxis = 0;
            HG.DirCode = 0;
            HG.Screw = 0;
            for (i = 0; i < 9; i++) HG.SeitzMx.s.R[i] = 0;
            for (i = 0; i < 3; i++) HG.SeitzMx.s.T[i] = 0;
            ClearHG = 0;
          }

          switch (FieldType) {
            case FT_Improper:
              HG.Improper = 1;
              break;
            case FT_Rotation:
              HG.Rotation = rotation;
              break;
            case FT_RefAxis:
              HG.RefAxis = refaxis;
              break;
            case FT_DirCode:
              HG.DirCode = dircode;
              break;
            case FT_Translation:
              if (translation != -1) {
                for (i = 0; i < 3; i++)
                  HG.SeitzMx.s.T[i] += ht[++translation];
              }
              break;
          }
        }
      } else {
        if (iOriginShift > 3) {
          SetSgError(Err_Too_ori_shi_val);
          return pos_hsym;
        }

        if (hsym.length() <= ihsym) c = ')';

        digit = -1;

        switch (c) {
          case ' ':
            break;

          case ')':
            if (iOriginShift != 3) {
              SetSgError("Error: Missing origin shift values");
              return pos_hsym;
            }
            iOriginShift++;
            FieldType = FT_Delimiter;
            break;

          case '-':
            if (SignOriginShift != 0) {
              SetSgError(Err_Ill_ori_shi_val);
              return pos_hsym;
            }
            SignOriginShift = 1;
            break;

          case '0':
            digit = 0;
            break;
          case '1':
            digit = 1;
            break;
          case '2':
            digit = 2;
            break;
          case '3':
            digit = 3;
            break;
          case '4':
            digit = 4;
            break;
          case '5':
            digit = 5;
            break;
          case '6':
            digit = 6;
            break;

          default:
            SetSgError(Err_Ill_ori_shi_val);
            return pos_hsym;
        }

        if (digit >= 0) {
          if (iOriginShift >= 3) {
            SetSgError(Err_Too_ori_shi_val);
            return pos_hsym;
          }
          if (SignOriginShift != 0) digit *= -1;
          SignOriginShift = 0;
          OriginShift[iOriginShift++] = digit;
        }
      }
    } while (ihsym++ <= hsym.length());

    if (lLatticeInfo == null) {
      SetSgError("Error: Lattice type not specified");
      return pos_hsym;
    }

    return pos_hsym;
  }

}
