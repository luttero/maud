/*
 * @(#)Sghkl.java	0.10 created 10/17/1999 Pergine Vals.
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

import java.awt.*;
import java.lang.*;

/**
 * The Sghkl performs hkl computation.
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:02 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Sghkl {

  T_SgInfo SgInfo = null;
  static String IErr_Inc_SymMx =
          new String("Internal Error: Inconsistent symmetry matrices");


  public Sghkl(T_SgInfo SgInfo) {
    super();
    this.SgInfo = SgInfo;
  }


  public int IsSysAbsent_hkl(int h, int k, int l, int[] TH_Restriction) {
    int iTrV, nTrV;
    int[] TrV;
    int iList, hm, km, lm;
    int TH, THr;
    boolean FlagMismatch;
    T_RTMx[] lsmx;


/* check list of symmetry operations
     take care of lattice type and "centric" flag */

    int iTHr = 0;
    THr = -1;
    if (TH_Restriction != null) TH_Restriction[iTHr] = THr;
    FlagMismatch = false;

    nTrV = SgInfo.LatticeInfo.nTrVector;
    lsmx = SgInfo.ListSeitzMx;

    int ilsmx = 0;
    int indexTrV = 0;
    for (iList = 0; iList < SgInfo.nList; iList++, ilsmx++) {
      hm = lsmx[ilsmx].s.R[0] * h + lsmx[ilsmx].s.R[3] * k + lsmx[ilsmx].s.R[6] * l;
      km = lsmx[ilsmx].s.R[1] * h + lsmx[ilsmx].s.R[4] * k + lsmx[ilsmx].s.R[7] * l;
      lm = lsmx[ilsmx].s.R[2] * h + lsmx[ilsmx].s.R[5] * k + lsmx[ilsmx].s.R[8] * l;

      TrV = SgInfo.LatticeInfo.TrVector;

      indexTrV = 0;
      for (iTrV = 0; iTrV < nTrV; iTrV++) {
        TH = (lsmx[ilsmx].s.T[0] + TrV[indexTrV++]) * h;
        TH += (lsmx[ilsmx].s.T[1] + TrV[indexTrV++]) * k;
        TH += (lsmx[ilsmx].s.T[2] + TrV[indexTrV++]) * l;
        TH %= T_SgInfo.STBF;
        if (TH < 0) TH += T_SgInfo.STBF;

        if (-h == hm && -k == km && -l == lm) {
          if (TH != 0 && SgInfo.Centric == -1)
            return -(iList + 1 + iTrV * SgInfo.nList);

          if (THr < 0)
            THr = TH;
          else if (THr != TH)
            FlagMismatch = true; /* must be systematic absent */
          /* will check later ...      */
        } else if (h == hm && k == km && l == lm) {
          if (TH != 0)
            return (iList + 1 + iTrV * SgInfo.nList);
        } else
          break;
      }
    }

    if (THr >= 0 && FlagMismatch) /* ... consistency check */
      T_SgInfo.SetSgError(IErr_Inc_SymMx);

    if (TH_Restriction != null) {
      if (SgInfo.Centric == -1)
        TH_Restriction[iTHr] = 0;
      else
        TH_Restriction[iTHr] = THr;
    }

    return 0;
  }


  public int BuildEq_hkl(int FriedelSym,
                         T_Eq_hkl Eq_hkl, int h, int k, int l) {
    int iList, hm, km, lm, i;
    T_RTMx[] lsmx;
    T_Eq_hkl BufEq_hkl = new T_Eq_hkl();


    if (Eq_hkl == null)
      Eq_hkl = BufEq_hkl;

    boolean centric = SgInfo.Centric == -1
            || SgInfo.InversionOffOrigin != 0
            || FriedelSym != 0;

    if (centric)
      Eq_hkl.Centric = -1;
    else
      Eq_hkl.Centric = 0;

    Eq_hkl.M = 0;
    Eq_hkl.N = 0;

    lsmx = SgInfo.ListSeitzMx;
    int ilsmx = 0;

    for (iList = 0; iList < SgInfo.nList; iList++, ilsmx++) {
      hm = lsmx[ilsmx].s.R[0] * h + lsmx[ilsmx].s.R[3] * k + lsmx[ilsmx].s.R[6] * l;
      km = lsmx[ilsmx].s.R[1] * h + lsmx[ilsmx].s.R[4] * k + lsmx[ilsmx].s.R[7] * l;
      lm = lsmx[ilsmx].s.R[2] * h + lsmx[ilsmx].s.R[5] * k + lsmx[ilsmx].s.R[8] * l;

      for (i = 0; i < Eq_hkl.N; i++) {
        if (hm == Eq_hkl.h[i] && km == Eq_hkl.k[i] && lm == Eq_hkl.l[i])
          break;
        if (Eq_hkl.Centric == 0)
          continue;
        if (-hm == Eq_hkl.h[i] && -km == Eq_hkl.k[i] && -lm == Eq_hkl.l[i])
          break;
      }

      if (i == Eq_hkl.N) {
        if (Eq_hkl.N >= Eq_hkl.h.length) {
          T_SgInfo.SetSgError(IErr_Inc_SymMx);
          return 0;
        }

        Eq_hkl.h[i] = hm;
        Eq_hkl.k[i] = km;
        Eq_hkl.l[i] = lm;

        Eq_hkl.TH[i] = (lsmx[ilsmx].s.T[0] * h
                + lsmx[ilsmx].s.T[1] * k
                + lsmx[ilsmx].s.T[2] * l) % T_SgInfo.STBF;
        if (Eq_hkl.TH[i] < 0)
          Eq_hkl.TH[i] += T_SgInfo.STBF;

        Eq_hkl.M++;
        if (Eq_hkl.Centric != 0 && (hm != 0 || km != 0 || lm != 0))
          Eq_hkl.M++;

        Eq_hkl.N++;
      }
    }

    if (SgInfo.nList % Eq_hkl.N != 0) /* another error trap */ {
      T_SgInfo.SetSgError(IErr_Inc_SymMx);
      return 0;
    }

    return Eq_hkl.M;
  }


  public int AreSymEquivalent_hkl(int FriedelSym,
                                  int h1, int k1, int l1,
                                  int h2, int k2, int l2) {
    boolean Centric;
    int iList, hm, km, lm;
    T_RTMx[] lsmx;


    Centric = SgInfo.Centric == -1
            || SgInfo.InversionOffOrigin != 0
            || FriedelSym != 0;

    lsmx = SgInfo.ListSeitzMx;
    int ilsmx = 0;

    for (iList = 0; iList < SgInfo.nList; iList++, ilsmx++) {
      hm = lsmx[ilsmx].s.R[0] * h1 + lsmx[ilsmx].s.R[3] * k1 + lsmx[ilsmx].s.R[6] * l1;
      km = lsmx[ilsmx].s.R[1] * h1 + lsmx[ilsmx].s.R[4] * k1 + lsmx[ilsmx].s.R[7] * l1;
      lm = lsmx[ilsmx].s.R[2] * h1 + lsmx[ilsmx].s.R[5] * k1 + lsmx[ilsmx].s.R[8] * l1;

      if (h2 == hm && k2 == km && l2 == lm)
        return (iList + 1);
      if (!Centric)
        continue;
      if (-h2 == hm && -k2 == km && -l2 == lm)
        return -(iList + 1);
    }

    return 0;
  }


  public int SetListMin_hkl(int FriedelSym,
                            int Maxh, int Maxk, int Maxl,
                            int[] Minh, int[] Mink, int[] Minl) {
    boolean Centric;
    int i, n, u;

    char[] xyz = {'x', 'y', 'z'};


    Centric = SgInfo.Centric == -1
            || SgInfo.InversionOffOrigin != 0
            || FriedelSym != 0;

    Minh[0] = -Maxh;
    Mink[0] = -Maxk;
    Minl[0] = -Maxl;

    for (i = 0; i < 3; i++) {
      if (SgInfo.FindSeitzMx(3, 0, xyz[i], '=') >= 0) {
        if (Centric) Minl[0] = 0;
        return 0;
      } else if (SgInfo.SgError != null)
        return -1;
    }

    n = 0;
    u = 0;

    for (i = 0; i < 3; i++)
      if (SgInfo.FindSeitzMx(2, 0, xyz[i], '=') >= 0) {
        n++;
        u = i;
      } else if (SgInfo.SgError != null)
        return -1;

    if (n == 2) {
      T_SgInfo.SetSgError(IErr_Inc_SymMx);
      return -1;
    }

    if (n == 0) {
      if (FriedelSym != 0)
        Minl[0] = 0;

      return 0;
    }

    if (n == 3) {
      if (Centric) {
        Minh[0] = 0;
        Mink[0] = 0;
        Minl[0] = 0;

        return 0;
      }

      for (i = 0; i < 3; i++)
        if (SgInfo.FindSeitzMx(2, 1, xyz[i], '=') >= 0)
          break;
        else if (SgInfo.SgError != null)
          return -1;

      if (i == 3) {
        T_SgInfo.SetSgError(IErr_Inc_SymMx);
        return -1;
      }

      if (i == 0) {
        Mink[0] = 0;
        Minl[0] = 0;
      } else if (i == 1) {
        Minh[0] = 0;
        Minl[0] = 0;
      } else {
        Minh[0] = 0;
        Mink[0] = 0;
      }

      return 0;
    }

    if (Centric) {
      if (u == 0)
        Minh[0] = 0;
      else
        Mink[0] = 0;

      Minl[0] = 0;

      return 0;
    }

    if (SgInfo.FindSeitzMx(2, 1, xyz[u], '=') >= 0) {
      if (u == 2)
        Mink[0] = 0;
      else
        Minl[0] = 0;

      return 0;
    } else if (SgInfo.SgError != null) {
      T_SgInfo.SetSgError(IErr_Inc_SymMx);
      return -1;
    }

    if (u == 0)
      Minh[0] = 0;
    else if (u == 1)
      Mink[0] = 0;
    else
      Minl[0] = 0;

    return 0;
  }


  public static int CmpEq_hkl(int h1, int k1, int l1,
                              int h2, int k2, int l2) {
    if (l1 >= 0 && l2 < 0) return -1;
    if (l1 < 0 && l2 >= 0) return 1;

    if (k1 >= 0 && k2 < 0) return -1;
    if (k1 < 0 && k2 >= 0) return 1;

    if (h1 >= 0 && h2 < 0) return -1;
    if (h1 < 0 && h2 >= 0) return 1;

    if (Math.abs(l1) < Math.abs(l2)) return -1;
    if (Math.abs(l1) > Math.abs(l2)) return 1;

    if (Math.abs(k1) < Math.abs(k2)) return -1;
    if (Math.abs(k1) > Math.abs(k2)) return 1;

    if (Math.abs(h1) < Math.abs(h2)) return -1;
    if (Math.abs(h1) > Math.abs(h2)) return 1;

    return 0;
  }


  public int IsHidden_hkl(int FriedelSym,
                          int Minh, int Mink, int Minl,
                          int Maxh, int Maxk, int Maxl,
                          int h, int k, int l) {
    int iList, hm, km, lm;
    boolean Mate;
    T_RTMx[] lsmx;


    Mate = false;

    if (FriedelSym != 0 || SgInfo.Centric == -1)
      Mate = true;

    lsmx = SgInfo.ListSeitzMx;
    int ilsmx = 0;

    for (iList = 0; iList < SgInfo.nList; iList++, ilsmx++) {
      hm = lsmx[ilsmx].s.R[0] * h + lsmx[ilsmx].s.R[3] * k + lsmx[ilsmx].s.R[6] * l;
      km = lsmx[ilsmx].s.R[1] * h + lsmx[ilsmx].s.R[4] * k + lsmx[ilsmx].s.R[7] * l;
      lm = lsmx[ilsmx].s.R[2] * h + lsmx[ilsmx].s.R[5] * k + lsmx[ilsmx].s.R[8] * l;

      if (iList != 0
              && (Minh <= hm && hm <= Maxh)
              && (Mink <= km && km <= Maxk)
              && (Minl <= lm && lm <= Maxl)
              && CmpEq_hkl(h, k, l, hm, km, lm) > 0)
        return (iList + 1);

      if (Mate
              && (Minh <= -hm && -hm <= Maxh)
              && (Mink <= -km && -km <= Maxk)
              && (Minl <= -lm && -lm <= Maxl)
              && CmpEq_hkl(h, k, l, -hm, -km, -lm) > 0)
        return -(iList + 1);
    }

    return 0;
  }


  public int Mult_hkl(int FriedelSym, int h, int k, int l) {
    int Centric;
    int R, M;
    int iList, hm, km, lm;
    T_RTMx[] lsmx;


    if (!(h != 0 || k != 0 || l != 0))
      return 1;

    boolean centric = SgInfo.Centric == -1
            || SgInfo.InversionOffOrigin != 0
            || FriedelSym != 0;

    if (centric)
      Centric = -1;
    else
      Centric = 0;

    R = 0;
    M = 0;

    lsmx = SgInfo.ListSeitzMx;
    int ilsmx = 0;

    for (iList = 0; iList < SgInfo.nList; iList++, ilsmx++) {
      hm = lsmx[ilsmx].s.R[0] * h + lsmx[ilsmx].s.R[3] * k + lsmx[ilsmx].s.R[6] * l;
      km = lsmx[ilsmx].s.R[1] * h + lsmx[ilsmx].s.R[4] * k + lsmx[ilsmx].s.R[7] * l;
      lm = lsmx[ilsmx].s.R[2] * h + lsmx[ilsmx].s.R[5] * k + lsmx[ilsmx].s.R[8] * l;

      if (hm == h && km == k && lm == l)
        M++;

      if (hm == -h && km == -k && lm == -l)
        R = 1;
    }

    if (M == 0 || SgInfo.nList % M != 0) {
      T_SgInfo.SetSgError(IErr_Inc_SymMx);
      return -1;
    }

    M = SgInfo.nList / M;

    if (Centric != 0 && R == 0)
      M *= 2;

    return M;
  }


  public static int RecHemisphere(int h, int k, int l) {
    if (l > 0) return 1;
    if (l == 0) {
      if (k > 0) return 1;
      if (k == 0) {
        if (h > 0) return 1;
        if (h == 0)
          return 0;
      }
    }

    return -1;
  }

}

