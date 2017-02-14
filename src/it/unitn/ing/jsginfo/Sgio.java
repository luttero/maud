/*
 * @(#)Sgio.java	0.10 created 10/17/1999 Pergine Vals.
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
import java.util.*;

/**
 * The Sgio performs IO operations on SgInfo.
 *
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:51 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Sgio {

//#define Fprintf (void) fprintf


  static String[] Ext_BT_or_UA =
          {
            /*  0 */ "abc",
                     /*  1 */ "ba-c",
                     /*  2 */ "cab",
                     /*  3 */ "-cba",
                     /*  4 */ "bca",
                     /*  5 */ "a-cb",
                     /*  6 */         "bac", /* 6 -> 1 */
                     /*  7 */         "cba", /* 7 -> 3 */
                     /*  8 */         "acb", /* 8 -> 5 */
                     /*  9 */ "-b", "b-", "bb", "bb", /* 10, 11, 12 ->  9 */
                     /* 13 */ "-c", "c-", "bc", "cb", /* 14, 15, 16 -> 13 */
                     /* 17 */ "-a", "a-", "ba", "ab", /* 18, 19, 20 -> 17 */
                     /* 21 */ "b",
                     /* 22 */ "c",
                     /* 23 */ "a",
                     null
          };

  public Sgio() {
    super();
  }

  public static int SkipWhite(String cp, int icp) {
    while (icp < cp.length() && (cp.charAt(icp) == '_' || Character.isWhitespace(cp.charAt(icp))))
      icp++;
    return icp;
  }


  static String IErr_Corrupt_TabSgName =
          "Internal Error: Corrupt TabSgName";


  public static int FindSchoenfliesSymbol(String SfSymbol) {
    int SgNumber;
    int s = 0, 	t = 0, smax, tmax;
    char tc = 0, sc = 0;


    for (SgNumber = 1; SgNumber <= 230; SgNumber++) {
      tmax = SgGroups.SchoenfliesSymbols[SgNumber].length();
      smax = SfSymbol.length();

      while (t < tmax && s < smax) {
        tc = SgGroups.SchoenfliesSymbols[SgNumber].toUpperCase().charAt(t);
        sc = SfSymbol.toUpperCase().charAt(s);

        if (tc != sc)
//          		&& (tc != '^' || Character.isLetter(sc) || Character.isDigit(sc)))
          break;

        t++;
        s++;
      }
      if (t == tmax)
        tc = '\ufffd';
      if (s == smax)
        sc = '\ufffd';
      if (tc == sc)
        return SgNumber;
    }

    return -1;
  }


  public static int SgLabelCmp(int SgNumber,
                               String SgLabel, String WtdLbl) {
    int sgl = 0, wl = 0;


/* first try: plain strcmp
   */
    sgl = 0;

    for (wl = 0; ; wl++, sgl++) {
      wl = SkipWhite(WtdLbl, wl);
      sgl = SkipWhite(SgLabel, sgl);

      if (sgl >= SgLabel.length() || SgLabel.charAt(sgl) == '=') {
        if (wl >= WtdLbl.length()) return 0;
        break;
      }

      if (sgl < SgLabel.length() && SgLabel.charAt(sgl) == '-') {
        if (wl >= WtdLbl.length() || (WtdLbl.charAt(wl) != '-' && WtdLbl.toUpperCase().charAt(wl) != 'B'))
          break;
      } else if ((sgl >= SgLabel.length() || wl >= WtdLbl.length()) || SgLabel.toUpperCase().charAt(sgl) != WtdLbl.toUpperCase().charAt(wl))
        break;

    }

/* second try: swap the dash (there should be only one)
   */
    sgl = 0;

    for (wl = 0; ; wl++) {
      wl = SkipWhite(WtdLbl, wl);
      sgl = SkipWhite(SgLabel, sgl);

      if (sgl < SgLabel.length() || SgLabel.charAt(sgl) == '-') {
        if (wl >= WtdLbl.length())
          break;
        if (WtdLbl.toUpperCase().charAt(1) != '-' && WtdLbl.toUpperCase().charAt(1) != 'B')
          break;
        if (SgLabel.toUpperCase().charAt(1) != WtdLbl.toUpperCase().charAt(wl))
          break;

        sgl++;
        wl++;
      } else {
        if (sgl >= SgLabel.length() || SgLabel.charAt(sgl) == '=') {
          if (wl >= WtdLbl.length()) return 0;
          break;
        }

        if (SgLabel.toUpperCase().charAt(sgl) != WtdLbl.toUpperCase().charAt(wl))
          break;
      }

      sgl++;
    }

    if (SgNumber >= 195)  /* cubic space groups only */ {
/* third try: ignore the "-3" dash
     */
      sgl = 0;

      for (wl = 0; ; wl++) {
        wl = SkipWhite(WtdLbl, wl);
        sgl = SkipWhite(SgLabel, sgl);

        if (sgl < SgLabel.length() &&
                (SgLabel.charAt(sgl) == '-' && SgLabel.charAt(1) == '3'))
          sgl++;

        if (sgl >= SgLabel.length() || SgLabel.charAt(sgl) == '=') {
          if (wl >= WtdLbl.length()) return 0;
          break;
        }

        if (SgLabel.toUpperCase().charAt(sgl) != WtdLbl.toUpperCase().charAt(wl))
          break;

        sgl++;
      }
    }

    return -1;
  }


  public static int ParseExtension(String Ext, T_ExtInfo ExtInfo) {
    int i, mode, iExt;


    ExtInfo.OriginChoice =
            ExtInfo.CellChoice =
            ExtInfo.BasisChoice = ' ';
    ExtInfo.BT_or_UA = "";

    if (Ext == null)
      return 0;

    mode = 0;

    iExt = 0;
    while (iExt < Ext.length()) {
      char cExt = Ext.charAt(iExt);
      if (cExt == '1' || cExt == '2') {
        ExtInfo.CellChoice =
                ExtInfo.OriginChoice = cExt;
        iExt++;
      } else if (cExt == '3') {
        ExtInfo.CellChoice = '3';
        iExt++;
      } else if (cExt == 'S' || cExt == 's') {
        ExtInfo.OriginChoice = '1';
        iExt++;
      } else if (cExt == 'Z' || cExt == 'z') {
        ExtInfo.OriginChoice = '2';
        iExt++;
      } else if (cExt == 'H' || cExt == 'h') {
        ExtInfo.BasisChoice = 'H';
        iExt++;
      } else if (cExt == 'R' || cExt == 'r') {
        ExtInfo.BasisChoice = 'R';
        iExt++;
      } else if (mode == 0)
        mode = 1;

      if (mode == 2)
        break;

      for (i = 0; Ext_BT_or_UA[i] != null; i++) {
        int ki = 0;
        while (ki + iExt < Ext.length() && ki < Ext_BT_or_UA[i].length() &&
                Ext.toUpperCase().charAt(ki + iExt) == Ext_BT_or_UA[i].toUpperCase().charAt(ki))
          ki++;

        if (ki == Ext_BT_or_UA[i].length()) {

          if (6 <= i && i <= 8)
            i = 2 * i - 11;
          else if (9 <= i && i <= 20)
            i = 9 + ((i - 9) / 4) * 4;

          ExtInfo.BT_or_UA = Ext_BT_or_UA[i];
          iExt += ki;
          break;
        }
      }

      if (mode == 0)
        break;

      mode = 2;
    }

    if (iExt < Ext.length())
      return -1;

    return 0;
  }


  public static String ExpandMonoclinic(char unique_axis, String o) {
    int im = 0, io = 0;
    StringBuffer mm = new StringBuffer("");
    int ol = o.length();

    if (io < ol) mm.append(o.charAt(io++));

    switch (Character.toLowerCase(unique_axis)) {
      case 'a':
        while (io < ol) mm.append(o.charAt(io++));
        mm.append('1');
        mm.append('1');
        break;
      case 'c':
        mm.append('1');
        mm.append('1');
        while (io < ol) mm.append(o.charAt(io++));
        break;
      default:
        mm.append('1');
        while (io < ol) mm.append(o.charAt(io++));
        mm.append('1');
        break;
    }

    return mm.toString();
  }


  public static T_TabSgName FindTabSgNameEntry(String UserSgName, char VolLetter) {

    int MaxWtdLbl = 20;
    String WtdLblOriginal;
    String WtdLblModified;
    String WtdLbl;
    int MaxWtdExt = 5;
    String WtdExt;
    int WtdSgNumber = 0;
    int WtdLblOriginChoice;
    int WtdLblBasisChoice;
    int iwl, iwe, isgl;
    StringBuffer wl = new StringBuffer(""), we = new StringBuffer("");

//  		System.out.println(UserSgName);
//  		System.out.println(VolLetter);

    int i, IsExpanded, lbl_match;
    String sgl;
    int tsgn;
    int WtdCC;
    String WtdUA;
    char[] WtdUA_Buf = new char[2];
    T_ExtInfo ExtInfo = new T_ExtInfo(), WtdExtInfo = new T_ExtInfo();

    if (VolLetter == 0 || Character.isWhitespace(VolLetter))
      VolLetter = 'A';
    else if (VolLetter == '1')
      VolLetter = 'I';
    else {
      VolLetter = Character.toUpperCase(VolLetter);
      if (VolLetter != 'I'
              && VolLetter != 'A')
        return null;
    }

    iwl = 0;

    while (iwl < UserSgName.length() && UserSgName.charAt(iwl) != ':') {
      if (!Character.isWhitespace(UserSgName.charAt(iwl)) && UserSgName.charAt(iwl) != '_') {
        if (iwl >= MaxWtdLbl)
          return null;

        wl.append(UserSgName.charAt(iwl));
      }

      iwl++;
    }

    WtdLblOriginal = WtdLbl = wl.toString();

    if (WtdLbl.length() == 0)
      return null;

    iwe = 0;

    if (iwe + iwl < UserSgName.length()) {
      iwl++;

      while (iwe + iwl < UserSgName.length()) {
        char tmp = UserSgName.charAt(iwe + iwl);
        if (!Character.isWhitespace(tmp) && tmp != '_') {
          if (iwe >= MaxWtdExt)
            return null;

          we.append(tmp);
        }

        iwe++;
      }
    }
    WtdExt = we.toString();

    WtdLblOriginChoice = ' ';
    WtdLblBasisChoice = ' ';

    if (WtdLbl.length() > 1) {
      iwl = WtdLbl.length() - 1;

      if (WtdLbl.charAt(iwl) == 'S' || WtdLbl.charAt(iwl) == 's') {
        WtdLblOriginChoice = '1';
        iwl--;
        WtdLbl = WtdLbl.substring(0, iwl + 1);
      } else if (WtdLbl.charAt(iwl) == 'Z' || WtdLbl.charAt(iwl) == 'z') {
        WtdLblOriginChoice = '2';
        iwl--;
        WtdLbl = WtdLbl.substring(0, iwl + 1);
      } else if (WtdLbl.charAt(iwl) == 'H' || WtdLbl.charAt(iwl) == 'h') {
        WtdLblBasisChoice = 'H';
        iwl--;
        WtdLbl = WtdLbl.substring(0, iwl + 1);
      } else if (WtdLbl.charAt(iwl) == 'R' || WtdLbl.charAt(iwl) == 'r') {
        WtdLblBasisChoice = 'R';
        iwl--;
        WtdLbl = WtdLbl.substring(0, iwl + 1);
      }
    }

    if (Character.isLetter(WtdLbl.charAt(0))) {
//  		System.out.println(WtdLbl);
      WtdSgNumber = FindSchoenfliesSymbol(WtdLbl);
//  		System.out.println(WtdSgNumber);
    } else {
      for (int jwl = 0; jwl < WtdLbl.length(); jwl++)
        if (!Character.isDigit(WtdLbl.charAt(jwl)))
          return null;

      StringTokenizer st = new StringTokenizer(WtdLbl, ": ,\t\r\n");
      String token = null;
      WtdSgNumber = 0;
      if (st.hasMoreTokens()) {
        token = st.nextToken();
        try {
          WtdSgNumber = Integer.valueOf(token).intValue();
        } catch (Exception e) {
        }
      }
      if (token == null || WtdSgNumber < 1 || WtdSgNumber > 230)
        return null;
    }

    if (ParseExtension(WtdExt, WtdExtInfo) != 0)
      return null;

    if (WtdExtInfo.OriginChoice == ' ')
      WtdExtInfo.OriginChoice = (char) WtdLblOriginChoice;
    else if (WtdExtInfo.OriginChoice != WtdLblOriginChoice
            && WtdLblOriginChoice != ' ')
      return null;

    if (WtdExtInfo.BasisChoice == ' ')
      WtdExtInfo.BasisChoice = (char) WtdLblBasisChoice;
    else if (WtdExtInfo.BasisChoice != WtdLblBasisChoice
            && WtdLblBasisChoice != ' ')
      return null;

    if (WtdExtInfo.OriginChoice != ' '
            && WtdExtInfo.BasisChoice != ' ')
      return null;

//    	System.out.println("Origin " + WtdExtInfo.OriginChoice);
    for (IsExpanded = 0; IsExpanded < 4; IsExpanded++) {

      for (tsgn = 0; tsgn < SgGroups.TabSgName.length; tsgn++) {
//    	for (tsgn = 0; ; tsgn++) {

//    	System.out.println("checking " + SgGroups.TabSgName[tsgn].SgLabels);
        if (IsExpanded != 0
                && SgGroups.TabSgName[tsgn].SgNumber > 15)
          break;

        lbl_match = 0;

        if (WtdSgNumber == -1) {
          i = 1;
          sgl = SgGroups.TabSgName[tsgn].SgLabels;
          isgl = 0;
          while (isgl < sgl.length() && i <= 2) {
            while (isgl < sgl.length() && (new String(" =\t")).indexOf(sgl.charAt(isgl)) >= 0) isgl++;

            if (SgLabelCmp(SgGroups.TabSgName[tsgn].SgNumber, sgl.substring(isgl), WtdLbl) == 0) {
              lbl_match = i;
              break;
            }

            while (isgl < sgl.length() && (new String(" =\t")).indexOf(sgl.charAt(isgl)) < 0) isgl++;

            i++;
          }
        }

        if (ParseExtension(SgGroups.TabSgName[tsgn].Extension, ExtInfo) != 0) {
          T_SgInfo.SetSgError(IErr_Corrupt_TabSgName);
          return null;
        }

        if (WtdSgNumber == SgGroups.TabSgName[tsgn].SgNumber || lbl_match != 0) {
          if (SgGroups.TabSgName[tsgn].SgNumber >= 3
                  && SgGroups.TabSgName[tsgn].SgNumber < 16) {
            if (WtdLblOriginChoice != ' '
                    || WtdExtInfo.BasisChoice != ' '
                    || WtdExtInfo.BT_or_UA.length() > 2)
              continue; /* next tsgn */

            if (WtdSgNumber == SgGroups.TabSgName[tsgn].SgNumber) {
              if (WtdExtInfo.BT_or_UA.length() > 0)
                WtdUA = WtdExtInfo.BT_or_UA;
              else if (VolLetter == 'I') {
                if (ExtInfo.BT_or_UA.charAt(0) != 'c'
                        && ExtInfo.BT_or_UA.charAt(1) != 'c')
                  continue; /* next tsgn */

                if (ExtInfo.CellChoice == ' '
                        && (WtdExtInfo.CellChoice == ' '
                        || WtdExtInfo.CellChoice == '1'))
                  return SgGroups.TabSgName[tsgn];

                i = 0;
                for (isgl = 0; isgl < SgGroups.TabSgName[tsgn].SgLabels.length(); isgl++)
                  if (SgGroups.TabSgName[tsgn].SgLabels.charAt(isgl) == '=') i++;

                if (i == 2
                        && (WtdExtInfo.CellChoice == ' '
                        || WtdExtInfo.CellChoice == ExtInfo.CellChoice))
                  return SgGroups.TabSgName[tsgn];

                continue; /* next tsgn */
              } else
                WtdUA = new String("b");
            } else /* if (lbl_match != 0) */ {
              if (WtdExtInfo.BT_or_UA.length() > 0)
                WtdUA = WtdExtInfo.BT_or_UA;
              else if (lbl_match > 1)
                WtdUA = ExtInfo.BT_or_UA;
              else if (VolLetter == 'I'
                      && ExtInfo.CellChoice == ' ')
                WtdUA = new String("c");
              else
                WtdUA = new String("b");
            }

            if (WtdExtInfo.CellChoice != ' ')
              WtdCC = WtdExtInfo.CellChoice;
            else if (ExtInfo.CellChoice == '1')
              WtdCC = ExtInfo.CellChoice;
            else
              WtdCC = ' ';

            if (ExtInfo.BT_or_UA.equals(WtdUA)) {
              if (WtdCC == ' ' && lbl_match > 1)
                return SgGroups.TabSgName[tsgn];
              if (ExtInfo.CellChoice == WtdCC)
                return SgGroups.TabSgName[tsgn];
              if (ExtInfo.CellChoice == ' ' && WtdCC == '1')
                return SgGroups.TabSgName[tsgn];
              if (ExtInfo.CellChoice == '1' && WtdCC == ' ')
                return SgGroups.TabSgName[tsgn];
            }
          } else if (ExtInfo.BasisChoice != ' ') {
            if (WtdExtInfo.OriginChoice != ' '
                    || WtdExtInfo.CellChoice != ' '
                    || WtdExtInfo.BT_or_UA.length() > 0)
              continue; /* next tsgn */

            if (ExtInfo.BasisChoice == WtdExtInfo.BasisChoice)
              return SgGroups.TabSgName[tsgn];

            if (WtdExtInfo.BasisChoice == ' ') {
              if (ExtInfo.BasisChoice == 'R' && VolLetter == 'I')
                return SgGroups.TabSgName[tsgn];
              if (ExtInfo.BasisChoice == 'H' && VolLetter != 'I')
                return SgGroups.TabSgName[tsgn];
            }
          } else if (WtdExtInfo.BasisChoice == ' ') {
            if ((WtdExtInfo.OriginChoice == ' ' && ExtInfo.OriginChoice == '1')
                    || (WtdExtInfo.OriginChoice == '1' && ExtInfo.OriginChoice == ' ')
                    || WtdExtInfo.OriginChoice == ExtInfo.OriginChoice) {
              if (WtdExtInfo.BT_or_UA.length() > 0) {
                if (WtdExtInfo.BT_or_UA.equals(ExtInfo.BT_or_UA))
                  return SgGroups.TabSgName[tsgn];
                if (WtdExtInfo.BT_or_UA.equals(Ext_BT_or_UA)
                        && ExtInfo.BT_or_UA.length() == 0)
                  return SgGroups.TabSgName[tsgn];
              } else {
//              System.out.println(lbl_match);
                if (lbl_match != 0)
                  return SgGroups.TabSgName[tsgn];
                if (ExtInfo.BT_or_UA.length() == 0)
                  return SgGroups.TabSgName[tsgn];
              }
            }
          }
        }
      }

//    System.out.println("Sorry.. ");
//    System.out.println(WtdSgNumber);
      if (WtdSgNumber != -1)
        return null;

      if (WtdExtInfo.BT_or_UA.length() > 2)
        return null;

      if (IsExpanded == 0) {
        iwl += 2;

        if (iwl > MaxWtdLbl)
          IsExpanded = 2;
        else {
          if (WtdExtInfo.BT_or_UA.length() > 0)
            WtdUA = WtdExtInfo.BT_or_UA;
          else {
            if (VolLetter == 'I')
              WtdUA = "c";
            else
              WtdUA = "b";
          }

          WtdLbl = WtdLblModified = ExpandMonoclinic(WtdUA.charAt(0), WtdLblOriginal);
        }
      } else if (IsExpanded == 1) {
        if (WtdExtInfo.BT_or_UA.length() > 0)
          return null;

        if (VolLetter == 'I')
          WtdUA = "b";
        else
          WtdUA = "c";

        WtdLblModified = ExpandMonoclinic(WtdUA.charAt(0), WtdLblOriginal);
      }

      if (IsExpanded == 2) {
        if (WtdExtInfo.BT_or_UA.length() > 0)
          return null;

        iwl -= 2;

        if (iwl < 2)
          return null;
        iwl--;
        WtdUA_Buf[0] = WtdLblOriginal.toLowerCase().charAt(iwl);
        WtdLblOriginal = WtdLblOriginal.substring(0, iwl);
//      WtdUA_Buf[1] = '\0';

        if ((new String("abc")).indexOf(WtdUA_Buf[0]) < 0)
          return null;

        iwl += 2;

        if (iwl > MaxWtdLbl)
          return null;

        WtdLbl = WtdLblModified = ExpandMonoclinic(WtdUA_Buf[0], WtdLblOriginal);
      }
    }

    return null;

  }

/*
const T_TabSgName *FindTabPgNameEntry(const char *UserLbl)
{
#define        MaxWtdLbl 20
  char  WtdLbl[MaxWtdLbl    + 1];
  int   WtdPgNumber;
  int   iwl;
  char  *wl;

  const T_TabSgName  *tpgn;


   wl = WtdLbl;
  iwl = 0;

  while (*UserLbl)
  {
    if (isspace(*UserLbl) == 0 && *UserLbl != '_')
    {
      if (iwl >= MaxWtdLbl)
        return null;

      *wl++ = *UserLbl;
      iwl++;
    }

    UserLbl++;
  }

  *wl = '\0';

  if (iwl == 0)
    return null;

  if (isalpha(WtdLbl[0]))
    WtdPgNumber = -1;
  else
  {
    for (wl = WtdLbl; *wl; wl++)
      if (isdigit(*wl) == 0)
        return null;

    if (   sscanf(WtdLbl, "%d", &WtdPgNumber) != 1
        || WtdPgNumber <  1
        || WtdPgNumber > 17)
      return null;
  }

  for (tpgn = TabPgName; tpgn.HallSymbol; tpgn++)
  {
    if (WtdPgNumber == abs(tpgn.SgNumber))
      return tpgn;

    if (SgLabelCmp(0, tpgn.SgLabels, WtdLbl) == 0)
      return tpgn;
  }

  return null;

#undef MaxWtdLbl
}


unsigned int SgID_Number(const T_TabSgName *tsgn)
{
  unsigned int  ID;
  int           iBT;
  const char    *UA;
  T_ExtInfo     ExtInfo;


  ID = tsgn.SgNumber;

  if (ParseExtension(tsgn.Extension, &ExtInfo) != 0)
    ID = 0;

  if (ID >= 3 && ID < 16)
  {
    UA = ExtInfo.BT_or_UA;

    if (   *UA != 'b'
        || (   ExtInfo.CellChoice != ' '
            && ExtInfo.CellChoice != '1'))
    {
      if (*UA == '-')
      {
        ID += 3000;
        UA++;
      }

      switch (*UA)
      {
        case 'b': ID += 10000; break;
        case 'c': ID += 20000; break;
        case 'a': ID += 30000; break;
        default:  ID = 0;      break;
      }

      if (ID != 0)
      {
        switch (ExtInfo.CellChoice)
        {
          case ' ':             break;
          case '1': ID += 1000; break;
          case '2': ID += 2000; break;
          case '3': ID += 3000; break;
          default:  ID = 0;     break;
        }
      }
    }
  }
  else
  {
    if (ExtInfo.BasisChoice == 'R')
      ID += 20000;
    else
    {
      if (ExtInfo.BT_or_UA[0])
      {
        for (iBT = 0; iBT < 6; iBT++)
          if (ExtInfo.BT_or_UA == Ext_BT_or_UA[iBT])
            break;
      }
      else
        iBT = 0;

      if (iBT < 6)
      {
        if (ExtInfo.OriginChoice == '2') ID += 20000;
        else if (iBT)                    ID += 10000;

        if (iBT)
          ID += (iBT + 1) * 1000;
      }
      else
        ID = 0;
    }
  }

  if (ID == 0)
    SetSgError(IErr_Corrupt_TabSgName);

  return ID;
}


int ParseSymXYZ(const char *SymXYZ, T_RTMx *SeitzMx, int FacTr)
{
  unsigned int  P_mode;
  int           Row, Column, Sign, GotXYZ, i;
  double        Value, Value1, Value2, Delta;


  for (i = 0; i < 12; i++) SeitzMx.a[i] = 0;

#define P_Blank   0x01u
#define P_Comma   0x02u
#define P_Plus    0x04u
#define P_Dash    0x08u
#define P_Slash   0x10u
#define P_Value1  0x20u
#define P_Value2  0x40u
#define P_XYZ     0x80u

  Value1 = 0.;

  Row    = 0;
  Sign   = 1;
  Value  = 0.;
  GotXYZ = 0;
  P_mode = P_Blank | P_Plus | P_Dash | P_Value1 | P_XYZ;

  do
  {
    switch (*SymXYZ)
    {
      case ' ':
      case '\t':
      case '_':
        if ((P_mode & P_Blank) == 0) return -1;
        break;
      case ',':
      case ';':
        if (Row == 2)                return -1;
      case '\0':
        if ((P_mode & P_Comma) == 0) return -1;
        if (GotXYZ == 0)             return -1;
        if (P_mode & P_Slash) Value += Value1;
        Value *= FacTr;
        if (Value < 0.) i = (int)(Value - .5);
        else            i = (int)(Value + .5);
        Delta = Value - i;
        if (Delta < 0.) Delta = -Delta;
        if (Delta > .01 * FacTr) return -1;
        i %= FacTr; if (i < 0) i += FacTr;
        SeitzMx.s.T[Row] = i;
        Row++;
        Sign   = 1;
        Value  = 0.;
        P_mode = P_Blank | P_Plus | P_Dash | P_Value1 | P_XYZ;
        GotXYZ = 0;
        break;
      case '+':
        if ((P_mode & P_Plus)  == 0) return -1;
        if (P_mode & P_Slash) Value += Value1;
        Sign =  1;
        if (P_mode & P_Value2)
          P_mode = P_Value2;
        else
          P_mode = P_Blank | P_Value1 | P_XYZ;
        break;
      case '-':
        if ((P_mode & P_Dash)  == 0) return -1;
        if (P_mode & P_Slash) Value += Value1;
        Sign = -1;
        if (P_mode & P_Value2)
          P_mode = P_Value2;
        else
          P_mode = P_Blank | P_Value1 | P_XYZ;
        break;
      case '/':
      case ':':
        if ((P_mode & P_Slash) == 0) return -1;
        Sign =  1;
        P_mode = P_Blank | P_Plus | P_Dash | P_Value2;
        break;
      case '.':
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        if      (P_mode & P_Value1)
        {
          if (sscanf(SymXYZ, "%lf%n", &Value1, &i) != 1) return -1;
          if (Sign == -1) Value1 = -Value1;
          P_mode = P_Blank | P_Comma | P_Plus | P_Dash | P_Slash;
        }
        else if (P_mode & P_Value2)
        {
          if (sscanf(SymXYZ, "%lf%n", &Value2, &i) != 1) return -1;
          if (Sign == -1) Value2 = -Value2;
          if (Value1 != 0.)
          {
            if (Value2 == 0.) return -1;
            Value += Value1 / Value2;
          }
          P_mode = P_Blank | P_Comma | P_Plus | P_Dash;
        }
        else
          return -1;
        SymXYZ += (i - 1);
        break;
      case 'X':
      case 'x': Column = 0; goto Process_XYZ;
      case 'Y':
      case 'y': Column = 1; goto Process_XYZ;
      case 'Z':
      case 'z': Column = 2;
       Process_XYZ:
        if ((P_mode & P_XYZ) == 0) return -1;
        i = Row * 3 + Column;
        if (SeitzMx.s.R[i] != 0) return -1;
        SeitzMx.s.R[i] = Sign;
        GotXYZ = 1;
        P_mode = P_Blank | P_Comma | P_Plus | P_Dash;
        break;
    }
  }
  while (*SymXYZ++);

  if (Row != 3) return -1;

  return 0;

#undef P_Blank
#undef P_Comma
#undef P_Plus
#undef P_Dash
#undef P_Slash
#undef P_Value1
#undef P_Value2
#undef P_XYZ
} */


  public static int LookupRotMx(T_HallGenerator HG) {
    int i, f, refaxis, dircode;
    int iNextBasis, nNextBasis;
    int txrmx;


    if (HG.Rotation <= 0) return 0;

    refaxis = HG.RefAxis;
    dircode = HG.DirCode;

    if (HG.Rotation == 1) {
      refaxis = 'o';
      dircode = '.';
      nNextBasis = 0;
    } else if (dircode == '*') {
      if (refaxis == 0) refaxis = 'o';
      nNextBasis = 0;
    } else {
      if (dircode == 0) dircode = '=';

      switch (refaxis) {
        case 'z':
          nNextBasis = 0;
          break;
        case 'x':
          nNextBasis = 1;
          break;
        case 'y':
          nNextBasis = 2;
          break;
        default:
          return 0;
      }
    }

    for (txrmx = 0; T_SgInfo.TabXtalRotMx[txrmx].Order != 0; txrmx++)
      if (T_SgInfo.TabXtalRotMx[txrmx].Order == HG.Rotation) break;

    while (T_SgInfo.TabXtalRotMx[txrmx].Order == HG.Rotation) {
      if (T_SgInfo.TabXtalRotMx[txrmx].getDirCode() == dircode) {
        if (HG.Improper == 0)
          f = 1;
        else
          f = -1;

        for (i = 0; i < 9; i++)
          HG.SeitzMx.s.R[i] = T_SgInfo.TabXtalRotMx[txrmx].RMx[i] * f;

        for (iNextBasis = 0; iNextBasis < nNextBasis; iNextBasis++)
          T_SgInfo.RotateRotMx(HG.SeitzMx.s.R, T_SgInfo.RMx_3_111, T_SgInfo.RMx_3i111);

        return 1;
      }

      txrmx++;
    }

    return 0;
  }


/*
static const char *PrintSgLabel(const char *lbl, int space, int *n,
                                FILE *fpout)
{
  while (*lbl && *lbl != ' ')
  {
    if (*lbl == '_')
    {
      if (space)
      {
        putc(space, fpout);
        if (n) (*n)++;
      }
    }
    else
    {
      putc(*lbl, fpout);
      if (n) (*n)++;
    }

    lbl++;
  }

  return lbl;
}


int PrintFullHM_SgName(const T_TabSgName *tsgn, int space, FILE *fpout)
{
  int         n;
  const char  *lbl;


  lbl = tsgn.SgLabels;

  if (tsgn.SgNumber >= 3 && tsgn.SgNumber < 16)
    while (*lbl) if (*lbl++ == '=') break;

  SkipWhite(lbl);

  n = 0;

  (void) PrintSgLabel(lbl, space, &n, fpout);

  lbl = tsgn.Extension;

  if (*lbl && strchr("12HhRr", *lbl))
  {
    putc(':', fpout);
    putc(*lbl, fpout);
    n += 2;
  }

  return n;
}


void PrintTabSgNameEntry(const T_TabSgName *tsgn, int Style, int space,
                         FILE *fpout)
{
  int         n;
  const char  *lbl, *SfSymbol;


  if (Style)
    n = fprintf(fpout, "%3d", abs(tsgn.SgNumber));
  else
    n = fprintf(fpout,  "%d", abs(tsgn.SgNumber));

  if (tsgn.Extension[0])
    n += fprintf(fpout, ":%s", tsgn.Extension);

  if (Style)
    while (n < 9) { putc(' ', fpout); n++; }

  putc(' ', fpout); n++;
  putc(' ', fpout); n++;

  if (tsgn.SgNumber >= 1 && tsgn.SgNumber <= 230)
    SfSymbol = SchoenfliesSymbols[tsgn.SgNumber];
  else
    SfSymbol = "";

  n += fprintf(fpout, "%s", SfSymbol);

  if (Style)
    while (n < 23) { putc(' ', fpout); n++; }

  putc(' ', fpout); n++;
  putc(' ', fpout); n++;

  if (tsgn.SgNumber >= 3 && tsgn.SgNumber < 16)
  {
    lbl = PrintSgLabel(tsgn.SgLabels, space, &n, fpout);

    if (tsgn.Extension[0])
      n += fprintf(fpout, ":%s", tsgn.Extension);

    putc(' ', fpout); putc('=', fpout); putc(' ', fpout); n += 3;

    n += PrintFullHM_SgName(tsgn, space, fpout);

    while (*lbl) if (*lbl++ == '=') break;
    while (*lbl) if (*lbl++ == '=') break;
    SkipWhite(lbl);

    if (*lbl)
    {
      putc(' ', fpout); putc('=', fpout); putc(' ', fpout); n += 3;

      (void) PrintSgLabel(lbl, space, &n, fpout);
    }
  }
  else
    n += PrintFullHM_SgName(tsgn, space, fpout);

  if (Style)
    while (n < 51) { putc(' ', fpout); n++; }

  putc(' ', fpout);
  putc(' ', fpout);

  Fprintf(fpout, "%s", tsgn.HallSymbol);
}


static void SimplifyFraction(int nume, int deno, int *o_nume, int *o_deno)
{
  int gcd, S[2];


  S[0] = nume;
  S[1] = deno;

      gcd = FindGCD(S, 2);
  if (gcd)
  {
    *o_nume = nume / gcd;
    *o_deno = deno / gcd;

    if (*o_deno < 0) {
      *o_nume *= -1;
      *o_deno *= -1;
    }
  }
}


const char *FormatFraction(int nume, int deno, int Decimal,
                           char *Buffer, int SizeBuffer)
{
  int          n, d;
  char         *cp, *cpp;
  static char  StaticBuffer[40];


  if (null == Buffer) {
              Buffer =        StaticBuffer;
          SizeBuffer = sizeof StaticBuffer / sizeof (*StaticBuffer);
  }

  Buffer[SizeBuffer - 1] = '\0';

  if (nume == 0)
  {
    Buffer[0] = '0';
    Buffer[1] = '\0';
  }
  if (Decimal)
  {
    (void) sprintf(Buffer, "%.6g", (double) nume / deno);

         cp = Buffer;
    if (*cp == '-') cp++;
    if (*cp == '0') {
      cpp = cp + 1; while (*cp) *cp++ = *cpp++;
    }
  }
  else
  {
    SimplifyFraction(nume, deno, &n, &d);

    if (d == 1)
      (void) sprintf(Buffer, "%d", n);
    else
      (void) sprintf(Buffer, "%d/%d", n, d);
  }

  if (Buffer[SizeBuffer - 1] != '\0') {
      Buffer[SizeBuffer - 1] =  '\0';
    SetSgError("Internal Error: FormatFraction(): Buffer too small");
    return null;
  }

  return Buffer;
}


const char *RTMx2XYZ(const T_RTMx *RTMx, int FacRo, int FacTr,
                     int Decimal, int TrFirst, int Low,
                     const char *Separator,
                     char *BufferXYZ, int SizeBufferXYZ)
{
  static const char *UpperXYZ = "XYZ";
  static const char *LowerXYZ = "xyz";

  int         i, j, p, iRo, iTr;
  char        *xyz, buf_tr[32];
  const char  *sep, *LetterXYZ, *ro, *tr;

  static char  StaticBufferXYZ[80];


  if (null == BufferXYZ) {
              BufferXYZ  =        StaticBufferXYZ;
          SizeBufferXYZ  = sizeof StaticBufferXYZ / sizeof (*StaticBufferXYZ);
  }

  BufferXYZ[SizeBufferXYZ - 1] = '\0';

  if (Low)
    LetterXYZ = LowerXYZ;
  else
    LetterXYZ = UpperXYZ;

  if (Separator == null)
      Separator = ",";

  xyz = BufferXYZ;

  for (i = 0; i < 3; i++)
  {
    if (i != 0)
      for (sep = Separator; *sep; sep++) *xyz++ = *sep;

    sep = xyz;

        iTr = iModPositive(RTMx.s.T[i], FacTr);
    if (iTr >  FacTr / 2)
        iTr -= FacTr;

        tr = FormatFraction(iTr, FacTr, Decimal,
                            buf_tr, sizeof buf_tr / sizeof (*buf_tr));
    if (tr == null)
      return null;

    p = 0;

    if (  TrFirst && iTr) {
      if (*tr) p = 1;
      while (*tr) *xyz++ = *tr++;
    }

    for (j = 0; j < 3; j++)
    {
          iRo = RTMx.s.R[i * 3 + j];
      if (iRo)
      {
            ro = FormatFraction(iRo, FacRo, Decimal, null, 0);
        if (ro == null)
          return null;

        if      (*ro == '-')
          *xyz++ = *ro++;
        else if (*ro && p)
          *xyz++ = '+';

        if (ro[0] != '1' || ro[1] != '\0') {
          while (*ro) *xyz++ = *ro++;
          *xyz++ = '*';
        }

        *xyz++ = LetterXYZ[j];

        p = 1;
      }
    }

    if (! TrFirst && iTr)
    {
      if (*tr && *tr != '-' && p)
        *xyz++ = '+';

      while (*tr) *xyz++ = *tr++;
    }

    if (xyz == sep)
      *xyz++ = '0';
  }

  *xyz = '\0';

  if (BufferXYZ[SizeBufferXYZ - 1] != '\0') {
      BufferXYZ[SizeBufferXYZ - 1] =  '\0';
    SetSgError("Internal Error: RTMx2XYZ(): BufferXYZ too small");
    return null;
  }

  return BufferXYZ;
}


void PrintMapleRTMx(const T_RTMx *RTMx, int FacRo, int FacTr,
                    const char *Label, FILE *fpout)
{
  int         i, j, nt;
  const int   *r, *t;
  const char  *ff;


  if (Label)
    Fprintf(fpout, "%s", Label);

  Fprintf(fpout, " := matrix(4,4, [");

  r = RTMx.s.R;
  t = RTMx.s.T;

  for (i = 0; i < 3; i++, t++)
  {
    putc(' ', fpout);

    for (j = 0; j < 3; j++, r++)
    {
          ff = FormatFraction(*r, FacRo, 0, null, 0);
      if (ff == null)
        return;

      Fprintf(fpout, "%s,", ff);
    }

        nt = iModPositive(*t, FacTr);
    if (nt >  FacTr / 2)
        nt -= FacTr;

        ff = FormatFraction(nt, FacTr, 0, null, 0);
    if (ff == null)
      return;

    Fprintf(fpout, "%s,", ff);
  }

  Fprintf(fpout, " 0,0,0,1]);\n");
}


void PrintMathMRTMx(const T_RTMx *RTMx, int FacRo, int FacTr,
                    const char *Label, FILE *fpout)
{
  int         i, j, nt;
  const int   *r, *t;
  const char  *ff;


  if (Label)
    Fprintf(fpout, "%s", Label);

  Fprintf(fpout, "={");

  r = RTMx.s.R;
  t = RTMx.s.T;

  for (i = 0; i < 3; i++, t++)
  {
    putc('{', fpout);

    for (j = 0; j < 3; j++, r++)
    {
          ff = FormatFraction(*r, FacRo, 0, null, 0);
      if (ff == null)
        return;

      Fprintf(fpout, "%s,", ff);
    }

        nt = iModPositive(*t, FacTr);
    if (nt >  FacTr / 2)
        nt -= FacTr;

        ff = FormatFraction(nt, FacTr, 0, null, 0);
    if (ff == null)
      return;

    Fprintf(fpout, "%s},", ff);
  }

  Fprintf(fpout, "{0,0,0,1}}\n");
}


static void PrintSeitzMx(const T_RTMx *SMx, FILE *fpout)
{
  int         i, nt;
  const char  *ff;
  const int   *r, *t;


  r = SMx.s.R;
  t = SMx.s.T;

  for (i = 0; i < 3; i++)
  {
    Fprintf(fpout, " %2d", *r++);
    Fprintf(fpout, " %2d", *r++);
    Fprintf(fpout, " %2d", *r++);

        nt = iModPositive(*t++, STBF);
    if (nt >  STBF / 2)
        nt -= STBF;

        ff = FormatFraction(nt, STBF, 0, null, 0);
    if (ff == null)
      return;

    Fprintf(fpout, " %6s\n", ff);
  }

  putc('\n', fpout);
}


void ListSgInfo(const T_SgInfo *SgInfo, int F_XYZ, int F_Verbose, FILE *fpout)
{
  int           iList, i_ssVM;
  char          buf[8];
  const char    *xyz;
  const T_RTMx  *lsmx;
  T_RotMxInfo   *rmxi, RotMxInfo;


  iList = PG_Index(SgInfo.PointGroup);

  Fprintf(fpout, "Point Group  %s\n", PG_Names[iList]);
  Fprintf(fpout, "Laue  Group  %s\n",
    PG_Names[PG_Index(LG_Code_of_PG_Index[iList])]);

  Fprintf(fpout, "%s\n", XS_Name[SgInfo.XtalSystem]);

  if (SgInfo.UniqueRefAxis != 0 || SgInfo.UniqueDirCode != 0)
  {
    Fprintf(fpout, "Unique Axis  ");
    if (SgInfo.UniqueRefAxis != 0 && SgInfo.UniqueRefAxis != 'o')
      Fprintf(fpout, "%c", SgInfo.UniqueRefAxis);
    if (SgInfo.UniqueDirCode != 0 && SgInfo.UniqueDirCode != '=')
      Fprintf(fpout, "%c", SgInfo.UniqueDirCode);
    Fprintf(fpout, "\n");
  }

  if (SgInfo.ExtraInfo != EI_Unknown)
    Fprintf(fpout, "%s\n", EI_Name[SgInfo.ExtraInfo]);

  if (SgInfo.InversionOffOrigin)
    Fprintf(fpout, "Note: Inversion operation off origin\n");

  if (SgInfo.Chiral > 0)
    Fprintf(fpout, "Chiral space group\n");

  putc('\n', fpout);

  Fprintf(fpout, "Order   %3d\n", SgInfo.OrderL);
  Fprintf(fpout, "Order P %3d\n", SgInfo.OrderP);
  putc('\n', fpout);

  if (SgInfo.n_ssVM >= 0)
  {
    Fprintf(fpout, "s.s.Vector  Modulus\n");
    for (i_ssVM = 0; i_ssVM < SgInfo.n_ssVM; i_ssVM++)
      Fprintf(fpout, " %2d %2d %2d   %d\n",
        SgInfo.ssVM[i_ssVM].V[0],
        SgInfo.ssVM[i_ssVM].V[1],
        SgInfo.ssVM[i_ssVM].V[2],
        SgInfo.ssVM[i_ssVM].M);
    putc('\n', fpout);
  }

  if (F_XYZ || F_Verbose)
  {
    Fprintf(fpout, "#List   %3d\n", SgInfo.nList);
    putc('\n', fpout);

    lsmx = SgInfo.ListSeitzMx;
    rmxi = SgInfo.ListRotMxInfo;

    if (rmxi == null) rmxi = &RotMxInfo;

    for (iList = 0; iList < SgInfo.nList; iList++, lsmx++)
    {
      if (rmxi == &RotMxInfo)
      {
        if (GetRotMxInfo(lsmx.s.R, &RotMxInfo, null) == 0) {
          SetSgError("Error: Illegal SeitzMx in list");
          return;
        }
      }

      if (F_Verbose)
      {
        (void) sprintf(buf, "(%d)", iList + 1);
        Fprintf(fpout, "%-4s", buf);

        Fprintf(fpout, "  %2d", rmxi.Order);
        if (rmxi.Inverse) Fprintf(fpout, "^-1");
        else               Fprintf(fpout, "   ");

        Fprintf(fpout, " [%2d %2d %2d]",
                        rmxi.EigenVector[0],
                        rmxi.EigenVector[1],
                        rmxi.EigenVector[2]);

        if (rmxi.RefAxis) Fprintf(fpout, " '%c'", rmxi.RefAxis);
        else               Fprintf(fpout, "    ");
        if (rmxi.DirCode) Fprintf(fpout, " '%c'", rmxi.DirCode);
        else               Fprintf(fpout, "    ");

        Fprintf(fpout, "    ");
      }

          xyz = RTMx2XYZ(lsmx, 1, STBF, 0, 0, 1, ", ", null, 0);
      if (xyz)
        Fprintf(fpout, "%s", xyz);

      putc('\n', fpout);

      if (xyz == null)
        return;

      if (F_Verbose)
        PrintSeitzMx(lsmx, fpout);

      if (rmxi != &RotMxInfo) rmxi++;
    }

    if (iList && F_Verbose == 0)
      putc('\n', fpout);
  }
}*/

}
