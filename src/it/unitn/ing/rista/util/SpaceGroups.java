/*
 * @(#)SpaceGroups.java created 14/10/1998 Mesiano
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
import java.util.Vector;

import it.unitn.ing.jsginfo.*;
import it.unitn.ing.rista.diffr.Reflection;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.util.Arrays;

import static it.unitn.ing.rista.util.Constants.fpsmNativeLibrary;

/**
 * The SpaceGroups is a class providing static methods for
 * space group manipulation.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.6 $, $Date: 2006/07/20 13:39:07 $
 * @since JDK1.1
 */

public class SpaceGroups {

  public static final String[] SYMMETRY = {"triclinic",
      "monoclinic",
      "orthorhombic",
      "tetragonal",
      "trigonal",
      "hexagonal",
      "cubic"};

  public static final int CONV_NUMBER = 0;
  public static final int CONV_SCHOENFLIES = 1;
  public static final int CONV_HM = 2;
  public static final int CONV_HALL = 3;

  public static final String[] laueGroup = {
      "0, -1, C1, 1",
      "1, 2/m, C2, 2",
      "2, 2/mmm, D2, 222",
      "3, 4/m, C4, 4",
      "4, 4/mmm, D4, 422",
      "5, -3, C3, 3",
      "6, -3m, D3, 322",
      "7, 6/m, C6, 6",
      "8, 6/mmm, D6, 622",
      "9, m3, T, 23",
      "10, m3m, O, 423"
  };

	public static final String[] laueGroupOnly = {
			"-1",
			"2/m",
			"2/mmm",
			"4/m",
			"4/mmm",
			"-3",
			"-3m",
			"6/m",
			"6/mmm",
			"m3",
			"m3m"
	};
	/*      SYMMETRY GROUP  : O   T  D4  C4  D2  C2  C1  D6  C6  D3  C3 */
/*                       432 23  422  4  222  2   1  622  6  322  3 */

/*      CODE NUMBER     : 7   6   5   4   3   2   1  11  10   9   8 */


  private SpaceGroups() {
  }

	public static boolean loaded = false;

	static {
		loaded = false;
		try {
			fpsmNativeLibrary = MaudPreferences.getPref("fpsmNativeLibrary.pathToLibrary", fpsmNativeLibrary);
//			System.loadLibrary(fpsmNativeLibrary);
//			loaded = true;
		} catch (UnsatisfiedLinkError err) {
			System.out.println("fpsm native library not loaded, using SgInfo!");
			err.printStackTrace(System.out);
//			throw new RuntimeException(err);
		}
	}

	// const char *fullPatternSearchMatch(char *, char *, char *);
	public static void testFPSMForMaud() {

	}

	public static native Vector<CrystalSystem> getAllSymmetriesAndSpaceGroups();

	public static native Spacegroup checkSpaceGroup(String spaceGroupHall, String spaceGroupHM, String spaceGroupNumber);

	public static native Vector<PureReflection> getReflectionListFor(String spaceGroupHall,
	                                                             double[] cell, double minimumDspace);

	public static boolean useCCTBX() {
		return MaudPreferences.getBoolean("spacegroup.useCCTBX", true) && loaded &&
				Constants.testing;
	}

	public static String getCorrectSpaceGroupHall(String sg) {
		Spacegroup spacegroup = checkSpaceGroup(sg, "", "");
		return spacegroup.hall;
	}

	public static Spacegroup getCorrectSpaceGroup(String sg) {
		return checkSpaceGroup("", sg, "");
	}

	public static final String getSymmetry(int number) {
    return SYMMETRY[number];
  }

  public static final int getSymmetryNumber(String aSymmetry) {
    for (int numb = 0; numb < SYMMETRY.length; numb++)
      if (SYMMETRY[numb].equalsIgnoreCase(aSymmetry))
        return numb;
    return -1;
  }

	public static final int getPGNumberLconvention(int PGnumber, int monoclinicAxis, boolean isRhombohedral) {

    /*  Convention adopted
        0			-1
        1			2/m		unique axis c
        2			2/m		unique axis b
        3			2/m		unique axis a
        4			2/mmm
        5			4/m
        6			4/mmm
        7			-3
        8			-3R
        9			-3m1
        10		-3m1R
        11		-31m
        12		-3m
        13		6/m
        14		6/mmm
        15		m3
        16		m3m
    */

		if (PGnumber >= 1 && PGnumber <= 2)
			return 0;
		if (PGnumber >= 3 && PGnumber <= 5)
			switch (monoclinicAxis) {
				case 3:
					return 1;
				case 2:
					return 2;
				case 1:
					return 3;
				default: {
				}
			}
		if (PGnumber >= 6 && PGnumber <= 8)
			return 4;
		if (PGnumber >= 9 && PGnumber <= 11)
			return 5;
		if (PGnumber >= 12 && PGnumber <= 16)
			return 6;
		if (PGnumber >= 17 && PGnumber <= 18)
			if (!isRhombohedral)
				return 7;
			else
				return 8;
		if (PGnumber == 19 || PGnumber == 22 || PGnumber == 25)
			if (!isRhombohedral)
				return 9;
			else
				return 10;
		if (PGnumber == 20 || PGnumber == 23 || PGnumber == 26)
			return 11;
		if (PGnumber == 21 || PGnumber == 24 || PGnumber == 27)
			return 10;
		if (PGnumber >= 28 && PGnumber <= 30)
			return 12;
		if (PGnumber >= 31 && PGnumber <= 35)
			return 13;
		if (PGnumber >= 36 && PGnumber <= 37)
			return 14;
		if (PGnumber >= 38 && PGnumber <= 40)
			return 15;
		return -1;
	}

	public static final int getLGNumber(int PGnumber) {

    /*  Convention adopted
        0			-1
        1			2/m
        2			2/mmm
        3			4/m
        4			4/mmm
        5			-3
        6		 -3m
        7 		6/m
        8 		6/mmm
        9 		m3
        10		m3m
    */

		if (PGnumber >= 1 && PGnumber <= 2)
			return 0;
		if (PGnumber >= 3 && PGnumber <= 5)
			return 1;
		if (PGnumber >= 6 && PGnumber <= 8)
			return 2;
		if (PGnumber >= 9 && PGnumber <= 11)
			return 3;
		if (PGnumber >= 12 && PGnumber <= 16)
			return 4;
		if (PGnumber >= 17 && PGnumber <= 18)
			return 5;
		if (PGnumber >= 19 && PGnumber <= 27)
			return 6;
		if (PGnumber >= 28 && PGnumber <= 30)
			return 7;
		if (PGnumber >= 31 && PGnumber <= 35)
			return 8;
		if (PGnumber >= 36 && PGnumber <= 37)
			return 9;
		if (PGnumber >= 38 && PGnumber <= 40)
			return 10;
		return -1;
	}

	public static final int getLGNumberSiegfriedConv(int PGnumber) {

    /*  Convention adopted by Siegfried
        1			-1
        2			2/m
        3			2/mmm
        4			4/m
        5			4/mmm
        6 		m3
        7 		m3m
        8			-3
        9		 -3m
        10 		6/m
        11 		6/mmm
    */

/*      SYMMETRY GROUP  : O   T  D4  C4  D2  C2  C1  D6  C6  D3  C3 */
/*                       432 23  422  4  222  2   1  622  6  322  3 */

/*      CODE NUMBER     : 7   6   5   4   3   2   1  11  10   9   8 */

		if (PGnumber >= 1 && PGnumber <= 2)
			return 1;
		if (PGnumber >= 3 && PGnumber <= 5)
			return 2;
		if (PGnumber >= 6 && PGnumber <= 8)
			return 3;
		if (PGnumber >= 9 && PGnumber <= 11)
			return 4;
		if (PGnumber >= 12 && PGnumber <= 16)
			return 5;
		if (PGnumber >= 17 && PGnumber <= 18)
			return 8;
		if (PGnumber >= 19 && PGnumber <= 27)
			return 9;
		if (PGnumber >= 28 && PGnumber <= 30)
			return 10;
		if (PGnumber >= 31 && PGnumber <= 35)
			return 11;
		if (PGnumber >= 36 && PGnumber <= 37)
			return 6;
		if (PGnumber >= 38 && PGnumber <= 40)
			return 7;
		return -1;
	}

  public static final String getSpaceGroup(int sgnumber, int sgconv) {
    if (sgconv == CONV_HM)
      return getSgHM(sgnumber);
    else if (sgconv == CONV_NUMBER)
      return getSgNumber(sgnumber);
    else if (sgconv == CONV_SCHOENFLIES)
      return getSgSchoenflies(sgnumber);
    else if (sgconv == CONV_HALL)
      return getSgHall(sgnumber);
    return getSgHM(sgnumber);
  }

  public static String getSgHall(int sgnumber) {

    String lbl;
    StringBuffer newsgname = new StringBuffer("");
    char ac;
    int i;

    lbl = SgGroups.TabSgName[sgnumber].HallSymbol;

    i = 0;
    int ilbl = 0;
    while (ilbl < lbl.length()) {
      ac = lbl.charAt(ilbl++);
      if (ac != '_' && ac != ' ') {
        newsgname.append(ac);
      }
    }

    return newsgname.toString();
  }

public static int[] getSGnumberAndExt(String sgHM, int symmetry) {
    int[] result = new int[3];
    for (int i = 0; i < SgGroups.TabSgName.length; i++) {
      String ansg = new String(getSpaceGroup(i, 2));
      if (sgHM.equalsIgnoreCase(ansg)) {
        result[0] = SgGroups.TabSgName[i].SgNumber;
        String extension = SgGroups.TabSgName[i].Extension;
        result[1] = 1;
        result[2] = 1;
        switch (symmetry) {
          case 1: // monoclinic
            if (extension.contains("a1"))
              result[1] = 2;
            else if (extension.contains("a2"))
              result[1] = 3;
            else if (extension.contains("a3"))
              result[1] = 4;
            else if (extension.contains("b1"))
              result[1] = 1;
            else if (extension.contains("b2"))
              result[1] = 5;
            else if (extension.contains("b3"))
              result[1] = 6;
            else if (extension.contains("c1"))
              result[1] = 7;
            else if (extension.contains("c2"))
              result[1] = 8;
            else if (extension.contains("c3"))
              result[1] = 9;
            else if (extension.contains("a"))
              result[1] = 2;
            else if (extension.contains("b"))
              result[1] = 1;
            else if (extension.contains("c"))
              result[1] = 3;
            break;
          case 2: // orthorhombic
            if (result[0] == 50 || result[0] == 59) {
              if (extension.equalsIgnoreCase("2")) {
                result[1] = 5;
                result[2] = 2;
              } else if (extension.equalsIgnoreCase("1cab"))
                result[1] = 2;
              else if (extension.equalsIgnoreCase("2cab")) {
                result[1] = 6;
                result[2] = 2;
              } else if (extension.equalsIgnoreCase("1bca"))
                result[1] = 3;
              else if (extension.equalsIgnoreCase("2bca")) {
                result[1] = 4;
                result[2] = 2;
              }
            } else if (result[0] == 68) {
            if (extension.contains("2"))
              result[2] = 2;
            if (extension.contains("cab"))
              result[1] = 2;
            else if (extension.contains("bca"))
              result[1] = 3;
            else if (extension.contains("ba-c"))
              result[1] = 5;
            else if (extension.contains("a-cb"))
              result[1] = 4;
            else if (extension.contains("-cba"))
              result[1] = 6;
            } else if (result[0] == 73) {
              if (extension.equalsIgnoreCase("ba-c"))
                result[1] = 2;
            } else {
            if (extension.equalsIgnoreCase("2"))
              result[2] = 2;
            else if (extension.equalsIgnoreCase("cab"))
              result[1] = 2;
            else if (extension.equalsIgnoreCase("bca"))
              result[1] = 3;
            else if (extension.equalsIgnoreCase("ba-c"))
              result[1] = 5;
            else if (extension.equalsIgnoreCase("a-cb"))
              result[1] = 4;
            else if (extension.equalsIgnoreCase("-cba"))
              result[1] = 6;
            }
            break;
          case 3: // tetragonal
            if (extension.equalsIgnoreCase("2"))
              result[2] = 2;
            break;
          case 4: // trigonal
            if (extension.equalsIgnoreCase("R"))
              result[1] = 2;
            break;
          case 6: // cubic
            if (extension.equalsIgnoreCase("2"))
              result[2] = 2;
            break;
          default: {}
        }
        break;
      }
    }
    return result;
  }

public static final String getSgHM(int sgnumber) {

    T_TabSgName tsgn;
    String lbl;
    StringBuffer newsgname = new StringBuffer("");
    char ac;
    int i, k;

    if (sgnumber < 0 || sgnumber >= SgGroups.TabSgName.length)
      return null;

    tsgn = SgGroups.TabSgName[sgnumber];
    lbl = tsgn.SgLabels;


    i = 0;
    int ilbl = 0;
    while (ilbl < lbl.length() && lbl.charAt(ilbl) != '=') {
      ac = lbl.charAt(ilbl++);
      if (ac != '_' && ac != ' ') {
        newsgname.append(ac);
      }
    }
    k = 0;
    if (tsgn.Extension.length() > 0) {
      newsgname.append(':');
      newsgname.append(tsgn.Extension);
    }

    return newsgname.toString();
  }

public static String getSgNumber(int sgnumber) {

    StringBuffer newsgname = new StringBuffer("");

    newsgname.append(Integer.toString(SgGroups.TabSgName[sgnumber].SgNumber));
    if (SgGroups.TabSgName[sgnumber].Extension.length() > 0)
      newsgname.append(":" + SgGroups.TabSgName[sgnumber].Extension);

    return newsgname.toString();
  }

public static String getSgSchoenflies(int sgnumber) {

    return SgGroups.SchoenfliesSymbols[SgGroups.TabSgName[sgnumber].SgNumber];

  }

  public static final int getBeginSG(String symmetry, int sgconv) {
    int sym = getSymmetryNumber(symmetry);
    if (sgconv == 1) {         //Schoenflies
      switch (sym) {
        case 0:
          return 0;
        case 1:
          return 2;
        case 2:
          return 107;
        case 3:
          return 348;
        case 4:
          return 429;
        case 5:
          return 461;
        case 6:
          return 488;
        default: {
        }
      }
    } else {
      switch (sym) {
        case 0:
          return 0;
        case 1:
          return 2;
        case 2:
          return 107;
        case 3:
          return 348;
        case 4:
          return 429;
        case 5:
          return 461;
        case 6:
          return 488;
        default: {
        }
      }
    }
    return 0;
  }

  public static final int getEndSG(String symmetry, int sgconv) {
    int sym = getSymmetryNumber(symmetry);
    if (sgconv == 1) {         //Schoenflies
      switch (sym) {
        case 0:
          return 1;
        case 1:
          return 106;
        case 2:
          return 347;
        case 3:
          return 428;
        case 4:
          return 460;
        case 5:
          return 487;
        case 6:
          return 529;
        default: {
        }
      }
    } else {
      switch (sym) {
        case 0:
          return 1;
        case 1:
          return 106;
        case 2:
          return 347;
        case 3:
          return 428;
        case 4:
          return 460;
        case 5:
          return 487;
        case 6:
          return 529;
        default: {
        }
      }
    }
    return 0;
  }

  public static final String[] getAllUniqueSgHM() {
    Vector sgNames = new Vector(230, 10);
    int lastSg = -1;
    for (int i = 0; i < SgGroups.TabSgName.length - 1; i++) {
      StringBuffer newsgname = new StringBuffer("");
      char ac;

      T_TabSgName tsgn = SgGroups.TabSgName[i];
      String lbl = tsgn.SgLabels;
      int ilbl = 0;
      while (ilbl < lbl.length() && lbl.charAt(ilbl) != '=') {
        ac = lbl.charAt(ilbl++);
        if (ac != '_' && ac != ' ')
          newsgname.append(ac);
      }
      if (lastSg != tsgn.SgNumber) {
        lastSg = tsgn.SgNumber;
        sgNames.add(newsgname.toString());
      }
    }

    String[] results = new String[sgNames.size()];
    for (int i = 0; i < sgNames.size(); i++)
      results[i] = (String) sgNames.elementAt(i);

    return results;
  }

public static final int getBeginSGUnique(String symmetry) {
    int sym = getSymmetryNumber(symmetry);
    switch (sym) {
      case 0:
        return 0;
      case 1:
        return 2;
      case 2:
        return 15;
      case 3:
        return 74;
      case 4:
        return 142;
      case 5:
        return 167;
      case 6:
        return 194;
      default: {
      }
    }
    return 0;
  }

  public static final int getEndSGUnique(String symmetry) {
    int sym = getSymmetryNumber(symmetry);
    switch (sym) {
      case 0:
        return 2;
      case 1:
        return 15;
      case 2:
        return 74;
      case 3:
        return 142;
      case 4:
        return 167;
      case 5:
        return 194;
      case 6:
        return 230;
      default: {
      }
    }
    return 0;
  }

public static String sglookup(String SgName, int sgconv) {

		char F_Convention;
		T_TabSgName tsgn = null;
//    int i,k;
		String lbl;
		StringBuffer newsgname = new StringBuffer("");
		char ac;

		if (SgName.contains("m3m"))
			SgName = SgName.replace("m3m", "m-3m");
		F_Convention = 'A';
		if (sgconv == 3) {
			F_Convention = 0;
			lbl = SgName;
		} else {
			tsgn = Sgio.FindTabSgNameEntry(SgName, F_Convention);
			if (tsgn == null)
				return null;
			lbl = tsgn.SgLabels;
		}

		int ilbl = 0;
		while (ilbl < lbl.length() && lbl.charAt(ilbl) != '=') {
			ac = lbl.charAt(ilbl++);
			if (ac != '_' && ac != ' ') {
				newsgname.append(ac);
			}
		}
		if (F_Convention != 0)
			if (tsgn.Extension.length() > 0) {
				newsgname.append(':');
				newsgname.append(tsgn.Extension);
			}

		return newsgname.toString();
	}

	public static String getSpaceGroupWithSpaces(String SgName, int sgconv) {

		char F_Convention;
		T_TabSgName tsgn = null;
//    int i,k;
		String lbl;
		StringBuffer newsgname = new StringBuffer("");
		char ac;

		if (SgName.contains("m3m"))
			SgName = SgName.replace("m3m", "m-3m");
		F_Convention = 'A';
		if (sgconv == 3) {
			F_Convention = 0;
			lbl = SgName;
		} else {
			tsgn = Sgio.FindTabSgNameEntry(SgName, F_Convention);
			if (tsgn == null)
				return null;
			lbl = tsgn.SgLabels;
		}

		int ilbl = 0;
		while (ilbl < lbl.length() && lbl.charAt(ilbl) != '=') {
			ac = lbl.charAt(ilbl++);
			if (ac != '_') {
				newsgname.append(ac);
			} else {
				newsgname.append(' ');
			}
		}
//    if (F_Convention != 0)
//      if (tsgn.Extension.length() > 0) {
//        newsgname.append(':');
//        newsgname.append(tsgn.Extension);
//      }

		return newsgname.toString();
	}

}
