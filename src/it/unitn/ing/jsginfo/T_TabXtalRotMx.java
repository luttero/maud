/*
 * @(#)T_TabXtalRotMx.java	0.10 created 10/10/1999 Pergine Vals.
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
 * The T_TabXtalRotMx contains many small classes.
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:02 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class T_TabXtalRotMx {
  public int Order;
  public int[] EigenVector = new int[3];
  private char DirCode;
  public int CarHex;
  public int[] RMx = new int[9];
  public int[] ShTrMx = new int[9];

  public T_TabXtalRotMx(int order, int[] eigenVector, char dirCode, int carHex,
                        int[] rMx, int[] shTrMx) {
    super();
    Order = order;
    setDirCode(dirCode);
    CarHex = carHex;

    for (int i = 0; i < eigenVector.length; i++)
      EigenVector[i] = eigenVector[i];
    for (int i = 0; i < rMx.length; i++)
      RMx[i] = rMx[i];
    ShTrMx = shTrMx;
  }

  public T_TabXtalRotMx(int order, int[] eigenVector, int dirCode, int carHex,
                        int[] rMx, int[] shTrMx) {
    super();
    Order = order;
    setDirCode(dirCode);
    CarHex = carHex;

    for (int i = 0; i < eigenVector.length; i++)
      EigenVector[i] = eigenVector[i];
    for (int i = 0; i < rMx.length; i++)
      RMx[i] = rMx[i];
    ShTrMx = shTrMx;
  }

  public T_TabXtalRotMx(int order, int j1, int j2, int j3, int dirCode, int carHex,
                        int i1, int i2, int i3,
                        int i4, int i5, int i6,
                        int i7, int i8, int i9, int[] shTrMx) {
    super();
    Order = order;
    setDirCode(dirCode);
    CarHex = carHex;

    EigenVector[0] = j1;
    EigenVector[1] = j2;
    EigenVector[2] = j3;
    RMx[0] = i1;
    RMx[1] = i2;
    RMx[2] = i3;
    RMx[3] = i4;
    RMx[4] = i5;
    RMx[5] = i6;
    RMx[6] = i7;
    RMx[7] = i8;
    RMx[8] = i9;
    ShTrMx = shTrMx;
  }

  public void setDirCode(char code) {
    DirCode = code;
  }

  public void setDirCode(int code) {
    DirCode = (char) code; //Character.forDigit(code, Character.MAX_RADIX);
  }

  public char getDirCode() {
    return DirCode;
  }

  public int getIntDirCode() {
    return DirCode; //Character.getNumericValue(DirCode);
  }
}

