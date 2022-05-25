/*
 * @(#)T_RTMx.java	0.10 created 10/10/1999 Pergine Vals.
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
 * The T_RTMx contains many small classes.
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:02 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class T_RTMx {
//  public int[] a = new int[12];
  public RT s = new RT();

  public T_RTMx() {
    super();
  }

  public void copy(T_RTMx source) {
    for (int i = 0; i < 9; i++)
      s.R[i] = source.s.R[i];
    for (int i = 0; i < 3; i++)
      s.T[i] = source.s.T[i];
  }

  public void a(int index, int value) {
    if (index < 9)
      s.R[index] = value;
    else
      s.T[index - 9] = value;
  }

  public int a(int index) {
    if (index < 9)
      return s.R[index];
    else
      return s.T[index - 9];
  }

  public class RT {
    public int[] R = new int[9], T = new int[3];
  }

}

