/*
 * @(#)T_LatticeInfo.java	0.10 created 10/10/1999 Pergine Vals.
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
 * The T_LatticeInfo contains many small classes.
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:02 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

/* #ifndef SGINFO_H__
#define SGINFO_H__


#ifndef SGCLIB_C__
extern
const char *SgError;
#ifdef SGCOREDEF__
extern
char        SgErrorBuffer[128];
#endif
#else
const char *SgError = NULL;
char        SgErrorBuffer[128];
#endif
*/

public class T_LatticeInfo {
  private int Code;
  public int nTrVector;
  public int[] TrVector = null;

  public T_LatticeInfo(int code, int ntrVector, int[] trVector) {
    super();
    setCode(code);
    nTrVector = ntrVector;

    TrVector = new int[trVector.length];
    for (int i = 0; i < trVector.length; i++)
      TrVector[i] = trVector[i];
  }

  public T_LatticeInfo(char code, int ntrVector, int[] trVector) {
    this((int) code, ntrVector, trVector);
  }

  public T_LatticeInfo(T_LatticeInfo latticeInfo) {
    this(latticeInfo.getCode(), latticeInfo.nTrVector, latticeInfo.TrVector);
  }

  public void setCode(char code) {
    setCode((int) code);
  }

  public void setCode(int code) {
    Code = code;
  }

  public char getCode() {
    return (char) getIntCode();
  }

  public int getIntCode() {
    return Code;
  }
}

