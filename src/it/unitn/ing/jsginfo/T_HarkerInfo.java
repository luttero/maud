/*
 * @(#)T_HarkerInfo.java	0.10 created 10/10/1999 Pergine Vals.
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
 * The T_HarkerInfo contains many small classes.
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:02 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class T_HarkerInfo {
  public int m;
  public T_RTMx Ox = null;
  public int Dim;
  public int[] N = new int[3];

  public T_HarkerInfo() {
    super();
  }
}


