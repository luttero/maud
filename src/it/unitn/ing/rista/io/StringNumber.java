/*
 * @(#)StringNumber.java created 01/01/1997 Mesiano
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

package it.unitn.ing.rista.io;

import java.lang.*;
import java.util.*;

/**
 * The StringNumber is a class
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:09 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class StringNumber {
  public String thestring;
  static char cnumber[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-'};
  int endnumber = -1;
  int errorpos = -1;
  boolean isE = false;
  public boolean hasError = false;
  public double value, error;

  public StringNumber(String astring) {
    thestring = astring;
  }

  public boolean isanumber() {
    try {
      int i = 0;
      char first = thestring.charAt(i++);
      if (isminusplus(first))
        first = thestring.charAt(i++);
      if (isdot(first))
        first = thestring.charAt(i++);
      if (is0to9(first)) {
        errorpos = thestring.indexOf('(');
        if (errorpos != -1)
          hasError = true;

        StringTokenizer st = new StringTokenizer(thestring, " ()");
        String token = st.nextToken();
        value = Double.valueOf(token).doubleValue();
        if (hasError && st.hasMoreTokens()) {
          token = st.nextToken();
          error = Double.valueOf(token).doubleValue();
        }
        if (hasError)
          thestring = thestring.substring(0, errorpos);
        return true;
      } else
        return false;
    } catch (Exception nfe) {
      return false;
    }
  }

  public static final boolean isanumber(String astring) {
    try {
      int i = 0;
      char first = astring.charAt(i++);
      if (isminusplus(first))
        first = astring.charAt(i++);
      if (isdot(first))
        first = astring.charAt(i++);
      if (is0to9(first)) {
        return true;
      } else
        return false;
    } catch (Exception nfe) {
      return false;
    }
  }

  public static boolean isminusplus(char achar) {
    for (int i = 10; i <= 11; i++)
      if (achar == cnumber[i])
        return true;
    return false;
  }

  public static boolean isdot(char achar) {
    if (achar == '.')
      return true;
    else
      return false;
  }

  public static boolean is0to9(char achar) {
    for (int i = 0; i <= 9; i++)
      if (achar == cnumber[i])
        return true;
    return false;
  }

  public static boolean isANumberOrSign(char achar) {
    if (is0to9(achar) || isminusplus(achar))
      return true;
    else
      return false;
  }

  public int numberofdecimal() {
    endnumber = thestring.toLowerCase().indexOf('e');
    if (endnumber == -1) {
      endnumber = thestring.indexOf('(');
      errorpos = endnumber;
      if (endnumber == -1) {
        endnumber = thestring.length();
        errorpos = endnumber;
      } else
        hasError = true;
    } else {
      isE = true;
      errorpos = thestring.indexOf('(');
      if (errorpos != -1)
        hasError = true;
      else
        errorpos = thestring.length();
    }
    int dotpos = thestring.indexOf('.');
    if (dotpos != -1 && dotpos <= endnumber)
      return endnumber - dotpos;
    else
      return 0;
  }

  public double getValue() {
    return Double.valueOf(thestring.substring(0, endnumber)).doubleValue();
  }

  public int getE() {
    if (!isE)
      return 0;
    int pos = endnumber + 1;
    if (thestring.substring(pos, errorpos).startsWith("+"))
      pos++;
    return Integer.parseInt(thestring.substring(pos, errorpos));
  }

  public double getError() {
    if (!hasError)
      return 0;
    String errorString = thestring.substring(errorpos + 1, thestring.length() - 1);
    if (isanumber(errorString))
      return Double.valueOf(errorString).doubleValue();
    else
      return 0.0;
  }
}
