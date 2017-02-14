/*
 * @(#)XGridSortedProperties.java created Aug 29, 2007 Casalino
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.xgridclient;

import java.util.*;
import java.io.*;

/**
 * The XGridSortedProperties is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Aug 29, 2007 10:10:42 PM $
 * @since JDK1.1
 */
public class XGridSortedProperties extends Properties {

  private static final String specialSaveChars = "=: \t\r\n\f#!";

  public synchronized void store(OutputStream out, String header)
      throws IOException {
    BufferedWriter awriter;
    awriter = new BufferedWriter(new OutputStreamWriter(out, "8859_1"));
    if (header != null)
      writeln(awriter, "#" + header);
    writeln(awriter, "#" + new Date().toString());
    Map sortedMap = Collections.synchronizedMap(new TreeMap(new XGridNaturalStringSorter()));
    sortedMap.putAll(this);
    for (Iterator e = sortedMap.keySet().iterator(); e.hasNext();) {
      String key = (String) e.next();
      String val = (String) get(key);
      key = saveConvert(key, true);

      // No need to escape embedded and trailing spaces for value, hence
      // pass false to flag.

      val = saveConvert(val, false);
      writeln(awriter, key + "=" + val);
    }
    awriter.flush();
  }

  //
  // Converts unicodes to encoded &#92;uxxxx
  // and writes out any of the characters in specialSaveChars
  // with a preceding slash
  //
  private String saveConvert(String theString, boolean escapeSpace) {
    int len = theString.length();
    StringBuffer outBuffer = new StringBuffer(len * 2);

    for (int x = 0; x < len; x++) {
      char aChar = theString.charAt(x);
      switch (aChar) {
        case ' ':
          if (x == 0 || escapeSpace)
            outBuffer.append('\\');

          outBuffer.append(' ');
          break;
        case '\\':
          outBuffer.append('\\');
          outBuffer.append('\\');
          break;
        case '\t':
          outBuffer.append('\\');
          outBuffer.append('t');
          break;
        case '\n':
          outBuffer.append('\\');
          outBuffer.append('n');
          break;
        case '\r':
          outBuffer.append('\\');
          outBuffer.append('r');
          break;
        case '\f':
          outBuffer.append('\\');
          outBuffer.append('f');
          break;
        default:
          if ((aChar < 0x0020) || (aChar > 0x007e)) {
            outBuffer.append('\\');
            outBuffer.append('u');
            outBuffer.append(toHex((aChar >> 12) & 0xF));
            outBuffer.append(toHex((aChar >> 8) & 0xF));
            outBuffer.append(toHex((aChar >> 4) & 0xF));
            outBuffer.append(toHex(aChar & 0xF));
          } else {
            if (specialSaveChars.indexOf(aChar) != -1)
              outBuffer.append('\\');
            outBuffer.append(aChar);
          }
      }
    }
    return outBuffer.toString();
  }

  private static void writeln(BufferedWriter bw, String s) throws IOException {
    bw.write(s);
    bw.newLine();
  }

  //
  // Convert a nibble to a hex character
  // @param	nibble	the nibble to convert.
  //
  private static char toHex(int nibble) {
    return hexDigit[(nibble & 0xF)];
  }

  // A table of hex digits //
  private static final char[] hexDigit = {
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
  };

  class XGridNaturalStringSorter implements Comparator {
    public int compare(Object obj1, Object obj2) {
      String phase1 = (String) obj1;
      String phase2 = (String) obj2;
      int len1 = phase1.length();
      int len2 = phase2.length();
      int minLength = Math.min(len1, len2);
      for (int i = 0; i < minLength; i++) {
        int diff = phase1.charAt(i) - phase2.charAt(i);
        if (diff > 0)
          return 1;
        else if (diff < 0)
          return -1;
      }
      if (len1 > len2)
        return 1;
      else if (len1 < len2)
        return -1;
//    System.out.println("Equals: " +  phase1 + " " + phase2);
      return 0;
    }
  }
}

