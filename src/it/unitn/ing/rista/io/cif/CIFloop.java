/*
 * @(#)CIFloop.java created 2/07/2001 Casalino
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.io.cif;

import java.io.*;
import java.util.*;
import java.net.*;

import it.unitn.ing.rista.util.*;

/**
 *  The CIFloop is a utility class to read, write CIF loop
 *
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:58 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class CIFloop {

  Vector[] listItems = null;
  String lastReadedLine = null;
  BufferedReader reader = null;

  public CIFloop(BufferedReader areader) {
    reader = areader;
  }

  public int getNumberOfCIFentries() {
    if (listItems != null)
      return listItems.length;
    else
      return 0;
  }

  public String getCIFentry(int cifEntry) {
    if (cifEntry < getNumberOfCIFentries() && cifEntry >= 0)
      return (String) listItems[cifEntry].elementAt(0);
    return null;
  }

  public int getNumberOfCIFelements() {
    return getNumberOfCIFelements(0);
  }

  public int getNumberOfCIFelements(int cifEntry) {
    if (cifEntry < getNumberOfCIFentries() && cifEntry >= 0)
      return listItems[cifEntry].size() - 1;
    return 0;
  }

  public String getCIFelement(int cifEntry, int itemNumber) {
    if (itemNumber < getNumberOfCIFelements(cifEntry) && itemNumber >= 0)
      return (String) listItems[cifEntry].elementAt(itemNumber + 1);
    return null;
  }

  public String getLastReadedLine() {
    return lastReadedLine;
  }

  public boolean lookForAndReadLoop() {
    if (lookForLoop())
      return readLoop();
    return false;
  }

  public boolean lookForLoop() {
    boolean found = false;
    if (reader != null) {
      try {
        String linedata = readLine();
        while (linedata != null && !linedata.toLowerCase().startsWith(CIFdictionary.loopDecl))
          linedata = readLine();
        if (linedata != null) {
          found = true;
        }
      } catch (IOException e) {
        System.out.println("Error looking for loop_ entry");
      }
    }
    return found;
  }

  public boolean readLoop() {
    boolean readed = false;
    if (reader != null) {
      try {
        String token = new String("");
        StringTokenizer st;
        String linedata = readLine();
        Vector cifItems = new Vector(0, 1);

        while (linedata.startsWith("_")) {
          st = new StringTokenizer(linedata, "' ,\t\r\n");
          while (st.hasMoreTokens()) {
            token = st.nextToken();
            cifItems.addElement(token);
          }
          linedata = readLine();
        }

        int maxindex = cifItems.size();
        int index = 0;

        listItems = new Vector[maxindex];
        for (int i = 0; i < maxindex; i++) {
          listItems[i] = new Vector(0, 1);
          listItems[i].addElement(cifItems.elementAt(i));
        }
//        cifItems.removeAllElements();
        cifItems = null;

        boolean endofloop = false;
        while (linedata != null && !endofloop) {
          st = new StringTokenizer(linedata, "' ,\t\r\n");
          while (st.hasMoreTokens()) {
            readed = true;  // at least we readed one item
            token = st.nextToken();
            index++;
            listItems[index - 1].addElement(token);
            if (index == maxindex)
              index = 0;
          }
          linedata = readLine();
          endofloop = checkEndOfLoop(linedata);
        }

      } catch (IOException e) {
        System.out.println("Error reading loop items!");
      }
    }
    return readed;
  }

  public boolean checkEndOfLoop(String linedata) {
    if (linedata == null)
      return true;
    StringTokenizer st = new StringTokenizer(linedata, "' ,\t\r\n");
    if (!st.hasMoreTokens())
      return true;
    if (linedata.startsWith("_"))
      return true;
    if (linedata.startsWith("data_"))
      return true;
    if (linedata.startsWith("loop_"))
      return true;
    return false;
  }

  public String readLine() throws IOException {
    if (reader == null)
      lastReadedLine = null;
    else
      lastReadedLine = reader.readLine();
    return lastReadedLine;
  }

}
