/*
 * @(#)CIFUtil.java created 7/04/1999 Mesiano
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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
 *  The CIFUtil is a utility class to read CIF files
 *
 *
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:58 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class CIFUtil {

  protected BufferedReader fileToRead = null;
  protected boolean isLoop = false;

  public CIFUtil(BufferedReader filename) {
    fileToRead = filename;
  }

  public boolean hasMoreDataBlocks() {
    try {
      String line = fileToRead.readLine();
      while (line != null) {
        StringTokenizer st = new StringTokenizer(line, " \t\r\n");
        if (st.hasMoreTokens()) {
//    		fileToRead.pushBack();
          return true;
        }
        line = fileToRead.readLine();
      }
    } catch (IOException io) {
    }
    return false;
  }

  public CIFDataBlock nextDataBlock() {
    if (hasMoreDataBlocks()) {
      CIFDataBlock cifdata = new CIFDataBlock(this);
      cifdata.readDataBlock();
      return cifdata;
    } else
      return null;
  }

  public boolean hasMoreCIFItems() {
    isLoop = false;

    try {
      String line = fileToRead.readLine();
      while (line != null) {
        if (line.toLowerCase().startsWith(CIFdictionary.dataDecl)) {
//    		fileToRead.pushBack();
          return false;
        }
        if (line.toLowerCase().startsWith(CIFdictionary.loopDecl)) {
          isLoop = true;
          return true;
        }
        StringTokenizer st = new StringTokenizer(line, " \t\r\n");
        if (st.hasMoreTokens()) {
//    		fileToRead.pushBack();
          return true;
        }
        line = fileToRead.readLine();
      }
    } catch (IOException io) {
    }
    return false;
  }

  public CIFItem[] nextCIFItems() {
    if (hasMoreCIFItems()) {
      if (isLoop)
        return getLoopedCIFItems();
      CIFItem cifdata[] = new CIFItem[1];
      cifdata[0] = new CIFItem(this);
      cifdata[0].readCIFItem();
      return cifdata;
    } else
      return null;
  }

  public CIFItem[] getLoopedCIFItems() {
    return null;
  }

  public static final void writeLoopDecl(BufferedWriter out, String[] loopIDs, Object sender) {
    try {
      out.newLine();
      out.write(CIFdictionary.loopDecl);
      out.newLine();
      int numberOfLoopIDs = loopIDs.length;
      for (int i = 0; i < numberOfLoopIDs; i++) {
        out.write(loopIDs[i]);
        out.newLine();
      }
    } catch (IOException ioe) {
      System.out.println("Error writing loop decl for object " + sender.toString());
    }
  }

  public static final void writeLoopDecl(PrintStream out, String[] loopIDs, Object sender) {
    try {
      out.print(Constants.lineSeparator);
      out.print(CIFdictionary.loopDecl);
      out.print(Constants.lineSeparator);
      int numberOfLoopIDs = loopIDs.length;
      for (int i = 0; i < numberOfLoopIDs; i++) {
        out.print(loopIDs[i]);
        out.print(Constants.lineSeparator);
      }
    } catch (Exception ioe) {
      System.out.println("Error writing loop decl for object " + sender.toString());
      ioe.printStackTrace();
    }
  }

}
