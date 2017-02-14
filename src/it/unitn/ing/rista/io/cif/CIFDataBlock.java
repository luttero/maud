/*
 * @(#)CIFDataBlock.java created 7/04/1999 Pergine Vals.
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
 *  The CIFDataBlock is a utility class to read a data block from a CIF file
 *
 *
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:58 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class CIFDataBlock {

  protected Vector cifItemV = null;
  protected CIFUtil cifUtil = null;

  public CIFDataBlock(CIFUtil cifutil) {
    cifUtil = cifutil;
  }

  public void readDataBlock() {
/*		cifItemV = new Vector(0,1);
		CIFItem cifItem[] = cifUtil.nextCIFItems();
    while (cifItem != null) {
    	for (int i = 0; i < cifItem.size(); i++)
				cifItemV.addElement(cifItem[i]);
			cifItem = cifUtil.nextCIFItems();
    }*/
  }

  public int getNumberOfCIFItem() {
    if (cifItemV != null)
      return cifItemV.size();
    else
      return 0;
  }

  public CIFItem getCIFItem(int index) {
    int total = getNumberOfCIFItem();
    if (index >= 0 && index < total)
      return (CIFItem) cifItemV.elementAt(index);
    else
      return null;
  }

  public static final void writeBlockDecl(BufferedWriter out, String blockID, Object sender) {
    try {
      out.newLine();
      out.write(CIFdictionary.dataDecl + blockID);
      out.newLine();
    } catch (IOException ioe) {
      System.out.println("Error writing data decl for object " + sender.toString());
    }
  }

  public static final void writeBlockDecl(PrintStream out, String blockID, Object sender) {
    try {
      out.print(Constants.lineSeparator);
      out.print(CIFdictionary.dataDecl + blockID);
      out.print(Constants.lineSeparator);
    } catch (Exception ioe) {
      System.out.println("Error writing data decl for object " + sender.toString());
      ioe.printStackTrace();
    }
  }

}




