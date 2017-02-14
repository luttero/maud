/*
 * @(#)CIFReader.java created 7/04/1999 Mesiano
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
 *  The CIFReader is a utility class to read CIF files
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:09 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class CIFReader {

  protected BufferedReader fileToRead = null;
  protected Vector dataBlockV = null;

  public CIFReader(BufferedReader filename) {
    fileToRead = filename;
    readAll();
  }

  public CIFReader(String filename) {
    this(Misc.getReader(filename));
  }

  public void readAll() {
    dataBlockV = new Vector(0, 1);
    CIFUtil filecif = new CIFUtil(fileToRead);
    CIFDataBlock datablock = filecif.nextDataBlock();
    while (datablock != null) {
      dataBlockV.addElement(datablock);
      datablock = filecif.nextDataBlock();
    }
    try {
      fileToRead.close();
    } catch (IOException io) {
    }
  }

  public int getNumberOfDataBlock() {
    if (dataBlockV != null)
      return dataBlockV.size();
    else
      return 0;
  }

  public CIFDataBlock getDataBlock(int index) {
    int total = getNumberOfDataBlock();
    if (index >= 0 && index < total)
      return (CIFDataBlock) dataBlockV.elementAt(index);
    else
      return null;
  }

}




