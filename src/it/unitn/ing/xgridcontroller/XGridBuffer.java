/*
 * @(#)XGridBuffer.java created Feb 17, 2006 Newark Liberty Int. Airport
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.xgridcontroller;

import it.unitn.ing.rista.util.Misc;

import java.io.*;


/**
 * The XGridBuffer is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:05 $
 * @since JDK1.1
 */

public class XGridBuffer {
  String buffer = null;

  public void send(XGridSendConnection conn) throws XGridException {
    DataOutputStream strm = conn.strm;
    try {
      ObjectOutputStream out;
      out = new ObjectOutputStream(conn.strm);
      out.writeObject(buffer);
    } catch (IOException ioe) {
      System.out.println("XGridBuffer, send - i/o exception");
      throw new XGridException("XGridBuffer, send - " +
          "i/o exception");
    }
  }

  public void recv(XGridRecvConnection conn) throws XGridException {
    int i, N;
    StringBuffer tmp;

    DataInputStream strm = conn.strm;
    try {
      ObjectInputStream in;
      in = new ObjectInputStream(conn.strm);
      N = strm.readInt();
      System.out.println("XGridBuffer, recv " + N +
          " buffer elements.");
/*      for (i = 0; i < N; i++) {
        tmp = new jpvmBufferElement();
        tmp.recv(conn);
        addElt(tmp);
      }*/
    } catch (IOException ioe) {
      System.out.println("XGridBuffer, recv - i/o exception");
      throw new XGridException("XGridBuffer, recv - " +
          "i/o exception");
    }
  }
}
