/*
 * @(#)XGridRecvConnection.java created Feb 17, 2006 Newark Liberty Int. Airport
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
import java.net.Socket;


/**
 * The XGridRecvConnection is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:05 $
 * @since JDK1.1
 */

public class XGridRecvConnection {

  private InputStream instrm;
  public DataInputStream strm;
  public XGridTaskId tid;

  public XGridRecvConnection() {
    instrm = null;
    strm = null;
    tid = null;
  }

  public XGridRecvConnection(Socket sock) {
    if (sock == null) return;
    try {
      instrm = sock.getInputStream();
      instrm = new BufferedInputStream(instrm);
      strm = new DataInputStream(instrm);
      tid = new XGridTaskId();
      try {
        tid.recv(strm);
      } catch (XGridException jpe) {
        strm = null;
        tid = null;
        System.out.println("XGridRecvConnection, internal" +
                " error");
      }
      System.out.println("XGridRecvConnection, connect to "
              + tid.toString() + " established");
    } catch (IOException ioe) {
      strm = null;
      tid = null;
      System.out.println("XGridRecvConnection, i/o exception");
    }
    if (strm == null) return;
  }

  public boolean hasMessagesQueued() {
    boolean ret = false;
    if (instrm != null) {
      try {
        if (instrm.available() > 0)
          ret = true;
      } catch (IOException ioe) {
        ret = false;
        System.out.println("XGridRecvConnection, " +
                "hasMessagesQueued - i/o exception");
      }
    }
    return ret;
  }
}
