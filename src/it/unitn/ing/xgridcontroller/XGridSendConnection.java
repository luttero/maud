/*
 * @(#)XGridSendConnection.java created Feb 17, 2006 Newark Liberty Int. Airport
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
 * The XGridSendConnection is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:05 $
 * @since JDK1.1
 */

public class XGridSendConnection {

  public DataOutputStream strm;
  public XGridTaskId tid;

  public XGridSendConnection() {
    strm = null;
    tid = null;
  }

  public XGridSendConnection(Socket sock, XGridTaskId t) {
    if (sock == null || t == null) return;
    tid = t;
    try {
      OutputStream outstrm = sock.getOutputStream();
      outstrm = new BufferedOutputStream(outstrm);
      strm = new DataOutputStream(outstrm);
    } catch (IOException ioe) {
      strm = null;
      tid = null;
      System.out.println("XGridSendConnection, i/o exception");
    }
  }

  public static XGridSendConnection connect(XGridTaskId t, XGridTaskId f)
          throws XGridException {
    XGridSendConnection ret = null;
    try {
      System.out.println("XGridSendConnection, " +
              "connecting to " + t.toString());

      // Make the new connection...
      Socket sock = new Socket(t.getHost(), t.getPort());
      ret = new XGridSendConnection(sock, t);

      // Send my identity to the newly connected task...
      f.send(ret.strm);
      ret.strm.flush();
    } catch (IOException ioe) {
      System.out.println("XGridSendConnection, connect - " +
              " i/o exception");
      throw new XGridException("XGridSendConnection, connect - "
              + " i/o exception: \"" + ioe + "\"");
    }
    return ret;
  }
}
