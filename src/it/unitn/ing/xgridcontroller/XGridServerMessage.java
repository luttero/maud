/*
 * @(#)XGridServerMessage.java created Feb 17, 2006 Newark Liberty Int. Airport
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
 * The XGridServerMessage is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:05 $
 * @since JDK1.1
 */

public class XGridServerMessage {
  public int messageTag;
  public XGridTaskId sourceTid;
  public XGridTaskId destTid;
  public XGridBuffer buffer;

  public XGridServerMessage() {
    messageTag = -1;
    sourceTid = null;
    destTid = null;
    buffer = null;
  }

  public XGridServerMessage(XGridBuffer buf, XGridTaskId dest, XGridTaskId src,
                     int tag) {
    messageTag = tag;
    sourceTid = src;
    destTid = dest;
    buffer = buf;
  }

  public XGridServerMessage(XGridRecvConnection conn) throws XGridException {
    messageTag = -1;
    sourceTid = null;
    destTid = null;
    buffer = null;
    recv(conn);
  }

  public void send(XGridSendConnection conn) throws XGridException {
    DataOutputStream strm = conn.strm;
    try {
      strm.writeInt(messageTag);
      sourceTid.send(strm);
      destTid.send(strm);
      buffer.send(conn);
      strm.flush();
    } catch (IOException ioe) {
      System.out.println("XGridServerMessage, send - i/o exception");
      throw new XGridException("XGridServerMessage, send - " +
              "i/o exception");
    }
  }

  public void recv(XGridRecvConnection conn) throws XGridException {
    DataInputStream strm = conn.strm;
    try {
      messageTag = strm.readInt();
      sourceTid = new XGridTaskId();
      sourceTid.recv(strm);
      destTid = new XGridTaskId();
      destTid.recv(strm);
      buffer = new XGridBuffer();
      buffer.recv(conn);
    } catch (IOException ioe) {
      System.out.println("XGridServerMessage, recv - i/o exception");
      throw new XGridException("XGridServerMessage, recv - " +
              "i/o exception");
    }
  }
}
