/*
 * @(#)XGridConnectionSet.java created Feb 17, 2006 Newark Liberty Int. Airport
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


/**
 * The XGridConnectionSet is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:05 $
 * @since JDK1.1
 */

public class XGridConnectionSet {

  private XGridRecvConnectionListNode recvList;
  private XGridSendConnectionListNode sendList;
  private XGridRecvConnectionListNode recvListIter;

  XGridConnectionSet() {
    recvList = null;
    sendList = null;
  }

  public synchronized XGridRecvConnection
          lookupRecvConnection(XGridTaskId tid) {
    XGridRecvConnectionListNode tmp = recvList;
    while (tmp != null) {
      if (tmp.conn.tid.equals(tid))
        return tmp.conn;
      tmp = tmp.next;
    }
    return null;
  }

  public synchronized void insertRecvConnection(XGridRecvConnection c) {
    XGridRecvConnectionListNode nw;
    nw = new XGridRecvConnectionListNode(c);
    nw.next = recvList;
    recvList = nw;
  }

  public synchronized XGridRecvConnection
          firstIterRecv() {
    recvListIter = recvList;
    if (recvListIter != null)
      return recvListIter.conn;
    return null;
  }

  public synchronized XGridRecvConnection
          nextIterRecv() {
    if (recvListIter != null)
      recvListIter = recvListIter.next;
    if (recvListIter != null)
      return recvListIter.conn;
    return null;
  }

  public synchronized XGridSendConnection
          lookupSendConnection(XGridTaskId tid) {
    XGridSendConnectionListNode tmp = sendList;
    while (tmp != null) {
      if (tmp.conn.tid.equals(tid))
        return tmp.conn;
      tmp = tmp.next;
    }
    return null;
  }

  public synchronized void insertSendConnection(XGridSendConnection c) {
    XGridSendConnectionListNode nw;
    nw = new XGridSendConnectionListNode(c);
    nw.next = sendList;
    sendList = nw;
  }

}

class XGridSendConnectionListNode {
  public XGridSendConnection conn;
  public XGridSendConnectionListNode next;

  public XGridSendConnectionListNode() {
    conn = null;
    next = null;
  }

  public XGridSendConnectionListNode(XGridSendConnection c) {
    conn = c;
    next = null;
  }
}


class XGridRecvConnectionListNode {
  public XGridRecvConnection conn;
  public XGridRecvConnectionListNode next;

  public XGridRecvConnectionListNode() {
    conn = null;
    next = null;
  }

  public XGridRecvConnectionListNode(XGridRecvConnection c) {
    conn = c;
    next = null;
  }
}
