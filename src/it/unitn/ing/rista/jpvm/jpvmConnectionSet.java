/* jpvmConnectionSet.java
 *
 * A class to manage the storage of a set of send and receive
 * connections to other jpvm tasks.
 *
 * Adam J Ferrari
 * Sun 05-26-1996
 *
 * modified by
 * Luca Lutterotti
 * Verona, 12 November 1999
 *
 * Copyright (C) 1996  Adam J Ferrari, 1999 Luca Lutterotti
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
 * MA 02139, USA.
 */

package it.unitn.ing.rista.jpvm;

/**
 *
 * @version $Revision: 1.3 $, $Date: 2004/01/21 11:14:51 $
 * @author Adam J Ferrari
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class jpvmConnectionSet {
  private jpvmRecvConnectionListNode recvList;
  private jpvmSendConnectionListNode sendList;
  private jpvmRecvConnectionListNode recvListIter;

  jpvmConnectionSet() {
    recvList = null;
    sendList = null;
  }

  public synchronized jpvmRecvConnection
          lookupRecvConnection(jpvmTaskId tid) {
    jpvmRecvConnectionListNode tmp = recvList;
    while (tmp != null) {
      if (tmp.conn.tid.equals(tid))
        return tmp.conn;
      tmp = tmp.next;
    }
    return null;
  }

  public synchronized void insertRecvConnection(jpvmRecvConnection c) {
    jpvmRecvConnectionListNode nw;
    nw = new jpvmRecvConnectionListNode(c);
    nw.next = recvList;
    recvList = nw;
  }

  public synchronized jpvmRecvConnection
          firstIterRecv() {
    recvListIter = recvList;
    if (recvListIter != null)
      return recvListIter.conn;
    return null;
  }

  public synchronized jpvmRecvConnection
          nextIterRecv() {
    if (recvListIter != null)
      recvListIter = recvListIter.next;
    if (recvListIter != null)
      return recvListIter.conn;
    return null;
  }

  public synchronized jpvmSendConnection
          lookupSendConnection(jpvmTaskId tid) {
    jpvmSendConnectionListNode tmp = sendList;
    while (tmp != null) {
      if (tmp.conn.tid.equals(tid))
        return tmp.conn;
      tmp = tmp.next;
    }
    return null;
  }

  public synchronized void insertSendConnection(jpvmSendConnection c) {
    jpvmSendConnectionListNode nw;
    nw = new jpvmSendConnectionListNode(c);
    nw.next = sendList;
    sendList = nw;
  }

}

class jpvmSendConnectionListNode {
  public jpvmSendConnection conn;
  public jpvmSendConnectionListNode next;

  public jpvmSendConnectionListNode() {
    conn = null;
    next = null;
  }

  public jpvmSendConnectionListNode(jpvmSendConnection c) {
    conn = c;
    next = null;
  }
}


class jpvmRecvConnectionListNode {
  public jpvmRecvConnection conn;
  public jpvmRecvConnectionListNode next;

  public jpvmRecvConnectionListNode() {
    conn = null;
    next = null;
  }

  public jpvmRecvConnectionListNode(jpvmRecvConnection c) {
    conn = c;
    next = null;
  }
}


