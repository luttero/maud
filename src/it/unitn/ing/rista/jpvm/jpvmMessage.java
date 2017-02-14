/* jpvmMessage
 *
 * A class representing a message in the jpvm system. Messages
 * can be either sent or received to or from other jpvm tasks.
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

import java.io.*;

/**
 *
 * @version $Revision: 1.3 $, $Date: 2004/01/21 11:14:51 $
 * @author Adam J Ferrari
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class jpvmMessage {
  public int messageTag;
  public jpvmTaskId sourceTid;
  public jpvmTaskId destTid;
  public jpvmBuffer buffer;

  public jpvmMessage() {
    messageTag = -1;
    sourceTid = null;
    destTid = null;
    buffer = null;
  }

  public jpvmMessage(jpvmBuffer buf, jpvmTaskId dest, jpvmTaskId src,
                     int tag) {
    messageTag = tag;
    sourceTid = src;
    destTid = dest;
    buffer = buf;
  }

  public jpvmMessage(jpvmRecvConnection conn) throws jpvmException {
    messageTag = -1;
    sourceTid = null;
    destTid = null;
    buffer = null;
    recv(conn);
  }

  public void send(jpvmSendConnection conn) throws jpvmException {
    DataOutputStream strm = conn.strm;
    try {
      strm.writeInt(messageTag);
      sourceTid.send(strm);
      destTid.send(strm);
      buffer.send(conn);
      strm.flush();
    } catch (IOException ioe) {
      jpvmDebug.note("jpvmMessage, send - i/o exception");
      throw new jpvmException("jpvmMessage, send - " +
              "i/o exception");
    }
  }

  public void recv(jpvmRecvConnection conn) throws jpvmException {
    DataInputStream strm = conn.strm;
    try {
      messageTag = strm.readInt();
      sourceTid = new jpvmTaskId();
      sourceTid.recv(strm);
      destTid = new jpvmTaskId();
      destTid.recv(strm);
      buffer = new jpvmBuffer();
      buffer.recv(conn);
    } catch (IOException ioe) {
      jpvmDebug.note("jpvmMessage, recv - i/o exception");
      throw new jpvmException("jpvmMessage, recv - " +
              "i/o exception");
    }
  }
}

