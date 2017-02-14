/* jpvmRecvConnection.java
 *
 * The jpvmRecvConnection class implements objects that represent
 * connections to remote jpvm processes from which messages may be
 * received.
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

import java.net.*;
import java.io.*;

/**
 *
 * @version $Revision: 1.3 $, $Date: 2004/01/21 11:14:51 $
 * @author Adam J Ferrari
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class jpvmRecvConnection {
  private InputStream instrm;
  public DataInputStream strm;
  public jpvmTaskId tid;

  public jpvmRecvConnection() {
    instrm = null;
    strm = null;
    tid = null;
  }

  public jpvmRecvConnection(Socket sock) {
    if (sock == null) return;
    try {
      instrm = sock.getInputStream();
      instrm = new BufferedInputStream(instrm);
      strm = new DataInputStream(instrm);
      tid = new jpvmTaskId();
      try {
        tid.recv(strm);
      } catch (jpvmException jpe) {
        strm = null;
        tid = null;
        jpvmDebug.error("jpvmRecvConnection, internal" +
                " error");
      }
      jpvmDebug.note("jpvmRecvConnection, connect to "
              + tid.toString() + " established");
    } catch (IOException ioe) {
      strm = null;
      tid = null;
      jpvmDebug.error("jpvmRecvConnection, i/o exception");
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
        jpvmDebug.error("jpvmRecvConnection, " +
                "hasMessagesQueued - i/o exception");
      }
    }
    return ret;
  }
}


