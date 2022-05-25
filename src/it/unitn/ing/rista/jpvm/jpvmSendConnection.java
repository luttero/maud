/* jpvmSendConnection.java
 *
 * The jpvmSendConnection class implements objects that represent
 * connections to remote jpvm processes to which messages may be
 * sent.
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

public class jpvmSendConnection {
  public DataOutputStream strm;
  public jpvmTaskId tid;

  public jpvmSendConnection() {
    strm = null;
    tid = null;
  }

  public jpvmSendConnection(Socket sock, jpvmTaskId t) {
    if (sock == null || t == null) return;
    tid = t;
    try {
      OutputStream outstrm = sock.getOutputStream();
      outstrm = new BufferedOutputStream(outstrm);
      strm = new DataOutputStream(outstrm);
    } catch (IOException ioe) {
      strm = null;
      tid = null;
      jpvmDebug.error("jpvmSendConnection, i/o exception");
    }
  }

  public static jpvmSendConnection connect(jpvmTaskId t, jpvmTaskId f)
          throws jpvmException {
    jpvmSendConnection ret = null;
    try {
      jpvmDebug.note("jpvmSendConnection, " +
              "connecting to " + t.toString());

      // Make the new connection...
      Socket sock = new Socket(t.getHost(), t.getPort());
      ret = new jpvmSendConnection(sock, t);

      // Send my identity to the newly connected task...
      f.send(ret.strm);
      ret.strm.flush();
    } catch (IOException ioe) {
      jpvmDebug.note("jpvmSendConnection, connect - " +
              " i/o exception");
      throw new jpvmException("jpvmSendConnection, connect - "
              + " i/o exception: \"" + ioe + "\"");
    }
    return ret;
  }
}

