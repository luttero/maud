/* jpvmConnectionServer.java
 *
 * The jpvmConnectionServer class implements the thread of control
 * in each jpvm program that establishes connections with other
 * jpvm tasks that want to send data.
 *
 * Adam J Ferrari
 * Sat 05-25-1996
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
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:58 $
 * @author Adam J Ferrari
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class jpvmConnectionServer extends Thread {
  private ServerSocket connectionSock;
  private int connectionPort;
  private jpvmConnectionSet connectionSet;
  private jpvmMessageQueue queue;

  public jpvmConnectionServer(jpvmConnectionSet c, jpvmMessageQueue q, int dport) {
    connectionSet = c;
    connectionSock = null;
    connectionPort = 0;
    queue = q;
    try {
      connectionSock = new ServerSocket(dport);
      connectionPort = connectionSock.getLocalPort();
    } catch (IOException ioe) {
      jpvmDebug.error("jpvmConnectionServer, i/o exception");
    }
  }

  public int getConnectionPort() {
    return connectionPort;
  }

  public void run() {
    while (true) {
      try {
        jpvmDebug.note("jpvmConnectionServer, blocking on port " +
                connectionSock.getLocalPort());
        Socket newConnSock = connectionSock.accept();
        jpvmDebug.note("jpvmConnectionServer, new connection.");
        jpvmRecvConnection nw = new jpvmRecvConnection(newConnSock);
        connectionSet.insertRecvConnection(nw);

        // Start a thread to recv on this pipe
        jpvmRecvThread rt = new jpvmRecvThread(nw, queue);
        rt.start();
      } catch (IOException ioe) {
        jpvmDebug.error("jpvmConnectionServer, run - " +
                "i/o exception");
      }
    }
  }
}

