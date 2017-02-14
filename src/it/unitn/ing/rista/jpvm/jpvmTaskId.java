/* jpvmTaskId.java
 *
 * Implements the unique jpvm task identifier class.
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
 * @version $Revision: 1.3 $, $Date: 2004/01/21 11:14:51 $
 * @author Adam J Ferrari
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class jpvmTaskId implements Serializable {
  private String taskHost;
  private int taskConnectPort;

  public jpvmTaskId() {
    taskHost = null;
    taskConnectPort = 0;
  };

  public jpvmTaskId(int my_port) {
    taskHost = null;
    taskConnectPort = 0;
    try {
      InetAddress taskAddr = InetAddress.getLocalHost();
      taskHost = taskAddr.getCanonicalHostName();
      taskConnectPort = my_port;
    } catch (UnknownHostException uhe) {
      jpvmDebug.error("jpvmTaskId, unknown host exception");
    }
  }

  public jpvmTaskId(String host, int port) {
    taskHost = new String(host);
    taskConnectPort = port;
  }

  public String getHost() {
    return taskHost;
  }

  public int getPort() {
    return taskConnectPort;
  }

  public String toString() {
    return ((taskHost != null ? taskHost : "(null)") +
            ", port #" + taskConnectPort);
  }

  public boolean equals(jpvmTaskId tid) {
    if (tid == null)
      return false;
    if (taskConnectPort != tid.taskConnectPort)
      return false;
    if (tid.taskHost == null)
      return false;
    boolean ret = tid.taskHost.equalsIgnoreCase(taskHost);
    return ret;
  }

  public void send(DataOutputStream strm) throws jpvmException {
    int i;
    try {
      int len = 0;
      if (taskHost != null) {
        len = taskHost.length();
        strm.writeInt(len);
        char hname[] = new char[len];
        taskHost.getChars(0, len, hname, 0);
        for (i = 0; i < len; i++) {
          strm.writeChar(hname[i]);
        }
      } else {
        strm.writeInt(len);
      }
      strm.writeInt(taskConnectPort);
    } catch (IOException ioe) {
      jpvmDebug.note("jpvmTaskId, send - i/o exception");
      throw new jpvmException("jpvmTaskId, send - i/o exception");
    }
  }

  public void recv(DataInputStream strm) throws jpvmException {
    int i;
    try {
      int len = strm.readInt();
      if (len > 0) {
        char hname[] = new char[len];
        for (i = 0; i < len; i++) {
          hname[i] = strm.readChar();
        }
        taskHost = new String(hname);
      } else {
        taskHost = null;
      }
      taskConnectPort = strm.readInt();
    } catch (IOException ioe) {
      jpvmDebug.note("jpvmTaskId, recv - i/o exception");
      throw new jpvmException("jpvmTaskId, recv - i/o exception");
    }
  }
}

