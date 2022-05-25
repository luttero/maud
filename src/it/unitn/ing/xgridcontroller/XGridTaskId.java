/*
 * @(#)XGridTaskId.java created Feb 17, 2006 Newark Liberty Int. Airport
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
import java.net.InetAddress;
import java.net.UnknownHostException;


/**
 * The XGridTaskId is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:05 $
 * @since JDK1.1
 */

public class XGridTaskId implements Serializable {
  private String taskHost;
  private int taskConnectPort;

  public XGridTaskId() {
    taskHost = null;
    taskConnectPort = 0;
  };

  public XGridTaskId(int my_port) {
    taskHost = null;
    taskConnectPort = 0;
    try {
      InetAddress taskAddr = InetAddress.getLocalHost();
      taskHost = taskAddr.getCanonicalHostName();
      taskConnectPort = my_port;
    } catch (UnknownHostException uhe) {
      System.err.println("XGridTaskId, unknown host exception");
    }
  }

  public XGridTaskId(String host, int port) {
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

  public boolean equals(XGridTaskId tid) {
    if (tid == null)
      return false;
    if (taskConnectPort != tid.taskConnectPort)
      return false;
    if (tid.taskHost == null)
      return false;
    boolean ret = tid.taskHost.equalsIgnoreCase(taskHost);
    return ret;
  }

  public void send(DataOutputStream strm) throws XGridException {
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
      System.out.println("XGridTaskId, send - i/o exception");
      throw new XGridException("XGridTaskId, send - i/o exception");
    }
  }

  public void recv(DataInputStream strm) throws XGridException {
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
      System.out.println("XGridTaskId, recv - i/o exception");
      throw new XGridException("XGridTaskId, recv - i/o exception");
    }
  }
}
