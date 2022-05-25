/*
 * @(#)Server.java created Feb 17, 2006 Newark
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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.beepcore.beep.transport.tcp.TCPSessionCreator;
import org.beepcore.beep.core.*;
import org.beepcore.beep.profile.ProfileConfiguration;

import java.net.ServerSocket;
import java.net.Socket;
import java.io.IOException;


/**
 * The Server is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:05 $
 * @since JDK1.1
 */

public class XGridConnectionServer extends Thread {

  private Log log = LogFactory.getLog(this.getClass());
  private int connectionPort;
  ProfileRegistry reg;
  public boolean mustRun = true;

  private ServerSocket connectionSock;
  private XGridConnectionSet connectionSet;
  private XGridQueue queue;

  public XGridConnectionServer(/*XGridConnectionSet c, XGridQueue q,*/ int dport) {
    connectionPort = dport;

    reg = new ProfileRegistry();

    XGridControllerProfile echoServer = new XGridControllerProfile();
    SessionTuningProperties tuningServer = new SessionTuningProperties();
    ProfileConfiguration profileConfServer = new ProfileConfiguration();
    XGridClientProfile echoClient = new XGridClientProfile();
    SessionTuningProperties tuningClient = new SessionTuningProperties();
    ProfileConfiguration profileConfClient = new ProfileConfiguration();
    XGridAgentProfile echoAgent = new XGridAgentProfile();
    SessionTuningProperties tuningAgent = new SessionTuningProperties();
    ProfileConfiguration profileConfAgent = new ProfileConfiguration();
    try {
      reg.addStartChannelListener(echoServer.URI,
                                  echoServer.init(echoServer.URI, profileConfServer), tuningServer);
      reg.addStartChannelListener(echoAgent.URI,
                                  echoAgent.init(echoAgent.URI, profileConfAgent), tuningAgent);
      reg.addStartChannelListener(echoClient.URI,
                                  echoClient.init(echoClient.URI, profileConfClient), tuningClient);
    } catch (BEEPException e) {
      e.printStackTrace();
    }

/*    connectionSet = c;
    connectionSock = null;
    queue = q;
    try {
      connectionSock = new ServerSocket(dport);
      connectionPort = connectionSock.getLocalPort();
    } catch (IOException ioe) {
      System.out.println("XGrid Server, i/o exception");
    }*/
  }

  public int getConnectionPort() {
    return connectionPort;
  }

  public void run() {
    try {
        // Loop listening for new Sessions
        while (mustRun) {
            TCPSessionCreator.listen(connectionPort, reg);
          Thread.sleep(100);
        }
    } catch (Exception e) {
        log.error("Listener exiting", e);
    }
/*    while (true) {
      try {
        System.out.println("XGridConnectionServer, blocking on port " +
                connectionSock.getLocalPort());
        Socket newConnSock = connectionSock.accept();
        System.out.println("XGridConnectionServer, new connection.");
        XGridRecvConnection nw = new XGridRecvConnection(newConnSock);
        connectionSet.insertRecvConnection(nw);

        // Start a thread to recv on this pipe
        XGridRecvThread rt = new XGridRecvThread(nw, queue);
        rt.start();
      } catch (IOException ioe) {
        System.out.println("XGridConnectionServer, run - " +
                "i/o exception");
      }
    }*/
  }


}
