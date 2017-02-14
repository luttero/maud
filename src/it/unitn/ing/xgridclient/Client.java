/*
 * @(#)Client.java created Mar 29, 2006 Casalino
 *
 * Copyright (c) 1996-2006 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.xgridclient;

import org.beepcore.beep.core.*;
import org.beepcore.beep.lib.NullReplyListener;
import org.beepcore.beep.transport.tcp.TCPSessionCreator;
import base64.Base64;
import it.unitn.ing.rista.util.Misc;

/**
 * The Client is a class for basic management of XGrid tasks
 * <p/>
 * This class as the other were inspired and constructed using as a starting
 * base the com.deadmoo.xgridagent package.
 * For more information on the XGridAgent see
 * http://sourceforge.net/projects/xgridagent-java/
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.3 $, $Date: 2006/07/20 13:39:07 $
 * @since JDK1.1
 */

public class Client {
  // properties of a client
  private final String clientName;

  // the channel to and from the controller
  public Channel channel;

  // hostname and address of this client
  private final String hostname;
  private final String address;
  public String jobIdentifier = null;

  public static final String PREPARED = "Prepared";
  public static final String RUNNING = "Running";
  public static final String FINISHED = "Finished";
  public static final String CANCELED = "Canceled";
  public static final String FAILED = "Failed";
  public static final String DELETED = "Deleted";
  public String jobStatus = "Undefined";
  public String jobResults = null;
  public String task = "0";
  private String password = "rtamaud1";

  /**
   * create a new client
   *
   * @param clientName the name of this client
   * @param hostname   the hostname of the client
   * @param address    the address of the client
   */
  Client(String clientName, String hostname, String address, String host) {
    super();
    this.clientName = clientName;
    this.hostname = hostname;
    this.address = address;
    Session session = null;
    boolean sessionStarted = false;
    int trial = 0;
    while (!sessionStarted) {
      try {
        session = TCPSessionCreator.initiate(host, XGridClient.xgridport);
        sessionStarted = true;
      } catch (BEEPException e) {
        e.printStackTrace();
        System.err.println("xgridclient: Error connecting to " + host + ":" + XGridClient.xgridport + "\n\t" + e.getMessage());
        if (trial < 10) {
          System.err.println("xgridclient: trying again connecting to " + host + ":" + XGridClient.xgridport);
          trial++;
          try {
            Thread.sleep(10000 * trial);
          } catch (InterruptedException ie) {
            ie.printStackTrace(System.err);
          }
        } else
          return;

      }
    }
    // Start a channel for the xgridclient profile
    channel = null;
    try {
//      channel = session.startChannel(XGridClient.PRO_XGRIDCLIENT_URI, new XGridClientRequestHandler(this));
      channel = session.startXGridAuthChannel(XGridClient.PRO_XGRIDCLIENT_URI,
            new XGridClientRequestHandler(this), password);
    } catch (BEEPError e) {
      try {
//        session = XGridPasswordProfile.AuthenticateXGridPassword(session,"rtamaud1");
        channel = session.startXGridAuthChannel(XGridClient.PRO_XGRIDCLIENT_URI,
              new XGridClientRequestHandler(this), password);
      } catch (Exception be) {
        be.printStackTrace();
        if (be instanceof BEEPError && ((BEEPError) be).getCode() == 550) {
          System.err.println("xgridclient: Error host does not support xgridclient profile");
        } else {
          System.err.println("xgridclient: Error starting channel (" + be.getMessage() + ")");
        }
        return;
      }
    } catch (BEEPException e) {
      e.printStackTrace();
      System.err.println("xgridclient: Error starting channel (" + e.getMessage() + ")");
      return;
    }

  }

  /**
   * send a message to the controller
   *
   * @param message the message to send
   */
  public synchronized void sendMSG(final XGridClientMessage message) throws BEEPException {
    boolean sessionStarted = false;
    int trial = 0;
    while (!sessionStarted) {
      try {
        // Uncomment to see messages to controller
//      System.out.println("Message to controller:\n" + message.getStringRepresentation());

        channel.sendMSG(new StringOutputDataStream(message.getStringRepresentation()), NullReplyListener.getListener());
        sessionStarted = true;
      } catch (BEEPException e) {
        System.err.println("client: Error sending request (" + e.getMessage() + ")");
//      System.exit(1);
        if (trial < 5) {
          trial++;
          try {
            Thread.sleep(10000 * trial);
          } catch (InterruptedException ie) {
            ie.printStackTrace(System.err);
          }
        } else
          throw new BEEPException(e);
      }
    }

  }

  // BEGIN getters and setters

  /**
   * get the client's name
   *
   * @return the name
   */
  public String getClientName() {
    return clientName;
  }

  public String getAddress() {
    return address;
  }

  /**
   * get the hostname of the client
   *
   * @return the hostname
   */
  public String getHostname() {
    return hostname;
  }
  // END getters and setters

  public void setRequestHandler() {
//    channel.setRequestHandler(new XGridClientRequestHandler(this));
  }

  public void removeRequestHandler() {
    channel.setRequestHandler(null);
  }

  public void setJobIdentifier(String jobIdentifier) {
    this.jobIdentifier = jobIdentifier;
    System.out.println(jobIdentifier);
  }

  public void subscribe() throws BEEPException {
    XGridClientMessage mes = new XGridClientMessage(1, XGridClientMessage.JOB_ATTRIBUTES, jobIdentifier,
          XGridClientMessage.SUBSCRIBE);
    sendMSG(mes);
  }

  public void status() throws BEEPException {
    XGridClientMessage mes = new XGridClientMessage(1, XGridClientMessage.JOB_ATTRIBUTES, jobIdentifier,
          XGridClientMessage.STATUS);
    sendMSG(mes);
  }

  public void setStatus(String jobStatus) {
    this.jobStatus = jobStatus;
  }

  public void askForResults() throws BEEPException {
    XGridClientMessage mes = new XGridClientMessage(2, XGridClientMessage.JOB_RESULTS, jobIdentifier,
          XGridClientMessage.RESULTS);
    sendMSG(mes);
  }

  public void setResults(String jobResults) {
    this.jobResults = jobResults;
  }

  public void removeTask() throws BEEPException {
    XGridClientMessage mes = new XGridClientMessage(1, XGridClientMessage.JOB_CONTROL, jobIdentifier,
          XGridClientMessage.REMOVE);
    sendMSG(mes);
  }

  public String submitJobAndWait(String title, String[] filenames, String[] filesBase64,
                                 String command, String[] arguments) throws BEEPException {
    XGridClientMessage mes = new XGridClientMessage(0, title, task, filenames, filesBase64, command, arguments);
    sendMSG(mes);
    setRequestHandler();
    while (jobIdentifier == null) {
      try {
        Thread.sleep(500);
      } catch (InterruptedException e) {
        break;
      }
    }
    subscribe();
    while (!jobStatus.equals(FINISHED) && !jobStatus.equals(CANCELED) && !jobStatus.equals(FAILED)
          && !jobStatus.equals(DELETED)) {
      try {
        Thread.sleep(500);
      } catch (InterruptedException e) {
        break;
      }
    }
    if (jobStatus.equals(DELETED)) {
      if (XGridClient.removeAll) {
        removeRequestHandler();
        closeChannel();
      }
      return CANCELED;
    }
    if (jobStatus.equals(CANCELED)) {
      removeTask();
      if (XGridClient.removeAll) {
        removeRequestHandler();
        closeChannel();
      }
      return CANCELED;
    }
    if (jobStatus.equals(FAILED)) {
      removeTask();
      if (XGridClient.removeAll) {
        removeRequestHandler();
        closeChannel();
      }
      return FAILED;
    }
    askForResults();
    while (jobResults == null) {
      try {
        Thread.sleep(500);
      } catch (InterruptedException e) {
        break;
      }
    }
    removeTask();
/*    while (!jobStatus.equals(CANCELED)) {
      try {
        Thread.sleep(300);
      } catch (InterruptedException e) {
        break;
      }
    }*/
    if (XGridClient.removeAll) {
      removeRequestHandler();
      closeChannel();
    }
    return new String(Base64.decode(jobResults));
  }

  public String submitJob(String title, String[] filenames, String[] filesBase64,
                          String command, String[] arguments) throws BEEPException {
    XGridClientMessage mes = new XGridClientMessage(0, title, task, filenames, filesBase64, command, arguments);
    sendMSG(mes);
    setRequestHandler();
    while (jobIdentifier == null) {
      try {
//        System.out.println(jobIdentifier);
        Thread.sleep(500);
      } catch (InterruptedException e) {
        break;
      }
    }
    if (XGridClient.removeAll) {
      removeRequestHandler();
      closeChannel();
    }
    return jobIdentifier;
  }

  public String getResults(String jobId) throws BEEPException {
//    System.out.println("Client:" + this);
    jobIdentifier = jobId;
    status();
    setRequestHandler();
    while (!jobStatus.equals(FINISHED) && !jobStatus.equals(CANCELED) && !jobStatus.equals(FAILED)
          && !jobStatus.equals(DELETED)) {
      status();
      try {
        Thread.sleep(500);
      } catch (InterruptedException e) {
        break;
      }
    }
    if (jobStatus.equals(DELETED)) {
      if (XGridClient.removeAll) {
        removeRequestHandler();
        closeChannel();
      }
      return CANCELED;
    }
    if (jobStatus.equals(CANCELED)) {
      removeTask();
      if (XGridClient.removeAll) {
        removeRequestHandler();
        closeChannel();
      }
      return CANCELED;
    }
    if (jobStatus.equals(FAILED)) {
      removeTask();
      if (XGridClient.removeAll) {
        removeRequestHandler();
        closeChannel();
      }
      return FAILED;
    }
    askForResults();
    while (jobResults == null) {
      try {
        Thread.sleep(500);
      } catch (InterruptedException e) {
        break;
      }
    }
    removeTask();
/*    while (!jobStatus.equals(CANCELED)) {
      try {
        Thread.sleep(300);
      } catch (InterruptedException e) {
        break;
      }
    }*/
    if (XGridClient.removeAll) {
      removeRequestHandler();
      closeChannel();
    }
    return new String(Base64.decode(jobResults));
  }

  public void closeChannel() {
    (new Thread() {
      public void run() {
        // wait a little before to close
        try {
          Thread.sleep(500);
        } catch (InterruptedException e) {
        }
        try {
          channel.getSession().close();
        } catch (BEEPException e) {
          e.printStackTrace();
        }
        System.out.println("Channel and session closed.");
      }
    }).start();
  }

}
