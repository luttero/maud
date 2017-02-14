/*
 * @(#)XGridAgentProfile.java created Feb 18, 2006 Atlantic ocean, somewhere
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

import org.beepcore.beep.profile.Profile;
import org.beepcore.beep.profile.ProfileConfiguration;
import org.beepcore.beep.core.*;
import org.beepcore.beep.util.BufferSegment;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.*;
import org.xml.sax.SAXException;

import javax.xml.parsers.*;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.HashMap;

import com.deadmoo.xgridagent.*;
import it.unitn.ing.rista.util.Misc;


/**
 * The XGridAgentProfile is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:05 $
 * @since JDK1.1
 */

public class XGridAgentProfile implements Profile, StartChannelListener, RequestHandler
{

  public static final String URI = XGridAgent.PRO_XGRIDAGENT_URI;
//      "http://www.apple.com/beep/xgrid/controller/agent";

  private Log log = LogFactory.getLog(this.getClass());

  public StartChannelListener init(String uri, ProfileConfiguration config)
      throws BEEPException
  {
      return this;
  }

  public void startChannel(Channel channel, String encoding, String data)
          throws StartChannelException
  {
      log.debug("XGridController StartChannel Callback");
      channel.setRequestHandler(this);
  }

  public void closeChannel(Channel channel) throws CloseChannelException
  {
      log.debug("XGridController CloseChannel Callback");
      channel.setRequestHandler(null);
  }

  public boolean advertiseProfile(Session session)
  {
      return true;
  }

  public void receiveMSG(MessageMSG message)
  {
    StringBuffer strBuf = new StringBuffer("");
       InputDataStream ds = message.getDataStream();

      while (true) {
          try {
              BufferSegment b = ds.waitForNextSegment();
              if (b == null) {
                  break;
              }
            strBuf.append(new String(b.getData()));
          } catch (InterruptedException e) {
              message.getChannel().getSession().terminate(e.getMessage());
              return;
          }
      }

    String newMessage = strBuf.toString().trim();

    // Uncomment to print out the messages from the controller
    System.out.println("Message from agent:\n" + newMessage);

    final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    try {
      final DocumentBuilder builder = factory.newDocumentBuilder();
      Document document = null;
      // get rid of DOCTYPE
      newMessage = newMessage.replaceAll("<!DOCTYPE.*>\n", "");
      // get rid of some characters that Apple puts in the XML
      newMessage = newMessage.replaceAll(XGridRequestHandler.hiddenChar1, "");
      newMessage = newMessage.replaceAll(XGridRequestHandler.hiddenChar2, "");

      document = builder.parse(new ByteArrayInputStream(newMessage.getBytes()));
      final NodeList nodes = document.getElementsByTagName("dict");
      final Element rootDict = (Element) nodes.item(0);
      final String identifier = XGridRequestHandler.getStringForKey(rootDict, XGridMessage.IDENTIFIER);
      long id;
      if (identifier != null && identifier.compareTo("") != 0) {
        id = Long.parseLong(identifier);
      } else {
        id = -1;
      }
      final String name = XGridRequestHandler.getStringForKey(rootDict, XGridMessage.NAME);
      final Element payloadDict = XGridRequestHandler.getDictForKey(rootDict, XGridMessage.PAYLOAD);
      final String type = XGridRequestHandler.getStringForKey(rootDict, XGridMessage.TYPE);

      sendRPY(message);

      if (type.compareTo(XGridMessage.REQUEST) == 0) {
        if (name.compareTo(XGridMessage.AGENT_REGISTRATION) == 0) {
/*          final String errorMessage = XGridRequestHandler.getStringForKey(payloadDict, XGridMessage.ERROR);
          if (errorMessage != null) {
            System.err.println("xgridrequesthandler: Error message from controller \n\t" + errorMessage);
            System.exit(1);
          } */
//          agent.setAgentCookie(XGridRequestHandler.getStringForKey(payloadDict, XGridMessage.AGENT_COOKIE));
        }
      } /* else if (type.compareTo(XGridMessage.REQUEST) == 0) {
        if (name.compareTo(XGridMessage.AGENT_STATUS) == 0) {
          agent.sendMSG(new XGridMessage(id, name, XGridMessage.REPLY, agent));
        } else if (name.compareTo(XGridMessage.TASK_SUBMISSION) == 0) {
          final String[] arguments = XGridRequestHandler.getArrayForKey(payloadDict, XGridMessage.ARGUMENTS);
          final String command = XGridRequestHandler.getStringForKey(payloadDict, XGridMessage.COMMAND);
          final Element inputFilesElement = XGridRequestHandler.getDictForKey(payloadDict, XGridMessage.INPUT_FILES);
          final HashMap inputFiles = XGridRequestHandler.getInputFiles(inputFilesElement);
          final String inputStream = XGridRequestHandler.getStringForKey(payloadDict, XGridMessage.INPUT_STREAM);
          final Task task = agent.newTask(command, arguments, inputFiles, inputStream);
          agent.sendMSG(new XGridMessage(id, name, XGridMessage.REPLY, task));
          agent.sendMSG(new XGridMessage(-1, XGridMessage.AGENT_STATUS, XGridMessage.NOTIFICATION, agent));
        } else if (name.compareTo(XGridMessage.TASK_CONTROL) == 0) {
          final String control = XGridRequestHandler.getStringForKey(payloadDict, XGridMessage.CONTROL);
          final String taskRef = XGridRequestHandler.getStringForKey(payloadDict, XGridMessage.TASK_REF);
          final Task task = agent.getTask(Long.parseLong(taskRef));
          if (task != null) {
            agent.sendMSG(new XGridMessage(id, name, XGridMessage.REPLY, task));
            if (control.compareTo(XGridMessage.START) == 0) {
              task.start();
            } else if (control.compareTo(XGridMessage.CANCEL) == 0) {
              agent.removeTask(Long.parseLong(taskRef));
              agent.sendMSG(new XGridMessage(-1, XGridMessage.AGENT_STATUS, XGridMessage.NOTIFICATION, agent));
            }
          }
        } else if (name.compareTo(XGridMessage.TASK_RESULTS) == 0) {
          final String taskRef = XGridRequestHandler.getStringForKey(payloadDict, XGridMessage.TASK_REF);
          final Task task = agent.getTask(Long.parseLong(taskRef));
          if (task != null) {
            agent.sendMSG(new XGridMessage(id, name, XGridMessage.REPLY, task));
          }
        } else if (name.compareTo(XGridMessage.TASK_FINAL_RESULTS) == 0) {
          final String taskRef = XGridRequestHandler.getStringForKey(payloadDict, XGridMessage.TASK_REF);
          final Task task = agent.getTask(Long.parseLong(taskRef));
          if (task != null) {
            agent.sendMSG(new XGridMessage(id, name, XGridMessage.REPLY, task));
          }
        }
      }*/
    } catch (ParserConfigurationException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    } catch (SAXException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    } catch (IOException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  public void sendRPY(final MessageMSG message) {
    try {
      message.sendRPY(new StringOutputDataStream(""));
    } catch (BEEPException e) {
      try {
        message.sendERR(BEEPError.CODE_REQUESTED_ACTION_ABORTED, "Error sending RPY");
      } catch (BEEPException x) {
        x.printStackTrace();
      }
    }
  }

}
