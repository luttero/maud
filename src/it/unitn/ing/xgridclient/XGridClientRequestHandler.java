/*
 * @(#)XGridClientRequestHandler.java created Mar 29, 2006 Casalino
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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.HashMap;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.beepcore.beep.core.*;
import org.beepcore.beep.util.BufferSegment;
import org.w3c.dom.CharacterData;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * The XGridClientRequestHandler handles the requests and messaging
 *
 * This class as the other were inspired and constructed using as a starting
 * base the com.deadmoo.xgridagent package.
 * For more information on the XGridAgent see
 * http://sourceforge.net/projects/xgridagent-java/
 *
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.3 $, $Date: 2006/07/20 13:39:07 $
 * @since JDK1.1
 */

public class XGridClientRequestHandler implements RequestHandler {

  private final Client client;
  // characters that Apple puts in the XML
  static char charValue1[] = {0x09};
  static char charValue2[] = {0x0A};
  public static final String hiddenChar1 = new String(charValue1);
  public static final String hiddenChar2 = new String(charValue2);;

  XGridClientRequestHandler(Client client) {
    super();
    this.client = client;

    // characters that Apple puts in the XML
  }

  public synchronized void receiveMSG(final MessageMSG message) {
    StringBuffer strBuf = new StringBuffer("");
    final InputDataStream ds = message.getDataStream();

    while (true) {
      try {
        final BufferSegment b = ds.waitForNextSegment();
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
//    System.out.println("Client:" + client + " Message from controller:\n" + newMessage);

    final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    try {
      final DocumentBuilder builder = factory.newDocumentBuilder();
      Document document = null;
      // get rid of DOCTYPE
      newMessage = newMessage.replaceAll("<!DOCTYPE.*>\n", "");
      // get rid of some characters that Apple puts in the XML
      newMessage = newMessage.replaceAll(hiddenChar1, "");
      newMessage = newMessage.replaceAll(hiddenChar2, "");

      document = builder.parse(new ByteArrayInputStream(newMessage.getBytes()));
      final NodeList nodes = document.getElementsByTagName("dict");
      final Element rootDict = (Element) nodes.item(0);
      final String identifier = getStringForKey(rootDict, XGridClientMessage.IDENTIFIER);
      long id;
      if (identifier != null && identifier.compareTo("") != 0) {
        id = Long.parseLong(identifier);
      } else {
        id = -1;
      }
      final String name = getStringForKey(rootDict, XGridClientMessage.NAME);
      final Element payloadDict = getDictForKey(rootDict, XGridClientMessage.PAYLOAD);
      final String type = getStringForKey(rootDict, XGridClientMessage.TYPE);

      sendRPY(message);

      if(type.equals(XGridClientMessage.REPLY)) {
        if (name.equals(XGridClientMessage.JOB_SUBMISSION)) {
          final String jobIdentifier = getStringForKey(payloadDict, XGridClientMessage.JOB_IDENTIFIER);
          client.setJobIdentifier(jobIdentifier);
        } else if (name.equals(XGridClientMessage.JOB_RESULTS)) {
          final Element resultsDict = getDictForKey(payloadDict, XGridClientMessage.TASK_RESULTS);
          final Element taskDict = getDictForKey(resultsDict, client.task);
          final String jobResults = getDataForKey(taskDict, XGridClientMessage.READ_DATA);
          client.setResults(jobResults);
        } else if (name.equals(XGridClientMessage.JOB_ATTRIBUTES)) {
          final Element attributesDict = getDictForKey(payloadDict, XGridClientMessage.JOB_ATTRIBUTES);
          if (attributesDict == null)
            client.setStatus(Client.DELETED);
          else {
            final String jobStatus = getStringForKey(attributesDict, XGridClientMessage.JOB_STATUS);
            client.setStatus(jobStatus);
          }
        } else if (name.equals(XGridClientMessage.JOB_CONTROL)) {
          if (!payloadDict.hasChildNodes())
            client.setStatus(Client.CANCELED);
        }
      } else if (type.equals(XGridClientMessage.NOTIFICATION)) {
        if (name.equals(XGridClientMessage.JOB_ATTRIBUTES)) {
          final Element attributesDict = getDictForKey(payloadDict, XGridClientMessage.JOB_ATTRIBUTES);
          final String jobStatus = getStringForKey(attributesDict, XGridClientMessage.JOB_STATUS);
          client.setStatus(jobStatus);
        }
      } else if(type.equals(XGridClientMessage.REQUEST)) {
      }
    } catch (ParserConfigurationException e) {
      e.printStackTrace();
    } catch (SAXException e) {
      e.printStackTrace();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private void sendRPY(final Message message) {
    try {
      message.sendRPY(new StringOutputDataStream(""));
    } catch (BEEPException e) {
      try {
        message.sendERR(BEEPError.CODE_REQUESTED_ACTION_ABORTED, "Error sending RPY");
      } catch (BEEPException x) {
        message.getChannel().getSession().terminate(x.getMessage());
      }
    }
  }

  // get the input files
  private static HashMap getInputFiles(final Element element) {
    final HashMap inputFiles = new HashMap();
    if (element != null) {
      final NodeList nodes = element.getElementsByTagName("key");
      for (int i = 0; i < nodes.getLength(); i++) {
        final Element keyElement = (Element) nodes.item(i);
        final String keyName = getStringFromElement(keyElement);
        final Element dictElement = (Element) keyElement.getNextSibling();
        if (dictElement.getTagName().compareTo("dict") == 0) {
          final String fileData = getDataForKey(dictElement, XGridClientMessage.FILE_DATA);
          final String isExecutable = getStringForKey(dictElement, XGridClientMessage.IS_EXECUTABLE);
          if (fileData != null) {
            final HashMap file = new HashMap();
            file.put(XGridClientMessage.FILE_DATA, fileData);
            if (isExecutable != null) {
              file.put(XGridClientMessage.IS_EXECUTABLE, isExecutable);
            }
            inputFiles.put(keyName, file);
          }
        }
      }
    }
    return inputFiles;
  }

  // get the dict for a given key
  private static Element getDictForKey(final Element element, final String searchKey) {
    final NodeList nodes = element.getElementsByTagName("key");
    for (int i = 0; i < nodes.getLength(); i++) {
      final Element keyElement = (Element) nodes.item(i);
      final String keyName = getStringFromElement(keyElement);
      if (keyName.compareTo(searchKey) == 0) {
        final Element dictElement = (Element) keyElement.getNextSibling();
        if (dictElement.getTagName().compareTo("dict") == 0) {
          return dictElement;
        } else {
          return null;
        }
      }
    }
    return null;
  }

  // get the string for a given key
  private static String getStringForKey(final Element element, final String searchKey) {
    final NodeList nodes = element.getElementsByTagName("key");
    for (int i = 0; i < nodes.getLength(); i++) {
      final Element keyElement = (Element) nodes.item(i);
      final String keyName = getStringFromElement(keyElement);
      if (keyName.compareTo(searchKey) == 0) {
        final Element stringElement = (Element) keyElement.getNextSibling();
        if (stringElement.getTagName().compareTo("string") == 0) {
          return getStringFromElement(stringElement);
        } else {
          return null;
        }
      }
    }
    return null;
  }

  // get the string for a given key
  private static String getDataForKey(final Element element, final String searchKey) {
    final NodeList nodes = element.getElementsByTagName("key");
    for (int i = 0; i < nodes.getLength(); i++) {
      final Element keyElement = (Element) nodes.item(i);
      final String keyName = getStringFromElement(keyElement);
      if (keyName.compareTo(searchKey) == 0) {
        final Element stringElement = (Element) keyElement.getNextSibling();
        if (stringElement.getTagName().compareTo("data") == 0) {
          return getStringFromElement(stringElement);
        } else {
          return null;
        }
      }
    }
    return null;
  }

  // get the string for a given key
  private static String[] getArrayForKey(final Element element, final String searchKey) {
    String[] values;
    final NodeList nodes = element.getElementsByTagName("key");
    for (int i = 0; i < nodes.getLength(); i++) {
      final Element keyElement = (Element) nodes.item(i);
      final String keyName = getStringFromElement(keyElement);
      if (keyName.compareTo(searchKey) == 0) {
        final Element arrayElement = (Element) keyElement.getNextSibling();
        if (arrayElement.getTagName().compareTo("array") == 0) {
          final NodeList stringNodes = arrayElement.getElementsByTagName("string");
          final int arrayLength = stringNodes.getLength();
          values = new String[arrayLength];
          for (int j = 0; j < arrayLength; j++) {
            final Element stringElement = (Element) stringNodes.item(j);
            values[j] = getStringFromElement(stringElement);
          }
          return values;
        } else {
          return null;
        }
      }
    }
    return null;
  }

  // get the string contained inside the element
  private static String getStringFromElement(final Element e) {
    final Node child = e.getFirstChild();
    if (child instanceof CharacterData) {
      final CharacterData cd = (CharacterData) child;
      return cd.getData();
    } else {
      return null;
    }
  }
}
