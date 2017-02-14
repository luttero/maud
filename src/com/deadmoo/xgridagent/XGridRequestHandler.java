package com.deadmoo.xgridagent;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.HashMap;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.beepcore.beep.core.BEEPError;
import org.beepcore.beep.core.BEEPException;
import org.beepcore.beep.core.InputDataStream;
import org.beepcore.beep.core.MessageMSG;
import org.beepcore.beep.core.RequestHandler;
import org.beepcore.beep.core.StringOutputDataStream;
import org.beepcore.beep.util.BufferSegment;
import org.w3c.dom.CharacterData;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class XGridRequestHandler implements RequestHandler {
  public static final String PRO_XGRIDAGENT_URI = "http://www.apple.com/beep/xgrid/controller/agent";

  private final Agent agent;

  // characters that Apple puts in the XML
  static char charValue1[] = {0x09};
  static char charValue2[] = {0x0A};
  public static final String hiddenChar1 = new String(charValue1);
  public static final String hiddenChar2 = new String(charValue2);;

  XGridRequestHandler(Agent agent) {
    super();
    this.agent = agent;
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
        agent.closeSession(e.getMessage());
        agent.restart();
      }
    }
    String newMessage = strBuf.toString().trim();

    sendRPY(message);
        
    // Uncomment to print out the messages from the controller
    // System.out.println("Message from controller:\n" + newMessage);

    final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    Document document = null;
    try {
      final DocumentBuilder builder = factory.newDocumentBuilder();
      // get rid of DOCTYPE
      newMessage = newMessage.replaceAll("<!DOCTYPE.*>\n", "");
      // get rid of some characters that Apple puts in the XML
      newMessage = newMessage.replaceAll(hiddenChar1, "");
      newMessage = newMessage.replaceAll(hiddenChar2, "");

      document = builder.parse(new ByteArrayInputStream(newMessage.getBytes()));

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

    final NodeList nodes = document.getElementsByTagName("dict");
    final Element rootDict = (Element) nodes.item(0);
    final String identifier = getStringForKey(rootDict, XGridMessage.IDENTIFIER);
    long id;
    if (identifier != null && identifier.compareTo("") != 0) {
      id = Long.parseLong(identifier);
    } else {
      id = -1;
    }
    final String name = getStringForKey(rootDict, XGridMessage.NAME);
    final Element payloadDict = getDictForKey(rootDict, XGridMessage.PAYLOAD);
    final String type = getStringForKey(rootDict, XGridMessage.TYPE);

    synchronized(agent) {
            
      if (type.compareTo(XGridMessage.REPLY) == 0) {
        if (name.compareTo(XGridMessage.AGENT_REGISTRATION) == 0) {
          final String errorMessage = getStringForKey(payloadDict, XGridMessage.ERROR);
          if (errorMessage != null) {
            System.err.println("xgridrequesthandler: Error message from controller \n\t" + errorMessage);
             agent.closeSession(errorMessage);
             agent.restart();
             return;
          }
          agent.setAgentCookie(getStringForKey(payloadDict, XGridMessage.AGENT_COOKIE));
          agent.sendMSG(new XGridMessage(-1, XGridMessage.AGENT_STATUS, XGridMessage.NOTIFICATION, agent));
        }
      } else if (type.compareTo(XGridMessage.REQUEST) == 0) {
        if (name.compareTo(XGridMessage.AGENT_STATUS) == 0) {
          agent.sendMSG(new XGridMessage(id, name, XGridMessage.REPLY, agent));
        } else if (name.compareTo(XGridMessage.TASK_SUBMISSION) == 0) {
          final String[] arguments = getArrayForKey(payloadDict, XGridMessage.ARGUMENTS);
          final String command = getStringForKey(payloadDict, XGridMessage.COMMAND);
          final Element inputFilesElement = getDictForKey(payloadDict, XGridMessage.INPUT_FILES);
          final HashMap inputFiles = getInputFiles(inputFilesElement);
          final String inputStream = getStringForKey(payloadDict, XGridMessage.INPUT_STREAM);
          final Task task = agent.newTask(command, arguments, inputFiles, inputStream);
          agent.sendMSG(new XGridMessage(id, name, XGridMessage.REPLY, task));
          agent.sendMSG(new XGridMessage(-1, XGridMessage.AGENT_STATUS, XGridMessage.NOTIFICATION, agent));
        } else if (name.compareTo(XGridMessage.TASK_CONTROL) == 0) {
          final String control = getStringForKey(payloadDict, XGridMessage.CONTROL);
          final String taskRef = getStringForKey(payloadDict, XGridMessage.TASK_REF);
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
          final String taskRef = getStringForKey(payloadDict, XGridMessage.TASK_REF);
          final Task task = agent.getTask(Long.parseLong(taskRef));
          if (task != null) {
            agent.sendMSG(new XGridMessage(id, name, XGridMessage.REPLY, task));
          }
        } else if (name.compareTo(XGridMessage.TASK_FINAL_RESULTS) == 0) {
          final String taskRef = getStringForKey(payloadDict, XGridMessage.TASK_REF);
          final Task task = agent.getTask(Long.parseLong(taskRef));
          if (task != null) {
            agent.sendMSG(new XGridMessage(id, name, XGridMessage.REPLY, task));
          }
        }
      }
    }
  }

  public void sendRPY(final MessageMSG message) {
    try {
      message.sendRPY(new StringOutputDataStream(""));
    } catch (BEEPException e) {
      try {
        message.sendERR(BEEPError.CODE_REQUESTED_ACTION_ABORTED, "Error sending RPY");
      } catch (BEEPException x) {
        agent.closeSession(x.getMessage());
        agent.restart();
      }
    }
  }

  // get the input files
  public static HashMap getInputFiles(final Element element) {
    final HashMap inputFiles = new HashMap();
    if (element != null) {
      final NodeList nodes = element.getElementsByTagName("key");
      for (int i = 0; i < nodes.getLength(); i++) {
        final Element keyElement = (Element) nodes.item(i);
        final String keyName = getStringFromElement(keyElement);
        final Element dictElement = (Element) keyElement.getNextSibling();
        if (dictElement.getTagName().compareTo("dict") == 0) {
          final String fileData = getDataForKey(dictElement, XGridMessage.FILE_DATA);
          final String isExecutable = getStringForKey(dictElement, XGridMessage.IS_EXECUTABLE);
          if (fileData != null) {
            final HashMap file = new HashMap();
            file.put(XGridMessage.FILE_DATA, fileData);
            if (isExecutable != null) {
              file.put(XGridMessage.IS_EXECUTABLE, isExecutable);
            }
            inputFiles.put(keyName, file);
          }
        }
      }
    }
    return inputFiles;
  }

  // get the dict for a given key
  public static Element getDictForKey(final Element element, final String searchKey) {
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
  public static String getStringForKey(final Element element, final String searchKey) {
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
  public static String getDataForKey(final Element element, final String searchKey) {
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
  public static String[] getArrayForKey(final Element element, final String searchKey) {
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
  public static String getStringFromElement(final Element e) {
    final Node child = e.getFirstChild();
    if (child instanceof CharacterData) {
      final CharacterData cd = (CharacterData) child;
      return cd.getData();
    } else {
      return null;
    }
  }
}
