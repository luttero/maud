/*
 * @(#)XGridClientMessage.java created Mar 29, 2006 Casalino
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

import java.io.*;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;
import base64.Base64;

/**
 * The XGridClientMessage is a class to manage messaging in XGrid
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

public class XGridClientMessage {
  // properties of a message
  private final long correlationID;
  private final String name;
  private final String type;
  private String stringRepresentation;

  private Document document;

  // message names for client
  /**
   * The String for the name of a client registration message.
   */
  public static final String CLIENT_REGISTRATION = "clientRegistration";
  /**
   * The String for the name of a client status message.
   */
  public static final String CLIENT_STATUS = "clientStatus";

  // message names for task
  /**
   * The String for the name of a task final results message.
   */
  public static final String TASK_FINAL_RESULTS = "taskFinalResults";
  /**
   * The String for the name of a task results message.
   */
  public static final String TASK_RESULTS = "taskResults";
  /**
   * The String for the name of a task status message.
   */
  public static final String TASK_STATUS = "taskStatus";
  /**
   * The String for the name of a task control message.
   */
  public static final String TASK_CONTROL = "taskControl";
  /**
   * The String for the name of a task submission message.
   */
  public static final String TASK_SUBMISSION = "taskSubmission";
  /**
   * The String for the name of a task specification message.
   */
  public static final String TASK_SPECIFICATIONS = "taskSpecifications";
  /**
   * The String for the name of a task command message.
   */
  public static final String TASK_COMMAND = "command";
  /**
   * The String for the name of a task command arguments message.
   */
  public static final String TASK_ARGUMENTS = "arguments";

  // message names for job
  /**
   * The String for the name of a job specification message.
   */
  public static final String JOB_SPECIFICATION = "jobSpecification";
  /**
   * The String for the name of a job submission message.
   */
  public static final String JOB_SUBMISSION = "jobSubmission";
  /**
   * The String for the name of a job identifier message.
   */
  public static final String JOB_IDENTIFIER = "jobIdentifier";
  /**
   * The String for the name of a job attributes message.
   */
  public static final String JOB_ATTRIBUTES = "jobAttributes";
  /**
   * The String for the name of a job status message.
   */
  public static final String JOB_STATUS = "jobStatus";
  /**
   * The String for the name of a job results message.
   */
  public static final String JOB_RESULTS = "jobResults";
  /**
   * The String for the name of a job control message.
   */
  public static final String JOB_CONTROL = "jobControl";

  // message types
  /**
   * The String for the type of a request message.
   */
  public static final String REQUEST = "request";
  /**
   * The String for the type of a reply message.
   */
  public static final String REPLY = "reply";
  /**
   * The String for the type of a notification message.
   */
  public static final String NOTIFICATION = "notification";

  // message dictionary keys
  /**
   * The String for the dictionary key of a message identifier.
   */
  public static final String IDENTIFIER = "identifier";
  /**
   * The String for the dictionary key of an application identifier.
   */
  public static final String APPLICATION_IDENTIFIER = "applicationIdentifier";
  /**
   * The String for the dictionary key of a submission identifier.
   */
  public static final String SUBMISSION_IDENTIFIER = "submissionIdentifier";
  /**
   * The String for the dictionary key of a message name.
   */
  public static final String NAME = "name";
  /**
   * The String for the dictionary key of a message payload.
   */
  public static final String PAYLOAD = "payload";
  /**
   * The String for the dictionary key of a message type.
   */
  public static final String TYPE = "type";

  // payload keys for client
  /**
   * The String for the payload key of a client's addresses.
   */
  public static final String ADDRESSES = "addresses";
  /**
   * The String for the payload key of a client's hostnames.
   */
  public static final String HOSTNAMES = "hostnames";
  /**
   * The String for the payload key of a client's cookie.
   */
  public static final String CLIENT_COOKIE = "clientCookie";
  /**
   * The String for the payload key of a client's name.
   */
  public static final String CLIENT_NAME = "clientName";
  /**
   * The String for the payload key of a client's maximum CPU power.
   */
  public static final String MAXIMUM_CPU_POWER = "maximumCPUPower";
  /**
   * The String for the payload key of a client's maximum task count.
   */
  public static final String MAXIMUM_TASK_COUNT = "maximumTaskCount";
  /**
   * The String for the payload key of a client's current CPU power.
   */
  public static final String CURRENT_CPU_POWER = "currentCPUPower";
  /**
   * The String for the payload key of a client's current task count.
   */
  public static final String CURRENT_TASK_COUNT = "currentTaskCount";
  /**
   * The String for the payload key of a client's task reference array.
   */
  public static final String TASK_REF_ARRAY = "taskRefArray";

  // payload keys for task
  /**
   * The String for the payload key of a task's arguments.
   */
  public static final String ARGUMENTS = "arguments";
  /**
   * The String for the payload key of a task's command.
   */
  public static final String COMMAND = "command";
  /**
   * The String for the payload key of a task's input files.
   */
  public static final String INPUT_FILES = "inputFiles";
  /**
   * The String for the dictionary key of an input file's data.
   */
  public static final String FILE_DATA = "fileData";
  /**
   * The String for the dictionary key of if an input file is executable.
   */
  public static final String IS_EXECUTABLE = "isExecutable";
  /**
   * The String for the payload key of a task's standard input.
   */
  public static final String INPUT_STREAM = "inputStream";
  /**
   * The String for the key of the file used to store the standard input of a task.
   */
  public static final String STDIN = "__stdin";
  /**
   * The String for the payload key of a task's reference.
   */
  public static final String TASK_REF = "taskRef";
  /**
   * The String for the payload key of a task's standard error.
   */
  public static final String ERR_DATA = "errData";
  /**
   * The String for the payload key of a task's standard output.
   */
  public static final String READ_DATA = "readData";
  /**
   * The String for the payload key of a task's ouput files.
   */
  public static final String OUTPUT_FILES = "outputFiles";
  /**
   * The String for the payload key of a task's termination status.
   */
  public static final String TERMINATION_STATUS = "terminationStatus";
  /**
   * The String for the payload key of a task control.
   */
  public static final String CONTROL = "control";
  /**
   * The String for the payload key of a task's state.
   */
  public static final String TASK_STATE = "taskState";
  /**
   * The String for the payload key for the CPU power for this task.
   */
  public static final String TASK_MEGAHERTZ = "taskMegahertz";
  /**
   * The String for the payload key of if there is data available from a task.
   */
  public static final String DATA_AVAILABLE = "dataAvailable";

  // task controls
  /**
   * The String for the task control of start.
   */
  public static final String START = "start";
  /**
   * The String for the task control of cancel.
   */
  public static final String CANCEL = "cancel";

  // booleans
  /**
   * The String for the payload value of a yes.
   */
  public static final String YES = "YES";
  /**
   * The String for the payload value of a no.
   */
  public static final String NO = "NO";

  // errors
  /**
   * The String for the payload key of an error.
   */
  public static final String ERROR = "error";

  // trailing zeros on cpu power
  /**
   * The String for the trailing zeros that are added to the end of the CPU power.
   */
  public static final String ZEROS = ".000000";

  public static final int SUBSCRIBE = 1;
  public static final int RESULTS = 2;
  public static final int REMOVE = 3;
  public static final int STATUS = 4;

  /**
   * Creates a new message based on an task.
   *
   * @param correlationID the identifier for this message
   * @param name          the name for this message
   * @param number        the task number
   * @param command       the task command
   * @param arguments     the command arguments
   */
  XGridClientMessage(long correlationID, String name, String number, String command, String[] arguments) {
    super();
    this.correlationID = correlationID;
    this.name = name;
    this.type = REQUEST;

    Element[] thePayloadDict = genJobTemplate();

    final Element theTaskNumber = document.createElement("key");
    thePayloadDict[1].appendChild(theTaskNumber);
    final Text keyText = document.createTextNode(number);
    theTaskNumber.appendChild(keyText);
    final Element theDictNode = document.createElement("dict");
    thePayloadDict[1].appendChild(theDictNode);
    addArrayPair(theDictNode, TASK_ARGUMENTS, arguments);
    addStringPair(theDictNode, TASK_COMMAND, command);

    documentToString();
  }

  /**
   * Creates a new message to request information or control a job.
   *    <dict>
   *    <key>identifier</key>
   *    <string>correlationID</string>
   *    <key>name</key>
   *    <string>name</string>
   *    <key>payload</key>
   *    <dict>
   *     <key>jobIdentifier</key>
   *     <string>number</string>
   *     .............
   *    </dict>
   *    <key>type</key>
   *    <string>request</string>
   *   </dict>
   *
   *
   * @param correlationID the identifier for this message
   * @param name          the name for this message
   * @param number        the job identifier number
   * @param action        the request to be done
   */

  XGridClientMessage(long correlationID, String name, String number, int action) {
    super();

    this.correlationID = correlationID;
    this.name = name;
    this.type = REQUEST;

    switch (action) {
      case SUBSCRIBE:
        Element thePayloadDict = genTemplate();
        addStringPair(thePayloadDict, JOB_IDENTIFIER, number);
        addStringPair(thePayloadDict, "shouldSubscribe", YES);
        break;
      case RESULTS:
        thePayloadDict = genTemplate();
        addStringPair(thePayloadDict, JOB_IDENTIFIER, number);
        break;
      case REMOVE:
        thePayloadDict = genTemplate();
        addStringPair(thePayloadDict, "control", "remove");
        addStringPair(thePayloadDict, JOB_IDENTIFIER, number);
        break;
      case STATUS:
        thePayloadDict = genTemplate();
        addStringPair(thePayloadDict, JOB_IDENTIFIER, number);
        addStringPair(thePayloadDict, "shouldSubscribe", NO);
        break;
      default: {}
    }

    documentToString();
  }

  /**
   * Creates a new message to submit a job to the controller.
   *
   * @param correlationID the identifier for this message
   * @param name          the name for this message
   * @param number        the task number
   * @param filenames     the filenames to include (relative path)
   * @param filesBase64   the files content encoded base64
   * @param command       the task command
   * @param arguments     the command arguments
   */
  XGridClientMessage(long correlationID, String name, String number, String[] filenames, String[] filesBase64,
                     String command, String[] arguments) {
    super();
    this.correlationID = correlationID;
    this.name = name;
    this.type = REQUEST;

    Element[] thePayloadDict = genJobTemplate();

    Element anElement = null;
    Text keyText = null;
    Element theDictNode = null;

    for (int i = 0; i < filenames.length; i++) {
      anElement = document.createElement("key");
      thePayloadDict[0].appendChild(anElement);
      keyText = document.createTextNode(filenames[i]);
      anElement.appendChild(keyText);
      theDictNode = document.createElement("dict");
      thePayloadDict[0].appendChild(theDictNode);
      addDataPair(theDictNode, "fileData", filesBase64[i]);
    }

    final Element theTaskNumber = document.createElement("key");
    thePayloadDict[1].appendChild(theTaskNumber);
    keyText = document.createTextNode(number);
    theTaskNumber.appendChild(keyText);
    theDictNode = document.createElement("dict");
    thePayloadDict[1].appendChild(theDictNode);
    addArrayPair(theDictNode, TASK_ARGUMENTS, arguments);
    addStringPair(theDictNode, TASK_COMMAND, command);

    documentToString();
  }


  /**
   * Creates a new message to submit a job to the controller.
   *
   * @param correlationID the identifier for this message
   * @param name          the name for this message
   * @param number        the task number
   * @param filenames     the filenames of the files to encode
   * @param folder        the folder for files
   */
  XGridClientMessage(long correlationID, String name, String number, String[] filenames, String folder) {
    super();
    this.correlationID = correlationID;
    this.name = name;
    this.type = REQUEST;

    String[] files = loadfiles(folder, filenames);

    Element[] thePayloadDict = genBatchJobTemplate();

    Element anElement = null;
    Text keyText = null;
    Element theDictNode = null;

    for (int i = 0; i < files.length; i++) {
      anElement = document.createElement("key");
      thePayloadDict[0].appendChild(anElement);
      keyText = document.createTextNode(filenames[i]);
      anElement.appendChild(keyText);
      theDictNode = document.createElement("dict");
      thePayloadDict[0].appendChild(theDictNode);
      addDataPair(theDictNode, "fileData", files[i]);
//      addStringPair(theDictNode, "isExecutable", "NO");
    }

    anElement = document.createElement("key");
    thePayloadDict[1].appendChild(anElement);
    keyText = document.createTextNode(number);
    anElement.appendChild(keyText);
    theDictNode = document.createElement("dict");
    thePayloadDict[1].appendChild(theDictNode);
    String[] arguments = {"-mx512M", "-jar", filenames[0], "-xgrid", "-analysis", filenames[1]};
    String command = "/usr/bin/java";
    addArrayPair(theDictNode, TASK_ARGUMENTS, arguments);
    addStringPair(theDictNode, TASK_COMMAND, command);

    documentToString();
  }

  /**
  * Prepare xml file String to submit (for XGrid command line)
  * @param correlationID the identifier for this message
  * @param name          the name for this message
  * @param number        the task number
  * @param fileEncodedBase64      a file already encoded
  * @param filenames      the filenames of the files to include
  * @param folder        the folder for files
  */
 XGridClientMessage(long correlationID, String name, String number, String fileEncodedBase64, String[] filenames,
                    String folder) {
   super();
   this.correlationID = correlationID;
   this.name = name;
   this.type = REQUEST;

   String file = loadfile(folder+filenames[1]);

   Element[] thePayloadDict = genBatchJobTemplate();

   Element anElement = null;
   Text keyText = null;
   Element theDictNode = null;

    anElement = document.createElement("key");
    thePayloadDict[0].appendChild(anElement);
    keyText = document.createTextNode(filenames[0]);
    anElement.appendChild(keyText);
    theDictNode = document.createElement("dict");
    thePayloadDict[0].appendChild(theDictNode);
    addDataPair(theDictNode, "fileData", fileEncodedBase64);

     anElement = document.createElement("key");
     thePayloadDict[0].appendChild(anElement);
     keyText = document.createTextNode(filenames[1]);
     anElement.appendChild(keyText);
     theDictNode = document.createElement("dict");
     thePayloadDict[0].appendChild(theDictNode);
     addDataPair(theDictNode, "fileData", file);
//      addStringPair(theDictNode, "isExecutable", "NO");

   anElement = document.createElement("key");
   thePayloadDict[1].appendChild(anElement);
   keyText = document.createTextNode(number);
   anElement.appendChild(keyText);
   theDictNode = document.createElement("dict");
   thePayloadDict[1].appendChild(theDictNode);
   String[] arguments = {"-mx512M", "-jar", filenames[0], "-xgrid", "-analysis", filenames[1]};
   String command = "/usr/bin/java";
   addArrayPair(theDictNode, TASK_ARGUMENTS, arguments);
   addStringPair(theDictNode, TASK_COMMAND, command);

   documentToString();
 }

  // add a pair with a string value to a dictionary
  private void addStringPair(final Element theDictNode, final String key, final String value) {
    final Element keyNode = document.createElement("key");
    theDictNode.appendChild(keyNode);
    final Text keyText = document.createTextNode(key);
    keyNode.appendChild(keyText);
    final Element stringNode = document.createElement("string");
    theDictNode.appendChild(stringNode);
    final Text stringText = document.createTextNode(value);
    stringNode.appendChild(stringText);
  }

  // add a pair with a string value to a dictionary
  private void addDataPair(final Element theDictNode, final String key, final String value) {
    final Element keyNode = document.createElement("key");
    theDictNode.appendChild(keyNode);
    final Text keyText = document.createTextNode(key);
    keyNode.appendChild(keyText);
    final Element stringNode = document.createElement("data");
    theDictNode.appendChild(stringNode);
    final Text stringText = document.createTextNode(value);
    stringNode.appendChild(stringText);
  }

  // add a pair with an array of string values to a dictionary
  private void addArrayPair(final Element theDictNode, final String key, final String[] values) {
    final Element keyNode = document.createElement("key");
    theDictNode.appendChild(keyNode);
    final Text keyText = document.createTextNode(key);
    keyNode.appendChild(keyText);
    final Element arrayNode = document.createElement("array");
    theDictNode.appendChild(arrayNode);
    for (int i = 0; i < values.length; i++) {
      final Element stringNode = document.createElement("string");
      arrayNode.appendChild(stringNode);
      final Text stringText = document.createTextNode(values[i]);
      stringNode.appendChild(stringText);
    }
  }

  // add a pair with a dictionary to a dictionary
  private Element addDictPair(final Element theDictNode, final String key) {
    final Element payloadKey = document.createElement("key");
    theDictNode.appendChild(payloadKey);
    final Text keyText = document.createTextNode(key);
    payloadKey.appendChild(keyText);
    final Element newDict = document.createElement("dict");
    theDictNode.appendChild(newDict);
    return newDict;
  }

  // generate template for xgrid message
  private Element genTemplate() {
    Element thePayloadDict = null;
    final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    try {
      final DocumentBuilder builder = factory.newDocumentBuilder();
      final DOMImplementation impl = builder.getDOMImplementation();
      // Create the document
      document = impl.createDocument(null, "plist", null);

      final Element root = document.getDocumentElement();
      root.setAttribute("version", "1.0");
      final Element theDictNode = document.createElement("dict");
      root.appendChild(theDictNode);
      if (correlationID >= 0) {
        addStringPair(theDictNode, IDENTIFIER, Long.toString(correlationID));
      } else {
        addStringPair(theDictNode, IDENTIFIER, "");
      }
      addStringPair(theDictNode, NAME, name);
      thePayloadDict = addDictPair(theDictNode, PAYLOAD);
      addStringPair(theDictNode, TYPE, type);
    } catch (ParserConfigurationException pce) {
      // Parser with specified options can't be built
      pce.printStackTrace();
    }
    return thePayloadDict;
  }

  // generate template for xgrid message with job submission
  private Element[] genJobTemplate() {
    Element[] thePayloadDict = new Element[2];
    final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    try {
      final DocumentBuilder builder = factory.newDocumentBuilder();
      final DOMImplementation impl = builder.getDOMImplementation();
      // Create the document
      document = impl.createDocument(null, "plist", null);

      final Element root = document.getDocumentElement();
      root.setAttribute("version", "1.0");

      final Element firstDictNode = document.createElement("dict");
      root.appendChild(firstDictNode);
      if (correlationID >= 0) {
        addStringPair(firstDictNode, IDENTIFIER, Long.toString(correlationID));
      } else {
        addStringPair(firstDictNode, IDENTIFIER, "0");
      }
      addStringPair(firstDictNode, NAME, JOB_SUBMISSION);
      final Element payload = document.createElement("key");
      firstDictNode.appendChild(payload);
      final Text keyText = document.createTextNode(PAYLOAD);
      payload.appendChild(keyText);

      final Element theDictNode = document.createElement("dict");
      firstDictNode.appendChild(theDictNode);
      final Element theTaskDict = addDictPair(theDictNode, JOB_SPECIFICATION);
      addStringPair(theTaskDict, APPLICATION_IDENTIFIER, "com.apple.xgrid.cli");
      thePayloadDict[0] = addDictPair(theTaskDict, INPUT_FILES);
      addStringPair(theTaskDict, NAME, name);
      addStringPair(theTaskDict, SUBMISSION_IDENTIFIER, "abc");
      thePayloadDict[1] = addDictPair(theTaskDict, TASK_SPECIFICATIONS);
      addStringPair(theTaskDict, TYPE, "XgTaskList");

      addStringPair(firstDictNode, TYPE, REQUEST);
    } catch (ParserConfigurationException pce) {
      // Parser with specified options can't be built
      pce.printStackTrace();
    }
    return thePayloadDict;
  }

  // generate template for xgrid message with job submission (for XGrid command line)
  private Element[] genBatchJobTemplate() {
    Element[] thePayloadDict = new Element[2];
    final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    try {
      final DocumentBuilder builder = factory.newDocumentBuilder();
      final DOMImplementation impl = builder.getDOMImplementation();
      // Create the document
      document = impl.createDocument(null, "plist", null);

      final Element root = document.getDocumentElement();
      root.setAttribute("version", "1.0");
      final Element theDictNode = document.createElement("dict");
      root.appendChild(theDictNode);
      final Element theTaskDict = addDictPair(theDictNode, JOB_SPECIFICATION);
      addStringPair(theTaskDict, APPLICATION_IDENTIFIER, "com.apple.xgrid.cli");
      thePayloadDict[0] = addDictPair(theTaskDict, INPUT_FILES);
      addStringPair(theTaskDict, NAME, name);
      addStringPair(theTaskDict, SUBMISSION_IDENTIFIER, "abc");
      thePayloadDict[1] = addDictPair(theTaskDict, TASK_SPECIFICATIONS);
    } catch (ParserConfigurationException pce) {
      // Parser with specified options can't be built
      pce.printStackTrace();
    }
    return thePayloadDict;
  }

  // generate template for xgrid message with array
  private Element genJobTemplateArray() {
    Element thePayloadDict = null;
    final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    try {
      final DocumentBuilder builder = factory.newDocumentBuilder();
      final DOMImplementation impl = builder.getDOMImplementation();
      // Create the document
      document = impl.createDocument(null, "plist", null);

      final Element root = document.getDocumentElement();
      root.setAttribute("version", "1.0");
      final Element theArray = document.createElement("array");
      root.appendChild(theArray);
      final Element theDictNode = document.createElement("dict");
      theArray.appendChild(theDictNode);
      if (correlationID >= 0) {
        addStringPair(theDictNode, IDENTIFIER, Long.toString(correlationID));
      }
      addStringPair(theDictNode, NAME, name);
      thePayloadDict = addDictPair(theDictNode, TASK_SPECIFICATIONS);
    } catch (ParserConfigurationException pce) {
      // Parser with specified options can't be built
      pce.printStackTrace();
    }
    return thePayloadDict;
  }

  // serialize the XML document to a string
  private void documentToString() {
    final OutputFormat format = new OutputFormat(document);
    format.setDoctype("-//Apple Computer//DTD PLIST 1.0//EN", "http://www.apple.com/DTDs/PropertyList-1.0.dtd");
    final ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
    final XMLSerializer serializer = new XMLSerializer(byteOut, format);
    try {
      serializer.serialize(document);
    } catch (IOException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    stringRepresentation = byteOut.toString().trim();
  }

  // BEGIN getters and setters

  /**
   * Get a String containing the full message.
   *
   * @return the full message
   */
  public String getStringRepresentation() {
    return stringRepresentation;
  }
  // END getters and setters

  public static String[] loadfiles(String folder, String[] filenames) {
    String[] data = new String[filenames.length];
    for (int i = 0; i < filenames.length; i++) {
      data[i] = loadfile(folder + filenames[i]);
    }
    return data;
  }

  // method to load a file and encode base64 for submission
  public static String loadfile(String filename) {
    String data = "";
    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    try {
      final BufferedInputStream in = new BufferedInputStream(new FileInputStream(new File(filename)));
      int mark = 0;
      int available = in.available();
      byte[] bytes = new byte[available];
      while (available > 0 && in.read(bytes, mark, available) != -1) {
        bos.write(bytes);
        mark += available;
        available = in.available();
        if (available > 0)
          bytes = new byte[available];
      }
      in.close();
      bos.flush();
      data = Base64.encodeBytes(bos.toByteArray());
      bos.close();
    } catch (IOException io) {
      io.printStackTrace();
    }
/*    try {
      final PrintWriter pw = new PrintWriter(bos);

      final BufferedReader br = new BufferedReader(new FileReader(new File(filename)));
      String line = null;
      while ((line = br.readLine()) != null) {
        pw.println(line);
      }
      pw.flush();
      pw.close();
      data = Base64.encodeBytes(bos.toByteArray());
    } catch (IOException ioe) {
      ioe.printStackTrace();
    }*/
    return data;
  }

}
