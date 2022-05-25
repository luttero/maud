package com.deadmoo.xgridagent;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;

public class XGridMessage {
	// properties of a message
	private final long correlationID;
	private final String name;
	private final String type;
	private String stringRepresentation;
	
	private Document document;
	
	// message names for agent
	/** The String for the name of an agent registration message. */
	public static final String AGENT_REGISTRATION = "agentRegistration";
	/** The String for the name of an agent status message. */
	public static final String AGENT_STATUS = "agentStatus";
	
	// message names for task
	/** The String for the name of a task final results message. */
	public static final String TASK_FINAL_RESULTS = "taskFinalResults";
	/** The String for the name of a task results message. */
	public static final String TASK_RESULTS = "taskResults";
	/** The String for the name of a task status message. */
	public static final String TASK_STATUS = "taskStatus";
	/** The String for the name of a task control message. */
	public static final String TASK_CONTROL = "taskControl";
	/** The String for the name of a task submission message. */
	public static final String TASK_SUBMISSION = "taskSubmission";

	// message types
	/** The String for the type of a request message. */
	public static final String REQUEST = "request";
	/** The String for the type of a reply message. */
	public static final String REPLY = "reply";
	/** The String for the type of a notification message. */
	public static final String NOTIFICATION = "notification";
	
	// message dictionary keys
	/** The String for the dictionary key of a message identifier. */
	public static final String IDENTIFIER = "identifier";
	/** The String for the dictionary key of a message name. */
	public static final String NAME = "name";
	/** The String for the dictionary key of a message payload. */
	public static final String PAYLOAD = "payload";
	/** The String for the dictionary key of a message type. */
	public static final String TYPE = "type";
	
	// payload keys for agent
	/** The String for the payload key of an agent's addresses. */
	public static final String ADDRESSES = "addresses";
	/** The String for the payload key of an agent's hostnames. */
	public static final String HOSTNAMES = "hostnames";
	/** The String for the payload key of an agent's cookie. */
	public static final String AGENT_COOKIE = "agentCookie";
	/** The String for the payload key of an agent's name. */
	public static final String AGENT_NAME = "agentName";
	/** The String for the payload key of an agent's maximum CPU power. */
	public static final String MAXIMUM_CPU_POWER = "maximumCPUPower";
	/** The String for the payload key of an agent's maximum task count. */
	public static final String MAXIMUM_TASK_COUNT = "maximumTaskCount";
	/** The String for the payload key of an agent's current CPU power. */
	public static final String CURRENT_CPU_POWER = "currentCPUPower";
	/** The String for the payload key of an agent's current task count. */
	public static final String CURRENT_TASK_COUNT = "currentTaskCount";
	/** The String for the payload key of an agent's task reference array. */
	public static final String TASK_REF_ARRAY = "taskRefArray";
	
	// payload keys for task
	/** The String for the payload key of a task's arguments. */
	public static final String ARGUMENTS = "arguments";
	/** The String for the payload key of a task's command. */
	public static final String COMMAND = "command";
	/** The String for the payload key of a task's input files. */
	public static final String INPUT_FILES = "inputFiles";
	/** The String for the dictionary key of an input file's data. */
	public static final String FILE_DATA = "fileData";
	/** The String for the dictionary key of if an input file is executable. */
	public static final String IS_EXECUTABLE = "isExecutable";
	/** The String for the payload key of a task's standard input. */
	public static final String INPUT_STREAM = "inputStream";
	/** The String for the key of the file used to store the standard input of a task. */
	public static final String STDIN = "__stdin";
	/** The String for the payload key of a task's reference. */
	public static final String TASK_REF = "taskRef";
	/** The String for the payload key of a task's standard error. */
	public static final String ERR_DATA = "errData";
	/** The String for the payload key of a task's standard output. */
	public static final String READ_DATA = "readData";
	/** The String for the payload key of a task's ouput files. */
	public static final String OUTPUT_FILES = "outputFiles";
	/** The String for the payload key of a task's termination status. */
	public static final String TERMINATION_STATUS = "terminationStatus";
	/** The String for the payload key of a task control. */
	public static final String CONTROL = "control";
	/** The String for the payload key of a task's state. */
	public static final String TASK_STATE = "taskState";
	/** The String for the payload key for the CPU power for this task. */
	public static final String TASK_MEGAHERTZ = "taskMegahertz";
	/** The String for the payload key of if there is data available from a task. */
	public static final String DATA_AVAILABLE = "dataAvailable";
	
	// task controls
	/** The String for the task control of start. */
	public static final String START = "start";
	/** The String for the task control of cancel. */
	public static final String CANCEL = "cancel";
	
	// booleans
	/** The String for the payload value of a yes. */
	public static final String YES = "YES";
	/** The String for the payload value of a no. */
	public static final String NO = "NO";
	
	// errors
	/** The String for the payload key of an error. */
	public static final String ERROR = "error";
	
	// trailing zeros on cpu power
	/** The String for the trailing zeros that are added to the end of the CPU power. */
	public static final String ZEROS = ".000000";
	
	/**
	 * Creates a new message based on an agent.
	 * 
	 * @param correlationID the identifier for this message
	 * @param name the name for this message
	 * @param type the type of this message
	 * @param agent the agent to base the message on
	 */
	public XGridMessage(long correlationID, String name, String type, Agent agent) {
        super();
		this.correlationID = correlationID;
		this.name = name;
		this.type = type;
		
		Element thePayloadDict = genTemplate();
		
		if(name.compareTo(AGENT_REGISTRATION) == 0 && type.compareTo(REQUEST) == 0) {
			addArrayPair(thePayloadDict, ADDRESSES, new String[]{agent.getAddress()});
			addArrayPair(thePayloadDict, HOSTNAMES, new String[]{agent.getHostname()});
			addStringPair(thePayloadDict, AGENT_COOKIE, agent.getAgentCookie());
			addStringPair(thePayloadDict, AGENT_NAME, agent.getAgentName());
			addStringPair(thePayloadDict, MAXIMUM_CPU_POWER, Long.toString(agent.getMaximumCPUPower()));
			addStringPair(thePayloadDict, MAXIMUM_TASK_COUNT, Long.toString(agent.getMaxTaskCount()));
		} else if(name.compareTo(AGENT_STATUS) == 0 && (type.compareTo(REPLY) == 0 || type.compareTo(NOTIFICATION) == 0)) {
			addStringPair(thePayloadDict, AGENT_COOKIE, agent.getAgentCookie());
			addStringPair(thePayloadDict, AGENT_NAME, agent.getAgentName());
			addStringPair(thePayloadDict, AGENT_STATUS, agent.getAgentStatus());
			addStringPair(thePayloadDict, CURRENT_CPU_POWER, Long.toString(agent.getCurrentCPUPower()) + ZEROS);
			addStringPair(thePayloadDict, CURRENT_TASK_COUNT, Long.toString(agent.getCurrentTaskCount()));
			int length = agent.getTaskQueue().size();
			Long[] longRefArray = new Long[length]; 
			longRefArray = (Long[]) agent.getTaskQueue().keySet().toArray(longRefArray);
			String[] stringRefArray = new String[length];
			for(int i = 0; i < length; i++) {
				stringRefArray[i] = longRefArray[i].toString();
			}
			addArrayPair(thePayloadDict, TASK_REF_ARRAY, stringRefArray);
		}
		
		documentToString();
	}

	/**
	 * Creates a new message based on an task.
	 * 
	 * @param correlationID the identifier for this message
	 * @param name the name for this message
	 * @param type the type of this message
	 * @param task the task to base the message on
	 */
	XGridMessage(long correlationID, String name, String type, Task task) {
        super();
		this.correlationID = correlationID; 
		this.name = name;
		this.type = type;
		
		Element thePayloadDict = genTemplate();
		
		if(name.compareTo(TASK_SUBMISSION) == 0 && type.compareTo(REPLY) == 0) {
			if(task != null) {
				addStringPair(thePayloadDict, TASK_REF, Long.toString(task.getTaskRef()));
			} else {
				addStringPair(thePayloadDict, ERROR, Agent.UNAVAILABLE);
			}
		//} else if(name.compareTo(TASK_CONTROL) == 0 && type.compareTo(REPLY) == 0) {
			// this has no payload
		} else if(name.compareTo(TASK_STATUS) == 0 && type.compareTo(NOTIFICATION) == 0) {
			addStringPair(thePayloadDict, DATA_AVAILABLE, task.getDataAvailable());
			addStringPair(thePayloadDict, TASK_MEGAHERTZ, Long.toString(task.getTaskMegahertz()));
			String status = task.getStatus();
			addStringPair(thePayloadDict, TASK_STATE, status);
			if(status.compareTo(Task.RUNNING) == 0) {
				addStringPair(thePayloadDict, TERMINATION_STATUS, "");
			} else {
				addStringPair(thePayloadDict, TERMINATION_STATUS, Long.toString(task.getTerminationResult()));
			}
		} else if(name.compareTo(TASK_RESULTS) == 0 && type.compareTo(REPLY) == 0) {
			addStringPair(thePayloadDict, AGENT_NAME, task.getAgent().getAgentName());
			addStringPair(thePayloadDict, TASK_REF, Long.toString(task.getTaskRef()));
			Element resultsNode = addDictPair(thePayloadDict, TASK_RESULTS);
			addDataPair(resultsNode, ERR_DATA, task.getErrData());
			addDataPair(resultsNode, READ_DATA, task.getReadData());
		} else if(name.compareTo(TASK_FINAL_RESULTS) == 0 && type.compareTo(REPLY) == 0) {
			addStringPair(thePayloadDict, AGENT_NAME, task.getAgent().getAgentName());
			addStringPair(thePayloadDict, TASK_REF, Long.toString(task.getTaskRef()));
			Element resultsNode = addDictPair(thePayloadDict, TASK_RESULTS);
			Element filesNode = addDictPair(resultsNode, OUTPUT_FILES);
			int length = task.getOutputFiles().size();
			String[] filenames = new String[length]; 
			filenames = (String[]) task.getOutputFiles().keySet().toArray(filenames);
			for(int i = 0; i < length; i++) {
				Element fileDict = addDictPair(filesNode, filenames[i]);
				addDataPair(fileDict, FILE_DATA, (String) task.getOutputFiles().get(filenames[i]));
			}
			addStringPair(resultsNode, TERMINATION_STATUS, Long.toString(task.getTerminationResult()));
		}
		
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
		for(int i = 0; i < values.length; i++) {
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
          if(correlationID >= 0) {
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
	/** Get a String containing the full message.
	 * 
	 * @return the full message
	 */
	public String getStringRepresentation() {
		return stringRepresentation;
	}
	// END getters and setters
}
