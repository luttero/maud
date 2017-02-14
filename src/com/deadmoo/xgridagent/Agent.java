package com.deadmoo.xgridagent;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import org.beepcore.beep.core.BEEPError;
import org.beepcore.beep.core.BEEPException;
import org.beepcore.beep.core.Channel;
import org.beepcore.beep.core.Session;
import org.beepcore.beep.core.StringOutputDataStream;
import org.beepcore.beep.lib.NullReplyListener;
import org.beepcore.beep.transport.tcp.TCPSessionCreator;

public class Agent {
  // properties of an agent
  private final long maxTaskCount;
  private long currentTaskCount;
  private final String agentName;
  private final String agentStatus;
  private String agentCookie;
  private long currentCPUPower;
  private final long maximumCPUPower;
  private final HashMap taskQueue;

  // agent status
  /**
   * The String for the agent status of Available.
   */
  public static final String AVAILABLE = "Available";
  /**
   * The String for the agent status of unavailable.
   */
  public static final String UNAVAILABLE = "Unavailable";

  // the session and channel to and from the controller
  private Session session;
  private Channel channel;

  // hostname and address of this agent
  private String hostname;
  private String address;

  // the server host and port
  private String host;
  private final int port;

  // messages that are buffered until reconnected
  private ArrayList messageBuffer;
  // is the agent connected to the controller
  public boolean isConnected = false;
  // is the agent already sending the message buffer
  private boolean sendingBuffer = false;

  /**
   * create a new agent
   *
   * @param host        the controller host address
   * @param port        the controller port for xgrid
   * @param agentName   the name of this agent
   * @param oneCPUPower the power of one CPU on the agent
   * @param hostname    the hostname of the agent
   * @param address     the address of the agent
   */
  Agent(String host, int port, String agentName, long oneCPUPower, String hostname,
        String address) {
    super();
    this.agentName = agentName;
    this.host = host;
    this.port = port;

    this.hostname = hostname;
    this.address = address;

    session = null;
    channel = null;

    currentTaskCount = 0;

    agentCookie = loadCookie(agentName);

    agentStatus = AVAILABLE;
    currentCPUPower = 0;

    maxTaskCount = Runtime.getRuntime().availableProcessors();
    // test value
    //maxTaskCount = 16;
    this.maximumCPUPower = oneCPUPower * maxTaskCount;

    taskQueue = new HashMap();

    messageBuffer = new ArrayList();
  }

  // register with the controller
  public void register() {
    session.addChannelListener(new XGridCloseListener(this));
    // don't need session listener
    //session.addSessionListener(new XGridCloseListener(this));
    channel.setRequestHandler(new XGridRequestHandler(this));

    final XGridMessage regMessage = new XGridMessage(-1, XGridMessage.AGENT_REGISTRATION, XGridMessage.REQUEST, this);
    sendMSG(regMessage);
  }

  public boolean startSession() {
    // Initiate a session with the server
    try {
      session = TCPSessionCreator.initiate(host, port);
    } catch (BEEPException e) {
      System.err.println("xgridagent: Error connecting to " + host + ":" + port + "\n\t" + e.getMessage());
      return false;
    }

    // Start a channel for the xgridagent profile
    try {
      channel = session.startChannel(XGridRequestHandler.PRO_XGRIDAGENT_URI);
    } catch (BEEPError e) {
      if (e.getCode() == 550) {
        System.err.println("xgridagent: Error host does not support xgridagent profile");
      } else {
        System.err.println("xgridagent: Error starting channel (" + e.getCode() + ": " + e.getMessage() + ")");
      }
      return false;
    } catch (BEEPException e) {
      System.err.println("xgridagent: Error starting channel (" + e.getMessage() + ")");
      return false;
    }

    return true;
  }

  public void closeSession(String errorMessage) {
    //shouldn't need to do this
    /*try {
        channel.close();
    } catch (BEEPException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
    }*/
    session.terminate(errorMessage);
    isConnected = false;
  }

  private void sendBufferedMessages() {
    if (!sendingBuffer) {
      sendingBuffer = true;
      Iterator i = messageBuffer.iterator();
      while (i.hasNext()) {
        sendMSG((XGridMessage) i.next());
      }
      messageBuffer.clear();
      sendingBuffer = false;
    }
  }

  public void restart() {
    System.out.println("restarting connection...\n");
    isConnected = false;
    while (!isConnected) {
      try {
        Thread.sleep(XGridAgent.statusDelay * 1000);
      } catch (InterruptedException e) {
      }
      isConnected = startSession();
    }
    register();
    sendBufferedMessages();
    System.out.println("\trestarted\n");
  }

  // load the cookie from a file
  public String loadCookie(String agentName) {
    String pathToName = XGridAgent.pathForCookie + ".cookie_" + agentName + "_" + host;
    String agentCookie = null;
    try {
      final BufferedReader br = new BufferedReader(new FileReader(pathToName));
      agentCookie = br.readLine();
      br.close();
    } catch (FileNotFoundException e) {
      agentCookie = "";
    } catch (IOException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return agentCookie;
  }

  // save the cookie to a file
  public void saveCookie(String agentCookie, String agentName) {
    String pathToName = XGridAgent.pathForCookie + ".cookie_" + agentName + "_" + XGridAgent.host;
    try {
      final PrintWriter pw = new PrintWriter(new FileWriter(pathToName));
      pw.println(agentCookie);
      pw.close();
    } catch (IOException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  /**
   * send a message to the controller
   *
   * @param message the message to send
   */
  public void sendMSG(final XGridMessage message) {
    try {
      // Uncomment to see messages to controller
      //System.out.println("Message to controller:\n" + message.getStringRepresentation());
      if (isConnected && channel != null && channel.getState() == Channel.STATE_ACTIVE) {
        if (!this.messageBuffer.isEmpty()) sendBufferedMessages();
        synchronized (channel) {
          channel.sendMSG(new StringOutputDataStream(message.getStringRepresentation()), NullReplyListener.getListener());
        }
      } else {
        if (!messageBuffer.contains(message)) messageBuffer.add(message);
      }
    } catch (BEEPException e) {
      messageBuffer.add(message);
      closeSession(e.getMessage());
      restart();
    }

  }

  /**
   * create new task
   *
   * @param command     the command to run
   * @param arguments   the arguments for the command
   * @param inputFiles  the input files for the working directory
   * @param inputStream the input file to get standard input from
   * @return the task
   */
  public Task newTask(final String command, final String[] arguments, final HashMap inputFiles, final String inputStream) {
    Task task = null;
    if (currentTaskCount < maxTaskCount) {
      long serialTaskRef = 0;
      while (serialTaskRef < maxTaskCount && taskQueue.containsKey(new Long(serialTaskRef))) {
        serialTaskRef++;
      }
      task = new Task(this, command, arguments, inputFiles, inputStream, maximumCPUPower / maxTaskCount, serialTaskRef);
      taskQueue.put(new Long(serialTaskRef), task);
      currentTaskCount++;
      currentCPUPower += maximumCPUPower / maxTaskCount;
    }
    return task;
  }

  /**
   * get a task
   *
   * @param taskRef the task reference
   * @return the task
   */
  public Task getTask(final long taskRef) {
    return (Task) taskQueue.get(new Long(taskRef));
  }

  /**
   * remove a task
   *
   * @param taskRef the task reference
   */
  public void removeTask(final long taskRef) {
    currentTaskCount--;
    currentCPUPower -= maximumCPUPower / maxTaskCount;
    getTask(taskRef).cleanUp();
    taskQueue.remove(new Long(taskRef));
  }

  // BEGIN getters and setters

  /**
   * get the agent's cookie
   *
   * @return the cookie
   */
  public String getAgentCookie() {
    return agentCookie;
  }

  /**
   * set the agent's cookie
   *
   * @param agentCookie the cookie
   */
  public void setAgentCookie(final String agentCookie) {
    this.agentCookie = agentCookie;

    saveCookie(agentCookie, agentName);
  }

  /**
   * get the agent's name
   *
   * @return the name
   */
  public String getAgentName() {
    return agentName;
  }

  /**
   * get the agent's status
   *
   * @return the status
   */
  public String getAgentStatus() {
    return agentStatus;
  }

  /**
   * get the current task count
   *
   * @return the number of tasks
   */
  public long getCurrentTaskCount() {
    return currentTaskCount;
  }

  /**
   * get the maximum tasks that can be handled
   *
   * @return the number of tasks
   */
  public long getMaxTaskCount() {
    return maxTaskCount;
  }

  /**
   * get the current CPU power being used
   *
   * @return the CPU power
   */
  public long getCurrentCPUPower() {
    return currentCPUPower;
  }

  /**
   * get the maximum CPU power of the agent
   *
   * @return the CPU power
   */
  public long getMaximumCPUPower() {
    return maximumCPUPower;
  }

  /**
   * get the agent's task queue
   *
   * @return HashMap<Long, Task> where the keys are the task references and the values are the tasks
   */
  public HashMap getTaskQueue() {
    return taskQueue;
  }

  /**
   * get the address of the agent
   *
   * @return the address
   */
  public String getAddress() {
    return address;
  }

  /**
   * get the hostname of the agent
   *
   * @return the hostname
   */
  public String getHostname() {
    return hostname;
  }
  // END getters and setters
}
