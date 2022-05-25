package com.deadmoo.xgridagent;

import java.io.*;
import java.util.Vector;

import javax.jmdns.JmDNS;
import javax.jmdns.ServiceInfo;

import jnt.scimark2.Benchmark;

public class XGridAgent {

  public static final String PRO_XGRIDAGENT_URI = "http://www.apple.com/beep/xgrid/controller/agent";
  public static final String xgridServiceString = "_xgrid._tcp.local.";
  public static String host;
  public static String[] hostname = null;
  private static String name;
  private static long power;
  public static int xgridport = 4111;
  public static String pathForCookie = "";
  public static String pathForQueue = "";
  public static boolean shouldRun = false;
  public static double version = 1.1;
  public static Vector controllerHostList = new Vector(0, 1);
  public static Agent agent = null;
  public static boolean saveName = true;
  public static boolean notifyStatus = false;
  public static int statusDelay = 15;
  public static boolean remove = true;

  private static final String usage =
      "usage:\tjava -jar xgridagent.jar [-h hostname] [-a agentname] [-p mhz ] [-c pathForCookie] " +
          "[-q pathForQueue] [-s notificationPause]\n" +
          "\n" +
          "\t-usage or -help will print this message\n" +
          "\thostname is the address or hostname on the controller\n" +
          "\tno hostname will resolve the controller via bonjour\n" +
          "\tagentname is the name of this agent\n" +
          "\tno entry will automatically assign a name to this agent (xgridAgent_hostname)\n" +
          "\tmhz is the CPU power of one CPU in this agent in mhz\n" +
          "\tno mhz will compute the CPU power this agent\n" +
          "\tpathForCookie (optional): the path where to save the cookie received\n" +
          "\tpathForQueue (optional): the path where to store the temporary queue\n" +
          "\tif no path, the starting directory of the agent will be used\n" +
          "\tnotificationPause is the pause in secs between agent notifications of status to the controller\n" +
          "\tno notificationPause will not notify the status until there is a change\n" +
          "\n" +
          "\trun the program from within the xgridagent directory\n";

  public static void main(final String[] argv) {
    hostname = checkHostNameAndAddress(hostname);
    // Parse command line args
    if (!parseArgs(argv)) {
      System.err.println(usage);
      return;
    }

    setControllerHost(getControllerAddress(host));
    // Start the Agent
    startAgent();
    if (agent != null)
      shouldRun = true;
    else
      shouldRun = false;
    while (shouldRun) {
      try {
        Thread.sleep(statusDelay * 1000);
        if (agent != null && shouldRun && notifyStatus)
          agent.sendMSG(new XGridMessage(-1, XGridMessage.AGENT_STATUS, XGridMessage.NOTIFICATION, agent));
      } catch (InterruptedException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
  }

  public static void startAgent() {
    // Start the Agent
    agent = new Agent(host, xgridport, name, power, hostname[0], hostname[1]);
    if (!agent.startSession()) {
      agent = null;
      return;
    } else {
      System.out.println("Connected to controller: " + host + "\n");
      agent.isConnected = true;
    }
    agent.register();
  }

  private static boolean parseArgs(final String[] argv) {
    host = "-r";
    name = null;
    power = -1;
    try {
      for (int i = 0; i < argv.length; i++) {
        if (argv[i].equalsIgnoreCase("-usage") || argv[i].equalsIgnoreCase("-help"))
          return false;
        else if (argv[i].equalsIgnoreCase("-h"))
          host = argv[++i];
        else if (argv[i].equalsIgnoreCase("-a"))
          name = argv[++i];
        else if (argv[i].equalsIgnoreCase("-p"))
          power = Long.parseLong(argv[++i]);
        else if (argv[i].equalsIgnoreCase("-c"))
          pathForCookie = argv[++i];
        else if (argv[i].equalsIgnoreCase("-q"))
          pathForQueue = argv[++i];
        else if (argv[i].equalsIgnoreCase("-s")) {
          notifyStatus = true;
          statusDelay = Integer.parseInt(argv[++i]);
        } else if (argv[i].equalsIgnoreCase("-noremove"))
          remove = false;
      }
    } catch (Exception n) {
      n.printStackTrace(System.err);
      System.err.println("xgridagent: error on parsing command line arguments");
      return false;
      //never reached
    }
    if (power == -1)
      power = computePower();
    if (name == null)
      name = loadName();
    if (pathForCookie.length() > 0 && !pathForCookie.endsWith(File.separator))
      pathForCookie += File.separator;
    if (pathForQueue.length() > 0 && !pathForQueue.endsWith(File.separator))
      pathForQueue += File.separator;
    return true;
  }

  public static void startAgentWithDefault() {
    if (agent != null) {
      System.out.println("xgridagent: already running!");
      return;
    }
    if (hostname == null || host == null)
      checkController();
    startAgent();
  }

  public static void stopAgent() {
    if (agent == null) {
      System.out.println("xgridagent: not running!");
      return;
    }
    shouldRun = false;
    agent.closeSession("Stop agent requested");
    agent = null;
  }

  public static void checkController() {
    hostname = checkHostNameAndAddress(hostname);
    setControllerHost(getControllerAddress(host));
  }

  public static String getControllerAddress(String host) {
    JmDNS jmdns = null;
    if (host == null)
      host = "-r";
    if (host.equalsIgnoreCase("-r")) {
      try {
        jmdns = new JmDNS();
      } catch (IOException e) {
        e.printStackTrace();
      }
      int trial = 0;
      while (host.equalsIgnoreCase("-r") && trial < 10) {
        final ServiceInfo[] infos = jmdns.list(xgridServiceString);
        for (int i = 0; i < infos.length; i++) {
          if (infos[i].getPort() == xgridport) {
            String host1 = infos[i].getHostAddress();
            if (host.equalsIgnoreCase("-r"))
              host = host1;
            controllerHostList.add(host1);
          }
        }
        try {
          Thread.sleep(1000 * trial);
        } catch (InterruptedException e) {
          break;
        }
        trial++;
      }
      if (jmdns != null) {
        try {
          jmdns.close();
          jmdns = null;
        } catch (Exception e) {
          e.printStackTrace();
        }
      }
    }
    return host;
  }

  public static String[] getControllersList() {
    int numberOfControllers = controllerHostList.size();
    String[] controllersList = new String[numberOfControllers];
    for (int i = 0; i < numberOfControllers; i++)
      controllersList[i] = (String) controllerHostList.elementAt(i);
    return controllersList;
  }

  public static String[] checkHostNameAndAddress(String[] hostname1) {
    if (hostname1 != null)
      return hostname1;
    JmDNS jmdns = null;
    try {
      jmdns = new JmDNS();
    } catch (IOException e) {
      e.printStackTrace();
    }
    hostname1 = new String[2];
    hostname1[0] = jmdns.getHostName();
    try {
      hostname1[1] = jmdns.getInterface().getHostAddress();
    } catch (IOException e) {
      e.printStackTrace();
    }
    jmdns.close();
    jmdns = null;
    return hostname1;
  }

  // load the cookie from a file
  public static String loadName() {
    if (saveName) {
      String agentName = "xgridAgent_" + Long.toString(System.currentTimeMillis());
      try {
        final BufferedReader br = new BufferedReader(new FileReader(pathForCookie + ".agentName"));
        agentName = br.readLine();
        br.close();
      } catch (FileNotFoundException e) {
        agentName = "xgridAgent_" + Long.toString(System.currentTimeMillis());
        saveName(agentName);
      } catch (IOException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
      return agentName;
    } else
      return "xgridAgent_" + hostname[0];
  }

  // save the cookie to a file
  public static void saveName(String agentName) {
    if (saveName) {
      try {
        final PrintWriter pw = new PrintWriter(new FileWriter(pathForCookie + ".agentName"));
        pw.println(agentName);
        pw.close();
      } catch (IOException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
  }

  public static long computePower() {
    System.out.println("Please wait for benchmark computing and speed estimation (about 15 secs)");
    Benchmark benchmark = new Benchmark();
    double score = benchmark.run();
    return (long) (12.65 * score); // normalized on G4 MHz
  }

  public static void setControllerHost(String controller) {
    host = controller;
    System.out.println("Xgrid: using controller " + host + "\n");
  }
}
