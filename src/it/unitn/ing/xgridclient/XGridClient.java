/*
 * @(#)XGridClient.java created Mar 29, 2006 Casalino
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

//import it.unitn.ing.rista.util.MaudPreferences;

import it.unitn.ing.rista.util.Misc;

import javax.jmdns.JmDNS;
import javax.jmdns.ServiceInfo;
import java.io.IOException;
import java.util.Vector;

/**
 * The XGridClient is a class for XGrid job submission
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

public class XGridClient {

  public static final String PRO_XGRIDCLIENT_URI = "http://www.apple.com/beep/xgrid/controller/client";
  public static boolean removeAll = false;
  public static String host;
  public static String[] hostname = null;
  public static double version = 1.0;
  public static Vector controllerHostList = new Vector(0, 1);
  public static int xgridport = 4111;

  public static String getVersion() {
    return "1.0";
  }

  public static void checkController() {
    hostname = checkHostNameAndAddress(hostname);
    setControllerHost(getControllerAddress(host));
  }

  public static void setControllerHost(String controller) {
    host = controller;
//    System.out.println("Xgrid: using controller " + host + "\n");
  }

  public static Client connectClient(String name) {
    if (hostname == null || host == null)
      checkController();
    // Initiate a session with the server
    // Start the client
    return new Client(name, hostname[0], hostname[1], host);
  }

  public static String submitJobAndWait(String title, String[] filenames, String[] filesBase64,
                                        String command, String[] arguments) {
    return submitJobAndWait("XGridClient", title, filenames, filesBase64, command, arguments);
  }

  public static String submitJobAndWait(String clientName, String title, String[] filenames, String[] filesBase64,
                                        String command, String[] arguments) {
    Client client = connectClient(clientName);
    if (client.channel != null) {
      try {
        return client.submitJobAndWait(title, filenames, filesBase64, command, arguments);
      } catch (Exception e) {
        e.printStackTrace();
        System.err.println("Failed getting results, trying again....");
        return Client.FAILED;
      }
    } else {
      System.err.println("No channel, trying again....");
      return Client.FAILED;
    }
  }

  public static String submitJob(String clientName, String title, String[] filenames, String[] filesBase64,
                                 String command, String[] arguments) {
    Client client = connectClient(clientName);
    if (client.channel != null)
      try {
        String jobid = client.submitJob(title, filenames, filesBase64, command, arguments);
//        System.out.println(jobid);
        return jobid;
      } catch (Exception e) {
        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        System.err.println("Failed submitting, trying again....");
        return Client.FAILED;
      }
    else {
      System.err.println("No channel, trying again....");
      return Client.FAILED;
    }
  }

  public static String getResults(String clientName, String jobIdentifier) {
    boolean request = true;
    while (request) {
      Client client = connectClient(clientName);
      try {
        if (client.channel != null)
          return client.getResults(jobIdentifier);
      } catch (Exception e) {
      }
      try {
        Thread.sleep(100000);
      } catch (InterruptedException e) {
        break;
      }
    }
    return Client.FAILED;
  }

  public static String getControllerAddress(String host) {
    JmDNS jmdns = null;
    if (host == null)
      host = XGridClientPreferences.getPref("xgrid.controllerAddress", "-r");
    if (host.equalsIgnoreCase("-r")) {
      try {
        jmdns = new JmDNS();
      } catch (IOException e) {
        e.printStackTrace();
      }
      int trial = 0;
      while (host.equalsIgnoreCase("-r") && trial < 10) {
        final ServiceInfo[] infos = jmdns.list("_xgrid._tcp.local.");
        for (int i = 0; i < infos.length; i++) {
          if (infos[i].getPort() == xgridport) {
            System.out.println("Service name: " + infos[i].getName());
            System.out.println("Service type: " + infos[i].getType());
            System.out.println("Service info: " + infos[i].getTextString());
            System.out.println("Service priority: " + infos[i].getPriority());
            System.out.println("Service weight: " + infos[i].getWeight());
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
    } else {
      controllerHostList.add(host);
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

}
