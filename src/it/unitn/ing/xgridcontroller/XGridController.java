/*
 * @(#)XGridController.java created Feb 17, 2006 Newark
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

import com.deadmoo.xgridagent.XGridAgent;

import javax.jmdns.JmDNS;
import javax.jmdns.ServiceInfo;
import java.io.*;

import it.unitn.ing.rista.util.Misc;


/**
 * The XGridController is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:05 $
 * @since JDK1.1
 */

public class XGridController {

  private static JmDNS jmdnsService = null;
  private static ServiceInfo xgridService = null;
  private static XGridConnectionServer xgridConnectionServer = null;

  public static void main(final String[] argv) {
    startController();
    boolean shouldRun = true;
      try {
        InputStreamReader userIn = new InputStreamReader(System.in);
        if (userIn == null) {
          System.out.println("No input stream, exiting...");
          System.exit(1);
        }
        BufferedReader user = new BufferedReader(userIn);
        while (shouldRun) {
          System.out.print("xgrid> ");
          try {
            System.out.flush();
            String command = user.readLine();
            if (command.equalsIgnoreCase("quit") ||
                    command.equalsIgnoreCase("q")) {
              stopController();
              shouldRun = false;
            } else if (command.equalsIgnoreCase("help") ||
                    command.equals("?")) {
              help();
            } else if (command.equalsIgnoreCase("conf")) {
              // Conf();
            } else if (command.equalsIgnoreCase("halt")) {
              stopController();
            } else if (command.equalsIgnoreCase("start")) {
              startController();
            } else if (command.equalsIgnoreCase("ps")) {
              status();
            } else {
              System.out.println(command +
                      ": not found");
            }

            try {
              Thread.sleep(100);
            } catch (InterruptedException ie) {
            }

          } catch (Exception ioe) {
            System.err.println("jpvm console: i/o " +
                    "exception.");
            ioe.printStackTrace();
            System.exit(1);
          }
        }
      } catch (Exception e) {
        System.out.println("Error, exiting...");
      }

  }

  public static void startController() {
    if (jmdnsService != null)
      return;
    try {
      jmdnsService = new JmDNS();
      String name = jmdnsService.getHostName();
      name = name.substring(0, name.indexOf(".")) + "-Maud";
      xgridService = new ServiceInfo(XGridAgent.xgridServiceString, name,
          XGridAgent.xgridport, "$servicePrincipal=xgrid/" + name + ".local");
      jmdnsService.registerService(xgridService);
      System.out.println("XGrid controller running");
      xgridConnectionServer = new XGridConnectionServer(XGridAgent.xgridport);
      xgridConnectionServer.start();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  public static void stopController() {
    if (jmdnsService == null)
      return;
    try {
      xgridConnectionServer.mustRun = false;
      xgridConnectionServer = null;
      jmdnsService.close();
      jmdnsService = null;
      xgridService = null;
      System.out.println("XGrid controller stopped");
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  private static void help() {
    System.out.println("Commands are:");
    System.out.println("  start\t- start the controller (if stopped)");
    System.out.println("  halt\t- stop controller");
    System.out.println("  help\t- Print helpful information " +
            "about commands");
    System.out.println("  ps\t- print information");
    System.out.println("  quit\t- stop and quit controller");
  }

  private static void status() {
    if (xgridService == null) {
      System.out.println("Controller not running!");
      return;
    }
    System.out.println("Service name: " + xgridService.getName());
    System.out.println("Service type: " + xgridService.getType());
    System.out.println("Service info: " + xgridService.getTextString());
    System.out.println("Service priority: " + xgridService.getPriority());
    System.out.println("Service weight: " + xgridService.getWeight());
  }
}
