/*
 * @(#)XGrid.java created Aug 31, 2007 Caen
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
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

import base64.Base64;

import java.io.*;
import java.util.Vector;

/**
 * The XGrid is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Aug 31, 2007 9:05:15 AM $
 * @since JDK1.1
 */
public class XGrid {

  public static String[] xgridFilenames = {"xgridhello.jar", "in.inp"};
  public static String baseJarBase64 = null;
  public static String[] arguments = {""};
  public static String command = "/usr/bin/java";
  public static int runningTasks = 0;
  public static String resultFile = "results.txt";
  public static OutputStream resultsOutput = null;
  public static String controllerHost = null;
  public static String inFolder = "";

  public static boolean retrieveData = true;

  /**
  * This is an example of how to use the XGridClient to submit java jobs
  *
  **/

  public static void main(String[] args) {
    String prefix = "in", suffix = ".inp";
    Vector javaArgs = new Vector(0, 1);

    if (args != null && args.length > 0) {

      // this is the work done when invoked from the the Agent
      for (int i = 0; i < args.length; i++) {
        if (args[i].equalsIgnoreCase("-in")) {
          i++;
          if (i < args.length) {
            inFolder += args[i];
          }

        }
        if (args[i].equalsIgnoreCase("-si")) {
          i++;
          if (i < args.length) {
            String filename = args[i];
            prefix = filename.substring(0, filename.lastIndexOf("#"));
            suffix = filename.substring(filename.lastIndexOf("#") + 1, filename.length());
          }
        }
        if (args[i].equalsIgnoreCase("-so")) {
          i++;
          if (i < args.length) {
            resultFile = args[i];
          }
        }
        if (args[i].toLowerCase().startsWith("-h")) {
          i++;
          if (i < args.length) {
            controllerHost = args[i];
          }
        }
        if (args[i].toLowerCase().startsWith("-run")) {
          i++;
          if (i < args.length) {
            xgridFilenames[0] = args[i];
            if (xgridFilenames[0].toLowerCase().endsWith(".jar")) {
              command = "java";
              javaArgs.add("-jar");
              javaArgs.add(xgridFilenames[0]);
            } else
              command = xgridFilenames[0];
            baseJarBase64 = XGridClientMessage.loadfile(inFolder + xgridFilenames[0]);
          }
        }
        if (args[i].toLowerCase().startsWith("-cmd")) {
          i++;
          if (i < args.length) {
            command = args[i];
          }
        }
        if (args[i].toLowerCase().startsWith("-args")) {
          i++;
          if (i < args.length) {
            javaArgs.add(args[i]);
          }
        }
        if (args[i].toLowerCase().startsWith("-usage") || args[i].toLowerCase().startsWith("-help")) {
          usage();
          end(0);
        }
      }

      if (javaArgs.size() > 0) {
        arguments = new String[javaArgs.size()];
        for (int i = 0; i < javaArgs.size(); i++) {
          arguments[i] = (String) javaArgs.elementAt(i);
          System.out.println(arguments[i]);
        }
      }

      XGrid xgrid = new XGrid();
      xgrid.initXGrid();

      // submitting ten times the same job only a different name

      retrieveData = false; // we do not want to retrieve data immediately (as an
                            // example, you may prefer retrieving it immediately
      int i = 0;
      String filename;
      xgridFilenames[1] = prefix + suffix;

      while ((filename = getNextFilename(inFolder + prefix, i, suffix)) != null) {
        xgrid.submitJob(i++, filename);
      }

      System.out.println("Tasks submitted, we wait 10 secs before starting asking for the results.");

      try {
        Thread.sleep(10000); // we wait 10 secs before retrieving the data
      } catch (InterruptedException ie) {
        ie.printStackTrace(System.err);
      }
      retrieveData = true; // now we retrieve everything

      while (runningTasks > 0) { // now we don't exit from the program until all tasks
                               // are done
        try {
          Thread.sleep(1000);
        } catch (InterruptedException ie) {
          ie.printStackTrace(System.err);
        }
      }

    } else {

      // the XGrid main job submission

      usage();
    }
    end(0);
  }

  public static void end(int result) {
    if (resultsOutput != null)
      try {
        resultsOutput.close();
      } catch (IOException e) {
        e.printStackTrace();
      }
    System.exit(result);
  }

  private static void usage() {
    System.out.println("Usage:");
    System.out.println("-run executable [-h controller_hostname] [-si fileInput] [-so fileOutput] [-cmd commandLine]");
    System.out.println("where all arguments except -run executable are opzional:");
    System.out.println("   -usage or -help will print this message");
    System.out.println("   executable is the name of the executable to run (full path)");
    System.out.println("   controller_hostname is the address or hostname on the controller, no controller_hostname will resolve ");
    System.out.println("         the controller via bonjour");
    System.out.println("   fileInput is the input file for the executable (full path), to launch more jobs create more input files ");
    System.out.println("         in the same directory with the same name and a progressive index starting from 0, the index should ");
    System.out.println("         be located in the name of the file and its position indicated with a #. ");
    System.out.println("   For example if you use as input file for the executable input.dat then generate");
    System.out.println("         the following input files: input0.dat, input1.dat, input2.dat etc. ");
    System.out.println("         and use: input#.dat as fileInput for the -si argument.");
    System.out.println("         If no name is provided the default in#.inp will be used.");
    System.out.println("   fileOutput is the name of the file where to save the results (full path), if not provided the default");
    System.out.println("         results.txt will be used");
    System.out.println("   commandLine is an optional parameter to pass a command line to be used to run the task; put the pure");
    System.out.println("         name of the executable or the full command for a jar file. When omitted the java command plus");
    System.out.println("         the jar file passed in the executable is used.");
  }

  static void initXGrid() {

    if (controllerHost == null) {
    // initialize the XGrid and check for available controllers around
    XGridClient.checkController();

    // now we ask for the list of controllers found to print it
    // we shows also how to choose for example the last controller in the list
    String[] controllerHostList = XGridClient.getControllersList();
    if (controllerHostList == null || controllerHostList.length < 1) {
      System.out.println("No XGrid controllers found! Exiting....");
      end(0);
    }
    System.out.println("List of available controllers, we choose the last one");
    for (int i = 0; i < controllerHostList.length; i++)
      System.out.println(controllerHostList[i]);
    // we select the last available controller
    XGridClient.setControllerHost(controllerHostList[controllerHostList.length - 1]);
    } else {
      XGridClient.setControllerHost(controllerHost);
    }

    // we pre-encode the jar file and setup file names and java arguments
    if (baseJarBase64 == null) {
      // this load the jar file and return it as base64 encoded
      baseJarBase64 = XGridClientMessage.loadfile(xgridFilenames[0]);
      arguments = new String[]{"-mx128M", "-jar", xgridFilenames[0], "-hello", xgridFilenames[1]};
    }

    resultsOutput = getOutputStream(resultFile);
  }


  public void submitJob(final int index, final String inputFilename) {

    (new Thread() {
      public void run() {
        String resultData = null;

        String[] filesBase64ToSubmit = new String[2];
        filesBase64ToSubmit[0] = baseJarBase64;
        // we do not need the real file, we just use the file content and the file Hello.txt will
        // be created on the agent only. If you have a real file, then you need to read it and
        // encode base64 as we do here for the String
        filesBase64ToSubmit[1] = XGridClientMessage.loadfile(inputFilename);
        boolean success = false;
        runningTasks++;
        while (!success) {
          System.out.println("Submitting job number " + index);
          String clientName = "XGridClient_" + index;
          success = true; // todo to be removed
          String jobId = XGridClient.submitJob(clientName,
              "myJob_number_" + index,
              xgridFilenames, filesBase64ToSubmit, command,
              arguments);
          if (!jobId.equals(Client.FAILED)) {
            // if retrieveData is false, we do not start retrieving results until it becomes true
            // you can use it to decide if the data should be retrieved soon or later
            while (!retrieveData) {
              try {
                Thread.sleep(1000);
              } catch (InterruptedException ie) {
                ie.printStackTrace(System.err);
              }
            }
            System.out.println("Retrieving data for job number " + index);
            // here we retrieve the results
            resultData = XGridClient.getResults(clientName, jobId);
            // we print the results on the console
            System.out.println(resultData);
            try {
              resultsOutput.write(resultData.getBytes());
              resultsOutput.flush();
            } catch (IOException e) {
              e.printStackTrace();
            }
            if (!resultData.equals(Client.CANCELED) && !resultData.equals(Client.FAILED)) {
              success = true; // we received the correct data, the job has been successful
            } else {
                // we got a problem here, the job was cancelled or failed, we wait 10 secs and resubmit it
                try {
                  Thread.sleep(10000);
                } catch (InterruptedException ie) {
                  ie.printStackTrace(System.err);
                }
            }
          } else {
            // we got a problem here, the job failed, we wait 10 secs and resubmit it
            try {
              Thread.sleep(10000);
            } catch (InterruptedException ie) {
              ie.printStackTrace(System.err);
            }
          }
        }
        runningTasks--;
      }
    }).start();
    // we pause a little before submitting the next job
    try {
      Thread.sleep(500);
    } catch (InterruptedException ie) {
      ie.printStackTrace(System.err);
    }
  }

  public String getStringAsBase64String(String aTextString) {
    ByteArrayOutputStream stream = new ByteArrayOutputStream();
    BufferedWriter br = new BufferedWriter(new PrintWriter(stream));
    try {
      br.write(aTextString);
    } catch (IOException ie) {
      ie.printStackTrace(System.err);
    }
    return Base64.encodeBytes(stream.toByteArray());
  }

  public static final OutputStream getOutputStream(String filename) {
    OutputStream out = null;
    try {
      out = new FileOutputStream(new File(filename));
      out = new BufferedOutputStream(out);
    } catch (IOException e) {
      e.printStackTrace();
      end(1);
    }
    return out;
  }

  public static String getNextFilename(String prefix, int i, String suffix) {
    String filename = prefix + Integer.toString(i) + suffix;
    if ((new File(filename)).exists())
      return filename;
    return null;
  }
}
