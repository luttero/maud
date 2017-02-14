/*
 * @(#)Executable.java created Jan 9, 2008 Caen
 *
 * Copyright (c) 2008 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.util;

import java.io.*;

/**
 * The Executable is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jan 9, 2008 2:36:54 PM $
 * @since JDK1.1
 */
public class Executable extends Thread {

  String pathCommand = null;
  String[] arguments = null;
  Process process = null;
  private String status;
  // task status
  /** The String for a task status of starting. */
  public static final String PREPARING = "Preparing";
  /** The String for a task status of running. */
  public static final String RUNNING = "Running";
  /** The String for a task status of terminated */
  public static final String TERMINATED = "Terminated";

  public boolean remove = true;
  private long terminationResult;
//  private String readData;
//  private String errData;

  String workingDir = null;


  public Executable(String command, String directory, String[] args) {
    super();
    pathCommand = command;
    workingDir = directory;
    arguments = args;
    status = PREPARING;
    if (args != null)
      setName("command running for file " + args[0]);
    else
      setName("command running");
    if (Constants.testing)
      remove = MaudPreferences.getBoolean("superflip_script.remove", false);
  }

  public void run() {
    // get the shell script
    final File script = setupShellScript();
    status = RUNNING;

    // the task is running
      try {
        // run the shell script
        process = Runtime.getRuntime().exec(script.getAbsolutePath());
        // gobble error message
        final StreamGobbler errGobbler = new StreamGobbler(this.getName() + " stderr", process.getErrorStream());
        // gobble output
        final StreamGobbler readGobbler = new StreamGobbler(this.getName() + " stdout", process.getInputStream());
        // kick off gobblers
        errGobbler.start();
        readGobbler.start();
        // send input stream
/*        if (inputStream != null) {
          final StreamPutter inPutter = new StreamPutter(this.getName() + " stdin", process.getOutputStream(),
              (String) ((HashMap) inputFiles.get(inputStream)).get(XGridMessage.FILE_DATA));
          inPutter.start();
          inPutter.join();
        }*/
        terminationResult = process.waitFor();
        errGobbler.join();
        readGobbler.join();
//        errData = errGobbler.getData();
//        readData = readGobbler.getData();
      } catch (IOException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      } catch (InterruptedException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }

      // delete the shell script
      if (remove)
        script.delete();

      // grab the output files
//      grabOutputFiles(tempdir);

      // the task has terminated
      status = TERMINATED;
  }

  private void makeExecutable(final String path) {
    // run the shell script
    try {
      final Process process = Runtime.getRuntime().exec("chmod +x " + path);
      // gobble error message
      final StreamGobbler errGobbler = new StreamGobbler(this.getName() + " chmod stderr", process.getErrorStream());
      // gobble output
      final StreamGobbler readGobbler = new StreamGobbler(this.getName() + " chmod stdout", process.getInputStream());
      // kick off gobblers
      errGobbler.start();
      readGobbler.start();
      process.waitFor();
      errGobbler.join();
      readGobbler.join();
    } catch (IOException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    } catch (InterruptedException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  // setup the shell script that changes into working directory and runs command
  private File setupShellScript() {
    String exec = "exec ";

    pathCommand = pathCommand.replace('/', File.separatorChar);

    //if a relative path is given
    if (pathCommand.toLowerCase().endsWith("java")) {
      String path = System.getProperty("java.home");
      if (!Constants.windoze) {
        exec = path + "/bin/java";
      } else {
        pathCommand = path + "\\bin\\java";
      }
    } else if (pathCommand.charAt(0) != File.separatorChar) {
      exec += "." + File.separator + pathCommand;
    } else {
      //if an absolute path is given
      exec += pathCommand;
    }
    if (!Constants.windoze) {
      exec.replaceAll(" ", "\\ ");
    } else {
      exec = "\"" + pathCommand + "\"";
    }
    if (arguments != null && arguments[0] != null && !arguments[0].equalsIgnoreCase("null")) {
      for (int i = 0; i < arguments.length; i++) {
        exec += " " + arguments[i];
      }
    }
    //create a wrapper script to run this specific 'Task'
    File script = null;
    try {
      //if *nix, make a bourne shell script, if win, make a batch file
      if (!Constants.windoze) {
        script = new File(workingDir + "wrapper_script.sh");
      } else {
        script = new File(workingDir + "wrapper_script.bat");
      }

      final PrintWriter pw = new PrintWriter(new FileWriter(script));
      if (!Constants.windoze) {
        pw.println("#!/bin/sh");
      } else {
        pw.println("@echo off");
      }
      pw.println("cd " + workingDir);
      pw.println(exec);
      pw.flush();
      pw.close();
    } catch (FileNotFoundException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    } catch (IOException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    if (!Constants.windoze) {
      makeExecutable(script.getAbsolutePath());
    }
    return script;
  }

  public String getStatus() {
    return status;
  }

  public long getTerminationResult() {
    return terminationResult;
  }

  public void cleanUp() {
    if(process != null) process.destroy();
  }

  // takes the inputstream and dumps it to a base64 encoded string
  private class StreamGobbler extends Thread {
    private final InputStream is;
//    private final ByteArrayOutputStream bos;
//    private String data;

    StreamGobbler(String name, InputStream is) {
      super();
      this.is = is;
//      this.bos = new ByteArrayOutputStream();
      this.setName(name);
    }

    public void run() {
      try {
//        final PrintWriter pw = new PrintWriter(bos);

        final InputStreamReader isr = new InputStreamReader(is);
        final BufferedReader br = new BufferedReader(isr);
        String line;
        while ((line = br.readLine()) != null && process != null) {
          System.out.println(line);
        }
 //       pw.flush();
//        pw.close();
//        data = bos.toString();
      } catch (IOException ioe) {
        ioe.printStackTrace();
      }
    }

 /*   public String getData() {
      return data;
    }*/
  }

}
