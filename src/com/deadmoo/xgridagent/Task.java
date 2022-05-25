package com.deadmoo.xgridagent;

import java.io.*;
import java.util.*;

import base64.Base64;

public class Task extends Thread {
  // properties of a task
  private long taskRef;
  private HashMap inputFiles;
  private String inputStream;
  private HashMap outputFiles;
  private String readData;
  private String errData;
  private String[] arguments;
  private String pathCommand;
  private long taskMegahertz;
  private String status;
  private String dataAvailable;
  private long terminationResult;
  private Agent agent;
  private File tempdir = null;
  private boolean isWindows;
  private Process process;
  static final String WORKING_DIR = "working_directory";
  static final String QUEUE_DIR = "queue";

  // task status
  /** The String for a task status of running. */
  public static final String RUNNING = "Running";
  /** The String for a task status of terminated */
  public static final String TERMINATED = "Terminated";

  // class for creating temp directory
  private static class TempDir {
    private static final int TEMP_MAX = 100;
    private static final File TEMP_LOCATION = new File(XGridAgent.pathForQueue + QUEUE_DIR);

    /**
     * Creates a temp directory in the System temp directory.
     */
    public static File create(final String prefix) throws IOException {
      return TempDir.create(prefix, TEMP_LOCATION);
    }

    /**
     * Creates a temp directory in a given directory.
     */
    private static File create(final String prefix, final File directory) throws IOException {
      for (int i = 0; i < TEMP_MAX; i++) {
        final File tempFile = File.createTempFile(prefix, "", directory);
        if (tempFile.delete() && tempFile.mkdir()) return tempFile;
      }
      throw new IOException("Could not create temporary working directory.");
    }
  }

  // class for deleting temp directory
  private static class DirDeleter {
    public static void deleteDirectory(final File dir) {
      if (dir != null) {
        final File[] fileArray = dir.listFiles();
        if (fileArray != null) {
          for (int i = 0; i < fileArray.length; i++) {
            if (fileArray[i].isDirectory())
              deleteDirectory(fileArray[i]);
            else
              fileArray[i].delete();
          }
        }
        dir.delete();
      }
    }
  }

  // takes the inputstream and dumps it to a base64 encoded string
  private class StreamGobbler extends Thread {
    private final InputStream is;
    private final ByteArrayOutputStream bos;
    private String data;

    StreamGobbler(String name, InputStream is) {
      super();
      this.is = is;
      this.bos = new ByteArrayOutputStream();
      this.setName(name);
    }

    public void run() {
      try {
        final PrintWriter pw = new PrintWriter(bos);

        final InputStreamReader isr = new InputStreamReader(is);
        final BufferedReader br = new BufferedReader(isr);
        String line;
        while ((line = br.readLine()) != null && process != null) {
          pw.println(line);
        }
        pw.flush();
        pw.close();
        data = Base64.encodeBytes(bos.toByteArray());
      } catch (IOException ioe) {
        ioe.printStackTrace();
      }
    }

    public String getData() {
      return data;
    }
  }

  // decodes a base64 string and dumps it to outputstream
  private class StreamPutter extends Thread {
    private final OutputStream os;
    private final ByteArrayInputStream bis;

    StreamPutter(String name, OutputStream os, String data) {
      super();
      this.os = os;
      this.bis = new ByteArrayInputStream(Base64.decode(data));
      this.setName(name);
    }

    public void run() {

// Luca: I substituted this because the original code was generating a wrong file when over a certain size
      try {
        final BufferedInputStream in = new BufferedInputStream(bis);
        int mark = 0;
        int available = in.available();
        byte[] bytes = new byte[available];
        while (available > 0 && in.read(bytes, mark, available) != -1 && process != null) {
          os.write(bytes);
          mark += available;
          available = in.available();
          if (available > 0)
            bytes = new byte[available];
        }
        os.flush();
        os.close();
        in.close();
      } catch (IOException io) {
        io.printStackTrace();
      }
/*			try {
				final PrintWriter pw = new PrintWriter(os);
				
				final InputStreamReader isr = new InputStreamReader(bis);
				final BufferedReader br = new BufferedReader(isr);
				String line = null;
				while ((line = br.readLine()) != null && process != null)
				{
					pw.println(line);
				}
				pw.flush();
				pw.close();
			} catch (IOException ioe) {
				ioe.printStackTrace();  
			}*/
    }
  }

  /**
   * create a new task
   *
   * @param agent         the agent for this task
   * @param command       the command to run
   * @param arguments     the arguments for the command
   * @param inputFiles    the files for the working directory
   * @param inputStream   the file to get standard input from
   * @param taskMegahertz the CPU power used by this task
   * @param taskRef       the task reference
   */
  public Task(Agent agent, String command, String[] arguments, HashMap inputFiles,
              String inputStream, long taskMegahertz, long taskRef) {
    super();
    this.agent = agent;
    pathCommand = command;
    this.arguments = arguments;
    this.inputFiles = inputFiles;
    this.inputStream = inputStream;
    this.taskMegahertz = taskMegahertz;
    this.taskRef = taskRef;
    outputFiles = new HashMap();
    this.setName("Xgrid Task " + taskRef);
    if (!TempDir.TEMP_LOCATION.exists())
      if (!TempDir.TEMP_LOCATION.mkdir())
        if (!TempDir.TEMP_LOCATION.mkdirs())
          System.err.println("Warning: not able to create queue tmp dir at: " +
              TempDir.TEMP_LOCATION.getAbsolutePath());
  }

  /**
   * run this task
   */
  public void run() {
    // are we running on windows
    if (System.getProperty("os.name").startsWith("Windows"))
      isWindows = true;
    else
      isWindows = false;

    // write out the input files
    writeInputFiles();

    // get the shell script
    final File script = setupShellScript();

    // the task is running
    status = RUNNING;
    dataAvailable = XGridMessage.NO;
      agent.sendMSG(new XGridMessage(taskRef, XGridMessage.TASK_STATUS, XGridMessage.NOTIFICATION, this));
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
        if (inputStream != null) {
          final StreamPutter inPutter = new StreamPutter(this.getName() + " stdin", process.getOutputStream(), (String) ((HashMap) inputFiles.get(inputStream)).get(XGridMessage.FILE_DATA));
          inPutter.start();
          inPutter.join();
        }
        terminationResult = process.waitFor();
        errGobbler.join();
        readGobbler.join();
        errData = errGobbler.getData();
        readData = readGobbler.getData();
      } catch (IOException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      } catch (InterruptedException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }

      // delete the shell script
      if (XGridAgent.remove)
        script.delete();

      // grab the output files
      grabOutputFiles(tempdir);

      // the task has terminated
      status = TERMINATED;
      dataAvailable = XGridMessage.YES;
      taskMegahertz = 0;
      agent.sendMSG(new XGridMessage(taskRef, XGridMessage.TASK_STATUS, XGridMessage.NOTIFICATION, this));
      agent.sendMSG(new XGridMessage(-1, XGridMessage.AGENT_STATUS, XGridMessage.NOTIFICATION, agent));
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
    final String dir = tempdir.getAbsolutePath();
    String exec = "exec ";

    pathCommand = pathCommand.replace('/', File.separatorChar);

    //if a relative path is given
    if (pathCommand.toLowerCase().endsWith("java")) {
      String path = System.getProperty("java.home");
      if (!isWindows) {
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
    if (!isWindows) {
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
      if (!isWindows) {
        script = File.createTempFile("wrapper_script", ".sh", TempDir.TEMP_LOCATION);
      } else {
        script = File.createTempFile("wrapper_script", ".bat", TempDir.TEMP_LOCATION);
      }

      final PrintWriter pw = new PrintWriter(new FileWriter(script));
      if (!isWindows) {
        pw.println("#!/bin/sh");
      } else {
        pw.println("@echo off");
      }
      pw.println("cd " + dir);
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
    if (!isWindows) {
      makeExecutable(script.getAbsolutePath());
    }
    return script;
  }

  // write out any inputFiles
  private void writeInputFiles() {
    try {
      tempdir = TempDir.create(WORKING_DIR);
    } catch (IOException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    final int length = inputFiles.size();
    String[] files = new String[length];
    files = (String[]) inputFiles.keySet().toArray(files);
    for (int i = 0; i < files.length; i++) {
      String filename = files[i];
      if (filename.compareTo(XGridMessage.STDIN) != 0) {
        final HashMap dict = (HashMap) inputFiles.get(filename);
        final String fileData = (String) dict.get(XGridMessage.FILE_DATA);
        final String isExecutable = (String) dict.get(XGridMessage.IS_EXECUTABLE);
        filename = filename.replace('/', File.separatorChar);
        final int lastSlash = filename.lastIndexOf(File.separatorChar);
        if (lastSlash != -1) {
          final String thisPath = filename.substring(0, lastSlash);
          final File thisDir = new File(tempdir.getAbsoluteFile(), thisPath);
          thisDir.mkdirs();
        }
        final File outFile = new File(tempdir.getAbsolutePath(), filename);
        try {
          final FileOutputStream fos = new FileOutputStream(outFile);
          fos.write(Base64.decode(fileData));
          fos.flush();
          fos.close();
        } catch (IOException e) {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }

        // Luca trick to make it working with the Mandelbrot Apple example
        boolean shouldBeExecutable = false;
        if (isExecutable != null && isExecutable.compareTo(XGridMessage.YES) == 0)
          shouldBeExecutable = true;
        else if (pathCommand.indexOf(filename) >= 0)
          shouldBeExecutable = true;
        if (!isWindows && shouldBeExecutable) {
          makeExecutable(outFile.getAbsolutePath());
        }
      }
    }
  }

  // grab any output files
  private void grabOutputFiles(final File dir) {
    final File[] fileArray = dir.listFiles();
    if (fileArray != null) {
      for (int i = 0; i < fileArray.length; i++) {
        if (fileArray[i].isDirectory()) {
          grabOutputFiles(fileArray[i]);
        } else {
          String filename = fileArray[i].getAbsolutePath().replace(File.separatorChar, '/');
          final String tempPath = tempdir.getAbsolutePath().replace(File.separatorChar, '/') + "/";
          filename = filename.replaceAll(tempPath, "");
          if (!inputFiles.containsKey(filename)) {
            final ByteArrayOutputStream bos = new ByteArrayOutputStream();
            try {
              final DataInputStream dis = new DataInputStream(new BufferedInputStream(
                  new FileInputStream(fileArray[i])));
              final DataOutputStream dos = new DataOutputStream(new BufferedOutputStream(bos));
              try {
                int bytesRead = 0;
                final byte[] buffer = new byte[1024];

                while (bytesRead != -1) {
                  bytesRead = dis.read(buffer);
                  dos.write(buffer, 0, bytesRead);
                }
              } catch (IOException ioe) {
                ioe.printStackTrace();
              } catch (IndexOutOfBoundsException e) {
                dos.close();
                dis.close();
              }
            } catch (FileNotFoundException e) {
              // TODO Auto-generated catch block
              e.printStackTrace();
            } catch (IOException e) {
              // TODO Auto-generated catch block
              e.printStackTrace();
            }
            final String fileData = Base64.encodeBytes(bos.toByteArray());
            outputFiles.put(filename, fileData);
          }
        }
      }
    }
  }

  public void cleanUp() {
    if(process != null) process.destroy();
    if (XGridAgent.remove)
      if(tempdir != null) DirDeleter.deleteDirectory(tempdir);
  }

  // BEGIN getters and setters
  public String getDataAvailable() {
    return dataAvailable;
  }

  public String getStatus() {
    return status;
  }

  public long getTaskMegahertz() {
    return taskMegahertz;
  }

  public long getTaskRef() {
    return taskRef;
  }

  public long getTerminationResult() {
    return terminationResult;
  }

  public String getErrData() {
    return errData;
  }

  public String getReadData() {
    return readData;
  }

  public HashMap getOutputFiles() {
    return outputFiles;
  }

  public Agent getAgent() {
    return agent;
  }

  public boolean isWindows() {
    return isWindows;
  }
  // END getters and setters

}
