/*
 * @(#)XGridJavaLauncher.java created Apr 6, 2006 Casalino
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

package it.unitn.ing.xgridagent;

import java.util.StringTokenizer;
import java.util.jar.JarFile;
import java.io.*;
import java.net.URL;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import base64.Base64;

/**
 * The XGridJavaLauncher is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:05 $
 * @since JDK1.1
 */

public class XGridJavaLauncher {

  static final String WORKING_DIR = "working_directory";
  public static String startPath = new String("/");
  public static boolean windoze = false;
  public static boolean macos = false;
  public static boolean macosx = false;
  public static String pathForQueue = "";
  public static String fileJar = "Maud_essential.jar";
  public static String[] javaArguments = new String[]{"-mx512M", "-jar", ".." + File.separator + fileJar};

  public static void main(String[] args) {
    checkSystem();
    args = preprocessArguments(args);
    File shellFile = setupShellScript(args);
    (new XGridJavaLauncher()).launch(shellFile);
    System.exit(0);
  }

  public XGridJavaLauncher() {
  }

  public void launch(File shellFile) {
    try {
      if (!windoze) {
        Process cprocess = Runtime.getRuntime().exec("chmod a+x " + shellFile.getAbsolutePath());
        cprocess.waitFor();
      }
      final Process process = Runtime.getRuntime().exec(shellFile.getAbsolutePath());
      // gobble error message
      final StreamGobbler errGobbler = new StreamGobbler(process.getErrorStream(), process);
      // gobble output
      final StreamGobbler readGobbler = new StreamGobbler(process.getInputStream(), process);
      // kick off gobblers
      errGobbler.start();
      readGobbler.start();
      process.waitFor();
      errGobbler.join();
      readGobbler.join();
      System.out.print(readGobbler.getData());
//      if (errGobbler.getData() != null && !errGobbler.getData().equals(""))
//        System.err.print(errGobbler.getData());
    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
    // delete the shell script
//    shellFile.delete();
  }

  static File setupShellScript(String[] arguments) {
    String exec = "";

    String path = System.getProperty("java.home");
    if (!windoze) {
      exec += path + "/bin/java";
      exec.replaceAll(" ", "\\ ");
    } else {
      exec = path + "\\bin\\java";
      exec = "\"" + exec + "\"";
    }
    if (arguments != null) {
      for (int i = 0; i < arguments.length; i++) {
        exec += " " + arguments[i];
      }
    }
    //create a wrapper script to run this specific 'Task'
    File script = null;
    try {
      //if *nix, make a bourne shell script, if win, make a batch file
      if (!windoze) {
        script = File.createTempFile("launch", ".sh", new File(pathForQueue));
      } else {
        script = File.createTempFile("launch", ".bat", new File(pathForQueue));
      }

      final PrintWriter pw = new PrintWriter(new FileWriter(script));
      if (!windoze) {
        pw.println("#!/bin/sh");
      } else {
        pw.println("@echo off");
      }
      pw.println("cd " + pathForQueue);
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
    return script;
  }

// Utilities

  public static String[] preprocessArguments(String[] myArguments) {
    int argsNumber = myArguments.length;
    int[] acceptArg = new int[argsNumber];
    int ftpArgs = 0;
    int httpArgs = 0;
    for (int i = 0; i < argsNumber; i++) {
      acceptArg[i] = 0;
      if (myArguments[i].equalsIgnoreCase("-getbyurl")) {
        httpArgs ++;
        acceptArg[i++] = -1;
        acceptArg[i] = 1;
      }
    }
    if (httpArgs + ftpArgs == 0)
      return myArguments;
    String[] newArguments = new String[argsNumber - httpArgs * 2 - ftpArgs * 2 + javaArguments.length];
    int index = 0;
    for (int i = 0; i < javaArguments.length; i++)
      newArguments[index++] = javaArguments[i];
    for (int i = 0; i < argsNumber; i++) {
      switch (acceptArg[i]) {
        case 0:
          newArguments[index++] = myArguments[i];
          break;
        case 1:
          getFileByURL(myArguments[i]); // not implemented still
          break;
        default: {}
      }
    }
    return newArguments;
  }

  public static void getFileByURL(String instructions) {
    instructions = new String(Base64.decode(instructions));
    String filename = fileJar;
    String useraddress = null;
    String md5sum = null;
    StringTokenizer st = new StringTokenizer(instructions, ";\t\r\n");
//    if (st.hasMoreTokens()) filename = st.nextToken();
    if (st.hasMoreTokens()) md5sum = st.nextToken();
    if (st.hasMoreTokens()) useraddress = st.nextToken();
    File file = new File(pathForQueue + File.separator + ".." + File.separator + filename);
    if (file.exists()) {
      if (md5sum != null) {
//        String local_md5sum = getMD5sumForFile(file);
//        if (local_md5sum != null && local_md5sum.equals(md5sum))
          return; // same version of jar file
      }
    }
         try {
           int index = useraddress.indexOf("/", 8);
           String address = useraddress;
           String location = "";
           if (index > 8) {
            address = useraddress.substring(0, index + 1);

            if (index + 1 < useraddress.length())
             location = useraddress.substring(index + 1);
            if (location.length() > 0 && !location.endsWith("/"))
              location += "/";
           }
           if (!address.endsWith("/"))
            address += "/";
           // Create an instance of HttpClient.

           URL url = new URL(address+location+filename);
           InputStream in = url.openStream();
           final File outFile = new File(pathForQueue + File.separator + ".." + File.separator, filename);
           final FileOutputStream fos = new FileOutputStream(outFile);

           byte[] b = new byte[1024];
           int len;

           while ((len = in.read(b)) != -1) {
             //write byte to System.out
             fos.write(b, 0, len);
             b = new byte[1024];
           }

           in.close();
           fos.flush();
           fos.close();
           if (!windoze) {
             Process cprocess = Runtime.getRuntime().exec("chmod a+x " + outFile.getAbsolutePath());
             cprocess.waitFor();
           }

         } catch (Exception ioe) {
           ioe.printStackTrace();
         }

  }

/*  public static void getFileByHTTP(String instructions) {
    instructions = new String(Base64.decode(instructions));
    String filename = fileJar;
    String useraddress = null;
    String md5sum = null;
    StringTokenizer st = new StringTokenizer(instructions, ";\t\r\n");
//    if (st.hasMoreTokens()) filename = st.nextToken();
    if (st.hasMoreTokens()) md5sum = st.nextToken();
    if (st.hasMoreTokens()) useraddress = st.nextToken();
    File file = new File(pathForQueue + File.separator + ".." + File.separator + filename);
    if (file.exists()) {
      if (md5sum != null) {
        String local_md5sum = getMD5sumForFile(file);
        if (local_md5sum != null && local_md5sum.equals(md5sum))
          return; // same version of jar file
      }
    }
    byte[] data = null;
         try {
           int index = useraddress.indexOf("/", 8);
           String address = useraddress;
           String location = "";
           if (index > 8) {
            address = useraddress.substring(0, index + 1);

            if (index + 1 < useraddress.length())
             location = useraddress.substring(index + 1);
            if (location.length() > 0 && !location.endsWith("/"))
              location += "/";
           }
           if (!address.endsWith("/"))
            address += "/";
//           System.out.println(address);
//           System.out.println(location + filename);
           URL httpURL = new URL(address);
             HTTPConnection con = new HTTPConnection(httpURL);
             HTTPResponse   rsp = con.Get(location + filename);
             if (rsp.getStatusCode() >= 300) {
               System.err.println("Received Error: "+rsp.getReasonLine());
               System.err.println(rsp.getText());
             } else
               data = rsp.getData();
         } catch (Exception ioe) {
           ioe.printStackTrace();
         }
    if (data == null)
      return;
    final File outFile = new File(pathForQueue + File.separator + ".." + File.separator, filename);
    try {
      final FileOutputStream fos = new FileOutputStream(outFile);
      fos.write(data);
      fos.flush();
      fos.close();
    } catch (IOException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

  }*/

  public static String getMD5sumForFile(File file) {
// obtain a message digest object
    MessageDigest md = null;
    try {
      md = MessageDigest.getInstance("MD5");
    } catch (NoSuchAlgorithmException e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }

// calculate the digest for the given file
    BufferedInputStream in = null;
    try {
      in = new BufferedInputStream(new FileInputStream(file));
    } catch (FileNotFoundException e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
      return null;
    }
    int mark = 0;
    StringBuffer content = new StringBuffer("");
    try {
      int available = in.available();
      byte[] bytes = new byte[available];
      while (available > 0 && in.read(bytes, mark, available) != -1) {
        md.update(bytes);
        mark += available;
        available = in.available();
        if (available > 0)
          bytes = new byte[available];
      }

    } catch (IOException e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
      return null;
    }
    return new String(md.digest());
  }

  public static BufferedInputStream getJarEntryInputStream(File file, String resourceName) {
    if (file.exists()) {
      try {
        JarFile jarFile = new JarFile(file);
        InputStream is = jarFile.getInputStream(jarFile.getJarEntry(resourceName));
        return new BufferedInputStream(is);
      } catch (IOException io) {
        io.printStackTrace();
      }
    }
    return null;
  }

  public static String getEntryContentAsString(BufferedInputStream in) {
    if (in == null)
      return null;
    int mark = 0;
    StringBuffer content = new StringBuffer("");
    try {
      int available = in.available();
      byte[] bytes = new byte[available];
      while (available > 0 && in.read(bytes, mark, available) != -1) {
        content.append(bytes);
        mark += available;
        available = in.available();
        if (available > 0)
          bytes = new byte[available];
      }
    } catch (IOException io) {
      io.printStackTrace();
    } finally {
      try {
        if (in != null)
          in.close();
      } catch (IOException io1) {
        io1.printStackTrace();
      }
    }
    return content.toString();
  }

  public static final String filterFileName(String filename) {
    if (filename == null || filename.equals(""))
      return filename;
    StringBuffer tmp = new StringBuffer(filename);
    for (int i = 0; i < tmp.length(); i++) {
      if (tmp.charAt(i) == '\\')
        tmp.setCharAt(i, '/');
    }
    String startpath = "";
    String astring = tmp.toString();
    if (!astring.startsWith("//") && astring.startsWith("/"))
      startpath = startPath;
    else if (windoze && (astring.length() > 1 && astring.charAt(1) == ':'))
      startpath = startPath;
    return new String(startpath + astring);
  }

  public static void checkSystem() {
    pathForQueue = filterFileName(System.getProperty("user.dir"));
    String osName = System.getProperty("os.name");
    if (osName != null && osName.toLowerCase().startsWith("mac os x")) {
      macosx = true;
    } else if (osName != null && osName.indexOf("Windows") != -1) {
      windoze = true;
      startPath = new String("//");
    } else if (osName != null && osName.indexOf("Mac") != -1) {
      macos = true;
    } else if (osName != null && osName.indexOf("Linux") != -1) {
    }
  }

  private class StreamGobbler extends Thread {
    private final InputStream is;
    private final ByteArrayOutputStream bos;
    private String data;
    Process process = null;

    StreamGobbler(InputStream is, Process process) {
      super();
      this.is = is;
      this.bos = new ByteArrayOutputStream();
      this.process = process;
    }

    public void run() {
      try {
        final PrintWriter pw = new PrintWriter(bos);

        final InputStreamReader isr = new InputStreamReader(is);
        final BufferedReader br = new BufferedReader(isr);
        String line = null;
        while ((line = br.readLine()) != null && process != null) {
          pw.println(line);
        }
        pw.flush();
        pw.close();
        data = new String(bos.toByteArray());
      } catch (IOException ioe) {
        ioe.printStackTrace();
      }
    }

    public String getData() {
      return data;
    }
  }

}
