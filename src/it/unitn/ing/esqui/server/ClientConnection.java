package it.unitn.ing.esqui.server;

import it.unitn.ing.rista.util.Misc;
import it.unitn.ing.rista.util.Constants;

import java.io.*;
import java.util.*;
import java.util.zip.*;
import java.text.*;

/** ClientConnection.java
 * <br>
 * Title:			<b>ESQUI Client Connection</b>
 * </br>
 * Description:	This class is the thread that handles all
 communication with a client. It also notifies
 the Vulture when the connection is dropped.
 * @author:			Leonardo Cont, December 2000
 * @revision:		December 2000
 * @comment:		none
 */


//This class is the thread that handles all communication with a client
//It also notifies the Vulture when the connection is dropped.

public class ClientConnection extends Connection {

  private final String threadLabel = new String("Client connection-");

//Define the maudgo software commands for informing the server
//that an analysis file is coming up
  protected final String[] analysisCommands = {"beginanalysisfile&",
                                               "endanalysisfile&",
                                               "beginanalysisdata&",
                                               "getdata&",
                                               "availablefiles&",
                                               "createzipdata&",
                                               "getdatainfo&"
  };
//All comments but analysisCommands sent to client start with this string
  protected final String comment = new String("msg&");
//Define the incoming commands recognized by server
  protected final String[] clientCommands = {"Kill", // Stop abruptly the device
                                             "Restore", // Revert analysis to the starting point
                                             "Stop", // Stop softly the device
                                             "Status"	// Show the current status
  };


  protected String[] USER_DEFAULT_DIRS = new String[3];

  protected long dataCompressedSize;
  protected long dataUncompressedSize;
  protected ProgressFrame progressFrame = null;

//Contructor for new connection
  public ClientConnection(Server server, int priority,
                          int connectionNumber) {
//	Give the thread a group, a name, and a priority
    super("Client connection-" + connectionNumber, server,
            priority, connectionNumber);
//	Create the client socket (waiting for the client to respond)
    while (true) {
      try {
        client = Server.getClientServerSocket().accept();
        break;
      } catch (IOException ioexc) {
        System.out.println("Exception while creating the socket: " + ioexc);
      }
    }
//	Create the streams
    createStreams();
//	Set user
    user = (String) server.getUser(getConnectionNumber());
//	Set up user's directories
    setUserSettings(user);
//	Set the info string
    info = connectionInfo();
//	Add the connection to the connections vector and to the connection listbox
    synchronized (server.connections) {
      server.connections.addElement(this);
      server.connectionListModel.addElement(info);
    }
//	And start the thread up
    this.start();
  }

//	Provide the service.
  public void run() {
    FtpServer ftpServer = null;
    File selectedFile = null;
    String fileName = null;
    String fileNameNoExt = null;
    String line = null;
    Vector dataVector = null;
    StringBuffer fileContent = null;
    boolean writeAnalysis = false;

    try {
      System.out.println(info);
//			Send a welcome message to the client authenticated
      sendBack(comment + "Welcome to ESQUI Server, ver 1.0");
      sendBack(comment + "Connected from " + DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.MEDIUM).format(new Date()));
      ServerMisc.logInformation(ServerMisc.date() + "-" + ServerMisc.time() + ": " + info);
//			Start listening
      while (true) {
//				Read in a line
        line = inReader.readLine();
        if (line == null)
          break;
//				Write the line on the server console if it is not empty
        if (!line.equals("")) {
          System.out.println("From " + getConnectionName() + ": " + line);
          ServerMisc.logInformation("From " + getConnectionName() + ": " + line);
        }
//				Check recognized commands and set the right action to be performed
        for (int i = 0; i < clientCommands.length; i++) {
          if (line.equals(clientCommands[i]))
            writeEsquigoCommand(clientCommands[i]);
        }
//				Check if end of analysis file has been reached
        if (line.startsWith(analysisCommands[1])) {
          writeAnalysisFile(fileContent.toString());
          writeAnalysis = false;
        }
//				Write incoming analysis file strings into the buffer
        if (writeAnalysis)
          fileContent.append(line + ServerMisc.getSystemProperty("line.separator"));
//				Check if an analysis file is incoming and start the file buffer
        if (line.startsWith(analysisCommands[0])) {
          writeAnalysis = true;
          fileContent = new StringBuffer();
        }
        if (line.startsWith(analysisCommands[6])) {
          fileName = ServerMisc.getAttachedValue(line);
//						fileNameNoExt = ServerMisc.getFileNoExtension(fileName);
          selectedFile = new File(USER_DEFAULT_DIRS[1] + fileName);
          dataUncompressedSize = selectedFile.length();
          sendBack(analysisCommands[6] + dataUncompressedSize);
        }
        if (line.startsWith(analysisCommands[3])) {
//						sendBack(analysisCommands[5] + "Server is creating zip file...please wait...");
          sendBack(analysisCommands[5] + "Server is sending the file...please wait...");
          try {
//							createZipFile(selectedFile, fileNameNoExt);
/*							sendBack(analysisCommands[2] +
												dataCompressedSize +
												"&" +
												dataUncompressedSize + "\n");*/
            sendBack(analysisCommands[2] +
                    dataUncompressedSize +
                    "&" +
                    dataUncompressedSize + "\n");
//							ftpServer = new FtpServer(new File(USER_DEFAULT_DIRS[1] + fileNameNoExt + ".zip"), 4, getConnectionNumber());
            ftpServer = new FtpServer(new File(USER_DEFAULT_DIRS[1] + fileName), 4, getConnectionNumber());
          } catch (Exception exc) {
            ;
          }
        }
        if (line.startsWith(analysisCommands[4])) {
          sendFilesList();
        }
      }
    } catch (IOException ioexc) {
      ;
    }

    synchronized (server.getConnectedUsersList()) {
      server.getConnectedUsersList().removeElementAt(connectionNumber);
    }

    try {
      client.close();
    } catch (IOException ioexc) {
      ;
    }
    synchronized (vulture) {
      vulture.notify();
    }
  }

  public void writeAnalysisFile(String messageLine) {
    try {
      String fileID = ServerMisc.date() + ServerMisc.time();
      BufferedWriter outputBuffer = new BufferedWriter(
              new FileWriter(USER_DEFAULT_DIRS[0] +
              "maud2go_" +
              fileID +
              ".par", false));
      outputBuffer.write(messageLine);
      outputBuffer.close();
    } catch (IOException ioExcep) {
      Server.fail(ioExcep, "Analysis file writing error!", false);
    }

  }

  void writeEsquigoCommand(String command) {
    BufferedWriter outputBuffer = null;
//		ServerMisc.checkDirectory(USER_DEFAULT_DIRS[2]);
    try {
      outputBuffer = new BufferedWriter(new FileWriter(USER_DEFAULT_DIRS[2] + command, true));
      sendBack(comment + "Command received: " + command);
    } catch (IOException ioexc) {
      Server.fail(ioexc, "Esquigo command writing error!", false);
      sendBack(comment + "Error while receiving command: " + command);
    }
    try {
      outputBuffer.close();
    } catch (IOException ioexc) {
      ;
    }
    outputBuffer = null;
  }

  public void sendBack(String tmpString) {
    out.println(tmpString);
  }

  void setUserSettings(String userName) {
    for (int i = 0; i < server.DEFAULT_DIRS.length; i++) {
      USER_DEFAULT_DIRS[i] = new String(server.DEFAULT_DIRS[i]);// + userName + "/");
    }
//	Create the users directories
    ServerMisc.checkDirectories(USER_DEFAULT_DIRS);
  }

  void sendFilesList() {
    StringBuffer tmpBuffer = new StringBuffer(analysisCommands[4]);
    File userOutDir = new File(USER_DEFAULT_DIRS[1]);
    String[] filesNames = userOutDir.list();
    for (int i = 0; i < filesNames.length; i++) {
      tmpBuffer.append(filesNames[i] + "&");
    }
    out.println(tmpBuffer.toString());
  }

  public void createZipFile(File filename, String filenameNoExt) {
    ZipOutputStream zipOut = null;
    BufferedOutputStream buffer = null;
    File outFile = null;
    InputStream in = null;
    ZipEntry entry = null;
    int BLOCK_SIZE = 512;
    byte[] bytes = new byte[BLOCK_SIZE];
    int len = 0;
    int mark = 0;
    try {
      outFile = new File(USER_DEFAULT_DIRS[1] + filenameNoExt + ".zip");
      buffer = new BufferedOutputStream(new FileOutputStream(outFile), BLOCK_SIZE);
      zipOut = new ZipOutputStream(buffer);
      in = new FileInputStream(filename);
      if (Constants.showProgressFrame) {
      progressFrame = new ProgressFrame("Creating zip file...", in.available());
      progressFrame.requestFocus();
      }
      entry = new ZipEntry(filename.getName().replace(File.separatorChar, '/'));
      zipOut.putNextEntry(entry);
      while ((len = in.read(bytes)) != -1) {
        mark += len;
        zipOut.write(bytes, 0, len);
        if (Constants.showProgressFrame)
          progressFrame.setBarValue(mark);
      }
      zipOut.closeEntry();
      zipOut.finish();
    } catch (Exception ioexc) {
      ioexc.printStackTrace();
      String tmpString = new String("Error creating the zipped analysis data file");
      out.println(tmpString);
      System.out.println(tmpString);
    }
    try {
      if (in != null)
        in.close();
      if (zipOut != null)
        zipOut.close();
      if (progressFrame != null)
        progressFrame.dispose();
    } catch (IOException ioexc) {
      ;
    }
    entry = null;
    in = null;
    zipOut = null;
    progressFrame = null;
    dataCompressedSize = outFile.length();
  }

  public synchronized void setConnectionNumber(int newConnectionNumber) {
    connectionNumber = newConnectionNumber;
  }

  public synchronized int getConnectionNumber() {
    return connectionNumber;
  }
}
