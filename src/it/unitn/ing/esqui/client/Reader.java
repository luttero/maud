package it.unitn.ing.esqui.client;

import java.io.*;
import java.util.*;
import java.net.*;
import java.awt.*;
import javax.swing.*;

/** Client.java
 * <br>
 * Title:			<b>ESQUI Reader</b>
 * <br>
 * Description:	Reader for receiving data from the ESQUI client
 * author:			Leonardo Cont, December 2000
 * revision:		December 2000
 * comment:			none
 */

class EsquiReader extends Thread {

  ServerConnection connection = null;
  ClientFrame clientframe = null;
  RetrieveDataFrame retrieveDataFrame = null;

  BufferedReader inReader = null;
  static File localFileName = null;
  File tmpFile = null;
  boolean isData = false;
  boolean isAvailableFiles = false;
  boolean closedbyserver = false;

  public EsquiReader(ServerConnection connection, ClientFrame clientframe) {
    super("Client Reader");
    this.connection = connection;
    this.clientframe = clientframe;
  }

  public void run() {
    while (connection.getSocketActive()) {
      InfoWindow tmpWindow = null;
      Vector values = null;
      String line = null;
      String tmpValues = null;
      int fileSize = 0;
      int uncompressedFileSize = 0;
      try {
        inReader = new BufferedReader(new InputStreamReader(connection.socket.getInputStream()));
        while (true) {
          line = inReader.readLine();
          if (line == null)
            break;
          if (!line.equals(null) && line.startsWith("msg")) {
            tmpValues = ClientMisc.getAttachedValues(line);
            clientframe.fromServerArea.append(tmpValues + "\n");
          }
          if (line.startsWith(ServerConnection.analysisCommands[4]))
            isAvailableFiles = true;
          if (line.startsWith(ServerConnection.analysisCommands[5]))
            tmpWindow = new WaitingWindow(clientframe, ClientMisc.getAttachedValues(line));
          if (line.startsWith(ServerConnection.analysisCommands[6]))
            retrieveDataFrame.dataInfo = ClientMisc.getAttachedValues(line);
          if (line.startsWith(ServerConnection.analysisCommands[2]))
            isData = true;
          if (isData) {
            tmpWindow.setVisible(false);
            tmpWindow.dispose();
            tmpWindow = null;
            tmpValues = ClientMisc.getAttachedValues(line);
            values = ClientMisc.getParsedValues(tmpValues);
            fileSize = Integer.parseInt((String) values.elementAt(0));
            uncompressedFileSize = Integer.parseInt((String) values.elementAt(1));
            clientframe.fromServerArea.append("Retrieving file " + localFileName + "\n");
            getData(fileSize, uncompressedFileSize);
            isData = false;
          }
          if (isAvailableFiles) {
            tmpValues = ClientMisc.getAttachedValues(line);
            if (tmpValues != null)
              getAvailableFiles(tmpValues);
            isAvailableFiles = false;
          }
        }
      } catch (IOException ioexc) {
        ioexc.printStackTrace();
        tmpWindow.setVisible(false);
        tmpWindow.dispose();
        tmpWindow = null;
        connection.setSocketActive(false);
      }
      synchronized (clientframe) {
        clientframe.displayConnectionStatus();
      }
      try {
        if (!Connection.getStoppedByUser()) {
          ClientMisc.messageBox(clientframe, "Alert", "Connection closed! Verify server status.", JOptionPane.WARNING_MESSAGE);
          Connection.setIsUserBack(true);
        }
        if (inReader != null)
          inReader.close();
      } catch (IOException ioexc) {
        ;
      }
      inReader = null;
    }
  }

  void getAvailableFiles(String files) {
    retrieveDataFrame = clientframe.retrieveDataFrame;
    Vector availableFiles = new Vector(0, 1);
    StringTokenizer tmpTokenizer = new StringTokenizer(files, "&");
    while (tmpTokenizer.hasMoreTokens()) {
      availableFiles.addElement(tmpTokenizer.nextToken());
    }
    retrieveDataFrame.setList(availableFiles);
  }

  public static void setLocalFile(String localFile) {
    localFileName = new File(localFile);
  }

  void getData(int fileSize, int uncompressedFileSize) {
    FtpConnection ftpReader = new FtpConnection(Client.getSetting(0), uncompressedFileSize,
            uncompressedFileSize, localFileName, clientframe);
  }

}
