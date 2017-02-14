package it.unitn.ing.esqui.client;

import it.unitn.ing.rista.awt.myJFrame;
import it.unitn.ing.rista.util.Misc;

import java.io.*;
import java.net.*;
import javax.swing.*;

/** FtpReader.java
 * <br>
 * Title:			<b>ESQUI Ftp Reader</b>
 * <br>
 * Description:	Reader for receiving any data from the ESQUI server
 * author:			Leonardo Cont, December 2000
 * revision:		February 2000
 * comment:			none
 */

class FtpConnection extends Thread {

  protected ClientFrame parentFrame = null;
  protected File tmpFile = new File("temp.zip");
  protected File localFileName = null;
  protected int finalFileSize;
  private int fileSize;
  private ProgressWindow tmpWindow = null;
  private String host = null;

  public FtpConnection(String host, int fileSize, int finalFileSize,
                       File localFileName, JFrame parentFrame) {
    super("Ftp Reader");
    this.host = host;
    this.fileSize = fileSize;
    this.parentFrame = (ClientFrame) parentFrame;
    this.finalFileSize = finalFileSize;
    this.localFileName = localFileName;

    this.start();
  }

  public void run() {
    Socket ftpSocket = null;
    DataInputStream ftpIn = null;
    ByteArrayOutputStream localByteArray = null;
    PrintStream ftpOut = null;
    FileOutputStream localStream = null;
    boolean isConnected = false;
    int BLOCK_SIZE = 512;
    byte[] bytes = new byte[BLOCK_SIZE];
    int bytesRead = 0;
    int progress = 0;
    while (!isConnected) {
      try {
        ftpSocket = new Socket(host, Integer.parseInt(Client.clientsettings[1]) - 1);
        ftpOut = new PrintStream(ftpSocket.getOutputStream());
        ftpIn = new DataInputStream(ftpSocket.getInputStream());
//							localStream = new FileOutputStream(tmpFile);
        localStream = new FileOutputStream(localFileName);
        localByteArray = new ByteArrayOutputStream(BLOCK_SIZE);
        isConnected = true;
      } catch (IOException ioexc) {
        ;
      }
    }
    try {
      ftpOut.println("readytogetdata");
      ftpOut.flush();
      tmpWindow = new ProgressWindow(parentFrame, "Retrieving analysis data...please wait...", fileSize);
      while (true) {
        bytesRead = ftpIn.read(bytes);
//              System.out.println(bytesRead);
        if (bytesRead >= 0) {
          localByteArray.write(bytes, 0, bytesRead);
          tmpWindow.setBarValue(progress += bytesRead);
        } else
          break;
        if (progress == fileSize)
          break;
      }
      localByteArray.writeTo(localStream);
      localByteArray.flush();
      localByteArray.close();
      localStream.flush();
      tmpWindow.setVisible(false);
      tmpWindow.dispose();
//					ZipTool zipTool = new ZipTool(parentFrame);
//					zipTool.unzipFile(tmpFile, localFileName, finalFileSize);
      parentFrame.fromServerArea.append("File " + localFileName + " retrieved successfully!\n");

    } catch (IOException ioexc) {
      System.out.println("Error reading the zip file");
      ioexc.printStackTrace();
    }
    try {
      ftpSocket.close();
      ftpOut.close();
      ftpIn.close();
      if (localStream != null)
        localStream.close();
      if (localByteArray != null)
        localByteArray.close();
    } catch (IOException ioexc) {
      ;
    }
    parentFrame = null;
    localByteArray = null;
    localStream = null;
//				tmpFile.delete();
  }
}
