package it.unitn.ing.esqui.server;

import it.unitn.ing.rista.util.Misc;

import java.net.*;
import java.io.*;
import java.awt.*;
import java.util.*;

/** FtpServer.java
 * <br>
 * Title:			<b>ESQUI Ftp Server</b>
 * </br>
 * Description:	Connection for managing the transfer of any zipped
 file between MAUD and the ESQUI goniometer software
 * @author:			Leonardo Cont, February 2000
 * @revision:		February 2001
 * @comment:		none
 */

public class FtpServer extends Thread {

  private Socket client;
  private DataInputStream in;
  private PrintStream out;
  private File fileToTransfer;

  public FtpServer(File fileToTransfer, int priority,
                   int connectionNumber) {
    super("Ftp Connection for Connection-" + connectionNumber);
    this.fileToTransfer = fileToTransfer;
    this.setPriority(priority);
    this.start();
  }

  public void run() {
    String line;
    boolean isConnected = false;
    while (!isConnected) {
      try {
        client = Server.getFTPServerSocket().accept();
        in = new DataInputStream(client.getInputStream());
        out = new PrintStream(client.getOutputStream());
        isConnected = true;
      } catch (IOException ioexcONE) {
        try {
          client.close();
        } catch (IOException ioexcTWO) {
          ;
        }
        System.err.println("Exception while getting socket streams: " + ioexcONE);
      }
    }
    try {
      while (true) {
        line = in.readLine();
        if (line == null)
          break;
        if (line.equals("readytogetdata")) ;
        sendData(fileToTransfer);
      }
    } catch (IOException ioexc) {
      ;
    }
    try {
      in.close();
    } catch (Exception ioexc) {
      ;
    }
    try {
      out.close();
    } catch (Exception ioexc) {
      ;
    }
    try {
      client.close();
    } catch (IOException ioexc) {
      ;
    }
//        fileToTransfer.delete();
  }

  void sendData(File fileToTransfer) {
    BufferedInputStream zipIn = null;
    int BLOCK_SIZE = 512;
    byte[] bytes = new byte[BLOCK_SIZE];
    int bytesRead = 0;
    int progress = 0;
//        System.out.println("Start transfert");
    try {
      zipIn = new BufferedInputStream(new FileInputStream(fileToTransfer));
      int fileDim = zipIn.available();
      while (zipIn.available() > 0) {
        bytesRead = zipIn.read(bytes);
        out.write(bytes, 0, bytesRead);
//        progress += bytesRead;
//        System.out.println("Sent " + progress);
      }
      out.flush();
    } catch (IOException ioexc) {
      ioexc.printStackTrace();
      String tmpString = new String("Error reading the analysis data file");
//   			String tmpString = new String("Error reading the zipped analysis data file");
      out.println(tmpString);
      System.out.println(tmpString);
    }
    try {
      if (zipIn != null)
        zipIn.close();
    } catch (IOException ioexc) {
      ;
    }
    zipIn = null;
  }
}
