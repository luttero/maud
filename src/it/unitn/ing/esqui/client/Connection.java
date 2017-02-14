package it.unitn.ing.esqui.client;

import it.unitn.ing.rista.util.Misc;

import java.io.*;
import java.util.*;
import java.net.*;
import javax.swing.*;

/** Connection.java
 * <br>
 * Title:			<b>ESQUI Server Connection</b>
 * <br>
 * Description:	Abstract class for creating connections to
 * 							the ESQUI server
 * <br>
 * @author:			Leonardo Cont, December 2000
 * @revision:		Februry 2001
 * @comment:		none
 */
public abstract class Connection extends Thread {

  static Socket socket = null;
  static PrintStream out = null;
  String host = null;
  int port;
  BufferedReader in = null;

  ClientFrame clientframe;

  String line = null;
  static boolean connectionOn = false;
  static boolean socketOn = false;
  static boolean isStoppedByUser = false;
  static boolean isUserBack = false;

  public Connection(ClientFrame clientframe) {
    super("Server Connections");
    this.clientframe = clientframe;
    this.setPriority(4);
  }
/*
	Method to send a message string to the server through the standard stream OUT
*/
  public synchronized static void sendString(String messageString)
          throws Exception {
    out.println(messageString);
  }

/*
	Methods to set on/off the socket and connection
*/
  public synchronized static void setSocketActive(boolean onoff) {
    socketOn = onoff;
  }

  public synchronized static void setConnectionActive(boolean onoff) {
    connectionOn = onoff;
  }

  public synchronized static void setStoppedByUser(boolean trueFalse) {
    isStoppedByUser = trueFalse;
  }

/*
	Methods to get the socket and connection status
*/
  public synchronized static boolean getSocketActive() {
    return socketOn;
  }

  public synchronized static boolean getConnectionActive() {
    return connectionOn;
  }

  public synchronized static boolean getStoppedByUser() {
    return isStoppedByUser;
  }

/*
	Method to create a socket and a stream based upon user's settings
*/
  public synchronized Socket createSocketStream() throws IOException {
    return createSocketStream(Integer.parseInt(Client.clientsettings[1]));
  }

/*
	Method to create a socket and a stream based upon server's settings
*/
  public synchronized Socket createSocketStream(int port) throws IOException {
    Socket tmpSocket = new Socket(Client.clientsettings[0], port);
    return tmpSocket;
  }

/*
	Method to close a socket and a stream
*/
  public synchronized void closeSocket() {
    try {
      if (socket != null)
        socket.close();
      if (out != null)
        out.close();
    } catch (IOException ioexc) {
      System.out.println("Closing socket!");
    }
    out = null;
    socket = null;
  }

  public synchronized static void setIsUserBack(boolean isBack) {
    isUserBack = isBack;
  }

  public synchronized static boolean getIsUserBack() {
    return isUserBack;
  }

  public synchronized void displayMessage(String msg) {
    clientframe.fromServerArea.append(msg + "\n");
  }

  public synchronized void displayStatus() {
    clientframe.displayConnectionStatus();
  }
}
