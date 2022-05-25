package it.unitn.ing.esqui.server;

import java.net.*;
import java.io.*;
import java.util.*;

/** Connection.java
 * <br>
 * Title:			<b>ESQUI Connection</b>
 * </br>
 * Description:	This abstract class defines common methods to handle a
 communication with a client.
 * @author:			Leonardo Cont, February 2001
 * @revision:		February 2001
 * @comment:		none
 */

public abstract class Connection extends Thread {

//All comments but analysisCommands sent to client start with this string
  final String comment = new String("msg&");


  protected Server server;
  protected Vulture vulture;
  protected Socket client;
  protected BufferedReader inReader;
  protected PrintStream out;

  protected String user = null;
  protected int connectionNumber;

  protected String info = null;

  String[] clientInfo = null;

  public Connection(String title, Server server,
                    int priority, int connectionNumber) {
//	Give the thread a group, a name, a priority and a number
    super(server.threadgroup, title);
    this.server = server;
    this.vulture = server.vulture;
    this.setPriority(priority);
    this.connectionNumber = connectionNumber;
  }

//Initialize the streams and start the thread
  public Connection(String title, Server server, Socket socket,
                    int priority, int connectionNumber) {
    this(title, server, priority, connectionNumber);
//	Save our other arguments away
    client = socket;
  }

//Create the streams
  void createStreams() {
    try {
      inReader = new BufferedReader(new InputStreamReader(client.getInputStream()));
      out = new PrintStream(client.getOutputStream());
    } catch (IOException ioexc) {
      try {
        client.close();
      } catch (IOException subioexc) {
        ;
      }
      System.err.println("Exception while getting socket streams: " + ioexc);
    }
  }

//This method returns the string representation of the Connection.
  public String connectionInfo() {
    return new String(getConnectionName() + ": "
            + client.getInetAddress().getHostName()
            + "(" + client.getInetAddress().getHostAddress() +
            ")");
  }

  public void sendBack(String tmpString) {
    out.println(tmpString);
  }

  public synchronized String getConnectionName() {
    return getName();
  }

//Check if user can be authenticated
  void checkUsernamePassword(String line) throws Exception {
    clientInfo = new String[2];
//		Exclude the 'checkuser' substring
    String tmpLine = ServerMisc.getAttachedValue(line);
    info = connectionInfo();
    ServerMisc.logInformation(ServerMisc.date() + "-" + ServerMisc.time() + ": " + info);
    if (tmpLine.indexOf("username") == -1 || tmpLine.indexOf("password") == -1)
      throw new Exception();
    StringTokenizer tmpString = new StringTokenizer(tmpLine, " _");
    int i = 0;
    while (tmpString.hasMoreTokens() && i < 2) {
//			store the unseful string (username or passowrd)
      clientInfo[i] = tmpString.nextToken();
//			store the real username or password
      clientInfo[i] = tmpString.nextToken();
      i++;
    }
  }
}
