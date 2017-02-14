package it.unitn.ing.esqui.server;

import it.unitn.ing.rista.util.Misc;

import java.net.*;
import java.io.*;

/** AuthenticationConnection.java
 * <br>
 * Title:			<b>ESQUI Authentication Connection</b>
 * </br>
 * Description:	This class is the thread which authenticates a client.
 * @author:			Leonardo Cont, February 2001
 * @revision:		February 2001
 * @comment:		none
 */

public class AuthenticationConnection extends Connection {

  private final String threadLabel = new String("Authentication connection-");

  protected boolean isAuthenticated = false;
  ClientConnection newConnection = null;

//Initialize the streams and start the thread
  public AuthenticationConnection(Server server, Socket authentication_socket,
                                  int priority, int connectionNumber) {
//	Give the thread a group, a name, and a priority
    super("Authentication connection-" + connectionNumber, server, authentication_socket, priority, connectionNumber);
//	Create the streams
    createStreams();
//	And start the thread up
    this.start();
  }

//	Provide the service.
  public void run() {
    String line = null;
    try {
      while (true) {
//				Read in a line
        line = inReader.readLine();
        if (line == null)
          break;
//				Check if new user is authenticated
        if (line.startsWith("checkuser")) {
          if (checkUser(line))
            isAuthenticated = true;
          break;
        }
        if (line.startsWith("getEncryptionKeycode")) {
          if (Server.allowKeyCode)
            sendBack("EncryptionKeycode=" + Server.keyCode);
          else
            sendBack("no_EncryptionKeycode");
        }
      }
    } catch (IOException ioexc) {
      ;
    }
    try {
      client.close();
    } catch (IOException ioexc) {
      ;
    }

//		Start the final client connection if user is authenticated
    if (isAuthenticated)
      newConnection = new ClientConnection(server, 4, connectionNumber);
  }

  boolean checkUser(String line) {
    try {
      checkUsernamePassword(line);
      ServerMisc.logInformation("User " + clientInfo[0] + " asks for connection" + ServerMisc.getSystemProperty("line.separator"));
      if (server.isUserAuthenticated(clientInfo)) {
//					Add the new user to the users vector
        synchronized (server.getConnectedUsersList()) {
          server.getConnectedUsersList().addElement(clientInfo[0]);
        }
//					Display information about user connected
        System.out.println(info);
//					Send the right to connect
        sendBack("userauthenticated");
//					Log information about user connected
        ServerMisc.logInformation("User " + clientInfo[0] + " was authenticated." + ServerMisc.getSystemProperty("line.separator"));
        return true;
      } else {
//					Send the deny to connect
        ServerMisc.logInformation("User " + clientInfo[0] + " was not authenticated." + "Password used was: " + clientInfo[1] + ServerMisc.getSystemProperty("line.separator"));
        sendBack("usernotauthenticated");
        throw new Exception();
      }
    } catch (Exception exc) {
      ServerMisc.logInformation("Troubles connecting the user " + clientInfo[0]);
    }
    return false;
  }
}
