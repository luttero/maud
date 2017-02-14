package it.unitn.ing.esqui.client;

import java.io.*;
import java.util.*;
import java.net.*;
import javax.swing.*;

/** ServerConnection.java
 * <br>
 * Title:			<b>ESQUI Connection</b>
 * <br>
 * Description:	Connection for managing data to/from the ESQUI server
 * <br>
 * @author:			Leonardo Cont, December 2000
 * @revision:		Februry 2001
 * @comment:		none
 */

class ServerConnection extends Connection {

//	Define the commands for informing the client about
//	 the data transfer
  final static String[] analysisCommands = {"beginanalysisfile&",
                                            "endanalysisfile&",
                                            "beginanalysisdata&",
                                            "getdata&",
                                            "availablefiles&",
                                            "createzipdata&",
                                            "getdatainfo&"
  };

  int attemptNumber = 0;

  public ServerConnection(ClientFrame clientframe) {
    super(clientframe);
    setConnectionActive(true);
    setSocketActive(false);
    this.start();
  }

  public void run() {
    while (getConnectionActive()) {
      if (!getSocketActive()) {
        attemptNumber++;
        try {
//						Ask for re-connection if user is back
          if (getIsUserBack())
            backConnection();
          socket = createSocketStream(Integer.parseInt(Client.clientsettings[1]) + 1);
          out = new PrintStream(socket.getOutputStream());
          setSocketActive(true);
          displayStatus();
          try {
//							Create the reader socket, give it a priority and start it
            EsquiReader reader = new EsquiReader(this, clientframe);
            reader.setPriority(4);
            reader.start();
          } catch (Exception exc) {
            ClientMisc.messageBox(clientframe, "Alert", "Unable to create the reader connection!", JOptionPane.WARNING_MESSAGE);
            setSocketActive(false);
            closeSocket();
          }
        } catch (IOException ioexc) {

          ioexc.printStackTrace();
          if (attemptNumber == 1)
            ClientMisc.messageBox(clientframe, "Alert", "Unable to connect! Contact your server master.", JOptionPane.WARNING_MESSAGE);
          setSocketActive(false);
        }
      }
    }
    setSocketActive(false);
    setIsUserBack(false);
    closeSocket();
  }

  void backConnection() throws IOException {
    Socket tmpSocket = createSocketStream();
    PrintStream tmpOut = new PrintStream(tmpSocket.getOutputStream());
    BufferedReader tmpIn = new BufferedReader(new InputStreamReader(tmpSocket.getInputStream()));
    tmpOut.println("checkuser&username_" + Client.getUser() +
            " password_" + Client.getPassword());
//	Wait for response from server
    while (true) {
      String in = tmpIn.readLine();
      if (in == null)
        break;
      if (in == "userauthenticated")
        break;
      if (in == "usernotauthenticated")
        throw new IOException();
    }
  }
}
