package it.unitn.ing.esqui.client;

import com.radiographema.Maud;
import it.unitn.ing.rista.util.Misc;
import it.unitn.ing.rista.awt.myJFrame;

import java.io.*;
import java.util.*;
import java.net.*;
import javax.swing.*;
import java.text.*;

/** AuthenticationConnection.java
 * <br>
 * Title:			<b>ESQUI Authentication Connection</b>
 * <br>
 * Description:	Connection for authenticating an user on the ESQUI server
 * <br>
 * @author:			Leonardo Cont, December 2000
 * @revision:		Februry 2001
 * @comment:		none
 */

class AuthenticationConnection extends Connection {

  AuthenticationFrame authenticationFrame = null;
  boolean isAuthenticated = false;

  public AuthenticationConnection(ClientFrame clientframe) {
    super(clientframe);
    this.authenticationFrame = clientframe.authenticationFrame;
    this.start();
  }

  public void run() {
    try {
      socket = createSocketStream();
      out = new PrintStream(socket.getOutputStream());
      in = new BufferedReader(new InputStreamReader(socket.getInputStream()));

      sendString("getEncryptionKeycode_");
      line = null;
      int counter = 0;
      while (line == null && counter < 5) {
        line = in.readLine();
        if (line.startsWith("EncryptionKeycode")) {
          int mark = line.indexOf("=");
          if (mark != -1) {
            String keycode = line.substring(mark + 1, line.length());
            Client.setSetting(keycode.trim(), 2);
          }
        } else if (line.startsWith("no_EncryptionKeycode")) {
          break;
        } else {
          System.out.println("Received wrongly:" + line);
          counter++; // try 5 times
          line = null;
        }
      }

      String password = Client.getPassword();
      try {
        Encryption encryption = new Encryption();
        password = encryption.encrypt(password);
      } catch (Exception exc) {
        System.out.println("Error on password encryption!");
        exc.printStackTrace();
      }

      sendString("checkuser&username_" + Client.getUser() +
              " password_" + password);
      while (true) {
        line = in.readLine();
        if (line.equals("usernotauthenticated")) {
          ClientMisc.messageBox(clientframe, "Alert", "Authentication failed! Check your username/password.", JOptionPane.WARNING_MESSAGE);
          authenticationFailed();
          isAuthenticated = false;
          break;
        } else if (line.equals("userauthenticated")) {
          isAuthenticated = true;
          break;
        }
      }
    } catch (IOException ioexc) {
      ClientMisc.messageBox(authenticationFrame, "Alert", "Unable to connect! Check the server status.", JOptionPane.WARNING_MESSAGE);
    } catch (Exception exc) {
      ClientMisc.messageBox(authenticationFrame, "Alert", "Unable to send messages! Check the server status.", JOptionPane.WARNING_MESSAGE);
    }
    myJFrame.prepareForDisposal(authenticationFrame);
    authenticationFrame.setVisible(false);
    authenticationFrame.dispose();
    try {
      if (in != null)
        in.close();
    } catch (IOException ioexc) {
      ;
    }
    in = null;
    closeSocket();
    if (isAuthenticated) {
      ServerConnection newConnection = new ServerConnection(clientframe);
      clientframe.startstopButton.setIcon(ClientMisc.getImage("Stop.gif"));
    }
  }

  void authenticationFailed() {
    try {
      sendString(InetAddress.getLocalHost().toString() +
              " was not logged in on " +
              (DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT, Locale.ITALY).format(new Date())));
    } catch (Exception exc) {
      ;
    }
    setConnectionActive(false);
    displayStatus();
    clientframe.startstopButton.setIcon(ClientMisc.getImage("GreenFlag.gif"));
  }

}
