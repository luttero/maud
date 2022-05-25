package it.unitn.ing.esqui.client;

import it.unitn.ing.rista.util.Misc;

import java.io.*;
import java.util.*;
import java.text.*;
import java.util.zip.*;
import java.net.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;

/** Client.java
 * <br>
 * Title:			<b>ESQUI Client</b>
 * <br>
 * Description:	Client for sending and receiving data to and from
 the ESQUI server
 * author:			Leonardo Cont, December 2000
 * revision:		February 2000
 * comment:			A first client for the ESQUI project
 */

public class Client {

  protected static boolean isForMaud = true;

  protected final static String DEFAULT_PREFS_FILE = "clientPreferences.pref";
  protected final static String[] DEFAULT_SETTINGS = {"127.0.0.1",
                                                      "2000",
                                                      "esquikeycodeforencryption"
  };
  protected final static int FTP_DEFAULT_PORT = 1999;
  protected final static int CONNECTION_DEFAULT_PORT = 2001;

  static String userName = null;
  static String passWord = null;
  ClientFrame clientframe;

//	Variable containing the host (index 0), port (index 1),
//	keycode (index 3)
  static String[] clientsettings = new String[3];
  final static String[] clientsettingsdefinition = {"_server_HOST ",
                                                    "_server_PORT ",
                                                    "_server_KEYCODE "
  };

//	Create the client constructor
  public Client(String title) {

    // Read the settings
    readPreferences(DEFAULT_PREFS_FILE);

    // Create the client interface
    clientframe = new ClientFrame(title, 550, 450);
    clientframe.toServerString.requestFocus();
  }

/*
	Following methods are not used in the for-MAUD version
*/
  public static void usage() {
    System.out.println("Usage: javaPath Client");
    //Luca System.exit(0);
  }

  public static final void main(String[] args) {
    if (args.length != 0)
      usage();
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
    } catch (Exception exc) {
      ;
    }

    isForMaud = false;

    new Client("ESQUI Client");
  }

/*
	Methods to get a setting
*/
  public synchronized static String getSetting(int index) {
    return clientsettings[index];
  }

/*
	Method to set a setting
*/
  public synchronized static void setSetting(String newinfo, int index) {
    clientsettings[index] = newinfo;
  }

  void setDefaultSettings() {
    for (int i = 0; i < DEFAULT_SETTINGS.length; i++)
      setSetting(DEFAULT_SETTINGS[i], i);
  }


/*
	Method to read the client settings
*/
  void readPreferences(String filename) {
    String tmpLine = null;
    int i = 0;
    try {
      BufferedReader settingsreader = new BufferedReader(new FileReader(filename));
      while ((tmpLine = settingsreader.readLine()) != null) {
        StringTokenizer tmptokenstring = new StringTokenizer(tmpLine);
        while (tmptokenstring.hasMoreTokens()) {
          String tmpToken = tmptokenstring.nextToken();
          if (!tmpToken.startsWith("_"))
            setSetting(tmpToken, i++);
        }
      }
    } catch (Exception exc) {
      System.out.println("Setting the default values for host and port");
      setDefaultSettings();
    }
  }


  public synchronized static String getUser() {
    return userName;
  }

  public synchronized static String getPassword() {
    return passWord;
  }

  public synchronized static void setUser(String tmpUser) {
    userName = tmpUser;
  }

  public synchronized static void setPassword(String tmpPassword) {
    passWord = tmpPassword;
  }
}
