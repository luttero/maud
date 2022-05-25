package it.unitn.ing.esqui.server;

import it.unitn.ing.rista.util.Misc;

import java.net.*;
import java.io.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/** Server.java
 * <br>
 * Title:			<b>ESQUI Server</b>
 * </br>
 * Description:		Server for managing the connection between MAUD
 and the ESQUI goniometer software
 * @author:			Leonardo Cont, December 2000
 * @revision:		December 2000
 * @comment:		A first server for the ESQUI project
 */

public class Server extends Thread {

  protected final String[] DEFAULT_DIRS = {"In/",
                                           "Out/",
                                           "Commands/"
  };

  int DEFAULT_FTP_PORT = 1999;
  int DEFAULT_AUTHENTICATION_PORT = 2000;
  int DEFAULT_CLIENT_PORT = 2001;
  protected static String DEFAULT_KEY_CODE = "esquikeycodeforencryption";
  protected static ServerSocket ftp_serversocket;
  protected static ServerSocket client_serversocket;

  protected static String DEFAULT_PWD_FILE = ".UserAndPassword.db";
//	protected Vector userpwdVector = new Vector(2,2);

  protected ServerSocket authentication_serversocket;
  protected ThreadGroup threadgroup;
  protected DefaultListModel connectionListModel;
  protected JList connectionList;
  protected Vector connections;
  protected Vector users = new Vector(0, 1);
  protected Vector connectedUsers = new Vector(0, 1);
  protected Vulture vulture;
  protected static String keyCode;
//	protected int port;
  protected int connectionNumber = 0;
  Vector encryptedpasswords = new Vector(0, 1);
  Vector decryptedpasswords = new Vector(0, 1);
  Encryption encryption = null;
  static boolean allowKeyCode = true;

//	Exit with an error message, when an exception occurs.
  public static void fail(Exception e, String msg, boolean quit) {
    System.err.println(msg + ": " + e);
    if (quit) {
      System.out.println("Server closed!!");
      System.exit(0);
    }
  }

//	Create a ServerSocket to listen for connections on;  start the thread.
  public Server(String tmpKeyCode, boolean backKeyCode, int defPort) {
//			Create our server thread with a name.
    super("Server");

    DEFAULT_AUTHENTICATION_PORT = defPort;
    DEFAULT_FTP_PORT = DEFAULT_AUTHENTICATION_PORT - 1;
    DEFAULT_CLIENT_PORT = DEFAULT_AUTHENTICATION_PORT + 1;

    allowKeyCode = backKeyCode;
    if (tmpKeyCode == null) tmpKeyCode = DEFAULT_KEY_CODE;
    keyCode = tmpKeyCode;
    try {
      ftp_serversocket = new ServerSocket(DEFAULT_FTP_PORT);
      authentication_serversocket = new ServerSocket(DEFAULT_AUTHENTICATION_PORT);
      client_serversocket = new ServerSocket(DEFAULT_CLIENT_PORT);
    } catch (IOException e) {
      e.printStackTrace();
      fail(e, "Exception creating server socket", true);
    }
//			Create a threadgroup for our connections
    threadgroup = new ThreadGroup("Server Connections");

//			Create a window to display our connections in
//			and for managing ESQUI users
    JFrame serverFrame = new JFrame("Server status on port " + DEFAULT_CLIENT_PORT);
    connectionListModel = new DefaultListModel();
    connectionList = new JList(connectionListModel);
    connectionList.setBorder(ServerMisc.newBorder("USERS CONNECTED", 1));
    connectionList.setEnabled(false);
    serverFrame.getContentPane().add(connectionList, BorderLayout.CENTER);
    JPanel controlPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    JButton usersButton = ServerMisc.createButton("Users.gif", "add/edit/delete ESQUI users");
    usersButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        JFrame administratorauthenticationFrame = new AdministratorAuthentication(Server.this);
      }
    });
    controlPanel.add(usersButton);
    JButton stopButton = ServerMisc.createButton("Exit.gif", "close the server");
    stopButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        System.exit(0);
      }
    });
    controlPanel.add(stopButton);
    serverFrame.getContentPane().add(controlPanel, BorderLayout.SOUTH);
    serverFrame.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        System.exit(0);
      }
    });
    ServerMisc.locateOnScreen(serverFrame, 5, 5);
    serverFrame.pack();
    serverFrame.setSize(450, 200);
    serverFrame.setVisible(true);


//			Initialize vectors to store our connections and users in
    connections = new Vector();

//			Create a Vulture thread to wait for other threads to die.
//			It starts itself automatically.
    vulture = new Vulture(this);

//			Create the defaults directories
    ServerMisc.checkDirectories(DEFAULT_DIRS);

//			Read the users and passwords file and stops the server if fails

    if (encryption == null)
      encryption = new Encryption();

    readFile();

/*				try {
					String tmpLine;
					BufferedReader userpwdReader = new BufferedReader(new FileReader(DEFAULT_PWD_FILE));
					while((tmpLine = userpwdReader.readLine()) != null) {
						StringTokenizer tmpString = new StringTokenizer(tmpLine);
						while(tmpString.hasMoreTokens())
								userpwdVector.addElement(tmpString.nextToken());
					}
//			Start the server listening for connections
				} catch (IOException ioexc) {
					System.out.println("Error reading the password file! Creating a new one!");
				}*/
    this.start();
  }

  // The body of the server thread.  Loop forever, listening for and
  // accepting connections from clients.  For each connection,
  // create a Connection object to handle communication through the
  // new Socket.  When we create a new connection, add it to the
  // Vector of connections, and display it in the List.  Note that we
  // use synchronized to lock the Vector of connections.  The Vulture
  // class does the same, so the vulture won't be removing dead
  // connections while we're adding fresh ones
  public void run() {
/*    	System.out.println("\nServer status on. Ports used:" +
    											"\n- FTP: "+ DEFAULT_FTP_PORT +
    											"\n- client authentication: "+ DEFAULT_AUTHENTICATION_PORT +
    											"\n- client connection: "+ DEFAULT_CLIENT_PORT
    											);*/
    try {
      int i = 0;
//					Listen forever for new connections to be authenticated
      while (true) {
        Socket authentication_socket = authentication_serversocket.accept();
//							The connection number is the users vector size
        synchronized (getConnectedUsersList()) {
          connectionNumber = getConnectedUsersList().size();
        }
        AuthenticationConnection connection = new AuthenticationConnection(this, authentication_socket, 3, connectionNumber);
//							Set a little timeout to wait for any connection to
//							be created before another can be accepted
        synchronized (this) {
          try {
            this.wait(1000);
          } catch (InterruptedException e) {
            ;
          }
        }
      }
    } catch (IOException e) {
      fail(e, "Exception while listening for connections", false);
    }
  }

  // Start the server up, listening on an optionally specified port
  public static void main(String[] args) {
    String tmpKeyCode = null;
    boolean backKeyCode = true;
    int defPort = 2000;
    int index = 0;
    while (index + 1 < args.length) {
//        System.out.println(args[index] + " " + args[index+1]);
      if (args[index].equalsIgnoreCase("-K"))
        tmpKeyCode = args[++index];
      else if (args[index].equalsIgnoreCase("-R")) {
        if (args[++index].equalsIgnoreCase("false"))
          backKeyCode = false;
      } else if (args[index].equalsIgnoreCase("-P"))
        defPort = Integer.valueOf(args[++index]).intValue();
      index++;
    }

    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
    } catch (Exception exc) {
      ;
    }

    new Server(tmpKeyCode, backKeyCode, defPort);
  }

/*    public synchronized Vector getUsersPasswordsVector() {
    	return userpwdVector;
    }*/

  public synchronized String getUser(int index) {
    try {
      return (String) getConnectedUsersList().elementAt(index);
    } catch (ArrayIndexOutOfBoundsException outboundexc) {
      return null;
    }
  }

  public synchronized static ServerSocket getFTPServerSocket() {
    return ftp_serversocket;
  }

  public synchronized static ServerSocket getClientServerSocket() {
    return client_serversocket;
  }

  void readFile() {
    Encryption ioEncr = new Encryption(DEFAULT_KEY_CODE);
    try {
      DataInputStream fileIn = new DataInputStream(new FileInputStream(new File(Server.DEFAULT_PWD_FILE)));
      if (fileIn != null) {
        int usersNumber = fileIn.readInt();
        byte[] b = new byte[16];
        for (int i = 0; i < usersNumber; i++) {
          fileIn.read(b);
          String passwdS = new String(b);
          encryptedpasswords.addElement(encryption.encrypt(ioEncr.decrypt(passwdS)));
          decryptedpasswords.addElement(ioEncr.decrypt(passwdS));
          int len = fileIn.readInt();
          byte[] b1 = new byte[len];
          fileIn.read(b1);
          String userS = new String(b1);
          users.addElement(userS);
        }

/*        while ((tmpLine = fileIn.readLine()) != null) {
          tokenizer = new StringTokenizer(tmpLine);
          String userS = tokenizer.nextToken();
          users.addElement(userS);
//          System.out.println(userS);
          String passwdS = tokenizer.nextToken();
          encryptedpasswords.addElement(passwdS);
          decryptedpasswords.addElement(encryption.decrypt(passwdS));
//          usersNumber++;
        }*/
        fileIn.close();
      }
    } catch (IOException ioexc) {
//			ioexc.printStackTrace();
    }
  }

  void writeNewUsersFile() {
    Encryption ioEncr = new Encryption(DEFAULT_KEY_CODE);
    try {
      DataOutputStream fileOut = new DataOutputStream(
              new FileOutputStream(new File(Server.DEFAULT_PWD_FILE)));
      fileOut.writeInt(getUsersNumber());
      for (int i = 0; i < getUsersNumber(); i++) {
        fileOut.write(ioEncr.encrypt(getDecrPassword(i)).getBytes());
        int len = getSingleUser(i).length();
        fileOut.writeInt(len);
        fileOut.write(getSingleUser(i).getBytes());
      }
      fileOut.close();
    } catch (IOException ioexc) {
      ioexc.printStackTrace();
    }
  }

  String getSingleUser(int index) {
    if (getUsersNumber() == 0)
      return null;
    return (String) users.elementAt(index);
  }

  String getDecrPassword(int index) {
    if (getUsersNumber() == 0)
      return null;
    return (String) decryptedpasswords.elementAt(index);
  }

  String getEncrPassword(int index) {
    if (getUsersNumber() == 0)
      return null;
    return (String) encryptedpasswords.elementAt(index);
  }

  void addUser(String userText, String passwordText) {
    if (encryption == null)
      encryption = new Encryption();
    users.addElement(userText);
    decryptedpasswords.addElement(passwordText);
    encryptedpasswords.addElement(encryption.encrypt(passwordText));
  }

  void editUser(String userText, String passwordText, int selectedUser) {
    if (encryption == null)
      encryption = new Encryption();
    users.setElementAt(userText, selectedUser);
    decryptedpasswords.setElementAt(passwordText, selectedUser);
    encryptedpasswords.setElementAt(encryption.encrypt(passwordText), selectedUser);
  }

  void removeUser(int selectedUser) {
    users.removeElementAt(selectedUser);
    decryptedpasswords.removeElementAt(selectedUser);
    encryptedpasswords.removeElementAt(selectedUser);
  }

  Vector getUsersList() {
    return users;
  }

  int getUsersNumber() {
    return users.size();
  }

  public boolean isUserAuthenticated(String[] dataToCheck) {
    String decrPasswd = encryption.decrypt(dataToCheck[1]);
    for (int i = 0; i < users.size(); i++) {
      String user = getSingleUser(i);
      String passwd = getDecrPassword(i);
//      System.out.println(user+" " +passwd);
      if (dataToCheck[0].equals(user))
        if (decrPasswd.equals(passwd))
          return true;
    }
    return false;
  }

  Vector getConnectedUsersList() {
    return connectedUsers;
  }

}
