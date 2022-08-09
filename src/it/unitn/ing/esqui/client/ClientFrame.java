package it.unitn.ing.esqui.client;

import it.unitn.ing.rista.awt.myJFrame;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;
import java.io.*;
import java.net.*;
import java.text.*;


/** ClientFrame.java
 * <br>
 * Title:			<b>ESQUI Client Frame</b>
 * </br>
 * Description:		Methods to create the layout for the ESQUI client
 * @author:			Leonardo Cont, December 2000
 * @revision:		January 2001
 * @comment:		none
 */

class ClientFrame extends JFrame {

  public UIManager clientManager;
  RetrieveDataFrame retrieveDataFrame = null;;
  AuthenticationFrame authenticationFrame = null;
  JFrame serverSettingsFrame = null;
  JFrame helpFrame = null;
  JTextField statusBoxTextField = null;
  JTextField serverAddress = null;
  JTextField serverPort = null;
  JTextField keyCode = null;
  JTextField toServerString = null;
  JTextArea toServerArea = null;
  JTextArea fromServerArea = null;
  JButton startstopButton = null;
  ClientAnalysis[] analysis = {new StdTexture("Standard texture"),
                               new OptTexture("Hexagonal texture"),
                               new Quantitative("Quantitative")
  };

  public ClientFrame(String title, int xSize, int ySize) {
    super(title);

//		Create a menubar
    JMenuBar menuBar = new JMenuBar();
    setJMenuBar(menuBar);
//		Create the analysis menu
    JMenu analysisMenu = (JMenu) menuBar.add(new JMenu("Analysis"));
    for (int i = 0; i < analysis.length; i++) {
      final ClientAnalysis tmpAnalysis = analysis[i];
      JMenuItem newAnMenuItem = (JMenuItem) analysisMenu.add(new JMenuItem("New " + tmpAnalysis.getMenuTitle()));
      newAnMenuItem.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          tmpAnalysis.showFrame(tmpAnalysis.getTitle());
        }
      });
    }

    analysisMenu.addSeparator();

    JMenuItem getAnMenuItem = (JMenuItem) analysisMenu.add(new JMenuItem("Retrieve old analysis"));
    getAnMenuItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (retrieveDataFrame != null && retrieveDataFrame.isShowing()) {
          retrieveDataFrame.toFront();
          return;
        }
        retrieveDataFrame = new RetrieveDataFrame();
      }
    });

//		Create the options menu
    JMenu optionsMenu = (JMenu) menuBar.add(new JMenu("Options"));
    JMenuItem aOpMenuItem = (JMenuItem) optionsMenu.add(new JMenuItem("Server settings"));
    aOpMenuItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (serverSettingsFrame != null && serverSettingsFrame.isShowing()) {
          serverSettingsFrame.toFront();
          return;
        }
        serverSettingsFrame = createServerSettingsFrame();
      }
    });

//		Create the interface menu
/*		JMenu interfaceMenu = (JMenu) menuBar.add(new JMenu("Interface"));
		ButtonGroup lookandfeelGroup = new ButtonGroup();
		UIManager.LookAndFeelInfo[] LFs = UIManager.getInstalledLookAndFeels();
		int lfsnumber = LFs.length;
		JRadioButtonMenuItem lfsMenuItem[] = new JRadioButtonMenuItem[lfsnumber];
		for (int i = 0; i < lfsnumber; i++) {
			lfsMenuItem[i] = (JRadioButtonMenuItem)
			interfaceMenu.add(new JRadioButtonMenuItem(LFs[i].getName()));
			lookandfeelGroup.add(lfsMenuItem[i]);
			lfsMenuItem[i].setSelected(UIManager.getLookAndFeel().getName().equals(LFs[i].getName()));
			lfsMenuItem[i].setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_1 + i, ActionEvent.ALT_MASK));
			lfsMenuItem[i].addItemListener(new ItemListener() {
				public void itemStateChanged(ItemEvent e) {
	    			JRadioButtonMenuItem rb = (JRadioButtonMenuItem) e.getSource();
        			try {
	       				if (rb.isSelected()) {
							UIManager.LookAndFeelInfo[] tmpLFs = UIManager.getInstalledLookAndFeels();
							int tmplfsnumber = tmpLFs.length;
							for (int j = 0; j < tmplfsnumber; j++) {
								if (rb.getText().equals(tmpLFs[j].getName())) {
	    	   						UIManager.setLookAndFeel(tmpLFs[j].getClassName());
	    	   						SwingUtilities.updateComponentTreeUI(ClientFrame.this);
	    	   					}
	    	   				}
	       				}
        			} catch (Exception exc) {
						rb.setEnabled(false);
						System.err.println("Unsupported LookAndFeel: " + rb.getText());
        			}
        		}
			});
		}*/

//		Create the help menu
    JMenu helpMenu = (JMenu) menuBar.add(new JMenu("Help"));
    JMenuItem aHlpMenuItem = (JMenuItem) helpMenu.add(new JMenuItem("Help index"));
    aHlpMenuItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        helpFrame = new JFrame("ESQUI client help");
        HtmlEditor helpPanel = new HtmlEditor(ClientMisc.getResource("help/index.html"));
        helpFrame.getContentPane().add(helpPanel);
        helpFrame.addWindowListener(new WindowAdapter() {
          public void windowClosing(WindowEvent e) {
            helpFrame.dispose();
          }
        });
        ClientMisc.locateOnScreen(helpFrame, 10, 10);
        helpFrame.setSize(new Dimension(600, 400));
        helpFrame.setVisible(true);
      }
    });
//		Create the mainPanel (the NORTH is FREE!!!)
    JPanel mainPanel = new JPanel(new BorderLayout(0, 5));

//	Create the connection status panel and connection buttons
    JPanel connectionPanel = new JPanel();
    JPanel statusPanel = new JPanel();
    statusPanel.setBorder(ClientMisc.newBorder(null, 1));
    JLabel checkConnectionLabel = new JLabel("Connection status");
    statusPanel.add(checkConnectionLabel);
    statusBoxTextField = new JTextField(12);
    statusBoxTextField.setFont(new Font("Arial", Font.BOLD, 12));
    statusBoxTextField.setHorizontalAlignment(JLabel.CENTER);
    statusBoxTextField.setEnabled(false);
    statusBoxTextField.setDisabledTextColor(Color.black);
    displayConnectionStatus();
    statusPanel.add(statusBoxTextField);
    connectionPanel.add(statusPanel);
//	Create a button to start/stop the connection
    JPanel startstopPanel = new JPanel();
    startstopButton = ClientMisc.createButton("GreenFlag.gif", "start/stop the current connection");
    startstopButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (Connection.getConnectionActive()) {
          try {
            Connection.sendString(InetAddress.getLocalHost().toString() +
                    " closed connection on " +
                    (DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT, Locale.ITALY).format(new Date())));
          } catch (Exception exc) {
            ;
          }
          Connection.setStoppedByUser(true);
          Connection.setConnectionActive(false);
          startstopButton.setIcon(ClientMisc.getImage("GreenFlag.gif"));
          displayConnectionStatus();
          toServerArea.append("\n");
        } else {
          Connection.setStoppedByUser(false);
          if (authenticationFrame != null && authenticationFrame.isShowing()) {
            authenticationFrame.toFront();
            return;
          }
          authenticationFrame = new AuthenticationFrame((ClientFrame) getTopLevelFrame());
        }
      }
    });
    startstopPanel.add(startstopButton);
    connectionPanel.add(startstopPanel);

//	Add the connection panel to the main panel
    mainPanel.add(connectionPanel, BorderLayout.NORTH);

//		Create the central panel (the NORTH area is free!!!)
    JPanel centralPanel = new JPanel(new BorderLayout(0, 1));

//		Create the connection info panel
    JPanel infoPanel = new JPanel(new GridLayout(2, 1));

//		Create the first panel and its components
//		Create the text area panel and its buttons
    JPanel firstAreaPanel = new JPanel(new BorderLayout());
    firstAreaPanel.setBorder(ClientMisc.newBorder("Messages sent to server", 2));
    toServerArea = new JTextArea();
    toServerArea.setLineWrap(true);
    toServerArea.setEditable(false);
    toServerArea.append("Welcome to ESQUI Client, ver 1.0\n");
    toServerArea.append("Today is " + ClientMisc.todayIs() + "\n");
//		Create the JScrollPane
    JScrollPane toServerScrollPane = new JScrollPane(toServerArea);
//		Add the JScrollPane to the datapanel
    firstAreaPanel.add(toServerScrollPane, BorderLayout.CENTER);
//		Create some buttons near the textarea to perform some actions
    JPanel toAreaButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

    JButton toClearAreaTextButton = ClientMisc.createButton("CleanUp.gif", "clear 'Messages sent to server' area");
    toClearAreaTextButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        toServerArea.setText(null);
      }
    });
    toAreaButtonPanel.add(toClearAreaTextButton);
    firstAreaPanel.add(toAreaButtonPanel, BorderLayout.EAST);
//		Add the first panel
    infoPanel.add(firstAreaPanel);

//		Create the second panel and its components
//		Create the text area panel and its buttons
    JPanel secondAreaPanel = new JPanel(new BorderLayout());
    fromServerArea = new JTextArea();
    fromServerArea.setLineWrap(true);
    fromServerArea.setEditable(false);
//		Create the JScrollPane
    JScrollPane fromServerScrollPane = new JScrollPane(fromServerArea);
//		Add the JScrollPane to the datapanel
    secondAreaPanel.add(fromServerScrollPane, BorderLayout.CENTER);
    secondAreaPanel.setBorder(ClientMisc.newBorder("Messages received from server", 2));
//		Create some buttons near the textarea to perform some actions
    JPanel fromAreaButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    JButton fromClearAreaTextButton = ClientMisc.createButton("CleanUp.gif", "clear 'Messages received from server' area");
    fromClearAreaTextButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        fromServerArea.setText(null);
      }
    });
    fromAreaButtonPanel.add(fromClearAreaTextButton);
    secondAreaPanel.add(fromAreaButtonPanel, BorderLayout.EAST);
//		Add the second panel
    infoPanel.add(secondAreaPanel);
//		Add the info panel to the center of your main frame
    centralPanel.add(infoPanel, BorderLayout.CENTER);

//		Create the command line panel and its components
    JPanel cmdLinePanel = new JPanel(new BorderLayout());
    cmdLinePanel.setBorder(ClientMisc.newBorder(null, 2));
    JLabel cmdLineLabel = new JLabel("Command to server:");
    cmdLinePanel.add(cmdLineLabel, BorderLayout.WEST);
//		Create a textfield to send any messages to server
    toServerString = new JTextField();
    toServerString.addKeyListener(new KeyListener() {
      public void keyPressed(KeyEvent e) {
        if (e.getKeyCode() == e.VK_ENTER) {
          try {
            Connection.sendString(toServerString.getText());
          } catch (Exception exc) {
            ;
          }
          if (Connection.getConnectionActive())
            toServerArea.append(toServerString.getText() + "\n");
          toServerString.setText(null);
        }
      }

      public void keyTyped(KeyEvent e) {
      }

      public void keyReleased(KeyEvent e) {
      }
    });
    toServerString.addFocusListener(new TextFieldListener(this));
    cmdLinePanel.add(toServerString, BorderLayout.CENTER);
//	Add the command line and its button
    centralPanel.add(cmdLinePanel, BorderLayout.SOUTH);
//	Add the central panel to the mainPanel
    mainPanel.add(centralPanel, BorderLayout.CENTER);
//	Add the main panel to the frame
    getContentPane().add(mainPanel, BorderLayout.CENTER);

//	Create a lower close panel
    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    JButton closeButton = ClientMisc.createButton("Exit", ClientMisc.EXIT_BUTTON, null);
    closeButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        closeClient();
      }
    });
    buttonPanel.add(closeButton);
//	Add the lower close button to the frame
    getContentPane().add(buttonPanel, BorderLayout.SOUTH);

    this.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        closeClient();
      }
    });

    pack();
    setSize(xSize, ySize);
    ClientMisc.locateOnScreen(this, ClientMisc.X_WINDOW_RELPOS, 25);
    setVisible(true);
  }

/*
	Methos to switch a textfield between activated and disactivated
*/
  public void displayConnectionStatus() {
    if (Connection.getConnectionActive() && Connection.getSocketActive()) {
      statusBoxTextField.setBackground(Color.green);
      statusBoxTextField.setText("CONNECTED");
    } else if (Connection.getConnectionActive()) {
      statusBoxTextField.setBackground(Color.yellow);
      statusBoxTextField.setText("WAITING");
    } else {
      statusBoxTextField.setBackground(Color.red);
      statusBoxTextField.setText("NOT CONNECTED");
    }
  }

/*
	Method to read each line in the textarea and return them in an array
*/

  String[] textAreaLines(String tmpAreaString) {
    Vector tmpVector = new Vector(5, 5);
    StringTokenizer tmpTokenString = new StringTokenizer(tmpAreaString);
    while (tmpTokenString.hasMoreTokens()) {
      tmpVector.addElement(tmpTokenString.nextToken());
    }
    String[] tmpString = new String[tmpVector.size()];
    for (int i = 0; i < tmpVector.size(); i++)
      tmpString[i] = (String) tmpVector.elementAt(i);
    return tmpString;
  }

/*
	Method to create the server settings frame
*/
  JFrame createServerSettingsFrame() {
    JFrame tmpFrame = new JFrame("Server settings");
    JPanel parametersPanel = new JPanel(new GridLayout(3, 3));
    JLabel addressLabel = new JLabel("Server IP address:");
    parametersPanel.add(addressLabel);
    serverAddress = new JTextField(12);
    serverAddress.addFocusListener(new TextFieldListener(tmpFrame));
    serverAddress.setText(Client.getSetting(0));
    parametersPanel.add(serverAddress);
    JLabel portLabel = new JLabel("Server port:");
    parametersPanel.add(portLabel);
    serverPort = new JTextField();
    serverAddress.addFocusListener(new TextFieldListener(tmpFrame));
    serverPort.setText(Client.getSetting(1));
    parametersPanel.add(serverPort);
    JLabel keycodeLabel = new JLabel("Key code:");
    parametersPanel.add(keycodeLabel);
    keyCode = new JTextField();
    keyCode.addFocusListener(new TextFieldListener(tmpFrame));
    keyCode.setText(Client.getSetting(2));
    parametersPanel.add(keyCode);
    tmpFrame.getContentPane().add(parametersPanel, BorderLayout.CENTER);

    JPanel closeButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    JButton exitButton = ClientMisc.createButton(null, ClientMisc.EXIT_BUTTON, null);
    exitButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        closeServerSettingsFrame();
      }
    });
    closeButtonPanel.add(exitButton);
    JButton confirmButton = ClientMisc.createButton("Check.gif", "save parameters and close the window");
    confirmButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        Client.setSetting(serverAddress.getText().trim(), 0);
        Client.setSetting(serverPort.getText().trim(), 1);
        Client.setSetting(keyCode.getText().trim(), 2);
        closeServerSettingsFrame();
      }
    });
    closeButtonPanel.add(confirmButton);
    tmpFrame.getContentPane().add(closeButtonPanel, BorderLayout.SOUTH);

    tmpFrame.pack();
    ClientMisc.locateOnScreen(tmpFrame, ClientMisc.X_WINDOW_RELPOS, 15);
    tmpFrame.setVisible(true);
    return tmpFrame;
  }

  void closeServerSettingsFrame() {
    String[] finalsettings = new String[3];
    for (int i = 0; i < finalsettings.length; i++)
      finalsettings[i] = Client.clientsettingsdefinition[i] + Client.clientsettings[i];
    ClientMisc.logInformation(Client.DEFAULT_PREFS_FILE, finalsettings, true, this, JOptionPane.ERROR_MESSAGE);
    serverSettingsFrame.setVisible(false);
    serverSettingsFrame.dispose();
    serverSettingsFrame = null;
  }

/*
	Method to close the client and save the settings
*/
  void closeClient() {
    if (Connection.getSocketActive())
      try {
        Connection.sendString(InetAddress.getLocalHost().toString() +
                " closed connection on " +
                (DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT, Locale.ITALY).format(new Date())));
      } catch (Exception exc) {
        ;
      }
    if (!Client.isForMaud)
      System.exit(0);
    setVisible(false);
    dispose();
  }

  JFrame getTopLevelFrame() {
    return this;
  }
}
