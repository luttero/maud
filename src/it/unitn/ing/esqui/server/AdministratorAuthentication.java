package it.unitn.ing.esqui.server;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/** AdministratorAuthentication.java
 * <br>
 * Title:			<b>ESQUI administrator authentication</b>
 * </br>
 * Description:	ESQUI administrator authentication
 * @author:			Leonardo Cont, August 2001
 * @revision:		August 2001
 * @comment:		none
 */

class AdministratorAuthentication extends JFrame {

  private final String ADMINISTRATOR_PASSWORD = "esqui1";

  Container contentPane;
  JFrame userFrame;
  JPasswordField passwordField;

  public AdministratorAuthentication(Server aServer) {
    super("ESQUI administrator authentication");
    final Server theServer = aServer;
    contentPane = this.getContentPane();
    JPanel passwordPanel = new JPanel();
    passwordPanel.setBorder(ServerMisc.newBorder("ESQUI ADMINISTRATOR PASSWORD", 1));
    passwordField = new JPasswordField(12);
    passwordField.setHorizontalAlignment(JPasswordField.CENTER);
    passwordPanel.add(passwordField);
    contentPane.add(passwordPanel, BorderLayout.CENTER);
//	Create buttons panel
    JPanel controlPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    JButton confirmButton = ServerMisc.createButton("Check.gif", "confirm password");
    confirmButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (getPassword().equals(ADMINISTRATOR_PASSWORD)) {
          setVisible(false);
          dispose();
          userFrame = new Users(theServer);
          return;
        }
        ServerMisc.messageBox(null, "ESQUI administrator authentication", "Authentication failed! Try again.", JOptionPane.ERROR_MESSAGE);
        passwordField.setText(null);
      }
    });
    controlPanel.add(confirmButton);
    JButton exitButton = ServerMisc.createButton("Exit.gif", "exit this window");
    exitButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setVisible(false);
        dispose();
      }
    });
    controlPanel.add(exitButton);
    contentPane.add(controlPanel, BorderLayout.SOUTH);

    ServerMisc.locateOnScreen(this, 10, 10);
    resize(300, 130);
    setResizable(false);
    setVisible(true);
  }

  String getPassword() {
    return new String(passwordField.getPassword());
  }
}
