package it.unitn.ing.esqui.client;

import it.unitn.ing.rista.awt.myJFrame;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/** AuthenticationFrame.java
 * <br>
 * Title:			<b>ESQUI Authentication Frame</b>
 * </br>
 * Description:	Class to create the authentication frame
 * @author:			Leonardo Cont, December 2000
 * @revision:		February 2001
 * @comment:		none
 */

class AuthenticationFrame extends JFrame {

  ClientFrame clientframe = null;
  JTextField userField = null;
  JPasswordField passwordField = null;

  public AuthenticationFrame(ClientFrame clientframe) {
    super("Authentication");
    this.clientframe = clientframe;

    JPanel tmpPanel = new JPanel(new BorderLayout());
    Font tmpFont = new Font("Arial", Font.BOLD, 12);
    JPanel valuesPanel = new JPanel(new KappaLayout());
    valuesPanel.setBackground(Color.yellow);
    valuesPanel.setBorder(ClientMisc.newBorder(null, 4));
    JLabel userLabel = new JLabel("Username:");
    userLabel.setFont(tmpFont);
    valuesPanel.add(userLabel, "0,0,,,,,1");
    userField = new JTextField(12);
    valuesPanel.add(userField, "1,0,,,,,1");
    JLabel passwdLabel = new JLabel("Password:");
    passwdLabel.setFont(tmpFont);
    valuesPanel.add(passwdLabel, "0,1,,,,,1");
    passwordField = new JPasswordField();
    valuesPanel.add(passwordField, "1,1,,,,w,1");
    tmpPanel.add(valuesPanel, BorderLayout.CENTER);

    JPanel closeButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    JButton exitButton = ClientMisc.createButton(ClientMisc.EXIT_BUTTON, null);
    exitButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setVisible(false);
        dispose();
      }
    });
    closeButtonPanel.add(exitButton);

    JButton confirmButton = ClientMisc.createButton("Check.gif", null);
    confirmButton.addMouseListener(new MouseAdapter() {
      JButton tmpButton = null;

      public void mouseEntered(MouseEvent e) {
        tmpButton = (JButton) e.getSource();
        String valueONE = userField.getText();
        String valueTWO = String.valueOf(passwordField.getPassword());
        if (valueONE.equals("")) {
          userField.setBackground(Color.red);
          if (valueTWO.equals(""))
            passwordField.setBackground(Color.red);
          tmpButton.setEnabled(false);
          return;
        } else if (valueTWO.equals("")) {
          passwordField.setBackground(Color.red);
          tmpButton.setEnabled(false);
          return;
        }
        tmpButton.setEnabled(true);
      }

      public void mouseExited(MouseEvent e) {
        tmpButton = (JButton) e.getSource();
        userField.setBackground(Color.white);
        passwordField.setBackground(Color.white);
        tmpButton.setEnabled(true);
      }
    });
    confirmButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        String password = String.valueOf(passwordField.getPassword());
        if (password.length() < 3 || password.length() > 16) {
          ClientMisc.messageBox(getThis(),
                  "Error on password",
                  "The password must be between 3 and 16 character",
                  JOptionPane.ERROR_MESSAGE);
          passwordField.setText(null);
          return;
        }
        Client.setUser(userField.getText());
        Client.setPassword(password);
        try {
          Connection authentication = new AuthenticationConnection(getTopFrame());
        } catch (Exception exc) {
          System.out.println("Error on authentication procedure!");
        }
        setVisible(false);
        dispose();
      }
    });
    closeButtonPanel.add(confirmButton);
    tmpPanel.add(closeButtonPanel, BorderLayout.SOUTH);

    getContentPane().add(tmpPanel);
    pack();
    setResizable(false);
    ClientMisc.locateOnScreen(this, ClientMisc.X_WINDOW_RELPOS, 20);
    setVisible(true);
  }

  ClientFrame getTopFrame() {
    return clientframe;
  }

  AuthenticationFrame getThis() {
    return this;
  }
}
