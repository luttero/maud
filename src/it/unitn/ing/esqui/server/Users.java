package it.unitn.ing.esqui.server;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.io.*;
import java.util.*;

/** Users.java
 * <br>
 * Title:			<b>ESQUI Users</b>
 * </br>
 * Description:	ESQUI users manager
 * @author:			Leonardo Cont, August 2001
 * @revision:		August 2001
 * @comment:		none
 */

class Users extends JFrame {

  Container contentPane;
  JPanel usersPanel;
  JComboBox usersBox;
  JButton editUserButton;
  JButton deleteUserButton;
  JTextField userField;
  JTextField passwordField;
  JLabel noUsersLabel;

  int selectedUser = 0;
  Server theServer = null;

  public Users(Server aServer) {
    super("ESQUI Users manager");

    theServer = aServer;
    contentPane = this.getContentPane();

//	Create the users combox
    usersPanel = new JPanel();
    usersPanel.setBorder(ServerMisc.newBorder("Select the user to be edited/removed", 1));
    usersBox = new JComboBox(theServer.getUsersList());
    setSize(usersBox, 140, 20);
    usersBox.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        selectedUser = usersBox.getSelectedIndex();
        userField.setText(theServer.getSingleUser(selectedUser));
        passwordField.setText(theServer.getEncrPassword(selectedUser));
      }
    });
    usersPanel.add(usersBox);
    contentPane.add(usersPanel, BorderLayout.NORTH);
//	Create the users/password panel
    JPanel parametersPanel = parametersPanel();
    contentPane.add(parametersPanel, BorderLayout.CENTER);
//	Create buttons panel
    JPanel controlPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    editUserButton = ServerMisc.createButton("EditUser.gif", "edit selected ESQUI user");
    editUserButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        addEditUser("Edit selected ESQUI user", 1);
        theServer.writeNewUsersFile();
        setZeroIndex();
      }
    });
    controlPanel.add(editUserButton);
    deleteUserButton = ServerMisc.createButton("DeleteUser.gif", "delete selected ESQUI user");
    deleteUserButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (ServerMisc.confirmBox(null,
                "Confirm delete ESQUI user",
                "Do you really want to delete " + theServer.getSingleUser(selectedUser) + " user?",
                JOptionPane.QUESTION_MESSAGE) == JOptionPane.CANCEL_OPTION)
          return;
        theServer.removeUser(selectedUser);
        theServer.writeNewUsersFile();
//			Disable edit/delete buttons if no users are present
        checkUsersNumber();
        setZeroIndex();
      }
    });
    controlPanel.add(deleteUserButton);
    JButton addUserButton = ServerMisc.createButton("User.gif", "add a new ESQUI user");
    addUserButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        addEditUser("Add a new ESQUI user", 0);
        checkUsersNumber();
      }
    });
    controlPanel.add(addUserButton);
    JButton exitButton = ServerMisc.createButton("Exit.gif", "exit ESQUI users manager");
    exitButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setVisible(false);
        dispose();
      }
    });
    controlPanel.add(exitButton);
    contentPane.add(controlPanel, BorderLayout.SOUTH);

//	Disable edit/delete buttons if no users have been loaded
    checkUsersNumber();

    ServerMisc.locateOnScreen(this, 10, 10);
    pack();
    setSize(300, 190);
    setResizable(false);
    setVisible(true);
  }

  void setSize(JComponent component, int width, int height) {
    component.setPreferredSize(new Dimension(width, height));
  }

  void enableEditDeleteButtons(boolean onOff) {
    deleteUserButton.setEnabled(onOff);
    editUserButton.setEnabled(onOff);
  }

  void enableParametersFields(boolean onOff) {
    userField.setEditable(onOff);
    passwordField.setEditable(onOff);
  }

  void checkUsersNumber() {
    if (theServer.getUsersNumber() == 0) {
      enableEditDeleteButtons(false);
      usersBox.setVisible(false);
      noUsersLabel = new JLabel("no users in the database");
      usersPanel.add(noUsersLabel);
      userField.setVisible(false);
      passwordField.setVisible(false);
    } else if (theServer.getUsersNumber() >= 1) {
      enableEditDeleteButtons(true);
      if (noUsersLabel != null)
        usersPanel.remove(noUsersLabel);
      usersBox.setVisible(true);
      userField.setVisible(true);
      passwordField.setVisible(true);
    }
  }

  void setZeroIndex() {
    if (usersBox.getItemCount() == 0)
      return;
    usersBox.setSelectedIndex(0);
  }

//Create the users/password panel
  JPanel parametersPanel() {
    JPanel parametersPanel = new JPanel(new GridLayout(0, 2, 5, 0));
    parametersPanel.setBorder(ServerMisc.newBorder(null, 2));
    JLabel tmpLabel = new JLabel("username", JLabel.CENTER);
    parametersPanel.add(tmpLabel);
    tmpLabel = new JLabel("password", JLabel.CENTER);
    parametersPanel.add(tmpLabel);
    userField = new JTextField(12);
    userField.setHorizontalAlignment(JTextField.CENTER);
    userField.setText(theServer.getSingleUser(selectedUser));
    parametersPanel.add(userField);
    passwordField = new JTextField(12);
    passwordField.setHorizontalAlignment(JTextField.CENTER);
    passwordField.setText(theServer.getEncrPassword(selectedUser));
    parametersPanel.add(passwordField);
    enableParametersFields(false);
    return parametersPanel;
  }

  void addEditUser(String title, int type) {
    final JFrame addUserFrame = new JFrame(title);
    final int actionType = type;
    JPanel parametersPanel = new JPanel(new GridLayout(0, 2, 2, 0));
    JLabel tmpLabel = new JLabel("Username");
    parametersPanel.add(tmpLabel);
    tmpLabel = new JLabel("Password");
    parametersPanel.add(tmpLabel);
    final JTextField tmpUserField = new JTextField(12);
    parametersPanel.add(tmpUserField);
    final JPasswordField tmpPasswordField = new JPasswordField(12);
    parametersPanel.add(tmpPasswordField);
    if (actionType == 1) {
      tmpUserField.setText(theServer.getSingleUser(selectedUser));
      tmpPasswordField.setText(theServer.getDecrPassword(selectedUser));
    }
    addUserFrame.getContentPane().add(parametersPanel, BorderLayout.CENTER);
    JPanel controlPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    JButton okButton = ServerMisc.createButton("Check.gif", "confirm");
    okButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        String userText = tmpUserField.getText();
        String passwordText = tmpPasswordField.getText();
        try {
          checkParameters(userText, passwordText);
        } catch (Exception exc) {
          ServerMisc.messageBox(null, "Error on parameters", exc.getMessage(), JOptionPane.ERROR_MESSAGE);
          return;
        }
        switch (actionType) {
          case 0:
//					Check the username is not yet used
            if (isNameUsed(userText)) {
              ServerMisc.messageBox(null, "Error on username", "Username already in use. Try another one.", JOptionPane.ERROR_MESSAGE);
              return;
            }
//					Note that the new user will be added automatically to the users combo box
//					Enable edit/delete buttons if at least one user is present
            theServer.addUser(userText, passwordText);
            setZeroIndex();
            break;
          case 1:
            theServer.editUser(userText, passwordText, selectedUser);

        }
        theServer.writeNewUsersFile();
        addUserFrame.setVisible(false);
        addUserFrame.dispose();
      }
    });
    controlPanel.add(okButton);
    JButton cancelButton = ServerMisc.createButton("Delete.gif", "cancel");
    cancelButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        addUserFrame.setVisible(false);
        addUserFrame.dispose();
      }
    });
    controlPanel.add(cancelButton);
    addUserFrame.getContentPane().add(controlPanel, BorderLayout.SOUTH);
    ServerMisc.locateOnScreen(addUserFrame, 20, 20);
    addUserFrame.setSize(250, 115);
    addUserFrame.setVisible(true);
  }

  void checkParameters(String user, String password)
          throws Exception {
    int userLength = user.length();
    int paswordLength = password.length();
    if (userLength > 16)
      throw new Exception("Username or password too long (max 16 characters)!");
    if (userLength < 3)
      throw new Exception("Username too short (min 3 characters)!");
    if (paswordLength > 16)
      throw new Exception("Password too long (max 16 characters)!");
    if (paswordLength < 3)
      throw new Exception("Password too short (min 3 characters)!");
  }

  boolean isNameUsed(String name) {
    if (theServer.getUsersNumber() == 0)
      return false;
    int i = 0;
    while (!(theServer.getSingleUser(i).equals(name))) {
      i++;
      if (i == theServer.getUsersNumber())
        return false;
    }
    return true;
  }
}
