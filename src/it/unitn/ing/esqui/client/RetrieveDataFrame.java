package it.unitn.ing.esqui.client;

import it.unitn.ing.rista.util.Misc;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

/** RetrieveDataFrame.java
 * <br>
 * Title:			<b>ESQUI Client Retrieve Data</b>
 * </br>
 * Description:	Class to create a frame for retrieving data for the ESQUI
 client
 * @author:			Leonardo Cont, February 2001
 * @revision:		January 2001
 * @comment:		February
 */

public class RetrieveDataFrame extends JFrame {

  JList filesList = null;
  JButton getListButton = null;
  JButton retrieveButton = null;
  String dataInfo = null;

  public RetrieveDataFrame() {
    super("Retrieve analysis data");

//	Create a temporary textfield for saving as the file
    JPanel localFilePanel = new JPanel();
    JLabel saveAsLabel = new JLabel("Select one of the available files and the retrieve it clicking on the green double arrow.");
    localFilePanel.add(saveAsLabel);
    getContentPane().add(localFilePanel, BorderLayout.CENTER);

//	Create the list panel
    JPanel listPanel = new JPanel();
    listPanel.setBorder(ClientMisc.newBorder("Available files", 2));
    filesList = new JList();
    filesList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    filesList.addMouseListener(new MouseAdapter() {
      public void mouseReleased(MouseEvent e) {
//				saveAsField.setText(getSelectedFile());
      }
    });
    JScrollPane scrollPanel = new JScrollPane(filesList);
    listPanel.add(scrollPanel);
    getContentPane().add(listPanel, BorderLayout.NORTH);

//	Create a lower close panel
    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    JButton closeButton = ClientMisc.createButton(null, ClientMisc.EXIT_BUTTON, null);
    closeButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setVisible(false);
        dispose();
      }
    });
    buttonPanel.add(closeButton);
    getListButton = ClientMisc.createButton("DocumentIn.gif", "refresh available files list");
    getListButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (Connection.getSocketActive() == false)
          return;
        filesList.removeAll();
        getFileList();
        retrieveButton.setEnabled(true);
      }
    });
    buttonPanel.add(getListButton);
    retrieveButton = ClientMisc.createButton(null, ClientMisc.ENTER_BUTTON, null);
    retrieveButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        String fileToRetrieve = getSelectedFile();
        if (fileToRetrieve == null)
          return;
        // If data info are not retrieved, print five marks
        dataInfo = new String("*****");
        sendRequest(fileToRetrieve, ServerConnection.analysisCommands[6]);
        String fileToCreate = ClientMisc.fileToSave(getThis(), "Save retrieved file as", fileToRetrieve);
        if (fileToCreate == null)
          return;
        EsquiReader.setLocalFile(fileToCreate);
//				createSaveFrame(fileToRetrieve, fileToCreate);
        sendRequest(fileToRetrieve, ServerConnection.analysisCommands[3]);
      }
    });
    retrieveButton.setEnabled(false);
    buttonPanel.add(retrieveButton);

    getContentPane().add(buttonPanel, BorderLayout.SOUTH);

    pack();
    ClientMisc.locateOnScreen(this, 50, 50);
    setVisible(true);

    try {
      if (!Connection.getSocketActive())
        throw new Exception();
      filesList.removeAll();
      getFileList();
      retrieveButton.setEnabled(true);
    } catch (Exception exc) {
      getListButton.setEnabled(false);
      ClientMisc.messageBox(getThis(), "Alert", "The connection or the reader is not active! Enable the connection before retrieving data.", JOptionPane.WARNING_MESSAGE);
    }
  }

  RetrieveDataFrame getThis() {
    return this;
  }

  void getFileList() {
    try {
      Connection.sendString(ServerConnection.analysisCommands[4]);
    } catch (Exception exc) {
      ClientMisc.messageBox(this, "Alert", "The server is not responding! Please try again later.", JOptionPane.WARNING_MESSAGE);
    }
  }

  public void setList(Vector tmpVector) {
    filesList.setListData(tmpVector);
  }

  String getSelectedFile() {
    if (filesList.getSelectedIndex() != -1)
      return (String) filesList.getSelectedValue();
    return null;
  }

/*	String getLocalFileName() {
		String tmpString = saveAsField.getText();
		if (tmpString != null)
			return tmpString;
		ClientMisc.messageBox(this, "Alert", "Put a name for the local data file before retrieving data.", JOptionPane.WARNING_MESSAGE);
		return null;
	}*/

  void sendRequest(String datafile, String command) {
    try {
      Connection.sendString(command + datafile);
    } catch (Exception exc) {
      ClientMisc.messageBox(this, "Alert", "The server is not responding! Please try again later.", JOptionPane.WARNING_MESSAGE);
    }
  }

  void createSaveFrame(String fileToRetrieve, String fileToCreate) {
    final JFrame saveFrame = new JFrame("Datafile retrieving information");
    final String fileToGet = fileToRetrieve;
    JPanel infoPanel = new JPanel(new GridLayout(0, 2));
    infoPanel.setBorder(ClientMisc.newBorder(null, 3));
    JLabel infoLabelONE = new JLabel("File to be retrieved:");
    infoPanel.add(infoLabelONE);
    JTextField infoONE = new JTextField(fileToRetrieve, 18);
    infoONE.setEditable(false);
    infoPanel.add(infoONE);
    JLabel infoLabelTWO = new JLabel("Uncompressed file size:");
    infoPanel.add(infoLabelTWO);
    JTextField infoTWO = new JTextField(dataInfo + " bytes", 18);
    infoTWO.setEditable(false);
    infoPanel.add(infoTWO);
    JLabel infoLabelTHREE = new JLabel("Estimated compressed file size:");
    infoPanel.add(infoLabelTHREE);
    int compressedSize = (int) (Integer.parseInt(dataInfo) * 0.18);
    JTextField infoTHREE = new JTextField(Integer.toString(compressedSize) + " bytes", 18);
    infoTHREE.setEditable(false);
    infoPanel.add(infoTHREE);
    JLabel infoLabelFOUR = new JLabel("File retrieved is being saved as:");
    infoPanel.add(infoLabelFOUR);
    JTextField infoFOUR = new JTextField(fileToCreate, 18);
    infoFOUR.setEditable(false);
    infoPanel.add(infoFOUR);
    saveFrame.getContentPane().add(infoPanel, BorderLayout.CENTER);
    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    JButton closeButton = ClientMisc.createButton(null, ClientMisc.EXIT_BUTTON, null);
    closeButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        saveFrame.setVisible(false);
        saveFrame.dispose();
      }
    });
    buttonPanel.add(closeButton);
    JButton getFileButton = ClientMisc.createButton(null, ClientMisc.ENTER_BUTTON, null);
    getFileButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        sendRequest(fileToGet, ServerConnection.analysisCommands[3]);
        System.out.println(fileToGet);
        saveFrame.setVisible(false);
        saveFrame.dispose();
      }
    });
    buttonPanel.add(getFileButton);
    if (fileToCreate.startsWith("null")) {
      infoTWO.setText("Bad file name! Try again.");
      infoTWO.setBackground(Color.red);
      getFileButton.setEnabled(false);
    }
    saveFrame.getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    saveFrame.pack();
    saveFrame.setVisible(true);
    ClientMisc.locateOnScreen(saveFrame, 30, 30);
  }

}
