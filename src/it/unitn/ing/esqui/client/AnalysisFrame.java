package it.unitn.ing.esqui.client;

import it.unitn.ing.rista.awt.myJFrame;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/** AnalysisFrame.java
 * <br>
 * Title:			<b>ESQUI Client Analyses</b>
 * </br>
 * Description:	Class to create the analysis frame for the ESQUI
 client
 * @author:			Leonardo Cont, December 2000
 * @revision:		January 2001
 * @comment:		none
 */

public class AnalysisFrame extends JFrame {

  ClientAnalysis newAnalysis = null;
  JScrollPane infoScrollPane = null;
  JTextArea infoArea = null;
  JPanel lowerPanel = null;
  JButton openAdvBox = null;
  JPanel advSettPanel = null;
  boolean isAdvSettPanelVisible = false;

  public AnalysisFrame(ClientAnalysis analysis) {
    super();
    newAnalysis = analysis;
    setTitle(newAnalysis.getTitle());

    getContentPane().setLayout(new KappaLayout());

//		Create a menubar
    JMenuBar menuBar = new JMenuBar();
    setJMenuBar(menuBar);
//		Create the Options menu
    JMenu optionsMenu = (JMenu) menuBar.add(new JMenu("Options"));
    JMenuItem aOpMenuItem = (JMenuItem) optionsMenu.add(new JMenuItem("Open old analysis"));
    aOpMenuItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        System.out.println("open");
      }
    });
    JMenuItem bOpMenuItem = (JMenuItem) optionsMenu.add(new JMenuItem("Save current analysis"));
    bOpMenuItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        System.out.println("save");
      }
    });
    JMenuItem cOpMenuItem = (JMenuItem) optionsMenu.add(new JMenuItem("Save current analysis as..."));
    cOpMenuItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        System.out.println("save as");
      }
    });


//	Create the left panel, always visible
    JPanel alwaysVisiblePanel = new JPanel(new BorderLayout());

//	Create a  panel which will contain info about analysis settings
    JPanel infoPanel = new JPanel(new BorderLayout());
//	Create the analysis info area
    infoArea = new JTextArea();
    infoArea.setLineWrap(true);
    infoArea.setEditable(false);
    infoArea.setFont(new Font("Arial", Font.PLAIN, 10));
//	Create the JScrollPane
    infoScrollPane = new JScrollPane(infoArea);
    infoScrollPane.setPreferredSize(new Dimension(440, 175));
    infoPanel.add(infoScrollPane, BorderLayout.CENTER);

//		SOUTH of infoArea panel is FREE!!!
    infoPanel.setBorder(ClientMisc.newEmptyBorder("CURRENT ANALYSIS SETTINGS", "CENTER", 0, 0));
    alwaysVisiblePanel.add(infoPanel, BorderLayout.CENTER);

//		Create the lower panel
    lowerPanel = new JPanel(new GridLayout(0, 2));
//		Create a button panel to manage the advanced settings in the first column
    JPanel opencloseButtonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
    openAdvBox = ClientMisc.createButton("Lock.gif", "open/close the advanced settings box");
    openAdvBox.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (advSettPanel.isVisible()) {
          setAdvSettingsPanelVisible(false);
        } else {
          newAnalysis.updateParameters();
          setAdvSettingsPanelVisible(true);
        }
      }
    });
    opencloseButtonPanel.add(openAdvBox);
    lowerPanel.add(opencloseButtonPanel);
//		Create the lower button panel in the second column
    JPanel finalButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    JButton sendData = ClientMisc.createButton(ClientMisc.ENTER_BUTTON, "send the current analysis");
    sendData.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        try {
          infoArea.append("\n\rAnalysis sent with following settings:\n\r");
          newAnalysis.sendAnalysis(newAnalysis.getAnalysisFile());
        } catch (Exception exc) {
          exc.printStackTrace();
          return;
        }
//				exitAnalysisFrame();
      }
    });
    finalButtonPanel.add(sendData);
    JButton exitAnalysis = ClientMisc.createButton(ClientMisc.EXIT_BUTTON, "exit the analysis window");
    exitAnalysis.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        exitAnalysisFrame();
      }
    });
    finalButtonPanel.add(exitAnalysis);

    lowerPanel.add(finalButtonPanel);
    alwaysVisiblePanel.add(lowerPanel, BorderLayout.SOUTH);
    getContentPane().add(alwaysVisiblePanel, "0,0,,,1,,");

//		Create the advanced settings panel, toggled visible/hidden
//		Its visibility is triggered by the lock/unlock button.
    advSettPanel = new JPanel(new BorderLayout());
  }

  public void addAdvSettingsPanel(JPanel tmpPanel) {
    advSettPanel.add(tmpPanel, BorderLayout.CENTER);
    advSettPanel.setVisible(false);
  }

  public void setAdvSettingsPanelVisible(boolean closeOpen) {
    if (closeOpen) {
      openAdvBox.setIcon(ClientMisc.getImage("OpenLock.gif"));
      advSettPanel.setBorder(ClientMisc.newEmptyBorder("EXPERT USER MODE", "CENTER", 3, 0));
      advSettPanel.setVisible(true);
      getContentPane().add(advSettPanel, "0,1,,,1,,10");
    } else {
      openAdvBox.setIcon(ClientMisc.getImage("Lock.gif"));
      advSettPanel.setVisible(false);
      getContentPane().remove(advSettPanel);
    }
    pack();
  }

  public boolean getAdvSettingsPanelVisible() {
    return isAdvSettPanelVisible;
  }

  public void exitAnalysisFrame() {
    setVisible(false);
    dispose();
  }

  public void displayInformation(String info) {
    if (info == null) {
      infoArea.setText(info);
      return;
    }
    for (int i = 0; i < 140; i++)
      infoArea.append("-");
    infoArea.append("\n\r");
    infoArea.append(info);
    infoArea.append("Estimated time to perform the analysis: " + newAnalysis.getAnalysisTime() + "\n\r");
  }
}
