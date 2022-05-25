/*
 * @(#)jpvmControlFrame.java created Jan 9, 2004 Casalino
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is 
 * provided as it is as confidential and proprietary information.  
 * You shall not disclose such Confidential Information and shall use 
 * it only in accordance with the terms of the license agreement you 
 * entered into with the author.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.jpvm;

import it.unitn.ing.rista.awt.myJFrame;
import it.unitn.ing.rista.awt.JIconButton;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.border.BevelBorder;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;


/**
 * The jpvmControlFrame is a class
 * 
 * @author Luca Lutterotti
 * @version $Revision: 1.3 $, $Date: 2006/01/19 14:45:58 $
 * @since JDK1.1
 */

public class jpvmControlFrame extends myJFrame {

  JTextField hostTF = null;
  JTextField portTF = null;
  JButton startD = null;
  JButton stopD = null;
  JButton addB = null;
  JButton viewB = null;
  JTextArea textArea = null;
  JScrollPane scrollarea = null;
  JCheckBox newVMCB = null;
  JCheckBox localCB = null;
  JTextField taskTF = null;

  public jpvmControlFrame() {

    super(null);

    initializeSizeAndPosition(false, "jpvmControlFrame.frameWidth", "jpvmControlFrame.frameHeight", 400, 500,
        true, "jpvmControlFrame.framePositionX", "jpvmControlFrame.framePositionY", 50, 50);

    Container c1 = getContentPane();

    c1.setLayout(new BorderLayout(6, 6));
    JPanel principalPanel = new JPanel();
    c1.add(BorderLayout.CENTER, principalPanel);

    principalPanel.setLayout(new BorderLayout(3, 3));

    JPanel controlPanel = new JPanel();
    controlPanel.setLayout(new BorderLayout(3, 3));
    principalPanel.add(BorderLayout.WEST, controlPanel);

    JPanel panel1 = new JPanel();
    panel1.setLayout(new GridLayout(3, 2));
    controlPanel.add(BorderLayout.NORTH, panel1);

    JPanel panel3 = new JPanel();
    panel3.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    panel1.add(panel3);
    newVMCB = new JCheckBox("New VM");
    newVMCB.setToolTipText("Start a new Virtual Machine for each task (only for master)");
    newVMCB.setSelected(jpvmDaemon.newVM);
    newVMCB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        changeVM();
      }
    });
    panel3.add(newVMCB);
    panel3 = new JPanel();
    panel3.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    panel1.add(panel3);
    localCB = new JCheckBox("Use local VM");
    localCB.setToolTipText("Spawn a task also on this local machine (only for master)");
    localCB.setSelected(jpvmDaemon.useLocalTask);
    panel3.add(localCB);
    panel3 = new JPanel();
    panel3.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    panel1.add(panel3);
    panel3.add(new JLabel("Number of tasks: "));
    taskTF = new JTextField(6);
    taskTF.setToolTipText("Set the number of maximum concurrent tasks");
    taskTF.setText(Integer.toString(jpvmDaemon.numTasks));
    panel3.add(taskTF);

    panel3 = new JPanel();
    panel3.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    panel1.add(panel3);

    panel3 = new JPanel();
    panel3.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    panel1.add(panel3);
    startD = new JIconButton("StartDaemon.gif", "Start Daemon");
    startD.setToolTipText("Start the Daemon for parallel computing networking");
    startD.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        startDaemon();
      }
    });
    panel3.add(startD);
    panel3 = new JPanel();
    panel3.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    panel1.add(panel3);
    stopD = new JIconButton("StopDaemon.gif", "Stop Daemon");
    stopD.setToolTipText("Stop the Daemon for parallel computing networking");
    stopD.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        (new Thread() {
          public void run() {
            stopDaemon();
          }
        }).start();
      }
    });
    panel3.add(stopD);

    panel1 = new JPanel();
    panel1.setLayout(new BorderLayout(3, 3));
    panel1.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Connect hosts"));
    controlPanel.add(BorderLayout.CENTER, panel1);
    JPanel panel2 = new JPanel();
    panel2.setLayout(new GridLayout(2, 1, 6, 6));
    panel1.add(BorderLayout.CENTER, panel2);

    panel3 = new JPanel();
    panel3.setLayout(new FlowLayout());
    panel2.add(panel3);
    panel3.add(new JLabel("Host name: "));
    hostTF = new JTextField(32);
    hostTF.setText("");
    panel3.add(hostTF);

    panel3 = new JPanel();
    panel3.setLayout(new FlowLayout());
    panel2.add(panel3);
    panel3.add(new JLabel("Host port: "));
    portTF = new JTextField(32);
    portTF.setText(Integer.toString(jpvmEnvironment.defaultPort));
    panel3.add(portTF);

    panel3 = new JPanel();
    panel3.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    panel1.add(BorderLayout.SOUTH, panel3);
    addB = new JIconButton("AddHost.gif", "Connect to host");
    addB.setToolTipText("Add a new host or connect this machine to the network computing");
    addB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        (new Thread() {
          public void run() {
            addHost();
          }
        }).start();
      }
    });
    panel3.add(addB);

    panel1 = new JPanel();
    panel1.setLayout(new BorderLayout(6, 6));
    panel1.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Network status"));
    principalPanel.add(BorderLayout.CENTER, panel1);

    scrollarea = new JScrollPane(textArea = new JTextArea());
    scrollarea.setBorder(new BevelBorder(BevelBorder.LOWERED));
    textArea.setEditable(false);
    panel1.add(BorderLayout.CENTER, textArea);
    textArea.setPreferredSize(new Dimension(300, 200));

    panel2 = new JPanel();
    panel2.setLayout(new FlowLayout(FlowLayout.LEFT, 6, 6));
    panel1.add(BorderLayout.SOUTH, panel2);
    viewB = new JIconButton("CheckNetwork.gif", "Computing status");
    viewB.setToolTipText("Check the status of the network computing");
    viewB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        (new Thread() {
          public void run() {
            checkNetwork();
          }
        }).start();
      }
    });
    panel2.add(viewB);

    panel1 = new JPanel();
    panel1.setLayout(new FlowLayout(FlowLayout.LEFT, 6, 6));
    c1.add(BorderLayout.SOUTH, panel1);
    this.setHelpButton(panel1);

    this.setTitle("Parallel computing control panel");

    setDefaultCloseOperation(HIDE_ON_CLOSE);

    pack();

  }

  public void startDaemon() {
    jpvmDaemon.useLocalTask = localCB.isSelected();
    jpvmDaemon.numTasks = Integer.valueOf(taskTF.getText()).intValue();
    jpvmConsole.startConsole();
    addB.setEnabled(true);
    startD.setEnabled(false);
    stopD.setEnabled(true);
    viewB.setEnabled(true);
    jpvmEnvironment jpvm = jpvmDaemon.getJpvm();
    textArea.setText("Daemon address: " + jpvm.pvm_mytid().toString());
  }

  public void stopDaemon() {
    jpvmConsole.console("halt", null, 0);
    reset();
  }

  public void changeVM() {
    jpvmDaemon.newVM = newVMCB.isSelected();
  }

  public void checkNetwork() {
    jpvmEnvironment jpvm = jpvmConsole.getJpvm();
    StringBuffer sbuffer = new StringBuffer("");

    jpvmConfiguration conf = jpvm.pvm_config();
    for (int i = 0; i < conf.numHosts; i++) {
      jpvmTaskStatus ps = jpvm.pvm_tasks(conf, i);
      sbuffer.append(ps.hostName);
      sbuffer.append(", ");
      sbuffer.append(ps.numTasks);
      sbuffer.append(" tasks:\n");
      for (int j = 0; j < ps.numTasks; j++) {
        sbuffer.append("\t");
        sbuffer.append(ps.taskNames[j]);
        sbuffer.append("\n");
      }
    }
    textArea.setText(sbuffer.toString());
    if (scrollarea.getVerticalScrollBar() != null)
	    javax.swing.SwingUtilities.invokeLater(new Runnable() {
		    public void run() {
			    scrollarea.getVerticalScrollBar().setValue(0);
		    }
	    });
  }

  public void addHost() {
//    if (!jpvmConsole.daemonRunning)
//      return;

    String hostname = hostTF.getText();
    String port = portTF.getText();
    jpvmConsole.console("add", hostname, Integer.valueOf(port).intValue());

    resetTF();
  }

  public void reset() {
    resetTF();
    addB.setEnabled(false);
    startD.setEnabled(true);
    stopD.setEnabled(false);
    viewB.setEnabled(false);
    textArea.setText("");
  }

  public void resetTF() {
    portTF.setText("");
    hostTF.setText("");
  }

}

