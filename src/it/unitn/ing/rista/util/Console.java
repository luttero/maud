/*
 * @(#)Console.java created Jan 7, 2008 Caen
 *
 * Copyright (c) 2008 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.util;

import it.unitn.ing.rista.awt.Utility;
import it.unitn.ing.rista.awt.myJFrame;

import javax.swing.*;
import java.io.*;
import java.awt.event.*;
import java.awt.*;

/**
 * The Console is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jan 7, 2008 3:53:44 PM $
 * @since JDK1.1
 */
public class Console extends myJFrame {
/*  PipedInputStream piOut = null;
  PipedInputStream piErr = null;
  PipedOutputStream poOut;
  PipedOutputStream poErr;*/
  JTextArea textArea;
  String saveas = "Save as...";
	String clear = "Clear console";
  String hide = "Hide console";
  static JCheckBoxMenuItem theControlCheckBoxInTheMenu = null;
	static int idealSize;
	static int maxExcess;

	public Console() throws IOException {

		super(null);
		initializeSizeAndPosition(
				true, "console.frameWidth", "console.frameHeight", 900, 400,
				true, "console.framePositionX", "console.framePositionY", 100, 500);

		// Set up System.out
/*    piOut = new PipedInputStream();
    poOut = new PipedOutputStream(piOut);
    System.setOut(new PrintStream(poOut, true));

    // Set up System.err
    piErr = new PipedInputStream();
    poErr = new PipedOutputStream(piErr);
    System.setErr(new PrintStream(poErr, true));*/

    // Add a scrolling text area
	  textArea = new JTextArea();
    textArea.setEditable(false);
    textArea.setRows(20);
    textArea.setColumns(50);
	  getContentPane().setLayout(new BorderLayout());
	  getContentPane().add(new JScrollPane(textArea),BorderLayout.CENTER);
    JMenuBar menubar = new JMenuBar();
    JMenu fileMenu = new JMenu("File");
	  menubar.add(fileMenu);
	  JMenuItem menuItem = new JMenuItem(saveas);
    fileMenu.add(menuItem);
	  menuItem.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent action) {
				saveContentToFile();
		  }
	  });
	  menuItem = new JMenuItem(clear);
	  fileMenu.add(menuItem);
	  menuItem.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent action) {
				textArea.setText("");
		  }
	  });
    fileMenu.add(new JMenuItem("-"));
	  menuItem = new JMenuItem(hide);
	  fileMenu.add(menuItem);
	  menuItem.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent action) {
				theControlCheckBoxInTheMenu.setSelected(false);
				Constants.setConsoleVisible(false);
		  }
	  });
    setJMenuBar(menubar);
    pack();
    setTitle("Maud console");
 //   setSize(800, 600);
 //   Utility.putOnScreenAt(this, 70, 70);
//    setVisible(true);

		idealSize = 1000000; // it's still not available MaudPreferences.getInteger("console.maxNumberOfCharsToStore", 1000000);
		maxExcess = idealSize / 5;
    // Create reader threads
	  redirectSystemStreams();
//    new ReaderThread(piOut).start();
//    new ReaderThread(piErr).start();
  }

	private void redirectSystemStreams() {
		OutputStream out = new OutputStream() {
			@Override
			public void write(int b) throws IOException {
				updateTextArea(String.valueOf((char) b));
			}

			@Override
			public void write(byte[] b, int off, int len) throws IOException {
				updateTextArea(new String(b, off, len));
			}

			@Override
			public void write(byte[] b) throws IOException {
				write(b, 0, b.length);
			}
		};

		System.setOut(new PrintStream(out, true));
		System.setErr(new PrintStream(out, true));
	}

	private void updateTextArea(final String text) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				try {
					textArea.append(text);
					int length = textArea.getText().length();
//					textArea.setCaretPosition(length - 1);

					// Keep the text area down to a certain character size
					int excess = length - idealSize;
					if (excess >= maxExcess)
						textArea.replaceRange("", 0, excess);
				} catch (Exception e) {
					textArea.append("\nConsole reports an internal error.");
					textArea.append("The error is: " + e + "\n");
				}
				}
			});
	}

	public void setVisible(boolean visible) {
    if (Constants.consoleShouldBeVisible)
      super.setVisible(visible);
    else
      super.setVisible(false);
  }

  public void saveContentToFile() {
    String filename = Utility.browseFilenametoSave(this, "Save console output to...");
    BufferedWriter bo = Misc.getWriter(filename);
    if (bo != null) {
      try {
        bo.write(textArea.getText());
        bo.flush();
        bo.close();
      } catch (Exception e) {
        e.printStackTrace();
      }
    }
  }

  public static void setSelectionMenuItem(JCheckBoxMenuItem cb2) {
    theControlCheckBoxInTheMenu = cb2;
  }

/*  class ReaderThread extends Thread {
    PipedInputStream pi;

    ReaderThread(PipedInputStream pi) {
      this.pi = pi;
    }

    public void run() {
      final BufferedInputStream in = new BufferedInputStream(pi);
      try {
	      int idealSize = 1000000;
	      int maxExcess = 100000;
        int available;
        while ((available = in.available()) == 0) sleep(10);
        byte[] bytes;
        while (available > 0 && in.read(bytes = new byte[available], 0, available) != -1) {
          textArea.append(new String(bytes));
          textArea.setCaretPosition(textArea.toString().length());

          // Keep the text area down to a certain character size
          int excess = textArea.toString().length() - idealSize;
          if (excess >= maxExcess)
            textArea.replaceRange("", 0, excess);

          while ((available = in.available()) == 0) sleep(100);
        }
        in.close();
      } catch (Exception e) {
      }
    }
  }*/
}
