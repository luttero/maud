/*
 * @(#)TextViewer.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.awt;

import java.awt.*;
import java.awt.print.PrinterJob;
import java.awt.print.PageFormat;
import java.awt.event.*;
import java.net.*;
import java.io.*;
import java.util.Properties;
import javax.swing.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.io.COD.CODCIFsubmitter;

/**
 * The TextViewer is a class
 *
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:53 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class TextViewer extends myJFrame {

  JTextArea textArea = null;
  JScrollPane scrollarea = null;

  public TextViewer(Frame parent) {

    this(parent, false);
  }

  public TextViewer(Frame parent, boolean editable) {

    super(parent);

    frameWLabel = "helpFrame.frameWidth";
    frameHLabel = "helpFrame.frameHeight";
    defaultFrameW = 600;
    defaultFrameH = 400;
    setOwnSize = true;
    framePositionX = "helpFrame.framePositionX";
    framePositionY = "helpFrame.framePositionY";
    defaultFramePositionX = 10;
    defaultFramePositionY = 20;
    setOwnPosition = true;

    createDefaultMenuBar(editable);

    scrollarea = new JScrollPane(textArea = new JTextArea());
    if (editable) {
      getContentPane().setLayout(new BorderLayout(3, 3));
      getContentPane().add(BorderLayout.CENTER, scrollarea);
    } else
      getContentPane().add(scrollarea);

    textArea.setEditable(editable);

    setTitle("");

//    setComponentToPrint(textArea);
  }

  public TextViewer() {
    this(null);
  }

  public TextViewer(Frame parent, boolean editable, boolean forCOD) {
    this(parent, editable);
    if (forCOD) {
      setTitle("CIF submission to COD");
      JPanel jp1 = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      getContentPane().add(BorderLayout.NORTH, jp1);
      jp1.add(new JLabel("Revise and edit the CIF before submitting to Crystallographic Open Database"));
      jp1 = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      getContentPane().add(BorderLayout.SOUTH, jp1);
      JButton abutton = new JButton("Cancel");
      abutton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setVisible(false);
          dispose();
        }
      });
      jp1.add(abutton);
      abutton = new JButton("Submit to COD");
      abutton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          saveFile();
          if (getHelpFilename() != null) {
            System.out.println("Submitting file: " + getHelpFilename() + " to COD.....");
            CODCIFsubmitter.submit(getHelpFilename());
          }
          setVisible(false);
          dispose();
        }
      });
      jp1.add(abutton);
    }
  }

  public void DisplayText(URL filename) {
//    setTitle("Help");
    BufferedReader data;
    String Line; // file data
    StringBuffer Buf = new StringBuffer();

    try {
      data = new BufferedReader(new InputStreamReader(filename.openStream()));

      while ((Line = data.readLine()) != null)
        Buf.append(Line + "\n");
      data.close();

      textArea.setText(Buf.toString());
      JScrollBar scrollBar = scrollarea.getVerticalScrollBar();
//      if (scrollBar != null)
//        scrollBar.setValue(0);
    } catch (IOException e) {
      setVisible(false);
    }
  }

  public void DisplayText(String filename) {
//    setTitle("Help");
    BufferedReader data;
    String Line; // file data
    StringBuffer Buf = new StringBuffer();

    try {
      data = Misc.getReader(filename);
      setHelpFilename(filename);

      while ((Line = data.readLine()) != null)
        Buf.append(Line + "\n");
      data.close();

      textArea.setText(Buf.toString());
	    if (scrollarea.getVerticalScrollBar() != null)
		    javax.swing.SwingUtilities.invokeLater(new Runnable() {
			    public void run() {
				    //scrollarea.getVerticalScrollBar().setValue(0);
			    }
		    });
    } catch (IOException e) {
      setVisible(false);
    }
  }

  public void DisplayText(URL acodeBase, String filename) {
    setTitle(filename);
    BufferedReader data;
    String Line; // file data
    StringBuffer Buf = new StringBuffer();

    try {
      URL url = new URL(acodeBase, filename);
      data = new BufferedReader(new InputStreamReader(url.openStream()));

      while ((Line = data.readLine()) != null)
        Buf.append(Line + "\n");
      data.close();

      textArea.setText(Buf.toString());
      JScrollBar scrollBar = scrollarea.getVerticalScrollBar();
      if (scrollBar != null)
        scrollBar.setValue(0);
    } catch (IOException e) {
      setVisible(false);
    }
  }

  public void saveFile() {
    if (getHelpFilename() == null)
      return;

    BufferedWriter data;

    try {
      data = Misc.getWriter(getHelpFilename());

      data.write(textArea.getText());

      data.close();

    } catch (IOException e) {
      setVisible(false);
    }
  }

  public void letsTryToPrint() {
    PrintJob pjob = getToolkit().getPrintJob(this, "Printing text", new Properties());
//    PrinterJob pjob = PrinterJob.getPrinterJob();
    if (pjob != null) {
//      PageFormat graphicsPageFormat = new PageFormat();
//      graphicsPageFormat = pjob.pageDialog(graphicsPageFormat);
//      if (pjob.printDialog()) {
//        graphicsPageFormat = pjob.validatePage(graphicsPageFormat);
//        pjob.setPrintable(this, graphicsPageFormat);
        try {
          Graphics pg = pjob.getGraphics();
          if (pg != null) {
            String s = textArea.getText();
            printLongString (pjob, pg, s);
            pg.dispose();
          }
          pjob.end();
        } catch (Exception PrintException) {
          PrintException.printStackTrace();
        }
 //     }
    }
  }

  // I'm assuming a one-inch margin on all
  // four sides. This could be done better.
  private int margin = 72;

  // Print string to graphics via printjob
  // Does not deal with word wrap or tabs
  private void printLongString (PrintJob pjob, Graphics pg, String s) {

    int pageNum = 1;
    int linesForThisPage = 0;
    int linesForThisJob = 0;
    // Note: String is immutable so won't change while printing.
    if (!(pg instanceof PrintGraphics)) {
      throw new IllegalArgumentException ("Graphics context not PrintGraphics");
    }
    StringReader sr = new StringReader (s);
    LineNumberReader lnr = new LineNumberReader (sr);
    String nextLine;
    int pageHeight = pjob.getPageDimension().height - margin;
    Font helv = new Font("Helvetica", Font.PLAIN, 12);
    //have to set the font to get any output
    pg.setFont (helv);
    FontMetrics fm = pg.getFontMetrics(helv);
    int fontHeight = fm.getHeight();
    int fontDescent = fm.getDescent();
    int curHeight = margin;
    try {
      do {
        nextLine = lnr.readLine();
        if (nextLine != null) {
          if ((curHeight + fontHeight) > pageHeight) {
            // New Page
            System.out.println ("" + linesForThisPage + " lines printed for page " + pageNum);
            if (linesForThisPage == 0) {
               System.out.println ("Font is too big for pages of this size; aborting...");
               break;
            }
            pageNum++;
            linesForThisPage = 0;
            pg.dispose();
            pg = pjob.getGraphics();
            if (pg != null) {
              pg.setFont (helv);
            }
            curHeight = 0;
          }
          curHeight += fontHeight;
          if (pg != null) {
            pg.drawString (nextLine, margin, curHeight - fontDescent);
            linesForThisPage++;

            linesForThisJob++;
          } else {
            System.out.println ("pg null");
          }
        }
      } while (nextLine != null);
    } catch (EOFException eof) {
      // Fine, ignore
    } catch (Throwable t) { // Anything else
      t.printStackTrace();
    }
    System.out.println ("" + linesForThisPage + " lines printed for page " + pageNum);
    System.out.println ("pages printed: " + pageNum);
    System.out.println ("total lines printed: " + linesForThisJob);
  }

}

