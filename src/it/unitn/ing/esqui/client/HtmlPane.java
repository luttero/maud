package it.unitn.ing.esqui.client;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import javax.swing.text.html.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;

public class HtmlPane extends JPanel {

  JEditorPane htmlEditor;
  JScrollPane scroller;
  URL backUrl;
  URL homeUrl;
  URL presentUrl;

  public static void main(String[] args) {
    JFrame frame = new JFrame("HtmlViewer");
    try {
      HtmlPane html = new HtmlPane(new URL(args[0]));
      frame.getContentPane().add(html);
      frame.addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent e) {
          //Luca System.exit(0);
        }

        public void windowClosed(WindowEvent e) {
          //Luca System.exit(0);
        }
      });
      frame.pack();
      frame.setSize(550, 500);
      frame.setVisible(true);
    } catch (Exception exc) {
    }
  }

  public HtmlPane(URL url) {
    try {
      htmlEditor = new JEditorPane(url);
      htmlEditor.setEditable(false);
      htmlEditor.addHyperlinkListener(createHyperlinkListener());
      scroller = new JScrollPane(htmlEditor);
      homeUrl = url;
      backUrl = url;
      presentUrl = url;

      JToolBar toolBar = new JToolBar();
      Action back = new AbstractAction("Back", ClientMisc.getImage("Exit.gif")) {
        public void actionPerformed(ActionEvent e) {
          try {
            htmlEditor.setPage(backUrl);
          } catch (IOException ioe) {
            System.err.println("Error on action back...");
          }
        }
      };
      Action refresh = new AbstractAction("Refresh", ClientMisc.getImage("Exit.gif")) {
        public void actionPerformed(ActionEvent e) {
          try {
            URL tmp = htmlEditor.getPage();
            htmlEditor.setPage(tmp);
          } catch (IOException ioe) {
            System.err.println("Error on action back...");
          }
        }
      };
      Action home = new AbstractAction("Home", ClientMisc.getImage("Exit.gif")) {
        public void actionPerformed(ActionEvent e) {
          try {
            htmlEditor.setPage(homeUrl);
          } catch (IOException ioe) {
            System.err.println("Error on action back...");
          }
        }
      };
      Action exit = new AbstractAction("Exit", ClientMisc.getImage("Exit.gif")) {
        public void actionPerformed(ActionEvent e) {
          ((JFrame) getTopLevelAncestor()).dispose();
        }
      };


      JButton button = toolBar.add(back);
      button.setText("");
      button.setToolTipText("Go back");
      button = toolBar.add(refresh);
      button.setText("");
      button.setToolTipText("Refresh");
      button = toolBar.add(home);
      button.setText("");
      button.setToolTipText("To start");
      button = toolBar.add(exit);
      button.setText("");
      button.setToolTipText("Close this browser");
      setLayout(new BorderLayout());
      add(scroller, BorderLayout.CENTER);
      add(toolBar, BorderLayout.NORTH);
    } catch (Exception e) {
    }

  }

  public HyperlinkListener createHyperlinkListener() {
    backUrl = htmlEditor.getPage();
    return new HyperlinkListener() {
      public void hyperlinkUpdate(HyperlinkEvent e) {
        if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
          if (e instanceof HTMLFrameHyperlinkEvent) {
            ((HTMLDocument) htmlEditor.getDocument()).processHTMLFrameHyperlinkEvent((HTMLFrameHyperlinkEvent) e);
          } else {
            try {
              htmlEditor.setPage(e.getURL());
            } catch (IOException ioexc) {
              System.err.println("*** Error: " + ioexc);
            }
          }
        }
      }
    };
  }
}
