package it.unitn.ing.esqui.client;

import it.unitn.ing.rista.util.Misc;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import javax.swing.text.html.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.net.*;

public class HtmlEditor extends JPanel {

  JEditorPane htmlEditor;
  JScrollPane scroller;
  JToolBar toolBar;
  Vector urlHistory = new Vector(0, 1);
  URL homeUrl;
  URL presentUrl;
  int historyIndex = 0;
  int maxHistoryIndex = 0;

/*    public static void main(String[] args) {
	    JFrame frame = new JFrame("HtmlViewer");
			try {
	    	HtmlEditor html = new HtmlEditor(new URL(args[0]));
	    	frame.getContentPane().add(html);
		    frame.addWindowListener(new WindowAdapter() {
			    public void windowClosing(WindowEvent e) { System.exit(0);}
			    public void windowClosed(WindowEvent e) { System.exit(0);}
	 	   });
	 	  	frame.pack();
   		 	frame.setSize(550, 500);
	 	   	frame.setVisible(true);
	    } catch (Exception exc) {
	    }
    }*/

  public HtmlEditor(URL url) {
    try {
      htmlEditor = new JEditorPane(url);
      htmlEditor.setEditable(false);
      htmlEditor.addHyperlinkListener(createHyperlinkListener());
      scroller = new JScrollPane(htmlEditor);
      addUrlToHistory(url);
      homeUrl = url;
      presentUrl = url;

      toolBar = new JToolBar();
      Action back = new AbstractAction("Back", ClientMisc.getImage("Back.gif")) {
        public void actionPerformed(ActionEvent e) {
          try {
            int tmpIndex = getHistoryIndex();
            if (tmpIndex == 0)
              return;
            htmlEditor.setPage(getUrlFromHistory(tmpIndex - 1));
            setHistoryIndex(tmpIndex - 1);
            checkNavigationButtons(tmpIndex - 1);
          } catch (Exception exc) {
            System.out.println("Error on action 'back'...");
          }
        }
      };
      Action forward = new AbstractAction("Forward", ClientMisc.getImage("Forward.gif")) {
        public void actionPerformed(ActionEvent e) {
          try {
            int tmpIndex = getHistoryIndex();
            if (tmpIndex == maxHistoryIndex)
              return;
            htmlEditor.setPage(getUrlFromHistory(tmpIndex + 1));
            setHistoryIndex(tmpIndex + 1);
            checkNavigationButtons(tmpIndex + 1);
          } catch (Exception exc) {
            System.out.println("Error on action 'forward'...");
          }
        }
      };
      Action refresh = new AbstractAction("Refresh", ClientMisc.getImage("Refresh.gif")) {
        public void actionPerformed(ActionEvent e) {
          try {
            URL tmp = htmlEditor.getPage();
            htmlEditor.setPage(tmp);
          } catch (Exception exc) {
            System.out.println("Error on action 'refresh'...");
          }
        }
      };
      Action home = new AbstractAction("Home", ClientMisc.getImage("Home.gif")) {
        public void actionPerformed(ActionEvent e) {
          try {
            htmlEditor.setPage(homeUrl);
            checkNavigationButtons(0);
          } catch (Exception exc) {
            System.out.println("Error on action 'go to index'...");
          }
        }
      };
      Action exit = new AbstractAction(null, ClientMisc.getImage("Exit.gif")) {
        public void actionPerformed(ActionEvent e) {
          ((JFrame) getTopLevelAncestor()).dispose();
        }
      };
      JButton button = toolBar.add(back);
      button.setText("");
      button.setToolTipText("Back");
      button.setEnabled(false);
      button = toolBar.add(forward);
      button.setText("");
      button.setToolTipText("Forward");
      button.setEnabled(false);
      button = toolBar.add(refresh);
      button.setText("");
      button.setToolTipText("Refresh");
      button = toolBar.add(home);
      button.setText("");
      button.setToolTipText("Index");
      setLayout(new BorderLayout());
      add(scroller, BorderLayout.CENTER);
      add(toolBar, BorderLayout.NORTH);
      JPanel exitPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
      exitPanel.add(new JButton(exit));
      add(exitPanel, BorderLayout.SOUTH);
    } catch (Exception exc) {
    }

  }

  public HyperlinkListener createHyperlinkListener() {
    return new HyperlinkListener() {
      public void hyperlinkUpdate(HyperlinkEvent e) {
        if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
          if (e instanceof HTMLFrameHyperlinkEvent) {
            ((HTMLDocument) htmlEditor.getDocument()).processHTMLFrameHyperlinkEvent((HTMLFrameHyperlinkEvent) e);
          } else {
            try {
              htmlEditor.setPage(e.getURL());
              addUrlToHistory(e.getURL());
              maxHistoryIndex = getHistoryIndex();
              checkNavigationButtons(maxHistoryIndex);
            } catch (Exception exc) {
              System.out.println("Error on action 'hyperlink'...");
            }
          }
        }
      }
    };
  }

  void addUrlToHistory(URL url) {
    urlHistory.addElement(url);
    setHistoryIndex(urlHistory.size() - 1);
  }

  URL getUrlFromHistory(int index) {
    return (URL) urlHistory.elementAt(index);
  }

  int getHistoryIndex() {
    return historyIndex;
  }

  void setHistoryIndex(int index) {
    historyIndex = index;
  }

  void checkNavigationButtons(int index) {
    if (index == 0)
      toolBar.getComponentAtIndex(0).setEnabled(false);
    if (index == 1)
      toolBar.getComponentAtIndex(0).setEnabled(true);
    if (index == maxHistoryIndex)
      toolBar.getComponentAtIndex(1).setEnabled(false);
    if (index == (maxHistoryIndex - 1))
      toolBar.getComponentAtIndex(1).setEnabled(true);
  }
}
