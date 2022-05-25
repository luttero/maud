/*
 * @(#)myJDialog.java created 1/01/1997 ?
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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
import javax.swing.*;
import javax.swing.text.*;
import java.awt.event.*;
import java.net.*;

import it.unitn.ing.rista.io.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.*;

import java.util.*;
import java.awt.datatransfer.*;

import it.unitn.ing.rista.interfaces.*;

/**
 * The myJDialog is a class that extend the basic JDialog to provide additional
 * features like closing box, a default menuBar if needed, etc.
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:53 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class myJDialog extends JDialog implements ParentFrame, ClipboardOwner {

  protected String helpFilename;
  protected JMenuBar menuBar = null;
  protected Component componentToPrint = null;
  protected final Hashtable listTable = new Hashtable();
  JButton HelpButtonB = null;

  public myJDialog(Frame parent, boolean modal) {

    super(parent, modal);

    helpFilename = new String("null.txt");

    setDefaultCloseOperation(DISPOSE_ON_CLOSE);
//		setIconImage( getProgramIcon() );

  }

  public myJDialog(Frame parent, String title, boolean modal) {
    this(parent, modal);
    setTitle(title);
  }

  public Image getProgramIcon() {
    return Toolkit.getDefaultToolkit().getImage(Misc.getResourceURL(Constants.imagesJar,
		    Constants.imagefolder + Constants.programIcon));
  }

  public Frame getFrameParent() {
    return (Frame) getParent();
  }

  public FilePar getFileParent() {
    Frame aparent = getFrameParent();
    while (aparent != null && !(aparent instanceof principalJFrame)) {
      aparent = ((ParentFrame) aparent).getFrameParent();
    }
    if (aparent != null)
      return ((ParentFrame) aparent).getFileParent();
    else
      return null;
  }

  public principalJFrame getmainFrame() {
    Frame aparent = getFrameParent();
    while (aparent != null && !(aparent instanceof principalJFrame)) {
      aparent = ((ParentFrame) aparent).getFrameParent();
    }
    if (aparent != null)
      return (principalJFrame) aparent;
    else
      return null;
  }

  public void centerOnScreen() {
    Dimension paneSize = getSize();
    Dimension screenSize = getToolkit().getScreenSize();

    setLocation((screenSize.width - paneSize.width) / 2,
            (screenSize.height - paneSize.height) / 2);
  }

  public void setHelpButton(Container comp) {
    HelpButtonB = new JHelpButton();
    comp.add(HelpButtonB);
    HelpButtonB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        showHelp();
      }
    });
    if (getHelpFilename().equalsIgnoreCase("null.txt"))
      HelpButtonB.setEnabled(false);
  }

  public String getHelpFilename() {
    return helpFilename;
  }

  public void setHelpFilename(String name) {
    helpFilename = name;
    if ((HelpButtonB != null))
      HelpButtonB.setEnabled(true);
  }

  public void showHelp() {

    TextViewer helpWindow = new TextViewer(null);

    URL helpfile = Misc.getResourceURL(Constants.helpJar, Constants.helpfolder + getHelpFilename());
	  if (helpfile != null) {
	    helpWindow.DisplayText(helpfile);
      helpWindow.setVisible(true);
	  }
  }

  /**
   * Create a JMenuBar for the frame containing some default commands
   */

  public JMenuBar createDefaultMenuBar() {
    JMenuBar menuBar = new JMenuBar();
    JMenu aMenu = menuBar.add(new JMenu("Options"));
    aMenu.setMnemonic('o');
    JMenuItem printing = aMenu.add(new JMenuItem("Printing"));
    printing.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    printing.setMnemonic('p');
    printing.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        letsTryToPrint();
      }
    });
    aMenu.addSeparator();
    printing = aMenu.add(new JMenuItem("Close"));
    printing.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    printing.setMnemonic('q');
    printing.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setVisible(false);
        dispose();
      }
    });
    menuBar.add(createEditMenu());
    setJMenuBar(menuBar);
    return menuBar;
  }

  public JMenu createEditMenu() {

    JMenuItem menuitem = null;

    JMenu editMenu = new JMenu("Edit");
    editMenu.setMnemonic('e');
    editMenu.add(menuitem = new JMenuItem("Undo"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
      }
    });
    editMenu.addSeparator();
    editMenu.add(menuitem = new JMenuItem("Cut"));
    menuitem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    menuitem.setMnemonic('x');
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        Clipboard clipboard = getToolkit().getSystemClipboard();
        Component comp = getFocusOwner();
        if (comp != null) {
          if (comp instanceof TextComponent) {
            TextComponent txtcomp = (TextComponent) comp;
            String tmpString = txtcomp.getSelectedText();
            StringSelection fieldContent = new StringSelection(tmpString);
            clipboard.setContents(fieldContent, myJDialog.this);
            if (tmpString != null && txtcomp.isEditable()) {
              String oldString = txtcomp.getText();
              String newString = new String(oldString.substring(0, txtcomp.getSelectionStart())
                      + oldString.substring(txtcomp.getSelectionEnd()));
              txtcomp.setText(newString);
            }
          } else if (comp instanceof JTextComponent) {
            JTextComponent txtcomp = (JTextComponent) comp;
            String tmpString = txtcomp.getSelectedText();
            StringSelection fieldContent = new StringSelection(tmpString);
            clipboard.setContents(fieldContent, myJDialog.this);
            if (tmpString != null && txtcomp.isEditable()) {
              String oldString = txtcomp.getText();
              String newString = new String(oldString.substring(0, txtcomp.getSelectionStart())
                      + oldString.substring(txtcomp.getSelectionEnd()));
              txtcomp.setText(newString);
            }
          } else if (comp instanceof JList) {
            ListVector obj = getListData((JList) comp);
            if (obj != null) {
              Object selobj = obj.selectedElement();
              if (selobj instanceof XRDcat) {
                XRDcatFlavor xrdobj = new XRDcatFlavor((XRDcat) selobj);
                clipboard.setContents(xrdobj, myJDialog.this);
              } else if (selobj instanceof Parameter) {
                ParameterFlavor xrdobj = new ParameterFlavor((Parameter) selobj);
                clipboard.setContents(xrdobj, myJDialog.this);
              }
              obj.removeSelElement();
            }
          }
        }

      }
    });
    editMenu.add(menuitem = new JMenuItem("Copy"));
    menuitem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    menuitem.setMnemonic('c');
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        Clipboard clipboard = getToolkit().getSystemClipboard();
        Component comp = getFocusOwner();
        if (comp != null) {
          if (comp instanceof TextComponent) {
            StringSelection fieldContent =
                    new StringSelection(((TextComponent) comp).getSelectedText());
            clipboard.setContents(fieldContent, myJDialog.this);
          } else if (comp instanceof JTextComponent) {
            StringSelection fieldContent =
                    new StringSelection(((JTextComponent) comp).getSelectedText());
            clipboard.setContents(fieldContent, myJDialog.this);
          } else if (comp instanceof JList) {
            ListVector obj = getListData((JList) comp);
            if (obj != null) {
              Object selobj = obj.selectedElement();
              if (selobj instanceof XRDcat) {
                XRDcatFlavor xrdobj = new XRDcatFlavor(((XRDcat) selobj).getCopy(((XRDcat) selobj).getParent()));
                clipboard.setContents(xrdobj, myJDialog.this);
              } else if (selobj instanceof Parameter) {
                ParameterFlavor xrdobj = new ParameterFlavor(((Parameter) selobj).getCopy(((Parameter) selobj).getParent()));
                clipboard.setContents(xrdobj, myJDialog.this);
              }
            }
          }
        }
      }
    });
    editMenu.add(menuitem = new JMenuItem("Paste"));
    menuitem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_V, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    menuitem.setMnemonic('v');
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        Clipboard clipboard = getToolkit().getSystemClipboard();
        Transferable clipboardContent = clipboard.getContents(this);
// 		System.out.println(clipboardContent);

        if (clipboardContent != null) {
          if (clipboardContent.isDataFlavorSupported(DataFlavor.stringFlavor)) {
            try {
              String tempString;
              tempString = (String) clipboardContent.getTransferData(DataFlavor.stringFlavor);
              if (tempString != null && !tempString.equals("")) {
                Component comp = getFocusOwner();
                if (comp != null) {
                  if (comp instanceof TextComponent) {
                    TextComponent txtcomp = (TextComponent) comp;
                    String tmpString = txtcomp.getSelectedText();
                    if (tmpString != null && txtcomp.isEditable()) {
                      String oldString = txtcomp.getText();
                      String newString = new String(oldString.substring(0, txtcomp.getSelectionStart())
                              + tempString
                              + oldString.substring(txtcomp.getSelectionEnd()));
                      txtcomp.setText(newString);
                    } else {
                      int insertionindex = txtcomp.getCaretPosition();
                      String oldString = txtcomp.getText();
                      String newString = new String(oldString.substring(0, insertionindex)
                              + tempString
                              + oldString.substring(insertionindex));
                      txtcomp.setText(newString);
                    }
                  } else if (comp instanceof JTextComponent) {
                    JTextComponent txtcomp = (JTextComponent) comp;
                    String tmpString = txtcomp.getSelectedText();
                    if (tmpString != null && txtcomp.isEditable()) {
                      String oldString = txtcomp.getText();
                      String newString = new String(oldString.substring(0, txtcomp.getSelectionStart())
                              + tempString
                              + oldString.substring(txtcomp.getSelectionEnd()));
                      txtcomp.setText(newString);
                    } else {
                      int insertionindex = txtcomp.getCaretPosition();
                      String oldString = txtcomp.getText();
                      String newString = new String(oldString.substring(0, insertionindex)
                              + tempString
                              + oldString.substring(insertionindex));
                      txtcomp.setText(newString);
                    }
                  }
                }

              }
            } catch (Exception ce) {
              ce.printStackTrace();
            }
          } else if (clipboardContent.isDataFlavorSupported(XRDcatFlavor.xrdFlavor)) {
            try {
// 	          XRDcat xrdobj = (XRDcat) clipboardContent.getTransferData(XRDcatFlavor.xrdFlavor);
              basicObj xrdobj = XRDcatFlavor.xrdObj;
              if (xrdobj != null) {
                Component comp = getFocusOwner();
                if (comp != null)
                  if (comp instanceof JList) {
                    ListVector obj = getListData((JList) comp);
                    if (obj != null && obj.getParent().isObjectSupported(xrdobj, obj)) {
                      xrdobj.setParent(obj.getParent());
                      obj.addItem(xrdobj);
                    }
                  }

              }
            } catch (Exception ce) {
              ce.printStackTrace();
            }
          } else if (clipboardContent.isDataFlavorSupported(ParameterFlavor.parameterFlavor)) {
            try {
// 	          XRDcat xrdobj = (XRDcat) clipboardContent.getTransferData(ParameterFlavor.parameterFlavor);
              Parameter xrdobj = (Parameter) ParameterFlavor.parameter;
              if (xrdobj != null) {
                Component comp = getFocusOwner();
                if (comp != null)
                  if (comp instanceof JList) {
                    ListVector obj = getListData((JList) comp);
                    if (obj != null && obj.getParent().isObjectSupported(obj)) {
                      xrdobj.setParent(obj.getParent());
                      obj.addItem(xrdobj);
                    }
                  }

              }
            } catch (Exception ce) {
              ce.printStackTrace();
            }
          }
        }
      }
    });

    return editMenu;
  }

  public void lostOwnership(Clipboard parClipboard, Transferable parTransferable) {
// 	  System.out.println ("Lost ownership");
  }

  /**
   * Set the component that will be printed by calling the default print method
   * (letsTryToPrint).
   * @param    component the component to print
   */

  public void setComponentToPrint(Component component) {
    componentToPrint = component;
  }

  /**
   * Send the content of the frame to the printer.
   */

  public void letsTryToPrint() {
//  	if (componentToPrint == null)
//  		componentToPrint = this;
/*    PrintJob pjob = getToolkit().getPrintJob(this, "Printing", null);
    if (pjob != null) {
      Graphics pg = pjob.getGraphics();

      if (pg != null) {
        printAll(pg);
        pg.dispose(); // flush page
      }
      pjob.end();
    }*/
  }

  public void addListData(JList alist, ListVector list) {
    listTable.put(alist, list);
  }

  public ListVector getListData(JList alist) {
    return (ListVector) listTable.get(alist);
  }

  public void updateLookAndFeel() {
    invalidate();
    pack();
    Frame parentframe = getFrameParent();
    if (parentframe != null && parentframe instanceof myJFrame)
      ((myJFrame) parentframe).updateLookAndFeel();
    setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
  }

  protected void finalize() throws Throwable {
//		if (Constants.testing)
//			System.out.println("DEBUG: dialog " + this.getTitle() + " finalizing");

    super.finalize();

  }

  static final String title = "Program registration";
  static final String label0 = "Insert the license and password ID's to register MAUD";
  static final String label2 = "License ID: ";
  static final String label3 = "Program ID: ";
  static final String label4 = "Password: ";

}
