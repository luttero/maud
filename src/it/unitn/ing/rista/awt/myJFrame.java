/*
 * @(#)myJFrame.java created 01/01/1997 Mesiano
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.interfaces.basicObj;
import it.unitn.ing.rista.util.*;
import com.radiographema.Maud;

import javax.swing.*;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.datatransfer.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

/**
 * The myJFrame is a class to implement the basic interface feature for frames.
 *
 * @version $Revision: 1.16 $, $Date: 2006/01/19 14:45:53 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class myJFrame extends iconJFrame implements ParentFrame, ClipboardOwner, Printable {

  Color ligthgrayc = new Color(14777215);
  Color whitec = new Color(16777215);
  Color grayc = new Color(12632256);
  Frame theparent = null;
  Component hitcomponent = null;
  Vector componentVector;
  Vector parameterVector;
  Vector listenerVector;
  JButton HelpButtonB = null;
//	Vector parameterTFVector;
//	public Font defaultFont = new Font("Dialog", Font.PLAIN, 12);
  private String helpFilename = null;
  public Component componentToPrint = null;


  protected final Hashtable listTable = new Hashtable();
//	public static TextViewer helpWindow = null;

  public String frameWLabel = "Frame.frameWidth";
  public String frameHLabel = "Frame.frameHeight";
  public int defaultFrameW = 600;
  public int defaultFrameH = 400;
  public String framePositionX = "Frame.framePositionX";
  public String framePositionY = "Frame.framePositionY";
  public int defaultFramePositionX = 10;
  public int defaultFramePositionY = 20;

  public boolean setOwnSize = false;
  public boolean setOwnPosition = false;

  protected Container contentPaneWithStatusBar = null;
  JPanel statusBar = null;

  public myJFrame(Frame parent) {
    this(parent, false);
  }

  public myJFrame(Frame parent, boolean createStatusBar) {
    super();

    if (createStatusBar) {
      Container c1 = getContentPane();
      c1.setLayout(new BorderLayout());
      contentPaneWithStatusBar = new JPanel();
      c1.add(BorderLayout.CENTER, contentPaneWithStatusBar);
      statusBar = new JPanel();
      JLabel emptyL = new JLabel(" ");
      emptyL.setFont(new Font("Helvetica", Font.PLAIN, 3));
      statusBar.add(emptyL);
      c1.add(BorderLayout.SOUTH, statusBar);
    }

    setComponentToPrint(getContentPane());   // the default
    setFrameParent(parent);

    helpFilename = new String("null.txt");

    componentVector = new Vector(0, 1);
    parameterVector = new Vector(0, 1);
//		parameterTFVector = new Vector(0, 1);
    listenerVector = new Vector(0, 1);

//		setFont(defaultFont);

    setDefaultCloseOperation(DISPOSE_ON_CLOSE);

  }

  public Container getContentPane() {
    if (contentPaneWithStatusBar != null)
      return contentPaneWithStatusBar;
    else
      return super.getContentPane();
  }

  public Container getStatusBar() {
    return  statusBar;
  }

  public myJFrame(Frame parent, String titleframe) {
    this(parent);

    setTitle(titleframe);
  }

  public void setFrameParent(Frame parent) {
    theparent = parent;
  }

  public Frame getFrameParent() {
    return theparent;
  }

  public FilePar getFileParent() {
    principalJFrame aframe = getmainFrame();
    if (aframe != null)
      return aframe.getFileParent();
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

  public void showHelp() {
    showHelp(getHelpFilename());
  }

  public void showHelp(String filename) {

//		if (helpWindow == null)
    TextViewer helpWindow = new TextViewer(null);
//		else {
//			helpWindow.setVisible(false);
//			helpWindow.dispose();
//		}

	  URL helpfile = Misc.getResourceURL(Constants.helpJar, Constants.helpfolder + filename);
	  if (helpfile == null)
		  System.out.println("File not found: " + Constants.helpfolder + filename);
		else
      helpWindow.DisplayText(helpfile);

    helpWindow.setVisible(true);
  }

  /**
   * Create a JMenuBar for the frame containing some default commands
   */

  public JMenuBar createDefaultMenuBar() {
    return createDefaultMenuBar(false);
  }

  public JMenuBar createDefaultMenuBar(boolean editable) {

    JMenuBar menuBar = new JMenuBar();

    JMenu aMenu = menuBar.add(new JMenu("File"));
    aMenu.setMnemonic('o');

    if (editable) {
      JMenuItem printing = aMenu.add(new JMenuItem("Save as..."));
      printing.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
      printing.setMnemonic('s');
      printing.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          saveFile();
        }
      });
      aMenu.addSeparator();
    }

    JMenuItem printing = aMenu.add(new JMenuItem("Print..."));
    printing.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    printing.setMnemonic('p');
    printing.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        letsTryToPrint();
      }
    });
    aMenu.addSeparator();
    printing = aMenu.add(new JMenuItem("Reset size"));
    printing.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_R, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    printing.setMnemonic('r');
    printing.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setSize(defaultFrameW, defaultFrameH);
      }
    });
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
/*    editMenu.add(menuitem = new JMenuItem("Undo"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
      }
    });
    editMenu.add(new JSeparator());     */
    editMenu.add(menuitem = new JMenuItem("Cut"));
    menuitem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X,
        Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    menuitem.setMnemonic('x');
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        cutEvent(e);
      }
    });
    editMenu.add(menuitem = new JMenuItem("Copy"));
    menuitem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    menuitem.setMnemonic('c');
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        copyEvent(e);
      }
    });
    editMenu.add(menuitem = new JMenuItem("Paste"));
    menuitem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_V, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    menuitem.setMnemonic('v');
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        pasteEvent(e);
      }
    });

    return editMenu;
  }

  protected void pasteEvent(ActionEvent e) {
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
          Parameter xrdobj = ParameterFlavor.parameter;
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

  protected void copyEvent(ActionEvent e) {
    Clipboard clipboard = getToolkit().getSystemClipboard();
    Component comp = componentToPrint; //getFocusOwner();
//        System.out.println(comp);
    if (comp != null) {
      if (comp instanceof TextComponent) {
        StringSelection fieldContent =
                new StringSelection(((TextComponent) comp).getSelectedText());
        clipboard.setContents(fieldContent, myJFrame.this);
      } else if (comp instanceof JTextComponent) {
        StringSelection fieldContent =
                new StringSelection(((JTextComponent) comp).getSelectedText());
        clipboard.setContents(fieldContent, myJFrame.this);
      } else if (comp instanceof JList) {
        ListVector obj = getListData((JList) comp);
        if (obj != null) {
          Object selobj = obj.selectedElement();
          if (selobj instanceof XRDcat) {
            XRDcatFlavor xrdobj = new XRDcatFlavor(((XRDcat) selobj).getCopy(((XRDcat) selobj).getParent()));
            clipboard.setContents(xrdobj, myJFrame.this);
          } else if (selobj instanceof Parameter) {
            ParameterFlavor xrdobj = new ParameterFlavor(((Parameter) selobj).getCopy(((Parameter) selobj).getParent()));
            clipboard.setContents(xrdobj, myJFrame.this);
          }
        }
      } else if (comp instanceof CopyPrintPanelNoBkg) {
        Rectangle rect = comp.getBounds();
        Image fileImage =
                createImage(rect.width, rect.height);
        Graphics g = fileImage.getGraphics();

//write to the image

        ((CopyPrintPanelNoBkg) comp).clearComponent(g);
        g.setColor(Color.white);
        g.fillRect(0, 0, rect.width, rect.height);
        ((CopyPrintPanelNoBkg) comp).paintComponent(g, comp);

//        Color bkgcolor = comp.getBackground();
//        System.out.println(comp.getBackground());
//        comp.setBackground(Color.white);
//        comp.print(g);
        clipboard.setContents(new ClipImage(fileImage), myJFrame.this);
// write it out in the format you want

//dispose of the graphics content
        g.dispose();
//        System.out.println("Printing: " + comp.getBackground());
//        comp.setBackground(bkgcolor);
      } else if (comp instanceof CopyPrintPanel) {
        Rectangle rect = comp.getBounds();
        Image fileImage =
                createImage(rect.width, rect.height);
        Graphics g = fileImage.getGraphics();
	      System.out.println("Copy with background");

//write to the image

//        ((CopyPrintPanel) comp).clearComponent(g);
//        ((CopyPrintPanel) comp).paintComponent(g, comp);

//        Color bkgcolor = comp.getBackground();
//        System.out.println(comp.getBackground());
//        comp.setBackground(Color.white);
        comp.print(g);
        clipboard.setContents(new ClipImage(fileImage), myJFrame.this);
// write it out in the format you want

//dispose of the graphics content
        g.dispose();
//        System.out.println(comp.getBackground());
//        comp.setBackground(bkgcolor);
      } else if (comp instanceof Component) {
        Rectangle rect = comp.getBounds();
        Image fileImage =
                createImage(rect.width, rect.height);
        Graphics g = fileImage.getGraphics();

//write to the image

        g.clearRect(0, 0, comp.getWidth(), comp.getHeight());
//            clearComponent(g, comp);
//            paintComponent(g, comp);

        comp.print(g);
        clipboard.setContents(new ClipImage(fileImage), myJFrame.this);
// write it out in the format you want

//dispose of the graphics content
        g.dispose();
      }
    }
  }

  protected void cutEvent(ActionEvent e) {
    Clipboard clipboard = getToolkit().getSystemClipboard();
    Component comp = getFocusOwner();
    if (comp != null) {
      if (comp instanceof TextComponent) {
        TextComponent txtcomp = (TextComponent) comp;
        String tmpString = txtcomp.getSelectedText();
        StringSelection fieldContent = new StringSelection(tmpString);
        clipboard.setContents(fieldContent, myJFrame.this);
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
        clipboard.setContents(fieldContent, myJFrame.this);
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
            clipboard.setContents(xrdobj, myJFrame.this);
          } else if (selobj instanceof Parameter) {
            ParameterFlavor xrdobj = new ParameterFlavor((Parameter) selobj);
            clipboard.setContents(xrdobj, myJFrame.this);
          }
          obj.removeSelElement();
        }
      }
    }
  }

  public void saveFile() {
    // nothing by default
  }

  public static void clearComponent(Graphics g, Component comp){
    g.clearRect(0, 0, comp.getWidth(), comp.getHeight());
//    System.out.println(comp + " " + comp.getWidth() + " " + comp.getHeight());
    if (comp instanceof Container) {
      Component[] compList = ((Container) comp).getComponents();
      for (int i = 0; i < compList.length; i++) {
        clearComponent(g, compList[i]);
      }
    }
  }

  public static void paintComponent(Graphics g, Component comp){
    g.translate(comp.getX(), comp.getY());
    comp.paint(g);
    g.translate(-comp.getX(), -comp.getY());
//    System.out.println(comp + " " + comp.getWidth() + " " + comp.getHeight());
    if (comp instanceof Container) {
      Component[] compList = ((Container) comp).getComponents();
      for (int i = 0; i < compList.length; i++) {
        paintComponent(g, compList[i]);
      }
    }
  }

  public void lostOwnership(Clipboard parClipboard, Transferable parTransferable) {
// 	  System.out.println ("Lost ownership");
  }

  public JPopupMenu getPopupMenu(Parameter par) {
    final Parameter apar = par;
    int index = par.getStatusIndex();

    JPopupMenu popup = new JPopupMenu("Parameter status");

    JCheckBoxMenuItem mi = new JCheckBoxMenuItem("Fixed");
    if (index == 0)
      mi.setState(true);
    else
      mi.setState(false);
    mi.addItemListener(new java.awt.event.ItemListener() {
      public void itemStateChanged(java.awt.event.ItemEvent event) {
        setparameterNotrefinable(apar);
      }
    });
    popup.add(mi);
    mi = new JCheckBoxMenuItem("Refined");
    if (index == 1)
      mi.setState(true);
    else
      mi.setState(false);
    mi.addItemListener(new java.awt.event.ItemListener() {
      public void itemStateChanged(java.awt.event.ItemEvent event) {
        setparameterRefinable(apar);
      }
    });
    popup.add(mi);
    mi = new JCheckBoxMenuItem("Equal to..");
    if (index == 2 || index == 3)
      mi.setState(true);
    else
      mi.setState(false);
    mi.addItemListener(new java.awt.event.ItemListener() {
      public void itemStateChanged(java.awt.event.ItemEvent event) {
        setparameterEqualto(apar);
      }
    });
    popup.add(mi);

    getContentPane().add(popup);
    return popup;
  }

  /**
   * Set the component that will be printed by calling the default print method
   * (letsTryToPrint).
   * @param    component the component to print
   */

  public void setComponentToPrint(Component component) {
    componentToPrint = component;
//    System.out.println("Set for printing: " + componentToPrint);
  }

  /**
   * Send the content of the frame to the printer.
   */

  public void letsTryToPrint() {
    PrinterJob pjob = PrinterJob.getPrinterJob();
    if (pjob != null) {
      PageFormat graphicsPageFormat = new PageFormat();
      graphicsPageFormat = pjob.pageDialog(graphicsPageFormat);
      if (pjob.printDialog()) {
        graphicsPageFormat = pjob.validatePage(graphicsPageFormat);
        pjob.setPrintable(this, graphicsPageFormat);
        try {
          pjob.print();
        } catch (Exception PrintException) {
          PrintException.printStackTrace();
        }
      }
    }
  }

  public int print(Graphics graphics, PageFormat pageFormat, int pageIndex) /* throws PrinterException */ {
//		Graphics2D  g2 = (Graphics2D) graphics;
//    if (componentToPrint == null)
//      componentToPrint = getContentPane();
    if (pageIndex > 0)
      return NO_SUCH_PAGE;
//    System.out.println("Printing: " + componentToPrint);
    graphics.translate((int) pageFormat.getImageableX(), (int) pageFormat.getImageableY());
    if (componentToPrint instanceof CopyPrintPanel)
      ((CopyPrintPanel) componentToPrint).print(graphics, pageFormat);
    else
      componentToPrint.print(graphics);
    return PAGE_EXISTS;
  }

  public void addListData(JList alist, ListVector list) {
    listTable.put(alist, list);
  }

  public ListVector getListData(JList alist) {
    return (ListVector) listTable.get(alist);
  }

  public void addComponenttolist(Component comp, Parameter apar) {
	  if (apar == null || comp == null)
		  return;
	  if (componentVector.contains(comp)) {
		  int index = componentVector.indexOf(comp);
		  Parameter tmpPar = (Parameter) parameterVector.elementAt(index);
		  if (tmpPar != apar) {
			  tmpPar.setComponent(null);
			  if (comp instanceof JTextField)
				  ((JTextField) comp).setText(apar.getValue());
			  parameterVector.setElementAt(apar, index);
			  apar.setComponent(comp);
		  }
	  } else {
		  if (comp instanceof JTextField)
			  ((JTextField) comp).setText(apar.getValue());
		  parameterVector.addElement(apar);
		  componentVector.addElement(comp);
		  apar.setComponent(comp);
	  }
	  comp.setBackground(ligthgrayc);
	  comp.revalidate();
	  comp.repaint();
	  if (!listenerVector.contains(comp)) {
		  MyMouse amouselistener = new MyMouse();
		  comp.addMouseListener(amouselistener);
		  listenerVector.addElement(comp);
		  listenerVector.addElement(amouselistener);
	  }
	  comp.setBackground(ligthgrayc);
	  comp.revalidate();
	  comp.repaint();
//	  System.out.println("Magenta: " + comp.toString());
	  comp.setBackground(ligthgrayc);
  }

  public void removeComponentfromlist(Component comp) {
    if (componentVector != null && componentVector.contains(comp)) {
      int index = componentVector.indexOf(comp);
      Parameter tmpPar = (Parameter) parameterVector.elementAt(index);
      tmpPar.setComponent(null);
      parameterVector.removeElementAt(index);
      componentVector.removeElementAt(index);
      if (listenerVector.contains(comp)) {
        index = listenerVector.indexOf(comp);
        MyMouse amouselistener = (MyMouse) listenerVector.elementAt(index + 1);
        comp.removeMouseListener(amouselistener);
        listenerVector.removeElementAt(index + 1);
        listenerVector.removeElementAt(index);
      }
//	    System.out.println("White: " + comp.toString());
      comp.setBackground(whitec);
      comp.revalidate();
	    comp.repaint();
    }
  }

/*	public void setField(String label) {
		// to overwrite
	}*/

/*	public String getField() {
		return "";
	}*/

  public String getLabel() {
    return "";
  }

  public boolean isValidComponent(Component hit) {
    for (int i = 0; i < componentVector.size(); i++) {
      if (hit.equals(componentVector.elementAt(i)))
        return true;
    }
    return false;
  }

  public void updateFieldsChanged() {
    for (int i = 0; i < parameterVector.size(); i++) {
      String value = ((Parameter) parameterVector.elementAt(i)).getValue();
      ((JTextField) componentVector.elementAt(i)).setText(value);
    }
  }

  public Parameter getparameterfrom(Component hit) {
    int index = componentVector.indexOf(hit);
    if (index >= 0 || index < componentVector.size())
      return (Parameter) parameterVector.elementAt(index);
    return null;
  }

  public void setparameterNotrefinable(Parameter par) {
    if (par != null)
      par.setNotRefinable();
  }

  public void setparameterRefinable(Parameter par) {
    if (par != null)
      par.setRefinable();
  }

  public void setparameterEqualto(Parameter par) {
    if (par != null) {
      SetEqualtoD setequaldlg = new SetEqualtoD(this, true, par);
      setequaldlg.setVisible(true);
    }
  }

  public JPanel createEditParField(LayoutManager alayout, String label, Parameter thepar) {
    JPanel jTFpanel = new JPanel();
    jTFpanel.setLayout(alayout);
    jTFpanel.add(new JLabel(label));

    JTextField thetextfield = new JTextField(Constants.FLOAT_FIELD);
    thetextfield.setText(thepar.getValue());

    jTFpanel.add(thetextfield);
//		parameterTFVector.addElement(thetextfield);
//		parameterTFVector.addElement(thepar);
    addComponenttolist(thetextfield, thepar);

    return jTFpanel;
  }

  public void addParField(JPanel apanel, String label, Parameter thepar) {
    apanel.add(new JLabel(label));
    JTextField thetextfield = new JTextField(Constants.FLOAT_FIELD);
    thetextfield.setText(thepar.getValue());
    apanel.add(thetextfield);
//		parameterTFVector.addElement(thetextfield);
//		parameterTFVector.addElement(thepar);
    addComponenttolist(thetextfield, thepar);
  }

  public void initParameters() {
    for (int i = 0; i < parameterVector.size(); i++) {
      JTextField aTF = (JTextField) componentVector.elementAt(i);
      Parameter apar = (Parameter) parameterVector.elementAt(i);
      aTF.setText(apar.getValue());
    }
  }

  public void setHelpFilename(String name) {
    helpFilename = name;
    if ((HelpButtonB != null))
      HelpButtonB.setEnabled(true);
  }

  public void retrieveParameters() {
    for (int i = 0; i < parameterVector.size(); i++) {
      JTextField aTF = (JTextField) componentVector.elementAt(i);
      Parameter apar = (Parameter) parameterVector.elementAt(i);
      apar.setValue(aTF.getText());
    }

  }

  public String gettheLabel() {
    // to be overrided in subclasses
    return "Label:";
  }

  public void myJFrame_mousePressed(java.awt.event.MouseEvent event) {
//		int index;
    hitcomponent = event.getComponent();
    Parameter par = getparameterfrom(hitcomponent);
//		index = par.getStatusIndex();
    JPopupMenu popup = getPopupMenu(par);
    popup.show(hitcomponent, event.getX(), event.getY());
  }

  public void dispose() {
    if (componentVector != null) {
      for (int i = componentVector.size() - 1; i >= 0; i--)
        removeComponentfromlist((Component) componentVector.elementAt(i));
      componentVector.removeAllElements();
    }
    if (parameterVector != null)
      parameterVector.removeAllElements();
    if (listenerVector != null)
      listenerVector.removeAllElements();
//		if (parameterTFVector != null)
//			parameterTFVector.removeAllElements();
    componentVector = null;
    parameterVector = null;
    listenerVector = null;
//		parameterTFVector = null;
    Enumeration num = listTable.keys();
    while (num.hasMoreElements()) {
      Object key = num.nextElement();
      listTable.remove(key);
    }
    num = null;
    componentToPrint = null;
    Frame parent = getFrameParent();
    if (parent != null && parent instanceof myJFrame)
      ((myJFrame) parent).dispose(this);

//		if (Constants.testing)
//			System.out.println("DEBUG: frame " + this.getTitle() + " disposing");
    super.dispose();
  }

  public void dispose(myJFrame child) {
/*	  java.awt.EventQueue.invokeLater(new Runnable() {
		  @Override
		  public void run() {
			  toFront();
			  repaint();
		  }
	  });*/
  }

  public void resizeWindow() {
    setSize(getPreferredSize());
    validate();
  }

  class MyMouse extends java.awt.event.MouseAdapter {
    public void mousePressed(java.awt.event.MouseEvent event) {
      if (event.isPopupTrigger())
        showPopup(event);
    }

    public void mouseReleased(java.awt.event.MouseEvent event) {
      if (event.isPopupTrigger())
        showPopup(event);
    }

    private void showPopup(MouseEvent event) {
      Object object = event.getSource();
      for (int i = 0; i < componentVector.size(); i++)
        if (object == componentVector.elementAt(i))
          myJFrame_mousePressed(event);
    }
  }

  public void updateLookAndFeel() {
    invalidate();
    pack();
    Frame parentframe = getFrameParent();
    if (parentframe != null && parentframe instanceof myJFrame)
      ((myJFrame) parentframe).updateLookAndFeel();
    setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
  }

  public void setVisible(boolean show) {

    try {
    if (show && !isVisible()) {
      if (setOwnPosition) {

        int frameX = WindowPreferences.getInteger(framePositionX, defaultFramePositionX);
        int frameY = WindowPreferences.getInteger(framePositionY, defaultFramePositionY);

        Dimension screenSize = this.getToolkit().getScreenSize();
        if (screenSize.width - 5 < frameX)
          frameX = defaultFramePositionX;
        if (screenSize.height - 5 < frameY)
          frameY = defaultFramePositionY;

//	      System.out.println(this.toString());
//	      System.out.println("Setting location at: " + frameX + " " + frameY);
        setLocation(frameX, frameY);
      }
      if (setOwnSize) {
        int frameW = WindowPreferences.getInteger(frameWLabel, defaultFrameW);
        int frameH = WindowPreferences.getInteger(frameHLabel, defaultFrameH);
        setSize(frameW, frameH);
      }
    } else if (!show) {
      if (setOwnSize) {
        Dimension dimxy = getSize();
        WindowPreferences.setPref(frameWLabel, dimxy.width);
        WindowPreferences.setPref(frameHLabel, dimxy.height);
      }
      if (setOwnPosition) {
        Point xy = getLocation();
        WindowPreferences.setPref(framePositionX, xy.x);
        WindowPreferences.setPref(framePositionY, xy.y);
      }
    }
/*    if (Constants.macosx && show == false &&
        (Maud.appMainFrame != null && this.getDefaultCloseOperation() == DISPOSE_ON_CLOSE)) {
      if (getJMenuBar() != null) {
        setJMenuBar(null);
        Maud.appMainFrame.requestFocus();
        requestFocus();
 //       System.out.println("JMenuBar workaround....");
      }
    }*/
    } catch(Exception e) {}
    super.setVisible(show);
  }

  public void initializeSizeAndPosition(boolean ownSize,
                                        String frameWL, String frameHL,
                                        int frameW, int frameH,
                                        boolean ownPosition,
                                        String framePosX, String framePosY,
                                        int frameX, int frameY) {
    setOwnSize = ownSize;
    if (setOwnSize) {
      frameWLabel = frameWL;
      frameHLabel = frameHL;
      defaultFrameW = frameW;
      defaultFrameH = frameH;
    }
    setOwnPosition = ownPosition;
    if (setOwnPosition) {
      framePositionX = framePosX;
      framePositionY = framePosY;
      defaultFramePositionX = frameX;
      defaultFramePositionY = frameY;
    }
  }

  public Dimension getMinimumSize() {
//      return new Dimension(4, 4);
    return new Dimension(defaultFrameW, defaultFrameH);
  }

  protected void finalize() throws Throwable {
//    if (Constants.testing)
//      System.out.println("DEBUG: frame " + this.getTitle() + " finalizing");

    super.finalize();

  }

  public static void prepareForDisposal(JFrame jframe) {
/*    if (!Constants.macosx || jframe.getJMenuBar() == null)
      return;
    jframe.setJMenuBar(null);
//    System.out.println("JMenuBar workaround called");
    if (Maud.appMainFrame != null)
      Maud.appMainFrame.requestFocus();
    jframe.requestFocus();*/
  }

}

