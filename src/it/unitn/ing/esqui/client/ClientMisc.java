package it.unitn.ing.esqui.client;

import it.unitn.ing.rista.util.Misc;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import java.text.*;
import java.util.*;
import java.net.*;
import java.io.*;

/** ClientMisc.java
 * <br>
 * Title:			<b>ESQUI Client Miscellaneous</b>
 * </br>
 * Description:		Miscellaneous methods for the ESQUI client
 * @author:			Leonardo Cont, December 2000
 * @revision:		December 2000
 * @comment:		None
 */

public class ClientMisc {

  public static final String imagefolder = new String("images/");
  public static final String iconfolder = new String(imagefolder + "20x20/");
  public final static String ENTER_BUTTON = "Enter.gif";
  public final static String EXIT_BUTTON = "Exit.gif";

  public static final int X_WINDOW_RELPOS = 50;

  private ClientMisc() {
  }

/*
	Create a pop-up message box
*/
  public static void messageBox(Component parent, String title, String message, int type) {
    JOptionPane.showMessageDialog(parent, message, title, type);
  }


/*
	Method to locate frames on the screen.
*/

  public static final void locateOnScreen(Window window, int xperc, int yperc) {
    Dimension windowSize = window.getSize();
    Dimension screenSize = window.getToolkit().getScreenSize();

    window.setLocation((screenSize.width - windowSize.width) * xperc / 100,
            (screenSize.height - windowSize.height) * yperc / 100);
  }

/*
	Method to return a date in MEDIUM format
*/
  public static final String todayIs() {
    return DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.MEDIUM).format(new Date());
  }

/*
	Method to get a resource from a JAR archive
*/
  public static URL getResource(String resourcePath) {
    Object obj = new Object();
    if (obj != null) {
      URL url = obj.getClass().getResource("/" + resourcePath);
      return url;
    }
    return null;
  }

/*
	Method to import an image from the JAR file.
*/
  public static ImageIcon getImage(String imagePath) {
    return new ImageIcon(ClientMisc.getResource(iconfolder + imagePath));
  }

/*
	Method to log any kind of information
*/
  public static void logInformation(String fileName, String[] stringArray, boolean isPreferences, Component parent, int type) {
    BufferedWriter outputBuffer = null;
    try {
      if (isPreferences) {
        outputBuffer = new BufferedWriter(new FileWriter(fileName, false));
      } else {
//				String fileForSave = Misc.openFileDialog(this, "ESQUI Client", false, null, null, "ServerMSG.log");
//				outputBuffer = new BufferedWriter(new FileWriter(fileForSave, true));
        outputBuffer = new BufferedWriter(new FileWriter(fileName, true));
      }
      for (int i = 0; i < stringArray.length; i++) {
        outputBuffer.write(stringArray[i]);
        outputBuffer.newLine();
      }
    } catch (IOException ioExcep) {
      messageBox(parent, "Alert", "File writing error!", type);
    }
    try {
      outputBuffer.close();
    } catch (IOException ioexc) {
      ;
    }
    outputBuffer = null;
  }

/*
	Method to create a swing button
*/
  public static JButton createButton(String icon, String toolTip) {
    JButton tmpButton = new JButton(ClientMisc.getImage(icon));
    tmpButton.setToolTipText(toolTip);
    return tmpButton;
  }

  public static JButton createButton(String text, String icon, String toolTip) {
    JButton tmpButton = new JButton(text, ClientMisc.getImage(icon));
    tmpButton.setToolTipText(toolTip);
    return tmpButton;
  }


/*
	Method to read a file and write it into a text area
*/
  public static void writeInformation(JTextArea tmpTextArea) {
    tmpTextArea.setText(null);
    try {
      BufferedReader readInfo = new BufferedReader(new FileReader("putaname.log"));
      for (; ;) {
        String tmpLine = readInfo.readLine();
        if (tmpLine == null) {
          tmpTextArea.setText("\n");
        } else {
          tmpTextArea.setText(tmpLine);
        }
      }
    } catch (Exception exc) {
      System.out.println("Error on writing the file!!!");
    }
  }

/*
	Method to set borders depending on a value
*/
  public static Border newBorder(String title, int value) {
    Border tmpBorder = null;
    switch (value) {
      case 1:
        tmpBorder = new TitledBorder(BorderFactory.createCompoundBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED), BorderFactory.createEmptyBorder(3, 3, 3, 3)), title);
        break;
      case 2:
        tmpBorder = new TitledBorder(BorderFactory.createCompoundBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED), BorderFactory.createEmptyBorder(3, 3, 3, 3)), title);
        break;
      case 3:
        tmpBorder = new TitledBorder(BorderFactory.createCompoundBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED), BorderFactory.createEmptyBorder(3, 3, 3, 3)), title);
        break;
      case 4:
        tmpBorder = new TitledBorder(BorderFactory.createCompoundBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED), BorderFactory.createEmptyBorder(1, 1, 1, 1)), title);
        break;
    }
    return tmpBorder;
  }

  public static Border newEmptyBorder(String title, String alignment, int top, int bottom) {
    Border tmpBorder = null;
    if (alignment.equals("CENTER")) {
      tmpBorder = new TitledBorder(BorderFactory.createEmptyBorder(top, 0, bottom, 0), title, TitledBorder.CENTER, TitledBorder.ABOVE_TOP, new Font("Arial", Font.BOLD, 11));
    } else {
      tmpBorder = new TitledBorder(BorderFactory.createEmptyBorder(top, 0, bottom, 0), title, TitledBorder.LEFT, TitledBorder.ABOVE_TOP, new Font("Arial", Font.BOLD, 11));
    }
    return tmpBorder;
  }

  public static String getSystemProperty(String property) {
    return System.getProperty(property);
  }

  public static String getAttachedValues(String entireString) {
    int index = entireString.indexOf("&");
    return entireString.substring(index + 1);
  }

  public static Vector getParsedValues(String entireString) {
    StringTokenizer tmpTokenizer = new StringTokenizer(entireString, "&");
    Vector tokens = new Vector();
    while (tmpTokenizer.hasMoreTokens())
      tokens.addElement(tmpTokenizer.nextToken());
    return tokens;
  }

  public static String fileToSave(JFrame parent, String title, String filename) {
    FileDialog dialog = new FileDialog(parent, title, FileDialog.SAVE);
    String[] fileAndDir = getFolderandName(filename);
    dialog.setFile(fileAndDir[1]);
    dialog.setModal(true);
    dialog.setVisible(true);
    String tmpString = dialog.getDirectory() + dialog.getFile();
//		System.out.println(tmpString);
    dialog.dispose();
    return tmpString;
  }

  public static final String[] getFolderandName(String name) {
    name = filterFileName(name);
    String[] filename = new String[2];

    int index = name.lastIndexOf("/");
    if (index >= name.length() - 2 || index < 0) {
      filename[0] = "";
      filename[1] = name;
    } else {
      filename[0] = name.substring(0, index + 1);
      filename[1] = name.substring(index + 1, name.length());
    }

    return filename;
  }

  public static String StartPath = new String("/");

  public static final String filterFileName(String filename) {
    if (filename == null || filename.equals(""))
      return filename;
    StringBuffer tmp = new StringBuffer(filename);
    for (int i = 0; i < tmp.length(); i++) {
      if (tmp.charAt(i) == '\\')
        tmp.setCharAt(i, '/');
    }
    String startpath = "";
    String astring = tmp.toString();
    if (!astring.startsWith("//") && astring.startsWith("/"))
      startpath = StartPath;
    else if (astring.length() > 1 && astring.charAt(1) == ':')
      startpath = StartPath;
    return new String(startpath + astring);
  }

}
