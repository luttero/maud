package it.unitn.ing.esqui.server;

import java.io.*;
import java.util.*;
import java.awt.*;
import java.net.*;
import javax.swing.*;
import javax.swing.border.*;

/** ServerMisc.java
 * <br>
 * Title:			<b>ESQUI Server Miscellaneous</b>
 * </br>
 * Description:	Miscellaneous methods for the ESQUI server
 * @author:			Leonardo Cont, December 2000
 * @revision:		December 2000
 * @comment:		none
 */

public class ServerMisc {

  public static final String imagefolder = new String("images/");
  public static final String iconfolder = new String(imagefolder + "20x20/");

  private ServerMisc() {
  }

  public static void checkDirectory(String dirName) {
    File dirToCheck = new File(dirName);
    if (!dirToCheck.isDirectory())
      dirToCheck.mkdir();
  }

  public static void checkDirectories(String[] dirNames) {
    File dirToCheck = null;
    for (int i = 0; i < dirNames.length; i++) {
      dirToCheck = new File(dirNames[i]);
      if (!dirToCheck.isDirectory())
        dirToCheck.mkdir();
    }
  }

/*
	Method to locate frames on the screen.
*/

  public static void locateOnScreen(Window window, int xperc, int yperc) {
    Dimension windowSize = window.getSize();
    Dimension screenSize = window.getToolkit().getScreenSize();

    window.setLocation((screenSize.width - windowSize.width) * xperc / 100,
            (screenSize.height - windowSize.height) * yperc / 100);
  }

  public static String getAttachedValue(String entireString) {
    int index = entireString.indexOf("&");
    return entireString.substring(index + 1);
  }

  public static String getFileNoExtension(String tmpString) {
    return tmpString.substring(0, tmpString.lastIndexOf("."));
  }

/*
	Create a pop-up message box
*/
  public static void messageBox(Component parent, String title, String message, int type) {
    JOptionPane.showMessageDialog(parent, message, title, type);
  }

  public static int confirmBox(Component parent, String title, String message, int type) {
    return JOptionPane.showConfirmDialog(parent, message, title, JOptionPane.OK_CANCEL_OPTION, type);
  }

  public static String date() {
    String day;
    String month;
    String year;
    int tmpDay = Calendar.getInstance().get(Calendar.DAY_OF_MONTH);
    if (tmpDay < 10) {
      day = new String("0" + String.valueOf(tmpDay));
    } else {
      day = String.valueOf(tmpDay);
    }
    int tmpMonth = Calendar.getInstance().get(Calendar.MONTH) + 1;
    if (tmpMonth < 10) {
      month = new String("0" + String.valueOf(tmpMonth));
    } else {
      month = String.valueOf(tmpMonth);
    }
    year = String.valueOf(Calendar.getInstance().get(Calendar.YEAR)).substring(2);
    return new String(day + month + year);
  }

  public static String time() {
    String hour;
    String minute;
    String second;
    int tmpHour = Calendar.getInstance().get(Calendar.HOUR_OF_DAY);
    if (tmpHour < 10) {
      hour = String.valueOf("0" + String.valueOf(tmpHour));
    } else {
      hour = String.valueOf(tmpHour);
    }
    int tmpMinute = Calendar.getInstance().get(Calendar.MINUTE);
    if (tmpMinute < 10) {
      minute = String.valueOf("0" + String.valueOf(tmpMinute));
    } else {
      minute = String.valueOf(tmpMinute);
    }
    int tmpSecond = Calendar.getInstance().get(Calendar.SECOND);
    if (tmpSecond < 10) {
      second = String.valueOf("0" + String.valueOf(tmpSecond));
    } else {
      second = String.valueOf(tmpSecond);
    }
    second = String.valueOf(tmpSecond);
    return new String(hour + minute + second);
  }

  public static String getSystemProperty(String property) {
    return System.getProperty(property);
  }

  public static void logInformation(String messageLine) {
    try {
      BufferedWriter outputBuffer = new BufferedWriter(new FileWriter("MessagesLog_" + date() + ".log", true));
      outputBuffer.write(messageLine);
      outputBuffer.newLine();
      outputBuffer.close();
    } catch (IOException ioexc) {
      Server.fail(ioexc, "Log file writing error!", false);
    }
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
    return new ImageIcon(ServerMisc.getResource(iconfolder + imagePath));
  }

/*
	Method to create a swing button
*/
  public static JButton createButton(String icon, String toolTip) {
    JButton tmpButton = new JButton(ServerMisc.getImage(icon));
    tmpButton.setToolTipText(toolTip);
    return tmpButton;
  }

/*
	Method to set borders depending on a value
*/
  public static Border newBorder(String title, int value) {
    Border tmpBorder = null;
    switch (value) {
      case 1:
        tmpBorder = new TitledBorder(BorderFactory.createCompoundBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED), BorderFactory.createEmptyBorder(1, 1, 1, 1)), title, TitledBorder.CENTER, TitledBorder.BELOW_TOP);
        break;
      case 2:
        tmpBorder = new TitledBorder(BorderFactory.createEmptyBorder(0, 0, 10, 0), title, TitledBorder.CENTER, TitledBorder.BELOW_TOP);
        break;
    }
    return tmpBorder;
  }
}
