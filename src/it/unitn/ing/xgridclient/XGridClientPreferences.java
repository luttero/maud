/*
 * @(#)XGridClientPreferences.java created Aug 29, 2007 Casalino
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.xgridclient;

import it.unitn.ing.rista.util.Misc;

import java.io.*;

/**
 * The XGridClientPreferences is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Aug 29, 2007 10:03:09 AM $
 * @since JDK1.1
 */
public class XGridClientPreferences {

  public static XGridSortedProperties prefs = null;
  static boolean testing = true;

  public XGridClientPreferences() {
  }

  public static void loadPreferences() {

    InputStream preferencesFile = getInputStream(System.getProperty("user.dir"), "lastInput.Maud");
    prefs = new XGridSortedProperties();
    // Default values

    if (preferencesFile != null) {
      try {
        prefs.load(preferencesFile);
//        if (Constants.testing)
//          prefs.list(System.out);
      } catch (Exception e) {
        System.out.println("Last input file not found (first run?), a new one will be created on exit");
//				e.printStackTrace();
      }
    }
  }

  public Object getValue(String key) {
    return getPref(key);
  }

  public void setValue(String key, Object value) {
    setPref(key, value);
  }

  public static Object getPref(String key) {
    return prefs.getProperty(key);
  }

  public static String getPref(String key, String defaultValue) {
    if (prefs.getProperty(key) == null) {
      addPref(key, defaultValue);
      if (testing)
        System.out.println("Adding last input value: " + key + ", value: " + defaultValue);
    }
    return (String) getPref(key);
  }

  public static void setPref(String key, Object value) {
    setPref(key, (String) value);
  }

  public static void setPref(String key, String value) {
    prefs.setProperty(key, value);
  }

  public static void setPref(String key, int intvalue) {
    prefs.setProperty(key, Integer.toString(intvalue));
  }

  public static void setPref(String key, double value) {
    prefs.setProperty(key, Double.toString(value));
  }

  public static void setPref(String key, boolean intvalue) {
    String value = "false";
    if (intvalue)
      value = "true";
    prefs.setProperty(key, value);
  }

  public static void addPref(String key, String value) {
    prefs.setProperty(key, value);
  }

  public static int getInteger(String key) {
    return Integer.valueOf((String) getPref(key)).intValue();
  }

  public static int getInteger(String key, String defaultValue) {
    return Integer.valueOf(getPref(key, defaultValue)).intValue();
  }

  public static int getInteger(String key, int defaultValue) {
    return getInteger(key, Integer.toString(defaultValue));
  }

  public static double getDouble(String key) {
    return Double.valueOf((String) getPref(key)).doubleValue();
  }

  public static double getDouble(String key, String defaultValue) {
    return Double.valueOf(getPref(key, defaultValue)).doubleValue();
  }

  public static double getDouble(String key, double defaultValue) {
    return getDouble(key, Double.toString(defaultValue));
  }

  public static boolean getBoolean(String key) {
    String tmp = (String) getPref(key);
    if (tmp.equalsIgnoreCase("true"))
      return true;
    else
      return false;
  }

  public static boolean getBoolean(String key, String defaultValue) {
    String tmp = getPref(key, defaultValue);
    if (tmp.equalsIgnoreCase("true"))
      return true;
    else
      return false;
  }

  public static boolean getBoolean(String key, boolean defaultValue) {
    String value = "true";
    if (!defaultValue)
      value = "false";
    return getBoolean(key, value);
  }

  public static void savePreferences() {
    FileOutputStream preferencesFile = getFileOutputStream(System.getProperty("user.dir"), "lastInput.Maud");
    if (preferencesFile != null)
      try {
        prefs.store(preferencesFile, " XGridClient, version " + XGridClient.getVersion());
      } catch (IOException io) {

      }
  }

  public static final InputStream getInputStream(String folder, String filename) {
    InputStream in;
    filename = folder + filename;
    try {
      in = new FileInputStream(new File(filename));
    } catch (IOException e) {
      in = null;
      System.out.println("File not found: " + filename);
    }
    return in;
  }

  public static final FileOutputStream getFileOutputStream(String folder, String filename) {
    FileOutputStream out;
    filename = folder + filename;
    try {
      out = new FileOutputStream(new File(filename));
    } catch (IOException e) {
      out = null;
      System.out.println("Unable to open file: " + filename);
    }
    return out;
  }

}
