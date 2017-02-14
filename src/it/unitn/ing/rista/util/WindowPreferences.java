/*
 * @(#)WindowPreferences.java created 02/09/2001 Riva Del Garda
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

import java.io.*;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

/**
 *  The WindowPreferences is a class to manage the Windows preferences.
 *
 * @version $Revision: 1.4 $, $Date: 2003/04/07 06:46:06 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class WindowPreferences {

	public static Preferences prefs;

	private WindowPreferences() {
  }

  public static void loadPreferences() {
	  prefs = Preferences.userRoot().node(WindowPreferences.class.getName());
  }

	public static boolean contains(String key) {
		return prefs.get(key, null) != null;
	}

	public static String getPref(String key, String defaultValue) {
	  if (!contains(key))
		  prefs.put(key, defaultValue);
    return prefs.get(key, defaultValue);
  }

  public static void setPref(String key, String defaultValue) {
    prefs.put(key, defaultValue);
  }

  public static void setPref(String key, int intvalue) {
    prefs.putInt(key, intvalue);
  }

  public static int getInteger(String key, String defaultValue) {
	  if (!contains(key))
		  prefs.put(key, defaultValue);
    return prefs.getInt(key, Integer.parseInt(defaultValue));
  }

  public static int getInteger(String key, int defaultValue) {
	  if (!contains(key))
		  prefs.putInt(key, defaultValue);
	  return prefs.getInt(key, defaultValue);
  }

  public static double getDouble(String key, String defaultValue) {
	  if (!contains(key))
		  prefs.put(key, defaultValue);
    return prefs.getDouble(key, Double.parseDouble(defaultValue));
  }

  public static double getDouble(String key, double defaultValue) {
	  if (!contains(key))
		  prefs.putDouble(key, defaultValue);
    return prefs.getDouble(key, defaultValue);
  }

  public static boolean getBoolean(String key, boolean defaultValue) {
	  if (!contains(key))
		  prefs.putBoolean(key, defaultValue);
    return prefs.getBoolean(key, defaultValue);
  }

}
