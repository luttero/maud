/*
 * @(#)LastInputValues.java created 5/07/2006 Casalino
 *
 * Copyright (c) 2006 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
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

import it.unitn.ing.rista.interfaces.PreferencesInterface;
import java.io.*;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

/**
 *  The LastInputValues is a class to manage the Maud last inputs.
 *
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:04 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class LastInputValues extends PreferencesInterface {

	public static Preferences prefs;

  public LastInputValues() {
  }

  public static void loadPreferences() {

	  prefs = Preferences.userRoot().node(LastInputValues.class.getName());
  }

	public static boolean contains(String key) {
		return prefs.get(key, null) != null;
	}

	public Object getValue(String key) {
    return getPref(key);
  }

  public void setValue(String key, Object value) {
    setPref(key, value);
  }

  public static Object getPref(String key) {
    return getPref(key, "");
  }

  public static String getPref(String key, String defaultValue) {
	  if (!contains(key))
		  prefs.put(key, defaultValue);
    return prefs.get(key, defaultValue);
  }

  public static void setPref(String key, Object value) {
    setPref(key, (String) value);
  }

  public static void setPref(String key, String value) {
    prefs.put(key, value);
  }

  public static void setPref(String key, int intvalue) {
    prefs.putInt(key, intvalue);
  }

  public static void setPref(String key, double value) {
    prefs.putDouble(key, value);
  }

  public static void setPref(String key, boolean value) {
    prefs.putBoolean(key, value);
  }

}
