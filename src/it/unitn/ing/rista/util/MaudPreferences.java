/*
 * @(#)MaudPreferences.java created 25/04/2000 Casalino
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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

import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

/**
 *  The MaudPreferences is a class to manage the Maud preferences.
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class MaudPreferences extends PreferencesInterface {

	public static Preferences prefs;

  public MaudPreferences() {
  }

	public static void resetPreferences() {
		try {
			prefs.clear();
		} catch (BackingStoreException e) {
			e.printStackTrace();
		}
	}

  public static void loadPreferences() {
	  prefs = Preferences.userRoot().node(MaudPreferences.class.getName());
  }

  public Object getValue(String key) {
    return MaudPreferences.getPref(key, "");
  }

  public void setValue(String key, Object value) {
    MaudPreferences.setPref(key, value.toString());
  }

	public static boolean contains(String key) {
		return prefs.get(key, null) != null;
	}

  public static void setPref(String key, String value) {
    prefs.put(key, value);
  }

  public static void setPref(String key, int intvalue) {
    prefs.putInt(key, intvalue);
  }

	public static void setPref(String key, long longvalue) {
		prefs.putLong(key, longvalue);
	}

	public static void setPref(String key, double value) {
    prefs.putDouble(key, value);
  }

  public static void setPref(String key, boolean value) {
    prefs.putBoolean(key, value);
  }

	public static String getPref(String key, String defaultValue) {
		if (!contains(key))
			prefs.put(key, defaultValue);
		return prefs.get(key, defaultValue);
	}

	public static int getInteger(String key, int defaultValue) {
	  if (!contains(key))
		  prefs.putInt(key, defaultValue);
	  return prefs.getInt(key, defaultValue);
  }

  public static double getDouble(String key, double defaultValue) {
	  if (!contains(key))
		  prefs.putDouble(key, defaultValue);
    return prefs.getDouble(key, defaultValue);
  }

	public static long getLong(String key, long defaultValue) {
		if (!contains(key))
			prefs.putLong(key, defaultValue);
		return prefs.getLong(key, defaultValue);
	}

	public static boolean getBoolean(String key) {
		return getBoolean(key, false);
  }

  public static boolean getBoolean(String key, boolean defaultValue) {
	  if (!contains(key))
		  prefs.putBoolean(key, defaultValue);
	  return prefs.getBoolean(key, defaultValue);
  }

}
