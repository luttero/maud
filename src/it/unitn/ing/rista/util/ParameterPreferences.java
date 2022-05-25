/*
 * @(#)ParameterPreferences.java created Mar 28, 2003 Berkeley
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.interfaces.PreferencesInterface;

import java.io.*;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;


/**
 * The ParameterPreferences is a class
 *  
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class ParameterPreferences extends PreferencesInterface {

	public static Preferences prefs;

  public ParameterPreferences() {
  }

  public static void loadPreferences() {

	  prefs = Preferences.userRoot().node(ParameterPreferences.class.getName());

  }

	public static boolean contains(String key) {
		return prefs.get(key, null) != null;
	}

	public Object getValue(String key) {
    return ParameterPreferences.getPref(key);
  }

  public void setValue(String key, Object value) {
    ParameterPreferences.setPref(key, value);
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

  public static void setPref(String key, double value) {
    prefs.put(key, Fmt.format(value));
  }

  public static double getDouble(String key, double defaultValue) {
	  if (!contains(key))
		  prefs.putDouble(key, defaultValue);
    return prefs.getDouble(key, defaultValue);
  }

}
