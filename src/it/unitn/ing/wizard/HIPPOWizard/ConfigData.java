package it.unitn.ing.wizard.HIPPOWizard;

import it.unitn.ing.rista.util.*;

public class ConfigData {
	
	public static final String INSTRUMENT_FILE_DIR = "Instrument file directory";
	public static final String DATA_FILE_DIR = "Hippo datafile directory";
	public static final String BANK_ENABLED = " enabled";

	public static final String[] BANK_DMIN_DMAX = {
      " d-spacing min",
      " d-spacing max"};
	
	public static String getPropertyValue(String property, String defaultvalue) {
    property = Misc.toStringNoBlank(property);
    return MaudPreferences.getPref("hippoWizard." + property, defaultvalue);
  }
    
    public static void setPropertyValue(String property, String value) {
      property = Misc.toStringNoBlank(property);
      MaudPreferences.setPref("hippoWizard." + property, value);
    }
    
	public static void writeConfig() {
  }

}
