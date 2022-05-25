package it.unitn.ing.wizard.HIPPOWizard;

import it.unitn.ing.rista.util.MaudPreferences;

import java.util.Vector;

public class HIPPOdata {
	
	public int BANK_NUMBER = 50;
	public String title = "New HIPPO analysis";
	public String author = "author";
	public String sampleName = "Sample_x";
	public String sampleDesc = "Sample description";
  public String calibrationFile;

	public boolean groupDatasetsByRotation = false;
	
	public double _instrument_neutron_flight_path = 10.0;
	
	public double omegaOffset = MaudPreferences.getDouble("hippoWizard.omegaOffset", 0.0);;
	
	public Vector mbank = new Vector(5, 3);/*{new HIPPOBank("150\ufffd bank", 1, true),
                              new HIPPOBank("90\ufffd bank", 9, true),
	                            new HIPPOBank("40\ufffd bank", 19, true),
	                            new HIPPOBank("20\ufffd bank", 31, false),
	                            new HIPPOBank("10\ufffd bank", 43, false)};*/
	
	public Vector/*<HIPPODataFile>*/ dataFiles = new Vector/*<HIPPODataFile>*/(10,10);

  public HIPPOdata(String title) {
    this.title = title;

  }
	
}
