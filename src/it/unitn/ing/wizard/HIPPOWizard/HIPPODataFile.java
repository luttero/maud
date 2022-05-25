package it.unitn.ing.wizard.HIPPOWizard;

public class HIPPODataFile {
	
	public String fileName;
	public double omegaAngle = 0.0;
	
	HIPPODataFile(String fileName) {
		this.fileName = fileName;
	}
	
	public String toString() {
		return fileName;
	}

}
