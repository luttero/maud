package it.unitn.ing.wizard.LoskoWizard;

import java.util.Vector;

public class LoskoCspad {
	public String name;

	public boolean enabled = true;
	public boolean available = false;

	public Vector<LoskoSensorImage> detectors;

	public LoskoCspad() {
		detectors = new Vector<LoskoSensorImage>(0, 1);
	}
}
