package it.unitn.ing.wizard.LCLS2Wizard;

import java.util.Vector;

public class Cspad {
	public String name;

	public boolean enabled = true;
	public boolean available = false;

	public Vector<SensorImage> detectors;

	public Cspad() {
		detectors = new Vector<SensorImage>(0, 1);
	}
}
