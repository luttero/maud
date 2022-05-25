package it.unitn.ing.wizard.LCLS2Wizard;

import java.util.Vector;

public class LCLSDetectorType {

	String typeName = "";

	int width;
	int height;

	public Vector<LCLSDetectorSensor> sensors;

	public LCLSDetectorType(String name) {
		typeName = name;
		sensors = new Vector<LCLSDetectorSensor>(0, 1);
	}
}
