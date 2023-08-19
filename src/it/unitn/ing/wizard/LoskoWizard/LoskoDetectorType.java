package it.unitn.ing.wizard.LoskoWizard;

import it.unitn.ing.wizard.LoskoWizard.LoskoDetectorSensor;

import java.util.Vector;

public class LoskoDetectorType {

	String typeName = "";

	int width;
	int height;

	public Vector<LoskoDetectorSensor> sensors;

	public LoskoDetectorType(String name) {
		typeName = name;
		sensors = new Vector<LoskoDetectorSensor>(0, 1);
	}
}
