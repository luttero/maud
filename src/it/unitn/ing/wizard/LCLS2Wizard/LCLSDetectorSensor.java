package it.unitn.ing.wizard.LCLS2Wizard;

public class LCLSDetectorSensor {

	public String panelType;
	public int detector_number;
	public int origin_x;
	public int origin_y;
	public int detector_width;
	public int detector_height;
	public int pedestal_number;
	public int pedestal_rotation;
	public int pedestal_shift_x;
	public int pedestal_shift_y;

	public LCLSDetectorSensor(String type) {
		panelType = type;
	}
}
