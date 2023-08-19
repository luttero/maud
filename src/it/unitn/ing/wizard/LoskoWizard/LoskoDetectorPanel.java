package it.unitn.ing.wizard.LoskoWizard;

public class LoskoDetectorPanel {

	public static final int ROT_NO = 0, ROT_CW = 1, ROT_CCW = 2, ROT_180 = 3;

	public String panelName;
	public String panelType;
	public int detectorsNumber = 0;
	public String pedestalsFile;
	public String pixelsStatusFile;
	public int panelRotation = 0;
	public double pixel_size_mm_x = 115.0 / 256;
	public double pixel_size_mm_y = pixel_size_mm_x;
	public double panel_distance = 120.0;
	public double center_x = 0.0;
	public double center_y = 0.0;
	public double panel_tilting = 0.0;
	public double panel_2theta = 0.0;
	public double panel_omegaDN = 0.0;

	public LoskoDetectorPanel(String name) {
		panelName = name;
	}

}
