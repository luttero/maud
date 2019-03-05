package it.unitn.ing.wizard.LCLS2Wizard;

import it.unitn.ing.rista.awt.Utility;
import it.unitn.ing.rista.util.Misc;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

import static it.unitn.ing.rista.util.Constants.ENERGY_LAMBDA;

public class ImagesPanel extends JPanel {

	private JTextField textSampleName;
	private JTextField textCalibrationDatafile;
	private JTextField omegaTF;
	private JTextField energyInKeVTF;
	private JTextField calibrationDirectoryTF;
	private JTextField filenameToSaveTF;
	private JTextField[] corrImagePanelTF;
	private JTextField[] uncorrImagePanelTF;
	int panelsNumber;

	LCLS2data data;

	PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);

	/**
	 * Create the panel
	 */
	public ImagesPanel(LCLS2data data) {
		super();
		this.data = data;
		initGUI();
	}

	private void initGUI() {

		setLayout(new BorderLayout(3, 3));

		final JPanel panelNorth = new JPanel(new BorderLayout(3, 3));
		add(panelNorth, BorderLayout.NORTH);
		final JPanel panelSampleName = new JPanel(new GridLayout(0, 1, 3, 3));
		panelNorth.add(panelSampleName, BorderLayout.WEST);

		JPanel oneRow = new JPanel(new FlowLayout());
		oneRow.add(new JLabel("Sample name"));
		textSampleName = new JTextField(15);
		textSampleName.setText("Sample x");
		oneRow.add(textSampleName);
		panelSampleName.add(oneRow);

		oneRow = new JPanel(new FlowLayout());
		oneRow.add(new JLabel("Sample omega"));
		omegaTF = new JTextField(12);
		omegaTF.setText("0");
		oneRow.add(omegaTF);
		panelSampleName.add(oneRow);

		oneRow = new JPanel(new FlowLayout());
		oneRow.add(new JLabel("Radiation energy (KeV)"));
		energyInKeVTF = new JTextField(12);
		energyInKeVTF.setText("10.217");
		oneRow.add(energyInKeVTF);
		panelSampleName.add(oneRow);

		final JPanel panelSampleName1 = new JPanel(new GridLayout(0, 1, 3, 3));
		panelNorth.add(panelSampleName1, BorderLayout.CENTER);

		oneRow = new JPanel(new FlowLayout());
		JButton browseCalibrationButton = createBrowseButtonToLoad("Directory for Cspad calibration files", false);
//    browseCalibrationButton2.setPreferredSize(new Dimension(100, 30));
		oneRow.add(browseCalibrationButton);
		calibrationDirectoryTF = new JTextField(40);
		calibrationDirectoryTF.setText("Directory with Cspad calibration files....");
		oneRow.add(calibrationDirectoryTF);
		panelSampleName1.add(oneRow);

		String property = "detector_config_file";
		String filename_cf = "LCLS2_config_data.cif";
		filename_cf = LCLS2ConfigData.getPropertyValue(property, filename_cf);
		if (Misc.checkForFile(filename_cf)) {
			LCLS2ConfigData.readLCLSConfigDataFromFile(filename_cf);
			omegaTF.setText(Double.toString(LCLS2ConfigData.omega));
			energyInKeVTF.setText(Double.toString(LCLS2ConfigData.radiationKeV));
			calibrationDirectoryTF.setText(LCLS2ConfigData.calibrationDirectory);
		} else {
			filename_cf += " (Not found, select it)";
		}
		oneRow = new JPanel(new FlowLayout());
		browseCalibrationButton = createBrowseButton("Load config file");
//    browseCalibrationButton2.setPreferredSize(new Dimension(100, 30));
		oneRow.add(browseCalibrationButton);
		textCalibrationDatafile = new JTextField(40);
		textCalibrationDatafile.setText(filename_cf);
		oneRow.add(textCalibrationDatafile);
		panelSampleName1.add(oneRow);

		oneRow = new JPanel(new FlowLayout());
		browseCalibrationButton = createBrowseButtonToLoad("Filename to save", true);
//    browseCalibrationButton2.setPreferredSize(new Dimension(100, 30));
		oneRow.add(browseCalibrationButton);
		filenameToSaveTF = new JTextField(40);
		filenameToSaveTF.setText("Filename for saving unrolled images datafile....");
		oneRow.add(filenameToSaveTF);
		panelSampleName1.add(oneRow);

		panelsNumber = data.panelsNumber;
		corrImagePanelTF = new JTextField[panelsNumber];
		uncorrImagePanelTF = new JTextField[panelsNumber];
		final JPanel imagePanels = new JPanel(new GridLayout(0, 1, 3, 3));
		for (int i = 0; i < panelsNumber; i++) {
			final JPanel panelCalibration = createImagePanel("Cspad." + i, i);
			imagePanels.add(panelCalibration);
		}
		add(imagePanels, BorderLayout.CENTER);

	}

	public JPanel createImagePanel(String imagePanelID, int index) {

		final JPanel panelCalibration = new JPanel(new BorderLayout(3, 3));

		panelCalibration.add(new JLabel(imagePanelID), BorderLayout.WEST);

		JPanel rightPanel = new JPanel(new GridLayout(0, 1, 3, 3));
		JPanel panel_1 = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
		rightPanel.add(panel_1);
		panel_1.add(new JLabel("Original image: "));
		uncorrImagePanelTF[index] = new JTextField(64);
		panel_1.add(uncorrImagePanelTF[index]);
		String property = imagePanelID + "_uncorrected_image";
		String filename_or = "Select uncorrected image....";
		filename_or = LCLS2ConfigData.getPropertyValue(property, filename_or);
		uncorrImagePanelTF[index].setText(filename_or);
		JButton browseCalibrationButton = createBrowseButton("Load original uncorrected image", uncorrImagePanelTF[index], property);
//    browseCalibrationButton.setPreferredSize(new Dimension(100, 30));
		panel_1.add(browseCalibrationButton);

		JPanel panel_2 = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
		rightPanel.add(panel_2);
		panel_2.add(new JLabel("Dark image file: "));
		corrImagePanelTF[index] = new JTextField(64);
		panel_2.add(corrImagePanelTF[index]);
		String property1 = imagePanelID + "_dark_current_image";
		String filename_dark = "Select dark current image....";
		filename_dark = LCLS2ConfigData.getPropertyValue(property1, filename_dark);
		corrImagePanelTF[index].setText(filename_dark);
		JButton browseCalibrationButton2 = createBrowseButton("Load pedestals file", corrImagePanelTF[index], property1);
//    browseCalibrationButton2.setPreferredSize(new Dimension(100, 30));
		panel_2.add(browseCalibrationButton2);

		panelCalibration.add(rightPanel, BorderLayout.CENTER);

		return panelCalibration;
	}

	public JButton createBrowseButton(String title) {
		JButton browseCalibrationButton = new JButton();
		browseCalibrationButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String calibrationFile = Utility.browseFilename(null, title);
				if (calibrationFile != null) {
					String property = "detector_config_file";
					LCLS2ConfigData.setPropertyValue(property, calibrationFile);
					textCalibrationDatafile.setText(calibrationFile);
					LCLS2ConfigData.readLCLSConfigDataFromFile(calibrationFile);
					omegaTF.setText(Double.toString(LCLS2ConfigData.omega));
					energyInKeVTF.setText(Double.toString(LCLS2ConfigData.radiationKeV));
					calibrationDirectoryTF.setText(LCLS2ConfigData.calibrationDirectory);
					propertyChangeSupport.firePropertyChange("CALIB_FILE_SET", "", textCalibrationDatafile);
				}
			}
		});
		browseCalibrationButton.setText("Browse ...");
		return browseCalibrationButton;
	}

	public JButton createBrowseButtonToLoad(String title, boolean isfile) {
		JButton browseCalibrationButton = new JButton();
		browseCalibrationButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String calibrationFile = Utility.browseFilename(null, title);
				if (calibrationFile != null) {
					if (isfile) {
						filenameToSaveTF.setText(calibrationFile);
						propertyChangeSupport.firePropertyChange("FILENAME_TOSAVE_SET", "", filenameToSaveTF);
					} else {
						calibrationDirectoryTF.setText(calibrationFile);
						propertyChangeSupport.firePropertyChange("CALIB_DIRECTORY_SET", "", calibrationDirectoryTF);
					}
				}
			}
		});
		browseCalibrationButton.setText("Browse ...");
		return browseCalibrationButton;
	}

	public JButton createBrowseButton(String title, JTextField textField, String filenamePrefKey) {
		JButton browseCalibrationButton = new JButton();
		browseCalibrationButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String imageFile = Utility.browseFilename(null, title);
				if (imageFile != null) {
					textField.setText(imageFile);
					LCLS2ConfigData.setPropertyValue(filenamePrefKey, imageFile);
					propertyChangeSupport.firePropertyChange("IMAGE_FILE_SET", "", imageFile);
				}
			}
		});
		browseCalibrationButton.setText("Browse ...");
		return browseCalibrationButton;
	}

	public void saveData() {
		LCLS2ConfigData.filenameToSave = filenameToSaveTF.getText();
		data.sampleName = textSampleName.getText();
		data.omega = Double.parseDouble(omegaTF.getText());
		data.wavelength = ENERGY_LAMBDA / (Double.parseDouble(energyInKeVTF.getText()) * 1000);
		for (int i = 0; i < panelsNumber; i++)
			data.addImagePanel(i, corrImagePanelTF[i].getText(), uncorrImagePanelTF[i].getText());
	}

	public boolean enabledToContinue() {
		boolean canContinue = false;
		for (int i = 0; i < panelsNumber; i++)
			canContinue = canContinue || (!(corrImagePanelTF[i].getText().trim().equals("")) &&
					!(uncorrImagePanelTF[i].getText().trim().equals("")));
//		System.out.println(canContinue);
		return true; //canContinue;
	}


	public void addPropertyChangeListener(PropertyChangeListener p) {
		if (propertyChangeSupport == null)
			propertyChangeSupport = new PropertyChangeSupport(this);
		propertyChangeSupport.addPropertyChangeListener(p);
	}

}
