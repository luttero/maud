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
	private JTextField filenameTemplateTF;
	private JTextField[] corrImagePanelTF;
	private JTextField[] uncorrImagePanelTF;
	private JCheckBox newFileParCB;
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
		textSampleName = new JTextField(10);
		textSampleName.setText("Sample x");
		oneRow.add(textSampleName);
		panelSampleName.add(oneRow);

		oneRow = new JPanel(new FlowLayout());
		oneRow.add(new JLabel("Sample omega"));
		omegaTF = new JTextField(10);
		omegaTF.setText("0");
		oneRow.add(omegaTF);
		panelSampleName.add(oneRow);

		oneRow = new JPanel(new FlowLayout());
		oneRow.add(new JLabel("Radiation energy (KeV)"));
		energyInKeVTF = new JTextField(10);
		energyInKeVTF.setText("10.217");
		oneRow.add(energyInKeVTF);
		panelSampleName.add(oneRow);

		oneRow = new JPanel(new FlowLayout());
		newFileParCB = new JCheckBox("New file parameter");
		newFileParCB.setSelected(false);
		oneRow.add(newFileParCB);
		panelSampleName.add(oneRow);

		final JPanel panelSampleName1 = new JPanel(new GridLayout(0, 1, 3, 3));
		panelNorth.add(panelSampleName1, BorderLayout.CENTER);

		oneRow = new JPanel(new FlowLayout());
		JButton browseCalibrationButton = createBrowseButtonToLoad("Directory for Cspad calibration files", false);
//    browseCalibrationButton2.setPreferredSize(new Dimension(100, 30));
		oneRow.add(browseCalibrationButton);
		calibrationDirectoryTF = new JTextField(36);
		calibrationDirectoryTF.setText("Cspad calibration files dir (not used)");
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
		browseCalibrationButton = createBrowseButton("Detector config file...");
//    browseCalibrationButton2.setPreferredSize(new Dimension(100, 30));
		oneRow.add(browseCalibrationButton);
		textCalibrationDatafile = new JTextField(36);
		textCalibrationDatafile.setText(filename_cf);
		oneRow.add(textCalibrationDatafile);
		panelSampleName1.add(oneRow);

		oneRow = new JPanel(new FlowLayout());
		browseCalibrationButton = createBrowseButtonToLoad("Unrolled images file...", true);
//    browseCalibrationButton2.setPreferredSize(new Dimension(100, 30));
		oneRow.add(browseCalibrationButton);
		filenameToSaveTF = new JTextField(36);
		filenameToSaveTF.setText(LCLS2ConfigData.filenameToSave);
		oneRow.add(filenameToSaveTF);
		panelSampleName1.add(oneRow);

		oneRow = new JPanel(new FlowLayout());
		browseCalibrationButton = createBrowseButtonForTemplate("Analysis template file...");
//    browseCalibrationButton2.setPreferredSize(new Dimension(100, 30));
		oneRow.add(browseCalibrationButton);
		filenameTemplateTF = new JTextField(36);
		filenameTemplateTF.setText(LCLS2ConfigData.filenameTemplate);
		oneRow.add(filenameTemplateTF);
		panelSampleName1.add(oneRow);

		panelsNumber = data.panelsNumber;
		corrImagePanelTF = new JTextField[panelsNumber];
		uncorrImagePanelTF = new JTextField[panelsNumber];
		final JPanel imagePanels = new JPanel(new GridLayout(0, 1, 3, 3));
		for (int i = 0; i < panelsNumber; i++) {
			String prefix = "Cspad";
			if (i > 0)
				prefix = prefix + "2x2";
			final JPanel panelCalibration = createImagePanel(prefix + "." + i, i);
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
		uncorrImagePanelTF[index] = new JTextField(54);
		panel_1.add(uncorrImagePanelTF[index]);
		String property = imagePanelID + "_uncorrected_image";
		String filename_or = "Select uncorrected image....";
		filename_or = LCLS2ConfigData.getPropertyValue(property, filename_or);
		uncorrImagePanelTF[index].setText(filename_or);
		JButton browseCalibrationButton = createBrowseButton("Load original uncorrected image", uncorrImagePanelTF[index], property);
//    browseCalibrationButton.setPreferredSize(new Dimension(100, 30));
		panel_1.add(browseCalibrationButton);
		if (index == 0) {
			JButton guessButton = new JButton("Guess the others");
			guessButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					String filename = uncorrImagePanelTF[0].getText();
					if (Misc.checkForFile(filename)) {
						for (int j = 1; j < 5; j++)
							uncorrImagePanelTF[j].setText(filename.replaceFirst("Cspad-0", "Cspad2x2-" + j));
					}
				}
			});
			panel_1.add(guessButton);
		}

		JPanel panel_2 = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
		rightPanel.add(panel_2);
		panel_2.add(new JLabel("Dark image file: "));
		corrImagePanelTF[index] = new JTextField(54);
		panel_2.add(corrImagePanelTF[index]);
		String property1 = imagePanelID + "_dark_current_image";
		String filename_dark = "Select dark current image....";
		filename_dark = LCLS2ConfigData.getPropertyValue(property1, filename_dark);
		corrImagePanelTF[index].setText(filename_dark);
		JButton browseCalibrationButton2 = createBrowseButton("Load pedestals file", corrImagePanelTF[index], property1);
//    browseCalibrationButton2.setPreferredSize(new Dimension(100, 30));
		panel_2.add(browseCalibrationButton2);
		if (index == 0) {
			JButton guessButton = new JButton("Guess the others");
			guessButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					String filename = corrImagePanelTF[0].getText();
					if (Misc.checkForFile(filename)) {
						filename = filename.replaceFirst("CsPad_", "CsPad2x2_");
						for (int j = 1; j < 5; j++)
							corrImagePanelTF[j].setText(filename.replaceFirst("Cspad.0", "Cspad2x2." + j));
					}
				}
			});
			panel_2.add(guessButton);
		}

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
		browseCalibrationButton.setText(title);
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
		browseCalibrationButton.setText(title);
		return browseCalibrationButton;
	}

	public JButton createBrowseButtonForTemplate(String title) {
		JButton browseCalibrationButton = new JButton();
		browseCalibrationButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String templateFile = Utility.browseFilename(null, title);
				if (templateFile != null) {
						filenameTemplateTF.setText(templateFile);
						propertyChangeSupport.firePropertyChange("FILENAME_TEMPLATE_SET", "", filenameTemplateTF);
				}
			}
		});
		browseCalibrationButton.setText(title);
		return browseCalibrationButton;
	}

	public JButton createBrowseButton(String title, JTextField textField, String filenamePrefKey) {
		JButton browseCalibrationButton = new JButton();
		browseCalibrationButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String imageFile = Utility.browseFilename(null, title);
				if (imageFile != null) {
					textField.setText(imageFile);
					propertyChangeSupport.firePropertyChange("IMAGE_FILE_SET", "", imageFile);
				}
			}
		});
		browseCalibrationButton.setText("Browse...");
		return browseCalibrationButton;
	}

	public void saveData() {
		LCLS2ConfigData.filenameToSave = filenameToSaveTF.getText();
		LCLS2ConfigData.setPropertyValue("UnrolledImagesDatafile", LCLS2ConfigData.filenameToSave);
		LCLS2ConfigData.filenameTemplate = filenameTemplateTF.getText();
		LCLS2ConfigData.setPropertyValue("LCLSdefaultTemplate", LCLS2ConfigData.filenameTemplate);
		data.useTempletaFile = !newFileParCB.isSelected();
		data.sampleName = textSampleName.getText();
		data.omega = Double.parseDouble(omegaTF.getText());
		data.wavelength = ENERGY_LAMBDA / (Double.parseDouble(energyInKeVTF.getText()) * 1000);
		for (int i = 0; i < panelsNumber; i++) {
			String prefix = "Cspad";
			if (i > 0)
				prefix = prefix + "2x2";
			String property1 = prefix + "." + i + "_uncorrected_image";
			String property2 = prefix + "." + i + "_dark_current_image";
			String file1 = uncorrImagePanelTF[i].getText();
			String file2 = corrImagePanelTF[i].getText();
			LCLS2ConfigData.setPropertyValue(property1, file1);
			LCLS2ConfigData.setPropertyValue(property2, file2);
			data.addImagePanel(i, file2, file1);
		}
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
