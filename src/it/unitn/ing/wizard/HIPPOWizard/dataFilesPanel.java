package it.unitn.ing.wizard.HIPPOWizard;

import it.unitn.ing.rista.util.MaudPreferences;
import it.unitn.ing.rista.util.Misc;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.*;

import javax.swing.*;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.filechooser.FileSystemView;

public class dataFilesPanel extends JPanel {

	private JTextField textSampleName;
	private JTextField textOmegaSet;
	private JTextField textOmegaReal;
	private JTextField textOmegaOffset;
  private JTextField textCalibrationFile;
	private JList listDataFiles;
	private DefaultListModel dataFilesModel;
	private int lastSelectedData = -1;
	double omegaOffset = MaudPreferences.getDouble("hippoWizard.omegaOffset", 0.0);

  HIPPOdata data;

  PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);
	
	/**
	 * Create the panel
	 */
	public dataFilesPanel(HIPPOdata data) {
		super();
    this.data = data;
    dataFilesModel = new DefaultListModel();
		initGUI();
  }
	
	private void initGUI() {

		final JPanel panelSampleName = new JPanel();
		panelSampleName.setLayout(new BorderLayout());
		add(panelSampleName);

		textSampleName = new JTextField(32);
    textSampleName.setText("Sample x");
		panelSampleName.add(textSampleName, BorderLayout.SOUTH);

		final JLabel sampleNameLabel = new JLabel();
		sampleNameLabel.setText("Sample name");
		panelSampleName.add(sampleNameLabel);

		final JPanel panelCalibration = new JPanel();
		final BorderLayout borderLayout = new BorderLayout();
		panelCalibration.setLayout(borderLayout);
		add(panelCalibration);
		setLayout(new FlowLayout());
		final JLabel chooseCalibrationFileLabel = new JLabel();
		chooseCalibrationFileLabel.setText("Choose calibration file");
		panelCalibration.add(chooseCalibrationFileLabel, BorderLayout.NORTH);

		final JPanel panel_2 = new JPanel();
		final FlowLayout flowLayout = new FlowLayout();
		panel_2.setLayout(flowLayout);
		panelCalibration.add(panel_2);

		textCalibrationFile = new JTextField(32);
		panel_2.add(textCalibrationFile);

		final JPanel panel_1 = new JPanel();
		panelCalibration.add(panel_1, BorderLayout.EAST);

    JButton browseCalibrationButton = new JButton();
//    browseCalibrationButton.setPreferredSize(new Dimension(100, 30));
		panel_1.add(browseCalibrationButton);
		browseCalibrationButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					String DefaultDirectory = FileSystemView.getFileSystemView().getDefaultDirectory().getCanonicalPath();
					String instrumentFileDir = ConfigData.getPropertyValue(ConfigData.INSTRUMENT_FILE_DIR, DefaultDirectory);
					final JFileChooser fileChooser = new JFileChooser(instrumentFileDir);
					if (fileChooser.showOpenDialog(dataFilesPanel.this) == JFileChooser.APPROVE_OPTION) {
						ConfigData.setPropertyValue(ConfigData.INSTRUMENT_FILE_DIR, fileChooser.getCurrentDirectory().getCanonicalPath());
						File file = fileChooser.getSelectedFile();
						String fileName = file.getCanonicalPath(); 
						textCalibrationFile.setText(fileName);
						Reader reader = new FileReader(fileName);
						BufferedReader buffer = new BufferedReader(reader);
						it.unitn.ing.wizard.HIPPOWizard.IPRFileReader iprReader = new IPRFileReader(buffer);
						iprReader.read(data);
						propertyChangeSupport.firePropertyChange("INTRUMENT_FILE_SET", "", fileName);
					}
				} catch (IOException IOE) {
					IOE.printStackTrace();
				}
			}
		});
		browseCalibrationButton.setText("Browse ...");

		final JPanel panelData = new JPanel();
		panelData.setMinimumSize(new Dimension(0, 0));
		panelData.setLayout(new BorderLayout());
		add(panelData);

		final JLabel chooseDatafilesToLabel = new JLabel();
		chooseDatafilesToLabel.setText("Choose datafiles:");
		panelData.add(chooseDatafilesToLabel, BorderLayout.NORTH);

		final JPanel panel_7 = new JPanel();
		panelData.add(panel_7);

		final JScrollPane scrollPane = new JScrollPane();
		panel_7.add(scrollPane);
		scrollPane.setPreferredSize(new Dimension(380, 180));
		listDataFiles = new JList(dataFilesModel);
		listDataFiles.addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
			  if (lastSelectedData >= 0)
					try {
						((HIPPODataFile) dataFilesModel.get(lastSelectedData)).omegaAngle = Double
								.parseDouble(textOmegaSet.getText());
						data.omegaOffset = Double.parseDouble(textOmegaOffset.getText());
					} catch (NumberFormatException nfe) {
          }
        setDataFile(listDataFiles.getSelectedIndex());
        lastSelectedData = listDataFiles.getSelectedIndex();
      }
		});
		scrollPane.setViewportView(listDataFiles);

		
		final JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		panelData.add(panel, BorderLayout.EAST);

		final JPanel panelOmega = new JPanel();
		panelOmega.setLayout(new BoxLayout(panelOmega, BoxLayout.Y_AXIS));
		panel.add(panelOmega);

		final JLabel fixedOffsetLabel = new JLabel();
		fixedOffsetLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		panelOmega.add(fixedOffsetLabel);
		fixedOffsetLabel.setText("Omega offset");

		final JPanel panel_3 = new JPanel();
		panelOmega.add(panel_3);

		textOmegaOffset = new JTextField(6);
		textOmegaOffset.setText(Float.toString((float) omegaOffset));
		panel_3.add(textOmegaOffset);

		final JLabel omegasetLabel = new JLabel();
		omegasetLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		omegasetLabel.setText("Omega (set)");
		panelOmega.add(omegasetLabel);

		final JPanel panel_5 = new JPanel();
		panelOmega.add(panel_5);

		textOmegaSet = new JTextField(6);
		panel_5.add(textOmegaSet);

		final JLabel omegarealLabel = new JLabel();
		omegarealLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		panelOmega.add(omegarealLabel);
		omegarealLabel.setText("Omega (real)");

		final JPanel panel_6 = new JPanel();
		panelOmega.add(panel_6);

		textOmegaReal = new JTextField(6);
		textOmegaReal.setEditable(false);
		panel_6.add(textOmegaReal);

		final JPanel panel_4 = new JPanel();
		panel.add(panel_4, BorderLayout.NORTH);

    JButton browseDataButton = new JButton();
		panel_4.add(browseDataButton);
		browseDataButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String DefaultDirectory;
				try {
					DefaultDirectory = FileSystemView.getFileSystemView().getDefaultDirectory().getCanonicalPath();
					String dataFileDir = ConfigData.getPropertyValue(ConfigData.DATA_FILE_DIR, DefaultDirectory);
					System.out.println(dataFileDir);
					final JFileChooser fileChooser = new JFileChooser(dataFileDir);
					fileChooser.setMultiSelectionEnabled(true);
					if (fileChooser.showOpenDialog(dataFilesPanel.this) == JFileChooser.APPROVE_OPTION) {
						ConfigData.setPropertyValue(ConfigData.DATA_FILE_DIR, fileChooser.getCurrentDirectory().getCanonicalPath());
						File[] files = fileChooser.getSelectedFiles();
						for (int nf = 0; nf < files.length; nf++) {
							String fileName = files[nf].getName();
							String filePath = files[nf].getCanonicalPath();
							HIPPODataFile datafile = new HIPPODataFile(filePath);
							dataFilesModel.addElement(datafile);
						}
						selectDataFile(dataFilesModel.getSize() - 1);
						propertyChangeSupport.firePropertyChange("DATAFILE_ADDED", null, null);
					}
				} catch (IOException IOE) {
					IOE.printStackTrace();
				}
				
				/*if (fileChooser.showOpenDialog(dataFilesPanel.this) == JFileChooser.APPROVE_OPTION) {
					try {
						File[] files = fileChooser.getSelectedFiles();
						for (int nf = 0; nf < files.length; nf++) {
							String fileName = files[nf].getName();
							String filePath = files[nf].getCanonicalPath();
							HIPPODataFile datafile = new HIPPODataFile(fileName);
							dataFilesModel.addElement(datafile);
							propertyChangeSupport.firePropertyChange("DATAFILE_ADDED", "", fileName);
						}
						dataFilesPanel.this.invalidate();
						dataFilesPanel.this.validate();
					} catch (IOException e1) {
						e1.printStackTrace();
					}
				}	*/
			}
		});
		browseDataButton.setText("Browse ...");
	}
	
	private void selectDataFile(int position) {
		if (position == -1)
      return;
    listDataFiles.setSelectedIndex(position);
		setDataFile(position);
	}
	
  private void setDataFile(int position) {
    if (position == -1)
      return;
    double omegaSet = ((HIPPODataFile)dataFilesModel.getElementAt(position)).omegaAngle;
    double omegaReal = omegaOffset + omegaSet;
    textOmegaOffset.setText(Float.toString((float) omegaOffset));
    textOmegaSet.setText(Float.toString((float) omegaSet));
    textOmegaReal.setText(Float.toString((float) omegaReal));
  }

	public void saveData() {
    if (lastSelectedData >= 0)
      try {
        ((HIPPODataFile) dataFilesModel.get(lastSelectedData)).omegaAngle = Double
            .parseDouble(textOmegaSet.getText());
        data.omegaOffset = Double.parseDouble(textOmegaOffset.getText());
      } catch (NumberFormatException nfe) {
      }
		data.sampleName = textSampleName.getText();
		data.calibrationFile = textCalibrationFile.getText();
		for (int nd = 0; nd < dataFilesModel.getSize(); nd++)
			data.dataFiles.add(dataFilesModel.getElementAt(nd));
	}

	public boolean enabledToContinue() {
    return !((textCalibrationFile.getText().trim().equals("")) || dataFilesModel.isEmpty());
  }
	

    public void addPropertyChangeListener(PropertyChangeListener p) {
      if (propertyChangeSupport == null)
        propertyChangeSupport = new PropertyChangeSupport(this);        
      propertyChangeSupport.addPropertyChangeListener(p);
    }

}
