package it.unitn.ing.wizard.LCLS2Wizard;

import ij.ImagePlus;
import ij.gui.FlatCCDReflectionSquareRoi;
import ij.process.ImageProcessor;
import it.unitn.ing.rista.awt.principalJFrame;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.cal.*;
import it.unitn.ing.rista.diffr.detector.StandardDetector;
import it.unitn.ing.rista.diffr.geometry.GeometryTransmissionFlatImage;
import it.unitn.ing.rista.diffr.instbroad.InstrumentBroadeningPVCaglioti;
import it.unitn.ing.rista.diffr.instrument.DefaultInstrument;
import it.unitn.ing.rista.diffr.measurement.TwoThetaMeasurement;
import it.unitn.ing.rista.diffr.radiation.XrayRadiation;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.wizard.*;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.WindowEvent;

public class LCLS2Wizard extends Wizard {
	private JFrame frame;
	private principalJFrame mainFrame = null;
	LCLS2data data;

	/**
	 * Create the application
	 */
	public LCLS2Wizard(principalJFrame mainFrame, LCLS2data data) {
		this.mainFrame = mainFrame;
		this.data = data;
		wizardModel = new WizardModel();
		wizardModel.addPropertyChangeListener(this);
		wizardController = new WizardController(this);
		initialize();
	}

	private void initialize() {
		frame = new JFrame();
//		frame.setLocationByPlatform(true);
		frame.getContentPane().setName("tabPane");
		frame.setResizable(true);
		frame.getContentPane().setLayout(new BorderLayout());
		frame.setSize(new Dimension(1124, 600));
		frame.setTitle("LCLS2 Wizard");
		frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

		cardPanel = new JPanel();
		cardPanel.setBorder(new EmptyBorder(new Insets(5, 10, 5, 10)));

		cardLayout = new CardLayout();
		cardPanel.setLayout(cardLayout);
		//frame.getContentPane().add(cardPanel, BorderLayout.CENTER);

		JPanel buttonPanel = new JPanel();
		JSeparator separator = new JSeparator();
		Box buttonBox = new Box(BoxLayout.X_AXIS);

		backButton = new JButton(BACK_BUTTON_ACTION_COMMAND);
		nextButton = new JButton(NEXT_BUTTON_ACTION_COMMAND);
		cancelButton = new JButton(CANCEL_BUTTON_ACTION_COMMAND);

		backButton.setActionCommand(BACK_BUTTON_ACTION_COMMAND);
		nextButton.setActionCommand(NEXT_BUTTON_ACTION_COMMAND);
		cancelButton.setActionCommand(CANCEL_BUTTON_ACTION_COMMAND);

		backButton.addActionListener(wizardController);
		nextButton.addActionListener(wizardController);
		cancelButton.addActionListener(wizardController);

		buttonPanel.setLayout(new BorderLayout());
		buttonPanel.add(separator, BorderLayout.NORTH);

		buttonBox.setBorder(new EmptyBorder(new Insets(5, 10, 5, 10)));
		buttonBox.add(backButton);
		buttonBox.add(Box.createHorizontalStrut(10));
		buttonBox.add(nextButton);
		buttonBox.add(Box.createHorizontalStrut(30));
		buttonBox.add(cancelButton);

		buttonPanel.add(buttonBox, java.awt.BorderLayout.EAST);

		frame.getContentPane().add(buttonPanel, java.awt.BorderLayout.SOUTH);
		frame.getContentPane().add(cardPanel, java.awt.BorderLayout.CENTER);

//		frame.pack();
	}

	/**
	 * Closes the dialog and sets the return code to the integer parameter.
	 *
	 * @param code The return code.
	 */
	public void close(int code) {
		returnCode = code;
		if (code == FINISH_RETURN_CODE) {
			getModel().getCurrentPanelDescriptor().aboutToHidePanel();
			setupTheAnalysis(mainFrame, data);
//			ConfigData.writeConfig();
			mainFrame.updateDataFilePlot(false);
		}


		frame.dispose();
//		System.exit(0);
	}

	public static void setupTheAnalysis(FilePar parameterfile, LCLS2data data) {
//		int max_nbkg = LCLS2ConfigData.getPropertyValue("background_parameters_to_add", 2);

		DataFileSet adataset = null;
		Sample asample = parameterfile.getActiveSample();
		int numberBackground = 3;
		if (!data.useTemplateFile) {
			parameterfile.removesample();
			parameterfile.loadingFile = true;
			if (asample == null)
				asample = parameterfile.newsample();
			asample.setLabel(data.sampleName);
			asample.initializeAsNew();
			asample.setPhaseRefinementBehaviour(2);
//		boolean original = analysis.storeSpectraWithAnalysis();
//		int numberIndividualBackground = MaudPreferences.getInteger("lcls2Wizard.numberOfIndividualBackgroundParameters", 3);
			numberBackground = LCLS2ConfigData.getPropertyValue("numberOfGeneralBackgroundParameters", 3);
		}
//		asample = parameterfile.getActiveSample();
		parameterfile.setStoreSpectraOption(true);

		int indexDataset = 0;
		System.out.println("Data panels: " + data.panels.size());
		for (int i = 0; i < data.panels.size(); i++) {
			Cspad panel = data.panels.elementAt(i);
			System.out.println("Panel: " + i + " " + panel.enabled);
			if (panel.enabled) {
				System.out.println("Sensors: " + panel.detectors.size());
				for (int j = 0; j < panel.detectors.size(); j++) {
					SensorImage sensor = panel.detectors.elementAt(j);
					System.out.println("Sensor: " + j + " " + sensor.enabled);
					if (sensor.enabled) {
						if (!data.useTemplateFile) {
							if (i == 0 && j == 0)
								adataset = asample.getDataSet(0);
							else
								adataset = asample.newData(numberBackground);
							adataset.initializeAsNew();
							adataset.setLabel(sensor.name);
							adataset.setInstrument(DefaultInstrument.modelID);
							Instrument inst = adataset.getInstrument();
							inst.setDetector(StandardDetector.modelID);
							inst.setGeometry(GeometryTransmissionFlatImage.modelID);
							inst.setInstrumentID("LCLS MEC XFEL");
							inst.setMeasurement(TwoThetaMeasurement.modelID);
							inst.setRadiationType(XrayRadiation.modelID);
							inst.getRadiationType().addRadiation("XFEL");
							inst.getRadiationType().getRadiation(0).getWavelength().setValue(data.wavelength);

							inst.setAngularCalibration(AngularInclinedFlatImageCalibration.modelID);
							AngularInclinedFlatImageCalibration angcal =
									(AngularInclinedFlatImageCalibration) adataset.getInstrument().getAngularCalibration();
							angcal.setOriginal2Theta(sensor.theta2);
							angcal.setOriginalCenterX(sensor.centerX);
							angcal.setOriginalCenterY(sensor.centerY);
							angcal.setOriginalDistance(sensor.distance);
							angcal.setOriginalEtaDA(sensor.etaDA);
							angcal.setOriginalOmegaDN(sensor.omegaDN);
							angcal.setOriginalPhiDA(sensor.phiDA);
							angcal.setDetector2Theta(sensor.theta2);
							angcal.setDetectorCenterX(0);
							angcal.setDetectorCenterY(0);
							angcal.setDetectorDistance(sensor.distance);
							angcal.setDetectorEtaDA(sensor.etaDA);
							angcal.setDetectorOmegaDN(sensor.omegaDN);
							angcal.setDetectorPhiDA(sensor.phiDA);

							parameterfile.loadingFile = false;
							inst.setInstrumentBroadening(InstrumentBroadeningPVCaglioti.modelID);
							InstrumentBroadeningPVCaglioti instBroad = (InstrumentBroadeningPVCaglioti) inst.getInstrumentBroadening();
							instBroad.initializeAsNew();
							for (int ia = 0; ia < instBroad.getasymmetrynumber(); ia++)
								instBroad.getAsymmetry(ia).setValue(0);
						} else {
							if (asample != null && asample.datasetsNumber() > indexDataset)
								adataset = asample.getDataSet(indexDataset++);
						}

						String filename = LCLS2ConfigData.filenameToSave;
						int dotLocation = filename.lastIndexOf(".");
						if (dotLocation >=0 )
							filename = filename.substring(0, dotLocation);
						filename += sensor.name;

						System.out.flush();

						if (adataset.activedatafilesnumber() > 0)
							adataset.removeAllFiles();

						integrateImage(sensor, adataset, data, filename + ".esg");

						adataset.removeAllDisabledFiles();

						parameterfile.loadingFile = true;
					}

				}
			}
		}

		parameterfile.loadingFile = false;
//		analysis.setStoreSpectraOption(original);
		parameterfile.refreshAll(false);
	}

	public static void setupTheAnalysis(principalJFrame mainFrame, LCLS2data data) {
//		int max_nbkg = LCLS2ConfigData.getPropertyValue("background_parameters_to_add", 2);

		if (data.useTemplateFile)  {
			String[] folderAndName = Misc.getFolderandName(LCLS2ConfigData.filenameTemplate);
			java.io.Reader in = null;
			try {
				if (mainFrame.parameterfile != null) {
					mainFrame.parameterfile.dispose(); // If we don't call this no finalization will occur!!
					mainFrame.parameterfile = null;
				}
				mainFrame.parameterfile = new FilePar(folderAndName[1], mainFrame);
				in = Misc.getReader(folderAndName[0], folderAndName[1]);
				mainFrame.parameterfile.readall(in, null);
				mainFrame.parameterfile.setFileName("noname.par", false);
				mainFrame.parameterfile.setDirectory(folderAndName[0]);
				if (mainFrame.titleField != null) {
					mainFrame.titleField.setText(mainFrame.parameterfile.getTitleField());
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		} else {
			if (mainFrame.parameterfile != null) {
				mainFrame.parameterfile.dispose(); // If we don't call this no finalization will occur!!
				mainFrame.parameterfile = null;
			}

			// check file existing
			String[] folderAndName = new String[2];
			folderAndName[0] = Constants.documentsDirectory;
			folderAndName[1] = "noname.par";

			mainFrame.parameterfile = new FilePar(folderAndName[1], mainFrame);
			mainFrame.parameterfile.setDirectory(folderAndName[0]);

		}

		setupTheAnalysis(mainFrame.parameterfile, data);
		mainFrame.initParameters();

	}

	public void createNextPanel(Object identifier) {
/*		if (ImageDataPanelDescriptor.IDENTIFIER.equals(identifier.toString())) {
			WizardPanelDescriptor descriptor2 = new ImageDataPanelDescriptor(data);
			registerWizardPanel(ImageDataPanelDescriptor.IDENTIFIER, descriptor2);
		}*/
	}

	/**
	 * If the user presses the close box on the dialog's window, treat it
	 * as a cancel.
	 *
	 * @param e The event passed in from AWT.
	 */

	public void windowClosing(WindowEvent e) {
		returnCode = CANCEL_RETURN_CODE;
	}

	public static void startWizard(principalJFrame mainFrame) {

		LCLS2data data = new LCLS2data("analysis x");
		LCLS2Wizard wizard = new LCLS2Wizard(mainFrame, data);

		WizardPanelDescriptor descriptor1 = new ImageDataPanelDescriptor(data);
		wizard.registerWizardPanel(ImageDataPanelDescriptor.IDENTIFIER, descriptor1);

		wizard.setCurrentPanel(ImageDataPanelDescriptor.IDENTIFIER);

		wizard.frame.setVisible(true);

	}

	public static boolean integrateImage(SensorImage sensor, DataFileSet dataset, LCLS2data data, String filename) {

		String[] folderAndName = Misc.getFolderandName(filename);
		String name = folderAndName[1];
		String directory = folderAndName[0];

		AngularInclinedFlatImageCalibration angcal =
				(AngularInclinedFlatImageCalibration) dataset.getInstrument().getAngularCalibration();

		ImagePlus imp = sensor.getImagePlus();

		boolean loadSuccessfull = false;

		ImageProcessor ip = imp.getChannelProcessor();

		int width = ip.getWidth();
		int height = ip.getHeight();
/*		double[] buffer = new double[width * height];
		for (int ix = 0; ix < width; ix++)
			for (int iy = 0; iy < height; iy++)
				buffer[ix + iy * width] = ip.getPixelValue(ix, iy);*/

		double azimuthal = angcal.getOriginalPhiDA();
		double phiDetector = angcal.getOriginalOmegaDN();
		double coneAngle = angcal.getOriginalEtaDA();
		double detector2Theta = angcal.getOriginal2Theta();
		double detectorDistance = angcal.getOriginalDistance();
		double centerX = angcal.getOriginalCenterX();
		double centerY = angcal.getOriginalCenterY();
		double coneInterval = data.coneInterval;
		double step2theta = data.step2theta;
		double omega = data.omega;
		double chi = data.chi;
		double phi = data.phi;

/*		int minX = MaudPreferences.getInteger("squareRoi.xminValue", 0);
		int maxX = Math.min(MaudPreferences.getInteger("squareRoi.xmaxValue", width), width);
		int minY = MaudPreferences.getInteger("squareRoi.yminValue", 0);
		int maxY = Math.min(MaudPreferences.getInteger("squareRoi.ymaxValue", height), height);*/
//    System.out.println("Using min max " + minX + " " + maxX + " " + minY + " " + maxY);
		double[] intensity = new double[width * height];
		double[] x = new double[width * height];
		double[] y = new double[width * height];
		int index = 0;
		for (int ix = 0; ix < width; ix++)
			for (int iy = 0; iy < height; iy++)
				intensity[index++] = ip.getPixelValue(ix, iy);

		ij.measure.Calibration cal = imp.getCalibration();
		FlatCCDReflectionSquareRoi.getXYFromPixelIndex(0, width, 0, height, cal.pixelWidth, cal.pixelHeight,
				x, y, centerX, centerY);

		double[] theta2 = new double[width * height];
		double[] eta = new double[width * height];
		Angles.getTheta2EtaFromXYPixelDetector(x, y, theta2, eta, omega,
				detector2Theta, azimuthal, phiDetector, coneAngle, detectorDistance, 0);

		double min2theta = 2.0 * Math.PI;
		double max2theta = -2 * Math.PI;
		double mineta = 2 * Math.PI;
		double maxeta = -2 * Math.PI;
		for (int i = 0; i < theta2.length; i++) {
			if (min2theta > theta2[i])
				min2theta = theta2[i];
			if (max2theta < theta2[i])
				max2theta = theta2[i];
			if (mineta > eta[i])
				mineta = eta[i];
			if (maxeta < eta[i])
				maxeta = eta[i];
		}
		double nmineta = 0.0;
		int i = 0;
		while (nmineta < mineta)
			nmineta = i++ * coneInterval * Constants.DEGTOPI;
		while (nmineta >= mineta + coneInterval * Constants.DEGTOPI)
			nmineta = i-- * coneInterval * Constants.DEGTOPI;
		mineta = nmineta;
		double nmintheta = 0.0;
		i = 0;
		while (nmintheta < min2theta)
			nmintheta = i++ * step2theta * Constants.DEGTOPI;
		while (nmintheta >= min2theta + step2theta * Constants.DEGTOPI)
			nmintheta = i-- * step2theta * Constants.DEGTOPI;
		min2theta = nmintheta;

		double[][][] profile = Angles.spectraFromPixelsByEtaTheta2(theta2, eta, intensity, x, y, detectorDistance,
				min2theta, max2theta, step2theta * Constants.DEGTOPI,
				mineta, maxeta, coneInterval * Constants.DEGTOPI);

		double xmin = min2theta * Constants.PITODEG;
		double etaStart = mineta * Constants.PITODEG;
		if (!MaudPreferences.getBoolean("imageUnrolling.saveEsgFileInCachesDir", false)) {
			System.out.println("Conversion to spectra done! Name to save: " + filename);
			FlatCCDReflectionSquareRoi.saveAsText(profile, profile[0].length, 0, profile[0][0].length, xmin, step2theta,
					etaStart, coneInterval, directory, name, "mm", detectorDistance, omega, chi, phi, detector2Theta,
					true);
		}
		for (int spectrumIndex = 0; spectrumIndex < profile[0].length; spectrumIndex++) {
			String numberString = Integer.toString(spectrumIndex);
			DiffrDataFile datafile = new DiffrDataFile(dataset, name + "(" + numberString + ")");
			dataset.addsubordinateloopField(2, datafile);
			boolean atmpB = datafile.isAbilitatetoRefresh;
			datafile.isAbilitatetoRefresh = false;

			datafile.setDataType(datafile.DIFFRACTION_IMAGE);
			datafile.setAngleValue(0, omega);
			datafile.setAngleValue(1, chi);
			datafile.setAngleValue(2, phi);
			datafile.setAngleValue(3, etaStart + spectrumIndex * coneInterval);
			datafile.setAngleValue(4, 0.0);

/*            datafile.setField("_riet_meas_datafile_calibrated", "true", "0", "0", "0", false, null, null, null, null,
                false);
*/
			int datanumber = 0;
			i = 0;
			while (i < profile[2][spectrumIndex].length)
				if (profile[2][spectrumIndex][i++] >= 0)
					datanumber++;
			datafile.datanumber = datanumber;
//            System.out.println("Check this: " + i + " =? " + datafile.datanumber);
			if (datanumber < 3)
				datafile.setCompute(false);
			datafile.initData(datanumber);
			datafile.constantstep = false;
			datafile.dspacingbase = false;

			i = 0;
			int indexPoint = 0;
			while (i < profile[2][spectrumIndex].length) {
				double intensityValue = profile[2][spectrumIndex][i];
				if (intensityValue >= 0) {
					datafile.setXData(indexPoint, indexPoint);
					datafile.setXImage(indexPoint, profile[0][spectrumIndex][i]);
					datafile.setYImage(indexPoint, profile[1][spectrumIndex][i]);
					datafile.setYData(indexPoint, intensityValue);
					double tmpweight = Math.sqrt(datafile.getYData(indexPoint));
					if (tmpweight != 0.0)
						datafile.setWeight(indexPoint, 1.0 / tmpweight);
					else
						datafile.setWeight(indexPoint, 1.0);
					indexPoint++;
				}
				i++;
			}
			datafile.isAbilitatetoRefresh = atmpB;
			loadSuccessfull = true;
			datafile.dataLoaded = true;
		}
		return loadSuccessfull;
	}

}
