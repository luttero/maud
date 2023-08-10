package it.unitn.ing.wizard.HIPPOWizard;

import it.unitn.ing.rista.diffr.cal.*;
import it.unitn.ing.rista.util.MaudPreferences;
import it.unitn.ing.rista.util.Misc;
import it.unitn.ing.wizard.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.radiation.TOFNeutronRadiation;
import it.unitn.ing.rista.diffr.measurement.TOFMeasurement;
import it.unitn.ing.rista.diffr.geometry.GeometryIPNS_LANSCE;
import it.unitn.ing.rista.diffr.detector.TOFDetector;
import it.unitn.ing.rista.diffr.instbroad.InstrumentBroadeningGSAS1f;
import it.unitn.ing.rista.diffr.instrument.DefaultInstrument;
import it.unitn.ing.rista.awt.DiffractionMainFrame;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.IOException;
import java.util.*;

import javax.swing.*;
import javax.swing.border.EmptyBorder;

public class HIPPOWizard extends Wizard {

  private JFrame frame;
  private FilePar analysis = null;
  HIPPOdata data;

  /**
   * Create the application
   */
  public HIPPOWizard(FilePar parameters, HIPPOdata data) {
    analysis = parameters;
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
    frame.setSize(new Dimension(560, 450));
    frame.setTitle("HIPPO Wizard");
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
      setupTheAnalysis(analysis, data);
      ConfigData.writeConfig();
      analysis.getMainFrame().updateDataFilePlot(false);
    }


    frame.dispose();
//		System.exit(0);
  }

  public static void setupTheAnalysis(FilePar analysis, HIPPOdata data) {
//	  int max_nbkg = MaudPreferences.getInteger("hippoWizard.background_parameters_to_add", 2);
	  analysis.removesample();
    analysis.loadingFile = true;
    Sample asample;
    DataFileSet adataset;
    if ((asample = analysis.getActiveSample()) == null)
      asample = analysis.newsample();
    asample.setLabel(data.sampleName);
    asample.initializeAsNew();
	  asample.setPhaseRefinementBehaviour(2);
    boolean original = analysis.storeSpectraWithAnalysis();
	  double maxBankToleranceTheta = MaudPreferences.getDouble("hippoWizard.maxDelta2ThetaForBankGrouping", 2.0);
	  int numberIndividualBackground = MaudPreferences.getInteger("hippoWizard.numberOfIndividualBackgroundParameters", 3);
	  int numberBackground = MaudPreferences.getInteger("hippoWizard.numberOfGeneralBackgroundParameters", 0);
		boolean useCebyshev = MaudPreferences.getBoolean("hippoWizard.useChebyshevPolynomials", false);
    analysis.setStoreSpectraOption(false);
    if (data.groupDatasetsByRotation) {
      for (int i = 0; i < data.mbank.size(); i++) {
        if (((HIPPOBank) data.mbank.elementAt(i)).enabled) {
          for (int k = 0; k < data.dataFiles.size(); k++) {
            double omega = ((HIPPODataFile) data.dataFiles.get(k)).omegaAngle +
                data.omegaOffset;
            if (i != 0 || k != 0) {
	            adataset = asample.newData(numberBackground);
            } else {
	            adataset = asample.getDataSet(0);
	            adataset.removeAllBackgroundCoeff();
	            for (int n = 0; n < numberBackground; n++) {
		            adataset.addBackgroudCoeff();
	            }
            }
            adataset.initializeAsNew();
            adataset.setLabel(((HIPPOBank) data.mbank.elementAt(i)).name + " omega " + ((float) omega));
            adataset.setInstrument(DefaultInstrument.modelID);
				adataset.useChebyshevPolynomials(useCebyshev);
            Instrument inst = adataset.getInstrument();
            inst.setDetector(TOFDetector.modelID);
            inst.setGeometry(GeometryIPNS_LANSCE.modelID);
            inst.setInstrumentID("LANSCE Hippo spectrometer");
            inst.setMeasurement(TOFMeasurement.modelID);
            inst.setRadiationType(TOFNeutronRadiation.modelID);

	        int maxBanksNumber = 0;

            inst.setAngularCalibration(MultiBankCalibration.modelID);
            MultiBankCalibration instAngCal = (MultiBankCalibration) inst.getAngularCalibration();
            instAngCal.initializeAsNew();
            instAngCal.setFileName(data.calibrationFile);
//            instAngCal.readall();

            inst.setInstrumentBroadening(InstrumentBroadeningGSAS1f.modelID);
            InstrumentBroadeningGSAS1f instBroad = (InstrumentBroadeningGSAS1f) inst.getInstrumentBroadening();
            instBroad.initializeAsNew();
            instBroad.setFileName(data.calibrationFile);
//            instBroad.readall();

	          inst.setIntensityCalibration(HippoMultBankIntCalibration.modelID);
	          HippoMultBankIntCalibration instIntCal = (HippoMultBankIntCalibration) inst.getIntensityCalibration();
	          instIntCal.initializeAsNew();
	          instIntCal.setFileName(data.calibrationFile);
//            instIntCal.readall();

            int startingBank = ((HIPPOBank) data.mbank.elementAt(i)).startBank;
            int endingBank = data.BANK_NUMBER;
            if (i + 1 < data.mbank.size())
              endingBank = ((HIPPOBank) data.mbank.elementAt(i + 1)).startBank;
            if (instBroad.banknumbers() + 1 < endingBank)
              endingBank = instBroad.banknumbers() + 1;
            adataset.setMinRange(((HIPPOBank) data.mbank.elementAt(i)).dSpacingMin);
            adataset.setMaxRange(((HIPPOBank) data.mbank.elementAt(i)).dSpacingMax);
	          analysis.loadingFile = false;
            for (int j = startingBank; j < endingBank; j++) {
              int actualdatanumber = adataset.datafilesnumber();
              String name = data.dataFiles.get(k).toString() + "(" + Integer.toString(j) + ")";
//              Misc.println("Add: " + name);
              adataset.addDataFileforName(name, false);

//              adataset.refreshAll(true);
              int newdatanumber = adataset.datafilesnumber();
              if (newdatanumber > actualdatanumber) {
                DiffrDataFile adatafile = adataset.getDataFile(actualdatanumber);
	              adatafile.useChebyshevPolynomials(useCebyshev);

	              adatafile.setAngleValue(0, -omega);
	              for (int bj = 0; bj < numberIndividualBackground; bj++)
                  adatafile.addBackgroundParameter();
              }
            }

            int lastLoadedBank = instBroad.banknumbers();
	          analysis.loadingFile = true;

/*            for (int j = lastLoadedBank; j >= endingBank; j--) {
//            Misc.println("Remove " + j);
              instBroad.removeBank(j);
              instIntCal.removeBank(j);
              instAngCal.removeBank(j);
            }
            for (int j = startingBank - 1; j > 0; j--) {
//            Misc.println("Remove " + j);
              instBroad.removeBank(j);
              instIntCal.removeBank(j);
              instAngCal.removeBank(j);
            }*/
          }
        }
      }
    } else {
      for (int i = 0; i < data.mbank.size(); i++) {
        if (((HIPPOBank) data.mbank.elementAt(i)).enabled) {
          if (i != 0)
            adataset = asample.newData(numberBackground);
          else {
	          adataset = asample.getDataSet(0);
	          adataset.removeAllBackgroundCoeff();
	          for (int n = 0; n < numberBackground; n++) {
		          adataset.addBackgroudCoeff();
	          }
          }
          adataset.initializeAsNew();
          adataset.setLabel(((HIPPOBank) data.mbank.elementAt(i)).name);
          adataset.setInstrument(DefaultInstrument.modelID);
	        adataset.useChebyshevPolynomials(useCebyshev);
          Instrument inst = adataset.getInstrument();
          inst.setDetector(TOFDetector.modelID);
          inst.setGeometry(GeometryIPNS_LANSCE.modelID);
          inst.setInstrumentID("LANSCE Hippo spectrometer");
          inst.setMeasurement(TOFMeasurement.modelID);
          inst.setRadiationType(TOFNeutronRadiation.modelID);

	        int maxBanksNumber = 0;

	        inst.setAngularCalibration(MultiBankCalibration.modelID);
	        MultiBankCalibration instAngCal = (MultiBankCalibration) inst.getAngularCalibration();
	        instAngCal.initializeAsNew();
	        instAngCal.setFileName(data.calibrationFile);
//            instAngCal.readall();

	        inst.setInstrumentBroadening(InstrumentBroadeningGSAS1f.modelID);
          InstrumentBroadeningGSAS1f instBroad = (InstrumentBroadeningGSAS1f) inst.getInstrumentBroadening();
          instBroad.initializeAsNew();
          instBroad.setFileName(data.calibrationFile);
//            instBroad.readall();

          inst.setIntensityCalibration(HippoMultBankIntCalibration.modelID);
          HippoMultBankIntCalibration instIntCal = (HippoMultBankIntCalibration) inst.getIntensityCalibration();
          instIntCal.initializeAsNew();
          instIntCal.setFileName(data.calibrationFile);

	        // we remove the unneeded banks
	        double bank2theta = ((HIPPOBank) data.mbank.elementAt(i)).theta2;
	        for (int k = instAngCal.banknumbers() - 1; k >= 0; k--) {
//		        System.out.println(bank2theta + " == " + instAngCal.getTtheta(k).getValueD());
		        if (Math.abs(bank2theta - instAngCal.getTtheta(k).getValueD()) > maxBankToleranceTheta)
			        instAngCal.removeBank(k+1);
	        }
	        for (int k = instAngCal.banknumbers() - 1; k >= 0; k--) {
		        boolean found = false;
		        for (int l = 0; l < instBroad.banknumbers(); l++) {
			        if (instAngCal.getBankID(k).equalsIgnoreCase(instBroad.getBankID(l)))
				        found = true;
		        }
		        if (!found) {
			        instAngCal.removeBank(k+1);
		        }
	        }
	        for (int k = instAngCal.banknumbers() - 1; k >= 0; k--) {
		        boolean found = false;
		        for (int l = 0; l < instIntCal.banknumbers(); l++)
			        if (instAngCal.getBankID(k).equalsIgnoreCase(instIntCal.getBankID(l)))
				        found = true;
		        if (!found)
			        instAngCal.removeBank(k+1);
	        }
	        for (int k = instBroad.banknumbers() - 1; k >= 0; k--) {  // we check now also the reverse is true
		        boolean found = false;
		        for (int l = 0; l < instAngCal.banknumbers(); l++)
			        if (instAngCal.getBankID(l).equalsIgnoreCase(instBroad.getBankID(k)))
				        found = true;
		        if (!found)
			        instBroad.removeBank(k+1);
	        }
	        for (int k = instIntCal.banknumbers() - 1; k >= 0; k--) {  // we check now also the reverse is true
		        boolean found = false;
		        for (int l = 0; l < instAngCal.banknumbers(); l++)
			        if (instAngCal.getBankID(l).equalsIgnoreCase(instIntCal.getBankID(k)))
				        found = true;
		        if (!found)
			        instIntCal.removeBank(k+1);
	        }

	        maxBanksNumber = instAngCal.banknumbers();

          adataset.setMinRange(((HIPPOBank) data.mbank.elementAt(i)).dSpacingMin);
          adataset.setMaxRange(((HIPPOBank) data.mbank.elementAt(i)).dSpacingMax);
	        analysis.loadingFile = false;
	        for (int k = 0; k < data.dataFiles.size(); k++) {
          	HashMap<String, Vector<int[]>> datafilesNumberForBank = getDatafilesNumbersForBank(data.dataFiles.get(k).toString());
            for (int j = 0; j < maxBanksNumber; j++) {
	            try {
		            Vector<int[]> bankNumber = datafilesNumberForBank.get(instAngCal.getBankID(j));
		            if (bankNumber != null) {
			            for (int kj = 0; kj < bankNumber.size(); kj++) {
				            String name = data.dataFiles.get(k).toString() + "(" + Integer.toString(bankNumber.elementAt(kj)[0]) + ")";
//				            System.out.println("Add: " + name + " , to: " + adataset.getLabel() + " for: " + instAngCal.getBankID(j));
				            DiffrDataFile[] adatafile = adataset.addDataFileforName(name, false);
				            for (int ij = 0; ij < adatafile.length; ij++) {
					            adatafile[ij].setAngleValue(0, -Double.parseDouble(adatafile[ij].getString(1)) -
							            ((HIPPODataFile) data.dataFiles.get(k)).omegaAngle -
							            data.omegaOffset);
					            adatafile[ij].useChebyshevPolynomials(useCebyshev);
					            for (int nbkg = 0; nbkg < numberIndividualBackground; nbkg++) {
						            adatafile[ij].addBackgroundParameter();
//						            adatafile[ij].addBackgroundParameter();
					            }
//					            System.out.println(adatafile[ij].toString() + " " + adatafile[ij].getBankID());
				            }
			            }
		            }
	            } catch (Exception e) {
		            e.printStackTrace();
	            }

            }
          }
	        analysis.loadingFile = true;

//	        adataset.refreshAll(true);

	        for (int j = instAngCal.banknumbers() - 1; j >= 0; j--) {
		        String bankID = instAngCal.getBankID(j);
		        boolean present = false;
		        for (int k = 0; k < adataset.datafilesnumber(); k++) {
							if (adataset.getDataFile(k).getBankID().equalsIgnoreCase(bankID))
								present = true;
		        }
		        if (!present) {
			        instAngCal.removeBank(bankID);
			        instBroad.removeBank(bankID);
			        instIntCal.removeBank(bankID);
		        }
	        }

        }
      }
    }

    analysis.loadingFile = false;
    analysis.setStoreSpectraOption(original);
    analysis.refreshAll(false);
    analysis.refineAllTOFSFBankCoefficients();
    if (data.groupDatasetsByRotation) {
      int setIndex = 0;
      int refIndex = 0;
      for (int i = 0; i < data.mbank.size(); i++) {
        if (((HIPPOBank) data.mbank.elementAt(i)).enabled) {
          refIndex = setIndex;
          for (int k = 0; k < data.dataFiles.size(); k++) {
            if (refIndex != setIndex)
              asample.getDataSet(setIndex).getInstrument().setEqualTo(asample.getDataSet(refIndex).getInstrument(),
                  true);
            asample.getDataSet(setIndex).getInstrument().forceAllBankIntRefinable();
            setIndex++;
          }

        }
      }
    } else {
    }
    int numberDataSets = asample.datasetsNumber();
    for (int i = 0; i < numberDataSets; i++)
      asample.getDataSet(i).forceRangeCut();
    analysis.refineAllZEROBankCoefficients(true);
    analysis.freeAllBackgroundParameters();
  }

	public static HashMap<String, Vector<int[]>> getDatafilesNumbersForBank(String datafileName) {
		HashMap<String, Vector<int[]>> map = new HashMap<String, Vector<int[]>>();
		BufferedReader reader = Misc.getReader(datafileName);
		if (reader != null) {
			try {
				String token = null;
				StringTokenizer st;
				boolean endoffile = false;

				int index = 1;
				int bankNumber = 0;
				String linedata = reader.readLine();
				while (linedata != null) {
					while (linedata != null && !linedata.startsWith("BANK")) {
						linedata = reader.readLine();
					}
					if (linedata != null && linedata.startsWith("BANK")) {
						st = new StringTokenizer(linedata.substring(4), " ,\t\r\n");
						token = st.nextToken();
						bankNumber = Integer.parseInt(token);
						String bankID = GSASbankCalibration.bankPrefix + Integer.toString(bankNumber);
						Vector<int[]> spectraNumbers = map.get(bankID);
						if (spectraNumbers == null) {
							spectraNumbers = new Vector<int[]>(10, 10);
							map.put(bankID, spectraNumbers);
						}
						int[] numb = new int[1];
						numb[0] = index;
						spectraNumbers.add(numb);
						index++;
					}
					linedata = reader.readLine();
				}
			} catch (Exception e) {
				e.printStackTrace();
//				System.out.println("Error in loading the data file! Try to remove this data file");
			}
			try {
				reader.close();
			} catch (IOException e) {
			}
		}
		return map;
	}

	public void createNextPanel(Object identifier) {
    if (detectorPanelDescriptor.IDENTIFIER.equals(identifier.toString())) {
      WizardPanelDescriptor descriptor2 = new detectorPanelDescriptor(data);
      registerWizardPanel(detectorPanelDescriptor.IDENTIFIER, descriptor2);
    }
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

  public static void startWizard(FilePar analysis) {

    HIPPOdata data = new HIPPOdata("analysis x");
    HIPPOWizard wizard = new HIPPOWizard(analysis, data);

    WizardPanelDescriptor descriptor1 = new dataFilesPanelDescriptor(data);
    wizard.registerWizardPanel(dataFilesPanelDescriptor.IDENTIFIER, descriptor1);

    WizardPanelDescriptor descriptor2 = new detectorPanelDescriptor(data);
    wizard.registerWizardPanel(detectorPanelDescriptor.IDENTIFIER, descriptor2);

    wizard.setCurrentPanel(dataFilesPanelDescriptor.IDENTIFIER);

    wizard.frame.setVisible(true);

  }

}
