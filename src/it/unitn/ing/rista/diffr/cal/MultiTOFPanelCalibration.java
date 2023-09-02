/*
 * @(#)TOFPanelCalibration.java created 8/29/2023 Los Alamos
 *
 * Copyright (c) 2099 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.diffr.cal;

		import it.unitn.ing.rista.diffr.*;

		import java.lang.*;

		import it.unitn.ing.rista.diffr.rta.StandardFunctionTexture;
		import it.unitn.ing.rista.util.*;
		import it.unitn.ing.rista.awt.*;

		import java.awt.*;
		import javax.swing.*;
		import javax.swing.border.BevelBorder;
		import javax.swing.border.TitledBorder;

/**
 *  The MultiTOFPanelCalibration is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2023/29/8 16:43:04 $
 * @author Luca Lutterotti
 * @since JDK19
 */

// Constants.LAMBDA_SPEED_NEUTRON_CONV_ANG

public class MultiTOFPanelCalibration extends AngularCalibration {

	public static String[] diclistc = {
			"_instrument_parameter_file",

			"_instrument_neutron_flight_path",

			"_instrument_bank_ID"
	};
	public static String[] diclistcrm = {
			"_instrument_parameter_file",

			"_instrument_neutron_flight_path",

			"_instrument_bank_ID"
	};

	public static String[] classlistc = {"it.unitn.ing.rista.diffr.cal.TOFPanelCalibration"};
	public static String[] classlistcs = {};

	boolean refreshCalibration = true;

	double flightPath = 9000.0;
	int FLIGHT_PATH_ID = 0;

//	public static final String panelPrefix = "MultiTOFPanel";
	public static String modelID = "Multi TOF 2D banks";

	public MultiTOFPanelCalibration(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initBaseObject();
		identifier = modelID;
		IDlabel = modelID;
	}

	public MultiTOFPanelCalibration(XRDcat aobj) {
		this(aobj, "");
	}

	public MultiTOFPanelCalibration() {
		identifier = modelID;
		IDlabel = modelID;
	}

	public void initConstant() {
		Nstring = 1;
		Nstringloop = 0;
		Nparameter = 1;
		Nparameterloop = 0;
		Nsubordinate = 0;
		Nsubordinateloop = 1;
	}

	public void initDictionary() {
		for (int i = 0; i < totsubordinateloop; i++)
			diclist[i] = diclistc[i];
		System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
		for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
			classlist[i] = classlistc[i];
		for (int i = 0; i < totsubordinate - totparameterloop; i++)
			classlists[i] = classlistcs[i];
	}

	public void initParameters() {
		super.initParameters();
		setFileName("");
		double flightP = MaudPreferences.getDouble("TOFbank2D.flightPathToSample_m", 9.07617);
		parameterField[FLIGHT_PATH_ID] = new Parameter(this, getParameterString(0), flightP,
				ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.0),
				ParameterPreferences.getDouble(getParameterString(0) + ".max", 100.0));

		refreshComputation = true;
	}

	public void updateParametertoDoubleBuffering(boolean firstLoading) {
		if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
			return;
		isAbilitatetoRefresh = false;
		int banks = banknumbers();
		flightPath = getParameterValue(FLIGHT_PATH_ID);
		isAbilitatetoRefresh = true;
		super.updateParametertoDoubleBuffering(firstLoading);
	}

	public void notifyParameterChanged(Parameter source) {
		FilePar filepar = getFilePar();
		if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
			if (source == getParameter(0)) {
				notifyParameterChanged(source, Constants.ANGULAR_CALIBRATION, -1);
				return;
			}
			super.notifyParameterChanged(source);
		}
	}

	public boolean freeAllBasicParameters() {
		parameterField[0].setRefinableCheckBound();
		int banks = banknumbers();
		for (int bank = 0; bank < banks; bank++) {
			if (bankIsActive(bank)) {
				getBank(bank).freeAllBasicParameters();
			}
		}
		return true;
	}

	protected boolean bankIsActive(int bank) {
		DataFileSet data = (DataFileSet) getParent().getParent();
		int datafiles = data.activedatafilesnumber();
		boolean isActive = false;
		for (int i = 0; i < datafiles && !isActive; i++) {
			if (data.getActiveDataFile(i).getAngBankNumber() == bank)
				isActive = true;
		}
		return isActive;
	}
/*
	public void removePanel(int indexToRemove) {
		removePanel(panelPrefix + Integer.toString(indexToRemove));
	}

	public void removePanel(String bankID) {
		try {
			int indexToRemove = getBankNumber(bankID);
//    System.out.println("Ang, removing " + bankID + " " + indexToRemove);
			boolean isAbilitate = isAbilitatetoRefresh;
			isAbilitatetoRefresh = false;
			for (int i = 0; i < stringloopField.length; i++)
				stringloopField[i].removeItemAt(indexToRemove);
			for (int i = 0; i < parameterloopField.length; i++)
				parameterloopField[i].removeItemAt(indexToRemove);
			isAbilitatetoRefresh = isAbilitate;
			notifyUpObjectChanged(this, Constants.ANGULAR_CALIBRATION, -1);
		} catch (Exception e) {
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}
*/
	public double getFlightPath() {
		return flightPath;
	}
	public double refreshAndGetFlightPath() {
		flightPath = getParameterValue(FLIGHT_PATH_ID);
		return flightPath;
	}


	public double getDetectorDistanceValue(DiffrDataFile datafile) {
		return getBank(datafile.getAngBankNumber()).getDetectorDistanceValue();
	}

	public void setFileName(String filename) {
		stringField[0] = filename;
		if (getFileName() != null && !getFileName().equals(""))
			readall();
	}

	public String getFileName() {
		return stringField[0];
	}

	public int banknumbers() {
		return numberofelementSubL(0);
	}

	public TOFPanelCalibration getBank(int index) {
		return (TOFPanelCalibration) subordinateloopField[0].elementAt(index);
	}

	public String getBankID(int index) {
		return getBank(index).getBankID();
	}

	public int getBankNumber(String bankID) {
		for (int i = 0; i < banknumbers(); i++)
			if (getBankID(i).equalsIgnoreCase(bankID))
				return i;
		return 0;
	}

	public void printBank() {
		for (int i = 0; i < banknumbers(); i++)
			System.out.println(getBankID(i));
	}

/*	public String loadDataFile(Frame parent) {
		String filename = Utility.openFileDialog(parent, "Import panel/bank calibration file",
				FileDialog.LOAD, getDirectory(), null, null);
		if (filename != null) {
			parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
//      relativePathChange = false;
			setFileName(filename);
			parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
		return filename;
	}*/

/*	public void readall() {
		refreshCalibration = true;
		boolean isAbilitate = isAbilitatetoRefresh;
		isAbilitatetoRefresh = false;
		BufferedReader reader = Misc.getReader(getFileName());
		if (reader != null) {
			try {
				String token = new String("");
				StringTokenizer st = null;
				String linedata = null;
				boolean endoffile = false;
				boolean found = false;
				int banknumber = 0;
				resetParameterToZero();

				while (!endoffile) {
					linedata = reader.readLine();
					if (linedata == null) {
						endoffile = true;
						break;
					}

					if (linedata.toUpperCase().startsWith("INS")) {
						linedata = "IN " + linedata.substring(3);
						String bankString = Misc.toStringDeleteBlank(linedata.substring(3, 6));
						if (bankString.length() > 0)
							banknumber = Integer.valueOf(bankString).intValue();
						st = new StringTokenizer(linedata, " ,\t\r\n");

						while (st.hasMoreTokens()) {
							token = st.nextToken();
							if (token.equalsIgnoreCase("FPATH1")) {
								stringField[2] = st.nextToken();
//	              System.out.println("FPATH1(" + banknumber + "): " + stringField[2]);
							} else if (token.equalsIgnoreCase("ICONS")) {
//              	 System.out.println(this + ", Adding bank number: " + banknumber);
								addBank(panelPrefix + bankString);
								addDifc(banknumber, token = st.nextToken());
								addDifa(banknumber, token = st.nextToken());
								addZero(banknumber, token = st.nextToken());
							} else if (token.toLowerCase().indexOf("bnkpar") != -1) {
								int j = token.toLowerCase().indexOf("bnkpar");
								addDetectorDistance(banknumber, token = st.nextToken());
								addTtheta(banknumber, token = st.nextToken());
							} else if (token.toLowerCase().indexOf("detazm") != -1) {
								int j = token.toLowerCase().indexOf("detazm");
								token = st.nextToken();
								double eta = Double.parseDouble(token);
								boolean invert = MaudPreferences.getBoolean("testing.HippoInvertDetzmInImport", true);
								double shift = MaudPreferences.getDouble("testing.HippoShiftDetzmInImport", 180.0);
								int sign = 1;
								if (invert) sign = -1;
								eta = eta * sign + shift;
								token = Double.toString(eta);
								addEta(banknumber, token);
							}
						}
					}
				}

			} catch (IOException e) {
				System.out.println("Error in loading the data file! Try to remove this data file");
			}
			try {
				reader.close();
			} catch (IOException e) {
			}
//      System.out.println("Number of banks loaded: " + banknumbers());
		}

		checkConsistency(true);

		isAbilitatetoRefresh = isAbilitate;
		notifyUpObjectChanged(this, Constants.ANGULAR_CALIBRATION, -1);
	}*/

	public void calibrateData(DiffrDataFile datafile) {
	}

	public void calibrateX(DiffrDataFile datafile) {
		getBank(datafile.getAngBankNumber()).calibrateX(datafile);
	}

	public double calibrateX(DiffrDataFile datafile, double value) {
		return getBank(datafile.getAngBankNumber()).calibrateX(datafile, value);
	}

	public double notCalibrated(DiffrDataFile datafile, double x) {
		return getBank(datafile.getAngBankNumber()).notCalibrated(datafile, x);
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
		JTOF2DBankOptionsDialog adialog = new JTOF2DBankOptionsDialog(parent, this);
		return adialog;
	}

	public class JTOF2DBankOptionsDialog extends JOptionsDialog {

		JSubordinateLoopListPane bank2DPanel;
		JTextField flightPathTF;

		public JTOF2DBankOptionsDialog(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new BorderLayout(3, 3));

			JPanel upperPanel = new JPanel();
			principalPanel.add(BorderLayout.NORTH, upperPanel);
			upperPanel.add(new JLabel("Flight path to sample (m): "));
			flightPathTF = new JTextField(Constants.FLOAT_FIELD);
			upperPanel.add(flightPathTF);

			upperPanel = new JPanel();
			principalPanel.add(BorderLayout.CENTER, upperPanel);
			bank2DPanel = new JSubordinateLoopListPane(this, "TOF 2D bank");
			bank2DPanel.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "TOF 2D bank"));
			upperPanel.add(bank2DPanel);

			setTitle("Multi TOF 2D options panel");
			initParameters();
			pack();
		}

		public void initParameters() {
			addComponenttolist(flightPathTF, parameterField[0]);
			bank2DPanel.setList(MultiTOFPanelCalibration.this, 0);
		}

		public void retrieveParameters() {
			parameterField[0].setValue(flightPathTF.getText());
			removeComponentfromlist(flightPathTF);
		}

		public void dispose() {
			bank2DPanel.dispose();
			super.dispose();
		}

	}

}
