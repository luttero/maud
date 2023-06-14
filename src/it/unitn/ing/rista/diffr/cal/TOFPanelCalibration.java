/*
 * @(#)TOFPanelCalibration.java created 7/06/2023 Los Alamos
 *
 * Copyright (c) 2023 Luca Lutterotti All Rights Reserved.
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

import java.io.*;
import java.lang.*;
import java.util.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 *  The TOFPanelCalibration is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2023/06/7 15:56:04 $
 * @author Luca Lutterotti
 * @since JDK19
 */


public class TOFPanelCalibration extends AngularCalibration {
	public static String[] diclistc = {"_instrument_parameter_file", "_instrument_counter_panel",
			"_instrument_neutron_flight_path",

			"_instrument_counter_panel_ID",

			"_instrument_panel_difc", "_instrument_panel_difa",
			"_instrument_panel_zero", "_instrument_panel_tof_theta",
			"_instrument_panel_eta", "_pd_instr_dist_spec/detc"};
	public static String[] diclistcrm = {"_instrument_parameter_file", "_instrument_counter_panel",
			"_instrument_neutron_flight_path",

			"_instrument_counter_panel_ID",

			"difc (GSAS) ", "difa (GSAS) ",
			"zero (GSAS) ", "TOF 2theta (deg) ",
			"eta angle position (deg) ", "sample detector distance (m) "};

	public static String[] classlistc = {};
	public static String[] classlistcs = {};

	boolean refreshCalibration = true;

	double[] difc = null;
	double[] difa = null;
	double[] zero = null;
	double[] theta = null;
	double[] eta = null;
	double[] dist = null;
	double flightPath = 9000.0;

	int maxNumberCoefficient = 6;

	int choosedPanelNumber = 0;

	public static final String panelPrefix = "Panel";
	public static String modelID = "TOF Panel";

	public TOFPanelCalibration(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initBaseObject();
		identifier = modelID;
		IDlabel = modelID;
	}

	public TOFPanelCalibration(XRDcat aobj) {
		this(aobj, "");
	}

	public TOFPanelCalibration() {
		identifier = modelID;
		IDlabel = modelID;
	}

	public void initConstant() {
		Nstring = 3;
		Nstringloop = 1;
		Nparameter = 0;
		Nparameterloop = maxNumberCoefficient;
		Nsubordinate = 0;
		Nsubordinateloop = 0;
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
		setBank("");
		stringField[2] = "9.07617";
	}

	public void updateStringtoDoubleBuffering(boolean firstLoading) {
		super.updateStringtoDoubleBuffering(false);
		flightPath = Double.parseDouble(stringField[2]) * 1000;
	}

	public void invertEta() {
		for (int bank = 0; bank < banknumbers(); bank++) {
			double eta = getParameterLoopValues(4, bank);
			getParameterFromLoop(4, bank).setValue(-eta);
		}
	}

	public void updateParametertoDoubleBuffering(boolean firstLoading) {
		if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
			return;
		isAbilitatetoRefresh = false;
		checkConsistency(firstLoading);
		isAbilitatetoRefresh = true;
		super.updateParametertoDoubleBuffering(firstLoading);
		isAbilitatetoRefresh = false;
		int banks = banknumbers();
		choosedPanelNumber = getBankNumber();
		difc = new double[banks];
		difa = new double[banks];
		zero = new double[banks];
		theta = new double[banks];
		eta = new double[banks];
		dist = new double[banks];
		for (int bank = 0; bank < banks; bank++) {
			difc[bank] = getParameterLoopValues(0, bank);
			difa[bank] = getParameterLoopValues(1, bank);
			zero[bank] = getParameterLoopValues(2, bank);
			theta[bank] = getParameterLoopValues(3, bank);
			eta[bank] = getParameterLoopValues(4, bank);
			double distance = getParameterLoopValues(5, bank);
			if (distance == 0)
				distance = 1.0;
			dist[bank] = distance * 1000.0;
		}
		flightPath = Double.parseDouble(stringField[2]) * 1000;
		isAbilitatetoRefresh = true;
	}

	public void notifyParameterChanged(Parameter source) {
		FilePar filepar = getFilePar();
		if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
			int banks = banknumbers();
			for (int bank = 0; bank < banks; bank++) {
				for (int i = 0; i < 3; i++)
					if (source == getParameterFromLoop(i, bank)) {
						notifyParameterChanged(source, Constants.ANGULAR_CALIBRATION);
						return;
					}
				for (int i = 3; i < 5; i++)
					if (source == getParameterFromLoop(i, bank)) {
						notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED);
						notifyParameterChanged(source, Constants.TEXTURE_CHANGED);
						return;
					}
				if (source == getParameterFromLoop(5, bank)) {
					notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED);
					return;
				}
			}
			super.notifyParameterChanged(source);
		}
	}

	public boolean freeAllBasicParameters() {
		freeAllZeroParameters(false);
		return true;
	}

	public void freeAllZeroParameters(boolean forceFree) { // DIFC
		int banks = banknumbers();
		for (int bank = 0; bank < banks; bank++) {
			if (bankIsActive(bank)) {
				if (forceFree)
					((Parameter) parameterloopField[0].elementAt(bank)).setRefinable();
				else
					((Parameter) parameterloopField[0].elementAt(bank)).setRefinableCheckBound();
			}
		}
	}

	protected boolean bankIsActive(int bank) {
		DataFileSet data = (DataFileSet) getParent().getParent();
		int datafiles = data.activedatafilesnumber();
		return datafiles > 0;
	}

	public void boundAllBankCoefficients() {
		int banks = banknumbers();
		for (int i = 0; i < Nparameterloop; i++) {
			Parameter apar = (Parameter) parameterloopField[i].elementAt(0);
			for (int bank = 1; bank < banks; bank++) {
				Parameter apar1 = (Parameter) parameterloopField[i].elementAt(bank);
				if (apar.getValueD() == apar1.getValueD())
					apar1.setEqualTo(apar, 1.0, 0.0);
			}
		}
	}

	public void forceBoundAllBankCoefficients() {
		try {
			int banks = banknumbers();
			int nparameter = Nparameterloop;
			Parameter[] apar = new Parameter[nparameter];
			int index = 0;
//      int selBankNumber = getBankNumber(getBankID());
			for (int i = 0; i < Nparameterloop; i++)
				apar[index++] = (Parameter) parameterloopField[i].elementAt(0);

			for (int bank = 1; bank < banks; bank++) {
//        if (bank != selBankNumber) {
				for (int i = 0; i < Nparameterloop; i++) {
					if (i < 3)
						((Parameter) parameterloopField[i].elementAt(bank)).setEqualTo(apar[i], 1.0, 0.0);
					else {
						if (((Parameter) parameterloopField[i].elementAt(bank)).getValueD() == apar[i].getValueD())
							((Parameter) parameterloopField[i].elementAt(bank)).setEqualTo(apar[i], 1.0, 0.0);
					}
				}
//        }
			}
		} catch (Exception e) {
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}

	public void removePanel(int indexToRemove) {
		removePanel(GSASbankCalibration.bankPrefix + Integer.toString(indexToRemove));
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
			notifyUpObjectChanged(this, Constants.ANGULAR_CALIBRATION);
		} catch (Exception e) {
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}

	public double getFlightPath() {
		return flightPath;
	}

	public void setFileName(String filename) {
		stringField[0] = filename;
		if (getFileName() != null && !getFileName().equals(""))
			readall();
	}

	public String getFileName() {
		return stringField[0];
	}

	public String getDifc() {
		if (getBankNumber() >= 0)
			return getDifc(getBankNumber()).getValue();
		else
			return "0";
	}

	public String getDifa() {
		if (getBankNumber() >= 0)
			return getDifa(getBankNumber()).getValue();
		else
			return "0";
	}

	public String getZero() {
		if (getBankNumber() >= 0)
			return getZero(getBankNumber()).getValue();
		else
			return "0";
	}

	public String getTtheta() {
		if (getBankNumber() >= 0)
			return getTtheta(getBankNumber()).getValue();
		else
			return "0";
	}

	public String getEta() {
		if (getBankNumber() >= 0)
			return getEta(getBankNumber()).getValue();
		else
			return "0";
	}

	public String getDetectorDistance() {
		if (getBankNumber() >= 0)
			return getDetectorDistance(getBankNumber()).getValue();
		else
			return "1";
	}

	public double getTthetaValue(DiffrDataFile datafile, double twotheta) {
		return theta[getBankNumber(datafile)];
	}

	public double getEtaValue(DiffrDataFile datafile) {
		return eta[getBankNumber(datafile)];
	}

	public double getDetectorDistanceValue(DiffrDataFile datafile) {
		return dist[getBankNumber(datafile)];
	}

	public double getDetectorDistanceValue(int bank) {
		return dist[bank];
	}

	public String getDifc(String bank) {
		for (int i = 0; i < banknumbers(); i++)
			if (getPanelID(i).equals(bank))
				return getDifc(i).getValue();
		return "0";
	}

	public String getDifa(String bank) {
		for (int i = 0; i < banknumbers(); i++)
			if (getPanelID(i).equals(bank))
				return getDifa(i).getValue();
		return "0";
	}

	public String getZero(String bank) {
		for (int i = 0; i < banknumbers(); i++)
			if (getPanelID(i).equals(bank))
				return getZero(i).getValue();
		return "0";
	}

	public String getTtheta(String bank) {
		for (int i = 0; i < banknumbers(); i++)
			if (getPanelID(i).equals(bank))
				return getTtheta(i).getValue();
		return "0";
	}

	public String getEta(String bank) {
		for (int i = 0; i < banknumbers(); i++)
			if (getPanelID(i).equals(bank))
				return getEta(i).getValue();
		return "0";
	}

	public String getDetectorDistance(String bank) {
		for (int i = 0; i < banknumbers(); i++)
			if (getPanelID(i).equals(bank))
				return getDetectorDistance(i).getValue();
		return "1";
	}

	public void addDifc(int index, String value) {
		addparameterloopField(0, new Parameter(this, getParameterString(0, index), value, "0",
				ParameterPreferences.getPref(getParameterString(0, index) + ".min", "0"),
				ParameterPreferences.getPref(getParameterString(0, index) + ".max", "10000"), false));
	}

	public void addDifa(int index, String value) {
		addparameterloopField(1, new Parameter(this, getParameterString(1, index), value, "0",
				ParameterPreferences.getPref(getParameterString(1, index) + ".min", "-10"),
				ParameterPreferences.getPref(getParameterString(1, index) + ".max", "10"), false));
	}

	public void addZero(int index, String value) {
		addparameterloopField(2, new Parameter(this, getParameterString(2, index), value, "0",
				ParameterPreferences.getPref(getParameterString(2, index) + ".min", "-10"),
				ParameterPreferences.getPref(getParameterString(2, index) + ".max", "10"), false));
	}

	public void addTtheta(int index, String value) {
		addparameterloopField(3, new Parameter(this, getParameterString(3, index), value, "0",
				ParameterPreferences.getPref(getParameterString(3, index) + ".min", "-180"),
				ParameterPreferences.getPref(getParameterString(3, index) + ".max", "180"), false));
	}

	public void addEta(int index, String value) {
		addparameterloopField(4, new Parameter(this, getParameterString(4, index), value, "0",
				ParameterPreferences.getPref(getParameterString(4, index) + ".min", "-180"),
				ParameterPreferences.getPref(getParameterString(4, index) + ".max", "180"), false));
	}

	public void addDetectorDistance(int index, String value) {
		addparameterloopField(5, new Parameter(this, getParameterString(5, index), value, "0.0",
				ParameterPreferences.getPref(getParameterString(5, index) + ".min", "0.1"),
				ParameterPreferences.getPref(getParameterString(5, index) + ".max", "10"), false));
	}

	public Parameter getDifc(int index) {
		if (index >= 0)
			return (Parameter) parameterloopField[0].elementAt(index);
		else
			return null;
	}

	public Parameter getDifa(int index) {
		if (index >= 0)
			return (Parameter) parameterloopField[1].elementAt(index);
		else
			return null;
	}

	public Parameter getZero(int index) {
		if (index >= 0)
			return (Parameter) parameterloopField[2].elementAt(index);
		else
			return null;
	}

	public Parameter getTtheta(int index) {
		if (index >= 0)
			return (Parameter) parameterloopField[3].elementAt(index);
		else
			return null;
	}

	public Parameter getEta(int index) {
		if (index >= 0)
			return (Parameter) parameterloopField[4].elementAt(index);
		else
			return null;
	}

	public Parameter getDetectorDistance(int index) {
		if (index >= 0)
			return (Parameter) parameterloopField[5].elementAt(index);
		else
			return null;
	}

/*
  public void setDifc(String value) {
    if (getBankNumber() >= 0)
      getDifc(getBankNumber()).setValue(value);
  }

  public void setDifa(String value) {
    if (getBankNumber() >= 0)
      getDifa(getBankNumber()).setValue(value);
  }

  public void setZero(String value) {
    if (getBankNumber() >= 0)
      getZero(getBankNumber()).setValue(value);
  }

  public void setTtheta(String value) {
    if (getBankNumber() >= 0)
      getTtheta(getBankNumber()).setValue(value);
  }

  public void setEta(String value) {
    if (getBankNumber() >= 0)
      getEta(getBankNumber()).setValue(value);
  }

  public void setDetectorDistance(String value) {
    if (getBankNumber() >= 0)
      getDetectorDistance(getBankNumber()).setValue(value);
  }*/

	public void setBank(String value) {
		stringField[1] = value;
	}

	public void setBank(int index) {
		if (index >= 0 && index < banknumbers())
			stringField[1] = getPanelID(index);
	}

	public int banknumbers() {
		return numberofelementSL(0);
	}

	public String getPanelID(int index) {
		if (index >= 0 && index < banknumbers())
			return (String) stringloopField[0].elementAt(index);
		else
			return "";
	}

	public void addBank(String value) {
		stringloopField[0].addItem(value);
	}

	public String getPanelID() {
		return stringField[1];
	}

	public int getBankNumber() {
		if (getPanelID().equalsIgnoreCase("") || getPanelID().equalsIgnoreCase("?"))
			setBank(0);
		try {
			return getBankNumber(getPanelID());
		} catch (Exception e) {
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
		return -1;
	}

	public int getBankNumber(String bankID) throws Exception {
		for (int i = 0; i < banknumbers(); i++)
			if (getPanelID(i).equalsIgnoreCase(bankID))
				return i;
/*    if (bankID.equalsIgnoreCase(getBankID())) {
      setBank(0);
      for (int i = 0; i < banknumbers(); i++) {
        if (getBankID(i).equalsIgnoreCase(bankID))
          return i;
      }
    }*/
//    printBank();
		throw new Exception(bankID + ": The panel ID is not corresponding, reload the instrument parameter file for angular calibration!");
	}

	public void printBank() {
		for (int i = 0; i < banknumbers(); i++)
			System.out.println(getPanelID(i));
	}

	public int getBankNumber(DiffrDataFile datafile) {
		return choosedPanelNumber;
	}

	public String loadDataFile(Frame parent) {
		String filename = Utility.openFileDialog(parent, "Import panel/bank calibration file",
				FileDialog.LOAD, getDirectory(), null, null);
		if (filename != null) {
			parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
//      relativePathChange = false;
			setFileName(filename);
			parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
		return filename;
	}

	public void resetParameterToZero() {
		boolean isAbilitate = isAbilitatetoRefresh;
		isAbilitatetoRefresh = false;
		for (int i = 0; i < Nstringloop; i++)
			if (stringloopField[i] != null)
				stringloopField[i].removeAllItems();
		for (int i = 0; i < Nparameterloop; i++)
			if (parameterloopField[i] != null)
				parameterloopField[i].removeAllItems();
		isAbilitatetoRefresh = isAbilitate;
	}

	public void readall() {
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
		notifyUpObjectChanged(this, Constants.ANGULAR_CALIBRATION);
	}

	public Parameter getCoeff(int loop, int index) {
		if (index >= 0 && index < parameterloopField[loop].size())
			return (Parameter) parameterloopField[loop].elementAt(index);
		else
			return null;
	}

	public void addCoeff(int loop, int index, String value) {
		addparameterloopField(loop, new Parameter(this, getParameterString(loop, index), value, "0",
				ParameterPreferences.getPref(getParameterString(loop, index) + ".min", "-100"),
				ParameterPreferences.getPref(getParameterString(loop, index) + ".max", "100"), false));

	}

	void checkConsistency(boolean firstLoading) {
		for (int i = 0; i < banknumbers(); i++)
			for (int j = 0; j < maxNumberCoefficient; j++)
				if (getCoeff(j, i) == null)
					addCoeff(j, i, "0.0");

		try {
			DataFileSet data = (DataFileSet) getInstrument().getParent();
			DiffrDataFile[] datafiles = data.getActiveDataFiles();
			boolean enabled = false;

//      System.out.println("Checking banks! " + datafiles + ": " + datafiles.length);
			if (datafiles != null) {
				for (int i = 0; i < datafiles.length; i++) {
//      System.out.println("Check datafile: " + datafiles[i].getBankID() + " == " + getBankID(getBankNumber(datafiles[i])));
					if (datafiles[i].getBankID().equalsIgnoreCase(getPanelID(getBankNumber(datafiles[i]))))
						enabled = true;
				}
			}
			if (!enabled && !firstLoading)
				fixAllParameters();
		} catch (Exception e) {
			e.printStackTrace();
		}


/*    int banks = banknumbers();
//    System.out.println("banks :" + banks);
    for (int i = 1; i < Nparameterloop; i++) {
//      System.out.println("par " + i + ": " + numberofelementPL(i));
      while (numberofelementPL(i) > banks) {
        int index = numberofelementPL(i);
        removesPLField(i, index - 1);
      }
      while (numberofelementPL(i) < banks) {
        int index = numberofelementPL(i);
        addparameterloopField(i, new Parameter(this, getParameterString(i, index), "0.0", "0"));
      }
//      System.out.println("At end par " + i + ": " + numberofelementPL(i));
    }*/
	}

	public void calibrateData(DiffrDataFile datafile) {
	}

	public void calibrateX(DiffrDataFile datafile) {
		int datanumber = datafile.getTotalNumberOfData();
//    int banknumber = getBankNumber(datafile);
		updateParametertoDoubleBuffering(false);
		int banknumber = getBankNumber(datafile);
		if (banknumber >= difa.length || banknumber < 0) {
			System.out.println("Warning, panel number: " + banknumber + " out of range: 0 - " + difa.length);
			return;
		}
		for (int i = 0; i < datanumber; i++) {
			double value = datafile.getXDataForCalibration(i);
			if (difa[banknumber] == 0.0) {
				if (difc[banknumber] != 0.0)
					value = (value - zero[banknumber]) / difc[banknumber];
				else
					value = value - zero[banknumber];
			} else
				value = (-difc[banknumber] + Math.sqrt(difc[banknumber] * difc[banknumber] -
						(4 * difa[banknumber] * (zero[banknumber] - value)))) / (2 * difa[banknumber]);
			datafile.setCalibratedXDataOnly(i, value);
		}
	}

	public double calibrateX(DiffrDataFile datafile, double value) {
//    int datasetsNumber = datafile.getTotalNumberOfData();
//    int banknumber = getBankNumber(datafile);
//    updateParametertoDoubleBuffering();
		int banknumber = getBankNumber(datafile);
		if (banknumber >= difa.length || banknumber < 0) {
			System.out.println("Problem, panel number: " + banknumber + " out of range: 0 - " + difa.length);
			return value;
		}
		if (difa[banknumber] == 0.0) {
			if (difc[banknumber] != 0.0)
				value = (value - zero[banknumber]) / difc[banknumber];
			else
				value = value - zero[banknumber];
		} else
			value = (-difc[banknumber] + Math.sqrt(difc[banknumber] * difc[banknumber] -
					(4 * difa[banknumber] * (zero[banknumber] - value)))) / (2 * difa[banknumber]);
		return value;
	}

	public double notCalibrated(DiffrDataFile datafile, double x) {
		int banknumber = getBankNumber(datafile);
		return difc[banknumber] * x + difa[banknumber] * x * x + zero[banknumber];
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
		JOptionsDialog adialog = new JPanelOptionsDialog(parent, this);
		return adialog;
	}

	class JPanelOptionsDialog extends JOptionsDialog {

		JTextField filenameL;
		JComboBox bankCB;
		JTextField difcTF;
		JTextField difaTF;
		JTextField zeroTF;
		JTextField tthetaTF;
		JTextField etaTF;
		JTextField ddistanceTF;
		JTextField fpTF;

		int selectedBank = 0;

		public JPanelOptionsDialog(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new BorderLayout(6, 6));

			JPanel jp2 = new JPanel();
			jp2.setLayout(new BorderLayout(6, 6));
			principalPanel.add(BorderLayout.NORTH, jp2);
			JPanel jp1 = new JPanel();
			jp1.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
			jp2.add(BorderLayout.NORTH, jp1);
			JLabel jl1 = new JLabel("Instrument Parameter File: ");
			jp1.add(jl1);
			jp1 = new JPanel();
			jp1.setLayout(new FlowLayout());
			jp2.add(BorderLayout.CENTER, jp1);
			filenameL = new JTextField(30);
			filenameL.setEditable(false);
			jp1.add(filenameL);
			JButton jb = new JIconButton("Open.gif", "Browse...");
			jp1.add(jb);
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					browsethefile();
				}
			});

			jp1 = new JPanel();
			jp1.setLayout(new GridLayout(5, 1, 6, 6));
			principalPanel.add(BorderLayout.CENTER, jp1);

			jp2 = new JPanel();
			jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
			jp1.add(jp2);
			jp2.add(new JLabel("Panel number: "));
			bankCB = new JComboBox();
			bankCB.setToolTipText("Select the panel number");
			jp2.add(bankCB);

			jp2 = new JPanel();
			jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
			jp1.add(jp2);
			JButton removeButton = new JButton("Remove panel");
			jp2.add(removeButton);
			removeButton.setToolTipText("Remove the selected panel");
			removeButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					removeSelectedPanel();
				}
			});

			jp2 = new JPanel();
			jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
			jp1.add(jp2);
			jp2.add(new JLabel("DIFC: "));
			difcTF = new JTextField(Constants.FLOAT_FIELD);
			jp2.add(difcTF);

			jp2 = new JPanel();
			jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
			jp1.add(jp2);
			jp2.add(new JLabel("DIFA: "));
			difaTF = new JTextField(Constants.FLOAT_FIELD);
			jp2.add(difaTF);

			jp2 = new JPanel();
			jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
			jp1.add(jp2);
			jp2.add(new JLabel("ZERO: "));
			zeroTF = new JTextField(Constants.FLOAT_FIELD);
			jp2.add(zeroTF);

			jp2 = new JPanel();
			jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
			jp1.add(jp2);
			jp2.add(new JLabel("TOF theta: "));
			tthetaTF = new JTextField(Constants.FLOAT_FIELD);
			jp2.add(tthetaTF);

			jp2 = new JPanel();
			jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
			jp1.add(jp2);
			jp2.add(new JLabel("Eta: "));
			etaTF = new JTextField(Constants.FLOAT_FIELD);
			jp2.add(etaTF);

			jp2 = new JPanel();
			jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
			jp1.add(jp2);
			jp2.add(new JLabel("Detector Distance: "));
			ddistanceTF = new JTextField(Constants.FLOAT_FIELD);
			jp2.add(ddistanceTF);

			jp2 = new JPanel();
			jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
			jp1.add(jp2);
			jp2.add(new JLabel("Flight path: "));
			fpTF = new JTextField(Constants.FLOAT_FIELD);
			jp2.add(fpTF);

			setTitle("TOF calibration");
			initParameters();

			bankCB.addItemListener(new ItemListener() {
				public void itemStateChanged(ItemEvent event) {
					bankchanged();
				}
			});


			pack();
		}

		boolean doRefreshBank = true;

		public void initBankList() {
			doRefreshBank = false;
			if (bankCB.getItemCount() > 0)
				bankCB.removeAllItems();
			for (int i = 0; i < banknumbers(); i++)
				bankCB.addItem(getPanelID(i));
			doRefreshBank = true;
			selectedBank = getBankNumber();
			bankCB.setSelectedItem(getPanelID(selectedBank));
		}

		public void initParameters() {
			isAbilitatetoRefresh = false;
			checkConsistency(false);
			isAbilitatetoRefresh = true;
			filenameL.setText(getFileName());
			initBankList();
			if (banknumbers() > 0)
				initParameterFields();
			fpTF.setText(stringField[2]);
		}

		public void initParameterFields() {
			if (banknumbers() > 0) {
				int bank = selectedBank;
				if (bank >= 0) {
					difcTF.setText(getDifc(bank).getValue());
					addComponenttolist(difcTF, getDifc(bank));
					difaTF.setText(getDifa(bank).getValue());
					addComponenttolist(difaTF, getDifa(bank));
					zeroTF.setText(getZero(bank).getValue());
					addComponenttolist(zeroTF, getZero(bank));
					tthetaTF.setText(getTtheta(bank).getValue());
					addComponenttolist(tthetaTF, getTtheta(bank));
					etaTF.setText(getEta(bank).getValue());
					addComponenttolist(etaTF, getEta(bank));
					ddistanceTF.setText(getDetectorDistance(bank).getValue());
					addComponenttolist(ddistanceTF, getDetectorDistance(bank));
				}
			}
		}

		public void removeParameterFields() {
			removeComponentfromlist(difcTF);
			removeComponentfromlist(difaTF);
			removeComponentfromlist(zeroTF);
			removeComponentfromlist(tthetaTF);
			removeComponentfromlist(etaTF);
			removeComponentfromlist(ddistanceTF);
		}

		public void retrieveParameters() {
			getDifc(selectedBank).setValue(difcTF.getText());
			getDifa(selectedBank).setValue(difaTF.getText());
			getZero(selectedBank).setValue(zeroTF.getText());
			getTtheta(selectedBank).setValue(tthetaTF.getText());
			getEta(selectedBank).setValue(etaTF.getText());
			getDetectorDistance(selectedBank).setValue(ddistanceTF.getText());
//			if (bankCB.getSelectedItem() != null)
			stringField[2] = fpTF.getText();
		}

		public void browsethefile() {
			removeParameterFields();
			isAbilitatetoRefresh = false;
			String filename = loadDataFile(this);
			filenameL.setText(getFileName());

			initParameterFields();
			initBankList();
			isAbilitatetoRefresh = true;
		}

		public void bankchanged() {
			if (doRefreshBank && bankCB.getSelectedItem() != null) {
				retrieveParameters();
				removeParameterFields();
				selectedBank = bankCB.getSelectedIndex();
				setBank(selectedBank);
				initParameterFields();
			}
		}

		public void removeSelectedPanel() {
			if (bankCB.getSelectedItem() != null) {
				try {
					String bankToRemove = (String) bankCB.getSelectedItem();
					int selectedIndex = bankCB.getSelectedIndex();
					int newIndex = selectedIndex;
					if (newIndex < bankCB.getItemCount() - 1)
						newIndex++;
					else
						newIndex--;
					if (newIndex >= 0) {
						bankCB.setSelectedIndex(newIndex);
						bankchanged();
//				 selectedBank = newIndex;
					} else
						removeParameterFields();
					removePanel(bankToRemove);
					bankCB.removeItemAt(selectedIndex);

//	        setVisible(false);
//	        dispose();
				} catch (Exception e) {
					e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
				}
			}
		}

	}

}