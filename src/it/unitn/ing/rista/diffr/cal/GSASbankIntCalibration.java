/*
 * @(#)GSASbankIntCalibration.java created 27/08/1998 Mesiano
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.io.cif.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 *  The GSASbankIntCalibration is a class
 *
 *
 * @version $Revision: 1.11 $, $Date: 2006/12/04 14:30:04 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GSASbankIntCalibration extends IntensityCalibration {

  public static String[] diclistc = {"_inst_inc_parameter_file", "_instrument_counter_bank",

                                     "_instrument_counter_bank_ID", "_inst_incident_spectrum_type",

                                     "_inst_inc_spectrum_coeff_0", "_inst_inc_spectrum_coeff_1",
                                     "_inst_inc_spectrum_coeff_2", "_inst_inc_spectrum_coeff_3",
                                     "_inst_inc_spectrum_coeff_4", "_inst_inc_spectrum_coeff_5",
                                     "_inst_inc_spectrum_coeff_6", "_inst_inc_spectrum_coeff_7",
                                     "_inst_inc_spectrum_coeff_8", "_inst_inc_spectrum_coeff_9",
                                     "_inst_inc_spectrum_coeff_10", "_inst_inc_spectrum_coeff_11",
		                               "_inst_inc_spectrum_coeff_12", "_inst_inc_spectrum_coeff_13",
		                               "_inst_inc_spectrum_coeff_14", "_inst_inc_spectrum_coeff_15",
                                     "_inst_inc_spectrum_scale_factor"
  };
  public static String[] diclistcrm = {"_inst_inc_parameter_file", "_instrument_counter_bank",

                                     "_instrument_counter_bank_ID", "_inst_incident_spectrum_type",

                                     "incident spectrum coeff 0", "incident spectrum coeff 1",
                                     "incident spectrum coeff 2", "incident spectrum coeff 3",
                                     "incident spectrum coeff 4", "incident spectrum coeff 5",
                                     "incident spectrum coeff 6", "incident spectrum coeff 7",
                                     "incident spectrum coeff 8", "incident spectrum coeff 9",
                                     "incident spectrum coeff 10", "incident spectrum coeff 11",
		                               "incident spectrum coeff 12", "incident spectrum coeff 13",
		                               "incident spectrum coeff 14", "incident spectrum coeff 15",
                                     "incident spectrum scale factor"
  };

  public static String[] classlistc = {};
  public static String[] classlistcs = {};
  public static String[] functiontype = {"0", "1", "2", "3", "4", "5", "10"};
  public static int functionnumber = functiontype.length;
  static int numberIncSpectrumCoefficients = 16;

  double MINIMUM_INTENSITY_CALIBRATION_VALUE = 1.0E-9;

  double[][] difc = null;
  int[] typeNumber = null;
	private ArrayList<double[]> incidentSpectrum = null;

  boolean refreshCalibration = true;

  int choosedBankNumber = 0;

  public GSASbankIntCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "IPNS/LANSCE Incident Spectrum";
    IDlabel = "IPNS/LANSCE Incident Spectrum";
  }

  public GSASbankIntCalibration(XRDcat aobj) {
    this(aobj, "");
  }

  public GSASbankIntCalibration() {
    identifier = "IPNS/LANSCE Incident Spectrum";
    IDlabel = "IPNS/LANSCE Incident Spectrum";
  }

  public void initConstant() {
    Nstring = 2;
    Nstringloop = 2;
    Nparameter = 0;
    Nparameterloop = numberIncSpectrumCoefficients + 1;
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
  }

  public void setFileName(String filename) {
    stringField[0] = new String(filename);
    if (getFileName() != null && !getFileName().equals(""))
      readall();
  }

  public String getFileName() {
    return stringField[0];
  }

  public String getCoeff(int index) {
    if (getBankNumber() >= 0)
      return getCoeff(index, getBankNumber()).getValue();
    else if (index < numberIncSpectrumCoefficients)
      return "0";
    else
      return "1.0";
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
// System.out.println("Updating string of " + this.toXRDcatString());

    int banks = banknumbers();
    typeNumber = new int[banks];
    for (int bank = 0; bank < banks; bank++)
      typeNumber[bank] = getTypeNumber(bank);
	  MINIMUM_INTENSITY_CALIBRATION_VALUE = MaudPreferences.getDouble("_TOF_incident_intensity_cal.minimumValue", MINIMUM_INTENSITY_CALIBRATION_VALUE);
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    isAbilitatetoRefresh = false;
    chekNumberOfLoopParameters();
    isAbilitatetoRefresh = true;
    super.updateParametertoDoubleBuffering(false);
    isAbilitatetoRefresh = false;
    choosedBankNumber = getBankNumber();
    int banks = banknumbers();
    difc = new double[banks][Nparameterloop];
    for (int bank = 0; bank < banks; bank++)
      for (int i = 0; i < Nparameterloop; i++)
        difc[bank][i] = getParameterLoopValues(i, bank);
    isAbilitatetoRefresh = true;
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
    int banks = banknumbers();
    int nparameter = Nparameterloop;
    Parameter[] apar = new Parameter[nparameter];
    try {
      int index = 0;
      int selBankNumber = getBankNumber(getBankID());
      for (int i = 0; i < Nparameterloop; i++)
        apar[index++] = (Parameter) parameterloopField[i].elementAt(selBankNumber);

      for (int bank = 0; bank < banks; bank++) {
        if (bank != selBankNumber) {
          for (int i = 0; i < Nparameterloop; i++) {
            ((Parameter) parameterloopField[i].elementAt(bank)).setEqualTo(apar[i], 1.0, 0.0);
          }
          setFunctionType(bank, getFunctionType(selBankNumber));
        }
      }
    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
  }

  public void freeAllTOFSFParameters() {
    getInstrument().getIntensity().setNotRefinableCheckBound();
    for (int i = 0; i < banknumbers(); i++)
      if (bankIsActive(i))
        getCoeff(numberIncSpectrumCoefficients, i).setRefinableCheckBound();
  }

  public void forceFreeAllTOFSFParameters() {
    for (int i = 0; i < banknumbers(); i++)
      if (bankIsActive(i))
        getCoeff(numberIncSpectrumCoefficients, i).setRefinable();
  }

  protected boolean bankIsActive(int bank) {
    DataFileSet data = (DataFileSet) ((Instrument) getParent()).getParent();
    int datafiles = data.activedatafilesnumber();
    return datafiles > 0;
  }

  public void removeBank(int indexToRemove) {
    removeBank(GSASbankCalibration.bankPrefix + Integer.toString(indexToRemove));
  }

  public void removeBank(String bankID) {
    try {
      int indexToRemove = getBankNumber(bankID);
//	    System.out.println("Int, removing " + bankID + " " + indexToRemove);
      boolean isAbilitate = isAbilitatetoRefresh;
      isAbilitatetoRefresh = false;
      for (int i = 0; i < stringloopField.length; i++)
        stringloopField[i].removeItemAt(indexToRemove);
      for (int i = 0; i < parameterloopField.length; i++)
        parameterloopField[i].removeItemAt(indexToRemove);
      isAbilitatetoRefresh = isAbilitate;
      notifyUpObjectChanged(this, Constants.INTENSITY_CALIBRATION);
    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
  }

  public double getCoeffD(int bank, int index) {
    return difc[bank][index];
  }

  public String getCoeff(int index, String bank) {
    for (int i = 0; i < banknumbers(); i++)
      if (getBankID(i).equals(bank)) {
        Parameter tmpPar = getCoeff(index, i);
        if (tmpPar != null)
          return tmpPar.getValue();
      }
    return "0";
  }

  public void addCoeff(int loop, int index, String value) {
    addparameterloopField(loop, new Parameter(this, getParameterString(loop, index), value, "0",
            ParameterPreferences.getPref(getParameterString(loop, index) + ".min", "-10000"),
            ParameterPreferences.getPref(getParameterString(loop, index) + ".max", "10000"), false));

  }

  public Parameter getCoeff(int loop, int index) {
    if (index >= 0 && index < parameterloopField[loop].size())
      return (Parameter) parameterloopField[loop].elementAt(index);
    else {
//	    System.out.println("Warning: requested: " + index + " " + ", for bank: " + loop);
      return null;
    }
  }

  public void chekNumberOfLoopParameters() {
    for (int i = 0; i < banknumbers(); i++)
	    chekNumberOfLoopParameters(i);
  }

	public void chekNumberOfLoopParameters(int bank) {
		for (int j = 0; j < numberIncSpectrumCoefficients + 1; j++)
			if (getCoeff(j, bank) == null) {
				if (j == 0 || j == numberIncSpectrumCoefficients)
					addCoeff(j, bank, "1.0");
				else
					addCoeff(j, bank, "0.0");
			}
	}

  public void setCoeff(int loop, String value) {
    if (getBankNumber() >= 0 && getTypeNumber(getBankNumber()) != 10) {
      Parameter tmpPar = getCoeff(loop, getBankNumber());
      if (tmpPar != null)
        tmpPar.setValue(value);
    }
  }

  public void setBank(String value) {
    stringField[1] = new String(value);
  }

  public void setBank(int index) {
    if (index >= 0 && index < banknumbers())
      stringField[1] = new String(getBankID(index));
  }

  public int banknumbers() {
    return numberofelementSL(0);
  }

  public int typenumber() {
    return numberofelementSL(1);
  }

  public String getBankID(int index) {
    if (index >= 0 && index < banknumbers())
      return (String) stringloopField[0].elementAt(index);
    else
      return "";
  }

  public void addBank(String value) {
//	  System.out.println("Adding bank " + value);
    stringloopField[0].addItem(value);
  }

  public void addType(String value) {
//	  System.out.println("Adding function type " + value + " , tot: " + stringloopField[1].size());
    stringloopField[1].addItem(value);
  }

  public void setFunctionType(int index, String value) {
    stringloopField[1].setElementAt(value, index);
  }

  public void setFunctionType(String value) {
    stringloopField[1].setElementAt(value, getBankNumber());
  }

  public String getFunctionType() {
    int number = getBankNumber();
    if (number >= 0)
      return getFunctionType(number);
    return functiontype[0];
  }

  public int getTypeNumber(int index) {
    return Integer.valueOf(getFunctionType(index)).intValue();
  }

  public String getFunctionType(int index) {
  	if (index > - 1 && index < stringloopField[1].size())
    return (String) stringloopField[1].elementAt(index);
  	return functiontype[0];
  }

  public String getFunctionType(String bank) {
    for (int i = 0; i < banknumbers(); i++)
      if (getBankID(i).equals(bank))
        return (String) stringloopField[1].elementAt(i);
    return functiontype[0];
  }

  public String getBankID() {
    return stringField[1];
  }

  public int getBankNumber() {
    if (getBankID().equalsIgnoreCase("") || getBankID().equalsIgnoreCase("?"))
      setBank(0);
    try {
      return getBankNumber(getBankID());
    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
    return -1;
  }

  public int getBankNumber(String bankID) {
//	  System.out.println(bankID + " " + banknumbers());
    for (int i = 0; i < banknumbers(); i++) {
//	    System.out.println("Bank ID for (" + i + "): " + getBankID(i));
      if (getBankID(i).equalsIgnoreCase(bankID))
        return i;
    }
//	  System.out.println("Check " + getBankID());
    if (bankID.equalsIgnoreCase(getBankID())) {
      for (int i = 0; i < banknumbers(); i++) {
        if (getBankID(i).equalsIgnoreCase(bankID))
          return i;
      }
    }
	  return 0;
//    printBank();
  }

  public void printBank() {
    for (int i = 0; i < banknumbers(); i++)
      System.out.println(getBankID(i));
  }

  public int getBankNumber(DiffrDataFile datafile) {
    return choosedBankNumber;
  }

  public String loadDataFile(Frame parent) {
    String filename = Utility.openFileDialog(parent, "Import intensity calibration file",
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
        int ncoeff = 0;

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
	          int j;

            while (st.hasMoreTokens()) {
              String lasttoken = new String(token);
              token = st.nextToken();
//          	System.out.println(token);
              if (token.equalsIgnoreCase("ITYP")) {
                addBank(GSASbankCalibration.bankPrefix + bankString);
                addType(token = st.nextToken());
//	              System.out.println("Bank ITYP " + token);
                ncoeff = 0;
//	              if (getTypeNumber(banknumber-1) != 10)
                addCoeff(numberIncSpectrumCoefficients, banknumber, "1.0");
	              if (token.equals("0"))
		              for (int i = 0; i < numberIncSpectrumCoefficients; i++)
			              addCoeff(i, banknumber, "0.0");
              } else if ((j = token.toLowerCase().indexOf("icoff")) != -1) {
                token.toLowerCase().indexOf("icoff");
                while (st.hasMoreTokens())
                  addCoeff(ncoeff++, banknumber, token = st.nextToken());
              } else if ((token.toLowerCase().indexOf("mfil")) != -1) {
	              if (incidentSpectrum == null)
		              incidentSpectrum = new ArrayList<double[]>(banknumbers());
	              int size = incidentSpectrum.size();
	              if (size != banknumbers()) {
//		              System.out.println("Setting bank " + banknumber + " " + banknumbers() + " " + incidentSpectrum + " " + size);
	                for (int i = size; i < banknumbers(); i++) {
//		                System.out.println("Adding incident data, 1");
		                incidentSpectrum.add(new double[1]);
	                }
	              }
	              chekNumberOfLoopParameters(banknumber - 1);
	              String filename = st.nextToken();
	              String[] folderAndFilename = Misc.getFolderandName(getFileName());

//	              System.out.println("Checking Incident spectrum for bank: " + banknumber + " " +
//	              ", number of functions: " + stringloopField[1].size());

	              if (getTypeNumber(banknumber - 1) == 10) {
		              try {
			              loadIncidentSpectrum(folderAndFilename[0] + filename, banknumber);
		              } catch (Exception e) {
			              System.out.println("Problem loading incident intensity file for bank number: " + banknumber + "!");
			              System.out.println("You cannot use incident intensity function type 10");
			              setFunctionType(banknumber, "0");
		              }
	              }
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
    }
    isAbilitatetoRefresh = isAbilitate;
    notifyUpObjectChanged(this, Constants.INTENSITY_CALIBRATION);
  }

	@Override
	public void writeCustomObject(BufferedWriter out) {
		if (incidentSpectrum == null || incidentSpectrum.size() < 1)
			return;

		try {
			out.newLine();
			out.write("#custom_object_" + "incident_spectrum");
			out.newLine();
			out.write("_instrument_counter_banks_number " + banknumbers() + " # not mandatory, not needed on reading");
			out.newLine();
			for (int bank = 0; bank < banknumbers() && bank < incidentSpectrum.size(); bank++) {
				out.newLine();
				out.write("_instrument_counter_bank " + bank + " # not mandatory, not needed on reading");
				out.newLine();
				double[] incidentIntensity = incidentSpectrum.get(bank);
				out.write("_pd_meas_number_of_points " + incidentIntensity.length + " # mandatory, needed to recognize a new incident spectrum");
				out.newLine();
				out.write(CIFdictionary.loopDecl);
				out.newLine();
				out.write("_pd_meas_intensity_total");
				out.newLine();
				for (int i = 0; i < incidentIntensity.length; i++) {
					out.write(Float.toString((float) incidentIntensity[i]));
					out.newLine();
				}
				out.newLine();
			}
			out.write("#end_custom_object_" + "incident_spectrum");
			out.newLine();
			out.newLine();
		} catch (Exception ioe) {
			System.out.println("Error in writing the incident spectrum for " + toXRDcatString());
		}

	}

	@Override
	public void readCustomObject(CIFtoken ciffile) {
		// to be override by subclasses
		// the default read and do nothing
		int tokentype;
		boolean endofInput = false;
		int index = 0;
		boolean loopStart = false;
		double[] incidentIntensity = null;
		int numberOfPoints = 0;
		int numberOfBanks = 0;
		incidentSpectrum = new ArrayList<double[]>(banknumbers());

//      System.out.println("Custom " + this);
			try {
				do {
					tokentype = ciffile.nextToken();
//        System.out.println(tokentype + " " + ciffile.thestring);
					switch (tokentype) {
						case CIFtoken.TT_CIFE:
							// should be the CIF entry for odf values
							String thecife = ciffile.thestring;
							if (thecife.equalsIgnoreCase("_pd_meas_number_of_points")) {
								if (incidentIntensity != null) {
//									System.out.println("Adding incident data, 2");
									incidentSpectrum.add(incidentIntensity);
								}
								int newtoken = ciffile.nextToken();
								if (newtoken == CIFtoken.TT_NUMBER)
									numberOfPoints = Integer.parseInt(ciffile.thestring);
								else {
									System.out.println("No number of data points for incident spectrum " + this.toXRDcatString());
									return;
								}
							} else if (thecife.equalsIgnoreCase("_instrument_counter_banks_number")) {
								if (ciffile.nextToken() == CIFtoken.TT_NUMBER) {
									numberOfBanks = Integer.parseInt(ciffile.thestring);
								}
							}
							break;
						case CIFtoken.TT_LOOP:
							// start the loop for the values here
							index = 0;
							incidentIntensity = new double[numberOfPoints];
							loopStart = true;
							break;
						case CIFtoken.TT_NUMBER:
							if (!loopStart)
								break;
							if (index < numberOfPoints)
								incidentIntensity[index++] = ciffile.thevalue;
							break;
						case CIFtoken.TT_CUSTOM_END:
							// subordinate loop
							endofInput = true;
							break;
						default: {
						}
					}
				} while (tokentype != CIFtoken.TT_EOF && !endofInput);
				if (incidentIntensity != null) {
//					System.out.println("Adding incident data, 3");
					incidentSpectrum.add(incidentIntensity);
				}
				if (numberOfBanks != incidentSpectrum.size()) {
					System.out.println("Warning: number of banks does not correspond in loading incident spectra for " + toXRDcatString());
				}
			} catch (IOException ioe) {
				ioe.printStackTrace();
				System.out.println("IO exception in custom object for " + toXRDcatString());
			}
	}

	public void loadIncidentSpectrum(String filename, int bank) {
		filename += "(" + Integer.toString(bank) + ")";
		DataFileSet dataset = (DataFileSet) getInstrument().getParent();

		boolean replaceDatafileOld = dataset.replaceDatafile();
		dataset.setReplaceDatafile(false);
		DiffrDataFile[] datafile = dataset.addDataFileforName(filename, false);
		dataset.setReplaceDatafile(replaceDatafileOld);
//		GSASDataFile datafile = new GSASDataFile(dataset, filename);
//		datafile.loadData();
//		System.out.println(this + " data loaded: " + datafile.getLabel());
/*		try {
			dummyMethod();
		} catch (Exception e) {
			e.printStackTrace();
		}*/
		setIncidentSpectrumDataFromDatafile(datafile[0], bank - 1);
		for (int i = 0; i < datafile.length; i++) {
			dataset.removeDatafile(datafile[i]);
		}
	}

	private void dummyMethod() throws Exception {
		throw new Exception();
	}

	private void setIncidentSpectrumDataFromDatafile(DiffrDataFile datafile, int bank) {
		double[] incidentData = datafile.getData();
		setIncidentSpectrumData(incidentData, bank);
	}

	public void setIncidentSpectrumData(double[] incidentData, int bank) {
//		System.out.println("Setting data " + incidentData.length + " " + bank);
		incidentSpectrum.set(bank, incidentData);
	}

	public double calibrateData(DiffrDataFile datafile, double x, int index) {
//    updateStringtoDoubleBuffering(false);
		int bank = getBankNumber(datafile);
		return calibrateData(bank, x, index);
	}

	public double calibrateData(int bank, double x, int index) {
//    updateStringtoDoubleBuffering(false);
//		System.out.println(bank + " " + x + " " + index);
    x /= 1000.0;
    double wt = 0.0, tx = 0.0;
    switch (typeNumber[bank]) {
      case 1:
        wt = getCoeffD(bank, 0);
        double w4 = 1.0;
        for (int i = 1; i <= 5; i++) {
          w4 *= x;
          int i1 = i * 2;
          wt += getCoeffD(bank, i1 - 1) * Math.exp(-getCoeffD(bank, i1) * w4);
        }
        break;
      case 2:
        wt = getCoeffD(bank, 0);
        w4 = x * x;
        wt += getCoeffD(bank, 1) * Math.exp(-getCoeffD(bank, 2) / w4) / (w4 * w4 * x);
        w4 = x;
        for (int i = 2; i <= 5; i++) {
          w4 *= x;
          int i1 = i * 2;
          wt += getCoeffD(bank, i1 - 1) * Math.exp(-getCoeffD(bank, i1) * w4);
        }
        break;
      case 3:

        tx = 2.0 / x - 1.0;
        wt = 0.0;

        for (int i = 0; i < numberIncSpectrumCoefficients; i++)
          wt += getCoeffD(bank, i) * ChebyshevPolynomial.getT(i, tx);

        break;
      case 4:

        tx = 2.0 / x - 1.0;
        wt = getCoeffD(bank, 0);
        w4 = x * x;
        wt += getCoeffD(bank, 1) * Math.exp(-getCoeffD(bank, 2) / w4) / (w4 * w4 * x);
//        w4 = x;
        for (int i = 3; i < numberIncSpectrumCoefficients; i++)
          wt += getCoeffD(bank, i) * ChebyshevPolynomial.getT(i - 2, tx);

        break;
      case 5:
        tx = x / 10.0;
        wt = 0.0;

        for (int i = 0; i < numberIncSpectrumCoefficients; i++)
          wt += getCoeffD(bank, i) * ChebyshevPolynomial.getT(i, tx);

        break;
	    case 10:
//		    System.out.println(this + " " + bank + " " + index + " " + incidentSpectrum.get(bank).length + " " + incidentSpectrum.size());
		    wt = incidentSpectrum.get(bank)[index];
		    break;
      default:
        {
          wt = 1.0;
        }
    }
    double cal = wt * getCoeffD(bank, numberIncSpectrumCoefficients);
	 if (cal < MINIMUM_INTENSITY_CALIBRATION_VALUE)
		 cal = MINIMUM_INTENSITY_CALIBRATION_VALUE;
	 return cal;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JBankOptionsD(parent, this);
    return adialog;
  }

  class JBankOptionsD extends JOptionsDialog {

    JTextField filenameL;
    JComboBox bankCB;
    JComboBox typeCB;
    JTextField coeffTF[], scaleTF;
	  int selectedBank = 0;

    public JBankOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel jp3 = new JPanel();
      jp3.setLayout(new BorderLayout(6, 6));
      JPanel jp2 = new JPanel();
      jp2.setLayout(new BorderLayout(6, 6));
      principalPanel.add(BorderLayout.NORTH, jp3);
      JPanel jp1 = new JPanel();
      jp1.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
      jp3.add(BorderLayout.WEST, jp1);
      JLabel jl1 = new JLabel("Instrument Parameter File: ");
      jp1.add(jl1);
      jp1 = new JPanel();
      jp1.setLayout(new FlowLayout(FlowLayout.RIGHT, 1, 1));
      jp3.add(BorderLayout.EAST, jp1);
      JButton jb = new JIconButton("Open.gif", "Browse...");
      jp1.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          browsethefile();
        }
      });
      jp1 = new JPanel();
      jp1.setLayout(new FlowLayout());
      principalPanel.add(BorderLayout.CENTER, jp1);
      filenameL = new JTextField(40);
      filenameL.setEditable(false);
      jp1.add(filenameL);

      jp1 = new JPanel();
      jp1.setLayout(new GridLayout(7, 2, 3, 3));
      principalPanel.add(BorderLayout.SOUTH, jp1);

      jp2 = new JPanel();
      jp2.setLayout(new FlowLayout(FlowLayout.LEFT, 2, 2));
      jp1.add(jp2);
      jp2.add(new JLabel("Bank number: "));
      bankCB = new JComboBox();
      bankCB.setToolTipText("Select the bank number");
      jp2.add(bankCB);

      jp2 = new JPanel();
      jp2.setLayout(new FlowLayout(FlowLayout.LEFT, 2, 2));
      jp1.add(jp2);
      jp2.add(new JLabel("Function type: "));
      typeCB = new JComboBox();
      typeCB.setToolTipText("Select the incident function type");
      jp2.add(typeCB);

		jp2 = new JPanel();
      jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      jp1.add(jp2);
      JButton removeButton = new JButton("Remove bank");
      jp2.add(removeButton);
      removeButton.setToolTipText("Remove the selected bank");
      removeButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          removeSelectedBank();
        }
      });

      coeffTF = new JTextField[numberIncSpectrumCoefficients];
      for (int i = 0; i < numberIncSpectrumCoefficients; i++) {
        jp2 = new JPanel();
        jp2.setLayout(new FlowLayout());
        jp1.add(jp2);
        jp2.add(new JLabel("Coeff. " + Integer.toString(i) + " :"));
        coeffTF[i] = new JTextField(Constants.FLOAT_FIELD);
        jp2.add(coeffTF[i]);
      }

      jp2 = new JPanel();
      jp2.setLayout(new FlowLayout());
      jp1.add(jp2);
      jp2.add(new JLabel("Scale factor :"));
      scaleTF = new JTextField(Constants.FLOAT_FIELD);
      jp2.add(scaleTF);

	    jp2 = new JPanel();
	    jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
	    jp1.add(jp2);
	    JButton plotButton = new JButton("Plot function");
	    jp2.add(plotButton);
	    plotButton.setToolTipText("Plot incident spectrum for the selected bank");
	    plotButton.addActionListener(new ActionListener() {
		    public void actionPerformed(ActionEvent event) {
			    plotSpectrum();
		    }
	    });

	    setTitle("IPNS/LANSCE intensity calibration");
      initParameters();

      bankCB.addItemListener(new ItemListener() {
        public void itemStateChanged(ItemEvent event) {
          bankchanged();
        }
      });

      typeCB.addItemListener(new ItemListener() {
        public void itemStateChanged(ItemEvent event) {
          typechanged();
        }
      });

      pack();
    }

    public void initParameters() {
/*      isAbilitatetoRefresh = false;
      checkConsistency(false);
      isAbilitatetoRefresh = true;*/
      filenameL.setText(getFileName());
      initBankList();
	    for (int i = 0; i < functionnumber; i++)
		    typeCB.addItem(functiontype[i]);
	    if (banknumbers() > 0) {
        typeCB.setSelectedItem(getFunctionType());
        initParameterFields();
	    }
    }

    public void initParameterFields() {
//	    System.out.println(getBankNumber() + " " + banknumbers());
      if (getBankNumber() >= 0) {
//	      if (getTypeNumber(getBankNumber()) > 0) {
		      for (int i = 0; i < numberIncSpectrumCoefficients; i++) {
//			      System.out.println(i + " ---- " + getCoeff(i));
			      coeffTF[i].setText(getCoeff(i));
			      addComponenttolist(coeffTF[i], getCoeff(i, getBankNumber()));
		      }
//	      }
        scaleTF.setText(getCoeff(numberIncSpectrumCoefficients));
        addComponenttolist(scaleTF, getCoeff(numberIncSpectrumCoefficients, getBankNumber()));
      }
    }

    public void removeParameterFields() {
      for (int i = 0; i < numberIncSpectrumCoefficients; i++)
        removeComponentfromlist(coeffTF[i]);
      removeComponentfromlist(scaleTF);
    }

    public void retrieveParameters() {
      setBank(selectedBank);
//      String type = typeCB.getSelectedItem().toString();
//      if (!type.equals(getFunctionType(bank)))
//          setFunctionType(type);
	    String scaleText = scaleTF.getText();
	    if (scaleText.length() > 0) {
		    setCoeff(numberIncSpectrumCoefficients, scaleText);
		    if (getBankNumber() >= 0) {
			    for (int i = 0; i < numberIncSpectrumCoefficients; i++)
				    setCoeff(i, coeffTF[i].getText());
		    }
	    }
    }

	  boolean doRefreshBank = true;

    public void initBankList() {
	    doRefreshBank = false;
	    if (bankCB.getItemCount() > 0)
		    bankCB.removeAllItems();
	    for (int i = 0; i < banknumbers(); i++)
		    bankCB.addItem(getBankID(i));
	    doRefreshBank = true;
	    bankCB.setSelectedItem(getBankID(selectedBank));
    }

    public void browsethefile() {
	    removeParameterFields();
	    isAbilitatetoRefresh = false;
	    String filename = loadDataFile(this);
	    filenameL.setText(getFileName());
	    selectedBank = 0;
	    initBankList();
	    initParameterFields();
	    typeCB.setSelectedItem(getFunctionType(selectedBank));
	    isAbilitatetoRefresh = true;
    }

    public void bankchanged() {
	    Object sbank = bankCB.getSelectedItem();
	    if (doRefreshBank && sbank != null) {
		    String bank = sbank.toString();
		    retrieveParameters();
		    removeParameterFields();
		    selectedBank = getBankNumber(bank);
		    setBank(selectedBank);
//		    System.out.println("Selected: " + selectedBank);
		    typeCB.setSelectedItem(getFunctionType(bank));
		    initParameterFields();
	    }
    }

    public void typechanged() {
	    Object stype = typeCB.getSelectedItem();
	    if (stype != null) {
		    String bank = bankCB.getSelectedItem().toString();
		    String type = typeCB.getSelectedItem().toString();
		    if (!type.equals(getFunctionType(bank)))
			    setFunctionType(getBankNumber(bank), type);
	    }
    }

    public void removeSelectedBank() {
	    if (bankCB.getSelectedItem() != null) {
		    String bankToRemove = (String) bankCB.getSelectedItem();
		    int selectedIndex = bankCB.getSelectedIndex();
		    int newIndex = selectedIndex;
		    if (newIndex < bankCB.getItemCount() - 1)
			    newIndex++;
		    else
			    newIndex--;
		    if (newIndex >= 0)
			    bankCB.setSelectedItem(getBankID(newIndex));
		    else
			    removeParameterFields();
		    removeBank(bankToRemove);
		    bankCB.removeItemAt(selectedIndex);
	    }
    }

	  public void plotSpectrum() {
		  retrieveParameters();
		  updateStringtoDoubleBuffering(false);
		  refreshComputation = true;
		  int bankIndex = 0;
		  if (selectedBank > 0)
			  bankIndex = selectedBank;
		  DataFileSet data = (DataFileSet) getInstrument().getParent();
		  int datafileIndex = 0;
		  DiffrDataFile datafile = data.getActiveDataFile(datafileIndex);
		  while (datafile.getBankNumber() != bankIndex && datafileIndex < data.activedatafilesnumber() - 1)
			  datafile = data.getActiveDataFile(++datafileIndex);
		  int lineCounts = datafile.datanumber;
		  double[] x = new double[lineCounts];
		  double[] y = new double[lineCounts];
		  for (int i = datafile.startingindex; i < datafile.finalindex; i++) {
			  x[i] = datafile.getXDataOriginal(i);
			  y[i] = calibrateData(datafile, x[i], i);
		  }
		  (new PlotSimpleData(this, x, y, true)).setVisible(true);
	  }
  }

}
