/*
 * @(#)InstrumentBroadeningGSAS1f.java created 19/10/2006 Milano-Verona
 *
 * Copyright (c) 2006 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of the author and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with the author.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.diffr.instbroad;

import it.unitn.ing.rista.util.*;

import java.lang.*;
import java.io.IOException;
import java.io.BufferedReader;
import java.awt.*;
import java.awt.event.*;
import java.util.StringTokenizer;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.cal.GSASbankCalibration;
import it.unitn.ing.rista.awt.*;

import javax.swing.*;

/**
 * The InstrumentBroadeningGSAS1f is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/12/04 14:30:04 $
 * @since JDK1.1
 */

public class InstrumentBroadeningGSAS1f extends InstrumentBroadening {

  public static String modelID = "GSAS TOF profile function";

  protected static final String[] diclistc = {
      "_instrument_parameter_file", "_riet_par_TOF_func_1_truncation_factor",

      "_instrument_counter_bank_ID", "_riet_par_TOF_function_type",

      "_riet_par_TOF_func1_alpha0", "_riet_par_TOF_func1_alpha1", "_riet_par_TOF_func1_beta0",
      "_riet_par_TOF_func1_beta1", "_riet_par_TOF_func1_sigma0", "_riet_par_TOF_func1_sigma1",
      "_riet_par_TOF_func1_sigma2",
      "_riet_par_TOF_func2_alpha0", "_riet_par_TOF_func2_alpha1", "_riet_par_TOF_func2_beta",
      "_riet_par_TOF_func2_switch", "_riet_par_TOF_func2_sigma0", "_riet_par_TOF_func2_sigma1",
      "_riet_par_TOF_func2_sigma2", /*"_riet_par_TOF_func2_gamma0", "_riet_par_TOF_func2_gamma1",
      "_riet_par_TOF_func2_gamma2",*/
		  "_riet_par_TOF_func3_alpha1", "_riet_par_TOF_func3_beta0",
		  "_riet_par_TOF_func3_beta1", "_riet_par_TOF_func3_sigma0", "_riet_par_TOF_func3_sigma1",
		  "_riet_par_TOF_func3_sigma2", "_riet_par_broadening_texture"
  };

  protected static final String[] diclistcrm = {
      "GSAS instrument parameter file", "truncation value",

      "bank number", "TOF profile function type (GSAS)",

      "Function 1 alpha0", "Function 1 alpha1", "Function 1 beta0",
      "Function 1 beta1", "Function 1 sigma0", "Function 1 sigma1",
      "Function 1 sigma2",
      "Function 2 alpha0", "Function 2 alpha1", "Function 2 beta",
      "Function 2 switch", "Function 2 sigma0", "Function 2 sigma1",
      "Function 2 sigma2", /*"Function 2 gamma0", "Function 2 gamma1",
      "Function 2 gamma2",*/
		  "Function 3 alpha1", "Function 3 beta0",
		  "Function 3 beta1", "Function 3 sigma0", "Function 3 sigma1",
		  "Function 3 sigma2", "texture broadening coeff "
  };

  protected static final String[] classlistc = {};

  protected static final String[] classlistcs = {};

  double[][] difc = null;
//         , s1ec = 0.0, s2ec = 0.0;

  public static String[] functiontype = {"1", "2", "3"};
  public static int functionnumber = functiontype.length;
  static int maxNumberFunctionCoefficients = 20;
  static int[] maxNcoeff = {7, 14, 20};
  int[] typeNumber = null;
	public static final int broadeningTextureID = maxNumberFunctionCoefficients;
//	int choosedBankNumber = 0;

	public InstrumentBroadeningGSAS1f(XRDcat afile, String alabel) {
    super(afile, alabel);
    initXRD();
    identifier = modelID;
    IDlabel = modelID;
  }

  public InstrumentBroadeningGSAS1f(XRDcat afile) {
    this(afile, modelID);
  }

  public InstrumentBroadeningGSAS1f() {
    identifier = modelID;
    IDlabel = modelID;
  }

  public void initConstant() {
    Nstring = 2;
    Nstringloop = 2;
    Nparameter = 0;
    Nparameterloop = maxNumberFunctionCoefficients + 1;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    System.arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
    System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
  }

  public void initParameters() {
    super.initParameters();
    setFileName("");
    setTruncation("0.01");
  }

  void setTruncation(String s) {
    stringField[1] = s;
  }

  public void initializeAsNew() {
    if (initialized)
      return;
    initialized = true;
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterloopField != null) {
      for (int j = 0; j < parameterloopField.length - 1; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.INSTRUMENT_BROADENING);
            return;
          }

	    for (int i = 0; i < parameterloopField[broadeningTextureID].size(); i++)
		    if (source == parameterloopField[broadeningTextureID].elementAt(i)) {
			    notifyParameterChanged(source, Constants.TEXTURE_CHANGED);
			    return;
		    }
      }

	    super.notifyParameterChanged(source);
    }
  }

  double truncationFactor = 0.01;
  int convolutionStep = MaudPreferences.getInteger("asymmetry.convolutionStep", 1);

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);
    int banks = banknumbers();
    truncationFactor = Double.parseDouble(getTruncationField());
    typeNumber = new int[banks];
    for (int bank = 0; bank < banks; bank++)
      typeNumber[bank] = getTypeNumber(bank);

    convolutionStep = MaudPreferences.getInteger("asymmetry.convolutionStep", 1);
    InstrumentBroadeningPVCaglioti.minimumHWHMvalue = MaudPreferences.getDouble(
        "instrBroadening.minimumHWHMvalue", 0.0000001);
  }

  String getTruncationField() {
    return stringField[1];
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    isAbilitatetoRefresh = false;
    checkConsistency();
    isAbilitatetoRefresh = true;
    super.updateParametertoDoubleBuffering(false);
    isAbilitatetoRefresh = false;
//	  choosedBankNumber = getBankNumber();
    int banks = banknumbers();
    difc = new double[banks][maxNumberFunctionCoefficients];
    for (int bank = 0; bank < banks; bank++) {
	    int type = getTypeNumber(bank);
	    switch (type) {
		    case 1:
			    for (int i = 0; i < maxNumberFunctionCoefficients; i++)
				    difc[bank][i] = getParameterLoopValues(i, bank);
			    break;
		    case 3:
			    difc[bank][0] = 0.0;
			    for (int i = 0; i < 6; i++)
				    difc[bank][i + 1] = getParameterLoopValues(i + maxNcoeff[1], bank);
			    break;
		    case 2: // not implemented todo
		    default: {
			    System.out.println(this + ": warning, function type " + type + " for bank " + bank + " not implemented!");
		    }
	    }
    }
    isAbilitatetoRefresh = true;
  }

  public void boundAllBankCoefficients() {
    int banks = banknumbers();
	  for (int i = 0; i < maxNumberFunctionCoefficients; i++) {
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
	  int nparameter = maxNumberFunctionCoefficients;
	  Parameter[] apar = new Parameter[nparameter];
      int index = 0;
      for (int i = 0; i < maxNumberFunctionCoefficients; i++)
        apar[index++] = (Parameter) parameterloopField[i].elementAt(0);

      for (int bank = 1; bank < banks; bank++) {
          for (int i = 0; i < maxNumberFunctionCoefficients; i++) {
            if (i < 3)
              ((Parameter) parameterloopField[i].elementAt(bank)).setEqualTo(apar[i], 1.0, 0.0);
            else {
              if (((Parameter) parameterloopField[i].elementAt(bank)).getValueD() == apar[i].getValueD())
                ((Parameter) parameterloopField[i].elementAt(bank)).setEqualTo(apar[i], 1.0, 0.0);
            }
          }
      }
  }

  public void removeBank(int indexToRemove) {
    removeBank(GSASbankCalibration.bankPrefix + Integer.toString(indexToRemove));
  }

  public void removeBank(String bankID) {
      int indexToRemove = getBankNumber(bankID);
//    System.out.println("prof, removing " + bankID + " " + indexToRemove);
      boolean isAbilitate = isAbilitatetoRefresh;
      isAbilitatetoRefresh = false;
      for (int i = 0; i < stringloopField.length; i++)
        stringloopField[i].removeItemAt(indexToRemove);
      for (int i = 0; i < parameterloopField.length; i++)
        parameterloopField[i].removeItemAt(indexToRemove);
      isAbilitatetoRefresh = isAbilitate;
      notifyUpObjectChanged(this, Constants.INSTRUMENT_BROADENING);
  }

  public void setFileName(String filename) {
    stringField[0] = filename;
    if (getFileName() != null && !getFileName().equals(""))
      readall();
  }

  public String getFileName() {
    return stringField[0];
  }

  public void setBank(int index) {
    if (index >= 0 && index < banknumbers())
      stringField[1] = getBankID(index);
  }

  public int banknumbers() {
    return numberofelementSL(0);
  }

  public String getBankID(int index) {
    if (index >= 0 && index < banknumbers())
      return (String) stringloopField[0].elementAt(index);
    else
      return "";
  }

  public void addBank(String value) {
    stringloopField[0].addItem(value);
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

  public int getBankNumber(DiffrDataFile datafile) {
	  return datafile.getBankNumber();
  }

  public void addType(String value) {
    stringloopField[1].addItem(value);
  }

  public void setFunctionType(int index, String value) {
    stringloopField[1].setElementAt(value, index);
  }

  public String getFunctionType(String bankID) {
		  int number = getBankNumber(bankID);
		  if (number >= 0)
			  return getFunctionType(number);
    return functiontype[0];
  }

  public int getTypeNumber(int index) {
    return Integer.valueOf(getFunctionType(index)).intValue();
  }

  public String getFunctionType(int index) {
	  if (stringloopField != null && stringloopField.length > 1 && stringloopField[1].size() > index)
      return (String) stringloopField[1].elementAt(index);
	  return "";
  }

  public double getCoeffD(int bank, int index) {
    return difc[bank][index];
  }

  public String getCoeffValue(int index, int bank) {
    for (int i = 0; i < banknumbers(); i++)
      if (getBankID(i).equals(bank)) {
        Parameter tmpPar = getCoeff(index, i);
        if (tmpPar != null)
          return tmpPar.getValue();
      }
    return "0";
  }

  public void addCoeff(int loop, int index, String value) {
//    System.out.println(loop + " " + index + " " + getBankNumber() + " " + value);
    addparameterloopField(loop, new Parameter(this, getParameterString(loop, index), value, "0",
        ParameterPreferences.getPref(getParameterString(loop, index) + ".min", "-10"),
        ParameterPreferences.getPref(getParameterString(loop, index) + ".max", "10"), false));
  }

  public Parameter getCoeff(int loop, int index) {
	  if (index < 0)
		  return null;
    if (index >= parameterloopField[loop].size()) {
	    while (index >= parameterloopField[loop].size()) {
        addparameterloopField(loop, new Parameter(this, getParameterString(loop, index), "0", "0",
		        ParameterPreferences.getPref(getParameterString(loop, index) + ".min", "-10"),
		        ParameterPreferences.getPref(getParameterString(loop, index) + ".max", "10"), false));
	    }
    }
	  return (Parameter) parameterloopField[loop].elementAt(index);
  }

  public void setCoeff(int loop, int bank, String value) {
    if (bank >= 0) {
      Parameter tmpPar = getCoeff(loop, bank);
      if (tmpPar != null) {
        tmpPar.setValue(value);
      }
    }
  }

  public String loadDataFile(Frame parent) {
    String filename = Utility.openFileDialog(parent, "Import GSAS instrument file",
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
    for (int i = 0; i < Nstringloop; i++)
      if (stringloopField[i] != null)
        stringloopField[i].removeAllItems();
    for (int i = 0; i < maxNumberFunctionCoefficients; i++)
      if (parameterloopField[i] != null)
        parameterloopField[i].removeAllItems();
  }

  public void readall() {
    boolean isAbilitate = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = Misc.getReader(getFileName());
    if (reader != null) {
      try {
        String token;
        StringTokenizer st;
        String linedata;
        boolean endoffile = false;
//        boolean found = false;
        int banknumber = 1;
        resetParameterToZero();
        int ncoeff = 0;
        int maxcoeff = 0;
        int type = 0;
	      String bankString = "";
        while (!endoffile) {
          linedata = reader.readLine();
          if (linedata == null) {
            endoffile = true;
            break;
          }
//          System.out.println(linedata);

          if (!linedata.toUpperCase().startsWith("COMMENT")) {
            st = new StringTokenizer(linedata, " ,\t\r\n");

            while (st.hasMoreTokens()) {
//              String lasttoken = token;
              token = st.nextToken();
//          	System.out.println(token);
              if (token.equalsIgnoreCase("ITYP")) {
                bankString = Misc.toStringDeleteBlank(linedata.substring(3, 6));
	              if (bankString.length() > 0)
                  banknumber = Integer.valueOf(bankString).intValue();
                addBank(GSASbankCalibration.bankPrefix + bankString);
//                System.out.println("bank " + banknumber);
//                for (int i = 0; i < maxNumberFunctionCoefficients; i++)
//                  addCoeff(i, banknumber, "0.0");
                ncoeff = 0;
              } else if (token.toUpperCase().indexOf("PRCF") != -1) {
//                banknumber = Integer.valueOf(Misc.toStringDeleteBlank(linedata.substring(4, 6))).intValue();
                token = Misc.toStringDeleteBlank(linedata.substring(10, 12));
                if (token.length() == 1) {
                  type = Integer.valueOf(token = st.nextToken()).intValue();
                  if (type < 4)
                    addType(token);
//                  System.out.println("Added type: " + token);
                  maxcoeff = Integer.valueOf(st.nextToken()).intValue();
	                ncoeff = 0;
	                if (type > 1)
		                ncoeff = maxNcoeff[type - 2];
                } else {
                  while (st.hasMoreTokens()) {
                    int typeIndex = type - 1;
//                    System.out.println("Bank " + banknumber + ", Coeff: " + ncoeff + " " + (typeIndex));
                    if (typeIndex < maxNcoeff.length && (ncoeff >= 0 && ncoeff < maxNcoeff[typeIndex])) {
                      addCoeff(ncoeff++, banknumber, token = st.nextToken());
                    } else
                      token = st.nextToken();
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
        e.printStackTrace();
      }
    }
    isAbilitatetoRefresh = isAbilitate;
    checkConsistency();
    notifyUpObjectChanged(this, Constants.INSTRUMENT_BROADENING);
  }

  void checkConsistency() {
    int banks = banknumbers();
//    System.out.println("banks :" + banks);
    for (int i = 1; i < maxNumberFunctionCoefficients; i++) {
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
    }
  }


  public java.util.Vector<double[]> getInstrumentBroadeningAt(double x, DiffrDataFile diffrDataFile) {

    int bank = getBankNumber(diffrDataFile);
    double d = diffrDataFile.getXDataDspace(x);
//    double alpha = difc[bank][0] + difc[bank][1] / d;
//    double alpha_2 = alpha * 0.5;
    double d2 = d * d;
    double d4 = d2 * d2;

    double sigmaq2 = Math.abs(difc[bank][4] + difc[bank][5] * d2 + difc[bank][6] * d4);
    double sigma = Math.sqrt(sigmaq2) * 0.5;
    double xo = diffrDataFile.getXDataInvertCalibration(x);
    Instrument ainstrument = (Instrument) getParent();
    double x1 = ainstrument.getAngularCalibration().calibrateX(diffrDataFile, xo - sigma);
    double x2 = ainstrument.getAngularCalibration().calibrateX(diffrDataFile, xo + sigma);
//	  System.out.println(x + " " + sigma + " " + xo + " " + (xo - sigma) + " " + (xo + sigma));
    sigma = x2 - x1;
  
    double[] hwhm = {Math.max(sigma, InstrumentBroadeningPVCaglioti.minimumHWHMvalue)};
    double[] eta = {0};
    java.util.Vector<double[]> broadV = new java.util.Vector<>(2);
    broadV.add(hwhm);
    broadV.add(eta);
    return broadV;
  }

/*  public double getConvolutedBroadening(double x, double[] tilting_angles, boolean dspacingbase) {

    return 0.0;
  }*/

  /**
   * Return the instrumental asymmetry for the convolution with the profile function.
   * The method here is called by Instrument and should not be modify in general.
   *
   * @param d             the 2-theta or d-spacing (if the spectrum is in d-spacing) of the
   *                      point for which the broadening should be computed.
   * @param diffrDataFile the spectrum
   * @return asymmetry parameter value
   */

  public double getInstrumentalAsymmetry(double d, DiffrDataFile diffrDataFile) {
    int bank = getBankNumber(diffrDataFile);
    double d2 = d * d;
    double d4 = d2 * d2;
    return difc[bank][2] + difc[bank][3] / d4;
  }

  public double getSecondInstrumentalAsymmetry(double d, DiffrDataFile diffrDataFile) {
    int bank = getBankNumber(diffrDataFile);
    return difc[bank][0] + difc[bank][1] / d;
  }

  public void computeAsymmetry(DiffrDataFile diffrDataFile, Sample asample, double[] afit, int min, int max) {


    Instrument ainstrument = (Instrument) getParent();

    double newFit[];

    double total_asymmetrymin = getInstrumentalAsymmetry(diffrDataFile.getXData(min), diffrDataFile);
    total_asymmetrymin = Math.abs(total_asymmetrymin);
    double total_asymmetrymax = getInstrumentalAsymmetry(diffrDataFile.getXData(max), diffrDataFile);
    total_asymmetrymax = Math.abs(total_asymmetrymax);
//    System.out.println(total_asymmetrymax + " " + total_asymmetrymin);
    if (Math.min(total_asymmetrymax, total_asymmetrymin) < 1 && max > min) {
      newFit = new double[max - min];

      for (int j = min; j < max; j++) {
        int absdirection = convolutionStep;  // increasing step
        double x = diffrDataFile.getXDataForCalibration(j);
        double total_asymmetry = getInstrumentalAsymmetry(diffrDataFile.getXData(j), diffrDataFile);
//      System.out.println(total_asymmetry);
        if (total_asymmetry == 0.0)
          newFit[j - min] = afit[j];
        else {
          if (total_asymmetry < 0.0)
            total_asymmetry = -total_asymmetry;
          else
            absdirection = -absdirection;
          int direction = absdirection;
          double function = afit[j];
          double normalization = 1.0;
          int ij = j + direction;
          if (diffrDataFile.insiderange(ij)) {
            double difference = Math.abs(diffrDataFile.getXDataForCalibration(ij) - x);
            double expasymmetry = 1.0;
            double truncation_angle = Math.abs(-Math.log(truncationFactor) / total_asymmetry);
//          System.out.println(total_asymmetry + " " + truncation_angle + " " + difference);
            for (; difference < truncation_angle && diffrDataFile.insiderange(ij); ij += direction) {
              difference = Math.abs(diffrDataFile.getXDataForCalibration(ij) - x);
              expasymmetry = Math.exp(-difference * total_asymmetry);
              function += afit[ij] * expasymmetry;
              normalization += expasymmetry;
//            System.out.println(expasymmetry + " " + difference);
            }
          }
          newFit[j - min] = function / normalization;
        }


      }

      System.arraycopy(newFit, 0, afit, min, max - min);
    }


    total_asymmetrymin = getSecondInstrumentalAsymmetry(diffrDataFile.getXData(min), diffrDataFile);
    total_asymmetrymin = Math.abs(total_asymmetrymin);
    total_asymmetrymax = getSecondInstrumentalAsymmetry(diffrDataFile.getXData(max), diffrDataFile);
    total_asymmetrymax = Math.abs(total_asymmetrymax);
//    System.out.println("alpha " + total_asymmetrymax + " " + total_asymmetrymin);
    if (Math.min(total_asymmetrymax, total_asymmetrymin) < 1) {
      newFit = new double[max - min];

      for (int j = min; j < max; j++) {
        int absdirection = -1;  // increasing step
        double x = diffrDataFile.getXDataForCalibration(j);
        double total_asymmetry = getSecondInstrumentalAsymmetry(diffrDataFile.getXData(j), diffrDataFile);
//      System.out.println(total_asymmetry);
        if (total_asymmetry == 0.0)
          newFit[j - min] = afit[j];
        else {
          if (total_asymmetry < 0.0)
            total_asymmetry = -total_asymmetry;
          else
            absdirection = -absdirection;
          int direction = absdirection;
          double function = afit[j];
          double normalization = 1.0;
          int ij = j + direction;
          if (diffrDataFile.insiderange(ij)) {
            double difference = Math.abs(diffrDataFile.getXDataForCalibration(ij) - x);
            double expasymmetry = 1.0;
            double truncation_angle = Math.abs(-Math.log(truncationFactor) / total_asymmetry);
//          System.out.println(total_asymmetry + " " + truncation_angle + " " + difference);
            for (; difference < truncation_angle && diffrDataFile.insiderange(ij); ij += direction) {
              difference = Math.abs(diffrDataFile.getXDataForCalibration(ij) - x);
              expasymmetry = Math.exp(-difference * total_asymmetry);
              function += afit[ij] * expasymmetry;
              normalization += expasymmetry;
//            System.out.println(expasymmetry + " " + difference);
            }
          }
          newFit[j - min] = function / normalization;
        }


      }

      System.arraycopy(newFit, 0, afit, min, max - min);
    }


  }

  public void computeReflectivityBroadening(DiffrDataFile diffrDataFile, Sample asample, double[] afit, int min, int max) {

//    DataFileSet adataset = diffrDataFile.getDataFileSet();
    double newFit[] = new double[max - min];

    int direction = 1;  // increasing step

    int bank = getBankNumber(diffrDataFile);

//      System.out.println("truncation " + truncation + " " + hwhm);
    for (int j = min; j < max; j++) {
//      System.out.println("Fit before " + afit[j]);
      double x = diffrDataFile.getXDataForCalibration(j);
      double d = diffrDataFile.getXData(j);
      double alpha = difc[bank][0] + difc[bank][1] / d;
      double alpha_2 = alpha * 0.5;
      double d2 = d * d;
      double d4 = d2 * d2;
      double beta = difc[bank][2] + difc[bank][3] / d4;
      double beta_2 = beta * 0.5;
      double sigmaq2 = difc[bank][4] + difc[bank][5] * d2 + difc[bank][6] * d4;
      double sigma = Math.sqrt(sigmaq2);
      double rad2sigma = 1.0 / (sigma * Constants.sqrt2);
      double alphasigma = alpha * sigmaq2;
      double betasigma = beta * sigmaq2;
      double difference = 0.0;
      double function = 0.0;
      double norm = 0.0;
      double truncation = sigma * truncationFactor;// * adataset.getPeakCutoffD();
      if (truncation != 0.0) {
        difference = 0.0;
        for (int ij = j; Math.abs(difference) < truncation && diffrDataFile.insiderange(ij); ij++) {
          difference = -diffrDataFile.getXDataForCalibration(ij) + x;
          double u = alpha_2 * (alphasigma + 2.0 * difference);
          double v = beta_2 * (betasigma - 2.0 * difference);
          double y = (alphasigma + difference) * rad2sigma;
          double z = (betasigma - difference) * rad2sigma;
          double pvFunction = GSASTOFfunction1.getY(u, v, y, z);
          norm += pvFunction;
          function += afit[ij] * pvFunction;
        }
        difference = 0.0;
        for (int ij = j - 1; Math.abs(difference) < truncation && diffrDataFile.insiderange(ij); ij--) {
          difference = -diffrDataFile.getXDataForCalibration(ij) + x;
          double u = alpha_2 * (alphasigma + 2.0 * difference);
          double v = beta_2 * (betasigma - 2.0 * difference);
          double y = (alphasigma + difference) * rad2sigma;
          double z = (betasigma - difference) * rad2sigma;
          double pvFunction = GSASTOFfunction1.getY(u, v, y, z);
          norm += pvFunction;
          function += afit[ij] * pvFunction;
        }
        function /= norm;
      } else
        function = afit[j];
      newFit[j - min] = function;
    }
    System.arraycopy(newFit, 0, afit, min, max - min);
  }

	public double getTextureBroadeningAt(double x) {
		int numberCoeff = numberOfLoopParameters[broadeningTextureID];
		if (numberCoeff == 0)
			return -1.0;
		double[] broadCoeff = getParameterLoopVector(broadeningTextureID);
		double hwhm = broadCoeff[0];
		for (int i = 1; i < numberCoeff; i++)
			hwhm += broadCoeff[i] * MoreMath.pow(x, i);

		if (hwhm < 0.0)
			hwhm = 0.0;
		return hwhm;
	}

	public boolean freeAllBasicParameters() {
    return false;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JBankOptionsD(parent, this);
    return adialog;
  }

  class JBankOptionsD extends JOptionsDialog {

    JTextField filenameL;
    JComboBox bankCB;
    JComboBox typeCB;
    JTextField coeffTF[], truncationTF;
	  JParameterListPane TexturePanel;
	  int selectedBank = -1;

    public JBankOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

	    JTabbedPane tabPanel1 = new JTabbedPane();
	    String tempString[] = {"GSAS Instrument parameters",
			    "Texture broad."};
	    principalPanel.add(BorderLayout.CENTER, tabPanel1);

	    JPanel gsasInstrumentPanel = new JPanel(new BorderLayout(3, 3));
	    tabPanel1.addTab(tempString[0], null, gsasInstrumentPanel);

	    JPanel jp3 = new JPanel();
      jp3.setLayout(new BorderLayout(6, 6));
      JPanel jp2 = new JPanel();
      jp2.setLayout(new BorderLayout(6, 6));
	    gsasInstrumentPanel.add(BorderLayout.NORTH, jp3);
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
	    gsasInstrumentPanel.add(BorderLayout.CENTER, jp1);
      filenameL = new JTextField(40);
      filenameL.setEditable(false);
      jp1.add(filenameL);

      jp1 = new JPanel();
      jp1.setLayout(new GridLayout(0, 3, 3, 3));
	    gsasInstrumentPanel.add(BorderLayout.SOUTH, jp1);

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
      typeCB.setToolTipText("Select the profile function type");
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

      coeffTF = new JTextField[maxNumberFunctionCoefficients];
      for (int i = 0; i < maxNumberFunctionCoefficients; i++) {
        jp2 = new JPanel();
        jp2.setLayout(new FlowLayout());
        jp1.add(jp2);
        jp2.add(new JLabel(diclistcrm[Nstring + Nstringloop + Nparameter + i] + " :"));
        coeffTF[i] = new JTextField(Constants.FLOAT_FIELD);
        jp2.add(coeffTF[i]);
      }

      jp2 = new JPanel();
      jp2.setLayout(new FlowLayout());
      jp1.add(jp2);
      jp2.add(new JLabel("Truncation factor (x width) :"));
      truncationTF = new JTextField(Constants.FLOAT_FIELD);
      jp2.add(truncationTF);


	    TexturePanel = new JParameterListPane(this, false, true);
	    tabPanel1.addTab(tempString[1], null, TexturePanel);

	    setTitle("GSAS type instrument profile function");
	    selectedBank = 0;
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
      filenameL.setText(getFileName());
      initBankList();
      for (int i = 0; i < functionnumber; i++)
        typeCB.addItem(functiontype[i]);
	    if (selectedBank >= 0) {
		    typeCB.setSelectedItem(getFunctionType(selectedBank));
		    initParameterFields();
		    TexturePanel.setList(InstrumentBroadeningGSAS1f.this, broadeningTextureID);
	    }
    }

    public void initParameterFields() {
        for (int i = 0; i < maxNumberFunctionCoefficients; i++) {
          coeffTF[i].setText(getCoeffValue(i, selectedBank));
          addComponenttolist(coeffTF[i], getCoeff(i, selectedBank));
        }
      truncationTF.setText(getTruncationField());
    }

    public void removeParameterFields() {
      for (int i = 0; i < maxNumberFunctionCoefficients; i++)
        removeComponentfromlist(coeffTF[i]);
    }

    public void retrieveParameters() {
      for (int i = 0; i < maxNumberFunctionCoefficients; i++)
        setCoeff(i, selectedBank, coeffTF[i].getText());
      setTruncation(truncationTF.getText());
	    TexturePanel.retrieveparlist();
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

	  public void dispose() {
		  TexturePanel.dispose();

		  super.dispose();
	  }
  }

}

