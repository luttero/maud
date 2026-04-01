package it.unitn.ing.rista.diffr.cal;
    
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.ParameterPreferences;
import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.awt.JParameterListPane;

import javax.swing.*;
import java.awt.*;

/**
 * The EnergyMultiStripAngularCalibration is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Apr 11, 2007 8:16:19 AM $
 * @since JDK1.1
 */
public class EnergyMultiStripAngularCalibration extends AngularCalibration {
  public static String modelID = "Multistrip channel calibration";
  public static String descriptionID = "Channel calibration of the multistrip detector with energy resolution";
  
  public static String[] diclistc = {"_inst_detector_multistrip_center_channel",
      "_inst_detector_multistrip_pitch", "_inst_ang_calibration_radius",
      "_inst_channel_calibration_zero", "_inst_channel_calibration_gain"};
  public static String[] diclistcrm = { "Center channel number ",
      "Strip pitch ", "Detector to sample distance ", "Energy of channel zero ", "Gain in eV per channel "};
  
  public static String[] classlistc = {};
  public static String[] classlistcs = {};
  
  boolean refreshCalibration = true;
  
  int centerChannel = 0;
  double pitch = 0.2;
  double radius = 283.0;
  double[] zero = null;
  double[] gain = null;
  
  public EnergyMultiStripAngularCalibration(XRDcat obj, String alabel) {
    super(obj, alabel);
    initXRD();
    identifier = modelID;
    IDlabel = modelID;
    description = descriptionID;
  }
  
  public EnergyMultiStripAngularCalibration(XRDcat afile) {
    this(afile, modelID);
  }
  
  public EnergyMultiStripAngularCalibration() {
    identifier = modelID;
    IDlabel = modelID;
    description = descriptionID;
  }
  
  public void initConstant() {
    Nstring = 1;
    Nstringloop = 0;
    Nparameter = 2;
    Nparameterloop = 2;
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
    parameterField[0] = new Parameter(this, getParameterString(0), 0.2,
        ParameterPreferences.getDouble(getParameterString(0) + ".min", 0.01),
        ParameterPreferences.getDouble(getParameterString(0) + ".max", 1));
    parameterField[1] = new Parameter(this, getParameterString(1), 283,
        ParameterPreferences.getDouble(getParameterString(1) + ".min", 3),
        ParameterPreferences.getDouble(getParameterString(1) + ".max", 1000));
/*    parameterField[2] = new Parameter(this, getParameterString(2), 0,
        ParameterPreferences.getDouble(getParameterString(2) + ".min", -10),
        ParameterPreferences.getDouble(getParameterString(2) + ".max", 10));
    parameterField[3] = new Parameter(this, getParameterString(3), 200.0,
        ParameterPreferences.getDouble(getParameterString(3) + ".min", 0.0001),
        ParameterPreferences.getDouble(getParameterString(3) + ".max", 1));*/
  }
  
  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);
    centerChannel = Integer.parseInt(getString(0));
  }
  
  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(firstLoading);
    pitch = getParameterValue(0);
    radius = getParameterValue(1);
    int banks = numberofelementPL(0);
    if (zero == null || zero.length != banks)
      zero = new double[banks];
    for (int bank = 0; bank < banks; bank++)
      zero[bank] = getParameterFromLoop(0, bank).getValueD();
    banks = numberofelementPL(1);
    if (gain == null || gain.length != banks)
      gain = new double[banks];
    for (int bank = 0; bank < banks; bank++)
      gain[bank] = getParameterFromLoop(1, bank).getValueD();
  }
  
  public void checkBanksNumber(int bank) {
    int banks = banknumbers();
    if (bank < banks)
      return;
    for (int i = banks; i <= bank; i++) {
      Parameter par = new Parameter(this, getParameterString(0, i), -1500.0,
          ParameterPreferences.getDouble(getParameterString(0, i) + ".min", -2000),
          ParameterPreferences.getDouble(getParameterString(0, i) + ".max", 2000));
      addparameterloopField(0, par);
      par = new Parameter(this, getParameterString(1, i), 63.0,
          ParameterPreferences.getDouble(getParameterString(1, i) + ".min", 1),
          ParameterPreferences.getDouble(getParameterString(1, i) + ".max", 1000));
      addparameterloopField(1, par);
    }
    banks = banknumbers();
    if (zero == null || zero.length != banks) {
      zero = new double[banks];
      gain = new double[banks];
    }
    for (int ibank = 0; ibank < banks; ibank++) {
      zero[ibank] = getParameterLoopValues(0, ibank);
      gain[ibank] = getParameterLoopValues(1, ibank);
    }
  }
  
  public int banknumbers() {
    return numberofelementPL(0);
  }
  
  public boolean freeAllBasicParameters() {
//    parameterField[2].setRefinableCheckBound();
//    parameterField[3].setRefinableCheckBound();
    return true;
  }
  
  public String getCenterChannelS() {
    return getString(0);
  }
  
  public void setCenterChannel(String value) {
    setString(0, value);
  }
  
/*  public int getBankNumber(DiffrDataFile datafile) {
    return datafile.getAngBankNumber();
  }
  
  public int getBankNumber(String bankID) throws Exception {
    int pos = bankID.lastIndexOf("ank") + 3;
    if (pos > 0) {
      int bank = Integer.parseInt(bankID.substring(pos));
      return bank;
    }
    throw new Exception(bankID + ": The bank ID does not contain the number!");
  }*/
  
  public double getReal2ThetaValue(int number, double twotheta) {
    if (pitch <= 0) {
      centerChannel = Integer.parseInt(getString(0));
      pitch = getParameterValue(0);
      radius = getParameterValue(1);
    }
    return twotheta + pitch * (number - centerChannel) / radius * Constants.PITODEG;
  }
  
  public void calibrateX(DiffrDataFile datafile) {
    int datanumber = datafile.getTotalNumberOfData();
    int bankNumber = datafile.getBankNumber();
    checkBanksNumber(bankNumber);
    updateParametertoDoubleBuffering(false);
    double angcal;
    for (int i = 0; i < datanumber; i++) {
      double value = datafile.getXDataForCalibration(i);
      angcal = zero[bankNumber] + gain[bankNumber] * value;
      datafile.setCalibratedXDataOnly(i, angcal);
    }
  }
  
  public int getChannelForZero(DiffrDataFile datafile) {
    int channel = 0;
    int bankNumber = datafile.getBankNumber();
    checkBanksNumber(bankNumber);
    double value = datafile.getXDataForCalibration(channel);
    double angcal = zero[bankNumber] + gain[bankNumber] * value;
    double minValue = Math.abs(angcal);
    
    while (angcal < 0) {
      angcal = zero[bankNumber] + gain[bankNumber] * datafile.getXDataForCalibration(++channel);
      double absValue = Math.abs(angcal);
      if (absValue < minValue)
        minValue = absValue;
    }
    while (angcal > 0 && channel > 0) {
      angcal = zero[bankNumber] + gain[bankNumber] * datafile.getXDataForCalibration(--channel);
    }
    if (minValue < Math.abs(angcal))
      channel++;
    
    return channel;
  }
  
  public double getChannelStep(DiffrDataFile diffrDataFile) {
    return gain[diffrDataFile.getBankNumber()];
  }
  
  public double notCalibrated(DiffrDataFile datafile, double x) {
    return x;
  }
  
  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JPolMSAngOptionsD(parent, this);
    return adialog;
  }
  
  class JPolMSAngOptionsD extends JOptionsDialog {
    
    JParameterListPane coeffPanel[];
    JTextField centerChannelTF;
    String[] tabLabels = {"Zero", "Gain"};
    
    public JPolMSAngOptionsD(Frame parent, XRDcat obj) {
      
      super(parent, obj);
  
      principalPanel.setLayout(new BorderLayout(6, 6));
  
      JPanel gridPanel = new JPanel(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.NORTH, gridPanel);
  
      gridPanel.add(new JLabel("Center channel number: "));
      centerChannelTF = new JTextField(Constants.FLOAT_FIELD);
      gridPanel.add(centerChannelTF);
      
      addParField(gridPanel, "Multistrip pitch (mm): ", parameterField[0]);
      addParField(gridPanel, "Detector distance (mm): ", parameterField[1]);
//      addParField(principalPanel, "Zero channel (eV): ", parameterField[2]);
//      addParField(principalPanel, "Gain (eV/channel): ", parameterField[3]);
  
      JTabbedPane tabPanel = new JTabbedPane();
      principalPanel.add(BorderLayout.CENTER, tabPanel);
      coeffPanel = new JParameterListPane[tabLabels.length];
      for (int i = 0; i < tabLabels.length; i++) {
        JPanel flowPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
        tabPanel.addTab(tabLabels[i], null, flowPanel);
        coeffPanel[i] = new JParameterListPane(this, false, true);
        flowPanel.add(coeffPanel[i]);
      }
      
      setTitle("Multistrip energy channel calibration");
      initParameters();
      
      pack();
    }
    
    public void initParameters() {
      super.initParameters();
      centerChannelTF.setText(getCenterChannelS());
      for (int i = 0; i < tabLabels.length; i++)
        coeffPanel[i].setList(XRDparent, i);
    }
  
    public void retrieveParameters() {
      super.retrieveParameters();
      setCenterChannel(centerChannelTF.getText());
    }
  
    public void dispose() {
      for (int i = 0; i < tabLabels.length; i++)
        coeffPanel[i].dispose();
      super.dispose();
    }
  
  }
  
  
}
