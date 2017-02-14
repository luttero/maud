/*
 * @(#)Instrument.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
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

package it.unitn.ing.rista.diffr;

import it.unitn.ing.rista.diffr.instbroad.InstrumentBroadeningGSAS1f;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.function.*;
import it.unitn.ing.rista.diffr.cal.*;

import java.lang.*;
import java.awt.*;
import java.util.Vector;
//import javax.swing.*;
import it.unitn.ing.rista.diffr.radiation.*;
import it.unitn.ing.rista.diffr.instbroad.InstrumentBroadeningPVCaglioti;
import it.unitn.ing.rista.io.cif.CIFItem;

/**
 * The Instrument is a class
 *
 * @version $Revision: 1.26 $, $Date: 2006/12/04 14:30:04 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Instrument extends XRDcat {
  protected static final String[] diclistc = {
    "_diffrn_measurement_device_type",
		"_maud_optional_intensity_factor",
    "_pd_proc_intensity_incident",
      
    "_riet_par_2-theta_offset",

    "_inst_intensity_calibration", "_inst_angular_calibration", "_pd_instr_geometry",
    "_diffrn_measurement_method", "_diffrn_radiation_type",
    "_diffrn_radiation_detector", "_diffrn_inst_broadening", "_exptl_absorpt_correction_type"
    };
  protected static final String[] diclistcrm = {
    "_diffrn_measurement_device_type",
		  "_maud_optional_intensity_factor",
      "Intensity (scale factor)",
    "2-theta or d-spacing offset ",

    "_inst_intensity_calibration", "_inst_angular_calibration", "_pd_instr_geometry",
    "_diffrn_measurement_method", "_diffrn_radiation_type",
    "_diffrn_radiation_detector", "Instrument broadening", "_exptl_absorpt_correction_type"
    };

  protected static final String[] classlistc = {};

  protected static final String[] classlistcs = {"superclass:it.unitn.ing.rista.diffr.cal.IntensityCalibration",
                                                 "superclass:it.unitn.ing.rista.diffr.cal.AngularCalibration",
                                                 "superclass:it.unitn.ing.rista.diffr.Geometry",
                                                 "superclass:it.unitn.ing.rista.diffr.Measurement",
                                                 "superclass:it.unitn.ing.rista.diffr.RadiationType",
                                                 "superclass:it.unitn.ing.rista.diffr.Detector",
                                                 "superclass:it.unitn.ing.rista.diffr.InstrumentBroadening",
                                                 "superclass:it.unitn.ing.rista.diffr.Absorption"};

  double thetaDisplacement[] = null;
  int thetaDisplacementN = 0;
  static int thetaDisplacementID = 0;

  public Instrument(XRDcat afile, String alabel) {
    super(afile, alabel);
    initXRD();
    identifier = "Diffraction Instrument";
  }

  public Instrument(XRDcat afile) {
    this(afile, "Instrument_x");
  }

  public Instrument() {
    
  }

  public void initConstant() {
    Nstring = 2;
    Nstringloop = 0;
    Nparameter = 1;
    Nparameterloop = 1;
    Nsubordinate = 8;
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

    stringField[0] = toXRDcatString(); // to avoid the notifyInstrumentLabelChanged
	  stringField[1] = "1.0";
    setIntensity("1.0");
    setGeometry("Bragg-Brentano");
    setMeasurement("Theta-2Theta");
    setRadiationType("X-ray tube");
    ((XrayTubeRadiation) getRadiationType()).setTheTube(2);
    setDetector("Scintillation");
    setAbsorption("none abs");
    setIntensityCalibration("none cal");
    setAngularCalibration("no ang");
    setInstrumentBroadening("Caglioti PV");
    for (int i = 0; i < 1; i++) {
      parameterField[i].setValueMin(ParameterPreferences.getDouble(parameterField[i].getLabel() + ".min",
                         0.0));
      parameterField[i].setValueMax(ParameterPreferences.getDouble(parameterField[i].getLabel() + ".max",
                        10000.0));
    }
    parameterField[0].setPositiveOnly();
//		pivotrequired[0] = true;
  }

  public void initializeAsNew() {
    if (initialized)
      return;
    initialized = true;

  }

  public void setLoop(Vector avector, int element) {

    super.setLoop(avector, element);
    // in case we load old parameter files

    if (getFilePar().getVersion() < 2.048 && avector.size() > 0 && check(((CIFItem) avector.elementAt(0)).cif)) {
      if (getInstrumentBroadening() == null || !getInstrumentBroadening().getLabel().equalsIgnoreCase("Caglioti PV"))
        setInstrumentBroadening("Caglioti PV");
      getInstrumentBroadening().setLoop(avector, element);
    }

  } // end of setLoop method

  public int setField(String cif, String astring, String astringerror, String min, String max, boolean free,
                      String refName, String refBound, String constant, String ratio, String expression,
                      boolean autoTrace, boolean positive) {
    int index = super.setField(cif, astring, astringerror, min, max, free,
        refName, refBound, constant, ratio, expression, autoTrace, positive);

    if (index == -1 && getFilePar().getVersion() < 2.048 && check(cif)) {
      if (getInstrumentBroadening() == null || !getInstrumentBroadening().getLabel().equalsIgnoreCase("Caglioti PV"))
        setInstrumentBroadening("Caglioti PV");
      getInstrumentBroadening().setField(cif, astring, astringerror, min, max, free,
        refName, refBound, constant, ratio, expression, autoTrace, positive);
    }

    return index;
  }

  boolean check(String cif) {
    for (int i = 0; i < InstrumentBroadeningPVCaglioti.diclistc.length; i++)
      if (InstrumentBroadeningPVCaglioti.diclistc[i].equalsIgnoreCase(cif))
        return true;
    return false;
  }

  public String toDataString() {
    return "instrument_" + toXRDcatString();
  }

  public void setLabel(String alabel) {
    setInstrumentID(alabel);
  }

  public String getInstrumentID() {
    return getString(0);
  }

  public void setInstrumentID(String astring) {
    super.setLabel(astring);
    if (stringField != null) {
      setString(0, astring);
    }
  }

  public Parameter getIntensity() {
    return parameterField[0];
  }

	public double getIntensityValue() {
		return getIntensity().getValueD()  * intensityScaleFactor;
	}

  public void setIntensity(String value) {
    parameterField[0].setValue(value);
  }

  public double[] getTextureAngles(DiffrDataFile datafile, double[] tilting_angles,
                                  double[] sampleAngles, double twotheta) {
    return getGeometry().getTextureAngles(datafile, tilting_angles, sampleAngles, twotheta);
  }

  public double[][] getTextureAngles(DiffrDataFile datafile, double[] tilting_angles,
                                  double[] sampleAngles, double[] twotheta) {
    return getGeometry().getTextureAngles(datafile, tilting_angles, sampleAngles, twotheta);
  }

  public double[] getAlternateTextureAngles(DiffrDataFile datafile, double[] tilting_angles,
                                  double[] sampleAngles, double twotheta) {
    return getGeometry().getAlternateTextureAngles(datafile, tilting_angles, sampleAngles, twotheta);
  }

  public void computeActiveTextureAngles(double[][] texture_angles, double[] sampleAngles, double position) {
    getGeometry().computeActiveTextureAngles(texture_angles, sampleAngles, position);
  }

  public int getthetaoffsetnumber() {
    return getThetaDisplacementList().size();
  }

  public ListVector getThetaDisplacementList() {
    return parameterloopField[thetaDisplacementID];
  }

  public Parameter getThetaDisplacement(int index) {
    return (Parameter) getThetaDisplacementList().elementAt(index);
  }

  public void add2ThetaDisplacementParameter() {
    addparameterloopField(0, new Parameter(this, getParameterString(0, getthetaoffsetnumber() - 1), 0,
        -0.1, 0.1));
  }

	double intensityScaleFactor = 1.0;

	public void updateStringtoDoubleBuffering(boolean firstLoading) {
		intensityScaleFactor = Double.parseDouble(stringField[1]);
	}

	public void updateParametertoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);

    parameterField[0].setPositiveOnly();
    thetaDisplacement = getParameterLoopVector(thetaDisplacementID);
    thetaDisplacementN = numberOfLoopParameters[thetaDisplacementID];
    for (int i = 0; i < thetaDisplacementN; i++)
      ((Parameter) getThetaDisplacementList().elementAt(i)).setMinimumSignificantValue(Math.pow(0.001, i + 1));
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
        if (source == parameterField[0]) {
          notifyParameterChanged(source, Constants.BEAM_INTENSITY_CHANGED);
          return;
        }
      for (int i = 0; i < parameterloopField[0].size(); i++)
        if (source == parameterloopField[0].elementAt(i)) {
	        notifyParameterChanged(source, Constants.ANGULAR_CALIBRATION);
	        return;
        }
      super.notifyParameterChanged(source);
    }
  }

/*  private void checkCagliotiFirstParameter() {
    Parameter firstCaglioti = getCaglioti(0);
    if (firstCaglioti != null) {
      double first = firstCaglioti.getValueD();
      if (first < 0.0)
        firstCaglioti.setValue(-first);
    }
  }*/

  public double[] getInstrumentalBroadeningAt(double x, DiffrDataFile diffrDataFile) {

// Attention: x equal to 2theta
    return getInstrumentBroadening().getInstrumentalBroadeningAt(x, diffrDataFile);
  }

/*  public double getConvolutedBroadening(double x, double[] tilting_angles, boolean dspacingbase) {

    return getInstrumentBroadening().getConvolutedBroadening(x, tilting_angles, dspacingbase);
  }*/

  public double getInstrumentalAsymmetry(double x, DiffrDataFile diffrDataFile) {

    return getInstrumentBroadening().getInstrumentalAsymmetry(x, diffrDataFile);
  }

  public double getCorrectedPosition(Sample asample, double x, double[] tilting_angles,
                                     DiffrDataFile adatafile) {

	  if (getAngularCalibration().positionAlreadyCorrected())
		  return x;
    return getGeometry().getCorrectedPosition(asample, x, tilting_angles, adatafile);
  }

	public double getCorrectedCoordinate(double x) {

		double position = x;
		for (int i = 0; i < thetaDisplacementN; i++)
			position -= thetaDisplacement[i] * MoreMath.pow(x, i);
		return position;
	}

	public int getIntensityCalibrationID() {
    return 0;
  }

  public IntensityCalibration getIntensityCalibration() {
    if (subordinateField[getIntensityCalibrationID()] == null)
      setIntensityCalibration(0);
    return (IntensityCalibration) subordinateField[getIntensityCalibrationID()];
  }

  public void setIntensityCalibration(String value) {
    if (subordinateField[getIntensityCalibrationID()] == null ||
            !value.equals(subordinateField[getIntensityCalibrationID()].identifier)) {
      boolean oldvalue = isAbilitatetoRefresh;
      isAbilitatetoRefresh = false;
      setsubordinateField(getIntensityCalibrationID(), value);
      isAbilitatetoRefresh = oldvalue;
      notifyUpObjectChanged(this, Constants.INTENSITY_CALIBRATION);
    }
  }

  public void setIntensityCalibration(int number) {
    setIntensityCalibration(getsubordIdentifier(getIntensityCalibrationID(), number));
  }

  public String getIntensityCalibrationMethod() {
    return getIntensityCalibration().identifier;
  }

  public boolean provideRealCalibration() {
    return !getIntensityCalibrationMethod().equals("none cal");
  }

  public int getAngularCalibrationID() {
    return 1;
  }

  public AngularCalibration getAngularCalibration() {
    if (subordinateField[getAngularCalibrationID()] == null)
      setAngularCalibration(0);
    return (AngularCalibration) subordinateField[getAngularCalibrationID()];
  }

  public void setAngularCalibration(String value) {
    if (subordinateField[getAngularCalibrationID()] == null ||
            !value.equals((subordinateField[getAngularCalibrationID()]).identifier)) {
      boolean oldvalue = isAbilitatetoRefresh;
      isAbilitatetoRefresh = false;
      setsubordinateField(getAngularCalibrationID(), value);
      isAbilitatetoRefresh = oldvalue;
      notifyUpObjectChanged(this, Constants.ANGULAR_CALIBRATION);
    }
  }

  public void setAngularCalibration(int number) {
    setAngularCalibration(getsubordIdentifier(getAngularCalibrationID(), number));
  }

  public String getAngularCalibrationMethod() {
    return getAngularCalibration().identifier;
  }

  public boolean provideRealAngularCalibration() {
    return !getAngularCalibrationMethod().equals("no ang");
  }

  public int getGeometryID() {
    return 2;
  }

  public Geometry getGeometry() {
    if (subordinateField[getGeometryID()] == null)
      setGeometry(0);
    return (Geometry) subordinateField[getGeometryID()];
  }

  public void setGeometry(String value) {
    if (subordinateField[getGeometryID()] == null ||
            !value.equals(subordinateField[getGeometryID()].identifier))
      setsubordinateField(getGeometryID(), value);
  }

  public void setGeometry(int number) {
    setGeometry(getsubordIdentifier(getGeometryID(), number));
  }

  public String getGeometryS() {
    return getGeometry().identifier;
  }


  public int getMeasurementID() {
    return 3;
  }

  public Measurement getMeasurement() {
    if (subordinateField[getMeasurementID()] == null)
      setMeasurement(0);
    return (Measurement) subordinateField[getMeasurementID()];
  }

  public void setMeasurement(String value) {
    if (subordinateField[getMeasurementID()] == null ||
            !value.equals(subordinateField[getMeasurementID()].identifier))
      setsubordinateField(getMeasurementID(), value);
  }

  public void setMeasurement(int number) {
    setMeasurement(getsubordIdentifier(getMeasurementID(), number));
  }

  public String getMeasurementS() {
    return getMeasurement().identifier;
  }


  public int getRadiationTypeID() {
    return 4;
  }

  public RadiationType getRadiationType() {
    if (subordinateField[getRadiationTypeID()] == null)
      setRadiationType(0);
    return (RadiationType) subordinateField[getRadiationTypeID()];
  }

  public void setRadiationType(String value) {
    if (subordinateField[getRadiationTypeID()] == null ||
            !value.equals(subordinateField[getRadiationTypeID()].identifier))
      setsubordinateField(getRadiationTypeID(), value);
  }

  public void setRadiationType(int number) {
    setRadiationType(getsubordIdentifier(getRadiationTypeID(), number));
  }

  public String getRadiationTypeS() {
    return getRadiationType().identifier;
  }

/*  public int getRadiationNumber() {
    int maxnumber = getsubordClassNumber(getRadiationTypeID());

    for (int i = 0; i < maxnumber; i++)
      if (getsubordIdentifier(getRadiationTypeID(), i).equals(getRadiationTypeS()))
        return i;

    return -1;
  }*/

  public boolean isNeutron() {
    return getRadiationType().isNeutron();
  }

  public boolean isElectron() {
    return getRadiationType().isElectron();
  }

/*	public boolean isDynamical() {
		return getRadiationType().isDynamical();
	}*/ // not used

	public boolean isTOF() {
    return getRadiationTypeS().toLowerCase().indexOf(("TOF").toLowerCase()) != -1;
  }

/*  public int getRadiationTubeNumber() {
    if (!isNeutron() && !isElectron())
      return getRadiationType().getRadiationTubeNumber();
    return -1;
  }
*/

  public int getDetectorID() {
    return 5;
  }

  public Detector getDetector() {
    if (subordinateField[getDetectorID()] == null)
      setDetector(0);
    return (Detector) subordinateField[getDetectorID()];
  }

  public void setDetector(String value) {
    if (subordinateField[getDetectorID()] == null ||
            !value.equals(subordinateField[getDetectorID()].identifier))
      setsubordinateField(getDetectorID(), value);
  }

  public void setDetector(int number) {
    setDetector(getsubordIdentifier(getDetectorID(), number));
  }

  public String getDetectorS() {
    return getDetector().identifier;
  }


  public int getAbsorptionID() {
    return 7;
  }

  public Absorption getAbsorption() {
    if (subordinateField[getAbsorptionID()] == null)
        setAbsorption(0);
    return (Absorption) subordinateField[getAbsorptionID()];
  }

  public void setAbsorption(String value) {
    if (subordinateField[getAbsorptionID()] == null ||
            !value.equals(subordinateField[getAbsorptionID()].identifier))
      setsubordinateField(getAbsorptionID(), value);
  }

  public void setAbsorption(int number) {
    setAbsorption(getsubordIdentifier(getAbsorptionID(), number));
  }

  public String getAbsorptionS() {
    return getAbsorption().identifier;
  }

  private int getInstrumentBroadeningID() {
    return 6;
  }

  public InstrumentBroadening getInstrumentBroadening() {
    if (subordinateField[getInstrumentBroadeningID()] == null)
      setInstrumentBroadening(0);
    return (InstrumentBroadening) subordinateField[getInstrumentBroadeningID()];
  }

  public void setInstrumentBroadening(String value) {
    if (subordinateField[getInstrumentBroadeningID()] == null ||
            !value.equals(subordinateField[getInstrumentBroadeningID()].identifier))
      setsubordinateField(getInstrumentBroadeningID(), value);
  }

  public void setInstrumentBroadening(int number) {
    setInstrumentBroadening(getsubordIdentifier(getInstrumentBroadeningID(), number));
  }

  public String getInstrumentBroadeningS() {
    return getInstrumentBroadening().identifier;
  }

  public double PhaseAndLayerAbsorption(DiffrDataFile adatafile, Sample asample,
                                        Phase aphase, double x, boolean dspacingbase,
                                        boolean energyDispersive, int datasetindex) {
    double[] incidentDiffractionangles = null;
    double pathK = 0.0;

    int phaseindex = asample.getPhase(aphase);
    double quantity, correction = 0.0;//, sintheta = 1.0;
    double[] tiltingAngles = adatafile.getTiltingAngle();
    double omega = getMeasurement().getOmega(tiltingAngles[0], x);
//    System.out.println("tilting : " + tiltingAngles[0] + " " + tiltingAngles[1] + " " + tiltingAngles[2] + " " + tiltingAngles[3]);

      tiltingAngles[0] = omega;
      incidentDiffractionangles = getGeometry().getIncidentAndDiffractionAngles(adatafile, tiltingAngles,
          asample.getSampleAngles(), x);
    DataFileSet adataset = adatafile.getDataFileSet();
    double absorpt;
//    System.out.println("Position + PathK: " + x + " " + " " + pathK);
//    if (!dspacingbase)
//      sintheta = MoreMath.sind(x / 2.0);
    RadiationType rad = getRadiationType();
    for (int i = 0; i < asample.numberOfLayers; i++) {
      quantity = asample.phaseQuantity[i][phaseindex][datasetindex];
//	    System.out.println("Quantity: " + quantity);
      if (quantity >= 0.0) {
//	      System.out.println("Geometry: " + getGeometry().toString());

	      absorpt = getGeometry().getLayerAbsorption_new(asample, rad, i, incidentDiffractionangles, adataset);
        correction += quantity * absorpt;
      }
    }
    return correction;
  }

  public void computeShapeAbsorptionCorrection(DiffrDataFile adatafile, Sample asample,
                                               double[] position, boolean dspacingbase, boolean energyDispersive, double[] intensity) {
    if (position != null && position.length > 0) // not reflectivity
      getGeometry().computeShapeAbsorptionCorrection(adatafile, asample, position, dspacingbase, energyDispersive, intensity,
              getLambdaForTOF(adatafile, position[0]));
  }

  public double getLambdaForTOF(DiffrDataFile adatafile, double position) {
    double toLambda = 0.0f;
    if (getMeasurement().isTOF()) {
      toLambda = (2.0 * MoreMath.sind(Math.abs(getGeometry().getThetaDetector(adatafile,
                       position) / 2.0)));
    }
    return toLambda;
  }

  public double LorentzPolarization(DiffrDataFile adatafile, Sample asample,
                                    double position, boolean dspacingbase, boolean energyDispersive) {
    return getGeometry().LorentzPolarization(adatafile, asample, position, dspacingbase, energyDispersive); /*
            asample.getActiveSubordinateModel(Sample.sampleShapeID).getCorrectionForVelocity(adatafile,
                    this, position, getLambdaForTOF(adatafile, (double) position)); */
  }

  public void LorentzPolarization(DiffrDataFile adatafile, Sample asample,
                                  double[] position, boolean dspacingbase, boolean energyDispersive, double[] intensity) {
    getGeometry().LorentzPolarization(adatafile, asample, position, dspacingbase, energyDispersive, intensity);
  }

  public double getBeamRelatedCorrection(DiffrDataFile adatafile, Sample asample, double position, int pointIndex) {
    return getGeometry().getBeamRelatedCorrection(adatafile, asample, position, pointIndex);
  }

  public void freeAllScaleParameters() {
    if (getParent() != getFilePar().getSelectedSample().getDataSet(0) ||
        getFilePar().getSelectedSample().thinFilmBehaviour() ||
        getFilePar().getSelectedSample().phasesNumber() == 1)
      getIntensity().setRefinableCheckBound();
    refineAllTOFSFBankCoefficients();
    if (MaudPreferences.getBoolean("spectraShift.refineWithScaleFactors", false)) {
      freeThetaParameters();
      refineAllZEROBankCoefficients(false);
    }
    getDetector().freeAllScaleParameters();
  }

  public boolean freeAllBasicParameters() {
    boolean done = getGeometry().freeAllBasicParameters();
    done = getAngularCalibration().freeAllBasicParameters() || done;
    if (!done)
      freeThetaParameters();
    return done;
  }

  public void freeThetaParameters() {
    for (int i = 0; i < getthetaoffsetnumber(); i++)
      getThetaDisplacement(i).setRefinableCheckBound();
  }

  public void boundAllBankCoefficients() {
    AngularCalibration acal = getAngularCalibration();
    if (acal instanceof GSASbankCalibration)
      ((GSASbankCalibration) acal).boundAllBankCoefficients();
    IntensityCalibration ical = getIntensityCalibration();
    if (ical instanceof GSASbankIntCalibration)
      ((GSASbankIntCalibration) ical).boundAllBankCoefficients();
	  InstrumentBroadening ib = getInstrumentBroadening();
	  if (ib instanceof InstrumentBroadeningGSAS1f)
		  ((InstrumentBroadeningGSAS1f) ib).boundAllBankCoefficients();
  }

  public void forceBoundAllBankCoefficients() {
    AngularCalibration acal = getAngularCalibration();
    if (acal instanceof GSASbankCalibration)
      ((GSASbankCalibration) acal).forceBoundAllBankCoefficients();
    IntensityCalibration ical = getIntensityCalibration();
    if (ical instanceof GSASbankIntCalibration)
      ((GSASbankIntCalibration) ical).forceBoundAllBankCoefficients();
	  InstrumentBroadening ib = getInstrumentBroadening();
	  if (ib instanceof InstrumentBroadeningGSAS1f)
		  ((InstrumentBroadeningGSAS1f) ib).forceBoundAllBankCoefficients();
  }

  public boolean refineAllZEROBankCoefficients(boolean forceFree) {
    AngularCalibration acal = getAngularCalibration();
    if (acal instanceof GSASbankCalibration) {
      ((GSASbankCalibration) acal).freeAllZeroParameters(forceFree);
      return true;
    } else {
      boolean done = getGeometry().freeAllBasicParameters();
      done = getAngularCalibration().freeAllBasicParameters() || done;
      if (!done)
        freeThetaParameters();
    }
    return false;
  }

  public void refineAllTOFSFBankCoefficients() {
    IntensityCalibration acal = getIntensityCalibration();
    if (acal instanceof GSASbankIntCalibration)
      ((GSASbankIntCalibration) acal).freeAllTOFSFParameters();
  }

  public void plotFunction(Frame theframe, int index) {
    ParameterFunction function = new PolynomialFunction(parameterloopField[index], isTOF());
    (new PlotParameterFunction(theframe, function)).setVisible(true);
  }

  public void edit(Frame aframe) {
    (new InstrumentD(aframe, this)).setVisible(true);
  }

  public void checkIntensity() {
    setIntensity(Double.toString(getIntensity().getValueD() * 2.0));
  }

  public void multiplyScaleFactorBy(double totalquantity) {
    setIntensity(Double.toString(getIntensity().getValueD() * totalquantity));
  }

  public void forceAllBankIntRefinable() {
    IntensityCalibration acal = getIntensityCalibration();
    if (acal instanceof GSASbankIntCalibration)
      ((GSASbankIntCalibration) acal).forceFreeAllTOFSFParameters();
  }
}

