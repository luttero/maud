/*
 * @(#)DiffrDataFile.java created 1/1/1997 xx
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.cal.*;
import it.unitn.ing.rista.diffr.measurement.Theta2ThetaMeasurement;
import it.unitn.ing.rista.io.cif.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.interfaces.Peak;
//import it.unitn.maud.JNIAltivec;

import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

import static java.lang.System.*;

/**
 * The DiffrDataFile is a class to compute and store a spectrum and its fit.
 * <p/>
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.33 $, $Date: 2006/12/04 14:30:04 $
 * @since JDK1.1
 */


public class DiffrDataFile extends XRDcat {

	public static String pd_meas_scan_method = "_pd_meas_scan_method";
	public static String pd_meas_scan_range_min = "_pd_meas_2theta_range_min";
	public static String pd_meas_scan_range_max = "_pd_meas_2theta_range_max";
	public static String pd_meas_scan_range_inc = "_pd_meas_2theta_range_inc";
	public static String pd_meas_counts_total = "_pd_meas_counts_total";

	public static int DATAFILE_OMEGA = 0;
	public static int DATAFILE_CHI = 1;
	public static int DATAFILE_PHI = 2;
	public static int DATAFILE_ETA = 3;
	public static int DATAFILE_THETA2 = 4;
//	public static int DATAFILE_PIXEL_X = 6;
//	public static int DATAFILE_PiXEL_Y = 7;

	public static String[] diclistc = {
      "_riet_meas_datafile_format",
      "_pd_meas_angle_omega",
      "_pd_meas_angle_chi",
      "_pd_meas_angle_phi",
      "_pd_meas_angle_eta",
			"_pd_meas_angle_2theta",
			"_pd_meas_energy_kev",
//			"_pd_meas_pixel_x",
//			"_pd_meas_pixel_y",
      "_riet_meas_datafile_compute",
      "_riet_meas_datafile_fitting",
      "_pd_meas_detector_id",
//                    "_riet_meas_datafile_bank",
      "_pd_meas_step_count_time",
      "_pd_meas_units_of_intensity",
      "_riet_meas_datafile_as_background",
      "_riet_meas_data_group_count",
      "_riet_datafile_type",
      "_riet_datafile_save_custom",
      "_pd_meas_image_id",
      "_riet_background_interpolated_manual",
		  "_pd_meas_datetime_initiated",
		  "_pd_proc_ls_datafile_weight",
			"_riet_use_count_time",
			"_riet_chebyshev_polynomial_background",

      "_pd_meas_counts_monitor",
      "_riet_par_spec_displac_x_2R",
      "_riet_par_spec_displac_z_2R",
			"_riet_par_fluo_diffr_scale",
			"_rita_shape_abs_velocity_corr_factor",

      "_riet_par_background_pol",
      "_riet_par_2-theta_offset",
      "_riet_par_bkg_file_monitor",

      "_riet_par_background_oscillator"
  };

  public static String[] diclistcrm = {
      "_riet_meas_datafile_format",
      "_pd_meas_orientation_omega",
      "chi angle (deg)",
      "phi angle (deg)",
      "eta angle (deg)",
		  "2theta detector (deg)",
		  "radiation energy (keV)",
//		  "_pd_meas_pixel_x (pixels)",
//		  "_pd_meas_pixel_y (pixels)",
      "_riet_meas_datafile_compute",
      "_riet_meas_datafile_fitting",
      "_pd_meas_detector_id",
//                    "_riet_meas_datafile_bank",
      "_pd_meas_step_count_time",
      "_pd_meas_units_of_intensity",
      "_riet_meas_datafile_as_background",
      "_riet_meas_data_group_count",
      "_riet_datafile_type",
      "_riet_datafile_save_custom",
		  "_pd_meas_image_id",
		  "manual selection of background interpolation points",
		  "_pd_meas_datetime_initiated",
		  "_pd_proc_ls_datafile_weight",
		  "use count time to scale",
		  "use Chebyshev polynomial for background",

      "monitor value (optional scale factor)",
      "sample displacement x/2R",
      "sample displacement z/2R",
		  "scale factor for diffraction in XRF",
		  "_rita_shape_abs_velocity_corr_factor",

      "background polynomial additional coeff ",
      "2-theta or d-spacing offset additional coeff ",
      "additional scale factor for file as background",
      "_riet_par_background_oscillator"
  };

  protected static String[] classlistc = {"it.unitn.ing.rista.diffr.Oscillator"};

  public String[] classlistcs = {};

  protected double[] intensity = null;
  protected double[] intensityCalibrated = null;
  protected double[] weight = null;
  //	protected double[] theoreticalweight;
  //	public double[] dspacing;
  protected double[] twothetaOriginal = null;
	protected double[] twotheta = null;
  protected double[] twothetacalibrated = null;
  protected double[] x_image = null;
  protected double[] y_image = null;
  protected double[] twotheta_image = null;
  protected double[] eta_image = null;
  protected double[] distance_image = null;
  //	public double[] fit;
  public double[] phasesfit = null;
  protected double[] bkgfit = null;
  protected double[] intbkgfit = null;

	protected double[] expbkgfit = null;
  public Vector phaseFit = null;
  public int datanumber;
  public double measurementstep;
  public boolean dspacingbase = false;
  public boolean energyDispersive = false;
  public boolean constantstep = false;
  public double startingvalue;
  public double finalvalue;
  public double radiation = 0.0;
  public double intensitymax;
  public double intensitymin;
  public String title;
  public int startingindex = 0;
  public int finalindex = 0;
  public boolean hasfit = false;
  int theindex = 0;
  boolean calibrated = false;
  boolean refreshBkgComputation = true;
  boolean refreshInterpolatedBkgComputation = true;
	boolean refreshExperimentalBkgComputation = true;
  boolean refreshSpectraComputation = true;
//  boolean computeSpectrum = true;
  public boolean spectrumModified = false;
  boolean generatePlotfile = false;
//  boolean asBackground = false;
  public static int maxAngleNumber = 6;
  final static int asBackgroundID = maxAngleNumber + 6;
  boolean manualBkgInterpolation = false;
  final static int manualBkgInterpolationID = maxAngleNumber + 11;
	final static int useCountTimeToScaleID = maxAngleNumber + 14;
	final static int useChebyshevPolynomialsID = maxAngleNumber + 15;
  double[] manualBkgPoints = null;
  final static int sampleDisplacementYID = 1;
  final static int sampleDisplacementZID = 2;
	boolean useCountTimeToScale = false;
	boolean useChebyshevPolynomials = false;
	final static int countingTimeValueID = maxAngleNumber + 4;

  //	boolean tobeloaded = true;
  String folder = ".";
  public boolean lorentzRestricted = true;
  public boolean intensityNotCalibrated = true;
  public static String CIFXcoord2T = "_pd_proc_2theta_corrected";
  public static String CIFXcoordD = "_pd_proc_d_spacing";
  public static String CIFXcoordEnergy = "_pd_proc_energy";
	public static String intensityCalcCIFstring = "_pd_calc_intensity_total";
	public static String intensityExpCIFstring = "_pd_meas_intensity_total";
  public static String backgroundCIFstring = "_pd_calc_intensity_bkg";
  private double tilting_angles[] = new double[maxAngleNumber];
	private double corrected_tilting_angles[] = new double[maxAngleNumber];
  boolean increasingX = true;
	int lastIndex = 0;
	int oscillatorsNumber = 0;
  double monitorCounts = 1.0;
  public static String[] meas_intensity_unit = {"counts", "cps"};
  double countingTime = 1.0;
	double countingTimeStat = 1.0;
	double datafileWeight = 1.0;
  int groupCount = 1;
  int oldGroupCount = 1;
  DataFileSet theDataFileSet = null;
  int bankNumber = -1;
  int angBankNumber = -1;
  public static int DIFFRACTION_PATTERN = 0;
  public static int DIFFRACTION_IMAGE = 1;
  int type = DIFFRACTION_PATTERN;
  int dataType = DIFFRACTION_PATTERN;
  public int saveCustomID = maxAngleNumber + 9;
  static int thetaDisplacementID = 1;
  double thetaDisplacement[] = null;
  int thetaDisplacementN = 0;

  double absorptionVelocityFactor = 0.0;
//  static int monitorAsBackgroundID = 2;
//  double monitorAsBackground[] = null;
//  int monitorAsBackgroundN = 0;

  boolean reflectivityStats = false;
  int weightSwitch = 0;
	boolean theoreticalWeights = false;
	boolean useNoBkg = false;
	boolean calibratedData = false;
	boolean useCalibratedData = false;


	boolean firstComputation = true;

  public boolean dataLoaded = false;
  //  public boolean readLater = false;
  protected int imageIndex = -1;

  boolean computeMinMax = true;

  boolean theta2thetaMeasurement = true;
//	boolean resetManualInterpolationWithRangeChange = false;

	public int positionsPerPattern = 1;
	public int radiationsNumber = 1;
	int instrumentBroadeningParNumber = 5;

	boolean[] needRestore = null;
	Vector overallVector = null;

	// phaseLorentzPolarization[number of reflection][number of points per pattern]
	private Map<Phase, double[][][]> phaseLorentzPolarization = new Hashtable<>();
	// phaseShapeAbsFactors[number of reflection][number of points per pattern]
	private Map<Phase, double[][][]> phaseShapeAbsFactors = new Hashtable<>();

	// phaseTextureFactors[2(meas,calc)][number of reflection][number of points per pattern]
	private Map<Phase, int[]> phaseReflectionIDs = new Hashtable<>();
	private Map<Phase, double[][][][]> phaseTextureFactors = new Hashtable<>();
	private Map<Phase, double[][][][]> phaseStrainFactors = new Hashtable<>();
	private Map<Phase, double[][][][]> phaseCrystallitesMicrostrains = new Hashtable<>();
	private Map<Phase, double[][][][]> phaseInstBroadFactors = new Hashtable<>();
	private Map<Phase, double[][][][]> phaseBroadFactors = new Hashtable<>();
	// phasePositions[radnumber][number of reflection][number of points per pattern]
	private Map<Phase, double[][][]> phasePositions = new Hashtable<>();
	private Map<Phase, int[][][][]> phaseMinMaxIndices = new Hashtable<>();
	static final int dateTimeFieldID = maxAngleNumber + 12;
	static final int datafileWeightFieldID = maxAngleNumber + 13;
	static final int scaleFactorDiffractionFluoID = 3;
	static final int absorptionFactorID = 4;
//	private String measurementDate;
//	private String measurementTime;

	protected Vector<double[]> indicesDataHoles = new Vector<>();

	public DiffrDataFile(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		setParent(aobj);
		initBaseObject();
		initDatafile(alabel);
  }

/*	public DiffrDataFile(XRDcat aobj, String alabel, boolean fromFile) {
		super(aobj, alabel);
		setParent(aobj);
		initXRD();
		if (fromFile)
			initDatafile(alabel);
		else
			dataLoaded = true;
	}*/

	public DiffrDataFile(XRDcat aobj) {
    this(aobj, "Datafile_x");
  }

  public DiffrDataFile() {
  }

	public void initDatafile(String alabel) {
		String[] folderandname = Misc.getFolderandName(alabel);
		if (!folderandname[0].startsWith("//")) {
			if (getVersion() < 1.2) {
				folderandname[0] = Misc.getAbsolutePath(folderandname[0], Misc.getUserDir() + "/");
			} else {
				if (getFilePar().getCreatorID().startsWith("Rietquan")) {
					if (folderandname[0].length() > 1 && folderandname[0].charAt(1) == ':') {
						folderandname[0] = "//" + folderandname[0];
					} else
						folderandname[0] = Misc.getAbsolutePath(folderandname[0], getDirectory());
				} else
					folderandname[0] = Misc.getAbsolutePath(folderandname[0], getDirectory());
			}
		}
		folder = folderandname[0];
		thelabel = folderandname[1];
//    System.out.println("Path to file: " + folder+thelabel);
		if (!getFilePar().isLoadingFile() || !getFilePar().storeSpectraWithAnalysis())
			dataLoaded = loadData();
//    else
//    readLater = true;
	}

  @Override
  public void initConstant() {
    Nstring = 22;
    Nstringloop = 0;
    Nparameter = 5;
    Nparameterloop = 3;
    Nsubordinate = 0;
    Nsubordinateloop = 1;
  }

  @Override
  public void initDictionary() {
    arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
    arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
    arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
  }

  @Override
  public void initParameters() {
    super.initParameters();
	  for (int i = 0; i < maxAngleNumber; i++)
		  setString(i + 1, "0");
    setCompute(true);
    setBankID("none");
    setGeneratePlotfile(false);
    setAsBackground(false);
    setManualBkgInterpolation(false);
    initializeParameter(0, 1.0, 0.0, 10.0);
    initializeParameter(sampleDisplacementYID, 0.0, -10.0, 10.0);
    initializeParameter(sampleDisplacementZID, 0.0, -10.0, 10.0);
	  initializeParameter(scaleFactorDiffractionFluoID, 1.0, 0.0, 100.0);
	  initializeParameter(absorptionFactorID, 0.0, 0.0, 10000.0);
    setCountTime("1.0");
	  useCountTimeToScale(false);
    setIntensityUnit(meas_intensity_unit[0]);
    setGroupCount(1);
    setDataType(DIFFRACTION_PATTERN);
    setImageIndex(-1);
	  setString(datafileWeightFieldID, "1.0");
	  useChebyshevPolynomials(false);

  }

	@Override
  public void setParent(XRDcat obj) {
    super.setParent(obj);
    theDataFileSet = (DataFileSet) obj;
//		System.out.println("Object :" + this);
//		System.out.println("Class :" + this.getClass());
//		System.out.println("Parent :" + getParent());
//		System.out.println("Setting :" + theDataFileSet);
  }

  public boolean loadData() {
    int indexFile = MultDiffrDataFile.isInCache(thelabel);
//		System.out.println("In cache :" + thelabel);
    return indexFile >= 0 || readallSpectra(getDataFileSet().askForRange);

  }

  public void setFolder(String path) {
    folder = path;
  }

  public String getFolder() {
    return folder;
  }

  public String getNameRelativeToPar() {
//	  System.out.println("Directory: " + getDirectory());
//	  System.out.println("Folder: " + getFolder());
    StringBuffer filenamewithPath = new StringBuffer(Misc.getRelativePath(getDirectory(), getFolder()));
    filenamewithPath.append(toXRDcatString());
    return filenamewithPath.toString();
  }

  public boolean readallSpectra(boolean askForRange) {
	  boolean oldPermission = Constants.refreshTreePermitted;
	  Constants.refreshTreePermitted = false;
//	  System.out.println("Reading data: " + getFolder() + toXRDcatString() + " " + identifier);
    boolean result = readallSpectra();
	  Constants.refreshTreePermitted = oldPermission;
	  return result;
  }

  public boolean readallSpectra() {
    return true;
  }

  public void initData(int number) {
    datanumber = number;
    twotheta = new double[datanumber];
	  twothetaOriginal = new double[datanumber];
	  twothetacalibrated = new double[datanumber];
    if (Integer.parseInt(getDataType()) == DIFFRACTION_IMAGE) {
      x_image = new double[datanumber];
      y_image = new double[datanumber];
	    twotheta_image = new double[datanumber];
	    eta_image = new double[datanumber];
	    distance_image = new double[datanumber];
    }
    intensity = new double[datanumber];
//		intensityCalibrated = new double[datasetsNumber];
    weight = new double[datanumber];
//		theoreticalweight = new double[datasetsNumber];
    phasesfit = new double[datanumber];
    bkgfit = new double[datanumber];
    intbkgfit = new double[datanumber];
	  expbkgfit = new double[datanumber];
    phaseFit = new Vector();
    startingindex = 0;
    finalindex = number;
//	  System.out.println("Init datafile: " + toString() + " " + startingindex + " " + finalindex);
  }

  private boolean hardRangeCut(int min, int max) {
// actually with calibration there is a problem, and we have to avoid it
// so we are disabling the hard range cut
    if (getDataFileSet().getInstrument().provideRealAngularCalibration())
      return false;
    if (!MaudPreferences.getBoolean("datafile.removeDataFromMemory", true))
      return false;
//	System.out.println("cutting range " + min + " " + max);
    realRangeCut(min, max);
//		startingindex = 0;
//		finalindex = datasetsNumber;
    return true;
  }

  public void realRangeCut(int min, int max) {
    checkPhaseFitVector();
    datanumber = max - min;
    twotheta = shrinkRange(twotheta, min, datanumber);
	  twothetaOriginal = shrinkRange(twothetaOriginal, min, datanumber);
	  twothetacalibrated = shrinkRange(twothetacalibrated, min, datanumber);
    if (Integer.parseInt(getDataType()) == DIFFRACTION_IMAGE) {
      x_image = shrinkRange(x_image, min, datanumber);
      y_image = shrinkRange(y_image, min, datanumber);
	    twotheta_image = shrinkRange(twotheta_image, min, datanumber);
	    eta_image = shrinkRange(eta_image, min, datanumber);
	    distance_image = shrinkRange(distance_image, min, datanumber);
    }
    intensity = shrinkRange(intensity, min, datanumber);
    intensityCalibrated = shrinkRange(intensityCalibrated, min, datanumber);
    weight = shrinkRange(weight, min, datanumber);
    phasesfit = shrinkRange(phasesfit, min, datanumber);
    bkgfit = shrinkRange(bkgfit, min, datanumber);
    intbkgfit = shrinkRange(intbkgfit, min, datanumber);
	  expbkgfit = shrinkRange(expbkgfit, min, datanumber);
    for (int i = 0; i < phaseFit.size(); i++) {
      double[] fit = (double[]) phaseFit.elementAt(i);
      phaseFit.setElementAt(shrinkRange(fit, min, datanumber), i);
    }
  }

  public static double[] shrinkRange(double[] oldvector, int min, int datanumber) {
    if (oldvector == null)
      return null;

    double[] newvector = new double[datanumber];

    arraycopy(oldvector, min, newvector, 0, datanumber);

    return newvector;
  }

  @Override
  public void update(boolean firstLoading) {
    if (!dataLoaded) { // && readLater) {
      dataLoaded = loadData();
//      readLater = false;
    }
    super.update(firstLoading);
  }

  public void reloadData() {
    boolean store = getFilePar().storeSpectraWithAnalysis();
    getFilePar().setStoreSpectraOption(false);
    getDataFileSet().removeDatafile(this);
    DiffrDataFile adatafile = (DiffrDataFile) getDataFileSet().addsubordinateloopField(2, getLabel());
    getFilePar().setStoreSpectraOption(store);
//    System.out.println(this+" String " + stringField[5]);
    copyCat(adatafile);
//    System.out.println(adatafile+" String copy " + adatafile.stringField[5]);
  }

  public void setImageIndex(int index) {
    setString(maxAngleNumber + 10, Integer.toString(index));
  }

  public int getImageIndex() {
    return imageIndex;
  }

  public boolean storeSpectraWithAnalysis() {
    return stringField[saveCustomID].equalsIgnoreCase("true") || getFilePar().storeSpectraWithAnalysis();
  }

  @Override
  public void writeCustomObject(BufferedWriter out) {

    if (manualBkgPoints != null) {
      try {
        out.newLine();
        out.write("#custom_object_" + "background_interpolated_points");
        out.newLine();
        out.write(CIFdictionary.loopDecl);
        out.newLine();
        out.write("_riet_background_interpolated_point_position");
        out.newLine();
        for (int ng = 0; ng < manualBkgPoints.length; ng++) {
          out.write(" " + Float.toString((float) manualBkgPoints[ng]));
          out.newLine();
        }
        out.newLine();
        out.write("#end_custom_object_" + "background_interpolated_points");
        out.newLine();
        out.newLine();
      } catch (IOException ioe) {
        System.out.println("Error in writing the interpolated background points for " + toXRDcatString());
      }

    }
    if (!(!dataLoaded || !storeSpectraWithAnalysis())) { // || data not Loaded)
    String orCalib = "false";
    String dspacingBase = "false";
    if (dspacingbase)
      dspacingBase = "true";
    String energydispersive = "false";
    if (energyDispersive)
      energydispersive = "true";
    String constStep = "false";
    if (constantstep)
      constStep = "true";

    try {
      out.newLine();
      out.write("#custom_object_" + "intensity_data");
      out.newLine();
      out.write("_pd_meas_number_of_points " + datanumber);
      out.newLine();
      out.write("_riet_meas_datafile_calibrated " + orCalib);
      out.newLine();
      out.write("_riet_meas_datafile_dspacing_based " + dspacingBase);
      out.newLine();
      out.write("_riet_meas_datafile_energy_dispersive " + energydispersive);
      out.newLine();
      out.write("_riet_meas_datafile_constant_step " + constStep);
      out.newLine();
      out.write(CIFdictionary.loopDecl);
      out.newLine();
      if (Integer.parseInt(getDataType()) == DIFFRACTION_IMAGE) {
        out.write("_pd_meas_position _pd_meas_position_x _pd_meas_position_y _pd_meas_intensity_total _pd_meas_intensity_sigma"); // _dummy_2theta _dummy_eta _dummy_distance");
        out.newLine();
        for (int ng = 0; ng < datanumber; ng++) {
          out.write(" " + twothetaOriginal[ng] + " " + x_image[ng] + " " + y_image[ng] + " " + intensity[ng] + " " + weight[ng]); // + " " + twotheta_image[ng] + " " + eta_image[ng] + " " + distance_image[ng]);
          out.newLine();
        }
      } else {
        out.write("_pd_meas_position _pd_meas_intensity_total _pd_meas_intensity_sigma");
        out.newLine();
        for (int ng = 0; ng < datanumber; ng++) {
          out.write(" " + twothetaOriginal[ng] + " " + intensity[ng] + " " + weight[ng]);
          out.newLine();
        }
      }
      out.newLine();
      out.write("#end_custom_object_" + "intensity_data");
      out.newLine();
      out.newLine();
    } catch (IOException ioe) {
      System.out.println("Error in writing the intensity data for " + toXRDcatString());
	    ioe.printStackTrace();
	    try {
		    out.newLine();
		    out.write("#end_custom_object_" + "intensity_data");
		    out.newLine();
	    } catch (IOException e) {
		    e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
	    }
    }
    }
	  Sample asample = getFilePar().getActiveSample();
	  radiationsNumber = getDataFileSet().getInstrument().getRadiationType().getLinesCount();
	  if (!getFilePar().compactSavingTextureFactors() && radiationsNumber < 5) { // we do not save the texture factors in compact saving
		  try {
			  out.newLine();
			  out.write("#custom_object_texture_factors");
			  out.newLine();
			  out.write(CIFdictionary.texture_points_number + " " + positionsPerPattern);
			  out.newLine();
			  out.write(CIFdictionary.texture_radiations_number + " " + radiationsNumber);
			  out.newLine();
			  for (int i = 0; i < asample.phasesNumber(); i++) {
				  Phase phase = asample.getPhase(i);
				  String blockID = "phase_" + phase.toXRDcatString();
				  CIFDataBlock.writeBlockDecl(out, blockID, this);
				  out.newLine();
				  out.write(CIFdictionary.loopDecl);
				  out.newLine();
				  out.write(CIFdictionary.refln_h + " ");
				  out.write(CIFdictionary.refln_k + " ");
				  out.write(CIFdictionary.refln_l + " ");
				  out.write(CIFdictionary.texture_factor_meas + " ");
				  out.write(CIFdictionary.texture_factor_calc + " ");
				  out.write(CIFdictionary.texture_factor_point + " ");
				  out.write(CIFdictionary.texture_radiation_point);
				  out.newLine();
//				String waveS = Fmt.format(wave[i]);

				  double[][][][] textureFactors = getTextureFactors(phase);
				  int reflNumber = phase.gethklNumber();
				  for (int j = 0; j < reflNumber && j < textureFactors[0].length; j++) {
					  Reflection refl = phase.getReflex(j);
					  for (int ppp = 0; ppp < positionsPerPattern; ppp++)
					  for (int ij = 0; ij < radiationsNumber && ij < textureFactors[0][0][0].length; ij++) {
						  if (refl != null && textureFactors != null) {
							  out.write(refl.getH() + " " + refl.getK() + " " + refl.getL() + " " + Fmt.format(textureFactors[0][j][ppp][ij]) +
									  " " + Fmt.format(textureFactors[1][j][ppp][ij]) + " " + ppp + " " + ij);
							  out.newLine();
						  }
					  }
				  }
				  out.newLine();
			  }
			  out.newLine();
			  out.write("#end_custom_object_texture_factors");
			  out.newLine();
			  out.newLine();
		  } catch (Exception ioe) {
			  try {
				  ioe.printStackTrace();
				  out.newLine();
				  System.out.println("Error in writing the texture factors for " + toXRDcatString());
				  out.write("#end_custom_object_" + "texture_factors");
				  out.newLine();
			  } catch (IOException e) {
				  e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
			  }
		  }
	  }

  }

  @Override
  public void readCustomObject(CIFtoken ciffile) {
    // to be override by subclasses
    // the default read and do nothing
    if (ciffile.thestring.indexOf("intensity_data") > 0) {
      int tokentype;
      boolean endofInput = false;
      int aindex = 0, bindex = 0, posindex = -1, xposindex = -1, yposindex = -1, intindex = 0, sigindex = 0;
      calibratedData = false;
      boolean loopStart = false;

//      System.out.println("Custom " + this.toXRDcatString());
      try {
        do {
          tokentype = ciffile.nextToken();
//        System.out.println(tokentype + " " + ciffile.thestring);
          switch (tokentype) {
            case CIFtoken.TT_CIFE:
              // should be the CIF entry for odf values
              String thecife = ciffile.thestring;
              if (thecife.equalsIgnoreCase("_pd_meas_number_of_points")) {
                int newtoken = ciffile.nextToken();
                if (newtoken == CIFtoken.TT_NUMBER)
                  initData(Integer.parseInt(ciffile.thestring));
                else {
                  out.println("No number of data points for spectrum " + this.toXRDcatString());
                  return;
                }
              } else if (thecife.equalsIgnoreCase("_riet_meas_datafile_calibrated")) {
                ciffile.nextToken();
                if (ciffile.thestring.equalsIgnoreCase("true"))
                  calibratedData = false; // true;       no more
              } else if (thecife.equalsIgnoreCase("_riet_meas_datafile_dspacing_based")) {
                ciffile.nextToken();
                dspacingbase = ciffile.thestring.equalsIgnoreCase("true");
              } else if (thecife.equalsIgnoreCase("_riet_meas_datafile_energy_dispersive")) {
                ciffile.nextToken();
                energyDispersive = ciffile.thestring.equalsIgnoreCase("true");
              } else if (thecife.equalsIgnoreCase("_riet_meas_datafile_constant_step")) {
                ciffile.nextToken();
                constantstep = ciffile.thestring.equalsIgnoreCase("true");
              } else if (thecife.equalsIgnoreCase("_pd_meas_position")) {
                posindex = bindex++;
              } else if (thecife.equalsIgnoreCase("_pd_meas_position_x")) {
                xposindex = bindex++;
              } else if (thecife.equalsIgnoreCase("_pd_meas_position_y")) {
                yposindex = bindex++;
              } else if (thecife.equalsIgnoreCase(intensityExpCIFstring)) {
                intindex = bindex++;
              } else if (thecife.equalsIgnoreCase("_pd_meas_intensity_sigma")) {
                sigindex = bindex++;
              }
              break;
            case CIFtoken.TT_LOOP:
              // start the loop for the values here
              aindex = 0;
              bindex = 0;
              loopStart = true;
              break;
            case CIFtoken.TT_NUMBER:
              if (!loopStart)
                break;
              bindex = 0;
              do {
                if (bindex == posindex) {
                  if (calibratedData)
                    setCalibratedXData(aindex, Double.parseDouble(ciffile.thestring));
                  else
                    setXData(aindex, Double.parseDouble(ciffile.thestring));
                } else if (bindex == intindex) {
                  setYData(aindex, Double.parseDouble(ciffile.thestring));
                } else if (bindex == sigindex) {
                  setWeight(aindex, Double.parseDouble(ciffile.thestring));
                } else if (bindex == xposindex) {
                  setXImage(aindex, Double.parseDouble(ciffile.thestring));
                } else if (bindex == yposindex) {
                  setYImage(aindex, Double.parseDouble(ciffile.thestring));
                }
                bindex++;
                if (bindex > sigindex) {
                  bindex = 0;
                  aindex++;
                }
//              System.out.println(aindex + " " + bindex + " " + ciffile.thestring);
                tokentype = ciffile.nextToken();
              } while ((tokentype != CIFtoken.TT_CUSTOM_END && aindex < datanumber) &&
                  (tokentype != CIFtoken.TT_EOF && !endofInput));
              ciffile.pushBack();
              break;
            case CIFtoken.TT_CUSTOM_END:
              // subordinate loop
              endofInput = true;
              break;
            default: {
            }
          }
        } while (tokentype != CIFtoken.TT_EOF && !endofInput);
        dataLoaded = true;
      } catch (IOException ioe) {
        ioe.printStackTrace();
        out.println("IO exception in custom object for " + toXRDcatString());
      }
    } else if (ciffile.thestring.indexOf("background_interpolated_points") > 0) {
      int tokentype;
      boolean endofInput = false;
      boolean loopStart = false;
      int aindex = 0, posindex = -1;
      Vector points = new Vector(10, 10);
      try {
        do {
          tokentype = ciffile.nextToken();
//        System.out.println(tokentype + " " + ciffile.thestring);
          switch (tokentype) {
            case CIFtoken.TT_CIFE:
              // should be the CIF entry for odf values
              String thecife = ciffile.thestring;
              if (thecife.equalsIgnoreCase("_riet_background_interpolated_point_position")) {
                posindex = aindex++;
              }
              break;
            case CIFtoken.TT_LOOP:
              // start the loop for the values here
              loopStart = true;
              aindex = 0;
              break;
            case CIFtoken.TT_NUMBER:
              if (!loopStart)
                break;
              aindex = 0;
              do {
                if (aindex == posindex) {
                  points.add(Double.parseDouble(ciffile.thestring));
                }
//                aindex++;
//              System.out.println(aindex + " " + bindex + " " + ciffile.thestring);
                tokentype = ciffile.nextToken();
              } while ((tokentype != CIFtoken.TT_CUSTOM_END) &&
                  (tokentype != CIFtoken.TT_EOF && !endofInput));
              ciffile.pushBack();
              break;
            case CIFtoken.TT_CUSTOM_END:
              // subordinate loop
              endofInput = true;
              break;
            default: {
            }
          }
        } while (tokentype != CIFtoken.TT_EOF && !endofInput);
        int number = points.size();
        if (number > 0) {
          manualBkgPoints = new double[number];
          for (int i = 0; i < number; i++)
            manualBkgPoints[i] = (Double) points.get(i);
          setManualBkgInterpolation(true);
//          System.out.println("Manual interpolated points loaded for " + this.getLabel() + ", number = " + number);
        }
      } catch (IOException ioe) {
        ioe.printStackTrace();
        out.println("IO exception in custom object for " + toXRDcatString());
      }
    } else if (ciffile.thestring.indexOf("texture_factors") > 0) {

	  int tokentype;
//		XRDcat theobj = null;
	  boolean endofInput = false;
	  int cifentry = 0, tmpVindex = 0;
	  int[] cifindex = new int[6];
	  Vector cifVector = new Vector(0, 1);
	  Vector[] tmpVector = null;
	  overallVector = new Vector(0, 1);
	  boolean newLoop = false;
	  boolean startLoop = false;
	  int maxCIFentries = 7;
	  radiationsNumber = 1;

	  try {
		  do {
			  tokentype = ciffile.nextToken();
			  switch (tokentype) {
				  case CIFtoken.TT_DATA:
				  case CIFtoken.TT_PHASE:
					  if (tmpVector != null) {
						  overallVector.addElement(tmpVector);
					  }
//					  System.out.println("Reading texture factors for phase: " + overallVector.size());
//						phaseindex++;
					  tmpVindex = 0;
					  tmpVector = null;
					  cifVector = new Vector(0, 1);
					  cifentry = 0;
					  newLoop = false;
					  break;
				  case CIFtoken.TT_CIFE:
					  // CIF item
					  String thecife = ciffile.thestring;
					  if (thecife.equalsIgnoreCase(CIFdictionary.texture_points_number)) {
						  int newtoken = ciffile.nextToken();
						  if (newtoken == CIFtoken.TT_NUMBER)
							  positionsPerPattern = Integer.parseInt(ciffile.thestring);
					  } else if (thecife.equalsIgnoreCase(CIFdictionary.texture_radiations_number)) {
					   int newtoken = ciffile.nextToken();
					   if (newtoken == CIFtoken.TT_NUMBER)
						  radiationsNumber = Integer.parseInt(ciffile.thestring);
				      } else
						  cifVector.addElement(thecife);
					  break;
				  case CIFtoken.TT_LOOP:
					  // start the loop for the values here
					  newLoop = true;
					  startLoop = true;
					  break;
				  case CIFtoken.TT_NUMBER:
					  if (!newLoop)
						  break;
					  if (startLoop) {
						  cifindex = new int[cifVector.size()];
						  tmpVindex = 0;
						  for (int i = 0; i < cifVector.size(); i++) {
							  thecife = (String) cifVector.elementAt(i);
							  if (thecife.equalsIgnoreCase(CIFdictionary.refln_h)) {
								  cifindex[i] = 0;
								  tmpVindex++;
							  } else if (thecife.equalsIgnoreCase(CIFdictionary.refln_k)) {
								  cifindex[i] = 1;
								  tmpVindex++;
							  } else if (thecife.equalsIgnoreCase(CIFdictionary.refln_l)) {
								  cifindex[i] = 2;
								  tmpVindex++;
							  } else if (thecife.equalsIgnoreCase(CIFdictionary.texture_factor_meas)) {
								  cifindex[i] = 3;
								  tmpVindex++;
							  } else if (thecife.equalsIgnoreCase(CIFdictionary.texture_factor_calc)) {
								  cifindex[i] = 4;
								  tmpVindex++;
							  } else if (thecife.equalsIgnoreCase(CIFdictionary.texture_factor_point)) {
								  cifindex[i] = 5;
								  tmpVindex++;
							  } else if (thecife.equalsIgnoreCase(CIFdictionary.texture_radiation_point)) {
								  cifindex[i] = 6;
								  tmpVindex++;
							  } else
								  cifindex[i] = -1;
						  }
						  startLoop = false;
						  cifVector.removeAllElements();
					  }
					  if (tmpVector == null) {
						  tmpVector = new Vector[maxCIFentries];
						  for (int i = 0; i < maxCIFentries; i++)
							  tmpVector[i] = new Vector(0, 10);
					  }
					  if (cifindex[cifentry] >= 0) {
						  double[] value = new double[1];
						  value[0] = ciffile.thevalue;
						  tmpVector[cifindex[cifentry]].addElement(value);
					  }
					  cifentry++;
					  if (cifentry == tmpVindex)
						  cifentry = 0;
					  break;
				  case CIFtoken.TT_CUSTOM_END:
					  if (tmpVector != null) {
						  overallVector.addElement(tmpVector);
					  }
					  endofInput = true;
					  break;
				  default: {
				  }
			  }
		  } while (tokentype != CIFtoken.TT_EOF && !endofInput);
//      System.out.println("Custom object loaded!");
		  if (overallVector.size() > 0) {
		    needRestore = new boolean[overallVector.size()];
		    for (int i = 0; i < overallVector.size(); i++)
			    needRestore[i] = true;
	    } else
			  needRestore = null;
//			notLoaded = false;
	  } catch (IOException ioe) {
		  out.println("IO exception in custom object for " + toXRDcatString());
	  }
    }

  }

  @Override
  protected void endOfReadingReached() {
    if (!dataLoaded) {
      reloadData();
    }
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
        -0.1, 0.1, false, 0.01));
  }

/*	public void notifyParameterChanged(Parameter source) {
		FilePar filepar = getFilePar();
		if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
			for (int j = 0; j < numberofelementPL(0); j++) {
				Parameter apar = (Parameter) parameterloopField[0].elementAt(j);
				if (apar == source) {
					notifyParameterChanged(source, Constants.BKG_PARAMETER_CHANGED);
					return;
				}
			}
			for (int i = 1; i < Nparameterloop; i++) {
				for (int j = 0; j < numberofelementPL(i); j++) {
					Parameter apar = (Parameter) parameterloopField[i].elementAt(j);
					if (apar == source) {
						notifyParameterChanged(source, Constants.PARAMETER_CHANGED);
						return;
					}
				}
			}
			if (getAsBackgroundPermission() && parameterField[0] == source) {
				notifyParameterChanged(source, Constants.BKG_FILE_CHANGED);
				return;
			}
			super.notifyParameterChanged(source);
		}
	}*/

@Override
  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      for (int i = 0; i < Nparameterloop; i++) {
        for (int j = 0; j < numberofelementPL(i); j++) {
          Parameter apar = (Parameter) parameterloopField[i].elementAt(j);
          if (apar == source) {
            if (i == 0) {
              notifyParameterChanged(source, Constants.BKG_PARAMETER_CHANGED, -1);
              return;
            } else if (i == 2) {
	            notifyParameterChanged(source, Constants.BKG_FILE_CHANGED, -1);
	            return;
            } else {
              notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED, -1);
	            notifyParameterChanged(source, Constants.ANGULAR_CALIBRATION, -1);
              return;
            }
          }
        }
      }
      for (int i = 0; i < Nparameter; i++) {
        if (parameterField[i] == source) {
          if (i == 0) {
            notifyParameterChanged(source, Constants.BKG_PARAMETER_CHANGED, -1);
            notifyParameterChanged(source, Constants.BKG_FILE_CHANGED, -1);
            return;
          } else {
          	notifyParameterChanged(source, Constants.BKG_FILE_CHANGED, -1);
          	notifyParameterChanged(source, Constants.BEAM_INTENSITY_CHANGED, -1);
            notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED, -1);
            notifyParameterChanged(source, Constants.ANGULAR_CALIBRATION, -1);
            return;
          }
        }
      }
    }
    super.notifyParameterChanged(source);
  }

  @Override
  public void setRefreshAllStatus() {
    super.setRefreshAllStatus();
    refreshBkgComputation = true;
    refreshSpectraComputation = true;
    refreshInterpolatedBkgComputation = true;
	  refreshExperimentalBkgComputation = true;
  }

  @Override
  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(firstLoading);
	  DataFileSet dataset = getDataFileSet(); //(DataFileSet) getParent();
	  Instrument ainstrument = dataset.getInstrument();
    generatePlotfile = getPlotfileString().equalsIgnoreCase("true");
//    asBackground = getAsBackgroundString().equalsIgnoreCase("true");
    manualBkgInterpolation = getManualBkgInterpolationString().equalsIgnoreCase("true");
	  useCountTimeToScale = useCountTimeToScaleString().equalsIgnoreCase("true");
	  useChebyshevPolynomials = useChebyshevPolynomialsString().equalsIgnoreCase("true");
//  computeSpectrum = !getAsBackgroundString().equalsIgnoreCase("true") && getComputeString().equalsIgnoreCase("true");
    dataType = Integer.parseInt(getDataType());

//    dataset.updateStringtoDoubleBuffering();
	  for (int i = 0; i < maxAngleNumber; i++)
		  tilting_angles[i] = Double.parseDouble(getString(i + 1));
	  for (int i = 0; i < 4; i++)
		  corrected_tilting_angles[i] = tilting_angles[i] + getDataFileSet().getDisalignementAngles()[i];
	  for (int i = 4; i < maxAngleNumber; i++)
		  corrected_tilting_angles[i] = tilting_angles[i];

	  if (MaudPreferences.getBoolean("testing.invertEta", true) && !MaudPreferences.getBoolean("testing.useNewRotationMatrices", true))
		  corrected_tilting_angles[3] = -corrected_tilting_angles[3];

    lorentzRestricted = dataset.isLorentzRestricted();
    oscillatorsNumber = backgOscillatorsNumber();
    for (int i = 0; i < oscillatorsNumber; i++)
      getBkgOscillator(i).preparecomputing();

    reflectivityStats = needReflectivityStatistic();
	  weightSwitch = getFilePar().getWeightingSchemeSwitch();
	  theoreticalWeights = getFilePar().theoreticalWeight();
	  useNoBkg = getFilePar().useNoBkgForWeightingScheme();
	  useCalibratedData = getFilePar().useIntensityCalibratedForWeights() &&
			  getDataFileSet().getInstrument().getIntensityCalibration() != null && intensityCalibrated != null;

    if (ainstrument != null) {
      IntensityCalibration intcal = ainstrument.getIntensityCalibration();
      if (intcal != null)
        try {
          bankNumber = intcal.getBankNumber(getBankID());
        } catch (Exception e) {
	        e.printStackTrace();
         // LogSystem.printStackTrace(e);
        }
      Detector det = ainstrument.getDetector();
      if (det != null)
        angBankNumber = det.getBankNumber(getBankID());
      if (angBankNumber == -1) {
        AngularCalibration acal = ainstrument.getAngularCalibration();
        if (acal != null)
          try {
            angBankNumber = acal.getBankNumber(getBankID());
          } catch (Exception e) {
	          e.printStackTrace();
            // LogSystem.printStackTrace(e);
          }
      }

    }
	  countingTime = Double.valueOf(getCountTime());
    switch (getIntensityUnitControl()) {
      case 0:
        countingTimeStat = 1.0;
        break;
      case 1:
        countingTimeStat = countingTime;
        break;
      default: {
      }
    }
    imageIndex = Integer.parseInt(getString(maxAngleNumber + 10));
    checkGroupCount();
    checkStep();
    theta2thetaMeasurement = ainstrument.getMeasurement() instanceof Theta2ThetaMeasurement;
	  datafileWeight = Double.parseDouble(getString(datafileWeightFieldID));

	  if (ainstrument.isTOF() && corrected_tilting_angles[4] == 0) {
		  corrected_tilting_angles[4] = ainstrument.getDetector().getThetaDetector(this, 0);
	  }
	  for (int i = 5; i < maxAngleNumber; i++)
		  corrected_tilting_angles[i] = tilting_angles[i];
  }

	public double getDatafileWeight() {
		return datafileWeight * getDataFileSet().getDatasetWeight();
	}

	public void setDatafileWeight(String value) {
		setString(datafileWeightFieldID, value);
	}

	public String getDatafileWeightString() {
		return getString(datafileWeightFieldID);
	}

	public void checkStep() {
//    System.out.println(startingindex+" "+finalindex);
    if (finalindex == 0)
      finalindex = datanumber;
    if (finalindex > startingindex + 1) {
      double step1 = Math.abs(getXData(startingindex + 1) - getXData(startingindex));
      double step2 = Math.abs(getXData(finalindex - 1) - getXData(finalindex - 2));
      constantstep = Math.abs(step1 - step2) < 1.0E-9;
      measurementstep = (step1 + step2) / 2.0;
    }
  }

  public double getStep() {
    if (measurementstep == 0.0)
      checkStep();
    return measurementstep;
  }

  public void checkGroupCount() {
    groupCount = Integer.parseInt(getGroupCount());
    if (groupCount != oldGroupCount) {
      if (oldGroupCount != 1) {
        out.println("Warning: data was already grouped differently! Save the analysis and reload it to " +
                      "complete the different grouping");
      } else {
        groupData(groupCount);
        oldGroupCount = groupCount;
      }
    }
  }

  private void groupData(int groupControlNumber) {
    //To change body of created methods use File | Settings | File Templates.
//      datasetsNumber = number;
    checkPhaseFitVector();
    twotheta = getReducedArray(twotheta, datanumber, groupControlNumber);
	  twothetaOriginal = getReducedArray(twothetaOriginal, datanumber, groupControlNumber);
	  twothetacalibrated = getReducedArray(twothetacalibrated, datanumber, groupControlNumber);
    if (Integer.parseInt(getDataType()) == DIFFRACTION_IMAGE) {
      x_image = getReducedArray(x_image, datanumber, groupControlNumber);
      y_image = getReducedArray(y_image, datanumber, groupControlNumber);
	    twotheta_image = getReducedArray(twotheta_image, datanumber, groupControlNumber);
	    eta_image = getReducedArray(eta_image, datanumber, groupControlNumber);
	    distance_image = getReducedArray(distance_image, datanumber, groupControlNumber);
    }
    intensity = getReducedArray(intensity, datanumber, groupControlNumber);
    weight = getReducedArray(weight, datanumber, groupControlNumber);
    phasesfit = getReducedArray(phasesfit, datanumber, groupControlNumber);
    bkgfit = getReducedArray(bkgfit, datanumber, groupControlNumber);
    intbkgfit = getReducedArray(intbkgfit, datanumber, groupControlNumber);
	  expbkgfit = getReducedArray(expbkgfit, datanumber, groupControlNumber);
    for (int i = 0; i < phaseFit.size(); i++) {
      double[] fit = (double[]) phaseFit.elementAt(i);
      phaseFit.setElementAt(getReducedArray(fit, datanumber, groupControlNumber), i);
    }
    datanumber /= groupControlNumber;
    startingindex = 0;
    finalindex = datanumber;
	  i_deltaX = Math.abs(1.0 / (getXData(finalindex - 1) - getXData(startingindex)));

  }

  private static double[] getReducedArray(double[] oldArray, int oldDataNumber, int groupControlNumber) {
    int newDataNumber = oldDataNumber / groupControlNumber;
    double[] newArray = new double[newDataNumber];
    for (int i = 0, j = 0; i < newDataNumber; i++, j += groupControlNumber) {
      for (int k = 0; k < groupControlNumber; k++)
        newArray[i] += oldArray[j + k];
      newArray[i] /= groupControlNumber;
    }
    return newArray;
  }

  @Override
  public void updateParametertoDoubleBuffering(boolean firstLoading) {

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);

    if (MaudPreferences.getBoolean("datafile.useMonitorCounts", true))
      monitorCounts = getMonitorCountsValue();
    thetaDisplacement = getParameterLoopVector(thetaDisplacementID);
    thetaDisplacementN = numberOfLoopParameters[thetaDisplacementID];

    for (int i = 0; i < thetaDisplacementN; i++)
      ((Parameter) getThetaDisplacementList().elementAt(i)).setMinimumSignificantValue(Math.pow(0.001, i + 1));

    int numberFileBackground = checkNumberBkgMonitor();
    for (int i = 0; i < numberFileBackground; i++) {

    }

	  absorptionVelocityFactor = getParameterValue(absorptionFactorID);
//    if (thetaDisplacementN > 0)
//      System.out.println("Update " + this.getLabel() + " " + thetaDisplacement[0]);
  }

  public int checkNumberBkgMonitor() {
    int numberFileBackground = getDataFileSet().getNumberOfBackgroundFiles();
    return numberFileBackground;
  }

  public void setCompute(boolean control) {
    if (control) {
      setString(maxAngleNumber + 1, "true");
      setAsBackground(false);
    } else
      setString(maxAngleNumber + 1, "false");
  }

  public String getComputeString() {
    return getString(maxAngleNumber + 1);
  }

  public boolean getComputePermission() {
    return !getAsBackgroundPermission() && getComputeString().equalsIgnoreCase("true");
  }

  public void setGeneratePlotfile(boolean control) {
    if (control)
      setString(maxAngleNumber + 2, "true");
    else
      setString(maxAngleNumber + 2, "false");
  }

  public String getPlotfileString() {
    return getString(maxAngleNumber + 2);
  }

  public boolean getPlotfilePermission() {
    return generatePlotfile;
  }

  public void setAsBackground(boolean control) {
    if (control) {
      setString(asBackgroundID, "true");
      setCompute(false);
    } else
      setString(asBackgroundID, "false");
  }

  public String getAsBackgroundString() {
    return getString(asBackgroundID);
  }

  public boolean getAsBackgroundPermission() {
    return getAsBackgroundString().equalsIgnoreCase("true");
  }

  public void setManualBkgInterpolation(boolean control) {
    if (control) {
      setString(manualBkgInterpolationID, "true");
	    manualBkgInterpolation = true;
    } else {
      setString(manualBkgInterpolationID, "false");
	    manualBkgInterpolation = false;
    }
  }

  public String getManualBkgInterpolationString() {
    return getString(manualBkgInterpolationID);
  }

  public boolean getManualBkgInterpolation() {
    return manualBkgInterpolation;
  }

  public void setIndex(int index) {
    theindex = index;
  }

  public int getIndex() {
    return theindex;
  }

  public void setFormat(String aformat) {
    setString(0, aformat);
  }

  public String getFormat() {
    return getString(0);
  }

  public void setBankID(String bankID) {
    setString(maxAngleNumber + 3, bankID);
  }

  public String getBankID() {
    return getString(maxAngleNumber + 3);
  }

  public void setCountTime(String time) {
    setString(countingTimeValueID, time);
  }

  public String getCountTime() {
    return getString(countingTimeValueID);
  }

  public double getCountTimeValue() {
    return countingTime;
  }

	public double getCountTimeValueForStatistic() {
		return countingTimeStat;
	}

	public void useCountTimeToScale(boolean value)
	{
		if (value)
			setString(useCountTimeToScaleID, "true");
		else
			setString(useCountTimeToScaleID, "false");
	}

	public String useCountTimeToScaleString() {
		return getString(useCountTimeToScaleID);
	}

	public boolean useCountTimeToScale() {
//		System.out.println("useCountTimeToScale = " + useCountTimeToScale);
		return useCountTimeToScale;
	}

	public void useChebyshevPolynomials(boolean value)
	{
		if (value)
			setString(useChebyshevPolynomialsID, "true");
		else
			setString(useChebyshevPolynomialsID, "false");
	}

	public String useChebyshevPolynomialsString() {
		return getString(useChebyshevPolynomialsID);
	}

	public boolean useChebyshevPolynomials() {
		return useChebyshevPolynomials;
	}

	public void setCPSmeasurement() {
    setIntensityUnit(meas_intensity_unit[1]);
  }

  public void setIntensityUnit(String unit) {
    setString(maxAngleNumber + 5, unit);
  }

  public String getIntensityUnit() {
    return getString(maxAngleNumber + 5);
  }

  public int getIntensityUnitControl() {
    int intUnitNumber = meas_intensity_unit.length;
    for (int i = 0; i < intUnitNumber; i++)
      if (meas_intensity_unit[i].equalsIgnoreCase(getIntensityUnit()))
        return i;
    return 0;
  }

  public void setGroupCount(String count) {
    setString(maxAngleNumber + 7, count);
  }

  public void setGroupCount(int count) {
    setString(maxAngleNumber + 7, Integer.toString(count));
  }

  public String getGroupCount() {
    return getString(maxAngleNumber + 7);
  }

/*  public double getGroupCountValue() {
    return groupCount;
  }*/

  public void setDataType(int type) {
    setString(maxAngleNumber + 8, Integer.toString(type));
  }

  public String getDataType() {
    return getString(maxAngleNumber + 8);
  }

	public double getAngleValue(int order) {
		if (order < 0 || order >= maxAngleNumber)
			return -9999999.99;
		return Double.parseDouble(getString(order + 1));
	}

	public void setAngleValue(int order, double value) {
		if (order < 0 || order >= maxAngleNumber)
			return;
		setString(order + 1, Double.toString(value));
	}

	public double getOmegaValue() {
		return corrected_tilting_angles[0];
	}

	public double getChiValue() {
		return corrected_tilting_angles[1];
	}

	public double getPhiValue() {
		return corrected_tilting_angles[2];
	}

	public double getEtaValue() {
		return corrected_tilting_angles[3];
	}

	public double get2ThetaValue() {
		return corrected_tilting_angles[4];
	}

	public double getValueEnergyInKeV() {
		return corrected_tilting_angles[5];
	}
 /* public void setOmega(String angle) {
    setString(1, angle);
  }

  public void setOmega(double angle) {
    setOmega(Double.toString(angle));
  }

  public String getOmega() {
    return getString(1);
  }

  public String getFormattedOmega() {
    float omega = (float) Double.valueOf(getString(1)).doubleValue();
    return Float.toString(omega);
  }

  public void setChi(String angle) {
    setString(2, angle);
  }

  public void setChi(double angle) {
    setChi(Double.toString(angle));
  }

  public String getChi() {
    return getString(2);
  }

  public String getFormattedChi() {
    float chi = (float) Double.valueOf(getString(2)).doubleValue();
    return Float.toString(chi);
  }

  public void setPhi(String angle) {
    setString(3, angle);
  }

  public void setPhi(double angle) {
    setPhi(Double.toString(angle));
  }

  public String getPhi() {
    return getString(3);
  }

  public String getFormattedPhi() {
    float phi = (float) Double.valueOf(getString(3)).doubleValue();
    return Float.toString(phi);
  }

  public void setEta(String angle) {
    setString(4, angle);
  }

  public void setEta(double angle) {
    setEta(Double.toString(angle));
  }

  public String getEta() {
    return getString(4);
  }

  public String getFormattedEta() {
    float eta = (float) Double.valueOf(getString(4)).doubleValue();
    return Float.toString(eta);
  }

	public void set2Theta(String angle) {
		setString(5, angle);
	}

	public void set2Theta(double angle) {
		set2Theta(Double.toString(angle));
	}

	public String get2Theta() {
		return getString(5);
	}

	public String getFormatted2Theta() {
		return Float.toString((float) Double.valueOf(getString(5)).doubleValue());
	}

	public void setEnergyInKeV(String energy) {
		setString(6, energy);
	}

	public void setEnergyInKeV(double energy) {
		setEnergyInKeV(Double.toString(energy));
	}

	public String getEnergyInKeV() {
		return getString(6);
	}

	public String getFormattedEnergyInKeV() {
		return Float.toString((float) Double.valueOf(getString(6)).doubleValue());
	}

	*/

	public void setNewAngles(int[] mult, double[] offset) {
    updateStringtoDoubleBuffering(false);
		for (int i = 0; i < maxAngleNumber; i++)
			setString(i + 1, Double.toString(offset[i] + Double.valueOf(getString(i + 1)) * mult[i]));
    for (int i = 0; i < this.datanumber; i++)
      setCalibratedXData(i, this.getXDataOriginal(i) * mult[maxAngleNumber] + offset[maxAngleNumber]);
		calibrated = false;
  }

  public Parameter getMonitorCounts() {
    return parameterField[0];
  }

  public double getMonitorCountsValue() {
    return parameterField[0].getValueD();
  }

  public void setMonitorCounts(double value) {
    parameterField[0].setValue(value);
  }

  public void setMonitorCounts(String value) {
    parameterField[0].setValue(value);
  }

  public static int getBackgroundID() {
    return 0;
  }

  public int numbercoefbackg() {
    return numberofelementPL(getBackgroundID());
  }

  public Parameter getbackgcoef(int index) {
    return (Parameter) parameterloopField[getBackgroundID()].elementAt(index);
  }

  @Override
  public boolean isActive(XRDcat acat) {
    return (getComputePermission() || getAsBackgroundPermission()) && super.isActive(acat);
  }

  public void addBackgroundParameter() {
    int numb = numberofelementPL(getBackgroundID());
    addparameterloopField(getBackgroundID(), new Parameter(this, getParameterString(getBackgroundID(), numb), 0,
        -10000.0 / (numb + 1) / (numb + 1) / (numb + 1), 10000.0 / (numb + 1) / (numb + 1) / (numb + 1)));
  }

  public void removeAllBackgroundParameters() {
    removeAllPLField(getBackgroundID());
  }

  public int backgOscillatorsNumber() {
    return numberofelementSubL(0);
  }

  public Oscillator getBkgOscillator(int index) {
    return (Oscillator) subordinateloopField[0].elementAt(index);
  }

  public void addShiftParameter() {
    int numb = numberofelementPL(thetaDisplacementID);
    addparameterloopField(thetaDisplacementID, new Parameter(this, getParameterString(thetaDisplacementID, numb), 0,
        -10000.0 / (numb + 1) / (numb + 1) / (numb + 1), 10000.0 / (numb + 1) / (numb + 1) / (numb + 1)));
  }

  public void removeAllShiftParameters() {
    removeAllPLField(thetaDisplacementID);
  }

  public String getAxisXLegend() {
    if (!calibrated)
      return "Uncalibrated";
    int mode = PlotDataFile.checkScaleModeX();
    if (mode == 2)
      return "Q [Angstrom{^-1}]";
	 if (mode == 4)
		 return "Uncalibrated (original)";
	  if (mode == 5)
		  return "Channel";
	  if (dspacingbase || mode == 1)
		  return "d [Angstrom]";
	  if (energyDispersive || mode == 3)
		  return "Energy [eV]";
    return "2-Theta [degrees]";
  }

  public String getAxisXLegendNoUnit() {
    if (!calibrated)
      return "Uncalibrated";
    int mode = PlotDataFile.checkScaleModeX();
    if (mode == 2)
      return "Q";
	  if (mode == 4)
		  return "Uncalibrated";
	  if (mode == 5)
		  return "Channel";
	  if (dspacingbase || mode == 1)
		  return "d";
	  if (energyDispersive || mode == 3)
		  return "Energy";
    return "2-Theta";
  }

  public String getAxisXLegendUnit() {
    if (!calibrated)
      return "";
    int mode = PlotDataFile.checkScaleModeX();
    if (mode == 2)
      return "Angstrom^-1";
	  if (mode == 4)
		  return "original";
	  if (mode == 5)
		  return "number";
	  if (dspacingbase || mode == 1)
		  return "Angstrom";
	  if (energyDispersive || mode == 3)
		  return "eV";
    return "degrees";
  }

  public static String getAxisYLegend() {
    switch (PlotDataFile.getScaleMode()) {
      case 1:
        return "Intensity [Count]";
      case 2:
        return "Log10(Intensity) [Log10(Count)]";
      case 3:
        return "Intensity{^1/2} * Q";
      case 4:
        return "Intensity{^1/2} * Q^2";
      case 5:
        return "Intensity{^1/2} * Q^4";
	    case 6:
		    return "Intensity * Q";
	    case 7:
		    return "Intensity * Q^2";
	    case 8:
		    return "Intensity * Q^4";
	    case 9:
		    return "Log10(Intensity) * Q";
	    case 10:
		    return "Log10(Intensity) * Q^2";
	    case 11:
		    return "Log10(Intensity) * Q^4";
	    case 12:
		    return "Intensity{^1/2} / Q^1/2";
	    case 13:
		    return "Intensity{^1/2} / Q";
	    case 14:
      default: {
        return "Intensity{^1/2} [Count{^1/2}]";
      }
    }

  }

/*  public static String getAxisYLegend2D() {
    switch (PlotDataFile.getScaleMode()) {
      case 1:
        return "Intensity (Count)";
      case 2:
        return "Log10[Intensity] (Log10[Count])";
      case 3:
        return "Log10(Intensity) * Q";
      case 4:
        return "Log10(Intensity) * Q^2";
      case 5:
        return "Log10(Intensity) * Q^4";
      case 0:
      default: {
        return "Intensity{^1/2} (Count{^1/2})";
      }
    }

  }*/

  public double getXData(int index) {
    if (!calibrated) {
      calibrateX();
      if (!calibrated)
        return getXDataOriginal(index);
    }

    try {
      if (twothetacalibrated != null) {
//	      if (index == 0)
//	         System.out.println(index + " " + twothetacalibrated[index]);
	      return twothetacalibrated[index];
      } else {
        out.println(this.toXRDcatString() + ", data vector not initialize!!");
        return twotheta[index];
      }
    } catch (Exception e) {
      if (twothetacalibrated != null) {
        if (startingindex >= twothetacalibrated.length) {
          out.println("Warning, datafile " + toXRDcatString() +
              " is out of the imposed range for the dataset, will be disabled!");
          out.println("Check also the calibration, if present in the instrument.");
          setCompute(false);
          e.printStackTrace();
          return index;
        }
        out.println(this.getLabel() + ", warning, index " + index + ">= " + twothetacalibrated.length +
            "(maximum for the range)? If not check also the subsequent stack trace");
	      e.printStackTrace();
	      // recalibrating
	      calibrated = false;
	      index = 0;
        return twothetacalibrated[index];
      }
      e.printStackTrace();
      return twotheta[index];
    }
  }

  public double[] getXData() {
    if (!calibrated) {
      calibrateX();
      if (!calibrated)
        return getXDataOriginal();
    }
    return twothetacalibrated;
  }

  public double getXDataInvertCalibration(double x) {
    return notCalibrated(x);
  }

	public double getXDataOriginal(int index) {
		if (twotheta != null)
			return twotheta[index];
		else
			return twothetacalibrated[index];
	}

	public double getXDataForCalibration(int index) {
  	   if (index < 0)
  		   return index;
  	   if (index >=  datanumber)
  	   	return index;
	  double result;
    if (twotheta != null)
      result = twotheta[index];
    else
      result = twothetacalibrated[index];

		double finalResult = result;
		for (int i = 0; i < thetaDisplacementN; i++)
			finalResult += getParameterLoopValues(thetaDisplacementID, i) * Math.pow(result, i);

		return finalResult;
  }

  public double getXDataImage(int index) {
    if (x_image != null)
      return x_image[index];
    else
      return 0;
  }

  public double getYDataImage(int index) {
    if (y_image != null)
      return y_image[index];
    else
      return 0;
  }

	public double getTwothetaImage(int index) {
		if (twotheta_image != null)
			return twotheta_image[index];
		else
			return 0;
	}

	public double getEtaImage(int index) {
		if (eta_image != null)
			return eta_image[index];
		else
			return 0;
	}

	public double getDistanceImage(int index) {
		if (distance_image != null)
			return distance_image[index];
		else
			return 0;
	}

/*	public double[] getXDataImage() {
    return x_image;
  }

  public double[] getYDataImage() {
    return y_image;
  }*/

  public double[] getXDataOriginal() {
    if (twotheta != null)
      return twotheta;
    else
      return twothetacalibrated;
  }

  public double getXDataDspace(int index) {
    return getXDataDspace(getXData(index));
  }

  public double getXDataDspace(double x) {
    if (dspacingbase || !calibrated)
      return x;
    if (energyDispersive)
      return 12398.424121 / x;
    double wave = getMeanRadiationWavelength();
    if (wave == 0.0)
      return x;
    return wave / 2.0 / MoreMath.sind(x / 2.0);
  }

  public double getXDataForPlot(int index, int mode) {
    switch (mode) {
      case 1:
        return getXDataDspace(index);
      case 2:
        return getXInQ(getXData(index));
      case 3:
        return getXInEnergy(getXData(index));
	    case 4:
		    return getXDataOriginal(index);
	    case 5:
		    return index;
      default: {
        return getXData(index);
      }
    }
  }

  public double revertXDataForPlot(double value, int mode) {
    switch (mode) {
      case 1:
        return getXfromDspace(value);
      case 2:
        return getXfromQ(value);
      case 3:
        return getXDataInvertCalibration(value);
	    case 4:
		    return getXfromEnergy(value);
	    case 5:
		    return getXData((int) value);
      default: {
        return value;
      }
    }
  }

  public double getXDataForPlot(int index) {
    int mode = PlotDataFile.checkScaleModeX();
    return getXDataForPlot(index, mode);
  }

  public double convertXToDspace(double xdata) {
    if (dspacingbase || !calibrated)
      return xdata;
    if (energyDispersive)
      return 12398.424121 / xdata;
    double wave = getMeanRadiationWavelength();
    if (wave == 0.0)
      return xdata;
    return wave / 2.0 / MoreMath.sind(xdata / 2.0);
  }

  public double getXfromDspace(double value) {
    if (dspacingbase || !calibrated)
      return value;
    if (energyDispersive)
      return 12398.424121 / value;
    double wave = getMeanRadiationWavelength();
    if (wave == 0.0)
      return value;
    return 2.0 * MoreMath.asind(wave / (value * 2.0));
  }

  public double get2ThetaFromEnergy(double value) {
    double wave = getMeanRadiationWavelength();
    if (wave == 0.0)
      return value;
    return 2.0 * MoreMath.asind(wave * value / (12398.424121 * 2.0));
  }

  public double get2ThetaFromDSpace(double value) {
    double wave = getMeanRadiationWavelength();
    if (wave == 0.0)
      return value;
    return 2.0 * MoreMath.asind(wave / (value * 2.0));
  }

  public double getDSpaceFrom2Theta(double value) {
    double wave = getMeanRadiationWavelength();
    if (wave == 0.0)
      return value;
    return wave / 2.0 / MoreMath.sind(value / 2.0);
  }

  public double getEnergyFrom2Theta(double value) {
    double wave = getMeanRadiationWavelength();
    if (wave == 0.0)
      return value;
    return 12398.424121 * 2.0 / (wave / MoreMath.sind(value / 2.0));
  }

	public double getWavelengthFromDSpace(double value) {
		return 2.0 * value * MoreMath.sind(get2ThetaValue() / 2.0);
	}

	public double convertXToDspace(double xdata, double wave) {
    if (dspacingbase || !calibrated)
      return xdata;
    if (energyDispersive)
      return 12398.424121 / xdata;
    if (wave == 0.0)
      return xdata;
    return wave / 2.0 / MoreMath.sind(xdata / 2.0);
  }

  public double convertXDataForPlot(double xdata, int mode) {
    switch (mode) {
      case 1:
        return convertXToDspace(xdata);
      case 2:
        return getXInQ(xdata);
      default: {
      }
    }
    return xdata;
  }

  public double convertXDataForPlot(double xdata, double wave, int mode) {
    switch (mode) {
      case 1:
        return convertXToDspace(xdata, wave);
      case 2:
        return getXInQ(xdata, wave);
      default: {
      }
    }
    return xdata;
  }

  public void setXData(int index, double value) {
    twothetaOriginal[index] = value;
	  twotheta[index] = value;
	  calibrated = false;
  }

	public void setXuncalibrated() {
		for (int i = 0; i < numberOfData; i++) {
			twothetaOriginal[i] = i;
			twotheta[i] = i;
		}
		calibrated = false;
	}



	public void setXImage(int index, double value) {
    x_image[index] = value;
    calibrated = false;
  }

  public void setYImage(int index, double value) {
    y_image[index] = value;
    calibrated = false;
  }

	public void setTwothetaImage(int index, double value) {
		twotheta_image[index] = value;
	}

	public void setEtaImage(int index, double value) {
		eta_image[index] = value;
	}

	public void setDistanceImage(int index, double value) {
		distance_image[index] = value;
	}

	public void setCalibratedXData(int index, double value) {
    calibrated = true;
    twothetacalibrated[index] = value;
    twothetaOriginal[index] = value;
	  twotheta[index] = value;
  }

  public void setCalibratedXDataOnly(int index, double value) {
    calibrated = true;
    twothetacalibrated[index] = value;
  }

/*	public void update2thetaValues() {
		for (int index = startingindex; index < finalindex; index++)
			twotheta[index] = getDataFileSet().getCorrectedCoordinate(twothetaOriginal[index]);
		calibrateX();
	}*/

	public void calibrateX() {
//    updateStringtoDoubleBuffering();
    checkGroupCount();
    Calibration angcal = null;
    Instrument inst = getDataFileSet().getInstrument();
    if (inst != null)
      angcal = inst.getAngularCalibration();
    if (angcal != null) {
      angcal.calibrateX(this);
      calibrated = true;
    } else
      out.println("Warning: uncalibrated x coordinate");

    checkDataHoles();
  }

  public double notCalibrated(double x) {
    Calibration angcal = null;
    Instrument inst = getDataFileSet().getInstrument();
    if (inst != null)
      angcal = inst.getAngularCalibration();
    if (angcal != null) {
      return angcal.notCalibrated(this, x);
    } else
      return x;
  }

  public void setCalibrated(boolean value) {
    calibrated = value;
  }

  public void setIntensityUncalibrated() {
    intensityNotCalibrated = true;
    checkForRealCalibration = true;
  }

/*  public double getXordData(int index) {
    double x = getXData(index);
    if (!dspacingbase)
      x *= Constants.DEGTOPI / 2.0;

    return x;
  }*/

	public void checkDataHoles() {
		indicesDataHoles.clear();
		int datanumber = getTotalNumberOfData();
		if (datanumber < 5) return;
		double previousValue = getXData(0);
		double meanDeltaValue = 0;
		for (int i = 1; i < datanumber; i++) {
			double value = getXData(i);
			meanDeltaValue += Math.abs(value - previousValue);
			previousValue = value;
		}
		meanDeltaValue /= (datanumber - 1);

		previousValue = getXData(0);
		double deltaValue = meanDeltaValue; // Math.abs(previousValue - getXData(0));
		int firstIndex = -1;
		for (int i = 1; i < datanumber; i++) {
			double value = getXData(i);
			double actualDeltaValue = Math.abs(value - previousValue);
//			System.out.println(i + ", value: " + value + " - " + previousValue + " " + deltaValue + " * 3 < " + actualDeltaValue + " |||| " + meanDeltaValue);
			if (deltaValue / meanDeltaValue > 0.01 && deltaValue * 3.0 < actualDeltaValue) {
//				System.out.println("Starting: " + i);
				firstIndex = i - 1;
			} else {
				if (firstIndex > -1) {
					double[] newData = new double[2];
					newData[0] = getXData(firstIndex);
					newData[1] = getXData(i - 1);
					if (newData[1] < newData[0]) {
						// switch
						double switchValue = newData[0];
						newData[0] = newData[1];
						newData[1] = switchValue;
					}
//					System.out.println("Ending: " + i + ", range: " + newData[0] + ", " + newData[1]);
					indicesDataHoles.add(newData);
					firstIndex = -1;
				}
			}
			deltaValue = actualDeltaValue;
			previousValue = value;
		}
	}

	boolean isInsideHoles(double x) {
		for (int i = 0; i < indicesDataHoles.size(); i++) {
			double[] values = indicesDataHoles.elementAt(i);
			if (x > values[0] && x < values[1])
				return true;
		}
		return false;
	}

  public double getYData(int index) {
/*		if (getAsBackgroundPermission()) {
			return intensity[-1];
		}*/
    return intensity[index];
  }

  public double getTotalIntensity() {
    double total = 0.0;
    for (int i = startingindex; i < finalindex; i++)
      total += getYData(i);
//	  System.out.print("Total:" + total + " ");
	  if (useCountTimeToScale()) {
		  total /= getCountTimeValue();
//		  System.out.print(getCountTimeValue() + " ");
	  }
//	  System.out.println(total);
    return total;
  }

  public double getYDataForStatistic(int index, double qExp) {
	  return getIntensityForStatistic(index, getYData(index), qExp);

  }

  public void setYData(int index, double value) {

    intensity[index] = (double) value;

//    System.out.println(intensity[index]);
  }

  public double getFit(int index) {
    return finalIntensityCalibration(phasesfit[index] + bkgfit[index]) + intbkgfit[index] + expbkgfit[index];
  }

	public double getPhaseFitNoCalibration(int index) {
		return phasesfit[index];
	}

	public double getPhaseFit(int index, int phaseIndex) {
    if (phaseIndex > -1 && phaseIndex < phaseFit.size()) {
      double[] fit = (double[]) phaseFit.elementAt(phaseIndex);
      return finalIntensityCalibration(fit[index]);
    } else
      return 0;
  }

  public double getFitNoInterpolationBackground(int index) {
//	  System.out.println(phasesfit[index] + bkgfit[index]);
    return finalIntensityCalibration(phasesfit[index] + bkgfit[index]) + expbkgfit[index];
  }

//  Instrument theInstrument = null;

  public double finalIntensityCalibration(double value) {
    return getDataFileSet().getInstrument().getIntensityCalibration().calibrateData(value);
  }

	public double getIntensityForStatistic(int index, double intensity, double qExp) {
		if (reflectivityStats) {
			if (intensity > 0.0)
				return MoreMath.log10(intensity);
			else
				return 1.0E-79;
		}

		double corr = getCountTimeValueForStatistic() * getDatafileWeight();
		if (corr < 1.0E-9 && corr > 1.0E-9)
			corr = 1.0;
		intensity *= corr;

		if (Constants.testIntensityModForWeighting) {
			double qCorrection = Math.pow(Math.abs(getXInQ(getXData(index))), qExp);

			if (useCalibratedData && intensityCalibrated != null)
				intensity /= intensityCalibrated[index];

			if (intensity < 0)
				intensity = -intensity;
			intensity *= qCorrection;

			if (intensity > 1.0E-9) {

				switch (weightSwitch) {
					case 0: // default
					case 1: // sqrt
//						intensity = intensity; // * Math.sqrt(corr);
						break;
					case 3:
						intensity = MoreMath.log10(intensity); // * MoreMath.log10(corr);
						break;
				}
			}

		}

		return intensity;
	}

	public double getFitForStatistic(int index, double qExp) {
		return getIntensityForStatistic(index, getFit(index), qExp);

  }

  public double getBkgFit(int index) {
    return finalIntensityCalibration(bkgfit[index]) + intbkgfit[index] + expbkgfit[index];
  }

  public double getBkgFitNoInterpolation(int index) {
    return finalIntensityCalibration(bkgfit[index]) + expbkgfit[index];
  }

  public double getBkgFitForStatistic(int index) {
    if (reflectivityStats) {
      return MoreMath.log10(getBkgFit(index));
    }
	  double corr = getCountTimeValueForStatistic() * getDatafileWeight();
		  if (corr == 0)
			  corr = 1;

      return getBkgFit(index) * corr;
  }

	public double[] getWeight() {
		int dtanumber = computeDataNumber();
		double wgt[] = new double[dtanumber];

		double qExp = getFilePar().getQexpForWeightingScheme();

		for (int j = 0; j < dtanumber; j++)
			wgt[j] = getWeight(j + startingindex, qExp);

		return wgt;
	}

	public double getWeight(int index, double qExp) {

    // this routine must be optimized

    if (reflectivityStats)
      return 1.0f;

    Region aregion;
    DataFileSet dataset = getDataFileSet();
    int totregions = dataset.excludedRegionsNumber();

    double x = getXData(index);

    for (int i = 0; i < totregions; i++) {
      aregion = dataset.getExcludedRegion(i);
      double maxX = aregion.getMaximum();
      double minX = aregion.getMinimum();
      if (x < maxX && x > minX)
        return 0.0f;
    }

    double value = 0.0;

    if (getDataFileSet().getInstrument().getIntensityCalibration().validX(this, x, index)) {
      double corr = getCountTimeValueForStatistic() * getDatafileWeight();
      if (corr == 0)
        corr = 1;

      double yint;
      if (theoreticalWeights)
        yint = getFit(index);
      else
        yint = getYData(index);
//      System.out.println(yint);

	    double qCorrection = Math.pow(Math.abs(getXInQ(getXData(index))), qExp);
	    if (qCorrection < 1.0E-9)
	    	qCorrection = 1.0;

	    if (useNoBkg)
        yint -= getBkgFit(index);
//      if (weightSwitch > 6 && weightSwitch < 10)
//        yint -= getBkgFitNoInterpolation(index);

	    if (useCalibratedData && intensityCalibrated != null)
		    yint /= intensityCalibrated[index];

	    if (yint < 0)
        yint = -yint;
	    value = yint * corr;
	    value *= qCorrection;

	    if (yint > 1.0E-32) {

		    switch (weightSwitch) {
			    case 0: // default
				    value = weight[index]; // * Math.sqrt(corr);
				    break;
			    case 1: // sqrt
				    value = 1.0 / Math.sqrt(yint); // * Math.sqrt(corr);
				    break;
			    case 2:
				    value = 1.0 / yint; // * corr;
				    break;
			    case 3:
				    value = 1.0 / MoreMath.log10(yint); // * MoreMath.log10(corr);
				    break;
			    default: {
				    value = 1.0 / Math.sqrt(yint);
			    }
		    }
	    }
    }
    return value;
  }

  public double getDataWeightSum() {
    double sum = 0.0, wgt, dta;
	 double qExp = getFilePar().getQexpForWeightingScheme();
    for (int i = startingindex; i < finalindex; i++) {
      wgt = getWeight(i, qExp);
      dta = getYDataForStatistic(i, qExp);
      sum += dta * dta * wgt * wgt;
    }
    return sum;
  }

  public void setWeight(int index, double value) {
    weight[index] = value;
  }

  public double[] getData() {
    int dtanumber = computeDataNumber();
    double dta[] = new double[dtanumber];

	  double qExp = getFilePar().getQexpForWeightingScheme();
    for (int j = 0; j < dtanumber; j++)
      dta[j] = getYDataForStatistic(j + startingindex, qExp);

    return dta;
  }

  public double[] getDataForStatistic() {
    return getData();
  }

  public double[] getFit() {
    int dtanumber = computeDataNumber();
    double dta[] = new double[dtanumber];
    double qExp = getFilePar().getQexpForWeightingScheme();
    for (int j = 0; j < dtanumber; j++)
      dta[j] = getFitForStatistic(j + startingindex, qExp);

    return dta;
  }

  public double[] getFitForStatistic() {
    return getFit();
  }

  public void finalOutput(OutputStream out, boolean outputGraph) throws IOException {
    double[] indexes = getRefinementIndexes(outputGraph);
    printString(out, "Datafile " + toXRDcatString() + " : ");
    printString(out, "Rwp: " + Fmt.format(indexes[0]) + ", ");
    printString(out, "Rp: " + Fmt.format(indexes[4]) + ", ");
    printString(out, "Rwpnb: " + Fmt.format(indexes[1]) + ", ");
	  printString(out, "Rwpnb1: " + Fmt.format(indexes[2]) + ", ");
	  printString(out, "Rwpnb2: " + Fmt.format(indexes[3]) + ", ");
    printString(out, "Rpnb: " + Fmt.format(indexes[5]));
	  printString(out, "Rpnb1: " + Fmt.format(indexes[6]));
	  printString(out, "Rpnb2: " + Fmt.format(indexes[7]));
    newLine(out);

	  if (outputGraph) {
		  double[] dataS = getData();
		  double[] fitS = getFit();
		  double[] wgtS = getWeight();
		  printString(out, "Showing data, fit and weights used for refinement");
		  Frame tmpFrame = getFilePar().getMainFrame(); // new Frame();
		  (new PlotSimpleData(tmpFrame, dataS, "Channel", "data")).setVisible(true);
		  (new PlotSimpleData(tmpFrame, fitS, "Channel", "fit")).setVisible(true);
		  (new PlotSimpleData(tmpFrame, wgtS, "Channel", "wgt")).setVisible(true);
	  }

  }

/*	public double getWss(int index) {
		double wss = (getYDataForStatistic(index) - getFitForStatistic(index)) * getWeight(index);
		return wss * wss;
	}*/

  boolean indexesComputed = false;
  double[] refinementIndexes = new double[18];

  public double[] getRefinementIndexes(boolean outputGraph) {

    if (!indexesComputed) {
      double diff, wgt, dta, diff2, wgt2, dta2, dtanb, dtanb2;
      double rw = 0.0, r = 0.0, rwb1 = 0, rwb2 = 0, r1 = 0, r2 = 0, den = 0.0, rden = 0.0,
		      denb = 0.0, rdenb = 0.0;

      double qExp = getFilePar().getQexpForWeightingScheme();

      for (int i = startingindex; i < finalindex; i++) {
        wgt = getWeight(i, qExp);
        wgt2 = wgt * wgt;
        dta = getYDataForStatistic(i, qExp);
        dta2 = dta * dta;
        dtanb = Math.abs(dta - getBkgFitForStatistic(i));
        dtanb2 = dtanb * dtanb;
        diff = Math.abs(dta - getFitForStatistic(i, qExp));
	      double modb = 1;
        if (dta > 0)
	        modb = Math.abs(dtanb / dta);

	      diff2 = diff * diff;
	      double diffb1 = diff2 * modb;
	      double diffb2 = diffb1 * modb;

        rw += diff2 * wgt2;
        r += diff;
	      rwb1 += diffb1 * wgt2;
	      rwb2 += diffb2 * wgt2;
	      r1 += diff * Math.sqrt(modb);
	      r2 += diff * modb;
        den += dta2 * wgt2;
        rden += dta;
        denb += dtanb2 * wgt2;
        rdenb += dtanb;
      }

      refinementIndexes[0] = MoreMath.sqrt(rw / den);
      refinementIndexes[1] = MoreMath.sqrt(rw / denb);
	    refinementIndexes[2] = MoreMath.sqrt(rwb1 / denb);
	    refinementIndexes[3] = MoreMath.sqrt(rwb2 / denb);
      refinementIndexes[4] = r / rden;
      refinementIndexes[5] = r / rdenb;
	    refinementIndexes[6] = r1 / rdenb;
	    refinementIndexes[7] = r2 / rdenb;
      refinementIndexes[8] = rw;
      refinementIndexes[9] = r;
      refinementIndexes[10] = rwb1;
      refinementIndexes[11] = rwb2;
	    refinementIndexes[12] = r1;
	    refinementIndexes[13] = r2;
      refinementIndexes[14] = den;
      refinementIndexes[15] = denb;
      refinementIndexes[16] = rden;
      refinementIndexes[17] = rdenb;

//	    for (int i = 0; i < 12; i++)
//		    System.out.println(i + " " + refinementIndexes[i]);

      indexesComputed = true;
    }

    return refinementIndexes;

  }

  public boolean needReflectivityStatistic() {
    return getDataFileSet().needReflectivityStatistic();
  }

  public void setPhasesFit(int index, double value) {
    phasesfit[index] = value;
  }

  public void setPhasesFit(int index, double value, int phaseIndex) {
//    phasesfit[index] = (double) value;
    double[] fit = (double[]) phaseFit.elementAt(phaseIndex);
    fit[index] = value;
  }

  protected boolean setBkgFit(int index, double value) {
	  if (index >= 0 && index < bkgfit.length) {
		  bkgfit[index] = value;
		  return true;
	  }
	  return false;
  }

	protected boolean setExpBkgFit(int index, double value) {
		if (index >= 0 && index < expbkgfit.length) {
			expbkgfit[index] = value;
			return true;
		}
		return false;
	}

	public boolean hasfit() {
    return hasfit;
  }

  public void addtoPhasesFit(int index, double value) {
    phasesfit[index] += value;
  }

  public void addtoPhasesFit(int index, double value, int phaseIndex) {
    addtoPhasesFit(index, value);
    double[] fit = (double[]) phaseFit.elementAt(phaseIndex);
    fit[index] += value;
  }

  public void addtoFit(int index, double value) {
    addtoPhasesFit(index + startingindex, value);
  }

  public boolean addtoBkgFit(int index, double value) {
  	if (index >= 0 && index < bkgfit.length) {
	   bkgfit[index] += value;
	   return true;
   }
  	return false;
  }

	public boolean addtoIntBkgFit(int index, double value) {
		if (index >= 0 && index < intbkgfit.length) {
			intbkgfit[index] += value;
			return true;
		}
		return false;
	}

	public boolean addtoExpBkgFit(int index, double value) {
		if (index >= 0 && index < expbkgfit.length) {
			expbkgfit[index] += value;
			return true;
		}
		return false;
	}

	public double[] getXrangeInEnergy() {
    int dtanumber = computeDataNumber();
    double q[] = new double[dtanumber];

//    DataFileSet adataset = getDataFileSet();

//    double lambda = adataset.getRadiationWavelength();
//    Sample asample = adataset.getSample();

    for (int j = 0; j < dtanumber; j++)
      q[j] = getXInEnergy(getXData(j + startingindex));
    return q;
  }

  public double[] getXrangeInQ() {
    int dtanumber = computeDataNumber();
    double q[] = new double[dtanumber];

    DataFileSet adataset = getDataFileSet();

    double lambda = getMeanRadiationWavelength();
    Sample asample = adataset.getSample();

    for (int j = 0; j < dtanumber; j++) {
      q[j] = getXData(j + startingindex);
      if (energyDispersive)
        q[j] = 12398.424121 / q[j];
      if (!dspacingbase && !energyDispersive)
        q[j] = 4.0 * Math.PI * MoreMath.sind(getCorrectedPosition(asample,
            q[j]) / 2) / lambda;
      else
        q[j] = 2.0 * Math.PI / getCorrectedPosition(asample, q[j]);
    }
    return q;
  }

  public double getXInQ(double x) {
    double lambda = getMeanRadiationWavelength();
    return getXInQ(x, lambda);
  }

  public double getXInQ(double x, double lambda) {
    if (energyDispersive)
      x = 12398.424121 / x;
    if (!dspacingbase) {
      return 4.0 * Math.PI * MoreMath.sind(x / 2) / lambda;
    } else {
      return Constants.PI2 / x;
    }
  }

  public double getXfromQ(double x) {
    double lambda = getMeanRadiationWavelength();
    return getXfromQ(x, lambda);
  }

  public double getXfromQ(double x, double lambda) {
    if (energyDispersive)
      x = 12398.424121 / x;
    if (!dspacingbase) {
      return 2.0 * MoreMath.asind((x * lambda / (4.0 * Math.PI)));
    } else {
      return 2.0 * Math.PI / x;
    }
  }

  public double getXInEnergy(double x) {
    double lambda = getMeanRadiationWavelength();
    return getXInEnergy(x, lambda);
  }

  public double getXInEnergy(double x, double lambda) {
    if (energyDispersive)
      return x;
    if (!dspacingbase) {
      return 4.0 * Math.PI * MoreMath.sind(x / 2) / lambda;
    } else {
      return 2.0 * Math.PI / x;
    }
  }

  public double getXfromEnergy(double x) {
    double lambda = getMeanRadiationWavelength();
    return getXfromEnergy(x, lambda);
  }

  public double getXfromEnergy(double x, double lambda) {
    return x;
  }

  public double getValueScaled(double intensity, int index) {
	  int sign = 1;
    double x = getXData(index);
    if (PlotDataFile.calibrateIntensity()) {
      double calibratingIntensity = computeIntensityCalibration(index);
      if (calibratingIntensity != 0.0)
        intensity /= calibratingIntensity;
    }
	  if (PlotDataFile.calibrateIntensityForLorentzPolarization()) {
		  Instrument ainstrument = getDataFileSet().getInstrument();
		  Sample asample = getFilePar().getActiveSample();
		  double calibratingIntensity = ainstrument.LorentzPolarization(this, asample, x, dspacingbase, energyDispersive);
		  if (calibratingIntensity != 0.0)
			  intensity /= calibratingIntensity;
	  }
    switch (PlotDataFile.getScaleMode()) {
      case 1:
        return intensity;
      case 2:
        if (intensity <= 0.0)
          intensity = 0.0;
        else
          intensity = Math.log(intensity) * Constants.log10Conv;
        return intensity;
      case 3:
	      intensity *= getXInQ(x);
	      if (intensity < 0.0) {
		      sign = -1;
		      intensity = -intensity;
	      }
        return Math.sqrt(intensity) * sign;
      case 4:
	      intensity *= MoreMath.pow(getXInQ(x), 2);
	      if (intensity < 0.0) {
		      sign = -1;
		      intensity = -intensity;
	      }
	      return Math.sqrt(intensity) * sign;
      case 5:
	      intensity *= MoreMath.pow(getXInQ(x), 4);
	      if (intensity < 0.0) {
		      sign = -1;
		      intensity = -intensity;
	      }
	      return Math.sqrt(intensity) * sign;
	    case 6:
		    intensity *= getXInQ(x);
		    return intensity;
	    case 7:
		    intensity *= MoreMath.pow(getXInQ(x), 2);
		    return intensity;
	    case 8:
		    intensity *= MoreMath.pow(getXInQ(x), 4);
		    return intensity;
	    case 9:
		    intensity *= getXInQ(x);
		    if (intensity == 0.0)
			    return 0.0;
		    else if (intensity < 0.0) {
			    sign = -1;
			    intensity = -intensity;
		    }
		    return Math.log(intensity) * Constants.log10Conv * sign;
	    case 10:
		    intensity *= MoreMath.pow(getXInQ(x), 2);
		    if (intensity == 0.0)
			    return 0.0;
		    else if (intensity < 0.0) {
			    sign = -1;
			    intensity = -intensity;
		    }
		    return Math.log(intensity) * Constants.log10Conv * sign;
	    case 11:
		    intensity *= MoreMath.pow(getXInQ(x), 4);
		    if (intensity == 0.0)
			    return 0.0;
		    else if (intensity < 0.0) {
			    sign = -1;
			    intensity = -intensity;
		    }
		    return Math.log(intensity) * Constants.log10Conv * sign;
	    case 12:
			 double factor = Math.sqrt(Math.abs(getXInQ(x)));
			 if (factor > 0)
		      intensity /= factor;
		    return intensity;
	    case 13:
		    double factor1 = getXInQ(x);
		    if (factor1 > 0)
			    intensity /= factor1;
		    return intensity;
      case 0:
      default: {
        if (intensity < 0.0) {
          sign = -1;
          intensity = -intensity;
        }
        return Math.sqrt(intensity) * sign;
      }
    }

  }

  public double getInterpolatedYSqrtIntensity(double xvalue, double expT, double expT2, int mode) {
    xvalue = revertXDataForPlot(xvalue, mode);
    if (isInsideHoles(xvalue))
    	return Double.NaN;
    if (PlotDataFile.subtractBackground())
      return getValueScaled(getInterpolatedIntensity(xvalue, expT, expT2) -
          getInterpolatedBackground(xvalue, expT, expT2), getOldNearestPoint(xvalue));
    else
      return getValueScaled(getInterpolatedIntensity(xvalue, expT, expT2), getOldNearestPoint(xvalue));
  }

  public double getInterpolatedYSqrtIntensity(double xvalue, int exponent, int mode) {
    xvalue = revertXDataForPlot(xvalue, mode);
    if (isInsideHoles(xvalue))
    	return Double.NaN;
    if (PlotDataFile.subtractBackground())
      return getValueScaled(getInterpolatedIntensityAt(xvalue, exponent) -
          getInterpolatedBkgFitAt(xvalue, exponent), getOldNearestPoint(xvalue));
    else
      return getValueScaled(getInterpolatedIntensityAt(xvalue, exponent), getOldNearestPoint(xvalue));
  }

  public double getInterpolatedYForSummation(double xvalue) {
    xvalue = revertXDataForPlot(xvalue, 0);
    if (isInsideHoles(xvalue))
    	return Double.NaN;
    return getInterpolatedIntensityAt(xvalue, 1);
  }

	public double getInterpolatedFitForSummation(double xvalue) {
		xvalue = revertXDataForPlot(xvalue, 0);
		if (isInsideHoles(xvalue))
			return Double.NaN;
		return getInterpolatedFitAt(xvalue, 1);
	}

	public double getInterpolatedFitSqrtIntensity(double xvalue, int exponent, int mode) {
    xvalue = revertXDataForPlot(xvalue, mode);
    if (isInsideHoles(xvalue))
    	return Double.NaN;
    if (PlotDataFile.subtractBackground())
      return getValueScaled(getInterpolatedFitAt(xvalue, exponent) -
          getInterpolatedBkgFitAt(xvalue, exponent), getOldNearestPoint(xvalue));
    else
      return getValueScaled(getInterpolatedFitAt(xvalue, exponent), getOldNearestPoint(xvalue));
  }

  public double getInterpolatedFitSqrtIntensity(double xvalue, int exponent, int mode, int phase) {
    xvalue = revertXDataForPlot(xvalue, mode);
    if (isInsideHoles(xvalue))
    	return Double.NaN;
    if (PlotDataFile.subtractBackground())
      return getValueScaled(getInterpolatedFitAt(xvalue, exponent, phase), getOldNearestPoint(xvalue));
    else
      return getValueScaled(getInterpolatedFitAt(xvalue, exponent, phase) +
          getInterpolatedBkgFitAt(xvalue, exponent), getOldNearestPoint(xvalue));
  }

  public double getInterpolatedBkgFitSqrtIntensity(double xvalue, int exponent, int mode) {
    xvalue = revertXDataForPlot(xvalue, mode);
    if (isInsideHoles(xvalue))
    	return Double.NaN;
    return getValueScaled(getInterpolatedBkgFitAt(xvalue, exponent), getOldNearestPoint(xvalue));
  }

  public double getYSqrtData(int index) {
    if (PlotDataFile.subtractBackground())
      return getValueScaled(getYData(index) - getBkgFit(index), index);
    else
      return getValueScaled(getYData(index), index);
  }

  public double getFitSqrtData(int index) {
    if (PlotDataFile.subtractBackground())
      return getValueScaled(getFit(index) - getBkgFit(index), index);
    else
      return getValueScaled(getFit(index), index);
  }

  public double getFitSqrtData(int index, int phaseIndex) {
    if (PlotDataFile.subtractBackground())
      return getValueScaled(getPhaseFit(index, phaseIndex), index);
    else
      return getValueScaled(getPhaseFit(index, phaseIndex) + getBkgFit(index), index);
  }

  public double getBkgFitSqrtData(int index) {
    return getValueScaled(getBkgFit(index), index);
  }

  public String getCIFXcoord() {
    if (dspacingbase)
      return CIFXcoordD;
    else if (energyDispersive)
      return CIFXcoordEnergy;
    else
      return CIFXcoord2T;
  }

  public void fittingFileOutput(boolean addStatisticalError, int angleMax) {
    double xcoorddata;

    if (!(getComputePermission()) || !getPlotfilePermission())
      return;
    BufferedWriter output = getWriter(angleMax);
    try {
      output.write("_pd_block_id noTitle|#0");
      output.newLine();
      output.newLine();
      setGeneratePlotfile(false);
      writeAllFields(output);
      if (Integer.parseInt(getDataType()) == DIFFRACTION_IMAGE) {
        output.write("_diffrn_measurement_method diffraction_image");
        output.newLine();
      }
      setGeneratePlotfile(true);
      output.newLine();
      output.write("_pd_meas_number_of_points " + Integer.toString(finalindex - startingindex));
      output.newLine();
      output.write("_riet_meas_datafile_calibrated false");
      output.newLine();
      output.newLine();
      output.write("loop_");
      output.newLine();
      if (Integer.parseInt(getDataType()) == DIFFRACTION_IMAGE) {
        output.write("_pd_meas_position_x _pd_meas_position_y " + intensityCalcCIFstring);
      } else {
        output.write(getCIFXcoord());
        output.newLine();
        output.write(intensityCalcCIFstring);
      }
      output.newLine();
//      MoreMath.prepareGaussianDistribution(300);
      for (int i = startingindex; i < finalindex; i++) {
        double intens = getFit(i);
        if (addStatisticalError)
          intens += MoreMath.getGaussianNoise(intens); //getIntensityRandomError(intens);
        if (intens < 0)
          intens = 0.0;
          if (Integer.parseInt(getDataType()) == DIFFRACTION_IMAGE) {
            output.write(" " + Fmt.format(this.getXDataImage(i)) + " " + Fmt.format(this.getYDataImage(i))
                + " " + Fmt.format(intens));
          } else {
            xcoorddata = getXDataForCalibration(i);
            output.write(" " + Fmt.format(xcoorddata) + " " + Fmt.format(intens));
          }
        output.newLine();
      }
//      MoreMath.resetGaussianDistribution();
    } catch (IOException io) {
      io.printStackTrace();
    }
    try {
      output.flush();
      output.close();
    } catch (IOException io) {
      io.printStackTrace();
    }
  }

  public void fittingPhaseOutput(Sample asample, boolean addStatisticalError) {
    checkPhaseFitVector();
    double xcoorddata;
    if (getComputeString().equalsIgnoreCase("false") || !getPlotfilePermission())
      return;
    BufferedWriter output = getWriter(0);
    try {
      setGeneratePlotfile(false);
      writeAllFields(output);
      setGeneratePlotfile(true);
      output.newLine();
      output.write("_pd_meas_number_of_points " + Integer.toString(finalindex - startingindex));
      output.newLine();
        output.write("_riet_meas_datafile_calibrated false");
        output.newLine();
      output.newLine();
      output.write("loop_");
      output.newLine();
      output.write(getCIFXcoord());
      output.newLine();
      output.write(intensityCalcCIFstring);
      output.newLine();
      output.write(intensityCalcCIFstring + " # background");
      output.newLine();
      MoreMath.prepareGaussianDistribution(300);
      int phasesNumber = asample.getPhasesList().size();
//      Vector phaseFit = new Vector(phasesNumber, 1);
      for (int i = 0; i < phasesNumber; i++) {
        Phase phase = (Phase) asample.getPhasesList().elementAt(i);
//        double[] phasefit = computeSpectrum(asample, phase);
//        phaseFit.add(phasefit);
        output.write(intensityCalcCIFstring + " # " + phase.getLabel());
        output.newLine();
      }
      for (int i = startingindex; i < finalindex; i++) {
        xcoorddata = getXDataForCalibration(i);
        output.write(" " + Fmt.format(xcoorddata));
        output.write(" " + Fmt.format(getFit(i)));
        output.write(" " + Fmt.format(this.getBkgFit(i)));
        for (int j = 0; j < phasesNumber; j++) {
          double[] phasefit = (double[]) phaseFit.elementAt(j);
          output.write(" " + Fmt.format(phasefit[i]));
        }
        output.newLine();
      }
      MoreMath.resetGaussianDistribution();
    } catch (IOException io) {
      io.printStackTrace();
    }
    try {
      output.flush();
      output.close();
    } catch (IOException io) {
      io.printStackTrace();
    }
  }

  public void resetPhasesFit() {
    indexesComputed = false;
    refreshSpectraComputation = true;
    getParent().refreshComputation = true;
    int datanumb = getTotalNumberOfData();
    for (int i = 0; i < datanumb; i++) {
      setPhasesFit(i, 0.0);
    }
    checkPhaseFitVector();
    for (int j = 0; j < phaseFit.size(); j++) {
      for (int i = 0; i < datanumb; i++) {
        setPhasesFit(i, 0.0, j);
      }
    }
  }

  public void checkPhaseFitVector() {
    int diff = phaseFit.size() - getFilePar().getActiveSample().phasesNumber();
    if (diff > 0) {
      for (int i = 0; i < diff; i++)
        phaseFit.removeElementAt(0);
    } else if (diff < 0) {
      diff = -diff;
      for (int i = 0; i < diff; i++)
        phaseFit.addElement(new double[phasesfit.length]);
    }

  }

  public void resetBkg() {
    indexesComputed = false;
    refreshBkgComputation = true;
    refreshInterpolatedBkgComputation = true;
	  refreshExperimentalBkgComputation = true;
    getParent().refreshComputation = true;
    int datanumb = getTotalNumberOfData();
//	  System.out.println("Reset background for " + thelabel);
    for (int i = 0; i < datanumb; i++) {
      setBkgFit(i, 0.0);
		setExpBkgFit(i, 0.0);
    }
  }

	public void resetExperimentalBackground() {
		int datanumb = getTotalNumberOfData();
//		System.out.println("Reset background for " + thelabel);
		for (int i = 0; i < datanumb; i++) {
			setExpBkgFit(i, 0.0);
		}
	}

	public int getTotalNumberOfData() {
    return datanumber;
  }

  int numberOfData = 0;

  public int computeDataNumber() {
    numberOfData = finalindex - startingindex;

    return numberOfData;
  }

  public int getNumberOfData() {
    return numberOfData;
  }

  double startingX = 0.0;
  double finalX = 0.0;
  int deltaindex = 0;
	double i_deltaX = 0.0;

  public void setMinMaxIndices(int min, int max) {
    startingindex = min;
    finalindex = max;
//	System.out.println("Setting range " + min + " " + max);
    startingX = getXData(startingindex);
//	System.out.println("Final range " + max);
    finalX = getXData(finalindex - 1);
    deltaindex = finalindex - startingindex;
    if (deltaindex < 4) {
      setCompute(false);
      out.println("Datafile " + this.getLabel() +
          ": computation range too small or out of imposed limits, it's being disabled!");
    }
    i_deltaX = 1.0 / (finalX - startingX);
//    System.out.println("Final min max " + " " + startingX + " " + finalX);
  }

  public boolean increasingX() {
    return increasingX;
  }

  public void checkIncreasingX() {
    int datanumb = getTotalNumberOfData();
    if (getXData(0) < getXData(datanumb - 1)) {
      increasingX = true;
      lastcalibrationX = 10000000.0;
      lastX = 10000000.0;
    } else {
      increasingX = false;
      lastcalibrationX = -10000000.0;
      lastX = -10000000.0;
    }
  }

  public int getMinIndex() {
    return startingindex;
  }

  public int getMaxIndex() {
    return finalindex;
  }

  public int getMinIndexforValue(double min, double max) {
    int datanumb = getTotalNumberOfData();
    if (min == 0.0 && max == 0.0)
      return 0;

    int i = 0;
    int datan1 = datanumb - 1;
    if (increasingX()) {
      while (i < datan1 && getXData(i) < min)
	      i++;
    } else {
      while (i < datan1 && getXData(i) > max)
	      i++;
    }

    return i;
  }

  public int getMaxIndexforValue(double min, double max) {
    int datanumb = getTotalNumberOfData();
    if (min == 0.0 && max == 0.0)
      return datanumb;

	  int i = datanumb - 1;
	  if (increasingX()) {
		  while (i > 0 && getXData(i) > max)
			  i--;
	  } else {
		  while (i > 0 && getXData(i) < min)
			  i--;
	  }

	  return i;
  }

	public double getXRange() {
		return Math.abs(getXData(finalindex - 1) - getXData(startingindex));
	}

  public double[] getRange(int minindex, int maxindex) {
    return getdRange(getXData(minindex), getXData(maxindex - 1));
  }

  public double[] getdRange(double rangemin, double rangemax) {
    double range[] = new double[2];
    radiation = 0.0;
    if (!dspacingbase) {
      if (energyDispersive) {
        if (rangemax > 0) {
          rangemax = 12398.424121 / rangemax;
        } else {
          rangemax = 1.0E+10;
        }
        if (rangemin > 0) {
          rangemin = 12398.424121 / rangemin;
        } else {
          rangemin = 1.0E+10;
        }
        if (rangemin <= 0.0)
          rangemin = 1.0E-01;
        if (rangemax <= 0.0)
          rangemax = 1.0E-01;

      } else {
        radiation = getMeanRadiationWavelength();
        if (radiation == 0.0)
          out.println("Error, unable to get radiation wavelength!");
        else {
//	        System.out.println(rangemin + " - " + rangemax);
          if (rangemax > 0.0 && rangemax < 180.0)
            rangemax = radiation / (2 * Math.sin(rangemax / 2 * Constants.DEGTOPI));
          else if (rangemax <= 0.0)
            rangemax = 1.0E+10;
          else if (rangemax >= 180.0)
            rangemax = 1.0E-01;
          if (rangemin > 0.0 && rangemin < 180.0)
            rangemin = radiation / (2 * Math.sin(rangemin / 2 * Constants.DEGTOPI));
          else if (rangemin <= 0.0)
            rangemin = 1.0E+10;
          else if (rangemin >= 180.0)
            rangemin = 1.0E-01;
        }
      }
    }
    if (rangemax > rangemin) {
      range[0] = rangemin;
      range[1] = rangemax;
    } else {
      range[1] = rangemin;
      range[0] = rangemax;
    }
//	  System.out.println(rangemin + " + " + rangemax);

    return range;
  }

  public double getMeanRadiationWavelength() {
    DataFileSet parent = getDataFileSet();
    if (parent != null) {
//      System.out.println(parent.getRadiationWavelength());
      return parent.getInstrument().getRadiationType().getMeanRadiationWavelength();
    } else
      return 0.0;
  }

  public DataFileSet getDataFileSet() {
    return theDataFileSet;
  }

  public double[] getTiltingAngle() {
    double[] newAngles = new double[maxAngleNumber];
    arraycopy(corrected_tilting_angles, 0, newAngles, 0, newAngles.length);
    return newAngles;
  }

  double[][] getTextureAnglesAndIntensityNoBkg(boolean experimental) {
    double[][] texture_angles = new double[3][getNumberOfData()];
    double[] twothetaang = getXData();
    double[][] textAngle = getDataFileSet().getTextureAngles(this, getTiltingAngle(), twothetaang);
    for (int j = 0; j < getNumberOfData(); j++) {
      for (int i = 0; i < 2; i++)
        texture_angles[i][j] = textAngle[i][j];
      if (experimental)
        texture_angles[2][j] = getYData(j) - getBkgFit(j);
      else
        texture_angles[2][j] = this.getFit(j) - getBkgFit(j);
    }
    return texture_angles;
  }

  public double[] getTextureAngles(double twothetaang) {  // todo this do not differentiate for different positions
//    double[] ta = getTiltingAngle();
//	System.out.println(this+" "+twothetaang+" "+ta[0]+" "+ta[1]+" "+ta[2]+" " +ta[3]);
    return getDataFileSet().getTextureAngles(this, getTiltingAngle(), twothetaang);
  }

  public double[] getAlternateTextureAngles(double twothetaang) {
    return getDataFileSet().getAlternateTextureAngles(this, getTiltingAngle(), twothetaang);
  }

	public double[] getIncidentAndDiffractionAngles(double twothetaang) {
		DataFileSet adataset = getDataFileSet();
		Geometry ageometry = adataset.getInstrument().getGeometry();
		double[] angles = ageometry.getIncidentAndDiffractionAngles(this, getTiltingAngle(),
				adataset.getSample().getSampleAngles(), twothetaang);
		return angles;
	}

  public double[] getCoordinatesForDspacing(double d) {
    double[] position;
	  RadiationType radType = getDataFileSet().getInstrument().getRadiationType();
    if (dspacingbase || radType.getLinesCount() <= 0) {
      position = new double[1];
      position[0] = d;
    } else if (energyDispersive) {
      position = new double[1];
      position[0] = Constants.ENERGY_LAMBDA / d;  // todo may be more lines for each atom?
    } else {
      int radNumber = radType.getLinesCount();
      position = new double[radNumber];
      for (int i = 0; i < radNumber; i++) {
        position[i] = computeposition(d, radType.getRadiationWavelength(i));
      }
    }
    return position;
  }

	public static final double TWO_DEGTOPI = 2.0 / Constants.DEGTOPI;

	public static double computeposition(double dspace, double wavelength) {
		double ratioposition = wavelength / (2.0 * dspace);
		double position = 180.0;
		if (ratioposition < 1.0)
			position = TWO_DEGTOPI * Math.asin(ratioposition);
		return position;
	}

	public boolean xInsideRange(double position) {
    Region aregion;
    DataFileSet dataset = getDataFileSet();
    int totregions = dataset.excludedRegionsNumber();

//	  if (!getFilePar().isComputingDerivate())
//	    System.out.println("position " + getXData(startingindex) + " " + getXData(finalindex - 1));

	  if (increasingX()) {
      if (position <= getXData(finalindex - 1) && position >= getXData(startingindex)) {
        for (int i = 0; i < totregions; i++) {
          aregion = dataset.getExcludedRegion(i);
          double maxX = aregion.getMaximum();
          double minX = aregion.getMinimum();
          if (position < maxX && position > minX)
            return false;
        }
        return true;
      }
    } else {
      if (position <= getXData(startingindex) && position >= getXData(finalindex - 1)) {
        for (int i = 0; i < totregions; i++) {
          aregion = dataset.getExcludedRegion(i);
          double maxX = aregion.getMaximum();
          double minX = aregion.getMinimum();
          if (position < maxX && position > minX)
            return false;
        }
        return true;
      }
    }
    return false;
  }

  public double getCutoffAngle() {
    return getDataFileSet().getPeakCutoffD();
  }

  public boolean computeExpTextureFactor(Sample asample) {
    DataFileSet dataset = getDataFileSet();
    if (dataset.isTextureFactorsExtractionPermitted()) {
	    storeComputedOverExperimentalTextureFactors();
      IntensityExtractor intExt = dataset.getIntensityExtractor();
      intExt.setDataFile(this);
      intExt.extractIntensities(asample);
      return true;
    }
    return false;
  }

  public boolean computeExpStrain(Sample asample) {
    if (getFilePar().isPositionExtractionPermitted()) {
      DataFileSet dataset = getDataFileSet();
      PositionExtractor posExt = dataset.getPositionExtractor();
      posExt.setDataFile(this);
      posExt.extractPositions(asample);
      return true;
    }
    return false;
  }

  public double computeAngularIntensityCorrection(Sample asample, Instrument ainstrument, int j) {
    double x = getXData(j);
//    if (lorentzRestricted)
	  return getIntensityCalibration(j) * ainstrument.getBeamRelatedCorrection(this, asample, x, j) *
			  computeAbsorptionPath(x, ainstrument);
//    else
//      return getIntensityCalibration(j) * ainstrument.getBeamOutCorrection(this, asample, x)
//          * ainstrument.LorentzPolarization(this, asample, x, dspacingbase, energyDispersive);
  }

  public double computeAngularIntensityCorrection(Sample asample, Instrument ainstrument, double x) {
    return computeAngularIntensityCorrection(asample, ainstrument, getOldNearestPoint(x)) *
		    computeAbsorptionPath(x, ainstrument);
  }

/*  public double computeAbsorptionAndPhaseQuantity(Instrument ainstrument, Sample asample, Phase aphase, double x) {
//		if (lorentzRestricted) {
    return ainstrument.PhaseAndLayerAbsorption(this, asample, aphase, x, dspacingbase,
        energyDispersive, getDataFileSet().getDataFileSetIndex());
//		} else
//			return 1.0;
  }
*/
public double computeAbsorptionPath(double x, Instrument ainstrument) {
	if (absorptionVelocityFactor == 0)
		return 1.0;
	double toLambda = 0.0;
	if (ainstrument.getMeasurement().isTOF()) {
		toLambda = (2.0 * x * MoreMath.sind(Math.abs(ainstrument.getGeometry().getThetaDetector(this,
				x))));
	}
	double arg1 = absorptionVelocityFactor * toLambda;
	if (arg1 < 200.0)
		arg1 = Math.exp(-arg1);
	else
		arg1 = 0.0f;
	return arg1;
}

	boolean checkForRealCalibration = true;
  boolean needRealCalibration = true;

  public double getIntensityCalibration(int j) {
    if (checkForRealCalibration) {
      Instrument ainstrument = getDataFileSet().getInstrument();
      if (ainstrument.provideRealCalibration())
        needRealCalibration = true;
      else {
        needRealCalibration = false;
        intensityCalibrated = null;
      }
    }
	  double countTime = 1;
	  if (useCountTimeToScale())
		  countTime = getCountTimeValue();
	  countTime *= monitorCounts;
    if (!needRealCalibration)
      return countTime;
    return intensityCalibrated[j] * countTime;
  }

	public void calculateIntensityCalibration() {
		if (checkForRealCalibration) {
			Instrument ainstrument = getDataFileSet().getInstrument();
			if (ainstrument.provideRealCalibration())
				needRealCalibration = true;
			else {
				needRealCalibration = false;
				intensityCalibrated = null;
			}
		}
		if (needRealCalibration && intensityNotCalibrated)  // to check, for now is always true
			calibrateIntensity();
	}

  public void calibrateIntensity() {
//	  System.out.println("Calibrating intensity");
    int dtanumber = getTotalNumberOfData();
    if (intensityCalibrated == null || intensityCalibrated.length != dtanumber)
      intensityCalibrated = new double[dtanumber];

// check Luca getDataFileSet().getInstrument().getIntensityCalibration().updateStringtoDoubleBuffering(false);

    for (int j = 0; j < dtanumber; j++)
      intensityCalibrated[j] = computeIntensityCalibration(j);

	  intensityNotCalibrated = false;
  }

  public double computeIntensityCalibration(int j) {
    double xc = getXDataOriginal(j);
    Instrument ainstrument = getDataFileSet().getInstrument();
    return ainstrument.getIntensityCalibration().calibrateData(this, xc, j, getXData(j));
  }

  public int getBankNumber() {
    return bankNumber;
  }

  public int getAngBankNumber() {
    return angBankNumber;
  }

  public void computeReflectivityBroadening(Sample asample) {
    computeReflectivityBroadening(asample, phasesfit, startingindex, finalindex);
  }

	public void postComputation(Sample asample) {
    postComputation(asample, phasesfit, startingindex, finalindex);
/*    if (!getFilePar().isComputingDerivate()) {
      for (int i = 0; i < phaseFit.size(); i++)
        postComputation(asample, (double[])phaseFit.elementAt(i), startingindex, finalindex);
    }*/ // todo  check this will make the same computation more times
  }

  public void postComputation(Sample asample, double[] expfit) {
    postComputation(asample, expfit, startingindex, finalindex);
  }

  public void postComputation(Sample asample, double[] expfit, int min, int max) {
    postComputation();
  }

  public void postComputation() {
/*    if (getDataFileSet().isBackgroundExperimental()) {
      addExperimentalBackground();
    }*/
    if (getDataFileSet().isBackgroundInterpolated())
      addInterpolatedBackgroundFromResiduals();
    else
	    resetBackgroundInterpolated();
//    refreshBkgComputation = false;
//    refreshSpectraComputation = false;
  }

/*  public void computeasymmetryandbkg(Sample asample, double[] expfit, int min, int max) {
    computeasymmetry(asample, expfit, min, max);
    postComputation(asample, expfit, min, max);
    for (int j = min; j < max; j++)
      expfit[j] += getBkgFit(j);
  }

  public void computeasymmetryandAddbkg(Sample asample, double[] expfit, int min, int max) {
    computeasymmetry(asample, expfit, min, max);
    for (int j = min; j < max; j++)
      expfit[j] += getBkgFit(j);
  }

	public void computeasymmetry(Sample asample, double[] expfit) {
		computeasymmetry(asample, expfit, startingindex, finalindex - 1);
	}

	public void computeasymmetry(Sample asample, double afit[], int min, int max) {

		DataFileSet adataset = getDataFileSet();
		Instrument ainstrument = adataset.getInstrument();

		ainstrument.getInstrumentBroadening().computeAsymmetry(this, asample, afit, min, max);

		for (int j = min; j < max; j++) {
//      System.out.print("Before: " + afit[j]);
			afit[j] *= computeAngularIntensityCorrection(asample, ainstrument, j);
//      System.out.println(", after: " + afit[j]);
		}
	}
*/

	public void computeBackground(int starti, int finali) {
    int numbercoef = numberOfLoopParameters[getBackgroundID()];
    if (numbercoef != 0) {
      double backgcoef[] = getParameterLoopVector(getBackgroundID());
      for (int j = starti; j < finali; j++) {
        double thetaord = getXData(j);
        double background = 0.0;
		  if (useChebyshevPolynomials()) {
			  for (int k = 0; k < numbercoef; k++)
				  background += backgcoef[k] * ChebyshevPolynomial.getT(k, thetaord);
		  } else {
			  for (int k = 0; k < numbercoef; k++)
				  background += backgcoef[k] * Math.pow(thetaord, k);
		  }
        double oscillatorBack = 0.0;
        if (oscillatorsNumber > 0)
          oscillatorBack = 1.0;
        for (int i = 0; i < oscillatorsNumber; i++) {
          oscillatorBack *= getBkgOscillator(i).getIntensity(thetaord);
        }
        background += oscillatorBack;
        background *= getIntensityCalibration(j);
        addtoBkgFit(j, background);
      }
    } else if (oscillatorsNumber > 0) {
      for (int j = starti; j < finali; j++) {
        double thetaord = getXData(j);
        double oscillatorBack = 1.0;
        for (int i = 0; i < oscillatorsNumber; i++) {
          oscillatorBack *= getBkgOscillator(i).getIntensity(thetaord);
        }
        oscillatorBack *= getIntensityCalibration(j);
        addtoBkgFit(j, oscillatorBack);
      }
    }
    refreshBkgComputation = false;
    spectrumModified = true;
  }

  public void computeReflectivityBroadening(Sample asample, double afit[], int min, int max) {

    DataFileSet adataset = getDataFileSet();
    Instrument ainstrument = adataset.getInstrument();

    ainstrument.getInstrumentBroadening().computeReflectivityBroadening(this, asample, afit, min, max);
/*    for (int j = min; j < max; j++) {
      double x = getXData(j);
      afit[j - min] = (afit[j - min] * ainstrument.getBeamOutCorrection(this, asample, x));
    }*/
  }

  public void computeFluorescenceBroadening(Sample asample, double afit[], int min, int max) {

    DataFileSet adataset = getDataFileSet();
    Instrument ainstrument = adataset.getInstrument();

    ainstrument.getInstrumentBroadening().computeFluorescenceBroadening(this, asample, afit, min, max);
    for (int j = min; j < max; j++) {
      double x = getXData(j);
      afit[j - min] = (afit[j - min] * ainstrument.getBeamRelatedCorrection(this, asample, x, j));
    }
  }

  public boolean insiderange(int index) {
    return (index >= startingindex && index < finalindex);
  }

	public boolean isInsideRange(double x) {
		boolean incrX = increasingX();
//		System.out.println(incrX + " " + x + " " + startingX + " " + finalX);
		if ((incrX && (x >= startingX && x <= finalX)) ||
				(!incrX && (x <= startingX && x >= finalX))) {
			return true;
		}
		return false;
	}

  public void setStartingPointForTools(boolean setData) {
    if (!dspacingbase) {
      int dtanumber = computeDataNumber();
      for (int j = 0; j < dtanumber; j++) {
	      if (setData)
          setPhasesFit(j + startingindex, getYData(j + startingindex));
        setBkgFit(j + startingindex, 0.0);
	      setExpBkgFit(j + startingindex, 0.0);
      }
    }
  }

  public double getMaxIntensity() {
    double max = -10.0E50;
    for (int i = startingindex; i < finalindex; i++)
      if (max < getYData(i))
        max = getYData(i);
    return max;
  }

  public boolean backgroundSubtraction() {
    return backgroundSubtraction(false);
  }

	public boolean smoothing() {
		return smoothing(false);
	}

	public void resetBackgroundInterpolated() {
    if (refreshInterpolatedBkgComputation) {
      if (getFilePar().isBackgroundInterpolationPermitted()) {
        for (int i = 0; i < intbkgfit.length; i++) {
          intbkgfit[i] = 0;
        }
      }
      refreshInterpolatedBkgComputation = false;
      firstComputation = true;
    }
  }

	public void resetBackgroundExperimental() {
//		if (refreshExperimentalBkgComputation) {
			for (int i = 0; i < expbkgfit.length; i++) {
				expbkgfit[i] = 0;
			}
//			refreshExperimentalBkgComputation = false;
//		}
	}

	public void addInterpolatedBackgroundFromResiduals() {
//    System.out.println("Check interpolation bkg: " + refreshInterpolatedBkgComputation + " "+ getFilePar().isBackgroundInterpolationPermitted() + " "+ firstComputation);
    if (refreshInterpolatedBkgComputation) {
      if (getFilePar().isBackgroundInterpolationPermitted() || firstComputation) {
        firstComputation = false;
        int[] sampledPoints = getInterpolatedPointsX();
        double[] sampledIntensity = getInterpolatedPointsIntensity(sampledPoints);
        int index = 0;
        while (sampledPoints[index] < 0)
          index++;
        if (index < sampledPoints.length - 1) {
        int left = sampledPoints[index];
        int right = sampledPoints[index + 1];
        double leftInt = sampledIntensity[index];
        double rightInt = sampledIntensity[index + 1];
        for (int i = startingindex; i < finalindex; i++) {
          if (i > right) {
            if (index < sampledPoints.length - 1)
              index++;
            left = sampledPoints[index - 1];
            right = sampledPoints[index];
            leftInt = sampledIntensity[index - 1];
            rightInt = sampledIntensity[index];
          }
          if (left > -1 && right > -1) {
	          double bkgIntensity = MoreMath.getLinearInterpolation(getXData(i),
			          getXData(left), getXData(right),
			          leftInt, rightInt);
	          intbkgfit[i] = bkgIntensity;
          } else if (left <= 0)
	          intbkgfit[i] = leftInt;
          else
	          intbkgfit[i] = leftInt;
          }
        } else if (index < sampledPoints.length) {
          for (int i = startingindex; i < finalindex; i++) {
            intbkgfit[i] = sampledIntensity[index];
          }
        }
      }
      refreshInterpolatedBkgComputation = false;
    }
  }

  public int getNumberOfPointToSmooth() {
    int pointsToInterpolate = getDataFileSet().getInterpolatedPointsValue();
    int pointToSmooth = pointsToInterpolate / 20;
    int minimumNumberOfPointForSmooth =
        MaudPreferences.getInteger("interpolateBackground.minimumNumberOfPointToSmooth", 10);
    if (pointToSmooth < minimumNumberOfPointForSmooth) {
      pointToSmooth = minimumNumberOfPointForSmooth;
    }
    return pointToSmooth;
  }

/*  private int getNumberPointToInterpolate() {
    int dtanumber = computeDataNumber();
    int sampledPointsNumber = getDataFileSet().getInterpolatedPointsValue();
    return dtanumber / (sampledPointsNumber - 1);
  }*/

	public boolean addInterpolatedPointsX(double v) {
	  if (!isInsideRange(v))
		  return false;
    boolean incrX = true;
    if (manualBkgPoints[0] > manualBkgPoints[manualBkgPoints.length - 1])
      incrX = false;
    int sel = -1;
    if (incrX) {
      for (int index = 0; index < manualBkgPoints.length; index++) {
        if (v > manualBkgPoints[index])
          sel = index;
      }
    } else {
      for (int index = 0; index < manualBkgPoints.length; index++) {
        if (v < manualBkgPoints[index])
          sel = index;
      }
    }
    double[] newPoints = new double[manualBkgPoints.length + 1];
    for (int index = 0; index <= sel; index++) {
      newPoints[index] = manualBkgPoints[index];
    }
//    System.out.println("Adding point " + sel + 1 + "with coordinates " + v);
    newPoints[sel + 1] = v;
    for (int index = sel + 2; index < manualBkgPoints.length + 1; index++) {
      newPoints[index] = manualBkgPoints[index - 1];
    }
    manualBkgPoints = newPoints;
    refreshInterpolatedBkgComputation = false;
    firstComputation = true;
	  return true;
  }

  public void removeInterpolatedPointsX(double v, double max_dist) {
    double dist = 1.0E32;
    int sel = -1;
    for (int index = 0; index < manualBkgPoints.length; index++) {
      double dista = v - manualBkgPoints[index];
      dista *= dista;
      if (dista < dist) {
        dist = dista;
        sel = index;
      }
    }
	  if (dist < max_dist) {
//    System.out.println("removing point " + sel + "with coordinates " + v + " " + manualBkgPoints[sel]);
		  double[] newPoints = new double[manualBkgPoints.length - 1];
		  for (int index = 0; index < sel; index++) {
			  newPoints[index] = manualBkgPoints[index];
		  }
		  for (int index = sel + 1; index < manualBkgPoints.length; index++) {
			  newPoints[index - 1] = manualBkgPoints[index];
		  }
		  manualBkgPoints = newPoints;
		  refreshInterpolatedBkgComputation = false;
		  firstComputation = true;
	  }
  }

	public int[] getInterpolatedPointsX() {

    int[] sampledPoints;

    if (getManualBkgInterpolation() && (manualBkgPoints != null && manualBkgPoints.length > 0)) {
      sampledPoints = new int[manualBkgPoints.length];
      for (int index = 0; index < manualBkgPoints.length; index++) {
        sampledPoints[index] = getOldNearestPointNoOutside(manualBkgPoints[index]);
      }
//      System.out.println("Manual " + manualBkgPoints.length);
    } else {
      int pointsToInterpolate = getDataFileSet().getInterpolatedPointsValue();
	    int pointToSmooth = getNumberOfPointToSmooth();
	    if (pointToSmooth >= pointsToInterpolate)
		    pointsToInterpolate = pointToSmooth + 1;
	    int sampledPointsNumber = 0;
	    while (pointToSmooth * 3 > finalindex - startingindex)
		    pointToSmooth--;
	    int index = startingindex + pointToSmooth / 2;
	    while (index < finalindex - pointToSmooth / 2) {
		    index += pointsToInterpolate;
		    sampledPointsNumber++;
	    }
	    if (sampledPointsNumber <= 1)
		    sampledPointsNumber = 2;
//      out.println("Points " + pointsToInterpolate + " " + sampledPointsNumber + " " + pointToSmooth);

      sampledPoints = new int[sampledPointsNumber];
	    double[] sampledIntensity = new double[sampledPointsNumber];

	    sampledPoints[0] = startingindex + pointToSmooth / 2;
	    sampledIntensity[0] = getInterpolatedPureIntensity(sampledPoints[0]);
	    int base = startingindex + pointToSmooth / 2;
      for (index = 1; index < sampledPointsNumber - 1; index++) {
	      sampledPoints[index] = base + pointsToInterpolate;
        base = sampledPoints[index];
		    sampledIntensity[index - 1] = (getInterpolatedPureIntensity(sampledPoints[index - 1]) +
				    getInterpolatedPureIntensity(sampledPoints[index])) / 2.0;
      }
	    sampledPoints[sampledPointsNumber - 1] = finalindex - pointToSmooth / 2;
	    while (sampledPoints[sampledPointsNumber - 1] <= sampledPoints[sampledPointsNumber - 2])
		    sampledPoints[sampledPointsNumber - 1]++;
	    sampledIntensity[sampledPointsNumber - 2] = (getInterpolatedPureIntensity(sampledPoints[sampledPointsNumber - 2]) +
			    getInterpolatedPureIntensity(sampledPoints[sampledPointsNumber - 1])) / 2.0;
	    sampledIntensity[sampledPointsNumber - 1] = (getInterpolatedPureIntensity(sampledPoints[sampledPointsNumber - 2]) +
			    getInterpolatedPureIntensity(sampledPoints[sampledPointsNumber - 1])) / 2.0;

	    // now we find the best points in the background

	    int iterationNumber = getDataFileSet().getInterpolationIterationsValue();
	    //MaudPreferences.getInteger("backgroundSubtraction.iterations", 10);
	    double intensityMax = getMaxIntensity();
	    double curvature = MaudPreferences.getDouble("backgroundSubtraction.curvature%", 0.00001);
	    double c = intensityMax * curvature;

	    double mi;
	    int i1;
		  for (int iteration = 0; iteration < iterationNumber; iteration++) {
			  int increment = iteration;
//			  if (iteration > pointToSmooth)
//				  break;
		    for (int i = 0; i < sampledPointsNumber; i++) {
			    int old = sampledPoints[i];
			    if (i > sampledPointsNumber - 2) {
				    i1 = i - 1;
			    } else {
				    i1 = i + 1;
			    }
			    boolean decreasing = true;
			    while (decreasing && sampledPoints[i] - increment > startingindex + pointToSmooth / 3) {
				    mi = (getInterpolatedPureIntensity(sampledPoints[i1]) +
						    getInterpolatedPureIntensity(sampledPoints[i] - increment)) / 2.0;
				    if (mi + c < sampledIntensity[i]) {
					    sampledIntensity[i] = mi;
					    sampledPoints[i] -= increment;
				    } else
					    decreasing = false;
			    }
			    boolean increasing = true;
			    while (increasing && sampledPoints[i] + increment < finalindex - pointToSmooth / 3) {
				    mi = (getInterpolatedPureIntensity(sampledPoints[i1]) +
						    getInterpolatedPureIntensity(sampledPoints[i] + increment)) / 2.0;
				    if (mi + c < sampledIntensity[i]) {
					    sampledIntensity[i] = mi;
					    sampledPoints[i] += increment;
				    } else
					    increasing = false;
			    }
//			    System.out.println(iteration + " " + i + " " + (sampledPoints[i] - old));
		    }
	    }
	    boolean[] keepPoint = new boolean[sampledPointsNumber];
	    int totalRemoved = 0;
	    keepPoint[0] = true;
	    for (int i = 1; i < sampledPointsNumber; i++) {
		    if (sampledPoints[i] < sampledPoints[i-1] + 4) {
			    keepPoint[i] = false;
			    sampledPoints[i] = sampledPoints[i-1];
			    totalRemoved++;
		    } else
			    keepPoint[i] = true;
	    }

	    int newTotal = sampledPointsNumber - totalRemoved;
	    int[] newSampledPoints = new int[newTotal];
	    index = 0;
	    for (int i = 0; i < sampledPointsNumber; i++)
		    if (keepPoint[i])
			    newSampledPoints[index++] = sampledPoints[i];
	    sampledPoints = new int[newTotal];
	    for (int i = 0; i < newTotal; i++)
			  sampledPoints[i] = newSampledPoints[i];
			setManualBkgInterpolation(true);
//	    resetInterpolationWithRangeChange = true;
//	    if (getManualBkgInterpolation()) {
        manualBkgPoints = new double[newTotal];
        for (index = 0; index < newTotal; index++) {
          manualBkgPoints[index] = getXData(sampledPoints[index]);
        }
        refreshInterpolatedBkgComputation = false;
        firstComputation = true;
//        System.out.println("Manual 2 " + manualBkgPoints.length);
//      }
    }

    return sampledPoints;

  }

  public void checkManualInterpolation() {
    getInterpolatedPointsX();
  }

  public void removeManualInterpolation() {
    manualBkgPoints = null;
    setManualBkgInterpolation(false);
    refreshInterpolatedBkgComputation = false;
    firstComputation = true;
  }

  public double[] getInterpolatedPointsIntensity(int[] sampledPoints) {

    int pointToSmooth = getNumberOfPointToSmooth();

    double[] sampledIntensity = new double[sampledPoints.length];
    for (int index = 0; index < sampledPoints.length; index++) {
      int total = 0;
//	    System.out.println(index + " " + sampledPoints[index]);
      for (int j = sampledPoints[index] - pointToSmooth / 2;
           j < sampledPoints[index] + pointToSmooth / 2; j++) {
        if (j >= startingindex && j < finalindex) {
          sampledIntensity[index] += getYData(j) - getFitNoInterpolationBackground(j);
//	        System.out.println(index + " " + j + " " + getYData(j) + " " + getFitNoInterpolationBackground(j));
          total++;
        }
      }
      if (total > 0)
        sampledIntensity[index] /= total;
//	    System.out.println(index + " " + sampledIntensity[index] + " " + total);
    }

    return sampledIntensity;
  }

	public double getInterpolatedPureIntensity(int sampledPoints) {

		int pointToSmooth = getNumberOfPointToSmooth();

		double sampledIntensity = 0;
		int total = 0;
			for (int j = sampledPoints - pointToSmooth / 2;
			     j < sampledPoints + pointToSmooth / 2; j++) {
				if (j >= startingindex && j < finalindex) {
					sampledIntensity += getYData(j);
					total++;
				}
			}
			if (total > 0)
				sampledIntensity /= total;

		return sampledIntensity;
	}

	public void addExperimentalBackground() {
//		System.out.println("Computing experimental background for " + thelabel);
//		if (expbkgfit[(startingindex + finalindex) / 2] != 0)
//			throw new RuntimeException("Experimental background not reset!");
			if (refreshExperimentalBkgComputation) {
				resetBackgroundExperimental();
				DiffrDataFile expDataFile = getDataFileSet().getBackgroundDataFile(this);
//				expDataFile.startingindex = 0;
//				expDataFile.finalindex = expDataFile.datanumber;
				double countTime = expDataFile.monitorCounts;
				if (useCountTimeToScale())
					countTime *= expDataFile.getCountTimeValue();
				if (getDataFileSet().omogeneous() && getDataFileSet().omogeneousDataset) {
//					System.out.println("Computing omogeneous experimental background " + expDataFile.startingindex + " " + expDataFile.finalindex + " " + countTime);
					for (int i = startingindex; i < finalindex; i++) {
						double bkgIntensity = expDataFile.getInterpolatedIntensity(i) * countTime;
//		  double cal = getIntensityCalibration(i);
//	      System.out.println("Computing omogeneous experimental background " + i + " " + bkgIntensity + " " + cal);
//	      bkgIntensity *= cal;

						addtoExpBkgFit(i, bkgIntensity);
					}
				} else {
					expDataFile.initializeInterpolation();
					double bkgExpShift = getDataFileSet().getParameterValue(DataFileSet.bkgExpShift);
					double bkgExpThermalShift = getDataFileSet().getParameterValue(DataFileSet.bkgExpThermalShift);
//					System.out.println("Computing experimental background " + expDataFile.startingindex + " " + expDataFile.finalindex + " " + countTime + " " + bkgExpShift + " " + bkgExpThermalShift);
					for (int i = startingindex; i < finalindex; i++) {
						double bkgIntensity = expDataFile.getInterpolatedIntensity(getXData(i), bkgExpShift,
								bkgExpThermalShift) * countTime;
//	      double cal = getIntensityCalibration(i);
//	      System.out.println("Computing experimental background " + i + " " + bkgIntensity + " " + cal);
//	      bkgIntensity *= cal;
						addtoExpBkgFit(i, bkgIntensity);
					}
				}
			}
		refreshExperimentalBkgComputation = false;

  }

  public double getInterpolatedIntensity(int index) {
    if (index >= finalindex)
      return getPhaseFitNoCalibration(finalindex - 1);
    if (index < startingindex)
      return getPhaseFitNoCalibration(startingindex);
    return getPhaseFitNoCalibration(index);
  }

  double[] xdata = null;
  double[] ydata = null;
  int polinomialDegree = 2;
  int additionalPoints = 2;

  public void initializeInterpolation() {
    polinomialDegree = MaudPreferences.getInteger("backgroundSpectrum.smoothing_polinomial_degree", 2);
    additionalPoints = MaudPreferences.getInteger("backgroundSpectrum.smoothing_pol_additional_points", 4);
    xdata = new double[polinomialDegree + 1 + additionalPoints];
    ydata = new double[polinomialDegree + 1 + additionalPoints];
  }

  public double getInterpolatedIntensity(double x0, double bkgExpShift, double bkgExpThermalShift) {
    double x = (x0 + bkgExpShift) * (1.0 + bkgExpThermalShift * 1.0E-4);
    int numberOfPoints = polinomialDegree + 1 + additionalPoints;
    int index = getOldNearestPoint(x);
//	  return getYData(index) * monitorCounts;
    int startIndex = index;
    int stopIndex = index;
    boolean bounch = false;
    boolean forceStop = false;
    while (!forceStop && stopIndex - startIndex < numberOfPoints) {
      boolean done = false;
      if (bounch) {
        if (startIndex > 0) {
          startIndex--;
          done = true;
        }
      } else {
        if (stopIndex < datanumber) {
          stopIndex++;
          done = true;
        }
      }
      if (!done)
        forceStop = true;
      bounch = !bounch;
    }
    int j = 0;
    for (int i = startIndex; i < stopIndex; i++) {
      xdata[j] = getXData(i);
      ydata[j++] = getYData(i);
    }
    return MoreMath.getPolinomialValue(x, MoreMath.getPolinomialInterpolation(polinomialDegree, xdata, ydata)); // * monitorCounts;
  }

  public double getInterpolatedFit(double x0, double bkgExpShift, double bkgExpThermalShift) {
    double x = (x0 + bkgExpShift) * (1.0 + bkgExpThermalShift * 1.0E-4);
    int numberOfPoints = polinomialDegree + 1 + additionalPoints;
    int index = getOldNearestPoint(x);
    int startIndex = index;
    int stopIndex = index;
    boolean bounch = false;
    boolean forceStop = false;
    while (!forceStop && stopIndex - startIndex < numberOfPoints) {
      boolean done = false;
      if (bounch) {
        if (startIndex > 0) {
          startIndex--;
          done = true;
        }
      } else {
        if (stopIndex < datanumber) {
          stopIndex++;
          done = true;
        }
      }
      if (!done)
        forceStop = true;
      bounch = !bounch;
    }
    int j = 0;
    for (int i = startIndex; i < stopIndex; i++) {
      xdata[j] = getXData(i);
      ydata[j++] = getFit(i);
    }
    return MoreMath.getPolinomialValue(x, MoreMath.getPolinomialInterpolation(polinomialDegree, xdata, ydata)) * monitorCounts;
  }

  public double getInterpolatedBackground(double x0, double bkgExpShift, double bkgExpThermalShift) {
    double x = (x0 + bkgExpShift) * (1.0 + bkgExpThermalShift * 1.0E-4);
    int numberOfPoints = polinomialDegree + 1 + additionalPoints;
    int index = getOldNearestPoint(x);
    int startIndex = index;
    int stopIndex = index;
    boolean bounch = false;
    boolean forceStop = false;
    while (!forceStop && stopIndex - startIndex < numberOfPoints) {
      boolean done = false;
      if (bounch) {
        if (startIndex > 0) {
          startIndex--;
          done = true;
        }
      } else {
        if (stopIndex < datanumber) {
          stopIndex++;
          done = true;
        }
      }
      if (!done)
        forceStop = true;
      bounch = !bounch;
    }
    int j = 0;
    for (int i = startIndex; i < stopIndex; i++) {
      xdata[j] = getXData(i);
      ydata[j++] = getBkgFit(i);
    }
    return MoreMath.getPolinomialValue(x, MoreMath.getPolinomialInterpolation(polinomialDegree, xdata, ydata)) * monitorCounts;
  }

  public void computeSmoothSpectrum(double bkgExpShift, double bkgExpThermalShift,
                                    DiffrDataFile adatafile) {
//	  System.out.println("Spectrum " + toString() + " calculate smooth spectrum");
    initializeInterpolation();
    phasesfit = new double[adatafile.datanumber];
    startingindex = adatafile.startingindex;
    finalindex = adatafile.finalindex;
    for (int i = adatafile.startingindex; i < adatafile.finalindex; i++) {
      double x = adatafile.getXData(i);
      setPhasesFit(i, getInterpolatedIntensity(x, bkgExpShift, bkgExpThermalShift));
    }
  }

  public boolean backgroundSubtraction(boolean forFit) {

    int sampledPointsNumber = getDataFileSet().getInterpolatedPointsValue();
    int dtanumber = computeDataNumber();
    int pointsToInterpolate = dtanumber / sampledPointsNumber;

    int remaining = dtanumber - sampledPointsNumber * pointsToInterpolate;

    int[] sampledPoints = new int[pointsToInterpolate];
    int[] sampledPointsLeft = new int[pointsToInterpolate];
    int[] sampledPointsRight = new int[pointsToInterpolate];
    double[] sampledIntensity = new double[pointsToInterpolate];
    int index = 0;
    int i = startingindex + remaining / 2;
    for (; i < finalindex && index < pointsToInterpolate; i += sampledPointsNumber, index++) {
      sampledPoints[index] = i;
      sampledPointsLeft[index] = i;
      sampledPointsRight[index] = i;
      if (forFit)
        sampledIntensity[index] = getYData(i);
      else
        sampledIntensity[index] = getFit(i);
    }

    int iterationNumber = MaudPreferences.getInteger("backgroundSubtraction.iterations", 10);
    double intensityMax = getMaxIntensity();
    double curvature = MaudPreferences.getDouble("backgroundSubtraction.curvature%", 0.00001);
    double c = intensityMax * curvature;

    double mi;
    for (int iteration = 0; iteration < iterationNumber; iteration++) {
      for (i = 0; i < pointsToInterpolate; i++) {
        if (sampledPointsLeft[i] > startingindex)
          sampledPointsLeft[i]--;
        if (sampledPointsRight[i] < finalindex - 1)
          sampledPointsRight[i]++;
        if (forFit)
          mi = (getYData(sampledPointsLeft[i]) + getYData(sampledPointsRight[i])) / 2.0;
        else
          mi = (getFit(sampledPointsLeft[i]) + getFit(sampledPointsRight[i])) / 2.0;
        if (mi + c < sampledIntensity[i])
          sampledIntensity[i] = mi;
      }
    }

    index = 0;
    int left = sampledPoints[0];
    int right = sampledPoints[1];
    double leftInt = sampledIntensity[0];
    double rightInt = sampledIntensity[1];
    for (i = startingindex; i < finalindex; i++) {
      if (i > right) {
        if (index < pointsToInterpolate - 1)
          index++;
        left = sampledPoints[index - 1];
        right = sampledPoints[index];
        leftInt = sampledIntensity[index - 1];
        rightInt = sampledIntensity[index];
      }
      double bkgIntensity = MoreMath.getLinearInterpolation(getXData(i),
          getXData(left), getXData(right),
          leftInt, rightInt);
      if (forFit) {
        addtoBkgFit(i, bkgIntensity);
      } else {
        double newIntensity = getFit(i) - bkgIntensity;
        if (newIntensity < 0)
          newIntensity = 0.0;
        setPhasesFit(i, newIntensity);
      }
    }

    return true;
  }

	public boolean smoothing(boolean forFit) {

		SavitzkyGolay.initialM = MaudPreferences.getInteger("Savitzky-Golay.initialM",
				SavitzkyGolay.initialM + 1) - 1;

		double qExp = getFilePar().getQexpForWeightingScheme();
		int dtanumber = computeDataNumber();

		double dta[] = new double[dtanumber];
		double wgt[] = new double[dtanumber];
		int index[] = new int[dtanumber];
		for (int j =  0; j < dtanumber; j++) {
			dta[j] = getYData(j);
			wgt[j] = getWeight(j, qExp);
			index[j] = SavitzkyGolay.initialM;
		}
		double[] smooth = SavitzkyGolay.smoothPattern(dta, wgt, index, true);

		for (int i = 0; i < dtanumber; i++) {
			setPhasesFit(i, smooth[i]);
		}

		return true;
	}

	public boolean kalpha2Stripping() {
    int pointsToInterpolate = MaudPreferences.getInteger("lineStripping.interpolatedPoints", 2);
    if (!dspacingbase) {
	    RadiationType radType = getDataFileSet().getInstrument().getRadiationType();
//      double meanRadWavelength = getDataFileSet().getMeanRadiationWavelength();
      int radsNumber = radType.getLinesCount();
//      System.out.println(radsNumber);
      double[] radsWave = new double[radsNumber];
      double[] radsWeigth = new double[radsNumber];

      int dtanumber = computeDataNumber();
      for (int i = 0; i < radsNumber; i++) {
        radsWave[i] = radType.getRadiationWavelength(i);
        radsWeigth[i] = radType.getRadiationWeigth(i);
      }

      for (int i = 1; i < radsNumber; i++)
        radsWeigth[i] /= radsWeigth[0];
      for (int i = 1; i < radsNumber; i++)
        for (int j = i + 1; j < radsNumber; j++)
          radsWeigth[i] += radsWeigth[j];
      //     for (int i = 1; i < radsNumber; i++)
      //       System.out.println("w " + radsWeigth[i]);

      boolean incrX = increasingX();

      for (int i = 1; i < radsNumber; i++) {
        radsWave[i] /= radsWave[0];
        //       System.out.println(radsWave[i]);
        if ((radsWave[i] > 1 && incrX) || radsWave[i] <= 1 && !incrX) {
          for (int j = 0; j < dtanumber; j++) {
            int index = j + startingindex;
            double intensity1 = getFit(index);
            double twoTheta = getXData(index) / 2.0;

            double twoTheta1 = 180.0;
            double arg2theta = MoreMath.sind(twoTheta) / radsWave[i];
            if (arg2theta <= 0.0)
              twoTheta1 = 0.0;
            else if (arg2theta < 1.0)
              twoTheta1 = 2.0 * Math.asin(arg2theta) * Constants.PITODEG;
            double intensityAtDelta = getInterpolatedFitAt(twoTheta1, pointsToInterpolate);
            if (intensityAtDelta < 0.0)
              intensityAtDelta = 0.0;
            double newIntensity = intensity1 - intensityAtDelta * radsWeigth[i];
            if (newIntensity < 0)
              newIntensity = 0.0;
            setPhasesFit(index, newIntensity);
          }
        } else {
          for (int j = dtanumber - 1; j >= 0; j--) {
            int index = j + startingindex;
            double intensity1 = getFit(index);
            double twoTheta = getXData(index) / 2.0;
            double twoTheta1 = 180.0;
            double arg2theta = MoreMath.sind(twoTheta) / radsWave[i];
            if (arg2theta <= 0.0)
              twoTheta1 = 0.0;
            else if (arg2theta < 1.0)
              twoTheta1 = 2.0 * Math.asin(arg2theta) * Constants.PITODEG;
            double intensityAtDelta = getInterpolatedFitAt(twoTheta1, pointsToInterpolate);
            if (intensityAtDelta < 0.0)
              intensityAtDelta = 0.0;
            double newIntensity = intensity1 - intensityAtDelta * radsWeigth[i];
            if (newIntensity < 0)
              newIntensity = 0.0;
            setPhasesFit(index, newIntensity);
          }
        }
      }
      return true;
    }
    return false;
  }

  public double getIntensityAt(double x) {
    if (increasingX()) {
      if (x <= getXData(startingindex))
        return getYData(startingindex);
      if (x >= getXData(finalindex - 1))
        return getYData(finalindex - 1);
      int index = getOldNearestPoint(x);
      if ((x <= getXData(index) && index > startingindex) || index > finalindex - 2)
        return MoreMath.getLinearInterpolation(x, getXData(index - 1), getXData(index),
            getYData(index - 1), getYData(index));
      else
        return MoreMath.getLinearInterpolation(x, getXData(index), getXData(index + 1),
            getYData(index), getYData(index + 1));
    } else {
      if (x >= getXData(startingindex))
        return getYData(startingindex);
      if (x <= getXData(finalindex - 1))
        return getYData(finalindex - 1);
      int index = getOldNearestPoint(x);
      if ((x >= getXData(index) && index > startingindex) || index > finalindex - 2)
        return MoreMath.getLinearInterpolation(x, getXData(index - 1), getXData(index),
            getYData(index - 1), getYData(index));
      else
        return MoreMath.getLinearInterpolation(x, getXData(index), getXData(index + 1),
            getYData(index), getYData(index + 1));
    }
  }

  public double getInterpolatedIntensityAt(double x, int interpolatedPoints) {
    if (interpolatedPoints <= 2) {
      return getIntensityAt(x);
    }
    if (increasingX()) {
      if (x <= getXData(startingindex))
        return getYData(startingindex);
      if (x >= getXData(finalindex - 1))
        return getYData(finalindex - 1);
    } else {
      if (x >= getXData(startingindex))
        return getYData(startingindex);
      if (x <= getXData(finalindex - 1))
        return getYData(finalindex - 1);
    }

    int index = getOldNearestPoint(x);
    int left = index;
    int right = index;
    boolean leftTurn = true;
    while (Math.abs(left - right) < interpolatedPoints) {
      leftTurn = !leftTurn;
      if (leftTurn && left > startingindex)
        left--;
      if (!leftTurn && right < finalindex)
        right++;
    }

    double[] ydata1 = new double[interpolatedPoints];
    double[] xdata1 = new double[interpolatedPoints];
    for (int i = 0; i < interpolatedPoints; i++) {
      ydata1[i] = getYData(left);
      xdata1[i] = getXData(left++);
    }

    return MoreMath.getPolinomialValue(x, MoreMath.getPolinomialInterpolation(2, xdata1, ydata1));
  }

  public double getFitAt(double x) {
    if (increasingX()) {
      if (x <= getXData(startingindex))
        return getFit(startingindex);
      if (x >= getXData(finalindex - 1))
        return getFit(finalindex - 1);
      int index = getOldNearestPoint(x);
      if ((x <= getXData(index) && index > startingindex) || index > finalindex - 2)
        return MoreMath.getLinearInterpolation(x, getXData(index - 1), getXData(index),
            getFit(index - 1), getFit(index));
      else
        return MoreMath.getLinearInterpolation(x, getXData(index), getXData(index + 1),
            getFit(index), getFit(index + 1));
    } else {
      if (x >= getXData(startingindex))
        return getFit(startingindex);
      if (x <= getXData(finalindex - 1))
        return getFit(finalindex - 1);
      int index = getOldNearestPoint(x);
      if ((x >= getXData(index) && index > startingindex) || index > finalindex - 2)
        return MoreMath.getLinearInterpolation(x, getXData(index - 1), getXData(index),
            getFit(index - 1), getFit(index));
      else
        return MoreMath.getLinearInterpolation(x, getXData(index), getXData(index + 1),
            getFit(index), getFit(index + 1));
    }
  }

  public double getInterpolatedFitAt(double x, int interpolatedPoints) {
    if (interpolatedPoints <= 2) {
      return getFitAt(x);
    }
    if (increasingX()) {
      if (x <= getXData(startingindex))
        return getFit(startingindex);
      if (x >= getXData(finalindex - 1))
        return getFit(finalindex - 1);
    } else {
      if (x >= getXData(startingindex))
        return getFit(startingindex);
      if (x <= getXData(finalindex - 1))
        return getFit(finalindex - 1);
    }

    int index = getOldNearestPoint(x);
    int left = index;
    int right = index;
    boolean leftTurn = true;
    while (Math.abs(left - right) < interpolatedPoints) {
      leftTurn = !leftTurn;
      if (leftTurn && left > startingindex)
        left--;
      if (!leftTurn && right < finalindex)
        right++;
    }

    double[] ydata1 = new double[interpolatedPoints];
    double[] xdata1 = new double[interpolatedPoints];
    for (int i = 0; i < interpolatedPoints; i++) {
      ydata1[i] = getFit(left);
      xdata1[i] = getXData(left++);
    }

    return MoreMath.getPolinomialValue(x, MoreMath.getPolinomialInterpolation(2, xdata1, ydata1));
  }

  public double getBkgFitAt(double x) {
    if (increasingX()) {
      if (x <= getXData(startingindex))
        return getBkgFit(startingindex);
      if (x >= getXData(finalindex - 1))
        return getBkgFit(finalindex - 1);
      int index = getOldNearestPoint(x);
      if ((x <= getXData(index) && index > startingindex) || index > finalindex - 2)
        return MoreMath.getLinearInterpolation(x, getXData(index - 1), getXData(index),
            getBkgFit(index - 1), getBkgFit(index));
      else
        return MoreMath.getLinearInterpolation(x, getXData(index), getXData(index + 1),
            getBkgFit(index), getBkgFit(index + 1));
    } else {
      if (x >= getXData(startingindex))
        return getBkgFit(startingindex);
      if (x <= getXData(finalindex - 1))
        return getBkgFit(finalindex - 1);
      int index = getOldNearestPoint(x);
      if ((x >= getXData(index) && index > startingindex) || index > finalindex - 2)
        return MoreMath.getLinearInterpolation(x, getXData(index - 1), getXData(index),
            getBkgFit(index - 1), getBkgFit(index));
      else
        return MoreMath.getLinearInterpolation(x, getXData(index), getXData(index + 1),
            getBkgFit(index), getBkgFit(index + 1));
    }
  }

  public double getInterpolatedBkgFitAt(double x, int interpolatedPoints) {
    if (interpolatedPoints <= 2) {
      return getBkgFitAt(x);
    }
    if (increasingX()) {
      if (x <= getXData(startingindex))
        return getBkgFit(startingindex);
      if (x >= getXData(finalindex - 1))
        return getBkgFit(finalindex - 1);
    } else {
      if (x >= getXData(startingindex))
        return getBkgFit(startingindex);
      if (x <= getXData(finalindex - 1))
        return getBkgFit(finalindex - 1);
    }

    int index = getOldNearestPoint(x);
    int left = index;
    int right = index;
    boolean leftTurn = true;
    while (Math.abs(left - right) < interpolatedPoints) {
      leftTurn = !leftTurn;
      if (leftTurn && left > startingindex)
        left--;
      if (!leftTurn && right < finalindex)
        right++;
    }

    double[] ydata1 = new double[interpolatedPoints];
    double[] xdata1 = new double[interpolatedPoints];
    for (int i = 0; i < interpolatedPoints; i++) {
      ydata1[i] = getBkgFit(left);
      xdata1[i] = getXData(left++);
    }

    return MoreMath.getPolinomialValue(x, MoreMath.getPolinomialInterpolation(2, xdata1, ydata1));
  }

  public double getPhaseFitAt(double x, int phase) {
    if (increasingX()) {
      if (x <= getXData(startingindex))
        return getPhaseFit(startingindex, phase);
      if (x >= getXData(finalindex - 1))
        return getPhaseFit(finalindex - 1, phase);
      int index = getOldNearestPoint(x);
      if ((x <= getXData(index) && index > startingindex) || index > finalindex - 2)
        return MoreMath.getLinearInterpolation(x, getXData(index - 1), getXData(index),
            getPhaseFit(index - 1, phase), getPhaseFit(index, phase));
      else
        return MoreMath.getLinearInterpolation(x, getXData(index), getXData(index + 1),
            getPhaseFit(index, phase), getPhaseFit(index + 1, phase));
    } else {
      if (x >= getXData(startingindex))
        return getPhaseFit(startingindex, phase);
      if (x <= getXData(finalindex - 1))
        return getPhaseFit(finalindex - 1, phase);
      int index = getOldNearestPoint(x);
      if ((x >= getXData(index) && index > startingindex) || index > finalindex - 2)
        return MoreMath.getLinearInterpolation(x, getXData(index - 1), getXData(index),
            getPhaseFit(index - 1, phase), getPhaseFit(index, phase));
      else
        return MoreMath.getLinearInterpolation(x, getXData(index), getXData(index + 1),
            getPhaseFit(index, phase), getPhaseFit(index + 1, phase));
    }
  }

  public double getInterpolatedFitAt(double x, int interpolatedPoints, int phase) {
    if (interpolatedPoints <= 2) {
      return getPhaseFitAt(x, phase);
    }
    if (increasingX()) {
      if (x <= getXData(startingindex))
        return getPhaseFit(startingindex, phase);
      if (x >= getXData(finalindex - 1))
        return getPhaseFit(finalindex - 1, phase);
    } else {
      if (x >= getXData(startingindex))
        return getPhaseFit(startingindex, phase);
      if (x <= getXData(finalindex - 1))
        return getPhaseFit(finalindex - 1, phase);
    }

    int index = getOldNearestPoint(x);
    int left = index;
    int right = index;
    boolean leftTurn = true;
    while (Math.abs(left - right) < interpolatedPoints) {
      leftTurn = !leftTurn;
      if (leftTurn && left > startingindex)
        left--;
      if (!leftTurn && right < finalindex)
        right++;
    }

    double[] ydata1 = new double[interpolatedPoints];
    double[] xdata1 = new double[interpolatedPoints];
    for (int i = 0; i < interpolatedPoints; i++) {
      ydata1[i] = getPhaseFit(left, phase);
      xdata1[i] = getXData(left++);
    }

    return MoreMath.getPolinomialValue(x, MoreMath.getPolinomialInterpolation(2, xdata1, ydata1));
  }

  public double[][] peaksLocation(double[] derivative2) {
    int pointsToInterpolate = MaudPreferences.getInteger("peaksLocation.interpolatedPoints", 7);
    int dtanumber = computeDataNumber();
    for (int i = 0; i < dtanumber; i++) {
      derivative2[i] = getSecondDerivativeFitAt(getXData(i + startingindex), pointsToInterpolate);
    }
    double intensityMax = 1.0; //getMaxIntensity();
    double curvature = MaudPreferences.getDouble("peaksLocation.minimum2derivative%", 1);
    double c = intensityMax * curvature;
    double factor = MaudPreferences.getDouble("peaksLocation.noiseFactor", 10);

    boolean wasPositive = false;
    int imin = 0;
    Vector indicesPeakList = new Vector(0, 10);
    if (derivative2[0] >= 0)
      wasPositive = true;
    for (int i = 1; i < dtanumber; i++) {
      if (wasPositive && derivative2[i] < 0.0) {
        imin = i;
        wasPositive = false;
      }
      if (!wasPositive) {
        if (derivative2[i] < derivative2[imin])
          imin = i;
        if (derivative2[i] >= 0.0) {
          if (imin != 0 && Math.abs(derivative2[imin]) > Math.abs(c)) {
            if (checkHeightOverBackground(imin + startingindex, factor)) {
//      System.out.println(derivative2[imin]);
              indicesPeakList.addElement(imin);
            }
          }
          wasPositive = true;
        }
      }
    }

    int numberOfPeaks = indicesPeakList.size();
    double[][] peakList = new double[2][numberOfPeaks];
    for (int i = 0; i < numberOfPeaks; i++) {
      int index = (Integer) indicesPeakList.elementAt(i);
      peakList[0][i] = getMaximumByInterpolation(derivative2, index, 3);
      peakList[1][i] = getFit(index + startingindex);
//      System.out.println(i + " " + peakList[0][i] + " " + peakList[1][i]);
    }

    return peakList;
  }

  public boolean checkHeightOverBackground(int index, double factor) {
    double orIntensity = getYData(index);
    double error = Math.sqrt(orIntensity);
    error *= factor;
    return getFit(index) > error;
  }

  public double getSecondDerivativeFitAt(double x, int interpolatedPoints) {
    if (interpolatedPoints <= 2) {
      return 0.0;
    }
    if (increasingX()) {
      if (x <= getXData(startingindex))
        return getYData(startingindex);
      if (x >= getXData(finalindex - 1))
        return getYData(finalindex - 1);
    } else {
      if (x >= getXData(startingindex))
        return getYData(startingindex);
      if (x <= getXData(finalindex - 1))
        return getYData(finalindex - 1);
    }

    int index = getOldNearestPoint(x);
    int left = index;
    int right = index;
    boolean leftTurn = true;
    while (Math.abs(left - right) < interpolatedPoints) {
      leftTurn = !leftTurn;
      if (leftTurn && left > startingindex)
        left--;
      if (!leftTurn && right < finalindex)
        right++;
    }

    double[] ydata1 = new double[interpolatedPoints];
    double[] xdata1 = new double[interpolatedPoints];
    for (int i = 0; i < interpolatedPoints; i++) {
      ydata1[i] = getFit(left);
      xdata1[i] = getXData(left++);
    }

    double[] pars = MoreMath.getPolinomialInterpolation(2, xdata1, ydata1);
    return 2.0 * pars[2];
  }

   public double getMaximumByInterpolation(double[] thedata, int index,
                                          int interpolatedPoints) {

    int left = index;
    int right = index;
    boolean leftTurn = true;
    while (Math.abs(left - right) < interpolatedPoints) {
      leftTurn = !leftTurn;
      if (leftTurn && left > startingindex)
        left--;
      if (!leftTurn && right < finalindex)
        right++;
    }

    double[] ydata1 = new double[interpolatedPoints];
    double[] xdata1 = new double[interpolatedPoints];
    for (int i = 0; i < interpolatedPoints; i++) {
      ydata1[i] = thedata[left];
      xdata1[i] = getXData(startingindex + left++);
    }

    double[] pars = MoreMath.getPolinomialInterpolation(2, xdata1, ydata1);

    return -pars[1] / (2.0 * pars[2]);
  }

  int ilastIndex = 0;
  double lastX = 1.0e79;

/*	public int getNearestPoint(double x) {
		return getOldNearestPoint(x);

		double tmpX = 0.0;
		if (increasingX()) {
			while (ilastIndex > startingindex && (x = getXData(ilastIndex)) < lastX) {
				ilastIndex--;
			}
			while (ilastIndex < finalindex &&
							(tmpX = getXData(ilastIndex)) <= x)
				ilastIndex++;
			ilastIndex--;
			lastX = getXData(ilastIndex);
			if (Math.abs(lastX - x) <= Math.abs(tmpX - x))
				return ilastIndex;
			else {
				ilastIndex++;
				lastX = tmpX;
				return ilastIndex;
			}
		} else {
			while (ilastIndex > startingindex && (x = getXData(ilastIndex)) > lastX) {
				ilastIndex--;
			}
			while (ilastIndex < finalindex &&
							(tmpX = getXData(ilastIndex)) >= x)
				ilastIndex++;
			ilastIndex--;
			lastX = getXData(ilastIndex);
			if (Math.abs(lastX - x) <= Math.abs(tmpX - x))
				return ilastIndex;
			else {
				ilastIndex++;
				lastX = tmpX;
				return ilastIndex;
			}
		}
	} */

  public int getOriginalNearestPoint(double x) {
	  boolean incrX = increasingX();
	  if (Math.abs((getXDataOriginal(lastIndex) - x) * i_deltaX) < 0.1) {
		  if (incrX) {
			  while (lastIndex < finalindex - 1 && getXDataOriginal(lastIndex + 1) < x)
				  lastIndex++;
			  while (lastIndex > startingindex && getXDataOriginal(lastIndex - 1) > x)
				  lastIndex--;
			  if (getXDataOriginal(lastIndex) < x && lastIndex < finalindex - 1)
				  if (x - getXDataOriginal(lastIndex) > getXDataOriginal(lastIndex + 1) - x) {
					  return ++lastIndex;
				  }
			  if (getXDataOriginal(lastIndex) > x && lastIndex > startingindex)
				  if (x - getXDataOriginal(lastIndex - 1) < getXDataOriginal(lastIndex) - x)
					  return --lastIndex;
		  } else {
			  while (lastIndex < finalindex - 1 && getXDataOriginal(lastIndex + 1) > x)
				  lastIndex++;
			  while (lastIndex > startingindex && getXDataOriginal(lastIndex - 1) < x)
				  lastIndex--;
			  if (getXDataOriginal(lastIndex) > x && lastIndex < finalindex - 1)
				  if (x - getXDataOriginal(lastIndex + 1) < getXDataOriginal(lastIndex) - x)
					  return ++lastIndex;
			  if (getXDataOriginal(lastIndex) < x && lastIndex > startingindex)
				  if (x - getXDataOriginal(lastIndex) > getXDataOriginal(lastIndex - 1) - x)
					  return --lastIndex;
		  }
		  return lastIndex;
	  }
	  double startingOriginalX = getXDataOriginal(startingindex);
	  double finalOriginalX = getXDataOriginal(finalindex - 1);
	  lastIndex = startingindex + (int) (((x - startingOriginalX) * i_deltaX) * deltaindex);
	  if (incrX) {
		  if (lastIndex < startingindex && x > startingOriginalX)
			  lastIndex = startingindex + 1;
		  else if (lastIndex >= finalindex && x < finalOriginalX)
			  lastIndex = finalindex - 2;
	  } else {
		  if (lastIndex < startingindex && x < startingOriginalX)
			  lastIndex = startingindex + 1;
		  else if (lastIndex >= finalindex && x > finalOriginalX)
			  lastIndex = finalindex - 2;
	  }
	  if (lastIndex < startingindex)
		  return lastIndex = startingindex;
	  if (lastIndex >= finalindex)
		  return lastIndex = finalindex - 1;
	  if (incrX) {
		  while (lastIndex < finalindex - 1 && getXDataOriginal(lastIndex + 1) < x)
			  lastIndex++;
		  while (lastIndex > startingindex && getXDataOriginal(lastIndex - 1) > x)
			  lastIndex--;
		  if (getXDataOriginal(lastIndex) < x && lastIndex < finalindex - 1)
			  if (x - getXDataOriginal(lastIndex) > getXDataOriginal(lastIndex + 1) - x) {
				  return ++lastIndex;
			  }
		  if (getXDataOriginal(lastIndex) > x && lastIndex > startingindex)
			  if (x - getXDataOriginal(lastIndex - 1) < getXDataOriginal(lastIndex) - x)
				  return --lastIndex;
	  } else {
		  while (lastIndex < finalindex - 1 && getXDataOriginal(lastIndex + 1) > x)
			  lastIndex++;
		  while (lastIndex > startingindex && getXDataOriginal(lastIndex - 1) < x)
			  lastIndex--;
		  if (getXDataOriginal(lastIndex) > x && lastIndex < finalindex - 1)
			  if (x - getXDataOriginal(lastIndex + 1) < getXDataOriginal(lastIndex) - x)
				  return ++lastIndex;
		  if (getXDataOriginal(lastIndex) < x && lastIndex > startingindex)
			  if (x - getXDataOriginal(lastIndex) > getXDataOriginal(lastIndex - 1) - x)
				  return --lastIndex;
	  }
	  return lastIndex;
  }

  public int getOldNearestPoint(double x) {
	  boolean incrX = increasingX();
	  if (Math.abs((getXData(lastIndex) - x) * i_deltaX) < 0.1) {
		  if (incrX) {
			  while (lastIndex < finalindex - 1 && getXData(lastIndex + 1) < x)
				  lastIndex++;
			  while (lastIndex > startingindex && getXData(lastIndex - 1) > x)
				  lastIndex--;
			  if (getXData(lastIndex) < x && lastIndex < finalindex - 1)
				  if (x - getXData(lastIndex) > getXData(lastIndex + 1) - x) {
					  return ++lastIndex;
				  }
			  if (getXData(lastIndex) > x && lastIndex > startingindex)
				  if (x - getXData(lastIndex - 1) < getXData(lastIndex) - x)
					  return --lastIndex;
		  } else {
			  while (lastIndex < finalindex - 1 && getXData(lastIndex + 1) > x)
				  lastIndex++;
			  while (lastIndex > startingindex && getXData(lastIndex - 1) < x)
				  lastIndex--;
			  if (getXData(lastIndex) > x && lastIndex < finalindex - 1)
				  if (x - getXData(lastIndex + 1) < getXData(lastIndex) - x)
					  return ++lastIndex;
			  if (getXData(lastIndex) < x && lastIndex > startingindex)
				  if (x - getXData(lastIndex) > getXData(lastIndex - 1) - x)
					  return --lastIndex;
		  }
		  return lastIndex;
	  }
    lastIndex = startingindex + (int) (((x - startingX) * i_deltaX) * deltaindex);
    if (incrX) {
      if (lastIndex < startingindex && x > getXData(startingindex))
	      lastIndex = startingindex + 1;
      else if (lastIndex >= finalindex && x < getXData(finalindex - 1))
	      lastIndex = finalindex - 2;
    } else {
      if (lastIndex < startingindex && x < getXData(startingindex))
	      lastIndex = startingindex + 1;
      else if (lastIndex >= finalindex && x > getXData(finalindex - 1))
	      lastIndex = finalindex - 2;
    }
    if (lastIndex < startingindex)
      return lastIndex = startingindex;
    if (lastIndex >= finalindex)
      return lastIndex = finalindex - 1;
    if (incrX) {
      while (lastIndex < finalindex - 1 && getXData(lastIndex + 1) < x)
	      lastIndex++;
      while (lastIndex > startingindex && getXData(lastIndex - 1) > x)
	      lastIndex--;
      if (getXData(lastIndex) < x && lastIndex < finalindex - 1)
        if (x - getXData(lastIndex) > getXData(lastIndex + 1) - x) {
          return ++lastIndex;
        }
      if (getXData(lastIndex) > x && lastIndex > startingindex)
        if (x - getXData(lastIndex - 1) < getXData(lastIndex) - x)
          return --lastIndex;
    } else {
      while (lastIndex < finalindex - 1 && getXData(lastIndex + 1) > x)
	      lastIndex++;
      while (lastIndex > startingindex && getXData(lastIndex - 1) < x)
	      lastIndex--;
      if (getXData(lastIndex) > x && lastIndex < finalindex - 1)
        if (x - getXData(lastIndex + 1) < getXData(lastIndex) - x)
          return ++lastIndex;
      if (getXData(lastIndex) < x && lastIndex > startingindex)
        if (x - getXData(lastIndex) > getXData(lastIndex - 1) - x)
          return --lastIndex;
    }
    return lastIndex;
  }

  public int getOldNearestPointNoOutside(double x) {
    int index = startingindex + (int) (((x - startingX) * i_deltaX) * deltaindex);
    boolean incrX = increasingX();
    if (incrX) {
      if (index < startingindex && x > getXData(startingindex))
        index = startingindex + 1;
      else if (index >= finalindex && x < getXData(finalindex - 1))
        index = finalindex - 2;
    } else {
      if (index < startingindex && x < getXData(startingindex))
        index = startingindex + 1;
      else if (index >= finalindex && x > getXData(finalindex - 1))
        index = finalindex - 2;
    }
    if (index < startingindex || index >= finalindex)
      return -1;
    if (incrX) {
      while (index < finalindex - 1 && getXData(index + 1) < x)
        index++;
      while (index > startingindex && getXData(index - 1) > x)
        index--;
      if (getXData(index) < x && index < finalindex - 1)
        if (x - getXData(index) > getXData(index + 1) - x) {
          return ++index;
        }
      if (getXData(index) > x && index > startingindex)
        if (x - getXData(index - 1) < getXData(index) - x)
          return --index;
    } else {
      while (index < finalindex - 1 && getXData(index + 1) > x)
        index++;
      while (index > startingindex && getXData(index - 1) < x)
        index--;
      if (getXData(index) > x && index < finalindex - 1)
        if (x - getXData(index + 1) < getXData(index) - x)
          return ++index;
      if (getXData(index) < x && index > startingindex)
        if (x - getXData(index) > getXData(index - 1) - x)
          return --index;
    }
    return index;
  }

  int lastcalibrationIndex = -1;
  double lastcalibrationX = 10000000.0;

/*	public int getCalibrationNearestPoint(double x) {

		double tmpcalibrationX = 0.0;
		if (increasingX()) {
			if (x < lastcalibrationX)
				lastcalibrationIndex = startingindex;
			while (lastcalibrationIndex < finalindex &&
							(tmpcalibrationX = getXData(lastcalibrationIndex)) <= x)
				lastcalibrationIndex++;
			lastcalibrationIndex--;
			lastcalibrationX = getXData(lastcalibrationIndex);
			if (Math.abs(lastcalibrationX - x) <= Math.abs(tmpcalibrationX - x))
				return lastcalibrationIndex;
			else {
				lastcalibrationIndex++;
				lastcalibrationX = tmpcalibrationX;
				return lastcalibrationIndex;
			}
		} else {
			if (x > lastcalibrationX)
				lastcalibrationIndex = startingindex;
			while (lastcalibrationIndex < finalindex &&
							(tmpcalibrationX = getXData(lastcalibrationIndex)) >= x)
				lastcalibrationIndex++;
			lastcalibrationIndex--;
			lastcalibrationX = getXData(lastcalibrationIndex);
			if (Math.abs(lastcalibrationX - x) <= Math.abs(tmpcalibrationX - x))
				return lastcalibrationIndex;
			else {
				lastcalibrationIndex++;
				lastcalibrationX = tmpcalibrationX;
				return lastcalibrationIndex;
			}
		}
	}*/

  public BufferedReader getReader() {
    String filename = filterfilename(this.toXRDcatString());
//    System.out.println("Loading " + getFolder() + filename);
    return Misc.getReader(getFolder(), filename);
  }

  public BufferedInputStream getBufferedInputStream() {
    String filename = filterfilename(this.toXRDcatString());

    return new BufferedInputStream(Misc.getInputStream(getFolder(), filename));
  }

  public static String filterfilename(String filename) {
    if (filename.endsWith(")")) {
      int startIndex = -1;
      int i = filename.length() - 5;
      if (i < 0)
        i = 0;
      for (; i < filename.length() - 1; i++) {
        if (filename.substring(i, i + 1).equals("(")) {
          startIndex = i;
          break;
        }
      }
      if (startIndex != -1) {
        filename = filename.substring(0, startIndex);
      }
    }
    return filename;
  }

  public BufferedWriter getWriter(int maxAngle) {
    String filename = this.toXRDcatString();
    String tmpname = "";
    if (filename.endsWith(")")) {
      int startIndex = -1;
      int i = filename.length() - 5;
      if (i < 0)
        i = 0;
      for (; i < filename.length() - 1; i++) {
        if (filename.substring(i, i + 1).equals("(")) {
          startIndex = i;
          break;
        }
      }
      if (startIndex != -1) {
        tmpname = filename.substring(startIndex, filename.length());
        filename = filename.substring(0, startIndex);
      }
    }
    int finalIndex = -1;
    for (int i = filename.length(); i > 0; i--) {
      if (filename.substring(i - 1, i).equals(".")) {
        finalIndex = i - 1;
        break;
      }
    }
    if (finalIndex != -1)
      filename = filename.substring(0, finalIndex);

    if (maxAngle > 0) {
    	filename = filename + tmpname;
    	for (int i = 0; i < maxAngle; i++) {
    		double angle = getAngleValue(i);
    		String angleS = Float.toString((float) angle);
	      filename = filename + "_" + angleS;
      }
    	filename = filename + ".esg";
    } else
      filename = filename + tmpname + ".fit";

    System.out.println("Saving to: " + getFolder() + filename);
    return Misc.getWriter(getFolder(), filename);
  }

  public String getTitle() {
    return title;
  }

  public void checkOwnPolynomial() {
    if (numbercoefbackg() > 0)
      return;
    if (!calibrated) {
      calibrateX();
    }
    double range = Math.abs(getXData(getMaxIndex() - 1) - getXData(getMinIndex()));
//    System.out.println("Range: " + range);
    double step = 10;
    if (dspacingbase)
      step = 0.5;
    int numb = (int) (range / step);
    numb++;
    if (numb > 5)
      numb = 5;
    for (int i = 0; i < numb; i++)
      this.addBackgroundParameter();
  }

  public void freeThetaParameters() {
    for (int i = 0; i < getthetaoffsetnumber(); i++)
      getThetaDisplacement(i).setRefinableCheckBound();
  }

	public void  boundAllShiftsParameters(DiffrDataFile datafileToBoundTo) {
		for (int i = 0; i < getthetaoffsetnumber(); i++) {
			Parameter apar1 = datafileToBoundTo.getThetaDisplacement(i);
			if (apar1 != null)
				getThetaDisplacement(i).setEqualTo(apar1, 1.0, 0.0);
		}
	}

  public void refineAllXYSampleDisplacements() {
    getParameter(sampleDisplacementYID).setRefinableCheckBound();
    getParameter(sampleDisplacementZID).setRefinableCheckBound();
   }

  public boolean freeAllBasicParameters() {
    freeThetaParameters();
    return true;
  }

  public void freeAllBackgroundParameters() {
    int numbercoef = numbercoefbackg();
    for (int k = 0; k < numbercoef; k++)
      getbackgcoef(k).setRefinableCheckBound();
/*		int numberofpeak = backgpeaksnumber();
		for (int k = 0; k < numberofpeak; k++)
			getBkgPeak(k).freeAllParametersPreserveBound();*/

  }

  public void fixAllBackgroundParametersPreserveBound() {
    int numbercoef = numbercoefbackg();
    for (int k = 0; k < numbercoef; k++)
      getbackgcoef(k).setNotRefinableCheckBound();
/*		int numberofpeak = backgpeaksnumber();
		for (int k = 0; k < numberofpeak; k++)
			getBkgPeak(k).freeAllParametersPreserveBound();*/

  }

  public void freeAllCountMonitors() {
    getMonitorCounts().setRefinable();
  }

  public void fixAllCountMonitorsPreserveBound() {
    getMonitorCounts().setNotRefinableCheckBound();
  }

	public void refreshIndices(Phase phase) {
		int numberOfReflections = phase.gethklNumber();
		int radNumber = getDataFileSet().getInstrument().getRadiationType().getLinesCount();
//		System.out.println("Diffraction datafile: " + this.getLabel() + ", refresh indices: " + numberOfReflections);
		int[] reflectionsIDs = getReflectionIDs(phase);
		boolean reflectionsChanged = false;
		boolean updatedIndices = false;
		int[] indices = null;
		if (reflectionsIDs == null) {
			reflectionsIDs = new int[numberOfReflections];
			phaseReflectionIDs.put(phase, reflectionsIDs);
			initReflectionIDs(phase, reflectionsIDs);
		} else {
			reflectionsChanged = checkReflectionsList(phase, reflectionsIDs);
			if (reflectionsChanged) {
				int[] newReflectionsIDs = new int[numberOfReflections];
				initReflectionIDs(phase, newReflectionsIDs);
				indices = updateIndicesFromTo(reflectionsIDs, newReflectionsIDs);
				phaseReflectionIDs.remove(phase);
				phaseReflectionIDs.put(phase, newReflectionsIDs);
				updatedIndices = true;
			}
		}

		double[][][][] textureFactors = phaseTextureFactors.get(phase);
		if (textureFactors == null) {
			textureFactors = new double[2][numberOfReflections][positionsPerPattern][radNumber];
			phaseTextureFactors.put(phase, textureFactors);
			resetTextureFactors(phase);
		} else if (reflectionsChanged || textureFactors[0].length != numberOfReflections || textureFactors[0][0].length != positionsPerPattern || textureFactors[0][0][0].length != radNumber) {
			if (!updatedIndices) {
				int[] newReflectionsIDs = new int[numberOfReflections];
				initReflectionIDs(phase, newReflectionsIDs);
				indices = updateIndicesFromTo(reflectionsIDs, newReflectionsIDs);
				phaseReflectionIDs.remove(phase);
				phaseReflectionIDs.put(phase, newReflectionsIDs);
				updatedIndices = true;
			}
			double[][][][] newTextureFactors = new double[2][numberOfReflections][positionsPerPattern][radNumber];
			phaseTextureFactors.remove(phase);
			phaseTextureFactors.put(phase, newTextureFactors);
			resetTextureFactors(phase);
			updateTextureFactors(textureFactors, newTextureFactors, indices);
		}

		double[][][][] strainFactors = phaseStrainFactors.get(phase);
		if (strainFactors == null) {
			strainFactors = new double[2][numberOfReflections][positionsPerPattern][radNumber];
			phaseStrainFactors.put(phase, strainFactors);
		} else if (reflectionsChanged || strainFactors[0].length != numberOfReflections || strainFactors[0][0].length != positionsPerPattern || strainFactors[0][0][0].length != radNumber) {
			if (!updatedIndices) {
				int[] newReflectionsIDs = new int[numberOfReflections];
				initReflectionIDs(phase, newReflectionsIDs);
				indices = updateIndicesFromTo(reflectionsIDs, newReflectionsIDs);
				phaseReflectionIDs.remove(phase);
				phaseReflectionIDs.put(phase, newReflectionsIDs);
				updatedIndices = true;
			}
			double[][][][] newStrainFactors = new double[2][numberOfReflections][positionsPerPattern][radNumber];
			phaseStrainFactors.remove(phase);
			phaseStrainFactors.put(phase, newStrainFactors);
			resetStrainFactors(phase);
			updateStrainFactors(strainFactors, newStrainFactors, indices);
		}

		double[][][] positions = phasePositions.get(phase);
		if (positions == null) {
			positions = new double[numberOfReflections][positionsPerPattern][radNumber];
			phasePositions.put(phase, positions);
		} else if (reflectionsChanged || positions.length != numberOfReflections || positions[0].length != positionsPerPattern || positions[0][0].length != radNumber) {
			if (!updatedIndices) {
				int[] newReflectionsIDs = new int[numberOfReflections];
				initReflectionIDs(phase, newReflectionsIDs);
				indices = updateIndicesFromTo(reflectionsIDs, newReflectionsIDs);
				phaseReflectionIDs.remove(phase);
				phaseReflectionIDs.put(phase, newReflectionsIDs);
				updatedIndices = true;
			}
			double[][][] newPositions = new double[numberOfReflections][positionsPerPattern][radNumber];
			phasePositions.remove(phase);
			phasePositions.put(phase, newPositions);
			resetPositions(phase);
			updatePositions(positions, newPositions, indices);
		}

		int[][][][] minmaxIndices = phaseMinMaxIndices.get(phase);
		if (minmaxIndices == null) {
			minmaxIndices = new int[2][numberOfReflections][positionsPerPattern][radNumber];
			phaseMinMaxIndices.put(phase, minmaxIndices);
		} else if (reflectionsChanged || minmaxIndices[0].length != numberOfReflections || minmaxIndices[0][0].length != positionsPerPattern || minmaxIndices[0][0][0].length != radNumber) {
			if (!updatedIndices) {
				int[] newReflectionsIDs = new int[numberOfReflections];
				initReflectionIDs(phase, newReflectionsIDs);
				indices = updateIndicesFromTo(reflectionsIDs, newReflectionsIDs);
				phaseReflectionIDs.remove(phase);
				phaseReflectionIDs.put(phase, newReflectionsIDs);
				updatedIndices = true;
			}
			int[][][][] newMinmaxIndices = new int[2][numberOfReflections][positionsPerPattern][radNumber];
			phaseMinMaxIndices.remove(phase);
			phaseMinMaxIndices.put(phase, newMinmaxIndices);
			resetMinMaxIndices(phase);
			updateMinMaxIndices(minmaxIndices, newMinmaxIndices, indices);
		}

		double[][][] lorentzPolarizations = phaseLorentzPolarization.get(phase);
		if (lorentzPolarizations == null) {
			lorentzPolarizations = new double[numberOfReflections][positionsPerPattern][radNumber];
			phaseLorentzPolarization.put(phase, lorentzPolarizations);
		} else if (reflectionsChanged || positions.length != numberOfReflections || positions[0].length != positionsPerPattern || positions[0][0].length != radNumber) {
			if (!updatedIndices) {
				int[] newReflectionsIDs = new int[numberOfReflections];
				initReflectionIDs(phase, newReflectionsIDs);
				indices = updateIndicesFromTo(reflectionsIDs, newReflectionsIDs);
				phaseReflectionIDs.remove(phase);
				phaseReflectionIDs.put(phase, newReflectionsIDs);
				updatedIndices = true;
			}
			double[][][] newLorentzPolarizations = new double[numberOfReflections][positionsPerPattern][radNumber];
			phaseLorentzPolarization.remove(phase);
			phaseLorentzPolarization.put(phase, newLorentzPolarizations);
			resetLorentzPolarization(phase);
			updateLorentzPolarization(lorentzPolarizations, newLorentzPolarizations, indices);

		}

		double[][][] shapeAbsorption = phaseShapeAbsFactors.get(phase);
		if (shapeAbsorption == null) {
			shapeAbsorption = new double[numberOfReflections][positionsPerPattern][radNumber];
			phaseShapeAbsFactors.put(phase, shapeAbsorption);
		} else if (reflectionsChanged || shapeAbsorption.length != numberOfReflections || shapeAbsorption[0].length != positionsPerPattern || shapeAbsorption[0][0].length != radNumber) {
			if (!updatedIndices) {
				int[] newReflectionsIDs = new int[numberOfReflections];
				initReflectionIDs(phase, newReflectionsIDs);
				indices = updateIndicesFromTo(reflectionsIDs, newReflectionsIDs);
				phaseReflectionIDs.remove(phase);
				phaseReflectionIDs.put(phase, newReflectionsIDs);
				updatedIndices = true;
			}
			double[][][] newShapeAbsorption = new double[numberOfReflections][positionsPerPattern][radNumber];
			phaseShapeAbsFactors.remove(phase);
			phaseShapeAbsFactors.put(phase, newShapeAbsorption);
			resetShapeAbsFactors(phase);
			updateShapeAbsFactors(shapeAbsorption, newShapeAbsorption, indices);

		}

		double[][][][] crystallitesMicrostrains = phaseCrystallitesMicrostrains.get(phase);
		int numberCrystStrains = phase.getNumberOfSizeStrainCoefficients();
		if (crystallitesMicrostrains == null) {
			crystallitesMicrostrains = new double[numberCrystStrains][numberOfReflections][positionsPerPattern][radNumber];
			phaseCrystallitesMicrostrains.put(phase, crystallitesMicrostrains);
		} else if (reflectionsChanged || crystallitesMicrostrains[0].length != numberOfReflections || crystallitesMicrostrains[0][0].length != positionsPerPattern || crystallitesMicrostrains[0][0][0].length != radNumber) {
			if (!updatedIndices) {
				int[] newReflectionsIDs = new int[numberOfReflections];
				initReflectionIDs(phase, newReflectionsIDs);
				indices = updateIndicesFromTo(reflectionsIDs, newReflectionsIDs);
				phaseReflectionIDs.remove(phase);
				phaseReflectionIDs.put(phase, newReflectionsIDs);
				updatedIndices = true;
			}
			double[][][][] newCrystallitesMicrostrains = new double[numberCrystStrains][numberOfReflections][positionsPerPattern][radNumber];
			phaseCrystallitesMicrostrains.remove(phase);
			phaseCrystallitesMicrostrains.put(phase, newCrystallitesMicrostrains);
			resetCrystallitesMicrostrains(phase);
			updateCrystallitesMicrostrains(crystallitesMicrostrains, newCrystallitesMicrostrains, indices);
		}

		double[][][][] instrumentBroadening = phaseInstBroadFactors.get(phase);
		if (instrumentBroadening == null) {
			instrumentBroadening = new double[instrumentBroadeningParNumber][numberOfReflections][positionsPerPattern][radNumber];
			phaseInstBroadFactors.put(phase, instrumentBroadening);
		} else if (reflectionsChanged || instrumentBroadening[0].length != numberOfReflections || instrumentBroadening[0][0].length != positionsPerPattern || instrumentBroadening[0][0][0].length != radNumber) {
			if (!updatedIndices) {
				int[] newReflectionsIDs = new int[numberOfReflections];
				initReflectionIDs(phase, newReflectionsIDs);
				indices = updateIndicesFromTo(reflectionsIDs, newReflectionsIDs);
				phaseReflectionIDs.remove(phase);
				phaseReflectionIDs.put(phase, newReflectionsIDs);
				updatedIndices = true;
			}
			double[][][][] newInstrumentBroadening = new double[instrumentBroadeningParNumber][numberOfReflections][positionsPerPattern][radNumber];
			phaseInstBroadFactors.remove(phase);
			phaseInstBroadFactors.put(phase, newInstrumentBroadening);
			resetInstBroadFactors(phase);
			updateInstBroadFactors(instrumentBroadening, newInstrumentBroadening, indices);
		}

		double[][][][] phaseBroadening = phaseBroadFactors.get(phase);
		if (phaseBroadening == null) {
			phaseBroadening = new double[2][numberOfReflections][positionsPerPattern][radNumber];
			phaseBroadFactors.put(phase, phaseBroadening);
		} else if (reflectionsChanged || phaseBroadening[0].length != numberOfReflections || phaseBroadening[0][0].length != positionsPerPattern || phaseBroadening[0][0][0].length != radNumber) {
			if (!updatedIndices) {
				int[] newReflectionsIDs = new int[numberOfReflections];
				initReflectionIDs(phase, newReflectionsIDs);
				indices = updateIndicesFromTo(reflectionsIDs, newReflectionsIDs);
				phaseReflectionIDs.remove(phase);
				phaseReflectionIDs.put(phase, newReflectionsIDs);
				updatedIndices = true;
			}
			double[][][][] newPhaseBroadening = new double[2][numberOfReflections][positionsPerPattern][radNumber];
			phaseBroadFactors.remove(phase);
			phaseBroadFactors.put(phase, newPhaseBroadening);
			resetBroadFactors(phase);
			updateBroadFactors(phaseBroadening, newPhaseBroadening, indices);
		}

	}

	private void resetPositions(Phase phase) {
		double[][][] positions = getPositions(phase);
		if (positions != null) {
			for (int i = 0; i < positions.length; i++)
				for (int j = 0; j < positions[0].length; j++)
					for (int k = 0; k < positions[0][0].length; k++)
						positions[i][j][k] = 180.0;
		}
	}

	private void updatePositions(double[][][] positions, double[][][] newPositions, int[] indices) {
		if (indices != null) {
		for (int j = 0; j < indices.length; j++)
			if (indices[j] >= 0 && j < newPositions.length && indices[j] < positions.length)
				for (int i = 0; i < newPositions[0].length && i < positions[0].length; i++)
					for (int k = 0; k < newPositions[0][0].length && k < positions[0][0].length; k++)
						newPositions[j][i][k] = positions[indices[j]][i][k];
		}
	}

	private void resetMinMaxIndices(Phase phase) {
		int[][][][] minMaxIndices = getMinMaxIndices(phase);
		if (minMaxIndices != null) {
			for (int i = 0; i < minMaxIndices[0].length; i++)
				for (int j = 0; j < minMaxIndices[0][0].length; j++)
					for (int k = 0; k < minMaxIndices[0][0][0].length; k++) {
						minMaxIndices[0][i][j][k] = startingindex;
						minMaxIndices[1][i][j][k] = finalindex;
					}
		}
	}

	private void updateMinMaxIndices(int[][][][] minmaxIndices, int[][][][] newMinmaxIndices, int[] indices) {
		if (indices != null)
		for (int j = 0; j < indices.length; j++) {
			if (indices[j] >= 0 && j < newMinmaxIndices[0].length && indices[j] < minmaxIndices[0].length)
				for (int i = 0; i < newMinmaxIndices.length && i < minmaxIndices.length; i++)
					for (int l = 0; l < newMinmaxIndices[0][0].length && l < minmaxIndices[0][0].length; l++)
						for (int k = 0; k < newMinmaxIndices[0][0][0].length && k < minmaxIndices[0][0][0].length; k++)
							newMinmaxIndices[i][j][l][k] = minmaxIndices[i][indices[j]][l][k];
		}
	}

	private void resetLorentzPolarization(Phase phase) {
		double[][][] lorentzPolarization = getLorentzPolarization(phase);
		if (lorentzPolarization != null) {
			for (int i = 0; i < lorentzPolarization.length; i++)
				for (int j = 0; j < lorentzPolarization[i].length; j++)
					for (int k = 0; k < lorentzPolarization[i][j].length; k++)
						lorentzPolarization[i][j][k] = 1.0;
		}
	}

	private void updateLorentzPolarization(double[][][] lorentzPolarizations, double[][][] newLorentzPolarizations, int[] indices) {
		if (indices != null)
		for (int j = 0; j < indices.length; j++) {
			if (indices[j] >= 0 && j < newLorentzPolarizations.length && indices[j] < lorentzPolarizations.length)
				for (int i = 0; i < newLorentzPolarizations[0].length && i < lorentzPolarizations[0].length; i++)
					for (int k = 0; k < newLorentzPolarizations[0][0].length && k < lorentzPolarizations[0][0].length; k++)
						newLorentzPolarizations[j][i][k] = lorentzPolarizations[indices[j]][i][k];
		}
	}

	private void resetShapeAbsFactors(Phase phase) {
		double[][][] shapeAbsFactors = getShapeAbsFactors(phase);
		if (shapeAbsFactors != null) {
			for (int i = 0; i < shapeAbsFactors.length; i++)
				for (int j = 0; j < shapeAbsFactors[0].length; j++)
					for (int k = 0; k < shapeAbsFactors[0][0].length; k++)
						shapeAbsFactors[i][j][k] = 1.0;
		}
	}

	private void updateShapeAbsFactors(double[][][] shapeAbsorption, double[][][] newShapeAbsorption, int[] indices) {
		if (indices != null)
		for (int j = 0; j < indices.length; j++) {
			if (indices[j] >= 0 && j < newShapeAbsorption.length && indices[j] < shapeAbsorption.length)
				for (int i = 0; i < newShapeAbsorption[0].length && i < shapeAbsorption[0].length; i++)
					for (int k = 0; k < shapeAbsorption[0][0].length && k < newShapeAbsorption[0][0].length; k++)
						newShapeAbsorption[j][i][k] = shapeAbsorption[indices[j]][i][k];
		}
	}

	private void resetCrystallitesMicrostrains(Phase phase) {
		double[][][][] crystallitesMicrostrains = getCrystallitesMicrostrains(phase);
		if (crystallitesMicrostrains != null) {
			for (int i = 0; i < crystallitesMicrostrains.length; i++)
				for (int j = 0; j < crystallitesMicrostrains[0].length; j++)
					for (int k = 0; k < crystallitesMicrostrains[0][0].length; k++)
						for (int l = 0; l < crystallitesMicrostrains[0][0][0].length; l++)
						crystallitesMicrostrains[i][j][k][l] = 0.0;
		}
	}

	private void updateCrystallitesMicrostrains(double[][][][] crystallitesMicrostrains, double[][][][] newCrystallitesMicrostrains, int[] indices) {
		if (indices != null)
		for (int j = 0; j < indices.length; j++) {
			for (int i = 0; i < newCrystallitesMicrostrains.length && i < crystallitesMicrostrains.length; i++)
				if (indices[j] >= 0 && j < newCrystallitesMicrostrains[0].length && indices[j] < crystallitesMicrostrains[0].length)
					for (int k = 0; k < newCrystallitesMicrostrains[0][0].length && k < crystallitesMicrostrains[0][0].length; k++)
						for (int l = 0; l < newCrystallitesMicrostrains[0][0][0].length && l < crystallitesMicrostrains[0][0][0].length; l++)
							newCrystallitesMicrostrains[i][j][k][l] = crystallitesMicrostrains[i][indices[j]][k][l];
		}
	}

	private void resetInstBroadFactors(Phase phase) {
		double[][][][] instBroadFactors = getInstBroadFactors(phase);
		if (instBroadFactors != null) {
			for (int i = 0; i < instBroadFactors.length; i++)
				for (int j = 0; j < instBroadFactors[0].length; j++)
					for (int k = 0; k < instBroadFactors[0][0].length; k++)
						for (int l = 0; l < instBroadFactors[0][0][0].length; l++)
							instBroadFactors[i][j][k][l] = 0.0;
		}
	}

	private void updateInstBroadFactors(double[][][][] instrumentBroadening, double[][][][] newInstrumentBroadening, int[] indices) {
		if (indices != null)
		for (int j = 0; j < indices.length; j++) {
			if (indices[j] >= 0 && j < newInstrumentBroadening[0].length && indices[j] < instrumentBroadening[0].length)
				for (int i = 0; i < newInstrumentBroadening.length && i < instrumentBroadening.length; i++)
					for (int k = 0; k < newInstrumentBroadening[0][0].length && k < instrumentBroadening[0][0].length; k++)
						for (int l = 0; l < newInstrumentBroadening[0][0][0].length && l < instrumentBroadening[0][0][0].length; l++)
							newInstrumentBroadening[i][j][k][l] = instrumentBroadening[i][indices[j]][k][l];
		}
	}

	private void resetBroadFactors(Phase phase) {
		double[][][][] broadFactors = getBroadFactors(phase);
		if (broadFactors != null) {
			for (int i = 0; i < broadFactors.length; i++)
				for (int j = 0; j < broadFactors[0].length; j++)
					for (int k = 0; k < broadFactors[0][0].length; k++)
						for (int l = 0; l < broadFactors[0][0][0].length; l++)
							broadFactors[i][j][k][l] = 0.0;
		}
	}

	private void updateBroadFactors(double[][][][] phaseBroadening, double[][][][] newPhaseBroadening, int[] indices) {
		if (indices != null)
		for (int j = 0; j < indices.length; j++) {
			if (indices[j] >= 0 && j < newPhaseBroadening[0].length && indices[j] < phaseBroadening[0].length)
				for (int i = 0; i < phaseBroadening.length &&  i < newPhaseBroadening.length; i++)
					for (int k = 0; k < phaseBroadening[0][0].length && k < newPhaseBroadening[0][0].length; k++)
						for (int l = 0; l < phaseBroadening[0][0][0].length && l < newPhaseBroadening[0][0][0].length; l++)
							newPhaseBroadening[i][j][k][l] = phaseBroadening[i][indices[j]][k][l];
		}
	}

	private void resetStrainFactors(Phase phase) {
		double[][][][] strainsFactors = getStrainFactors(phase);
		if (strainsFactors != null) {
			for (int i = 0; i < strainsFactors.length; i++)
				for (int j = 0; j < strainsFactors[0].length; j++)
					for (int k = 0; k < strainsFactors[0][0].length; k++)
						for (int l = 0; l < strainsFactors[0][0][0].length; l++)
							strainsFactors[i][j][k][l] = 0.0;
		}
	}

	private void updateStrainFactors(double[][][][] strainFactors, double[][][][] newStrainFactors, int[] indices) {
		if (indices != null)
		for (int j = 0; j < indices.length; j++) {
			if (indices[j] >= 0 && j < newStrainFactors[0].length && indices[j] < strainFactors[0].length)
				for (int i = 0; i < strainFactors.length && i < newStrainFactors.length; i++)
					for (int k = 0; k < strainFactors[0][0].length && k < newStrainFactors[0][0].length; k++)
						for (int l = 0; l < strainFactors[0][0][0].length && l < newStrainFactors[0][0][0].length; l++)
					newStrainFactors[i][j][k][l] = strainFactors[i][indices[j]][k][l];
		}
	}

	private void initReflectionIDs(Phase phase, int[] reflectionsIDs) {
		int numberOfReflections = phase.gethklNumber();
		for (int i = 0; i < numberOfReflections && i < reflectionsIDs.length; i++) {
			Reflection refl = phase.getReflex(i);
			int reflID = Reflection.getUniqueIdFor(refl.getH(), refl.getK(), refl.getL());
			reflectionsIDs[i] = reflID;
		}
	}

	private boolean checkReflectionsList(Phase phase, int[] reflectionsIDs) {
		boolean changed = false;
		int numberOfReflections = phase.gethklNumber();
		if (numberOfReflections != reflectionsIDs.length)
			return true;
		for (int i = 0; i < numberOfReflections && !changed; i++) {
			Reflection refl = phase.getReflex(i);
			int reflID = Reflection.getUniqueIdFor(refl.getH(), refl.getK(), refl.getL());
			if (reflectionsIDs[i] != reflID)
				changed = true;
		}
		return changed;
	}

	private int[] updateIndicesFromTo(int[] oldID, int[] newID) {
		int numberOfReflections = newID.length;
		int[] indices = new int[numberOfReflections];
		for (int i = 0; i < numberOfReflections; i++) {
			int reflID = newID[i];
			boolean stop = false;
			int incr = 0;
			int index = i;
			if (index < oldID.length && reflID == oldID[index])
				stop = true;
			while (!stop) {
				index = i + incr;
				boolean outOfRange = true;
				if (index < oldID.length) {
					outOfRange = false;
					if (reflID == oldID[index])
						stop = true;
				}
				index = i - incr;
				if (index >= 0 && index < oldID.length) {
					outOfRange = false;
					if (reflID == oldID[index])
						stop = true;
				}
				if (outOfRange && incr > 10) {
					stop = true;
					index = -1;
				}
				incr++;
			}
			indices[i] = index;
		}
		return indices;
	}

	static boolean checkHKL = true;


	private void resetTextureFactors(Phase phase) {
//		System.out.println("Reset texture factors for phase: " + phase.getPhaseName());
		double[][][][] textureFactors = getTextureFactors(phase);
		if (textureFactors != null && textureFactors.length > 0) {
			int phaseIndex = getDataFileSet().getSample().getPhaseIndex(phase);
//			System.out.println("Need restore: " + needRestore[phaseIndex]);
// temporarly, to fix
		needRestore = null;
			if (needRestore != null && needRestore.length > phaseIndex && needRestore[phaseIndex]) {
				for (int i1 = 0; i1 < textureFactors[0].length; i1++) {
					for (int j = 0; j < textureFactors[0][i1].length; j++) {
						for (int j1 = 0; j1 < textureFactors[0][i1][j].length; j1++) {
							textureFactors[0][i1][j][j1] = 1.0;
							textureFactors[1][i1][j][j1] = 1.0;
						}
					}
				}
				int h, k, l;
				if (phaseIndex >= 0) {
					Vector[] tmpVector = (Vector[]) overallVector.elementAt(phaseIndex);
					int hklNumber = Math.min(tmpVector[0].size(), phase.getReflectionVector().size());
					for (int j = 0; j < hklNumber; j++) {
						for (int cp = 0; cp < positionsPerPattern; cp++)
						for (int r = 0; r < radiationsNumber; r++) {
							boolean check = true;
							int index = j;
							Reflection refl = phase.getReflectionVector().elementAt(j);
							if (checkHKL) {
								h = (int) ((double[]) tmpVector[0].elementAt(r * positionsPerPattern * hklNumber + cp * hklNumber + j))[0];
								if (refl.getH() != h)
									check = false;
								k = (int) ((double[]) tmpVector[1].elementAt(r * positionsPerPattern * hklNumber + cp * hklNumber + j))[0];
								if (refl.getK() != k)
									check = false;
								l = (int) ((double[]) tmpVector[2].elementAt(r * positionsPerPattern * hklNumber + cp * hklNumber + j))[0];
								if (refl.getL() != l)
									check = false;
							}
							if (!check) {
								int delta = 1;
								while (!check && ((j - delta) >= 0 || (j + delta) < hklNumber)) {
									if ((j - delta) >= 0) {
										check = true;
										index = j - delta;
										h = (int) ((double[]) tmpVector[0].elementAt(r * positionsPerPattern * hklNumber + cp * hklNumber + index))[0];
										if (refl.getH() != h)
											check = false;
										k = (int) ((double[]) tmpVector[1].elementAt(r * positionsPerPattern * hklNumber + cp * hklNumber + index))[0];
										if (refl.getK() != k)
											check = false;
										l = (int) ((double[]) tmpVector[2].elementAt(r * positionsPerPattern * hklNumber + cp * hklNumber + index))[0];
										if (refl.getL() != l)
											check = false;
									}
									if (!check && (j + delta) < hklNumber) {
										check = true;
										index = j + delta;
										h = (int) ((double[]) tmpVector[0].elementAt(r * positionsPerPattern * hklNumber + cp * hklNumber + index))[0];
										if (refl.getH() != h)
											check = false;
										k = (int) ((double[]) tmpVector[1].elementAt(r * positionsPerPattern * hklNumber + cp * hklNumber + index))[0];
										if (refl.getK() != k)
											check = false;
										l = (int) ((double[]) tmpVector[2].elementAt(r * positionsPerPattern * hklNumber + cp * hklNumber + index))[0];
										if (refl.getL() != l)
											check = false;
									}
									delta++;
								}
							}
							if (check) {
								if (index * positionsPerPattern < tmpVector[3].size())
									textureFactors[0][j][cp][r] = ((double[]) tmpVector[3].elementAt(r * positionsPerPattern * hklNumber + cp * hklNumber + index))[0];
								if (index * positionsPerPattern < tmpVector[4].size())
									textureFactors[1][j][cp][r] = ((double[]) tmpVector[4].elementAt(r * positionsPerPattern * hklNumber + cp * hklNumber + index))[0];
							}
						}
					}
				}
				needRestore[phaseIndex] = false;
			} else {
				for (int i1 = 0; i1 < textureFactors[0].length; i1++) {
					for (int j = 0; j < textureFactors[0][i1].length; j++) {
						for (int j1 = 0; j1 < textureFactors[0][0][0].length; j1++) {
							textureFactors[0][i1][j][j1] = 1.0;
							textureFactors[1][i1][j][j1] = 1.0;
						}
					}
				}
			}
		}
	}

	private void updateTextureFactors(double[][][][] textureFactors, double[][][][] newTextureFactors, int[] indices) {
		for (int j = 0; j < indices.length; j++) {
			if (indices[j] >= 0 && indices[j] < textureFactors[0].length)
				for (int i = 0; i < 2; i++)
					for (int k = 0; k < positionsPerPattern && k < textureFactors[0][0].length && k < newTextureFactors[0][0].length; k++)
						for (int l = 0; l < textureFactors[0][0][0].length && l < newTextureFactors[0][0][0].length; l++)
							newTextureFactors[i][j][k][l] = textureFactors[i][indices[j]][k][l];
		}
	}

	public void storeComputedTextureFactors(Phase phase, double[][][] thkl) {
		double[][][][] textureFactors = getTextureFactors(phase);
		for (int i = 0; i < textureFactors[0].length &&  i < thkl.length; i++)
			for (int i1 = 0; i1 < textureFactors[0][0].length && i1 < thkl[0].length; i1++)
				for (int i2 = 0; i2 < textureFactors[0][0][0].length && i2 < thkl[0][0].length; i2++)
					textureFactors[1][i][i1][i2] = thkl[i][i1][i2];
	}

	public void storeExperimentalTextureFactors(Phase phase, double[][][] thkl) {
		double[][][][] textureFactors = getTextureFactors(phase);
		for (int i = 0; i < textureFactors[0].length &&  i < thkl.length; i++)
			for (int i1 = 0; i1 < textureFactors[0][0].length &&  i1 < thkl[0].length; i1++)
				for (int i2 = 0; i2 < textureFactors[0][0][0].length; i2++)
					textureFactors[0][i][i1][i2] = thkl[i][i1][i2];
	}

	public void storeComputedOverExperimentalTextureFactors() {
		Sample sample = getDataFileSet().getSample();
		for (int i = 0; i < sample.numberOfPhases; i++)
			storeComputedOverExperimentalTextureFactors(sample.getPhase(i));
	}

	public void storeComputedOverExperimentalTextureFactors(Phase phase) {
		double[][][][] textureFactors = getTextureFactors(phase);
		for (int i = 0; i < textureFactors[0].length; i++)
			for (int i1 = 0; i1 < textureFactors[0][0].length; i1++)
				for (int i2 = 0; i2 < textureFactors[0][0][0].length; i2++)
					textureFactors[0][i][i1][i2] = textureFactors[1][i][i1][i2];
	}

	public void storeExperimentalOverComputedTextureFactors() {
		Sample sample = getDataFileSet().getSample();
		for (int i = 0; i < sample.numberOfPhases; i++)
			storeExperimentalOverComputedTextureFactors(sample.getPhase(i));
	}

	public void storeExperimentalOverComputedTextureFactors(Phase phase) {
		synchronized(this) {
		double[][][][] textureFactors = getTextureFactors(phase);
		for (int i = 0; i < textureFactors[0].length; i++)
			for (int i1 = 0; i1 < textureFactors[0][0].length; i1++)
				for (int i2 = 0; i2 < textureFactors[0][0][0].length; i2++)
					textureFactors[1][i][i1][i2] = textureFactors[0][i][i1][i2];
		}
	}

	public double[][][][] getTextureFactors(Phase phase) {
		double[][][][] temp = phaseTextureFactors.get(phase);
		if (temp == null) {
			int radNumber = getDataFileSet().getInstrument().getRadiationType().getLinesCount();
			temp = new double[2][phase.gethklNumber()][positionsPerPattern][radNumber];
			phaseTextureFactors.put(phase, temp);
			resetTextureFactors(phase);
		}
		return temp;
	}

	public double[][][][] getStrainFactors(Phase phase) {
		double[][][][] temp = phaseStrainFactors.get(phase);
		if (temp == null) {
			int radNumber = getDataFileSet().getInstrument().getRadiationType().getLinesCount();
			temp = new double[2][phase.gethklNumber()][positionsPerPattern][radNumber];
			phaseStrainFactors.put(phase, temp);
			resetStrainFactors(phase);
		}
		return temp;
	}

	public double[][][][] getBroadFactors(Phase phase) {
		double[][][][] temp = phaseBroadFactors.get(phase);
		if (temp == null) {
			int radNumber = getDataFileSet().getInstrument().getRadiationType().getLinesCount();
			temp = new double[2][phase.gethklNumber()][positionsPerPattern][radNumber];
			phaseBroadFactors.put(phase, temp);
			resetBroadFactors(phase);
		}
		return temp;
	}

	public double[][][][] getInstBroadFactors(Phase phase) {
		double[][][][] temp = phaseInstBroadFactors.get(phase);
		if (temp == null) {
			int radNumber = getDataFileSet().getInstrument().getRadiationType().getLinesCount();
			temp = new double[instrumentBroadeningParNumber][phase.gethklNumber()][positionsPerPattern][radNumber];
			phaseInstBroadFactors.put(phase, temp);
			resetInstBroadFactors(phase);
		}
		return temp;
	}

	public double[][][][] getCrystallitesMicrostrains(Phase phase) {
		double[][][][] temp = phaseCrystallitesMicrostrains.get(phase);
		if (temp == null) {
			int numberCrystStrains = phase.getNumberOfSizeStrainCoefficients();
			int radNumber = getDataFileSet().getInstrument().getRadiationType().getLinesCount();
			temp = new double[numberCrystStrains][phase.gethklNumber()][positionsPerPattern][radNumber];
			phaseCrystallitesMicrostrains.put(phase, temp);
			resetCrystallitesMicrostrains(phase);
		}
		return temp;
	}

	public double[][][] getPositions(Phase phase) {
		double[][][] temp = phasePositions.get(phase);
		if (temp == null) {
			int radNumber = getDataFileSet().getInstrument().getRadiationType().getLinesCount();
			temp = new double[phase.gethklNumber()][positionsPerPattern][radNumber];
			phasePositions.put(phase, temp);
			resetPositions(phase);
		}
		return temp;
	}

	public double[][][] getShapeAbsFactors(Phase phase) {
		double[][][] temp = phaseShapeAbsFactors.get(phase);
		if (temp == null) {
			int radNumber = getDataFileSet().getInstrument().getRadiationType().getLinesCount();
			temp = new double[phase.gethklNumber()][positionsPerPattern][radNumber];
			phaseShapeAbsFactors.put(phase, temp);
			resetShapeAbsFactors(phase);
		}
		return temp;
	}

	public double[][][] getLorentzPolarization(Phase phase) {
		double[][][] temp = phaseLorentzPolarization.get(phase);
		if (temp == null) {
			int radNumber = getDataFileSet().getInstrument().getRadiationType().getLinesCount();
			temp = new double[phase.gethklNumber()][positionsPerPattern][radNumber];
			phaseLorentzPolarization.put(phase, temp);
			resetLorentzPolarization(phase);
		}
		return temp;
	}

	public int[][][][] getMinMaxIndices(Phase phase) {
		int[][][][] temp = phaseMinMaxIndices.get(phase);
		if (temp == null) {
			int radNumber = getDataFileSet().getInstrument().getRadiationType().getLinesCount();
			temp = new int[2][phase.gethklNumber()][positionsPerPattern][radNumber];
			phaseMinMaxIndices.put(phase, temp);
			resetMinMaxIndices(phase);
		}
		return temp;
	}

	public int[] getReflectionIDs(Phase phase) {
		int[] temp = phaseReflectionIDs.get(phase);
		if (temp == null) {
			temp = new int[phase.gethklNumber()];
			phaseReflectionIDs.put(phase, temp);
		}
		return temp;
	}

	public int[][][] getMinIndex(Phase phase) {
		return getMinMaxIndices(phase)[0];
	}

	public int[][][] getMaxIndex(Phase phase) {
		return getMinMaxIndices(phase)[1];
	}

	public void computePosition(Phase aphase) { // no errors correction applied
		double PI_TO_2DEG = Constants.PITODEG * 2.0;
		int radNumber = getDataFileSet().getInstrument().getRadiationType().getLinesCount();
		double[][][] positions = phasePositions.get(aphase);
//		Sample sample = aphase.getSample();

		int reflNumber = aphase.gethklNumber();
		if (dspacingbase || energyDispersive) {
			for (int i = 0; i < reflNumber &&  i < positions.length; i++) {
				Reflection refl = aphase.getReflex(i);
				for (int j = 0; j < positionsPerPattern && j < positions[0].length; j++)
					for (int rad = 0; rad < radNumber && rad < positions[0][0].length; rad++)
						positions[i][j][rad] = refl.d_space;
			}
		} else {
			for (int i = 0; i < reflNumber && i < positions.length; i++) {
				Reflection refl = aphase.getReflex(i);
				for (int rad = 0; rad < radNumber && rad < positions[0][0].length; rad++) {
					double wavelength = getDataFileSet().getInstrument().getRadiationType().getRadiationWavelength(rad);
					for (int j = 0; j < positionsPerPattern && j < positions[0].length; j++) {
						double ratioposition = wavelength / (2.0 * refl.d_space);
						positions[i][j][rad] = 180.0;
						if (ratioposition < 1.0)
							positions[i][j][rad] = PI_TO_2DEG * Math.asin(ratioposition);
					}
				}
			}
		}
	}

	public void computePositionForStrained(Phase aphase) {
//		double PI_TO_2DEG = Constants.PITODEG * 2.0;
		int radNumber = getDataFileSet().getInstrument().getRadiationType().getLinesCount();
		double[][][] positions = phasePositions.get(aphase);
//		double[][][][] strain = getStrainFactors(aphase);
		Sample sample = aphase.getSample();

		int reflNumber = aphase.gethklNumber();
//		if (dspacingbase || energyDispersive) {
			for (int i = 0; i < reflNumber && i < positions.length; i++) {
				Reflection refl = aphase.getReflex(i);
				for (int j = 0; j < positionsPerPattern && j < positions[0].length; j++)
					for (int rad = 0; rad < radNumber && rad < positions[0][0].length; rad++) {
						double strain = getStrains(aphase, i)[j][rad];
						positions[i][j][rad] = computeFinalPosition(sample, refl, strain, positions[i][j][rad]);
					}
			}
/*		} else {
			for (int i = 0; i < reflNumber && i < positions.length; i++) {
				Reflection refl = aphase.getReflex(i);
				for (int j = 0; j < positionsPerPattern && j < positions[0].length; j++)
					for (int rad = 0; rad < radNumber && rad < positions[0][0].length; rad++)
						if (positions[i][j][rad] != 180)
							positions[i][j][rad] = computeFinalPosition(sample, refl, strain[0][i][j][rad], positions[i][j][rad]);
			}
		}*/
	}

	public double getCorrectedPosition(Sample asample, double x) {
		return getCorrectedPosition(asample, x, getTiltingAngle());
	}

	public double getCorrectedPosition(Sample asample, double x, double[] tilting_angles) {
		if (!dspacingbase && !energyDispersive && x == 180)
			return x;
		double position = x;
		for (int i = 0; i < thetaDisplacementN; i++) {
			position += thetaDisplacement[i] * MoreMath.pow(x, i);
		}
		if (theta2thetaMeasurement)
			x /= 2;
		if (getParameterValue(sampleDisplacementYID) != 0.0)
			if (dspacingbase)
				position -= getDSpaceFrom2Theta(Constants.PITODEG * getParameterValue(sampleDisplacementYID) *
						Math.sin(get2ThetaFromDSpace(x)));
			else if (energyDispersive)
				position -= getEnergyFrom2Theta(Constants.PITODEG * getParameterValue(sampleDisplacementYID) *
						Math.sin(get2ThetaFromEnergy(x)));
			else
				position -= Constants.PITODEG * getParameterValue(sampleDisplacementYID) * MoreMath.sind(x);
		if (getParameterValue(sampleDisplacementZID) != 0.0)
			if (dspacingbase)
				position += getDSpaceFrom2Theta(Constants.PITODEG * getParameterValue(sampleDisplacementZID) *
						Math.cos(get2ThetaFromDSpace(x)));
			else if (energyDispersive)
				position += getEnergyFrom2Theta(Constants.PITODEG * getParameterValue(sampleDisplacementZID) *
						Math.cos(get2ThetaFromEnergy(x)));
			else
				position += Constants.PITODEG * getParameterValue(sampleDisplacementZID) * MoreMath.cosd(x);

		return getDataFileSet().getCorrectedPosition(asample, position, tilting_angles, this);
	}

	public double getPositionForStrained(double position, double strain) {
		// exact treatment
		strain = Math.exp(strain);
		// approximate treatment
//		strain = 1.0 + strain;
		if (dspacingbase)
				position *= strain;
		else if (energyDispersive) {
				position /= strain;
		} else
				if (position != 180.0) {
					position = Math.sin(position * 0.5 * Constants.DEGTOPI) / strain;
					if (position != Constants.PI)
						position = 2.0 * Math.asin(position) * Constants.PITODEG;
					else
						position = 180.0;
			}
		return position;
	}

/*  public double[] getFinalPosition(Sample asample, Reflection refl, int computeStrain) {
    // in deg if an angle

    Radiation[] rad = getDataFileSet().getRadiations();
    if (dspacingbase || rad == null)
      return new double[]{refl.d_space};
    if (energyDispersive)
      return new double[]{Constants.ENERGY_LAMBDA / refl.d_space};

    double[] position = new double[rad.length];
    for (int i = 0; i < rad.length; i++) {
      position[i] = getDataFileSet().computeposition(refl.d_space, i);
    }
    computeFinalPosition(asample, refl, computeStrain, position);
//	  System.out.println(position[0]);
    return position;
  }*/

  public double computeFinalPosition(Sample asample, Reflection refl, double strain, double pos) {
    // in deg if an angle

// position from d-space + sample positioning errors
    pos = getCorrectedPosition(asample, pos);

//	  System.out.println(strain);
    if (strain != 0.0)
      pos = getPositionForStrained(pos, strain);
	  return pos;
  }


	public void computeLorentzPolarization(Phase aphase) {
		double[][][] positions = phasePositions.get(aphase);
		double[][][] lorentzPolarization = phaseLorentzPolarization.get(aphase);
		Sample asample = getDataFileSet().getSample();
		Instrument ainstrument = getDataFileSet().getInstrument();
		int radNumber = ainstrument.getRadiationType().getLinesCount();

		int reflNumber = aphase.getReflectionVector().size();
		for (int j = 0; j < positionsPerPattern; j++) {
			for (int i = 0; i < reflNumber; i++) {
				for (int k = 0; k < radNumber; k++)
					lorentzPolarization[i][j][k] = ainstrument.LorentzPolarization(this, asample, positions[i][j][k],
							                                  dspacingbase, energyDispersive);
			}
		}

	}

	public void computeShapeAbsorptionCorrection(Phase aphase) {

  	   double abs = getParameter(absorptionFactorID).getValueD();
		Sample asample = getDataFileSet().getSample();
		Instrument ainstrument = getDataFileSet().getInstrument();
		int radNumber = ainstrument.getRadiationType().getLinesCount();
		boolean isTOF = ainstrument.isTOF();
		double[][][] positions = phasePositions.get(aphase);

		double[][][] shapeAbsorption = phaseShapeAbsFactors.get(aphase);
		double[][] intensity = new double[positionsPerPattern][radNumber];

			int reflNumber = aphase.gethklNumber();
			for (int i = 0; i < reflNumber; i++) {
				for (int j = 0; j < positionsPerPattern; j++)
					for (int k = 0; k < radNumber; k++)
					intensity[j][k] = 1.0f;
				ainstrument.computeShapeAbsorptionCorrection(this, asample, positions[i],
						dspacingbase, energyDispersive, intensity);
				for (int j = 0; j < positionsPerPattern; j++) {
					double[] layer_abs = ainstrument.PhaseAndLayerAbsorption(this, asample, aphase, positions[i][j]);
					// computeAbsorptionAndPhaseQuantity(ainstrument, aphase.getSample(), aphase, positions[0][i][j]);
//					System.out.println(i + " " + intensity[j][0] + " " + layer_abs[0]);
					double linearCorrection = 1.0;
					if (isTOF) { // dspacingbase
							double arg1 = abs * getWavelengthFromDSpace(positions[i][j][0]);
							if (i == 50)
//						      System.out.println(abs + " " + positions[i][j][0] + " " + arg1 + " " + layer_abs[0]);
							if (arg1 != 0.0) {
								if (arg1 < 200.0)
									arg1 = Math.exp(-arg1);
								else
									arg1 = 0.0f;
								linearCorrection = arg1;
							}
					}

					for (int a = 0; a < layer_abs.length; a++) {
						shapeAbsorption[i][j][a] = intensity[j][a] * layer_abs[a] * linearCorrection;
					}
				}
			}
	}

	public void computeInstBroadFactor(Phase aphase) {
		double[][][] positions = phasePositions.get(aphase);
		double[][][][] instrumentBroadening = phaseInstBroadFactors.get(aphase);
		Instrument ainstrument = getDataFileSet().getInstrument();

		for (int j = 0; j < positionsPerPattern; j++) {
			int reflNumber = aphase.getReflectionVector().size();
			for (int i = 0; i < reflNumber; i++) {
				for (int rad = 0; rad < positions[0][0].length; rad++) {
					double[][] broad = ainstrument.getInstrumentalBroadeningAt(positions[i][j][rad], this);
					for (int b = 0; b < broad[0].length; b++)
						instrumentBroadening[b][i][j][rad] = broad[0][b];
				}
			}
		}

	}

	public void computeSizeStrainBroadening(Phase aphase) {

		double[][][][] sizeStrains = phaseCrystallitesMicrostrains.get(aphase);

		double[][][] positions = getPositions(aphase);
		int reflNumber = aphase.getReflectionVector().size();
		for (int i = 0; i < reflNumber; i++) {
			double crst[] = aphase.getCrystalliteMicrostrain(aphase.getReflex(i), getTextureAngles(positions[i][0][0]));
			for (int b = 0; b < crst.length; b++) {
				for (int l = 0; l < sizeStrains[0][0].length; l++)
					for (int k = 0; k < sizeStrains[0][0][0].length; k++)
						sizeStrains[b][i][l][k] = crst[b];
			}
		}

	}

	public void computeStrain(Phase aphase) {
		if (!getDataFileSet().hasNoStrain()) {
			double[][][][] strains = getStrainFactors(aphase);
			double[][][] positions = getPositions(aphase);
			int reflNumber = aphase.gethklNumber();
			for (int i = 0; i < reflNumber; i++) {
				for (int l = 0; l < strains[0][0].length; l++) {
					for (int k = 0; k < strains[0][0][0].length; k++) {
						double strain = aphase.getActiveStrain().computeStrain(aphase.getReflex(i), getTextureAngles(positions[i][l][k]));
						strains[1][i][l][k] = strain;
					}
				}
			}
		}
	}

	public void computeSampleBroadening(Phase aphase, double wave) {
 		double[] betaf = new double[]{0,0};
		int numberofpeaks = aphase.gethklNumber();
		double[][][][] sizeStrains = getCrystallitesMicrostrains(aphase);
		double[][][] positions = getPositions(aphase);
		double[][][][] instBroadening = getInstBroadFactors(aphase);
		double[][][][] phaseBroadening = getBroadFactors(aphase);

		double[] broadInst = new double[instrumentBroadeningParNumber];
		for (int i = 0; i < positionsPerPattern; i++) {
			for (int kj = 0; kj < numberofpeaks; kj++) {
				Reflection refl = aphase.getReflex(kj);
				for (int k = 0; k < sizeStrains[0][0][0].length; k++) {
					betaf[0] = aphase.getActiveSizeStrain().getBetaChauchy(refl.d_space, sizeStrains[0][kj][i][k],
							sizeStrains[1][kj][i][k]); // / 2.0;
					betaf[1] = aphase.getActiveSizeStrain().getBetaGauss(refl.d_space, sizeStrains[0][kj][i][k],
							sizeStrains[1][kj][i][k]); // / 2.0;
					if (!dspacingbase && !energyDispersive) {
						double position = positions[kj][i][0] / 2 * Constants.DEGTOPI;
						double sintheta = Math.sin(position);
						sintheta *= sintheta;
						double costheta = Math.cos(position);
						double corr = 4.0 * sintheta / (wave * costheta) * Constants.PITODEG;
						betaf[0] *= corr;
						betaf[1] *= corr;
					}
					for (int j = 0; j < instrumentBroadeningParNumber; j++)
						broadInst[j] = instBroadening[j][kj][i][k];
					double[] hwhm_eta = PseudoVoigtPeak.getHwhmEtaFromIntegralBeta(betaf, broadInst);
/*	            System.out.println("Refreshing " + hwhm_eta[0] + " " + hwhm_eta[1]
			            + " " + broadInst[0] + " " + broadInst[1]
			            + " " + betaf[0] + " " + betaf[1]);*/
					for (int j = 0; j < hwhm_eta.length; j++)
						phaseBroadening[j][kj][i][k] = hwhm_eta[j];
//				for (int j = hwhm_eta.length; j < broadInst.length; j++)
//					phaseBroadening[j][kj][i] = broadInst[j];  // asymmetry
				}
			}
		}
	}

	public void removingPhase(Phase phase) {
		phaseLorentzPolarization.remove(phase);
		phaseBroadFactors.remove(phase);
		phaseInstBroadFactors.remove(phase);
		phaseShapeAbsFactors.remove(phase);
		phaseStrainFactors.remove(phase);
		phaseCrystallitesMicrostrains.remove(phase);
		phaseMinMaxIndices.remove(phase);
		phasePositions.remove(phase);
		phaseTextureFactors.remove(phase);
	}

	public void resetForRandomTexture(Phase phase) {
		double[][][][] textureFactors = getTextureFactors(phase);
		if (textureFactors != null) {
			for (int i1 = 0; i1 < textureFactors[0].length; i1++) {
				for (int j = 0; j < textureFactors[0][0].length; j++) {
					for (int k = 0; k < textureFactors[0][0][0].length; k++)
						textureFactors[0][i1][j][k] = textureFactors[1][i1][j][k] = 1.0;
				}
			}
		}
	}

	public void resetForNoStrain(Phase phase) {
		double[][][][] strainFactors = getStrainFactors(phase);
		if (strainFactors != null) {
			for (int i1 = 0; i1 < strainFactors[0].length; i1++) {
				for (int j = 0; j < strainFactors[0][0].length; j++) {
					for (int k = 0; k < strainFactors[0][0][0].length; k++) {
						for (int i = 0; i < strainFactors.length; i++)
							strainFactors[i][i1][j][k] = 0.0;
					}
				}
			}
		}
	}

	public void computedToExperimentalTextureFactors(Phase phase) {
		double[][][][] textureFactors = getTextureFactors(phase);
		if (textureFactors != null) {
			for (int i1 = 0; i1 < textureFactors[0].length; i1++) {
				for (int j = 0; j < textureFactors[0][i1].length; j++) {
					textureFactors[0][i1][j] = textureFactors[1][i1][j];
				}
			}
		}
	}

	public void randomToTextureFactors(Phase phase) {
		double[][][][] textureFactors = getTextureFactors(phase);
		if (textureFactors != null) {
			for (int i1 = 0; i1 < textureFactors[0].length; i1++) {
				for (int j = 0; j < textureFactors[0][0].length; j++) {
					for (int k = 0; k < textureFactors[0][0][0].length; k++)
						textureFactors[0][i1][j][k] = textureFactors[1][i1][j][k];
				}
			}
		}
	}

	public boolean checkinRangeandIntensity(Phase aphase, int k) {
		boolean isIn = false;
		for (int i = 0; i < positionsPerPattern; i++) {
//			if (!getFilePar().isComputingDerivate())
//				System.out.println("position " + getPositions(aphase)[0][k][i]);
			if (xInsideRange(getPositions(aphase)[k][i][0]))
				isIn = true;
		}
		return isIn;
	}

	public boolean checkPeakInsideRange(Phase phase, int reflIndex) {
		boolean isIn = false;
		for (int i = 0; i < positionsPerPattern; i++) {
			if (xInsideRange(getPositions(phase)[reflIndex][i][0]))
				isIn = true;
		}
		return isIn;
	}

	public boolean checkPeakInsideRange(Phase phase, int reflIndex, double rangeFactor) {
		boolean isIn = false;
		for (int i = 0; i < positionsPerPattern; i++) {
			double pos = getPositions(phase)[reflIndex][i][0];
			double hwhm = Math.abs(getBroadFactors(phase)[0][reflIndex][i][0] * rangeFactor * getCutoffAngle());
			if (xInsideRange(pos) || xInsideRange(pos + hwhm) || xInsideRange(pos - hwhm))
				isIn = true;
		}
		return isIn;
	}

	public double[][] getShapeAbsFactors(Phase aphase, int j) {
		//To change body of created methods use File | Settings | File Templates.
		if (j < getShapeAbsFactors(aphase).length)
			return getShapeAbsFactors(aphase)[j];
//		System.out.println("Shape absorption for phase: " + aphase.getPhaseName() +
//				", peak requested: " + j + ", maximum stored: " + phaseShapeAbsFactors.get(aphase).length);
		return getShapeAbsFactors(aphase)[getShapeAbsFactors(aphase).length - 1];
	}

	public double[][] getLorentzPolarization(Phase aphase, int j) {
		//To change body of created methods use File | Settings | File Templates.
		return getLorentzPolarization(aphase)[j];
	}

	public double[][] getExperimentalTextureFactors(Phase aphase, int j) {
		return getTextureFactors(aphase)[0][j];  //To change body of created methods use File | Settings | File Templates.
	}

	public double[][] getTextureFactors(Phase aphase, int j) {
		if (j > getTextureFactors(aphase)[1].length)
			out.println(aphase.getPhaseName() + " " + aphase.gethklNumber() + " " + j + " " + getTextureFactors(aphase)[1].length);
		return getTextureFactors(aphase)[1][j];  //To change body of created methods use File | Settings | File Templates.
	}

	public double[][] getStrains(Phase aphase, int j) {
		return getStrainFactors(aphase)[1][j];  //To change body of created methods use File | Settings | File Templates.
	}

/*no more	public void setStrain(Phase aphase, int j, double value) {
		double[][] strain = getStrainFactors(aphase);  //To change body of created methods use File | Settings | File Templates.
// todo add also for more points par pattern
		strain[j][0] = value;
	}*/

	public double[][] getExpTextureFactor(Phase phase, Peak peak) {
//		return peak.getReflex().getExpTextureFactor(getIndex(), peak.getLayer().getIndex());
		return getExperimentalTextureFactors(phase, peak.getOrderPosition());
	}

	public void setExpTextureFactor(Phase phase, Peak peak, int pointNumber, double value) {
//		peak.getReflex().setExpTextureFactor(getIndex(), peak.getLayer().getIndex(), value);
		double[] expValues = getExperimentalTextureFactors(phase, peak.getOrderPosition())[pointNumber];
		for (int i = 0; i < expValues.length; i++)
			expValues[i] = value;
	}

	public void setTextureFactor(Phase phase, Peak peak, int pointNumber, double value) {
//		peak.getReflex().setTextureFactor(getIndex(), peak.getLayer().getIndex(), value);
		setTextureFactor(phase, peak.getOrderPosition(), pointNumber, value);
	}

	public void setTextureFactor(Phase phase, int reflIndex, int pointNumber, double value) {
//		peak.getReflex().setTextureFactor(getIndex(), peak.getLayer().getIndex(), value);
		double[] calcValues = getTextureFactors(phase, reflIndex)[pointNumber];
		for (int i = 0; i < calcValues.length; i++)
			calcValues[i] = value;
	}

	public void setTextureFactors(Phase aphase, double[] textureValues) {
		if (!getDataFileSet().hasRandomTexture()) {
			synchronized (this) {
				double[][][] textF = getTextureFactors(aphase)[1];
				int hkln = aphase.gethklNumber();
				for (int i = 0; i < hkln; i++)
					for (int k = 0; k < textF[i].length; k++)
						for (int l = 0; l < textF[0][0].length; l++)
							textF[i][k][l] = textureValues[i];
			}
		}
	}

	public void setTextureFactors(Phase aphase, int i, double[] textureValues) {
		if (!getDataFileSet().hasRandomTexture()) {
			synchronized (this) {
				double[][][] textF = getTextureFactors(aphase)[1];
				int index = 0;
				for (int k = 0; k < textF[0].length; k++)
					for (int l = 0; l < textF[0][0].length; l++) {
						textF[i][k][l] = textureValues[index++];
					}
			}
		}
	}

	public void setStrain(Phase phase, Peak peak, int pointNumber, double value) {
		//		peak.getReflex().setStrain(getIndex(), peak.getLayer().getIndex(), value);
		double[] expValues = getStrains(phase, peak.getOrderPosition())[pointNumber];
		for (int i = 0; i < expValues.length; i++)
			expValues[i] = value;
	}

/*	@Override
	protected void finalize() throws Throwable {
		intensity = null;
		intensityCalibrated = null;
		weight = null;
		twotheta = null;
		twothetaOriginal = null;
		twothetacalibrated = null;
		phasesfit = null;
		phaseFit.removeAllElements();
		phaseFit = null;
		bkgfit = null;
		intbkgfit = null;
		classlistcs = null;
		tilting_angles = null;
		corrected_tilting_angles = null;
		phaseLorentzPolarization = null;
		phaseBroadFactors = null;
		phaseInstBroadFactors = null;
		phaseShapeAbsFactors = null;
		phaseStrainFactors = null;
		phaseCrystallitesMicrostrains = null;
		phaseMinMaxIndices = null;
		phasePositions = null;
		phaseTextureFactors = null;
		super.finalize();
	}*/

	public void setMeasurementDate(String measurementDate) {
		// need to be already in a format for cif:  yyyy-mm-dd
		String measurementTime = getMeasurementTime();
		if (measurementDate.indexOf("-") != 4) {
			// not in proper format
			return;
		}
		setString(dateTimeFieldID, measurementDate + "T" + measurementTime);
	}

	public void setMeasurementTime(String measurementTime) {
		// need to be already in a format for cif:  hh:mm:ss+zz     +zz is the time zone, optional
		String measurementDate = getMeasurementDate();
		if (measurementTime.indexOf(":") != 2) {
			// not in proper format
			return;
		}
		setString(dateTimeFieldID, measurementDate + "T" + measurementTime);
	}

	public String getMeasurementDate() {
		StringTokenizer st = new StringTokenizer(stringField[dateTimeFieldID], "T");
		if (st.hasMoreTokens())
			return st.nextToken();
		setMeasurementDate("1984-01-01");
		return getMeasurementDate();
	}

	public String getMeasurementTime() {
		StringTokenizer st = new StringTokenizer(stringField[dateTimeFieldID], "T");
		if (st.hasMoreTokens()) {
			st.nextToken();
			if (st.hasMoreTokens()) {
				String time = st.nextToken();
				if (time.contains("+")) {
					return time.substring(0, time.indexOf("+"));
				}
				return time;
			}
		}
		return "00:00:00";
	}

	public String[] getMeasurementDateAndTime() {
		String[] dateAndTime = new String[2];
		dateAndTime[0] = getMeasurementDate();
		dateAndTime[1] = getMeasurementTime();
		return dateAndTime;
	}

	public long getMeasurementTimeInMilliSec() {
		getMeasurementDate(); // to check/update the value
		SimpleDateFormat ft = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		String dateTime = getMeasurementDate() + " " + getMeasurementTime();
		if (dateTime.contains("+"))
			dateTime = dateTime.substring(0, dateTime.indexOf("+"));
		Date t;
		try {
			t = ft.parse(dateTime);
			return t.getTime();
		} catch (ParseException e) {
			out.println(dateTime + " is an unparseable date using " + ft);
		}
		return 0l;
	}

	public void addAndMean(Vector<DiffrDataFile> groupOfDatafiles) {
		int total = groupOfDatafiles.size() + 1;
		double[] angles = new double[maxAngleNumber];
		for (int j = 0; j < maxAngleNumber; j++)
			angles[j] = tilting_angles[j];
		double countTime = getCountTimeValue();
		double dataWeight = datafileWeight;
		int minSetSize = getTotalNumberOfData();
		for (int i = 0; i < groupOfDatafiles.size(); i++) {
			DiffrDataFile datafileToAdd = groupOfDatafiles.elementAt(i);
			for (int j = 0; j < maxAngleNumber; j++)
				angles[j] += datafileToAdd.tilting_angles[j];
			countTime += datafileToAdd.getCountTimeValue();
			dataWeight += datafileToAdd.datafileWeight;
			int setSize = getTotalNumberOfData();
			if (setSize < minSetSize)
				minSetSize = setSize;
		}
		for (int i = 0; i < maxAngleNumber; i++)
			angles[i] /= total;
		for (int j = 0; j < maxAngleNumber; j++) {
			tilting_angles[j] = angles[j];
			setString(j + 1, Double.toString(angles[j]));
		}
		setCountTime(Double.toString(countTime));  // we set this or the weight as summed and not the mean value
		setDatafileWeight(Double.toString(dataWeight / total));

		for (int i = 0; i < groupOfDatafiles.size(); i++) {
			DiffrDataFile datafileToAdd = groupOfDatafiles.elementAt(i);
			for (int j = 0; j < minSetSize; j++) {
				intensity[j] += datafileToAdd.intensity[j];
				twotheta[j] += datafileToAdd.twotheta[j];
				twothetacalibrated[j] += datafileToAdd.twothetacalibrated[j];
				twothetaOriginal[j] += datafileToAdd.twothetaOriginal[j];
				weight[j] += datafileToAdd.weight[j];
				if (intensityCalibrated != null)
					intensityCalibrated[j] += datafileToAdd.intensityCalibrated[j];
				if (phasesfit != null)
					phasesfit[j] += datafileToAdd.phasesfit[j];
				if (bkgfit != null)
					bkgfit[j] += datafileToAdd.bkgfit[j];
				if (intbkgfit != null)
					intbkgfit[j] += datafileToAdd.intbkgfit[j];
				if (expbkgfit != null)
					expbkgfit[j] += datafileToAdd.expbkgfit[j];
			}
		}
		for (int j = 0; j < minSetSize; j++) {
			intensity[j] /= total;
			twotheta[j] /= total;
			twothetacalibrated[j] /= total;
			twothetaOriginal[j] /= total;
			weight[j] /= total;
			if (intensityCalibrated != null)
				intensityCalibrated[j] /= total;
			if (phasesfit != null)
				phasesfit[j] /= total;
			if (bkgfit != null)
				bkgfit[j] /= total;
			if (intbkgfit != null)
				intbkgfit[j] /= total;
			if (expbkgfit != null)
				expbkgfit[j] /= total;
		}

		realRangeCut(0, minSetSize);  // careful we skip the check on calibration problem
		calibrated = false;

		// todo images?
	}

	public double getIncidentDetectorAngle(int pointIndex) {
		// in rad
		// to do
		AngularCalibration angcal = getDataFileSet().getInstrument().getAngularCalibration();
		double[][] tmat = null;
		if (angcal instanceof AngularInclinedFlatImageCalibration)
			tmat = ((AngularInclinedFlatImageCalibration) angcal).getTmat(corrected_tilting_angles[0]);
		return angcal.getBeamInclination(this, pointIndex, tmat);
	}

	public int getChannelForZero() {
		return getDataFileSet().getInstrument().getAngularCalibration().getChannelForZero(this);
	}

	public double getChannelStep() {
		return getDataFileSet().getInstrument().getAngularCalibration().getChannelStep(this);
	}

	@Override
	public JOptionsDialog getOptionsDialog(Frame parent) {
		return new JDataFileOptionsD(parent, this);
	}

	public double getDiffractionIntensityForFluorescence(double energyInKeV, double twothetadetector) {
		return getParameterValue(scaleFactorDiffractionFluoID);
	}

	class JDataFileOptionsD extends JOptionsDialog {

    JParameterListPane[] polynomialP;
    JTextField displacementZTF = null;
    JTextField displacementYTF = null;
    JTextField absorptionFactorTF = null;

		JCheckBox chebUseCB;

    public JDataFileOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JTabbedPane p1 = new JTabbedPane();
      principalPanel.add(p1, BorderLayout.CENTER);
      String p1String[] = {"Background pol.",
          "Error position pol.", "As bkg counts", "Sample par."};


	    JPanel polinomialBkgPanel = new JPanel();
	    polinomialBkgPanel.setLayout(new BorderLayout(3, 3));
	    JPanel chebPanel = new JPanel();
	    chebPanel.setLayout(new FlowLayout(FlowLayout.RIGHT, 1, 1));

	    polynomialP = new JParameterListPane[Nparameterloop];

	    chebUseCB = new JCheckBox("Chebyshev polynomial");
	    chebUseCB.setToolTipText(
			    "Check this box to use Chebyshev polynomial instead of normal polynomial function");
	    chebPanel.add(chebUseCB);
	    polinomialBkgPanel.add(BorderLayout.NORTH, chebPanel);
	    polynomialP[0] = new JParameterListPane(this, false, true);
	    p1.addTab(p1String[0], null, polinomialBkgPanel);
	    polinomialBkgPanel.add(BorderLayout.CENTER, polynomialP[0]);

      for (int i = 1; i < Nparameterloop; i++) {
        polynomialP[i] = new JParameterListPane(this, false, true);
        p1.addTab(p1String[i], null, polynomialP[i]);
      }

      JPanel samplePanel = new JPanel(new GridLayout(0, 1, 3, 3));
      p1.addTab(p1String[3], null, samplePanel);

      JPanel jp1 = new JPanel(new FlowLayout());
      jp1.add(new JLabel("Sample displacement y/2R:"));
      displacementYTF = new JTextField(Constants.FLOAT_FIELD);
      jp1.add(displacementYTF);
      samplePanel.add(jp1);
      jp1 = new JPanel(new FlowLayout());
      jp1.add(new JLabel("Sample displacement z/2R:"));
      displacementZTF = new JTextField(Constants.FLOAT_FIELD);
      jp1.add(displacementZTF);
      samplePanel.add(jp1);
	    jp1 = new JPanel(new FlowLayout());
	    jp1.add(new JLabel("Absorption factor:"));
	    absorptionFactorTF = new JTextField(Constants.FLOAT_FIELD);
	    jp1.add(absorptionFactorTF);
	    samplePanel.add(jp1);

      setTitle("Additional parameters for datafile: " + this.toString());
      initParameters();
      pack();
    }

    @Override
    public void initParameters() {
      for (int i = 0; i < Nparameterloop; i++)
        polynomialP[i].setList(DiffrDataFile.this, i);
      displacementYTF.setText(getParameter(sampleDisplacementYID).getValue());
      addComponenttolist(displacementYTF, getParameter(sampleDisplacementYID));
      displacementZTF.setText(getParameter(sampleDisplacementZID).getValue());
      addComponenttolist(displacementZTF, getParameter(sampleDisplacementZID));
	    absorptionFactorTF.setText(getParameter(absorptionFactorID).getValue());
	    addComponenttolist(absorptionFactorTF, getParameter(absorptionFactorID));
	    chebUseCB.setSelected(useChebyshevPolynomials());
    }

    @Override
    public void retrieveParameters() {
      for (int i = 0; i < Nparameterloop; i++)
        polynomialP[i].retrieveparlist();
      getParameter(sampleDisplacementYID).setValue(displacementYTF.getText());
      removeComponentfromlist(displacementYTF);
      getParameter(sampleDisplacementZID).setValue(displacementZTF.getText());
      removeComponentfromlist(displacementZTF);
	    getParameter(absorptionFactorID).setValue(absorptionFactorTF.getText());
	    removeComponentfromlist(absorptionFactorTF);
	    useChebyshevPolynomials(chebUseCB.isSelected());
    }

    @Override
    public void dispose() {
      for (int i = 0; i < Nparameterloop; i++)
        polynomialP[i].dispose();
      polynomialP = null;
      displacementYTF = null;
      displacementZTF = null;
	    absorptionFactorTF = null;

//			theDataFileSet = null;
      super.dispose();
    }

  }
  public void exportExperimentalComputedData(BufferedWriter output) {
	  exportExperimentalComputedData(output, true);
  }
  public void exportExperimentalComputedData(BufferedWriter output, boolean closeFile) {

    int numberphases = getFilePar().getActiveSample().phasesNumber();

    if (output != null) {

      try {
        int nPoints = computeDataNumber();

        output.write("_pd_block_id  " + title);
        output.newLine();
        output.newLine();

        output.write("_pd_meas_number_of_points " + Integer.toString(nPoints));
        output.newLine();
//        if (datafile[0].originalNotCalibrated)
//          output.write("_riet_meas_datafile_calibrated false");
//        else
        output.write("_riet_meas_datafile_calibrated true");
        output.newLine();
        output.newLine();
        output.write("# On the following loop you will have:");
        output.newLine();
        output.write("#  2theta/d_coordinate  experimental_intensity  calculated_intensity  background  esd    exp_calibrated_intensity  calc_calibrated_intensity");
        for (int j = 0; j < numberphases; j++)
          output.write("  intensity " + getFilePar().getActiveSample().getPhase(j));
        output.newLine();
        output.newLine();
        output.write("loop_");
        output.newLine();
        output.write(DiffrDataFile.CIFXcoord2T);
        output.newLine();
        output.write("_pd_proc_intensity_total");
        output.newLine();
        output.write(DiffrDataFile.intensityCalcCIFstring);
        output.newLine();
        output.write("_pd_proc_intensity_bkg_calc");
        output.newLine();
        output.write("_pd_proc_intensity_weight");
        output.newLine();
        output.write("_pd_proc_intensity_total_cal");
        output.newLine();
        output.write(DiffrDataFile.intensityCalcCIFstring + "_cal");
        output.newLine();
        for (int j = 0; j < numberphases; j++) {
          output.write(DiffrDataFile.intensityCalcCIFstring);
          output.newLine();
        }
	      double qExp = getFilePar().getQexpForWeightingScheme();
        for (int i = startingindex; i < finalindex; i++) {
	      double intensE = getYData(i);
          double intensEcal = finalIntensityCalibration(intensE);
          double intens = getFit(i);
          double intenscal = finalIntensityCalibration(intens);
          double xcoorddata = 0.0;
//          if (datafile[0].originalNotCalibrated)
//            xcoorddata = datafile[0].getXDataOriginal(i);
//          else
          xcoorddata = getXData(i);
          output.write(" " + Fmt.format(xcoorddata) + " " + Fmt.format(intensE) + " " + Fmt.format(intens));
          output.write(" " + Fmt.format(getBkgFit(i)));
          output.write(" " + Fmt.format(getWeight(i, qExp)));
          output.write(" " + Fmt.format(intensEcal));
          output.write(" " + Fmt.format(intenscal));
          for (int j = 0; j < numberphases; j++)
            output.write(" " + Fmt.format(getPhaseFit(i, j)));
          output.newLine();
        }
      } catch (IOException io) {
      }
	  if (closeFile) {
        try {
          output.close();
        } catch (IOException io) {
        }
      }
    }
  }

}
