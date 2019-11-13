/*
 * @(#)DataFileSet.java created 01/01/1997 Mesiano
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

import java.util.*;

import static java.lang.System.*;
import static java.util.Collections.sort;
import java.io.*;
import java.awt.*;
import java.util.concurrent.TimeUnit;

import it.unitn.ing.rista.chemistry.XRayDataSqLite;
import it.unitn.ing.rista.diffr.instrument.DefaultInstrument;
import it.unitn.ing.rista.io.cif.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.interfaces.Peak;


/**
 * The DataFileSet is a class that control a set of datafiles.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.35 $, $Date: 2006/12/04 14:30:04 $
 * @since JDK1.1
 */

public class DataFileSet extends XRDcat {

  protected static String[] diclistc = new String[]{
		  "_pd_meas_datetime_initiated",
		  "_pd_meas_info_author_name", "_riet_meas_datafile_format",
		  "_pd_proc_ls_background_function", "_pd_proc_ls_profile_function",
		  "_pd_proc_ls_peak_cutoff",
		  "_pd_proc_2theta_range_min", "_pd_proc_2theta_range_max",
		  "_pd_proc_2theta_range_inc", "_diffrn_ambient_pressure", "_diffrn_ambient_temperature",
		  "_riet_lorentz_restricted", "_riet_par_background_interpolated",
		  "_riet_par_background_interpolation_range",
		  "_riet_meas_dataset_compute", "_riet_meas_datafile_replace",
		  "_riet_meas_dataset_random_texture", "_maud_background_add_automatic",
		  "_maud_interpolated_background_iterations",
		  "_pd_proc_ls_datafile_weight", "_riet_meas_dataset_no_strain",

		  "_riet_par_background_exp_shift",
		  "_riet_par_background_exp_thermal_shift",
		  "_pd_spec_orientation_omega",
		  "_pd_spec_orientation_chi",
		  "_pd_spec_orientation_phi",
    "_pd_meas_orientation_omega_offset",
    "_pd_meas_orientation_chi_offset",
    "_pd_meas_orientation_phi_offset",
    "_pd_meas_orientation_eta_offset",
		  "_riet_par_spec_displac_x", "_riet_par_spec_displac_y", "_riet_par_spec_displac_z",

		  "_riet_par_background_pol", "_riet_par_background_chi", "_riet_par_background_eta",

		  "_riet_intensity_extraction", "_riet_position_extraction", "_reflectivity_model_type",
		  "_diffrn_measurement_device", "_fluorescence_model_type", "_diffraction_model_type",

		  "_pd_proc_info_excluded_regions", "_riet_par_background_peak_id",
		  "_riet_meas_datafile_name", "_original_diffraction_image"};
  protected static String[] diclistcrm = new String[]{
		  "_pd_meas_datetime_initiated",
		  "_pd_meas_info_author_name", "_riet_meas_datafile_format",
		  "_pd_proc_ls_background_function", "_pd_proc_ls_profile_function",
		  "_pd_proc_ls_peak_cutoff",
		  "_pd_proc_2theta_range_min", "_pd_proc_2theta_range_max",
		  "_pd_proc_2theta_range_inc", "_diffrn_ambient_pressure", "_diffrn_ambient_temperature",
		  "_riet_lorentz_restricted", "_riet_par_background_interpolated",
		  "_riet_par_background_interpolation_range",
		  "_riet_meas_dataset_compute", "_riet_meas_datafile_replace",
		  "_riet_meas_dataset_random_texture", "_maud_background_add_automatic",
		  "number of iteration for background interpolation",
		  "_pd_proc_ls_datafile_weight", "_riet_meas_dataset_no_strain",

/*    "_pd_meas_orientation_omega_offset",
    "_pd_meas_orientation_chi_offset",
    "_pd_meas_orientation_phi_offset",
    "_pd_meas_orientation_eta_offset",*/
		  "_riet_par_background_exp_shift",
		  "_riet_par_background_exp_thermal_shift",
		  "Additional sample omega",
		  "Additional sample chi",
		  "Additional sample phi",
		  "omega disalignement",
		  "chi disalignement",
		  "phi disalignement",
		  "eta disalignement",
		  "x sample displacement", "y sample displacement", "z sample displacement",

		  "background polynomial coeff ", "chi background coeff ", "eta background coeff",

		  "_riet_intensity_extraction", "_riet_position_extraction", "_reflectivity_model_type",
		  "_diffrn_measurement_device", "_fluorescence_model_type", "_diffraction_model_type",

		  "_pd_proc_info_excluded_regions", "_riet_par_background_peak_id",
		  "_riet_meas_datafile_name", "_original_diffraction_image"};

  protected static String[] classlistc = new String[]{"superclass:it.unitn.ing.rista.diffr.Region",
		  "superclass:it.unitn.ing.rista.diffr.BkgPeak",
		  "superclass:it.unitn.ing.rista.diffr.DiffrDataFile",
		  "superclass:it.unitn.ing.rista.diffr.DiffractionImageDatafile"/*,
"superclass:it.unitn.ing.rista.diffr.CustomAddition"*/
  };

  public static String[] classlistcs = new String[]{"superclass:it.unitn.ing.rista.diffr.IntensityExtractor",
		  "superclass:it.unitn.ing.rista.diffr.PositionExtractor",
		  "superclass:it.unitn.ing.rista.diffr.Reflectivity",
		  "superclass:it.unitn.ing.rista.diffr.Instrument",
		  "superclass:it.unitn.ing.rista.diffr.Fluorescence",
		  "superclass:it.unitn.ing.rista.diffr.Diffraction"};

  public static String[] peakFunctionClass = new String[]{"PseudoVoigt"};
  public static boolean[] needDist = new boolean[]{false};
  public static int nFunction = 1;
  public static final int instrumentID = 0;

  int activedatanumber;
  private int[] datafileindex;

  Vector<Peak> thepeaklist = new Vector<Peak>(0, 1);

  int theindex = -1;
	public boolean omogeneousDataset = true;

  int interpolatedPoints = 5;
  public final static int bkgExpShift = 0;
  public final static int bkgExpThermalShift = 1;
  public final static int sampleOmegaID = 2;
  public final static int sampleChiID = 3;
  public final static int samplePhiID = 4;
	public final static int disalignementOmegaID = 5;
	public final static int displacementxID = 9;

  public boolean isBackgroundExperimental = false;

  public static final int minRangeID = 6;
  public static final int maxRangeID = 7;
  public static final int lorentzRestrictedID = 11;
  public static final int backgroundInterpolatedID = 12;
  public static final int interpolatedPointsID = 13;
  public static final int enabledID = 14;
  public boolean enabled = true;
  public static final int replaceID = 15;
  public boolean replaceDatafile = false;
  public static final int automaticPolynomialBackgroundID = 17;
  public boolean automaticPolynomialBackground = false;
  public static final int randomTextureID = 16;
  public boolean randomTexture = false;
  public boolean noStrain = false;
  boolean hasNegative2theta = false;
  double[] sample_angles = new double[3];
	double[] disalignement_angles = new double[4];
	double[] displacement_errors = new double[3];
	public static int interpolationIterationsID = 18;
	protected int interpolationIterations;
	static final int datasetWeightFieldID = 19;
	static final int noStrainID = 20;
	double datasetWeight = 1.0;
	boolean[] needRestore = null;
	Vector overallVector = null;

	private Map<Phase, double[][]> phaseStructureFactors = new Hashtable<Phase, double[][]>();
	private Map<Phase, double[][][][]> phaseScatFactors = new Hashtable<Phase, double[][][][]>();

	public DataFileSet(XRDcat afile, String alabel) {
    super(afile, alabel);
    initXRD();
    identifier = "dataset";
  }

  public DataFileSet(XRDcat afile) {
    this(afile, "DataFileSet_x");
  }

	public DataFileSet() {}

  @Override
  public void initConstant() {
    Nstring = 21;
    Nstringloop = 0;
    Nparameter = 12;
    Nparameterloop = 3;
    Nsubordinate = classlistcs.length;
    Nsubordinateloop = classlistc.length;
  }

    @Override
  public void initDictionary() {
    arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
    arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
    arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
  }

    @Override
  public void initParameters() {
    super.initParameters();
    setLorentzRestricted("true");
    setBackgroundInterpolated("false");
    setEnabled("true");
    setReplaceDatafile(MaudPreferences.getPref("datafile.replaceOnAdd", "false"));
    setAutomaticPolynomialBackground(MaudPreferences.getPref("datafile.addOwnBackgroundPolynomial", "false"));
    setInterpolatedPoints(MaudPreferences.getInteger("backgroundSubtraction.interpolatedPointsInterval", 150));
    setPeakCutoff(MaudPreferences.getPref("pseudoVoigt.defaultPeakCutoff", "30"));
    setMaxRange("0");
    setMinRange("0");
    setIntensityExtractor("Le Bail");
    setPositionExtractor("none pe");
    setInstrument(DefaultInstrument.modelID);
	 setDiffraction("Basic diffraction");
    setReflectivity("none reflectivity");
    setFluorescence("none fluorescence");
    stringField[0] = "Date/time meas"; // to avoid the notify staff
    setRandomTexture("false");
    setNoStrain("false");
	  setBackgroundInterpolationIterations(MaudPreferences.getInteger("backgroundSubtraction.iterations", 10));
	  setString(datasetWeightFieldID, "1.0");
	    for (int i = 0; i < Nparameter; i++)
		    parameterField[i] = new Parameter(this, getParameterString(i), 0);
  }

	@Override
  public String toDataString() {
    return "dataset_" + toXRDcatString();
  }

/*  public void setLabel(String alabel) {
    setDataFileSetID(alabel);
  }*/

  double[] backgroundChi, backgroundEta, backgroundPol; // todo: background for 2theta and energy
  int nchibckgpar, netabckgpar, npolbckgpar;
  Vector bkgDatafiles = new Vector(0, 1);

	public void checkConsistencyForVersion(double version) {
		if (version < 1.821) {
			if (getFluorescenceMethod().startsWith("none") && getReflectivityMethod().startsWith("none"))
				setDiffraction("Basic diffraction");
		}
	}

	@Override
  public void updateParametertoDoubleBuffering(boolean firstLoading) {

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);
//      setRadiation();

//	    System.out.println("Refreshing angles!");
      sample_angles[0] = getParameterValue(sampleOmegaID); // omega
      sample_angles[1] = getParameterValue(sampleChiID); // chi
      sample_angles[2] = getParameterValue(samplePhiID); // phi
	    for (int i = 0; i < 4; i++)
	      disalignement_angles[i] = getParameterValue(disalignementOmegaID + i);
	    for (int i = 0; i < 3; i++)
		    displacement_errors[i] = getParameterValue(displacementxID + i);

      backgroundChi = getParameterLoopVector(chiBackgroundID);
      backgroundEta = getParameterLoopVector(etaBackgroundID);
      backgroundPol = getParameterLoopVector(backgroundID);

      nchibckgpar = numberOfLoopParameters[chiBackgroundID];
      netabckgpar = numberOfLoopParameters[etaBackgroundID];
      npolbckgpar = numberOfLoopParameters[backgroundID];

  }

    @Override

    public void updateStringtoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;

	  boolean refreshBackgroundInterpolation = false;

	  if (interpolatedPoints != Integer.valueOf(getInterpolatedPoints())) {
	    interpolatedPoints = Integer.valueOf(getInterpolatedPoints());
	    refreshBackgroundInterpolation = true;
    }
	  if (interpolationIterations != Integer.valueOf(getInterpolationIterations())) {
		  interpolationIterations = Integer.valueOf(getInterpolationIterations());
		  refreshBackgroundInterpolation = true;
	  }
	  if (refreshBackgroundInterpolation) {
		  removeManualInterpolation(false);
	  }
		isBackgroundExperimental = false;

    int alldatafilenumber = datafilesnumber();
    for (int i = 0; i < alldatafilenumber; i++) {
      DiffrDataFile bdatafile = getDataFile(i);
      if (bdatafile.getAsBackgroundPermission())
        isBackgroundExperimental = true;
    }
    enabled = isEnabled();
    replaceDatafile = mustReplace();
    automaticPolynomialBackground = isAutomaticPolynomialBackground();
    randomTexture = hasRandomTexture();
    noStrain = hasNoStrain();
	    bkgDatafiles.removeAllElements();
      for (int i = 0; i < datafilesnumber(); i++) {
        DiffrDataFile tmpDatafile = getDataFile(i);
        if (tmpDatafile.getAsBackgroundPermission()) {
          bkgDatafiles.add(tmpDatafile);
        }
      }
	    datasetWeight = Double.parseDouble(getString(datasetWeightFieldID));
    }


	public double[] dataForPlot = null;

	public double[] getDataForPlot() {
		return dataForPlot;
	}

	public double[] datafitForPlot = null;

	public double[] getDatafitForPlot() {
		return datafitForPlot;
	}

	public double[] backgroundForPlot = null;

	public double[] getBackgroundForPlot() {
		return backgroundForPlot;
	}

	public double[][] dataphaseForPlot = null;

	public double[][] getDataphaseForPlot() {
		return dataphaseForPlot;
	}

	public void updateDataForPlot() {

		dataForPlot = null;
		datafitForPlot = null;
		backgroundForPlot = null;
		dataphaseForPlot = null;

		DiffrDataFile[] datafile = getActiveDataFiles();
		if (datafile == null || datafile.length == 0)
			return;

		int ylength = datafile.length;
		int startingIndex = datafile[0].startingindex;
		int finalIndex = datafile[0].finalindex;
		double xmin = 1.0E10, xmax = 0.0;

		// minimum maximum range

		if (datafile[0].increasingX()) {
			if (xmin > datafile[0].getXDataForPlot(datafile[0].startingindex))
				xmin = datafile[0].getXDataForPlot(datafile[0].startingindex);
			if (xmax < datafile[0].getXDataForPlot(datafile[0].finalindex - 1))
				xmax = datafile[0].getXDataForPlot(datafile[0].finalindex - 1);
			if (xmin > datafile[0].getXDataForPlot(datafile[0].finalindex - 1))
				xmin = datafile[0].getXDataForPlot(datafile[0].finalindex - 1);
			if (xmax < datafile[0].getXDataForPlot(datafile[0].startingindex))
				xmax = datafile[0].getXDataForPlot(datafile[0].startingindex);
			for (int is1 = 1; is1 < ylength; is1++) {
				if (startingIndex > datafile[is1].startingindex) {
					startingIndex = datafile[is1].startingindex;
				}
				if (finalIndex < datafile[is1].finalindex) {
					finalIndex = datafile[is1].finalindex;
				}
				if (xmin > datafile[is1].getXDataForPlot(datafile[is1].startingindex))
					xmin = datafile[is1].getXDataForPlot(datafile[is1].startingindex);
				if (xmax < datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1))
					xmax = datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1);
				if (xmin > datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1))
					xmin = datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1);
				if (xmax < datafile[is1].getXDataForPlot(datafile[is1].startingindex))
					xmax = datafile[is1].getXDataForPlot(datafile[is1].startingindex);
			}
		} else {
			xmin = 1.0E10;
			xmax = 0.0;
			if (xmin > datafile[0].getXDataForPlot(datafile[0].startingindex))
				xmin = datafile[0].getXDataForPlot(datafile[0].startingindex);
			if (xmax < datafile[0].getXDataForPlot(datafile[0].finalindex - 1))
				xmax = datafile[0].getXDataForPlot(datafile[0].finalindex - 1);
			if (xmin > datafile[0].getXDataForPlot(datafile[0].finalindex - 1))
				xmin = datafile[0].getXDataForPlot(datafile[0].finalindex - 1);
			if (xmax < datafile[0].getXDataForPlot(datafile[0].startingindex))
				xmax = datafile[0].getXDataForPlot(datafile[0].startingindex);
			for (int is1 = 1; is1 < ylength; is1++) {
				if (startingIndex > datafile[is1].startingindex) {
					startingIndex = datafile[is1].startingindex;
				}
				if (finalIndex < datafile[is1].finalindex) {
					finalIndex = datafile[is1].finalindex;
				}
				if (xmin > datafile[is1].getXDataForPlot(datafile[is1].startingindex))
					xmin = datafile[is1].getXDataForPlot(datafile[is1].startingindex);
				if (xmax < datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1))
					xmax = datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1);
				if (xmin > datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1))
					xmin = datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1);
				if (xmax < datafile[is1].getXDataForPlot(datafile[is1].startingindex))
					xmax = datafile[is1].getXDataForPlot(datafile[is1].startingindex);
			}
		}
		int xlength = finalIndex - startingIndex;
		double stepX = (xmax - xmin) / (xlength - 1);
		int np = xlength;


		if (np > 0) {
			dataForPlot = new double[2 * np];
			if (datafile[0].hasfit()/* || peaksLocated todo ripristinare */) {
				datafitForPlot = new double[2 * np];
			}
			int mode = PlotDataFile.checkScaleModeX();
			for (int is1 = 0; is1 < xlength; is1++) {
				int is2 = is1 * 2;
				dataForPlot[is2] = xmin + is1 * stepX;
				int total = 0;
				int totalFit = 0;

// data

				for (int sn = 0; sn < ylength; sn++) {
					double xstartmin = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
					double xendmax = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
					if (xendmax < datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode))
						xendmax = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
					if (xstartmin > datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode))
						xstartmin = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
					if (dataForPlot[is2] >= xstartmin && dataForPlot[is2] <= xendmax) {
						dataForPlot[is2 + 1] += datafile[sn].getInterpolatedYSqrtIntensity(dataForPlot[is2], 2, mode);
						total++;
					}
				}
				if (total > 0)
					dataForPlot[is2 + 1] /= total;

// fit

				if (datafile[0].hasfit()) {
					datafitForPlot[is2] = dataForPlot[is2];

					for (int sn = 0; sn < ylength; sn++) {
						double xstartmin = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
						double xendmax = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
						if (xendmax < datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode))
							xendmax = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
						if (xstartmin > datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode))
							xstartmin = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
						if (datafitForPlot[is2] >= xstartmin && datafitForPlot[is2] <= xendmax) {
							datafitForPlot[is2 + 1] += datafile[sn].getInterpolatedFitSqrtIntensity(datafitForPlot[is2], 2, mode);
							totalFit++;
						}
					}
					if (totalFit > 0)
						datafitForPlot[is2 + 1] /= totalFit;
				}
			}

// Phases fit

			if (datafitForPlot != null) {
				int numberphases = getFilePar().getActiveSample().phasesNumber();
				dataphaseForPlot = new double[numberphases][2 * np];
				for (int s = 0; s < numberphases; s++) {
					Phase phase = getFilePar().getActiveSample().getPhase(s);
					if (phase.plotFit()) {
						int j;
						for (int i = j = 0; i < np; i++, j += 2) {
							int totalFit = 0;
							for (int sn = 0; sn < ylength; sn++) {
								double xstartmin = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
								double xendmax = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
								if (xendmax < datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode))
									xendmax = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
								if (xstartmin > datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode))
									xstartmin = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
								if (datafitForPlot[j] >= xstartmin && datafitForPlot[j] <= xendmax) {
									dataphaseForPlot[s][j + 1] += datafile[sn].getInterpolatedFitSqrtIntensity(datafitForPlot[j], 2, mode, s);
									totalFit++;
								}
							}
							if (totalFit > 0)
								dataphaseForPlot[s][j + 1] /= totalFit;
							dataphaseForPlot[s][j] = datafitForPlot[j];

						}
					}
				}

// Background

				backgroundForPlot = new double[2 * np];
				int j;
				for (int i = j = 0; i < np; i++, j += 2) {
					int totalFit = 0;
					for (int sn = 0; sn < ylength; sn++) {
						double xstartmin = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
						double xendmax = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
						if (xendmax < datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode))
							xendmax = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
						if (xstartmin > datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode))
							xstartmin = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
						if (datafitForPlot[j] >= xstartmin && datafitForPlot[j] <= xendmax) {
							backgroundForPlot[j + 1] += datafile[sn].getInterpolatedBkgFitSqrtIntensity(datafitForPlot[j], 2, mode);
							totalFit++;
						}
					}
					if (totalFit > 0)
						backgroundForPlot[j + 1] /= totalFit;
					backgroundForPlot[j] = datafitForPlot[j];
				}
			}

		}
	}

	public double getDatasetWeight() {
		return datasetWeight;
	}

	public String getDatasetWeightString() {
		return getString(datasetWeightFieldID);
	}

	public void setDatasetWeight(String value) {
		setString(datasetWeightFieldID, value);
	}

	public void setGroupCount(int groupCount) {
    int alldatafilenumber = datafilesnumber();
    for (int i = 0; i < alldatafilenumber; i++) {
      DiffrDataFile bdatafile = getDataFile(i);
      bdatafile.setGroupCount(groupCount);
    }
  }

  public String getGroupCount() {
    if (datafilesnumber() > 0) {
      return (getDataFile(0)).getGroupCount();
    } else
      return "1";
  }

  public String getDataFileSetID() {
    return getString(0);
  }

  public void setDataFileSetID(String astring) {
    super.setString(0, astring);
  }

  public int getInstrumentID() {
    return 3;
  }

  public void setInstrument(String value) {
//    System.out.println("Setting Instrument (in dataset):" + value);
    if (subordinateField[getInstrumentID()] == null ||
        !value.equals((subordinateField[getInstrumentID()]).identifier))
      setsubordinateField(getInstrumentID(), value);
  }

  public void setInstrument(int number) {
    setInstrument(getsubordIdentifier(getInstrumentID(), number));
  }

  public void setInstrument(XRDcat inst) {
    inst.setParent(this);
    setsubordinateField(getInstrumentID(), inst);
    inst.setLabel("Diffraction Instrument");
  }

  public void setNewInstrument(int number) {
    setsubordinateField(getInstrumentID(), getsubordIdentifier(getInstrumentID(), number));
  }

  public Instrument getInstrument() {
    if (subordinateField[getInstrumentID()] == null) {
 //     System.out.println("Inizialize new Instrument (in dataset)");
      setInstrument(0);
      getInstrument().initializeAsNew();
    }
    return (Instrument) subordinateField[getInstrumentID()];
  }

  public void loadInstrument(String filename, Frame aframe) {
    CIFParser instrumentParser = new CIFParser(filename, aframe, this, "Instrument");
      Object[] ainstrument = instrumentParser.getMainCat();
      if (ainstrument != null && ainstrument[0] != null) {
        getFilePar().loadingFile = true;
        subordinateField[getInstrumentID()] = (XRDcat) ainstrument[0];
        getFilePar().loadingFile = false;
        notifyUpObjectChanged(this, Constants.OBJECT_ADDED);
        refreshAll(true);
      }      
  }

  @Override
  public int setField(String cif, String astring, String astringerror, String min, String max, boolean free,
                      String refName, String refBound, String constant, String ratio, String expression,
                      boolean autoTrace, boolean positive) {
    int index1 = super.setField(cif, astring, astringerror, min, max, free,
                   refName, refBound, constant, ratio, expression, autoTrace, positive);
    if (cif.equalsIgnoreCase("_diffrn_measurement_device")) {
      // check if old parameter file containing instruments
      if (FilePar.instruments != null && FilePar.instruments.size() > 0) {
        for (int i = 0; i < FilePar.instruments.size(); i++) {
          Instrument inst = (Instrument) FilePar.instruments.elementAt(i);
          if (inst.getLabel().equalsIgnoreCase(astring)) {
            setInstrument(inst);
            return index1;
          }
        }
      }
    }
    return index1;
  }


  public Parameter getomega() {
    return parameterField[sampleOmegaID];
  }

  public Parameter getchi() {
    return parameterField[sampleChiID];
  }

  public Parameter getphi() {
    return parameterField[samplePhiID];
  }


  public ListVector getDataFileList() {
    return subordinateloopField[2];
  }

  public ListVector getDataImageList() {
    return subordinateloopField[3];
  }

  public boolean askForRange = false;

  public DiffrDataFile[] addDataFileforName(String filename, boolean browsing) {
    askForRange = browsing;
    if (replaceDatafile())
      removeAllFiles();
//	  Vector<DiffrDataFile> listActualDatafiles = new Vector<DiffrDataFile>(datafilesnumber());
//	  for (int i = 0; i < datafilesnumber(); i++)
//	    listActualDatafiles.add(getDataFile(i));
	  int actualNumber = datafilesnumber(); // listActualDatafiles.size();
	  addDatafiles(filename);
	  int datafilesAddedNumber = datafilesnumber() - actualNumber;
	  DiffrDataFile[] succeed = new DiffrDataFile[datafilesAddedNumber];
	  for (int i = actualNumber; i < datafilesnumber(); i++)
		  succeed[i - actualNumber] = getDataFile(i);
    askForRange = false;
    return succeed;
  }

	public XRDcat[] addDatafiles(String alabel) {
		try {
			XRDcat obj = (XRDcat) factory(this, alabel, filterClass(classlist[2], alabel));
			addsubordinateloopField(2, obj);
		} catch (CannotCreateXRDcat e) {
			e.printStackTrace();
		} catch (PrototypeNotFound ex) {
			ex.printStackTrace();
		}
		return null;
	}

	public void checkOwnPolynomial() {
    for (int i = 0; i < datafilesnumber(); i++) {
      getDataFile(i).checkOwnPolynomial();
    }
  }

  public void removeSelectedDataFile() {
    Constants.refreshTreePermitted = false;
    removeselSubLField(2);
    Constants.refreshTreePermitted = true;
    notifyUpObjectChanged(this, 0);
  }

  public void disableByAngles(int angleNumber, double startingAngle, double finalAngle) {
    Constants.refreshTreePermitted = false;
    double angle;
    Vector datafiles = getSelectedDatafiles();
    int numberDatafiles = datafiles.size();
    for (int i = 0; i < numberDatafiles; i++) {
      DiffrDataFile diffrDatafile = (DiffrDataFile) datafiles.elementAt(i);
	    angle = diffrDatafile.getAngleValue(angleNumber);
      if (angle < finalAngle && angle > startingAngle)
        disable(diffrDatafile);
    }
    Constants.refreshTreePermitted = true;
    notifyUpObjectChanged(this, 0);
  }

	public void disableByAngles(int angleNumber, double startingAngle, double finalAngle, double everyAngle) {
		Constants.refreshTreePermitted = false;
		double angle;
		Vector datafiles = getSelectedDatafiles();
		int numberDatafiles = datafiles.size();
		for (double angleIncremented = startingAngle; angleIncremented < finalAngle; angleIncremented += everyAngle) {
			for (int i = 0; i < numberDatafiles; i++) {
				DiffrDataFile diffrDatafile = (DiffrDataFile) datafiles.elementAt(i);
				angle = diffrDatafile.getAngleValue(angleNumber);
				if (Math.abs(angle - angleIncremented) < Math.abs(everyAngle/1000))
					disable(diffrDatafile);
			}
		}
    Constants.refreshTreePermitted = true;
    notifyUpObjectChanged(this, 0);
	}

	public void groupByAngles(int angleNumber, double startingAngle, double finalAngle, double everyAngle) { // todo
		Constants.refreshTreePermitted = false;
		double angle;
		Vector datafiles = getSelectedDatafiles();
		int numberDatafiles = datafiles.size();
		for (double angleIncremented = startingAngle; angleIncremented < finalAngle; angleIncremented += everyAngle) {
			for (int i = 0; i < numberDatafiles; i++) {
				DiffrDataFile diffrDatafile = (DiffrDataFile) datafiles.elementAt(i);
				angle = diffrDatafile.getAngleValue(angleNumber);
				if (Math.abs(angle - angleIncremented) < Math.abs(everyAngle/1000))
					disable(diffrDatafile);
			}
		}
		Constants.refreshTreePermitted = true;
		notifyUpObjectChanged(this, 0);
	}

	private void disable(DiffrDataFile diffrDatafile) {
	  diffrDatafile.setCompute(false);
  }

	private void enable(DiffrDataFile diffrDatafile) {
		diffrDatafile.setCompute(true);
	}

	public void removeEvery(int number) {
		Constants.refreshTreePermitted = false;
		Vector datafiles = getSelectedDatafiles();
		int numberDatafiles = datafiles.size();
		int index = 1;
		for (int i = 0; i < numberDatafiles; i++, index++) {
			DiffrDataFile diffrDatafile = (DiffrDataFile) datafiles.elementAt(i);
			if (index == number) {
				disable(diffrDatafile);
				index = 0;
			}
		}
		Constants.refreshTreePermitted = true;
		notifyUpObjectChanged(this, 0);
	}

	public void enableEvery(int number) {
		Constants.refreshTreePermitted = false;
		Vector datafiles = getSelectedDatafiles();
		int numberDatafiles = datafiles.size();
		int index = 1;
		for (int i = 0; i < numberDatafiles; i++, index++) {
			DiffrDataFile diffrDatafile = (DiffrDataFile) datafiles.elementAt(i);
			if (index == number) {
				enable(diffrDatafile);
				index = 0;
			} else
				disable(diffrDatafile);
		}
		Constants.refreshTreePermitted = true;
		notifyUpObjectChanged(this, 0);
	}

	public void groupEvery(int number) {  // todo
		Constants.refreshTreePermitted = false;
		Vector datafiles = getSelectedDatafiles();
		int numberDatafiles = datafiles.size();
		int index = 1;
		Vector<DiffrDataFile> groupOfDatafiles = new Vector<DiffrDataFile>(number);
		for (int i = 0; i < numberDatafiles; i++, index++) {
			DiffrDataFile diffrDatafile = (DiffrDataFile) datafiles.elementAt(i);
			diffrDatafile.updateParametertoDoubleBuffering(false);
			diffrDatafile.updateStringtoDoubleBuffering(false);
			groupOfDatafiles.add(diffrDatafile);
			if (index == number) {
				groupAndRemove(groupOfDatafiles);
				index = 0;
			}
		}
		Constants.refreshTreePermitted = true;
		notifyUpObjectChanged(this, 0);
	}

	private void groupAndRemove(Vector<DiffrDataFile> groupOfDatafiles) {
		if (groupOfDatafiles == null) // paranoid check
			return;
		if (groupOfDatafiles.size() > 1) {
			DiffrDataFile primaryDatafile = groupOfDatafiles.firstElement();
			groupOfDatafiles.removeElementAt(0);
			primaryDatafile.addAndMean(groupOfDatafiles);
			getFilePar().setStoreSpectraOption(true);
			primaryDatafile.setLabel(primaryDatafile.getLabel() + "_grouped");
			for (int j = 0; j < groupOfDatafiles.size(); j++)
				removeDatafile(groupOfDatafiles.elementAt(j));
			groupOfDatafiles.removeAllElements();
		} else if (groupOfDatafiles.size() == 1)
			groupOfDatafiles.removeAllElements();
	}

	public void removeDataFile(int index) {
    removeSubLField(2, index);
  }

  public void removeImageFile(int index) {
    removeSubLField(3, index);
  }

  public DiffrDataFile getSelectedDataFile() {
    return (DiffrDataFile) subordinateloopField[2].selectedElement();
  }

  public DiffrDataFile getDataFile(int index) {
    return (DiffrDataFile) subordinateloopField[2].elementAt(index);
  }

  public DiffractionImageDatafile getDiffractionImage(int index) {
    return (DiffractionImageDatafile) subordinateloopField[3].elementAt(index);
  }

  public DiffrDataFile getActiveDataFile(int index) {
    return getDataFile(datafileindex[index]);
  }

  public Vector getSelectedDatafiles() {
    return subordinateloopField[2].selectedElements();
  }

  public DiffrDataFile[] getSelectedDataFiles() {
    Vector datafiles = getSelectedDatafiles();
    int numberDatafiles = datafiles.size();
    DiffrDataFile[] diffrDatafiles = new DiffrDataFile[numberDatafiles];
    for (int i = 0; i < numberDatafiles; i++) {
      diffrDatafiles[i] = (DiffrDataFile) datafiles.elementAt(i);
    }
    return diffrDatafiles;
  }

  public void removeAllDisabledFiles() {
    Constants.refreshTreePermitted = false;
    int datafilenumber = datafilesnumber();

    for (int i = datafilenumber; i > 0; i--) {
      DiffrDataFile tmpdatafile = getDataFile(i - 1);
      if (!tmpdatafile.getComputePermission())
        removeDataFile(i - 1);
    }
    Constants.refreshTreePermitted = true;
    notifyUpObjectChanged(this, 0);
//		updateStringtoDoubleBuffering();
  }

  public void removeAllFiles() {
    Constants.refreshTreePermitted = false;
    int datafilenumber = datafilesnumber();

    for (int i = datafilenumber; i > 0; i--) {
//			DiffrDataFile tmpdatafile = (DiffrDataFile) getDataFile(i-1);
      removeDataFile(i - 1);
    }
    Constants.refreshTreePermitted = true;
    notifyUpObjectChanged(this, 0);
//		updateStringtoDoubleBuffering();
  }

  public void setStatusAllFiles(boolean status) {
    int datafilenumber = datafilesnumber();

    for (int i = datafilenumber; i > 0; i--) {
      DiffrDataFile tmpdatafile = getDataFile(i - 1);
      tmpdatafile.setCompute(status);
    }
//		updateStringtoDoubleBuffering();
  }

  public void setStatusAllSelectedFiles(boolean status) {
    Vector datafiles = getSelectedDatafiles();
    if (datafiles == null) {
      (new AttentionD(new Frame(), "No item from the list selected!")).setVisible(true);
      return;
    }
    int datafilenumber = datafiles.size();
    for (int i = 0; i < datafilenumber; i++) {
      DiffrDataFile tmpdatafile = (DiffrDataFile) datafiles.elementAt(i);
      tmpdatafile.setCompute(status);
    }
  }

  public void setFittingOutputAllSelectedFiles(boolean status) {
    Vector datafiles = getSelectedDatafiles();
    if (datafiles == null) {
      (new AttentionD(new Frame(), "No item from the list selected!")).setVisible(true);
      return;
    }
    int datafilenumber = datafiles.size();
    for (int i = 0; i < datafilenumber; i++) {
      DiffrDataFile tmpdatafile = (DiffrDataFile) datafiles.elementAt(i);
      tmpdatafile.setGeneratePlotfile(status);
    }
  }

  public void setAsBkgAllSelectedFiles(boolean status) {
    Vector datafiles = getSelectedDatafiles();
    if (datafiles == null) {
      (new AttentionD(new Frame(), "No item from the list selected!")).setVisible(true);
      return;
    }
    int datafilenumber = datafiles.size();
    for (int i = 0; i < datafilenumber; i++) {
      DiffrDataFile tmpdatafile = (DiffrDataFile) datafiles.elementAt(i);
      tmpdatafile.setAsBackground(status);
    }
  }

  public void setNewAngles(int[] mult, double[] offset) {
    Vector datafiles = getSelectedDatafiles();
    if (datafiles == null) {
      (new AttentionD(new Frame(), "No item from the list selected!")).setVisible(true);
      return;
    }
    int datafilenumber = datafiles.size();
    for (int i = 0; i < datafilenumber; i++) {
      DiffrDataFile tmpdatafile = (DiffrDataFile) datafiles.elementAt(i);
      tmpdatafile.setNewAngles(mult, offset);
    }
  }

  public void setIndex(int index) {
    theindex = index;
  }

/*  public int getIndex() {
    if (theindex == -1)
      refreshIndex();
    return theindex;
  }

  public void refreshIndex() {
    getFilePar().refreshDataIndices();
  }*/

	public void refreshIndices(Phase phase) {
		int numberOfReflections = phase.gethklNumber();
//		System.out.println("Refreshing structure factors -------------------- " + numberOfReflections);
		double[][] structureFactors = phaseStructureFactors.get(phase);
		if (structureFactors == null) {
			structureFactors = new double[4][numberOfReflections];
			phaseStructureFactors.put(phase, structureFactors);
//			System.out.println("Resetting structure factors");
			resetStructureFactors(phase);
		} else if (structureFactors[0].length != numberOfReflections) {
//			System.out.println("Problem here, to be implemented");
//			System.out.println("Updating structure factors");
			double[][] newStructureFactors = new double[4][numberOfReflections];

			updateStructureFactorsFromTo(phase, structureFactors, newStructureFactors);

			phaseStructureFactors.remove(phase);
			phaseStructureFactors.put(phase, newStructureFactors);
//			resetStructureFactors(phase);
		}
		reloadScatteringFactors(phase);
/*		double[][][][] scatFactors = phaseScatFactors.get(phase);
		int linesCount = 1; // todo, should be this in the end: getInstrument().getRadiationType().getLinesCount();
		if (scatFactors == null) {
			scatFactors = new double[numberOfReflections][linesCount][phase.getFullAtomList().size()][2];
			phaseScatFactors.put(phase, scatFactors);
			reloadScatteringFactors(phase);
		} else if (scatFactors.length != numberOfReflections ||
				(scatFactors.length > 0 && scatFactors[0].length != linesCount) ||
				(scatFactors.length > 0 && scatFactors[0].length > 0 && scatFactors[0][0].length != phase.getFullAtomList().size())
				|| resetScatFactors) {
			phaseScatFactors.remove(phase);
			scatFactors = new double[numberOfReflections][linesCount][phase.getFullAtomList().size()][2];
			phaseScatFactors.put(phase, scatFactors);
			reloadScatteringFactors(phase);
		}*/

		for (int i = 0; i < activedatafilesnumber(); i++) {
			getActiveDataFile(i).refreshIndices(phase);
		}
	}

	private void updateStructureFactorsFromTo(Phase phase, double[][] structureFactors, double[][] newStructureFactors) {
		int numberOfReflections = phase.gethklNumber();
		for (int i = 0; i < numberOfReflections; i++) {
			Reflection refl = phase.getReflex(i);
			int reflID = Reflection.getUniqueIdFor(refl.getH(), refl.getK(), refl.getL());
			boolean stop = false;
			int incr = 0;
			int index = i;
			if (index < structureFactors[3].length && reflID == structureFactors[3][index])
				stop = true;
			while (!stop) {
				index = i + incr;
				boolean outOfRange = true;
				if (index < structureFactors[3].length) {
					outOfRange = false;
					if (reflID == structureFactors[3][index])
						stop = true;
				}
				index = i - incr;
				if (index >= 0 && index < structureFactors[3].length) {
					outOfRange = false;
					if (reflID == structureFactors[3][index])
						stop = true;
				}
				if (outOfRange && incr > 10) {
					stop = true;
					index = -1;
				}
				incr++;
			}
			if (index >=0) {
				for (int j = 0; j < 4; j++) {
					newStructureFactors[j][i] = structureFactors[j][index];
				}
			} else {
				newStructureFactors[0][i] = Constants.STARTING_STRUCTURE_FACTOR * Constants.STARTING_STRUCTURE_FACTOR;
				newStructureFactors[1][i] = -1.0;
				newStructureFactors[2][i] = 0.0;
				newStructureFactors[3][i] = reflID;
			}
		}
	}

	static boolean checkHKL = true;

	private void resetStructureFactors(Phase phase) {
		double[][] structureFactors = getStructureFactors(phase);
		if (structureFactors != null && structureFactors.length != 0) {
//      System.out.println("resetting exp structure factors");
			int phaseIndex = getSample().getPhaseIndex(phase);
			if (needRestore != null && phaseIndex < needRestore.length && needRestore[phaseIndex]) {
//				System.out.println("Loading structure factors");
				for (int i1 = 0; i1 < structureFactors[0].length; i1++) {
					structureFactors[0][i1] = Constants.STARTING_STRUCTURE_FACTOR * Constants.STARTING_STRUCTURE_FACTOR;
					structureFactors[1][i1] = -1.0;
					structureFactors[2][i1] = 0.0;
					Reflection refl = phase.getReflex(i1);
					structureFactors[3][i1] = Reflection.getUniqueIdFor(refl.getH(), refl.getK(), refl.getL());
				}
				int h = -1, k = -1, l = -1;
				if (phaseIndex >= 0) {
					Vector[] tmpVector = (Vector[]) overallVector.elementAt(phaseIndex);
					int hklNumber = Math.min(tmpVector[0].size(), phase.gethklNumber());
					for (int j = 0; j < hklNumber; j++) {
						boolean check = true;
						int index = j;
						Reflection refl = phase.getReflectionVector().elementAt(j);
						if (checkHKL) {
							h = (int) ((double[]) tmpVector[0].elementAt(j))[0];
							if (refl.getH() != h)
								check = false;
							k = (int) ((double[]) tmpVector[1].elementAt(j))[0];
							if (refl.getK() != k)
								check = false;
							l = (int) ((double[]) tmpVector[2].elementAt(j))[0];
							if (refl.getL() != l)
								check = false;
						}
						if (!check) {
							int delta = 1;
							while (!check && ((j - delta) >= 0 || (j + delta) < hklNumber)) {
								if ((j - delta) >= 0) {
									check = true;
									index = j - delta;
									h = (int) ((double[]) tmpVector[0].elementAt(index))[0];
									if (refl.getH() != h)
										check = false;
									k = (int) ((double[]) tmpVector[1].elementAt(index))[0];
									if (refl.getK() != k)
										check = false;
									l = (int) ((double[]) tmpVector[2].elementAt(index))[0];
									if (refl.getL() != l)
										check = false;
								}
								if (!check && (j + delta) < hklNumber) {
									check = true;
									index = j + delta;
									h = (int) ((double[]) tmpVector[0].elementAt(index))[0];
									if (refl.getH() != h)
										check = false;
									k = (int) ((double[]) tmpVector[1].elementAt(index))[0];
									if (refl.getK() != k)
										check = false;
									l = (int) ((double[]) tmpVector[2].elementAt(index))[0];
									if (refl.getL() != l)
										check = false;
								}
								delta++;
							}
						}
						if (check) {
							if (j < tmpVector[3].size())
								structureFactors[0][j] = ((double[]) tmpVector[3].elementAt(index))[0];
							if (j < tmpVector[4].size())
								structureFactors[1][j] = ((double[]) tmpVector[4].elementAt(index))[0];
							if (j < tmpVector[5].size())
								structureFactors[2][j] = ((double[]) tmpVector[5].elementAt(index))[0];
							structureFactors[3][j] = Reflection.getUniqueIdFor(refl.getH(), refl.getK(), refl.getL());
						} else
							out.println("Reflection: " + h + "," + k + "," + l + " not corresponding on loading structure factors");
					}
				}
				needRestore[phaseIndex] = false;
			} else {
				for (int i1 = 0; i1 < structureFactors[0].length; i1++) {
					structureFactors[0][i1] = Constants.STARTING_STRUCTURE_FACTOR * Constants.STARTING_STRUCTURE_FACTOR;
					structureFactors[1][i1] = -1.0;
					structureFactors[2][i1] = 0.0;
					Reflection refl = phase.getReflectionVector().elementAt(i1);
					structureFactors[3][i1] = Reflection.getUniqueIdFor(refl.getH(), refl.getK(), refl.getL());
				}
			}
		}
	}

	public void storeComputedStructureFactors(Phase phase, double[] fhkl) {
		double[][] structureFactors = phaseStructureFactors.get(phase);
		arraycopy(fhkl, 0, structureFactors[1], 0, structureFactors[0].length);
	}

	public void storeExperimentalStructureFactors(Phase phase, double[] fhkl) {
		double[][] structureFactors = phaseStructureFactors.get(phase);
		arraycopy(fhkl, 0, structureFactors[0], 0, structureFactors[0].length);
	}

	public void storeComputedOverExperimentalStructureFactors() {
		Sample sample = getSample();
		for (int i = 0; i < sample.numberOfPhases; i++)
			storeComputedOverExperimentalStructureFactors(sample.getPhase(i));
	}

	public void storeComputedOverExperimentalStructureFactors(Phase phase) {
		double[][] structureFactors = phaseStructureFactors.get(phase);
		arraycopy(structureFactors[1], 0, structureFactors[0], 0, structureFactors[0].length);
	}

	public void storeExperimentalOverComputedStructureFactors() {
		Sample sample = getSample();
		for (int i = 0; i < sample.numberOfPhases; i++)
			storeExperimentalOverComputedStructureFactors(sample.getPhase(i));
	}

	public void storeExperimentalOverComputedStructureFactors(Phase phase) {
		double[][] structureFactors = phaseStructureFactors.get(phase);
		arraycopy(structureFactors[0], 0, structureFactors[1], 0, structureFactors[0].length);
	}

		public double[][] getStructureFactors(Phase phase) {
		 return phaseStructureFactors.get(phase);
	 }

	public void reloadScatteringFactors(Phase phase) {
		Vector<AtomSite> atoms = phase.getFullAtomList();
		int atomsNumber = atoms.size();
		int numberofpeaks = phase.gethklNumber() + 1;
		int linesCount = 1; // todo, should be this in the end: getInstrument().getRadiationType().getLinesCount();
		double[][][][] scatFactors = phaseScatFactors.get(phase);
		if (scatFactors == null) {
			scatFactors = new double[numberofpeaks][linesCount][atomsNumber][2];
			phaseScatFactors.put(phase, scatFactors);
		} else if (scatFactors.length != numberofpeaks ||
				(scatFactors.length > 0 && scatFactors[0].length != linesCount) ||
				(scatFactors.length > 0 && scatFactors[0].length > 0 && scatFactors[0][0].length != atomsNumber)) {
			phaseScatFactors.remove(phase);
			scatFactors = new double[numberofpeaks][linesCount][atomsNumber][2];
			phaseScatFactors.put(phase, scatFactors);
		}
		double[] fu = new double[2];
//		boolean isXray = !rad1.isElectron() && !rad1.isNeutron();
		for (int j = 0; j < linesCount; j++) {
			Radiation rad1 = getInstrument().getRadiationType().getRadiation(j);
//			double lambda = getInstrument().getRadiationType().getRadiationWavelength(j);
//			double energyInKeV = Constants.ENERGY_LAMBDA / lambda * 0.001;
			fu[0] = 0;
			fu[1] = 0;
			for (int ato = 0; ato < atomsNumber; ato++) {
				AtomSite atom = atoms.elementAt(ato);
				double[] scatteringFactors = atom.scatfactor(0, rad1);
				scatFactors[0][j][ato][0] = scatteringFactors[0] + fu[0];
				scatFactors[0][j][ato][1] = scatteringFactors[1] + fu[1];
				for (int kj = 1; kj < numberofpeaks; kj++) {
					Reflection refl = phase.getReflex(kj - 1);
					scatteringFactors = atom.scatfactor(refl.d_space, rad1);
					scatFactors[kj][j][ato][0] = scatteringFactors[0] + fu[0];
					scatFactors[kj][j][ato][1] = scatteringFactors[1] + fu[1];
				}
			}
		}
	}

	public double[][][][] getScatteringFactor(Phase phase) {
		return phaseScatFactors.get(phase);
	}

	public void removingPhase(Phase phase) {
		phaseScatFactors.remove(phase);
		phaseStructureFactors.remove(phase);
		for (int i = 0; i < datafilesnumber(); i++)
			getDataFile(i).removingPhase(phase);
	}

	public void writeCustomObject(BufferedWriter out) {

		Sample asample = getFilePar().getActiveSample();
		if (getFilePar().compactSavingStructureFactors()) // we do not save the structure factors in compact saving
			return;

		try {
			out.newLine();
			out.write("#custom_object_" + "Fhkl");
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
				out.write(CIFdictionary.refln_FsquaredMeas + " ");
				out.write(CIFdictionary.refln_FsquaredCalc + " ");
				out.write(CIFdictionary.refln_FsquaredEsd + " ");
				out.newLine();
//				String waveS = Fmt.format(wave[i]);

				int numberReflections = phase.gethklNumber();
				double[][] structureFactors = phaseStructureFactors.get(phase);
				if (structureFactors != null) {
				for (int j = 0; j < numberReflections && j < structureFactors[0].length; j++) {
					Reflection refl = phase.getReflex(j);
					out.write(refl.getH() + " " + refl.getK() + " " + refl.getL() + " " +
							Fmt.format(structureFactors[0][j]) + " " + Fmt.format(structureFactors[1][j]) + " " +
							Fmt.format(structureFactors[2][j]));
					out.newLine();
				}
				out.newLine();
				}
			}
			out.newLine();
			out.write("#end_custom_object_" + "Fhkl");
			out.newLine();
			out.newLine();
		} catch (IOException ioe) {
			System.out.println("Error in writing the structure factors for " + toXRDcatString());
			ioe.printStackTrace();
			try {
				out.newLine();
				out.write("#end_custom_object_" + "Fhkl");
				out.newLine();
				out.newLine();
			} catch (IOException e) {
				e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
			}
		}

	}

	public void readCustomObject(CIFtoken ciffile) {
		// to be override by subclasses
		// the default read and do nothing

		int newtoken, tokentype;
//		XRDcat theobj = null;
		boolean endofInput = false;
		int hklindex = 0, phaseindex = 0, cifentry = 0, tmpVindex = 0;
		int[] cifindex = new int[6];
		Vector cifVector = new Vector(0, 1);
		Vector[] tmpVector = null;
		overallVector = new Vector(0, 1);
		boolean newLoop = false;
		boolean startLoop = false;
		int maxCIFentries = 6;

		try {
			do {
				tokentype = ciffile.nextToken();
				switch (tokentype) {
					case CIFtoken.TT_DATA:
					case CIFtoken.TT_PHASE:
						if (tmpVector != null) {
							overallVector.addElement(tmpVector);
						}
//						phaseindex++;
						tmpVindex = 0;
						tmpVector = null;
						cifVector = new Vector(0, 1);
						cifentry = 0;
						newLoop = false;
						break;
					case CIFtoken.TT_CIFE:
						// CIF item
						cifVector.addElement(ciffile.thestring);
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
								String thecife = (String) cifVector.elementAt(i);
								if (thecife.equalsIgnoreCase(CIFdictionary.refln_h)) {
									cifindex[i] = 0;
									tmpVindex++;
								} else if (thecife.equalsIgnoreCase(CIFdictionary.refln_k)) {
									cifindex[i] = 1;
									tmpVindex++;
								} else if (thecife.equalsIgnoreCase(CIFdictionary.refln_l)) {
									cifindex[i] = 2;
									tmpVindex++;
								} else if (thecife.equalsIgnoreCase(CIFdictionary.refln_FsquaredMeas)) {
									cifindex[i] = 3;
									tmpVindex++;
								} else if (thecife.equalsIgnoreCase(CIFdictionary.refln_FsquaredCalc)) {
									cifindex[i] = 4;
									tmpVindex++;
								} else if (thecife.equalsIgnoreCase(CIFdictionary.refln_FsquaredEsd)) {
									cifindex[i] = 5;
									tmpVindex++;
								}else
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

/*		if (theobj != null)
			theobj.readall(ciffile);*/
	}

	public void SumDatafileOutput(Frame aframe) {
    int datanumber = 0;
    double[] xcoord = null;
    double[] intensity = null;

    String filename = Utility.openFileDialog(aframe, "Save summed datafile as (CIF file)",
        FileDialog.SAVE, getFilePar().getDirectory(), null, "put a name (with extension).cif");
    if (filename == null)
      return;

    String[] folderAndName = Misc.getFolderandName(filename);

    String folder = folderAndName[0];
    filename = folderAndName[1];

    if (!filename.endsWith(".cif"))
      filename += ".cif";
//    int numberOfFilesTotal = 0;
    Vector datafiles = getSelectedDatafiles();
    if (datafiles == null) {
      (new AttentionD(new Frame(), "No item from the list selected!")).setVisible(true);
      return;
    }
    int ylength = datafiles.size();
    BufferedWriter output = Misc.getWriter(folder, filename);
    try {

      DiffrDataFile tmpdatafile = (DiffrDataFile) datafiles.elementAt(0);
      int startingIndex = tmpdatafile.startingindex;
      int finalIndex = tmpdatafile.finalindex;
      double xmin = 1.0E10, xmax = 0.0;
      if (xmin > tmpdatafile.getXDataForPlot(tmpdatafile.startingindex))
        xmin = tmpdatafile.getXDataForPlot(tmpdatafile.startingindex);
      if (xmax < tmpdatafile.getXDataForPlot(tmpdatafile.finalindex - 1))
        xmax = tmpdatafile.getXDataForPlot(tmpdatafile.finalindex - 1);
      if (xmin > tmpdatafile.getXDataForPlot(tmpdatafile.finalindex - 1))
        xmin = tmpdatafile.getXDataForPlot(tmpdatafile.finalindex - 1);
      if (xmax < tmpdatafile.getXDataForPlot(tmpdatafile.startingindex))
        xmax = tmpdatafile.getXDataForPlot(tmpdatafile.startingindex);
      for (int is1 = 1; is1 < ylength; is1++) {
        tmpdatafile = (DiffrDataFile) datafiles.elementAt(is1);
        if (startingIndex > tmpdatafile.startingindex) {
          startingIndex = tmpdatafile.startingindex;
        }
        if (finalIndex < tmpdatafile.finalindex) {
          finalIndex = tmpdatafile.finalindex;
        }
        if (xmin > tmpdatafile.getXDataForPlot(tmpdatafile.startingindex))
          xmin = tmpdatafile.getXDataForPlot(tmpdatafile.startingindex);
        if (xmax < tmpdatafile.getXDataForPlot(tmpdatafile.finalindex - 1))
          xmax = tmpdatafile.getXDataForPlot(tmpdatafile.finalindex - 1);
        if (xmin > tmpdatafile.getXDataForPlot(tmpdatafile.finalindex - 1))
          xmin = tmpdatafile.getXDataForPlot(tmpdatafile.finalindex - 1);
        if (xmax < tmpdatafile.getXDataForPlot(tmpdatafile.startingindex))
          xmax = tmpdatafile.getXDataForPlot(tmpdatafile.startingindex);
      }
      int xlength = finalIndex - startingIndex;
      double stepX = (xmax - xmin) / (xlength - 1);
//      int np = xlength;
      if (xlength <= 0) {
        return;
      }
      xcoord = new double[xlength];
      intensity = new double[xlength];
      int mode = 0;  // default
      for (int is1 = 0; is1 < xlength; is1++) {
      	tmpdatafile = (DiffrDataFile) datafiles.elementAt(0);
        xcoord[is1] = tmpdatafile.getXDataOriginal(tmpdatafile.startingindex + is1);
        int total = 0;
        for (int sn = 0; sn < ylength; sn++) {
          tmpdatafile = (DiffrDataFile) datafiles.elementAt(sn);
/*          double xstartmin = tmpdatafile.getXDataForPlot(tmpdatafile.startingindex, mode);
          double xendmax = tmpdatafile.getXDataForPlot(tmpdatafile.finalindex - 1, mode);
          if (xendmax < tmpdatafile.getXDataForPlot(tmpdatafile.startingindex, mode))
            xendmax = tmpdatafile.getXDataForPlot(tmpdatafile.startingindex, mode);
          if (xstartmin > tmpdatafile.getXDataForPlot(tmpdatafile.finalindex - 1, mode))
            xstartmin = tmpdatafile.getXDataForPlot(tmpdatafile.finalindex - 1, mode);
          if (xcoord[is1] >= xstartmin && xcoord[is1] <= xendmax) {
            double xvalue = xcoord[is1];
            double fitValue = tmpdatafile.getInterpolatedYForSummation(xvalue);
            intensity[is1] += fitValue;
            total++;
          }*/
	        intensity[is1] += tmpdatafile.getYData(tmpdatafile.startingindex + is1);
	        total++;
        }
        if (total > 0)
          intensity[is1] /= total;

      }
      boolean first = true;

//      boolean[] verifySameAngles = new boolean[DiffrDataFile.maxAngleNumber];
      double[] meanAngles = new double[DiffrDataFile.maxAngleNumber];
      for (int ib = 0; ib < DiffrDataFile.maxAngleNumber; ib++)
	      meanAngles[ib] = 0;
//        verifySameAngles[ib] = true;
      double[] firstTilt = null;
      int start = 0, end = 0;

      boolean energyDispersive = false;

      for (int i = 0; i < ylength; i++) {
        try {
          tmpdatafile = (DiffrDataFile) datafiles.elementAt(i);
          if (tmpdatafile.energyDispersive)
	          energyDispersive = true;
          double[] tilt = tmpdatafile.getTiltingAngle();
//          if (i == 0) firstTilt = tilt;
          for (int ib = 0; ib < DiffrDataFile.maxAngleNumber; ib++)
	          meanAngles[ib] += tilt[ib] / ylength;
//            if (tilt[ib] != firstTilt[ib])
//              verifySameAngles[ib] = false;
//				if (tmpdatafile.getComputePermission()) {
//          numberOfFilesTotal++;
          if (first) {
            first = false;
            end = tmpdatafile.datanumber; // tmpdatafile.finalindex;
            start = 0; // tmpdatafile.startingindex;
            datanumber = end - start;
//          System.out.println("n="+i+" , start="+tmpdatafile.startingindex+" , end:"+tmpdatafile.finalindex);
          }

        } catch (Exception e) {
          e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
//          out.println("File number: " + i + ", start: " + start + ", end: " + end);
//          out.println("Error summing files, may be they are uncompatible?");
        }
      }


      if (energyDispersive)
      	output.write("_pd_meas_scan_method disp");
	    output.newLine();
      output.write("_pd_meas_number_of_points " + Integer.toString(xlength));
      output.newLine();
/*      if (tmpdatafile.originalNotCalibrated) {
        output.write("_riet_meas_datafile_calibrated false");
        output.newLine();
      }*/
      output.newLine();
      for (int ib = 0; ib < DiffrDataFile.maxAngleNumber; ib++) {
//        if (verifySameAngles[ib]) {
          output.write(DiffrDataFile.diclistc[ib + 1] + ' ');
          output.write(Fmt.format(meanAngles[ib]));
          output.newLine();
//        }
      }
      output.write(DiffrDataFile.diclistc[7] + ' ');
      output.write(tmpdatafile.getString(7));
      output.newLine();

      output.newLine();
      output.write("loop_");
      output.newLine();
      output.write(tmpdatafile.getCIFXcoord());
      output.newLine();
      output.write(DiffrDataFile.intensityCalcCIFstring);
      output.newLine();
      for (int i = 0; i < xlength; i++) {
        output.write(' ' + Fmt.format(xcoord[i]) + ' ' + Fmt.format(intensity[i]));
        output.newLine();
      }
    } catch (Exception io) {
    }
    try {
      output.flush();
      output.close();
    } catch (IOException io) {
    }
  }

	public void sumDatafileOutput(Frame aframe, int selectedIndex, double angle_range) {
		int datanumber = 0;
		double[] xcoord = null;
		double[] intensity = null;
		double angleRange = Math.abs(2 * angle_range);

		String filename = Utility.openFileDialog(aframe, "Save summed datafile as (.esg extension mandatory)",
				FileDialog.SAVE, getFilePar().getDirectory(), null, "put a name (with extension).esg");
		if (filename == null)
			return;

		String[] folderAndName = Misc.getFolderandName(filename);
		String folder = folderAndName[0];
		filename = folderAndName[1];

		DiffrDataFile tmpdatafile = null;

		Vector datafiles = getSelectedDatafiles();
		int datafilenumber = datafiles.size();
		if (datafilenumber == 0) {
			(new AttentionD(new Frame(), "No selected items in the list")).setVisible(true);
			return;
		}

		Vector<DiffrDataFile> datafileOrdered = new Vector<>(datafilenumber, 10);
		for (int i = 0; i < datafilenumber; i++) {
			tmpdatafile = (DiffrDataFile) datafiles.elementAt(i);
			datafileOrdered.add(tmpdatafile);
		}
		Collections.sort(datafileOrdered, new AngleComparator(selectedIndex));

		Vector<Vector<DiffrDataFile>> datafileTable = new Vector<>();
		double startingAngle = datafileOrdered.elementAt(0).getTiltingAngle()[selectedIndex];
		double maxAngle = datafileOrdered.elementAt(datafilenumber - 1).getTiltingAngle()[selectedIndex];
		double angle = startingAngle;
		int index = 0;
		int groupIndex = 0;
		while (angle < maxAngle && index < datafilenumber) {
			double nextAngle = angle + angleRange;
			boolean nextGroup = false;
			Vector<DiffrDataFile> datalist = new Vector<>();
			while (index < datafilenumber && !nextGroup) {
				double actualAngle = datafileOrdered.elementAt(index).getTiltingAngle()[selectedIndex];
//				System.out.println(index + " " + angle + " " + nextAngle + " " + actualAngle);
				if (actualAngle >= angle && actualAngle <= nextAngle) {
					datalist.add(datafileOrdered.elementAt(index++));
				} else {
					nextGroup = true;
				}
			}
			if (datalist.size() > 0)
				datafileTable.add(datalist);
			angle = nextAngle;
		}

		BufferedWriter output = Misc.getWriter(folder, filename);
		int groupfilenumber = datafileTable.size();
		for (int ij = 0; ij < groupfilenumber; ij++) {
			Vector<DiffrDataFile> datalist = datafileTable.elementAt(ij);
			try {
				boolean first = true;
				boolean energyDispersive = false;
				boolean useCountTimeToScale = false;

				datafilenumber = datalist.size();

				double[] meanAngle = new double[DiffrDataFile.maxAngleNumber];
				double countingTime = 0;
				for (int k = 0; k < DiffrDataFile.maxAngleNumber; k++)
					meanAngle[k] = 0;
				for (int j = 0; j < datafilenumber; j++) {
					for (int k = 0; k < DiffrDataFile.maxAngleNumber; k++)
						meanAngle[k] += datalist.elementAt(j).getTiltingAngle()[k];
					if (datalist.elementAt(j).useCountTimeToScale())
						useCountTimeToScale = true;
					countingTime += datalist.elementAt(j).getCountTimeValue();
				}
				for (int k = 0; k < DiffrDataFile.maxAngleNumber; k++)
					meanAngle[k] /= datafilenumber;

				for (int i = 0; i < datafilenumber; i++) {
					tmpdatafile = datalist.elementAt(i);
					if (tmpdatafile.energyDispersive)
						energyDispersive = true;
					if (first) {
						first = false;
						datanumber = tmpdatafile.finalindex - tmpdatafile.startingindex;
						xcoord = new double[datanumber];
						intensity = new double[datanumber];
						for (int j = tmpdatafile.startingindex; j < tmpdatafile.finalindex; j++) {
							xcoord[j - tmpdatafile.startingindex] = tmpdatafile.getXDataOriginal(j);
							intensity[j - tmpdatafile.startingindex] = tmpdatafile.getYData(j);
						}
					} else {
						for (int j = tmpdatafile.startingindex; j < tmpdatafile.finalindex; j++) {
							intensity[j - tmpdatafile.startingindex] += tmpdatafile.getYData(j);
						}
					}
				}
				output.write("_pd_block_id " + tmpdatafile.toString());
				output.newLine();
				output.newLine();
				output.write("_pd_meas_number_of_points " + Integer.toString(datanumber));
				output.newLine();
				output.write("_riet_meas_datafile_calibrated false");
				output.newLine();
				output.write(DiffrDataFile.diclistc[DiffrDataFile.countingTimeValueID] + " ");
				output.write(Fmt.format(countingTime));
				output.newLine();
				if (energyDispersive) {
					output.write(DiffrDataFile.pd_meas_scan_method + " disp");
					output.newLine();
				}
				if (useCountTimeToScale) {
					output.write(DiffrDataFile.diclistc[DiffrDataFile.useCountTimeToScaleID] + " true");
					output.newLine();
				}
				output.newLine();
				for (int ib = 0; ib < DiffrDataFile.maxAngleNumber; ib++) {
						output.write(DiffrDataFile.diclistc[ib + 1] + ' ');
						output.write(Fmt.format(meanAngle[ib]));
						output.newLine();
				}
//				output.write(DiffrDataFile.diclistc[7] + ' ');
//				output.write(tmpdatafile.getString(7));
//				output.newLine();
				output.newLine();
				output.write("loop_");
				output.newLine();
				if (energyDispersive) {
					output.write(DiffrDataFile.pd_meas_counts_total);
					output.newLine();
					for (int i = 0; i < datanumber; i++) {
						output.write(Fmt.format(intensity[i]));
						output.newLine();
					}
					output.newLine();
				} else {
					output.write(tmpdatafile.getCIFXcoord());
					output.newLine();
					output.write(DiffrDataFile.intensityCalcCIFstring);
					output.newLine();
					for (int i = 0; i < datanumber; i++) {
						output.write(' ' + Fmt.format(xcoord[i]) + ' ' + Fmt.format(intensity[i]));
						output.newLine();
					}
					output.newLine();
				}
			} catch (IOException ignored) {
			}
//			datalist.removeAllElements();
//      datalist = null;
		}
		try {
			output.flush();
			output.close();
		} catch (IOException ignored) {
		}
		datafileTable.removeAllElements();
		datafileOrdered.removeAllElements();
//    datafileTable = null;
	}

	public void SumDatafileOutput(Frame aframe, boolean[] sameAngle, double[] angle) {
    int datanumber = 0;
    double[] xcoord = null;
    double[] intensity = null;
    double[] angleRange = MoreMath.abs(angle);

    String filename = Utility.openFileDialog(aframe, "Save summed datafile as (group CIF file)",
        FileDialog.SAVE, getFilePar().getDirectory(), null, "put a name (with extension).cif");
    if (filename == null)
      return;

    String[] folderAndName = Misc.getFolderandName(filename);
    String folder = folderAndName[0];
    filename = folderAndName[1];

    if (filename.endsWith(".cif"))
      filename = filename.substring(0, filename.length() - 4);

    DiffrDataFile tmpdatafile = null;

    int datafilenumber = datafilesnumber();
    if (datafilenumber == 0) {
      (new AttentionD(new Frame(), "No items in the list")).setVisible(true);
      return;
    }

    Vector<double[]> angles = new Vector<>(10, 10);
    Vector<Vector<DiffrDataFile>> datafileTable = new Vector<>(10, 10);
    int check;
//    boolean[] alreadyPickedUp = new boolean[datafilenumber];
//    for (int i = 0; i < datafilenumber; i++)
//      alreadyPickedUp[i] = false;

    for (int i = 0; i < datafilenumber; i++) {
      tmpdatafile = getDataFile(i);
      if (tmpdatafile.getComputePermission()) {
        double[] tilt = tmpdatafile.getTiltingAngle();
        check = getAnglesPosition(angles, tilt, sameAngle, angleRange);
        if (check == -1) {
          angles.addElement(tilt);
          Vector<DiffrDataFile> datalist = new Vector<>(0, 1);
          datalist.addElement(tmpdatafile);
          datafileTable.addElement(datalist);
        } else {
          Vector<DiffrDataFile> datalist = datafileTable.elementAt(check);
          datalist.addElement(tmpdatafile);
        }
      }
    }

    int groupfilenumber = datafileTable.size();

    StringBuilder groupfilename = new StringBuilder("");
    groupfilename.append(filename);
    groupfilename.append("Group.cif");
    BufferedWriter groupoutput = null;
    try {
      groupoutput = Misc.getWriter(folder, groupfilename.toString());
      groupoutput.write("loop_");
      groupoutput.newLine();
      groupoutput.write(diclistc[totsubordinate + 2]);
      groupoutput.newLine();
    } catch (IOException ignored) {
    }

    boolean[] verifySameAngles = new boolean[sameAngle.length];
    double[] meanAngles = new double[angle.length];
    for (int ij = 0; ij < groupfilenumber; ij++) {

      Vector<DiffrDataFile> datalist = datafileTable.elementAt(ij);

      StringBuilder newfilename = new StringBuilder("");
      newfilename.append(filename);

      BufferedWriter output = null;

      try {
        boolean first = true;

	      boolean energyDispersive = false;

	      datafilenumber = datalist.size();
        for (int ib = 0; ib < sameAngle.length; ib++)
          verifySameAngles[ib] = true;
        double[] firstTilt = null;
        for (int i = 0; i < datafilenumber; i++) {
          tmpdatafile = datalist.elementAt(i);
          if (tmpdatafile.energyDispersive)
          	energyDispersive = true;
          double[] tilt = tmpdatafile.getTiltingAngle();
          if (i == 0) {
            firstTilt = tilt;
            arraycopy(tilt, 0, meanAngles, 0, sameAngle.length);
          }
          for (int ib = 0; ib < sameAngle.length; ib++)
            if (!(tilt[ib] >= firstTilt[ib] - angleRange[ib] && tilt[ib] <= firstTilt[ib] + angleRange[ib]))
              verifySameAngles[ib] = false;
//					if (tmpdatafile.getComputePermission()) {
          if (first) {
	          for (int j = 0; j < sameAngle.length; j++)
            if (sameAngle[j])
              newfilename.append('_').append(Integer.toString((int) tilt[j]));
            newfilename.append(".cif");
            groupoutput.write(" '" + newfilename + '\'');
            groupoutput.newLine();
            first = false;
            output = Misc.getWriter(folder, newfilename.toString());
            datanumber = tmpdatafile.finalindex - tmpdatafile.startingindex;
            xcoord = new double[datanumber];
            intensity = new double[datanumber];
            for (int j = tmpdatafile.startingindex; j < tmpdatafile.finalindex; j++) {
              xcoord[j - tmpdatafile.startingindex] = tmpdatafile.getXDataForCalibration(j);
              intensity[j - tmpdatafile.startingindex] = tmpdatafile.getYData(j);
            }
          } else {
            for (int kj = 0; kj < sameAngle.length; kj++)
              meanAngles[kj] += tilt[kj];
            for (int j = tmpdatafile.startingindex; j < tmpdatafile.finalindex; j++) {
              intensity[j - tmpdatafile.startingindex] += tmpdatafile.getYData(j);
            }
          }
//					}

        }

        output.write("_pd_meas_number_of_points " + Integer.toString(datanumber));
        output.newLine();
        output.write("_riet_meas_datafile_calibrated false");
        output.newLine();
        if (energyDispersive) {
	        output.write("_pd_meas_scan_method disp");
	        output.newLine();
        }
	      output.newLine();
        for (int ib = 0; ib < sameAngle.length; ib++) {
          if (verifySameAngles[ib]) {
            output.write(DiffrDataFile.diclistc[ib + 1] + ' ');
            output.write(Fmt.format(meanAngles[ib] / datafilenumber));
            output.newLine();
          }
        }
        output.write(DiffrDataFile.diclistc[7] + ' ');
        output.write(tmpdatafile.getString(7));
        output.newLine();
        output.newLine();
        output.write("loop_");
        output.newLine();
        output.write(tmpdatafile.getCIFXcoord());
        output.newLine();
        output.write(DiffrDataFile.intensityCalcCIFstring);
        output.newLine();
        for (int i = 0; i < datanumber; i++) {
          output.write(' ' + Fmt.format(xcoord[i]) + ' ' + Fmt.format(intensity[i] / datafilenumber));
          output.newLine();
        }
      } catch (IOException ignored) {
      }
      try {
        output.flush();
        output.close();
      } catch (IOException ignored) {
      }
      datalist.removeAllElements();
//      datalist = null;
    }
    try {
      groupoutput.flush();
      groupoutput.close();
    } catch (IOException ignored) {
    }
    datafileTable.removeAllElements();
//    datafileTable = null;
  }

  private boolean checkAngle(double v, double v1, double v2) {
    return v >= v1 - v2 && v <= v1 + v2;
  }

  public int getAnglesPosition(Vector<double[]> angles, double[] tilt,
                               boolean[] sameAngle, double[] rangeAngles) {
    int size = angles.size();
    double[] angle;

	  for (int i = 0; i < size; i++) {
		  angle = angles.elementAt(i);
		  boolean check = true;
		  for (int j = 0; j < sameAngle.length; j++)
			  if (sameAngle[j] && !checkAngle(tilt[j], angle[j], rangeAngles[j]))
				  check = false;

		  if (check)
			  return i;
	  }
    return size - 1;
  }

  public void mergeReflectivityDatafile(Frame aframe) {
    int datanumber;
    //   double[] xcoord = null;
    //   double[] intensity = null;

    String filename = Utility.openFileDialog(aframe, "Save merge reflectivity datafile as (*.cif)",
        FileDialog.SAVE, getFilePar().getDirectory(), null, "put a name.cif");
    if (filename == null)
      return;

    String[] folderAndName = Misc.getFolderandName(filename);

    String folder = folderAndName[0];
    filename = folderAndName[1];

    if (!filename.endsWith(".cif"))
      filename += ".cif";
//    int numberOfFilesTotal = 0;
    Vector datafiles = getSelectedDatafiles();
    if (datafiles == null || datafiles.size() <= 1) {
      (new AttentionD(new Frame(), "No more than 1 item from the list selected!")).setVisible(true);
      return;
    }
    int datafilenumber = datafiles.size();
    BufferedWriter output = Misc.getWriter(folder, filename);
    try {
// check order

      DiffrDataFile datafile;
      int datafound = 0;
      double startingangle = 200.0;
      int actualdata = 0;
      for (int i = 0; i < datafilenumber; i++) {
        datafile = (DiffrDataFile) datafiles.elementAt(i);
        double angle = datafile.getXData(datafile.startingindex);
        if (angle < startingangle) {
          actualdata = i;
          startingangle = angle;
        }
      }

      Misc.swapVectorElements(datafiles, 0, actualdata);
      datafound++;

      while (datafound < datafilenumber) {
        double actualangle = 200.0;
        actualdata = datafound;
        for (int i = datafound; i < datafilenumber; i++) {
          datafile = (DiffrDataFile) datafiles.elementAt(i);
          double angle = datafile.getXData(datafile.startingindex);
          if (angle < actualangle) {
            actualdata = i;
            actualangle = angle;
          }
        }
        Misc.swapVectorElements(datafiles, datafound++, actualdata);
      }

      // finish ordering

      // check overlapping

      datafile = (DiffrDataFile) datafiles.elementAt(0);
      double angle1 = datafile.getXData(datafile.finalindex - 1);
      for (int i = 1; i < datafilenumber; i++) {
        datafile = (DiffrDataFile) datafiles.elementAt(i);
        double angle2 = datafile.getXData(datafile.startingindex);
        if (datafile.measurementstep == 0.0)
          datafile.measurementstep = Math.abs(datafile.getXData(datafile.startingindex + 1) - angle2);
        double angletocheck;
        if (i == 1)
          angletocheck = angle1 + 0.000001;
        else
          angletocheck = angle1 + datafile.measurementstep + 0.000001;
        if (angle2 > angletocheck) {
          (new AttentionD(new Frame(), "Improper alignement or overlapping of data!")).setVisible(true);
          return;
        }
        angle1 = datafile.getXData(datafile.finalindex - 1);
      }

      // merging files

      datafile = (DiffrDataFile) datafiles.elementAt(0);
      startingangle = datafile.getXData(datafile.startingindex);
      DiffrDataFile datafile2 = (DiffrDataFile) datafiles.elementAt(datafilenumber - 1);

      Vector<double[]> data = new Vector<double[]>(0, 100);

      // computing attenuator factor

      double attenuatorfactor = 1.0;
      int index2 = 0;
      for (int i = 1; i < datafilenumber; i++) {
        datafile = (DiffrDataFile) datafiles.elementAt(i - 1);
        datafile2 = (DiffrDataFile) datafiles.elementAt(i);
        double attenuatorfactor1 = 0.0;
        double attenuatorfactor2 = 0.0;
        int dataindex = datafile.startingindex;

        double startingAngle2 = datafile2.getXData(datafile2.startingindex);
        double actualAngle = datafile.getXData(dataindex);
        while (actualAngle < startingAngle2 - 0.000001) {
          double[] xdata = new double[2];
          xdata[0] = actualAngle;
          xdata[1] = datafile.getYData(dataindex) * attenuatorfactor;
          data.addElement(xdata);
          actualAngle = datafile.getXData(++dataindex);
//          wgt.addItem(datafile.getWeight(index++));
        }
        int pauseIndex = dataindex;
        index2 = datafile2.startingindex;
        while (dataindex < datafile.finalindex) {
          attenuatorfactor1 += datafile.getYData(dataindex++) * attenuatorfactor;
          attenuatorfactor2 += datafile2.getYData(index2++);
        }
        double newattenuatorfactor = attenuatorfactor1 / attenuatorfactor2;
        dataindex = pauseIndex;
        index2 = datafile2.startingindex;
        while (dataindex < datafile.finalindex) {
          double[] xdata = new double[2];
          xdata[1] = (datafile.getYData(dataindex) * attenuatorfactor
              + datafile2.getYData(index2) * newattenuatorfactor) / 2.0;
          if (datafile.getXData(dataindex) != datafile2.getXData(index2))
            out.println("Different: " + datafile.getXData(dataindex) + ' ' +
                datafile2.getXData(index2));
          xdata[0] = (datafile.getXData(dataindex++) + datafile2.getXData(index2++)) / 2.0;
          data.addElement(xdata);
        }
        attenuatorfactor = newattenuatorfactor;
      }
      while (index2 < datafile2.finalindex) {
        double[] xdata = new double[2];
        xdata[0] = datafile2.getXData(index2);
        xdata[1] = datafile2.getYData(index2) * attenuatorfactor;
        data.addElement(xdata);
        index2++;
//          wgt.addItem(datafile2.getWeight(index2++));
      }

      datanumber = data.size();
      output.write("_pd_meas_number_of_points " + Integer.toString(datanumber));
      output.newLine();
      datafile = (DiffrDataFile) datafiles.elementAt(0);
      output.write("_riet_meas_datafile_calibrated false");
      output.newLine();
      output.newLine();
      double[] tilt = datafile.getTiltingAngle();
      for (int ib = 0; ib < tilt.length; ib++) {
        output.write(DiffrDataFile.diclistc[ib + 1] + ' ');
        output.write(Fmt.format(tilt[ib]));
        output.newLine();
      }
      output.newLine();
      output.write("loop_");
      output.newLine();
      output.write(datafile.getCIFXcoord());
      output.newLine();
      output.write(DiffrDataFile.intensityCalcCIFstring);
      output.newLine();
      for (int i = 0; i < datanumber; i++) {
        double[] xdata = data.elementAt(i);
        output.write(' ' + Fmt.format(xdata[0]) + ' ' + Fmt.format(xdata[1]));
        output.newLine();
      }
    } catch (IOException ignored) {
    }
    try {
      output.flush();
      output.close();
    } catch (IOException ignored) {
    }
  }

  public void addAdditionalBackgroundToSelected() {
    DiffrDataFile[] datafiles = getSelectedDataFiles();
    if (datafiles != null)
      for (DiffrDataFile datafile : datafiles) datafile.addBackgroundParameter();
  }

  public void removeAdditionalBackgroundToSelected() {
    DiffrDataFile[] datafiles = getSelectedDataFiles();
    if (datafiles != null)
      for (DiffrDataFile datafile : datafiles) datafile.removeAllBackgroundParameters();
  }

  public void addAdditionalShiftToSelected() {
    DiffrDataFile[] datafiles = getSelectedDataFiles();
    if (datafiles != null)
      for (DiffrDataFile datafile : datafiles) datafile.addShiftParameter();
  }

  public void removeAdditionalShiftToSelected() {
    DiffrDataFile[] datafiles = getSelectedDataFiles();
    if (datafiles != null)
      for (DiffrDataFile datafile : datafiles) datafile.removeAllShiftParameters();
  }

  /**
   * Bound all parameters of selected spectra to the first selected.
   */

  public void boundSelectedSpectra() {
    DiffrDataFile[] datafiles = getSelectedDataFiles();
    if (datafiles != null)
      for (int i = 1; i < datafiles.length; i++)
        datafiles[i].setEqualTo(datafiles[0], true);
  }

  public void addAdditionalBackgroundToAll() {
    int datafilenumber = datafilesnumber();

    for (int i = 0; i < datafilenumber; i++)
      getDataFile(i).addBackgroundParameter();
  }

  public void removeDatafile(DiffrDataFile datafile) {
    int datafilenumber = datafilesnumber();

    for (int i = 0; i < datafilenumber; i++) {
      DiffrDataFile tmpdatafile = getDataFile(i);
      if (tmpdatafile == datafile) {
        removeDataFile(i);
        i = datafilenumber;
      }
    }
//		updateStringtoDoubleBuffering();
  }

  public Sample getSample() {
    return (Sample) getParent();
  }

  public double[] getAdditionalSampleAngles() {
    return sample_angles;
  }

	public double[] getDisalignementAngles() {
		return disalignement_angles;
	}

	public double[] getDisplacementErrors() {
		return displacement_errors;
	}

	int actualIndex;

  public int getDataFileSetIndex() {
    return actualIndex;
  }

  public void setDataFileSetIndex(int value) {
    actualIndex = value;
  }

  public String getMinRange() {
    return getString(minRangeID);
  }

  public void setMinRange(String value) {
    setString(minRangeID, value);
  }

  public String getMaxRange() {
    return getString(maxRangeID);
  }

  public void setMaxRange(String value) {
    setString(maxRangeID, value);
  }

  public double getMinRangeD() {
    return Double.valueOf(getMinRange());
  }

  public double getMaxRangeD() {
    return Double.valueOf(getMaxRange());
  }

  public void setLorentzRestricted(String avalue) {
    setString(lorentzRestrictedID, avalue);
  }

  public void setLorentzRestricted(boolean avalue) {
    if (avalue)
      setLorentzRestricted("true");
    else
      setLorentzRestricted("false");
  }

  public String getLorentzRestricted() {
    return getString(lorentzRestrictedID);
  }

  public boolean isLorentzRestricted() {
    return getLorentzRestricted().equals("true");
  }

  public void setEnabled(String avalue) {
    setString(enabledID, avalue);
  }

  public void setEnabled(boolean avalue) {
    if (avalue)
      setEnabled("true");
    else
      setEnabled("false");
    enabled = isEnabled();
    Sample asample = (Sample) getParent();
    asample.checkActiveDataSets(); // todo only temporarly, fix in the computation
    asample.refreshDataIndices();

//    System.out.println(toXRDcatString() + " enabled");
  }

  public String getEnabled() {
    return getString(enabledID);
  }

  public boolean isEnabled() {
    return getEnabled().equals("true");
  }

  public boolean enabled() {
    return enabled;
  }

  public String getRandomTexture() {
    return getString(randomTextureID);
  }

  public boolean hasRandomTexture() {
    return getRandomTexture().equals("true");
  }

  public void setRandomTexture(String avalue) {
    setString(randomTextureID, avalue);
  }

  public void setRandomTexture(boolean avalue) {
    if (avalue)
      setRandomTexture("true");
    else
      setRandomTexture("false");
    randomTexture = hasRandomTexture();
    Sample asample = (Sample) getParent();
    resetForRandomTexture();

//    System.out.println(toXRDcatString() + " enabled");
  }

	private void resetForRandomTexture() {
		int phasesnumber = getSample().phasesNumber();
		for (int j = 0; j < phasesnumber; j++) {
			Phase phase = getSample().getPhase(j);
			for (int i = 0; i < activedatafilesnumber(); i++)
				getActiveDataFile(i).resetForRandomTexture(phase);
		}
	}

	public String getNoStrain() {
		return getString(noStrainID);
	}

	public boolean hasNoStrain() {
		return getNoStrain().equals("true");
	}

	public void setNoStrain(String avalue) {
		setString(noStrainID, avalue);
	}

	public void setNoStrain(boolean avalue) {
		if (avalue)
			setNoStrain("true");
		else
			setNoStrain("false");
		noStrain = hasNoStrain();
		Sample asample = (Sample) getParent();
		resetForNoStrain();

//    System.out.println(toXRDcatString() + " enabled");
	}

	private void resetForNoStrain() {
		int phasesnumber = getSample().phasesNumber();
		for (int j = 0; j < phasesnumber; j++) {
			Phase phase = getSample().getPhase(j);
			for (int i = 0; i < activedatafilesnumber(); i++)
				getActiveDataFile(i).resetForNoStrain(phase);
		}
	}

	public void setReplaceDatafile(boolean b) {
    if (b)
      setReplaceDatafile("true");
    else
      setReplaceDatafile("false");
    replaceDatafile = mustReplace();
  }

  public void setReplaceDatafile(String avalue) {
    setString(replaceID, avalue);
  }

  public boolean replaceDatafile() {
    return replaceDatafile;
  }

  public String getReplaceDatafile() {
    return getString(replaceID);
  }

  public boolean mustReplace() {
    return getReplaceDatafile().equals("true");
  }

  public void setAutomaticPolynomialBackground(boolean b) {
    if (b)
      setAutomaticPolynomialBackground("true");
    else
      setAutomaticPolynomialBackground("false");
    automaticPolynomialBackground = isAutomaticPolynomialBackground();
  }

  public void setAutomaticPolynomialBackground(String avalue) {
    setString(automaticPolynomialBackgroundID, avalue);
  }

  public boolean automaticPolynomialBackground() {
    return automaticPolynomialBackground;
  }

  public String getAutomaticPolynomialBackground() {
    return getString(automaticPolynomialBackgroundID);
  }

  public boolean isAutomaticPolynomialBackground() {
    return getAutomaticPolynomialBackground().equals("true");
  }

  public void setBackgroundInterpolated(String avalue) {
    setString(backgroundInterpolatedID, avalue);
  }

  public void setBackgroundInterpolated(boolean avalue) {
    if (avalue)
      setBackgroundInterpolated("true");
    else
      setBackgroundInterpolated("false");
  }

  public String getBackgroundInterpolated() {
    return getString(backgroundInterpolatedID);
  }

  public boolean isBackgroundInterpolated() {
    return getBackgroundInterpolated().equals("true");
  }

  public void setInterpolatedPoints(int avalue) {
    setInterpolatedPoints(Integer.toString(avalue));
  }

  public void setInterpolatedPoints(String avalue) {
    setString(interpolatedPointsID, avalue);
//    interpolatedPoints = Integer.valueOf(getInterpolatedPoints());
  }

  public String getInterpolatedPoints() {
    return getString(interpolatedPointsID);
  }

  public int getInterpolatedPointsValue() {
    return interpolatedPoints;
  }

	public void setBackgroundInterpolationIterations(int integer) {
		setBackgroundInterpolationIterations(Integer.toString(integer));
	}

	public void setBackgroundInterpolationIterations(String integer) {
		setString(interpolationIterationsID, integer);
	}

	public int getInterpolationIterationsValue() {
		return interpolationIterations;
	}

	public String getInterpolationIterations() {
		return getString(interpolationIterationsID);
	}

	public void removeManualInterpolation(boolean forceRemove) {
    for (int i = 0; i < datafilesnumber(); i++) {
	    if (forceRemove || !getDataFile(i).getManualBkgInterpolation())
        getDataFile(i).removeManualInterpolation();
    }
  }

	public void checkManualInterpolation() {
    for (int i = 0; i < datafilesnumber(); i++) {
      this.getDataFile(i).checkManualInterpolation();
    }
  }

  public int datafilesnumber() {
    return numberofelementSubL(2);
  }

  public int activedatafilesnumber() {
    return activedatanumber;
  }

  public static final int backgroundID = 0;

  public int numbercoefbackg() {
    return numberofelementPL(backgroundID);
  }

  public void addBackgroudCoeff() {
    int numb = numbercoefbackg();
    String parameterString = getParameterString(backgroundID, numb);
    double minmax = -10000.0 / (numb + 1) / (numb + 1) / (numb + 1);
    addparameterloopField(backgroundID, new Parameter(this, parameterString, 0,
        ParameterPreferences.getDouble(parameterString + ".min", -minmax),
        ParameterPreferences.getDouble(parameterString + ".max", minmax)));
  }

  public Parameter getbackgcoef(int index) {
    return (Parameter) parameterloopField[backgroundID].elementAt(index);
  }

  public static final int chiBackgroundID = 1;

  public int numbercoefbackgChi() {
    return numberofelementPL(chiBackgroundID);
  }

  public Parameter getbackgcoefChi(int index) {
    return (Parameter) parameterloopField[chiBackgroundID].elementAt(index);
  }

  public static final int etaBackgroundID = 2;

  public int numbercoefbackgEta() {
    return numberofelementPL(etaBackgroundID);
  }

  public Parameter getbackgcoefEta(int index) {
    return (Parameter) parameterloopField[etaBackgroundID].elementAt(index);
  }

  public int backgpeaksnumber() {
    return numberofelementSubL(1);
  }

  public ListVector getBkgPeakList() {
    return subordinateloopField[1];
  }

  public void addBkgPeak(BkgPeak peak) {
    getBkgPeakList().addItem(peak);
  }

  public BkgPeak getBkgPeak(int index) {
    return (BkgPeak) subordinateloopField[1].elementAt(index);
  }

  public int excludedRegionsNumber() {
    return numberofelementSubL(0);
  }

  public Region getExcludedRegion(int index) {
    return (Region) subordinateloopField[0].elementAt(index);
  }

  public void setPeakFunction(String value) {
    setString(4, value);
  }

  public String getPeakFunction() {
    return getString(4);
  }

  public void setPeakCutoff(String value) {
    setString(5, value);
  }

  public String getPeakCutoff() {
    return getString(5);
  }

  public double getPeakCutoffD() {
    return Double.valueOf(getPeakCutoff());
  }

  public boolean hasSimilarRadiationTo(DataFileSet adataset) {
    RadiationType radtypeThis = getInstrument().getRadiationType();
    RadiationType radtypeOther = adataset.getInstrument().getRadiationType();
    return radtypeThis.isSimilarTo(radtypeOther);
  }

  int numberOfData;

  public int computeDataNumber() {
    numberOfData = 0;

    int datafilenumber = activedatafilesnumber();

    for (int i = 0; i < datafilenumber; i++) {
      DiffrDataFile adfile = getActiveDataFile(i);
      numberOfData += adfile.computeDataNumber();
    }

    return numberOfData;
  }

  public int getNumberOfData() {
    return numberOfData;
  }

  @Override
  public boolean isActive(XRDcat obj) {
    return isEnabled() && super.isActive(obj);
  }

  public double[] getData() {
    int dnumber = 0;
    double dta[] = new double[getNumberOfData()];

    int datafilenumber = activedatafilesnumber();

    for (int i = 0; i < datafilenumber; i++) {
      DiffrDataFile adfile = getActiveDataFile(i);
      int apnumber = adfile.getNumberOfData();
      double adta[] = adfile.getDataForStatistic();
      arraycopy(adta, 0, dta, dnumber, apnumber);
      dnumber += apnumber;
    }
    return dta;
  }

  public double[] getWeight() {
    int dnumber = 0;
    double dta[] = new double[getNumberOfData()];

    int datafilenumber = activedatafilesnumber();
    double sumDataWeighted = 1.0;
    FilePar afilepar = getFilePar();

    for (int i = 0; i < datafilenumber; i++) {
      DiffrDataFile adfile = getActiveDataFile(i);
      switch (afilepar.getMinimizeQuantitySwitch()) {
        case 0:
          sumDataWeighted = 1.0;
          break;
        case 1:
          sumDataWeighted = Math.sqrt(adfile.getDataWeightSum());
          break;
      }
      int apnumber = adfile.getNumberOfData();
      double adta[] = adfile.getWeight();
      for (int j = 0; j < apnumber; j++)
        dta[dnumber + j] = adta[j] / sumDataWeighted;
      dnumber += apnumber;
    }
    return dta;
  }

  public double[] getFit() {
    int dnumber = 0;
    double dta[] = new double[getNumberOfData()];

    int datafilenumber = activedatafilesnumber();

    for (int i = 0; i < datafilenumber; i++) {
      DiffrDataFile adfile = getActiveDataFile(i);
      int apnumber = adfile.getNumberOfData();
      double adta[] = adfile.getFitForStatistic();
      arraycopy(adta, 0, dta, dnumber, apnumber);
      dnumber += apnumber;
    }
    return dta;
  }

  public DiffrDataFile[] getActiveDataFiles() {
    int datafilenumber = activedatafilesnumber();
    if (datafilenumber == 0)
      return null;
//    System.out.println(toXRDcatString() + " " + datafilenumber);
    DiffrDataFile[] adfile = new DiffrDataFile[datafilenumber];
    for (int i = 0; i < datafilenumber; i++) {
      adfile[i] = getActiveDataFile(i);
    }
    return adfile;
  }

  public void multiPlot(Frame aframe) {
    int datafilenumber = activedatafilesnumber();

    final Frame newFrame = aframe;
    final DiffrDataFile[] adfile = new DiffrDataFile[datafilenumber];
    for (int i = 0; i < datafilenumber; i++) {
      adfile[i] = getActiveDataFile(i);
    }
    final String label = DataFileSet.this.toXRDcatString();
    (new PersistentThread() {
  @Override
      public void executeJob() {
        if (adfile.length > 1)
          (new MultiPlotFitting(newFrame, adfile, label)).setVisible(true);
        else if (adfile.length > 0)
          (new PlotFitting(newFrame, adfile)).setVisible(true);
        else
          (new PlotFitting(newFrame, null)).setVisible(true);
      }
    }).start();
  }

  public void multiPlot2D(Frame aframe) {
    int datafilenumber = activedatafilesnumber();

    final Frame newFrame = aframe;
    final DiffrDataFile[] adfile = new DiffrDataFile[datafilenumber];
    for (int i = 0; i < datafilenumber; i++) {
      adfile[i] = getActiveDataFile(i);
    }
    final String label = DataFileSet.this.toXRDcatString();
    (new PersistentThread() {
  @Override
      public void executeJob() {
        new MultiPlotFitting2D(newFrame, adfile, label);
      }
    }).start();
  }

	public void plot2DandExportPng(String plotOutput2DFileName) {
		int datafilenumber = activedatafilesnumber();

		final DiffrDataFile[] adfile = new DiffrDataFile[datafilenumber];
		for (int i = 0; i < datafilenumber; i++) {
			adfile[i] = getActiveDataFile(i);
		}
		final String label = DataFileSet.this.toXRDcatString();
		MultiPlotFitting2D plot = new MultiPlotFitting2D(null, adfile, label);
		(new PersistentThread() {
			@Override
			public void executeJob() {

				try {
					TimeUnit.MILLISECONDS.sleep(3000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}

				Component comp = plot.fullGraphPanel;
				if (comp != null) {
					Rectangle rect = comp.getBounds();
					Image fileImage =
							plot.createImage(rect.width, rect.height);
					Graphics g = fileImage.getGraphics();

					//write to the image
					g.clearRect(0, 0, comp.getWidth(), comp.getHeight());
					comp.paint(g);
					// write it out in the format you want
					BeartexPFPlot.savePic(fileImage, "png", plotOutput2DFileName + label + ".png", comp);

					try {
						TimeUnit.MILLISECONDS.sleep(3000);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}

					//dispose of the graphics content
					g.dispose();
					plot.setVisible(false);
					plot.dispose();
				}
			}
		}).start();
	}

	public void plotAndExportPng(String plotOutputFileName) {
		int datafilenumber = activedatafilesnumber();

		final DiffrDataFile[] adfile = new DiffrDataFile[datafilenumber];
		for (int i = 0; i < datafilenumber; i++) {
			adfile[i] = getActiveDataFile(i);
		}
		final String label = DataFileSet.this.toXRDcatString();
		PlotFitting plot = new PlotFitting(null, adfile, false);
		plot.setVisible(true);
		(new PersistentThread() {
			@Override
			public void executeJob() {

				try {
					TimeUnit.MILLISECONDS.sleep(3000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}

				Component comp = plot.thePlotPanel.getComponentToPrint();
				if (comp != null) {
					Rectangle rect = comp.getBounds();
					Image fileImage =
							plot.createImage(rect.width, rect.height);
					Graphics g = fileImage.getGraphics();

					//write to the image
					g.clearRect(0, 0, comp.getWidth(), comp.getHeight());
					comp.paint(g);
					// write it out in the format you want
					BeartexPFPlot.savePic(fileImage, "png", plotOutputFileName + label + ".png", comp);

					try {
						TimeUnit.MILLISECONDS.sleep(3000);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}

					//dispose of the graphics content
					g.dispose();
					plot.setVisible(false);
					plot.dispose();
				}
			}
		}).start();
	}

	public void polarPlot2D(Frame aframe) {
    final Frame newFrame = aframe;

    (new PersistentThread() {
  @Override
      public void executeJob() {
        int zoom = MaudPreferences.getInteger("polarPlotData2D.zoom", 4);
        int gridPoints = MaudPreferences.getInteger("polarPlotData2D.gridPoints", 201);
        new PolarPlotData2D(newFrame, DataFileSet.this, gridPoints, zoom, 0, false, Constants.PI / 2, false, 512);
      }
    }).start();
  }

  public double[][] getPolarPlotData() {
    int numberExpPoints = 0;
    int datafilenumber = activedatafilesnumber();
    final DiffrDataFile[] adfile = new DiffrDataFile[datafilenumber];
    for (int i = 0; i < datafilenumber; i++) {
      adfile[i] = getActiveDataFile(i);
      numberExpPoints += adfile[i].getNumberOfData();
    }

    boolean experimental = MaudPreferences.getBoolean("polarPlotData2D.experimental", true);

    if(numberExpPoints <= 0)
      return null;

    double[][] expTFAndAngles = new double[3][numberExpPoints];
    int index = 0;
    for (int i = 0; i < datafilenumber; i++) {
      double[][] texture_angles = adfile[i].getTextureAnglesAndIntensityNoBkg(experimental);
      for (int j = 0; j < adfile[i].getNumberOfData(); j++) {
        expTFAndAngles[0][index] = texture_angles[0][j];
        expTFAndAngles[1][index] = texture_angles[1][j];
        expTFAndAngles[2][index++] = texture_angles[2][j];
      }
      double[] textAngl = adfile[i].getTiltingAngle();
      int j = index - adfile[i].getNumberOfData();
/*      System.out.println(i + " " + textAngl[0] + " " + textAngl[1] + " " + textAngl[2] + " " + textAngl[3]);
      System.out.println(i + " " + expTFAndAngles[0][j] + " " + expTFAndAngles[0][index - 1]
          + " " + expTFAndAngles[1][j] + " " + expTFAndAngles[1][index - 1]
          + " " + expTFAndAngles[2][j] + " " + expTFAndAngles[2][index - 1]);*/
    }

    return expTFAndAngles;
  }

  public void differencePlot2D(Frame aframe) {
    int datafilenumber = activedatafilesnumber();

    final Frame newFrame = aframe;
    final DiffrDataFile[] adfile = new DiffrDataFile[datafilenumber];
    for (int i = 0; i < datafilenumber; i++) {
      adfile[i] = getActiveDataFile(i);
    }
    final String label = DataFileSet.this.toXRDcatString();
    (new PersistentThread() {
  @Override
      public void executeJob() {
        new DifferencePlot2D(newFrame, adfile, label);
      }
    }).start();
  }

	public void sectionPlot2D(Frame aframe) {
		final Frame newFrame = aframe;

		int datafilenumber = activedatafilesnumber();
		final DiffrDataFile[] adfile = new DiffrDataFile[datafilenumber];
		for (int i = 0; i < datafilenumber; i++) {
			adfile[i] = getActiveDataFile(i);
		}
		final String label = DataFileSet.this.toXRDcatString();
		(new PersistentThread() {
			@Override
			public void executeJob() {
				new SectionPlotData2D(newFrame, adfile, label).setVisible(true);
			}
		}).start();
	}

	//  DiffrDataFile adatafile[];
  int minindex[];
  int maxindex[];

  @Override
  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
	  if (filepar != null && !filepar.isLoadingFile() && isAbilitatetoRefresh) {
      for (int i = 0; i < sampleOmegaID; i++)
        if (source == parameterField[i]) {
          notifyParameterChanged(source, Constants.THERMAL_SHIFT_CHANGED);
          return;
        }
      for (int i = sampleOmegaID; i < sampleOmegaID + 3; i++)
        if (source == parameterField[i]) {
          notifyParameterChanged(source, Constants.SAMPLE_ORIENTATION_CHANGED);
          notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED);
          return;
        }
		  for (int i = displacementxID; i < displacementxID + 3; i++)
			  if (source == parameterField[i]) {
				  notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED);
				  return;
			  }
	    for (int i = disalignementOmegaID; i < disalignementOmegaID + 4; i++)
		    if (source == parameterField[i]) {
			    notifyParameterChanged(source, Constants.TEXTURE_CHANGED);
			    return;
		    }
      for (int i = 0; i < Nparameterloop; i++) {
        for (int j = 0; j < numberofelementPL(i); j++) {
          Parameter apar = (Parameter) parameterloopField[i].elementAt(j);
          if (apar == source) {
            notifyParameterChanged(source, Constants.BKG_PARAMETER_CHANGED);
            return;
          }
        }
      }
    }
    super.notifyParameterChanged(source);
  }

  // todo: check notification up and down after moving instrument

  @Override
  public void refreshForNotificationUp(XRDcat source, int reason) {
    refreshComputation = true;
//    if (source instanceof DiffrDataFile)
//      System.out.println("Notification received from " + source + " , reason " + reason);
//    boolean isFromInstrument = source instanceof Instrument;
    if (reason == Constants.ANGULAR_CALIBRATION) {
      refreshComputation = true;
      int datafilenumber = datafilesnumber();
      for (int i = 0; i < datafilenumber; i++) {
        getDataFile(i).setCalibrated(false);
        getDataFile(i).refreshBkgComputation = true;
        getDataFile(i).refreshSpectraComputation = true;
      }
      reason = 0;
    } else if (reason == Constants.INTENSITY_CALIBRATION) {
      refreshComputation = true;
      int datafilenumber = datafilesnumber();
      for (int i = 0; i < datafilenumber; i++) {
        getDataFile(i).setIntensityUncalibrated();
        getDataFile(i).refreshBkgComputation = true;
        getDataFile(i).refreshSpectraComputation = true;
      }
      reason = 0;
    } else if (reason == Constants.BKG_FILE_CHANGED) {
      int datafilenumber = datafilesnumber();
      for (int i = 0; i < datafilenumber; i++) {
        getDataFile(i).refreshBkgComputation = true;
      }
      reason = 0;
    } else if (reason == Constants.TEXTURE_CHANGED) {
	    refreshComputation = true;
	    int datafilenumber = datafilesnumber();
	    for (int i = 0; i < datafilenumber; i++) {
		    getDataFile(i).refreshBkgComputation = true;
		    getDataFile(i).refreshSpectraComputation = true;
	    }
    }
    if (source == this || source instanceof BkgPeak) {
      int datafilenumber = datafilesnumber();
//      System.out.println("Setting all bckg changed");
      if (reason == Constants.BKG_PARAMETER_CHANGED)
	      for (int i = 0; i < datafilenumber; i++) {
		      getDataFile(i).refreshBkgComputation = true;
	         getDataFile(i).refreshSpectraComputation = true;
         }
    } else {
      XRDcat sourceDatafile = source;
      while (sourceDatafile != null && (!(sourceDatafile instanceof FilePar) &&
          !(sourceDatafile instanceof DiffrDataFile)))
        sourceDatafile = sourceDatafile.getParent();
      if (sourceDatafile instanceof DiffrDataFile) {
        int datafilenumber = datafilesnumber();
        for (int i = 0; i < datafilenumber; i++) {
          if (getDataFile(i) == sourceDatafile) {
            if (reason == Constants.BKG_PARAMETER_CHANGED) {
              getDataFile(i).refreshBkgComputation = true;
              getDataFile(i).refreshSpectraComputation = true;
//            System.out.println("refreshing bkg "+sourceDatafile.toXRDcatString());
            } else {
//              System.out.println("Setting parameter changed, " + sourceDatafile);
              if (isBackgroundInterpolated())
                getDataFile(i).refreshBkgComputation = true;
            }
          }
        }
      } else {
        int datafilenumber = datafilesnumber();
        for (int i = 0; i < datafilenumber; i++) {
          if (isBackgroundInterpolated())
            getDataFile(i).refreshBkgComputation = true;
          getDataFile(i).refreshSpectraComputation = true;
        }
      }
    }
  }

  @Override
  public void refreshForNotificationDown(XRDcat source, int reason) {
    boolean isFromDataset = source instanceof DataFileSet;
    XRDcat sourceparent = source;
    if (!isFromDataset) {
      while (sourceparent != null && !(sourceparent instanceof FilePar)) {
        sourceparent = sourceparent.getParent();
        if (sourceparent instanceof DataFileSet) {
          isFromDataset = true;
          break;
        }
      }
    }
    if (isFromDataset)
      return;

    refreshComputation = true;  // we need to compute all probably
    int datafilenumber = datafilesnumber();
    for (int i = 0; i < datafilenumber; i++) {
      if (isBackgroundInterpolated())
        getDataFile(i).refreshBkgComputation = true;
      getDataFile(i).refreshSpectraComputation = true;
    }
  }

  @Override
  public void setRefreshAllStatus() {
    super.setRefreshAllStatus();
  }

  int numberofpeak;
  BkgPeak[] abkgPeak;

  @Override
  public void update(boolean firstLoading) {

    super.update(firstLoading);

    activedatanumber = 0;
    int datafilenumber = datafilesnumber();
//    setRadiation();

    for (int i = 0; i < datafilenumber; i++)
      if (getDataFile(i).getComputePermission())
        activedatanumber++;

//    System.out.println("Refresh active datafiles!");
    datafileindex = new int[activedatanumber];
    activedatanumber = 0;
    for (int i = 0; i < datafilenumber; i++)
      if (getDataFile(i).getComputePermission()) {
        datafileindex[activedatanumber] = i;
        activedatanumber++;
      }

    numberofpeak = backgpeaksnumber();
    abkgPeak = new BkgPeak[numberofpeak];
    for (int k = 0; k < numberofpeak; k++) {
      abkgPeak[k] = getBkgPeak(k);
//      abkgPeak[k].preparecomputing();
    }

  }

  public double[] getTotalIntensityForActiveSpectra() {
    update(false);

    double[] total = new double[activedatanumber];
    for (int i = 0; i < activedatanumber; i++)
      total[i] = getActiveDataFile(i).getTotalIntensity();
    return total;
  }

	public double[] get2ThetaForActiveSpectra() {
		update(false);

		double[] total = new double[activedatanumber];
		for (int i = 0; i < activedatanumber; i++)
			total[i] = getActiveDataFile(i).getAngleValue(4) + getActiveDataFile(i).getAngleValue(0);
		return total;
	}

	public void forceRangeCut() {

    double drange0 = getMinRangeD();
    double drange1 = getMaxRangeD();

    int datafilenumber = datafilesnumber();

    for (int i = 0; i < datafilenumber; i++) {
      DiffrDataFile adatafile1 = getDataFile(i);
      adatafile1.checkIncreasingX();
      int minindex1 = adatafile1.getMinIndexforValue(drange0, drange1);
      int maxindex1 = adatafile1.getMaxIndexforValue(drange0, drange1);
	    if ((maxindex1 - minindex1) < 4)
		    adatafile1.setCompute(false);
	    else
        adatafile1.setMinMaxIndices(minindex1, maxindex1);
    }

  }

  double drange_final[] = new double[2];

  public DiffrDataFile getBackgroundDataFile(DiffrDataFile requestingDataFile) {
    int banknumber = requestingDataFile.getBankNumber();
    int alldatafilenumber = datafilesnumber();
    for (int i = 0; i < alldatafilenumber; i++) {
      DiffrDataFile bdatafile = getDataFile(i);
      if (bdatafile.getAsBackgroundPermission() && bdatafile.getBankNumber() == banknumber)
        return bdatafile;
    }
    return null;
  }

  public boolean isBackgroundExperimental() {
    return isBackgroundExperimental;
  }

  public void setRefreshComputation(boolean value) {
    refreshComputation = false;
    int datafilenumber = activedatafilesnumber();
    for (int i = 0; i < datafilenumber; i++)
      getActiveDataFile(i).refreshSpectraComputation = false;
  }

  public void finalOutput(OutputStream out) throws IOException {
    double[] indexes = getRefinementIndexes();
    printLine(out, "DataSet " + toXRDcatString() + " :");
    printLine(out, "DataSet Rwp: " + Fmt.format(indexes[0]));
    printLine(out, "DataSet Rp: " + Fmt.format(indexes[4]));
    printLine(out, "DataSet Rwpnb (no background): " + Fmt.format(indexes[1]));
	  printLine(out, "DataSet Rwpnb1 (no bkg rescaled): " + Fmt.format(indexes[2]));
	  printLine(out, "DataSet Rwpnb2 (no bkg rescaled^2): " + Fmt.format(indexes[3]));
    printLine(out, "DataSet Rpnb (no background): " + Fmt.format(indexes[5]));
	  printLine(out, "DataSet Rpnb1 (no bkg rescaled): " + Fmt.format(indexes[6]));
	  printLine(out, "DataSet Rpnb2 (no bkg rescaled^2): " + Fmt.format(indexes[7]));
    newLine(out);
    printLine(out, "Refinement final output indices for single spectra:");
    int datafilenumber = activedatafilesnumber();
    for (int i = 0; i < datafilenumber; i++)
      getActiveDataFile(i).finalOutput(out);
    out.flush();
  }

  boolean indexesComputed = false;
  double[] refinementIndexes = new double[18];

  public double[] getRefinementIndexes() { // todo: add WSS

    if (!indexesComputed) {

      for (int j = 0; j < 18; j++)
        refinementIndexes[j] = 0.0;

      int datafilenumber = activedatafilesnumber();
      for (int i = 0; i < datafilenumber; i++) {

        double[] refIndex = getActiveDataFile(i).getRefinementIndexes();
        for (int j = 8; j < 18; j++)
          refinementIndexes[j] += refIndex[j];
      }
      refinementIndexes[0] = MoreMath.sqrt(refinementIndexes[8] / refinementIndexes[14]);
      refinementIndexes[1] = MoreMath.sqrt(refinementIndexes[8] / refinementIndexes[15]);
	    refinementIndexes[2] = MoreMath.sqrt(refinementIndexes[10] / refinementIndexes[15]);
	    refinementIndexes[3] = MoreMath.sqrt(refinementIndexes[11] / refinementIndexes[15]);
      refinementIndexes[4] = refinementIndexes[9] / refinementIndexes[16];
      refinementIndexes[5] = refinementIndexes[9] / refinementIndexes[17];
	    refinementIndexes[6] = refinementIndexes[12] / refinementIndexes[17];
	    refinementIndexes[7] = refinementIndexes[13] / refinementIndexes[17];

      indexesComputed = true;
    }
    return refinementIndexes;

  }

  public void computeRange() {

    // preparing computation

    int datafilenumber = activedatafilesnumber();
//    System.out.println(datafilenumber);
//    adatafile = new DiffrDataFile[datafilenumber];
//    System.out.println(toXRDcatString() + " number of datafiles " + datafilenumber);
    minindex = new int[datafilenumber];
    maxindex = new int[datafilenumber];
    double[] drange = new double[2];
    drange[0] = getMinRangeD();
    drange[1] = getMaxRangeD();

//    System.out.println(drange[0] + ", " + drange[1]);
    omogeneousDataset = true;
    drange_final[1] = -1.0E+10;
    drange_final[0] = 1.0E+10;

    for (int i = 0; i < datafilenumber; i++) {
//      adatafile[i] = getActiveDataFile(i);
      DiffrDataFile bdatafile = getActiveDataFile(i);
      bdatafile.calibrateX();
      bdatafile.checkIncreasingX();
      minindex[i] = bdatafile.getMinIndexforValue(drange[0], drange[1]);
      maxindex[i] = bdatafile.getMaxIndexforValue(drange[0], drange[1]);
//      System.out.println("Datafile #" + i + ": " + bdatafile + " , minmax " + minindex[i] + " " + maxindex[i]);
      bdatafile.setMinMaxIndices(minindex[i], maxindex[i]);
      if (i > 0) {
        if (minindex[0] != minindex[i])
          omogeneousDataset = false;
        if (maxindex[0] != maxindex[i])
          omogeneousDataset = false;
      }
      double[] arange = bdatafile.getRange(minindex[i], maxindex[i]);
      if (drange_final[0] > arange[0])
        drange_final[0] = arange[0];
      if (drange_final[1] < arange[1])
        drange_final[1] = arange[1];

    }
 //   if (drange_final[0] < 0)
 //     hasNegative2theta = true;

    getSample().setRange(drange_final);

    // preparing background datafiles if any

    for (int i = 0; i < datafilenumber; i++) {
//      adatafile[i] = getActiveDataFile(i);
      DiffrDataFile bdatafile = getActiveDataFile(i);
      if (bdatafile.getAsBackgroundPermission()) {
        bdatafile.calibrateX();
//        bdatafile.checkIncreasingX();
        double drangeMin = drange_final[0];
        double drangeMax = drange_final[1];
        if (bdatafile.dspacingbase) {
          double newdrangeMin = drangeMin / (1.0 + getParameterValue(bkgExpThermalShift) * 1.0E-4) -
              getParameterValue(bkgExpShift);
          if (newdrangeMin < drangeMin)
            drangeMin = newdrangeMin;
          double newdrangeMax = drangeMax / (1.0 + getParameterValue(bkgExpThermalShift) * 1.0E-4) -
              getParameterValue(bkgExpShift);
          if (newdrangeMax > drangeMax)
            drangeMax = newdrangeMax;
        } else if (bdatafile.energyDispersive) {
          double newdrangeMin = drangeMin * (1.0 + getParameterValue(bkgExpThermalShift) * 1.0E-4) -
              getParameterValue(bkgExpShift);
          if (newdrangeMin < drangeMin)
            drangeMin = newdrangeMin;
          double newdrangeMax = drangeMax * (1.0 + getParameterValue(bkgExpThermalShift) * 1.0E-4) -
              getParameterValue(bkgExpShift);
          if (newdrangeMax > drangeMax)
            drangeMax = newdrangeMax;
        } else {
          double newdrangeMin = drangeMin / (1.0 + getParameterValue(bkgExpThermalShift) * 1.0E-4) -
              getParameterValue(bkgExpShift);
          if (newdrangeMin < drangeMin)
            drangeMin = newdrangeMin;
          double newdrangeMax = drangeMax / (1.0 + getParameterValue(bkgExpThermalShift) * 1.0E-4) -
              getParameterValue(bkgExpShift);
          if (newdrangeMax > drangeMax)
            drangeMax = newdrangeMax;
        }
        int minindex1 = bdatafile.getMinIndexforValue(drangeMin, drangeMax);
        int maxindex1 = bdatafile.getMaxIndexforValue(drangeMin, drangeMax);
        bdatafile.setMinMaxIndices(minindex1, maxindex1);
        bdatafile.computeSmoothSpectrum(getParameterValue(bkgExpShift),
            getParameterValue(bkgExpThermalShift), bdatafile);
      }
    }

  }

  public int getNumberOfBackgroundFiles() {
    int index = 0;
    for (int i = 0; i < datafilesnumber(); i++) {
      DiffrDataFile tmpDatafile = getDataFile(i);
      if (tmpDatafile.getAsBackgroundPermission()) {
        index++;
      }
    }
    return index;
  }

	public void refreshAll(boolean firstLoading) {
		FilePar filepar = getFilePar();
		if (filepar != null && !filepar.isLoadingFile() && isAbilitatetoRefresh) {
//      System.out.println("Refreshing " + this.toXRDcatString());
			refreshComputation = true;
			setRefreshAllStatus();
			update(firstLoading);
			Object[] childrens = getObjectChildren();
			int numberOfChildrens = childrens.length;
			for (int i = 0; i < numberOfChildrens; i++) {
//				System.out.println(this.toXRDcatString() + ": " + childrens[i].toXRDcatString());
				((XRDcat) childrens[i]).refreshAll(firstLoading);
			}
		}
	}

	public void computeBackground() {

    // preparing computation

    int datafilenumber = activedatafilesnumber();

    // compute background

//		System.out.println("Compute background");

    for (int i = 0; i < datafilenumber; i++) {
      DiffrDataFile tmpDatafile = getActiveDataFile(i);
      if (tmpDatafile.refreshBkgComputation) {
//        System.out.println("Refreshing backg: " + tmpDatafile.toXRDcatString());
//        System.out.println("Min, max: " + minindex[i] + " " + maxindex[i]);
//        System.out.println("Max: " + tmpDatafile.twothetacalibrated.length);
//	      System.out.println("Start-end: " + tmpDatafile.startingindex + " " + tmpDatafile.finalindex);
        tmpDatafile.resetBkg();
//        tmpDatafile.computeBackground(minindex[i], maxindex[i]);
	      tmpDatafile.computeBackground(tmpDatafile.startingindex, tmpDatafile.finalindex);

        double tbackgroundChi = 0.0;
        double[] chieta = tmpDatafile.getTiltingAngle();
        if (nchibckgpar > 0) {
          for (int k = 0; k < nchibckgpar; k++)
            tbackgroundChi += backgroundChi[k] * MoreMath.pow(chieta[1], k + 1);
        }
        if (netabckgpar > 0) {
          for (int k = 0; k < netabckgpar; k++)
            tbackgroundChi += backgroundEta[k] * MoreMath.pow(chieta[3], k + 1);
        }
//        System.out.println("Next min, max: " + minindex[i] + " " + maxindex[i]);

//	      for (int j = minindex[i]; j < maxindex[i]; j++) {
        for (int j = tmpDatafile.startingindex; j < tmpDatafile.finalindex; j++) {
          double thetaord = tmpDatafile.getXData(j);
          double background = tbackgroundChi;
          for (int k = 0; k < npolbckgpar; k++)
            background += backgroundPol[k] * MoreMath.pow(thetaord, k);
          for (int k = 0; k < numberofpeak; k++)
            background += abkgPeak[k].computeIntensity(thetaord, chieta[0], chieta[1], chieta[2], chieta[3]);

          if (bkgDatafiles.size() > 0) {
            for (int k = 0; k < bkgDatafiles.size(); k++) {
              DiffrDataFile bkgDatafile = (DiffrDataFile)bkgDatafiles.get(k);
              double intensity = bkgDatafile.getIntensityAt(thetaord);
              background += intensity * bkgDatafile.monitorCounts;
            }
          }

          background *= tmpDatafile.getIntensityCalibration(j);
          tmpDatafile.addtoBkgFit(j, background);
        }
        tmpDatafile.spectrumModified = true;
      }
    }
//    refreshBkgComputation = false;
  }

  public void exportExperimentalComputedData(BufferedWriter output) {
    int datafilenumber = activedatafilesnumber();

    for (int i = 0; i < datafilenumber; i++) {
      DiffrDataFile tmpDatafile = getActiveDataFile(i);
      if (tmpDatafile != null)
        tmpDatafile.exportExperimentalComputedData(output);
    }
  }

  public void addPhasePeaks(Phase aphase, Sample asample, boolean lastDataset) {

//			 System.out.println("adding peaks");

//    int datasetNumber = getIndex();
    DiffrDataFile datafiletmp = null;// = adatafile[0];
	  if (activedatafilesnumber() == 0)
		  return;

    if (datafiletmp == null)
      datafiletmp = this.getActiveDataFile(0);

    Instrument ainstrument = getInstrument();

    // compute peaks

    int numberofradiation = ainstrument.getRadiationType().getLinesCount();
    double[] wavelength = new double[numberofradiation];
		double[] radweight = new double[numberofradiation];
    for (int j = 0; j < numberofradiation; j++) {
	    wavelength[j] = ainstrument.getRadiationType().getRadiationWavelength(j);
	    radweight[j] = ainstrument.getRadiationType().getRadiationWeigth(j);
//	    System.out.println("Rad: " + j + " " + wavelength[j] + " " + radweight[j]);
    }

    int numberofpeaks = aphase.gethklNumber();
//		System.out.println("Dataset compute, phase " + aphase.getPhaseName() + ", number of peaks: " + numberofpeaks);
//    aphase.setReflectionNumber(numberofpeaks);

//    double Fhkl[] =
//    Radiation rad1 = radtype.getRadiation(0);
// todo: Luca 23/06/2011    aphase.Fhklcompv(rad1.getRadiationIDNumber(), rad1.tubeNumber, this.getIndex(), lastDataset);

    double Rhkl;

    double intensity = ainstrument.getIntensityValue();
//    boolean distribution = needDistribution();

//			 System.out.println("Adding peaks: " + numberofpeaks);
    for (int i = 0; i < numberofpeaks; i++) {
      double dspace = aphase.getDspacing(i);
      if (datafiletmp.dspacingbase || datafiletmp.energyDispersive ||
		      datafiletmp.computeposition(dspace, wavelength[0]) < 180.0) {
//        if (Fhkl != null)
//          aphase.getReflex(i).setStructureFactor(datasetNumber, Fhkl[i]);
//      double dspace = aphase.getDspacing(i);

        Rhkl = intensity; // * Fhkl[i];
        Peak hklpeak = aphase.getActiveSizeStrain().createPeak(dspace, datafiletmp.dspacingbase,
            datafiletmp.energyDispersive, wavelength, radweight,
                                  aphase.getReflex(i), i);
        hklpeak.setIntensity(Rhkl);
//				hklpeak.setLayer(alayer);
//        hklpeak.setCrystMstrainDistribution(crystallite, microstrain, distx, distsize);
//        hklpeak.setCrystalliteMicrostrain(aphase.getCrystallite(i), aphase.getMicrostrainD(i));

        thepeaklist.addElement(hklpeak);

        dspace = -dspace;
        if (datafiletmp.xInsideRange(datafiletmp.computeposition(dspace, wavelength[0]))) {

          hklpeak = aphase.getActiveSizeStrain().createPeak(dspace, datafiletmp.dspacingbase,
              datafiletmp.energyDispersive, wavelength, radweight,
                                    aphase.getReflex(i), i);
          hklpeak.setIntensity(Rhkl);
//				hklpeak.setLayer(alayer);
//        hklpeak.setCrystMstrainDistribution(crystallite, microstrain, distx, distsize);
//          hklpeak.setCrystalliteMicrostrain(aphase.getCrystallite(i), aphase.getMicrostrainD(i));

          thepeaklist.addElement(hklpeak);
        }
      }// else
      // System.out.println("Not added : " + dspace + ", " + computeposition(dspace, wavelength[0]));
    }

//			 System.out.println("adding peaks completed");
  }

  public void computeSpectra(Sample asample) {

    indexesComputed = false;

	  final Diffraction diffr = getDiffraction();
	  final Reflectivity refle = getReflectivity();
	  final Fluorescence fluo = getFluorescence();

    int datafilenumber = activedatafilesnumber();
	  for (int k = 0; k < datafilenumber; k++) {
		  DiffrDataFile datafile = getActiveDataFile(k);
		  if (datafile.refreshSpectraComputation)
			  datafile.resetPhasesFit();
	  }

	  diffr.computeDiffraction(asample,this);
	  refle.computeReflectivity(asample, this);
	  fluo.computeFluorescence(asample, this);

	  for (int k = 0; k < datafilenumber; k++) {
	  	DiffrDataFile datafile = getActiveDataFile(k);
		  if (datafile.refreshSpectraComputation) {
			  datafile.hasfit = true;
			  datafile.spectrumModified = true;
			  if (isBackgroundExperimental()) {
				  datafile.addExperimentalBackground();
			  }
			  if (isBackgroundInterpolated()) {
				  datafile.addInterpolatedBackgroundFromResiduals();
			  } else
				  datafile.resetBackgroundInterpolated();
		  }

	  }

  }

	public double meanAbsorptionScaleFactor = 1.0;

  public void setMeanAbsorption(double value) {
    meanAbsorptionScaleFactor = value;
  }

  public double getMeanAbsorption() {
    return meanAbsorptionScaleFactor;
  }

  public double getSampleAsymmetry(double x, double[] tilting_angles) {
    return getSample().getSampleAsymmetry(x, tilting_angles);
  }

  public double[] getTextureAngles(DiffrDataFile datafile, double[] tilting_angles, double twotheta) {
    return getInstrument().getTextureAngles(datafile, tilting_angles,
		    getSample().getSampleAngles(), twotheta);
  }

  public double[][] getTextureAngles(DiffrDataFile datafile, double[] tilting_angles, double[] twotheta) {

    return getInstrument().getTextureAngles(datafile, tilting_angles,
		    getSample().getSampleAngles(), twotheta);
  }

  public double[] getAlternateTextureAngles(DiffrDataFile datafile, double[] tilting_angles, double twotheta) {

    return getInstrument().getAlternateTextureAngles(datafile, tilting_angles,
		    getSample().getSampleAngles(), twotheta);
  }

	public double getXshift() {
		return getSample().getXshift() + getDisplacementErrors()[0];
	}

	public double getYshift() {
		return getSample().getYshift() + getDisplacementErrors()[1];
	}

	public double getZshift() {
		return getSample().getZshift() + getDisplacementErrors()[2];
	}

  public double getCorrectedPosition(Sample asample, double x, double[] tilting_angles,
                                     DiffrDataFile adatafile) {
    return getInstrument().getCorrectedPosition(asample, x, tilting_angles, adatafile);
  }

	public boolean extractIntensity() {
    return !getIntensityExtractorMethod().equalsIgnoreCase("none ie") && getFilePar().isTextureFactorsExtractionPermitted();
  }

  public boolean isRandomTexture() {
    return !getIntensityExtractorMethod().equalsIgnoreCase("none ie");
  }

  public boolean isTextureFactorsExtractionPermitted() {
    return extractIntensity();   // todo at the moment we just rely on the fact no texture extraction is in
  }

  public boolean extractPosition() {
    return !getPositionExtractorMethod().equalsIgnoreCase("none pe");
  }

  boolean computed = false;
  int actualThread;
  int index;

  public void computeExpTextureFactor(Sample asample) {

//    FilePar aparFile = getFilePar();
    final Sample theSample = asample;

    if (!isTextureFactorsExtractionPermitted())
      return;
    int datafilenumber = activedatafilesnumber();

    final ProgressFrame prF;
    if (!Constants.textonly && Constants.showProgressFrame) {
//      try {
        prF = new ProgressFrame(datafilenumber);
//      } catch (NullPointerException npe) {
//        System.out.println("Not able to create frame, MacOSX display sleep bug?");
//      }
    } else
      prF = null;
//    printf("Extracting texture weights for dataset: " + toXRDcatString(), prF);
    computed = false;

    actualThread = 0;
    for (index = 0; index < datafilenumber;) {
      while (actualThread >= 1 /* Constants.maxNumberOfThreads*/) {
        try {
          Thread.sleep(Constants.timeToWaitThreadsEnding);
        } catch (InterruptedException ie) {
        }
      }
      int actualIndex1 = index;
      (new PersistentThread() {
        @Override
        public void executeJob() {
          actualThread++;
          DiffrDataFile datafile = getActiveDataFile(index++);
          if (datafile.computeExpTextureFactor(theSample))
            computed = true;
          if (prF != null)
            prF.increaseProgressBarValue();
          actualThread--;
        }
      }).start();
      while (actualIndex1 == index) {
        try {
          Thread.sleep(Constants.timeToWaitThreadsStarting);
        } catch (InterruptedException ie) {
        }
      }
    }
    while (actualThread > 0) {
      try {
        Thread.sleep(Constants.timeToWaitThreadsEnding);
      } catch (InterruptedException ie) {
      }
    }

    if (prF != null) {
      prF.setVisible(false);
      prF.dispose();
    }
//		prF = null;
    if (computed)
      refreshAll(false);
  }

  public void computeExpStrain(Sample asample) {

    FilePar aparFile = getFilePar();

    if (!aparFile.isPositionExtractionPermitted())
      return;
    int datafilenumber = activedatafilesnumber();

    ProgressFrame prF = null;
    if (!Constants.textonly && Constants.showProgressFrame)
      try {
          prF = new ProgressFrame(datafilenumber);
      } catch (NullPointerException npe) {
        out.println("Not able to create frame, MacOSX display sleep bug?");
      }
    printf("Extracting positions for dataset: " + toXRDcatString(), prF);
    boolean computed1 = false;
    for (int k = 0; k < datafilenumber; k++) {
      if (getActiveDataFile(k).computeExpStrain(asample))
        computed1 = true;
      if (prF != null)
        prF.increaseProgressBarValue();
    }
    if (prF != null) {
      prF.setVisible(false);
      prF.dispose();
    }
//		prF = null;
    if (computed1)
      refreshAll(false);
  }

  public int getIntensityExtractorID() {
    return 0;
  }

  public void setIntensityExtractor(String value) {
    if (subordinateField[getIntensityExtractorID()] == null ||
        !value.equals((subordinateField[getIntensityExtractorID()]).identifier))
      setsubordinateField(getIntensityExtractorID(), value);
  }

  public void setIntensityExtractor(int number) {
    setIntensityExtractor(getsubordIdentifier(getIntensityExtractorID(), number));
  }

  public IntensityExtractor getIntensityExtractor() {
    if (subordinateField[getIntensityExtractorID()] == null)
      setIntensityExtractor(0);
    return (IntensityExtractor) subordinateField[getIntensityExtractorID()];
  }

  public String getIntensityExtractorMethod() {
    return getIntensityExtractor().identifier;
  }

  public int getPositionExtractorID() {
    return 1;
  }

  public void setPositionExtractor(String value) {
    if (subordinateField[getPositionExtractorID()] == null ||
        !value.equals((subordinateField[getPositionExtractorID()]).identifier))
      setsubordinateField(getPositionExtractorID(), value);
  }

  public void setPositionExtractor(int number) {
    setPositionExtractor(getsubordIdentifier(getPositionExtractorID(), number));
  }

  public PositionExtractor getPositionExtractor() {
    if (subordinateField[getPositionExtractorID()] == null)
      setPositionExtractor(0);
    return (PositionExtractor) subordinateField[getPositionExtractorID()];
  }

  public String getPositionExtractorMethod() {
    return getPositionExtractor().identifier;
  }

  public int getReflectivityID() {
    return 2;
  }

  public void setReflectivity(String value) {
    if (subordinateField[getReflectivityID()] == null ||
        !value.equals((subordinateField[getReflectivityID()]).identifier))
      setsubordinateField(getReflectivityID(), value);
  }

  public void setReflectivity(int number) {
    setReflectivity(getsubordIdentifier(getReflectivityID(), number));
  }

  public Reflectivity getReflectivity() {
    if (subordinateField[getReflectivityID()] == null)
      setReflectivity(0);
    return (Reflectivity) subordinateField[getReflectivityID()];
  }

  public String getReflectivityMethod() {
    return getReflectivity().identifier;
  }

  public boolean needReflectivityStatistic() {
    return getReflectivity().needReflectivityStatistic();
  }

  public int getDiffractionID() {
    return 5;
  }

	public void setDiffraction(String value) {
		if (subordinateField[getDiffractionID()] == null ||
				!value.equals((subordinateField[getDiffractionID()]).identifier))
			setsubordinateField(getDiffractionID(), value);
	}

	public void setDiffraction(int number) {
		setFluorescence(getsubordIdentifier(getDiffractionID(), number));
	}

	public Diffraction getDiffraction() {
		if (subordinateField[getDiffractionID()] == null)
			setDiffraction(0);
		return (Diffraction) subordinateField[getDiffractionID()];
	}

	public String getDiffractionMethod() {
		return getDiffraction().identifier;
	}

	public int getFluorescenceID() {
		return 4;
	}

	public void setFluorescence(String value) {
    if (subordinateField[getFluorescenceID()] == null ||
        !value.equals((subordinateField[getFluorescenceID()]).identifier))
      setsubordinateField(getFluorescenceID(), value);
  }

  public void setFluorescence(int number) {
    setFluorescence(getsubordIdentifier(getFluorescenceID(), number));
  }

  public Fluorescence getFluorescence() {
    if (subordinateField[getFluorescenceID()] == null)
      setFluorescence(0);
    return (Fluorescence) subordinateField[getFluorescenceID()];
  }

  public String getFluorescenceMethod() {
    return getFluorescence().identifier;
  }

  public boolean needFluorescenceStatistic() {
    return getFluorescence().needFluorescenceStatistic();
  }

  public void fittingFileOutput(boolean addStatisticalError, boolean phaseOutput) {
    int datafilenumber = activedatafilesnumber();

    for (int i = 0; i < datafilenumber; i++) {
      DiffrDataFile adtafile1 = getActiveDataFile(i);
      if (phaseOutput)
        adtafile1.fittingPhaseOutput(getSample(), addStatisticalError);
      else
        adtafile1.fittingFileOutput(addStatisticalError);
    }
  }

  public void freeAllBackgroundParameters() {
    int numbercoef = numbercoefbackg();
    for (int k = 0; k < numbercoef; k++)
      getbackgcoef(k).setRefinableCheckBound();
    numbercoef = numbercoefbackgChi();
    for (int k = 0; k < numbercoef; k++)
      getbackgcoefChi(k).setRefinableCheckBound();
    numbercoef = numbercoefbackgEta();
    for (int k = 0; k < numbercoef; k++)
      getbackgcoefEta(k).setRefinableCheckBound();
    int numberofpeak1 = backgpeaksnumber();
    for (int k = 0; k < numberofpeak1; k++)
      getBkgPeak(k).freeAllBackgroundParameters();

    int datafilenumber = activedatafilesnumber();

    for (int i = 0; i < datafilenumber; i++) {
      getActiveDataFile(i).freeAllBackgroundParameters();
    }
  }

  public void boundMonitorsByBank() {
    int datafilenumber = activedatafilesnumber();
    for (int i = 0; i < datafilenumber; i++) {
	    DiffrDataFile adatafile1 = getActiveDataFile(i);
	    int bankNumber = adatafile1.getBankNumber();
	    for (int j = 0; j < i; j++) {
		    DiffrDataFile adatafile2 = getActiveDataFile(j);
		    int bankNumber2 = adatafile2.getBankNumber();
		    if (bankNumber == bankNumber2) {
			    adatafile1.setEqualTo(adatafile2, false);
			    break;
		    }
	    }
    }
  }

  public void boundMonitorsByAngles(boolean[] useAngle) {
    int datafilenumber = activedatafilesnumber();
    for (int i = 0; i < datafilenumber; i++) {
      DiffrDataFile adatafile1 = getActiveDataFile(i);
      double[] angles = adatafile1.getTiltingAngle();
      for (int j = 0; j < i; j++) {
        DiffrDataFile adatafile2 = getActiveDataFile(j);
        double[] angles2 = adatafile2.getTiltingAngle();
        boolean sameAngles = true;
        for (int k = 0; k < angles.length; k++) {
          if (useAngle[k] && angles[k] != angles2[k]) {
            sameAngles = false;
            break;
          }
        }
        if (sameAngles) {
          adatafile1.setEqualTo(adatafile2, false);
          break;
        }
      }
    }
  }

  public void freeAllScaleParameters() {
    getInstrument().freeAllScaleParameters();
  }

  public boolean freeAllBasicParameters() {
    int datafilenumber = activedatafilesnumber();
    for (int i = 0; i < datafilenumber; i++)
      getActiveDataFile(i).freeAllBasicParameters();
    return getInstrument().freeAllBasicParameters();
  }

  public void fixAllBackgroundParametersPreserveBound() {
    int numbercoef = numbercoefbackg();
    for (int k = 0; k < numbercoef; k++)
      getbackgcoef(k).setNotRefinableCheckBound();
    numbercoef = numbercoefbackgChi();
    for (int k = 0; k < numbercoef; k++)
      getbackgcoefChi(k).setNotRefinableCheckBound();
    numbercoef = numbercoefbackgEta();
    for (int k = 0; k < numbercoef; k++)
      getbackgcoefEta(k).setNotRefinableCheckBound();
    int numberofpeak1 = backgpeaksnumber();
    for (int k = 0; k < numberofpeak1; k++)
      getBkgPeak(k).fixAllParametersPreserveBound();

    for (int i = 0; i < activedatafilesnumber(); i++)
      getActiveDataFile(i).fixAllBackgroundParametersPreserveBound();
  }

  public void freeAllCountMonitors() {
    for (int i = 0; i < activedatafilesnumber(); i++)
      getActiveDataFile(i).freeAllCountMonitors();
  }

  public void freeAllShiftsParameters(boolean forceFree) {
    if (!getInstrument().refineAllZEROBankCoefficients(forceFree))
      for (int i = 0; i < activedatafilesnumber(); i++)
        getActiveDataFile(i).freeThetaParameters();
  }

  public void freeAllShiftsParameters() {
    for (int i = 0; i < activedatafilesnumber(); i++)
      getActiveDataFile(i).freeThetaParameters();
	}

  public void refineAllXYSampleDisplacements() {
    for (int i = 0; i < activedatafilesnumber(); i++)
      getActiveDataFile(i).refineAllXYSampleDisplacements();

  }

  public void fixAllCountMonitorsPreserveBound() {
    for (int i = 0; i < activedatafilesnumber(); i++)
      getActiveDataFile(i).fixAllCountMonitorsPreserveBound();
  }

  @Override
  public void writeSubordinate(BufferedWriter out, String dicstring, XRDcat subordinate) {

    try {
      if (subordinate != null) {
        String name = subordinate.toXRDcatString();
        if (subordinate instanceof DiffrDataFile)
	        name = ((DiffrDataFile) subordinate).getNameRelativeToPar();
        out.newLine();
        out.write("#subordinateObject_" + name);
        out.newLine();
        out.newLine();
        out.write(dicstring + " '" + name + '\'');
        out.newLine();
        out.newLine();
        subordinate.writeParameters(out);
        out.newLine();
        out.write("#end_subordinateObject_" + name);
        out.newLine();
        out.newLine();
      }
    } catch (IOException ioe) {
      System.out.println("Error in writing the subordinate object " + toXRDcatString());
    }

  }

  public void sortPeakArray() {
    sort(thepeaklist, new dhklComparer());
  }

	public void resetPeakArray() {
		thepeaklist = new Vector<Peak>(100, 100);
	}

	public Vector<Peak> getPeakList() {
    return thepeaklist;
  }

  public Peak getPeak(int i) {
    return thepeaklist.elementAt(i);
  }

  public int getNumberofPeaks() {
    return thepeaklist.size();
  }

  @Override
  public void edit(Frame aframe) {
    (new DataD(aframe, this)).setVisible(true);
  }

  public void sortByBankNumber() {
    sort(getDataFileList(), new datafileBankComparer());
    getDataFileList().updateList();
  }

  public void sortByAngles(int[] order) {
    sort(getDataFileList(), new datafileAnglesComparer(order));
    getDataFileList().updateList();
  }

  public void sortByName() {
    sort(getDataFileList(), new datafileNameComparer());
    getDataFileList().updateList();
  }

  public void addAnglesToMeasurement(double[] angles) {
	  int maxNumber = angles.length;
    int[] mult = new int[maxNumber];
    double[] offset = new double[maxNumber];
	  for (int i = 0; i < maxNumber; i++) {
		  mult[i] = 1;
		  offset[i] = angles[i];
	  }
    for (int i = 0; i < datafilesnumber(); i++) {
      DiffrDataFile tmpdatafile = getDataFile(i);
      tmpdatafile.setNewAngles(mult, offset);
    }
  }

	public void changeBackgroundInterpolation() {
		int datafilenumber = datafilesnumber();
		int maxrange = 0;
		int oldInterval = getInterpolatedPointsValue();
		int newInterval = -1;
		for (int i = 0; i < datafilenumber; i++) {
			int range = Math.abs(getDataFile(i).finalindex - getDataFile(i).startingindex);
			if (newInterval == -1)
				newInterval = range  / oldInterval;
			if (maxrange < range) {
				newInterval = range / oldInterval;
				maxrange = range;
			}
		}
		setInterpolatedPoints(newInterval);
	}

	public int getNumberOfTexturePoints(Phase aphase, int hklnumbersel) {
		int total = 0;
		for (int i = 0; i < activedatafilesnumber(); i++) {
			DiffrDataFile adatafile = getActiveDataFile(i);
			for (int j = 0; j < adatafile.positionsPerPattern; j++) {
				boolean goodPoint = adatafile.isInsideRange(adatafile.getPositions(aphase)[hklnumbersel][j][0]);
//				System.out.println(goodPoint);
				if (goodPoint)
					total++;
			}
		}
		return total;
	}

	public void useCountTimeAllSelectedFiles(boolean status) {
		Vector datafiles = getSelectedDatafiles();
		if (datafiles == null) {
			(new AttentionD(new Frame(), "No item from the list selected!")).setVisible(true);
			return;
		}
		int datafilenumber = datafiles.size();
		for (int i = 0; i < datafilenumber; i++) {
			DiffrDataFile tmpdatafile = (DiffrDataFile) datafiles.elementAt(i);
			tmpdatafile.useCountTimeToScale(status);
		}
	}

	public boolean isDiffraction() {
		if (getFluorescence().identifier.toLowerCase().startsWith("none") &&
				getReflectivity().identifier.toLowerCase().startsWith("none"))
			return true;
		return false;
	}

	static class datafileAnglesComparer implements Comparator {

    int[] order;

    public datafileAnglesComparer(int[] order) {
      this.order = order;
    }

    public int compare(Object obj1, Object obj2) {
      double[] angles1 = ((DiffrDataFile) obj1).getTiltingAngle();
      double[] angles2 = ((DiffrDataFile) obj2).getTiltingAngle();
//      for (int ordervalue : order) {
      for (int ordervalue : order) {
        double diff = angles2[ordervalue] - angles1[ordervalue];
        if (diff != 0.0) {
          if (diff > 0)
            return -1;
          return 1;
        }
      }
      return 0;
    }
  }

  class datafileNameComparer implements Comparator {
    public int compare(Object obj1, Object obj2) {
      String name1 = ((DiffrDataFile) obj1).getLabel();
      String name2 = ((DiffrDataFile) obj2).getLabel();
      int len1 = name1.length();
      int len2 = name2.length();
      int minLength = Math.min(len1, len2);
      for (int i = 0; i < minLength; i++) {
        int diff = name2.charAt(i) - name1.charAt(i);
        if (diff != 0.0) {
          if (diff > 0)
            return -1;
          return 1;
        }
      }
      int diff = len2 - len1;
      if (diff == 0.0)
        return 0;
      else if (diff > 0)
        return -1;
      return 1;
    }
  }

  static class datafileBankComparer implements Comparator {
    public int compare(Object obj1, Object obj2) {
      int bank1 = ((DiffrDataFile) obj1).getBankNumber();
      int bank2 = ((DiffrDataFile) obj2).getBankNumber();
      int diff = bank2 - bank1;

      if (diff == 0.0)
        return 0;
      else if (diff > 0)
        return -1;
      return 1;
    }
  }

  static class dhklComparer implements Comparator {
    public int compare(Object obj1, Object obj2) {
      Reflection refl1 = ((Peak) obj1).getReflex();
      Reflection refl2 = ((Peak) obj2).getReflex();
      double dspace1 = refl1.d_space;
      double dspace2 = refl2.d_space;
      double diff = dspace1 - dspace2;

      if (diff == 0.0)
        return 0;
      else if (diff > 0)
        return -1;
      return 1;
    }
  }

	class AngleComparator implements Comparator {

		public int selectedAngle = 0;

		AngleComparator(int index) {
			selectedAngle = index;
		}

		public int compare(Object obj1, Object obj2) {
			double dspace1 = ((DiffrDataFile) obj1).getTiltingAngle()[selectedAngle];
			double dspace2 = ((DiffrDataFile) obj2).getTiltingAngle()[selectedAngle];
			double diff = dspace1 - dspace2;

			if (diff == 0.0)
				return 0;
			else if (diff > 0)
				return 1;
			return -1;
		}
	}

}

