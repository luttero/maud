/*
 * @(#)Sample.java created 01/12/1996 Mesiano
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.awt.SampleD;
import it.unitn.ing.rista.io.JSONFileImport;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.io.cif.*;

import java.awt.*;
import java.io.*;
import java.util.StringTokenizer;
import java.util.Vector;

/**
 * The Sample is a class
 * <p/>
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.29 $, $Date: 2006/12/04 14:30:04 $
 * @since JDK1.1
 */

public class Sample extends Maincat {

  static String[] diclistc = {"_pd_spec_description", "_riet_thin_film_phase_refinement",
		  "_reflectivity_layer_qc_from_phases",

      "_pd_spec_orientation_omega", "_pd_spec_orientation_chi", "_pd_spec_orientation_phi",
      "_riet_par_spec_displac_x", "_riet_par_spec_displac_y", "_riet_par_spec_displac_z",
      "_pd_spec_size_axial", "_pd_spec_size_equat", "_pd_spec_size_thick",
      "_pd_spec_size_radius", "_pd_spec_size_radius_y",

      "_pd_spec_shape", "_riet_layer_solution_method", "_riet_spec_precession_error",

      "_riet_spec_layer_id", "_pd_meas_dataset_id", "_pd_phase_name"};
  static String[] diclistcrm = {"_pd_spec_description", "phase quantities refinement model",
		  "compute layer qc from phase composition/density",

      "sample ref. system omega (deg)", "sample ref. system chi (deg)",
      "sample ref. system phi (deg)",
      "sample displacement x (mm)", "sample displacement y (mm)", "sample displacement z (mm)",
      "sample size x (axial, mm)", "sample size y (equatorial, mm)", "sample thickness z (mm)",
      "sample radius X (spherical, mm)", "sample radius Y (spherical, mm)",

      "_pd_spec_shape", "_riet_layer_solution_method", "_riet_spec_precession_error",

      "_riet_spec_layer_id", "_pd_meas_dataset_id", "_pd_phase_name"};

  static String[] classlistc = {"superclass:it.unitn.ing.rista.diffr.Layer",
      "superclass:it.unitn.ing.rista.diffr.DataFileSet",
      "superclass:it.unitn.ing.rista.diffr.Phase"
  };

  static String[] classlistcs = {
      "superclass:it.unitn.ing.rista.diffr.SampleShape",
      "superclass:it.unitn.ing.rista.diffr.LayerSolutionMethod",
      "superclass:it.unitn.ing.rista.diffr.SpecimenPrecessionError"
  };

  public static final int sampleShapeID = 0;
  public static final int layerSolutionMethodID = 1;
  public static final int precessionErrorID = 2;
	public static final int qcFromPhaseID = 2;

  public static final int layerID = 0;
  public static final int dataID = 1;
  public static final int phaseID = 2;

  double omega = 0.0;
  double chi = 0.0;
  double phi = 0.0;
  double axialDimension = 0.0;
  double equatorialDimension = 0.0;
	double thicknessDimension = 0.0;
  double radiusDimensionX = 0.0;
  double radiusDimensionY = 0.0;
  private double zshift = 0.0;
  private double yshift = 0.0;
  private double xshift = 0.0;
  int theindex = -1;
  public boolean energyModified = false;
	public boolean qc_from_phase = false;

  double drange_final[] = new double[2];

  int activeDatasetsNumber = -1;
  DataFileSet[] activeDataSet = null;
//  protected boolean oldBehaviour = false;
  public static String phaseRefinementModel[] = {"automatic", "phases", "films"};

	public Sample(XRDcat afile, String alabel) {
    super(afile, alabel);
    initXRD();
    identifier = "Sample";
  }

  public Sample(XRDcat afile) {
    this(afile, "Sample_x");
  }

	public Sample() {
		identifier = "Sample";
		IDlabel = "Sample";
		description = "select this to use a Sample";
	}

  public void initConstant() {
    Nstring = 3;
    Nstringloop = 0;
    Nparameter = 11;
    Nparameterloop = 0;
    Nsubordinate = 3;
    Nsubordinateloop = 3;
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

    pivotrequired[0] = true;
    setSampleID("Sample description");
    setPhaseRefinementBehaviour(phaseRefinementModel[0]);
	  setQcFromPhase(false);
    setSubordinateModel(sampleShapeID, "No shape");
    setSubordinateModel(layerSolutionMethodID, "None Layer workout");
    setSubordinateModel(precessionErrorID, "No precession");
    drange_final[0] = 0.0;
    drange_final[1] = 0.0;
	  parameterField[6] = new Parameter(this, getParameterString(6), ParameterPreferences.getDouble(getParameterString(6) + ".value", 0),
			  ParameterPreferences.getDouble(getParameterString(6) + ".min", 0),
			  ParameterPreferences.getDouble(getParameterString(6) + ".max", 100));
	  parameterField[6].setPositiveOnly();
//	  parameterField[6].setMinimumSignificantValue(0.001);
	  parameterField[7] = new Parameter(this, getParameterString(7), ParameterPreferences.getDouble(getParameterString(7) + ".value", 0),
			  ParameterPreferences.getDouble(getParameterString(7) + ".min", 0),
			  ParameterPreferences.getDouble(getParameterString(7) + ".max", 100));
	  parameterField[7].setPositiveOnly();
//	  parameterField[7].setMinimumSignificantValue(0.001);
	  parameterField[8] = new Parameter(this, getParameterString(8), ParameterPreferences.getDouble(getParameterString(8) + ".value", 0),
			  ParameterPreferences.getDouble(getParameterString(8) + ".min", 0),
			  ParameterPreferences.getDouble(getParameterString(8) + ".max", 100));
	  parameterField[8].setPositiveOnly();
//	  parameterField[8].setMinimumSignificantValue(0.000001);
	  parameterField[9] = new Parameter(this, getParameterString(9), ParameterPreferences.getDouble(getParameterString(9) + ".value", 0),
			  ParameterPreferences.getDouble(getParameterString(9) + ".min", 0),
			  ParameterPreferences.getDouble(getParameterString(9) + ".max", 100));
	  parameterField[9].setPositiveOnly();
//	  parameterField[9].setMinimumSignificantValue(0.001);
	  parameterField[10] = new Parameter(this, getParameterString(10), ParameterPreferences.getDouble(getParameterString(10) + ".value", 0),
			  ParameterPreferences.getDouble(getParameterString(10) + ".min", 0),
			  ParameterPreferences.getDouble(getParameterString(10) + ".max", 100));
	  parameterField[10].setPositiveOnly();
//	  parameterField[10].setMinimumSignificantValue(0.001);
  }

  public String toDataString() {
    return "sample_" + toXRDcatString();
  }

  public void setLabel(String alabel) {
    super.setLabel(alabel);
  }

  public String getSampleID() {
    return getString(0);
  }

  public void setSampleID(String astring) {
    setString(0, astring);
  }

  public String getPhaseRefinementBehaviourAsString() {
    return getString(1);
  }

  public int getPhaseRefinementBehaviour() {
    for (int i = 0; i < phaseRefinementModel.length; i++)
      if (getPhaseRefinementBehaviourAsString().equalsIgnoreCase(phaseRefinementModel[i]))
      return i;
    return 0;
  }

  public void setPhaseRefinementBehaviour(String astring) {
    setString(1, astring);
  }

  public void setPhaseRefinementBehaviour(int value) {
    setPhaseRefinementBehaviour(phaseRefinementModel[value]);
  }

  public boolean thinFilmBehaviour() {
    int model = getPhaseRefinementBehaviour();
    switch (model) {
      case 1:
        return false;
      case 2:
        return true;
      default: {
        return layersnumber() > 1;
      }
    }
  }

	public void setQcFromPhase(boolean value) {
		if (value)
			setString(qcFromPhaseID, "true");
		else
			setString(qcFromPhaseID, "false");
	}

	public boolean getQcFromPhase() {
		return getString(qcFromPhaseID).equalsIgnoreCase("true");
	}

	public boolean qcFromPhase() {
		return qc_from_phase;
	}

  public int layersnumber() {
    return getlayerlist().size();
  }

  public ListVector getlayerlist() {
    return subordinateloopField[layerID];
  }

  public ListVector getPhasesList() {
    return subordinateloopField[phaseID];
  }

  public int phasesNumber() {
    return getPhasesList().size();
  }

  public Phase getPhase(int index) {
    if (index >= 0 && index < phasesNumber())
      return (Phase) getPhasesList().elementAt(index);
    else
      return null;
  }

  public int getPhase(Phase aphase) {
    int i;

    for (i = 0; i < phasesNumber(); i++)
      if (aphase == getPhase(i))
        return i;
    return -1;
  }

  public void addPhase(Phase aphase) {
//    System.out.println("Adding phase " + aphase.toXRDcatString() + " " + aphase.getSpaceGroup());
    getPhasesList().addItem(aphase);
    phaseAdded();
  }

  public Phase newPhase() {
    Phase aphase = new Phase(this);
    aphase.initializeAsNew();
    addPhase(aphase);
    return aphase;
//		aphase.addAtom();
  }

  public void removePhase() {
    int index = getPhasesList().getSelectedIndex();
	  for (int i = 0; i < datasetsNumber(); i++)
		  getDataSet(i).removingPhase((Phase) getPhasesList().get(getPhasesList().getSelectedIndex()));
	  getPhasesList().removeSelElement();
    phaseRemovedAt(index);
  }

	public void removeAllPhases() {
		for (int i = getPhasesList().size() - 1; i >= 0; i--) {
			Phase aphase = (Phase) getPhasesList().elementAt(i);
				for (int j = 0; j < datasetsNumber(); j++)
					getDataSet(j).removingPhase(aphase);
				getPhasesList().removeItemAt(i);
				phaseRemovedAt(i);
			}
	}

	public void removePhase(Phase aphase) {
    for (int i = 0; i < getPhasesList().size(); i++)
      if (getPhasesList().elementAt(i) == aphase) {
	      for (int j = 0; j < datasetsNumber(); j++)
		      getDataSet(j).removingPhase(aphase);
	      getPhasesList().removeItemAt(i);
        phaseRemovedAt(i);
        return;
      }
  }

  public void loadPhase(String filename) {
		if (filename.endsWith(".json") || filename.endsWith(".JSON")) {
			JSONFileImport jsonPhaseParser = new JSONFileImport();
			jsonPhaseParser.readFile(filename);
//			JSONArray phaseArray = jsonPhaseParser.getPhasesList();
//			System.out.println (phaseArray);
		} else {
			getFilePar().loadingFile = true;
			CIFParser phaseParser = (new CIFParser(filename, getFilePar().themainframe, this, "Phase"));
			Object[] aphase = phaseParser.getMainCat();
			if (aphase != null) {
				for (int i = 0; i < aphase.length; i++) {
					if (aphase[i] != null) {
						addPhase((Phase) aphase[i]);
						((Phase) aphase[i]).fixAllParameters();
						((Phase) aphase[i]).moveAtomsToStructureModel();
					}
				}
				getFilePar().loadingFile = false;
				notifyUpObjectChanged(this, Constants.OBJECT_ADDED);
				refreshAll(false);
			}
			getFilePar().loadingFile = false;
		}
  }

  public void savePhase() {
  }

  public void loadDataSet(String filename) {
    getFilePar().loadingFile = true;
    CIFParser datasetParser = (new CIFParser(filename, getFilePar().themainframe, this, "DataFileSet"));
    Object[] adata = datasetParser.getMainCat();
    if (adata != null) {
      for (int i = 0; i < adata.length; i++) {
        if (adata[i] != null) {
          addDataFileSet((DataFileSet) adata[i]);
          ((DataFileSet) adata[i]).fixAllParameters();
        }
      }
      getFilePar().loadingFile = false;
      notifyUpObjectChanged(this, Constants.OBJECT_ADDED);
      refreshAll(false);
    }
    getFilePar().loadingFile = false;
  }

  public ListVector getDatasetsList() {
    return subordinateloopField[dataID];
  }

  public int datasetsNumber() {
    return getDatasetsList().size();
  }

  public int activeDatasetsNumber() {
    return activeDatasetsNumber;
  }

  public DataFileSet getActiveDataSet(int index) {
    if (index >= 0 && index < activeDatasetsNumber())
      return activeDataSet[index];
    else
      return null;
  }

  public DataFileSet getDataSet(int index) {
    if (index >= 0 && index < datasetsNumber())
      return (DataFileSet) getDatasetsList().elementAt(index);
    else
      return null;
  }

  public int getDataSet(String id) {
    for (int i = 0; i < datasetsNumber(); i++) {
      if (id.equals(getDataSet(i).toXRDcatString()))
        return i;
    }
    return -1;
  }

  public DataFileSet getDataSetByName(String id) {
    return getDataSet(getDataSet(id));
  }

  public DataFileSet getSelectedDataSet() {
    DataFileSet object = (DataFileSet) getDatasetsList().selectedElement();
    if (object == null) {
      if (getDatasetsList().size() == 0) {
	      object = newData(3);
	      object.getInstrument();
      }
      object = getDataSet(0);
    }
    return object;
  }

	public void addDatafilesFromScript(String filename) {
		Constants.refreshTreePermitted = false;

		if (filename != null) {

			System.out.println("Reading ins file: " + filename);
			String[] folderandname = Misc.getFolderandName(filename);

			BufferedReader reader = Misc.getReader(filename);
			if (reader != null) {
				try {

					String token;
					StringTokenizer st;
					String linedata = reader.readLine();
					Vector cifItems = new Vector(0, 1);

					int pivot = 0;
					int datasetToken = -1;
					int datasetNumber = 0;
					while (!linedata.toLowerCase().startsWith("loop_"))
						linedata = reader.readLine();
					linedata = reader.readLine();
//					System.out.println("Line: " + linedata);
					while (linedata.startsWith("_")) {
						st = new StringTokenizer(linedata, "' ,\t\r\n");
						while (st.hasMoreTokens()) {
							token = st.nextToken();
							cifItems.addElement(token);
//							System.out.println("token: " + token);
							if (token.equalsIgnoreCase("_pd_meas_dataset_id"))
								datasetToken = cifItems.size();
							if (token.equalsIgnoreCase("_riet_meas_datafile_name"))
								pivot = cifItems.size();
						}
						linedata = reader.readLine();
					}

					int maxindex = cifItems.size();
					int index = 0;

					DiffrDataFile datafile[] = null;
					String[] listItems = new String[maxindex];

					while ((linedata != null)) {
//						System.out.println("Data line: " + linedata);
						st = new StringTokenizer(linedata, "' ,\t\r\n");
						while (st.hasMoreTokens()) {
							token = st.nextToken();
							index++;
//							System.out.println("index: " + (index - 1) + " " + token);
							if (index == pivot) {
//								System.out.println(getDataSet(datasetNumber).toString());
//								System.out.println("Adding: " + folderandname[0] + token);
								datafile = getDataSet(datasetNumber).addDataFileforName(folderandname[0] + token, false);
							} else if (index == datasetToken) {
								datasetNumber = getDataSet(token);
							}
							listItems[index - 1] = token;

							if (index == maxindex) {
								index = 0;
								if (datafile != null)
									for (int i = 0; i < maxindex; i++)
										if (i != pivot - 1 && i != datasetToken - 1) {
											for (int ij = 0; ij < datafile.length; ij++) {
												datafile[ij].setField((String) cifItems.elementAt(i), listItems[i], "0", "0", "0", false,
														null, null, null, null, null, false, false);
//								System.out.println(datafile[ij].toString());
//					System.out.println(cifItems.elementAt(i) + " " + listItems[i]);
											}
										}
							}
						}
						linedata = reader.readLine();
					}

				} catch (IOException e) {
					System.out.println("Error loading cif file!");
				}
				try {
					reader.close();
				} catch (IOException e) {
					e.printStackTrace();
					// LogSystem.printStackTrace(e);
				}
			}
		}
		Constants.refreshTreePermitted = true;
		notifyUpObjectChanged(this, 0);
	}

/*  public DataFileSet newData() {
    DataFileSet adata = new DataFileSet(this);
    getDatasetsList().addItem(adata);
    for (int i = 0; i < 3; i++) {
      adata.addBackgroudCoeff();
    }
    adata.initializeAsNew();
    checkActiveDataSets();
    refreshDataIndices();
    return adata;
  }*/

	public DataFileSet newData(int numberBackgroundCoeff) {
		DataFileSet adata = new DataFileSet(this);
		getDatasetsList().addItem(adata);
		for (int i = 0; i < numberBackgroundCoeff; i++) {
			adata.addBackgroudCoeff();
		}
		adata.initializeAsNew();
		checkActiveDataSets();
		refreshDataIndices();
		return adata;
	}

	public void addDataFileSet(DataFileSet adata) {
    getDatasetsList().addItem(adata);
    checkActiveDataSets();
    refreshDataIndices();
  }

  public void removeData() {
    getDatasetsList().removeSelElement();
    checkActiveDataSets();
    refreshDataIndices();
  }

/*  public void loadData(String filename) {
    getFilePar().loadingFile = true;
    CIFParser dataParser = (new CIFParser(filename, (Frame) getFilePar().themainframe, this, "DataFileSet"));
    Object[] adata = dataParser.getMainCat();
    if (adata != null) {
      for (int i = 0; i < adata.length; i++) {
        if (adata[i] != null) {
          getDatasetsList().addItem(adata[i]);
          ((DataFileSet) adata[i]).fixAllParameters();
        }
      }
      getFilePar().loadingFile = false;
      refreshAll(false);
    }
  }*/

  public void savedata() {
  }

  public boolean isActive(XRDcat acat) {
//		System.out.println(acat);
    for (int i = 0; i < datasetsNumber(); i++) {
      DataFileSet adata = getDataSet(i);
      if (acat == adata) {
        if (isDataSetActive((DataFileSet) acat))
          return true;
        else
          return false;
      }
    }
    for (int i = 0; i < phasesNumber(); i++) {
      Phase obj = getPhase(i);
      if (acat == obj) {
        if (isPhasePresent((Phase) acat))
          return true;
        else
          return false;
      }
    }
    return super.isActive(acat);
  }

  public void refreshAll(boolean firstLoading) {
    if (!getFilePar().isLoadingFile() && isAbilitatetoRefresh) {
      refreshComputation = true;
      setRefreshAllStatus();
      update(firstLoading);
      Object[] childrens = getObjectChildren();
      int numberOfChildrens = childrens.length;
      for (int i = 0; i < numberOfChildrens; i++) {
//				System.out.println(childrens[i].toXRDcatString());
        ((XRDcat) childrens[i]).refreshAll(firstLoading);
      }
//      System.out.println("indices");
      checkActiveDataSets();
      refreshDataIndices();
    }
  }

  public void checkActiveDataSets() {
    activeDatasetsNumber = 0;
    for (int i = 0; i < datasetsNumber(); i++)
      if (getDataSet(i).enabled())
        activeDatasetsNumber++;
    activeDataSet = new DataFileSet[activeDatasetsNumber];
//    System.out.println("Check " + activeDatasetsNumber);
    int index = 0;
    for (int i = 0; i < datasetsNumber(); i++)
      if (getDataSet(i).enabled())
        activeDataSet[index++] = getDataSet(i);
  }

  public DiffrDataFile getActiveDiffrDataFile(int datafilenumber) {
    int index = 0;
    int oldindex = 0;
    for (int i = 0; i < activeDatasetsNumber(); i++) {
      DataFileSet adataset = getActiveDataSet(i);
      int filenumber = adataset.activedatafilesnumber();
      index += filenumber;
      if (datafilenumber >= oldindex && datafilenumber < index)
        return adataset.getActiveDataFile(datafilenumber - oldindex);
      oldindex = index;
    }
    return null;
  }

  int numberActiveDataFiles = 0;

  public void resetActiveDatafiles() {
    numberActiveDataFiles = checkNumberActiveDatafiles();
  }

  public int getNumberActiveDatafiles() {
    return numberActiveDataFiles;
  }

  public void setNumberActiveDatafiles(int value) {
    numberActiveDataFiles = value;
  }

  public boolean isDataSetActive(DataFileSet adataset) {
    if (adataset.enabled())
      return true;
    return false;
  }

  public Layer getlayer(int index) {
    if (index >= 0 && index < layersnumber())
      return (Layer) subordinateloopField[0].elementAt(index);
    else
      return null;
  }

  public Layer getTopLayer() {
    return getlayer(0);
  }

  public Layer getSubstrateLayer() {
    return getlayer(layersnumber() - 1);
  }

  public Layer addLayer() {
    return (Layer) addsubordinateloopField(0, new Layer(this));
  }

  public Layer addLayer(int index) {
    if (index >= 0)
      return (Layer) addsubordinateloopField(0, new Layer(this), index);
    else
      return addLayer();
  }

  public void phaseRemovedAt(int index) {
    for (int i = 0; i < layersnumber(); i++)
      getlayer(i).removePhase(index);
  }

  public void phaseAdded() {
    for (int i = 0; i < layersnumber(); i++)
      getlayer(i).addPhase();
  }

  public SpecimenPrecessionError getSpecimenPrecessionError() {
    return (SpecimenPrecessionError) getActiveSubordinateModel(precessionErrorID);
  }

  /**
   * Check the consistency of this object respect to the problem implemented.
   * It's automaticly called before computation by the validate routine.
   * Subclasses of this object could overwrite it to implement specific checking that must be
   * done before starting computation.
   *
   * @return the status of the checking; null if all it's ok, a message with the problem otherwise.
   */
  public String checkIntegrity() {
//    checkDataSetList();
    if (activeDatasetsNumber() > 0)
      return null;
    else
      return "No active datasets in the sample!";
  }

  int numberOfData = 0;

  public int computeDataNumber() {
    numberOfData = 0;

    for (int i = 0; i < activeDatasetsNumber(); i++) {
      DataFileSet adataset = getActiveDataSet(i);
      if (adataset != null) {
        numberOfData += adataset.computeDataNumber();
      } else
        System.out.println("dataset not found: " + getActiveDataSet(i));
    }

    return numberOfData;
  }

  public int getNumberOfData() {
    return numberOfData;
  }

  public double[] getData() {
    int dnumber = 0;
    double dta[] = new double[getNumberOfData()];

    for (int i = 0; i < activeDatasetsNumber(); i++) {
      DataFileSet adataset = getActiveDataSet(i);
      if (adataset != null) {
        int apnumber = adataset.getNumberOfData();
        double adta[] = adataset.getData();
        for (int j = 0; j < apnumber; j++)
          dta[dnumber + j] = adta[j];
        dnumber += apnumber;
      }
    }
    return dta;
  }

  public double[] getWeight() {
    int dnumber = 0;
    double finalWgt[] = new double[getNumberOfData()];

    for (int i = 0; i < activeDatasetsNumber(); i++) {
      DataFileSet adataset = getActiveDataSet(i);
      if (adataset != null) {
        int apnumber = adataset.getNumberOfData();
        double wgt[] = adataset.getWeight();
        for (int j = 0; j < apnumber; j++)
	        finalWgt[dnumber + j] = wgt[j];
        dnumber += apnumber;
      }
    }
    return finalWgt;
  }

  public double[] getFit() {
    int dnumber = 0;
    double dta[] = new double[getNumberOfData()];

    for (int i = 0; i < activeDatasetsNumber(); i++) {
      DataFileSet adataset = getActiveDataSet(i);
      if (adataset != null) {
        int apnumber = adataset.getNumberOfData();
        double adta[] = adataset.getFit();
        for (int j = 0; j < apnumber; j++)
          dta[dnumber + j] = adta[j];
        dnumber += apnumber;
      }
    }
    return dta;
  }

  public Parameter getomega() {
    return parameterField[0];
  }

  public Parameter getchi() {
    return parameterField[1];
  }

  public Parameter getphi() {
    return parameterField[2];
  }

  public Parameter getdispx() {
    return parameterField[3];
  }

  public Parameter getdispy() {
    return parameterField[4];
  }

  public Parameter getdispz() {
    return parameterField[5];
  }

  public Parameter getAxialDimension() {
    return parameterField[6];
  }

  public Parameter getEquatorialDimension() {
    return parameterField[7];
  }

  public Parameter getThicknessDimension() {
    return parameterField[8];
  }

  public Parameter getRadiusDimensionX() {
    return parameterField[9];
  }

  public Parameter getRadiusDimensionY() {
    return parameterField[10];
  }

  public void setomega(String astring) {
    parameterField[0].setValue(astring);
  }

  public void setchi(String astring) {
    parameterField[1].setValue(astring);
  }

  public void setphi(String astring) {
    parameterField[2].setValue(astring);
  }

  public void setdispx(String astring) {
    parameterField[3].setValue(astring);
  }

  public void setdispy(String astring) {
    parameterField[4].setValue(astring);
  }

  public void setdispz(String astring) {
    parameterField[5].setValue(astring);
  }

  public void normalizePhaseQuantity() {
    for (int j = 0; j < layersnumber(); j++)
      getlayer(j).normalizePhaseQuantity(true);
  }

  public void normalizePhaseQuantity(double phaseLimitForRemove) {
    for (int j = 0; j < layersnumber(); j++)
      getlayer(j).normalizePhaseQuantity(phaseLimitForRemove);
  }

  double[] texture_angles = new double[3];

  @Override
  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(firstLoading);
	  qc_from_phase = getQcFromPhase();
  }

  @Override
  public void updateParametertoDoubleBuffering(boolean firstLoading) {

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);

    texture_angles[0] = getParameterValue(0); // omega
    texture_angles[1] = getParameterValue(1); // chi
    texture_angles[2] = getParameterValue(2); // phi
    axialDimension = Math.abs(getParameterValue(6));
    equatorialDimension = Math.abs(getParameterValue(7));
	  thicknessDimension = Math.abs(getParameterValue(8));
    radiusDimensionX = Math.abs(getParameterValue(9));
    radiusDimensionY = Math.abs(getParameterValue(10));
    zshift = getParameterValue(5);
    yshift = getParameterValue(4);
    xshift = getParameterValue(3);

  }

	public double getXshift() {
		return xshift;
	}

	public double getYshift() {
		return yshift;
	}

	public double getZshift() {
		return zshift;
	}

	public double[] getSampleAngles() {
    return texture_angles;
  }

  public double getAxialDimensionD() {
    return axialDimension;
  }

  public double getEquatorialDimensionD() {
    return equatorialDimension;
  }

	public double getThicknessDimensionD() {
		return thicknessDimension;
	}

	public double getRadiusDimensionXD() {
    return radiusDimensionX;
  }

  public double getRadiusDimensionYD() {
    return radiusDimensionY;
  }

  public void finalOutput(OutputStream out, boolean outputGraph) throws IOException {
    double[] indexes = getRefinementIndexes();
    printLine(out, "Sample " + toXRDcatString() + " :");
	  printLine(out, "Sample Rwp: " + Fmt.format(indexes[0]));
	  printLine(out, "Sample Rp: " + Fmt.format(indexes[4]));
	  printLine(out, "Sample Rwpnb (no background): " + Fmt.format(indexes[1]));
	  printLine(out, "Sample Rwpnb1 (no bkg rescaled): " + Fmt.format(indexes[2]));
	  printLine(out, "Sample Rwpnb2 (no bkg rescaled^2): " + Fmt.format(indexes[3]));
	  printLine(out, "Sample Rpnb (no background): " + Fmt.format(indexes[5]));
	  printLine(out, "Sample Rpnb1 (no bkg rescaled): " + Fmt.format(indexes[6]));
	  printLine(out, "Sample Rpnb2 (no bkg rescaled^2): " + Fmt.format(indexes[7]));
	  printLine(out, "Sample energy: " + Fmt.format(indexes[18]));
    newLine(out);
    printLine(out, "Refinement final output indices for single datasets:");
    out.flush();
    for (int i = 0; i < activeDatasetsNumber(); i++) {
      getActiveDataSet(i).finalOutput(out, outputGraph);
    }
  }

  boolean indexesComputed = false;
  double[] refinementIndexes = new double[19];

  public double[] getRefinementIndexes() {

    if (!indexesComputed) {

      for (int j = 0; j < 19; j++)
        refinementIndexes[j] = 0.0;

      for (int i = 0; i < activeDatasetsNumber(); i++) {
        DataFileSet adataset = getActiveDataSet(i);

        double[] refIndex = adataset.getRefinementIndexes();
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

      refinementIndexes[18] = 0.0;
      for (int i = 0; i < phasesNumber(); i++) {
        double energy = getEnergy(i);
        refinementIndexes[18] += energy * energy;
      }

      indexesComputed = true;
    }
    return refinementIndexes;

  }

  public void setIndex(int index) {
    theindex = index;
  }

  public int getIndex() {
    if (theindex == -1)
      refreshIndex();
    return theindex;
  }

  public void refreshIndex() {
    getFilePar().refreshSampleIndices();
    refreshDataIndices();
  }

  public void refreshDataIndices() {
    FilePar aparFile = getFilePar();
//    aparFile.refreshDataIndices();
  }

  public void prepareComputation() {
	  Constants.threadingGranularity = MaudPreferences.getInteger("parallel_processing.granularity(0-3)", Constants.MEDIUM_GRANULARITY);
	  Constants.debugThreads = MaudPreferences.getBoolean("parallel_processing.threads_debug", false);
    Constants.maxNumberOfThreads =
		    MaudPreferences.getInteger("parallel_processing.threads_maxNumber", Constants.maxNumberOfThreads);
//    if (Constants.maxNumberOfThreads > 1)
//      Constants.warnAboutThreads();
    // set layer indices
    checkActiveDataSets();
	  resetActiveDatafiles();
    numberOfLayers = layersnumber();
    for (int j = 0; j < numberOfLayers; j++)
      getlayer(j).setIndex(j);

//		int index = 0;

    for (int i = 0; i < activeDatasetsNumber(); i++) {
      getActiveDataSet(i).setDataFileSetIndex(i);
//      getActiveDataSet(i).computeRange();
    }

    numberOfPhases = phasesNumber();
    //just temporarly added
    for (int j = 0; j < numberOfLayers; j++) {
      Layer alayer = getlayer(j);
      alayer.prepareComputation();
    }
  }

  public void computeBefore() {
  }

  public void computeAll() {
  }

  public void computeAfter() {
  }

  public void closeComputation() {

    // free resources
  }

  public void preparePartialComputation() {
  }

  public void computePartial() {
  }

  public void closePartialComputation() {
  }

  public void computeRange() {
    drange_final[1] = 0.0;
    drange_final[0] = 1.0E+10;
//    System.out.println("d " +activeDatasetsNumber());

    for (int i = 0; i < activeDatasetsNumber(); i++) {
      DataFileSet adataset = getActiveDataSet(i);
      if (adataset != null) {
//        System.out.println("Computing range dataset " + adataset.toXRDcatString());
        adataset.computeRange();
      } else
        System.out.println("dataset not found: " + getActiveDataSet(i));
    }
  }

  public void setRange(double drange[]) {
    if (drange[0] < drange_final[0])
      drange_final[0] = drange[0];
    if (drange[1] > drange_final[1])
      drange_final[1] = drange[1];
  }

  public double getDspacingMin() {
    if (drange_final[0] == 0.0)
      computeRange();
    return drange_final[0];
  }

  public double getDspacingMax() {
    if (drange_final[1] == 0.0)
      computeRange();
    return drange_final[1];
  }

  public int checkNumberActiveDatafiles() {
    int totnumber = 0;
    for (int i = 0; i < activeDatasetsNumber(); i++) {
      DataFileSet adataset = getActiveDataSet(i);
      totnumber += adataset.activedatafilesnumber();
    }
    return totnumber;
  }

	public double[] getActiveTextureAngles(Reflection refl, int index) {
		int totnumber = 0;
		Phase phase = refl.getParent();
		int reflIndex = phase.getReflexIndex(refl);
		for (int i = 0; i < activeDatasetsNumber(); i++) {
			DataFileSet adataset = getActiveDataSet(i);
			for (int j = 0; j < adataset.activedatafilesnumber(); j++) {
				DiffrDataFile datafile = adataset.getActiveDataFile(j);
				if (totnumber == index) {
					double position = datafile.getPositions(phase)[reflIndex][0][0];
					return datafile.getTextureAngles(position);
				}
				totnumber++; // todo positionsPerPattern
			}
		}
		return null;
	}

  public void checkForOldPhasesBehaviour() {
    boolean oldBehaviour = true;
    if (getDataSet(0) != null) {
      oldBehaviour = oldBehaviour &&
          getDataSet(0).getInstrument().getIntensity().getFree();
      boolean layerBehaviour = false;
      for (int j = 0; j < layersnumber(); j++) {
        if (!layerBehaviour) {
          layerBehaviour = getlayer(j).checkForOldPhasesBehaviour();
        }
      }
      oldBehaviour = oldBehaviour && layerBehaviour;
    }
    this.setPhaseRefinementBehaviour(phaseRefinementModel[2]);
  }

/*  public void refreshForNotificationUp(XRDcat source, int reason) {
    refreshComputation = true;
    if (source == null)
      return;
    if (source == this) {
      updateAll();
      normalizePhaseQuantity();
    } else if (source instanceof Phase || source instanceof StructureModel)
      normalizePhaseQuantity();
  }   */

  public double[][][] phaseQuantity = null;
  public int numberOfPhases = 0;
  public int numberOfLayers = 0;
  boolean[] firstComputationSpectra = null;
  boolean extractIntensities = false;
  boolean extractPositions = false;
  int index = 0;
  int phaseIndex = 0;

  public void computeSpectra(boolean hasoutput) {

    indexesComputed = false;

    FilePar aparFile = getFilePar();
//    for (int nd = 0; nd < activeDatasetsNumber(); nd++)
//      getActiveDataSet(nd).setRadiation();

	  Texture.rotateAlpha = MaudPreferences.getDouble("Texture.phiZero", -90.0);

	  // compute spectra
    phaseQuantity = new double[numberOfLayers][numberOfPhases][activeDatasetsNumber()];
    for (int j = 0; j < numberOfLayers; j++) {
      Layer alayer = getlayer(j);

      for (int ph = 0; ph < numberOfPhases; ph++) {
        Phase aphase = getPhase(ph);
        double quantity = 0.0;
        if (thinFilmBehaviour())
          quantity = alayer.getNormalizedPhaseQuantity(ph);
        else
          quantity = alayer.getPhaseScaleFactor(ph);
        double cellVol = aphase.getCellVolume();
        for (int nd = 0; nd < activeDatasetsNumber(); nd++) {
          quantity = aphase.getApparentQuantity(quantity,
		          getActiveDataSet(nd).getInstrument().getRadiationType(), alayer);
          phaseQuantity[j][ph][nd] = quantity / (cellVol * cellVol);
        }
      }
    }

    for (int nd = 0; nd < activeDatasetsNumber(); nd++) {
      getActiveDataSet(nd).setMeanAbsorption(getMeanAbsorption(getActiveDataSet(nd).getInstrument().getRadiationType()));
//      getActiveDataSet(nd).setTotalLayerAbsorption(getTotalLayerAbsorption(
//		      getActiveDataSet(nd).getInstrument().getRadiationType()));
    }

	  for (int i = 0; i < phasesNumber(); i++) {
		  getPhase(i).refreshIndices(this);
	  }

	  long previousTime = Constants.tmpTime;
    if (Constants.testtime)
      System.out.println("Sample preparation: " +
          (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");

    for (index = 0; index < activeDatasetsNumber();) {
      if (getActiveDataSet(index) != null && getActiveDataSet(index).refreshComputation) {
        if (numberOfPhases > 0) { // && (getActiveDataSet(index).getFluorescence() instanceof FluorescenceNone))
	        DataFileSet tadataset = getActiveDataSet(index);
	        tadataset.resetPeakArray();
	        for (phaseIndex = 0; phaseIndex < numberOfPhases;) {
            double totQuantity = 0.0;
            for (int j = 0; j < numberOfLayers; j++) {
              totQuantity += phaseQuantity[j][phaseIndex][index];
            }
            if (totQuantity != 0.0) {
              Phase aphase = getPhase(phaseIndex);
              if (phaseIndex == numberOfPhases - 1)
                index++;
              phaseIndex++;
	            if (tadataset.activedatafilesnumber() > 0)
                  tadataset.addPhasePeaks(aphase, Sample.this, false);
            } else {
              phaseIndex++;
              if (phaseIndex == numberOfPhases)
                index++;
            }
          }
        } else
          index++;
      } else
        index++;
    }

    if (Constants.testtime)
      System.out.println("Sample add Phase peaks: " +
          (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");

      for (int ip = 0; ip < activeDatasetsNumber(); ip++) {
        if (getActiveDataSet(ip) != null && getActiveDataSet(ip).refreshComputation) {
          DataFileSet tadataset = getActiveDataSet(ip);
          tadataset.sortPeakArray();
        }
      }

	  boolean[] positionRefreshed = computeReflectionsPosition();
	  for (int ph = 0; ph < numberOfPhases; ph++)
		  getPhase(ph).checkReflectionsInRange();

	  for (int ip = 0; ip < numberOfPhases; ip++) {
		  double totQuantity = 0.0;
		  for (int j = 0; j < numberOfLayers; j++)
			  for (int nd = 0; nd < activeDatasetsNumber(); nd++)
				  totQuantity += phaseQuantity[j][ip][nd];
		  if (totQuantity != 0.0) {
			  Phase aphase = getPhase(ip);
//			  aphase.computeTextureFactor(Sample.this);
			  aphase.computeStrain(this);
		  }
	  }

	  computeFinalPositions();

	  computeLorentzPolarization(positionRefreshed);
     computeScatteringFactors(positionRefreshed);

	  for (int ip = 0; ip < numberOfPhases; ip++) {
		  Phase aphase = getPhase(ip);
		  double totQuantity = 0.0;
		  for (int j = 0; j < numberOfLayers; j++) {
			  for (int nd = 0; nd < activeDatasetsNumber(); nd++)
				  totQuantity += phaseQuantity[j][ip][nd];
			  getlayer(j).getChemicalComposition(); // we refresh the chemical composition for fluorescence
		  }
		  if (totQuantity != 0.0) {
			  aphase.computeStructureFactors(this, false);  // todo check if merging with subsequent can be done
		  }
	  }
//	  System.out.println("Size Strain");
    boolean[] refreshPhases = computeSizeStrainBroadening(); // todo, group all together?
    boolean[] refreshDataset = computeInstrumentBroadening();
    computeSampleBroadening(refreshDataset, refreshPhases);


    if (Constants.testtime)
      System.out.println("Refreshing peaks: " +
          (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");

    computeShapeAbsorptionCorrection();

    if (Constants.testtime)
      System.out.println("Shape absorption: " +
          (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");

	  extractIntensities = false;
    for (int i = 0; i < numberOfPhases; i++) {
      Phase aphase = getPhase(i);
      if (aphase.extractIntensities()) {
        extractIntensities = true;
/*	      for (int j = 0; j < activeDatasetsNumber(); j++) {
		      for (int k = 0; k < getActiveDataSet(j).activedatafilesnumber(); k++) {
			      getActiveDataSet(j).getActiveDataFile(k).computedToExperimentalTextureFactors(aphase);
		      }
	      }*/
      }
      if (aphase.extractPositions())
        extractPositions = true;
    }

    if (Constants.testtime)
      System.out.println("Setting datafiles and layers: " +
          (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");

    firstComputationSpectra = new boolean[activeDatasetsNumber()];
    for (int i = 0; i < activeDatasetsNumber(); i++)
      firstComputationSpectra[i] = false;

      for (int ip = 0; ip < activeDatasetsNumber(); ip++) {
        if (getActiveDataSet(ip) != null && getActiveDataSet(ip).refreshComputation) {
          DataFileSet tadataset = getActiveDataSet(ip);
          tadataset.computeBackground();
          if (tadataset.extractIntensity() && extractIntensities) {
            tadataset.computeSpectra(Sample.this);
            firstComputationSpectra[ip] = true;
            tadataset.computeExpTextureFactor(Sample.this);
          }
          if (tadataset.extractPosition() && extractPositions) {
            if (!firstComputationSpectra[ip]) {
              tadataset.computeSpectra(Sample.this);
              firstComputationSpectra[ip] = true;
            }
            getActiveDataSet(ip).computeExpStrain(Sample.this);
          }
        }
      }

    if (Constants.testtime)
      System.out.println("Background and extraction: " +
          (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");

      for (int ip = 0; ip < numberOfPhases; ip++) {
        double totQuantity = 0.0;
        for (int j = 0; j < numberOfLayers; j++)
          for (int nd = 0; nd < activeDatasetsNumber(); nd++)
            totQuantity += phaseQuantity[j][ip][nd];
        if (totQuantity != 0.0) {
          Phase aphase = getPhase(ip);
          aphase.computeTextureFactor(Sample.this);
//          aphase.computeStrain(Sample.this);
        }
      }
//		computeStrains(); // todo eliminate second call, not necessary

	  if (Constants.testtime)
      System.out.println("Texture and strain: " +
          (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");

    boolean extractInPhase = false;
    if (aparFile.isStructureFactorExtractionPermitted()) {
      for (int i = 0; i < numberOfPhases; i++) {
        Phase aphase = getPhase(i);
        if (aphase.extractStructureFactors()) {
          extractInPhase = true;
        }
      }
    }

    if (extractInPhase) {
        for (int ip = 0; ip < activeDatasetsNumber(); ip++) {
          if (!firstComputationSpectra[ip] && getActiveDataSet(ip).refreshComputation) {
            getActiveDataSet(ip).computeSpectra(Sample.this);
            firstComputationSpectra[ip] = true;
          }
        }
    }

// luca2013 computeStructureFactors was here

//    if (aparFile.isStructureFactorExtractionPermitted()) {
	  for (int i = 0; i < numberOfPhases; i++) {
		  Phase aphase = getPhase(i);
		  if (aphase.extractStructureFactors()) {
			  double totQuantity = 0.0;
			  for (int j = 0; j < numberOfLayers; j++)
				  for (int nd = 0; nd < activeDatasetsNumber(); nd++)
					  totQuantity += phaseQuantity[j][i][nd];
			  if (totQuantity != 0.0 && aphase.extractStructureFactors()) {
				  aphase.extractStructureFactors(this);
				  if (aphase.solveCrystalStructure())
				    aphase.computeStructureFactors(this, true);
			  }
		  }
	  }
//    }

	  if (Constants.testtime)
      System.out.println("Structure factor extraction and computation: " +
          (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");

      for (int ip = 0; ip < activeDatasetsNumber(); ip++) {
        if (getActiveDataSet(ip) != null && getActiveDataSet(ip).refreshComputation) {
          getActiveDataSet(ip).computeSpectra(Sample.this);
          getActiveDataSet(ip).setRefreshComputation(false);
        }
      }

    if (Constants.testtime)
      System.out.println("spectra computing: " +
          (-previousTime + (previousTime = System.currentTimeMillis())) + " millisecs.");

    firstComputationSpectra = null;
    refreshComputation = false;
  }

  public void computeEnergy() {
      for (int ip = 0; ip < phasesNumber(); ip++) {
        if (getPhase(ip) != null && getPhase(ip).refreshEnergyComputation) {
          energyModified = true;
          Phase aphase = getPhase(ip);
          aphase.computeEnergy();
        }
      }
  }

  public double getEnergy(int i) {
    if (i >= 0 && i < phasesNumber())
      return getPhase(i).getEnergyLevel();
    else
      return 0.0;
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      for (int i = 0; i < parameterField.length; i++) {
        if (source == parameterField[i]) {
          if (i < 3) {
            notifyParameterChanged(source, Constants.SAMPLE_ORIENTATION_CHANGED);
            return;
          }
          if (i > 2 && i < 6) {
            notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED);
            return;
          }
          if (i > 5 && i < 11) {
            notifyParameterChanged(source, Constants.ANGULAR_CALIBRATION);
            notifyParameterChanged(source, Constants.LORENTZ_POLARIZATION_CHANGED);
            return;
          }
        }
      }
      super.notifyParameterChanged(source);
    }
  }

  public void updateAll() {
    refreshComputation = true;
  }

  public int getPhaseIndex(Phase aphase) {
    for (int ph = 0; ph < numberOfPhases; ph++) {
      Phase phase = getPhase(ph);
      if (phase == aphase)
        return ph;
    }
    return 0;
  }

  public int getLayerIndex(Layer alayer) {
    for (int j = 0; j < layersnumber(); j++) {
      Layer layer = getlayer(j);
      if (alayer == layer)
        return j;
    }
    return 0;
  }

  public boolean isPhasePresent(Phase aphase) {
    return isPhasePresent(aphase, 0.0);
  }

  public boolean isPhasePresent(Phase aphase, double quantity) {
    // must be rewritten to average the quantity over all layers
    int numberoflayers = layersnumber();
    for (int i = 0; i < phasesNumber(); i++) {
      if (getPhase(i).toXRDcatString().equals(aphase.toXRDcatString())) {
        for (int j = 0; j < numberoflayers; j++)
          if (getlayer(j).getNormalizedPhaseQuantity(i) > quantity)
            return true;
      }
    }
    return false;
  }

  public boolean isPhasePresent(int indexofPhase) {
    return isPhasePresent(indexofPhase, 0.0);
  }

  public boolean isPhasePresent(int indexofPhase, double quantity) {
    // must be rewritten to average the quantity over all layers
//		FilePar aparFile = (FilePar) getFilePar();

    int numberoflayers = layersnumber();
    for (int j = 0; j < numberoflayers; j++)
      if (getlayer(j).getNormalizedPhaseQuantity(indexofPhase) > quantity)
        return true;
    return false;
  }

  public double getSampleAsymmetry(double x, double[] tilting_angles) {
    // up to now is not completed
    return 0.0;
  }

  public double getLayerAbsorption_new(double energyInKeV, int layerIndex, double[] incidentDiffractionAngles,
                                       DataFileSet adataset) {

	  // todo what about Debye-Scherrer
	  double expTransmission = 0.0;
	  double expAbsorption = 1.0;
	  Layer alayer = getlayer(layerIndex);
//    System.out.println(incidentDiffractionAngles[0] * Constants.PITODEG
//        + " " + incidentDiffractionAngles[2] * Constants.PITODEG);
	  if (incidentDiffractionAngles[0] < 1.0E-5 || incidentDiffractionAngles[2] < 1.0E-5)
		  return 0.0;
	  double sinIncAngle = Math.abs(Math.sin(incidentDiffractionAngles[0]));
	  double sinDiffAngle = Math.abs(Math.sin(incidentDiffractionAngles[2]));
	  double factor = 1.0 / sinIncAngle + 1.0 / sinDiffAngle;
	  double absorption = alayer.getLayerAbsorption(energyInKeV);
	  double absorptionLayer = factor * absorption;
	  // alayer.getLayerAbsorption(rad, sinIncAngle, sinDiffAngle);
	  if (incidentDiffractionAngles[2] > 0.0) {
		  double transmission = alayer.getOverLayerAbsorption(energyInKeV) * factor;
//    System.out.println("transmission: " + transmission + " layer " + layerIndex);
		  if (transmission < 200)
			  expTransmission = Math.exp(-transmission);
		  if (absorptionLayer < 200)
			  expAbsorption = 1.0 - Math.exp(-absorptionLayer);
	  }

//	  System.out.println("Layer: " + alayer.toString() + " " + absorption);
	  if (absorption >= 0.0) {
//      System.out.println("abs = " + sinDiffAngle + " * " + alayer.getThicknessValue() + " / (" + absorption +
//          " * (" + sinIncAngle + " + " + sinDiffAngle + " )) * " +
//          expTransmission + " * " + expAbsorption);
		  return 2.0 * sinDiffAngle * alayer.getThicknessValue() / (absorption * (sinIncAngle + sinDiffAngle)) *
				  expTransmission * expAbsorption * adataset.getMeanAbsorption();
	  } else
		  return 1.0;
  }

  public double[] getLayerAbsorption_new(RadiationType rad, int layerIndex, double[][] incidentDiffractionAngles,
                                       DataFileSet adataset) {

    // todo what about Debye-Scherrer
    double expTransmission = 0.0;
    double expAbsorption = 1.0;
    Layer alayer = getlayer(layerIndex);

    double[] radAbs = new double[rad.getLinesCount()];
//    System.out.println(incidentDiffractionAngles[0][0] * Constants.PITODEG
//        + " " + incidentDiffractionAngles[0][2] * Constants.PITODEG);
	  for (int i = 0; i < rad.getLinesCount(); i++) {
		  if (incidentDiffractionAngles[i][0] < 1.0E-5 || incidentDiffractionAngles[i][2] < 1.0E-5)
			  radAbs[i] = 0;
		  else {
			  double sinIncAngle = Math.abs(Math.sin(incidentDiffractionAngles[i][0]));
			  double sinDiffAngle = Math.abs(Math.sin(incidentDiffractionAngles[i][2]));
			  double factor = 1.0 / sinIncAngle + 1.0 / sinDiffAngle;
			  double absorption = alayer.getLayerAbsorption(rad);
			  double absorptionLayer = factor * absorption;
			  // alayer.getLayerAbsorption(rad, sinIncAngle, sinDiffAngle);
			  if (incidentDiffractionAngles[i][2] > 0.0) {
				  double transmission = alayer.getOverLayerAbsorption(rad) * factor;
//    System.out.println("transmission: " + transmission + " layer " + layerIndex);
				  if (transmission < 200)
					  expTransmission = Math.exp(-transmission);
				  if (absorptionLayer < 200)
					  expAbsorption = 1.0 - Math.exp(-absorptionLayer);
			  }

//	  System.out.println("Layer: " + alayer.toString() + " " + absorption);
			  if (absorption >= 0.0) {
//      System.out.println("abs = " + sinDiffAngle + " * " + alayer.getThicknessValue() + " / (" + absorption +
//          " * (" + sinIncAngle + " + " + sinDiffAngle + " )) * " +
//          expTransmission + " * " + expAbsorption);
				  radAbs[i] = 2.0 * sinDiffAngle * alayer.getThicknessValue() / (absorption * (sinIncAngle + sinDiffAngle)) *
						  expTransmission * expAbsorption * adataset.getMeanAbsorption();
			  } else
				  radAbs[i] = 1.0;
		  }
	  }

	  return radAbs;
  }

/*  public double getLayerAbsorption(RadiationType rad, int layerIndex, double pathK, double omega,
                                   double chi, double eta, DataFileSet adataset) {

    Layer alayer = getlayer(layerIndex);
    double absorption = alayer.getLayerAbsorption(rad) * pathK;
    double transmission = alayer.getOverLayerAbsorption(rad) * pathK;
//    System.out.println("transmission: " + transmission + " layer " + layerIndex);
    double expTransmission = 0.0;
    double expAbsorption = 0.0;
    if (transmission < 200)
      expTransmission = Math.exp(-transmission);
    if (absorption < 200)
      expAbsorption = Math.exp(-absorption);

    if (absorption != 0.0 && pathK != 0.0) {
      double coschi = MoreMath.cosd(chi);
      double coseta = MoreMath.cosd(eta);//     todo: this is not correct, compatibility old files
      double sinomega = MoreMath.sind(omega);

      return expTransmission * (1.0 - expAbsorption) / (absorption * coschi * coseta * sinomega / 2.0 /
          alayer.getThicknessValue()) * adataset.getMeanAbsorption();
    } else
      return 1.0;
  }*/

  public double getMeanAbsorption(RadiationType rad) {

    double absorption = 0.0;
    double thickness = 0.0;

    int numberoflayers = layersnumber();
    for (int j = 0; j < numberoflayers; j++) {
      Layer alayer = getlayer(j);
      absorption += alayer.getLayerAbsorption(rad);
      thickness += alayer.getThicknessValue();
//	    System.out.println("Thickness: " + thickness + " absorption " + absorption);
    }
    if (thickness != 0.0)
      absorption /= thickness;

    return absorption;
  }

	public double[] getAbsorption(RadiationType rad) {
		int radCount = rad.getLinesCount();
		double[] absorption = new double[radCount];
		double thickness = 0.0;

		int numberoflayers = layersnumber();
		for (int j = 0; j < numberoflayers; j++) {
			Layer alayer = getlayer(j);
			thickness += alayer.getThicknessValue();
			for (int i = 0; i < radCount; i++)
				absorption[i] += alayer.getLayerAbsorption(rad, i);
//	    System.out.println("Thickness: " + thickness + " absorption " + absorption);
		}
		if (thickness != 0.0)
			for (int i = 0; i < radCount; i++)
				absorption[i] /= thickness;

		return absorption;
	}

/*  public double getTotalLayerAbsorption(RadiationType rad) {

    double absorption = 0.0;
    double thickness = 0.0;

    int numberoflayers = layersnumber();
    for (int j = 0; j < numberoflayers; j++) {
      Layer alayer = getlayer(j);
      absorption += alayer.getLayerAbsorption(rad) * alayer.getThicknessValue() * 2;
      thickness += alayer.getThicknessValue();
    }
    if (thickness != 0.0)
      absorption /= thickness;

    return absorption; // * 10E3;
  }*/

  private boolean[] computeReflectionsPosition() {
    boolean[] phaseChanged = new boolean[phasesNumber()];
    for (int ki = 0; ki < phasesNumber(); ki++) {
      phaseChanged[ki] = false;
      Phase aphase = getPhase(ki);
      if (aphase.refreshPositions) {
//        System.out.println("Computing position");
        phaseChanged[ki] = true;
        aphase.refreshPositions = false;
        aphase.gethklNumber();
          for (int i = 0; i < activeDatasetsNumber(); i++) {
            if (getActiveDataSet(i) != null && getActiveDataSet(i).isDiffraction()) {
	            for (int j = 0; j < getActiveDataSet(i).activedatafilesnumber(); j++) {
                DiffrDataFile adatafile = getActiveDataSet(i).getActiveDataFile(j);
		            adatafile.computePosition(aphase);
	            }
            }
          }
      }
    }
    return phaseChanged;
  }

	private void computeLorentzPolarization(boolean[] phaseChanged) {
    for (int ki = 0; ki < phasesNumber(); ki++) {
      Phase aphase = getPhase(ki);
      if (phaseChanged[ki]) {
          for (int i = 0; i < activeDatasetsNumber(); i++) {
            if (getActiveDataSet(i) != null) {
              for (int j = 0; j < getActiveDataSet(i).activedatafilesnumber(); j++) {
                DiffrDataFile adatafile = getActiveDataSet(i).getActiveDataFile(j);
                adatafile.computeLorentzPolarization(aphase);
              }
            }
          }
      }
    }
  }

  private void computeScatteringFactors(boolean[] phaseChanged) {
    for (int ki = 0; ki < phasesNumber(); ki++) {
      if (phaseChanged[ki]) {
        Phase phase = getPhase(ki);
	      for (int i = 0; i < activeDatasetsNumber(); i++) {
		      DataFileSet dataset = getActiveDataSet(i);
		      if (dataset != null) {
			      dataset.reloadScatteringFactors(phase);
          }
        }
      }
    }
  }

  private void computeShapeAbsorptionCorrection() {
//    if (getActiveSubordinateModel(sampleShapeID).refreshComputation) { // todo it should include phase and layer absorption
//      System.out.println("Computing shape absorption.....");

      for (int i = 0; i < activeDatasetsNumber(); i++) {
        if (getActiveDataSet(i) != null && getActiveDataSet(i).getFluorescence().identifier == "none fluorescence") { // todo now only for diffraction
//          Instrument ainstrument = getActiveDataSet(i).getInstrument();
          for (int j = 0; j < getActiveDataSet(i).activedatafilesnumber(); j++) {
             DiffrDataFile adatafile = getActiveDataSet(i).getActiveDataFile(j);
	          for (int p = 0; p < phasesNumber(); p++) {
	          adatafile.computeShapeAbsorptionCorrection(getPhase(p));
//            int datafileIndex = adatafile.getIndex();
//            System.out.println("Computing for datafile number " + datafileIndex);
/*            ainstrument.computeShapeAbsorptionCorrection(adatafile, this, x, adatafile.dspacingbase,
                adatafile.energyDispersive, intensity);*/

//            System.out.println("Computed for datafile number " + datafileIndex);
	          }
          }
        }
      }
//      System.out.println("Finished computing shape absorption.....");


//    }

    getActiveSubordinateModel(sampleShapeID).refreshComputation = false;
  }

  private boolean[] computeInstrumentBroadening() {
    int[] numberofpeaks = new int[phasesNumber()];
    for (int i = 0; i < phasesNumber(); i++) {
      Phase aphase = getPhase(i);
      numberofpeaks[i] = aphase.gethklNumber();
    }
    boolean[] refreshDataset = new boolean[activeDatasetsNumber()];
      for (int i = 0; i < activeDatasetsNumber(); i++) {
        if (getActiveDataSet(i) != null) {
          Instrument ainstrument = getActiveDataSet(i).getInstrument();
          InstrumentBroadening ainstBroadening = ainstrument.getInstrumentBroadening();
          if (ainstBroadening.refreshComputation) {
//	          System.out.println("Refreshing Instrument broadening");
            for (int j = 0; j < getActiveDataSet(i).activedatafilesnumber(); j++) {
              DiffrDataFile adatafile = getActiveDataSet(i).getActiveDataFile(j);
              for (int ki = 0; ki < phasesNumber(); ki++) {
                Phase aphase = getPhase(ki);
                adatafile.computeInstBroadFactor(aphase);
              }
            }
            refreshDataset[i] = true;
            ainstBroadening.refreshComputation = false;
          }
        }
      }
    return refreshDataset;
  }

  private boolean[] computeSizeStrainBroadening() {
    boolean[] oneRefresh = new boolean[phasesNumber()];
//    int[] numberofpeaks = new int[phasesNumber()];
    for (int ki = 0; ki < phasesNumber(); ki++) {
      Phase aphase = getPhase(ki);
//      numberofpeaks[ki] = aphase.gethklNumber();
      if (aphase.refreshCrystMicrostrain) {
        oneRefresh[ki] = true;
        aphase.computehklSizeStrain();
	      for (int i = 0; i < activeDatasetsNumber(); i++) {
		      for (int j = 0; j < getActiveDataSet(i).activedatafilesnumber(); j++) {
			      DiffrDataFile adatafile = getActiveDataSet(i).getActiveDataFile(j);
			      adatafile.computeSizeStrainBroadening(aphase);
		      }
	      }
      } else
        oneRefresh[ki] = false;
    }

    return oneRefresh;
  }


  private void computeSampleBroadening(boolean[] refreshDataset, boolean[] refreshPhase) {
    for (int ki = 0; ki < phasesNumber(); ki++) {
      Phase aphase = getPhase(ki);
        for (int i = 0; i < activeDatasetsNumber(); i++) {
          if (getActiveDataSet(i) != null && (refreshDataset[i] || refreshPhase[ki])) {
            double wave = getActiveDataSet(i).getInstrument().getRadiationType().getMeanRadiationWavelength();
            for (int j = 0; j < getActiveDataSet(i).activedatafilesnumber(); j++) {
              DiffrDataFile adatafile = getActiveDataSet(i).getActiveDataFile(j);
             adatafile.computeSampleBroadening(aphase, wave);
            }
          }
        }
    }

  }

	private void computeFinalPositions() {
		for (int ki = 0; ki < phasesNumber(); ki++) {
			Phase aphase = getPhase(ki);
			aphase.refreshPositions = true;
				for (int i = 0; i < activeDatasetsNumber(); i++) {
					if (getActiveDataSet(i).isDiffraction())
					for (int j = 0; j < getActiveDataSet(i).activedatafilesnumber(); j++) {
						DiffrDataFile adatafile = getActiveDataSet(i).getActiveDataFile(j);
						adatafile.computePositionForStrained(aphase);
					}
				}
		}
	}

	public void computeAbsorptionTroughPath(RadiationType rad, double[][][] incidentAndDiffraction_angles, double[][] position,
                                          double[][] intensity, double toLambda) {
    // only an approximation

    double[] absorption = getAbsorption(rad);
    SampleShape sampleShape = (SampleShape) getActiveSubordinateModel(sampleShapeID);
    sampleShape.computeAbsorptionPath(incidentAndDiffraction_angles, absorption, position, intensity, toLambda);

  }

	public void computeAbsorptionTroughPath(RadiationType rad, double[][] incidentAndDiffraction_angles, double[] position,
	                                        double[] intensity, double toLambda) {
		// only an approximation

		double absorption = getMeanAbsorption(rad);
		SampleShape sampleShape = (SampleShape) getActiveSubordinateModel(sampleShapeID);
		sampleShape.computeAbsorptionPath(incidentAndDiffraction_angles, absorption, position, intensity, toLambda);

	}

	public double getAbsorptionTroughPath(RadiationType rad, double[][] incidentAndDiffraction_angles, double[] position,
                                        double toLambda) {
    double[] intensity = new double[1];
    intensity[0] = 1.0f;
    computeAbsorptionTroughPath(rad, incidentAndDiffraction_angles, position, intensity, toLambda);
    return intensity[0];
//    double absorption = 1.0;
//		if (useExp) {
//    if (arg1 < 200.0)
//      absorption = Math.exp(-arg1);
//    else
//      absorption = 0.0;
//		} else
//			absorption = 1.0 / arg1;
//    return absorption;
  }

  public double[][] getShapeAbsorptionPoleFigureGrid(Reflection refl, int numberofPoints, double maxAngle) {

    double[] sampleAngles = getSampleAngles(); //new double[3];
//		sampleAngles[0] = 0.0f; //getShapeOmegaD();
//		sampleAngles[1] = 0.0f; //getShapeChiD();
//		sampleAngles[2] = 0.0f; //getShapePhiD();
    if (activeDatasetsNumber() <= 0)
      return null;
    DataFileSet adataset = getActiveDataSet(0);
    Instrument ainstrument = adataset.getInstrument();
    RadiationType rad = ainstrument.getRadiationType();
    boolean isTOF = ainstrument.getMeasurement().isTOF();

    Geometry ageometry = ainstrument.getGeometry();
    double[] position = new double[1];
    position[0] = 0.0f;
    boolean dspacingbase = false;
    if (adataset.datafilesnumber() > 0)
      dspacingbase = adataset.getDataFile(0).dspacingbase;
//		double[] tilting_angles = adatafile.getTiltingAngle();

    double[][] PFreconstructed = new double[numberofPoints][numberofPoints];
    double tilting_angles[] = new double[4];
    if (dspacingbase) {  // don't know what to do
      position[0] = refl.d_space;
      tilting_angles[0] = ageometry.getThetaDetector(adataset.getDataFile(0), position[0]) / 2.0f;
    } else {
      position[0] = adataset.getDataFile(0).computeposition(refl.d_space, rad.getRadiationWavelength(0));
      position[0] = adataset.getDataFile(0).getCorrectedPosition(this, position[0], tilting_angles);
      tilting_angles[0] = position[0] / 2;

    }
    double toLambda = 0.0f;
    if (isTOF) {
      toLambda = (2.0 * MoreMath.sind(Math.abs(tilting_angles[0])));
    }

    double x, y, r;
    double dxy = 2.0 * maxAngle / (numberofPoints - 1);

    for (int i = 0; i < numberofPoints; i++)
      for (int j = 0; j < numberofPoints; j++) {
        x = j * dxy - maxAngle;
        y = i * dxy - maxAngle;
        r = Math.sqrt(x * x + y * y);
        if (r == 0.0) {
          tilting_angles[1] = 0.0f;
          tilting_angles[2] = 0.0f;
          double[][] angles = ageometry.getIncidentAndDiffractionAngles(adataset.getDataFile(0),
              tilting_angles, sampleAngles, position);

          PFreconstructed[i][j] = getAbsorptionTroughPath(rad, angles, position, toLambda);
        } else if (r <= maxAngle) {
          double phaseAng = Math.atan2(x, y);
          if (phaseAng < 0.0)
            phaseAng += Constants.PI2;
          tilting_angles[1] = 2.0f * (double) (Math.asin(r / Constants.sqrt2) * Constants.PITODEG);
          if (tilting_angles[1] < 0.0) {
            tilting_angles[1] = -tilting_angles[1];
            phaseAng += Constants.PI;
            while (phaseAng >= Constants.PI2)
              phaseAng -= Constants.PI2;
          }
          tilting_angles[2] = (double) (phaseAng * Constants.PITODEG);
          double[][] angles = ageometry.getIncidentAndDiffractionAngles(adataset.getDataFile(0),
              tilting_angles, sampleAngles, position);

          PFreconstructed[i][j] = getAbsorptionTroughPath(rad, angles, position, toLambda);
        } else {
/*          double phaseAng = Math.atan2(x, y);
          if (phaseAng < 0.0)
            phaseAng += Constants.PI2;
          tilting_angles[1] = 2.0f * (double) (Math.asin(maxAngle / Constants.sqrt2) * Constants.PITODEG);
          if (tilting_angles[1] < 0.0) {
            tilting_angles[1] = -tilting_angles[1];
            phaseAng += Constants.PI;
            while (phaseAng >= Constants.PI2)
              phaseAng -= Constants.PI2;
          }
          tilting_angles[2] = (double) (phaseAng * Constants.PITODEG);
          double[][] angles = ageometry.getIncidentAndDiffractionAngles(adataset.getDataFile(0),
                  tilting_angles, sampleAngles, position);
*/
          PFreconstructed[i][j] = Double.NaN; //getAbsorptionTroughPath(rad, angles, position, toLambda);
        }
      }
    return PFreconstructed;
  }

  public double[] getLayerParams() {
    int numberoflayers = layersnumber();
    int paramsNumber = numberoflayers * 3;
    double[] params = new double[paramsNumber];
    int np = 0;
    for (int j = 1; j < numberoflayers - 1; j++) { // we skip the air and the substrate
      Layer alayer = getlayer(j);
      params[np++] = alayer.getThicknessInAngstrom();
      params[np++] = alayer.getCriticalQcValue();
      params[np++] = alayer.getRoughnessValue();
    }
    return params;
  }

  public void setLayerParams(double[] params) {
    int numberoflayers = layersnumber();
    int np = 0;
    for (int j = 1; j < numberoflayers - 1; j++) {
      Layer alayer = getlayer(j);
      alayer.setThickness(params[np++]);
      alayer.setCriticalQc(params[np++]);
      alayer.setRoughness(params[np++]);
      alayer.refreshAll(false);
    }
  }

  public boolean workoutHeterostructure() {
    boolean result = false;

    DataFileSet[] tmpDataSet = null;
    int numberOfDatasets = 0;
    for (int i = 0; i < activeDatasetsNumber(); i++) {
      DataFileSet adataset = getActiveDataSet(i);
      if (!(adataset.getReflectivityMethod().toLowerCase().startsWith("none"))) {
        result = true;
        numberOfDatasets++;
      }
    }
    tmpDataSet = new DataFileSet[numberOfDatasets];
    numberOfDatasets = 0;
    for (int i = 0; i < activeDatasetsNumber(); i++) {
      DataFileSet adataset = getActiveDataSet(i);
      if (!(adataset.getReflectivityMethod().toLowerCase().startsWith("none"))) {
        tmpDataSet[numberOfDatasets] = adataset;
        numberOfDatasets++;
      }
    }

    if (result) {
      result = ((LayerSolutionMethod) getActiveSubordinateModel(layerSolutionMethodID)).
          workoutHeterostructure(tmpDataSet);
    }

    return result;
  }

  public void computeSpectra(DataFileSet[] tmpDataSet) {
	  Texture.rotateAlpha = MaudPreferences.getDouble("Texture.phiZero", -90.0);
    for (int i = 0; i < tmpDataSet.length; i++)
      tmpDataSet[i].computeSpectra(this);
  }

/*	public double getShapeOmegaD() {
    SampleShape sampleShape = (SampleShape) getActiveSubordinateModel(sampleShapeID);
    return sampleShape.getShapeOmegaD();
  }

  public double getShapeChiD() {
    SampleShape sampleShape = (SampleShape) getActiveSubordinateModel(sampleShapeID);
    return sampleShape.getShapeChiD();
  }

  public double getShapePhiD() {
    SampleShape sampleShape = (SampleShape) getActiveSubordinateModel(sampleShapeID);
    return sampleShape.getShapePhiD();
  }*/

  public void fittingFileOutput(boolean addStatisticalError, boolean phaseOutput) {
    int numberdataset = activeDatasetsNumber();

    for (int i = 0; i < numberdataset; i++) {
      DataFileSet adataset = getActiveDataSet(i);
      if (adataset != null) {
        adataset.fittingFileOutput(addStatisticalError, phaseOutput);
      } else
        System.out.println("dataset not found: " + getDataSet(i));
    }
  }

  public void freeAllBackgroundParameters() {
    for (int i = 0; i < datasetsNumber(); i++) {
      DataFileSet adataset = getDataSet(i);
      if (adataset != null)
        adataset.freeAllBackgroundParameters();
    }
  }

  public void freeAllScaleParameters() {
    for (int i = 0; i < datasetsNumber(); i++) {
      DataFileSet adataset = getDataSet(i);
      if (adataset != null)
        adataset.freeAllScaleParameters();
    }
    int numberoflayers = layersnumber();
    for (int j = 0; j < numberoflayers; j++)
      getlayer(j).freeAllScaleParameters();
  }

  public boolean freeAllBasicParameters(double limit) {
    boolean done = false;
    for (int i = 0; i < phasesNumber(); i++)
      if (isPhasePresent(i, limit))
        done = getPhase(i).freeAllBasicParameters() || done;
    for (int i = 0; i < datasetsNumber(); i++) {
      DataFileSet adataset = getDataSet(i);
      if (adataset != null)
        done = adataset.freeAllBasicParameters() || done;
    }
    return done;
  }

  public void freeAllMicroParameters(double limit) {
    for (int i = 0; i < phasesNumber(); i++)
      if (isPhasePresent(i, limit))
        getPhase(i).freeAllMicroParameters();
  }

  public void freeAllCrystalParameters(double limit) {
    for (int i = 0; i < phasesNumber(); i++)
      if (isPhasePresent(i, limit))
        getPhase(i).freeAllCrystalParameters();
  }

  public void freeAllTextureParameters(double limit) {
    for (int i = 0; i < phasesNumber(); i++)
      if (isPhasePresent(i, limit))
        getPhase(i).freeAllTextureParameters();
  }

  public void fixAllTextureParametersPreserveBound() {
    for (int i = 0; i < phasesNumber(); i++)
      getPhase(i).fixAllTextureParametersPreserveBound();
  }

  public void freeAllStrainParameters(double limit) {
    for (int i = 0; i < phasesNumber(); i++)
      if (isPhasePresent(i, limit))
        getPhase(i).freeAllStrainParameters();
  }

  public void fixAllStrainParametersPreserveBound() {
    for (int i = 0; i < phasesNumber(); i++)
      getPhase(i).fixAllStrainParametersPreserveBound();
  }

  public void fixAllBackgroundParametersPreserveBound() {
    for (int i = 0; i < datasetsNumber(); i++) {
      DataFileSet adataset = getDataSet(i);
      if (adataset != null)
        adataset.fixAllBackgroundParametersPreserveBound();
    }
  }

  public void freeAllCountMonitors() {
    for (int i = 0; i < datasetsNumber(); i++) {
      DataFileSet adataset = getDataSet(i);
      if (adataset != null)
        adataset.freeAllCountMonitors();
    }
  }

  public void fixAllCountMonitorsPreserveBound() {
    for (int i = 0; i < datasetsNumber(); i++) {
      DataFileSet adataset = getDataSet(i);
      if (adataset != null)
        adataset.fixAllCountMonitorsPreserveBound();
    }
  }

  public void edit(Frame aframe) {
    (new SampleD(aframe, this)).setVisible(true);
  }

  public void boundMonitorsByBank() {
    for (int i = 0; i < datasetsNumber(); i++) {
      DataFileSet adataset = getDataSet(i);
      if (adataset != null)
        adataset.boundMonitorsByBank();
    }
  }

  public void boundMonitorsByAngles(boolean[] useAngle) {
    for (int i = 0; i < datasetsNumber(); i++) {
      DataFileSet adataset = getDataSet(i);
      if (adataset != null)
        adataset.boundMonitorsByAngles(useAngle);
    }
  }

  public void writeSimpleResults(BufferedWriter out, int ph) throws IOException {

//    FilePar aparFile = getFilePar();
    for (int i = 0; i < layersnumber(); i++) {
      Layer selectedlayer = getlayer(i);
      out.write(Fmt.format(Math.abs(selectedlayer.getPhaseQuantity(ph).getValueD()) * 100) + "\t");
      out.write(Fmt.format(selectedlayer.getVolumeErrorPhaseQuantity(ph) * 100) + "\t");
      out.write(Fmt.format(selectedlayer.getWeightedPhaseQuantity(ph) * 100) + "\t");
      out.write(Fmt.format(selectedlayer.getWeightedErrorPhaseQuantity(ph) * 100) + "\t");
    }
  }

  public void writeSimpleResultsFirstLine(BufferedWriter out, int ph) throws IOException {

//    FilePar aparFile = getFilePar();
    for (int i = 0; i < layersnumber(); i++) {
//      Layer selectedlayer = getlayer(i);
//        Phase aphase = aparFile.getPhase(ph);
      out.write("Vol.(%)" + "\t");
      out.write("error(%)" + "\t");
      out.write("Wt.(%)" + "\t");
      out.write("error(%)" + "\t");
    }
  }

  public double getPhaseTotalVolumeFraction(int index) {
    double total = 0.0;
    for (int i = 0; i < layersnumber(); i++) {
      Layer selectedlayer = getlayer(i);
      total += Math.abs(selectedlayer.getPhaseQuantity(index).getValueD());
    }

    return total;
  }

  public double[] getPhaseTotalQuantity(int index) {
    double[] total = new double[4];
    for (int i = 0; i < layersnumber(); i++) {
      Layer selectedlayer = getlayer(i);
      total[0] += Math.abs(selectedlayer.getPhaseQuantity(index).getValueD() * 100);
      total[1] += Math.abs(selectedlayer.getVolumeErrorPhaseQuantity(index) * 100);
      total[2] += Math.abs(selectedlayer.getWeightedPhaseQuantity(index) * 100);
      total[3] += Math.abs(selectedlayer.getWeightedErrorPhaseQuantity(index) * 100);
    }

    return total;
  }

  public void setZeroPhase(int index) {
    for (int i = 0; i < layersnumber(); i++) {
      getlayer(i).setZeroPhase(index);
    }
  }

  public Vector oldEnabledDatasets = new Vector(0, 1);

  public void setLoop(Vector avector, int element) {

    super.setLoop(avector, element);
    // in case we load old parameter files

    int loopitem = avector.size();
    int i = 0;
    if (loopitem > 0) {
      while (i < loopitem) {
        CIFItem item = (CIFItem) avector.elementAt(i++);
        if (item.cif.equalsIgnoreCase("_pd_meas_dataset_id")) {
          oldEnabledDatasets.add(item.thestring);
        }
      } // end of while (i < loopitem)
    }
  } // end of setLoop method

  public void checkEnabledDatasets(DataFileSet data) {
    String datasetname = data.getLabel();
    data.setEnabled(false);
    for (int i = 0; i < oldEnabledDatasets.size(); i++)
      if (((String) oldEnabledDatasets.elementAt(i)).equalsIgnoreCase(datasetname))
        data.setEnabled(true);
  }

  public void readOldParFile(Reader in, String dir, String name) {

//					System.out.println("Start loading");
    String thecife;
    int newtoken, tokentype;
    XRDcat theobj = null;
    Vector instrumentsForDataset = new Vector(0, 0);
    Vector activeDataset = new Vector(0, 0);

    if (in != null) {

      CIFtoken ciffile = new CIFtoken(in);

//					System.out.println("The try loop");
      try {
        do {
          tokentype = ciffile.nextToken();
          switch (tokentype) {
            case CIFtoken.TT_DATA:
              // new block of data
              break;
            case CIFtoken.TT_GLOB:
              // global data
              theobj = this;
              break;
            case CIFtoken.TT_INST:
              // instrumental data

//              getInstrumentsList().addItem(new Instrument(this, ciffile.thestring));
//              theobj = (XRDcat) getInstrumentsList().lastElement();
              break;
            case CIFtoken.TT_DATASET:
              // data
              getDatasetsList().addItem(new DataFileSet(this, ciffile.thestring));
              theobj = (XRDcat) getDatasetsList().lastElement();
              break;
            case CIFtoken.TT_SAMPLE:
              // sample
              theobj = null;
              break;
            case CIFtoken.TT_BOUND:
              // parameter binding
              tokentype = CIFtoken.TT_EOF;
              break;
            case CIFtoken.TT_PHASE:
              // phase data
              getPhasesList().addItem(new Phase(this, ciffile.thestring));
//              phaseAdded();
              theobj = (XRDcat) getPhasesList().lastElement();
              break;
            case CIFtoken.TT_SUBO:
              // subordinate loop
/*          if (Constants.testing) {
            System.out.println("SUB:" + ciffile.thestring);
            System.out.println(theobj);
          }*/
              if (theobj != null)
                theobj.readtheobject(ciffile);
              break;
            case CIFtoken.TT_LOOP:
              // data loop
              boolean datasetID = false;
              Vector itemlistv = new Vector(0, 1);
              Vector cifelistv = new Vector(0, 1);
              newtoken = ciffile.nextToken();
//							System.out.println(ciffile.thestring);
              while (newtoken == CIFtoken.TT_CIFE) {
                itemlistv.addElement(ciffile.thestring);
                if (theobj == null)
                  datasetID = ciffile.thestring.equalsIgnoreCase("_pd_meas_dataset_id") || datasetID;
                newtoken = ciffile.nextToken();
//								System.out.println(ciffile.thestring);
              }
              int loopitem = itemlistv.size();
              if (loopitem > 0) {
                while (FilePar.isValidToken(newtoken)) {
                  cifelistv.addElement(new CIFItem((String) itemlistv.elementAt(0),
                      ciffile));
                  if (datasetID && theobj == null) {
                    activeDataset.addElement(ciffile.thestring);
                  }
                  for (int i = 1; i < loopitem; i++) {
                    newtoken = ciffile.nextToken();
//										System.out.println(ciffile.thestring);
                    cifelistv.addElement(new CIFItem((String) itemlistv.elementAt(i),
                        ciffile));
                  }
                  newtoken = ciffile.nextToken();
//									System.out.println(ciffile.thestring);
                }
              }
              ciffile.pushBack();
//							System.out.println("Pushback: " + ciffile.thestring);
              if (theobj != null)
                theobj.setLoop(cifelistv, loopitem);

              cifelistv.removeAllElements();
              itemlistv.removeAllElements();
              break;
            case CIFtoken.TT_CIFE:
              // CIF item
              thecife = new String(ciffile.thestring);
              newtoken = ciffile.nextToken();
//							System.out.println(ciffile.thestring);
              if (FilePar.isValidToken(newtoken)) {
                if (theobj != null) {
                  theobj.setField(thecife, new CIFItem(thecife, ciffile));
                  if (theobj instanceof DataFileSet && thecife.equalsIgnoreCase("_diffrn_measurement_device")) {
                    instrumentsForDataset.add(ciffile.thestring);
//                    instrumentsForDataset.add("Diffraction Instrument");
                  }
                }
              } else {
                ciffile.pushBack();
//								System.out.println("Pushback: " + ciffile.thestring);
              }
              break;
            default: {
            }
          }
        } while (tokentype != CIFtoken.TT_EOF);
      } catch (IOException ioe) {
        System.out.println("IO exception");
      }

      try {
        in.close();
      } catch (IOException ioe) {
        System.out.println("IO exception in closing the file");
      }
    }
    // Reading instruments now
    in = Misc.getReader(dir, name);

    if (in != null) {

      CIFtoken ciffile = new CIFtoken(in);

//					System.out.println("The try loop");
      theobj = null;
      try {
        do {
          tokentype = ciffile.nextToken();
          switch (tokentype) {
            case CIFtoken.TT_DATA:
              // new block of data
              break;
            case CIFtoken.TT_GLOB:
              // global data
              break;
            case CIFtoken.TT_INST:
              // instrumental data
              int index = 0;
              for (int i = 0; i < datasetsNumber(); i++) {
                String inst = (String) instrumentsForDataset.elementAt(i);
                if (inst.equalsIgnoreCase(ciffile.thestring)) {
                  index = i;
//                  System.out.println("Set instrument " + i + " " + ciffile.thestring);
                }
              }
              getDataSet(index).setNewInstrument(0);
              theobj = getDataSet(index).getInstrument();
              theobj.setLabel("Diffraction Instrument");
              break;
            case CIFtoken.TT_DATASET:
              // data
              tokentype = CIFtoken.TT_EOF;
              break;
            case CIFtoken.TT_SAMPLE:
              // sample
              tokentype = CIFtoken.TT_EOF;
              break;
            case CIFtoken.TT_PHASE:
              // phase data
              tokentype = CIFtoken.TT_EOF;
              break;
            case CIFtoken.TT_SUBO:
              // subordinate loop
/*          if (Constants.testing) {
            System.out.println("SUB:" + ciffile.thestring);
            System.out.println(theobj);
          }*/
              if (theobj != null)
                theobj.readtheobject(ciffile);
              break;
            case CIFtoken.TT_LOOP:
              // data loop
              Vector itemlistv = new Vector(0, 1);
              Vector cifelistv = new Vector(0, 1);
              newtoken = ciffile.nextToken();
//							System.out.println(ciffile.thestring);
              while (newtoken == CIFtoken.TT_CIFE) {
                itemlistv.addElement(ciffile.thestring);
                newtoken = ciffile.nextToken();
//								System.out.println(ciffile.thestring);
              }
              int loopitem = itemlistv.size();
              if (loopitem > 0) {
                while (FilePar.isValidToken(newtoken)) {
                  cifelistv.addElement(new CIFItem((String) itemlistv.elementAt(0),
                      ciffile));
                  for (int i = 1; i < loopitem; i++) {
                    newtoken = ciffile.nextToken();
//										System.out.println(ciffile.thestring);
                    cifelistv.addElement(new CIFItem((String) itemlistv.elementAt(i),
                        ciffile));
                  }
                  newtoken = ciffile.nextToken();
//									System.out.println(ciffile.thestring);
                }
              }
              ciffile.pushBack();
//							System.out.println("Pushback: " + ciffile.thestring);
              if (theobj != null)
                theobj.setLoop(cifelistv, loopitem);
              cifelistv.removeAllElements();
              itemlistv.removeAllElements();
              break;
            case CIFtoken.TT_CIFE:
              // CIF item
              thecife = new String(ciffile.thestring);
              newtoken = ciffile.nextToken();
//							System.out.println(ciffile.thestring);
              if (FilePar.isValidToken(newtoken)) {
                if (theobj != null)
                  theobj.setField(thecife, new CIFItem(thecife, ciffile));
              } else {
                ciffile.pushBack();
//								System.out.println("Pushback: " + ciffile.thestring);
              }
              break;
            default: {
            }
          }
        } while (tokentype != CIFtoken.TT_EOF);
      } catch (IOException ioe) {
        System.out.println("IO exception");
      }

      try {
        in.close();
      } catch (IOException ioe) {
        System.out.println("IO exception in closing the file");
      }

    }
    instrumentsForDataset.removeAllElements();
    instrumentsForDataset = null;
    if (activeDataset.size() > 0) {
      for (int j = 0; j < datasetsNumber(); j++) {
        getDataSet(j).setEnabled(false);
      }
      for (int i = 0; i < activeDataset.size(); i++) {
        String activeData = (String) activeDataset.elementAt(i);
        for (int j = 0; j < datasetsNumber(); j++) {
          if (getDataSet(j).toXRDcatString().equalsIgnoreCase(activeData)) {
            getDataSet(j).setEnabled(true);
          }
        }
      }
    }
  }

  public void loadPhase(String filename, boolean b) {
    getFilePar().loadingFile = true;
    CIFParser phaseParser = (new CIFParser(filename, null, this, "Phase"));
    Object[] aphase = phaseParser.getMainCat();
    if (aphase != null) {
      for (int i = 0; i < aphase.length; i++) {
        if (aphase[i] != null) {
          addPhase((Phase) aphase[i]);
          ((Phase) aphase[i]).fixAllParameters();
          ((Phase) aphase[i]).moveAtomsToStructureModel();
        }
      }
      getFilePar().loadingFile = false;
      notifyUpObjectChanged(this, Constants.OBJECT_ADDED);
      refreshAll(false);
    }
    getFilePar().loadingFile = false;
  }

  public void transferAnglesToMeasurement() {
    for (int i = 0; i < datasetsNumber(); i++) {
      DataFileSet adataset = getDataSet(i);
      if (adataset != null) {
	      double[] angles = new double[DiffrDataFile.maxAngleNumber];
	      angles[0] = getomega().getValueD();
	      angles[1] = getchi().getValueD();
			  angles[2] = getphi().getValueD();
	      adataset.addAnglesToMeasurement(angles);
      }
    }
  }

  public void setReplaceDatafile(boolean b) {
    for (int i = 0; i < datasetsNumber(); i++)
      getDataSet(i).setReplaceDatafile(b);
  }

  public void setAutomaticPolynomialBackground(boolean b) {
    for (int i = 0; i < datasetsNumber(); i++)
      getDataSet(i).setAutomaticPolynomialBackground(b);
  }

	public int getIndexOf(Phase phase) {
		for (int i = 0; i < phasesNumber(); i++)
			if (phase == getPhase(i))
				return i;
		return -1;
	}

	public int getNumberOfTexturePoints(Phase phase, int index) {
		int totalPoints = 0;
		for (int i = 0; i < activeDatasetsNumber(); i++) {
			totalPoints += getActiveDataSet(i).getNumberOfTexturePoints(phase, index);
		}
		return totalPoints;
	}

  public void exportExperimentalComputedData(BufferedWriter output) {
    int datasetnumber = activeDatasetsNumber();

    for (int i = 0; i < datasetnumber; i++) {
      DataFileSet tmpDatafileset = getActiveDataSet(i);
      if (tmpDatafileset != null)
        tmpDatafileset.exportExperimentalComputedData(output);
    }
  }

}

