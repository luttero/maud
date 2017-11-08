/*
 * @(#)XRDcat.java created 01/12/1996 Mesiano
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

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.interfaces.BaseFactoryObject;
import it.unitn.ing.rista.interfaces.basicObj;
import it.unitn.ing.rista.io.cif.CIFItem;
import it.unitn.ing.rista.io.cif.CIFtoken;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.util.function.PolynomialFunction;

import java.awt.*;
import java.io.*;
import java.lang.reflect.Constructor;
import java.net.URL;
import java.util.Vector;
import java.util.Enumeration;

/**
 *  The XRDcat is a class that implements a basic physical object to
 *  compute some properties, part of or anything has an influence on a
 *  spectrum.
 *
 *
 * @version $Revision: 1.25 $, $Date: 2006/12/04 14:30:04 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class XRDcat extends BaseFactoryObject implements basicObj, Cloneable {

  public boolean mustRemoved = false;

  private XRDcat parent;
  public String thelabel;

  public String[] diclist;
  public String[] diclistRealMeaning;
  public String[] classlist;
  public String[] classlists;
  public boolean[] pivotrequired;

  public int Nstring = 0, Nstringloop = 0, Nparameter = 0, Nparameterloop = 0,
  Nsubordinate = 0, Nsubordinateloop = 0; //, Narray = 0, Narrayloop = 0;
  public int totstring, totstringloop, totparameter, totparameterloop,
  totsubordinate, totsubordinateloop; //, totarray, totarrayloop;

  protected String stringField[];
  protected ListVector stringloopField[];
  public Parameter parameterField[];
  public ListVector parameterloopField[];
  public XRDcat subordinateField[];
  public ListVector subordinateloopField[];
//  public double valuesArray[] = null;
//  public Vector valuesLoopArray[] = null;
	protected double parameterValues[] = null;
	protected int numberOfLoopParameters[] = null;
	protected Vector parameterLoopValuesVector = null;

  public Vector parametersV;

  public Vector[] nsubordSuperclass;
  public Vector[] nsubordloopSuperclass;
  public boolean refreshComputation = true;
  public boolean refreshPrepareComputation = true;
  public boolean refreshComputeBefore = true;
  public boolean refreshComputeAll = true;
  public boolean refreshComputeAfter = true;
  public boolean refreshCloseComputation = true;
  public boolean refreshPreparePartialComputation = true;
  public boolean refreshComputePartial = true;
  public boolean refreshClosePartialComputation = true;
  public boolean isAbilitatetoRefresh = false;
  public boolean isReadingFromFile = false;
  public boolean automaticOutput = false;
  public boolean initialized = false;
  public boolean isImportingFromCIF = false;

  public boolean autoDialog = false;

  private static final String[] CIFtoReplace = {"entrophy", "_riet_meas_datafile_bank",
                                                   "_atom_site_U_iso_or_equiv",
		"_atom_site_aniso_U_11", "_atom_site_aniso_U_22", "_atom_site_aniso_U_33",
		"_atom_site_aniso_U_23", "_atom_site_aniso_U_13", "_atom_site_aniso_U_12",
    "_riet_Z_molecula_number", "_pd_phase_id", "_pd_proc_ls_prof_R_factor",
    "_pd_proc_ls_prof_wR_factor", "_pd_proc_ls_prof_wR_expected", "_maud_proc_ls_minimize_quantity",
    "_pd_proc_info_author_name", "_pd_proc_title", "_riet_par_distribution_strain_slope",
    "_riet_par_background_peak_heigth", "_texture_spherical_component_hwhm", "_texture_fiber_component_hwhm",
    "_riet_par_spec_displac_y_2R", "_instrument_detector_gas_fluorescence_intensity",
		  "_pd_meas_orientation_omega", "_pd_meas_orientation_chi", "_pd_meas_orientation_phi", "_pd_meas_orientation_eta",
		  "_pd_meas_orientation_2theta"};
  private static final String[] replacingCIF = {"entropy", "_pd_meas_detector_id",
                                                   "_atom_site_B_iso_or_equiv",
		"_atom_site_aniso_B_11", "_atom_site_aniso_B_22", "_atom_site_aniso_B_33",
		"_atom_site_aniso_B_23", "_atom_site_aniso_B_13", "_atom_site_aniso_B_12",
    "_cell_formula_units_Z", "_chemical_formula_sum", "_refine_ls_R_factor_all",
    "_refine_ls_wR_factor_all", "_refine_ls_goodness_of_fit_all", "_refine_ls_weighting_scheme",
    "_publ_contact_author_name", "_publ_section_title", "_riet_par_distribution_strain_decay",
    "_riet_par_background_peak_height", "_texture_spherical_component_fwhm", "_texture_fiber_component_fwhm",
      "_riet_par_spec_displac_y_2R", "_instrument_detector_air_fluorescence_intensity",
		  "_pd_meas_angle_omega", "_pd_meas_angle_chi", "_pd_meas_angle_phi", "_pd_meas_angle_eta",
		  "_pd_meas_angle_2theta"};
  private static final String[] stringToReplace = {"Entrophy", "refl. Matrix method",
                                                   "Hippo specimen precession error",
                                                   "No specimen precession error",
                                                   "Default Instrument",
                                                   "Hybrid Refinement",
                                                   "Hybrid MonteCarlo Refinement", "flat_sheet",
                                                   "exact-Delf", "exact-EDXRF broadening", "exact-EDXRF channel calibration",
		                                               "exact-XRF/XRD", "exact-Qualitative EDXRF", "exact-Qualitative XRF",
                                                   "arbitrary sf", "none sf extractor"};
  private static final String[] replacingString = {"Entropy", "Matrix method",
                                                   "Hippo precession",
                                                   "No precession",
                                                   "Diffraction Instrument",
                                                   "Lamarckian Refinement",
                                                   "Lamarckian MonteCarlo Refinement", "No shape",
		                                               "Delft", "XRF/EDXRF broadening", "XRF/EDXRF channel calibration",
		                                               "XRF instrument", "Quantitative EDXRF", "Quantitative XRF",
                                                   "extracted sf", "Le Bail"};

  // List of CIF entries for parameters that has been removed but can be in old files
  public static final String[] removedParameterList = {"_riet_par_spec_displac_x", "_riet_par_spec_displac_y",
                                                       "_riet_par_spec_displac_z", "_pd_spec_absbkg_scale_factor"};
  public static final String[] removedStringList169 = {"_exptl_absorpt_cryst_size"};

  public XRDcat(XRDcat obj, String alabel) {
    super();

    if (alabel.equals(""))
      setLabel("unknown");
    else
      setLabel(alabel);
    setParent(obj);
    parametersV = new Vector(0, 1);
  }

  public XRDcat(XRDcat obj) {
    this(obj, "unknown");
  }

  public XRDcat(String alabel) {
    this(null, alabel);
  }

  public XRDcat() {
    super();
  }

  public void initXRD() {
    initConstant();
    computeConstant();
    initDictionary();
    initParameters();
    isAbilitatetoRefresh = true;
//		notifyUpObjectChanged(null);
  }

  public void initConstant() {
    //	to be overrided in subclasses
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void computeConstant() {
    totstring = Nstring;
    totstringloop = totstring + Nstringloop;
    totparameter = totstringloop + Nparameter;
    totparameterloop = totparameter + Nparameterloop;
    totsubordinate = totparameterloop + Nsubordinate;
    totsubordinateloop = totsubordinate + Nsubordinateloop;
//    totarray = totsubordinateloop + Narray;
//    totarrayloop = totarray + Narrayloop;

    diclist = new String[totsubordinateloop];
    diclistRealMeaning = new String[totsubordinateloop];
    classlist = new String[Nsubordinateloop];
    classlists = new String[Nsubordinate];
    pivotrequired = new boolean[Nsubordinateloop];
    stringField = new String[Nstring];
    stringloopField = new ListVector[Nstringloop];
    parameterField = new Parameter[Nparameter];
    parameterloopField = new ListVector[Nparameterloop];
    subordinateField = new XRDcat[Nsubordinate];
    subordinateloopField = new ListVector[Nsubordinateloop];
//    valuesArray = new double[Narray];
//    valuesLoopArray = new Vector[Narrayloop];

    parameterValues = new double[Nparameter];
    parameterLoopValuesVector = new Vector(Nparameterloop, 1);
    numberOfLoopParameters = new int[Nparameterloop];

  }

  public void initDictionary() {
  }

  public void initParameters() {
    int i;

    for (i = 0; i < Nstring; i++)
      stringField[i] = "";
    for (i = 0; i < Nstringloop; i++)
      stringloopField[i] = new ListVector(0, 1, this);
    for (i = 0; i < Nparameterloop; i++)
      parameterloopField[i] = new ListVector(0, 1, this);
    for (i = 0; i < Nsubordinateloop; i++)
      subordinateloopField[i] = new ListVector(0, 1, this);
    for (i = 0; i < Nsubordinateloop; i++)
      pivotrequired[i] = false;
    for (i = 0; i < Nparameter; i++)
      parameterField[i] = new Parameter(this, getParameterString(i), 0);

    int nsubord = totsubordinate - totparameterloop;
    nsubordSuperclass = new Vector[nsubord];
    for (i = 0; i < nsubord; i++) {
      if (classlists[i].startsWith("superclass")) {
        nsubordSuperclass[i] = Constants.getClassList(filterClass(classlists[i]));
//		  	System.out.println(nsubordSuperclass[i]);
      } else
        nsubordSuperclass[i] = null;
    }

    int nsubordloop = totsubordinateloop - totsubordinate;
    nsubordloopSuperclass = new Vector[nsubordloop];
    for (i = 0; i < nsubordloop; i++) {
      if (classlist[i].startsWith("superclass"))
        nsubordloopSuperclass[i] = Constants.getClassList(filterClass(classlist[i]));
      else
        nsubordloopSuperclass[i] = null;
    }

/*    for (i = 0; i < Narray; i++)
    	valuesArray[i] = 0.0;
    for (i = 0; i < Nlooparray; i++)
    	valuesLoopArray[i] = new Vector(0, 1);*/
	  refreshComputation = true;
  }

  public void initializeAsNew() {
    if (initialized)
      return;
    initialized = true;
  }

  public boolean isObjectSupported(basicObj source, ListVector list) {
    for (int i = 0; i < Nsubordinateloop; i++)
      if (list.equals(subordinateloopField[i]))
        if (Misc.areClassCompatibles(classlist[i], source.getClass()))
          return true;
    return false;
  }

  public boolean isObjectSupported(ListVector list) {
    for (int i = 0; i < Nparameterloop; i++)
      if (list.equals(parameterloopField[i]))
        return true;
    return false;
  }

  public int getsubordClassNumber(int subord) {
    if (nsubordSuperclass[subord] != null)
      return nsubordSuperclass[subord].size() / 3;
    else
      return 0;
  }

  public int getsubordloopClassNumber(int subord) {
    if (nsubordloopSuperclass[subord] != null)
      return nsubordloopSuperclass[subord].size() / 3;
    else
      return 0;
  }

  public String getsubordClassName(int subord, int element) {
    if (nsubordSuperclass[subord] != null)
      return (String) nsubordSuperclass[subord].elementAt(element * 3);
    else
      return "";
  }

  public String getsubordIdentifier(int subord, int element) {
    if (nsubordSuperclass[subord] != null)
      return (String) nsubordSuperclass[subord].elementAt(element * 3 + 1);
    else
      return "";
  }

  public void setSubordinateModel(int identifierNumber, String value) {
    if (subordinateField[identifierNumber] == null || !getSubordinateModel(identifierNumber).equals(value))
      setsubordinateField(identifierNumber, value);
  }

  public void setSubordinateModel(int identifierNumber, int number) {
    setSubordinateModel(identifierNumber, getsubordIdentifier(identifierNumber, number));
  }

  public XRDcat getActiveSubordinateModel(int identifierNumber) {
    if (subordinateField[identifierNumber] == null)
      setSubordinateModel(identifierNumber, 0);
    return subordinateField[identifierNumber];
  }

  public String getSubordinateModel(int identifierNumber) {
    return getActiveSubordinateModel(identifierNumber).identifier;
  }

  public int subordinateModelsNumber(int identifierNumber) {
    return getsubordClassNumber(identifierNumber);
  }

  public String getsubordID(int subord, int element) {
    if (nsubordSuperclass[subord] != null)
      return (String) nsubordSuperclass[subord].elementAt(element * 3 + 2);
    else
      return "";
  }

  public String getsubordloopClassName(int subord, int element) {
    if (nsubordloopSuperclass[subord] != null)
      return (String) nsubordloopSuperclass[subord].elementAt(element * 3);
    else
      return "";
  }

  public String getsubordloopIdentifier(int subord, int element) {
    if (nsubordloopSuperclass[subord] != null)
      return (String) nsubordloopSuperclass[subord].elementAt(element * 3 + 1);
    else
      return "";
  }

  public String getsubordloopID(int subord, int element) {
    if (nsubordloopSuperclass[subord] != null)
      return (String) nsubordloopSuperclass[subord].elementAt(element * 3 + 2);
    else
      return "";
  }

  public ListVector getList(int index) {
    return subordinateloopField[index];
  }

  public XRDcat getParent() {
    return parent;
  }

  public void setParent(XRDcat obj) {
    parent = obj;
  }

  public boolean automaticOutput() {
    return automaticOutput;
  }

  public void setAutomaticOutput(boolean status) {
    automaticOutput = status;
  }

  public void writeResults(BufferedWriter out) throws IOException {
    if (automaticOutput) {
      writeObjectResults(out);
    }
    writeParameterResults(out);
  }

  public void writeObjectResults(BufferedWriter out) throws IOException {
    out.write(getLabel());
    out.write("\t");
  }

  public void writeParameterResults(BufferedWriter out) throws IOException {
    Vector parV = getParameterVector(true, false);
    int totnumberParameters = parV.size();
    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parV.elementAt(i);
      if (apar.automaticOutput) {
        out.write(apar.getValue());
        out.write("\t");
        if (apar.getFree()) {
          out.write(apar.getError());
          out.write("\t");
        }
      }
    }
    for (int i = 0; i < Nsubordinate; i++) {
      subordinateField[i].writeResults(out);
    }
    for (int i = 0; i < Nsubordinateloop; i++) {
      for (int j = 0; j < numberofelementSubL(i); j++) {
        ((XRDcat) subordinateloopField[i].elementAt(j)).writeResults(out);
      }
    }

  }

  public void writeResultsFirstLine(BufferedWriter out) throws IOException {
    if (automaticOutput) {
      writeObjectResultsFirstLine(out);
    }
    writeParameterResultsFirstLine(out);
  }

  public void writeObjectResultsFirstLine(BufferedWriter out) throws IOException {
    out.write("Object");
    out.write("\t");
  }

  public void writeParameterResultsFirstLine(BufferedWriter out) throws IOException {
    Vector parV = getParameterVector(true, false);
    int totnumberParameters = parV.size();
    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parV.elementAt(i);
      if (apar.automaticOutput) {
        out.write(apar.getLabel());
        out.write("\t");
        if (apar.getFree()) {
          out.write("Error");
          out.write("\t");
        }
      }
    }
    for (int i = 0; i < Nsubordinate; i++) {
      subordinateField[i].writeResultsFirstLine(out);
    }
    for (int i = 0; i < Nsubordinateloop; i++) {
      for (int j = 0; j < numberofelementSubL(i); j++) {
        ((XRDcat) subordinateloopField[i].elementAt(j)).writeResultsFirstLine(out);
      }
    }
  }

  public String getLabel() {
    return toXRDcatString();
  }

  public void setLabel(String alabel) {
    thelabel = alabel;
  }

  public String toXRDcatString() {
    return thelabel;
  }

  @Override
  public String toString() {
    return toXRDcatString(); // super.toString()+" ("+thelabel+")";
  }

  public String toIDString() {
    XRDcat obj = getParent();
    String astring = new String("");
    if (obj != null)
      astring = new String(obj.toIDString());
    if (!astring.equals(""))
      return new String(astring + ":" + toXRDcatString());
    else
      return new String(toXRDcatString());
  }

  public String getnewString(String astring, int number) {
    String partial;
    String prefix = new String(astring);
    String finalstring = new String("");

    int i = prefix.indexOf('_');
    while (i != -1) {
      prefix = prefix.substring(i + 1);
      i = prefix.indexOf('_');
      if (i != -1) {
        partial = prefix.substring(0, i);
        if (partial.equals(partial.toUpperCase()))
          finalstring = finalstring.concat(partial);
        finalstring = finalstring.concat(" ");
      } else {
        partial = prefix.substring(0);
        if (partial.equals(partial.toUpperCase()))
          finalstring = finalstring.concat(partial);
        finalstring = finalstring.concat(" ");
      }
    }
    finalstring = finalstring.concat(Integer.toString(number));

    return finalstring.trim();
  }

  public String getParameterString(int index) {
//    System.out.println(diclist[index + totstringloop]);
    return getParameterString(diclist[index + totstringloop], "");
  }

  public void setParameterString() {

  }

  public String getParameterString(int index, int number) {
//    System.out.println(diclist[index + totparameter]);
    return getParameterString(diclist[index + totparameter], Integer.toString(number));
  }

  public String getParameterStringOnlyZero(int index, int number) {
//    System.out.println(diclist[index + totparameter]);
    if (number == 0)
      return getParameterString(index, number);
    return getParameterString(diclist[index + totparameter], "");
  }

  public static final String getParameterString(String astring, String number) {
    return astring.concat(number);
  }

  public FilePar getFilePar() {
    Object aparent = getParent();
    while (aparent != null && !(aparent instanceof FilePar))
      aparent = ((XRDcat) aparent).getParent();
    return (FilePar) aparent;
  }

  public URL getCodeBase() {
    return getFilePar().getCodeBase();
  }

  public String getDirectory() {
    return getFilePar().getDirectory();
  }

  public double getVersion() {
    return getFilePar().getVersion();
  }

  @Override
  public Object clone() {
    try {
      Object xrdcatclone = super.clone();
      ((XRDcat) xrdcatclone).notifyParentChanged();
      return xrdcatclone;

    } catch (CloneNotSupportedException e) {
      return null;
    }
  }

  public XRDcat getCopy(XRDcat parent) {
    try {
      XRDcat thecopy = (XRDcat) factory(parent, getLabel(), this.getClass().getName());

      copyCat(thecopy);
      return thecopy;
    } catch (PrototypeNotFound prototypeNotFound) {
      prototypeNotFound.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    } catch (CannotCreateXRDcat cannotCreateXRDcat) {
      cannotCreateXRDcat.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
    return null;
  }

  public void copyCat(XRDcat thecopy) {
    thecopy.setLabel(getLabel());
    for (int i = 0; i < Nstring; i++)
      thecopy.stringField[i] = stringField[i].toString();
    for (int i = 0; i < Nstringloop; i++) {
      thecopy.stringloopField[i].removeAllItems();
      for (int j = 0; j < stringloopField[i].size(); j++)
        thecopy.stringloopField[i].addItem(stringloopField[i].elementAt(j).toString());
    }
    for (int i = 0; i < Nparameter; i++)
      thecopy.parameterField[i] = parameterField[i].getCopy(thecopy);
    for (int i = 0; i < Nparameterloop; i++) {
      thecopy.parameterloopField[i].removeAllItems();
      for (int j = 0; j < numberofelementPL(i); j++)
        thecopy.parameterloopField[i].addElement(((Parameter) parameterloopField[i].elementAt(j)).getCopy(thecopy));
    }

    for (int i = 0; i < Nsubordinate; i++) {
      thecopy.setsubordinateField(i, subordinateField[i].getCopy(thecopy));
    }
    for (int i = 0; i < Nsubordinateloop; i++) {
      for (int j = 0; j < numberofelementSubL(i); j++) {
        thecopy.addsubordinateloopField(i, ((XRDcat) subordinateloopField[i].elementAt(j)).getCopy(thecopy));
      }
    }
  }

  public void merge(basicObj obj) {
  }

  public void notifyParentChanged() {
    int i, j;

    for (i = 0; i < Nsubordinate; i++) {
      XRDcat obj = subordinateField[i];
      if (obj != null)
        obj.setParent(this);
    }
    for (i = 0; i < Nsubordinateloop; i++) {
      for (j = 0; j < numberofelementSubL(i); j++) {
        ((XRDcat) subordinateloopField[i].elementAt(j)).setParent(this);
      }
    }
  }

  public void setEqualTo(XRDcat master, boolean subAlso) {
    if (this.getClass() != master.getClass()) {
      System.out.println("Objects not compatibles for bounding");
      return;
    }

    Vector parList = getParameterVector(true, false);
    Vector masterList = master.getParameterVector(true, false);
    if (parList.size() == masterList.size()) {
      for (int j = 0; j < parList.size(); j++) {
        Parameter par1 = (Parameter) parList.elementAt(j);
        Parameter par2 = (Parameter) masterList.elementAt(j);
        par1.setEqualTo(par2, 1.0, 0.0);
      }
    }

    if (!subAlso)
      return;
    parList = getParameterVector(false, true);
    masterList = master.getParameterVector(false, true);
    if (parList.size() == masterList.size()) {
      for (int j = 0; j < parList.size(); j++) {
        Parameter par1 = (Parameter) parList.elementAt(j);
        Parameter par2 = (Parameter) masterList.elementAt(j);
        par1.setEqualTo(par2, 1.0, 0.0);
      }
    }
  }

  public void refreshparametersV() {
    if (parametersV != null)
      parametersV.removeAllElements();
    parametersV = getParameterVector(true, true);
  }

  public Vector getParameterVector(boolean mainPars, boolean subAlso) {
    int i, j, k;
    XRDcat obj = null;

    Vector parVector = new Vector(0, 1);
    Vector tmpVector = null;

    if (mainPars) {
      for (i = 0; i < Nparameter; i++)
        parVector.addElement(parameterField[i]);
      for (i = 0; i < Nparameterloop; i++) {
        for (j = 0; j < numberofelementPL(i); j++)
          parVector.addElement(parameterloopField[i].elementAt(j));
      }
    }

    if (subAlso) {
      for (i = 0; i < Nsubordinate; i++) {
        obj = subordinateField[i];
        if (obj != null) {
          tmpVector = obj.getParameterVector(true, subAlso);
          for (j = 0; j < tmpVector.size(); j++)
            parVector.addElement(tmpVector.elementAt(j));
        }
      }
      for (i = 0; i < Nsubordinateloop; i++) {
        for (j = 0; j < numberofelementSubL(i); j++) {
          obj = (XRDcat) subordinateloopField[i].elementAt(j);
          tmpVector = obj.getParameterVector(true, subAlso);
          for (k = 0; k < tmpVector.size(); k++)
            parVector.addElement(tmpVector.elementAt(k));
        }
      }
    }
    return parVector;
  }

  public int totParameterNumber() {
    return parametersV.size();
  }

  public void fixAllParameters() {
    refreshparametersV();
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      apar.setNotRefinable();
    }
  }

  public void fixAllParametersPreserveBound() {
    refreshparametersV();
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      apar.setNotRefinableCheckBound();
    }
  }

  public void freeAllParameters() {
    refreshparametersV();
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      apar.setRefinable();
    }
  }

  public void freeAllParameters(String searchString) {
    basicObj[] objs = getChildren(searchString);
    for (int i = 0; i < objs.length; i++)
      objs[i].freeAllParameters(searchString);
  }

  public void fixAllParameters(String searchString) {
    basicObj[] objs = getChildren(searchString);
    for (int i = 0; i < objs.length; i++)
      objs[i].fixAllParameters(searchString);
  }

  public void freeAllParametersPreserveBound() {
    refreshparametersV();
    int totnumberParameters = totParameterNumber();

    for (int i = 0; i < totnumberParameters; i++) {
      Parameter apar = (Parameter) parametersV.elementAt(i);
      apar.setRefinableCheckBound();
    }
  }

  public basicObj[] getChildren(String searchString) {

    int i, j, k;

    int numberofObjects = getChildCount(searchString);
    basicObj childrens[] = new basicObj[numberofObjects];
// System.out.println(this + " " + searchString);
//    System.out.println("getChildren "+numberofObjects);
    basicObj obj = null;

    k = 0;
    for (i = 0; i < Nparameter; i++) {
      if (searchString == null || searchString.equalsIgnoreCase("") ||
          parameterField[i].getLabel().contains(searchString))
        childrens[k++] = parameterField[i];
    }
    for (i = 0; i < Nparameterloop; i++)
      for (j = 0; j < numberofelementPL(i); j++)
        if ((obj = (basicObj) parameterloopField[i].elementAt(j)) != null)
          if (searchString == null || searchString.equalsIgnoreCase("") ||
              obj.getLabel().contains(searchString))
            childrens[k++] = obj;
    for (i = 0; i < Nsubordinate; i++)
      if ((subordinateField[i]) != null && subordinateField[i].getChildCount(searchString) > 0) {
        childrens[k++] = subordinateField[i];
//	      System.out.println("Adding: " + subordinateField[i].toString());
      }
    for (i = 0; i < Nsubordinateloop; i++)
      for (j = 0; j < numberofelementSubL(i); j++)
        if ((obj = (basicObj) subordinateloopField[i].elementAt(j)) != null &&
            ((basicObj) subordinateloopField[i].elementAt(j)).getChildCount(searchString) > 0)
          childrens[k++] = obj;

    return childrens;
  }

  public boolean isLeaf() {return false;}

  public boolean getAllowsChildren() {return true;}

  public int getChildCount(String searchString) {
    int i, j;

    int numberofObjects = 0;
// System.out.println(this + " " + searchString);
    for (i = 0; i < Nparameter; i++) {
      // here the last modification
        if (searchString == null || searchString.equalsIgnoreCase("") ||
            parameterField[i].getLabel().contains(searchString))
          numberofObjects++;
    }

    for (i = 0; i < Nparameterloop; i++)
      for (j = 0; j < numberofelementPL(i); j++)
        if (parameterloopField[i].elementAt(j) != null)
          if (searchString == null || searchString.equalsIgnoreCase("") ||
              ((basicObj)parameterloopField[i].elementAt(j)).getLabel().contains(searchString))
          numberofObjects++;
    for (i = 0; i < Nsubordinate; i++)
      if (subordinateField[i] != null && subordinateField[i].getChildCount(searchString) > 0)
        numberofObjects++;
    for (i = 0; i < Nsubordinateloop; i++)
      for (j = 0; j < numberofelementSubL(i); j++)
        if (subordinateloopField[i].elementAt(j) != null &&
            ((basicObj) subordinateloopField[i].elementAt(j)).getChildCount(searchString) > 0)
          numberofObjects++;

//    System.out.println("getChildCount "+numberofObjects);
    return numberofObjects;
  }

  public basicObj getChildAt(int childIndex) {
    int i, j, index = -1;

    if (childIndex < Nparameter)
        return parameterField[childIndex];
    index = Nparameter;
    for (i = 0; i < Nparameterloop; i++)
      for (j = 0; j < numberofelementPL(i); j++) {
        if (parameterloopField[i].elementAt(j) != null) {
          if (index == childIndex)
            return (basicObj) parameterloopField[i].elementAt(j);
          index++;
        }
      }
    for (i = 0; i < Nsubordinate; i++)
      if (subordinateField[i] != null) {
        if (index == childIndex)
          return (basicObj) subordinateField[i];
        index++;
      }
    for (i = 0; i < Nsubordinateloop; i++)
      for (j = 0; j < numberofelementSubL(i); j++)
        if (subordinateloopField[i].elementAt(j) != null) {
          if (index == childIndex)
            return (basicObj) subordinateloopField[i].elementAt(j);
          index++;
        }

    return null;
  }

  public Enumeration children() {
    return new Enumeration() {
      int index = 0;
      public boolean hasMoreElements() {
        return index < getChildCount(null);
      }
      public Object nextElement() {
        return getChildAt(index++);
      }
    };
  }

  public int getIndex(basicObj node) {
    int i, j, index = 0;

    for (index = 0; index < Nparameter; index++) {
      if (node == parameterField[index])
        return index;
    }
    for (i = 0; i < Nparameterloop; i++)
      for (j = 0; j < numberofelementPL(i); j++) {
        if (parameterloopField[i].elementAt(j) != null) {
          if (parameterloopField[i].elementAt(j) == node)
            return index;
          index++;
        }
      }
    for (i = 0; i < Nsubordinate; i++)
      if (subordinateField[i] != null) {
        if (subordinateField[i] == node)
          return index;
        index++;
      }
    for (i = 0; i < Nsubordinateloop; i++)
      for (j = 0; j < numberofelementSubL(i); j++)
        if (subordinateloopField[i].elementAt(j) != null) {
          if (subordinateloopField[i].elementAt(j) == node)
            return index;
          index++;
        }

    return -1;
  }

  public Object[] getObjectChildren() {

    int i, j, k;

    int numberofObjects = 0;
    for (i = 0; i < Nsubordinate; i++)
      if (subordinateField[i] != null)
        numberofObjects++;
    for (i = 0; i < Nsubordinateloop; i++)
      for (j = 0; j < numberofelementSubL(i); j++)
        if (subordinateloopField[i].elementAt(j) != null)
          numberofObjects++;

    Object childrens[] = new Object[numberofObjects];

    Object obj = null;

    k = 0;
    for (i = 0; i < Nsubordinate; i++)
      if ((obj = subordinateField[i]) != null)
        childrens[k++] = obj;
    for (i = 0; i < Nsubordinateloop; i++)
      for (j = 0; j < numberofelementSubL(i); j++)
        if ((obj = subordinateloopField[i].elementAt(j)) != null)
          childrens[k++] = obj;

    return childrens;
  }

  public void addstringloopField(int numberlist, String astring) {
    stringloopField[numberlist].addItem(astring);
  }

  public void addparameterloopField(int numberlist, Parameter aparameter) {
    parameterloopField[numberlist].addItem(aparameter);
    notifyUpObjectChanged(this, Constants.PARAMETER_ADDED);
  }

  public XRDcat setsubordinateField(int numberlist, XRDcat obj) {
    if (subordinateField[numberlist] != null) {
      subordinateField[numberlist].dispose();
      subordinateField[numberlist] = null;
    }
    subordinateField[numberlist] = obj;
    notifyUpObjectChanged(this, Constants.OBJECT_CHANGED);
    return obj;
  }

  public XRDcat setsubordinateField(int numberlist) {
    return setsubordinateField(numberlist, "");
  }

  public XRDcat setsubordinateField(int numberlist, String alabel) {
//	  System.out.println("Setting model: " + alabel + ", with identifier: " + numberlist);
    if (subordinateField[numberlist] != null) {
      subordinateField[numberlist].dispose();
      subordinateField[numberlist] = null;
    }
    try {
      subordinateField[numberlist] = (XRDcat) factory(this, alabel, filterClass(classlists[numberlist], alabel));
      notifyUpObjectChanged(this, Constants.OBJECT_CHANGED);
    } catch (CannotCreateXRDcat e) {
      e.printStackTrace();
    } catch (PrototypeNotFound ex) {
      ex.printStackTrace();
    }
    if (!getFilePar().isLoadingFile()) {
      subordinateField[numberlist].initializeAsNew();
    }
    return subordinateField[numberlist];
  }

  public XRDcat addsubordinateloopField(int numberlist, XRDcat obj) {
    if (!obj.mustRemoved) {
//    System.out.println("The sub: " + obj + ", " + obj.getTheRealOne());
      subordinateloopField[numberlist].addItem(obj.getTheRealOne());
      notifyUpObjectChanged(this, Constants.OBJECT_ADDED);
//			System.out.println(obj.toXRDcatString() + " added");
      return obj;
    } else {
//    System.out.println("Add sub: " + obj + ", " + obj.getTheRealOne());
      return obj.getTheRealOne();
    }
  }

  public XRDcat addsubordinateloopField(int numberlist, XRDcat obj, int index) {
    if (!obj.mustRemoved) {
      subordinateloopField[numberlist].insertItem(obj.getTheRealOne(), index);
      notifyUpObjectChanged(this, Constants.OBJECT_ADDED);
//			System.out.println(obj.toXRDcatString() + " added");
      return obj;
    } else {
      return obj.getTheRealOne();
    }
  }

  public XRDcat getTheRealOne() {
    return this;
  }

  public XRDcat addsubordinateloopField(int numberlist) {
    try {
      XRDcat obj = (XRDcat) factory(this, "", filterClass(classlist[numberlist], ""));
      return addsubordinateloopField(numberlist, obj);
    } catch (CannotCreateXRDcat e) {
      e.printStackTrace();
    } catch (PrototypeNotFound ex) {
      ex.printStackTrace();
    }
    return null;
  }

  public XRDcat addsubordinateloopField(int numberlist, String alabel) {
    try {
      XRDcat obj = (XRDcat) factory(this, alabel, filterClass(classlist[numberlist], alabel));
      return addsubordinateloopField(numberlist, obj);
    } catch (CannotCreateXRDcat e) {
      e.printStackTrace();
    } catch (PrototypeNotFound ex) {
      ex.printStackTrace();
    }
    return null;
  }

  public boolean removeselSLField(int numberlist) {
    boolean res = stringloopField[numberlist].removeSelElement();
    notifyUpObjectChanged(this, Constants.STRING_REMOVED);
    return res;
  }

  public boolean removeselPLField(int numberlist) {
    boolean res = parameterloopField[numberlist].removeSelElement();
    notifyUpObjectChanged(this, Constants.PARAMETER_REMOVED);
    return res;
  }

  public boolean removeAllPLField(int numberlist) {
    parameterloopField[numberlist].removeAllItems();
    notifyUpObjectChanged(this, Constants.PARAMETER_REMOVED);
    return true;
  }

  public boolean removesPLField(int numberlist, int index) {
    parameterloopField[numberlist].removeItemAt(index);
    notifyUpObjectChanged(this, Constants.PARAMETER_REMOVED);
    return true;
  }

  public boolean removeselSubLField(int numberlist) {
    boolean res = subordinateloopField[numberlist].removeSelElement();
    notifyUpObjectChanged(this, Constants.OBJECT_REMOVED);
    return res;
  }

  public void removeSubLFieldAt(int numberlist, int element) {
    subordinateloopField[numberlist].removeItemAt(element);
    notifyUpObjectChanged(this, Constants.OBJECT_REMOVED);
  }

  public void removeSubLField(int numberlist, int index) {
    subordinateloopField[numberlist].removeItemAt(index);
    notifyUpObjectChanged(this, Constants.OBJECT_REMOVED);
  }

  public int numberofelementSL(int numberlist) {
    return stringloopField[numberlist].size();
  }

  public int numberofelementPL(int numberlist) {
    return parameterloopField[numberlist].size();
  }

  public int numberofelementSubL(int numberlist) {
    return subordinateloopField[numberlist].size();
  }

  public void notifyParameterChanged(Parameter source) {
/*    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
          if (i > 0 && i < 4) {
            notifyParameterChanged(source, Constants.ATOM_POSITION_CHANGED);
            notifyParameterChanged(source, Constants.STRUCTURE_FACTOR_CHANGED);
            return;
          } else {
            notifyParameterChanged(source, Constants.STRUCTURE_FACTOR_CHANGED);
            return;
          }
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED);
            return;
          }

      super.notifyParameterChanged(source);
    }
*/
    notifyParameterChanged(source, Constants.PARAMETER_CHANGED);
  }

  public void notifyParameterChanged(Parameter source, int reason) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
//      updateParametertoDoubleBuffering();
      notifyUpObjectChanged(this, reason);
    }
  }

  public void notifyStringChanged(String source) {
    notifyStringChanged(source, Constants.STRING_CHANGED);
  }

  public void notifyStringChanged(String source, int reason) {
    FilePar filepar = getFilePar();
    if (filepar != null && !filepar.isLoadingFile() && isAbilitatetoRefresh) {
//      updateStringtoDoubleBuffering();
      notifyUpObjectChanged(this, reason);
    }
  }

  public void notifyObjectChanged(XRDcat source) {
    notifyUpObjectChanged(source, Constants.OBJECT_CHANGED);
  }

  public void notifyUpObjectChanged(XRDcat source, int reason) {
    FilePar filepar = getFilePar();
    if (filepar != null && !filepar.isLoadingFile() && isAbilitatetoRefresh) {
      refreshForNotificationUp(source, reason);
      if (source == this)
        update(false);
      if (shouldNotifyParent(source, reason)) {
        XRDcat obj = getParent();
        if (obj != null)
          obj.notifyUpObjectChanged(source, reason);
      }
    }
  }

  public boolean shouldNotifyParent(XRDcat source, int reason) {
    return true; // the default
  }

  public void notifyDownObjectChanged(XRDcat source, int reason) {
    FilePar filepar = getFilePar();
//    System.out.println("Notify: " + this + " for " + source + " reason: " + reason + " ; will do: " + isAbilitatetoRefresh);
    if (filepar != null && !filepar.isLoadingFile() && isAbilitatetoRefresh) {
      refreshForNotificationDown(source, reason);
      Object[] childrens = getObjectChildren();
      int numberOfChildrens = childrens.length;
      for (int i = 0; i < numberOfChildrens; i++) {
        ((XRDcat) childrens[i]).notifyDownObjectChanged(source, reason);
      }
    }
  }

	public void checkForVersion(double version) {
		checkConsistencyForVersion(version);
		Object[] childrens = getObjectChildren();
		int numberOfChildrens = childrens.length;
		for (int i = 0; i < numberOfChildrens; i++)
			((XRDcat) childrens[i]).checkForVersion(version);
	}

	public void checkConsistencyForVersion(double version) {
	}

	public void refreshForNotificationUp(XRDcat source, int reason) {
    if (!getFilePar().isComputingDerivate() || source == this)
      refreshComputation = true;
  }

  public void refreshForNotificationDown(XRDcat source, int reason) {
    if (!getFilePar().isComputingDerivate() || source == this)
      refreshComputation = true;
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

  public void setRefreshAllStatus() {
    refreshPrepareComputation = true;
    refreshComputeBefore = true;
    refreshComputeAll = true;
    refreshComputeAfter = true;
    refreshCloseComputation = true;
  }

  public void setNoRefreshAllStatus() {
    refreshPrepareComputation = false;
    refreshComputeBefore = false;
    refreshComputeAll = false;
    refreshComputeAfter = false;
    refreshCloseComputation = false;
  }

  public void refreshPartiallyAllChildrenObjects() {
    FilePar filepar = getFilePar();
    if (filepar != null && !filepar.isLoadingFile() && isAbilitatetoRefresh) {
      setRefreshPartialStatus();
      update(false);
      Object[] childrens = getObjectChildren();
      int numberOfChildrens = childrens.length;
      for (int i = 0; i < numberOfChildrens; i++) {
        ((XRDcat) childrens[i]).refreshPartiallyAllChildrenObjects();
      }
    }
  }

  public void setRefreshPartialStatus() {
    refreshPreparePartialComputation = true;
    refreshComputePartial = true;
    refreshClosePartialComputation = true;
  }

  public void setNoRefreshPartialStatus() {
    refreshPreparePartialComputation = false;
    refreshComputePartial = false;
    refreshClosePartialComputation = false;
  }

  public void preparingComputation() {
    if (refreshPrepareComputation && isActive(this)) {
      prepareComputation();
    }
    Object[] childrens = getObjectChildren();
    int numberOfChildrens = childrens.length;
    for (int i = 0; i < numberOfChildrens; i++) {
      ((XRDcat) childrens[i]).preparingComputation();
    }
    if (refreshPrepareComputation && isActive(this)) {
      prepareComputationPost();
    }
    refreshPrepareComputation = false;
  }

  public void computingBefore() {
    if (refreshComputeBefore && isActive(this)) {
      computeBefore();
    }
    Object[] childrens = getObjectChildren();
    int numberOfChildrens = childrens.length;
    for (int i = 0; i < numberOfChildrens; i++) {
      ((XRDcat) childrens[i]).computingBefore();
    }
    if (refreshComputeBefore && isActive(this)) {
      computeBeforePost();
    }
    refreshComputeBefore = false;
  }

  public void computingAll() {
    if (refreshComputeAll && isActive(this)) {
      computeAll();
    }
    Object[] childrens = getObjectChildren();
    int numberOfChildrens = childrens.length;
    for (int i = 0; i < numberOfChildrens; i++) {
      ((XRDcat) childrens[i]).computingAll();
    }
    if (refreshComputeAll && isActive(this)) {
      computeAllPost();
    }
    refreshComputeAll = false;
  }

  public void computingAfter() {
    if (refreshComputeAfter && isActive(this)) {
      computeAfter();
    }
    Object[] childrens = getObjectChildren();
    int numberOfChildrens = childrens.length;
    for (int i = 0; i < numberOfChildrens; i++) {
      ((XRDcat) childrens[i]).computingAfter();
    }
    if (refreshComputeAfter && isActive(this)) {
      computeAfterPost();
    }
    refreshComputeAfter = false;
  }

  public void closingComputation() {
    if (refreshCloseComputation && isActive(this)) {
      closeComputation();
    }
    Object[] childrens = getObjectChildren();
    int numberOfChildrens = childrens.length;
    for (int i = 0; i < numberOfChildrens; i++) {
      ((XRDcat) childrens[i]).closingComputation();
    }
    if (refreshCloseComputation && isActive(this)) {
      closeComputationPost();
    }
    refreshCloseComputation = false;
    setNoRefreshPartialStatus();
    setNoRefreshAllStatus();
  }

  public void prepareComputation() {
    // to be implemented in subclasses
  }

  public void prepareComputationPost() {
    // to be implemented in subclasses
  }

  public void computeBefore() {
    // to be implemented in subclasses
  }

  public void computeBeforePost() {
    // to be implemented in subclasses
  }

  public void computeAll() {
    // to be implemented in subclasses
  }

  public void computeAllPost() {
    // to be implemented in subclasses
  }

  public void computeAfter() {
    // to be implemented in subclasses
  }

  public void computeAfterPost() {
    // to be implemented in subclasses
  }

  public void closeComputation() {
    // to be implemented in subclasses
  }

  public void closeComputationPost() {
    // to be implemented in subclasses
  }

  public void preparingPartialComputation() {
    if (refreshPreparePartialComputation && isActive(this)) {
      preparePartialComputation();
    }
    Object[] childrens = getObjectChildren();
    int numberOfChildrens = childrens.length;
    for (int i = 0; i < numberOfChildrens; i++) {
      ((XRDcat) childrens[i]).preparingPartialComputation();
    }
    if (refreshPreparePartialComputation && isActive(this)) {
      preparePartialComputationPost();
    }
    refreshPreparePartialComputation = false;
  }

  public void computingPartial() {
    if (refreshComputePartial && isActive(this)) {
      computePartial();
    }
    Object[] childrens = getObjectChildren();
    int numberOfChildrens = childrens.length;
    for (int i = 0; i < numberOfChildrens; i++) {
      ((XRDcat) childrens[i]).computingPartial();
    }
    if (refreshComputePartial && isActive(this)) {
      computePartialPost();
    }
    refreshComputePartial = false;
  }

  public void closingPartialComputation() {
    if (refreshClosePartialComputation && isActive(this)) {
      closePartialComputation();
    }
    Object[] childrens = getObjectChildren();
    int numberOfChildrens = childrens.length;
    for (int i = 0; i < numberOfChildrens; i++) {
      ((XRDcat) childrens[i]).closingPartialComputation();
    }
    if (refreshClosePartialComputation && isActive(this)) {
      closePartialComputationPost();
    }
    refreshClosePartialComputation = false;
    setNoRefreshPartialStatus();
    setNoRefreshAllStatus();
  }

  public void preparePartialComputation() {
    // to be implemented in subclasses
  }

  public void preparePartialComputationPost() {
    // to be implemented in subclasses
  }

  public void computePartial() {
    // to be implemented in subclasses
  }

  public void computePartialPost() {
    // to be implemented in subclasses
  }

  public void closePartialComputation() {
    // to be implemented in subclasses
  }

  public void closePartialComputationPost() {
    // to be implemented in subclasses
  }

  public void update(boolean firstLoading) {
    // for parent = FilePar this must be overrided
    updateStringtoDoubleBuffering(firstLoading);
    updateParametertoDoubleBuffering(firstLoading);
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses

//		if (Constants.testing)
//			System.out.println("Updating parameters in: " + this.toXRDcatString());

    FilePar filepar = getFilePar();
    if (filepar == null || filepar.isLoadingFile() || !isAbilitatetoRefresh)
      return;
    for (int i = 0; i < Nparameter; i++) {
      parameterValues[i] = parameterField[i].getValueD();
    }

    parameterLoopValuesVector.removeAllElements();
    for (int i = 0; i < Nparameterloop; i++) {
      numberOfLoopParameters[i] = numberofelementPL(i);
      double parameterLoopValues[] = new double[numberOfLoopParameters[i]];
      for (int j = 0; j < numberOfLoopParameters[i]; j++)
        parameterLoopValues[j] = ((Parameter) parameterloopField[i].elementAt(j)).getValueD();
      parameterLoopValuesVector.addElement(parameterLoopValues);
    }
  }

  public void refreshCellForChange(Phase.CellOperation operation) {
   changeCellFor(operation);
    Object[] childrens = getObjectChildren();
    int numberOfChildrens = childrens.length;
    for (int i = 0; i < numberOfChildrens; i++) {
//				System.out.println(this.toXRDcatString() + ": " + childrens[i].toXRDcatString());
      ((XRDcat) childrens[i]).refreshCellForChange(operation);
    }
  }

  public void changeCellFor(Phase.CellOperation operation) {
      switch (operation) {
        case FORWARD:
          turnForward();
          break;
        case BACKWARD:
          turnBackward();
          break;
        case INVERT_A:
          invertA();
          break;
        case INVERT_B:
          invertB();
          break;
        case INVERT_C:
          invertC();
          break;
        case INVERT_ALPHA:
          invertAlpha();
          break;
        case INVERT_BETA:
          invertBeta();
          break;
        case INVERT_GAMMA:
          invertGamma();
          break;
        case SWITCH_AB:
          switchAandB();
          break;
        case SWITCH_BC:
          switchBandC();
          break;
        case SWITCH_CA:
          switchCandA();
          break;
      }
  }

  public void switchAandB() {
  }

  public void switchBandC() {
  }

  public void switchCandA() {
  }

  public void invertGamma() {
  }

  public void invertAlpha() {
  }

  public void invertBeta() {
  }

  public void invertA() {
  }

  public void invertB() {
  }

  public void invertC() {
  }

  public void turnForward() {
  }

  public void turnBackward() {
  }

  public double getParameterValue(int index) {
    return parameterValues[index];
  }

	public Parameter getParameter(int index) {
    return parameterField[index];
  }

  public String getParameterValueAsString(int index) {
    return getParameter(index).getValue();
  }

	public Parameter getParameterFromLoop(int loop, int index) {
		return (Parameter) parameterloopField[loop].elementAt(index);
	}

  public double getParameterLoopValues(int loop, int index) {
    double[] parameterLoopValues = getParameterLoopVector(loop);
// more safe but time consuming
    if (index >= parameterLoopValues.length) {
      update(false);
      parameterLoopValues = getParameterLoopVector(loop);
      if (index >= parameterLoopValues.length) {
        // may be is loading file, so we try to get it straight
        return getParameterFromLoop(loop, index).getValueD();
      }
    }
    return parameterLoopValues[index];
  }


  public double getParameterLoopError(int loop, int index) {
    return Double.parseDouble(getParameterFromLoop(loop, index).getError());
  }

  public double[] getParameterLoopVector(int loop) {
    return (double[]) parameterLoopValuesVector.elementAt(loop);
  }

  public void setString(int index, String value) {
    if (index < 0 || index > Nstring)
      System.out.println("Not available string: " + Integer.toString(index) + " for value: " + value);
    else {
	    if (stringField[index] == null || !stringField[index].equals(value)) {
        stringField[index] = value;
        notifyStringChanged(stringField[index], Constants.STRING_CHANGED);
	    }
    }
  }

  public String getString(int index) {
    if (index < 0 || index > Nstring) {
      System.out.println("Not available string number: " + Integer.toString(index));
      return "";
    }
    return stringField[index];
  }

  public int ciftonumber(String cif) {
    int number = -1;
    for (int i = 0; i < totsubordinateloop; i++)
      if (cif.equalsIgnoreCase(diclist[i]))
        return i;
    return number;
  }

  public int setField(String cif, CIFItem item) {
    return setField(cif, item.thestring, item.thestringerror, item.theminValue, item.themaxValue, item.free,
            item.refName,  item.refBound, item.constant, item.ratio, item.expression, item.autoTrace, item.positive);
  }

  public int setField(String cif, String astring, String astringerror, String min, String max, boolean free,
                      String refName, String refBound, String constant, String ratio, String expression,
                      boolean autoTrace, boolean positive) {

//    System.out.println("Super " + this.toXRDcatString() + " " + cif + " " + astring);
    if (astring != null && (astring.equals("?") || astring.equals(".")))
      return -1;
    cif = validateCIF(cif);
    astring = validateString(astring);
    astringerror = validateStringError(astringerror);

    int index = ciftonumber(cif);

    if ((0 <= index) && index < totstring)
      stringField[index] = new String(astring);
    else if (index >= totstring && index < totstringloop)
      stringloopField[index - totstring].addItem(new String(astring));
    else if ((totstringloop <= index) && index < totparameter) {
      parameterField[index - totstringloop] = new Parameter(this, getParameterString(index - totstringloop),
                          astring, astringerror, min, max, free, refName, refBound, constant, ratio, expression,
		      autoTrace, positive);
      if (!isaStringPromotedToParameter(cif))
        getFilePar().addParameterToList(parameterField[index - totstringloop]);
    } else if (index >= totparameter && index < totparameterloop) {
      parameterloopField[index - totparameter].addItem(new Parameter(this, getParameterString(index - totparameter,
              numberofelementPL(index - totparameter)), astring, astringerror,
              min, max, free, refName, refBound, constant, ratio, expression, autoTrace, positive));
      if (!isaStringPromotedToParameter(cif))
        getFilePar().addParameterToList(parameterField[index - totparameter]);
    } else if (index >= totparameterloop && index < totsubordinate)
      try {
        setsubordinateField(index - totparameterloop, (XRDcat) factory(this, astring,
                filterClass(classlists[index - totparameterloop], astring)));
      } catch (CannotCreateXRDcat e) {
        e.printStackTrace();
      } catch (PrototypeNotFound ex) {
        ex.printStackTrace();
      }
    else if (index >= totsubordinate && index < totsubordinateloop)
      try {
        addsubordinateloopField(index - totsubordinate, (XRDcat) factory(this, astring,
                filterClass(classlist[index - totsubordinate], astring)));
      } catch (CannotCreateXRDcat e) {
        e.printStackTrace();
      } catch (PrototypeNotFound ex) {
        ex.printStackTrace();
      }
/*		else if (index >= totsubordinateloop && index < totarray)
			try {
			} catch (CannotCreateXRDcat e) {
				e.printStackTrace();
			} catch (PrototypeNotFound ex) {
				ex.printStackTrace();
			}
		else if (index >= totarray && index < totarrayloop)
			try {
			} catch (CannotCreateXRDcat e) {
				e.printStackTrace();
			} catch (PrototypeNotFound ex) {
				ex.printStackTrace();
			} */
    else if (index == -1) {   // could be a removed parameter, we just add a null to the parameters bound list
      if (isaParameterRemoved(cif))
        getFilePar().addParameterToList(null);
/* Luca todo     else
        trySubordinate(cif, astring, astringerror, min, max, free,
          refName, refBound, constant, ratio, autoTrace, positive);*/
    }
    return index;
  }

  public static boolean isaParameterRemoved(String cif) {
    for (int i = 0; i < removedParameterList.length; i++)
      if (cif.equalsIgnoreCase(removedParameterList[i])) {
//		  	if (Constants.testing)
//		  		System.out.println("Parameter removed: " + cif);
        return true;
      }
    return false;
  }

  public boolean isaStringPromotedToParameter(String cif) {
    if (getFilePar().getVersion() > 1.69)
      return false;
    for (int i = 0; i < removedStringList169.length; i++)
      if (cif.equalsIgnoreCase(removedStringList169[i])) {
//		  	if (Constants.testing)
//		  		System.out.println("String promoted to parameter: " + cif);
        return true;
      }
    return false;
  }

  public boolean mayRefines(Parameter apar) {
    return isActive(this);
  }

  public boolean isActive(XRDcat acat) {
    XRDcat aparent = getParent();
    if (aparent != null)
      return aparent.isActive(this);
    return true;
  }

  public XRDcat setSubordinateField(String cif, String astring) {
    cif = validateCIF(cif);
    astring = validateString(astring);

	  if (this instanceof Phase) {
		  if (ciftonumber(cif) == Phase.textureID && astring.equalsIgnoreCase("none"))
			  astring = "none tex";
		  else if (ciftonumber(cif) == Phase.microAbsorptionID && astring.toLowerCase().startsWith("none"))
			  astring = "No microabsorption";
		  else if (ciftonumber(cif) == Phase.planarDefectsID && astring.toLowerCase().startsWith("no"))
			  astring = "none pd";
	  }

    int index = ciftonumber(cif);
    XRDcat obj = null;

    if (index >= totparameterloop && index < totsubordinate)
      try {
//	      System.out.println("filterclass: " + filterClass(classlists[index - totparameterloop], astring));
        obj = setsubordinateField(index - totparameterloop, (XRDcat) factory(this, astring,
                filterClass(classlists[index - totparameterloop], astring)));
      } catch (CannotCreateXRDcat e) {
        e.printStackTrace();
      } catch (PrototypeNotFound ex) {
        ex.printStackTrace();
      }
    else if (index >= totsubordinate && index < totsubordinateloop)
      try {
        obj = addsubordinateloopField(index - totsubordinate, (XRDcat) factory(this, astring,
                filterClass(classlist[index - totsubordinate], astring)));
      } catch (CannotCreateXRDcat e) {
        e.printStackTrace();
      } catch (PrototypeNotFound ex) {
        ex.printStackTrace();
      }
    return obj;
  }

  public void setLoop(Vector avector, int element) {
    validateCIF(avector);

    int loopitem = avector.size();
    int pivot[] = new int[(loopitem / element)];
    XRDcat theobj = null;
    int i = 0, index, newindex;
    int j, itmp;
    int isPresent = 0;
    ListVector myList = null;
    CIFItem item;
    boolean pivotreq = false;
//Luca todo    Vector notFound = null;

    if (loopitem > 0) {
      itmp = 0;
      while (i < loopitem) {
        item = (CIFItem) avector.elementAt(i);
        index = ciftonumber(item.cif);
        if (index >= 0 && index < totstring) {
          stringField[index] = new String(item.thestring);
          avector.removeElementAt(i);
          loopitem--;
        } else if (index >= totstring && index < totstringloop) {
          stringloopField[index - totstring].addItem(new String(item.thestring));
          avector.removeElementAt(i);
          loopitem--;
        } else if (index >= totparameter && index < totparameterloop) {
          newindex = index - totparameter;
          Parameter anewpar = new Parameter(this, getParameterString(newindex,
                  numberofelementPL(newindex)), item.thestring, item.thestringerror,
                                                item.theminValue, item.themaxValue, item.free,
                                                item.refName, item.refBound,  item.constant,  item.ratio,
		                                            item.expression,
                                                item.autoTrace, item.positive);
          parameterloopField[newindex].addItem(anewpar);
          if (!isaStringPromotedToParameter(item.cif))
            getFilePar().addParameterToList(anewpar);
          avector.removeElementAt(i);
          loopitem--;
        } else if (index >= totparameterloop && index < totsubordinate) {
          newindex = index - totparameterloop;
          pivotreq = true;
          if (myList == null)
            myList = new ListVector(0, 1, this);
          if (subordinateField[newindex] == null ||
                  !subordinateField[newindex].toXRDcatString().equals(item.thestring)) {
            try {
              setsubordinateField(newindex, (XRDcat) factory(this, item.thestring,
                      filterClass(classlists[newindex], item.thestring)));
            } catch (CannotCreateXRDcat e) {
              e.printStackTrace();
            } catch (PrototypeNotFound ex) {
              ex.printStackTrace();
            }
          }
          pivot[itmp] = itmp;
          itmp++;
          avector.removeElementAt(i);
          loopitem--;
          if (element <= 1)
            myList = null;
          else
            myList.addElement(subordinateField[newindex]);
        } else if (index >= totsubordinate && index < totsubordinateloop) {
          newindex = index - totsubordinate;
          if (pivotrequired[newindex]) {
            //		pivot element required in the cif file

            pivotreq = pivotrequired[newindex];
            isPresent = 0;
            for (j = 0; j < numberofelementSubL(newindex); j++)
              if ((subordinateloopField[newindex].elementAt(j).toString()).equals(item.thestring))
                isPresent = j + 1;
            if (isPresent == 0) {
              try {
//              	System.out.println("Pivot: " + classlist[newindex] + " " + item.thestring);
                subordinateloopField[newindex].addItem(factory(this, item.thestring,
                        filterClass(classlist[newindex], item.thestring)));
              } catch (CannotCreateXRDcat e) {
                e.printStackTrace();
              } catch (PrototypeNotFound ex) {
                ex.printStackTrace();
              }
              pivot[itmp] = numberofelementSubL(newindex) - 1;
            } else
              pivot[itmp] = isPresent - 1;
            itmp++;
            avector.removeElementAt(i);
            loopitem--;
            if (myList == null)
              myList = subordinateloopField[newindex];
          } else {
            //		pivot element not required in the cif file
            try {
//            	System.out.println("No pivot: " + classlist[newindex] + " " + item.thestring);
              subordinateloopField[newindex].addItem(factory(this, item.thestring,
                      filterClass(classlist[newindex], item.thestring)));
            } catch (CannotCreateXRDcat e) {
              e.printStackTrace();
            } catch (PrototypeNotFound ex) {
              ex.printStackTrace();
            }
            pivot[itmp] = itmp;
            itmp++;
            if (myList == null)
              myList = subordinateloopField[newindex];
            i++;
          }
        } /* else if (index >= totsubordinateloop && index < totarray)
        {
          valuesArray[index] = Double.valueOf(item.thestring).doubleValue();
          avector.removeElementAt(i);
          loopitem--;
        } else if (index >= totarray && index < totarrayloop)
        {
          valuesLoopArray[index-totarray].addItem(new String(item.thestring));
          avector.removeElementAt(i);
          loopitem--;*/
 		else /* Luca todo       {
          if (notFound == null)
            notFound = new Vector(0, avector.size());
          notFound.add(item);*/
          i++;
/* Luca todo        } */
      } // end of while (i < loopitem)
      loopitem = avector.size();
      itmp = 0;
      if (myList != null) {
        if (pivotreq) {
          int newelement = element - 1;
          while ((itmp + 1) * newelement <= loopitem) {
//				  	boolean treeobj = false;
            theobj = (XRDcat) myList.elementAt(pivot[itmp]);
            for (i = itmp * newelement; i < (itmp + 1) * newelement; i++) {
              item = (CIFItem) avector.elementAt(i);
              int ind = theobj.ciftonumber(item.cif);
              if (ind >= theobj.totparameterloop) {
                Vector tmpv = new Vector(0, 1);
                while (i < (itmp + 1) * newelement)
                  tmpv.addElement(avector.elementAt(i++));
                theobj.setLoop(tmpv, tmpv.size());
                tmpv = null;
              } else
                theobj.setField(item.cif, item);
            }
            itmp++;
          }
        } else {
          while ((itmp + 1) * element <= loopitem) {
            theobj = (XRDcat) myList.elementAt(itmp);
            for (i = itmp * element; i < (itmp + 1) * element; i++) {
              item = (CIFItem) avector.elementAt(i);
              int ind = theobj.ciftonumber(item.cif);
              if (ind >= theobj.totparameterloop) {
                Vector tmpv = new Vector(0, 1);
                while (i < (itmp + 1) * element)
                  tmpv.addElement(avector.elementAt(i++));
                theobj.setLoop(tmpv, tmpv.size());
                tmpv = null;
              } else
                theobj.setField(item.cif, item);
            }
            itmp++;
          }
        }
      } // end of if (myList != null)
    } // end of if (loopitem>0)
/* Luca todo    if (notFound != null)
      trySubordinate(notFound, element);*/
  } // end of setLoop method

  protected void trySubordinate(Vector notFound, int element) {
    Object[] childrens = getObjectChildren();
    int numberOfChildrens = childrens.length;
    for (int i = 0; i < numberOfChildrens; i++) {
      ((XRDcat) childrens[i]).setLoop(notFound, element);
    }
  }

  protected int trySubordinate(String cif, String astring, String astringerror, String min, String max, boolean free,
                               String refName, String refBound, String constant, String ratio, String expression,
                               boolean autoTrace, boolean positive) {
    Object[] childrens = getObjectChildren();
    int numberOfChildrens = childrens.length;
    int index = -1;
    for (int i = 0; i < numberOfChildrens; i++) {
      index = ((XRDcat) childrens[i]).setField(cif, astring, astringerror, min, max, free,
        refName, refBound, constant, ratio, expression, autoTrace, positive);
    }
    return index;
  }

  public static final String validateCIF(String cif) {
    int totnumber = CIFtoReplace.length;
    for (int i = 0; i < totnumber; i++)
      if (cif.toLowerCase().indexOf(CIFtoReplace[i].toLowerCase()) >= 0)
        return Misc.replaceSubstringInStringIgnoreCase(cif, CIFtoReplace[i], replacingCIF[i]);
    return cif;
  }

  public static String validateString(String astring) {
    int totnumber = stringToReplace.length;
    for (int i = 0; i < totnumber; i++) {
	    if (stringToReplace[i].indexOf("exact-") != 0) {
		    if (astring.indexOf(stringToReplace[i]) >= 0)
			    return Misc.replaceSubstringInString(astring, stringToReplace[i], replacingString[i]);
	    } else {
		    String replString = stringToReplace[i].substring(6);
		    if (astring.compareTo(replString) == 0)
			    return replacingString[i];
	    }
    }
    return astring;
  }

  public static String validateStringError(String astringerror) {
    return astringerror;
  }

  public static void validateCIF(Vector avector) {
    int loopitem = avector.size();
    for (int i = 0; i < loopitem; i++) {
      CIFItem item = (CIFItem) avector.elementAt(i);
      if (item.cif.equalsIgnoreCase("_atom_site_U_iso_or_equiv")) {
	      if (item.thestring.equalsIgnoreCase("?") || item.thestring.equalsIgnoreCase("."))
		      item.thestring = "0.0";
        double value = Double.parseDouble(item.thestring);
	      if (Math.abs(value * 8.0 * Constants.PI * Constants.PI) < 5) {
		      // it's a true uiso not a biso
		      value = value * 8.0 * Constants.PI * Constants.PI;
	      }
        item.thestring = Double.toString(value);
      }
      item.cif = validateCIF(item.cif);
      item.thestring = validateString(item.thestring);
      item.thestringerror = validateStringError(item.thestringerror);
    }
  }

  public String filterClass(String classname, String alabel) {
    String aclassname = null;
    if (classname.startsWith("superclass:"))
      aclassname = getClassbyName(filterClass(classname), alabel);
    if (aclassname == null)
      return filterClass(classname);
    else
      return aclassname;
  }

  public static String filterClass(String classname) {
    if (classname.startsWith("superclass:"))
      return classname.substring(11);
    else
      return classname;
  }

  public String getClassbyName(String classname, String alabel) {
    int index = getNumberbyName(classname, alabel);
    if (index != -1)
      return Constants.classname[index];
    return null;
  }

  public int getNumberbyName(String classname, String alabel) {
// first try an exact match
    for (int i = 0; i < Constants.numberofclasstype; i++) {
      if (alabel.equalsIgnoreCase(Constants.identifier[i].toLowerCase()))
       if (Misc.areClassCompatibles(classname, Constants.classname[i]))
          return i;
    }
// failed let's see for a substring

//	  System.out.println(classname + ", substring: " + alabel);
    for (int i = 0; i < Constants.numberofclasstype; i++) {
      int occurrence = alabel.toLowerCase().indexOf(Constants.identifier[i].toLowerCase());
      if (occurrence >= 0 && occurrence < alabel.length())
        if (Misc.areClassCompatibles(classname, Constants.classname[i]))
          return i;
    }
    return -1;
  }

  public int getNumberbyClassname(String alabel) {
    for (int i = 0; i < Constants.numberofclasstype; i++)
      if (alabel.equals(Constants.classname[i]))
        return i;
    return -1;
  }

  public static class PrototypeNotFound extends Exception {
  }

  public static class CannotCreateXRDcat extends Exception {
  }

  private static Vector XRDcatTypes = new Vector(0, 1);

  public static Object factory(XRDcat obj, String alabel, String classname)
          throws PrototypeNotFound, CannotCreateXRDcat {
//		System.out.println("Creating " + classname + " " + alabel);
    for (int i = 0; i < XRDcatTypes.size(); i++) {
      Class xc = (Class) XRDcatTypes.elementAt(i);
      if (xc.getName().indexOf(classname) != -1) {
        try {
          Constructor ctor = xc.getConstructor(
                  new Class[]{XRDcat.class, String.class});
          return ctor.newInstance(
                  new Object[]{obj, alabel});
        } catch (Exception ex) {
          System.out.println(obj);
          System.out.println(alabel);
          System.out.println(classname);
          ex.printStackTrace();
          throw new CannotCreateXRDcat();
        }
      }
    }
    try {
//			System.out.println("Loading " + classname);
      XRDcatTypes.addElement(Class.forName(classname));
    } catch (Exception ex) {
      ex.printStackTrace();
      throw new PrototypeNotFound();
    }
    return factory(obj, alabel, classname);
  }

/*  public static Object factory(String classname)
          throws PrototypeNotFound {
    try {
      Class xc = Class.forName(classname);
      Constructor ctor = xc.getConstructor(new Class[]{XRDcat.class, String.class});
      return ctor.newInstance(new Object[]{});
    } catch (Exception ex) {
      ex.printStackTrace();
      throw new PrototypeNotFound();
    }
  }*/

  public String toDataString() {
    return toXRDcatString();
  }

  public void storeOnDB(String databaseName) {
    try {
      BufferedWriter out = Misc.getWriterForAppend(databaseName);
      if (out != null) {
        out.write("data_" + toDataString());
        out.newLine();
        writeParameters(out);
        out.flush();
        out.close();
      } else {
        System.out.println("Not able to open the file for append");
      }
    } catch (IOException ioe) {
      System.out.println("Unable to save the object " + toXRDcatString());
    }
  }

  public void writeParameters(BufferedWriter out) {

    writeDataField(out);
    writeAllFields(out);
    writeAllLoopFields(out);
    writeAllParameters(out);
    writeAllLoopParameters(out);
    writeAllSubordinates(out, "", "");
    writeCustomObject(out);
    writeBounds(out);

  }

  public void writeDataField(BufferedWriter out) {
  }

  public void writeBounds(BufferedWriter out) {
  }

  public void writeAllFields(BufferedWriter out) {
    for (int i = 0; i < Nstring; i++)
      writeField(out, diclist[i], stringField[i]);

  }

  public void writeAllLoopFields(BufferedWriter out) {
    try {

      for (int i = 0; i < Nstringloop; i++) {
        if (stringloopField[i].size() > 0) {
          out.newLine();
          out.write("loop_");
          out.newLine();
          out.write(diclist[totstring + i]);
          out.newLine();
          for (int j = 0; j < stringloopField[i].size(); j++) {
            out.write(" ");
            String field = (String) stringloopField[i].elementAt(j);
            if (field.indexOf(' ') >= 0)
              out.write("'" + field + "'");
            else
              out.write(field);
//            out.write("'" + (String) stringloopField[i].elementAt(j) + "'");
            out.newLine();
          }
        }
      }
      if (Nstringloop > 0)
        out.newLine();

    } catch (IOException ioe) {
      System.out.println("Error in writing the loop field " + toXRDcatString());
    }

  }

  public void writeAllParameters(BufferedWriter out) {

    for (int i = 0; i < Nparameter; i++)
      writeParameter(out, diclist[i + totstringloop], parameterField[i]);

  }

  public void writeAllLoopParameters(BufferedWriter out) {
    try {

      for (int i = 0; i < Nparameterloop; i++) {
        if (parameterloopField[i].size() > 0) {
          out.newLine();
          out.write("loop_");
          out.newLine();
          out.write(diclist[totparameter + i]);
          out.newLine();
          for (int j = 0; j < parameterloopField[i].size(); j++) {
            out.write(" ");
            writeParameterLoop(out, (Parameter) parameterloopField[i].elementAt(j));
          }
        }
      }
      if (Nparameterloop > 0)
        out.newLine();

    } catch (IOException ioe) {
      System.out.println("Error in writing the loop parameter " + toXRDcatString());
    }

  }

  public void writeAllSubordinates(BufferedWriter out, String previousDic, String previousObj) {

    for (int i = 0; i < Nsubordinate; i++) {
      XRDcat obj = subordinateField[i];
      if (obj != null)
        writeSubordinate(out, diclist[totparameterloop + i], obj);
    }
    for (int i = 0; i < Nsubordinateloop; i++) {
      if (subordinateloopField[i].size() > 0)
        for (int j = 0; j < subordinateloopField[i].size(); j++) {
          XRDcat obj = (XRDcat) subordinateloopField[i].elementAt(j);
          if (obj != null)
            writeSubordinate(out, diclist[totsubordinate + i], obj);
        }
    }

  }

  public void writeSubordinate(BufferedWriter out, String dicstring, XRDcat subordinate) {

    try {
      if (subordinate != null) {
        out.newLine();
        out.write("#subordinateObject_" + subordinate.toXRDcatString());
        out.newLine();
        out.newLine();
        out.write(dicstring + " '" + subordinate.toXRDcatString() + "'");
        out.newLine();
        out.newLine();
        subordinate.writeParameters(out);
        out.newLine();
        out.write("#end_subordinateObject_" + subordinate.toXRDcatString());
        out.newLine();
        out.newLine();
      }
    } catch (IOException ioe) {
      System.out.println("Error in writing the subordinate object " + toXRDcatString());
    }

  }

  public void writeSubordinate_no_subordinate(BufferedWriter out, String dicstring, XRDcat subordinate) {
// we don't want the default #subordinate ........... #endSubordinate stuff for main objects
    try {
      if (subordinate != null) {
        out.newLine();
        subordinate.writeParameters(out);
        out.newLine();
      }
    } catch (IOException ioe) {
      System.out.println("Error in writing the subordinate object " + toXRDcatString());
    }
  }

  public void writeDiclistnoLoop(BufferedWriter out) {

    try {
      for (int i = 0; i < Nstring; i++)
        out.write(" " + diclist[i]);
      for (int i = 0; i < Nparameter; i++)
        out.write(" " + diclist[i + totstringloop]);
      out.newLine();

    } catch (IOException ioe) {
      System.out.println("Error in writing the object " + toXRDcatString());
    }
  }

  public void writeField(BufferedWriter out, String dicterm, String field) {
    try {
      if (field.equals(""))
        field = new String("?");
      out.write(dicterm);
      out.write(" ");
      if (field.indexOf(' ') >= 0)
        out.write("'" + field + "'");
      else
        out.write(field);
      out.newLine();
    } catch (IOException ioe) {
      System.out.println("Error in writing the object " + toXRDcatString());
    }
  }

  public void writeloopField(BufferedWriter out, String field) {
    try {
      if (field.equals(""))
        field = new String("?");
      out.write(" ");
      if (field.indexOf(' ') >= 0)
        out.write("'" + field + "'");
      else
        out.write(field);
    } catch (IOException ioe) {
      System.out.println("Error in writing the object " + toXRDcatString());
    }
  }

  public void writeParameter(BufferedWriter out, String dicterm, Parameter par) {
    par.writeParameter(out, dicterm, getFilePar().getBoundTracker());
  }

  public void writeParameterLoop(BufferedWriter out, Parameter par) {
    par.writeParameter(out, "", getFilePar().getBoundTracker());
  }

  public void writeCustomObject(BufferedWriter out) {

/*		try {
    	out.newLine();
    	out.write("#custom_object_" + "nothing");
    		out.newLine();
    		out.newLine();
    		out.write("#end_custom_object_" + "nothing");
    		out.newLine();
    		out.newLine();
    	}
    } catch (IOException ioe) {
    	System.out.println("Error in writing the custom object for " + toXRDcatString());
    }*/

  }

  public void readCustomObject(CIFtoken ciffile) {
    // to be override by subclasses
    // the default read and do nothing
//		String thecife;
//		int newtoken;
    int tokentype;
//		XRDcat theobj = null;
    boolean endofInput = false;

    try {
      do {
        tokentype = ciffile.nextToken();
        switch (tokentype) {
          case CIFtoken.TT_CIFE:
/*							// CIF item
						thecife = new String(ciffile.thestring);
						newtoken = ciffile.nextToken();
//							System.out.println(ciffile.thestring);
						if (FilePar.isValidToken(newtoken)) {
							theobj = setSubordinateField(thecife, ciffile.thestring);
							endofInput = true;
						}
						else {
							ciffile.pushBack();
//								System.out.println("Pushback: " + ciffile.thestring);
						}*/
            break;
          case CIFtoken.TT_CUSTOM_END:
            // subordinate loop
            endofInput = true;
            break;
          default:
            {
            }
        }
      } while (tokentype != CIFtoken.TT_EOF && !endofInput);
    } catch (IOException ioe) {
      System.out.println("IO exception in custom object for " + toXRDcatString());
    }

/*		if (theobj != null)
			theobj.readall(ciffile);*/
  }

  public void readtheobject(CIFtoken ciffile) {

    String thecife;
    int newtoken, tokentype;
    XRDcat theobj = null;
    boolean endofInput = false;

    try {
      do {
        tokentype = ciffile.nextToken();
        switch (tokentype) {
          case CIFtoken.TT_CIFE:
            // CIF item
            thecife = new String(ciffile.thestring);
            newtoken = ciffile.nextToken();
            if (Constants.checkCIFinputInConsole)
              System.out.println("Subordinate object: " + ciffile.thestring + ", (" + thecife + ")");
//							System.out.println(this.toXRDcatString() + " " + ciffile.thestring);
            if (FilePar.isValidToken(newtoken)) {
              theobj = setSubordinateField(thecife, ciffile.thestring);
              if (theobj != null)
                endofInput = true;
            } else {
              ciffile.pushBack();
//								System.out.println("Pushback: " + ciffile.thestring);
            }
            break;
          case CIFtoken.TT_SUBE:
            // subordinate loop
            endofInput = true;
            break;
          default:
            {
            }
        }
      } while (tokentype != CIFtoken.TT_EOF && !endofInput);
    } catch (IOException ioe) {
      ioe.printStackTrace();
      System.out.println("IO exception in object " + toXRDcatString());
    }
    if (Constants.checkCIFinputInConsole)
      System.out.println("Reading object: " + theobj + ", from: " + this.toXRDcatString());
    if (theobj != null)
      theobj.readall(ciffile);
  }

	public void readall(CIFtoken ciffile) {
    boolean checkInput = Constants.checkCIFinputInConsole;
    String thecife;
    int newtoken, tokentype;
//		XRDcat theobj = this;
    boolean endofInput = false;
    boolean oldAbilitation = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
//	  isReadingFromFile = true;
		for (int i = 0; i < Nparameterloop; i++)
			parameterloopField[i].removeAllItems();
		for (int i = 0; i < Nstringloop; i++)
			stringloopField[i].removeAllItems();


		try {
      do {
        tokentype = ciffile.nextToken();
        if (checkInput)
          System.out.println(ciffile.thestring);
        switch (tokentype) {
          case CIFtoken.TT_DATA:
          case CIFtoken.TT_GLOB:
          case CIFtoken.TT_INST:
          case CIFtoken.TT_DATASET:
          case CIFtoken.TT_SAMPLE:
          case CIFtoken.TT_PHASE:
          case CIFtoken.TT_SUBE:
          case CIFtoken.TT_CUSTOM_END:
            if (checkInput)
            System.out.println("End of Input!");
            // subordinate loop
            endofInput = true;
            break;
          case CIFtoken.TT_SUBO:
            // subordinate loop
//            System.out.println("Subo " + ciffile);
            readtheobject(ciffile);
            break;
          case CIFtoken.TT_CUSTOM:
            // subordinate loop
            readCustomObject(ciffile);
            break;
          case CIFtoken.TT_LOOP:
            // data loop
            Vector itemlistv = new Vector(0, 1);
            Vector cifelistv = new Vector(0, 1);
            newtoken = ciffile.nextToken();
            while (newtoken == CIFtoken.TT_CIFE) {
              itemlistv.addElement(ciffile.thestring);
              newtoken = ciffile.nextToken();
              if (checkInput)
								System.out.println(ciffile.thestring);
            }
            int loopitem = itemlistv.size();
            if (loopitem > 0) {
              while (FilePar.isValidToken(newtoken)) {
                cifelistv.addElement(new CIFItem((String) itemlistv.elementAt(0),
                        ciffile));
                for (int i = 1; i < loopitem; i++) {
                  newtoken = ciffile.nextToken();
                  if (checkInput)
										System.out.println(ciffile.thestring);
                  cifelistv.addElement(new CIFItem((String) itemlistv.elementAt(i),
                          ciffile));
                }
                newtoken = ciffile.nextToken();
                if (checkInput)
									System.out.println(ciffile.thestring);
              }
            }
            ciffile.pushBack();
            if (checkInput)
							System.out.println("Pushback: " + ciffile.thestring);
            setLoop(cifelistv, loopitem);
            cifelistv.removeAllElements();
            itemlistv.removeAllElements();
            break;
          case CIFtoken.TT_CIFE:
            // CIF item
            thecife = ciffile.thestring;
	          if (checkInput)
		          System.out.println(thecife);
            newtoken = ciffile.nextToken();
            if (FilePar.isValidToken(newtoken))
              setField(thecife, new CIFItem(thecife, ciffile));
            else {
              ciffile.pushBack();
              if (checkInput)
								System.out.println("Pushback: " + ciffile.thestring);
            }
            break;
          default:
            {
            }
        }
      } while (tokentype != CIFtoken.TT_EOF && !endofInput);
    } catch (Exception ioe) {
      ioe.printStackTrace();
      System.out.println("IO exception in object " + toXRDcatString());
    }
    endOfReadingReached();
    isAbilitatetoRefresh = oldAbilitation;
    notifyObjectChanged(this);
//    isReadingFromFile = false;
  }

  protected void endOfReadingReached() {
    // to be implemented by subclasses if needed
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    if (autoDialog)
      return new JOptionDialogComplete(parent, this);
    else
      return new JOptionsDialog(parent, this);
  }

  public void edit(Frame aframe) {
    getOptionsDialog(aframe).setVisible(true);
  }

  public void plotFunction(Frame theframe, int index) {
    if (Constants.textonly)
      return;
    PolynomialFunction function = new PolynomialFunction(parameterloopField[index]);
    (new PlotParameterFunction(theframe, function)).setVisible(true);
  }

  /**
   * Validate the consistency of this object respect to the problem implemented.
   * If something is wrong a modal dialog is displayed.
   * @return the consistency of the problem; true if all it's ok, false otherwise.
   */
  public boolean validate() {

    boolean allItsOk = true;

    String result = checkIntegrity();
    if (result != null) {
      allItsOk = false;
      if (Constants.textonly)
        System.out.println("Warning: " + result);
      else
        (new AttentionD(toXRDcatString(), result)).setVisible(true);
    }

    for (int i = 0; i < Nsubordinate; i++) {
      XRDcat obj = subordinateField[i];
      if (obj != null)
        allItsOk = (allItsOk && obj.validate());
    }
    for (int i = 0; i < Nsubordinateloop; i++) {
      for (int j = 0; j < numberofelementSubL(i); j++) {
        allItsOk = (allItsOk && ((XRDcat) subordinateloopField[i].elementAt(j)).validate());
      }
    }

    return allItsOk;
  }

  /**
   * Check the consistency of this object respect to the problem implemented.
   * It's automaticly called before computation by the validate routine.
   * Subclasses of this object could overwrite it to implement specific checking that must be
   * done before starting computation.
   * @return the status of the checking; null if all it's ok, a message with the problem otherwise.
   */
  public String checkIntegrity() {
    return null;
  }

  boolean disposed = false;

  public void dispose() {

    isAbilitatetoRefresh = false;

    int i;

    for (i = 0; i < Nstringloop; i++) {
      if (stringloopField != null && stringloopField[i] != null)
        stringloopField[i].dispose();
    }
    for (i = 0; i < Nparameter; i++) {
      if (parameterField != null && parameterField[i] != null)
        parameterField[i].dispose();
    }
    for (i = 0; i < Nparameterloop; i++) {
      if (parameterloopField != null && parameterloopField[i] != null)
        parameterloopField[i].dispose();
    }
    for (i = 0; i < Nsubordinate; i++) {
      if (subordinateField != null && subordinateField[i] != null)
        subordinateField[i].dispose();
    }
    for (i = 0; i < Nsubordinateloop; i++) {
      if (subordinateloopField != null && subordinateloopField[i] != null)
        subordinateloopField[i].dispose();
    }
  }

  public void forceRemove() {
    int i;

    for (i = 0; i < Nstring; i++)
      stringField[i] = null;
    for (i = 0; i < Nstringloop; i++) {
      stringloopField[i] = null;
    }
    for (i = 0; i < Nparameter; i++) {
      parameterField[i] = null;
    }
    for (i = 0; i < Nparameterloop; i++) {
      parameterloopField[i] = null;
    }
    for (i = 0; i < Nsubordinate; i++) {
      subordinateField[i] = null;
    }
    for (i = 0; i < Nsubordinateloop; i++) {
      subordinateloopField[i] = null;
    }

  }

    @Override
  protected void finalize() throws Throwable {
//		if (Constants.testing)
//			System.out.println("DEBUG: " + this.toXRDcatString() + " finalizing");

    if (!disposed)
      dispose();
    forceRemove();

    thelabel = null;

    parent = null;

    diclist = null;
    diclistRealMeaning = null;
    classlist = null;
    classlists = null;
    pivotrequired = null;

    super.finalize();

  }

  public static final void printf(String message, ProgressFrame prF) {
    if (prF != null)
      prF.setProgressText(message);
    else
      System.out.println(message);
  }

  public String getCifID(Parameter parameter) {

    for (int i = 0; i < Nparameter; i++)
      if (parameterField[i] == parameter)
        return getParameterString(i);
    for (int i = 0; i < Nparameterloop; i++)
      for (int j = 0; j < parameterloopField[i].size(); j++)
         if (parameter == parameterloopField[i].elementAt(j))
          return getParameterStringOnlyZero(i, j);

    // this should not happen
    System.out.println("CifID error in:" + toXRDcatString() + " " + parameter.toString());
    return null;
  }

  public void printInformations(OutputStream out) {
    try {
//      out.flush();
      printPreInformations(out);
      printBasicInformations(out);
      printCustomInformations(out);
      printStringInformations(out);
      printStringLoopInformations(out);
      printParameterInformations(out);
      printParameterLoopInformations(out);
      printSubordinateInformations(out);
      printSubordinateLoopInformations(out);
//      out.flush();
    } catch (IOException io) {
      io.printStackTrace();
    }
  }

  public void printPreInformations(OutputStream out) throws IOException {
    // to be implemented by subclasses
//    newLine(out);
  }

  public void printBasicInformations(OutputStream out) throws IOException {
    newLine(out);
    printLine(out, "                 Object: " + toXRDcatString());
    newLine(out);
  }

  public void printCustomInformations(OutputStream out) throws IOException {
    // to be implemented by subclasses
//    newLine(out);
  }

  public void printStringInformations(OutputStream out) throws IOException {
    if (Nstring == 0)
      return;
    printLine(out, "String informations (CIF term, value) :");
    for (int i = 0; i < Nstring; i++)
      printLine(out, diclist[i] + ", " + stringField[i]);
    newLine(out);
  }

  public void printStringLoopInformations(OutputStream out) throws IOException {
    if (Nstringloop == 0)
      return;
    printLine(out, "Loop of string informations (CIF term, value) :");
    for (int i = 0; i < Nstringloop; i++) {
      if (stringloopField[i].size() > 0) {
        printLine(out, "loop of " + diclist[totstring + i]);
        for (int j = 0; j < stringloopField[i].size(); j++)
          printLine(out, " " + (String) stringloopField[i].elementAt(j));
        newLine(out);
      }
    }
    newLine(out);
  }

  public void printParameterInformations(OutputStream out) throws IOException {
    if (Nparameter == 0)
      return;
    printLine(out, "Parameter informations :");
    for (int i = 0; i < Nparameter; i++)
      parameterField[i].printInformations(out, this);
    newLine(out);
  }

  public void printParameterLoopInformations(OutputStream out) throws IOException {
    if (Nparameterloop == 0)
      return;
    printLine(out, "Parameter loop informations :");
    for (int i = 0; i < Nparameterloop; i++) {
      if (numberofelementPL(i) > 0) {
        printLine(out, "Parameter loop number : " + i);
        for (int j = 0; j < numberofelementPL(i); j++)
          ((Parameter) parameterloopField[i].elementAt(j)).printInformations(out, this);
        newLine(out);
      }
    }
    newLine(out);
  }

  public void printSubordinateInformations(OutputStream out) throws IOException {
    if (Nsubordinate == 0)
      return;
    printLine(out, "Subordinate objects :");
    for (int i = 0; i < Nsubordinate; i++) {
      printLine(out, " Subordinate object number " + Integer.toString(i) + " :");
      subordinateField[i].printInformations(out);
    }
    newLine(out);
  }

  public void printSubordinateLoopInformations(OutputStream out) throws IOException {
    if (Nsubordinate == 0)
      return;
    printLine(out, "Loops of subordinate objects :");
    for (int i = 0; i < Nsubordinateloop; i++) {
      if (numberofelementSubL(i) > 0) {
        printLine(out, " Object loop number " + Integer.toString(i) + " :");
        for (int j = 0; j < numberofelementSubL(i); j++) {
          printLine(out, "  Object number " + Integer.toString(j) + " :");
          ((XRDcat) subordinateloopField[i].elementAt(j)).printInformations(out);
        }
        newLine(out);
      }
    }
    newLine(out);
  }

  public final void newLine(OutputStream out) throws IOException {
	  synchronized(this) {
	  if (out != null)
		  printString(out, Constants.lineSeparator);
	  }
  }

  public final void printString(OutputStream out, String string) throws IOException {
	  synchronized(this) {
	  if (out != null)
      out.write(string.getBytes());
	  }
  }

  public final void printString(OutputStream out, String string, int length) throws IOException {
	  synchronized(this) {
    if (string.length() < length) {
      int diff = length - string.length();
      StringBuffer news = new StringBuffer();
      while (diff > 0) {
        news.append(" ");
        diff--;
      }
      string = string + news.toString();
    } else if (string.length() > length) {
      string = string.substring(0, length);
    }
	  if (out != null)
		  out.write(string.getBytes());
	  }
  }

  public final void printLine(OutputStream out, String string) throws IOException {
      printString(out, string);
      newLine(out);
  }

  public void printLogInformation() {
    // to be rewritten by subclasses, take this as an example
    // call this method to output

    FilePar filepar = getFilePar();
    if (filepar.logOutput()) {
      OutputStream out = getFilePar().getResultStream();
      try {
        printLine(out, "Print whatever you want");
      } catch (IOException io) {
        io.printStackTrace();
      }
    }

  }

  public void initializeParameter(int index, double value, double min, double max) {
    parameterField[index].setValue(value);
    parameterField[index].setValueMin(ParameterPreferences.getDouble(parameterField[index].getLabel() + ".min", min));
    parameterField[index].setValueMax(ParameterPreferences.getDouble(parameterField[index].getLabel() + ".max", max));
    if (min >= 0.0 && max > min)
      parameterField[index].setPositiveOnly();
  }
}



