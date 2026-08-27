/*
 * @(#)PrasadLeleDhcpFaultingModel.java created Aug 10, 2026 Los Alamos
 *
 * Copyright (c) 2026 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.sizestrain;

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.awt.Utility;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import org.diffax.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Vector;

/**
 * The PrasadLeleDhcpFaultingModel is a class using DiffaX
 * to calculate the pattern
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Aug 10, 2026 19:03:00 AM $
 * @since JDK1.1
 */


public class PrasadLeleDhcpFaultingModel extends PlanarDefects {

  public static String[] diclistc = {
      "_diffax_number_divisions_c",
      "_diffax_use_crystallite",
      "_riet_deformation_fault_intrinsic_c",
      "_riet_deformation_fault_intrinsic_h",
      "_riet_deformation_fault_intrinsic_2c",
      "_riet_deformation_fault_intrinsic_2h",
      "_riet_deformation_fault_intrinsic_3c",
      "_riet_deformation_fault_intrinsic_3h",
      "_riet_deformation_fault_intrinsic_ch",
      "_riet_deformation_fault_extrinsic_4c",
      "_riet_deformation_fault_extrinsic_cch",
      "_diffax_fats_waller_c11",
      "_diffax_fats_waller_c22",
      "_diffax_fats_waller_c33",
      "_diffax_fats_waller_c12",
      "_diffax_fats_waller_c13",
      "_diffax_fats_waller_c23",
      "_riet_par_caglioti_u",
      "_riet_par_caglioti_v",
      "_riet_par_caglioti_w",
      "_riet_par_gaussian_value",
      "_riet_par_gaussian_slope",
      "_diffax_density_correction"
  };
  public static String[] diclistcrm = {
      "_diffax_number_divisions_c",
      "_diffax_use_crystallite",
      "_riet_deformation_fault_intrinsic_c",
      "_riet_deformation_fault_intrinsic_h",
      "_riet_deformation_fault_intrinsic_2c",
      "_riet_deformation_fault_intrinsic_2h",
      "_riet_deformation_fault_intrinsic_3c",
      "_riet_deformation_fault_intrinsic_3h",
      "_riet_deformation_fault_intrinsic_ch",
      "_riet_deformation_fault_extrinsic_4c",
      "_riet_deformation_fault_extrinsic_cch",
      "_diffax_fats_waller_c11",
      "_diffax_fats_waller_c22",
      "_diffax_fats_waller_c33",
      "_diffax_fats_waller_c12",
      "_diffax_fats_waller_c13",
      "_diffax_fats_waller_c23",
      "_riet_par_caglioti_u",
      "_riet_par_caglioti_v",
      "_riet_par_caglioti_w",
      "_riet_par_gaussian_value",
      "_riet_par_gaussian_slope",
      "_diffax_density_correction"
  };

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  public PrasadLeleDhcpFaultingModel(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Prasad-Lele dhcp DiffaX";
    IDlabel = "Prasad-Lele dhcp DiffaX";
    description = "select this to use the Prasad-Lele DiffaX model for planar defects in dhcp structures";
  }

  public PrasadLeleDhcpFaultingModel(XRDcat aobj) {
    this(aobj, "Prasad-Lele dhcp DiffaX");
  }

  public PrasadLeleDhcpFaultingModel() {
    identifier = "Prasad-Lele dhcp DiffaX";
    IDlabel = "Prasad-Lele dhcp DiffaX";
    description = "select this to use the Prasad-Lele DiffaX model for planar defects in dhcp structures";
  }

  public void initConstant() {
    Nstring = 2;
    Nstringloop = 0;
    Nparameter = 21;
    Nparameterloop = 0;
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

    setString(0, "4");
    setString(1, "false");
    for (int i = 0; i < 9; i++) {
      parameterField[i] = new Parameter(this, getParameterString(i), 0.0,
          ParameterPreferences.getDouble(getParameterString(i) + ".min", 0.0),
          ParameterPreferences.getDouble(getParameterString(i) + ".max", 1.0));
      parameterField[i].setPositiveOnly();
      parameterField[i].setMinimumSignificantValue(0.0001);
    }
    for (int i = 9; i < 15; i++) {
      parameterField[i] = new Parameter(this, getParameterString(i), 0.0,
          ParameterPreferences.getDouble(getParameterString(i) + ".min", -10.0),
          ParameterPreferences.getDouble(getParameterString(i) + ".max", 10.0));
    }
    for (int i = 15; i < 18; i++) {
      parameterField[i] = new Parameter(this, getParameterString(i), 0.0001,
          ParameterPreferences.getDouble(getParameterString(i) + ".min", 0.0),
          ParameterPreferences.getDouble(getParameterString(i) + ".max", 1.0));
    }
    for (int i = 18; i < 20; i++) {
      parameterField[i] = new Parameter(this, getParameterString(i), 0.0,
          ParameterPreferences.getDouble(getParameterString(i) + ".min", 0.0),
          ParameterPreferences.getDouble(getParameterString(i) + ".max", 1.0));
    }
    parameterField[20] = new Parameter(this, getParameterString(20), 2.0,
        ParameterPreferences.getDouble(getParameterString(20) + ".min", 0.2),
        ParameterPreferences.getDouble(getParameterString(20) + ".max", 20.0));
    parameterField[20].setPositiveOnly();
    parameterField[20].setMinimumSignificantValue(0.0001);
  }

  public double getDensityCorrection() {
    return getParameterValue(20);
  }

  public boolean useCrystallite() {
    return Boolean.parseBoolean(getString(1));
  }

  public void setUseCrystallite(boolean value) {
    if (value)
      setString(1, "true");
    else
      setString(1, "false");
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);
    for (int i = 0; i < 9; i++) {
      parameterField[i].setPositiveOnly();
      parameterField[i].setMinimumSignificantValue(0.0001);
    }
    applyNormalization();
  }

  public void refreshForNotificationDown(XRDcat source, int reason) {
    boolean sourceIsPhase = source == getParent();
    XRDcat parent = source.getParent();
    while (!sourceIsPhase && parent != null) {
      sourceIsPhase = parent == getParent();
      parent = parent.getParent();
    }
//    if (reason == Constants.CELL_CHANGED)
//      System.out.println("Source: " + source.thelabel + " "  + sourceIsPhase);
    if (!getFilePar().isComputingDerivate() || sourceIsPhase)
      refreshComputation = true;
  }

  double[][][] datap = null;
  double[] layerWidth = null;
  double[] v = new double[5];

  public double[][][] getPowderPattern(DataFileSet adataset) {

    if (!refreshComputation)
      return datap;

//    boolean fromFile = MaudPreferences.getBoolean("diffax.loadInputFromFile", false);
    Phase aphase = (Phase) getParent();
    aphase.lattice(); // for the angles computation
    Texture textureModel = aphase.getActiveTexture();
    boolean hasTexture = !textureModel.isRandomTexture();
    if (hasTexture)
      textureModel.initQuickComputation();

    Vector<AtomSite> atomList = aphase.fullAtomList;
    Vector<DiffaxAtom> datomList = new Vector<>(atomList.size());
    ArrayList<String> list = new ArrayList<String>();
    for (int i = 0; i < atomList.size(); i++) {
      AtomSite atom = atomList.get(i);
      double bIso = atom.getBfactorValue();
      double occ = atom.getOccupancyValue();
      double[] coord = atom.getCoordinates(0);
      for (int j = 0; j < atom.getNumberOfScatterers(); j++) {
        AtomScatterer scat = atom.getAtomScatterer(j);
        double pocc = scat.getOccupancy() * occ;
        String label = scat.getAtomSymbol();
        if (!list.contains(label))
          list.add(label);
        DiffaxAtom datom = new DiffaxAtom(label, coord[0], coord[1], coord[2], bIso, pocc);
        datomList.add(datom);
      }
    }

    double[] rangeStep = adataset.getMaximumRangeAndMinimumStep();
    int multiplyStepN = MaudPreferences.getInteger("diffax.dividePatternStepBy", 4);
    rangeStep[2] /= multiplyStepN;
    int np = (int) Math.abs((rangeStep[1] - rangeStep[0]) / rangeStep[2]) + 1;

/*

    RadiationType radType = adataset.getInstrument().getRadiationType();
    int radiationType = DiffaxModel.X_RAY;
    if (radType.isNeutron())
      radiationType = DiffaxModel.NEUTRN;
    if (radType.isElectron())
      radiationType = DiffaxModel.ELECTN;

    double singleLambda = radType.getMeanRadiationWavelength();
    InstrumentBroadeningPVCaglioti instBroad = (InstrumentBroadeningPVCaglioti) adataset.getInstrument().getInstrumentBroadening();

*/

    String pointGroupKey = "6/MMM";
    double rest = 1.0;
    layerWidth = null;  // to take out the effect as it is a bit strange for large crystallites
    boolean useCryst = useCrystallite();
    double criticalCrystallite = MaudPreferences.getDouble("diffax.criticalCrystallite", 10000.0);
    double meanCryst = aphase.getMeanCrystallite();
    if (useCryst && meanCryst <= criticalCrystallite) {
      layerWidth = new double[2];
      layerWidth[0] = meanCryst;
      layerWidth[1] = meanCryst;
//      InstrumentBroadeningPVCaglioti instBroad = (InstrumentBroadeningPVCaglioti) adataset.getInstrument().getInstrumentBroadening();
      for (int i = 0; i < 5; i++)
        v[i] = getParameterValue(15 + i);
//      for (int i = 0; i < 3; i++)
//        v[i] = instBroad.getCaglioti(i).getValueD();
//        v[3] = instBroad.getGaussian(0).getValueD() + instBroad.getGaussian(1).getValueD() * Math.abs(rangeStep[1] - rangeStep[0]) / 2;
    } else {
      for (int i = 0; i < 5; i++)
        v[i] = getParameterValue(15 + i);
    }

    Vector<DiffaxLayer> layers = new Vector<>(BetaCePrasadLeleRefinement.STATE_COUNT);
    DiffaxLayer firstLayer = new DiffaxLayer(true);
    firstLayer.setAtomList(datomList);
    layers.add(firstLayer);
    for (int i = 0; i < BetaCePrasadLeleRefinement.STATE_COUNT - 1; i++)
      layers.add(new DiffaxLayer(true, layers.elementAt(0)));

    boolean recursiveMode = true;
    int[] numberStacking = null;

    double[][][] transitions = {
        {{0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
     , {1.0, 0.666667, 0.333333, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}}

    , {{0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
     , {1.0, 0.666667, 0.333333, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}}

    , {{0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
     , {1.0, -0.666667, -0.333333, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}}

    , {{0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
     , {1.0, -0.666667, -0.333333, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}}

    , {{0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
     , {1.0, -0.666667, -0.333333, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}}

    , {{0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
     , {1.0, -0.666667, -0.333333, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}}

     , {{1.0, 0.666667, 0.333333, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}}

     , {{1.0, 0.666667, 0.333333, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}}

    , {{0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
     , {1.0, 0.666667, 0.333333, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}}

    , {{0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
     , {1.0, -0.666667, -0.333333, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}}

    , {{0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
     , {1.0, -0.666667, -0.333333, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}}

    , {{0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
     , {1.0, 0.666667, 0.333333, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}}

    , {{0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
     , {1.0, 0.666667, 0.333333, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}}

    , {{0.0, 0.0, 0.0, 1.0}
     , {1.0, -0.666667, -0.333333, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}
    , {0.0, 0.0, 0.0, 1.0}}
    };
//    double[][][] transitions = new double[BetaCePrasadLeleRefinement.STATE_COUNT][BetaCePrasadLeleRefinement.STATE_COUNT][4];
    double[][][] fatsWaller = null;
    int cellDivisions = Integer.parseInt(getString(0)); // d-hco

    if (hasTexture) {
        int maxNumberThreads = Runtime.getRuntime().availableProcessors();
        int datafilenumber = adataset.activedatafilesnumber();
        final int maxThreads = Math.min(maxNumberThreads, datafilenumber);
        if (maxThreads > 1) { // ] && Constants.threadingGranularity >= Constants.MEDIUM_GRANULARITY) {
//          if (Constants.debugThreads)
//            System.out.println("Thread datafileset " + getLabel());
          int i;
          PersistentThread[] threads = new PersistentThread[maxThreads];
          for (i = 0; i < maxThreads; i++) {
            threads[i] = new PersistentThread(i) {
              @Override
              public void executeJob() {
                try {

                  int i1 = this.getJobNumberStart();
                  int i2 = this.getJobNumberEnd();

                  // -------------------------------------------------
                  DiffaxModel amodel = new DiffaxModel(aphase, adataset, BetaCePrasadLeleRefinement.STATE_COUNT, datomList.size(),
                      list.size(), np + 1, 1, 16);
                  amodel.loadModel(cellDivisions, v, pointGroupKey, rest,
                      layerWidth, layers, recursiveMode, numberStacking,
                      transitions, fatsWaller, multiplyStepN);
                  BetaCePrasadLeleRefinement ce = new BetaCePrasadLeleRefinement(amodel);
                  BetaCePrasadLeleRefinement.Parameters p =
                      new BetaCePrasadLeleRefinement.Parameters(normalizedToPhysicalRegion());
                  double[] parp = new double[6];
                  for (int i = 0; i < parp.length; i++)
                    parp[i] = getParameterValue(i + 9);
                  ce.apply(p, parp);

                  for (int j = i1; j < i2; j++) {
//                  for (int j = 0; j < adataset.activedatafilesnumber(); j++) {
                    amodel.activeDatafileNumber = j;
                    DiffaxModel.PowderResult r = ce.powder(rangeStep[0], rangeStep[1], rangeStep[2], true);
                    if (datap == null)
                      datap = new double[adataset.activedatafilesnumber()][2][r.twoThetaDeg.length];
                    for (int i = 0; i < r.twoThetaDeg.length; i++) {
                      datap[j][0][i] = r.twoThetaDeg[i];
                      datap[j][1][i] = r.broadenedIntensity[i];
                    }
                  }
                  // -------------------------------------------------
                } catch (Exception e) {
                  e.printStackTrace();
                }

              }
            };
          }
          i = 0;
          int istep = (int) (0.9999 + datafilenumber / maxThreads);
          for (int j = 0; j < maxThreads; j++) {
            int is = i;
            if (j < maxThreads - 1)
              i = Math.min(i + istep, datafilenumber);
            else
              i = datafilenumber;
            threads[j].setJobRange(is, i);
            threads[j].start();
          }
          boolean running;
          do {
            running = false;
            try {
              Thread.sleep(Constants.timeToWaitThreadsEnding);
            } catch (InterruptedException r) {
              r.printStackTrace();
            }
            for (int h = 0; h < maxThreads; h++) {
              if (!threads[h].isEnded())
                running = true;
            }
          } while (running);

        } else {
          try {
            DiffaxModel amodel = new DiffaxModel(aphase, adataset, BetaCePrasadLeleRefinement.STATE_COUNT, datomList.size(),
                list.size(), np + 1, 1, 16);
            amodel.loadModel(cellDivisions, v, pointGroupKey, rest,
                layerWidth, layers, recursiveMode, numberStacking,
                transitions, fatsWaller, multiplyStepN);
            BetaCePrasadLeleRefinement ce = new BetaCePrasadLeleRefinement(amodel);
            BetaCePrasadLeleRefinement.Parameters p =
                new BetaCePrasadLeleRefinement.Parameters(normalizedToPhysicalRegion());
            double[] parp = new double[6];
            for (int i = 0; i < parp.length; i++)
              parp[i] = getParameterValue(i + 9);
            ce.apply(p, parp);

            for (int j = 0; j < datafilenumber; j++) {
              amodel.activeDatafileNumber = j;
              DiffaxModel.PowderResult r = ce.powder(rangeStep[0], rangeStep[1], rangeStep[2], true);
              if (datap == null)
                datap = new double[adataset.activedatafilesnumber()][2][r.twoThetaDeg.length];
              for (int i = 0; i < r.twoThetaDeg.length; i++) {
                datap[j][0][i] = r.twoThetaDeg[i];
                datap[j][1][i] = r.broadenedIntensity[i];
              }
            }
            // -------------------------------------------------
          } catch (Exception e) {
            e.printStackTrace();
          }
        }

    } else {
      try {
        DiffaxModel amodel = new DiffaxModel(aphase, adataset, BetaCePrasadLeleRefinement.STATE_COUNT, datomList.size(),
              list.size(), np + 1, 1, 16);
        amodel.loadModel(cellDivisions, v, pointGroupKey, rest,
              layerWidth, layers, recursiveMode, numberStacking,
              transitions, fatsWaller, multiplyStepN);
        BetaCePrasadLeleRefinement ce = new BetaCePrasadLeleRefinement(amodel);
        BetaCePrasadLeleRefinement.Parameters p =
            new BetaCePrasadLeleRefinement.Parameters(normalizedToPhysicalRegion());
        double[] parp = new double[6];
        for (int i = 0; i < parp.length; i++)
          parp[i] = getParameterValue(i + 9);
        ce.apply(p, parp);

        DiffaxModel.PowderResult r = ce.powder(rangeStep[0], rangeStep[1], rangeStep[2], true);
        datap = new double[1][2][r.twoThetaDeg.length];
        for (int i = 0; i < r.twoThetaDeg.length; i++) {
          datap[0][0][i] = r.twoThetaDeg[i];
          datap[0][1][i] = r.broadenedIntensity[i];
        }
      } catch (Exception e) {
        e.printStackTrace();
      }
    }
    refreshComputation = false;
    return datap;
  }

  public Vector<double[][]> getTransitions() {
    getFilePar().refreshAll(true);
    Vector<double[][]> result = new Vector<>(BetaCePrasadLeleRefinement.STATE_COUNT);
    Phase aphase = (Phase) getParent();
    aphase.refreshAtoms();
    Vector<AtomSite> atomList = aphase.fullAtomList;
    Vector<DiffaxAtom> datomList = new Vector<>(atomList.size());
    ArrayList<String> list = new ArrayList<String>();
    for (int i = 0; i < atomList.size(); i++) {
      AtomSite atom = atomList.get(i);
      double bIso = atom.getBfactorValue();
      double occ = atom.getOccupancyValue();
      double[] coord = atom.getCoordinates(0);
      for (int j = 0; j < atom.getNumberOfScatterers(); j++) {
        AtomScatterer scat = atom.getAtomScatterer(j);
        double pocc = scat.getOccupancy() * occ;
        String label = scat.getAtomSymbol();
        if (!list.contains(label))
          list.add(label);
        DiffaxAtom datom = new DiffaxAtom(label, coord[0], coord[1], coord[2], bIso, pocc);
        datomList.add(datom);
      }
    }

    int multiplyStepN = MaudPreferences.getInteger("diffax.dividePatternStepBy", 4);

    DiffaxModel amodel = new DiffaxModel(aphase, null, BetaCePrasadLeleRefinement.STATE_COUNT,
        datomList.size(), list.size(), 100, 100, 16);
    for (int i = 0; i < 5; i++)
      v[i] = getParameterValue(15 + i); // instBroad.getCaglioti(i).getValueD();

    String pointGroupKey = "6/MMM";
    double rest = 1.0;
    double[] layerWidth = null;  // to take out the effect as it is a bit strange for large crystallites

    Vector<DiffaxLayer> layers = new Vector<>(BetaCePrasadLeleRefinement.STATE_COUNT);
    DiffaxLayer firstLayer = new DiffaxLayer(true);
    firstLayer.setAtomList(datomList);
    layers.add(firstLayer);
    for (int i = 0; i < BetaCePrasadLeleRefinement.STATE_COUNT - 1; i++)
      layers.add(new DiffaxLayer(true, layers.elementAt(0)));

    boolean recursiveMode = true;
    int[] numberStacking = null;

    double[][][] transitions = {
        {{0.0, 0.0, 0.0, 1.0}
            , {0.0, 0.0, 0.0, 1.0}
            , {1.0, 0.666667, 0.333333, 1.0}
            , {0.0, 0.0, 0.0, 1.0}
            , {0.0, 0.0, 0.0, 1.0}
            , {0.0, 0.0, 0.0, 1.0}
            , {0.0, 0.0, 0.0, 1.0}
            , {0.0, 0.0, 0.0, 1.0}
            , {0.0, 0.0, 0.0, 1.0}
            , {0.0, 0.0, 0.0, 1.0}
            , {0.0, 0.0, 0.0, 1.0}
            , {0.0, 0.0, 0.0, 1.0}
            , {0.0, 0.0, 0.0, 1.0}
            , {0.0, 0.0, 0.0, 1.0}}

        , {{0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {1.0, 0.666667, 0.333333, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}}

        , {{0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {1.0, -0.666667, -0.333333, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}}

        , {{0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {1.0, -0.666667, -0.333333, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}}

        , {{0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {1.0, -0.666667, -0.333333, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}}

        , {{0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {1.0, -0.666667, -0.333333, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}}

        , {{1.0, 0.666667, 0.333333, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}}

        , {{1.0, 0.666667, 0.333333, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}}

        , {{0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {1.0, 0.666667, 0.333333, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}}

        , {{0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {1.0, -0.666667, -0.333333, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}}

        , {{0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {1.0, -0.666667, -0.333333, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}}

        , {{0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {1.0, 0.666667, 0.333333, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}}

        , {{0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {1.0, 0.666667, 0.333333, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}}

        , {{0.0, 0.0, 0.0, 1.0}
        , {1.0, -0.666667, -0.333333, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}
        , {0.0, 0.0, 0.0, 1.0}}
    };
//    double[][][] transitions = new double[BetaCePrasadLeleRefinement.STATE_COUNT][BetaCePrasadLeleRefinement.STATE_COUNT][4];
    double[][][] fatsWaller = null;
    int cellDivisions = Integer.parseInt(getString(0)); // d-hco

    try {
        amodel.loadModel(cellDivisions, v, pointGroupKey, rest,
            layerWidth, layers, recursiveMode, numberStacking,
            transitions, fatsWaller, multiplyStepN);

      BetaCePrasadLeleRefinement ce = new BetaCePrasadLeleRefinement(amodel);

      // Example: start by refining only the visually dominant c fault.
      BetaCePrasadLeleRefinement.Parameters p =
          new BetaCePrasadLeleRefinement.Parameters(normalizedToPhysicalRegion());
      double[] parp = new double[6];
      for (int i = 0; i < parp.length; i++)
        parp[i] = getParameterValue(i + 9);
      ce.apply(p, parp);

      // DiffaxModel.PowderResult r = ce.powder(rangeStep[0], rangeStep[1], rangeStep[2], true);

      for (int from=1; from<=ce.STATE_COUNT; from++) {
        double[][] lP = new double[ce.STATE_COUNT][4];
        for (int to=1; to<=ce.STATE_COUNT; to++) {
          lP[to - 1][0] = amodel.lAlpha[to][from];
          lP[to - 1][1] = amodel.lR[1][to][from];
          lP[to - 1][2] = amodel.lR[2][to][from];
          lP[to - 1][3] = amodel.lR[3][to][from];
        }
        result.add(lP);
      }
      double[] g = ce.statePopulations();

      for (int i = 0; i < g.length; i++) {
        System.out.printf("state %2d  G = %.12g%n", i + 1, g[i]);
      }

    } catch (Exception e) {
      e.printStackTrace();
    }
    return result;
  }

  public void applyNormalization() {
    double[] norm = normalizedToPhysicalRegion();
    for (int i = 0; i < norm.length; i++)
      parameterField[i].setValue(norm[i]);
  }

  public double[] normalizedToPhysicalRegion() {
    double[] faults = new double[9];
    for (int i = 0; i < faults.length; i++)
      faults[i] = Math.abs(getParameter(i).getValueD());
    double s = Math.max(cTypeFaultSum(faults), hTypeFaultSum(faults));
    if (s <= 1.0) return faults;
    double f = (1.0 - 1.0e-12) / s;
    for (int i = 0; i < faults.length; i++)
      faults[i] = faults[i] * f;
    return faults;
  }

  public double cTypeFaultSum(double[] faults) {
    return faults[1] + faults[3] + faults[5] + faults[6] + faults[8];
  }

  public double hTypeFaultSum(double[] faults) {
    return faults[0] + faults[2] + faults[4] + faults[6] + faults[7] + faults[8];
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new PrasadLeleDhcpFaultingModel.PLDMOptionsD(parent, this);
    return adialog;
  }

  class PLDMOptionsD extends JOptionsDialog {

    JComboBox ssmodelCB = null;
    JCheckBox textureCB;
    JTextField[] pars = null;
    JTextField[] cijTF = null;
    JCheckBox useCrystalliteCB = null;

    String[] faultslabels = {
        "Intrinsic c : ",
        "Intrinsic h : ",
        "Intrinsic 2c : ",
        "Intrinsic 2h : ",
        "Intrinsic 3c : ",
        "Intrinsic 3h : ",
        "Intrinsic ch : ",
        "Intrinsic 4c : ",
        "Intrinsic cch : "};

    String[] fwlabels = {
        " Fats-Waller_11 : ",
        " Fats-Waller_22 : ",
        " Fats-Waller_33 : ",
        " Fats-Waller_12 : ",
        " Fats-Waller_13 : ",
        " Fats-Waller_23 : "};

    public PLDMOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      pars = new JTextField[faultslabels.length];

      principalPanel.setLayout(new BorderLayout(6, 6));
      JPanel jPaneln = new JPanel();
      jPaneln.setLayout(new BorderLayout(6, 6));
      JPanel jPanel1 = new JPanel();
      jPanel1.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      useCrystalliteCB = new JCheckBox("Use Crystallite for layer width ");
      useCrystalliteCB.setSelected(useCrystallite());
      jPanel1.add(useCrystalliteCB);

      principalPanel.add(BorderLayout.NORTH, jPaneln);
      jPaneln.add(jPanel1, BorderLayout.CENTER);

      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.CENTER, jPanel8);

      for (int i = 0; i < faultslabels.length; i++) {
        JPanel jpl = new JPanel();
        jpl.setLayout(new FlowLayout(FlowLayout.LEFT));
        jPanel8.add(jpl);
        jpl.add(new JLabel(faultslabels[i]));
        pars[i] = new JTextField(Constants.FLOAT_FIELD);
        pars[i].setText("0");
        jpl.add(pars[i]);
      }

      JButton exportTransitionsButton = new JButton("Export Transitions");
      exportTransitionsButton.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          final String filename = Utility.browseFilenametoSave(PLDMOptionsD.this, "Choose a file for export");
          (new PersistentThread() {
            public void executeJob() {
              Vector<double[][]> transitions = getTransitions();

              BufferedWriter output = null;
              if (filename != null) {
                try {
                  output = Misc.getWriter(filename);
                  output.write("{ Transitions }");
                  output.newLine();
                  for (int i = 0; i < transitions.size(); i++) {
                    output.write("{ From layer " + i + " }");
                    output.newLine();
                    double[][] trans = transitions.elementAt(i);
                    for (int j = 0; j < trans.length; j++) {
                      for (int k = 0; k < trans[j].length; k++) {
                        output.write(trans[j][k] + "    ");
                      }
                      output.newLine();
                    }
                    output.newLine();
                  }
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
            }
          }).start();
        }
      });
      JPanel jpl = new JPanel();
      jpl.setLayout(new FlowLayout(FlowLayout.LEFT));
      jPanel8.add(jpl);
      jpl.add(exportTransitionsButton);


      setTitle("Moment pole figures options panel");
      initParameters();
      pack();

    }

    public void initParameters() {
      for (int i = 0; i < faultslabels.length; i++) {
//        pars[i].setText(parameterField[i + 21].getValue());
        addComponenttolist(pars[i], parameterField[i]);
      }
    }

    public void retrieveParameters() {
      for (int i = 0; i < faultslabels.length; i++) {
        parameterField[i].setValue(pars[i].getText());
      }
      setUseCrystallite(useCrystalliteCB.isSelected());
    }

  }

}

