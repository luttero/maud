/*
 * @(#)DiffaXLayerModel.java created Jun 30, 2009 Caen
 *
 * Copyright (c) 2009 Luca Lutterotti All Rights Reserved.
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
 * The DiffaXLayerModel is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Aug 25, 2026 11:14:24 AM $
 * @since JDK1.1
 */


public class DiffaXLayerModel extends PlanarDefects {

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

  public DiffaXLayerModel(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "DiffaX Layers";
    IDlabel = "DiffaX Layers";
    description = "select this to use the DiffaX model for planar defects";
  }

  public DiffaXLayerModel(XRDcat aobj) {
    this(aobj, "DiffaX");
  }

  public DiffaXLayerModel() {
    identifier = "DiffaX Layers";
    IDlabel = "DiffaX Layers";
    description = "select this to use the DiffaX model for planar defects";
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

//    int multiplyStepN = MaudPreferences.getInteger("diffax.dividePatternStepBy", 4);

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
          transitions, fatsWaller, 4);

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
    JOptionsDialog adialog = new DiffaXLayerModel.DMOptionsD(parent, this);
    return adialog;
  }

  class DMOptionsD extends JOptionsDialog {

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

    public DMOptionsD(Frame parent, XRDcat obj) {

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
          final String filename = Utility.browseFilenametoSave(DiffaXLayerModel.DMOptionsD.this, "Choose a file for export");
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


// ***********************************************************************
//                                                                      *
//     Copyright 1987-2002 Michael M. J. Treacy and Michael W. Deem     *
//                                                                      *
// ***********************************************************************
// ***********************************************************************
// *******************      Source file DIFFaX.f       *******************
// ***********************************************************************
// ***********************************************************************
// ******************** version 1.812, 3rd July, 2005 ********************
// ***********************************************************************
// ***********************************************************************
// This program calculates the powder diffraction spectrum of a crystal *
// formed from layers which stack coherently, but not deterministically.*
// The algorithm used is described in detail in "A General Recursion    *
// Method for Calculating Diffracted Intensities From Crystals          *
// Containing Planar Faults", by M.M.J. Treacy, M.W. Deem and           *
// J.M. Newsam, Proceedings of the Royal Society, London, A (1991) 433, *
// pp 499 - 520.                                                        *
//                                                                      *
// Source code written by Michael M. J. Treacy and Michael W. Deem      *
//                                                                      *
// HISTORY:                                                             *
// 6/87-5/88; MMJT: Original versions were named 'betaPXD' and 'FauPXD'.*
// They were 'hardwired' for simulating PXD patterns from zeolite beta, *
// and the faujasite/'Breck's structure 6' zeolite family.              *
//                                                                      *
// 5-8/88; MWD: Completely rewritten in generalized form for Cray and   *
// VAX, and named 'PDS'.                                                *
//                                                                      *
// 8/88-11/89; MMJT: Control file option added. Improved handling of    *
// sharp peaks. Symmetry testing added. Layer 'stacking uncertainty'    *
// factors added. Selected area electron diffraction option added.      *
// Explicit layer sequencing option. Optimization of layer form factor  *
// calculations. Renamed 'DIFFaX' for, (D)iffraction (I)ntensities      *
// (F)rom (Fa)ulted (X)tals.                                            *
//                                                                      *
// 12/89 - 3/90; MMJT: Finite crystal thickness now accepted under      *
//              'RECURSIVE' option. Self-consistency check of atomic    *
//               coordinates in data file. (v1.76)                      *
//                                                                      *
// 4/90 - 12/90; MMJT: Minor bug fixes. Streamlined 'data.sfc' file.    *
//               (v1.761 and v1.762)                                    *
//                                                                      *
// 1/91; MMJT: Eliminated the use of scratch file while reading data.   *
//             GETLNE now handles multiple, nested, comments (v1.763)   *
//                                                                      *
// 5/91; MMJT: Eliminated bug in default tolerance parameter. Added     *
//             average crystal composition printout to dump file.       *
//             (v1.764)                                                 *
//                                                                      *
// 8/91; MMJT: Replaced the LU decomposition routines CLUDCM and CLUBKS *
//             (from "Numerical Recipes") with the faster linpack       *
//             routines, CGEFA and CGESL (v1.765)                       *
//                                                                      *
// 8/91; MMJT: Improved sharp peak detection - use peak widths rather   *
//             than the more complicated phase coherence argument in    *
//             subroutine SHARP (v1.766)                                *
//                                                                      *
// 4/92; MMJT: Fixed bug in INTEN2 where last layer in an explicit      *
//             sequence was inadvertently assigned a scattering factor  *
//             of C_ONE. Improved error checking for explicit layers so *
//             that when alpha(j,i) = 0, an error is issued if j        *
//             follows i. XP_MAX increased to 5000. GETLNE now checks   *
//             that data lines do not exceed maximum length. (v1.767)   *
//                                                                      *
// 12/94; MMJT:Reinstated the use of the scratch file that had been     *
//             eliminated in v1.763. The Cray, and Microsoft fortran    *
//             compiler for PC, adhere to the FORTRAN77 standard and    *
//             do not allow unformatted reads/writes from/to strings.   *
//                                                                      *
// 1/95; MMJT: Finessed the diffraction symmetry detection routines.    *
//             Introduced the subroutine THRESH. (v1.769)               *
//                                                                      *
// 2/95; MMJT: Fixed glitches in THRESH, TST_MIR and TST_ROT that were  *
//             introduced in the 1/95 fix. (still v1.769)               *
//                                                                      *
// 3/95; MMJT: Implemented Debye-Scherrer type broadening due to finite *
//             lateral layer widths. Added CHWDTH, RDWDTH. Modified the *
//             way the powder pattern is written to the array spec, so  *
//             that the low angle intensity begins at spec(1). AtomSite     *
//             names are now case insensitive. The layer Bij factors    *
//             were reordered - B23, B31 become B13, B23.               *
//             GET_G modified to handle singularities better. (v1.80)   *
//                                                                      *
// 7/95; MMJT: Fixed rare zero integration range bug in GLQ16.          *
//             Fixed "fatsWalla" bug in GET_MAT.  (v1.801)              *
//                                                                      *
// 5/96; MMJT: Fixed a bug in the LL() function in INTEGR() which was   *
//             introduced by a "cosmetic" change in v1.801.  (v1.802)   *
//                                                                      *
// 10/96; MMJT:Changed eps3 to eps5 in CHWDTH function so that the      *
//             broadening tails extend further.  (v1.803)               *
//                                                                      *
// 7/97; MMJT: Added subroutines NXTARG and RDNMBR. These allow data    *
//             to be entered as fractions (ie 1/3). Improved robustness *
//             of the "fatswalla" interlayer uncertainty code. (v1.804) *
//                                                                      *
// 6/98; MMJT: Fixed bug in PV() that was introduced in v1.80. (v1.805) *
//                                                                      *
// 3/00; MMJT: Now allow 16-bit deep SADPs. (v1.806)                    *
//                                                                      *
// 8/00; MMJT: RDSTAK changed so that if a stacking probability is zero,*
//              the rest of the line is ignored.        (v1.807)        *
//                                                                      *
// 4/02; MMJT: Removed calls to iidnnt in WRTSADP.      (v1.808)        *
//                                                                      *
// 2/03; MMJT: Halved the value of ffhkcnst in GETSPC. The half-width   *
//             was being used instead of the FWHM, which made the shape *
//             broadening twice as large as it should be. (v1.809)      *
//                                                                      *
// 3/04; MMJT: Fixed a minor printing bug in TST_MIR      (v1.810)      *
//                                                                      *
// 1/05; MMJT: Fixed some f77 compiler compatibility bugs.  (v1.811)    *
//                                                                      *
// 7/05; MMJT: Fixed a bug in EQUALB that caused DIFFaX to ignore the   *
//             sign of the Fats-Waller Bij terms.  (v1.812)             *
//                                                                      *
// ***********************************************************************
// ***************************** Legal Note ******************************
// ***********************************************************************
//                                                                      *
// * * * * * * * * * *  DISCLAIMER OF WARRANTIES: * * * * * * * * * * * *
//                                                                      *
// The authors make no warranties whatsoever, express or implied, with  *
// respect to the DIFFaX software or any of its parts, nor do they      *
// warrant that the DIFFaX software, or any of its parts, will be       *
// error-free, will operate without interruption, or will be compatible *
// with any software or hardware possessed by the user.                 *
//                                                                      *
// * * * * * * * * * *  LIMITATION OF LIABILITY:  * * * * * * * * * * * *
//                                                                      *
// The authors will not be liable for any special, incidental, or       *
// consequential damages, even if informed of the possibility of such   *
// damages in advance.                                                  *
//                                                                      *
// ***********************************************************************
// ************************** DIFFaX file i/o. ***************************
// ***********************************************************************
//                                                                      *
// * * * * OPTIONAL CONTROLFILE FOR AUTOMATIC RUNNING OF DIFFaX * * * * *
//                                                                      *
// DIFFaX first searches the current directory for a control file named *
// 'control.dif'. If it finds this file it opens it on unit 'cntrl'     *
// and this becomes the default input unit. Structure filenames,        *
// and the various parameters (which would normally be requested        *
// interactively) obviously must be in the correct sequence. The data   *
// read from 'control' is echoed on the default output device (ie. the  *
// screen, unit 'op') so the user can check that the responses          *
// are properly synchronized. If 'control.dif' does not exist, the      *
// default input device is the keyboard (unit number 'ip'), and the     *
// user is expected to answer the prompts. DIFFaX will loop through the *
// contents of 'control', and thus can be used to rerun DIFFaX on fresh *
// data files, without quitting. Under direction from a control file,   *
// normal termination of DIFFaX occurs when a filename 'END' is         *
// encountered. Interactively, DIFFaX will end normally when the user   *
// chooses not to return to the function menu.                          *
// The name of the control file is stored in the global character       *
// variable 'cfname', and is assigned in 'main'.                        *
//                                                                      *
//                                                                      *
// * * * * * * * * * * *  STRUCTURE INPUT FILE  * * * * * * * * * * * * *
//                                                                      *
// The structure input file is opened on unit 'df'. It can have any     *
// name except 'END' (case insensitive). For clarity it may be best to  *
// keep it short (less than 8 characters) and (optionally) with '.dat'  *
// appended. Output files use the input name up to the first blank      *
// (' ') or period ('.') as their root name. Thus, if 'beta.dat'        *
// (or 'beta') is the data input file name, then 'beta.spc' etc... will *
// be the form of the output file names.                                *
//                                                                      *
//                                                                      *
// * * * * * * * STRUCTURE FACTOR PARAMETER INPUT FILE  * * * * * * * * *
//                                                                      *
// The structure factor parameter file, 'data.sfc' is opened on unit    *
// 'sf'. If a file of name 'data.sfc' is not found, DIFFaX will abort.  *
// The name of the structure factor parameter file is stored in the     *
// global character variable 'sfname', and is assigned in 'main'.       *
//                                                                      *
//                                                                      *
// * * * * * * * * * * *  SPECTRUM OUTPUT FILE  * * * * * * * * * * * * *
//                                                                      *
// Spectra are output as text files on unit 'sp'. Each record contains  *
//        2theta     intensity     (instrumentally broadened intensity) *
// in tab-delimited format. 'Instrumentally broadened intensity' is     *
// output only if the pseudo-Voigt, Gaussian or Lorentzian options were *
// requested. Spectra output file names are in the form 'rootname.spc', *
// or alternatively, if that name is already taken, as 'rootname.spc#', *
// where #=1,2,3 etc...                                                 *
//                                                                      *
//                                                                      *
// * * * * * * * * * STREAK INTENSITIES OUTPUT FILE * * * * * * * * * * *
//                                                                      *
// Streak calculations are output on unit 'sk'. Streak output file      *
// names are in the form 'rootname.str', or alternatively, if that name *
// is already taken, as 'rootname.str#', where #=1,2,3 etc...           *
//                                                                      *
//                                                                      *
// * * *   SELECTED AREA DIFFRACTION PATTERN (SADP) OUTPUT FILE   * * * *
//                                                                      *
// Selected area diffraction pattern data is saved in binary format in  *
// a file named 'rootname.sadp' which is output on unit 'sad'. If that  *
// name is already taken, the alternative name 'rootname.sadp#' is      *
// used, where #=1,2,3 etc...                                           *
//                                                                      *
//                                                                      *
// * * * * * *  OPTIONAL DUMP FILE OF STRUCTURAL PARAMETERS * * * * * * *
//                                                                      *
// If the user requests a dump of the structure data file (as DIFFaX    *
// read it!) a dumpfile named 'rootname.dmp' is output on unit 'dp'. If *
// that name is already taken, the alternative name 'rootname.dmp#' is  *
// used, where #=1,2,3 etc...This is valuable for debugging the input   *
// data file.                                                           *
//                                                                      *
//                                                                      *
// * * * * * * *  OPTIONAL DUMP FILE OF INTENSITIES FOUND   * * * * * * *
// * * * * * * * WHEN EVALUATING DIFFRACTION POINT SYMMETRY * * * * * * *
//                                                                      *
// The user may also output the history of the intensity values found   *
// when DIFFaX attempts to establish the point group symmetry of the    *
// diffraction output. This is useful when debugging the datafile. The  *
// intensity data is saved in a file named 'rootname.sym' which is      *
// output on unit 'sy'. If that name is already taken, the alternative  *
// name 'rootname.sym#' is used, where #=1,2,3 etc...                   *
//                                                                      *
// ***********************************************************************
// ******************************* DIFFaX ********************************
// ***********************************************************************

//
// Title: DIFFaX
// Authors: MWD and MMJT
// Date: 23 Oct 1988
// Description: This is the main program. First, important global
// constants, such as PI, are defined. The name of the control file
// is assigned to cfname, and then FNDCTL searches for this file in the
// current directory. If found, the control file is opened and it
// becomes the default input device. If not found, then the keyboard is
// the standard input device. The user's data file, and the atomic
// scattering factor data file (whose name is contained in 'sfname')
// are then searched for in the current directory (GETFIL), and opened.
// The user's data file is then read (RDFILE). The standard scattering
// factor data file 'sfname' is then searched for data on the atom
// types specified by the user (SFC). The layer existence
// probabilities are calculated (GET_G). If the user data file
// requested EXPLICIT, RANDOM stacking, then DIFFaX computes a random
// layer sequence consistent with the stacking probabilities (GETLAY).
// Reciprocal lattice constants related to the unit cell are then
// calculated (SPHCST). If the user requested (either interactively,
// or through the control file) a dump of what DIFFaX read from the
// user's data file, then an annotated dump is generated (DUMP). DETUN
// then delicately adjusts the probability data so as to avoid zero
// determinants at the sharp peaks. The user is then asked if a dump
// of DIFFaX's symmetry evaluations is required, and then searches the
// data looking for simple opportunities to speed up the calculation
// (OPTIMZ). The user is then asked if he wants to calculate the
// intensity at a point (POINT), along a streak (GOSTRK), integrated
// within a defined interval (GOINTR), a powder pattern (GOSPEC) or
// a selected area diffraction pattern (GOSADP). If running
// interactively, the user can return to any of these menu options,
// except if GOSPEC was chosen, where DIFFaX will finish. If a control
// file is being used, then DIFFaX will return to the beginning if
// GOSPEC was chosen. If a new data file name is read then DIFFaX will
// run again. If the control file reads 'End' (case insensitive) as the
// new file name, then DIFFaX will finish.
// Note: The file names contained in 'cfname' and 'sfname', and the name
// 'End' are reserved names, and cannot be used by the user as data file
// names.

//      COMMON VARIABLES:
//            uses:  rndm, cntrl, CFile, SymGrpNo

//        modifies:  PI, PI2, DEG2RAD, RAD2DEG, DoDatdump,
//                   DoSymDump, cfname, sfname
//

// What type of intensity output does the user want? (operation)
// 0 POINT, 1 STREAK, 2 INTEGRATE, 3 POWDER PATTERN, 4 SADP
