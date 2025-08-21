/*
 * @(#)ExtinctionSabineModel.java created 20/08/2025 Los Alamos
 *
 * Copyright (c) 2025 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.sfm;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

/**
 * The ExtinctionSabineModel is a class for dynamical extinction
 * correction using Sabine model
 * See: SABINE, T. M., VON DREELE, R. B. & JORGENSEN,J.-E. (1988).
 * Acta Cryst. A44, 374-379
 *
 * @version $Revision: 1.0 $, $Date: 2025/08/20 11:24:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class ExtinctionSabineModel extends ExtinctionModel {
  protected static String[] diclistc = {"_refine_ls_extinction_coef"};
  protected static String[] diclistcrm = {"_refine_ls_extinction_coef"};
  protected static String[] classlistc = {};

  public ExtinctionSabineModel(XRDcat obj, String alabel) {
    super(obj, alabel);
    initBaseObject();
    identifier = "Sabine Extinction Model";
    IDlabel = "Sabine Extinction Model";
    description = "select this for Sabine extinction correction in structure factors";
  }

  public ExtinctionSabineModel(XRDcat afile) {
    this(afile, "Sabine Extinction Model");
  }

  public ExtinctionSabineModel() {
    identifier = "Sabine Extinction Model";
    IDlabel = "Sabine Extinction Model";
    description = "select this for Sabine extinction correction in structure factors";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 1;
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
  }

  public void initParameters() {
    super.initParameters();
    parameterField[0] = new Parameter(this, getParameterString(0), 1,
        ParameterPreferences.getDouble(getParameterString(0) + ".min", 0),
        ParameterPreferences.getDouble(getParameterString(0) + ".max", 100));
    parameterField[0].setPositiveOnly();
    parameterField[0].setMinimumSignificantValue(1.0E-6);
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
        for (int i = 0; i < parameterField.length; i++) {
          if (parameterField[i] == source) {
            notifyParameterChanged(source, Constants.STRUCTURE_FACTOR_CHANGED, -1);
            return;
          }
        }
      super.notifyParameterChanged(source);
    }
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(firstLoading);

    mosaicBlockSize = Math.abs(getParameterValue(0));
  }

  public boolean canCorrect(DataFileSet adataset) {
    return true;
  }

  double cellVolume2 = 1.0;
  double Efactor = 1.0;
  double mosaicBlockSize = 1000.0;

  public double getMosaicBlockSize() {return mosaicBlockSize;}

  public void preparecomputing() {
    Phase aphase = (Phase) getParent();
    cellVolume2 = aphase.getCellVolume();
    cellVolume2 *= cellVolume2;
    Efactor = getMosaicBlockSize() * getMosaicBlockSize() / cellVolume2;
  }

  public double getExtinctionCorrectionByThetaRadiants(double dspace, double structureFactor, double thetar) {
    double sintheta2 = Math.sin(thetar);
    double wavelength = 2.0 * dspace * sintheta2;
    sintheta2 *= sintheta2;
    double costheta2 = 1.0 - sintheta2;
    wavelength *= wavelength;
    double eCorr = wavelength * structureFactor;
    double x = Efactor * eCorr * 1.0E-4;
    double x2 = x * x;
    double x3 = x * x2;
    double x4 = x2 * x2;
    double eb = 1.0 / Math.sqrt(1.0 + x);
    double el = 0.0;
    if (x > 1) {
      double serie = 1.0 - 1.0 / (8.0 * x) - 3.0 / (128.0 * x2) - 15.0 / (1024.0 * x3);
      el = Math.sqrt(2.0 / (x * Constants.PI) * serie);
    } else
      el = 1.0 - x / 2.0 + x2 / 4.0 - 5.0 * x3 / 48.0 + 7.0 * x4 / 192;
System.out.println(dspace + " " + structureFactor + " " + (thetar * Constants.PITODEG) + " " + x + " " +
    eb + " " + el + " " + (eb * sintheta2 + el * costheta2));
    return eb * sintheta2 + el * costheta2;
  }

  public double getExtinctionCorrectionByWave(double dspace, double structureFactor, double wavelength) {
    double sintheta2 = wavelength / (dspace * 2.0);
    if (sintheta2 > 1.0)
      sintheta2 = 1.0;
    sintheta2 *= sintheta2;
    double costheta2 = 1.0 - sintheta2;
    wavelength *= wavelength;
    double eCorr = wavelength * structureFactor;
    double x = Efactor * eCorr * 1.0E-4;
    double x2 = x * x;
    double x3 = x * x2;
    double x4 = x2 * x2;
    double eb = 1.0 / Math.sqrt(1.0 + x);
    double el = 0.0;
    if (x > 1) {
      double serie = 1.0 - 1.0 / (8.0 * x) - 3.0 / (128.0 * x2) - 15.0 / (1024.0 * x3);
      el = Math.sqrt(2.0 / (x * Constants.PI) * serie);
    } else
      el = 1.0 - x / 2.0 + x2 / 4.0 - 5.0 * x3 / 48.0 + 7.0 * x4 / 192;

    return eb * sintheta2 + el * costheta2;
  }

}


