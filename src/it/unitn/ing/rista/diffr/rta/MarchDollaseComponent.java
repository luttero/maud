/*
 * @(#)MarchDollaseComponent.java created Jul 24, 2009 Caen
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
package it.unitn.ing.rista.diffr.rta;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.ParameterPreferences;
import it.unitn.ing.rista.util.Constants;

/**
 * The MarchDollaseComponent is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jul 24, 2009 3:42:09 PM $
 * @since JDK1.1
 */
public class MarchDollaseComponent extends XRDcat {

    public static String[] diclistc = {"_riet_hkl_orient_plane_h","_riet_hkl_orient_plane_k","_riet_hkl_orient_plane_l",

                                       "_riet_par_pref_or_coef", "_riet_par_pref_or_wgt"};
    public static String[] diclistcrm = {"_riet_hkl_orient_plane_h","_riet_hkl_orient_plane_k","_riet_hkl_orient_plane_l",

                                       "March-Dollase coefficient", "weight of the M-D component"};

    public static String[] classlistc = {};
    public static String[] classlistcs = {};

    public MarchDollaseComponent(XRDcat aobj, String alabel) {
      super(aobj, alabel);
      initBaseObject();

      identifier = new String("March-Dollase component");
      IDlabel = new String("March-Dollase component");
      description = new String("select this to use a March-Dollase component");
    }

    public MarchDollaseComponent(XRDcat aobj) {
      this(aobj, "March-Dollase component");
    }

    public MarchDollaseComponent() {
      identifier = new String("March-Dollase component");
      IDlabel = new String("March-Dollase component");
      description = new String("select this to use a March-Dollase component");
    }

    public void initConstant() {
      //	to be overrided in subclasses
      Nstring = 0;
      Nstringloop = 0;
      Nparameter = 5;
      Nparameterloop = 0;
      Nsubordinate = 0;
      Nsubordinateloop = 0;
    }

    public void initDictionary() {
      for (int i = 0; i < totsubordinateloop; i++)
        diclist[i] = diclistc[i];
      System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
      for (int i = 0; i < totsubordinate - totparameterloop; i++)
        classlists[i] = classlistcs[i];
      for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
        classlist[i] = classlistc[i];
    }

    public void initParameters() {
      super.initParameters();
      parameterField[0] = new Parameter(this, getParameterString(0), 1,
              ParameterPreferences.getDouble(getParameterString(0) + ".min", -100),
              ParameterPreferences.getDouble(getParameterString(0) + ".max", 100));
      parameterField[1] = new Parameter(this, getParameterString(1), 1,
              ParameterPreferences.getDouble(getParameterString(1) + ".min", -100),
              ParameterPreferences.getDouble(getParameterString(1) + ".max", 100));
      parameterField[2] = new Parameter(this, getParameterString(2), 1,
              ParameterPreferences.getDouble(getParameterString(2) + ".min", -100),
              ParameterPreferences.getDouble(getParameterString(2) + ".max", 100));
      parameterField[3] = new Parameter(this, getParameterString(3), 1,
              ParameterPreferences.getDouble(getParameterString(3) + ".min", 0),
              ParameterPreferences.getDouble(getParameterString(3) + ".max", 1));
      parameterField[3].setPositiveOnly();
      parameterField[4] = new Parameter(this, getParameterString(4), 0.5,
              ParameterPreferences.getDouble(getParameterString(4) + ".min", 0),
              ParameterPreferences.getDouble(getParameterString(4) + ".max", 1));
      parameterField[4].setPositiveOnly();
    }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
            notifyParameterChanged(source, Constants.TEXTURE_CHANGED);
            return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.TEXTURE_CHANGED);
            return;
          }

      super.notifyParameterChanged(source);
    }
  }

    public void initializeReflexes(Sample asample) {}

    public void refreshForNotificationDown(XRDcat source, int reason) {
      if (!getFilePar().isComputingDerivate() || (source == this ||
              (reason == Constants.SAMPLE_ORIENTATION_CHANGED ||
              reason == Constants.CELL_CHANGED)))
        refreshComputation = true;
    }

    public void updateParametertoDoubleBuffering(boolean firstLoading) {
      // to be implemented by subclasses

      if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
        return;
      super.updateParametertoDoubleBuffering(false);

      parameterField[3].setPositiveOnly();
    }

  public double getWeightFactorValue() {
    return Math.abs(getParameterValue(4));
  }

  public double getMarchCoefficient() {
    return Math.abs(parameterValues[3]);
  }

  double dpio = 0.0;
  double hpg[] = new double[3];
  double marchDollaseCoefficient = 1.0;
  double marchDollaseCoefficient2 = 1.0;

    public void computeTextureFactor(Phase aphase, Sample asample) {
      aphase.sghklcompute(false);
      if (refreshComputation) {
        refreshComputation = false;
        for (int i = 0; i < 3; i++)
          hpg[i] = getParameterValue(i);
        aphase.cellVolumeComp();
	      double[] so = aphase.getSoVector();
        dpio = 1.0 / Math.sqrt(so[0] * hpg[0] * hpg[0] + so[1] * hpg[1] * hpg[1]
		        + so[2] * hpg[2] * hpg[2]
		        + 2 * so[5] * hpg[0] * hpg[1]
                + 2 * so[3] * hpg[1] * hpg[2]
                + 2 * so[4] * hpg[0] * hpg[2]);
        marchDollaseCoefficient = getMarchCoefficient();
        marchDollaseCoefficient2 = Math.pow(marchDollaseCoefficient, 2.0);
      }
    }

    public void computeTextureFactor(Sample asample) {
      Phase aphase = (Phase) getParent();
      computeTextureFactor(aphase, asample);
    }

    public double textureFactorComp(Phase aphase, Reflection refl) {
      double textureFactor = 0.0;
      double dspace = dpio * refl.d_space;
	    double[] so = aphase.getSoVector();
      for (int j = 0; j < refl.hlist.length; j++) {
        int h = refl.hlist[j];
        int k = refl.klist[j];
        int l = refl.llist[j];
      double dpc = so[0] * h * hpg[0] + so[1] * k * hpg[1]
		      + so[2] * l * hpg[2] + so[3] * (k * hpg[2] + l * hpg[1]) +
              so[4] * (l * hpg[0] + h * hpg[2]) +
              so[5] * (h * hpg[1] + k * hpg[0]);
      double cosangpl = dspace * dpc;
        cosangpl *= cosangpl;
      double senangpl = 1.0 - cosangpl;
      textureFactor +=  Math.pow(marchDollaseCoefficient2 * cosangpl + senangpl /
              marchDollaseCoefficient, -1.5);
    }
      return textureFactor / refl.hlist.length;
    }

  }

