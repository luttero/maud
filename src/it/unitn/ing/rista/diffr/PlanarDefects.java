/*
 * @(#)PlanarDefects.java created 11/01/1999 Pergine Vals
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
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

import java.lang.*;
import java.awt.*;
import java.util.Vector;

import it.unitn.ing.jsginfo.Sghkl;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.Constants;

import javax.swing.*;


/**
 * The PlanarDefects is a class
 * <p/>
 * 
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:05 $
 * @since JDK1.1
 */


public class PlanarDefects extends XRDcat {

  public double[] divideFactor = {1.0, 1.0, 1.0};

  public PlanarDefects(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public PlanarDefects(XRDcat aobj) {
    this(aobj, "Planar defect x");
  }

  public PlanarDefects() {
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
          notifyParameterChanged(source, Constants.SAMPLE_BROADENING, -1);
          notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED, -1);
          return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.SAMPLE_BROADENING, -1);
            notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED, -1);
            return;
          }

      super.notifyParameterChanged(source);
    }
  }

  public void prepareComputation(int cpType) {
  }

  public double getCrystalliteEquivalent(Reflection refl) {
    return 0.0;
  }

  public double getPlanarDefectDisplacement(Reflection refl) {
    // return delta(d)/d
    return 0.0;
  }

  public double getPlanarDefectAsymmetry(Reflection refl) {
    return 0.0;
  }

  public double getPlanarDefectAsymmetryConstant1(Reflection reflex) {
    return 0.0f;
  }

  public double getPlanarDefectAsymmetryConstant2(Reflection refl) {
    return 0.0f;
  }

  public void checkhklListForPlanarDefects(Vector reflectionv, int cpType) {
  }

  public boolean acceptReflection(int h, int k, int l, double dspace) {
    return true;
  }

  public double getStructureFactorModifier(Reflection refl) {
	  if (refl == null)
		  return 1.0;
    return refl.getStructureModifier();
  }

  public int getSuperCellFactor(int index) {
    return 1;  //To change body of created methods use File | Settings | File Templates.
  }

  public double getCrystalliteFactor(int h, int k, int l) {
    return 1;  //To change body of created methods use File | Settings | File Templates.
  }

  public double getMicrostrainFactor(int h, int k, int l) {
    return 1;  //To change body of created methods use File | Settings | File Templates.
  }

  public double[] getDivisionFactors() {
    return divideFactor;
  }

  public void freeAllMicroParameters(int cpType) {
    int i, j;

    for (i = 0; i < Nparameter; i++)
      parameterField[i].setRefinableCheckBound();
    for (i = 0; i < Nparameterloop; i++) {
      for (j = 0; j < numberofelementPL(i); j++)
        ((Parameter) parameterloopField[i].elementAt(j)).setRefinableCheckBound();
    }
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JPlanarDefectsOptionsD(parent, this);
    return adialog;
  }

	public Vector<double[]> computeReflectionsList(Phase phase, Sghkl sghkl,int totNumber, double dmin,
	                                               boolean permitAbsent, double sumOverlapped) {
		return null;
	}

	public boolean checkSghkllist(Phase aphase, Sghkl sghkl, double dplanecut, double dplanestart) {
		return false;
	}

	public boolean checkSghkllist(Phase aphase, double dplanecut) {
		return false;
	}

	public class JPlanarDefectsOptionsD extends JOptionsDialog {

    public JPlanarDefectsOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout());


      principalPanel.add(new JLabel("No options for this model"));

      setTitle("Planar defects");
      initParameters();
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }
  }

}
