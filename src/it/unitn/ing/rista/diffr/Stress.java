/*
 * @(#)Stress.java created 15/11/1999 Pergine Vals.
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;

import java.awt.*;
import java.io.*;
import java.lang.*;
import java.util.*;
import javax.swing.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.io.*;


/**
 *  The Stress is a general class to compute strain maps from an applied stress tensor
 * of I kind (macro) and II kind (micro). This one does nothing, particular theories
 * must be implemented in subclasses of this class.
 *
 *
 * @version $Revision: 1.5 $, $Date: 2005/05/06 18:07:25 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class Stress extends XRDcat {

  public boolean refreshStress = true;

  public Stress(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public Stress(XRDcat aobj) {
    this(aobj, "Stress model x");
  }

  public Stress() {
  }

/*	public void initConstant() {
		Nstring = 0;
		Nstringloop = 0;
		Nparameter = 0;
		Nparameterloop = 0;
		Nsubordinate = 0;
		Nsubordinateloop = 0;
	}

	public void initDictionary() {
		for (int i = 0; i < totsubordinateloop; i++)
		  diclist[i] = diclistc[i];
    for (int i = 0; i < totsubordinateloop; i++)
      diclistRealMeaning[i] = diclistcrm[i];
		for (int i = 0; i < totsubordinate - totparameterloop; i++)
		  classlists[i] = classlistcs[i];
		for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
		  classlist[i] = classlistc[i];
	}

	public void initParameters()
	{
		super.initParameters();
	}*/

  public void computeStress(Sample asample) {
  }

  public void computeStress(Phase aphase, Sample asample) {
/*  	FilePar aparFile = (FilePar) getFilePar();
 		if (!aparFile.isStressComputationPermitted())
 			return;
		int hkln = aphase.gethklNumber();
		for (int j = 0; j < hkln; j++) {
			Reflection refl = (Reflection) aphase.reflectionv.elementAt(j);
			refl.expToStress();
		}*/
    refreshStress = false;
  }

  public double computeStress(Phase aphase, double stress_angles[],
                              int h, int k, int l) {
    return 0.0;
  }

  public double[] computeStress(Phase aphase, double alpha[], double beta[],
                                Reflection reflex) {

    int numberOfPoints = alpha.length;
    double[] random = new double[numberOfPoints];
    for (int i = 0; i < numberOfPoints; i++)
      random[i] = 0.0;
    return random;
  }

  public double[][] getPoleFigureGrid(Reflection refl, int numberofPoints, double maxAngle) {

    return null;
  }

  public double[][] getExpPoleFigureGrid(Reflection reflex, int numberofPoints, double maxAngle) {

    return null;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JStressOptionsD(parent, this);
    return adialog;
  }

  public class JStressOptionsD extends JOptionsDialog {

    public JStressOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      principalPanel.add(new JLabel("No options for this model"));

      setTitle("Stress options panel");
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }

  }
}
