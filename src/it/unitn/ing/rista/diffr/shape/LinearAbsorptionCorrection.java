/*
 * @(#)LinearAbsorptionCorrection.java created 1/8/2019 White Rock (NM)
 *
 * Copyright (c) 2019 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.shape;

		import it.unitn.ing.rista.awt.JOptionsDialog;
		import it.unitn.ing.rista.diffr.SampleShape;
		import it.unitn.ing.rista.diffr.XRDcat;
		import it.unitn.ing.rista.diffr.*;
		import it.unitn.ing.rista.util.Constants;

		import javax.swing.*;
		import java.awt.*;

/**
 *  The LinearAbsorptionCorrection is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2019/08/01 19:18:25 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class LinearAbsorptionCorrection extends SampleShape {

	double abs = 0.0;

	protected static String[] diclistc = {
			"_rita_shape_abs_velocity_corr_factor"
	};
	protected static String[] diclistcrm = {
			"_rita_shape_abs_velocity_corr_factor"
	};

	protected static String[] classlistc = {};

	protected static String[] classlistcs = {};


	public LinearAbsorptionCorrection(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initBaseObject();
		identifier = "Linear Absorption";
		IDlabel = "Linear Absorption";
		description = "Linear Absorption";
	}

	public LinearAbsorptionCorrection(XRDcat aobj) {
		this(aobj, "Linear Absorption");
	}

	public LinearAbsorptionCorrection() {
		identifier = "Linear Absorption";
		IDlabel = "Linear Absorption";
		description = "Linear Absorption";
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
		for (int i = 0; i < totsubordinate - totparameterloop; i++)
			classlists[i] = classlistcs[i];
	}

	public void initParameters() {
		super.initParameters();
		parameterField[0].setValue("0.0");
	}

	public void notifyParameterChanged(Parameter source) {
		FilePar filepar = getFilePar();
		if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
			if (parameterField != null)
				for (int i = 0; i < parameterField.length; i++) {
					if (parameterField[i] == source) {
						notifyParameterChanged(source, Constants.SHAPE_ABSORPTION_CHANGED, -1);
						return;
					}
				}
/*			if (parameterloopField != null)
				for (int j = 0; j < parameterloopField.length; j++)
					for (int i = 0; i < parameterloopField[j].size(); i++)
						if (source == parameterloopField[j].elementAt(i)) {
							notifyParameterChanged(source, Constants.SHAPE_ABSORPTION_CHANGED);
							return;
						}
*/
			super.notifyParameterChanged(source);
		}
	}

	public void updateParametertoDoubleBuffering(boolean firstLoading) {
		if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
			return;

		super.updateParametertoDoubleBuffering(false);

		abs = getParameterValue(0);
	}

	public Parameter getAbsortionFactor() {
		return parameterField[0];
	}

	public void setAbsortionFactor(String astring) {
		parameterField[0].setValue(astring);
	}

	public void computeAbsorptionPath(double[][] incidentAndDiffraction_angles, double absorption, double[] position,
	                                  double[] intensity, double toLambda) {
		double arg1 = abs * toLambda;
		if (arg1 < 200.0)
			arg1 = Math.exp(-arg1);
		else
			arg1 = 0.0f;
		for (int i = 0; i < position.length; i++)
			intensity[i] *= arg1;
	}

	public void computeAbsorptionPath(double[][][] incidentAndDiffraction_angles, double[] absorption, double[][] position,
	                                  double[][] intensity, double toLambda) {
		double arg1 = abs * toLambda;
		if (arg1 < 200.0) {
			arg1 = Math.exp(-arg1);
			arg1 = 0.0f;
		}
		for (int i = 0; i < position.length; i++)
			for (int k = 0; k < absorption.length; k++)
				intensity[i][k] *= arg1;
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
		JOptionsDialog adialog = new LinearAbsorptionCorrection.JLinearAbsorptionOptionsD(parent, this);
		return adialog;
	}

	public class JLinearAbsorptionOptionsD extends JOptionsDialog {

		JTextField scalePlot;

		public JLinearAbsorptionOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 6, 6));

			principalPanel.add(BorderLayout.NORTH, add(createEditParField(new FlowLayout(),
					"Absorption factor: ", getAbsortionFactor())));

			initParameters();

			setTitle("Linear absorption correction");

			setHelpFilename("LinearAbsortionFactor.txt");
			pack();

		}

	}
}
