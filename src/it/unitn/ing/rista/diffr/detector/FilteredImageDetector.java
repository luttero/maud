/*
 * @(#)FilteredImageDetector.java created 30/01/2015 Casalino
 *
 * Copyright (c) 2015 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.detector;

import java.awt.*;
import java.lang.*;

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.awt.JSubordinateLoopListPane;
import it.unitn.ing.rista.diffr.*;

/**
 *  The FilteredImageDetector is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2015/01/30 09:32:50 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class FilteredImageDetector extends Detector {

	public static String[] diclistc = {
			"_instrument_filter_material"
	};
	public static String[] diclistcrm = {
			"_instrument_filter_material"
	};

	public static String[] classlistc = {
			"superclass:it.unitn.ing.rista.diffr.AbsorptionWindow"};

	public static String[] classlistcs = {};

	private static int filter_material_id = 0;

	public FilteredImageDetector(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initBaseObject();
		identifier = "Filtered Image Detector";
		IDlabel = "Filtered Image Detector";
		description = "Filtered Image Detector";
	}

	public FilteredImageDetector(XRDcat aobj) {
		this(aobj, "Filtered Image Detector");
	}

	public FilteredImageDetector() {
		identifier = "Filtered Image Detector";
		IDlabel = "Filtered Image Detector";
		description = "Filtered Image Detector";
	}

	public void initConstant() {
		Nstring = 0;
		Nstringloop = 0;
		Nparameter = 0;
		Nparameterloop = 0;
		Nsubordinate = 0;
		Nsubordinateloop = 1;
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
	}

	public double getAbsorptionCorrection(DiffrDataFile adatafile, int pointIndex, double energyInKeV) {
		double incidentAngleInRad = adatafile.getIncidentDetectorAngle(pointIndex);
		double integral = computeAbsorptionForLineWithEnergy(energyInKeV, incidentAngleInRad);
//		System.out.println(adatafile.getEtaValue() + " " + pointIndex + " " +
//				energyInKeV + " " + incidentAngleInRad * Constants.PITODEG + " " + integral);
		return integral;
	}

	public double computeAbsorptionForLineWithEnergy(double energyInKeV, double incidentAngleInRad) {
		double integral = 1.0;
		for (int i = 0; i < getList(filter_material_id).size(); i++) {
			AbsorptionWindow absorptionWindow = (AbsorptionWindow) getList(filter_material_id).elementAt(i);
			integral *= absorptionWindow.computeAbsorptionForLineWithEnergy(energyInKeV, incidentAngleInRad);
		}
		return integral;
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
		JOptionsDialog adialog = new JXRFDetectorOptionsD(parent, this);
		return adialog;
	}

	public class JXRFDetectorOptionsD extends JOptionsDialog {

		JSubordinateLoopListPane absorptionMaterialPanel;

		public JXRFDetectorOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new BorderLayout(3, 3));
//			jp1 = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
			absorptionMaterialPanel = new JSubordinateLoopListPane(this, "List of windows and filters");
			principalPanel.add(BorderLayout.CENTER, absorptionMaterialPanel);

			setTitle("Filtered Image detector data");
			initParameters();
			pack();
		}

		public void initParameters() {
			absorptionMaterialPanel.setList(FilteredImageDetector.this, filter_material_id);
		}

		/**
		 * This method is automatically called when the user press the close button on the dialog.
		 */
		public void retrieveParameters() {
		}
	}


}
