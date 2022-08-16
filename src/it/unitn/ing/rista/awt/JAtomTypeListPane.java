/*
 * @(#)JAtomTypeListPane.java created 08/06/16 Casalino
 *
 * Copyright (c) 1996-2016 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of the author and it is 
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

package it.unitn.ing.rista.awt;

import com.github.tschoonj.xraylib.Xraylib;
import it.unitn.ing.rista.chemistry.XRayDataSqLite;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.MoreMath;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * The JAtomTypeListPane is a class ....
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.0 $, $Date: 08/06/16 10:55 $
 * @since JDK1.1
 */

public class JAtomTypeListPane extends JSubordListPane {

	JButton atomtypechoice = null;

	public JAtomTypeListPane(Frame parent, boolean showTotal) {
		super(parent, showTotal);
	}

	public void addCustomControlsToFieldsPanel() {
		JPanel jPanel18;

		fieldsPanel.add(jPanel18 = new JPanel(new FlowLayout()), BorderLayout.NORTH);

		jPanel18.add(new JLabel("Atom type:"));
		jPanel18.add(atomtypechoice = new JIconButton("PeriodicTable.gif"));
		atomtypechoice.setToolTipText("Press to select the atom type");
		atomtypechoice.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent event) {
				chooseTheAtom();
			}
		});
		JButton infoButton = new JButton("Info");
		infoButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent event) {
				showInfoPanel();
			}
		});

		jPanel18.add(infoButton);


	}

	public void setparameterlist() {
		if (itsparent != null) {
			AtomScatterer scatterer = (AtomScatterer) itsparent.subordinateloopField[theindex].selectedElement();
			if (scatterer != null && scatterer != selectedObject) {
				selectedObject = scatterer;
				for (int i = 0; i < fieldNumber; i++) {
					Parameter apar = scatterer.parameterField[i];
					if (apar != null) {
						((myJFrame) getFrameParent()).removeComponentfromlist(valueTF[i]);
//						System.out.println("Adding jatom: " + i + " " + valueTF[i].getText());
						((myJFrame) getFrameParent()).addComponenttolist(valueTF[i], apar);
						valueTF[i].setText(apar.getValue());
					}/* else {
//						System.out.println("Removing jatom: " + i + " " + valueTF[i].getText());
						((myJFrame) getFrameParent()).removeComponentfromlist(valueTF[i]);
					}*/
				}
				atomtypechoice.setText(scatterer.getAtomSymbol());
			}
		}
	}

	void chooseTheAtom() {
		AtomScatterer scatterer = null;
		if (itsparent != null) {
			scatterer = (AtomScatterer) itsparent.subordinateloopField[theindex].selectedElement();
			if (scatterer != null)
				ChooseAtomD.getAtomType(getFrameParent(), scatterer);
			atomtypechoice.setText(scatterer.getAtomSymbol());
		}
	}

	public void showInfoPanel() {
		AtomScatterer scatterer = (AtomScatterer) itsparent.subordinateloopField[theindex].selectedElement();
		if (scatterer == null)
			return;
		int atomicNumber = scatterer.getAtomicNumber();

		int plotCounts = Constants.energiesMaxNumber;
		double[] x = new double[plotCounts];
		double[] y = new double[plotCounts];
		double xstart = Constants.BASE_ENERGY_IN_KEV;
		double xstep = Constants.INV_MULTIPLE_ENERGY_TO_INT;
		for (int i = 0; i < plotCounts; i++) {
			x[i] = xstart + i * xstep;
			y[i] = scatterer.getSiteAbsorption(x[i]) / scatterer.getSiteWeight(); // XRayDataSqLite.getTotalAbsorptionForAtomAndEnergy(atomicNumber, x[i]);
//			x[i] = MoreMath.log10(x[i]);
			if (y[i] > 0)
				y[i] = MoreMath.log10(y[i]);
			else
				y[i] = 0;
		}
		(new PlotSimpleData(getFrameParent(), x, y)).setVisible(true);

		for (int i = 0; i < plotCounts; i++) {
			x[i] = xstart + i * xstep;
			if (Constants.useXrayLib)
				y[i] = Xraylib.CS_Rayl(atomicNumber, x[i]);
			else
				y[i] = XRayDataSqLite.getCoherentScatteringForAtomAndEnergy(atomicNumber, x[i]);
			if (y[i] > 0)
				y[i] = MoreMath.log10(y[i]);
			else
				y[i] = 0;
		}
		(new PlotSimpleData(getFrameParent(), x, y)).setVisible(true);

		for (int i = 0; i < plotCounts; i++) {
			x[i] = xstart + i * xstep;
			if (Constants.useXrayLib)
				y[i] = Xraylib.CS_Compt(atomicNumber, x[i]);
			else
				y[i] = XRayDataSqLite.getIncoherentScatteringForAtomAndEnergy(atomicNumber, x[i]);
			if (y[i] > 0)
				y[i] = MoreMath.log10(y[i]);
			else
				y[i] = 0;
		}
		(new PlotSimpleData(getFrameParent(), x, y)).setVisible(true);

		for (int i = 0; i < plotCounts; i++) {
			x[i] = xstart + i * xstep;
			if (Constants.useXrayLib)
				y[i] = Xraylib.CS_Photo_Total(atomicNumber, x[i]);
			else
				y[i] = XRayDataSqLite.getPhotoAbsorptionForAtomAndEnergy(atomicNumber, x[i]);
			if (y[i] > 0)
				y[i] = MoreMath.log10(y[i]);
			else
				y[i] = 0;
		}
		(new PlotSimpleData(getFrameParent(), x, y)).setVisible(true);

/*		for (int i = 0; i < plotCounts; i++) {
			x[i] = xstart + i * xstep;
			y[i] = XRayDataSqLite.getPhotoAbsorptionForAtomAndEnergyDiv(atomicNumber, x[i]);
			if (y[i] > 0)
				y[i] = MoreMath.log10(y[i]);
			else
				y[i] = 0;
		}
		(new PlotSimpleData(getFrameParent(), x, y)).setVisible(true);*/


	}


}
