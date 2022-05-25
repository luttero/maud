/*
* @(#)ForceField.java created 18/10/2002 Mesiano
*
* Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.diffr.structure.StructureAtomic;

import javax.swing.*;
import java.awt.*;


/**
*  The ForceField class represent a force field and the associated potential energy.
*
* @version $Revision: 1.7 $, $Date: 2005/03/14 13:38:08 $
* @author Luca Lutterotti
* @since JDK1.1
*/


public class ForceField extends XRDcat {

  public ForceField(XRDcat aobj, String alabel) {
		super(aobj, alabel);
	}

	public ForceField(XRDcat aobj) {
		this(aobj, "Force Field x");
	}

	public StructureAtomic getParentStructure() {
		return (StructureAtomic) getParent();
	}

	public ForceField() {
	}

	public double computeEnergy() {
		return 0.0;
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
		JOptionsDialog adialog = new JForceFieldOptionsD(parent, this);
		return adialog;
	}

	public class JForceFieldOptionsD extends JOptionsDialog {

		public JForceFieldOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new FlowLayout());


			principalPanel.add(new JLabel("No options for this model"));

			setTitle("Force Field");
			initParameters();
			pack();
		}

		public void initParameters() {
		}

		public void retrieveParameters() {
		}
	}

}


