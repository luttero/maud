/*
 * @(#)DataMask.java created 5/11/2021 DII, Povo
 *
 * Copyright (c) 2021 Luca Lutterotti All Rights Reserved.
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

import java.lang.*;

import ij.ImagePlus;
import it.unitn.ing.rista.awt.*;
import java.awt.*;
import javax.swing.*;


/**
 *  The DataMask is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2021/11/5 10:43:54 $
 * @author Luca Lutterotti
 * @since JDK1.8
 */


public class DataMask extends it.unitn.ing.rista.diffr.XRDcat {

	public DataMask(XRDcat aobj, String alabel) {
		super(aobj, alabel);
	}

	public DataMask(XRDcat aobj) {
		this(aobj, "Image mask baseclass");
	}

	public DataMask() {
	}

	public void filterData(int[][] buffer) {
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
		JOptionsDialog adialog = new JMaskOptionsD(parent, this);
		return adialog;
	}

	public class JMaskOptionsD extends JOptionsDialog {

		public JMaskOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.setLayout(new FlowLayout());
			principalPanel.add(new JLabel("No options for this model"));

			setTitle("Options panel");
			initParameters();
			pack();
		}

		public void initParameters() {
		}

		public void retrieveParameters() {
		}
	}

}
