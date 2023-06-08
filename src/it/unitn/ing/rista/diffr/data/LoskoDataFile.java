/*
 * @(#)LoskoDataFile.java created 6/06/2023 Los Alamos
 *
 * Copyright (c) 2023 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.data;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.cal.*;

import java.lang.*;

import it.unitn.ing.rista.io.StringNumber;
import it.unitn.ing.rista.util.*;
import ij.*;
import ij.io.*;
import ij.gui.*;
import ij.process.*;


/**
 *  The LoskoDataFile is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2023/06/7 17:07:29 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class LoskoDataFile extends it.unitn.ing.rista.diffr.MultDiffrDataFile {

	public LoskoDataFile(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		identifier = ".losko";
	}

	public LoskoDataFile() {
		identifier = ".losko";
	}


	public boolean readallSpectra() {

		boolean loadSuccessfull = false;
		DataFileSet data = getDataFileSet();
		boolean tmpB = isAbilitatetoRefresh;
		isAbilitatetoRefresh = false;
//    new Opener().open();
//		OpenDialog od = new OpenDialog("Open...", "");
		String directory = getFolder(); //od.getDirectory();
		String name = getLabel(); //od.getFileName();
//    System.out.println("Opening file: "+ directory + " - " + name);
		if (name != null) {
			ImagePlus imp = (new Opener()).openImage(directory, name);
			if (imp != null) {
				double[] gonioAngles = StringNumber.checkAngles(name);
				AngularCalibration angcal = getDataFileSet().getInstrument().getAngularCalibration();

				if (angcal != null) {
					if (angcal instanceof TOFPanelCalibration) {
						angcal.loadAndUnrollImage(imp, this, gonioAngles);
						loadSuccessfull = true;
					}
				}
			}
		}
		isAbilitatetoRefresh = tmpB;
//    data.refreshAll(false);

		return loadSuccessfull;
	}

}
