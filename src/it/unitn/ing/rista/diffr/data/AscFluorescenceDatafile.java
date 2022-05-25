/*
 * @(#)AscFluorescenceDatafile.java created Apr 7, 2017, DII, Povo
 *
 * Copyright (c) 1996-2017 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.util.Misc;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.StringTokenizer;
import java.util.Vector;

/**
 * The AscFluorescenceDatafile is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2017/04/07 14:45:55 $
 * @since JDK1.1
 */

public class AscFluorescenceDatafile extends it.unitn.ing.rista.diffr.DiffrDataFile {

	public AscFluorescenceDatafile(XRDcat aobj, String alabel) {
		super(aobj, alabel);

		// user modification: put here the extension for the file format
		// you want to use. In this case this format is used when the
		// data file has the extension .dat

		identifier = ".asc";
	}

	public AscFluorescenceDatafile() {
		// user modification: put here the extension for the file format
		// you want to use. In this case this format is used when the
		// data file has the extension .dat

		identifier = ".asc";
	}


	public boolean readallSpectra() {

		boolean loadSuccessfull = false;
		boolean tmpB = isAbilitatetoRefresh;
		isAbilitatetoRefresh = false;
		BufferedReader reader = getReader();
		if (reader != null) {
			try {

				energyDispersive = true;

				Vector y = new Vector(100, 100);

				datanumber = 0;

				String token1 = new String("");

				String linedata = reader.readLine();
				linedata = Misc.removeUTF8BOM(linedata);

				while (linedata.startsWith("#"))
					linedata = reader.readLine();
//              System.out.println(linedata);

				while (linedata != null) {

					StringTokenizer st = new StringTokenizer(linedata, "' ,\t\r\n");

					try {
						if (st.hasMoreTokens()) {
							token1 = st.nextToken();

//              System.out.println(token1 + " " + token2);
							Double n2 = Double.valueOf(token1);
							datanumber++;
							y.addElement(n2);
						}
					} catch (Exception ge) {
// not numbers, we don't store them
//              System.out.println("not a number: " + linedata);
					}

					linedata = reader.readLine();
				}

				initData(datanumber);

				for (int i = 0; i < datanumber; i++) {
					setXData(i, i);
					double intensityValue = ((Double) y.elementAt(i)).doubleValue();
//          if (intensityValue < 0.0) // we will not accept it, we suppose is an error
//            intensityValue = 0.0;
					setYData(i, intensityValue);
					double tmpweight = Math.sqrt(Math.abs(intensityValue));
					if (tmpweight != 0.0)
						setWeight(i, 1.0 / tmpweight);
					else
						setWeight(i, 1.0);
				}
				loadSuccessfull = true;

			} catch (Exception e) {
				e.printStackTrace();
				System.out.println("Error in loading the data file! Try to remove this data file");
			}
			try {
				reader.close();
			} catch (IOException e) {
			}
		}
		isAbilitatetoRefresh = tmpB;
		return loadSuccessfull;
	}

}
