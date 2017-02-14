/*
 * @(#)EnixeDatafile.java created 17/09/2014 Povo2
 *
 * Copyright (c) 2014 Luca Lutterotti All Rights Reserved.
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

import java.io.*;
import java.lang.*;
import java.util.*;

/**
 * The xyDataFile is a class to load datafile
 * consisting of only two columns, one with the two-theta
 * and the other with the intensity.
 *
 *
 * @version $Revision: 1.8 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class EnixeDatafile extends it.unitn.ing.rista.diffr.DiffrDataFile {

	public EnixeDatafile(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		identifier = ".enixe";
	}

	public EnixeDatafile() {
		identifier = ".enixe";
	}

	public boolean readallSpectra() {

		boolean loadSuccessfull = false;
		boolean tmpB = isAbilitatetoRefresh;
		isAbilitatetoRefresh = false;
		BufferedReader reader = getReader();
		if (reader != null) {
			try {

				dspacingbase = false;
				Vector y = new Vector(100, 100);

				datanumber = 0;

				String token1 = new String("");

				String linedata = reader.readLine();

				while (linedata.startsWith("#")) {
					if (linedata.startsWith("#PSI")) {
						StringTokenizer st = new StringTokenizer(linedata, "=' ,\t\r\n");
						token1 = st.nextToken();
						setString(2, st.nextToken());
					} else if (linedata.startsWith("#CHI")) {
						StringTokenizer st = new StringTokenizer(linedata, "=' ,\t\r\n");
						token1 = st.nextToken();
						setString(2, st.nextToken());
					} else if (linedata.startsWith("#OMEGA")) {
						StringTokenizer st = new StringTokenizer(linedata, "=' ,\t\r\n");
						token1 = st.nextToken();
						setString(1, st.nextToken());
					} else if (linedata.startsWith("#ETA")) {
						StringTokenizer st = new StringTokenizer(linedata, "=' ,\t\r\n");
						token1 = st.nextToken();
						setString(4, st.nextToken());
					} else if (linedata.startsWith("#PHI")) {
						StringTokenizer st = new StringTokenizer(linedata, "=' ,\t\r\n");
						token1 = st.nextToken();
						setString(3, st.nextToken());
					}
					linedata = reader.readLine();
				}
//              System.out.println(linedata);

				while (linedata != null) {

					StringTokenizer st = new StringTokenizer(linedata, "' ,\t\r\n");

					try {
						if (st.hasMoreTokens()) {
							token1 = st.nextToken();
							Double n1 = Double.valueOf(token1);
							datanumber++;
							y.addElement(n1);
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
