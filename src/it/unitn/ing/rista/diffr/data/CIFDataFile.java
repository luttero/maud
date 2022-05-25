/*
 * @(#)CIFDataFile.java created 02/01/2000 Casalino
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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
import it.unitn.ing.rista.util.Misc;

import java.io.*;
import java.lang.*;
import java.util.*;

/**
 * The CIFDataFile is an object to load CIF datafiles.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.10 $, $Date: 2006/01/19 14:45:55 $
 * @since JDK1.1
 */

public class CIFDataFile extends it.unitn.ing.rista.diffr.DiffrDataFile {

	public CIFDataFile(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		identifier = ".cif";
	}

	public CIFDataFile() {
		identifier = ".cif";
	}

	public boolean readallSpectra() {

		boolean loadSuccessfull = false;
		boolean tmpB = isAbilitatetoRefresh;
		isAbilitatetoRefresh = false;
		boolean iscalibrated = true;
		BufferedReader reader = getReader();
		startingvalue = 0;
		measurementstep = 1;
		if (reader != null) {
			try {

				dspacingbase = false;
				datanumber = 0;
//        constantstep = false;
				String token = new String("");
				String linedata = null;
				StringTokenizer st = null;

				while (token != null && !token.toLowerCase().startsWith("loop_")) {
					linedata = Misc.toStringStripLeadingTrailingBlankAndTab(Misc.removeUTF8BOM(reader.readLine()));

//          System.out.println(linedata);
					st = new StringTokenizer(linedata, "' ,\t\r\n");

					while (st.hasMoreTokens()) {
						token = st.nextToken();
						if (token.equalsIgnoreCase("_pd_meas_number_of_points")) {
							token = st.nextToken();
							datanumber = Integer.valueOf(token).intValue();
							initData(datanumber);
						} else if (token.equalsIgnoreCase(pd_meas_scan_range_inc)) {
							token = st.nextToken();
							measurementstep = Double.valueOf(token).doubleValue();
						} else if (token.equalsIgnoreCase(pd_meas_scan_range_min)) {
							token = st.nextToken();
							startingvalue = Double.valueOf(token).doubleValue();
						} else if (token.equalsIgnoreCase("_riet_meas_datafile_calibrated")) {
							token = st.nextToken();
							if (token.equalsIgnoreCase("false"))
								iscalibrated = false;
							else
								iscalibrated = true;
//              initData(datanumber);
						} else if (token.equalsIgnoreCase("_pd_meas_detector_id")) {
							token = st.nextToken();
							if (token.startsWith("Amptek")) {
								energyDispersive = true;
								iscalibrated = false;
//								System.out.println("Energy ");
							}
						} else if (token.equalsIgnoreCase("_pd_meas_scan_method")) {
							token = st.nextToken();
							if (token.toLowerCase().startsWith("disp")) {
								energyDispersive = true;
								iscalibrated = false;
//								System.out.println("Energy ");
							}
						} else {
							if (token.startsWith("_") && st.hasMoreTokens()) {
								setField(token, st.nextToken(), "0", "0", "0", false, null, null, null, null, null, false, false);
//								System.out.println("Setting " + token);
							}
						}
					}
				}

				int i = 0;
				int indexXcoord = -1;
				int indexIntensity = -1;

				if (token != null) {
					linedata = Misc.toStringStripLeadingTrailingBlankAndTab(reader.readLine());
					while (linedata.startsWith("_")) {
						st = new StringTokenizer(linedata, "' ,\t\r\n");
						while (st.hasMoreTokens()) {
							token = st.nextToken();
							if (token.startsWith("_")) {
								if (token.equalsIgnoreCase(CIFXcoord2T)) {
									indexXcoord = i;
								} else if (token.equalsIgnoreCase(CIFXcoordD)) {
									indexXcoord = i;
									dspacingbase = true;
								} else if (token.equalsIgnoreCase(intensityExpCIFstring) ||
										token.equalsIgnoreCase(intensityCalcCIFstring) || token.equalsIgnoreCase("_pd_meas_counts_total")) {
									indexIntensity = i;
								}
								i++;
							}
						}
						linedata = Misc.toStringStripLeadingTrailingBlankAndTab(reader.readLine());
					}

					int maxindex = i;
					i = 0;
					int index = 0;

/*					if (datanumber > 0) {
						while (linedata != null && i < datanumber) {
							st = new StringTokenizer(linedata, "' ,\t\r\n");
							while (st.hasMoreTokens()) {
								token = st.nextToken();
								if (index == indexXcoord) {
//                if (iscalibrated)
//                  setCalibratedXData(i, Double.valueOf(token).doubleValue());
//                else
									setXData(i, Double.valueOf(token).doubleValue());
								} else if (index == indexIntensity) {
									double intensityValue = Double.valueOf(token).doubleValue();
									setYData(i, intensityValue);
									if (intensityValue < 0.0) // we will not accept it, we suppose is an error
										intensityValue = 0.0;
									double tmpweight = Math.sqrt(intensityValue);
									if (tmpweight != 0.0)
										setWeight(i, 1.0 / tmpweight);
									else
										setWeight(i, 1.0);
								}

								index++;
								if (index == maxindex) {
									index = 0;
									i++;
								}
							}
							linedata = reader.readLine();
						}
					} else {*/
						Vector dataX = new Vector(500, 500);
						Vector dataY = new Vector(500, 500);
						linedata = Misc.toStringStripLeadingTrailingBlankAndTab(linedata);
					if (linedata != null && linedata.startsWith("#"))
						linedata = Misc.toStringStripLeadingTrailingBlankAndTab(reader.readLine());
//						System.out.println(linedata);
						while (linedata != null && !(linedata.startsWith("_") || linedata.startsWith("data"))) {
							st = new StringTokenizer(linedata, "' ,\t\r\n");
							while (st.hasMoreTokens()) {
								token = st.nextToken();
								if (index == indexXcoord) {
									dataX.add(token);
								} else if (index == indexIntensity) {
									dataY.add(token);
//									System.out.println(i + " " + token);
								}
								index++;
								if (index == maxindex) {
									index = 0;
									i++;
								}
							}
							linedata = Misc.toStringStripLeadingTrailingBlankAndTab(reader.readLine());
							if (linedata != null && linedata.startsWith("#"))
								linedata = Misc.toStringStripLeadingTrailingBlankAndTab(reader.readLine());
						}
						datanumber = dataY.size();
						initData(datanumber);
//						System.out.println(datanumber + " " + dataX.size() + " " + startingvalue + " " + measurementstep);
						if (dataX.size() == 0) {
							for (int i1 = 0; i1 < datanumber; i1++)
								dataX.add(Double.toString(startingvalue + i1 * measurementstep));
						}
						for (int j = 0; j < datanumber; j++) {
							if (iscalibrated)
								setCalibratedXData(j, Double.valueOf((String) dataX.elementAt(j)).doubleValue());
							else
								setXData(j, Double.valueOf((String) dataX.elementAt(j)).doubleValue());
							double intensityValue = Double.valueOf((String) dataY.elementAt(j)).doubleValue();
							setYData(j, intensityValue);
							if (intensityValue < 0.0) // we will not accept it, we suppose is an error
								intensityValue = 0.0;
							double tmpweight = Math.sqrt(intensityValue);
							if (tmpweight != 0.0)
								setWeight(j, 1.0 / tmpweight);
							else
								setWeight(j, 1.0);
//							System.out.println(dataX.elementAt(j) + " " + intensityValue + " " + tmpweight);
						}
						dataX = null;
						dataY = null;
					}
//				}
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
