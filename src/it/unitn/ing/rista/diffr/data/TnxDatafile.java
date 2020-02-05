/*
 * @(#)TnxDatafile.java created 21/10/2015 Povo (FBK)
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

package it.unitn.ing.rista.diffr.data;

import it.unitn.ing.rista.diffr.DiffrDataFile;
import it.unitn.ing.rista.diffr.MultDiffrDataFile;
import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.diffr.instrument.XRFInstrument;
import it.unitn.ing.rista.util.Misc;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.StringTokenizer;


/**
 *  The TnxDatafile is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2015/10/21 11:26:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class TnxDatafile extends MultDiffrDataFile {

	public static int maxNumberOfChannels = 8192 * 4;


	public TnxDatafile(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		identifier = ".tnx";
	}

	public TnxDatafile() {
		identifier = ".tnx";
	}


	public boolean readallSpectra() {

		boolean loadSuccessfull = false;
		boolean tmpB = isAbilitatetoRefresh;
		isAbilitatetoRefresh = false;
		boolean calibrated = true;
		BufferedReader reader = getReader();
		int spectrumNumber = -1;
		double omega_angle = 0.0;
		double phi_angle = 0.0;
		double chi_angle = 0.0;
		double eta_angle = 0.0;
		int indexXcoord = -1, indexYcoord = -1, indexIntensity = 0;
		boolean stepscan = false;
		double twothetaShift = 0.0;
		double twothetaStart = 0.0;
		double twothetaStep = 0.01;
		startingvalue = 0;
		measurementstep = 1;
		DiffrDataFile datafile = null;

		if (reader != null) {
			try {

				String token = new String("");
				String token1 = new String("");
				StringTokenizer st = null;
				String linedata = reader.readLine();
				linedata = Misc.removeUTF8BOM(linedata);
				boolean endoffile = false;
				String numberString = null;
				boolean atmpB = true;
				while (!endoffile) {
					if (linedata == null) {
						endoffile = true;
						break;
					} else {
						while (linedata.startsWith("#")) {
							if (linedata.startsWith("#PSI")) {
								st = new StringTokenizer(linedata, "=' ,\t\r\n");
								token1 = st.nextToken();
								chi_angle = Double.parseDouble(st.nextToken());
							} else if (linedata.startsWith("#CHI")) {
								st = new StringTokenizer(linedata, "=' ,\t\r\n");
								token1 = st.nextToken();
								chi_angle = Double.parseDouble(st.nextToken());
							} else if (linedata.startsWith("#OMEGA")) {
								st = new StringTokenizer(linedata, "=' ,\t\r\n");
								token1 = st.nextToken();
								omega_angle = Double.parseDouble(st.nextToken());
							} else if (linedata.startsWith("#ETA")) {
								st = new StringTokenizer(linedata, "=' ,\t\r\n");
								token1 = st.nextToken();
								eta_angle = Double.parseDouble(st.nextToken());
							} else if (linedata.startsWith("#PHI")) {
								st = new StringTokenizer(linedata, "=' ,\t\r\n");
								token1 = st.nextToken();
								phi_angle = Double.parseDouble(st.nextToken());
							}
							linedata = reader.readLine();
						}
						boolean readingData = true;
						double[] xrdDataX = new double[maxNumberOfChannels];
						double[] xrdDataY = new double[maxNumberOfChannels];
						int xrdDataNumber = 0;
						while (readingData && linedata != null) {
							st = new StringTokenizer(linedata, " |;,\t\r\n");
							token = " ";
							if (st.hasMoreTokens()) {
								token = st.nextToken();
								xrdDataX[xrdDataNumber] = Double.parseDouble(token);
								omega_angle = xrdDataX[xrdDataNumber]/2;
							}
							if (st.hasMoreTokens()) {
								token = st.nextToken();
								xrdDataY[xrdDataNumber] = Double.parseDouble(token);
							}
							int xrfDatanumber = 0;
							double[] xrfData = new double[maxNumberOfChannels];
							while (st.hasMoreTokens()) {
								token = st.nextToken();
								if (!token.startsWith("status"))
									xrfData[xrfDatanumber++] = Double.parseDouble(token);
								else
									break;
							}
							numberString = Integer.toString(xrdDataNumber);
							if (getDataFileSet().getInstrument() instanceof XRFInstrument) {
								System.out.println("Adding XRF spectrum number: " + numberString);
								datafile = addDiffrDatafile(numberString);
								atmpB = datafile.isAbilitatetoRefresh;
								datafile.isAbilitatetoRefresh = false;
								datafile.title = title + " number " + numberString;
								datafile.dspacingbase = false;
								datafile.constantstep = false;
								datafile.energyDispersive = true;
								datafile.setField("_riet_meas_datafile_calibrated", "false", "0", "0", "0", false, null, null, null, null, null,
										false, false);
								datafile.setAngleValue(0, omega_angle);
								datafile.setAngleValue(1, chi_angle);
								datafile.setAngleValue(2, phi_angle);
								datafile.setAngleValue(3, eta_angle);

								datafile.initData(xrfDatanumber);
								for (int i = 0; i < xrfDatanumber; i++) {
									datafile.setXData(i, i);
									datafile.setYData(i, xrfData[i]);
								}
								datafile.dataLoaded = true;
								datafile.isAbilitatetoRefresh = atmpB;
							}

							xrdDataNumber++;
							linedata = reader.readLine();
//							loadSuccessfull = true;
						}

						numberString = Integer.toString(xrdDataNumber);
						datafile = addDiffrDatafile(numberString);
						atmpB = datafile.isAbilitatetoRefresh;
						datafile.isAbilitatetoRefresh = false;
						datafile.title = title + " number " + numberString;
						datafile.dspacingbase = false;
						datafile.constantstep = false;
						datafile.energyDispersive = false;
						datafile.setField("_riet_meas_datafile_calibrated", "true", "0", "0", "0", false, null, null, null, null, null,
								false, false);
						datafile.setAngleValue(0, 0);
						datafile.setAngleValue(1, chi_angle);
						datafile.setAngleValue(2, phi_angle);
						datafile.setAngleValue(3, eta_angle);
						datafile.initData(xrdDataNumber);

						for (int i = 0; i < xrdDataNumber; i++) {
							datafile.setXData(i, xrdDataX[i]);
							datafile.setYData(i, xrdDataY[i]);
						}
						loadSuccessfull = true;
						datafile.dataLoaded = true;
						datafile.isAbilitatetoRefresh = atmpB;


					}
					loadSuccessfull = true;
					datafile.dataLoaded = true;
					datafile.isAbilitatetoRefresh = atmpB;
//          System.out.println(datafile.toXRDcatString() + " " + isAbilitatetoRefresh);

//        	linedata = reader.readLine();
					if (linedata == null)
						endoffile = true;
				} // end of data

			} catch (Exception e) {
				e.printStackTrace();
				System.out.println("Error in loading the data file! Try to remove this data file");
			}
			try {
				reader.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		isAbilitatetoRefresh = tmpB;
		return loadSuccessfull;
	}

/*	public void setXData(int index, double value) {
		super.setXData(index, value);
//		originalNotCalibrated = true;
	}

	public void setCalibratedXData(int index, double value) {
		setCalibrated(true);
		twothetacalibrated[index] = (double) (value + twothetaShift - 60.0);
	}*/

	public class SpectrumObject {

		int datanumber = 8192;
		double[] y = new double[datanumber];

		public SpectrumObject(int number) {

		}

	}
}
