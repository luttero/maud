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

import java.io.BufferedReader;
import java.io.IOException;
import java.lang.*;
import java.util.Hashtable;
import java.util.StringTokenizer;

import it.unitn.ing.rista.io.StringNumber;
import it.unitn.ing.rista.util.*;
import ij.*;
import ij.io.*;
import ij.gui.*;
import ij.process.*;


/**
 *  The LoskoDataFile is a class to load the LumaCam
 *  detector by Adrian Losko in his text format (prepared
 *  by Tim)
 *
 *
 * @version $Revision: 1.0 $, $Date: 2023/06/7 17:07:29 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class LoskoDataFile extends it.unitn.ing.rista.diffr.MultDiffrDataFile {

	public LoskoDataFile(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		identifier = ".luma";
	}

	public LoskoDataFile() {
		identifier = ".luma";
	}


	int groupPixelsX = 8;
	int groupPixelsY = groupPixelsX;

	int originalPixelsNumberX = 256;
	int originalPixelsNumberY = originalPixelsNumberX;
	int pixelsNumberX = originalPixelsNumberX / groupPixelsX;
	int pixelsNumberY = originalPixelsNumberY / groupPixelsY;
	double sensorSizeX = 115.0;
	double sensorSizeY = sensorSizeX;
	double pixelSizeX = sensorSizeX / pixelsNumberX;
	double pixelSizeY = sensorSizeY / pixelsNumberY;

	String loadedBankID = "";

	int tofNumber = 2048;
	double clockWidth = 100.0;
	double minTOF = 0.0;
	double maxTOF = 7.5;
	int[][][] counts = null;
	double[][][] x_m = null;
	double[][][] y_m = null;
	double[][][] tof_m = null;

	double coeffA = 0;
	double coeffB = 0;
	double[] tmap = null;

	protected void addToCount(double x, double y, double tof) {
		int index = (int) Math.exp((tof - coeffA) / coeffB);
		if (index >= 0 && index < tofNumber) {
			int ix = (int) (x / groupPixelsX);
			int iy = (int) (y / groupPixelsY);
			if (ix >= 0 && ix < pixelsNumberX && iy >= 0 && iy < pixelsNumberY) {
				counts[ix][iy][index]++;
				x_m[ix][iy][index] += x;
				y_m[ix][iy][index] += y;
				tof_m[ix][iy][index] += tof;
//				MultiTOFPanelCalibration angcal = (MultiTOFPanelCalibration) getDataFileSet().getInstrument().getAngularCalibration();
//				int bankNumber = angcal.getBankNumber(loadedBankID);
//				double tofCorrection = angcal.getBank(bankNumber).getTOFcorrectionForBinning(x, y, tof);
			}
		}
	}
	protected void normalize() {
		for (int ix = 0; ix < pixelsNumberX;  ix++)
			for (int iy = 0; iy < pixelsNumberY;  iy++)
				for (int it = 0; it < tofNumber;  it++) {
					if (counts[ix][iy][it] > 0) {
						x_m[ix][iy][it] /= counts[ix][iy][it];
						y_m[ix][iy][it] /= counts[ix][iy][it];
						tof_m[ix][iy][it] /= counts[ix][iy][it];
					} else {
						x_m[ix][iy][it] = 0.5 + ix;
						y_m[ix][iy][it] = 0.5 + iy;
						tof_m[ix][iy][it] = tmap[it];
					}
				}

	}
	public boolean readallSpectra() {

		originalPixelsNumberX = MaudPreferences.getInteger("LumaCam.dataPixelsNumberX", originalPixelsNumberX);
		originalPixelsNumberY = MaudPreferences.getInteger("LumaCam.dataPixelsNumberY", originalPixelsNumberX);;

		groupPixelsX = MaudPreferences.getInteger("LumaCam.groupPixelsBy", groupPixelsX);
		groupPixelsY = groupPixelsX;

		sensorSizeX = MaudPreferences.getDouble("LumaCam.sensorSize_mm", sensorSizeX);;
		sensorSizeY = sensorSizeX;
		pixelsNumberX = originalPixelsNumberX / groupPixelsX;
		pixelsNumberY = originalPixelsNumberY / groupPixelsY;
		pixelSizeX = sensorSizeX / pixelsNumberX;
		pixelSizeY = sensorSizeY / pixelsNumberY;

		tofNumber = MaudPreferences.getInteger("LumaCam.numberTOFpoints", 2048);

		double clockWidth = 100.0;
		double minTOF = MaudPreferences.getDouble("LumaCam.minTOF", 0);
		double maxTOF = MaudPreferences.getDouble("LumaCam.maxTOF", 7.5);

		boolean loadSuccessfull = false;
		boolean tmpB = isAbilitatetoRefresh;
		isAbilitatetoRefresh = false;

		BufferedReader reader = getReader();
		if (reader != null) {
			try {
				int index = 1;
				clockWidth = 100.0;
				int bankNumber = 0;

				String token = new String("");
				StringTokenizer st = null;
				String linedata = reader.readLine();
				boolean endoffile = false;

				while (!linedata.startsWith("X\tY\tToF"))
					linedata = reader.readLine();

				double omega = 0.0, chi = 0.0, phi = 0.0, eta = 0.0;
				double scale_factor = 1.0;

				counts = new int[pixelsNumberX][pixelsNumberY][tofNumber];
				x_m = new double[pixelsNumberX][pixelsNumberY][tofNumber];
				y_m = new double[pixelsNumberX][pixelsNumberY][tofNumber];
				tof_m = new double[pixelsNumberX][pixelsNumberY][tofNumber];

				tmap = new double[tofNumber];

				// t = a + b log(i)
				// b = (tmax - tmin) / (log(imax) - log(imin))
				coeffB = (maxTOF - minTOF) / (Math.log(tofNumber));
				// a = tmax - b log(imax) = tmin
				coeffA = minTOF;

				for (int i = 0; i < tofNumber; i++)
					tmap[i] = coeffA + coeffB * Math.log(0.5 + i);

				linedata = reader.readLine();
				st = new StringTokenizer(linedata, " ,\t\r\n");
				while (linedata != null) {
					if (st.hasMoreTokens()) {
						double x = Double.parseDouble(st.nextToken());
						double y = Double.parseDouble(st.nextToken());
						double tof = Double.parseDouble(st.nextToken()) * 1000.0 / clockWidth;
						addToCount(x, y, tof);
					}

					linedata = reader.readLine();
					if (linedata != null) {
						st = new StringTokenizer(linedata, " ,\t\r\n");
						if (!st.hasMoreTokens())
							linedata = null;
					}
				}
				normalize();
				for (int ix = 0; ix < pixelsNumberX; ix++) {
					for (int iy = 0; iy < pixelsNumberY; iy++) {
						DiffrDataFile datafile = null;
						boolean atmpB = false;
						if (bankNumber >= 0) {
							datafile = addDiffrDatafile(Integer.toString(index++));
							atmpB = datafile.isAbilitatetoRefresh;
							datafile.isAbilitatetoRefresh = false;

							datafile.setDataType(DIFFRACTION_IMAGE);
							datafile.dspacingbase = true;
							datafile.constantstep = false;
							datafile.initData(tofNumber);

							String titleString = "LumaCam number " + Integer.toString(index - 1);
							datafile.title = new String(titleString);

							datafile.setAngleValue(DiffrDataFile.DATAFILE_OMEGA, omega);
							datafile.setAngleValue(DiffrDataFile.DATAFILE_CHI, chi);
							datafile.setAngleValue(DiffrDataFile.DATAFILE_PHI, phi);
							datafile.setAngleValue(DiffrDataFile.DATAFILE_ETA, eta);
							for (int i1 = 0; i1 < tofNumber; i1++) {
								datafile.setXData(i1, tof_m[ix][iy][i1]);
								datafile.setYData(i1, counts[ix][iy][i1]);
								datafile.setXImage(i1, x_m[ix][iy][i1] * pixelSizeX);
								datafile.setYImage(i1, y_m[ix][iy][i1] * pixelSizeY);
							}
							String bankID = new String(GSASbankCalibration.bankPrefix + Integer.toString(bankNumber));
							datafile.setBankID(bankID);
						} else index++;
						if (datafile != null) {
							loadSuccessfull = true;
							datafile.isAbilitatetoRefresh = atmpB;
							datafile.dataLoaded = true;
						}
					}
				}

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
