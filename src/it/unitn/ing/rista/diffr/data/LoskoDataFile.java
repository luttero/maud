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

	double totalCounts = 0;

	String loadedBankID = "";

	int tofNumber = 2048;
	double clockWidth = 100.0;
	double minTOF = 0.0;
	double maxTOF = 7.5E3;
	double[][][] counts = null;
	double[][][] x_m = null;
	double[][][] y_m = null;
	double[][][] tof_m = null;

	double coeffA = 0;
	double coeffB = 0;
	double[] tmap = null;

	int countsHit = 0;

	protected void addToCount(double x, double y, double phot, double tof, double psd) {
//		int index = (int) Math.exp((1.0 / tof - coeffA) / coeffB);
		int index = (int) (Math.sqrt((tof - coeffA) / coeffB) + 0.5);
/*		countsHit++;
		boolean printThis = (10000 * (countsHit / 10000) == countsHit);
		if (printThis) {
			System.out.println("tof " + tof + " " + phot + " " + index + " " + tmap[index]);
			countsHit = 0;
		}*/
		if (index >= 0 && index < tofNumber) {
			int ix = (int) (x / groupPixelsX + 0.5);
			int iy = (int) (y / groupPixelsY + 0.5);
//			if (printThis)
//				System.out.println("xy " + (ix * groupPixelsX) + " " + (iy * groupPixelsY) + " " + x + " " + y);
			if (ix >= 0 && ix < pixelsNumberX && iy >= 0 && iy < pixelsNumberY) {
				counts[ix][iy][index] = counts[ix][iy][index] + phot;
//				x_m[ix][iy][index] = 0.5 + ix;  //  += x;
//				y_m[ix][iy][index] = 0.5 + iy;  // += y;
//				tof_m[ix][iy][index] = tmap[index]; //+= tof;
				x_m[ix][iy][index] += x * phot;
				y_m[ix][iy][index] += y * phot;
				tof_m[ix][iy][index] += tof * phot;
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
//						System.out.println("Ok: " + tof_m[ix][iy][it] + " " + x_m[ix][iy][it] + " " + y_m[ix][iy][it] + " " + counts[ix][iy][it]);
					} else {
						x_m[ix][iy][it] = (0.5 + ix) * groupPixelsX;
						y_m[ix][iy][it] = (0.5 + iy) * groupPixelsY;
						tof_m[ix][iy][it] = tmap[it];
//						System.out.println("No: " + tof_m[ix][iy][it] + " " + x_m[ix][iy][it] + " " + y_m[ix][iy][it] + " " + counts[ix][iy][it]);
				   }
					if (it > 0)
						counts[ix][iy][it] /= (tmap[it] - tmap[it - 1]);  //Math.log(it + 1) / (it * it);
					else
						counts[ix][iy][it] /= (tmap[it + 1] - tmap[it]);  //Math.log(it + 1) / (it * it);
				}

	}
	public boolean readallSpectra() {

		originalPixelsNumberX = MaudPreferences.getInteger("LumaCam.dataPixelsNumberX", 256);
		originalPixelsNumberY = MaudPreferences.getInteger("LumaCam.dataPixelsNumberY", 256);

		boolean flipX = MaudPreferences.getBoolean("LumaCam.flipX", false);
		boolean flipY = MaudPreferences.getBoolean("LumaCam.flipY", false);
		boolean flipXY = MaudPreferences.getBoolean("LumaCam.flipXY", false);
		int startX = MaudPreferences.getInteger("LumaCam.startingX", 0);
		int cutX = MaudPreferences.getInteger("LumaCam.cutXatEnd", 0);
		int startY = MaudPreferences.getInteger("LumaCam.startingY", 0);
		int cutY = MaudPreferences.getInteger("LumaCam.cutYatEnd", 0);

		groupPixelsX = MaudPreferences.getInteger("LumaCam.groupXPixelsBy", 8);
		groupPixelsY = MaudPreferences.getInteger("LumaCam.groupYPixelsBy", 8);

		sensorSizeX = MaudPreferences.getDouble("LumaCam.sensorSizeX_mm", 134.0);
		sensorSizeY = MaudPreferences.getDouble("LumaCam.sensorSizeY_mm", 134.0);
		pixelsNumberX = originalPixelsNumberX / groupPixelsX;
		pixelsNumberY = originalPixelsNumberY / groupPixelsY;
		pixelSizeX = sensorSizeX / originalPixelsNumberX;
		pixelSizeY = sensorSizeY / originalPixelsNumberY;

		tofNumber = MaudPreferences.getInteger("LumaCam.numberTOFpoints", 2048);

		clockWidth = MaudPreferences.getDouble("LumaCam.clockWidth", 100.0);
		minTOF = MaudPreferences.getDouble("LumaCam.minTOF", 1.0E-5);
		maxTOF = MaudPreferences.getDouble("LumaCam.maxTOF", 10000.0);

		boolean loadSuccessfull = false;
		boolean tmpB = isAbilitatetoRefresh;
		isAbilitatetoRefresh = false;

		BufferedReader reader = getReader();
		if (reader != null) {
			try {
				int index = 1;
				clockWidth = 100.0;
				int bankNumber = 1;

				String token = new String("");
				StringTokenizer st = null;
				String linedata = reader.readLine();
				boolean endoffile = false;

				while (!linedata.startsWith("X\tY"))
					linedata = reader.readLine();

				double omega = 0.0, chi = 0.0, phi = 0.0, eta = 0.0;
				double scale_factor = 1.0;

				counts = new double[pixelsNumberX][pixelsNumberY][tofNumber];
				x_m = new double[pixelsNumberX][pixelsNumberY][tofNumber];
				y_m = new double[pixelsNumberX][pixelsNumberY][tofNumber];
				tof_m = new double[pixelsNumberX][pixelsNumberY][tofNumber];

				tmap = new double[tofNumber];

//				if (minTOF < 1.0E-6)
//					minTOF = 1.0E-6;
//				coeffB = (1.0 / maxTOF - 1.0 / minTOF) / (Math.log(tofNumber));
				coeffB = (maxTOF - minTOF) / ((tofNumber - 1) * (tofNumber - 1));
//				coeffA = 1.0 / minTOF;
				coeffA = minTOF;
//				System.out.println("Tmap(" + tofNumber + "): " + minTOF + " " + maxTOF + " " + coeffA + " " + coeffB);

				for (int i = 0; i < tofNumber; i++) {
					tmap[i] = coeffA + coeffB * i * i;
//					tmap[i] = 1.0 / (coeffA + coeffB * Math.log(1.0 + i));
//					System.out.println(i + " " + tmap[i]);
				}
//					tmap[i] = coeffA + coeffB * (0.5 + i);

				linedata = reader.readLine();
				st = new StringTokenizer(linedata, " ,\t\r\n");
				while (linedata != null) {
					if (st.hasMoreTokens()) {
						double x = Double.parseDouble(st.nextToken());
						double y = Double.parseDouble(st.nextToken());
						if (flipXY) {
							double tmp = x;
							x = y;
							y = tmp;
						}
						if (flipX)
							x = originalPixelsNumberX - x;
						if (flipY)
							y = originalPixelsNumberY - y;
						double phot = Double.parseDouble(st.nextToken());
						double tof = Double.parseDouble(st.nextToken()) * 1.0E6; // / clockWidth;
						double psd = Double.parseDouble(st.nextToken());
						totalCounts += phot;
						addToCount(x, y, phot, tof, psd);
					}

					linedata = reader.readLine();
					if (linedata != null) {
						st = new StringTokenizer(linedata, " ,\t\r\n");
						if (!st.hasMoreTokens())
							linedata = null;
					}
				}
				normalize();
				System.out.println("Total number of photon detected: " + totalCounts);
				for (int ix = startX; ix < pixelsNumberX - cutX; ix++) {
					for (int iy = startY; iy < pixelsNumberY - cutY; iy++) {
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

								double x1 = x_m[ix][iy][i1] * pixelSizeX;
								double y1 = y_m[ix][iy][i1] * pixelSizeY;
								datafile.setXImage(i1, x1);
								datafile.setYImage(i1, y1);
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
