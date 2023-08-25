/*
 * @(#)LoskoPanelCalibration.java created 21/08/2023 Los Alamos
 *
 * Copyright (c) 2099 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.cal;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import java.lang.*;

/**
 *  The LoskoPanelCalibration is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2023/08/21 17:39:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class LoskoPanelCalibration extends MultiBankCalibration {

	public static String modelID = "LumaCam Panel";

	public LoskoPanelCalibration(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		identifier = modelID;
		IDlabel = modelID;
	}

	public LoskoPanelCalibration(XRDcat aobj) {
		this(aobj, modelID);
	}

	public LoskoPanelCalibration() {
		identifier = modelID;
		IDlabel = modelID;
	}

	public double getTthetaValue(DiffrDataFile datafile, double twotheta) {
		return theta[getBankNumber(datafile)];
	}

	public double getEtaValue(DiffrDataFile datafile) {
		return eta[getBankNumber(datafile)];
	}

	public double getDetectorDistanceValue(DiffrDataFile datafile) {
		return dist[getBankNumber(datafile)];
	}

	public double getDetectorDistanceValue(int bank) {
		return dist[bank];
	}

	public Parameter getDifc(int index) {
		if (index >= 0)
			return (Parameter) parameterloopField[0].elementAt(index);
		else
			return null;
	}

	public Parameter getDifa(int index) {
		if (index >= 0)
			return (Parameter) parameterloopField[1].elementAt(index);
		else
			return null;
	}

	public Parameter getZero(int index) {
		if (index >= 0)
			return (Parameter) parameterloopField[2].elementAt(index);
		else
			return null;
	}

	public Parameter getTtheta(int index) {
		if (index >= 0)
			return (Parameter) parameterloopField[3].elementAt(index);
		else
			return null;
	}

	public Parameter getEta(int index) {
		if (index >= 0)
			return (Parameter) parameterloopField[4].elementAt(index);
		else
			return null;
	}

	public Parameter getDetectorDistance(int index) {
		if (index >= 0)
			return (Parameter) parameterloopField[5].elementAt(index);
		else
			return null;
	}

	public void calibrateX(DiffrDataFile datafile) {
		int datanumber = datafile.getTotalNumberOfData();
//    int banknumber = getBankNumber(datafile);
		updateParametertoDoubleBuffering(false);
		int banknumber = getBankNumber(datafile);
		if (banknumber >= difa.length || banknumber < 0) {
			System.out.println("Warning, bank number: " + banknumber + " out of range: 0 - " + difa.length);
			return;
		}
		for (int i = 0; i < datanumber; i++) {
			double value = datafile.getXDataForCalibration(i);
			if (difa[banknumber] == 0.0) {
				if (difc[banknumber] != 0.0)
					value = (value - zero[banknumber]) / difc[banknumber];
				else
					value = value - zero[banknumber];
			} else
				value = (-difc[banknumber] + Math.sqrt(difc[banknumber] * difc[banknumber] -
						(4 * difa[banknumber] * (zero[banknumber] - value)))) / (2 * difa[banknumber]);
			datafile.setCalibratedXDataOnly(i, value);
		}
	}

	public double calibrateX(DiffrDataFile datafile, double value) {
//    int datasetsNumber = datafile.getTotalNumberOfData();
//    int banknumber = getBankNumber(datafile);
//    updateParametertoDoubleBuffering();
		int banknumber = getBankNumber(datafile);
		if (banknumber >= difa.length || banknumber < 0) {
			System.out.println("Problem, bank number: " + banknumber + " out of range: 0 - " + difa.length);
			return value;
		}
		if (difa[banknumber] == 0.0) {
			if (difc[banknumber] != 0.0)
				value = (value - zero[banknumber]) / difc[banknumber];
			else
				value = value - zero[banknumber];
		} else
			value = (-difc[banknumber] + Math.sqrt(difc[banknumber] * difc[banknumber] -
					(4 * difa[banknumber] * (zero[banknumber] - value)))) / (2 * difa[banknumber]);
		return value;
	}

	public double notCalibrated(DiffrDataFile datafile, double x) {
		int banknumber = getBankNumber(datafile);
		return difc[banknumber] * x + difa[banknumber] * x * x + zero[banknumber];
	}

/*	public void panelUnrolling(DiffrDataFile datafile) {

		datafile.getIma

		double[] xf;
		double angcal;

		int datanumber = datafile.getTotalNumberOfData();
		updateParametertoDoubleBuffering(false);

		DataFileSet dataset = datafile.getDataFileSet();
		double zs = dataset.getZshift();
		double rx = dataset.getSample().getRadiusDimensionXD();
		double ry = dataset.getSample().getRadiusDimensionYD();
		double[] tiltingAngles = datafile.getTiltingAngle();
		double omega = tiltingAngles[0];
		double chi = tiltingAngles[1];
		double detectorProper2Theta = detector2Theta + tiltingAngles[4];

		if (Math.abs(omega) > 1.0E-18)
			zs /= Math.cos((90.0 - omega) * Constants.DEGTOPI);
		zs /= Math.cos(chi * Constants.DEGTOPI);
		double dx = zs * Math.cos((detectorProper2Theta - 90.0) * Constants.DEGTOPI);
		double dd = zs * Math.sin((detectorProper2Theta - 90.0) * Constants.DEGTOPI);

		double[][] tmat = ConvertImageToSpectra.getTransformationMatrixNew(detectorOmegaDN, detectorPhiDA,
				detectorEtaDA, detectorProper2Theta, omega);
//    double pangcal = -999.0;
		for (int i = 0; i < datanumber; i++) {
//      double value = datafile.getXDataOriginal(i);
			double x = datafile.getXDataImage(i);
			double y = datafile.getYDataImage(i);

			xf = ConvertImageToSpectra.getTransformedVectorNew(tmat, x, y, centerX + dx, centerY, detectorDistance - dd);
			angcal = ConvertImageToSpectra.get2ThetaNew(xf) * Constants.PITODEG;
//      if (Math.abs(pangcal - angcal) < 1E-6)
//	      System.out.println(i + " " + angcal + " " + pangcal + " " + x + " " + y + " " + datafile.getXDataImage(i - 1) + " " + datafile.getYDataImage(i - 1));
//	    pangcal = angcal;
			if (rx != 0) { // || ry != 0) {
				double xc = rx * MoreMath.sind(90.0 - angcal / 2.0);
				double zc = rx * (1.0 - MoreMath.cosd(90.0 - angcal));
				double dx1 = (zs - zc) * Math.cos((detectorProper2Theta - 90.0) * Constants.DEGTOPI);
				double dd1 = (zs - zc) * Math.sin((detectorProper2Theta - 90.0) * Constants.DEGTOPI);
				double dx2 = xc * Math.sin((detectorProper2Theta - 90.0) * Constants.DEGTOPI);
				double dd2 = xc * Math.cos((detectorProper2Theta - 90.0) * Constants.DEGTOPI);
				xf = ConvertImageToSpectra.getTransformedVectorNew(tmat, x, y, centerX + dx1 + dx2, centerY, detectorDistance - dd1 - dd2);
				angcal = ConvertImageToSpectra.get2ThetaNew(xf) * Constants.PITODEG;
			}

			datafile.setCalibratedXDataOnly(i, angcal);
		}
	}
	public double[][] getTmat(double omega) {
		return ConvertImageToSpectra.getTransformationMatrixNew(detectorOmegaDN, detectorPhiDA,
				detectorEtaDA, detector2Theta, omega);
	}

	public double getBeamInclination(DiffrDataFile datafile, int index, double[][] tmat) {
		updateParametertoDoubleBuffering(false);

		double zs = datafile.getDataFileSet().getZshift();
//    System.out.println("zs = " + zs);
		double[] tiltingAngles = datafile.getTiltingAngle();
		double omega = tiltingAngles[0];
		if (Math.abs(omega) > 1.0E-9)
			zs /= Math.sin(omega * Constants.DEGTOPI);
		double chi = tiltingAngles[1];
		double detectorProper2Theta = detector2Theta + tiltingAngles[4];
		zs /= Math.cos(chi * Constants.DEGTOPI);
		double dx = zs * Math.sin((180.0 - detectorProper2Theta) * Constants.DEGTOPI);
		double dd = zs * Math.cos((180.0 - detectorProper2Theta) * Constants.DEGTOPI);

		double x = datafile.getXDataImage(index);
		double y = datafile.getYDataImage(index);

		double[] xf = ConvertImageToSpectra.getTransformedVectorNew(tmat, x, y, centerX + dx, centerY, detectorDistance - dd);
		double[] xfc = ConvertImageToSpectra.getTransformedVectorNew(tmat, 0, 0, centerX + dx, centerY, detectorDistance - dd);

		double angle = Math.abs(MoreMath.getAngleBetweenPoints(xf, xfc));
		angle += Constants.PI_2;

		return angle;
	}*/

}
