/*
 * @(#)TOFPanelIntensityCalibration.java created 7/06/2023 Los Alamos
 *
 * Copyright (c) 2023 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.io.cif.CIFdictionary;
import it.unitn.ing.rista.io.cif.CIFtoken;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.util.ArrayList;
import java.util.StringTokenizer;

/**
 *  The TOFPanelIntensityCalibration is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2023/06/07 14:54:53 $
 * @author Luca Lutterotti
 * @since JDK19
 */

public class TOFPanelIntensityCalibration extends HippoMultBankIntCalibration {

	public static String modelID = "Multibank 2D Incident Spectrum";

	public TOFPanelIntensityCalibration(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		identifier = modelID;
		IDlabel = modelID;
	}

	public TOFPanelIntensityCalibration(XRDcat aobj) {
		this(aobj, "");
	}

	public TOFPanelIntensityCalibration() {
		identifier = modelID;
		IDlabel = modelID;
	}

	public double calibrateData(DiffrDataFile datafile, double x, int index, double d) {
//    updateStringtoDoubleBuffering(false);
		int bank = getBankNumber(datafile);
		double wt = 0.0, tx = 0.0, cal = 0.0, timeCorr = 0.0;
		x /= 1000.0;
		int typeBankFunction = typeNumber[bank];
		int typeSplitBankFunction = typeSplitNumber[bank];
		if (typeBankFunction < 0 && typeSplitBankFunction < 0) {
			typeBankFunction = -typeBankFunction;
			typeSplitBankFunction = -typeSplitBankFunction;
			GSASbankCalibration calib = (GSASbankCalibration) getInstrument().getAngularCalibration();
			DataFileSet dataset = getFilePar().getActiveSample().getActiveDataSet(0);
			GSASbankCalibration calib_ref = (GSASbankCalibration) dataset.getInstrument().getAngularCalibration();
			double distDiff = calib.getDetectorDistanceValue(bank) -
					calib_ref.getDetectorDistanceValue(bank);
			timeCorr = 2.0 * distDiff * Constants.LAMBDA_SPEED_NEUTRON_CONV_ANG * d *
					MoreMath.sind(calib.getTtheta(bank).getValueD() * 0.5);
//			System.out.println(bank + " " + x + " " + timeCorr + " " + distDiff + " " + d);
		}
		x += timeCorr;
		if (index > splitPosition) {
			switch (typeBankFunction) {
				case 1:
					wt = difc[bank][0];
					double w4 = 1.0;
					for (int i = 1; i <= 5; i++) {
						w4 *= x;
						int i1 = i * 2;
						wt += difc[bank][i1 - 1] * Math.exp(-difc[bank][i1] * w4);
					}
					break;
				case 2:
					wt = difc[bank][0];
					w4 = x * x;
					wt += difc[bank][1] * Math.exp(-difc[bank][2] / w4) / (w4 * w4 * x);
					w4 = x;
					for (int i = 2; i <= 5; i++) {
						w4 *= x;
						int i1 = i * 2;
						wt += difc[bank][i1 - 1] * Math.exp(-difc[bank][i1] * w4);
					}
					break;
				case 3:

					tx = 2.0 / x - 1.0;
					wt = 0.0;

					for (int i = 0; i < numberIncSpectrumCoefficients; i++)
						wt += difc[bank][i] * ChebyshevPolynomial.getT(i, tx);

					break;
				case 4:

					tx = 2.0 / x - 1.0;
					wt = difc[bank][0];
					w4 = x * x;
					wt += difc[bank][1] * Math.exp(-difc[bank][2] / w4) / (w4 * w4 * x);
//        w4 = x;
					for (int i = 3; i < numberIncSpectrumCoefficients; i++)
						wt += difc[bank][i] * ChebyshevPolynomial.getT(i - 2, tx);

					break;
				case 5:
					tx = x / 10.0;
					wt = 0.0;

					for (int i = 0; i < numberIncSpectrumCoefficients; i++)
						wt += difc[bank][i] * ChebyshevPolynomial.getT(i, tx);

					break;
				case 6:
					tx = 1.0 / x;
					wt = 0.0;

					for (int i = 0; i < numberIncSpectrumCoefficients; i++)
						wt += difc[bank][i] * ChebyshevPolynomial.getT(i, tx);

					break;
				case 10:
//		    System.out.println(this + " " + bank + " " + index + " " + incidentSpectrum.get(bank).length + " " + incidentSpectrum.size());
					wt = incidentSpectrum.get(bank)[index];
					break;
				default: {
					wt = 1.0;
				}
			}
			cal = wt * difc[bank][numberIncSpectrumCoefficients];
		} else {
			switch (typeSplitBankFunction) {
				case 1:
					wt = difc2[bank][0];
					double w4 = 1.0;
					for (int i = 1; i <= 5; i++) {
						w4 *= x;
						int i1 = i * 2;
						wt += difc2[bank][i1 - 1] * Math.exp(-difc2[bank][i1] * w4);
					}
					break;
				case 2:
					wt = difc2[bank][0];
					w4 = x * x;
					wt += difc2[bank][1] * Math.exp(-difc2[bank][2] / w4) / (w4 * w4 * x);
					w4 = x;
					for (int i = 2; i <= 5; i++) {
						w4 *= x;
						int i1 = i * 2;
						wt += difc2[bank][i1 - 1] * Math.exp(-difc2[bank][i1] * w4);
					}
					break;
				case 3:

					tx = 2.0 / x - 1.0;
					wt = 0.0;

					for (int i = 0; i < numberIncSpectrumCoefficients; i++)
						wt += difc2[bank][i] * ChebyshevPolynomial.getT(i, tx);

					break;
				case 4:

					tx = 2.0 / x - 1.0;
					wt = difc2[bank][0];
					w4 = x * x;
					wt += difc2[bank][1] * Math.exp(-difc2[bank][2] / w4) / (w4 * w4 * x);
//        w4 = x;
					for (int i = 3; i < numberIncSpectrumCoefficients; i++)
						wt += difc2[bank][i] * ChebyshevPolynomial.getT(i - 2, tx);

					break;
				case 5:
					tx = x / 10.0;
					wt = 0.0;

					for (int i = 0; i < numberIncSpectrumCoefficients; i++)
						wt += difc2[bank][i] * ChebyshevPolynomial.getT(i, tx);

					break;
				case 6:
					tx = 1.0 / x;
					wt = 0.0;

					for (int i = 0; i < numberIncSpectrumCoefficients; i++)
						wt += difc2[bank][i] * ChebyshevPolynomial.getT(i, tx);

					break;
				case 10:
//		    System.out.println(this + " " + bank + " " + index + " " + incidentSpectrum.get(bank).length + " " + incidentSpectrum.size());
					wt = incidentSpectrum.get(bank)[index];
					break;
				default:
				{
					wt = 1.0;
				}
			}
			cal = wt * difc2[bank][numberIncSpectrumCoefficients];

		}
		if (cal < MINIMUM_INTENSITY_CALIBRATION_VALUE)
			cal = MINIMUM_INTENSITY_CALIBRATION_VALUE;
		return cal;
	}

}
