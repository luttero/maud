/*
 * @(#)AngularFlatImageReflectionCalibration.java created Jul 6, 2007 Caen
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.diffr.cal;

import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.diffr.DiffrDataFile;
import it.unitn.ing.rista.util.Constants;

/**
 * The AngularFlatImageReflectionCalibration is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jul 6, 2007 5:03:24 PM $
 * @since JDK1.1
 */
public class AngularFlatImageReflectionCalibration extends AngularFlatImageTransmissionCalibration {
  public AngularFlatImageReflectionCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
//    initXRD();
    identifier = "Flat Image Reflection";
    IDlabel = "Flat Image Reflection";
  }

  public AngularFlatImageReflectionCalibration(XRDcat aobj) {
    this(aobj, "Flat Image Reflection calibration x");
  }

  public AngularFlatImageReflectionCalibration() {
    identifier = "Flat Image Reflection";
    IDlabel = "Flat Image Reflection";
  }

  public void calibrateX(DiffrDataFile datafile) {
    int datanumber = datafile.getTotalNumberOfData();
    updateParametertoDoubleBuffering(false);
    double eta = datafile.getEtaValue() * Constants.DEGTOPI;
    double coseta = Math.cos(eta);
    double sineta = Math.sin(eta);
    double Xc = getParameterValue(2);
    double Yc = getParameterValue(3);
    double Sx = getParameterValue(4);
    double Sy = getParameterValue(5);
    double ratio = getParameterValue(6);
    double angcal = 0.0;
    double detectDist2 = detectorDistance * detectorDistance;
    for (int i = 0; i < datanumber; i++) {
      double value = datafile.getXDataForCalibration(i);
      double x = value * coseta * ratio;
      double y = value * sineta;
      double distX = x - Xc;
      double distY = y - Yc;
      double distSx = distX * Sx;
      double distSy = distY * Sy;
      distX *= distX;
      distY *= distY;
      double dist = distSx + distSy;
      double upper = distSx + distSy + detectorDistance;
      double lower = detectDist2 + 2.0 * detectorDistance * dist + distX + distY;
      angcal = 180.0 - Math.acos(upper / Math.sqrt(lower)) * Constants.PITODEG;

      datafile.setCalibratedXDataOnly(i, angcal);
    }
  }

}
