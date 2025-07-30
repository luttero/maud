/*
 * @(#)TOFMeasurement.java created 07/01/1999 Pergine Vals.
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.measurement;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.cal.GSASbankCalibration;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.MoreMath;

/**
 *  The TOFMeasurement is a class
 *
 *
 * @version $Revision: 1.7 $, $Date: 2006/11/10 09:33:01 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class TOFMeasurement extends Measurement {

  public static String modelID = "TOF";

  public static String[] diclistc = {};
  public static String[] diclistcrm = {};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  public TOFMeasurement(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = modelID;
    IDlabel = modelID;
    description = "TOF measurement";
  }

  public TOFMeasurement(XRDcat aobj) {
    this(aobj, modelID);
  }

  public TOFMeasurement() {
    identifier = modelID;
    IDlabel = modelID;
    description = "TOF measurement";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
  }

  public void initParameters() {
    super.initParameters();
  }

  public boolean isTOF() {
    return true;
  }

  public double getCorrectedPosition(Sample asample, double x, double[] angles, double radius, DiffrDataFile adatafile, int ppp) {
//  public double getCorrectedPosition(Sample asample, double x, double[] tilting_angles,
//                                     DiffrDataFile adatafile, int ppp) {
    double yShift = 0;
    if (ppp > 0)
      yShift = asample.getSampleShapeModel().getYShiftFor(ppp);

    double[] xyz = asample.getSpecimenPrecessionError().getXYZForPrecession(angles, x);

    double commonFactor = 90.0 / (Math.PI * radius);

    double[] xyzg = new double[3];
    DataFileSet adataset = adatafile.getDataFileSet();
    xyzg[0] = adataset.getXshift();
    xyzg[1] = adataset.getYshift();
    xyzg[2] = adataset.getZshift();

    double cosphi = MoreMath.cosd(angles[2]);
    double sinphi = MoreMath.sind(angles[2]);
    double x1 = xyzg[0] * cosphi + xyzg[1] * sinphi;
    xyzg[1] = -xyzg[0] * sinphi + xyzg[1] * cosphi;
    xyzg[0] = x1;

    double coschi = MoreMath.cosd(angles[1]);
    double sinchi = MoreMath.sind(angles[1]);
    x1 = xyzg[1] * coschi + xyzg[2] * sinchi;
    xyzg[2] = -xyzg[1] * sinchi + xyzg[2] * coschi;
    xyzg[1] = x1;

    double cosomega = MoreMath.cosd(angles[0]);
    double sinomega = MoreMath.sind(angles[0]);
    x1 = xyzg[0] * cosomega + xyzg[1] * sinomega;
    xyzg[1] = -xyzg[0] * sinomega + xyzg[1] * cosomega;
    xyzg[0] = x1;

    double xp = xyz[0] + xyzg[0];
    double yp1 = xyz[1] + xyzg[1];
    double zp1 = xyz[2] + xyzg[2];
//    System.out.println(asample.xshift + " " + asample.yshift + " " + asample.zshift);
//    System.out.println(xp + " " + yp + " " + zp);
    Calibration cal = ((Instrument) getParent()).getAngularCalibration();
//    double xt = cal.notCalibrated(adatafile, (double) x);
    double flightPath = 9000.0;
    if (cal instanceof GSASbankCalibration)
      flightPath = ((GSASbankCalibration) cal).getFlightPath();
    double R = cal.getDetectorDistanceValue(adatafile);
    double totalpathDen = 1.0 / (flightPath + R);
    double ttheta = ((Instrument) getParent()).getGeometry().getThetaDetector(adatafile, x) * Constants.DEGTOPI;

    double etaRad = angles[3] * Constants.DEGTOPI;
    double sinEta = Math.sin(etaRad);
    double cosEta = Math.cos(etaRad);

    double yp = zp1 * cosEta + yp1 * sinEta;
    double zp = -zp1 * sinEta + yp1 * cosEta;

    double cos2theta = Math.cos(ttheta);
    double sin2theta = Math.sin(ttheta);
    double cottheta = 1.0 / Math.tan(ttheta / 2.0);
    double L12denCottheta = cottheta / (2.0 * R);

    double dpx = -xp * ((1.0 - cos2theta) * totalpathDen + sin2theta * L12denCottheta);
    double dpy = (Math.sqrt(R * R + yp * yp) - R) * totalpathDen;
    double dpz = zp * (sin2theta * totalpathDen + (cottheta / (2.0 * flightPath) + cos2theta * L12denCottheta));
    if (ttheta < 0.0)
      dpz = - dpz;

    double dp = 1.0 + dpx + dpy + dpz;
//    System.out.println(angles[3] + " " + dp);

//    double toLambda = 2.0 * MoreMath.sind(Math.abs(ttheta) / 2.0);
//    if (toLambda > 0)
//      xt += dp * x / toLambda;
//    xt = ((Instrument) getParent()).getAngularCalibration().calibrateX(adatafile, (double) xt);
/*    double s = -yp * MoreMath.sind(angles[3] - 90.0) + zp * MoreMath.sind(angles[3]);

    double den = MoreMath.sind(ttheta / 2.0 + (s * MoreMath.cosd(ttheta) +
            xp * MoreMath.sind(ttheta)) / (2.0 * getRadius(adatafile)));
    if (den == 0.0)
      return x;
    den = MoreMath.sind(ttheta / 2.0) / den;
//		System.out.println("Corr: " + den + " " + adatafile + " " + x);

    return x * den;       */

    return x * dp;
  }

}
