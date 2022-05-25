/*
 * @(#)GeometryIPNS_LANSCE.java created 06/01/1999 Riva del Garda
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

package it.unitn.ing.rista.diffr.geometry;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.cal.GSASbankCalibration;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

import java.awt.*;
import javax.swing.*;

/**
 *  The GeometryIPNS_LANSCE is a class
 *
 *
 * @version $Revision: 1.8 $, $Date: 2006/11/10 09:33:01 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GeometryIPNS_LANSCE extends GeometryDebyeScherrer {

  public static String modelID = "IPNS/LANSCE TOF";

  public GeometryIPNS_LANSCE(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = modelID;
    IDlabel = modelID;
    description = modelID + " instrument geometry";
  }

  public GeometryIPNS_LANSCE(XRDcat aobj) {
    this(aobj, modelID);
  }

  public GeometryIPNS_LANSCE() {
    identifier = modelID;
    IDlabel = modelID;
    description = modelID + " instrument geometry";
  }

  public double[] getTextureAngles(DiffrDataFile datafile, double[] tilting_angles,
                                  double[] sampleAngles, double twotheta) {
    double[] newtilting_angles = new double[4];
    newtilting_angles[0] = tilting_angles[0];
    newtilting_angles[1] = tilting_angles[1];
    newtilting_angles[2] = tilting_angles[2];
    newtilting_angles[3] = tilting_angles[3];

    double theta_detector = getThetaDetector(datafile, twotheta);

    double newTwoTheta = theta_detector; //180.0 - theta_detector;
    return super.getTextureAngles(datafile, newtilting_angles, sampleAngles, newTwoTheta);
  }

  public double[][] getIncidentAndDiffractionAngles(DiffrDataFile datafile, double[] tilting_angles,
                                                 double[] sampleAngles, double[] twotheta) {

    int numberPositions = twotheta.length;
    double[] theta_detector = new double[numberPositions];
    for (int j = 0; j < numberPositions; j++)
      theta_detector[j] = getThetaDetector(datafile, twotheta[j]);
    return super.getIncidentAndDiffractionAngles(datafile, tilting_angles, sampleAngles, theta_detector);

  }

  public double[] getTextureAngles(double[] tilting_angles, double twotheta) {

    double[] newtilting_angles = new double[4];
    newtilting_angles[0] = tilting_angles[0];
    newtilting_angles[1] = tilting_angles[1];
    newtilting_angles[2] = tilting_angles[2];
    newtilting_angles[3] = tilting_angles[3];
    return super.getTextureAngles(newtilting_angles, twotheta);

  }

  public double LorentzPolarization(DiffrDataFile adatafile, Sample asample, double position, boolean dspacingbase, boolean energyDispersive) {

    double theta_detector = getThetaDetector(adatafile, position) / 2;
//		double eta_detector = getEtaDetector(adatafile);
    double lp = Math.abs(Math.sin(theta_detector * Constants.DEGTOPI));
    lp *= position * position * position * position;

    return lp;
  }

  public double getRadius(DiffrDataFile adatafile) {
    return ((Instrument) getParent()).getAngularCalibration().getDetectorDistanceValue(adatafile);
  }

  public double getCorrectedPosition(Sample asample, double x, double[] tilting_angles,
                                     DiffrDataFile adatafile) {
    double[] angles = getTrueTiltingAngles(adatafile, tilting_angles, x);

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
    double ttheta = getThetaDetector(adatafile, x) * Constants.DEGTOPI;

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

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JGeometryILOptionsD(parent, this);
    return adialog;
  }

  public class JGeometryILOptionsD extends JOptionsDialog {

    public JGeometryILOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout());
      principalPanel.add(new JLabel("No options for this geometry"));

      setTitle("Options panel");
      initParameters();
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }
  }

}
