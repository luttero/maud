/*
 * @(#)TwoThetaMeasurement.java created 07/01/1999 Pergine Vals.
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
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

import java.awt.*;
import java.awt.*;
import java.awt.event.*;

/**
 *  The TwoThetaMeasurement is a class
 *
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class TwoThetaMeasurement extends Measurement {

  public static String[] diclistc = {};
  public static String[] diclistcrm = {};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  public TwoThetaMeasurement(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "2Theta";
    IDlabel = "2Theta";
    description = "2Theta measurement method";
  }

  public TwoThetaMeasurement(XRDcat aobj) {
    this(aobj, "2Theta");
  }

  public TwoThetaMeasurement() {
    identifier = "2Theta";
    IDlabel = "2Theta";
    description = "2Theta measurement method";
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

/*

	absorption correction for fixed theta measurement by
	Heissmann, J. Appl. Cryst. .....
	translated in omega, 2theta angle convention by D. Chateigner for the INEL CPS120

*/
/*	public double getIntensityCorrection(	DiffrDataFile adatafile, Sample asample, double position, boolean dspacingbase) {

		double[] tilting_angles = adatafile.getTiltingAngle();

		if (!dspacingbase)
			return 2.0 / (MoreMath.sind(tilting_angles[0]) * getAbsorptionCorrection(adatafile, asample, position, dspacingbase));
		else
			return 1.0;
	}

	public double getAbsorptionCorrection(DiffrDataFile adatafile, Sample asample, double position, boolean dspacingbase) {

		double[] tilting_angles = adatafile.getTiltingAngle();

		double twothetaMinusOmega = position - tilting_angles[0];

		if (dspacingbase)
			return 1.0;
		if (twothetaMinusOmega > 1.0E-79)
			return 1.0 / MoreMath.sind(tilting_angles[0]) + 1.0 / MoreMath.sind(twothetaMinusOmega);
		else
			return 1.0E79;
	}*/

  public double getCorrectedPosition(Sample asample, double x, double[] angles, double radius, DiffrDataFile adatafile) {

    double sintheta = MoreMath.sind(x);
    double costheta = MoreMath.cosd(x);

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

//    double[] xyz = asample.getSpecimenPrecessionError().getXYZForPrecession(angles, x);

//    double xp = xyz[0] + xyzg[0];
//    double yp = xyz[1] + xyzg[1];
//    double zp = xyz[2] + xyzg[2];

    double delta2thetax = -xyzg[0] * sintheta * commonFactor;
    double delta2thetaz = xyzg[2] * costheta * commonFactor;
// todo : missing eta angle correction using yp1

    return x + delta2thetax + delta2thetaz;
/*
//    old one method
//    System.out.println("Omega = " + tilting_angles[0]);
//    System.out.println("2theta = " + x);
    double sinomega = MoreMath.sind(angles[0]);
    double dt = (asample.zshift + asample.yshift * MoreMath.tand(angles[1]))
            * MoreMath.sind(x) + asample.xshift * (MoreMath.sind(x - angles[0]) - sinomega);
    double den = radius * sinomega;
//    System.out.println("dt = " + dt);
//    System.out.println("den = " + den);
//    System.out.println("d2theta = " + (dt/den * Constants.PITODEG));

    if (den != 0.0)
      return x + dt / den * Constants.PITODEG;
    else
      return x;*/
  }

}
