/*
 * @(#)GeometryDebyeScherrer.java created 06/01/1999 Riva del Garda
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.MoreMath;

/**
 *  The GeometryDebyeScherrer is a class to apply correction for Debye-Scherrer
 *  geometry.
 *
 *
 * @version $Revision: 1.11 $, $Date: 2006/07/20 13:39:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GeometryDebyeScherrer extends GeometryDiffractometer {

  public GeometryDebyeScherrer(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "Debye-Scherrer";
    IDlabel = "Debye-Scherrer";
    description = "Debye-Scherrer instrument geometry";
  }

  public GeometryDebyeScherrer(XRDcat aobj) {
    this(aobj, "Debye-Scherrer");
  }

  public GeometryDebyeScherrer() {
    identifier = "Debye-Scherrer";
    IDlabel = "Debye-Scherrer";
    description = "Debye-Scherrer instrument geometry";
  }

  public double Lorentz(DiffrDataFile adatafile, double position) {
    double sintheta, costheta, lp;
    sintheta = Math.sin(position);
    costheta = Math.cos(position);
    lp = 0.5 / (costheta * sintheta * sintheta);
    return lp;
  }

  public void computeShapeAbsorptionCorrection(DiffrDataFile adatafile, Sample asample, double[][] position,
                                               boolean dspacingbase, boolean energyDispersive, double[][] intensity, double toLambda) {

    double[] sampleAngles = asample.getSampleAngles();
    double[] tilting_angles = adatafile.getTiltingAngle();

    double[][][] angles = getIncidentAndDiffractionAngles(adatafile, tilting_angles, sampleAngles, position);

    RadiationType rad = ((Instrument) getParent()).getRadiationType();

    asample.computeAbsorptionTroughPath(rad, angles, position, intensity, toLambda);
  }

/*  public double getCorrectedPosition(Sample asample, double x, double[] tilting_angles, DiffrDataFile adatafile) {
//    System.out.println("Omega = " + tilting_angles[0]);
//    System.out.println("2theta = " + x);
    double[] angles = getTrueTiltingAngles(adatafile, tilting_angles, (double) x);

    
    double radius = getRadius(null);
    double sintheta = MoreMath.sind(x);
    double costheta = MoreMath.cosd(x);

    double commonFactor = 90.0 / (Math.PI * radius);

    double[] xyzg = new double[3];
    xyzg[0] = asample.xshift;
    xyzg[1] = asample.yshift;
    xyzg[2] = asample.zshift;

    double coschi = MoreMath.cosd(angles[1]);
    double sinchi = MoreMath.sind(angles[1]);

    double yt1 = xyzg[1] * coschi + xyzg[2] * sinchi;
    xyzg[2] = -xyzg[1] * sinchi + xyzg[2] * coschi;
    double cosomega = MoreMath.cosd(angles[0]);
    double sinomega = MoreMath.sind(angles[0]);
    xyzg[0] = xyzg[0] * cosomega + yt1 * sinomega;
    xyzg[1] = -xyzg[0] * sinomega + yt1 * cosomega;

    double[] xyz = asample.getSpecimenPrecessionError().getXYZForPrecession(angles, x);

    double xp = xyz[0] + xyzg[0];
    double yp = xyz[1] + xyzg[1];
    double zp = xyz[2] + xyzg[2];

    double delta2thetax = -xp * sintheta * commonFactor;
    double delta2thetaz = zp * costheta * commonFactor;
// todo : missing eta angle correction using yp1

    return x + delta2thetax + delta2thetaz;
  }
*/

	public double getBeamRelatedCorrection(DiffrDataFile adatafile, Sample asample, double position, int pointIndex) {

		double lp = 1.0, beamsizez, beamsizey, delta;
		if ((slitaperture > 0.0 || slitaperturey > 0.0) && radius > 0.0) {

//		double sintheta = Math.sin(position * Constants.DEGTOPI / 2.0);

			double sradius = asample.getRadiusDimensionXD();
			if (sradius > 0.0)
				sradius = 2 * sradius;
			else
				sradius = 10.0e6;
			double width = asample.getEquatorialDimensionD();
			if (width <= 0.0)
				width = 10.0e6;
			width = Math.min(width, sradius);
			double height = asample.getAxialDimensionD();
			if (height <= 0.0)
				height = 10.0e6;
			double thick = asample.getThicknessDimensionD();
			if (thick <= 0.0)
				thick = 10.0e6;

//		double[] sampleAngles = asample.getSampleAngles();
				double[] angles = adatafile.getTiltingAngle();

//		double[][] angles = getIncidentAndDiffractionAngles(adatafile, tilting_angles, sampleAngles, position);
				// displacement

//		double sintheta = MoreMath.sind(position);
//		double costheta = MoreMath.cosd(position);

//				double commonFactor = 90.0 / (Math.PI * radius);

				double[] xyzg = new double[3];
			DataFileSet adataset = adatafile.getDataFileSet();
			xyzg[0] = adataset.getXshift();
			xyzg[1] = adataset.getYshift();
			xyzg[2] = adataset.getZshift();
			double[] dim = new double[3];
			dim[0] = width;
			dim[1] = height;
			dim[2] = thick;

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

			double xp = xyzg[0];
				double yp = xyzg[1];
				double zp = xyzg[2];

			x1 = dim[0] * cosphi + dim[1] * sinphi;
			dim[1] = -dim[0] * sinphi + dim[1] * cosphi;
			dim[0] = x1;
			x1 = dim[1] * coschi + dim[2] * sinchi;
			dim[2] = -dim[1] * sinchi + dim[2] * coschi;
			dim[1] = x1;
			x1 = dim[0] * cosomega + dim[1] * sinomega;
			dim[1] = -xyzg[0] * sinomega + dim[1] * cosomega;
			dim[0] = x1;
			width = dim[0];
			height = dim[1];

				if (zp < 0)
					zp = -zp;

				beamsizez = radius * MoreMath.sind(slitaperture);

				if (width > 0.0 && beamsizez > 0) {
					delta = 0.5 * (beamsizez - width);
					if (delta < 0.0)
						delta = -delta;
					if (zp > delta)
						lp -= (zp - delta) / width;
					if (lp < 0)
						lp = 0;
				}

			if (yp < 0)
				yp = -yp;

				beamsizey = radius * MoreMath.sind(slitaperturey);

				if (height > 0.0 && beamsizey > 0) {
					delta = 0.5 * (beamsizey - height);
					if (delta < 0.0)
						delta = -delta;
					if (yp > delta)
						lp -= (yp - delta) / height;
					if (lp < 0)
						lp = 0;
				}
			}

//		double delta2thetax = -xp * sintheta * commonFactor;
//		double delta2thetaz = zp * costheta * commonFactor;
// todo : missing eta angle correction using yp1

//		return x + delta2thetax + delta2thetaz;

// not used    double[] tilt_angles = adatafile.getTiltingAngle();

// not used    sintheta = Math.sin((90.0 - tilt_angles[1]) * Constants.DEGTOPI);
// not used    double sinomega = Math.sin((tilt_angles[0]) * Constants.DEGTOPI);

/*    double height = asample.getAxialDimensionD();

    if (height > 0.0 && (slitaperture > 0.0 && radius > 0.0)) {
      beamsize = radius * MoreMath.sind(slitaperture) / sintheta / sinomega;
      if (beamsize > height)
        lp *= height / beamsize;
    }*/

		return lp;
	}

}
