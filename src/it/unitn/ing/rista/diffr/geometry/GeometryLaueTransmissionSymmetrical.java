/*
 * @(#)GeometryLaueTransmissionSymmetrical.java created Jan 30, 2009 Verona-Trento
 *
 * Copyright (c) 2009 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.diffr.geometry;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.MoreMath;

/**
 * The GeometryTransmissionFlatImageNoLorentzNew is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jan 30, 2009 6:20:32 PM $
 * @since JDK1.1
 */
public class GeometryLaueTransmissionSymmetrical extends GeometryDiffractometer {

  static final String id1 = "Symmetrical Laue transmission";
  static final String id2 = "Symmetrical Laue transmission instrument geometry";

  public GeometryLaueTransmissionSymmetrical(XRDcat obj, String alabel) {
    super(obj, alabel);
    identifier = id1;
    IDlabel = id1;
    description = id2;
  }

  public GeometryLaueTransmissionSymmetrical(XRDcat aobj) {
    this(aobj, id1);
  }

  public GeometryLaueTransmissionSymmetrical() {
    identifier = id1;
    IDlabel = id1;
    description = id2;
  }

  public double Lorentz(DiffrDataFile adatafile, double position) {
    // Lorentz - velocity
	  double lp = 0.0;
	  if (position < Constants.PI) {
		  double sin2theta = Math.sin(position);
		  sin2theta *= sin2theta;
		  lp = 1.0 / sin2theta;
	  }
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

  public double getCorrectedPosition(Sample asample, double x, double[] tilting_angles, DiffrDataFile adatafile) {
//    System.out.println("Omega = " + tilting_angles[0]);
//    System.out.println("2theta = " + x);
    double radius = getRadius(null);
    double sintheta = MoreMath.sind(x);
    double costheta = MoreMath.cosd(x);

    double commonFactor = 90.0 / (Math.PI * radius);

    double[] angles = getTrueTiltingAngles(adatafile, tilting_angles, (double) x);

//    double[] xyz = asample.getSpecimenPrecessionError().getXYZForPrecession(angles, x);

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

	  double xp = xyzg[0];
    double yp1 = xyzg[1];
    double zp1 = xyzg[2];

    double delta2thetax = -xp * sintheta * commonFactor;
    double delta2thetaz = zp1 * costheta * commonFactor;

    return x + delta2thetax + delta2thetaz;
  }

}
