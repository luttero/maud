/*
 * @(#)GeometryBBReflectivity.java created 16/08/2000 Mesiano.
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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
import it.unitn.ing.rista.util.*;

/**
 *  The GeometryBBReflectivity is a class
 *
 *
 * @version $Revision: 1.7 $, $Date: 2005/09/07 17:14:07 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GeometryBBReflectivity extends GeometryDiffractometer {

  public GeometryBBReflectivity(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "BB reflectivity";
    IDlabel = "BB reflectivity";
    description = "Bragg Brentano reflectivity instrument geometry";
  }

  public GeometryBBReflectivity(XRDcat aobj) {
    this(aobj, "BB reflectivity");
  }

  public GeometryBBReflectivity() {
    identifier = "BB reflectivity";
    IDlabel = "BB reflectivity";
    description = "Bragg-Brentano reflectivity instrument geometry";
  }

  public double[] getTextureAngles(DiffrDataFile datafile, double[] tilting_angles,
                                  double[] sampleAngles, double twotheta) {
    double[] newtilting_angles = new double[4];
    newtilting_angles[0] = tilting_angles[0];
    newtilting_angles[1] = tilting_angles[1];
    newtilting_angles[2] = tilting_angles[2];
    newtilting_angles[3] = tilting_angles[3];

    double theta_detector = tilting_angles[0] * 2; //getThetaDetector(datafile, twotheta);

    return super.getTextureAngles(datafile, newtilting_angles, sampleAngles, theta_detector);
  }

  public double LorentzPolarization(DiffrDataFile adatafile, Sample asample, double position, boolean dspacingbase, boolean energyDispersive) {

    // first correction for absorption on asymmetric diffraction
    double lp = 1.0;

/*    double sintheta, sin2theta, cos2theta, Ph, width, beamsize;

    sintheta = Math.sin(position * Constants.DEGTOPI / 2.0);

    double sradius = asample.getRadiusDimensionD();
    if (sradius > 0.0)
      sradius = 2 * sradius * sintheta;
    else
      sradius = 10.0e6;
    width = asample.getEquatorialDimensionD();
    if (width <= 0.0)
      width = 10.0e6;

    width = Math.min(width, sradius);

    if (width > 0.0 && (slitaperture > 0.0 && radius > 0.0)) {
      beamsize = radius * MoreMath.sind(slitaperture) / sintheta;
      if (beamsize > width)
        lp *= width / beamsize;
    }

    double[] tilt_angles = adatafile.getTiltingAngle();

    sintheta = Math.sin((90.0 - tilt_angles[1]) * Constants.DEGTOPI);

    double height = asample.getAxialDimensionD();

    if (height > 0.0 && (slitaperture > 0.0 && radius > 0.0)) {
      beamsize = radius * MoreMath.sind(slitaperture) / sintheta;
      if (beamsize > height)
        lp *= height / beamsize;
    }*/


    return lp;
  }

  public double getBeamRelatedCorrection(DiffrDataFile adatafile, Sample asample, double position, int pointIndex) {
    // first correction for absorption on asymmetric diffraction
    double lp = 1.0, beamsize;

    double sintheta = Math.sin(position * Constants.DEGTOPI / 2.0);

    double sradius = asample.getRadiusDimensionXD();
    if (sradius > 0.0)
      sradius = 2 * sradius * sintheta;
    else
      sradius = 10.0e6;
    double width = asample.getEquatorialDimensionD();
    if (width <= 0.0)
      width = 10.0e6;

    width = Math.min(width, sradius);

    if (width > 0.0 && (slitaperture > 0.0 && radius > 0.0)) {
      beamsize = radius * MoreMath.sind(slitaperture) / sintheta;
      if (beamsize > width)
        lp *= width / beamsize;
    }

/*    double[] tilt_angles = adatafile.getTiltingAngle();

    sintheta = Math.sin((90.0 - tilt_angles[1]) * Constants.DEGTOPI);

    double height = asample.getAxialDimensionD();

    if (height > 0.0 && (slitaperture > 0.0 && radius > 0.0)) {
      beamsize = radius * MoreMath.sind(slitaperture) / sintheta;
      if (beamsize > height)
        lp *= height / beamsize;
    }*/

    return lp;
  }

}
