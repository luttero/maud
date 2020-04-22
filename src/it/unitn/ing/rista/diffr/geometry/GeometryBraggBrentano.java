/*
 * @(#)GeometryBraggBrentano.java created 06/01/1999 Pergine Vals.
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
import it.unitn.ing.rista.util.*;

/**
 *  The GeometryBraggBrentano is a class
 *
 *
 * @version $Revision: 1.11 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GeometryBraggBrentano extends GeometryDiffractometer {

  public GeometryBraggBrentano(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "Bragg-Brentano";
    IDlabel = "Bragg-Brentano";
    description = "Bragg-Brentano instrument geometry";
  }

  public GeometryBraggBrentano(XRDcat aobj) {
    this(aobj, "Bragg-Brentano");
  }

  public GeometryBraggBrentano() {
    identifier = "Bragg-Brentano";
    IDlabel = "Bragg-Brentano";
    description = "Bragg-Brentano instrument geometry";
  }

  public double[] getTextureAngles(DiffrDataFile datafile, double[] tilting_angles,
                                  double[] sampleAngles, double twotheta) {

    double[] newtilting_angles = new double[4];
    newtilting_angles[0] = getMeasurement().getOmega(tilting_angles[0], twotheta);
    newtilting_angles[1] = tilting_angles[1];
    newtilting_angles[2] = tilting_angles[2];
    newtilting_angles[3] = tilting_angles[3];

    double theta_detector = getThetaDetector(datafile, twotheta);

    return super.getTextureAngles(datafile, newtilting_angles, sampleAngles, theta_detector);
  }

  boolean warningAlreadyPrinted = false;

  public double LorentzPolarization(DiffrDataFile adatafile, Sample asample, double position, boolean dspacingbase,
                                    boolean energyDispersive) {

    double sin2theta;
    double lp = 1.0;
    position *= Constants.DEGTOPI;
	  double positionHalf = position * 0.5;
    if (getAutomaticSlit()) {
      if (!dspacingbase && !energyDispersive)
        lp *= Math.sin(positionHalf);
      else if (!warningAlreadyPrinted) {
        System.out.println("Warning: programmable slits option not selectable with non-2theta based data");
        warningAlreadyPrinted = true;
      }
    }

    double[] tilt_angles = adatafile.getTiltingAngle();
    sin2theta = Math.sin(position);
    lp *= polarization(adatafile, positionHalf) / (Math.sin(positionHalf) * sin2theta);
    double omega = getMeasurement().getOmega(tilt_angles[0], position) * Constants.DEGTOPI;
    if (omega >= position || position >= Constants.PI)
      return 0.0;

    return lp;
  }

  public double getBeamRelatedCorrection(DiffrDataFile adatafile, Sample asample, double position, int pointIndex) {

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

  public double getPathK(double position, boolean dspacingbase, double[] tiltingAngles) {
    return getMeasurement().getPathK(position, dspacingbase, tiltingAngles);
  }

/*  public double getLayerAbsorption_new(Sample asample, RadiationType rad, int layerIndex, double[] incidentDiffractionAngles,
                                       DataFileSet adataset) {
    return asample.getLayerAbsorption_new(rad, layerIndex, incidentDiffractionAngles, adataset);
  }*/

	public double[] getLayerAbsorption_new(Sample asample, RadiationType rad, int layerIndex, double[][] incidentDiffractionAngles,
	                                       DataFileSet adataset) {
		return asample.getLayerAbsorption_new(rad, layerIndex, incidentDiffractionAngles, adataset);
	}

}
