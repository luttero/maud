/*
 * @(#)GeometryDubnaSkat.java created 12/04/1999 Firenze
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

import java.lang.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

import java.awt.*;
import javax.swing.*;

/**
 *  The GeometryDubnaSkat is a class
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:07 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GeometryDubnaSkat extends GeometryDebyeScherrer {

  public GeometryDubnaSkat(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "Dubna/SKAT TOF";
    IDlabel = "Dubna/SKAT TOF";
    description = "Dubna/SKAT TOF instrument geometry";
  }

  public GeometryDubnaSkat(XRDcat aobj) {
    this(aobj, "Dubna/SKAT TOF");
  }

  public GeometryDubnaSkat() {
    identifier = "Dubna/SKAT TOF";
    IDlabel = "Dubna/SKAT TOF";
    description = "Dubna/SKAT TOF instrument geometry";
  }

  public double getThetaDetector(double twotheta) {
    return getDetector().getThetaDetector(null, twotheta);
  }

  public double[] getTextureAngles(double[] tilting_angles, double twotheta) {

    // tilting_angles[0] = Omega not used
    // tilting_angles[1] = Chi
    // tilting_angles[2] = Phi
    // tilting_angles[3] = Eta

    double theta_detector = getThetaDetector(twotheta);

    double polar_angles[] = new double[2];

/*  not correct, substituded from the next one

		double thetai = 360 - 2 * theta_detector;
		double xi = 0.5 * (1.0 + MoreMath.cosd(thetai));
		double yi = 0.5 * MoreMath.sind(thetai);

		if (xi != 0.0) {
			polar_angles[1] = Math.atan(yi / xi);
			if (xi < 0.0)
				polar_angles[1] += Constants.PI;
			double cosbeta = Math.cos(polar_angles[1]);
			polar_angles[1] *= Constants.PITODEG;
			polar_angles[0] = Math.asin(1.0 - xi / cosbeta) * Constants.PITODEG;
		} else {
			polar_angles[1] = 270.0;
			polar_angles[0] = 0.0;
		}*/

    // See K. Ullmeyer et al., Nuclear Instr. Meth. Phys. Research, A 412 (1998) 80-88.

    double singamma2 = MoreMath.sind(theta_detector / 2.0);
    singamma2 *= singamma2;

    polar_angles[1] = (double) (2.0 * Math.atan(Math.sqrt((1.0 - singamma2) / (1.0 + singamma2))));
    singamma2 = Math.cos(polar_angles[1]);
    polar_angles[1] *= Constants.PITODEG;
    if (singamma2 < 1.0)
      polar_angles[0] = (double) (Math.atan(Math.sqrt(2.0 * singamma2 / (1.0 - singamma2)))
              * Constants.PITODEG);
    else
      polar_angles[0] = 90.0f;

    polar_angles[1] = tilting_angles[2] - polar_angles[1];

    while (polar_angles[1] < 0.0)
      polar_angles[1] += 360.0f;
    while (polar_angles[1] > 360.0)
      polar_angles[1] -= 360.0f;
    while (polar_angles[0] > 90.0) {
      polar_angles[0] -= 90.0f;
      polar_angles[1] -= 180.0f;
      while (polar_angles[1] < 0.0)
        polar_angles[1] += 360.0f;
    }

    return polar_angles;
  }

  public double LorentzPolarization(DiffrDataFile adatafile, Sample asample, double position,
                                    boolean dspacingbase, boolean energyDispersive) {

    double detector_efficiency = getDetector().getIntensityCalibration(adatafile, asample,
		    position, dspacingbase, energyDispersive);

    return position * position * position * position * detector_efficiency;
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
