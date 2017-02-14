/*
 * @(#)GeometryLANSCE_GSAS.java created 13/08/1999 London-Heathrow
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

import java.awt.*;
import javax.swing.*;
import javax.swing.event.*;

/**
 *  The GeometryLANSCE_GSAS is a class
 *
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GeometryLANSCE_GSAS extends GeometryIPNS_LANSCE {

  public GeometryLANSCE_GSAS(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "LANSCE/GSAS TOF";
    IDlabel = "LANSCE/GSAS TOF";
    description = "LANSCE TOF instrument geometry with GSAS angular convention";
  }

  public GeometryLANSCE_GSAS(XRDcat aobj) {
    this(aobj, "LANSCE/GSAS TOF");
  }

  public GeometryLANSCE_GSAS() {
    identifier = "LANSCE/GSAS TOF";
    IDlabel = "LANSCE/GSAS TOF";
    description = "LANSCE TOF instrument geometry with GSAS angular convention";
  }


  public double[] getTextureAngles(double tilting_angles0,
                                  double tilting_angles1,
                                  double tilting_angles2,
                                  double tilting_angles3,
                                  double thetaAng,
                                  double[] sampleAngles, boolean planeProjection) {

    // tilting_angles0 = Omega - theta
    // tilting_angles1 = Chi
    // tilting_angles2 = Phi
    // tilting_angles3 = Eta

//		System.out.println(tilting_angles0);

    double sinOmegaSample = MoreMath.sind(sampleAngles[0]);
    double cosOmegaSample = MoreMath.cosd(sampleAngles[0]);
//		System.out.println(theta_detector);
    double sinOmega = MoreMath.sind(tilting_angles0);
    double cosOmega = MoreMath.cosd(tilting_angles0);
    tilting_angles2 += sampleAngles[2];
    double sinPhi = MoreMath.sind(tilting_angles2);
    double cosPhi = MoreMath.cosd(tilting_angles2);
    double sinCsiSample = MoreMath.sind(sampleAngles[1]);
    double cosCsiSample = MoreMath.cosd(sampleAngles[1]);
    double sinCsi = MoreMath.sind(tilting_angles1);
    double cosCsi = MoreMath.cosd(tilting_angles1);
    double sinEta = MoreMath.sind(tilting_angles3);
    double cosEta = MoreMath.cosd(tilting_angles3);
    double sinTheta = MoreMath.sind(thetaAng);
    double cosTheta = MoreMath.cosd(thetaAng);

    double[][] omega = {{1.0, 0.0, 0.0},
                        {0.0, cosOmega, sinOmega},
                        {0.0, -sinOmega, cosOmega}
    };
    double[][] omegaS = {{1.0, 0.0, 0.0},
                         {0.0, cosOmegaSample, sinOmegaSample},
                         {0.0, -sinOmegaSample, cosOmegaSample}
    };
    double[][] chi = {{cosCsi, 0.0, -sinCsi},
                      {0.0, 1.0, 0.0},
                      {sinCsi, 0.0, cosCsi}
    };
    double[][] chiS = {{cosCsiSample, 0.0, -sinCsiSample},
                       {0.0, 1.0, 0.0},
                       {sinCsiSample, 0.0, cosCsiSample}
    };
    double[][] phi = {{1.0, 0.0, 0.0},
                      {0.0, cosPhi, sinPhi},
                      {0.0, -sinPhi, cosPhi}
    };
    double[][] eta = {{cosEta, 0.0, -sinEta},
                      {0.0, 1.0, 0.0},
                      {sinEta, 0.0, cosEta}
    };
    double[][] theta = {{1.0, 0.0, 0.0},
                        {0.0, cosTheta, -sinTheta},
                        {0.0, sinTheta, cosTheta}
    };

    double[][] res = MoreMath.MatProduct(omegaS, chiS, 3, 3, 3);
    res = MoreMath.MatProduct(res, phi, 3, 3, 3);
    res = MoreMath.MatProduct(res, chi, 3, 3, 3);
    res = MoreMath.MatProduct(res, omega, 3, 3, 3);
//    res = MoreMath.MatProduct(res,eta,3,3,3);
    res = MoreMath.MatProduct(res, theta, 3, 3, 3);
/*    double[][] res = MoreMath.MatProduct(theta,eta,3,3,3);
    res = MoreMath.MatProduct(res,omega,3,3,3);
    res = MoreMath.MatProduct(res,chi,3,3,3);
    res = MoreMath.MatProduct(res,phi,3,3,3);
    res = MoreMath.MatProduct(res,chiS,3,3,3);
    res = MoreMath.MatProduct(res,omegaS,3,3,3);*/

    double M13 = res[0][2];
    double M23 = res[1][2];
    double cosPsi = res[2][2];

    double[] textureAngles = new double[2];

    if (Math.abs(M23) < 1.0E-9)
      M23 = 0.0;
    if (Math.abs(M13) < 1.0E-9) {
      if (M23 >= 0.0)
        textureAngles[1] = 90.0f;
      else
        textureAngles[1] = -90.0f;
    } else if (M23 == 0.0) {
      if (M13 > 0.0)
        textureAngles[1] = 180.0f;
      else
        textureAngles[1] = 0.0f;
    } else {
      textureAngles[1] = -(double) MoreMath.atand(M23 / M13);
      if (M13 > 0.0)
        textureAngles[1] += 180.0f;
    }

    textureAngles[0] = (double) MoreMath.acosd(cosPsi);

/*    if (textureAngles[0] < 0.0) {
      textureAngles[0] = -textureAngles[0];
      textureAngles[1] += 180.0f;
    } */
    if (planeProjection && textureAngles[0] > 90.0) {
      textureAngles[0] = 180.0f - textureAngles[0];
      textureAngles[1] += 180.0f;
    }
    while (textureAngles[1] > 360.0)
      textureAngles[1] -= 360.0f;
    while (textureAngles[1] < 0.0)
      textureAngles[1] += 360.0f;

    return textureAngles;
  }

  public double[] getTextureAngles(DiffrDataFile datafile, double[] tilting_angles,
                                  double[] sampleAngles, double twotheta) {
//		double[] newtilting_angles = new double[4];
    double[] addSampleAngles = datafile.getDataFileSet().getAdditionalSampleAngles();
    double[] newSampleAngles = new double[3];
    for (int i = 0; i < 3; i++)
      newSampleAngles[i] = sampleAngles[i] + addSampleAngles[i];
    double tilting_angles0 = tilting_angles[0];
    double tilting_angles1 = tilting_angles[1];
    double tilting_angles2 = tilting_angles[2];
    double tilting_angles3 = tilting_angles[3] + getEtaDetector(datafile);

    double theta_detector = getThetaDetector(datafile, twotheta);

    double newTwoTheta = theta_detector; //180.0 - theta_detector;
//		double[] tiltAngles = getTextureAngles(datafile, newtilting_angles, sampleAngles, newTwoTheta);
//    tiltAngles[1] = 360.0f - tiltAngles[1];
//    return tiltAngles;
    return getTextureAngles(tilting_angles0, tilting_angles1, tilting_angles2, tilting_angles3,
            newTwoTheta / 2.0f, newSampleAngles, true);
  }

  public double[] getTextureAngles(double[] tilting_angles, double twotheta) {

    double[] newtilting_angles = new double[4];
    newtilting_angles[0] = tilting_angles[0];
    newtilting_angles[1] = tilting_angles[1];
    newtilting_angles[2] = tilting_angles[2];
    newtilting_angles[3] = tilting_angles[3];
    double[] tiltAngles = super.getTextureAngles(newtilting_angles, twotheta);
    tiltAngles[1] = 360.0f - tiltAngles[1];
    return tiltAngles;

  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JGeometryLGOptionsD(parent, this);
    return adialog;
  }

  public class JGeometryLGOptionsD extends JOptionsDialog {

    public JGeometryLGOptionsD(Frame parent, XRDcat obj) {

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
