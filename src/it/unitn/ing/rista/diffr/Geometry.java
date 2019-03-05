/*
 * @(#)Geometry.java created 06/01/1999 Pergine Vals.
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

package it.unitn.ing.rista.diffr;

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.diffr.cal.AngularCalibration;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;


/**
 *  The Geometry is a class to performs computation specific for the geometry of a
 *  goniometer. Different goniometer geometries should overwrite and implements the methods
 *  of this class. This class is never instantiated. It is mere like an interface object
 *  to specify and implements the basic methods and funtionalities.
 *
 *
 * @version $Revision: 1.13 $, $Date: 2006/11/10 09:33:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class Geometry extends XRDcat {

	public static double pi_over2 = Math.PI * 0.5;
	public static double degtopi2 = Constants.DEGTOPI / 2.0;

	public double radius = 175.0; // in mm, the default goniometer radius

  public Geometry(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public Geometry(XRDcat aobj) {
    this(aobj, "Geometry x");
  }

  public Geometry() {
  }

  /**
   * Return the AngularCalibration object. Convenient method. It asks to the instrument (the
   * parent) to return the actual AngularCalibration.
   */

  public AngularCalibration getAngularCalibration() {
    return ((Instrument) getParent()).getAngularCalibration();
  }

  /**
   * Return the Detector object. Convenient method. It asks to the instrument (the
   * parent) to return the actual Detector.
   */

  public Detector getDetector() {
    return ((Instrument) getParent()).getDetector();
  }

  /**
   * Return the theta value of the detector position. Convenient method.
   * @param datafile  the spectrum for which the position is required
   * @param position  the peak or point position in the spectrum.
   * For example for TOF, the theta position of the detector is returned.
   */

  public double getThetaDetector(DiffrDataFile datafile, double position) {
    return getDetector().getThetaDetector(datafile, position);
  }

  /**
   * Return the eta value of the detector position. Convenient method.
   * @param datafile  the spectrum for which the position is required
   * For example for TOF, the eta position of the detector is returned.
   */

  public double getEtaDetector(DiffrDataFile datafile) {
    return getDetector().getEtaDetector(datafile);
  }

  /**
   * Return the Measurement object. Convenient method. It asks to the instrument (the
   * parent) to return the actual Measurement.
   */

  public Measurement getMeasurement() {
    return ((Instrument) getParent()).getMeasurement();
  }

  /**
   * Return the goniometer radius. Actually this return just the default radius. Subclasses
   * must override this method to return the radius value obtained from the list of parameters
   * or from a field object. This class is never instantiated, so doesn't provide parameters
   * or others fields.
   * @param adatafile
   */

  public double getRadius(DiffrDataFile adatafile) {
    return radius;
  }

  /**
   * Return an array of two floats containing the azimuthal and polar angles in pole
   * figure coordinates for texture analysis. The angles are computed for a specific
   * position of a peak in a spectrum collected at a certain orientation and the sample
   * positioned at a specific orientation. The following is the general method. Subclasses
   * of Geometry that need a specific different way to compute the pole figure angles need
   * to overwrite this method.
   * @param datafile  the spectrum or datafile for which the angles should be computed
   * @param tilting_angles  array containing omega, chi, phi and eta for the spectrum in
   *                        the insturment coordinates
   * @param sampleAngles  array containing omega, chi and phi angular position of the sample
   * @param twotheta  the 2-theta or d-spacing (if the spectrum is in d-spacing) of the
   * peak for which the angles should be computed.
   */

  public double[] getTextureAngles(DiffrDataFile datafile, double[] tilting_angles,
                                  double[] sampleAngles, double twotheta) {

    double tilting_angles0 = tilting_angles[0];
    double tilting_angles1 = tilting_angles[1];
    double tilting_angles2 = tilting_angles[2];
    double tilting_angles3 = tilting_angles[3] + getEtaDetector(datafile);

    double[] addSampleAngles = datafile.getDataFileSet().getAdditionalSampleAngles();
    double[] newSampleAngles = new double[3];
    for (int i = 0; i < 3; i++)
      newSampleAngles[i] = sampleAngles[i] + addSampleAngles[i];

    return getTextureAngles(tilting_angles0, tilting_angles1, tilting_angles2, tilting_angles3,
            twotheta / 2.0f, newSampleAngles, true);

  }

  public double[][] getTextureAngles(DiffrDataFile datafile, double[] tilting_angles,
                                  double[] sampleAngles, double[] twotheta) {

    double tilting_angles0 = tilting_angles[0];
    double tilting_angles1 = tilting_angles[1];
    double tilting_angles2 = tilting_angles[2];
    double tilting_angles3 = tilting_angles[3] + getEtaDetector(datafile);

    double[] addSampleAngles = datafile.getDataFileSet().getAdditionalSampleAngles();
    double[] newSampleAngles = new double[3];
    for (int i = 0; i < 3; i++)
      newSampleAngles[i] = sampleAngles[i] + addSampleAngles[i];

    return getTextureAngles(tilting_angles0, tilting_angles1, tilting_angles2, tilting_angles3,
            twotheta, newSampleAngles, true);

  }

  public double[] getAlternateTextureAngles(DiffrDataFile datafile, double[] tilting_angles,
                                  double[] sampleAngles, double twotheta) {

    // to check D19 image angle coverage by Daniel routines

    double[] textureAngles = new double[2];

    double tilting_angles0 = tilting_angles[0];
    double tilting_angles1 = tilting_angles[1];
    double tilting_angles2 = tilting_angles[2];
    double tilting_angles3 = tilting_angles[3];
    double rd = getRadius(datafile);
    double y = Angles.getY(tilting_angles3, rd, twotheta);

    double nu = MoreMath.atand(y / rd);
    double cosnu = MoreMath.cosd(nu);
    double sinnu = MoreMath.sind(nu);
    double cosk = 1.0;
    if (Math.abs(cosnu) > 1.0E-9)
      cosk = MoreMath.cosd(twotheta) / cosnu;
    double sink = Math.sqrt(1.0 - cosk * cosk);

    double costhetayprime = sinnu / (2.0 * MoreMath.sind(twotheta / 2.0));
    double thetayprime = MoreMath.acosd(costhetayprime);
    double cosphiyprime = cosnu * sink / (2.0 * MoreMath.sind(twotheta / 2.0) * MoreMath.sind(thetayprime));
    double phiyprime = MoreMath.acosd(cosphiyprime);

    double costhetay = MoreMath.cosd(90.0 - thetayprime + tilting_angles1) * MoreMath.cosd(phiyprime - tilting_angles0);
    double thetay = MoreMath.acosd(costhetay);
    double sinphiy = 0.0;
    if (Math.abs(costhetay) < 0.999999999)
      sinphiy = MoreMath.sind(90.0 - thetayprime + tilting_angles1) / MoreMath.sind(thetay);
    double phiy = MoreMath.asind(sinphiy);

    textureAngles[0] = (double) thetay;
    textureAngles[1] = (double) phiy;

    return textureAngles;

  }

  /**
   * Return an array of floats with the angles in the Instrument convention as
   * omega, chi, phi, and eta. Non transformed in texture coordinates.
   * @param datafile  the relative spectrum
   * @param tilting_angles  the angles at which the spectrum was collected in Instrument convention.
   * @param position  the point or peak position in the spectrum (d-space or 2theta coordinates).
   */

  public double[] getTrueTiltingAngles(DiffrDataFile datafile,
                                      double[] tilting_angles, double position) {
    double[] trueTiltingAngles = new double[4];
    trueTiltingAngles[0] = tilting_angles[0];
    trueTiltingAngles[1] = tilting_angles[1];
    trueTiltingAngles[2] = tilting_angles[2];
    trueTiltingAngles[3] = tilting_angles[3] + getEtaDetector(datafile);
    return trueTiltingAngles;
  }

  /**
   * Return the correction for path absorption in layered system. Should be rewrite to
   * be consistent with the new program structure. Deprecated, please don't use it.
   * @param position  the point or peak position in the spectrum (d-space or 2theta coordinates).
   * @param dspacingbase  true if position in d-spacing, false if in 2theta
   * @param tilting_angles  the angles at which the spectrum was collected in Instrument convention.
   */

  public double getPathK(double position, boolean dspacingbase, double[] tilting_angles) {
    return 0.0f;
  }

  /**
   * Return an array of two floats containing the azimuthal and polar angles in pole
   * figure coordinates for texture analysis.
   * The method performs the angles transformation and it is called by the omonymous
   * method.
   * @param tilting_angles0  Omega angle of the spectrum in instrument coordinates
   * @param tilting_angles1  Chi angle of the spectrum
   * @param tilting_angles2  Phi angle of the spectrum
   * @param tilting_angles3  eta angle of the spectrum
   * @param thetaAng  position in the theta circle, 0 if in d-spacing
   * @param sampleAngles  array containing omega, chi and phi angular position of the sample
   */

  public double[] getTextureAngles(double tilting_angles0,
                                  double tilting_angles1,
                                  double tilting_angles2,
                                  double tilting_angles3,
                                  double thetaAng,
                                  double[] sampleAngles, boolean planeProjection) {

    // tilting_angles0 = Omega
    // tilting_angles1 = Chi
    // tilting_angles2 = Phi
    // tilting_angles3 = Eta

    double sinOmegaSample = MoreMath.sind(sampleAngles[0]);
    double cosOmegaSample = MoreMath.cosd(sampleAngles[0]);
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

    double const1 = cosOmegaSample * sinCsiSample * cosPhi + sinOmegaSample * sinPhi;
    double const2 = -const1 * sinCsi + cosOmegaSample * cosCsiSample * cosCsi;
    double const3 = cosOmegaSample * sinCsiSample * sinPhi - sinOmegaSample * cosPhi;
    double const4 = const1 * cosCsi + cosOmegaSample * cosCsiSample * sinCsi;
    double const5 = sinOmegaSample * sinCsiSample * cosPhi - cosOmegaSample * sinPhi;
    double const6 = -const5 * sinCsi + sinOmegaSample * cosCsiSample * cosCsi;
    double const7 = sinOmegaSample * sinCsiSample * sinPhi + cosOmegaSample * cosPhi;
    double const8 = const5 * cosCsi + sinOmegaSample * cosCsiSample * sinCsi;
    double const9 = -cosCsiSample * cosPhi * sinCsi - sinCsiSample * cosCsi;
    double const10 = cosCsiSample * sinPhi * sinOmega + const9 * cosOmega;
    double const11 = cosCsiSample * cosPhi * cosCsi - sinCsiSample * sinCsi;

    double M13 = -(cosCsiSample * sinPhi * cosOmega - const9 * sinOmega) * sinTheta +
            (const11 * sinEta + const10 * cosEta) * cosTheta; // res[0][2];
    double M23 = -(const7 * cosOmega - const6 * sinOmega) * sinTheta + (const8 * sinEta +
            (const7 * sinOmega + const6 * cosOmega) * cosEta) * cosTheta; // res[1][2];
    double cosPsi = -(const3 * cosOmega - const2 * sinOmega) * sinTheta + (const4 * sinEta +
            (const3 * sinOmega + const2 * cosOmega) * cosEta) * cosTheta; // res[2][2];

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
      textureAngles[1] = -MoreMath.atand(M23 / M13);
      if (M13 > 0.0)
        textureAngles[1] += 180.0f;
    }
//    System.out.println("1: " + cosPsi);
    if (cosPsi > 0.99999999)
      textureAngles[0] = 0f;
    else if (cosPsi < -0.99999999)
      textureAngles[0] = 180f;
    else
      textureAngles[0] = MoreMath.acosd(cosPsi);
//    System.out.println("2: " + textureAngles[0]);

    /*if (textureAngles[0] < 0.0) {  // should never happen
      textureAngles[0] = -textureAngles[0];
      textureAngles[1] += 180.0f;
    }
    */
    if (planeProjection && textureAngles[0] > 90.0) {        // only for texture
      textureAngles[0] = 180.0f - textureAngles[0];
      textureAngles[1] += 180.0f;
    }
    if (textureAngles[1] > 360.0)
      textureAngles[1] -= 360.0f;
    if (textureAngles[1] < 0.0)
      textureAngles[1] += 360.0f;

    return textureAngles;
  }

  public double[][] getTextureAngles(double tilting_angles0,
                                   double tilting_angles1,
                                   double tilting_angles2,
                                   double tilting_angles3,
                                   double[] thetaAng,
                                   double[] sampleAngles, boolean planeProjection) {

     // tilting_angles0 = Omega
     // tilting_angles1 = Chi
     // tilting_angles2 = Phi
     // tilting_angles3 = Eta

     double sinOmegaSample = MoreMath.sind(sampleAngles[0]);
     double cosOmegaSample = MoreMath.cosd(sampleAngles[0]);
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
     double const1 = cosOmegaSample * sinCsiSample * cosPhi + sinOmegaSample * sinPhi;
     double const2 = -const1 * sinCsi + cosOmegaSample * cosCsiSample * cosCsi;
     double const3 = cosOmegaSample * sinCsiSample * sinPhi - sinOmegaSample * cosPhi;
     double const4 = const1 * cosCsi + cosOmegaSample * cosCsiSample * sinCsi;
     double const5 = sinOmegaSample * sinCsiSample * cosPhi - cosOmegaSample * sinPhi;
     double const6 = -const5 * sinCsi + sinOmegaSample * cosCsiSample * cosCsi;
     double const7 = sinOmegaSample * sinCsiSample * sinPhi + cosOmegaSample * cosPhi;
     double const8 = const5 * cosCsi + sinOmegaSample * cosCsiSample * sinCsi;
     double const9 = -cosCsiSample * cosPhi * sinCsi - sinCsiSample * cosCsi;
     double const10 = cosCsiSample * sinPhi * sinOmega + const9 * cosOmega;
     double const11 = cosCsiSample * cosPhi * cosCsi - sinCsiSample * sinCsi;

    double[][] textureAngles = new double[2][thetaAng.length];

    for (int i = 0; i < thetaAng.length; i++) {

      double theta = thetaAng[i] / 2.0;
    double sinTheta = MoreMath.sind(theta);
    double cosTheta = MoreMath.cosd(theta);

     double M13 = -(cosCsiSample * sinPhi * cosOmega - const9 * sinOmega) * sinTheta +
             (const11 * sinEta + const10 * cosEta) * cosTheta; // res[0][2];
     double M23 = -(const7 * cosOmega - const6 * sinOmega) * sinTheta + (const8 * sinEta +
             (const7 * sinOmega + const6 * cosOmega) * cosEta) * cosTheta; // res[1][2];
     double cosPsi = -(const3 * cosOmega - const2 * sinOmega) * sinTheta + (const4 * sinEta +
             (const3 * sinOmega + const2 * cosOmega) * cosEta) * cosTheta; // res[2][2];

     if (Math.abs(M23) < 1.0E-9)
       M23 = 0.0;
     if (Math.abs(M13) < 1.0E-9) {
       if (M23 >= 0.0)
         textureAngles[1][i] = 90.0f;
       else
         textureAngles[1][i] = -90.0f;
     } else if (M23 == 0.0) {
       if (M13 > 0.0)
         textureAngles[1][i] = 180.0f;
       else
         textureAngles[1][i] = 0.0f;
     } else {
       textureAngles[1][i] = -MoreMath.atand(M23 / M13);
       if (M13 > 0.0)
         textureAngles[1][i] += 180.0f;
     }
//    System.out.println("1: " + cosPsi);
     if (cosPsi > 0.99999999)
       textureAngles[0][i] = 0f;
     else if (cosPsi < -0.99999999)
       textureAngles[0][i] = 180f;
     else
       textureAngles[0][i] = MoreMath.acosd(cosPsi);
//    System.out.println("2: " + textureAngles[0]);

     /*if (textureAngles[0] < 0.0) {  // should never happen
       textureAngles[0] = -textureAngles[0];
       textureAngles[1] += 180.0f;
     }
     */
     if (planeProjection && textureAngles[0][i] > 90.0) {        // only for texture
       textureAngles[0][i] = 180.0f - textureAngles[0][i];
       textureAngles[1][i] += 180.0f;
     }
     if (textureAngles[1][i] > 360.0)
       textureAngles[1][i] -= 360.0f;
     if (textureAngles[1][i] < 0.0)
       textureAngles[1][i] += 360.0f;
    }

     return textureAngles;
   }

  /**
   * Return an array of two floats containing the azimuthal and polar angles in pole
   * figure coordinates for texture analysis.
   * The method performs the angles transformation and it is called by the omonymous
   * method.
   * @param tilting_angles0  Omega angle of the spectrum in instrument coordinates
   * @param tilting_angles1  Chi angle of the spectrum
   * @param tilting_angles2  Phi angle of the spectrum
   * @param tilting_angles3  omega angle of the spectrum
   * @param thetaAng  position in the theta circle, 0 if in d-spacing
   * @param sampleAngles  array containing omega, chi and phi angular position of the sample
   */

  public double[] getTextureAnglesR(double tilting_angles0,
                                   double tilting_angles1,
                                   double tilting_angles2,
                                   double tilting_angles3,
                                   double thetaAng,
                                   double[] sampleAngles, boolean planeProjection) {

    // tilting_angles0 = Omega
    // tilting_angles1 = Chi
    // tilting_angles2 = Phi
    // tilting_angles3 = Eta

    double sinOmegaSample = MoreMath.sind(sampleAngles[0]);
    double cosOmegaSample = MoreMath.cosd(sampleAngles[0]);
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

    double const1 = cosOmegaSample * sinCsiSample * cosPhi + sinOmegaSample * sinPhi;
    double const2 = -const1 * sinCsi + cosOmegaSample * cosCsiSample * cosCsi;
    double const3 = cosOmegaSample * sinCsiSample * sinPhi - sinOmegaSample * cosPhi;
    double const4 = const1 * cosCsi + cosOmegaSample * cosCsiSample * sinCsi;
    double const5 = sinOmegaSample * sinCsiSample * cosPhi - cosOmegaSample * sinPhi;
    double const6 = -const5 * sinCsi + sinOmegaSample * cosCsiSample * cosCsi;
    double const7 = sinOmegaSample * sinCsiSample * sinPhi + cosOmegaSample * cosPhi;
    double const8 = const5 * cosCsi + sinOmegaSample * cosCsiSample * sinCsi;
    double const9 = -cosCsiSample * cosPhi * sinCsi - sinCsiSample * cosCsi;
    double const10 = cosCsiSample * sinPhi * sinOmega + const9 * cosOmega;
    double const11 = cosCsiSample * cosPhi * cosCsi - sinCsiSample * sinCsi;

    double M13 = -(cosCsiSample * sinPhi * cosOmega - const9 * sinOmega) * sinTheta +
            (const11 * sinEta + const10 * cosEta) * cosTheta; // res[0][2];
    double M23 = -(const7 * cosOmega - const6 * sinOmega) * sinTheta + (const8 * sinEta +
            (const7 * sinOmega + const6 * cosOmega) * cosEta) * cosTheta; // res[1][2];
    double cosPsi = -(const3 * cosOmega - const2 * sinOmega) * sinTheta + (const4 * sinEta +
            (const3 * sinOmega + const2 * cosOmega) * cosEta) * cosTheta; // res[2][2];

    double[] textureAngles = new double[2];

    if (Math.abs(M23) < 1.0E-9)
      M23 = 0.0;
    if (Math.abs(M13) < 1.0E-9) {
      if (M23 >= 0.0)
        textureAngles[1] = pi_over2;
      else
        textureAngles[1] = -pi_over2;
    } else if (M23 == 0.0) {
      if (M13 > 0.0)
        textureAngles[1] = Math.PI;
      else
        textureAngles[1] = 0.0f;
    } else {
      textureAngles[1] = -Math.atan(M23 / M13);
      if (M13 > 0.0)
        textureAngles[1] += Math.PI;
    }

    textureAngles[0] = Math.acos(cosPsi);

    if (planeProjection && textureAngles[0] > pi_over2) {        // only for texture
      textureAngles[0] = Math.PI - textureAngles[0];
      textureAngles[1] += Math.PI;
    }
    if (textureAngles[1] > Constants.PI2)
      textureAngles[1] -= Constants.PI2;
    if (textureAngles[1] < 0.0)
      textureAngles[1] += Constants.PI2;

    return textureAngles;
  }

  /**
   * Alternative method to getTextureAngles; to speed up the computation instead of calling
   * the getTextureAngles method for each peak in a datafile, a matrix of angles can be
   * used. The first index refers to which angle (omega, chi, phi and eta) and the second to
   * the spectrum number in the dataset. The evaluation is done for the same peak or reflection
   * in the same dataset (same instrument).
   * So position, either in 2theta or d-space is always the same.
   * @param tilting_angles  matrix for angles, first index refers to the different angles, second
   * index to the spectrum or different point.
   * @param sampleAngles  array containing omega, chi and phi angular position of the sample
   * @param position  the 2-theta or d-spacing (if the spectrum is in d-spacing) of the
   * peak for which the angles should be computed.
   * The angles are putted in the tilting_angles matrix on [0][index]  and  [1][index]  elements
   * for the azimuthal and polar angles.
   */

  public void computeActiveTextureAngles(double[][] tilting_angles, double[] sampleAngles, double position) {
/*    if (Constants.useAltivec)
      JNIAltivec.vComputeTextureAngles(tilting_angles[0],
              tilting_angles[1],
              tilting_angles[2],
              tilting_angles[3],
              sampleAngles, position);
    else if (Constants.nativeComputation)
      JNIAltivec.sComputeTextureAngles(tilting_angles[0],
              tilting_angles[1],
              tilting_angles[2],
              tilting_angles[3],
              sampleAngles, position);
    else   */
      computeTextureAngles(tilting_angles, sampleAngles, position);
  }

  /**
   * Called by computeActiveTextureAngles, this is the pure Java implementation of routine to
   * do the angle conversion.
   * @param tilting_angles  matrix for angles, first index refers to the different angles, second
   * index to the spectrum or different point.
   * @param sampleAngles  array containing omega, chi and phi angular position of the sample
   * @param position  the 2-theta or d-spacing (if the spectrum is in d-spacing) of the
   * peak for which the angles should be computed.
   * The angles are putted in the tilting_angles matrix on [0][index]  and  [1][index]  elements
   * for the azimuthal and polar angles.
   */

  public void computeTextureAngles(double[][] tilting_angles, double[] sampleAngles, double position) {
    // tilting_angles[0][] = Omega
    // tilting_angles[1][] = Chi
    // tilting_angles[2][] = Phi
    // tilting_angles[3][] = Eta

    int numberOfPoint = tilting_angles[0].length;
    position /= 2.0;
    position *= Constants.DEGTOPI;
    for (int i = 0; i < numberOfPoint; i++)
      for (int j = 0; j < 4; j++)
        tilting_angles[j][i] *= Constants.DEGTOPI;
    for (int j = 0; j < 3; j++)
      sampleAngles[j] *= Constants.DEGTOPI;

    double sinOmegaSample = Math.sin(sampleAngles[0]);
    double cosOmegaSample = Math.cos(sampleAngles[0]);
    double sinCsiSample = Math.sin(sampleAngles[1]);
    double cosCsiSample = Math.cos(sampleAngles[1]);
    double sinTheta = Math.sin(position);
    double cosTheta = Math.cos(position);

    for (int i = 0; i < numberOfPoint; i++) {
      tilting_angles[2][i] += sampleAngles[2];
      double sinOmega = Math.sin(tilting_angles[0][i]);
      double cosOmega = Math.cos(tilting_angles[0][i]);
      double sinPhi = Math.sin(tilting_angles[2][i]);
      double cosPhi = Math.cos(tilting_angles[2][i]);
      double sinCsi = Math.sin(tilting_angles[1][i]);
      double cosCsi = Math.cos(tilting_angles[1][i]);
      double sinEta = Math.sin(tilting_angles[3][i]);
      double cosEta = Math.cos(tilting_angles[3][i]);

      double c1 = cosOmegaSample * cosPhi;
      double cs1 = cosOmegaSample * sinPhi;
      double s2 = sinOmegaSample * cosPhi;
      double c2 = cosCsiSample * cosCsi;
      double c3 = cosCsiSample * sinCsi;
      double c4 = sinCsiSample * sinCsi;
      double s1 = cosCsiSample * sinPhi;
      double const1 = c1 * sinCsiSample + c2;
      double const2 = -const1 * sinCsi + cosOmegaSample * c2;
      double const3 = cs1 * sinCsiSample - s2;
      double const4 = const1 * cosCsi + cosOmegaSample * c3;
      double const5 = s2 * sinCsiSample - cs1;
      double const6 = -const5 * sinCsi + sinOmegaSample * c2;
      double const7 = sinOmegaSample * sinPhi * sinCsiSample + c1;
      double const8 = const5 * cosCsi + sinOmegaSample * c3;
      double const9 = -c3 * cosPhi - sinCsiSample * cosCsi;
      double const10 = s1 * sinOmega + const9 * cosOmega;
      double const11 = c2 * cosPhi - c4;

      double M13 = -(s1 * cosOmega - const9 * sinOmega) * sinTheta +
              (const11 * sinEta + const10 * cosEta) * cosTheta; // res[0][2];
      double M23 = -(const7 * cosOmega - const6 * sinOmega) * sinTheta + (const8 * sinEta +
              (const7 * sinOmega + const6 * cosOmega) * cosEta) * cosTheta; // res[1][2];
      double cosPsi = -(const3 * cosOmega - const2 * sinOmega) * sinTheta + (const4 * sinEta +
              (const3 * sinOmega + const2 * cosOmega) * cosEta) * cosTheta; // res[2][2];

      tilting_angles[0][i] = Math.acos(cosPsi);

      if (Math.abs(M23) < 1.0E-9)
        M23 = 0.0f;
      if (Math.abs(M13) < 1.0E-9) {
        if (M23 >= 0.0)
          tilting_angles[1][i] = pi_over2;
        else
          tilting_angles[1][i] = -pi_over2;
      } else if (M23 == 0.0) {
        if (M13 > 0.0)
          tilting_angles[1][i] = Math.PI;
        else
          tilting_angles[1][i] = 0.0f;
      } else {
        tilting_angles[1][i] = -Math.atan(M23 / M13);
        if (M13 > 0.0)
          tilting_angles[1][i] += Math.PI;
      }

      if (tilting_angles[0][i] > pi_over2) {
        tilting_angles[0][i] = Math.PI - tilting_angles[0][i];
        tilting_angles[1][i] += Math.PI;
      }
      if (tilting_angles[1][i] > Constants.PI2)
        tilting_angles[1][i] -= Constants.PI2;
      if (tilting_angles[1][i] < 0.0)
        tilting_angles[1][i] += Constants.PI2;

    }

  }

  /**
   * Return an array of two floats containing the azimuthal and polar angles in pole
   * figure coordinates for the incident and  diffracted beam. To be used, for example,
   * for absorption correction.
   * @param datafile  the spectrum or datafile for which the angles should be computed
   * @param tilting_angles  angles at which the spectrum was collected in the instrument coordinates.
   * @param sampleAngles  array containing omega, chi and phi angular position of the sample
   * @param position  the 2-theta or d-spacing (if the spectrum is in d-spacing) of the
   * peak for which the angles should be computed.
   * The angles are putted in the tilting_angles matrix on [0][index]  and  [1][index]  elements
   * for the azimuthal and polar angles.
   */

  public double[][] getIncidentAndDiffractionAngles(DiffrDataFile datafile, double[] tilting_angles,
                                                   double[] sampleAngles, double[] position) {

    double[] addSampleAngles = datafile.getDataFileSet().getAdditionalSampleAngles();
    double[] newSampleAngles = new double[3];
    for (int i = 0; i < 3; i++)
      newSampleAngles[i] = sampleAngles[i] + addSampleAngles[i];
    double tilting_angles0 = tilting_angles[0];
    double tilting_angles1 = tilting_angles[1];
    double tilting_angles2 = tilting_angles[2];
    double oldTilt0 = tilting_angles0 + 1.0f;
    double oldTilt1 = tilting_angles1 + 1.0f;
    double oldTilt2 = tilting_angles2 + 1.0f;
    double oldTilt3 = - 90001.0f;
    int numberPositions = position.length;
    double[] incidentAngles = null, diffractionAngles = null, textureAngles = null;
    double[][] allAngles = new double[numberPositions][6];
    for (int j = 0; j < numberPositions; j++) {
      double tilting_angles3 = tilting_angles[3] + getEtaDetector(datafile);
      if (oldTilt0 != tilting_angles0 || (oldTilt1 != tilting_angles1 || (oldTilt2 != tilting_angles2 ||
          (oldTilt3 != tilting_angles3)))) {
        incidentAngles = getTextureAnglesR(tilting_angles0, tilting_angles1, tilting_angles2,
              tilting_angles3, 90.0f, newSampleAngles, false);

        diffractionAngles = getTextureAnglesR(tilting_angles0, tilting_angles1, tilting_angles2,
              tilting_angles3, -90.0f + position[j], newSampleAngles, false);

        textureAngles = getTextureAnglesR(tilting_angles0, tilting_angles1, tilting_angles2,
              tilting_angles3, position[j] / 2.0f, newSampleAngles, false);

      }
      oldTilt3 = tilting_angles3;
      for (int i = 0; i < 2; i++) {
        allAngles[j][i] = incidentAngles[i];
        allAngles[j][i + 2] = diffractionAngles[i];
        allAngles[j][i + 4] = textureAngles[i];
      }
    }
    return allAngles;
  }

	public double[][][] getIncidentAndDiffractionAngles(DiffrDataFile datafile, double[] tilting_angles,
	                                                  double[] sampleAngles, double[][] position) {

		double[] addSampleAngles = datafile.getDataFileSet().getAdditionalSampleAngles();
		double[] newSampleAngles = new double[3];
		for (int i = 0; i < 3; i++)
			newSampleAngles[i] = sampleAngles[i] + addSampleAngles[i];
		double tilting_angles0 = tilting_angles[0];
		double tilting_angles1 = tilting_angles[1];
		double tilting_angles2 = tilting_angles[2];
		double oldTilt0 = tilting_angles0 + 1.0f;
		double oldTilt1 = tilting_angles1 + 1.0f;
		double oldTilt2 = tilting_angles2 + 1.0f;
		double oldTilt3 = - 90001.0f;
		int numberPositions = position.length;
		int nrad = position[0].length;
		double[] incidentAngles = null, diffractionAngles = null, textureAngles = null;
		double[][][] allAngles = new double[numberPositions][nrad][6];
		for (int j = 0; j < numberPositions; j++) {
			for (int k = 0; k < nrad; k++) {
				double tilting_angles3 = tilting_angles[3] + getEtaDetector(datafile);
				if (oldTilt0 != tilting_angles0 || (oldTilt1 != tilting_angles1 || (oldTilt2 != tilting_angles2 ||
						(oldTilt3 != tilting_angles3)))) {
					incidentAngles = getTextureAnglesR(tilting_angles0, tilting_angles1, tilting_angles2,
							tilting_angles3, 90.0f, newSampleAngles, false);

					diffractionAngles = getTextureAnglesR(tilting_angles0, tilting_angles1, tilting_angles2,
							tilting_angles3, -90.0f + position[j][k], newSampleAngles, false);

					textureAngles = getTextureAnglesR(tilting_angles0, tilting_angles1, tilting_angles2,
							tilting_angles3, position[j][k] / 2.0f, newSampleAngles, false);

				}
				oldTilt3 = tilting_angles3;
				for (int i = 0; i < 2; i++) {
					allAngles[j][k][i] = incidentAngles[i];
					allAngles[j][k][i + 2] = diffractionAngles[i];
					allAngles[j][k][i + 4] = textureAngles[i];
				}
			}
		}
		return allAngles;
	}

	/**
   * Return an array of two floats containing the azimuthal and polar angles in pole
   * figure coordinates for the incident and  diffracted beam. To be used, for example,
   * for absorption correction.
   * @param datafile  the spectrum or datafile for which the angles should be computed
   * @param tilting_angles  angles at which the spectrum was collected in the instrument coordinates.
   * @param sampleAngles  array containing omega, chi and phi angular position of the sample
   * @param position  the 2-theta or d-spacing (if the spectrum is in d-spacing) of the
   * peak for which the angles should be computed.
   * The angles are putted in the tilting_angles matrix on [0][index]  and  [1][index]  elements
   * for the azimuthal and polar angles.
   */

  public double[] getIncidentAndDiffractionAngles(DiffrDataFile datafile, double[] tilting_angles,
                                                   double[] sampleAngles, double position) {
    double[] addSampleAngles = datafile.getDataFileSet().getAdditionalSampleAngles();
    double[] newSampleAngles = new double[3];
    for (int i = 0; i < 3; i++)
      newSampleAngles[i] = sampleAngles[i] + addSampleAngles[i];
    double tilting_angles3 = tilting_angles[3] + getEtaDetector(datafile);
    double[] incidentAngles = getTextureAnglesR(tilting_angles[0], tilting_angles[1], tilting_angles[2],
              tilting_angles3, 90.0f, newSampleAngles, false);

    double[] diffractionAngles = getTextureAnglesR(tilting_angles[0], tilting_angles[1], tilting_angles[2],
              tilting_angles3, -90.0f + position, newSampleAngles, false);

    double[] allAngles = new double[4];
      for (int i = 0; i < 2; i++) {
        allAngles[i] = incidentAngles[i];
        allAngles[i + 2] = diffractionAngles[i];
      }

    allAngles[0] = (pi_over2 - allAngles[0]);
    allAngles[2] = (pi_over2 - allAngles[2]);
/*    System.out.println(position + " " + tilting_angles[0] + " " + tilting_angles[1] + " " + tilting_angles[2] + " " + tilting_angles3 + " " +
        allAngles[0] * Constants.PITODEG + " " + allAngles[2] * Constants.PITODEG + " " +
        allAngles[1] * Constants.PITODEG + " " + allAngles[3] * Constants.PITODEG);*/
    return allAngles;
  }

// not to use
  public double[] getTextureAngles(double[] tilting_angles, double twotheta) {
    return getDetector().getTextureAngles(tilting_angles, twotheta);
  }

  /**
   * Return the Lorentz-Polarization factor.
   * @param adatafile  the spectrum or datafile for which the factor must be computed.
   * @param asample  the sample object asking for the computation.
   * @param position  the 2-theta or d-spacing (if the spectrum is in d-spacing) of the
* point for which the correction should be computed.
   * @param dspacingbase  true if position is in d-space (Angstroms), false if 2theta
* The basic method here just return 1.0 always. Must be overwrited for other geometries.
* The dspacingbase actually should not be used, as its value should be equal to
   * @param energyDispersive
   */

  public double LorentzPolarization(DiffrDataFile adatafile, Sample asample, double position,
                                    boolean dspacingbase, boolean energyDispersive) {
    position *= degtopi2;
    return Math.abs(polarization(adatafile, position) * Lorentz(adatafile, position));
  }

  public double Lorentz(DiffrDataFile adatafile, double position) {
    return 1.0;
  }

  public double polarization(DiffrDataFile adatafile, double position) {
    return 1.0;
  }

  /**
   * Compute the Lorentz-Polarization factor and return it as a multipler in the intensity array.
   * @param adatafile  the spectrum or datafile for which the factor must be computed.
   * @param asample  the sample object asking for the computation.
   * @param position  the 2-theta or d-spacing (if the spectrum is in d-spacing) of the
* point for which the correction should be computed.
   * @param dspacingbase  true if position is in d-space (Angstroms), false if 2theta
   * @param energyDispersive
   * @param intensity  the array of intensities. In return the method give intensity *= LP
   * The basic method here just return 1.0 always. Must be overwrited for other geometries.
   * The dspacingbase actually should not be used, as its value should be equal to
   */

  public void LorentzPolarization(DiffrDataFile adatafile, Sample asample, double[] position, boolean dspacingbase,
                                  boolean energyDispersive, double[] intensity) {
    for (int i = 0; i < intensity.length; i++) {
      intensity[i] *= LorentzPolarization(adatafile, asample, position[i], dspacingbase, energyDispersive);
    }
  }

	public double getPolarizationAmount() {
		return 0;
	}

	public double getPolarizationAngle() {
		return 0;
	}

  /**
   * Return the correction factor for the beam going out of the sample. Factor 1.0 means all
   * the beam is inside.
   * @param adatafile  the spectrum or datafile for which the factor must be computed.
   * @param asample  the sample object asking for the computation.
   * @param position  the 2-theta or d-spacing (if the spectrum is in d-spacing) of the
 * point for which the correction should be computed.
   */

  public double getBeamRelatedCorrection(DiffrDataFile adatafile, Sample asample, double position, int pointIndex) {
    return 1.0;
  }

  /**
   * Return the correction factor for the absorption by the sample.
   * @param adatafile  the spectrum or datafile for which the factor must be computed.
   * @param asample  the sample object asking for the computation.
   * @param position  the 2-theta or d-spacing (if the spectrum is in d-spacing) of the
* point for which the correction should be computed.
   * @param dspacingbase  true if position is in d-space (Angstroms), false if 2theta
   * @param energyDispersive
   * @param intensity  the array of intensities. In return the method give intensity *= LP
   * The basic method here just return 1.0 always. Must be overwrited for other geometries.
   * The dspacingbase actually should not be used, as its value should be equal to
   */

  public void computeShapeAbsorptionCorrection(DiffrDataFile adatafile, Sample asample, double[][] position,
                                               boolean dspacingbase, boolean energyDispersive, double[][] intensity, double toLambda) {
  }

  public double[] getLayerAbsorption_new(Sample asample, RadiationType rad, int layerIndex, double[][] incidentDiffractionAngles,
                                       DataFileSet adataset) {
	  double[] radAbs = new double[rad.getLinesCount()];
		for (int i = 0; i < rad.getLinesCount(); i++)
			radAbs[i] = 1.0;
	  return radAbs;
  }

	public double getLayerAbsorption_new(Sample asample, double energyInKeV, int layerIndex, double[] incidentDiffractionAngles,
	                                     DataFileSet adataset) {
		return asample.getLayerAbsorption_new(energyInKeV, layerIndex, incidentDiffractionAngles, adataset);
	}

	/**
   * Return the position of the peak corrected for position, aberration, instrumental errors.
   * The basic method don't compute any correction and give back the same value.
   * @param asample  the sample for which should be computed.
   * @param position  the 2-theta or d-spacing (if the spectrum is in d-spacing) of the
   * point for which the broadening should be computed.
   * @param tilting_angles  angles at which the spectrum was collected in the instrument coordinates.
   * @param adatafile  the spectrum or datafile for which the factor must be computed.
   */

  public double getCorrectedPosition(Sample asample, double position, double[] tilting_angles,
                                     DiffrDataFile adatafile) {
    return position;
  }

  /**
   * Free all the parameters for a basic refine that could be refined here. Actually none for
   * geometry.
   */

  public boolean freeAllBasicParameters() {
// here is an example from Instrument
//		for (int i = 0; i < getthetaoffsetnumber(); i++)
//			getThetaDisplacement(i).setRefinableCheckBound();
    return false;
  }

  /**
   * Instantiates and display the properties/options frame of this object.
   */

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JGeometryOptionsD(parent, this);
    return adialog;
  }

  public void setRadius(String value) {
  }

  /**
   * Class JGeometryOptionsD, to edit properties/options of this object.
   */

  public class JGeometryOptionsD extends JOptionsDialog {

    public JGeometryOptionsD(Frame parent, XRDcat obj) {

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
