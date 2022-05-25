/*
 * @(#)HarmonicStrainT.java created 04/12/1999 Pergine Vals.
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.rsa;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.rta.*;

/**
 *  The HarmonicStrainT is a class to compute the strain pole figure for textured
 * sample using the harmonic expansion approximation.
 * To be completed!!!
 *
 *
 * @version $Revision: 1.11 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class HarmonicStrainT extends HarmonicStrainRT {

  Texture activeTexture = null;
  public static int nfismax = (int) (360.0 / Constants.integrationStepPF + 1.00001);

  public HarmonicStrainT(XRDcat aobj, String alabel) {
    super(aobj, alabel);
//		initXRD();
    identifier = "Harmonic Strain with Texture";
    IDlabel = "Harmonic Strain with Texture";
    description = "select this to apply Harmonic model for SODF with texture";
  }

  public HarmonicStrainT(XRDcat aobj) {
    this(aobj, "Harmonic method for SODF and texture");
  }

  public HarmonicStrainT() {
    identifier = "Disabled Harmonic Strain with Texture";
    IDlabel = "Harmonic Strain with Texture";
    description = "select this to apply Harmonic model for SODF with texture";
  }

  public void applySymmetryRules() {
    super.applySymmetryRules();
    Phase aphase = getPhase();
    activeTexture = aphase.getActiveTexture();
    activeTexture.initializeAll();

  }

  public void refreshForNotificationDown(XRDcat source, int reason) {
    if (!getFilePar().isComputingDerivate() || (source == this ||
            (reason == Constants.SAMPLE_ORIENTATION_CHANGED || (source == getParent() &&
            reason == Constants.TEXTURE_CHANGED))))
      refreshComputation = true;
  }

  public double computeStrain(double[] texture_angles,
                              double[] sctf, double fhir, int inv) {

    return 0.0; //calculateSPF(texture_angles[0], texture_angles[1],
            // sctf[0], sctf[1], fhir, inv);
  }

  public double computeStrain(double phi, double beta, double psi, double gamma) {
    // Angles must be in radiants
    // phi and beta are the polar and azimuthal angles for the crystal setting
    // psi and gamma for the sample

    return 0.0;
  }

  public double calculateSPF(double theta, double phi,
                double sthi, double cthi,
                double fhir, int inv) {
    double gam, bets, als, ang;
    int nfis, ntfs;
    double ca2, cb2, sa2, g2r, ffak;
    int nga, nal, nb, iswitch, ivorz;

		double fs = 0.;


    cb2 = cthi;
    g2r = Constants.PI - fhir;
		boolean checkL13 = false;
   	boolean nextCheck = false;
   	theta *= Constants.DEGTOPI;
   	phi *= Constants.DEGTOPI;
		double cr = Math.cos(theta);
		double sr = Math.sin(theta);
		do {
    	while (g2r < 0.) {
				g2r += Constants.PI2;
    	}
    	for (nfis = 0; nfis < nfismax; nfis++) {
				ang = Constants.DEGTOPI * nfis * Constants.integrationStepPF;
				ca2 = - Math.cos(ang);
				sa2 = Math.sin(ang);
//				for (ntety = 1; ntety <= 19; ++ntety) {
	  	  double[] angles = Uwimvuo.g20g100(ca2, sa2, cb2, sthi, cr, sr);
	  	 	angles[0] += phi;
	   		angles[2] += g2r;
	  	  double odff = activeTexture.getODF(angles[0], angles[1], angles[2]);
	  	  double sodff = 1.0; // getODF(angles[0], angles[1], angles[2]);
	  	  ffak = odff * sodff;
				if (!(nfis == 0 || nfis == nfismax-1)) {
		  	  if (MoreMath.powint(-1, nfis+1) < 0)
		  	  	ffak *= 2.;
					else
						ffak *= 4.;
				}
				fs += ffak;
    	}
    	if (inv == 1 || nextCheck) {
				checkL13 = true;
    	} else {
    		nextCheck = true;
    		cb2 = -cb2;
    		g2r -= Constants.PI;
    	}
    } while (!checkL13);

    if (inv != 1) {
			fs /= 2.;
    }
//		System.out.println(fs);
    return fs * Constants.pisimg;
	}

  public double[][] getPoleFigureGrid(Reflection refl, int numberofPoints, double maxAngle) {

    int h = refl.getH();
    int k = refl.getK();
    int l = refl.getL();

    applySymmetryRules();

    Phase aphase = (Phase) getParent();

    double[][] PFreconstructed = new double[numberofPoints][numberofPoints];

    double[] cdsc = aphase.lattice();

    int LaueGroupSnumber = SpaceGroups.getLGNumberSiegfriedConv(aphase.getPointGroup());
    double[] sctf = Uwimvuo.tfhkl(h, k, l, cdsc[7], cdsc[5], cdsc[3], cdsc[6], cdsc[0], cdsc[1]);
    double fhir = Math.acos(sctf[3]);
    int inv = Uwimvuo.equiv(LaueGroupSnumber, sctf);

    double strain_angles[] = new double[2];

    double x , y, r;
    double dxy = 2.0 * maxAngle / (numberofPoints - 1);

    for (int i = 0; i < numberofPoints; i++)
      for (int j = 0; j < numberofPoints; j++) {
        x = j * dxy - maxAngle;
        y = i * dxy - maxAngle;
        r = Math.sqrt(x * x + y * y);
        if (r == 0.0) {
          strain_angles[0] = 0.0f;
          strain_angles[1] = 0.0f;
          PFreconstructed[i][j] = computeStrain(strain_angles, sctf, fhir,
                  inv);
        } else if (r <= maxAngle) {
          double phaseAng = Math.atan2(x, y);
          if (phaseAng < 0.0)
            phaseAng += Constants.PI2;
          strain_angles[0] = (double) (2.0 * Math.asin(r / Constants.sqrt2) * Constants.PITODEG);
          if (strain_angles[0] < 0.0) {
            strain_angles[0] = -strain_angles[0];
            phaseAng += Constants.PI;
            while (phaseAng >= Constants.PI2)
              phaseAng -= Constants.PI2;
          }
          strain_angles[1] = (double) (phaseAng * Constants.PITODEG);

          PFreconstructed[i][j] = computeStrain(strain_angles, sctf, fhir,
                  inv);
        } else
          PFreconstructed[i][j] = Double.NaN;
      }
    return PFreconstructed;
  }

}
