/*
 * @(#)ExpHarmonicTexture.java created Jun 15, 2016 Bari-Bologna train
 *
 * Copyright (c) 2016 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.rta;

import com.amd.aparapi.*;
import com.amd.aparapi.device.Device;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

/**
 * The ExpHarmonicTextureGPU is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 15, 2016 3:56:33 PM $
 * @since JDK1.1
 */
public class ExpHarmonicTextureGPU extends HarmonicTexture {

  public static final String identifierS = "Disabled Exponential Harmonic GPU";
  public static final String IDlabelS = "Exponential Harmonic GPU";
  public static final String descriptionS = "select this to apply exponential Harmonic model of Van Houtte (GPU accelerated)";

  double odf[] = null;
  boolean refreshODF = true;
//  static boolean tubeProjection = MaudPreferences.getBoolean("expHarmonic.tubeProjection", true);

  public ExpHarmonicTextureGPU(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = identifierS;
    IDlabel = IDlabelS;
    description = descriptionS;
  }

  public ExpHarmonicTextureGPU(XRDcat aobj) {
    this(aobj, identifierS + " method");
  }

  public ExpHarmonicTextureGPU() {
    identifier = identifierS;
    IDlabel = IDlabelS;
    description = descriptionS;
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);
    refreshODF = true;
  }

/*  void computeODF() {
    initializeAll();
  }*/

  public int getLaueGroupNumber() {
    return SpaceGroups.getLGNumberSiegfriedConv(getPhase().getPointGroup());
  }

/*  void odfNormalization() {

    double vbg = 0.0, va = 0.0, hvg = 0., hvb = 0., a = 0., b = 0.;
    double fn = 0.0;

    double fnorm = 0.0;
    double roundOffCorrection = 0.0;
    for (int ng = 0; ng < alphama; ng++) {
      hvg = resolutionR;
      if (ng == 0 || ng == alphama1)
        hvg /= 2;
      for (int nb = 0; nb < betama; nb++) {
        if (nb == 0) {
          a = 0.0;
          b = pi25g;
        } else if (nb == betama1) {
          b = nb * resolutionR;
          a -= pi25g;
        } else {
          a = nb * resolutionR - pi25g;
          b = a + resolutionR;
        }
        hvb = Math.cos(a) - Math.cos(b);
        vbg = hvb * hvg;

        for (int na = 0; na < alphama; na++) {
          if (na == 0 || na == alphama1)
            va = pi25g * vbg;
          else
            va = resolutionR * vbg;
          fn = getODF(na * resolutionR - pi25g, nb * resolutionR - pi25g, ng * resolutionR - pi25g)
              * normalizationFactor;
          fnorm += fn * va;
          roundOffCorrection += va;
        }

      }
    }

//    double fnormTheoretical = Constants.PI * 8. * Constants.PI / fnorm;
    normalizationFactor = roundOffCorrection / fnorm;


//	  System.out.println("Normalization factor: " + Fmt.format(fnormTheoretical));
  }*/

  public double[] computeTextureFactor(double[][] texture_angles,
                                       double[] sctf, double fhir, int inv) {
    return calculatePFbyTubeProjection(texture_angles, sctf[0], sctf[1], fhir, inv);
  }

//  int actualRunningThreads = 0;
//  int datafile = 0;
  double[] cdsc = null;
  int LaueGroupSnumber = 0;
  double resolution = 5.0;
  double resolutionR = resolution * Constants.DEGTOPI;
  double pi25g = resolutionR / 2.;
  public static double integrationStepPF = 1.0; // MaudPreferences.getDouble(Texture.prefs[1], Texture.prefVal[1]);
  public static double integrationStepPFR = integrationStepPF * Constants.DEGTOPI;
  public static int nfismax = (int) (Constants.PI2 / integrationStepPFR + 1.00001);
  public static double pisimg = integrationStepPFR / (6.0 * Constants.PI) / 2;
  int alphama = 0, betama = 0, betaalphama = 0;
  int alphama1 = 0, betama1 = 0;
  double normalizationFactor = 1.0;
  int nge = 0, nbe = 0, nae = 0;
  double[] odfMaxAngles = {360.0, 180.0, 360.0};
  double[] odfMaxAnglesR = {Constants.PI2, Constants.PI, Constants.PI2};
  int rotationFold = 1;
  int mdb = 0;
  double dist_factor2 = 1.0; //Math.pow(3, 2/3);
	int[] ml2;
	int[] nl2;

  public void initializeAll() {
//    tubeProjection = MaudPreferences.getBoolean("expHarmonic.tubeProjection", true);
    boolean newODF = false;
    if (LaueGroupSnumber != getLaueGroupNumber() || odf == null/* && tubeProjection*/) {
      LaueGroupSnumber = getLaueGroupNumber();
      refreshODF = true;
      newODF = true;
    }
    applySymmetryRules();
    alphama = Uwimvuo.getAlphamax(resolution);
    betama = (alphama + 1) / 2;
	  betaalphama = alphama * betama;
    alphama1 = alphama - 1;
    betama1 = betama - 1;
    integrationStepPF = MaudPreferences.getDouble(Texture.prefs[1], Double.parseDouble(Texture.prefVal[1]));
    if (integrationStepPF <= 0.0)
      integrationStepPF = resolution / 2.0;
	  if (integrationStepPF > 1.0)
		  integrationStepPF = 1.0;
    integrationStepPFR = integrationStepPF * Constants.DEGTOPI;
    nfismax = (int) (Constants.PI2 / integrationStepPFR + 1.00001);
//    System.out.println(nfismax);
    pisimg = integrationStepPFR / (6.0 * Constants.PI);
//    if (!tubeProjection)
//      odfNormalization();
    double dist_factor = resolution * 2.0;
    dist_factor2 = dist_factor * dist_factor;

    int betamad2 = (betama + 1) / 2;
    int alphamad3 = (alphama + 2) / 3;
    int alphamad6 = (alphama + 5) / 6;

    nae = alphama;
    rotationFold = 1;
    switch (LaueGroupSnumber) {
      case 2:
        nbe = betama;
        nge = betama;
        rotationFold = 2;
        mdb = 0;
        break;
      case 3:
      case 6:
        nbe = betamad2;
        nge = betama;
        rotationFold = 2;
        mdb = 1;
        break;
      case 4:
        nbe = betama;
        nge = betamad2;
        rotationFold = 4;
        mdb = 0;
        break;
      case 5:
      case 7:
        nbe = betamad2;
        nge = betamad2;
        rotationFold = 4;
        mdb = 1;
        break;
      case 8:
        nbe = betama;
        nge = alphamad3;
        rotationFold = 3;
        mdb = 0;
        break;
      case 9:
        nbe = betamad2;
        nge = alphamad3;
        rotationFold = 3;
        mdb = 1;
        break;
      case 10:
        nbe = betama;
        nge = alphamad6;
        rotationFold = 6;
        mdb = 0;
        break;
      case 11:
        nbe = betamad2;
        nge = alphamad6;
        rotationFold = 6;
        mdb = 1;
        break;
      case 1:
      default:
        {
          nge = alphama;
          nbe = betama;
          rotationFold = 1;
          mdb = 0;
        }
    }

    odfMaxAngles[0] = 360.0;
    odfMaxAngles[1] = 180.0 / (mdb + 1);
    odfMaxAngles[2] = 360.0 / rotationFold;
    odfMaxAnglesR[0] = odfMaxAngles[0] * Constants.DEGTOPI;
    odfMaxAnglesR[1] = odfMaxAngles[1] * Constants.DEGTOPI;
    odfMaxAnglesR[2] = odfMaxAngles[2] * Constants.DEGTOPI;

	  int lMax = expansionDegree / 2;
	  ml2 = new int[lMax];
	  nl2 = new int[lMax];
	  for (int l = 2, i = 0; l <= expansionDegree; l += 2, i++) {
		  ml2[i] = SphericalHarmonics.getN(LGIndex, l);
		  nl2[i] = SphericalHarmonics.getN(sampleSymmetry, l);
	  }

	  if (newODF)
		  odf = new double[alphama * betaalphama];
	  if (refreshODF/* && tubeProjection*/)
		  computeODFFromCoefficients();

	}

	public void computeTextureFactor(final Phase aphase, final Sample asample) {

    if (!refreshComputation)
      return;

    initializeAll();

//    fiottu();
//    odfNormalization();
//		checkFullODF();

    refreshComputation = false;

    cdsc = aphase.lattice();

//    double phoninp = subfmin();

    int hkln = aphase.gethklNumber();

    int totdatafile = 0;
    for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
	    for (int j = 0; j < asample.getActiveDataSet(i).activedatafilesnumber(); j++)
	      totdatafile += asample.getActiveDataSet(i).getActiveDataFile(j).positionsPerPattern;
    }

	  double[][] textF = new double[totdatafile][hkln];

	  for (int ij = 0; ij < hkln; ij++) {
		  double texAngle[][] = new double[2][totdatafile];

//            for (int j = 0; j < hkln; j++) {
		  Reflection refl = aphase.getReflectionVector().elementAt(ij);
		  double[] sctf = Uwimvuo.tfhkl(refl.getH(), refl.getK(), refl.getL(), cdsc[7], cdsc[5], cdsc[3], cdsc[6], cdsc[0], cdsc[1]);
		  double fhir = Math.acos(sctf[3]);
		  int inv = Uwimvuo.equiv(LaueGroupSnumber, sctf);
//      System.out.println("h k l, inv : " + refl.h + refl.k + refl.l + ", " + inv);

		  int idatafile = 0;
		  for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
			  int datafilenumber = asample.getActiveDataSet(i).activedatafilesnumber();
			  for (int ij1 = 0; ij1 < datafilenumber; ij1++) {
				  DiffrDataFile adatafile = asample.getActiveDataSet(i).getActiveDataFile(ij1);
				  double[][] positions = adatafile.getPositions(aphase)[0];
//				  for (int ppp = 0; ppp < adatafile.positionsPerPattern; ppp++) {
					  double texture_angles[] = adatafile.getTextureAngles(positions[ij][0]);
					  texAngle[0][idatafile] = (texture_angles[0] * Constants.DEGTOPI);
					  texAngle[1][idatafile] = (texture_angles[1] * Constants.DEGTOPI);
					  idatafile++;
//				  }
			  }
		  }
		  double[] texFactor = computeTextureFactor(texAngle, sctf, fhir, inv);
		  idatafile = 0;
		  for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
			  int datafilenumber = asample.getActiveDataSet(i).activedatafilesnumber();
			  for (int ij1 = 0; ij1 < datafilenumber; ij1++) {
//				  DiffrDataFile adatafile = asample.getActiveDataSet(i).getActiveDataFile(ij1);
//				  for (int ppp = 0; ppp < adatafile.positionsPerPattern; ppp++) {
					  textF[idatafile][ij] = texFactor[idatafile];
				  idatafile++;
//				  }
			  }
		  }
//						refl.setExpTextureFactor(adatafile.getIndex(), textF[j][datafile]);
//            }
	  }

		  int idatafile = 0;
		  for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
			  int datafilenumber = asample.getActiveDataSet(i).activedatafilesnumber();
			  for (int ij1 = 0; ij1 < datafilenumber; ij1++) {
				  DiffrDataFile adatafile = asample.getActiveDataSet(i).getActiveDataFile(ij1);
				  adatafile.setTextureFactors(aphase, textF[idatafile++]);
			  }
		  }

	  // checking odf
//		Reflection refl = (Reflection) aphase.reflectionv.elementAt(0);
//		checkComputation(refl, 10.0 ,55.0);
  }

  public double[] computeTextureFactor(Phase aphase, double[][] alphabeta, Reflection reflex) {

//    int numberOfPoints = alphabeta.length/2;

    initializeAll();

    double[] cdsc = aphase.lattice();

//    double phoninp = subfmin();

    double[] sctf = Uwimvuo.tfhkl(reflex.getH(), reflex.getK(), reflex.getL(), cdsc[7], cdsc[5], cdsc[3], cdsc[6], cdsc[0], cdsc[1]);
    double fhir = Math.acos(sctf[3]);
    int inv = Uwimvuo.equiv(LaueGroupSnumber, sctf);

    return computeTextureFactor(alphabeta, sctf, fhir, inv);
  }

  public double[][] getInversePoleFigureGrid(double[] texture_angles,
                                             double maxPhi, int phiPointNumber,
                                             double maxBeta, int betaPointNumber) {

    initializeAll();

    double[][] PFreconstructed = new double[phiPointNumber][betaPointNumber];

    double[][] textureAngles = new double[2][1];
    for (int i = 0; i < 2; i++)
      textureAngles[i][0] = (texture_angles[i] * Constants.DEGTOPI);
    double phi, beta;
    double dphi = maxPhi / (phiPointNumber - 1);
    double dbeta = maxBeta / (betaPointNumber - 1);
    double[] sctf = new double[4];

    for (int i = 0; i < phiPointNumber; i++)
      for (int j = 0; j < betaPointNumber; j++) {
        beta = j * dbeta;
        phi = i * dphi;
        sctf[0] = Math.sin(phi);
        sctf[1] = Math.cos(phi);
        sctf[2] = Math.sin(beta);
        sctf[3] = Math.cos(beta);
        double fhir = beta;
        int inv = Uwimvuo.equiv(LaueGroupSnumber, sctf);
        double[] value = computeTextureFactor(textureAngles, sctf, fhir, inv);
        PFreconstructed[i][j] = value[0];
      }
    return PFreconstructed;
  }

  public double[] getInversePoleFigureGrid(double[] texture_angles,
                                           double[][] phibeta) {
    initializeAll();

    int pointNumber = phibeta[0].length;
    double[] PFreconstructed = new double[pointNumber];

    double[][] textureAngles = new double[2][1];
    for (int i = 0; i < 2; i++)
      textureAngles[i][0] = (texture_angles[i] * Constants.DEGTOPI);
    double[] sctf = new double[4];

    for (int i = 0; i < pointNumber; i++) {
      sctf[0] = Math.sin(phibeta[0][i]);
      sctf[1] = Math.cos(phibeta[0][i]);
      sctf[2] = Math.sin(phibeta[1][i]);
      sctf[3] = Math.cos(phibeta[1][i]);
      double fhir = phibeta[1][i];
      int inv = Uwimvuo.equiv(LaueGroupSnumber, sctf);
      double[] value = computeTextureFactor(textureAngles, sctf, fhir, inv);
      PFreconstructed[i] = value[0];
    }

    return PFreconstructed;
  }

  public double[][] getPoleFigureGrid(Reflection refl, int numberofPoints, double maxAngle) {

    int h = refl.getH();
    int k = refl.getK();
    int l = refl.getL();

    initializeAll();

    double[][] PFreconstructed = new double[numberofPoints][numberofPoints];

    Phase aphase = getPhase();

    double[] cdsc = aphase.lattice();

//    double phoninp = subfmin();

//		int hkln = aphase.gethklNumber();

    double[] sctf = Uwimvuo.tfhkl(h, k, l, cdsc[7], cdsc[5], cdsc[3], cdsc[6], cdsc[0], cdsc[1]);
    double fhir = Math.acos(sctf[3]);
    int inv = Uwimvuo.equiv(LaueGroupSnumber, sctf);

    double[][] texture_angles = new double[2][numberofPoints * numberofPoints];
    boolean[] included = new boolean[numberofPoints * numberofPoints];

    double x, y, r;
    double dxy = 2.0 * maxAngle / numberofPoints;

//          System.out.println(2.0 * Math.asin(maxAngle / Constants.sqrt2) * Constants.PITODEG);
    int count = 0;
    int countIncluded = 0;
    for (int i = 0; i < numberofPoints; i++)
      for (int j = 0; j < numberofPoints; j++) {
        x = (j + 0.5) * dxy - maxAngle;
        y = (i + 0.5) * dxy - maxAngle;
        r = Math.sqrt(x * x + y * y);
        if (r == 0.0) {
          texture_angles[0][countIncluded] = 0.0f;
          texture_angles[1][countIncluded++] = 0.0f;
          included[count++] = true;
        } else if (r < maxAngle) {
          double phaseAng = Math.atan2(x, y);
          if (phaseAng < 0.0)
            phaseAng += Constants.PI2;
          texture_angles[0][countIncluded] = 2.0f * Math.asin(r / Constants.sqrt2);
          if (texture_angles[0][countIncluded] < 0.0) {
            texture_angles[0][countIncluded] = -texture_angles[0][countIncluded];
            phaseAng += Constants.PI;
            while (phaseAng >= Constants.PI2)
              phaseAng -= Constants.PI2;
          }
          texture_angles[1][countIncluded++] = phaseAng;
          included[count++] = true;
//          System.out.println(texture_angles[0] + " " + texture_angles[1]);

        } else {
          PFreconstructed[i][j] = Double.NaN;
          included[count++] = false;
        }
      }

    double[][] new_texture_angles = new double[2][countIncluded];

    for (int i = 0; i < countIncluded; i++)
      for (int j = 0; j < 2; j++)
        new_texture_angles[j][i] = texture_angles[j][i];

    double[] textureFactors = computeTextureFactor(new_texture_angles, sctf, fhir, inv);

    count = 0;
    countIncluded = 0;
    for (int i = 0; i < numberofPoints; i++)
      for (int j = 0; j < numberofPoints; j++)
        if (included[count++])
          PFreconstructed[i][j] = textureFactors[countIncluded++];

    return PFreconstructed;
  }

/*  public double[] calculatePF(double[][] thetaphi,
                              double sthi, double cthi, double fhir, int inv) {
// Local variables
    double ang;
    int nfis;
    double ca2, cb2, sa2;

//     Calculation of a complete reduced pole figure
       INPUT FIO given in the whole G-space OUTPUT POLREF=FS

//    boolean negODFout = false;
//    if (Constants.testing)
//    	negODFout = MaudPreferences.getBoolean("debug.negativeODFout", false);

    cb2 = cthi;
    int nfismax = (int) (Constants.PI2 / integrationStepPFR + 1.000001);

    int numberOfPoints = thetaphi[0].length;
    double[] fs = new double[numberOfPoints];

    int[] referenceCounter = new int[numberOfPoints];
    int[] references = new int[numberOfPoints];
    int[] pointReference = new int[numberOfPoints];

    pointReference[0] = 0;
    references[0] = 0;
    int numberOfReferences = 0;
    for (int n = 0; n < numberOfPoints; n++) {
//      thetaphi[0][n] *= Constants.DEGTOPI;
//      thetaphi[1][n] *= Constants.DEGTOPI;
      boolean isNewReference = true;
      for (int nscan = 0; nscan < numberOfReferences; nscan++)
        if (Math.abs(thetaphi[0][references[nscan]] - thetaphi[0][n]) < 0.0002) {
          isNewReference = false;
          referenceCounter[pointReference[references[nscan]]]++;
          pointReference[n] = pointReference[references[nscan]];
          break;
        }
      if (isNewReference) {
        references[numberOfReferences] = n;
        pointReference[n] = numberOfReferences;
        referenceCounter[numberOfReferences]++;
        numberOfReferences++;
      }
    }

    int[] finalPointReference = new int[numberOfReferences];
    finalPointReference[0] = referenceCounter[0];
    references[0] = 0;
    for (int n = 1; n < numberOfReferences; n++) {
      references[n] = finalPointReference[n - 1];
      finalPointReference[n] = finalPointReference[n - 1] + referenceCounter[n];
    }

    for (int n = 0; n < numberOfPoints; n++) {
      referenceCounter[references[pointReference[n]]] = n;
      references[pointReference[n]]++;
    }

    int startingPoint = 0;

    double[] angles = new double[3];
    int[] iaindex = new int[3];

    int maxRepeat = 2 - inv;
    double[][] phiRef = new double[maxRepeat][nfismax];
    double[][] angle1 = new double[maxRepeat][nfismax];
    double[][] angle2 = new double[maxRepeat][nfismax];
    double[] g2rv = new double[maxRepeat];
    double[] cb2v = new double[maxRepeat];
    g2rv[0] = Constants.PI - fhir;
    while (g2rv[0] < 0.)
      g2rv[0] += Constants.PI2;
    cb2v[0] = cb2;
    for (int iu = 1; iu < maxRepeat; iu++) {
      cb2v[iu] = -cb2;
      g2rv[iu] = g2rv[0] - Constants.PI;
      while (g2rv[iu] < 0.)
        g2rv[iu] += Constants.PI2;
    }

    for (int nref = 0; nref < numberOfReferences; nref++) {
      // evaluate the first new theta value, all the other change only in phi
      int n1 = referenceCounter[startingPoint];
      fs[n1] = 0.;

// Projection thread loop, Simpson integration
      double cr = Math.cos(thetaphi[0][n1]);
      double sr = Math.sin(thetaphi[0][n1]);
      for (nfis = 0; nfis < nfismax; nfis++) {
        ang = nfis * integrationStepPFR;
        ca2 = -Math.cos(ang);
        sa2 = Math.sin(ang);
        double ffak1 = 0.0;
        for (int repeat = 0; repeat < maxRepeat; repeat++) {
          Angles.g20g100(angles, ca2, sa2, cb2v[repeat], sthi, cr, sr);
          phiRef[repeat][nfis] = angles[0]; // - iaindex[0] * resolutionR;
          angles[0] += thetaphi[1][n1];
          angles[2] += g2rv[repeat];
          angle1[repeat][nfis] = angles[1];
          angle2[repeat][nfis] = angles[2];
          ffak1 += getODF(angles[0], angles[1], angles[2]);
        }
        if (0 < nfis && nfis < nfismax - 1) {
          if (MoreMath.odd(nfis + 1))
            ffak1 *= 2;
          else
            ffak1 *= 4;
        }
        fs[n1] += ffak1;
      }

      fs[n1] *= pisimg / maxRepeat;

      startingPoint++;
      for (int n = startingPoint; n < finalPointReference[nref]; n++) {
        // referenceCounter[n] is the point to evaluate
        n1 = referenceCounter[n];
        fs[n1] = 0.;

        // Projection thread loop, Simpson integration
        for (nfis = 0; nfis < nfismax; nfis++) {
          double ffak1 = 0.0;
          for (int repeat = 0; repeat < maxRepeat; repeat++) {
            angles[0] = phiRef[repeat][nfis] + thetaphi[1][n1];
            angles[1] = angle1[repeat][nfis];
            angles[2] = angle2[repeat][nfis];

//						double f_odf = getODF(iaindex); // Luca_last1202 f[findex[0]][findex[1]][findex[2]];
            ffak1 += getODF(angles[0], angles[1], angles[2]);
          }
          if (0 < nfis && nfis < nfismax - 1) {
            if (MoreMath.odd(nfis + 1))
              ffak1 *= 2.;
            else
              ffak1 *= 4.;
          }
          fs[n1] += ffak1;
        }
        fs[n1] *= pisimg / maxRepeat;
      }
      startingPoint = finalPointReference[nref];  // next one
    }

//                                          Normalization to PINPF
//		System.out.println(fs);
    return fs;
  }*/

		public final double getDTesseralFunction(int l, int m, int n,
		                                         double alpha, double beta, double gamma) {

			double cx0, cx1;
			double i1 = (MoreMath.odd(m + n)) ? -1 : 1;
			double i2 = (MoreMath.odd(l)) ? -1 : 1;
			int mabs = m > 0 ? m : -m ;
			int nabs = n > 0 ? n : -n ;

			double arg1 = mabs * alpha + nabs * gamma;
			double arg2 = mabs * alpha - nabs * gamma;

			if (m > 0) {
				if (n > 0) {
					cx0 = Math.cos(arg1);
					cx1 = Math.cos(arg2);
				} else if (n < 0) {
					cx0 = -Math.sin(arg1);
					cx1 = Math.sin(arg2);
				} else {
					cx0 = Constants.sqrt2 * Math.cos(arg1);
					cx1 = 0;
				}
			} else if (m < 0) {
				if (n > 0) {
					cx0 = Math.sin(arg1);
					cx1 = Math.sin(arg2);
				} else if (n < 0) {
					cx0 = Math.cos(arg1);
					cx1 = -Math.cos(arg2);
				} else {
					cx0 = Constants.sqrt2 * Math.sin(arg1);
					cx1 = 0.0;
				}
			} else {
				if (n > 0) {
					cx0 = Constants.sqrt2 * Math.cos(arg1);
					cx1 = 0.0;
				} else if (n < 0) {
					cx0 = -Constants.sqrt2 * Math.sin(arg1);
					cx1 = 0.0;
				} else {
					cx0 = Math.cos(arg1);
					cx1 = 0;
				}
			}

			double result;
			if (cx1 != 0) {
				double df0, df1;
				double dftmp;
				double dbeta = Constants.PI - beta;

				double arg = Constants.PI_2 * (mabs - nabs);
				dftmp = SphericalHarmonics.deltaV[l][0][mabs] * SphericalHarmonics.deltaV[l][0][nabs];
				df0 = dftmp * Math.cos(arg);
				df1 = df0;
				for (int i = 1; i <= l; i++) {
					dftmp = 2.0 * SphericalHarmonics.deltaV[l][i][mabs] * SphericalHarmonics.deltaV[l][i][nabs];
					df0 += dftmp * Math.cos(beta * i - arg);
					df1 += dftmp * Math.cos(dbeta * i - arg);
				}
				result = i1 * cx0 * df0 + i2 * cx1 * df1;
			} else {
				double arg = Constants.PI * (mabs - nabs) / 2.0;
				double df = SphericalHarmonics.deltaV[l][0][mabs] * SphericalHarmonics.deltaV[l][0][nabs] *
						Math.cos(arg);
				for (int i = 1; i <= l; i++)
					df += 2.0 * SphericalHarmonics.deltaV[l][i][mabs] * SphericalHarmonics.deltaV[l][i][nabs] *
							Math.cos(beta * i - arg);
				result = i1 * cx0 * df;
			}
			return result;
		}

		public void odf_comp(int index) {
/*			for (int ng = 0; ng < alphama; ng++)
				for (int nb = 0; nb < betama; nb++)
					for (int na = 0; na < alphama; na++)
						index = na + nb * alphama + ng * betaalphama;*/

			int ng = index / betaalphama;
			int remaining = index - ng * betaalphama;
			int nb = remaining / alphama;
			int na = remaining - nb * alphama;

			double _odf = 0;
			int k = 0;

			double alpha = Constants.PI - resolutionR * na + pi25g;
			double beta = resolutionR * nb - pi25g;
			double gamma = Constants.PI - resolutionR * ng + pi25g;

			for (int l = 2, i = 0; l <= expansionDegree; l += 2, i++)
				for (int m = 1; m <= ml2[i]; m++)
					for (int n = 1; n <= nl2[i]; n++) {
						double Dlmu = 0;
						for (int n1 = -l, i1 = 0; n1 <= l; n1++, i1++) {
							double AlS = AlmunS[i + (n - 1) * lindex + i1 * lindex * sindex];
							if (AlS != 0.0) {
								double tmplmu = 0.0;
								for (int m1 = -l, i2 = 0; m1 <= l; m1++, i2++) {
									double AlC = AlmunC[i + (m - 1) * lindex + i2 * lindex * cindex];
									if (AlC != 0.0)
										tmplmu += AlC * getDTesseralFunction(l, m1, n1, alpha, beta, gamma);
								}
								Dlmu += tmplmu * AlS;
							}
						}
						_odf += coefficient[k++] * Dlmu;
					}

			odf[index] = Math.exp(_odf);

		}

		public void computeODF() {

		}





	final static class ExponentialHarmonicODF extends Kernel {

		double[] AlmunS, AlmunC, deltaV;
		double resolutionR, pi25g;
		int[] ml2, nl2;
		int expansionDegree, alphama, betama, betaalphama, lindex, sindex, cindex, maxExpansion, maxExpansion2;
		double[] coefficient;
		double[] odf;

		public final double getDTesseralFunction(int l, int m, int n,
		                                         double alpha, double beta, double gamma) {

			double cx0, cx1;
			double i1 = (MoreMath.odd(m + n)) ? -1 : 1;
			double i2 = (MoreMath.odd(l)) ? -1 : 1;
			int mabs = m > 0 ? m : -m ;
			int nabs = n > 0 ? n : -n ;

			double arg1 = mabs * alpha + nabs * gamma;
			double arg2 = mabs * alpha - nabs * gamma;

			if (m > 0) {
				if (n > 0) {
					cx0 = Math.cos(arg1);
					cx1 = Math.cos(arg2);
				} else if (n < 0) {
					cx0 = -Math.sin(arg1);
					cx1 = Math.sin(arg2);
				} else {
					cx0 = Constants.sqrt2 * Math.cos(arg1);
					cx1 = 0;
				}
			} else if (m < 0) {
				if (n > 0) {
					cx0 = Math.sin(arg1);
					cx1 = Math.sin(arg2);
				} else if (n < 0) {
					cx0 = Math.cos(arg1);
					cx1 = -Math.cos(arg2);
				} else {
					cx0 = Constants.sqrt2 * Math.sin(arg1);
					cx1 = 0.0;
				}
			} else {
				if (n > 0) {
					cx0 = Constants.sqrt2 * Math.cos(arg1);
					cx1 = 0.0;
				} else if (n < 0) {
					cx0 = -Constants.sqrt2 * Math.sin(arg1);
					cx1 = 0.0;
				} else {
					cx0 = Math.cos(arg1);
					cx1 = 0;
				}
			}

			double result;
			if (cx1 != 0) {
				double df0, df1;
				double dftmp;
				double dbeta = Constants.PI - beta;

				double arg = Constants.PI_2 * (mabs - nabs);
				dftmp = deltaV[l + mabs * maxExpansion2] * deltaV[l + nabs * maxExpansion2];
				df0 = dftmp * Math.cos(arg);
				df1 = df0;
				for (int i = 1; i <= l; i++) {
					dftmp = 2.0 * deltaV[l + i * maxExpansion + mabs * maxExpansion2] *
							deltaV[l + i * maxExpansion + nabs * maxExpansion2];
					df0 += dftmp * Math.cos(beta * i - arg);
					df1 += dftmp * Math.cos(dbeta * i - arg);
				}
				result = i1 * cx0 * df0 + i2 * cx1 * df1;
			} else {
				double arg = Constants.PI * (mabs - nabs) / 2.0;
				double df = deltaV[l + mabs * maxExpansion2] * deltaV[l + nabs * maxExpansion2] * Math.cos(arg);
				for (int i = 1; i <= l; i++)
					df += 2.0 * deltaV[l + i * maxExpansion + mabs * maxExpansion2] *
							deltaV[l + i * maxExpansion + nabs * maxExpansion2] * Math.cos(beta * i - arg);
				result = i1 * cx0 * df;
			}
			return result;
		}

		@Override public void run() {

			int index = getGlobalId(0);

/*			for (int ng = 0; ng < alphama; ng++)
				for (int nb = 0; nb < betama; nb++)
					for (int na = 0; na < alphama; na++)
						index = na + nb * alphama + ng * betaalphama;*/

			int ng = index / betaalphama;
			int remaining = index - ng * betaalphama;
			int nb = remaining / alphama;
			int na = remaining - nb * alphama;

			double _odf = 0;
			int k = 0;

			double alpha = Constants.PI - resolutionR * na + pi25g;
			double beta = resolutionR * nb - pi25g;
			double gamma = Constants.PI - resolutionR * ng + pi25g;

			for (int l = 2, i = 0; l <= expansionDegree; l += 2, i++)
				for (int m = 1; m <= ml2[i]; m++)
					for (int n = 1; n <= nl2[i]; n++) {
						double Dlmu = 0;
						for (int n1 = -l, i1 = 0; n1 <= l; n1++, i1++) {
							double AlS = AlmunS[i + (n - 1) * lindex + i1 * lindex * sindex];
							if (AlS != 0.0) {
								double tmplmu = 0.0;
								for (int m1 = -l, i2 = 0; m1 <= l; m1++, i2++) {
									double AlC = AlmunC[i + (m - 1) * lindex + i2 * lindex * cindex];
									if (AlC != 0.0)
										tmplmu += AlC * getDTesseralFunction(l, m1, n1, alpha, beta, gamma);
								}
								Dlmu += tmplmu * AlS;
							}
						}
						_odf += coefficient[k++] * Dlmu;
					}

			odf[index] = Math.exp(_odf);

		}

		public void computeODF(double[] _AlmunS, double[] _AlmunC, double _resolutionR, double _pi25g,
		                       int[] _ml2, int[] _nl2, int _expansionDegree, int _alphama, int _betama,
		                       int _lindex, int _sindex, int _cindex, double[] _deltaV, int _maxExpansion,
		                       double[] _coefficient, double[] _odf) {

			AlmunS = _AlmunS;
			AlmunC = _AlmunC;
			resolutionR = _resolutionR;
			pi25g = _pi25g;
			ml2 = _ml2;
			nl2 = _nl2;
			expansionDegree = _expansionDegree;
			alphama = _alphama;
			betama = _betama;
			coefficient = _coefficient;
			odf = _odf;
			lindex = _lindex;
			sindex = _sindex;
			cindex = _cindex;
			deltaV = _deltaV;
			maxExpansion = _maxExpansion;
			maxExpansion2 = maxExpansion * maxExpansion;

			betaalphama = alphama * betama;

			Device device = Constants.openclDevice;
//			System.out.println("Using openCL device: " + Constants.openclDevice.getDeviceId());
			Range range = device.createRange(betaalphama * alphama);
			execute(range);
//			execute(Range.create(betaalphama * alphama));
		}

	}

	final static class FloatExponentialHarmonicODF extends Kernel {

		float[] AlmunS, AlmunC, deltaV;
		float resolutionR, pi25g;
		int[] ml2, nl2;
		int expansionDegree, alphama, betama, betaalphama, lindex, sindex, cindex, maxExpansion, maxExpansion2;
		float[] coefficient;
		float[] odf;

		public final float getDTesseralFunction(int l, int m, int n,
		                                         float alpha, float beta, float gamma) {

			float cx0, cx1;
			float i1 = (MoreMath.odd(m + n)) ? -1 : 1;
			float i2 = (MoreMath.odd(l)) ? -1 : 1;
			int mabs = m > 0 ? m : -m ;
			int nabs = n > 0 ? n : -n ;

			float arg1 = mabs * alpha + nabs * gamma;
			float arg2 = mabs * alpha - nabs * gamma;

			if (m > 0) {
				if (n > 0) {
					cx0 = cos(arg1);
					cx1 = cos(arg2);
				} else if (n < 0) {
					cx0 = -sin(arg1);
					cx1 = sin(arg2);
				} else {
					cx0 = Constants.sqrt2f * cos(arg1);
					cx1 = 0;
				}
			} else if (m < 0) {
				if (n > 0) {
					cx0 = sin(arg1);
					cx1 = sin(arg2);
				} else if (n < 0) {
					cx0 = cos(arg1);
					cx1 = -cos(arg2);
				} else {
					cx0 = Constants.sqrt2f * sin(arg1);
					cx1 = 0;
				}
			} else {
				if (n > 0) {
					cx0 = Constants.sqrt2f * cos(arg1);
					cx1 = 0;
				} else if (n < 0) {
					cx0 = -Constants.sqrt2f * sin(arg1);
					cx1 = 0;
				} else {
					cx0 = cos(arg1);
					cx1 = 0;
				}
			}

			float result;
			if (cx1 != 0) {
				float df0, df1;
				float dftmp;
				float dbeta = Constants.PIf - beta;

				float arg = Constants.PI_2f * (mabs - nabs);
				dftmp = deltaV[l + mabs * maxExpansion2] * deltaV[l + nabs * maxExpansion2];
				df0 = dftmp * cos(arg);
				df1 = df0;
				for (int i = 1; i <= l; i++) {
					dftmp = 2f * deltaV[l + i * maxExpansion + mabs * maxExpansion2] *
							deltaV[l + i * maxExpansion + nabs * maxExpansion2];
					df0 += dftmp * cos(beta * i - arg);
					df1 += dftmp * cos(dbeta * i - arg);
				}
				result = i1 * cx0 * df0 + i2 * cx1 * df1;
			} else {
				float arg = Constants.PIf * (mabs - nabs) / 2;
				float df = deltaV[l + mabs * maxExpansion2] * deltaV[l + nabs * maxExpansion2] * cos(arg);
				for (int i = 1; i <= l; i++)
					df += 2f * deltaV[l + i * maxExpansion + mabs * maxExpansion2] *
							deltaV[l + i * maxExpansion + nabs * maxExpansion2] * cos(beta * i - arg);
				result = i1 * cx0 * df;
			}
			return result;
		}

		@Override public void run() {

			int index = getGlobalId(0);

/*			for (int ng = 0; ng < alphama; ng++)
				for (int nb = 0; nb < betama; nb++)
					for (int na = 0; na < alphama; na++)
						index = na + nb * alphama + ng * betaalphama;*/

			int ng = index / betaalphama;
			int remaining = index - ng * betaalphama;
			int nb = remaining / alphama;
			int na = remaining - nb * alphama;

			float _odf = 0;
			int k = 0;

			float alpha = Constants.PIf - resolutionR * na + pi25g;
			float beta = resolutionR * nb - pi25g;
			float gamma = Constants.PIf - resolutionR * ng + pi25g;

			for (int l = 2, i = 0; l <= expansionDegree; l += 2, i++)
				for (int m = 1; m <= ml2[i]; m++)
					for (int n = 1; n <= nl2[i]; n++) {
						float Dlmu = 0;
						for (int n1 = -l, i1 = 0; n1 <= l; n1++, i1++) {
							float AlS = AlmunS[i + (n - 1) * lindex + i1 * lindex * sindex];
							if (AlS != 0) {
								float tmplmu = 0;
								for (int m1 = -l, i2 = 0; m1 <= l; m1++, i2++) {
									float AlC = AlmunC[i + (m - 1) * lindex + i2 * lindex * cindex];
									if (AlC != 0)
										tmplmu += AlC * getDTesseralFunction(l, m1, n1, alpha, beta, gamma);
								}
								Dlmu += tmplmu * AlS;
							}
						}
						_odf += coefficient[k++] * Dlmu;
					}

			odf[index] = exp(_odf);

		}

		public void computeODF(float[] _AlmunS, float[] _AlmunC, float _resolutionR, float _pi25g,
		                       int[] _ml2, int[] _nl2, int _expansionDegree, int _alphama, int _betama,
		                       int _lindex, int _sindex, int _cindex, float[] _deltaV, int _maxExpansion,
		                       float[] _coefficient, float[] _odf) {

			AlmunS = _AlmunS;
			AlmunC = _AlmunC;
			resolutionR = _resolutionR;
			pi25g = _pi25g;
			ml2 = _ml2;
			nl2 = _nl2;
			expansionDegree = _expansionDegree;
			alphama = _alphama;
			betama = _betama;
			coefficient = _coefficient;
			odf = _odf;
			lindex = _lindex;
			sindex = _sindex;
			cindex = _cindex;
			deltaV = _deltaV;
			maxExpansion = _maxExpansion;
			maxExpansion2 = maxExpansion * maxExpansion;

			betaalphama = alphama * betama;

			Device device = Device.best();
			Range range = device.createRange(betaalphama * alphama);
			execute(range);
//			execute(Range.create(betaalphama * alphama));
		}

	}

	double[] AlmunS = null;
	double[] AlmunC = null;
	float[] fAlmunS = null;
	float[] fAlmunC = null;
	int lindex, sindex, cindex, lmax2;

	private final void initializeAlmunSC(int lMax, int LGnumberS, int LGnumberC) {
		int nlMax = 0;
		for (int l = 2, i = 0; l <= lMax; l += 2, i++)
			if (nlMax < nl2[i])
				nlMax = nl2[i];
		int mlMax = 0;
		for (int l = 2, i = 0; l <= lMax; l += 2, i++)
			if (mlMax < ml2[i])
				mlMax = ml2[i];

		lindex = lMax / 2;
		sindex = nlMax;
		cindex = mlMax;
		lmax2 = 2 * lMax + 1;
		AlmunS = new double[lindex * sindex * lmax2];
		AlmunC = new double[lindex * cindex * lmax2];
		for (int l = 2, i = 0; l <= lMax; l += 2, i++) {
			for (int vu = 1; vu <= nl2[i]; vu++)
				for (int n = -l; n <= l; n++)
					AlmunS[i + (vu - 1) * lindex + (n + l) * lindex * sindex] = SphericalHarmonics.getAlmum(LGnumberS, l, vu, n);
			for (int mu = 1; mu <= ml2[i]; mu++)
				for (int m = -l; m <= l; m++)
					AlmunC[i + (mu - 1) * lindex + (m + l) * lindex * cindex] = SphericalHarmonics.getAlmum(LGnumberC, l, mu, m);
		}
	}

	private final void initializefAlmunSC(int lMax, int LGnumberS, int LGnumberC) {
		int nlMax = 0;
		for (int l = 2, i = 0; l <= lMax; l += 2, i++)
			if (nlMax < nl2[i])
				nlMax = nl2[i];
		int mlMax = 0;
		for (int l = 2, i = 0; l <= lMax; l += 2, i++)
			if (mlMax < ml2[i])
				mlMax = ml2[i];

		lindex = lMax / 2;
		sindex = nlMax;
		cindex = mlMax;
		lmax2 = 2 * lMax + 1;
		fAlmunS = new float[lindex * sindex * lmax2];
		fAlmunC = new float[lindex * cindex * lmax2];
		for (int l = 2, i = 0; l <= lMax; l += 2, i++) {
			for (int vu = 1; vu <= nl2[i]; vu++)
				for (int n = -l; n <= l; n++)
					fAlmunS[i + (vu - 1) * lindex + (n + l) * lindex * sindex] = (float)
							SphericalHarmonics.getAlmum(LGnumberS, l, vu, n);
			for (int mu = 1; mu <= ml2[i]; mu++)
				for (int m = -l; m <= l; m++)
					fAlmunC[i + (mu - 1) * lindex + (m + l) * lindex * cindex] = (float)
							SphericalHarmonics.getAlmum(LGnumberC, l, mu, m);
		}
	}

	void computeODFFromCoefficients() {

		if (refreshODF) {
			refreshODF = false;

			boolean float_opencl = MaudPreferences.getBoolean("opencl.useFloat", true) && Constants.useOpenCL;

			SphericalHarmonics.initialize();

			float fresolutionR = (float) resolutionR, fpi25g = (float) pi25g;

			int maxExpansion = SphericalHarmonics.maxExpansion;
			int maxExpansion2 = maxExpansion * maxExpansion;
			double[] deltaV = null;
			float[] fdeltaV = null;
			float[] fcoefficient = null;
			int index = 0;
			if (float_opencl) {
				initializefAlmunSC(expansionDegree, sampleSymmetry, LGIndex);
				fcoefficient = new float[coefficient.length];
				for (int i = 0; i < coefficient.length; i++)
					fcoefficient[i] = (float) coefficient[i];
				fdeltaV = new float[maxExpansion * maxExpansion2];
				for (int i = 0; i < maxExpansion; i++)
					for (int j = 0; j < maxExpansion; j++)
						for (int k = 0; k < maxExpansion; k++)
							fdeltaV[index++] = (float) SphericalHarmonics.deltaV[i][j][k];
			} else {
				initializeAlmunSC(expansionDegree, sampleSymmetry, LGIndex);
				deltaV = new double[maxExpansion * maxExpansion2];
				for (int i = 0; i < maxExpansion; i++)
					for (int j = 0; j < maxExpansion; j++)
						for (int k = 0; k < maxExpansion; k++)
							deltaV[index++] = SphericalHarmonics.deltaV[i][j][k];
			}

			double vbg, hvg, hvb, a = 0., b;
			double[] va = new double[alphama * betaalphama];

//	    double fn = 0.0;
			normalizationFactor = 1.0;

			double fnorm = 0.0;
			double roundOffCorrection = 0.0;
			index = 0;
			for (int ng = 0; ng < alphama; ng++) {
				hvg = resolutionR;
				if (ng == 0 || ng == alphama1)
					hvg /= 2;
				for (int nb = 0; nb < betama; nb++) {
					if (nb == 0) {
						a = 0.0;
						b = pi25g;
					} else if (nb == betama1) {
						b = nb * resolutionR;
						a -= pi25g;
					} else {
						a = nb * resolutionR - pi25g;
						b = a + resolutionR;
					}
					hvb = Math.cos(a) - Math.cos(b);
					vbg = hvb * hvg;
					for (int na = 0; na < alphama; na++) {
						if (na == 0 || na == alphama1)
							va[index] = pi25g * vbg;
						else
							va[index] = resolutionR * vbg;
						roundOffCorrection += va[index++];
					}
				}
			}

			if (float_opencl) {
				float[] fodf = new float[alphama * betaalphama];
				(new FloatExponentialHarmonicODF()).computeODF(fAlmunS, fAlmunC, fresolutionR, fpi25g, ml2, nl2,
						expansionDegree, alphama, betama, lindex, sindex, cindex, fdeltaV, maxExpansion, fcoefficient, fodf);
				index = 0;
				for (int ng = 0; ng < alphama; ng++)
					for (int nb = 0; nb < betama; nb++)
						for (int na = 0; na < alphama; na++) {
							odf[index] = fodf[index];
							fnorm += va[index] * odf[index++];
						}
			} else {
				if (Constants.useOpenCL)
					(new ExponentialHarmonicODF()).computeODF(AlmunS, AlmunC, resolutionR, pi25g, ml2, nl2,
							expansionDegree, alphama, betama, lindex, sindex, cindex, deltaV, maxExpansion, coefficient, odf);
				else
					computeODF();

				index = 0;
				for (int ng = 0; ng < alphama; ng++)
					for (int nb = 0; nb < betama; nb++)
						for (int na = 0; na < alphama; na++)
							fnorm += va[index] * odf[index++];
			}

//    double fnormTheoretical = Constants.PI * 8. * Constants.PI / fnorm;
			normalizationFactor = roundOffCorrection / fnorm;
			index = 0;
			for (int ng = 0; ng < alphama; ng++)
				for (int nb = 0; nb < betama; nb++)
					for (int na = 0; na < alphama; na++)
						odf[index++] *= normalizationFactor;

		}

//	  System.out.println("Normalization factor: " + Fmt.format(fnormTheoretical));
	}

	public double[] calculatePFbyTubeProjection(double[][] thetaphi,
                                              double sthi, double cthi, double fhir, int inv) {
/* Local variables */
    double ffak, ang, ca2, cb2, sa2,
            dist_a, dist_b, dist_g, real_dist, wgtcell;
    int nfis, stepa, stepb, stepg, maxa, maxb, maxg;
    int[] tmp_index = new int[3];

/*     Calculation of a complete reduced pole figure
       INPUT FIO given in the whole G-space OUTPUT POLREF=FS */

//    boolean negODFout = false;
//    if (Constants.testing)
//    	negODFout = MaudPreferences.getBoolean("debug.negativeODFout", false);

    cb2 = cthi;
    int nfismax = (int) (Constants.PI2 / integrationStepPFR + 1.000001);

    int numberOfPoints = thetaphi[0].length;
    double[] fs = new double[numberOfPoints];

    int[] referenceCounter = new int[numberOfPoints];
    int[] references = new int[numberOfPoints];
    int[] pointReference = new int[numberOfPoints];

    pointReference[0] = 0;
    references[0] = 0;
    int numberOfReferences = 0;
    for (int n = 0; n < numberOfPoints; n++) {
      //     thetaphi[0][n] *= Constants.DEGTOPI;
      //     thetaphi[1][n] *= Constants.DEGTOPI;
      boolean isNewReference = true;
      for (int nscan = 0; nscan < numberOfReferences; nscan++)
        if (Math.abs(thetaphi[0][references[nscan]] - thetaphi[0][n]) < 0.0002) {
          isNewReference = false;
          referenceCounter[pointReference[references[nscan]]]++;
          pointReference[n] = pointReference[references[nscan]];
          break;
        }
      if (isNewReference) {
        references[numberOfReferences] = n;
        pointReference[n] = numberOfReferences;
        referenceCounter[numberOfReferences]++;
        numberOfReferences++;
      }
    }

    int[] finalPointReference = new int[numberOfReferences];
    finalPointReference[0] = referenceCounter[0];
    references[0] = 0;
    for (int n = 1; n < numberOfReferences; n++) {
      references[n] = finalPointReference[n - 1];
      finalPointReference[n] = finalPointReference[n - 1] + referenceCounter[n];
    }

    for (int n = 0; n < numberOfPoints; n++) {
      referenceCounter[references[pointReference[n]]] = n;
      references[pointReference[n]]++;
    }

    int startingPoint = 0;

    double[] angles = new double[3];
    int[] iaindex = new int[3];
    double[] gridAngles = new double[3];

    int maxRepeat = 2 - inv;
    int[][] cellIndex1 = new int[maxRepeat][nfismax];
    int[][] cellIndex2 = new int[maxRepeat][nfismax];
    int[][] cellstepb = new int[maxRepeat][nfismax];
    int[][] cellstepg = new int[maxRepeat][nfismax];
    int[][] cellmaxb = new int[maxRepeat][nfismax];
    int[][] cellmaxg = new int[maxRepeat][nfismax];
    double[][] phiRef = new double[maxRepeat][nfismax];
    double[][] cellDistb = new double[maxRepeat][nfismax];
    double[][] cellDistg = new double[maxRepeat][nfismax];
    double[] g2rv = new double[maxRepeat];
    double[] cb2v = new double[maxRepeat];
    g2rv[0] = Constants.PI - fhir;
    while (g2rv[0] < 0.)
      g2rv[0] += Constants.PI2;
    cb2v[0] = cb2;
    for (int iu = 1; iu < maxRepeat; iu++) {
      cb2v[iu] = -cb2;
      g2rv[iu] = g2rv[0] - Constants.PI;
      while (g2rv[iu] < 0.)
        g2rv[iu] += Constants.PI2;
    }

    for (int nref = 0; nref < numberOfReferences; nref++) {
      // evaluate the first new theta value, all the other change only in phi
      int n1 = referenceCounter[startingPoint];
      fs[n1] = 0.;

/* Projection thread loop, Simpson integration */
      double cr = Math.cos(thetaphi[0][n1]);
      double sr = Math.sin(thetaphi[0][n1]);
      for (nfis = 0; nfis < nfismax; nfis++) {
        ang = nfis * integrationStepPFR;
        ca2 = -Math.cos(ang);
        sa2 = Math.sin(ang);
        double ffak1 = 0.0;
        for (int repeat = 0; repeat < maxRepeat; repeat++) {
          Angles.g20g100(angles, ca2, sa2, cb2v[repeat], sthi, cr, sr);
          phiRef[repeat][nfis] = angles[0];
          angles[0] += thetaphi[1][n1];
          angles[2] += g2rv[repeat];
          getIndicesNoCheckR(angles, iaindex);
          getAnglesR(iaindex, gridAngles);
          cellIndex1[repeat][nfis] = iaindex[1];
          cellIndex2[repeat][nfis] = iaindex[2];
          if (gridAngles[0] > angles[0]) {
            stepa = -1;
            maxa = -2;
          } else if (gridAngles[0] == angles[0]) {
            stepa = 1;
            maxa = 1;
          } else {
            stepa = 1;
            maxa = 2;
          }
          if (gridAngles[1] > angles[1]) {
            stepb = -1;
            maxb = -2;
          } else if (gridAngles[1] == angles[1]) {
            stepb = 1;
            maxb = 1;
          } else {
            stepb = 1;
            maxb = 2;
          }
          if (gridAngles[2] > angles[2]) {
            stepg = -1;
            maxg = -2;
          } else if (gridAngles[2] == angles[2]) {
            stepg = 1;
            maxg = 1;
          } else {
            stepg = 1;
            maxg = 2;
          }

          double orDista = -stepa * (gridAngles[0] - angles[0]);
          double orDistb = -stepb * (gridAngles[1] - angles[1]);
          double orDistg = -stepg * (gridAngles[2] - angles[2]);
          cellstepb[repeat][nfis] = stepb;
          cellmaxb[repeat][nfis] = maxb;
          cellstepg[repeat][nfis] = stepg;
          cellmaxg[repeat][nfis] = maxg;
          cellDistb[repeat][nfis] = orDistb;
          cellDistg[repeat][nfis] = orDistg;

          ffak = 0.0;
          double wgtot = 0.0;
//					int cellNumber = 0;
          for (int ia = 0; ia != maxa; ia += stepa) {
            dist_a = ia * resolutionR * stepa - orDista;
            dist_a *= dist_a;
            for (int ib = 0; ib != maxb; ib += stepb) {
              dist_b = ib * resolutionR * stepb - orDistb;
              dist_b *= dist_b;
              for (int ig = 0; ig != maxg; ig += stepg) {
                dist_g = ig * resolutionR * stepg - orDistg;
                real_dist = dist_a + dist_b + dist_g * dist_g;
                if (real_dist <= dist_factor2) {
                  if (real_dist < 1.0E-4)
                    real_dist = 1.0E-4;
                  wgtcell = 1.0 / Math.sqrt(real_dist);
//									wgtcell = ((dist_factor - Math.sqrt(real_dist)) / dist_factor);
//									wgtcell = *= wgtcell;
                  tmp_index[0] = ia + iaindex[0];
                  tmp_index[1] = ib + iaindex[1];
                  tmp_index[2] = ig + iaindex[2];
                  ffak += getODF(tmp_index) * wgtcell;
                  wgtot += wgtcell;
                }
              }
            }
          }
          if (wgtot == 0.0)
            ffak1 += getODF(iaindex);
          else
            ffak1 += ffak / wgtot;
        }
        if (0 < nfis && nfis < nfismax - 1) {
          if (MoreMath.odd(nfis + 1))
            ffak1 *= 2;
          else
            ffak1 *= 4;
        }
        fs[n1] += ffak1;
      }

      fs[n1] *= pisimg / maxRepeat;

      startingPoint++;
      for (int n = startingPoint; n < finalPointReference[nref]; n++) {
        // referenceCounter[n] is the point to evaluate
        n1 = referenceCounter[n];
        fs[n1] = 0.;

        /* Projection thread loop, Simpson integration */
        for (nfis = 0; nfis < nfismax; nfis++) {
          double ffak1 = 0.0;
          for (int repeat = 0; repeat < maxRepeat; repeat++) {
            double anglesPhi = phiRef[repeat][nfis] + thetaphi[1][n1];
            iaindex[0] = (int) ((anglesPhi + pi25g) / resolutionR + .000001);
            double anglesGridPhi = iaindex[0] * resolutionR;

            iaindex[1] = cellIndex1[repeat][nfis];
            iaindex[2] = cellIndex2[repeat][nfis];

            anglesGridPhi = anglesGridPhi - anglesPhi;
            if (anglesGridPhi > 0.0) {
              stepa = -1;
              maxa = -2;
            } else if (anglesGridPhi == 0.0) {
              stepa = 1;
              maxa = 1;
            } else {
              stepa = 1;
              maxa = 2;
            }

            stepb = cellstepb[repeat][nfis];
            maxb = cellmaxb[repeat][nfis];
            stepg = cellstepg[repeat][nfis];
            maxg = cellmaxg[repeat][nfis];
            double orDista = -stepa * anglesGridPhi;
            double orDistb = cellDistb[repeat][nfis];
            double orDistg = cellDistg[repeat][nfis];

            ffak = 0.0;
            double wgtot = 0.0;
            for (int ia = 0; ia != maxa; ia += stepa) {
              dist_a = ia * resolutionR * stepa - orDista;
              dist_a *= dist_a;
              for (int ib = 0; ib != maxb; ib += stepb) {
                dist_b = ib * resolutionR * stepb - orDistb;
                dist_b *= dist_b;
                for (int ig = 0; ig != maxg; ig += stepg) {
                  dist_g = ig * resolutionR * stepg - orDistg;
                  real_dist = dist_a + dist_b + dist_g * dist_g;
                  if (real_dist <= dist_factor2) {
                    if (real_dist < 1.0E-4)
                      real_dist = 1.0E-4;
                    wgtcell = 1.0 / Math.sqrt(real_dist);
//										wgtcell = ((dist_factor - Math.sqrt(real_dist)) / dist_factor);
//										wgtcell *= wgtcell;
                    tmp_index[0] = ia + iaindex[0];
                    tmp_index[1] = ib + iaindex[1];
                    tmp_index[2] = ig + iaindex[2];
                    ffak += getODF(tmp_index) * wgtcell;
                    wgtot += wgtcell;
                  }
                }
              }
            }
            if (wgtot == 0.0)
              ffak1 += getODF(iaindex);
            else
              ffak1 += ffak / wgtot;
          }
          if (0 < nfis && nfis < nfismax - 1) {
            if (MoreMath.odd(nfis + 1))
              ffak1 *= 2.;
            else
              ffak1 *= 4.;
          }
          fs[n1] += ffak1;
        }

        fs[n1] *= pisimg / maxRepeat;

      }
      startingPoint = finalPointReference[nref];  // next one
    }

/*                                          Normalization to PINPF */
//		System.out.println(fs);
    return fs;
  } /* calpolo_ */

/*  public void getIndicesR(double[] angles, int[] index) {

//		if (alpha > Constants.PI2 - pi25g)
//			alpha -= Constants.PI2;

    index[0] = (int) ((angles[0] + pi25g) / resolutionR + .000001);
    index[1] = (int) ((angles[1] + pi25g) / resolutionR + .000001);
    index[2] = (int) ((angles[2] + pi25g) / resolutionR + .000001);

    applyCrystalSymmetryAndCheck(index);

  }*/

  public void applyCrystalSymmetryAndCheck(int[] index) {

//    int fold, lphiturn;

    if (index[1] >= alphama)
      index[1] -= alphama1;
    if (index[1] < 0)
      index[1] += alphama1;
    if (index[1] >= betama) {
      index[1] = alphama1 - index[1];
      index[0] += betama1;
      index[2] += betama1;
    }


    if (index[0] >= alphama1)
      index[0] -= alphama1;
    if (index[0] < 0)
      index[0] += alphama1;

    if (index[2] >= alphama)
      index[2] -= alphama1;
    if (index[2] < 0)
      index[2] += alphama1;

    if (mdb != 0) {
//  For GB = Dn : Symmetry element C2XB
//                PI+ALPHA, PI-BETA, 2PI-GAMMA

      if (index[1] >= nbe) {
        index[1] = betama1 - index[1];
        index[2] = alphama1 - index[2];
        index[0] += betama1;
      }
      if (index[0] >= alphama1)
        index[0] -= alphama1;
    }

    while (index[2] >= nge)
      index[2] -= nge - 1;

// ODF space general properties
// beta = 0

    if (index[1] == 0) {
      index[0] += index[2];
      index[2] = 0;
      if (index[0] >= alphama1)
        index[0] -= alphama1;
      if (index[0] < 0)
        index[0] += alphama1;
    }

// beta = PI

    if (index[1] == betama1) {
      index[0] -= index[2];
      index[2] = 0;
      if (index[0] >= alphama1)
        index[0] -= alphama1;
      if (index[0] < 0)
        index[0] += alphama1;
    }

//  GAMMA REGION 0-360 degrees IS COMPLETE

// Sample symmetry
//    if (getSampleSymmetryValue() == 11)
//      index[0] = 0;
/*    switch (getSampleSymmetryValue()) {
/ *			case 1:
			case 2:
			case 3:
				fold = multiplicity + 1;
				lphiturn = (alphama1) / fold;
        while (index[0] > lphiturn)
          index[0] -= lphiturn;
				break;
			case 4:
				fold = 6;
				lphiturn = (alphama1) / fold;
        while (index[0] > lphiturn)
          index[0] -= lphiturn;
				break;
			case 5:
        if (index[0] >= (alphama1) / 2)
          index[0] = (alphama1) - index[0];
				break;
			case 6: // orthorhombic
        if (index[0] < (alphama1) / 4)
          break;
        else if (index[0] <= (alphama1) / 2)
					index[0] = (alphama1) / 2 - index[0];
        else if (index[0] <= (alphama1) * 3 / 4)
					index[0] = index[0] - (alphama1) / 2;
        else
          index[0] = (alphama1) - na;
				break;* /
      case 7:
        index[0] = 0;
        break;
      default:
        {
        }
    } */
  }

  public void getAnglesR(int[] alpha, double[] index) {

    index[0] = alpha[0] * resolutionR;
    index[1] = alpha[1] * resolutionR;
    index[2] = alpha[2] * resolutionR;


  }

  public void getIndicesNoCheckR(double[] angles, int[] index) {

//		if (alpha > Constants.PI2 - pi25g)
//			alpha -= Constants.PI2;

    index[0] = (int) ((angles[0] + pi25g) / resolutionR + .000001);
    index[1] = (int) ((angles[1] + pi25g) / resolutionR + .000001);
    index[2] = (int) ((angles[2] + pi25g) / resolutionR + .000001);

  }

  public double getODF(int[] index) {
//    applyCrystalSymmetryLightCheck(index);
    applyCrystalSymmetryAndCheck(index);
    return odf[index[0] + index[1] * alphama + index[2] * betaalphama];
  }

  public double getParameterMinSignificantValue(int i) {
    return 0;
  }
}
