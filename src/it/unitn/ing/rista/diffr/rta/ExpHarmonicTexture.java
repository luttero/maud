/*
 * @(#)ExpHarmonicTexture.java created Dec 15, 2006 Casalino
 *
 * Copyright (c) 2006 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

/**
 * The ExpHarmonicTexture is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Dec 15, 2006 1:44:33 PM $
 * @since JDK1.1
 */
public class ExpHarmonicTexture extends HarmonicTexture {

  public static final String identifierS = "Exponential Harmonic";
  public static final String IDlabelS = "Exponential Harmonic";
  public static final String descriptionS = "select this to apply exponential Harmonic model of Van Houtte";

  double odf[][][] = null;
  boolean refreshODF = true;
  static boolean tubeProjection = MaudPreferences.getBoolean("expHarmonic.tubeProjection", true);

  public ExpHarmonicTexture(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = identifierS;
    IDlabel = IDlabelS;
    description = descriptionS;
  }

  public ExpHarmonicTexture(XRDcat aobj) {
    this(aobj, identifierS + " method");
  }

  public ExpHarmonicTexture(String[] labels) {
    this();
    if (labels != null) {
      if (labels.length > 1) {
        identifier = labels[0];
        IDlabel = labels[1];
      }
      if (labels.length > 2)
        description = labels[2];
    }
  }

  public ExpHarmonicTexture() {
    identifier = identifierS;
    IDlabel = IDlabelS;
    description = descriptionS;
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);
  }

/*  public void refreshForNotificationUp(XRDcat source, int reason) {
    refreshComputation = true;
    if (source == this) {
      refreshODF = true;
    }
  }*/

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);
    refreshODF = true;
  }

  void computeODF() {
    initializeAll();
  }

  public int getLaueGroupNumber() {
    return SpaceGroups.getLGNumberSiegfriedConv(getPhase().getPointGroup());
  }

  void computeODFFromCoefficients() {

    if (refreshODF) {
      refreshODF = false;
      Misc.println("Compute ODF!");
    double vbg = 0.0, va = 0.0, hvg = 0., hvb = 0., a = 0., b = 0.;
    double fn = 0.0;
    normalizationFactor = 1.0;

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
          odf[na][nb][ng] = getODF(na * resolutionR - pi25g, nb * resolutionR - pi25g,
              ng * resolutionR - pi25g);
          fnorm += odf[na][nb][ng] * va;
          roundOffCorrection += va;
        }

      }
    }

//    double fnormTheoretical = Constants.PI * 8. * Constants.PI / fnorm;
    normalizationFactor = roundOffCorrection / fnorm;
      for (int ng = 0; ng < alphama; ng++)
        for (int nb = 0; nb < betama; nb++)
          for (int na = 0; na < alphama; na++)
            odf[na][nb][ng] *= normalizationFactor;

    }

//	  Misc.println("Normalization factor: " + Fmt.format(fnormTheoretical));
  }

  void odfNormalization() {

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


//	  Misc.println("Normalization factor: " + Fmt.format(fnormTheoretical));
  }

  public double[] computeTextureFactor(double[][] texture_angles,
                                       double[] sctf, double fhir, int inv) {
    if (tubeProjection)
      return calculatePFbyTubeProjection(texture_angles, sctf[0], sctf[1], fhir, inv);
    else
      return calculatePF(texture_angles, sctf[0], sctf[1], fhir, inv);
  }

  int actualRunningThreads = 0;
  int datafile = 0;
  double textF[][] = null;
  double[] cdsc = null;
  int LaueGroupSnumber = 0;
  double resolution = 5.0;
  double resolutionR = resolution * Constants.DEGTOPI;
  double pi25g = resolutionR / 2.;
  public static double integrationStepPF = 1.0; // MaudPreferences.getDouble(Texture.prefs[1], Texture.prefVal[1]);
  public static double integrationStepPFR = integrationStepPF * Constants.DEGTOPI;
  public static int nfismax = (int) (Constants.PI2 / integrationStepPFR + 1.00001);
  public static double pisimg = integrationStepPFR / (6.0 * Constants.PI) / 2;
  int alphama = 0, betama = 0;
  int alphama1 = 0, betama1 = 0;
  double normalizationFactor = 1.0;
  int nge = 0, nbe = 0, nae = 0;
  double[] odfMaxAngles = {360.0, 180.0, 360.0};
  double[] odfMaxAnglesR = {Constants.PI2, Constants.PI, Constants.PI2};
  int rotationFold = 1;
  int mdb = 0;
  double dist_factor2 = 1.0; //Math.pow(3, 2/3);

  public void initializeAll() {
    tubeProjection = MaudPreferences.getBoolean("expHarmonic.tubeProjection", true);
    boolean newODF = false;
    if (LaueGroupSnumber != getLaueGroupNumber() || odf == null && tubeProjection) {
      LaueGroupSnumber = getLaueGroupNumber();
      refreshODF = true;
      newODF = true;
    }
    applySymmetryRules();
    alphama = Uwimvuo.getAlphamax(resolution);
    betama = (alphama + 1) / 2;
    alphama1 = alphama - 1;
    betama1 = betama - 1;
    if (newODF)
      odf = new double[alphama][betama][alphama];
    if (refreshODF && tubeProjection)
      computeODFFromCoefficients();
    integrationStepPF = MaudPreferences.getDouble(Texture.prefs[1], 0.0);
    if (integrationStepPF <= 0.0)
      integrationStepPF = resolution / 2.0;
    integrationStepPFR = integrationStepPF * Constants.DEGTOPI;
    nfismax = (int) (Constants.PI2 / integrationStepPFR + 1.00001);
//    Misc.println(nfismax);
    pisimg = integrationStepPFR / (6.0 * Constants.PI);
    if (!tubeProjection)
      odfNormalization();
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

  }

  public void computeTextureFactor(final Phase aphase, final Sample asample) {

    if (!refreshComputation)
      return;

	  recomputedTextureFactor(aphase, asample, true);
  }

	public double[][] recomputedTextureFactor(final Phase aphase, final Sample asample, final boolean setValues) {

		initializeAll();

		cdsc = aphase.lattice();

		int hkln = aphase.gethklNumber();

		int totdatafile = 0;
		for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
			for (int j = 0; j < asample.getActiveDataSet(i).activedatafilesnumber(); j++)
				totdatafile += asample.getActiveDataSet(i).getActiveDataFile(j).positionsPerPattern;
		}

		double[][] textF = new double[totdatafile][hkln];

		final int maxThreads = Math.min(Constants.maxNumberOfThreads, hkln / 10);
		int checkNumber = totdatafile * hkln;
		if (maxThreads > 1 && checkNumber > 1000 &&
				Constants.threadingGranularity >= Constants.MEDIUM_GRANULARITY) {
			if (Constants.debugThreads)
				System.out.println("Thread discrete texture computation " + getLabel());
			int i;
			PersistentThread[] threads = new PersistentThread[maxThreads];
			for (i = 0; i < maxThreads; i++) {
				final int totData = totdatafile;
				final double[][] textFt = textF;
				threads[i] = new PersistentThread(i) {
					@Override
					public void executeJob() {
						int i1 = this.getJobNumberStart();
						int i2 = this.getJobNumberEnd();

						for (int ij = i1; ij < i2; ij++) {
							double texAngle[][] = new double[2][totData];

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
//								  for (int ppp = 0; ppp < adatafile.positionsPerPattern; ppp++) {
									double texture_angles[] = adatafile.getTextureAngles(positions[ij][0]);
									texAngle[0][idatafile] = (texture_angles[0] * Constants.DEGTOPI);
									texAngle[1][idatafile] = (texture_angles[1] * Constants.DEGTOPI);
									idatafile++;
//								  }
								}
							}
							double[] texFactor = computeTextureFactor(texAngle, sctf, fhir, inv);
							synchronized(asample) {
								int index = 0;
								for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
									int datafilenumber = asample.getActiveDataSet(i).activedatafilesnumber();
									for (int ij1 = 0; ij1 < datafilenumber; ij1++) {
										textFt[index][ij] = texFactor[index];
										index++;
									}
								}
							}
						}

					}
				};
			}
			i = 0;
			int istep = (int) (0.9999 + hkln / maxThreads);
			for (int j = 0; j < maxThreads; j++) {
				int is = i;
				if (j < maxThreads - 1)
					i = Math.min(i + istep, hkln);
				else
					i = hkln;
				threads[j].setJobRange(is, i);
				threads[j].start();
			}
			boolean running;
			do {
				running = false;
				try {
					Thread.sleep(Constants.timeToWaitThreadsEnding);
				} catch (InterruptedException r) {
				}
				for (int h = 0; h < maxThreads; h++) {
					if (!threads[h].isEnded())
						running = true;
				}
			} while (running);

		} else {
			for (int ij = 0; ij < hkln; ij++) {
				double texAngle[][] = new double[2][totdatafile];

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
//					  for (int ppp = 0; ppp < adatafile.positionsPerPattern; ppp++) {
						double texture_angles[] = adatafile.getTextureAngles(positions[ij][0]);
						texAngle[0][idatafile] = (texture_angles[0] * Constants.DEGTOPI);
						texAngle[1][idatafile] = (texture_angles[1] * Constants.DEGTOPI);
						idatafile++;
//					  }
					}
				}
				double[] texFactor = computeTextureFactor(texAngle, sctf, fhir, inv);
				idatafile = 0;
				for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
					int datafilenumber = asample.getActiveDataSet(i).activedatafilesnumber();
					for (int ij1 = 0; ij1 < datafilenumber; ij1++) {
						textF[idatafile][ij] = texFactor[idatafile];
						idatafile++;
					}
				}
			}
		}
		if (setValues) {
			int idatafile = 0;
			for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
				int datafilenumber = asample.getActiveDataSet(i).activedatafilesnumber();
				for (int ij1 = 0; ij1 < datafilenumber; ij1++) {
					DiffrDataFile adatafile = asample.getActiveDataSet(i).getActiveDataFile(ij1);
					adatafile.setTextureFactors(aphase, textF[idatafile++]);
				}
			}
		}

		return textF;
	}

	public double[] computeTextureFactor(Phase aphase, double[][] alphabeta, Reflection reflex) {

//    int numberOfPoints = alphabeta.length/2;

    initializeAll();

    double[] cdsc = aphase.lattice();

//    float phoninp = subfmin();

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
      textureAngles[i][0] = texture_angles[i] * Constants.DEGTOPI;
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
      textureAngles[i][0] = texture_angles[i] * Constants.DEGTOPI;
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

//    float phoninp = subfmin();

//		int hkln = aphase.gethklNumber();

    double[] sctf = Uwimvuo.tfhkl(h, k, l, cdsc[7], cdsc[5], cdsc[3], cdsc[6], cdsc[0], cdsc[1]);
    double fhir = Math.acos(sctf[3]);
    int inv = Uwimvuo.equiv(LaueGroupSnumber, sctf);

    double[][] texture_angles = new double[2][numberofPoints * numberofPoints];
    boolean[] included = new boolean[numberofPoints * numberofPoints];

    double x, y, r;
    double dxy = 2.0 * maxAngle / numberofPoints;

//          Misc.println(2.0 * Math.asin(maxAngle / Constants.sqrt2) * Constants.PITODEG);
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
//          Misc.println(texture_angles[0] + " " + texture_angles[1]);

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

  public double[] calculatePF(double[][] thetaphi,
                              double sthi, double cthi, double fhir, int inv) {
/* Local variables */
    double ang;
    int nfis;
    double ca2, cb2, sa2;

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

        /* Projection thread loop, Simpson integration */
        for (nfis = 0; nfis < nfismax; nfis++) {
          double ffak1 = 0.0;
          for (int repeat = 0; repeat < maxRepeat; repeat++) {
            angles[0] = phiRef[repeat][nfis] + thetaphi[1][n1];
            angles[1] = angle1[repeat][nfis];
            angles[2] = angle2[repeat][nfis];

//						float f_odf = getODF(iaindex); // Luca_last1202 f[findex[0]][findex[1]][findex[2]];
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

/*                                          Normalization to PINPF */
//		Misc.println(fs);
    return fs;
  }

  public double getODF(double alpha, double beta, double gamma) {
    double odf = super.getODF(alpha, beta, gamma);
    return Math.exp(odf - 1.0) / normalizationFactor;
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
//		Misc.println(fs);
    return fs;
  } /* calpolo_ */

  public void getIndicesR(double[] angles, int[] index) {

//		if (alpha > Constants.PI2 - pi25g)
//			alpha -= Constants.PI2;

    index[0] = (int) ((angles[0] + pi25g) / resolutionR + .000001);
    index[1] = (int) ((angles[1] + pi25g) / resolutionR + .000001);
    index[2] = (int) ((angles[2] + pi25g) / resolutionR + .000001);

    applyCrystalSymmetryAndCheck(index);

  }

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
    return odf[index[0]][index[1]][index[2]];
  }

}
