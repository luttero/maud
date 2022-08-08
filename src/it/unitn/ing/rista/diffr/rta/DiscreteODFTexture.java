/*
 * @(#)DiscreteODFTexture.java created Jun 14, 2003 Berkeley
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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
import it.unitn.ing.rista.io.cif.*;
import it.unitn.ing.rista.awt.Utility;

import java.io.*;
import java.awt.*;
import java.util.*;


/**
 * The DiscreteODFTexture is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.18 $, $Date: 2006/12/04 14:30:05 $
 * @since JDK1.1
 */

public class DiscreteODFTexture extends Texture {

  public static int symmetrychoicenumber = 8;

	public static final int ODF_FORMAT_UNKNOW = -1;
	public static final int ODF_FORMAT_STANDARD = 0;
	public static final int ODF_FORMAT_SIEGFRIED_ALPHA = 1;
	public static final int ODF_FORMAT_SIEGFRIED_GAMMA = 2;
	public static final int ODF_FORMAT_SECTIONS = 3;
	public static final int ODF_FORMAT_BEARTEX = 4;
	public static final int ODF_FORMAT_MAUD = 5;

//  boolean odfnotLoaded = true;
  double odf[][][] = null;
  boolean odf_covered[][][] = null;
  public int nge = 0, nbe = 0, nae = 0;
  int alphama = 0, betama = 0;
  int alphama1 = 0, betama1 = 0;
  double[] cdsc = null;
  int LaueGroupSnumber = 0;
  public double[] odfMaxAngles = {360.0, 180.0, 360.0};
  public double[] odfMaxAnglesR = {Constants.PI2, Constants.PI, Constants.PI2};
  double resolution = 1.0;
  double resolutionR = resolution * Constants.DEGTOPI;
  double pi25g = resolutionR / 2.;
  public static double integrationStepPF = 1.0; // MaudPreferences.getDouble(Texture.prefs[1], Texture.prefVal[1]);
  public static double integrationStepPFR = integrationStepPF * Constants.DEGTOPI;
  public static int nfismax = (int) (Constants.PI2 / integrationStepPFR + 1.00001);
  public static double pisimg = integrationStepPFR / (6.0 * Constants.PI) / 2;
  boolean fromPF = false;
  int[][][][] hklPF = null;
  int[] izoveriPF = null;
  int[][] allhklmult = null;
  double[][] weightSingle = null;
  double[][][] textureAngles = null;
//  double[][] experimentalPoleFigures = null;
  int[] numberOfPFPoint = null;
  public int rotationFold = 1;
  public int mdb = 0;
  int maxizoveri = 1;
  public int sampleSymmetryValue = 0;
  int[] poleFigureIndex;
  int numberOfPoleFigureIzoveri = 0;
  boolean needToRotate = false;
  double[] anglesToRotate = new double[3];
  int[] multAngles = new int[3];

  public DiscreteODFTexture(XRDcat obj, String alabel) {
    super(obj, alabel);
  }

  public DiscreteODFTexture() {
  }

  public DiscreteODFTexture(XRDcat afile) {
    this(afile, "DiscreteODFTexture x");
  }

  public boolean needIntensityExtractor() {
    return ODFisRefinable();
  }

  public boolean ODFisRefinable() {
    // to implement in subclasses
    return true;
  }

  public int getLaueGroupNumber() {
    return SpaceGroups.getLGNumberSiegfriedConv(getPhase().getPointGroup());
  }

  public void sharpODF(double exponent) {
    if (odf == null) // || odfnotLoaded)
      return;
      for (int ng = 0; ng < alphama; ng++) {
        for (int nb = 0; nb < betama; nb++) {
          for (int na = 0; na < alphama; na++) {
            odf[na][nb][ng] = Math.pow(odf[na][nb][ng], exponent);
          }
        }
      }
  }

	public void rotateODFBy(double alpha, double beta, double gamma, int multAlpha, int multBeta, int multGamma) {
		if (odf == null) {// || odfnotLoaded)
			needToRotate = true;
			anglesToRotate[0] = alpha;
			anglesToRotate[1] = beta;
			anglesToRotate[2] = gamma;
			multAngles[0] = multAlpha;
			multAngles[1] = multBeta;
			multAngles[2] = multGamma;
			return;
		}
		System.out.println("Legagy ODF, rotate by: " + alpha + " " + beta + " " + gamma);
		textureInitialization();
		double[][][] odfOr = new double[alphama][betama][alphama];
		for (int ng = 0; ng < alphama; ng++) {
			for (int nb = 0; nb < betama; nb++) {
				for (int na = 0; na < alphama; na++) {
					odfOr[na][nb][ng] = odf[na][nb][ng];
				}
			}
		}

		// rotate by alpha
		if (alpha != 0 || multAlpha != 1) {
			int cellStep = (int) (alpha / resolution);
			for (int ng = 0; ng < alphama; ng++) {
				for (int nb = 0; nb < betama; nb++) {
					for (int na = 0; na < alphama; na++) {
						int na1 = multAlpha * na + cellStep;
						while (na1 >= alphama)
							na1 -= alphama;
						while (na1 < 0)
							na1 += alphama;
						odfOr[na1][nb][ng] = odf[na][nb][ng];
					}
				}
			}
		}

		// rotate by beta
		if (beta != 0 || multBeta != 1) {
			int cellStep = (int) (beta / resolution);
			for (int ng = 0; ng < alphama; ng++) {
				for (int nb = 0; nb < betama; nb++) {
					int nb1 = nb + cellStep;
					if (nb1 >= betama)
						nb1 -= betama;
					for (int na = 0; na < alphama; na++) {
						odfOr[na][nb1][ng] = odf[na][nb][ng];
					}
				}
			}
		}

		// rotate by gamma
		if (gamma != 0 || multGamma != 1) {
			int cellStep = (int) (gamma / resolution);
			for (int ng = 0; ng < alphama; ng++) {
				int ng1 = multGamma * ng + cellStep;
				while (ng1 >= alphama)
					ng1 -= alphama;
				while (ng1 < 0)
					ng1 += alphama;
				for (int nb = 0; nb < betama; nb++) {
					for (int na = 0; na < alphama; na++) {
						odfOr[na][nb][ng1] = odf[na][nb][ng];
					}
				}
			}
		}

		for (int ng = 0; ng < alphama; ng++) {
			for (int nb = 0; nb < betama; nb++) {
				for (int na = 0; na < alphama; na++) {
					odf[na][nb][ng] = odfOr[na][nb][ng];
				}
			}
		}
	}

	public void writeCustomObject(BufferedWriter out) {

    if (odf == null) // || odfnotLoaded)
      return;

    try {
      out.newLine();
      out.write("#custom_object_" + "odf");
      out.newLine();
      out.write(CIFdictionary.loopDecl);
      out.newLine();
      out.write(CIFdictionary.odf_values);
      out.newLine();
      for (int ng = 0; ng < nge; ng++) {
        for (int nb = 0; nb < nbe; nb++) {
          for (int na = 0; na < nae; na++) {
            float value = (float) odf[na][nb][ng];
            out.write(Float.toString(value) + " ");
          }
          out.newLine();
        }
        out.newLine();
      }
      out.newLine();
      out.write("#end_custom_object_" + "odf");
      out.newLine();
      out.newLine();
    } catch (IOException ioe) {
      System.out.println("Error in writing the odf for " + toXRDcatString());
    }

  }

  public void readCustomObject(CIFtoken ciffile) {
    // to be override by subclasses
    // the default read and do nothing
    int tokentype;
    boolean endofInput = false;
    int aindex = 0, bindex = 0, gindex = 0;
    resetODF();

    try {
      do {
        tokentype = ciffile.nextToken();
        switch (tokentype) {
          case CIFtoken.TT_CIFE:
            // should be the CIF entry for odf values
/*							// CIF item
						thecife = new String(ciffile.thestring);
						newtoken = ciffile.nextToken();
//							System.out.println(ciffile.thestring);
						if (FilePar.isValidToken(newtoken)) {
							theobj = setSubordinateField(thecife, ciffile.thestring);
							endofInput = true;
						}
						else {
							ciffile.pushBack();
//								System.out.println("Pushback: " + ciffile.thestring);
						}*/
            break;
          case CIFtoken.TT_LOOP:
            // start the loop for the values here
            aindex = 0;
            bindex = 0;
            gindex = 0;
            break;
          case CIFtoken.TT_NUMBER:
            odf[aindex][bindex][gindex] = ciffile.thevalue;
            if (odf[aindex][bindex][gindex] >= 0 && odf[aindex][bindex][gindex] != 1.0)
              odf_covered[aindex][bindex][gindex] = true;
            aindex++;
            if (aindex == nae) {
              bindex++;
              aindex = 0;
            }
            if (bindex == nbe) {
              gindex++;
              bindex = 0;
            }
            break;
          case CIFtoken.TT_CUSTOM_END:
            // subordinate loop
            endofInput = true;
            break;
          default:
            {
            }
        }
      } while (tokentype != CIFtoken.TT_EOF && !endofInput);
    } catch (IOException ioe) {
      System.out.println("IO exception in custom object for " + toXRDcatString());
    }

    fiottu();
    if (needToRotate) {
    	rotateODFBy(anglesToRotate[0], anglesToRotate[1], anglesToRotate[2], multAngles[0], multAngles[1], multAngles[2]);
    	needToRotate = false;
    }
/*		if (theobj != null)
			theobj.readall(ciffile);*/
  }

  public void loadOdfFromFile() {
    String filename = new String(getPhase().getFilePar().getDirectory() +
        getPhase().toXRDcatString() + ".odf");
    ODFinputStandard(filename);
  }

  public void setResolution(String value) {
  }

  public double[][][] getODF() {
    return odf;
  }

  public void resetODF() {

    textureInitialization();
    odf = new double[alphama][betama][alphama];
    odf_covered = new boolean[alphama][betama][alphama];
//    System.out.println("ODF reset");

    for (int ng = 0; ng < alphama; ng++)
      for (int nb = 0; nb < betama; nb++)
        for (int na = 0; na < alphama; na++) {
          odf[na][nb][ng] = 1.0f;
          odf_covered[na][nb][ng] = false;
        }

          //odfnotLoaded = false;
  }

  public void sharpenODF(double treshold) {

    if (odf == null)
      return;

    for (int ng = 0; ng < alphama; ng++)
      for (int nb = 0; nb < betama; nb++)
        for (int na = 0; na < alphama; na++)
          if (odf[na][nb][ng] <= treshold && odf[na][nb][ng] >= 0.0)
            odf[na][nb][ng] = 0.0f;

    //odfnotLoaded = false;
  }

  public void initializeReflexes(Sample asample) {

    FilePar aparFile = getFilePar();
    if (!aparFile.isTextureComputationPermitted())
      return;

	  Phase aphase = getPhase();
	  int numberReflexes = aphase.gethklNumber();
    if (numberReflexes <= 0) {
      System.out.println("Warning: no reflection to initialize for discrete texture methods!");
      return;
    }
    int numberDataSets = asample.activeDatasetsNumber();

    Reflection[] refl = new Reflection[numberReflexes];
    double intensity = 0;
    double maxInt = 0.0;

    cdsc = aphase.lattice();

    for (int k = 0; k < numberReflexes; k++) {
      refl[k] = aphase.getReflex(k);

/*	    boolean isInRange = false;
	    for (int i = 0; i < numberDataSets; i++) {
		    DataFileSet dataset = asample.getActiveDataSet(i);
		    if (dataset != null) {
			    for (int j = 0; j < dataset.activedatafilesnumber(); j++) {
				    boolean check = dataset.getActiveDataFile(j).checkinRangeandIntensity(aphase, k);
				    if (check)
					    isInRange = true;
			    }
		    }
	    }
	    refl[k].goodforStructureFactor = isInRange;  // todo this should go elsewhere
	    refl[k].goodforStrain = isInRange;  // todo this should go elsewhere
	    refl[k].goodforTexture = isInRange; */
    }

	  double meanintensity[] = new double[numberReflexes];
    int[] datasetCounts = new int[numberReflexes];

    for (int j = 0; j < numberReflexes; j++) {
      meanintensity[j] = 0.0;
      datasetCounts[j] = 0;
    }

    maxInt = 0;

    for (int i = 0; i < numberDataSets; i++) {
      DataFileSet adataset = asample.getActiveDataSet(i);
      Instrument ainstrument = adataset.getInstrument();

      int datafilenumber = adataset.activedatafilesnumber();
      for (int j = 0; j < numberReflexes; j++) {
        intensity = 0.0;
        int number = 0;
        boolean doneOnce = false;
	      double structureFactor = adataset.getStructureFactors(aphase)[1][j];
	      for (int i1 = 0; i1 < datafilenumber; i1++) {
          DiffrDataFile adatafile = adataset.getActiveDataFile(i1);
		      double[][] position = adatafile.getPositions(aphase)[j];
		      for (int k = 0; k < adatafile.positionsPerPattern; k++) {
			      for (int l = 0; l < position[0].length; l++) {
				      if (adatafile.xInsideRange(position[k][l])) {
					      double correction = /*adatafile.computeAbsorptionAndPhaseQuantity(ainstrument, asample, getPhase(), position[j]) */
							      adatafile.computeAngularIntensityCorrection(asample, ainstrument, position[k][l]) * adatafile.getShapeAbsFactors(aphase, j)[k][l] *
									      adatafile.getLorentzPolarization(aphase, j)[k][l];
					      if (!Double.isNaN(correction)) {
						      intensity += structureFactor * correction; //Fhkl[j] *
						      number++;
						      if (!doneOnce) {
							      doneOnce = true;
							      datasetCounts[j]++;
						      }
					      }
				      }
			      }
		      }
        }
        if (number > 0)
          meanintensity[j] += intensity / number;
//        else
//          meanintensity[j] = 0.0;
      }
    }
    for (int j = 0; j < numberReflexes; j++) {
      if (datasetCounts[j] > 0)
        meanintensity[j] /= datasetCounts[j];
//      System.out.println("Mean Intensity ("+ j + ") = " + meanintensity[j]);
      if (meanintensity[j] > maxInt)
        maxInt = meanintensity[j];
      // int izoveri = refl[j].izoveri;
      refl[j].izoveri = 1;
      // for (int i = 0; i < izoveri; i++)
      refl[j].setWeight(meanintensity[j]);
    }
//    maxInt /= numberDataSets;

    double referencedspace = refl[0].d_space;

    double delta = -1.0;
    if (getWIMVstatus())
      delta = MaudPreferences.getDouble("overlappedReflection.relativeDelta", 1.0E-4);

    int referenceReflex = 0;
    int izoveri = 1;

    for (int j = 1; j < numberReflexes; j++) {
      double dspace = refl[j].d_space;

      if (Math.abs((dspace - referencedspace) / referencedspace) < delta) {
        refl[j].setnoGoodforTexture();
        refl[j].izoveri = 1;
        izoveri++;
        meanintensity[referenceReflex] += meanintensity[j];
      } else {
        refl[referenceReflex].setOverlapNumber(izoveri);
        for (int i = 0; i < izoveri; i++) {
        	if (meanintensity[referenceReflex] > 0)
          refl[referenceReflex + i].setOverlappedWeight(refl[referenceReflex + i].getWeight() /
              meanintensity[referenceReflex]);
        	else
	         refl[referenceReflex + i].setOverlappedWeight(1.0 / izoveri);
          //         System.out.println();
        }
        referencedspace = dspace;
        izoveri = 1;
        referenceReflex = j;
      }

    }
    refl[referenceReflex].setOverlapNumber(izoveri);
    for (int i = 0; i < izoveri; i++) {
	    if (meanintensity[referenceReflex] > 0)
         refl[referenceReflex + i].setOverlappedWeight(refl[referenceReflex + i].getWeight() /
          meanintensity[referenceReflex]);
	    else
		    refl[referenceReflex + i].setOverlappedWeight(1.0 / izoveri);
    }

    double minPermitted = getMinimumIntensityD();
    double minDspacing =  getMinimumDspacingD();
    System.out.println(" Phase : " + getPhase().toXRDcatString());
    System.out.println("Minimumum intensity accepted (% of max) : " + minPermitted);
    System.out.println("Maximum intensity : " + maxInt);
    System.out.println(" - Pole figures used - Total number = " + numberReflexes);
    int number = 0;
    poleFigureIndex = new int[numberReflexes];
    numberOfPoleFigureIzoveri = 0;
    for (int i = 0; i < numberReflexes; i++) {
      if (minPermitted > meanintensity[i] / maxInt || minDspacing > refl[i].d_space) {
        refl[i].setnoGoodforTexture();
        System.out.println("Reflection not used : (" + refl[i].getH() + " " + refl[i].getK() + " " + refl[i].getL() + ") , intensity : " +
            meanintensity[i]);
      }
      if (refl[i].isGoodforTexture()) {
        poleFigureIndex[number++] = i;
        numberOfPoleFigureIzoveri += refl[i].izoveri;
        System.out.println("Reflection used: (" + refl[i].getH() + " " + refl[i].getK() + " " + refl[i].getL() + ") , intensity : " +
            meanintensity[i]);
        if (refl[i].izoveri > 1) {
          System.out.println("    It has superposed reflections : ");
          for (int k = 0; k < refl[i].izoveri; k++)
            System.out.println("         " + k + " - (" + refl[i + k].getH() + " " + refl[i + k].getK() + " " + refl[i + k].getL() +
                ") , overlapping weight: " + refl[i + k].getOverlappedWeight());
        }
      }
    }
    numberPoleFigures = number;
  }

  public double getMinimumIntensityD() {
    return 0.02; // to be overwrited by subclasses
  }

  public double getMinimumDspacingD() {
    return 0.0;
  }

/*  public int getNumberExperimentalPoints(Reflection refl, Sample asample) {
    return asample.getNumberActiveDatafiles()
        * getSampleSymmetryMultiplicity();
  }*/

  public int textureInitialization() {
    LaueGroupSnumber = getLaueGroupNumber();
    sampleSymmetryValue = computeSampleSymmetryValue();
//    System.out.println(LaueGroupSnumber + " - " + sampleSymmetryValue);
    resolution = getResolutionD();
    resolutionR = resolution * Constants.DEGTOPI;
    pi25g = resolutionR / 2.;
    integrationStepPF = MaudPreferences.getDouble(Texture.prefs[1], Double.parseDouble(Texture.prefVal[1]));
    if (integrationStepPF <= 0.0)
      integrationStepPF = resolution / 2.0;
	  if (integrationStepPF > 1.0)
		  integrationStepPF = 1.0;
    integrationStepPFR = integrationStepPF * Constants.DEGTOPI;
    nfismax = (int) (Constants.PI2 / integrationStepPFR + 1.00001);
//    System.out.println(nfismax);
    pisimg = integrationStepPFR / (6.0 * Constants.PI);
    return computeParameterNumber();
  }

  public int computeParameterNumber() {

    alphama = Uwimvuo.getAlphamax(resolution);
    betama = (alphama + 1) / 2;
    alphama1 = alphama - 1;
    betama1 = betama - 1;
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

    return nae * nbe * nge;
  }

  public double subfmin() {

    double fmin = 10000000000.f;
    for (int ng = 0; ng < nge; ng++)
      for (int nb = 0; nb < nbe; nb++)
        for (int na = 0; na < nae; na++)
          if (odf[na][nb][ng] < fmin && odf[na][nb][ng] >= 0.0f)
            fmin = odf[na][nb][ng];
    return fmin;
  }

  public void loadPFandComputeODF(Frame aframe) {
    try {
      String filename = Utility.openFileDialog(aframe, "Open PF file (Beartex or CIF format)", FileDialog.LOAD,
          MaudPreferences.getPref(FilePar.analysisPath, Constants.documentsDirectory),
          null, "");
      if (filename != null) {
        resetODF();
        fromPF = true;
        textureAngles = null;
        Vector expPF = poleFigureInput(filename); //, numberOfPFPoint, textureAngles);
        numberPoleFigures = (expPF.size() - 1) / 4;
//			System.out.println("Number of PFs, init : " + numberPoleFigures);
//			System.out.println("Number of PFs, init : " + getPoleFigureNumber());

        int[] izoveriv = (int[]) expPF.elementAt(expPF.size() - 1);
        maxizoveri = izoveriv[0];

        Phase thephase = getPhase();
        int hklnumber = thephase.gethklNumber();
        int maxEq = 1;
        for (int i = 0; i < hklnumber; i++) {
          Reflection reflex = thephase.getReflex(i);
          int mult = reflex.multiplicity / 2;
          if (mult > maxEq)
            maxEq = mult;
        }

        hklPF = new int[3][maxEq][numberPoleFigures][maxizoveri];
        allhklmult = new int[numberPoleFigures][maxizoveri];
        weightSingle = new double[numberPoleFigures][maxizoveri];
        izoveriPF = new int[numberPoleFigures];

//        double res = getResolutionD();
//        int alphamax = getAlphamax(res);
//        int betamax = (getBetamax(res) - 1) / 2 + 1;
        int totmax = 0; // (alphamax-1) * betamax;
        for (int i = 0; i < numberPoleFigures; i++) {
          int totpf = 0;
          totpf = numberOfPFPoint[i];
          if (totpf > totmax)
            totmax = totpf;
        }

        double[][] experimentalPF = new double[numberPoleFigures][totmax];

//            int numberIzoveri = 0;
        int monoclinicAxis = 0; // TODO : fix for monoclinic
        int igbl = SpaceGroups.getLGNumberSiegfriedConv(thephase.getPointGroup());
        if (igbl == 2)
          monoclinicAxis = thephase.getMonoclinicAxis();
        igbl = 0; // temporarly to disable it
        int i1 = 0, i2 = 1, i3 = 2;
        switch (monoclinicAxis) {
          case 1:
            i1 = 1;
            i2 = 2;
            i3 = 0;
            break;
          case 2:
            i1 = 2;
            i2 = 0;
            i3 = 1;
            break;
          default:
            {
            }
        }

        for (int i = 0; i < numberPoleFigures; i++) {
          int[][] hkl = (int[][]) expPF.elementAt(i * 4);
          double[] weight = (double[]) expPF.elementAt(i * 4 + 1);
          izoveriPF[i] = weight.length;
          for (int j = 0; j < izoveriPF[i]; j++) {
            weightSingle[i][j] = weight[j];
//            System.out.println(numberIzoveri + " " + weight[j] + " " + hkl[0][j] +" "+ hkl[1][j]+" "+ hkl[2][j]);
//            numberIzoveri++;
          }
          for (int k = 0; k < izoveriPF[i]; k++) {
            Reflection reflex = thephase.getReflectionByAnyhkl(hkl[0][k], hkl[1][k], hkl[2][k]);
            if (reflex != null) {
              allhklmult[i][k] = reflex.multiplicity;
              int mult2 = reflex.multiplicity / 2;
              for (int ij = 0; ij < mult2; ij++) {
                hklPF[0][ij][i][k] = hkl[i1][k];
                hklPF[1][ij][i][k] = hkl[i2][k];
                hklPF[2][ij][i][k] = hkl[i3][k];
              }
            } else {
              allhklmult[i][k] = 1;
              hklPF[0][0][i][k] = hkl[i1][k];
              hklPF[1][0][i][k] = hkl[i2][k];
              hklPF[2][0][i][k] = hkl[i3][k];
            }
          }
          double[] pfInt = (double[]) expPF.elementAt(i * 4 + 3);
//          int j = 0;
//          int k = 0;

          for (int k1 = 0; k1 < pfInt.length; k1++) {
            experimentalPF[i][k1] = pfInt[k1];
//            System.out.print(pfInt[k1] + " ");
          }
//          System.out.println("");

        }
        computeTextureFromPF(experimentalPF);
        fromPF = false;
      }
    } catch (Throwable to) {
      to.printStackTrace();
      System.out.println("Error catched, check it out or send it to maud@ing.unitn.it");
      System.out.println("Your command was not completed successfully!");
    }
  }

  public void computeTextureFromPF(double[][] experimentalPF) {
  }

  public Vector poleFigureInput(String filename) {

/*    Phase thephase = getPhase();
    int monoclinicAxis = 0;
    int igbl = SpaceGroups.getLGNumberSiegfriedConv(thephase);
    if (igbl == 2)
      monoclinicAxis = thephase.getMonoclinicAxis();
    int i1 = 0, i2 = 1, i3 = 2;
    switch (monoclinicAxis) {
      case 1:
        i1 = 1; i2 = 2; i3 = 0;
        break;
      case 2:
        i1 = 2; i2 = 0; i3 = 1;
        break;
      default:{}
    }*/

    Sample asample = getFilePar().getSample(0);
    double[] sampleAngles = asample.getSampleAngles();
//    double[] expInt = null;
    int[][] hkli = null;
    double[] weight = null;
    Vector expPF = null;
    String token = null;
    int maxizoveri_local = 1;

    if (filename.toLowerCase().endsWith(".xrdml")) {
      XRDMLPoleFigureReader xreader = new XRDMLPoleFigureReader();
      xreader.readFile(Misc.getDataBufferedInputStream(filename));
      Vector polesAndAngles = xreader.getPoleFigure();

      int numberPoleFiguresPF = polesAndAngles.size() / 2;
      numberOfPFPoint = new int[numberPoleFiguresPF];
      int totmax = 0;
      for (int i = 0; i < numberPoleFiguresPF; i++) {
        double[][] pf = (double[][]) polesAndAngles.elementAt(i * 2 + 1);
        numberOfPFPoint[i] = pf[0].length;
        if (numberOfPFPoint[i] > totmax)
          totmax = numberOfPFPoint[i];
      }
      textureAngles = new double[2][numberPoleFiguresPF][totmax];
      expPF = new Vector(0, 1);
      for (int i = 0; i < numberPoleFiguresPF; i++) {
        double[] thetaAndRes = new double[3];
        thetaAndRes[0] = 0.0;
        thetaAndRes[1] = 90.0;
        thetaAndRes[2] = 5.0;
        weight = new double[1];
        weight[0] = 1.0;
        hkli = (int[][]) polesAndAngles.elementAt(i * 2);
        double[] pfInt = new double[numberOfPFPoint[i]];
        double[][] pf = (double[][]) polesAndAngles.elementAt(i * 2 + 1);
        for (int j = 0; j < numberOfPFPoint[i]; j++) {
          textureAngles[0][i][j] = pf[0][j] + sampleAngles[1];
          textureAngles[1][i][j] = pf[1][j] + sampleAngles[2];
          pfInt[j] = pf[2][j];
        }
        expPF.addElement(hkli);
        expPF.addElement(weight);
        expPF.addElement(thetaAndRes);
        expPF.addElement(pfInt);
      }
      int[] izoveriv = new int[1];
      izoveriv[0] = maxizoveri_local;
      expPF.addElement(izoveriv);
    } else {
      BufferedReader PFreader = Misc.getReader(filename);
      if (PFreader != null) {
        try {

          int izoveriCheck = 0;
          int izoveri = 1;
          hkli = new int[3][1];
          weight = new double[1];
          weight[0] = 1.0;

          String line = PFreader.readLine();
          StringTokenizer st = null;
          if (filename.toLowerCase().endsWith(".apf")) {
            // texture weights Maud export format

            line = PFreader.readLine();
            st = new StringTokenizer(line, "' ,\t\r\n");
            int totmax = 0;
            int numberPoleFiguresPF = Integer.valueOf(st.nextToken()).intValue();
            numberOfPFPoint = new int[numberPoleFiguresPF];
            Vector allPFs = new Vector(numberPoleFiguresPF * 2, 1);
            boolean mistake = false;
            for (int i = 0; i < numberPoleFiguresPF; i++) {
              int[] hkl = new int[3];
              if (!mistake)
                line = PFreader.readLine();
              mistake = false;
//              System.out.println("Reading line: " + line);
              st = new StringTokenizer(line, "' ,\t\r\n");
              hkl[0] = Integer.valueOf(st.nextToken()).intValue();
              hkl[1] = Integer.valueOf(st.nextToken()).intValue();
              hkl[2] = Integer.valueOf(st.nextToken()).intValue();
              line = PFreader.readLine();
//              System.out.println("Reading line 2: " + line);
              st = new StringTokenizer(line, "' ,\t\r\n");
              numberOfPFPoint[i] = Integer.valueOf(st.nextToken()).intValue();
              Vector poleFig = new Vector(numberOfPFPoint[i], 1);
              int effectiveNumber = 0;
              for (int j = 0; j < numberOfPFPoint[i]; j++) {
                line = PFreader.readLine();
                if (line == null || line.contains("H K L")) {
                  mistake = true;
                  break;
                }
//                System.out.println("Reading line data: " + line);
                st = new StringTokenizer(line, "' ,\t\r\n");
                double[] pf = new double[4];
                pf[0] = Double.valueOf(st.nextToken()).doubleValue();
                pf[1] = Double.valueOf(st.nextToken()).doubleValue();
                pf[2] = Double.valueOf(st.nextToken()).doubleValue();
                if (st.hasMoreTokens()) st.nextToken();
                if (st.hasMoreTokens()) st.nextToken();
                if (st.hasMoreTokens())
                  pf[3] = Double.valueOf(st.nextToken()).doubleValue();
                poleFig.add(pf);
                effectiveNumber++;
              }
              numberOfPFPoint[i] = effectiveNumber;
              if (effectiveNumber > totmax)
                totmax = effectiveNumber;
              allPFs.add(hkl);
              allPFs.add(poleFig);
            }
            textureAngles = new double[2][numberPoleFiguresPF][totmax];
            expPF = new Vector(0, 1);
            for (int i = 0; i < numberPoleFiguresPF; i++) {
              double[] thetaAndRes = new double[3];
              thetaAndRes[0] = 0.0;
              thetaAndRes[1] = 90.0;
              thetaAndRes[2] = 5.0;
              hkli = new int[3][1];
              weight = new double[1];
              weight[0] = 1.0;
              int[] hkl = (int[]) allPFs.elementAt(i * 2);
              for (int i1 = 0; i1 < 3; i1++)
                hkli[i1][0] = hkl[i1];
              double[] pfInt = new double[numberOfPFPoint[i]];
              Vector poleFig = (Vector) allPFs.elementAt(i * 2 + 1);
              for (int j = 0; j < numberOfPFPoint[i]; j++) {
                double[] pf = (double[]) poleFig.elementAt(j);
                textureAngles[0][i][j] = pf[0] + sampleAngles[1];
                textureAngles[1][i][j] = pf[1] + sampleAngles[2];
                pfInt[j] = pf[2];
              }
              expPF.addElement(hkli);
              expPF.addElement(weight);
              expPF.addElement(thetaAndRes);
              expPF.addElement(pfInt);
            }
            maxizoveri_local = 1;
          } else if (line.toLowerCase().startsWith("data_") || line.startsWith("_")) {
            // cif format
            Vector allPFs = new Vector(0, 1);
            while (line != null) {
              int[] hkl = new int[3];
              while (line != null && !line.toLowerCase().startsWith("loop_")) {
//          	System.out.println(linedata);
                st = new StringTokenizer(line, "' ,\t\r\n");

                while (st.hasMoreTokens()) {
                  token = st.nextToken();
                  if (token.equalsIgnoreCase("_diffrn_refln_index_h")) {
                    token = st.nextToken();
                    hkl[0] = Integer.valueOf(token).intValue();
                  } else if (token.equalsIgnoreCase("_diffrn_refln_index_k")) {
                    token = st.nextToken();
                    hkl[1] = Integer.valueOf(token).intValue();
                  } else if (token.equalsIgnoreCase("_diffrn_refln_index_l")) {
                    token = st.nextToken();
                    hkl[2] = Integer.valueOf(token).intValue();
                  }
                }
                line = PFreader.readLine();
              }

//						System.out.println("Reading pole figure: " + hkl[0] + " " + hkl[1] + " " + hkl[2]);

              int index = -1;
              int chiIndex = -1, phiIndex = -1, intensityIndex = -1, sigmaIndex = -1;

              line = PFreader.readLine();
              while (line != null && line.toLowerCase().startsWith("_")) {
//          	System.out.println(line);
                st = new StringTokenizer(line, "' ,\t\r\n");
                while (st.hasMoreTokens()) {
                  index++;
                  token = st.nextToken();
                  if (token.equalsIgnoreCase("_diffrn_refln_angle_chi") ||
                      token.equalsIgnoreCase("_pd_meas_angle_chi")) {
                    chiIndex = index;
                  } else if (token.equalsIgnoreCase("_diffrn_refln_angle_phi") ||
                      token.equalsIgnoreCase("_pd_meas_angle_phi")) {
                    phiIndex = index;
                  } else if (token.equalsIgnoreCase("_diffrn_refln_intensity_net") ||
                      token.equalsIgnoreCase("_pd_meas_intensity_total")) {
                    intensityIndex = index;
                  } else if (token.equalsIgnoreCase("_diffrn_refln_intensity_sigma")) {
                    sigmaIndex = index;
                  }
                }
                line = PFreader.readLine();
              }

//						System.out.println("Reading data for pole figure: " + chiIndex + " " + phiIndex + " " + intensityIndex);

              Vector poleFig = new Vector(10, 10);
              while (line != null && !line.toLowerCase().startsWith("_")
                  && !line.toLowerCase().startsWith("data_")) {
//          	System.out.println(line);
                st = new StringTokenizer(line, "' ,\t\r\n");

                index = 0;
                boolean found = false;
                double[] pf = new double[4];
                pf[3] = 1.0;
                while (st.hasMoreTokens()) {
                  token = st.nextToken();
                  if (index == chiIndex) {
                    pf[0] = Double.valueOf(token).doubleValue();
                  } else if (index == phiIndex) {
                    pf[1] = Double.valueOf(token).doubleValue();
                  } else if (index == intensityIndex) {
                    pf[2] = Double.valueOf(token).doubleValue();
                    found = true;
                    if (sigmaIndex == -1) {
                      if (pf[3] > 0.0)
                        pf[3] = 1.0 / pf[2];
                      else
                        pf[3] = 1.0;
                    }
                  } else if (index == sigmaIndex) {
                    pf[3] = Double.valueOf(token).doubleValue();
/*									if (pf[3] > 0.0)
										pf[3] = 1.0 / pf[3];
									else
										pf[3] = 0.0;*/
                  }
                  index++;
                }
                if (found) {
                  poleFig.add(pf);
//						System.out.println("Adding to pole figure: " + pf[0] + " " + pf[1] + " " + pf[2]);
                }
                line = PFreader.readLine();
              }
              allPFs.add(hkl);
              allPFs.add(poleFig);
            }

            int totmax = 0;
            int numberPoleFiguresPF = allPFs.size() / 2;
            numberOfPFPoint = new int[numberPoleFiguresPF];
            for (int i = 0; i < numberPoleFiguresPF; i++) {
//						int[] hkl = (int[]) allPFs.elementAt(i * 2);
              Vector poleFig = (Vector) allPFs.elementAt(i * 2 + 1);
              numberOfPFPoint[i] = poleFig.size();
              if (numberOfPFPoint[i] > totmax)
                totmax = numberOfPFPoint[i];
            }
            textureAngles = new double[2][numberPoleFiguresPF][totmax];
            expPF = new Vector(0, 1);
            for (int i = 0; i < numberPoleFiguresPF; i++) {
              double[] thetaAndRes = new double[3];
              thetaAndRes[0] = 0.0;
              thetaAndRes[1] = 90.0;
              thetaAndRes[2] = 5.0;
              hkli = new int[3][1];
              weight = new double[1];
              weight[0] = 1.0;
              int[] hkl = (int[]) allPFs.elementAt(i * 2);
              for (int i1 = 0; i1 < 3; i1++)
                hkli[i1][0] = hkl[i1];
              double[] pfInt = new double[numberOfPFPoint[i]];
              Vector poleFig = (Vector) allPFs.elementAt(i * 2 + 1);
              for (int j = 0; j < numberOfPFPoint[i]; j++) {
                double[] pf = (double[]) poleFig.elementAt(j);
                textureAngles[0][i][j] = pf[0] + sampleAngles[1];
                textureAngles[1][i][j] = pf[1] + sampleAngles[2];
                pfInt[j] = pf[2];
                weight[0] = pf[3];
              }
              expPF.addElement(hkli);
              expPF.addElement(weight);
              expPF.addElement(thetaAndRes);
              expPF.addElement(pfInt);
            }
            maxizoveri_local = 1;
          } else if (filename.toLowerCase().endsWith(".gpf")) {
            Vector allPFs = new Vector(3, 3);
            line = PFreader.readLine();
            while (line != null) {

              Vector poleFig = new Vector(10, 10);

              System.out.println("Reading: " + line);
              st = new StringTokenizer(line, " ,\t\r\n");
              hkli = new int[3][1];
              int index = 1;
              for (int i = 0; i < 3; i++) {
//              token = st.nextToken();
//              hkli[i][0] = Integer.valueOf(token).intValue();
                hkli[i][0] = Integer.valueOf(line.substring(index, index + 1)).intValue();
                index++;
//              System.out.println(hkli[i][0]);
              }
              double[] thetaAndRes = new double[3];
/*            for (int i = 0; i < 3; i++) {
              token = st.nextToken();
              thetaAndRes[i] = Double.valueOf(token).doubleValue();
            }*/
              thetaAndRes[0] = 0.0;
              thetaAndRes[1] = 90.0;
              thetaAndRes[2] = 5.0;

              allPFs.addElement(hkli);
//            double weightTot = 0.0;
              izoveri = 1;
              weight = new double[izoveri];
              for (int i = 0; i < izoveri; i++)
                weight[i] = 1.0;
              allPFs.addElement(weight);
//            expPF.addElement(thetaAndRes);

              double res = thetaAndRes[2];

              int alphamax = Uwimvuo.getAlphamax(res);
              int betamax = (Uwimvuo.getBetamax(res) - 1) / 2 + 1;
              int totData = 0;
              int a = 0;
              int b = 0;
              do {
                line = PFreader.readLine();
                for (int i = 1; i < 72; i += 4) {
                  if (totData < (alphamax - 1) * betamax) {
                    String subData = line.substring(i, i + 4);
                    totData++;
//                System.out.println(i + " " + subData);
                    double pfvalue = Double.valueOf(Misc.toStringDeleteBlank(subData)).doubleValue() / 100.0;
                    double theta = b * res; //(totData / (alphamax - 1)) * res;
                    double phi = a * res;
                    if (theta >= thetaAndRes[0] && theta <= thetaAndRes[1]) {
                      double[] pf = new double[3];
                      pf[0] = theta;
                      pf[1] = phi;
                      pf[2] = pfvalue;
//                    System.out.println(theta + " " + phi + " " + pfvalue);
                      poleFig.add(pf);
                    }
                    a++;
                    if (a >= alphamax - 1) {
                      a = 0;
                      b++;
                    }
                  }
                }
              } while (totData < (alphamax - 1) * betamax);
              allPFs.add(poleFig);
              line = PFreader.readLine();
              line = PFreader.readLine();

// next pole figure
              line = PFreader.readLine();
            }

            int totmax = 0;
            int numberPoleFiguresPF = allPFs.size() / 3;
            numberOfPFPoint = new int[numberPoleFiguresPF];
            for (int i = 0; i < numberPoleFiguresPF; i++) {
//						int[] hkl = (int[]) allPFs.elementAt(i * 2);
              Vector poleFig = (Vector) allPFs.elementAt(i * 3 + 2);
              numberOfPFPoint[i] = poleFig.size();
              if (numberOfPFPoint[i] > totmax)
                totmax = numberOfPFPoint[i];
            }
            textureAngles = new double[2][numberPoleFiguresPF][totmax];
            expPF = new Vector(0, 1);
            for (int i = 0; i < numberPoleFiguresPF; i++) {
              double[] thetaAndRes = new double[3];
              thetaAndRes[0] = 0.0;
              thetaAndRes[1] = 90.0;
              thetaAndRes[2] = 5.0;
              int[][] hkl = (int[][]) allPFs.elementAt(i * 3);
              double[] weig = (double[]) allPFs.elementAt(i * 3 + 1);
              double[] pfInt = new double[numberOfPFPoint[i]];
              Vector poleFig = (Vector) allPFs.elementAt(i * 3 + 2);
              for (int j = 0; j < numberOfPFPoint[i]; j++) {
                double[] pf = (double[]) poleFig.elementAt(j);
                textureAngles[0][i][j] = pf[0] + sampleAngles[1];
                textureAngles[1][i][j] = pf[1] + sampleAngles[2];
                pfInt[j] = pf[2];
              }
              expPF.addElement(hkl);
              expPF.addElement(weig);
              expPF.addElement(thetaAndRes);
              expPF.addElement(pfInt);
            }
          } else if (filename.toLowerCase().endsWith(".ppf")) {
 //           Vector allPFs = new Vector(3, 3);

            for (int i = 0; i < 4; i++)
              line = PFreader.readLine();
            st = new StringTokenizer(line, " ,\t\r\n");
            int poleFiguresNumber = Integer.valueOf(st.nextToken());
            line = PFreader.readLine();

            int[][] indices = new int[3][poleFiguresNumber];
            double[][] grid = new double[6][poleFiguresNumber];
            double[] cutoff = new double[poleFiguresNumber];
            for (int i = 0; i < poleFiguresNumber; i++) {
              line = PFreader.readLine();
              st = new StringTokenizer(line, " ,\t\r\n");
              st.nextToken();
              grid[0][i] = Double.valueOf(st.nextToken());
              grid[1][i] = Double.valueOf(st.nextToken());
              grid[2][i] = Double.valueOf(st.nextToken());
              grid[3][i] = Double.valueOf(st.nextToken());
              grid[4][i] = Double.valueOf(st.nextToken());
              grid[5][i] = Double.valueOf(st.nextToken());
              st.nextToken();
              indices[0][i] = Integer.valueOf(st.nextToken());
              indices[1][i] = Integer.valueOf(st.nextToken());
              indices[2][i] = Integer.valueOf(st.nextToken());
              st.nextToken();
	            if (st.hasMoreTokens())
                st.nextToken();
              cutoff[i] = grid[1][i];
              if (st.hasMoreTokens())
                cutoff[i] = Double.valueOf(st.nextToken());
            }

            expPF = new Vector(0, 1);

            int totmax = 0;
            numberOfPFPoint = new int[poleFiguresNumber];
            int[] numberOfPFPointReal = new int[poleFiguresNumber];
            for (int i = 0; i < poleFiguresNumber; i++) {
              numberOfPFPoint[i] = (int) ((cutoff[i] - grid[0][i]) / grid[2][i] + 1.01);
              numberOfPFPointReal[i] = (int) ((grid[1][i] - grid[0][i]) / grid[2][i] + 1.01);
              numberOfPFPoint[i] *= (int) (360 / grid[2][i] + 0.00001);
              numberOfPFPointReal[i] *= (int) (360 / grid[2][i] + 0.00001);
              if (numberOfPFPoint[i] > totmax)
                totmax = numberOfPFPoint[i];
            }
            textureAngles = new double[2][poleFiguresNumber][totmax];

            for (int i = 0; i < poleFiguresNumber; i++) {
              double[] thetaAndRes = new double[3];
              thetaAndRes[0] = grid[0][i];
              thetaAndRes[1] = grid[1][i];
              thetaAndRes[2] = cutoff[i];
              int[][] hkl = new int[3][1];
              for (int j = 0; j < 3; j++)
                hkl[j][0] = indices[j][i];

              double[] weig = new double[1];
              weig[0] = 1.0;

              double[] pfInt = new double[totmax];

              System.out.println("Pole figure: " + indices[0][i] + " " + indices[1][i] + " " + indices[2][i]
                                  + ", number of points to read: " + numberOfPFPointReal[i]);

              int j = 0;
              line = PFreader.readLine();
              double alpha = grid[3][i];
              double beta = grid[0][i];
              while (j < numberOfPFPointReal[i] && line != null) {
                st = new StringTokenizer(line, " ,\t\r\n");
                while (st.hasMoreTokens()) {
                  double pfvalue = Double.valueOf(st.nextToken());
//                    System.out.println(beta + " " + alpha + " " + pf[2]);
                  if (j < numberOfPFPoint[i]) {
                    pfInt[j] = pfvalue;
                    textureAngles[0][i][j] = beta;
                    textureAngles[1][i][j] = alpha;
                  }
                  alpha += grid[5][i];
                  if (alpha > grid[4][i]) {
 //                   System.out.println(beta + " " + alpha + " " + pfInt[2][j]);
                    alpha = grid[3][i];
                    beta += grid[2][i];
                  }
                  j++;
                }
                line = PFreader.readLine();
              }

              if (cutoff[i] > 0) {
              expPF.addElement(hkl);
              expPF.addElement(weig);
              expPF.addElement(thetaAndRes);
              expPF.addElement(pfInt);
              }
            }

          } else {
            Vector allPFs = new Vector(3, 3);
            for (int i = 0; i < 4; i++) {
              if (line != null) {
                line = PFreader.readLine();
                if (line != null && !line.equals("")) {
                  st = new StringTokenizer(line, " ,\t\r\n");
                  while (st.hasMoreTokens()) {
                    switch (izoveriCheck) {
                      case 0:
                        izoveri = Integer.valueOf(st.nextToken()).intValue();
                        if (maxizoveri_local < izoveri)
                          maxizoveri_local = izoveri;
                        hkli = new int[3][izoveri];
                        weight = new double[izoveri];
                        izoveriCheck++;
                        break;
                      default:
                        {
                          hkli[0][izoveriCheck - 1] = Integer.valueOf(st.nextToken()).intValue();
                          hkli[1][izoveriCheck - 1] = Integer.valueOf(st.nextToken()).intValue();
                          hkli[2][izoveriCheck - 1] = Integer.valueOf(st.nextToken()).intValue();
                          weight[izoveriCheck - 1] = Double.valueOf(st.nextToken()).doubleValue();
                          izoveriCheck++;
                        }
                    }
                  }
                }
              }
            }

            while (line != null) {

              Vector poleFig = new Vector(10, 10);

              PFreader.readLine();
              line = PFreader.readLine();
              System.out.println("Reading: " + line);
              int repeat = 1;
              int number = 11;
              int[] digits = {4,3,3,5,5,5,5,5,5,2,2};

              String[] data = Misc.readFormattedLine(line, digits, number, repeat);
              st = new StringTokenizer(line, " ,\t\r\n");
              for (int i = 0; i < 3; i++) {
                hkli[i][0] = Integer.valueOf(Misc.toStringDeleteBlankAndTab(data[i])).intValue();
//              System.out.println(hkli[i][0]);
              }
              double[] thetaAndRes = new double[3];
              double[] phiAndRes = new double[3];
              for (int i = 0; i < 3; i++) {
                thetaAndRes[i] = Double.valueOf(Misc.toStringDeleteBlankAndTab(data[i+3])).doubleValue();
                phiAndRes[i] = Double.valueOf(Misc.toStringDeleteBlankAndTab(data[i+6])).doubleValue();
              }

              allPFs.addElement(hkli);
              double weightTot = 0.0;
              for (int i = 0; i < izoveri; i++)
                weightTot += weight[i];
              if (weightTot == 0) {
                for (int i = 0; i < izoveri; i++)
                  weight[i] = 1.0 / izoveri;
              } else
                for (int i = 0; i < izoveri; i++)
                  weight[i] /= weightTot;
              allPFs.addElement(weight);
//            expPF.addElement(thetaAndRes);

              double res = thetaAndRes[2];

              int alphamax = Uwimvuo.getAlphamax(res);
              int betamax = (Uwimvuo.getBetamax(res) - 1) / 2 + 1;
              int totData = 0;
              int a = 0;
              int b = 0;
              double firstPFvalue = 0.0;
              do {
                line = PFreader.readLine();
                for (int i = 1; i < 72; i += 4) {
                  if (totData < alphamax * betamax) {
                    String subData = line.substring(i, i + 4);
                    totData++;
//                System.out.println(i + " " + subData);
                    double pfvalue = Double.valueOf(Misc.toStringDeleteBlank(subData)).doubleValue() / 100.0;
                    double theta = b * res; //(totData / (alphamax - 1)) * res;
                    double phi = a * res;
                    if (a == 0)
                      firstPFvalue = pfvalue;
                    if (theta >= thetaAndRes[0] && theta <= thetaAndRes[1] &&
                        phi >= phiAndRes[0] && phi <= phiAndRes[1] && pfvalue >= 0) {
                      double[] pf = new double[3];
                      pf[0] = theta;
                      pf[1] = phi;
                      pf[2] = pfvalue;
//                    System.out.println(theta + " " + phi + " " + pfvalue);
                      poleFig.add(pf);
                    }
                    a++;
                    if (a == alphamax - 1) {
//                  System.out.println(totData + " " + alphamax + " " + Double.toXRDcatString(firstPFvalue));
                      totData++;
                      theta = b * res; //(totData / (alphamax - 1)) * res;
                      phi = a * res;
                      if (theta >= thetaAndRes[0] && theta <= thetaAndRes[1] && firstPFvalue >= 0) {
                        double[] pf = new double[3];
                        pf[0] = theta;
                        pf[1] = phi;
                        pf[2] = firstPFvalue;
//                    System.out.println(theta + " " + phi + " " + pfvalue);
                        poleFig.add(pf);
                      }
                      a++;
                    }
                    if (a >= alphamax) {
                      a = 0;
                      b++;
                    }
                  }
                }

              } while (totData < alphamax * betamax);
              allPFs.add(poleFig);
              line = PFreader.readLine();

// next pole figure
              line = PFreader.readLine();

              izoveriCheck = 0;
              izoveri = 1;
              hkli = new int[3][1];
              weight = new double[1];
              weight[0] = 1.0;
              for (int i = 0; i < 4; i++) {
                if (line != null) {
                  line = PFreader.readLine();
                  if (line != null && !line.equals("")) {
                    st = new StringTokenizer(line, " ,\t\r\n");
                    while (st.hasMoreTokens()) {
                      switch (izoveriCheck) {
                        case 0:
                          izoveri = Integer.valueOf(token = st.nextToken()).intValue();
                          if (maxizoveri_local < izoveri)
                            maxizoveri_local = izoveri;
                          hkli = new int[3][izoveri];
                          weight = new double[izoveri];
                          izoveriCheck++;
                          break;
                        default:
                          {
                            hkli[0][izoveriCheck - 1] = Integer.valueOf(token = st.nextToken()).intValue();
                            hkli[1][izoveriCheck - 1] = Integer.valueOf(token = st.nextToken()).intValue();
                            hkli[2][izoveriCheck - 1] = Integer.valueOf(token = st.nextToken()).intValue();
                            weight[izoveriCheck - 1] = Double.valueOf(token = st.nextToken()).doubleValue();
                            izoveriCheck++;
                          }
                      }
                    }
                  }
                }
              }

            }

            int totmax = 0;
            int numberPoleFiguresPF = allPFs.size() / 3;
            numberOfPFPoint = new int[numberPoleFiguresPF];
            for (int i = 0; i < numberPoleFiguresPF; i++) {
//						int[] hkl = (int[]) allPFs.elementAt(i * 2);
              Vector poleFig = (Vector) allPFs.elementAt(i * 3 + 2);
              numberOfPFPoint[i] = poleFig.size();
              if (numberOfPFPoint[i] > totmax)
                totmax = numberOfPFPoint[i];
            }
            textureAngles = new double[2][numberPoleFiguresPF][totmax];
            expPF = new Vector(0, 1);
            for (int i = 0; i < numberPoleFiguresPF; i++) {
              double[] thetaAndRes = new double[3];
              thetaAndRes[0] = 0.0;
              thetaAndRes[1] = 90.0;
              thetaAndRes[2] = 5.0;
              int[][] hkl = (int[][]) allPFs.elementAt(i * 3);
              double[] weig = (double[]) allPFs.elementAt(i * 3 + 1);
              double[] pfInt = new double[numberOfPFPoint[i]];
              Vector poleFig = (Vector) allPFs.elementAt(i * 3 + 2);
              for (int j = 0; j < numberOfPFPoint[i]; j++) {
                double[] pf = (double[]) poleFig.elementAt(j);
                textureAngles[0][i][j] = pf[0] + sampleAngles[1];
                textureAngles[1][i][j] = pf[1] + sampleAngles[2];
                pfInt[j] = pf[2];
              }
              expPF.addElement(hkl);
              expPF.addElement(weig);
              expPF.addElement(thetaAndRes);
              expPF.addElement(pfInt);
            }
          }


          int[] izoveriv = new int[1];
          izoveriv[0] = maxizoveri_local;
          expPF.addElement(izoveriv);

          PFreader.close();
        } catch (Throwable io) {
          io.printStackTrace();
          try {
            PFreader.close();
          } catch (Throwable to) {
          }
          System.out.println("Error catched, check it out or send it to maud@ing.unitn.it");
          System.out.println("Your command was not completed successfully!");
        }
      }
    }

    return expPF;
  }

  public void initializeAll() {
    if (odf == null) //odfnotLoaded) {
      loadOdfFromFile();
  }

  public double[][] getPoleFigureGrid(Reflection refl, int numberofPoints, double maxAngle) {

    int h = refl.getH();
    int k = refl.getK();
    int l = refl.getL();

    initializeAll();

    double[][] PFreconstructed = new double[numberofPoints][numberofPoints];

    if (odf == null) {
      System.out.println("Warning: null odf");
      return null;
    }

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
        x = (0.5 + j) * dxy - maxAngle;
        y = (0.5 + i) * dxy - maxAngle;
        r = Math.sqrt(x * x + y * y);
        if (r == 0.0) {
          texture_angles[0][countIncluded] = 0.0f;
          texture_angles[1][countIncluded++] = 0.0f;
          included[count++] = true;
        } else if (r < maxAngle) {
          double phaseAng = Math.atan2(x, y);
          if (phaseAng < 0.0)
            phaseAng += Constants.PI2;
          texture_angles[0][countIncluded] = 2.0 * Math.asin(r / Constants.sqrt2);
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

    double[] textureFactors = computeTextureFactor(new_texture_angles, sctf, fhir, inv, h, k, l);

    count = 0;
    countIncluded = 0;
    for (int i = 0; i < numberofPoints; i++)
      for (int j = 0; j < numberofPoints; j++)
        if (included[count++])
          PFreconstructed[i][j] = textureFactors[countIncluded++];

    return PFreconstructed;
  }

  public double[][] getInversePoleFigureGrid(double[] texture_angles,
                                             double maxPhi, int phiPointNumber,
                                             double maxBeta, int betaPointNumber) {

    initializeAll();

    double[][] PFreconstructed = new double[phiPointNumber][betaPointNumber];

    double[][] textureAngles = new double[2][1];
    for (int i = 0; i < 2; i++)
      textureAngles[i][0] = (double) (texture_angles[i] * Constants.DEGTOPI);
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
        double[] value = computeTextureFactor(textureAngles, sctf, fhir, inv, 0, 0, 0);
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
      double[] value = computeTextureFactor(textureAngles, sctf, fhir, inv, 0, 0, 0);
      PFreconstructed[i] = value[0];
    }

    return PFreconstructed;
  }

  public void ODFoutputStandard(String filename) {

    fiottu();
    Phase aphase = getPhase();
    int[] ne = getCellNumber(LaueGroupSnumber, 5.0);

    BufferedWriter PFwriter = Misc.getWriter(filename);
    if (PFwriter != null) {
      try {

        PFwriter.write(getFilePar().getTitleField() + " - " + aphase.toXRDcatString());
        PFwriter.write(Constants.lineSeparator);
        PFwriter.write(Integer.toString(LaueGroupSnumber) + " " + "5.0");
        PFwriter.write(Constants.lineSeparator);
        PFwriter.write(aphase.getFullCellValue(0) + " " +
            aphase.getFullCellValue(1) + " " +
            aphase.getFullCellValue(2) + " " +
            aphase.getFullCellValue(3) + " " +
            aphase.getFullCellValue(4) + " " +
            aphase.getFullCellValue(5) + " ");
        PFwriter.write(Constants.lineSeparator);
        for (int ng = 0; ng < ne[2]; ng++) {
          double gamma = ng * 5.0;
          if (ng == 0)
            gamma += 1.25;
          if (ng == ne[2] - 1)
            gamma -= 1.25;
          for (int nb = 0; nb < ne[1]; nb++) {
            double beta = nb * 5.0;
            if (nb == 0)
              beta += 1.25;
            if (nb == ne[1] - 1)
              beta -= 1.25;
            for (int na = 0; na < ne[0]; na++) {
              double alpha = na * 5.0;
              if (na == 0)
                alpha += 1.25;
              if (na == ne[0] - 1)
                alpha -= 1.25;
              float value = (float) getODF(alpha * Constants.DEGTOPI, beta * Constants.DEGTOPI,
                  gamma * Constants.DEGTOPI);
              PFwriter.write(Float.toString(value) + " ");
//							PFwriter.write(Fmt.format(odf[na][nb][ng]));
//    					PFwriter.flush();
            }
            PFwriter.write(Constants.lineSeparator);
          }
          PFwriter.write(Constants.lineSeparator);
        }
        PFwriter.flush();
        PFwriter.close();
      } catch (IOException io) {
        try {
          PFwriter.flush();
          PFwriter.close();
        } catch (IOException ieo) {
        }
      }
    }
  }

  public int[] checkODFInputFormat(String filename) {

    BufferedReader PFreader = Misc.getReader(filename);

	  int[] format = new int[2];
	  format[0] = ODF_FORMAT_UNKNOW;
	  format[1] = 0;
	  int index = -1;
    if (PFreader != null) {
      try {
        PFreader.readLine();  // first line, title
	      index++;
        String line = PFreader.readLine();
	      index++;
	      if (Misc.toStringDeleteBlankTabAndEOF(line).length() <= 0) {
		      // ODF_FORMAT_SIEGFRIED ?
		      while (line != null) {
			      line = PFreader.readLine();
			      index++;
			      if (line.startsWith(" ALPHA>")) {
				      format[0] = ODF_FORMAT_SIEGFRIED_GAMMA;
				      index -= 6;
				      line = null;
			      } else if (line.startsWith(" GAMMA>")) {
				      format[0] = ODF_FORMAT_SIEGFRIED_ALPHA;
				      index -= 6;
				      line = null;
			      }
		      }
	      } else {
		      StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");
		      String token = st.nextToken();
		      try {
			      Integer.valueOf(token).intValue();
			      // is a number
			      // ODF_FORMAT_SECTIONS or ODF_FORMAT_STANDARD
			      token = st.nextToken();
			      if (st.hasMoreTokens())
				      format[0] = ODF_FORMAT_SECTIONS;
			      else
				      format[0] = ODF_FORMAT_STANDARD;
		      } catch (Exception e) {
			      // not a number
			      while (line != null) {
				      line = PFreader.readLine();
				      index++;
				      if (line.startsWith(" ALPHA>")) {
					      format[0] = ODF_FORMAT_SIEGFRIED_GAMMA;
					      index -= 6;
					      line = null;
				      } else if (line.startsWith(" GAMMA>")) {
					      format[0] = ODF_FORMAT_SIEGFRIED_ALPHA;
					      index -= 6;
					      line = null;
				      }
			      }
		      }
	      }
      } catch (IOException io) {
	      io.printStackTrace();
      }
	    try {
		    PFreader.close();
	    } catch (Exception e) {
		    e.printStackTrace();
	    }
    }
	  format[1] = index;

    return format;
  }

	public String ODFinputStandard(String filename) {

		resetODF();

		int[] odfFormat = checkODFInputFormat(filename);

		String resr = null;

		switch (odfFormat[0]) {
			case ODF_FORMAT_STANDARD:
				resr = ODFInputStandardFormat(filename, odfFormat[1]);
				break;
			case ODF_FORMAT_SIEGFRIED_ALPHA:
				resr = ODFInputSiegfriedAlphaFormat(filename, odfFormat[1]);
				break;
			case ODF_FORMAT_SIEGFRIED_GAMMA:
				resr = ODFInputSiegfriedGammaFormat(filename, odfFormat[1]);
				break;
			case ODF_FORMAT_SECTIONS:
				resr = ODFInputSectionsFormat(filename, odfFormat[1]);
				break;
			default: {}
		}
		fiottu();
		return resr;
	}

	public String ODFInputStandardFormat(String filename, int titleLines) {

		BufferedReader PFreader = Misc.getReader(filename);
		String resr = null;

		if (PFreader != null) {
			try {

				String line = PFreader.readLine();
				line = PFreader.readLine();
				StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");
				String token = st.nextToken();
				int igbr = Integer.valueOf(token).intValue();
				token = st.nextToken();
				resr = token;
//    		if (st.hasMoreTokens()) {
//    			token = st.nextToken();
//    		}
				setResolution(resr);
				line = PFreader.readLine();

				if (igbr == LaueGroupSnumber) {
					line = PFreader.readLine();
					st = new StringTokenizer(line, " ,\t\r\n");
					for (int ng = 0; ng < nge; ng++)
						for (int nb = 0; nb < nbe; nb++)
							for (int na = 0; na < nae; na++) {
								if (!st.hasMoreTokens()) {
									do {
										line = PFreader.readLine();
										st = new StringTokenizer(line, " ,\t\r\n");
									} while (!st.hasMoreTokens());
								}
//								if (moreData == 1 || (ng < nge - 1 && (na < nae - 1 && nb < nbe - 1)))
								odf[na][nb][ng] = Float.valueOf(st.nextToken()).floatValue();
								if (odf[na][nb][ng] >= 0 && odf[na][nb][ng] != 1.0)
									odf_covered[na][nb][ng] = true;
							}


/*          if (moreData == 0)
					  for (int ng = 0; ng < nge; ng++)
						  for (int nb = 0; nb < nbe; nb++)
							  for (int na = 0; na < nae; na++) {
								  if (na == nae - 1)
								    odfl[na][nb][ng] = odfl[0][nb][ng];
								  if (nb == nbe - 1)
								    odfl[na][nb][ng] = odfl[na][0][ng];
								  if (ng == nge - 1)
								    odfl[na][nb][ng] = odfl[na][nb][0];
								}*/
				} else {

					System.out.println("Crystal symmetry or resolution not corresponding!");

					for (int ng = 0; ng < nge; ng++)
						for (int nb = 0; nb < nbe; nb++)
							for (int na = 0; na < nae; na++) {
								odf[na][nb][ng] = 1.0f;
								odf_covered[na][nb][ng] = false;
							}
				}
				fiottu();
			} catch (IOException io) {
				io.printStackTrace();
				for (int ng = 0; ng < nge; ng++)
					for (int nb = 0; nb < nbe; nb++)
						for (int na = 0; na < nae; na++) {
							odf[na][nb][ng] = 1.0f;
							odf_covered[na][nb][ng] = false;
						}
			}
			try {
				PFreader.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		} else {
			for (int ng = 0; ng < nge; ng++)
				for (int nb = 0; nb < nbe; nb++)
					for (int na = 0; na < nae; na++) {
						odf[na][nb][ng] = 1.0f;
						odf_covered[na][nb][ng] = false;
					}
		}
		return resr;
	}

	public String ODFInputSiegfriedGammaFormat(String filename, int titleLines) {

		BufferedReader PFreader = Misc.getReader(filename);
		String resr = "5.0";
		setResolution(resr);
		double[] alphaValues = null;
		double[] betaValues = null;
		double[] gammaValues = null;
		Vector<String> alphaNumbers = new Vector<String>(19, 19);
		Vector<String> betaNumbers = new Vector<String>(19, 19);
		Vector<String> gammaNumbers = new Vector<String>(19, 19);
		Vector<double[][]> odfNumbers = new Vector<double[][]>(19, 19);
		Vector<double[]> odfValues = new Vector<double[]>(1387, 1387);
		boolean readingGammaBlock = true;
		boolean readingAlphaBlock = true;

		if (PFreader != null) {
			try {

				String line = null;

				for (int i = 0; i < titleLines; i++)
					line = PFreader.readLine();

				while (line != null && !Misc.toStringStripLeadingTrailingBlankAndTab(line).startsWith("GAMMA ="))
					line = PFreader.readLine();

				while (readingGammaBlock) {
					readingGammaBlock = false;
					StringTokenizer st = new StringTokenizer(line, " =,\t\r\n");
					String token = st.nextToken();
					gammaNumbers.add(token = st.nextToken());

					while (line != null && !Misc.toStringStripLeadingTrailingBlankAndTab(line).startsWith("ALPHA>"))
						line = PFreader.readLine();

					while (readingAlphaBlock) {
						readingAlphaBlock = false;
						System.out.println(line);
						if (line != null && alphaValues == null) {
							st = new StringTokenizer(line, " ,\t\r\n");
							st.nextToken();
							while (st.hasMoreTokens())
								alphaNumbers.add(st.nextToken());
						}
						line = PFreader.readLine(); // first BETA ------------- line
						line = PFreader.readLine();
						while (line != null && Misc.toStringDeleteBlankTabAndEOF(line).length() > 0) {
							if (betaValues == null) {
								String beta = Misc.toStringDeleteBlank(line.substring(0, 4));
								betaNumbers.add(beta);
							}
							line = line.substring(5);
							double[] tmpValues = new double[19];
							for (int i = 0; i < 19; i++) {
								String value = Misc.toStringDeleteBlank(line.substring(i * 6, (i + 1) * 6));
								tmpValues[i] = Double.parseDouble(value);
							}
							odfValues.add(tmpValues);
							line = PFreader.readLine();
						}

						if (betaNumbers.size() > 0) { // transfer the values of beta numbers read previously
							betaValues = new double[betaNumbers.size()];
							for (int i = 0; i < betaNumbers.size(); i++)
								betaValues[i] = Double.parseDouble(betaNumbers.elementAt(i));
							betaNumbers.removeAllElements();
						}
						while (line != null && Misc.toStringDeleteBlankTabAndEOF(line).length() == 0)
							line = PFreader.readLine();
						if (line != null && Misc.toStringStripLeadingTrailingBlankAndTab(line).startsWith("ALPHA>")) {
							if (alphaValues == null)
								alphaNumbers.removeElementAt(alphaNumbers.size() - 1);
							readingAlphaBlock = true;
						}
					}

					if (alphaNumbers.size() > 0) { // transfer the values of alpha numbers read previously
						alphaValues = new double[alphaNumbers.size()];
						for (int i = 0; i < alphaNumbers.size(); i++)
							alphaValues[i] = Double.parseDouble(alphaNumbers.elementAt(i));
						alphaNumbers.removeAllElements();
					}

					double[][] odfnmb = new double[betaValues.length][alphaValues.length];
					int repeat = alphaValues.length / (betaValues.length - 1);
					int subset = alphaValues.length / repeat;
					int index = 0;
					double[] tmpValues = null;
					for (int i = 0; i < betaValues.length; i++) {
						for (int k = 0; k < repeat; k++) {
							tmpValues = odfValues.elementAt(k * betaValues.length + i);
							for (int j = 0; j < subset; j++)
								odfnmb[i][k * subset + j] = tmpValues[j];
						}
						if (repeat > 1)
							odfnmb[i][alphaValues.length - 1] = tmpValues[subset];
					}
					odfNumbers.add(odfnmb);
					if (line != null && Misc.toStringStripLeadingTrailingBlankAndTab(line).startsWith("GAMMA =")) {
						readingGammaBlock = true;
						readingAlphaBlock = true;
					}
				}
				if (gammaNumbers.size() > 0) { // transfer the values of beta numbers read previously
					gammaValues = new double[gammaNumbers.size()];
					for (int i = 0; i < gammaNumbers.size(); i++)
						gammaValues[i] = Double.parseDouble(gammaNumbers.elementAt(i));
					gammaNumbers.removeAllElements();
				}

				for (int ng = 0; ng < gammaValues.length && ng < nge; ng++) {
					double[][] tmpValues = odfNumbers.elementAt(ng);
					for (int nb = 0; nb < betaValues.length && nb < nbe; nb++)
						for (int na = 0; na < alphaValues.length && na < nae; na++) {
							odf[na][nb][ng] = tmpValues[nb][na];
							odf_covered[na][nb][ng] = true;
						}
				}

				fiottu();
			} catch (IOException io) {
				io.printStackTrace();
				for (int ng = 0; ng < nge; ng++)
					for (int nb = 0; nb < nbe; nb++)
						for (int na = 0; na < nae; na++) {
							odf[na][nb][ng] = 1.0f;
							odf_covered[na][nb][ng] = false;
						}
			}
			try {
				PFreader.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		} else {
			for (int ng = 0; ng < nge; ng++)
				for (int nb = 0; nb < nbe; nb++)
					for (int na = 0; na < nae; na++) {
						odf[na][nb][ng] = 1.0f;
						odf_covered[na][nb][ng] = false;
					}
		}
		return resr;
	}

	public String ODFInputSiegfriedAlphaFormat(String filename, int titleLines) {

		BufferedReader PFreader = Misc.getReader(filename);
		String resr = "5.0";
		setResolution(resr);
		double[] alphaValues = null;
		double[] betaValues = null;
		double[] gammaValues = null;
		Vector<String> alphaNumbers = new Vector<String>(19, 19);
		Vector<String> betaNumbers = new Vector<String>(19, 19);
		Vector<String> gammaNumbers = new Vector<String>(19, 19);
		Vector<double[][]> odfNumbers = new Vector<double[][]>(19, 19);
		Vector<double[]> odfValues = new Vector<double[]>(1387, 1387);
		boolean readingGammaBlock = true;
		boolean readingAlphaBlock = true;

		if (PFreader != null) {
			try {

				String line = null;

				for (int i = 0; i < titleLines; i++)
					line = PFreader.readLine();

				while (line != null && !Misc.toStringStripLeadingTrailingBlankAndTab(line).startsWith("ALPHA ="))
					line = PFreader.readLine();

				while (readingGammaBlock) {
					readingGammaBlock = false;
					StringTokenizer st = new StringTokenizer(line, " =,\t\r\n");
					String token = st.nextToken();
					gammaNumbers.add(token);

					while (line != null && !Misc.toStringStripLeadingTrailingBlankAndTab(line).startsWith("GAMMA>"))
						line = PFreader.readLine();

					while (readingAlphaBlock) {
						readingAlphaBlock = false;
						if (line != null && alphaValues == null) {
							st = new StringTokenizer(line, " ,\t\r\n");
							st.nextToken();
							while (st.hasMoreTokens())
								alphaNumbers.add(st.nextToken());
						}
						line = PFreader.readLine(); // first BETA ------------- line
						line = PFreader.readLine();
						while (line != null && Misc.toStringDeleteBlankTabAndEOF(line).length() > 0) {
							if (betaNumbers == null) {
								String beta = Misc.toStringDeleteBlank(line.substring(0, 4));
								line = line.substring(5);
								betaNumbers.add(beta);
							}
							double[] tmpValues = new double[19];
							for (int i = 0; i < 19; i++) {
								String value = Misc.toStringDeleteBlank(line.substring(i * 6, (i + 1) * 6));
								tmpValues[i] = Double.parseDouble(value);
							}
							odfValues.add(tmpValues);
							line = PFreader.readLine();
						}

						if (betaNumbers.size() > 0) { // transfer the values of beta numbers read previously
							betaValues = new double[betaNumbers.size()];
							for (int i = 0; i < betaNumbers.size(); i++)
								betaValues[i] = Double.parseDouble(betaNumbers.elementAt(i));
							betaNumbers.removeAllElements();
						}
						while (line != null && Misc.toStringDeleteBlankTabAndEOF(line).length() == 0)
							line = PFreader.readLine();
						if (line != null && Misc.toStringStripLeadingTrailingBlankAndTab(line).startsWith("GAMMA>")) {
							if (alphaValues == null)
								alphaNumbers.removeElementAt(alphaNumbers.size() - 1);
							readingAlphaBlock = true;
						}
					}

					if (alphaNumbers.size() > 0) { // transfer the values of alpha numbers read previously
						alphaValues = new double[alphaNumbers.size()];
						for (int i = 0; i < alphaNumbers.size(); i++)
							alphaValues[i] = Double.parseDouble(alphaNumbers.elementAt(i));
						alphaNumbers.removeAllElements();
					}

					double[][] odfnmb = new double[betaValues.length][alphaValues.length];
					int repeat = alphaValues.length / (betaValues.length - 1);
					int subset = alphaValues.length / repeat;
					int index = 0;
					double[] tmpValues = null;
					for (int i = 0; i < betaValues.length; i++) {
						for (int k = 0; k < repeat; k++) {
							tmpValues = odfValues.elementAt(index++);
							for (int j = 0; j < subset; j++)
								odfnmb[i][k * subset + j] = tmpValues[j];
						}
						if (repeat > 1)
							odfnmb[i][alphaValues.length - 1] = tmpValues[subset];
					}
					odfNumbers.add(odfnmb);

					if (line != null && Misc.toStringStripLeadingTrailingBlankAndTab(line).startsWith("GAMMA ="))
						readingGammaBlock = true;
				}
				if (gammaNumbers.size() > 0) { // transfer the values of beta numbers read previously
					gammaValues = new double[gammaNumbers.size()];
					for (int i = 0; i < gammaNumbers.size(); i++)
						gammaValues[i] = Double.parseDouble(gammaNumbers.elementAt(i));
					gammaNumbers.removeAllElements();
				}

				for (int ng = 0; ng < gammaValues.length && ng < nae; ng++) {
					double[][] tmpValues = odfNumbers.elementAt(ng);
					for (int nb = 0; nb < betaValues.length && nb < nbe; nb++)
						for (int na = 0; na < alphaValues.length && na < nge; na++) {
							odf[na][nb][ng] = tmpValues[nb][na];
							odf_covered[ng][nb][na] = true;
						}
				}

				fiottu();
			} catch (IOException io) {
				io.printStackTrace();
				for (int ng = 0; ng < nge; ng++)
					for (int nb = 0; nb < nbe; nb++)
						for (int na = 0; na < nae; na++) {
							odf[na][nb][ng] = 1.0f;
							odf_covered[na][nb][ng] = false;
						}
			}
			try {
				PFreader.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		} else {
			for (int ng = 0; ng < nge; ng++)
				for (int nb = 0; nb < nbe; nb++)
					for (int na = 0; na < nae; na++) {
						odf[na][nb][ng] = 1.0f;
						odf_covered[na][nb][ng] = false;
					}
		}
		return resr;
	}

	public String ODFInputSectionsFormat(String filename, int titleLines) {
		return "5";
	}

	public void fiottu() {

    /* Local variables */
    int lgam, nczh, ngamstep, na, nb, ng, ngstep, nav, nbv, ngv;

/* 	COMPLETES THE ARRAY FIO TO THE FULL G-SPACE DIMENSION */
/*       USING THE LEFTSIDED AND RIGHTSIDED SYMMETRY GROUPS GB,GA */
/*       AND THE INPUT VALUES FIO IN THE REGION  : */
/*                  NA=NAA,NAEE (cf. 2)) ; NB=1,NBE ; NG=1,NGE */

    int betama = (alphama1) / 2 + 1;
    int betamad2 = (betama1) / 2 + 1;

    nczh = rotationFold - 1;
    ngstep = nge - 1;


    for (ng = 0; ng < nge; ng++)
      for (nb = 0; nb < nbe; nb++) {
        odf[nae - 1][nb][ng] = odf[0][nb][ng];
        odf_covered[nae - 1][nb][ng] = odf_covered[0][nb][ng];
      }
    for (na = 0; na < nae; ++na)
      for (ng = 1; ng < nge; ++ng) {
        int nan = na + ng;
        while (nan >= nae)
          nan -= nae - 1;
        odf[na][0][ng] = odf[nan][0][0];
        odf_covered[na][0][ng] = odf_covered[nan][0][0];
        if (nbe == betama) {
          nan = na - ng;
          while (nan < 0)
            nan += nae - 1;
          odf[na][nbe - 1][ng] = odf[nan][nbe - 1][0];
          odf_covered[na][nbe - 1][ng] = odf_covered[nan][nbe - 1][0];
        }
      }

// Sample symmetry
//    System.out.println(multiplicity);
    switch (getSampleSymmetryValue()) {
/*			case 1:
			case 2:
			case 3:
				fold = multiplicity + 1;
				lphiturn = (alpham - 1) / fold;
        for (ng = 0; ng < nge; ++ng)
          for (nb = 0; nb < nbe; ++nb) {
            for (na = 0; na < lphiturn; ++na)
              if (fl[na][nb][ng] >= 0)
                for (int index = 1; index < fold; index++)
                  fl[na + lphiturn * index][nb][ng] = fl[na][nb][ng];
            fl[nae - 1][nb][ng] = fl[0][nb][ng];
          }
				break;
			case 4:
				fold = 6;
				lphiturn = (alpham - 1) / fold;
        for (ng = 0; ng < nge; ++ng)
          for (nb = 0; nb < nbe; ++nb) {
            for (na = 0; na < lphiturn; ++na)
              if (fl[na][nb][ng] >= 0) {
                for (int index = 1; index < 6; index++)
                  fl[na + lphiturn * index][nb][ng] = fl[na][nb][ng];
              }
            fl[nae - 1][nb][ng] = fl[0][nb][ng];
          }
				break;
			case 5:
        for (ng = 0; ng < nge; ++ng)
          for (nb = 0; nb < nbe; ++nb) {
            for (na = 0; na < (alpham - 1) / 2; ++na)
              if (fl[na][nb][ng] >= 0) {
                fl[(alpham - 1) - na][nb][ng] = fl[na][nb][ng];
              }
            fl[nae - 1][nb][ng] = fl[0][nb][ng];
          }
				break;
			case 6: // orthorhombic
        for (ng = 0; ng < nge; ++ng)
          for (nb = 0; nb < nbe; ++nb) {
            for (na = 0; na < (alpham - 1) / 4; ++na)
              if (fl[na][nb][ng] >= 0) {
                fl[(alpham - 1) / 2 - na][nb][ng] = fl[na][nb][ng];
                fl[na + (alpham - 1) * 3 / 4][nb][ng] = fl[na][nb][ng];
                fl[(alpham - 1) - na][nb][ng] = fl[na][nb][ng];
              }
            fl[nae - 1][nb][ng] = fl[0][nb][ng];
          }
				break;*/
      case 7:
        for (ng = 0; ng < nge; ++ng)
          for (nb = 0; nb < nbe; ++nb) {
            if (odf[0][nb][ng] >= 0)
              for (na = 1; na < nae; ++na) {
                odf[na][nb][ng] = odf[0][nb][ng];
                odf_covered[na][nb][ng] = odf_covered[0][nb][ng];
              }
          }
        break;
      default:
        {
        }
    }

/*  COMPLETITION OF THE GAMMA REGION USING SYMMETRY GROUP GB */

/*  Symmetry element CnZB : ALPHA,BETA,GAMMA+2PI/n */

    if (LaueGroupSnumber != 1) {
      for (lgam = 1; lgam <= nczh; ++lgam) {
        ngamstep = ngstep * lgam;
        for (ng = 0; ng < nge; ++ng) {
          ngv = ngamstep + ng;
          if (ngv < nge)
            for (na = 0; na < nae; ++na) {
              for (nb = 0; nb < nbe; ++nb) {
                if (odf[na][nb][ng] >= 0) {
                  odf[na][nb][ngv] = odf[na][nb][ng];
                  odf_covered[na][nb][ngv] = odf_covered[na][nb][ng];
                }
                else if (odf[na][nb][ngv] >= 0) {
                  odf[na][nb][ng] = odf[na][nb][ngv];
                  odf_covered[na][nb][ng] = odf_covered[na][nb][ngv];
                }
              }
            }
        }
      }

/*  GAMMA REGION 0-360 degrees IS COMPLETE */

      if (mdb != 0) {
/*  For GB = Dn : Symmetry element C2XB */
/*                PI+ALPHA, PI-BETA, 2PI-GAMMA */

        for (nb = 0; nb < betamad2; ++nb) {
          nbv = betama - nb - 1;
          if (nbv < nbe && nb < nbe) {
            for (ng = 0; ng < alphama; ++ng) {
              ngv = alphama - ng - 1;
              if (ngv < nge && ng < nge) {
                for (na = 0; na < nae; ++na) {
                  nav = na + betama1;
                  if (nav >= alphama) {
                    nav -= (alphama1);
                  }
                  if (nav < nae) {
                    if (odf[na][nb][ng] >= 0) {
                      odf[nav][nbv][ngv] = odf[na][nb][ng];
                      odf_covered[nav][nbv][ngv] = odf_covered[na][nb][ng];
                    } else if (odf[nav][nbv][ngv] >= 0) {
                      odf[na][nb][ng] = odf[nav][nbv][ngv];
                      odf_covered[na][nb][ng] = odf_covered[nav][nbv][ngv];
                    }
                  }
                }
                if (nae >= betama) {
                  if (odf[alphama - 1][nbv][ngv] >= 0) {
                    odf[0][nbv][ngv] = odf[alphama - 1][nbv][ngv];
                    odf_covered[0][nbv][ngv] = odf_covered[alphama - 1][nbv][ngv];
                  } else if (odf[0][nbv][ngv] >= 0) {
                    odf[alphama - 1][nbv][ngv] = odf[0][nbv][ngv];
                    odf_covered[alphama - 1][nbv][ngv] = odf_covered[0][nbv][ngv];
                  }
                }
              }
            }
          }
        }
      }
    }

    for (na = 0; na < nae; ++na)
      for (ng = 1; ng < nge; ++ng) {
        int nan = na + ng;
        while (nan >= nae)
          nan -= nae - 1;
        odf[na][0][ng] = odf[nan][0][0];
        odf_covered[na][0][ng] = odf_covered[nan][0][0];
        if (nbe == betama) {
          nan = na - ng;
          while (nan < 0)
            nan += nae - 1;
          odf[na][nbe - 1][ng] = odf[nan][nbe - 1][0];
          odf_covered[na][nbe - 1][ng] = odf_covered[nan][nbe - 1][0];
        }
      }

    checkFullODF();
  }

  public void odfNormalization() {
    double vbg = 0.0, va = 0.0, hvg = 0., hvb = 0., a = 0., b = 0.;
    double fn = 0.0;

    double fnorm = 0.0f;
    double roundOffCorrection = 0.0;
    double coverageCorrection = 0.0;
    double totalCorrection = 0.0;
    int[] tmp_index = new int[3];

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
          tmp_index[0] = na;
          tmp_index[1] = nb;
          tmp_index[2] = ng;
// l_last1202    			applyCrystalSymmetryAndCheck(tmp_index);
// l_last1202					fn = odf[tmp_index[0]][tmp_index[1]][tmp_index[2]];
          fn = getODF(tmp_index);
          if (odf_covered[tmp_index[0]][tmp_index[1]][tmp_index[2]]) {
            fnorm += fn * va;
            roundOffCorrection += va;
            coverageCorrection += va;
          } else {
//	  	  		fnorm -= fn * va;
//	  	  		roundOffCorrection += va;
          }
          totalCorrection += va;
        }

      }
    }

//    double fnormTheoretical = Constants.PI * 8. * Constants.PI / fnorm;
    fnorm = roundOffCorrection / fnorm;

    for (int ng = 0; ng < alphama; ng++)
      for (int nb = 0; nb < betama; nb++)
        for (int na = 0; na < alphama; na++)
          if (odf_covered[na][nb][ng]) {
            odf[na][nb][ng] *= fnorm;
//            odf_covered[na][nb][ng] = true;
          }

    double ODFcoverage = coverageCorrection / totalCorrection * 100.0;
    setODFcoverage(Fmt.format(ODFcoverage));
    System.out.println("ODF coverage: " + Fmt.format(ODFcoverage) + " %, Normalization factor: " +
        Fmt.format(fnorm));
//	  System.out.println("Normalization factor: " + Fmt.format(fnormTheoretical));
  }

  public double getODF(int[] index) {
//    applyCrystalSymmetryLightCheck(index);
    applyCrystalSymmetryAndCheck(index);
    return odf[index[0]][index[1]][index[2]];
  }

  public void setODFcoverage(String s) {
  }

  public void checkFullODF() {
    int[] index = new int[3];
    for (int na = 0; na < alphama; na++)
      for (int nb = 0; nb < betama; nb++)
        for (int ng = 0; ng < alphama; ng++)
          if (!((na < nae && nb < nbe) && ng < nge)) {
            index[0] = na;
            index[1] = nb;
            index[2] = ng;
            applyCrystalSymmetryAndCheck(index);
            odf[na][nb][ng] = odf[index[0]][index[1]][index[2]];
            odf_covered[na][nb][ng] = odf_covered[index[0]][index[1]][index[2]];
          }

  }

//  public int computeSampleSymmetryValue() {
//    return 0;
//  }
  public int getSampleSymmetryValue() {
    return sampleSymmetryValue;
  }

	public int computeSampleSymmetryValue() {

		String samplesym = getSampleSymmetry();

		for (int i = 0; i < symmetrychoicenumber; i++) {
			if (samplesym.equals(symmetrychoice[i]))
				return i;
		}
		if (samplesym.equals("cylindrical")) { // in old analyses was cylindrical instead of fiber
			for (int i = 0; i < symmetrychoicenumber; i++) {
				if (FIBER.equals(symmetrychoice[i]))
					return i;
			}
		}
		return 0;
	}

/*  public double getExperimentalTextureFactors(Reflection refl, int point, Sample asample) {
    int truepointmax = asample.getNumberActiveDatafiles();
    int sector = point / truepointmax;
    int residual = point - sector * truepointmax;
    int index = asample.getActiveDiffrDataFile(residual).getIndex();
    return refl.getExpTextureFactor(index);
  }
*/

  public double[] applySampleSymmetry(Reflection reflex, int point, int truepointmax) {
    return null;
  }

  public String getSampleSymmetry() {
    return null;
  }

  public int getSampleSymmetryMultiplicity() {
    switch (getSampleSymmetryValue()) {
      case 0:
      case 1:
      case 2:
      case 3:
        return getSampleSymmetryValue() + 1;
      case 4:
        return 6;
      case 5:
        return 2;
      case 6:
        return 4;
      case 7:
        return 72;
      default:
        {
        }
    }
    return 1;
  }

  public void applyCrystalSymmetryAndCheck(int[] index) {
  }

  public static int[] getCellNumber(int laueGroup, double res) {

    int alphamax = Uwimvuo.getAlphamax(res);
    int betamax = (alphamax + 1) / 2;
    int betamad2 = (betamax + 1) / 2;
    int alphamad3 = (alphamax + 2) / 3;
    int alphamad6 = (alphamax + 5) / 6;
    int[] ne = new int[5];
    ne[0] = alphamax;
    switch (laueGroup) {
      case 2:
        ne[1] = betamax;
        ne[2] = betamax;
        ne[3] = 2;
        ne[4] = 0;
        break;
      case 3:
      case 6:
        ne[1] = betamad2;
        ne[2] = betamax;
        ne[3] = 2;
        ne[4] = 1;
        break;
      case 4:
        ne[1] = betamax;
        ne[2] = betamad2;
        ne[3] = 4;
        ne[4] = 0;
        break;
      case 5:
      case 7:
        ne[1] = betamad2;
        ne[2] = betamad2;
        ne[3] = 4;
        ne[4] = 1;
        break;
      case 8:
        ne[1] = betamax;
        ne[2] = alphamad3;
        ne[3] = 3;
        ne[4] = 0;
        break;
      case 9:
        ne[1] = betamad2;
        ne[2] = alphamad3;
        ne[3] = 3;
        ne[4] = 1;
        break;
      case 10:
        ne[1] = betamax;
        ne[2] = alphamad6;
        ne[3] = 6;
        ne[4] = 0;
        break;
      case 11:
        ne[1] = betamad2;
        ne[2] = alphamad6;
        ne[3] = 6;
        ne[4] = 1;
        break;
      case 1:
      default:
        {
          ne[1] = betamax;
          ne[2] = alphamax;
          ne[3] = 1;
          ne[4] = 0;
        }
    }

    return ne;
  }

  public double[] computeTextureFactor(double[][] texture_angles,
                                       double[] sctf, double fhir, int inv,
                                       int h, int k, int l) {
    return null;
  }

  public double[][] recomputedTextureFactor(final Phase aphase, final Sample asample, final boolean setValues) {

	  textureInitialization();

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
								  double[][][] positions = adatafile.getPositions(aphase);
//								  for (int ppp = 0; ppp < adatafile.positionsPerPattern; ppp++) {
									  double texture_angles[] = adatafile.getTextureAngles(positions[ij][0][0]);
									  texAngle[0][idatafile] = (texture_angles[0] * Constants.DEGTOPI);
									  texAngle[1][idatafile] = (texture_angles[1] * Constants.DEGTOPI);
									  idatafile++;
//								  }
							  }
						  }
						  double[] texFactor = computeTextureFactor(texAngle, sctf, fhir, inv, refl.getH(), refl.getK(), refl.getL());
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
					  double[][][] positions = adatafile.getPositions(aphase);
//					  for (int ppp = 0; ppp < adatafile.positionsPerPattern; ppp++) {
						  double texture_angles[] = adatafile.getTextureAngles(positions[ij][0][0]);
						  texAngle[0][idatafile] = (texture_angles[0] * Constants.DEGTOPI);
						  texAngle[1][idatafile] = (texture_angles[1] * Constants.DEGTOPI);
						  idatafile++;
//					  }
				  }
			  }
			  double[] texFactor = computeTextureFactor(texAngle, sctf, fhir, inv, refl.getH(), refl.getK(), refl.getL());
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

}
