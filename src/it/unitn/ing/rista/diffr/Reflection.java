/*
 * @(#)Reflection.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.io.cif.*;
import it.unitn.ing.jgraph.ColorMap;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.Vector;

/**
 * The Reflection is a class
 *
 * @version $Revision: 1.23 $, $Date: 2006/11/10 09:33:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Reflection {
  //insert class definition here
  private int h = 0, k = 0, l = 0, h2 = 0, k2 = 0, l2 = 0;
	public int multiplicity = 0;
	private int base_h = 0, base_k = 0, base_l = 0, base_h2 = 0, base_k2 = 0, base_l2 = 0;
	public boolean superReflection = false;
  public double d_space = 0;
	public double structureFactor = 0; // only for the custom peaks
	public double crystallite = 0;
	public double microstrain = 0;

  public double textureWeight = 1.0;
  public double textureOverlappedWeight = 0.0;
//	public double strainweight = 1.0;
  public boolean goodforTexture = true;
  public boolean goodforStrain = true;
  public boolean goodforStructureFactor = true;
  public boolean poleFigurePlot = false;
  public int izoveri = 1;
  Phase phase = null;
//	public int orderIndex;
  public int[] hlist = null, klist = null, llist = null;
	public double[] pd_deltaD = null;
	public double[] pd_deltaIndex = null;
//	public int[] base_v = {1, 1, 1};

  public int Lsum = 0;
  public int Labssum = 0;
  public int Lsumdivideabs = 0;
  public double hzero = 0;
  public int hzero2 = 0;
  public int broadened = 0;
  public double[] phi = null, beta = null;
	public double idNumber = 0;
	public double tmp_position; // do not use it, only for temp storage, it can change between datasets

	private double structureModifier = 1.0;
	private double[] dividers = {1.0, 1.0, 1.0};

	public Reflection(int h, int k, int l) {
		this.h = h;
		this.k = k;
		this.l = l;
	}

  private Reflection(Phase aphase, int h, int k, int l, int multi, double d_space, int b_h, int b_k, int b_l) {
	  phase = aphase;
	  multiplicity = multi;
	  setDSpace(d_space);
	  base_h = b_h;
	  base_k = b_k;
	  base_l = b_l;
    this.h = h;
    this.k = k;
    this.l = l;
	  h2 = h * h;
	  k2 = k * k;
	  l2 = l * l;
	  base_h2 = base_h * base_h;
	  base_k2 = base_k * base_k;
	  base_l2 = base_l * base_l;
	  idNumber = getUniqueIdFor(h, k, l);
  }

	public Reflection(Phase aphase, int[] hlist, int[] klist, int[] llist, int multi, double d_space) {
		this(aphase, hlist, klist, llist, multi, d_space, hlist[0], klist[0], llist[0]);
	}

	public Reflection(Phase aphase, int[] hlist, int[] klist, int[] llist, int multi,
	                  double d_space, int b_h, int b_k, int b_l) {
		this(aphase, hlist[0], klist[0], llist[0], multi, d_space, b_h, b_k, b_l);

		if (base_h != h || base_k != h || base_l != l) {
			superReflection = true;
/*			base_v[0] = hlist[0] / b_h;
			base_v[1] = klist[0] / b_k;
			base_v[2] = llist[0] / b_l;*/
		}

		int cType = aphase.getClosePackedType();

		hzero2 = hlist[0] * hlist[0] + klist[0] * klist[0] + llist[0] * llist[0];
		hzero = Math.sqrt(hzero2);

		int mult2 = multi / 2;
		this.hlist = new int[mult2];
		this.klist = new int[mult2];
		this.llist = new int[mult2];

		phi = new double[multi];
		beta = new double[multi];
//		int PGIndex = SpaceGroups.getPGNumberLconvention(aphase.getPointGroup(), aphase.getMonoclinicAxis(), aphase.isRhombohedral());
//		double[] acell = Angles.getLattice(aphase);
//		double[] astar = Angles.getReciprocalLattice(aphase);

		int Lzero, Lzerosign, is3N1, absLzero, h1, k1, l1;

		pd_deltaD = new double[3];
		pd_deltaIndex = new double[3];

		for (int i = 0; i < multi; i++) {
			if (i < mult2) {
				this.hlist[i] = hlist[i];
				this.klist[i] = klist[i];
				this.llist[i] = llist[i];
				h1 = hlist[i];
				k1 = klist[i];
				l1 = llist[i];
			} else {
				h1 = -hlist[i - mult2];
				k1 = -klist[i - mult2];
				l1 = -llist[i - mult2];
			}

			//this are in radiants
			double[] phicosphi = Angles.getPhicosPhi(aphase, h1, k1, l1);
			beta[i] = phicosphi[0];
			phi[i] = phicosphi[1];

			if (cType == 2)
				Lzero = 2 * l1 - h1 - k1;
			else
				Lzero = h1 + k1 + l1;

			Lzerosign = MoreMath.is3Neven(Lzero);
			pd_deltaD[Lzerosign + 1] += Lzerosign * Lzero;
			pd_deltaIndex[Lzerosign + 1]++;
			if (Lzerosign != 0) {
				broadened++;
				absLzero = Math.abs(Lzero);
				Labssum += absLzero;
				Lsum += Lzerosign * Lzero;
				Lsumdivideabs += Lzerosign * Lzero / absLzero;
			}
		}
		Lsumdivideabs *= broadened / (Constants.sqrt3 * Constants.PI * multiplicity);
		for (int i = 0; i < 3; i++) {
			pd_deltaD[i] *= Constants.sqrt3 / (2.0 * Constants.PI * hzero2 * multiplicity);
			pd_deltaIndex[i] /= multi;
		}
	}

	private static int hklmax = 500;
	private static int idConst1 = hklmax * hklmax * 4;
	private static int idConst2 = hklmax * 2;

	public static int getUniqueIdFor(int h, int k, int l) {
		return (h + hklmax) * idConst1 + (k + hklmax) * idConst2 + l + hklmax;
	}

	public int getH() { return h; }
	public int getK() { return k; }
	public int getL() { return l; }
	public int getH2() { return h2; }
	public int getK2() { return k2; }
	public int getL2() { return l2; }
	public int getBaseH() { return base_h; }
	public int getBaseK() { return base_k; }
	public int getBaseL() { return base_l; }
	public int getBaseH2() { return base_h2; }
	public int getBaseK2() { return base_k2; }
	public int getBaseL2() { return base_l2; }
	public double getStructureModifier() { return structureModifier; }
	public void setStructureModifier(double factor) { structureModifier = factor; }
	public double[] getDivisionFactors() { return dividers; }
	public void setDividers(double divh, double divk, double divl) {
		dividers[0] = divh;
		dividers[1] = divk;
		dividers[2] = divl;
	}

	public static int[] getHKLfromId(int id) {
		int[] hkl = new int[3];
		int first = id / idConst1;
		int rest = id % idConst1;
		int second = rest / idConst2;
		hkl[0] = first - hklmax;
		hkl[1] = second - hklmax;
		hkl[2] = rest % idConst2 -hklmax;
		return hkl;
	}

  public void setDSpace(double value) {
    d_space = value;
  }

  public void refreshforUpdate() {
    int mult2 = hlist.length;
    for (int i = 0; i < mult2; i++) {

      int h1 = hlist[i];
      int k1 = klist[i];
      int l1 = llist[i];

      //this are in radiants
      double[] phicosphi = Angles.getPhicosPhi(phase, h1, k1, l1);

      beta[i] = phicosphi[0];
      phi[i] = phicosphi[1];

      h1 = -h1;
      k1 = -k1;
      l1 = -l1;

      phicosphi = Angles.getPhicosPhi(phase, h1, k1, l1);

      beta[i + mult2] = phicosphi[0];
      phi[i + mult2] = phicosphi[1];

    }
  }

  public double getPlanarDefectDisplacement(int index) {
    return getParent().getActivePlanarDefects().getPlanarDefectDisplacement(this) * pd_deltaD[index];
  }

  public Phase getParent() {
    return phase;
  }

  public double getWeight() {
    return textureWeight;
  }

  public void setWeight(double wgt) {
    textureWeight = wgt;
  }

  public void setOverlapNumber(int numberOverlapped) {
    izoveri = numberOverlapped;
  }

  public double getOverlappedWeight() {
    return textureOverlappedWeight;
  }

  public void setOverlappedWeight(double wgt) {
    textureOverlappedWeight = wgt;
  }

  public boolean isGoodforTexture() {
    return goodforTexture;
  }

  public void setnoGoodforTexture() {
//		System.out.println("No good for texture: " + h + " " + k + " " + l);
    goodforTexture = false;
  }

  public void setGoodforTexture() {
//		System.out.println("Good for texture: " + h + " " + k + " " + l);
    goodforTexture = true;
  }

  public boolean isGoodforStrain() {
    return goodforStrain;
  }

  public void setnoGoodforStrain() {
    goodforStrain = false;
  }

  public void setGoodforStrain() {
    goodforStrain = true;
  }

  public boolean isGoodforStructureFactor() {
    return goodforStructureFactor;
  }

  public boolean equalsTo(Reflection arefl) {
    boolean check = true;
    if (h != arefl.h)
      check = false;
    if (k != arefl.k)
      check = false;
    if (l != arefl.l)
      check = false;
    return check;
  }

  public boolean equalsTo(int h, int k, int l) {
    boolean check = false;
    for (int i = 0; i < hlist.length; i++) {
      if (h == hlist[i] && k == klist[i] && l == llist[i])
        check = true;
      if (h == -hlist[i] && k == -klist[i] && l == -llist[i])
        check = true;
    }
    return check;
  }

/*	public boolean equalsToBase(int h, int k, int l) {
		if (!superReflection)
			return equalsTo(h, k, l);

		boolean check = false;
		for (int i = 0; i < hlist.length; i++) {
			if (h == base_v[0] * hlist[i] && k == base_v[1] * klist[i] && l == base_v[2] * llist[i])
				check = true;
			if (h == -base_v[0] * hlist[i] && k == -base_v[1] * klist[i] && l == -base_v[2] * llist[i])
				check = true;
		}
		return check;
	}*/

	public boolean similarsTo(double dspace, double error) {
    return Math.abs(d_space - dspace) < error;
  }

  public static double[] getPhiBeta(double x, double y) {
    double[] phibeta = new double[2];
    double r = Math.sqrt(x * x + y * y);
    if (r > 1.0E-9) {
      phibeta[1] = Math.acos(x / r);
      if (y < 0)
        phibeta[1] += Math.PI;
    } else
      phibeta[1] = 0.0;
    phibeta[0] = r;
    return phibeta;
  }

  public static double[] getXYminmax(int group) {
    double[] xymax = new double[4];
    switch (group) {
      case 0:  // -1
        xymax[0] = -Math.PI / 2.0;
        xymax[1] = Math.PI / 2.0;
        xymax[2] = -Math.PI / 2.0;
        xymax[3] = Math.PI / 2.0;
        break;
      case 1:  // 2/m
        xymax[0] = -Math.PI / 2.0;
        xymax[1] = Math.PI / 2.0;
        xymax[2] = -Math.PI / 2.0;
        xymax[3] = Math.PI / 2.0;
        break;
      case 2:  // 2/mmm
      case 3:  // 4/m
      case 4:  // 4/mmmm
        xymax[0] = 0.0;
        xymax[1] = Math.PI / 2.0;
        xymax[2] = 0.0;
        xymax[3] = Math.PI / 2.0;
        break;
      case 5:  // -3
        xymax[0] = -Math.PI / 2.0;
        xymax[1] = Math.PI / 2.0;
        xymax[2] = -Math.PI / 2.0;
        xymax[3] = Math.PI / 2.0;
        break;
      case 6:  // -3m
      case 7:  // 6/m
      case 8:  // 6/mmm
        xymax[0] = 0.0;
        xymax[1] = Math.PI / 2.0;
        xymax[2] = 0.0;
        xymax[3] = Math.PI / 2.0;
        break;
      case 9:  // m3
      case 10: // m3m
        xymax[0] = 0.0;
        xymax[1] = Math.PI / 4.0;
        xymax[2] = 0.0;
        xymax[3] = Math.PI / 4.0;
        break;
    }
    return xymax;
  }

  public static boolean insideLimits(int group, double phi, double beta) {
    boolean result = (phi <= Math.PI / 2.0 && phi >= 0.0);
    switch (group) {
      case 0:  // -1
        result = result && (beta >= 0.0 && beta <= 2.0 * Math.PI);
        break;
      case 1:  // 2/m
        result = result && (beta >= 0.0 && beta <= Math.PI);
        break;
      case 2:  // 2/mmm
        result = result && (beta >= 0.0 && beta <= Math.PI / 2.0);
        break;
      case 3:  // 4/m
        result = result && (beta >= 0.0 && beta <= Math.PI / 4.0);
        break;
      case 4:  // 4/mmmm
        result = result && (beta >= 0.0 && beta <= Math.PI / 4.0);
        break;
      case 5:  // -3
        result = result && (beta >= 0.0 && beta <= Math.PI / 1.5);
        break;
      case 6:  // -3m
        result = result && (beta >= 0.0 && beta <= Math.PI / 1.5);
        break;
      case 7:  // 6/m
        result = result && (beta >= 0.0 && beta <= Math.PI / 3.0);
        break;
      case 8:  // 6/mmm
        result = result && (beta >= 0.0 && beta <= Math.PI / 3.0);
        break;
      case 9:  // m3
      case 10: // m3m
        result = (beta >= 0.0 && beta <= Math.PI / 4.0);
        double k;
        if (!result)
          return result;
        if (beta < 1.0E-9) {
//          h = 0.0;
          k = 2.0;
        } else if (phi <= 1.0E-9)
          return result;
        else {
          double tanbeta = Math.tan(beta);
          double cot2beta = tanbeta * tanbeta;
          k = Math.sqrt(1.0 + cot2beta) / Math.tan(phi);
//          h = k * tanbeta;
        }
        result = k >= 1.0; // (result && k >= 1.0);
        break;
    }
    return result;
  }

  public double[][] getInversePoleFigureGrid(double[] texture_angles, int numberofPoints, Sample asample) {
    double[][] value = new double[numberofPoints][numberofPoints];
    double[] singleValues;
    Phase aphase = getParent();
    int group = SpaceGroups.getLGNumber(aphase.getPointGroup());
    double[] step = new double[2];
    double[] minmax = getXYminmax(group);
    step[0] = (minmax[1] - minmax[0]) / (numberofPoints - 1);
    step[1] = step[0];

    int index = 0;
    for (int i = 0; i < numberofPoints; i++)
      for (int j = 0; j < numberofPoints; j++) {
        double[] angles = getPhiBeta(minmax[0] + i * step[0], minmax[2] + j * step[1]);
        if (!insideLimits(group, angles[0], angles[1]))
          value[i][j] = Double.NaN;
        else
          index++;
      }
    int pointToCompute = index;
    double[][] phibeta = new double[2][pointToCompute];
    index = 0;
    for (int i = 0; i < numberofPoints; i++)
      for (int j = 0; j < numberofPoints; j++) {
        if (!Double.isNaN(value[i][j])) {
          double[] angles = getPhiBeta(minmax[0] + i * step[0], minmax[2] + j * step[1]);
          phibeta[0][index] = angles[0];
          phibeta[1][index++] = angles[1];
        }
      }
    Texture texturemodel = aphase.getActiveTexture();
    if (texturemodel != null)
      singleValues = texturemodel.getInversePoleFigureGrid(texture_angles, phibeta);
    else
      return value;
/*    if (texturemodel != null)
      value = texturemodel.getInversePoleFigureGrid(texture_angles,
                                                    Math.PI/2.0, numberofPoints,
                                                    Math.PI/2.0, numberofPoints);*/
    index = 0;
    for (int i = 0; i < numberofPoints; i++)
      for (int j = 0; j < numberofPoints; j++) {
        if (!Double.isNaN(value[i][j])) {
          value[i][j] = singleValues[index++];
        }
      }
    return value;
  }

  public double[][] getPoleFigureGrid(int numberofPoints, double maxAngle) {
    double[][] value = null;
    Phase aphase = getParent();
    Texture texturemodel = aphase.getActiveTexture();
    if (texturemodel != null)
      value = texturemodel.getPoleFigureGrid(this, numberofPoints, maxAngle);
    if (value == null)
      return new double[numberofPoints][numberofPoints];
    return value;
  }

  public double[] getPoleFigureGrid(double[] x, double[] y) {
    Phase aphase = getParent();
    Texture texturemodel = aphase.getActiveTexture();
    if (texturemodel != null)
      y = texturemodel.getPoleFigureGrid(this, x, y);
    return y;
  }

  public double[][] getExpPoleFigureGrid() {
    Phase aphase = getParent();
    Sample asample = aphase.getFilePar().getActiveSample();
//    int numberExpPoints = asample.getNumberActiveDatafiles();

	  int index = aphase.getReflexIndex(this);
    int numberOfGoodPoints = 0;
    for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
	    DataFileSet dataset = asample.getActiveDataSet(i);
	    for (int j = 0; j < dataset.activedatafilesnumber(); j++) {
		    DiffrDataFile dataFile = dataset.getActiveDataFile(j);
		      if (dataFile.isPeakInsideRange(aphase, index) && !Double.isNaN(dataFile.getExperimentalTextureFactor(aphase, index, 0))) // todo v3.0 : only radnumber 0 ??
               numberOfGoodPoints++;
	    }
    }
    if(numberOfGoodPoints <= 0)
      return null;

    double[][] expTFAndAngles = new double[3][numberOfGoodPoints];
    numberOfGoodPoints = 0;
	  for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
		  DataFileSet dataset = asample.getActiveDataSet(i);
		  for (int j = 0; j < dataset.activedatafilesnumber(); j++) {
			  DiffrDataFile dataFile = dataset.getActiveDataFile(j);
			  if (dataFile.isPeakInsideRange(aphase, index)) {
				  double expTextureFactor = dataFile.getExperimentalTextureFactor(aphase, index, 0); // todo v3.0 : only radnumber 0 ??
					if (!Double.isNaN(expTextureFactor)) {
				  double[] texture_angles = dataFile.getTextureAngles(dataFile.getPosition(aphase, index, 0)); // todo v3.0 : only radnumber 0 ??
				  expTFAndAngles[0][numberOfGoodPoints] = texture_angles[0];
				  expTFAndAngles[1][numberOfGoodPoints] = texture_angles[1];
				  expTFAndAngles[2][numberOfGoodPoints++] = expTextureFactor;
					}
			  }
		  }
	  }
    return expTFAndAngles;
  }

  public double[][] getPoleFigureGridStrain(int numberofPoints, double maxAngle) {
    double[][] value = null;
    Phase aphase = getParent();
    Strain strainmodel = aphase.getActiveStrain();
    if (strainmodel != null)
      value = strainmodel.getPoleFigureGrid(this, numberofPoints, maxAngle);
    if (value == null)
      return new double[numberofPoints][numberofPoints];
    return value;
  }

  public double[][] getExpPoleFigureGridStrain(int numberofPoints, double maxAngle) {
    double[][] value = null;
    Phase aphase = getParent();
    Strain strainmodel = aphase.getActiveStrain();
    if (strainmodel != null)
      value = strainmodel.getExpPoleFigureGrid(this, numberofPoints, maxAngle);
    if (value == null)
      return new double[numberofPoints][numberofPoints];
    return value;
  }

  public double[][] getShapeAbsorptionPoleFigureGrid(int numberofPoints, double maxAngle, Sample asample) {
    double[][] value = null;
    if (asample != null)
      value = asample.getShapeAbsorptionPoleFigureGrid(this, numberofPoints, maxAngle);
    if (value == null)
      return new double[numberofPoints][numberofPoints];
    return value;
  }

/*  public double getShapeAbsorptionCorrection(DiffrDataFile adatafile, Sample asample) {
    return adatafile.getShapeAbsFactor();
  }
*/

  public String getInformationString() {
    StringBuffer astring = new StringBuffer("");
    astring.append(" ");
    astring.append(h);
    astring.append(" ");
    astring.append(k);
    astring.append(" ");
    astring.append(l);
    astring.append(" ");
    astring.append(multiplicity);
    astring.append(" ");
	  astring.append(d_space);
	  astring.append(" ");
	  double meanSF = 0.0;
/*  todo fix and re-enable
    if (structureFactors != null && structureFactors.length > 0) {
      for (double structureFactor1 : structureFactors)
        meanSF += structureFactor1;
      meanSF /= structureFactors.length;
    } else
      meanSF = structureFactor;

    astring.append(meanSF);
    astring.append(" ");
    astring.append(crystallite);
    astring.append(" ");
    astring.append(microstrain);
    astring.append(" ");*/
    return astring.toString();
  }

  public void updateForCellChange(Phase.CellOperation operation) {
    switch (operation) {
      case FORWARD:
        int h1 = l;
        l = k;
        k = h;
        h = h1;
	      h1 = base_l;
	      base_l = base_k;
	      base_k = base_h;
	      base_h = h1;
        break;
      case BACKWARD:
        h1 = h;
        h = k;
        k = l;
        l = h1;
	      h1 = base_h;
	      base_h = base_k;
	      base_k = base_l;
	      base_l = h1;
        break;
      case INVERT_A:
	      h = -h;
	      base_h = -base_h;
        break;
      case INVERT_B:
        k = -k;
	      base_k = -base_k;
        break;
      case INVERT_C:
        l = -l;
	      base_l = -base_l;
        break;
      case INVERT_ALPHA:
        break;
      case INVERT_BETA:
        break;
      case INVERT_GAMMA:
        break;
      case SWITCH_AB:
        h1 = h;
        h = k;
        k = h1;
	      h1 = base_h;
	      base_h = base_k;
	      base_k = h1;
        break;
      case SWITCH_BC:
        h1 = k;
        k = l;
        l = h1;
	      h1 = base_k;
	      base_k = base_l;
	      base_l = h1;
        break;
      case SWITCH_CA:
        h1 = h;
        h = l;
        l = h1;
	      h1 = base_h;
	      base_h = base_l;
	      base_l = h1;
        break;
    }
	  h2 = h * h;
	  k2 = k * k;
	  l2 = l * l;
	  base_h2 = base_h * base_h;
	  base_k2 = base_k * base_k;
	  base_l2 = base_l * base_l;
    int mult2 = hlist.length;
    for (int i = 0; i < mult2; i++) {
      switch (operation) {
        case FORWARD:
          int h1 = llist[i];
          llist[i] = klist[i];
          klist[i] = hlist[i];
          hlist[i] = h1;
          break;
        case BACKWARD:
          h1 = hlist[i];
          hlist[i] = klist[i];
          klist[i] = llist[i];
          llist[i] = h1;
          break;
        case INVERT_A:
          hlist[i] = -hlist[i];
          break;
        case INVERT_B:
          klist[i] = -klist[i];
          break;
        case INVERT_C:
          llist[i] = -llist[i];
          break;
        case INVERT_ALPHA:
          break;
        case INVERT_BETA:
          break;
        case INVERT_GAMMA:
          break;
        case SWITCH_AB:
          h1 = hlist[i];
          hlist[i] = klist[i];
          klist[i] = h1;
          break;
        case SWITCH_BC:
          h1 = klist[i];
          klist[i] = llist[i];
          llist[i] = h1;
          break;
        case SWITCH_CA:
          h1 = hlist[i];
          hlist[i] = llist[i];
          llist[i] = h1;
          break;
      }
    }
  }

  public void writeCustomObject(BufferedWriter out) {
/*
    try {
      out.newLine();
      out.write("#custom_object_" + "reflection");
      out.newLine();

      public int h = 0, k = 0, l = 0, multiplicity = 0;  // todo make it private
      public double d_space = 0;
//  public double[] crystallite = null;
//  public double[] microstrain = null;
      private double textureFactor = 1.0;
      private double textureFactors[] = null;
      private double expTextureFactors[] = null;
      private double strain = 0.0;
      private double strains[] = null;
      public double expStrains[] = null;
      private double structureFactor = 1.0;
      private double structureFactors[] = null;
      public double expStructureFactors[] = null;
      public double esdStructureFactors[] = null;
      public double shapeAbsFactor = 1.0f;
      public double[] shapeAbsFactors = null;
      public double microstrain = 0.0;
      public double microstrains[] = null;
      public double expMicrostrains[] = null;
      public double crystallite = 0.0;
      public double crystallites[] = null;
      public double expCrystallites[] = null;
      public int numberDataFiles = 0;
      public int numberDataSets = 0;
      public double textureWeight = 1.0;
      public double textureOverlappedWeight = 0.0;
//	public double strainweight = 1.0;
      public boolean goodforTexture = true;
      public boolean goodforStrain = true;
      public boolean goodforStructureFactor = true;
      public boolean poleFigurePlot = false;
      public int izoveri = 1;
      Phase phase = null;
      public int[] hlist = null, klist = null, llist = null;

      public int Lsum = 0;
      public int Labssum = 0;
      public int Lsumdivideabs = 0;
      public double hzero = 0;
      public int hzero2 = 0;
      public int broadened = 0;
      public double[] phi = null, beta = null;
      
      for (int i = 0; i < wave.length && i < asample.datasetsNumber(); i++) {
        DataFileSet adataset = asample.getDataSet(i);
        String blockID = "dataset_" + adataset.toXRDcatString();
        CIFDataBlock.writeBlockDecl(out, blockID, this);
        out.newLine();
        out.write(CIFdictionary.loopDecl);
        out.newLine();
        out.write(CIFdictionary.refln_h + " ");
        out.write(CIFdictionary.refln_k + " ");
        out.write(CIFdictionary.refln_l + " ");
        out.write(CIFdictionary.refln_FsquaredMeas + " ");
        out.write(CIFdictionary.refln_FsquaredCalc + " ");
        out.write(CIFdictionary.refln_FsquaredEsd + " ");
        out.write(CIFdictionary.refln_wavelength);
        out.newLine();
        String waveS = Fmt.format(wave[i]);

        int hkln = aphase.gethklNumber();
        for (int j = 0; j < hkln && j < dspacing.length; j++) {
          out.write(hklm[0][j] + " " + hklm[1][j] + " " + hklm[2][j] + " " +
              Fmt.format(Fhkl[0][j][i]) + " " + Fmt.format(Fhkl[1][j][i]) + " " +
              Fmt.format(Fhkl[2][j][i]) + " " + " " + waveS);
          out.newLine();
        }
        out.newLine();
      }
      out.newLine();
      out.write("#end_custom_object_" + "reflection");
      out.newLine();
      out.newLine();
    } catch (IOException ioe) {
      System.out.println("Error in writing the data for reflection: " + h + " " + k + " " + l);
    }*/

  }

  public void readCustomObject(CIFtoken ciffile) {/*
    // to be override by subclasses
    // the default read and do nothing

    int newtoken, tokentype;
//		XRDcat theobj = null;
    boolean endofInput = false;
    int hklindex = 0, datasetindex = 0, cifentry = 0, tmpVindex = 0;
    int[] cifindex = new int[7];
    Vector cifVector = new Vector(0, 1);
    Vector[] tmpVector = null;
    Vector overallVector = new Vector(0, 1);
    boolean newLoop = false;
    boolean startLoop = false;
    int maxCIFentries = 7;

    try {
      do {
        tokentype = ciffile.nextToken();
        switch (tokentype) {
          case CIFtoken.TT_DATA:
          case CIFtoken.TT_DATASET:
            if (tmpVector != null) {
              overallVector.addElement(tmpVector);
            }
            datasetindex++;
            tmpVindex = 0;
            tmpVector = null;
            cifVector = new Vector(0, 1);
            cifentry = 0;
            newLoop = false;
            break;
          case CIFtoken.TT_CIFE:
            // CIF item
            cifVector.addElement(ciffile.thestring);
            break;
          case CIFtoken.TT_LOOP:
            // start the loop for the values here
            newLoop = true;
            startLoop = true;
            break;
          case CIFtoken.TT_NUMBER:
            if (!newLoop)
              break;
            if (startLoop) {
              cifindex = new int[cifVector.size()];
              tmpVindex = 0;
              for (int i = 0; i < cifVector.size(); i++) {
                String thecife = (String) cifVector.elementAt(i);
                if (thecife.equalsIgnoreCase(CIFdictionary.refln_h)) {
                  cifindex[i] = 0;
                  tmpVindex++;
                } else if (thecife.equalsIgnoreCase(CIFdictionary.refln_k)) {
                  cifindex[i] = 1;
                  tmpVindex++;
                } else if (thecife.equalsIgnoreCase(CIFdictionary.refln_l)) {
                  cifindex[i] = 2;
                  tmpVindex++;
                } else if (thecife.equalsIgnoreCase(CIFdictionary.refln_FsquaredMeas)) {
                  cifindex[i] = 3;
                  tmpVindex++;
                } else if (thecife.equalsIgnoreCase(CIFdictionary.refln_FsquaredCalc)) {
                  cifindex[i] = 4;
                  tmpVindex++;
                } else if (thecife.equalsIgnoreCase(CIFdictionary.refln_FsquaredEsd)) {
                  cifindex[i] = 5;
                  tmpVindex++;
                } else if (thecife.equalsIgnoreCase(CIFdictionary.refln_wavelength)) {
                  cifindex[i] = 6;
                  tmpVindex++;
                } else
                  cifindex[i] = -1;
              }
              startLoop = false;
              cifVector.removeAllElements();
            }
            if (tmpVector == null) {
              tmpVector = new Vector[maxCIFentries];
              for (int i = 0; i < maxCIFentries; i++)
                tmpVector[i] = new Vector(0, 10);
            }
            if (cifindex[cifentry] >= 0) {
              double[] value = new double[1];
              value[0] = ciffile.thevalue;
              tmpVector[cifindex[cifentry]].addElement(value);
            }
            cifentry++;
            if (cifentry == tmpVindex)
              cifentry = 0;
            break;
          case CIFtoken.TT_CUSTOM_END:
            if (tmpVector != null) {
              overallVector.addElement(tmpVector);
            }
            endofInput = true;
            break;
          default: {
          }
        }
      } while (tokentype != CIFtoken.TT_EOF && !endofInput);

      datasetindex = overallVector.size();
      hklindex = 0;
      for (int i = 0; i < datasetindex; i++) {
        if (i == 0) {
          tmpVector = (Vector[]) overallVector.elementAt(i);
          tmpVindex = tmpVector.length;
          if (tmpVindex > 0)
            hklindex = tmpVector[0].size();
          initializeMatrices(hklindex, datasetindex);
        }
        if (hklindex > 0) {
          double[] value = (double[]) tmpVector[6].elementAt(0);
          wave[i] = value[0];
          for (int j = 0; j < hklindex; j++) {
            if (i == 0) {
              value = (double[]) tmpVector[0].elementAt(j);
              hklm[0][j] = (int) value[0];
              value = (double[]) tmpVector[1].elementAt(j);
              hklm[1][j] = (int) value[0];
              value = (double[]) tmpVector[2].elementAt(j);
              hklm[2][j] = (int) value[0];
            }
            if (j < tmpVector[3].size()) {
              value = (double[]) tmpVector[3].elementAt(j);
              Fhkl[0][j][i] = value[0];
            }
            if (j < tmpVector[4].size()) {
              value = (double[]) tmpVector[4].elementAt(j);
              Fhkl[1][j][i] = value[0];
            }
            if (j < tmpVector[5].size()) {
              value = (double[]) tmpVector[5].elementAt(j);
              Fhkl[2][j][i] = value[0];
            }
          }
        }
      }
//      System.out.println("Custom object loaded!");
      needRestore = true;
      notLoaded = false;
    } catch (IOException ioe) {
      System.out.println("IO exception in custom object for " + toXRDcatString());
    }*/

/*		if (theobj != null)
			theobj.readall(ciffile);*/
  }



}
