/*
 * @(#)SampleShapeIntegral.java created 07/03/2003 Berkeley
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.shape;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.io.cif.CIFtoken;
import it.unitn.ing.rista.render3d.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.comp.*;

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import java.util.Vector;

/**
 *  The SampleShapeIntegral is a class
 *
 *
 * @version $Revision: 1.15 $, $Date: 2006/07/20 13:39:06 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SampleShapeIntegral extends SampleShape implements Shape3D, SimpleFunction {

  public static int Ndivision = MaudPreferences.getInteger("sampleShapeIntegral.integralDivisions", 8);
  public static int maxCount = MaudPreferences.getInteger("sampleShapeIntegral.maxCount", 500);

  protected static String[] diclistc = {
    "_rita_sample_symmetry", "_rita_harmonic_expansion_degree", "_rita_shape_abs_velocity_corr",
    "_rita_shape_fe_division_number",
    "_pd_spec_orientation_omega", "_pd_spec_orientation_chi", "_pd_spec_orientation_phi",
    "_riet_par_sampleshape_model_weight", "_riet_par_standard_size",

    "_riet_par_sampleshape_size"
  };
  protected static String[] diclistcrm = {
    "_rita_sample_symmetry", "_rita_harmonic_expansion_degree", "_rita_shape_abs_velocity_corr",
    "_rita_shape_fe_division_number",
    "ref. system omega (deg)", "ref. system chi (deg)", "ref. system phi (deg)",
    "model weight", "standard mean size (mm)",
    
    "shape coeff "
  };

  protected static String[] classlistc = {};

  protected static String[] classlistcs = {};

  public static String[] symmetrychoice = {"-1",
                                           "2/m",
                                           "2/mmm",
                                           "4/m",
                                           "4/mmm",
                                           "-3",
                                           "-3m",
                                           "6/m",
                                           "6/mmm",
                                           "m3",
                                           "m3m",
                                           "cylinder"};

  int sampleSymmetry = 0;
  int LGIndex = 0;
  int expansionDegree = 4;
  double omega = 0.0;
  double chi = 0.0;
  double phi = 0.0;
  double[][] res = new double[3][3];
  double[] shapeParameters = null;
  boolean velocityCorrection = false;

  public SampleShapeIntegral(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Harmonic Integration";
    IDlabel = "Harmonic Integration";
    description = "Shape described by harmonic coefficients, numerical integration";
  }

  public SampleShapeIntegral(XRDcat aobj) {
    this(aobj, "Shape described by harmonic coefficients, numerical integration");
  }

  public SampleShapeIntegral() {
    identifier = "Harmonic Integration";
    IDlabel = "Harmonic Integration";
    description = "Shape described by harmonic coefficients, numerical integration";
  }


  public void initConstant() {
    Nstring = 4;
    Nstringloop = 0;
    Nparameter = 5;
    Nparameterloop = 1;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
  }

  public void initParameters() {
    super.initParameters();
    stringField[2] = "false";
    stringField[3] = MaudPreferences.getPref("sampleShapeIntegral.integralDivisions", "10");
    setSampleSymmetry(0);
    setHarmonicExpansion(4);
    parameterField[3].setValue("0.3");
    parameterField[3].setPositiveOnly();
    parameterField[4].setValue("1.0");
    parameterField[4].setPositiveOnly();
	  applySymmetryRules();
  }

	public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
            notifyParameterChanged(source, Constants.SHAPE_ABSORPTION_CHANGED);
            return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.SHAPE_ABSORPTION_CHANGED);
            return;
          }

      super.notifyParameterChanged(source);
    }
  }

  public String getSampleSymmetry() {
    return stringField[0];
  }

  public int getSampleSymmetryValue() {

    String samplesym = getSampleSymmetry();

    for (int i = 0; i < symmetrychoice.length; i++) {
      if (samplesym.equals(symmetrychoice[i]))
        return i;
    }
    return 0;
  }

  public void setSampleSymmetry(int i) {
    stringField[0] = new String(symmetrychoice[i]);
  }

  public void setSampleSymmetry(String value) {
    stringField[0] = new String(value);
  }

  public String getHarmonicExpansion() {
    return stringField[1];
  }

  public int getHarmonicExpansionValue() {
    return Integer.valueOf(getHarmonicExpansion()).intValue();
  }

  public void setHarmonicExpansion(int i) {
    setHarmonicExpansion(Integer.toString(i));
  }

  public void setHarmonicExpansion(String value) {
    stringField[1] = new String(value);
  }

  public void setVelocityCorrection(boolean corr) {
    if (corr)
      stringField[2] = "true";
    else
      stringField[2] = "false";
  }

  public boolean getVelocityCorrection() {
    if (stringField[2].equalsIgnoreCase("true"))
      return true;
    return false;
  }

  public boolean velocityCorrection() {
    return velocityCorrection;
  }

  public Parameter getOmega() {
    return parameterField[0];
  }

  public Parameter getChi() {
    return parameterField[1];
  }

  public Parameter getPhi() {
    return parameterField[2];
  }

  public void setChi(String astring) {
    parameterField[1].setValue(astring);
  }

  public void setPhi(String astring) {
    parameterField[2].setValue(astring);
  }

  public double getShapeOmegaD() {
    return omega;
  }

  public double getShapeChiD() {
    return chi;
  }

  public double getShapePhiD() {
    return phi;
  }

  public ListVector getShapeCoefficientsList() {
    return parameterloopField[0];
  }

  public int numberShapeParameters() {
    return getShapeCoefficientsList().size();
  }

  public Parameter getShapeCoefficient(int index) {
    return (Parameter) getShapeCoefficientsList().elementAt(index);
  }

  public double getShapeValue(int index) {
/*    Parameter shape = getShapeCoefficient(index);
    if (shape != null)
      return shape.getValueD();
    else
      return 0.0;     */
    return shapeParameters[index];
  }

  public void setExpansionDegree(int value) {
    setHarmonicExpansion(value);
    if (expansionDegree != value) {
      expansionDegree = value;
      checkShapeParameters();
    }
  }

  public void checkShapeParameters() {
    int numberShapeCoefficients = getNumberHarmonics();
    int actualNumber = numberShapeParameters();

    isAbilitatetoRefresh = false;
    if (actualNumber < numberShapeCoefficients) {
      for (int i = actualNumber; i < numberShapeCoefficients; i++) {
        if (i == 0.0)
          addparameterloopField(0, new Parameter(this, getParameterString(0, i), 1,
                  ParameterPreferences.getDouble(getParameterString(0, i) + ".min", 0.01),
                  ParameterPreferences.getDouble(getParameterString(0, i) + ".max", 20)));
        else
          addparameterloopField(0, new Parameter(this, getParameterString(0, i), 0,
                  ParameterPreferences.getDouble(getParameterString(0, i) + ".min", -10),
                  ParameterPreferences.getDouble(getParameterString(0, i) + ".max", 10)));
      }
    }
    if (actualNumber > numberShapeCoefficients) {
      for (int i = actualNumber - 1; i >= numberShapeCoefficients; i--)
        getShapeCoefficientsList().removeItemAt(i);
    }
    isAbilitatetoRefresh = true;
  }

  public double getMeanShape() {
    return getShapeValue(0);
  }

  double phicosphi[] = new double[2];

  public double getShape(double azimuthal, double polar) {   // angles in radiant
    int index = 0;
//    polar *= Constants.DEGTOPI;
//    azimuthal *= Constants.DEGTOPI;
    double sinPolar = Math.sin(polar);
    double cosPolar = Math.cos(polar);
    double sinAzimuthal = Math.sin(azimuthal);
    double cosAzimuthal = Math.cos(azimuthal);

    double[] xyz = {sinPolar * cosAzimuthal, sinPolar * sinAzimuthal, cosPolar};

    double xp = res[0][0] * xyz[0] + res[0][1] * xyz[1] + res[0][2] * xyz[2];
    double yp = res[1][0] * xyz[0] + res[1][1] * xyz[1] + res[1][2] * xyz[2];
    double zp = res[2][0] * xyz[0] + res[2][1] * xyz[1] + res[2][2] * xyz[2];

    if (zp < 0.99999999 || zp > -0.99999999) {
      phicosphi[0] = Angles.getAngleR(xp, yp);
      phicosphi[1] = Math.acos(zp);
    } else {
      phicosphi[0] = 0.0;
      if (zp == 1) {
        phicosphi[1] = 0.0;
      } else {
        phicosphi[1] = Math.PI;
      }
    }

//		phicosphi[1] = polar * Constants.DEGTOPI;
//		phicosphi[0] = azimuthal * Constants.DEGTOPI;

    double Rh = getMeanShape();

    for (int i = 2; i <= expansionDegree; i += 2) {
      int nl2 = SphericalHarmonics.getN(LGIndex, i);
      for (int n = 1; n <= nl2; n++) {

        Rh += getShapeValue(++index)
                * SphericalHarmonics.getSphericalHarmonic(LGIndex, i, n,
                        phicosphi[0], phicosphi[1]);
      }
    }
    return Math.abs(Rh);
  }

  public double getShapeR(double[] xyz) {
    int index = 0;
    double xp = res[0][0] * xyz[0] + res[0][1] * xyz[1] + res[0][2] * xyz[2];
    double yp = res[1][0] * xyz[0] + res[1][1] * xyz[1] + res[1][2] * xyz[2];
    double zp = res[2][0] * xyz[0] + res[2][1] * xyz[1] + res[2][2] * xyz[2];

    if (zp != 1 || zp != -1) {
      phicosphi[0] = Angles.getAngleR(xp, yp);
      phicosphi[1] = Math.acos(zp);
    } else {
      phicosphi[0] = 0.0;
      if (zp == 1) {
        phicosphi[1] = 0.0;
      } else {
        phicosphi[1] = Math.PI;
      }
    }

//		phicosphi[1] = polar * Constants.DEGTOPI;
//		phicosphi[0] = azimuthal * Constants.DEGTOPI;

    double Rh = getMeanShape();

    for (int i = 2; i <= expansionDegree; i += 2) {
      int nl2 = SphericalHarmonics.getN(LGIndex, i);
      for (int n = 1; n <= nl2; n++) {

        Rh += getShapeValue(++index)
                * SphericalHarmonics.getSphericalHarmonic(LGIndex, i, n,
                        phicosphi[0], phicosphi[1]);
      }
    }
    return Math.abs(Rh);
  }

  public double getNormalizedShape(double azimuthal, double polar) {

    double Rh = getMeanShape();

    if (Rh == 0.0)
      return 1.0;
    else
      return getShape(azimuthal * Constants.DEGTOPI, polar * Constants.DEGTOPI) / Rh;
  }

  public double getNormalizedShapeR(double azimuthal, double polar) {

    double Rh = getMeanShape();

    if (Rh == 0.0)
      return 1.0;
    else
      return getShape(azimuthal, polar) / Rh;
  }

  public int getNumberHarmonics() {

    int index = 1;
    for (int i = 2; i <= expansionDegree; i += 2) {
      int nl2 = SphericalHarmonics.getN(LGIndex, i);
      for (int n = 1; n <= nl2; n++)
        ++index;
    }
    return index;
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    isAbilitatetoRefresh = false;
    applySymmetryRules();
    isAbilitatetoRefresh = true;

    super.updateParametertoDoubleBuffering(false);

    omega = getParameterValue(0);
    chi = getParameterValue(1);
    phi = getParameterValue(2);

    double sinOmega = MoreMath.sind(omega+90.0);
    double cosOmega = MoreMath.cosd(omega+90.0);
    double sinCsi = MoreMath.sind(chi);
    double cosCsi = MoreMath.cosd(chi);
    double sinPhi = MoreMath.sind(phi);
    double cosPhi = MoreMath.cosd(phi);
    double[][] tomega = {
      {cosOmega, -sinOmega, 0.0},
      {sinOmega, cosOmega, 0.0},
      {0.0, 0.0, 1.0}
    };
    double[][] tchi = {
      {1.0, 0.0, 0.0},
      {0.0, cosCsi, -sinCsi},
      {0.0, sinCsi, cosCsi}
    };
    double[][] tphi = {
      {cosPhi, -sinPhi, 0.0},
      {sinPhi, cosPhi, 0.0},
      {0.0, 0.0, 1.0}
    };

    res = MoreMath.MatProduct(tphi, tchi, 3, 3, 3);
    res = MoreMath.MatProduct(res, tomega, 3, 3, 3);

    shapeParameters = getParameterLoopVector(0);

    Ndivision = Integer.parseInt(stringField[3]);

      parameterField[3].setPositiveOnly();
      parameterField[4].setPositiveOnly();
  
    prepareFiniteElementsDescription();
    updateStringtoDoubleBuffering(false);
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    velocityCorrection = getVelocityCorrection();
  }

  public void applySymmetryRules() {
    LGIndex = getSampleSymmetryValue();
    expansionDegree = getHarmonicExpansionValue();
    checkShapeParameters();
  }

  protected double step = 1.0;
  double[][] cellList = null;
  int Ncell = 0;
  double Volume = 0.0;
  double step10 = step / 10.0;
  double step4 = step / 4.0;

  protected void prepareFiniteElementsDescription() {
    updateStringtoDoubleBuffering(false); // update meanSize
    step = getMeanShape() / Ndivision;
    step10 = step * MaudPreferences.getDouble("sampleShapeIntegral.speedCheckBorder", 0.001);
    step4 = step * MaudPreferences.getDouble("sampleShapeIntegral.emptyBorderFactor", 0.2);
    Vector elementList = new Vector(300, 100);
    boolean addition = addElement(elementList, 0, 0, 0);
    int N = 1;
    while (addition) {
      addition = false;
      int N2 = 2 * N;
      int Nm1 = 1 - N;
      for (int k = -N; k <= N; k += N2)
        for (int i = Nm1; i < N; i++)
          for (int j = Nm1; j < N; j++) {
            boolean newElement = addElement(elementList, i, j, k);
            addition = addition || newElement;
          }
      for (int i = -N; i <= N; i += N2)
        for (int k = Nm1; k < N; k++)
          for (int j = Nm1; j < N; j++) {
            boolean newElement = addElement(elementList, i, j, k);
            addition = addition || newElement;
          }
      for (int j = -N; j <= N; j += N2)
        for (int i = Nm1; i < N; i++)
          for (int k = Nm1; k < N; k++) {
            boolean newElement = addElement(elementList, i, j, k);
            addition = addition || newElement;
          }
      N++;
    }
    Volume = 0.0;
    Ncell = elementList.size(); // System.out.println(Ncell);
    cellList = new double[Ncell][5];
    for (int i = 0; i < Ncell; i++) {
      double[] cell = (double[]) elementList.get(i);
      for (int j = 0; j < 5; j++)
        cellList[i][j] = cell[j];
      Volume += cell[4];
    }
    elementList.removeAllElements();
    elementList = null;
  }

  protected boolean addElement(Vector elementList, int i1, int i2, int i3) {
    double x1 = i1 * step;
    double y1 = i2 * step;
    double z1 = i3 * step;
    double equatdist = x1 * x1 + y1 * y1;
    double dist = Math.sqrt(equatdist + z1 * z1);
    double ang1 = Angles.getAngleR(x1, y1);
    double r = Math.sqrt(equatdist);
    double ang2 = 0.0;
    if (r != 0.0)
      ang2 = Angles.getAngleR(z1, r);
    r = getShape(ang1, ang2);
//    System.out.println(Math.sqrt(dist) + " " + r + " " + x1 + " " + y1 + " " + z1);
    if (dist + step4 < r) {
      double[] cell = new double[5];
      cell[0] = x1;
      cell[1] = y1;
      cell[2] = z1;
      double residual = r - dist;
      cell[3] = residual; //getMeanShape(); //r;
      if (residual > step / 2.0)
        cell[4] = 1.0;
      else {
        cell[4] = residual / step;
      }
      elementList.add(cell);
      return true;
    }
    return false;
  }

  public void computeAbsorptionPath_disabled(double[][] incidentAndDiffraction_angles, double absorption, double[] position,
                                    double[] intensity, double toLambda) {
    double dist1[] = new double[Ncell], dist2[] = new double[Ncell], arg1 = absorption;
    boolean positionCorr = false;
    if (velocityCorrection() && toLambda != 0.0f) {
      absorption *= toLambda;
      positionCorr = true;
    }
//    System.out.println(position[0]);
//    System.out.println(incidentAndDiffraction_angles[0][0] + " " + incidentAndDiffraction_angles[0][1]);
//    System.out.println(incidentAndDiffraction_angles[0][2] + " " + incidentAndDiffraction_angles[0][3]);
//    System.out.println(incidentAndDiffraction_angles[0][4] + " " + incidentAndDiffraction_angles[0][5]);
    int iterations = MaudPreferences.getInteger("sampleShapeIntegral.iterationsNumber", 15);
    LeastSquareFit solver = new LeastSquareFit(this, iterations);
    solver.outputEnabled = false;
    solver.setPrecision(0.000001);
    solver.setDerivateStep(0.001);
    double wssLimit = 1E-8;
    solver.wssLimit = wssLimit;
    wssLimit *= 100;
    int time = (int) System.currentTimeMillis();  // safe because we're getting low-order bits
    ec.util.MersenneTwisterFast randomizer = new ec.util.MersenneTwisterFast(time);
    double[] coord = new double[3];
    double[] wgt = new double[3];
    double[] incdta = new double[3];
    double[] diffrdta = new double[3];
    double[] fit = new double[3];
    double[] diffrParm = new double[2];
    double[] incParm = new double[2];
    for (int i = 0; i < 3; i++)
      wgt[i] = 1.0f;
    int[] controls = new int[1];
    double wss = 0.0;

    for (int k = 0; k < 2; k++) {
      incParm[k] = incidentAndDiffraction_angles[0][k];
      diffrParm[k] = incidentAndDiffraction_angles[0][k+2];
    }
    for (int i = 0; i < position.length; i++) {
//      System.out.println("position " + i);
//      System.out.flush();
      double totalPath = 0.0;
      boolean asPrevious = true;
      if (i == 0)
        asPrevious = false;
      else
        for (int j = 0; j < 6; j++)
          if (incidentAndDiffraction_angles[i][j] != incidentAndDiffraction_angles[i - 1][j])
            asPrevious = false;
      if (positionCorr)
        arg1 = absorption * position[i];// * Constants.LAMBDA_SPEED_NEUTRON_CONV_REC;

      computeNormCoord(incdta, incidentAndDiffraction_angles[i][1], incidentAndDiffraction_angles[i][0]);
      computeNormCoord(diffrdta, incidentAndDiffraction_angles[i][3], incidentAndDiffraction_angles[i][2]);
      for (int k = 0; k < 6; k+=2) {
        System.out.println(i+ " " + k +" " + incidentAndDiffraction_angles[i][k] * Constants.PITODEG + " " +
                           incidentAndDiffraction_angles[i][k+1] * Constants.PITODEG);
      }
      for (int j = 0; j < Ncell; j++) {
        if (!asPrevious) {

          System.out.println(cellList[j][0]+" "+cellList[j][1]+" "+cellList[j][2]);

          controls[0] = j;
          // incident ray
          double dist1c = computeDistance(cellList[j], incidentAndDiffraction_angles[i][1],
                  incidentAndDiffraction_angles[i][0]);
          wss = 0;
          do {
            if (wss > wssLimit) {
              incParm[0] = (double) (Math.PI * randomizer.nextDouble());
              incParm[1] = (double) (Math.PI * 2.0 * randomizer.nextDouble());
            }
            wss = solver.simpleSolve(incdta, wgt, fit, incParm, false, controls);
          } while (wss > wssLimit);
          computeCoord(coord, incParm[1], incParm[0]); // attention, we need to invert
          dist1[j] = 0.0;
          for (int ij = 0; ij < 3; ij++) {
            coord[ij] -= cellList[j][ij];
            dist1[j] += coord[ij] * coord[ij];
          }
          dist1[j] = Math.sqrt(dist1[j]);
          System.out.println("Distance " + dist1[j] + " " + dist1c);
          // diffracted ray
          double dist2c = computeDistance(cellList[j], incidentAndDiffraction_angles[i][3],
                  incidentAndDiffraction_angles[i][2]);

          wss = 0;
          do {
            if (wss > wssLimit) {
              diffrParm[0] = (double) (Math.PI * randomizer.nextDouble());
              diffrParm[1] = (double) (Math.PI * 2.0 * randomizer.nextDouble());
            }
            wss = solver.simpleSolve(diffrdta, wgt, fit, diffrParm, false, controls);
          } while (wss > wssLimit);
          computeCoord(coord, diffrParm[1], diffrParm[0]); // attention, we need to invert
          dist2[j] = 0.0;
          for (int ij = 0; ij < 3; ij++) {
            coord[ij] -= cellList[j][ij];
            dist2[j] += coord[ij] * coord[ij];
          }
          dist2[j] = Math.sqrt(dist2[j]);
          System.out.println("Distance " + dist2[j] + " " + dist2c);
        }
//      System.out.println(i + " " + dist1 + " " + dist2);

        double arg2 = arg1 * (dist1[j] + dist2[j]);
        if (arg2 < 200.0)
          totalPath += Math.exp(-arg2) * cellList[j][4];
      }
//System.out.println(i + " " + totalPath);
      intensity[i] *= totalPath / Volume;
    }
    solver.releaseMemory();
    solver = null;
  }

  public void computeCoord(double[] coord, double azimuth, double polar) {

    double sinPolar = Math.sin(polar);
    double cosPolar = Math.cos(polar);
    double sinAzimuthal = Math.sin(azimuth);
    double cosAzimuthal = Math.cos(azimuth);

    coord[0] = (double) (sinPolar * cosAzimuthal);
    coord[1] = (double) (sinPolar * sinAzimuthal);
    coord[2] = (double) cosPolar;

    double r = getShapeR(coord);

    coord[0] = (double) (r * cosAzimuthal * sinPolar);
    coord[1] = (double) (r * sinAzimuthal * sinPolar);
    coord[2] = (double) (r * cosPolar);

  }

  public void computeNormCoord(double[] coord, double azimuth, double polar) {

    double sinPolar = Math.sin(polar);
    double cosPolar = Math.cos(polar);
    double sinAzimuthal = Math.sin(azimuth);
    double cosAzimuthal = Math.cos(azimuth);

    coord[0] = (double) (sinPolar * cosAzimuthal);
    coord[1] = (double) (sinPolar * sinAzimuthal);
    coord[2] = (double) cosPolar;
  }

  double coord[] = new double[3];

  public void refreshFit(double[] fit, double[] parm, int[] controls) {
    int cellIndex = controls[0];
    computeCoord(fit, parm[1], parm[0]); // attention, we need to invert
    for (int i = 0; i < 3; i++)
      fit[i] -= cellList[cellIndex][i];
//    computeAnglesR(fit, coord);
    double sum = 0.0f;
    for (int i = 0; i < 3; i++)
      sum += fit[i] * fit[i];
    sum = (double) Math.sqrt(sum);
    for (int i = 0; i < 3; i++)
      fit[i] /= sum;
  }

  public void computeAnglesR(double[] angles, double[] coord) { // attention first polar then azimuth
    double x1 = coord[0];
    double y1 = coord[1];
    double z1 = coord[2];
    double eqdist = x1 * x1 + y1 * y1;
    angles[1] = (double) Angles.getAngleR(x1, y1);
    double r = Math.sqrt(eqdist);
    angles[0] = 0.0f;
    if (r != 0.0)
      angles[0] = (double) Angles.getAngleR(z1, r);
  }

  public void computeAbsorptionPath(double[][] incidentAndDiffraction_angles, double absorption, double[] position,
                                    double[] intensity, double toLambda) {
    double dist1[] = new double[Ncell], dist2[] = new double[Ncell], arg1 = absorption;
    double corrPath = 0.0;
    boolean positionCorr = false;
    if (velocityCorrection() && toLambda != 0.0f) {
      absorption *= toLambda;
      positionCorr = true;
    }
//    System.out.println(position[0]);
//    System.out.println(incidentAndDiffraction_angles[0][0] + " " + incidentAndDiffraction_angles[0][1]);
//    System.out.println(incidentAndDiffraction_angles[0][2] + " " + incidentAndDiffraction_angles[0][3]);
//    System.out.println(incidentAndDiffraction_angles[0][4] + " " + incidentAndDiffraction_angles[0][5]);
    for (int i = 0; i < position.length; i++) {
//      System.out.println("position " + i);
//      System.out.flush();
      double totalPath = 0.0;
      boolean asPrevious = true;
      if (i == 0)
        asPrevious = false;
      else
        for (int j = 0; j < 6; j++)
          if (incidentAndDiffraction_angles[i][j] != incidentAndDiffraction_angles[i - 1][j])
            asPrevious = false;
      if (positionCorr)
        arg1 = absorption * position[i];// * Constants.LAMBDA_SPEED_NEUTRON_CONV_REC;

      if (!asPrevious) {
        double sin0 = Math.sin(incidentAndDiffraction_angles[i][0]);
        double sin1 = Math.sin(incidentAndDiffraction_angles[i][1]);
        double sin2 = Math.sin(incidentAndDiffraction_angles[i][2]);
        double sin3 = Math.sin(incidentAndDiffraction_angles[i][3]);
        double cos0 = Math.cos(incidentAndDiffraction_angles[i][0]);
        double cos1 = Math.cos(incidentAndDiffraction_angles[i][1]);
        double cos2 = Math.cos(incidentAndDiffraction_angles[i][2]);
        double cos3 = Math.cos(incidentAndDiffraction_angles[i][3]);
        double xdiffs = cos1 * sin0 - cos3 * sin2;
        double ydiffs = sin1 * sin0 - sin3 * sin2;
        double zdiffs = cos0 - cos2;
        xdiffs *= xdiffs;
        ydiffs *= ydiffs;
        zdiffs *= zdiffs;
        corrPath = ((1 - getParameterValue(3)) * Math.sqrt(xdiffs + ydiffs + zdiffs)
                    + getParameterValue(3)) * getParameterValue(4);
      }
      for (int j = 0; j < Ncell; j++) {
        if (!asPrevious) {
          // incident ray
          dist1[j] = computeDistance(cellList[j], incidentAndDiffraction_angles[i][1],
                  incidentAndDiffraction_angles[i][0]);
          // diffracted ray
          dist2[j] = computeDistance(cellList[j], incidentAndDiffraction_angles[i][3],
                  incidentAndDiffraction_angles[i][2]);
        }

        double arg2 = arg1 * (dist1[j] + dist2[j]);
        if (arg2 < 200.0)
          totalPath += Math.exp(-arg2) * cellList[j][4];
      }
      double arg3 = arg1 * corrPath;
      if (arg3 < 200.0)
        totalPath *= Math.exp(arg3);
//System.out.println(i + " " + totalPath);
      intensity[i] *= totalPath / Volume;
    }
  }

  protected double computeDistance(double[] acell, double angle1, double angle2) {
    double distR = acell[3] / 2.0;
    double stepR = distR / 2.0;
    boolean inside = true;
    double x1 = acell[0], y1 = acell[1], z1 = acell[2];
    double xpart = Math.cos(angle1) * Math.sin(angle2);
    double ypart = Math.sin(angle1) * Math.sin(angle2);
    double zpart = Math.cos(angle2);

    double xt = x1 + distR * xpart;
    double yt = y1 + distR * ypart;
    double zt = z1 + distR * zpart;
    double eqdist = xt * xt + yt * yt;
    double dist = eqdist + zt * zt;
    double ang1 = Angles.getAngleR(xt, yt);
    double r = Math.sqrt(eqdist);
    double ang2 = 0.0;
    if (r != 0.0)
      ang2 = Angles.getAngleR(zt, r);
    r = getShape(ang1, ang2);
    if (dist > r * r)
      inside = false;

    int count = 0;
//    System.out.println("X: " + acell[0] + " " + acell[1] + " " + acell[2]);
    while (stepR > step10 && count < maxCount) {
      count++;
      if (!inside) {
        while (distR < stepR)
          stepR /= 2.0;
        distR -= stepR;
      } else
        distR += stepR;

      xt = x1 + distR * xpart;
      yt = y1 + distR * ypart;
      zt = z1 + distR * zpart;
      eqdist = xt * xt + yt * yt;
      dist = eqdist + zt * zt;
      ang1 = Angles.getAngleR(xt, yt);
      r = Math.sqrt(eqdist);
      ang2 = 0.0;
      if (r != 0.0)
        ang2 = Angles.getAngleR(zt, r);
      r = getShape(ang1, ang2);

      if (dist < r * r) {
        if (!inside) {
          stepR /= 3.0;
          inside = true;
        } else
          stepR *= 1.3;
      } else {
        if (inside) {
          stepR /= 3.0;
          inside = false;
        } else
          stepR *= 1.3;
      }

    }
    return distR;
  }

  public void freeAllShapeParameters() {
    int i, j;

    for (i = 0; i < Nparameter; i++)
      parameterField[i].setRefinableCheckBound();
    for (i = 0; i < Nparameterloop; i++) {
      for (j = 0; j < numberofelementPL(i); j++)
        ((Parameter) parameterloopField[i].elementAt(j)).setRefinableCheckBound();
    }
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {

    applySymmetryRules();

    JOptionsDialog adialog = new JShapeHarmIntOptionsD(parent, this);
    return adialog;
  }

  public class JShapeHarmIntOptionsD extends JOptionsDialog {

    ShapePane shapeP;
    JSlider resolutionJS;
    JRadioButton openGlRB;
    JRadioButton idx3DRB;
    JTextField scalePlot;
    JTextField divisionTF;
    JComboBox symmetryCB;
    JTextField meanSizeTF;
    JRadioButton velocityRB;

    public JShapeHarmIntOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 6, 6));
      principalPanel.add(BorderLayout.NORTH, jPanel8);
      JPanel jPanel7 = new JPanel(new GridLayout(0, 1, 3, 3));
      jPanel8.add(jPanel7);
      JPanel jPanel6 = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
      jPanel7.add(jPanel6);
      jPanel6.add(new JLabel("Sample symmetry: "));
      symmetryCB = new JComboBox();
      for (int i = 0; i < symmetrychoice.length; i++)
        symmetryCB.addItem(symmetrychoice[i]);
      symmetryCB.setToolTipText("Set up expected sample symmetry");
      jPanel6.add(symmetryCB);

      jPanel7.add(createEditParField(new FlowLayout(),
              "Model weight: ",
              parameterField[3]));
      jPanel7.add(createEditParField(new FlowLayout(),
              "Standard size: ",
              parameterField[4]));

      jPanel7.add(velocityRB = new JRadioButton("Velocity abs. correction (TOF)"));
      velocityRB.setToolTipText("For TOF only, correct intensities for velocity dependent absorption");

      jPanel6 = new JPanel(new FlowLayout(FlowLayout.LEFT, 3, 3));
      jPanel6.add(new JLabel("Divisions number: "));
      jPanel6.add(divisionTF = new JTextField(6));
      jPanel7.add(jPanel6);
      divisionTF.setToolTipText("Optimum value: 7-9; don't exceed 15");

      JPanel SampleOrientationBPanel = new JPanel();
      SampleOrientationBPanel.setBorder(new TitledBorder(
              new BevelBorder(BevelBorder.LOWERED), "Model options"));
      SampleOrientationBPanel.setLayout(new GridLayout(0, 1, 6, 6));
      jPanel8.add(SampleOrientationBPanel);
      SampleOrientationBPanel.add(createEditParField(new FlowLayout(),
              "Omega: ",
              getOmega()));
      SampleOrientationBPanel.add(createEditParField(new FlowLayout(),
              "Chi: ",
              getChi()));
      SampleOrientationBPanel.add(createEditParField(new FlowLayout(),
              "Phi: ",
              getPhi()));

      shapeP = new ShapePane(parent, false);

      JPanel jp3 = new JPanel();
      jp3.setLayout(new BorderLayout());
      jp3.setBorder(new TitledBorder(
              new BevelBorder(BevelBorder.LOWERED), "Sample shape and size (in cm)"));
      principalPanel.add(BorderLayout.CENTER, jp3);
      jp3.add("Center", shapeP);
      JPanel jp5 = new JPanel();
      jp5.setLayout(new BorderLayout());
      jp5.setBorder(new TitledBorder(
              new BevelBorder(BevelBorder.RAISED), "Plot shape"));
      jp3.add("South", jp5);
      JPanel jp4 = new JPanel();
      jp4.setLayout(new FlowLayout());
      jp5.add("North", jp4);
      JButton showShapeB = new JButton("Solid");
      jp4.add(showShapeB);
      showShapeB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          showSampleShape(JShapeHarmIntOptionsD.this, 1);
        }
      });
      showShapeB = new JButton("Wireframe");
      jp4.add(showShapeB);
      showShapeB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          showSampleShape(JShapeHarmIntOptionsD.this, 2);
        }
      });
      showShapeB = new JButton("Nodal");
      jp4.add(showShapeB);
      showShapeB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          showSampleShape(JShapeHarmIntOptionsD.this, 3);
        }
      });

      jp4 = new JPanel();
      jp4.setLayout(new FlowLayout());
      jp5.add("Center", jp4);
      jp4.add(new JLabel("Scale (%):"));
      scalePlot = new JTextField(5);
      scalePlot.setText("100");
      jp4.add(scalePlot);
      showShapeB = new JButton("Change Color");
      jp4.add(showShapeB);
      showShapeB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          chooseColor();
        }
      });

      jp4 = new JPanel();
      jp4.setLayout(new FlowLayout());
      jp5.add("South", jp4);
      jp4.add(new JLabel("Resolution: "));
      resolutionJS = new JSlider();
      resolutionJS.setToolTipText("Set the resolution for the plot");
      jp4.add(resolutionJS);

      initParameters();

      setTitle("Sample shape");

      setHelpFilename("sampleshape.txt");
      pack();

      symmetryCB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          setSampleSymmetry(symmetryCB.getSelectedItem().toString());
          applySymmetryRules();
        }
      });

      shapeP.initListener();
      shapeP.setSliderValue(expansionDegree);
    }

    public void setResolutionSlider(int min, int max) {
      resolutionJS.setMaximum(max);
      resolutionJS.setMinimum(min);
      resolutionJS.setPaintTicks(true);
      resolutionJS.setMajorTickSpacing(15);
      resolutionJS.setMinorTickSpacing(5);

      resolutionJS.setValue(50);

      resolutionJS.setPaintLabels(true);
      resolutionJS.setSnapToTicks(true);

      resolutionJS.setLabelTable(resolutionJS.createStandardLabels(15));
    }

    public void initParameters() {
      applySymmetryRules();
      symmetryCB.setSelectedItem(getSampleSymmetry());
      setResolutionSlider(5, 95);
      shapeP.setExpansionSlider(0, 22);
      shapeP.setList(XRDparent, 0);
      velocityRB.setSelected(getVelocityCorrection());
      divisionTF.setText(stringField[3]);
    }

    public void retrieveParameters() {
      setSampleSymmetry(symmetryCB.getSelectedItem().toString());
      setVelocityCorrection(velocityRB.isSelected());
      try {
        int div = Integer.parseInt(divisionTF.getText());
        if (div > 0 && div < 16)
          stringField[3] = Integer.toString(div);
      } catch (Exception e) {}
      shapeP.retrieveparlist();
      super.retrieveParameters();

    }

    public void chooseColor() {
//			if (Constants.OpenGL)
      Constants.crystallite_color = JColorChooser.showDialog(
              this, "Choose color", Constants.crystallite_color);
    }

    Show3DShape sampleshape = null;

    public void showSampleShape(Frame parent, int mode) {

      int value = resolutionJS.getValue();
      double scaleplot = Double.valueOf(scalePlot.getText()).doubleValue();
      if (scaleplot <= 0)
        scaleplot = 100.0;
      retrieveParameters();
      myJFrame shapeFrame = new myJFrame(parent);
      shapeFrame.createDefaultMenuBar();

      shapeFrame.setVisible(false);
/*
      if (Constants.OpenGL) {
        try {
          sampleshape = new Crystallite3Dgl(SampleShapeIntegral.this, mode, value, scaleplot);
        } catch (Throwable e) {
          Constants.OpenGL = false;
          sampleshape = new Crystallite3Djgl(SampleShapeIntegral.this, mode, value, scaleplot);
//					((Crystallite3Djgl) sampleshape).setUseRepaint(false);
        }
      } else {
        sampleshape = new Crystallite3Djgl(SampleShapeIntegral.this, mode, value, scaleplot);
//					((Crystallite3Djgl) sampleshape).setUseRepaint(false);
      }

      shapeFrame.getContentPane().add((Component) sampleshape);*/
      shapeFrame.getContentPane().add(sampleshape = new Show3DShape(SampleShapeIntegral.this, mode, value, scaleplot));
      shapeFrame.setSize(400, 480);
      sampleshape.initComponents();
      shapeFrame.setVisible(true);
      sampleshape.setVisible(true);
//      sampleshape.startRotation();
    }

    public void dispose() {
      shapeP.dispose();
      if (sampleshape != null) {
        sampleshape.setVisible(false);
        sampleshape = null;
      }
      super.dispose();
    }

  }

  public class ShapePane extends JPopaSSListPane {
    public ShapePane(Frame parent, boolean showTotal) {
      super(parent, showTotal);
    }

    public void expansionHasChanged(int value) {
      if (!MoreMath.odd(value)) {
        retrieveparlist(selected);
        selected = -1;
        setparameterlist(selected);
        setExpansionDegree(value);
      }
    }
  }

}
