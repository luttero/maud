/*
 * @(#)SampleShapeHarmonic.java created 19/04/2003 Berkeley
 *
 * Copyright (c) 2003 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.shape;

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.awt.JPopaSSListPane;
import it.unitn.ing.rista.awt.myJFrame;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.interfaces.Shape3D;
import it.unitn.ing.rista.render3d.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import javax.swing.border.BevelBorder;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 *  The SampleShapeHarmonic is a class
 *
 *
 * @version $Revision: 1.7 $, $Date: 2006/01/19 14:45:58 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SampleShapeHarmonic extends SampleShape implements Shape3D {

  protected static String[] diclistc = {
    "_rita_sample_symmetry", "_rita_harmonic_expansion_degree", "_rita_shape_abs_velocity_corr",
    "_pd_spec_orientation_omega", "_pd_spec_orientation_chi", "_pd_spec_orientation_phi",
    "_riet_par_sampleshape_model_weight", "_riet_par_standard_size",
    "_riet_par_sampleshape_size"
  };
  protected static String[] diclistcrm = {
    "_rita_sample_symmetry", "_rita_harmonic_expansion_degree", "_rita_shape_abs_velocity_corr",
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
  double meanShapeCorr = 0.0;

  public SampleShapeHarmonic(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Harmonic Coeff";
    IDlabel = "Harmonic Coeff";
    description = "Shape described by harmonic coefficients";
  }

  public SampleShapeHarmonic(XRDcat aobj) {
    this(aobj, "Shape described by harmonic coefficients");
  }

  public SampleShapeHarmonic() {
    identifier = "Harmonic Coeff";
    IDlabel = "Harmonic Coeff";
    description = "Shape described by harmonic coefficients";
  }

  public void initConstant() {
    Nstring = 3;
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
            notifyParameterChanged(source, Constants.SHAPE_ABSORPTION_CHANGED, -1);
            return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.SHAPE_ABSORPTION_CHANGED, -1);
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
	  if (shapeParameters == null)
		  updateParametertoDoubleBuffering(false);
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
          addparameterloopField(0, new Parameter(this, getParameterString(0, i), 0.7,
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

  public double getShape(double azimuthal, double polar) {
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
//    System.out.println(phicosphi[0] + " " + phicosphi[1] + " " + Rh + " " +azimuthal + " " +polar);
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

    meanShapeCorr = getMeanShape() * ((1 - getParameterValue(3)) * Math.sqrt(2.0) + getParameterValue(3));

      parameterField[3].setPositiveOnly();
      parameterField[4].setPositiveOnly();
  
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

  public void computeAbsorptionPath(double[][] incidentAndDiffraction_angles, double absorption, double[] position,
                                    double[] intensity, double toLambda) {
    double totalPath = 0.0;
    double arg1 = 0.0;
    boolean positionCorr = false;
    if (velocityCorrection() && toLambda != 0.0f) {
      absorption *= toLambda;
      positionCorr = true;
    }
    for (int i = 0; i < position.length; i++) {
      boolean asPrevious = true;
      if (i == 0)
        asPrevious = false;
      else
        for (int j = 0; j < 6; j++)
          if (incidentAndDiffraction_angles[i][j] != incidentAndDiffraction_angles[i - 1][j])
            asPrevious = false;
      if (!asPrevious) {
        double incidentPathLength = getShape(incidentAndDiffraction_angles[i][1],
                incidentAndDiffraction_angles[i][0]);

        double diffractedPathLength = getShape(incidentAndDiffraction_angles[i][3],
                incidentAndDiffraction_angles[i][2]);

        double normalPathLength = getShape(incidentAndDiffraction_angles[i][5],
                incidentAndDiffraction_angles[i][4]);

        double sin0 = Math.sin(incidentAndDiffraction_angles[i][0]);
        double sin1 = Math.sin(incidentAndDiffraction_angles[i][1]);
        double sin2 = Math.sin(incidentAndDiffraction_angles[i][2]);
        double sin3 = Math.sin(incidentAndDiffraction_angles[i][3]);
        double cos0 = Math.cos(incidentAndDiffraction_angles[i][0]);
        double cos1 = Math.cos(incidentAndDiffraction_angles[i][1]);
        double cos2 = Math.cos(incidentAndDiffraction_angles[i][2]);
        double cos3 = Math.cos(incidentAndDiffraction_angles[i][3]);
//    return incidentPathLength + diffractedPathLength;
        double xdiff = incidentPathLength * cos1 * sin0 - diffractedPathLength * cos3 * sin2;
        double ydiff = incidentPathLength * sin1 * sin0 - diffractedPathLength * sin3 * sin2;
        double zdiff = incidentPathLength * cos0 - diffractedPathLength * cos2;
        double xdiffs = cos1 * sin0 - cos3 * sin2;
        double ydiffs = sin1 * sin0 - sin3 * sin2;
        double zdiffs = cos0 - cos2;
        xdiff *= xdiff;
        ydiff *= ydiff;
        zdiff *= zdiff;
        xdiffs *= xdiffs;
        ydiffs *= ydiffs;
        zdiffs *= zdiffs;
        totalPath = (1 - getParameterValue(3)) * (Math.sqrt(xdiff + ydiff + zdiff) -
                getParameterValue(4) * Math.sqrt(xdiffs + ydiffs + zdiffs))
                + getParameterValue(3) * (normalPathLength - getParameterValue(4));
      }
//System.out.println(incidentPathLength + " " + incidentAndDiffraction_angles[0] + " " + incidentAndDiffraction_angles[1]
//                   + " " + diffractedPathLength + " " + incidentAndDiffraction_angles[2] + " "
//                   + incidentAndDiffraction_angles[3] + " " + totalPath);
      if (positionCorr)
        arg1 = absorption * totalPath * position[i];// * Constants.LAMBDA_SPEED_NEUTRON_CONV_REC;
      else
        arg1 = absorption * totalPath;
      if (arg1 < 200.0)
        intensity[i] *= Math.exp(-arg1);
      else
        intensity[i] *= 0.0f;
    }
  }

	public void computeAbsorptionPath(double[][][] incidentAndDiffraction_angles, double[] absorption, double[][] position,
	                                  double[][] intensity, double toLambda) {
		double totalPath = 0.0;
		double arg1[] = new double[absorption.length];
		for (int i = 0; i < absorption.length; i++)
			arg1[i] = 0.0;
		boolean positionCorr = false;
		if (velocityCorrection() && toLambda != 0.0f) {
			for (int i = 0; i < absorption.length; i++)
				absorption[i] *= toLambda;
			positionCorr = true;
		}
		for (int i = 0; i < position.length; i++) {
			for (int k = 0; k < absorption.length; k++) {
				boolean asPrevious = true;
				if (k == 0)
					asPrevious = false;
				else
					for (int j = 0; j < 6; j++)
						if (incidentAndDiffraction_angles[i][k][j] != incidentAndDiffraction_angles[i][k - 1][j])
							asPrevious = false;
				if (!asPrevious) {
					double incidentPathLength = getShape(incidentAndDiffraction_angles[i][k][1],
							incidentAndDiffraction_angles[i][k][0]);

					double diffractedPathLength = getShape(incidentAndDiffraction_angles[i][k][3],
							incidentAndDiffraction_angles[i][k][2]);

					double normalPathLength = getShape(incidentAndDiffraction_angles[i][k][5],
							incidentAndDiffraction_angles[i][k][4]);

					double sin0 = Math.sin(incidentAndDiffraction_angles[i][k][0]);
					double sin1 = Math.sin(incidentAndDiffraction_angles[i][k][1]);
					double sin2 = Math.sin(incidentAndDiffraction_angles[i][k][2]);
					double sin3 = Math.sin(incidentAndDiffraction_angles[i][k][3]);
					double cos0 = Math.cos(incidentAndDiffraction_angles[i][k][0]);
					double cos1 = Math.cos(incidentAndDiffraction_angles[i][k][1]);
					double cos2 = Math.cos(incidentAndDiffraction_angles[i][k][2]);
					double cos3 = Math.cos(incidentAndDiffraction_angles[i][k][3]);
//    return incidentPathLength + diffractedPathLength;
					double xdiff = incidentPathLength * cos1 * sin0 - diffractedPathLength * cos3 * sin2;
					double ydiff = incidentPathLength * sin1 * sin0 - diffractedPathLength * sin3 * sin2;
					double zdiff = incidentPathLength * cos0 - diffractedPathLength * cos2;
					double xdiffs = cos1 * sin0 - cos3 * sin2;
					double ydiffs = sin1 * sin0 - sin3 * sin2;
					double zdiffs = cos0 - cos2;
					xdiff *= xdiff;
					ydiff *= ydiff;
					zdiff *= zdiff;
					xdiffs *= xdiffs;
					ydiffs *= ydiffs;
					zdiffs *= zdiffs;
					totalPath = (1 - getParameterValue(3)) * (Math.sqrt(xdiff + ydiff + zdiff) -
							getParameterValue(4) * Math.sqrt(xdiffs + ydiffs + zdiffs))
							+ getParameterValue(3) * (normalPathLength - getParameterValue(4));
				}
//System.out.println(incidentPathLength + " " + incidentAndDiffraction_angles[0] + " " + incidentAndDiffraction_angles[1]
//                   + " " + diffractedPathLength + " " + incidentAndDiffraction_angles[2] + " "
//                   + incidentAndDiffraction_angles[3] + " " + totalPath);
				if (positionCorr)
					arg1[k] = absorption[k] * totalPath * position[i][k];// * Constants.LAMBDA_SPEED_NEUTRON_CONV_REC;
				else
					arg1[k] = absorption[k] * totalPath;
				if (arg1[k] < 200.0)
					intensity[i][k] *= Math.exp(-arg1[k]);
				else
					intensity[i][k] *= 0.0f;
			}
		}
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

    JOptionsDialog adialog = new JShapeHarmonicOptionsD(parent, this);
    return adialog;
  }

  public class JShapeHarmonicOptionsD extends JOptionsDialog {

    ShapePane shapeP;
    JSlider resolutionJS;
    JRadioButton openGlRB;
    JRadioButton idx3DRB;
    JTextField scalePlot;
    JComboBox symmetryCB;
    JTextField meanSizeTF;
    JRadioButton velocityRB;

    public JShapeHarmonicOptionsD(Frame parent, XRDcat obj) {

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

      JPanel SampleOrientationBPanel = new JPanel();
      SampleOrientationBPanel.setBorder(new TitledBorder(
              new BevelBorder(BevelBorder.LOWERED), "Sample orientation"));
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
              new BevelBorder(BevelBorder.LOWERED), "Sample shape and size"));
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
          showSampleShape(JShapeHarmonicOptionsD.this, 1);
        }
      });
      showShapeB = new JButton("Wireframe");
      jp4.add(showShapeB);
      showShapeB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          showSampleShape(JShapeHarmonicOptionsD.this, 2);
        }
      });
      showShapeB = new JButton("Nodal");
      jp4.add(showShapeB);
      showShapeB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          showSampleShape(JShapeHarmonicOptionsD.this, 3);
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
    }

    public void retrieveParameters() {
      setSampleSymmetry(symmetryCB.getSelectedItem().toString());
      setVelocityCorrection(velocityRB.isSelected());
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

/*      if (Constants.OpenGL) {
        try {
          sampleshape = new Crystallite3Dgl(SampleShapeHarmonic.this, mode, value, scaleplot);
        } catch (Throwable e) {
          Constants.OpenGL = false;
          sampleshape = new Crystallite3Djgl(SampleShapeHarmonic.this, mode, value, scaleplot);
//					((Crystallite3Djgl) sampleshape).setUseRepaint(false);
        }
      } else {
        sampleshape = new Crystallite3Djgl(SampleShapeHarmonic.this, mode, value, scaleplot);
//					((Crystallite3Djgl) sampleshape).setUseRepaint(false);
      }
      JPanel principal = new JPanel(new BorderLayout(3, 3));
      principal.add(BorderLayout.CENTER, (Component) sampleshape);
      shapeFrame.getContentPane().add(principal);*/
      shapeFrame.getContentPane().add(sampleshape = new Show3DShape(SampleShapeHarmonic.this, mode, value, scaleplot));
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
