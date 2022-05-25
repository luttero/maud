/*
 * @(#)SizeStrainSymPopa.java created 04/10/1998 Verona-Trento
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.sizestrain;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.io.cif.CIFtoken;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

import it.unitn.ing.rista.render3d.*;

/**
 *  The SizeStrainSymPopa is a class to describe anisotropic crystallite sizes
 *  and microstrains
 *
 *
 * @version $Revision: 1.11 $, $Date: 2006/12/04 14:30:15 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SizeStrainSymPopa extends SizeStrainSymModel implements Shape3D {

  protected static String[] diclistc = {
    "_rita_harmonic_expansion_degree",
    "_riet_par_anisocryst_size",
    "_riet_par_aniso_microstrain"
  };
  protected static String[] diclistcrm = {
    "_rita_harmonic_expansion_degree",
    "size coeff (angstrom) ",
    "microstrain coeff"
  };

  protected static String[] classlistc = {};

  protected static String[] classlistcs = {};

  double acell[];
//	double astar[];

  Phase aphase = null;

  int LGIndex = 0;
  int PGIndex = 0;
  int numberMicrostrainCoeff = 0;
  int expansionDegree = 4;

  public SizeStrainSymPopa(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Popa rules";
    IDlabel = "Popa rules";
    description = "select this to apply Popa model";
  }

  public SizeStrainSymPopa(XRDcat aobj) {
    this(aobj, "Size-Strain symmetry Popa model");
  }

  public SizeStrainSymPopa() {
    identifier = "Popa rules";
    IDlabel = "Popa rules";
    description = "select this to apply Popa model";
  }

  public void initConstant() {
    Nstring = 1;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 2;
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
    aphase = (Phase) getParent();
    setHarmonicExpansion(4);
//	  if (!getFilePar().isLoadingFile())
    applySymmetryRules();
  }

	public String getHarmonicExpansion() {
    return stringField[0];
  }

  public int getHarmonicExpansionValue() {
    return Integer.valueOf(getHarmonicExpansion()).intValue();
  }

  public void setHarmonicExpansion(int i) {
    setHarmonicExpansion(Integer.toString(i));
  }

  public void setHarmonicExpansion(String value) {
    stringField[0] = new String(value);
  }

  public ListVector getCrystalliteList() {
    return parameterloopField[0];
  }

  public int numberCrystalliteParameters() {
    return getCrystalliteList().size();
  }

  public Parameter getCrystalliteSize(int index) {
    return (Parameter) getCrystalliteList().elementAt(index);
  }

  public void correctCrystalliteAndMicrostrain() {
    for (int i = 0; i < numberCrystalliteParameters(); i++)
      getCrystalliteSize(i).setValue(getCrystalliteValue(i) * 2.0);
    for (int i = 0; i < numberMicrostrainParameters(); i++)
      getMicrostrain(i).setValue(getMicrostrainValue(i) / 2.0);
  }

  public double getCrystalliteValue(int index) {
/*		Parameter cryst = (Parameter) getCrystalliteList().elementAt(index);
		if (cryst != null)
			return cryst.getValueD();
		else
			return 0.0;*/
    return getParameterLoopValues(0, index);
  }

  public ListVector getMicrostrainList() {
    return parameterloopField[1];
  }

  public int numberMicrostrainParameters() {
    return getMicrostrainList().size();
  }

  public Parameter getMicrostrain(int index) {
    return (Parameter) getMicrostrainList().elementAt(index);
  }

  public double getMeanCrystallite() {
    return getCrystalliteValue(0);
  }

  public double getMeanMicrostrain() {
    double microstrain = 0.0;
	  ((Phase) getParent()).cellVolumeComp(); // to refresh
    int n = 0;
    for (int h = -3; h < 3; h++)
      for (int k = -3; k < 3; k++)
        for (int l = -3; l < 3; l++) {
          if (!(h == 0 && k == 0 && l == 0)) {
            double dspace = aphase.getDspacing(h, k, l);
            microstrain += getMicrostrain(dspace, h, k, l);
            n++;
          }
        }
    microstrain /= n;
    return microstrain;
  }

  public void setExpansionDegree(int value) {
    setHarmonicExpansion(value);
    if (expansionDegree != value) {
      expansionDegree = value;
      checkCrystalliteParameters();
    }
  }

  public void checkCrystalliteParameters() {
    int numberCrystallite = getNumberHarmonics();
    int actualNumber = numberCrystalliteParameters();

    isAbilitatetoRefresh = false;
    if (actualNumber < numberCrystallite) {
      for (int i = actualNumber; i < numberCrystallite; i++) {
        if (i == 0.0)
          addparameterloopField(0, new Parameter(this, getParameterString(0, i), 1000,
                  ParameterPreferences.getDouble(getParameterString(0, i) + ".min", 5),
                  ParameterPreferences.getDouble(getParameterString(0, i) + ".max", 10000),
                      false, 5));
        else
          addparameterloopField(0, new Parameter(this, getParameterString(0, i), 0,
                  ParameterPreferences.getDouble(getParameterString(0, i) + ".min", -1000),
                  ParameterPreferences.getDouble(getParameterString(0, i) + ".max", 1000),
                      false, 1));
      }
    }
    if (actualNumber > numberCrystallite) {
      for (int i = actualNumber - 1; i >= numberCrystallite; i--)
        getCrystalliteList().removeItemAt(i);
    }
    isAbilitatetoRefresh = true;
  }

  public void checkMicrostrainParameters() {
    int actualNumber = numberMicrostrainParameters();

    if (actualNumber < numberMicrostrainCoeff) {
      for (int i = actualNumber; i < numberMicrostrainCoeff; i++) {
        addparameterloopField(1, new Parameter(this, getParameterString(1, i), 0,
                ParameterPreferences.getDouble(getParameterString(1, i) + ".min", -0.1),
                ParameterPreferences.getDouble(getParameterString(1, i) + ".max", 0.1),
                      false, 0.0001));
      }
    }
    if (actualNumber > numberMicrostrainCoeff) {
      for (int i = actualNumber - 1; i >= numberMicrostrainCoeff; i--)
        getMicrostrainList().removeItemAt(i);
    }
  }

  public double getMicrostrainValue(int index) {
/*		Parameter microstrain = (Parameter) getMicrostrainList().elementAt(index);
		if (microstrain != null)
			return microstrain.getValueD();
		else
			return 0.0;*/
    return getParameterLoopValues(1, index);
  }

  public int getLGnumber() {
    return aphase.getLaueGroup();
  }

  public int getPGnumber() {
    return aphase.getPointGroup();
  }

  public double getCrystallite(double dspace, int h, int k, int l) {
    // Angles must be in radiants

    int index = 0;

    double Rh = getCrystalliteValue(index);
    double phicosphi[];

    phicosphi = Angles.getPhicosPhi(aphase, h, k, l);

    for (int i = 2; i <= expansionDegree; i += 2) {
      int nl2 = SphericalHarmonics.getN(LGIndex, i);
      for (int n = 1; n <= nl2; n++) {
        Rh += getCrystalliteValue(++index)
                * SphericalHarmonics.getSphericalHarmonic(LGIndex, i, n,
                        phicosphi[0], phicosphi[1]);
      }
    }
    return Rh;
  }

  public double getCrystallite(double azimuthal, double polar) {
    int index = 0;

    double Rh = getCrystalliteValue(index);
    double phicosphi[] = new double[2];
    phicosphi[0] = azimuthal * Constants.DEGTOPI;
    phicosphi[1] = polar * Constants.DEGTOPI;

    for (int i = 2; i <= expansionDegree; i += 2) {
      int nl2 = SphericalHarmonics.getN(LGIndex, i);
      for (int n = 1; n <= nl2; n++) {

        Rh += getCrystalliteValue(++index)
                * SphericalHarmonics.getSphericalHarmonic(LGIndex, i, n,
                        phicosphi[0], phicosphi[1]);
      }
    }
    return Rh;
  }

  public double getNormalizedShape(double azimuthal, double polar) {

    double Rh = getCrystalliteValue(0);

    if (Rh == 0.0)
      return 1.0;
    else
      return getCrystallite(azimuthal, polar) / Rh;
  }

  public double getNormalizedShapeR(double azimuthal, double polar) {

    double Rh = getCrystalliteValue(0);

    if (Rh == 0.0)
      return 1.0;
    else
      return getCrystallite(azimuthal / Constants.DEGTOPI, polar / Constants.DEGTOPI) / Rh;
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

  public double getMicrostrain(double dspace, int h, int k, int l) {
    double ms = 0.0;

    double aH = acell[0] / dspace;
    double Eh = aH * aH * aH * aH;

    double hklproduct[] = gethklproduct(PGIndex, numberMicrostrainCoeff, h, k, l);

    for (int i = 0; i < numberMicrostrainCoeff; i++) {
      double ms1 = getMicrostrainValue(i);
      ms += ms1 * Math.abs(ms1) * hklproduct[i];
    }
    return Math.sqrt(Math.abs(ms / Eh));
  }

  public static int getMicrostrainCoeff(int PGnumber) {

    switch (PGnumber) {
      case 0:
        return 15;
      case 1:
      case 2:
      case 3:
        return 9;
      case 4:
        return 6;
      case 5:
        return 5;
      case 6:
        return 4;
      case 7:
        return 5;
      case 8:
        return 5;
      case 9:
        return 4;
      case 10:
        return 4;
      case 11:
      case 12:
        return 4;
      case 13:
      case 14:
        return 3;
      case 15:
      case 16:
        return 2;
      default:
        {
        }
    }
    return 0;
  }

  public static double[] gethklproduct(int PGnumber, int numbercoeff, int h, int k, int l) {

    double hklproduct[] = new double[numbercoeff];

    switch (PGnumber) {
      case 0:
        hklproduct[0] = h * h * h * h;
        hklproduct[1] = k * k * k * k;
        hklproduct[2] = l * l * l * l;

        hklproduct[3] = 2 * h * h * k * k;
        hklproduct[4] = 2 * k * k * l * l;
        hklproduct[5] = 2 * h * h * l * l;

        hklproduct[6] = 4 * h * h * h * k;
        hklproduct[7] = 4 * h * h * h * l;
        hklproduct[8] = 4 * k * k * k * h;
        hklproduct[9] = 4 * k * k * k * l;
        hklproduct[10] = 4 * l * l * l * h;
        hklproduct[11] = 4 * l * l * l * k;

        hklproduct[12] = 4 * h * h * k * l;
        hklproduct[13] = 4 * k * k * h * l;
        hklproduct[14] = 4 * l * l * h * k;
        break;
      case 1:
        hklproduct[0] = h * h * h * h;
        hklproduct[1] = k * k * k * k;
        hklproduct[2] = l * l * l * l;

        hklproduct[3] = 2 * h * h * k * k;
        hklproduct[4] = 2 * k * k * l * l;
        hklproduct[5] = 2 * h * h * l * l;
        hklproduct[6] = 4 * h * h * h * k;
        hklproduct[7] = 4 * k * k * k * h;
        hklproduct[8] = 4 * l * l * h * k;
        break;
      case 2:
        hklproduct[0] = h * h * h * h;
        hklproduct[1] = k * k * k * k;
        hklproduct[2] = l * l * l * l;

        hklproduct[3] = 2 * h * h * k * k;
        hklproduct[4] = 2 * k * k * l * l;
        hklproduct[5] = 2 * h * h * l * l;
        hklproduct[6] = 4 * l * l * l * h;
        hklproduct[7] = 4 * h * h * h * l;
        hklproduct[8] = 4 * k * k * h * l;
        break;
      case 3:
        hklproduct[0] = h * h * h * h;
        hklproduct[1] = k * k * k * k;
        hklproduct[2] = l * l * l * l;

        hklproduct[3] = 2 * h * h * k * k;
        hklproduct[4] = 2 * k * k * l * l;
        hklproduct[5] = 2 * h * h * l * l;
        hklproduct[6] = 4 * k * k * k * l;
        hklproduct[7] = 4 * l * l * l * k;
        hklproduct[8] = 4 * h * h * k * l;
        break;
      case 4:
        hklproduct[0] = h * h * h * h;
        hklproduct[1] = k * k * k * k;
        hklproduct[2] = l * l * l * l;

        hklproduct[3] = 2 * h * h * k * k;
        hklproduct[4] = 2 * k * k * l * l;
        hklproduct[5] = 2 * h * h * l * l;
        break;
      case 5:
        hklproduct[0] = h * h * h * h + k * k * k * k;
        hklproduct[1] = l * l * l * l;

        hklproduct[2] = 2 * h * h * k * k;
        hklproduct[3] = 2 * l * l * (h * h + k * k);
        hklproduct[4] = 4 * h * k * (h * h - k * k);
        break;
      case 6:
        hklproduct[0] = h * h * h * h + k * k * k * k;
        hklproduct[1] = l * l * l * l;

        hklproduct[2] = 2 * h * h * k * k;
        hklproduct[3] = 2 * l * l * (h * h + k * k);
        break;
      case 7:
        hklproduct[0] = h * h + k * k + h * k;
        hklproduct[1] = 2 * l * l * hklproduct[0];
        hklproduct[0] *= hklproduct[0];
        hklproduct[2] = l * l * l * l;
        hklproduct[3] = 4 * l * (h * h * h - k * k * k + 3 * h * h * k) / 3;
        hklproduct[4] = 4 * l * (-h * h * h + k * k * k + 3 * h * k * k) / 3;
        break;
      case 8:
        hklproduct[0] = h * h * h * h + k * k * k * k + l * l * l * l;
        hklproduct[1] = 2 * (h * h * k * k + k * k * l * l + h * h * l * l);
        hklproduct[2] = 4 * h * k * l * (h + k + l);
        hklproduct[3] = 4 * (h * h * h * k + k * k * k * l + h * l * l * l);
        hklproduct[4] = 4 * (h * h * h * l + k * k * k * h + k * l * l * l);
        break;
      case 9:
        hklproduct[0] = h * h + k * k + h * k;
        hklproduct[1] = 2 * l * l * hklproduct[0];
        hklproduct[0] *= hklproduct[0];
        hklproduct[2] = l * l * l * l;
        hklproduct[3] = 4 * l * (2 * h * h * h - 2 * k * k * k + 3 * h * h * k
                - 3 * h * k * k) / 3;
        break;
      case 10:
        hklproduct[0] = h * h * h * h + k * k * k * k + l * l * l * l;
        hklproduct[1] = 2 * (h * h * k * k + k * k * l * l + h * h * l * l);
        hklproduct[2] = 4 * h * k * l * (h + k + l);
        hklproduct[3] = 4 * (h * k * (h * h + k * k)
                + k * l * (l * l + k * k)
                + l * h * (h * h + l * l));
        break;
      case 11:
      case 12:
        hklproduct[0] = h * h + k * k + h * k;
        hklproduct[1] = 2 * l * l * hklproduct[0];
        hklproduct[0] *= hklproduct[0];
        hklproduct[2] = l * l * l * l;
        hklproduct[3] = 4 * l * (3 * h * h * k + 3 * h * k * k) / 3;
        break;
      case 13:
      case 14:
        hklproduct[0] = h * h + k * k + h * k;
        hklproduct[1] = 2 * l * l * hklproduct[0];
        hklproduct[0] *= hklproduct[0];
        hklproduct[2] = l * l * l * l;
        break;
      case 15:
      case 16:
        hklproduct[0] = h * h * h * h + k * k * k * k + l * l * l * l;
        hklproduct[1] = 2 * (h * h * k * k + k * k * l * l + h * h * l * l);
        break;
      default:
        {
        }
    }
    return hklproduct;
  }

  public void applySymmetryRules() {
    LGIndex = SpaceGroups.getLGNumber(aphase.getPointGroup());
    PGIndex = SpaceGroups.getPGNumberLconvention(aphase.getPointGroup(), aphase.getMonoclinicAxis(), aphase.isRhombohedral());
    numberMicrostrainCoeff = getMicrostrainCoeff(PGIndex);
    expansionDegree = getHarmonicExpansionValue();
    acell = Angles.getLattice(aphase);
//		astar = Angles.getReciprocalLattice(aphase);
    checkCrystalliteParameters();
    checkMicrostrainParameters();
//		System.out.println("apply sysmmetry");
  }

/*	public void updateParametertoDoubleBuffering() {
		System.out.println("updating");
    FilePar filepar = getFilePar();
		if (filepar != null) {
			System.out.println(filepar.isLoadingFile());
		}
		System.out.println(isAbilitatetoRefresh);
		super.updateParametertoDoubleBuffering();
		System.out.println("updated");
	}*/

  double cryststrain[] = new double[2];

  public double[] getCrystalliteMicrostrain(double d_space, int h, int k, int l, double[] texture_angles) {
    cryststrain[0] = getCrystallite(d_space, h, k, l);
    cryststrain[1] = getMicrostrain(d_space, h, k, l);
    return cryststrain;
  }

  public void freeAllMicroParameters(boolean amorphous) {
    int i, j;

    if (!amorphous) {
      for (i = 0; i < Nparameter; i++)
        parameterField[i].setRefinableCheckBound();
      for (i = 0; i < Nparameterloop; i++) {
        for (j = 0; j < numberofelementPL(i); j++)
          ((Parameter) parameterloopField[i].elementAt(j)).setRefinableCheckBound();
      }
    } else {
      ListVector clist = getCrystalliteList();
      for (j = 0; j < clist.size(); j++)
        ((Parameter) clist.elementAt(j)).setRefinableCheckBound();
    }
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {

    applySymmetryRules();

    JOptionsDialog adialog = new JSizeStrainAnisoOptionsD(parent, this);
    return adialog;
  }

  public class JSizeStrainAnisoOptionsD extends JOptionsDialog {

    CrystallitePane crystalliteP;
    JParListPane microstrainP;
    JSlider resolutionJS;
    JRadioButton openGlRB;
    JRadioButton idx3DRB;
    JTextField scalePlot;

    public JSizeStrainAnisoOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      crystalliteP = new CrystallitePane(parent, false);
      microstrainP = new JParListPane(parent, false);

      JPanel jp3 = new JPanel();
      jp3.setLayout(new BorderLayout());
      jp3.setBorder(new TitledBorder(
              new BevelBorder(BevelBorder.LOWERED), "Crystallite size"));
      principalPanel.add(BorderLayout.WEST, jp3);
      jp3.add("Center", crystalliteP);
      JPanel jp5 = new JPanel();
      jp5.setLayout(new BorderLayout());
      jp5.setBorder(new TitledBorder(
              new BevelBorder(BevelBorder.RAISED), "Plot crystallite"));
      jp3.add("South", jp5);
      JPanel jp4 = new JPanel();
      jp4.setLayout(new FlowLayout());
      jp5.add("North", jp4);
      JButton showCrystB = new JButton("Solid");
      jp4.add(showCrystB);
      showCrystB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          showCrystallite(JSizeStrainAnisoOptionsD.this, 1);
        }
      });
      showCrystB = new JButton("Wireframe");
      jp4.add(showCrystB);
      showCrystB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          showCrystallite(JSizeStrainAnisoOptionsD.this, 2);
        }
      });
      showCrystB = new JButton("Nodal");
      jp4.add(showCrystB);
      showCrystB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          showCrystallite(JSizeStrainAnisoOptionsD.this, 3);
        }
      });

      jp4 = new JPanel();
      jp4.setLayout(new FlowLayout());
      jp5.add("Center", jp4);
      jp4.add(new JLabel("Scale (%):"));
      scalePlot = new JTextField(5);
      scalePlot.setText("100");
      jp4.add(scalePlot);
      showCrystB = new JButton("Change Color");
      jp4.add(showCrystB);
      showCrystB.addActionListener(new ActionListener() {
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

      jp3 = new JPanel();
      jp3.setLayout(new BorderLayout());
      jp3.setBorder(new TitledBorder(
              new BevelBorder(BevelBorder.LOWERED), "R.m.s. microstrain"));
      principalPanel.add(BorderLayout.EAST, jp3);
      jp3.add("Center", microstrainP);

      initParameters();

      setTitle("Anisotropic size-strain, Popa model");

      setHelpFilename("popahelp.txt");
      pack();

      crystalliteP.initListener();
      crystalliteP.setSliderValue(expansionDegree);
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
      setResolutionSlider(5, 95);
      crystalliteP.setExpansionSlider(0, 22);
      crystalliteP.setList(XRDparent, 0);
      microstrainP.setList(XRDparent, 1);
    }

    public void retrieveParameters() {
      crystalliteP.retrieveparlist();
      microstrainP.retrieveparlist();
    }

    public void chooseColor() {
      Constants.crystallite_color = JColorChooser.showDialog(
              this, "Choose color", Constants.crystallite_color);
    }

    Show3DShape crystallite = null;

    public void showCrystallite(Frame parent, int mode) {

      int value = resolutionJS.getValue();
      double scaleplot = Double.valueOf(scalePlot.getText()).doubleValue();
      if (scaleplot <= 0)
        scaleplot = 100.0;
      crystalliteP.retrieveparlist();
//			Frame crystalliteFrame = new Frame();
      myJFrame crystalliteFrame = new myJFrame(parent);
      crystalliteFrame.createDefaultMenuBar();

      crystalliteFrame.setVisible(false);
/*
      if (Constants.OpenGL) {
        try {
          crystallite = new Crystallite3Dgl(SizeStrainSymPopa.this, mode, value, scaleplot);
        } catch (Throwable e) {
          Constants.OpenGL = false;
          crystallite = new Crystallite3Djgl(SizeStrainSymPopa.this, mode, value, scaleplot);
//					((Crystallite3Djgl) crystallite).setUseRepaint(false);
        }
      } else {
        crystallite = new Crystallite3Djgl(SizeStrainSymPopa.this, mode, value, scaleplot);
//					((Crystallite3Djgl) crystallite).setUseRepaint(false);
      }

      crystalliteFrame.getContentPane().add((Component) crystallite);*/
      crystalliteFrame.getContentPane().add(crystallite = new Show3DShape(SizeStrainSymPopa.this, mode, value,
              scaleplot));
//			crystalliteFrame.add((Component) crystallite);
      crystalliteFrame.setSize(400, 480);
      crystallite.initComponents();
      crystalliteFrame.setVisible(true);
      crystallite.setVisible(true);
//      crystallite.startRotation();

    }

    public void dispose() {
      crystalliteP.dispose();
      microstrainP.dispose();
      if (crystallite != null) {
        crystallite.setVisible(false);
        crystallite = null;
      }
      super.dispose();
    }

  }

  public class CrystallitePane extends JPopaSSListPane {
    public CrystallitePane(Frame parent, boolean showTotal) {
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
