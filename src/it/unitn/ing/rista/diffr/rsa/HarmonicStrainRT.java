/*
 * @(#)HarmonicStrainRT.java created 16/11/1999 Pergine Vals.
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

import java.awt.*;
import javax.swing.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.rta.*;

import javax.swing.border.*;
import java.awt.event.*;
import java.util.*;

/**
 *  The HarmonicStrain is a class
 *
 *
 * @version $Revision: 1.14 $, $Date: 2006/12/04 14:30:04 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class HarmonicStrainRT extends Strain {

  public static String[] diclistc = {"_rista_sample_symmetry",
                                     "_rista_harmonic_expansion_degree",

                                     "_rista_harmonic_strain_11",
                                     "_rista_harmonic_strain_22",
                                     "_rista_harmonic_strain_33",
                                     "_rista_harmonic_strain_23",
                                     "_rista_harmonic_strain_13",
                                     "_rista_harmonic_strain_12"
  };
  public static String[] diclistcrm = {"_rista_sample_symmetry",
                                     "_rista_harmonic_expansion_degree",

                                     "strain_11",
                                     "strain_22",
                                     "strain_33",
                                     "strain_23",
                                     "strain_13",
                                     "strain_12"
  };

  public static String[] classlistcs = {};
  public static String[] classlistc = {};
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
                                           "fiber"};

  public static String[] strainchoice = {"11", "22", "33", "23", "13", "12"};

  Sample actualsample = null;
//	int actuallayer = 0;

  int expansionDegree = 4;
  int sampleSymmetry = 0;

//	double acell[];
//	double astar[];

  int LGIndex = 0;
//	int PGIndex = 0;

  double[][] coefficient = null;
  public static int numberStrainParameters = 6; // remember should be equal to the one in StrainSphericalHarmonics

  public HarmonicStrainRT(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Harmonic Texture Weighted Strain";
    IDlabel = "Harmonic Texture Weighted Strain";
    description = "select this to apply Harmonic model for WSODF";
  }

  public HarmonicStrainRT(XRDcat aobj) {
    this(aobj, "Harmonic method for WSODF");
  }

  public HarmonicStrainRT() {
    identifier = "Disabled Harmonic Texture Weighted Strain";
    IDlabel = "Harmonic Texture Weighted Strain";
    description = "select this to apply Harmonic model for WSODF";
  }

  public void initConstant() {
    Nstring = 2;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = numberStrainParameters = 6;
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

    setSampleSymmetry(0);
    setHarmonicExpansion(4);

	  applySymmetryRules();
  }

	public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
            notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED, -1);
            notifyParameterChanged(source, Constants.STRAIN_CHANGED, -1);
            return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED, -1);
            notifyParameterChanged(source, Constants.STRAIN_CHANGED, -1);
            return;
          }

      super.notifyParameterChanged(source);
    }
  }

  public String getSampleSymmetry() {
    return getString(0);
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
    setString(0, symmetrychoice[i]);
  }

  public void setSampleSymmetry(String value) {
    setString(0, value);
  }

  public String getHarmonicExpansion() {
    return getString(1);
  }

  public int getHarmonicExpansionValue() {
    return Integer.valueOf(getHarmonicExpansion()).intValue();
  }

  public void setHarmonicExpansion(int i) {
    setHarmonicExpansion(Integer.toString(i));
  }

  public void setHarmonicExpansion(String value) {
    setString(1, value);
  }

  public int getLaueGroupNumber() {
    return SpaceGroups.getLGNumberSiegfriedConv(getPhase().getPointGroup());
  }

  public ListVector getHarmonicParameterList(int index) {
    return parameterloopField[index];
  }

  public int[] numberHarmonicParameters() {
    int[] nharm = new int[numberStrainParameters];
    for (int j = 0; j < numberStrainParameters; j++)
      nharm[j] = getHarmonicParameterList(j).size();
    return nharm;
  }

  public void setExpansionDegree(int value) {
    setHarmonicExpansion(value);
    if (expansionDegree != value) {
      expansionDegree = value;
      applySymmetryRules();
      refreshComputation = true;
    }
  }

  public void checkHarmonicParameters() {
    int[] numberHarmonics = getNumberHarmonics();
    int[] actualNumber = numberHarmonicParameters();

    isAbilitatetoRefresh = false;
    for (int j = 0; j < numberStrainParameters; j++)
      if (actualNumber[j] < numberHarmonics[j]) {
        for (int i = actualNumber[j]; i < numberHarmonics[j]; i++)
          addparameterloopField(j, new Parameter(this, getParameterString(j, i), 0,
                  ParameterPreferences.getDouble(getParameterString(j, i) + ".min", -0.1),
                  ParameterPreferences.getDouble(getParameterString(j, i) + ".max", 0.1)));
        refreshComputation = true;
      }

    for (int j = 0; j < numberStrainParameters; j++)
      if (actualNumber[j] > numberHarmonics[j]) {
        for (int i = actualNumber[j] - 1; i >= numberHarmonics[j]; i--)
          getHarmonicParameterList(j).removeItemAt(i);
        refreshComputation = true;
      }
    isAbilitatetoRefresh = true;
  }

  public int[] getNumberHarmonics() {

    int[] index = new int[numberStrainParameters];

    for (int j = 0; j < numberStrainParameters; j++)
      index[j] = getNumberHarmonics(j);

    return index;
  }

  public int getNumberHarmonics(int strainIndex) {

    int index = 0;
    for (int l = 0; l <= expansionDegree; l += 2) {
      index += StrainSphericalHarmonics.getN(LGIndex, l, strainIndex) *
              StrainSphericalHarmonics.getN(sampleSymmetry, l);
    }

    return index;
  }

  public int getLGnumber() {
    return getPhase().getLaueGroup();
  }

  public int getPGnumber() {
    return ((Phase) getPhase()).getPointGroup();
  }

  public void applySymmetryRules() {
    LGIndex = SpaceGroups.getLGNumber(getPhase().getPointGroup());
    sampleSymmetry = getSampleSymmetryValue();
    expansionDegree = getHarmonicExpansionValue();
    checkHarmonicParameters();
    refreshCoefficients();
  }

  public void refreshCoefficients() {
    coefficient = getParameterLoopVector();
//    System.out.println(coefficient[0][0]);
  }

  public double[][] getParameterLoopVector() {
    Vector[] coeffVect = new Vector[numberStrainParameters];
    for (int i = 0; i < numberStrainParameters; i++)
      coeffVect[i] = parameterloopField[i];
    return StrainSphericalHarmonics.getParameterMatrix(coeffVect);
  }

  public double computeStrain(double phi, double beta, double psi, double gamma) {
    // Angles must be in radiants
    // phi and beta are the polar and azimuthal angles for the crystal setting
    // psi and gamma for the sample
    // see Popa, J. Appl. Cryst. 25, 611, 1992.

    double[] poleIntensity = new double[numberStrainParameters];
    double strain33 = 0.0;
    int k = 0;
    double[] tmpIntensity = new double[numberStrainParameters];
    double[] tmpPole = new double[numberStrainParameters];

    for (int i = 0; i < numberStrainParameters; i++) {
      poleIntensity[i] = coefficient[i][k];
      System.out.println(poleIntensity[i]);
    }
    k++;
    for (int l = 2; l <= expansionDegree; l += 2) {
      for (int i = 0; i < numberStrainParameters; i++)
        tmpIntensity[i] = 0.0;
      int nl2 = StrainSphericalHarmonics.getN(sampleSymmetry, l);
      for (int n = 1; n <= nl2; n++) {
        int ml2 = StrainSphericalHarmonics.getN(LGIndex, l);
        for (int m = 1; m <= ml2; m++) {
          for (int i = 0; i < numberStrainParameters; i++)
            tmpPole[i] = 0.0;
          double[] pole = StrainSphericalHarmonics.
                  getStrainSphericalHarmonic(LGIndex, l, m, beta, phi);
          for (int i = 0; i < numberStrainParameters; i++)
            tmpPole[i] += coefficient[i][k] * pole[i];
          k++;
        }
        double tmpSpher = StrainSphericalHarmonics.
                getSphericalHarmonic(sampleSymmetry, l, n, gamma, psi);
        for (int i = 0; i < numberStrainParameters; i++)
          tmpIntensity[i] += tmpPole[i] * tmpSpher;
      }
      double norm = (4.0 * Constants.PI) / (2 * l + 1);
      for (int i = 0; i < numberStrainParameters; i++) {
        poleIntensity[i] += (tmpIntensity[i] * norm);
        System.out.println(i + " " + poleIntensity[i]);
      }
    }

    strain33 = getStrain33(poleIntensity, beta, phi);
    return strain33;
  }

  public double getStrain33(double[] poleIntensity, double beta, double phi) {
    double sinphi = Math.sin(phi);
    double A1 = Math.cos(beta) * sinphi;
    double A2 = Math.sin(beta) * sinphi;
    double A3 = Math.cos(phi);
    double[] Erho = new double[6];
    Erho[0] = A1 * A1;
    Erho[1] = A2 * A2;
    Erho[2] = A3 * A3;
    Erho[3] = 2.0 * A2 * A3;
    Erho[4] = 2.0 * A1 * A3;
    Erho[5] = 2.0 * A1 * A2;
    double strain33 = 0.0;
    for (int i = 0; i < 6; i++) {
      strain33 += (Erho[i] * poleIntensity[i]);
    }
    return strain33;
  }

  public double[] getODF(double alpha, double beta, double gamma) {
    int k = 0;

    alpha = Constants.PI - alpha;
    gamma = Constants.PI - gamma;

    double[] wsodf = new double[numberStrainParameters];

    for (int i = 0; i < numberStrainParameters; i++)
      wsodf[i] = coefficient[i][k];
    k++;
    for (int l = 2; l <= expansionDegree; l += 2) {
      int nl2 = StrainSphericalHarmonics.getN(sampleSymmetry, l);
      for (int n = 1; n <= nl2; n++) {
        int ml2 = StrainSphericalHarmonics.getN(LGIndex, l);
        for (int m = 1; m <= ml2; m++) {
          for (int i = 0; i < numberStrainParameters; i++)
            wsodf[i] += coefficient[i][k] * StrainSphericalHarmonics.getDSphericalHarmonic(
                    LGIndex, sampleSymmetry, l, m, n, gamma, beta, alpha, i);
          k++;
        }
      }
    }
    return wsodf;
  }

  public static double computeStrain(double[][][] odfl, double[] cdsc, double strain_angles[],
                                     double[] sctf, double fhir, int inv, double phoninp,
                                     double res) {

    double pfValue = 0.0;

    return pfValue;
  }

  public double computeStrain(Phase aphase, double strain_angles[],
                              int h, int k, int l) {

    Reflection refl = aphase.getReflectionByhkl(h, k, l);
    return computeStrain(refl.phi[0], refl.beta[0],
            strain_angles[0] * Constants.DEGTOPI,
            strain_angles[1] * Constants.DEGTOPI);

  }

  public double[] computeStrain(Phase aphase, double alpha[], double beta[],
                                Reflection reflex) {

    int numberOfPoints = alpha.length;

    double[] strainValues = new double[numberOfPoints];

    double[] strain_angles = new double[2];
    for (int i = 0; i < numberOfPoints; i++) {
      strain_angles[0] = alpha[i];
      strain_angles[1] = beta[i];
      strainValues[i] = computeStrain(reflex.phi[0], reflex.beta[0],
              strain_angles[0] * Constants.DEGTOPI,
              strain_angles[1] * Constants.DEGTOPI);
    }

    return strainValues;
  }

  public double[][] getPoleFigureGrid(Reflection refl, int numberofPoints, double maxAngle) {

    double[][] PFreconstructed = new double[numberofPoints][numberofPoints];

    double strain_angles[] = new double[2];

    double x , y, r;
    double dxy = 2.0 * maxAngle / (numberofPoints - 1);

//		Phase aphase = (Phase) refl.getParent();
    applySymmetryRules();
//		aphase.sghklcompute(false);

    for (int i = 0; i < numberofPoints; i++)
      for (int j = 0; j < numberofPoints; j++) {
        x = j * dxy - maxAngle;
        y = i * dxy - maxAngle;
        r = Math.sqrt(x * x + y * y);
        if (r == 0.0) {
          strain_angles[0] = 0.0;
          strain_angles[1] = 0.0;
          PFreconstructed[i][j] = computeStrain(refl.phi[0], refl.beta[0],
                  strain_angles[0] * Constants.DEGTOPI,
                  strain_angles[1] * Constants.DEGTOPI);
        } else if (r <= maxAngle) {
          double phaseAng = Math.atan2(x, y);
          if (phaseAng < 0.0)
            phaseAng += Constants.PI2;
          strain_angles[0] = 2.0 * Math.asin(r / Constants.sqrt2) * Constants.PITODEG;
          if (strain_angles[0] < 0.0) {
            strain_angles[0] = -strain_angles[0];
            phaseAng += Constants.PI;
            while (phaseAng >= Constants.PI2)
              phaseAng -= Constants.PI2;
          }
          strain_angles[1] = phaseAng * Constants.PITODEG;
//					System.out.println(Double.toXRDcatString(strain_angles[0]) + " " + Double.toXRDcatString(strain_angles[1]));

          PFreconstructed[i][j] = computeStrain(refl.phi[0], refl.beta[0],
                  strain_angles[0] * Constants.DEGTOPI,
                  strain_angles[1] * Constants.DEGTOPI);
        } else
          PFreconstructed[i][j] = Double.NaN;
      }
    return PFreconstructed;
  }

  public double[] computeMacroStrain() {

    if (getFilePar().isComputingDerivate())
      return null;

    double[] macrostrain = new double[numberStrainParameters];

    double va = 0.0, b = 0.0;

    double fnorm = 0.0;
    double resolutionR = Constants.ODFresolution * Constants.DEGTOPI;

    for (int i = 0; i < numberStrainParameters; i++)
      macrostrain[i] = 0.0;

    b = resolutionR / 2.0;

    for (double gamma = b; gamma <= Constants.PI2; gamma += resolutionR) {

      for (double beta = b; beta <= Constants.PI; beta += resolutionR) {

        va = resolutionR * resolutionR * (Math.cos(beta - b) - Math.cos(beta + b));

        for (double alpha = b; alpha <= Constants.PI2; alpha += resolutionR) {
          double[] fn = Angles.trasformInSampleRefFrame(getODF(alpha, beta, gamma),
                  alpha, beta, gamma);
          for (int i = 0; i < numberStrainParameters; i++)
            macrostrain[i] += fn[i] * va;
        }
      }
    }
    System.out.println("Phase: " + getParent().toXRDcatString());
    for (int i = 0; i < numberStrainParameters; i++) {
      macrostrain[i] /= (8.0 * Constants.PI * Constants.PI);

      System.out.println("Macrostrain" + strainchoice[i] + ": " + macrostrain[i]);
    }
    return macrostrain;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JHStrainOptionsD(parent, this);
    return adialog;
  }

  class JHStrainOptionsD extends JOptionsDialog {

    JComboBox symmetryCB;
    JComboBox strainCB;
    HarmonicPane harmonicCoefficientP;

    public JHStrainOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));
      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 6, 6));
      principalPanel.add(BorderLayout.NORTH, jPanel8);
      jPanel8.add(new JLabel("Sample symmetry: "));
      symmetryCB = new JComboBox();
      for (int i = 0; i < symmetrychoice.length; i++)
        symmetryCB.addItem(symmetrychoice[i]);
      symmetryCB.setToolTipText("Set up expected sample symmetry");
      jPanel8.add(symmetryCB);

      harmonicCoefficientP = new HarmonicPane(parent, false);
      JPanel jp3 = new JPanel();
      jp3.setLayout(new BorderLayout());
      jp3.setBorder(new TitledBorder(
              new BevelBorder(BevelBorder.LOWERED), "Harmonic coefficients"));
      principalPanel.add(BorderLayout.CENTER, jp3);
      jp3.add("Center", harmonicCoefficientP);

      jPanel8 = new JPanel();
      jPanel8.setLayout(new FlowLayout(FlowLayout.LEFT, 6, 6));
      jp3.add(BorderLayout.NORTH, jPanel8);
      jPanel8.add(new JLabel("Strain parameter: "));
      strainCB = new JComboBox();
      for (int i = 0; i < strainchoice.length; i++)
        strainCB.addItem(strainchoice[i]);
      strainCB.setToolTipText("Choose the strain element");
      jPanel8.add(strainCB);

      JPanel jp1 = new JPanel();
      jp1.setLayout(new BorderLayout());
      jp1.setBorder(new TitledBorder(
              new BevelBorder(BevelBorder.LOWERED), "Options"));
      principalPanel.add(BorderLayout.SOUTH, jp1);

      jp3 = new JPanel();
      jp3.setLayout(new FlowLayout());
      jp1.add(BorderLayout.CENTER, jp3);
      jp3.add(new JLabel("Export PFs for "));
      JButton jb;
      jp3.add(jb = new JButton("Beartex"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          exportPFsinBEARTEXformat();
        }
      });
      jb.setToolTipText("Press this to save the PFs using the Beartex format");

      jp3 = new JPanel();
      jp3.setLayout(new FlowLayout());
      jp1.add(BorderLayout.SOUTH, jp3);
      jp3.add(new JLabel("Export Coeffs for "));
      jp3.add(jb = new JButton("Beartex"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          exportCoeffinBEARTEXformat();
        }
      });
      jb.setToolTipText("Press this to save the coefficients in the Beartex format");

      setTitle("Harmonic strain options panel");
      initParameters();
      pack();

      symmetryCB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          setSampleSymmetry(symmetryCB.getSelectedItem().toString());
          applySymmetryRules();
        }
      });

      strainCB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          changeHarmonicCoeff(strainCB.getSelectedIndex());
        }
      });

      harmonicCoefficientP.initListener();
      harmonicCoefficientP.setSliderValue(expansionDegree);
    }

    public void initParameters() {
      applySymmetryRules();
      symmetryCB.setSelectedItem(getSampleSymmetry());
      strainCB.setSelectedItem(strainchoice[0]);
      harmonicCoefficientP.setExpansionSlider(0, 100);
      harmonicCoefficientP.setList(XRDparent, 0);
    }

    public void retrieveParameters() {
      setSampleSymmetry(symmetryCB.getSelectedItem().toString());
      harmonicCoefficientP.retrieveparlist();
    }

    public void changeHarmonicCoeff(int index) {
//			harmonicCoefficientP.retrieveparlist();
      harmonicCoefficientP.changeList(XRDparent, index);
    }

    public void exportPFsinBEARTEXformat() {
      final String filename = Utility.browseFilenametoSave(this, "choose a file for PFs in BEARTEX format (.xpc)");
      (new PersistentThread() {
        public void executeJob() {
          PoleFigureOutput pfOutput = new PoleFigureOutput(filename, getPhase());
          pfOutput.computeAndWrite();
        }
      }).start();
    }

    public void exportCoeffinBEARTEXformat() {
/*			final String filename = Misc.browseFilenametoSave(this, "choose a file for Coefficients in BEARTEX format (.hha)");

			Phase phase = getPhase();

			refreshCoefficients();

			BufferedWriter PFwriter = Misc.getWriter(filename);

    	String commentLine =	new String("Harmonic coeeficients from RiSTA computing of phase: " + phase.toXRDcatString());

			try {
				PFwriter.write(commentLine);
    		PFwriter.newLine();

				PFwriter.write(filename);
    		PFwriter.newLine();

    		PFwriter.write("    " + Integer.toXRDcatString(SpaceGroups.getLGNumberSiegfriedConv(phase)) + "    " +
    										Integer.toXRDcatString(getSampleSymmetryValue() + 1));
    		PFwriter.newLine();

//			new String("    1.0000    1.0000    1.0000   90.0000   90.0000   90.0000");
				PFwriter.write(Misc.getFirstHHline(phase));
    		PFwriter.newLine();

    		String firstline = new String("    10.000    40.411     1.000   " + Integer.toXRDcatString(expansionDegree));
				PFwriter.write(firstline);
    		PFwriter.newLine();

    		int k = 0;
			  for (int l = 2; l <= expansionDegree; l += 2) {
					int ml2 = StrainSphericalHarmonics.getN(LGIndex, l-1);
					for (int m = 1; m <= ml2; m++) {
				  	int nl2 = StrainSphericalHarmonics.getN(sampleSymmetry, l-1);
				  	for (int n = 1; n <= nl2; n++) {
							PFwriter.write("  " + Integer.toXRDcatString(l-1) + "  ");
							PFwriter.write(Integer.toXRDcatString(m) + "  ");
							PFwriter.write(Integer.toXRDcatString(n) + "  ");
							PFwriter.write("0.0");
    					PFwriter.newLine();
						}
					}
					ml2 = StrainSphericalHarmonics.getN(LGIndex, l);
					for (int m = 1; m <= ml2; m++) {
				  	int nl2 = StrainSphericalHarmonics.getN(sampleSymmetry, l);
				  	for (int n = 1; n <= nl2; n++) {
							PFwriter.write("  " + Integer.toXRDcatString(l) + "  ");
							PFwriter.write(Integer.toXRDcatString(m) + "  ");
							PFwriter.write(Integer.toXRDcatString(n) + "  ");
							double coeff = coefficient[k++]; // * (2 * l + 1) / Math.sqrt(8.0 * Constants.PI);
							PFwriter.write(Fmt.format(coeff));
    					PFwriter.newLine();
						}
					}
				}
    	} catch (IOException io) {}

    	try {
        PFWriter.flush();
    		PFwriter.close();
    	} catch (IOException io) {} */
    }

  }

  public class HarmonicPane extends JPopaSSListPane {
    public HarmonicPane(Frame parent, boolean showTotal) {
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

    public void setExpansionSlider(int min, int max) {
      expansionJS.setMaximum(max);
      expansionJS.setMinimum(min);
      expansionJS.setPaintTicks(true);
      expansionJS.setMajorTickSpacing(25);
      expansionJS.setMinorTickSpacing(2);

      expansionJS.setPaintLabels(true);
      expansionJS.setSnapToTicks(true);

      expansionJS.setValue(max);

      expansionJS.setLabelTable(expansionJS.createStandardLabels(25));
    }

  }

}

