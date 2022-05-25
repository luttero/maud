/*
 * @(#)AtomInfo.java created 6/04/1999 Pergine Vals.
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

package it.unitn.ing.rista.chemistry;

import java.lang.*;
import java.util.*;
import java.io.*;

import it.unitn.ing.rista.diffr.radiation.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.io.*;

/**
 * The AtomInfo is a class providing physical constants for atoms.
 *
 * @version $Revision: 1.9 $, $Date: 2006/07/20 13:39:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class AtomInfo {

  static Hashtable atomWeights = null;
  static Hashtable atomXrayScatFactors = null;
  static Hashtable atomXrayScatFactors95 = null;
  static Hashtable atomRadii = null;
  static Hashtable atomDispAbs = null;
  static Hashtable atomElectronScatFactors = null;
  static Hashtable atomNeutronScatFactors = null;
  static Hashtable atomMagneticFormFactors = null;
  static Hashtable atomNeutronAbsorptionCrossSection = null;
  static Hashtable atomElectronAbsorptionCrossSection = null;
  static Hashtable atomNeutronScatFactorsAlternate = null;
  static Hashtable atomNeutronScatFactorsIsotope = null;
  static Hashtable atomNumber = null;
  static Vector atomXrayCrossSections = null;
  static Vector labelList = null;
  static Vector isotopeList = null;
  static Hashtable fluorescenceTable = null;

	public static String[] atomLabels = {"H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S",
			"Cl", "Ar", "K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br",
			"Kr", "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe",
			"Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta",
			"W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa", "U",
			"Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md", "No", "Lw", "Uq", "Up", "Uh"};


	static int numberRadiations = XrayTubeRadiation.numbertube;

  private AtomInfo() {
  }

  public static int retrieveAtomNumber(String AtomLabel) {
    double[] number = getValueObjectforKey(AtomLabel, atomNumber);
    if (number != null)
      return (int) number[0];
    else
      return 1;
  }

  public static double retrieveAtomRadius(String AtomLabel) {
    double[] radius = getValueObjectforKey(AtomLabel, atomRadii);
    if (radius != null)
      return radius[0];
    else
      return 1.0;
  }

  public static double retrieveAtomWeight(String AtomLabel) {
    double[] weight = getValueObjectforKey(AtomLabel, atomWeights);
    if (weight != null)
      return weight[0];
    else
      return 1.0;
  }

  public static double[][] retrieveXrayScatFactors() {
    double[][] scatf = new double[AtomNumber()][9];
    for (int i = 0; i < AtomNumber(); i++) {
      double[] tscatf = getValueObjectforKey(getAtomType(i), atomXrayScatFactors);
      if (tscatf == null)
        tscatf = new double[9];
      for (int j = 0; j < 9; j++)
        scatf[i][j] = tscatf[j];
    }
    return scatf;
  }

  public static double[] retrieveXrayScatFactors(String AtomLabel) {
    double[] scatf = getValueObjectforKey(AtomLabel, atomXrayScatFactors);
    if (scatf == null)
      scatf = new double[9];
    return scatf;
  }

  public static double[][] retrieveMagneticScatFactors() {
    double[][] magscatf = new double[AtomNumber()][7];
    for (int i = 0; i < AtomNumber(); i++) {
      double[] tmagscatf = getValueObjectforKey(getAtomType(i));
      if (tmagscatf == null)
        tmagscatf = new double[7];
      for (int j = 0; j < 7; j++)
        magscatf[i][j] = tmagscatf[j];
    }
    return magscatf;
  }

  public static double[] retrieveMagneticScatFactors(String AtomLabel) {
    double[] magscatf = getValueObjectforKey(AtomLabel);
    if (magscatf == null)
      magscatf = new double[7];
    return magscatf;
  }

  public static double[][][] retrieveXrayImSFandAbs() {
    double[][][] disp = new double[numberRadiations][3][AtomNumber()];
    for (int tubeNumber = 0; tubeNumber < numberRadiations; tubeNumber++) {
      for (int i = 0; i < AtomNumber(); i++) {
        double[] disp1 = getValueObjectforKey(getAtomType(i), atomDispAbs);
        if (disp1 != null)
          for (int j = 0; j < 3; j++)
            disp[tubeNumber][j][i] = disp1[j * numberRadiations + tubeNumber];
//        System.out.println(disp[2][i] + " =? " + getAbsorption(retrieveAtomNumber(getAtomType(i)), wave));
      }
    }/* else {
      for (int i = 0; i < AtomNumber(); i++) {
        for (int j = 0; j < 2; j++)
          disp[tubeNumber][j][i] = 0.0;
        disp[tubeNumber][2][i] = getAbsorption(retrieveAtomNumber(getAtomType(i)));
      }
    }*/
    return disp;
  }

  public static double[][] retrieveXrayImSFandAbs(String AtomLabel) {
    double[] disp1 = getValueObjectforKey(AtomLabel, atomDispAbs);
    double[][] disp = new double[numberRadiations][3];
    if (disp1 != null)
      for (int nrad = 0; nrad < numberRadiations; nrad++)
        for (int i = 0; i < 3; i++)
          disp[nrad][i] = disp1[i * numberRadiations + nrad];
    return disp;
  }

  public static double[] retrieveNeutronScatFactor() {
    double[] atomscatfactor = new double[AtomNumber()];
    for (int i = 0; i < AtomNumber(); i++) {
      double[] tatomscatfactor = getValueObjectforKey(getAtomType(i), atomNeutronScatFactorsAlternate);
      if (tatomscatfactor != null)
        atomscatfactor[i] = tatomscatfactor[0];
    }
    return atomscatfactor;
  }

  public static double[] retrieveNeutronIsotopicScatFactor() {
    double[] isotopescatfactor = new double[IsotopeNumber()];
    for (int i = 0; i < IsotopeNumber(); i++) {
      double[] tatomscatfactor = getValueObjectforKey(getIsotopeType(i), atomNeutronScatFactorsIsotope);
      if (tatomscatfactor != null)
        isotopescatfactor[i] = tatomscatfactor[0];
    }
    return isotopescatfactor;
  }

/*  public static double retrieveNeutronScatFactor(String AtomLabel) {
    double[] atomscatfactor = getValueObjectforKey(AtomLabel, atomNeutronScatFactorsAlternate);
    if (atomscatfactor != null)
      return atomscatfactor[0];
    return 0.0;
  }*/

  public static double[] retrieveNeutronAbsorptionCrossSection() {
    double[] absorptionCrossSection = new double[AtomNumber()];
    for (int i = 0; i < AtomNumber(); i++) {
      double[] tabsorptionCrossSection = getValueObjectforKey(getAtomType(i), atomNeutronAbsorptionCrossSection);
      if (tabsorptionCrossSection != null)
        absorptionCrossSection[i] = tabsorptionCrossSection[0] / 1.7982; // is reported for velocity 2200 or lambda 1.7982
    }
    return absorptionCrossSection;

  }

  public static double retrieveNeutronAbsorptionCrossSection(String AtomLabel) {
    double[] absorptionCrossSection = getValueObjectforKey(AtomLabel, atomNeutronAbsorptionCrossSection);
    if (absorptionCrossSection != null)
      return absorptionCrossSection[0] / 1.7982; // is reported for velocity 2200 or lambda 1.7982
    return 0.0;

  }

  public static double[][] retrieveElectronScatFactor() {
    double[][] scatf = new double[AtomNumber()][10];
    for (int i = 0; i < AtomNumber(); i++) {
      double[] tscatf = getValueObjectforKey(getAtomType(i), atomElectronScatFactors);
      if (tscatf == null)
        tscatf = new double[10];
      for (int j = 0; j < 10; j++)
        scatf[i][j] = tscatf[j];
    }
    return scatf;
  }

  public static double[] retrieveElectronAbsorptionCrossSection() {
    double[] absorptionCrossSection = new double[AtomNumber()];
    for (int i = 0; i < AtomNumber(); i++) {
      double[] tabsorptionCrossSection = getValueObjectforKey(getAtomType(i), atomElectronAbsorptionCrossSection);
      if (tabsorptionCrossSection != null)
        absorptionCrossSection[i] = tabsorptionCrossSection[0];
    }
    return absorptionCrossSection;

  }

  public static double retrieveElectronAbsorptionCrossSection(String AtomLabel) {
    double[] absorptionCrossSection = getValueObjectforKey(AtomLabel, atomElectronAbsorptionCrossSection);
    if (absorptionCrossSection != null)
      return absorptionCrossSection[0];
    return 0.0;

  }

  public static int AtomNumber() {
    return labelList.size();
  }

  public static int IsotopeNumber() {
    return isotopeList.size();
  }

  public static String getAtomType(int index) {
    return (String) labelList.elementAt(index);
  }

  public static String getIsotopeType(int index) {
    return (String) isotopeList.elementAt(index);
  }

  public static int getAtomNumber(String label) {
    for (int i = 0; i < AtomNumber(); i++)
      if (getAtomType(i).equalsIgnoreCase(label))
        return i;
    return -1;
  }

  public static int getIsotopeNumber(String label) {
//    System.out.println("isotope: " + label);
    for (int i = 0; i < IsotopeNumber(); i++)
      if (getIsotopeType(i).equalsIgnoreCase(label))
        return i;
//      else
//        System.out.println("Not equal to isotope[" + i + "] " + getIsotopeType(i));      
    return -1;
  }

  public static Vector getOxidationStates(String atomLabel) {
    int atomNumbers = AtomNumber();
    Vector oxList = new Vector(0, 1);
    for (int i = 0; i < atomNumbers; i++) {
      if (atomLabel.equalsIgnoreCase(cutAllNumber(getAtomType(i))))
        oxList.addElement(getOxidationNumber(getAtomType(i)));
    }
    return oxList;
  }

  public static Vector getIsotopeNumbers(String atomLabel) {
    int isotopeNumbers = IsotopeNumber();
    Vector isoList = new Vector(0, 1);
    for (int i = 0; i < isotopeNumbers; i++) {
      if (atomLabel.equalsIgnoreCase(cutAllNumber(getIsotopeType(i))))
        isoList.addElement(getIsotopicNumber(getIsotopeType(i)));
    }
    return isoList;
  }

  public static double[] getValueObjectforKey(String AtomLabel, Hashtable table) {
    if (table == null)
      return null;
    AtomLabel = AtomLabel.toLowerCase();
    if (table.containsKey(AtomLabel))
      return (double[]) table.get(AtomLabel);
    else {
      AtomLabel = cutOxidationNumber(AtomLabel);
      if (table.containsKey(AtomLabel))
        return (double[]) table.get(AtomLabel);
      else {
      AtomLabel = cutIsotopicNumber(AtomLabel);
      if (table.containsKey(AtomLabel))
        return (double[]) table.get(AtomLabel);
        else {
      AtomLabel = cutAllNumber(AtomLabel);
      if (table.containsKey(AtomLabel))
        return (double[]) table.get(AtomLabel);
    }
    }
    }
    return null;
  }

  public static double[] getValueObjectforKey(String AtomLabel) {
    AtomLabel = AtomLabel.toLowerCase();
    if (atomMagneticFormFactors.containsKey(AtomLabel))
      return (double[]) atomMagneticFormFactors.get(AtomLabel);
    else {
      AtomLabel = cutSign(AtomLabel);
      if (atomMagneticFormFactors.containsKey(AtomLabel))
        return (double[]) atomMagneticFormFactors.get(AtomLabel);
    }
    return null;
  }

  public static String cutOxidationNumber(String atomLabel) {
    return Misc.toStringDeleteFinalDigits(atomLabel);
  }

  public static String cutIsotopicNumber(String atomLabel) {
    return Misc.toStringDeleteFirstDigits(atomLabel);
  }

  public static String cutAllNumber(String atomLabel) {
    return Misc.toStringDeleteDigits(atomLabel);
  }

  public static String getOxidationNumber(String atomLabel) {
    return Misc.toStringFinalOnlyDigits(atomLabel);
  }

  public static String cutSign(String atomLabel) {
    boolean nonumber = true;
    for (int i = 0; i < atomLabel.length(); i++)
      if (StringNumber.is0to9(atomLabel.charAt(i)))
        nonumber = false;
    if (nonumber)
      atomLabel = atomLabel + "0";
    return Misc.toStringDeleteSign(atomLabel);
  }

  public static String getIsotopicNumber(String atomLabel) {
    return Misc.toStringFirstOnlyDigits(atomLabel);
  }

  public static void loadAtomConstants() {
    BufferedReader reader;
    if (MaudPreferences.getBoolean("atominfo.useAlternateAtominfoFileinPreferencesFolder", false)) {
      reader = Misc.getReader(Constants.documentsDirectory, "atominfo.cif");
      System.out.println("Reading alternate atominfo.cif at: " + Constants.documentsDirectory);
    } else
      reader = Misc.getResourceReader(Constants.maudJar, "files/atominfo.cif");
    if (reader != null) {
      try {


        String token;
        StringTokenizer st;
        String linedata = reader.readLine();

// atom names and weights
        atomWeights = new Hashtable();
        atomNumber = new Hashtable();

        while (!linedata.startsWith("loop_"))
          linedata = reader.readLine();
        for (int i = 0; i < 4; i++)
          linedata = reader.readLine();

        double[] properties = new double[1];
        String label = null;
        double[] number = new double[1];

        int maxindex;
        int index = 0;
        while (!linedata.startsWith("data")) {
          linedata = reader.readLine();
          st = new StringTokenizer(linedata, "' ,\t\r\n");
          while (st.hasMoreTokens() && !linedata.startsWith("data")) {
            token = st.nextToken();
            switch (index) {
              case 3:
                properties[0] = Double.valueOf(token).doubleValue();
                atomWeights.put(label, properties);
                index = 0;
                properties = new double[1];
                break;
              case 0:
                number[0] = Double.valueOf(token).doubleValue();
                index++;
                break;
              case 1:
                label = token.toLowerCase();
                atomNumber.put(label, number);
                number = new double[1];
              default:
                {
                  index++;
                }
            }
          }
        }

// X-ray scattering factors
        atomXrayScatFactors = new Hashtable();
        labelList = new Vector(0, 1);

        while (!linedata.startsWith("loop_"))
          linedata = reader.readLine();
        for (int i = 0; i < 10; i++)
          linedata = reader.readLine();

        label = null;

        maxindex = 9;
        properties = new double[maxindex];
        index = 0;
        while (!linedata.startsWith("data")) {
          linedata = reader.readLine();
          st = new StringTokenizer(linedata, "' ,\t\r\n");
          while (st.hasMoreTokens()) {
            token = st.nextToken();
            switch (index) {
              case 1:
              case 2:
              case 3:
              case 4:
              case 5:
              case 6:
              case 7:
              case 8:
                properties[index - 1] = Double.valueOf(token).doubleValue();
                index++;
                break;
              case 9:
                properties[8] = Double.valueOf(token).doubleValue();
                atomXrayScatFactors.put(label.toLowerCase(), properties);
                labelList.addElement(label);
                index = 0;
                properties = new double[maxindex];
                break;
              case 0:
                label = token;
              default:
                {
                  index++;
                }
            }
          }
        }

        atomXrayScatFactors95 = new Hashtable();

        while (!linedata.startsWith("loop_"))
          linedata = reader.readLine();
        for (int i = 0; i < 12; i++)
          linedata = reader.readLine();

        label = null;

        maxindex = 11;
        properties = new double[maxindex];
        index = 0;
        while (!linedata.startsWith("data")) {
          linedata = reader.readLine();
          st = new StringTokenizer(linedata, "' ,\t\r\n");
          while (st.hasMoreTokens()) {
            token = st.nextToken();
            switch (index) {
              case 1:
              case 2:
              case 3:
              case 4:
              case 5:
              case 6:
              case 7:
              case 8:
              case 9:
              case 10:
                properties[index - 1] = Double.valueOf(token).doubleValue();
                index++;
                break;
              case 11:
                properties[10] = Double.valueOf(token).doubleValue();
                atomXrayScatFactors95.put(label, properties);
                index = 0;
                properties = new double[maxindex];
                break;
              case 0:
                label = token.toLowerCase();
              default:
                {
                  index++;
                }
            }
          }
        }

        atomRadii = new Hashtable();

        while (!linedata.startsWith("loop_"))
          linedata = reader.readLine();
        for (int i = 0; i < 2; i++)
          linedata = reader.readLine();

        label = null;

        maxindex = 1;
        properties = new double[maxindex];
        index = 0;
        while (!linedata.startsWith("data")) {
          linedata = reader.readLine();
          st = new StringTokenizer(linedata, "' ,\t\r\n");
          while (st.hasMoreTokens()) {
            token = st.nextToken();
            switch (index) {
              case 1:
                properties[0] = Double.valueOf(token).doubleValue();
                atomRadii.put(label, properties);
                index = 0;
                properties = new double[maxindex];
                break;
              case 0:
                label = token.toLowerCase();
              default:
                {
                  index++;
                }
            }
          }
        }

        atomDispAbs = new Hashtable();

        while (!linedata.startsWith("loop_"))
          linedata = reader.readLine();
        maxindex = numberRadiations * 3;
        for (int i = 0; i < maxindex + 1; i++)
          linedata = reader.readLine();

        label = null;

        properties = new double[maxindex];
        index = 0;
        while (!linedata.startsWith("data")) {
          linedata = reader.readLine();
          st = new StringTokenizer(linedata, "' ,\t\r\n");
          while (st.hasMoreTokens()) {
            token = st.nextToken();
            switch (index) {
              case 1:
              case 2:
              case 3:
              case 4:
              case 5:
              case 6:
              case 7:
              case 8:
              case 9:
              case 10:
              case 11:
              case 12:
              case 13:
              case 14:
              case 15:
              case 16:
              case 17:
                properties[index - 1] = Double.valueOf(token).doubleValue();
                index++;
                break;
              case 18:
                properties[17] = Double.valueOf(token).doubleValue();
                atomDispAbs.put(label, properties);
                index = 0;
                properties = new double[maxindex];
                break;
              case 0:
                label = token.toLowerCase();
              default:
                {
                  index++;
                }
            }
          }
        }

        atomNeutronScatFactors = new Hashtable();    // Ralf Grosse-Kunstleve table

        while (!linedata.startsWith("loop_"))
          linedata = reader.readLine();
        for (int i = 0; i < 2; i++)
          linedata = reader.readLine();

        label = null;

        maxindex = 1;
        properties = new double[maxindex];
        index = 0;
        while (!linedata.startsWith("data")) {
          linedata = reader.readLine();
          st = new StringTokenizer(linedata, "' ,\t\r\n");
          while (st.hasMoreTokens()) {
            token = st.nextToken();
            switch (index) {
              case 1:
                properties[0] = Double.valueOf(token).doubleValue();
                atomNeutronScatFactors.put(label, properties);
                index = 0;
                properties = new double[maxindex];
                break;
              case 0:
                label = token.toLowerCase();
              default:
                {
                  index++;
                }
            }
          }
        }

        atomMagneticFormFactors = new Hashtable();

        while (!linedata.startsWith("loop_"))
          linedata = reader.readLine();
        for (int i = 0; i < 8; i++)
          reader.readLine();

        label = null;

        maxindex = 7;
        properties = new double[maxindex];
        index = 0;
        linedata = reader.readLine();
        while ((linedata != null) && !linedata.startsWith("data")) {
          st = new StringTokenizer(linedata, "' ,\t\r\n");
          while (st.hasMoreTokens()) {
            token = st.nextToken();
            switch (index) {
              case 1:
              case 2:
              case 3:
              case 4:
              case 5:
              case 6:
                properties[index - 1] = Double.valueOf(token).doubleValue();
                index++;
                break;
              case 7:
                properties[6] = Double.valueOf(token).doubleValue();
                atomMagneticFormFactors.put(label, properties);
                index = 0;
                properties = new double[maxindex];
                break;
              case 0:
                label = token.toLowerCase();
              default:
                {
                  index++;
                }
            }
          }
          linedata = reader.readLine();
        }

        atomNeutronScatFactorsAlternate = new Hashtable();    // From Sears, FullProf

        while (!linedata.startsWith("loop_"))
          linedata = reader.readLine();
        for (int i = 0; i < 2; i++)
          linedata = reader.readLine();

        label = null;

        maxindex = 1;
        properties = new double[maxindex];
        index = 0;
        while (!linedata.startsWith("data")) {
          linedata = reader.readLine();
          st = new StringTokenizer(linedata, "' ,\t\r\n");
          while (st.hasMoreTokens()) {
            token = st.nextToken();
            switch (index) {
              case 1:
                properties[0] = Double.valueOf(token).doubleValue();
                atomNeutronScatFactorsAlternate.put(label, properties);
                index = 0;
                properties = new double[maxindex];
                break;
              case 0:
                label = token.toLowerCase();
              default:
                {
                  index++;
                }
            }
          }
        }

        atomNeutronAbsorptionCrossSection = new Hashtable();

        while (!linedata.startsWith("_atom_type_abs_cross_sect_neutron_2200"))
          linedata = reader.readLine();

        label = null;

        maxindex = 2;
        properties = new double[maxindex];
        index = 0;
        linedata = reader.readLine();
        while ((linedata != null) && !linedata.startsWith("data")) {
          st = new StringTokenizer(linedata, "' ,\t\r\n");
          while (st.hasMoreTokens()) {
            token = st.nextToken();
            switch (index) {
              case 1:
                properties = new double[maxindex];
                properties[0] = Double.valueOf(token).doubleValue();
                index++;
                break;
              case 2:
                properties[1] = Double.valueOf(token).doubleValue();
                atomNeutronAbsorptionCrossSection.put(label, properties);
                index = 0;
                break;
              case 0:
                label = token.toLowerCase();
              default:
                {
                  index++;
                }
            }
          }
          linedata = reader.readLine();
        }

// X-ray scattering factors
        atomElectronScatFactors = new Hashtable();

        while (!linedata.startsWith("data_atominfo_tables_Electron_scattering_factors"))
          linedata = reader.readLine();

       while (!linedata.startsWith("loop_"))
          linedata = reader.readLine();
        for (int i = 0; i < 12; i++)
          linedata = reader.readLine();

        label = null;

        maxindex = 10;
        properties = new double[maxindex];
        index = 0;
        linedata = reader.readLine();
        while (linedata != null && !linedata.startsWith("data_")) {
          st = new StringTokenizer(linedata, "' ,\t\r\n");
          while (st.hasMoreTokens()) {
            token = st.nextToken();
            switch (index) {
              case 2:
              case 3:
              case 4:
              case 5:
              case 6:
              case 7:
              case 8:
              case 9:
              case 10:
                properties[index - 2] = Double.valueOf(token).doubleValue();
                index++;
                break;
              case 11:
                properties[9] = Double.valueOf(token).doubleValue();
                atomElectronScatFactors.put(label.toLowerCase(), properties);
//                labelList.addElement(label);
                index = 0;
                properties = new double[maxindex];
                break;
              case 0:
                label = token;
              case 1:
              default:
                {
                  index++;
                }
            }
          }
          linedata = reader.readLine();
        }

        atomNeutronScatFactorsIsotope = new Hashtable();
        isotopeList = new Vector(0, 1);

        while (!linedata.startsWith("loop_"))
          linedata = reader.readLine();
        for (int i = 0; i < 11; i++)
          linedata = reader.readLine();

        label = null;

        maxindex = 10;
        properties = new double[maxindex];
        index = 0;
        linedata = reader.readLine();
        while (linedata != null && !linedata.startsWith("data_")) {
          st = new StringTokenizer(linedata, "' ,\t\r\n");
          while (st.hasMoreTokens()) {
            token = st.nextToken();
            switch (index) {
              case 1:
              case 2:
              case 3:
              case 4:
              case 5:
              case 6:
              case 7:
              case 8:
              case 9:
                properties[index - 1] = Double.valueOf(token).doubleValue();
                index++;
                break;
              case 10:
                properties[9] = Double.valueOf(token).doubleValue();
                atomNeutronScatFactorsIsotope.put(label.toLowerCase(), properties);
                isotopeList.addElement(label);
                index = 0;
                properties = new double[maxindex];
                break;
              case 0:
                label = token;
              default:
                {
                  index++;
                }
            }
          }
          linedata = reader.readLine();
        }


      } catch (IOException e) {
        System.out.println("Error loading atom info properties: atominfo.cif missing!");
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
    }
//    loadAtomCrossSections();

    // loadFluorescenceTable();
  }

  public static double AvogadroBarnConv = 0.60221367;

/*  public static void loadAtomCrossSections() {
    BufferedReader reader = Misc.getResourceReader(Constants.maudJar, "files/CrossSection.dat");
    if (reader != null) {
      try {


        String token;
        StringTokenizer st;
        String linedata = reader.readLine();

// atom cross section (X-ray)
        atomXrayCrossSections = new Vector(100, 10);

        while (linedata != null && !linedata.startsWith("#S"))
          linedata = reader.readLine();
        while (linedata != null) {

          double[][] crossSection = new double[2][322];
          int index  = 0;
          st = new StringTokenizer(linedata, "' ,\t\r\n");
          st.nextToken();
          st.nextToken();
          double weight = retrieveAtomWeight(st.nextToken());
          reader.readLine();
          reader.readLine();
          linedata = reader.readLine();
          while (linedata != null && !linedata.startsWith("#S")) {
            st = new StringTokenizer(linedata, "' ,\t\r\n");
            token = st.nextToken();
            crossSection[0][index] = (double) (12.39842 / Double.parseDouble(token) / 1000.0); // to KeV
            for (int j = 2; j < 8; j++)
              token = st.nextToken();
            crossSection[1][index++] = (double) (Double.parseDouble(token) * AvogadroBarnConv /
                    weight);
//            System.out.println(crossSection[0][index - 1] + " " + crossSection[1][index - 1]);
            linedata = reader.readLine();
          }
          atomXrayCrossSections.addElement(crossSection);
        }

      } catch (IOException e) {
        System.out.println("Error loading atom cross section: CrossSection.dat missing!");
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
    }
  }*/

/*  public static double getAbsorption(int atomicNumber, double wave) {
    int atomIndex = atomicNumber - 1;
    if (atomIndex >= atomXrayCrossSections.size())
      return 1.0;
    double[][] crossSection = (double[][]) atomXrayCrossSections.elementAt(atomIndex);
    int numberPoints = crossSection[0].length;
    int i;
    for (i = 0; i < numberPoints; i++) {
      if (crossSection[0][i] < wave)
        break;
    }
    int i1 = i - 1;
    if (i == 0)
      i1 = 0;
    else if (i == numberPoints)
      i1 = numberPoints - 2;
    int i2 = i1 + 1;

    double a = (crossSection[1][i1] - crossSection[1][i2]) / (crossSection[0][i1] - crossSection[0][i2]);
    double b = crossSection[1][i1] - a * crossSection[0][i1];
    return a * wave + b;
  }*/


/*  public static void loadFluorescenceTable() {

    final int ELEMENT = 0, EDGE = 1, CK = 2, CKTOTAL = 3, LINES = 4, PHOTO = 5, SCATTER = 6;
    final String ElementString = "Element";
    final String EndElementString = "EndElement";
    final String EdgeString = "Edge";
    final String PhotoString = "Photo";
    final String ScatterString = "Scatter";
    final String CkString = "  CK";
    final String CkTotalString = "  CKtotal";
    final String LinesString = "  Lines";

    //BufferedReader reader = Misc.getReader(Constants.filesfolder, "ElamDB12.txt");
    BufferedReader reader = Misc.getResourceReader(Constants.maudJar, "files/ElamDB12.txt");
    if (reader != null) {
      fluorescenceTable = new Hashtable();
      try {


        StringTokenizer st;
        String linedata = reader.readLine();

        while (linedata != null && !linedata.startsWith(ElementString))
          linedata = reader.readLine();
        int whatsReading = ELEMENT;
        String label = null;
        FluorescenceElement newElement = null;
        FluorescenceEdge newEdge = null;

//        System.out.println(linedata);
        while (linedata != null) {

//          System.out.println(" - " + linedata);
          switch (whatsReading) {
            case ELEMENT:
              st = new StringTokenizer(linedata, "' \t\r\n");
              st.nextToken();
              newElement = new FluorescenceElement(label = st.nextToken());
              fluorescenceTable.put(label, newElement);
              newElement.setAtomicNumber(Integer.parseInt(st.nextToken()));
              newElement.setAtomicWeight(Double.parseDouble(st.nextToken()));
              newElement.setDensity(Double.parseDouble(st.nextToken()));
              break;
            case EDGE:
              st = new StringTokenizer(linedata, "' \t\r\n");
              st.nextToken();
              newEdge = new FluorescenceEdge(st.nextToken());
              newElement.addEdgeLinesGroup(newEdge);
              newEdge.setEnergy(Double.parseDouble(st.nextToken()));
              newEdge.setFluorescenceYield(Double.parseDouble(st.nextToken()));
              newEdge.setJumpRatio(Double.parseDouble(st.nextToken()));
              break;
            case LINES:
              st = new StringTokenizer(linedata, "' \t\r\n");
              newEdge.addLineGroup(new Line(st.nextToken(), st.nextToken(), st.nextToken(), st.nextToken()));
              break;
            case CK:
              st = new StringTokenizer(linedata, "' \t\r\n");
              st.nextToken();
              while (st.hasMoreTokens()) {
                Ck ck = new Ck(st.nextToken(), Double.parseDouble(st.nextToken()));
                newEdge.addCkGroup(ck);
              }
              break;
            case CKTOTAL:
              st = new StringTokenizer(linedata, "' \t\r\n");
              st.nextToken();
              while (st.hasMoreTokens()) {
                Ck ck = new Ck(st.nextToken(), Double.parseDouble(st.nextToken()));
                newEdge.addTotalCkGroup(ck);
              }
              break;
            case PHOTO:
              st = new StringTokenizer(linedata, "' \t\r\n");
              double[] newPhoto = new double[3];
              for (int i = 0; i < 3; i++)
                newPhoto[i] = Double.parseDouble(st.nextToken());
              newElement.addPhoto(newPhoto);
              break;
            case SCATTER:
              st = new StringTokenizer(linedata, "' \t\r\n");
              double[] newScatter = new double[5];
              for (int i = 0; i < 5; i++)
                newScatter[i] = Double.parseDouble(st.nextToken());
              newElement.addScatter(newScatter);
              break;
            default: {}
          }
          
          linedata = reader.readLine();
//          System.out.println(linedata);
          if (linedata.startsWith(EndElementString)) {
            linedata = reader.readLine();
            whatsReading = -1;
          }

          if (linedata.startsWith(ElementString)) {
            whatsReading = ELEMENT;
          } else if (linedata.startsWith(EdgeString)) {
            whatsReading = EDGE;
          } else if (linedata.startsWith(LinesString)) {
            whatsReading = LINES;
            linedata = reader.readLine();
//            System.out.println(" lines " + linedata);
          } else if (linedata.startsWith(CkString)) {
            whatsReading = CK;
          } else if (linedata.startsWith(CkTotalString)) {
            whatsReading = CKTOTAL;
          } else if (linedata.startsWith(PhotoString)) {
            whatsReading = PHOTO;
            linedata = reader.readLine();
          } else if (linedata.startsWith(ScatterString)) {
            whatsReading = SCATTER;
            linedata = reader.readLine();
          } else if (linedata.startsWith("End")) {
            linedata = null;
          }

        }

      } catch (IOException e) {
        System.out.println("Error loading atom cross section: CrossSection.dat missing!");
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
    }
  }*/

 /* public static Vector<FluorescenceLine> getFluorescenceLinesFor(String label, double energy) {
    Vector<FluorescenceLine> linesForAtom = new Vector(0, 10);
//    double lambda = Constants.ENERGY_LAMBDA / energy;    // in Angstrom

    FluorescenceElement element = (FluorescenceElement) fluorescenceTable.get(label);
    Vector edgeLines = element.edgeLinesGroup;
    for (int i = 0; i < edgeLines.size(); i++) {
      FluorescenceEdge edge = (FluorescenceEdge) edgeLines.elementAt(i);
      double fluorescenceYeld = edge.getFluorescenceYield();
      double jumpRatio = edge.getJumpRatio();
      double jumpFactor = (jumpRatio - 1.0) / jumpRatio * fluorescenceYeld;
      if (energy > edge.getEnergy()) {
        Vector<Line> lines = edge.getLine();
        for (int j = 0; j < edge.getLineCount(); j++) {
            Line line = lines.get(j);
            FluorescenceLine aLine = new FluorescenceLine(line.getEnergy(), -1);
            aLine.setIntensity(line.getIntensity() * jumpFactor);
            // todo manca photoelectric part e linear absortpion da migliorare
            linesForAtom.addElement(aLine);

        }
      }
    }
    return linesForAtom;
  }*/

/*  public static Vector <double[]> getPhotoAbsorptionFor(String label) {
    FluorescenceElement element = (FluorescenceElement) fluorescenceTable.get(label);
    Vector <double[]> photo = new Vector <double[]> (3, 1);
    for (int i = 0; i < 3; i++) {
      double[] x = new double[element.getPhotoAbsorption().size()];
      for (int j = 0; j < x.length; j++)
        x[j] = element.getPhotoAbsorption().elementAt(j)[i];
      photo.add(x);
    }
    return photo;
  }*/
}
