/*
 * @(#)FullProfStructureModel.java created May 17, 2011 Nancy
 *
 * Copyright (c) 2011 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.diffr.sfm;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.jsginfo.T_SgInfo;

import javax.swing.*;
import java.io.*;
import java.util.Vector;
import java.awt.*;

/**
 * The FullProfStructureModel is a class to use FullProf program to compute Fhkl2
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: May 17, 2011 3:46:48 PM $
 * @since JDK1.1
 */
public class FullProfStructureModel extends StructureFactorModel {

  public static String[] diclistc = {};
  public static String[] diclistcrm = {};

  public static String[] classlistcs = {};
  public static String[] classlistc = {};

  public static String dummyPowderData = "    0.00    1.00  179.00\n" +
      " 3   141 3   132 3   137 3   135 3   130 3   144 3   136 3   140 3   142 3   122\n" +
      " 3   141 3   130 3   127 3   130 3   131 3   125 3   133 3   136 3   129 3   129\n" +
      " 3   126 3   139 3   120 3   132 3   120 3   132 3   142 3   128 3   129 3   134\n" +
      " 3   137 3   128 3   134 3   142 3   136 3   124 3   118 3   135 3   123 3   134\n" +
      " 3   124 3   134 3   117 3   136 3   131 3   117 3   121 3   119 3   114 3   131\n" +
      " 3   120 3   110 3   118 3   128 3   125 3   125 3   115 3   119 3   113 3   116\n" +
      " 4   122 4   118 4   126 4   112 4   131 4   117 4   116 4   120 4   121 4   124\n" +
      " 4   122 4   114 4   114 4   113 4   123 4   127 4   121 4   120 4   123 4   115\n" +
      " 4   117 4   120 4   122 4   130 4   121 4   123 4   106 4   114 4   108 4   126\n" +
      " 4   118 4   111 4   107 4   124 4   108 4   116 4   112 4   122 4   114 4   123\n" +
      " 4   100 4   119 4   111 4   115 4   113 4   127 4   122 4   117 4   115 4   116\n" +
      " 4   117 4   121 4   111 4   118 4   113 4   118 4   115 4   113 4   124 4   110\n" +
      " 4   119 5   120 5   112 5   119 5   113 5   115 5   115 5   114 5   111 5   112\n" +
      " 5   116 5   113 5   111 5   104 5   113 5   109 5   111 5   117 5   105 5   111\n" +
      " 5   112 5   113 5   112 5   112 5   110 5   108 5   118 5   123 5   121 5   107\n" +
      " 5   116 5   105 5   114 5   113 5   105 5   112 5   110 5   115 5   106 5   110\n" +
      " 5   111 5   111 5   113 5   112 5   112 5   111 5   113 5   108 5   110 5   112\n" +
      " 5   115 5   109 5   108 5   111 5   112 5   110 5   116 5   117 5   105 5   120\n";

//  StructureFactorList[] structureFactorList = null;
  StructureFactor[] absentReflSF = null;
  int numberOfData = 0;
  boolean useAbsentReflections = true;

  boolean fitNotInitialized = true;
  public static String programName = "fp2k";

  protected int Job, Npr, Nph, Nba, Nex, Nsc, Nor, Dum, Iwg, Ilo, Ias, Res, Ste, Nre, Cry, Uni, Cor, Opt, Aut;
  protected int Ipr, Ppl, Ioc, Mat, Pcr, Ls1, Ls2, Ls3, Syo, Prf, Ins, Rpa, Sym, Hkl, Fou, Sho, Ana;
  protected double Lambda1, Lambda2, Ratio, Bkpos, Wdt, Cthm, muR, AsyLim, Rpolarz;
  protected int NCY, Nrefined, MORE;
  protected double Eps, R_at, R_an, R_pr, R_gl, Thmin, Step, Thmax, PSD, Sent0;
  protected double Zero, Sycos, Sysin, Lambda;
  protected String codeRefined = "1.00";
  protected String codeFixed = "0.00";
  protected int numberBackgroundCoefficient = 12;
  protected double[] backgroundParameters = new double[numberBackgroundCoefficient];
  protected int Nat, Dis, Ang;
  protected double Pr1, Pr2, Pr3;
  protected int Jbt, Irf, Isy, Str, Furth;
  protected double ATZ;
  protected int Nvk, Npr1, More;

  public FullProfStructureModel(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "FullProf (J. Rodrigues-Carvajal)";
    IDlabel = "FullProf (J. Rodrigues-Carvajal)";
    description = "select this to use FullProf (J. Rodrigues-Carvajal) for structure factor computation";
  }

  public FullProfStructureModel(XRDcat aobj) {
    this(aobj, "FullProf (J. Rodrigues-Carvajal)");
  }

  public FullProfStructureModel() {
    identifier = "FullProf (J. Rodrigues-Carvajal)";
    IDlabel = "FullProf (J. Rodrigues-Carvajal)";
    description = "select this to use FullProf (J. Rodrigues-Carvajal) for structure factor computation";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  @Override
  public void initDictionary() {
    System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
    System.arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
  }

  public void initParameters() {
    super.initParameters();
    if (Constants.windoze)
      programName = programName + ".exe";
    programName = MaudPreferences.getPref("fullprof.executable_name", programName);

  }

  @Override
  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);
    Constants.STARTING_STRUCTURE_FACTOR = MaudPreferences.getDouble("structure_factors.default_value",
        Constants.STARTING_STRUCTURE_FACTOR);
  }

  @Override
  public String checkIntegrity() {
    programName = MaudPreferences.getPref("fullprof.executable_name", programName);
    String programNameString = Misc.getUserDir() + Constants.pluginsDir + programName;
    File fullProfFile = new File(programNameString);
    if (fullProfFile.exists())
      return null;
    return "FullProf executable missing!\nGo to http://www.ill.eu/sites/fullprof/\ndownload the fullprof suite and put fp2k[.exe] in the\nplugins directory of Maud, where the fullprof.jar file is.";
  }

  public void computeStructureFactors(Sample asample, DataFileSet adataset) {
    Phase aphase = (Phase) getParent();
    Instrument ainstrument = adataset.getInstrument();
    int radCount = ainstrument.getRadiationType().getLinesCount();
    Radiation rad1 = ainstrument.getRadiationType().getRadiation(0);
//    aphase.Fhklcompv(rad1.getRadiationIDNumber(), rad1.tubeNumber, adataset.getIndex(), false);

    programName = MaudPreferences.getPref("fullprof.executable_name", programName);

    String insName = Misc.toStringDeleteBlank(aphase.getPhaseName());
    saveDatafile(getFilePar().getDirectory() + insName + ".dat");
    initAll(aphase, getFilePar().getDirectory() + insName + ".pcr");

    // call superflip
    String fullprofProgram = Misc.getUserDir() + Constants.pluginsDir + programName;
    try {
      System.out.println("Executing: " + Misc.checkForWindowsPath(fullprofProgram) + " " + Misc.checkForWindowsPath(insName));
      Executable process = new Executable(Misc.checkForWindowsPath(fullprofProgram),
          Misc.checkForWindowsPath(getFilePar().getDirectory()), new String[]
          {Misc.checkForWindowsPath(insName)});
      process.start();
      while (!process.getStatus().equals(Executable.TERMINATED))
        Thread.currentThread().sleep(100);
      System.out.println("Execution of fullprof terminate with code: " + process.getTerminationResult());
      process.cleanUp();
//      Runtime.getRuntime().exec(superflipProgram + " " + filename);
      String hklFilename = getFilePar().getDirectory() + insName + ".hkl";
      if (Constants.windoze) {
        hklFilename = Misc.getUserDir() + "\\" + insName + ".hkl";
      }
      System.out.println("FullProf results saved in " + hklFilename);
      Vector hklList = loadFhkl(aphase, hklFilename);
      fitNotInitialized = false;

      int hkln = aphase.gethklNumber();
//      System.out.println("FullProf, number of reflections: " + hkln);
	    double[][] fhkl = new double[hkln][radCount];
      for (int j = 0; j < hkln; j++) {
        Reflection refl = aphase.getReflectionVector().elementAt(j);
          for (int n = 0; n < hklList.size(); n++) {
            HKLIntensityPeak peak = (HKLIntensityPeak) hklList.get(n);
            if (HKLIntensityPeak.equivalent(peak, refl)) {
//              System.out.println("FullProf, h k l: " + refl.h + " " + refl.k + " " + refl.l);

              double factor = peak.intensity * peak.intensity * refl.multiplicity;
              for (int n1 = 0; n1 < radCount; n1++)
                  fhkl[j][n1] = factor;
              hklList.removeElementAt(n);
              break;
            }
          }
      }
	    adataset.storeComputedStructureFactors(aphase, fhkl);
    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }

  }

/*  public void setStructureFactorList(StructureFactorList[] structureFactorList) {
    FilePar aparFile = getFilePar();
    Phase aphase = (Phase) getParent();
    Sample asample = aparFile.getActiveSample();
    int datasetsnumber = asample.activeDatasetsNumber();
    for (int i = 0; i < datasetsnumber; i++) {
      DataFileSet adataset = asample.getActiveDataSet(i);
      int hkln = aphase.gethklNumber();
      for (int j = 0; j < hkln; j++) {
        Reflection refl = aphase.reflectionv.elementAt(j);
//        if (refl.isGoodforStructureFactor() && refl.checkinRange(adataset)) {
          int multiplicity = refl.multiplicity;
          double SF = structureFactorList[i].structureFactor[j].Fhkl_calc;
//          System.out.println(refl.h + " " + refl.k + " " + refl.l + " " + refl);
          SF *= SF;
          refl.setStructureFactor(adataset.getIndex(), SF * multiplicity);
          refl.setExpStructureFactor(adataset.getIndex(), SF * multiplicity);
//        }
      }
    }
  }*/

  public void setStructureFactor(Reflection refl, int adatasetIndex, double fhkl) {
//    refl.setStructureFactor(adatasetIndex, fhkl);
//    refl.setExpStructureFactor(adatasetIndex, fhkl);
  }

  private void saveDatafile(String datafilename) {
    BufferedWriter output = null;
    if (datafilename != null) {
        try {
          output = Misc.getWriter(datafilename);
          output.write(dummyPowderData);
        } catch (IOException io) {
        }
        try {
          output.flush();
          output.close();
        } catch (IOException io) {
        }
    }

  }

  void initAll(Phase aphase, String filename) {
    BufferedWriter output = null;
    if (filename != null) {
        try {
          output = Misc.getWriter(filename);
          output.write("COM " + aphase.getPhaseName() + ", pcr generated by Maud");
          output.newLine();
          output.write("! Files => DAT-file: " + Misc.toStringDeleteBlank(aphase.getPhaseName()) + ",  PCR-file: "
              + Misc.toStringDeleteBlank(aphase.getPhaseName()));
          output.newLine();

          output.write("!Job Npr Nph Nba Nex Nsc Nor Dum Iwg Ilo Ias Res Ste Nre Cry Uni Cor Opt Aut");
          output.newLine();
          RadiationType rad = getFilePar().getSample(0).getDataSet(0).getInstrument().getRadiationType();
          if (rad.isNeutron())
            Job = 3;
          else
            Job = 2;
          Nph = 1;
          Ilo = -1;
          Aut = 1;
          output.write("  " + Job + "   " + Npr + "   " + Nph + "   " + Nba + "   " + Nex + "   " + Nsc + "   " + Nor
                     + "  " + Dum + "   " + Iwg + "   " + Ilo + "   " + Ias + "   " + Res + "   " + Ste + "   " + Nre
                     + "  " + Cry + "   " + Uni + "   " + Cor + "   " + Opt + "   " + Aut);
          output.newLine();
          output.write("!");
          output.newLine();

          output.write("!Ipr Ppl Ioc Mat Pcr Ls1 Ls2 Ls3 Syo Prf Ins Rpa Sym Hkl Fou Sho Ana");
          output.newLine();
          Ins = 1;
          Hkl = 3;
          Sho = 1;
          output.write("  " + Ipr + "  " + Ppl + "  " + Ioc + "  " + Mat + "  " + Pcr + "  " + Ls1 + "  " + Ls2
                     + "  " + Ls3 + "  " + Syo + "  " + Prf + "  " + Ins + "  " + Rpa + "  " + Sym + "  " + Hkl
                     + "  " + Fou + "  " + Sho + "  " + Ana);
          output.newLine();
          output.write("!");
          output.newLine();

          output.write("! lambda1 lambda2    Ratio    Bkpos    Wdt    Cthm     muR   AsyLim   Rpolarz ->Patt# 1");
          output.newLine();         
          Lambda1 = Lambda2 = (double) rad.getMeanRadiationWavelength();
          Ratio = 1.0f;
          Wdt = 0.5f;
          Rpolarz = 0.5f;
          output.write(" " + Lambda1 + " " + Lambda2 + " " + Ratio + " " + Bkpos + " " + Wdt + " " + Cthm
                     + " " + muR + " " + AsyLim + " " + Rpolarz);
          output.newLine();

          output.write("!");
          output.newLine();
          output.write("!NCY  Eps  R_at  R_an  R_pr  R_gl     Thmin       Step       Thmax    PSD    Sent0");
          output.newLine();
          Eps = 0.3f;
          R_at = 0.8f;
          R_an = 0.8f;
          R_pr = 0.8f;
          R_gl = 0.8f;
          Step = 1.0f;
          Thmax = 179.0f;
          output.write(" " + NCY + " " + Eps + " " + R_at + " " + R_an + " " + R_pr + " " + R_gl + " " + Thmin
                     + " " + Step + " " + Thmax + " " + PSD + " " + Sent0);
          output.newLine();

          output.write("!");
          output.newLine();
          output.write("       " + Nrefined + "     !Number of refined parameters");
          output.newLine();
          output.write("!");
          output.newLine();

          output.write("!  Zero    Code    Sycos    Code   Sysin    Code  Lambda     Code MORE ->Patt# 1");
          output.newLine();
          output.write(" " + Zero + " " + codeFixed + " " + Sycos + " " + codeFixed + " " + Sysin + " " + codeFixed
                     + " " + Lambda + " " + codeFixed + " " + MORE);
          output.newLine();

          output.write("!   Background coefficients/codes  for Pattern#  1");
          output.newLine();
          output.write("   ");
          for (int i = 0; i < numberBackgroundCoefficient; i++) {
            output.write(backgroundParameters[i] + " ");
            if (i == 5) {
              output.newLine();
              output.write("       ");
            }
          }
          output.newLine();

          output.write("!-------------------------------------------------------------------------------");
          output.newLine();
          output.write("!  Data for PHASE number:   1  ==> Current R_Bragg for Pattern#  1:     ?.??");
          output.newLine();
          output.write("!-------------------------------------------------------------------------------");
          output.newLine();
          output.write(aphase.getLabel());
          output.newLine();
          output.write("!");
          output.newLine();
          output.write("!Nat Dis Ang Pr1 Pr2 Pr3 Jbt Irf Isy Str Furth     ATZ   Nvk Npr More");
          output.newLine();
          Pr3 = 1.0f;
          Nat = 0;
          for (int i = 0; i < aphase.getFullAtomList().size(); i++)
            if (aphase.getFullAtomList().get(i).useThisAtom &&
                aphase.getFullAtomList().get(i).getQuantityD() > 0)
              Nat++;

          output.write("   " + Nat + " " + Dis + " " + Ang + " " + Pr1 + " " + Pr2 + " " + Pr3 + " " + Jbt
                    + " " +Irf + " " + Isy + " " + Str + " " + Furth + " " + ATZ + " " + Nvk
                    + " " + Npr1 + " " + More);
          output.newLine();
          output.write("!");
          output.newLine();
          T_SgInfo sgInfo = aphase.getPhaseInfo().SgInfo;
          String hallSymbol = sgInfo.getHallSymbolAsString();
          output.write("HALL " + hallSymbol + "                    <--Space group symbol");
          output.newLine();
          output.write("!AtomSite Typ       X        Y        Z     Biso      Occ     In Fin N_t Spc /Codes");
          output.newLine();
//          double[] divideFactors = aphase.getActivePlanarDefects().getDivisionFactors(h, k, l);

          for (int i = 0; i < Nat; i++) {
            AtomSite atom = aphase.getFullAtomList().get(i);
//            double Za = aphase.getSitePositionNumber() / atom.getSiteMultiplicity();
//            double Z = atom.getSiteMultiplicity() / Za;
            double occupancy = (atom.getOccupancy().getValueD() * atom.getSiteMultiplicity() /
		            aphase.getPhaseInfo().getSitePositionNumber());
            if (atom.useThisAtom && occupancy > 0) {
              double[] x = atom.getCoordinates(0);
	            for (int j = 0; j < atom.getNumberOfScatterers(); j++) {
		            AtomScatterer atomScatterer = atom.getAtomScatterer(j);
		            String oxidation = "";
		            if (atomScatterer.getOxidationNumber() != 0) {
			            if (atomScatterer.getOxidationNumber() > 0)
				            oxidation += "+";
			            oxidation += Integer.toString(atomScatterer.getOxidationNumber());
		            }
		            output.write(atom.getLabel() + " " + AtomSite.stripOxidation(AtomSite.stripIsotopeNumber(
				            atomScatterer.getAtomSymbol())).toUpperCase() + oxidation + " " +
				            (x[0] /* divideFactors[0]*/) + " " + (x[1] /* divideFactors[1]*/) + " " +
				            (x[2] /* divideFactors[2]*/) + " " + (atom.getBfactorValue()) + " " +
				            occupancy * atomScatterer.getOccupancy() + "  0  0  0  0");
		            output.newLine();
		            output.write("                0.00     0.00     0.00     0.00     0.00");
		            output.newLine();
	            }
            }
          }

          output.write("!-------> Profile Parameters for Pattern #  1");
          output.newLine();
          output.write("!  Scale        Shape1      Bov      Str1      Str2      Str3   Strain-Model");
          output.newLine();
          output.write("  1.00000     0.00000   0.00000   0.00000   0.00000   0.00000       0");
          output.newLine();
          output.write("     0.00000     0.000     0.000     0.000     0.000     0.000");
          output.newLine();
          output.write("!       U         V          W           X          Y        GauSiz   LorSiz Size-Model");
          output.newLine();
          output.write("   0.000000   0.000000   0.100000   0.000000   0.0000000   0.000000   0.000000    0");
          output.newLine();
          output.write("      0.000      0.000      0.000      0.000      0.000      0.000      0.000");
          output.newLine();
          output.write("!     a          b         c        alpha      beta       gamma");
          output.newLine();
          for (int i = 0; i < 6; i++)
            output.write(" " + aphase.getFullCellValue(i));
          output.newLine();
          output.write("    0.00000    0.00000    0.00000    0.00000    0.00000    0.00000");
          output.newLine();
          output.write("!  Pref1    Pref2      Asy1     Asy2     Asy3     Asy4      S_L      D_L");
          output.newLine();
          output.write("  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000");
          output.newLine();
          output.write("     0.00     0.00     0.00     0.00     0.00     0.00     0.00     0.00");
          output.newLine();
        } catch (IOException io) {
        }
        try {
          output.flush();
          output.close();
        } catch (IOException io) {
        }
    }
  }

  public void computeStructureFactor(int h, int k, int l, int multiplicity, double dspacing, int radType,
                                       int tubeNumber, int adatasetIndex, double factor, double[] structureFactor) {
//    int h = refl.h, k = refl.k, l = refl.l, int multiplicity = refl.multiplicity;
//    double dspacing = refl.d_space;

    if (fitNotInitialized) {
	    for (int i = 0; i < structureFactor.length; i++)
		    structureFactor[i] = Constants.STARTING_STRUCTURE_FACTOR * Constants.STARTING_STRUCTURE_FACTOR;
    } else {
	    for (int i = 0; i < structureFactor.length; i++)
		    structureFactor[i] = factor;
    }
  }

  public Vector loadFhkl(Phase aphase, String fhklFilename) {

/*
    This is the format of the input file for the Fourier module of Jana2000 (Petrıcek et al., 2000).
    Its standard extension is .m80. Each line of the file has format (di4,i4,13e12.5),
    where d is the number of reflection indices. The information in each line is:
    reflection indices, number of structure(always 1 in superflip),Fobs, Fobs, Fcalc, A, B
    The rest of the line is compulsory in the format but it is irrelevant for the output from superflip and is padded with zeroes.
*/
    double sfCalcR = 0;
    String line = null;
    boolean coincidence = false;
    boolean invertHK = aphase.getSymmetry().equalsIgnoreCase("tetragonal"); // ||
//        aphase.getSymmetry().equalsIgnoreCase("hexagonal") ||
//        aphase.getSymmetry().equalsIgnoreCase("trigonal");
    BufferedReader reader = Misc.getReader(fhklFilename);
//    System.out.println("Reading: " + fhklFilename);
    Vector hklList = new Vector(100, 100);
    if (reader != null) {
      try {
        reader.readLine(); // 0 0 0
        reader.readLine(); // 0 0 0
        line = reader.readLine(); // 0 0 0
        while (line != null) {
          line = reader.readLine();
          if (line != null && line.length() > 56) {
            int h1 = Integer.parseInt(Misc.toStringDeleteBlankAndTab(line.substring(0, 4)));
            int k1 = Integer.parseInt(Misc.toStringDeleteBlankAndTab(line.substring(4, 8)));
            int l1 = Integer.parseInt(Misc.toStringDeleteBlankAndTab(line.substring(8, 12)));
            String mult = Misc.toStringDeleteBlankAndTab(line.substring(16, 20));
            String sfcalcS = Misc.toStringDeleteBlankAndTab(line.substring(56, 72));
            sfCalcR = Double.parseDouble(sfcalcS);
            double sfFhkl = Math.sqrt(sfCalcR / Integer.parseInt(mult));
//            if (invertHK)
//              hklList.add(new HKLIntensityPeak(k1, h1, l1, sfFhkl));
//            else
              hklList.add(new HKLIntensityPeak(h1, k1, l1, sfFhkl));
          }
        }
      } catch (Exception e) {
        System.out.println("Error in loading the Fhkl FullProf file!");
        e.printStackTrace();
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
    }
    return hklList;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    return new JFPOptionsD(parent, this);
  }

  public class JFPOptionsD extends JOptionsDialog {

    JTextField[] parsTF = null;
    JCheckBox computeFromMapCB = null;
    JTextField compositionTF = null;
//    JTextField initialSliceTF = null;

    public JFPOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel tfPanel = new JPanel();
      tfPanel.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.NORTH, tfPanel);

      tfPanel.add(new JLabel("FullProf Options: "));
      tfPanel = new JPanel();
      tfPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(BorderLayout.CENTER, tfPanel);

      setTitle("FullProf model options panel");
      initParameters();
      setHelpFilename("fullprof.txt");
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }

  }

}
