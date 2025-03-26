/*
 * @(#)EPSCmodel.java created 13/12/2024 Povo
 *
 * Copyright (c) 2024 Luca Lutterotti All Rights Reserved.
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

    import com.jtex.arrays.Array1D;
    import com.jtex.geom.Miller;
    import com.jtex.geom.Vec3;
    import it.unitn.ing.fortran.Format;
    import it.unitn.ing.rista.diffr.*;
    import it.unitn.ing.rista.awt.*;
    import it.unitn.ing.rista.util.*;

    import javax.swing.border.*;
    import java.io.*;
    import java.util.Vector;

/**
 *  The EPSCmodel is a class to compute the diffraction shift from
 *  the EPSC model of C. N. Tomè and R. A. Lebenshon, LANL.
 *  Based on EPSC4 version
 *
 * @version $Revision: 1.0 $, $Date: 2024/12/13 12:44:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

// enum usage:
// Ellipsoid.valueOf("EVOLVING") -> EVOLVING
// Ellipsoid array[] = Ellipsoid.values();
// for (Ellipsoid ell : array) {
//    System.out.println(ell + " at index "
//                               + ell.ordinal());
// }
//
// NON_EVOLVING at index 0
// etc

enum Ellipsoid {
  NON_EVOLVING,
  EVOLVING,
  INDIVIDUAL;
}

enum Crysym {
      CUBIC,
      HEXAG,
      TRIGO,
      TETRA,
      ORTHO,
      MONOC,
      TRICL;
}

record PlaneDirections(int h, int k, int l, Vector<double[]> angles) {}

public class EPSCmodel extends Strain {

  public static String[] diclistc = {
      "_rista_epsc_user_title",
      "_rista_epsc_grain_shape_ctrl",
      "_rista_residual_stress_use_texture",
      "_rista_epsc_large_strain_model",
      "_rista_epsc_iterations_self_cons",
      "_rista_epsc_error_self_cons",
      "_rista_epsc_iterations_active_systems",
      "_rista_epsc_use_previous_iteration",
      "_rista_epsc_inverse_pole_figures",
      "_rista_epsc_number_processes",
      "_rista_epsc_inverse_pole_figures",
      "_rista_epsc_inverse_pole_figures",
      "_rista_epsc_inverse_pole_figures",
      "_rista_epsc_stiff_pressure_dependence",  // (iSM=0 no; iSM=1 yes)


      "_rista_epsc_ellipsoid_ratio_x",
      "_rista_epsc_ellipsoid_ratio_y",
      "_rista_epsc_ellipsoid_ratio_z",
      "_rista_epsc_ellipsoid_euler_alpha",
      "_rista_epsc_ellipsoid_euler_beta",
      "_rista_epsc_ellipsoid_euler_gamma",

      "_rista_stiffness_11", // 1
      "_rista_stiffness_12", // 2
      "_rista_stiffness_13", // 3
      "_rista_stiffness_14", // 4
      "_rista_stiffness_15", // 5
      "_rista_stiffness_16", // 6
      "_rista_stiffness_22", // 7
      "_rista_stiffness_23", // 8
      "_rista_stiffness_24", // 9
      "_rista_stiffness_25", // 10
      "_rista_stiffness_26", // 11
      "_rista_stiffness_33", // 12
      "_rista_stiffness_34", // 13
      "_rista_stiffness_35", // 14
      "_rista_stiffness_36", // 15
      "_rista_stiffness_44", // 16
      "_rista_stiffness_45", // 17
      "_rista_stiffness_46", // 18
      "_rista_stiffness_55", // 19
      "_rista_stiffness_56", // 20
      "_rista_stiffness_66", // 21

      "_rista_macrostress_11",
      "_rista_macrostress_22",
      "_rista_macrostress_33",
      "_rista_macrostress_23",
      "_rista_macrostress_13",
      "_rista_macrostress_12",

      "_rista_thermal_expansion_11",
      "_rista_thermal_expansion_22",
      "_rista_thermal_expansion_33",
      "_rista_thermal_expansion_23",
      "_rista_thermal_expansion_13",
      "_rista_thermal_expansion_12"

  };
  public static String[] diclistcrm = {
      "_rista_residual_stress_model",
      "_rista_residual_stress_use_texture",
      "voigt-reuss weight",
      "stiffness_11 (arb)", // 1
      "stiffness_12 (arb)", // 2
      "stiffness_13 (arb)", // 3
      "stiffness_14 (arb)", // 4
      "stiffness_15 (arb)", // 5
      "stiffness_16 (arb)", // 6
//                                      "_rista_stiffness_21",
      "stiffness_22 (arb)", // 7
      "stiffness_23 (arb)", // 8
      "stiffness_24 (arb)", // 9
      "stiffness_25 (arb)", // 10
      "stiffness_26 (arb)", // 11
//                                      "_rista_stiffness_31",
//                                      "_rista_stiffness_32",
      "stiffness_33 (arb)", // 12
      "stiffness_34 (arb)", // 13
      "stiffness_35 (arb)", // 14
      "stiffness_36 (arb)", // 15
//                                      "_rista_stiffness_41",
//                                      "_rista_stiffness_42",
//                                      "_rista_stiffness_43",
      "stiffness_44 (arb)", // 16
      "stiffness_45 (arb)", // 17
      "stiffness_46 (arb)", // 18
//                                      "_rista_stiffness_51",
//                                      "_rista_stiffness_52",
//                                      "_rista_stiffness_53",
//                                      "_rista_stiffness_54",
      "stiffness_55 (arb)", // 19
      "stiffness_56 (arb)", // 20
//                                      "_rista_stiffness_61",
//                                      "_rista_stiffness_62",
//                                      "_rista_stiffness_63",
//                                      "_rista_stiffness_64",
//                                      "_rista_stiffness_65",
      "stiffness_66 (arb)", // 21
      "macrostress_11 (arb)",
      "macrostress_22 (arb)",
      "macrostress_33 (arb)",
      "macrostress_23 (arb)",
      "macrostress_13 (arb)",
      "macrostress_12 (arb)"
  };

  public static String[] classlistcs = {};
  public static String[] classlistc = {};

  public static String[] reminders = {
      "        Information about grain shape:", // line 2
      "        Name and Path for Texture File:", // line 6
      "        Name and Path for Material Data File (Single Crystal File):", // line 9
      "        Precision Settings for Convergence Procedures", // line 11
      "        Input/Output Settings for the Run", // line 15
      "        Number of thermomechanical processes to be run:", // line 22
      "        Path and Name for Process Files", // line 24
      "" // line 28
  };

  String programName = "epsc";
  String insName = "epsc4.ins";

  public String userDefinedTitle = "strain, created by Maud";

  // Information about grain shape:
  public int grainShape = 0;
  public double[] ellipsoidRatios = {1.0, 1.0, 1.0};
  public double[] ellipsoidEulerAngles = {0.0, 0.0, 0.0};

  // Name and Path for Texture File:
//  public String textureFile = "texture.red";
  public boolean textureEvolution = false; // 0 = false, 1 = true

  // Name and Path for Material Data File (Single Crystal File)
//  public String materialDatafile = "crystal.sx";

  // Precision Settings for Convergence Procedures
  public int itmax_mod = 100;
  public double error_mod = 0.01;
  public int itmax_grain = 100;

  // Input/Output Settings for the Run
  public boolean prev_proc = false;    // "i_prev_proc" - Reads state from previous process (1=YES or 0=NO) and related file:
  public String prev_proc_file = "epsc4.out";
  public int  itexskip = 50; // "itexskip" - Sets Frequency of Texture Downloads
//  public boolean diff_dir = true; // "i_diff_dir"    Read diffracting planes and directions (1=YES or 0=NO) and file:
//  public String diff_dirFile = "epsc4.dif";
  public boolean strpf = false; // "i_strpf"       Read directions and calculate strain pole figure (1=YES or 0=NO):

  // Number of thermomechanical processes to be run:
  public int nproc = 3;
  public String[] processFiles = {
      "tension_1.pro",
      "tension_2.pro",
      "unload_3.pro"
  };

  public double spread = 5.0;

  int irandom = 1;
  double[][] e0 = new double[6][6];

  /*  double pfthreshold_tmp = 0.05;
  double[] hj = new double[9], hjm = new double[9];
  //  int[][] mij = new int[3][3];
  double pi, pif, p2i, pi5, pi25, pisim;
  double[][] facun = new double[6][6];
  int[][] mij = new int[6][2];
  int[][] mik = new int[3][3];
  int[] ifiw = new int[73];
  double[] cr2 = new double[181], sr2 = new double[181], cr4 = new double[181], sr4 = new double[181],
      cr6 = new double[181], sr6 = new double[181], cr8 = new double[181], sr8 = new double[181];
  double[][] trigs = new double[9][73];
  double[][] s0 = new double[6][6];
  double[][] c0 = new double[6][6];
  double[][] egeom = new double[6][6], ea = new double[6][6];
  double[][] svoigt = new double[6][6];
  double s12l, c12l, s23l, c23l, s31l, c31l;
  double cda, cdb;
  double[] spmas = new double[9], spmasm = new double[9];
  double[][][] sshi0 = null, sshi0m = null;
  double[][][] hs = new double[36][36][9];
  //  double fio[73][37][73];
  double[][][][] sgeofull = new double[3][3][3][3];
  //	double f[73][37][73];
  double[][] wwarir = new double[36][36];
  double[][] wwarim = new double[36][36];
  //  int iadd[] = new int[9885];
//  int c00 = 0;
  public static int c6 = 6;
  int c73 = 73;
  int c37 = 37;
  double phon = 0.;*/

  boolean debug_output = MaudPreferences.getBoolean("EPSC.debug", false);
  boolean log_output = false;

/*  public static final int[] mi = {1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 2, 3, 4,
      5, 6, 3, 4, 5, 6, 4, 5, 6, 5, 6, 6};
  public static final int[] mj = {1, 2, 3, 4, 5, 6, 2, 3, 4, 5, 6, 3, 4, 5, 6, 4, 5, 6, 5, 6, 6, 1, 1, 1,
      1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5};
  public static final int[] mivoigt = {1, 2, 3, 2, 3, 1, 3, 1, 2};
  public static final int[] mjvoigt = {1, 2, 3, 3, 1, 2, 2, 3, 1};
*/
  private int actualReflexIndex = 0;

  public EPSCmodel(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "EPSC";
    IDlabel = "EPSC";
    description = "select this to apply the EPSC model of Tomè and Lebensohn";
  }

  public EPSCmodel(XRDcat aobj) {
    this(aobj, "Moment Pole Stress");
  }

  public EPSCmodel() {
    identifier = "EPSC";
    IDlabel = "EPSC";
    description = "select this to apply the  EPSC model of Tomè and Lebensohn";
  }

  public void initConstant() {
    Nstring = 2;
    Nstringloop = 0;
    Nparameter = 28;
    Nparameterloop = 0;
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

    double s11 = 168.4;
    double s12 = 121.4;
    parameterField[0] = new Parameter(this, getParameterString(0), 0.5,
        ParameterPreferences.getDouble(getParameterString(0) + ".min", 0),
        ParameterPreferences.getDouble(getParameterString(0) + ".max", 1));
    for (int i = 1; i < 22; i++) {
      if (i == 1 || i == 7 || i == 12) // 11, 22, 33
        parameterField[i] = new Parameter(this, getParameterString(i), s11,
            ParameterPreferences.getDouble(getParameterString(i) + ".min", 1),
            ParameterPreferences.getDouble(getParameterString(i) + ".max", 1000));
      else if (i == 16 || i == 19 || i == 21)  // 44, 55, 66
        parameterField[i] = new Parameter(this, getParameterString(i), 2.0 * (s11 - s12),
            ParameterPreferences.getDouble(getParameterString(i) + ".min", 1),
            ParameterPreferences.getDouble(getParameterString(i) + ".max", 500));
      else if (i == 2 || i == 3 || i == 8)  // 12, 13, 23
        parameterField[i] = new Parameter(this, getParameterString(i), s12,
            ParameterPreferences.getDouble(getParameterString(i) + ".min", 1),
            ParameterPreferences.getDouble(getParameterString(i) + ".max", 300));
      else
        parameterField[i] = new Parameter(this, getParameterString(i), 0,
            ParameterPreferences.getDouble(getParameterString(i) + ".min", -100),
            ParameterPreferences.getDouble(getParameterString(i) + ".max", 100));
    }
    for (int i = 22; i < 28; i++) {
      parameterField[i] = new Parameter(this, getParameterString(i), 0.0,
          ParameterPreferences.getDouble(getParameterString(i) + ".min", -1),
          ParameterPreferences.getDouble(getParameterString(i) + ".max", 1));
    }

    refreshComputation = true;
  }

  double[] macrostress = new double[6];
  int imodel = 0;

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    super.updateParametertoDoubleBuffering(false);

    fio_tmp = null;

    int k = 1;
    for (int i = 0; i < 6; i++)
      for (int j = 0; j < 6; j++) {
        if (i <= j)
          e0[i][j] = parameterValues[k++];
        else
          e0[i][j] = e0[j][i];
      }
    int factor = 1;
    for (int i = 0; i < 6; i++) {
      if (i == 3) factor++;
      macrostress[i] = parameterValues[k++] * factor;
    }
    checkForSymmetries(e0);
  }

  public void checkForSymmetries(double[][] e0) {
  }

  public boolean useTexture() {
    return stringField[1].equalsIgnoreCase("true");
  }

  public void useTexture(boolean status) {
    if (status)
      stringField[1] = "true";
    else
      stringField[1] = "false";
  }

  public void useTexture(String value) {
    stringField[1] = value;
  }

/*  public void computeStrain(Sample asample) {

    Phase aphase = getPhase();
    computeStrain(aphase, asample);

  }*/

  double[][][] fio_tmp = null;
  OutputStream out = null;

  void initAll(Sample asample, Phase aphase, String filename) {
    BufferedWriter output = null;
    if (filename != null) {
      try {
        output = Misc.getWriter(filename);
        filename = null;
        output.write(aphase.getPhaseName() + userDefinedTitle);
        output.newLine();
        output.write(reminders[0]);
        output.newLine();
        output.write(grainShape + "                       Grain Shape and Orientation Control (0 non-evolving ellipsoid; 1 evolving ellipsoid; 2 individual ellipsoid)");
        output.newLine();
        for (int i = 0; i < ellipsoidRatios.length; i++)
          output.write(Fmt.format(ellipsoidRatios[i]) + "  ");
        output.write("         Initial Ellipsoid Ratios");
        output.newLine();
        for (int i = 0; i < ellipsoidEulerAngles.length; i++)
          output.write(Fmt.format(ellipsoidEulerAngles[i]) + "  ");
        output.write("         Initial Euler Angle Ellipsoid Axes");
        output.newLine();
        output.write(reminders[1]);
        output.newLine();
        output.write(aphase.getPhaseName() + ".red");
        output.newLine();
        String texv = "0";
        if (textureEvolution)
          texv = "1";
        output.write(texv + "\t\t\tRotations Due to Slip - IE Texture evolution (0 for no, 1 for yes)");
        output.newLine();
        output.write(reminders[2]);
        output.newLine();
        output.write(aphase.getPhaseName() + ".sx");
        output.newLine();
        output.write(reminders[3]);
        output.newLine();
        output.write(itmax_mod + "                     itmax_mod");
        output.newLine();
        output.write(error_mod + "                     error_mod");
        output.newLine();
        output.write(itmax_grain + "                     itmax_grain");
        output.newLine();

        int prevproc = 0;
        if (prev_proc)
          prevproc = 1;
  /*      int i_diff_dir = 0;
        if (diff_dir)
          i_diff_dir = 1;*/
        int i_strpf = 0;
        if (strpf)
          i_strpf = 1;

        output.write(reminders[4]);
        output.newLine();
        output.write(prevproc + "                       i_prev_proc - Reads state from previous process (1=YES or 0=NO) and related file:");
        output.newLine();
        output.write(prev_proc_file);
        output.newLine();
        output.write(itexskip + "                      itexskip - Sets Frequency of Texture Downloads");
        output.newLine();
        output.write(1 + "       i_diff_dir    Read diffracting planes and directions (1=YES or 0=NO) and file:");
        output.newLine();
        filename = aphase.getPhaseName() + ".dif";
        output.write(filename);
        output.newLine();
        output.write(i_strpf + "       i_strpf       Read directions and calculate strain pole figure (1=YES or 0=NO):");
        output.newLine();
        output.write(reminders[5]);
        output.newLine();
        output.write(nproc + "                       nproc");
        output.newLine();
        output.write(reminders[6]);
        output.newLine();
        for (int i = 0; i < nproc; i++) {
          output.write(processFiles[i]);
          output.newLine();
        }

        output.flush();
        output.close();
      } catch (Exception io) {
        io.printStackTrace();
      }
    }

    if (filename != null) {
      try {
        output = Misc.getWriter(filename);
        output.write("*DIFFRACTING PLANES AND DIRECTION FOR " + aphase.getPhaseName());
        output.newLine();
        output.write("*Number of diffraction directions and diffracting angle spread: 0,0 is 3, 90,90 is 2, 90,0 is 1");
        output.newLine();

        int ndif = 0;
        int hkln = aphase.gethklNumber();
        Vector<PlaneDirections> allData = new Vector<>(hkln);
        for (int j = 0; j < hkln; j++) {
          Reflection refl = aphase.getReflectionVector().elementAt(j);

          if (refl.isGoodforTexture()) {

            Vector<double[]> pf_data = new Vector<>(100, 100);
            int numberDatasets = asample.activeDatasetsNumber();
            for (int i = 0; i < numberDatasets; i++) {
              DataFileSet dataset = asample.getActiveDataSet(i);
              int radCount = dataset.getInstrument().getRadiationType().getLinesCount();
              for (int k = 0; k < dataset.activedatafilesnumber(); k++) {
                DiffrDataFile datafile = dataset.getActiveDataFile(k);
                for (int ppp = 0; ppp < datafile.positionsPerPattern; ppp++) {
                  for (int l = 0; l < radCount; l++) {
                    double[] pfd = new double[3];
                    pfd[2] = datafile.getExperimentalTextureFactors(aphase, j)[ppp][0];
                    double position = datafile.getPositions(aphase)[j][ppp][0];
                    if (!Double.isNaN(pfd[2]) && datafile.isInsideRange(position)) {
                      double[] angles = datafile.getTextureAngles(position);
                      pfd[0] = angles[0];
                      pfd[1] = angles[1];
                      pf_data.add(pfd);
                      ndif++;
                    }
                  }
                }
              }
            }
            PlaneDirections planeData = new PlaneDirections(refl.getH(), refl.getK(), refl.getL(), pf_data);
            allData.add(planeData);
          }
        }

        output.write("    " + ndif + "  " + spread + "   ndif  Spread");
        output.newLine();

        output.flush();
        output.close();
      } catch (Exception io) {
        io.printStackTrace();
      }
    }

  }

  public Vector loadFhkl(Phase aphase, String fhklFilename) {

/*
    This is the format of the input file for the Fourier module of Jana2000 (Petrıcek et al., 2000).
    Its standard extension is .m80. Each line of the file has format (di4,i4,13e12.5),
    where d is the number of reflection indices. The information in each line is:
    reflection indices, number of structure(always 1 in superflip),Fobs, Fobs, Fcalc, A, B
    The rest of the line is compulsory in the format, but it is irrelevant for the output from superflip
    and is padded with zeroes.
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

  public void prepareComputation(Phase aphase, Sample asample) {
    log_output = /* getFilePar().isStrainComputationPermitted() && */getFilePar().logOutput();
    if (log_output)
      out = getFilePar().getResultStream();
    update(false);
//    int igb = SpaceGroups.getLGNumberSiegfriedConv(aphase.getPointGroup());
//    int iga = 1;
//    phon = 0.0;

    if (useTexture())
      irandom = 0;
    else
      irandom = 1;


    programName = MaudPreferences.getPref("epsc.executable_name", programName);

    MaudPreferences.getPref("epsc.instr_filename", insName);
    initAll(asample, aphase, getFilePar().getDirectory() + insName);

    // call superflip
    String epscProgram = Misc.getUserDir() + Constants.pluginsDir + programName;
    try {
      System.out.println("Executing: " + Misc.checkForWindowsPath(epscProgram) + " " + Misc.checkForWindowsPath(insName));
      Executable process = new Executable(Misc.checkForWindowsPath(epscProgram),
          Misc.checkForWindowsPath(getFilePar().getDirectory()), new String[]
          {Misc.checkForWindowsPath(insName)});
      process.start();
      while (!process.getStatus().equals(Executable.TERMINATED))
        Thread.currentThread().sleep(100);
      System.out.println("Execution of EPSC terminate with code: " + process.getTerminationResult());
      process.cleanUp();
//      Runtime.getRuntime().exec(superflipProgram + " " + filename);
      String hklFilename = getFilePar().getDirectory() + insName + ".hkl";
      if (Constants.windoze) {
        hklFilename = Misc.getUserDir() + "\\" + insName + ".hkl";
      }
      System.out.println("EPSC results saved in " + hklFilename);
      Vector hklList = loadFhkl(aphase, hklFilename);

    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }


//    double fmin = 10.0;
/*    Texture atexture = aphase.getActiveTexture();
    atexture.initializeAll();

    double resolution = 5.0; //atexture.getResolutionD(); // for the moment to be changed to variable in the future

    int alphaMaxIndex = (int) (360.0 / resolution + 1.00001);
    int betaMaxIndex = (int) (180.0 / resolution + 1.00001);
    int gammaMaxIndex = (int) (360.0 / resolution + 1.00001);

    fio_tmp = new double[alphaMaxIndex][betaMaxIndex][gammaMaxIndex];
    double odf_min = 1E30;
    double odf_max = -1E30;
    for (int ia = 0; ia < alphaMaxIndex; ia++)
      for (int ib = 0; ib < betaMaxIndex; ib++)
        for (int ig = 0; ig < gammaMaxIndex; ig++) {
          double alpha = resolution * (.25 + ia);
          if (alpha > 360.0)
            alpha -= 360.0;
          double beta = resolution * (.25 + ib);
          if (beta > 180.0)
            beta -= 180.0;
          double gamma = resolution * (.25 + ig);
          if (gamma > 360.0)
            gamma -= 360.0;
          fio_tmp[ia][ib][ig] = atexture.getODF(alpha * Constants.DEGTOPI,
              beta * Constants.DEGTOPI, gamma * Constants.DEGTOPI);
          if (fio_tmp[ia][ib][ig] < odf_min)
            odf_min = fio_tmp[ia][ib][ig];
          if (fio_tmp[ia][ib][ig] > odf_max)
            odf_max = fio_tmp[ia][ib][ig];
//          System.out.println("fio "+fio_tmp[ia][ib][ig]);
        }
//    if (fmin == 1.0)
//      irandom = 1;
//		System.out.println("ODF min/max: " + odf_min + " - " + odf_max);
    if (log_output) {
      try {
        printString(out, "Use texture in moment pole stress computation : " );
        if (irandom == 1)
          printLine(out, "no" );
        else
          printLine(out, "yes");
      } catch (IOException io) {
        io.printStackTrace();
      }
    }
    */
//    System.out.println("irandom "+irandom);

/*   int hkln = aphase.gethklNumber();
    sshi0 = new double[hkln][6][6];
    sshi0m = new double[hkln][6][6];
    double[] cdsc = aphase.lattice();
    for (int j = 0; j < hkln; j++) {
      Reflection refl = aphase.getReflectionVector().elementAt(j);

      double[] sctf = Uwimvuo.tfhkl(refl.getH(), refl.getK(), refl.getL(),
          cdsc[7], cdsc[5], cdsc[3], cdsc[6], cdsc[0], cdsc[1]);
    } */
  }

  public double computeStrain(Reflection refl, double[] strain_angles) { // you don't need to modify this unless
    actualReflexIndex = getPhase().getReflexIndex(refl);
    return super.computeStrain(refl, strain_angles);
  }

  public double computeStrain(double psi, double beta, double chi, double phi) {
    // Angles must be in radiants
    // psi and beta are the polar and azimuthal angles for the crystal setting
    // phi and chi for the sample

    double cfhi = Math.cos(beta);
    double sfhi = Math.sin(beta);
    double cthi = Math.cos(psi);
    double sthi = Math.sin(psi);
    double ctyj = Math.cos(chi);
    double styj = Math.sin(chi);
    double cfyj = Math.cos(phi);
    double sfyj = Math.sin(phi);
    if (debug_output) {
      try {
        printLine(out, "fhi " + beta * Constants.PITODEG + ", " +
            "thi " + psi * Constants.PITODEG + ", " +
            "tyj " + chi * Constants.PITODEG + ", " +
            "fyj " + phi * Constants.PITODEG
        );
      } catch (IOException io) {
        io.printStackTrace();
      }
    }
/*    double[] sla33 = subsla33(imodel, irandom, bk0_tmp,
        sthi, cthi, sfhi, cfhi, 1,
        styj, ctyj,
        sfyj, cfyj, pfthreshold_tmp, fio_tmp);*/
    double strain33 = 0.0;

/*    for (int i = 0; i < 6; i++)
      strain33 += sla33[i] * macrostress[i];*/
    return strain33;
  }


  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JTSStrainOptionsD(parent, this);
    return adialog;
  }

  class JTSStrainOptionsD extends JOptionsDialog {

    JComboBox ssmodelCB = null;
    JCheckBox textureCB;
    JTextField[] pars = null;
    JTextField[] cijTF = null;
    String[] labels = {
        "Grain shape ctrl (0-2): ",
        "    Ellipsoid ratio x : ",
        "    Ellipsoid ratio y : ",
        "    Ellipsoid ratio z : ",
        "    Orientation alpha : ",
        "    Orientation beta  : ",
        "    Orientation gamma : ",
        "    Max iterations (100) : ",
        "    Orientation gamma : ",
        "    Orientation gamma : ",
    };

    public JTSStrainOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      pars = new JTextField[labels.length];

      principalPanel.setLayout(new BorderLayout(6, 6));
      JPanel jPaneln = new JPanel();
      jPaneln.setLayout(new BorderLayout(6, 6));
      principalPanel.add(BorderLayout.NORTH, jPaneln);
      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new GridLayout(0, 2, 3, 3));
      jPaneln.add(BorderLayout.WEST, jPanel8);

      for (int i = 1; i < labels.length; i++) {
        JPanel jpl = new JPanel();
        jpl.setLayout(new FlowLayout(FlowLayout.LEFT));
        jPanel8.add(jpl);
        jpl.add(new JLabel(labels[i]));
        pars[i] = new JTextField(Constants.FLOAT_FIELD);
        pars[i].setText("0");
        jpl.add(pars[i]);
      }

      JPanel jPanel6 = new JPanel();
      jPanel6.setLayout(new GridLayout(0, 1, 3, 3));
      jPaneln.add(BorderLayout.CENTER, jPanel6);
      JPanel jPanel7 = new JPanel();
      jPanel7.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      jPanel6.add(jPanel7);
      jPanel7.add(new JLabel("Stress/strain model: "));
      ssmodelCB = new JComboBox();
//      for (int i = 0; i < stressModels.length; i++)
//        ssmodelCB.addItem(stressModels[i]);
      ssmodelCB.setToolTipText("Select the micromechanical model for strain computation from macrostresses");
      jPanel7.add(ssmodelCB);

      jPanel7 = new JPanel();
      jPanel7.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      jPanel6.add(jPanel7);
      jPanel7.add(new JLabel(labels[0]));
      pars[0] = new JTextField(Constants.FLOAT_FIELD);
      pars[0].setText("0");
      jPanel7.add(pars[0]);

      jPanel7 = new JPanel();
      jPanel7.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      jPanel6.add(jPanel7);
      textureCB = new JCheckBox("Use texture ODF");
      textureCB.setToolTipText("Check the box to use the ODF for strain computation from stress and stiffness tensors");
      jPanel7.add(textureCB);

      jPanel8 = new JPanel();
      jPanel8.setBorder(new TitledBorder(
          new BevelBorder(BevelBorder.LOWERED), "Stiffness matrix"));
      jPanel8.setLayout(new GridLayout(0, 6, 1, 1));
      principalPanel.add(BorderLayout.CENTER, jPanel8);

      cijTF = new JTextField[21];
      int ij = 0;
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          if (i <= j) {
            cijTF[ij] = new JTextField(Constants.FLOAT_FIELD);
            cijTF[ij].setText("0");
            jPanel8.add(cijTF[ij++]);
          } else
            jPanel8.add(new JLabel("-"));
        }
      }

      setTitle("Moment pole figures options panel");
      initParameters();
      pack();

    }

    public void initParameters() {
//      pars[0].setText(parameterField[0].getValue());
      addComponenttolist(pars[0], parameterField[0]);
      for (int i = 1; i < labels.length; i++) {
//        pars[i].setText(parameterField[i + 21].getValue());
        addComponenttolist(pars[i], parameterField[i + 21]);
      }
      for (int i = 0; i < 21; i++) {
//        System.out.println(i + " " + cijTF[i] + " " + parameterField[i+1]);
//        cijTF[i].setText(parameterField[i + 1].getValue());
        addComponenttolist(cijTF[i], parameterField[i + 1]);
      }
//      ssmodelCB.setSelectedItem(getStressModelID());
      textureCB.setSelected(useTexture());
    }

    public void retrieveParameters() {
      parameterField[0].setValue(pars[0].getText());
      for (int i = 1; i < labels.length; i++) {
        parameterField[i + 21].setValue(pars[i].getText());
      }
      for (int i = 0; i < 21; i++) {
        parameterField[i + 1].setValue(cijTF[i].getText());
      }
  //    setStressModel(ssmodelCB.getSelectedItem().toString());
      useTexture(textureCB.isSelected());
    }

  }

}

