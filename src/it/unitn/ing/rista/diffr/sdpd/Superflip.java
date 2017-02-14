/*
 * @(#)Superflip.java created Dec 30, 2007 Riva Del Garda
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.diffr.sdpd;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.io.cif.CIFdictionary;
import it.unitn.ing.rista.io.cif.CIFtoken;

import javax.swing.*;
import java.util.StringTokenizer;
import java.util.Vector;
import java.io.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * The Superflip is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Dec 30, 2007 3:03:30 PM $
 * @since JDK1.1
 */
public class Superflip extends StructureFactorSolveCrystalStructure {

  public static String[] diclistc = {
      "_rita_atom_map_division_number_a",
      "_rita_atom_map_division_number_b",
      "_rita_atom_map_division_number_c",
      "_rita_superflip_compute_from_map",
      "_rita_superflip_cell_composition",
      "_rita_superflip_optional_parameters",
      "_rita_superflip_parameters_file",
      "_rita_superflip_phase_recycle"};
  public static String[] diclistcrm = {
      "_rita_atom_map_division_number_a",
      "_rita_atom_map_division_number_b",
      "_rita_atom_map_division_number_c",
      "_rita_superflip_compute_from_map",
      "_rita_superflip_cell_composition",
      "_rita_superflip_optional_parameters",
      "_rita_superflip_parameters_file",
      "_rita_superflip_phase_recycle"};

  public static String[] classlistcs = {};
  public static String[] classlistc = {};

  StructureFactorList[] structureFactorList = null;
  StructureFactor[] absentReflSF = null;
  int numberOfData = 0;
  boolean useAbsentReflections = true;

  private double atomMap[] = null;
  boolean mapnotLoaded = true;
  boolean computeFromMap = false;
  int aSlices = 0;
  int bSlices = 0;
  int cSlices = 0;
  double[] reducedCell = null;
  int numberOfParameters = 0;
  boolean fitNotInitialized = true;
  public static String superflipName = "superflip";

  public Superflip(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Superflip (Palatinus)";
    IDlabel = "Superflip (Palatinus)";
    description = "select this to obtain Density Maps by Superflip (Palatinus)";
  }

  public Superflip(XRDcat aobj) {
    this(aobj, "Superflip (Palatinus)");
  }

  public Superflip() {
    identifier = "Superflip (Palatinus)";
    IDlabel = "Superflip (Palatinus)";
    description = "select this to obtain Density Maps by Superflip (Palatinus)";
  }

  public void initConstant() {
    Nstring = 8;
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
    String default_res = MaudPreferences.getPref("atomMap.divisionNumber_even", "10");
    setResolution(default_res, default_res, default_res);
    setComputeFromMap(false);
    mapnotLoaded = true;
    if (Constants.windoze)
      superflipName = superflipName + ".exe";
    superflipName = MaudPreferences.getPref("superflip.executable_name", superflipName);

  }

  public void setComputeFromMap(boolean value) {
    if (value)
      stringField[3] = "true";
    else
      stringField[3] = "false";
  }

  @Override
  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);
    computeFromMap = stringField[3].equalsIgnoreCase("true");
    Constants.STARTING_STRUCTURE_FACTOR = MaudPreferences.getDouble("structure_factors.default_value",
        Constants.STARTING_STRUCTURE_FACTOR);
  }

  @Override
  public String checkIntegrity() {
    superflipName = MaudPreferences.getPref("superflip.executable_name", superflipName);
    String superflipProgram = Misc.getUserDir() + Constants.pluginsDir + superflipName;
    File superflipFile = new File(superflipProgram);
    if (superflipFile.exists())
      return null;
    return "Superflip executable missing!\nGo to http://superspace.epfl.ch/superflip/\ndownload the executable for your platform and put it in the\nplugins directory of Maud,x where the ethZurich.jar file is.";
  }

  public boolean computeFromMap() {
    return computeFromMap;
  }

  public boolean solveStructure(StructureFactorList[] structureFactorList) {

    this.structureFactorList = structureFactorList;

    superflipName = MaudPreferences.getPref("superflip.executable_name", superflipName);

	  Phase aphase = (Phase) getParent();

	  String mapName = Misc.toStringDeleteBlank(aphase.getPhaseName()) + ".inflip";
    String filename = getFilePar().getDirectory() + mapName;
    if (mapnotLoaded) {
      loadMapFromFile();
    }
    initAll(filename);

    // call superflip
    String superflipProgram = Misc.getUserDir() + Constants.pluginsDir + superflipName;
    try {
      System.out.println("Executing: " + Misc.checkForWindowsPath(superflipProgram) + " " + Misc.checkForWindowsPath(filename));
      Executable process = new Executable(Misc.checkForWindowsPath(superflipProgram), Misc.checkForWindowsPath(getFilePar().getDirectory()), new String[]{Misc.checkForWindowsPath(filename)});
      process.start();
      while (!process.getStatus().equals(Executable.TERMINATED))
        Thread.currentThread().sleep(100);
      System.out.println("Execution of superflip terminate with code: " + process.getTerminationResult());
      process.cleanUp();
//      Runtime.getRuntime().exec(superflipProgram + " " + filename);
      String xplorFilename = getFilePar().getDirectory() + Misc.toStringDeleteBlank(aphase.getPhaseName()) + ".xplor";
      String m80Filename = getFilePar().getDirectory() + Misc.toStringDeleteBlank(aphase.getPhaseName()) + ".m80";
      if (Constants.windoze) {
        xplorFilename = Misc.getUserDir() + "\\" + Misc.toStringDeleteBlank(aphase.getPhaseName()) + ".xplor";
        m80Filename = Misc.getUserDir() + "\\" + Misc.toStringDeleteBlank(aphase.getPhaseName()) + ".m80";
      }
      System.out.println("Electron density saved in " + xplorFilename);
      loadXplorMap(xplorFilename);
      if (computeFromMap())
        computeFhkl();
      else
        loadFhkl(m80Filename);
      fitNotInitialized = false;
    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }

    mapnotLoaded = false;

    return true;
  }

	void initAll(String filename) {
    boolean unique = MaudPreferences.getBoolean("superflip.reflectionList_unique", true);
//    unique = MaudPreferences.getBoolean("superflip.reflectionList_unique", false);
//    unique = MaudPreferences.getBoolean("superflip.reflectionList_unique", false);

    boolean terminalOutput = MaudPreferences.getBoolean("superflip.terminal_output", true);
    Instrument inst = getFilePar().getSample(0).getDataSet(0).getInstrument();

		Phase aphase = (Phase) getParent();
		BufferedWriter output = null;
    String dir = getFilePar().getDirectory();
    if (filename != null) {
      try {
        output = Misc.getWriter(filename);
        if (getInstructionsFile().length() > 0 && Misc.checkForFile(dir + getInstructionsFile())) {
          BufferedReader reader = Misc.getReader(dir + getInstructionsFile());
          if (reader != null) {
            try {
              String line = "";
              while (line != null) {
                line = reader.readLine();
                if (line != null) {
                  output.write(line);
                  output.newLine();
                }
              }
            } catch (Exception e) {
              System.out.println("Error in loading Superflip preamble instruction file!");
              e.printStackTrace();
            }
            try {
              reader.close();
            } catch (IOException e) {
            }
          }
        } else {
          output.write("title phase " + aphase.getPhaseName());
          output.newLine();
          output.newLine();
          output.write("# Keywords influencing the form of the files");
          output.newLine();
          output.write("outputfile " + Misc.toStringDeleteBlank(aphase.getPhaseName()) + ".m80 "
              + Misc.toStringDeleteBlank(aphase.getPhaseName()) + ".xplor");
          output.newLine();
          output.newLine();
          if (!terminalOutput) {
            output.write("terminal no");
            output.newLine();
            output.newLine();
          }
//          output.write("expandedlog no");
//          output.newLine();
//          output.newLine();
          output.write("# Basic crystallographic information");
          output.newLine();
          if (getCellComposition().length() > 0) {
            output.newLine();
//            output.write("normalize yes");
//            output.newLine();
            output.newLine();
            output.write("composition " + getCellComposition());
            output.newLine();
            output.newLine();
          }
          if (getResolution_a() != null && Integer.valueOf(getResolution_a()) > 0) {
            output.write("dimension 3");
            output.newLine();
            output.write("voxel " + getResolution_a() + " " + getResolution_b() + " " + getResolution_c());
          }
          output.newLine();
          output.newLine();
          output.write("cell");
          for (int i = 0; i < 6; i++)
            output.write(" " + aphase.getFullCellValue(i));
          output.newLine();
          output.write("Space group " + aphase.getSpaceGroup());
          output.newLine();
          output.write("symmetry");
          output.newLine();
          for (int i = 0; i < aphase.getPhaseInfo().getSitePositionNumber(); i++) {
            output.write(changexyz((aphase.getPhaseInfo().getSitePosition(i)).getx()
                + " " + (aphase.getPhaseInfo().getSitePosition(i)).gety()
                + " " + (aphase.getPhaseInfo().getSitePosition(i)).getz()));
            output.newLine();
          }
          output.write("endsymmetry");
          output.newLine();
          output.newLine();
          output.write("lambda " + inst.getRadiationType().getMeanRadiationWavelength());
          output.newLine();
          output.newLine();
          if (getInstructionsParameters().length() > 0) {
            output.write(getInstructionsParameters());
            output.newLine();
          }
        }
        output.newLine();
        output.write("# List of reflections");
        output.newLine();
        if (unique)
          output.write("reflectionlist unique");
        else
          output.write("reflectionlist complete");
        output.newLine();
        if (usePhaseRecycling())
          output.write("dataformat amplitude phase");
        else
          output.write("dataformat amplitude");
        output.newLine();
        output.write("fbegin");
        output.newLine();
        for (StructureFactorList aStructureFactorList : structureFactorList) {
          int reflectionNumber = aStructureFactorList.structureFactor.length;
          for (int j = 0; j < reflectionNumber; j++) {
            StructureFactor sf = aStructureFactorList.structureFactor[j];
            sf.Fhkl_exp = sf.Fhkl_calc; // the calc is the mean value for all exp
            double value = sf.Fhkl_exp;
            if (Math.abs(sf.Fhkl_exp) < 1.0E-3)
              value = 0.0f;
            String phase = "?";
            if (sf.phase_exp > -999.8)
              phase = Double.toString(sf.phase_exp);
            if (sf.weight > 0) {
              output.write(sf.h + " " + sf.k + " " + sf.l + " " + value);
              if (usePhaseRecycling())
                output.write(" " + phase);
              output.newLine();
            }
          }
        }
        output.write("endf");
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

  private String changexyz(String xyz) {
    StringBuffer s = new StringBuffer("");
    for (int i = 0; i < xyz.length(); i++) {
      char c = xyz.charAt(i);
      if (c == 'x')
        s.append("x1");
      else if (c == 'y')
        s.append("x2");
      else if (c == 'z')
        s.append("x3");
      else
        s.append(c);
    }
    return s.toString();
  }

  public void computeFhkl() {
    for (int i = 0; i < structureFactorList.length; i++) {
      int reflectionNumber = structureFactorList[i].structureFactor.length;
      for (int j = 0; j < reflectionNumber; j++) {
        StructureFactor sf = structureFactorList[i].structureFactor[j];
        double[] Fhkl = Fhklcomp(sf.h, sf.k, sf.l, atomMap, aSlices, bSlices, cSlices);
        sf.Fhkl_calc = Math.sqrt(Fhkl[0] * Fhkl[0] + Fhkl[1] * Fhkl[1]);
//        System.out.println(sf.h + " " + sf.k + " " + sf.l + " " + sf.Fhkl_calc);
        if (getFilePar().isStructureFactorComputationPermitted())
          sf.Fhkl_exp = sf.Fhkl_calc;
      }
    }
  }

	public double computeStructureFactor(int h, int k, int l, int multiplicity, double dspacing, RadiationType radType,
	                                     double defaultFactor) {
//    int h = refl.h, k = refl.k, l = refl.l, int multiplicity = refl.multiplicity;
//    double dspacing = refl.d_space;

    if (computeFromMap()) {
      if (atomMap == null)
        resetMAP();
      if (fitNotInitialized)
        return Constants.STARTING_STRUCTURE_FACTOR * Constants.STARTING_STRUCTURE_FACTOR;
//    Phase aphase = (Phase) getParent();
      double Fhkls; // = -1.0; // refl.getStructureFactor(adataset.getIndex());
//    if (Fhkls == -1.0) {
      double[] Fhkl = Fhklcomp(h, k, l, atomMap, aSlices, bSlices, cSlices);
      Fhkls = (Fhkl[0] * Fhkl[0] + Fhkl[1] * Fhkl[1]) * multiplicity;
//    System.out.println(h + " " + k + " " + l + " " + Fhkls);
//    refl.setStructureFactor(adatasetIndex, Fhkls);
//    }

      return Fhkls;
    } else {
      return defaultFactor;
    }
  }

  public double[] Fhklcomp(int h, int k, int l, double[] map,
                           int aSlices, int bSlices, int cSlices) {
    // compute Fhkl from electron map
    double[] a1 = new double[2];
    a1[0] = 0.0;
    a1[1] = 0.0;
    Phase aphase = (Phase) getParent();
    int siteNumber = 1;
//    if (useAllSites)
    siteNumber = aphase.getPhaseInfo().getSitePositionNumber();
    double factors = aphase.getActivePlanarDefects().getStructureFactorModifier(null);
    double[] divideFactors = aphase.getActivePlanarDefects().getDivisionFactors();
    double norm = Math.sqrt(factors) / (siteNumber * reducedCell[0] * reducedCell[1] * reducedCell[2]);
    SitePosition[] sitepos = new SitePosition[siteNumber];
    for (int i = 0; i < siteNumber; i++) {
      sitepos[i] = aphase.getPhaseInfo().getSitePosition(i);
    }
    double x[][] = new double[3][siteNumber], xf[] = new double[3];

    double fi_ra = 1.0 / aSlices * reducedCell[0];
    double fi_rb = 1.0 / bSlices * reducedCell[1];
    double fi_rc = 1.0 / cSlices * reducedCell[2];

//    double constantPart = Constants.PI * (fi_ra + fi_rb + fi_rc);
    double multPart = Constants.PI2;
    int i = 0;
    xf[2] = 0;
    for (int iz = 0; iz < cSlices; iz++) {
      xf[1] = 0;
      for (int iy = 0; iy < bSlices; iy++) {
        xf[0] = 0;
        for (int ix = 0; ix < aSlices; ix++) {
          for (int is = 0; is < siteNumber; is++) {
            for (int js = 0; js < 3; js++)
              x[js][is] = sitepos[is].getcoord(js, xf);
            double arg = multPart * (h * x[0][is] * divideFactors[0] + k * x[1][is] * divideFactors[1] +
                l * x[2][is] * divideFactors[2]);
            double w1 = Math.cos(arg);
            double w2 = Math.sin(arg);
            a1[0] += map[i] * w1 * norm;
            a1[1] += map[i] * w2 * norm;
          }
          i++;
          xf[0] += fi_ra;
        }
        xf[1] += fi_rb;
      }
      xf[2] += fi_rc;
    }
    return a1;
  }

  public void setResolution(String a_value, String b_value, String c_value) {
    boolean resetMap = false;
    if (a_value != null && !a_value.equals(getResolution_a())) {
      stringField[0] = new String(a_value);
      resetMap = true;
    }
    if (b_value != null && !b_value.equals(getResolution_b())) {
      stringField[1] = new String(b_value);
      resetMap = true;
    }
    if (c_value != null && !c_value.equals(getResolution_c())) {
      stringField[2] = new String(c_value);
      resetMap = true;
    }
    if (resetMap)
      resetMAP();
  }

  public String getResolution_a() {
    return stringField[0];
  }

  public String getResolution_b() {
    return stringField[1];
  }

  public String getResolution_c() {
    return stringField[2];
  }

  public int getResolutionD_a() {
//    System.out.println("res " + getResolution());
    return Integer.valueOf(getResolution_a());
  }

  public int getResolutionD_b() {
//    System.out.println("res " + getResolution());
    return Integer.valueOf(getResolution_b());
  }

  public int getResolutionD_c() {
//    System.out.println("res " + getResolution());
    return Integer.valueOf(getResolution_c());
  }

  public String getCellComposition() {
    return stringField[4];
  }

  public void setCellComposition(String comp) {
    stringField[4] = comp;
  }

  public String getInstructionsParameters() {
    return stringField[5];  //To change body of created methods use File | Settings | File Templates.
  }

  public void setInstructionsParameters(String text) {
    stringField[5] = text;
  }

  public String getInstructionsFile() {
    return stringField[6];  //To change body of created methods use File | Settings | File Templates.
  }

  public void setInstructionsFile(String text) {
    stringField[6] = text;
  }

  public boolean usePhaseRecycling() {
    if (stringField[7].equalsIgnoreCase("true"))
      return true;
    return false;
  }

  public void setPhaseRecycling(boolean value) {
    if (value)
      stringField[7] = "true";
    else
      stringField[7] = "false";
  }

  public void loadXplorMap(String xplorFilename) {
    BufferedReader reader = Misc.getReader(xplorFilename);
    if (reader != null) {
      try {
        StringTokenizer st;
        String linedata;

        reader.readLine(); // blank
        reader.readLine(); // always 1
        reader.readLine(); // title

        linedata = reader.readLine();
        st = new StringTokenizer(linedata, "() ,\t\r\n");

        String a_value = st.nextToken();
        st.nextToken();
        st.nextToken();
        String b_value = st.nextToken();
        st.nextToken();
        st.nextToken();
        String c_value = st.nextToken();

//	      double a1 = Double.parseDouble(c_value);

        setResolution(a_value, b_value, c_value);

        reader.readLine(); // cell parameters
        reader.readLine(); // ZYX

        int aSlices1 = getASlices();
        int bSlices1 = getBSlices();
        int cSlices1 = getCSlices();
        atomMap = new double[computeParameterNumber()];
        int index = 0;
        for (int nc = 0; nc < cSlices1; nc++) {
          reader.readLine(); // nc increasing
          for (int nb = 0; nb < bSlices1; nb++)
            for (int na = 0; na < aSlices1; na += 6) {
              linedata = reader.readLine(); // map value
//              st = new StringTokenizer(linedata, "() ,\t\r\n");
              for (int s = 0; s < 6; s++) {
                if (s + na < aSlices1)
                  atomMap[index++] = Float.valueOf(linedata.substring(s * 12, (s + 1) * 12));
              }
            }
        }
        atomMapNormalization();
        mapnotLoaded = false;

      } catch (IOException e) {
        System.out.println("Error in loading the xplor map file!");
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
    }
  }

  public void loadFhkl(String xplorFilename) {

/*
    This is the format of the input file for the Fourier module of Jana2000 (Petrıcek et al., 2000).
    Its standard extension is .m80. Each line of the file has format (di4,i4,13e12.5),
    where d is the number of reflection indices. The information in each line is:
    reflection indices, number of structure(always 1 in superflip),Fobs, Fobs, Fcalc, A, B
    The rest of the line is compulsory in the format but it is irrelevant for the output from superflip and is padded with zeroes.
*/
    boolean unique = MaudPreferences.getBoolean("superflip.reflectionList_unique", true);

    double sfCalcR = 0;
    String line = null;
    boolean coincidence = false;
    BufferedReader reader = Misc.getReader(xplorFilename);
    if (reader != null) {
      try {
        line = reader.readLine(); // 0 0 0
/*        if (unique) {
        for (StructureFactorList aStructureFactorList : structureFactorList) {
          int reflectionNumber = aStructureFactorList.structureFactor.length;
          for (int j = 0; j < reflectionNumber; j++) {
            StructureFactor sf = aStructureFactorList.structureFactor[j];
            if (sf.weight > 0) {
              if (!coincidence)
                line = reader.readLine();
              else
                coincidence = false;
//            System.out.println(line);
              int h1 = Integer.parseInt(Misc.toStringDeleteBlankAndTab(line.substring(0, 4)));
              int k1 = Integer.parseInt(Misc.toStringDeleteBlankAndTab(line.substring(4, 8)));
              int l1 = Integer.parseInt(Misc.toStringDeleteBlankAndTab(line.substring(8, 12)));
              if ((sf.h == h1 && sf.k == k1 && sf.l == l1) ||
                  (sf.h == k1 && sf.k == h1 && sf.l == l1)) {
                String sfcalcS = Misc.toStringDeleteBlankAndTab(line.substring(40, 52));
                String sfcalcA = Misc.toStringDeleteBlankAndTab(line.substring(52, 64));
                String sfcalcB = Misc.toStringDeleteBlankAndTab(line.substring(64, 76));
                sfCalcR = Double.parseDouble(sfcalcS);
                sf.Fhkl_calc = sfCalcR;
                if (getFilePar().isStructureFactorComputationPermitted())
                  sf.Fhkl_exp = sf.Fhkl_calc;
              } else {
//              System.out.println("Warning, the reflection list has changed!");
                coincidence = true;
                sf.Fhkl_calc = sfCalcR;
                if (getFilePar().isStructureFactorComputationPermitted())
                  sf.Fhkl_exp = sf.Fhkl_calc;
              }
//              System.out.println(sf.h + " " + sf.k + " " + sf.l + " " + sf.Fhkl_calc);
            }
          }
        }
        } else {*/
        Vector hklList = new Vector(100, 100);
        while (line != null) {
          line = reader.readLine();
          if (line != null && line.length() > 56) {
            int h1 = Integer.parseInt(Misc.toStringDeleteBlankAndTab(line.substring(0, 4)));
            int k1 = Integer.parseInt(Misc.toStringDeleteBlankAndTab(line.substring(4, 8)));
            int l1 = Integer.parseInt(Misc.toStringDeleteBlankAndTab(line.substring(8, 12)));
            sfCalcR = Double.parseDouble(Misc.toStringDeleteBlankAndTab(line.substring(40, 52)));
            double sfCalcA = Double.parseDouble(Misc.toStringDeleteBlankAndTab(line.substring(52, 64)));
            double sfCalcB = Double.parseDouble(Misc.toStringDeleteBlankAndTab(line.substring(64, 76)));
            hklList.add(new HKLIntensityPeak(h1, k1, l1, sfCalcR, sfCalcA, sfCalcB));
          }
        }
        for (StructureFactorList aStructureFactorList : structureFactorList) {
          int reflectionNumber = aStructureFactorList.structureFactor.length;
          for (int j = 0; j < reflectionNumber; j++) {
            boolean found = false;
            StructureFactor sf = aStructureFactorList.structureFactor[j];
            StructureFactor sf1 = null;
            if (j > 0) {
              sf1 = aStructureFactorList.structureFactor[j - 1];
              sf.Fhkl_calc = sf1.Fhkl_calc;
              sf.phase_calc = sf1.phase_calc;
              if (getFilePar().isStructureFactorComputationPermitted()) {
                sf.Fhkl_exp = sf.Fhkl_calc;
                sf.phase_exp = sf.phase_calc;
              }
            }
//            System.out.println("Find: " + sf.h + " " + sf.k + " " + sf.l);
            for (int n = 0; n < hklList.size(); n++) {
              HKLIntensityPeak refl = (HKLIntensityPeak) hklList.get(n);
//              System.out.println("       Check: " + refl.h + " " + refl.k + " " + refl.l);
              if (HKLIntensityPeak.equivalent(refl, sf)) {
                sf.Fhkl_calc = refl.intensity;
                sf.phase_calc = MoreMath.phase(refl.A, refl.B);
//                  System.out.println("Found: " + sf.h + " " + sf.k + " " + sf.l + " " + sf.Fhkl_calc + " " +
//                      refl.A + " " + refl.B + " " + sf.phase_calc);
                if (getFilePar().isStructureFactorComputationPermitted()) {
                  sf.Fhkl_exp = sf.Fhkl_calc;
                  sf.phase_exp = sf.phase_calc;
                }
                hklList.removeElementAt(n);
                found = true;
                break;
              }
            }
            if (!found) {
              System.out.println("Not found: " + sf.h + " " + sf.k + " " + sf.l);
              if (sf1 != null)
                System.out.println("Assigned tentatively as: " + sf1.h + " " + sf1.k + " " + sf1.l);
            }
          }
        }
//        }
      } catch (Exception e) {
        System.out.println("Error in loading the Fhkl m80 file!");
        e.printStackTrace();
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
    }
  }

  public void atomMapNormalization() {
    double[] a1 = new double[2];
    a1[0] = 0.0;
    a1[1] = 0.0;
    Phase aphase = (Phase) getParent();
    int siteNumber = 1;
//    if (useAllSites)
    siteNumber = aphase.getPhaseInfo().getSitePositionNumber();
    double norm = 1.0;// / (siteNumber * reducedCell[0] * reducedCell[1] * reducedCell[2]);
    int index = 0;
    double totalMap = 0.0;
    for (int ix = 0; ix < cSlices; ix++)
      for (int iy = 0; iy < bSlices; iy++)
        for (int iz = 0; iz < aSlices; iz++)
          totalMap += MoreMath.positive_or_zero((float) atomMap[index++]);
    norm = aphase.getAtomMapNormalization() / totalMap * (reducedCell[0] * reducedCell[1] * reducedCell[2]);
    index = 0;
    for (int nc = 0; nc < cSlices; nc++)
      for (int nb = 0; nb < bSlices; nb++)
        for (int na = 0; na < aSlices; na++)
          atomMap[index++] *= norm;

    System.out.println("Normalization factor: " + Fmt.format(norm) + ", total: "
        + totalMap);
//    useAllSites = MaudPreferences.getBoolean("meemMap.useAllSites", false);
//    if (reducedCell[0] != 1.0 || reducedCell[1] != 1.0 || reducedCell[2] != 1.0)
//      useAllSites = true;
//    useequivalentReflections = MaudPreferences.getBoolean("meemMap.useMultipleReflections", false);

  }

/*  public void atomMapNormalization() {
    Phase aphase = (Phase) getParent();
    int siteNumber = 1;
//    if (useAllSites)
      siteNumber = aphase.getSitePositionNumber();
    double norm = siteNumber * reducedCell[0] * reducedCell[1] * reducedCell[2];
    int index = 0;
    double totalMap = 0.0;
    for (int ix = 0; ix < aSlices; ix++)
      for (int iy = 0; iy < bSlices; iy++)
        for (int iz = 0; iz < cSlices; iz++)
          totalMap += atomMap[index++]; // * norm;
    norm = aphase.getAtomMapNormalization() / totalMap;
    index = 0;
    for (int na = 0; na < aSlices; na++)
      for (int nb = 0; nb < bSlices; nb++)
        for (int ng = 0; ng < cSlices; ng++)
          atomMap[index++] *= norm;

//    System.out.println("Normalization factor: " + Fmt.format(norm) + ", total: "
//        + totalMap);
//    useAllSites = MaudPreferences.getBoolean("meemMap.useAllSites", false);
//    if (reducedCell[0] != 1.0 || reducedCell[1] != 1.0 || reducedCell[2] != 1.0)
//      useAllSites = true;
//    useequivalentReflections = MaudPreferences.getBoolean("meemMap.useMultipleReflections", false);

  }*/

  public void loadMapFromFile() {
	  String filename = new String(getFilePar().getDirectory() +
			  getParent().toXRDcatString() + ".map");
    atomMap = MAPinputStandard(filename,
        getResolutionD_a(), getResolutionD_b(), getResolutionD_c());

    atomMapNormalization();
    mapnotLoaded = false;

  }

  public void atomMapInitialization() {
		if (reducedCell == null)
			reducedCell = new double[3];
    for (int i = 0; i < 3; i++)
      reducedCell[i] = 1.0; // ((Phase) getParent()).reducedCellFactor[i];

    aSlices = getResolutionD_a();
    bSlices = getResolutionD_b();
    cSlices = getResolutionD_c();

    numberOfParameters = computeParameterNumber();
//		totalWeight = new double[numberOfParameters];
//    useAllSites = MaudPreferences.getBoolean("meemMap.useAllSites", false);
//    if (reducedCell[0] != 1.0 || reducedCell[1] != 1.0 || reducedCell[2] != 1.0)
//      useAllSites = true;
//    useequivalentReflections = MaudPreferences.getBoolean("meemMap.useMultipleReflections", false);

  }

  public int computeParameterNumber() {
    aSlices = getResolutionD_a();
    bSlices = getResolutionD_b();
    cSlices = getResolutionD_c();

    return aSlices * bSlices * cSlices;
  }

  public void resetMAP() {

    atomMapInitialization();

//    MersenneTwisterFast randomizer = new MersenneTwisterFast();

    int aSlices1 = getASlices();
    int bSlices1 = getBSlices();
    int cSlices1 = getCSlices();
    atomMap = new double[computeParameterNumber()];
    int index = 0;
    for (int nc = 0; nc < cSlices1; nc++)
      for (int nb = 0; nb < bSlices1; nb++)
        for (int na = 0; na < aSlices1; na++)
          atomMap[index++] = 1.0f; // (double) randomizer.nextDouble();
    atomMapNormalization();
    fitNotInitialized = true;

    mapnotLoaded = false;
  }

  public void shakeMAP() {


    int time = (int) System.currentTimeMillis();  // safe because we're getting low-order bits
    ec.util.MersenneTwisterFast randomizer = new ec.util.MersenneTwisterFast(time);

    int aSlices1 = getASlices();
    int bSlices1 = getBSlices();
    int cSlices1 = getCSlices();
//    atomMap = new double[computeParameterNumber()];
    int index = 0;
    for (int na = 0; na < cSlices1; na++)
      for (int nb = 0; nb < bSlices1; nb++)
        for (int ng = 0; ng < aSlices1; ng++)
          atomMap[index] = atomMap[index++] * (double) (randomizer.nextDouble() * 0.01) +
              (double) (randomizer.nextDouble() * 0.001);

    atomMapNormalization();
    mapnotLoaded = false;
  }

  public int getASlices() {
    return aSlices;
  }

  public int getBSlices() {
    return bSlices;
  }

  public int getCSlices() {
    return cSlices;
  }

  public void writeCustomObject(BufferedWriter out) {

    if (atomMap == null)
      return;
    int aSlices1 = getASlices();
    int bSlices1 = getBSlices();
    int cSlices1 = getCSlices();

    try {
      out.newLine();
      out.write("#custom_object_" + "atomMap");
      out.newLine();
      out.write(CIFdictionary.loopDecl);
      out.newLine();
      out.write(CIFdictionary.atomMap_values);
      out.newLine();
      int index = 0;
      for (int nc = 0; nc < cSlices1; nc++) {
        for (int nb = 0; nb < bSlices1; nb++) {
          for (int na = 0; na < aSlices1; na++) {
            double value = atomMap[index++];
            out.write(Double.toString(value) + " ");
          }
          out.newLine();
        }
        out.newLine();
      }
      out.newLine();
      out.write("#end_custom_object_" + "atomMap");
      out.newLine();
      out.newLine();
    } catch (IOException ioe) {
      System.out.println("Error in writing the atom Map for " + toXRDcatString());
    }

  }

  public void readCustomObject(CIFtoken ciffile) {
    // to be override by subclasses
    // the default read and do nothing
//    String thecife;
    int tokentype;
//		XRDcat theobj = null;
    boolean endofInput = false;
    int aindex = 0, bindex = 0, gindex = 0;
    int index = -1;
    resetMAP();
    int aSlices1 = getCSlices();
    int bSlices1 = getBSlices();
    int cSlices1 = getASlices();

    try {
      do {
        tokentype = ciffile.nextToken();
        switch (tokentype) {
          case CIFtoken.TT_CIFE:
            // should be the CIF entry for atomMap values
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
            // index = MAPindex(aindex, bindex, gindex);
            index++;
            if (index < atomMap.length)
              atomMap[index] = (double) ciffile.thevalue;
            fitNotInitialized = false;

            gindex++;
            if (gindex == cSlices1) {
              bindex++;
              gindex = 0;
            }
            if (bindex == bSlices1) {
              aindex++;
              bindex = 0;
            }
            break;
          case CIFtoken.TT_CUSTOM_END:
            // subordinate loop
            endofInput = true;
            break;
          default: {
          }
        }
      } while (tokentype != CIFtoken.TT_EOF && !endofInput);
    } catch (IOException ioe) {
      System.out.println("IO exception in custom object for " + toXRDcatString());
    }

    /*		if (theobj != null)
			theobj.readall(ciffile);*/
  }

  public void MAPoutputStandard(String filename, double[] map, int ares, int bres, int cres) {

    int aSlices1 = getASlices();
    int bSlices1 = getBSlices();
    int cSlices1 = getCSlices();
    BufferedWriter PFwriter = Misc.getWriter(filename);
    if (PFwriter != null) {
      try {

        PFwriter.write(Integer.toString(ares));
        PFwriter.write(" ");
        PFwriter.write(Integer.toString(bres));
        PFwriter.write(" ");
        PFwriter.write(Integer.toString(cres));
        PFwriter.newLine();
        int index = 0;
        for (int nc = 0; nc < cSlices1; nc++) {
          for (int nb = 0; nb < bSlices1; nb++) {
            for (int na = 0; na < aSlices1; na++) {
              double value = map[index++];
              PFwriter.write(Double.toString(value) + " ");
            }
            PFwriter.newLine();
          }
          PFwriter.newLine();
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

  public double[] MAPinputStandard(String filename, int ares, int bres, int cres) {

    double[] map = new double[getASlices() * getBSlices() * getCSlices()];
    BufferedReader PFreader = Misc.getReader(filename);

    int aSlices1 = getASlices();
    int bSlices1 = getBSlices();
    int cSlices1 = getCSlices();

    if (PFreader != null) {
      try {

        String line = PFreader.readLine();
        StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");
        String token = null;
        int aresr = Integer.valueOf(token = st.nextToken()).intValue();
        int bresr = Integer.valueOf(token = st.nextToken()).intValue();
        int cresr = Integer.valueOf(token = st.nextToken()).intValue();
        if (st.hasMoreTokens()) {
          token = st.nextToken();
        }

        if (aresr == ares && bresr == bres && cresr == cres) {
          line = PFreader.readLine();
          st = new StringTokenizer(line, " ,\t\r\n");
          int index = 0;
          for (int na = 0; na < cSlices1; na++)
            for (int nb = 0; nb < bSlices1; nb++)
              for (int ng = 0; ng < aSlices1; ng++) {
                if (!st.hasMoreTokens()) {
                  do {
                    line = PFreader.readLine();
                    st = new StringTokenizer(line, " ,\t\r\n");
                  } while (!st.hasMoreTokens());
                }
                map[index++] = Float.valueOf(st.nextToken()).floatValue();
              }
        } else {

          System.out.println("Resolution not corresponding!");

          int index = 0;
          for (int nc = 0; nc < cSlices1; nc++)
            for (int nb = 0; nb < bSlices1; nb++)
              for (int na = 0; na < aSlices1; na++)
                map[index++] = 1.0f;
        }
        PFreader.close();
      } catch (IOException io) {

        int index = 0;
        for (int nc = 0; nc < cSlices1; nc++)
          for (int nb = 0; nb < bSlices1; nb++)
            for (int na = 0; na < aSlices1; na++)
              map[index++] = 1.0f;

      }
    } else {
      int index = 0;
      for (int nc = 0; nc < cSlices1; nc++)
        for (int nb = 0; nb < bSlices1; nb++)
          for (int na = 0; na < aSlices1; na++)
            map[index++] = 1.0f;
    }

    return map;
  }

  public int MAPindex(int ia, int ib, int ic) {
    return getASlices() * getBSlices() * ic + getASlices() * ib + ia;
  }

  public void plotElectronMap() {
    if (atomMap == null)
      return;
//    if (sliceStart > nSlices)
    int numberOfSections = getCSlices();
    int is15 = (int) (Math.sqrt(numberOfSections) * 640.0 / 480.0);
    int columns = is15;
    int rows = 0;
    rows = numberOfSections / is15;  // + 1;
    if (numberOfSections < is15)
      columns = numberOfSections;
    if (rows * columns < numberOfSections)
      rows++;
    int width = columns * getASlices() + columns - 1;
    int height = rows * getBSlices() + rows - 1;
    double[][] mapToPlot = new double[width][height];
    width = 0;
    height = 0;
    int row = 0;
    int column = 0;
    double IntensityMin = 0.0f;
    double IntensityMax = 0.0f;
    for (int i = 0; i < numberOfSections; i++) {
      for (int n = 0; n < getASlices(); n++) {
        for (int m = 0; m < getBSlices(); m++) {
          mapToPlot[width + n][height + m] = atomMap[MAPindex(n, m, i)];
          if (IntensityMax < atomMap[MAPindex(n, m, i)])
            IntensityMax = atomMap[MAPindex(n, m, i)];
        }
      }
      column++;
      if (column >= is15) {
        row++;
        column = 0;
      }
      width = column * (getASlices() + 1);
      height = row * (getBSlices() + 1);
    }
    String title = "Electron density map for " + getParent().toXRDcatString();
    new ElectronMap2DPlot(new Frame(), mapToPlot, title,
        IntensityMin, IntensityMax, column, row);
  }

  public void plot3DElectronMap() {
    if (atomMap == null)
      resetMAP();
    int aSlices1 = getASlices();
    int bSlices1 = getBSlices();
    int cSlices1 = getCSlices();
    double[] map = new double[computeParameterNumber()];
    int index = 0;
    for (int na = 0; na < aSlices1; na++)
      for (int nb = 0; nb < bSlices1; nb++)
        for (int nc = 0; nc < cSlices1; nc++)
          map[index++] = atomMap[MAPindex(na, nb, nc)];
    String title = "Electron density slices map for " + getParent().toXRDcatString();
//    new ThreeDMapPlot(new Frame(), atomMap, title);
    myJFrame mapFrame = new myJFrame(null);
    mapFrame.getContentPane().setLayout(new BorderLayout(Constants.borderInside,
        Constants.borderInside));
    mapFrame.getContentPane().add(BorderLayout.CENTER, new Slices2DPlotPanel(mapFrame, map,
        title, aSlices1, bSlices1, cSlices1));
    mapFrame.pack();
    mapFrame.setVisible(true);
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
//    Phase aphase = (Phase) getParent();
//    aphase.computeReducedCellFactors();
//    for (int i = 0; i < 3; i++)
//      reducedCell[i] = aphase.reducedCellFactor[i];
    return new JSFSDPDOptionsD(parent, this);
  }

  public class JSFSDPDOptionsD extends JOptionsDialog {

    JTextField[] parsTF = null;
    JCheckBox computeFromMapCB = null;
    JTextField compositionTF = null;
    JTextField fileInsTF = null;
    JCheckBox usePhaseRecyclingCB = null;
//    JTextField initialSliceTF = null;

    public JSFSDPDOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel tfPanel = new JPanel();
      tfPanel.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.NORTH, tfPanel);

      tfPanel.add(new JLabel("Composition: "));
      compositionTF = new JTextField(30);
      tfPanel.add(compositionTF);
      compositionTF.setToolTipText("Set composition of the entire cell, e.g.: Cu8 O12 H24");

      String[] labels = {
          "Map divisions a  : ",
          "Map divisions b  : ",
          "Map divisions c  : "
      };

      int numberFields = labels.length;
      parsTF = new JTextField[numberFields];

      for (int i = 0; i < numberFields; i++) {
        tfPanel.add(new JLabel(labels[i]));
        parsTF[i] = new JTextField(Constants.FLOAT_FIELD);
        tfPanel.add(parsTF[i]);
      }

      tfPanel.add(new JLabel("Use instructions from file: "));
      fileInsTF = new JTextField(30);
      tfPanel.add(fileInsTF);
      fileInsTF.setToolTipText("Superflip preamble before Fhkl list is readed from file name (relative to the analysis folder)");

      usePhaseRecyclingCB = new JCheckBox("");
      tfPanel.add(usePhaseRecyclingCB);
      tfPanel.add(new JLabel("Use phase recycling"));
      usePhaseRecyclingCB.setToolTipText("If checked the phase information, if available, will be in input to Superflip");

      if (Constants.testing) {
        computeFromMapCB = new JCheckBox("");
        tfPanel.add(computeFromMapCB);
        tfPanel.add(new JLabel("Compute Fhkl from e-map"));
        computeFromMapCB.setToolTipText("If unchecked the Fhkl are taken form the superflip calculation");
      }

      tfPanel = new JPanel();
      tfPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      principalPanel.add(BorderLayout.CENTER, tfPanel);

      /* tfPanel.add(new JLabel("Initial c slice: "));
      initialSliceTF = new JTextField(Constants.FLOAT_FIELD);
      tfPanel.add(initialSliceTF);*/

      JButton jb = new JButton("Reset map");
      tfPanel.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          resetMAP();
        }
      });
      jb.setToolTipText("Press this to reset the electron density map");

      jb = new JButton("Shake map");
      tfPanel.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
          shakeMAP();
        }
      });
      jb.setToolTipText("Press this to shake randomly the electron density map");

      jb = new JButton("Plot map");
      tfPanel.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          plotElectronMap();
        }
      });
      jb.setToolTipText("Press this to plot the electron density map");

      jb = new JButton("Single slices map");
      tfPanel.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          plot3DElectronMap();
        }
      });
      jb.setToolTipText("Press this for an electron density map");

      setTitle("Superflip options panel");
      initParameters();
      setHelpFilename("superflip.txt");
      pack();
    }

    public void initParameters() {
      for (int i = 0; i < parsTF.length; i++)
        parsTF[i].setText(stringField[i]);
      if (Constants.testing)
        computeFromMapCB.setSelected(stringField[3].equalsIgnoreCase("true"));
      compositionTF.setText(getCellComposition());
      fileInsTF.setText(getInstructionsFile());
      usePhaseRecyclingCB.setSelected(usePhaseRecycling());
//      initialSliceTF.setText("0");
//      for (int i = parsTF.length - 3; i < parsTF.length; i++) {
//        parsTF[i].setText(Double.toXRDcatString(reducedCell[i - parsTF.length + 3]));
//        parsTF[i].setEditable(false);
//      }
    }

    public void retrieveParameters() {
      for (int i = 0; i < parsTF.length; i++)
        if (i == 0) {
//          String textnumber = parsTF[i].getText();
          setResolution(parsTF[i].getText(), parsTF[++i].getText(), parsTF[++i].getText());
        } else
          stringField[i] = parsTF[i].getText();
      if (Constants.testing)
        setComputeFromMap(computeFromMapCB.isSelected());
      setCellComposition(compositionTF.getText());
      setInstructionsFile(fileInsTF.getText());
      setPhaseRecycling(usePhaseRecyclingCB.isSelected());
//      for (int i = parsTF.length - 3; i < parsTF.length; i++)
//        reducedCell[i - parsTF.length + 3] = Double.parseDouble(parsTF[i].getText());
//      useAllSites = MaudPreferences.getBoolean("MEMmap.useAllSites", false);
//      if (reducedCell[0] != 1.0 || reducedCell[1] != 1.0 || reducedCell[2] != 1.0)
//        useAllSites = true;
//      useequivalentReflections = MaudPreferences.getBoolean("MEMmap.useMultipleReflections", false);
    }

/*    public void plotElectronMap() {
      int sliceStart = Integer.valueOf(initialSliceTF.getText()).intValue();
      SDPDFourierMapsMEM.this.plotElectronMap(sliceStart);
    }*/

  }

}
