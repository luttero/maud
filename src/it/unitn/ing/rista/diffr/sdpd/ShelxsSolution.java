/*
 * @(#)ShelxsSolution.java created Feb 12, 2009 Caen
 *
 * Copyright (c) 2009 Luca Lutterotti All Rights Reserved.
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
import it.unitn.ing.rista.io.cif.CIFdictionary;
import it.unitn.ing.rista.io.cif.CIFtoken;
import it.unitn.ing.rista.awt.*;

import javax.swing.*;
import java.io.*;
import java.util.StringTokenizer;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * The ShelxsSolution is a class to run Shelxs program by Sheldrick
 * and import its solution back into Maud
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Feb 12, 2009 10:13:30 AM $
 * @since JDK1.1
 */
public class ShelxsSolution extends StructureSolutionMethod {

  public static String[] diclistc = {
      "_rita_atom_map_division_number_a",
      "_rita_atom_map_division_number_b",
      "_rita_atom_map_division_number_c"};
  public static String[] diclistcrm = {
      "_rita_atom_map_division_number_a",
      "_rita_atom_map_division_number_b",
      "_rita_atom_map_division_number_c"};

  public static String[] classlistcs = {};
  public static String[] classlistc = {};

  StructureFactorList[] structureFactorList = null;
  StructureFactor[] absentReflSF = null;
  int numberOfData = 0;
  boolean useAbsentReflections = true;

  private double atomMap[] = null;
  boolean mapnotLoaded = true;
  int aSlices = 0;
  int bSlices = 0;
  int cSlices = 0;
  double[] reducedCell = {1.0, 1.0, 1.0};
  int numberOfParameters = 0;
  boolean fitNotInitialized = true;

  public ShelxsSolution(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Disabled Shelxs (Sheldrick)";
    IDlabel = "Shelxs (Sheldrick)";
    description = "select this to obtain a structure solution by Shelxs (Sheldrick)";
  }

  public ShelxsSolution(XRDcat aobj) {
    this(aobj, "Shelxs (Sheldrick)");
  }

  public ShelxsSolution() {
    identifier = "Disabled Shelxs (Sheldrick)";
    IDlabel = "Shelxs (Sheldrick)";
    description = "select this to obtain a structure solution by Shelxs (Sheldrick)";
  }

  public void initConstant() {
    Nstring = 3;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

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
    mapnotLoaded = true;
  }

  public boolean canSolveStructure() {
    return true;
  }

  public boolean solveStructure(StructureFactorList[] structureFactorList) {

    this.structureFactorList = structureFactorList;

    Phase aphase = (Phase) getParent();

    String mapName = Misc.toStringDeleteBlank(aphase.getPhaseName());
    String filename = getFilePar().getDirectory() + mapName;
/*    if (mapnotLoaded) {
      loadMapFromFile(aphase);
    }*/
    initAll(aphase, filename);

    // call superflip
    String shelxsProgram = Misc.getUserDir() + Constants.pluginsDir + "shelxs";
    try {
      System.out.println("Executing: " + shelxsProgram + " " + filename);
      Executable process = new Executable(shelxsProgram, getFilePar().getDirectory(), new String[] {filename});
      process.start();
      while (!process.getStatus().equals(Executable.TERMINATED))
        Thread.currentThread().sleep(100);
      System.out.println("Execution of shelx terminate with code: " + process.getTerminationResult());
      process.cleanUp();
//      Runtime.getRuntime().exec(superflipProgram + " " + filename);
      String xplorFilename = getFilePar().getDirectory() + Misc.toStringDeleteBlank(aphase.getPhaseName()) + ".xplor";
      String m80Filename = getFilePar().getDirectory() + Misc.toStringDeleteBlank(aphase.getPhaseName()) + ".m80";
      System.out.println("Electron density saved in " + xplorFilename);
      loadXplorMap(xplorFilename);
      loadFhkl(aphase, m80Filename);
      fitNotInitialized = false;
      computeFhkl();
    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }

    mapnotLoaded = false;

    return true;
  }

  void initAll(Phase aphase, String filename) {
    BufferedWriter output = null;
    if (filename != null) {
        try {
          output = Misc.getWriter(filename);
          output.write("title phase " + aphase.getPhaseName());
          output.newLine();
          output.newLine();
          output.write("# Keywords influencing the form of the files");
          output.newLine();
          output.write("outputfile " + Misc.toStringDeleteBlank(aphase.getPhaseName()) + ".m80 "
                    + Misc.toStringDeleteBlank(aphase.getPhaseName()) + ".xplor");
          output.newLine();
          output.newLine();
          output.write("# Basic crystallographic information");
          output.newLine();
          output.newLine();
          if (getResolution_a() != null && Integer.valueOf(getResolution_a()) > 0) {
            output.write("voxel " + getResolution_a() + " " + getResolution_b() + " " + getResolution_c());
          }
          output.newLine();
          output.newLine();
          output.write("cell");
          for (int i = 0; i < 6; i++)
            output.write(" " + ((double) aphase.getFullCellValue(i)));
          output.newLine();
          output.write("#Space group " + aphase.getSpaceGroup());
          output.newLine();
          output.write("symmetry");
          output.newLine();
          for (int i = 0; i < aphase.getPhaseInfo().sitePositionv.size(); i++) {
            output.write(changexyz((aphase.getPhaseInfo().sitePositionv.elementAt(i)).getx()
                + " " + (aphase.getPhaseInfo().sitePositionv.elementAt(i)).gety()
                + " " + (aphase.getPhaseInfo().sitePositionv.elementAt(i)).getz()));
            output.newLine();
          }
          output.write("endsymmetry");
          output.newLine();
          output.newLine();
          output.newLine();
          output.write("# List of reflections");
          output.newLine();
          output.write("reflectionlist unique");
          output.newLine();
          output.write("dataformat amplitude");
          output.newLine();
          output.write("fbegin");
          output.newLine();
          for (StructureFactorList aStructureFactorList : structureFactorList) {
            int reflectionNumber = aStructureFactorList.structureFactor.length;
            for (int j = 0; j < reflectionNumber; j++) {
              StructureFactor sf = aStructureFactorList.structureFactor[j];
              sf.Fhkl_exp = sf.Fhkl_calc; // the calc is the mean value for all exp
              double value = (double) sf.Fhkl_exp;
              if (Math.abs(sf.Fhkl_exp) < 1.0E-3)
                value = 0.0f;
              if (sf.weight > 0) {
                output.write(sf.h + " " + sf.k + " " + sf.l + " " + value);
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

  public double computeStructureFactor(int h, int k, int l, int multiplicity, double dspacing, int radType,
                                       int tubeNumber, int adatasetIndex, double factor) {
//    int h = refl.h, k = refl.k, l = refl.l, int multiplicity = refl.multiplicity;
//    double dspacing = refl.d_space;

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
  }

  public double[] Fhklcomp(int h, int k, int l, double[] map,
                           int aSlices, int bSlices, int cSlices) {
    // compute Fhkl from electron map
    double[] a1 = new double[2];
    a1[0] = 0.0;
    a1[1] = 0.0;
    Phase aphase = (Phase) getParent();
    double factors = aphase.getActivePlanarDefects().getStructureFactorModifier(null);
    double[] divideFactors = aphase.getActivePlanarDefects().getDivisionFactors();
    int siteNumber = 1;
//    if (useAllSites)
      siteNumber = aphase.getPhaseInfo().getSitePositionNumber();
    double norm = Math.sqrt(factors) / (siteNumber * reducedCell[0] * reducedCell[1] * reducedCell[2]);
    SitePosition[] sitepos = new SitePosition[siteNumber];
    for (int i = 0; i < siteNumber; i++) {
      sitepos[i] = aphase.getPhaseInfo().sitePositionv.elementAt(i);
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

        setResolution(a_value, b_value, c_value);

        reader.readLine(); // cell parameters
        reader.readLine(); // ZYX

        int aSlices1 = getASlices();
        int bSlices1 = getBSlices();
        int cSlices1 = getCSlices();
        atomMap = new double[computeParameterNumber()];
        int index = 0;
        for (int ng = 0; ng < cSlices1; ng++) {
          reader.readLine(); // na increasing
          for (int nb = 0; nb < bSlices1; nb++)
            for (int na = 0; na < aSlices1; na++) {
              linedata = reader.readLine(); // map value
              st = new StringTokenizer(linedata, "() ,\t\r\n");
              atomMap[index++] = Float.valueOf(st.nextToken());
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

  public void loadFhkl(Phase aphase, String xplorFilename) {
    BufferedReader reader = Misc.getReader(xplorFilename);
    if (reader != null) {
      try {
        StringTokenizer st;
        String linedata;

        int aSlices1 = getASlices();
        int bSlices1 = getBSlices();
        int cSlices1 = getCSlices();
        reader.readLine(); // 0 0 0
        int index = 0;
        for (int ng = 0; ng < cSlices1; ng++) {
          reader.readLine(); // na increasing
          for (int nb = 0; nb < bSlices1; nb++)
            for (int na = 0; na < aSlices1; na++) {
              linedata = reader.readLine(); // map value
              st = new StringTokenizer(linedata, "() ,\t\r\n");
              atomMap[index++] = Float.valueOf(st.nextToken());
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
          totalMap += atomMap[index++];
    norm = aphase.getAtomMapNormalization() / totalMap * (reducedCell[0] * reducedCell[1] * reducedCell[2]);
    index = 0;
    for (int na = 0; na < cSlices; na++)
      for (int nb = 0; nb < bSlices; nb++)
        for (int ng = 0; ng < aSlices; ng++)
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

  public void loadMapFromFile(Phase aphase) {
    String filename = new String(getFilePar().getDirectory() +
        aphase.toXRDcatString() + ".map");
    atomMap = MAPinputStandard(filename,
        getResolutionD_a(), getResolutionD_b(), getResolutionD_c());

    atomMapNormalization();
    mapnotLoaded = false;

  }

  public void atomMapInitialization() {

    for (int i = 0; i < 3; i++)
      reducedCell[i] = 1; // todo, see how to get aphase.reducedCellFactor[i];

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
    for (int na = 0; na < cSlices1; na++)
      for (int nb = 0; nb < bSlices1; nb++)
        for (int ng = 0; ng < aSlices1; ng++)
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
      for (int na = 0; na < cSlices1; na++) {
        for (int nb = 0; nb < bSlices1; nb++) {
          for (int ng = 0; ng < aSlices1; ng++) {
            float value = (float) atomMap[index++];
            out.write(Float.toString(value) + " ");
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
        for (int na = 0; na < cSlices1; na++) {
          for (int nb = 0; nb < bSlices1; nb++) {
            for (int ng = 0; ng < aSlices1; ng++) {
              float value = (float) map[index++];
              PFwriter.write(Float.toString(value) + " ");
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
          for (int na = 0; na < cSlices1; na++)
            for (int nb = 0; nb < bSlices1; nb++)
              for (int ng = 0; ng < aSlices1; ng++)
                map[index++] = 1.0f;
        }
        PFreader.close();
      } catch (IOException io) {

        int index = 0;
        for (int na = 0; na < cSlices1; na++)
          for (int nb = 0; nb < bSlices1; nb++)
            for (int ng = 0; ng < aSlices1; ng++)
              map[index++] = 1.0f;

      }
    } else {
      int index = 0;
      for (int na = 0; na < cSlices1; na++)
        for (int nb = 0; nb < bSlices1; nb++)
          for (int ng = 0; ng < aSlices1; ng++)
            map[index++] = 1.0f;
    }

    return map;
  }

  public int MAPindex(int ia, int ib, int ig) {
    return getCSlices() * getBSlices() * ia + getCSlices() * ib + ig;
  }

  public void plotElectronMap() {
    if (atomMap == null)
      return;
//    if (sliceStart > nSlices)
    int numberOfSections = cSlices;
    int is15 = (int) (Math.sqrt(numberOfSections) * 640.0 / 480.0);
    int columns = is15;
    int rows = 0;
    rows = numberOfSections / is15;  // + 1;
    if (numberOfSections < is15)
      columns = numberOfSections;
    if (rows * columns < numberOfSections)
      rows++;
    int width = columns * aSlices + columns - 1;
    int height = rows * bSlices + rows - 1;
    double[][] mapToPlot = new double[width][height];
    width = 0;
    height = 0;
    int row = 0;
    int column = 0;
    double IntensityMin = 0.0f;
    double IntensityMax = 0.0f;
    for (int i = 0; i < numberOfSections; i++) {
      for (int n = 0; n < aSlices; n++) {
        for (int m = 0; m < bSlices; m++) {
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
      width = column * (aSlices + 1);
      height = row * (bSlices + 1);
    }
    double xMax = (double) reducedCell[0] * columns;
    double yMax = (double) reducedCell[1] * rows;
    String title = "Electron density map for " + getParent().toXRDcatString();
    new ElectronMap2DPlot(new Frame(), mapToPlot, title,
            IntensityMin, IntensityMax, xMax, yMax);
  }

  public void plot3DElectronMap() {
    if (atomMap == null)
      resetMAP();
    int aSlices1 = getASlices();
    int bSlices1 = getBSlices();
    int cSlices1 = getCSlices();
    String title = "Electron density slices map for " + getParent().toXRDcatString();
//    new ThreeDMapPlot(new Frame(), atomMap, title);
    myJFrame mapFrame = new myJFrame(null);
    mapFrame.getContentPane().setLayout(new BorderLayout(Constants.borderInside,
        Constants.borderInside));
    mapFrame.getContentPane().add(BorderLayout.CENTER, new Slices2DPlotPanel(mapFrame, atomMap,
        title, aSlices1, bSlices1, cSlices1));
    mapFrame.pack();
    mapFrame.setVisible(true);
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
//    Phase aphase = (Phase) getParent();
//    aphase.computeReducedCellFactors();
//    for (int i = 0; i < 3; i++)
//      reducedCell[i] = aphase.reducedCellFactor[i];
    JOptionsDialog adialog = new JSFSDPDOptionsD(parent, this);
    return adialog;
  }

  public class JSFSDPDOptionsD extends JOptionsDialog {

    JTextField[] parsTF = null;
//    JTextField initialSliceTF = null;

    public JSFSDPDOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel tfPanel = new JPanel();
      tfPanel.setLayout(new GridLayout(0, 2, 3, 3));
      principalPanel.add(BorderLayout.NORTH, tfPanel);

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
//        genTF.setToolTipText("Set the number of generations for the Genetic Algorithm");
        tfPanel.add(parsTF[i]);
      }

      String[] rlabels = {
          "reduced cell a (factor): ", Double.toString(reducedCell[0]),
          "reduced cell b (factor): ", Double.toString(reducedCell[1]),
          "reduced cell c (factor): ", Double.toString(reducedCell[2])
      };
      for (int i = 0; i < rlabels.length; i++)
        tfPanel.add(new JLabel(rlabels[i]));

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
      pack();
    }

    public void initParameters() {
      for (int i = 0; i < parsTF.length; i++)
        parsTF[i].setText(stringField[i]);
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
//      for (int i = parsTF.length - 3; i < parsTF.length; i++)
//        reducedCell[i - parsTF.length + 3] = Double.parseDouble(parsTF[i].getText());
//      useAllSites = MaudPreferences.getBoolean("meemMap.useAllSites", false);
//      if (reducedCell[0] != 1.0 || reducedCell[1] != 1.0 || reducedCell[2] != 1.0)
//        useAllSites = true;
//      useequivalentReflections = MaudPreferences.getBoolean("meemMap.useMultipleReflections", false);
    }

/*    public void plotElectronMap() {
      int sliceStart = Integer.valueOf(initialSliceTF.getText()).intValue();
      SDPDFourierMapsMEM.this.plotElectronMap(sliceStart);
    }*/

  }
}
