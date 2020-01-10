/*
 * @(#)GiacovazzoSirIC.java created Jun 6, 2011 Caen
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
package it.unitn.ing.rista.diffr.sdpd;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.jsginfo.T_SgInfo;
import it.unitn.ing.fortran.Format;
import it.unitn.ing.fortran.Formatter;

import javax.swing.*;
import java.io.*;
import java.util.Vector;
import java.util.StringTokenizer;
import java.awt.*;

/**
 * The GiacovazzoSirIC is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 6, 2011 10:15:42 AM $
 * @since JDK1.1
 */
public class GiacovazzoSirIC extends StructureSolutionMethod {

  public static String[] diclistc = {
      "_rita_phase_cell_composition",
      "_sdpd_phase_method_solution"
  };
  public static String[] diclistcrm = {
      "_rita_phase_cell_composition",
      "_sdpd_phase_method_solution"
  };

  public static String[] classlistcs = {};
  public static String[] classlistc = {};

  public static String[] shelxKeywords = {"TITL", "CELL", "ZERR", "LATT", "SYMM", "SFAC", "UNIT", "L.S.", "WGHT",
      "FMAP", "PLAN", "FVAR", "HKLF", "END", "OMIT", "BOND", "AFIX", "HFIX", "DFIX", "SADI", "PART", "SIMU", "DELU",
      "ANIS", "SAME", "SPEC", "SUMP", "DANG", "ISOR", "FLAT", "EXYZ", "EADP", "EQUIV", "MORE", "PATT", "TREF",
      "ESEL", "TEXP", "CONF", "LIST", "BASF", "EXTI", "SWAT"};

  StructureFactorList[] structureFactorList = null;
  StructureFactor[] absentReflSF = null;
  int numberOfData = 0;
  boolean useAbsentReflections = true;

  boolean fitNotInitialized = true;
  public static String programName = "Sir2011";
  public Vector atomSites = null;

  public GiacovazzoSirIC(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Disabled Sir201x (Giacovazzo et al.)";
    IDlabel = "Sir201x (Giacovazzo et al.)";
    description = "select this to use Sir201x (Giacovazzo et al.) for structure solution and structure factor comp.";
  }

  public GiacovazzoSirIC(XRDcat aobj) {
    this(aobj, "Sir201x (Giacovazzo et al.)");
  }

  public GiacovazzoSirIC() {
    identifier = "Disabled Sir201x (Giacovazzo et al.)";
    IDlabel = "Sir201x (Giacovazzo et al.)";
    description = "select this to use Sir201x (Giacovazzo et al.) for structure solution and structure factor comp.";
  }

  public void initConstant() {
    Nstring = 2;
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
    programName = MaudPreferences.getPref("sir201x.executable_name", programName);

  }

  @Override
  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);
    Constants.STARTING_STRUCTURE_FACTOR = MaudPreferences.getDouble("structure_factors.default_value",
        Constants.STARTING_STRUCTURE_FACTOR);
  }

  @Override
  public String checkIntegrity() {
    programName = MaudPreferences.getPref("sir201x.executable_name", programName);
    String programNameString = Misc.getUserDir() + Constants.pluginsDir + programName;
    File fullProfFile = new File(programNameString);
    if (fullProfFile.exists())
      return null;
    return "Sir201x executable missing!\nGo to http://www.ic.cnr.it/\ndownload the Sir201x program and put sir201x[.exe] in the\nplugins directory of Maud, where the sir2k.jar file is.";
  }

  public String getCellComposition() {
    return stringField[0];
  }

  public void setCellComposition(String comp) {
    stringField[0] = comp;
  }

  public String getPhaseMethod() {
    return stringField[1];
  }

  public void setPhaseMethod(String comp) {
    stringField[1] = comp;
  }

  public boolean canSolveStructure() {
    return true;
  }

  public boolean solveStructure(StructureFactorList[] structureFactorList) {

    this.structureFactorList = structureFactorList;

    programName = MaudPreferences.getPref("sir201x.executable_name", programName);

    Phase aphase = (Phase) getParent();

    String insName = Misc.toStringDeleteBlank(aphase.getPhaseName());
//      saveDatafile(getFilePar().getDirectory() + insName + ".dat");
    initAll(aphase, getFilePar().getDirectory() + insName);

    // call superflip
    String sir2kProgram = Misc.getUserDir() + Constants.pluginsDir + programName;
    try {
      System.out.println("Executing: " + Misc.checkForWindowsPath(sir2kProgram) + " " + Misc.checkForWindowsPath(insName));
      Executable process = new Executable(Misc.checkForWindowsPath(sir2kProgram),
          Misc.checkForWindowsPath(getFilePar().getDirectory()), new String[]{Misc.checkForWindowsPath(insName), Misc.checkForWindowsPath(insName)});
      process.start();
      while (!process.getStatus().equals(Executable.TERMINATED))
        Thread.currentThread().sleep(100);
      System.out.println("Execution of Sir201x terminate with code: " + process.getTerminationResult());
      process.cleanUp();
//      Runtime.getRuntime().exec(superflipProgram + " " + filename);
      String hklFilename = getFilePar().getDirectory() + Misc.toStringDeleteBlank(aphase.getPhaseName()) + ".res";
      if (Constants.windoze) {
        hklFilename = Misc.getUserDir() + "\\" + Misc.toStringDeleteBlank(aphase.getPhaseName()) + ".res";
      }
      System.out.println("Sir201x results saved in " + hklFilename);
      atomSites = loadFhkl(aphase, hklFilename);
      System.out.println("Found " + atomSites.size() + " atoms/sites");
      for (int i = 0; i < atomSites.size(); i++) {
        SimpleSite site = (SimpleSite) atomSites.get(i);
        System.out.println("   " + site.siteLabel + " " + site.atomSymbol + " " + site.coord[0] + " " + site.coord[1]
          + " " + site.coord[2]);
      }
      fitNotInitialized = false;
    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }

    return true;
  }

  /*

%Structure crambin
%Data
Cell	40.763 18.492 22.333 90.00 90.61 90.00
SpaceGroup P 21
Content	C 406 H 776 N 110 O 131 S 12
Reflections crambin.hkl
%Phase
VLD
%End

  */


  void initAll(Phase aphase, String filename) {
    BufferedWriter output = null;
    if (filename != null) {
      try {
        output = Misc.getWriter(filename + ".sir");
        output.write("%nowindow");
        output.newLine();
        output.write("%Structure " + Misc.toStringDeleteBlank(aphase.getPhaseName()));
        output.newLine();
        output.write("%data");
        output.newLine();

//            Lambda1 = Lambda2 = (double) getFilePar().getSample(0).getDataSet(0).getInstrument().getRadiationType().
//                getMeanRadiationWavelength();
        T_SgInfo sgInfo = aphase.getPhaseInfo().SgInfo;
        String hallSymbol = sgInfo.getHallSymbolAsString();
        String spaceGroup = SpaceGroups.getSpaceGroupWithSpaces(aphase.getSpaceGroup(), 2);
        output.write("Cell ");
        for (int i = 0; i < 6; i++)
          output.write(" " + ((double) aphase.getFullCellValue(i)));
        output.newLine();
        output.write("SpaceGroup " + spaceGroup);
        output.newLine();
        output.write("Content " + getCellComposition());
        output.newLine();
        String insName = Misc.toStringDeleteBlank(aphase.getPhaseName());
        output.write("Format free");
        output.newLine();
        output.write("Reflections " + insName + ".hkl");
        output.newLine();
        if (getPhaseMethod().length() > 0) {
          output.write("%Phase");
          output.newLine();
          output.write(getPhaseMethod());
          output.newLine();
        }
//          output.write("Crystals");
//          output.newLine();
        output.write("%Continue");
        output.newLine();

      } catch (IOException io) {
      }
      try {
        output.flush();
        output.close();
      } catch (IOException io) {
      }
      try {
        output = Misc.getWriter(filename + ".hkl");
        for (StructureFactorList aStructureFactorList : structureFactorList) {
          int reflectionNumber = aStructureFactorList.structureFactor.length;
          for (int j = 0; j < reflectionNumber; j++) {
            StructureFactor sf = aStructureFactorList.structureFactor[j];
            sf.Fhkl_exp = sf.Fhkl_calc; // the calc is the mean value for all exp
            double value = (double) (sf.Fhkl_exp * sf.Fhkl_exp);
            double sigma2 = (double) (sf.Fhkl_esd * sf.Fhkl_esd);
//                Formatter fFormat = new Formatter("F9.5");
            if (sf.weight > 0) {
              if (value < 0.001)
                value = 0;
              output.write(sf.h + " " + sf.k + " " + sf.l + " " + value + " 0.01");
/*                  String out = new String();
                  PrintStream printS = new PrintStream(out);
                  fFormat.write(new Float(sigma2), printS);
                  output.write(out);*/
              output.newLine();
            }
          }
        }
      } catch (Exception io) {
        io.printStackTrace();
      }
      try {
        output.flush();
        output.close();
      } catch (IOException io) {
      }
    }
  }

  /*   public void computeStructureFactor(int h, int k, int l, int multiplicity, double dspacing, int radType,
                                       int tubeNumber, int adatasetIndex, double factor, double[] structureFactor) {
//    int h = refl.h, k = refl.k, l = refl.l, int multiplicity = refl.multiplicity;
//    double dspacing = refl.d_space;

    if (fitNotInitialized)
      return Constants.STARTING_STRUCTURE_FACTOR * Constants.STARTING_STRUCTURE_FACTOR;
    return factor;
  }*/

  public Vector loadFhkl(Phase aphase, String fhklFilename) {

/*
    This is the format of the shelx result file from Sir20xx.
*/
    String line = " ";
    BufferedReader reader = Misc.getReader(fhklFilename);
    Vector atoms = new Vector(10, 10);
    Vector quantities = new Vector(10, 10);
    Vector sites = new Vector(10, 10);
    StringTokenizer st;
    if (reader != null) {
      try {
        while (line != null) {
          line = reader.readLine();
          if (line != null) {
            int shelxCode = getShelxIndexForKeyword(line);
            switch (shelxCode) {
              case 5:
                st = new StringTokenizer(line, "() ,\t\r\n");
                st.nextToken();
                while (st.hasMoreTokens()) {
                  String value = st.nextToken();
                  atoms.add(value);
                }
                break;
              case 6:
                st = new StringTokenizer(line, "() ,\t\r\n");
                st.nextToken();
                while (st.hasMoreTokens()) {
                  String value = st.nextToken();
                  quantities.add(value);
                }
                break;
              case 13: // end
                line = null;
                break;
              case -99:
//                System.out.println(line);
                st = new StringTokenizer(line, "() ,\t\r\n");
                String siteLabel = st.nextToken();
                String atomSymbol = (String) atoms.get(Integer.parseInt(st.nextToken()) - 1);
                double[] xyz = new double[3];
                for (int i = 0; i < 3; i++)
                if (st.hasMoreTokens()) {
                  String value = st.nextToken();
                  xyz[i] = Double.parseDouble(value);
                }
                SimpleSite site = new SimpleSite(siteLabel, atomSymbol, xyz);
                site.computePositions(aphase);
                sites.add(site);
                break;
              default: {
              }
            }
          }
        }
/*        for (StructureFactorList aStructureFactorList : structureFactorList) {
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
            if (!found) {
              System.out.println("Not found: " + sf.h + " " + sf.k + " " + sf.l);
              if (sf1 != null)
                System.out.println("Assigned tentatively as: " + sf1.h + " " + sf1.k + " " + sf1.l);
            }
          }
        }*/
//        }
      } catch (Exception e) {
        System.out.println("Error in loading the Fhkl Sir201x file!");
        e.printStackTrace();
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
    }
    return sites;
  }

  public void computeStructureFactors(Sample asample, DataFileSet adataset) {
//    System.out.println("Computing factors 1");
    Phase aphase = (Phase) getParent();
    Instrument ainstrument = adataset.getInstrument();
    Radiation rad1 = ainstrument.getRadiationType().getRadiation(0);
    aphase.refreshFhklcomp = true;
//todo to fix    aphase.Fhklcompv(rad1.getRadiationIDNumber(), rad1.tubeNumber, adataset.getIndex(), false);
  }

/*  public void computeStructureFactor(int h, int k, int l, int multiplicity, double dspacing, int radType,
                                       int tubeNumber, int adatasetIndex, double factor, double[] structureFactor) {
//    int h = refl.h, k = refl.k, l = refl.l, int multiplicity = refl.multiplicity;
//    double dspacing = refl.d_space;
//    System.out.println("computing factors");
    return Fhklcomp(h, k, l, dspacing, radType, tubeNumber) * multiplicity;
  }

  public double Fhklcomp(int h, int k, int l, double dspacing, int radType, int tubeNumber) {
    if (atomSites == null)
      return -1;
//    System.out.println("computing factor for " + h + " " + k + " " + l);
    double a1 = 0.0;
    double a2 = 0.0;
    for (int j = 0; j < atomSites.size(); j++) {
      SimpleSite ato = (SimpleSite) atomSites.get(j);
      double[] scatf = ato.scatfactor(dspacing, radType, tubeNumber);
      double DWfactor = 1.0; //ato.DebyeWaller(h, k, l, dspacing);
      double scatFactor = DWfactor; // * ato.getQuantityD() / ato.getSiteMultiplicity();
      scatf[0] *= scatFactor;
      scatf[1] *= scatFactor;
      for (int ix = 0; ix < ato.getSiteMultiplicity(); ix++) {
        double[] x = ato.getCoordinates(ix);
        double arg = 2.0 * Constants.PI * (h * x[0] + k * x[1] + l * x[2]);
        double w1 = Math.cos(arg);
        double w2 = Math.sin(arg);
        a1 += scatf[0] * w1 - scatf[1] * w2;
        a2 += scatf[0] * w2 + scatf[1] * w1;
      }
    }
    return (a1 * a1 + a2 * a2);
  }*/



  public static int getShelxIndexForKeyword(String line) {
    for (int i = 0; i < shelxKeywords.length; i++)
      if (line.toUpperCase().startsWith(shelxKeywords[i]))
        return i;
    if (Misc.toStringDeleteBlankAndTab(line).length() > 1 && !(Misc.toStringDeleteBlankAndTab(line).
        toUpperCase().startsWith("!")))
      return -99;
    return -1;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    return new JSirOptionsD(parent, this);
  }

  public class JSirOptionsD extends JOptionsDialog {

    JTextField[] parsTF = null;
    JCheckBox computeFromMapCB = null;
    JTextField compositionTF = null;
    JTextField phaseTF = null;

    public JSirOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel tfPanel = new JPanel();
      tfPanel.setLayout(new GridLayout(0, 1, 3, 3));
      principalPanel.add(BorderLayout.NORTH, tfPanel);

      tfPanel.add(new JLabel("Sir201x Options: "));
      JPanel tfPanel1 = new JPanel();
      tfPanel1.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      tfPanel.add(tfPanel1);

      tfPanel1.add(new JLabel("Composition: "));
      compositionTF = new JTextField(30);
      tfPanel1.add(compositionTF);
      compositionTF.setToolTipText("Set composition of the entire cell, e.g.: Cu 8 O 12 H 24");

      tfPanel1 = new JPanel();
      tfPanel1.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      tfPanel.add(tfPanel1);
      tfPanel1.add(new JLabel("Phase method: "));
      phaseTF = new JTextField(30);
      tfPanel1.add(phaseTF);
      phaseTF.setToolTipText("Set the phase method (eg. VLD, leave blank for no specification)");

      setTitle("Sir201x model options panel");
      initParameters();
      setHelpFilename("Sir201x.txt");
      pack();
    }

    public void initParameters() {
      compositionTF.setText(getCellComposition());
      phaseTF.setText(getPhaseMethod());
    }

    public void retrieveParameters() {
      setCellComposition(compositionTF.getText());
      setPhaseMethod(phaseTF.getText());
    }

  }

}
