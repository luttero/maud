/*
 * @(#)ABINITEnergyComputation.java created Dec 20, 2009 Caen
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
package it.unitn.ing.rista.diffr.forcefield;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.JOptionsDialog;

import javax.swing.*;
import java.io.*;
import java.util.StringTokenizer;
import java.util.Vector;
import java.awt.*;

/**
 * The ABINITEnergyComputation is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Dec 20, 2009 5:33:38 PM $
 * @since JDK1.1
 */
public class ABINITEnergyComputation extends ForceField {

  public double ecut = 3.0f;
  public int kptopt = 1;
  public int nkpt = 1;
  public int[] ngkpt = {1, 1, 1};
  public double[] shiftk = {0.5f, 0.5f, 0.5f};
  public double toldff = 5.0E-6;
  public int chkprim = 0;

  public static String[] pspFiles = {
  "1h.1.hgh",
  "2he.2.hgh",
  "3li.1.hgh",
//  "3li.3.hgh",
  "4be.2.hgh",
//  "4be.4.hgh",
  "5b.3.hgh",
  "6c.4.hgh",
  "7n.5.hgh",
  "8o.6.hgh",
  "9f.7.hgh",
  "10ne.8.hgh",
  "11na.1.hgh",
//  "11na.9.hgh",
//  "12mg.10.hgh",
  "12mg.2.hgh",
  "13al.3.hgh",
  "14si.4.hgh",
  "15p.5.hgh",
  "16s.6.hgh",
  "17cl.7.hgh",
  "18ar.8.hgh",
  "19k.1.hgh",
//  "19k.9.hgh",
//  "20ca.10.hgh",
  "20ca.2.hgh",
//  "21sc.11.hgh",
  "21sc.3.hgh",
//  "22ti.12.hgh",
  "22ti.4.hgh",
//  "23v.13.hgh",
  "23v.5.hgh",
//  "24cr.14.hgh",
  "24cr.6.hgh",
//  "25mn.15.hgh",
  "25mn.7.hgh",
//  "26fe.16.hgh",
  "26fe.8.hgh",
//  "27co.17.hgh",
  "27co.9.hgh",
  "28ni.10.hgh",
//  "28ni.18.hgh",
  "29cu.1.hgh",
//  "29cu.11.hgh",
//  "30zn.12.hgh",
  "30zn.2.hgh",
//  "31ga.13.hgh",
  "31ga.3.hgh",
  "32ge.4.hgh",
  "33as.5.hgh",
  "34se.6.hgh",
  "35br.7.hgh",
  "36kr.8.hgh",
  "37rb.1.hgh",
//  "37rb.9.hgh",
//  "38sr.10.hgh",
  "38sr.2.hgh",
//  "39y.11.hgh",
  "39y.3.hgh",
//  "40zr.12.hgh",
  "40zr.4.hgh",
//  "41nb.13.hgh",
  "41nb.5.hgh",
//  "42mo.14.hgh",
  "42mo.6.hgh",
//  "43tc.15.hgh",
  "43tc.7.hgh",
//  "44ru.16.hgh",
  "44ru.8.hgh",
//  "45rh.17.hgh",
  "45rh.9.hgh",
  "46pd.10.hgh",
//  "46pd.18.hgh",
  "47ag.1.hgh",
//  "47ag.11.hgh",
//  "48cd.12.hgh",
  "48cd.2.hgh",
//  "49in.13.hgh",
  "49in.3.hgh",
  "50sn.4.hgh",
  "51sb.5.hgh",
  "52te.6.hgh",
  "53i.7.hgh",
  "54xe.8.hgh",
  "55cs.1.hgh",
//  "55cs.9.hgh",
//  "56ba.10.hgh",
  "56ba.2.hgh",
  "57la.11.hgh",
  "58ce.12.hgh",
  "59pr.13.hgh",
  "60nd.14.hgh",
  "61pm.15.hgh",
  "62sm.16.hgh",
  "63eu.17.hgh",
  "64gd.18.hgh",
  "65tb.19.hgh",
  "66dy.20.hgh",
  "67ho.21.hgh",
  "68er.22.hgh",
  "69tm.23.hgh",
  "70yb.24.hgh",
  "71lu.25.hgh",
  "72hf.12.hgh",
//  "73ta.13.hgh",
  "73ta.5.hgh",
//  "74w.14.hgh",
  "74w.6.hgh",
//  "75re.15.hgh",
  "75re.7.hgh",
//  "76os.16.hgh",
  "76os.8.hgh",
//  "77ir.17.hgh",
  "77ir.9.hgh",
  "78pt.10.hgh",
//  "78pt.18.hgh",
  "79au.1.hgh",
//  "79au.11.hgh",
//  "80hg.12.hgh",
  "80hg.2.hgh",
//  "81tl.13.hgh",
  "81tl.3.hgh",
  "82pb.4.hgh",
  "83bi.5.hgh",
  "84po.6.hgh",
  "85at.7.hgh",
  "86rn.8.hgh"};


  protected static String[] diclistc = {"_abinit_energy_background_level", "_abinit_energy_multiplier",
		  "_abinit_energy_cell_a_repetition", "_abinit_energy_cell_b_repetition", "_abinit_energy_cell_c_repetition"};
  protected static String[] diclistcrm = {"_abinit_energy_background_level", "_abinit_energy_multiplier",
		  "_abinit_energy_cell_a_repetition", "_abinit_energy_cell_b_repetition", "_abinit_energy_cell_c_repetition"};

  protected static String[] classlistc = {};

  protected static String[] classlistcs = {};

  public static final String idString = "DFT energy computation";

  public ABINITEnergyComputation(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = idString;
    IDlabel = idString;
    description = idString;
  }

  public ABINITEnergyComputation(XRDcat aobj) {
    this(aobj, idString);
  }

  public ABINITEnergyComputation() {
    identifier = idString;
    IDlabel = idString;
    description = idString;
  }

  public void initConstant() {
    Nstring = 5;
    Nstringloop = 0;
    Nparameter = 0;
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
    setString(0, "0");
    setString(1, "1");
	  setString(2, "2");
	  setString(3, "2");
	  setString(4, "2");
  }

  public double computeEnergy() {
    double PEnergy = 0.0;
    Phase aphase = (Phase) getParent().getParent();

    String phasePrefix = Misc.toStringDeleteBlank(aphase.getPhaseName());
    String mapName = phasePrefix + ".files";
    String filename = getFilePar().getDirectory() + mapName;
    initAll(aphase, phasePrefix, filename);

	  String abinit = MaudPreferences.getPref("abinit.executableName", "abinit");
	  String abinitFolder = MaudPreferences.getPref("abinit.path", "");
    if (Constants.windoze && !abinit.contains(".exe")) {
	    abinit += ".exe";
    }

    String superflipProgram = "";
		if (abinitFolder.equalsIgnoreCase(""))
	    superflipProgram = Misc.getUserDir() + Constants.pluginsDir + abinit + " < " + mapName + " >& log";
	  else
			superflipProgram = abinitFolder + Constants.fileSeparator + abinit + " < " + mapName + " >& log";
    long time = System.currentTimeMillis();
    try {
	    String resultsFilename = getFilePar().getDirectory() + phasePrefix + ".out";
//	    System.out.println("Search for file: " + resultsFilename);
      File resultsFile = new File(resultsFilename);
	    if (resultsFile.exists())
        resultsFile.delete();
      System.out.println("Executing: " + superflipProgram);
      Executable process = new Executable(superflipProgram, getFilePar().getDirectory(), null);
      process.start();
      while (!process.getStatus().equals(Executable.TERMINATED))
        Thread.currentThread().sleep(100);
      long time1 = System.currentTimeMillis();
      System.out.println("Execution of ABINIT terminate with code: " + process.getTerminationResult()
		      + " with a total time of " + (time1 - time) + " millisecs.");
      process.cleanUp();
//      Runtime.getRuntime().exec(superflipProgram + " " + filename);
      PEnergy = loadResults(resultsFilename);
      System.out.println("Energy calculated by abinit for phase " + getParentStructure().getPhaseParent().getPhaseName()
		      + " was: " + PEnergy);
    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
    return PEnergy; //Math.log(PEnergy + 1);
  }

  void initAll(Phase aphase, String prefix, String filename) {

	  int[] repetitionCell = new int[3];
	  for (int i = 0; i < 3; i++)
		  repetitionCell[i] = Integer.valueOf(getString(2 + i));


    String dirForpspgth = Misc.getUserDir() + Constants.pluginsDir + "HGH/";

    BufferedWriter output = null;
    Vector atoms = aphase.getFullAtomList();
	  int[] atomTypes = getUniqueAtomTypes(atoms);
    if (filename != null) {
        try {
          output = Misc.getWriter(filename);
          output.write(prefix + ".in");
          output.newLine();
          output.write(prefix + ".out");
          output.newLine();
          output.write(prefix + "xi");
          output.newLine();
          output.write(prefix + "xo");
          output.newLine();
          output.write(prefix + "x");
          output.newLine();
          for (int i = 0; i < atomTypes.length; i++) {
            output.write(dirForpspgth + pspFiles[atomTypes[i] - 1]);
            output.newLine();
          }
        } catch (IOException io) {
        }
        try {
          output.flush();
          output.close();
        } catch (IOException io) {
        }
    }
    String inFile = getFilePar().getDirectory() + prefix + ".in";
    if (inFile != null) {
        try {
          output = Misc.getWriter(inFile);
          output.write("natrd " + atoms.size());
          output.newLine();
          output.write("chkprim " + chkprim);
          output.newLine();
          int[] spg = SpaceGroups.getSGnumberAndExt(aphase.getSpaceGroup(), Phase.getNumber(aphase.getSymmetry()));
          output.write("spgroup " + spg[0]);
          output.newLine();
          output.write("spgaxor " + spg[1]);
          output.newLine();
          output.write("spgorig " + spg[2]);
          output.newLine();
          output.write("acell");
          for (int i = 0; i < 3; i++)
            output.write(" " + (aphase.getCellValue(i) * 2));
          output.newLine();
          output.write("angdeg");
          for (int i = 3; i < 6; i++)
            output.write(" " + aphase.getCellValue(i));
          output.newLine();
          output.write("xred");
          int natom = 0;
          for (int i = 0; i < atoms.size(); i++) {
            AtomSite atom = (AtomSite) atoms.elementAt(i);
            Coordinates coord = atom.getCoordinates();
            output.write("   " + coord.x + " " + coord.y + " " + coord.z);
            output.newLine();
            natom += atom.getSiteMultiplicity();
          }
          output.write("natom " + natom);
          output.newLine();
          output.write("ecut " + ecut);
          output.newLine();
          output.write("ntypat " + atomTypes.length);
          output.newLine();
          output.write("typat");
          for (int i = 0; i < atoms.size(); i++) {
	          for (int j = 0; j < atomTypes.length; j++)
		          if (((AtomSite) atoms.elementAt(i)).getAtomScatterer(0).getAtomicNumber() == atomTypes[j])
                output.write(" " + (j + 1));
          }
          output.newLine();
          output.newLine();
	        if (repetitionCell[0] == 2) {
          output.write("kptopt " + kptopt);
          output.newLine();
          output.write("nkpt " + nkpt);
          output.newLine();
          output.write("ngkpt");
          for (int i = 0; i < ngkpt.length; i++) {
            output.write(" " + ngkpt[i]);
          }
          output.newLine();
          output.write("shiftk");
          for (int i = 0; i < shiftk.length; i++) {
            output.write(" " + shiftk[i]);
          }
          output.newLine();
          output.newLine();
	        }
          output.write("toldff " + toldff);
          output.newLine();
          output.write("znucl");
          for (int i = 0; i < atomTypes.length; i++) {
            output.write(" " + atomTypes[i]);
          }
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

	private int[] getUniqueAtomTypes(Vector atoms) {
		int[] atomTypesNumber = new int[atoms.size()];
		int totalRef = 0;
		if (atoms.size() == 0)
			return atomTypesNumber;
		atomTypesNumber[0] = ((AtomSite) atoms.elementAt(0)).getAtomScatterer(0).getAtomicNumber();
		totalRef++;
		for (int i = 1; i < atoms.size(); i++) {
			int atomNumber = ((AtomSite) atoms.elementAt(i)).getAtomScatterer(0).getAtomicNumber();
			boolean alreadyIn = false;
			for (int j = 0; j < totalRef; j++) {
				if (alreadyIn = (atomNumber == atomTypesNumber[j]))
					break;
			}
			if (!alreadyIn)
				atomTypesNumber[totalRef++] = atomNumber;
		}
		int[] newRef = new int[totalRef];
		for (int j = 0; j < totalRef; j++)
			newRef[j] = atomTypesNumber[j];
		return newRef;
	}

	public double loadResults(String resultsFilename) {
    double eTotal = 0.0;
    BufferedReader reader = Misc.getReader(resultsFilename);
    if (reader != null) {
      try {
        StringTokenizer st;
        String linedata;

        linedata = reader.readLine();
	      linedata = Misc.removeUTF8BOM(linedata);
	      while (linedata != null && !linedata.contains("Etotal")) {
          linedata = reader.readLine();
        }
        if (linedata != null) {
          st = new StringTokenizer(linedata, "()>=: ,\t\r\n");
          st.nextToken();
          eTotal = Double.parseDouble(st.nextToken());
//          System.out.println("Total energy from Abinit: " + eTotal);
        }
      } catch (IOException e) {
        System.out.println("Error in loading the ABINIT results file!");
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
    }
    double energyBaseLevel = Double.parseDouble(getString(0));
    double energyMultiplier = Double.parseDouble(getString(1));
    return (eTotal - energyBaseLevel) * energyMultiplier;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    return new JAbinitOptionsD(parent, this);
  }

  public class JAbinitOptionsD extends JOptionsDialog {

    JTextField baseLevelTF;
    JTextField multiplierTF;
	  JTextField[] repetitonCellTF;

    public JAbinitOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);


      principalPanel.setLayout(new GridLayout(0, 1, 6, 6));
      JPanel jPanel8 = new JPanel();
      jPanel8.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      principalPanel.add(jPanel8);
      jPanel8.add(new JLabel("Energy base level: "));
      baseLevelTF = new JTextField(12);
      jPanel8.add(baseLevelTF);
      jPanel8 = new JPanel();
      jPanel8.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      principalPanel.add(jPanel8);
      jPanel8.add(new JLabel("Energy multiplier: "));
      multiplierTF = new JTextField(12);
      jPanel8.add(multiplierTF);

	    jPanel8 = new JPanel();
	    jPanel8.setLayout(new GridLayout(0, 2, 3, 3));
	    principalPanel.add(jPanel8);
	    repetitonCellTF = new JTextField[3];
	    jPanel8.add(new JLabel("Repetition cell along a: "));
	    repetitonCellTF[0] = new JTextField(12);
	    jPanel8.add(repetitonCellTF[0]);
	    jPanel8.add(new JLabel("Repetition cell along b: "));
	    repetitonCellTF[1] = new JTextField(12);
	    jPanel8.add(repetitonCellTF[1]);
	    jPanel8.add(new JLabel("Repetition cell along c: "));
	    repetitonCellTF[2] = new JTextField(12);
	    jPanel8.add(repetitonCellTF[2]);

      setTitle("ABINIT options");
      initParameters();
      setHelpFilename("abinit.txt");
      pack();
    }

    public void initParameters() {
      baseLevelTF.setText(getString(0));
      multiplierTF.setText(getString(1));
	    for (int i = 0; i < 3; i++)
		    repetitonCellTF[i].setText(getString(2 + i));
    }

    public void retrieveParameters() {
      setString(0, baseLevelTF.getText());
      setString(1, multiplierTF.getText());
	    for (int i = 0; i < 3; i++)
		    setString(2 + i, repetitonCellTF[i].getText());
    }

  }
}
