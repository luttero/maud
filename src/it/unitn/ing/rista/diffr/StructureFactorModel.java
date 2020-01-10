/*
 * @(#)StructureFactorModel.java created 28/6/2001 Casalino
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr;

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.awt.Utility;
import it.unitn.ing.rista.io.cif.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.fortran.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;


/**
 *  The StructureFactorModel is a generic model for structure factor computation
 *
 *
 * @version $Revision: 1.10 $, $Date: 2006/07/20 13:39:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class StructureFactorModel extends XRDcat {

  public boolean output = MaudPreferences.getBoolean("structureFactors.output", false);

  public StructureFactorModel(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public StructureFactorModel(XRDcat aobj) {
    this(aobj, "Structure Factor model x");
  }

  public StructureFactorModel() {
  }

  public void computeStructureFactors(Sample asample, boolean structureSolution) {
//    prepareStructureFactorComputing(structureSolution);
    for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
      DataFileSet adataset = asample.getActiveDataSet(i);
      computeStructureFactors(asample, adataset);
    }
	  Phase phase = (Phase) getParent();
	  phase.refreshFhklcomp = false;
	  storeStructureFactors(asample);
    if (output)
      saveStructureFactors(null);
  }

	public void computeStructureFactors(Sample asample, DataFileSet adataset) {
	}

	public void prepareStructureFactorComputing(boolean structureSolution) {
  }

  public void storeStructureFactors(Sample asample) {
	  for (int i = 0; i < asample.activeDatasetsNumber(); i++)
	    asample.getActiveDataSet(i).storeComputedOverExperimentalStructureFactors();
  }

	public boolean solveStructure() {
		return true;
	}

	public boolean canSolveStructure() {
		return false;
	}

	public void saveStructureFactors(String filename) {
    FilePar aparFile = getFilePar();
    Phase aphase = (Phase) getParent();
    double Zelectron = aphase.getAtomMapNormalization();
    if (filename == null)
      return;
//      filename = aparFile.getDirectory() + aphase.toXRDcatString() + ".sf";


    PrintStream printStream = new PrintStream(Misc.getOutputStream(filename));

//    StructureFactorList[] sfl = getStructureFactorList();
    try {
	    Phase phase = (Phase) getParent();
      Sample asample = aparFile.getActiveSample();
      int datasetsnumber = asample.datasetsNumber();
      for (int i = 0; i < datasetsnumber; i++) {
        DataFileSet adataset = asample.getDataSet(i);

        String blockID = "dataset_" + adataset.toXRDcatString();
        CIFDataBlock.writeBlockDecl(printStream, blockID, this);
        printStream.print(Constants.lineSeparator);

        String[] loopIDs = new String[8];
        loopIDs[0] = CIFdictionary.refln_h;
        loopIDs[1] = CIFdictionary.refln_k;
        loopIDs[2] = CIFdictionary.refln_l;
        loopIDs[3] = CIFdictionary.refln_FsquaredMeas;
        loopIDs[4] = CIFdictionary.refln_FsquaredCalc;
        loopIDs[5] = CIFdictionary.refln_FsquaredEsd;
        loopIDs[6] = CIFdictionary.refln_dspacing;
        loopIDs[7] = CIFdictionary.refln_wavelength;

        CIFUtil.writeLoopDecl(printStream, loopIDs, this);

        double wave = adataset.getInstrument().getRadiationType().getMeanRadiationWavelength();
        String waveS = Fmt.format(wave);

	      double[][][] sf = adataset.getStructureFactors(phase);

        int hkln = sf[0].length;
        for (int j = 0; j < hkln; j++) {
	        Reflection refl = phase.getReflex(j);
          if (sf[1][j][0] >= 0.0) {
              printStream.print(refl.getH() + " " + refl.getK() + " " + refl.getL() + " " + Fmt.format(sf[0][j][0] / refl.multiplicity) +
                      " " + Fmt.format(sf[1][j][0] / refl.multiplicity) + " " + Fmt.format(sf[2][j][0]) +
                      " " + Fmt.format(refl.d_space) + " " + waveS);
            printStream.print(Constants.lineSeparator);
          }
        }
        printStream.print(Constants.lineSeparator);

        loopIDs = new String[5];
        loopIDs[0] = CIFdictionary.refln_h;
        loopIDs[1] = CIFdictionary.refln_k;
        loopIDs[2] = CIFdictionary.refln_l;
        loopIDs[3] = CIFdictionary.refln_FMeas;
        loopIDs[4] = CIFdictionary.refln_FEsd;

        CIFUtil.writeLoopDecl(printStream, loopIDs, this);

        for (int j = 0; j < hkln; j++) {
	        Reflection refl = phase.getReflex(j);
	        if (sf[1][j][0] >= 0.0) {
          printStream.print(refl.getH() + " " + refl.getK() + " " + refl.getL() + " " +
                  Fmt.format(Math.sqrt(sf[0][j][0] / refl.multiplicity)) + " " + Fmt.format(Math.sqrt(sf[2][j][0])));
          printStream.print(Constants.lineSeparator);
          }
        }
        printStream.print(Constants.lineSeparator);
        printStream.print(Constants.lineSeparator);

        // for Sir2004
        loopIDs = new String[5];
        loopIDs[0] = CIFdictionary.refln_h;
        loopIDs[1] = CIFdictionary.refln_k;
        loopIDs[2] = CIFdictionary.refln_l;
        loopIDs[3] = CIFdictionary.refln_FsquaredMeas;
        loopIDs[4] = CIFdictionary.refln_FsquaredEsd;

        CIFUtil.writeLoopDecl(printStream, loopIDs, this);

        try {
          Formatter hklFormatter = new Formatter("I4");
          Formatter FFormatter = new Formatter("F8.2");
          double maxsf = 0.0;
          double maxesd = 0.0;
          for (int j = 0; j < hkln; j++) {
            if (sf[1][j][0] >= 0.0) {
	            Reflection refl = phase.getReflex(j);
	            double expSF = sf[0][j][0] / refl.multiplicity;  // is the mean of the exp
            double esdSF = sf[2][j][0];
            if (esdSF == 0.0)
              esdSF = Math.sqrt(Math.sqrt(expSF));
            if (maxsf < expSF)
              maxsf = expSF;
            if (maxesd < esdSF)
              maxesd = esdSF;
            }
          }
          double ctrl = 1.0;
          while (maxsf > 99999.99 || maxesd > 99999.99) {
            ctrl *= 0.5;
            maxsf *= 0.5;
            maxesd *= 0.5;
          }
          for (int j = 0; j < hkln; j++) {
            if (sf[1][j][0] >= 0.0) {
	            Reflection refl = phase.getReflex(j);
	            double expSF = Math.sqrt(sf[0][j][0] / refl.multiplicity);  // is the mean of the exp
	            double esdSF = Math.sqrt(sf[2][j][0]);
            if (esdSF == 0.0)
              esdSF = Math.sqrt(expSF);
	            hklFormatter.write(refl.getH(), printStream);
            hklFormatter.write(refl.getK(), printStream);
            hklFormatter.write(refl.getL(), printStream);
            FFormatter.write(new Float(expSF * expSF * ctrl), printStream);
            FFormatter.write(new Float(esdSF * esdSF * ctrl), printStream);
            printStream.print(Constants.lineSeparator);
            }
          }
          hklFormatter.write(0, printStream);
          hklFormatter.write(0, printStream);
          hklFormatter.write(0, printStream);
          FFormatter.write(new Float(Zelectron), printStream);
          FFormatter.write(new Float(0.0), printStream);
          printStream.print(Constants.lineSeparator);
        } catch (InvalidFormatException e) {
          e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (OutputFormatException e) {
          e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }

        // for Shelx76
        loopIDs = new String[5];
        loopIDs[0] = CIFdictionary.refln_h;
        loopIDs[1] = CIFdictionary.refln_k;
        loopIDs[2] = CIFdictionary.refln_l;
        loopIDs[3] = CIFdictionary.refln_FMeas;
        loopIDs[4] = CIFdictionary.refln_FEsd;

        CIFUtil.writeLoopDecl(printStream, loopIDs, this);

        try {
          Formatter hklFormatter = new Formatter("I4");
          Formatter FFormatter = new Formatter("F8.2");
          for (int j = 0; j < hkln; j++) {
	          if (sf[1][j][0] >= 0.0) {
		          Reflection refl = phase.getReflex(j);
		          double expSF = Math.sqrt(sf[0][j][0] / refl.multiplicity);  // is the mean of the exp
            double esdSF = Math.sqrt(sf[2][j][0]);
            if (esdSF == 0.0)
              esdSF = Math.sqrt(expSF);
		          hklFormatter.write(refl.getH(), printStream);
            hklFormatter.write(refl.getK(), printStream);
            hklFormatter.write(refl.getL(), printStream);
            FFormatter.write(new Float(expSF), printStream);
            FFormatter.write(new Float(esdSF), printStream);
            printStream.print(Constants.lineSeparator);
            }
          }
          hklFormatter.write(0, printStream);
          hklFormatter.write(0, printStream);
          hklFormatter.write(0, printStream);
          FFormatter.write(new Float(Zelectron), printStream);
          FFormatter.write(new Float(0.0), printStream);
          printStream.print(Constants.lineSeparator);
        } catch (InvalidFormatException e) {
          e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (OutputFormatException e) {
          e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }

      }
    } catch (Exception io) {
      io.printStackTrace();
    }
    try {
      printStream.flush();
      printStream.close();
    } catch (Exception io) {
      io.printStackTrace();
    }
  }

  public StructureFactorList[] getStructureFactorList() {
    FilePar aparFile = getFilePar();
    Phase aphase = (Phase) getParent();
    Sample asample = aparFile.getActiveSample();
    int datasetsnumber = asample.activeDatasetsNumber();
    StructureFactorList[] structureFactorList = new StructureFactorList[datasetsnumber];
    for (int i = 0; i < datasetsnumber; i++) {
      DataFileSet adataset = asample.getActiveDataSet(i);
      RadiationType rad = adataset.getInstrument().getRadiationType();

      int hkln = aphase.gethklNumber();
      structureFactorList[i] = new StructureFactorList(hkln);
      structureFactorList[i].radiation = rad;
	    double[][][] structureFactors = adataset.getStructureFactors(aphase);
      for (int j = 0; j < hkln; j++) {
        Reflection refl = aphase.getReflectionVector().elementAt(j);
          int multiplicity = refl.multiplicity;
	      double expSF = structureFactors[0][j][0];
	      double SF = structureFactors[1][j][0];
	      double esdSF = structureFactors[2][j][0];
          if (expSF == Constants.STARTING_STRUCTURE_FACTOR * Constants.STARTING_STRUCTURE_FACTOR)
            expSF = SF;
          else
            expSF /= multiplicity;
          structureFactorList[i].structureFactor[j] = new StructureFactor(
                  refl.getH(), refl.getK(), refl.getL(), refl.d_space,
                  Math.sqrt(expSF), Math.sqrt(SF), Math.sqrt(esdSF),
                  refl.hlist, refl.klist, refl.llist);
	      boolean isInRange = false;
	      for (int k = 0; k < adataset.activedatafilesnumber() && !isInRange; k++)
		      if (adataset.getActiveDataFile(k).checkinRangeandIntensity(aphase, j))
			      isInRange = true;
        if (!(refl.isGoodforStructureFactor() && isInRange))
          structureFactorList[i].structureFactor[j].weight = 0.0;
      }
    }
    return structureFactorList;
  }
/*
  public void setStructureFactorList(StructureFactorList[] structureFactorList) {
    FilePar aparFile = getFilePar();
    Phase aphase = (Phase) getParent();
    Sample asample = aparFile.getActiveSample();
    int datasetsnumber = asample.activeDatasetsNumber();
    for (int i = 0; i < datasetsnumber; i++) {
      DataFileSet adataset = asample.getActiveDataSet(i);
      int hkln = aphase.gethklNumber();
       for (int j = 0; j < hkln; j++) {
        Reflection refl = aphase.reflectionv.elementAt(j);
          int multiplicity = refl.multiplicity;
          double SF = structureFactorList[i].structureFactor[j].Fhkl_calc;
//          System.out.println(refl.h + " " + refl.k + " " + refl.l + " " + refl);
          SF *= SF;
          refl.setStructureFactor(adataset.getIndex(), SF * multiplicity);
//          refl.setExpStructureFactor(adataset.getIndex(), SF * multiplicity);
      }
    }
  }
*/
/*  public void computeStructureFactor(int h, int k, int l, int multiplicity, double dspacing, int radType,
                                       int tubeNumber, int adatasetIndex, double factor, double[] structureFactor) {
//    int h = refl.h, k = refl.k, l = refl.l, int multiplicity = refl.multiplicity;
//    double dspacing = refl.d_space;

    return -1;
  }*/

  public void initializeAll() {
  }

  public int checkForDataSet(String filename, FilePar aparFile) {
    int result = 0;
    int numberOfDataSets = 0;
    Sample asample = aparFile.getActiveSample();
    BufferedReader PFreader = Misc.getReader(filename);
    boolean endoffile = false;
    String datasetEntry = CIFdictionary.dataDecl + "dataset_";
    int datasetLength = datasetEntry.length();
    if (PFreader != null) {
      try {
        String line = PFreader.readLine();
        while (!endoffile) {
          while (line != null && !line.startsWith(datasetEntry)) {
            line = PFreader.readLine();
          }
          if (line == null)
            endoffile = true;
          else {
// dataset entry
            numberOfDataSets++;
            String datasetName = line.substring(datasetLength);
            DataFileSet adataset = asample.getDataSetByName(datasetName);
            if (adataset != null)
              result++;
          }
          line = PFreader.readLine();
        } //endoffile
        PFreader.close();
      } catch (IOException io) {
      }
      try {
        PFreader.close();
      } catch (IOException io) {
      }
    }

    int realDataSetNumber = asample.datasetsNumber();

    if (realDataSetNumber == result)
      return result;
    else if (realDataSetNumber <= numberOfDataSets)
      return -numberOfDataSets;
    else
      return 0;
  }

  public boolean needStructureFactorExtractor() {
    return false;
  }

  public void refreshStructureFactor() {
    //To change body of created methods use File | Settings | File Templates.
  }

/*  public void setStructureFactor(Reflection refl, int adatasetIndex, double fhkl) {
    refl.setStructureFactor(adatasetIndex, fhkl);
  }*/

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JSFMOptionsD(parent, this);
    return adialog;
  }

  public class JSFMOptionsD extends JOptionsDialog {

    public JSFMOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      JButton jb = new JButton("Export Fhkl to File");
      principalPanel.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
//          retrieveParameters();
          exportStructureFactors();
        }
      });
      jb.setToolTipText("Press this to save structure factors in CIF format");

      setTitle("Structure Factor options panel");
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }

    public void exportStructureFactors() {
      String filename = Utility.browseFilenametoSave(this, "Save structure factors in CIF format");
      if (filename != null) {
        Phase aphase = (Phase) StructureFactorModel.this.getParent();
        aphase.gethklNumber(); // forcing to build a reflection list
        saveStructureFactors(filename);
      }
    }

  }
}
