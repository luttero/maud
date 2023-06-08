/*
 * @(#)StructureFactorArbitraryModel.java created 28/06/2001 Casalino
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

package it.unitn.ing.rista.diffr.sfm;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;

import java.io.*;
import java.awt.*;
import java.util.*;
import java.awt.event.*;
import javax.swing.*;

import it.unitn.ing.rista.interfaces.Peak;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.io.cif.*;

/**
 * The StructureFactorArbitraryModel is a class that implements an arbitrary correction of the
 * structure factors based only on the extracted values that are used without any
 * manipolation.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.11 $, $Date: 2006/07/20 13:39:06 $
 * @since JDK1.1
 */


public class StructureFactorArbitraryModel extends StructureFactorModel {

  public StructureFactorArbitraryModel(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "extracted sf model";
    IDlabel = "extracted sf model";
    description = "select this to use extracted or experimental structure factors";
  }

  public StructureFactorArbitraryModel(XRDcat aobj) {
    this(aobj, "extracted sf model");
  }

  public StructureFactorArbitraryModel() {
    identifier = "extracted sf model";
    IDlabel = "extracted sf model";
    description = "select this to use extracted or experimental structure factors";
  }

	public void storeStructureFactors(Sample asample) {
		for (int i = 0; i < asample.activeDatasetsNumber(); i++)
			asample.getActiveDataSet(i).storeExperimentalOverComputedStructureFactors();
	}

	public boolean canSolveStructure() {
		return true;
	}

	public boolean needStructureFactorExtractor() {
		return true;
	}

  public void loadStructureFactors(String filename) {

	  // todo fix this
//    notLoaded = false;
    FilePar aparFile = getFilePar();
    Phase aphase = (Phase) getParent();
    int hkln = aphase.gethklNumber();
    Sample asample = aparFile.getActiveSample();
    int realDataSetNumber = asample.datasetsNumber();
//    initializeMatrices(hkln, realDataSetNumber);
    if (filename == null)
      filename = aparFile.getDirectory() + aphase.toXRDcatString() + ".sf";
    int hasValidDataSet = checkForDataSet(filename, aparFile);
    BufferedReader PFreader = Misc.getReader(filename);
    boolean endoffile = false;
    String datasetEntry = CIFdictionary.dataDecl + "dataset_";
    int datasetLength = datasetEntry.length();
    if (PFreader != null) {
      try {
        String line = PFreader.readLine();
        while (!endoffile) {
          if (hasValidDataSet != 0) {
            while (line != null && !line.startsWith(datasetEntry)) {
              line = PFreader.readLine();
            }
          }
          if (line == null)
            endoffile = true;
          else {
// dataset entry
            int datasetindex = -1;

            if (hasValidDataSet > 0) {
              String datasetName = line.substring(datasetLength);
              DataFileSet adataset = asample.getDataSetByName(datasetName);
//              if (adataset != null)
//                datasetindex = adataset.getIndex();
	            datasetindex++;
            } else {
              datasetindex++;
            }
//                System.out.println("dataset " + adataset.toXRDcatString());
            CIFloop aloop = new CIFloop(PFreader);
            aloop.lookForAndReadLoop();
            int cifEntriesNumber = aloop.getNumberOfCIFentries();

            int maxEntries = 10;
            int[] FhklCIFindex = new int[maxEntries];
            for (int i = 0; i < maxEntries; i++)
              FhklCIFindex[i] = -1;
            String[] loopIDs = new String[maxEntries];
            loopIDs[0] = CIFdictionary.refln_h;
            loopIDs[1] = CIFdictionary.refln_k;
            loopIDs[2] = CIFdictionary.refln_l;
            loopIDs[3] = CIFdictionary.refln_multiplicity;
            loopIDs[4] = CIFdictionary.refln_FsquaredMeas;
            loopIDs[5] = CIFdictionary.refln_FsquaredCalc;
            loopIDs[6] = CIFdictionary.refln_FsquaredEsd;
//          loopIDs[6] = CIFdictionary.refln_dspacing;
            loopIDs[7] = CIFdictionary.refln_wavelength;
            loopIDs[8] = CIFdictionary.refln_FMeas;
            loopIDs[9] = CIFdictionary.refln_FCalc;

//              System.out.println(cifEntriesNumber);
            for (int i = 0; i < cifEntriesNumber; i++) {
              String CIFentry = aloop.getCIFentry(i);
//              System.out.println(CIFentry);
              for (int j = 0; j < maxEntries; j++)
                if (CIFentry.equalsIgnoreCase(loopIDs[j])) {
                  FhklCIFindex[j] = i;
//                  System.out.println(j + " " + i);
                }
            }
            boolean meas = (FhklCIFindex[4] != -1);
            boolean calc = (FhklCIFindex[5] != -1);
            boolean meas_notsquared = (FhklCIFindex[8] != -1);
            boolean calc_notsquared = (FhklCIFindex[9] != -1);
            if (datasetindex < realDataSetNumber &&
                ((FhklCIFindex[0] != -1 && FhklCIFindex[1] != -1 && FhklCIFindex[2] != -1) &&
                    (meas || calc || meas_notsquared || calc_notsquared))) { // sufficient information

              int elementsNumber = aloop.getNumberOfCIFelements();

/*              if (FhklCIFindex[7] != -1 && elementsNumber > 0) {
                if (hasValidDataSet == 0)
                  for (int ij = 0; ij < realDataSetNumber; ij++)
                    wave[ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[7], 0));
                else
                  wave[datasetindex] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[7], 0));
              } else {
                if (hasValidDataSet == 0)
                  for (int ij = 0; ij < realDataSetNumber; ij++)
                    wave[ij] = asample.getDataSet(ij).getMeanRadiationWavelength();
                else
                  wave[datasetindex] = asample.getDataSet(datasetindex).getMeanRadiationWavelength();
              }*/

	            int[][] hklm = new int[3][elementsNumber];
	            double[][] Fhkl = new double[3][elementsNumber];

// set structure factors
              for (int i = 0; i < elementsNumber; i++) {
                hklm[0][i] = Integer.valueOf(aloop.getCIFelement(FhklCIFindex[0], i));
                hklm[1][i] = Integer.valueOf(aloop.getCIFelement(FhklCIFindex[1], i));
                hklm[2][i] = Integer.valueOf(aloop.getCIFelement(FhklCIFindex[2], i));
                if (meas) {
//                  if (hasValidDataSet == 0)
//                    for (int ij = 0; ij < realDataSetNumber; ij++)
//                    Fhkl[0][i]/*[ij]*/ = Double.valueOf(aloop.getCIFelement(FhklCIFindex[4], i));
//                  else
                    Fhkl[0][i]/*[datasetindex]*/ = Double.valueOf(aloop.getCIFelement(FhklCIFindex[4], i));
                  if (!calc) {
                    if (hasValidDataSet == 0)
//                      for (int ij = 0; ij < realDataSetNumber; ij++)
//                        Fhkl[1][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[4], i));
//                    else
                      Fhkl[1][i]/*[datasetindex]*/ = Double.valueOf(aloop.getCIFelement(FhklCIFindex[4], i));
                  }
                }
                if (calc) {
//                  if (hasValidDataSet == 0)
//                    for (int ij = 0; ij < realDataSetNumber; ij++)
//                      Fhkl[1][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[5], i));
//                  else
                    Fhkl[1][i]/*[datasetindex]*/ = Double.valueOf(aloop.getCIFelement(FhklCIFindex[5], i));
                  if (!meas) {
//                    if (hasValidDataSet == 0)
//                      for (int ij = 0; ij < realDataSetNumber; ij++)
//                        Fhkl[0][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[5], i));
//                    else
                      Fhkl[0][i]/*[datasetindex]*/ = Double.valueOf(aloop.getCIFelement(FhklCIFindex[5], i));
                  }
                }
                if (meas_notsquared) {
/*                  if (hasValidDataSet == 0) {
                    for (int ij = 0; ij < realDataSetNumber; ij++) {
                      Fhkl[0][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[8], i));
                      Fhkl[0][i][ij] *= Fhkl[0][i][ij];
                    }
                  } else {*/
                    Fhkl[0][i]/*[datasetindex]*/ = Double.valueOf(aloop.getCIFelement(FhklCIFindex[8], i));
                    Fhkl[0][i]/*[datasetindex]*/ *= Fhkl[0][i]/*[datasetindex]*/;
//                  }
                  if (!calc_notsquared) {
/*                    if (hasValidDataSet == 0) {
                      for (int ij = 0; ij < realDataSetNumber; ij++) {
                        Fhkl[1][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[8], i));
                        Fhkl[1][i][ij] *= Fhkl[1][i][ij];
                      }
                    } else {*/
                      Fhkl[1][i]/*[datasetindex]*/ = Double.valueOf(aloop.getCIFelement(FhklCIFindex[8], i));
                      Fhkl[1][i]/*[datasetindex]*/ *= Fhkl[1][i]/*[datasetindex]*/;
//                    }
                  }
                }
                if (calc_notsquared) {
/*                  if (hasValidDataSet == 0) {
                    for (int ij = 0; ij < realDataSetNumber; ij++) {
                      Fhkl[1][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[9], i));
                      Fhkl[1][i][ij] *= Fhkl[1][i][ij];
                    }
                  } else {*/
                    Fhkl[1][i]/*[datasetindex]*/ = Double.valueOf(aloop.getCIFelement(FhklCIFindex[9], i));
                    Fhkl[1][i]/*[datasetindex]*/ *= Fhkl[1][i]/*[datasetindex]*/;
//                  }
                  if (!meas_notsquared) {
/*                    if (hasValidDataSet == 0) {
                      for (int ij = 0; ij < realDataSetNumber; ij++) {
                        Fhkl[0][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[9], i));
                        Fhkl[0][i][ij] *= Fhkl[0][i][ij];
                      }
                    } else {*/
                      Fhkl[0][i]/*[datasetindex]*/ = Double.valueOf(aloop.getCIFelement(FhklCIFindex[9], i));
                      Fhkl[0][i]/*[datasetindex]*/ *= Fhkl[0][i]/*[datasetindex]*/;
//                    }
                  }
                }
//                System.out.println("Loaded " + hklm[0][i] + " " + hklm[1][i] + " " + hklm[2][i] + " " +
//                    Fhkl[0][i][datasetindex] + " " + Fhkl[1][i][datasetindex]);
                if (FhklCIFindex[6] != -1) {
/*                  if (hasValidDataSet == 0)
                    for (int ij = 0; ij < realDataSetNumber; ij++)
                      Fhkl[2][i][ij] = Double.valueOf(aloop.getCIFelement(FhklCIFindex[6], i));
                  else*/
                    Fhkl[2][i]/*[datasetindex]*/ = Double.valueOf(aloop.getCIFelement(FhklCIFindex[6], i));
                }
              }

	            for (int dataindex = 0; dataindex < asample.activeDatasetsNumber(); dataindex++) {
		            DataFileSet dataset = asample.getActiveDataSet(dataindex);
		            int datafilenumber = dataset.activedatafilesnumber();
		            Vector<Peak> fullpeaklist = dataset.getPeakList();
		            int numberofpeaks = dataset.getNumberofPeaks();
//		  System.out.println("Number of peaks: " + numberofpeaks + " " + fullpeaklist.size());
//llll		            int[] reflectionListIndices = new int[numberReflections];
//      double[] esdfactors = new double[numberofpeaks];

		            double[][] datasetSFactors = dataset.getStructureFactors((Phase) getParent());

		            for (int np = 0; np < numberofpeaks; np++) {
			            int peakNumber = fullpeaklist.elementAt(np).getOrderPosition();
				          datasetSFactors[0][peakNumber] = Fhkl[0][peakNumber];
			            datasetSFactors[1][peakNumber] = Fhkl[1][peakNumber];
		            }
	            }


            }
            if (hasValidDataSet == 0)
              datasetindex = asample.datasetsNumber();
            line = aloop.getLastReadedLine();
            aloop = null;
          }
        } //endoffile
//        needRestore = true;
        PFreader.close();
      } catch (IOException io) {
      }
      try {
        PFreader.close();
      } catch (IOException io) {
      }
    }
//    notLoaded = false;
/*
    BufferedReader PFreader = Misc.getReader(filename);
    if (PFreader != null) {
      try {
        String line = PFreader.readLine();
          if (line != null && line.contains("h  k  l   Fo   Fc   s")) {
	            Vector<int[]> allValues = new Vector<int[]>(20000, 10000);
	            line = PFreader.readLine();
							while ((line = PFreader.readLine()) != null && Misc.toStringDeleteBlankTabAndEOF(line).length() > 5) {
								StringTokenizer st = new StringTokenizer(line, " \t\r\n");
								while (st.hasMoreTokens()) {
									int[] values = new int[6];
									for (int i = 0; i < 6; i++)
										values[i] = Integer.parseInt(st.nextToken());
									allValues.add(values);
								}
								line = PFreader.readLine();
							}

	            // now we need to save
	            saveStructureFactors(filename + ".hkl", allValues);
	          saveSquaredStructureFactors(filename + ".shkl", allValues);
            }
        PFreader.close();
      } catch (IOException io) {
      }
      try {
        PFreader.close();
      } catch (IOException io) {
      }
    }*/
  }

	public void saveStructureFactors(String filename, Vector<int[]> values) {
		if (filename == null)
			return;

		PrintStream printStream = new PrintStream(Misc.getOutputStream(filename));

		try {

				String[] loopIDs = new String[6];
				loopIDs[0] = CIFdictionary.refln_h;
				loopIDs[1] = CIFdictionary.refln_k;
				loopIDs[2] = CIFdictionary.refln_l;
				loopIDs[3] = CIFdictionary.refln_FMeas;
				loopIDs[4] = CIFdictionary.refln_FCalc;
				loopIDs[5] = CIFdictionary.refln_FEsd;

				CIFUtil.writeLoopDecl(printStream, loopIDs, this);

				for (int j = 0; j < values.size(); j++) {
					int[] value = values.get(j);
					String[] valString = new String[6];
					for (int i = 0; i < 3; i++) {
						valString[i] = Integer.toString(value[i]);
						while (valString[i].length() < 5)
							valString[i] = " " + valString[i];
					}
					for (int i = 3; i < 6; i++) {
						valString[i] = Double.toString(value[i]);
						while (valString[i].length() < 10)
							valString[i] = " " + valString[i];
					}
					for (int i = 0; i < 6; i++)
						printStream.print(valString[i]);
						printStream.print(Constants.lineSeparator);
				}
				printStream.print(Constants.lineSeparator);

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

	public void saveSquaredStructureFactors(String filename, Vector<int[]> values) {
		if (filename == null)
			return;

		PrintStream printStream = new PrintStream(Misc.getOutputStream(filename));

		try {

			String[] loopIDs = new String[7];
			loopIDs[0] = CIFdictionary.refln_h;
			loopIDs[1] = CIFdictionary.refln_k;
			loopIDs[2] = CIFdictionary.refln_l;
			loopIDs[3] = CIFdictionary.refln_FsquaredMeas;
			loopIDs[4] = CIFdictionary.refln_FsquaredCalc;
			loopIDs[5] = CIFdictionary.refln_FsquaredEsd;
			loopIDs[6] = "_refln_observed_status";

			CIFUtil.writeLoopDecl(printStream, loopIDs, this);

			for (int j = 0; j < values.size(); j++) {
				int[] value = values.get(j);
				String[] valString = new String[6];
				for (int i = 0; i < 3; i++) {
					valString[i] = Integer.toString(value[i]);
					while (valString[i].length() < 5)
						valString[i] = " " + valString[i];
				}
				for (int i = 3; i < 6; i++) {
					valString[i] = Double.toString(value[i] * value[i]);
					while (valString[i].length() < 10)
						valString[i] = " " + valString[i];
				}
				for (int i = 0; i < 6; i++)
					printStream.print(valString[i]);
				printStream.print(" o" + Constants.lineSeparator);
			}
			printStream.print(Constants.lineSeparator);

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

	public JOptionsDialog getOptionsDialog(Frame parent) {
    return new JASFMOptionsD(parent, this);
  }

  public class JASFMOptionsD extends JOptionsDialog {

    public JASFMOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      JButton jb = new JButton("Import Fhkl from File");
      principalPanel.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
//          retrieveParameters();
          importStructureFactors();
        }
      });
      jb.setToolTipText("Press this to load structure factors in CIF/table format");

      jb = new JButton("Export Fhkl to File");
      principalPanel.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
//          retrieveParameters();
          exportStructureFactors();
        }
      });
      jb.setToolTipText("Press this to save structure factors in CIF format");

/*      jb = new JButton("Solve structure");
			principalPanel.add(jb);
			jb.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					solveCrystalStructure();
				}
			});
			jb.setToolTipText("Press this to solve the crystal structure from structure factors");
			*/
      setTitle("Extracting Structure Factors options panel");
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }

    public void importStructureFactors() {
      String filename = Utility.browseFilename(this, "import structure factors in CIF/table format");
      if (filename != null) {
        Phase aphase = (Phase) StructureFactorArbitraryModel.this.getParent();
        aphase.gethklNumber(); // forcing to build a reflection list
        loadStructureFactors(filename);
      }
    }

    public void exportStructureFactors() {
      String filename = Utility.browseFilenametoSave(this, "Save structure factors in CIF format");
      if (filename != null) {
        Phase aphase = (Phase) StructureFactorArbitraryModel.this.getParent();
        aphase.gethklNumber(); // forcing to build a reflection list
        saveStructureFactors(filename);
      }
    }

  }
}
