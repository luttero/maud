/*
 * @(#)batchProcess.java created 22/10/1999 Berkeley, Rudy's home
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.util;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;

import java.io.*;
import java.util.*;

import it.unitn.ing.rista.io.cif.*;

/**
 * The batchProcess is a class to process an instruction file for analysis as
 * a batch file. No interface will be generated or shown (textonly mode).
 * <p/>
 * 
 * @author Luca Lutterotti
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:59 $
 * @since JDK1.1
 */


public class batchProcess {

  String filename = null;
  String[] folderandname = null;
  String filenameToSave = null;
  String simpleResultFileName = null;
  String resultFileName = null;
  String plotOutputFileName = null;

  public String[] diclist = {"_riet_analysis_file",
                             "_riet_analysis_iteration_number", "_riet_analysis_wizard_index",
                             "_riet_analysis_fileToSave", "_riet_meas_datafile_name",
                             "_riet_append_simple_result_to", "_riet_append_result_to",
                             "_riet_meas_datafile_replace", "_maud_background_add_automatic",
                             "_maud_output_plot_filename"};

  public batchProcess(String insFileName) {
    filename = insFileName;
  }

  public void process() {
    if (filename != null) {
      folderandname = Misc.getFolderandName(filename);
	    System.out.println("Working in directory: " + folderandname[0]);

      String thecife;
      int newtoken, tokentype;

      BufferedReader reader = Misc.getReader(filename);
      if (reader != null) {
        CIFtoken ciffile = new CIFtoken(reader);

        try {
          do {
            tokentype = ciffile.nextToken();
//					System.out.println(ciffile.thestring);
            switch (tokentype) {
              case CIFtoken.TT_DATA:
                // new block of data
                break;
              case CIFtoken.TT_GLOB:
                // global data
                break;
              case CIFtoken.TT_INST:
                break;
              case CIFtoken.TT_DATASET:
                break;
              case CIFtoken.TT_SAMPLE:
                break;
              case CIFtoken.TT_BOUND:
                break;
              case CIFtoken.TT_PHASE:
                break;
              case CIFtoken.TT_SUBO:
                break;
              case CIFtoken.TT_LOOP:
                // data loop
                Vector itemlistv = new Vector(0, 1);
                Vector cifelistv = new Vector(0, 1);
                newtoken = ciffile.nextToken();
//							System.out.println(ciffile.thestring);
                while (newtoken == CIFtoken.TT_CIFE) {
                  itemlistv.addElement(ciffile.thestring);
                  newtoken = ciffile.nextToken();
//								System.out.println(ciffile.thestring);
                }
                int loopitem = itemlistv.size();
                if (loopitem > 0) {
                  while (FilePar.isValidToken(newtoken)) {
                    cifelistv.addElement(new CIFItem((String) itemlistv.elementAt(0),
                        ciffile.thestring, ciffile.thestringerror, ciffile.free));
                    for (int i = 1; i < loopitem; i++) {
                      ciffile.nextToken();
//										System.out.println(ciffile.thestring);
                      cifelistv.addElement(new CIFItem((String) itemlistv.elementAt(i),
                          ciffile.thestring, ciffile.thestringerror, ciffile.free));
                    }
                    newtoken = ciffile.nextToken();
//									System.out.println(ciffile.thestring);
                  }
                }
                ciffile.pushBack();
//							System.out.println("Pushback: " + ciffile.thestring);
                setLoop(cifelistv, loopitem);
                cifelistv.removeAllElements();
                itemlistv.removeAllElements();
                break;
              case CIFtoken.TT_CIFE:
                // CIF item
                thecife = ciffile.thestring;
                newtoken = ciffile.nextToken();
//							System.out.println(ciffile.thestring);
                if (FilePar.isValidToken(newtoken))
                  setField(thecife, ciffile.thestring);
                else {
                  ciffile.pushBack();
//								System.out.println("Pushback: " + ciffile.thestring);
                }
                break;
              default:
                {
                }
            }
          } while (tokentype != CIFtoken.TT_EOF);

        } catch (IOException e) {
          System.out.println("Error loading cif file!");
        }
        try {
          reader.close();
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
    }
  }

  public int setField(String cif, String astring) {
    cif = XRDcat.validateCIF(cif);
//		astring = XRDcat.validateString(astring);

    int index = ciftonumber(cif);

    if (index == 0) {
      String[] newfolderandname = Misc.getFolderandName(astring);//folderandname[0] +
      FilePar analysis = new FilePar(newfolderandname[1]);
      analysis.setDirectory(newfolderandname[0]);
      Reader in = Misc.getReader(analysis.getDirectory(), analysis.getFileName());
      analysis.readall(in, null);
	    analysis.setFileNamePreserveExtension(newfolderandname[1], false);
	    analysis.setDirectory(newfolderandname[0]);
      processAnalysis(analysis, -1);
    }

    return index;
  }

  public int ciftonumber(String cif) {
    int number = -1;
    for (int i = 0; i < diclist.length; i++)
      if (cif.equalsIgnoreCase(diclist[i]))
        return i;
    return number;
  }

  public void setLoop(Vector avector, int element) {

    XRDcat.validateCIF(avector);

    int loopitem = avector.size();
    FilePar analysis = null;
    int i = 0, index;
    CIFItem item;

    String token;
    int wizardindex = -1;

    if (loopitem > 0) {
      boolean automaticBackground = false;
      while (i < loopitem) {
        item = (CIFItem) avector.elementAt(i);
        index = ciftonumber(item.cif);
//	      System.out.println(item.cif + " " + index + " " + item.thestring);
        if (index == 0) {
          if (analysis != null) {
            if (automaticBackground) {
              analysis.checkOwnPolynomial();
              analysis.freeAllBackgroundParameters();
            }
            processAnalysis(analysis, wizardindex);
          }
          wizardindex = -1;

          token = item.thestring;
          String[] newfolderandname = Misc.getFolderandName(folderandname[0] + token);
          analysis = new FilePar(newfolderandname[1]);
	        analysis.setFileNamePreserveExtension(newfolderandname[1], true);
          analysis.setDirectory(newfolderandname[0]);
          filenameToSave = null;
          simpleResultFileName = null;
          resultFileName = null;
          plotOutputFileName = null;
          Reader in = Misc.getReader(analysis.getDirectory(), analysis.getFileName());
          analysis.readall(in, null);
	        analysis.setFileNamePreserveExtension(newfolderandname[1], false);
	        analysis.setDirectory(newfolderandname[0]);
          avector.removeElementAt(i);
          loopitem--;
        } else if (index == 1) {
          if (analysis != null) {
            analysis.setNumberofIterations(Integer.valueOf(item.thestring).intValue());
            wizardindex = -Integer.valueOf(item.thestring).intValue() - 1;
          }
          avector.removeElementAt(i);
          loopitem--;
        } else if (index == 2) {
          if (analysis != null) {
            wizardindex = Integer.valueOf(item.thestring).intValue() - 1;
          }
          avector.removeElementAt(i);
          loopitem--;
        } else if (index == 3) {
          if (analysis != null) {
            filenameToSave = item.thestring;
          }
          avector.removeElementAt(i);
          loopitem--;
        } else if (index == 4) {
//	        System.out.println(analysis + " " + item.thestring);
          if (analysis != null) {
            analysis.getSample(0).getDataSet(0).addDataFileforName(item.thestring, false);
          }
          avector.removeElementAt(i);
          loopitem--;
        } else if (index == 5) {
          if (analysis != null) {
            simpleResultFileName = item.thestring;
          }
          avector.removeElementAt(i);
          loopitem--;
        } else if (index == 6) {
          if (analysis != null) {
            resultFileName = item.thestring;
          }
          avector.removeElementAt(i);
          loopitem--;
        } else if (index == 7) {
          if (analysis != null) {
            if (item.thestring.equalsIgnoreCase("true"))
              analysis.getSample(0).setReplaceDatafile(true);
            else
              analysis.getSample(0).setReplaceDatafile(false);
          }
          avector.removeElementAt(i);
          loopitem--;
        } else if (index == 8) {
          if (analysis != null) {
            if (item.thestring.equalsIgnoreCase("true")) {
              analysis.getSample(0).setAutomaticPolynomialBackground(true);
              automaticBackground = true;
            } else
              analysis.getSample(0).setAutomaticPolynomialBackground(false);
          }
          avector.removeElementAt(i);
          loopitem--;
        } else if (index == 9) {
          if (analysis != null) {
            plotOutputFileName = item.thestring;
          }
          avector.removeElementAt(i);
          loopitem--;
        } else
          i++;
      } // end of while (i < loopitem)

      if (automaticBackground) {
        analysis.checkOwnPolynomial();
        analysis.freeAllBackgroundParameters();
      }
      processAnalysis(analysis, wizardindex);

    } // end of if (loopitem>0)

  }

//XSolo	static double[] interpolatedValues = {100.5, 105, 112.9, 117.9, 129.7, 138.8, 145.9, 157, 159, 161};

  public void processAnalysis(FilePar analysis, int wizardindex) {
/*	  if (interpolatedValues.length > 1) {
		  Sample sample = analysis.getActiveSample();
		  for (int i = 0; i < sample.activeDatasetsNumber(); i++) {
			  DataFileSet dataset = sample.getActiveDataSet(i);
			  if (dataset.isBackgroundInterpolated()) {
				  for (int k = 0; k < dataset.activedatafilesnumber(); k++) {
					  DiffrDataFile datafile = dataset.getActiveDataFile(k);
					  datafile.setManualBkgInterpolation(true);
					  for (int j = 0; j < interpolatedValues.length; j++)
						  if (datafile.xInsideRange(interpolatedValues[j]))
							  datafile.addInterpolatedPointsX(interpolatedValues[j]);
				  }
			  }
		  }
	  }*/
	  if (filenameToSave == null)
      filenameToSave = analysis.getDirectory() + analysis.getFileName();
	  else
	   filenameToSave = folderandname[0] + filenameToSave;
	  if (analysis != null) {
      long time = System.currentTimeMillis();
      if (wizardindex == 999) {
//        System.out.println("Starting function computation for analysis file: " + analysis.toXRDcatString());
        analysis.launchrefine(null);
        System.out.println(analysis.getWSS());
        double[] parameters = analysis.getfreeParameters();
        for (int i = 0; i < parameters.length; i++) {
          System.out.println(parameters[i]);
        }
//        System.out.println("Time for computation was: " + (System.currentTimeMillis() - time) + " millisecs.");
      } else if (wizardindex == -1) {
        System.out.println("Starting function computation for analysis file: " + analysis.toXRDcatString());
        analysis.compute(null);
        System.out.println("Time for computation was: " + (System.currentTimeMillis() - time) + " millisecs.");
      } else if (wizardindex < 0) {
        System.out.println("Starting function computation for analysis file: " + analysis.toXRDcatString());
        analysis.compute(null);
        System.out.println("Time for computation was: " + (System.currentTimeMillis() - time) + " millisecs.");
        System.out.println("Starting refinement for analysis file: " + analysis.toXRDcatString());
        //       analysis.startingRefine();
        analysis.launchrefine(null);
        System.out.println("Time for computation was: " + (System.currentTimeMillis() - time) + " millisecs.");
        String[] folderandnameToSave = Misc.getFolderandName(filenameToSave);
	      analysis.resetIncrementRefinementNumber();
	      analysis.setFileNamePreserveExtension(folderandnameToSave[1], false);
	      analysis.setDirectory(folderandnameToSave[0]);
        BufferedWriter out = Misc.getWriter(folderandnameToSave[0], folderandnameToSave[1]);
        analysis.writeall(out);
      } else if (wizardindex > 10000) { // Xspider
	      wizardindex -= 10000;
	      System.out.println("Starting function computation for analysis file: " + analysis.toXRDcatString());
	      analysis.compute(null);
	      System.out.println("Time for computation was: " + (System.currentTimeMillis() - time) + " millisecs.");
	      System.out.println("Starting refinement for analysis file: " + analysis.toXRDcatString());
	      //       analysis.startingRefine();
	      analysis.launchrefine(null);
	      System.out.println("Time for computation was: " + (System.currentTimeMillis() - time) + " millisecs.");
	      String[] folderandnameToSave = Misc.getFolderandName(filenameToSave);
	      analysis.resetIncrementRefinementNumber();
	      analysis.setFileNamePreserveExtension(folderandnameToSave[1], false);
	      analysis.setDirectory(folderandnameToSave[0]);
	      BufferedWriter out = Misc.getWriter(folderandnameToSave[0], folderandnameToSave[1]);
	      analysis.writeall(out);
      } else if (wizardindex > 1000) {
        wizardindex -= 1000;
        System.out.println("Starting wizard for analysis file: " + analysis.toXRDcatString());
        double lastGofF;
        double actualGofF = 999.0;
        do {
          String[] folderandnameToSave = Misc.getFolderandName(filenameToSave);
	        analysis.resetIncrementRefinementNumber();
	        analysis.setFileNamePreserveExtension(folderandnameToSave[1], false);
	        analysis.setDirectory(folderandnameToSave[0]);
          BufferedWriter out = Misc.getWriter(folderandnameToSave[0], folderandnameToSave[1]);
          analysis.writeall(out);
          lastGofF = actualGofF;
          if (actualGofF != 999.0) {
            analysis.setZeroPhase(analysis.getMinorPhase());
            wizardindex = 4;
          }
          if (analysis.getNumberNonZeroPhases() > 0) {
            analysis.refineWizard(null, wizardindex);
            actualGofF = Math.sqrt(Double.parseDouble(analysis.getRw()) / Double.parseDouble(analysis.getRexp()));
          } else
            actualGofF = lastGofF + 10.0;
        } while (lastGofF >= actualGofF && analysis.getNumberNonZeroPhases() > 0);
        System.out.println("Time for computation was: " + (System.currentTimeMillis() - time) + " millisecs.");
      } else {
        System.out.println("Starting wizard for analysis file: " + analysis.toXRDcatString());
        analysis.refineWizard(null, wizardindex);
        System.out.println("Time for computation was: " + (System.currentTimeMillis() - time) + " millisecs.");
        String[] folderandnameToSave = Misc.getFolderandName(filenameToSave);
        System.out.println("Saving file: " + folderandnameToSave[0] + folderandnameToSave[1]);
	      analysis.resetIncrementRefinementNumber();
	      analysis.setFileNamePreserveExtension(folderandnameToSave[1], false);
	      analysis.setDirectory(folderandnameToSave[0]);
        BufferedWriter out = Misc.getWriter(folderandnameToSave[0], folderandnameToSave[1]);
        analysis.writeall(out);
      }
      if (simpleResultFileName != null) {
        String[] folderAndName = Misc.getFolderandName(simpleResultFileName);
        analysis.appendResultsTo(folderandname[0] + folderAndName[0], folderAndName[1], true);
      }
      if (resultFileName != null) {
        String[] folderAndName = Misc.getFolderandName(resultFileName);
        analysis.appendResultsTo(folderandname[0] + folderAndName[0], folderAndName[1], false);
      }
      if (plotOutputFileName != null) {
        String[] folderAndName = Misc.getFolderandName(plotOutputFileName);
        exportExperimentalComputedData(analysis, folderandname[0] + folderAndName[0], folderAndName[1]);
      }
    }
  }

  public void exportExperimentalComputedData(FilePar analysis, String folder, String afilename) {

    String[] folderAndName = Misc.getFolderandName(filename);

    if (!afilename.endsWith(".cif"))
      afilename = afilename + ".cif";

    if (afilename != null) {

      BufferedWriter output = Misc.getWriter(folder, afilename);
      try {
        output.write("data_");
        output.newLine();
        output.newLine();
        analysis.exportExperimentalComputedData(output);
        output.close();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

}
