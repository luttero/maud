/*
 * @(#)GSASDataFile.java created 10/07/1998 Grenoble, ILL
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.data;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.cal.GSASbankCalibration;

import java.io.*;
import java.lang.*;
import java.util.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.fortran.*;


/**
 *  The GSASDataFile is a class to load a GSASdatafile and transfer all single spectra
 *  in different instances of DiffrDataFile (assuming a GSAS datafile is a multi-spectra
 *  datafile
 *
 *
 * @version $Revision: 1.12 $, $Date: 2006/11/10 09:33:01 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GSASDataFile extends MultDiffrDataFile {

  public GSASDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".gda";
  }

  public GSASDataFile() {
    identifier = ".gda";
  }


  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = getReader();
    if (reader != null) {
      try {
	      int index = 1;
//				boolean timeMap = false;
        Hashtable tmapTable = new Hashtable();
        int tmap[][] = null;
        TimeMap aTimeMap = null;
        int mapno = 0, nvals = 0, nrec = 0, tmax = 0;
        double clckwdt = 0.0;
        double width = 1.0;

        String token = new String("");
        StringTokenizer st = null;
        String linedata = null;
        boolean endoffile = false;

        String titleString = reader.readLine();
	      titleString = Misc.removeUTF8BOM(titleString);
	      double omega = 0.0, chi = 0.0, phi = 0.0;
	      double scale_factor = 1.0;

        st = new StringTokenizer(titleString, " ,\t\r\n");
        token = st.nextToken();
//	      System.out.println(token);
        if (!token.equalsIgnoreCase("BANK") && !token.startsWith("TIME_MAP")) {
          while (st.hasMoreTokens()) {
            token = st.nextToken();
//	          System.out.println("1 - " + token);
            if (token.equalsIgnoreCase("Omega")) {
              token = st.nextToken();
              token = st.nextToken();
              omega = Double.parseDouble(token) / 10.0;
            } else if (token.equalsIgnoreCase("Chi")) {
              token = st.nextToken();
              token = st.nextToken();
              chi = Double.parseDouble(token) / 10.0;
            } else if (token.equalsIgnoreCase("Phi")) {
              token = st.nextToken();
              token = st.nextToken();
              phi = Double.parseDouble(token) / 10.0;
            }

          }
        }

        while (!endoffile) {
          scale_factor = 1.0;
          while (!token.equalsIgnoreCase("BANK") && !token.startsWith("TIME_MAP")) {
	          if (token.startsWith("#scale_factor")) {
		          token = st.nextToken();
		          scale_factor = Double.parseDouble(token = st.nextToken());
	          } else if (token.startsWith("#omega") || token.startsWith("#Omega") || token.startsWith("#OMEGA")) {
		          token = st.nextToken();
		          omega = Double.parseDouble(token = st.nextToken());
//		          System.out.println(token);
	          } else if (token.startsWith("#chi") || token.startsWith("#Chi") || token.startsWith("#CHI")) {
		          token = st.nextToken();
		          chi = Double.parseDouble(token = st.nextToken());
	          } else if (token.startsWith("#phi") || token.startsWith("#Phi") || token.startsWith("#PHI")) {
		          token = st.nextToken();
		          phi = Double.parseDouble(token = st.nextToken());
	          }
	          linedata = reader.readLine();
//	          System.out.println(linedata);
            if (linedata == null || linedata.equals("")) {
              endoffile = true;
              break;
            }

            st = new StringTokenizer(linedata, " ,\t\r\n");

            token = st.nextToken();
          }

          if (endoffile)
            break;

          if (token.startsWith("TIME_MAP")) {
//        		timeMap = true;
            if (token.length() == 8)
              mapno = Integer.parseInt(token = st.nextToken());
            else
              mapno = Integer.parseInt(token.substring(8));
            nvals = (Integer.parseInt(token = st.nextToken()) - 1) / 3;
            nrec = Integer.parseInt(token = st.nextToken());
            token = st.nextToken();
            clckwdt = Double.parseDouble(token = st.nextToken());
            aTimeMap = new TimeMap(mapno, nvals, nrec, clckwdt);

            linedata = reader.readLine();
            st = new StringTokenizer(linedata, " ,\t\r\n");
            for (int i = 0; i < nvals; i++) {
              for (int j = 0; j < 3; j++) {
                if (!st.hasMoreTokens()) {
                  linedata = reader.readLine();
                  st = new StringTokenizer(linedata, " ,\t\r\n");
                }
                aTimeMap.setMap(j, i, st.nextToken());
//          			System.out.println(tmap[j][i]);
              }
            }
            if (!st.hasMoreTokens()) {
              linedata = reader.readLine();
              st = new StringTokenizer(linedata, " ,\t\r\n");
            }
            aTimeMap.setTmax(Integer.parseInt(token = st.nextToken()));
            tmapTable.put(Integer.toString(mapno), aTimeMap);
          } else {
            int mapNumber = 0;
            int banknumber = Integer.parseInt(token = st.nextToken());
            String bankID = new String(GSASbankCalibration.bankPrefix + Integer.toString(banknumber));
				GSASbankCalibration cal = (GSASbankCalibration) ((DataFileSet) getParent()).getInstrument().getAngularCalibration();
				int bankNumber = -1;
            try {
	            bankNumber = cal.getBankNumber(bankID);
            } catch (Exception e) {
            }

//        	DataCalibration gsascal = getCalibration("bank", 6);

//       	   System.out.println("Reading bank number: " + token + " " + bankID + " " + bankNumber);

	          DiffrDataFile datafile = null;
	          boolean atmpB = false;
	          if (bankNumber >= 0) {
		          datafile = addDiffrDatafile(Integer.toString(index++));
		          atmpB = datafile.isAbilitatetoRefresh;
		          datafile.isAbilitatetoRefresh = false;

		          datafile.title = new String(titleString);

		          datafile.setAngleValue(0, omega);
		          datafile.setAngleValue(1, chi);
		          datafile.setAngleValue(2, phi);
		          datafile.setBankID(bankID);
	          } else index++;
//	          System.out.println(datafile.getLabel() + ", setting bank ID and omega :" + bankID + " " + omega);

            int nchannel = Integer.parseInt(token = st.nextToken());
				 if (datafile != null)
               datafile.initData(nchannel);

            nrec = Integer.parseInt(token = st.nextToken());
            String bintyp = st.nextToken();
            String form = "STD";
            double bcoeff[] = new double[4];
            int numbercoeff = 0;
            if (bintyp.startsWith("TIME_MAP")) {
              form = "TIME_MAP";
              mapNumber = Integer.parseInt(token = st.nextToken());
            } else {
              for (int j = 0; j < 4; j++)
                if (st.hasMoreTokens()) {
                  bcoeff[j] = Double.parseDouble(token = st.nextToken());
                  numbercoeff++;
                }
            }
            if (st.hasMoreTokens()) {
              form = st.nextToken();
//              System.out.println("Form " + form);
            }

//            System.out.println("Form final " + form);

	          if (datafile != null) {
		          datafile.dspacingbase = true;
		          datafile.constantstep = false;
		          datafile.datanumber = nchannel;
		          if (bintyp.equalsIgnoreCase("lpsd")) {
			          datafile.dspacingbase = false;
			          datafile.constantstep = true;
		          }
	          }
//            if (bintyp.equalsIgnoreCase("const")) {  todo : check this, also const may be in twotheta
//              datafile.dspacingbase = false;
//              datafile.constantstep = true;
//            }

            int i = 0;
            int tmap_index = 0;
            while (i < nchannel) {
              linedata = reader.readLine();
              if (linedata == null) {
                endoffile = true;
	              if (datafile != null) {
						  datafile.isAbilitatetoRefresh = atmpB;
						  datafile.dataLoaded = true;
	              }
                break;
              }
              String inputString = null;
              int repeat = 0;
              int number = 1;
              int[] digits = null;
              if (form.equalsIgnoreCase("TIME_MAP")) {
//        				inputString = "I8";
                digits = new int[1];
                digits[0] = 8;
                repeat = 10;
                if (nchannel - i < repeat)
                  repeat = nchannel - i;
              } else if (form.equalsIgnoreCase("STD")) {
//        				inputString = "I2,F6.0";
                digits = new int[2];
                digits[0] = 2;
                digits[1] = 6;
                repeat = 10;
                number = 2;
                if (nchannel - i < repeat)
                  repeat = nchannel - i;
              } else if (form.equalsIgnoreCase("ESD")) {
//        				inputString = "F8,F8";
                digits = new int[2];
                digits[0] = 8;
                digits[1] = 8;
                repeat = 5;
                number = 2;
                if (nchannel - i < repeat)
                  repeat = nchannel - i;
              } else if (form.equalsIgnoreCase("ALT")) {
//        				inputString = "F8.0,F7.4,F5.4";
                digits = new int[3];
                digits[0] = 8;
                digits[1] = 7;
                digits[2] = 5;
                repeat = 4;
                number = 3;
                if (nchannel - i < repeat)
                  repeat = nchannel - i;
              } else if (form.equalsIgnoreCase("FXY")) {
	              repeat = 1;
	              number = 2;
              } else if (form.equalsIgnoreCase("FXYE")) {
	              repeat = 1;
	              number = 3;
              }
              String[] data = Misc.readFormattedLine(linedata, digits, number, repeat);
//        			String[] data = readFortranLine(linedata,inputString,number,repeat);
              for (int j = 0; j < repeat * number; j += number) {
                int nctr = 1;
                double x = 0.0;
                double esd = 0.0;
                if (form.equalsIgnoreCase("TIME_MAP")) {
                  token = data[j];
                } else if (form.equalsIgnoreCase("STD")) {
                  if (data[j] != null && !data[j].equals("  ")) {
                    nctr = Integer.parseInt(Misc.toStringDeleteBlankAndTab(data[j]));
                    if (nctr <= 0)
                      nctr = 1;
                  }
                  token = data[j + 1];
                } else if (form.equalsIgnoreCase("ESD")) {
                  token = data[j];
                  esd = Double.parseDouble(data[j + 1]);
                } else if (form.equalsIgnoreCase("ALT")) {
                  x = Double.parseDouble(data[j]);
                  token = data[j + 1];
                  esd = Double.parseDouble(data[j + 2]);
                } else if (form.equalsIgnoreCase("FXY")) {
	                x = Double.parseDouble(data[j]);
	                token = data[j + 1];
	                esd = 0.0;
                } else if (form.equalsIgnoreCase("FXYE")) {
	                x = Double.parseDouble(data[j]);
	                token = data[j + 1];
	                esd = Double.parseDouble(data[j + 2]);
                }

	              if (datafile != null) {
		              if (bintyp.equalsIgnoreCase("RALF"))
			              datafile.setXData(i, x / 32);
		              else if (bintyp.equalsIgnoreCase("const"))
			              datafile.setXData(i, (bcoeff[0] + bcoeff[1] * i));
		              else if (bintyp.equalsIgnoreCase("lpsd"))
			              datafile.setXData(i, (bcoeff[0] + bcoeff[2] * (i - bcoeff[1])) / 100.0);
		              else if (bintyp.equalsIgnoreCase("log6"))
			              datafile.setXData(i, bcoeff[0] + bcoeff[1] * Math.log(i));
		              else if (bintyp.equalsIgnoreCase("SLOG"))
			              datafile.setXData(i, x);
		              else if (bintyp.startsWith("TIME_MAP")) {
			              String mapNumberKey = Integer.toString(mapNumber);
			              if (tmapTable.containsKey(mapNumberKey))
				              aTimeMap = (TimeMap) tmapTable.get(mapNumberKey);
			              if (tmap_index < aTimeMap.nvals - 1 && i + 1 >= aTimeMap.tmap[0][tmap_index + 1])
				              tmap_index++;
			              double tof = aTimeMap.getTOF(i, tmap_index);
			              width = aTimeMap.tmap[2][tmap_index] * aTimeMap.clckwdt / 1000;
			              datafile.setXData(i, tof);
		              } else
			              datafile.setXData(i, bcoeff[0] + bcoeff[1] * i);


//      System.out.println( bcoeff[0] + bcoeff[1] * i );
//      System.out.println( token );
		              datafile.setYData(i, Double.parseDouble(token) /* nctr */ / scale_factor / width);
		              double tmpweight = 0.0;
		              if (form.equalsIgnoreCase("ESD")) {
			              if (esd != 0.0)
				              tmpweight = 1.0 / esd;
			              else
				              tmpweight = 1.0;
		              } else {
			              tmpweight = Math.sqrt(datafile.getYData(i));
			              if (tmpweight != 0.0)
				              tmpweight = 1.0 / tmpweight;
			              else
				              tmpweight = 1.0;
		              }

		              datafile.setWeight(i, tmpweight);
	              }
                i++;
              }
            }
	          if (datafile != null) {
		          loadSuccessfull = true;
		          datafile.isAbilitatetoRefresh = atmpB;
		          datafile.dataLoaded = true;
	          }

//          gsascal.calibrate(this);
          }
        }
        tmapTable = null;

      } catch (Exception e) {
        e.printStackTrace();
        System.out.println("Error in loading the data file! Try to remove this data file");
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
    }
    isAbilitatetoRefresh = tmpB;
    return loadSuccessfull;
  }

	// not used
  public static String[] readFortranLine(String line, String form, int number, int repeat) {

    StringBuffer finalForm = new StringBuffer(form);
    for (int i = 1; i < repeat; i++)
      finalForm.append("," + form);

    String[] data = new String[number * repeat];

    try {
      it.unitn.ing.fortran.Formatter f = new it.unitn.ing.fortran.Formatter(finalForm.toString());
      String[] keys = new String[number * repeat];
      for (int i = 0; i < number * repeat; i++)
        keys[i] = Integer.toString(i);
      Hashtable ht = new Hashtable();
      f.read(keys, ht, new DataInputStream(new StringBufferInputStream(line)));

      for (int i = 0; i < number * repeat; i++)
        data[i] = (String) ht.get(keys[i]);
    } catch (InvalidFormatException e) {
      System.out.println(e);
    } catch (InputFormatException e) {
      System.out.println(e);
    }

    return data;

  }

  class TimeMap {
    int mapNumber = 0;
    int nvals = 0;
    int nrec = 0;
    double clckwdt = 0.0;
    int[][] tmap = null;
    int tmax = 0;

    public TimeMap(int mapno, int nvals, int nrec, double clckwdt) {
      this.mapNumber = mapno;
      this.nvals = nvals;
      this.nrec = nrec;
      this.clckwdt = clckwdt;
      tmap = new int[3][nvals];
    }

    public void setMap(int j, int i, String token) {
      tmap[j][i] = Integer.valueOf(token).intValue();
    }

    public void setTmax(int tmax) {
      this.tmax = tmax;
    }

    public double getTOF(int i, int tmap_index) {
      return (tmap[1][tmap_index] + (i - tmap[0][tmap_index] + 1) *
              tmap[2][tmap_index]) * clckwdt / 1000;
    }
  }

/*
	public String[] readFortranLine(String line, String form, int number, int repeat) {

		StringBuffer finalForm = new StringBuffer(form);
		for (int i = 1; i < repeat; i++)
			finalForm.append("," + form);

		String[] data = new String[number * repeat];

		try {
			it.unitn.ing.fortran.Formatter f = new it.unitn.ing.fortran.Formatter(finalForm.toString());
			String[] keys = new String[number * repeat];
			for (int i = 0; i < number * repeat; i++)
				keys[i] = Integer.toString(i);
			Hashtable ht = new Hashtable();
			f.read(keys, ht, new DataInputStream(new StringBufferInputStream(line)));

			for (int i = 0; i < number * repeat; i++)
				data[i] = (String) ht.get(keys[i]);
		} catch (it.unitn.ing.fortran.InvalidFormatException e) {
			System.out.println(e);
		} catch (it.unitn.ing.fortran.InputFormatException e) {
			System.out.println(e);
		}

		return data;

	}
*/
	public static String sumTheDatafiles(String[] filename) {

		String finalName = "";
		int filesNumber = filename.length;
		if (filesNumber <= 0)
			return finalName;
		StringBuilder newfilename = new StringBuilder("");
		newfilename.append(filename);
		int maxBankNumber = MaudPreferences.getInteger("gsas_format.max_number_banks", 50);

		BufferedWriter output = null;

		try {
			String[] folderAndName = Misc.getFolderandName(filename[0]);
			String[] folderAndNamef = Misc.getFolderandName(filename[filesNumber - 1]);
			String filenameOnly = folderAndName[1].substring(0, folderAndName[1].length() - 4) +
					"-" + folderAndNamef[1].substring(0, folderAndNamef[1].length() - 4);
			finalName = folderAndName[0] + filenameOnly + "_sum.gda";
			output = Misc.getWriter(folderAndName[0], filenameOnly + "_sum.gda");
		} catch(Exception e) {
			return "";
		}

		BufferedReader[] readers = new BufferedReader[filesNumber];
		for (int i = 0; i < filesNumber; i++) {
			String filenameS = filterfilename(filename[i]);
//    System.out.println("Loading " + getFolder() + filename);
			readers[i] = Misc.getReader(filenameS);
		}

		try {
			boolean timeMap = false;
			int tmap[][] = null;
			int mapno = 0, nvals = 0, nrec = 0, tmax = 0;
			double clckwdt = 0.0;

			String[] token = new String[filesNumber];
			StringTokenizer st = null;
			String linedata = null;
			boolean[] endoffile = new boolean[filesNumber];
			for (int i = 0; i < filesNumber; i++)
				endoffile[i] = false;

			String titleString;
			for (int i = filesNumber - 1; i >= 0; i--) {
				titleString = readers[i].readLine();
				titleString = Misc.removeUTF8BOM(titleString);
				if (i == 0 && titleString != null) {
					output.write(titleString);
					output.newLine();
				}
				double omega = 0.0, chi = 0.0, phi = 0.0;
				st = new StringTokenizer(titleString, " ,\t\r\n");
				token[i] = st.nextToken();
				if (!token[i].equalsIgnoreCase("BANK") && !token[i].startsWith("TIME_MAP")) {
					while (st.hasMoreTokens()) {
						token[i] = st.nextToken();
						if (token[i].equalsIgnoreCase("Omega")) {
							token[i] = st.nextToken();
							token[i] = st.nextToken();
							omega = Double.valueOf(token[i]).doubleValue() / 10.0;
						} else if (token[i].equalsIgnoreCase("Chi")) {
							token[i] = st.nextToken();
							token[i] = st.nextToken();
							chi = Double.valueOf(token[i]).doubleValue() / 10.0;
						} else if (token[i].equalsIgnoreCase("Phi")) {
							token[i] = st.nextToken();
							token[i] = st.nextToken();
							phi = Double.valueOf(token[i]).doubleValue() / 10.0;
						}
					}
				}

			}


			String[][] nctr = null;
			String[][] x = null;
			double[][] y = null;
			double[][] esd = null;

			for (int i = filesNumber - 1; i >= 0; i--) {
//			  System.out.println("File: " + i);
//			  System.out.flush();
//			  Thread.sleep(100);
				while (!endoffile[i]) {
					while (!token[i].equalsIgnoreCase("BANK") && !token[i].startsWith("TIME_MAP")) {
						linedata = readers[i].readLine();
//					  System.out.println(linedata);
						if (linedata == null || linedata.equals("")) {
//						  System.out.println("End of file: " + i);
//						  System.out.flush();
//						  Thread.sleep(100);
							endoffile[i] = true;
							break;
						}
						if (i == 0 && linedata != null) {
							output.write(linedata);
							output.newLine();
						}
						st = new StringTokenizer(linedata, " ,\t\r\n");
						token[i] = st.nextToken();
					}

					if (endoffile[i])
						break;

					if (token[i].startsWith("TIME_MAP")) {
						timeMap = true;
						if (token[i].length() == 8)
							mapno = Integer.valueOf(token[i] = st.nextToken()).intValue();
						else
							mapno = Integer.valueOf(token[i].substring(8)).intValue();
						nvals = (Integer.valueOf(token[i] = st.nextToken()).intValue() - 1) / 3;
						nrec = Integer.valueOf(token[i] = st.nextToken()).intValue();
						token[i] = st.nextToken();
						clckwdt = Double.valueOf(token[i] = st.nextToken()).doubleValue();

						linedata = readers[i].readLine();
//					  System.out.println(linedata);
						if (i == 0 && linedata != null) {
							output.write(linedata);
							output.newLine();
						}
						st = new StringTokenizer(linedata, " ,\t\r\n");
						tmap = new int[3][nvals];
						for (int ii = 0; ii < nvals; ii++) {
							for (int j = 0; j < 3; j++) {
								if (!st.hasMoreTokens()) {
									linedata = readers[i].readLine();
//								  System.out.println(linedata);
									if (i == 0 && linedata != null) {
										output.write(linedata);
										output.newLine();
									}
									st = new StringTokenizer(linedata, " ,\t\r\n");
								}
								tmap[j][ii] = Integer.valueOf(token[i] = st.nextToken()).intValue();
//          			System.out.println(tmap[j][i]);
							}
						}
						if (!st.hasMoreTokens()) {
							linedata = readers[i].readLine();
//						  System.out.println(linedata);
							if (i == 0 && linedata != null) {
								output.write(linedata);
								output.newLine();
							}
							st = new StringTokenizer(linedata, " ,\t\r\n");
						}
						tmax = Integer.valueOf(token[i] = st.nextToken()).intValue();
					} else {
						int banknumber = Integer.valueOf(token[i] = st.nextToken()).intValue() - 1;
						int nchannel = Integer.valueOf(token[i] = st.nextToken()).intValue();
						if (nctr == null) {
							nctr = new String[maxBankNumber][nchannel];
							x = new String[maxBankNumber][nchannel];
							y = new double[maxBankNumber][nchannel];
							esd = new double[maxBankNumber][nchannel];
							for (int jj = 0; jj < maxBankNumber; jj++)
								for (int ii1 = 0; ii1 < nchannel; ii1++) {
									y[jj][ii1] = 0.0;
									esd[jj][ii1] = 0.0;
								}
						}
						nrec = Integer.valueOf(token[i] = st.nextToken()).intValue();
						String bintyp = st.nextToken();
						String form = "STD";
						double bcoeff[] = new double[4];
						int numbercoeff = 0;
						if (bintyp.startsWith("TIME_MAP")) {
							form = "TIME_MAP";
							token[i] = st.nextToken(); // mapNumber
						} else {
							for (int j = 0; j < 4; j++)
								if (st.hasMoreTokens()) {
									bcoeff[j] = Double.valueOf(token[i] = st.nextToken()).doubleValue();
									numbercoeff++;
								}
						}
						if (st.hasMoreTokens()) {
							form = st.nextToken();
						}

//					  System.out.println("Final form: " + form);

						int ii = 0;
						int tmap_index = 0;

						int repeat = 0;
						int number = 1;
						int[] digits = null;
						if (form.equalsIgnoreCase("TIME_MAP")) {
//        				inputString = "I8";
							digits = new int[1];
							digits[0] = 8;
							repeat = 10;
						} else if (form.equalsIgnoreCase("STD")) {
//        				inputString = "I2,F6.0";
							digits = new int[2];
							digits[0] = 2;
							digits[1] = 6;
							repeat = 10;
							number = 2;
						} else if (form.equalsIgnoreCase("ESD")) {
//        				inputString = "F8,F8";
							digits = new int[2];
							digits[0] = 8;
							digits[1] = 8;
							repeat = 5;
							number = 2;
						} else if (form.equalsIgnoreCase("ALT")) {
//        				inputString = "F8.0,F7.4,F5.4";
							digits = new int[3];
							digits[0] = 8;
							digits[1] = 7;
							digits[2] = 5;
							repeat = 4;
							number = 3;
						}

						while (ii < nchannel) {
							linedata = readers[i].readLine();
							if (linedata == null) {
//							  System.out.println("End of file_1: " + i);
//							  System.out.flush();
//							  Thread.sleep(100);
								endoffile[i] = true;
								break;
							}
							if (form.equalsIgnoreCase("TIME_MAP")) {
								if (nchannel - ii < repeat)
									repeat = nchannel - ii;
							} else if (form.equalsIgnoreCase("STD")) {
								if (nchannel - ii < repeat)
									repeat = nchannel - ii;
							} else if (form.equalsIgnoreCase("ESD")) {
								if (nchannel - ii < repeat)
									repeat = nchannel - ii;
							} else if (form.equalsIgnoreCase("ALT")) {
								if (nchannel - ii < repeat)
									repeat = nchannel - ii;
							}
							String[] data = Misc.readFormattedLine(linedata, digits, number, repeat);
//        			String[] data = readFortranLine(linedata,inputString,number,repeat);
							for (int j = 0; j < repeat * number; j += number) {
								double tmpEsd = 0.0;
								if (form.equalsIgnoreCase("TIME_MAP")) {
									token[i] = data[j];
								} else if (form.equalsIgnoreCase("STD")) {
									nctr[banknumber][ii] = data[j];
									token[i] = data[j + 1];
								} else if (form.equalsIgnoreCase("ESD")) {
									token[i] = data[j];
									tmpEsd = Double.valueOf(data[j + 1]).doubleValue();
//                  System.out.println("ESD " + data[j] + " " + data[j + 1]);
								} else if (form.equalsIgnoreCase("ALT")) {
									x[banknumber][ii] = data[j];
									token[i] = data[j + 1];
									tmpEsd = Double.valueOf(data[j + 2]).doubleValue();
								}

								y[banknumber][ii] += Double.valueOf(token[i]).doubleValue();
								esd[banknumber][ii] += tmpEsd * tmpEsd;

								ii++;
							}
						}

						if (i == 0) {
//						  System.out.println("Writing file: " + i + " " + form);
//						  System.out.flush();
//						  Thread.sleep(100);
							ii = 0;
							repeat = 0;
							number = 1;
							if (form.equalsIgnoreCase("TIME_MAP")) {
//        				inputString = "I8";
								digits = new int[1];
								digits[0] = 8;
								repeat = 10;
							} else if (form.equalsIgnoreCase("STD")) {
//        				inputString = "I2,F6.0";
								digits = new int[2];
								digits[0] = 2;
								digits[1] = 6;
								repeat = 10;
								number = 2;
							} else if (form.equalsIgnoreCase("ESD")) {
//        				inputString = "F8,F8";
								digits = new int[2];
								digits[0] = 8;
								digits[1] = 8;
								repeat = 5;
								number = 2;
							} else if (form.equalsIgnoreCase("ALT")) {
//        				inputString = "F8.0,F7.4,F5.4";
								digits = new int[3];
								digits[0] = 8;
								digits[1] = 7;
								digits[2] = 5;
								repeat = 4;
								number = 3;
							}
							while (ii < nchannel) {
								linedata = "";
								if (form.equalsIgnoreCase("TIME_MAP")) {
									if (nchannel - ii < repeat)
										repeat = nchannel - ii;
								} else if (form.equalsIgnoreCase("STD")) {
									if (nchannel - ii < repeat)
										repeat = nchannel - ii;
								} else if (form.equalsIgnoreCase("ESD")) {
									if (nchannel - ii < repeat)
										repeat = nchannel - ii;
								} else if (form.equalsIgnoreCase("ALT")) {
									if (nchannel - ii < repeat)
										repeat = nchannel - ii;
								}
								for (int j = 0; j < repeat; j++) {
									if (form.equalsIgnoreCase("TIME_MAP")) {
//        				inputString = "I8";
										linedata += formatStringFor(y[banknumber][ii], digits[0], -1);
									} else if (form.equalsIgnoreCase("STD")) {
//        				inputString = "I2,F6.0";
										linedata += nctr[banknumber][ii]; // nctr
										linedata += formatStringFor(y[banknumber][ii], digits[1], -1);
									} else if (form.equalsIgnoreCase("ESD")) {
//        				inputString = "F8,F8";
										linedata += formatStringFor(y[banknumber][ii], digits[0], -1);
										linedata += formatStringFor(Math.sqrt(esd[banknumber][ii]), digits[1], -1);
//                  System.out.println("ESD " + data[j] + " " + data[j + 1]);
									} else if (form.equalsIgnoreCase("ALT")) {
//        				inputString = "F8.0,F7.4,F5.4";
										linedata += x[banknumber][ii];
										linedata += formatStringFor(y[banknumber][ii], digits[1], 4);
										linedata += formatStringFor(Math.sqrt(esd[banknumber][ii]), digits[2], 4);
									}
									ii++;
								}
								if (linedata != null) {
									output.write(linedata);
									output.newLine();
								}
							}
						}

					}
				}
			}

		} catch (Exception e) {
			e.printStackTrace();
			System.out.println("Error in loading the data file! Try to remove this data file");
		}
		try {
			for (int i = filesNumber - 1; i >= 0; i--)
				readers[i].close();
		} catch (IOException e) {
		}
		try {
			output.flush();
			output.close();
		} catch (IOException io) {
			io.printStackTrace();
		}

		return finalName;
	}

	public static String formatStringFor(double value, int digits, int decimals) {
		if (decimals < 0) // integer
			return Misc.getIntStringFormatted((int) value, digits);
		else // float
			return Misc.getDoubleStringFormatted(value, digits, decimals);
	}
}
