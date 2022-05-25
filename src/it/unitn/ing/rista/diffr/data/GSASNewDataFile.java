/*
 * @(#)GSASNewDataFile.java created 06/06/2002 Mesiano
 *
 * Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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

import java.io.*;
import java.util.*;

import it.unitn.ing.rista.util.*;


/**
 *  The GSASNewDataFile is a class
 *  NOT USED
 *
 * @version $Revision: 1.9 $, $Date: 2006/01/19 14:45:55 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GSASNewDataFile extends MultDiffrDataFile {

  public GSASNewDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".gda";
  }

  public GSASNewDataFile() {
    identifier = ".gda";
  }


  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = getReader();
    if (reader != null) {
      try {
        boolean timeMap = false;
        int tmap[][] = null;
        int mapno = 0, nvals = 0, nrec = 0, tmax = 0;
        double clckwdt = 0.0;

        String token = new String("");
        StringTokenizer st = null;
        String linedata = null;
        boolean endoffile = false;

        String titleString = reader.readLine();
	      titleString = Misc.removeUTF8BOM(titleString);
        double omega = 0.0, chi = 0.0, phi = 0.0;


        st = new StringTokenizer(titleString, " ,\t\r\n");
        token = st.nextToken();
        if (!token.equalsIgnoreCase("BANK") && !token.startsWith("TIME_MAP")) {
          while (st.hasMoreTokens()) {
            token = st.nextToken();
            if (token.equalsIgnoreCase("Omega")) {
              token = st.nextToken();
              token = st.nextToken();
              omega = Double.valueOf(token).doubleValue() / 10.0;
            } else if (token.equalsIgnoreCase("Chi")) {
              token = st.nextToken();
              token = st.nextToken();
              chi = Double.valueOf(token).doubleValue() / 10.0;
            } else if (token.equalsIgnoreCase("Phi")) {
              token = st.nextToken();
              token = st.nextToken();
              phi = Double.valueOf(token).doubleValue() / 10.0;
            }
          }
        }

        while (!endoffile) {
          while (!token.equalsIgnoreCase("BANK") && !token.startsWith("TIME_MAP")) {
            linedata = reader.readLine();
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
            timeMap = true;
            if (token.length() == 8)
              mapno = Integer.valueOf(token = st.nextToken()).intValue();
            else
              mapno = Integer.valueOf(token.substring(8)).intValue();
            nvals = (Integer.valueOf(token = st.nextToken()).intValue() - 1) / 3;
            nrec = Integer.valueOf(token = st.nextToken()).intValue();
            token = st.nextToken();
            clckwdt = Double.valueOf(token = st.nextToken()).doubleValue();

            linedata = reader.readLine();
            st = new StringTokenizer(linedata, " ,\t\r\n");
            tmap = new int[3][nvals];
            for (int i = 0; i < nvals; i++) {
              for (int j = 0; j < 3; j++) {
                if (!st.hasMoreTokens()) {
                  linedata = reader.readLine();
                  st = new StringTokenizer(linedata, " ,\t\r\n");
                }
                tmap[j][i] = Integer.valueOf(token = st.nextToken()).intValue();
//          			System.out.println(tmap[j][i]);
              }
            }
            if (!st.hasMoreTokens()) {
              linedata = reader.readLine();
              st = new StringTokenizer(linedata, " ,\t\r\n");
            }
            tmax = Integer.valueOf(token = st.nextToken()).intValue();
          } else {
            int mapNumber = 0;
            int banknumber = Integer.valueOf(token = st.nextToken()).intValue();
            DataFileSet dataset = (DataFileSet) getParent();

//        	DataCalibration gsascal = getCalibration("bank", 6);

//        	System.out.println("Reading bank number: " + token);

            DiffrDataFile datafile = addDiffrDatafile(token);
            boolean atmpB = datafile.isAbilitatetoRefresh;
            datafile.isAbilitatetoRefresh = false;

            datafile.title = new String(titleString);

            datafile.setAngleValue(0, omega);
            datafile.setAngleValue(1, chi);
            datafile.setAngleValue(2, phi);

            int nchannel = Integer.valueOf(token = st.nextToken()).intValue();
            datafile.initData(nchannel);

            nrec = Integer.valueOf(token = st.nextToken()).intValue();
            String bintyp = st.nextToken();
            String form = "STD";
            double bcoeff[] = new double[4];
            int numbercoeff = 0;
            if (bintyp.startsWith("TIME_MAP")) {
              form = "TIME_MAP";
              mapNumber = Integer.valueOf(token = st.nextToken()).intValue() - 1;
            } else {
              for (int j = 0; j < 4; j++)
                if (st.hasMoreTokens()) {
                  bcoeff[j] = Double.valueOf(token = st.nextToken()).doubleValue();
                  numbercoeff++;
                }
            }
            if (st.hasMoreTokens()) {
              form = st.nextToken();
              System.out.println("Form " + form);
            }

            System.out.println("Form final " + form);

            datafile.dspacingbase = true;
            datafile.constantstep = false;
            datafile.datanumber = nchannel;
            if (bintyp.equalsIgnoreCase("lpsd")) {
              datafile.dspacingbase = false;
              datafile.constantstep = true;
            }

            int i = 0;
            int tmap_index = 0;
            while (i < nchannel) {
              linedata = reader.readLine();
              if (linedata == null) {
                endoffile = true;
                datafile.isAbilitatetoRefresh = atmpB;
                datafile.dataLoaded = true;
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
                    nctr = Integer.valueOf(data[j]).intValue();
                    if (nctr <= 0)
                      nctr = 1;
                  }
                  token = data[j + 1];
                } else if (form.equalsIgnoreCase("ESD")) {
                  token = data[j];
                  esd = Double.valueOf(data[j + 1]).doubleValue();
//                  System.out.println("ESD " + data[j] + " " + data[j + 1]);
                } else if (form.equalsIgnoreCase("ALT")) {
                  x = Double.valueOf(data[j]).doubleValue();
                  token = data[j + 1];
                  esd = Double.valueOf(data[j + 2]).doubleValue();
                }

                if (bintyp.equalsIgnoreCase("RALF"))
                  datafile.setXData(i, x / 32);
                else if (bintyp.equalsIgnoreCase("const"))
                  datafile.setXData(i, bcoeff[0] + bcoeff[1] * i);
                else if (bintyp.equalsIgnoreCase("lpsd"))
                  datafile.setXData(i, (bcoeff[0] + bcoeff[2] * (i - bcoeff[1])) / 100.0);
                else if (bintyp.equalsIgnoreCase("log6"))
                  datafile.setXData(i, bcoeff[0] + Math.log(bcoeff[1]) * i);
                else if (bintyp.startsWith("TIME_MAP")) {
                  if (tmap_index < nvals - 1 && i + 1 >= tmap[0][tmap_index + 1])
                    tmap_index++;
                  double tof = (tmap[1][tmap_index] + (i - tmap[0][tmap_index] + 1) *
                          tmap[2][tmap_index]) * clckwdt / 1000;
                  datafile.setXData(i, tof);
                } else
                  datafile.setXData(i, bcoeff[0] + bcoeff[1] * i);

//      System.out.println( bcoeff[0] + bcoeff[1] * i );
//      System.out.println( token );
                datafile.setYData(i, Double.valueOf(token).doubleValue() * nctr);
                double tmpweight = 0.0;
                if (form.equalsIgnoreCase("ESD")) {
                  tmpweight = esd;
                } else {
                  tmpweight = Math.sqrt(datafile.getYData(i));
                  if (tmpweight != 0.0)
                    tmpweight = 1.0 / tmpweight;
                  else
                    tmpweight = 1.0;
                }

                datafile.setWeight(i, tmpweight);

                i++;
              }
            }
            datafile.isAbilitatetoRefresh = atmpB;
            datafile.dataLoaded = true;
            loadSuccessfull = true;
//          gsascal.calibrate(this);
          }
        }

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

}
