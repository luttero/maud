/*
 * @(#)SiemensDataFile.java created 10/12/1998 Mesiano
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.data;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Misc;

import java.io.*;
import java.lang.*;
import java.util.*;

/**
 *  The SiemensDataFile is a class
 *
 *
 * @version $Revision: 1.10 $, $Date: 2006/02/02 16:11:56 $
 * @author Fabio Boscolo
 * @since JDK1.1
 */

public class SiemensDataFile extends it.unitn.ing.rista.diffr.MultDiffrDataFile {

  public SiemensDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".uxd";
  }

  public SiemensDataFile() {
    identifier = ".uxd";
  }

  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = getReader();
    if (reader != null) {
      try {

        int spectrumNumber = 0;
        dspacingbase = false;
        String token = " ";
        boolean endoffile = false;
        String linedata = " ";

        String timeStep = "1.0";
        while (linedata != null) {
          String spectrumName = Integer.toString(++spectrumNumber);
          DiffrDataFile datafile = addDiffrDatafile(spectrumName);
          boolean atmpB = datafile.isAbilitatetoRefresh;
          datafile.isAbilitatetoRefresh = false;
          datafile.dspacingbase = dspacingbase;
          datanumber = 0;
          StringTokenizer st = null;
          token = " ";
          while (!token.equalsIgnoreCase("_CPS") &&
              !token.equalsIgnoreCase("_COUNTS") && !token.equalsIgnoreCase("_2THETACOUNTS")) {
            linedata = reader.readLine();
	          linedata = Misc.removeUTF8BOM(linedata);
            if (linedata != null) {
              st = new StringTokenizer(linedata, " ,=\t\r\n");
            } else
              break;

            while (st.hasMoreTokens() &&
                    (!token.equalsIgnoreCase("_CPS") && !token.equalsIgnoreCase("_COUNTS") &&
                        !token.equalsIgnoreCase("_2THETACOUNTS"))) {
              token = st.nextToken();
              if (token.equalsIgnoreCase("_SAMPLE") && st.hasMoreTokens())
                datafile.title = st.nextToken();
              if (token.equalsIgnoreCase("_START") && st.hasMoreTokens())
                startingvalue = Double.valueOf(token = st.nextToken()).doubleValue();

//              if (token.equalsIgnoreCase("_WL1"))
//								radiation = Double.valueOf(token = st.nextToken()).doubleValue();

//              if (token.equalsIgnoreCase("_WL2"))
//								wavelength2 = Double.valueOf(token = st.nextToken()).doubleValue();

//              if (token.equalsIgnoreCase("_WLRATIO"))
//								ratio21 = Double.valueOf(token = st.nextToken()).doubleValue();
              if (token.equalsIgnoreCase("_STEPSIZE"))
                measurementstep = Double.valueOf(token = st.nextToken()).doubleValue();

              if (token.equalsIgnoreCase("_STEPCOUNT"))
                datanumber = Integer.valueOf(token = st.nextToken()).intValue();
//              if (token.equals("_STEPTIME"))
//								scansteptime = Double.valueOf(token = st.nextToken()).doubleValue();
              if (token.equals("_KHI"))
                datafile.setAngleValue(1, Double.valueOf(token = st.nextToken()).doubleValue());
              if (token.equals("_PHI"))
                datafile.setAngleValue(2, Double.valueOf(token = st.nextToken()).doubleValue());
	            if (token.equals("_THETA"))
		            datafile.setAngleValue(0, Double.valueOf(token = st.nextToken()).doubleValue());
              if (token.equalsIgnoreCase("_STEPTIME")) {
                timeStep = st.nextToken();
              }
            }
          }

          int formatnumber = 0;
          if (token.equalsIgnoreCase("_2THETACOUNTS"))
            formatnumber = 2;
          else
            formatnumber = 1;
          if (token.equalsIgnoreCase("_CPS")) {
            datafile.setCPSmeasurement();
            datafile.setCountTime(timeStep);
          }
//          System.out.println(formatnumber + " " + datanumber);

          if (datanumber != 0) {
            datafile.initData(datanumber);

            int i = 0;
            while (i < datanumber) {
              linedata = reader.readLine();
              if (linedata == null || (linedata.startsWith("_")))
                break;
              if (linedata.startsWith(";"))
                linedata = reader.readLine();
              if (linedata == null || linedata.startsWith("_"))
                break;

//              System.out.println(linedata);


              st = new StringTokenizer(linedata, " ,\t\r\n");

              if (formatnumber == 2 && i < datanumber) {
                while (st.hasMoreTokens()) {
                  datafile.setXData(i, Double.valueOf(st.nextToken()).doubleValue());
                  double intensityValue = Double.valueOf(st.nextToken()).doubleValue();
                  datafile.setYData(i, intensityValue);
                  double tmpweight = Math.sqrt(datafile.getYData(i));
                  if (tmpweight != 0.0)
                    datafile.setWeight(i, 1.0 / tmpweight);
                  else
                    datafile.setWeight(i, 1.0);
                  i++;
                }
              } else {
                while (st.hasMoreTokens() && i < datanumber) {
                  datafile.setXData(i, startingvalue + i * measurementstep);
                  double intensityValue = Double.valueOf(st.nextToken()).doubleValue();
                  datafile.setYData(i, intensityValue);
                  double tmpweight = Math.sqrt(datafile.getYData(i));
                  if (tmpweight != 0.0)
                    datafile.setWeight(i, 1.0 / tmpweight);
                  else
                    datafile.setWeight(i, 1.0);
                  i++;
                }
              }
            }
          } else {
            linedata = reader.readLine();

            Vector x = new Vector(100, 100);
            Vector y = new Vector(100, 100);
            String token1 = null;
            String token2 = null;

            while (linedata != null && !linedata.startsWith("_")) {
              if (linedata.startsWith(";"))
                linedata = reader.readLine();
              if (linedata == null || linedata.startsWith("_"))
                break;

              st = new StringTokenizer(linedata, " ,\t\r\n");

              if (formatnumber == 2) {
                while (st.hasMoreTokens()) {
                  token1 = st.nextToken();
                  token2 = st.nextToken();
                  datanumber++;
                  x.addElement(token1);
                  y.addElement(token2);
                }
              } else {
                while (st.hasMoreTokens()) {
                  token1 = st.nextToken();
                  datanumber++;
                  y.addElement(token1);
                }
              }
              linedata = reader.readLine();
//								System.out.println(linedata);
            }

            datafile.initData(datanumber);

            int i = 0;
            while (i < datanumber) {
              if (formatnumber == 2) {
                token1 = (String) x.elementAt(i);
                token2 = (String) y.elementAt(i);
//								System.out.println(i + " " + token1 + ", " + token2);
                datafile.setXData(i, Double.valueOf(token1).doubleValue());
                double intensityValue = Double.valueOf(token2).doubleValue();
                datafile.setYData(i, intensityValue);
                double tmpweight = Math.sqrt(datafile.getYData(i));
                if (tmpweight != 0.0)
                  datafile.setWeight(i, 1.0 / tmpweight);
                else
                  datafile.setWeight(i, 1.0);
                i++;
              } else {
                datafile.setXData(i, startingvalue + i * measurementstep);
                token2 = (String) y.elementAt(i);
                double intensityValue = Double.valueOf(token2).doubleValue();
                datafile.setYData(i, intensityValue);
                double tmpweight = Math.sqrt(datafile.getYData(i));
                if (tmpweight != 0.0)
                  datafile.setWeight(i, 1.0 / tmpweight);
                else
                  datafile.setWeight(i, 1.0);
                i++;
              }
            }
          }
          loadSuccessfull = true;
          datafile.isAbilitatetoRefresh = atmpB;

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
}
