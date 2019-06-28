/*
 * @(#)InelDataFile.java created 04/07/1998 Le Mans
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
 *  The InelDataFile is a class
 *
 *
 * @version $Revision: 1.9 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class InelDataFile extends MultDiffrDataFile {

  public static int actualnumberofdata = 4096;

  double twothetaShift = 0.0;

  public InelDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".inl";
  }

  public InelDataFile() {
    identifier = ".inl";
  }


  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = getReader();
    boolean askForSpectra = false;
    boolean atmpB = false;
    if (reader != null) {
      try {

        String token = new String("");
        StringTokenizer st = null;
        String linedata = null;
        while (!token.equals("points")) {
          linedata = reader.readLine();

          st = new StringTokenizer(linedata, " ,\t\r\n");

          while (st.hasMoreTokens() && !token.equals("points"))
            token = st.nextToken();
        }
        if (st.hasMoreTokens())
          token = st.nextToken();
        if (st.hasMoreTokens())
          actualnumberofdata = Integer.valueOf(token = st.nextToken()).intValue();

        while (!token.startsWith("*****")) {
          linedata = reader.readLine();

          st = new StringTokenizer(linedata, " ,\t\r\n");

          while (st.hasMoreTokens())
            token = st.nextToken();
        }

        boolean endoffile = false;
        linedata = reader.readLine();
        if (linedata == null) {
          endoffile = true;
        }

        int[] indexSkip = new int[3]; //getRangeSkipData(actualnumberofdata, askForSpectra);
        indexSkip[0] = 0;
        indexSkip[1] = actualnumberofdata;
        indexSkip[2] = 0;

        DiffrDataFile fakedatafile = null;
        if (indexSkip[2] > 0) {
          fakedatafile = new DiffrDataFile();
          fakedatafile.initXRD();
        }

        int toSkip = 0;

        while (!endoffile) {
          st = new StringTokenizer(linedata, " ,\t\r\n");

          while (st.hasMoreTokens() && !token.equals(":"))
            token = st.nextToken();
          if (st.hasMoreTokens())
            token = st.nextToken();

//        	System.out.println("Reading spectrum number: " + token);
          DiffrDataFile datafile = null;
          toSkip++;

          if (toSkip == indexSkip[2]) {
            datafile = fakedatafile;
            toSkip = 0;
          } else {
            datafile = addDiffrDatafile(token);
            atmpB = datafile.isAbilitatetoRefresh;
            datafile.isAbilitatetoRefresh = false;
          }

          linedata = reader.readLine();
          if (linedata == null) {
            endoffile = true;
            break;
          }


/*

Date: Fri, 31 Jul 1998 21:24:19 +0200
To: Luca.Lutterotti@ing.unitn.it
From: Daniel Chateigner <daniel.chateigner@univ-lemans.fr>

Hi Luca,
I hope your travel back gone well.
>From a scan I have here, the angle positions in the TTX files are:
?  theta  2theta  chi  ?  ?  ?
I'll fix the phi, I think it is in front

Date: Mon, 09 Nov 1998 14:40:09 +0100
To: Luca.Lutterotti@ing.unitn.it
From: Daniel Chateigner <daniel.chateigner@univ-lemans.fr>

Dear Luca,
Hope all is going fine in Berkeley, and that your flight went good.
Just a note:
in the ttx files, data are ranged:
phi  theta  2theta   chi   X   Y    Z
X Y and Z are just trnslation stages, if available.
best thoughts

*/

          st = new StringTokenizer(linedata, " ,\t\r\n");
          if (st.hasMoreTokens()) {
            twothetaShift = Double.valueOf(token = st.nextToken()).doubleValue();
          }
          if (st.hasMoreTokens()) {
            double omega_angle = Double.valueOf(token = st.nextToken()).doubleValue();
            datafile.setAngleValue(0, omega_angle);
          }

          if (st.hasMoreTokens()) {
            double chi_angle = Double.valueOf(token = st.nextToken()).doubleValue();
            datafile.setAngleValue(1, chi_angle);
          }
          if (st.hasMoreTokens())
            token = st.nextToken();  // X
          if (st.hasMoreTokens())
            token = st.nextToken();  // Y
          if (st.hasMoreTokens())
            token = st.nextToken();  // Z
          if (st.hasMoreTokens())
            datafile.setString(3, token = st.nextToken());  // Phi

          datafile.dspacingbase = false;
          datafile.constantstep = false;

          linedata = reader.readLine();
          if (linedata == null) {
            endoffile = true;
            break;
          }
          System.out.println(linedata);

          datafile.initData(indexSkip[1] - indexSkip[0]);

          int i = 0;
          linedata = reader.readLine();
          if (linedata == null) {
            endoffile = true;
            datafile.isAbilitatetoRefresh = atmpB;
            datafile.dataLoaded = true;
            break;
          }
          st = new StringTokenizer(linedata, " ,\t\r\n");
          token = st.nextToken();
          double twotheta = 0.0;
          boolean uncalibrated = false;
          while (!token.startsWith("*****") && !endoffile) {
            twotheta = Double.valueOf(token).doubleValue();
            if (i == 0 && twotheta == 1.0)
              uncalibrated = true;
            token = st.nextToken();
            if (i >= indexSkip[0] && i < indexSkip[1]) {
              if (uncalibrated)
                datafile.setXData(i, twotheta);
              else {
                datafile.setXData(i, twotheta + twothetaShift);
                System.out.println(twotheta + twothetaShift);
              }
              datafile.setYData(i, Double.valueOf(token).doubleValue());
              double tmpweight = Math.sqrt(datafile.getYData(i));
              if (tmpweight != 0.0)
                datafile.setWeight(i, 1.0 / tmpweight);
              else
                datafile.setWeight(i, 0.0);  // with high intensity inel set intensity to zero!!
            }
            i++;
            linedata = reader.readLine();
            if (linedata == null) {
              endoffile = true;
              datafile.isAbilitatetoRefresh = atmpB;
              datafile.dataLoaded = true;
              break;
            }
            st = new StringTokenizer(linedata, " ,\t\r\n");
            token = st.nextToken();
          }
          datafile.isAbilitatetoRefresh = atmpB;
          datafile.dataLoaded = true;
          loadSuccessfull = true;
          linedata = reader.readLine();
          if (linedata == null)
            endoffile = true;
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

  public void setXData(int index, double value) {
    super.setXData(index, value);
//		originalNotCalibrated = true;
  }

  public void setCalibratedXDataOnly(int index, double value) {
    setCalibrated(true);
    twothetacalibrated[index] = (double) (value + twothetaShift);
  }

}
