/*
 * @(#)Lpec1DataFile.java created 13/03/2000 Le Mans
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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
 *  The Lpec1DataFile is a class
 *
 *
 * @version $Revision: 1.9 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Lpec1DataFile extends MultDiffrDataFile {

  public Lpec1DataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".lp1";
  }

  public Lpec1DataFile() {
    identifier = ".lp1";
  }


  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = getReader();
    if (reader != null) {
      try {

        int datafilesNumber = 0, datafilesDataNumber = 0;
        double startingAngle = 0.0, stepOfMeasurement = 0.0;

        String token = new String("");
        StringTokenizer st = null;
        String linedata = null;

        linedata = reader.readLine();
	      linedata = Misc.removeUTF8BOM(linedata);

        st = new StringTokenizer(linedata, " ,\t\r\n");

        if (st.hasMoreTokens())
          datafilesNumber = Integer.valueOf(token = st.nextToken()).intValue();
        if (st.hasMoreTokens())
          datafilesDataNumber = Integer.valueOf(token = st.nextToken()).intValue();

        if (st.hasMoreTokens())
          startingAngle = Double.valueOf(token = st.nextToken()).doubleValue();
        if (st.hasMoreTokens())
          stepOfMeasurement = Double.valueOf(token = st.nextToken()).doubleValue();

        boolean endoffile = false;
        linedata = reader.readLine();
        if (linedata == null) {
          endoffile = true;
        } else
          st = new StringTokenizer(linedata, " ,\t\r\n");

        for (int nd = 0; nd < datafilesNumber && !endoffile; nd++) {

          DiffrDataFile datafile = addDiffrDatafile(Integer.toString(nd));
          boolean atmpB = datafile.isAbilitatetoRefresh;
          datafile.isAbilitatetoRefresh = false;

          datafile.dspacingbase = false;
          datafile.constantstep = true;

          datafile.initData(datafilesDataNumber);

          int i = 0;
          while (i < datafilesDataNumber && !endoffile) {

            while (st.hasMoreTokens() && !endoffile) {
              token = st.nextToken();
              datafile.setXData(i, startingAngle + i * stepOfMeasurement);
              datafile.setYData(i, Double.valueOf(token).doubleValue());
              double tmpweight = Math.sqrt(datafile.getYData(i));
              if (tmpweight != 0.0)
                datafile.setWeight(i, 1.0 / tmpweight);
              else
                datafile.setWeight(i, 1.0);
              i++;
            }
            linedata = reader.readLine();
            if (linedata == null) {
              endoffile = true;
              datafile.isAbilitatetoRefresh = atmpB;
              datafile.dataLoaded = true;
              break;
            }
            st = new StringTokenizer(linedata, " ,\t\r\n");
          }
          datafile.isAbilitatetoRefresh = atmpB;
          datafile.dataLoaded = true;
          loadSuccessfull = true;
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
