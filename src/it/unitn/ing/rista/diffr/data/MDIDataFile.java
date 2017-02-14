/*
 * @(#)MDIDataFile.java created May 20, 2008 Caen
 *
 * Copyright (c) 2008 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.util.Misc;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.Vector;
import java.util.StringTokenizer;

/**
 * The MDIDataFile is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: May 20, 2008 8:05:50 AM $
 * @since JDK1.1
 */
public class MDIDataFile extends it.unitn.ing.rista.diffr.DiffrDataFile {

  public MDIDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".mdi";
  }

  public MDIDataFile() {
    identifier = ".mdi";
  }

  public boolean readallSpectra() {


    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = getReader();
    double timeStep; // todo
    if (reader != null) {
      try {

// Variable definitions

        String token;
        String line;
        StringTokenizer st;

// user modification: do not modify before this line
// we read the Venezia format

// the data is in 2Theta

        dspacingbase = false;

        // read the entire first line and use it as title

        title = reader.readLine();

        line = reader.readLine();    // read the second line

// now the program read from the String line the values reported above

// initialize the StringTokenizer object from the String line

        st = new StringTokenizer(line, " ,\t\r\n");

// read from the same StringTokenizer the next data etc.

        startingvalue = Double.valueOf(token = st.nextToken()).doubleValue();
        measurementstep = Double.valueOf(token = st.nextToken()).doubleValue();
        timeStep = Double.valueOf(token = st.nextToken()).doubleValue();
        String tubeTarget = st.nextToken();
        radiation = Double.valueOf(token = st.nextToken()).doubleValue();
        finalvalue = Double.valueOf(token = st.nextToken()).doubleValue();

        datanumber = Integer.valueOf(token = st.nextToken()).intValue();
        initData(datanumber);        // initialize all the vectors specifying the
        // dimension equal to the number of Data
        // It is necessary before to load the intensities
        // and coordinates (2Theta or d-spacing)


// now we read the data, in this case one column (only intensities) or two columns
// (2Theta_or_d-spacing Intensity)

        int i;
        constantstep = true;         // We assume a constant step
        i = 0;
        line = reader.readLine();
        while (i < datanumber) {
          st = new StringTokenizer(line, " ,\t\r\n");

// only one column, the first, is used; the x value is computed from the
// starting value of 2Theta or d-spacing and from the measurement step
          while (st.hasMoreTokens()) {
            setXData(i, startingvalue + measurementstep * i);
            double intensityValue = Double.valueOf(st.nextToken()).doubleValue();
            setYData(i, intensityValue);
            double tmpweight = Math.sqrt(intensity[i]);
            if (tmpweight != 0.0)
              setWeight(i, 1.0 / tmpweight);
            else
              setWeight(i, 1.0);
            i++;
          }
          line = reader.readLine();
          if (line == null)
            break;
        }
        loadSuccessfull = true;

// user modification: do not modify after this line

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
