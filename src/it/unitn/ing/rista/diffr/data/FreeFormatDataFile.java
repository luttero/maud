/*
 * @(#)FreeFormatDataFile.java 03/07/2001 Casalino
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

package it.unitn.ing.rista.diffr.data;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Misc;

import java.io.*;
import java.lang.*;
import java.util.*;

/*
 * The FreeFormatDataFile is a class to load the free data format
 *
 * starting_angle step ending_angle
 * intensity
 * intensity
 * ......
 *
 * @version $Revision: 1.7 $, $Date: 2006/01/19 14:45:55 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class FreeFormatDataFile extends it.unitn.ing.rista.diffr.DiffrDataFile {

  public FreeFormatDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);

    identifier = ".rit";
  }

  public FreeFormatDataFile() {

    identifier = ".rit";
  }

  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = getReader();
    if (reader != null) {
      try {

// Variable definitions

        String token;
        String line;
        StringTokenizer st;

        dspacingbase = false;

        line = reader.readLine();    // read the first line

        st = new StringTokenizer(line, " ,\t\r\n");

// read from the same StringTokenizer the next data etc.

        startingvalue = Double.valueOf(token = st.nextToken()).doubleValue();
        measurementstep = Double.valueOf(token = st.nextToken()).doubleValue();
        double endingvalue = Double.valueOf(token = st.nextToken()).doubleValue();

        datanumber = (int) ((endingvalue - startingvalue + 0.00000001) / measurementstep + 1);

        initData(datanumber);        // initialize all the vectors specifying the
        // dimension equal to the number of Data
        // It is necessary before to load the intensities
        // and coordinates (2Theta or d-spacing)


// now we read the data, in this case one column (only intensities) or two columns
// (2Theta_or_d-spacing Intensity)

        constantstep = true;         // We assume a constant step
        int i = 0;
        while (i < datanumber) {
          line = reader.readLine();
          st = new StringTokenizer(line, " ,\t\r\n");

// only one column, the first, is used; the x value is computed from the
// starting value of 2Theta or d-spacing and from the measurement step
          while (st.hasMoreTokens()) {
            setXData(i, startingvalue + measurementstep * i);
            double intensityValue = Double.valueOf(st.nextToken()).doubleValue();
            if (intensityValue < 0.0) // we will not accept it, we suppose is an error
              intensityValue = 0.0;
            setYData(i, intensityValue);
            double tmpweight = Math.sqrt(intensityValue);
            if (tmpweight != 0.0)
              setWeight(i, 1.0 / tmpweight);
            else
              setWeight(i, 1.0);
            i++;
          }
        }
        loadSuccessfull = true;

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
