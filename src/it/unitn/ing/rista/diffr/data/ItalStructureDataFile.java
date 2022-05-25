/*
 * @(#)ItalStructureDataFile.java 29/12/1998 Mesiano
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

// the package name

package it.unitn.ing.rista.diffr.data;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Misc;

import java.io.*;
import java.lang.*;
import java.util.*;

/*
 * The ItalStructureDataFile is a class to load the Ital Structure data format
 * @version $Revision: 1.7 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class ItalStructureDataFile extends it.unitn.ing.rista.diffr.DiffrDataFile {

  public ItalStructureDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);

    identifier = ".its";
  }

  public ItalStructureDataFile() {
    identifier = ".its";
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

// user modification: do not modify before this line
// we read the Venezia format

// the data is in 2Theta

        dspacingbase = false;

        // read the entire first line and use it as title

        title = reader.readLine();
	      title = Misc.removeUTF8BOM(title);

        line = reader.readLine();    // read the second line

// now the program read from the String line the values reported above

// initialize the StringTokenizer object from the String line

        st = new StringTokenizer(line, " ,\t\r\n");

// read from the StringTokenizer the first data (the number_of_data) and convert it
// in an integer to be putted in the variable datasetsNumber.

        token = st.nextToken();
        datanumber = Integer.valueOf(token).intValue();

// read from the same StringTokenizer the next data etc.

        line = reader.readLine();    // read the second line
        st = new StringTokenizer(line, " ,\t\r\n");
        startingvalue = Double.valueOf(token = st.nextToken()).doubleValue();
        double finalvalue = Double.valueOf(token = st.nextToken()).doubleValue();
        measurementstep = Double.valueOf(token = st.nextToken()).doubleValue();

// in Java variable can be defined directly where you need them as here for
// columnnumber

        initData(datanumber);        // initialize all the vectors specifying the
        // dimension equal to the number of Data
        // It is necessary before to load the intensities
        // and coordinates (2Theta or d-spacing)


// now we read the data, in this case one column (only intensities) or two columns
// (2Theta_or_d-spacing Intensity)

        int i;
        constantstep = true;         // We assume a constant step
        i = 0;
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
