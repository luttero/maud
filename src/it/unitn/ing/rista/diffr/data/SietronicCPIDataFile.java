/*
 * @(#)SietronicCPIDataFile.java created Oct 16, 2003 Casalino
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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
import java.util.StringTokenizer;


/**
 * The SietronicCPIDataFile is a class
 *  
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class SietronicCPIDataFile extends it.unitn.ing.rista.diffr.DiffrDataFile {

  public SietronicCPIDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".cpi";
  }

  public SietronicCPIDataFile() {
    identifier = ".cpi";
  }

  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = getReader();
    if (reader != null) {
      try {

// Variable definitions

//        String token;
        String line;
        StringTokenizer st;

// user modification: do not modify before this line
// we read the Venezia format

// the data is in 2Theta

        dspacingbase = false;

        // read the entire first line and use it as title

        title = reader.readLine();
	      title = Misc.removeUTF8BOM(title);
	      if (title.startsWith("SIETRONICS")) {

          st = new StringTokenizer(reader.readLine(), " ,\t\r\n");
          startingvalue = Double.valueOf(st.nextToken()).doubleValue();

// read from the same StringTokenizer the next data etc.

          st = new StringTokenizer(reader.readLine(), " ,\t\r\n");
          double endingvalue = Double.valueOf(st.nextToken()).doubleValue();
          st = new StringTokenizer(reader.readLine(), " ,\t\r\n");
          measurementstep = Double.valueOf(st.nextToken()).doubleValue();
          datanumber = (int) ((endingvalue + 0.000001 - startingvalue) / measurementstep);
//        radiation = Double.valueOf(token = st.nextToken()).doubleValue();

// in Java variable can be defined directly where you need them as here for
// columnnumber

          for (int i = 0; i < 6; i++) {
            line = reader.readLine();
            if (i == 2)
              title = title + " " + line;
          }
          initData(datanumber);        // initialize all the vectors specifying the
          // dimension equal to the number of Data
          // It is necessary before to load the intensities
          // and coordinates (2Theta or d-spacing)


// now we read the data, in this case one column (only intensities) or two columns
// (2Theta_or_d-spacing Intensity)

          int i = 0;
          constantstep = true;         // We assume a constant step

          while (i < datanumber) {
            line = reader.readLine();
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
          }
          loadSuccessfull = true;

// user modification: do not modify after this line
        } else {
          System.out.println("Error! This datafile ending in .cpi is not a SIETRONIC CPI datafile.");
          System.out.println("Remove it and change the extension to the appropriate one!");
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
