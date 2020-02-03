/*
 * @(#)FluorescenceDataFileSpe.java created Feb 9, 2005 ITS, Riva del Garda
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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
 * The FluorescenceDataFileSpe is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:55 $
 * @since JDK1.1
 */

public class FluorescenceDataFileSpe extends it.unitn.ing.rista.diffr.DiffrDataFile {
  public FluorescenceDataFileSpe(XRDcat aobj, String alabel) {
    super(aobj, alabel);

    // user modification: put here the extension for the file format
    // you want to use. In this case this format is used when the
    // data file has the extension .dat

    identifier = ".spe";
  }

  public FluorescenceDataFileSpe() {
    // user modification: put here the extension for the file format
    // you want to use. In this case this format is used when the
    // data file has the extension .dat

    identifier = ".spe";
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

        energyDispersive = true;

        // read the entire first line and use it as title

//          title = reader.readLine();

        line = reader.readLine();    // read the second line
	      line = Misc.removeUTF8BOM(line);

// now the program read from the String line the values reported above

// initialize the StringTokenizer object from the String line

        st = new StringTokenizer(line, " ,\t\r\n");

// read from the StringTokenizer the first data (the number_of_data) and convert it
// in an integer to be putted in the variable datasetsNumber.

        token = st.nextToken();
        double omega = Double.parseDouble(token);
        datanumber = Integer.valueOf(token).intValue();

// read from the same StringTokenizer the next data etc.

        measurementstep = 1; // Double.valueOf(token = st.nextToken()).doubleValue();
        startingvalue = 1; // Double.valueOf(token = st.nextToken()).doubleValue();
//          radiation = Double.valueOf(token = st.nextToken()).doubleValue();

// in Java variable can be defined directly where you need them as here for
// columnnumber

        int columnnumber = 1; // Integer.valueOf(token = st.nextToken()).intValue();

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
