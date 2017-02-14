/*
 * @(#)PhilipsDataFile.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

/**
 * The PhilipsDataFile is an object to load Philips udf ascii datafiles.
 *
 * @version $Revision: 1.7 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class PhilipsDataFile extends it.unitn.ing.rista.diffr.DiffrDataFile {

  public PhilipsDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".udf";
  }

  public PhilipsDataFile() {
    identifier = ".udf";
  }

  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = getReader();
    if (reader != null) {
      try {

        dspacingbase = false;
        String token = new String("");
        while (!token.equals("RawScan")) {
          String linedata = reader.readLine();

//          System.out.println(linedata);
          StringTokenizer st = new StringTokenizer(linedata, " ,\t\r\n");

          while (st.hasMoreTokens()) {
            token = st.nextToken();
            if (token.equals("SampleIdent")) {
              token = st.nextToken();
              title = new String(token);
//          System.out.println(title);
            }
            if (token.equals("DataAngleRange")) {
              startingvalue = Double.valueOf(token = st.nextToken()).doubleValue();
//          System.out.println(startingvalue);
              finalvalue = Double.valueOf(token = st.nextToken()).doubleValue();
//          System.out.println(finalvalue);
            }
            if (token.equals("LabdaAlpha1")) {
              radiation = Double.valueOf(token = st.nextToken()).doubleValue();
            }
            if (token.equals("LambdaAlpha1")) {
              radiation = Double.valueOf(token = st.nextToken()).doubleValue();
            }
            if (token.equals("LabdaAlpha2")) {
//								wavelength2 = Double.valueOf(token = st.nextToken()).doubleValue();
            }
            if (token.equals("LambdaAlpha2")) {
//								wavelength2 = Double.valueOf(token = st.nextToken()).doubleValue();
            }
            if (token.equals("RatioAlpha21")) {
//								ratio21 = Double.valueOf(token = st.nextToken()).doubleValue();
            }
            if (token.equals("ScanStepSize"))
              measurementstep = Double.valueOf(token = st.nextToken()).doubleValue();
//          System.out.println(measurementstep);
//              if (token.equals("ScanStepTime"))
//								scansteptime = Double.valueOf(token = st.nextToken()).doubleValue();
          }
        }
//          System.out.println(startingvalue);
//          System.out.println(finalvalue);
//          System.out.println(measurementstep);
        datanumber = (int) ((finalvalue - startingvalue) / measurementstep + 1.000001);
        initData(datanumber);

        int i = 0;
        while (!token.equals("/") && i < datanumber) {
          String linedata = reader.readLine();
//          System.out.println(i);
//          System.out.println(linedata);
          if (linedata == null)
            break;
          StringTokenizer st = new StringTokenizer(linedata, " ,\t\r\n");

          while (st.hasMoreTokens()) {
            token = st.nextToken();
            if (!token.equals("/")) {
              setXData(i, startingvalue + i * measurementstep);
              double intensityValue = Double.valueOf(token).doubleValue();
              setYData(i, intensityValue);
              double tmpweight = Math.sqrt(intensity[i]);
              if (tmpweight != 0.0)
                setWeight(i, 1.0 / tmpweight);
              else
                setWeight(i, 1.0);
              i++;
            }
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
