/*
 * @(#)FullProfDataFile.java created 30/3/2000 Le Mans
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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
 * The FullProfDataFile is an object to load Philips udf ascii datafiles.
 *
 * @version $Revision: 1.7 $, $Date: 2006/01/19 14:45:55 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class FullProfDataFile extends it.unitn.ing.rista.diffr.DiffrDataFile {

  public FullProfDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".fpr";
  }

  public FullProfDataFile() {
    identifier = ".fpr";
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

        String line = reader.readLine();  // check new format where there are two more lines at the beginning

        StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");

        double startingvalue = Double.valueOf(token = st.nextToken()).doubleValue();
        double measurementstep = Double.valueOf(token = st.nextToken()).doubleValue();
        double finalvalue = Double.valueOf(token = st.nextToken()).doubleValue();

        datanumber = (int) ((finalvalue - startingvalue) / measurementstep) + 1;

        initData(datanumber);

        for (int i = 0; i < datanumber;) {
          String linedata = reader.readLine();
          if (linedata == null)
            break;
/*          st = new StringTokenizer(linedata, " ,\t\r\n");

          while (st.hasMoreTokens()) {
            token = st.nextToken();
            setCalibratedXData(i, startingvalue + i * measurementstep);
            double intensityValue = Double.valueOf(token).doubleValue();
            if (intensityValue < 0.0) // we will not accept it, we suppose is an error
              intensityValue = 0.0;
            setYData(i, intensityValue);
            double tmpweight = Math.sqrt(intensityValue);
            if (tmpweight != 0.0)
              setWeight(i, 1.0 / tmpweight);
            else
              setWeight(i, 1.0);
            i++;
          }*/
          for (int j = 0; j < 10; j++) {
            if (j * 8 + 4 > linedata.length())
              break;
            String next = linedata.substring(j * 8 + 2, (j + 1) * 8);
//            System.out.println(next);
            if (next != null) {
              setXData(i, startingvalue + i * measurementstep);
              double intensityValue = Double.valueOf(Misc.toStringDeleteBlankAndTab(next)).doubleValue();
//              System.out.println(i + " " + intensityValue);
              if (intensityValue < 0.0) // we will not accept it, we suppose is an error
                intensityValue = 0.0;
              setYData(i, intensityValue);
              double tmpweight = Math.sqrt(intensityValue);
              if (tmpweight != 0.0)
                setWeight(i, 1.0 / tmpweight);
              else
                setWeight(i, 1.0);
              i++;
            } else
              break;
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
