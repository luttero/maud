/*
 * @(#)xyDataFile.java created 18/03/1999 Mesiano
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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
 * The xyDataFile is a class to load datafile
 * consisting of only two columns, one with the two-theta
 * and the other with the intensity.
 *
 *
 * @version $Revision: 1.8 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class xyDataFile extends it.unitn.ing.rista.diffr.DiffrDataFile {

  public xyDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".xy";
  }

  public xyDataFile() {
    identifier = ".xy";
  }

  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = getReader();
    if (reader != null) {
      try {

        dspacingbase = false;
        Vector x = new Vector(100, 100);
        Vector y = new Vector(100, 100);

        datanumber = 0;

        String token1 = new String("");
        String token2 = new String("");

        String linedata = reader.readLine();
	      linedata = Misc.removeUTF8BOM(linedata);

        while (linedata.startsWith("#"))
          linedata = reader.readLine();
//              System.out.println(linedata);

        while (linedata != null) {

          StringTokenizer st = new StringTokenizer(linedata, "' ,\t\r\n");

          try {
            if (st.hasMoreTokens()) {
              token1 = st.nextToken();
              if (st.hasMoreTokens()) {
                token2 = st.nextToken();

//              System.out.println(token1 + " " + token2);
                Double n1 = Double.valueOf(token1);
                Double n2 = Double.valueOf(token2);
                datanumber++;
                x.addElement(n1);
                y.addElement(n2);
              }
            }
          } catch (Exception ge) {
// not numbers, we don't store them
//              System.out.println("not a number: " + linedata);
          }

          linedata = reader.readLine();
        }

        initData(datanumber);

        for (int i = 0; i < datanumber; i++) {
          setXData(i, ((Double) x.elementAt(i)).doubleValue());
          double intensityValue = ((Double) y.elementAt(i)).doubleValue();
//          if (intensityValue < 0.0) // we will not accept it, we suppose is an error
//            intensityValue = 0.0;
          setYData(i, intensityValue);
          double tmpweight = Math.sqrt(Math.abs(intensityValue));
          if (tmpweight != 0.0)
            setWeight(i, 1.0 / tmpweight);
          else
            setWeight(i, 1.0);
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
