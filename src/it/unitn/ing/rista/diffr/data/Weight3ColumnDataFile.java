/*
 * @(#)ETHThreeColumnDataFile.java created 19/10/2006 Peschiera (Vr-Mi)
 *
 * Copyright (c) 2006 Luca Lutterotti All Rights Reserved.
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
 * The Weight3ColumnDataFile is a class to load datafile
 * consisting of only three columns, one with the two-theta
 * and the others with the intensity and the weight.
 *
 *
 * @version $Revision: 1.1 $, $Date: 2006/11/10 17:08:24 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Weight3ColumnDataFile extends it.unitn.ing.rista.diffr.DiffrDataFile {

  public Weight3ColumnDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".inp";
  }

  public Weight3ColumnDataFile() {
    identifier = ".inp";
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
        Vector w = new Vector(100, 100);

        datanumber = 0;

        String token1 = new String("");
        String token2 = new String("");
        String token3 = new String("");

        String linedata = reader.readLine();

        while (linedata != null) {

          StringTokenizer st = new StringTokenizer(linedata, "' ,\t\r\n");

          if (st.hasMoreTokens())
            token1 = st.nextToken();
          if (st.hasMoreTokens())
            token2 = st.nextToken();
          if (st.hasMoreTokens())
            token3 = st.nextToken();

          datanumber++;
          x.addElement(token1);
          y.addElement(token2);
          w.addElement(token3);

          linedata = reader.readLine();
        }

        initData(datanumber);

        for (int i = 0; i < datanumber; i++) {
          token1 = (String) x.elementAt(i);
          token2 = (String) y.elementAt(i);
          token3 = (String) w.elementAt(i);
          setXData(i, Double.valueOf(token1).doubleValue());
          double intensityValue = Double.valueOf(token2).doubleValue();
          setYData(i, intensityValue);
          double tmpweight = Double.valueOf(token3).doubleValue();
          setWeight(i, tmpweight);
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
