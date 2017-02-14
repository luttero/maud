/*
 * @(#)TitledDoubleColumnDatafile.java created 05/03/2002 Mesiano
 *
 * Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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
 * The TitledDoubleColumnDatafile is a class to load datafile
 * consisting of only two columns, one with the two-theta
 * and the other with the intensity and a title at the beginning.
 *
 *
 * @version $Revision: 1.8 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class TitledDoubleColumnDatafile extends it.unitn.ing.rista.diffr.DiffrDataFile {

  public TitledDoubleColumnDatafile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".asc";
  }

  public TitledDoubleColumnDatafile() {
    identifier = ".asc";
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

        title = reader.readLine();
        if (title.startsWith("*TYPE")) {
          String token = " ";
          StringTokenizer st = null;
          while (!token.equalsIgnoreCase("*COUNT")) {
            String linedata = reader.readLine();
            if (linedata != null) {
              st = new StringTokenizer(linedata, " ,=\t\r\n");
            } else
              break;

            while (st.hasMoreTokens() && !token.equalsIgnoreCase("*COUNT")) {
              token = st.nextToken();
              if (token.equalsIgnoreCase("*SAMPLE"))
                title = new String(token = st.nextToken());
              if (token.equalsIgnoreCase("*START"))
                startingvalue = Double.valueOf(token = st.nextToken()).doubleValue();
              if (token.equalsIgnoreCase("*STEP"))
                measurementstep = Double.valueOf(token = st.nextToken()).doubleValue();
            }
          }
          if (token.equalsIgnoreCase("*COUNT"))
           datanumber = Integer.valueOf(token = st.nextToken()).intValue();
          initData(datanumber);

          int i = 0;
          while (i < datanumber) {
            String linedata = reader.readLine();
            if (linedata == null || (linedata.startsWith("*END") || linedata.startsWith("*EOF")))
              break;

            st = new StringTokenizer(linedata, " ,\t\r\n");

              while (st.hasMoreTokens()) {
                setXData(i, startingvalue + i * measurementstep);
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
        } else {
        String linedata = reader.readLine();

        while (linedata != null) {

          StringTokenizer st = new StringTokenizer(linedata, "' ,\t\r\n");

          if (st.hasMoreTokens())
            token1 = st.nextToken();
          if (st.hasMoreTokens())
            token2 = st.nextToken();

          datanumber++;
          x.addElement(token1);
          y.addElement(token2);

          linedata = reader.readLine();
        }

        initData(datanumber);

        for (int i = 0; i < datanumber; i++) {
          token1 = (String) x.elementAt(i);
          token2 = (String) y.elementAt(i);
          setXData(i, Double.valueOf(token1).doubleValue());
          double intensityValue = Double.valueOf(token2).doubleValue();
          setYData(i, intensityValue);
          double tmpweight = Math.sqrt(intensity[i]);
          if (tmpweight != 0.0)
            setWeight(i, 1.0 / tmpweight);
          else
            setWeight(i, 1.0);
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
