/*
 * @(#)RigakuDataFile.java created 10/12/1998 ILL, Mesiano
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

package it.unitn.ing.rista.diffr.data;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Misc;

import java.io.*;
import java.lang.*;
import java.util.*;

/**
 *  The RigakuDataFile is a class
 *
 *
 * @version $Revision: 1.8 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class RigakuDataFile extends it.unitn.ing.rista.diffr.DiffrDataFile {

  public static String[] keyValues = {"*START", "*STOP", "*STEP", "*OFFSET", "*SEC_COUNT", "*COMMENT", "*YUNIT", "*COUNT"};

  public RigakuDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".raw";
  }

  public RigakuDataFile() {
    identifier = ".raw";
  }

  public int getIndexForKey(String key) {
    for (int i = 0; i < keyValues.length; i++)
      if (key.equalsIgnoreCase(keyValues[i]))
        return i;
    return keyValues.length;
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
        String line = null;
        String key;
        StringTokenizer st;

// user modification: do not modify before this line
// we read the Rigaku format

// the data is in 2Theta

        dspacingbase = false;

        line = reader.readLine();
        if (line.toUpperCase().startsWith("*CLASS")) {
          double endvalue = 180.0;
          double offset = 0.0;
          int index = -1;
          // new format
          do {
            line = reader.readLine();
            st = new StringTokenizer(line, " =,\t\r\n");
            key = st.nextToken();

            index = getIndexForKey(key);

            switch(index) {
              case 5:
                title = line.substring(18, line.length());
                break;
              case 6:
                if (!st.nextToken().equalsIgnoreCase("STEP"))
                  setCPSmeasurement();
                break;
              case 0:
                startingvalue = Double.valueOf(st.nextToken()).doubleValue();
                break;
              case 1:
                endvalue = Double.valueOf(st.nextToken()).doubleValue();
                break;
              case 2:
                measurementstep = Double.valueOf(st.nextToken()).doubleValue();
                break;
              case 3:
                offset = Double.valueOf(st.nextToken()).doubleValue();
                break;
              case 4:
                setCountTime(st.nextToken());
                break;
              case 7:
                datanumber = Integer.parseInt(st.nextToken()) + 1;
                break;
              default: {}
            }
          } while(index != 7);

          initData(datanumber);

// now we read the data, in this case four columns (only intensities)

          constantstep = true;         // We assume a constant step
          int i = 0;
          while (i < datanumber && line != null && !line.startsWith("*END")) {
            line = reader.readLine();
            if (line != null) {
              st = new StringTokenizer(line, " ,\t\r\n");

// only one column, the first, is used; the x value is computed from the
// starting value of 2Theta or d-spacing and from the measurement step
              while (st.hasMoreTokens()) {
                setXData(i, startingvalue + measurementstep * i);
                double intensityValue = Double.parseDouble(st.nextToken());
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
// This in case the input process was ended before the last data point
          for (; i < datanumber; i++) {
            setXData(i, startingvalue + measurementstep * i);
            setYData(i, 0.0);
            setWeight(i, 0.0);
          }
        } else {
        for (int i = 0; i < 2; i++)
          line = reader.readLine();

// read the entire forth line and use it as title

        title = reader.readLine();

        for (int i = 0; i < 11; i++)
          reader.readLine();

        st = new StringTokenizer(reader.readLine(), " ,\t\r\n");
        int meas_mode = Integer.valueOf(st.nextToken()).intValue();
        if (meas_mode == 1)
          setCPSmeasurement();

        for (int i = 0; i < 6; i++)
          reader.readLine();

        st = new StringTokenizer(reader.readLine(), " ,\t\r\n");
        startingvalue = Double.valueOf(st.nextToken()).doubleValue();

        st = new StringTokenizer(reader.readLine(), " ,\t\r\n");
        measurementstep = Double.valueOf(st.nextToken()).doubleValue();

        st = new StringTokenizer(reader.readLine(), " ,\t\r\n");
        datanumber = Integer.valueOf(st.nextToken()).intValue();

        st = new StringTokenizer(reader.readLine(), " ,\t\r\n");
        setCountTime(st.nextToken());

        initData(datanumber);

        for (int i = 0; i < 12; i++)
          line = reader.readLine();

// now we read the data, in this case one column (only intensities)

        constantstep = true;         // We assume a constant step
        int i = 0;
        while (i < datanumber && line != null) {
          line = reader.readLine();
          if (line != null) {
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
        }
// This in case the input process was ended before the last data point
        for (; i < datanumber; i++) {
          setXData(i, startingvalue + measurementstep * i);
          setYData(i, 0.0);
          setWeight(i, 0.0);
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
