/*
 * @(#)ILLDataFile.java created 08/07/1998 ILL, Grenoble
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
 *  The ILLDataFile is a class
 *
 *
 * @version $Revision: 1.9 $, $Date: 2006/01/19 14:45:55 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class ILLDataFile extends it.unitn.ing.rista.diffr.MultDiffrDataFile {

  public ILLDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".ill";
  }

  public ILLDataFile() {
    identifier = ".ill";
  }


  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = getReader();
    if (reader != null) {
      try {

        String token = new String("");
        StringTokenizer st = null;
        String linedata = null;
        boolean endoffile = false;

        while (!endoffile) {
          while (!token.toLowerCase().startsWith("sssss")) {
            linedata = reader.readLine();
	          linedata = Misc.removeUTF8BOM(linedata);
            if (linedata == null) {
              endoffile = true;
              break;
            }
            st = new StringTokenizer(linedata, " ,\t\r\n");
            token = st.nextToken();
          }

          if (endoffile)
            break;

          linedata = reader.readLine();
	        linedata = Misc.removeUTF8BOM(linedata);
          if (linedata == null) {
            endoffile = true;
            break;
          }

          st = new StringTokenizer(linedata, " ,\t\r\n");
          token = st.nextToken();

//        	System.out.println(	"Reading spectrum number: " + token +
//        											" ," + st.nextToken() + " to the end");
          DiffrDataFile datafile = addDiffrDatafile(token);
          boolean atmpB = datafile.isAbilitatetoRefresh;
          datafile.isAbilitatetoRefresh = false;

          linedata = reader.readLine();
          linedata = reader.readLine();
          st = new StringTokenizer(linedata, " ,\t\r\n");
          int nfields = Integer.valueOf(token = st.nextToken()).intValue();
          int nlines = Integer.valueOf(token = st.nextToken()).intValue();

          for (int i = 0; i < nlines; i++)
            linedata = reader.readLine();

          int i = 0;
          while (i < nfields) {
            linedata = reader.readLine();
            st = new StringTokenizer(linedata, " ,\t\r\n");
            while (st.hasMoreTokens()) {
              token = st.nextToken();
              if (i == 15)
                datafile.setString(1, token);
              if (i == 16)
                datafile.setString(2, token);
              if (i == 17)
                datafile.setString(3, token);
              i++;
            }
          }

          linedata = reader.readLine();

          linedata = reader.readLine();
          st = new StringTokenizer(linedata, " ,\t\r\n");
          int nchannel = Integer.valueOf(token = st.nextToken()).intValue();
//        	System.out.println(nchannel);

          datafile.initData(nchannel);
          datafile.constantstep = false;
          datafile.datanumber = nchannel;
          datafile.dspacingbase = false;

          i = 0;
          while (i < nchannel) {
            linedata = reader.readLine();
            if (linedata == null) {
              endoffile = true;
              datafile.isAbilitatetoRefresh = atmpB;
              datafile.dataLoaded = true;
              break;
            }
            st = new StringTokenizer(linedata, " ,\t\r\n");
            while (st.hasMoreTokens()) {
              datafile.setXData(i, (double) i);
              datafile.setYData(i, Double.valueOf(token = st.nextToken()).doubleValue());
              double tmpweight = Math.sqrt(datafile.getYData(i));
              if (tmpweight != 0.0)
                datafile.setWeight(i, 1.0 / tmpweight);
              else
                datafile.setWeight(i, 1.0);
              i++;
            }
          }
          datafile.isAbilitatetoRefresh = atmpB;
          datafile.dataLoaded = true;
          loadSuccessfull = true;
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
