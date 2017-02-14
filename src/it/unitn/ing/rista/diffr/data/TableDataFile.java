/*
 * @(#)TableDataFile.java created Jul 26, 2005 Povo, ITC
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Misc;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.StringTokenizer;
import java.util.Vector;


/**
 * The TableDataFile is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/01/19 14:45:56 $
 * @since JDK1.1
 */

public class TableDataFile extends MultDiffrDataFile {

  public TableDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".tbl";
  }

  public TableDataFile() {
    identifier = ".tbl";
  }


  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    boolean calibrated = true;
    BufferedReader reader = getReader();
    int spectrumNumber = -1;
    Vector lineString = new Vector(100, 100);

    title = toXRDcatString();
    DiffrDataFile datafile = null;
    if (reader != null) {
      try {
        String token = new String("");
        StringTokenizer st = null;
        String linedata = null;
        boolean endoffile = false;
        String numberString = null;
        boolean atmpB = true;
        linedata = reader.readLine();
        if (linedata == null)
          endoffile = true;
        else {
          st = new StringTokenizer(linedata, " ,\t\r\n");
          while (st.hasMoreTokens()) {
            token = st.nextToken();
            lineString.add(token);
          }
        }
        datanumber = lineString.size();
        double[] xcoord = new double[datanumber];
        for (int i = 0; i < datanumber; i++)
          xcoord[i] = Double.parseDouble((String) lineString.elementAt(i));
        linedata = reader.readLine();
        while (linedata != null && !endoffile) {
          st = new StringTokenizer(linedata, " ,\t\r\n");
          if (st.hasMoreTokens()) {
            numberString = Integer.toString(++spectrumNumber);
            datafile = addDiffrDatafile(numberString);
            atmpB = datafile.isAbilitatetoRefresh;
            datafile.isAbilitatetoRefresh = false;
            datafile.title = title + " number " + numberString;
            datafile.dspacingbase = false;
            datafile.constantstep = false;
            String valueT = "true";
            datafile.setField("_riet_meas_datafile_calibrated", valueT, "0", "0", "0", false, null, null, null, null, null,
                false, false);
            if (datanumber < 3)
              datafile.setCompute(false);
            datafile.initData(datanumber);
            int index = 0;
            while (st.hasMoreTokens() && index < datanumber) {
              token = st.nextToken();
              double intensitytmp = Double.valueOf(token).doubleValue();
              datafile.setXData(index, xcoord[index]);
              datafile.setYData(index, intensitytmp);
              double tmpweight = Math.sqrt(intensitytmp);
              if (tmpweight != 0.0)
                datafile.setWeight(index, 1.0 / tmpweight);
              else
                datafile.setWeight(index, 1.0);
              index++;
            }
            datafile.isAbilitatetoRefresh = atmpB;
            datafile.dataLoaded = true;
          }
          linedata = reader.readLine();
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
