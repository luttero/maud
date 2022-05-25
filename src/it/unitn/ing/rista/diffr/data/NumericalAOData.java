/*
 * @(#)NumericalAOData.java created May 18, 2011 Caen
 *
 * Copyright (c) 2011 Luca Lutterotti All Rights Reserved.
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
 * The NumericalAOData is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: May 18, 2011 5:30:04 PM $
 * @since JDK1.1
 */
public class NumericalAOData extends MultDiffrDataFile {

  public NumericalAOData(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".aodat";
  }

  public NumericalAOData() {
    identifier = ".aodat";
  }


  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = getReader();
    int spectrumNumber = -1;

    DiffrDataFile datafile = null;
    if (reader != null) {
      try {

        String token = new String("");
        StringTokenizer st = null;
        String linedata = null;
        boolean endoffile = false;
        String numberString = null;
        boolean atmpB = true;
        while (!endoffile) {
          if (token == null)
            token = " ";
          while (!endoffile && !token.startsWith("$")) {
            if (linedata == null)
              linedata = reader.readLine();
	          linedata = Misc.removeUTF8BOM(linedata);
//            System.out.println("early " + linedata);
            if (linedata == null)
              endoffile = true;
            else {
              st = new StringTokenizer(linedata, " ,\t\r\n");
              while (st.hasMoreTokens() && !token.startsWith("$") && !token.startsWith("!"))
                token = st.nextToken();
              if (token.startsWith("$")) {
                if (st.hasMoreTokens())
                  title = linedata.substring(2, linedata.length());
              }
              numberString = Integer.toString(++spectrumNumber);
              datafile = addDiffrDatafile(numberString);
              atmpB = datafile.isAbilitatetoRefresh;
              datafile.isAbilitatetoRefresh = false;
              linedata = reader.readLine();
              datafile.title = title + ":" + linedata.substring(1, linedata.length());
//                System.out.println(datafile.title);
            }
          }
          if (endoffile)
            break;
          datafile.dspacingbase = false;
          datafile.constantstep = false;

          Vector x = new Vector(100, 100);
          Vector y = new Vector(100, 100);

          datanumber = 0;

          String token1 = new String("");
          String token2 = new String("");

          linedata = reader.readLine();

          while (linedata != null && !linedata.startsWith("$")) {
//            System.out.println("next " + linedata);
            st = new StringTokenizer(linedata, "' ,\t\r\n");

            if (st.hasMoreTokens())
              token1 = st.nextToken();
            if (st.hasMoreTokens())
              token2 = st.nextToken();

            datanumber++;
            x.addElement(token1);
            y.addElement(token2);

            linedata = reader.readLine();
          }

          datafile.initData(datanumber);

          for (int i = 0; i < datanumber; i++) {
            token1 = (String) x.elementAt(i);
            token2 = (String) y.elementAt(i);
            datafile.setXData(i, Double.valueOf(token1).doubleValue());
            double intensityValue = Double.valueOf(token2).doubleValue();
            datafile.setYData(i, intensityValue);
            double tmpweight = Math.sqrt(Math.abs(intensityValue));
            if (tmpweight != 0.0)
              datafile.setWeight(i, 1.0 / tmpweight);
            else
              datafile.setWeight(i, 1.0);
          }
          datafile.dataLoaded = true;
          datafile.isAbilitatetoRefresh = atmpB;

          if (linedata == null)
            endoffile = true;
          else
            token = " ";
        }

        loadSuccessfull = true;

      } catch (Exception e) {
        e.printStackTrace();
        System.out.println("Error in loading the data file! Try to remove this data file");
      }
      try {
        reader.close();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
    isAbilitatetoRefresh = tmpB;
    return loadSuccessfull;
  }


}
