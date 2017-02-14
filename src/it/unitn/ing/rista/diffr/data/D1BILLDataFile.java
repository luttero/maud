/*
 * @(#)D1BILLDataFile.java created 10/07/1998 ILL, Grenoble
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

import java.io.*;
import java.lang.*;
import java.util.*;

import it.unitn.ing.rista.util.*;


/**
 *  The D1BILLDataFile is a class to load B1B datafiles from ILL
 *
 *
 * @version $Revision: 1.10 $, $Date: 2006/07/20 13:39:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class D1BILLDataFile extends MultDiffrDataFile {

  public static int actualnumberofdata = 0;
  public static int spectrumNumber = 0;

  public D1BILLDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".f1b";
  }

  public D1BILLDataFile() {
    identifier = ".f1b";
  }


  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
	  double d1b_chi_zero = MaudPreferences.getDouble("d1b.chiZero", -89.0);
	  BufferedReader reader = getReader();
    if (reader != null) {
      try {

        String token = new String("");
        String titleString = "";
        StringTokenizer st = null;
        String linedata = null;
        String numor = "";
        boolean endoffile = false;
        spectrumNumber = 0;
        int numberOfLeadingLines = 2;
	      boolean newD1B = false;
	      boolean readData = true;

        while (!endoffile) {
          for (int i = 0; i < numberOfLeadingLines; i++) {
            linedata = reader.readLine();
            if (linedata == null) {
              endoffile = true;
              break;
            } else
              titleString = linedata;
          }
	        if (readData && titleString.length() > 9) {
		        String year = titleString.substring(7, 9);
		        int yearnumber = Integer.parseInt(year);
		        if (yearnumber < 79 && yearnumber > 11)
			        newD1B = true;
		        readData = false;
	        }
          numberOfLeadingLines = 2;

          if (endoffile)
            break;

          linedata = reader.readLine();
          if (linedata == null) {
            endoffile = true;
            break;
          }

          st = new StringTokenizer(linedata, " ,\t\r\n");
          if (st.hasMoreTokens())
            token = st.nextToken();
          else {
            linedata = reader.readLine();
          }
          if (linedata == null) {
            endoffile = true;
            break;
          }

          spectrumNumber++;


// there is a bug at ILL, if the number of spectra is > 999
// the label becomes ***
// So we use our counter in that case
          if (token.equals("***"))
            token = Integer.toString(spectrumNumber);
          DiffrDataFile datafile = addDiffrDatafile(token);
          boolean atmpB = datafile.isAbilitatetoRefresh;
          datafile.isAbilitatetoRefresh = false;

//        	System.out.println("Reading spectrum number: " + spectrumNumber);
          if (st.hasMoreTokens()) {
            numor = st.nextToken();
//            System.out.println("Numor #: " + numor);
          }


          datafile.title = titleString;
          linedata = reader.readLine();
          st = new StringTokenizer(linedata, " ,\t\r\n");
          token = st.nextToken();
          token = st.nextToken();
          startingvalue = Double.valueOf(token = st.nextToken()).doubleValue();
          double d1bomega = Double.valueOf(token = st.nextToken()).doubleValue();

//        	System.out.println("Omega: " + token);
	        if (newD1B)
		        datafile.setAngleValue(0, d1bomega); // corretto MZ 2012 perchè nuovo setup conta omega già in positivo
	        else
	          datafile.setAngleValue(0, 270.0 - d1bomega);
          token = st.nextToken();
//        	System.out.println("Chi: " + token);
          double chiD1B = Double.valueOf(token).doubleValue();
	        if (newD1B)
		        chiD1B = -chiD1B;
          datafile.setAngleValue(1, chiD1B + d1b_chi_zero);
          datafile.setString(3, token = st.nextToken()); // phi
          token = st.nextToken();
          radiation = Double.valueOf(token = st.nextToken()).doubleValue();
	        // Luca: legge il valore radiation, ma non lo imposta poi per lo strumento
	        // se si vuole imporre questa radiation basta decommentare le linee seguenti
/*	        RadiationType radType = datafile.getDataFileSet().getInstrument().getRadiationType();
	        if (radType.isNeutron()) { // we change only if it is neutron, otherwise the instrument is still not the correct one
		        if (radType.getRadiationIDNumber() > 1) {
			        radType.removeAllRadiations();
			        radType.addRadiation("D1b from file");
		        }
	        } else
		        datafile.getDataFileSet().getInstrument().setRadiationType("Neutron");
	        if (radType.getRadiationWavelength(0) != radiation)
	        radType.getRadiation(0).setRadiation("Added from file", radiation, 1.0);*/

	        measurementstep = Double.valueOf(token = st.nextToken()).doubleValue();

          linedata = reader.readLine();

          st = new StringTokenizer(linedata, " ,\t\r\n");
          int nchannel = Integer.valueOf(token = st.nextToken()).intValue();

          boolean readExtraLine = true;
          while (st.hasMoreTokens()) {
            token = st.nextToken();
            if (token.equalsIgnoreCase("to"))
              readExtraLine = false;
          }
          if (readExtraLine)
            linedata = reader.readLine();

//        	System.out.println("Nchannel: " + nchannel);
          datafile.initData(nchannel);
          datafile.constantstep = true;
          datafile.datanumber = nchannel;
          datafile.dspacingbase = false;

          int i = 0;
          while (i < nchannel) {
            linedata = reader.readLine();
            if (linedata == null) {
              endoffile = true;
              datafile.isAbilitatetoRefresh = atmpB;
              datafile.dataLoaded = true;
              break;
            } else if (linedata.startsWith("     -1000")) {
              numberOfLeadingLines = 1; // one already done
              break;
            }
            int[] digits = new int[2];
            digits[0] = 2;
            digits[1] = 8;
            String[] data = Misc.readFormattedLine(linedata, digits, 2, 10);
            for (int j = 0; j < 10 && i < datafile.datanumber; j++) {
              datafile.setXData(i, startingvalue + i * measurementstep);
              datafile.setYData(i, Double.valueOf(data[j * 2 + 1]).doubleValue());
              double tmpweight = Math.sqrt(datafile.getYData(i));
              if (tmpweight != 0.0)
                datafile.setWeight(i, 1.0 / tmpweight);
              else
                datafile.setWeight(i, 1.0);
              i++;
            }
          }
          loadSuccessfull = true;
          datafile.dataLoaded = true;
          datafile.isAbilitatetoRefresh = atmpB;
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
