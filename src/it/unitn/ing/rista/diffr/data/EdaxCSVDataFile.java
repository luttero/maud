/*
 * @(#)EdaxCSVDataFile.java created Oct 20, 2008 Mesiano
 *
 * Copyright (c) 2008 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.DiffrDataFile;
import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.util.Misc;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.Vector;
import java.util.StringTokenizer;

/**
 * The EdaxCSVDataFile is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Oct 20, 2008 7:10:27 PM $
 * @since JDK1.1
 */
public class EdaxCSVDataFile extends DiffrDataFile {

  public EdaxCSVDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);

    // user modification: put here the extension for the file format
    // you want to use. In this case this format is used when the
    // data file has the extension .dat

    identifier = ".csv";
  }

  public EdaxCSVDataFile() {
    // user modification: put here the extension for the file format
    // you want to use. In this case this format is used when the
    // data file has the extension .dat

    identifier = ".csv";
  }

  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = getReader();
    if (reader != null) {
      try {

// Variable definitions

        String token = "";
        String line;
        StringTokenizer st;

// user modification: do not modify before this line
// we read the Venezia format

//        dspacingbase = true;
        energyDispersive = true;

  Vector values = new Vector(4000, 1000);
        line = reader.readLine();
	while (line != null && token != null) {
        	st = new StringTokenizer(line, " ,\t\r\n");
		token = st.nextToken();
		values.addElement(token = st.nextToken());
        	line = reader.readLine();
	}

        datanumber = values.size();
        measurementstep = 10;
        startingvalue = 0;

        initData(datanumber);        // initialize all the vectors specifying the

	for (int i = 0; i < datanumber; i++) {
                setXData(i, startingvalue + i * measurementstep);
		double valoreY = Double.parseDouble((String) values.elementAt(i));
                setYData(i, valoreY);
                double tmpweight = Math.sqrt(valoreY);
                if (tmpweight != 0.0)
                  setWeight(i, 1.0 / tmpweight);
                else
                  setWeight(i, 1.0);  // Intensity is zero or less, the statical weight is
                // forced to 1.
        }

// user modification: do not modify after this line

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
