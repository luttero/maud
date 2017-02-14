/*
 * @(#)DoubleColumnQDataFile.java created 27/07/2000 Barcelona
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

import java.io.*;
import java.lang.*;
import java.util.*;

import it.unitn.ing.rista.util.*;

/**
 * The ESRFB11QDataFile is a class to load a datafile coming from the
 * Beaamline 11 at ESRF consisting of some columns, one with the q
 * and the second with the intensity.
 *
 *
 * @version $Revision: 1.8 $, $Date: 2006/01/19 14:45:55 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class ESRFB11QDataFile extends it.unitn.ing.rista.diffr.data.DoubleColumnQDataFile {

  public ESRFB11QDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".c00";
  }

  public ESRFB11QDataFile() {
    identifier = ".c00";
  }

/*	public boolean readallSpectra() {

    boolean loadSuccessfull = false;
		BufferedReader reader = getReader();
		if (reader != null) {
				try {

					dspacingbase = true;
					Vector x = new Vector(100,100);
					Vector y = new Vector(100,100);

					datasetsNumber = 0;

					String token1 = new String("");
					String token2 = new String("");

          String linedata = null;
          StringTokenizer st = null;

          linedata = reader.readLine();
					while (linedata != null) {

          	st = new StringTokenizer(linedata, "' ,\t\r\n");

          	if (st.hasMoreTokens())
          		token1 = st.nextToken();
          	if (st.hasMoreTokens())
          		token2 = st.nextToken();

          	datasetsNumber++;
          	x.addElement(token1);
          	y.addElement(token2);

          	linedata = reader.readLine();
					}

					initData(datasetsNumber);

          double dspace = 0.0;
          for (int i = 0; i < datasetsNumber; i++) {
          	token1 = (String) x.elementAt(i);
          	token2 = (String) y.elementAt(i);
          	dspace = 2 * Math.PI / Double.valueOf(token1).doubleValue();
          	setCalibratedXData(i, dspace);
       			double intensityValue = Double.valueOf(token2).doubleValue();
						if (intensityValue < 0.0) // we will not accept it, we suppose is an error
							intensityValue = 0.0;
       			setYData(i, intensityValue);
       			double tmpweight = Math.sqrt(intensityValue);
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
	  	} catch (IOException e) {}
	  }
	  return loadSuccessfull;
	}*/
}
