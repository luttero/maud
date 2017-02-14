/*
 * @(#)DubnaDataFile.java created 12/04/1999 Firenze
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

import java.io.BufferedReader;
import java.io.IOException;
import java.lang.*;
import java.util.StringTokenizer;
import java.util.Vector;


/**
 *  The DubnaDataFile is a class
 *
 *
 * @version $Revision: 1.5 $, $Date: 2005/05/06 18:07:26 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class DubnaDataFile extends MultDiffrDataFile {

  public DubnaDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "srwa";
  }

  public DubnaDataFile() {
    identifier = "srwa";
  }


	public boolean readallSpectra() {
		// this method must be overrided by subclasses
		boolean tmpB = isAbilitatetoRefresh;
		isAbilitatetoRefresh = false;
		BufferedReader reader = getReader();
		if (reader != null) {
			try {

//			 insert the code to read from the file
//				 example (this read the entire file):

				String token = new String("");
				Vector multicolumn = new Vector(100, 100);
				StringTokenizer st = null;
				String token1 = new String("");
				String token2 = new String("");


				boolean firstline = true;

				int columns = 1;

				String linedata = reader.readLine();      // read one line
				while (linedata != null) {

					st = new StringTokenizer(linedata, " ,\t\r\n");  	// create the tokenizer, fields are separed by
					// spaces ( ), commas (,), tabs (\t),
					// carriage return (\r) or line feed (\n)

					while (st.hasMoreTokens()) {
						token = st.nextToken();      // retrieve one by one the fields inside the linedata string
						multicolumn.addElement(token);
					}

					if (firstline) {
						columns = multicolumn.size();
					}

					linedata = reader.readLine();		// read next line

				}

				if (columns > 2)
					columns = 1;
				datanumber = multicolumn.size() / columns;

				dspacingbase = true;
				initData(datanumber);

				int j = 0;
				for (int i = 0; i < datanumber; i++) {

					token1 = (String) multicolumn.elementAt(j);
					if (columns > 1) {
						token2 = (String) multicolumn.elementAt(j + 1);
						setCalibratedXData(i, Double.valueOf(token1).doubleValue());  // old was setCalibratedXData
					} else {
						token2 = token1;
						setXData(i, (double) i);
					}
					double intensityValue = Double.valueOf(token2).doubleValue();
					setYData(i, intensityValue);
					double tmpweight = Math.sqrt(intensity[i]);
					if (tmpweight != 0.0)
						setWeight(i, 1.0 / tmpweight);
					else
						setWeight(i, 1.0);
					j += columns;
				}

			} catch (IOException e) {
				System.out.println("Error in loading the data file! Try to remove this data file");
			}
			try {
				reader.close();
			} catch (IOException e) {
			}
		}
		else
			return false;
		isAbilitatetoRefresh = tmpB;
		return true;
	}


}
