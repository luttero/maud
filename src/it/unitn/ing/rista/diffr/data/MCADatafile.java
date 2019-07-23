/*
 * @(#)MCADatafile.java created May 18, 2009 Caen
 *
 * Copyright (c) 2009 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.diffr.DiffrDataFile;
import it.unitn.ing.rista.util.MaudPreferences;
import it.unitn.ing.rista.util.Misc;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.StringTokenizer;

/**
 * The MCADatafile is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: May 18, 2009 4:16:46 PM $
 * @since JDK1.1
 */
public class MCADatafile extends DiffrDataFile {
  public MCADatafile(XRDcat aobj, String alabel) {
    super(aobj, alabel);

    // user modification: put here the extension for the file format
    // you want to use. In this case this format is used when the
    // data file has the extension .dat

    identifier = ".mca";
  }

  public MCADatafile() {
    // user modification: put here the extension for the file format
    // you want to use. In this case this format is used when the
    // data file has the extension .dat

    identifier = ".mca";
  }

	static int MAX_CHANNELS = 8192;

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
	      boolean AMTEK_FORMAT = false;

        double c_value = 0.0;

        line = reader.readLine();
	      if (line.startsWith("<<PMCA SPECTRUM>>")) {
		      boolean takeTimeFromDatfile = MaudPreferences.getBoolean("mac_datafile.useInternalTime", true);
		      AMTEK_FORMAT = true;
		      while (line != null && token != null && !line.startsWith("<<DATA>>")) {
			      if (line.startsWith("REAL_TIME")) {
				      st = new StringTokenizer(line, " -\t\r\n");
				      token = st.nextToken();
				      String measurementTime = st.nextToken();
				      setCountTime(measurementTime);
			      } else if (line.startsWith("START_TIME")) {
				      st = new StringTokenizer(line, " -\t\r\n");
				      token = st.nextToken();
              if (st.hasMoreTokens()) {
				        String startingDate = st.nextToken();
				        String startingTime = st.nextToken();
				        StringTokenizer dt = new StringTokenizer(startingDate, "/");
				        String month = dt.nextToken();
				        String day = dt.nextToken();
				        String year = dt.nextToken();
				        setMeasurementDate(year + "-" + month + "-" + day);
				        setMeasurementTime(startingTime);
              }
			      }
			      line = reader.readLine();
		      }

		      MAX_CHANNELS = MaudPreferences.getInteger("mca_datafile.maxNumberOfChannels", 8192);
		      double[] data = new double[MAX_CHANNELS];
		      if (line != null && line.startsWith("<<DATA>>"))
			      line =reader.readLine();
		      int i1 = 0;
		      while (line != null && token != null && !line.startsWith("<<END>>")) {
			      st = new StringTokenizer(line, " \\\t\r\n");
			      if (i1 < MAX_CHANNELS)
			        data[i1] = Double.parseDouble(st.nextToken());
			      if (!st.hasMoreTokens()) {
				      line = reader.readLine();
				      if (line != null)
					      st = new StringTokenizer(line, " \\\t\r\n");
			      }
			      i1++;
		      }
		      datanumber = i1;
		      if (datanumber > MAX_CHANNELS) {
			      System.out.println("Warning: max number of channels passed!");
			      System.out.println("To read effectively all data increase the mca_datafile.maxNumberOfChannels in Maud preferences.");
			      System.out.println("Set it to >= " + datanumber + ", and reload the data");
			      datanumber = MAX_CHANNELS;
		      }
		      initData(datanumber);        // initialize all the vectors specifying the
		      for (int i = 0; i < datanumber; i++) {
			      setXData(i, i);
			      setYData(i, data[i]);
			      double tmpweight = Math.sqrt(Math.abs(data[i]));
			      if (tmpweight != 0.0)
				      setWeight(i, 1.0 / tmpweight);
			      else
				      setWeight(i, 1.0);  // Intensity is zero or less, the statical weight is
			      // forced to 1.
		      }
	      } else {

	        datanumber = -1;
		      while (line != null && token != null && !line.startsWith("@A") && !line.startsWith("#DATA")) {
			      if (line.startsWith("#@CHANN")) {
				      st = new StringTokenizer(line, " ,\t\r\n");
				      token = st.nextToken();
				      datanumber = Integer.parseInt(st.nextToken());
				      initData(datanumber);        // initialize all the vectors specifying the
			      } else if (line.startsWith("#@CALIB")) {
				      st = new StringTokenizer(line, " ,\t\r\n");
				      token = st.nextToken();
				      startingvalue = Double.parseDouble(st.nextToken());
				      measurementstep = Double.parseDouble(st.nextToken());
				      c_value = Double.parseDouble(st.nextToken());
			      }
			      line = reader.readLine();
//			      System.out.println(line);
		      }
		      if (line != null) {
			      line = reader.readLine();
//			      System.out.println("-----" + line);
			      if (datanumber < 0) {
				      token = "#DATA";
				      MAX_CHANNELS = MaudPreferences.getInteger("mca_datafile.maxNumberOfChannels", 8192);
				      double[] data = new double[MAX_CHANNELS];
				      int i1 = 0;
				      while (line != null && token != null) {
					      st = new StringTokenizer(line, " \\\t\r\n");
					      if (i1 < MAX_CHANNELS)
						      data[i1] = Double.parseDouble(st.nextToken());
					      if (!st.hasMoreTokens()) {
						      line = reader.readLine();
						      if (line != null)
							      st = new StringTokenizer(line, " \\\t\r\n");
					      }
					      i1++;
				      }
				      datanumber = i1;
				      if (datanumber > MAX_CHANNELS) {
					      System.out.println("Warning: max number of channels passed!");
					      System.out.println("To read effectively all data increase the mca_datafile.maxNumberOfChannels in Maud preferences.");
					      System.out.println("Set it to >= " + datanumber + ", and reload the data");
					      datanumber = MAX_CHANNELS;
				      }
				      initData(datanumber);        // initialize all the vectors specifying the
				      for (int i = 0; i < datanumber; i++) {
					      setXData(i, i);
					      setYData(i, data[i]);
					      double tmpweight = Math.sqrt(Math.abs(data[i]));
					      if (tmpweight != 0.0)
						      setWeight(i, 1.0 / tmpweight);
					      else
						      setWeight(i, 1.0);  // Intensity is zero or less, the statical weight is
					      // forced to 1.
				      }
			      } else {
				      st = new StringTokenizer(line, " ,\\\t\r\n");
				      st.nextToken();
				      for (int i = 0; i < datanumber; i++) {
					      setXData(i, (startingvalue + i * measurementstep + i * i * c_value) * 1000);
					      double valoreY = Double.parseDouble(st.nextToken());
					      setYData(i, valoreY);
					      double tmpweight = Math.sqrt(valoreY);
					      if (tmpweight != 0.0)
						      setWeight(i, 1.0 / tmpweight);
					      else
						      setWeight(i, 1.0);  // Intensity is zero or less, the statical weight is
					      // forced to 1.
					      if (!st.hasMoreTokens()) {
						      line = reader.readLine();
						      if (line != null)
							      st = new StringTokenizer(line, " ,\\\t\r\n");
					      }
				      }
			      }
		      }
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
