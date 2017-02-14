/*
 * @(#)FluorescenceDataFileSpm.java created Feb 9, 2005 ITS, Riva del Garda
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
import it.unitn.ing.rista.util.MaudPreferences;
import it.unitn.ing.rista.util.Misc;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.StringTokenizer;


/**
 * The FluorescenceDataFileSpm is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:55 $
 * @since JDK1.1
 */

public class FluorescenceDataFileSpm extends MultDiffrDataFile {

  public int maxNumberOfChannels = MaudPreferences.getInteger("loadingDatafiles.detectorMaxChannels",  8192 * 2);

  double twothetaShift = 0.0;

  public FluorescenceDataFileSpm(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".spm";
  }

  public FluorescenceDataFileSpm() {
    identifier = ".spm";
  }


  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    title = "Spectrum";
    BufferedReader reader = getReader();
    int spectrumNumber = -1;
    double twoTheta = 0.0;
    double omega_angle = 0.0;
    double phi_angle = 0.0;
    double chi_angle = 0.0;
    double eta_angle = 0.0;
    int indexXcoord = -1, indexIntensity = 0;

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
          linedata = reader.readLine();
          if (linedata == null)
            endoffile = true;
          else {
            st = new StringTokenizer(linedata, " ,\t\r\n");
            if (!st.hasMoreTokens()) {
              endoffile = true;
              break;
            }
            twoTheta = Double.parseDouble(st.nextToken());
            omega_angle = Double.parseDouble(st.nextToken());
            chi_angle = Double.parseDouble(st.nextToken()) - 90.0;
            phi_angle = Double.parseDouble(st.nextToken());
          }
          
          if (endoffile)
            break;

          numberString = Integer.toString(++spectrumNumber);
          datafile = addDiffrDatafile(numberString);
          atmpB = datafile.isAbilitatetoRefresh;
          datafile.isAbilitatetoRefresh = false;
          datafile.title = title + " number " + numberString;

          datafile.energyDispersive = true;
          datafile.constantstep = false;
          String detector_string = "";

          datafile.setAngleValue(0, omega_angle);
          datafile.setAngleValue(1, chi_angle);
          datafile.setAngleValue(2, phi_angle);
          datafile.setAngleValue(3, eta_angle);

          linedata = reader.readLine();
          st = new StringTokenizer(linedata, "' ,\t\r\n");
          datanumber = Integer.parseInt(st.nextToken());
          linedata = reader.readLine();
          linedata = reader.readLine();
          if (datanumber < 3)
            datafile.setCompute(false);
          datafile.initData(datanumber);

          for (int i = 0; i < datanumber; i++) {
            linedata = reader.readLine();
            st = new StringTokenizer(linedata, "' ,\t\r\n");
            double intensity = Double.parseDouble(st.nextToken());
            datafile.setXData(i, i + 1);
            datafile.setYData(i, intensity);
            double tmpweight = Math.sqrt(intensity);
            if (tmpweight != 0.0)
              datafile.setWeight(i, 1.0 / tmpweight);
            else
              datafile.setWeight(i, 1.0);

          }
          loadSuccessfull = true;
          datafile.isAbilitatetoRefresh = atmpB;
          datafile.dataLoaded = true;

        	linedata = reader.readLine();
          linedata = reader.readLine();
          if (linedata == null)
            endoffile = true;
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
