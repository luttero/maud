/*
 * @(#)ISISGemRotaxDatafile.java created Nov 1, 2004 ISIS
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
import it.unitn.ing.rista.diffr.detector.TOFMultiDetector;
import it.unitn.ing.rista.diffr.detector.TOFDetector;
import it.unitn.ing.rista.util.Misc;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.StringTokenizer;
import java.util.Vector;


/**
 * The ISISGemRotaxDatafile is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:55 $
 * @since JDK1.1
 */

public class ISISGemRotaxDatafile extends MultDiffrDataFile {

  public ISISGemRotaxDatafile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".gem";
  }

  public ISISGemRotaxDatafile() {
    identifier = ".gem";
  }


  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    int spectrumNumber = -1;
    int group = 0;
    String omega_angle = "0.0";
    String phi_angle = "0.0";
    String chi_angle = "0.0";
    String eta_angle = "0.0";
    double TOFangle = 0.0; // angle
    double totalFlightPath = 0.0; //fpath
    double primaryFlightPath = 0.0;

    BufferedReader reader = getReader();
    if (reader != null) {
      try {

        String token = new String("");
        StringTokenizer st = null;
        boolean endoffile = false;
        DiffrDataFile datafile = null;
        String bankID = null;
        String linedata = reader.readLine();
	      linedata = Misc.removeUTF8BOM(linedata);
        boolean atmpB = false;

        while (!endoffile) {
//          System.out.println(linedata);
          while (linedata != null && !linedata.startsWith("#L ")) {
            if (linedata == null) {
              endoffile = true;
              break;
            }
            st = new StringTokenizer(linedata, " ,\t\r\n");
            token = st.nextToken();
            if (token.startsWith("#C  Primary flight path")) {
              // read primary flight path here
              for (int i = 0; i < 4; i++)
                token = st.nextToken();
              primaryFlightPath = Double.valueOf(token).doubleValue();
            } else if (token.startsWith("#P")) {
              token = st.nextToken(); // lt0
              token = st.nextToken(); // lstep
              TOFangle = Double.valueOf(st.nextToken()).doubleValue(); // angle
              totalFlightPath = Double.valueOf(st.nextToken()).doubleValue(); //fpath
            } else if (token.startsWith("#S")) {
              token = st.nextToken(); // spectrum number
//        	    System.out.println(	"Reading spectrum number: " + token);
              spectrumNumber = Integer.parseInt(token);
              datafile = addDiffrDatafile(token);
              atmpB = datafile.isAbilitatetoRefresh;
              datafile.isAbilitatetoRefresh = false;
              token = st.nextToken();
              token = st.nextToken();
              token = st.nextToken();
              bankID = "Group" + token; // the Group
            } else if (linedata.startsWith("#A  OMEGA")) {
              token = st.nextToken();
              omega_angle = st.nextToken();
            } else if (linedata.startsWith("#A  CHI")) {
              token = st.nextToken();
              chi_angle = st.nextToken();
            } else if (linedata.startsWith("#A  PHI")) {
              token = st.nextToken();
              phi_angle = st.nextToken();
            } else if (linedata.startsWith("#A  ETA")) {
              token = st.nextToken();
              eta_angle = st.nextToken();
            }
            linedata = reader.readLine();
          }

          if (endoffile)
            break;

          linedata = reader.readLine();
          if (linedata == null) {
            endoffile = true;
            break;
          }
          Vector dataV = new Vector(100, 100);
          while (linedata != null && !linedata.startsWith("#")) {
            st = new StringTokenizer(linedata, " ,\t\r\n");

            double[] dDataErrorTOF = new double[4];
            for (int i = 0; i < 3; i++)
              dDataErrorTOF[i] = Double.parseDouble(st.nextToken());
            // check for 4th column
            if (st.hasMoreTokens())
              dDataErrorTOF[3] = Double.parseDouble(st.nextToken());
            else
              dDataErrorTOF[3] = -1.0;
            dataV.add(dDataErrorTOF);
            linedata = reader.readLine();
          }

          int nchannel = dataV.size();
//        	System.out.println(nchannel);

          datafile.initData(nchannel);
          datafile.constantstep = false;
          datafile.datanumber = nchannel;
          datafile.dspacingbase = true;

          int i = 0;
          while (i < nchannel) {
            double[] dDataErrorTOF = (double[]) dataV.elementAt(i);
            datafile.setXData(i, (double) dDataErrorTOF[0]);
            if (dDataErrorTOF[1] < 0.0)
              dDataErrorTOF[1] = 0.0;
            datafile.setYData(i, dDataErrorTOF[1]);
            double tmpweight = dDataErrorTOF[2];
            if (tmpweight != 0.0)
              datafile.setWeight(i, 1.0 / tmpweight);
            else
              datafile.setWeight(i, 0.0);
            i++;
          }
          datafile.setBankID(bankID);
          datafile.setString(1, omega_angle);
          datafile.setString(2, chi_angle);
          datafile.setString(3, phi_angle);
          datafile.setString(4, eta_angle);
          datafile.isAbilitatetoRefresh = atmpB;
          datafile.dataLoaded = true;
          checkGroupConsistency(datafile, spectrumNumber, bankID, TOFangle, totalFlightPath,
              primaryFlightPath);
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

  private void checkGroupConsistency(DiffrDataFile datafile, int spectrumNumber, String bankID, double toFangle,
                                     double totalFlightPath, double primaryFlightPath) {
    Instrument instr = datafile.getDataFileSet().getInstrument();
    if (instr == null)
      return;
    Detector detector = instr.getDetector();
    if (detector == null)
      return;
    if (detector instanceof TOFMultiDetector) {
      int bankn = detector.getBankNumber(bankID);
      if (bankn < 0)
        System.out.println("Warning: no TOFdetector corresponding!");
      TOFDetector tofdect = ((TOFMultiDetector) detector).getDetector(bankn);
      boolean checkGood;
      if (tofdect != null) {
        double diff = Math.abs(toFangle - tofdect.getTheta().getValueD());
        checkGood = (diff < 0.00001);
        checkGood = (checkGood && (bankID.equalsIgnoreCase(tofdect.getLabel())));
        checkGood = (checkGood && (spectrumNumber == (bankn + 1)));
        if (!checkGood) {
          System.out.println("Warning: detector/grouping/spectrum not corresponding:");
          System.out.println("Spectrum n:" + spectrumNumber + ", " + bankID + ", Theta: " + toFangle);
          System.out.println("Detector n:" + (bankn + 1) + ", " + tofdect.getLabel() +
              ", Theta: " + tofdect.getTheta().getValueD());
        }
      } else
        System.out.println("Warning: no TOFdetector at this time!");

    } //else
    // System.out.println("Warning: cross check of groups cannot be done, detector not corresponding!");
  }


}
