/*
 * @(#)EsquiGoCIFDataFile.java created 03/01/2001 Riva Del Garda (ItalStructures)
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.DiffrDataFile;
import it.unitn.ing.rista.diffr.MultDiffrDataFile;
import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.util.Misc;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.StringTokenizer;


/**
 *  The EsquiGoCIFDataFile is a class
 *
 *
 * @version $Revision: 1.19 $, $Date: 2006/07/20 13:39:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class EsquiGoCIFDataFile extends MultDiffrDataFile {

  public static int maxNumberOfChannels = 8192 * 4;


  public EsquiGoCIFDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".esg";
  }

  public EsquiGoCIFDataFile() {
    identifier = ".esg";
  }


  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    boolean calibrated = true;
    BufferedReader reader = getReader();
    int spectrumNumber = -1;
    double omega_angle = 0.0;
    double phi_angle = 0.0;
    double chi_angle = 0.0;
    double eta_angle = 0.0;
	  String theta2_angle = "0";
    int indexXcoord = -1, indexYcoord = -1, indexIntensity = 0;
    boolean stepscan = false;
	  double twothetaShift = 0.0;
	  double twothetaStart = 0.0;
    double twothetaStep = 0.01;
    boolean braun = false;
    boolean ketek = false;
    boolean ImagePlate = true;
    boolean scintillator = false;
    boolean bkgmess = false;
    boolean APD2000 = false;
	  boolean amptek = false;
	  boolean xparty = false;
    boolean image2D = false;
	  startingvalue = 0;
	  measurementstep = 1;
	  boolean oldESGfile = false;

    DiffrDataFile datafile = null;
    if (reader != null) {
      try {

        String token = new String("");
        StringTokenizer st = null;
        String linedata = reader.readLine();
        boolean endoffile = false;
        String numberString = null;
        boolean atmpB = true;
        while (!endoffile) {
	        if (!oldESGfile) {
		        indexXcoord = -1;
		        indexYcoord = -1;
		        indexIntensity = 0;
		        stepscan = false;
		        twothetaShift = 0.0;
		        twothetaStart = 0.0;
		        twothetaStep = 1;
		        braun = false;
		        ketek = false;
		        ImagePlate = true;
		        scintillator = false;
		        amptek = false;
		        image2D = false;
		        startingvalue = 0;
		        measurementstep = 1;
		        energyDispersive = false;
	        }

	        while (linedata != null && !linedata.startsWith("_pd_block_id")) {
	          if (linedata.startsWith("data_") && linedata.contains("Equinox3500"))
		          xparty = true;
	          linedata = reader.readLine();
          }
	        if (linedata == null) {
		        endoffile = true;
		        break;
	        } else {
		        st = new StringTokenizer(linedata, " ,\t\r\n");
		        token = " ";
		        while (st.hasMoreTokens() && !token.contains("_pd_block_id"))
			        token = st.nextToken();
		        if (st.hasMoreTokens()) {
			        title = st.nextToken();
			        if (title.startsWith("2001-10-2"))
				        bkgmess = true;
			        if (title.contains("|#"))
				        oldESGfile = true;
			        if (linedata.contains("APD2000"))
				        APD2000 = true;
			        numberString = Integer.toString(++spectrumNumber);
			        datafile = addDiffrDatafile(numberString);
			        atmpB = datafile.isAbilitatetoRefresh;
			        datafile.isAbilitatetoRefresh = false;
			        datafile.title = title + " number " + numberString;
		        }
	        }
          datafile.dspacingbase = false;
          datafile.constantstep = false;
          if (APD2000)
            datafile.setIntensityUnit(meas_intensity_unit[1]);
          String detector_string = "";
          while (token != null && !token.toLowerCase().startsWith("loop_")) {
            linedata = reader.readLine();
//            System.out.println(linedata);
            token = null;
            if (linedata == null)
              break;
            if (!linedata.equals("")) {
              st = new StringTokenizer(linedata, " ,\t\r\n");

// _pd_meas_angle_2theta                  -58
// _pd_meas_angle_omega                    75
// _pd_meas_angle_chi                     -90
// _pd_meas_angle_phi                      0
              while (st.hasMoreTokens()) {
	              token = st.nextToken();
	              if (token.equalsIgnoreCase("_pd_meas_angle_2theta")) {
		              twothetaShift = Double.valueOf(token = st.nextToken()).doubleValue();
	              } else if (token.equalsIgnoreCase("_pd_meas_2theta_range_min") ||
			              token.equalsIgnoreCase("_energy_dispersive_zero_eV")) {
		              twothetaStart = Double.valueOf(token = st.nextToken()).doubleValue();
	              } else if (token.equalsIgnoreCase("_pd_meas_2theta_range_inc") ||
			              token.equalsIgnoreCase("_pd_meas_degree_per_channel") ||
			              token.equalsIgnoreCase("_energy_dispersive_gain_eV")) {
		              twothetaStep = Double.valueOf(token = st.nextToken()).doubleValue();
	              } else if (token.equalsIgnoreCase("_diffrn_detector")) {
		              detector_string = st.nextToken();
	              } else if (token.equalsIgnoreCase("_diffrn_detector_type")) {
		              String detector = st.nextToken();
		              if (detector.toLowerCase().startsWith("unknown"))
			              detector = detector_string;
		              if (detector.toLowerCase().startsWith("mbraun"))
			              braun = true;
		              if (detector.toLowerCase().startsWith("ketek"))
			              ketek = true;
		              if (detector.toLowerCase().startsWith("mar") || detector.toLowerCase().startsWith("image"))
			              ImagePlate = true;
		              if (detector.toLowerCase().startsWith("scintillator"))
			              scintillator = true;
		              if (scintillator || braun)
			              stepscan = true;
		              else
			              stepscan = false;
	              } else if (token.equalsIgnoreCase("_pd_meas_angle_omega") || token.equalsIgnoreCase("_pd_meas_orientation_omega")) {
		              omega_angle = Double.valueOf(token = st.nextToken()).doubleValue();
		              if (scintillator || APD2000) // to be fixed
			              omega_angle = 0.0;
	              } else if (token.equalsIgnoreCase("_pd_meas_2theta_fixed")) {
		              theta2_angle = st.nextToken();
	              } else if (token.equalsIgnoreCase("_pd_meas_angle_chi") || token.equalsIgnoreCase("_pd_meas_orientation_chi")) {
		              if (APD2000)
			              chi_angle = Double.valueOf(token = st.nextToken()).doubleValue() - 180;
		              else if (!ImagePlate)
			              chi_angle = Double.valueOf(token = st.nextToken()).doubleValue() - 90.0;
		              else
			              chi_angle = Double.valueOf(token = st.nextToken()).doubleValue();
	              } else if (token.equalsIgnoreCase("_pd_meas_angle_phi") || token.equalsIgnoreCase("_pd_meas_orientation_phi")) {
		              phi_angle = Double.valueOf(token = st.nextToken()).doubleValue();
	              } else if (token.equalsIgnoreCase("_diffrn_measurement_method")) {
		              if (st.nextToken().equalsIgnoreCase("diffraction_image"))
			              image2D = true;
	              } else if (token.equalsIgnoreCase("_pd_meas_angle_eta") || token.equalsIgnoreCase("_pd_meas_orientation_eta")) {
		              eta_angle = Double.valueOf(token = st.nextToken()).doubleValue();
	              } else if (token.equalsIgnoreCase("_pd_meas_detector_id")) {
		              token = st.nextToken();
		              if (token.contains("Amptek")) {
			              energyDispersive = true;
			              amptek = true;
		              } else if (braun) {
			              int braunChannels = Integer.valueOf(token).intValue();
			              twothetaStart = twothetaShift - twothetaStep * ((braunChannels / 2) - 1);
//                    System.out.println(twothetaStart + " " + twothetaShift + " " + braunChannels);
		              } else if (ketek) {
			              int channels = Integer.valueOf(token).intValue();
		              }
	              } else if (token.equalsIgnoreCase("_riet_meas_datafile_calibrated") && st.hasMoreTokens()) {
		              String valueT = st.nextToken();
		              if (valueT.equalsIgnoreCase("false"))
			              calibrated = false;
		              else
			              calibrated = true;
	              } else if (token.contains("_meas_length_pixels")) {
		              // diffraction image in text format
									image2D = true;

                } else if (token.contains("_meas_width_pixels")) {
		              // diffraction image in text format
		              image2D = true;

	              } else {
                  if (token.startsWith("_") && st.hasMoreTokens())
                    datafile.setField(token, st.nextToken(), "0", "0", "0", false, null, null, null, null, null, false, false);
                }
              }
            } else
              token = " ";
          }
          String valueT = "false";
          if (calibrated)
            valueT = "true";
          datafile.setField("_riet_meas_datafile_calibrated", valueT, "0", "0", "0", false, null, null, null, null, null,
              false, false);
          datafile.setAngleValue(0, omega_angle);
          datafile.setAngleValue(1, chi_angle);
          datafile.setAngleValue(2, phi_angle);
          datafile.setAngleValue(3, eta_angle);
	        if (theta2_angle.equalsIgnoreCase("=omega"))
		        datafile.setAngleValue(4, omega_angle);
	        else
		        datafile.setAngleValue(4, Double.valueOf(theta2_angle));


// _pd_meas_detector_id
// _pd_meas_intensity_total
          if (token != null) {
            int i = 0;
            linedata = reader.readLine();
	          if (linedata != null && linedata.startsWith("#"))
		          linedata = reader.readLine();
 //           System.out.println(linedata);
            while (linedata.startsWith("_")) {
              st = new StringTokenizer(linedata, "' ,\t\r\n");
              while (st.hasMoreTokens()) {
                token = st.nextToken();
                if (token.startsWith("_")) {
                  if (token.equalsIgnoreCase(CIFXcoord2T)) {
                    indexXcoord = i;
                    stepscan = false;
                  } else if (token.equalsIgnoreCase(CIFXcoordD)) {
                    indexXcoord = i;
                    dspacingbase = true;
                    stepscan = false;
                  } else if (token.equalsIgnoreCase(CIFXcoordEnergy)) {
	                  indexXcoord = i;
	                  energyDispersive = true;
	                  stepscan = false;
                  } else if (token.equalsIgnoreCase("_pd_meas_detector_id")) {
                    indexXcoord = i;
                    if (braun) {
                      token = st.nextToken();
                      int braunChannels = Integer.valueOf(token).intValue();
                      twothetaStart = twothetaShift - twothetaStep * ((braunChannels / 2) - 1);
//                      System.out.println(twothetaStart + " " + twothetaShift + " " + braunChannels);
                    }
                  } else if (token.equalsIgnoreCase("_pd_meas_position_x")) {
                    indexXcoord = i;
                  } else if (token.equalsIgnoreCase("_pd_meas_position_y")) {
                    indexYcoord = i;
                  } else if (token.equalsIgnoreCase("_pd_meas_intensity_total")) {
                    indexIntensity = i;
                  } else if (token.equalsIgnoreCase(intensityCalcCIFstring) ||
		                  token.equalsIgnoreCase(intensityExpCIFstring)) {
                    indexIntensity = i;
                  } else if (token.equalsIgnoreCase("_pd_proc_intensity_total") || token.equalsIgnoreCase("_pd_meas_counts_total")) {
                    indexIntensity = i;
                  }
                  i++;
                }
              }
              linedata = reader.readLine();
	            if (linedata != null && linedata.startsWith("#"))
		            linedata = reader.readLine();
//              System.out.println(linedata);
            }
            int maxCIFEntries = i;

            double[][] xy = new double[maxCIFEntries][maxNumberOfChannels];

            datanumber = 0;

            while (linedata != null && (!linedata.startsWith("_") && !linedata.startsWith("loop"))) {

              st = new StringTokenizer(linedata, "' ,\t\r\n");

              token = null;
              for (int j = 0; j < maxCIFEntries; j++) {
                if (st.hasMoreTokens()) {
                  token = st.nextToken();
                  xy[j][datanumber] = Double.valueOf(token).doubleValue();
                  if (braun && bkgmess)      // bkg removed in that week
                    xy[j][datanumber] += 250.0;
                }
//                if (stepscan)
//                  xy[indexXcoord][j] = twothetaStart + datasetsNumber * twothetaStep;
              }
              if (token != null)
                datanumber++;

              linedata = reader.readLine();
	            if (linedata != null && linedata.startsWith("#"))
		            linedata = reader.readLine();
            }

            if (datanumber < 3)
              datafile.setCompute(false);
            if (image2D)
              datafile.setDataType(DIFFRACTION_IMAGE);
            datafile.initData(datanumber);
	          datafile.dspacingbase = dspacingbase;
	          datafile.energyDispersive = energyDispersive;

//           System.out.println(datafile.toXRDcatString() + " " + energyDispersive + " " + twothetaStart + " " + twothetaStep);

            for (int j = 0; j < datanumber; j++) {
              if (stepscan || ketek || amptek || indexXcoord == -1)
                datafile.setXData(j, twothetaStart + j * twothetaStep);
              else if (calibrated)
                datafile.setXData(j, twothetaShift + xy[indexXcoord][j]);
              else if (image2D) {
                datafile.setXData(j, j);
                datafile.setXImage(j, xy[indexXcoord][j]);
                datafile.setYImage(j, xy[indexYcoord][j]);
              } else
                datafile.setXData(j, twothetaShift + xy[indexXcoord][j]);
              datafile.setYData(j, xy[indexIntensity][j]);
//            System.out.println(xy[indexIntensity][j]);
              double tmpweight = Math.sqrt(datafile.getYData(i));
              if (tmpweight != 0.0)
                datafile.setWeight(j, 1.0 / tmpweight);
              else
                datafile.setWeight(j, 1.0);
            }
            if (linedata == null) {
              endoffile = true;
              datafile.isAbilitatetoRefresh = atmpB;
              datafile.dataLoaded = true;
//              System.out.println(datafile.toXRDcatString() + " " + isAbilitatetoRefresh);
              break;
            }
          }
          loadSuccessfull = true;
          datafile.dataLoaded = true;
          datafile.isAbilitatetoRefresh = atmpB;
//          System.out.println(datafile.toXRDcatString() + " " + isAbilitatetoRefresh);

//        	linedata = reader.readLine();
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
        e.printStackTrace();
      }
    }
    isAbilitatetoRefresh = tmpB;
    return loadSuccessfull;
  }

/*	public void setXData(int index, double value) {
		super.setXData(index, value);
//		originalNotCalibrated = true;
	}

	public void setCalibratedXData(int index, double value) {
		setCalibrated(true);
		twothetacalibrated[index] = (double) (value + twothetaShift - 60.0);
	}*/

}
