/*
 * @(#)D19ImageDataFile.java created Jun 16, 2008 Caen
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

import ij.plugin.D19ImageReader;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.cal.*;
import it.unitn.ing.rista.util.*;

import java.io.*;
import java.util.Vector;

/**
 * The D19ImageDataFile is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 16, 2008 3:59:16 PM $
 * @since JDK1.1
 */
public class D19ImageDataFile extends it.unitn.ing.rista.diffr.MultDiffrDataFile {

  public static int minimumNumberOfPoints = 10;

  public D19ImageDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "_LAMP";
  }

  public D19ImageDataFile() {
    identifier = "_LAMP";
  }


  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;

    int[] imageDimension = new int[3];
    Vector<double[]> dimension = new Vector<>();
    String[] properties = new String[1];
//    new Opener().open();
//		OpenDialog od = new OpenDialog("Open...", "");
//    boolean calibrateLater = MaudPreferences.getBoolean("d19Detector.calibrateInMaud", true);
    boolean takeWavelengthFromDatafile = MaudPreferences.getBoolean("d19Detector.useWavelengthFromDatafile", true);
	  AngularCalibration angularCalibration = getDataFileSet().getInstrument().getAngularCalibration();

	  String directory = getFolder(); //od.getDirectory();
    String name = getLabel(); //od.getFileName();
//    System.out.println("Opening file: " + directory + name);
    if (name != null) {
      File file = new File(directory + name);
      if (file.exists()) {
//        imp.show();
        D19ImageReader fileReader = new D19ImageReader();
        Vector buffer = fileReader.loadImage(directory, name, imageDimension, dimension, properties);

        if (buffer != null) {

          if (takeWavelengthFromDatafile) {
//            getDataFileSet().getInstrument().setRadiationType("Neutron");
// todo fix the problem      getDataFileSet().getInstrument().getRadiationType().getRadiation(0).getWavelength().setValue(dimension.get(0)[4]);
          }
          double centerX = MaudPreferences.getDouble("d19Detector.ImageCenterX", -58);
          double centerY = MaudPreferences.getDouble("d19Detector.ImageCenterY", 194.11);
          double radius = MaudPreferences.getDouble("d19Detector.detectorSampleDistance", 768.8);

          double coneInterval = MaudPreferences.getDouble("d19Detector.defaultEtaConeInterval", 5.0);
          double theta2Step = MaudPreferences.getDouble("d19Detector.defaultDiffractionStepAngle", 0.05);
          double coneAngleMax = MaudPreferences.getDouble("d19Detector.defaultEtaConeAngleMax", 180.0);

          double coneStep = coneInterval;
          double halfConeStep = coneStep / 2.0;
          int nprofiles = (int) (2.0 * coneAngleMax / coneStep + 1.000001);

          int npoints = imageDimension[0];
          int npointsY = imageDimension[1];
          double[][] profile = new double[nprofiles][npoints];

          double[] coordX = (double[]) buffer.elementAt(0);
          double[] coordY = (double[]) buffer.elementAt(1);
          double[] coordPhi = (double[]) buffer.elementAt(2);
	        int minX = MaudPreferences.getInteger("squareRoi.xminValue", 0);
	        int maxX = MaudPreferences.getInteger("squareRoi.xmaxValue", coordX.length);
	        int minY = MaudPreferences.getInteger("squareRoi.yminValue", 0);
	        int maxY = MaudPreferences.getInteger("squareRoi.ymaxValue", coordY.length);
					if (maxX > coordX.length) { // problem here
						minX = 0;
						maxX = coordX.length;
						minY = 0;
						maxY = coordY.length;
					}
	        if (maxY > coordY.length) { // problem here
		        minX = 0;
		        maxX = coordX.length;
		        minY = 0;
		        maxY = coordY.length;
	        }

//	        double radius = dimension.get(0)[5];

          int totalsize = coordX.length * coordY.length;

          double[] photoplateIntensity = new double[totalsize];

          double[] theta2Coord = new double[totalsize];
          double[] etaCoord = new double[totalsize];
          double etaStep = 5.0;

          Angles.getTheta2EtaFromCurvedImageDetector(coordX, coordY, theta2Coord, etaCoord, radius, centerX, centerY);

          double min2theta = 360;
          double max2theta = -360;
          double mineta = 360;
          double maxeta = -360;
          for (int i = 0; i < theta2Coord.length; i++) {
            if (min2theta > theta2Coord[i])
              min2theta = theta2Coord[i];
            if (max2theta < theta2Coord[i])
              max2theta = theta2Coord[i];
            if (mineta > etaCoord[i])
              mineta = etaCoord[i];
            if (maxeta < etaCoord[i])
              maxeta = etaCoord[i];
          }

          double nmineta = 0.0;
          int i = 0;
          while (nmineta < mineta)
            nmineta = i++ * etaStep;
          while (nmineta >= mineta + etaStep)
            nmineta = i-- * etaStep;
          mineta = nmineta;
          double nmintheta = 0.0;
          i = 0;
          while (nmintheta < min2theta)
            nmintheta = i++ * theta2Step;
          while (nmintheta >= min2theta + theta2Step)
            nmintheta = i-- * theta2Step;
          min2theta = nmintheta;

//	        System.out.println("2theta range: " + min2theta + " " + max2theta);

          if (angularCalibration instanceof AngularCameraCalibration) {
            AngularCameraCalibration angcal = (AngularCameraCalibration) angularCalibration;
            angcal.setRadius(Double.toString(radius));
            angcal.setOriginalCenterX(Double.toString(centerX));
            angcal.setOriginalCenterY(Double.toString(centerY));
            angcal.setStartingValue(Double.toString(0));
          } else if (angularCalibration instanceof Angular2DCurvedDetectorCalibration) {
		        Angular2DCurvedDetectorCalibration angcal = (Angular2DCurvedDetectorCalibration) angularCalibration;
		        angcal.setRadius(Double.toString(radius));
		        angcal.setOriginalCenterX(Double.toString(centerX));
		        angcal.setOriginalCenterY(Double.toString(centerY));
		        angcal.setDetector2Theta(0);
	        }

          int spectrumNumber = 0;

          int stepLoading = MaudPreferences.getInteger("d19Detector.loadAnImageEvery", 1);
	        int sumEvery = MaudPreferences.getInteger("d19Detector.sumImagesNumber", 1);
	        for (int yi = 0; yi < imageDimension[2]; yi+=stepLoading) {
		        double[][] photoplate = new double[coordX.length][coordY.length];
		        for (int yj = 0; yj < sumEvery; yj++) {
			        double[][] photo = (double[][]) buffer.elementAt(3 + yi + yj);
			        for (int ij = 0; ij < minX; ij++)
				        for (int j = 0; j < coordY.length; j++)
					        photoplate[ij][j] = -999;
			        for (int ij = minX; ij < maxX; ij++) {
				        for (int j = 0; j < minY; j++)
					        photoplate[ij][j] = -999;
				        for (int j = minY; j < maxY; j++)
					        photoplate[ij][j] += photo[ij][j];
				        for (int j = maxY; j < coordY.length; j++)
					        photoplate[ij][j] = -999;
			        }
			        for (int ij = maxX; ij < coordX.length; ij++)
				        for (int j = 0; j < coordY.length; j++)
					        photoplate[ij][j] = -999;
		        }

		        double chi = dimension.get(yi)[1];
		        double omega = dimension.get(yi)[0];
		        double phi = dimension.get(yi)[2];
            i = 0;

            for (int ij = 0; ij < coordX.length; ij++) {
              for (int j = 0; j < coordY.length; j++) {
                photoplateIntensity[i++] = photoplate[ij][j];
              }
            }

            double[][][] spectra = Angles.spectraFromPixelsByEtaTheta2CurvedDetector(theta2Coord, etaCoord, photoplateIntensity,
                coordX, coordY, radius, centerX, centerY,
                min2theta, max2theta, theta2Step,
                mineta, maxeta, etaStep);

            // we save to check
//          String[] folderAndName = saveDataAsCIF(spectra, mineta * Constants.PITODEG, etaStep, radius, );

//          setLabel(filename);
            for (int spectrumIndex = 0; spectrumIndex < spectra[2].length; spectrumIndex++) {
              String numberString = Integer.toString(spectrumNumber++);
              DiffrDataFile datafile = addDiffrDatafile(numberString);
              boolean atmpB = datafile.isAbilitatetoRefresh;
	            if (angularCalibration instanceof Angular2DCurvedDetectorCalibration)
		            datafile.setDataType(DIFFRACTION_IMAGE);
              datafile.isAbilitatetoRefresh = false;

	            datafile.setAngleValue(0, omega);
              datafile.setAngleValue(1, chi);
              datafile.setAngleValue(2, phi);
              datafile.setAngleValue(3, mineta + spectrumIndex * coneInterval);

              datanumber = 0;
              i = 0;
              while (i < spectra[2][spectrumIndex].length)
                if (spectra[2][spectrumIndex][i++] >= 0)
                  datanumber++;
              datafile.datanumber = datanumber;
//            System.out.println(datafile.datanumber);
              if (datanumber < minimumNumberOfPoints)
                datafile.setCompute(false);
              datafile.initData(datanumber);
              datafile.constantstep = false;
              datafile.dspacingbase = false;

              i = 0;
              int indexPoint = 0;
              while (i < spectra[2][spectrumIndex].length) {
                double intensityValue = spectra[2][spectrumIndex][i];
                if (intensityValue >= 0) {
                  if (angularCalibration instanceof AngularCameraCalibration) {
		                datafile.setXData(indexPoint, spectra[0][spectrumIndex][i] * Constants.DEGTOPI * radius);
	                } else if (angularCalibration instanceof Angular2DCurvedDetectorCalibration) {
	                  datafile.setXData(indexPoint, indexPoint);
	                  datafile.setXImage(indexPoint, spectra[3][spectrumIndex][i]);
	                  datafile.setYImage(indexPoint, spectra[4][spectrumIndex][i]);
                  } else
                    datafile.setXData(indexPoint, spectra[0][spectrumIndex][i]);
                  datafile.setYData(indexPoint, intensityValue);
                  double tmpweight = Math.sqrt(datafile.getYData(indexPoint));
                  if (tmpweight != 0.0)
                    datafile.setWeight(indexPoint, 1.0 / tmpweight);
                  else
                    datafile.setWeight(indexPoint, 1.0);
                  indexPoint++;
                }
                i++;
              }
              datafile.isAbilitatetoRefresh = atmpB;
              loadSuccessfull = true;
              datafile.dataLoaded = true;
            }
          }
        }
        getFilePar().setStoreSpectraOption(true);
      }
      isAbilitatetoRefresh = tmpB;

    }
    return loadSuccessfull;
  }


  public String[] saveDataAsCIF(String filename, double[][][] profile, double etaStart, double etaStep, double radius,
                                double omega, double chi, double phi) {

//    String filename = Utility.openFileDialog(new Frame(), "Save as CIF (.esg)...",
//            FileDialog.SAVE, null, null, "put a name (with extension).esg");
    String[] folderAndName = Misc.getFolderandName(filename);

    String folder = folderAndName[0];
    filename = folderAndName[1];

    if (filename == null) return null;
    if (!filename.endsWith(".esg"))
      filename = filename + ".esg";

    int nprofiles = profile[0].length;
    int dataNumber = profile[0][0].length;

    String title = "Data_from_D19_datafile_" + filename;
    BufferedWriter output = Misc.getWriter(folder, filename);
    try {
      output.write("_pd_block_id " + title);
      output.newLine();
      output.newLine();
      output.write("_diffrn_detector Area Detector D19");
      output.newLine();
      output.write("_diffrn_detector_type Curved 2D");
      output.newLine();
      output.write("_pd_meas_step_count_time ?");
      output.newLine();
      output.write("_diffrn_measurement_method diffraction_image");
      output.newLine();
      output.write("_diffrn_measurement_distance_unit ?");
      output.newLine();
      output.write("_pd_instr_dist_spec/detc " + radius);
      output.newLine();
      output.write("_diffrn_radiation_wavelength ?");
      output.newLine();
      output.write("_diffrn_source_target ?");
      output.newLine();
      output.write("_diffrn_source_power ?");
      output.newLine();
      output.write("_diffrn_source_current ?");
      output.newLine();
      output.write("_pd_meas_angle_omega " + omega);
      output.newLine();
      output.write("_pd_meas_angle_chi " + chi);
      output.newLine();
      output.write("_pd_meas_angle_phi " + phi);
      output.newLine();
      output.write("_riet_par_spec_displac_x 0");
      output.newLine();
      output.write("_riet_par_spec_displac_y 0");
      output.newLine();
      output.write("_riet_par_spec_displac_z 0");
      output.newLine();
      output.write("_riet_meas_datafile_calibrated true");
      output.newLine();
      output.newLine();
      for (int ij = 0; ij < nprofiles; ij++) {
        int checkPoints = 0;
        for (int i = 0; i < dataNumber; i++) {
          if (profile[2][ij][i] > 0.0) {
            checkPoints++;
          }
        }
        if (checkPoints > minimumNumberOfPoints) {
          output.write("_pd_block_id " + title + "|#" + ij);
          output.newLine();
          output.newLine();
          double eta = etaStart + etaStep * ij;
          output.write("_pd_meas_angle_eta " + Double.toString(eta));
          output.newLine();
          output.newLine();
          output.write("loop_");
          output.newLine();
          output.write(CIFXcoord2T + " _pd_meas_intensity_total");
          output.newLine();
          for (int i = 0; i < dataNumber; i++) {
            double intensity = profile[2][ij][i];
            if (intensity > 0.0) {
              output.write(" " + Fmt.format(profile[0][ij][i]) + " " + Fmt.format(intensity));
              output.newLine();
            }
          }
          output.newLine();
        }
      }
    } catch (IOException io) {
      io.printStackTrace();
    }

    try {
      output.close();
    } catch (IOException io) {
      io.printStackTrace();
    }
    return folderAndName;
  }
}
