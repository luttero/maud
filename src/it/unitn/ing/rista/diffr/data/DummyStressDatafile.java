package it.unitn.ing.rista.diffr.data;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Misc;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.StringTokenizer;

/**
 * Created by IntelliJ IDEA.
 * User: luca
 * Date: May 7, 2007
 * Time: 5:10:35 PM
 * To change this template use File | Settings | File Templates.
 */
public class DummyStressDatafile extends MultDiffrDataFile {

  public static int maxNumberOfChannels = 8192 * 4;


  double twothetaShift = 0.0;

  public DummyStressDatafile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".desg";
  }

  public DummyStressDatafile() {
    identifier = ".desg";
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
    int indexXcoord = -1, indexIntensity = 0;
    boolean stepscan = false;
    double twothetaStart = 0.0;
    double twothetaStep = 0.01;
    boolean braun = false;
    boolean ketek = false;
    boolean ImagePlate = false;
    boolean scintillator = false;
    boolean bkgmess = false;
    boolean APD2000 = false;

    DiffrDataFile datafile = null;
    if (reader != null) {
      try {

        String token = new String("");
        StringTokenizer st = null;
        String linedata = null;
        boolean endoffile = false;
        String numberString = null;
        boolean atmpB = true;

        for (int i = 0; i < 7; i++) {
          linedata = reader.readLine();
	        linedata = Misc.removeUTF8BOM(linedata);
          st = new StringTokenizer(linedata, "' ,\t\r\n");
          numberString = Integer.toString(++spectrumNumber);
          datafile = addDiffrDatafile(numberString);
          atmpB = datafile.isAbilitatetoRefresh;
          datafile.isAbilitatetoRefresh = false;
          title = "Stress molla";
          datafile.title = title + " number " + numberString;
          datafile.dspacingbase = false;
          datafile.constantstep = true;
          twothetaStart = 145.25;
          twothetaStep = 0.0212658;
          chi_angle = Double.valueOf(token = st.nextToken()).doubleValue();
          calibrated = true;
          String valueT = "true";
          datafile.setField("_riet_meas_datafile_calibrated", valueT, "0", "0", "0", false, null, null, null, null, null,
              false, false);
          datafile.setAngleValue(1, chi_angle);
          datanumber = 1023;
          datafile.setCompute(false);
          datafile.initData(datanumber);
          linedata = reader.readLine();
          for (int j = 0; j < datanumber; j++) {
            datafile.setXData(j, twothetaStart + j * 0.0212658);
            linedata = reader.readLine();
            st = new StringTokenizer(linedata, "' ,\t\r\n");
            double value = Double.valueOf(token = st.nextToken()).doubleValue();
            datafile.setYData(j, value);
//            System.out.println(xy[indexIntensity][j]);
            double tmpweight = Math.sqrt(datafile.getYData(j));
            if (tmpweight != 0.0)
              datafile.setWeight(j, 1.0 / tmpweight);
            else
              datafile.setWeight(j, 1.0);
          }
          if (i < 6) {
          linedata = reader.readLine();
          linedata = reader.readLine();
           }
          loadSuccessfull = true;
          datafile.dataLoaded = true;
          datafile.isAbilitatetoRefresh = atmpB;
        }
//          System.out.println(datafile.toXRDcatString() + " " + isAbilitatetoRefresh);

//        	linedata = reader.readLine();
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
