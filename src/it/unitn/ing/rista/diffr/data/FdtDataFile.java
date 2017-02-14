/*
 * @(#)FdtDataFile.java created 11/10/2001 Le Mans
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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
import java.awt.*;
import java.lang.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.Utility;


/**
 *  The FdtDataFile is a class to read the binary FDT datafile of the INEL banana
 *  detector.
 *
 *
 * @version $Revision: 1.10 $, $Date: 2006/01/19 14:45:55 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class FdtDataFile extends InelDataFile {

  public FdtDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".fdt";
  }

  public FdtDataFile() {
    identifier = ".fdt";
  }

/*      from Henry Pilliere at INEL

{***********************.FDT P3D file format *********************
******************************************************************}
 FDTinidef = record
    tailFDT          : smallint;
    taille           : integer;
    nb_scan          : smallint;
    commentaires     : cmtdef;
    tilt,azim,dataN  : integer;
    sectors          : boolean;
    number           : integer;
    sample           : str20;
    plane            : str9;
    date1            : str20;
    Meas_Type        : real48;
    bkgMeas          : boolean;
    wl               : real;
    volt,curr        : str9;
    colW,colH,DetW,DetH : real48;
    nomcal           : str255;
    FullOK           : boolean;
    Limlow,LimHigh   : smallint;
 end;
 FDTdef = record
    dateFDT   : array[1..7] of smallint;
    tacq      : single;
    valmot    : array[1..7] of single;
    humid     : single;
    weight_factor : single;
    lab_sc    : str9;
 end;


{*****************************.CAL Calibration *******************
******************************************************************}
   Xdec    = array[1..300] of real;{Channel values divided by 1000}
   AsplDec = array[1..4,1..100] of real;{for i=1, 2theta value}
   Pa1     = array[1..11] of real; {param?tres polynome degr\ufffd n- no more used}

   CLDec = record
     wl       : real;
     Averr    : single;
     a1       : Pa1;
     ICSPL,NV : smallint;
     Xd       : Xdec;
     ASPL     : AsplDec;
   end;



 {*****************************************************************}
FDT file format
HEADER 16448 octets
    *fdtini.TailFDT : Integer [2] : *1024 = size of the block of data
    *fdtini.nb_scan : Integer[2] : number of blocks
    *fdtini.Commentaires[1] : string[80] : comments
    *Fdtini.Commentaires[2] : string[80] : comments
    *fdtini.Tilt    : [4] : not used
    *fdtini.Azim    : [4] : not used
    *fdtini.DataN   : [4] : not used
    *fdtini.Sectors :boolean : not used
    *fdtini.Number  : [4] : not used
    *fdtini.Sample  : [20] : not used
    *fdtini.Plane   : [9] : not used
    *fdtini.date1   : [20] : date of file creation
    *fdtini.date1   : [20] : date of file creation
    *fdtini.Meas_Type : [6] : not used
    *fdtini.bkgMeas :[Boolean] : not used
    *fdtini.WL      : [8] : wavelength
    *fdtini.Volt    : string[9] : voltage
    *fdtini.Curr    : string[9] : current
    *fdtini.ColW    : [6] : not used
    *fdtini.ColH    : [6] : not used
    *fdtini.DetW    : [6] : not used
    *fdtini.DetH    : [6] : not used
    *fdtini.FullOK  : boolean : true if the whole channel domain is used, if not, limits are the 2 following
    *fdtini.Limlow  : [2] : low channel limit if FullOK is true
    *fdtini.Limhigh : [2] : high channel limit if FullOK is true
    *calchar        : string[255] : calibration filename
    *[15628 octets empty]

Nsc BLOCKS (fdtini.tailfdt*1024*4+64) octets


   fdt.dateFDT    :7 * Integer[2] :         {date and time of the block}
   *fdt.tacq      : real[4] :  {acquisition time in seconds}
   *Scan^         : fdtini.tailfdt*1024 * Integer[4] :    array of data
   *fdt.ValMot[2] : real[4] : {theta/temperature}
   *fdt.ValMot[1] : real[4] : {2theta/rampe}
   *fdt.ValMot[3] : real[4] : {Chi}
   *fdt.ValMot[4] : real[4] : {Tr. X}
   *fdt.ValMot[5] : real[4] : { Tr. Y}
   *fdt.ValMot[6] : real[4] : { Tr. Z}
   *fdt.ValMot[7] : real[4] : {Phi}
   *fdt.humid     : real[4] : {humidity}
   *fdt.weight_factor : real[4] : {weight factor}
   *fdt.lab_sc    : string[9] : {label of the scan}
   {POUR HUBER : }
   for i:=1 to 7 do transf[i]:=fdt.valMot[i];
   fdt.valMot[1]:=transf[3];  {2theta}
   fdt.valMot[2]:=transf[2];  {theta}
   fdt.valMot[3]:=transf[4];  {Chi}
   fdt.valMot[4]:=transf[5];  {X}
   fdt.valMot[5]:=transf[6];  {Y}
   fdt.valMot[6]:=transf[7];  {Z}
   fdt.valMot[7]:=transf[1];  {Phi}





CALIBRATION FUNCTIONS AND FORMAT

        The calibration function can be expressed in 2 mathematical formulae, as selected by user (see setup calibration) :
        - polynomial function (degree 1 to 10)
        - spline function (up to 100 points)-recommended-

        The calibration file is composed in 3 parts :
        - Wavelength
        - part for polynomial if selected
        - part for spline if selected

  *rien1   : Integer[2] : not used = 25675
  *Xlambda : real[8] :wavelength
  *Averr   : real[4] : case polynomial (no more used)
  *A1      : 11 * real[8] : array of parameter for the polynomial (no more used)
  *rien2   : Integer[2] : not used = 32156
  *ICSPL   : Integer[2] : if 0 then polynomial, if 1 then spline calibration
  *NV      : Integer [2] : number of experimental points used for spline calibration
  *X       : 100 * real[8] : array of peak position of standard in channel units, each multiple of 16, read Integer[2] of unused
  *ASPL    : 100 * 4 *real[8] : double array [1 to 4,1 to 100] of spline parameters. Each multiple of 16, read Integer[2]. The column ASPL[1,i] is the 2theta values corresponding to the theoretical position of the peak position of standard.


In Pascal, Codes of programming give :

The function Degree allows to convert channel in 2theta, by using a spline

FUNCTION Degree(XKan : REAL):DOUBLE; {Call to convert a Channel value in 2theta value}
{2 possibilities; either in polynomial or in spline}
VAR       XEN, YEN 	: DOUBLE;
          Id       	: INTEGER;
BEGIN
   IF ICSPL<>0 THEN
   BEGIN XEN:=XKAN/1000;Degree:=GetVal1(Xen);END {Spline case}
   ELSE
   BEGIN {polynomial case}
      XEN:=A1[11];
      Id:=10;
      REPEAT
         XEN:=XKan*XEN+A1[Id];
         DEC(Id);
      UNTIL Id=0;
      Degree:=XEN;
  END;
END;

FUNCTION GetVal1(XVal: DOUBLE):DOUBLE;          {Spline case}
VAR     XR,YVal       	: DOUBLE;
        itc,jtc       	: INTEGER;
BEGIN
   IF NV=0 THEN YVal:=0
   ELSE
   BEGIN
      IF XVal<X[1] THEN
      BEGIN XR:=XVal-X[1];YVal:=ASPL[1,1]+ASPL[2,1]*XR;GetVal1:=YVal;END
      ELSE
      BEGIN
         IF (XVal>X[NV]) THEN
         BEGIN
            XR:=XVal-X[NV];YVal:=ASPL[1,NV]+ASPL[2,NV]*XR;GetVal1:=YVal;END
         ELSE
         BEGIN
            itc:=0;
            REPEAT INC(itc);UNTIL (XVal>=X[itc]) AND (XVal<=X[itc+1]);
            XR:=XVal-X[itc];YVal:=ASPL[4,itc];GetVal1:=YVal;jtc:=3;
            REPEAT YVal:=XR*YVal+ASPL[jtc,itc];DEC(jtc);UNTIL jtc=0;
            GetVal1:=YVal;
         END;
      END;
   END;
END;

*/

  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    DataInputStream reader = new DataInputStream(getBufferedInputStream());
    boolean askForSpectra = false;
    boolean checkSpikes = MaudPreferences.getBoolean("fdtDataFile.checkSpikes", false);;
    int spikeMinimumValue = MaudPreferences.getInteger("fdtDataFile.spikeMinimumValue", 10000);
    boolean atmpB = false;

    if (reader != null) {
      try {

// Header 16448 bytes
        short TailFDT = Misc.readShortLittleEndian(reader); // *1024 = size of the block of data

        actualnumberofdata = TailFDT * 1024;

        boolean uncalibrated = true;
        double[] xvalues = new double[actualnumberofdata];
        for (int k = 0; k < actualnumberofdata; k++)
          xvalues[k] = k;

        short nb_scan = Misc.readShortLittleEndian(reader); // number of blocks

        byte Commentaires1[] = new byte[80];
        reader.read(Commentaires1);  // comments
        byte Commentaires2[] = new byte[80];
        reader.read(Commentaires2);  // comments                        164
        float Tilt = Misc.readFloatLittleEndian(reader); // not used
        float Azim = Misc.readFloatLittleEndian(reader); // not used    172
        int DataN = Misc.readIntLittleEndian(reader); // not used       176
        boolean Sectors = reader.readBoolean(); // not used
        int Number = Misc.readIntLittleEndian(reader); // not used
        byte Sample[] = new byte[20];
        reader.read(Sample); // not used
        byte Plane[] = new byte[9];
        reader.read(Plane); // not used                                 210
        byte date1[] = new byte[20];
        reader.read(date1); // date of file creation
        reader.read(date1); // date of file creation, second time?      250
        byte Meas_Type[] = new byte[6];
        reader.read(Meas_Type); // not used                             256
        boolean bkgMeas = reader.readBoolean(); // not used
        double WL = Misc.readDoubleLittleEndian(reader); // wavelength  265
        byte Volt[] = new byte[9];
        reader.read(Volt); // voltage
        byte Curr[] = new byte[9];
        reader.read(Curr); // current
        byte ColW[] = new byte[6];
        reader.read(ColW); // not used
        byte ColH[] = new byte[6];
        reader.read(ColH); // not used
        byte DetW[] = new byte[6];
        reader.read(DetW); // not used
        byte DetH[] = new byte[6];
        reader.read(DetH); // not used                                  307
        boolean FullOK = reader.readBoolean(); // true if the whole channel domain is used,
// if not, limits are the 2 following
        int bytesToSkip = 15885;
        if (!FullOK) {
          short Limlow = Misc.readShortLittleEndian(reader); // low channel limit if FullOK is true
          short Limhigh = Misc.readShortLittleEndian(reader); // high channel limit if FullOK is true
          bytesToSkip -= 4;
        }
//                        312
        StringBuffer calchar = new StringBuffer();
        for (int i = 0; i < 255; i++)
          calchar.append(Misc.readCharOneByte(reader)); // calibration filename

        String calName = calchar.toString();
        int index = calName.toLowerCase().indexOf(".cal");
        if (index > 0)
          calName = calName.substring(0, index) + ".cal";
        String calibrationFile = Misc.filterFileName(calName);

        DataInputStream calreader = getDataBufferedInputStreamNoChance(calibrationFile);
        String s1 = "", s2 = "";
        if (calreader == null) {
          index = calibrationFile.lastIndexOf("/");
          calibrationFile = calibrationFile.substring(index + 1);
          s1 = getFolder() + calibrationFile;
          calreader = getDataBufferedInputStreamNoChance(s1);
        }
        if (calreader == null) {
          s2 = getFilePar().getDirectory() + calibrationFile;
          calreader = getDataBufferedInputStreamNoChance(s2);
        }
        if (calreader == null) {  // still we don't find the calibration file, asking to the user
          calibrationFile = Utility.browseFilename(new Frame(),
                  "Choose the calibration file for the INEL fdt datafile");
          calibrationFile = Misc.filterFileName(calibrationFile);
          System.out.println("Find to read: " + calibrationFile);

          calreader = getDataBufferedInputStreamNoChance(calibrationFile);
        }
//        System.out.println("Trying to read: " + s1);
//        System.out.println("Trying to read: " + s2);
//        System.out.println("Reading: " + calibrationFile);
//        System.out.println("Check :" + calibrationFile.equalsIgnoreCase(s1) + ", " + calibrationFile.equalsIgnoreCase(s2));
//        System.out.println("Length :" + calibrationFile.length() + " " + s1.length() + ", " + s1.indexOf("Users"));
        if (calreader != null) {
          try {
            short rien1 = Misc.readShortLittleEndian(calreader); // not used = 25675
            double Xlambda = Misc.readDoubleLittleEndian(calreader); // wavelength
            double Averr = Misc.readFloatLittleEndian(calreader); // case polynomial (no more used)
            double[] A1 = new double[11];
            for (int ij = 0; ij < 11; ij++)
              A1[ij] = Misc.readDoubleLittleEndian(calreader); 	// array of parameter for the polynomial
// (no more used)
            short rien2 = Misc.readShortLittleEndian(calreader); // not used = 32156
            short ICSPL = Misc.readShortLittleEndian(calreader); // if 0 then polynomial,
// if 1 then spline calibration
            short NV = Misc.readShortLittleEndian(calreader);    // number of experimental points used
// for spline calibration

            if (NV > 100) NV = 100; // maximum 100
            double[] Xpeak = new double[100];
            int mult16 = 0;
            for (int ij = 0; ij < 100; ij++) {
              mult16++;
              if (mult16 == 16) {
                mult16 = 0;
                Misc.readShortLittleEndian(calreader);
              }
              Xpeak[ij] = Misc.readDoubleLittleEndian(calreader); 	// array of peak position of standard
// in channel units, each multiple of 16, read Integer[2] of unused
            }
            double[][] ASPL = new double[4][100];
//            mult16 = 0;
            for (int ij = 0; ij < 100; ij++) {
              for (int kl = 0; kl < 4; kl++) {
                mult16++;
                if (mult16 == 16) {
                  mult16 = 0;
                  Misc.readShortLittleEndian(calreader);
                }
                ASPL[kl][ij] = Misc.readDoubleLittleEndian(calreader);
              }
// double array [1 to 4,1 to 100] of spline parameters.
// Each multiple of 16, read Integer[2].
// The column ASPL[1,i] is the 2theta values corresponding to the theoretical
// position of the peak position of standard.
            }
            uncalibrated = false;
            for (int k = 0; k < actualnumberofdata; k++) {
              xvalues[k] = getSplineCalibration(0.001 * (k + 1), Xpeak, ASPL, NV);
            }


          } catch (IOException ioe) {
            ioe.printStackTrace();
            System.out.println("Not able to load the calibration file: " + calName);
          }
        } else
          System.out.println("Not able to find the calibration file: " + calName);



//        16444 still to read as we readed only to shorts
//        reader.skipBytes(16444); // reading the rest of the header
//        15881 to read
        reader.skipBytes(bytesToSkip); // reading the rest of the header

        int[] indexSkip = new int[3]; //getRangeSkipData(actualnumberofdata, askForSpectra);
        indexSkip[0] = 0;
        indexSkip[1] = actualnumberofdata;
        indexSkip[2] = 0;

        DiffrDataFile fakedatafile = null;
        if (indexSkip[2] > 0) {
          fakedatafile = new DiffrDataFile();
          fakedatafile.initXRD();
        }

        int toSkip = 0;

        for (int i = 0; i < nb_scan; i++) {
          DiffrDataFile datafile = null;
          toSkip++;

          if (toSkip == indexSkip[2]) {
            datafile = fakedatafile;
            toSkip = 0;
          } else {
            datafile = addDiffrDatafile(Integer.toString(i));
            atmpB = datafile.isAbilitatetoRefresh;
            datafile.isAbilitatetoRefresh = false;
          }

          byte dateFDT[] = new byte[14];
          reader.read(dateFDT); // date and time of the block
          float tacq = Misc.readFloatLittleEndian(reader); // acquisition time in seconds

          datafile.initData(actualnumberofdata);

          datafile.dspacingbase = false;
          datafile.constantstep = false;

          double twotheta = 0.0;
          for (int j = 0; j < actualnumberofdata; j++) {
            int Scan = Misc.readIntLittleEndian(reader);
            if (Scan < 0) // we will not accept it, we suppose is an error
              Scan = 0;
            if (checkSpikes && Scan > spikeMinimumValue)
              Scan = 0;
            datafile.setYData(j, Scan);
            double tmpweight = Math.sqrt(Scan);
            if (tmpweight != 0.0)
              datafile.setWeight(j, 1.0 / tmpweight);
            else
              datafile.setWeight(j, 0.0);  // with high intensity inel set intensity to zero!!

          }

          float ValMot[] = new float[7];
          ValMot[0] = Misc.readFloatLittleEndian(reader); // theta/temperature
          ValMot[1] = Misc.readFloatLittleEndian(reader); // Phi
          ValMot[2] = Misc.readFloatLittleEndian(reader); // 2theta/rampe
          ValMot[3] = Misc.readFloatLittleEndian(reader); // Chi
          ValMot[4] = Misc.readFloatLittleEndian(reader); // Tr. X
          ValMot[5] = Misc.readFloatLittleEndian(reader); // Tr. Y
          ValMot[6] = Misc.readFloatLittleEndian(reader); // Tr. Z
          float humid = Misc.readFloatLittleEndian(reader); // humidity
          float weight_factor = Misc.readFloatLittleEndian(reader); // weight factor
          byte lab_sc[] = new byte[10];  // 1 byte more to arrive at end
          reader.read(lab_sc); // label of the scan

          if (Math.abs(ValMot[2] + 23.8) < 0.1) {
            System.out.println("2theta offset is probably wrong: " + ValMot[2] + ", we reset it to zero!");
            ValMot[2] = 0;
          }
          datafile.setAngleValue(2, (double) ValMot[1]);
          datafile.setAngleValue(0, ValMot[0]);
          twothetaShift = ValMot[2];
          datafile.setAngleValue(1, ValMot[3]);
          for (int j = 0; j < actualnumberofdata; j++) {
            if (uncalibrated)
              datafile.setXData(j, j);
            else
              datafile.setXData(j, xvalues[j] + twothetaShift);
          }

          datafile.isAbilitatetoRefresh = atmpB;
          datafile.dataLoaded = true;

/*          if (indexSkip[0] != 0 || indexSkip[1] != actualnumberofdata) {
            datafile.realRangeCut(indexSkip[0], indexSkip[1]);
            datafile.setMinMaxIndices(0, indexSkip[1] - indexSkip[0]);
					}*/
        }
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

  public static double getSplineCalibration(double XVal, double[] Xpeak, double[][] ASPL, short NV) {
    double xvalue = 0.0;
    if (NV != 0) {
      if (XVal < Xpeak[0])
        xvalue = ASPL[0][0] + ASPL[1][0] * (XVal - Xpeak[0]);
      else {
        if (XVal > Xpeak[NV - 1])
          xvalue = ASPL[0][NV - 1] + ASPL[1][NV - 1] * (XVal - Xpeak[NV - 1]);
        else {
          int itc = -1;
          do {
            itc++;
          } while (!((XVal >= Xpeak[itc]) && (XVal <= Xpeak[itc + 1])));
          double XR = XVal - Xpeak[itc];
          xvalue = ASPL[3][itc];
          int jtc = 2;
          do {
            xvalue = XR * xvalue + ASPL[jtc][itc];
            jtc--;
          } while (jtc >= 0);
        }
      }
    }
    return xvalue;
  }

  public static DataInputStream getDataBufferedInputStream(String filename) {
    return getDataBufferedInputStream("", filename);
  }

  public static DataInputStream getDataBufferedInputStream(String folder, String filename) {
    InputStream in = Misc.getInputStream(folder, filename);
    if (in != null)
      return new DataInputStream(new BufferedInputStream(in));
    return null;
  }

	public static DataInputStream getDataBufferedInputStreamNoChance(String filename) {
		return getDataBufferedInputStreamNoChance("", filename);
	}

	public static DataInputStream getDataBufferedInputStreamNoChance(String folder, String filename) {
		InputStream in = Misc.getInputStreamNoChance(folder, filename);
		if (in != null)
			return new DataInputStream(new BufferedInputStream(in));
		return null;
	}

}


