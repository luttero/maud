/*
 * @(#)StressX3000DataFile.java created Apr 10, 2006 Casalino
 *
 * Copyright (c) 1996-2006 Luca Lutterotti All Rights Reserved.
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
import it.unitn.ing.rista.util.Misc;

import java.io.*;


/**
 * The StressX3000DataFile is a class to read the Italstructures datafile
 * obtained from the StressX3000 instrument
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:04 $
 * @since JDK1.1
 */

public class StressX3000DataFile extends MultDiffrDataFile {

  public StressX3000DataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".str";
  }

  public StressX3000DataFile() {
    identifier = ".str";
  }

  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;

    // try to read the .PAR file containing some informations

    short psiOrOmega = 0;

    String filename = filterfilename(this.toXRDcatString());
    String fileparname = filename.substring(0, filename.length() - 3) + "PAR";

    DataInputStream readerPAR = new DataInputStream(new BufferedInputStream(
        Misc.getInputStream(getFolder(), fileparname)));
    if (readerPAR != null) {
      try {
        readerPAR.skipBytes(81);
        psiOrOmega = Misc.readShortLittleEndian(readerPAR);
//        System.out.println("pairOrOmega = " + psiOrOmega);
      } catch (IOException e) {
        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
      }
      try {
        readerPAR.close();
      } catch (IOException e) {
        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
      }
    }

    // read now the real datafile
    DataInputStream reader = new DataInputStream(getBufferedInputStream());
    boolean atmpB = false;
    int iPeak = 0;

    if (reader != null) {
      try {

        boolean hasMorePeaks = true;
        int shiftBytes = 4;
        int nstp = 0; // number of channels per peak
        float tme = 0.0f; // time per step
        float step = 0.0f; // step also called ssz
        int flag = 0; // ?
        float samp = 0.0f; // ?
        float theta0 = 0.0f; // starting 2theta
        float omega0 = 0.0f; // omega
        float chi = 0.0f; // chi
        float phi = 0.0f; // phi
//        String name = ""; // name of the peak, 32 bytes
        float lbd1 = 0; // ?
        float lbd2 = 0; // ?
//        String empty = ""; // Empty 76 bytes
        int rang = 0; // has more peaks

        // skips first bytes
        reader.skipBytes(shiftBytes); // reading the rest of the header
//        for (int i = 0; i < shiftBytes; i++)
//          Misc.readCharOneByte(reader);

        while (hasMorePeaks) {
          nstp = Misc.readIntLittleEndian(reader);
//          System.out.println("nstp = " + nstp);
          tme = Misc.readFloatLittleEndian(reader);
//          System.out.println("tme = " + tme);
          step = Misc.readFloatLittleEndian(reader);
//          System.out.println("step = " + step);
          flag = Misc.readIntLittleEndian(reader);
//          System.out.println("flag = " + flag);
          samp = Misc.readFloatLittleEndian(reader);
//          System.out.println("samp = " + samp);
          theta0 = Misc.readFloatLittleEndian(reader);
//          System.out.println("theta0 = " + theta0);
          omega0 = Misc.readFloatLittleEndian(reader);
//          System.out.println("omega0 = " + omega0);
          chi = Misc.readFloatLittleEndian(reader);
//          System.out.println("chi = " + chi);
          phi = Misc.readFloatLittleEndian(reader);
//          System.out.println("phi = " + phi);
          // name
          for (int i = 0; i < 32; i++)
            Misc.readCharOneByte(reader);
          lbd1 = Misc.readFloatLittleEndian(reader);
//          System.out.println("lbd1 = " + lbd1);
          lbd2 = Misc.readFloatLittleEndian(reader);
//          System.out.println("lbd2 = " + lbd2);
          // empty
          reader.skipBytes(72); // reading the rest of the header
//          for (int i = 0; i < 76; i++)
//            Misc.readCharOneByte(reader);
          rang = Misc.readIntLittleEndian(reader);
//          System.out.println("rang = " + rang);

          DiffrDataFile datafile = addDiffrDatafile(Integer.toString(iPeak));
          atmpB = datafile.isAbilitatetoRefresh;
          datafile.isAbilitatetoRefresh = false;
          datafile.initData(nstp);
          datafile.dspacingbase = false;
          datafile.constantstep = true;

          datafile.setAngleValue(2, phi);
          if (psiOrOmega == 0) {
            datafile.setAngleValue(0, (theta0 + step * nstp / 2) / 2);
            datafile.setAngleValue(1, -chi);
          } else {
            datafile.setAngleValue(0, (theta0 + step * nstp / 2) / 2 - chi);
            datafile.setAngleValue(1, 0.0);
          }
          datafile.measurementstep = step;

          for (int i = 0; i < nstp; i++) {
            float counts = Misc.readFloatLittleEndian(reader);
            datafile.setXData(i, theta0 + step * i);
            datafile.setYData(i, counts);
            double tmpweight = Math.sqrt(counts);
            if (tmpweight != 0.0)
              datafile.setWeight(i, 1.0 / tmpweight);
            else
              datafile.setWeight(i, 0.0);  // with high intensity inel set intensity to zero!!
          }
          datafile.isAbilitatetoRefresh = atmpB;
          datafile.dataLoaded = true;

          iPeak++;
          if (rang == 0)
            hasMorePeaks = false;
        }
        loadSuccessfull = true;

      } catch (Exception e) {
        e.printStackTrace();
        System.out.println("Error in loading the data file! Try to remove this data file");
        if (iPeak > 1) // at least one peak has beed readed successfully
          loadSuccessfull = true;
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
    }
    isAbilitatetoRefresh = tmpB;
    return loadSuccessfull;
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


}
