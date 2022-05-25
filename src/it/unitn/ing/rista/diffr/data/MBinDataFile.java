/*
 * @(#)MBinDataFile.java created 23/10/2001 Le Mans
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
import java.lang.*;


/**
 *  The MBinDataFile is a class to read the binary Maud datafile of the INEL banana
 *  detector.
 *
 *
 * @version $Revision: 1.8 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class MBinDataFile extends MultDiffrDataFile {

  public static final short version = 1;

  public MBinDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".mbi";
  }

  public MBinDataFile() {
    identifier = ".mbi";
  }

  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    DataInputStream reader = new DataInputStream(getBufferedInputStream());
    if (reader != null) {
      try {
        short vers = reader.readShort();
        short actualnumberofdata = reader.readShort();

        boolean uncalibrated = true;
        double[] xvalues = new double[actualnumberofdata];
//        short nb_scan = reader.readShort(); // number of blocks
        uncalibrated = false;
        for (int k = 0; k < actualnumberofdata; k++)
          xvalues[k] = reader.readFloat();

        int i = -1;
        while (true) {
          i++;
          DiffrDataFile datafile = addDiffrDatafile(Integer.toString(i));
          boolean atmpB = datafile.isAbilitatetoRefresh;
          datafile.isAbilitatetoRefresh = false;
          datafile.initData(actualnumberofdata);
          datafile.dspacingbase = false;
          datafile.constantstep = false;
          datafile.setAngleValue(0, (double) reader.readFloat());
          datafile.setAngleValue(1, (double) reader.readFloat());
          datafile.setAngleValue(2, (double) reader.readFloat());

          for (int j = 0; j < actualnumberofdata; j++) {
            int Scan = reader.readInt();
            datafile.setYData(j, Scan);
            double tmpweight = Math.sqrt(datafile.getYData(i));
            if (tmpweight != 0.0)
              datafile.setWeight(j, 1.0 / tmpweight);
            else
              datafile.setWeight(j, 0.0);  // with high intensity inel set intensity to zero!!

// this must go after the second block

            if (uncalibrated)
              datafile.setXData(j, j);
            else
              datafile.setXData(j, xvalues[j]);
          }
          datafile.isAbilitatetoRefresh = atmpB;
          datafile.dataLoaded = true;
          loadSuccessfull = true;

        }

      } catch (Exception e) {
//        System.out.println("Error in loading the data file! Try to remove this data file");
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


