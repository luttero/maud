/*
 * @(#)DummyDatafile.java created 11/08/2025 Los Alamos
 *
 * Copyright (c) 2025 Luca Lutterotti All Rights Reserved.
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
import it.unitn.ing.rista.util.*;

import java.io.*;
import java.lang.*;
import java.util.*;

/**
 * The DummyDatafile is a class to hold a dummy datafile
 * used to create and store simulated data.
 *
 *
 * @version $Revision: 1.8 $, $Date: 2025/08/11 08:57:00 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class DummyDatafile extends it.unitn.ing.rista.diffr.DiffrDataFile {

  public DummyDatafile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".dum";
  }

  public DummyDatafile() {
    identifier = ".dum";
  }

  public boolean readallSpectra() {

//    System.out.println("Creating dummy datafile!");
    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;

    double start = MaudPreferences.getDouble("dummyDatafile.Q_start", 0.1);
    double end = MaudPreferences.getDouble("dummyDatafile.Q_end", 40.1);
    double step = MaudPreferences.getDouble("dummyDatafile.Q_step", 0.005);
    datanumber = (int) ((end - start) / step) + 1;

    Vector<double[]> data = new Vector<double[]>(datanumber, 100);

    dspacingbase = false;
    initData(datanumber);

    for (int i = 0; i < datanumber; i++) {
      double x = getXfromQ(start + step * i);
      double y = 1.0 + MoreMath.getGaussianNoise(1.0);
      double[] point = new double[2];
      point[0] = x;
      point[1] = y;
      data.add(point);
    }

    for (int i = 0; i < datanumber; i++) {
      double x = ((double[]) data.elementAt(i))[0];
      setXData(i, x);
      double intensityValue = ((double[]) data.elementAt(i))[1];
//          if (intensityValue < 0.0) // we will not accept it, we suppose is an error
//            intensityValue = 0.0;
      setYData(i, intensityValue);
      double tmpweight = Math.sqrt(Math.abs(intensityValue));
      if (tmpweight != 0.0)
        setWeight(i, 1.0 / tmpweight);
      else
        setWeight(i, 1.0);
  //    System.out.println(i + " " + x + " " + intensityValue);
    }
    loadSuccessfull = true;

    isAbilitatetoRefresh = tmpB;
    return loadSuccessfull;
  }
}
