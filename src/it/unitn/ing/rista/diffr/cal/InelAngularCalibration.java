/*
 * @(#)InelAngularCalibration.java created 03/05/2000 Trento-Firenze
 *
 * Copyright (c) 2000 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.cal;

import it.unitn.ing.rista.diffr.*;

import java.io.*;
import java.lang.*;
import java.util.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;


/**
 *  The InelAngularCalibration is a class
 *
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:55 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class InelAngularCalibration extends D20AngularCalibration {

  public InelAngularCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "Inel Angular cal";
    IDlabel = "Inel Angular cal";
  }

  public InelAngularCalibration(XRDcat aobj) {
    this(aobj, "Calibration file x");
  }

  public InelAngularCalibration() {
    identifier = "Inel Angular cal";
    IDlabel = "Inel Angular cal";
  }

  public void readall() {

    refreshCalibration = true;
    boolean isAbilitate = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = Misc.getReader(getFileName());
    if (reader != null) {
      try {
        Vector angular = new Vector(0, 100);
        String token = new String("");
        StringTokenizer st = null;
        String linedata = null;
        while (!token.equals("points")) {
          linedata = reader.readLine();

          st = new StringTokenizer(linedata, " ,\t\r\n");

          while (st.hasMoreTokens() && !token.equals("points"))
            token = st.nextToken();
        }
        if (st.hasMoreTokens())
          token = st.nextToken();
        if (st.hasMoreTokens())
          datanumber = Integer.valueOf(token = st.nextToken()).intValue();
        calangular = new double[datanumber];


        while (!token.startsWith("*****")) {
          linedata = reader.readLine();

          st = new StringTokenizer(linedata, " ,\t\r\n");

          while (st.hasMoreTokens())
            token = st.nextToken();
        }

        boolean endoffile = false;
        linedata = reader.readLine();
        if (linedata == null) {
          endoffile = true;
        }
        while (!endoffile) {
          linedata = reader.readLine();
          if (linedata == null) {
            endoffile = true;
            break;
          }

          linedata = reader.readLine();
          if (linedata == null) {
            endoffile = true;
            break;
          }

          int i = 0;
          linedata = reader.readLine();
          if (linedata == null) {
            endoffile = true;
            break;
          }
          st = new StringTokenizer(linedata, " ,\t\r\n");
          token = st.nextToken();
          double twotheta = 0.0;
          while (!token.startsWith("*****") && !endoffile) {
            calangular[i++] = Double.valueOf(token).doubleValue();
            linedata = reader.readLine();
            if (linedata == null) {
              endoffile = true;
              break;
            }
            st = new StringTokenizer(linedata, " ,\t\r\n");
            token = st.nextToken();
          }
          endoffile = true;
        }

      } catch (IOException e) {
        System.out.println("Error in loading the data file! Try to remove this data file");
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
    }
    isAbilitatetoRefresh = isAbilitate;
    notifyUpObjectChanged(this, Constants.ANGULAR_CALIBRATION);

  }

}
