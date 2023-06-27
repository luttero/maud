/*
 * @(#)D20IntensityCalibration.java created 24/07/1999 Pergine
 *
 * Copyright (c) 1997-1999 Luca Lutterotti All Rights Reserved.
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


/**
 *  The D20IntensityCalibration is a class
 *
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:55 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class D20IntensityCalibration extends IntensityFileCalibration {

  public D20IntensityCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "D20 Intensity cal";
    IDlabel = "D20 Intensity cal";
  }

  public D20IntensityCalibration(XRDcat aobj) {
    this(aobj, "Calibration file x");
  }

  public D20IntensityCalibration() {
    identifier = "D20 Intensity cal";
    IDlabel = "D20 Intensity cal";
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
        boolean endoffile = false;
        boolean found = false;

        linedata = reader.readLine();

        st = new StringTokenizer(linedata, "() ,\t\r\n");
        token = st.nextToken();
        token = st.nextToken();
        datanumber = Integer.valueOf(token).intValue();
        calintensity = new double[datanumber];

        int i = 0;

        while (i < datanumber) {
          linedata = reader.readLine();
          if (linedata == null) {
            endoffile = true;
            break;
          }

          st = new StringTokenizer(linedata, " ,\t\r\n");

          while (st.hasMoreTokens() && i < datanumber) {
            st.nextToken();
            i++;
          }
        }

        i = 0;

        while (i < datanumber) {
          linedata = reader.readLine();
          if (linedata == null) {
            endoffile = true;
            break;
          }

          st = new StringTokenizer(linedata, " ,\t\r\n");

          while (st.hasMoreTokens() && i < datanumber) {
            double calibration = Double.valueOf(st.nextToken()).doubleValue();
            if (calibration == 0.0)
              calibration = 3.86173;
            calintensity[i++] = 1.0 / calibration;
          }
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
    notifyUpObjectChanged(this, Constants.INTENSITY_CALIBRATION, -1);

  }

  public double calibrateData(DiffrDataFile datafile, double x, int index, double coord) {
    if (calintensity == null) {
      if (getFileName() != null && !getFileName().equals("")) {
      } else
        return 1.0;
    }

    return calintensity[index];
  }

  public boolean validX(DiffrDataFile datafile, double x, int index) {
    if (calintensity == null) {
      if (getFileName() != null && !getFileName().equals("")) {
      } else
        return true;
    }

    if (calintensity[index] >= 3.86173)
      return false;
    else
      return true;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JIDCOptionsD(parent, this);
    return adialog;
  }

  class JIDCOptionsD extends JOptionsDialog {

    JTextField filenameL;

    public JIDCOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));

      JPanel jp3 = new JPanel();
      jp3.setLayout(new BorderLayout(6, 6));
      JPanel jp2 = new JPanel();
      jp2.setLayout(new BorderLayout(6, 6));
      principalPanel.add(BorderLayout.NORTH, jp3);
      JPanel jp1 = new JPanel();
      jp1.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
      jp3.add(BorderLayout.WEST, jp1);
      JLabel jl1 = new JLabel("Intensity calibration File: ");
      jp1.add(jl1);
      jp1 = new JPanel();
      jp1.setLayout(new FlowLayout(FlowLayout.RIGHT, 1, 1));
      jp3.add(BorderLayout.EAST, jp1);
      JButton jb = new JIconButton("Open.gif", "Browse...");
      jp1.add(jb);
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          browsethefile();
        }
      });
      jp1 = new JPanel();
      jp1.setLayout(new FlowLayout());
      principalPanel.add(BorderLayout.CENTER, jp1);
      filenameL = new JTextField(40);
      filenameL.setEditable(false);
      jp1.add(filenameL);

      setTitle("Intensity calibration");
      initParameters();

      pack();
    }

    public void initParameters() {
      filenameL.setText(getFileName());
    }

    public void retrieveParameters() {
    }

    public void browsethefile() {
      String filename = loadDataFile(this);
      filenameL.setText(getFileName());

    }

  }

}
