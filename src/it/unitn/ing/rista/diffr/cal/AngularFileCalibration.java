/*
 * @(#)AngularFileCalibration.java created 10/07/1998 ILL, Grenoble
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.cal;

import it.unitn.ing.rista.diffr.*;

import java.io.*;
import java.lang.*;
import java.util.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import java.awt.*;
import javax.swing.*;
import java.awt.event.*;


/**
 *  The AngularFileCalibration is a class
 *
 *
 * @version $Revision: 1.8 $, $Date: 2006/01/19 14:45:55 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class AngularFileCalibration extends AngularCalibration {

  public static String[] diclistc = {"_inst_ang_parameter_file"};
  public static String[] diclistcrm = {"_inst_ang_parameter_file"};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshCalibration = true;

  int datanumber = 0;
  double[] calangular = null;
  boolean relativePathChange = false;

  public AngularFileCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = ".acal";
    IDlabel = "Exp angular";
  }

  public AngularFileCalibration(XRDcat aobj) {
    this(aobj, "Calibration file x");
  }

  public AngularFileCalibration() {
    identifier = ".acal";
    IDlabel = "Exp angular";
  }

  public void initConstant() {
    Nstring = 1;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
  }

  public void initParameters() {
    super.initParameters();
    setFileName("");
  }

  public void setFileName(String filename) {
    if (filename != null && !filename.equals("") && !relativePathChange) {
      String[] folderandname = Misc.getFolderandName(filename);
      if (!folderandname[0].startsWith("//")) {
        folderandname[0] = Misc.getAbsolutePath(folderandname[0], getDirectory());
      }
      stringField[0] = new String(folderandname[0] + folderandname[1]);
      relativePathChange = true;
      if (getFileName() != null && !getFileName().equals(""))
        readall();
    }
  }

  public String getFileName() {
    setFileName(stringField[0]);
    return stringField[0];
  }

  public String loadDataFile(Frame parent) {
    String filename = Utility.openFileDialog(parent, "Import angular calibration file",
            FileDialog.LOAD, getDirectory(), null, null);
    if (filename != null) {
      parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
      relativePathChange = false;
      setFileName(filename);
      parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    }
    return filename;
  }

  public String getNameRelativeToPar(String field) {
    String[] folderandname = Misc.getFolderandName(field);
    StringBuffer filenamewithPath = new StringBuffer(
            Misc.getRelativePath(getDirectory(), folderandname[0]));
    filenamewithPath.append(folderandname[1]);
    return filenamewithPath.toString();
  }

  public void writeField(BufferedWriter out, String dicstring, String field) {
    try {
      if (dicstring.equals(diclist[0])) {
        if (field.equals(""))
          field = new String("?");
        else
          field = getNameRelativeToPar(getFileName());
        out.write(dicstring);
        out.write(" ");
        if (field.indexOf(' ') >= 0)
          out.write("'" + field + "'");
        else
          out.write(field);
        out.newLine();
      } else
        super.writeField(out, dicstring, field);
    } catch (IOException ioe) {
      System.out.println("Error in writing the object " + toXRDcatString());
    }

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

        while (!endoffile) {
          linedata = reader.readLine();
          if (linedata == null) {
            endoffile = true;
            break;
          }

          st = new StringTokenizer(linedata, "' ,\t\r\n");

          while (st.hasMoreTokens()) {
            String lasttoken = new String(token);
            token = st.nextToken();
//          	System.out.println(token);
            angular.addElement(token);
          }
        }

        datanumber = angular.size();
        calangular = new double[datanumber];
        for (int i = 0; i < datanumber; i++) {
          String tmp = (String) angular.elementAt(i);
          calangular[i] = Double.valueOf(tmp).doubleValue();
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

  public void calibrateData(DiffrDataFile datafile) {
  }

  public void calibrateX(DiffrDataFile datafile) {
    if (calangular == null) {
      if (getFileName() != null && !getFileName().equals("")) {
      } else
        return;
    }

    int datanumber = datafile.getTotalNumberOfData();
//		updateParametertoDoubleBuffering();
    for (int i = 0; i < datanumber; i++) {
      datafile.setCalibratedXDataOnly(i, calangular[i]);
    }
  }

  public double notCalibrated(DiffrDataFile datafile, double x) {

    if (calangular == null) {
      if (getFileName() != null && !getFileName().equals("")) {
      } else
        return 0.0;
    }

    int datanumber = calangular.length;
    int i = 0;

    while (i++ < datanumber - 1 && calangular[i] < x) {
    }

    double diff1 = Math.abs(calangular[i - 1] - x);
    double diff2 = Math.abs(calangular[i] - x);
    double diff3 = Math.abs(calangular[i + 1] - x);

    if (diff1 < diff2)
      return datafile.getXDataForCalibration(i - 1);
    if (diff3 < diff2)
      return datafile.getXDataForCalibration(i + 1);
    return datafile.getXDataForCalibration(i);

  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JAFCOptionsD(parent, this);
    return adialog;
  }

  class JAFCOptionsD extends JOptionsDialog {

    JTextField filenameL;

    public JAFCOptionsD(Frame parent, XRDcat obj) {

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
      JLabel jl1 = new JLabel("Angular calibration File: ");
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

      setTitle("Angular calibration");
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
