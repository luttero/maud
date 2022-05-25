/*
 * @(#)Calibration.java created 10/07/1998 ILL, Grenoble
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr;

import java.io.*;
import java.lang.*;
import java.util.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import java.net.*;
import java.awt.*;
import javax.swing.*;


/**
 *  The Calibration is a class
 *
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:54 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class Calibration extends it.unitn.ing.rista.diffr.XRDcat {
/*	protected String[] diclistc = {"_riet_meas_datafile_format"};
/*	protected String[] diclistcrm = {"_riet_meas_datafile_format"};

	protected String[] classlistcs = {};
	protected String[] classlistc = {};*/

  public double calcoeff[] = null;
  public int numbercalcoeff = 0;

  public double calibrationData[] = null;
  public int datanumber = 0;

  public Calibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public Calibration(XRDcat aobj) {
    this(aobj, "Calibration file x");
  }

  public Calibration() {
  }

/*	public void initConstant() {
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
    for (int i = 0; i < totsubordinateloop; i++)
      diclistRealMeaning[i] = diclistcrm[i];
		for (int i = 0; i < totsubordinate - totparameterloop; i++)
		  classlists[i] = classlistcs[i];
		for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
		  classlist[i] = classlistc[i];
	}

	public void initParameters()
	{
		super.initParameters();
	}*/

  public String getFileName() {
    return new String("");
  }

/*	public BufferedReader getReader() {
		try {
			return new BufferedReader(new FileReader(new File(getDirectory() + getFileName())));
	  } catch (IOException fe) {
			try {
				return new BufferedReader(new FileReader(new File(RelativeURL.getURL(getFileName()).getFile())));
	  	} catch (IOException e) {
				try {
					return new BufferedReader(new FileReader(new File(getFileName())));
	  		} catch (IOException ie) {
					System.out.println("Second trial fail!");
	  		}
				System.out.println("File not found: "+ getFileName());
	  	}
	  }
	  return null;
	}*/

  public void readall() {
  }

  public void initData(int number) {
    datanumber = number;
    calibrationData = new double[datanumber];
  }

  public void initCoeff(int number) {
    numbercalcoeff = number;
    calcoeff = new double[datanumber];
  }

  public Instrument getInstrument() {
    XRDcat parent = getParent();
    while (parent != null && !(parent instanceof Instrument))
      parent = parent.getParent();
    if (parent != null)
      return (Instrument) parent;
    else
      return null;
  }

  public double calibrateData(double intensity) {
    return intensity;
  }

  public double calibrateData(DiffrDataFile datafile, double x, int index) {
    return 1.0;
  }

	public void calibrateX(DiffrDataFile datafile) {
		int datanumber = datafile.getTotalNumberOfData();
		updateParametertoDoubleBuffering(false);
		for (int i = 0; i < datanumber; i++)
			datafile.setCalibratedXDataOnly(i, ((Instrument) getParent()).getCorrectedCoordinate(datafile.getXDataForCalibration(i)));
	}

	public double calibrateX(DiffrDataFile datafile, double value) {
		return ((Instrument) getParent()).getCorrectedCoordinate(value);
	}

	public double notCalibrated(DiffrDataFile datafile, double x) {
		return x;
	}

	public boolean validX(DiffrDataFile datafile, double x, int index) {
		return true;
	}

	public String getTtheta() {
    return null;
  }

  public String getTtheta(double twotheta) {
    return getTtheta();
  }

  public double getTthetaValue(DiffrDataFile datafile, double twotheta) {
    return twotheta;
  }

  public double getEtaValue(DiffrDataFile datafile) {
    return 0.0;
  }

  public double getDetectorDistanceValue(DiffrDataFile datafile) {
    return 1000.0;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JCalOptionsD(parent, this);
    return adialog;
  }

  public class JCalOptionsD extends JOptionsDialog {

    public JCalOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout());
      principalPanel.add(new JLabel("No options for this model"));

      setTitle("Options panel");
      initParameters();
      pack();
    }

    public void initParameters() {
    }

    public void retrieveParameters() {
    }
  }

}
