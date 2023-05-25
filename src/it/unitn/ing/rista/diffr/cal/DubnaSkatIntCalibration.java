/*
 * @(#)DubnaSkatIntCalibration.java created 13/04/1999 Firenze
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
 *  The DubnaSkatIntCalibration is a class
 *
 *
 * @version $Revision: 1.8 $, $Date: 2006/01/19 14:45:55 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class DubnaSkatIntCalibration extends IntensityCalibration {
  public static String[] diclistc = {"_inst_inc_parameter_file",
                                     "_inst_inc_spectrum_coeff"};
  public static String[] diclistcrm = {"_inst_inc_parameter_file",
                                     "incident spectrum coeff "};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshCalibration = true;

  public DubnaSkatIntCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Dubna/SKAT Incident Spectrum";
    IDlabel = "Dubna/SKAT Incident Spectrum";
  }

  public DubnaSkatIntCalibration(XRDcat aobj) {
    this(aobj, "Dubna/SKAT");
  }

  public DubnaSkatIntCalibration() {
    identifier = "Dubna/SKAT Incident Spectrum";
    IDlabel = "Dubna/SKAT Incident Spectrum";
  }

  public void initConstant() {
    Nstring = 1;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 1;
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
    stringField[0] = new String(filename);
    if (getFileName() != null && !getFileName().equals(""))
      readall();
  }

  public String getFileName() {
    return stringField[0];
  }

  public String getCoeff(int index) {
    return getCoeffP(index).getValue();
  }

  public double getCoeffD(int index) {
    return Double.valueOf(getCoeff(index)).doubleValue();
  }

  public void addCoeff(int index, String value) {
    addparameterloopField(0, new Parameter(this, getParameterString(0, index), value, "0",
            ParameterPreferences.getPref(getParameterString(0, index) + ".min", "-1"),
            ParameterPreferences.getPref(getParameterString(0, index) + ".max", "1"), false));
  }

  public Parameter getCoeffP(int index) {
    return (Parameter) parameterloopField[0].elementAt(index);
  }

  public void setCoeff(int index, String value) {
    getCoeffP(index).setValue(value);
  }

  public String loadDataFile(Frame parent) {
    String filename = Utility.openFileDialog(parent, "Import intensity calibration file",
            FileDialog.LOAD, getDirectory(), null, null);
    if (filename != null) {
      parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
//      relativePathChange = false;
      setFileName(filename);
      parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    }
    return filename;
  }

  public void readall() {
    refreshCalibration = true;
    boolean isAbilitate = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = Misc.getReader(getFileName());
    if (reader != null) {
      try {
        String token = new String("");
        StringTokenizer st = null;
        String linedata = null;
        boolean endoffile = false;
        boolean found = false;
        int ncoeff = 0;

        while (!endoffile) {
          linedata = reader.readLine();
          if (linedata == null) {
            endoffile = true;
            break;
          }

          st = new StringTokenizer(linedata, " ,\t\r\n");

          while (st.hasMoreTokens()) {
            String lasttoken = new String(token);
            token = st.nextToken();
//          	System.out.println(token);
            addCoeff(ncoeff++, token);
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
    notifyUpObjectChanged(this, Constants.INTENSITY_CALIBRATION);
  }

/*
double lpcomp(double w3,int ij1)
{
   double lp, w1,wt,w2,cosmon,t1,t2, cosx, w4;
   int ijt,nsptemp, ijtt, bank;

	nsptemp = nsp;
	if (nsptemp < nsp1 || nsptemp > nsp2)
		nsptemp = nsp1;
   switch (diffraction_type)
   {
     case 0:
       w1 = w3 * pfd6;
       wt = sin(w1);
       w2 = sqrt(1. - wt * wt);
       if (monpol)
         cosmon = parm[paa+pasy+caglioti*(ndiffexp+1)+broaphi+1];
       else
         cosmon = cos(mon);
	     cosx = cos(w1 * 2.);
       lp = ((1. + cosmon * cosmon * cosx * cosx) / (wt * wt * w2));
       break;
     case 1:
	     bank = detector[nsptemp]-1;
	     lp = fabs(sin(tof_theta[bank]*pfd6));
         lp *=  w3 * w3 * w3 * w3;
			 switch (itypg[bank])
			 {
			   case 0:
				   break;
			   case 1:
	         wt = *(sinc+bank*12);
	         w2 = ((difcg[bank] * w3 + difag[bank] * w3 * w3 + zerog[bank])/1000.);
	         w4 = 1.0;
	         for (ijt = 1; ijt < 5; ijt++)
	         {
		         w4 *= w2;
		         ijtt = ijt*2;
		         wt += *(sinc+bank*12+ijtt-1) * exp(- *(sinc+bank*12+ijtt) * w4);
	         }
				 break;
			   case 2:
	         wt = *(sinc+bank*12);
	         w2 = ((difcg[bank] * w3 + difag[bank] * w3 * w3 + zerog[bank])/1000.);
		       w4 = w2 * w2;
		       wt += *(sinc+bank*12+1) * exp(- *(sinc+bank*12+2) / w4) / (w4 * w4 * w2);
	         w4 = w2;
	         for (ijt = 2; ijt < 5; ijt++)
	         {
		         w4 *= w2;
		         ijtt = ijt*2;
		         wt += *(sinc+bank*12+ijtt-1) * exp(- *(sinc+bank*12+ijtt) * w4);
	         }
				 break;
			   default: {
			     printf("Incident intensity function unknown, exiting..\n");
			     fprintf(output_file, "Incident intensity function unknown, exiting..\n");
					 prog_exit(0);
				 }
			 }
	     lp *= wt;
     break;
     case 5:
		 if (read_tof_theta != 0)
			 lp = sin(tof_theta[nsptemp]*pfd6);
		 else
			 lp = 1.0f;
       lp *= pow(w3,4.);

	   wt = parm[pain+4];
	   w2 = ij1;
	   w4 = 1.0f;
	   for (ijt = 1; ijt <= 4; ijt++)
	   {
		   w4 *= w2;
		   ijtt = ijt*2;
		   wt += parm[pain+ijtt+3] * (exp(- parm[pain+ijtt+4] * w4));
	   }
	   lp *= wt;
       break;
     case 3:
     case 6:
		 case 7:
       w1 = w3 * pfd6;
       wt = sin(w1);
       w2 = sqrt(1. - wt * wt);
       if (monpol)
         cosmon = parm[paa+pasy+caglioti*(ndiffexp+1)+broaphi+1];
       else
         cosmon = cos(mon);
       lp = ((1. + pow(cosmon * cos(w1 * 2.), 2.)) / (wt * wt * w2));
       t1=1.7133f-0.0368f*wt*wt;
       t2=-0.097f-0.375f*wt*wt;
       if (parm[paw+2]>1)
         lp *= exp(-parm[paw+2]*0.001*lamb[1]*(t1+t2*lamb[1]));
       break;
     case 4:
       w1 = w3 * pfd6;
       wt = sin(w1);
       w2 = sin(2 * w1);
       lp = 1.f;
       break;
     default:{
       w1 = w3 * pfd6;
       wt = sin(w1);
       w2 = sin(2 * w1);
       lp = 1/wt/w2;
	}
   }
  return (double) lp;
}

*/

  public double calibrateData(DiffrDataFile datafile, double x, int index, double coord) {
    double wt = getCoeffD(0);
    double w4 = 1.0;
//		System.out.println("x:");
//		System.out.println(x);
    int ncoeff = (numberofelementPL(0) + 1) / 2;
    for (int i = 1; i < ncoeff; i++) {
      w4 *= x;
      int i1 = i * 2;
      wt += getCoeffD(i1 - 1) * Math.exp(-getCoeffD(i1) * w4);
    }
    return wt;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JSKATOptionsD(parent, this);
    return adialog;
  }

  class JSKATOptionsD extends JOptionsDialog {

    JTextField filenameL;
    JParameterListPane coeffPanel;

    public JSKATOptionsD(Frame parent, XRDcat obj) {

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
      JLabel jl1 = new JLabel("Instrument Parameter File: ");
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

      coeffPanel = new JParameterListPane(this, false, true);
      principalPanel.add(BorderLayout.SOUTH, coeffPanel);

      setTitle("Dubna/SKAT intensity calibration");
      initParameters();

      pack();
    }

    public void initParameters() {
      filenameL.setText(getFileName());
      coeffPanel.setList(XRDparent, 0);
    }

    public void retrieveParameters() {
      coeffPanel.retrieveparlist();
    }

    public void browsethefile() {
      String filename = loadDataFile(this);
      filenameL.setText(getFileName());

    }

    public void dispose() {
      coeffPanel.dispose();
      super.dispose();
    }

  }

}
