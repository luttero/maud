/*
 * @(#)TOFMultiDetector.java created Nov 1, 2004 ISIS
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.detector;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.io.BufferedReader;
import java.io.IOException;
import java.util.StringTokenizer;


/**
 * The TOFMultiDetector is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2006/11/10 09:33:01 $
 * @since JDK1.1
 */

public class TOFMultiDetector extends Detector {

  public static String modelID = "Multi TOF detector";

  public static String[] diclistc = {"_instrument_counter_bank_ID"};
  public static String[] diclistcrm = {"_instrument_counter_bank_ID"};

  protected static String[] classlistc = {"it.unitn.ing.rista.diffr.detector.TOFDetector"};
  public static String[] classlistcs = {};

  public TOFMultiDetector(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = modelID;
    IDlabel = modelID;
    description = modelID;
  }

  public TOFMultiDetector(XRDcat aobj) {
    this(aobj, modelID);
  }

  public TOFMultiDetector() {
    identifier = modelID;
    IDlabel = modelID;
    description = modelID;
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 1;
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
  }

  public int detectorsNumber() {
    return numberofelementSubL(0);
  }

  public ListVector getDetectorList() {
    return subordinateloopField[0];
  }

  public TOFDetector getDetector(int index) {
    if (index >= 0 && index < detectorsNumber())
      return (TOFDetector) getDetectorList().elementAt(index);
    else
      return null;
  }

  public TOFDetector addDetector() {
    return (TOFDetector) addsubordinateloopField(0, new TOFDetector(this));
  }

  public TOFDetector addDetector(int index) {
    if (index >= 0)
      return (TOFDetector) addsubordinateloopField(0, new TOFDetector(this), index);
    else
      return addDetector();
  }

  public int getBankNumber(DiffrDataFile datafile) {
    return datafile.getAngBankNumber();
  }

  public int getBankNumber(String bankID) {
//    System.out.println("BankID from datafile: " + bankID);
    for (int i = 0; i < detectorsNumber(); i++) {
//      System.out.println("Detector: " + i + ": " + getDetector(i).getString(0));
      if (getDetector(i).getString(0).equalsIgnoreCase(bankID))
        return i;
    }
    return -1;
  }

  public double getThetaDetector(DiffrDataFile datafile, double twotheta) {
//    System.out.println("Detector theta: " + getBankNumber(datafile) + " " + getDetector(getBankNumber(datafile)).getThetaDetector());
    return getDetector(getBankNumber(datafile)).getThetaDetector();
  }

  public double getEtaDetector(DiffrDataFile datafile) {
    return (double) getDetector(getBankNumber(datafile)).getEtaDetector();
  }

  public void loadGemRotaxParameters(Frame aframe) {
    String filename = Utility.browseFilename(aframe, "Choose the GEM-Rotax grouping file");
    loadGemRotaxParameters(filename, true);
  }

  public void gemRotaxDropped(java.io.File[] files) {
    boolean dropOld = true;
    if (files != null && files.length > 0) {
      if (files.length > 1)
        dropOld = false;
      for (int i = 0; i < files.length; i++) {
        try {
          loadGemRotaxParameters(files[i].getCanonicalPath(), dropOld);
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
    }
  }

  public void loadGemRotaxParameters(String filename, boolean dropOld) {
    BufferedReader reader = Misc.getReader(filename);
    if (reader != null) {
      if (dropOld)
        getDetectorList().removeAllItems();
      try {
        String token, tokenG;
        String line;
        StringTokenizer st;

        line = reader.readLine();
        while (line != null) {
          st = new StringTokenizer(line, " ,:\t\r\n");

          token = st.nextToken();
          token = st.nextToken();
          token = st.nextToken();

          int groupNumber = Integer.valueOf(tokenG = st.nextToken()).intValue();
          double thetaAngle = Double.valueOf(token = st.nextToken()).doubleValue();
          double etaAngle = Double.valueOf(token = st.nextToken()).doubleValue();
          TOFDetector tofdetc = addDetector();
          tofdetc.setTheta(thetaAngle);
          tofdetc.setEta(etaAngle);
          tofdetc.setString(0, "Group" + tokenG);
          tofdetc.setLabel("Group" + tokenG);

          line = reader.readLine();
        }
      } catch (IOException e) {
        e.printStackTrace();
        System.out.println("Error in loading the grouping file!");
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
    }
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new TOFMultiDetector.JMultiBankTOFDetectorOptionsD(parent, this);
    return adialog;
  }

  public class JMultiBankTOFDetectorOptionsD extends JOptionsDialog {

    JSubordListPane detectorP;

    public JMultiBankTOFDetectorOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(6, 6));
      detectorP = new JSubordListPane(this, false);
      principalPanel.add(BorderLayout.CENTER, detectorP);
      new FileDrop(principalPanel, new FileDrop.Listener() {
        public void filesDropped(java.io.File[] files) {
          // handle file drop
          gemRotaxDropped(files);
        }   // end filesDropped
      }); // end FileDrop.Listener


      JPanel buttonP = new JPanel();
      principalPanel.add(BorderLayout.NORTH, buttonP);
      JButton loadB = new JButton("Load bank parameters (Gem-Rotax)");
      buttonP.add(loadB);
      loadB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          loadGemRotaxParameters(JMultiBankTOFDetectorOptionsD.this);
        }
      });

      initParameters();

      setTitle("Multi TOF detector characteristics");
      pack();
    }

    public void initParameters() {
      String labels[] = {"TOF theta angle (degrees):", "Eta angle (degrees):",
                         "Detector efficiency:", "Sample-detector distance (m):"};
      detectorP.setList(TOFMultiDetector.this, 0, 4, labels);
    }

    public void retrieveParameters() {
      detectorP.retrieveparlist();
    }

  }

}
