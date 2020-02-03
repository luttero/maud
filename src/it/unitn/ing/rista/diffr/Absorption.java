/*
 * @(#)Absorption.java created 06/01/1999 Riva del Garda
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

package it.unitn.ing.rista.diffr;

import java.lang.*;
import java.awt.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import javax.swing.*;


/**
 *  The Absorption is a class
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:04 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class Absorption extends XRDcat {

  public Absorption(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public Absorption(XRDcat aobj) {
    this(aobj, "Absorption x");
  }

  public Absorption() {
  }
  
  public double getAbsorptionCorrection(DiffrDataFile adatafile, Phase aphase, double positionOrEnergy) {
    Instrument inst = (Instrument) getParent();
    Sample asample = adatafile.getDataFileSet().getSample();
    double[] sampleAngles = asample.getSampleAngles();
    double[] tilting_angles = adatafile.getTiltingAngle();
    double omega = tilting_angles[0];
    tilting_angles[0] = inst.getMeasurement().getOmega(omega, positionOrEnergy);
    double[] angles = inst.getGeometry().getIncidentAndDiffractionAngles(adatafile, tilting_angles, sampleAngles, positionOrEnergy);
    
    int phaseindex = asample.getPhase(aphase);
    DataFileSet adataset = adatafile.getDataFileSet();
    int datasetIndex = adataset.getDataFileSetIndex();
    double correction = 0;
    RadiationType rad = adataset.getInstrument().getRadiationType();
    for (int i = 0; i < asample.numberOfLayers; i++) {
      Layer alayer = asample.getlayer(i);
      double quantity = asample.phaseQuantity[i][phaseindex][datasetIndex];
      if (quantity >= 0.0) {
        double absCorrection = getLayerAbsorption_new(rad, alayer, angles, adataset);
        correction += quantity * absCorrection;
//				System.out.println("Abs layer corr: " + absCorrection + " " + quantity);
      }
    }
    double toLambda = inst.getLambdaForTOF(adatafile, positionOrEnergy);
    return correction; // todo: * computeShapeAbsorptionCorrection(asample, inst.getRadiationType(), rad_index, angles, positionOrEnergy, toLambda);
  }
  
  public double getLayerAbsorption_new(RadiationType rad, Layer alayer, double[] incidentDiffractionAngles,
                                       DataFileSet adataset) {
    
    // todo what about Debye-Scherrer
    double expTransmission = 0.0;
    double expAbsorption = 1.0;
    
    double radAbs;
  
    boolean isNeutron = rad.isNeutron();
    boolean isElectron = rad.isElectron();
    
    if (incidentDiffractionAngles[0] < 1.0E-5 || incidentDiffractionAngles[2] < 1.0E-5)
      radAbs = 0;
    else {
      double sinIncAngle = Math.abs(Math.sin(incidentDiffractionAngles[0]));
      double sinDiffAngle = Math.abs(Math.sin(incidentDiffractionAngles[2]));
      double factor = 1.0 / sinIncAngle + 1.0 / sinDiffAngle;
      double absorption = alayer.getLayerAbsorptionForXray(rad.energy);
      double absorptionLayer = factor * absorption;
      // alayer.getLayerAbsorption(rad, sinIncAngle, sinDiffAngle);
      if (incidentDiffractionAngles[2] > 0.0) {
        double transmission = alayer.getOverLayerAbsorptionForXray(rad.energy) * factor;
//    System.out.println("transmission: " + transmission + " layer " + layerIndex);
        if (transmission < 200)
          expTransmission = Math.exp(-transmission);
        if (absorptionLayer < 200)
          expAbsorption = 1.0 - Math.exp(-absorptionLayer);
      }

//	  System.out.println("Layer: " + alayer.toString() + " " + absorption);
      if (absorption >= 0.0) {
//      System.out.println("abs = " + sinDiffAngle + " * " + alayer.getThicknessValue() + " / (" + absorption +
//          " * (" + sinIncAngle + " + " + sinDiffAngle + " )) * " +
//          expTransmission + " * " + expAbsorption);
        radAbs = 2.0 * sinDiffAngle * alayer.getThicknessValue() / (absorption * (sinIncAngle + sinDiffAngle)) *
            expTransmission * expAbsorption * adataset.getMeanAbsorption();
      } else
        radAbs = 1.0;
    }
    
    return radAbs;
  }
  
  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JAbsorptionOptionsD(parent, this);
    return adialog;
  }

  public class JAbsorptionOptionsD extends JOptionsDialog {

    public JAbsorptionOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout());
      principalPanel.add(new JLabel("No options for this absorption"));

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
