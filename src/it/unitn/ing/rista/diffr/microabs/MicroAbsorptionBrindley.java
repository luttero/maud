/*
 * @(#)MicroAbsorptionBrindley.java created 27/01/2001 Casalino
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

package it.unitn.ing.rista.diffr.microabs;

import java.lang.*;
import java.awt.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.*;

import javax.swing.*;


/**
 *  The MicroAbsorptionBrindley is the class for microabsorption that
 *  implement the Brindley model as described in ....
 *
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class MicroAbsorptionBrindley extends MicroAbsorption {

  public static String[] diclistc = {
    "_pd_micro_absorption_brindley_model"
  };
  public static String[] diclistcrm = {
    "_pd_micro_absorption_brindley_model"
  };

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  public static String[] models = {"linear", "cubic", "spherical"};

  public MicroAbsorptionBrindley(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "Brindley microabsorption";
    IDlabel = "Brindley microabsorption";
    description = "Brindley microabsorption correction is performed";
    initXRD();
  }

  public MicroAbsorptionBrindley(XRDcat aobj) {
    this(aobj, "Brindley microabsorption");
  }

  public MicroAbsorptionBrindley() {
    identifier = "Brindley microabsorption";
    IDlabel = "Brindley microabsorption";
    description = "Brindley microabsorption correction is performed";
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
    stringField[0] = new String(models[0]);
  }

  public int getModelNumber() {
    for (int i = 0; i < models.length; i++) {
      if (models[i].equals(getModel()))
        return i;
    }
    return 0;
  }

  public String getModel() {
    return stringField[0];
  }

  public void setModel(String value) {
    setString(0, value);
//		System.out.println(getModel());
  }

  public double getApparentQuantity(double volFraction, RadiationType rad,
                                    Layer alayer, double crystSize) {

    double muf = 1.0;
    if (crystSize == 0.0)
      return volFraction;

    Phase aphase = (Phase) getParent();

    double mr = (aphase.getAbsorption(rad) * aphase.getDensity() -
            alayer.getAbsorption(rad) * alayer.getDensity()) * crystSize * 1.0E-4;
//	System.out.println(aphase.getPhaseName() + " " + rad.energy + " " + aphase.getAbsorption(rad) + " " +
//			aphase.getDensity() + " " + (alayer.getAbsorption(rad) * alayer.getDensity()) + " " +
//			alayer.getDensity() + " " + crystSize + " " + mr);

    switch (getModelNumber()) {
      case 0:
        muf = (1.0 - Math.exp(-mr)) / mr;
        break;
      case 1:
        muf = (1.0 - Math.exp(-mr)) / mr;
        muf *= muf;
        break;
      case 2:
        double y0 = -0.00229;
        double A1 = 2.0754;
        double t1 = 0.69525;
        double mr0 = -0.50356;
        mr = mr / 2.0;
        muf = (y0 + A1 * Math.exp(-(mr - mr0) / t1));
        break;
      default:
        {
        }
    }
    return volFraction * muf;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JMicroAbsorptionOptionsD(parent, this);
    return adialog;
  }

  public class JMicroAbsorptionOptionsD extends JOptionsDialog {

    JComboBox modelCB = null;

    public JMicroAbsorptionOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout());
      principalPanel.add(new JLabel("Brindley model: "));

      modelCB = new JComboBox();
      for (int i = 0; i < models.length; i++)
        modelCB.addItem(models[i]);
      principalPanel.add(modelCB);
//			modelCB.addActionListener(new

      setTitle("Options panel");
      initParameters();
      pack();
    }

    public void initParameters() {
      modelCB.setSelectedItem(getModel());
    }

    public void retrieveParameters() {
      setModel(modelCB.getSelectedItem().toString());
    }
  }

}
