/*
 * @(#)MarchDollaseTexture.java created 16/07/1998 ILL, Grenoble
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

package it.unitn.ing.rista.diffr.rta;

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.awt.JSubordListPane;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.ParameterPreferences;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.border.BevelBorder;
import java.awt.*;

/**
 *  The MarchDollaseTexture is a class
 *
 *
 * @version $Revision: 1.9 $, $Date: 2005/05/06 18:07:27 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class MarchDollaseTexture extends Texture {
  public static String[] diclistc = {"_rita_odf_background",

                                     "_riet_par_pref_or_march_dollase"};
  public static String[] diclistcrm = {"Texture random value",

                                       "March-Dollase component"};

  public static String[] classlistc = {"it.unitn.ing.rista.diffr.rta.MarchDollaseComponent"};
  public static String[] classlistcs = {};

  double background = 0.0;

  public MarchDollaseTexture(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();

    identifier = new String("March-Dollase");
    IDlabel = new String("March-Dollase");
    description = new String("select this to apply March-Dollase model");
  }

  public MarchDollaseTexture(XRDcat aobj) {
    this(aobj, "March-Dollase model");
  }

  public MarchDollaseTexture() {
    identifier = new String("March-Dollase");
    IDlabel = new String("March-Dollase");
    description = new String("select this to apply March-Dollase model");
  }

  public void initConstant() {
    //	to be overrided in subclasses
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 1;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 1;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
  }

  public void initParameters() {
    super.initParameters();
    parameterField[0] = new Parameter(this, getParameterString(0), 0.5,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", 0),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 1));
    parameterField[0].setPositiveOnly();
    refreshComputation = true;
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      if (parameterField != null)
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
            notifyParameterChanged(source, Constants.TEXTURE_CHANGED);
            return;
        }
      }
      if (parameterloopField != null)
      for (int j = 0; j < parameterloopField.length; j++)
        for (int i = 0; i < parameterloopField[j].size(); i++)
          if (source == parameterloopField[j].elementAt(i)) {
            notifyParameterChanged(source, Constants.TEXTURE_CHANGED);
            return;
          }

      super.notifyParameterChanged(source);
    }
  }

  public void initializeReflexes(Sample asample) {}

  public void refreshForNotificationDown(XRDcat source, int reason) {
    if (!getFilePar().isComputingDerivate() || (source == this ||
            (reason == Constants.SAMPLE_ORIENTATION_CHANGED ||
            reason == Constants.CELL_CHANGED ||
                reason == Constants.TEXTURE_CHANGED)))
      refreshComputation = true;
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);

    parameterField[0].setPositiveOnly();
  }

  public Parameter getTextureBackground() {
    return parameterField[0];
  }

  public void update(boolean firstLoading) {
    background = Math.abs(getTextureBackground().getValueD());
  }

  public int marchDollaseComponentsNumber() {
    return numberofelementSubL(0);
  }

  public MarchDollaseComponent getMarchDollaseComponent(int index) {
    return (MarchDollaseComponent) subordinateloopField[0].elementAt(index);
  }

  public void computeTextureFactor(Phase aphase, Sample asample) {
    aphase.sghklcompute(false);
    if (refreshComputation) {
      refreshComputation = false;
      for (int k = 0; k < marchDollaseComponentsNumber(); k++)
        getMarchDollaseComponent(k).computeTextureFactor(aphase, asample);
      int hkln = aphase.gethklNumber();
	    double[] textF = new double[hkln];
      for (int i = 0; i < hkln; i++) {
        Reflection refl = aphase.getReflectionVector().elementAt(i);
        double totTexture = background;
        double totWeight = background;
        for (int k = 0; k < marchDollaseComponentsNumber(); k++) {
          double textureFactor = getMarchDollaseComponent(k).textureFactorComp(aphase, refl);
          double weight = getMarchDollaseComponent(k).getWeightFactorValue();
          totTexture += weight * textureFactor;
          totWeight += weight;
        }
	      textF[i] = totTexture / totWeight;
      }
	    for (int j = 0; j < asample.activeDatasetsNumber(); j++) {
		    DataFileSet dataset = asample.getActiveDataSet(j);
		    for (int k = 0; k < dataset.activedatafilesnumber(); k++) {
			    DiffrDataFile datafile = dataset.getActiveDataFile(k);
//			      for (int ppp = 0; ppp < datafile.positionsPerPattern; ppp++)
			    datafile.setTextureFactors(aphase, textF);
		    }
	    }
    }
  }

  public void computeTextureFactor(Sample asample) {
    Phase aphase = (Phase) getParent();
    computeTextureFactor(aphase, asample);
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JMDParameterPanel(parent, this);
    return adialog;
  }

  class JMDParameterPanel extends JOptionsDialog {

    JTextField backGroundODFTF;
    JSubordListPane marchDollaseComponentPanel;

    public JMDParameterPanel(Frame parent, XRDcat obj) {
      super(parent, obj);

      principalPanel.setLayout(new BorderLayout(3, 3));

      JPanel upperPanel = new JPanel();
      principalPanel.add(BorderLayout.NORTH, upperPanel);
      upperPanel.add(new JLabel("ODF background: "));
      backGroundODFTF = new JTextField(Constants.FLOAT_FIELD);
      upperPanel.add(backGroundODFTF);

      marchDollaseComponentPanel = new JSubordListPane(this, false);
      marchDollaseComponentPanel.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "March-Dollase components"));
      principalPanel.add(BorderLayout.CENTER, marchDollaseComponentPanel);

      setTitle("March-Dollase Texture");
      initParameters();
      pack();
    }

    public void initParameters() {
      String[] labels = new String[5];
      labels[0] = "h";
      labels[1] = "k";
      labels[2] = "l";
      labels[3] = "March-Dollase coefficient";
      labels[4] = "weight";
      marchDollaseComponentPanel.setList(MarchDollaseTexture.this, 0, 5, labels);
      addComponenttolist(backGroundODFTF, parameterField[0]);
    }

    public void retrieveParameters() {
      marchDollaseComponentPanel.retrieveparlist();
      parameterField[0].setValue(backGroundODFTF.getText());
      removeComponentfromlist(backGroundODFTF);
    }
  }

}

