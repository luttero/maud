/*
 * @(#)PhiPrecessionOverChiOmega.java created Jul 16, 2008 Caen
 *
 * Copyright (c) 2008 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.diffr.PrecessionError;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.MoreMath;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.awt.JOptionsDialog;

import java.awt.*;

/**
 * The PhiPrecessionOverChiOmega is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jul 16, 2008 7:36:27 AM $
 * @since JDK1.1
 */
public class PhiPrecessionOverChiOmega  extends SpecimenPrecessionError {

/*  protected static String[] diclistc = {
    "_pd_spec_precession_error_radius", "_pd_spec_precession_error_angle"
  };
  protected static String[] diclistcrm = {
    "radius (mm)", "starting angle (deg)"
  };

  protected static String[] classlistc = {};

  protected static String[] classlistcs = {};

  public PhiPrecessionOverChiOmega(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Phi precession";
    IDlabel = "Phi precession";
    description = "Specimen precession error in the phi rotation plane";
  }

  public PhiPrecessionOverChiOmega(XRDcat aobj) {
    this(aobj, "Specimen precession error in the phi rotation plane");
  }

  public PhiPrecessionOverChiOmega() {
    identifier = "Phi precession";
    IDlabel = "Phi precession";
    description = "Specimen precession error in the phi rotation plane";
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 2;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    for (int i = 0; i < totsubordinateloop; i++)
      diclistRealMeaning[i] = diclistcrm[i];
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
  }

  public void initParameters() {
    super.initParameters();
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      for (int i = 0; i < parameterField.length; i++) {
        if (source == parameterField[i]) {
          notifyParameterChanged(source, Constants.ERROR_POSITION_CHANGED);
          return;
        }
      }
      super.notifyParameterChanged(source);
    }
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    super.updateParametertoDoubleBuffering(false);
    FilePar filepar = getFilePar();
    if (filepar == null || filepar.isLoadingFile() || !isAbilitatetoRefresh)
      return;
    for (int i = 0; i < 3; i++)
      xyz[i] = 0.0;
  }

  public double[] getXYZForPrecession(double[] angles, double x) {

    if (parameterValues[0] != 0.0) {
      double phi = angles[2] + parameterValues[1];
      xyz[0] = parameterValues[0] * MoreMath.cosd(phi);
      xyz[1] = parameterValues[0] * MoreMath.sind(phi);
      xyz[2] = 0.0;

      double coschi = MoreMath.cosd(angles[1]);
      double sinchi = MoreMath.sind(angles[1]);
      xyz[1] = xyz[1] * coschi + xyz[2] * sinchi;
      xyz[2] = -xyz[1] * sinchi + xyz[2] * coschi;
      double cosomega = MoreMath.cosd(angles[0]);
      double sinomega = MoreMath.sind(angles[0]);
      xyz[0] = xyz[0] * cosomega + xyz[1] * sinomega;
      xyz[1] = -xyz[0] * sinomega + xyz[1] * cosomega;
    }
    return xyz;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JPhiPrecessionErrorOptionsD(parent, this);
    return adialog;
  }

  public class JPhiPrecessionErrorOptionsD extends JOptionsDialog {

    public JPhiPrecessionErrorOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new GridLayout(0, 1, 6, 6));

      String labels[] = {"Precession radius: ", "Precession starting angle: "};

      for (int i = 0; i < labels.length; i++)
        principalPanel.add(createEditParField(new GridLayout(1, 2, 5, 5),
                labels[i],
                parameterField[i]));
      setTitle("Phi specimen precession error options panel");
      pack();
    }

  }*/

}
