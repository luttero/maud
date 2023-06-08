/*
 * @(#)HippoSpecimenPrecessionError.java created 08/10/2002 Mesiano
 *
 * Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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

import java.awt.*;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

/**
 *  The HippoSpecimenPrecessionError is a class
 *
 *
 * @version $Revision: 1.6 $, $Date: 2005/05/06 18:07:25 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class HippoSpecimenPrecessionError extends SpecimenPrecessionError {

  protected static String[] diclistc = {
    "_pd_spec_precession_error_radius", "_pd_spec_precession_error_angle"
  };
  protected static String[] diclistcrm = {
    "radius (mm)", "starting angle (deg)"
  };

  protected static String[] classlistc = {};

  protected static String[] classlistcs = {};

  public HippoSpecimenPrecessionError(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Hippo precession";
    IDlabel = "Hippo precession";
    description = "Hippo specimen precession error in the omega rotation plane";
  }

  public HippoSpecimenPrecessionError(XRDcat aobj) {
    this(aobj, "Hippo specimen precession error in the omega rotation plane");
  }

  public HippoSpecimenPrecessionError() {
    identifier = "Hippo precession";
    IDlabel = "Hippo precession";
    description = "Hippo specimen precession error in the omega rotation plane";
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
      double omega = angles[0] + parameterValues[1];
      xyz[0] = parameterValues[0] * MoreMath.cosd(omega);
      xyz[2] = 0.0;
      xyz[1] = parameterValues[0] * MoreMath.sind(omega);
    }
    return xyz;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JHippoPrecessionErrorOptionsD(parent, this);
    return adialog;
  }

  public class JHippoPrecessionErrorOptionsD extends JOptionsDialog {

    public JHippoPrecessionErrorOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new GridLayout(0, 1, 6, 6));

      String labels[] = {"Precession radius: ", "Precession starting angle: "};

      for (int i = 0; i < 2; i++)
        principalPanel.add(createEditParField(new GridLayout(1, 2, 5, 5),
                labels[i],
                parameterField[i]));
      setTitle("Hippo specimen precession error options panel");
      pack();
    }

  }
}
