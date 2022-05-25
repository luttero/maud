/*
 * @(#)SizeStrainSymBFrules.java created 09/10/1998 Mesiano
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

package it.unitn.ing.rista.diffr.sizestrain;

import it.unitn.ing.rista.diffr.*;

/**
 *  The SizeStrainSymBFrules is a class
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:08 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SizeStrainSymBFrules extends SizeStrainSymDefault {

  public SizeStrainSymBFrules(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Disabled B-factors rules";
    IDlabel = "B-factors rules";
    description = "select this to apply B-Factor model";
  }

  public SizeStrainSymBFrules(XRDcat aobj) {
    this(aobj, "Size-Strain symmetry B-Factor model");
  }

  public SizeStrainSymBFrules() {
    identifier = "Disabled Disabled B-factors rules";
    IDlabel = "B-factors rules";
    description = "select this to apply B-Factor model";
  }
  /*
  void gotoIso() {
    double md1,md2,md3,mdmean;
    double mm1,mm2,mm3,mmmean;

    if (!isocryst) {
      isocryst = true;
      md1 = Float.valueOf(m[0].getText()).floatValue();
      md2 = Float.valueOf(m[1].getText()).floatValue();
      md3 = Float.valueOf(m[2].getText()).floatValue();
      mm1 = Float.valueOf(e[0].getText()).floatValue();
      mm2 = Float.valueOf(e[1].getText()).floatValue();
      mm3 = Float.valueOf(e[2].getText()).floatValue();
      mdmean = (md1 + md2 + md3)/3;
      mmmean = (mm1 + mm2 + mm3)/3;
      setIsotropic(String.valueOf(mdmean), String.valueOf(mmmean));
      isotropicRB.setSelected(true);
    }
  }

  void gotoAniso() {
    if (isocryst) {
      isocryst = false;
      setAnisotropic(crystallite.getText(), microstrain.getText());
      anisotropicRB.setSelected(true);
    }
  }

  */
}
