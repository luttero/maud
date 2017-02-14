/*
 * @(#)IntensityExtractor.java created 02/08/1998 Pergine Vals.
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

import java.lang.*;
import java.awt.*;
//import it.unitn.ing.rista.util.Assert;
import it.unitn.ing.rista.awt.*;

import javax.swing.*;


/**
 *  The IntensityExtractor is a extract experimental intensities from a diffraction spectrum.
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:04 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class IntensityExtractor extends XRDcat {

  DiffrDataFile thedatafile = null;

  public IntensityExtractor(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public IntensityExtractor(XRDcat aobj) {
    this(aobj, "Intensity extractor x");
  }

  public IntensityExtractor() {
  }


  public void setDataFile(DiffrDataFile adatafile) {
    thedatafile = adatafile;
  }

  public DiffrDataFile getDataFile() {
    return thedatafile;
  }

  public void extractIntensities(Sample asample) {
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JIntExtOptionsD(parent, this);
    return adialog;
  }

  public class JIntExtOptionsD extends JOptionsDialog {

    public JIntExtOptionsD(Frame parent, XRDcat obj) {

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
