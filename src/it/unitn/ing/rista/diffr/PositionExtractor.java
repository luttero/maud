/*
 * @(#)PositionExtractor.java created 16/11/1999 Pergine Vals.
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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
 *  The PositionExtractor is a extract experimental positions from a diffraction spectrum.
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class PositionExtractor extends XRDcat {

  DiffrDataFile thedatafile = null;

  public PositionExtractor(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public PositionExtractor(XRDcat aobj) {
    this(aobj, "Position extractor x");
  }

  public PositionExtractor() {
  }


  public void setDataFile(DiffrDataFile adatafile) {
    thedatafile = adatafile;
  }

  public DiffrDataFile getDataFile() {
    return thedatafile;
  }

  public void extractPositions(Sample asample) {
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JPosExtOptionsD(parent, this);
    return adialog;
  }

  public class JPosExtOptionsD extends JOptionsDialog {

    public JPosExtOptionsD(Frame parent, XRDcat obj) {

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
