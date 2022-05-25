/*
 * @(#)XrayTubeRadiation.java created 06/01/1999 Riva del Garda
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

package it.unitn.ing.rista.diffr.radiation;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;

import javax.swing.*;
import java.awt.*;

/**
 *  The XrayTubeRadiation is a class
 *
 *
 * @version $Revision: 1.5 $, $Date: 2004/08/12 09:36:07 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class XrayTubeRadiation extends XrayRadiation {

  public static final String[] tube = {"Cr Kalpha", "Fe Kalpha", "Cu Kalpha", "Mo Kalpha", "Ag Kalpha", "Co Kalpha"};
  public static final int numbertube = 6;

	// Luca update 31/5/2013 using X-ray International table 4.2.2.1 Chapter 4 section 2, Volume C

  public static final double[][] kalpha = {{2.289726d, 2.293651d}, {1.936041d, 1.939973d}, {1.5405929d, 1.5444274d},
                                           {.70931715d, .713607d}, {.55942178d, .5638131d}, {1.788996d, 1.792835d}};

  public static final double[] weight = {1.0, 0.5};

  public XrayTubeRadiation(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "X-ray tube";
    IDlabel = "X-ray tube";
    description = "X-ray tube radiation";
  }

  public XrayTubeRadiation(XRDcat aobj) {
    this(aobj, "X-ray tube");
  }

  public XrayTubeRadiation() {
    identifier = "X-ray tube";
    IDlabel = "X-ray tube";
    description = "X-ray tube radiation";
  }

  public void initParameters() {
    super.initParameters();
  }

  public String getRadiationTube() {
    checkRadiation();
    Radiation rad = (Radiation) subordinateloopField[0].elementAt(0);
    return XrayTubeRadiation.tube[getSimilarTubeNumber(rad)];
  }

  public void setTheTube(int rad) {
    Radiation theobj;
    int i;
    String astring;

    subordinateloopField[0].removeAllItems();
    for (i = 0; i < 2; i++) {
      astring = getnewString(tube[rad], i + 1);
      theobj = new Radiation(this, astring);
      theobj.setRadiation(astring, kalpha[rad][i], weight[i]);
      subordinateloopField[0].addItem(theobj);
    }
  }

  public void checkRadiation() {
    if (getLinesCount() <= 0)
      setTheTube(2);
  }

  public static final int getSimilarTubeNumber(Radiation rad) {
    return getSimilarTubeNumber(rad.getWavelengthValue());
  }

  public static final int getSimilarTubeNumber(double wave) {
    double mindelta = 0.01;
    int tubenumberis = 0;
    for (int i = 0; i < numbertube; i++) {
      double delta = Math.abs(wave - kalpha[i][0]);
      if (mindelta > delta) {
        mindelta = delta;
        tubenumberis = i;
      }
    }
    return tubenumberis;
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JTubeRadiationOptionsD(parent, this);
    return adialog;
  }

  public class JTubeRadiationOptionsD extends JOptionsDialog {

    JComboBox tubechoice;

    public JTubeRadiationOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new FlowLayout());

      principalPanel.add(new JLabel("X-ray tube:"));

      tubechoice = new JComboBox();
      principalPanel.add(tubechoice);
      tubechoice.setEditable(false);
      tubechoice.setMaximumRowCount(5);

      setTitle("X-ray tube radiation");
      initParameters();
      pack();
    }

    public void initParameters() {
      for (int i = 0; i < numbertube; i++)
        tubechoice.addItem(tube[i]);
      if (getRadiationTube() != "")
        tubechoice.setSelectedItem(getRadiationTube());
    }

    public void retrieveParameters() {
      setTheTube(tubechoice.getSelectedIndex());
    }
  }

}
