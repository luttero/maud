/*
 * @(#)PlotRadialPoleFigure.java created Oct 21, 2009 Caen
 *
 * Copyright (c) 2009 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.awt;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Constants;

import java.awt.*;

/**
 * The PlotRadialPoleFigure is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Oct 21, 2009 7:46:22 PM $
 * @since JDK1.1
 */
public class PlotRadialPoleFigure {

  public PlotRadialPoleFigure(Frame parent, Sample asample, Reflection[] pole, int mode,
                        int numberofPoints, double zoom, double filterWidth,
                        boolean grayScale, double maxAngle, boolean logScale, int colrsNumber,
                              boolean equalArea) {

//    double[][] PF = null;
    for (int i = 0; i < pole.length; i++) {
      double[] x = new double[numberofPoints];
      double[] y = new double[numberofPoints];
      double stepx = maxAngle / (numberofPoints - 1);
      for (int j = 0; j < numberofPoints; j++) {
        x[j] = stepx * j;
        y[j] = 0.0;
      }
      Phase aphase = pole[i].getParent();
      Texture texturemodel = aphase.getActiveTexture();
      if (texturemodel != null) {
        if (equalArea) {
          y = texturemodel.getPoleFigureGridRadial(pole[i], x, y);
          for (int ij = 0; ij < x.length; ij++) {
            x[ij] = Math.asin(x[ij] / Constants.sqrt2) * 90.0;
          }
        } else {
          y = texturemodel.getPoleFigureGridPolar(pole[i], x, y);
          for (int ij = 0; ij < x.length; ij++) {
            x[ij] = x[ij] * 90.0;
          }
        }
      }
      if (y != null)
        plotFunction(parent, x, y, equalArea);
    }
  }

  public void plotFunction(Frame theframe, double[] x, double[] y, boolean equalArea) {
    String title = "Radial distribution";
    if (!equalArea)
      title = "Polar distribution";

    PlotSimpleData radialPlot = new PlotSimpleData(theframe, x, y, 1, false, "Polar angle", "PF", title);
    radialPlot.setXaxisTitle("Polar angle");
    radialPlot.setYaxisTitle("PF");
    radialPlot.setVisible(true);

  }


}



