/*
 * @(#)PlotParameterFunction.java created 21/03/1999 Pergine Vals.
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

package it.unitn.ing.rista.awt;

import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.net.URL;

import it.unitn.ing.jgraph.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.util.function.*;

/**
 * The PlotParameterFunction will display the graph of a polynomial function.
 *
 * @version $Revision: 1.3 $, $Date: 2006/01/19 14:45:53 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class PlotParameterFunction extends GraphFrame {

//      URL markerURL;

  public PlotParameterFunction(Frame parent, ParameterFunction function) {

    super(parent);

    frameWLabel = "plotParFunction.frameWidth";
    frameHLabel = "plotParFunction.frameHeight";
    defaultFrameW = 600;
    defaultFrameH = 400;
    setOwnSize = true;
    framePositionX = "plotParFunction.framePositionX";
    framePositionY = "plotParFunction.framePositionY";
    defaultFramePositionX = 10;
    defaultFramePositionY = 20;
    setOwnPosition = true;

    createDefaultMenuBar();

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout(6, 6));

    int i;
    int j;
/*
**      Get the passed parameters
*/

/*
**      Create the Graph instance and modify the default behaviour
*/
    graph = new G2Dint();
    G2Dint lgraph = (G2Dint) graph;

    lgraph.drawzero = false;
    lgraph.drawgrid = false;
    lgraph.borderTop = 20;
/*
**      Load a file containing Marker definitions
*/
//        System.out.println("Loading markers....");

    Markers marker = null;
    try {
      marker = new Markers(Constants.documentsDirectory + "marker.txt");
    } catch (java.io.IOException ioe) {
      ioe.printStackTrace();
    }

    if (marker != null)
      lgraph.setMarkers(marker);

/*
**      Retrieve the data Set.
*/

    if (function != null) {

      c1.add("Center", lgraph);
      setTitle("Function plot");
//          System.out.println("Loading data....");
      int np = 1001;
      double min = function.getMin();
      double max = function.getMax();
      double stepx = (max - min) / (np - 1);
//          System.out.println(min + " " + max + " " + stepx);

      double data[] = new double[2 * np];
      for (i = j = 0; i < np; i++, j += 2) {
        double xcoord = i * stepx + min;
        data[j] = xcoord;
        data[j + 1] = function.f(xcoord);
      }

//          System.out.println("Data loaded");

      DataSet data1 = lgraph.loadDataSet(data, np);

/*
**      Attach data sets to the Xaxis
*/
//          System.out.println("Attaching X-axis....");

      Axis xaxis = lgraph.createXAxis();
      xaxis.attachDataSet(data1);
      xaxis.setTitleText("x");
      xaxis.setTitleFont(new Font("TimesRoman", Font.BOLD, 14));
      xaxis.setLabelFont(new Font("Helvetica", Font.PLAIN, 12));
      xaxis.setTitleColor(new Color(0, 0, 255));
/*
**      Attach the first data set to the Left Axis
*/
//          System.out.println("Attaching Y-axis....");

      Axis yaxis = lgraph.createYAxis();
      yaxis.attachDataSet(data1);
      yaxis.setTitleText("Function");
      yaxis.setTitleFont(new Font("TimesRoman", Font.BOLD, 14));
      yaxis.setLabelFont(new Font("Helvetica", Font.PLAIN, 12));
      yaxis.setTitleColor(new Color(0, 0, 255));
/*
**      Attach the second data set to the Right Axis
*/
//          System.out.println("Plotting....");
    } else
      c1.add("South", new Label("No function available!"));
    setComponentToPrint(graph);

  }

}
