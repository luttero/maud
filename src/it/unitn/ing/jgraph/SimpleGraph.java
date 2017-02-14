/*
 * @(#)SimpleGraph.java created Dec 8, 2005 Casalino
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.jgraph;

import it.unitn.ing.rista.util.Constants;

import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.util.Vector;


/**
 * The SimpleGraph is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/01/19 15:51:49 $
 * @since JDK1.1
 */

public class SimpleGraph extends JPanel {

  Graph2D graph = null;
  JPanel fullGraphPanel = null;
  public String xLabel = "x";
  public String yLabel = "y";
  public Vector data = null;
  int markerType = 1;

  public SimpleGraph(Vector adata, String xlabel, String ylabel,
                     int marker) {

    initializeFrame();

    xLabel = xlabel;
    yLabel = ylabel;
    markerType = marker;

    createGraph(adata);

  }

  public SimpleGraph(Vector adata) {

    initializeFrame();

    createGraph(adata);

  }

  private void initializeFrame() {
    setLayout(new BorderLayout(0, 0));
    fullGraphPanel = new JPanel();
    fullGraphPanel.setBackground(Color.white);
    add(fullGraphPanel, BorderLayout.CENTER);
    fullGraphPanel.setLayout(new BorderLayout(6, 6));

/*
**      Get the passed parameters
*/

/*
**      Create the Graph instance and modify the default behaviour
*/
    graph = new G2Dint();
//    G2Dint lgraph = (G2Dint) graph;
//    graph = new Graph2D();

    graph.drawzero = false;
    graph.drawgrid = false;
    graph.borderTop = 20;
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
      graph.setMarkers(marker);

    fullGraphPanel.add(BorderLayout.CENTER, graph);

  }

  private void createGraph(Vector data) {
/*
**      Retrieve the data Set.
*/

    if (data != null) {
      this.data = data;
      DataSet data1 = null;
      Axis xaxis = null, yaxis = null;
      int nsets = data.size();
      for (int i = 0; i < nsets; i++) {
        double[][] adata = (double[][]) data.get(i);
        int np = adata[0].length;
        double pdata[] = new double[2 * np];
        for (int k = 0, j = 0; k < np; k++, j += 2) {
          pdata[j] = adata[0][k];
          pdata[j + 1] = adata[1][k];
        }

      data1 = graph.loadDataSet(pdata, np);
        if (markerType < 0) {
          data1.linestyle = 1;
          data1.linecolor = getPastelColor(i);
        } else {
          data1.linestyle = 0;
          data1.marker = markerType;
          data1.markerscale = 2;
          data1.markercolor = getPastelColor(i);
        }

        if (i == 0) {
          xaxis = graph.createAxis(Axis.BOTTOM);
          xaxis.setTitleText(xLabel);
          xaxis.setTitleFont(new Font("TimesRoman", Font.BOLD, 14));
          xaxis.setLabelFont(new Font("Helvetica", Font.PLAIN, 12));
          xaxis.setTitleColor(new Color(0, 0, 255));

          yaxis = graph.createAxis(Axis.LEFT);
          yaxis.setTitleText(yLabel);
          yaxis.setTitleFont(new Font("TimesRoman", Font.BOLD, 14));
          yaxis.setLabelFont(new Font("Helvetica", Font.PLAIN, 12));
          yaxis.setTitleColor(new Color(0, 0, 255));
        }
        xaxis.attachDataSet(data1);
        yaxis.attachDataSet(data1);
      }

    } else
      fullGraphPanel.add("South", new Label("No data available!"));
//    setComponentToPrint(fullGraphPanel);

  }

  public void saveFile() {
    // nothing by default
    if (data == null)
      return;
    BufferedWriter output = getWriter(openFileDialog(getParentFrame(), "Save plot data as...", FileDialog.SAVE));
    try {
      int nsets = data.size();
      for (int i = 0; i < nsets; i++) {
        double[][] adata = (double[][]) data.get(i);
        int np = adata[0].length;
        output.write("_dataset_number " + Integer.toString(i));
        output.newLine();
        output.write("_pd_meas_number_of_points " + Integer.toString(np));
        output.newLine();
        output.newLine();
        output.write("loop_");
        output.newLine();
        output.write("_x_coordinate ");
        output.write("_y_ordinate");
        output.newLine();
        for (int k = 0; k < np; k++) {
          output.write(" " + ((float) adata[0][k]) + " " + ((float) adata[1][k]));
          output.newLine();
        }
        output.newLine();
      }
    } catch (IOException io) {
    }
    try {
      output.flush();
      output.close();
    } catch (IOException io) {
    }
  }

  public Frame getParentFrame() {
    Container frame = getParent();
    while (frame != null && !(frame instanceof Frame)) {
      frame = frame.getParent();
    }
    return (Frame) frame;
  }

  public static java.net.URL getResource(String name) {
    Object obj = new Object();
    if (obj != null) {
      java.net.URL url = obj.getClass().getResource(name);
      return url;
    }
    return null;
  }

  public static final BufferedWriter getWriter(String filename) {
    BufferedWriter out = null;
      try {
        out = new BufferedWriter(new FileWriter(new File(filename)));
      } catch (IOException ie) {
        Graph2D.out.println("Unable to open file: " + filename);
      }
//	  }
    return out;
  }

  public static String openFileDialog(Frame parent, String title, int loading) {
    String filename = null;
    FileDialog fd = new FileDialog(parent, title, loading);
    fd.setVisible(true);
    if (fd.getFile() != null) {
      filename = fd.getDirectory() + fd.getFile();
    }
    fd.dispose();
    return filename;
  }

  public static Color[] PastelColor = {Color.red, new Color(0, 102, 0), Color.blue,
                                       Color.magenta, Color.green, Color.orange,
                                       Color.cyan, Color.pink, Color.black,
                                       new Color(102, 51, 0), new Color(153, 0, 153), Color.yellow,
                                       new Color(102, 102, 0), new Color(0, 102, 102)
  };

  public static Color getPastelColor(int index) {
    int maxNumber = PastelColor.length;
    while (index >= maxNumber)
      index -= maxNumber;
    return PastelColor[index];
  }

}
