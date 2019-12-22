/*
 * @(#)PlotSimpleData.java created 06/01/2003 Berkeley
 *
 * Copyright (c) 2003 Luca Lutterotti All Rights Reserved.
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
import java.io.BufferedWriter;
import java.io.IOException;

import it.unitn.ing.jgraph.*;
import it.unitn.ing.rista.util.*;

/**
 * The PlotSimpleData will display the graph of a set of data.
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:53 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class PlotSimpleData extends GraphFrame {

//      URL markerURL;
  double[] function = null;
  double[] xCoord = null;
	boolean dotsPlot = false;
	int lineStyle = 0;
	Axis xaxis = null, yaxis = null;
	String xAxisTitle = "x";
	String yAxisTitle = "Function";


	public PlotSimpleData(Frame parent, double[] function, boolean dots) {

		super(parent);

		initializeFrame();

		dotsPlot = dots;

		createGraph(function);

	}

	public PlotSimpleData(Frame parent, double[] function) {

    super(parent);

    initializeFrame();

    createGraph(function);

  }

	public PlotSimpleData(Frame parent, double[] x, double[] function, boolean dots) {

		super(parent);

		initializeFrame();

		dotsPlot = dots;

		createGraph(x, function);

	}

	public PlotSimpleData(Frame parent, double[] x, double[] function) {

    super(parent);

    initializeFrame();

    createGraph(x, function);

  }

	public PlotSimpleData(Frame parent, double[] x, double[][] function) {

		super(parent);

		initializeFrame();

		createGraph(x, function);

	}

	public PlotSimpleData(Frame parent, float[] function) {

    super(parent);

    initializeFrame();
    double[] functiond = null;
    if (function != null) {
    functiond = new double[function.length];
    for (int i = 0; i < function.length; i++) {
      functiond[i] = function[i];
    }
    }
    createGraph(functiond);
  }

	public PlotSimpleData(Frame parent, double[] x, double[] function, int line, boolean plotdots, String xTitleValue, String yTitleValue, String title) {

		super(parent);

		initializeFrame();

		setXaxisTitle(xTitleValue);
		setYaxisTitle(yTitleValue);
		lineStyle = line;
		dotsPlot = plotdots;

		createGraph(x, function);

		setTitle(title);
	}

	public void setXaxisTitle(String value) {
		xAxisTitle = value;
  }

	public void setYaxisTitle(String value) {
		yAxisTitle = value;
	}

	private void initializeFrame() {
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

    createDefaultMenuBar(true);

    Container p1 = getContentPane();
    p1.setLayout(new BorderLayout(0, 0));
    fullGraphPanel = new CopyPrintPanelNoBkg();
    fullGraphPanel.setBackground(Color.white);
    p1.add(fullGraphPanel, BorderLayout.CENTER);
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

    fullGraphPanel.add("Center", graph);
    setTitle("Function plot");

  }

  private void createGraph(double[] function) {
/*
**      Retrieve the data Set.
*/

    if (function != null) {
      this.function = function;
      this.xCoord = null;
//          System.out.println("Loading data....");
      int np = function.length;
      double stepx = 1;
      double min = 1.0;
//          System.out.println(min + " " + max + " " + stepx);

      double data[] = new double[2 * np];
      for (int i = 0, j = 0; i < np; i++, j += 2) {
        double xcoord = i * stepx + min;
        data[j] = xcoord;
        data[j + 1] = function[i];
      }

//          System.out.println("Data loaded");

      DataSet data1 = graph.loadDataSet(data, np);
	    if (dotsPlot) {
		    data1.linestyle = lineStyle;
		    data1.marker = 3;
		    data1.markerscale = 1;
		    data1.markercolor = Color.blue;
	    }

      createAxes(data1);

    } else
      fullGraphPanel.add("South", new Label("No function available!"));
    setComponentToPrint(fullGraphPanel);

  }

  private void createGraph(double[] x, double[] function) {
/*
**      Retrieve the data Set.
*/

    if (function != null) {
      this.function = function;
      this.xCoord = x;
//          System.out.println("Loading data....");
      int np = function.length;

      double data[] = new double[2 * np];
      for (int i = 0, j = 0; i < np; i++, j += 2) {
        data[j] = x[i];
        data[j + 1] = function[i];
      }

//          System.out.println("Data loaded");

      DataSet data1 = graph.loadDataSet(data, np);
	    if (dotsPlot) {
		    data1.linestyle = lineStyle;
		    data1.marker = 3;
		    data1.markerscale = 1;
		    data1.markercolor = Color.blue;
	    }

	    createAxes(data1);

    } else
      fullGraphPanel.add("South", new Label("No function available!"));
    setComponentToPrint(fullGraphPanel);

  }

	private void createGraph(double[] x, double[][] function) {
/*
**      Retrieve the data Set.
*/

		if (function != null) {
			this.function = null;
			this.xCoord = x;
//          System.out.println("Loading data....");

			for (int k = 0; k < function.length; k++) {
				int np = function.length;
				double data[] = new double[2 * np];
				for (int i = 0, j = 0; i < np; i++, j += 2) {
					data[j] = x[i];
					data[j + 1] = function[k][i];
				}

//          System.out.println("Data loaded");

				DataSet data1 = graph.loadDataSet(data, np);
				data1.linecolor = SpectrumPlotPanel.getPastelColor(k);
				if (dotsPlot) {
					data1.linestyle = lineStyle;
					data1.marker = 3;
					data1.markerscale = 1;
					data1.markercolor = SpectrumPlotPanel.getPastelColor(k);
				}

				if (xaxis == null)
					createAxes(data1);
				else {
					xaxis.attachDataSet(data1);
					yaxis.attachDataSet(data1);
				}
			}

		} else
			fullGraphPanel.add("South", new Label("No function available!"));
		setComponentToPrint(fullGraphPanel);

	}

	public void saveFile() {
		// nothing by default
		if (function == null)
			return;
		BufferedWriter output = Misc.getWriter(Utility.browseFilenametoSave(this, "Save plot data as..."));
		try {
			output.newLine();
			output.write("_pd_meas_number_of_points " + Integer.toString(function.length));
			output.newLine();
			output.newLine();
			output.write("loop_");
			output.newLine();
			output.write("_x_coordinate");
			output.newLine();
			output.write("_y_ordinate");
			output.newLine();
			double stepx = 1;
			double min = 1.0;
			for (int i = 0; i < function.length; i++) {
				double xcoord = i * stepx + min;
				if (xCoord != null)
					xcoord = xCoord[i];
				output.write(" " + Fmt.format(xcoord) + " " + Fmt.format(function[i]));
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

  private void createAxes(DataSet data1) {
/*
**      Attach data sets to the Xaxis
*/
//          System.out.println("Attaching X-axis....");

    xaxis = graph.createAxis(Axis.BOTTOM);
    xaxis.attachDataSet(data1);
    xaxis.setTitleText(xAxisTitle);
    xaxis.setTitleFont(new Font("TimesRoman", Font.BOLD, 14));
    xaxis.setLabelFont(new Font("Helvetica", Font.PLAIN, 12));
    xaxis.setTitleColor(new Color(0, 0, 255));
/*
**      Attach the first data set to the Left Axis
*/
//          System.out.println("Attaching Y-axis....");

    yaxis = graph.createAxis(Axis.LEFT);
    yaxis.attachDataSet(data1);
    yaxis.setTitleText(yAxisTitle);
    yaxis.setTitleFont(new Font("TimesRoman", Font.BOLD, 14));
    yaxis.setLabelFont(new Font("Helvetica", Font.PLAIN, 12));
    yaxis.setTitleColor(new Color(0, 0, 255));
/*
**      Attach the second data set to the Right Axis
*/
//          System.out.println("Plotting....");

  }

}
