/*
 * @(#)PlotPFCoverage.java created 8/12/1998 Berkeley, CA
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

package it.unitn.ing.rista.awt;

import it.unitn.ing.jgraph.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.MaudPreferences;
import it.unitn.ing.rista.util.Misc;

import java.awt.*;

/**
 * The PlotPFCoverage is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.13 $, $Date: 2006/01/19 14:45:53 $
 * @since JDK1.1
 */

public class PlotPFCoverage extends GraphFrame {

  DataSet data1;
  Axis xaxis;
  Axis yaxis;
//  int np;
//      URL markerURL;


  public PlotPFCoverage(Frame parent, Sample asample, Phase aphase, int hklnumbersel) {

    super(parent);

    frameWLabel = "plotPFCover.frameWidth";
    frameHLabel = "plotPFCover.frameHeight";
    defaultFrameW = 400;
    defaultFrameH = 400;
    setOwnSize = true;
    framePositionX = "plotPFCover.framePositionX";
    framePositionY = "plotPFCover.framePositionY";
    defaultFramePositionX = 10;
    defaultFramePositionY = 20;
    setOwnPosition = true;

//        System.out.println("Initializing graph....");

    createDefaultMenuBar();

    CopyPrintPanel c1 = new CopyPrintPanelNoBkg();
    getContentPane().setBackground(Color.white);
    getContentPane().add(c1);
    c1.setLayout(new BorderLayout(6, 6));

    int i;
    int j;
/*
**      Get the passed parameters
*/

/*
**      Create the Graph instance and modify the default behaviour
*/
    graph = new PF2Dint();
    PF2Dint lgraph = (PF2Dint) graph;

    lgraph.drawzero = false;
    lgraph.drawgrid = false;
    lgraph.frame = false;
    lgraph.borderTop = 10;
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

    Reflection reflex = aphase.getReflex(hklnumbersel);

    if (reflex != null && asample != null) {
      c1.add("Center", lgraph);
      setTitle("Pole figure coverage: " + Integer.toString(reflex.getH()) + " " +
          Integer.toString(reflex.getK()) + " " +
          Integer.toString(reflex.getL()) + " ");
//          System.out.println("Loading data....");

      asample.prepareComputation();
      for (int j1 = 0; j1 < asample.layersnumber(); j1++)
        asample.getlayer(j1).setIndex(j1);

      boolean plotScaleFactors = MaudPreferences.getBoolean("plotCoverage.useScaleFactors", true);
      boolean plotScaleFactorsBW = MaudPreferences.getBoolean("plotCoverage.useScaleFactorsBW", true);
      boolean plotIncidentAndDiffraction = MaudPreferences.getBoolean("plotCoverage.plotIncidentAndDiffraction", false);
      double plotScaleFactorsBWZoom = MaudPreferences.getDouble("plotCoverage.useScaleFactorsBWZoom", 3.0);
      boolean forceNameDataset = MaudPreferences.getBoolean("plotCoverage.useAlwaysDatasetNames", false);
      boolean plotAlternateCoverage = false;
      if (Constants.testing) {
        plotAlternateCoverage = MaudPreferences.getBoolean("plotCoverage.plotAlternateCoverage", false);
      }

      int multi = 1;
	    int np = 0;
      if (plotIncidentAndDiffraction)
        multi = 3;
      int nd = asample.activeDatasetsNumber();
	    for (int n = 0; n < nd; n++) {
		    DataFileSet adataset = asample.getActiveDataSet(n);
		    np += adataset.getNumberOfTexturePoints(aphase, hklnumbersel);
	    }

	    if (np > 0) {

		    np = 0;
		    if (plotScaleFactorsBW) {
			    for (int n = 0; n < nd; n++) {
				    DataFileSet adataset = asample.getActiveDataSet(n);
				    np = adataset.getNumberOfTexturePoints(aphase, hklnumbersel);
				    double[] angles;
				    double min = 1.0E150;
				    double max = -1.0E150;
				    j = 0;
				    for (i = 0; i < adataset.activedatafilesnumber(); i++) {
					    DiffrDataFile adatafile = adataset.getActiveDataFile(i);
					    for (int ppp = 0; ppp < adatafile.positionsPerPattern; ppp++)
						    if (adatafile.isInsideRange(adatafile.getPositions(aphase)[hklnumbersel][ppp][0])) {
							    j++;
//todo            if (!Double.isNaN(reflex.getExpTextureFactor(i))) {
							    double data[] = new double[2 * multi];
							    double color_data[] = new double[multi];
							    if (plotScaleFactors) {
								    color_data[0] = adatafile.getMonitorCountsValue();
							    } else {
								    // todo modify for more peaks par pattern
								    color_data[0] = adatafile.getShapeAbsFactors(aphase, hklnumbersel)[0][0];
							    }
							    if (color_data[0] < min)
								    min = color_data[0];
							    if (color_data[0] > max)
								    max = color_data[0];
							    double position = adatafile.getPositions(aphase)[hklnumbersel][ppp][0];
							    if (plotAlternateCoverage)
								    angles = adatafile.getAlternateTextureAngles(position);
							    else
								    angles = adatafile.getTextureAngles(position);
							    double[] iangles = adatafile.getIncidentAndDiffractionAngles(position);

							    double projection = Constants.sqrt2 * Math.sin(angles[0] * Constants.DEGTOPI / 2.0);
//            System.out.println(angles[0] + " " + angles[1]);
							    if (angles[0] > 90.) {
								    projection = Constants.sqrt2 * Math.sin((180. - angles[0]) * Constants.DEGTOPI / 2.0);
							    }
							    data[0] = projection * Math.cos(angles[1] * Constants.DEGTOPI);
							    data[1] = projection * Math.sin(angles[1] * Constants.DEGTOPI);

							    if (plotIncidentAndDiffraction) {
								    j += 2;
								    projection = Constants.sqrt2 * Math.sin(iangles[0] / 2.0);
//            System.out.println(angles[0] + " " + angles[1]);
								    if (iangles[0] > 90.) {
									    projection = Constants.sqrt2 * Math.sin((Math.PI - iangles[0]) / 2.0);
								    }
								    data[0] = projection * Math.cos(iangles[1]);
								    data[1] = projection * Math.sin(iangles[1]);
								    j += 2;
								    projection = Constants.sqrt2 * Math.sin(iangles[2] / 2.0);
//            System.out.println(angles[0] + " " + angles[1]);
								    if (iangles[2] > 90.) {
									    projection = Constants.sqrt2 * Math.sin((Math.PI - iangles[2]) / 2.0);
								    }
								    data[0] = projection * Math.cos(iangles[3]);
								    data[1] = projection * Math.sin(iangles[3]);
							    }

//          System.out.println("Data loaded");

							    if (multi > 0) {
								    data1 = lgraph.loadDataSet(data, multi);
								    data1.linestyle = 0;
								    data1.marker = 3;
								    if (plotScaleFactorsBW)
									    data1.markerscale = plotScaleFactorsBWZoom * Math.pow(color_data[0], plotScaleFactorsBWZoom) / 2.0;
								    else
									    data1.markerscale = plotScaleFactorsBWZoom * 0.5;
              /*           ThermalColorMap thermalMap = null;
      if (plotScaleFactorsBW)
        thermalMap = new ThermalColorMap(1, 1, 32, false);
      else
        thermalMap = new ThermalColorMap(min, max, 32, false);*/

//          System.out.println("Max & min for plotting: " + min + ", " + max);

//            data1.setColorMap(thermalMap);
//            data1.setColorData(color_data);
								    data1.markercolor = Color.black;
								    if (adataset.toXRDcatString().length() > 4)
									    data1.legend(1, n * 20 + 30, "#" + (n + 1)/*adataset.toXRDcatString()*/);
								    else
									    data1.legend(1, n * 20 + 30, adataset.toXRDcatString());
								    data1.legendColor(Color.black);
								    /*
								     **      Attach data sets to the Xaxis
								     */
//          System.out.println("Attaching X-axis....");

								    if (xaxis == null) {
									    xaxis = lgraph.createXAxis();
									    xaxis.setTitleText("");
									    xaxis.setTitleFont(new Font("TimesRoman", Font.BOLD, 14));
									    xaxis.setLabelFont(new Font("Helvetica", Font.PLAIN, 12));
									    xaxis.setTitleColor(Color.white);
									    xaxis.setLabelColor(Color.white);
									    xaxis.drawLine = false;
                /* axiscolor = Color.white;
         xaxis.minor_tic_size = 0;
         xaxis.major_tic_size = 0;     */
								    }
								    if (yaxis == null) {
									    yaxis = lgraph.createYAxis();
									    yaxis.setTitleText("");
									    yaxis.setTitleFont(new Font("TimesRoman", Font.BOLD, 24));
									    yaxis.setLabelFont(new Font("Helvetica", Font.PLAIN, 12));
									    yaxis.setTitleColor(Color.white);
									    yaxis.setLabelColor(Color.white);
									    yaxis.drawLine = false;
                /* yaxis.axiscolor = Color.white;
           yaxis.minor_tic_size = 0;
           yaxis.major_tic_size = 0;   */
								    }

								    xaxis.attachDataSet(data1);
								    yaxis.attachDataSet(data1);
								    xaxis.minimum = -1.0;
								    xaxis.maximum = 1.0;
								    yaxis.minimum = -1.0;
								    yaxis.maximum = 1.0;
								    j += 2;
							    }
						    }
					    //todo }
				    }
			    }
		    } else {
			    for (int n = 0; n < nd; n++) {
				    int totalPlotting = 0;
				    DataFileSet adataset = asample.getActiveDataSet(n);
				    np = adataset.getNumberOfTexturePoints(aphase, hklnumbersel);
				    double[] angles = null;
				    double data[] = new double[2 * np * multi];
				    System.out.println(data.length);
				    double color_data[] = new double[adataset.activedatafilesnumber()];
				    double min = 1.0E150;
				    double max = -1.0E150;
				    j = 0;
				    for (i = 0; i < adataset.activedatafilesnumber(); i++) {
					    DiffrDataFile adatafile = adataset.getActiveDataFile(i);
					    for (int ppp = 0; ppp < adatafile.positionsPerPattern; ppp++)
						    if (adatafile.isInsideRange(adatafile.getPositions(aphase)[hklnumbersel][ppp][0])) {
//todo            if (!Double.isNaN(reflex.getExpTextureFactor(i))) {
							    adatafile = adataset.getActiveDataFile(i);

							    if (plotScaleFactors)
								    color_data[i] = adatafile.getMonitorCountsValue();
							    else
								    color_data[i] = adatafile.getShapeAbsFactors(aphase, hklnumbersel)[0][0];
							    if (color_data[i] < min)
								    min = color_data[i];
							    if (color_data[i] > max)
								    max = color_data[i];
							    double position = adatafile.getPositions(aphase)[hklnumbersel][ppp][0];
							    angles = adatafile.getTextureAngles(position);
							    double[] iangles = adatafile.getIncidentAndDiffractionAngles(position);

							    double projection = Constants.sqrt2 * Math.sin(angles[0] * Constants.DEGTOPI / 2.0);
//            System.out.println(angles[0] + " " + angles[1]);
							    if (angles[0] > 90.) {
								    projection = Constants.sqrt2 * Math.sin((180. - angles[0]) * Constants.DEGTOPI / 2.0);
							    }
							    data[j] = projection * Math.cos(angles[1] * Constants.DEGTOPI);
							    data[j + 1] = projection * Math.sin(angles[1] * Constants.DEGTOPI);

							    if (plotIncidentAndDiffraction) {
								    j += 2;
								    projection = Constants.sqrt2 * Math.sin(iangles[0] / 2.0);
//            System.out.println(angles[0] + " " + angles[1]);
								    if (iangles[0] > 90.) {
									    projection = Constants.sqrt2 * Math.sin((Math.PI - iangles[0]) / 2.0);
								    }
								    data[j] = projection * Math.cos(iangles[1]);
								    data[j + 1] = projection * Math.sin(iangles[1]);
								    j += 2;
								    projection = Constants.sqrt2 * Math.sin(iangles[2] / 2.0);
//            System.out.println(angles[0] + " " + angles[1]);
								    if (iangles[2] > 90.) {
									    projection = Constants.sqrt2 * Math.sin((Math.PI - iangles[2]) / 2.0);
								    }
								    data[j] = projection * Math.cos(iangles[3]);
								    data[j + 1] = projection * Math.sin(iangles[3]);
							    }
							    j += 2;
							    totalPlotting++;
						    }
//todo            }
				    }

				    if (totalPlotting > 0) {
					    data1 = lgraph.loadDataSet(data, totalPlotting * multi);
					    data1.linestyle = 0;
					    data1.marker = n + 1;
					    if (nd == 1) data1.marker = 10; //large points
					    data1.markerscale = plotScaleFactorsBWZoom * 0.5;
					    ThermalColorMap thermalMap = new ThermalColorMap(min, max, 32, false);

//          System.out.println("Max & min for plotting: " + min + ", " + max);


					    data1.setColorMap(thermalMap);
					    data1.setColorData(color_data);
					    data1.markercolor = new Color(0, 0, 0);
					    if (adataset.toXRDcatString().length() > 4 && !forceNameDataset)
						    data1.legend(1, n * 20 + 30, "#" + (n + 1)/*adataset.toXRDcatString()*/);
					    else
						    data1.legend(1, n * 20 + 30, adataset.toXRDcatString());
					    data1.legendColor(Color.black);
					    /*
					     **      Attach data sets to the Xaxis
					     */
//          System.out.println("Attaching X-axis....");

					    if (xaxis == null) {
						    xaxis = lgraph.createXAxis();
						    xaxis.setTitleText("");
						    xaxis.setTitleFont(new Font("TimesRoman", Font.BOLD, 14));
						    xaxis.setLabelFont(new Font("Helvetica", Font.PLAIN, 12));
						    xaxis.setTitleColor(Color.white);
						    xaxis.setLabelColor(Color.white);
						    xaxis.drawLine = false;
            /* axiscolor = Color.white;
            xaxis.minor_tic_size = 0;
            xaxis.major_tic_size = 0;     */
					    }
					    if (yaxis == null) {
						    yaxis = lgraph.createYAxis();
						    yaxis.setTitleText("");
						    yaxis.setTitleFont(new Font("TimesRoman", Font.BOLD, 24));
						    yaxis.setLabelFont(new Font("Helvetica", Font.PLAIN, 12));
						    yaxis.setTitleColor(Color.white);
						    yaxis.setLabelColor(Color.white);
						    yaxis.drawLine = false;
            /* yaxis.axiscolor = Color.white;
            yaxis.minor_tic_size = 0;
            yaxis.major_tic_size = 0;   */
					    }

					    xaxis.attachDataSet(data1);
					    yaxis.attachDataSet(data1);
					    xaxis.minimum = -1.0;
					    xaxis.maximum = 1.0;
					    yaxis.minimum = -1.0;
					    yaxis.maximum = 1.0;
				    }
			    }
		    }
	    } else {
		    c1.add("South", new Label("No experimental points in the range for this reflection!"));
	    }
    } else
      c1.add("South", new Label("No Pole Figure coverage possible!"));
    setComponentToPrint(c1);

  }

  public void dispose() {
    super.dispose();
    data1 = null;
  }

}
