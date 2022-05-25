/*
 * @(#)DifferencePlot2DPanel.java created Feb 6, 2005 Riva del Garda
 *
 * Copyright (c) 1996-2005 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.DiffrDataFile;
import gov.noaa.pmel.sgt.swing.JPlotLayout;
import gov.noaa.pmel.sgt.swing.JPlotSpectraLayout;
import gov.noaa.pmel.sgt.*;
import gov.noaa.pmel.sgt.dm.SGTMetaData;
import gov.noaa.pmel.sgt.dm.SimpleGrid;
import gov.noaa.pmel.util.Range2D;


/**
 * The DifferencePlot2DPanel is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/01/19 14:45:52 $
 * @since JDK1.1
 */

public class DifferencePlot2DPanel extends MultiPlotFitting2DPanel {


  public DifferencePlot2DPanel(myJFrame parent) {
    super(parent);
  }

  public void setNewData(DiffrDataFile[] afile, double[][] peaks,
                         double[] derivative2) {
    removeAll();
    initPanel(afile, "Difference 2D plot", 0.0f, 0.0f);
  }

  JPlotLayout makeGraph(DiffrDataFile[] datafile) {
    /*
     * This example uses a pre-created "Layout" for raster time
     * series to simplify the construction of a plot. The
     * JPlotLayout can plot a single grid with
     * a ColorKey, time series with a LineKey, point collection with a
     * PointCollectionKey, and general X-Y plots with a
     * LineKey. JPlotLayout supports zooming, object selection, and
     * object editing.
     */

    JPlotLayout rpl = null;
    ContourLevels clevels;

    /* create the grid data */
    double[] xaxis, yaxis;
    double[] values;

    int hasFit = 1;
    if (datafile[0].hasfit()) {
    int ylength = datafile.length;
    int startingIndex = datafile[0].startingindex;
    int finalIndex = datafile[0].finalindex;
    datafile[0].initializeInterpolation();
    int startDatafile = 0;
    int finalDatafile = 0;
    double xmin = 1.0E10, xmax = 0.0;
    if (xmin > datafile[0].getXDataForPlot(datafile[0].startingindex))
      xmin = datafile[0].getXDataForPlot(datafile[0].startingindex);
    if (xmax < datafile[0].getXDataForPlot(datafile[0].finalindex - 1))
      xmax = datafile[0].getXDataForPlot(datafile[0].finalindex - 1);
    if (xmin > datafile[0].getXDataForPlot(datafile[0].finalindex - 1))
      xmin = datafile[0].getXDataForPlot(datafile[0].finalindex - 1);
    if (xmax < datafile[0].getXDataForPlot(datafile[0].startingindex))
      xmax = datafile[0].getXDataForPlot(datafile[0].startingindex);
    for (int i = 1; i < ylength; i++) {
      datafile[i].initializeInterpolation();
      if (startingIndex > datafile[i].startingindex) {
        startingIndex = datafile[i].startingindex;
        startDatafile = i;
      }
      if (finalIndex < datafile[i].finalindex) {
        finalIndex = datafile[i].finalindex;
        finalDatafile = i;
      }
        if (xmin > datafile[i].getXDataForPlot(datafile[i].startingindex))
          xmin = datafile[i].getXDataForPlot(datafile[i].startingindex);
        if (xmax < datafile[i].getXDataForPlot(datafile[i].finalindex - 1))
          xmax = datafile[i].getXDataForPlot(datafile[i].finalindex - 1);
        if (xmin > datafile[i].getXDataForPlot(datafile[i].finalindex - 1))
          xmin = datafile[i].getXDataForPlot(datafile[i].finalindex - 1);
        if (xmax < datafile[i].getXDataForPlot(datafile[i].startingindex))
          xmax = datafile[i].getXDataForPlot(datafile[i].startingindex);
    }
    int xlength = finalIndex - startingIndex;
    double stepX = (xmax - xmin) / (xlength - 1);
    xaxis = new double[xlength];
    int ylen = ylength * hasFit + (hasFit - 1);
    if (hasFit == 1 && ylength == 1)
      ylen += 2;
    yaxis = new double[ylen];
    values = new double[xlength * ylen];
    int j = 0;
//    IntensityMin = (double) 1.0E60;
//    IntensityMax = (double) -1.0E60;
      int mode = PlotDataFile.checkScaleModeX();
      double sep = 0.0;
      for (int i = 0; i < xlength; i++) {
/*        if (startingIndex + i < datafile[0].startingindex)
          xaxis[i] = (double) datafile[startDatafile].getXDataForPlot(i + startingIndex, mode);
        else if (finalIndex + i >= datafile[0].finalindex)
          xaxis[i] = (double) datafile[finalDatafile].getXDataForPlot(i + startingIndex, mode);
        else
          xaxis[i] = (double) datafile[0].getXDataForPlot(i + startingIndex, mode);*/
      	xaxis[i] = xmin + i * stepX;
        for (int sn = 0; sn < ylength; sn++) {
          if (i == 0) {
            yaxis[sn] = sn;
            if (hasFit == 1 && ylength == 1) {
              yaxis[1] = 1;
              yaxis[2] = 2;
            }
          }
//        System.out.println(i + " " + sn + " " + j);

        double xstartmin = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
        double xendmax = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
        if (xendmax < datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode))
          xendmax = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
        if (xstartmin > datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode))
          xstartmin = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
        if (xaxis[i] < xstartmin || xaxis[i] > xendmax) {
            values[j++] = it.unitn.ing.jgraph.ColorMap.DUMMY_VALUE;
            if (hasFit == 1 && ylength == 1) {
              values[j++] = it.unitn.ing.jgraph.ColorMap.DUMMY_VALUE;
              values[j++] = it.unitn.ing.jgraph.ColorMap.DUMMY_VALUE;
            }
          } else {
          double intValue = datafile[sn].getInterpolatedYSqrtIntensity(xaxis[i], 2, mode) -
                datafile[sn].getInterpolatedFitSqrtIntensity(xaxis[i], 2, mode);
          values[j++] = intValue;
          if (hasFit == 1 && ylength == 1) {
            values[j++] = intValue;
            values[j++] = intValue;
          }
	        if (values[j - 1] != Double.NaN) {
		        if (values[j - 1] < IntensityMin && computeMinMax)
			        IntensityMin = values[j - 1];
		        else if (values[j - 1] < IntensityMin && !computeMinMax)
			        values[j - 1] = IntensityMin;
		        if (values[j - 1] > IntensityMax && computeMinMax)
			        IntensityMax = values[j - 1];
		        else if (values[j - 1] > IntensityMax && !computeMinMax)
			        values[j - 1] = IntensityMax;
	        }
          }

        }
      }

      /* Contour plot lines, defining range */
      double deltaRange = (IntensityMax - IntensityMin) / 13;
      Range2D datar = new Range2D(IntensityMin, IntensityMax, deltaRange);

      //
      // create SimpleGrid
      //
      SGTMetaData zMeta = new SGTMetaData(datafile[0].getAxisYLegend(), "");
      SGTMetaData xMeta = new SGTMetaData(datafile[0].getAxisXLegendNoUnit(), datafile[0].getAxisXLegendUnit());

      String information = "residuals";
      SGTMetaData yMeta = new SGTMetaData("Spectrum #", information);
      SimpleGrid sg = new SimpleGrid(values, xaxis, yaxis, "2D Multiplot");
      sg.setXMetaData(xMeta);
      sg.setYMetaData(yMeta);
      sg.setZMetaData(zMeta);

      /*
       * Create the layout without a Logo image and with the
       * ColorKey on a separate Pane object.
       */
      rpl = new JPlotSpectraLayout(true, false, false, true, true, "Intensity Map", null, true);
      rpl.setEditClasses(false);
      /*
       * Create a GridAttribute for CONTOUR style.
       */
      clevels = ContourLevels.getDefault(datar);
      GridAttribute gridAttr_ = new GridAttribute(clevels);
      /*
       * Create a ColorMap and change the style to RASTER.
       */
      gov.noaa.pmel.sgt.ColorMap cmap = createColorMap(datar);
      gridAttr_.setColorMap(cmap);
      gridAttr_.setStyle(GridAttribute.RASTER);  // or AREA_FILL
      /*
       * Add the grid to the layout and give a label for
       * the ColorKey.
       */
      rpl.addData(sg, gridAttr_, datafile[0].getAxisYLegend());
      /*
       * Change the layout's three title lines.
       */

      if (cmap instanceof TransformAccess) {
        ((TransformAccess) cmap).setRange(new Range2D(IntensityMin, IntensityMax));
      }

      rpl.setTitles("2D Multiplot for " + title,
          "Residuals",
          "");
    }
    /*
     * Resize the graph  and place in the "Center" of the frame.
     */
//    rpl.setSize(new Dimension(600, 500));
    /*
     * Resize the key Pane, both the device size and the physical
     * size. Set the size of the key in physical units and place
     * the key pane at the "South" of the frame.
     */
//    rpl.setKeyLayerSizeP(new Dimension2D(6.0, 1.02));
//    rpl.setKeyBoundsP(new Rectangle2D.Double(0.01, 1.01, 5.98, 1.0));

    return rpl;
  }

}
