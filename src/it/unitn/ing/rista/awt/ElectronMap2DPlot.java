/*
 * @(#)ElectronMap2DPlot.java created 26/11/2001 Verona-Milano
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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
import javax.swing.*;

import gov.noaa.pmel.sgt.swing.*;
import gov.noaa.pmel.sgt.*;
import gov.noaa.pmel.sgt.ColorMap;
import gov.noaa.pmel.sgt.IndexedColorMap;
import gov.noaa.pmel.sgt.LinearTransform;
import gov.noaa.pmel.sgt.swing.prop.*;
import gov.noaa.pmel.sgt.dm.*;
import gov.noaa.pmel.util.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.jgraph.ThermalColorMap;


/**
 * The ElectronMap2DPlot is a class to plot sections of the electron Map.
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:52 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class ElectronMap2DPlot extends myJFrame {

  Panel fullGraphPanel;
  String title = null;
  public JPlotLayout rpl_data;
//  public JPlotLayout rpl_fit;
//  private GridAttribute gridAttr_;
  JButton edit_;
  JButton space_ = null;
  JButton tree_;
  double[][] data = null;
  double IntensityMin = Float.MAX_VALUE;
  double IntensityMax = Float.MIN_VALUE;
  double xMax = 1.0f;
  double yMax = 1.0f;
  boolean computeMinMax = true;
  int ratio = 6;

  public ElectronMap2DPlot(Frame parent) {

    super(parent);

    framePositionX = "MapPlot2D.framePositionX";
    framePositionY = "MapPlot2D.framePositionY";
    defaultFramePositionX = 10;
    defaultFramePositionY = 20;
    setOwnPosition = true;
    frameWLabel = "MapPlot2D.frameWidth";
    frameHLabel = "MapPlot2D.frameHeight";
    defaultFrameW = 500;
    defaultFrameH = 550;
    setOwnSize = true;

  }

  public ElectronMap2DPlot(Frame parent, double[][] adata, String title,
                           double IntensityMin, double IntensityMax, double xMax, double yMax) {

    this(parent);

    this.xMax = xMax;
    this.yMax = yMax;

    if (IntensityMin != IntensityMax) {
      computeMinMax = false;
      this.IntensityMin = IntensityMin;
      this.IntensityMax = IntensityMax;
    } else {
      computeMinMax = true;
      this.IntensityMin = Float.MAX_VALUE;
      this.IntensityMax = Float.MIN_VALUE;
    }
    this.title = title;
    data = adata;

    getContentPane().setBackground(Color.white);
    createDefaultMenuBar();

    if (data != null) {

/*					FilePar filepar = (FilePar) datafile[0].getFilePar();

					int numberphases = filepar.phasesNumber();
					Phase[] phaselist = new Phase[numberphases];
					for (int i = 0; i < numberphases; i++)
						phaselist[i] = (Phase) filepar.getphase(i);*/

      setTitle(title);
      setSize(defaultFrameW, defaultFrameH);
      Container p1 = getContentPane();
      p1.setLayout(new BorderLayout());

      JPanel button = makeButtonPanel(true);
/*
     * Create JPlotLayout and turn batching on.  With batching on the
     * plot will not be updated as components are modified or added to
     * the plot tree.
     */
      rpl_data = makeGraph(data);
      rpl_data.setBatch(true);
      double w = data.length;
      double h = data[0].length;
      while (w > 10 || h > 10) {
        w /= 2;
        h /= 2;
      }
      while (w > 6 || h > 6) {
        w /= 1.2;
        h /= 1.2;
      }
      rpl_data.setLayerSizeP(new Dimension2D(w, h));

      fullGraphPanel = new Panel();
      fullGraphPanel.setBackground(Color.white);
      fullGraphPanel.setLayout(new BorderLayout());

/*
     * Create button panel with "mark" button
     */
/*
     * Layout the plot, key, and buttons.
     */
      JPane gridKeyPane = rpl_data.getKeyPane();
      gridKeyPane.setSize(new Dimension(defaultFrameW, defaultFrameH / ratio));
      rpl_data.setKeyLayerSizeP(new Dimension2D(6.0, 1.0));
      rpl_data.setKeyBoundsP(new Rectangle2D.Double(0.0, 1.0, 6.0, 1.0));

      fullGraphPanel.add(rpl_data, BorderLayout.CENTER);
      fullGraphPanel.add(gridKeyPane, BorderLayout.SOUTH);

      p1.add(fullGraphPanel, BorderLayout.CENTER);
      p1.add(button, BorderLayout.SOUTH);
//      pack();
/*
     * Turn batching off. JPlotLayout will redraw if it has been
     * modified since batching was turned on.
     */
      setVisible(true);
      setComponentToPrint(fullGraphPanel);
      rpl_data.setBatch(false);
      fullGraphPanel.addComponentListener(new java.awt.event.ComponentAdapter() {
        public void componentResized(java.awt.event.ComponentEvent event) {
          Component comp = event.getComponent();
          if (comp == fullGraphPanel) {
            rpl_data.getKeyPane().setSize(new Dimension(fullGraphPanel.getWidth(), fullGraphPanel.getHeight() / ratio));
            rpl_data.getKeyPane().draw();
          }
        }
      });
      
    } else {
      setSize(100, 100);
      setVisible(true);
    }
  }

  public ElectronMap2DPlot(Frame parent, double[][] adata, String title) {
    this(parent, adata, title, 0.0f, 0.0f, 1, 1);
  }

  JPlotLayout makeGraph(double[][] data) {
    /*
     * This example uses a pre-created "Layout" for raster time
     * series to simplify the construction of a plot. The
     * JPlotLayout can plot a single grid with
     * a ColorKey, time series with a LineKey, point collection with a
     * PointCollectionKey, and general X-Y plots with a
     * LineKey. JPlotLayout supports zooming, object selection, and
     * object editing.
     */

    JPlotLayout rpl;
    ContourLevels clevels;

    /* create the grid data */
    double[] xaxis, yaxis;
    double[] values;

    int ylength = data[0].length;
    int xlength = data.length;
    xaxis = new double[xlength];
    yaxis = new double[ylength];
    values = new double[xlength * ylength];
    int j = 0;
//    IntensityMin = (double) 1.0E60;
//    IntensityMax = (double) -1.0E60;
    int mode = 0;
    double xstep = xMax / xlength;
    double ystep = yMax / ylength;
    for (int i = 0; i < xlength; i++) {
      xaxis[i] = (0.5 + i) * xstep;
      for (int sn = 0; sn < ylength; sn++) {
        if (i == 0)
          yaxis[sn] = (0.5 + sn) * ystep;
        values[j++] = (double) data[i][sn];
      }
    }

    /* Contour plot lines, defining range */
    double deltaRange = (IntensityMax - IntensityMin) / 13;
    Range2D datar = new Range2D(IntensityMin, IntensityMax, deltaRange);

    //
    // create SimpleGrid
    //
    SGTMetaData zMeta = new SGTMetaData("Electron Density", "e/Angstrom^3");
    SGTMetaData xMeta = new SGTMetaData("x", "fractional coordinate * slice");

    String information;
    SGTMetaData yMeta = new SGTMetaData("y", "fractional coordinate * slice");
    SimpleGrid sg = new SimpleGrid(values, xaxis, yaxis, "2D Electron Map Sections");
    sg.setXMetaData(xMeta);
    sg.setYMetaData(yMeta);
    sg.setZMetaData(zMeta);

    /*
     * Create the layout without a Logo image and with the
     * ColorKey on a separate Pane object.
     */
    rpl = new JPlotLayout(true, false, false, true, true, "2D Electron Map Sections", null, true);
    rpl.setEditClasses(false);
    /*
     * Create a GridAttribute for CONTOUR style.
     */
    clevels = ContourLevels.getDefault(datar);
    GridAttribute gridAttr_ = new GridAttribute(clevels);
    /*
     * Create a ColorMap and change the style to RASTER.
     */
    ColorMap cmap = createColorMap(datar);
    gridAttr_.setColorMap(cmap);
    gridAttr_.setStyle(GridAttribute.RASTER);  // or AREA_FILL
    /*
     * Add the grid to the layout and give a label for
     * the ColorKey.
     */
    rpl.addData(sg, gridAttr_, "Sqrt(Electron density) [a.u.]");
    /*
     * Change the layout's three title lines.
     */

    rpl.setTitles("2D Electron Map Sections", "", "");

    return rpl;
  }

  ColorMap createColorMap(Range2D datar) {
    int colrsNumber = 64;
    int[] red = new int[colrsNumber], green = new int[colrsNumber], blue = new int[colrsNumber];
    if (MaudPreferences.getBoolean("multiplot2D.grayscale", false)) {
      double fract = 248.0 / (colrsNumber - 1);
      if (MaudPreferences.getBoolean("multiplot2D.colors_inverted", false)) {
        for (int i = 0; i < colrsNumber; i++) {
          int grayLevel = (int) (fract * i);
          red[i] = grayLevel;
          green[i] = grayLevel;
          blue[i] = grayLevel;
        }
      } else
        for (int i = 0; i < colrsNumber; i++) {
          int grayLevel = (int) (fract * (colrsNumber - i - 1));
          red[i] = grayLevel;
          green[i] = grayLevel;
          blue[i] = grayLevel;
        }
    } else if (colrsNumber == 64) {
      if (MaudPreferences.getBoolean("multiplot2D.colors_inverted", false)) {
        for (int i = 0; i < 64; i++) {
          red[63 - i] = ThermalColorMap.red64[i];
          green[63 - i] = ThermalColorMap.green64[i];
          blue[63 - i] = ThermalColorMap.blue64[i];
        }
      } else
        for (int i = 0; i < 64; i++) {
          red[i] = ThermalColorMap.red64[i];
          green[i] = ThermalColorMap.green64[i];
          blue[i] = ThermalColorMap.blue64[i];
        }
    } else if (colrsNumber == 16) {
      if (MaudPreferences.getBoolean("multiplot2D.colors_inverted", false)) {
        for (int i = 0; i < 16; i++) {
          red[15 - i] = ThermalColorMap.red16[i];
          green[15 - i] = ThermalColorMap.green16[i];
          blue[15 - i] = ThermalColorMap.blue16[i];
        }
      } else
        for (int i = 0; i < 16; i++) {
          red[i] = ThermalColorMap.red16[i];
          green[i] = ThermalColorMap.green16[i];
          blue[i] = ThermalColorMap.blue16[i];
        }
    }

    IndexedColorMap cmap = new IndexedColorMap(red, green, blue);
    cmap.setTransform(new LinearTransform(0.0, (double) red.length,
            datar.start, datar.end));
    return cmap;
  }

  JPanel makeButtonPanel(boolean mark) {
    JPanel button = new JPanel();
    button.setLayout(new FlowLayout());
    tree_ = new JButton("Tree View");
    java.awt.event.ActionListener myAction = new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent event) {
        Object obj = event.getSource();
        if (obj == edit_)
          edit_actionPerformed(event);
        if (obj == space_) {
          System.out.println("  <<Mark>>");
        }
        if (obj == tree_)
          tree_actionPerformed(event);
      }
    };
    tree_.addActionListener(myAction);
    button.add(tree_);
    edit_ = new JButton("Edit GridAttribute");
    edit_.addActionListener(myAction);
    button.add(edit_);
    /*
     * Optionally leave the "mark" button out of the button panel
     */
    if (mark) {
      space_ = new JButton("Add Mark");
      space_.addActionListener(myAction);
      button.add(space_);
    }
    return button;
  }

  void edit_actionPerformed(java.awt.event.ActionEvent e) {
    /*
     * Create a GridAttributeDialog and set the renderer.
     */
    GridAttributeDialog gad = new GridAttributeDialog();
    gad.setJPane(rpl_data);
    CartesianRenderer rend = ((CartesianGraph) rpl_data.getFirstLayer().getGraph()).getRenderer();
    gad.setGridCartesianRenderer((GridCartesianRenderer) rend);
    //        gad.setGridAttribute(gridAttr_);
    gad.setVisible(true);
  }

  void tree_actionPerformed(java.awt.event.ActionEvent e) {
    /*
     * Create a JClassTree for the JPlotLayout objects
     */
    JClassTree ct = new JClassTree();
    ct.setModal(false);
    ct.setJPane(rpl_data);
    ct.show();
  }

  public void showNewFrame() {
    setVisible(false);
    new ElectronMap2DPlot(getFrameParent(), data, title, IntensityMin, IntensityMax, xMax, yMax);
    dispose();
  }

  public JMenuBar createDefaultMenuBar() {
    JMenuBar amenubar = super.createDefaultMenuBar();
    amenubar.add(createPlottingOptionMenu());
    return amenubar;
  }

  public JMenu createPlottingOptionMenu() {

    JMenuItem menuitem = null;

    JMenu optionsMenu = new JMenu("Plotting");
    optionsMenu.setMnemonic('p');
    optionsMenu.add(menuitem = new JMenuItem("Options"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        showOptionsDialog();
      }
    });

    return optionsMenu;
  }

  public void showOptionsDialog() {
    JOptionsDialog adialog = new J2DMapPlotD(this, null);
    adialog.setVisible(true);
  }

  class J2DMapPlotD extends JOptionsDialog {

    JComboBox plotModeCB;
    JTextField legendMinTF;
    JTextField legendMaxTF;

    public J2DMapPlotD(Frame parent, XRDcat obj) {

      super(parent, obj);

      principalPanel.setLayout(new GridLayout(0, 2, 6, 6));

      principalPanel.add(new JLabel("Intensity legend, min: "));
      legendMinTF = new JTextField(Constants.FLOAT_FIELD);
      legendMinTF.setToolTipText("Set the intensity minimum for the legend");
      principalPanel.add(legendMinTF);

      principalPanel.add(new JLabel("Intensity legend, max: "));
      legendMaxTF = new JTextField(Constants.FLOAT_FIELD);
      legendMaxTF.setToolTipText("Set the intensity maximum for the legend");
      principalPanel.add(legendMaxTF);

      principalPanel.add(new JLabel("Intensity scale mode: "));
      plotModeCB = new JComboBox();
      int nmode = PlotDataFile.plotMode.length;
      for (int i = 0; i < nmode; i++)
        plotModeCB.addItem(PlotDataFile.plotMode[i]);
      plotModeCB.setToolTipText("Choose the scale mode for the intensity axis");
      principalPanel.add(plotModeCB);

      J2DMapPlotD.this.setTitle("Plotting options");
      initParameters();
      J2DMapPlotD.this.pack();
    }

    public void initParameters() {
      legendMinTF.setText(new String(Double.toString(IntensityMin)));
      legendMaxTF.setText(new String(Double.toString(IntensityMax)));
      plotModeCB.setSelectedItem(MaudPreferences.getPref(principalJFrame.plotScale, PlotDataFile.plotMode[0]));
    }

    public void retrieveParameters() {
      IntensityMin = Float.valueOf(legendMinTF.getText()).floatValue();
      IntensityMax = Float.valueOf(legendMaxTF.getText()).floatValue();
      MaudPreferences.setPref(principalJFrame.plotScale, plotModeCB.getSelectedItem().toString());

      showNewFrame();
    }
  }

}
