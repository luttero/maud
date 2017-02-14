/*
 * @(#)MultiPlotFitting2D.java created 26/06/2001 Casalino
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

import gov.noaa.pmel.sgt.*;
import gov.noaa.pmel.sgt.dm.SGTMetaData;
import gov.noaa.pmel.sgt.dm.SimpleGrid;
import gov.noaa.pmel.sgt.swing.*;
import gov.noaa.pmel.sgt.swing.prop.GridAttributeDialog;
import gov.noaa.pmel.util.Dimension2D;
import gov.noaa.pmel.util.Range2D;
import gov.noaa.pmel.util.Rectangle2D;
import it.unitn.ing.rista.diffr.DiffrDataFile;
import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.jgraph.ThermalColorMap;

import javax.swing.*;
import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;


/**
 * The MultiPlotFitting2D is a class to plot several spectra in the same box.
 *
 * @version $Revision: 1.8 $, $Date: 2006/01/19 14:45:52 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class MultiPlotFitting2D extends myJFrame {

  Panel fullGraphPanel;
  String title = null;
  public JPlotLayout rpl_data;
//  public JPlotLayout rpl_fit;
//  private GridAttribute gridAttr_;
  JButton edit_;
  JButton space_ = null;
  JButton tree_;
  DiffrDataFile[] datafile = null;
  double IntensityMin = Float.MAX_VALUE;
  double IntensityMax = Float.MIN_VALUE;
  boolean computeMinMax = true;
  int ratio = 6;

  public MultiPlotFitting2D(Frame parent) {

    super(parent);

    framePositionX = "plot2D.framePositionX";
    framePositionY = "plot2D.framePositionY";
    defaultFramePositionX = 10;
    defaultFramePositionY = 20;
    setOwnPosition = true;
    frameWLabel = "plot2D.frameWidth";
    frameHLabel = "plot2D.frameHeight";
    defaultFrameW = 500;
    defaultFrameH = 550;
    setOwnSize = true;

  }

  public MultiPlotFitting2D(Frame parent, DiffrDataFile[] adatafile, String title,
                            double IntensityMin, double IntensityMax) {

    this(parent);

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
    datafile = adatafile;

    Container p1 = getContentPane();
    p1.setLayout(new BorderLayout());
    p1.setBackground(Color.white);
    createDefaultMenuBar();

    if (datafile != null) {
      setTitle(title);
      setSize(defaultFrameW, defaultFrameH);
      JPanel button = makeButtonPanel(true);
      rpl_data = makeGraph(datafile);
      rpl_data.setBatch(true);

      fullGraphPanel = new Panel();
      fullGraphPanel.setBackground(Color.white);

//      GridBagLayout gridbag = new GridBagLayout();
//      GridBagConstraints c = new GridBagConstraints();
      fullGraphPanel.setLayout(new BorderLayout()); //gridbag);
/*      addWindowListener(new java.awt.event.WindowAdapter() {
        public void windowOpened(java.awt.event.WindowEvent event) {
          rpl_data.getKeyPane().draw();
        }
      });*/

      JPane gridKeyPane = rpl_data.getKeyPane();
      gridKeyPane.setSize(new Dimension(defaultFrameW, defaultFrameH / ratio));
      rpl_data.setKeyLayerSizeP(new Dimension2D(6.0, 1.0));
      rpl_data.setKeyBoundsP(new Rectangle2D.Double(0.0, 1.0, 6.0, 1.0));

      fullGraphPanel.add(rpl_data, BorderLayout.CENTER);

      fullGraphPanel.add(gridKeyPane, BorderLayout.SOUTH);

      p1.add(fullGraphPanel, BorderLayout.CENTER);
      p1.add(button, BorderLayout.SOUTH);
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

  public MultiPlotFitting2D(Frame parent, DiffrDataFile[] adatafile, String title) {
    this(parent, adatafile, title, 0.0f, 0.0f);
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

    JPlotLayout rpl;
    ContourLevels clevels;

    /* create the grid data */
    double[] xaxis, yaxis;
    double[] values;

    int hasFit = 1;
    if (datafile[0].hasfit())
      hasFit++;
    int ylength = datafile.length;
    int startingIndex = datafile[0].startingindex;
    int finalIndex = datafile[0].finalindex;
    datafile[0].initializeInterpolation();
    int startDatafile = 0;
    int finalDatafile = 0;
    int mode = PlotDataFile.checkScaleModeX();
    double xmin = 1.0E10, xmax = 0.0;
    if (xmin > datafile[0].getXDataForPlot(datafile[0].startingindex, mode))
      xmin = datafile[0].getXDataForPlot(datafile[0].startingindex, mode);
    if (xmax < datafile[0].getXDataForPlot(datafile[0].finalindex - 1, mode))
      xmax = datafile[0].getXDataForPlot(datafile[0].finalindex - 1, mode);
    if (xmin > datafile[0].getXDataForPlot(datafile[0].finalindex - 1, mode))
      xmin = datafile[0].getXDataForPlot(datafile[0].finalindex - 1, mode);
    if (xmax < datafile[0].getXDataForPlot(datafile[0].startingindex, mode))
      xmax = datafile[0].getXDataForPlot(datafile[0].startingindex, mode);
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
        if (xmin > datafile[i].getXDataForPlot(datafile[i].startingindex, mode))
          xmin = datafile[i].getXDataForPlot(datafile[i].startingindex, mode);
        if (xmax < datafile[i].getXDataForPlot(datafile[i].finalindex - 1, mode))
          xmax = datafile[i].getXDataForPlot(datafile[i].finalindex - 1, mode);
        if (xmin > datafile[i].getXDataForPlot(datafile[i].finalindex - 1, mode))
          xmin = datafile[i].getXDataForPlot(datafile[i].finalindex - 1, mode);
        if (xmax < datafile[i].getXDataForPlot(datafile[i].startingindex, mode))
          xmax = datafile[i].getXDataForPlot(datafile[i].startingindex, mode);
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
    double sep = 0.0;
    for (int i = 0; i < xlength; i++) {
/*
      if (startingIndex + i < datafile[0].startingindex)
        xaxis[i] = (double) datafile[startDatafile].getXDataForPlot(i + startingIndex, mode);
      else if (finalIndex + i >= datafile[0].finalindex)
        xaxis[i] = (double) datafile[finalDatafile].getXDataForPlot(i + startingIndex, mode);
      else
        xaxis[i] = (double) datafile[0].getXDataForPlot(i + startingIndex, mode);
*/
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
          double intValue = datafile[sn].getInterpolatedYSqrtIntensity(xaxis[i], 2, mode);
          values[j++] = intValue;
          if (hasFit == 1 && ylength == 1) {
            values[j++] = intValue;
            values[j++] = intValue;
          }
          if (values[j - 1] < IntensityMin && computeMinMax)
            IntensityMin = (double) values[j - 1];
          else if (values[j - 1] < IntensityMin && !computeMinMax)
            values[j - 1] = IntensityMin;
          if (values[j - 1] > IntensityMax && computeMinMax)
            IntensityMax = (double) values[j - 1];
          else if (values[j - 1] > IntensityMax && !computeMinMax)
            values[j - 1] = IntensityMax;
        }
/*

        if (startingIndex + i < datafile[sn].startingindex ||
            startingIndex + i >= datafile[sn].finalindex) {
          values[j++] = it.unitn.ing.jgraph.ColorMap.DUMMY_VALUE;
          if (hasFit == 1 && ylength == 1) {
            values[j++] = it.unitn.ing.jgraph.ColorMap.DUMMY_VALUE;
            values[j++] = it.unitn.ing.jgraph.ColorMap.DUMMY_VALUE;
          }
        } else {
          values[j++] = datafile[sn].getYSqrtData(i + startingIndex);
          if (hasFit == 1 && ylength == 1) {
            values[j++] = datafile[0].getYSqrtData(i + startingIndex);
            values[j++] = datafile[0].getYSqrtData(i + startingIndex);
          }
          if (values[j - 1] < IntensityMin && computeMinMax)
            IntensityMin = (double) values[j - 1];
          else if (values[j - 1] < IntensityMin && !computeMinMax)
            values[j - 1] = IntensityMin;
          if (values[j - 1] > IntensityMax && computeMinMax)
            IntensityMax = (double) values[j - 1];
          else if (values[j - 1] > IntensityMax && !computeMinMax)
            values[j - 1] = IntensityMax;
        }
*/
      }
      if (hasFit > 1) {

        values[j++] = it.unitn.ing.jgraph.ColorMap.DUMMY_VALUE;
        if (i == 0)
          yaxis[ylength] = ylength;

        for (int sn = 0; sn < ylength; sn++) {
          double xstartmin = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
          double xendmax = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
          if (xendmax < datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode))
            xendmax = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
          if (xstartmin > datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode))
            xstartmin = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
          if (i == 0)
            yaxis[ylength + 1 + sn] = ylength + 1 + sn;
          if (xaxis[i] < xstartmin || xaxis[i] > xendmax)
            values[j++] = it.unitn.ing.jgraph.ColorMap.DUMMY_VALUE;
          else {
            values[j++] = datafile[sn].getInterpolatedFitSqrtIntensity(xaxis[i], 2, mode);
            if (values[j - 1] < IntensityMin && computeMinMax)
              IntensityMin = (double) values[j - 1];
            else if (values[j - 1] < IntensityMin && !computeMinMax)
              values[j - 1] = IntensityMin;
            if (values[j - 1] > IntensityMax && computeMinMax)
              IntensityMax = (double) values[j - 1];
            else if (values[j - 1] > IntensityMax && !computeMinMax)
              values[j - 1] = IntensityMax;
          }
        }
/*
        for (int sn = 0; sn < ylength; sn++) {
          if (i == 0)
            yaxis[ylength + 1 + sn] = ylength + 1 + sn;
          if (startingIndex + i < datafile[sn].startingindex ||
              startingIndex + i >= datafile[sn].finalindex)
            values[j++] = it.unitn.ing.jgraph.ColorMap.DUMMY_VALUE;
          else {
            values[j++] = datafile[sn].getFitSqrtData(i + startingIndex);
            if (values[j - 1] < IntensityMin && computeMinMax)
              IntensityMin = (double) values[j - 1];
            else if (values[j - 1] < IntensityMin && !computeMinMax)
              values[j - 1] = IntensityMin;
            if (values[j - 1] > IntensityMax && computeMinMax)
              IntensityMax = (double) values[j - 1];
            else if (values[j - 1] > IntensityMax && !computeMinMax)
              values[j - 1] = IntensityMax;
          }
        }
*/
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

    String information;
    if (hasFit > 1)
      information = "data | fit";
    else
      information = "experimental";
    SGTMetaData yMeta = new SGTMetaData("Spectrum #", information);
    SimpleGrid sg = new SimpleGrid(values, xaxis, yaxis, "2D Multiplot");
    sg.setXMetaData(xMeta);
    sg.setYMetaData(yMeta);
    sg.setZMetaData(zMeta);

    /*
     * Create the layout without a Logo image and with the
     * ColorKey on a separate Pane object.
     */
    rpl = new JPlotSpectraLayout(true, false, false, "Intensity Map", null, true);
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

    if (datafile[0].hasfit())
      information = "measured data and fit";
    else
      information = "measured data only";
    rpl.setTitles("2D Multiplot for " + title,
        information,
        "");
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
    new MultiPlotFitting2D(getFrameParent(), datafile, title, IntensityMin, IntensityMax);
    dispose();
  }

  public JMenuBar createDefaultMenuBar() {
    JMenuBar amenubar = super.createDefaultMenuBar();
//    amenubar.add(createEditMenu());
    amenubar.add(createPlottingOptionMenu());
    return amenubar;
  }

  public JMenu createEditMenu() {
    JMenuItem menuitem = null;

    JMenu editMenu = new JMenu("Edit");

    editMenu.setMnemonic('e');
    editMenu.add(menuitem = new JMenuItem("Copy"));
    menuitem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    menuitem.setMnemonic('c');
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        Clipboard clipboard = getToolkit().getSystemClipboard();
        Component comp = fullGraphPanel;
        if (comp != null) {
          Rectangle rect = comp.getBounds();
          Image fileImage =
                  createImage(rect.width, rect.height);
          Graphics g = fileImage.getGraphics();

          //write to the image
          g.clearRect(0, 0, comp.getWidth(), comp.getHeight());
          comp.paint(g);
          clipboard.setContents(new ClipImage(fileImage), MultiPlotFitting2D.this);
          // write it out in the format you want

          //dispose of the graphics content
          g.dispose();
        }
      }
    });

    return editMenu;
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
    JOptionsDialog adialog = new JMulti2DPlottingOptionsD(this, null);
    adialog.setVisible(true);
  }

  class JMulti2DPlottingOptionsD extends JOptionsDialog {

    JComboBox plotModeCB, xplotModeCB;
    JTextField legendMinTF;
    JTextField legendMaxTF;

    public JMulti2DPlottingOptionsD(Frame parent, XRDcat obj) {

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

      principalPanel.add(new JLabel("X-axis plot mode: "));
      xplotModeCB = new JComboBox();
      nmode = PlotDataFile.xplotMode.length;
      for (int i = 0; i < nmode; i++)
        xplotModeCB.addItem(PlotDataFile.xplotMode[i]);
      xplotModeCB.setToolTipText("Choose the plot mode for the x-axis");
      principalPanel.add(xplotModeCB);

      JMulti2DPlottingOptionsD.this.setTitle("Plotting options");
      initParameters();
      JMulti2DPlottingOptionsD.this.pack();
    }

    public void initParameters() {
      legendMinTF.setText(new String(Double.toString(IntensityMin)));
      legendMaxTF.setText(new String(Double.toString(IntensityMax)));
      plotModeCB.setSelectedItem(MaudPreferences.getPref(principalJFrame.plotScale, PlotDataFile.plotMode[0]));
      xplotModeCB.setSelectedItem(MaudPreferences.getPref(PlotDataFile.xaxisModePref, PlotDataFile.xplotMode[0]));
    }

    public void retrieveParameters() {
      IntensityMin = Float.valueOf(legendMinTF.getText()).floatValue();
      IntensityMax = Float.valueOf(legendMaxTF.getText()).floatValue();
      MaudPreferences.setPref(principalJFrame.plotScale, plotModeCB.getSelectedItem().toString());
      MaudPreferences.setPref(PlotDataFile.xaxisModePref, xplotModeCB.getSelectedItem().toString());
      PlotDataFile.checkScaleMode();

      showNewFrame();
    }
  }

}
