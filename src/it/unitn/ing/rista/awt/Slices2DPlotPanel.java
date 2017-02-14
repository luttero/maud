/*
 * @(#)Slices2DPlotPanel.java created Aug 3, 2005 Mesiano
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

package it.unitn.ing.rista.awt;

import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.jgraph.ThermalColorMap;
import gov.noaa.pmel.sgt.swing.*;
import gov.noaa.pmel.sgt.swing.prop.GridAttributeDialog;
import gov.noaa.pmel.sgt.*;
import gov.noaa.pmel.sgt.dm.SGTMetaData;
import gov.noaa.pmel.sgt.dm.SimpleGrid;
import gov.noaa.pmel.util.*;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.border.TitledBorder;
import javax.swing.border.BevelBorder;

import org.javadev.AnimatingCardLayout;
import org.javadev.effects.SlideAnimation;

import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.event.*;
import java.util.Vector;


/**
 * The Slices2DPlotPanel is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/01/19 14:45:53 $
 * @since JDK1.1
 */

public class Slices2DPlotPanel extends JPanel {

  JPanel fullGraphPanel;
  String title = null;
  public JPlotLayout rpl_data;
//  public JPlotLayout rpl_fit;
//  private GridAttribute gridAttr_;
  JButton edit_;
  JButton space_ = null;
  JButton tree_;
//  double[][][] data = null;
  double IntensityMin = Float.MAX_VALUE;
  double IntensityMax = Float.MIN_VALUE;
  boolean computeMinMax = true;
  int ratio = 6;
  myJFrame theParent = null;

  AnimatingCardLayout panelLayout = null;

  JComboBox plotModeCB, xplotModeCB;
  JList slicesCB;
  JTextField legendMinTF;
  JTextField legendMaxTF;
  double[][] xaxis, yaxis;
  double[][] values;
  double zstep = 0.0f;
  GridAttribute gridAttr_ = null;

  public Slices2DPlotPanel(myJFrame parent) {
    theParent = parent;
  }

  public Slices2DPlotPanel(myJFrame parent, double[] data, String title,
                           double IntensityMin, double IntensityMax, int aSlices, int bSlices, int cSlices) {
    theParent = parent;
    initPanel(data, title, IntensityMin, IntensityMax, aSlices, bSlices, cSlices);
  }

  public Slices2DPlotPanel(myJFrame parent, double[] data, String title, int aSlices, int bSlices, int cSlices) {
    this(parent, data, title, 0.0f, 0.0f, aSlices, bSlices, cSlices);
  }

  public void initPanel(double[] adata, String title,
                        double IntensityMin, double IntensityMax,
                        int aSlices, int bSlices, int cSlices) {
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
//    data = adata;

    panelLayout = new AnimatingCardLayout();
    panelLayout.setAnimation(new SlideAnimation());
    setLayout(panelLayout);
    setBackground(Color.white);
    JPanel p2 = new JPanel(new BorderLayout());
    add(p2, "plot");

    if (adata != null) {
//      boolean isVisible = isVisible();
//      System.out.println("first " + isVisible);
//      if (isVisible)
//        setVisible(false);
      rpl_data = makeGraph(adata, aSlices, bSlices, cSlices);
      int w = aSlices / 5;
      int h = bSlices / 5;
      rpl_data.setLayerSizeP(new Dimension2D(w, h));
      if (rpl_data != null) {
        rpl_data.setBatch(true);

        fullGraphPanel = new JPanel();
        fullGraphPanel.setBackground(Color.white);

        fullGraphPanel.setLayout(new BorderLayout());

        JPane gridKeyPane = rpl_data.getKeyPane();

        double wk = (w+h)/2.0;
        double wh = wk/6.0;
        rpl_data.setKeyLayerSizeP(new Dimension2D(wk, wh));
        rpl_data.setKeyBoundsP(new Rectangle2D.Double(0.0, wh, wk, wh));

        fullGraphPanel.add(rpl_data, BorderLayout.CENTER);

        fullGraphPanel.add(gridKeyPane, BorderLayout.SOUTH);

        p2.add(fullGraphPanel, BorderLayout.CENTER);
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        p2.add(buttonPanel, BorderLayout.SOUTH);

        slicesCB = new JList();
        Vector dataList = new Vector();
        for (int i = 0; i < cSlices; i++)
          dataList.add("Slice # " + Integer.toString(i));
        slicesCB.setListData(dataList);
        slicesCB.setVisibleRowCount(3);
        JScrollPane scrollPanel = new JScrollPane(slicesCB);
        buttonPanel.add(scrollPanel);
        slicesCB.setSelectedIndex(0);
        slicesCB.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
          public void valueChanged(ListSelectionEvent e) {
            changeSlice(slicesCB.getSelectedIndex());
          }
        });

        JButton optionsB = new JButton("Options");
        buttonPanel.add(optionsB);
        optionsB.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
//          System.out.println("options");
            panelLayout.show(Slices2DPlotPanel.this, "options");
          }
        });

        add(createOptionsPanel(), "options");

//      setComponentToPrint(fullGraphPanel);
        rpl_data.setBatch(false);
//      if (isVisible)
//        setVisible(true);

        fullGraphPanel.addComponentListener(new java.awt.event.ComponentAdapter() {
          public void componentResized(java.awt.event.ComponentEvent event) {
            Component comp = event.getComponent();
            if (comp == fullGraphPanel) {
              rpl_data.getKeyPane().setSize(new Dimension(fullGraphPanel.getWidth(),
                  fullGraphPanel.getHeight() / ratio));
              rpl_data.getKeyPane().draw();
            }
          }
        });
      }
    }
  }

  public void setVisible(boolean visible) {
    super.setVisible(visible);
    if (rpl_data != null) {
      if (visible) {
        rpl_data.draw();
        getFrameParent().repaint();
      }
    }
  }

  public void repaintPlot() {
    if (rpl_data != null) {
    rpl_data.getKeyPane().setSize(new Dimension(fullGraphPanel.getWidth(), fullGraphPanel.getHeight()
        / ratio));
    rpl_data.getKeyPane().draw();
    getFrameParent().repaint();
    }
  }

  JPlotLayout makeGraph(double[] adata, int aSlices, int bSlices, int cSlices) {
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
    int xlength = aSlices;
    int ylength = bSlices;
    int zlength = cSlices;
    xaxis = new double[zlength][xlength];
    yaxis = new double[zlength][ylength];
    values = new double[zlength][xlength * ylength];

    if (computeMinMax) {
      for (int i = 0; i < adata.length; i++) {
        if (adata[i] < IntensityMin)
          IntensityMin = adata[i];
        if (adata[i] > IntensityMax)
          IntensityMax = adata[i];
      }
    }

//    IntensityMin = (double) 1.0E60;
//    IntensityMax = (double) -1.0E60;
//    int mode = 0;
    double xstep = 1.0 / xlength;
    double ystep = 1.0 / ylength;
    zstep = 1.0f / zlength;
    for (int kn = 0; kn < zlength; kn++) {
      int j = 0;
      for (int i = 0; i < xlength; i++) {
        xaxis[kn][i] = i * xstep;
        for (int sn = 0; sn < ylength; sn++) {
          if (i == 0)
            yaxis[kn][sn] = sn * ystep;
          values[kn][j++] = (double) adata[i * bSlices * cSlices + sn * cSlices + kn];
        }
      }
    }

    /* Contour plot lines, defining range */
    double deltaRange = (IntensityMax - IntensityMin) / 65;
    Range2D datar = new Range2D(IntensityMin, IntensityMax, deltaRange);

    //
    // create SimpleGrid
    //
    SimpleGrid sg = createSimpleGrid(0);
    /*
     * Create the layout without a Logo image and with the
     * ColorKey on a separate Pane object.
     */
    rpl = new JPlotSpectraLayout(true, false, false, "Electron density Map", null, true);
    rpl.autoIntensityResize = false;
    rpl.setEditClasses(false);
    /*
     * Create a GridAttribute for CONTOUR style.
     */
    clevels = ContourLevels.getDefault(datar);
    gridAttr_ = new GridAttribute(clevels);
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
    rpl.addData(sg, gridAttr_, "Electron density");
    /*
     * Change the layout's three title lines.
     */

    if (cmap instanceof TransformAccess) {
      ((TransformAccess) cmap).setRange(new Range2D(IntensityMin, IntensityMax));
    }

    rpl.setTitles(title, "Electron density", "");

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

  private SimpleGrid createSimpleGrid(int slice) {
    SGTMetaData zMeta = new SGTMetaData("Electron density", "");
    SGTMetaData xMeta = new SGTMetaData("x", "fractional coordinate");
    SGTMetaData yMeta = new SGTMetaData("y", "fractional coordinate");
    SimpleGrid sg = new SimpleGrid(values[slice], xaxis[slice], yaxis[slice], "Map at " +
        Float.toString((float) (slice * zstep)));
    sg.setXMetaData(xMeta);
    sg.setYMetaData(yMeta);
    sg.setZMetaData(zMeta);
    return sg;
  }

  public void changeSlice(int slice) {
    SimpleGrid sg = createSimpleGrid(slice);
    rpl_data.clear();
    rpl_data.addData(sg, gridAttr_, "Electron density");
  }

  gov.noaa.pmel.sgt.ColorMap createColorMap(Range2D datar) {
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

    gov.noaa.pmel.sgt.IndexedColorMap cmap = new gov.noaa.pmel.sgt.IndexedColorMap(red, green, blue);
    cmap.setTransform(new gov.noaa.pmel.sgt.LinearTransform(0.0, (double) red.length,
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

  public JPanel createOptionsPanel() {
    JPanel container = new JPanel(new FlowLayout(FlowLayout.CENTER));
    JPanel layoutPanel = new JPanel(new BorderLayout(6, 6));
    container.add(layoutPanel);
    JPanel optionsPanel = new JPanel();
    layoutPanel.add(optionsPanel, BorderLayout.CENTER);
    optionsPanel.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Plotting options"));
    optionsPanel.setLayout(new GridLayout(0, 2, 6, 6));

    optionsPanel.add(new JLabel("Intensity legend, min: "));
    legendMinTF = new JTextField(Constants.FLOAT_FIELD);
    legendMinTF.setToolTipText("Set the intensity minimum for the legend");
    optionsPanel.add(legendMinTF);

    optionsPanel.add(new JLabel("Intensity legend, max: "));
    legendMaxTF = new JTextField(Constants.FLOAT_FIELD);
    legendMaxTF.setToolTipText("Set the intensity maximum for the legend");
    optionsPanel.add(legendMaxTF);

    optionsPanel.add(new JLabel("Intensity scale mode: "));
    plotModeCB = new JComboBox();
    int nmode = PlotDataFile.plotMode.length;
    for (int i = 0; i < nmode; i++)
      plotModeCB.addItem(PlotDataFile.plotMode[i]);
    plotModeCB.setToolTipText("Choose the scale mode for the intensity axis");
    optionsPanel.add(plotModeCB);

    optionsPanel.add(new JLabel("X-axis plot mode: "));
    xplotModeCB = new JComboBox();
    nmode = PlotDataFile.xplotMode.length;
    for (int i = 0; i < nmode; i++)
      xplotModeCB.addItem(PlotDataFile.xplotMode[i]);
    xplotModeCB.setToolTipText("Choose the plot mode for the x-axis");
    optionsPanel.add(xplotModeCB);

    initParameters();

    layoutPanel.add(makeButtonPanel(true), BorderLayout.EAST);

    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
    layoutPanel.add(buttonPanel, BorderLayout.SOUTH);
    JButton doneB = new JButton("Done");
    buttonPanel.add(doneB);
    doneB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        panelLayout.show(Slices2DPlotPanel.this, "plot");
      }
    });

    return container;
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

//      showNewFrame();
  }

  public JMenu createEditMenu() {
    JMenuItem menuitem = null;

    JMenu editMenu = new JMenu("Edit");

    editMenu.setMnemonic('e');
    editMenu.add(menuitem = new JMenuItem("Copy"));
    menuitem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,
        Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
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
          clipboard.setContents(new ClipImage(fileImage), getFrameParent());
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
    JOptionsDialog adialog = new Slices2DPlotPanel.JSlices2DPlottingPanelOptionsD(getFrameParent(), null);
    adialog.setVisible(true);
  }

  private myJFrame getFrameParent() {
    return theParent;  //To change body of created methods use File | Settings | File Templates.
  }

  class JSlices2DPlottingPanelOptionsD extends JOptionsDialog {

    JComboBox plotModeCB, xplotModeCB;
    JTextField legendMinTF;
    JTextField legendMaxTF;

    public JSlices2DPlottingPanelOptionsD(Frame parent, XRDcat obj) {

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

      Slices2DPlotPanel.JSlices2DPlottingPanelOptionsD.this.setTitle("Plotting options");
      initParameters();
      Slices2DPlotPanel.JSlices2DPlottingPanelOptionsD.this.pack();
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

//      showNewFrame();
    }
  }
}
