/*
 * @(#)ThreeDMapPlot.java created Aug 3, 2005 Mesiano
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

import gov.noaa.pmel.sgt.swing.JPlotLayout;
import gov.noaa.pmel.sgt.swing.JClassTree;
import gov.noaa.pmel.sgt.swing.prop.GridAttributeDialog;
import gov.noaa.pmel.sgt.*;
import gov.noaa.pmel.sgt.dm.*;
import gov.noaa.pmel.util.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.XRDcat;


/**
 * The ThreeDMapPlot is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/01/19 14:45:53 $
 * @since JDK1.1
 */

public class ThreeDMapPlot extends myJFrame {

  String title = null;
  public JPlotLayout rpl_data;
//  public JPlotLayout rpl_fit;
//  private GridAttribute gridAttr_;
  JButton edit_;
  JButton space_ = null;
  JButton tree_;
  double[][][] data = null;
  double IntensityMin = Float.MAX_VALUE;
  double IntensityMax = Float.MIN_VALUE;
  boolean computeMinMax = true;

  public ThreeDMapPlot(Frame parent) {

    super(parent);

    framePositionX = "MapPlot3D.framePositionX";
    framePositionY = "MapPlot3D.framePositionY";
    defaultFramePositionX = 10;
    defaultFramePositionY = 20;
    setOwnPosition = true;

  }

  public ThreeDMapPlot(Frame parent, double[][][] adata, String title,
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
      setSize(600, 480);
      Container p1 = getContentPane();
      p1.setLayout(new BorderLayout());
      JPanel c1 = new JPanel();
      c1.setBackground(Color.white);
      p1.add(c1, BorderLayout.NORTH);
      c1.setLayout(new BorderLayout());

/*
     * Create button panel with "mark" button
     */
      JPanel button = makeButtonPanel(true);
/*
     * Create JPlotLayout and turn batching on.  With batching on the
     * plot will not be updated as components are modified or added to
     * the plot tree.
     */
      rpl_data = makeGraph(data);
      rpl_data.setBatch(true);
/*
     * Layout the plot, key, and buttons.
     */
      c1.add(rpl_data, BorderLayout.CENTER);
      JPane gridKeyPane = rpl_data.getKeyPane();
      gridKeyPane.setSize(new Dimension(600, 100));
      rpl_data.setKeyLayerSizeP(new Dimension2D(6.0, 1.0));
      rpl_data.setKeyBoundsP(new Rectangle2D.Double(0.0, 1.0, 6.0, 1.0));
      c1.add(gridKeyPane, BorderLayout.SOUTH);
      p1.add(button, BorderLayout.SOUTH);
      pack();
/*
     * Turn batching off. JPlotLayout will redraw if it has been
     * modified since batching was turned on.
     */
      if (Constants.macosx) {
        setVisible(true);
        rpl_data.setBatch(false);
        rpl_data.repaint();
      } else {
        rpl_data.setBatch(false);
        setVisible(true);
      }

    } else {
      setSize(100, 100);
      setVisible(true);
    }
  }

  public ThreeDMapPlot(Frame parent, double[][][] adata, String title) {
    this(parent, adata, title, 0.0f, 0.0f);
  }

  JPlotLayout makeGraph(double[][][] data) {
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
    double[] xaxis, yaxis, zaxis;
    double[] values;

    int zlength = data[0][0].length;
    int ylength = data[0].length;
    int xlength = data.length;
    xaxis = new double[xlength];
    yaxis = new double[ylength];
    zaxis = new double[ylength];
    values = new double[xlength * ylength * zlength];
    int j = 0;
//    IntensityMin = (double) 1.0E60;
//    IntensityMax = (double) -1.0E60;
//    int mode = 0;
    double xstep = 1.0 / xlength;
    double ystep = 1.0 / ylength;
    double zstep = 1.0 / zlength;
    for (int i = 0; i < xlength; i++) {
      xaxis[i] = (0.5 + i) * xstep;
      for (int sn = 0; sn < ylength; sn++) {
        if (i == 0)
          yaxis[sn] = (0.5 + sn) * ystep;
        for (int kn = 0; kn < zlength; kn++) {
          if (i == 0 && sn == 0)
            zaxis[kn] = (0.5 + kn) * zstep;
          values[j++] = (double) data[i][sn][kn];
          if (computeMinMax) {
            if (data[i][sn][kn] < IntensityMin)
              IntensityMin = data[i][sn][kn];
            if (data[i][sn][kn] > IntensityMax)
              IntensityMax = data[i][sn][kn];
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
    SGTMetaData valMeta = new SGTMetaData("Electron Density", "e/Angstrom^3");
    SGTMetaData xMeta = new SGTMetaData("x", "fractional coordinates");

    String information;
    SGTMetaData yMeta = new SGTMetaData("y", "fractional coordinates");
    SGTMetaData zMeta = new SGTMetaData("z", "fractional coordinates");
    ThreeDGrid sg = new ThreeDGrid(values, xaxis, yaxis, zaxis, "2D Electron Map Sections");
    sg.setXMetaData(xMeta);
    sg.setYMetaData(yMeta);
    sg.setZMetaData(zMeta);
    sg.setValMetaData(valMeta);

    /*
     * Create the layout without a Logo image and with the
     * ColorKey on a separate Pane object.
     */
    rpl = new JPlotLayout(true, false, false, "3D Electron Map", null, true);
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
    rpl.addData(sg, gridAttr_, "x");
    /*
     * Change the layout's three title lines.
     */

    rpl.setTitles("3D Electron Map", "", "");
    /*
     * Resize the graph  and place in the "Center" of the frame.
     */
    rpl.setSize(new Dimension(600, 400));
    /*
     * Resize the key Pane, both the device size and the physical
     * size. Set the size of the key in physical units and place
     * the key pane at the "South" of the frame.
     */
    rpl.setKeyLayerSizeP(new Dimension2D(6.0, 1.02));
    rpl.setKeyBoundsP(new Rectangle2D.Double(0.01, 1.01, 5.98, 1.0));

    return rpl;
  }

  gov.noaa.pmel.sgt.ColorMap createColorMap(Range2D datar) {
    int[] red =
            {0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0,
             0, 7, 23, 39, 55, 71, 87, 103,
             119, 135, 151, 167, 183, 199, 215, 231,
             247, 255, 255, 255, 255, 255, 255, 255,
             255, 255, 255, 255, 255, 255, 255, 255,
             255, 246, 228, 211, 193, 175, 158, 140};
    int[] green =
            {0, 0, 0, 0, 0, 0, 0, 0,
             0, 11, 27, 43, 59, 75, 91, 107,
             123, 139, 155, 171, 187, 203, 219, 235,
             251, 255, 255, 255, 255, 255, 255, 255,
             255, 255, 255, 255, 255, 255, 255, 255,
             255, 247, 231, 215, 199, 183, 167, 151,
             135, 119, 103, 87, 71, 55, 39, 23,
             7, 0, 0, 0, 0, 0, 0, 0};
    int[] blue =
            {0, 143, 159, 175, 191, 207, 223, 239,
             255, 255, 255, 255, 255, 255, 255, 255,
             255, 255, 255, 255, 255, 255, 255, 255,
             255, 247, 231, 215, 199, 183, 167, 151,
             135, 119, 103, 87, 71, 55, 39, 23,
             7, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0};

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

  public void showNewFrame() {
    setVisible(false);
    new ThreeDMapPlot(getFrameParent(), data, title, IntensityMin, IntensityMax);
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
    JOptionsDialog adialog = new J3DMapPlotD(this, null);
    adialog.setVisible(true);
  }

  class J3DMapPlotD extends JOptionsDialog {

    JComboBox plotModeCB;
    JTextField legendMinTF;
    JTextField legendMaxTF;

    public J3DMapPlotD(Frame parent, XRDcat obj) {

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

      J3DMapPlotD.this.setTitle("Plotting options");
      initParameters();
      J3DMapPlotD.this.pack();
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
