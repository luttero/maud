/*
 * @(#)PoleFigureMap.java created Jun 1, 2003 Berkeley
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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
import gov.noaa.pmel.sgt.IndexedColorMap;
import gov.noaa.pmel.sgt.LinearTransform;
import gov.noaa.pmel.sgt.dm.SGTMetaData;
import gov.noaa.pmel.sgt.dm.SimpleGrid;
import gov.noaa.pmel.sgt.swing.*;
import gov.noaa.pmel.sgt.swing.prop.GridAttributeDialog;
import gov.noaa.pmel.util.*;
import it.unitn.ing.jgraph.ThermalColorMap;
import it.unitn.ing.rista.util.MaudPreferences;
import it.unitn.ing.rista.util.MoreMath;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * The LegendPoleFigureMap is a class
 *
 * @version $Revision: 1.8 $, $Date: 2006/01/19 14:45:53 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class LegendPoleFigureMap extends JPanel {

	int nslicesW = 0;
	int nslicesH = 0;
	int colrsNumber = 16;
	double scaleMin = 0;
	double scaleMax = 0;
	boolean grayScale = false;
	String label = "";
	JMapPlotLayout rpl_data = null;
	int defaultSize = 200;
	double zoom = 1.0;
	Point2D.Double origin = null;

	public LegendPoleFigureMap(double[][] grid, int nslicesW, int nslicesH, double scaleMin, double scaleMax,
	    boolean grayScale, String label, int colrsNumber, Object editMenu, double zoom, int defaultsize,
	    double origin, int scaleType) {
	    
		this.nslicesW = nslicesW;
		this.nslicesH = nslicesH;
		this.origin = new Point2D.Double(0, origin);
		this.colrsNumber = colrsNumber;
		this.scaleMin = scaleMin;
		this.scaleMax = scaleMax;
		this.grayScale = grayScale;
		this.label = label;
		this.zoom = zoom;
		defaultSize = defaultsize;
		setLayout(new BorderLayout());
		rpl_data = makeGraph(grid, scaleMin, scaleMax, scaleType);

		rpl_data.setBatch(true);
		add(rpl_data, BorderLayout.CENTER);
		createEditMenu(editMenu);
	}

	public void setBatch(boolean value) {
		if (rpl_data != null)
			rpl_data.setBatch(value);
	}

	public Dimension getPreferredSize() {
		if (zoom == 0)
			zoom = 1.0;
		int size = (int) (defaultSize * zoom + inset * 2);
		return new Dimension(size, size);
	}

	public static int inset = 6;

	JMapPlotLayout makeGraph(double[][] grid, double min, double max, int scaleType) {
    /*
     * This example uses a pre-created "Layout" for raster time
     * series to simplify the construction of a plot. The
     * JMapPlotLayout can plot a single grid with
     * a ColorKey, time series with a LineKey, point collection with a
     * PointCollectionKey, and general X-Y plots with a
     * LineKey. JMapPlotLayout supports zooming, object selection, and
     * object editing.
     */

		JMapPlotLayout rpl;
		ContourLevels clevels;

    /* create the grid data */
		double[] xaxis, yaxis;
		double[] values;

		int ylength = nslicesH;
		int xlength = nslicesW;
		xaxis = new double[xlength];
		yaxis = new double[ylength];
		values = new double[xlength * ylength];
		int j = 0;
		double amin = max;
		double amax = min;
		for (int i = 0; i < xlength; i++) {
			xaxis[i] = i;
			for (int sn = 0; sn < ylength; sn++) {
				if (i == 0)
					yaxis[sn] = grid[i][sn];
				values[j++] = grid[i][sn];
				if (!Double.isNaN(grid[i][sn])) {
					amin = Math.min(amin, grid[i][sn]);
					amax = Math.max(amax, grid[i][sn]);
				}
			}
		}
//    System.out.println("Pole figure " + label + ", min/max : " + amin + " " +amax);
    /* Contour plot lines, defining range */
		double deltaRange = ((max - min) / 8);
		Range2D datar = new Range2D(min, max, deltaRange);

		//
		// create SimpleGrid
		//
		SGTMetaData zMeta = new SGTMetaData("", "");
		SGTMetaData xMeta = new SGTMetaData("", "");

		SGTMetaData yMeta = new SGTMetaData("Probability", label);
		SimpleGrid sg = new SimpleGrid(values, xaxis, yaxis, "");
		sg.setXMetaData(xMeta);
		sg.setYMetaData(yMeta);
		sg.setZMetaData(zMeta);

    /*
     * Create the layout without a Logo image and with the
     * ColorKey on a separate Pane object.
     */
		rpl = new JMapPlotLayout(true, false, false, true, scaleType == 0,"Pole figure", null, true);
		rpl.setEditClasses(false);
    /*
     * Create a GridAttribute for CONTOUR style.
     */

//    if (scaleType == 0) {
	    clevels = ContourLevels.getDefault(datar);
/*    } else {
	    clevels = new ContourLevels();
	    double logStart = MoreMath.log10(datar.start);
	    double logEnd = MoreMath.log10(datar.end);
	    double logDelta = ((logEnd - logStart) / 8);
	    double val = logStart;
	    while(val <= logEnd) {
		    clevels.addLevel(Math.pow(10, val));
		    val = val + logDelta;
	    }
    }*/
		if (clevels != null) {
			DefaultContourLineAttribute d_attr = clevels.getDefaultContourLineAttribute();
			d_attr.setLabelEnabled(false);
		}
		GridAttribute gridAttr_ = new GridAttribute(clevels);
    /*
     * Create a ColorMap and change the style to RASTER.
     */
		gov.noaa.pmel.sgt.ColorMap cmap = createColorMap(grayScale, scaleType, min, max, colrsNumber);
		gridAttr_.setColorMap(cmap);
		boolean contour = MaudPreferences.getBoolean("plotPF.useContourForLegend", false);
		if (contour)
			gridAttr_.setStyle(GridAttribute.RASTER_CONTOUR);  // or AREA_FILL
		else
			gridAttr_.setStyle(GridAttribute.RASTER);  // or AREA_FILL
    /*
     * Add the grid to the layout and give a label for
     * the ColorKey.
     */
		rpl.addData(sg, gridAttr_, "");
		if(cmap instanceof TransformAccess) {
			((TransformAccess)cmap).setRange(new Range2D(min, max));
		}
    /*
     * Change the layout's three title lines.
     */

		rpl.setTitles("", "", "");
    /*
     * Resize the graph  and place in the "Center" of the frame.
     */
//    rpl.setSize(new Dimension(400, 400));
		Dimension2D dim2D = new Dimension2D(1, 6);
		rpl.setLayerSizeP(dim2D);
//    rpl.setAxesOriginP(new Point2D.Double(0.1, 0.1));
		rpl.setTitleHeightP(0.6, 0.01);
		SGLabel title = rpl.getTitle();
		title.setLocationP(new Point2D.Double(0, 5.0));
		title.setAlign(SGLabel.RIGHT, SGLabel.TOP);

		CartesianGraph graph = (CartesianGraph) rpl.getFirstLayer().getGraph();
		try {
			graph.getXAxis("Bottom Axis").setVisible(false);
			if (scaleType == 1) {
				PlainLogAxis axis = (PlainLogAxis) graph.getYAxis("Left Axis");
				axis.setLabelHeightP(0.4 * zoom);

				axis.getTitle().setHeightP(0.4 * zoom);
				axis.setVisible(true);
//			axis.setRangeU(new Range2D(min, max));
				axis.setLocationU(origin);
			} else {
				PlainAxis axis = (PlainAxis) graph.getYAxis("Left Axis");
				axis.setLabelHeightP(0.4 * zoom);

				axis.getTitle().setHeightP(0.4 * zoom);
				axis.setVisible(true);
//			axis.setRangeU(new Range2D(min, max));
				axis.setLocationU(origin);
			}
		} catch (AxisNotFoundException e) {
		}
		JComponent circleLayer = new JComponent() {
			public void paint(Graphics g) {
//			  System.out.println(getWidth() + " " + getHeight());
				Graphics2D g2d = (Graphics2D) g;
				g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER));
				g.setColor(Color.black);
				Container parent = getParent();
				while (parent != null && !(parent instanceof JPlotLayout)) {
					parent = parent.getParent();
				}
				if (parent != null) {
					JPlotLayout plot = (JPlotLayout) parent;
					Layer layer = plot.getFirstLayer();
/*				  Enumeration en = layer.childElements();
				  while (en.hasMoreElements()) {
					  Object comp = en.nextElement();
					  System.out.println(comp.toString());
				  }*/
					CartesianGraph graph = (CartesianGraph) layer.getGraph();
					Rectangle dim = graph.getRenderer().drawingRectangle;
					Stroke stroke = g2d.getStroke();
					g2d.setStroke(new BasicStroke(1));
					g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
					g.drawRect(dim.x, dim.y, dim.width - 1 - dim.x, dim.height - 1 - dim.y);
					g2d.setStroke(stroke);
				}
			}
		};
		circleLayer.setSize((int) dim2D.getWidth(), (int) dim2D.getHeight());
		rpl.add(circleLayer, 1);

    /*
     * Resize the key Pane, both the device size and the physical
     * size. Set the size of the key in physical units and place
     * the key pane at the "South" of the frame.
     */
//    rpl.setKeyLayerSizeP(new Dimension2D(6.0, 1.02));
//    rpl.setKeyBoundsP(new Rectangle2D.Double(0.01, 1.01, 5.98, 1.0));

		return rpl;
	}

	public static gov.noaa.pmel.sgt.ColorMap createColorMap(boolean grayScale, int scaleType, double min, double max, int colrsNumber) {
//    int colrsNumber = 64;
		int[] red = new int[colrsNumber], green = new int[colrsNumber], blue = new int[colrsNumber];
		if (grayScale) {
			double fract = 248.0 / (colrsNumber - 1);
			if (MaudPreferences.getBoolean("plotPF.colors_inverted", false)) {
				for (int i = 0; i < colrsNumber; i++) {
					int grayLevel = (int) (fract * i);
					red[i] = grayLevel;
					green[i] = grayLevel;
					blue[i] = grayLevel;
				}
			} else {
				for (int i = 0; i < colrsNumber; i++) {
					int grayLevel = (int) (fract * (colrsNumber - i - 1));
					red[i] = grayLevel;
					green[i] = grayLevel;
					blue[i] = grayLevel;
				}
			}
		} else if (colrsNumber == 64) {
			for (int i = 0; i < 64; i++) {
				red[i] = ThermalColorMap.red64[i];
				green[i] = ThermalColorMap.green64[i];
				blue[i] = ThermalColorMap.blue64[i];
			}
		} else if (colrsNumber == 16) {
			for (int i = 0; i < 16; i++) {
				red[i] = ThermalColorMap.red16[i];
				green[i] = ThermalColorMap.green16[i];
				blue[i] = ThermalColorMap.blue16[i];
			}
		} else {
			// 1/3 -> 0     2/3 -> 0-255    5/6 -> 255   6/6 -> 255-(255*.6)
			// 1/3 -> 0-255     2/3 -> 255     3/3 -> 255-0
			// 1/6 -> (255*.6)-255   2/6 -> 255   2/3 -> 255-0     3/3 -> 0

			red[0] = 0;
			green[0] = 0;
			blue[0] = 0;
			for (int i = 1; i < colrsNumber; i++) {
				int[] sectorAndRem = getSectorAndRemaining(i - 1, colrsNumber - 1);
				switch (sectorAndRem[0]) {
					case 1:
						red[i] = 0;
						green[i] = 255 - sectorAndRem[2];
						blue[i] = 255 / 2 + (255 / 2 - sectorAndRem[1]);
						break;
					case 2:
						red[i] = 0;
						green[i] = 255 - sectorAndRem[2];
						blue[i] = 255;
						break;
					case 4:
						red[i] = 255 - sectorAndRem[2];
						green[i] = 255;
						blue[i] = sectorAndRem[2];
						break;
					case 5:
						red[i] = 255;
						green[i] = sectorAndRem[2];
						blue[i] = 0;
						break;
					default:
					{
						// case 6
						red[i] = 255 - (255 / 2 - sectorAndRem[1]);
						green[i] = sectorAndRem[2];
						blue[i] = 0;
					}
				}
//      System.out.println(i + " "+sectorAndRem[0]+" "+sectorAndRem[1]+" "+sectorAndRem[2]);
				if (red[i] < 0)
					red[i] = 0;
				if (red[i] > 255)
					red[i] = 255;
				if (green[i] < 0)
					green[i] = 0;
				if (green[i] > 255)
					green[i] = 255;
				if (blue[i] < 0)
					blue[i] = 0;
				if (blue[i] > 255)
					blue[i] = 255;
//        System.out.println(i+" " +red[i]+" "+green[i]+" "+blue[i]);
			}
		}

		IndexedColorMap cmap = new IndexedColorMap(red, green, blue);
		AxisTransform trasform = null;
		if (scaleType == 1)
			trasform = new LinearTransform(0.0, (double) red.length, min, max);
		else
			trasform = new LinearTransform(0.0, (double) red.length, min, max);
		cmap.setTransform(trasform);
		return cmap;
	}

	public static int[] getSectorAndRemaining(double i, double numberOfColors) {
		int[] sector = new int[3];
		int sect3 = 0;
		if (i < numberOfColors / 6.0) {
			sector[0] = 1;
			sect3 = 1;
		} else if (i < numberOfColors / 3.0) {
			sector[0] = 2;
			sect3 = 1;
		} else if (i < 2.0 / 3.0 * numberOfColors) {
			sector[0] = 4;
			sect3 = 2;
		} else if (i < 5.0 / 6.0 * numberOfColors) {
			sector[0] = 5;
			sect3 = 3;
		} else {
			sector[0] = 6;
			sect3 = 3;
		}
		double res = (1.0 / 6.0 * sector[0] + 0.00000001 - i / numberOfColors) * 6 / 2 * 255.0;
		sector[1] = (int) res;
		res = (1.0 / 3.0 * sect3 + 0.00000001 - i / numberOfColors) * 3 * 255.0;
		sector[2] = (int) res;
		return sector;
	}

	public void createEditMenu(Object editMenu) {
		if (editMenu instanceof JMenu) {
			JMenuItem menuitem = null;

//    editMenu.setMnemonic('e');
			JMenu poleEditMenu = new JMenu("Edit " + label);
			((JMenu) editMenu).add(poleEditMenu);
			poleEditMenu.add(menuitem = new JMenuItem("Tree View"));
			menuitem.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					tree_actionPerformed(event);
				}
			});

			poleEditMenu.add(menuitem = new JMenuItem("GridAttribute"));
			menuitem.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					edit_actionPerformed(event);
				}
			});
		} else if (editMenu instanceof Menu) {
			MenuItem menuitem = null;

//    editMenu.setMnemonic('e');
			Menu poleEditMenu = new Menu("Edit " + label);
			((Menu) editMenu).add(poleEditMenu);
			poleEditMenu.add(menuitem = new MenuItem("Tree View"));
			menuitem.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					tree_actionPerformed(event);
				}
			});

			poleEditMenu.add(menuitem = new MenuItem("GridAttribute"));
			menuitem.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent event) {
					edit_actionPerformed(event);
				}
			});
		}

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
     * Create a JClassTree for the JMapPlotLayout objects
     */
		JClassTree ct = new JClassTree();
		ct.setModal(false);
		ct.setJPane(rpl_data);
		ct.show();
	}

}
