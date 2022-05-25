/*
 * @(#)GeneticAlgorithmPlot.java created 12/12/2002 Mesiano
 *
 * Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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

import ec.EvolutionState;
import ec.util.Parameter;
import gov.noaa.pmel.sgt.*;
import gov.noaa.pmel.sgt.dm.Collection;
import gov.noaa.pmel.sgt.dm.SimplePoint;
import gov.noaa.pmel.sgt.swing.JPlotLayout;
import gov.noaa.pmel.util.Dimension2D;
import gov.noaa.pmel.util.Point2D;
import gov.noaa.pmel.util.Range2D;
import it.unitn.ing.rista.diffr.XRDcat;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;


/**
 * GeneticAlgorithmPlot represent graphically the genetic algorithm structure solution.
 *
 * @version $Revision: 1.4 $, $Date: 2003/04/07 10:14:24 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GeneticAlgorithmPlot extends myJFrame implements PropertyChangeListener {


	Collection GAData;
	JPane mainPane_;
	CartesianGraph graph_;
	LinearTransform xt_, yt_;
	gov.noaa.pmel.sgt.Layer layer_;
	PlainAxis x_axis;
	PlainAxis y_axis;
	Range2D xrange_;
	Range2D yrange_;
	String title = null;
	public JPlotLayout layout_;

	static final int XINTERVALS = 10;
	static final int YINTERVALS = 5;


	public GeneticAlgorithmPlot(Frame parent, final EvolutionState state) {

		super(parent);

		mainPane_ = new JPane("Genetic Algorithm Structure Solution", new Dimension(600, 400));
		mainPane_.setLayout(new StackedLayout());
		mainPane_.setBackground(Color.white);

		xrange_ = new Range2D(0, state.numGenerations);
		yrange_ = new Range2D(0, state.parameters.getDoubleWithDefault(
			new Parameter("max-fitness"), null, 1.0));

		GAData = new Collection("GAPoints", 100);
		GAData.addElement(new SimplePoint(0, -10E50, "Start Fitness"));

		double xsize = 6;
		double xstart = 0.5;
		double xend = 5.5;
		double ysize = 4;
		double ystart = 0.5;
		double yend = 3.5;

		xt_ = new LinearTransform(xstart, xend, xrange_.start, xrange_.end);
		yt_ = new LinearTransform(ystart, yend, yrange_.start, yrange_.end);

		//TestData td = new TestData(xrange_, yrange_, 50);
		//Collection newData = td.getCollection();

		layer_ = new gov.noaa.pmel.sgt.Layer("GA", new Dimension2D(xsize, ysize));
		mainPane_.add(layer_);

		graph_ = new CartesianGraph("GA Graph");
		layer_.setGraph(graph_);
		graph_.setXTransform(xt_);
		graph_.setYTransform(yt_);

		PointAttribute pattr;
		Color markColor = new Color(200, 0, 255);
		pattr = new PointAttribute(9, markColor);
		pattr.setWidthP(3);
		pattr.setMarkHeightP(0.2);

		graph_.setData(GAData, pattr);

/*
 * Create the bottom axis, set its range in user units
 * and its origin. Add the axis to the graph.
 */

		x_axis = new PlainAxis("Generations");
		x_axis.setRangeU(xrange_);
		x_axis.setLocationU(new Point2D.Double(xrange_.start, yrange_.start));
		Font xbfont = new Font("Helvetica", Font.ITALIC, 14);
		x_axis.setLabelFont(xbfont);
		SGLabel xtitle = new SGLabel("xaxis title", "Generation", new Point2D.Double(0.0, 0.0));

		xtitle.setFont(new Font("Helvetica", Font.PLAIN, 14));
		xtitle.setHeightP(0.2);
		x_axis.setTitle(xtitle);
		graph_.addXAxis(x_axis);

/*
 * Create the left axis, set its range in user units
 * and its origin. Add the axis to the graph.
 */

		y_axis = new PlainAxis("Left Axis");
		y_axis.setRangeU(yrange_);
		y_axis.setLocationU(new Point2D.Double(xrange_.start, yrange_.start));
		y_axis.setLabelFont(xbfont);
		SGLabel ytitle = new SGLabel("yaxis title", "Fitness",
			new Point2D.Double(0.0, 0.0));

		ytitle.setFont(new Font("Helvetica", Font.PLAIN, 14));
		ytitle.setHeightP(0.2);
		y_axis.setTitle(ytitle);
		graph_.addYAxis(y_axis);

		/*layout_ = new JPlotLayout(newData, "Genetic Algorithm Structure Solution", null, false);
		layout_.setBatch(true);
		layout_.setTitles("Fitness", "Generation", "");
		layout_.setTitleHeightP(0.2, 0.2);
		layout_.addData( newData, "Random Data");
		layout_.setBatch(false);*/

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(mainPane_, BorderLayout.CENTER);
		pack();
		setVisible(true);
		mainPane_.setBatch(false);



		/*framePositionX = "MapPlot2D.framePositionX";
		framePositionY = "MapPlot2D.framePositionY";
		defaultFramePositionX = 10;
		defaultFramePositionY = 20;
		setOwnPosition = true;*/

	}

	public void setXRange(double xMin, double xMax) {
		mainPane_.setBatch(true);
		xrange_ = new Range2D(xMin, xMax);
		xt_.setRangeU(xrange_);
		x_axis.setRangeU(xrange_);
		mainPane_.setBatch(false);
	}

	public void setYRange(double yMin, double yMax) {
		mainPane_.setBatch(true);
		yrange_ = new Range2D(yMin, yMax);
		yt_.setRangeU(yrange_);
		y_axis.setRangeU(yrange_);
		mainPane_.setBatch(false);
	}

	public void setPlotRange(double xMin, double xMax, double yMin, double yMax) {
		boolean batch = mainPane_.isBatch();
		mainPane_.setBatch(true);
		xrange_ = graph_.computeRange(new Range2D(xMin, xMax), XINTERVALS);
		yrange_ = graph_.computeRange(new Range2D(yMin, yMax), YINTERVALS);

		xt_.setRangeU(xrange_);
		yt_.setRangeU(yrange_);
		x_axis.setRangeU(xrange_);
		y_axis.setRangeU(yrange_);
		x_axis.setLocationU(new Point2D.Double(xrange_.start, yrange_.start));
		y_axis.setLocationU(new Point2D.Double(xrange_.start, yrange_.start));

    if(!batch) mainPane_.setBatch(false);
	}

	public void addPoint(int gNum, double Fit) {
		mainPane_.setBatch(true);
		GAData.addElement(new SimplePoint(gNum, Fit, "Fitness"));

		if (gNum > xrange_.end)
			setPlotRange(xrange_.start, (int) 1.2*gNum, yrange_.start, yrange_.end);
		if (Fit > yrange_.end)
			setPlotRange(xrange_.start, xrange_.end, yrange_.start, (int) 1.2*Fit);

		mainPane_.setModified(true, "forced setModified");
		mainPane_.setBatch(false);
	}


	private void resetRange() {
		/*
		 * A change in the range has occured. Get new range
		 * and set transforms, axes, and origin appropriately.
		 */
		mainPane_.setBatch(true);
		/*SoTRange.GeoDate xrange = (SoTRange.GeoDate)rtData_.getXRange();
		SoTRange.Double yrange = (SoTRange.Double)rtData_.getYRange();
		SoTPoint origin = new SoTPoint(xrange.start, yrange.start);
		xt_.setRangeU(xrange);
		yt_.setRangeU(yrange);
		x_axis.setRangeU(xrange);
		x_axis.setLocationU(origin);
		y_axis.setRangeU(yrange);
		y_axis.setLocationU(origin);*/
		mainPane_.setBatch(false);
	}

	public void propertyChange(PropertyChangeEvent evt) {
		/**
		 * dataModified property is handled by CartesianGraph
		 * only need to look for rangeModified here to make sure
		 * range is properly updated
		 */
		if ("rangeModified".equals(evt.getPropertyName())) {
			resetRange();
		}
	}

	public void showOptionsDialog() {
		JOptionsDialog adialog = new GAPlotOptionsD(this, null);
		adialog.setVisible(true);
	}

	class GAPlotOptionsD extends JOptionsDialog {


		public GAPlotOptionsD(Frame parent, XRDcat obj) {
			super(parent, obj);
		}



		/*principalPanel.setLayout(new GridLayout(0,2,6,6));

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
		initParameters();*/


		public void initParameters() {
		}

		public void retrieveParameters() {
		}
	}

}
