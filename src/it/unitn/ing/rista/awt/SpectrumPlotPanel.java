/*
 * @(#)SpectrumPlotPanel.java created Jan 27, 2004 Casalino
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

import it.unitn.ing.jgraph.*;
import it.unitn.ing.rista.chemistry.AtomInfo;
import it.unitn.ing.rista.chemistry.XRayDataSqLite;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.instrument.XRFInstrument;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.interfaces.Peak;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.IOException;
import java.util.Vector;

import org.javadev.AnimatingCardLayout;


/**
 * The SpectrumPlotPanel is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.10 $, $Date: 2006/11/10 09:32:59 $
 * @since JDK1.1
 */

public class SpectrumPlotPanel extends CopyPrintablePanel {

  //      Graph2D graph;
  CopyPrintPanel graphPanel = null;
  DataSet data1 = null;
  Axis xaxis = null;
  Axis yaxis = null;
  int np;
  DataSet dataFit = null;
  DataSet datar = null;
  PeakSet[] datap = null;
	G2Dint residuals = null;
  G2Dint positions = null;
  double[][] peaksList = null;
  double[] secondDerivative = null;
  public DiffrDataFile[] datafile = null;
	public DataFileSet dataset = null;
  Axis yaxisp = null;
  Axis xaxisr = null;
  Axis yaxisr = null;

	private static Vector<PeakInfo> peaksInfoXRF = null;
	private static double energyUsedForPeakInfoXRF = MaudPreferences.getDouble("xrf_peaks_info.maximumEnergyInKeV", 30.0);
  public static int defaultMarker = 2;
  public static int markerNumber = defaultMarker;
  public static double markerScale = 1;
  public static Color markerColor = Color.black;
	public static Color fitColor = Color.red;
  public static int XaxisTitleFontScale = 16;
  public static int XaxisLabelFontScale = 16;
  public static Color XaxisTitleColor = Color.blue;
  public static int YaxisTitleFontScale = 16;
  public static int YaxisLabelFontScale = 16;
  public static Color YaxisTitleColor = Color.blue;
  public static int PhasesFontScale = 14;

  public static String[] plotMode = {"sqrt", "linear", "log10", "sqrt*q", "sqrt*q^2", "sqrt*q^4",
		  "linear*q", "linear*q^2", "linear*q^4", "log10*q", "log10*q^2", "log10*q^4"};
  public static String[] xplotMode = {"Default", "d-space", "Q space", "Energy"};
  public static final String xaxisModePref = "plot.XaxisMode";
  public static final String plotNoBkg = "plot.backgroundSubtraction";
  public static boolean plotNoBkgDefault = false;
  public static String axisFont = MaudPreferences.getPref("plot.axisFont", "TimesRoman");
  public static String labelFont = MaudPreferences.getPref("plot.labelFont", "Helvetica");
  public static boolean blackAndWhite = MaudPreferences.getBoolean("plot.black&white", false);
  public static boolean plotResiduals = MaudPreferences.getBoolean("plot.plotResiduals", true);
  public static boolean plotPeaks = MaudPreferences.getBoolean("plot.plotPeaks", true);
  public static boolean plotBackground = MaudPreferences.getBoolean("plot.plotBackground", false);

	G2Dint graph = null;
//  CopyPrintPanel fullGraphPanel = null;
  AnimatingCardLayout panelLayout = null;
  JTextField markerNumberTF;
  JTextField markerScaleTF;
  JTextField axisTitleFontTF;
  JTextField XaxisTitleFontScaleTF;
  JTextField YaxisTitleFontScaleTF;
  JTextField LabelFontTF;
  JTextField XaxisLabelFontScaleTF;
  JTextField YaxisLabelFontScaleTF;
  JTextField PhasesFontScaleTF;
  JComboBox plotModeCB, xplotModeCB;

  JCheckBox subtractBackground;
  JCheckBox calibrateIntensity;
	JCheckBox calibrateForLPIntensity;
  JCheckBox blackAndWhiteCB;
  JCheckBox plotResidualsCB;
  JCheckBox plotPeaksCB;
  JCheckBox plotBackgroundCB;

  JTextField legendMinTF;
  JTextField legendMaxTF;
  JTextField legendxMinTF;
  JTextField legendxMaxTF;
  double IntensityMin = 0.0, IntensityMax = 0.0;
  double xMin = 0.0, xMax = 0.0;
  boolean summedDatafiles = false;
  boolean computeMinMax = true;

  public SpectrumPlotPanel() {
    this(null);
  }

  public SpectrumPlotPanel(boolean summedDatafiles) {
    this();
    this.summedDatafiles = summedDatafiles;
  }

	public SpectrumPlotPanel(DataFileSet adata) {

		initResources();

		dataset = adata;
		graphPanel = createGraph(false);

		setComponentToPrint(graphPanel);

		setLayout(new BorderLayout());
		add(graphPanel, BorderLayout.CENTER);

	}

	public SpectrumPlotPanel(DiffrDataFile[] afile, double[][] peaks,
                           double[] derivative2) {

    initResources();

//    panelLayout = new AnimatingCardLayout();
//    panelLayout.setAnimation(new SlideAnimation());
//    setLayout(panelLayout);
//    JPanel principalPlotPanel = new JPanel(new BorderLayout());

//    add(principalPlotPanel, "plot");

    datafile = afile;
    if (afile == null || afile.length > 1)
      graphPanel = createGraph(afile, peaks, derivative2, false);
    else
      graphPanel = createGraphSingle(afile[0], peaks, derivative2, false);

    setComponentToPrint(graphPanel);

//    principalPlotPanel.add(graphPanel, BorderLayout.CENTER);
    setLayout(new BorderLayout());
    add(graphPanel, BorderLayout.CENTER);

  }

  public JPanel createOptionsPanel(final myJDialog theDialog) {
    JPanel container = new JPanel(new FlowLayout(FlowLayout.CENTER));
    JPanel layoutPanel = new JPanel(new BorderLayout(Constants.borderInsideMinimum,
        Constants.borderInsideMinimum));
    container.add(layoutPanel);
    JPanel leftOptionsPanel = new JPanel(new GridLayout(0, 2, Constants.borderInsideMinimum,
        Constants.borderInsideMinimum));
    layoutPanel.add(leftOptionsPanel, BorderLayout.CENTER);

    leftOptionsPanel.add(new JLabel("Intensity scale mode: "));
    plotModeCB = new JComboBox();
    int nmode = PlotDataFile.plotMode.length;
    for (int i = 0; i < nmode; i++)
      plotModeCB.addItem(PlotDataFile.plotMode[i]);
    plotModeCB.setToolTipText("Choose the scale mode for the intensity axis");
    leftOptionsPanel.add(plotModeCB);

    leftOptionsPanel.add(new JLabel("X-axis plot mode: "));
    xplotModeCB = new JComboBox();
    nmode = PlotDataFile.xplotMode.length;
    for (int i = 0; i < nmode; i++)
      xplotModeCB.addItem(PlotDataFile.xplotMode[i]);
    xplotModeCB.setToolTipText("Choose the plot mode for the x-axis");
    leftOptionsPanel.add(xplotModeCB);

    subtractBackground = new JCheckBox("Subtract background");
    leftOptionsPanel.add(subtractBackground);

    calibrateIntensity = new JCheckBox("Calibration Correction");
    leftOptionsPanel.add(calibrateIntensity);

	  calibrateForLPIntensity = new JCheckBox("Lorentz-Polar. Corr.");
	  leftOptionsPanel.add(calibrateForLPIntensity);

	  blackAndWhiteCB = new JCheckBox("Black & White");
    leftOptionsPanel.add(blackAndWhiteCB);

    plotResidualsCB = new JCheckBox("Residuals");
    leftOptionsPanel.add(plotResidualsCB);

    plotPeaksCB = new JCheckBox("Peak positions");
    leftOptionsPanel.add(plotPeaksCB);

    plotBackgroundCB = new JCheckBox("Plot background");
    leftOptionsPanel.add(plotBackgroundCB);

    leftOptionsPanel.add(new JLabel(""));  // to make it even

    leftOptionsPanel.add(new JLabel("Data marker number: "));
    markerNumberTF = new JTextField(Constants.FLOAT_FIELD);
    markerNumberTF.setToolTipText(
        "The shape of the data marker by number (-1 for line plot; see file markers.txt in the maud.jar)");
    leftOptionsPanel.add(markerNumberTF);

    leftOptionsPanel.add(new JLabel("Data marker scale factor: "));
    markerScaleTF = new JTextField(Constants.FLOAT_FIELD);
    markerScaleTF.setToolTipText("The dimension of the data marker is scaled by this factor");
    leftOptionsPanel.add(markerScaleTF);

    leftOptionsPanel.add(new JLabel("Axes title font: "));
    axisTitleFontTF = new JTextField(Constants.FLOAT_FIELD);
    axisTitleFontTF.setToolTipText("The font used for the title of the both axes");
    leftOptionsPanel.add(axisTitleFontTF);

    leftOptionsPanel.add(new JLabel("X axis title font size: "));
    XaxisTitleFontScaleTF = new JTextField(Constants.FLOAT_FIELD);
    XaxisTitleFontScaleTF.setToolTipText("The size of the font used for the title of the X axis");
    leftOptionsPanel.add(XaxisTitleFontScaleTF);

    leftOptionsPanel.add(new JLabel("Y axis title font size: "));
    YaxisTitleFontScaleTF = new JTextField(Constants.FLOAT_FIELD);
    YaxisTitleFontScaleTF.setToolTipText("The size of the font used for the title of the Y axis");
    leftOptionsPanel.add(YaxisTitleFontScaleTF);

    leftOptionsPanel.add(new JLabel("Labels font: "));
    LabelFontTF = new JTextField(Constants.FLOAT_FIELD);
    LabelFontTF.setToolTipText("The font used for the labels of the both axes");
    leftOptionsPanel.add(LabelFontTF);

    leftOptionsPanel.add(new JLabel("X axis label font size: "));
    XaxisLabelFontScaleTF = new JTextField(Constants.FLOAT_FIELD);
    XaxisLabelFontScaleTF.setToolTipText("The size of the font used for the label of the X axis");
    leftOptionsPanel.add(XaxisLabelFontScaleTF);

    leftOptionsPanel.add(new JLabel("Y axis label font size: "));
    YaxisLabelFontScaleTF = new JTextField(Constants.FLOAT_FIELD);
    YaxisLabelFontScaleTF.setToolTipText("The size of the font used for the label of the Y axis");
    leftOptionsPanel.add(YaxisLabelFontScaleTF);

    leftOptionsPanel.add(new JLabel("Phase label font size: "));
    PhasesFontScaleTF = new JTextField(Constants.FLOAT_FIELD);
    PhasesFontScaleTF.setToolTipText("The size of the font used for the phase labels (only if a fit is available)");
    leftOptionsPanel.add(PhasesFontScaleTF);

    initParameters();

    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, Constants.borderInside, Constants.borderInside));
    layoutPanel.add(buttonPanel, BorderLayout.SOUTH);
    JButton doneB = new JButton("Done");
    buttonPanel.add(doneB);
    doneB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        boolean preserveLimits = retrieveParameters();
        replot(preserveLimits);
        theDialog.setVisible(false);
        theDialog.dispose();
//        panelLayout.show(SpectrumPlotPanel.this, "plot");
      }
    });

    return container;
  }

  public void initParameters() {
    markerNumberTF.setText(new String(Integer.toString(markerNumber)));
    markerScaleTF.setText(new String(Double.toString(markerScale)));
    XaxisTitleFontScaleTF.setText(new String(Integer.toString(XaxisTitleFontScale)));
    YaxisTitleFontScaleTF.setText(new String(Integer.toString(YaxisTitleFontScale)));
    axisTitleFontTF.setText(axisFont);
    LabelFontTF.setText(labelFont);
    XaxisLabelFontScaleTF.setText(new String(Integer.toString(XaxisLabelFontScale)));
    YaxisLabelFontScaleTF.setText(new String(Integer.toString(YaxisLabelFontScale)));
    PhasesFontScaleTF.setText(new String(Integer.toString(PhasesFontScale)));
    plotModeCB.setSelectedItem(MaudPreferences.getPref(principalJFrame.plotScale, PlotDataFile.plotMode[0]));
    xplotModeCB.setSelectedItem(MaudPreferences.getPref(xaxisModePref, xplotMode[0]));
    subtractBackground.setSelected(MaudPreferences.getBoolean(plotNoBkg, PlotDataFile.plotNoBkgDefault));
    calibrateIntensity.setSelected(MaudPreferences.getBoolean(PlotDataFile.plotCalIntensity,
        PlotDataFile.plotCalIntensityDefault));
	  calibrateForLPIntensity.setSelected(MaudPreferences.getBoolean(PlotDataFile.plotLPIntensity,
			  PlotDataFile.plotLPIntensityDefault));
    blackAndWhiteCB.setSelected(blackAndWhite);
    plotResidualsCB.setSelected(plotResiduals);
    plotPeaksCB.setSelected(plotPeaks);
    plotBackgroundCB.setSelected(plotBackground);
  }

  public boolean retrieveParameters() {
    markerNumber = Integer.valueOf(markerNumberTF.getText()).intValue();
    markerScale = Double.valueOf(markerScaleTF.getText()).doubleValue();
    axisFont = axisTitleFontTF.getText();
    labelFont = LabelFontTF.getText();
    MaudPreferences.setPref("plot.axisFont", axisFont);
    MaudPreferences.setPref("plot.labelFont", labelFont);
    XaxisTitleFontScale = Integer.valueOf(XaxisTitleFontScaleTF.getText()).intValue();
    YaxisTitleFontScale = Integer.valueOf(YaxisTitleFontScaleTF.getText()).intValue();
    XaxisLabelFontScale = Integer.valueOf(XaxisLabelFontScaleTF.getText()).intValue();
    YaxisLabelFontScale = Integer.valueOf(YaxisLabelFontScaleTF.getText()).intValue();
    PhasesFontScale = Integer.valueOf(PhasesFontScaleTF.getText()).intValue();
    MaudPreferences.setPref(principalJFrame.plotScale, plotModeCB.getSelectedItem().toString());
    MaudPreferences.setPref(plotNoBkg, subtractBackground.isSelected());
    blackAndWhite = blackAndWhiteCB.isSelected();
    plotResiduals = plotResidualsCB.isSelected();
    plotPeaks = plotPeaksCB.isSelected();
    plotBackground = plotBackgroundCB.isSelected();
    MaudPreferences.setPref("plot.black&white", blackAndWhite);
    MaudPreferences.setPref(PlotDataFile.plotCalIntensity, calibrateIntensity.isSelected());
	  MaudPreferences.setPref(PlotDataFile.plotLPIntensity, calibrateForLPIntensity.isSelected());
    MaudPreferences.setPref(xaxisModePref, xplotModeCB.getSelectedItem().toString());
    PlotDataFile.checkScaleMode();
    PlotDataFile.checkBackgroundSubtraction();
    PlotDataFile.checkCalibrateIntensity();
    return false;
  }

//	public static Color[] allColors = {Color.black, Color.red, Color.green, Color.blue, Color.magenta, Color.cyan,
//	Color.orange, Color.gray, Color.pink};

  public void initResources() {
    axisFont = MaudPreferences.getPref("plot.axisFont", "TimesRoman");
    labelFont = MaudPreferences.getPref("plot.labelFont", "Helvetica");
	  markerColor = getPastelColor(MaudPreferences.getInteger("plot.markerColor", 0));
	  fitColor = getPastelColor(MaudPreferences.getInteger("plot.fitColor", 1));
	  XaxisTitleColor = Color.black;
	  YaxisTitleColor = Color.black;
	  blackAndWhite = MaudPreferences.getBoolean("plot.black&white", false);
    if (blackAndWhite) {
      markerColor = Color.black;
	    fitColor = Color.black;
    }
  }

	public void replot(boolean preserveLimits) {
		if (dataset != null)
      setNewData(dataset, preserveLimits);
		else
			setNewData(datafile, null, null, preserveLimits);
		invalidate();
		getFrameParent().validate();
  }

	public void setNewData(DataFileSet adata, boolean keepMaxima) {
		if (keepMaxima && graph != null && graph.userLimits()) {
			double[] ranges = graph.getRanges();
			xMin = ranges[0];
			xMax = ranges[1];
			IntensityMin = ranges[2];
			IntensityMax = ranges[3];
//      System.out.println("Maxima: " + xMin + " " + xMax + " " + IntensityMin + " " + IntensityMax);
		} else
			keepMaxima = false;
		removeAll();
		adata.updateDataForPlot();

		initResources();

		dataset = adata;

		graphPanel = null;
		graph = null;

		graphPanel = createGraph(keepMaxima);

		setComponentToPrint(graphPanel);
		setLayout(new BorderLayout());
		add(graphPanel, BorderLayout.CENTER);
	}

	public void setNewData(DiffrDataFile[] afile, double[][] peaks,
                         double[] derivative2, boolean keepMaxima) {
    if (keepMaxima && graph != null && graph.userLimits()) {
      double[] ranges = graph.getRanges();
      xMin = ranges[0];
      xMax = ranges[1];
      IntensityMin = ranges[2];
      IntensityMax = ranges[3];
//      System.out.println("Maxima: " + xMin + " " + xMax + " " + IntensityMin + " " + IntensityMax);
    } else
      keepMaxima = false;
		removeAll();

    initResources();

	  datafile = afile;
	  if (afile == null || afile.length > 1)
		  graphPanel = createGraph(afile, peaks, derivative2, keepMaxima);
	  else
		  graphPanel = createGraphSingle(afile[0], peaks, derivative2, keepMaxima);

	  setComponentToPrint(graphPanel);
	  setLayout(new BorderLayout());
    add(graphPanel, BorderLayout.CENTER);
  }

  public Frame getFrameParent() {
    Container parent = getParent();
    while (parent.getParent() != null && !(parent instanceof Frame)) {
      parent = parent.getParent();
    }
    return (Frame) parent;
  }

  public void showOptionsDialog() {
    myJDialog theOptionsDialog = new myJDialog(getFrameParent(), true);
    JPanel optionsPanel = createOptionsPanel(theOptionsDialog);
    theOptionsDialog.getContentPane().add(optionsPanel);
    theOptionsDialog.pack();
    theOptionsDialog.setVisible(true);
  }

	public CopyPrintPanel createGraph(boolean keepMaxima) {
		CopyPrintPanel fullGraphPanel;
		int mode = PlotDataFile.checkScaleModeX();
		PlotDataFile.checkCalibrateIntensity();
		PlotDataFile.checkBackgroundSubtraction();
		XRayDataSqLite.checkMinimumEnergy();

		if (dataset == null) {
			return new NoDatafileCanvas();
		}
//    if (peaks != null)
		peaksList = null;
//    if (derivative2 != null)
		secondDerivative = null;
//    if (afile != null)
		boolean peaksLocated = (peaksList != null);

		boolean markExcludedRegion = MaudPreferences.getBoolean("excludedRegion.setZeroForPlot", true);  // todo

		double[] data = dataset.getDataForPlot();
		double[] datafit = dataset.getDatafitForPlot();
		double[] dataResidual = null;
		if (data != null)
			dataResidual = new double[data.length];
		double[] backgroundData = dataset.getBackgroundForPlot();
		double[][] dataphase = dataset.getDataphaseForPlot();

		if (data != null) {
			np = data.length / 2;
			setTitle("");
			FilePar filepar = dataset.getFilePar();

			int numberphases = filepar.getActiveSample().phasesNumber();

			Phase[] phaselist = new Phase[numberphases];
			for (int i = 0; i < numberphases; i++)
				phaselist[i] = filepar.getActiveSample().getPhase(i);

			fullGraphPanel = new CopyPrintPanelNoBkg();
//      fullGraphPanel.setBackground(Color.white);

//					KappaLayout gridbag = new KappaLayout();
			GridBagLayout gridbag = new GridBagLayout();
			GridBagConstraints c = new GridBagConstraints();

			fullGraphPanel.setLayout(gridbag);

/*
**      Create the Graph instance and modify the default behaviour
*/
			graph = new plotPanelG2Dint();

			graph.startedloading();

			graph.setDataBackground(Color.white);
			graph.setInfoDefaultLength(MaudPreferences.getInteger("plot.numberOfInfoPeaks", 10));

			fullGraphPanel.add(graph);
			c.fill = GridBagConstraints.BOTH;
			c.weighty = 1.0;
			c.weightx = 1.0;
			c.gridwidth = GridBagConstraints.REMAINDER;

			gridbag.setConstraints(graph, c);

			graph.drawzero = false;
			graph.drawgrid = false;
			graph.borderTop = 30;
			graph.borderBottom = 1;

/*
**      Load a file containing Marker definitions
*/
			Markers marker = null;
			try {
				marker = new Markers(Constants.documentsDirectory + "marker.txt");
			} catch (IOException ioe) {
				ioe.printStackTrace();
			}

			if (marker != null)
				graph.setMarkers(marker);

			// data

			data1 = graph.loadDataSet(data, np);
			if (markerNumber < 0 && datafit != null)
				markerNumber = defaultMarker;
			if (markerNumber < 0) {
				data1.linestyle = 1;
			} else {
				data1.linestyle = 0;
				data1.marker = markerNumber;
				data1.markerscale = markerScale;
				data1.markercolor = markerColor;
			}

			// fit

			if (datafit != null) {
				dataFit = graph.loadDataSet(datafit, np);
				if (blackAndWhite) {
					dataFit.linecolor = Color.black;
				} else {
					dataFit.linecolor = fitColor;
				}
			}

			// phases

			DataSet[] phaseData = new DataSet[numberphases];

			if (datafit != null) {
				for (int s = 0; s < numberphases; s++) {
					if (phaselist[s].plotFit()) {
						phaseData[s] = graph.loadDataSet(dataphase[s], np);
						phaseData[s].linestyle = 1;
						if (blackAndWhite)
							phaseData[s].linecolor = Color.black;
						else
							phaseData[s].linecolor = getPastelColor(s + 2);
					}
				}

			}

			// background

			DataSet bkgData = null;

			if (datafit != null && plotBackground) {
				bkgData = graph.loadDataSet(backgroundData, np);
				bkgData.linestyle = 1;
				if (blackAndWhite)
					bkgData.linecolor = Color.black;
				else
					bkgData.linecolor = getPastelColor(1);

			}

/*
**      Attach data sets to the Xaxis
*/
//          System.out.println("Attaching X-axis....");

			xaxis = graph.createXAxis();
			xaxis.axiscolor = Color.black;
			if ((plotResiduals && datafit != null) || peaksLocated) {
				xaxis.drawLabel = false;
				xaxis.drawTitle = false;
			}
			xaxis.attachDataSet(data1);
			if (datafit != null || peaksLocated) {
				xaxis.attachDataSet(dataFit);
			}
			if (datafit != null) {
				for (int s = 0; s < numberphases; s++)
					if (phaselist[s].plotFit())
						xaxis.attachDataSet(phaseData[s]);
				if (plotBackground)
					xaxis.attachDataSet(bkgData);
			}

/*
**      Attach the data set to the Left Axis
*/
//          System.out.println("Attaching Y-axis....");

			yaxis = graph.createYAxis();
			yaxis.attachDataSet(data1);
			if (datafit != null)
				yaxis.attachDataSet(dataFit);
			if (datafit != null) {
				for (int s = 0; s < numberphases; s++)
					if (phaselist[s].plotFit())
						yaxis.attachDataSet(phaseData[s]);
				if (plotBackground)
					yaxis.attachDataSet(bkgData);
			}

			yaxis.setTitleText(DiffrDataFile.getAxisYLegend());
			yaxis.setTitleFont(new Font(axisFont, Font.BOLD, YaxisTitleFontScale));
			yaxis.setLabelFont(new Font(labelFont, Font.PLAIN, YaxisLabelFontScale));
			yaxis.setTitleColor(YaxisTitleColor);
			yaxis.axiscolor = Color.black;

			int numberofPeaks = dataset.getNumberofPeaks();

			if (dataset.getInstrument() instanceof XRFInstrument) {
				numberofPeaks = 0;
				double energyInKeV = Constants.ENERGY_LAMBDA / dataset.getInstrument().getRadiationType().
						getShortestWavelengthForFluorescence() * 0.001;
				graph.attachPeaksInfos(getFluorescencePeakInfos(energyInKeV));
				graph.plot_type = graph.FLUORESCENCE_PLOT;
			}

			if ((plotPeaks && numberofPeaks > 0) && datafit != null) {

/*
**      prepare the positions box
*/
				if (!peaksLocated) {
					Vector<Peak> peaklist = dataset.getPeakList();

					int numberradiation = dataset.getInstrument().getRadiationType().getLinesCountForPlot();

					if (numberradiation == 0)
						numberradiation = 1;
					int numberofRefl = numberofPeaks;

					if (numberofRefl > 0) {

						Vector<PeakInfo> peaksInfos = new Vector<PeakInfo>(numberofPeaks / numberradiation);

						positions = new G2Dint(new plotPhaseMouseAdapter(), null);

						graph.addComponent(positions);

						c.weighty = 0.04 * (numberphases + 1);
						c.gridwidth = GridBagConstraints.REMAINDER;
//              c.h = numberphases + 1;

						gridbag.setConstraints(positions, c);
//        			c1.add(positions, c);
						fullGraphPanel.add(positions);
//              c1.add(positions, "0 60 100 80");
//              c.y += c.h;

						positions.drawzero = false;
						positions.drawgrid = false;
						positions.frame = false;
						positions.borderTop = 1;
						positions.borderBottom = 1;
/*
**      Load a file containing Marker definitions
*/

						if (marker != null)
							positions.setMarkers(marker);

						double[] datapeak = new double[2 * numberofRefl];

						int dimension = numberradiation;

						if (numberphases > dimension)
							dimension = numberphases;
						datap = new PeakSet[dimension];

						for (int ijn = 0; ijn < numberradiation; ijn++) {
							int j;
							double wave = dataset.getInstrument().getRadiationType().getRadiationWavelength(ijn);
							for (int i = j = 0; i < numberofPeaks; i++, j += 2) {
								Peak peak = peaklist.elementAt(i);
								Phase tmpphase = peak.getPhase();
								int reflIndex = peak.getOrderPosition();
								int phaseindex = 0;
								for (int ij = 0; ij < numberphases; ij++)
									if (tmpphase == phaselist[ij])
										phaseindex = ij;

								double dspace = 0;
								if (ijn < dataset.getActiveDataFile(0).getPositions(tmpphase)[0][0].length) {
									double pos = dataset.getActiveDataFile(0).getPositions(tmpphase)[reflIndex][0][ijn];
									datapeak[j] = dataset.getActiveDataFile(0).convertXDataForPlot(pos, wave, mode);
									datapeak[j + 1] = (double) (phaseindex + 1);
									dspace = dataset.getActiveDataFile(0).convertXToDspace(pos, wave);
								}
								if (ijn == 0) {
									PeakInfo peakInfo = new PeakInfo();
									Reflection refl = peak.getReflex();
									peakInfo.info = tmpphase.getPhaseName() + " (" + Integer.toString(refl.getH()) + " " +
											Integer.toString(refl.getK()) + " " +
											Integer.toString(refl.getL()) + ") " + Double.toString(dspace);
									peakInfo.coordinate = datapeak[j];
//		              System.out.println("Adding: " + peakInfo.coordinate + " " + peakInfo.info);
									peaksInfos.add(peakInfo);
								}
							}
							datap[ijn] = positions.loadPeakSet(datapeak, numberofRefl);
							datap[ijn].linestyle = 0;
							datap[ijn].marker = 9;
							double mscale = 2.0 - ijn;
							if (mscale <= 0.01)
								mscale = 0.5;
							datap[ijn].markerscale = mscale;

//        		 		int index = 0;
//        		 		if (ijn > 0)
//        		 			index = 1;
//              if (blackAndWhite)
							datap[ijn].markercolor = Color.black;
//              else
//                datap[ijn].markercolor = getPastelColor(ijn);

						}

						datapeak = new double[2];
						for (int ijn = numberradiation; ijn < numberphases; ijn++) {
							datapeak[0] = 0.0;
							datapeak[1] = 1.0;
							datap[ijn] = positions.loadPeakSet(datapeak, 1);
							datap[ijn].linestyle = 0;
							datap[ijn].marker = 9;
							datap[ijn].markerscale = 0.1;
							if (blackAndWhite)
								datap[ijn].markercolor = Color.black;
							else
								datap[ijn].markercolor = getPastelColor(ijn);
						}
						for (int ij = 0; ij < numberphases; ij++) {
							double ypos = (double) (ij + 1);
							datap[ij].legend(1, ypos, phaselist[ij].toXRDcatString());
							datap[ij].legendFont(new Font(labelFont, Font.PLAIN, PhasesFontScale));
							if (blackAndWhite)
								datap[ij].legendColor(Color.black);
							else
								datap[ij].legendColor(getPastelColor(ij + 2));
						}

//          System.out.println("Data loaded");

/*
**      Attach data sets to the Xaxis
*/
//          System.out.println("Attaching X-axis....");

						Axis xaxisp = positions.createXAxis();
						xaxisp.drawLine = false;
						xaxisp.drawLabel = false;
						xaxisp.drawTitle = false;
						xaxisp.referenceAxis = xaxis;
						xaxisp.axiscolor = Color.black;

						for (int ijn = 0; ijn < dimension; ijn++)
							xaxisp.attachDataSet(datap[ijn]);

/*
**      Attach the first data set to the Left Axis
*/
//          System.out.println("Attaching Y-axis....");

						yaxisp = positions.createYAxis();
						yaxisp.drawLine = false;
						yaxisp.drawLabel = false;
						yaxisp.drawTitle = false;
						yaxisp.referenceAxisWidth = yaxis;
						for (int ijn = 0; ijn < dimension; ijn++)
							yaxisp.attachDataSet(datap[ijn]);
						yaxisp.setTitleText(" ");
						yaxisp.setTitleFont(new Font(axisFont, Font.BOLD, YaxisTitleFontScale));
						yaxisp.setLabelFont(new Font(labelFont, Font.PLAIN, YaxisLabelFontScale));
						yaxisp.setTitleColor(getBackground());
						yaxisp.axiscolor = Color.black;
						yaxisp.minimum = 0.5;
						yaxisp.maximum = ((double) numberphases) + 0.5;

						graph.attachPeaksInfos(peaksInfos);
					}
				} else {
					int numberofRefl = peaksList[0].length;

					if (numberofRefl > 0) {

						positions = new G2Dint(new g2DPmouse(), null);

						graph.addComponent(positions);

						c.weighty = 0.04 * 2;
						c.gridwidth = GridBagConstraints.REMAINDER;
//              c.h = numberphases + 1;

						gridbag.setConstraints(positions, c);
//        			c1.add(positions, c);
						fullGraphPanel.add(positions);
//              c1.add(positions, "0 60 100 80");
//              c.y += c.h;

						positions.drawzero = false;
						positions.drawgrid = false;
						positions.frame = false;
						positions.borderTop = 1;
						positions.borderBottom = 1;
/*
**      Load a file containing Marker definitions
*/

						if (marker != null)
							positions.setMarkers(marker);

						double[] datapeak = new double[2 * numberofRefl];

						int dimension = 1;
						datap = new PeakSet[dimension];

						double wave = dataset.getInstrument().getRadiationType().getRadiationWavelength(0);
						for (int i = 0; i < numberofRefl; i++) {
							datapeak[2 * i] = dataset.getActiveDataFile(0).convertXDataForPlot(peaksList[0][i], wave, mode);

							datapeak[2 * i + 1] = 1.0;
						}
						datap[0] = positions.loadPeakSet(datapeak, numberofRefl);
						datap[0].linestyle = 0;
						datap[0].marker = 9;
						double mscale = 2.0;
						datap[0].markerscale = mscale;
//            if (blackAndWhite)
						datap[0].markercolor = Color.black;
//            else
//              datap[0].markercolor = getPastelColor(0);

						double ypos = 1.0;
						datap[0].legend(1, ypos, "Peaks");
						datap[0].legendFont(new Font(labelFont, Font.PLAIN, PhasesFontScale));
//            if (blackAndWhite)
						datap[0].legendColor(Color.black);
//            else
//              datap[0].legendColor(getPastelColor(0));
/*
**      Attach data sets to the Xaxis
*/
//          System.out.println("Attaching X-axis....");

						Axis xaxisp = positions.createXAxis();
						xaxisp.drawLine = false;
						xaxisp.drawLabel = false;
						xaxisp.drawTitle = false;
						xaxisp.referenceAxis = xaxis;
						xaxisp.axiscolor = Color.black;

						for (int ijn = 0; ijn < dimension; ijn++)
							xaxisp.attachDataSet(datap[ijn]);

/*
**      Attach the first data set to the Left Axis
*/
//          System.out.println("Attaching Y-axis....");

						yaxisp = positions.createYAxis();
						yaxisp.drawLine = false;
						yaxisp.drawLabel = false;
						yaxisp.drawTitle = false;
						yaxisp.axiscolor = Color.black;
						yaxisp.referenceAxisWidth = yaxis;
						for (int ijn = 0; ijn < dimension; ijn++)
							yaxisp.attachDataSet(datap[ijn]);
						yaxisp.setTitleText(" ");
						yaxisp.setTitleFont(new Font(axisFont, Font.BOLD, YaxisTitleFontScale));
						yaxisp.setLabelFont(new Font(labelFont, Font.PLAIN, YaxisLabelFontScale));
						yaxisp.setTitleColor(getBackground());
						yaxisp.minimum = 0.5;
						yaxisp.maximum = 1.5;

					}
				}
/*
**      prepare the residuals box
*/
			}

			if (plotResiduals && datafit != null) {

				residuals = new G2Dint();
				residuals.setDataBackground(Color.white);

				graph.addComponent(residuals);

				c.weighty = 0.3;
				if (peaksLocated)
					c.weighty = 0.7;
//            c.h = 5;
				c.gridwidth = GridBagConstraints.REMAINDER;

				gridbag.setConstraints(residuals, c);
				fullGraphPanel.add(residuals);
//        		c1.add(residuals, c);
//            c1.add(residuals, "0 80 100 100");

				residuals.drawzero = true;
				residuals.drawgrid = false;
				residuals.borderTop = 0;
				residuals.borderBottom = 10;
/*
**      Load a file containing Marker definitions
*/

				if (marker != null)
					residuals.setMarkers(marker);

				int j;
				for (int i = j = 0; i < np; i++, j += 2) {
					dataResidual[j + 1] = datafit[j + 1] - data[j + 1];
					dataResidual[j] = data[j];
				}

				datar = residuals.loadDataSet(dataResidual, np);

/*
**      Attach data sets to the Xaxis
*/
//          System.out.println("Attaching X-axis....");

				xaxisr = residuals.createAxis(Axis.BOTTOM);
				xaxisr.referenceAxis = xaxis;
				xaxisr.axiscolor = Color.black;
				xaxisr.attachDataSet(datar);
				xaxisr.setTitleText(dataset.getActiveDataFile(0).getAxisXLegend());
				xaxisr.setTitleFont(new Font(axisFont, Font.BOLD, XaxisTitleFontScale));
				xaxisr.setLabelFont(new Font(labelFont, Font.PLAIN, XaxisLabelFontScale));
				xaxisr.setTitleColor(XaxisTitleColor);
/*
**      Attach the first data set to the Left Axis
*/
//          System.out.println("Attaching Y-axis....");

				yaxisr = residuals.createAxis(Axis.LEFT);
				yaxisr.drawLine = false;
				yaxisr.drawLabel = false;
				yaxisr.drawTitle = false;
				yaxisr.axiscolor = Color.black;
				yaxisr.referenceAxisWidth = yaxis;
				if (!peaksLocated) {
					yaxisr.referenceAxisScale = yaxis;
				}
				yaxisr.attachDataSet(datar);
				yaxisr.setTitleText(" ");
				yaxisr.setTitleFont(new Font(axisFont, Font.BOLD, YaxisTitleFontScale));
				yaxisr.setLabelFont(new Font(labelFont, Font.PLAIN, YaxisLabelFontScale));
				yaxisr.setTitleColor(getBackground());
				yaxisr.axiscolor = Color.black;

			} else {
				xaxis.setTitleText(dataset.getActiveDataFile(0).getAxisXLegend());
				xaxis.setTitleFont(new Font(axisFont, Font.BOLD, XaxisTitleFontScale));
				xaxis.setLabelFont(new Font(labelFont, Font.PLAIN, XaxisLabelFontScale));
				xaxis.setTitleColor(XaxisTitleColor);
				xaxis.axiscolor = Color.black;
				graph.borderBottom = 10;
			}
		} else {
//			graph.finishedloading();
			graph = null;
			String[] labels = new String[2];
			labels[0] = "Spectrum not loaded";
			labels[1] = "check the datafiles!";
			fullGraphPanel = new NoDatafileCanvas(labels);
			return fullGraphPanel;
		}
		graph.finishedloading();

		if (keepMaxima) {
			graph.setUserLimits(xMin, xMax, IntensityMin, IntensityMax);
//      System.out.println("Keeping maxima: " + xMin + " " + xMax + " " + IntensityMin + " " + IntensityMax);
		} else {
			xMin = 0.0;
			xMax = 0.0;
			IntensityMin = 0.0;
			IntensityMax = 0.0;
		}

		return fullGraphPanel;
	}

	public CopyPrintPanel createGraph(DiffrDataFile[] afile, double[][] peaks,
                                    double[] derivative2, boolean keepMaxima) {
    double datafit[] = null;
    double dataphase[];
	  CopyPrintPanel fullGraphPanel = null;

	  if (afile == null) {
      return new NoDatafileCanvas();
    }

		int mode = PlotDataFile.checkScaleModeX();
		PlotDataFile.checkCalibrateIntensity();
		PlotDataFile.checkBackgroundSubtraction();
		XRayDataSqLite.checkMinimumEnergy();

//    if (peaks != null)
    peaksList = peaks;
//    if (derivative2 != null)
    secondDerivative = derivative2;
//    if (afile != null)
    boolean peaksLocated = (peaksList != null);

    boolean markExcludedRegion = MaudPreferences.getBoolean("excludedRegion.setZeroForPlot", true);

    if (datafile[0] != null) {
	    setTitle(datafile[0].getTitle());
	    FilePar filepar = datafile[0].getFilePar();

	    int numberphases = filepar.getActiveSample().phasesNumber();
//      if (!plotPeaks)
//        numberphases = 0;
	    Phase[] phaselist = new Phase[numberphases];
	    for (int i = 0; i < numberphases; i++)
		    phaselist[i] = filepar.getActiveSample().getPhase(i);

	    int i;
	    int j;

	    int ylength = datafile.length;
	    int startingIndex = datafile[0].startingindex;
	    int finalIndex = datafile[0].finalindex;
	    double xmin = 1.0E10, xmax = 0.0;
	    if (datafile[0].increasingX()) {
		    if (xmin > datafile[0].getXDataForPlot(datafile[0].startingindex))
			    xmin = datafile[0].getXDataForPlot(datafile[0].startingindex);
		    if (xmax < datafile[0].getXDataForPlot(datafile[0].finalindex - 1))
			    xmax = datafile[0].getXDataForPlot(datafile[0].finalindex - 1);
		    if (xmin > datafile[0].getXDataForPlot(datafile[0].finalindex - 1))
			    xmin = datafile[0].getXDataForPlot(datafile[0].finalindex - 1);
		    if (xmax < datafile[0].getXDataForPlot(datafile[0].startingindex))
			    xmax = datafile[0].getXDataForPlot(datafile[0].startingindex);
		    for (int is1 = 1; is1 < ylength; is1++) {
			    if (startingIndex > datafile[is1].startingindex) {
				    startingIndex = datafile[is1].startingindex;
			    }
			    if (finalIndex < datafile[is1].finalindex) {
				    finalIndex = datafile[is1].finalindex;
			    }
			    if (xmin > datafile[is1].getXDataForPlot(datafile[is1].startingindex))
				    xmin = datafile[is1].getXDataForPlot(datafile[is1].startingindex);
			    if (xmax < datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1))
				    xmax = datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1);
			    if (xmin > datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1))
				    xmin = datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1);
			    if (xmax < datafile[is1].getXDataForPlot(datafile[is1].startingindex))
				    xmax = datafile[is1].getXDataForPlot(datafile[is1].startingindex);
		    }
	    } else {
		    xmin = 1.0E10;
		    xmax = 0.0;
		    if (xmin > datafile[0].getXDataForPlot(datafile[0].startingindex))
			    xmin = datafile[0].getXDataForPlot(datafile[0].startingindex);
		    if (xmax < datafile[0].getXDataForPlot(datafile[0].finalindex - 1))
			    xmax = datafile[0].getXDataForPlot(datafile[0].finalindex - 1);
		    if (xmin > datafile[0].getXDataForPlot(datafile[0].finalindex - 1))
			    xmin = datafile[0].getXDataForPlot(datafile[0].finalindex - 1);
		    if (xmax < datafile[0].getXDataForPlot(datafile[0].startingindex))
			    xmax = datafile[0].getXDataForPlot(datafile[0].startingindex);
		    for (int is1 = 1; is1 < ylength; is1++) {
			    if (startingIndex > datafile[is1].startingindex) {
				    startingIndex = datafile[is1].startingindex;
			    }
			    if (finalIndex < datafile[is1].finalindex) {
				    finalIndex = datafile[is1].finalindex;
			    }
			    if (xmin > datafile[is1].getXDataForPlot(datafile[is1].startingindex))
				    xmin = datafile[is1].getXDataForPlot(datafile[is1].startingindex);
			    if (xmax < datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1))
				    xmax = datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1);
			    if (xmin > datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1))
				    xmin = datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1);
			    if (xmax < datafile[is1].getXDataForPlot(datafile[is1].startingindex))
				    xmax = datafile[is1].getXDataForPlot(datafile[is1].startingindex);
		    }
	    }
	    int xlength = finalIndex - startingIndex;
	    double stepX = (xmax - xmin) / (xlength - 1);
	    np = xlength;

	    if (np > 0) {
		    double data[] = new double[2 * np];
		    if (datafile[0].hasfit() || peaksLocated) {
			    datafit = new double[2 * np];
		    }
		    mode = PlotDataFile.checkScaleModeX();
		    for (int is1 = 0; is1 < xlength; is1++) {
			    int is2 = is1 * 2;
			    data[is2] = xmin + is1 * stepX;
			    int total = 0;
			    int totalFit = 0;
			    for (int sn = 0; sn < ylength; sn++) {
				    double xstartmin = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
				    double xendmax = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
				    if (xendmax < datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode))
					    xendmax = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
				    if (xstartmin > datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode))
					    xstartmin = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
				    if (data[is2] >= xstartmin && data[is2] <= xendmax) {
				    	double value = datafile[sn].getInterpolatedYSqrtIntensity(data[is2], 2, mode);
				    	if (!Double.isNaN(value)) {
					      data[is2 + 1] += value;
					      total++;
				      }
				    }
			    }
			    if (total > 0)
				    data[is2 + 1] /= total;

			    if (datafile[0].hasfit()) {
				    datafit[is2] = data[is2];

				    for (int sn = 0; sn < ylength; sn++) {
					    double xstartmin = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
					    double xendmax = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
					    if (xendmax < datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode))
						    xendmax = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
					    if (xstartmin > datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode))
						    xstartmin = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
					    if (datafit[is2] >= xstartmin && datafit[is2] <= xendmax) {
						    double value = datafile[sn].getInterpolatedFitSqrtIntensity(datafit[is2], 2, mode);
						    if (!Double.isNaN(value)) {
							    datafit[is2 + 1] += value;
							    totalFit++;
						    }
					    }
				    }
				    if (totalFit > 0)
					    datafit[is2 + 1] /= totalFit;
			    }
		    }


	    setTitle(datafile[0].getTitle());
	    fullGraphPanel = new CopyPrintPanelNoBkg();
//      fullGraphPanel.setBackground(Color.white);

//					KappaLayout gridbag = new KappaLayout();
	    GridBagLayout gridbag = new GridBagLayout();
	    GridBagConstraints c = new GridBagConstraints();

	    fullGraphPanel.setLayout(gridbag);

/*
**      Create the Graph instance and modify the default behaviour
*/
	    graph = new plotPanelG2Dint();

	    graph.startedloading();

	    graph.setDataBackground(Color.white);
	    graph.setInfoDefaultLength(MaudPreferences.getInteger("plot.numberOfInfoPeaks", 10));

	    fullGraphPanel.add(graph);
	    c.fill = GridBagConstraints.BOTH;
	    c.weighty = 1.0;
	    c.weightx = 1.0;
	    c.gridwidth = GridBagConstraints.REMAINDER;

	    gridbag.setConstraints(graph, c);

	    graph.drawzero = false;
	    graph.drawgrid = false;
	    graph.borderTop = 30;
	    graph.borderBottom = 1;

/*
**      Load a file containing Marker definitions
*/
	    Markers marker = null;
	    try {
		    marker = new Markers(Constants.documentsDirectory + "marker.txt");
	    } catch (IOException ioe) {
		    ioe.printStackTrace();
	    }

	    if (marker != null)
		    graph.setMarkers(marker);

		    data1 = graph.loadDataSet(data, np);
		    if (markerNumber < 0 && (datafile[0].hasfit() || peaksLocated))
			    markerNumber = defaultMarker;
		    if (markerNumber < 0) {
			    data1.linestyle = 1;
		    } else {
			    data1.linestyle = 0;
			    data1.marker = markerNumber;
			    data1.markerscale = markerScale;
			    data1.markercolor = markerColor;
		    }
//        	data1.legend(200,100,datafile[0].getAxisXLegend());
//        	data1.legendColor(Color.black);

		    if (datafile[0].hasfit() || peaksLocated) {
			    dataFit = graph.loadDataSet(datafit, np);
			    if (blackAndWhite) {
				    dataFit.linecolor = Color.black;
			    } else {
				    dataFit.linecolor = fitColor;
			    }
		    }

		    DataSet[] phaseData = new DataSet[numberphases];

		    if (datafile[0].hasfit()) {
			    for (int s = 0; s < numberphases; s++) {
				    if (phaselist[s].plotFit()) {
					    dataphase = new double[2 * np];
					    for (i = j = 0; i < np; i++, j += 2) {
						    int totalFit = 0;
						    for (int sn = 0; sn < ylength; sn++) {
							    double xstartmin = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
							    double xendmax = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
							    if (xendmax < datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode))
								    xendmax = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
							    if (xstartmin > datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode))
								    xstartmin = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
//            if (is1 == 0)
//              syaxis[ylength + 1 + sn] = ylength + 1 + sn;
							    if (datafit[j] >= xstartmin && datafit[j] <= xendmax) {
							    	double value = datafile[sn].getInterpolatedFitSqrtIntensity(datafit[j], 2, mode, s);
							    	if (!Double.isNaN(value)) {
								      dataphase[j + 1] += value;
								      totalFit++;
							      }
							    }
						    }
						    if (totalFit > 0)
							    dataphase[j + 1] /= totalFit;
						    dataphase[j] = datafit[j];
//            System.out.println(datafit[j] + " " + datafit[j + 1]);

					    }
					    phaseData[s] = graph.loadDataSet(dataphase, np);
					    phaseData[s].linestyle = 1;
					    if (blackAndWhite)
						    phaseData[s].linecolor = Color.black;
					    else
						    phaseData[s].linecolor = getPastelColor(s + 2);
				    }
			    }

		    }

		    DataSet bkgData = null;

		    if (datafile[0].hasfit() && plotBackground) {
			    dataphase = new double[2 * np];
			    for (i = j = 0; i < np; i++, j += 2) {
				    int totalFit = 0;
				    for (int sn = 0; sn < ylength; sn++) {
					    double xstartmin = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
					    double xendmax = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
					    if (xendmax < datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode))
						    xendmax = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
					    if (xstartmin > datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode))
						    xstartmin = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
//            if (is1 == 0)
//              syaxis[ylength + 1 + sn] = ylength + 1 + sn;
					    if (datafit[j] >= xstartmin && datafit[j] <= xendmax) {
					    	double value = datafile[sn].getInterpolatedBkgFitSqrtIntensity(datafit[j], 2, mode);
					    	if (!Double.isNaN(value)) {
						      dataphase[j + 1] += value;
						      totalFit++;
					      }
					    }
				    }
				    if (totalFit > 0)
					    dataphase[j + 1] /= totalFit;
				    dataphase[j] = datafit[j];
//            System.out.println(datafit[j] + " " + datafit[j + 1]);

			    }
			    bkgData = graph.loadDataSet(dataphase, np);
			    bkgData.linestyle = 1;
			    if (blackAndWhite)
				    bkgData.linecolor = Color.black;
			    else
				    bkgData.linecolor = getPastelColor(1);

		    }

/*
**      Attach data sets to the Xaxis
*/
//          System.out.println("Attaching X-axis....");

		    xaxis = graph.createXAxis();
		    xaxis.axiscolor = Color.black;
		    if ((plotResiduals && datafile[0].hasfit()) || peaksLocated) {
			    xaxis.drawLabel = false;
			    xaxis.drawTitle = false;
		    }
		    xaxis.attachDataSet(data1);
		    if (datafile[0].hasfit() || peaksLocated) {
			    xaxis.attachDataSet(dataFit);
		    }
		    if (datafile[0].hasfit()) {
			    for (int s = 0; s < numberphases; s++)
				    if (phaselist[s].plotFit())
					    xaxis.attachDataSet(phaseData[s]);
			    if (plotBackground)
				    xaxis.attachDataSet(bkgData);
		    }

/*
**      Attach the first data set to the Left Axis
*/
//          System.out.println("Attaching Y-axis....");

		    yaxis = graph.createYAxis();
		    yaxis.attachDataSet(data1);
		    if (datafile[0].hasfit() || peaksLocated) {
			    yaxis.attachDataSet(dataFit);
		    }
		    if (datafile[0].hasfit()) {
			    for (int s = 0; s < numberphases; s++)
				    if (phaselist[s].plotFit())
					    yaxis.attachDataSet(phaseData[s]);
			    if (plotBackground)
				    yaxis.attachDataSet(bkgData);
		    }


		    yaxis.setTitleText(DiffrDataFile.getAxisYLegend());
		    yaxis.setTitleFont(new Font(axisFont, Font.BOLD, YaxisTitleFontScale));
		    yaxis.setLabelFont(new Font(labelFont, Font.PLAIN, YaxisLabelFontScale));
		    yaxis.setTitleColor(YaxisTitleColor);
		    yaxis.axiscolor = Color.black;

		    DataFileSet adataset = datafile[0].getDataFileSet();
		    int numberofPeaks = adataset.getNumberofPeaks();
		    if (adataset.getInstrument() instanceof XRFInstrument) {
			    numberofPeaks = 0;
			    double energyInKeV = Constants.ENERGY_LAMBDA / adataset.getInstrument().getRadiationType().
					    getShortestWavelengthForFluorescence() * 0.001;
			    if (peaksInfoXRF == null || energyUsedForPeakInfoXRF < energyInKeV) {
				    energyUsedForPeakInfoXRF = energyInKeV;
				    peaksInfoXRF = new Vector<PeakInfo>(10000, 1000);
				    double threasholdXRFLines = MaudPreferences.getDouble("fluorescenceLinesPeaksInfo.thresholdOnMinors", 0.001);
				    for (int atomNumber = 0; atomNumber < XRayDataSqLite.atomsNumber; atomNumber++) {
					    String atomLabel = AtomInfo.atomLabels[atomNumber];
					    Vector<FluorescenceLine> linesForAtom = XRayDataSqLite.getFluorescenceLinesFor(
							    atomNumber + 1, energyInKeV);
					    for (int lindex = 0; lindex < linesForAtom.size(); lindex++) {
						    FluorescenceLine line = linesForAtom.get(lindex);
						    if (line.getIntensity() > threasholdXRFLines) {
							    PeakInfo peakInfo = new PeakInfo();
							    peakInfo.info = atomLabel + " " + line.toString();
							    peakInfo.coordinate = line.getEnergy() * 1000;
							    peaksInfoXRF.add(peakInfo);
						    }/* else {
						    System.out.println("Not added: " + atomLabel + " " + line.toString() + " " + line.getEnergy() + " " + line.getIntensity());
					    }*/
					    }
				    }
			    }
			    graph.attachPeaksInfos(peaksInfoXRF);
			    graph.plot_type = graph.FLUORESCENCE_PLOT;
		    }

		    if (((plotPeaks && numberofPeaks > 0) && datafile[0].hasfit()) || peaksLocated) {

/*
**      prepare the positions box
*/
			    if (!peaksLocated) {
				    Vector<Peak> peaklist = adataset.getPeakList();

				    int numberradiation = adataset.getInstrument().getRadiationType().getLinesCountForPlot();

				    if (numberradiation == 0)
					    numberradiation = 1;
				    int numberofRefl = numberofPeaks;

				    if (numberofRefl > 0) {

					    Vector<PeakInfo> peaksInfos = new Vector<PeakInfo>(numberofPeaks / numberradiation);

					    positions = new G2Dint(new plotPhaseMouseAdapter(), null);

					    graph.addComponent(positions);

					    c.weighty = 0.04 * (numberphases + 1);
					    c.gridwidth = GridBagConstraints.REMAINDER;
//              c.h = numberphases + 1;

					    gridbag.setConstraints(positions, c);
//        			c1.add(positions, c);
					    fullGraphPanel.add(positions);
//              c1.add(positions, "0 60 100 80");
//              c.y += c.h;

					    positions.drawzero = false;
					    positions.drawgrid = false;
					    positions.frame = false;
					    positions.borderTop = 1;
					    positions.borderBottom = 1;
/*
**      Load a file containing Marker definitions
*/

					    if (marker != null)
						    positions.setMarkers(marker);

					    double[] datapeak = new double[2 * numberofRefl];

					    int dimension = numberradiation;

					    if (numberphases > dimension)
						    dimension = numberphases;
					    datap = new PeakSet[dimension];

					    for (int ijn = 0; ijn < numberradiation; ijn++) {
						    double wave = adataset.getInstrument().getRadiationType().getRadiationWavelength(ijn);
						    for (i = j = 0; i < numberofPeaks; i++, j += 2) {
							    Peak peak = peaklist.elementAt(i);
							    Phase tmpphase = peak.getPhase();
							    int reflIndex = peak.getOrderPosition();
							    int phaseindex = 0;
							    for (int ij = 0; ij < numberphases; ij++)
								    if (tmpphase == phaselist[ij])
									    phaseindex = ij;

							    double dspace = 0;
							    if (ijn < datafile[0].getPositions(tmpphase)[0][0].length) {
								    double pos = datafile[0].getPositions(tmpphase)[reflIndex][0][ijn];
								    datapeak[j] = datafile[0].convertXDataForPlot(pos, wave, mode);
								    datapeak[j + 1] = (double) (phaseindex + 1);
								    dspace = datafile[0].convertXToDspace(pos, wave);
							    }
							    if (ijn == 0) {
								    PeakInfo peakInfo = new PeakInfo();
								    Reflection refl = peak.getReflex();
								    peakInfo.info = tmpphase.getPhaseName() + " (" + Integer.toString(refl.getH()) + " " +
										    Integer.toString(refl.getK()) + " " +
										    Integer.toString(refl.getL()) + ") " + Double.toString(dspace);
								    peakInfo.coordinate = datapeak[j];
//		              System.out.println("Adding: " + peakInfo.coordinate + " " + peakInfo.info);
								    peaksInfos.add(peakInfo);
							    }
						    }
						    datap[ijn] = positions.loadPeakSet(datapeak, numberofRefl);
						    datap[ijn].linestyle = 0;
						    datap[ijn].marker = 9;
						    double mscale = 2.0 - ijn;
						    if (mscale <= 0.01)
							    mscale = 0.5;
						    datap[ijn].markerscale = mscale;

//        		 		int index = 0;
//        		 		if (ijn > 0)
//        		 			index = 1;
//              if (blackAndWhite)
						    datap[ijn].markercolor = Color.black;
//              else
//                datap[ijn].markercolor = getPastelColor(ijn);

					    }

					    datapeak = new double[2];
					    for (int ijn = numberradiation; ijn < numberphases; ijn++) {
						    datapeak[0] = 0.0;
						    datapeak[1] = 1.0;
						    datap[ijn] = positions.loadPeakSet(datapeak, 1);
						    datap[ijn].linestyle = 0;
						    datap[ijn].marker = 9;
						    datap[ijn].markerscale = 0.1;
						    if (blackAndWhite)
							    datap[ijn].markercolor = Color.black;
						    else
							    datap[ijn].markercolor = getPastelColor(ijn);
					    }
					    for (int ij = 0; ij < numberphases; ij++) {
						    double ypos = (double) (ij + 1);
						    datap[ij].legend(1, ypos, phaselist[ij].toXRDcatString());
						    datap[ij].legendFont(new Font(labelFont, Font.PLAIN, PhasesFontScale));
						    if (blackAndWhite)
							    datap[ij].legendColor(Color.black);
						    else
							    datap[ij].legendColor(getPastelColor(ij + 2));
					    }

//          System.out.println("Data loaded");

/*
**      Attach data sets to the Xaxis
*/
//          System.out.println("Attaching X-axis....");

					    Axis xaxisp = positions.createXAxis();
					    xaxisp.drawLine = false;
					    xaxisp.drawLabel = false;
					    xaxisp.drawTitle = false;
					    xaxisp.referenceAxis = xaxis;
					    xaxisp.axiscolor = Color.black;

					    for (int ijn = 0; ijn < dimension; ijn++)
						    xaxisp.attachDataSet(datap[ijn]);

/*
**      Attach the first data set to the Left Axis
*/
//          System.out.println("Attaching Y-axis....");

					    yaxisp = positions.createYAxis();
					    yaxisp.drawLine = false;
					    yaxisp.drawLabel = false;
					    yaxisp.drawTitle = false;
					    yaxisp.referenceAxisWidth = yaxis;
					    for (int ijn = 0; ijn < dimension; ijn++)
						    yaxisp.attachDataSet(datap[ijn]);
					    yaxisp.setTitleText(" ");
					    yaxisp.setTitleFont(new Font(axisFont, Font.BOLD, YaxisTitleFontScale));
					    yaxisp.setLabelFont(new Font(labelFont, Font.PLAIN, YaxisLabelFontScale));
					    yaxisp.setTitleColor(getBackground());
					    yaxisp.axiscolor = Color.black;
					    yaxisp.minimum = 0.5;
					    yaxisp.maximum = ((double) numberphases) + 0.5;

					    graph.attachPeaksInfos(peaksInfos);
				    }
			    } else {
				    int numberofRefl = peaksList[0].length;

				    if (numberofRefl > 0) {

					    positions = new G2Dint(new g2DPmouse(), null);

					    graph.addComponent(positions);

					    c.weighty = 0.04 * 2;
					    c.gridwidth = GridBagConstraints.REMAINDER;
//              c.h = numberphases + 1;

					    gridbag.setConstraints(positions, c);
//        			c1.add(positions, c);
					    fullGraphPanel.add(positions);
//              c1.add(positions, "0 60 100 80");
//              c.y += c.h;

					    positions.drawzero = false;
					    positions.drawgrid = false;
					    positions.frame = false;
					    positions.borderTop = 1;
					    positions.borderBottom = 1;
/*
**      Load a file containing Marker definitions
*/

					    if (marker != null)
						    positions.setMarkers(marker);

					    double[] datapeak = new double[2 * numberofRefl];

					    int dimension = 1;
					    datap = new PeakSet[dimension];

					    double wave = adataset.getInstrument().getRadiationType().getRadiationWavelength(0);
					    for (i = 0; i < numberofRefl; i++) {
						    datapeak[2 * i] = datafile[0].convertXDataForPlot(peaksList[0][i], wave, mode);

						    datapeak[2 * i + 1] = 1.0;
					    }
					    datap[0] = positions.loadPeakSet(datapeak, numberofRefl);
					    datap[0].linestyle = 0;
					    datap[0].marker = 9;
					    double mscale = 2.0;
					    datap[0].markerscale = mscale;
//            if (blackAndWhite)
					    datap[0].markercolor = Color.black;
//            else
//              datap[0].markercolor = getPastelColor(0);

					    double ypos = 1.0;
					    datap[0].legend(1, ypos, "Peaks");
					    datap[0].legendFont(new Font(labelFont, Font.PLAIN, PhasesFontScale));
//            if (blackAndWhite)
					    datap[0].legendColor(Color.black);
//            else
//              datap[0].legendColor(getPastelColor(0));
/*
**      Attach data sets to the Xaxis
*/
//          System.out.println("Attaching X-axis....");

					    Axis xaxisp = positions.createXAxis();
					    xaxisp.drawLine = false;
					    xaxisp.drawLabel = false;
					    xaxisp.drawTitle = false;
					    xaxisp.referenceAxis = xaxis;
					    xaxisp.axiscolor = Color.black;

					    for (int ijn = 0; ijn < dimension; ijn++)
						    xaxisp.attachDataSet(datap[ijn]);

/*
**      Attach the first data set to the Left Axis
*/
//          System.out.println("Attaching Y-axis....");

					    yaxisp = positions.createYAxis();
					    yaxisp.drawLine = false;
					    yaxisp.drawLabel = false;
					    yaxisp.drawTitle = false;
					    yaxisp.axiscolor = Color.black;
					    yaxisp.referenceAxisWidth = yaxis;
					    for (int ijn = 0; ijn < dimension; ijn++)
						    yaxisp.attachDataSet(datap[ijn]);
					    yaxisp.setTitleText(" ");
					    yaxisp.setTitleFont(new Font(axisFont, Font.BOLD, YaxisTitleFontScale));
					    yaxisp.setLabelFont(new Font(labelFont, Font.PLAIN, YaxisLabelFontScale));
					    yaxisp.setTitleColor(getBackground());
					    yaxisp.minimum = 0.5;
					    yaxisp.maximum = 1.5;

				    }
			    }
/*
**      prepare the residuals box
*/
		    }

		    if ((plotResiduals && datafile[0].hasfit()) || peaksLocated) {

			    residuals = new G2Dint();
			    residuals.setDataBackground(Color.white);

			    graph.addComponent(residuals);

			    c.weighty = 0.3;
			    if (peaksLocated)
				    c.weighty = 0.7;
//            c.h = 5;
			    c.gridwidth = GridBagConstraints.REMAINDER;

			    gridbag.setConstraints(residuals, c);
			    fullGraphPanel.add(residuals);
//        		c1.add(residuals, c);
//            c1.add(residuals, "0 80 100 100");

			    residuals.drawzero = false;
			    residuals.drawgrid = false;
			    residuals.borderTop = 0;
			    residuals.borderBottom = 10;
/*
**      Load a file containing Marker definitions
*/

			    if (marker != null)
				    residuals.setMarkers(marker);

			    if (peaksLocated)
				    for (i = j = 0; i < np; i++, j += 2) {
					    data[j + 1] = MoreMath.sign(derivative2[i]) *
							    Math.sqrt(Math.sqrt(Math.abs(derivative2[i])));
				    }
			    else
				    for (i = j = 0; i < np; i++, j += 2) {
					    data[j + 1] = datafit[j + 1] - data[j + 1];
				    }

			    datar = residuals.loadDataSet(data, np);

/*
**      Attach data sets to the Xaxis
*/
//          System.out.println("Attaching X-axis....");

			    xaxisr = residuals.createAxis(Axis.BOTTOM);
			    xaxisr.referenceAxis = xaxis;
			    xaxisr.axiscolor = Color.black;
			    xaxisr.attachDataSet(datar);
			    xaxisr.setTitleText(datafile[0].getAxisXLegend());
			    xaxisr.setTitleFont(new Font(axisFont, Font.BOLD, XaxisTitleFontScale));
			    xaxisr.setLabelFont(new Font(labelFont, Font.PLAIN, XaxisLabelFontScale));
			    xaxisr.setTitleColor(XaxisTitleColor);
/*
**      Attach the first data set to the Left Axis
*/
//          System.out.println("Attaching Y-axis....");

			    yaxisr = residuals.createAxis(Axis.LEFT);
			    yaxisr.drawLine = false;
			    yaxisr.drawLabel = false;
			    yaxisr.drawTitle = false;
			    yaxisr.axiscolor = Color.black;
			    yaxisr.referenceAxisWidth = yaxis;
			    if (!peaksLocated) {
				    yaxisr.referenceAxisScale = yaxis;
			    }
			    yaxisr.attachDataSet(datar);
			    yaxisr.setTitleText(" ");
			    yaxisr.setTitleFont(new Font(axisFont, Font.BOLD, YaxisTitleFontScale));
			    yaxisr.setLabelFont(new Font(labelFont, Font.PLAIN, YaxisLabelFontScale));
			    yaxisr.setTitleColor(getBackground());
			    yaxisr.axiscolor = Color.black;

		    } else {
			    xaxis.setTitleText(datafile[0].getAxisXLegend());
			    xaxis.setTitleFont(new Font(axisFont, Font.BOLD, XaxisTitleFontScale));
			    xaxis.setLabelFont(new Font(labelFont, Font.PLAIN, XaxisLabelFontScale));
			    xaxis.setTitleColor(XaxisTitleColor);
			    xaxis.axiscolor = Color.black;
			    graph.borderBottom = 10;
		    }
	    } else {
		    graph.finishedloading();
		    graph = null;
		    String[] labels = new String[1];
		    labels[0] = "No datafile selected!";
		    return new NoDatafileCanvas(labels);
	    }
	    graph.finishedloading();

	    if (keepMaxima) {
		    graph.setUserLimits(xMin, xMax, IntensityMin, IntensityMax);
//      System.out.println("Keeping maxima: " + xMin + " " + xMax + " " + IntensityMin + " " + IntensityMax);
	    } else {
		    xMin = 0.0;
		    xMax = 0.0;
		    IntensityMin = 0.0;
		    IntensityMax = 0.0;
	    }
    } else {
	    graph.finishedloading();
	    graph = null;
	    String[] labels = new String[2];
	    labels[0] = "Spectrum not loaded";
	    labels[1] = "check the datafiles!";
	    fullGraphPanel = new NoDatafileCanvas(labels);
    }

	  return fullGraphPanel;
  }

  public CopyPrintPanel createGraphSingle(DiffrDataFile afile, double[][] peaks,
                                          double[] derivative2, boolean keepMaxima) {
    double datafit[] = null;
    double dataphase[] = null;

    if (afile == null) {
      return new NoDatafileCanvas();
    }

	  int mode = PlotDataFile.checkScaleModeX();
	  PlotDataFile.checkCalibrateIntensity();
	  PlotDataFile.checkBackgroundSubtraction();
	  XRayDataSqLite.checkMinimumEnergy();

//    datafile[0] = afile;
//    if (peaks != null)
    peaksList = peaks;
//    if (derivative2 != null)
    secondDerivative = derivative2;
//    if (afile != null)
	  CopyPrintPanel fullGraphPanel = null;

    boolean peaksLocated = (peaksList != null);

    boolean markExcludedRegion = MaudPreferences.getBoolean("excludedRegion.setZeroForPlot", true);

    if (afile != null) {
      setTitle(afile.getTitle());
      FilePar filepar = afile.getFilePar();

      int numberphases = filepar.getActiveSample().phasesNumber();
//      if (!plotPeaks)
//        numberphases = 0;
      Phase[] phaselist = new Phase[numberphases];
      for (int i = 0; i < numberphases; i++)
        phaselist[i] = filepar.getActiveSample().getPhase(i);

      int i;
      int j;

      fullGraphPanel = new CopyPrintPanelNoBkg();

//      fullGraphPanel.setBackground(Color.white);

//					KappaLayout gridbag = new KappaLayout();
      GridBagLayout gridbag = new GridBagLayout();
      GridBagConstraints c = new GridBagConstraints();
//          KappaLayout.Constraints c = KappaLayout.createConstraint();
//          PercentLayout gridbag = new PercentLayout();

      fullGraphPanel.setLayout(gridbag);

/*
**      Create the Graph instance and modify the default behaviour
*/
      graph = new plotPanelG2Dint();
	    graph.startedloading();
      graph.setDataBackground(Color.white);
	    graph.setInfoDefaultLength(MaudPreferences.getInteger("plot.numberOfInfoPeaks", 10));

/*          c.x = 0;
          c.y = 0;
          c.w = 1;
          c.h = 20;
          c.a = 0;
          c.s = "wh";
          c.p = 0;
        	c1.add(lgraph, c);*/
//          c1.add(lgraph, "0 0 100 60");
//          c.y += c.h;

      fullGraphPanel.add(graph);
      c.fill = GridBagConstraints.BOTH;
      c.weighty = 1.0;
      c.weightx = 1.0;
      c.gridwidth = GridBagConstraints.REMAINDER;

      gridbag.setConstraints(graph, c);

      graph.drawzero = false;
      graph.drawgrid = false;
      graph.borderTop = 30;
      graph.borderBottom = 1;

/*
**      Load a file containing Marker definitions
*/
      Markers marker = null;
      try {
        marker = new Markers(Constants.documentsDirectory + "marker.txt");
      } catch (IOException ioe) {
        ioe.printStackTrace();
      }

      if (marker != null)
        graph.setMarkers(marker);

      int startingIndexG = afile.startingindex;
      int finalIndexG = afile.finalindex;
      np = finalIndexG - startingIndexG;
      if (np <= 0) {
	      graph.finishedloading();
        graph = null;
        String[] labels = new String[2];
        labels[0] = "Spectrum not loaded";
        labels[1] = "check the datafiles!";
        return new NoDatafileCanvas(labels);
      }
      double data[] = new double[2 * np];

//          boolean forceDspace = MaudPreferences.getBoolean(xaxisModePref, xplotMode[0]);
      mode = PlotDataFile.checkScaleModeX();
      for (i = j = 0; i < np; i++, j += 2) {
        int index = i + startingIndexG;
        if (index >= afile.startingindex && index < afile.finalindex) {
          data[j] = afile.getXDataForPlot(index, mode);
//            System.out.println(j + " " + data[j]);
          data[j + 1] = afile.getYSqrtData(index);
        }
      }
//          System.out.println("Data loaded");

      data1 = graph.loadDataSet(data, np);
      if (markerNumber < 0 && (afile.hasfit() || peaksLocated))
        markerNumber = defaultMarker;
      if (markerNumber < 0) {
        data1.linestyle = 1;
      } else {
        data1.linestyle = 0;
        data1.marker = markerNumber;
        data1.markerscale = markerScale;
        data1.markercolor = markerColor;
      }
//        	data1.legend(200,100,datafile[0].getAxisXLegend());
//        	data1.legendColor(Color.black);

      if (afile.hasfit() || peaksLocated) {

        datafit = new double[2 * np];
        for (i = j = 0; i < np; i++, j += 2) {
          int index = i + startingIndexG;
//            System.out.println(data[j]);
          if (index >= afile.startingindex && index < afile.finalindex) {
            if (afile.xInsideRange(afile.getXData(index)) || !markExcludedRegion)
              datafit[j + 1] = afile.getFitSqrtData(index);
            else
              datafit[j + 1] = Double.NaN;
          }
          datafit[j] = data[j];
        }
        dataFit = graph.loadDataSet(datafit, np);
	      if (blackAndWhite)
		      dataFit.linecolor = Color.black;
		    else
	        dataFit.linecolor = fitColor;

      }

      DataSet[] phaseData = new DataSet[numberphases];

      if (afile.hasfit()) {
        for (int s = 0; s < numberphases; s++) {
          if (phaselist[s].plotFit()) {
            dataphase = new double[2 * np];
            for (i = j = 0; i < np; i++, j += 2) {
              int index = i + startingIndexG;
              if (index >= afile.startingindex && index < afile.finalindex) {
                if (afile.xInsideRange(afile.getXData(index)) || !markExcludedRegion)
                  dataphase[j + 1] = afile.getFitSqrtData(index, s);
                else
                  dataphase[j + 1] = Double.NaN;
              }
              dataphase[j] = data[j];
//            System.out.println(datafit[j] + " " + datafit[j + 1]);

            }
            phaseData[s] = graph.loadDataSet(dataphase, np);
            phaseData[s].linestyle = 1;
            if (blackAndWhite)
              phaseData[s].linecolor = Color.black;
            else
              phaseData[s].linecolor = getPastelColor(s + 2);
          }
        }

      }

      DataSet bkgData = null;

      if (afile.hasfit() && plotBackground) {
        dataphase = new double[2 * np];
        for (i = j = 0; i < np; i++, j += 2) {
          int index = i + startingIndexG;
          if (index >= afile.startingindex && index < afile.finalindex) {
            if (afile.xInsideRange(afile.getXData(index)) || !markExcludedRegion)
              dataphase[j + 1] = afile.getBkgFitSqrtData(index);
            else
              dataphase[j + 1] = Double.NaN;
          }
          dataphase[j] = data[j];
//            System.out.println(datafit[j] + " " + datafit[j + 1]);

        }
        bkgData = graph.loadDataSet(dataphase, np);
        bkgData.linestyle = 1;
//            if (blackAndWhite)
        bkgData.linecolor = Color.black;
//            else
//              bkgData.linecolor = getPastelColor(0);

      }

/*
**      Attach data sets to the Xaxis
*/
//          System.out.println("Attaching X-axis....");

      xaxis = graph.createXAxis();
      xaxis.axiscolor = Color.black;
      if ((plotResiduals && afile.hasfit()) || peaksLocated) {
        xaxis.drawLabel = false;
        xaxis.drawTitle = false;
      }
      xaxis.attachDataSet(data1);
      if (afile.hasfit() || peaksLocated) {
        xaxis.attachDataSet(dataFit);
      }
      if (afile.hasfit()) {
        for (int s = 0; s < numberphases; s++)
          if (phaselist[s].plotFit())
            xaxis.attachDataSet(phaseData[s]);
        if (plotBackground)
          xaxis.attachDataSet(bkgData);
      }

/*
**      Attach the first data set to the Left Axis
*/
//          System.out.println("Attaching Y-axis....");

      yaxis = graph.createYAxis();
      yaxis.attachDataSet(data1);
      if (afile.hasfit() || peaksLocated) {
        yaxis.attachDataSet(dataFit);
      }
      if (afile.hasfit()) {
        for (int s = 0; s < numberphases; s++)
          if (phaselist[s].plotFit())
            yaxis.attachDataSet(phaseData[s]);
        if (plotBackground)
          yaxis.attachDataSet(bkgData);
      }


      yaxis.setTitleText(DiffrDataFile.getAxisYLegend());
      yaxis.setTitleFont(new Font(axisFont, Font.BOLD, YaxisTitleFontScale));
      yaxis.setLabelFont(new Font(labelFont, Font.PLAIN, YaxisLabelFontScale));
      yaxis.setTitleColor(YaxisTitleColor);
      yaxis.axiscolor = Color.black;

	    DataFileSet adataset = afile.getDataFileSet();
	    int numberofPeaks = adataset.getNumberofPeaks();
	    if (adataset.getInstrument() instanceof XRFInstrument) {
		    numberofPeaks = 0;
		    double energyInKeV = Constants.ENERGY_LAMBDA / adataset.getInstrument().getRadiationType().
				    getRadiationWavelengthForFluorescence(0) * 0.001;
		    if (peaksInfoXRF == null || energyUsedForPeakInfoXRF < energyInKeV) {
			    energyUsedForPeakInfoXRF = energyInKeV;
			    peaksInfoXRF = new Vector<PeakInfo>(10000, 1000);
			    double threasholdXRFLines = MaudPreferences.getDouble("fluorescenceLinesPlot.thresholdOnMinors", 0.001);
			    for (int atomNumber = 0; atomNumber < XRayDataSqLite.atomsNumber; atomNumber++) {
				    String atomLabel = AtomInfo.atomLabels[atomNumber];
				    Vector<FluorescenceLine> linesForAtom = XRayDataSqLite.getFluorescenceLinesFor(
						    atomNumber + 1, energyInKeV);
				    for (int lindex = 0; lindex < linesForAtom.size(); lindex++) {
					    FluorescenceLine line = linesForAtom.get(lindex);
					    if (line.getIntensity() > threasholdXRFLines) {
						    PeakInfo peakInfo = new PeakInfo();
						    peakInfo.info = atomLabel + " " + line.toString();
						    peakInfo.coordinate = line.getEnergy() * 1000;
						    peaksInfoXRF.add(peakInfo);
					    }/* else {
						    System.out.println("Not added: " + atomLabel + " " + line.toString() + " " + line.getEnergy() + " " + line.getIntensity());
					    }*/
				    }
			    }
		    }
		    graph.attachPeaksInfos(peaksInfoXRF);
		    graph.plot_type = graph.FLUORESCENCE_PLOT;
	    }

      if (((plotPeaks && numberofPeaks > 0) && afile.hasfit()) || peaksLocated) {

/*
**      prepare the positions box
*/
        if (!peaksLocated) {
          Vector<Peak> peaklist = adataset.getPeakList();

          int numberradiation = adataset.getInstrument().getRadiationType().getLinesCountForPlot();

          if (numberradiation == 0)
            numberradiation = 1;
          int numberofRefl = numberofPeaks;

          if (numberofRefl > 0) {

	          Vector<PeakInfo> peaksInfos = new Vector<PeakInfo>(1000, 100);

	          positions = new G2Dint(new plotPhaseMouseAdapter(), null);

            graph.addComponent(positions);

            c.weighty = 0.04 * (numberphases + 1);
            c.gridwidth = GridBagConstraints.REMAINDER;
//              c.h = numberphases + 1;

            gridbag.setConstraints(positions, c);
//        			c1.add(positions, c);
            fullGraphPanel.add(positions);
//              c1.add(positions, "0 60 100 80");
//              c.y += c.h;

            positions.drawzero = false;
            positions.drawgrid = false;
            positions.frame = false;
            positions.borderTop = 1;
            positions.borderBottom = 1;
/*
**      Load a file containing Marker definitions
*/

            if (marker != null)
              positions.setMarkers(marker);

            double[] datapeak = new double[2 * numberofRefl];

            int dimension = numberradiation;

            if (numberphases > dimension)
              dimension = numberphases;
            datap = new PeakSet[dimension];

            for (int ijn = 0; ijn < numberradiation; ijn++) {
              double wave = adataset.getInstrument().getRadiationType().getRadiationWavelength(ijn);
              for (i = j = 0; i < numberofPeaks; i++, j += 2) {
                Phase tmpphase = peaklist.elementAt(i).getPhase();
                int phaseindex = 0;
                for (int ij = 0; ij < numberphases; ij++)
                  if (tmpphase == phaselist[ij])
                    phaseindex = ij;
								int reflIndex = tmpphase.getReflexIndex(peaklist.elementAt(i).getReflex());
                double pos = adataset.getActiveDataFile(0).getPositions(tmpphase)[reflIndex][0][ijn];
                datapeak[j] = afile.convertXDataForPlot(pos, wave, mode);
                double dspace = adataset.getActiveDataFile(0).convertXToDspace(pos, wave);
                datapeak[j + 1] = (double) (phaseindex + 1);
	              if (ijn == 0) {
		              PeakInfo peakInfo = new PeakInfo();
		              Reflection refl = peaklist.elementAt(i).getReflex();
		              peakInfo.info = tmpphase.getPhaseName() + " (" + Integer.toString(refl.getH()) + " " +
				              Integer.toString(refl.getK()) + " " +
				              Integer.toString(refl.getL()) + ") " + Double.toString(dspace);
		              peakInfo.coordinate = datapeak[j];
//		              System.out.println("Adding: " + peakInfo.coordinate + " " + peakInfo.info);
		              peaksInfos.add(peakInfo);
	              }
              }
              datap[ijn] = positions.loadPeakSet(datapeak, numberofRefl);
              datap[ijn].linestyle = 0;
              datap[ijn].marker = 9;
              double mscale = 2.0 - ijn;
              if (mscale <= 0.01)
                mscale = 0.5;
              datap[ijn].markerscale = mscale;

//        		 		int index = 0;
//        		 		if (ijn > 0)
//        		 			index = 1;
//              if (blackAndWhite)
              datap[ijn].markercolor = Color.black;
//              else
//                datap[ijn].markercolor = getPastelColor(ijn);

            }

            datapeak = new double[2];
            for (int ijn = numberradiation; ijn < numberphases; ijn++) {
              datapeak[0] = 0.0;
              datapeak[1] = 1.0;
              datap[ijn] = positions.loadPeakSet(datapeak, 1);
              datap[ijn].linestyle = 0;
              datap[ijn].marker = 9;
              datap[ijn].markerscale = 0.1;
              if (blackAndWhite)
                datap[ijn].markercolor = Color.black;
              else
                datap[ijn].markercolor = getPastelColor(ijn);
            }
            for (int ij = 0; ij < numberphases; ij++) {
              double ypos = (double) (ij + 1);
              datap[ij].legend(1, ypos, phaselist[ij].toXRDcatString());
              datap[ij].legendFont(new Font(labelFont, Font.PLAIN, PhasesFontScale));
              if (blackAndWhite)
                datap[ij].legendColor(Color.black);
              else
                datap[ij].legendColor(getPastelColor(ij + 2));
            }

//          System.out.println("Data loaded");

/*
**      Attach data sets to the Xaxis
*/
//          System.out.println("Attaching X-axis....");

            Axis xaxisp = positions.createXAxis();
            xaxisp.drawLine = false;
            xaxisp.drawLabel = false;
            xaxisp.drawTitle = false;
            xaxisp.referenceAxis = xaxis;
            xaxisp.axiscolor = Color.black;

            for (int ijn = 0; ijn < dimension; ijn++)
              xaxisp.attachDataSet(datap[ijn]);

/*
**      Attach the first data set to the Left Axis
*/
//          System.out.println("Attaching Y-axis....");

            yaxisp = positions.createYAxis();
            yaxisp.drawLine = false;
            yaxisp.drawLabel = false;
            yaxisp.drawTitle = false;
            yaxisp.referenceAxisWidth = yaxis;
            for (int ijn = 0; ijn < dimension; ijn++)
              yaxisp.attachDataSet(datap[ijn]);
            yaxisp.setTitleText(" ");
            yaxisp.setTitleFont(new Font(axisFont, Font.BOLD, YaxisTitleFontScale));
            yaxisp.setLabelFont(new Font(labelFont, Font.PLAIN, YaxisLabelFontScale));
            yaxisp.setTitleColor(getBackground());
            yaxisp.axiscolor = Color.black;
            yaxisp.minimum = 0.5;
            yaxisp.maximum = ((double) numberphases) + 0.5;

	          graph.attachPeaksInfos(peaksInfos);
          }
        } else {
          int numberofRefl = peaksList[0].length;

          if (numberofRefl > 0) {

            positions = new G2Dint(new g2DPmouse(), null);

            graph.addComponent(positions);

            c.weighty = 0.04 * 2;
            c.gridwidth = GridBagConstraints.REMAINDER;
//              c.h = numberphases + 1;

            gridbag.setConstraints(positions, c);
//        			c1.add(positions, c);
            fullGraphPanel.add(positions);
//              c1.add(positions, "0 60 100 80");
//              c.y += c.h;

            positions.drawzero = false;
            positions.drawgrid = false;
            positions.frame = false;
            positions.borderTop = 1;
            positions.borderBottom = 1;
/*
**      Load a file containing Marker definitions
*/

            if (marker != null)
              positions.setMarkers(marker);

            double[] datapeak = new double[2 * numberofRefl];

            int dimension = 1;
            datap = new PeakSet[dimension];

            double wave = adataset.getInstrument().getRadiationType().getRadiationWavelength(0);
            for (i = 0; i < numberofRefl; i++) {
              datapeak[2 * i] = afile.convertXDataForPlot(peaksList[0][i], wave, mode);

              datapeak[2 * i + 1] = 1.0;
            }
            datap[0] = positions.loadPeakSet(datapeak, numberofRefl);
            datap[0].linestyle = 0;
            datap[0].marker = 9;
            double mscale = 2.0;
            datap[0].markerscale = mscale;
            if (blackAndWhite)
              datap[0].markercolor = Color.black;
            else
              datap[0].markercolor = getPastelColor(1);

            double ypos = 1.0;
            datap[0].legend(1, ypos, "Peaks");
            datap[0].legendFont(new Font(labelFont, Font.PLAIN, PhasesFontScale));
            if (blackAndWhite)
              datap[0].legendColor(Color.black);
            else
              datap[0].legendColor(getPastelColor(1));
/*
**      Attach data sets to the Xaxis
*/
//          System.out.println("Attaching X-axis....");

            Axis xaxisp = positions.createXAxis();
            xaxisp.drawLine = false;
            xaxisp.drawLabel = false;
            xaxisp.drawTitle = false;
            xaxisp.referenceAxis = xaxis;
            xaxisp.axiscolor = Color.black;

            for (int ijn = 0; ijn < dimension; ijn++)
              xaxisp.attachDataSet(datap[ijn]);

/*
**      Attach the first data set to the Left Axis
*/
//          System.out.println("Attaching Y-axis....");

            yaxisp = positions.createYAxis();
            yaxisp.drawLine = false;
            yaxisp.drawLabel = false;
            yaxisp.drawTitle = false;
            yaxisp.axiscolor = Color.black;
            yaxisp.referenceAxisWidth = yaxis;
            for (int ijn = 0; ijn < dimension; ijn++)
              yaxisp.attachDataSet(datap[ijn]);
            yaxisp.setTitleText(" ");
            yaxisp.setTitleFont(new Font(axisFont, Font.BOLD, YaxisTitleFontScale));
            yaxisp.setLabelFont(new Font(labelFont, Font.PLAIN, YaxisLabelFontScale));
            yaxisp.setTitleColor(getBackground());
            yaxisp.minimum = 0.5;
            yaxisp.maximum = 1.5;

          }
        }
/*
**      prepare the residuals box
*/
      }

      if ((plotResiduals && afile.hasfit()) || peaksLocated) {

        residuals = new G2Dint();
        residuals.setDataBackground(Color.white);

        graph.addComponent(residuals);

        c.weighty = 0.3;
        if (peaksLocated)
          c.weighty = 0.7;
//            c.h = 5;
        c.gridwidth = GridBagConstraints.REMAINDER;

        gridbag.setConstraints(residuals, c);
        fullGraphPanel.add(residuals);
//        		c1.add(residuals, c);
//            c1.add(residuals, "0 80 100 100");

        residuals.drawzero = false;
        residuals.drawgrid = false;
        residuals.borderTop = 0;
        residuals.borderBottom = 10;
/*
**      Load a file containing Marker definitions
*/

        if (marker != null)
          residuals.setMarkers(marker);

        if (peaksLocated)
          for (i = j = 0; i < np; i++, j += 2) {
            data[j + 1] = MoreMath.sign(derivative2[i]) *
                Math.sqrt(Math.sqrt(Math.abs(derivative2[i])));
          }
        else
          for (i = j = 0; i < np && j+1 < data.length && j+1 < datafit.length; i++, j += 2) {
            data[j + 1] = datafit[j + 1] - data[j + 1];
          }

        datar = residuals.loadDataSet(data, np);

/*
**      Attach data sets to the Xaxis
*/
//          System.out.println("Attaching X-axis....");

        xaxisr = residuals.createAxis(Axis.BOTTOM);
        xaxisr.referenceAxis = xaxis;
        xaxisr.axiscolor = Color.black;
        xaxisr.attachDataSet(datar);
        xaxisr.setTitleText(afile.getAxisXLegend());
        xaxisr.setTitleFont(new Font(axisFont, Font.BOLD, XaxisTitleFontScale));
        xaxisr.setLabelFont(new Font(labelFont, Font.PLAIN, XaxisLabelFontScale));
        xaxisr.setTitleColor(XaxisTitleColor);
/*
**      Attach the first data set to the Left Axis
*/
//          System.out.println("Attaching Y-axis....");

        yaxisr = residuals.createAxis(Axis.LEFT);
        yaxisr.drawLine = false;
        yaxisr.drawLabel = false;
        yaxisr.drawTitle = false;
        yaxisr.axiscolor = Color.black;
        yaxisr.referenceAxisWidth = yaxis;
        if (!peaksLocated) {
          yaxisr.referenceAxisScale = yaxis;
        }
        yaxisr.attachDataSet(datar);
        yaxisr.setTitleText(" ");
        yaxisr.setTitleFont(new Font(axisFont, Font.BOLD, YaxisTitleFontScale));
        yaxisr.setLabelFont(new Font(labelFont, Font.PLAIN, YaxisLabelFontScale));
        yaxisr.setTitleColor(getBackground());
        yaxisr.axiscolor = Color.black;

      } else {
        xaxis.setTitleText(afile.getAxisXLegend());
        xaxis.setTitleFont(new Font(axisFont, Font.BOLD, XaxisTitleFontScale));
        xaxis.setLabelFont(new Font(labelFont, Font.PLAIN, XaxisLabelFontScale));
        xaxis.setTitleColor(XaxisTitleColor);
        xaxis.axiscolor = Color.black;
        graph.borderBottom = 10;
      }
    } else {
      graph = null;
	    graph.finishedloading();
      String[] labels = new String[1];
      labels[0] = "No datafile selected!";
      return new NoDatafileCanvas(labels);
    }

	  graph.finishedloading();
    if (keepMaxima) {
      graph.setUserLimits(xMin, xMax, IntensityMin, IntensityMax);
//      System.out.println("Keeping maxima: " + xMin + " " + xMax + " " + IntensityMin + " " + IntensityMax);
    } else {
      xMin = 0.0;
      xMax = 0.0;
      IntensityMin = 0.0;
      IntensityMax = 0.0;
    }

    return fullGraphPanel;
  }

  public static Color[] PastelColor = {Color.black, Color.red, new Color(0, 102, 0), Color.blue,
      Color.magenta, Color.cyan, Color.orange, Color.green, Color.pink, Color.gray,
      new Color(102, 51, 0), new Color(153, 0, 153), Color.yellow,
      new Color(102, 102, 0), new Color(0, 102, 102)
  };

  public static Color getPastelColor(int index) {
    int maxNumber = PastelColor.length;
    while (index >= maxNumber) index -= maxNumber;
    return PastelColor[index];
  }

  public void addPeak(double dToRemove) {
    int numberOfPeaks = peaksList[0].length;
    double[][] newPeaksList = new double[2][numberOfPeaks + 1];
    if (numberOfPeaks <= 1) {
      newPeaksList[0][numberOfPeaks] = dToRemove;
      newPeaksList[1][numberOfPeaks] = 1.0;
    } else {
      int indexToAdd = 0;
      int sign = (int) ((peaksList[0][numberOfPeaks - 1] - peaksList[0][0]) /
          Math.abs(peaksList[0][numberOfPeaks - 1] - peaksList[0][0]));
      for (int i = 0; i < numberOfPeaks; i++) {
        if (sign * peaksList[0][i] < dToRemove) {
          indexToAdd = i + 1;
        } else
          break;
      }
      int j = 0;
      for (int i = 0; i < numberOfPeaks + 1; i++) {
        if (i != indexToAdd) {
          newPeaksList[0][i] = peaksList[0][j];
          newPeaksList[1][i] = peaksList[1][j++];
        } else {
          newPeaksList[0][i] = dToRemove;
          newPeaksList[1][i] = 1.0;
        }
      }
    }
    peaksList = newPeaksList;
    updatePlotForPeaks();
  }

  public void removePeak(double dToRemove) {
    int numberOfPeaks = peaksList[0].length;
    double[][] newPeaksList = new double[2][numberOfPeaks - 1];
    int indexToRemove = 0;
    double dist = Math.abs(peaksList[0][0] - dToRemove);
    for (int i = 1; i < numberOfPeaks; i++) {
      double dist1 = Math.abs(peaksList[0][i] - dToRemove);
      if (dist1 < dist) {
        dist = dist1;
        indexToRemove = i;
      }
    }
    int j = 0;
    for (int i = 0; i < numberOfPeaks; i++) {
      if (i != indexToRemove) {
        newPeaksList[0][j] = peaksList[0][i];
        newPeaksList[1][j++] = peaksList[1][i];
      }
    }
    peaksList = newPeaksList;
    updatePlotForPeaks();
  }

  public void updatePlotForPeaks() {

    if (peaksList == null)
      return;

    double[] trange = positions.getRanges();

    try {

      int mode = PlotDataFile.checkScaleModeX();
      datap[0].deleteData();

      int numberofRefl = peaksList[0].length;
      double[] datapeak = new double[2 * numberofRefl];

      DataFileSet adataset = datafile[0].getDataFileSet();
      double wave = adataset.getInstrument().getRadiationType().getRadiationWavelength(0);
      for (int i = 0; i < numberofRefl; i++) {
        datapeak[2 * i] = datafile[0].convertXDataForPlot(peaksList[0][i], wave, mode);
//          System.out.println(datapeak[2*i]);
        datapeak[2 * i + 1] = 1.0;
      }
      datap[0].append(datapeak, numberofRefl);

    } catch (Exception e) {
    }

    positions.updateDataAndPaint(trange);

  }

  public Component getComponentToPrint() {
    return componentToPrint;
  }

  private void setTitle(String title) {
  }

//  double[] avPositions;

	boolean editInterpolatedPoints = false;

  public void editInterpolatedBackgroundPoints() {
	  if (!editInterpolatedPoints) {
		  double[] range = graph.getRanges();
		  int[] points = datafile[0].getInterpolatedPointsX();
		  double[] avPositions = new double[points.length];
		  int[] numberHits = new int[points.length];
		  for (int i = 0; i < datafile.length; i++) {
			  datafile[i].setManualBkgInterpolation(true);
			  points = datafile[i].getInterpolatedPointsX();
			  double[] positions = new double[points.length];
			  for (int j = 0; j < points.length; j++) {
				  if (points[j] >= 0) {
					  positions[j] = datafile[i].getXDataForPlot(points[j]);
					  avPositions[j] += positions[j];
					  numberHits[j]++;
				  }
			  }
		  }
		  double data[] = new double[points.length * 2];
		  for (int j = 0; j < points.length; j++) {
			  if (numberHits[j] > 0) {
				  avPositions[j] /= numberHits[j];
				  MyPoint p = ((plotPanelG2Dint) graph).addPoint(avPositions[j]);
				  data[j * 2] = p.x;
				  data[j * 2 + 1] = p.y;
			  }
		  }
		  try {
			  ((plotPanelG2Dint) graph).pointsDataset = new DataSet(data, points.length);
		  } catch (Exception e) {
			  e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		  }

		  graph.attachDataSet(((plotPanelG2Dint) graph).pointsDataset);
		  ((plotPanelG2Dint) graph).pointsDataset.linestyle = 0;
		  ((plotPanelG2Dint) graph).pointsDataset.marker = 10;
		  ((plotPanelG2Dint) graph).pointsDataset.markerscale = 2;
		  ((plotPanelG2Dint) graph).pointsDataset.markercolor = getPastelColor(4);
		  xaxis.attachDataSet(((plotPanelG2Dint) graph).pointsDataset);
		  yaxis.attachDataSet(((plotPanelG2Dint) graph).pointsDataset);

		  ((plotPanelG2Dint) graph).isLikeAltDown = true;
		  editInterpolatedPoints = true;
		  graph.updateDataAndPaint(range);
	  }
  }

	public static Vector<PeakInfo> getFluorescencePeakInfos(double energyInKeV) {
		if (energyInKeV <= 0)
			energyInKeV = energyUsedForPeakInfoXRF;
		if (peaksInfoXRF == null || energyUsedForPeakInfoXRF < energyInKeV) {
			energyUsedForPeakInfoXRF = energyInKeV;
			peaksInfoXRF = new Vector<>(10000, 1000);
			double threasholdXRFLines = MaudPreferences.getDouble("fluorescenceLinesPeaksInfo.thresholdOnMinors", 0.001);
			for (int atomNumber = 0; atomNumber < XRayDataSqLite.atomsNumber; atomNumber++) {
				String atomLabel = AtomInfo.atomLabels[atomNumber];
				Vector<FluorescenceLine> linesForAtom = XRayDataSqLite.getFluorescenceLinesFor(
						atomNumber + 1, energyInKeV);
				for (int lindex = 0; lindex < linesForAtom.size(); lindex++) {
					FluorescenceLine line = linesForAtom.get(lindex);
					if (line.getIntensity() > threasholdXRFLines) {
						PeakInfo peakInfo = new PeakInfo();
						peakInfo.info = atomLabel + " " + line.toString();
						peakInfo.coordinate = line.getEnergy() * 1000;
						peaksInfoXRF.add(peakInfo);
					}
				}
			}
		}
		return peaksInfoXRF;
	}

  class g2DPmouse extends MouseAdapter {
    G2Dint positionBox = null;
    int x1 = 0;
    int y1 = 0;

    public void mousePressed(MouseEvent e) {
      if (e.isPopupTrigger() /*G2Dint.isRightMouseButton(e)*/ && e.getComponent() instanceof G2Dint) {

        x1 = e.getX();
        y1 = e.getY();

        JPopupMenu popup = new JPopupMenu("Options");
        popup.setLightWeightPopupEnabled(false);

        positionBox = (G2Dint) e.getComponent();

        JMenuItem mi = new JMenuItem("Add peak");
        mi.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent ae) {
            double d[] = positionBox.getPoint(x1, y1);
            addPeak(d[0]);
          }
        });
        popup.add(mi);

        mi = new JMenuItem("Remove peak");
        mi.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent ae) {
            double d[] = positionBox.getClosestPoint(x1, y1);
            removePeak(d[0]);
          }
        });
        popup.add(mi);

        popup.show(e.getComponent(), e.getX(), e.getY());
      }
    }

    public void mouseReleased(MouseEvent e) {
      if (e.isPopupTrigger() /*G2Dint.isRightMouseButton(e)*/ && e.getComponent() instanceof G2Dint) {

        x1 = e.getX();
        y1 = e.getY();

        JPopupMenu popup = new JPopupMenu("Options");
        popup.setLightWeightPopupEnabled(false);

        positionBox = (G2Dint) e.getComponent();

        JMenuItem mi = new JMenuItem("Add peak");
        mi.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent ae) {
            double d[] = positionBox.getPoint(x1, y1);
            addPeak(d[0]);
          }
        });
        popup.add(mi);

        mi = new JMenuItem("Remove peak");
        mi.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent ae) {
            double d[] = positionBox.getClosestPoint(x1, y1);
            removePeak(d[0]);
          }
        });
        popup.add(mi);

        popup.show(e.getComponent(), e.getX(), e.getY());
      }
    }

    public void mouseClicked(MouseEvent e) {
    }

  }

  class plotPhaseMouseAdapter extends MouseAdapter {

    int x1 = 0;
    int y1 = 0;

    public void mousePressed(MouseEvent e) {
      JCheckBoxMenuItem mi;

      if (e.isPopupTrigger() /*G2Dint.isRightMouseButton(e)*/ && e.getComponent() instanceof G2Dint) {

        x1 = e.getX();
        y1 = e.getY();

//	      System.out.println("Mouse pressed");

	      FilePar filepar = null;

        if (datafile != null)
	        filepar = datafile[0].getFilePar();
        else if (dataset != null)
	        filepar = dataset.getFilePar();
				if (filepar != null) {
          JPopupMenu popup = new JPopupMenu("Plot fit for phases");
          popup.setLightWeightPopupEnabled(false);

          int numberphases = filepar.getActiveSample().phasesNumber();
//      if (!plotPeaks)
//        numberphases = 0;
          Phase[] phaselist = new Phase[numberphases];
          for (int i = 0; i < numberphases; i++) {
            phaselist[i] = filepar.getActiveSample().getPhase(i);

            final int index = i;
            mi = new JCheckBoxMenuItem(phaselist[i].getLabel());
            mi.setSelected(phaselist[i].plotFit());
            mi.addActionListener(new ActionListener() {
              public void actionPerformed(ActionEvent ae) {
                toggleViewForPhase(index);
                replot(true);
              }
            });
            popup.add(mi);

          }

          popup.show(e.getComponent(), e.getX(), e.getY());
        }
      }
    }

    public void mouseReleased(MouseEvent e) {
      JCheckBoxMenuItem mi;

      if (e.isPopupTrigger() /*G2Dint.isRightMouseButton(e)*/ && e.getComponent() instanceof G2Dint) {

        x1 = e.getX();
        y1 = e.getY();

//	      System.out.println("Mouse released");

	      FilePar filepar = null;

	      if (datafile != null)
		      filepar = datafile[0].getFilePar();
	      else if (dataset != null)
		      filepar = dataset.getFilePar();
	      if (filepar != null) {

          JPopupMenu popup = new JPopupMenu("Plot fit for phases");
          popup.setLightWeightPopupEnabled(false);

          int numberphases = filepar.getActiveSample().phasesNumber();
//      if (!plotPeaks)
//        numberphases = 0;
          Phase[] phaselist = new Phase[numberphases];
          for (int i = 0; i < numberphases; i++) {
            phaselist[i] = filepar.getActiveSample().getPhase(i);

            final int index = i;
            mi = new JCheckBoxMenuItem(phaselist[i].getLabel());
            mi.setSelected(phaselist[i].plotFit());
            mi.addActionListener(new ActionListener() {
              public void actionPerformed(ActionEvent ae) {
                toggleViewForPhase(index);
                replot(true);
              }
            });
            popup.add(mi);

          }

          popup.show(e.getComponent(), e.getX(), e.getY());
        }
      }
    }

    public void mouseClicked(MouseEvent e) {
    }

  }

  private void toggleViewForPhase(int index) {
	  if (datafile != null)
      datafile[0].getFilePar().getActiveSample().getPhase(index).toggleView();
	  else if (dataset != null)
		  dataset.getFilePar().getActiveSample().getPhase(index).toggleView();
  }

  class NoDatafileCanvas extends CopyPrintPanelNoBkg {
    String[] labels = {"No datafile specified!", "Drag and drop a datafile here, or",
        "use menu command: File->Load datafile."};

    public NoDatafileCanvas(String[] labels) {
      this();
      this.labels = labels;
    }

    public NoDatafileCanvas() {
      super();
    }

    public void paint(Graphics g) {
      super.paint(g);

      g.setColor(Color.red);
      int linePixels = 48;
      g.setFont(new Font("Arial", Font.PLAIN, linePixels / 2));
      int xp = 50;
      int yp = 3;
      for (int i = 0; i < labels.length; i++) {
        g.drawString(labels[i], xp, linePixels * yp++);
      }
    }
  }

  class plotPanelG2Dint extends G2Dint {

    G2Dint graphBox = null;
    int x1 = 0;
    int y1 = 0;
	  DataSet pointsDataset = null;
	  Vector points = new Vector();


	  public MyPoint addPoint(double x) {
      double d[] = getClosestPoint(x);
      MyPoint point = new MyPoint(d[0], d[1]);
      points.add(point);
	    return point;
    }

    public void addNewPoint(double x) {
	    double[] range = graph.getRanges();
	    double d[] = getClosestPoint(x);
	    boolean added = true;
	    for (int i = 0; i < datafile.length; i++) {
		    datafile[i].addInterpolatedPointsX(d[0]);
	    }
	    if (added) {
		    MyPoint point = new MyPoint(d[0], d[1]);
		    points.add(point);
		    double[] data = {point.x, point.y};
		    try {
			    pointsDataset.append(data, 1);
		    } catch (Exception e) {
			    e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		    }
		    graph.updateDataAndPaint(range);
	    }
    }

    public void removePoint(double x) {
	    double[] range = graph.getRanges();
	    double d[] = getClosestPoint(x);
      int indexToRemove = getClosestBkgPoint(d[0], d[1]);
	    try {
		    pointsDataset.delete(indexToRemove, indexToRemove);
	    } catch (Exception e) {
		    e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
	    }
	    MyPoint pointToRemove = (MyPoint) points.elementAt(indexToRemove);
      for (int i = 0; i < datafile.length; i++) {
	      if (datafile.length > 1)
          datafile[i].removeInterpolatedPointsX(pointToRemove.x, 100);
	      else
	        datafile[i].removeInterpolatedPointsX(pointToRemove.x, 0.0001);
      }
	    points.remove(pointToRemove);
	    graph.updateDataAndPaint(range);
    }

    public int getClosestBkgPoint(double x, double y) {
      MyPoint ds;

      double closestDistance = 1.0E256;
      int closest = -1;
      for (int i = 0; i < points.size(); i++) {
        ds = (MyPoint) (points.elementAt(i));

        double distx = ds.x - x;
        double disty = ds.y - y;
        double dist = distx * distx + disty * disty;

        if (dist < closestDistance) {
          closestDistance = dist;
          closest = i;
        }
      }
      return closest;

    }

    public void altDownMousePressed(MouseEvent e) {
      if (e.isPopupTrigger() /*G2Dint.isRightMouseButton(e)*/ && e.getComponent() instanceof G2Dint) {

        x1 = e.getX();
        y1 = e.getY();

        JPopupMenu popup = new JPopupMenu("Options");
        popup.setLightWeightPopupEnabled(false);

        graphBox = (G2Dint) e.getComponent();

        JMenuItem mi = new JMenuItem("Add bkg point");
        mi.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent ae) {
            double d[] = graphBox.getPoint(x1, y1);
            addNewPoint(d[0]);
          }
        });
        popup.add(mi);

        mi = new JMenuItem("Remove bkg point");
        mi.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent ae) {
            double d[] = graphBox.getPoint(x1, y1);
            removePoint(d[0]);
          }
        });
        popup.add(mi);

        popup.show(e.getComponent(), e.getX(), e.getY());
      }
    }

    public void altDownMouseReleased(MouseEvent e) {
      if (e.isPopupTrigger() /*G2Dint.isRightMouseButton(e)*/ && e.getComponent() instanceof G2Dint) {

        x1 = e.getX();
        y1 = e.getY();

        JPopupMenu popup = new JPopupMenu("Options");
        popup.setLightWeightPopupEnabled(false);

        graphBox = (G2Dint) e.getComponent();

        JMenuItem mi = new JMenuItem("Add bkg point");
        mi.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent ae) {
            double d[] = graphBox.getPoint(x1, y1);
            addNewPoint(d[0]);
          }
        });
        popup.add(mi);

        mi = new JMenuItem("Remove bkg point");
        mi.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent ae) {
            double d[] = graphBox.getPoint(x1, y1);
            removePoint(d[0]);
          }
        });
        popup.add(mi);

        popup.show(e.getComponent(), e.getX(), e.getY());
      }
    }

    public void altDownMouseClicked(MouseEvent e) {

    }

    public void altDownMouseDragged(MouseEvent e) {

    }

    public void altDownMouseMoved(MouseEvent e) {

    }

  }

  class MyPoint {
    double x;
    double y;

    public MyPoint(double x, double y) {
      this.x = x;
      this.y = y;
    }
  }
}
